// Copyright 2019 Ian Castleden
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
use super::patch::nl;
use super::QuoteT;
use super::{filter_visible, ident_from_str, ParseContext, QuoteMaker, QuoteMakerKind};
use quote::quote;
use serde_derive_internals::{ast, ast::Variant, attr::TagType};
const CONTENT: &str = "fields"; // default content tag
                                // const TAG: &'static str = "kind"; // default tag tag
struct TagInfo<'a> {
    /// #[serde(tag = "...")]
    tag: Option<&'a str>,
    /// #[serde(content = "...")]
    content: Option<&'a str>,
    /// flattened without tag `{ "key1": "", "key2": "" }`
    untagged: bool,
}

impl<'a> TagInfo<'a> {
    fn from_enum(e: &'a TagType) -> Self {
        match e {
            TagType::Internal { tag, .. } => TagInfo {
                tag: Some(tag),
                content: None,
                untagged: false,
            },
            TagType::Adjacent { tag, content, .. } => TagInfo {
                tag: Some(tag),
                content: Some(&content),
                untagged: false,
            },
            TagType::External => TagInfo {
                tag: None,
                content: None,
                untagged: false,
            },
            TagType::None => TagInfo {
                tag: None,
                content: None,
                untagged: true,
            },
        }
    }
}

struct VariantQuoteMaker {
    /// message type possibly including tag key value
    pub source: QuoteT,
    /// enum factory quote token stream
    // pub enum_factory: Result<QuoteT, &'static str>,
    /// inner type token stream
    pub inner_type: Option<QuoteT>,
}

#[allow(clippy::or_fun_call, clippy::bind_instead_of_map)]
impl<'a> ParseContext {
    pub(crate) fn derive_enum(
        &self,
        variants: &[ast::Variant<'a>],
        ast_container: &ast::Container,
    ) -> QuoteMaker {
        // https://serde.rs/enum-representations.html
        let taginfo = TagInfo::from_enum(ast_container.attrs.tag());
        // remove skipped ( check for #[serde(skip)] )
        let variants: Vec<&ast::Variant<'a>> = variants
            .iter()
            .filter(|v| !v.attrs.skip_serializing())
            .collect();

        // is typescript enum compatible
        let is_enum = taginfo.tag.is_none()
            && taginfo.content.is_none()
            && variants.iter().all(|v| matches!(v.style, ast::Style::Unit));

        if is_enum {
            let comments = variants
                .iter()
                .map(|variant| crate::attrs::Attrs::from_variant(variant).to_comment_attrs())
                .collect::<Vec<_>>();
            let v = &variants
                .into_iter()
                .map(|v| v.attrs.name().serialize_name()) // use serde name instead of v.ident
                .collect::<Vec<_>>();

            let k = v.iter().map(|v| ident_from_str(&v)).collect::<Vec<_>>();

            return QuoteMaker {
                source: quote! ( { #(#(#comments)* #k = #v),* } ),
                kind: QuoteMakerKind::Enum,
            };
        }

        let content: Vec<(&Variant, VariantQuoteMaker)> = variants
            .iter()
            .map(|variant| {
                (
                    *variant,
                    match variant.style {
                        ast::Style::Struct => self.derive_struct_variant(
                            &taginfo,
                            variant,
                            &variant.fields,
                            ast_container,
                        ),
                        ast::Style::Newtype => {
                            self.derive_newtype_variant(&taginfo, variant, &variant.fields[0])
                        }
                        ast::Style::Tuple => {
                            self.derive_tuple_variant(&taginfo, variant, &variant.fields)
                        }
                        ast::Style::Unit => self.derive_unit_variant(&taginfo, variant),
                    },
                )
            })
            .collect::<Vec<_>>();

        // OK generate A | B | C etc
        let newl = nl();
        let body = content.iter().map(|(_, q)| q.source.clone());

        let newls = std::iter::repeat(quote!(#newl));
        QuoteMaker {
            source: quote! ( #( #newls | #body)* ),
            kind: QuoteMakerKind::Union,
        }
    }

    /// Depends on TagInfo for layout
    fn derive_unit_variant(&self, taginfo: &TagInfo, variant: &Variant) -> VariantQuoteMaker {
        let variant_name = variant.attrs.name().serialize_name(); // use serde name instead of variant.ident
        let comments = crate::attrs::Attrs::from_variant(variant).to_comment_attrs();
        if taginfo.tag.is_none() {
            return VariantQuoteMaker {
                source: quote!(#variant_name),
                inner_type: None,
            };
        }
        let tag = ident_from_str(taginfo.tag.unwrap());
        VariantQuoteMaker {
            source: quote! (
                { #(#comments)* #tag: #variant_name }
            ),
            inner_type: None,
        }
    }

    /// Depends on TagInfo for layout
    /// example variant: `C(u32)`
    fn derive_newtype_variant(
        &self,
        taginfo: &TagInfo,
        variant: &Variant,
        field: &ast::Field<'a>,
    ) -> VariantQuoteMaker {
        if field.attrs.skip_serializing() {
            return self.derive_unit_variant(taginfo, variant);
        };
        let comments = crate::attrs::Attrs::from_variant(variant).to_comment_attrs();
        let ty = self.field_to_ts(field);
        let variant_name = self.variant_name(variant);

        if taginfo.tag.is_none() {
            if taginfo.untagged {
                return VariantQuoteMaker {
                    source: quote! ( #ty ),
                    inner_type: Some(ty),
                };
            };
            let tag = ident_from_str(&variant_name);

            return VariantQuoteMaker {
                source: quote! (
                    { #(#comments)* #tag : #ty }

                ),
                inner_type: Some(ty),
            };
        };
        let tag = ident_from_str(taginfo.tag.unwrap());

        let content = if let Some(content) = taginfo.content {
            ident_from_str(&content)
        } else {
            ident_from_str(CONTENT) // should not get here...
        };

        VariantQuoteMaker {
            source: quote! (
                { #(#comments)* #tag: #variant_name; #content: #ty }
            ),
            inner_type: Some(ty),
        }
    }

    /// Depends on TagInfo for layout
    /// `C { a: u32, b: u32 }` => `C: { a: number, b: number }`
    fn derive_struct_variant(
        &self,
        taginfo: &TagInfo,
        variant: &Variant,
        fields: &[ast::Field<'a>],
        ast_container: &ast::Container,
    ) -> VariantQuoteMaker {
        use std::collections::HashSet;
        let fields = filter_visible(fields);
        if fields.is_empty() {
            return self.derive_unit_variant(taginfo, variant);
        }
        self.check_flatten(&fields, ast_container);

        let comments = crate::attrs::Attrs::from_variant(variant).to_comment_attrs();
        let contents = self.derive_fields(&fields).collect::<Vec<_>>();
        let variant_name = self.variant_name(variant);

        let ty_inner = quote!(#(#contents);*);
        let ty = quote! (
            { #ty_inner }
        );

        if taginfo.tag.is_none() {
            if taginfo.untagged {
                return VariantQuoteMaker {
                    source: quote!(#ty),
                    inner_type: Some(ty),
                };
            };
            let tag = ident_from_str(&variant_name);
            return VariantQuoteMaker {
                source: quote! (
                    { #(#comments)* #tag : #ty  }
                ),
                inner_type: Some(ty),
            };
        }
        let tag_str = taginfo.tag.unwrap();
        let tag = ident_from_str(tag_str);

        if let Some(content) = taginfo.content {
            let content = ident_from_str(&content);

            VariantQuoteMaker {
                source: quote! (
                    { #(#comments)* #tag: #variant_name; #content: #ty }
                ),
                inner_type: Some(ty),
            }
        } else {
            if let Some(ref cx) = self.ctxt {
                let fnames = fields
                    .iter()
                    .map(|field| field.attrs.name().serialize_name())
                    .collect::<HashSet<_>>();
                if fnames.contains(tag_str) {
                    cx.error_spanned_by(
                        tag_str,
                        format!(
                            "clash with field in \"{}::{}\". \
                         Maybe use a #[serde(content=\"...\")] attribute.",
                            ast_container.ident, variant_name
                        ),
                    );
                }
            };
            // spread together tagged no content
            VariantQuoteMaker {
                source: quote! (
                    { #(#comments)* #tag: #variant_name; #ty_inner }
                ),
                inner_type: Some(ty),
            }
        }
    }

    #[inline]
    fn variant_name(&self, variant: &Variant) -> String {
        variant.attrs.name().serialize_name() // use serde name instead of variant.ident
    }

    /// `B(u32, u32)` => `B: [number, number]`
    fn derive_tuple_variant(
        &self,
        taginfo: &TagInfo,
        variant: &Variant,
        fields: &[ast::Field<'a>],
    ) -> VariantQuoteMaker {
        let variant_name = self.variant_name(variant);
        let fields = filter_visible(fields);
        let comments = crate::attrs::Attrs::from_variant(variant).to_comment_attrs();
        let contents = self.derive_field_tuple(&fields);
        let ty = quote!([ #(#contents),* ]);

        if taginfo.tag.is_none() {
            if taginfo.untagged {
                return VariantQuoteMaker {
                    source: quote! (#ty),
                    inner_type: Some(ty),
                };
            }
            let tag = ident_from_str(&variant_name);
            return VariantQuoteMaker {
                source: quote! ({ #(#comments)* #tag : #ty }),
                inner_type: Some(ty),
            };
        };

        let tag = ident_from_str(taginfo.tag.unwrap());
        let content = if let Some(content) = taginfo.content {
            ident_from_str(&content)
        } else {
            ident_from_str(CONTENT)
        };

        VariantQuoteMaker {
            source: quote! (
            { #(#comments)* #tag: #variant_name; #content : #ty }
            ),
            inner_type: Some(ty),
        }
    }
}

extern crate proc_macro2;
extern crate quote;

use proc_macro2::TokenStream;
use quote::ToTokens;
// use std::str::ToString;
trait Tbuild {
    fn build(&self) -> TokenStream;
}
enum QuoteT<'a> {
    Tokens(TokenStream),
    Closure(Box<Fn() -> TokenStream + 'a>), //Builder(fn() -> TokenStream)
    //Builder(Fn() -> TokenStream)
    Builder(Box<Tbuild + 'a>),
}

impl ToTokens for QuoteT<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            QuoteT::Tokens(t) => t.to_tokens(tokens),
            QuoteT::Closure(f) => f().to_tokens(tokens),
            QuoteT::Builder(b) => b.build().to_tokens(tokens),
        }
    }
}
impl ToString for QuoteT<'_> {
    fn to_string(&self) -> String {
        match self {
            QuoteT::Tokens(t) => t.to_string(),
            QuoteT::Closure(f) => f().to_string(),
            QuoteT::Builder(b) => b.build().to_string(),
        }
    }
}
impl QuoteT<'_> {
    fn from_quote<'a>(s: TokenStream) -> QuoteT<'a> {
        QuoteT::Tokens(s)
    }
    fn from_closure<'a, F: Fn() -> TokenStream + 'a>(f: F) -> QuoteT<'a> {
        QuoteT::Closure(Box::new(f))
    }
    fn from_builder<'a, F: Tbuild + 'a>(f: F) -> QuoteT<'a> {
        QuoteT::Builder(Box::new(f))
    }
}
struct S {
    v: Vec<QuoteT>,
}

impl Tbuild for S {
    fn build(&self) -> TokenStream {
        let v = &self.v;
        quote ! (some more #(#v)&* )
    }
}
fn make_closure<'a>() -> QuoteT<'a> {
    let s = vec![
        QuoteT::from_quote(quote!(a b)),
        QuoteT::from_quote(quote!(c b)),
    ];
    let f = move || {
        let t = &s; // this needs to be a ref....!
        quote! (some more #(#t),*)
    };
    QuoteT::from_closure(f)
}
fn make_builder<'a>() -> QuoteT<'a> {
    let s = vec![
        QuoteT::from_quote(quote!(a b)),
        QuoteT::from_quote(quote!(c b)),
    ];
;
    QuoteT::from_builder(S { v: s })
}
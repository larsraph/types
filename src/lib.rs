//! # Rust
//! ```
//! use enum_type::enum_type;
//! use serde::{Serialize, Deserialize};
//! use chrono::{NaiveDate, NaiveDateTime};
//!
//! struct A<T: Debug, U> {
//!     pub t: T,
//!     u: U,
//! }
//!
//! pub struct B<'a, T>(String, pub &'a T) where T: Copy;
//!
//! type_enum! {
//!     pub enum Num<T> {
//!         U8: u8,
//!         U16: u16,
//!         U32: u32,
//!         U64: u64,
//!         U128: u128,
//!     }
//!     
//!
//!     impl {
//!         #[derive(Clone, Copy, PartialEq, Eq, Hash)]
//!         Self;
//!         #[derive(Clone, Copy, Debug)]
//!         pub(self) A<T, bool>;
//!     }
//!     impl<'a> {
//!         #[derive(Serialize, Deserialize)]
//!         pub(super) B<'a, T>;
//!     }
//!     // -- may also be written as a single block --
//!     // impl<'a> {
//!     //     #[derive(Clone, Copy, PartialEq, Eq, Hash)]
//!     //     Self;
//!     //     #[derive(Clone, Copy, Debug)]
//!     //     pub(self) A<T, bool>;
//!     //     #[derive(Serialize, Deserialize)]
//!     //     pub(super) B<'a, T>;
//!     // }
//! }
//!
//! type_enum! {
//!     enum Date<U> {
//!         Date: NaiveDate,
//!         DateTime: NaiveDateTime,
//!     }
//!     impl<I> {
//!         pub(crate) B<'static, U>;
//!         A<I, U>;
//!     }
//! }
//! ```
//!
//! # Expansion
//! ```
//! struct A<T: Debug, U> {
//!     pub t: T,
//!     u: U,
//! }
//!
//! pub struct B<'a, T>(String, pub &'a T) where T: Copy;
//!
//! pub mod Num {
//!     use super::*;
//!
//!     #[derive(Clone, Copy, PartialEq, Eq, Hash)]
//!     enum Tag {
//!         U8,
//!         U16,
//!         U32,
//!         U64,
//!         U128,
//!     }
//!
//!     use super::A as __A;
//!     #[derive(Clone, Copy, Debug)]
//!     pub(super) enum A {
//!         U8(__A<u8, bool>),
//!         U16(__A<u16, bool>),
//!         U32(__A<u32, bool>),
//!         U64(__A<u64, bool>),
//!         U128(__A<u128, bool>),
//!     }
//!
//!     use super::B as __B;
//!     #[derive(Serialize, Deserialize)]
//!     pub enum B<'a> {
//!         U8(__B<'a, u8>),
//!         U16(__B<'a, u16>),
//!         U32(__B<'a, u32>),
//!         U64(__B<'a, u64>),
//!         U128(__B<'a, u128>),
//!     }
//! }
//!
//! mod Date {
//!     use super::*;
//!     
//!     use super::B as __B;
//!     pub enum B {
//!         Date(__B<'static, NaiveDate>),
//!         DateTime(__B<'static, NaiveDateTime>),
//!     }
//!
//!     use super::B as __B;
//!     pub(super) enum A<I> {
//!         Date(__A<I, NaiveDate>),
//!         DateTime(__A<I, NaiveDateTime>),
//!     }
//! }
//! ```
//!
//! # Publicity
//! Publicity of the enum mapping is forwarded to the publicity of the module.
//!
//! Publicity of each instantiated enum is mapped to it's parent (super) module.

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Brace;
use syn::{
    AngleBracketedGenericArguments, Attribute, Expr, GenericArgument, GenericParam, Generics,
    Ident, Result, Token, Type, Visibility, braced,
};

#[proc_macro]
pub fn type_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let type_enum = syn::parse_macro_input!(input as TypeEnum);
    type_enum.into_token_stream().into()
}

struct TypeEnum {
    def: Def,
    impls: Vec<Impl>,
}

impl Parse for TypeEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            def: input.parse()?,
            impls: {
                let mut impls = Vec::new();
                while !input.is_empty() {
                    impls.push(input.parse()?);
                }
                impls
            },
        })
    }
}

fn arg_ident(arg: &GenericArgument) -> Option<&Ident> {
    match arg {
        GenericArgument::Lifetime(lt) => Some(&lt.ident),
        GenericArgument::Type(Type::Path(tp)) => {
            if tp.qself.is_none() && tp.path.segments.len() == 1 {
                Some(&tp.path.segments.first().unwrap().ident)
            } else {
                None
            }
        }
        GenericArgument::Const(Expr::Path(p)) => {
            if p.qself.is_none() && p.path.segments.len() == 1 {
                Some(&p.path.segments.first().unwrap().ident)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn param_ident(param: &GenericParam) -> &Ident {
    match param {
        GenericParam::Type(ty) => &ty.ident,
        GenericParam::Lifetime(lt) => &lt.lifetime.ident,
        GenericParam::Const(c) => &c.ident,
    }
}

impl ToTokens for TypeEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let items = self.impls.iter().map(|ipml| {
            let items = ipml.items.iter().map(|item| {
                if let ItemInner::Type {
                    name,
                    generic_arguments,
                } = &item.inner
                {
                    let alias = format_ident!("__{name}");

                    let vis = upgrade_visibility(&item.vis);

                    let attrs = &item.attrs;

                    let params = ipml
                        .generics
                        .params
                        .iter()
                        .filter(|p| {
                            let p = param_ident(p);
                            generic_arguments.args.iter().any(|arg| {
                                arg_ident(arg).map(|ident| *ident == *p).unwrap_or_default()
                            })
                        })
                        .cloned()
                        .collect();

                    let generics = Generics {
                        params,
                        ..ipml.generics.clone()
                    };

                    let swap_index = generic_arguments
                        .args
                        .iter()
                        .position(|arg| {
                            if let Some(ident) = arg_ident(arg) {
                                *ident == self.def.swap.ident
                            } else {
                                false
                            }
                        })
                        .expect("otherwise this whole thing is useless");

                    let generic_args = self.def.variants.iter().map(|v| {
                        let mut generic_arguments = generic_arguments.clone();
                        generic_arguments.args[swap_index] = GenericArgument::Type(v.ty.clone());
                        generic_arguments
                    });

                    let variants_ident = self.def.variants.iter().map(|v| &v.name);

                    quote! {
                        use super::#name as #alias;
                        #( #attrs )*
                        #vis enum #name #generics {
                            #(
                                #variants_ident(#alias #generic_args),
                            )*
                        }
                    }
                } else {
                    let attrs = &item.attrs;

                    let vis = upgrade_visibility(&item.vis);

                    let variants_ident = self.def.variants.iter().map(|v| &v.name);

                    quote! {
                        #( #attrs )*
                        #vis enum Tag {
                            #(
                                #variants_ident,
                            )*
                        }
                    }
                }
            });

            quote! {
                #( #items )*
            }
        });

        let vis = &self.def.vis;
        let name = &self.def.name;

        tokens.extend(quote! {
            #vis mod #name {
                use super::*;

                #( #items )*
            }
        })
    }
}

struct Def {
    vis: Visibility,
    _enum: Token![enum],
    name: Ident,
    swap: SwapIdent,
    _brace: Brace,
    variants: Punctuated<Variant, Token![,]>,
}

impl Parse for Def {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            vis: input.parse()?,
            _enum: input.parse()?,
            name: input.parse()?,
            swap: input.parse()?,
            _brace: braced!(content in input),
            variants: content.parse_terminated(Variant::parse, Token![,])?,
        })
    }
}

struct SwapIdent {
    _lt: Token![<],
    ident: Ident,
    _gt: Token![>],
}

impl Parse for SwapIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            _lt: input.parse()?,
            ident: input.parse()?,
            _gt: input.parse()?,
        })
    }
}

struct Variant {
    name: Ident,
    _colon: Token![:],
    ty: Type,
}

impl Parse for Variant {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            name: input.parse()?,
            _colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}

struct Impl {
    _impl: Token![impl],
    generics: Generics,
    _brace: Brace,
    items: Punctuated<Item, Token![;]>,
}

impl Parse for Impl {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            _impl: input.parse()?,
            generics: input.parse()?,
            _brace: braced!(content in input),
            items: content.parse_terminated(Item::parse, Token![;])?,
        })
    }
}

struct Item {
    attrs: Vec<Attribute>,
    vis: Visibility,
    inner: ItemInner,
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            inner: input.parse()?,
        })
    }
}

enum ItemInner {
    Tag,
    Type {
        name: Ident,
        generic_arguments: AngleBracketedGenericArguments,
    },
}

impl Parse for ItemInner {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(if input.peek(Token![Self]) {
            ItemInner::Tag
        } else {
            ItemInner::Type {
                name: input.parse()?,
                generic_arguments: input.parse()?,
            }
        })
    }
}

fn upgrade_visibility(vis: &Visibility) -> Visibility {
    match vis {
        // private → pub(super)
        Visibility::Inherited => {
            let mut segments = Punctuated::new();
            segments.push(syn::PathSegment::from(Ident::new(
                "super",
                Span::call_site(),
            )));

            Visibility::Restricted(syn::VisRestricted {
                pub_token: Default::default(),
                paren_token: Default::default(),
                in_token: None,
                path: Box::new(syn::Path {
                    leading_colon: None,
                    segments,
                }),
            })
        }

        // pub stays pub
        v @ Visibility::Public(_) => v.clone(),

        Visibility::Restricted(r) => {
            let first = r.path.segments.first().unwrap().ident.to_string();

            // do NOT modify pub(crate) or absolute paths
            if r.path.leading_colon.is_some() || first == "crate" {
                return vis.clone();
            }

            // prepend `super::`
            let mut new = r.clone();
            new.path.segments.insert(
                0,
                syn::PathSegment::from(Ident::new("super", Span::call_site())),
            );

            Visibility::Restricted(new)
        }
    }
}

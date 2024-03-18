#![allow(clippy::too_many_arguments)]
use std::fmt::Display;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    parse::Parser, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma,
    Attribute, DeriveInput, Expr, ExprClosure, Ident, ImplGenerics, Path, Result,
    TypeGenerics, WhereClause,
};

use crate::bevy_ecs_path;

pub fn derive_states(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let sources = parse_state_type(&ast).expect("Failed to parse substate sources");

    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut base_trait_path = bevy_ecs_path();
    base_trait_path
        .segments
        .push(format_ident!("schedule").into());

    let struct_name = &ast.ident;

    let mut sections: Vec<proc_macro2::TokenStream> = vec![];

    match sources {
        StateType::Normal => {
            sections.push(simple_states(
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
            sections.push(freely_mutable(
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
        }
        StateType::SubStateSource {
            source_type,
            source_value,
        } => {
            sections.push(freely_mutable(
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
            sections.push(state_with_dependencies(
                format_ident!("SubStates"),
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
            sections.push(substates_with_single_type(
                source_type,
                source_value,
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
        }
        StateType::SubStateFunction {
            types,
            function_body,
        } => {
            sections.push(freely_mutable(
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
            sections.push(state_with_dependencies(
                format_ident!("SubStates"),
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
            sections.push(state_with_computation(
                format_ident!("SubStates"),
                &types,
                format_ident!("exists"),
                function_body,
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
        }
        StateType::ComputedFunction {
            types,
            function_body,
        } => {
            sections.push(state_with_computation(
                format_ident!("ComputedStates"),
                &types,
                format_ident!("compute"),
                function_body,
                &impl_generics,
                &ty_generics,
                &where_clause,
                struct_name,
                &base_trait_path,
            ));
        }
    }

    proc_macro2::TokenStream::from_iter(sections).into()
}

fn freely_mutable(
    impl_generics: &ImplGenerics,
    ty_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    struct_name: &Ident,
    base_trait_path: &Path,
) -> proc_macro2::TokenStream {
    let mut trait_path = base_trait_path.clone();
    trait_path
        .segments
        .push(format_ident!("FreelyMutableState").into());

    quote!(
        impl #impl_generics #trait_path for #struct_name #ty_generics #where_clause {}
    )
}

fn simple_states(
    impl_generics: &ImplGenerics,
    ty_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    struct_name: &Ident,
    base_trait_path: &Path,
) -> proc_macro2::TokenStream {
    let mut trait_path = base_trait_path.clone();
    trait_path.segments.push(format_ident!("States").into());

    quote! {
        impl #impl_generics #trait_path for #struct_name #ty_generics #where_clause {}
    }
}

fn state_with_dependencies(
    dep_trait: Ident,
    impl_generics: &ImplGenerics,
    ty_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    struct_name: &Ident,
    base_trait_path: &Path,
) -> proc_macro2::TokenStream {
    let mut trait_path = base_trait_path.clone();
    trait_path.segments.push(format_ident!("States").into());

    let mut dep_trait_path = base_trait_path.clone();
    dep_trait_path.segments.push(dep_trait.into());

    quote! {
        impl #impl_generics #trait_path for #struct_name #ty_generics #where_clause {
            const DEPENDENCY_DEPTH : usize = <Self as #dep_trait_path>::SourceStates::SET_DEPENDENCY_DEPTH + 1;
        }
    }
}

fn state_with_computation(
    dep_trait: Ident,
    dep_types: &Expr,
    computation_name: Ident,
    computation_body: ExprClosure,
    impl_generics: &ImplGenerics,
    ty_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    struct_name: &Ident,
    base_trait_path: &Path,
) -> proc_macro2::TokenStream {
    let mut trait_path = base_trait_path.clone();
    trait_path.segments.push(dep_trait.into());

    let arguments = computation_body.inputs;
    let body = computation_body.body;

    quote! {
        impl #impl_generics #trait_path for #struct_name #ty_generics #where_clause {
            type SourceStates = #dep_types;

            fn #computation_name(#arguments: <<Self as #trait_path>::SourceStates as StateSet>::OptionalStateSet) -> Option<Self> {
                #body
            }
        }
    }
}

fn substates_with_single_type(
    source_state_type: Path,
    source_state_value: Expr,
    impl_generics: &ImplGenerics,
    ty_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    struct_name: &Ident,
    base_trait_path: &Path,
) -> proc_macro2::TokenStream {
    let mut trait_path = base_trait_path.clone();
    trait_path.segments.push(format_ident!("SubStates").into());

    quote! {
        impl #impl_generics #trait_path for #struct_name #ty_generics #where_clause {
            type SourceStates = #source_state_type;

            fn exists(sources: Option<#source_state_type>) -> Option<Self> {
                if sources == Some(#source_state_value) {
                    Some(Self::default())
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Clone)]
enum StateType {
    Normal,
    SubStateSource {
        source_type: Path,
        source_value: Expr,
    },
    SubStateFunction {
        types: Expr,
        function_body: ExprClosure,
    },
    ComputedFunction {
        types: Expr,
        function_body: ExprClosure,
    },
}

fn parse_state_type(ast: &DeriveInput) -> Result<StateType> {
    let attrs = &ast.attrs;
    let span = ast.span();

    if let Some(source) = parse_source_attr(attrs, span)? {
        return Ok(source);
    }

    if let Some(functional) = parse_computation_attr(attrs, span)? {
        return Ok(functional);
    }

    Ok(StateType::Normal)
}

fn parse_source_attr(attrs: &[Attribute], span: Span) -> Result<Option<StateType>> {
    let result = attrs
        .iter()
        .filter(|a| a.path().is_ident("substate"))
        .filter_map(|meta| {
            let mut source = None;
            let value = meta.parse_nested_meta(|nested| {
                let source_type = nested.path.clone();
                let source_value = nested.value()?.parse::<Expr>()?;
                source = Some(StateType::SubStateSource {
                    source_type,
                    source_value,
                });
                Ok(())
            });
            match source {
                Some(value) => Some(Ok(value)),
                None => match value {
                    Ok(_) => Some(Err(syn::Error::new(
                        span,
                        "Couldn't parse SubStates source",
                    ))),
                    Err(_e) => None,
                },
            }
        })
        .collect::<Result<Vec<_>>>()?;

    if result.len() > 1 {
        return Err(syn::Error::new(
            span,
            "Only one source is allowed for SubStates",
        ));
    }

    Ok(result.first().cloned())
}

#[derive(Debug)]
enum ComputationTypes {
    SubState,
    ComputedState,
}

impl Display for ComputationTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ComputationTypes::SubState => "SubStates",
            ComputationTypes::ComputedState => "ComputedStates",
        })
    }
}

fn parse_computation_attr(attrs: &[Attribute], span: Span) -> Result<Option<StateType>> {
    let result = attrs
        .iter()
        .filter_map(|a| match &a.meta {
            syn::Meta::List(list) => {
                let path = &list.path;

                if path.is_ident("substate") {
                    Some((ComputationTypes::SubState, list))
                } else if path.is_ident("computed") {
                    Some((ComputationTypes::ComputedState, list))
                } else {
                    None
                }
            }
            _ => None,
        })
        .map(|(computation_type, meta)| {
            let parser = Punctuated::<Expr, Comma>::parse_separated_nonempty;
            let content = match parser.parse2(meta.tokens.clone()) {
                Ok(content) => content,
                Err(e) => {
                    return Err(e);
                }
            };

            let mut iterator = content.iter();
            let types = match iterator.next() {
                Some(types) => types.clone(),
                None => {
                    return Err(syn::Error::new(
                        span,
                        format!("no types provided for deriving {computation_type}"),
                    ));
                }
            };

            let function_body = match iterator.next() {
                Some(Expr::Closure(function_body)) => function_body.clone(),
                _ => {
                    return Err(syn::Error::new(
                        span,
                        format!("no closure provided for deriving {computation_type}"),
                    ));
                }
            };

            Ok(match computation_type {
                ComputationTypes::SubState => StateType::SubStateFunction {
                    types,
                    function_body,
                },
                ComputationTypes::ComputedState => StateType::ComputedFunction {
                    types,
                    function_body,
                },
            })
        })
        .collect::<Result<Vec<_>>>()?;

    if result.len() > 1 {
        return Err(syn::Error::new(
            span,
            "Can't define multiple computations on a single state type",
        ));
    }

    Ok(result.first().cloned())
}

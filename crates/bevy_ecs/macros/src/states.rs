use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse::Parse, parse_macro_input, DeriveInput, Path, Token};

use crate::bevy_ecs_path;

struct EventBasedStateStruct {
    event_type: Path,
    process_function: Path,
}

impl Parse for EventBasedStateStruct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let source = input.to_string();

        let event_type = input.parse()?;
        input.parse::<Token![,]>()?;
        let process_function = input.parse()?;
        Ok(EventBasedStateStruct {
            event_type,
            process_function,
        })
    }
}

pub fn derive_states(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut base_trait_path = bevy_ecs_path();
    base_trait_path
        .segments
        .push(format_ident!("schedule").into());

    let mut trait_path = base_trait_path.clone();
    trait_path.segments.push(format_ident!("States").into());

    let mut state_mutation_trait_path = base_trait_path.clone();
    state_mutation_trait_path
        .segments
        .push(format_ident!("StateMutation").into());

    let event_based: Option<EventBasedStateStruct> = ast.attrs.iter().find_map(|v| match &v.meta {
        syn::Meta::List(list) => syn::parse2(list.tokens.clone()).ok(),
        _ => None,
    });

    let mut state_mutation_type_path = base_trait_path.clone();

    if event_based.is_some() {
        state_mutation_type_path
            .segments
            .push(format_ident!("EventBasedStateMutation").into());
    } else {
        state_mutation_type_path
            .segments
            .push(format_ident!("FreeStateMutation").into());
    }

    let struct_name = &ast.ident;

    let event_based_tokens = if let Some(EventBasedStateStruct {
        event_type,
        process_function,
    }) = event_based
    {
        let mut event_based_trait = base_trait_path.clone();
        event_based_trait
            .segments
            .push(format_ident!("EventBasedState").into());

        quote! {
            impl #impl_generics #event_based_trait for #struct_name #ty_generics #where_clause {
                type Event = #event_type;

                fn process(current: Option<Self>, event: &Self::Event) -> Option<Self> {
                    #process_function(current, event)
                }
            }
        }
    } else {
        quote! {}
    };

    quote! {
        impl #impl_generics #trait_path for #struct_name #ty_generics #where_clause {}

        impl #impl_generics #state_mutation_trait_path for #struct_name #ty_generics #where_clause {
            type MutationType = #state_mutation_type_path;
        }

        #event_based_tokens
    }
    .into()
}

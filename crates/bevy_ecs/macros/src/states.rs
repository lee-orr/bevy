use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse::Parse, parse_macro_input, DeriveInput, Path, Token};

use crate::bevy_ecs_path;

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
        .push(format_ident!("FreelyMutableState").into());

    let struct_name = &ast.ident;

    quote! {
        impl #impl_generics #trait_path for #struct_name #ty_generics #where_clause {}

        impl #impl_generics #state_mutation_trait_path for #struct_name #ty_generics #where_clause {
        }
    }
    .into()
}

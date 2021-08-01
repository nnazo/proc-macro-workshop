use darling::ToTokens;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::DataStruct;
use syn::Ident;
use syn::Type;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let out = match &input.data {
        syn::Data::Struct(r#struct) => {
            let struct_name = input.ident;

            let (builder_name, builder_struct) = generate_builder_struct(&struct_name, r#struct);

            let builder_fn_impl = generate_builder_fn(&struct_name, &builder_name, r#struct);

            quote! {
                #builder_fn_impl
                #builder_struct
            }
        }
        syn::Data::Enum(_) | syn::Data::Union(_) => unimplemented!(),
    };

    out.to_token_stream().into()
}

fn get_struct_fields(r#struct: &DataStruct) -> Vec<(&Ident, &Type)> {
    match &r#struct.fields {
        syn::Fields::Named(fields) => fields
            .named
            .iter()
            .map(|field| {
                let ident = field.ident.as_ref().expect("Named");
                let ty = &field.ty;
                (ident, ty)
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    }
}

fn generate_builder_struct(struct_name: &Ident, r#struct: &DataStruct) -> (Ident, TokenStream) {
    let builder_name = format_ident!("{}Builder", struct_name);

    let fields = transform_struct_fields(
        r#struct,
        |(ident, ty)| quote! { #ident: ::std::option::Option<#ty> },
    );

    let setter_fns = transform_struct_fields_map(
        r#struct,
        |(field, ty)| {
            quote! {
                pub fn #field (&mut self, #field: #ty) -> &mut Self {
                    self.#field = ::std::option::Option::Some(#field);
                    self
                }
            }
        },
        |setters| quote! { #( #setters )* },
    );

    let build_fn = generate_build_fn(struct_name, r#struct);

    (
        builder_name.clone(),
        quote! {
            pub struct #builder_name {
                #fields
            }

            impl #builder_name {
                #setter_fns
                #build_fn
            }
        },
    )
}

fn generate_builder_fn(
    struct_name: &Ident,
    builder_name: &Ident,
    r#struct: &DataStruct,
) -> TokenStream {
    let fields = transform_struct_fields(r#struct, |(ident, _ty)| quote! { #ident: None });

    quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #fields
                }
            }
        }
    }
}

fn generate_build_fn(struct_name: &Ident, r#struct: &DataStruct) -> TokenStream {
    let fields = transform_struct_fields(r#struct, |(ident, _ty)| {
        quote! {
            #ident: self.#ident.clone().map_or_else(
                || {
                    let e: ::std::boxed::Box<dyn ::std::error::Error> = format!("Field not present").into();
                    ::std::result::Result::Err(e)
                },
                |field| ::std::result::Result::Ok(field),
            )?
        }
    });

    quote! {
        pub fn build(&mut self) -> ::std::result::Result<#struct_name, ::std::boxed::Box<dyn ::std::error::Error>> {
            Ok(#struct_name {
               #fields
            })
        }
    }
}

fn transform_struct_fields_map<F, G>(r#struct: &DataStruct, f: F, g: G) -> TokenStream
where
    F: FnMut((&Ident, &Type)) -> TokenStream,
    G: FnOnce(Vec<TokenStream>) -> TokenStream,
{
    let fields = get_struct_fields(r#struct)
        .into_iter()
        .map(f)
        .collect::<Vec<_>>();
    g(fields)
}

fn transform_struct_fields<F>(r#struct: &DataStruct, f: F) -> TokenStream
where
    F: FnMut((&Ident, &Type)) -> TokenStream,
{
    transform_struct_fields_map(r#struct, f, |fields| quote! { #( #fields ),* })
}

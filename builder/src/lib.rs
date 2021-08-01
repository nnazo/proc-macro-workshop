use std::str::FromStr;

use darling::ToTokens;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Attribute, DataStruct, DeriveInput, GenericArgument, Ident, Meta, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
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

fn get_struct_fields(
    r#struct: &DataStruct,
) -> Vec<(&Ident, &Type, Option<&Type>, Option<VecInfo<'_>>)> {
    match &r#struct.fields {
        syn::Fields::Named(fields) => fields
            .named
            .iter()
            .map(|field| {
                let ident = field.ident.as_ref().expect("Named");
                let ty = &field.ty;

                let each_name = field.attrs.iter().find_map(|attr| get_each_attr_name(attr));
                let vec_inner = vec_inner_type(ty);

                let vec_info = match vec_inner {
                    Some(inner_ty) => Some(VecInfo {
                        inner_ty,
                        each_method_name: each_name,
                    }),
                    _ => None,
                };

                (ident, ty, option_inner_type(ty), vec_info)
            })
            .collect::<Vec<_>>(),
        _ => unimplemented!(),
    }
}

fn get_each_attr_name(attr: &Attribute) -> Option<(String, Option<String>)> {
    let ident = attr.path.get_ident()?;
    if ident == "builder" {
        let meta = attr.parse_meta();
        let meta = match meta {
            Ok(meta) => Some(meta),
            Err(_) => None,
        }?;

        let meta_list = match meta {
            Meta::List(meta_list) => Some(meta_list),
            _ => None,
        }?;

        let nested_meta = meta_list.nested.first()?;
        let meta = match nested_meta {
            syn::NestedMeta::Meta(attr_meta) => Some(attr_meta),
            _ => None,
        }?;

        match meta {
            Meta::NameValue(name_value) => {
                let ident = name_value.path.get_ident()?;
                if ident == "each" {
                    match &name_value.lit {
                        syn::Lit::Str(name) => {
                            let name = name.value();
                            Some((ident.to_string(), Some(name.replace("\"", ""))))
                        }
                        _ => Some((ident.to_string(), None)),
                    }
                } else {
                    Some((ident.to_string(), None))
                }
            }
            _ => None,
        }
    } else {
        None
    }
}

struct VecInfo<'a> {
    inner_ty: &'a Type,
    each_method_name: Option<(String, Option<String>)>,
}

fn vec_inner_type(ty: &Type) -> Option<&Type> {
    get_inner_type(ty, "Vec")
}

fn option_inner_type(ty: &Type) -> Option<&Type> {
    get_inner_type(ty, "Option")
}

fn get_inner_type<'a>(ty: &'a Type, outer_match: &str) -> Option<&'a Type> {
    match ty {
        Type::Path(type_path) => {
            let path_segment = type_path.path.segments.first();
            match path_segment {
                Some(path_segment) => {
                    if path_segment.ident == outer_match {
                        match &path_segment.arguments {
                            syn::PathArguments::AngleBracketed(args) => {
                                let arg = args.args.first();
                                match arg {
                                    Some(GenericArgument::Type(ty)) => Some(ty),
                                    _ => None,
                                }
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn generate_builder_struct(struct_name: &Ident, r#struct: &DataStruct) -> (Ident, TokenStream) {
    let builder_name = format_ident!("{}Builder", struct_name);

    let fields = transform_struct_fields(r#struct, |(ident, ty, opt, _vec)| {
        let ty = match opt {
            Some(inner_ty) => inner_ty.to_token_stream(),
            _ => ty.to_token_stream(),
        };
        quote! { #ident: ::std::option::Option<#ty> }
    });

    let setter_fns = transform_struct_fields_map(
        r#struct,
        |(field, ty, opt, vec)| {
            let ty = match opt {
                Some(inner_ty) => inner_ty.to_token_stream(),
                None => ty.to_token_stream(),
            };

            let each_setter = match &vec {
                Some(vec_info) => generate_each_setter_fn(field, vec_info),
                _ => None,
            };

            let setter = quote! {
                pub fn #field (&mut self, #field: #ty) -> &mut Self {
                    self.#field = ::std::option::Option::Some(#field);
                    self
                }
            };

            match each_setter {
                Some(Ok(each_setter)) => {
                    // match each_setter {
                    // Some(each_setter) => {
                    let (_attr, each_setter_name) = vec
                        .as_ref()
                        .expect("has attr")
                        .each_method_name
                        .as_ref()
                        .expect("has attr");

                    match each_setter_name.as_ref() {
                        Some(each_setter_name) => {
                            let each_setter_name = each_setter_name
                                .to_token_stream()
                                .to_string()
                                .replace("\"", "");
                            let field = field.to_token_stream();

                            if each_setter_name != field.to_string() {
                                quote! {
                                    #each_setter
                                    #setter
                                }
                            } else {
                                each_setter
                            }
                        }
                        _ => {
                            // compile error
                            each_setter
                        }
                    }
                }
                Some(Err(err)) => err.to_compile_error(),
                _ => setter,
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

fn generate_each_setter_fn(field: &Ident, vec_info: &VecInfo) -> Option<syn::Result<TokenStream>> {
    match &vec_info.each_method_name {
        Some((_attr, each_method_name)) => match each_method_name {
            Some(each_method_name) => {
                let ty = vec_info.inner_ty.to_token_stream();
                let each_method_name = TokenStream::from_str(each_method_name)
                    .expect("Could not make token stream from string for `each = ...`");

                Some(Ok(quote! {
                    pub fn #each_method_name (&mut self, #field: #ty) -> &mut Self {
                        if self.#field.is_none() {
                            self.#field = ::std::option::Option::Some(::std::vec![]);
                        }
                        self.#field.as_mut().expect("Initialized vec").push(#field);
                        self
                    }
                }))
            }
            _ => Some(Err(syn::Error::new(
                field.span(),
                r#"expected `builder(each = "...")`"#,
            ))),
        },
        _ => None,
    }
}

fn generate_builder_fn(
    struct_name: &Ident,
    builder_name: &Ident,
    r#struct: &DataStruct,
) -> TokenStream {
    let fields =
        transform_struct_fields(r#struct, |(ident, _ty, _opt, _vec)| quote! { #ident: None });

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
    let fields = transform_struct_fields(r#struct, |(ident, _ty, opt, vec)| {
        let mut field = quote! {
            #ident: self.#ident.clone()
        };

        if opt.is_none() && vec.is_none() {
            field = quote! {
                #field.map_or_else(
                    || {
                        let e: ::std::boxed::Box<dyn ::std::error::Error> = format!("Field not present").into();
                        ::std::result::Result::Err(e)
                    },
                    |field| ::std::result::Result::Ok(field),
                )?
            }
        } else if vec.is_some() && opt.is_none() {
            field = quote! {
                #field.unwrap_or(::std::vec![])
            }
        }

        field
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
    F: FnMut((&Ident, &Type, Option<&Type>, Option<VecInfo<'_>>)) -> TokenStream,
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
    F: FnMut((&Ident, &Type, Option<&Type>, Option<VecInfo<'_>>)) -> TokenStream,
{
    transform_struct_fields_map(r#struct, f, |fields| quote! { #( #fields ),* })
}

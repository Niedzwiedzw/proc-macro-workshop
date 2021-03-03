use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn option_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    match ty {
        syn::Type::Path(ref p) => p
            .path
            .segments
            .iter()
            .take(1)
            .filter(|segment| segment.ident == "Option")
            .map(|segment| &segment.arguments)
            .filter_map(|arguments| match arguments {
                syn::PathArguments::AngleBracketed(ref inner_ty) => Some(inner_ty),
                _ => None,
            })
            .filter_map(|inner_ty| inner_ty.args.first())
            .filter_map(|inner_ty| match inner_ty {
                syn::GenericArgument::Type(ref t) => Some(t),
                _ => None,
            })
            .last(),
        _ => None,
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let builder_ident = syn::Ident::new(&bname, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let optionized_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if option_inner_type(&field.ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;

        if let Some(inner_type) = option_inner_type(&field.ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_type) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        if option_inner_type(&field.ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let empty_fields = fields.iter().map(|field| {
        let name = &field.ident;
        quote! {
            #name: None
        }
    });
    let expanded = quote! {
        pub struct #builder_ident {
            #(#optionized_fields),*
        }

        impl #builder_ident {
            #(#methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields),*
                })
            }
        }

        impl #name {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #(#empty_fields),*
                }
            }
        }

    };
    expanded.into()
}

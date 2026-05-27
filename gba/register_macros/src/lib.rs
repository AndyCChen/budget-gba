use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::quote;

mod attr;
use attr::*;
use syn::spanned::Spanned;

// macros to generate default read/write functions for 16/32 bit IO registers

#[proc_macro_derive(ReadIo16)]
pub fn read16_io_macro_derive(input: pc::TokenStream) -> pc::TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_read16_io_macro(&ast).into()
}

fn impl_read16_io_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let generated = quote! {
        impl ReadIo16 for #name {
            fn read(&self, byte_select: HalfwordIo) -> u8 {
                match byte_select {
                    HalfwordIo::B0 => self.into_bits() as u8,
                    HalfwordIo::B1 => (self.into_bits() >> 8) as u8,
                }
            }
        }
    };
    generated.into()
}

#[proc_macro_derive(ReadIo32)]
pub fn read32_io_macro_derive(input: pc::TokenStream) -> pc::TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_read32_io_macro_derive(&ast).into()
}

fn impl_read32_io_macro_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let generated = quote! {
        impl ReadIo32 for #name {
            fn read(&self, byte_select: WordIo) -> u8 {
                match byte_select {
                    WordIo::B0 => self.into_bits() as u8,
                    WordIo::B1 => (self.into_bits() >> 8) as u8,
                    WordIo::B2 => (self.into_bits() >> 16) as u8,
                    WordIo::B3 => (self.into_bits() >> 24) as u8,
                }
            }
        }
    };
    generated.into()
}

#[proc_macro_derive(WriteIo16)]
pub fn write16_io_macro_derive(input: pc::TokenStream) -> pc::TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_write16_io_macro_derive(&ast).into()
}

fn impl_write16_io_macro_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let generated = quote! {
        impl WriteIo16 for #name {
            fn write(&mut self, value: u8, byte_select: HalfwordIo) {
                let shift = match byte_select {
                    HalfwordIo::B0 => 0,
                    HalfwordIo::B1 => 8
                };

                let value = u16::from(value) << shift;
                let dst_value = self.into_bits();

                let mask: u16 = 0xFFFF ^ (0xFF << shift);
                *self = Self::from_bits( (dst_value & mask) | value )
            }
        }
    };
    generated.into()
}

#[proc_macro_derive(WriteIo32)]
pub fn write32_io_macro_derive(input: pc::TokenStream) -> pc::TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_write32_io_macro_derive(&ast).into()
}

fn impl_write32_io_macro_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let generated = quote! {
        impl WriteIo32 for #name {
            fn write(&mut self, value: u8, byte_select: WordIo) {
                let shift = match byte_select {
                    WordIo::B0 => 0,
                    WordIo::B1 => 8,
                    WordIo::B2 => 16,
                    WordIo::B3 => 24,
                };

                let value = u32::from(value) << shift;
                let dst_value = self.into_bits();

                let mask: u32 = 0xFFFF_FFFF ^ (0xFF << shift);
                *self = Self::from_bits((dst_value & mask) | value)
            }
        }
    };
    generated.into()
}

/// Auto generate register write functions with a mask to determine which bits are writable.
///
/// Arugments begin with integer types `u16` or `u32`.
/// For example: `#[register_write(u16, mask = 0xFF00)]` will generate a write
/// function that only allows writes the the upper byte while any writes to the lower
/// byte will be masked out and ignored.
/// The mask field is optional which case the mask will be `0xFFFF for u16` or `0xFFFF_FFFF for u32`.
#[proc_macro_attribute]
pub fn register_write(args: pc::TokenStream, input: pc::TokenStream) -> pc::TokenStream {
    match register_write_inner(args.into(), input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn register_write_inner(args: TokenStream, input: TokenStream) -> syn::Result<TokenStream> {
    let mut input = syn::parse2::<syn::ItemStruct>(input)?;
    let mask_type: MaskType = syn::parse2(args.clone())?;

    let name = &input.ident;

    let syn::Fields::Named(fields) = &mut input.fields else {
        return Err(syn::Error::new(
            input.fields.span(),
            "only named fields allowed",
        ));
    };

    let mut offset = 0;
    let mut members: Vec<Member> = Vec::with_capacity(fields.named.len());
    for field in &mut fields.named {
        let member = Member::new(offset, field)?;
        offset += member.bit_width;
        members.push(member);
    }

    println!("{members:#?}");

    let generated = match mask_type {
        MaskType::Mask32(value_mask) => {
            quote! {
                #[bitfield(#args)]
                #input
                impl #name {
                    fn write32(&mut self, value: u8, byte_select: WordIo) {
                        let shift = match byte_select {
                            WordIo::B0 => 0,
                            WordIo::B1 => 8,
                            WordIo::B2 => 16,
                            WordIo::B3 => 24,
                        };

                        let value = (u32::from(value) << shift) & #value_mask;
                        let dst_value = self.into_bits();

                        let mask: u32 = 0xFFFF_FFFF ^ (((#value_mask >> shift) & 0xFF) << shift);
                        *self = Self::from_bits((dst_value & mask) | value);
                    }
                }
            }
        }
        MaskType::Mask16(value_mask) => {
            quote! {
                #[bitfield(#args)]
                #input
                impl #name {
                    fn write16(&mut self, value: u8, byte_select: HalfwordIo) {
                        let shift = match byte_select {
                            HalfwordIo::B0 => 0,
                            HalfwordIo::B1 => 8,
                        };

                        let value = (u16::from(value) << shift) & #value_mask;
                        let dst_value = self.into_bits();

                        let mask: u16 = 0xFFFF ^ (((#value_mask >> shift) & 0xFF) << shift);
                        *self = Self::from_bits((dst_value & mask) | value);
                    }
                }
            }
        }
    };

    Ok(generated.into())
}

#[derive(Debug)]
struct Member {
    offset: usize,
    bit_width: usize,
    access: AccessType,
}

#[derive(Debug)]
enum AccessType {
    ReadWrite,
    Read,
    Write,
}

impl Member {
    fn new(offset: usize, field: &mut syn::Field) -> syn::Result<Self> {
        let mut bit_width: Option<usize> = None;
        let mut access = AccessType::ReadWrite;

        for attr in &field.attrs {
            if let syn::Attribute {
                style: syn::AttrStyle::Outer,
                meta: syn::Meta::List(syn::MetaList { path, tokens, .. }),
                ..
            } = attr
            {
                if path.is_ident("bits") {
                    let BitsAttr { bits } = syn::parse2(tokens.clone())?;
                    bit_width = bits;
                } else if path.is_ident("readonly") || path.is_ident("writeonly") {
                    return Err(syn::Error::new(
                        path.span(),
                        "incorrect format, use #[readonly] or #[writeonly]",
                    ));
                }
            } else if let syn::Attribute {
                style: syn::AttrStyle::Outer,
                meta: syn::Meta::Path(syn::Path { segments, .. }),
                ..
            } = attr
            {
                if segments.len() != 1 {
                    return Err(syn::Error::new(
                        segments.span(),
                        "incorrect format, use #[readonly] or #[writeonly]",
                    ));
                }

                let Some(syn::PathSegment {
                    ident,
                    arguments: syn::PathArguments::None,
                }) = segments.first()
                else {
                    return Err(syn::Error::new(
                        segments.span(),
                        "incorrect format, use #[readonly] or #[writeonly]",
                    ));
                };

                if ident == "readonly" {
                    access = AccessType::Read;
                } else if ident == "writeonly" {
                    access = AccessType::Write;
                }
            };
        }

        // strip out readonly and writeonly attributes after we process them
        field
            .attrs
            .retain(|a| !a.path().is_ident("readonly") && !a.path().is_ident("writeonly"));

        let bit_width = if let Some(bit_width) = bit_width {
            bit_width
        } else {
            let (type_class, bits) = type_info(&field.ty);
            if matches!(type_class, TypeClass::Other) {
                return Err(syn::Error::new(
                    field.ty.span(),
                    "bits need to be explicitly set for this type",
                ));
            }

            bits
        };

        Ok(Self {
            offset,
            bit_width,
            access,
        })
    }
}

enum TypeClass {
    Bool,
    UInt,
    SInt,
    Other,
}

/// Only care about bools, u8, u16, and u32 for my use case
fn type_info(ty: &syn::Type) -> (TypeClass, usize) {
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else {
        return (TypeClass::Other, 0);
    };

    let Some(ident) = path.get_ident() else {
        return (TypeClass::Other, 0);
    };

    match ident {
        _ if ident == "bool" => (TypeClass::Bool, 1),

        _ if ident == "u8" => (TypeClass::UInt, u8::BITS as _),
        _ if ident == "i8" => (TypeClass::SInt, i8::BITS as _),

        _ if ident == "u16" => (TypeClass::UInt, u16::BITS as _),
        _ if ident == "i16" => (TypeClass::SInt, i16::BITS as _),

        _ if ident == "u32" => (TypeClass::UInt, u32::BITS as _),
        _ if ident == "i32" => (TypeClass::SInt, i32::BITS as _),

        _ if ident == "u64" => (TypeClass::UInt, u64::BITS as _),
        _ if ident == "i64" => (TypeClass::SInt, i64::BITS as _),

        _ if ident == "u128" => (TypeClass::UInt, u128::BITS as _),
        _ if ident == "i128" => (TypeClass::SInt, i128::BITS as _),

        _ => (TypeClass::Other, 0),
    }
}

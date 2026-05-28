use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::quote;

mod attr;
use attr::*;
use syn::spanned::Spanned;

// macros to generate default read/write functions for 16/32 bit IO registers

/// Proc macro that wraps around the bitfield-struct macro to create bit-field structs with
/// generated read/write functions for gba io registers.
///
/// Register types can only be `u8`, `u16`, or `u32`.
/// For example: `#[gba_register(u16)]` will generate a u16 register.
/// Attributes `#[readonly]` or `#[writeonly]` can be added to individual fields which will cause the
/// generated read/write functions to mask out the incoming read/write.
/// By default all fields will be read and write.
#[proc_macro_attribute]
pub fn gba_register(args: pc::TokenStream, input: pc::TokenStream) -> pc::TokenStream {
    match gba_register_inner(args.into(), input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn gba_register_inner(args: TokenStream, input: TokenStream) -> syn::Result<TokenStream> {
    let mask_type: RegisterType = syn::parse2(args.clone())?;
    let mut input = syn::parse2::<syn::ItemStruct>(input)?;

    let name = &input.ident;
    let vis = &input.vis;
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

    let (read_mask, write_mask) =
        members
            .iter()
            .fold((0u64, 0u64), |(read_mask, write_mask), member| {
                let bits = ((1 << member.bit_width) - 1) << member.offset;

                match member.access {
                    AccessType::ReadWrite => (read_mask | bits, write_mask | bits),
                    AccessType::Read => (read_mask | bits, write_mask),
                    AccessType::Write => (read_mask, write_mask | bits),
                }
            });

    let generated = match mask_type {
        RegisterType::RegisterU8 => {
            let write_mask = write_mask as u8;
            let read_mask = read_mask as u8;
            quote! {
                #[bitfield(#args)]
                #input

                impl #name {
                    #vis fn write(&mut self, value: u8) {
                        let value = value & #write_mask;
                        let dst_value = self.into_bits();

                        *self = Self::from_bits((dst_value & !#write_mask) | value);
                    }

                    #vis fn read(&self) -> u8 {
                        self.into_bits() & #read_mask
                    }
                }
            }
        }
        RegisterType::RegisterU16 => {
            let write_mask = write_mask as u16;
            let read_mask = read_mask as u16;
            quote! {
                #[bitfield(#args)]
                #input
                impl #name {
                    #vis fn write(&mut self, value: u8, byte_select: HalfwordIo) {
                        let shift = match byte_select {
                            HalfwordIo::B0 => 0,
                            HalfwordIo::B1 => 8,
                        };

                        let value = (u16::from(value) << shift) & #write_mask;
                        let dst_value = self.into_bits();

                        let mask = #write_mask & (0xFF << shift); 
                        *self = Self::from_bits((dst_value & !mask) | value);
                    }

                    #vis fn read(&self, byte_select: HalfwordIo) -> u8 {
                        let shift = match byte_select {
                            HalfwordIo::B0 => 0,
                            HalfwordIo::B1 => 8,
                        };

                        let read_value = self.into_bits() & #read_mask;
                        (read_value >> shift) as _
                    }
                }
            }
        }
        RegisterType::RegisterU32 => {
            let write_mask = write_mask as u32;
            let read_mask = read_mask as u32;
            quote! {
                #[bitfield(#args)]
                #input
                impl #name {
                    #vis fn write(&mut self, value: u8, byte_select: WordIo) {
                        let shift = match byte_select {
                            WordIo::B0 => 0,
                            WordIo::B1 => 8,
                            WordIo::B2 => 16,
                            WordIo::B3 => 24,
                        };

                        let value = (u32::from(value) << shift) & #write_mask;
                        let dst_value = self.into_bits();

                        let mask = #write_mask & (0xFF << shift);
                        *self = Self::from_bits((dst_value & !mask) | value);
                    }

                    #vis fn read(&self, byte_select: WordIo) -> u8 {
                        let shift = match byte_select {
                            WordIo::B0 => 0,
                            WordIo::B1 => 8,
                            WordIo::B2 => 16,
                            WordIo::B3 => 24,
                        };

                        let read_value = self.into_bits() & #read_mask;
                        (read_value >> shift) as _
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

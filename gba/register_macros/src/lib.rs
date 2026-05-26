use proc_macro as pc;
use proc_macro::TokenStream;
use quote::quote;

mod attr;
use attr::*;

// macros to generate default read/write functions for 16/32 bit IO registers

#[proc_macro_derive(ReadIo16)]
pub fn read16_io_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_read16_io_macro(&ast)
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
pub fn read32_io_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_read32_io_macro_derive(&ast)
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
pub fn write16_io_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_write16_io_macro_derive(&ast)
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
pub fn write32_io_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_write32_io_macro_derive(&ast)
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
pub fn register_write(args: pc::TokenStream, item: pc::TokenStream) -> pc::TokenStream {
    match register_write_inner(args.into(), item.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn register_write_inner(args: TokenStream, item: TokenStream) -> syn::Result<TokenStream> {
    let mask_type: MaskType = syn::parse2(args.into())?;
    let ast = syn::parse::<syn::DeriveInput>(item)?;

    let name = &ast.ident;

    let generated = match mask_type {
        MaskType::Mask32(value_mask) => {
            quote! {
                #ast
                impl #name {
                    fn write(&mut self, value: u8, byte_select: WordIo) {
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
                #ast
                impl #name {
                    fn write(&mut self, value: u8, byte_select: HalfwordIo) {
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

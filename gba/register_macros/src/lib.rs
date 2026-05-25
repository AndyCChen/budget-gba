use proc_macro::TokenStream;
use quote::quote;

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
            fn write(&mut self, value: u8, byte_select: Word) {
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

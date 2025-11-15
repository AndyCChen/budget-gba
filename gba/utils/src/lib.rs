use proc_macro::TokenStream;
use quote::quote;

// macros to generate default read/write io functions

#[proc_macro_derive(ReadIo16)]
pub fn read16_io_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_read16_io_macro(&ast)
}

fn impl_read16_io_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let generated = quote! {
        impl ReadIoHalfWord for #name {
            fn read(&self, byte_select: HalfwordIo) -> u8 {
                match byte_select {
                    HalfwordIo::B1 => self.into_bits() as u8,
                    HalfwordIo::B2 => (self.into_bits() >> 8) as u8,
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
        impl ReadIoWord for #name {
            fn read(&self, byte_select: WordIo) -> u8 {
                match byte_select {
                    WordIo::B1 => self.into_bits() as u8,
                    WordIo::B2 => (self.into_bits() >> 8) as u8,
                    WordIo::B3 => (self.into_bits() >> 16) as u8,
                    WordIo::B4 => (self.into_bits() >> 24) as u8,
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
        impl WriteIoHalfword for #name {
            fn write(&mut self, value: u8, byte_select: HalfwordIo) {
                let value = u16::from(value);
                let v = self.into_bits();
                match byte_select {
                    HalfwordIo::B1 => *self = Self::from_bits((v & 0xFF00) | value),
                    HalfwordIo::B2 => *self = Self::from_bits((v & 0x00FF) | (value << 8)),
                }
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
        impl WriteIoword for #name {
            fn write(&mut self, value: u8, byte_select: Word) {
                let value = u32::from(value);
                let v = self.into_bits();
                match byte_select {
                    Word::B1 => *self = Self::from_bits((v & 0xFFFF_FF00) | value),
                    Word::B2 => *self = Self::from_bits((v & 0xFFFF_00FF) | (value << 8)),
                    Word::B3 => *self = Self::from_bits((v & 0xFF00_FFFF) | (value << 16)),
                    Word::B4 => *self = Self::from_bits((v & 0x00FF_FFFF) | (value << 24)),
                }
            }
        }
    };
    generated.into()
}

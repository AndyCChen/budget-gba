use proc_macro2::TokenStream;
use syn::parse::Parse;

pub enum RegisterType {
    RegisterU8,
    RegisterU16,
    RegisterU32,
}

impl Parse for RegisterType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let Ok(ty) = syn::Type::parse(input) else {
            return Err(syn::Error::new(input.span(), "unknown type"));
        };

        let class = uint_type_info(&ty);

        let mask_type = match class {
            TypeClass::U8 => RegisterType::RegisterU8,
            TypeClass::U16 => RegisterType::RegisterU16,
            TypeClass::U32 => RegisterType::RegisterU32,
            TypeClass::Other => {
                return Err(syn::Error::new(input.span(), "type must be u8, u16, u32"));
            }
        };

        // We only care about the type which is u16 or u32,
        // so we consume and discard the rest of the parse stream.
        let _ = input.parse::<TokenStream>();
        Ok(mask_type)
    }
}

enum TypeClass {
    U8,
    U16,
    U32,
    Other,
}

fn uint_type_info(ty: &syn::Type) -> TypeClass {
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else {
        return TypeClass::Other;
    };

    let Some(ident) = path.get_ident() else {
        return TypeClass::Other;
    };

    match ident {
        _ if ident == "u8" => TypeClass::U8,
        _ if ident == "u16" => TypeClass::U16,
        _ if ident == "u32" => TypeClass::U32,
        _ => TypeClass::Other,
    }
}

pub struct BitsAttr {
    pub bits: Option<usize>,
}

impl Parse for BitsAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut output = Self { bits: None };

        if let Ok(bits) = syn::LitInt::parse(input) {
            output.bits = Some(bits.base10_parse()?);
        }

        let _ = input.parse::<TokenStream>(); // consume and discard rest of parse stream

        Ok(output)
    }
}

use syn::{Token, parse::Parse};

pub enum MaskType {
    Mask32(u32),
    Mask16(u16),
}

impl Parse for MaskType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let Ok(ty) = syn::Type::parse(input) else {
            return Err(syn::Error::new(input.span(), "unknown type"));
        };

        let class = uint_type_info(&ty);

        let mask_type = match class {
            TypeClass::U16 => MaskType::Mask16(u16::MAX),
            TypeClass::U32 => MaskType::Mask32(u32::MAX),
            TypeClass::Other => {
                return Err(syn::Error::new(input.span(), "type must be u16 or u32"));
            }
        };

        if input.is_empty() {
            return Ok(mask_type);
        }

        <Token![,]>::parse(input)?;
        if let Ok(mask_ident) = syn::Ident::parse(input)
            && mask_ident != "mask"
        {
            return Err(syn::Error::new(mask_ident.span(), "unknown argument"));
        }

        <Token![=]>::parse(input)?;
        let mask = syn::LitInt::parse(input)?;

        let mask_type = match mask_type {
            MaskType::Mask32(_) => MaskType::Mask32(mask.base10_parse()?),
            MaskType::Mask16(_) => MaskType::Mask16(mask.base10_parse()?),
        };

        Ok(mask_type)
    }
}

enum TypeClass {
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
        _ if ident == "u16" => TypeClass::U16,
        _ if ident == "u32" => TypeClass::U32,
        _ => TypeClass::Other,
    }
}
use std::borrow::Cow;
use std::{iter, mem};

use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    Attribute, Data, Field, Fields, Generics, Lifetime, LifetimeParam, LitStr, Path, Type,
    TypePath, Variant, Meta,
};

use crate::component::schema::{EnumSchema, NamedStructSchema, Root};
use crate::doc_comment::CommentAttributes;
use crate::path::media_type::{DefaultSchema, MediaTypeAttr, ParsedType, Schema};
use crate::{
    as_tokens_or_diagnostics, parse_utils, Array, Diagnostics, OptionExt, ToTokensDiagnostics,
};

use super::{
    DeriveIntoResponsesValue, DeriveResponseValue, DeriveToResponseValue, ResponseTuple,
    ResponseTupleInner, ResponseValue,
};

pub struct ToResponse<'r> {
    ident: Ident,
    lifetime: Lifetime,
    generics: Generics,
    response: ResponseTuple<'r>,
}

impl<'r> ToResponse<'r> {
    const LIFETIME: &'static str = "'__r";

    pub fn new(
        attributes: Vec<Attribute>,
        data: &'r Data,
        generics: Generics,
        ident: Ident,
    ) -> Result<ToResponse<'r>, Diagnostics> {
        let response = match &data {
            Data::Struct(struct_value) => match &struct_value.fields {
                Fields::Named(fields) => {
                    ToResponseNamedStructResponse::new(&attributes, &ident, &fields.named)?.0
                }
                Fields::Unnamed(fields) => {
                    let field = fields
                        .unnamed
                        .iter()
                        .next()
                        .expect("Unnamed struct must have 1 field");

                    ToResponseUnnamedStructResponse::new(&attributes, &field.ty, &field.attrs)?.0
                }
                Fields::Unit => ToResponseUnitStructResponse::new(&attributes)?.0,
            },
            Data::Enum(enum_value) => {
                EnumResponse::new(&ident, &enum_value.variants, &attributes)?.0
            }
            Data::Union(_) => {
                return Err(Diagnostics::with_span(
                    ident.span(),
                    "`ToResponse` does not support `Union` type",
                ))
            }
        };

        let lifetime = Lifetime::new(ToResponse::LIFETIME, Span::call_site());

        Ok(Self {
            ident,
            lifetime,
            generics,
            response,
        })
    }
}

impl ToTokensDiagnostics for ToResponse<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) -> Result<(), Diagnostics> {
        let (_, ty_generics, where_clause) = self.generics.split_for_impl();

        let lifetime = &self.lifetime;
        let ident = &self.ident;
        let name = ident.to_string();
        let response = as_tokens_or_diagnostics!(&self.response);

        let mut to_response_generics = self.generics.clone();
        to_response_generics
            .params
            .push(syn::GenericParam::Lifetime(LifetimeParam::new(
                lifetime.clone(),
            )));
        let (to_response_impl_generics, _, _) = to_response_generics.split_for_impl();

        tokens.extend(quote! {
            impl #to_response_impl_generics utoipa::ToResponse <#lifetime> for #ident #ty_generics #where_clause {
                fn response() -> (& #lifetime str, utoipa::openapi::RefOr<utoipa::openapi::response::Response>) {
                    (#name, #response.into())
                }
            }
        });

        Ok(())
    }
}

pub struct IntoResponses {
    pub attributes: Vec<Attribute>,
    pub data: Data,
    pub generics: Generics,
    pub ident: Ident,
}

impl ToTokensDiagnostics for IntoResponses {
    fn to_tokens(&self, tokens: &mut TokenStream) -> Result<(), Diagnostics> {
        // Collect response tuples and optional flatten extensions.
        let (responses, flatten_extends): (Array<TokenStream>, Vec<TokenStream>) = match &self.data {
            Data::Struct(struct_value) => {
                let responses = match &struct_value.fields {
                    Fields::Named(fields) => {
                        let response =
                            NamedStructResponse::new(&self.attributes, &self.ident, &fields.named)?.0;
                        let status = &response.status_code;
                        let response_tokens = as_tokens_or_diagnostics!(&response);
                        Array::from_iter(iter::once(quote!((#status, #response_tokens))))
                    }
                    Fields::Unnamed(fields) => {
                        let field = fields
                            .unnamed
                            .iter()
                            .next()
                            .expect("Unnamed struct must have 1 field");
                        let response =
                            UnnamedStructResponse::new(&self.attributes, &field.ty, &field.attrs)?.0;
                        let status = &response.status_code;
                        let response_tokens = as_tokens_or_diagnostics!(&response);
                        Array::from_iter(iter::once(quote!((#status, #response_tokens))))
                    }
                    Fields::Unit => {
                        let response = UnitStructResponse::new(&self.attributes)?.0;
                        let status = &response.status_code;
                        let response_tokens = as_tokens_or_diagnostics!(&response);
                        Array::from_iter(iter::once(quote!((#status, #response_tokens))))
                    }
                };

                (responses, Vec::new())
            }
            Data::Enum(enum_value) => {
                // Helper function to check #[response(flatten)] on variant
                fn variant_has_flatten(attrs: &[Attribute]) -> Result<bool, Diagnostics> {
                    for attr in attrs {
                        if attr.path().is_ident("response") {
                            let args = attr
                                .parse_args_with(|input: ParseStream| {
                                    Punctuated::<Meta, Comma>::parse_terminated(input)
                                })
                                .map_err(Diagnostics::from)?;
                            for meta in args {
                                if let Meta::Path(path) = &meta {
                                    if path.is_ident("flatten") {
                                        return Ok(true);
                                    }
                                }
                            }
                        }
                    }
                    Ok(false)
                }

                let mut normal_pairs: Vec<TokenStream> = Vec::new();
                let mut flatten_extends: Vec<TokenStream> = Vec::new();

                for variant in enum_value.variants.iter() {
                    let is_flatten = variant_has_flatten(&variant.attrs)?;

                    if is_flatten {
                        // Only allow unnamed variant with exactly one field.
                        match &variant.fields {
                            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                                let ty = &fields.unnamed.first().unwrap().ty;
                                flatten_extends.push(quote! {
                                    __utoipa_merge_responses(&mut __map, <#ty as utoipa::IntoResponses>::responses());
                                });
                            }
                            _ => {
                                return Err(Diagnostics::with_span(
                                    variant.span(),
                                    "`#[response(flatten)]` is only supported on unnamed enum variants with exactly one field",
                                ));
                            }
                        }
                        continue;
                    }

                    // Non-flatten variants: keep original behavior.
                    match &variant.fields {
                        Fields::Named(fields) => {
                            let response = NamedStructResponse::new(
                                &variant.attrs,
                                &variant.ident,
                                &fields.named,
                            )?
                            .0;
                            let status = &response.status_code;
                            let response_tokens = as_tokens_or_diagnostics!(&response);
                            normal_pairs
                                .push(quote!((#status, utoipa::openapi::RefOr::from(#response_tokens))));
                        }
                        Fields::Unnamed(fields) => {
                            let field = fields
                                .unnamed
                                .iter()
                                .next()
                                .expect("Unnamed enum variant must have 1 field");
                            let response =
                                UnnamedStructResponse::new(&variant.attrs, &field.ty, &field.attrs)?.0;
                            let status = &response.status_code;
                            let response_tokens = as_tokens_or_diagnostics!(&response);
                            normal_pairs
                                .push(quote!((#status, utoipa::openapi::RefOr::from(#response_tokens))));
                        }
                        Fields::Unit => {
                            let response = UnitStructResponse::new(&variant.attrs)?.0;
                            let status = &response.status_code;
                            let response_tokens = as_tokens_or_diagnostics!(&response);
                            normal_pairs
                                .push(quote!((#status, utoipa::openapi::RefOr::from(#response_tokens))));
                        }
                    }
                }

                (Array::from_iter(normal_pairs), flatten_extends)
            }
            Data::Union(_) => {
                return Err(Diagnostics::with_span(
                    self.ident.span(),
                    "`IntoResponses` does not support `Union` type",
                ))
            }
        };

        let ident = &self.ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        let responses_from_iter = if responses.len() > 0 {
            Some(quote!( .responses_from_iter(#responses)))
        } else {
            None
        };

        // --- begin helpers for axum_extras codegen ---
        fn has_utoipa_to_axum(attrs: &[Attribute]) -> Result<bool, Diagnostics> {
            for attr in attrs {
                if attr.path().is_ident("utoipa") {
                    let args = attr
                        .parse_args_with(|input: ParseStream| {
                            Punctuated::<Meta, Comma>::parse_terminated(input)
                        })
                        .map_err(Diagnostics::from)?;
                    for meta in args {
                        if let Meta::Path(path) = &meta {
                            if path.is_ident("to_axum") {
                                return Ok(true);
                            }
                        }
                    }
                }
            }
            Ok(false)
        }

        fn response_has_flatten(attrs: &[Attribute]) -> Result<bool, Diagnostics> {
            for attr in attrs {
                if attr.path().is_ident("response") {
                    let args = attr
                        .parse_args_with(|input: ParseStream| {
                            Punctuated::<Meta, Comma>::parse_terminated(input)
                        })
                        .map_err(Diagnostics::from)?;
                    for meta in args {
                        if let Meta::Path(path) = &meta {
                            if path.is_ident("flatten") {
                                return Ok(true);
                            }
                        }
                    }
                }
            }
            Ok(false)
        }

        fn response_status_u16(attrs: &[Attribute]) -> Result<Option<u16>, Diagnostics> {
            for attr in attrs {
                if attr.path().is_ident("response") {
                    let args = attr
                        .parse_args_with(|input: ParseStream| {
                            Punctuated::<Meta, Comma>::parse_terminated(input)
                        })
                        .map_err(Diagnostics::from)?;
                    for meta in args {
                        if let Meta::NameValue(nv) = &meta {
                            if nv.path.is_ident("status") {
                                // Support status = 200 / 403 / ...
                                if let syn::Expr::Lit(expr_lit) = &nv.value {
                                    if let syn::Lit::Int(li) = &expr_lit.lit {
                                        let v = li.base10_parse::<u16>().map_err(Diagnostics::from)?;
                                        return Ok(Some(v));
                                    }
                                    if let syn::Lit::Str(ls) = &expr_lit.lit {
                                        let v = ls
                                            .value()
                                            .parse::<u16>()
                                            .map_err(|e| Diagnostics::from(syn::Error::new(ls.span(), e.to_string())))?;
                                        return Ok(Some(v));
                                    }
                                }
                                // Also support status = "200" via expr
                                if let syn::Expr::Path(_) = &nv.value {
                                    // unsupported (non-literal)
                                    return Ok(None);
                                }
                                return Ok(None);
                            }
                        }
                    }
                }
            }
            Ok(None)
        }
        // --- end helpers for axum_extras codegen ---

        let to_axum = has_utoipa_to_axum(&self.attributes)?;

        if flatten_extends.is_empty() {
            tokens.extend(quote! {
                impl #impl_generics utoipa::IntoResponses for #ident #ty_generics #where_clause {
                    fn responses() -> std::collections::BTreeMap<String, utoipa::openapi::RefOr<utoipa::openapi::response::Response>> {
                        utoipa::openapi::response::ResponsesBuilder::new()
                            #responses_from_iter
                            .build()
                            .into()
                    }
                }
            });
        } else {
            tokens.extend(quote! {
                impl #impl_generics utoipa::IntoResponses for #ident #ty_generics #where_clause {
                    fn responses() -> std::collections::BTreeMap<String, utoipa::openapi::RefOr<utoipa::openapi::response::Response>> {
                        fn __utoipa_merge_responses(
                            dst: &mut std::collections::BTreeMap<
                                String,
                                utoipa::openapi::RefOr<utoipa::openapi::response::Response>,
                            >,
                            src: std::collections::BTreeMap<
                                String,
                                utoipa::openapi::RefOr<utoipa::openapi::response::Response>,
                            >,
                        ) {
                            for (status, incoming) in src {
                                match dst.get(&status).cloned() {
                                    None => {
                                        dst.insert(status, incoming);
                                    }
                                    Some(existing) => {
                                        let merged = __utoipa_merge_response(existing, incoming);
                                        dst.insert(status, merged);
                                    }
                                }
                            }
                        }

                        fn __utoipa_merge_response(
                            existing: utoipa::openapi::RefOr<utoipa::openapi::response::Response>,
                            incoming: utoipa::openapi::RefOr<utoipa::openapi::response::Response>,
                        ) -> utoipa::openapi::RefOr<utoipa::openapi::response::Response> {
                            use utoipa::openapi::{RefOr, response::Response, schema::{Schema, OneOfBuilder}};

                            // If they are literally equal, keep one.
                            // NOTE: we must avoid using `incoming` after it has been moved.
                            match (existing, incoming) {
                                (ex, inc) if ex == inc => {
                                    return ex;
                                }
                                // We only merge inline responses. If there is a $ref, prefer the incoming.
                                (RefOr::T(mut ex), RefOr::T(inc)) => {
                                    // continue below with `ex` and `inc`

                                    // If content types don't overlap, prefer incoming (keeps behavior simple and deterministic).
                                    let overlap = ex
                                        .content
                                        .keys()
                                        .any(|k| inc.content.contains_key(k));
                                    if !overlap {
                                        return RefOr::T(inc);
                                    }

                                    // Merge overlapping content-types by producing oneOf schemas.
                                    for (ct, inc_ct) in inc.content {
                                        match ex.content.remove(&ct) {
                                            None => {
                                                ex.content.insert(ct, inc_ct);
                                            }
                                            Some(mut ex_ct) => {
                                                let ex_schema = ex_ct.schema.take();
                                                let inc_schema = inc_ct.schema.clone();

                                                // If schemas are identical, keep one; otherwise wrap into oneOf.
                                                let merged_schema = match (ex_schema, inc_schema) {
                                                    (Some(a), Some(b)) if a == b => Some(a),
                                                    (Some(a), Some(b)) => {
                                                        Some(RefOr::T(Schema::OneOf(
                                                            OneOfBuilder::new().item(a).item(b).build(),
                                                        )))
                                                    }
                                                    (None, Some(b)) => Some(b),
                                                    (Some(a), None) => Some(a),
                                                    (None, None) => None,
                                                };

                                                ex_ct.schema = merged_schema;
                                                ex.content.insert(ct, ex_ct);
                                            }
                                        }
                                    }

                                    return RefOr::T(ex);
                                }
                                // Fallback: if there is a $ref, prefer the incoming.
                                (_, inc) => {
                                    return inc;
                                }
                            }
                        }

                        let mut __map: std::collections::BTreeMap<String, utoipa::openapi::RefOr<utoipa::openapi::response::Response>> =
                            utoipa::openapi::response::ResponsesBuilder::new()
                                #responses_from_iter
                                .build()
                                .into();
                        #(#flatten_extends)*
                        __map
                    }
                }
            });
        }

        // --- SchemaReferences codegen (collect referenced schemas for OpenApi components) ---
        fn field_has_to_schema(attrs: &[Attribute]) -> bool {
            attrs.iter().any(|a| a.path().is_ident("to_schema"))
        }

        // Collect types which must be added to components/schemas.
        let mut schema_ref_tys: Vec<&Type> = Vec::new();

        match &self.data {
            Data::Struct(struct_value) => match &struct_value.fields {
                Fields::Named(fields) => {
                    for field in fields.named.iter() {
                        if field_has_to_schema(&field.attrs) {
                            schema_ref_tys.push(&field.ty);
                        }
                    }
                }
                Fields::Unnamed(fields) => {
                    if let Some(field) = fields.unnamed.first() {
                        if field_has_to_schema(&field.attrs) {
                            schema_ref_tys.push(&field.ty);
                        }
                    }
                }
                Fields::Unit => {}
            },
            Data::Enum(enum_value) => {
                // Include schema refs from flatten variants and from fields explicitly annotated with #[to_schema].
                for variant in enum_value.variants.iter() {
                    let is_flatten = response_has_flatten(&variant.attrs)?;
                    if is_flatten {
                        if let Fields::Unnamed(fields) = &variant.fields {
                            if let Some(field) = fields.unnamed.first() {
                                schema_ref_tys.push(&field.ty);
                            }
                        }
                        continue;
                    }

                    match &variant.fields {
                        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                            let field = fields.unnamed.first().unwrap();
                            if field_has_to_schema(&field.attrs) {
                                schema_ref_tys.push(&field.ty);
                            }
                        }
                        Fields::Named(fields) => {
                            for field in fields.named.iter() {
                                if field_has_to_schema(&field.attrs) {
                                    schema_ref_tys.push(&field.ty);
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            Data::Union(_) => {}
        }

        if !schema_ref_tys.is_empty() {
            let schema_refs = schema_ref_tys.into_iter().map(|ty| {
                quote! {
                    <#ty as utoipa::ToSchema>::schemas(schemas);
                }
            });

            tokens.extend(quote! {
                impl #impl_generics utoipa::__dev::SchemaReferences for #ident #ty_generics #where_clause {
                    fn schemas(
                        schemas: &mut Vec<
                            (
                                String,
                                utoipa::openapi::RefOr<utoipa::openapi::schema::Schema>,
                            ),
                        >,
                    ) {
                        #(#schema_refs)*
                    }
                }
            });
        }
        // --- end SchemaReferences codegen ---
        // --- axum_extras IntoResponse codegen ---
        if to_axum {
            let axum_impl = match &self.data {
                Data::Struct(struct_value) => {
                    // Requires a literal status on the container #[response(status = ...)]
                    let Some(status) = response_status_u16(&self.attributes)? else {
                        return Err(Diagnostics::with_span(
                            self.ident.span(),
                            "`#[utoipa(to_axum)]` requires a literal `#[response(status = ...)]` on the type",
                        ));
                    };

                    // Determine how to serialize body for struct: Json(self)
                    let status_u16 = status;
                    quote! {
                        impl #impl_generics axum::response::IntoResponse for #ident #ty_generics #where_clause
                        where
                            #ident #ty_generics: serde::Serialize,
                        {
                            fn into_response(self) -> axum::response::Response {
                                let status = axum::http::StatusCode::from_u16(#status_u16)
                                    .expect("valid status code");
                                (status, axum::Json(self)).into_response()
                            }
                        }
                    }
                }
                Data::Enum(enum_value) => {
                    // Build match arms. Flatten arms delegate to inner IntoResponse.
                    let mut arms: Vec<TokenStream> = Vec::new();

                    for variant in enum_value.variants.iter() {
                        let is_flatten = response_has_flatten(&variant.attrs)?;

                        if is_flatten {
                            // Only unnamed with exactly one field.
                            match &variant.fields {
                                Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                                    let v_ident = &variant.ident;
                                    arms.push(quote! {
                                        Self::#v_ident(inner) => axum::response::IntoResponse::into_response(inner)
                                    });
                                }
                                _ => {
                                    return Err(Diagnostics::with_span(
                                        variant.span(),
                                        "`#[utoipa(to_axum)]` with `#[response(flatten)]` requires an unnamed enum variant with exactly one field",
                                    ));
                                }
                            }
                            continue;
                        }

                        // Non-flatten variants must have literal status.
                        let Some(status) = response_status_u16(&variant.attrs)? else {
                            return Err(Diagnostics::with_span(
                                variant.span(),
                                "`#[utoipa(to_axum)]` requires a literal `#[response(status = ...)]` on each non-flatten variant",
                            ));
                        };
                        let status_u16 = status;
                        let v_ident = &variant.ident;

                        match &variant.fields {
                            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                                arms.push(quote! {
                                    Self::#v_ident(inner) => {
                                        let status = axum::http::StatusCode::from_u16(#status_u16)
                                            .expect("valid status code");
                                        (status, axum::Json(inner)).into_response()
                                    }
                                });
                            }
                            Fields::Unit => {
                                arms.push(quote! {
                                    Self::#v_ident => {
                                        let status = axum::http::StatusCode::from_u16(#status_u16)
                                            .expect("valid status code");
                                        (status, axum::Json(())).into_response()
                                    }
                                });
                            }
                            _ => {
                                return Err(Diagnostics::with_span(
                                    variant.span(),
                                    "`#[utoipa(to_axum)]` currently supports only unit variants or unnamed variants with exactly one field",
                                ));
                            }
                        }
                    }

                    quote! {
                        impl #impl_generics axum::response::IntoResponse for #ident #ty_generics #where_clause
                        where
                            #ident #ty_generics: serde::Serialize,
                        {
                            fn into_response(self) -> axum::response::Response {
                                match self {
                                    #(#arms,)*
                                }
                            }
                        }
                    }
                }
                Data::Union(_) => {
                    quote! {}
                }
            };

            #[cfg(feature = "axum_extras")]
            {
                tokens.extend(axum_impl);
            }
        
            #[cfg(not(feature = "axum_extras"))]
            {
                return Err(Diagnostics::with_span(
                    self.ident.span(),
                    "`#[utoipa(to_axum)]` requires enabling `utoipa` feature `axum_extras`",
                ));
            }
        }

        Ok(())
    }
}

trait Response {
    fn to_type(ident: &Ident) -> Type {
        let path = Path::from(ident.clone());
        let type_path = TypePath { path, qself: None };
        Type::Path(type_path)
    }

    fn has_no_field_attributes(attribute: &Attribute) -> (bool, &'static str) {
        const ERROR: &str = "Unexpected field attribute";

        let ident = attribute.path().get_ident().unwrap();
        match &*ident.to_string() {
            // `#[to_schema]` is allowed also on named fields (struct named fields / named enum variant fields)
            // to request inlining of that field's schema.
            "to_schema" => (true, ERROR),

            // The following response-specific field attributes are still only supported on unnamed fields.
            "ref_response" => (false, "Unexpected field attribute, field attributes are only supported at unnamed fields"),
            "content" => (false, "Unexpected field attribute, field attributes are only supported at unnamed fields"),
            "to_response" => (false, "Unexpected field attribute, field attributes are only supported at unnamed fields"),
            _ => (true, ERROR),
        }
    }

    fn validate_attributes<'a, I: IntoIterator<Item = &'a Attribute>>(
        attributes: I,
        validate: impl Fn(&Attribute) -> (bool, &'static str) + 'a,
    ) -> impl Iterator<Item = Diagnostics> {
        attributes.into_iter().filter_map(move |attribute| {
            let (valid, error_message) = validate(attribute);
            if !valid {
                Some(Diagnostics::with_span(attribute.span(), error_message))
            } else {
                None
            }
        })
    }
}

struct UnnamedStructResponse<'u>(ResponseTuple<'u>);

impl Response for UnnamedStructResponse<'_> {}

impl<'u> UnnamedStructResponse<'u> {
    fn new(
        attributes: &[Attribute],
        ty: &'u Type,
        inner_attributes: &[Attribute],
    ) -> Result<Self, Diagnostics> {
        let is_inline = inner_attributes
            .iter()
            .any(|attribute| attribute.path().get_ident().unwrap() == "to_schema");
        let ref_response = inner_attributes
            .iter()
            .any(|attribute| attribute.path().get_ident().unwrap() == "ref_response");
        let to_response = inner_attributes
            .iter()
            .any(|attribute| attribute.path().get_ident().unwrap() == "to_response");

        if is_inline && (ref_response || to_response) {
            return Err(Diagnostics::with_span(ty.span(), "Attribute `to_schema` cannot be used with `ref_response` and `to_response` attribute"));
        }
        let mut derive_value = DeriveIntoResponsesValue::from_attributes(attributes)?
            .expect("`IntoResponses` must have `#[response(...)]` attribute");
        let description = {
            let s = CommentAttributes::from_attributes(attributes).as_formatted_string();
            parse_utils::LitStrOrExpr::LitStr(LitStr::new(&s, Span::call_site()))
        };
        let status_code = mem::take(&mut derive_value.status);

        let response = match (ref_response, to_response) {
            (false, false) => Self(
                (
                    status_code,
                    ResponseValue::from_derive_into_responses_value(
                        derive_value,
                        ParsedType {
                            ty: Cow::Borrowed(ty),
                            is_inline,
                        },
                        description,
                    ),
                )
                    .into(),
            ),
            (true, false) => Self(ResponseTuple {
                inner: Some(ResponseTupleInner::Ref(ParsedType {
                    ty: Cow::Borrowed(ty),
                    is_inline: false,
                })),
                status_code,
            }),
            (false, true) => Self(ResponseTuple {
                inner: Some(ResponseTupleInner::Ref(ParsedType {
                    ty: Cow::Borrowed(ty),
                    is_inline: true,
                })),
                status_code,
            }),
            (true, true) => {
                return Err(Diagnostics::with_span(
                    ty.span(),
                    "Cannot define `ref_response` and `to_response` attribute simultaneously",
                ))
            }
        };

        Ok(response)
    }
}

struct NamedStructResponse<'n>(ResponseTuple<'n>);

impl Response for NamedStructResponse<'_> {}

impl NamedStructResponse<'_> {
    fn new(
        attributes: &[Attribute],
        ident: &Ident,
        fields: &Punctuated<Field, Comma>,
    ) -> Result<Self, Diagnostics> {
        if let Some(diagnostics) =
            Self::validate_attributes(attributes, Self::has_no_field_attributes)
                .chain(Self::validate_attributes(
                    fields.iter().flat_map(|field| &field.attrs),
                    Self::has_no_field_attributes,
                ))
                .collect::<Option<Diagnostics>>()
        {
            return Err(diagnostics);
        }

        let mut derive_value = DeriveIntoResponsesValue::from_attributes(attributes)?
            .expect("`IntoResponses` must have `#[response(...)]` attribute");
        let description = {
            let s = CommentAttributes::from_attributes(attributes).as_formatted_string();
            parse_utils::LitStrOrExpr::LitStr(LitStr::new(&s, Span::call_site()))
        };
        let status_code = mem::take(&mut derive_value.status);
        let inline_schema = NamedStructSchema::new(
            &Root {
                ident,
                attributes,
                generics: &Generics::default(),
            },
            fields,
            Vec::new(),
        )?;

        let ty = Self::to_type(ident);

        Ok(Self(
            (
                status_code,
                ResponseValue::from_derive_into_responses_value(
                    derive_value,
                    Schema::Default(DefaultSchema::Raw {
                        tokens: inline_schema.to_token_stream(),
                        ty: Cow::Owned(ty),
                    }),
                    description,
                ),
            )
                .into(),
        ))
    }
}

struct UnitStructResponse<'u>(ResponseTuple<'u>);

impl Response for UnitStructResponse<'_> {}

impl UnitStructResponse<'_> {
    fn new(attributes: &[Attribute]) -> Result<Self, Diagnostics> {
        if let Some(diagnostics) =
            Self::validate_attributes(attributes, Self::has_no_field_attributes)
                .collect::<Option<Diagnostics>>()
        {
            return Err(diagnostics);
        }

        let mut derive_value = DeriveIntoResponsesValue::from_attributes(attributes)?
            .expect("`IntoResponses` must have `#[response(...)]` attribute");
        let status_code = mem::take(&mut derive_value.status);
        let description = {
            let s = CommentAttributes::from_attributes(attributes).as_formatted_string();
            parse_utils::LitStrOrExpr::LitStr(LitStr::new(&s, Span::call_site()))
        };

        Ok(Self(
            (
                status_code,
                ResponseValue::from_derive_into_responses_value(
                    derive_value,
                    Schema::Default(DefaultSchema::None),
                    description,
                ),
            )
                .into(),
        ))
    }
}

struct ToResponseNamedStructResponse<'p>(ResponseTuple<'p>);

impl Response for ToResponseNamedStructResponse<'_> {}

impl<'p> ToResponseNamedStructResponse<'p> {
    fn new(
        attributes: &[Attribute],
        ident: &Ident,
        fields: &Punctuated<Field, Comma>,
    ) -> Result<Self, Diagnostics> {
        if let Some(diagnostics) =
            Self::validate_attributes(attributes, Self::has_no_field_attributes)
                .chain(Self::validate_attributes(
                    fields.iter().flat_map(|field| &field.attrs),
                    Self::has_no_field_attributes,
                ))
                .collect::<Option<Diagnostics>>()
        {
            return Err(diagnostics);
        }

        let derive_value = DeriveToResponseValue::from_attributes(attributes)?;
        let description = {
            let s = CommentAttributes::from_attributes(attributes).as_formatted_string();
            parse_utils::LitStrOrExpr::LitStr(LitStr::new(&s, Span::call_site()))
        };
        let ty = Self::to_type(ident);

        let inline_schema = NamedStructSchema::new(
            &Root {
                ident,
                attributes,
                generics: &Generics::default(),
            },
            fields,
            Vec::new(),
        )?;

        let response_value = if let Some(derive_value) = derive_value {
            ResponseValue::from_derive_to_response_value(
                derive_value,
                Schema::Default(DefaultSchema::Raw {
                    tokens: inline_schema.to_token_stream(),
                    ty: Cow::Owned(ty),
                }),
                description,
            )
        } else {
            ResponseValue::from_schema(
                Schema::Default(DefaultSchema::Raw {
                    tokens: inline_schema.to_token_stream(),
                    ty: Cow::Owned(ty),
                }),
                description,
            )
        };
        // response_value.response_type = Some(response_type);

        Ok(Self(response_value.into()))
    }
}

struct ToResponseUnnamedStructResponse<'c>(ResponseTuple<'c>);

impl Response for ToResponseUnnamedStructResponse<'_> {}

impl<'u> ToResponseUnnamedStructResponse<'u> {
    fn new(
        attributes: &[Attribute],
        ty: &'u Type,
        inner_attributes: &[Attribute],
    ) -> Result<Self, Diagnostics> {
        if let Some(diagnostics) =
            Self::validate_attributes(attributes, Self::has_no_field_attributes)
                .chain(Self::validate_attributes(inner_attributes, |attribute| {
                    const ERROR: &str =
                "Unexpected attribute, `content` is only supported on unnamed field enum variant";
                    if attribute.path().get_ident().unwrap() == "content" {
                        (false, ERROR)
                    } else {
                        (true, ERROR)
                    }
                }))
                .collect::<Option<Diagnostics>>()
        {
            return Err(diagnostics);
        }
        let derive_value = DeriveToResponseValue::from_attributes(attributes)?;
        let description = {
            let s = CommentAttributes::from_attributes(attributes).as_formatted_string();
            parse_utils::LitStrOrExpr::LitStr(LitStr::new(&s, Span::call_site()))
        };

        let is_inline = inner_attributes
            .iter()
            .any(|attribute| attribute.path().get_ident().unwrap() == "to_schema");

        let response_value = if let Some(derive_value) = derive_value {
            ResponseValue::from_derive_to_response_value(
                derive_value,
                ParsedType {
                    ty: Cow::Borrowed(ty),
                    is_inline,
                },
                description,
            )
        } else {
            ResponseValue::from_schema(
                ParsedType {
                    ty: Cow::Borrowed(ty),
                    is_inline,
                },
                description,
            )
        };

        Ok(Self(response_value.into()))
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
struct VariantAttributes<'r> {
    type_and_content: Option<(&'r Type, String)>,
    derive_value: Option<DeriveToResponseValue>,
    is_inline: bool,
}

struct EnumResponse<'r>(ResponseTuple<'r>);

impl Response for EnumResponse<'_> {}

impl<'r> EnumResponse<'r> {
    fn new(
        ident: &Ident,
        variants: &'r Punctuated<Variant, Comma>,
        attributes: &[Attribute],
    ) -> Result<Self, Diagnostics> {
        if let Some(diagnostics) =
            Self::validate_attributes(attributes, Self::has_no_field_attributes)
                .chain(Self::validate_attributes(
                    variants.iter().flat_map(|variant| &variant.attrs),
                    Self::has_no_field_attributes,
                ))
                .collect::<Option<Diagnostics>>()
        {
            return Err(diagnostics);
        }

        let ty = Self::to_type(ident);
        let description = {
            let s = CommentAttributes::from_attributes(attributes).as_formatted_string();
            parse_utils::LitStrOrExpr::LitStr(LitStr::new(&s, Span::call_site()))
        };

        let content = variants
            .into_iter()
            .map(Self::parse_variant_attributes)
            .collect::<Result<Vec<VariantAttributes>, Diagnostics>>()?
            .into_iter()
            .filter(|variant| variant.type_and_content.is_some())
            .collect::<Vec<_>>();

        let derive_value = DeriveToResponseValue::from_attributes(attributes)?;
        if let Some(derive_value) = &derive_value {
            if (!content.is_empty() && derive_value.example.is_some())
                || (!content.is_empty() && derive_value.examples.is_some())
            {
                let ident = derive_value
                    .example
                    .as_ref()
                    .map(|(_, ident)| ident)
                    .or_else(|| derive_value.examples.as_ref().map(|(_, ident)| ident))
                    .expect("Expected `example` or `examples` to be present");
                return Err(
                    Diagnostics::with_span(ident.span(),
                        "Enum with `#[content]` attribute in variant cannot have enum level `example` or `examples` defined")
                    .help(format!("Try defining `{}` on the enum variant", ident))
                );
            }
        }

        let generics = Generics::default();
        let root = &Root {
            ident,
            attributes,
            generics: &generics,
        };
        let inline_schema = EnumSchema::new(root, variants)?;

        let response_value = if content.is_empty() {
            if let Some(derive_value) = derive_value {
                ResponseValue::from_derive_to_response_value(
                    derive_value,
                    Schema::Default(DefaultSchema::None),
                    description,
                )
            } else {
                ResponseValue::from_schema(
                    Schema::Default(DefaultSchema::Raw {
                        tokens: inline_schema.to_token_stream(),
                        ty: Cow::Owned(ty),
                    }),
                    description,
                )
            }
        } else {
            let content = content
                .into_iter()
                .map(
                    |VariantAttributes {
                         type_and_content,
                         derive_value,
                         is_inline,
                     }| {
                        let (content_type, schema) = if let Some((ty, content)) = type_and_content {
                            (
                                Some(content.into()),
                                Some(Schema::Default(DefaultSchema::TypePath(ParsedType {
                                    ty: Cow::Borrowed(ty),
                                    is_inline,
                                }))),
                            )
                        } else {
                            (None, None)
                        };
                        let (example, examples) = if let Some(derive_value) = derive_value {
                            (
                                derive_value.example.map(|(example, _)| example),
                                derive_value.examples.map(|(examples, _)| examples),
                            )
                        } else {
                            (None, None)
                        };

                        MediaTypeAttr {
                            content_type,
                            schema: schema.unwrap_or_else(|| Schema::Default(DefaultSchema::None)),
                            example,
                            examples: examples.unwrap_or_default(),
                            ..MediaTypeAttr::default()
                        }
                    },
                )
                .collect::<Vec<_>>();

            let mut response = if let Some(derive_value) = derive_value {
                ResponseValue::from_derive_to_response_value(
                    derive_value,
                    Schema::Default(DefaultSchema::None),
                    description,
                )
            } else {
                ResponseValue::from_schema(
                    Schema::Default(DefaultSchema::Raw {
                        tokens: inline_schema.to_token_stream(),
                        ty: Cow::Owned(ty),
                    }),
                    description,
                )
            };
            response.content = content;

            response
        };

        Ok(Self(response_value.into()))
    }

    fn parse_variant_attributes(variant: &Variant) -> Result<VariantAttributes, Diagnostics> {
        let variant_derive_response_value =
            DeriveToResponseValue::from_attributes(variant.attrs.as_slice())?;
        // named enum variant should not have field attributes
        if let Fields::Named(named_fields) = &variant.fields {
            if let Some(diagnostics) = Self::validate_attributes(
                named_fields.named.iter().flat_map(|field| &field.attrs),
                Self::has_no_field_attributes,
            )
            .collect::<Option<Diagnostics>>()
            {
                return Err(diagnostics);
            }
        };

        let field = variant.fields.iter().next();

        let content_type = field.and_then_try(|field| {
            field
                .attrs
                .iter()
                .find(|attribute| attribute.path().get_ident().unwrap() == "content")
                .map_try(|attribute| {
                    attribute
                        .parse_args_with(|input: ParseStream| input.parse::<LitStr>())
                        .map(|content| content.value())
                        .map_err(Diagnostics::from)
                })
        })?;

        let is_inline = field
            .map(|field| {
                field
                    .attrs
                    .iter()
                    .any(|attribute| attribute.path().get_ident().unwrap() == "to_schema")
            })
            .unwrap_or(false);

        Ok(VariantAttributes {
            type_and_content: field.map(|field| &field.ty).zip(content_type),
            derive_value: variant_derive_response_value,
            is_inline,
        })
    }
}

struct ToResponseUnitStructResponse<'u>(ResponseTuple<'u>);

impl Response for ToResponseUnitStructResponse<'_> {}

impl ToResponseUnitStructResponse<'_> {
    fn new(attributes: &[Attribute]) -> Result<Self, Diagnostics> {
        if let Some(diagnostics) =
            Self::validate_attributes(attributes, Self::has_no_field_attributes)
                .collect::<Option<Diagnostics>>()
        {
            return Err(diagnostics);
        }

        let derive_value = DeriveToResponseValue::from_attributes(attributes)?;
        let description = {
            let s = CommentAttributes::from_attributes(attributes).as_formatted_string();
            parse_utils::LitStrOrExpr::LitStr(LitStr::new(&s, Span::call_site()))
        };

        let response_value = if let Some(derive_value) = derive_value {
            ResponseValue::from_derive_to_response_value(
                derive_value,
                Schema::Default(DefaultSchema::None),
                description,
            )
        } else {
            ResponseValue {
                description,
                ..Default::default()
            }
        };

        Ok(Self(response_value.into()))
    }
}

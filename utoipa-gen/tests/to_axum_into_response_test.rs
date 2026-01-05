//! Tests for `#[utoipa(to_axum)]` which generates `axum::response::IntoResponse`
//! impls behind the `axum_extras` feature.

#![cfg(feature = "axum_extras")]

use axum::{
    http::StatusCode,
    response::IntoResponse,
};

#[test]
fn to_axum_generates_into_response_for_struct() {
    #[derive(serde::Serialize, utoipa::IntoResponses)]
    #[utoipa(to_axum)]
    #[response(status = 200)]
    struct OkDto {
        foo: String,
    }

    let res = OkDto { foo: "bar".into() }.into_response();
    assert_eq!(res.status(), StatusCode::OK);
}

#[test]
fn to_axum_generates_into_response_for_enum_variants_and_flatten() {
    #[derive(Debug, Clone, serde::Serialize, utoipa::ToSchema)]
    struct BadTokenBody {
        kind: &'static str,
    }

    #[derive(Debug, Clone, serde::Serialize, utoipa::ToSchema)]
    struct InternalBody {
        kind: &'static str,
    }

    #[derive(Debug, Clone, serde::Serialize, utoipa::IntoResponses)]
    #[utoipa(to_axum)]
    enum AuthLayerError {
        #[response(status = 403)]
        BadToken(#[to_schema] BadTokenBody),

        #[response(status = 500)]
        Internal(#[to_schema] InternalBody),
    }

    #[derive(Debug, Clone, serde::Serialize, utoipa::IntoResponses)]
    #[utoipa(to_axum)]
    enum GetMeError {
        #[response(status = 500)]
        Internal(#[to_schema] InternalBody),

        #[response(flatten)]
        Auth(#[to_schema] AuthLayerError),
    }

    // Non-flatten variant
    let res = GetMeError::Internal(InternalBody { kind: "INTERNAL" }).into_response();
    assert_eq!(res.status(), StatusCode::INTERNAL_SERVER_ERROR);

    // Flatten delegates to inner IntoResponse
    let res = GetMeError::Auth(AuthLayerError::BadToken(BadTokenBody { kind: "BAD_TOKEN" }))
        .into_response();
    assert_eq!(res.status(), StatusCode::FORBIDDEN);
}

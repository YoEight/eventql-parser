//! Error types for lexical analysis and parsing.
//!
//! This module defines the error types that can occur during tokenization
//! and parsing of EventQL queries. All errors include position information
//! (line and column numbers) to help diagnose issues in query strings.

use crate::{Type, token::Symbol};
use serde::Serialize;
use thiserror::Error;

/// Top-level error type for the EventQL parser.
///
/// This enum wraps both lexer and parser errors, providing a unified
/// error type for the entire parsing pipeline.
#[derive(Debug, Error, Serialize)]
pub enum Error {
    /// Error during lexical analysis (tokenization).
    #[error(transparent)]
    Lexer(LexerError),

    /// Error during syntactic analysis (parsing).
    #[error(transparent)]
    Parser(ParserError),

    /// Error during static analysis.
    #[error(transparent)]
    Analysis(AnalysisError),
}

/// Errors that can occur during lexical analysis.
///
/// These errors are produced by the tokenizer when the input string
/// contains invalid characters or ends unexpectedly.
#[derive(Debug, Error, Serialize)]
pub enum LexerError {
    /// The input ended unexpectedly while parsing a token.
    ///
    /// This typically occurs when a string literal or other multi-character
    /// token is not properly closed.
    #[error("unexpected end of input")]
    IncompleteInput,

    /// An invalid character was encountered at the specified position.
    ///
    /// The tuple contains `(line_number, column_number)`.
    #[error("{0}:{1}: invalid character")]
    InvalidSymbol(u32, u32),
}

/// Errors that can occur during syntactic analysis.
///
/// These errors are produced by the parser when the token sequence
/// does not match the expected grammar of EventQL.
#[derive(Debug, Error, Serialize)]
pub enum ParserError {
    /// Expected an identifier but found something else.
    ///
    /// Fields: `(line, column, found_token)`
    #[error("{0}:{1}: expected identifier but got {2}")]
    ExpectedIdent(u32, u32, String),

    #[error("{0}:{1}: missing FROM statement")]
    MissingFromStatement(u32, u32),

    /// Expected a specific keyword but found something else.
    ///
    /// Fields: `(line, column, expected_keyword, found_token)`
    #[error("{0}:{1}: expected keyword {2} but got {3}")]
    ExpectedKeyword(u32, u32, &'static str, String),

    /// Expected a specific symbol but found something else.
    ///
    /// Fields: `(line, column, expected_symbol, found_token)`
    /// ```
    #[error("{0}:{1}: expected {2} but got {3}")]
    ExpectedSymbol(u32, u32, Symbol, String),

    /// An unexpected token was encountered.
    ///
    /// Fields: `(line, column, found_token)`
    ///
    /// This is a general error for tokens that don't fit the current parse context.
    #[error("{0}:{1}: unexpected token {2}")]
    UnexpectedToken(u32, u32, String),

    /// Expected a type name but found something else.
    ///
    /// Fields: `(line, column, found_token)`
    ///
    /// This occurs when defining a type conversion operation but the left side is
    /// not a type.
    #[error("{0}:{1}: expected a type")]
    ExpectedType(u32, u32),

    /// The input ended unexpectedly while parsing.
    ///
    /// This occurs when the parser expects more tokens but encounters
    /// the end of the token stream.
    #[error("unexpected end of file")]
    UnexpectedEof,
}

/// Errors that can occur during static analysis.
///
/// These errors are produced by the type checker when it encounters
/// type mismatches, undeclared variables, or other semantic issues
/// in the query.
#[derive(Debug, Error, Serialize)]
pub enum AnalysisError {
    /// A binding with the same name already exists in the current scope.
    ///
    /// Fields: `(line, column, binding_name)`
    ///
    /// This occurs when trying to declare a variable that shadows an existing
    /// binding in the same scope, such as using the same alias for multiple
    /// FROM sources.
    #[error("{0}:{1}: binding '{2}' already exists")]
    BindingAlreadyExists(u32, u32, String),

    /// A variable was referenced but not declared in any accessible scope.
    ///
    /// Fields: `(line, column, variable_name)`
    ///
    /// This occurs when referencing a variable that hasn't been bound by a
    /// FROM clause or defined in the default scope.
    #[error("{0}:{1}: variable '{2}' is undeclared")]
    VariableUndeclared(u32, u32, String),

    /// A type mismatch occurred between expected and actual types.
    ///
    /// Fields: `(line, column, expected_type, actual_type)`
    ///
    /// This occurs when an expression has a different type than what is
    /// required by its context (e.g., using a string where a number is expected).
    #[error("{0}:{1}: type mismatch: expected {2:?} but got {3:?} ")]
    TypeMismatch(u32, u32, Type, Type),

    /// A record field was accessed but doesn't exist in the record type.
    ///
    /// Fields: `(line, column, field_name)`
    ///
    /// This occurs when trying to access a field that is not defined in the
    /// record's type definition.
    #[error("{0}:{1}: record field '{2:?}' is undeclared ")]
    FieldUndeclared(u32, u32, String),

    /// A function was called but is not declared in the scope.
    ///
    /// Fields: `(line, column, function_name)`
    ///
    /// This occurs when calling a function that is not defined in the default
    /// scope or any accessible scope.
    #[error("{0}:{1}: function '{2:?}' is undeclared ")]
    FuncUndeclared(u32, u32, String),

    /// Expected a record type but found a different type.
    ///
    /// Fields: `(line, column, actual_type)`
    ///
    /// This occurs when a record type is required (e.g., for field access)
    /// but a different type was found.
    #[error("{0}:{1}: expected record but got {2:?}")]
    ExpectRecord(u32, u32, Type),

    /// Expected an array type but found a different type.
    ///
    /// Fields: `(line, column, actual_type)`
    ///
    /// This occurs when an array type is required but a different type was found.
    #[error("{0}:{1}: expected an array but got {2:?}")]
    ExpectArray(u32, u32, Type),

    /// Expected a field literal but found a different expression.
    ///
    /// Fields: `(line, column)`
    ///
    /// This occurs in contexts where only a simple field reference is allowed,
    /// such as in GROUP BY or ORDER BY clauses.
    #[error("{0}:{1}: expected a field")]
    ExpectFieldLiteral(u32, u32),

    /// Expected a record literal but found a different expression.
    ///
    /// Fields: `(line, column)`
    ///
    /// This occurs when a record literal is required, such as in the
    /// SELECT projection clause.
    #[error("{0}:{1}: expected a record")]
    ExpectRecordLiteral(u32, u32),
}

use std::{
    collections::{BTreeMap, HashMap},
    mem,
    ops::ControlFlow,
};

use crate::{
    Access, Attrs, Expr, Query, Raw, Source, SourceKind, Type, Typed, Value, error::AnalysisError,
    token::Operator,
};

pub type AnalysisResult<A> = std::result::Result<A, AnalysisError>;

#[derive(Default)]
pub struct AnalysisOptions {
    default_scope: Scope,
    event_type_info: Type,
}

pub fn static_analysis(
    options: &AnalysisOptions,
    query: Query<Raw>,
) -> AnalysisResult<Query<Typed>> {
    let mut analysis = Analysis::new(options);

    analysis.analyze_query(query)
}

#[derive(Default)]
pub struct TypeRegistry {
    pub scopes: HashMap<u64, Scope>,
}

#[derive(Default)]
pub struct Scope {
    pub entries: HashMap<String, Type>,
}

struct Analysis<'a> {
    options: &'a AnalysisOptions,
    registry: TypeRegistry,
}

impl<'a> Analysis<'a> {
    fn new(options: &'a AnalysisOptions) -> Self {
        Self {
            options,
            registry: TypeRegistry::default(),
        }
    }

    fn analyze_query(&mut self, query: Query<Raw>) -> AnalysisResult<Query<Typed>> {
        let scope = query.scope;
        let mut typed_sources = Vec::with_capacity(query.sources.len());

        for source in query.sources {
            typed_sources.push(self.analyze_source(scope, source)?);
        }

        if let Some(expr) = &query.predicate {}

        todo!()
    }

    fn analyze_source(
        &mut self,
        scope_id: u64,
        source: Source<Raw>,
    ) -> AnalysisResult<Source<Typed>> {
        let kind = self.analyze_source_kind(source.kind)?;
        let type_info = match &kind {
            SourceKind::Name(_) | SourceKind::Subject(_) => self.options.event_type_info.clone(),
            SourceKind::Subquery(query) => self.projection_type(query),
        };

        let scope = self.registry.scopes.entry(scope_id).or_default();

        if scope
            .entries
            .insert(source.binding.name.clone(), type_info)
            .is_some()
        {
            return Err(AnalysisError::BindingAlreadyExists(
                source.binding.pos.line,
                source.binding.pos.col,
                source.binding.name,
            ));
        }

        Ok(Source {
            binding: source.binding,
            kind,
        })
    }

    fn analyze_source_kind(&mut self, kind: SourceKind<Raw>) -> AnalysisResult<SourceKind<Typed>> {
        match kind {
            SourceKind::Name(n) => Ok(SourceKind::Name(n)),
            SourceKind::Subject(s) => Ok(SourceKind::Subject(s)),
            SourceKind::Subquery(query) => {
                let query = self.analyze_query(*query)?;
                Ok(SourceKind::Subquery(Box::new(query)))
            }
        }
    }

    fn analyze_expr(&mut self, expr: &Expr, expect: Type) -> AnalysisResult<Type> {
        self.analyze_value(&expr.attrs, &expr.value, expect)
    }

    fn analyze_value(
        &mut self,
        attrs: &Attrs,
        value: &Value,
        expect: Type,
    ) -> AnalysisResult<Type> {
        let result = match value {
            Value::Number(_) => expect.check(Type::Number),
            Value::String(_) => expect.check(Type::String),
            Value::Bool(_) => expect.check(Type::Bool),

            Value::Id(id) => {
                if let Some(tpe) = self.options.default_scope.entries.get(id) {
                    expect.check(tpe.clone())
                } else if let Some(tpe) = self.get_id_type_mut(attrs.scope, id.as_str()) {
                    let tmp = mem::take(tpe);
                    tmp.check(expect).map(|res| {
                        *tpe = res;
                        tpe.clone()
                    })
                } else {
                    return Err(AnalysisError::VariableUndeclared(
                        attrs.pos.line,
                        attrs.pos.col,
                        id.to_owned(),
                    ));
                }
            }

            Value::Array(exprs) => match expect {
                Type::Array(mut types) if exprs.len() == types.len() => {
                    for (expr, expect) in exprs.iter().zip(types.iter_mut()) {
                        let tmp = mem::take(expect);
                        *expect = self.analyze_expr(expr, tmp)?;
                    }

                    Ok(Type::Array(types))
                }

                expect => Err((expect, self.project_type(attrs.scope, value))),
            },

            Value::Record(fields) => match expect {
                Type::Record(mut types) if fields.len() == types.len() => {
                    for field in fields {
                        if let Some(tpe) = types.remove(field.name.as_str()) {
                            types.insert(field.name.clone(), self.analyze_expr(&field.value, tpe)?);
                        } else {
                            return Err(AnalysisError::FieldUndeclared(
                                attrs.pos.line,
                                attrs.pos.col,
                                field.name.clone(),
                            ));
                        }
                    }

                    Ok(Type::Record(types))
                }

                expect => Err((expect, self.project_type(attrs.scope, value))),
            },

            Value::Access(access) => Ok(self.analyze_access(attrs.scope, access)?),

            Value::App(app) => match expect {
                Type::App { args, mut result } if app.args.len() == args.len() => {
                    let mut arg_types = Vec::with_capacity(args.capacity());
                    for (arg, tpe) in app.args.iter().zip(args.into_iter()) {
                        arg_types.push(self.analyze_expr(arg, tpe)?);
                    }

                    if let Some(tpe) = self.options.default_scope.entries.get(app.func.as_str()) {
                        let tmp = mem::take(result.as_mut());
                        tmp.check(tpe.clone()).map(|tpe| {
                            *result = tpe;
                            Type::App {
                                args: arg_types,
                                result,
                            }
                        })
                    } else {
                        return Err(AnalysisError::FuncUndeclared(
                            attrs.pos.line,
                            attrs.pos.col,
                            app.func.clone(),
                        ));
                    }
                }

                expect => Err((expect, self.project_type(attrs.scope, value))),
            },

            Value::Binary(binary) => match binary.operator {
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                    self.analyze_expr(&binary.lhs, Type::Number)?;
                    self.analyze_expr(&binary.rhs, Type::Number)?;
                    expect.check(Type::Number)
                }

                Operator::Eq
                | Operator::Neq
                | Operator::Lt
                | Operator::Lte
                | Operator::Gt
                | Operator::Gte => {
                    let lhs_expect = self.analyze_expr(&binary.lhs, Type::Unspecified)?;
                    let rhs_expect = self.analyze_expr(&binary.rhs, lhs_expect.clone())?;

                    // If the left side didn't have enough type information while the other did,
                    // we replay another typecheck pass on the left side if the right side was conclusive
                    if matches!(lhs_expect, Type::Unspecified)
                        && !matches!(rhs_expect, Type::Unspecified)
                    {
                        self.analyze_expr(&binary.lhs, rhs_expect)?;
                    }

                    expect.check(Type::Bool)
                }

                Operator::And | Operator::Or | Operator::Xor => {
                    self.analyze_expr(&binary.lhs, Type::Bool)?;
                    self.analyze_expr(&binary.rhs, Type::Bool)?;

                    expect.check(Type::Bool)
                }

                Operator::Not => unreachable!(),
            },

            Value::Unary(unary) => match unary.operator {
                Operator::Add | Operator::Sub => {
                    self.analyze_expr(&unary.expr, Type::Number)?;
                    expect.check(Type::Number)
                }

                Operator::Not => {
                    self.analyze_expr(&unary.expr, Type::Bool)?;
                    expect.check(Type::Bool)
                }

                _ => unreachable!(),
            },

            Value::Group(expr) => Ok(self.analyze_expr(expr.as_ref(), expect)?),
        };

        result.map_err(|(expect, actual)| {
            AnalysisError::TypeMismatch(attrs.pos.line, attrs.pos.col, expect, actual)
        })
    }

    fn analyze_access(&mut self, scope_id: u64, access: &Access) -> AnalysisResult<Type> {
        todo!()
    }

    fn get_id_type_mut(&mut self, scope_id: u64, id: &str) -> Option<&mut Type> {
        let scope = self.registry.scopes.entry(scope_id).or_default();
        scope.entries.get_mut(id)
    }

    fn projection_type(&self, query: &Query<Typed>) -> Type {
        todo!()
    }

    fn project_type(&self, scope: u64, value: &Value) -> Type {
        todo!()
    }
}

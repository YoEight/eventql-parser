use std::{
    collections::{BTreeMap, HashMap, btree_map::Entry},
    mem,
};

use crate::{
    Attrs, Expr, Query, Raw, Source, SourceKind, Type, Typed, Value, error::AnalysisError,
    token::Operator,
};

pub type AnalysisResult<A> = std::result::Result<A, AnalysisError>;

pub struct AnalysisOptions {
    pub default_scope: Scope,
    pub event_type_info: Type,
}

impl Default for AnalysisOptions {
    fn default() -> Self {
        Self {
            default_scope: Scope {
                entries: HashMap::from([
                    (
                        "ABS".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "CEIL".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "FLOOR".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "ROUND".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "COS".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "EXP".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "POW".to_owned(),
                        Type::App {
                            args: vec![Type::Number, Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "SQRT".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "RAND".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "RAND".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "PI".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "LOWER".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "UPPER".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "TRIM".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "LTRIM".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "RTRIM".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "LEN".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "INSTR".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "SUBSTRING".to_owned(),
                        Type::App {
                            args: vec![Type::String, Type::Number, Type::Number],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "REPLACE".to_owned(),
                        Type::App {
                            args: vec![Type::String, Type::String, Type::String],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "STARTSWITH".to_owned(),
                        Type::App {
                            args: vec![Type::String, Type::String],
                            result: Box::new(Type::Bool),
                        },
                    ),
                    (
                        "ENDSWITH".to_owned(),
                        Type::App {
                            args: vec![Type::String, Type::String],
                            result: Box::new(Type::Bool),
                        },
                    ),
                    (
                        "NOW".to_owned(),
                        Type::App {
                            args: vec![],
                            result: Box::new(Type::String),
                        },
                    ),
                    (
                        "YEAR".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "MONTH".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "DAY".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "HOUR".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "MINUTE".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "SECOND".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "WEEKDAY".to_owned(),
                        Type::App {
                            args: vec![Type::String],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "IF".to_owned(),
                        Type::App {
                            args: vec![Type::Bool, Type::Unspecified, Type::Unspecified],
                            result: Box::new(Type::Unspecified),
                        },
                    ),
                    (
                        "COUNT".to_owned(),
                        Type::App {
                            args: vec![],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "SUM".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "AVG".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "MIN".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "MAX".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "MEDIAN".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "STDDEV".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "VARIANCE".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                    (
                        "UNIQUE".to_owned(),
                        Type::App {
                            args: vec![Type::Number],
                            result: Box::new(Type::Number),
                        },
                    ),
                ]),
            },
            event_type_info: Type::Record(BTreeMap::from([
                ("specversion".to_owned(), Type::String),
                ("id".to_owned(), Type::String),
                ("time".to_owned(), Type::String),
                ("source".to_owned(), Type::String),
                ("subject".to_owned(), Type::Subject),
                ("type".to_owned(), Type::String),
                ("datacontenttype".to_owned(), Type::String),
                ("data".to_owned(), Type::Unspecified),
                ("predecessorhash".to_owned(), Type::String),
                ("hash".to_owned(), Type::String),
                ("traceparent".to_owned(), Type::String),
                ("tracestate".to_owned(), Type::String),
                ("signature".to_owned(), Type::String),
            ])),
        }
    }
}

pub fn static_analysis(
    options: &AnalysisOptions,
    query: Query<Raw>,
) -> AnalysisResult<Query<Typed>> {
    let mut analysis = Analysis::new(options);

    analysis.analyze_query(query)
}

#[derive(Default)]
pub struct Scope {
    pub entries: HashMap<String, Type>,
}

impl Scope {
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

struct Analysis<'a> {
    options: &'a AnalysisOptions,
    prev_scopes: Vec<Scope>,
    scope: Scope,
}

impl<'a> Analysis<'a> {
    fn new(options: &'a AnalysisOptions) -> Self {
        Self {
            options,
            prev_scopes: Default::default(),
            scope: Scope::default(),
        }
    }

    fn enter_scope(&mut self) {
        if self.scope.is_empty() {
            return;
        }

        let prev = mem::take(&mut self.scope);
        self.prev_scopes.push(prev);
    }

    fn exit_scope(&mut self) -> Scope {
        if let Some(prev) = self.prev_scopes.pop() {
            mem::replace(&mut self.scope, prev)
        } else {
            mem::take(&mut self.scope)
        }
    }

    fn analyze_query(&mut self, query: Query<Raw>) -> AnalysisResult<Query<Typed>> {
        self.enter_scope();

        let mut sources = Vec::with_capacity(query.sources.len());

        for source in query.sources {
            sources.push(self.analyze_source(source)?);
        }

        if let Some(expr) = &query.predicate {
            self.analyze_expr(expr, Type::Bool)?;
        }

        if let Some(group_by) = &query.group_by {
            if !matches!(&group_by.expr.value, Value::Access(_)) {
                return Err(AnalysisError::ExpectFieldLiteral(
                    group_by.expr.attrs.pos.line,
                    group_by.expr.attrs.pos.col,
                ));
            }

            self.analyze_expr(&group_by.expr, Type::Unspecified)?;

            if let Some(expr) = &group_by.predicate {
                self.analyze_expr(expr, Type::Bool)?;
            }
        }

        if let Some(order_by) = &query.order_by {
            if !matches!(&order_by.expr.value, Value::Access(_)) {
                return Err(AnalysisError::ExpectFieldLiteral(
                    order_by.expr.attrs.pos.line,
                    order_by.expr.attrs.pos.col,
                ));
            }
            self.analyze_expr(&order_by.expr, Type::Unspecified)?;
        }

        if !matches!(&query.projection.value, Value::Record(_) | Value::Id(_)) {
            return Err(AnalysisError::ExpectRecordLiteral(
                query.projection.attrs.pos.line,
                query.projection.attrs.pos.col,
            ));
        }

        let tpe = self.analyze_expr(&query.projection, Type::Unspecified)?;

        if !matches!(&tpe, Type::Record(f) if !f.is_empty()) {
            return Err(AnalysisError::ExpectRecord(
                query.projection.attrs.pos.line,
                query.projection.attrs.pos.col,
                tpe,
            ));
        }

        self.exit_scope();

        Ok(Query {
            attrs: query.attrs,
            sources,
            predicate: query.predicate,
            group_by: query.group_by,
            order_by: query.order_by,
            limit: query.limit,
            projection: query.projection,
            distinct: query.distinct,
            _marker: std::marker::PhantomData,
        })
    }

    fn analyze_source(&mut self, source: Source<Raw>) -> AnalysisResult<Source<Typed>> {
        let kind = self.analyze_source_kind(source.kind)?;
        let tpe = match &kind {
            SourceKind::Name(_) | SourceKind::Subject(_) => self.options.event_type_info.clone(),
            SourceKind::Subquery(query) => self.projection_type(query),
        };

        if self
            .scope
            .entries
            .insert(source.binding.name.clone(), tpe)
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
        match value {
            Value::Number(_) => expect.check(attrs, Type::Number),
            Value::String(_) => expect.check(attrs, Type::String),
            Value::Bool(_) => expect.check(attrs, Type::Bool),

            Value::Id(id) => {
                if let Some(tpe) = self.options.default_scope.entries.get(id) {
                    expect.check(attrs, tpe.clone())
                } else if let Some(tpe) = self.scope.entries.get_mut(id.as_str()) {
                    let tmp = mem::take(tpe);
                    *tpe = tmp.check(attrs, expect)?;

                    Ok(tpe.clone())
                } else {
                    Err(AnalysisError::VariableUndeclared(
                        attrs.pos.line,
                        attrs.pos.col,
                        id.to_owned(),
                    ))
                }
            }

            this @ Value::Array(exprs) => {
                if matches!(expect, Type::Unspecified) {
                    return Ok(self.project_type(this));
                }

                match expect {
                    Type::Array(mut types) if exprs.len() == types.len() => {
                        for (expr, expect) in exprs.iter().zip(types.iter_mut()) {
                            let tmp = mem::take(expect);
                            *expect = self.analyze_expr(expr, tmp)?;
                        }

                        Ok(Type::Array(types))
                    }

                    expect => Err(AnalysisError::TypeMismatch(
                        attrs.pos.line,
                        attrs.pos.col,
                        expect,
                        self.project_type(value),
                    )),
                }
            }

            this @ Value::Record(fields) => {
                if matches!(expect, Type::Unspecified) {
                    return Ok(self.project_type(this));
                }

                match expect {
                    Type::Record(mut types) if fields.len() == types.len() => {
                        for field in fields {
                            if let Some(tpe) = types.remove(field.name.as_str()) {
                                types.insert(
                                    field.name.clone(),
                                    self.analyze_expr(&field.value, tpe)?,
                                );
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

                    expect => Err(AnalysisError::TypeMismatch(
                        attrs.pos.line,
                        attrs.pos.col,
                        expect,
                        self.project_type(value),
                    )),
                }
            }

            this @ Value::Access(_) => Ok(self.analyze_access(attrs, this, expect)?),

            this @ Value::App(app) => {
                if matches!(expect, Type::Unspecified) {
                    return Ok(self.project_type(this));
                }

                match expect {
                    Type::App { args, mut result } if app.args.len() == args.len() => {
                        let mut arg_types = Vec::with_capacity(args.capacity());
                        for (arg, tpe) in app.args.iter().zip(args.into_iter()) {
                            arg_types.push(self.analyze_expr(arg, tpe)?);
                        }

                        if let Some(tpe) = self.options.default_scope.entries.get(app.func.as_str())
                        {
                            let tmp = mem::take(result.as_mut());
                            *result = tmp.check(attrs, tpe.clone())?;

                            Ok(Type::App {
                                args: arg_types,
                                result,
                            })
                        } else {
                            Err(AnalysisError::FuncUndeclared(
                                attrs.pos.line,
                                attrs.pos.col,
                                app.func.clone(),
                            ))
                        }
                    }

                    expect => Err(AnalysisError::TypeMismatch(
                        attrs.pos.line,
                        attrs.pos.col,
                        expect,
                        self.project_type(value),
                    )),
                }
            }

            Value::Binary(binary) => match binary.operator {
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                    self.analyze_expr(&binary.lhs, Type::Number)?;
                    self.analyze_expr(&binary.rhs, Type::Number)?;
                    expect.check(attrs, Type::Number)
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

                    expect.check(attrs, Type::Bool)
                }

                Operator::And | Operator::Or | Operator::Xor => {
                    self.analyze_expr(&binary.lhs, Type::Bool)?;
                    self.analyze_expr(&binary.rhs, Type::Bool)?;

                    expect.check(attrs, Type::Bool)
                }

                Operator::Not => unreachable!(),
            },

            Value::Unary(unary) => match unary.operator {
                Operator::Add | Operator::Sub => {
                    self.analyze_expr(&unary.expr, Type::Number)?;
                    expect.check(attrs, Type::Number)
                }

                Operator::Not => {
                    self.analyze_expr(&unary.expr, Type::Bool)?;
                    expect.check(attrs, Type::Bool)
                }

                _ => unreachable!(),
            },

            Value::Group(expr) => Ok(self.analyze_expr(expr.as_ref(), expect)?),
        }
    }

    fn analyze_access(
        &mut self,
        attrs: &Attrs,
        access: &Value,
        expect: Type,
    ) -> AnalysisResult<Type> {
        struct State<A, B> {
            depth: u8,
            /// When true means we are into dynamically type object.
            dynamic: bool,
            definition: Def<A, B>,
        }

        impl<A, B> State<A, B> {
            fn new(definition: Def<A, B>) -> Self {
                Self {
                    depth: 0,
                    dynamic: false,
                    definition,
                }
            }
        }

        enum Def<A, B> {
            User(A),
            System(B),
        }

        fn go<'a>(
            scope: &'a mut Scope,
            sys: &'a AnalysisOptions,
            attrs: &'a Attrs,
            value: &'a Value,
        ) -> AnalysisResult<State<&'a mut Type, &'a Type>> {
            match value {
                Value::Id(id) => {
                    if let Some(tpe) = sys.default_scope.entries.get(id.as_str()) {
                        Ok(State::new(Def::System(tpe)))
                    } else if let Some(tpe) = scope.entries.get_mut(id.as_str()) {
                        Ok(State::new(Def::User(tpe)))
                    } else {
                        Err(AnalysisError::VariableUndeclared(
                            attrs.pos.line,
                            attrs.pos.col,
                            id.clone(),
                        ))
                    }
                }
                Value::Access(access) => {
                    let mut state = go(scope, sys, &access.target.attrs, &access.target.value)?;

                    // TODO - we should consider make that field and depth configurable.
                    let is_data_field = state.depth == 0 && access.field == "data";

                    // TODO - we should consider make that behavior configurable.
                    // the `data` property is where the JSON payload is located, which means
                    // we should be lax if a property is not defined yet.
                    if !state.dynamic && is_data_field {
                        state.dynamic = true;
                    }

                    match state.definition {
                        Def::User(tpe) => {
                            if let Type::Record(fields) = tpe {
                                match fields.entry(access.field.clone()) {
                                    Entry::Vacant(entry) => {
                                        if state.dynamic || is_data_field {
                                            return Ok(State {
                                                depth: state.depth + 1,
                                                definition: Def::User(
                                                    entry.insert(Type::Unspecified),
                                                ),
                                                ..state
                                            });
                                        }

                                        return Err(AnalysisError::FieldUndeclared(
                                            attrs.pos.line,
                                            attrs.pos.col,
                                            access.field.clone(),
                                        ));
                                    }

                                    Entry::Occupied(entry) => {
                                        return Ok(State {
                                            depth: state.depth + 1,
                                            definition: Def::User(entry.into_mut()),
                                            ..state
                                        });
                                    }
                                }
                            }

                            Err(AnalysisError::ExpectRecord(
                                attrs.pos.line,
                                attrs.pos.col,
                                tpe.clone(),
                            ))
                        }

                        Def::System(tpe) => {
                            if let Type::Record(fields) = tpe {
                                if let Some(field) = fields.get(access.field.as_str()) {
                                    return Ok(State {
                                        depth: state.depth + 1,
                                        definition: Def::System(field),
                                        ..state
                                    });
                                }

                                return Err(AnalysisError::FieldUndeclared(
                                    attrs.pos.line,
                                    attrs.pos.col,
                                    access.field.clone(),
                                ));
                            }

                            Err(AnalysisError::ExpectRecord(
                                attrs.pos.line,
                                attrs.pos.col,
                                tpe.clone(),
                            ))
                        }
                    }
                }
                Value::Number(_)
                | Value::String(_)
                | Value::Bool(_)
                | Value::Array(_)
                | Value::Record(_)
                | Value::App(_)
                | Value::Binary(_)
                | Value::Unary(_)
                | Value::Group(_) => unreachable!(),
            }
        }

        let state = go(&mut self.scope, self.options, attrs, access)?;

        match state.definition {
            Def::User(tpe) => {
                let tmp = mem::take(tpe);
                *tpe = tmp.check(attrs, expect)?;

                Ok(tpe.clone())
            }

            Def::System(tpe) => tpe.clone().check(attrs, expect),
        }
    }

    fn projection_type(&self, query: &Query<Typed>) -> Type {
        self.project_type(&query.projection.value)
    }

    fn project_type(&self, value: &Value) -> Type {
        match value {
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
            Value::Id(id) => {
                if let Some(tpe) = self.options.default_scope.entries.get(id) {
                    tpe.clone()
                } else if let Some(tpe) = self.scope.entries.get(id) {
                    tpe.clone()
                } else {
                    Type::Unspecified
                }
            }
            Value::Array(exprs) => {
                Type::Array(exprs.iter().map(|v| self.project_type(&v.value)).collect())
            }
            Value::Record(fields) => Type::Record(
                fields
                    .iter()
                    .map(|field| (field.name.clone(), self.project_type(&field.value.value)))
                    .collect(),
            ),
            Value::Access(access) => {
                let tpe = self.project_type(&access.target.value);
                if let Type::Record(fields) = tpe {
                    fields
                        .get(access.field.as_str())
                        .cloned()
                        .unwrap_or_default()
                } else {
                    Type::Unspecified
                }
            }
            Value::App(app) => self
                .options
                .default_scope
                .entries
                .get(app.func.as_str())
                .cloned()
                .unwrap_or_default(),
            Value::Binary(binary) => match binary.operator {
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => Type::Number,
                Operator::Eq
                | Operator::Neq
                | Operator::Lt
                | Operator::Lte
                | Operator::Gt
                | Operator::Gte
                | Operator::And
                | Operator::Or
                | Operator::Xor
                | Operator::Not => Type::Bool,
            },
            Value::Unary(unary) => match unary.operator {
                Operator::Add | Operator::Sub => Type::Number,
                Operator::Mul
                | Operator::Div
                | Operator::Eq
                | Operator::Neq
                | Operator::Lt
                | Operator::Lte
                | Operator::Gt
                | Operator::Gte
                | Operator::And
                | Operator::Or
                | Operator::Xor
                | Operator::Not => unreachable!(),
            },
            Value::Group(expr) => self.project_type(&expr.value),
        }
    }
}

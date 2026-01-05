use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
    ops::{Add, AddAssign},
};

/// A locational span of text.
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    start_row: usize,
    start_col: usize,
    end_row: usize,
    end_col: usize,
}
impl Span {
    pub const fn null() -> Self {
        Self {
            start_row: 0,
            start_col: 0,
            end_row: 0,
            end_col: 0,
        }
    }
    pub const fn start() -> Self {
        Self {
            start_row: 1,
            start_col: 1,
            end_row: 1,
            end_col: 1,
        }
    }
    pub const fn is_null(&self) -> bool {
        let null = Span::null();
        self.start_row == null.start_row && //
            self.start_col == null.start_col && //-
            self.end_row == null.end_row && //-
            self.end_col == null.end_col
    }
    pub const fn is_left_adjacent_to(&self, other: Span) -> bool {
        if self.is_null() || other.is_null() {
            return false;
        }
        self.end_row == other.start_row && self.end_col + 1 == other.start_col
    }
    pub fn shift_forwards(&mut self) {
        self.start_col += 1;
        self.end_col += 1;
    }
    pub fn next_column(&self) -> Span {
        let mut span = *self;
        span.shift_forwards();
        span
    }
    pub fn shift_down(&mut self) {
        self.start_col = 1;
        self.end_col = 1;
        self.start_row += 1;
        self.end_row += 1;
    }
    pub fn new(
        mut start_row: usize,
        mut start_col: usize,
        mut end_row: usize,
        mut end_col: usize,
    ) -> Self {
        if (start_row, start_col) > (end_row, end_col) {
            core::mem::swap(&mut start_row, &mut end_row);
            core::mem::swap(&mut start_col, &mut end_col);
        }
        Self {
            start_row,
            start_col,
            end_row,
            end_col,
        }
    }
    pub fn single(row: usize, col: usize) -> Self {
        Self {
            start_row: row,
            start_col: col,
            end_row: row,
            end_col: col,
        }
    }

    pub fn start_row(&self) -> usize {
        self.start_row
    }
    pub fn start_col(&self) -> usize {
        self.start_col
    }
    pub fn end_row(&self) -> usize {
        self.end_row
    }
    pub fn end_col(&self) -> usize {
        self.end_col
    }
}
impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}:{} => {}:{}]",
            self.start_row, self.start_col, self.end_row, self.end_col
        )
    }
}
impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
impl Add for Span {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        if self.is_null() {
            return rhs;
        }
        if rhs.is_null() {
            return self;
        }
        let (start_row, start_col) =
            (self.start_row, self.start_col).min((rhs.start_row, rhs.start_col));
        let (end_row, end_col) = (self.end_row, self.end_col).max((rhs.end_row, rhs.end_col));
        Self {
            start_row,
            start_col,
            end_row,
            end_col,
        }
    }
}
impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

/// Contains shared context for parsing.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseContext<T> {
    pub eof_row: usize,
    pub eof_col: usize,
    pub data: T,
}
impl<T> ParseContext<T> {
    /// Constructs a single-character span just past the given end of text.
    pub fn eof_span(&self) -> Span {
        Span::single(self.eof_row, self.eof_col)
    }
    /// Keeps implementation information, but injects new user data.
    pub fn mutate<U>(&self, new_data: U) -> ParseContext<U> {
        ParseContext {
            eof_row: self.eof_row,
            eof_col: self.eof_col,
            data: new_data,
        }
    }
}

/// Whether or not parsing is recoverable. Unrecoverable errors are propagated immediately,
/// while recoverable errors can be suppressed in cases of syntactical sequences or variants.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Recoverability {
    Unrecoverable,
    #[default]
    Recoverable,
}
impl Display for Recoverability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Recoverability::Unrecoverable => write!(f, "unrecoverable"),
            Recoverability::Recoverable => write!(f, "recoverable"),
        }
    }
}
/// Must be implemented for [`Syntax`] error types.
pub trait SyntaxError {
    fn recoverability(&self) -> Recoverability;
    fn set_recoverability(&mut self, new_recoverability: Recoverability);
}
impl SyntaxError for Recoverability {
    fn recoverability(&self) -> Recoverability {
        *self
    }
    fn set_recoverability(&mut self, new_recoverability: Recoverability) {
        *self = new_recoverability;
    }
}

/// Like [`cursor!`], but without spans.
#[macro_export]
macro_rules! nospan_cursor {
    ($type:ty) => { (impl Iterator<Item = $type> + Clone) };
}
/// An iterator type used in parsers. Really just `(impl Iterator<Item = (Span, T)> + Clone)`.
#[macro_export]
macro_rules! cursor {
    ($type:ty) => {
        $crate::nospan_cursor!(($crate::Span, $type))
    };
}

/// The type all syntactical elements implement. Allows parsing from and serialising to sequences of tokens.
pub trait Syntax<Tok, E: SyntaxError, D>: Sized {
    type Item;
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E>;
    fn to_tokens(&self) -> Vec<(Span, Tok)>;
    fn span(&self) -> Span;
    fn to_item(self) -> (Span, Self::Item);
    fn parse(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<(Span, Self::Item), E> {
        Ok(Self::from_tokens(tokens, context)?.to_item())
    }
}

/// Internal macro used for summing a series of expressions.
#[macro_export]
macro_rules! __sum {
    ($expr:expr) => { $expr };
    ($expr:expr, $($exprs:expr),+) => { ($expr) + $crate::__sum!($($exprs),+) };
}

#[doc(hidden)]
pub use paste::paste as __paste;
#[macro_export]
macro_rules! syntax {
    ([$($type_attrs:tt)*] $type_vis:vis struct $type_name:ident: $token_type:ty, $data_type:ty => !$error_type:ty { $($field_vis:vis $field_name:ident: $field_type:ty),+ $(,)? }) => {
        $($attrs)* $type_vis struct $type_name {
            __span: $crate::Span,
            $($field_vis $field_name: $field_type),+
        }
        impl $crate::Syntax<$token_type, $error_type, $data_type> for $type_name {
            fn from_tokens(__tokens: &mut $crate::cursor!($token_type), __context: $crate::ParseContext<$data_type>) -> ::std::result::Result<Self, $error_type> {
                let mut __try_tokens = __tokens.clone();
                $(let $field_name = <$field_type as $crate::Syntax<$token_type, $error_type, $data_type>>::from_tokens(&mut __try_tokens, __context.clone())?;)+
                *__tokens = __try_tokens;
                let __span = $crate::__sum!($(<$field_type as $crate::Syntax<$token_type, $error_type, $data_type>>::span(&$field_name)),+);
                Ok(Self { __span, $($field_name),* } )
            }
            fn to_tokens(&self) -> Vec<($crate::Span, $token_type)> {
                let mut __tokens = vec![];
                $(__tokens.append(&mut <$field_type as $crate::Syntax<$token_type, $error_type, $data_type>>::to_tokens(&self.$field_name));)+
                __tokens
            }
            fn span(&self) -> $crate::Span { self.__span }
        }
    };
    ([$($attrs:tt)*] $type_vis:vis enum $type_name:ident: $token_type:ty, $data_type:ty => !$error_type:ty[$error_ctor:expr] { $($name:literal => $variant_name:ident($inner:ty)),+ $(,)? }) => {
        $crate::__paste!($crate::syntax!([$($attrs)*] $type_vis enum $type_name dissolves to [$($attrs)*] $type_vis enum [<$type_name Item>]: $token_type, $data_type => !$error_type[$error_ctor] { $($name => $variant_name($inner)),+ }););
    };
    ([$($attrs:tt)*] $type_vis:vis enum $type_name:ident dissolves to [$($item_attrs:tt)*] $item_vis:vis enum $item_name:ident: $token_type:ty, $data_type:ty => !$error_type:ty[$error_ctor:expr] { $($name:literal => $variant_name:ident($inner:ty)),+ $(,)? }) => {
        $($attrs)* $type_vis enum $type_name {
            $($variant_name($inner)),+
        }
        $($attrs)* $type_vis enum $item_name {
            $($variant_name(<$inner as $crate::Syntax<$token_type, $error_type, $data_type>>::Item)),+
        }
        impl $crate::Syntax<$token_type, $error_type, $data_type> for $type_name {
            type Item = $item_name;
            fn from_tokens(__tokens: &mut $crate::cursor!($token_type), __context: $crate::ParseContext<$data_type>) -> ::std::result::Result<Self, $error_type> {
                let __first_span = __tokens.clone().next().map(|(__span, _)| __span).unwrap_or(__context.eof_span());
                let mut __errors = vec![];
                $(
                    let mut __try_tokens = __tokens.clone();
                    match <$inner>::from_tokens(&mut __try_tokens, __context.clone()) {
                        Ok(__expr) => {
                            *__tokens = __try_tokens;
                            return Ok(Self::$variant_name(__expr));
                        },
                        Err(__err) if $crate::SyntaxError::recoverability(&__err) == $crate::Recoverability::Unrecoverable => {
                            return Err(__err);
                        }
                        Err(__err) => __errors.push(__err),
                    };
                )*
                Err(($error_ctor)([$($name),*].into_iter().zip(__errors), __first_span))
            }
            fn to_tokens(&self) -> Vec<(Span, $token_type)> {
                match self {
                    $(Self::$variant_name(__inner) => <$inner as $crate::Syntax<$token_type, $error_type, $data_type>>::to_tokens(__inner)),+
                }
            }
            fn span(&self) -> Span {
                match self {
                    $(Self::$variant_name(__inner) => <$inner as $crate::Syntax<$token_type, $error_type, $data_type>>::span(__inner)),+
                }
            }
            fn to_item(self) -> ($crate::Span, Self::Item) {
                match self {
                    $(Self::$variant_name(__inner) => {
                        let item = <$inner as $crate::Syntax<$token_type, $error_type, $data_type>>::to_item(__inner);
                        (item.0, <$item_name>::$variant_name(item.1))
                    }),+
                }
            }
        }
    }
}

/// Implements [`Syntax`] for tuples over the types passed in, then instantiates [`tuple_impl`] for all but the first types.
///
/// For example, `tuple_impl!(A, B, C);` implements [`Syntax`] for `(A, B, C)`, `(A, B)`, and `(A,)`.
///
/// Currently, 32-element tuples are supported. `()` is not, as it has no span, though this may change. [`Null`] should be used in place of `()`.
macro_rules! tuple_impl {
    ($T:ident) => {
        impl<__Tok, __E: $crate::SyntaxError, __D: Clone, $T: $crate::Syntax<__Tok, __E, __D>> $crate::Syntax<__Tok, __E, __D> for ($T,) {
            type Item = $T;
            fn from_tokens(__tokens: &mut $crate::cursor!(__Tok), __context: $crate::ParseContext<__D>) -> ::std::result::Result<Self, __E> {
                let mut __try_tokens = __tokens.clone();
                let __result = (<$T as $crate::Syntax<__Tok, __E, __D>>::from_tokens(&mut __try_tokens, __context.clone())?,);
                *__tokens = __try_tokens;
                Ok(__result)
            }
            fn to_tokens(&self) -> Vec<($crate::Span, __Tok)> {
                <$T as $crate::Syntax<__Tok, __E, __D>>::to_tokens(&self.0)
            }
            fn span(&self) -> $crate::Span {
                <$T as $crate::Syntax<__Tok, __E, __D>>::span(&self.0)
            }
            fn parse(__tokens: &mut $crate::cursor!(__Tok), __context: $crate::ParseContext<__D>) -> ::std::result::Result<(Span, Self::Item), __E> {
                let item = Self::from_tokens(__tokens, __context)?;
                Ok((item.span(), item.0))
            }
            fn to_item(self) -> (Span, Self::Item) {
                (self.0.span(), self.0)
            }
        }
    };
    ($T:ident, $($Ts:ident),+) => {
        impl<__Tok, __E: $crate::SyntaxError, __D: Clone, $T: $crate::Syntax<__Tok, __E, __D>, $($Ts: $crate::Syntax<__Tok, __E, __D>),+>
            $crate::Syntax<__Tok, __E, __D> for ($T, $($Ts),+) {
            type Item = ((Span, $T::Item), $((Span, $Ts::Item)),+);
            fn from_tokens(__tokens: &mut $crate::cursor!(__Tok), __context: $crate::ParseContext<__D>) -> ::std::result::Result<Self, __E> {
                let mut __try_tokens = __tokens.clone();
                let __result = (
                    <$T as $crate::Syntax<__Tok, __E, __D>>::from_tokens(&mut __try_tokens, __context.clone())?,
                    $(<$Ts as $crate::Syntax<__Tok, __E, __D>>::from_tokens(&mut __try_tokens, __context.clone())?),+
                );
                *__tokens = __try_tokens;
                Ok(__result)
            }
            fn to_tokens(&self) -> Vec<($crate::Span, __Tok)> {
                let mut __tokens = vec![];
                #[allow(non_snake_case)]
                let ($T, $($Ts),+) = self;
                __tokens.append(&mut <$T as $crate::Syntax<__Tok, __E, __D>>::to_tokens($T));
                $(__tokens.append(&mut <$Ts as $crate::Syntax<__Tok, __E, __D>>::to_tokens($Ts));)+
                __tokens
            }
            fn span(&self) -> $crate::Span {
                #[allow(non_snake_case)]
                let ($T, $($Ts),+) = self;
                $crate::__sum!(
                    <$T as $crate::Syntax<__Tok, __E, __D>>::span($T),
                    $(<$Ts as $crate::Syntax<__Tok, __E, __D>>::span($Ts)),+
                )
            }
            fn to_item(self) -> (Span, Self::Item) {
                let __span = <Self as $crate::Syntax<__Tok, __E, __D>>::span(&self);
                #[allow(non_snake_case)]
                let ($T, $($Ts),+) = self;
                (__span, (
                    <$T as $crate::Syntax<__Tok, __E, __D>>::to_item($T),
                    $(<$Ts as $crate::Syntax<__Tok, __E, __D>>::to_item($Ts)),+
                ))
            }
        }
        tuple_impl!($($Ts),+);
    };
}
tuple_impl!(
    T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20,
    T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31
);

/// Extension trait for parse results.
pub trait ParseResultExt<T, E: SyntaxError> {
    /// Turns a `Result<T>` into a `Result<Result<T>>` based on recoverability:
    /// - `Ok(Ok(_))` signifies success
    /// - `Ok(Err(_))` signifies a recoverable error
    /// - `Err(_)` signifies an unrecoverable error
    /// Used for terser propagation; `(...).nest()?` propagates all unrecoverable errors but leaves recoverable errors checkable.
    fn nest(self) -> Result<Result<T, E>, E>;
}
impl<T, E: SyntaxError> ParseResultExt<T, E> for Result<T, E> {
    fn nest(self) -> Result<Result<T, E>, E> {
        match self {
            Ok(value) => Ok(Ok(value)),
            Err(recoverable) if recoverable.recoverability() == Recoverability::Recoverable => {
                Ok(Err(recoverable))
            }
            Err(unrecoverable) => Err(unrecoverable),
        }
    }
}

/// Always succeds while parsing, but does not actually parse anything.
pub struct Null(pub Span);
impl<Tok, E: SyntaxError, D> Syntax<Tok, E, D> for Null {
    type Item = ();
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
        Ok(Self(match tokens.clone().next() {
            Some((span, _)) => span,
            None => context.eof_span(),
        }))
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        vec![]
    }
    fn span(&self) -> Span {
        self.0
    }
    fn to_item(self) -> (Span, Self::Item) {
        (self.0, ())
    }
}
/// Makes unrecoverable errors recoverable.
#[derive(Clone, Copy, Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Try<T>(pub T);
impl<Tok, E: SyntaxError, D, T: Syntax<Tok, E, D>> Syntax<Tok, E, D> for Try<T> {
    type Item = T::Item;
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
        match T::from_tokens(tokens, context) {
            Ok(item) => Ok(Self(item)),
            Err(mut err) => {
                err.set_recoverability(Recoverability::Recoverable);
                Err(err)
            }
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        self.0.to_tokens()
    }
    fn span(&self) -> Span {
        self.0.span()
    }
    fn to_item(self) -> (Span, Self::Item) {
        self.0.to_item()
    }
}
/// Makes recoverable errors unrecoverable.
#[derive(Clone, Copy, Default, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Force<T>(pub T);
impl<Tok, E: SyntaxError, D, T: Syntax<Tok, E, D>> Syntax<Tok, E, D> for Force<T> {
    type Item = T::Item;
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
        match T::from_tokens(tokens, context) {
            Ok(item) => Ok(Self(item)),
            Err(mut err) => {
                err.set_recoverability(Recoverability::Unrecoverable);
                Err(err)
            }
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        self.0.to_tokens()
    }
    fn span(&self) -> Span {
        self.0.span()
    }
    fn to_item(self) -> (Span, Self::Item) {
        self.0.to_item()
    }
}

/// Requirements for whether or not punctuation can trail.
trait TrailingRule {
    fn matches(is_trailing: bool) -> bool;
}
/// Trailing and non-trailing punctuation are both allowed.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct AllowTrailing;
impl TrailingRule for AllowTrailing {
    fn matches(_: bool) -> bool {
        true
    }
}
/// Punctuation must trail. The final punctuation in a `Punctuated<_, _, RequireTrailing, _>` will be `Some`.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RequireTrailing;
impl TrailingRule for RequireTrailing {
    fn matches(is_trailing: bool) -> bool {
        is_trailing
    }
}
/// Punctuation cannot trail. The final punctuation in a `Punctuated<_, _, RequireTrailing, _>` will be `None`.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DenyTrailing;
impl TrailingRule for DenyTrailing {
    fn matches(is_trailing: bool) -> bool {
        !is_trailing
    }
}

/// Requirements for the number of elements in a sequence.
trait ElementRequirementRule {
    fn matches(element_count: usize) -> bool;
}
/// At least one element must appear in a sequence.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RequireOne;
impl ElementRequirementRule for RequireOne {
    fn matches(element_count: usize) -> bool {
        element_count > 0
    }
}
/// Any number of items can appear in a sequence.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnyCount;
impl ElementRequirementRule for AnyCount {
    fn matches(_: usize) -> bool {
        true
    }
}

/// Parses a sequence of elements separated by some other element.
///
/// Allows trailing punctuation by default, as well as zero elements, but this can be overridden.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Punctuated<T, By, Trail = AllowTrailing, Require = AnyCount> {
    pub span: Span,
    pub items: Vec<(T, Option<By>)>,
    _pd: PhantomData<(Trail, Require)>,
}
impl<
    Tok,
    E: SyntaxError + Debug,
    D: Clone,
    T: Syntax<Tok, E, D>,
    P: Syntax<Tok, E, D>,
    Trail: TrailingRule,
    Require: ElementRequirementRule,
> Syntax<Tok, E, D> for Punctuated<T, P, Trail, Require>
{
    type Item = Vec<(Span, T::Item)>;
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
        let first_span = tokens
            .clone()
            .next()
            .map(|(span, _)| span)
            .unwrap_or(context.eof_span());
        let mut items = vec![];
        'parse_loop: loop {
            let mut try_tokens = tokens.clone();
            let item = T::from_tokens(&mut try_tokens, context.clone()).nest()?;
            let item = match item {
                Ok(item) => item,
                Err(err) => {
                    if (items.is_empty() || Trail::matches(true)) && Require::matches(items.len()) {
                        // we have a correct number of items,
                        // and either trailing punctuation is allowed,
                        // or we don't have anything to begin with
                        break 'parse_loop;
                    } else {
                        return Err(err);
                    }
                }
            };
            *tokens = try_tokens;
            try_tokens = tokens.clone();
            match P::from_tokens(&mut try_tokens, context.clone()).nest()? {
                Ok(punct) => {
                    *tokens = try_tokens;
                    items.push((item, Some(punct)));
                }
                Err(err) => {
                    if Trail::matches(false) && Require::matches(items.len() + 1) {
                        // we have a correct number of items and no trailing punctuation
                        items.push((item, None));
                        break 'parse_loop;
                    } else {
                        return Err(err);
                    }
                }
            }
        }
        let span = items
            .iter()
            .map(|(item, punct)| {
                let mut span = item.span();
                if let Some(punct) = punct {
                    span += punct.span();
                }
                span
            })
            .reduce(|a, b| a + b)
            .unwrap_or(first_span);
        Ok(Self {
            span,
            items,
            _pd: PhantomData,
        })
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        let mut out = vec![];
        for (item, punct) in &self.items {
            out.append(&mut item.to_tokens());
            if let Some(punct) = punct {
                out.append(&mut punct.to_tokens());
            }
        }
        out
    }
    fn span(&self) -> Span {
        self.span
    }
    fn to_item(self) -> (Span, Self::Item) {
        (
            self.span,
            self.items
                .into_iter()
                .map(|(item, _)| item.to_item())
                .collect(),
        )
    }
}

/// Parses a sequence of elements, but will not go past the next given terminating item.
pub struct Terminate<T, At> {
    pub items: Vec<T>,
    pub terminator: At,
}
impl<Tok, E: SyntaxError, D: Clone, T: Syntax<Tok, E, D>, At: Syntax<Tok, E, D>> Syntax<Tok, E, D>
    for Terminate<T, At>
{
    type Item = Vec<(Span, T::Item)>;
    fn from_tokens(
        tokens: &mut cursor!(Tok),
        context: ParseContext<D>,
    ) -> std::result::Result<Self, E> {
        let mut items = vec![];
        let terminator = loop {
            let mut try_tokens = tokens.clone();
            match At::from_tokens(&mut try_tokens, context.clone()).nest()? {
                Ok(terminator) => {
                    *tokens = try_tokens;
                    break terminator;
                }
                Err(_) => {
                    items.push(T::from_tokens(tokens, context.clone())?);
                }
            }
        };
        Ok(Self { items, terminator })
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        self.items
            .iter()
            .flat_map(|item| item.to_tokens())
            .collect()
    }
    fn span(&self) -> Span {
        self.items
            .iter()
            .map(|item| item.span())
            .fold(Span::null(), Add::add)
    }
    fn to_item(self) -> (Span, Self::Item) {
        (
            self.span(),
            self.items.into_iter().map(|item| item.to_item()).collect(),
        )
    }
}

impl<Tok, E: SyntaxError, D: Clone, T: Syntax<Tok, E, D>> Syntax<Tok, E, D> for Vec<T> {
    type Item = Vec<(Span, T::Item)>;
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
        let mut out = vec![];
        loop {
            let mut try_tokens = tokens.clone();
            match T::from_tokens(&mut try_tokens, context.clone()) {
                Ok(value) => {
                    *tokens = try_tokens;
                    out.push(value)
                }
                Err(err) if err.recoverability() == Recoverability::Recoverable => {
                    return Ok(out);
                }
                Err(unrecoverable) => return Err(unrecoverable),
            }
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        self.iter().flat_map(|item| item.to_tokens()).collect()
    }
    fn span(&self) -> Span {
        self.iter()
            .map(|item| item.span())
            .reduce(|a, b| a + b)
            .unwrap_or(Span::null())
    }
    fn to_item(self) -> (Span, Self::Item) {
        (
            self.span(),
            self.into_iter().map(|item| item.to_item()).collect(),
        )
    }
}

impl<Tok, E: SyntaxError, D: Clone, T: Syntax<Tok, E, D>> Syntax<Tok, E, D> for Option<T> {
    type Item = Option<T::Item>;
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
        let mut try_tokens = tokens.clone();
        match T::from_tokens(&mut try_tokens, context) {
            Ok(item) => {
                *tokens = try_tokens;
                Ok(Some(item))
            }
            Err(recoverable) if recoverable.recoverability() == Recoverability::Recoverable => {
                Ok(None)
            }
            Err(unrecoverable) => Err(unrecoverable),
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        match self {
            Some(item) => item.to_tokens(),
            None => vec![],
        }
    }
    fn span(&self) -> Span {
        match self {
            Some(value) => value.span(),
            None => Span::null(),
        }
    }
    fn to_item(self) -> (Span, Self::Item) {
        (self.span(), self.map(|item| item.to_item().1))
    }
}

/// Retains the richer structure of a syntactical element.
///
/// `KeepElements::<T>::to_item` will not call `T::to_item` and will instead just return `T`.
pub struct KeepElements<T>(pub T);
impl<Tok, E: SyntaxError, D: Clone, T: Syntax<Tok, E, D>> Syntax<Tok, E, D> for KeepElements<T> {
    type Item = T;
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
        Ok(Self(T::from_tokens(tokens, context)?))
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        self.0.to_tokens()
    }
    fn span(&self) -> Span {
        self.0.span()
    }
    fn to_item(self) -> (Span, Self::Item) {
        (self.span(), self.0)
    }
}

/// Implements [`Syntax`] for each given `W<T>` where `T: Into<W>` and `W: Deref<Target = T>`.
macro_rules! wrapper_impl {
    ($($wrapper:ty),+) => {
        $(impl<Tok, E: SyntaxError, D: Clone, T: Syntax<Tok, E, D>> Syntax<Tok, E, D> for $wrapper {
            type Item = T::Item;
            fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self, E> {
                Ok(T::from_tokens(tokens, context)?.into())
            }
            fn to_tokens(&self) -> Vec<(Span, Tok)> {
                (**self).to_tokens()
            }
            fn span(&self) -> Span {
                (**self).span()
            }
            fn to_item(self) -> (Span, Self::Item) {
                (*self).to_item()
            }
        })+
    };
}

wrapper_impl!(Box<T>);

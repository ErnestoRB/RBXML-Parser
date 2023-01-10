use std::ops::RangeBounds;
pub mod utils;
pub use utils::*;

pub fn a_parser(input: &str) -> ParserResult<()> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// Constructs a parser that matches the expected string
pub fn parser_builder(expected: &'static str) -> impl Fn(&str) -> ParserResult<()> {
    move |input| match input.get(..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// Adhoc parser that matches the `<identifier>` for XML input
pub fn match_identifier(input: &str) -> ParserResult<String> {
    let mut matched = String::new();
    let mut it = input.chars();

    match it.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = it.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    Ok((&input[matched.len()..], matched))
}

/// Returns a parser that mix the result of the two parsers passed as
/// arguments, and put 'em on a tuple.
pub fn combinator<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        p1.parse(input).and_then(|(value_to_next, result_1)| {
            p2.parse(value_to_next)
                .map(|(output, result_2)| (output, (result_1, result_2)))
        })
    }
}

/// The same as [combinator] but ignores the result of the right parser and instead returns the result from left
pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(combinator(p1, p2), |(r1, _r2)| r1)
}

/// Same as [left] but returns the result of the right parser
pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(combinator(p1, p2), |(_r1, r2)| r2)
}

/// Returns a parser that "transform" the result from the input parser via a closure
pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

/// Returns a parser that matches the input parser one or more times
pub fn one_or_more<'a, O>(parser: impl Parser<'a, O>) -> impl Parser<'a, Vec<O>> {
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((after_parsing, parsed)) = parser.parse(input) {
            input = after_parsing;
            result.push(parsed);
        } else {
            return Err(input);
        }

        while let Ok((after_parsing, value)) = parser.parse(input) {
            input = after_parsing;
            result.push(value);
        }
        Ok((input, result))
    }
}

/// Creates a parser that matches the input parser the specified times in a [core::ops::range::RangeBounds]
///
/// # Example
/// ```
///  use rc_parserq::parser::*;
///  let two_more = times(parser_builder("."), 2..);
/// assert_eq!(two_more.parse("..."), Ok(("", vec![(), (), ()])));
/// assert_eq!(two_more.parse("."), Err("")); // '.' consumed
/// ```
///
pub fn times<'a, O>(
    parser: impl Parser<'a, O>,
    times: impl RangeBounds<usize>,
) -> impl Parser<'a, Vec<O>> {
    move |mut input: &'a str| {
        let start = match times.start_bound() {
            std::ops::Bound::Included(i) => *i,
            _ => 0,
        };
        let end = match times.start_bound() {
            std::ops::Bound::Excluded(i) => i - 1,
            _ => input.len() + 1,
        };

        let mut result = Vec::new();

        for _i in 0..start {
            if let Ok((after_parsing, parsed)) = parser.parse(input) {
                input = after_parsing;
                result.push(parsed);
            } else {
                return Err(input);
            }
        }

        for _i in start..end {
            if let Ok((after_parsing, parsed)) = parser.parse(input) {
                input = after_parsing;
                result.push(parsed);
            }
        }

        Ok((input, result))
    }
}

pub fn any_char(input: &str) -> ParserResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        _ => Err(input),
    }
}

pub fn predicate<'a, Out>(
    parser: impl Parser<'a, Out>,
    predicate: impl Fn(&Out) -> bool,
) -> impl Parser<'a, Out> {
    move |input| match parser.parse(input) {
        Ok((out, matched)) => {
            if predicate(&matched) {
                return Ok((out, matched));
            }
            return Err(input);
        }
        _ => Err(input),
    }
}

pub fn parse_whitespace<'a>() -> impl Parser<'a, char> {
    predicate(any_char, |char| char.is_whitespace())
}

pub fn quoted_string<'a>() -> impl Parser<'a, String> {
    map(
        right(
            parser_builder("\""),
            left(
                times(predicate(any_char, |char| *char != '"'), 1..),
                parser_builder("\""),
            ),
        ),
        |chars| chars.into_iter().collect(),
    )
}
pub fn attribute<'a>() -> impl Parser<'a, AttributePair> {
    combinator(
        map(
            times(predicate(any_char, |char| char.is_alphabetic()), 1..),
            |char| char.into_iter().collect(),
        ),
        right(parser_builder("="), quoted_string()),
    )
}
pub fn attributes<'a>() -> impl Parser<'a, Vec<AttributePair>> {
    times(right(times(parse_whitespace(), 1..), attribute()), ..)
}

pub fn element_start<'a>() -> impl Parser<'a, (String, Vec<AttributePair>)> {
    right(
        parser_builder("<"),
        combinator(match_identifier, attributes()),
    )
}

pub fn single_element<'a>() -> impl Parser<'a, Element> {
    map(
        left(
            element_start(),
            combinator(times(parse_whitespace(), ..), parser_builder("/>")),
        ),
        |(name, attributes)| Element {
            name,
            attributes,
            children: vec![],
        },
    )
}

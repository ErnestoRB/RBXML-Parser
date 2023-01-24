pub mod utils;
pub use utils::*;

/// Constructs a parser that matches the expected string
pub fn parser_builder<'a>(expected: &'a str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// Adhoc parser that matches the identifier part of `<identifier>` for XML input
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

pub fn or<'a, T, P1, P2>(p1: P1, p2: P2) -> impl Parser<'a, T>
where
    P1: Parser<'a, T> + 'a,
    P2: Parser<'a, T> + 'a,
{
    move |input| match p1.parse(input) {
        Ok(x) => Ok(x),
        Err(_) => p2.parse(input),
    }
}

/// Returns a parser that mix the result of the two parsers passed as
/// arguments, and put 'em on a tuple.
pub fn combinator<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| match p1.parse(input) {
        Ok((output1, result1)) => match p2.parse(output1) {
            Ok((output2, result2)) => Ok((output2, (result1, result2))),
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    }
}

/// The same as [combinator] but ignores the result of the right parser and instead returns the result from left
pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    combinator(p1, p2).map(|(r1, _r2)| r1)
}

/// Same as [left] but returns the result of the right parser
pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    R1: 'a,
    R2: 'a,
{
    combinator(p1, p2).map(|(_r1, r2)| r2)
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

/// Parses any char one time
pub fn any_char(input: &str) -> ParserResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        _ => Err(input),
    }
}

/// Apply a predicate to the result of the provided parser.
/// Useful if you need to define whether the parser should fail
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

/// Specific case of [any_char] and [predicate]
pub fn parse_whitespace<'a>() -> impl Parser<'a, char> {
    predicate(any_char, |char| char.is_whitespace())
}

pub fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        parser_builder("\""),
        left(
            predicate(any_char, |char| *char != '"').times(1..),
            parser_builder("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}
pub fn attribute<'a>() -> impl Parser<'a, AttributePair> {
    combinator(
        predicate(any_char, |char| char.is_alphabetic())
            .times(1..)
            .map(|char| char.into_iter().collect()),
        right(parser_builder("="), quoted_string()),
    )
}
pub fn attributes<'a>() -> impl Parser<'a, Vec<AttributePair>> {
    right(parse_whitespace().times(1..), attribute()).times(..)
}

pub fn element_start<'a>() -> impl Parser<'a, (String, Vec<AttributePair>)> {
    right(
        parser_builder("<"),
        right(
            parse_whitespace().times(..),
            combinator(match_identifier, attributes()),
        ),
    )
}

pub fn parent_element_opening_end<'a>() -> impl Parser<'a, ()> {
    combinator(parse_whitespace().times(..), parser_builder(">")).map(|_| {})
}

pub fn single_element_end<'a>() -> impl Parser<'a, ()> {
    combinator(parse_whitespace().times(..), parser_builder("/>")).map(|_| {})
}

pub fn closing_element<'a>(identifier: String) -> impl Parser<'a, String> {
    predicate(
        right(
            left(parser_builder("</"), parse_whitespace().times(..)),
            left(match_identifier, parent_element_opening_end()),
        ),
        move |input| *input == identifier,
    )
}

pub fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), single_element_end()).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

pub fn parent_element<'a>() -> impl Parser<'a, Element> {
    left(
        element_start(),
        combinator(parse_whitespace().times(..), parser_builder(">")),
    )
    .then(move |(identifier, attributes)| {
        left(
            right(parse_whitespace().times(..), element().times(..)),
            combinator(
                parse_whitespace().times(..),
                closing_element(identifier.clone()),
            ),
        )
        .map(move |children| Element {
            name: identifier.clone(),
            attributes: attributes.clone(),
            children,
        })
    })
}

pub fn element<'a>() -> impl Parser<'a, Element> {
    right(
        parse_whitespace().times(..),
        left(
            or(single_element(), parent_element()),
            parse_whitespace().times(..),
        ),
    )
}

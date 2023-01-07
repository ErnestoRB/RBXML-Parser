mod parser {
    use std::ops::RangeBounds;

    type ParserResult<'a, R> = Result<(&'a str, R), &'a str>;

    #[derive(Clone, Debug, PartialEq, Eq)]
    struct Element {
        name: String,
        attributes: Vec<(String, String)>,
        children: Vec<Element>,
    }

    pub fn a_parser(input: &str) -> ParserResult<()> {
        match input.chars().next() {
            Some('a') => Ok((&input['a'.len_utf8()..], ())),
            _ => Err(input),
        }
    }

    pub fn parser_builder(expected: &'static str) -> impl Fn(&str) -> ParserResult<()> {
        move |input| match input.get(..expected.len()) {
            Some(next) if next == expected => Ok((&input[expected.len()..], ())),
            _ => Err(input),
        }
    }

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

    pub fn combinator<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        /* move |input| match p1(input) {
            Ok((a, b)) => match p2(a) {
                Ok((c, d)) => Ok((c, (b, d))),
                Err(e2) => Err(e2),
            },
            Err(e1) => Err(e1),
        } */

        move |input| {
            p1.parse(input).and_then(|(value_to_next, result_1)| {
                p2.parse(value_to_next)
                    .map(|(output, result_2)| (output, (result_1, result_2)))
            })
        }
    }

    pub fn left<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R1>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        map(combinator(p1, p2), |(r1, _r2)| r1)
    }

    pub fn right<'a, P1, P2, R1, R2>(p1: P1, p2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        map(combinator(p1, p2), |(_r1, r2)| r2)
    }

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

    pub trait Parser<'a, Output> {
        fn parse(&self, input: &'a str) -> ParserResult<'a, Output>;
    }

    impl<'a, Function, Output> Parser<'a, Output> for Function
    where
        Function: Fn(&'a str) -> ParserResult<'a, Output>,
    {
        fn parse(&self, input: &'a str) -> ParserResult<'a, Output> {
            self(input)
        }
    }
}

mod testing {

    use crate::parser::Parser;

    use super::parser;

    #[test]
    fn a_parser() {
        let string = "equivocado";
        let string2 = "ahuevo";
        assert_eq!(parser::a_parser(string), Err(string));
        assert_eq!(parser::a_parser(string2), Ok(("huevo", ())));
    }

    #[test]
    fn parser_builder() {
        let matching = "<";
        let parser = parser::parser_builder(matching);
        assert_eq!(parser("dasdasd"), Err("dasdasd"));
        assert_eq!(parser("<h1>"), Ok(("h1>", ())));
    }

    #[test]
    fn match_identifier() {
        assert_eq!(
            parser::match_identifier("body"),
            Ok(("", "body".to_string()))
        );
        assert_eq!(
            parser::match_identifier("h1 center='a'>"),
            Ok((" center='a'>", "h1".to_string()))
        );
        assert_eq!(
            parser::match_identifier(" h1 center='a'>"),
            Err(" h1 center='a'>")
        );
    }

    #[test]
    fn combinator() {
        let c_parser = parser::combinator(parser::parser_builder("<"), parser::match_identifier);
        assert_eq!(c_parser.parse("<h1>"), Ok((">", ((), "h1".to_string()))));
        assert_eq!(c_parser.parse("!<h1>"), Err("!<h1>"));
        assert_eq!(c_parser.parse("<!h1>"), Err("!h1>"));
    }

    #[test]
    fn map() {
        let mapped = parser::map(|input| Ok((input, "ok")), |_output| 5);
        let str = "useless string";
        assert_eq!(mapped.parse(str), Ok((str, 5)));
    }

    #[test]
    fn left() {
        let parser = parser::left(parser::parser_builder("<"), parser::match_identifier);

        assert_eq!(parser.parse("<a"), Ok(("", ())));
        assert_eq!(parser.parse("<!"), Err("!"));
    }

    #[test]
    fn right() {
        let parser = parser::right(parser::parser_builder("<"), parser::match_identifier);

        assert_eq!(parser.parse("<a"), Ok(("", "a".to_string())));
        assert_eq!(parser.parse("<!"), Err("!"));
    }

    #[test]
    fn one_or_more() {
        let mut matched = Vec::new();
        matched.push(());
        matched.push(());
        let parser = parser::one_or_more(parser::parser_builder("."));
        assert_eq!(parser.parse(".."), Ok(("", matched)));
        assert_eq!(parser.parse(""), Err(""));
    }

    #[test]
    fn times() {
        let mut one_more_matched = Vec::new();
        one_more_matched.push(());
        one_more_matched.push(());
        let one_more = parser::times(parser::parser_builder("."), 1..);
        assert_eq!(one_more.parse(".."), Ok(("", one_more_matched)));
        assert_eq!(one_more.parse(""), Err(""));
        let two_more = parser::times(parser::parser_builder("."), 2..);
        let mut two_more_matched = Vec::new();
        two_more_matched.push(());
        two_more_matched.push(());
        two_more_matched.push(());
        assert_eq!(two_more.parse("..."), Ok(("", two_more_matched)));
        assert_eq!(two_more.parse("."), Err("")); // '.' consumed
    }
}

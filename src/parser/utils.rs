use std::ops::RangeBounds;

pub type ParserResult<'a, Output> = Result<(&'a str, Output), &'a str>;

pub fn constraint<'b, T>(parser: impl Parser<'b, T>) -> impl Parser<'b, T> {
    parser
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

pub type AttributePair = (String, String);
pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, Output>;
    fn map<Mapper, MappedOutput>(self, mapper: Mapper) -> BoxedParser<'a, MappedOutput>
    where
        Self: Sized + 'a, // El parser tiene que ser construido en tiempo de compilación (no caja)
        Mapper: Fn(Output) -> MappedOutput + 'a,
    {
        BoxedParser::new(move |input| {
            self.parse(input)
                .map(|(next_input, result)| (next_input, mapper(result)))
        })
    }
    /// Creates a parser that matches the input parser the specified times in a [core::ops::range::RangeBounds]
    ///
    /// # Example
    /// ```
    ///  use rc_parser::parser::*;
    ///  let two_more = parser_builder(".").times( 2..);
    /// assert_eq!(two_more.parse("..."), Ok(("", vec![(), (), ()])));
    /// assert_eq!(two_more.parse("."), Err("")); // '.' consumed
    /// ```
    ///
    fn times<'b>(self, times: impl RangeBounds<usize> + 'b) -> BoxedParser<'a, Vec<Output>>
    where
        Self: Sized + 'a,
        'b: 'a,
    {
        BoxedParser::new(move |mut input: &'a str| {
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
                if let Ok((after_parsing, parsed)) = self.parse(input) {
                    input = after_parsing;
                    result.push(parsed);
                } else {
                    return Err(input);
                }
            }

            for _i in start..end {
                if let Ok((after_parsing, parsed)) = self.parse(input) {
                    input = after_parsing;
                    result.push(parsed);
                }
            }

            Ok((input, result))
        })
    }

    fn then<F, NewParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        NewParser: Parser<'a, NewOutput>,
        F: Fn(Output) -> NewParser + 'a,
    {
        BoxedParser::new(move |input| match self.parse(input) {
            Ok((next, result)) => f(result).parse(next),
            Err(e) => Err(e),
        })
    }
}

impl<'a, Function, Output> Parser<'a, Output> for Function
where
    Function: Fn(&'a str) -> ParserResult<'a, Output>,
{
    fn parse(&self, input: &'a str) -> ParserResult<'a, Output> {
        self(input)
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, Output> {
        self.parser.parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Element {
    pub name: String,
    pub attributes: Vec<(String, String)>,
    pub children: Vec<Element>,
}

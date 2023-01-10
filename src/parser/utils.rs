pub type ParserResult<'a, Output> = Result<(&'a str, Output), &'a str>;

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
    fn map<Mapper, MappedOutput>(&'a self, mapper: Mapper) -> BoxedParser<'a, MappedOutput>
    where
        Self: Sized, // El parser tiene que ser construido en tiempo de compilación (no caja)
        Mapper: Fn(Output) -> MappedOutput + 'a,
    {
        BoxedParser::new(move |input| {
            self.parse(input)
                .map(|(next_input, result)| (next_input, mapper(result)))
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

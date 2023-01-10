use crate::parser;
use crate::parser::utils::Parser;

use std::vec;

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
    let zero_more = parser::times(parser::parser_builder("a"), 0..);
    assert_eq!(zero_more.parse("xx"), Ok(("xx", vec![])));
    assert_eq!(zero_more.parse("aaax"), Ok(("x", vec![(), (), ()])));
    let one_more = parser::times(parser::parser_builder("."), 1..);
    assert_eq!(one_more.parse(".."), Ok(("", vec![(), ()])));
    assert_eq!(one_more.parse(""), Err(""));
    let two_more = parser::times(parser::parser_builder("."), 2..);
    assert_eq!(two_more.parse("..."), Ok(("", vec![(), (), ()])));
    assert_eq!(two_more.parse("."), Err("")); // '.' consumed

    let all_except_quotes = parser::map(
        parser::times(
            parser::predicate(parser::any_char, |char| *char != '"'),
            1..,
        ),
        |chars| chars.into_iter().collect(),
    );
    assert_eq!(
        all_except_quotes.parse("asdasd"),
        Ok(("", "asdasd".to_string()))
    );
    assert_eq!(
        all_except_quotes.parse("asd\"asd"),
        Ok(("\"asd", "asd".to_string()))
    );
    assert_eq!(all_except_quotes.parse("\"asd"), Err("\"asd"))
}

#[test]
fn predicate_char() {
    let conditional_parse = parser::predicate(parser::any_char, |char| *char == '*');
    assert_eq!(conditional_parse.parse("*nix"), Ok(("nix", '*')));
    assert_eq!(conditional_parse.parse("lol"), Err("lol"));
}

#[test]
fn quoted_string() {
    assert_eq!(
        parser::quoted_string().parse("\"hola\""),
        Ok(("", "hola".to_string())),
    )
}

#[test]
fn attribute() {
    assert_eq!(
        parser::attribute().parse("a=\"hola\""),
        Ok(("", ("a".to_string(), "hola".to_string())))
    )
}

#[test]
fn attributes() {
    assert_eq!(
        parser::attributes().parse("   a=\"hola\"  b=\"adios\""),
        Ok((
            "",
            vec![
                ("a".to_string(), "hola".to_string()),
                ("b".to_string(), "adios".to_string())
            ]
        ))
    )
}

#[test]
fn element_start() {
    assert_eq!(
        parser::element_start().parse("<Component props=\"hola\""),
        Ok((
            "",
            (
                "Component".to_string(),
                vec![("props".to_string(), "hola".to_string())]
            )
        ))
    );
    assert_eq!(
        parser::element_start().parse("<Component props=\"hola\">"),
        Ok((
            ">",
            (
                "Component".to_string(),
                vec![("props".to_string(), "hola".to_string())]
            )
        ))
    );
    assert_eq!(
        parser::element_start().parse("Component props=\"hola\">"),
        Err("Component props=\"hola\">",)
    )
}

#[test]
fn single_element() {
    assert_eq!(
        parser::single_element().parse("<Component    props=\"hola\"  />"),
        Ok((
            "",
            parser::Element {
                name: "Component".to_string(),
                attributes: vec![("props".to_string(), "hola".to_string())],
                children: vec![]
            }
        ))
    );
    assert_eq!(
        parser::single_element().parse("<Component props=\"hola\" >"),
        Err(">")
    );
}

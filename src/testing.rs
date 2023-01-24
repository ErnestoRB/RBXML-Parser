use crate::parser::utils::Parser;
use crate::parser::{self, Element};
use std::vec;

#[test]
fn parser_builder() {
    let matching = "<";
    let parser = parser::parser_builder(matching);
    assert_eq!(parser.parse("dasdasd"), Err("dasdasd"));
    assert_eq!(parser.parse("<h1>"), Ok(("h1>", ())));
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
    let mapped = parser::constraint(|input| Ok((input, "ok"))).map(|_output| 5);
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
    let zero_more = parser::parser_builder("a").times(0..);
    assert_eq!(zero_more.parse("xx"), Ok(("xx", vec![])));
    assert_eq!(zero_more.parse("aaax"), Ok(("x", vec![(), (), ()])));
    let one_more = parser::parser_builder(".").times(1..);
    assert_eq!(one_more.parse(".."), Ok(("", vec![(), ()])));
    assert_eq!(one_more.parse(""), Err(""));
    let two_more = parser::parser_builder(".").times(2..);
    assert_eq!(two_more.parse("..."), Ok(("", vec![(), (), ()])));
    assert_eq!(two_more.parse("."), Err("")); // '.' consumed

    let all_except_quotes = parser::predicate(parser::any_char, |char| *char != '"')
        .times(1..)
        .map(|chars| chars.into_iter().collect());
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
        parser::element_start().parse("< Component props=\"hola\""),
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

#[test]
fn element_end() {
    assert_eq!(
        parser::parent_element_opening_end().parse("      >"),
        Ok(("", ()))
    );
    assert_eq!(
        parser::parent_element_opening_end().parse("      />"),
        Err("/>")
    );
}

#[test]
fn closing_element() {
    assert_eq!(
        parser::closing_element("asd".to_string()).parse("</   asd   >"),
        Ok(("", "asd".to_string()))
    );
    assert_eq!(
        parser::closing_element("asd".to_string()).parse("<   asd   >"),
        Err("<   asd   >")
    );
}

#[test]
fn parent_element() {
    assert_eq!(
        parser::parent_element().parse("<hola></hola>"),
        Ok((
            "",
            Element {
                name: "hola".to_string(),
                attributes: vec![],
                children: vec![],
            }
        ))
    );
}

#[test]
fn xml_parser() {
    let doc = r#"
        <top label="Top">
        
            <semi-bottom label="Bottom"/>

            <middle>
                <bottom label="Another bottom"/>
            </middle>
        </top>"#;
    let parsed_doc = Element {
        name: "top".to_string(),
        attributes: vec![("label".to_string(), "Top".to_string())],
        children: vec![
            Element {
                name: "semi-bottom".to_string(),
                attributes: vec![("label".to_string(), "Bottom".to_string())],
                children: vec![],
            },
            Element {
                name: "middle".to_string(),
                attributes: vec![],
                children: vec![Element {
                    name: "bottom".to_string(),
                    attributes: vec![("label".to_string(), "Another bottom".to_string())],
                    children: vec![],
                }],
            },
        ],
    };
    assert_eq!(Ok(("", parsed_doc)), parser::element().parse(doc));
}

#[test]
fn mismatched_closing_tag() {
    let doc = r#"
        <top>
            <bottom/>
        </middle>"#;
    assert_eq!(Err("</middle>"), parser::element().parse(doc));
}

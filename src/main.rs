use rc_parser::parser::element;
use rc_parser::parser::Parser;
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    match handle.read_to_string(&mut buffer) {
        Ok(_) => {
            println!("Tratando de analizar la cadena '{}'", buffer);
            match element().parse(&buffer) {
                Ok(element) => {
                    println!("Cadena restante: '{}'", element.0);
                    println!("Resultado: {:?}", element.1)
                }
                Err(output) => println!("Error al analizar la cadena: '{}'", output),
            }
        }
        Err(_) => println!("Ha ocurrido un error"),
    }
}

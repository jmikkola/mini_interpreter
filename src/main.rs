use std::io::{stdin, BufReader, BufRead};
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Value {
    IntVal(i64),
    FloatVal(f64),
    StringVal(String),
    CharVar(char),
    BoolVal(bool),
}

#[derive(Debug, Clone)]
enum Command {
    Push(Value),
    Pop,
    Dup,
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
    Print,
    MkType,
    New,
    GetRef,
    SetRef,
    SysGC,
    SysMemstats,
}

fn read_lines() -> Vec<String> {
    BufReader::new(stdin()).lines()
        .map(|l| l.unwrap().trim().to_owned())
        .collect()
}

fn parse_command(line: String) -> Result<Command, String> {
    Ok(Command::Dup)
}

fn build(lines: Vec<String>) -> Result<(Vec<Command>, HashMap<String, usize>), String> {
    let mut commands = vec![];
    let mut names = HashMap::new();

    for mut line in lines {
        if line.len() == 0 || line.starts_with("#") {
            continue
        } else if line.starts_with("!") {
            names.insert(line.trim_left_matches("!").to_owned(), commands.len());
        } else {
            commands.push(try!(parse_command(line)))
        }
    }

    Ok((commands, names))
}

fn main() {
    let lines = read_lines();
    println!("{:?}", lines);
    println!("{:?}", build(lines));
}

use std::io::{stdin, BufReader, BufRead};
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
enum Value {
    IntVal(i64),
    FloatVal(f64),
    StringVal(String),
    CharVar(char),
    BoolVal(bool),
}

#[derive(PartialEq, Debug, Clone)]
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
    BufReader::new(stdin())
        .lines()
        .map(|l| l.unwrap().trim().to_owned())
        .collect()
}

fn parse_err<a>(msg: &str) -> Result<a, String> {
    Err(msg.to_owned())
}

fn parse_value(parts: &Vec<&str>) -> Result<Value, String> {
    parse_err("not implemented")
}

fn parse_command(line: String) -> Result<Command, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 1 {
        return parse_err("empty command?");
    }
    match parts[0] {
        "push" => {
            if parts.len() > 2 {
                Ok(Command::Push(try!(parse_value(&parts))))
            } else {
                parse_err("invalid push statement")
            }
        }
        "pop" => Ok(Command::Pop),
        "dup" => Ok(Command::Dup),
        "plus" => Ok(Command::Plus),
        "minus" => Ok(Command::Minus),
        "times" => Ok(Command::Times),
        "divide" => Ok(Command::Divide),
        "mod" => Ok(Command::Mod),
        "print" => Ok(Command::Print),
        "mktype" => Ok(Command::MkType),
        "new" => Ok(Command::New),
        "getref" => Ok(Command::GetRef),
        "setref" => Ok(Command::SetRef),
        "sysgc" => Ok(Command::SysGC),
        "sysmemstats" => Ok(Command::SysMemstats),
        _ => Err("invalid command: ".to_owned() + line.as_str()),
    }
}

fn build(lines: Vec<String>) -> Result<(Vec<Command>, HashMap<String, usize>), String> {
    let mut commands = vec![];
    let mut names = HashMap::new();

    for line in lines {
        if line.len() == 0 || line.starts_with("#") {
            continue;
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

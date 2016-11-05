use std::io::{stdin, BufReader, BufRead};
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
enum Value {
    I(i64),
    F(f64),
    S(String),
    C(char),
    B(bool),
    R(i32, i32),
}

impl Value {
    fn to_str(&self) -> String {
        match *self {
            Value::I(ref i) => format!("{}", i),
            Value::F(ref f) => format!("{}", f),
            Value::S(ref s) => s.clone(),
            Value::C(ref c) => format!("{}", c),
            Value::B(ref b) => format!("{}", b),
            Value::R(ref t, ref v) => format!("<{}, {}>", t, v),
        }
    }
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

fn parse_err<A>(msg: &str) -> Result<A, String> {
    Err(msg.to_owned())
}

fn parse_value(parts: &Vec<&str>) -> Result<Value, String> {
    if parts.len() <= 2 {
        return parse_err("push stmt too short");
    }
    match parts[1] {
        "int" => Ok(Value::I(parts[2].to_owned().parse().unwrap())),
        "float" => Ok(Value::F(parts[2].to_owned().parse().unwrap())),
        "string" => Ok(Value::S(parts[2].to_owned())),
        "char" => Ok(Value::C(parts[2].chars().next().unwrap())),
        "bool" => Ok(Value::B(parts[2].to_owned().parse().unwrap())),
        _ => parse_err("invalid value"),
    }
}

fn parse_command(line: String) -> Result<Command, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 1 {
        return parse_err("empty command?");
    }
    match parts[0] {
        "push" => Ok(Command::Push(try!(parse_value(&parts)))),
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

fn interpret(commands: Vec<Command>, names: HashMap<String, usize>) {
    let mut stack: Vec<Value> = vec![];
//    let mut heap = HashMap::new();
//    let mut types = vec![];
    let mut next_obj_id = 1;

    let mut pc = 0;
    while pc < commands.len() {
        let cmd = &commands[pc];
        pc += 1;

        match *cmd {
            Command::Push(ref v) => {
                stack.push(v.clone());
            },
            Command::Pop => {
                stack.pop();
            },
            Command::Dup => {
                let top_val = stack.last().unwrap().clone();
                stack.push(top_val);
            },
            Command::Plus => {}
            Command::Minus => {}
            Command::Times => {}
            Command::Divide => {}
            Command::Mod => {}
            Command::Print => {
                let top_val = stack.last().unwrap();
                println!("{}", top_val.to_str());
            }
            Command::MkType => {}
            Command::New => {}
            Command::GetRef => {}
            Command::SetRef => {}
            Command::SysGC => {}
            Command::SysMemstats => {}
        };
    }
}

fn main() {
    match build(read_lines()) {
        Ok((commands, names)) => interpret(commands, names),
        Err(err) => println!("Error! {}", err),
    };
}

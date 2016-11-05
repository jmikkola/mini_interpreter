use std::io::{stdin, BufReader, BufRead};
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
enum Value {
    I(i64),
    F(f64),
    S(String),
    C(char),
    B(bool),
    R(i64, i64),
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

    fn must_string(&self) -> String {
        match *self {
            Value::S(ref s) => s.clone(),
            _ => panic!("not a string"),
        }
    }

    fn must_int(&self) -> i64 {
        match *self {
            Value::I(ref i) => *i,
            _ => panic!("not a string"),
        }
    }

    fn must_ref(&self) -> (i64, i64) {
        match *self {
            Value::R(ref t, ref v) => (*t, *v),
            _ => panic!("not a string"),
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

enum MathOp {
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}

fn math(stack: &mut Vec<Value>, op :MathOp) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    let result = match (left, right) {
        (Value::I(l), Value::I(r)) => match op {
            MathOp::Plus => Value::I(l + r),
            MathOp::Minus => Value::I(l - r),
            MathOp::Times => Value::I(l * r),
            MathOp::Divide => Value::I(l / r),
            MathOp::Mod => Value::I(l % r),
        },
        (Value::F(l), Value::F(r)) => match op {
            MathOp::Plus => Value::F(l + r),
            MathOp::Minus => Value::F(l - r),
            MathOp::Times => Value::F(l * r),
            MathOp::Divide => Value::F(l / r),
            MathOp::Mod => Value::F(l % r),
        },
        _ => panic!("can't apply math operators"),
    };
    stack.push(result);
}

fn default_value(t: &String) -> Value {
    match t.as_ref() {
        "int" => Value::I(0),
        "float" => Value::F(0.),
        "string" => Value::S(String::new()),
        "char" => Value::C('\0'),
        "bool" => Value::B(false),
        "ref" => Value::R(0, 0),
        _ => panic!("invalid type"),
    }
}

fn interpret(commands: Vec<Command>, names: HashMap<String, usize>) {
    let mut stack: Vec<Value> = vec![];
    let mut heap: HashMap<i64, Vec<Value>> = HashMap::new();
    let mut types = vec![];
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
            Command::Plus => {
                math(&mut stack, MathOp::Plus);
            },
            Command::Minus => {
                math(&mut stack, MathOp::Minus);
            },
            Command::Times => {
                math(&mut stack, MathOp::Times);
            },
            Command::Divide => {
                math(&mut stack, MathOp::Divide);
            },
            Command::Mod => {
                math(&mut stack, MathOp::Mod);
            },
            Command::Print => {
                let top_val = stack.last().unwrap();
                println!("{}", top_val.to_str());
            },
            Command::MkType => {
                let name = stack.pop().unwrap().must_string();
                let n_fields = stack.pop().unwrap().must_int();
                let mut fields = vec![];
                for _ in 0..n_fields {
                    fields.push(stack.pop().unwrap().must_string());
                }

                types.push((name, fields));
                stack.push(Value::I((types.len() - 1) as i64));
            },
            Command::New => {
                let type_id = stack.pop().unwrap().must_int();
                let t = &types[type_id as usize];
                let val_id = next_obj_id;
                next_obj_id += 1;

                heap.insert(val_id, t.1.iter().map(default_value).collect());
                stack.push(Value::R(type_id, val_id));
            },
            Command::GetRef => {
                let field = stack.pop().unwrap().must_int();
                let (_, val_id) = stack.pop().unwrap().must_ref();
                stack.push(heap[&val_id][field as usize].clone());
            },
            Command::SetRef => {
                let value = stack.pop().unwrap();
                let field = stack.pop().unwrap().must_int();
                let (_, val_id) = stack.pop().unwrap().must_ref();
                heap.get_mut(&val_id).unwrap()[field as usize] = value;
            },
            Command::SysGC => {},
            Command::SysMemstats => {},
        };
    }
}

fn main() {
    match build(read_lines()) {
        Ok((commands, names)) => interpret(commands, names),
        Err(err) => println!("Error! {}", err),
    };
}

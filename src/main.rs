use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::io::{stdin, BufReader, BufRead};

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
            _ => panic!("not an int"),
        }
    }

    fn must_ref(&self) -> (i64, i64) {
        match *self {
            Value::R(ref t, ref v) => (*t, *v),
            _ => panic!("not a reference"),
        }
    }

    fn must_bool(&self) -> bool {
        match *self {
            Value::B(b) => b,
            _ => panic!("not a boolean"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Function<T> {
    name: String,
    commands: Vec<Command<T>>,
    names: HashMap<String, usize>,
    n_args: i32,
    n_locals: i32,
}

#[derive(PartialEq, Debug, Clone)]
enum Command<T> {
    Noop,
    Push(Value),
    Pop,
    Dup,
    DupN(usize),
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    Jump(T),
    JIf(T),
    Print,
    MkType,
    New,
    GetRef,
    SetRef,
    Call(T),
    Return,
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

fn parse_int(s: &str) -> Result<i64, String> {
    s.to_owned().parse().map_err(|_| format!("can't parse {} as int", s))
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

fn parse_command(line: String) -> Result<Command<String>, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 1 {
        return parse_err("empty command?");
    }
    match parts[0] {
        "noop" => Ok(Command::Noop),
        "push" => Ok(Command::Push(try!(parse_value(&parts)))),
        "pop" => Ok(Command::Pop),
        "dup" => Ok(Command::Dup),
        "dup_n" => Ok(Command::DupN(try!(parse_int(parts[1])) as usize)),
        "plus" => Ok(Command::Plus),
        "minus" => Ok(Command::Minus),
        "times" => Ok(Command::Times),
        "divide" => Ok(Command::Divide),
        "mod" => Ok(Command::Mod),
        "eq" => Ok(Command::Eq),
        "neq" => Ok(Command::Neq),
        "gt" => Ok(Command::Gt),
        "gte" => Ok(Command::Gte),
        "lt" => Ok(Command::Lt),
        "lte" => Ok(Command::Lte),
        "jump" => Ok(Command::Jump(parts[1].to_owned())),
        "jif" => Ok(Command::JIf(parts[1].to_owned())),
        "print" => Ok(Command::Print),
        "mktype" => Ok(Command::MkType),
        "new" => Ok(Command::New),
        "getref" => Ok(Command::GetRef),
        "setref" => Ok(Command::SetRef),
        "call" => Ok(Command::Call(parts[1].to_owned())),
        "return" => Ok(Command::Return),
        "sysgc" => Ok(Command::SysGC),
        "sysmemstats" => Ok(Command::SysMemstats),
        _ => Err("invalid command: ".to_owned() + line.as_str()),
    }
}

fn parse_fn(line: String) -> Result<Function<String>, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() != 3 {
        return parse_err("invalid function def");
    }

    Ok(Function {
        name: parts[1].to_owned(),
        commands: Vec::new(),
        names: HashMap::new(),
        n_locals: 0, // TODO?
        n_args: try!(parts[2].parse().map_err(|_| "can't parse int")),
    })
}

fn build(lines: Vec<String>) -> Result<Vec<Function<usize>>, String> {
    let mut functions = vec![];
    let mut main_idx = 0;

    // First pass
    for line in lines {
        if line.len() == 0 || line.starts_with("#") {
            continue;
        } else if line.starts_with(".F") {
            let f = try!(parse_fn(line));
            if f.name.as_str() == "main" {
                main_idx = functions.len();
            }
            functions.push(f);
        } else if line.starts_with("!") {
            if let Some(mut f) = functions.last_mut() {
                let name = line.trim_left_matches("!").to_owned();
                let location = f.commands.len();
                f.names.insert(name, location);
            } else {
                return parse_err("command outside function?");
            }
        } else {
            if let Some(mut f) = functions.last_mut() {
                let command = try!(parse_command(line));
                // TODO: check for local var frame size?
                f.commands.push(command);
            } else {
                return parse_err("command outside function?");
            }
        }
    }

    // Make sure main always appears at index 0
    functions.swap(0, main_idx);

    // Second pass
    let func_names: HashMap<String, usize> = functions.iter()
        .enumerate()
        .map(|(i, f)| (f.name.clone(), i))
        .collect();
    let out = functions.iter()
        .map(|f| {
            let commands: Vec<Command<usize>> = f.commands
                .iter()
                .map(|cmd| {
                    match cmd.clone() {
                        Command::Jump(label) => Command::Jump(*f.names.get(&label).unwrap()),
                        Command::JIf(label) => Command::JIf(*f.names.get(&label).unwrap()),
                        Command::Call(name) => Command::Call(*func_names.get(&name).unwrap()),
                        // leave the rest alone
                        Command::Noop => Command::Noop,
                        Command::Push(v) => Command::Push(v),
                        Command::Pop => Command::Pop,
                        Command::Dup => Command::Dup,
                        Command::DupN(n) => Command::DupN(n),
                        Command::Plus => Command::Plus,
                        Command::Minus => Command::Minus,
                        Command::Times => Command::Times,
                        Command::Divide => Command::Divide,
                        Command::Mod => Command::Mod,
                        Command::Eq => Command::Eq,
                        Command::Neq => Command::Neq,
                        Command::Gt => Command::Gt,
                        Command::Gte => Command::Gte,
                        Command::Lt => Command::Lt,
                        Command::Lte => Command::Lte,
                        Command::Print => Command::Print,
                        Command::MkType => Command::MkType,
                        Command::New => Command::New,
                        Command::GetRef => Command::GetRef,
                        Command::SetRef => Command::SetRef,
                        Command::Return => Command::Return,
                        Command::SysGC => Command::SysGC,
                        Command::SysMemstats => Command::SysMemstats,
                    }
                })
                .collect();

            Function {
                name: f.name.clone(),
                commands: commands,
                names: f.names.clone(),
                n_args: f.n_args,
                n_locals: f.n_locals,
            }
        })
        .collect();

    Ok(out)
}

enum MathOp {
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}

fn math(stack: &mut Vec<Value>, op: MathOp) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    let result = match (left, right) {
        (Value::I(l), Value::I(r)) => {
            match op {
                MathOp::Plus => Value::I(l + r),
                MathOp::Minus => Value::I(l - r),
                MathOp::Times => Value::I(l * r),
                MathOp::Divide => Value::I(l / r),
                MathOp::Mod => Value::I(l % r),
            }
        }
        (Value::F(l), Value::F(r)) => {
            match op {
                MathOp::Plus => Value::F(l + r),
                MathOp::Minus => Value::F(l - r),
                MathOp::Times => Value::F(l * r),
                MathOp::Divide => Value::F(l / r),
                MathOp::Mod => Value::F(l % r),
            }
        }
        _ => panic!("can't apply math operators"),
    };
    stack.push(result);
}

fn compare_equality(stack: &mut Vec<Value>, is_eq: bool) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(Value::B(is_eq == (left == right)));
}

#[derive(PartialEq, Debug)]
enum Comp {
    Gt,
    Gte,
    Lt,
    Lte,
}

fn compare(stack: &mut Vec<Value>, comp: Comp) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    let result = match (left, right) {
        (Value::I(l), Value::I(r)) => {
            match comp {
                Comp::Gt => l > r,
                Comp::Gte => l >= r,
                Comp::Lt => l < r,
                Comp::Lte => l <= r,
            }
        }
        (Value::F(l), Value::F(r)) => {
            match comp {
                Comp::Gt => l > r,
                Comp::Gte => l >= r,
                Comp::Lt => l < r,
                Comp::Lte => l <= r,
            }
        }
        _ => panic!("can't apply comparison operator"),
    };
    stack.push(Value::B(result));
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

#[derive(Debug)]
struct Frame {
    stack: Vec<Value>,
    locals: Vec<Value>,
    prev_fn: usize,
    prev_pc: usize,
}

fn new_frame(prev_pc: usize, prev_fn: usize) -> Frame {
    Frame {
        stack: vec![],
        locals: vec![],
        prev_pc: prev_pc,
        prev_fn: prev_fn,
    }
}

fn interpret(functions: Vec<Function<usize>>) {
    let mut types = vec![];

    let mut stack: Vec<Frame> = vec![];
    let mut frame = new_frame(0, 99999999);

    let mut heap: HashMap<i64, Vec<Value>> = HashMap::new();
    let mut next_obj_id = 1;

    let mut fn_number = 0;
    let mut f = &functions[fn_number];
    let mut pc = 0;
    while pc < f.commands.len() {
        let cmd = &f.commands[pc];
        pc += 1;

        match *cmd {
            Command::Noop => {}
            Command::Push(ref v) => {
                frame.stack.push(v.clone());
            }
            Command::Pop => {
                frame.stack.pop();
            }
            Command::Dup => {
                let top_val = frame.stack.last().unwrap().clone();
                frame.stack.push(top_val);
            }
            Command::DupN(n) => {
                let len = frame.stack.len();
                let val = frame.stack[len - n - 1].clone();
                frame.stack.push(val);
            }
            Command::Plus => {
                math(&mut frame.stack, MathOp::Plus);
            }
            Command::Minus => {
                math(&mut frame.stack, MathOp::Minus);
            }
            Command::Times => {
                math(&mut frame.stack, MathOp::Times);
            }
            Command::Divide => {
                math(&mut frame.stack, MathOp::Divide);
            }
            Command::Mod => {
                math(&mut frame.stack, MathOp::Mod);
            }
            Command::Eq => {
                compare_equality(&mut frame.stack, true);
            }
            Command::Neq => {
                compare_equality(&mut frame.stack, false);
            }
            Command::Gt => {
                compare(&mut frame.stack, Comp::Gt);
            }
            Command::Gte => {
                compare(&mut frame.stack, Comp::Gte);
            }
            Command::Lt => {
                compare(&mut frame.stack, Comp::Lt);
            }
            Command::Lte => {
                compare(&mut frame.stack, Comp::Lte);
            }
            Command::Jump(ref location) => pc = *location,
            Command::JIf(ref location) => {
                if frame.stack.pop().unwrap().must_bool() {
                    pc = *location
                }
            }
            Command::Print => {
                let top_val = frame.stack.pop().unwrap();
                println!("{}", top_val.to_str());
            }
            Command::MkType => {
                let name = frame.stack.pop().unwrap().must_string();
                let n_fields = frame.stack.pop().unwrap().must_int();
                let mut fields = vec![];
                for _ in 0..n_fields {
                    fields.push(frame.stack.pop().unwrap().must_string());
                }

                types.push((name, fields));
                frame.stack.push(Value::I((types.len() - 1) as i64));
            }
            Command::New => {
                let type_id = frame.stack.pop().unwrap().must_int();
                let t = &types[type_id as usize];
                let val_id = next_obj_id;
                next_obj_id += 1;

                heap.insert(val_id, t.1.iter().map(default_value).collect());
                frame.stack.push(Value::R(type_id, val_id));
            }
            Command::GetRef => {
                let field = frame.stack.pop().unwrap().must_int();
                let (_, val_id) = frame.stack.pop().unwrap().must_ref();
                frame.stack.push(heap[&val_id][field as usize].clone());
            }
            Command::SetRef => {
                let value = frame.stack.pop().unwrap();
                let field = frame.stack.pop().unwrap().must_int();
                let (_, val_id) = frame.stack.pop().unwrap().must_ref();
                heap.get_mut(&val_id).unwrap()[field as usize] = value;
            }
            Command::Call(ref fn_no) => {
                let called_f = &functions[*fn_no];
                let mut next_frame = new_frame(pc, fn_number);

                for _ in 0..called_f.n_args {
                    next_frame.stack.push(frame.stack.pop().unwrap());
                }
                // Fix the argument order:
                next_frame.stack.reverse();

                stack.push(frame);
                frame = next_frame;
                f = called_f;
                fn_number = *fn_no;
                pc = 0;
            }
            Command::Return => {
                let return_val = frame.stack.pop().unwrap();

                pc = frame.prev_pc;
                f = &functions[frame.prev_fn];
                frame = stack.pop().unwrap();

                frame.stack.push(return_val);
            }
            Command::SysGC => {
                let mut live_refs = HashSet::new();
                let mut to_traverse = VecDeque::new();

                // Collect object roots from the frame.stack
                for frame in stack.iter() {
                    for item in frame.stack.iter() {
                        if let Value::R(_, v) = *item {
                            to_traverse.push_back(v);
                        }
                    }
                }

                // Traverse to find all live objects
                while !to_traverse.is_empty() {
                    let v = to_traverse.pop_back().unwrap();
                    if !live_refs.contains(&v) {
                        if let Some(value) = heap.get(&v) {
                            live_refs.insert(v);
                            for field in value {
                                if let Value::R(_, v2) = *field {
                                    to_traverse.push_back(v2);
                                }
                            }
                        }
                    }
                }

                // Actually free the memory
                let mut to_remove = vec![];
                for key in heap.keys() {
                    if !live_refs.contains(key) {
                        to_remove.push(key.clone());
                    }
                }
                for key in to_remove {
                    heap.remove(&key);
                }
            }
            Command::SysMemstats => {
                println!("{} heap items", heap.len());
            }
        };
    }
}

fn main() {
    match build(read_lines()) {
        Ok(functions) => interpret(functions),
        Err(err) => println!("Error! {}", err),
    };
}

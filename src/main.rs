use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::io::{stdin, BufReader, BufRead};

#[derive(PartialEq, Debug, Clone)]
struct Function {
    name: String,
    commands: Vec<Command>,
    names: HashMap<String, usize>,
    n_args: i32,
}

#[derive(PartialEq, Debug, Clone)]
enum Label {
    Name(String),
    Offset(usize),
}

#[derive(PartialEq, Debug, Clone)]
enum Command {
    Noop,
    PushInt(i64),
    PushFloat(f64),
    Pop,
    Dup,
    DupN(usize),
    PlusInt,
    MinusInt,
    TimesInt,
    DivideInt,
    ModInt,
    PlusFloat,
    MinusFloat,
    TimesFloat,
    DivideFloat,
    Eq,
    Neq,
    GtInt,
    GteInt,
    LtInt,
    LteInt,
    GtFloat,
    GteFloat,
    LtFloat,
    LteFloat,
    Jump(Label),
    JIf(Label),
    PrintInt,
    PrintFloat,
    MkType,
    New,
    GetRef,
    SetRef,
    Call(Label),
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

fn parse_float(s: &str) -> Result<f64, String> {
    s.to_owned().parse().map_err(|_| format!("can't parse {} as float", s))
}

fn to_label(s: &str) -> Label {
    Label::Name(s.to_owned())
}

fn parse_command(line: String) -> Result<Command, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 1 {
        return parse_err("empty command?");
    }
    match parts[0] {
        "noop" => Ok(Command::Noop),
        "push-int" => Ok(Command::PushInt(try!(parse_int(parts[1])))),
        "push-float" => Ok(Command::PushFloat(try!(parse_float(parts[1])))),
        "pop" => Ok(Command::Pop),
        "dup" => Ok(Command::Dup),
        "dup_n" => Ok(Command::DupN(try!(parse_int(parts[1])) as usize)),
        "plus-int" => Ok(Command::PlusInt),
        "minus-int" => Ok(Command::MinusInt),
        "times-int" => Ok(Command::TimesInt),
        "divide-int" => Ok(Command::DivideInt),
        "mod-int" => Ok(Command::ModInt),
        "plus-float" => Ok(Command::PlusFloat),
        "minus-float" => Ok(Command::MinusFloat),
        "times-float" => Ok(Command::TimesFloat),
        "divide-float" => Ok(Command::DivideFloat),
        "eq" => Ok(Command::Eq),
        "neq" => Ok(Command::Neq),
        "gt-int" => Ok(Command::GtInt),
        "gte-int" => Ok(Command::GteInt),
        "lt-int" => Ok(Command::LtInt),
        "lte-int" => Ok(Command::LteInt),
        "gt-float" => Ok(Command::GtFloat),
        "gte-float" => Ok(Command::GteFloat),
        "lt-float" => Ok(Command::LtFloat),
        "lte-float" => Ok(Command::LteFloat),
        "jump" => Ok(Command::Jump(to_label(parts[1]))),
        "jif" => Ok(Command::JIf(to_label(parts[1]))),
        "print-int" => Ok(Command::PrintInt),
        "print-float" => Ok(Command::PrintFloat),
        "mktype" => Ok(Command::MkType),
        "new" => Ok(Command::New),
        "getref" => Ok(Command::GetRef),
        "setref" => Ok(Command::SetRef),
        "call" => Ok(Command::Call(to_label(parts[1]))),
        "return" => Ok(Command::Return),
        "sysgc" => Ok(Command::SysGC),
        "sysmemstats" => Ok(Command::SysMemstats),
        _ => Err("invalid command: ".to_owned() + line.as_str()),
    }
}

fn parse_fn(line: String) -> Result<Function, String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() != 3 {
        return parse_err("invalid function def");
    }

    Ok(Function {
        name: parts[1].to_owned(),
        commands: Vec::new(),
        names: HashMap::new(),
        n_args: try!(parts[2].parse().map_err(|_| "can't parse int")),
    })
}

fn build_with_string_labels(lines: Vec<String>) -> Result<Vec<Function>, String> {
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

    Ok(functions)
}

fn collect_fn_names(functions: &Vec<Function>) -> HashMap<String, usize> {
    functions.iter()
        .enumerate()
        .map(|(i, f)| (f.name.clone(), i))
        .collect()
}

fn resolve_labels_func(f: &Function, func_names: &HashMap<String, usize>)
                       -> Result<Function, String> {
    let mut commands = vec![];
    for cmd in f.commands.iter() {
        commands.push(try!(resolve_labels_command(cmd.clone(), func_names, &f.names)));
    }

    Ok(Function {
        name: f.name.clone(),
        commands: commands,
        names: f.names.clone(),
        n_args: f.n_args,
    })
}

fn name_to_offset_label(name: &String, names: &HashMap<String, usize>)
                        -> Result<Label, String> {
    match names.get(name) {
        None => Err(format!("label {} not defined", name)),
        Some(offset) => Ok(Label::Offset(*offset)),
    }
}

fn resolve_labels_command(c: Command, func_names: &HashMap<String, usize>, labels: &HashMap<String, usize>)
                          -> Result<Command, String> {
    Ok(match c {
        Command::Jump(Label::Name(name)) =>
            Command::Jump(try!(name_to_offset_label(&name, labels))),
        Command::JIf(Label::Name(name)) =>
            Command::JIf(try!(name_to_offset_label(&name, labels))),
        Command::Call(Label::Name(name)) =>
            Command::Call(try!(name_to_offset_label(&name, func_names))),
        _ => c,
    })
}

fn string_labels_to_offsets(functions: Vec<Function>) -> Result<Vec<Function>, String> {
    let func_names = collect_fn_names(&functions);

    let mut out = vec![];
    for f in functions.iter() {
        out.push(try!(resolve_labels_func(f, &func_names)));
    }
    Ok(out)
}

fn build(lines: Vec<String>) -> Result<Vec<Function>, String> {
    let functions = try!(build_with_string_labels(lines));
    string_labels_to_offsets(functions)
}

#[derive(Debug)]
struct Frame {
    stack: Vec<i64>,
    locals: Vec<i64>,
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

#[inline(always)]
fn f2i(f: f64) -> i64 {
    unsafe {
        return std::mem::transmute(f);
    }
}

#[inline(always)]
fn i2f(i: i64) -> f64 {
    unsafe {
        return std::mem::transmute(i);
    }
}

#[inline(always)]
fn binary_int_op<F>(stack: &mut Vec<i64>, f: F)
    where F: Fn(i64, i64) -> i64 {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(f(left, right));
}

#[inline(always)]
fn binary_float_op<F>(stack: &mut Vec<i64>, f: F)
    where F: Fn(f64, f64) -> f64 {
    let right = i2f(stack.pop().unwrap());
    let left = i2f(stack.pop().unwrap());
    stack.push(f2i(f(left, right)));
}

#[inline(always)]
fn binary_float_to_int_op<F>(stack: &mut Vec<i64>, f: F)
    where F: Fn(f64, f64) -> i64 {
    let right = i2f(stack.pop().unwrap());
    let left = i2f(stack.pop().unwrap());
    stack.push(f(left, right));
}

#[inline(always)]
fn bool_to_i64(b: bool) -> i64 {
    if b { 1 } else { 0 }
}

fn interpret(functions: Vec<Function>) {
    let mut types = vec![];

    let mut stack: Vec<Frame> = vec![];
    let mut frame = new_frame(0, 99999999);

    let mut heap: HashMap<i64, Vec<i64>> = HashMap::new();
    let mut next_obj_id = 1;

    let mut fn_number = 0;
    let mut f = &functions[fn_number];
    let mut pc = 0;
    while pc < f.commands.len() {
        let cmd = &f.commands[pc];
        pc += 1;

        match *cmd {
            Command::Noop => {}
            Command::PushInt(i) => {
                frame.stack.push(i);
            }
            Command::PushFloat(f) => {
                frame.stack.push(f2i(f));
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
            Command::PlusInt => {
                binary_int_op(&mut frame.stack, |a, b| { a + b });
            }
            Command::MinusInt => {
                binary_int_op(&mut frame.stack, |a, b| { a - b });
            }
            Command::TimesInt => {
                binary_int_op(&mut frame.stack, |a, b| { a * b });
            }
            Command::DivideInt => {
                binary_int_op(&mut frame.stack, |a, b| { a / b });
            }
            Command::ModInt => {
                binary_int_op(&mut frame.stack, |a, b| { a % b });
            }
            Command::PlusFloat => {
                binary_float_op(&mut frame.stack, |a, b| { a + b });
            }
            Command::MinusFloat => {
                binary_float_op(&mut frame.stack, |a, b| { a - b });
            }
            Command::TimesFloat => {
                binary_float_op(&mut frame.stack, |a, b| { a * b });
            }
            Command::DivideFloat => {
                binary_float_op(&mut frame.stack, |a, b| { a / b });
            }
            Command::Eq => {
                binary_int_op(&mut frame.stack, |a, b| { bool_to_i64(a == b) });
            }
            Command::Neq => {
                binary_int_op(&mut frame.stack, |a, b| { bool_to_i64(a != b) });
            }
            Command::GtInt => {
                binary_int_op(&mut frame.stack, |a, b| { bool_to_i64(a > b) });
            }
            Command::GteInt => {
                binary_int_op(&mut frame.stack, |a, b| { bool_to_i64(a >= b) });
            }
            Command::LtInt => {
                binary_int_op(&mut frame.stack, |a, b| { bool_to_i64(a < b) });
            }
            Command::LteInt => {
                binary_int_op(&mut frame.stack, |a, b| { bool_to_i64(a <= b) });
            }
            Command::GtFloat => {
                binary_float_to_int_op(&mut frame.stack, |a, b| { bool_to_i64(a > b) });
            }
            Command::GteFloat => {
                binary_float_to_int_op(&mut frame.stack, |a, b| { bool_to_i64(a >= b) });
            }
            Command::LtFloat => {
                binary_float_to_int_op(&mut frame.stack, |a, b| { bool_to_i64(a < b) });
            }
            Command::LteFloat => {
                binary_float_to_int_op(&mut frame.stack, |a, b| { bool_to_i64(a <= b) });
            }
            Command::Jump(Label::Offset(offset)) => pc = offset,
            Command::Jump(Label::Name(ref n)) => {
                panic!("unexpected name {} left in jump command", n);
            }
            Command::JIf(Label::Offset(offset)) => {
                if frame.stack.pop().unwrap() != 0 {
                    pc = offset
                }
            }
            Command::JIf(Label::Name(ref n)) => {
                panic!("unexpected name {} left in jif command", n);
            }
            Command::PrintInt => {
                let top_val = frame.stack.pop().unwrap();
                println!("{}", top_val);
            }
            Command::PrintFloat => {
                let top_val = frame.stack.pop().unwrap();
                println!("{}", i2f(top_val));
            }
            Command::MkType => {
                let name = frame.stack.pop().unwrap();
                let n_fields = frame.stack.pop().unwrap();
                let mut fields = vec![];
                for _ in 0..n_fields {
                    fields.push(frame.stack.pop().unwrap());
                }

                types.push((name, fields));
                frame.stack.push((types.len() - 1) as i64);
            }
            Command::New => {
                let type_id = frame.stack.pop().unwrap();
                let t = &types[type_id as usize];
                let val_id = next_obj_id;
                next_obj_id += 1;

                heap.insert(val_id, t.1.iter().map(|_| { 0 }).collect());
                frame.stack.push(val_id as i64);
            }
            Command::GetRef => {
                let field = frame.stack.pop().unwrap();
                let val_id = frame.stack.pop().unwrap();
                frame.stack.push(heap[&val_id][field as usize].clone());
            }
            Command::SetRef => {
                let value = frame.stack.pop().unwrap();
                let field = frame.stack.pop().unwrap();
                let val_id = frame.stack.pop().unwrap();
                heap.get_mut(&val_id).unwrap()[field as usize] = value;
            }
            Command::Call(Label::Offset(fn_no)) => {
                let called_f = &functions[fn_no];
                let mut next_frame = new_frame(pc, fn_number);

                for _ in 0..called_f.n_args {
                    next_frame.stack.push(frame.stack.pop().unwrap());
                }
                // Fix the argument order:
                next_frame.stack.reverse();

                stack.push(frame);
                frame = next_frame;
                f = called_f;
                fn_number = fn_no;
                pc = 0;
            }
            Command::Call(Label::Name(ref n)) => {
                panic!("unexpected name {} left in call command", n);
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
                        // Shoot, does this need stackmaps?
                        /*
                        if let Value::R(_, v) = *item {
                            to_traverse.push_back(v);
                        }
                         */
                    }
                }

                // Traverse to find all live objects
                while !to_traverse.is_empty() {
                    let v = to_traverse.pop_back().unwrap();
                    if !live_refs.contains(&v) {
                        if let Some(value) = heap.get(&v) {
                            live_refs.insert(v);
                            for field in value {
                                /*
                                if let Value::R(_, v2) = *field {
                                    to_traverse.push_back(v2);
                                }
                                 */
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

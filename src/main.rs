use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::mem;
use im::HashMap;

use sexp::*;
use sexp::Atom::*;

use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};
use std::collections::HashSet;

// Error codes
const ERROR_ARITH_NOT_NUM: i32 = -1;
const ERROR_COMP_NOT_NUM: i32 = -3;
const ERROR_LOGIC_NOT_BOOL: i32 = -5;
const ERROR_IF_NOT_BOOL: i32 = -7;
const ERROR_OVERFLOW: i32 = -9;
const ERROR_EQUAL_COMP_TYPES: i32 = -11;
const ERROR_INFINITE_LOOP: i32 = -13;
const ERROR_LOOPLESS_BREAK: i32 = -15;
const ERROR_UNBOUND_VARIABLE: i32 = -17;
const ERROR_BAD_CAST: i32 = -19;

// Constants for tagging scheme 
const BOOL_TAG: i64 = 0b1;
const NUM_TAG: i64 = 0b0;
const BOOL_TRUE: i64 = 0b11;  // true << 1 | BOOL_TAG
const BOOL_FALSE: i64 = 0b01; // false << 1 | BOOL_TAG

// Tag manipulation functions (64-bit)
fn tag_number(n: i64) -> i64 {
    (n << 1) | NUM_TAG
}

fn tag_boolean(b: bool) -> i64 {
    if b { BOOL_TRUE } else { BOOL_FALSE }
}

fn is_number(v: i64) -> bool {
    v & 1 == NUM_TAG
}

fn is_boolean(v: i64) -> bool {
    v & 1 == BOOL_TAG
}

fn untag_number(v: i64) -> i64 {
    v >> 1
}

fn untag_boolean(v: i64) -> bool {
    v == BOOL_TRUE
}

// A host-side error function the JIT can call directly.
// This is separate from the AOT runtime `snek_error` defined in runtime/start.rs.
#[allow(unused_variables)]
extern "C" fn snek_error_host(errcode: i64) {
    let err_str = match errcode {
        -1 => "invalid argument - ERROR_ARITH_NOT_NUM",
        -3 => "invalid argument - ERROR_COMP_NOT_NUM",
        -5 => "invalid argument - ERROR_LOGIC_NOT_BOOL",
        -7 => "invalid argument - ERROR_IF_NOT_BOOL",
        -9 => "overflow error - ERROR_OVERFLOW",
        -11 => "inequal types for comparison - ERROR_EQUAL_COMP_TYPES",
        -13 => "ERROR_INFINITE_LOOP", //no more
        -15 => "break outside of a loop expression - ERROR_LOOPLESS_BREAK",
        -17 => "unbound variable - ERROR_UNBOUND_VARIABLE",
        -19 => "bad cast",
        _ => unreachable!(),
    };
    eprintln!("an error ocurred: {}", err_str);
    std::process::exit(1);
}

extern "C" fn snek_print_host(val: i64) -> i64 {
    println!("{}", format_value(val));
    val
}


fn parse_input_host(input: &str) -> i64 {
    match input {
        "true" => BOOL_TRUE,
        "false" => BOOL_FALSE,
        s => {
            if let Ok(n) = s.parse::<i64>() {
                tag_number(n)
            } else {
                // default to false on invalid input
                BOOL_FALSE
            }
        }
    }
}

fn parse_add_parenthesis(src: &str) -> Result<Sexp, String> {
    match parse(src) {
        Ok(sexp) => Ok(sexp),
        Err(_) => {
            // Try wrapping multiple top-level forms in a list
            let wrapped = format!("({})", src);
            match parse(&wrapped) {
                Ok(sexp_wrapped) => Ok(sexp_wrapped),
                Err(e2) => Err(format!("Parse error: {}", e2)),
            }
        }
    }
}

impl Instr {
    fn get_value(&self) -> i32 {
        match self {
            Instr::IMov(_, Val::I32(n)) => *n,
            _ => 0, // Default for other instructions
        }
    }
}

/*
/// Compile a source program into a string of x86-64 assembly
fn compile(program: String) -> String {
    let num = program.trim().parse::<i32>().unwrap();
    return format!("mov rax, {}", num);
}*/



#[derive(Clone)]
enum Reg {
    Rax,
    //Rbx,
    Rcx,
    /*Rdx,
    Rsi,*/
    Rdi,
    /*Rsp,
    Rbp,*/
    Rbp,
    Rsp,
}

#[derive(Clone)]
enum Val {
    Reg(Reg),
    I32(i32),
    I64(i64),
}

#[derive(Clone)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    // Legacy stack (rsp-based) helpers - kept for backward compatibility in early code
    MovFromStack(Val, i32),
    MovToStack(Val, i32),
    // Generic base+offset memory ops (e.g., rbp-relative locals/params)
    MovFromMem(Val, Reg, i32), // mov dst, [base+offset]
    MovToMem(Reg, i32, Val),   // mov [base+offset], src
    MovFromHeap(Val, Val), // mov dst, [addrReg]
    MovToHeap(Val, Val),   // mov [addrReg], src
    ICMovEq(Val, Val),  // Conditional move if equal
    ICMovG(Val, Val),   // Conditional move if greater
    ICMovGE(Val, Val),  // Conditional move if greater or equal
    ICMovL(Val, Val),   // Conditional move if less
    ICMovLE(Val, Val),  // Conditional move if less or equal
    ISar(Val, Val),     // Arithmetic right shift for untagging
    IShl(Val, Val),     // Left shift for tagging
    ICmp(Val, Val),     // Compare values
    // setcc into AL (low 8 bits of RAX). Must be preceded by a CMP.
    ISetE,   // sete al
    ISetG,   // setg al (signed)
    ISetGE,  // setge al (signed)
    ISetL,   // setl al (signed)
    ISetLE,  // setle al (signed)
    // control flow jumps + labels
    Jz(String),          // jump if zero
    Jnz(String),         // jump if not zero
    JO(String),          // jump if overflow
    Jmp(String),         // unconditional jump
    Label(String),       // bind a label
    CallError(i32),      // move error code into rdi and call snek_error
    // Calling/ret/stack-frame helpers
    CallLabel(String),   // call <label>
    CallPtr(i64),        // call absolute pointer in rax via mov/call rax
    Ret(),
    SubRsp(i32),
    AddRsp(i32),
    Push(Reg),
    Pop(Reg),
    CallPrint(),         // call snek_print with value in rax

}

#[derive(Clone, Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    Print,
}

#[derive(Clone, Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Clone)]
struct FunDef {
    name: String,
    params: Vec<(String, Type)>,
    return_type: Type,
    body: Expr,
}

#[derive(Clone)]
struct Program {
    defs: Vec<FunDef>,
    body: Expr,
}

#[derive(Clone)]
enum Expr {
    Num(i64),
    Boolean(bool),
    Input,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
    Cast(Type, Box<Expr>),
}

enum ReplEntry {
    Define(String, Box<Expr>),
    DefineFun(FunDef),
    Expr(Box<Expr>),
    Exit(),
}

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Num,
    Bool,
    Any,
    Nothing,
}

fn parse_expr(s: &Sexp) -> Expr {
    let reserved = ["let", "if", "loop", "break", "set!", "block", "add1", "sub1", "neg", "+", "-", "*", "true", "false", "=", ">", ">=", "<", "<=", "input", "isnum", "isbool", "print", "fun", "cast"];

    match s {
        Sexp::Atom(atom) => match atom {
            S(sym) => {
                match sym.as_str() {
                    "true" => Expr::Boolean(true),
                    "false" => Expr::Boolean(false),
                    "input" => Expr::Input,
                    _ => {
                        if reserved.contains(&sym.as_str()) {
                            panic!("Invalid binding: name cannot contain keyword {}", &sym.as_str());
                        }
                        Expr::Id(sym.clone())
                    }
                }
            }

            I(n) => {
                // Accept 64-bit integers but require representable 63-bit payload
                let v: i64 = *n as i64;
                let min = -(1i64 << 62);
                let max = (1i64 << 62) - 1;
                if v < min || v > max { panic!("Invalid: integer literal out of 63-bit range"); }
                Expr::Num(v)
            }
            F(_) => panic!("Invalid: No Floats"),
        },

        Sexp::List(items) => {
            if items.is_empty() {
                panic!("Invalid: Must provide an expression");
            }

            match &items[0] {
                Sexp::Atom(S(op)) => match op.as_str() {

                    "add1" | "sub1" | "isnum" | "isbool" | "print" => {
                        if items.len() != 2 { panic!("Invalid number of arguments for {}", op.as_str()); }
                        let op1 = match op.as_str() {
                            "add1" => Op1::Add1,
                            "sub1" => Op1::Sub1,
                            "isnum" => Op1::IsNum,
                            "isbool" => Op1::IsBool,
                            "print" => Op1::Print,
                            _ => unreachable!(),
                        };
                        Expr::UnOp(op1, Box::new(parse_expr(&items[1])))
                    }

                    "+" | "-" | "*" | "=" | ">" | ">=" | "<" | "<=" => {
                        if items.len() != 3 { panic!("Invalid number of arguments for {}", op.as_str()); }
                        let op2 = match op.as_str() {
                            "+" => Op2::Plus,
                            "-" => Op2::Minus,
                            "*" => Op2::Times,
                            "=" => Op2::Equal,
                            ">" => Op2::Greater,
                            ">=" => Op2::GreaterEqual,
                            "<" => Op2::Less,
                            "<=" => Op2::LessEqual,
                            _ => unreachable!(),
                        };
                        Expr::BinOp(op2, Box::new(parse_expr(&items[1])), Box::new(parse_expr(&items[2])))
                    }

                    "set!" => {
                        if items.len() != 3 { panic!("Invalid number of arguments for set!"); }
                        let var = match &items[1] {
                            Sexp::Atom(S(name)) => {
                                if reserved.contains(&name.as_str()) { panic!("Invalid binding: name cannot contain keyword {}", &name.as_str()); }
                                name.clone()
                            }
                            _ => panic!("Invalid: set! requires an identifier as first argument"),
                        };
                        let rhs = Box::new(parse_expr(&items[2]));
                        Expr::Set(var, rhs)
                    }

                    "block" => {
                        if items.len() < 2 { panic!("Invalid number of arguments for block"); }
                        let mut exprs: Vec<Expr> = Vec::new();
                        for it in &items[1..] {
                            exprs.push(parse_expr(it));
                        }
                        Expr::Block(exprs)
                    }

                    "if" => {
                        if items.len() != 4 { panic!("Invalid number of arguments for if"); }
                        let cond = Box::new(parse_expr(&items[1]));
                        let thn = Box::new(parse_expr(&items[2]));
                        let els = Box::new(parse_expr(&items[3]));
                        Expr::If(cond, thn, els)
                    }

                    "loop" => {
                        if items.len() != 2 { panic!("Invalid number of arguments for loop"); }
                        let body = Box::new(parse_expr(&items[1]));
                        Expr::Loop(body)
                    }

                    "break" => {
                        if items.len() != 2 { panic!("Invalid number of arguments for break"); }
                        let val = Box::new(parse_expr(&items[1]));
                        Expr::Break(val)
                    }

                    "cast" => {
                        if items.len() != 3 { panic!("Invalid number of arguments for cast"); }
                        let cast_type = parse_type(&items[1]);
                        let expr = Box::new(parse_expr(&items[2]));
                        Expr::Cast(cast_type, expr)
                    }

                    "let" => {
                        if items.len() != 3 { panic!("Invalid number of arguments for {}", op.as_str()); }
                        match &items[1] {
                            Sexp::List(bindings) => {
                                let mut binds: Vec<(String, Expr)> = Vec::new();
                                for b in bindings {
                                    match b {
                                        Sexp::List(pair) => {
                                            if pair.len() != 2 { panic!("Invalid: No binding provided"); }
                                            match &pair[0] {
                                                Sexp::Atom(S(id)) => {
                                                    if reserved.contains(&id.as_str()) { panic!("Invalid binding: name cannot contain keyword {}", &id.as_str()); }
                                                    binds.push((id.clone(), parse_expr(&pair[1])));
                                                }
                                                _ => panic!("Invalid binding provided: Binding must be a ((String i32))"),
                                            }
                                        }
                                        _ => panic!("Invalid: Need to have format: ((bind_name value) (bind_name value) ...)"),
                                    }
                                }
                                Expr::Let(binds, Box::new(parse_expr(&items[2])))
                            }
                            _ => panic!("Invalid: No bindings provided"),
                        }
                    }

                    // Function call: (name arg*)
                    _ => {
                        // treat as call if head is not a special form
                        let fname = op.as_str().to_string();
                        if reserved.contains(&fname.as_str()) {
                            panic!("Invalid: No expression provided");
                        }
                        let mut args: Vec<Expr> = Vec::new();
                        for it in &items[1..] {
                            args.push(parse_expr(it));
                        }
                        Expr::Call(fname, args)
                    }
                },

                _ => panic!("Invalid"),
            }
        }
    }
}

fn parse_type(s: &Sexp) -> Type {
    match s {
        Sexp::Atom(S(t)) => match t.as_str() {
            "Num" => Type::Num,
            "Bool" => Type::Bool,
            "Any" => Type::Any,
            "Nothing" => Type::Nothing,
            _ => panic!("Invalid: unknown type {}", t),
        },
        _ => panic!("Invalid: type must be an identifier (Num, Bool, Any, or Nothing)"),
    }
}

fn parse_fun_def(s: &Sexp) -> Option<FunDef> {
    // Supports two formats:
    // New: (fun (name (param : type)*) -> type body)
    // Old (backward compatible): (fun (name param*) body) - defaults to Any
    match s {
        Sexp::List(items) if !items.is_empty() => {
            if let Sexp::Atom(S(op)) = &items[0] {
                if op.as_str() != "fun" { return None; }
                
                // Parse header
                let header = &items[1];
                let (name, params, has_types) = match header {
                    Sexp::List(h) if !h.is_empty() => {
                        let fname = match &h[0] { 
                            Sexp::Atom(S(n)) => n.clone(), 
                            _ => panic!("Invalid: fun name must be identifier") 
                        };
                        
                        // Check if first param is typed (has : in it)
                        let is_typed = if h.len() > 1 {
                            match &h[1] {
                                Sexp::List(param_def) if param_def.len() == 3 => {
                                    matches!(&param_def[1], Sexp::Atom(S(s)) if s == ":")
                                }
                                _ => false
                            }
                        } else {
                            false
                        };
                        
                        let mut ps: Vec<(String, Type)> = Vec::new();
                        
                        if is_typed {
                            // New format: (param : type)*
                            for p in &h[1..] {
                                match p {
                                    Sexp::List(param_def) if param_def.len() == 3 => {
                                        let param_name = match &param_def[0] {
                                            Sexp::Atom(S(n)) => n.clone(),
                                            _ => panic!("Invalid: parameter name must be identifier"),
                                        };
                                        match &param_def[1] {
                                            Sexp::Atom(S(colon)) if colon == ":" => {},
                                            _ => panic!("Invalid: parameter definition must have format (name : type)"),
                                        }
                                        let param_type = parse_type(&param_def[2]);
                                        ps.push((param_name, param_type));
                                    }
                                    _ => panic!("Invalid: parameter must have format (name : type)") 
                                }
                            }
                        } else {
                            // Old format: just param names, default to Any
                            for p in &h[1..] {
                                match p {
                                    Sexp::Atom(S(n)) => ps.push((n.clone(), Type::Any)),
                                    _ => panic!("Invalid: parameter must be identifier in untyped format")
                                }
                            }
                        }
                        
                        (fname, ps, is_typed)
                    }
                    _ => panic!("Invalid: fun header must be a list (name params...)"),
                };
                
                // Parse return type and body based on format
                // Check if we have a -> which indicates typed format
                let has_arrow = items.len() >= 3 && matches!(&items[2], Sexp::Atom(S(arrow)) if arrow == "->");
                
                let (return_type, body) = if has_arrow {
                    // New format: (fun (name (param : type)*) -> type body)
                    if items.len() != 5 { 
                        panic!("Invalid: annotated fun expects format (fun (name (param : type)*) -> type body)"); 
                    }
                    
                    let return_type = parse_type(&items[3]);
                    let body = parse_expr(&items[4]);
                    (return_type, body)
                } else {
                    // Old format: (fun (name param*) body)
                    if items.len() != 3 { 
                        panic!("Invalid: unannotated fun expects format (fun (name param*) body)"); 
                    }
                    let body = parse_expr(&items[2]);
                    (Type::Any, body)
                };
                
                Some(FunDef { name, params, return_type, body })
            } else { None }
        }
        _ => None,
    }
}

fn parse_program(sexp: &Sexp) -> Program {
    // Accept either a single Expr (no defs) or a top-level list where leading items are fun defs and last is body expr
    match sexp {
        // If the top-level is a list whose first element is a function definition, parse leading defs then a single body expr.
        Sexp::List(items) if !items.is_empty() && matches!(parse_fun_def(&items[0]), Some(_)) => {
            let mut defs: Vec<FunDef> = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if let Some(fd) = parse_fun_def(&items[i]) {
                    defs.push(fd);
                    i += 1;
                } else {
                    break;
                }
            }
            if i >= items.len() { panic!("Invalid: program must end with a body expression after definitions"); }
            if items.len() - i != 1 { panic!("Invalid: only one body expression allowed after definitions"); }
            let body = parse_expr(&items[i]);
            Program { defs, body }
        }
        // Otherwise, treat the entire sexp as a single expression program (even if it's a list-form expression like (print ...))
        _ => Program { defs: vec![], body: parse_expr(sexp) },
    }
}

fn parse_repl_entry(s: &Sexp) -> ReplEntry {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), Sexp::Atom(S(var)), expr] if op == "define" => {
                    ReplEntry::Define(var.to_string(), Box::new(parse_expr(expr)))
                }
                // (define (name params...) body)
                [Sexp::Atom(S(op)), header, body] if op == "define" => {
                    // Old REPL format: parameters default to Any, return type defaults to Any
                    let (name, params) = match header {
                        Sexp::List(h) if !h.is_empty() => {
                            let fname = match &h[0] { Sexp::Atom(S(n)) => n.clone(), _ => panic!("Invalid: fun name must be identifier") };
                            let mut ps: Vec<(String, Type)> = Vec::new();
                            for p in &h[1..] {
                                match p { Sexp::Atom(S(n)) => ps.push((n.clone(), Type::Any)), _ => panic!("Invalid: parameter must be identifier") }
                            }
                            (fname, ps)
                        }
                        _ => panic!("Invalid: define function header must be a list (name params...)"),
                    };
                    let fun_body = parse_expr(body);
                    ReplEntry::DefineFun(FunDef { name, params, return_type: Type::Any, body: fun_body })
                }
                // (fun (name params...) body)
                [Sexp::Atom(S(op)), ..] if op == "fun" => {
                    if let Some(fd) = parse_fun_def(s) { ReplEntry::DefineFun(fd) } else { panic!("Invalid fun definition") }
                }
                [Sexp::Atom(S(op))] if (op == "exit" || op == "quit") => ReplEntry::Exit(),
                _ => ReplEntry::Expr(Box::new(parse_expr(s))),
            }
        }
        Sexp::Atom(S(op)) if (op == "exit" || op == "quit") => ReplEntry::Exit(),
        _ => ReplEntry::Expr(Box::new(parse_expr(s)))
    }
}

fn val_to_str(val: &Val) -> String {
    match val {
        Val::Reg(r) => match r {
            Reg::Rax => "rax".to_string(),
            //Reg::Rbx => "rbx".to_string(),
            Reg::Rcx => "rcx".to_string(),
            /*Reg::Rdx => "rdx".to_string(),
            Reg::Rsi => "rsi".to_string(),*/
            Reg::Rdi => "rdi".to_string(),
            Reg::Rbp => "rbp".to_string(),
            Reg::Rsp => "rsp".to_string(),
        }
        Val::I32(n) => n.to_string(),
        Val::I64(n) => n.to_string(),
  }
}

fn reg_to_str(r: &Reg) -> &'static str {
    match r {
        Reg::Rax => "rax",
        Reg::Rcx => "rcx",
        Reg::Rdi => "rdi",
        Reg::Rbp => "rbp",
        Reg::Rsp => "rsp",
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(reg, val) => format!("mov {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::IAdd(reg, val) => format!("add {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::ISub(reg, val) => format!("sub {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::IMul(reg, val) => format!("imul {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::MovFromStack(reg, offset ) => format!("mov {}, [rsp - {}]", val_to_str(reg), offset),
        Instr::MovToStack(reg, offset ) => format!("mov [rsp - {}], {}", offset, val_to_str(reg)),
        Instr::MovFromMem(dst, base, offset) => format!("mov {}, [{} {} {}]", val_to_str(dst), reg_to_str(base), if *offset>=0 {"+"} else {"-"}, offset.abs()),
        Instr::MovToMem(base, offset, src) => format!("mov [{} {} {}], {}", reg_to_str(base), if *offset>=0 {"+"} else {"-"}, offset.abs(), val_to_str(src)),
        Instr::MovFromHeap(dst, addr) => format!("mov {}, [{}]", val_to_str(dst), val_to_str(addr)),
        Instr::MovToHeap(addr, src) => format!("mov [{}], {}", val_to_str(addr), val_to_str(src)),
        Instr::ISar(reg, val) => format!("sar {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::IShl(reg, val) => format!("shl {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::ICmp(reg, val) => format!("cmp {}, {}", val_to_str(reg), val_to_str(val)),
    Instr::ISetE => "sete al".to_string(),
    Instr::ISetG => "setg al".to_string(),
    Instr::ISetGE => "setge al".to_string(),
    Instr::ISetL => "setl al".to_string(),
    Instr::ISetLE => "setle al".to_string(),
        Instr::ICMovEq(dst, src) => format!("cmove {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ICMovG(dst, src) => format!("cmovg {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ICMovGE(dst, src) => format!("cmovge {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ICMovL(dst, src) => format!("cmovl {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ICMovLE(dst, src) => format!("cmovle {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Jz(label) => format!("jz {}", label),
        Instr::Jnz(label) => format!("jnz {}", label),
        Instr::JO(label) => format!("jo {}", label),
        Instr::Jmp(label) => format!("jmp {}", label),
        Instr::Label(label) => format!("{}:", label),
        Instr::CallError(code) => format!("mov rdi, {}\ncall snek_error", code),
        Instr::CallLabel(label) => format!("call {}", label),
        Instr::CallPtr(ptr) => format!("; call *{} (via rax)\nmov rax, {}\ncall rax", ptr, ptr),
        Instr::Ret() => format!("ret"),
        Instr::SubRsp(n) => format!("sub rsp, {}", n),
        Instr::AddRsp(n) => format!("add rsp, {}", n),
        Instr::Push(r) => format!("push {}", reg_to_str(r)),
        Instr::Pop(r) => format!("pop {}", reg_to_str(r)),
        Instr::CallPrint() => format!("push rdi\nmov rdi, rax\ncall snek_print\npop rdi"),
    }
}

fn instr_to_asm(i: &Instr, ops: &mut dynasmrt::x64::Assembler) {
    match i {
        Instr::IMov(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov rax, rax),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov rax, rcx),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rdi)) => dynasm!(ops ; .arch x64 ; mov rax, rdi),
            (Val::Reg(Reg::Rdi), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov rdi, rax),
            (Val::Reg(Reg::Rdi), Val::I64(n)) => dynasm!(ops ; .arch x64 ; mov rdi, QWORD *n),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov rcx, rax),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov rcx, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rdi)) => dynasm!(ops ; .arch x64 ; mov rcx, rdi),
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; mov rax, *n),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; mov rcx, *n),
            (Val::Reg(Reg::Rax), Val::I64(n)) => dynasm!(ops ; .arch x64 ; mov rax, QWORD *n),
            (Val::Reg(Reg::Rcx), Val::I64(n)) => dynasm!(ops ; .arch x64 ; mov rcx, QWORD *n),
            _ => panic!("Unsupported IMov combination"),
        },

        Instr::IAdd(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; add rax, rax),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; add rax, rcx),
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; add rax, *n),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; add rcx, rax),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; add rcx, rcx),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; add rcx, *n),
            _ => panic!("Unsupported IAdd combination"),
        },

        Instr::ISub(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; sub rax, rax),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; sub rax, rcx),
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; sub rax, *n),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; sub rcx, rax),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; sub rcx, rcx),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; sub rcx, *n),
            _ => panic!("Unsupported ISub combination"),
        },

        Instr::IMul(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; imul rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; imul rcx, rax),
            (Val::Reg(Reg::Rax), Val::I32(n)) => {
                // move immediate into RCX then imul RAX, RCX
                dynasm!(ops ; .arch x64 ; mov rcx, *n ; imul rax, rcx);
            }
            (Val::Reg(Reg::Rcx), Val::I32(n)) => {
                // move immediate into RAX then imul RCX, RAX
                dynasm!(ops ; .arch x64 ; mov rax, *n ; imul rcx, rax);
            }
            _ => panic!("Unsupported IMul combination"),
        },

        Instr::MovFromStack(dst, offset) => match dst {
            Val::Reg(Reg::Rax) => dynasm!(ops ; .arch x64 ; mov rax, [rsp - *offset]),
            Val::Reg(Reg::Rcx) => dynasm!(ops ; .arch x64 ; mov rcx, [rsp - *offset]),
            _ => panic!("Unsupported MovFromStack destination"),
        },

        Instr::MovToStack(src, offset) => match src {
            Val::Reg(Reg::Rax) => dynasm!(ops ; .arch x64 ; mov [rsp - *offset], rax),
            Val::Reg(Reg::Rcx) => dynasm!(ops ; .arch x64 ; mov [rsp - *offset], rcx),
            _ => panic!("Unsupported MovToStack source"),
        },
        Instr::MovFromMem(dst, base, offset) => {
            match (dst, base) {
                (Val::Reg(Reg::Rax), Reg::Rbp) => dynasm!(ops ; .arch x64 ; mov rax, [rbp + *offset]),
                (Val::Reg(Reg::Rcx), Reg::Rbp) => dynasm!(ops ; .arch x64 ; mov rcx, [rbp + *offset]),
                (Val::Reg(Reg::Rax), Reg::Rsp) => dynasm!(ops ; .arch x64 ; mov rax, [rsp + *offset]),
                (Val::Reg(Reg::Rcx), Reg::Rsp) => dynasm!(ops ; .arch x64 ; mov rcx, [rsp + *offset]),
                _ => panic!("Unsupported MovFromMem combination"),
            }
        },
        Instr::MovToMem(base, offset, src) => {
            match (base, src) {
                (Reg::Rbp, Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov [rbp + *offset], rax),
                (Reg::Rbp, Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov [rbp + *offset], rcx),
                (Reg::Rsp, Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov [rsp + *offset], rax),
                (Reg::Rsp, Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov [rsp + *offset], rcx),
                _ => panic!("Unsupported MovToMem combination"),
            }
        },
        Instr::MovFromHeap(dst, addr) => match (dst, addr) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov rax, [rax]),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov rax, [rcx]),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov rcx, [rax]),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov rcx, [rcx]),
            _ => panic!("Unsupported MovFromHeap combination"),
        },
        Instr::MovToHeap(addr, src) => match (addr, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov [rax], rax),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov [rax], rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov [rcx], rax),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov [rcx], rcx),
            _ => panic!("Unsupported MovToHeap combination"),
        },
        
        Instr::ISar(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; sar rax, (*n) as i8),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; sar rcx, (*n) as i8),
            _ => panic!("Unsupported ISar combination"),
        },

        Instr::IShl(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; shl rax, (*n) as i8),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; shl rcx, (*n) as i8),
            _ => panic!("Unsupported IShl combination"),
        },

        Instr::ICmp(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; cmp rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; cmp rcx, rax),
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; cmp rax, *n),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; cmp rcx, *n),
            _ => panic!("Unsupported ICmp combination"),
        },
        Instr::ISetE => { dynasm!(ops ; .arch x64 ; sete al); },
        Instr::ISetG => { dynasm!(ops ; .arch x64 ; setg al); },
        Instr::ISetGE => { dynasm!(ops ; .arch x64 ; setge al); },
        Instr::ISetL => { dynasm!(ops ; .arch x64 ; setl al); },
        Instr::ISetLE => { dynasm!(ops ; .arch x64 ; setle al); },
        Instr::ICMovEq(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; cmove rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; cmove rcx, rax),
            _ => panic!("Unsupported ICMovEq combination"),
        },
        Instr::ICMovG(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; cmovg rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; cmovg rcx, rax),
            _ => panic!("Unsupported ICMovG combination"),
        },
        Instr::ICMovGE(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; cmovge rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; cmovge rcx, rax),
            _ => panic!("Unsupported ICMovGE combination"),
        },
        Instr::ICMovL(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; cmovl rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; cmovl rcx, rax),
            _ => panic!("Unsupported ICMovL combination"),
        },
        Instr::ICMovLE(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; cmovle rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; cmovle rcx, rax),
            _ => panic!("Unsupported ICMovLE combination"),
        },
        // The following pseudo instructions are handled in compile_ops (need labels/host fn ptr)
        Instr::Jz(_) | Instr::Jnz(_) | Instr::JO(_) | Instr::Jmp(_) | Instr::Label(_) | Instr::CallError(_) | Instr::CallLabel(_) | Instr::CallPtr(_) | Instr::Ret() | Instr::SubRsp(_) | Instr::AddRsp(_) | Instr::Push(_) | Instr::Pop(_) => {
            panic!("Control-flow pseudo-instruction must be lowered in compile_ops");
        },
        Instr::CallPrint() => {
            let print_fn_ptr: i64 = snek_print_host as i64;
            dynasm!(ops ; .arch x64 ; mov rdi, rax ; mov rax, QWORD print_fn_ptr; call rax);
        },
    }
}

// I dont know why i did this its not necessary 
fn contains_break_shallow(e: &Expr) -> bool {
    match e {
        Expr::Break(_) => true,
        Expr::Loop(_) => false, // don't attribute inner-loop breaks to the outer loop
        Expr::If(cond, thn, els) => {
            if contains_break_shallow(cond) {
                true
            } else {
                contains_break_shallow(thn) && contains_break_shallow(els) //make sure BOTH then/else branches have breaks
            }
        }
        /* ---- KNOWN Implementation decision: ----
        Outlined in test case C19.snek, but consider a loop such as: 
        (loop 
            (let 
                (( x (break 1) )) (+ 1 0)))
        Wasn't sure about how to implement based on spec, but I see two(or kinda 3) possible implementations:
        1: (I chose this one) Loop checks for any break statements inside of the loop, and finds one in a binding,
            so it deems it ok. Then, loop evaluates let and since let is formatted (String, expr), break is 
            allowed to be binded to a variable. Therefore, the expr is evaluated, break is evaluated, and the 
            loop immidiately exits.
        2: Loop checks for any break statements inside of the loop, and finds one in a binding. Then, the
            contains_break_shallow function checks to see if the second expr in the let contains the variable(in
            this case x). When it doesn't, it calls snek_error and returns an error.
            OR
        2.5: Program returns an error when reading the code because we cannot define a variable as a break statement, 
            because functionally in a language that would look like: "let x = break;" or "let x = return 1;".

            ^^THIS OPTION is also important because it questions wether we should allow break <expr>s to be passed
            in other exprs such as in UnOp and BinOp, without erroring(assuming they are inside a loop)(test case C20.snek).
        
        Waste of time to read this comment but I wanted to note this down.
        -------------------------------------------- */
        Expr::Let(binds, body) => {
            for (_, ex) in binds {
                if contains_break_shallow(ex) { return true; }
            }
            contains_break_shallow(body)
        }
        Expr::UnOp(_, sub) => contains_break_shallow(sub),
        Expr::BinOp(_, l, r) => contains_break_shallow(l) || contains_break_shallow(r),
        Expr::Set(_, rhs) => contains_break_shallow(rhs),
        Expr::Block(es) => es.iter().any(|e| contains_break_shallow(e)),
        Expr::Cast(_, expr) => contains_break_shallow(expr),
        _ => false,
    }
}

// stack_buff: next local slot offset in bytes (positive multiple of 8). Locals are addressed as [rbp - stack_buff].
// env: maps variable name to rbp-relative offset in bytes (can be positive for params or negative for locals)
fn compile_to_instrs_inner(e: &Expr, stack_buff: i32, env: &im::HashMap<String, i32>, define_env: &im::HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>, lbl: &mut i32, break_label: Option<&String>, fun_labels: &std::collections::HashMap<String, (usize, String)>, fun_ptrs: &std::collections::HashMap<String, (usize, i64)>) -> Vec<Instr> {
    match e {
        Expr::Num(n) => vec![Instr::IMov(Val::Reg(Reg::Rax), Val::I64(tag_number(*n)))],
        Expr::Boolean(b) => vec![Instr::IMov(Val::Reg(Reg::Rax), Val::I64(tag_boolean(*b)))],
        Expr::Input => vec![Instr::IMov(Val::Reg(Reg::Rax), Val::Reg(Reg::Rdi))],
        Expr::Id(s) => {
            match env.get(s) {
                Some(off) => vec![Instr::MovFromMem(Val::Reg(Reg::Rax), Reg::Rbp, *off)],
                _ => {
                    if let Some(ptr) = define_ptr_env.get(s) {
                        // load through pointer
                        vec![
                            Instr::IMov(Val::Reg(Reg::Rax), Val::I64(*ptr as i64)),
                            Instr::MovFromHeap(Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)),
                        ]
                    } else {
                        match define_env.get(s) {
                            Some(value) => vec![Instr::IMov(Val::Reg(Reg::Rax), Val::I64(*value))],
                            _ => panic!("Unbound variable indentifier {}", s),
                            //_ => vec![Instr::CallError(ERROR_UNBOUND_VARIABLE)],
                        }
                    }
                }
            }
        },
        Expr::Let(bindings, body_expr) => {
            let mut env2 = env.clone();
            let mut instr_vec:Vec<Instr> = Vec::new();
            let mut dup_vec:Vec<String> = Vec::new();
            let mut i = 0;
            for (name, bind_expr) in bindings {
                if dup_vec.contains(name) {
                    println!("ERROR | Duplicate binding: {}", name);
                    break;
                }
                instr_vec.extend(compile_to_instrs_inner(bind_expr, stack_buff + i, &env2, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));
                // store into local slot [rbp - (stack_buff + i)]
                let off = -(stack_buff + i);
                instr_vec.push(Instr::MovToMem(Reg::Rbp, off, Val::Reg(Reg::Rax)));
                env2.insert(name.clone(), off);
                dup_vec.push(name.clone());
                i += 8;
            }
            instr_vec.extend(compile_to_instrs_inner(body_expr, stack_buff + i, &env2, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));
            instr_vec
            
        },
        Expr::Loop(body) => {
            /*
            if !contains_break_shallow(body) {
                return vec![Instr::CallError(ERROR_INFINITE_LOOP)];
            }*/
            let mut instr_vec: Vec<Instr> = Vec::new();
            let start_l = format!("loop_start_{}", *lbl);
            let end_l   = format!("loop_end_{}", *lbl);
            *lbl += 1;
            instr_vec.push(Instr::Label(start_l.clone()));
            instr_vec.extend(compile_to_instrs_inner(body, stack_buff, env, define_env, define_ptr_env, lbl, Some(&end_l), fun_labels, fun_ptrs));
            instr_vec.push(Instr::Jmp(start_l.clone()));
            instr_vec.push(Instr::Label(end_l.clone()));
            instr_vec
        }
        Expr::Break(val) => {
            match break_label {
                Some(label_name) => {
                    let mut instr_vec: Vec<Instr> = compile_to_instrs_inner(val, stack_buff, env, define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs);
                    instr_vec.push(Instr::Jmp(label_name.clone()));
                    instr_vec
                }
                None => {
                    // break outside of any loop: error with LOOPLESS_BREAK
                    vec![Instr::CallError(ERROR_LOOPLESS_BREAK)]
                }
            }
        }
        Expr::Set(name, rhs) => {
            let mut instr_vec: Vec<Instr> = compile_to_instrs_inner(rhs, stack_buff, env, define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs);
            match env.get(name) {
                Some(off) => {
                    instr_vec.push(Instr::MovToMem(Reg::Rbp, *off, Val::Reg(Reg::Rax)));
                }
                None => {
                    if let Some(ptr) = define_ptr_env.get(name) {
                        // store into heap location
                        instr_vec.push(Instr::IMov(Val::Reg(Reg::Rcx), Val::I64(*ptr as i64)));
                        instr_vec.push(Instr::MovToHeap(Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)));
                    } else {
                        instr_vec.push(Instr::CallError(ERROR_UNBOUND_VARIABLE));
                    }
                }
            }
            instr_vec
        }
        Expr::Block(exprs) => {
            let mut instr_vec: Vec<Instr> = Vec::new();
            for ex in exprs {
                instr_vec.extend(compile_to_instrs_inner(ex, stack_buff, env, define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));
            }
            instr_vec
        }
        Expr::If(cond, thn, els) => {
            let mut instr_vec: Vec<Instr> = Vec::new();
            instr_vec.extend(compile_to_instrs_inner(cond, stack_buff, env, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));

            let then_l = format!("if_then_{}", *lbl);
            let else_l = format!("if_else_{}", *lbl);
            let end_l  = format!("if_end_{}", *lbl);
            let err_l  = format!("if_err_{}", *lbl);
            *lbl += 1;

            // branch based on condition
            instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
            instr_vec.push(Instr::Jz(then_l.clone()));
            instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
            instr_vec.push(Instr::Jz(else_l.clone()));
            instr_vec.push(Instr::Jmp(err_l.clone()));

            // then
            instr_vec.push(Instr::Label(then_l.clone()));
            instr_vec.extend(compile_to_instrs_inner(thn, stack_buff, env, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));
            instr_vec.push(Instr::Jmp(end_l.clone()));

            // else
            instr_vec.push(Instr::Label(else_l.clone()));
            instr_vec.extend(compile_to_instrs_inner(els, stack_buff, env, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));
            instr_vec.push(Instr::Jmp(end_l.clone()));

            // error
            instr_vec.push(Instr::Label(err_l.clone()));
            instr_vec.push(Instr::CallError(ERROR_IF_NOT_BOOL));

            instr_vec.push(Instr::Label(end_l.clone()));

            instr_vec
        }
        Expr::UnOp(op, subexpr) => {
            let mut instr_vec: Vec<Instr> = compile_to_instrs_inner(subexpr, stack_buff, env, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs);
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    // numeric operand required
                    let err_l = format!("err_unop_{}", *lbl);
                    let ok_l = format!("ok_unop_{}", *lbl); *lbl += 1;
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::Jmp(ok_l.clone()));
                    instr_vec.push(Instr::Label(err_l));
                    instr_vec.push(Instr::CallError(ERROR_ARITH_NOT_NUM));
                    instr_vec.push(Instr::Label(ok_l));

                    match op {
                        Op1::Add1 => {
                            let of_l = format!("of_unop_{}", *lbl);
                            let cont_l = format!("cont_unop_{}", *lbl); *lbl += 1;
                            instr_vec.push(Instr::IAdd(Val::Reg(Reg::Rax), Val::I32(2))); // add 2 since values are shifted
                            instr_vec.push(Instr::JO(of_l.clone()));
                            instr_vec.push(Instr::Jmp(cont_l.clone()));
                            instr_vec.push(Instr::Label(of_l));
                            instr_vec.push(Instr::CallError(ERROR_OVERFLOW));
                            instr_vec.push(Instr::Label(cont_l));
                        },
                        Op1::Sub1 => {
                            let of_l = format!("of_unop_{}", *lbl);
                            let cont_l = format!("cont_unop_{}", *lbl); *lbl += 1;
                            instr_vec.push(Instr::ISub(Val::Reg(Reg::Rax), Val::I32(2))); // sub 2 since values are shifted
                            instr_vec.push(Instr::JO(of_l.clone()));
                            instr_vec.push(Instr::Jmp(cont_l.clone()));
                            instr_vec.push(Instr::Label(of_l));
                            instr_vec.push(Instr::CallError(ERROR_OVERFLOW));
                            instr_vec.push(Instr::Label(cont_l));
                        },
                        _ => unreachable!(),
                    }
                }
                Op1::IsNum => {
                    // If value equals true or false -> boolean -> result false; else number -> result true
                    let set_false = format!("isnum_set_false_{}", *lbl);
                    let done = format!("isnum_done_{}", *lbl); *lbl += 1;
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(set_false.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(set_false.clone()));
                    // number
                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jmp(done.clone()));
                    // boolean -> false
                    instr_vec.push(Instr::Label(set_false));
                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Label(done));
                }
                Op1::IsBool => {
                    // If value equals true or false -> boolean -> result true; else number -> result false
                    let set_true = format!("isbool_set_true_{}", *lbl);
                    let done = format!("isbool_done_{}", *lbl); *lbl += 1;
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(set_true.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(set_true.clone()));
                    // number -> false
                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jmp(done.clone()));
                    // boolean -> true
                    instr_vec.push(Instr::Label(set_true));
                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Label(done));
                }
                Op1::Print => {
                    // value already in rax
                    instr_vec.push(Instr::CallPrint());
                }
            }
            instr_vec
        }
        Expr::BinOp(op,left ,right ) => {
            let mut instr_vec: Vec<Instr> = compile_to_instrs_inner(left, stack_buff, env, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs);
            // save left into temp local [rbp - stack_buff]
            let temp_off = -stack_buff;
            instr_vec.push(Instr::MovToMem(Reg::Rbp, temp_off, Val::Reg(Reg::Rax)));
            instr_vec.extend(compile_to_instrs_inner(right, stack_buff + 8, env, &define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));
            // load left into rcx
            instr_vec.push(Instr::MovFromMem(Val::Reg(Reg::Rcx), Reg::Rbp, temp_off));

            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    let err_l = format!("err_ar_{}", *lbl);
                    let ok_l = format!("ok_ar_{}", *lbl); *lbl += 1;
                    // right must not be boolean
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    // left must not be boolean
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::Jmp(ok_l.clone()));
                    instr_vec.push(Instr::Label(err_l));
                    instr_vec.push(Instr::CallError(ERROR_ARITH_NOT_NUM));
                    instr_vec.push(Instr::Label(ok_l));
                }
                Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                    let err_l = format!("err_cmp_{}", *lbl);
                    let ok_l = format!("ok_cmp_{}", *lbl); *lbl += 1;
                    // right must not be boolean
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    // left must not be boolean
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::Jmp(ok_l.clone()));
                    instr_vec.push(Instr::Label(err_l));
                    instr_vec.push(Instr::CallError(ERROR_COMP_NOT_NUM));
                    instr_vec.push(Instr::Label(ok_l));
                }
                Op2::Equal => {
                    let right_is_bool = format!("eq_right_bool_{}", *lbl);
                    let types_ok = format!("eq_types_ok_{}", *lbl);
                    let err_l = format!("eq_err_{}", *lbl); *lbl += 1;
                    // check if right is boolean; if so, ensure left is also boolean
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(right_is_bool.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(right_is_bool.clone()));
                    // right is number; left must be number 
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::Jmp(types_ok.clone()));
                    // right is boolean; left must be boolean
                    instr_vec.push(Instr::Label(right_is_bool));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(types_ok.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(types_ok.clone()));
                    instr_vec.push(Instr::Jmp(err_l.clone()));
                    // type mismatch error
                    instr_vec.push(Instr::Label(err_l.clone()));
                    instr_vec.push(Instr::CallError(ERROR_EQUAL_COMP_TYPES));
                    // types ok
                    instr_vec.push(Instr::Label(types_ok));
                }
            }
            match op {
                Op2::Plus => {
                    let of_l = format!("of_ar_{}", *lbl);
                    let cont_l = format!("cont_ar_{}", *lbl); *lbl += 1;
                    instr_vec.push(Instr::IAdd(Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)));
                    instr_vec.push(Instr::JO(of_l.clone()));
                    instr_vec.push(Instr::Jmp(cont_l.clone()));
                    instr_vec.push(Instr::Label(of_l));
                    instr_vec.push(Instr::CallError(ERROR_OVERFLOW));
                    instr_vec.push(Instr::Label(cont_l));
                },
                Op2::Minus => {
                    let of_l = format!("of_ar_{}", *lbl);
                    let cont_l = format!("cont_ar_{}", *lbl); *lbl += 1;
                    instr_vec.push(Instr::ISub(Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)));
                    instr_vec.push(Instr::JO(of_l.clone()));
                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)));
                    instr_vec.push(Instr::Jmp(cont_l.clone()));
                    instr_vec.push(Instr::Label(of_l));
                    instr_vec.push(Instr::CallError(ERROR_OVERFLOW));
                    instr_vec.push(Instr::Label(cont_l));
                }
                Op2::Times => {
                    // shift right
                    instr_vec.push(Instr::ISar(Val::Reg(Reg::Rax), Val::I32(1)));
                    instr_vec.push(Instr::ISar(Val::Reg(Reg::Rcx), Val::I32(1)));

                    instr_vec.push(Instr::IMul(Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)));
                    // shift back left
                    let of_l = format!("of_ar_{}", *lbl);
                    let cont_l = format!("cont_ar_{}", *lbl); *lbl += 1;
                    instr_vec.push(Instr::IShl(Val::Reg(Reg::Rax), Val::I32(1)));
                    instr_vec.push(Instr::JO(of_l.clone()));
                    instr_vec.push(Instr::Jmp(cont_l.clone()));
                    instr_vec.push(Instr::Label(of_l));
                    instr_vec.push(Instr::CallError(ERROR_OVERFLOW));
                    instr_vec.push(Instr::Label(cont_l));
                },
                Op2::Equal | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                    match op {
                        Op2::Equal => {
                            // cmp right (rax) vs left (rcx); sete al; tag to boolean (2*al+1)
                            instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)));
                            instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::I32(0)));
                            instr_vec.push(Instr::ISetE);
                            instr_vec.push(Instr::IShl(Val::Reg(Reg::Rax), Val::I32(1)));
                            instr_vec.push(Instr::IAdd(Val::Reg(Reg::Rax), Val::I32(1)));
                        },
                        _ => {
                            // compare left (rcx) vs right (rax); setcc al; tag to boolean (2*al+1)
                            instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)));
                            instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::I32(0)));
                            match op {
                                Op2::Greater => instr_vec.push(Instr::ISetG),
                                Op2::GreaterEqual => instr_vec.push(Instr::ISetGE),
                                Op2::Less => instr_vec.push(Instr::ISetL),
                                Op2::LessEqual => instr_vec.push(Instr::ISetLE),
                                _ => unreachable!(),
                            }
                            instr_vec.push(Instr::IShl(Val::Reg(Reg::Rax), Val::I32(1)));
                            instr_vec.push(Instr::IAdd(Val::Reg(Reg::Rax), Val::I32(1)));
                        }
                    }
                },
                
            }
            instr_vec
        }
        Expr::Cast(cast_type, expr) => {
            let mut instr_vec: Vec<Instr> = compile_to_instrs_inner(expr, stack_buff, env, define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs);
            
            match cast_type {
                Type::Num => {
                    // If value is a number, keep it; if boolean, error
                    let ok_l = format!("cast_num_ok_{}", *lbl);
                    let err_l = format!("cast_num_err_{}", *lbl);
                    *lbl += 1;
                    
                    // Check if it's a boolean (tag bit is 1)
                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(err_l.clone()));
                    instr_vec.push(Instr::Jmp(ok_l.clone()));

                    instr_vec.push(Instr::Label(err_l));
                    instr_vec.push(Instr::CallError(ERROR_BAD_CAST));
                    instr_vec.push(Instr::Label(ok_l));
                }
                Type::Bool => {
                    // If value is a boolean, keep it; if number, error
                    let ok_l = format!("cast_bool_ok_{}", *lbl);
                    let err_l = format!("cast_bool_err_{}", *lbl);
                    *lbl += 1;
                    

                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_TRUE as i32)));
                    instr_vec.push(Instr::Jz(ok_l.clone()));
                    instr_vec.push(Instr::ICmp(Val::Reg(Reg::Rcx), Val::I32(BOOL_FALSE as i32)));
                    instr_vec.push(Instr::Jz(ok_l.clone()));

                    instr_vec.push(Instr::Label(err_l));
                    instr_vec.push(Instr::CallError(ERROR_BAD_CAST));
                    instr_vec.push(Instr::Label(ok_l));
                }
                Type::Nothing => {
                    // If <type> is Nothing, error with a string containing "bad cast"
                    instr_vec.push(Instr::CallError(ERROR_BAD_CAST));
                }
                Type::Any => {
                    // If <type> is Any, evaluate to v
                }
            }
            
            instr_vec
        }
        Expr::Call(name, args) => {
            // Arity check
            let mut arity_opt: Option<usize> = None;
            if let Some((arity, _label)) = fun_labels.get(name) { arity_opt = Some(*arity); }
            if let Some((arity, _ptr)) = fun_ptrs.get(name) { arity_opt = Some(*arity); }
            match arity_opt {
                Some(arity) => {
                    if arity != args.len() { panic!("Arity mismatch calling {}: expected {}, got {}", name, arity, args.len()); }
                }
                None => { panic!("Undefined function {}", name); }
            }
            let mut instr_vec: Vec<Instr> = Vec::new();
            // push args right-to-left
            for arg in args.iter().rev() {
                instr_vec.extend(compile_to_instrs_inner(arg, stack_buff, env, define_env, define_ptr_env, lbl, break_label, fun_labels, fun_ptrs));
                // Use push rax to avoid any rsp-immediate encoding quirks
                instr_vec.push(Instr::Push(Reg::Rax));
            }
            // call
            if let Some((_a, lbls)) = fun_labels.get(name) {
                instr_vec.push(Instr::CallLabel(lbls.clone()));
            } else if let Some((_a, ptr)) = fun_ptrs.get(name) {
                instr_vec.push(Instr::CallPtr(*ptr));
            } else {
                panic!("Undefined function {}", name);
            }
            if args.len() > 0 { instr_vec.push(Instr::AddRsp((args.len() as i32)*8)); }
            instr_vec
        }
    }
}

fn compile_to_instrs(e: &Expr, stack_buff: i32, env: &im::HashMap<String, i32>, define_env: &im::HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>, lbl: &mut i32, fun_labels: &std::collections::HashMap<String, (usize, String)>, fun_ptrs: &std::collections::HashMap<String, (usize, i64)>) -> Vec<Instr> {
    compile_to_instrs_inner(e, stack_buff, env, define_env, define_ptr_env, lbl, None, fun_labels, fun_ptrs)
}

// Stack usage analysis: estimate number of local slots needed (in 8-byte words)
fn max_stack_usage(e: &Expr) -> i32 {
    match e {
        Expr::Num(_) | Expr::Boolean(_) | Expr::Input | Expr::Id(_) => 0,
        Expr::UnOp(_, sub) => max_stack_usage(sub),
        Expr::Set(_, rhs) | Expr::Break(rhs) | Expr::Loop(rhs) => max_stack_usage(rhs),
        Expr::Cast(_, expr) => max_stack_usage(expr),
        Expr::Block(es) => es.iter().map(max_stack_usage).max().unwrap_or(0),
        Expr::If(c,t,e2) => max_stack_usage(c).max(max_stack_usage(t)).max(max_stack_usage(e2)),
        Expr::BinOp(_, l, r) => {
            // need 1 temp slot to save left while evaluating right
            let left_need = max_stack_usage(l) + 1;
            let right_need = max_stack_usage(r);
            left_need.max(right_need)
        }
        Expr::Let(binds, body) => {
            let binds_need: i32 = binds.iter().map(|(_, ex)| max_stack_usage(ex)).max().unwrap_or(0);
            let bind_count = binds.len() as i32;
            let body_need = max_stack_usage(body);
            binds_need.max(bind_count + body_need)
        }
        Expr::Call(_, args) => {
            // arguments are pushed on caller stack (rsp), not locals; only need temps inside args
            args.iter().map(max_stack_usage).max().unwrap_or(0)
        }
    }
}

struct FunSig { arity: usize, label: String }

fn build_fun_sigs(defs: &Vec<FunDef>) -> std::collections::HashMap<String, FunSig> {
    let mut map = std::collections::HashMap::new();
    for d in defs {
        if map.contains_key(&d.name) { panic!("Duplicate function {}", d.name); }
        let label = format!("fun_{}", d.name);
        map.insert(d.name.clone(), FunSig { arity: d.params.len(), label });
    }
    map
}

fn compile_function_instrs(def: &FunDef, fun_sigs: &std::collections::HashMap<String, FunSig>, lbl: &mut i32, define_env: &HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    let label = fun_sigs.get(&def.name).unwrap().label.clone();
    instrs.push(Instr::Label(label));
    // prologue
    instrs.push(Instr::Push(Reg::Rbp));
    // mov rbp, rsp
    instrs.push(Instr::IMov(Val::Reg(Reg::Rbp), Val::Reg(Reg::Rsp)));
    // frame size
    let frame_slots = max_stack_usage(&def.body) + (def.params.len() as i32 * 0); // params are not locals
    let frame_size = if frame_slots > 0 { frame_slots * 8 } else { 0 };
    if frame_size > 0 { instrs.push(Instr::SubRsp(frame_size)); }

    // Build var env: params at [rbp + 24 + 8*i]
    let mut env: im::HashMap<String, i32> = im::HashMap::new();
    for (i, (param_name, _param_type)) in def.params.iter().enumerate() {
        let off = 16 + (i as i32)*8;
        env.insert(param_name.clone(), off);
    }
    let fun_map: std::collections::HashMap<String, (usize, String)> = fun_sigs.iter().map(|(k,v)| (k.clone(), (v.arity, v.label.clone()))).collect();
    let empty_ptrs: std::collections::HashMap<String, (usize, i64)> = std::collections::HashMap::new();
    let body_instrs = compile_to_instrs(&def.body, 8, &env, define_env, define_ptr_env, lbl, &fun_map, &empty_ptrs);
    instrs.extend(body_instrs);

    // epilogue
    if frame_size > 0 { instrs.push(Instr::AddRsp(frame_size)); }
    instrs.push(Instr::Pop(Reg::Rbp));
    instrs.push(Instr::Ret());
    instrs
}

fn compile_entry_instrs(body: &Expr, fun_sigs: &std::collections::HashMap<String, FunSig>, lbl: &mut i32, define_env: &HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    instrs.push(Instr::Label("our_code_starts_here".to_string()));
    // prologue
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::IMov(Val::Reg(Reg::Rbp), Val::Reg(Reg::Rsp)));
    let frame_slots = max_stack_usage(body);
    let frame_size = if frame_slots > 0 { frame_slots * 8 } else { 0 };
    if frame_size > 0 { instrs.push(Instr::SubRsp(frame_size)); }

    let env: im::HashMap<String, i32> = im::HashMap::new();
    let fun_map: std::collections::HashMap<String, (usize, String)> = fun_sigs.iter().map(|(k,v)| (k.clone(), (v.arity, v.label.clone()))).collect();
    let empty_ptrs: std::collections::HashMap<String, (usize, i64)> = std::collections::HashMap::new();
    let body_instrs = compile_to_instrs(body, 8, &env, define_env, define_ptr_env, lbl, &fun_map, &empty_ptrs);
    instrs.extend(body_instrs);

    if frame_size > 0 { instrs.push(Instr::AddRsp(frame_size)); }
    instrs.push(Instr::Pop(Reg::Rbp));
    instrs.push(Instr::Ret());
    instrs
}

fn compile_program_to_string(p: &Program) -> String {
    let fun_sigs = build_fun_sigs(&p.defs);
    let mut all: Vec<Instr> = Vec::new();
    let mut lbl = 0;
    for d in &p.defs { all.extend(compile_function_instrs(d, &fun_sigs, &mut lbl, &HashMap::new(), &std::collections::HashMap::new())); }
    all.extend(compile_entry_instrs(&p.body, &fun_sigs, &mut lbl, &HashMap::new(), &std::collections::HashMap::new()));
    let mut res = String::new();
    for instr in all {
        res.push_str(&instr_to_str(&instr));
        res.push_str("\n");
    }
    res
}


/* 
fn compile_expr(e: &Expr) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", *n),
        Expr::Add1(subexpr) => compile_expr(subexpr) + "\nadd rax, 1",
        Expr::Sub1(subexpr) => compile_expr(subexpr) + "\nsub rax, 1",
        Expr::Neg(subexpr) => compile_expr(subexpr) + "\nneg rax",
    }
}
*/

fn compile_program_ops(p: &Program, ops : &mut dynasmrt::x64::Assembler, input_value: Option<i64>, define_env: &HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>, _fun_ptrs: &std::collections::HashMap<String, (usize, i64)>) {
    let fun_sigs = build_fun_sigs(&p.defs);
    let mut all_instrs: Vec<Instr> = Vec::new();
    // Emit entry first so our start pointer jumps directly into entry code
    let mut lbl = 0;
    // Entry may call previously jitted funs via fun_ptrs
    all_instrs.extend(compile_entry_instrs(&p.body, &fun_sigs, &mut lbl, define_env, define_ptr_env));
    // If program carries defs (file-based), compile them now; for REPL exprs, defs is empty and calls go via pointers
    for d in &p.defs { all_instrs.extend(compile_function_instrs(d, &fun_sigs, &mut lbl, define_env, define_ptr_env)); }

    // Set input value in RDI for JIT execution paths
    let to_load: i64 = input_value.unwrap_or_else(|| tag_boolean(false));
    dynasm!(ops ; .arch x64 ; mov rdi, QWORD to_load);

    // allocate dynamic labels
    use std::collections::HashMap as StdHashMap;
    let mut label_map: StdHashMap<String, dynasmrt::DynamicLabel> = StdHashMap::new();
    for ins in &all_instrs {
        if let Instr::Label(name) = ins {
            label_map.entry(name.clone()).or_insert_with(|| ops.new_dynamic_label());
        }
    }

    let err_fn_ptr: i64 = snek_error_host as i64;
    let print_fn_ptr: i64 = snek_print_host as i64;
    for instr in all_instrs {
        match instr {
            Instr::IMov(Val::Reg(Reg::Rbp), Val::Reg(Reg::Rsp)) => { dynasm!(ops ; .arch x64 ; mov rbp, rsp); }
            Instr::Jz(label) => {
                let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label());
                dynasm!(ops ; .arch x64 ; jz =>*dl);
            }
            Instr::Jnz(label) => {
                let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label());
                dynasm!(ops ; .arch x64 ; jnz =>*dl);
            }
            Instr::Jmp(label) => {
                let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label());
                dynasm!(ops ; .arch x64 ; jmp =>*dl);
            }
            Instr::JO(label) => {
                let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label());
                dynasm!(ops ; .arch x64 ; jo =>*dl);
            }
            Instr::Label(label) => {
                if let Some(dl) = label_map.get(&label) {
                    dynasm!(ops ; .arch x64 ; =>*dl);
                } else {
                    let dl = ops.new_dynamic_label();
                    label_map.insert(label.clone(), dl);
                    let dl_ref = label_map.get(&label).unwrap();
                    dynasm!(ops ; .arch x64 ; =>*dl_ref);
                }
            }
            Instr::CallError(code) => {
                let code_i64 = code as i64;
                dynasm!(ops ; .arch x64 ; mov rdi, QWORD code_i64);
                dynasm!(ops ; .arch x64 ; mov rax, QWORD err_fn_ptr ; call rax);
            }
            Instr::CallPrint() => {
                // Preserve RDI across host call (caller-saved)
                dynasm!(ops ; .arch x64 ; push rdi);
                dynasm!(ops ; .arch x64 ; mov rdi, rax);
                dynasm!(ops ; .arch x64 ; mov rax, QWORD print_fn_ptr ; call rax);
                dynasm!(ops ; .arch x64 ; pop rdi);
            }
            Instr::CallLabel(label) => {
                let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label());
                dynasm!(ops ; .arch x64 ; call =>*dl);
            }
            Instr::CallPtr(ptr) => {
                let p = ptr;
                dynasm!(ops ; .arch x64 ; mov rax, QWORD p ; call rax);
            }
            Instr::Ret() => { dynasm!(ops ; .arch x64 ; ret); }
            Instr::SubRsp(n) => { let imm = n; let imm_ref = &imm; dynasm!(ops ; .arch x64 ; sub rsp, *imm_ref); }
            Instr::AddRsp(n) => { let imm = n; let imm_ref = &imm; dynasm!(ops ; .arch x64 ; add rsp, *imm_ref); }
            Instr::Push(Reg::Rbp) => { dynasm!(ops ; .arch x64 ; push rbp); }
            Instr::Push(Reg::Rax) => { dynasm!(ops ; .arch x64 ; push rax); }
            Instr::Push(Reg::Rcx) => { dynasm!(ops ; .arch x64 ; push rcx); }
            Instr::Push(Reg::Rdi) => { dynasm!(ops ; .arch x64 ; push rdi); }
            Instr::Push(_) => panic!("Unsupported push register"),
            Instr::Pop(Reg::Rbp) => { dynasm!(ops ; .arch x64 ; pop rbp); }
            Instr::Pop(Reg::Rax) => { dynasm!(ops ; .arch x64 ; pop rax); }
            Instr::Pop(Reg::Rcx) => { dynasm!(ops ; .arch x64 ; pop rcx); }
            Instr::Pop(Reg::Rdi) => { dynasm!(ops ; .arch x64 ; pop rdi); }
            Instr::Pop(_) => panic!("Unsupported pop register"),
            other => {
                instr_to_asm(&other, ops);
            }
        }
    }
}

fn eval_program(source: &str, input_value: Option<i64>) -> Result<String, String> {
    let sexp = parse_add_parenthesis(source)?;
    let prog = parse_program(&sexp);
    match eval_jit_program(&prog, input_value) {
        Ok(raw) => Ok(format_value(raw)),
        Err(e) => Err(e),
    }
}


fn format_value(val: i64) -> String {
    if is_number(val) {
        untag_number(val).to_string()
    } else if is_boolean(val) {
        if untag_boolean(val) {
            "true".to_string()
        } else {
            "false".to_string()
        }
    } else {
        format!("Unknown value: {}", val)
    }
}

fn eval_jit_program(prog: &Program, input_value: Option<i64>) -> Result<i64, String> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();

    let empty_ptrs: std::collections::HashMap<String, (usize, i64)> = std::collections::HashMap::new();
    compile_program_ops(prog, &mut ops, input_value, &HashMap::new(), &std::collections::HashMap::new(), &empty_ptrs);

    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
    let result = jitted_fn();
    Ok(result)
}

// Compatibility shim for REPL paths (we ignore define env semantics here)
fn eval_jit_with_define_ptr(expr: &Expr, _define_env: &HashMap<String, i64>, _define_ptr_env: &std::collections::HashMap<String, i64>, input_value: Option<i64>) -> Result<i64, String> {
    let prog = Program { defs: vec![], body: expr.clone() };
    eval_jit_program(&prog, input_value)
}

fn eval_jit_program_with_env(prog: &Program, input_value: Option<i64>, define_env: &HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>) -> Result<i64, String> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();
    let empty_ptrs: std::collections::HashMap<String, (usize, i64)> = std::collections::HashMap::new();
    compile_program_ops(prog, &mut ops, input_value, define_env, define_ptr_env, &empty_ptrs);
    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
    let result = jitted_fn();
    Ok(result)
}

// Lower a list of instructions into an assembler, handling labels and control flow.
fn lower_instrs_to_ops(instrs: Vec<Instr>, ops: &mut dynasmrt::x64::Assembler) {
    use std::collections::HashMap as StdHashMap;
    let mut label_map: StdHashMap<String, dynasmrt::DynamicLabel> = StdHashMap::new();
    for ins in &instrs {
        if let Instr::Label(name) = ins {
            label_map.entry(name.clone()).or_insert_with(|| ops.new_dynamic_label());
        }
    }
    let err_fn_ptr: i64 = snek_error_host as i64;
    let print_fn_ptr: i64 = snek_print_host as i64;
    for instr in instrs {
        match instr {
            Instr::IMov(Val::Reg(Reg::Rbp), Val::Reg(Reg::Rsp)) => { dynasm!(ops ; .arch x64 ; mov rbp, rsp); }
            Instr::Jz(label) => { let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label()); dynasm!(ops ; .arch x64 ; jz =>*dl); }
            Instr::Jnz(label) => { let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label()); dynasm!(ops ; .arch x64 ; jnz =>*dl); }
            Instr::Jmp(label) => { let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label()); dynasm!(ops ; .arch x64 ; jmp =>*dl); }
            Instr::JO(label) => { let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label()); dynasm!(ops ; .arch x64 ; jo =>*dl); }
            Instr::Label(label) => {
                if let Some(dl) = label_map.get(&label) { dynasm!(ops ; .arch x64 ; =>*dl); }
                else { let dl = ops.new_dynamic_label(); label_map.insert(label.clone(), dl); let dl_ref = label_map.get(&label).unwrap(); dynasm!(ops ; .arch x64 ; =>*dl_ref); }
            }
            Instr::CallError(code) => { let code_i64 = code as i64; dynasm!(ops ; .arch x64 ; mov rdi, QWORD code_i64 ; mov rax, QWORD err_fn_ptr ; call rax); }
            Instr::CallPrint() => { dynasm!(ops ; .arch x64 ; push rdi ; mov rdi, rax ; mov rax, QWORD print_fn_ptr ; call rax ; pop rdi); }
            Instr::CallLabel(label) => { let dl = label_map.entry(label.clone()).or_insert_with(|| ops.new_dynamic_label()); dynasm!(ops ; .arch x64 ; call =>*dl); }
            Instr::CallPtr(ptr) => { let p = ptr; dynasm!(ops ; .arch x64 ; mov rax, QWORD p ; call rax); }
            Instr::Ret() => { dynasm!(ops ; .arch x64 ; ret); }
            Instr::SubRsp(n) => { let imm = n; let imm_ref = &imm; dynasm!(ops ; .arch x64 ; sub rsp, *imm_ref); }
            Instr::AddRsp(n) => { let imm = n; let imm_ref = &imm; dynasm!(ops ; .arch x64 ; add rsp, *imm_ref); }
            Instr::Push(Reg::Rbp) => { dynasm!(ops ; .arch x64 ; push rbp); }
            Instr::Push(Reg::Rax) => { dynasm!(ops ; .arch x64 ; push rax); }
            Instr::Push(Reg::Rcx) => { dynasm!(ops ; .arch x64 ; push rcx); }
            Instr::Push(Reg::Rdi) => { dynasm!(ops ; .arch x64 ; push rdi); }
            Instr::Push(_) => panic!("Unsupported push register"),
            Instr::Pop(Reg::Rbp) => { dynasm!(ops ; .arch x64 ; pop rbp); }
            Instr::Pop(Reg::Rax) => { dynasm!(ops ; .arch x64 ; pop rax); }
            Instr::Pop(Reg::Rcx) => { dynasm!(ops ; .arch x64 ; pop rcx); }
            Instr::Pop(Reg::Rdi) => { dynasm!(ops ; .arch x64 ; pop rdi); }
            Instr::Pop(_) => panic!("Unsupported pop register"),
            other => { instr_to_asm(&other, ops); }
        }
    }
}

fn compile_entry_instrs_with_ptrs(body: &Expr, lbl: &mut i32, define_env: &HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>, fun_ptrs: &std::collections::HashMap<String, (usize, i64)>) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    instrs.push(Instr::Label("our_code_starts_here".to_string()));
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::IMov(Val::Reg(Reg::Rbp), Val::Reg(Reg::Rsp)));
    let frame_slots = max_stack_usage(body);
    let frame_size = if frame_slots > 0 { frame_slots * 8 } else { 0 };
    if frame_size > 0 { instrs.push(Instr::SubRsp(frame_size)); }

    let env: im::HashMap<String, i32> = im::HashMap::new();
    let empty_labels: std::collections::HashMap<String, (usize, String)> = std::collections::HashMap::new();
    let body_instrs = compile_to_instrs(body, 8, &env, define_env, define_ptr_env, lbl, &empty_labels, fun_ptrs);
    instrs.extend(body_instrs);

    if frame_size > 0 { instrs.push(Instr::AddRsp(frame_size)); }
    instrs.push(Instr::Pop(Reg::Rbp));
    instrs.push(Instr::Ret());
    instrs
}

fn compile_function_instrs_repl(def: &FunDef, lbl: &mut i32, define_env: &HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>, fun_ptrs: &std::collections::HashMap<String, (usize, i64)>) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    // function entry label local to this buffer
    let label = format!("fun_{}_entry", def.name);
    instrs.push(Instr::Label(label));
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::IMov(Val::Reg(Reg::Rbp), Val::Reg(Reg::Rsp)));
    let frame_slots = max_stack_usage(&def.body);
    let frame_size = if frame_slots > 0 { frame_slots * 8 } else { 0 };
    if frame_size > 0 { instrs.push(Instr::SubRsp(frame_size)); }
    let mut env: im::HashMap<String, i32> = im::HashMap::new();
    for (i, (param_name, _param_type)) in def.params.iter().enumerate() { let off = 16 + (i as i32)*8; env.insert(param_name.clone(), off); }
    let empty_labels: std::collections::HashMap<String, (usize, String)> = std::collections::HashMap::new();
    let body_instrs = compile_to_instrs(&def.body, 8, &env, define_env, define_ptr_env, lbl, &empty_labels, fun_ptrs);
    instrs.extend(body_instrs);
    if frame_size > 0 { instrs.push(Instr::AddRsp(frame_size)); }
    instrs.push(Instr::Pop(Reg::Rbp));
    instrs.push(Instr::Ret());
    instrs
}

fn eval_jit_expr_with_env_and_ptrs(expr: &Expr, define_env: &HashMap<String, i64>, define_ptr_env: &std::collections::HashMap<String, i64>, fun_ptrs: &std::collections::HashMap<String, (usize, i64)>) -> Result<i64, String> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();
    let mut lbl = 0;
    let entry = compile_entry_instrs_with_ptrs(expr, &mut lbl, define_env, define_ptr_env, fun_ptrs);
    // set input to false by default for REPL
    let to_load: i64 = tag_boolean(false);
    dynasm!(ops ; .arch x64 ; mov rdi, QWORD to_load);
    lower_instrs_to_ops(entry, &mut ops);
    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
    let result = jitted_fn();
    Ok(result)
}

// puts every set! variable into a HashSet
fn collect_set_targets(e: &Expr, acc: &mut HashSet<String>) {
    match e {
        Expr::Set(name, rhs) => {
            acc.insert(name.clone());
            collect_set_targets(rhs, acc);
        }
        Expr::Let(bindings, body) => {
            for (_, ex) in bindings { collect_set_targets(ex, acc); }
            collect_set_targets(body, acc);
        }
        Expr::If(c,t,e2) => { collect_set_targets(c, acc); collect_set_targets(t, acc); collect_set_targets(e2, acc); }
        Expr::Loop(b) | Expr::Break(b) | Expr::UnOp(_, b) => collect_set_targets(b, acc),
        Expr::BinOp(_, l, r) => { collect_set_targets(l, acc); collect_set_targets(r, acc); }
        Expr::Block(es) => { for ex in es { collect_set_targets(ex, acc); } }
        _ => {}
    }
}

fn run_expr_with_define_mut(expr: &Expr, define_env: &mut HashMap<String, i64>, input_value: Option<i64>) -> Result<i64, String> {
    // Determine which defined vars are set! in this prompt
    let mut targets: HashSet<String> = HashSet::new();
    collect_set_targets(expr, &mut targets);
    // Allocate boxes for those present in define_env
    let mut define_ptr_env: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
    for name in targets {
        if let Some(val) = define_env.get(&name) {
            let v64: i64 = *val as i64;
            let boxed = Box::new(v64);
            let ptr = Box::into_raw(boxed) as i64;
            define_ptr_env.insert(name.clone(), ptr);
        }
    }
    // Run JIT with pointer env
    let res = eval_jit_with_define_ptr(expr, define_env, &define_ptr_env, input_value);
    // Read back any updates and update define_env
    for (name, ptr) in define_ptr_env.into_iter() {
        unsafe {
            let boxed = Box::from_raw(ptr as *mut i64);
            let val_i64 = (*boxed) as i64;
            define_env.insert(name, val_i64);
            // box drops here, freeing memory
        }
    }
    res
}

//helper functions for tc()
fn union(t1: Type, t2: Type) -> Type {
    match (t1.clone(), t2.clone()) {
        (t1, t2) if t1 == t2 => t1,
        (Type::Num, Type::Bool) => Type::Any,
        (Type::Bool, Type::Num) => Type::Any,
        (Type::Any, _) => Type::Any,
        (_, Type::Any) => Type::Any,
        (t, Type::Nothing) => t,
        (Type::Nothing, t) => t,
        _ => Type::Any,
    }
}

fn subtype(t1: Type, t2: Type) -> bool {
    match (t1, t2) {
        (_, Type::Any) => true,
        (Type::Nothing, _) => false,
        (t1, t2) => t1 == t2
    }
}

// Type environment structure
#[derive(Clone)]
struct TEnv {
    vars: im::HashMap<String, Type>,
    funcs: std::collections::HashMap<String, (Vec<Type>, Type)>, // (param_types, return_type)
    input_type: Type, // Type of input (default Any)
}

// returns two types, where the first is the type of expr and the second is the type of all breaks in expr
fn tc(expr: &Expr, tenv: &TEnv) -> (Type, Type) {
    match expr {
        Expr::Num(_) => (Type::Num, Type::Nothing),
        
        Expr::Boolean(_) => (Type::Bool, Type::Nothing),
        
        Expr::Input => (tenv.input_type.clone(), Type::Nothing),
        
        Expr::Id(name) => {
            match tenv.vars.get(name) {
                Some(t) => (t.clone(), Type::Nothing),
                None => panic!("Unbound variable: {}", name),
            }
        }
        
        Expr::UnOp(op, e) => {
            let (t, b) = tc(e, tenv);
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    if !subtype(t.clone(), Type::Num) {
                        panic!("Type error: {:?} requires Num", op);
                    }
                    (Type::Num, b)
                }
                Op1::IsNum | Op1::IsBool => {
                    (Type::Bool, b)
                }
                Op1::Print => {
                    (t, b)
                }
            }
        }
        
        Expr::BinOp(op, e1, e2) => {
            let (t1, b1) = tc(e1, tenv);
            let (t2, b2) = tc(e2, tenv);
            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    if !(subtype(t1, Type::Num) && subtype(t2, Type::Num)) {
                        panic!("Type error: arithmetic operations require Num");
                    }
                    (Type::Num, union(b1, b2))
                }
                Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                    if !(subtype(t1, Type::Num) && subtype(t2, Type::Num)) {
                        panic!("Type error: comparison operations require Num");
                    }
                    (Type::Bool, union(b1, b2))
                }
                Op2::Equal => {
                    (Type::Bool, union(b1, b2))
                }
            }
        }
        
        Expr::Let(bindings, body) => {
            let mut new_tenv = tenv.clone();
            let mut break_type = Type::Nothing;
            
            for (name, bind_expr) in bindings {
                let (t, b) = tc(bind_expr, &new_tenv);
                new_tenv.vars.insert(name.clone(), t);
                break_type = union(break_type, b);
            }
            
            let (body_type, body_break) = tc(body, &new_tenv);
            (body_type, union(break_type, body_break))
        }
        
        Expr::If(cond, thn, els) => {
            let (cond_t, cond_b) = tc(cond, tenv);
            if !subtype(cond_t, Type::Bool) {
                panic!("Type error: if condition must be Bool");
            }
            
            let (thn_t, thn_b) = tc(thn, tenv);
            let (els_t, els_b) = tc(els, tenv);
            
            let result_type = union(thn_t, els_t);
            let break_type = union(union(cond_b, thn_b), els_b);
            (result_type, break_type)
        }
        
        Expr::Loop(body) => {
            let (_, body_break) = tc(body, tenv);
            (body_break, Type::Nothing)
        }
        
        Expr::Break(e) => {
            let (t, b) = tc(e, tenv);
            (Type::Nothing, union(t, b))
        }
        
        Expr::Set(name, rhs) => {
            let (rhs_t, rhs_b) = tc(rhs, tenv);
            match tenv.vars.get(name) {
                Some(var_t) => {
                    if !subtype(rhs_t.clone(), var_t.clone()) {
                        panic!("Type error: cannot assign {:?} to variable of type {:?}", rhs_t, var_t);
                    }
                    (rhs_t, rhs_b)
                }
                None => panic!("Unbound variable in set!: {}", name),
            }
        }
        
        Expr::Block(exprs) => {
            if exprs.is_empty() {
                panic!("Empty block");
            }
            
            let mut result_type = Type::Nothing;
            let mut break_type = Type::Nothing;
            
            for e in exprs {
                let (t, b) = tc(e, tenv);
                result_type = t;
                break_type = union(break_type, b);
            }
            
            (result_type, break_type)
        }
        
        Expr::Call(fname, args) => {
            match tenv.funcs.get(fname) {
                Some((param_types, return_type)) => {
                    if args.len() != param_types.len() {
                        panic!("Type error: function {} expects {} arguments, got {}", 
                               fname, param_types.len(), args.len());
                    }
                    
                    let mut break_type = Type::Nothing;
                    for (i, arg) in args.iter().enumerate() {
                        let (arg_t, arg_b) = tc(arg, tenv);
                        if !subtype(arg_t.clone(), param_types[i].clone()) {
                            panic!("Type error: argument {} of {} has type {:?}, expected {:?}", 
                                   i, fname, arg_t, param_types[i]);
                        }
                        break_type = union(break_type, arg_b);
                    }
                    
                    (return_type.clone(), break_type)
                }
                None => panic!("Undefined function: {}", fname),
            }
        }
        
        Expr::Cast(target_type, e) => {
            let (_expr_t, expr_b) = tc(e, tenv);
            match target_type {
                Type::Any => (Type::Any, expr_b),
                Type::Nothing => (Type::Nothing, expr_b),
                _ => {
                    // For Num and Bool, the cast enforces that type at runtime
                    (target_type.clone(), expr_b)
                }
            }
        }
    }
}

// Helper function to build type environment from function definitions
fn build_tenv(defs: &Vec<FunDef>) -> TEnv {
    let mut funcs = std::collections::HashMap::new();
    
    for def in defs {
        let param_types: Vec<Type> = def.params.iter().map(|(_, t)| t.clone()).collect();
        funcs.insert(def.name.clone(), (param_types, def.return_type.clone()));
    }
    
    TEnv {
        vars: im::HashMap::new(),
        funcs,
        input_type: Type::Any,
    }
}

// Helper function to build type environment with specific input type
fn build_tenv_with_input(defs: &Vec<FunDef>, input_type: Type) -> TEnv {
    let mut funcs = std::collections::HashMap::new();
    
    for def in defs {
        let param_types: Vec<Type> = def.params.iter().map(|(_, t)| t.clone()).collect();
        funcs.insert(def.name.clone(), (param_types, def.return_type.clone()));
    }
    
    TEnv {
        vars: im::HashMap::new(),
        funcs,
        input_type,
    }
}

// Determine type from input string
fn get_input_type(input: &str) -> Type {
    match input {
        "true" | "false" => Type::Bool,
        _ => {
            if input.parse::<i64>().is_ok() {
                Type::Num
            } else {
                Type::Any
            }
        }
    }
}

// Type-check a function definition
fn tc_function(def: &FunDef, tenv: &TEnv) {
    // Build environment with parameters
    let mut func_tenv = tenv.clone();
    for (param_name, param_type) in &def.params {
        func_tenv.vars.insert(param_name.clone(), param_type.clone());
    }
    
    let (body_type, _break_type) = tc(&def.body, &func_tenv);
    
    // Check that body type matches return type
    if !subtype(body_type.clone(), def.return_type.clone()) {
        panic!("Type error in function {}: body has return type {:?}, expected {:?}", 
               def.name, body_type, def.return_type);
    }
    /*
    // Breaks should not escape function body (break_type should be Nothing)
    if break_type != Type::Nothing {
        panic!("Type error in function {}: break escapes function body", def.name);
    }*/
}

// Type-check an entire program
fn tc_program(program: &Program) -> Type {
    let tenv = build_tenv(&program.defs);
    
    // Type-check all function definitions
    for def in &program.defs {
        tc_function(def, &tenv);
    }
    
    // Type-check the main body and return its type
    let (body_type, break_type) = tc(&program.body, &tenv);
    body_type
}

// Type-check a program with specific input type
fn tc_program_with_input(program: &Program, input_type: Type) -> Type {
    let tenv = build_tenv_with_input(&program.defs, input_type);
    
    // Type-check all function definitions
    for def in &program.defs {
        tc_function(def, &tenv);
    }
    
    // Type-check the main body and return its type
    let (body_type, break_type) = tc(&program.body, &tenv);
    body_type
}

fn interactive_env() -> std::io::Result<()> {
    println!("\nWelcome to the snek REPL! Type \"exit\" to quit.\n");

    let stdin = io::stdin();
    let mut reader = stdin.lock();
    let mut define_env: HashMap<String, i64> = HashMap::new();
    let mut define_ptr_env: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
    let mut fun_defs: Vec<FunDef> = Vec::new(); // keep optionally for inspection
    let mut fun_ptrs: std::collections::HashMap<String, (usize, i64)> = std::collections::HashMap::new();
    let mut code_pages: Vec<dynasmrt::ExecutableBuffer> = Vec::new(); // keep JITed functions alive

    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        match reader.read_line(&mut input) {
            Ok(0) => {
                println!("See you next time!\n");
                std::process::exit(1);
            }
            Ok(_) => {
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }

                match parse(input) {
                    Ok(sexp) => {
                        match std::panic::catch_unwind(|| parse_repl_entry(&sexp)) {
                            Ok(ReplEntry::Define(var, expr)) => {
                                // Evaluate expr with current function defs and define pointers (allowing calls/side-effects)
                                // Ensure a cell exists for every defined var
                                for (name, val) in define_env.iter() {
                                    if !define_ptr_env.contains_key(name) {
                                        let boxed = Box::new(*val as i64);
                                        let ptr = Box::into_raw(boxed) as i64;
                                        define_ptr_env.insert(name.clone(), ptr);
                                    }
                                }
                                match eval_jit_expr_with_env_and_ptrs(&expr, &define_env, &define_ptr_env, &fun_ptrs) {
                                    Ok(value) => {
                                        // Write value into (existing or new) cell and env
                                        if let Some(ptr) = define_ptr_env.get(&var) {
                                            unsafe { *( *ptr as *mut i64) = value as i64; }
                                        } else {
                                            let boxed = Box::new(value as i64);
                                            let ptr = Box::into_raw(boxed) as i64;
                                            define_ptr_env.insert(var.clone(), ptr);
                                        }
                                        define_env.insert(var.clone(), value);
                                    }
                                    Err(e) => println!("{}", e),
                                }
                            },
                            Ok(ReplEntry::DefineFun(fd)) => {
                                // Add/replace function def
                                if fun_defs.iter().any(|f| f.name == fd.name) {
                                    // Replace existing by name
                                    if let Some(pos) = fun_defs.iter().position(|f| f.name == fd.name) { fun_defs.remove(pos); }
                                }
                                let arity = fd.params.len();
                                // Compile this function alone, using already-compiled fun_ptrs
                                let mut ops = dynasmrt::x64::Assembler::new().unwrap();
                                let start = ops.offset();
                                let mut lbl = 0;
                                let finstrs = compile_function_instrs_repl(&fd, &mut lbl, &define_env, &define_ptr_env, &fun_ptrs);
                                lower_instrs_to_ops(finstrs, &mut ops);
                                let buf = ops.finalize().unwrap();
                                let ptr = buf.ptr(start) as i64;
                                fun_ptrs.insert(fd.name.clone(), (arity, ptr));
                                code_pages.push(buf);
                                fun_defs.push(fd);
                            }
                            Ok(ReplEntry::Exit()) => {
                                println!("\nSee you next time!");
                                std::process::exit(0);
                            }
                            Ok(ReplEntry::Expr(expr)) => {
                                // Evaluate expression with current function defs and define pointers
                                // Ensure cells exist for all current defines
                                for (name, val) in define_env.iter() {
                                    if !define_ptr_env.contains_key(name) {
                                        let boxed = Box::new(*val as i64);
                                        let ptr = Box::into_raw(boxed) as i64;
                                        define_ptr_env.insert(name.clone(), ptr);
                                    }
                                }
                                match eval_jit_expr_with_env_and_ptrs(&expr, &define_env, &define_ptr_env, &fun_ptrs) {
                                    Ok(result) => println!("{}", format_value(result)),
                                    Err(e) => println!("{}", e),
                                }
                            }
                            Err(e) => {
                                println!("Compile error: {:?}", e);
                            }
                        }
                    }
                    Err(_) => {
                        println!("Invalid: Parse error");
                    }
                }
            }
            Err(e) => {
                println!("Error reading input: {}", e);
            }
        }
    }
}

fn typed_interactive_env() -> std::io::Result<()> {
    println!("\nWelcome to the snek REPL with type checking! Type \"exit\" to quit.\n");

    let stdin = io::stdin();
    let mut reader = stdin.lock();
    let mut define_env: HashMap<String, i64> = HashMap::new();
    let mut define_ptr_env: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
    let mut fun_defs: Vec<FunDef> = Vec::new();
    let mut fun_ptrs: std::collections::HashMap<String, (usize, i64)> = std::collections::HashMap::new();
    let mut code_pages: Vec<dynasmrt::ExecutableBuffer> = Vec::new();
    let mut var_types: im::HashMap<String, Type> = im::HashMap::new(); // Track variable types

    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        match reader.read_line(&mut input) {
            Ok(0) => {
                println!("See you next time!\n");
                std::process::exit(1);
            }
            Ok(_) => {
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }

                match parse(input) {
                    Ok(sexp) => {
                        match std::panic::catch_unwind(|| parse_repl_entry(&sexp)) {
                            Ok(ReplEntry::Define(var, expr)) => {
                                // Type-check the expression first
                                let tenv = TEnv {
                                    vars: var_types.clone(),
                                    funcs: fun_defs.iter().map(|fd| {
                                        let param_types: Vec<Type> = fd.params.iter().map(|(_, t)| t.clone()).collect();
                                        (fd.name.clone(), (param_types, fd.return_type.clone()))
                                    }).collect(),
                                    input_type: Type::Any,
                                };
                                
                                match std::panic::catch_unwind(|| tc(&expr, &tenv)) {
                                    Ok((expr_type, _)) => {
                                        // Type check passed, now evaluate
                                        for (name, val) in define_env.iter() {
                                            if !define_ptr_env.contains_key(name) {
                                                let boxed = Box::new(*val as i64);
                                                let ptr = Box::into_raw(boxed) as i64;
                                                define_ptr_env.insert(name.clone(), ptr);
                                            }
                                        }
                                        match eval_jit_expr_with_env_and_ptrs(&expr, &define_env, &define_ptr_env, &fun_ptrs) {
                                            Ok(value) => {
                                                if let Some(ptr) = define_ptr_env.get(&var) {
                                                    unsafe { *( *ptr as *mut i64) = value as i64; }
                                                } else {
                                                    let boxed = Box::new(value as i64);
                                                    let ptr = Box::into_raw(boxed) as i64;
                                                    define_ptr_env.insert(var.clone(), ptr);
                                                }
                                                define_env.insert(var.clone(), value);
                                                var_types.insert(var.clone(), expr_type);
                                            }
                                            Err(e) => println!("{}", e),
                                        }
                                    }
                                    Err(e) => {
                                        if let Some(msg) = e.downcast_ref::<String>() {
                                            println!("Type error: {}", msg);
                                        } else if let Some(msg) = e.downcast_ref::<&str>() {
                                            println!("Type error: {}", msg);
                                        } else {
                                            println!("Type error occurred");
                                        }
                                    }
                                }
                            },
                            Ok(ReplEntry::DefineFun(fd)) => {
                                // Type-check the function
                                let tenv = TEnv {
                                    vars: im::HashMap::new(),
                                    funcs: fun_defs.iter().map(|f| {
                                        let param_types: Vec<Type> = f.params.iter().map(|(_, t)| t.clone()).collect();
                                        (f.name.clone(), (param_types, f.return_type.clone()))
                                    }).collect(),
                                    input_type: Type::Any,
                                };
                                
                                match std::panic::catch_unwind(|| tc_function(&fd, &tenv)) {
                                    Ok(_) => {
                                        // Type check passed, compile and store
                                        if fun_defs.iter().any(|f| f.name == fd.name) {
                                            if let Some(pos) = fun_defs.iter().position(|f| f.name == fd.name) { 
                                                fun_defs.remove(pos); 
                                            }
                                        }
                                        let arity = fd.params.len();
                                        let mut ops = dynasmrt::x64::Assembler::new().unwrap();
                                        let start = ops.offset();
                                        let mut lbl = 0;
                                        let finstrs = compile_function_instrs_repl(&fd, &mut lbl, &define_env, &define_ptr_env, &fun_ptrs);
                                        lower_instrs_to_ops(finstrs, &mut ops);
                                        let buf = ops.finalize().unwrap();
                                        let ptr = buf.ptr(start) as i64;
                                        fun_ptrs.insert(fd.name.clone(), (arity, ptr));
                                        code_pages.push(buf);
                                        fun_defs.push(fd);
                                    }
                                    Err(e) => {
                                        if let Some(msg) = e.downcast_ref::<String>() {
                                            println!("Type error: {}", msg);
                                        } else if let Some(msg) = e.downcast_ref::<&str>() {
                                            println!("Type error: {}", msg);
                                        } else {
                                            println!("Type error occurred");
                                        }
                                    }
                                }
                            }
                            Ok(ReplEntry::Exit()) => {
                                println!("See you next time!\n");
                                std::process::exit(1);
                            }
                            Ok(ReplEntry::Expr(expr)) => {
                                // Type-check the expression first
                                let tenv = TEnv {
                                    vars: var_types.clone(),
                                    funcs: fun_defs.iter().map(|fd| {
                                        let param_types: Vec<Type> = fd.params.iter().map(|(_, t)| t.clone()).collect();
                                        (fd.name.clone(), (param_types, fd.return_type.clone()))
                                    }).collect(),
                                    input_type: Type::Any,
                                };
                                
                                match std::panic::catch_unwind(|| tc(&expr, &tenv)) {
                                    Ok((_expr_type, _)) => {
                                        // Type check passed, now evaluate
                                        for (name, val) in define_env.iter() {
                                            if !define_ptr_env.contains_key(name) {
                                                let boxed = Box::new(*val as i64);
                                                let ptr = Box::into_raw(boxed) as i64;
                                                define_ptr_env.insert(name.clone(), ptr);
                                            }
                                        }
                                        match eval_jit_expr_with_env_and_ptrs(&expr, &define_env, &define_ptr_env, &fun_ptrs) {
                                            Ok(result) => println!("{}", format_value(result)),
                                            Err(e) => println!("{}", e),
                                        }
                                    }
                                    Err(e) => {
                                        if let Some(msg) = e.downcast_ref::<String>() {
                                            println!("Type error: {}", msg);
                                        } else if let Some(msg) = e.downcast_ref::<&str>() {
                                            println!("Type error: {}", msg);
                                        } else {
                                            println!("Type error occurred");
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                if let Some(msg) = e.downcast_ref::<String>() {
                                    println!("Parse error: {}", msg);
                                } else if let Some(msg) = e.downcast_ref::<&str>() {
                                    println!("Parse error: {}", msg);
                                } else {
                                    println!("Parse error");
                                }
                            }
                        }
                    }
                    Err(e) => {
                        println!("Parse error: {}", e);
                    }
                }
            }
            Err(e) => {
                eprintln!("Error reading line: {}", e);
                return Err(e);
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        panic!("Invalid Flag - Usage: program (-c|-e|-g|-i|-t|-tc|-tg|-te|-ti) <in.snek> <out.asm>");
    }

    let flag = &args[1];

    // Handle interactive flags
    if flag == "-i" {
        if args.len() > 2 {
            panic!("Invalid arguments for \"-i\" - The interactive environment flag takes no arguments");
        }
        match interactive_env() {
            Err(e) => eprintln!("Error: {}", e),
            _ => return Ok(())
        }
    }

    if flag == "-ti" {
        if args.len() > 2 {
            panic!("Invalid arguments for \"-ti\" - The typed interactive environment flag takes no arguments");
        }
        match typed_interactive_env() {
            Err(e) => eprintln!("Error: {}", e),
            _ => return Ok(())
        }
    }

    let in_name = &args[2];
    
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp_top = parse_add_parenthesis(&in_contents).expect("Parse error");
    let program = parse_program(&sexp_top);
    let result= compile_program_to_string(&program);
    // result already contains labeled functions and our_code_starts_here with prologue/ret

    match flag.as_str() {
        "-c" => {
            if args.len() != 4 && args.len() != 5 {
                panic!("Invalid arguments for \"-c\" - Usage: program -c <in.snek> <out.asm> [input]");
            }
            // AOT: write assembly and exit
            let out_name = &args[3];
            // If an input is provided, inject mov rdi,<tagged> at top of function
            let asm_program = if args.len() == 5 {
                let tagged = parse_input_host(&args[4]) as i64;
                // Inject mov rdi just after our_code_starts_here label
                let injected = result.replacen("our_code_starts_here:\n", &format!("our_code_starts_here:\n  mov rdi, {}\n", tagged), 1);
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", injected)
            } else {
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", result)
            };
            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
            println!("Wrote assembly to {}", out_name);
            return Ok(());
        }
        "-g" => {
            if args.len() != 4 && args.len() != 5 {
                panic!("Invalid arguments for \"-g\" - Usage: program -g <in.snek> <out.asm> [input]");
            }
            let out_name = &args[3];
            let mut out_file = File::create(out_name)?;
            // add assembly to move input if input is provided
            let asm_program = if args.len() == 5 {
                let tagged = parse_input_host(&args[4]) as i64;
                let injected = result.replacen("our_code_starts_here:\n", &format!("our_code_starts_here:\n  mov rdi, {}\n", tagged), 1);
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", injected)
            } else {
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", result)
            };
            out_file.write_all(asm_program.as_bytes())?;
            // Evaluate via JIT with optional input
            let input_opt = if args.len() == 5 { Some(parse_input_host(&args[4])) } else { None };
            match eval_program(&in_contents, input_opt) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        "-e" => {
            if args.len() != 3 && args.len() != 4 {
                panic!("Invalid arguments for \"-e\" - Usage: program -e <in.snek> [input]");
            }
            let input_opt = if args.len() == 4 { Some(parse_input_host(&args[3])) } else { None };
            match eval_program(&in_contents, input_opt) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        "-t" => {
            // Typecheck only
            if args.len() != 3 {
                panic!("Invalid arguments for \"-t\" - Usage: program -t <in.snek>");
            }
            match std::panic::catch_unwind(|| tc_program(&program)) {
                Ok(return_type) => {
                    println!("Type check passed!");
                    println!("Program return type: {:?}", return_type);
                }
                Err(e) => {
                    if let Some(msg) = e.downcast_ref::<String>() {
                        eprintln!("Type error: {}", msg);
                    } else if let Some(msg) = e.downcast_ref::<&str>() {
                        eprintln!("Type error: {}", msg);
                    } else {
                        eprintln!("Type error occurred");
                    }
                    std::process::exit(1);
                }
            }
        }
        "-tc" => {
            // Typecheck and compile
            if args.len() != 4 && args.len() != 5 {
                panic!("Invalid arguments for \"-tc\" - Usage: program -tc <in.snek> <out.asm> [input]");
            }
            
            // First typecheck
            match std::panic::catch_unwind(|| tc_program(&program)) {
                Ok(_return_type) => {
                    println!("Type check passed!");
                }
                Err(e) => {
                    if let Some(msg) = e.downcast_ref::<String>() {
                        eprintln!("Compile type error: {}", msg);
                    } else if let Some(msg) = e.downcast_ref::<&str>() {
                        eprintln!("Compile type error: {}", msg);
                    } else {
                        eprintln!("Compile type error");
                    }
                    std::process::exit(1);
                }
            }
            
            // Then compile
            let out_name = &args[3];
            let asm_program = if args.len() == 5 {
                let tagged = parse_input_host(&args[4]) as i64;
                let injected = result.replacen("our_code_starts_here:\n", &format!("our_code_starts_here:\n  mov rdi, {}\n", tagged), 1);
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", injected)
            } else {
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", result)
            };
            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
            println!("Wrote assembly to {}", out_name);
            return Ok(());
        }
        "-tg" => {
            // Typecheck with input type, then generate and run
            if args.len() != 4 && args.len() != 5 {
                panic!("Invalid arguments for \"-tg\" - Usage: program -tg <in.snek> <out.asm> [input]");
            }
            
            // Determine input type
            let input_type = if args.len() == 5 {
                get_input_type(&args[4])
            } else {
                Type::Any
            };
            
            // Typecheck with input type
            match std::panic::catch_unwind(|| tc_program_with_input(&program, input_type)) {
                Ok(_return_type) => {
                    println!("Type check passed!");
                }
                Err(e) => {
                    if let Some(msg) = e.downcast_ref::<String>() {
                        eprintln!("Type error: {}", msg);
                    } else if let Some(msg) = e.downcast_ref::<&str>() {
                        eprintln!("Type error: {}", msg);
                    } else {
                        eprintln!("Type error occurred");
                    }
                    std::process::exit(1);
                }
            }
            
            // Generate assembly
            let out_name = &args[3];
            let asm_program = if args.len() == 5 {
                let tagged = parse_input_host(&args[4]) as i64;
                let injected = result.replacen("our_code_starts_here:\n", &format!("our_code_starts_here:\n  mov rdi, {}\n", tagged), 1);
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", injected)
            } else {
                format!("section .text\nextern snek_error, snek_print\nglobal our_code_starts_here\n{}", result)
            };
            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
            
            // Evaluate via JIT with optional input
            let input_opt = if args.len() == 5 { Some(parse_input_host(&args[4])) } else { None };
            match eval_program(&in_contents, input_opt) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        "-te" => {
            // Typecheck with input type, then evaluate
            if args.len() != 3 && args.len() != 4 {
                panic!("Invalid arguments for \"-te\" - Usage: program -te <in.snek> [input]");
            }
            
            // Determine input type
            let input_type = if args.len() == 4 {
                get_input_type(&args[3])
            } else {
                Type::Any
            };
            
            // Typecheck with input type
            match std::panic::catch_unwind(|| tc_program_with_input(&program, input_type)) {
                Ok(_return_type) => {
                    println!("Type check passed!");
                }
                Err(e) => {
                    if let Some(msg) = e.downcast_ref::<String>() {
                        eprintln!("Type error: {}", msg);
                    } else if let Some(msg) = e.downcast_ref::<&str>() {
                        eprintln!("Type error: {}", msg);
                    } else {
                        eprintln!("Type error occurred");
                    }
                    std::process::exit(1);
                }
            }
            
            // Evaluate
            let input_opt = if args.len() == 4 { Some(parse_input_host(&args[3])) } else { None };
            match eval_program(&in_contents, input_opt) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        _ => panic!("Invalid Flag - Usage: program (-c|-e|-g|-i|-t|-tc|-tg|-te|-ti) <in.snek> <out.asm>"),
    }
    
    
    
    Ok(())
}

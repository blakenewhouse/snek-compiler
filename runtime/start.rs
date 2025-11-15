use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(val: u64) -> u64 {
    // detag value
    if (val & 1) == 0 {
        // number
        let n = (val as i64) >> 1;
        println!("{n}");
    } else if val == 0b11 { // true
        println!("true");
    } else if val == 0b01 { // false
        println!("false");
    } else {
        println!("Unknown value: {val}");
    }
    val // return the value
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    let mut err_str = "";
    match errcode {
        -1 => err_str = "invalid argument - ERROR_ARITH_NOT_NUM",
        -3 => err_str = "invalid argument - ERROR_COMP_NOT_NUM",
        -5 => err_str = "invalid argument - ERROR_LOGIC_NOT_BOOL",
        -7 => err_str = "invalid argument - ERROR_IF_NOT_BOOL",
        -9 => err_str = "overflow error - ERROR_OVERFLOW",
        -11 => err_str = "ERROR_EQUAL_COMP_TYPES",
        -13 => err_str = "ERROR_INFINITE_LOOP",
        -15 => err_str = "ERROR_LOOPLESS_BREAK",
        -17 => err_str = "ERROR_UNBOUND_VARIABLE",
        -19 => err_str = "bad cast",
        _ => unreachable!(),
    }
    eprintln!("an error ocurred: {}", err_str);
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    // Tagged value scheme: numbers are (n << 1) | 0, booleans are 0b01 (false) and 0b11 (true)
    match input {
        "true" => 0b11u64,
        "false" => 0b01u64,
        s => {
            // Try parse as integer (supports leading +/-)
            if let Ok(n) = s.parse::<i64>() {
                ((n as u64) << 1) | 0u64
            } else {
                // On invalid input string, default to false
                0b01u64
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    // Decode tagged value for pretty printing
    if (i & 1) == 0 {
        // number: arithmetic shift by 1; cast to i64 for sign
        let n = (i as i64) >> 1;
        println!("{n}");
    } else if i == 0b11 { // true
        println!("true");
    } else if i == 0b01 { // false
        println!("false");
    } else {
        // Unknown tag (shouldn't happen)
        println!("{i}");
    }
}
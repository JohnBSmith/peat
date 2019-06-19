
use std::env;
use libpeatc::{read_file,compile};

struct CommandLineInfo {
    path: Option<String>
}

fn command_line_info() -> CommandLineInfo {
    let mut info = CommandLineInfo{path: None};
    let mut first = true;
    for arg in env::args() {
        if first {
            first = false;
        }else{
            info.path = Some(arg);
        }
    }
    return info;
}

fn main() {
    let info = command_line_info();
    let path = match info.path {
        Some(value) => value,
        None => {
            println!("Peat compiler\n\nUsage: peatc [OPTIONS] INPUT\n");
            return;
        }
    };
    let input = match read_file(&path) {
        Ok(value) => value,
        Err(_) => {
            println!("Error: could not read file '{}'\n",path);
            return;
        }
    };
    compile(&input);
}

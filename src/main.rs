use clap::{Arg, Command};

mod fmt;

fn main() {
    let matches = Command::new("vbsfmt")
        .version("1.0")
        .author("Your Name <your.email@example.com>")
        .about("VBScript standard formatter")
        .arg(
            Arg::new("input")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("output")
                .help("Sets the output file to use, defaults to overwriting the input file")
                .required(false)
                .index(2),
        )
        .get_matches();

    // Gets a value for input if supplied by user
    let input_file = matches.get_one::<String>("input").unwrap();
    println!("Using input file: {}", input_file);

    // Gets a value for output if supplied by user, else use default output file name
    let output_file = matches.get_one::<String>("output");
    if let Some(output) = output_file {
        println!("Using output file: {}", output);
    }

    // read input file
    let input_path = std::path::Path::new(&input_file);
    if !input_path.exists() {
        eprintln!("Input file does not exist: {}", input_file);
        std::process::exit(1);
    }
    // TODO handle non-utf8 files
    let input = std::fs::read_to_string(input_path).unwrap();

    let options = fmt::FormatOptions::default();
    let output = fmt::fmt(&input, options);

    let output_path = match output_file {
        Some(output) => {
            let path = std::path::Path::new(output);
            if path.exists() {
                eprintln!("Output file already exists: {}", output);
                std::process::exit(1);
            }
            path
        }
        None => input_path,
    };

    std::fs::write(output_path, output).unwrap();
}

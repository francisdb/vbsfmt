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
            Arg::new("v")
                .short('v')
                .action(clap::ArgAction::Count)
                .help("Sets the level of verbosity"),
        )
        .get_matches();

    // Gets a value for input if supplied by user
    let input = matches.get_one::<String>("input").unwrap();
    println!("Using input file: {}", input);

    // Vary the output based on how many times the user used the "v" flag
    // (i.e. 'myprog -v -v -v' or 'myprog -vvv' vs 'myprog -v'
    match matches.get_count("v") {
        0 => println!("No verbose info"),
        1 => println!("Some verbose info"),
        2 => println!("Tons of verbose info"),
        3 => println!("Don't be crazy"),
        _ => panic!("Verbosity level > 3 not supported"),
    }

    // read input file
    let input_path = std::path::Path::new(&input);
    if !input_path.exists() {
        eprintln!("Input file does not exist: {}", input);
        std::process::exit(1);
    }
    // TODO handle non-utf8 files
    let input = std::fs::read_to_string(input_path).unwrap();

    let options = fmt::FormatOptions::default();
    let output = fmt::fmt(&input, options);

    // write output file
    let output_path = input_path.with_extension("fmt.vbs");
    std::fs::write(output_path, output).unwrap();
}

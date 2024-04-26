use clap::{Arg, ArgMatches, Command};
use yansi::Paint;

mod fmt;

fn main() {
    let matches = Command::new("vbsfmt")
        .version("1.0")
        .author("Your Name <your.email@example.com>")
        .about("VBScript standard formatter")
        .subcommand(
            Command::new("fmt")
                .about("Format the input file (default)")
                .arg(
                    Arg::new("input")
                        .help("Sets the input file to use")
                        .required(true),
                )
                .arg(
                    Arg::new("output")
                        .help("Sets the output file to use, defaults to overwriting the input file")
                        .required(false),
                ),
        )
        .subcommand(
            Command::new("check")
                .about("Check if the input file is already formatted")
                .arg(
                    Arg::new("input")
                        .help("Sets the input file to use")
                        .required(true),
                ),
        )
        .get_matches();

    match matches.subcommand() {
        Some(("fmt", sub_matches)) => {
            fmt(sub_matches);
        }
        Some(("check", sub_matches)) => {
            check(sub_matches);
        }
        Some(_) => {
            eprintln!("Invalid subcommand");
            std::process::exit(1);
        }
        None => {
            // default to fmt
            let sub_matches = matches.subcommand_matches("fmt").unwrap();
            fmt(sub_matches);
        }
    }
}

fn fmt(sub_matches: &ArgMatches) {
    // Gets a value for input if supplied by user
    let input_file = sub_matches.get_one::<String>("input").unwrap();
    println!("Using input file: {}", input_file);
    // read input file
    let input_path = std::path::Path::new(&input_file);
    if !input_path.exists() {
        eprintln!("Input file does not exist: {}", input_file);
        std::process::exit(1);
    }

    // Gets a value for output if supplied by user, else use default output file name
    let output_file = sub_matches.get_one::<String>("output");
    if let Some(output) = output_file {
        println!("Using output file: {}", output);
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

fn check(sub_matches: &ArgMatches) {
    // Gets a value for input if supplied by user
    let input_file = sub_matches.get_one::<String>("input").unwrap();
    println!("Using input file: {}", input_file);
    // read input file
    let input_path = std::path::Path::new(&input_file);
    if !input_path.exists() {
        eprintln!("Input file does not exist: {}", input_file);
        std::process::exit(1);
    }

    // read input file
    let input = std::fs::read_to_string(input_path).unwrap();

    let options = fmt::FormatOptions::default();
    let output = fmt::fmt(&input, options);

    // use diff crate
    let diff = diff::lines(&input, &output);
    // if equal, print "already formatted"
    if diff.iter().all(|d| matches!(d, diff::Result::Both(_, _))) {
        // all ok
    } else {
        for d in diff {
            match d {
                diff::Result::Left(l) => println!("{}", Paint::red(&format!("-{}", l))),
                diff::Result::Both(b, _) => println!(" {}", b),
                diff::Result::Right(r) => println!("{}", Paint::green(&format!("+{}", r))),
            }
        }
        std::process::exit(1);
    }
}

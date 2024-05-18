// use std::ops::Range;
//
// use indoc::indoc;
// use pretty_assertions::assert_eq;

/// It tries to parse all `.vbs` files going one level lower from the root of the project.
/// We suggest to make sure you have https://github.com/jsm174/vpx-standalone-scripts cloned
/// in the same directory as this project.
///
/// Run this test with `cargo test --release -- --nocapture --ignored try_parsing_all_vbs_files`
#[test]
fn try_formatting_all_vbs_files() {
    // print current directory
    println!("Current directory: {:?}", std::env::current_dir().unwrap());
    // only check on linux
    if cfg!(linux) {
        assert!(std::path::Path::new("./testscripts/vpinball/scripts").exists());
    }
    let paths = glob::glob("./testscripts/**/*.vbs")
        .unwrap()
        .filter_map(Result::ok);
    for path in paths {
        println!("Parsing file: {}", path.display());
        let input = std::fs::read_to_string(&path).unwrap();
        vbsfmt::fmt::fmt(&input, vbsfmt::fmt::FormatOptions::default());
    }
}

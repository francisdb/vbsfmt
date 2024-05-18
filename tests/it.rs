// use std::ops::Range;
//
// use indoc::indoc;
// use pretty_assertions::assert_eq;

#[test]
fn try_formatting_all_vbs_files() {
    let paths = glob::glob("./testscripts/**/*.vbs")
        .unwrap()
        .filter_map(Result::ok);
    for path in paths {
        println!("Parsing file: {}", path.display());
        let input = std::fs::read_to_string(&path).unwrap();
        vbsfmt::fmt::fmt(&input, vbsfmt::fmt::FormatOptions::default());
    }
}

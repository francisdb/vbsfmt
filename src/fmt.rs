/// Keywords to search/replace.
/// The search does not use case sensitivity. Finds will be replaced according to the specified case in the array.
/// E.g. "Sub" will match any sub, case-insensitive, and replace with "Sub".
const VBS_KEYWORDS: [&str; 173] = [
    // VBScript (taken from Geshi with a few modifications)
    "Empty",
    "Nothing",
    "Null",
    "vbArray",
    "vbBoolean",
    "vbByte",
    "vbCr",
    "vbCrLf",
    "vbCurrency",
    "vbDate",
    "vbDouble",
    "vbEmpty",
    "vbError",
    "vbFirstFourDays",
    "vbFirstFullWeek",
    "vbFirstJan1",
    "vbFormFeed",
    "vbFriday",
    "vbInteger",
    "vbLf",
    "vbLong",
    "vbMonday",
    "vbNewLine",
    "vbNull",
    "vbNullChar",
    "vbNullString",
    "vbObject",
    "vbSaturday",
    "vbSingle",
    "vbString",
    "vbSunday",
    "vbTab",
    "vbThursday",
    "vbTuesday",
    "vbUseSystem",
    "vbUseSystemDayOfWeek",
    "vbVariant",
    "vbWednesday",
    "False",
    "True",
    "bs",
    "Array",
    "Asc",
    "Atn",
    "CBool",
    "CByte",
    "CDate",
    "CDbl",
    "Chr",
    "CInt",
    "Class",
    "CLng",
    "Cos",
    "CreateObject",
    "CSng",
    "CStr",
    "Date",
    "DateAdd",
    "DateDiff",
    "DatePart",
    "DateSerial",
    "DateValue",
    "Day",
    "Default",
    "Eval",
    "Exp",
    "Filter",
    "Fix",
    "FormatDateTime",
    "FormatNumber",
    "FormatPercent",
    "GetObject",
    "Hex",
    "Hour",
    "InputBox",
    "InStr",
    "InstrRev",
    "Int",
    "IsArray",
    "IsDate",
    "IsEmpty",
    "IsNull",
    "IsNumeric",
    "IsObject",
    "Join",
    "LBound",
    "LCase",
    "Left",
    "Len",
    "Log",
    "LTrim",
    "Mid",
    "Minute",
    "Month",
    "MonthName",
    "MsgBox",
    "Now",
    "Oct",
    "Replace",
    "RGB",
    "Right",
    "Rnd",
    "Round",
    "RTrim",
    "ScriptEngine",
    "ScriptEngineBuildVersion",
    "ScriptEngineMajorVersion",
    "ScriptEngineMinorVersion",
    "Second",
    "Sgn",
    "Sin",
    "Space",
    "Split",
    "Sqr",
    "StrComp",
    "String",
    "StrReverse",
    "Tan",
    "Time",
    "TimeSerial",
    "TimeValue",
    "Trim",
    "TypeName",
    "UBound",
    "UCase",
    "VarType",
    "Weekday",
    "WeekdayName",
    "Year",
    "Call",
    "Case",
    "Const",
    "Dim",
    "Do",
    "Each",
    "Else",
    "End",
    "Erase",
    "Execute",
    "Exit",
    "For",
    "Function",
    "GoSub",
    "GoTo",
    "If",
    "Loop",
    "Next",
    "On Error",
    "Option Explicit",
    "Private",
    "Public",
    "Randomize",
    "ReDim",
    "Rem",
    "Resume",
    "Select",
    "Set",
    "Sub",
    "Then",
    "Wend",
    "While",
    "With",
    "In",
    "To",
    "Step",
    "And",
    "Eqv",
    "Imp",
    "Is",
    "Mod",
    "Not",
    "Or",
    "Xor",
];

/// Visual Pinball Globals
const VPINBALL_KEYWORDS: [&str; 62] = [
    "GameTime",
    "SystemTime",
    "GetCustomParam",
    "NightDay",
    "LeftFlipperKey",
    "RightFlipperKey",
    "LeftTiltKey",
    "RightTiltKey",
    "CenterTiltKey",
    "PlungerKey",
    "StartGameKey",
    "AddCreditKey",
    "AddCreditKey2",
    "LeftMagnaSave",
    "RightMagnaSave",
    "LockbarKey",
    "ActiveBall",
    "ActiveTable",
    "ShowDT",
    "ShowFSS",
    "WindowWidth",
    "WindowHeight",
    "DMDWidth",
    "DMDHeight",
    "DMDPixels",
    "DMDColoredPixels",
    "RenderingMode",
    "Nudge",
    "NudgeGetCalibration",
    "NudgeSetCalibration",
    "NudgeSensorStatus",
    "NudgeTiltStatus",
    "PlaySound",
    "StopSound",
    "PlayMusic",
    "MusicVolume",
    "EndMusic",
    "FireKnocker",
    "QuitPlayer",
    "Version",
    "VPBuildVersion",
    "VersionMajor",
    "VersionMinor",
    "VersionRevision",
    "GetBalls",
    "GetElements",
    "GetElementByName",
    "UpdateMaterial",
    "GetMaterial",
    "UpdateMaterialPhysics",
    "GetMaterialPhysics",
    "MaterialColor",
    "GetSerialDevices",
    "OpenSerial",
    "CloseSerial",
    "FlushSerial",
    "SetupSerial",
    "ReadSerial",
    "WriteSerial",
    "LoadValue",
    "SaveValue",
    "UserDirectory",
];

const INDENT_STARTERS: [&str; 21] = [
    "Case ",
    "Do",
    "Do ",
    "Else",
    "ElseIf ",
    "For ",
    "Function ",
    "Public Function ",
    "Public Default Function ",
    "Private Function ",
    "If ",
    "Select Case ",
    "Sub ",
    "Public Sub ",
    "Private Sub ",
    "Property ",
    "Public Property ",
    "Private Property ",
    "While ",
    "With ",
    "Class ",
];

const INDENT_ENDERS: [&str; 7] = ["Else", "ElseIf ", "End ", "Loop", "Next", "Wend ", "Case "];

const INDENT: &str = "    "; //"\t";

pub(crate) struct FormatOptions {
    pub(crate) capitalize_keywords: bool,
    pub(crate) remove_chained_code: bool,
    pub(crate) fix_indentation: bool,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            capitalize_keywords: true,
            remove_chained_code: true,
            fix_indentation: true,
        }
    }
}

pub(crate) fn fmt(input: &str, options: FormatOptions) -> String {
    let mut data = input.to_string();

    // Uniformize line endings to LF
    // Also, replace any CR that is not part of a CRLF pair
    data = data.replace("\r\n", "\n").replace('\r', "\n");

    if options.capitalize_keywords {
        data = capitalize_keywords(&data);
    }

    if options.remove_chained_code {
        data = remove_chained_code(&data);
    }

    if options.fix_indentation {
        data = fix_indentation(&data);
    }

    // make sure the file ends with a newline
    if !data.ends_with('\n') {
        data.push('\n');
    }

    // All line endings back to windows style (CRLF)
    data = data.replace('\n', "\r\n");

    data
}

fn capitalize_keywords(input: &str) -> String {
    let mut data = input.to_string();
    data = capitalize_keywords_for(&data, &VBS_KEYWORDS);
    data = capitalize_keywords_for(&data, &VPINBALL_KEYWORDS);
    data
}

fn capitalize_keywords_for(input: &str, keywords: &[&str]) -> String {
    let mut data = input.to_string();
    for keyword in keywords.iter() {
        data = replace_vbscript_code(&data, keyword, keyword);
    }
    data
}

/// Replace all instances in a string
/// by doing a case-insensitive match and replacing search_string with replace_string.
/// only replace if the search_string is not inside a string or a comment
///
/// @param {string} vbscriptCode The vbscript code
/// @param {string|RegExp} searchString The string or regex to find
/// @param {string} replaceString The string to which all searchString occurrences are changed
/// @returns The modified vbscript code
fn replace_vbscript_code(vbscript_code: &str, search_string: &str, replace_string: &str) -> String {
    let lower_vbscript_code = vbscript_code.to_ascii_lowercase();
    let lower_search_string = search_string.to_ascii_lowercase();
    // first do a case-insensitive search, so we don't do a lot of work if there is no match.
    if !lower_vbscript_code.contains(&search_string.to_ascii_lowercase()) {
        return vbscript_code.to_string();
    }
    let mut lines: Vec<String> = vbscript_code.lines().map(|line| line.to_string()).collect();
    for line in lines.iter_mut() {
        let mut new_line = String::new();
        let mut inside_string = false;
        let mut escape_next = false;
        let mut inside_comment = false;
        let mut at_word_start = true;
        // if we found a match we need to skip the characters we just replaced
        let mut skip_chars = 0;

        for (i, char) in line.chars().enumerate() {
            if skip_chars > 0 {
                skip_chars -= 1;
                continue;
            } else if escape_next {
                new_line.push(char);
                escape_next = false;
            } else if char == '\\' {
                escape_next = true;
                new_line.push(char);
            } else if char == '"' {
                inside_string = !inside_string;
                new_line.push(char);
            } else if char == '\'' && !inside_string {
                inside_comment = true;
                new_line.push(char);
            } else if char == '\n' {
                inside_comment = false;
                new_line.push(char);
            } else if at_word_start && !inside_comment && !inside_string {
                at_word_start = false;
                let lower_char = char.to_ascii_lowercase();
                let mut found = false;
                if lower_char == lower_search_string.chars().next().unwrap() {
                    let mut match_found = true;
                    for (j, search_char) in lower_search_string.chars().enumerate() {
                        match line.chars().nth(i + j) {
                            Some(c) => {
                                if c.to_ascii_lowercase() != search_char {
                                    match_found = false;
                                    break;
                                }
                            }
                            None => {
                                match_found = false;
                                break;
                            }
                        }
                    }
                    // if the word continues after the search string, we don't want to replace the match
                    if match_found {
                        let next_char = line.chars().nth(i + search_string.len());
                        if let Some(next_char) = next_char {
                            if next_char.is_alphanumeric() {
                                match_found = false;
                            }
                        }
                    }
                    if match_found {
                        new_line += replace_string;
                        skip_chars = search_string.len() - 1;
                        found = true;
                    }
                }
                if !found {
                    new_line.push(char);
                }
            } else {
                new_line.push(char);
            }
            if !char.is_alphanumeric() {
                at_word_start = true;
            }
        }
        *line = new_line;
    }
    lines.join("\n")
}

fn find_vbscript_comment_index(vbscript_line: &str) -> Option<usize> {
    let mut comment_index = None;
    let mut inside_string = false;

    for (i, char) in vbscript_line.chars().enumerate() {
        match char {
            '"' if !inside_string => inside_string = true,
            '"' if inside_string => inside_string = false,
            '\'' if !inside_string => {
                comment_index = Some(i);
                break;
            }
            _ => {}
        }
    }

    comment_index
}

/// When code is chained together on the same line with a colon, break it up onto new lines and remove the colons.
///
/// @param {string} vbscriptCode The vbscript code
/// @returns {string} The modified vbscript code
fn remove_chained_code(vbscript_code: &str) -> String {
    let mut stack = vec![]; // List of end statements we must ensure exists

    // Split and process code line by line
    let updated_lines: Vec<String> = vbscript_code
        .lines()
        .map(|script_line| {
            // Trim the line of whitespace, and calculate length
            let trimmed_line = script_line.trim();
            let mut line = trimmed_line.to_string();
            let mut end_char = line.len();

            // If a comment exists, determine the line length to end at the start of the comment (so as to ignore comments)
            let comment_index = find_vbscript_comment_index(&line);
            if let Some(comment_index) = comment_index {
                end_char = comment_index;
            }

            // let trimmed_line_no_comments = if let Some(comment_index) = comment_index {
            //     line[..comment_index].to_string()
            // } else {
            //     line.clone()
            // };

            // Split line into segments delimited by quotes
            let mut segments: Vec<String> = vec![];
            let mut current_segment: String = String::new();
            let mut in_quotes = false;
            let mut escape_next = false;
            for j in 0..end_char {
                // TODO is this UTF-8 safe ?
                let chr = line.chars().nth(j).unwrap();

                if escape_next {
                    // append char to current_segment
                    current_segment.push(chr);
                    escape_next = false;
                } else if chr == '\\' {
                    escape_next = true;
                    current_segment.push(chr);
                } else if chr == '"' {
                    in_quotes = !in_quotes;
                    current_segment.push(chr);
                } else if chr == ':' && !in_quotes {
                    // We found a colon outside a quotation that must be split into multiple lines.
                    // If the colon is at the end of the line, then there is no code to separate.
                    if j < end_char - 1 {
                        // Special case for "If Then" statement with one or more colons
                        if current_segment.to_ascii_lowercase().starts_with("if ")
                            && current_segment.to_ascii_lowercase().contains(" then ")
                        {
                            let then_index =
                                current_segment.to_ascii_lowercase().find(" then ").unwrap();
                            let until_then = &current_segment[..then_index + 5];
                            let between_then_and_colon = &current_segment[then_index + 5..j];

                            segments.push(until_then.trim().to_string());
                            segments.push(between_then_and_colon.trim().to_string());
                            current_segment = String::new();
                        } else {
                            segments.push(current_segment.trim().to_string());
                            current_segment = String::new();
                        }
                    }
                } else {
                    current_segment.push(chr);
                }
            }

            // Continue if colons were found
            if !segments.is_empty() {
                segments.push(current_segment.trim().to_string());

                // Special case: If any "End If" statements existed, then split all "If/Then" onto a new line to prevent indentation breakages.
                let mut has_end_if = false;
                segments.reverse();
                segments = segments
                    .iter()
                    .map(|segment| {
                        let lower_segment = segment.to_ascii_lowercase();
                        if lower_segment.starts_with("end if") {
                            has_end_if = true;
                        }

                        if !has_end_if {
                            return segment.to_string();
                        }

                        let then_index = lower_segment.find(" then ");
                        if let Some(then_index) = then_index {
                            let first_part = &segment[..then_index + 5];
                            let second_part = &segment[then_index + 6..];
                            let new_segment = format!("{}\n{}", first_part, second_part);
                            new_segment
                        } else {
                            segment.to_string()
                        }
                    })
                    .collect();
                segments.reverse();

                // Join each segment onto a new line (effectively putting the code separated by colons onto new lines)

                line = segments.join("\n");
                let line_lower = line.to_ascii_lowercase();

                // Determine what end statements must exist
                if line_lower.starts_with("if ") {
                    stack.push("End If");
                } else if line_lower.starts_with("select") {
                    stack.push("End Select");
                } else if line_lower.starts_with("for ") {
                    stack.push("Next");
                } else if line_lower.starts_with("do ") {
                    stack.push("Loop");
                } else if line_lower.starts_with("function ")
                    || line_lower.starts_with("public function ")
                    || line_lower.starts_with("public default function ")
                    || line_lower.starts_with("private function ")
                {
                    stack.push("End Function");
                } else if line_lower.starts_with("sub ")
                    || line_lower.starts_with("public sub ")
                    || line_lower.starts_with("private sub ")
                {
                    stack.push("End Sub");
                } else if line_lower.starts_with("property ")
                    || line_lower.starts_with("public property ")
                    || line_lower.starts_with("private property ")
                {
                    stack.push("End Property");
                } else if line_lower.starts_with("class ") {
                    stack.push("End Class");
                } else if line_lower.starts_with("while ") {
                    stack.push("Wend");
                } else if line_lower.starts_with("with ") {
                    stack.push("End With");
                }

                // If a comment existed on the original line, start with the comment first, new line, and then the code.
                if let Some(comment_index) = comment_index {
                    let mut commented = trimmed_line[comment_index..].to_string();
                    if !line.is_empty() {
                        commented += "\n";
                        commented += &line;
                    }
                    line = commented;
                }

                // Determine which of the end statements already exists; we do not need to add them again, or we will break the code.
                if !stack.is_empty() {
                    let mut split: Vec<&str> = line.split('\n').collect();
                    split.reverse();
                    split.iter().for_each(|n_line| {
                        if !stack.is_empty()
                            && stack[stack.len() - 1].to_ascii_lowercase()
                                == n_line.trim().to_ascii_lowercase()
                        {
                            stack.pop();
                        }
                    });
                }

                // For each of the end statements missing, we must add them on a new line.
                while !stack.is_empty() {
                    line += "\n";
                    line += stack.pop().unwrap();
                }
            }
            line
        })
        .collect();

    updated_lines.join("\n")
}

/// Re-formats the indentation of the vbscript code using tabs based on the code blocks.
///
/// This function expects the code to have all keywords capitalized. So it should be called after `capitalize_keywords`.
///
/// @param {string} vbscriptCode The vbscript code
/// @returns {string} The re-indented code
fn fix_indentation(vbscript_code: &str) -> String {
    let mut current_indentation = 0;

    let updated_lines: Vec<String> = vbscript_code
        .lines()
        .enumerate()
        .map(|(line_nr, script_line)| {
            let trimmed_line = script_line.trim().to_string();
            //let end_char = line.len();

            let comment_index = find_vbscript_comment_index(&trimmed_line);
            let line_without_comment = if let Some(comment_index) = comment_index {
                &trimmed_line[..comment_index]
            } else {
                if trimmed_line.is_empty() {
                    return trimmed_line;
                }
                &trimmed_line
            };

            for ender in INDENT_ENDERS.iter() {
                if (ender.ends_with(' ') && line_without_comment.starts_with(ender))
                    || line_without_comment.eq_ignore_ascii_case(ender)
                {
                    if current_indentation == 0 {
                        eprintln!(
                            "ERROR: Negative indentation level at line {line_nr}: {trimmed_line}"
                        );
                        current_indentation = 1;
                    }

                    // End Select follows a double indentation so we need to remove an extra indentation level.
                    if line_without_comment.starts_with("End Select") {
                        current_indentation -= 1;
                    }

                    current_indentation -= 1;
                }
            }

            // print current line at current indentation level
            let indent = INDENT.repeat(current_indentation);
            let indented_line = format!("{}{}", indent, trimmed_line);

            for starter in INDENT_STARTERS.iter() {
                if (starter.ends_with(' ') && line_without_comment.starts_with(starter))
                    || line_without_comment.eq_ignore_ascii_case(starter)
                {
                    if starter == &"If "
                        && line_without_comment.to_ascii_lowercase().contains(" then ")
                    {
                        continue;
                    }

                    // In case of Select Case the next case will do an unindent.
                    // So we need to add an extra indentation level.
                    if starter == &"Select Case " {
                        current_indentation += 1;
                    }

                    current_indentation += 1;
                }
            }

            // enable this for debuging
            // println!("{}: {}", line_nr, indented_line);
            indented_line
        })
        .collect();

    // If the input ends with a newline, the output should too
    // lines() removes the trailing newline, so we need to add it back
    if vbscript_code.ends_with('\n') {
        updated_lines.join("\n") + "\n"
    } else {
        updated_lines.join("\n")
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use trim_margin::MarginTrimmable;

    use super::*;

    trait FullTrim {
        fn trim_margin_crlf(&self) -> String;
        fn trim_margin_unsafe(&self) -> String;
    }

    impl FullTrim for &str {
        fn trim_margin_crlf(&self) -> String {
            self.trim_margin().unwrap().replace('\n', "\r\n")
        }

        fn trim_margin_unsafe(&self) -> String {
            self.trim_margin().unwrap()
        }
    }

    #[test]
    fn test_capitalize_keywords() {
        let input = r#"class MyClass:END ClaSS"#;
        let expected = "Class MyClass:End Class";
        let actual = capitalize_keywords(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_capitalize_keywords_for() {
        let input = "FOr";
        let expected = "For";
        let actual = capitalize_keywords(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_capitalize_keywords_do_not_touch_comments() {
        let input = "'if this then else that end";
        let expected = "'if this then else that end";
        let actual = capitalize_keywords(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_capitalize_keywords_do_not_touch_strings() {
        let input = r#"foo = "if this then else that end""#;
        let expected = r#"foo = "if this then else that end""#;
        let actual = capitalize_keywords(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_capitalize_keywords_do_nothing() {
        let input = "If (a = 1) Then\n    MsgBox \"Hello, World!\"\nEnd If";
        let expected = "If (a = 1) Then\n    MsgBox \"Hello, World!\"\nEnd If";
        let actual = capitalize_keywords(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_capitalize_keywords_do_nothing_partial() {
        let input = r#"    document.write("<br></br>")"#;
        let expected = r#"    document.write("<br></br>")"#;
        let actual = capitalize_keywords(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_capitalize_keywords_do_nothing_escaped_quotes() {
        let input = r#"if \\" if ""#;
        let expected = r#"If \\" if ""#;
        let actual = capitalize_keywords(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_remove_chained_code() {
        let input = r#"
          |'comment before chain
          |test():test2():test3()'chain comment
          |'comment after chain
        "#
        .trim_margin_unsafe();
        let expected = r#"
          |'comment before chain
          |'chain comment
          |test()
          |test2()
          |test3()
          |'comment after chain
          "#
        .trim_margin_unsafe();
        let actual = remove_chained_code(&input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_remove_chained_code_if_with_colon() {
        let input = r#"
          |If a = 1 Then b = 2: c = 3: d = 4
        "#
        .trim_margin_unsafe();
        // no indentation yet at this point
        let expected = r#"
          |If a = 1 Then
          |b = 2
          |c = 3
          |d = 4
          |End If
        "#
        .trim_margin_unsafe();
        let actual = remove_chained_code(&input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_remove_chained_code_function_single_line() {
        let input = r#"
            |Private Function test() : if a > 10 then a = 0 : End If : End Function
            "#
        .trim_margin_unsafe();
        let expected = r#"
            |Private Function test()
            |if a > 10 then
            |a = 0
            |End If
            |End Function
            "#
        .trim_margin_unsafe();
        let actual = remove_chained_code(&input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_indentation() {
        let input = r#"
          |    If mode = 5 Then
          |Dim i
          |   For Each i In Lights
          |     Lights.state = 0
          |                       Next
          | End If
          |
        "#
        .trim_margin_unsafe();
        let expected = r#"
            |If mode = 5 Then
            |    Dim i
            |    For Each i In Lights
            |        Lights.state = 0
            |    Next
            |End If
            |
        "#
        .trim_margin_unsafe();
        let actual = fix_indentation(&input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_indentation_keep() {
        let input = r#"
            |' Hello World
            |Option Explicit
            |Class Test
            |End Class
        "#
        .trim_margin_unsafe();
        let expected = r#"
            |' Hello World
            |Option Explicit
            |Class Test
            |End Class
        "#
        .trim_margin()
        .unwrap();
        let actual = fix_indentation(&input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_indentation_for_each() {
        let input = r#"
            |For Each i In Lights
            |Lights.state = 0
            |Next
        "#
        .trim_margin_unsafe();
        let expected = r#"
            |For Each i In Lights
            |    Lights.state = 0
            |Next
        "#
        .trim_margin_unsafe();
        let actual = fix_indentation(&input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fmt_class() {
        let input = r#"class MyClass:END ClaSS"#;
        let expected = r#"
            |Class MyClass
            |End Class
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(input, FormatOptions::default());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fmt_select_case() {
        let input = r#"
            |select case strPerson
            |case "Alex"
            |WScript.Echo "We found Alex"
            |case "Jasper"
            |WScript.Echo "We found Jasper"
            |end select
            |
            "#
        .trim_margin_crlf();
        let expected = r#"
            |Select Case strPerson
            |    Case "Alex"
            |        WScript.Echo "We found Alex"
            |    Case "Jasper"
            |        WScript.Echo "We found Jasper"
            |End Select
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(&input, FormatOptions::default());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fmt_while() {
        let input = r#"
            |Dim Counter :  Counter = 10
            |While Counter < 15    ' Test value of Counter.
            |Counter = Counter + 1   ' Increment Counter.
            |document.write("The current value of the counter is : " & Counter)
            |document.write("<br></br>")
            |Wend ' While loop exits if Counter Value becomes 15.
            |
            "#
        .trim_margin_crlf();
        let expected = r#"
            |Dim Counter
            |Counter = 10
            |While Counter < 15    ' Test value of Counter.
            |    Counter = Counter + 1   ' Increment Counter.
            |    document.write("The current value of the counter is : " & Counter)
            |    document.write("<br></br>")
            |Wend ' While loop exits if Counter Value becomes 15.
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(&input, FormatOptions::default());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fmt_with() {
        let input = r#"
            |With plungerIM
            |.InitImpulseP swPlunger, IMPowerSetting, IMTime
            |.Random 0.8
            |End With
            |
            "#
        .trim_margin_crlf();
        let expected = r#"
            |With plungerIM
            |    .InitImpulseP swPlunger, IMPowerSetting, IMTime
            |    .Random 0.8
            |End With
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(&input, FormatOptions::default());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fmt_if_with_exit() {
        let input = r#"
            |If Err Then x = 0: Exit Sub
            |
            "#
        .trim_margin_crlf();
        let expected = r#"
            |If Err Then
            |    x = 0
            |    Exit Sub
            |End If
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(&input, FormatOptions::default());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fmt_function_single_line() {
        let input = r#"
            |Private Function test() : if a > 10 then a = 0 : End If : End Function
            |
            "#
        .trim_margin_crlf();
        let expected = r#"
            |Private Function test()
            |    If a > 10 Then
            |        a = 0
            |    End If
            |End Function
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(&input, FormatOptions::default());
        assert_eq!(expected, actual);
    }

    /*

     Public default Function init(primary, secondary, prim, sw, animate, isDropped)
     Set m_primary = primary
     End Function


    */

    #[test]
    fn test_fmt_public_default_function() {
        let input = r#"
            |Public default Function init()
            |Set m_primary = primary
            |End Function
            |
            "#
        .trim_margin_crlf();
        let expected = r#"
            |Public Default Function init()
            |    Set m_primary = primary
            |End Function
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(&input, FormatOptions::default());
        assert_eq!(expected, actual);
    }

    /*

           If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
        BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
        If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
    End If

    */

    // TODO fix this test
    #[ignore]
    #[test]
    fn test_fmt_nested_if() {
        let input = r#"
            |If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
            |BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
            |If ballpos > 0.65 Then Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
            |End If
            |End If
            |
            "#
        .trim_margin_crlf();
        let expected = r#"
            |If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
            |    BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
            |    If ballpos > 0.65 Then
            |        Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
            |    End If
            |End If
            |
            "#
        .trim_margin_crlf();
        let actual = fmt(&input, FormatOptions::default());
        assert_eq!(expected, actual);
    }
}

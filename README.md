# vbsfmt

VBScript standard formatter

## Install

Download the latest release for your operating system at https://github.com/francisdb/vbsfmt/releases, extract it and
if wanted copy or symlink the binary to `$HOME/bin` to put in on your path

### macOS

After extracting the archive you will have to remove the quarantine flag
through `System Settings / Privacy & Security / Allow Anyway button` or on the command line as shown below.

```
xattr -d com.apple.quarantine vbsfmt
```

## Running the integration tests

Make sure you populate the `testsctipts` folder, feel free to add more vbs files to the folder.

```bash
./testsctipts/populate.sh
```

Run the tests. They will also be included in the default `cargo test` run.

```bash
RUST_BACKTRACE=1 cargo test -- --nocapture try_formatting_all_vbs_files
```

## Attributions

This started as a port of https://gitlab.com/PDStig/vpin-workshop-code-formatter

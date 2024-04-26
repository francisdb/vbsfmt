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

## Attributions

This started as a port of https://gitlab.com/PDStig/vpin-workshop-code-formatter

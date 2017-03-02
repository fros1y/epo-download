# epo-download

## Usage

A command line tool for downloading patent documents from the EPO and saving as
a PDF. Various options can be listed by typing `--help`. Citations can be provided in a variety of formats, including some poorly formatted, but common, ways of representing a patent document.  These citations can be provided as a list on the command line or, if the csv flags are set, from a given column of a CSV file.

Requires a consumer and secret key from the EPO OPS developer console to be passed in
as command line parameters or in a file, defaulting to ~/.patent-api-config. The file should look like this:

`[EPO]
consumerKey=XXXXX
secretKey=XXXXXX
`

Credentials can be obtained by registering for a free account from https://developers.epo.org, selecting "My Apps" from the menu bar, and adding a new app. Free accounts have rate and download caps.  The `patent-api` package used attempts to rate limit all requests according to EPO usage guidance.

Assembly of PDFs requires the Ghostscript "gs" executable to be available in the PATH and able to write to into the working directory.

## Building

This has been testing on MacOS Sierra using the Haskell `stack` tool.  Provided that you have the Ghostscript intepreter installed, and I believe most MacOS and Linux distributions do, it should be sufficient to install `stack` per your system and run `stack install` in a copy of your system. Pull requests to make more portable and/or allow for builds on Windows/Linux welcome.

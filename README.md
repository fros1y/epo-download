# epo-ops-download

An alpha-quality set of tools for working with EPO patent data.

## EPODOC
Module with tools for parsing various patent formats into EPODOC format and converting into other formats.

## EPOOPS
Module with tools for querying various EPO OPS API services. Has some support
for rate limiting queries.  Does not support anonymous access.

## epo-download
A command line tool for downloading patent documents from the EPO and saving as
a PDF. Requires a consumer and secret key from the EPO OPS developer console to be passed in
as command line parameters.  Takes one patent document citation.

Requires Ghostscript "gs" executable to be available to assemble PDF.

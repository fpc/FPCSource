# Markdown parser examples

## Synopsis
This directory contains 3 markdown parser examples:

They are:

* demomd - simple markdown parser and html renderer.
* md2html - slightly more complete markdown-to-html converter/
* md2fpdoc - simple version of a markdown - to fpdoc converter.

## conversion to fpdoc 

The headers determine what is generated for a given section:

* Level 1 - contains the unit name.
* Level 2 - Start Element or Topic (use "topic: name")
* Level 3 - Parts of element/topic. Must have one of the following titles
  * short
  * descr or description
  * errors
  * seealso
  * example or examples

links must be rendered as \[text\]\(text\) or \[\]\(text\)

You can find a simple example in the [sample.md](sample.md) file.



This directory contains an extensible markdown parser.

Extensible means 2 things:

* You can add new block types by simply registering a processor for it.
  You can also override a block type's parser to provide custom behaviour.

* You can render the markdown to whatever format you want. 
  By default, rendering to html and fpdoc are supported. (LaTeX still planned)

Both renderers are demoed in the [demo](demo) directory.

While the commonmark spec has been used in the implementation, 
the parser makes no pretence at being fully commonmark compliant.

In particular, it should parse whatever commonmark defines, but the html
output may not be 100% the same, as the commonmark spec is overly pedantic
where it concerns whitespace usage in the output.
(although patches to improve the output are welcome)

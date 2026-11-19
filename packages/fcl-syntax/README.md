# fcl-syntax

fcl-syntax is a Free Pascal package for **syntax highlighting**.

A syntax highlighter reads source code (for example a Pascal program) and cuts
it into small pieces. Each piece is called a **token**. For each token, the
highlighter says what kind of text it is: a keyword, a number, a comment, a
string, and so on. You can then show each kind in its own colour, for example
on a web page.

The package is part of Free Pascal. You do not need to install anything else.

## Contents

1. [A first program](#a-first-program)
2. [Tokens](#tokens)
3. [Token kinds](#token-kinds)
4. [Tokens one by one](#tokens-one-by-one)
5. [Only some lines](#only-some-lines)
6. [Choosing the language by name](#choosing-the-language-by-name)
7. [Making HTML](#making-html)
8. [Categories](#categories)
9. [Supported languages](#supported-languages)
10. [Writing your own highlighter](#writing-your-own-highlighter)
11. [Examples and tests](#examples-and-tests)

## A first program

This program highlights one line of Pascal code and prints each token:

```pascal
program ex1;

{$mode objfpc}{$H+}

uses
  syntax.highlighter, syntax.pascal;

var
  Highlighter: TSyntaxHighlighter;
  Tokens: TSyntaxTokenArray;
  Token: TSyntaxToken;

begin
  Highlighter := TPascalSyntaxHighlighter.Create;
  try
    Tokens := Highlighter.Execute('if a > 10 then { big }');
    for Token in Tokens do
      Writeln(Token.Kind.ToString, ': "', Token.Text, '"');
  finally
    Highlighter.Free;
  end;
end.
```

The output is:

```
Keyword: "if"
Default: " "
Default: "a"
Default: " "
Symbol: ">"
Default: " "
Numbers: "10"
Default: " "
Keyword: "then"
Default: " "
Comment: "{ big }"
```

The steps are always the same:

1. Put `syntax.highlighter` in the `uses` clause. This unit contains the
   basic types.
2. Put the unit for your language in the `uses` clause, for example
   `syntax.pascal`.
3. Create the highlighter object.
4. Call `Execute` with the source code. You get an array of tokens.
5. Free the highlighter when you are done.

You can call `Execute` many times with the same highlighter object.

## Tokens

A token is a record of type `TSyntaxToken`. The most important fields are:

| Field  | Type                   | Meaning                                  |
|--------|------------------------|------------------------------------------|
| `Text` | `string`               | The text of the token, exactly as in the source. |
| `Kind` | `TSyntaxHighlightKind` | What kind of text this is. See below.     |
| `Line` | `Integer`              | The line where the token starts. The first line is 1. |

Spaces, tabs and line endings are also tokens. Normally they have the kind
`shDefault`. When you put the `Text` of all tokens together, you get the
original source code again.

A token can also have **categories**. See [Categories](#categories).

`Execute` returns a `TSyntaxTokenArray`. This is a normal dynamic array, so you
can use `Length`, `High` and `for ... in` with it.

## Token kinds

`TSyntaxHighlightKind` has these values. Not every language uses every value.

| Kind              | Used for                                           |
|-------------------|----------------------------------------------------|
| `shDefault`       | Normal text: names, spaces, line endings.          |
| `shInvalid`       | Text that is not valid.                            |
| `shSymbol`        | Punctuation, like `;` `(` `)` `:=`.                |
| `shKeyword`       | Reserved words, like `begin`, `if`, `class`.       |
| `shComment`       | Comments.                                          |
| `shDirective`     | Compiler directives and similar, like `{$mode objfpc}` or `#include`. |
| `shNumbers`       | Numbers, like `42`, `$FF`, `1.5e3`.                |
| `shCharacters`    | Character constants, like `'A'` or `#13`.          |
| `shStrings`       | Strings, like `'hello'`.                           |
| `shAssembler`     | Assembler code inside another language.            |
| `shRegex`         | Regular expressions.                               |
| `shInterpolation` | Code inside a string, like `${name}`.              |
| `shEscape`        | Escape sequences, like `\n`.                       |
| `shRawString`     | Raw strings, and code blocks in Markdown.          |
| `shTemplate`      | Template strings, like `` `text` `` in TypeScript. |
| `shLambda`        | Lambda expressions. No highlighter uses it at the moment. |
| `shOperator`      | Operators, like `+` `=` `and`.                     |
| `shError`         | Errors. In a diff: removed lines.                  |
| `shWarning`       | Warnings. In a diff: changed lines.                |
| `shAttribute`     | Attributes, types, or names of HTML attributes.    |
| `shSection`       | Section names, like `[main]` in an INI file or a heading in Markdown. |
| `shKey`           | Keys, like the name before `=` in an INI file.     |

`Kind.ToString` gives the name without `sh`, for example `Keyword`.

## Tokens one by one

`Execute` can also give you the tokens one by one, while it reads the source.
Then the highlighter does not make an array. This is useful for big files.

Pass a method as the second argument of `Execute`. The method must have this
type:

```pascal
TSyntaxTokenEvent = procedure(Sender: TObject; const aToken: TSyntaxToken) of object;
```

The highlighter calls the method once for every token, in the order of the
source. `Sender` is the highlighter.

If you want to stop early, call `Abort` in the method. `Execute` then stops,
and the `EAbort` exception goes to your code.

## Only some lines

Sometimes you only need a few lines, for example the lines that are visible
in an editor. Call `SetRange(aFrom, aTo)` before `Execute`. The highlighter
then only gives you the tokens that are on the lines `aFrom` to `aTo`. The
first line is 1.

* `-1` means "no limit". `SetRange(10, -1)` gives line 10 to the end.
  `SetRange(-1, -1)` gives all lines again. This is the default.
* A token can be longer than one line, for example a comment. If a part of the
  token is inside the range, you get the whole token.
* The highlighter always starts reading at the start of the source. This is
  necessary, because a comment or string can start before `aFrom`. It stops
  reading after line `aTo`.
* The range also works for the array that `Execute` returns.
* The range stays set until you call `SetRange` again.

This program prints the tokens of line 2:

```pascal
program ex5;

{$mode objfpc}{$H+}

uses
  SysUtils, syntax.highlighter, syntax.pascal;

type
  TPrinter = class
    procedure PrintToken(Sender: TObject; const aToken: TSyntaxToken);
  end;

procedure TPrinter.PrintToken(Sender: TObject; const aToken: TSyntaxToken);
begin
  Writeln(aToken.Line, ' ', aToken.Kind.ToString, ': "',
    StringReplace(aToken.Text, #10, '\n', [rfReplaceAll]), '"');
end;

const
  Source = 'begin'#10 + 'a := 1;'#10 + 'b := 2;'#10 + 'end.';

var
  Highlighter: TSyntaxHighlighter;
  Printer: TPrinter;

begin
  Printer := TPrinter.Create;
  Highlighter := TPascalSyntaxHighlighter.Create;
  try
    Highlighter.SetRange(2, 2);
    Highlighter.Execute(Source, @Printer.PrintToken);
  finally
    Highlighter.Free;
    Printer.Free;
  end;
end.
```

The output is:

```
2 Default: "a"
2 Default: " "
2 Symbol: ":="
2 Default: " "
2 Numbers: "1"
2 Symbol: ";"
2 Default: "\n"
```

## Choosing the language by name

Sometimes you only know the name of the language at run time, for example from
a file extension or from a Markdown code block. Then use the **registry**.

Every highlighter registers itself with one or more names. This happens when
its unit is loaded. So the unit must be in the `uses` clause of your program,
even if you do not use the class directly.

```pascal
program ex2;

{$mode objfpc}{$H+}

uses
  syntax.highlighter, syntax.python, syntax.c;

procedure ShowCode(const aLanguage, aCode: string);

var
  Highlighter: TSyntaxHighlighter;
  Token: TSyntaxToken;
begin
  if TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(aLanguage) = nil then
  begin
    Writeln('No highlighter for ', aLanguage);
    Exit;
  end;
  Highlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter(aLanguage);
  try
    for Token in Highlighter.Execute(aCode) do
      Writeln(Token.Kind.ToString, ': "', Token.Text, '"');
  finally
    Highlighter.Free;
  end;
end;

begin
  ShowCode('python', 'x = 1  # one');
  ShowCode('cobol', 'MOVE 1 TO X.');
end.
```

The registry has these methods:

| Method                              | What it does                                  |
|-------------------------------------|-----------------------------------------------|
| `FindSyntaxHighlighterClass(Name)`  | Returns the class, or `nil` if there is none. |
| `GetSyntaxHighlighterClass(Name)`   | Returns the class. Raises `ESyntaxHighlighter` if there is none. |
| `CreateSyntaxHighlighter(Name)`     | Creates a new highlighter object. Raises `ESyntaxHighlighter` if there is none. You must free the object. |

Always use lowercase names, like `'python'` or `'cpp'`. The list of names is in
[Supported languages](#supported-languages).

## Making HTML

The unit `syntax.htmlrender` turns tokens into HTML. Each token becomes a
`<span>` element with CSS classes.

```pascal
program ex3;

{$mode objfpc}{$H+}

uses
  syntax.highlighter, syntax.pascal, syntax.htmlrender;

var
  Highlighter: TSyntaxHighlighter;
  Renderer: THtmlSyntaxRenderer;
  Html: string;

begin
  Highlighter := TPascalSyntaxHighlighter.Create;
  Renderer := THtmlSyntaxRenderer.Create;
  try
    Renderer.Options := [hroNoDefaultSpan];
    Renderer.RenderTokensToString(Highlighter.Execute('x := 42;'), Html);
    Writeln(Html);
  finally
    Renderer.Free;
    Highlighter.Free;
  end;
end.
```

The output is (on one line):

```html
x <span class="symbol symbol-pascal">:=</span> <span class="numbers numbers-pascal">42</span><span class="symbol symbol-pascal">;</span>
```

Each span has two kinds of CSS class:

* The kind in lowercase, for example `symbol` or `numbers`.
* The kind plus the category, for example `numbers-pascal`. With this class you
  can use different colours for different languages on the same page.

The renderer also changes `<`, `>`, `&`, `"` and `'` into HTML entities, so the
code is shown correctly in the browser.

You must write the CSS yourself. Put the result inside `<pre>` so that spaces
and line endings are kept. A small example:

```css
pre .keyword  { color: navy; font-weight: bold; }
pre .comment  { color: green; font-style: italic; }
pre .strings  { color: maroon; }
pre .numbers  { color: purple; }
pre .symbol   { color: gray; }
```

The renderer has these methods and properties:

| Name                    | What it does                                        |
|-------------------------|-----------------------------------------------------|
| `RenderTokensToString`  | Puts all the HTML in one string.                    |
| `RenderTokens`          | Adds the HTML to a `TStrings`.                      |
| `Options`               | A set of options, see below.                        |
| `ExtraClasses`          | Extra CSS class names. They are added to every span. |

The options are:

| Option                     | Meaning                                        |
|----------------------------|------------------------------------------------|
| `hroNoDefaultSpan`         | Do not put a span around `shDefault` tokens. The HTML is smaller. |
| `hroPreserveLineStructure` | `RenderTokens` adds one line to the `TStrings` for each line of source code. Without this option, it adds everything as one line. |

## Categories

A category is an extra label on a token. It is a number. Each number has a
name, for example `pascal` or `python`.

Most highlighters give every token the category of their language. This is
useful when code of one language is inside another language. For example, the
HTML highlighter also highlights CSS in a `<style>` element and JavaScript in a
`<script>` element. The categories tell you which language each token belongs
to: HTML tokens have the category `HTML`, CSS tokens have `EmbeddedCSS`, and
JavaScript tokens have `EmbeddedJS`.

```pascal
// Is this token Pascal code?
if Token.HasCategory(TSyntaxHighlighter.GetRegisteredCategoryID('pascal')) then
  ...

// The names of all categories of the token, separated by spaces
Writeln(Token.CategoriesAsString);
```

Useful methods:

| Method                                         | What it does                          |
|------------------------------------------------|---------------------------------------|
| `Token.Categories`                             | Array with the category numbers.      |
| `Token.HasCategory(ID)`                        | `True` if the token has this category. |
| `Token.CategoriesAsString`                     | The category names, separated by spaces. |
| `TSyntaxHighlighter.GetRegisteredCategoryID(Name)` | The number for a name, or `-1`.   |
| `TSyntaxHighlighter.GetCategoryName(ID)`       | The name for a number.                |

If you do not want the language category, set the `DefaultCategory` property
of the highlighter to `0` before you call `Execute`.

## Supported languages

Each language has its own unit. The names in the last column are the names for
the registry.

| Language          | Unit                | Class                          | Names |
|-------------------|---------------------|--------------------------------|-------|
| Ada               | `syntax.ada`        | `TAdaSyntaxHighlighter`        | `ada` |
| Assembly          | `syntax.assembly`   | `TAssemblySyntaxHighlighter`   | `asm`, `nasm`, `gas`, `x86`, `arm` |
| Barye             | `syntax.barye`      | `TBaryeSyntaxHighlighter`      | `barye`, `bry` |
| Bash              | `syntax.bash`       | `TBashSyntaxHighlighter`       | `bash`, `sh`, `zsh` |
| BASIC             | `syntax.basic`      | `TBasicSyntaxHighlighter`      | `basic`, `vb`, `freebasic` |
| Batch files       | `syntax.batch`      | `TBatchSyntaxHighlighter`      | `batch`, `bat`, `cmd` |
| C                 | `syntax.c`          | `TCSyntaxHighlighter`          | `c` |
| C++               | `syntax.cpp`        | `TCppSyntaxHighlighter`        | `cpp`, `c++`, `cc`, `h`, `hpp` |
| C#                | `syntax.csharp`     | `TCSharpSyntaxHighlighter`     | `csharp`, `cs`, `c#` |
| CSS               | `syntax.css`        | `TCssSyntaxHighlighter`        | `css` |
| D                 | `syntax.d`          | `TDSyntaxHighlighter`          | `d` |
| Diff / patch      | `syntax.diff`       | `TDiffSyntaxHighlighter`       | `diff`, `patch` |
| Dockerfile        | `syntax.dockerfile` | `TDockerSyntaxHighlighter`     | `dockerfile` |
| Fortran           | `syntax.fortran`    | `TFortranSyntaxHighlighter`    | `fortran` |
| Go                | `syntax.go`         | `TGoSyntaxHighlighter`         | `go`, `golang` |
| HTML              | `syntax.html`       | `THtmlSyntaxHighlighter`       | `html`, `htm` |
| INI files         | `syntax.ini`        | `TIniSyntaxHighlighter`        | `ini`, `cfg`, `conf` |
| Java              | `syntax.java`       | `TJavaSyntaxHighlighter`       | `java` |
| JavaScript        | `syntax.javascript` | `TJavaScriptSyntaxHighlighter` | `js`, `javascript` |
| JSON              | `syntax.json`       | `TJsonSyntaxHighlighter`       | `json` |
| Kotlin            | `syntax.kotlin`     | `TKotlinSyntaxHighlighter`     | `kotlin`, `kt` |
| Lazarus/Delphi forms | `syntax.lfm`     | `TLfmSyntaxHighlighter`        | `lfm`, `dfm`, `xfm`, `fmx` |
| Lua               | `syntax.lua`        | `TLuaSyntaxHighlighter`        | `lua` |
| Makefile          | `syntax.makefile`   | `TMakeSyntaxHighlighter`       | `makefile`, `make` |
| Markdown          | `syntax.markdown`   | `TMarkdownSyntaxHighlighter`   | `markdown`, `md`, `mkd`, `mdown` |
| Modula-2          | `syntax.modula2`    | `TModulaSyntaxHighlighter`     | `modula-2`, `modula2` |
| Oberon            | `syntax.oberon`     | `TOberonSyntaxHighlighter`     | `oberon` |
| Pascal            | `syntax.pascal`     | `TPascalSyntaxHighlighter`     | `pascal`, `delphi`, `objectpascal` |
| Perl              | `syntax.perl`       | `TPerlSyntaxHighlighter`       | `perl`, `pl` |
| PHP               | `syntax.php`        | `TPhpSyntaxHighlighter`        | `php` |
| PowerShell        | `syntax.powershell` | `TPowerShellSyntaxHighlighter` | `powershell`, `ps1` |
| Python            | `syntax.python`     | `TPythonSyntaxHighlighter`     | `python`, `py` |
| reStructuredText  | `syntax.rst`        | `TRstSyntaxHighlighter`        | `rst`, `restructuredtext` |
| Ruby              | `syntax.ruby`       | `TRubySyntaxHighlighter`       | `ruby`, `rb` |
| Rust              | `syntax.rust`       | `TRustSyntaxHighlighter`       | `rust`, `rs` |
| SQL               | `syntax.sql`        | `TSqlSyntaxHighlighter`        | `sql`, `mysql`, `postgresql`, `sqlite`, `firebird`, `oracle`, `mssql`, `tsql` |
| Swift             | `syntax.swift`      | `TSwiftSyntaxHighlighter`      | `swift` |
| TOML              | `syntax.toml`       | `TTomlSyntaxHighlighter`       | `toml` |
| TypeScript        | `syntax.typescript` | `TTypeScriptSyntaxHighlighter` | `typescript`, `ts`, `tsx` |
| XML               | `syntax.xml`        | `TXmlSyntaxHighlighter`        | `xml`, `svg`, `xsd`, `lpi`, `lpk`, `lps` |
| YAML              | `syntax.yaml`       | `TYamlSyntaxHighlighter`       | `yaml`, `yml` |
| Zig               | `syntax.zig`        | `TZigSyntaxHighlighter`        | `zig` |

There are two more units. You only need them to write your own highlighter:

* `syntax.highlighter`: the base class `TSyntaxHighlighter`, the token types
  and the registry.
* `syntax.table`: base classes for highlighters that use a table of keywords,
  comment styles and string styles.

## Writing your own highlighter

To write a highlighter for a new language:

1. Make a class that inherits from `TSyntaxHighlighter`.
2. Override the class function `GetLanguages`. It returns the names of the
   language.
3. Override the protected method `DoExecute`. For each piece of text, call
   `AddToken(Text, Kind)`. Do not skip any text: also call `AddToken` for
   spaces and line endings. `Execute` uses the text of the tokens to count the
   lines.
4. Call the class procedure `Register` once, normally in the `initialization`
   section of the unit.

This small highlighter marks every word in capital letters as a keyword:

```pascal
program ex4;

{$mode objfpc}{$H+}

uses
  Types, syntax.highlighter;

type
  TShoutSyntaxHighlighter = class(TSyntaxHighlighter)
  protected
    class function GetLanguages: TStringDynArray; override;
    procedure DoExecute(const aSource: string); override;
  end;

class function TShoutSyntaxHighlighter.GetLanguages: TStringDynArray;
begin
  Result := ['shout'];
end;

procedure TShoutSyntaxHighlighter.DoExecute(const aSource: string);
var
  I, Start: Integer;
  Kind: TSyntaxHighlightKind;
begin
  I := 1;
  while I <= Length(aSource) do
  begin
    Start := I;
    if aSource[I] in ['A'..'Z'] then
    begin
      while (I <= Length(aSource)) and (aSource[I] in ['A'..'Z']) do
        Inc(I);
      Kind := shKeyword;
    end
    else
    begin
      while (I <= Length(aSource)) and not (aSource[I] in ['A'..'Z']) do
        Inc(I);
      Kind := shDefault;
    end;
    AddToken(Copy(aSource, Start, I - Start), Kind);
  end;
end;

var
  Highlighter: TSyntaxHighlighter;
  Token: TSyntaxToken;

begin
  TShoutSyntaxHighlighter.Register;
  Highlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter('shout');
  try
    for Token in Highlighter.Execute('say HELLO to me') do
      Writeln(Token.Kind.ToString, ': "', Token.Text, '"');
  finally
    Highlighter.Free;
  end;
end.
```

The output is:

```
Default: "say "
Keyword: "HELLO"
Default: " to me"
```

Many languages only differ in their keywords, comments and strings. For such a
language, inherit from `TTableSyntaxHighlighter` in the unit `syntax.table`
and override only the class function `Describe`. Look at `syntax.lua.pp` or
`syntax.java.pp` to see how this works.

## Examples and tests

* The `examples` directory has small demo programs, for example
  `demo_simple.pp`, `demo_htmlrender.pp` and `demo_categories.pp`.
* `examples/demo_bitmap.pp` draws a source file as PNG images. It uses
  fcl-image and the FreeType library. You choose the size of the images and
  the font on the command line. When an image is full, the program starts a
  new one. For `source.pp` it writes `source-001.png`, `source-002.png`, and
  so on, in the current directory:

  ```
  demo_bitmap -w 800 -h 600 -f "DejaVu Sans Mono" -s 10 source.pp
  ```

* `examples/demo_pdf.pp` writes a source file as a PDF document with A4 pages.
  It uses fcl-pdf. It adds as many pages as it needs. For `source.pp` it writes
  `source.pdf`:

  ```
  demo_pdf -f "DejaVu Sans Mono" -s 9 source.pp
  ```

  Both programs have these options:

  | Option        | Meaning                                                  |
  |---------------|----------------------------------------------------------|
  | `-w width`    | Width of the images in pixels. Default 800. Only `demo_bitmap`. |
  | `-h height`   | Height of the images in pixels. Default 600. Only `demo_bitmap`. |
  | `-f font`     | Font family name, or the name of a `.ttf` file. Use a monospace font. The default is `DejaVu Sans Mono`, or `Courier New` on Windows and macOS. |
  | `-s size`     | Font size in points. Default 10 for images and 9 for PDF. |
  | `-l language` | Language name, for example `python`. By default the program uses the file extension. |
  | `--page=N`    | Write only page N: `source-007.png`, or `source-007.pdf` with one page. The program highlights only the lines on that page (see [Only some lines](#only-some-lines)). |

  Lines that are too long for the image or the page continue on the next
  line. Tabs are 8 characters wide. The programs find fonts with the font
  cache of fcl-pdf (unit `fpTTF`), so both need fcl-pdf.
* The `tests` directory has the unit tests. Open `tests/testsyntax.lpi` in
  Lazarus, or compile `tests/testsyntax.lpr` with the `src` directory in the
  unit path. Run the program with `-a` to run all tests.

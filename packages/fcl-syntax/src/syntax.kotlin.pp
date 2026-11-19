{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Kotlin syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.kotlin;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TKotlinSyntaxHighlighter }

  TKotlinSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TKotlinSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('kotlin kt', 'as break class continue do else for fun if in interface is object package return super this throw try typealias typeof val var when while',
    'Any Boolean Byte Char Double Float Int Long Nothing Short String Unit true false null');
  LineComment(Result, '//');
  BlockComment(Result, '/*', '*/', True);
  StringForm(Result, '"""', '"""', #0, True);
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''', '\', False, False, shCharacters);
  StringForm(Result, '`', '`', #0);
  Result.Attribute := '@';
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Separators := True;
  Result.Numbers.Suffixes := 'uUlLfF';
end;

initialization
  TKotlinSyntaxHighlighter.Register;
end.

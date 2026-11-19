{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    TOML syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.toml;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TTomlSyntaxHighlighter }

  TTomlSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TTomlSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('toml', '', 'true false inf nan');
  Result.Flavor := lfToml;
  LineComment(Result, '#');
  StringForm(Result, '"""', '"""', '\', True);
  StringForm(Result, '''''''', '''''''', #0, True, False, shRawString);
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''', #0, False, False, shRawString);
  Result.IdentifierRest := Result.IdentifierRest + ['-'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.Separators := True;
end;

initialization
  TTomlSyntaxHighlighter.Register;
end.

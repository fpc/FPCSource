{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    TypeScript syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.typescript;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TTypeScriptSyntaxHighlighter }

  TTypeScriptSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TTypeScriptSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('typescript ts tsx',
    'abstract as async await break case catch class const constructor continue debugger declare default delete do else enum export extends finally for from function get if implements import in infer instanceof interface is keyof let module namespace new of override package private protected public readonly require return satisfies set static super switch this throw try type typeof using var void while with yield', 'any bigint boolean never number object string symbol unknown true false null undefined');
  LineComment(Result, '//');
  BlockComment(Result, '/*', '*/');
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''');
  StringForm(Result, '`', '`', '\', True, False, shTemplate);
  Result.Attribute := '@';
  Result.IdentifierStart := Result.IdentifierStart + ['$'];
  Result.IdentifierRest := Result.IdentifierRest + ['$'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.Separators := True;
  Result.Numbers.Suffixes := 'n';
end;

initialization
  TTypeScriptSyntaxHighlighter.Register;
end.

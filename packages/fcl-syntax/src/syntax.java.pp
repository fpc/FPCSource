{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Java syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.java;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TJavaSyntaxHighlighter }

  TJavaSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TJavaSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('java',
    'abstract assert break case catch class const continue default do else enum extends final finally for goto if implements import instanceof interface native new package private protected public return static strictfp super switch synchronized this throw throws transient try volatile while', 'boolean byte char double float int long short void true false null');
  LineComment(Result, '//');
  BlockComment(Result, '/*', '*/');
  StringForm(Result, '"""', '"""', '\', True);
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''', '\', False, False, shCharacters);
  Result.Attribute := '@';
  Result.IdentifierStart := Result.IdentifierStart + ['$'];
  Result.IdentifierRest := Result.IdentifierRest + ['$'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.LeadingOctal := True;
  Result.Numbers.HexExponent := True;
  Result.Numbers.Separators := True;
  Result.Numbers.Suffixes := 'lLfFdD';
end;

initialization
  TJavaSyntaxHighlighter.Register;
end.

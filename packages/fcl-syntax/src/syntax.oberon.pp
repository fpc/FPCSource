{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Oberon syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.oberon;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TOberonSyntaxHighlighter }

  TOberonSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TOberonSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('oberon',
    'ARRAY BEGIN BY CASE CONST DIV DO ELSE ELSIF END EXIT FOR IF IMPORT IN IS LOOP MOD MODULE OF OR POINTER PROCEDURE RECORD REPEAT RETURN THEN TO TYPE UNTIL VAR WHILE WITH',
    'BOOLEAN BYTE CHAR INTEGER LONGINT LONGREAL REAL SET SHORTINT TRUE FALSE NIL');
  BlockComment(Result, '(*', '*)', True);
  StringForm(Result, '"', '"', #0);
  StringForm(Result, '''', '''', #0);
  Result.Numbers.ExponentLetters := 'eED';
  Result.Numbers.TrailingBase := True;
end;

initialization
  TOberonSyntaxHighlighter.Register;
end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Modula-2 syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.modula2;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TModulaSyntaxHighlighter }

  TModulaSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TModulaSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('modula-2 modula2',
    'AND ARRAY BEGIN BY CASE CONST DEFINITION DIV DO ELSE ELSIF END EXCEPT EXIT EXPORT FINALLY FOR FORWARD FROM IF IMPLEMENTATION IMPORT IN LOOP MOD MODULE NOT OF OR PACKEDSET POINTER PROCEDURE QUALIFIED RECORD REM REPEAT RETRY RETURN SET THEN TO TYPE UNTIL VAR WHILE WITH', 'BITSET BOOLEAN CARDINAL CHAR COMPLEX INTEGER LONGCARD LONGCOMPLEX LONGINT LONGREAL PROC REAL SHORTCARD SHORTINT TRUE FALSE NIL');
  BlockComment(Result, '(*', '*)', True);
  StringForm(Result, '"', '"', #0);
  StringForm(Result, '''', '''', #0);
  Result.Numbers.ExponentLetters := 'eED';
  Result.Numbers.TrailingBase := True;
end;

initialization
  TModulaSyntaxHighlighter.Register;
end.

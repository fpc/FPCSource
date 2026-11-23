{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Python syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.python;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, SysUtils, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TPythonSyntaxHighlighter }

  TPythonSyntaxHighlighter = class(TByteSyntaxHighlighter)
  protected
    class function GetLanguages: TStringDynArray; override;
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  end;

implementation

const
  PythonKeywords = 'and as assert async await break class continue def del elif else except finally for from global if import in is '
    + 'lambda nonlocal not or pass raise return try while with yield';
  PythonBuiltins = 'True False None Ellipsis NotImplemented bool bytes bytearray complex dict float frozenset int list memoryview '
    + 'object range set slice str tuple type';

var
  PythonWords: TSyntaxWordTable;

class function TPythonSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['python', 'py'];
end;


procedure TPythonSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst, lPos, lPrefixLength: SizeInt;
  lQuote: char;
  lDelimiter, lValue: string;
  lKind: TSyntaxHighlightKind;
  lLineStart: boolean;
begin
  Start(aSource);
  lLineStart := True;
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    lKind := shDefault;
    lPos := FCursor;
    while (lPos <= FLimit) and (lPos - FCursor < 2) and (FText[lPos] in ['r', 'R', 'b', 'B', 'u', 'U', 'f', 'F', 't', 'T']) do
      Inc(lPos);
    lPrefixLength := lPos - FCursor;
    if Ch in ['"', ''''] then
      begin
      lPos := FCursor;
      lPrefixLength := 0;
      end;
    if (lPos <= FLimit) and (FText[lPos] in ['"', '''']) then
      begin
      lQuote := FText[lPos];
      lDelimiter := lQuote;
      if (lPos + 2 <= FLimit) and (FText[lPos + 1] = lQuote) and (FText[lPos + 2] = lQuote) then
        lDelimiter := lQuote + lQuote + lQuote;
      FCursor := lPos;
      // A raw prefix keeps backslashes, which still shield quotes
      Quoted(lDelimiter, lDelimiter, '\', Length(lDelimiter) = 3, False);
      lKind := shStrings;
      if (lPrefixLength > 0) and (Pos('r', LowerCase(Copy(FText, lFirst, lPrefixLength))) > 0) then
        lKind := shRawString;
      end
    else if Ch = '#' then
      begin
      LineEnd;
      lKind := shComment;
      end
    else if lLineStart and (Ch = '@') and (Ch(1) in NameStartChars) then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and (Ch in NameChars + ['.']) do
        Inc(FCursor);
      lKind := shAttribute;
      end
    else if Ch in NameStartChars then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and (Ch in NameChars) do
        Inc(FCursor);
      lValue := Copy(FText, lFirst, FCursor - lFirst);
      lKind := PythonWords.Lookup(lValue);
      end
    else if (Ch in ['0'..'9']) or ((Ch = '.') and (Ch(1) in ['0'..'9'])) then
      begin
      if At('0x') or At('0X') or At('0b') or At('0B') or At('0o') or At('0O') then
        begin
        Inc(FCursor, 2);
        while (FCursor <= FLimit) and (Ch in ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
          Inc(FCursor);
        end
      else
        begin
        while (FCursor <= FLimit) and (Ch in ['0'..'9', '_']) do
          Inc(FCursor);
        if (Ch = '.') and (Ch(1) <> '.') then
          begin
          Inc(FCursor);
          while (FCursor <= FLimit) and (Ch in ['0'..'9', '_']) do
            Inc(FCursor);
          end;
        if Ch in ['e', 'E'] then
          begin
          Inc(FCursor);
          if Ch in ['+', '-'] then
            Inc(FCursor);
          while (FCursor <= FLimit) and (Ch in ['0'..'9', '_']) do
            Inc(FCursor);
          end;
        end;
      if Ch in ['j', 'J'] then
        Inc(FCursor);
      lKind := shNumbers;
      end
    else if Ch in [' ', #9, #10, #13] then
      begin
      while (FCursor <= FLimit) and (Ch in [' ', #9, #10, #13]) do
        begin
        if Ch in [#10, #13] then
          lLineStart := True;
        Inc(FCursor);
        end;
      Emit(lFirst, lKind);
      Continue;
      end
    else
      begin
      if Ch in ['+', '-', '*', '/', '%', '=', '<', '>', '!', '&', '|', '^', '~', '@'] then
        lKind := shOperator
      else if Ch in ['(', ')', '[', ']', '{', '}', '.', ',', ':', ';'] then
        lKind := shSymbol;
      Unknown;
      end;
    Emit(lFirst, lKind);
    lLineStart := False;
    end;
  Finish;
end;

initialization
  PythonWords := TSyntaxWordTable.Create(True);
  PythonWords.AddWords(PythonKeywords, shKeyword);
  PythonWords.AddWords(PythonBuiltins, shKey);
  TPythonSyntaxHighlighter.Register;
finalization
  FreeAndNil(PythonWords);
end.

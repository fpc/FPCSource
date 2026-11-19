{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    YAML syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.yaml;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, SysUtils, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TYamlSyntaxHighlighter }

  TYamlSyntaxHighlighter = class(TByteSyntaxHighlighter)
  protected
    class function GetLanguages: TStringDynArray; override;
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  end;

implementation

class function TYamlSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['yaml', 'yml'];
end;


procedure TYamlSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst, lPos, lIndent, lBlockIndent, lCurrentIndent: SizeInt;
  lValue: string;
  lQuote: char;
  lKind: TSyntaxHighlightKind;
  lAtLineStart, lBlockActive, lInBlock: boolean;
begin
  Start(aSource);
  lAtLineStart := True;
  lBlockActive := False;
  lBlockIndent := 0;
  lIndent := 0;
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    lKind := shDefault;
    if lAtLineStart then
      begin
      lPos := FCursor;
      while (lPos <= FLimit) and (FText[lPos] = ' ') do
        Inc(lPos);
      lCurrentIndent := lPos - FCursor;
      lInBlock := lBlockActive and ((lPos > FLimit) or (FText[lPos] in [#10, #13]) or (lCurrentIndent > lBlockIndent));
      if lInBlock then
        begin
        LineEnd;
        if Ch = #13 then
          Inc(FCursor);
        if Ch = #10 then
          Inc(FCursor);
        Emit(lFirst, shStrings);
        Continue;
        end;
      lBlockActive := False;
      lIndent := lCurrentIndent;
      lAtLineStart := False;
      end;
    if Ch in [#10, #13] then
      begin
      if At(#13#10) then
        Inc(FCursor, 2)
      else
        Inc(FCursor);
      lAtLineStart := True;
      end
    else if Ch in [' ', #9] then
      begin
      while (FCursor <= FLimit) and (Ch in [' ', #9]) do
        Inc(FCursor);
      end
    else if (Ch = '#') and ((FCursor = 1) or (Ch(-1) in [' ', #9, #10, #13])) then
      begin
      LineEnd;
      lKind := shComment;
      end
    else if Ch in ['"', ''''] then
      begin
      lQuote := Ch;
      if lQuote = '"' then
        Quoted(lQuote, lQuote, '\', True, False)
      else
        Quoted(lQuote, lQuote, #0, True, True);
      lKind := shStrings;
      end
    else if Ch in ['|', '>'] then
      begin
      LineEnd;
      lKind := shSymbol;
      lBlockActive := True;
      lBlockIndent := lIndent;
      end
    else if Ch in ['&', '*', '!'] then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and not (Ch in [' ', #9, #10, #13, ',', '[', ']', '{', '}']) do
        Inc(FCursor);
      lKind := shAttribute;
      end
    else if Ch in [':', ',', '[', ']', '{', '}', '?'] then
      begin
      Inc(FCursor);
      lKind := shSymbol;
      end
    else if (At('---') or At('...')) and (Ch(3) in [' ', #9, #10, #13, #0]) then
      begin
      Inc(FCursor, 3);
      lKind := shDirective;
      end
    else if Ch = '%' then
      begin
      LineEnd;
      lKind := shDirective;
      end
    else if (Ch = '-') and (Ch(1) in [' ', #9, #10, #13]) then
      begin
      Inc(FCursor);
      lKind := shSymbol;
      end
    else
      begin
      Inc(FCursor);
      while FCursor <= FLimit do
        begin
        if Ch in [#10, #13, ',', '[', ']', '{', '}'] then
          Break;
        if (Ch = ':') and (Ch(1) in [' ', #9, #10, #13, #0]) then
          Break;
        if (Ch = '#') and (Ch(-1) in [' ', #9]) then
          Break;
        Inc(FCursor);
        end;
      lValue := Trim(Copy(FText, lFirst, FCursor - lFirst));
      if Ch = ':' then
        lKind := shKey
      else if WordsContain('true false True False TRUE FALSE null Null NULL ~', lValue) then
        lKind := shKey
      else if (lValue <> '') and (lValue[1] in ['0'..'9', '+', '-']) then
        lKind := shNumbers
      else
        lKind := shStrings;
      end;
    Emit(lFirst, lKind);
    end;
  Finish;
end;

initialization
  TYamlSyntaxHighlighter.Register;
end.

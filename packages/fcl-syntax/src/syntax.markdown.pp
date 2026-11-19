{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown source syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.markdown;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, SysUtils, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TMarkdownSyntaxHighlighter }

  TMarkdownSyntaxHighlighter = class(TByteSyntaxHighlighter)
  private
    // Searches in the current line that found nothing
    FFailRun: array of string;
    FFailFrom, FFailBefore: array of SizeInt;
    procedure InlineMarkup(aEol: SizeInt);
    function FindRun(const aRun: string; aFrom, aBefore: SizeInt): SizeInt;
  protected
    class function GetLanguages: TStringDynArray; override;
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  end;

implementation

const
  // Characters that start inline markup
  SpecialChars = ['\', '`', '*', '_', '~', '[', '!', '<', '|'];
  PunctuationChars = ['!'..'/', ':'..'@', '['..'`', '{'..'~'];

class function TMarkdownSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['markdown', 'md', 'mkd', 'mdown'];
end;


// Position of aRun at or after aFrom and before aBefore, 0 if not found
function TMarkdownSyntaxHighlighter.FindRun(const aRun: string; aFrom, aBefore: SizeInt): SizeInt;

var
  lPos: SizeInt;
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FFailRun) do
    if (FFailBefore[I] = aBefore) and (aFrom >= FFailFrom[I]) and (FFailRun[I] = aRun) then
      Exit;
  lPos := aFrom;
  while lPos + Length(aRun) <= aBefore do
    begin
    if (FText[lPos] = aRun[1]) and (Copy(FText, lPos, Length(aRun)) = aRun) then
      Exit(lPos);
    Inc(lPos);
    end;
  for I := 0 to High(FFailRun) do
    if FFailRun[I] = aRun then
      begin
      FFailFrom[I] := aFrom;
      FFailBefore[I] := aBefore;
      Exit;
      end;
  I := Length(FFailRun);
  SetLength(FFailRun, I + 1);
  SetLength(FFailFrom, I + 1);
  SetLength(FFailBefore, I + 1);
  FFailRun[I] := aRun;
  FFailFrom[I] := aFrom;
  FFailBefore[I] := aBefore;
end;


// Tokens for inline markup from the cursor up to aEol
procedure TMarkdownSyntaxHighlighter.InlineMarkup(aEol: SizeInt);

var
  lFirst, lClose, lCount: SizeInt;
  lMark: char;
  lRun: string;
begin
  while FCursor < aEol do
    begin
    lFirst := FCursor;
    if (Ch = '\') and (FCursor + 1 < aEol) and (Ch(1) in PunctuationChars) then
      begin
      Inc(FCursor, 2);
      Emit(lFirst, shEscape);
      end
    else if Ch = '`' then
      begin
      // A code span closes on a run of as many backticks
      lCount := 0;
      while (FCursor + lCount < aEol) and (Ch(lCount) = '`') do
        Inc(lCount);
      lRun := StringOfChar('`', lCount);
      lClose := FindRun(lRun, FCursor + lCount, aEol);
      if lClose > 0 then
        begin
        FCursor := lClose + lCount;
        Emit(lFirst, shStrings);
        end
      else
        begin
        Inc(FCursor, lCount);
        Emit(lFirst, shDefault);
        end;
      end
    else if (Ch in ['*', '_']) and not ((Ch = '_') and (FCursor > 1) and (FText[FCursor - 1] in ['A'..'Z', 'a'..'z', '0'..'9'])) then
      begin
      // **strong**, *emphasis*, ***both***
      lMark := Ch;
      lCount := 0;
      while (FCursor + lCount < aEol) and (Ch(lCount) = lMark) and (lCount < 3) do
        Inc(lCount);
      lRun := StringOfChar(lMark, lCount);
      lClose := 0;
      if (FCursor + lCount < aEol) and not (Ch(lCount) in [' ', #9]) then
        lClose := FindRun(lRun, FCursor + lCount + 1, aEol);
      if (lClose > 0) and not (FText[lClose - 1] in [' ', #9]) then
        begin
        FCursor := lClose + lCount;
        if lCount >= 2 then
          Emit(lFirst, shKeyword)
        else
          Emit(lFirst, shAttribute);
        end
      else
        begin
        Inc(FCursor, lCount);
        Emit(lFirst, shDefault);
        end;
      end
    else if (Ch = '~') and (Ch(1) = '~') then
      begin
      lClose := FindRun('~~', FCursor + 2, aEol);
      if lClose > 0 then
        begin
        FCursor := lClose + 2;
        Emit(lFirst, shComment);
        end
      else
        begin
        Inc(FCursor, 2);
        Emit(lFirst, shDefault);
        end;
      end
    else if (Ch = '[') and (Ch(1) = '[') then
      begin
      // Wiki link [[Page|words]]
      lClose := FindRun(']]', FCursor + 2, aEol);
      if lClose > 0 then
        begin
        FCursor := lClose + 2;
        Emit(lFirst, shKey);
        end
      else
        begin
        Inc(FCursor, 2);
        Emit(lFirst, shDefault);
        end;
      end
    else if (Ch = '[') or ((Ch = '!') and (Ch(1) = '[')) then
      begin
      // [words](address) and ![picture](address)
      if Ch = '!' then
        Inc(FCursor);
      lClose := FindRun(']', FCursor + 1, aEol);
      if lClose = 0 then
        begin
        Inc(FCursor);
        Emit(lFirst, shDefault);
        end
      else
        begin
        FCursor := lClose + 1;
        Emit(lFirst, shKey);
        if Ch = '(' then
          begin
          lFirst := FCursor;
          lClose := FindRun(')', FCursor + 1, aEol);
          if lClose > 0 then
            FCursor := lClose + 1
          else
            FCursor := aEol;
          Emit(lFirst, shStrings);
          end;
        end;
      end
    else if (Ch = '<') and (Ch(1) in ['A'..'Z', 'a'..'z', '/', '!']) then
      begin
      // <https://...> and <tag ...>
      lClose := FindRun('>', FCursor + 1, aEol);
      if lClose = 0 then
        begin
        Inc(FCursor);
        Emit(lFirst, shDefault);
        end
      else
        begin
        FCursor := lClose + 1;
        if (Pos('://', Copy(FText, lFirst, FCursor - lFirst)) > 0) or (Copy(FText, lFirst + 1, 7) = 'mailto:') then
          Emit(lFirst, shStrings)
        else
          Emit(lFirst, shDirective);
        end;
      end
    else if Ch = '|' then
      begin
      Inc(FCursor);
      Emit(lFirst, shSymbol);
      end
    else
      begin
      Unknown;
      while (FCursor < aEol) and not (Ch in SpecialChars) do
        Unknown;
      Emit(lFirst, shDefault);
      end;
    end;
  FCursor := aEol;
end;


procedure TMarkdownSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst, lEol, lIndent, lCount: SizeInt;
  lPos: Integer;
  lFence, lLine, lBare: string;
  lRuleChar: char;
  lRuled: boolean;
begin
  Start(aSource);
  FFailRun := nil;
  FFailFrom := nil;
  FFailBefore := nil;
  lFence := '';
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    lEol := FCursor;
    while (lEol <= FLimit) and not (FText[lEol] in [#10, #13]) do
      Inc(lEol);
    lLine := Copy(FText, FCursor, lEol - FCursor);
    lBare := TrimLeft(lLine);
    lIndent := Length(lLine) - Length(lBare);
    if lFence <> '' then
      begin
      // Inside a fence up to and including the closing line
      FCursor := lEol;
      Emit(lFirst, shRawString);
      if (lIndent < 4) and (Copy(lBare, 1, Length(lFence)) = lFence) and (Trim(Copy(lBare, Length(lFence) + 1, MaxInt)) = '') then
        lFence := '';
      end
    else if (lIndent < 4) and ((Copy(lBare, 1, 3) = '```') or (Copy(lBare, 1, 3) = '~~~')) then
      begin
      lCount := 0;
      while (lCount < Length(lBare)) and (lBare[lCount + 1] = lBare[1]) do
        Inc(lCount);
      lFence := StringOfChar(lBare[1], lCount);
      FCursor := lEol;
      Emit(lFirst, shRawString);
      end
    else if (lIndent < 4) and (lBare <> '') and (lBare[1] = '#') then
      begin
      lCount := 0;
      while (lCount < Length(lBare)) and (lBare[lCount + 1] = '#') do
        Inc(lCount);
      FCursor := lEol;
      if (lCount <= 6) and ((lCount = Length(lBare)) or (lBare[lCount + 1] in [' ', #9])) then
        Emit(lFirst, shSection)
      else
        begin
        FCursor := lFirst;
        InlineMarkup(lEol);
        end;
      end
    else
      begin
      // A rule: three or more of one of - * _ = and nothing else
      lRuled := False;
      if (lIndent < 4) and (lBare <> '') and (lBare[1] in ['-', '*', '_', '=']) then
        begin
        lRuleChar := lBare[1];
        lCount := 0;
        lRuled := True;
        for lPos := 1 to Length(lBare) do
          if lBare[lPos] = lRuleChar then
            Inc(lCount)
          else if not (lBare[lPos] in [' ', #9]) then
            lRuled := False;
        lRuled := lRuled and (lCount >= 3);
        end;
      if lRuled then
        begin
        FCursor := lEol;
        Emit(lFirst, shSymbol);
        end
      else
        begin
        Inc(FCursor, lIndent);
        Emit(lFirst, shDefault);
        lFirst := FCursor;
        if Ch = '>' then
          begin
          FCursor := lEol;
          Emit(lFirst, shComment);
          end
        else
          begin
          // List mark - * + or 1. 1) and a space, then a task box
          if (Ch in ['-', '*', '+']) and (Ch(1) in [' ', #9]) then
            Inc(FCursor)
          else if Ch in ['0'..'9'] then
            begin
            lCount := 0;
            while (lCount < 9) and (Ch(lCount) in ['0'..'9']) do
              Inc(lCount);
            if (Ch(lCount) in ['.', ')']) and (Ch(lCount + 1) in [' ', #9]) then
              Inc(FCursor, lCount + 1);
            end;
          if FCursor > lFirst then
            begin
            Emit(lFirst, shSymbol);
            lFirst := FCursor;
            while (FCursor < lEol) and (Ch in [' ', #9]) do
              Inc(FCursor);
            Emit(lFirst, shDefault);
            if (Ch = '[') and (Ch(1) in [' ', 'x', 'X']) and (Ch(2) = ']') then
              begin
              lFirst := FCursor;
              Inc(FCursor, 3);
              Emit(lFirst, shSymbol);
              end;
            end;
          InlineMarkup(lEol);
          end;
        end;
      end;
    lFirst := FCursor;
    while (FCursor <= FLimit) and (Ch in [#10, #13]) do
      Inc(FCursor);
    Emit(lFirst, shDefault);
    end;
  Finish;
end;

initialization
  TMarkdownSyntaxHighlighter.Register;
end.

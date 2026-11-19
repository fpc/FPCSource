{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    reStructuredText source syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.rst;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, System.StrUtils, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, SysUtils, StrUtils, syntax.highlighter, syntax.table;
  {$ENDIF}

type
  TRstBlock = (rbNone, rbRaw, rbComment);

  { TRstSyntaxHighlighter }

  TRstSyntaxHighlighter = class(TByteSyntaxHighlighter)
  private
    FLineStart, FEol, FPending: SizeInt;
    // Per inline markup kind: the search for its end mark that found nothing
    FFailFrom, FFailEol: array[0..4] of SizeInt;
    function StartOk(aPos: SizeInt): boolean;
    function EndOk(aPos: SizeInt): boolean;
    function RoleEnd(aPos: SizeInt; aPrefix: boolean): SizeInt;
    function AfterEnd(aKind: integer; aMarkPos, aMarkLen: SizeInt): SizeInt;
    function FindEnd(aKind: integer; const aMark: string; aFrom: SizeInt; out aMarkPos: SizeInt): SizeInt;
    function FieldEnd(aPos: SizeInt): SizeInt;
    function Adornment(aFirst, aLast, aLeast: SizeInt): boolean;
    procedure Token(aUpto: SizeInt; aKind: TSyntaxHighlightKind);
    procedure InlineMarkup;
    procedure Explicit(aDots: SizeInt; out aBody: TRstBlock; out aOptions: boolean);
  protected
    class function GetLanguages: TStringDynArray; override;
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  end;

implementation

const
  SpaceChars = [' ', #9];
  AlnumChars = ['A'..'Z', 'a'..'z', '0'..'9'];
  // Characters of a role or directive name, including a domain prefix
  RoleNameChars = ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '+', '-', ':'];
  // Characters allowed right before inline markup
  BeforeChars = [' ', #9, '-', ':', '/', '''', '"', '<', '(', '[', '{'];
  // Characters allowed right after inline markup
  AfterChars = [' ', #9, '-', '.', ',', ':', ';', '!', '?', '\', '/', '''', '"', ')', ']', '}', '>'];
  PunctuationChars = ['!'..'/', ':'..'@', '['..'`', '{'..'~'];
  // Directives whose body is shown as written
  RawBodies: array[0..13] of string = ('code', 'code-block', 'sourcecode', 'math', 'raw', 'parsed-literal',
    'csv-table', 'doctest', 'testcode', 'testoutput', 'literalinclude', 'include', 'highlight', 'productionlist');
  // Directives whose argument is a path or a language name
  PathArgs: array[0..8] of string = ('code', 'code-block', 'sourcecode', 'image', 'figure', 'include',
    'literalinclude', 'highlight', 'raw');

class function TRstSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['rst', 'restructuredtext'];
end;


// True if inline markup may start at aPos
function TRstSyntaxHighlighter.StartOk(aPos: SizeInt): boolean;

begin
  Result := (aPos <= FLineStart) or (FText[aPos - 1] in BeforeChars);
end;


// True if inline markup may end before aPos
function TRstSyntaxHighlighter.EndOk(aPos: SizeInt): boolean;

begin
  Result := (aPos >= FEol) or (FText[aPos] in AfterChars);
end;


// At aPos a colon: as prefix the position of the backquote after :name:, else the position after :name:; 0 if none
function TRstSyntaxHighlighter.RoleEnd(aPos: SizeInt; aPrefix: boolean): SizeInt;

var
  lPos: SizeInt;
begin
  Result := 0;
  lPos := aPos + 1;
  while (lPos < FEol) and (lPos - aPos <= 40) and (FText[lPos] in RoleNameChars) do
    begin
    if (FText[lPos] = ':') and (lPos > aPos + 1) then
      if aPrefix and (lPos + 1 < FEol) and (FText[lPos + 1] = '`') then
        Exit(lPos + 1)
      else if not aPrefix and EndOk(lPos + 1) then
        Exit(lPos + 1);
    Inc(lPos);
    end;
end;


// End of the markup for an end mark of length aMarkLen at aMarkPos, with a trailing _ or :role:; 0 if it cannot end there
function TRstSyntaxHighlighter.AfterEnd(aKind: integer; aMarkPos, aMarkLen: SizeInt): SizeInt;

var
  lPos: SizeInt;
begin
  lPos := aMarkPos + aMarkLen;
  if aKind in [3, 4] then
    if (lPos < FEol) and (FText[lPos] = '_') then
      begin
      Inc(lPos);
      if (lPos < FEol) and (FText[lPos] = '_') then
        Inc(lPos);
      end
    else if (aKind = 3) and (lPos < FEol) and (FText[lPos] = ':') and (RoleEnd(lPos, False) > 0) then
      lPos := RoleEnd(lPos, False);
  if EndOk(lPos) then
    Result := lPos
  else if EndOk(aMarkPos + aMarkLen) then
    Result := aMarkPos + aMarkLen
  else
    Result := 0;
end;


// End of the markup closed by aMark at or after aFrom in this line, aMarkPos the mark position; 0 if not found
function TRstSyntaxHighlighter.FindEnd(aKind: integer; const aMark: string; aFrom: SizeInt; out aMarkPos: SizeInt): SizeInt;

var
  lLen: SizeInt;
begin
  Result := 0;
  aMarkPos := 0;
  if (FFailEol[aKind] = FEol) and (aFrom >= FFailFrom[aKind]) then
    Exit;
  lLen := Length(aMark);
  aMarkPos := aFrom;
  while aMarkPos + lLen <= FEol do
    begin
    if (FText[aMarkPos] = aMark[1]) and ((lLen = 1) or (FText[aMarkPos + 1] = aMark[2]))
       and not (FText[aMarkPos - 1] in [' ', #9, '\']) then
      begin
      Result := AfterEnd(aKind, aMarkPos, lLen);
      if Result > 0 then
        Exit;
      end;
    Inc(aMarkPos);
    end;
  aMarkPos := 0;
  FFailEol[aKind] := FEol;
  FFailFrom[aKind] := aFrom;
end;


// At aPos a colon starting a line: the position after the field name and its space, 0 if not a field
function TRstSyntaxHighlighter.FieldEnd(aPos: SizeInt): SizeInt;

var
  lPos: SizeInt;
begin
  Result := 0;
  if (aPos + 1 >= FEol) or (FText[aPos + 1] in SpaceChars) then
    Exit;
  lPos := aPos + 1;
  while (lPos < FEol) and (lPos - aPos <= 80) and (FText[lPos] <> ':') do
    Inc(lPos);
  if (lPos < FEol) and (FText[lPos] = ':') and (lPos > aPos + 1) and not (FText[lPos - 1] in SpaceChars)
     and ((lPos + 1 = FEol) or (FText[lPos + 1] in SpaceChars)) then
    Result := lPos + 1;
end;


// True if the text from aFirst to aLast-1 repeats one punctuation character at least aLeast times
function TRstSyntaxHighlighter.Adornment(aFirst, aLast, aLeast: SizeInt): boolean;

var
  I: Integer;
  lCount: SizeInt;
begin
  Result := False;
  while (aLast > aFirst) and (FText[aLast - 1] in SpaceChars) do
    Dec(aLast);
  if (aLast - aFirst < aLeast) or not (FText[aFirst] in PunctuationChars) then
    Exit;
  lCount := 0;
  for I := aFirst to aLast - 1 do
    if FText[I] <> FText[aFirst] then
      Exit
    else
      Inc(lCount);
  Result := lCount >= aLeast;
end;


// Emit the pending plain text, then the text from the cursor up to aUpto as aKind
procedure TRstSyntaxHighlighter.Token(aUpto: SizeInt; aKind: TSyntaxHighlightKind);

var
  lFirst: SizeInt;
begin
  Emit(FPending, shDefault);
  lFirst := FCursor;
  FCursor := aUpto;
  Emit(lFirst, aKind);
  FPending := FCursor;
end;


// Tokens for inline markup from the cursor to the end of the line
procedure TRstSyntaxHighlighter.InlineMarkup;

var
  lPos, lMarkPos, lEnd, lCount: SizeInt;

  function Solid(aIndex: SizeInt): boolean;
  begin
    Result := (aIndex < FEol) and not (FText[aIndex] in SpaceChars);
  end;

begin
  while FCursor < FEol do
    begin
    lPos := FCursor;
    lEnd := 0;
    if (Ch = '\') and Solid(lPos + 1) and (FText[lPos + 1] < #128) then
      Token(lPos + 2, shEscape)
    else if (Ch = '`') and (Ch(1) = '`') and StartOk(lPos) and Solid(lPos + 2) then
      begin
      lEnd := FindEnd(0, '``', lPos + 3, lMarkPos);
      if lEnd > 0 then
        Token(lEnd, shStrings)
      else
        Inc(FCursor, 2);
      end
    else if (Ch = '*') and (Ch(1) = '*') and StartOk(lPos) and Solid(lPos + 2) then
      begin
      lEnd := FindEnd(1, '**', lPos + 3, lMarkPos);
      if lEnd > 0 then
        Token(lEnd, shKeyword)
      else
        Inc(FCursor, 2);
      end
    else if (Ch = '*') and StartOk(lPos) and Solid(lPos + 1) then
      begin
      lEnd := FindEnd(2, '*', lPos + 2, lMarkPos);
      if lEnd > 0 then
        Token(lEnd, shAttribute)
      else
        Inc(FCursor);
      end
    else if (Ch = ':') and StartOk(lPos) then
      begin
      // :role:`text`
      lCount := RoleEnd(lPos, True);
      if (lCount > 0) and Solid(lCount + 1) then
        lEnd := FindEnd(3, '`', lCount + 2, lMarkPos);
      if lEnd > 0 then
        begin
        Token(lCount, shDirective);
        Token(lEnd, shStrings);
        end
      else
        Inc(FCursor);
      end
    else if (Ch = '`') and StartOk(lPos) and Solid(lPos + 1) then
      begin
      // `text`_ reference, `text`:role: or `text`
      lEnd := FindEnd(3, '`', lPos + 2, lMarkPos);
      if lEnd = 0 then
        Inc(FCursor)
      else if (lMarkPos + 1 < FEol) and (FText[lMarkPos + 1] = '_') then
        Token(lEnd, shKey)
      else if lEnd > lMarkPos + 1 then
        begin
        Token(lMarkPos + 1, shStrings);
        Token(lEnd, shDirective);
        end
      else
        Token(lEnd, shStrings);
      end
    else if (Ch = '_') and (Ch(1) = '`') and StartOk(lPos) and Solid(lPos + 2) then
      begin
      // _`inline target`
      lEnd := FindEnd(3, '`', lPos + 3, lMarkPos);
      if lEnd > 0 then
        Token(lEnd, shKey)
      else
        Inc(FCursor, 2);
      end
    else if Ch = '|' then
      begin
      // |substitution| or a table or line block bar
      if StartOk(lPos) and Solid(lPos + 1) then
        lEnd := FindEnd(4, '|', lPos + 2, lMarkPos);
      if lEnd > 0 then
        Token(lEnd, shKey)
      else
        Token(lPos + 1, shSymbol);
      end
    else if (Ch = '[') and StartOk(lPos) then
      begin
      // [1]_ [#]_ [*]_ [Citation]_
      lMarkPos := lPos + 1;
      while (lMarkPos < FEol) and (lMarkPos - lPos <= 40) and (FText[lMarkPos] in AlnumChars + ['#', '*', '_', '.', '-']) do
        Inc(lMarkPos);
      if (lMarkPos > lPos + 1) and (lMarkPos + 1 < FEol) and (FText[lMarkPos] = ']') and (FText[lMarkPos + 1] = '_') and EndOk(lMarkPos + 2) then
        Token(lMarkPos + 2, shKey)
      else
        Inc(FCursor);
      end
    else if Ch in AlnumChars then
      begin
      if StartOk(lPos) and (At('https://') or At('http://') or At('mailto:')) then
        begin
        lMarkPos := lPos;
        while (lMarkPos < FEol) and not (FText[lMarkPos] in [' ', #9, '<', '>', '`']) do
          Inc(lMarkPos);
        Token(lMarkPos, shStrings);
        end
      else
        begin
        // A word, a reference when followed by _ or __
        lMarkPos := lPos;
        while (lMarkPos < FEol) and ((FText[lMarkPos] in AlnumChars) or (FText[lMarkPos] >= #128)
              or ((FText[lMarkPos] in ['-', '.', '+', '_']) and (lMarkPos + 1 < FEol) and (FText[lMarkPos + 1] in AlnumChars))) do
          Inc(lMarkPos);
        lCount := lMarkPos;
        if (lCount < FEol) and (FText[lCount] = '_') then
          begin
          Inc(lCount);
          if (lCount < FEol) and (FText[lCount] = '_') then
            Inc(lCount);
          end;
        if (lCount > lMarkPos) and StartOk(lPos) and EndOk(lCount) then
          Token(lCount, shKey)
        else
          FCursor := lMarkPos;
        end;
      end
    else
      Unknown;
    end;
  FCursor := FEol;
  Emit(FPending, shDefault);
  FPending := FCursor;
end;


// Explicit markup line with its .. at aDots; aBody tells what the lines indented under it are
procedure TRstSyntaxHighlighter.Explicit(aDots: SizeInt; out aBody: TRstBlock; out aOptions: boolean);

var
  lStart, lPos: SizeInt;
  lName: string;
begin
  aBody := rbNone;
  aOptions := False;
  FCursor := aDots + 2;
  while (FCursor < FEol) and (Ch in SpaceChars) do
    Inc(FCursor);
  lStart := FCursor;
  FCursor := aDots;
  if (lStart < FEol) and (FText[lStart] = '[') then
    begin
    lPos := lStart + 1;
    while (lPos < FEol) and (FText[lPos] <> ']') do
      Inc(lPos);
    if lPos < FEol then
      begin
      Token(lPos + 1, shKey);
      InlineMarkup;
      Exit;
      end;
    end
  else if (lStart < FEol) and (FText[lStart] = '_') then
    begin
    // .. _name: address, .. _`a name`: address, .. __: address
    lPos := lStart + 1;
    if (lPos < FEol) and (FText[lPos] = '`') then
      begin
      Inc(lPos);
      while (lPos < FEol) and not ((FText[lPos] = '`') and (lPos + 1 < FEol) and (FText[lPos + 1] = ':')) do
        Inc(lPos);
      Inc(lPos);
      end
    else
      while (lPos < FEol) and not ((FText[lPos] = ':') and ((lPos + 1 = FEol) or (FText[lPos + 1] in SpaceChars))) do
        Inc(lPos);
    if lPos < FEol then
      begin
      Token(lPos + 1, shKey);
      Token(FEol, shStrings);
      Emit(FPending, shDefault);
      FPending := FCursor;
      Exit;
      end;
    end
  else if (lStart < FEol) and (FText[lStart] = '|') then
    begin
    // .. |name| directive:: argument
    lPos := lStart + 1;
    while (lPos < FEol) and (FText[lPos] <> '|') do
      Inc(lPos);
    if lPos < FEol then
      begin
      Token(lPos + 1, shKey);
      while (FCursor < FEol) and (Ch in SpaceChars) do
        Inc(FCursor);
      lStart := FCursor;
      end;
    end;
  // Directive name::
  lPos := lStart;
  while (lPos + 1 < FEol) and (lPos - lStart <= 60) and (FText[lPos] in RoleNameChars) do
    begin
    if (lPos > lStart) and (FText[lPos] = ':') and (FText[lPos + 1] = ':') and ((lPos + 2 = FEol) or (FText[lPos + 2] in SpaceChars)) then
      begin
      lName := LowerCase(Copy(FText, lStart, lPos - lStart));
      Token(lPos + 2, shDirective);
      if IndexText(lName, PathArgs) >= 0 then
        begin
        Token(FEol, shStrings);
        Emit(FPending, shDefault);
        FPending := FCursor;
        end
      else
        InlineMarkup;
      if IndexText(lName, RawBodies) >= 0 then
        begin
        aBody := rbRaw;
        aOptions := True;
        end;
      Exit;
      end;
    Inc(lPos);
    end;
  // Anything else after .. is a comment, including the lines indented under it
  if FCursor = aDots then
    begin
    Token(FEol, shComment);
    aBody := rbComment;
    end
  else
    InlineMarkup;
end;


procedure TRstSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst, lIndent, lBase, lNext, lNextEol, lCount, lPos, lBreaks: SizeInt;
  lLine, lBare: string;
  lBody: TRstBlock;
  lOptions, lDone: boolean;
begin
  Start(aSource);
  FillChar(FFailEol, SizeOf(FFailEol), 0);
  FillChar(FFailFrom, SizeOf(FFailFrom), 0);
  lBody := rbNone;
  lOptions := False;
  lBase := 0;
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    FLineStart := lFirst;
    FPending := lFirst;
    FEol := FCursor;
    while (FEol <= FLimit) and not (FText[FEol] in [#10, #13]) do
      Inc(FEol);
    lLine := Copy(FText, FCursor, FEol - FCursor);
    lBare := TrimLeft(lLine);
    lIndent := Length(lLine) - Length(lBare);
    lDone := False;
    if lBody <> rbNone then
      if Trim(lLine) = '' then
        begin
        FCursor := FEol;
        Emit(lFirst, shDefault);
        lOptions := False;
        lDone := True;
        end
      else if lIndent > lBase then
        begin
        FCursor := lFirst + lIndent;
        if (lBody = rbRaw) and lOptions and (lBare[1] = ':') and (FieldEnd(FCursor) > 0) then
          begin
          Token(FieldEnd(FCursor), shAttribute);
          FCursor := FEol;
          Emit(FPending, shDefault);
          end
        else
          begin
          lOptions := False;
          FCursor := FEol;
          if lBody = rbRaw then
            Emit(lFirst, shRawString)
          else
            Emit(lFirst, shComment);
          end;
        lDone := True;
        end
      else
        lBody := rbNone;
    if not lDone then
      begin
      // The next line, for a title underline
      lNext := FEol;
      if (lNext <= FLimit) and (FText[lNext] = #13) then
        Inc(lNext);
      if (lNext <= FLimit) and (FText[lNext] = #10) then
        Inc(lNext);
      lNextEol := lNext;
      while (lNextEol <= FLimit) and not (FText[lNextEol] in [#10, #13]) do
        Inc(lNextEol);
      if Trim(lLine) = '' then
        begin
        FCursor := FEol;
        Emit(lFirst, shDefault);
        end
      else if (Copy(lBare, 1, 2) = '..') and ((Length(lBare) = 2) or (lBare[3] in SpaceChars)) then
        begin
        Explicit(lFirst + lIndent, lBody, lOptions);
        lBase := lIndent;
        end
      else if (lIndent = 0) and Adornment(lFirst, FEol, 2) then
        begin
        FCursor := FEol;
        Emit(lFirst, shSymbol);
        end
      else if (lBare[1] in ['=', '-', '+']) and (Length(lBare) >= 3)
              and (Trim(StringReplace(StringReplace(StringReplace(lBare, '=', '', [rfReplaceAll]), '-', '', [rfReplaceAll]), '+', '', [rfReplaceAll])) = '') then
        begin
        // Table rule: ====  ==== or +----+----+
        FCursor := FEol;
        Emit(lFirst, shSymbol);
        end
      else if (lNext > FEol) and (lNextEol > lNext) and Adornment(lNext, lNextEol, 2) then
        begin
        FCursor := FEol;
        Emit(lFirst, shSection);
        end
      else if (Copy(lBare, 1, 4) = '>>> ') or (TrimRight(lBare) = '>>>') then
        begin
        FCursor := FEol;
        Emit(lFirst, shRawString);
        end
      else
        begin
        FCursor := lFirst + lIndent;
        // List mark - * + or 1. #. a) (i) and a space, or a field
        if (Ch in ['-', '*', '+']) and ((FCursor + 1 = FEol) or (Ch(1) in SpaceChars)) then
          Token(FCursor + 1, shSymbol)
        else if Ch = ':' then
          begin
          if FieldEnd(FCursor) > 0 then
            Token(FieldEnd(FCursor), shAttribute);
          end
        else
          begin
          lPos := FCursor;
          if FText[lPos] = '(' then
            Inc(lPos);
          lCount := lPos;
          if (lCount < FEol) and (FText[lCount] = '#') then
            Inc(lCount)
          else if (lCount < FEol) and (FText[lCount] in ['0'..'9']) then
            while (lCount < FEol) and (lCount - lPos < 9) and (FText[lCount] in ['0'..'9']) do
              Inc(lCount)
          else if (lCount + 1 < FEol) and (FText[lCount] in ['A'..'Z', 'a'..'z']) and (FText[lCount + 1] in ['.', ')']) then
            Inc(lCount);
          if (lCount > lPos) and (lCount < FEol)
             and (((lPos > FCursor) and (FText[lCount] = ')')) or ((lPos = FCursor) and (FText[lCount] in ['.', ')'])))
             and ((lCount + 1 = FEol) or (FText[lCount + 1] in SpaceChars)) then
            Token(lCount + 1, shSymbol);
          end;
        InlineMarkup;
        end;
      // A line ending in :: has a literal block indented under it
      if (lBody = rbNone) and (Copy(TrimRight(lBare), Length(TrimRight(lBare)) - 1, 2) = '::')
         and not ((Copy(lBare, 1, 2) = '..') and ((Length(lBare) = 2) or (lBare[3] in SpaceChars))) then
        begin
        lBody := rbRaw;
        lOptions := False;
        lBase := lIndent;
        end;
      end;
    FCursor := FEol;
    lFirst := FCursor;
    lBreaks := 0;
    while (FCursor <= FLimit) and (Ch in [#10, #13]) do
      begin
      if (Ch = #10) or (Ch(1) <> #10) then
        Inc(lBreaks);
      Inc(FCursor);
      end;
    Emit(lFirst, shDefault);
    // A blank line ends the directive options
    if lBreaks > 1 then
      lOptions := False;
    end;
  Finish;
end;

initialization
  TRstSyntaxHighlighter.Register;
end.

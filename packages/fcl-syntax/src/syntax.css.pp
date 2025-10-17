{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    CSS syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.css;

interface

uses
  types, syntax.highlighter;

type

  { TCssSyntaxHighlighter }

  TCssSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
  protected
    procedure ProcessSingleQuoteString(var endPos: integer);
    procedure ProcessDoubleQuoteString(var endPos: integer);
    procedure ProcessMultiLineComment(var endPos: integer);
    procedure ProcessSelector(var endPos: integer);
    procedure ProcessProperty(var endPos: integer);
    procedure ProcessColor(var endPos: integer);
    function CheckForAtRule(var endPos: integer): boolean;
    function CheckForProperty(var endPos: integer): boolean;
    procedure ProcessNumber(var endPos: integer);
    procedure ProcessUrl(var endPos: integer);
    function IsWordChar(ch: char): boolean;
    function IsHexChar(ch: char): boolean;
    class procedure CheckCategories;
    class procedure RegisterDefaultCategories; override;
    class function GetLanguages : TStringDynarray; override;
  public
    constructor Create; override;
    class var
      CategoryCSS : Integer;
      CategoryEmbeddedCSS : Integer;
    function Execute(const Source: string): TSyntaxTokenArray; override;
  end;

const
  MaxKeywordLength = 20;
  MaxKeyword = 41;

  CssAtRuleTable: array[0..MaxKeyword] of string = (
    '@charset', '@import', '@namespace', '@media', '@supports', '@page', '@font-face',
    '@keyframes', '@webkit-keyframes', '@moz-keyframes', '@ms-keyframes', '@o-keyframes',
    '@document', '@font-feature-values', '@viewport', '@counter-style', '@property',
    '@layer', '@container', '@scope', '@starting-style', '@position-try',
    'animation', 'background', 'border', 'color', 'display', 'font', 'height',
    'margin', 'padding', 'position', 'width', 'flex', 'grid', 'transform',
    'transition', 'opacity', 'z-index', 'top', 'right', 'bottom'
    );

function DoCssHighlighting(const Source: string): TSyntaxTokenArray;

implementation

uses
  SysUtils;

  { TCssSyntaxHighlighter }


procedure TCssSyntaxHighlighter.ProcessSingleQuoteString(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening quote
  while FPos <= Length(FSource) do
  begin
    if FSource[FPos] = '''' then
    begin
      Inc(FPos);
      break;
    end
    else if FSource[FPos] = '\' then
    begin
      if FPos < Length(FSource) then Inc(FPos); // Skip escaped character
    end;
    Inc(FPos);
  end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;

procedure TCssSyntaxHighlighter.ProcessDoubleQuoteString(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening quote
  while FPos <= Length(FSource) do
    begin
    if FSource[FPos] = '"' then
      begin
      Inc(FPos);
      break;
      end
    else if FSource[FPos] = '\' then
      begin
      if FPos < Length(FSource) then
        Inc(FPos); // Skip escaped character
      end;
    Inc(FPos);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;

procedure TCssSyntaxHighlighter.ProcessMultiLineComment(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos, 2); // Skip the opening /*
  while FPos < Length(FSource) do
    begin
    if (FSource[FPos] = '*') and (FSource[FPos + 1] = '/') then
      begin
      Inc(FPos, 2);
      break;
      end;
    Inc(FPos);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;

procedure TCssSyntaxHighlighter.ProcessSelector(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;

  // Handle class selectors (.class)
  if FSource[FPos] = '.' then
    begin
    Inc(FPos);
    while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
      Inc(FPos);
    end
  // Handle ID selectors (#id)
  else if FSource[FPos] = '#' then
    begin
    Inc(FPos);
    while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
      Inc(FPos);
    end
  // Handle attribute selectors ([attr])
  else if FSource[FPos] = '[' then
    begin
    Inc(FPos);
    while (FPos <= Length(FSource)) and (FSource[FPos] <> ']') do
      Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] = ']') then
      Inc(FPos);
    end
  // Handle pseudo-selectors (:hover, ::before)
  else if FSource[FPos] = ':' then
    begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] = ':') then
      Inc(FPos); // Handle ::
    while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
      Inc(FPos);
    end
  // Handle element selectors
  else
    begin
    while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
      Inc(FPos);
    end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
end;

procedure TCssSyntaxHighlighter.ProcessProperty(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  while (FPos <= Length(FSource)) and (FSource[FPos] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) do
    Inc(FPos);
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shKeyword);
end;

procedure TCssSyntaxHighlighter.ProcessColor(var endPos: integer);
var
  startPos: integer;
  digitCount: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip #
  digitCount := 0;

  while (FPos <= Length(FSource)) and IsHexChar(FSource[FPos]) and (digitCount < 8) do
    begin
    Inc(FPos);
    Inc(digitCount);
    end;

  // Valid hex colors are 3, 4, 6, or 8 digits
  if digitCount in [3, 4, 6, 8] then
    begin
    endPos := FPos - 1;
    AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
    end
  else
    begin
    // Not a valid color, treat as selector
    FPos := startPos;
    ProcessSelector(endPos);
    end;
end;

function TCssSyntaxHighlighter.CheckForAtRule(var endPos: integer): boolean;
var
  i, j: integer;
  atRule: string;
begin
  Result := False;
  if FSource[FPos] <> '@' then Exit;

  i := 0;
  while (FPos + i <= Length(FSource)) and (i < MaxKeywordLength) and
    (FSource[FPos + i] in ['@', 'a'..'z', 'A'..'Z', '0'..'9', '-', '_']) do
  begin
    Inc(i);
  end;

  atRule := Copy(FSource, FPos, i);

  for j := 0 to 21 do // Only check @-rules (first 22 entries)
    if CssAtRuleTable[j] = atRule then
    begin
      Result := True;
      break;
    end;

  if Result then
  begin
    Inc(FPos, i);
    endPos := FPos - 1;
    AddToken(atRule, shDirective);
  end;
end;

function TCssSyntaxHighlighter.CheckForProperty(var endPos: integer): boolean;
var
   i, j: integer;
  prop: string;
begin
  Result := False;
  i := 0;

  while (FPos + i <= Length(FSource)) and (i < MaxKeywordLength) and
    (FSource[FPos + i] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) do
  begin
    Inc(i);
  end;

  prop := Copy(FSource, FPos, i);

  for j := 22 to MaxKeyword do // Check properties (from index 22 onwards)
    if CssAtRuleTable[j] = prop then
    begin
      Result := True;
      break;
    end;

  if Result then
  begin
    Inc(FPos, i);
    endPos := FPos - 1;
    AddToken(prop, shKeyword);
  end;
end;

procedure TCssSyntaxHighlighter.ProcessNumber(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;

  // Handle numbers (including decimals)
  while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
    Inc(FPos);

  // Handle decimal point
  if (FPos <= Length(FSource)) and (FSource[FPos] = '.') then
  begin
    Inc(FPos);
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
      Inc(FPos);
  end;

  // Handle CSS units (px, em, rem, %, etc.)
  if (FPos <= Length(FSource)) and (FSource[FPos] in ['a'..'z', 'A'..'Z', '%']) then
  begin
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['a'..'z', 'A'..'Z', '%']) do
      Inc(FPos);
  end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
end;

procedure TCssSyntaxHighlighter.ProcessUrl(var endPos: integer);
var
  startPos: integer;
  parenCount: integer;
begin
  startPos := FPos;
  Inc(FPos, 4); // Skip 'url('
  parenCount := 1;

  while (FPos <= Length(FSource)) and (parenCount > 0) do
  begin
    if FSource[FPos] = '(' then
      Inc(parenCount)
    else if FSource[FPos] = ')' then
      Dec(parenCount);
    Inc(FPos);
  end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;

function TCssSyntaxHighlighter.IsWordChar(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-'];
end;

function TCssSyntaxHighlighter.IsHexChar(ch: char): boolean;
begin
  Result := ch in ['0'..'9', 'A'..'F', 'a'..'f'];
end;

class procedure TCssSyntaxHighlighter.CheckCategories;
begin
  if CategoryCSS=0 then
    RegisterDefaultCategories;
end;

class procedure TCssSyntaxHighlighter.RegisterDefaultCategories;
begin
  CategoryCSS:=RegisterCategory('CSS');
  CategoryEmbeddedCSS:=RegisterCategory('EmbeddedCSS');
end;

class function TCssSyntaxHighlighter.GetLanguages: TStringDynarray;
begin
  Result:=['css']
end;

constructor TCssSyntaxHighlighter.Create;
begin
  inherited Create;
  CheckCategories;
  DefaultCategory:=CategoryCSS;
end;

function TCssSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  lLen, endPos, startPos: integer;
  ch: char;
begin
  Result:=Nil;
  CheckCategories;
  lLen:=Length(Source);
  if lLen = 0 then
    Exit;
  FSource := Source;
  FTokens.Reset;
  FPos := 1;
  EndPos:=0;

  while FPos <= lLen do
    begin
    ch := FSource[FPos];
    case ch of
    '''':
      ProcessSingleQuoteString(endPos);
    '"':
      ProcessDoubleQuoteString(endPos);
    '/':
      begin
      if (FPos < Length(FSource)) and (FSource[FPos + 1] = '*') then
        ProcessMultiLineComment(endPos)
      else
        begin
        AddToken('/', shOperator);
        endPos := FPos;
        Inc(FPos);
        end;
      end;
    '#':
      begin
      if (FPos < Length(FSource)) and IsHexChar(FSource[FPos + 1]) then
        ProcessColor(endPos)
      else
        ProcessSelector(endPos);
      end;
    '@':
      begin
      if not CheckForAtRule(endPos) then
        begin
        AddToken('@', shSymbol);
        endPos := FPos;
        Inc(FPos);
        end;
      end;
    '0'..'9':
      ProcessNumber(endPos);
    'a'..'t', 'v'..'z', 'A'..'Z':
      begin
      if not CheckForProperty(endPos) then
        begin
        startPos := FPos;
        while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
        end;
      end;
    'u':
      begin
      if (FPos + 3 <= Length(FSource)) and
        (Copy(FSource, FPos, 4) = 'url(') then
        ProcessUrl(endPos)
      else if not CheckForProperty(endPos) then
        begin
        startPos := FPos;
        while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
        end;
      end;
    '.', ':', '[', ']': ProcessSelector(endPos);
    '{', '}', ';', '(', ')', ',':
      begin
      AddToken(ch, shSymbol);
      endPos := FPos;
      Inc(FPos);
      end;
    '>', '+', '~', '*', '=', '!':
      begin
      AddToken(ch, shOperator);
      endPos := FPos;
      Inc(FPos);
      end;
    ' ', #9, #10, #13:
      begin
      startPos := FPos;
      while (FPos <= Length(FSource)) and (FSource[FPos] in [' ', #9, #10, #13]) do
        Inc(FPos);
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
      end;
    else
      AddToken(ch, shInvalid);
      endPos := FPos;
      Inc(FPos);
    end;
    if FPos = endPos then Inc(FPos);
  end;
  Result := FTokens.GetTokens;
end;

function DoCssHighlighting(const Source: string): TSyntaxTokenArray;
var
  highlighter: TCssSyntaxHighlighter;
begin
  highlighter := TCssSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

initialization
  TCssSyntaxHighlighter.Register;
end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Javascript syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$MODE objfpc}
{$H+}

unit syntax.javascript;

interface

uses
  types, syntax.highlighter;

type

  { TJavaScriptSyntaxHighlighter }

  TJavaScriptSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
  protected
    procedure ProcessSingleQuoteString(var endPos: integer);
    procedure ProcessDoubleQuoteString(var endPos: integer);
    procedure ProcessTemplateString(var endPos: integer);
    procedure ProcessRegexLiteral(var endPos: integer);
    procedure ProcessSingleLineComment(var endPos: integer);
    procedure ProcessMultiLineComment(var endPos: integer);
    function CheckForComment(var endPos: integer): boolean;
    function CheckForKeyword(var endPos: integer): boolean;
    procedure ProcessNumber(var endPos: integer);
    procedure ProcessOperator(var endPos: integer);
    function IsWordChar(ch: char): boolean;
    function IsRegexContext: boolean;
    class function GetLanguages: TStringDynArray; override;
    procedure CheckCategory;
    class procedure RegisterDefaultCategories; override;
  public
    class var CategoryJavascript : integer;
              CategoryEmbeddedJS : Integer;
    constructor create; override;
    function Execute(const Source: string): TSyntaxTokenArray; override;
    procedure reset; override;
    end;

const
  LF_SH_Valid = $01;
  LF_SH_Multiline1 = $02;
  LF_SH_Multiline2 = $04;
  LF_SH_Multiline3 = $08;
  LF_SH_Multiline4 = $10;
  LF_SH_Multiline5 = $20;
  LF_SH_Multiline6 = $40;
  LF_SH_Multiline7 = $80;

  LF_SH_MLComment = LF_SH_Multiline1;  { Multi-line comments }
  LF_SH_Template = LF_SH_Multiline2;  { Template literals }

  MaxKeywordLength = 15;
  MaxKeyword = 62;

  JavaScriptKeywordTable: array[0..MaxKeyword] of string = (
    'abstract', 'arguments', 'async', 'await', 'boolean', 'break', 'byte', 'case', 'catch', 'char',
    'class', 'const', 'continue', 'debugger', 'default', 'delete', 'do', 'double', 'else', 'enum',
    'eval', 'export', 'extends', 'false', 'final', 'finally', 'float', 'for', 'function', 'goto',
    'if', 'implements', 'import', 'in', 'instanceof', 'int', 'interface', 'let', 'long', 'native',
    'new', 'null', 'package', 'private', 'protected', 'public', 'return', 'short', 'static', 'super',
    'switch', 'synchronized', 'this', 'throw', 'throws', 'transient', 'true', 'try', 'typeof', 'undefined',
    'var', 'void', 'while'
    );

function DoJavaScriptHighlighting(const Source: string): TSyntaxTokenArray;

implementation

uses
  SysUtils;

  { TJavaScriptSyntaxHighlighter }


procedure TJavaScriptSyntaxHighlighter.ProcessSingleQuoteString(var endPos: integer);
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

procedure TJavaScriptSyntaxHighlighter.ProcessDoubleQuoteString(var endPos: integer);
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
      if FPos < Length(FSource) then Inc(FPos); // Skip escaped character
      end;
    Inc(FPos);
    end;
    endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;

procedure TJavaScriptSyntaxHighlighter.ProcessTemplateString(var endPos: integer);
var
  startPos: integer;
  braceCount: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening backtick
  braceCount := 0;

  while FPos <= Length(FSource) do
    begin
    if FSource[FPos] = '`' then
      begin
      if braceCount = 0 then
        begin
        Inc(FPos);
        break;
        end;
      end
    else if FSource[FPos] = '\' then
      begin
      if FPos < Length(FSource) then Inc(FPos); // Skip escaped character
      end
    else if (FPos < Length(FSource)) and (FSource[FPos] = '$') and (FSource[FPos + 1] = '{') then
      begin
      Inc(braceCount);
      Inc(FPos); // Skip the $
      end
    else if FSource[FPos] = '{' then
      begin
      Inc(braceCount);
      end
    else if FSource[FPos] = '}' then
      begin
      Dec(braceCount);
      if braceCount < 0 then
        braceCount := 0;
      end;
    Inc(FPos);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shRawString);
end;

procedure TJavaScriptSyntaxHighlighter.ProcessRegexLiteral(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening /

  while FPos <= Length(FSource) do
    begin
    if FSource[FPos] = '/' then
      begin
      Inc(FPos);
      // Handle regex flags (g, i, m, etc.)
      while (FPos <= Length(FSource)) and (FSource[FPos] in ['g', 'i', 'm', 's', 'u', 'y']) do
        Inc(FPos);
      break;
      end
    else if FSource[FPos] = '\' then
      begin
      if FPos < Length(FSource) then Inc(FPos); // Skip escaped character
      end
    else if FSource[FPos] in [#10, #13] then
      begin
      break; // Regex can't span lines
      end;
    Inc(FPos);
    end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shRegex);
end;

procedure TJavaScriptSyntaxHighlighter.ProcessSingleLineComment(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) do
    Inc(FPos);
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;

procedure TJavaScriptSyntaxHighlighter.ProcessMultiLineComment(var endPos: integer);
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

function TJavaScriptSyntaxHighlighter.CheckForComment(var endPos: integer): boolean;
begin
  Result := True;

  if (FPos < Length(FSource)) and (FSource[FPos] = '/') and (FSource[FPos + 1] = '/') then
    begin
    ProcessSingleLineComment(endPos);
    end
  else if (FPos < Length(FSource)) and (FSource[FPos] = '/') and (FSource[FPos + 1] = '*') then
    begin
    ProcessMultiLineComment(endPos);
    end
  else
    Result := False;
end;

function TJavaScriptSyntaxHighlighter.CheckForKeyword(var endPos: integer): boolean;
var
  i, j: integer;
  keyword: string;
begin

  i := 0;
  while (FPos + i <= Length(FSource)) and (i < MaxKeywordLength) and
    IsWordChar(FSource[FPos + i]) do
    begin
    Inc(i);
    end;

  keyword := Copy(FSource, FPos, i);
  Result := False;

  for j := 0 to MaxKeyword do
    if JavaScriptKeywordTable[j] = keyword then
      begin
      Result := True;
      break;
      end;

  if not Result then Exit;

  Inc(FPos, i);
    endPos := FPos - 1;
  AddToken(keyword, shKeyword);
end;

procedure TJavaScriptSyntaxHighlighter.ProcessNumber(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;

  // Handle hex numbers (0x...)
  if (FPos < Length(FSource)) and (FSource[FPos] = '0') and
    (UpCase(FSource[FPos + 1]) = 'X') then
    begin
    Inc(FPos, 2);
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9', 'A'..'F', 'a'..'f']) do
      Inc(FPos);
    end
  // Handle binary numbers (0b...)
  else if (FPos < Length(FSource)) and (FSource[FPos] = '0') and
    (UpCase(FSource[FPos + 1]) = 'B') then
    begin
    Inc(FPos, 2);
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0', '1']) do
      Inc(FPos);
    end
  // Handle octal numbers (0o...) or regular numbers
  else
    begin
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
      Inc(FPos);

    // Handle decimal point
    if (FPos <= Length(FSource)) and (FSource[FPos] = '.') then
      begin
      Inc(FPos);
      while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
        Inc(FPos);
      end;

    // Handle scientific notation (e/E)
    if (FPos <= Length(FSource)) and (UpCase(FSource[FPos]) = 'E') then
      begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['+', '-']) then
        Inc(FPos);
      while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
        Inc(FPos);
      end;
    end;

    endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
end;

procedure TJavaScriptSyntaxHighlighter.ProcessOperator(var endPos: integer);
var
  startPos: integer;
  ch: char;
begin
  startPos := FPos;
  ch := FSource[FPos];

  // Handle multi-character operators
  case ch of
    '=': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['=', '>']) then Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos - 1] = '=') and (FSource[FPos] = '=') then Inc(FPos);
      end;
    '!': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] = '=') then
        begin
        Inc(FPos);
        if (FPos <= Length(FSource)) and (FSource[FPos] = '=') then Inc(FPos);
        end;
      end;
    '<': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['=', '<']) then Inc(FPos);
      end;
    '>': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['=', '>']) then
        begin
        Inc(FPos);
        if (FPos <= Length(FSource)) and (FSource[FPos] = '>') then Inc(FPos);
        end;
      end;
    '&': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] = '&') then Inc(FPos);
      end;
    '|': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] = '|') then Inc(FPos);
      end;
    '+': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['+', '=']) then Inc(FPos);
      end;
    '-': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['-', '=']) then Inc(FPos);
      end;
    '*': begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['*', '=']) then
        begin
        Inc(FPos);
        if (FPos <= Length(FSource)) and (FSource[FPos - 1] = '*') and (FSource[FPos] = '=') then Inc(FPos);
        end;
      end;
    '.': begin
      Inc(FPos);
      if (FPos < Length(FSource)) and (FSource[FPos] = '.') and (FSource[FPos + 1] = '.') then
        Inc(FPos, 2);
      end;
    else
      Inc(FPos);
    end;

    endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shOperator);
end;

function TJavaScriptSyntaxHighlighter.IsWordChar(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$'];
end;

function TJavaScriptSyntaxHighlighter.IsRegexContext: boolean;

var
  i: integer;
  lastToken: TSyntaxToken;
  lText : String;
  lLen : integer;

begin
  Result := True; // Default to allowing regex

  if FTokens.Count = 0 then Exit;

  // Look at the last meaningful token
  i := FTokens.Count - 1;
  while (i >= 0) and (FTokens.Tokens[i].Kind = shDefault) and
    (Trim(FTokens.Tokens[i].Text) = '') do
    Dec(i);

  if i < 0 then Exit;

  lastToken := FTokens.Tokens[i];
  lText:=lastToken.text;
  lLen:=Length(lText);

  // After these tokens, / is likely division, not regex
  Result := not ((lText = ')') or (lText = ']') or (lText = '}')) and
            not (LastToken.Kind = shNumbers) and
            not (
                 (LastToken.Kind = shDefault)
                 and (llen > 0)
                 and IsWordChar(lText[llen])
                );
end;

class function TJavaScriptSyntaxHighlighter.GetLanguages: TStringDynArray;
begin
  Result := ['js', 'javascript'];
end;

procedure TJavaScriptSyntaxHighlighter.CheckCategory;
begin
  if CategoryJavascript=0 then
    RegisterDefaultCategories;
end;

class procedure TJavaScriptSyntaxHighlighter.RegisterDefaultCategories;
begin
  CategoryJavascript:=RegisterCategory('javascript');
  CategoryEmbeddedJS:=RegisterCategory('EmbeddedJS');
  inherited RegisterDefaultCategories;
end;

constructor TJavaScriptSyntaxHighlighter.create;
begin
  Inherited ;
  CheckCategory;
  DefaultCategory:=CategoryJavascript;
end;

function TJavaScriptSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  lLen,endPos, startPos: integer;
  ch: char;
begin
  Result:=Nil;
  CheckCategory;
  lLen:=Length(Source);
  if lLen = 0 then
    Exit;
  endPos:=0;
  FSource:=Source;
  FTokens.Reset;
  FPos := 1;
  while FPos <= lLen do
    begin
    ch := FSource[FPos];
    if not CheckForComment(endPos) then
      begin
      case ch of
      '''':
        ProcessSingleQuoteString(endPos);
      '"':
        ProcessDoubleQuoteString(endPos);
      '`':
        ProcessTemplateString(endPos);
      '/':
        begin
        if IsRegexContext then
          ProcessRegexLiteral(endPos)
        else
          ProcessOperator(endPos);
        end;
      '0'..'9': ProcessNumber(endPos);
      'a'..'z', 'A'..'Z', '_', '$':
        begin
        if not CheckForKeyword(endPos) then
          begin
          startPos := FPos;
          while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
            Inc(FPos);
              endPos := FPos - 1;
          AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
          end;
        end;
      '=', '!', '<', '>', '&', '|', '+', '-', '*', '%', '^', '~', '?', ':', '.':
        ProcessOperator(endPos);
      ';', '(', ')', '[', ']', '{', '}', ',':
        begin
        AddToken(ch, shSymbol);
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
      end;
    if FPos = endPos then
      Inc(FPos);
    end;
  Result := FTokens.GetTokens;
end;

procedure TJavaScriptSyntaxHighlighter.reset;
begin
  inherited reset;
  FPos := 0;
end;

function DoJavaScriptHighlighting(const Source: string): TSyntaxTokenArray;
var
  highlighter: TJavaScriptSyntaxHighlighter;
begin
  highlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

initialization
  TJavaScriptSyntaxHighlighter.Register;
end.

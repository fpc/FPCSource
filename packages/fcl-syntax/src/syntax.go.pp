{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Go syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$MODE objfpc}
{$H+}

unit syntax.go;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter;
  {$ELSE}
  Types, SysUtils, syntax.highlighter;
  {$ENDIF}

type

  { TGoSyntaxHighlighter }

  TGoSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
  protected
    procedure ProcessInterpretedString(var endPos: integer);
    procedure ProcessRawString(var endPos: integer);
    procedure ProcessRuneLiteral(var endPos: integer);
    procedure ProcessSingleLineComment(var endPos: integer);
    procedure ProcessMultiLineComment(var endPos: integer);
    function CheckForComment(var endPos: integer): boolean;
    function CheckForKeyword(var endPos: integer): boolean;
    procedure ProcessNumber(var endPos: integer);
    procedure ProcessOperator(var endPos: integer);
    function IsWordChar(ch: char): boolean;
    function IsDigitSequence(aChars: TSysCharSet): boolean;
    class function GetLanguages: TStringDynArray; override;
    procedure CheckCategory;
    class procedure RegisterDefaultCategories; override;
  public
    class var CategoryGo : integer;
              CategoryBuiltin : Integer;
    constructor create; override;
    function Execute(const Source: string): TSyntaxTokenArray; override;
    procedure reset; override;
    end;

const
  MaxKeyword = 24;

  GoKeywordTable: array[0..MaxKeyword] of string = (
    'break', 'case', 'chan', 'const', 'continue', 'default', 'defer', 'else', 'fallthrough', 'for',
    'func', 'go', 'goto', 'if', 'import', 'interface', 'map', 'package', 'range', 'return',
    'select', 'struct', 'switch', 'type', 'var'
    );

  MaxBuiltin = 43;

  { Predeclared identifiers: types, constants and built-in functions. }
  GoBuiltinTable: array[0..MaxBuiltin] of string = (
    'any', 'append', 'bool', 'byte', 'cap', 'clear', 'close', 'comparable', 'complex', 'complex128',
    'complex64', 'copy', 'delete', 'error', 'false', 'float32', 'float64', 'imag', 'int', 'int16',
    'int32', 'int64', 'int8', 'iota', 'len', 'make', 'max', 'min', 'new', 'nil',
    'panic', 'print', 'println', 'real', 'recover', 'rune', 'string', 'true', 'uint', 'uint16',
    'uint32', 'uint64', 'uint8', 'uintptr'
    );

function DoGoHighlighting(const Source: string): TSyntaxTokenArray;

implementation

  { TGoSyntaxHighlighter }


procedure TGoSyntaxHighlighter.ProcessInterpretedString(var endPos: integer);
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
      end
    else if FSource[FPos] in [#10, #13] then
      break; // An interpreted string literal may not span lines
    Inc(FPos);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;


procedure TGoSyntaxHighlighter.ProcessRawString(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening backquote
  while FPos <= Length(FSource) do
    begin
    if FSource[FPos] = '`' then
      begin
      Inc(FPos);
      break;
      end;
    Inc(FPos);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shRawString);
end;


procedure TGoSyntaxHighlighter.ProcessRuneLiteral(var endPos: integer);
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
      end
    else if FSource[FPos] in [#10, #13] then
      break;
    Inc(FPos);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shCharacters);
end;


procedure TGoSyntaxHighlighter.ProcessSingleLineComment(var endPos: integer);
var
  startPos: integer;
  lText: string;
  lKind: TSyntaxHighlightKind;
begin
  startPos := FPos;
  while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) do
    Inc(FPos);
  endPos := FPos - 1;
  lText := Copy(FSource, startPos, endPos - startPos + 1);
  // //go: and // +build steer the toolchain
  if (Copy(lText, 1, 5) = '//go:') or (Copy(lText, 1, 9) = '// +build') then
    lKind := shDirective
  else
    lKind := shComment;
  AddToken(lText, lKind);
end;


procedure TGoSyntaxHighlighter.ProcessMultiLineComment(var endPos: integer);
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
  if FPos = Length(FSource) then
    Inc(FPos);
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;


function TGoSyntaxHighlighter.CheckForComment(var endPos: integer): boolean;
begin
  Result := True;
  if (FPos < Length(FSource)) and (FSource[FPos] = '/') and (FSource[FPos + 1] = '/') then
    ProcessSingleLineComment(endPos)
  else if (FPos < Length(FSource)) and (FSource[FPos] = '/') and (FSource[FPos + 1] = '*') then
    ProcessMultiLineComment(endPos)
  else
    Result := False;
end;


function TGoSyntaxHighlighter.CheckForKeyword(var endPos: integer): boolean;
var
  i, j: integer;
  keyword: string;
begin
  i := 0;
  while (FPos + i <= Length(FSource)) and IsWordChar(FSource[FPos + i]) do
    Inc(i);
  keyword := Copy(FSource, FPos, i);
  Result := False;
  for j := 0 to MaxKeyword do
    if GoKeywordTable[j] = keyword then
      begin
      Inc(FPos, i);
      endPos := FPos - 1;
      AddToken(keyword, shKeyword);
      Exit(True);
      end;
  for j := 0 to MaxBuiltin do
    if GoBuiltinTable[j] = keyword then
      begin
      Inc(FPos, i);
      endPos := FPos - 1;
      AddToken(keyword, shKeyword, CategoryBuiltin);
      Exit(True);
      end;
end;


function TGoSyntaxHighlighter.IsDigitSequence(aChars: TSysCharSet): boolean;
begin
  Result := False;
  while (FPos <= Length(FSource)) and (CharInSet(FSource[FPos], aChars) or (FSource[FPos] = '_')) do
    begin
    Result := True;
    Inc(FPos);
    end;
end;


procedure TGoSyntaxHighlighter.ProcessNumber(var endPos: integer);
var
  startPos: integer;
  lBase: char;
begin
  startPos := FPos;
  lBase := #0;
  if (FSource[FPos] = '0') and (FPos < Length(FSource)) then
    lBase := UpCase(FSource[FPos + 1]);
  if lBase in ['X', 'B', 'O'] then
    begin
    Inc(FPos, 2);
    case lBase of
      'X': IsDigitSequence(['0'..'9', 'a'..'f', 'A'..'F']);
      'B': IsDigitSequence(['0', '1']);
      'O': IsDigitSequence(['0'..'7']);
    end;
    // Hexadecimal floats take a binary exponent
    if (lBase = 'X') and (FPos <= Length(FSource)) and (FSource[FPos] = '.') then
      begin
      Inc(FPos);
      IsDigitSequence(['0'..'9', 'a'..'f', 'A'..'F']);
      end;
    if (lBase = 'X') and (FPos <= Length(FSource)) and (UpCase(FSource[FPos]) = 'P') then
      begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['+', '-']) then
        Inc(FPos);
      IsDigitSequence(['0'..'9']);
      end;
    end
  else
    begin
    IsDigitSequence(['0'..'9']);
    if (FPos <= Length(FSource)) and (FSource[FPos] = '.') then
      begin
      Inc(FPos);
      IsDigitSequence(['0'..'9']);
      end;
    if (FPos <= Length(FSource)) and (UpCase(FSource[FPos]) = 'E') then
      begin
      Inc(FPos);
      if (FPos <= Length(FSource)) and (FSource[FPos] in ['+', '-']) then
        Inc(FPos);
      IsDigitSequence(['0'..'9']);
      end;
    end;
  // Imaginary literal
  if (FPos <= Length(FSource)) and (FSource[FPos] = 'i') then
    Inc(FPos);
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
end;


procedure TGoSyntaxHighlighter.ProcessOperator(var endPos: integer);

  // Consume the character at the cursor when it is one of aChars.
  function Take(const aChars: TSysCharSet): boolean;
  begin
    Result := (FPos <= Length(FSource)) and CharInSet(FSource[FPos], aChars);
    if Result then
      Inc(FPos);
  end;

var
  startPos: integer;
  ch: char;
begin
  startPos := FPos;
  ch := FSource[FPos];
  Inc(FPos);
  case ch of
    ':', '=', '!', '*', '/', '%', '^':
      Take(['=']);
    '+', '-', '|':
      Take([ch, '=']);
    '<':
      if not Take(['=', '-']) then
        if Take(['<']) then
          Take(['=']);
    '>':
      if not Take(['=']) then
        if Take(['>']) then
          Take(['=']);
    '&':
      if not Take(['&', '=']) then
        if Take(['^']) then
          Take(['=']);
    '.':
      if (FPos < Length(FSource)) and (FSource[FPos] = '.') and (FSource[FPos + 1] = '.') then
        Inc(FPos, 2);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shOperator);
end;


function TGoSyntaxHighlighter.IsWordChar(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_', #128..#255];
end;


class function TGoSyntaxHighlighter.GetLanguages: TStringDynArray;
begin
  Result := ['go', 'golang'];
end;


procedure TGoSyntaxHighlighter.CheckCategory;
begin
  if CategoryGo = 0 then
    RegisterDefaultCategories;
end;


class procedure TGoSyntaxHighlighter.RegisterDefaultCategories;
begin
  CategoryGo := RegisterCategory('go');
  CategoryBuiltin := RegisterCategory('builtin');
  inherited RegisterDefaultCategories;
end;


constructor TGoSyntaxHighlighter.create;
begin
  Inherited ;
  CheckCategory;
  DefaultCategory := CategoryGo;
end;


function TGoSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  lLen, endPos, startPos: integer;
  ch: char;
begin
  Result := Nil;
  CheckCategory;
  lLen := Length(Source);
  if lLen = 0 then
    Exit;
  endPos := 0;
  FSource := Source;
  FTokens.Reset;
  FPos := 1;
  while FPos <= lLen do
    begin
    ch := FSource[FPos];
    if not CheckForComment(endPos) then
      begin
      case ch of
      '"':
        ProcessInterpretedString(endPos);
      '`':
        ProcessRawString(endPos);
      '''':
        ProcessRuneLiteral(endPos);
      '0'..'9':
        ProcessNumber(endPos);
      '.':
        begin
        if (FPos < Length(FSource)) and (FSource[FPos + 1] in ['0'..'9']) then
          ProcessNumber(endPos)
        else
          ProcessOperator(endPos);
        end;
      'a'..'z', 'A'..'Z', '_', #128..#255:
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
      '=', '!', '<', '>', '&', '|', '+', '-', '*', '/', '%', '^', ':', '~':
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


procedure TGoSyntaxHighlighter.reset;
begin
  inherited reset;
  FPos := 0;
end;


function DoGoHighlighting(const Source: string): TSyntaxTokenArray;
var
  highlighter: TGoSyntaxHighlighter;
begin
  highlighter := TGoSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

initialization
  TGoSyntaxHighlighter.Register;
end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Pascal syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}
{$H+}

unit syntax.pascal;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter;
  {$ELSE}
  types, sysutils, syntax.highlighter;
  {$ENDIF}

type

  { TPascalSyntaxHighlighter }

  TPascalSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
  protected
    procedure CheckCategories;
    procedure ProcessComment1(var endPos: integer; akind : TSyntaxHighlightKind);
    procedure ProcessComment2(var endPos: integer);
    function CheckForComment(var endPos: integer): boolean;
    procedure ProcessAsm(var endPos: integer);
    function CheckForKeyword(var endPos: integer): boolean;
    procedure ProcessSymbol(var endPos: integer);
    class function GetLanguages: TStringDynArray; override;
  public
    constructor Create; override;
    class var
       CategoryPascal,
       CategoryIdentifier : Integer;
    function Execute(const Source: string): TSyntaxTokenArray; override;
  end;

function DoPascalHighlighting(const Source: string): TSyntaxTokenArray;


implementation

const
  MaxKeywordLength = 15;
  MaxKeyword = 61;

  KeywordTable: array[0..MaxKeyword] of string =
    ('AND', 'ARRAY', 'ASM', 'ASSEMBLER',
    'BEGIN', 'BREAK',
    'CASE', 'CONST', 'CONSTRUCTOR', 'CLASS',
    'DEFAULT', 'DESTRUCTOR', 'DIV', 'DO', 'DOWNTO',
    'ELSE', 'END', 'EXCEPT', 'EXIT',
    'FINALIZATION', 'FINALLY', 'FOR', 'FUNCTION',
    'GOTO',
    'IF', 'IMPLEMENTATION', 'IN', 'INHERITED', 'INITIALIZATION', 'INTERFACE',
    'NIL', 'NOT',
    'OBJECT', 'OF', 'ON', 'OPERATOR', 'OR', 'OVERRIDE',
    'PACKED', 'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PROPERTY', 'PROTECTED',
    'PUBLIC', 'PUBLISHED',
    'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING',
    'SET',
    'THEN', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES',
    'VAR', 'VIRTUAL',
    'WHILE', 'WITH',
    'XOR');

  KeywordAsmIndex = 2;


  { TPascalSyntaxHighlighter }

procedure TPascalSyntaxHighlighter.CheckCategories;
begin
  if CategoryPascal=0 then
    begin
    CategoryPascal:=RegisterCategory('pascal');
    CategoryIdentifier:=RegisterCategory('identifier');
    end;
end;


procedure TPascalSyntaxHighlighter.ProcessComment1(var endPos: integer; akind: TSyntaxHighlightKind);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos);  // Skip the opening '{'
  while (FPos <= Length(FSource)) and (FSource[FPos] <> '}') do
    Inc(FPos);
  if (FPos <= Length(FSource)) and (FSource[FPos] = '}') then
    Inc(FPos);
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), aKind);
end;

procedure TPascalSyntaxHighlighter.ProcessComment2(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos, 2);  // Skip the opening '(*'
  while (FPos < Length(FSource)) and not ((FSource[FPos] = '*') and (FSource[FPos + 1] = ')')) do
    Inc(FPos);
  if (FPos < Length(FSource)) and (FSource[FPos] = '*') and (FSource[FPos + 1] = ')') then
    begin
    Inc(FPos, 2);
    end
  else
    FPos := Length(FSource) + 1;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;

function TPascalSyntaxHighlighter.CheckForComment(var endPos: integer): boolean;
var
  startPos: integer;
  kind: TSyntaxHighlightKind;
begin
  Result := True;
  startPos := FPos;

  if (FPos <= Length(FSource)) and (FSource[FPos] = '{') then
    begin
    if (FPos < Length(FSource)) and (FSource[FPos + 1] = '$') then
      kind := shDirective
    else
      kind := shComment;
    ProcessComment1(endPos,kind);
    end
  else if (FPos < Length(FSource)) and (FSource[FPos] = '(') and (FSource[FPos + 1] = '*') then
    begin
    ProcessComment2(endPos);
    end
  else if (FPos < Length(FSource)) and (FSource[FPos] = '/') and (FSource[FPos + 1] = '/') then
    begin
    while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) do
      Inc(FPos);
    endPos := FPos - 1;
    AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
    end
  else
    Result := False;
end;

procedure TPascalSyntaxHighlighter.ProcessAsm(var endPos: integer);
var
  startPos: integer;
  lastChar: char;
begin
  startPos := FPos;
  lastChar := ' ';
  while FPos <= Length(FSource) do
    begin
    if (lastChar in [' ', #9, #10, #13]) and
      (FPos + 2 <= Length(FSource)) and
      (UpCase(FSource[FPos]) = 'E') and (UpCase(FSource[FPos + 1]) = 'N') and
      (UpCase(FSource[FPos + 2]) = 'D') then
      begin
      endPos := FPos - 1;
      if endPos >= startPos then
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shAssembler);
      AddToken('END', shKeyword);
      Inc(FPos, 3);
      Exit;
      end
    else
      begin
      if CheckForComment(endPos) then
        lastChar := ' '
      else
        begin
        lastChar := FSource[FPos];
        Inc(FPos);
        end;
      end;
    end;
  endPos := FPos - 1;
  if endPos >= startPos then
    AddToken(Copy(FSource, startPos, endPos - startPos + 1), shAssembler);
end;

function TPascalSyntaxHighlighter.CheckForKeyword(var endPos: integer): boolean;

const
  IdentifierChars = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];

var
  i, lIdx: integer;
  keyword, ukeyword: string;

begin
  i := 0;
  while (FPos + i <= Length(FSource))
        and (i < MaxKeywordLength)
        and (FSource[FPos + i] in IdentifierChars) do
    Inc(i);
  keyword := Copy(FSource, FPos, i);
  ukeyword := UpperCase(keyword);

  Result := False;
  lIdx:=MaxKeyWord;
  While (Not Result) and (lIdx>=0) do
    begin
    Result:=KeywordTable[lIdx] = ukeyword;
    Dec(lIdx);
    end;

  if not Result then
    Exit;

  Inc(lIdx); // Index of actual keyword
  Inc(FPos,i);
  endPos:=FPos - 1;
  AddToken(keyword,shKeyword);
  if lIdx=KeywordAsmIndex then
    ProcessAsm(endPos);
end;

procedure TPascalSyntaxHighlighter.ProcessSymbol(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  if (FPos < Length(FSource)) and (FSource[FPos] = ':') and (FSource[FPos + 1] = '=') then
    Inc(FPos, 2)
  else
    Inc(FPos);
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shSymbol);
end;

class function TPascalSyntaxHighlighter.GetLanguages: TStringDynArray;
begin
  Result:=['pascal','delphi','objectpascal']
end;

constructor TPascalSyntaxHighlighter.Create;
begin
  inherited Create;
  CheckCategories;
  DefaultCategory:=CategoryPascal;
end;

function TPascalSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  endPos: integer;
  StringLength: integer;
  lLen,startPos: integer;
  ch: char;
begin
  Result:=Nil;
  CheckCategories;
  if Length(Source) = 0 then
    Exit;
  FSource:=Source;
  lLen:=Length(FSource);
  FTokens.Reset;
  FPos := 1;
  EndPos:=0;
  while FPos <= llen do
    begin
    ch := FSource[FPos];
    if CheckForComment(endPos) then
      begin
      FPos := endPos + 1;
      continue;
      end;

    case ch of
      ',', ';', ':', '.', '(', ')', '[', ']', '<', '>', '=',
      '*', '/', '+', '-', '^', '&', '@':
        ProcessSymbol(endPos);
      '#':
        begin
        startPos := FPos;
        Inc(FPos);
        if (FPos <= Length(FSource)) and (FSource[FPos] = '$') then
        Inc(FPos);
        while (FPos <= Length(FSource)) and (FSource[FPos] >= '0') and (FSource[FPos] <= '9') do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shCharacters);
        end;
      '$':
        begin
        startPos := FPos;
        Inc(FPos);
        while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
        end;
      '0'..'9':
        begin
        startPos := FPos;
        Inc(FPos);
        while (FPos <= Length(FSource)) and (FSource[FPos] >= '0') and (FSource[FPos] <= '9') do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
        end;
      '''':
        begin
        startPos := FPos;
        Inc(FPos);
        StringLength := 0;
        while (FPos <= Length(FSource)) do
          begin
          if FSource[FPos] = '''' then
            if (FPos < Length(FSource)) and (FSource[FPos + 1] = '''') then
              begin
              Inc(FPos, 2);
              Inc(StringLength);
              end
            else
              begin
              Inc(FPos);
              break;
              end
          else
            begin
            Inc(StringLength);
            Inc(FPos);
            end;
          end;
        endPos := FPos - 1;
        if StringLength = 1 then
          AddToken(Copy(FSource, startPos, endPos - startPos + 1), shCharacters)
        else if (FPos > Length(FSource)) and (FSource[endPos] <> '''') then
          AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid)
        else
          AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
        end;
      '_', 'A'..'Z', 'a'..'z':
        begin
        if not CheckForKeyword(endPos) then
          begin
          startPos := FPos;
          while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9', '_', 'A'..'Z', 'a'..'z']) do
            Inc(FPos);
          endPos := FPos - 1;
          AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
          end;
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
        Inc(FPos);
      end;
    if FPos = endPos then
      Inc(FPos);
  end;
  Result := FTokens.GetTokens;
end;

function DoPascalHighlighting(const Source: string): TSyntaxTokenArray;

var
  highlighter: TPascalSyntaxHighlighter;

begin
  highlighter := TPascalSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;


initialization
  TPascalSyntaxHighlighter.Register;
end.

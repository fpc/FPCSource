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
  System.Types, System.SysUtils, syntax.highlighter, syntax.table;
  {$ELSE}
  types, sysutils, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TPascalSyntaxHighlighter }

  TPascalSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
    // True if a comment starts at FPos
    function IsCommentStart: boolean;
    // True if the word END, in any case, starts at FPos
    function IsAsmEnd: boolean;
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
  protected
    procedure DoExecute(const Source: string); override;
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

var
  PascalKeywords: TSyntaxWordTable;


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
  AddSourceToken(FSource, startPos, endPos - startPos + 1, aKind);
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
  AddSourceToken(FSource, startPos, endPos - startPos + 1, shComment);
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
    AddSourceToken(FSource, startPos, endPos - startPos + 1, shComment);
    end
  else
    Result := False;
end;

function TPascalSyntaxHighlighter.IsCommentStart: boolean;

begin
  Result := (FPos <= Length(FSource))
            and ((FSource[FPos] = '{')
                 or ((FPos < Length(FSource)) and (FSource[FPos] = '(') and (FSource[FPos + 1] = '*'))
                 or ((FPos < Length(FSource)) and (FSource[FPos] = '/') and (FSource[FPos + 1] = '/')));
end;


function TPascalSyntaxHighlighter.IsAsmEnd: boolean;

begin
  Result := (FPos + 2 <= Length(FSource))
            and (UpCase(FSource[FPos]) = 'E') and (UpCase(FSource[FPos + 1]) = 'N')
            and (UpCase(FSource[FPos + 2]) = 'D')
            and ((FPos + 3 > Length(FSource)) or not (FSource[FPos + 3] in ['0'..'9', 'A'..'Z', 'a'..'z', '_']));
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
    if (lastChar in [' ', #9, #10, #13]) and IsAsmEnd then
      begin
      if FPos > startPos then
        AddSourceToken(FSource, startPos, FPos - startPos, shAssembler);
      AddSourceToken(FSource, FPos, 3, shKeyword);
      Inc(FPos, 3);
      endPos := FPos - 1;
      Exit;
      end;
    if IsCommentStart then
      begin
      if FPos > startPos then
        AddSourceToken(FSource, startPos, FPos - startPos, shAssembler);
      CheckForComment(endPos);
      startPos := FPos;
      lastChar := ' ';
      end
    else
      begin
      lastChar := FSource[FPos];
      Inc(FPos);
      end;
    end;
  endPos := FPos - 1;
  if FPos > startPos then
    AddSourceToken(FSource, startPos, FPos - startPos, shAssembler);
end;

function TPascalSyntaxHighlighter.CheckForKeyword(var endPos: integer): boolean;

const
  IdentifierChars = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];

var
  i: integer;
  lIsAsm: boolean;

begin
  i := 0;
  while (FPos + i <= Length(FSource))
        and (i < MaxKeywordLength)
        and (FSource[FPos + i] in IdentifierChars) do
    Inc(i);
  Result := PascalKeywords.Lookup(PChar(Pointer(FSource)) + FPos - 1, i) = shKeyword;
  if not Result then
    Exit;
  lIsAsm := (i = 3) and (UpCase(FSource[FPos]) = 'A') and (UpCase(FSource[FPos + 1]) = 'S')
            and (UpCase(FSource[FPos + 2]) = 'M');
  AddSourceToken(FSource, FPos, i, shKeyword);
  Inc(FPos,i);
  endPos:=FPos - 1;
  if lIsAsm then
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
  AddSourceToken(FSource, startPos, endPos - startPos + 1, shSymbol);
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

procedure TPascalSyntaxHighlighter.DoExecute(const Source: string);
var
  endPos: integer;
  StringLength: integer;
  lLen,startPos: integer;
  ch: char;
begin
  CheckCategories;
  if Length(Source) = 0 then
    Exit;
  FSource:=Source;
  lLen:=Length(FSource);
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
        AddSourceToken(FSource, startPos, endPos - startPos + 1, shCharacters);
        end;
      '$':
        begin
        startPos := FPos;
        Inc(FPos);
        while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(FPos);
        endPos := FPos - 1;
        AddSourceToken(FSource, startPos, endPos - startPos + 1, shNumbers);
        end;
      '0'..'9':
        begin
        startPos := FPos;
        Inc(FPos);
        while (FPos <= Length(FSource)) and (FSource[FPos] >= '0') and (FSource[FPos] <= '9') do
          Inc(FPos);
        endPos := FPos - 1;
        AddSourceToken(FSource, startPos, endPos - startPos + 1, shNumbers);
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
          AddSourceToken(FSource, startPos, endPos - startPos + 1, shCharacters)
        else if (FPos > Length(FSource)) and (FSource[endPos] <> '''') then
          AddSourceToken(FSource, startPos, endPos - startPos + 1, shInvalid)
        else
          AddSourceToken(FSource, startPos, endPos - startPos + 1, shStrings);
        end;
      '_', 'A'..'Z', 'a'..'z':
        begin
        if not CheckForKeyword(endPos) then
          begin
          startPos := FPos;
          while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9', '_', 'A'..'Z', 'a'..'z']) do
            Inc(FPos);
          endPos := FPos - 1;
          AddSourceToken(FSource, startPos, endPos - startPos + 1, shDefault);
          end;
        end;
      ' ', #9, #10, #13:
        begin
        startPos := FPos;
        while (FPos <= Length(FSource)) and (FSource[FPos] in [' ', #9, #10, #13]) do
          Inc(FPos);
        endPos := FPos - 1;
        AddSourceToken(FSource, startPos, endPos - startPos + 1, shDefault);
        end;
      else
        AddSourceToken(FSource, FPos, 1, shInvalid);
        Inc(FPos);
      end;
    if FPos = endPos then
      Inc(FPos);
  end;
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


// Fill PascalKeywords from KeywordTable
procedure CreatePascalKeywords;

var
  I: Integer;
begin
  PascalKeywords := TSyntaxWordTable.Create(False);
  for I := 0 to MaxKeyword do
    PascalKeywords.AddWords(KeywordTable[I], shKeyword);
end;


initialization
  CreatePascalKeywords;
  TPascalSyntaxHighlighter.Register;
finalization
  FreeAndNil(PascalKeywords);
end.

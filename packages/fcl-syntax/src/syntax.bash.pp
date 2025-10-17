{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Bash syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.bash;

interface

uses
  types, syntax.highlighter;

type

  { TBashSyntaxHighlighter }

  TBashSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
  protected
    procedure ProcessSingleQuoteString(var endPos: integer);
    procedure ProcessDoubleQuoteString(var endPos: integer);
    procedure ProcessBackquoteString(var endPos: integer);
    procedure ProcessVariable(var endPos: integer);
    procedure ProcessComment(var endPos: integer);
    function CheckForKeyword(var endPos: integer): boolean;
    procedure ProcessNumber(var endPos: integer);
    procedure ProcessOperator(var endPos: integer);
    function IsWordChar(ch: char): boolean;
    class function GetLanguages: TStringDynArray; override;
    procedure checkcategory; virtual;
  public
    constructor create; override;
    class var CategoryBash : Integer;
    function Execute(const Source: string): TSyntaxTokenArray; override;
  end;

const
  MaxKeywordLength = 15;
  MaxKeyword = 39;

  BashKeywordTable: array[0..MaxKeyword] of string = (
    'case', 'do', 'done', 'elif', 'else', 'esac', 'fi', 'for', 'function', 'if',
    'in', 'select', 'then', 'until', 'while', 'break', 'continue', 'return', 'exit', 'declare',
    'local', 'readonly', 'export', 'alias', 'unalias', 'type', 'which', 'command', 'builtin', 'enable',
    'help', 'let', 'eval', 'exec', 'source', 'trap', 'umask', 'ulimit', 'set', 'shift'
    );

function DoBashHighlighting(const Source: string): TSyntaxTokenArray;

implementation


  { TBashSyntaxHighlighter }

procedure TBashSyntaxHighlighter.ProcessSingleQuoteString(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening quote
  while (FPos <= Length(FSource)) and (FSource[FPos] <> '''') do
    Inc(FPos);
  if (FPos <= Length(FSource)) and (FSource[FPos] = '''') then
    Inc(FPos); // Include closing quote
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;

procedure TBashSyntaxHighlighter.ProcessDoubleQuoteString(var endPos: integer);
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

procedure TBashSyntaxHighlighter.ProcessBackquoteString(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening backquote
  while (FPos <= Length(FSource)) and (FSource[FPos] <> '`') do
    begin
    if FSource[FPos] = '\' then
      begin
      if FPos < Length(FSource) then
        Inc(FPos); // Skip escaped character
      end;
    Inc(FPos);
    end;
  if (FPos <= Length(FSource)) and (FSource[FPos] = '`') then
    Inc(FPos); // Include closing backquote
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInterpolation);
end;

procedure TBashSyntaxHighlighter.ProcessVariable(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip $

  // Handle special variables like $?, $$, $!, etc.
  if (FPos <= Length(FSource)) and (FSource[FPos] in ['?', '$', '!', '#', '*', '@', '-', '0'..'9']) then
    begin
    Inc(FPos);
    end
  // Handle ${variable} syntax
  else if (FPos <= Length(FSource)) and (FSource[FPos] = '{') then
    begin
    Inc(FPos); // Skip {
    while (FPos <= Length(FSource)) and (FSource[FPos] <> '}') do
      Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] = '}') then
      Inc(FPos); // Include }
    end
  // Handle regular variable names
  else
    begin
    while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
      Inc(FPos);
    end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault); // Variables as default for now
end;

procedure TBashSyntaxHighlighter.ProcessComment(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) do
    Inc(FPos);
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;

function TBashSyntaxHighlighter.CheckForKeyword(var endPos: integer): boolean;
var
  i, j: integer;
  keyword: string;
begin
  i := 0;
  while (FPos+i<=Length(FSource)) and (i < MaxKeywordLength) and IsWordChar(FSource[FPos + i]) do
    Inc(i);
  keyword:=Copy(FSource,FPos,i);
  Result := False;

  for j := 0 to MaxKeyword do
    if BashKeywordTable[j] = keyword then
      begin
      Result := True;
      break;
      end;

  if not Result then
    Exit;

  Inc(FPos, i);
  endPos := FPos - 1;
  AddToken(keyword, shKeyword);
end;

procedure TBashSyntaxHighlighter.ProcessNumber(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
    Inc(FPos);
  // Handle decimal point
  if (FPos <= Length(FSource)) and (FSource[FPos] = '.') then
    begin
    Inc(FPos);
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
      Inc(FPos);
    end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
end;

procedure TBashSyntaxHighlighter.ProcessOperator(var endPos: integer);
var
  startPos: integer;
  ch: char;
begin
  startPos := FPos;
  ch := FSource[FPos];

  // Handle multi-character operators
  case ch of
  '=':
    begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] = '=') then
      Inc(FPos);
    end;
  '!':
    begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] = '=') then
      Inc(FPos);
    end;
  '<':
    begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] in ['=', '<']) then
      Inc(FPos);
    end;
  '>':
    begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] in ['=', '>']) then Inc(FPos);
    end;
  '&':
    begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] = '&') then Inc(FPos);
    end;
  '|':
    begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] = '|') then Inc(FPos);
    end;
  else
    Inc(FPos);
  end;
  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shOperator);
end;

function TBashSyntaxHighlighter.IsWordChar(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_'];
end;

class function TBashSyntaxHighlighter.GetLanguages: TStringDynArray;
begin
  Result:=['bash','sh','zsh']
end;

procedure TBashSyntaxHighlighter.checkcategory;
begin
  if CategoryBash=0 then
    CategoryBash:=RegisterCategory('bash');
end;

constructor TBashSyntaxHighlighter.create;
begin
  inherited create;
  CheckCategory;
  DefaultCategory:=CategoryBash;
end;

function TBashSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  lLen,startPos,endPos: integer;
  ch: char;
begin
  Result:=Nil;
  if Length(Source) = 0 then
    Exit;
  FSource := Source;
  lLen:=Length(FSource);
  FPos := 1;
  endpos:=0;
  while FPos <= lLen do
    begin
    ch := FSource[FPos];
    case ch of
      '#':
        ProcessComment(endPos);
      '''':
        ProcessSingleQuoteString(endPos);
      '"':
        ProcessDoubleQuoteString(endPos);
      '`':
        ProcessBackquoteString(endPos);
      '$':
        ProcessVariable(endPos);
      '0'..'9':
        ProcessNumber(endPos);
      'a'..'z', 'A'..'Z', '_':
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
      '=', '!', '<', '>', '&', '|', '+', '-', '*', '/', '%', '^': ProcessOperator(endPos);
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
    if FPos = endPos then
      Inc(FPos);
    end;
  Result := FTokens.GetTokens;
end;

function DoBashHighlighting(const Source: string): TSyntaxTokenArray;
var
  highlighter: TBashSyntaxHighlighter;
begin
  highlighter := TBashSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

initialization
  TBashSyntaxHighlighter.Register;
end.

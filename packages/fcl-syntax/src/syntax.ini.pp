{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    INI syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.ini;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter;
  {$ELSE}
  Types, SysUtils, syntax.highlighter;
  {$ENDIF}

type

  { TIniSyntaxHighlighter }

  TIniSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
  protected
    procedure ProcessSection(var endPos: integer);
    procedure ProcessKey(var endPos: integer);
    procedure ProcessComment(var endPos: integer);
    procedure ProcessValue(var endPos: integer);
    function IsWordChar(ch: char): boolean;
    function IsKeyChar(ch: char): boolean;
    class procedure CheckCategories;
    class procedure RegisterDefaultCategories; override;
    class function GetLanguages : TStringDynarray; override;
  public
    constructor Create; override;
    class var
      CategoryINI : Integer;
    function Execute(const Source: string): TSyntaxTokenArray; override;
  end;

function DoIniHighlighting(const Source: string): TSyntaxTokenArray;

implementation

{ TIniSyntaxHighlighter }

procedure TIniSyntaxHighlighter.ProcessSection(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening [

  while (FPos <= Length(FSource)) and (FSource[FPos] <> ']') and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) do
    Inc(FPos);

  if (FPos <= Length(FSource)) and (FSource[FPos] = ']') then
    Inc(FPos); // Include closing ]

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shSection);
end;

procedure TIniSyntaxHighlighter.ProcessKey(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;

  while (FPos <= Length(FSource)) and IsKeyChar(FSource[FPos]) do
    Inc(FPos);

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shKey);
end;

procedure TIniSyntaxHighlighter.ProcessComment(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;

  // Process until end of line
  while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) do
    Inc(FPos);

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;

procedure TIniSyntaxHighlighter.ProcessValue(var endPos: integer);
var
  startPos: integer;
  inQuotes: boolean;
  quoteChar: char;
begin
  startPos := FPos;
  inQuotes := False;
  quoteChar := #0;

  // Skip leading whitespace
  while (FPos <= Length(FSource)) and (FSource[FPos] in [' ', #9]) do
    Inc(FPos);

  // Check if value starts with quotes
  if (FPos <= Length(FSource)) and (FSource[FPos] in ['"', '''']) then
  begin
    inQuotes := True;
    quoteChar := FSource[FPos];
    Inc(FPos);
  end;

  if inQuotes then
  begin
    // Process quoted value
    while (FPos <= Length(FSource)) and (FSource[FPos] <> quoteChar) do
    begin
      if (FSource[FPos] = '\') and (FPos < Length(FSource)) then
        Inc(FPos); // Skip escaped character
      Inc(FPos);
    end;
    if (FPos <= Length(FSource)) and (FSource[FPos] = quoteChar) then
      Inc(FPos); // Include closing quote
  end
  else
  begin
    // Process unquoted value until end of line or comment
    while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) and (FSource[FPos] <> ';') and (FSource[FPos] <> '#') do
      Inc(FPos);
  end;

  endPos := FPos - 1;
  if inQuotes then
    AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings)
  else
    AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
end;

function TIniSyntaxHighlighter.IsWordChar(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.'];
end;

function TIniSyntaxHighlighter.IsKeyChar(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.', ' '];
end;

class procedure TIniSyntaxHighlighter.CheckCategories;
begin
  if CategoryINI = 0 then
    RegisterDefaultCategories;
end;

class procedure TIniSyntaxHighlighter.RegisterDefaultCategories;
begin
  CategoryINI := RegisterCategory('INI');
end;

class function TIniSyntaxHighlighter.GetLanguages: TStringDynarray;
begin
  Result := ['ini', 'cfg', 'conf'];
end;

constructor TIniSyntaxHighlighter.Create;
begin
  inherited Create;
  CheckCategories;
  DefaultCategory := CategoryINI;
end;

function TIniSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  lLen, endPos, startPos: integer;
  ch: char;
  atLineStart: boolean;
begin
  Result := Nil;
  CheckCategories;
  lLen := Length(Source);
  if lLen = 0 then
    Exit;
  FSource := Source;
  FTokens.Reset;
  FPos := 1;
  EndPos := 0;
  atLineStart := True;

  while FPos <= lLen do
  begin
    ch := FSource[FPos];
    case ch of
    '[':
      begin
      if atLineStart then
        ProcessSection(endPos)
      else
      begin
        AddToken('[', shSymbol);
        endPos := FPos;
        Inc(FPos);
      end;
      atLineStart := False;
      end;
    ';', '#':
      begin
      ProcessComment(endPos);
      atLineStart := False;
      end;
    '=':
      begin
      AddToken('=', shOperator);
      endPos := FPos;
      Inc(FPos);
      // Process value after =
      if FPos <= Length(FSource) then
        ProcessValue(endPos);
      atLineStart := False;
      end;
    '"', '''':
      begin
      FPos := FPos - 1; // Back up one to include quote in ProcessValue
      ProcessValue(endPos);
      Inc(FPos); // Move past the value
      atLineStart := False;
      end;
    #10, #13:
      begin
      startPos := FPos;
      if (FSource[FPos] = #13) and (FPos < Length(FSource)) and (FSource[FPos + 1] = #10) then
        Inc(FPos, 2) // CRLF
      else
        Inc(FPos); // LF or CR only
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
      atLineStart := True;
      end;
    ' ', #9:
      begin
      startPos := FPos;
      while (FPos <= Length(FSource)) and (FSource[FPos] in [' ', #9]) do
        Inc(FPos);
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
      end;
    'a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.':
      begin
      if atLineStart then
      begin
        // This could be a key
        ProcessKey(endPos);
      end
      else
      begin
        // Regular text
        startPos := FPos;
        while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
      end;
      atLineStart := False;
      end;
    else
      AddToken(ch, shDefault);
      endPos := FPos;
      Inc(FPos);
      atLineStart := False;
    end;
    if FPos = endPos then Inc(FPos);
  end;
  Result := FTokens.GetTokens;
end;

function DoIniHighlighting(const Source: string): TSyntaxTokenArray;
var
  highlighter: TIniSyntaxHighlighter;
begin
  highlighter := TIniSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

initialization
  TIniSyntaxHighlighter.Register;
end.
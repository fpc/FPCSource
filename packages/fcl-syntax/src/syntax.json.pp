{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    JSON syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.json;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter;
  {$ELSE}
  Types, SysUtils, syntax.highlighter;
  {$ENDIF}

type

  { TJsonSyntaxHighlighter }

  TJsonSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
    FExpectingKey: boolean; // Track context to distinguish keys from string values
  protected
    procedure ProcessString(var endPos: integer; isKey: boolean);
    procedure ProcessNumber(var endPos: integer);
    function CheckForKeyword(var endPos: integer): boolean;
    function IsHexChar(ch: char): boolean;
    function IsDigitChar(ch: char): boolean;
    procedure SkipWhitespace;
    class procedure CheckCategories;
    class procedure RegisterDefaultCategories; override;
    class function GetLanguages : TStringDynarray; override;
  public
    constructor Create; override;
    class var
      CategoryJSON : Integer;
    function Execute(const Source: string): TSyntaxTokenArray; override;
  end;

function DoJsonHighlighting(const Source: string): TSyntaxTokenArray;

implementation

{ TJsonSyntaxHighlighter }

procedure TJsonSyntaxHighlighter.ProcessString(var endPos: integer; isKey: boolean);
var
  startPos: integer;
  ch: char;
  kind: TSyntaxHighlightKind;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening quote

  while FPos <= Length(FSource) do
  begin
    ch := FSource[FPos];

    if ch = '"' then
    begin
      Inc(FPos); // Include closing quote
      break;
    end
    else if ch = '\' then
    begin
      // Handle escape sequences
      Inc(FPos);
      if FPos <= Length(FSource) then
      begin
        ch := FSource[FPos];
        case ch of
          '"', '\', '/', 'b', 'f', 'n', 'r', 't':
            Inc(FPos); // Valid single-char escape
          'u':
            begin
            // Unicode escape sequence \uXXXX
            Inc(FPos);
            if FPos + 3 <= Length(FSource) then
            begin
              if IsHexChar(FSource[FPos]) and IsHexChar(FSource[FPos + 1]) and
                 IsHexChar(FSource[FPos + 2]) and IsHexChar(FSource[FPos + 3]) then
                Inc(FPos, 4)
              else
              begin
                // Invalid unicode escape - mark as invalid
                endPos := FPos - 1;
                AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
                Exit;
              end;
            end
            else
            begin
              // Incomplete unicode escape
              endPos := Length(FSource);
              AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
              FPos := Length(FSource) + 1;
              Exit;
            end;
            end;
        else
          // Invalid escape sequence
          endPos := FPos - 1;
          AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
          Exit;
        end;
      end
      else
      begin
        // String ends with backslash
        endPos := Length(FSource);
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
        FPos := Length(FSource) + 1;
        Exit;
      end;
    end
    else if Ord(ch) < 32 then
    begin
      // Control characters must be escaped in JSON
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
      Exit;
    end
    else
      Inc(FPos);
  end;

  endPos := FPos - 1;

  // Check if we reached end of input without finding closing quote
  if (FPos > Length(FSource)) or (FSource[FPos - 1] <> '"') then
  begin
    // Unterminated string - mark as invalid
    AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
    Exit;
  end;

  // Determine if this is a key or a string value
  if isKey then
    kind := shKey
  else
    kind := shStrings;

  AddToken(Copy(FSource, startPos, endPos - startPos + 1), kind);
end;

procedure TJsonSyntaxHighlighter.ProcessNumber(var endPos: integer);
var
  startPos: integer;
  hasDecimal, hasExponent: boolean;
  ch: char;
begin
  startPos := FPos;
  hasDecimal := False;
  hasExponent := False;

  // Handle optional minus sign
  if (FPos <= Length(FSource)) and (FSource[FPos] = '-') then
    Inc(FPos);

  // Must have at least one digit
  if (FPos > Length(FSource)) or not IsDigitChar(FSource[FPos]) then
  begin
    endPos := FPos - 1;
    AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
    Exit;
  end;

  // Handle leading zero (JSON doesn't allow leading zeros except for "0")
  if FSource[FPos] = '0' then
  begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and IsDigitChar(FSource[FPos]) then
    begin
      // Invalid: leading zero followed by another digit
      while (FPos <= Length(FSource)) and (IsDigitChar(FSource[FPos]) or (FSource[FPos] in ['.', 'e', 'E', '+', '-'])) do
        Inc(FPos);
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
      Exit;
    end;
  end
  else
  begin
    // Handle integer part (non-zero leading digit)
    while (FPos <= Length(FSource)) and IsDigitChar(FSource[FPos]) do
      Inc(FPos);
  end;

  // Handle decimal part
  if (FPos <= Length(FSource)) and (FSource[FPos] = '.') then
  begin
    hasDecimal := True;
    Inc(FPos);

    // Must have at least one digit after decimal point
    if (FPos > Length(FSource)) or not IsDigitChar(FSource[FPos]) then
    begin
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
      Exit;
    end;

    while (FPos <= Length(FSource)) and IsDigitChar(FSource[FPos]) do
      Inc(FPos);
  end;

  // Handle exponent part
  if (FPos <= Length(FSource)) and (FSource[FPos] in ['e', 'E']) then
  begin
    hasExponent := True;
    Inc(FPos);

    // Optional sign
    if (FPos <= Length(FSource)) and (FSource[FPos] in ['+', '-']) then
      Inc(FPos);

    // Must have at least one digit after exponent
    if (FPos > Length(FSource)) or not IsDigitChar(FSource[FPos]) then
    begin
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
      Exit;
    end;

    while (FPos <= Length(FSource)) and IsDigitChar(FSource[FPos]) do
      Inc(FPos);
  end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
end;

function TJsonSyntaxHighlighter.CheckForKeyword(var endPos: integer): boolean;
var
  startPos: integer;
  keyword: string;
begin
  Result := False;
  startPos := FPos;

  // Check for 'true'
  if (FPos + 3 <= Length(FSource)) and
     (Copy(FSource, FPos, 4) = 'true') then
  begin
    Inc(FPos, 4);
    endPos := FPos - 1;
    AddToken('true', shKeyword);
    Result := True;
  end
  // Check for 'false'
  else if (FPos + 4 <= Length(FSource)) and
          (Copy(FSource, FPos, 5) = 'false') then
  begin
    Inc(FPos, 5);
    endPos := FPos - 1;
    AddToken('false', shKeyword);
    Result := True;
  end
  // Check for 'null'
  else if (FPos + 3 <= Length(FSource)) and
          (Copy(FSource, FPos, 4) = 'null') then
  begin
    Inc(FPos, 4);
    endPos := FPos - 1;
    AddToken('null', shKeyword);
    Result := True;
  end;
end;

function TJsonSyntaxHighlighter.IsHexChar(ch: char): boolean;
begin
  Result := ch in ['0'..'9', 'A'..'F', 'a'..'f'];
end;

function TJsonSyntaxHighlighter.IsDigitChar(ch: char): boolean;
begin
  Result := ch in ['0'..'9'];
end;

procedure TJsonSyntaxHighlighter.SkipWhitespace;
begin
  while (FPos <= Length(FSource)) and (FSource[FPos] in [' ', #9, #10, #13]) do
    Inc(FPos);
end;

class procedure TJsonSyntaxHighlighter.CheckCategories;
begin
  if CategoryJSON = 0 then
    RegisterDefaultCategories;
end;

class procedure TJsonSyntaxHighlighter.RegisterDefaultCategories;
begin
  CategoryJSON := RegisterCategory('JSON');
end;

class function TJsonSyntaxHighlighter.GetLanguages: TStringDynarray;
begin
  Result := ['json'];
end;

constructor TJsonSyntaxHighlighter.Create;
begin
  inherited Create;
  CheckCategories;
  DefaultCategory := CategoryJSON;
end;

function TJsonSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  lLen, endPos, startPos: integer;
  ch: char;
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
  FExpectingKey := True; // Start expecting a key in root object

  while FPos <= lLen do
  begin
    ch := FSource[FPos];
    case ch of
    '"':
      begin
      ProcessString(endPos, FExpectingKey);
      // After processing a string, update context
      if FExpectingKey then
        FExpectingKey := False; // Next string will be a value
      end;
    '0'..'9', '-':
      ProcessNumber(endPos);
    't', 'f', 'n': // true, false, null
      begin
      if not CheckForKeyword(endPos) then
      begin
        // Invalid identifier
        startPos := FPos;
        while (FPos <= Length(FSource)) and (FSource[FPos] in ['a'..'z', 'A'..'Z']) do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shInvalid);
      end;
      end;
    '{':
      begin
      AddToken('{', shSymbol);
      endPos := FPos;
      Inc(FPos);
      FExpectingKey := True; // After opening brace, expect a key
      end;
    '}':
      begin
      AddToken('}', shSymbol);
      endPos := FPos;
      Inc(FPos);
      FExpectingKey := False; // After closing brace, context depends on parent
      end;
    '[':
      begin
      AddToken('[', shSymbol);
      endPos := FPos;
      Inc(FPos);
      FExpectingKey := False; // Arrays contain values, not keys
      end;
    ']':
      begin
      AddToken(']', shSymbol);
      endPos := FPos;
      Inc(FPos);
      FExpectingKey := False; // After closing array, context depends on parent
      end;
    ':':
      begin
      AddToken(':', shSymbol);
      endPos := FPos;
      Inc(FPos);
      FExpectingKey := False; // After colon, expect a value
      end;
    ',':
      begin
      AddToken(',', shSymbol);
      endPos := FPos;
      Inc(FPos);
      // After comma, context depends on whether we're in object or array
      // For simplicity, we'll assume we might be expecting a key
      // (This is a limitation - proper JSON parsing would maintain a stack)
      FExpectingKey := True;
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

function DoJsonHighlighting(const Source: string): TSyntaxTokenArray;
var
  highlighter: TJsonSyntaxHighlighter;
begin
  highlighter := TJsonSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

initialization
  TJsonSyntaxHighlighter.Register;
end.
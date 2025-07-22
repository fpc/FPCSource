{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt (michael@freepascal.org)

    EBNF scanner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ebnf.scanner;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils;
{$ELSE}
uses sysutils;
{$ENDIF}
// --- Scanner/Tokenizer Definitions ---

type
  EEBNFScanner = class(Exception);

  TEBNFTokenType = (
    ttUnknown,
    ttIdentifier,
    ttStringLiteral,
    ttEquals,       // =
    ttPipe,         // |
    ttOpenParen,    // (
    ttCloseParen,   // )
    ttOpenBracket,  // [
    ttCloseBracket, // ]
    ttOpenBrace,    // {
    ttCloseBrace,   // }
    ttSemicolon,    // ;
    ttQuestion,     // ? (for special sequences)
    ttEOF           // End of File
  );
  TEBNFTokenTypes = set of TEBNFTokenType;

  { TTokenPosition }

  TTokenPosition = record
    Position: Integer;
    Col : Integer;
    Row : integer;
    function ToString : string;
  end;

  TToken = record
    TokenType: TEBNFTokenType;
    Value: string;
    Position : TTokenPosition; // Start position in the source string
  end;

  { TEBNFScanner }

  TEBNFScanner = class
  private
    FSource: string;
    FPos : TTokenPosition;
    FCurrentChar: Char;
    FNewLine : boolean;
    procedure SkipComment;
  protected
    // Returns previous char
    function Advance: Char;
    function CurrentTokenPos : TTokenPosition;
    procedure SkipWhitespace;
    function IsIdentifierStart(aChar: Char): Boolean;
    function IsIdentifierChar(aChar: Char): Boolean;
    function IsDigit(aChar: Char): Boolean;
    function ReadIdentifier: string;
    function ReadStringLiteral(aQuoteChar: Char): string;
  public
    constructor Create(const aSource: string);
    function GetNextToken: TToken;
  end;

implementation

Resourcestring
  SErrUnterminatedComment = 'Unterminated comment block';
  SErrUnknownToken = 'Unknown token: "%s" at position %s';
  SErrUnterminatedString = 'Unterminated string literal';

{ TTokenPosition }

function TTokenPosition.ToString: string;
begin
  Result:=format('pos %d (row: %d, col: %d)',[Position,Row,Col]);
end;

constructor TEBNFScanner.Create(const aSource: string);
begin
  FSource := aSource;
  FPos.Position:=1;
  FNewLine:=True;
  FPos.Col:=0; // Delphi strings are 1-indexed
  FPos.Row:=1;
  FCurrentChar := #0; // Initialize
  if Length(FSource) > 0 then
    FCurrentChar := FSource[FPos.Position];
end;

function TEBNFScanner.Advance: Char;
begin
  Result := FCurrentChar;
  if FCurrentChar=#10 then
    begin
    inc(FPos.Row);
    FPos.Col:=0;
    FNewLine:=True;
    end;
  inc(FPos.Col);
  Inc(FPos.Position);
  if FPos.Position <= Length(FSource) then
    FCurrentChar := FSource[FPos.Position]
  else
    FCurrentChar := #0; // EOF
  FNewLine:=FNewLine and (FCurrentChar=' ');
end;

function TEBNFScanner.CurrentTokenPos: TTokenPosition;
begin
  Result:=FPos;
end;

procedure TEBNFScanner.SkipWhitespace;
begin
  while (FCurrentChar <> #0) and (FCurrentChar <= ' ') do // Includes space, tab, newline, etc.
    Advance;
end;

function TEBNFScanner.IsIdentifierStart(aChar: Char): Boolean;
begin
  Result := (aChar >= 'a') and (aChar <= 'z') or
            (aChar >= 'A') and (aChar <= 'Z');
end;

function TEBNFScanner.IsIdentifierChar(aChar: Char): Boolean;
begin
  Result := IsIdentifierStart(aChar) or IsDigit(aChar) or (aChar = '_');
end;

function TEBNFScanner.IsDigit(aChar: Char): Boolean;
begin
  Result := (aChar >= '0') and (aChar <= '9');
end;

function TEBNFScanner.ReadIdentifier: string;
var
  StartPos: Integer;
begin
  StartPos := FPos.Position;
  while IsIdentifierChar(FCurrentChar) do
    Advance;
  Result := Copy(FSource, StartPos, FPos.Position - StartPos);
end;

function TEBNFScanner.ReadStringLiteral(aQuoteChar: Char): string;
var
  LiteralValue: string;
begin
  Advance; // Consume the opening quote
  LiteralValue := '';
  while (FCurrentChar <> #0) and (FCurrentChar <> aQuoteChar) do
  begin
    LiteralValue := LiteralValue + FCurrentChar;
    Advance;
  end;
  if FCurrentChar <> aQuoteChar then
    raise EEBNFScanner.Create(SErrUnterminatedString);
  Advance; // Consume the closing quote
  Result := LiteralValue;
end;

function TEBNFScanner.GetNextToken: TToken;
var
  HaveComment : Boolean;
begin
  repeat
    SkipWhitespace;
    HaveComment:=(FCurrentChar = '(') and (FPos.Position + 1 <= Length(FSource)) and (FSource[FPos.Position + 1] = '*');
    if HaveComment then
      SkipComment
  until Not HaveComment;

  Result.Position := FPos;
  Result.Value := '';

  if FCurrentChar = #0 then
  begin
    Result.TokenType := ttEOF;
    Exit;
  end;

  if IsIdentifierStart(FCurrentChar) then
  begin
    Result.TokenType := ttIdentifier;
    Result.Value := ReadIdentifier;
    Exit;
  end;

  if (FCurrentChar = '''') or (FCurrentChar = '"') then
  begin
    Result.TokenType := ttStringLiteral;
    Result.Value := ReadStringLiteral(FCurrentChar);
    Exit;
  end;

  case FCurrentChar of
    '=': Result.TokenType := ttEquals;
    '|': Result.TokenType := ttPipe;
    '(': Result.TokenType := ttOpenParen;
    ')': Result.TokenType := ttCloseParen;
    '[': Result.TokenType := ttOpenBracket;
    ']': Result.TokenType := ttCloseBracket;
    '{': Result.TokenType := ttOpenBrace;
    '}': Result.TokenType := ttCloseBrace;
    ';': Result.TokenType := ttSemicolon;
    '?': Result.TokenType := ttQuestion;
    else
      Result.TokenType := ttUnknown;
      Result.Value := FCurrentChar;
      raise EEBNFScanner.CreateFmt(SErrUnknownToken, [FCurrentChar, FPos.ToString]);
  end;
  Advance; // Consume the character
end;

procedure TEBNFScanner.SkipComment;
var
  FoundEnd: Boolean;
begin
  Advance; // Consume '('
  Advance; // Consume '*'
  FoundEnd := False;
  while (FCurrentChar <> #0) and not FoundEnd do
  begin
    if (FCurrentChar = '*') then
    begin
      Advance; // Consume '*'
      if (FCurrentChar = ')') then
      begin
        Advance; // Consume ')'
        FoundEnd := True;
      end;
    end
    else
    begin
      Advance; // Consume any other character
    end;
  end;
  if not FoundEnd then
    raise EEBNFScanner.Create(SErrUnterminatedComment);
end;


end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    JSON highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.json;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.json;

type
  TTestJsonHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    function DoJsonHighlighting(const source: string): TSyntaxTokenArray;
  published
    procedure TestJsonKeys;
    procedure TestJsonStrings;
    procedure TestJsonNumbers;
    procedure TestJsonKeywords;
    procedure TestJsonSymbols;
    procedure TestJsonEscapeSequences;
    procedure TestJsonUnicodeEscapes;
    procedure TestJsonInvalidStrings;
    procedure TestJsonInvalidNumbers;
    procedure TestComplexJsonObject;
    procedure TestJsonArray;
    procedure TestNestedJsonStructures;
    procedure TestJsonWhitespace;
    procedure TestJsonScientificNotation;
    procedure TestJsonEdgeCases;
    procedure TestJsonEmpty;
  end;

implementation

procedure TTestJsonHighlighter.SetUp;
begin
end;

procedure TTestJsonHighlighter.TearDown;
begin
  // Nothing to do
end;

function TTestJsonHighlighter.DoJsonHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: TJsonSyntaxHighlighter;
begin
  highlighter := TJsonSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

procedure TTestJsonHighlighter.TestJsonKeys;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundKey: Boolean;
begin
  // Test simple object with key
  tokens := DoJsonHighlighting('{"name": "value"}');
  foundKey := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"name"') and (tokens[i].Kind = shKey) then
    begin
      foundKey := True;
      break;
    end;
  end;
  AssertTrue('Should find key token', foundKey);

  // Test multiple keys
  tokens := DoJsonHighlighting('{"firstName": "John", "lastName": "Doe"}');
  foundKey := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"firstName"') and (tokens[i].Kind = shKey) then
    begin
      foundKey := True;
      break;
    end;
  end;
  AssertTrue('Should find firstName key', foundKey);
end;

procedure TTestJsonHighlighter.TestJsonStrings;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundString: Boolean;
begin
  // Test string value (not key)
  tokens := DoJsonHighlighting('{"name": "John Doe"}');
  foundString := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"John Doe"') and (tokens[i].Kind = shStrings) then
    begin
      foundString := True;
      break;
    end;
  end;
  AssertTrue('Should find string value token', foundString);

  // Test empty string
  tokens := DoJsonHighlighting('{"empty": ""}');
  foundString := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '""') and (tokens[i].Kind = shStrings) then
    begin
      foundString := True;
      break;
    end;
  end;
  AssertTrue('Should find empty string token', foundString);
end;

procedure TTestJsonHighlighter.TestJsonNumbers;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundNumber: Boolean;
begin
  // Test integer
  tokens := DoJsonHighlighting('{"age": 25}');
  foundNumber := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '25') and (tokens[i].Kind = shNumbers) then
    begin
      foundNumber := True;
      break;
    end;
  end;
  AssertTrue('Should find integer number', foundNumber);

  // Test decimal
  tokens := DoJsonHighlighting('{"price": 19.99}');
  foundNumber := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '19.99') and (tokens[i].Kind = shNumbers) then
    begin
      foundNumber := True;
      break;
    end;
  end;
  AssertTrue('Should find decimal number', foundNumber);

  // Test negative number
  tokens := DoJsonHighlighting('{"temperature": -15}');
  foundNumber := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '-15') and (tokens[i].Kind = shNumbers) then
    begin
      foundNumber := True;
      break;
    end;
  end;
  AssertTrue('Should find negative number', foundNumber);

  // Test zero
  tokens := DoJsonHighlighting('{"zero": 0}');
  foundNumber := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '0') and (tokens[i].Kind = shNumbers) then
    begin
      foundNumber := True;
      break;
    end;
  end;
  AssertTrue('Should find zero', foundNumber);
end;

procedure TTestJsonHighlighter.TestJsonKeywords;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundKeyword: Boolean;
begin
  // Test true
  tokens := DoJsonHighlighting('{"valid": true}');
  foundKeyword := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = 'true') and (tokens[i].Kind = shKeyword) then
    begin
      foundKeyword := True;
      break;
    end;
  end;
  AssertTrue('Should find true keyword', foundKeyword);

  // Test false
  tokens := DoJsonHighlighting('{"valid": false}');
  foundKeyword := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = 'false') and (tokens[i].Kind = shKeyword) then
    begin
      foundKeyword := True;
      break;
    end;
  end;
  AssertTrue('Should find false keyword', foundKeyword);

  // Test null
  tokens := DoJsonHighlighting('{"value": null}');
  foundKeyword := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = 'null') and (tokens[i].Kind = shKeyword) then
    begin
      foundKeyword := True;
      break;
    end;
  end;
  AssertTrue('Should find null keyword', foundKeyword);
end;

procedure TTestJsonHighlighter.TestJsonSymbols;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundSymbol: Boolean;
begin
  tokens := DoJsonHighlighting('{"key": "value"}');

  // Test opening brace
  foundSymbol := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '{') and (tokens[i].Kind = shSymbol) then
    begin
      foundSymbol := True;
      break;
    end;
  end;
  AssertTrue('Should find opening brace', foundSymbol);

  // Test colon
  foundSymbol := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = ':') and (tokens[i].Kind = shSymbol) then
    begin
      foundSymbol := True;
      break;
    end;
  end;
  AssertTrue('Should find colon', foundSymbol);

  // Test closing brace
  foundSymbol := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '}') and (tokens[i].Kind = shSymbol) then
    begin
      foundSymbol := True;
      break;
    end;
  end;
  AssertTrue('Should find closing brace', foundSymbol);
end;

procedure TTestJsonHighlighter.TestJsonEscapeSequences;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundString: Boolean;
begin
  // Test basic escape sequences
  tokens := DoJsonHighlighting('{"message": "Hello\nWorld"}');
  foundString := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"Hello\nWorld"') and (tokens[i].Kind = shStrings) then
    begin
      foundString := True;
      break;
    end;
  end;
  AssertTrue('Should handle newline escape', foundString);

  // Test quote escape
  tokens := DoJsonHighlighting('{"quote": "Say \"Hello\""}');
  foundString := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"Say \"Hello\""') and (tokens[i].Kind = shStrings) then
    begin
      foundString := True;
      break;
    end;
  end;
  AssertTrue('Should handle quote escape', foundString);

  // Test backslash escape
  tokens := DoJsonHighlighting('{"path": "C:\\Windows"}');
  foundString := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"C:\\Windows"') and (tokens[i].Kind = shStrings) then
    begin
      foundString := True;
      break;
    end;
  end;
  AssertTrue('Should handle backslash escape', foundString);
end;

procedure TTestJsonHighlighter.TestJsonUnicodeEscapes;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundString: Boolean;
begin
  // Test unicode escape
  tokens := DoJsonHighlighting('{"unicode": "\u0048\u0065\u006C\u006C\u006F"}');
  foundString := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"\u0048\u0065\u006C\u006C\u006F"') and (tokens[i].Kind = shStrings) then
    begin
      foundString := True;
      break;
    end;
  end;
  AssertTrue('Should handle unicode escapes', foundString);
end;

procedure TTestJsonHighlighter.TestJsonInvalidStrings;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundInvalid: Boolean;
begin
  // Test invalid escape sequence
  tokens := DoJsonHighlighting('{"invalid": "\x"}');
  foundInvalid := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Kind = shInvalid then
    begin
      foundInvalid := True;
      break;
    end;
  end;
  AssertTrue('Should find invalid escape sequence', foundInvalid);

  // Test unterminated string
  tokens := DoJsonHighlighting('{"unterminated": "hello');
  foundInvalid := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Kind = shInvalid then
    begin
      foundInvalid := True;
      break;
    end;
  end;
  AssertTrue('Should find unterminated string', foundInvalid);
end;

procedure TTestJsonHighlighter.TestJsonInvalidNumbers;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundInvalid: Boolean;
begin
  // Test leading zero (invalid in JSON)
  tokens := DoJsonHighlighting('{"invalid": 01}');
  foundInvalid := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Kind = shInvalid then
    begin
      foundInvalid := True;
      break;
    end;
  end;
  AssertTrue('Should find invalid leading zero', foundInvalid);

  // Test incomplete decimal
  tokens := DoJsonHighlighting('{"incomplete": 12.}');
  foundInvalid := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Kind = shInvalid then
    begin
      foundInvalid := True;
      break;
    end;
  end;
  AssertTrue('Should find incomplete decimal', foundInvalid);
end;

procedure TTestJsonHighlighter.TestComplexJsonObject;
var
  tokens: TSyntaxTokenArray;
  jsonText: string;
  i: Integer;
  hasKeys, hasStrings, hasNumbers, hasKeywords, hasSymbols: Boolean;
begin
  jsonText := '{"name": "John", "age": 30, "married": true, "children": null}';
  tokens := DoJsonHighlighting(jsonText);

  AssertTrue('Should have multiple tokens', Length(tokens) > 10);

  // Check for different token types
  hasKeys := False;
  hasStrings := False;
  hasNumbers := False;
  hasKeywords := False;
  hasSymbols := False;

  for i := 0 to High(tokens) do
  begin
    case tokens[i].Kind of
      shKey: hasKeys := True;
      shStrings: hasStrings := True;
      shNumbers: hasNumbers := True;
      shKeyword: hasKeywords := True;
      shSymbol: hasSymbols := True;
    end;
  end;

  AssertTrue('Should contain key tokens', hasKeys);
  AssertTrue('Should contain string tokens', hasStrings);
  AssertTrue('Should contain number tokens', hasNumbers);
  AssertTrue('Should contain keyword tokens', hasKeywords);
  AssertTrue('Should contain symbol tokens', hasSymbols);
end;

procedure TTestJsonHighlighter.TestJsonArray;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundBrackets: Boolean;
begin
  tokens := DoJsonHighlighting('[1, 2, 3]');

  foundBrackets := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '[') and (tokens[i].Kind = shSymbol) then
    begin
      foundBrackets := True;
      break;
    end;
  end;
  AssertTrue('Should find opening bracket', foundBrackets);

  foundBrackets := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = ']') and (tokens[i].Kind = shSymbol) then
    begin
      foundBrackets := True;
      break;
    end;
  end;
  AssertTrue('Should find closing bracket', foundBrackets);
end;

procedure TTestJsonHighlighter.TestNestedJsonStructures;
var
  tokens: TSyntaxTokenArray;
  jsonText: string;
  i: Integer;
  hasKeys, hasNumbers: Boolean;
begin
  jsonText := '{"person": {"name": "John", "address": {"city": "New York", "zip": 10001}}}';
  tokens := DoJsonHighlighting(jsonText);

  AssertTrue('Should handle nested structures', Length(tokens) > 15);

  // Verify we have both keys and numbers in nested structure
  hasKeys := False;
  hasNumbers := False;

  for i := 0 to High(tokens) do
  begin
    case tokens[i].Kind of
      shKey: hasKeys := True;
      shNumbers: hasNumbers := True;
    end;
  end;

  AssertTrue('Should contain keys in nested structure', hasKeys);
  AssertTrue('Should contain numbers in nested structure', hasNumbers);
end;

procedure TTestJsonHighlighter.TestJsonWhitespace;
var
  tokens: TSyntaxTokenArray;
  jsonText: string;
  i: Integer;
  foundWhitespace: Boolean;
begin
  jsonText := '{' + #10 + '  "name": "John"' + #10 + '}';
  tokens := DoJsonHighlighting(jsonText);

  foundWhitespace := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Kind = shDefault then
    begin
      foundWhitespace := True;
      break;
    end;
  end;
  AssertTrue('Should handle whitespace', foundWhitespace);
end;

procedure TTestJsonHighlighter.TestJsonScientificNotation;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundNumber: Boolean;
begin
  // Test scientific notation
  tokens := DoJsonHighlighting('{"scientific": 1.23e-4}');
  foundNumber := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '1.23e-4') and (tokens[i].Kind = shNumbers) then
    begin
      foundNumber := True;
      break;
    end;
  end;
  AssertTrue('Should handle scientific notation', foundNumber);

  // Test with capital E
  tokens := DoJsonHighlighting('{"scientific": 2.5E+10}');
  foundNumber := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '2.5E+10') and (tokens[i].Kind = shNumbers) then
    begin
      foundNumber := True;
      break;
    end;
  end;
  AssertTrue('Should handle capital E notation', foundNumber);
end;

procedure TTestJsonHighlighter.TestJsonEdgeCases;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundKey, foundString: Boolean;
begin
  // Test single character key and value
  tokens := DoJsonHighlighting('{"a": "b"}');
  foundKey := False;
  foundString := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"a"') and (tokens[i].Kind = shKey) then
      foundKey := True;
    if (tokens[i].Text = '"b"') and (tokens[i].Kind = shStrings) then
      foundString := True;
  end;
  AssertTrue('Should handle single character key', foundKey);
  AssertTrue('Should handle single character string', foundString);
end;

procedure TTestJsonHighlighter.TestJsonEmpty;
var
  tokens: TSyntaxTokenArray;
begin
  // Test empty object
  tokens := DoJsonHighlighting('{}');
  AssertTrue('Should handle empty object', Length(tokens) >= 2);

  // Test empty array
  tokens := DoJsonHighlighting('[]');
  AssertTrue('Should handle empty array', Length(tokens) >= 2);
end;

initialization
  RegisterTest(TTestJsonHighlighter);
end.
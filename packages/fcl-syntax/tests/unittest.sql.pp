{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    SQL highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.sql;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.sql;

type
  TTestSqlHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    function DoSqlHighlighting(const source: string): TSyntaxTokenArray;
    function DoSqlHighlightingWithMode(const source: string; mode: TSqlStringEscapeMode): TSyntaxTokenArray;
  published
    procedure TestSqlKeywords;
    procedure TestSqlStringsDoubledEscape;
    procedure TestSqlStringsBackslashEscape;
    procedure TestSqlStringEscapeModeProperty;
    procedure TestSqlNumbers;
    procedure TestSqlComments;
    procedure TestSqlOperators;
    procedure TestSqlSymbols;
    procedure TestComplexSqlQuery;
    procedure TestSqlDataTypes;
    procedure TestSqlFunctions;
    procedure TestSqlJoins;
    procedure TestHexNumbers;
    procedure TestScientificNotation;
    procedure TestMultiCharOperators;
    procedure TestNestedComments;
  end;

implementation

procedure TTestSqlHighlighter.SetUp;
begin
end;

procedure TTestSqlHighlighter.TearDown;
begin
  // Nothing to do
end;

function TTestSqlHighlighter.DoSqlHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: TSqlSyntaxHighlighter;
begin
  highlighter := TSqlSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

function TTestSqlHighlighter.DoSqlHighlightingWithMode(const source: string; mode: TSqlStringEscapeMode): TSyntaxTokenArray;
var
  highlighter: TSqlSyntaxHighlighter;
begin
  highlighter := TSqlSyntaxHighlighter.Create;
  try
    highlighter.StringEscapeMode := mode;
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

procedure TTestSqlHighlighter.TestSqlKeywords;
const
  Keywords: array[0..9] of string = (
    'SELECT', 'FROM', 'WHERE', 'INSERT', 'UPDATE', 'DELETE', 'CREATE', 'TABLE', 'JOIN', 'ORDER'
  );
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(Keywords) do
  begin
    tokens := DoSqlHighlighting(Keywords[i]);
    AssertEquals('Should have 1 token for ' + Keywords[i], 1, Length(tokens));
    AssertEquals('Token should be ' + Keywords[i], Keywords[i], tokens[0].Text);
    AssertEquals(Keywords[i] + ' should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));

    // Test lowercase version
    tokens := DoSqlHighlighting(LowerCase(Keywords[i]));
    AssertEquals('Should have 1 token for ' + LowerCase(Keywords[i]), 1, Length(tokens));
    AssertEquals('Token should be ' + LowerCase(Keywords[i]), LowerCase(Keywords[i]), tokens[0].Text);
    AssertEquals(LowerCase(Keywords[i]) + ' should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
  end;
end;

procedure TTestSqlHighlighter.TestSqlStringsDoubledEscape;
var
  tokens: TSyntaxTokenArray;
begin
  // Test simple single-quoted string with doubled escaping (default mode)
  tokens := DoSqlHighlighting('''Hello World''');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be single-quoted string', '''Hello World''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test string with escaped single quote (doubled)
  tokens := DoSqlHighlighting('''Can''''t do it''');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be escaped string', '''Can''''t do it''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test double-quoted string with escaped double quote (doubled)
  tokens := DoSqlHighlighting('"Say ""Hello"""');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be escaped double-quoted string', '"Say ""Hello"""', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));
end;

procedure TTestSqlHighlighter.TestSqlStringsBackslashEscape;
var
  tokens: TSyntaxTokenArray;
begin
  // Test simple single-quoted string with backslash escaping
  tokens := DoSqlHighlightingWithMode('''Hello World''', semBackslash);
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be single-quoted string', '''Hello World''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test string with escaped single quote (backslash)
  tokens := DoSqlHighlightingWithMode('''Can\''t do it''', semBackslash);
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be escaped string', '''Can\''t do it''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test string with escaped backslash
  tokens := DoSqlHighlightingWithMode('''Path\\to\\file''', semBackslash);
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be escaped string', '''Path\\to\\file''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));
end;

procedure TTestSqlHighlighter.TestSqlStringEscapeModeProperty;
var
  highlighter: TSqlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
begin
  highlighter := TSqlSyntaxHighlighter.Create;
  try
    // Test default mode
    AssertEquals('Default should be doubled escaping', Ord(semDoubled), Ord(highlighter.StringEscapeMode));

    // Test setting backslash mode
    highlighter.StringEscapeMode := semBackslash;
    AssertEquals('Should be backslash escaping', Ord(semBackslash), Ord(highlighter.StringEscapeMode));

    // Test that mode affects string parsing
    tokens := highlighter.Execute('''Can\''t''');
    AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
    AssertEquals('Should parse as single string token', '''Can\''t''', tokens[0].Text);
    AssertEquals('Should be string token', Ord(shStrings), Ord(tokens[0].Kind));
  finally
    highlighter.Free;
  end;
end;

procedure TTestSqlHighlighter.TestSqlNumbers;
var
  tokens: TSyntaxTokenArray;
begin
  // Test integer
  tokens := DoSqlHighlighting('123');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be integer', '123', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test decimal
  tokens := DoSqlHighlighting('123.45');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be decimal', '123.45', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test scientific notation
  tokens := DoSqlHighlighting('1.23E-4');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be scientific notation', '1.23E-4', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestSqlHighlighter.TestSqlComments;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundComment: Boolean;
begin
  // Test single-line comment
  tokens := DoSqlHighlighting('-- This is a comment');
  foundComment := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Kind = shComment) and (Pos('--', tokens[i].Text) = 1) then
    begin
      foundComment := True;
      break;
    end;
  end;
  AssertTrue('Should find single-line comment', foundComment);

  // Test multi-line comment
  tokens := DoSqlHighlighting('/* Multi-line comment */');
  foundComment := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Kind = shComment) and (Pos('/*', tokens[i].Text) = 1) then
    begin
      foundComment := True;
      break;
    end;
  end;
  AssertTrue('Should find multi-line comment', foundComment);
end;

procedure TTestSqlHighlighter.TestSqlOperators;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundOperator: Boolean;
begin
  // Test equals operator
  tokens := DoSqlHighlighting('=');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be equals', '=', tokens[0].Text);
  AssertEquals('Token should be operator', Ord(shOperator), Ord(tokens[0].Kind));

  // Test not equals operator
  tokens := DoSqlHighlighting('!=');
  foundOperator := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '!=') and (tokens[i].Kind = shOperator) then
    begin
      foundOperator := True;
      break;
    end;
  end;
  AssertTrue('Should find != operator', foundOperator);

  // Test less than or equal
  tokens := DoSqlHighlighting('<=');
  foundOperator := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '<=') and (tokens[i].Kind = shOperator) then
    begin
      foundOperator := True;
      break;
    end;
  end;
  AssertTrue('Should find <= operator', foundOperator);
end;

procedure TTestSqlHighlighter.TestSqlSymbols;
var
  tokens: TSyntaxTokenArray;
begin
  // Test parentheses
  tokens := DoSqlHighlighting('(');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be opening parenthesis', '(', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));

  // Test semicolon
  tokens := DoSqlHighlighting(';');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be semicolon', ';', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));
end;

procedure TTestSqlHighlighter.TestComplexSqlQuery;
var
  tokens: TSyntaxTokenArray;
  sqlQuery: string;
  i: Integer;
  hasKeywords, hasStrings, hasSymbols, hasNumbers: Boolean;
begin
  sqlQuery := 'SELECT name, age FROM users WHERE age > 18 AND name = ''John'';';
  tokens := DoSqlHighlighting(sqlQuery);

  AssertTrue('Should have multiple tokens', Length(tokens) > 10);

  // Check that we have different token types
  hasKeywords := False;
  hasStrings := False;
  hasSymbols := False;
  hasNumbers := False;

  for i := 0 to High(tokens) do
  begin
    case tokens[i].Kind of
      shKeyword: hasKeywords := True;
      shStrings: hasStrings := True;
      shSymbol: hasSymbols := True;
      shNumbers: hasNumbers := True;
    end;
  end;

  AssertTrue('Should contain keyword tokens', hasKeywords);
  AssertTrue('Should contain string tokens', hasStrings);
  AssertTrue('Should contain symbol tokens', hasSymbols);
  AssertTrue('Should contain number tokens', hasNumbers);
end;

procedure TTestSqlHighlighter.TestSqlDataTypes;
const
  DataTypes: array[0..4] of string = ('INTEGER', 'VARCHAR', 'DATE', 'DECIMAL', 'BOOLEAN');
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(DataTypes) do
  begin
    tokens := DoSqlHighlighting(DataTypes[i]);
    AssertEquals('Should have 1 token for ' + DataTypes[i], 1, Length(tokens));
    AssertEquals('Token should be ' + DataTypes[i], DataTypes[i], tokens[0].Text);
    AssertEquals(DataTypes[i] + ' should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
  end;
end;

procedure TTestSqlHighlighter.TestSqlFunctions;
const
  Functions: array[0..4] of string = ('COUNT', 'SUM', 'MAX', 'MIN', 'AVG');
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(Functions) do
  begin
    tokens := DoSqlHighlighting(Functions[i]);
    AssertEquals('Should have 1 token for ' + Functions[i], 1, Length(tokens));
    AssertEquals('Token should be ' + Functions[i], Functions[i], tokens[0].Text);
    AssertEquals(Functions[i] + ' should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
  end;
end;

procedure TTestSqlHighlighter.TestSqlJoins;
const
  JoinKeywords: array[0..4] of string = ('JOIN', 'INNER', 'LEFT', 'RIGHT', 'OUTER');
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(JoinKeywords) do
  begin
    tokens := DoSqlHighlighting(JoinKeywords[i]);
    AssertEquals('Should have 1 token for ' + JoinKeywords[i], 1, Length(tokens));
    AssertEquals('Token should be ' + JoinKeywords[i], JoinKeywords[i], tokens[0].Text);
    AssertEquals(JoinKeywords[i] + ' should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
  end;
end;

procedure TTestSqlHighlighter.TestHexNumbers;
var
  tokens: TSyntaxTokenArray;
begin
  // Test hexadecimal number (some SQL dialects support $-prefixed hex)
  tokens := DoSqlHighlighting('$DEADBEEF');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be hex number', '$DEADBEEF', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test shorter hex number
  tokens := DoSqlHighlighting('$FF');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be hex number', '$FF', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestSqlHighlighter.TestScientificNotation;
var
  tokens: TSyntaxTokenArray;
begin
  // Test positive exponent
  tokens := DoSqlHighlighting('1.23E+10');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be scientific notation', '1.23E+10', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test lowercase e
  tokens := DoSqlHighlighting('2.5e-3');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be scientific notation', '2.5e-3', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestSqlHighlighter.TestMultiCharOperators;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundOperator: Boolean;
begin
  // Test >= operator
  tokens := DoSqlHighlighting('>=');
  foundOperator := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '>=') and (tokens[i].Kind = shOperator) then
    begin
      foundOperator := True;
      break;
    end;
  end;
  AssertTrue('Should find >= operator', foundOperator);

  // Test <> operator (not equal in some SQL dialects)
  tokens := DoSqlHighlighting('<>');
  foundOperator := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '<>') and (tokens[i].Kind = shOperator) then
    begin
      foundOperator := True;
      break;
    end;
  end;
  AssertTrue('Should find <> operator', foundOperator);
end;

procedure TTestSqlHighlighter.TestNestedComments;
var
  tokens: TSyntaxTokenArray;
  sqlWithComment: string;
  i: Integer;
  hasKeywords, hasComments: Boolean;
begin
  sqlWithComment := 'SELECT * /* This is a comment */ FROM table1;';
  tokens := DoSqlHighlighting(sqlWithComment);

  AssertTrue('Should have multiple tokens', Length(tokens) > 5);

  hasKeywords := False;
  hasComments := False;

  for i := 0 to High(tokens) do
  begin
    case tokens[i].Kind of
      shKeyword: hasKeywords := True;
      shComment: hasComments := True;
    end;
  end;

  AssertTrue('Should contain keyword tokens', hasKeywords);
  AssertTrue('Should contain comment tokens', hasComments);
end;

initialization
  RegisterTest(TTestSqlHighlighter);
end.

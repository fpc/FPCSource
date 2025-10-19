{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    INI highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.ini;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.ini;

type
  TTestIniHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    function DoIniHighlighting(const source: string): TSyntaxTokenArray;
  published
    procedure TestIniSections;
    procedure TestIniKeys;
    procedure TestIniComments;
    procedure TestIniValues;
    procedure TestIniQuotedValues;
    procedure TestIniSymbols;
    procedure TestCompleteIniFile;
    procedure TestEmptyLines;
    procedure TestSemicolonComment;
    procedure TestHashComment;
    procedure TestKeyWithSpaces;
  end;

implementation

procedure TTestIniHighlighter.SetUp;
begin
end;

procedure TTestIniHighlighter.TearDown;
begin
  // Nothing to do
end;

function TTestIniHighlighter.DoIniHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: TIniSyntaxHighlighter;
begin
  highlighter := TIniSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

procedure TTestIniHighlighter.TestIniSections;
var
  tokens: TSyntaxTokenArray;
begin
  // Test simple section
  tokens := DoIniHighlighting('[General]');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  AssertEquals('Token should be [General]', '[General]', tokens[0].Text);
  AssertEquals('Token should be section', Ord(shSection), Ord(tokens[0].Kind));

  // Test section with spaces
  tokens := DoIniHighlighting('[My Section]');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  AssertEquals('Token should be [My Section]', '[My Section]', tokens[0].Text);
  AssertEquals('Token should be section', Ord(shSection), Ord(tokens[0].Kind));

  // Test section with numbers
  tokens := DoIniHighlighting('[Section123]');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  AssertEquals('Token should be [Section123]', '[Section123]', tokens[0].Text);
  AssertEquals('Token should be section', Ord(shSection), Ord(tokens[0].Kind));
end;

procedure TTestIniHighlighter.TestIniKeys;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundKey: Boolean;
begin
  // Test simple key
  tokens := DoIniHighlighting('username=admin');
  foundKey := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = 'username') and (tokens[i].Kind = shKey) then
    begin
      foundKey := True;
      break;
    end;
  end;
  AssertTrue('Should find key token', foundKey);

  // Test key with spaces
  tokens := DoIniHighlighting('user name=admin');
  foundKey := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = 'user name') and (tokens[i].Kind = shKey) then
    begin
      foundKey := True;
      break;
    end;
  end;
  AssertTrue('Should find key token with spaces', foundKey);

  // Test key with underscore
  tokens := DoIniHighlighting('user_name=admin');
  foundKey := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = 'user_name') and (tokens[i].Kind = shKey) then
    begin
      foundKey := True;
      break;
    end;
  end;
  AssertTrue('Should find key token with underscore', foundKey);
end;

procedure TTestIniHighlighter.TestIniComments;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundComment: Boolean;
begin
  // Test semicolon comment
  tokens := DoIniHighlighting('; This is a comment');
  foundComment := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '; This is a comment') and (tokens[i].Kind = shComment) then
    begin
      foundComment := True;
      break;
    end;
  end;
  AssertTrue('Should find semicolon comment', foundComment);

  // Test hash comment
  tokens := DoIniHighlighting('# This is also a comment');
  foundComment := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '# This is also a comment') and (tokens[i].Kind = shComment) then
    begin
      foundComment := True;
      break;
    end;
  end;
  AssertTrue('Should find hash comment', foundComment);
end;

procedure TTestIniHighlighter.TestIniValues;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundValue: Boolean;
begin
  // Test simple value
  tokens := DoIniHighlighting('key=value');
  foundValue := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Text = 'value' then
    begin
      foundValue := True;
      AssertEquals('Value should be default token', Ord(shDefault), Ord(tokens[i].Kind));
      break;
    end;
  end;
  AssertTrue('Should find value token', foundValue);

  // Test numeric value
  tokens := DoIniHighlighting('port=8080');
  foundValue := False;
  for i := 0 to High(tokens) do
  begin
    if tokens[i].Text = '8080' then
    begin
      foundValue := True;
      break;
    end;
  end;
  AssertTrue('Should find numeric value', foundValue);
end;

procedure TTestIniHighlighter.TestIniQuotedValues;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundQuotedValue: Boolean;
begin
  // Test double-quoted value
  tokens := DoIniHighlighting('name="John Doe"');
  foundQuotedValue := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '"John Doe"') and (tokens[i].Kind = shStrings) then
    begin
      foundQuotedValue := True;
      break;
    end;
  end;
  AssertTrue('Should find double-quoted value as string', foundQuotedValue);

  // Test single-quoted value
  tokens := DoIniHighlighting('path=''C:\Program Files''');
  foundQuotedValue := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '''C:\Program Files''') and (tokens[i].Kind = shStrings) then
    begin
      foundQuotedValue := True;
      break;
    end;
  end;
  AssertTrue('Should find single-quoted value as string', foundQuotedValue);
end;

procedure TTestIniHighlighter.TestIniSymbols;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundEquals: Boolean;
begin
  // Test equals sign
  tokens := DoIniHighlighting('key=value');
  foundEquals := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Text = '=') and (tokens[i].Kind = shOperator) then
    begin
      foundEquals := True;
      break;
    end;
  end;
  AssertTrue('Should find equals sign as operator', foundEquals);
end;

procedure TTestIniHighlighter.TestCompleteIniFile;
var
  tokens: TSyntaxTokenArray;
  iniContent: string;
  i: Integer;
  hasSections, hasKeys, hasValues, hasComments: Boolean;
begin
  iniContent := '[Database]' + #13#10 +
                'host=localhost' + #13#10 +
                'port=3306' + #13#10 +
                '; Connection timeout in seconds' + #13#10 +
                'timeout=30' + #13#10 +
                #13#10 +
                '[Application]' + #13#10 +
                'name="My App"' + #13#10 +
                'debug=true';

  tokens := DoIniHighlighting(iniContent);

  AssertTrue('Should have multiple tokens', Length(tokens) > 10);

  // Check that we have different token types
  hasSections := False;
  hasKeys := False;
  hasValues := False;
  hasComments := False;

  for i := 0 to High(tokens) do
  begin
    case tokens[i].Kind of
      shSection: hasSections := True;
      shKey: hasKeys := True;
      shDefault: hasValues := True;
      shComment: hasComments := True;
    end;
  end;

  AssertTrue('Should contain section tokens', hasSections);
  AssertTrue('Should contain key tokens', hasKeys);
  AssertTrue('Should contain value tokens', hasValues);
  AssertTrue('Should contain comment tokens', hasComments);
end;

procedure TTestIniHighlighter.TestEmptyLines;
var
  tokens: TSyntaxTokenArray;
  iniContent: string;
begin
  iniContent := '[Section1]' + #13#10 +
                'key1=value1' + #13#10 +
                #13#10 +  // Empty line
                'key2=value2';

  tokens := DoIniHighlighting(iniContent);
  AssertTrue('Should handle empty lines without crashing', Length(tokens) > 0);
end;

procedure TTestIniHighlighter.TestSemicolonComment;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundComment: Boolean;
begin
  tokens := DoIniHighlighting('; Configuration file');
  foundComment := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Kind = shComment) and (Pos(';', tokens[i].Text) = 1) then
    begin
      foundComment := True;
      break;
    end;
  end;
  AssertTrue('Should recognize semicolon comments', foundComment);
end;

procedure TTestIniHighlighter.TestHashComment;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundComment: Boolean;
begin
  tokens := DoIniHighlighting('# This is a hash comment');
  foundComment := False;
  for i := 0 to High(tokens) do
  begin
    if (tokens[i].Kind = shComment) and (Pos('#', tokens[i].Text) = 1) then
    begin
      foundComment := True;
      break;
    end;
  end;
  AssertTrue('Should recognize hash comments', foundComment);
end;

procedure TTestIniHighlighter.TestKeyWithSpaces;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundKey: Boolean;
begin
  tokens := DoIniHighlighting('display name = John Doe');
  foundKey := False;
  for i := 0 to High(tokens) do
  begin
    // Writeln('*',tokens[i].Text,'*');
    if (tokens[i].Kind = shKey) and (tokens[i].Text = 'display name ') then
    begin
      foundKey := True;
      break;
    end;
  end;
  AssertTrue('Should handle keys with spaces', foundKey);
end;

initialization
  RegisterTest(TTestIniHighlighter);
end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Bash highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit unittest.bash;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.bash;

type
  TTestBashHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    function DoBashHighlighting(const source: string): TSyntaxTokenArray;
  published
    procedure TestBashKeywords;
    procedure TestNonKeywordCommands;
    procedure TestBashStrings;
    procedure TestBashComments;
    procedure TestBashVariables;
    procedure TestBashNumbers;
    procedure TestBashOperators;
    procedure TestBashCommandSubstitution;
    procedure TestBashSymbols;
    procedure TestComplexBashScript;
    procedure TestBashStringTypes;
  end;

implementation

procedure TTestBashHighlighter.SetUp;
begin

end;

procedure TTestBashHighlighter.TearDown;
begin
  // Nothing to do
end;

function TTestBashHighlighter.DoBashHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: TBashSyntaxHighlighter;
begin
  highlighter := TBashSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

procedure TTestBashHighlighter.TestBashKeywords;
const
  Keywords: array[0..18] of string = (
    'if', 'then', 'else', 'fi', 'case', 'esac', 'for', 'do', 'done', 'while',
    'function', 'return', 'break', 'continue', 'declare', 'local', 'export', 'set', 'eval'
  );
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(Keywords) do
    begin
    tokens := DoBashHighlighting(Keywords[i]);
    AssertEquals('Should have 1 token for ' + Keywords[i], 1, Length(tokens));
    AssertEquals('Token should be ' + Keywords[i], Keywords[i], tokens[0].Text);
    AssertEquals(Keywords[i] + ' should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
    end;
end;

procedure TTestBashHighlighter.TestNonKeywordCommands;
var
  tokens: TSyntaxTokenArray;
begin
  // 'test' is not recognized as a keyword in the bash highlighter
  tokens := DoBashHighlighting('test');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be test', 'test', tokens[0].Text);
  AssertEquals('test should be default (not keyword)', Ord(shDefault), Ord(tokens[0].Kind));
end;

procedure TTestBashHighlighter.TestBashStrings;
var
  tokens: TSyntaxTokenArray;
begin
  // Test single-quoted string
  tokens := DoBashHighlighting('''hello world''');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be single-quoted string', '''hello world''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test double-quoted string (may have variable expansion)
  tokens := DoBashHighlighting('"hello $USER"');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  AssertEquals('First token should be string type', Ord(shStrings), Ord(tokens[0].Kind));

  // Test simple double-quoted string
  tokens := DoBashHighlighting('"hello world"');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be double-quoted string', '"hello world"', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));
end;

procedure TTestBashHighlighter.TestBashComments;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoBashHighlighting('# This is a comment');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be comment', '# This is a comment', tokens[0].Text);
  AssertEquals('Token should be comment type', Ord(shComment), Ord(tokens[0].Kind));
end;

procedure TTestBashHighlighter.TestBashVariables;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoBashHighlighting('$USER');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be variable', '$USER', tokens[0].Text);
  AssertEquals('Token should be default type', Ord(shDefault), Ord(tokens[0].Kind));

  // Test variable in braces
  tokens := DoBashHighlighting('${HOME}');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be braced variable', '${HOME}', tokens[0].Text);
  AssertEquals('Token should be default type', Ord(shDefault), Ord(tokens[0].Kind));
end;

procedure TTestBashHighlighter.TestBashNumbers;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoBashHighlighting('123');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be number', '123', tokens[0].Text);
  AssertEquals('Token should be number type', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test floating point
  tokens := DoBashHighlighting('3.14');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  // First token should be the integer part
  AssertEquals('First token should be number type', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestBashHighlighter.TestBashOperators;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoBashHighlighting('==');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be equality operator', '==', tokens[0].Text);
  AssertEquals('Token should be operator type', Ord(shOperator), Ord(tokens[0].Kind));

  tokens := DoBashHighlighting('!=');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be inequality operator', '!=', tokens[0].Text);
  AssertEquals('Token should be operator type', Ord(shOperator), Ord(tokens[0].Kind));

  tokens := DoBashHighlighting('&&');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be logical AND', '&&', tokens[0].Text);
  AssertEquals('Token should be operator type', Ord(shOperator), Ord(tokens[0].Kind));
end;

procedure TTestBashHighlighter.TestBashCommandSubstitution;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoBashHighlighting('`date`');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be command substitution', '`date`', tokens[0].Text);
  AssertEquals('Token should be interpolation type', Ord(shInterpolation), Ord(tokens[0].Kind));

  // Test $(command) syntax
  tokens := DoBashHighlighting('$(whoami)');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  // Should contain interpolation tokens
end;

procedure TTestBashHighlighter.TestBashSymbols;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoBashHighlighting('[');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be opening bracket', '[', tokens[0].Text);
  AssertEquals('Token should be symbol type', Ord(shSymbol), Ord(tokens[0].Kind));

  tokens := DoBashHighlighting(';');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be semicolon', ';', tokens[0].Text);
  AssertEquals('Token should be symbol type', Ord(shSymbol), Ord(tokens[0].Kind));

  tokens := DoBashHighlighting('|');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be pipe', '|', tokens[0].Text);
  AssertEquals('Token should be operator type', Ord(shOperator), Ord(tokens[0].Kind));
end;

procedure TTestBashHighlighter.TestComplexBashScript;
var
  tokens: TSyntaxTokenArray;
  script: string;
  i: Integer;
  hasKeywords, hasStrings, hasOperators, hasSymbols: Boolean;
begin
  script := 'if [ "$USER" == "root" ]; then echo "Admin user"; fi';
  tokens := DoBashHighlighting(script);

  AssertTrue('Should have multiple tokens', Length(tokens) > 10);

  // Check that we have different token types
  hasKeywords := False;
  hasStrings := False;
  hasOperators := False;
  hasSymbols := False;

  for i := 0 to High(tokens) do
    begin
    case tokens[i].Kind of
      shKeyword: hasKeywords := True;
      shStrings: hasStrings := True;
      shOperator: hasOperators := True;
      shSymbol: hasSymbols := True;
    end;
    end;

  AssertTrue('Should contain keyword tokens', hasKeywords);
  AssertTrue('Should contain string tokens', hasStrings);
  AssertTrue('Should contain operator tokens', hasOperators);
  AssertTrue('Should contain symbol tokens', hasSymbols);

  // First token should be 'if' keyword
  AssertEquals('First token should be if', 'if', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));

  // Last token should be 'fi' keyword
  AssertEquals('Last token should be fi', 'fi', tokens[High(tokens)].Text);
  AssertEquals('Last token should be keyword', Ord(shKeyword), Ord(tokens[High(tokens)].Kind));
end;

procedure TTestBashHighlighter.TestBashStringTypes;
var
  tokens: TSyntaxTokenArray;
begin
  // Test heredoc-style strings or other bash string features
  tokens := DoBashHighlighting('echo "test"');
  AssertTrue('Should have multiple tokens', Length(tokens) >= 3);

  // Should have echo as default (command), space as default, and "test" as string
  AssertEquals('First token should be echo', 'echo', tokens[0].Text);
  AssertEquals('First token should be default', Ord(shDefault), Ord(tokens[0].Kind));

  AssertEquals('Second token should be space', ' ', tokens[1].Text);
  AssertEquals('Second token should be default', Ord(shDefault), Ord(tokens[1].Kind));

  AssertEquals('Third token should be quoted string', '"test"', tokens[2].Text);
  AssertEquals('Third token should be string', Ord(shStrings), Ord(tokens[2].Kind));
end;

initialization
  RegisterTest(TTestBashHighlighter);
end.

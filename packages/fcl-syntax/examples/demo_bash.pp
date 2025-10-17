program test_bash;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.bash;

procedure TestBashKeywords;
const
  BashKeywords: array[0..19] of string = (
    'if', 'then', 'else', 'fi', 'case', 'esac', 'for', 'do', 'done', 'while',
    'function', 'return', 'break', 'continue', 'declare', 'local', 'export', 'set', 'test', 'eval'
  );
var
  highlighter: TBashSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  WriteLn('Testing Bash Keywords:');
  WriteLn('=====================');

  highlighter := TBashSyntaxHighlighter.Create;
  try
    for i := 0 to High(BashKeywords) do begin
      tokens := highlighter.Execute(BashKeywords[i]);

      if (Length(tokens) = 1) and (tokens[0].Kind = shKeyword) then
        WriteLn(BashKeywords[i] + ': PASS - recognized as keyword')
      else
        WriteLn(BashKeywords[i] + ': FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));
    end;
  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestBashTokenTypes;
var
  highlighter: TBashSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
begin
  WriteLn('Testing Bash Token Types:');
  WriteLn('========================');

  highlighter := TBashSyntaxHighlighter.Create;
  try

    // Test single-quoted string
    tokens := highlighter.Execute('''hello world''');
    if (Length(tokens) = 1) and (tokens[0].Kind = shStrings) then
      WriteLn('Single-quoted string: PASS')
    else
      WriteLn('Single-quoted string: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test double-quoted string
    tokens := highlighter.Execute('"hello $USER"');
    if (Length(tokens) >= 1) and (tokens[0].Kind = shStrings) then
      WriteLn('Double-quoted string: PASS')
    else
      WriteLn('Double-quoted string: FAIL');


    // Test comment
    tokens := highlighter.Execute('# This is a comment');
    if (Length(tokens) = 1) and (tokens[0].Kind = shComment) then
      WriteLn('Comment: PASS')
    else
      WriteLn('Comment: FAIL');


    // Test variable
    tokens := highlighter.Execute('$USER');
    if (Length(tokens) = 1) and (tokens[0].Kind = shDefault) then
      WriteLn('Variable: PASS')
    else
      WriteLn('Variable: FAIL');


    // Test number
    tokens := highlighter.Execute('123');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Number: PASS')
    else
      WriteLn('Number: FAIL');


    // Test operator
    tokens := highlighter.Execute('==');
    if (Length(tokens) = 1) and (tokens[0].Kind = shOperator) then
      WriteLn('Operator: PASS')
    else
      WriteLn('Operator: FAIL');


    // Test backquote command substitution
    tokens := highlighter.Execute('`date`');
    if (Length(tokens) = 1) and (tokens[0].Kind = shInterpolation) then
      WriteLn('Command substitution: PASS')
    else
      WriteLn('Command substitution: FAIL');

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestBashScript;
var
  highlighter: TBashSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  script: string;
begin
  WriteLn('Testing Complete Bash Script:');
  WriteLn('============================');

  script := '|';

  highlighter := TBashSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(script);

    WriteLn('Script: ', script);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
              ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestCategorySystem;
var
  categories: TStringList;
  bashCategoryID: Integer;
  i: Integer;
begin
  WriteLn('Testing Category System for Bash:');
  WriteLn('=================================');

  bashCategoryID := TSyntaxHighlighter.RegisterCategory('Bash');
  WriteLn('Bash category ID: ', bashCategoryID);

  categories := TStringList.Create;
  try
    TSyntaxHighlighter.GetRegisteredCategories(categories);
    WriteLn('All registered categories:');
    for i := 0 to categories.Count - 1 do
      WriteLn('  ', categories[i], ' = ', PtrInt(categories.Objects[i]));
  finally
    categories.Free;
  end;

  WriteLn;
end;

begin
  WriteLn('Bash Syntax Highlighter Test');
  WriteLn('=============================');
  WriteLn;

  TestCategorySystem;
  TestBashKeywords;
  TestBashTokenTypes;
  TestBashScript;

  WriteLn('Test completed. Press Enter to exit.');
  ReadLn;
end.
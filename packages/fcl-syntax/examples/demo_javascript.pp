program test_javascript;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.javascript;

procedure TestJavaScriptKeywords;
const
  JSKeywords: array[0..19] of string = (
    'var', 'let', 'const', 'function', 'if', 'else', 'for', 'while', 'do', 'switch',
    'case', 'default', 'break', 'continue', 'return', 'try', 'catch', 'finally', 'throw', 'new'
  );
var
  highlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  WriteLn('Testing JavaScript Keywords:');
  WriteLn('===========================');

  highlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    for i := 0 to High(JSKeywords) do begin
      tokens := highlighter.Execute(JSKeywords[i]);

      if (Length(tokens) = 1) and (tokens[0].Kind = shKeyword) then
        WriteLn(JSKeywords[i] + ': PASS - recognized as keyword')
      else
        WriteLn(JSKeywords[i] + ': FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));
    end;
  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestJavaScriptTokenTypes;
var
  highlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
begin
  WriteLn('Testing JavaScript Token Types:');
  WriteLn('==============================');

  highlighter := TJavaScriptSyntaxHighlighter.Create;
  try

    // Test single-quoted string
    tokens := highlighter.Execute('''hello world''');
    if (Length(tokens) = 1) and (tokens[0].Kind = shStrings) then
      WriteLn('Single-quoted string: PASS')
    else
      WriteLn('Single-quoted string: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test double-quoted string
    tokens := highlighter.Execute('"hello world"');
    if (Length(tokens) = 1) and (tokens[0].Kind = shStrings) then
      WriteLn('Double-quoted string: PASS')
    else
      WriteLn('Double-quoted string: FAIL');


    // Test template literal
    tokens := highlighter.Execute('`hello ${name}`');
    if (Length(tokens) = 1) and (tokens[0].Kind = shRawString) then
      WriteLn('Template literal: PASS')
    else
      WriteLn('Template literal: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test regex literal
    tokens := highlighter.Execute('/[a-z]+/gi');
    if (Length(tokens) = 1) and (tokens[0].Kind = shRegex) then
      WriteLn('Regex literal: PASS')
    else
      WriteLn('Regex literal: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test single-line comment
    tokens := highlighter.Execute('// This is a comment');
    if (Length(tokens) = 1) and (tokens[0].Kind = shComment) then
      WriteLn('Single-line comment: PASS')
    else
      WriteLn('Single-line comment: FAIL');


    // Test multi-line comment
    tokens := highlighter.Execute('/* This is a comment */');
    if (Length(tokens) = 1) and (tokens[0].Kind = shComment) then
      WriteLn('Multi-line comment: PASS')
    else
      WriteLn('Multi-line comment: FAIL');


    // Test number
    tokens := highlighter.Execute('123.45');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Decimal number: PASS')
    else
      WriteLn('Decimal number: FAIL');


    // Test hex number
    tokens := highlighter.Execute('0xFF');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Hex number: PASS')
    else
      WriteLn('Hex number: FAIL');


    // Test operator
    tokens := highlighter.Execute('===');
    if (Length(tokens) = 1) and (tokens[0].Kind = shOperator) then
      WriteLn('Operator: PASS')
    else
      WriteLn('Operator: FAIL');


    // Test identifier
    tokens := highlighter.Execute('myVariable');
    if (Length(tokens) = 1) and (tokens[0].Kind = shDefault) then
      WriteLn('Identifier: PASS')
    else
      WriteLn('Identifier: FAIL');

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestJavaScriptNumbers;
var
  highlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
begin
  WriteLn('Testing JavaScript Number Formats:');
  WriteLn('==================================');

  highlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    // Test scientific notation
    tokens := highlighter.Execute('1.23e-4');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Scientific notation: PASS')
    else
      WriteLn('Scientific notation: FAIL');

    // Test binary number
    tokens := highlighter.Execute('0b1010');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Binary number: PASS')
    else
      WriteLn('Binary number: FAIL');

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestJavaScriptFunction;
var
  highlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  jsCode: string;
begin
  WriteLn('Testing Complete JavaScript Function:');
  WriteLn('====================================');

  jsCode := '/* This is a comment */';

  highlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(jsCode);

    WriteLn('Code: ', jsCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestRegexContext;
var
  highlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
begin
  WriteLn('Testing Regex vs Division Context:');
  WriteLn('==================================');

  highlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    // This should be recognized as regex (after =)
    tokens := highlighter.Execute('var pattern = /test/;');
    WriteLn('Code: var pattern = /test/;');
    if (Length(tokens) >= 5) and (tokens[4].Kind = shRegex) then
      WriteLn('  Regex after = : PASS')
    else
      WriteLn('  Regex after = : FAIL');

    // This should be division (after variable)
    tokens := highlighter.Execute('result = a / b;');
    WriteLn('Code: result = a / b;');
    if (Length(tokens) >= 5) and (tokens[4].Kind = shOperator) then
      WriteLn('  Division: PASS')
    else
      WriteLn('  Division: FAIL - kind=' + IntToStr(Ord(tokens[4].Kind)));

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestCategorySystem;
var
  categories: TStringList;
  jsCategoryID: Integer;
  i: Integer;
begin
  WriteLn('Testing Category System for JavaScript:');
  WriteLn('======================================');

  jsCategoryID := TSyntaxHighlighter.RegisterCategory('JavaScript');
  WriteLn('JavaScript category ID: ', jsCategoryID);

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
  WriteLn('JavaScript Syntax Highlighter Test');
  WriteLn('===================================');
  WriteLn;

  TestCategorySystem;
  TestJavaScriptKeywords;
  TestJavaScriptTokenTypes;
  TestJavaScriptNumbers;
  TestRegexContext;
  TestJavaScriptFunction;

  WriteLn('Test completed. Press Enter to exit.');
  ReadLn;
end.
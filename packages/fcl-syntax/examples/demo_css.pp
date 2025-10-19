program test_css;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.css;

procedure TestCssAtRules;
const
  CssAtRules: array[0..9] of string = (
    '@charset', '@import', '@media', '@keyframes', '@font-face',
    '@supports', '@page', '@namespace', '@viewport', '@layer'
  );
var
  highlighter: TCssSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  WriteLn('Testing CSS At-Rules:');
  WriteLn('====================');

  highlighter := TCssSyntaxHighlighter.Create;
  try
    for i := 0 to High(CssAtRules) do begin
      tokens := highlighter.Execute(CssAtRules[i]);

      if (Length(tokens) = 1) and (tokens[0].Kind = shDirective) then
        WriteLn(CssAtRules[i] + ': PASS - recognized as directive')
      else
        WriteLn(CssAtRules[i] + ': FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));
    end;
  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestCssProperties;
const
  CssProperties: array[0..9] of string = (
    'color', 'background', 'margin', 'padding', 'border',
    'font', 'width', 'height', 'position', 'display'
  );
var
  highlighter: TCssSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  WriteLn('Testing CSS Properties:');
  WriteLn('======================');

  highlighter := TCssSyntaxHighlighter.Create;
  try
    for i := 0 to High(CssProperties) do begin
      tokens := highlighter.Execute(CssProperties[i]);

      if (Length(tokens) = 1) and (tokens[0].Kind = shKeyword) then
        WriteLn(CssProperties[i] + ': PASS - recognized as property')
      else
        WriteLn(CssProperties[i] + ': FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));
    end;
  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestCssTokenTypes;
var
  highlighter: TCssSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
begin
  WriteLn('Testing CSS Token Types:');
  WriteLn('=======================');

  highlighter := TCssSyntaxHighlighter.Create;
  try

    // Test single-quoted string
    tokens := highlighter.Execute('''Arial''');
    if (Length(tokens) = 1) and (tokens[0].Kind = shStrings) then
      WriteLn('Single-quoted string: PASS')
    else
      WriteLn('Single-quoted string: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test double-quoted string
    tokens := highlighter.Execute('"Helvetica"');
    if (Length(tokens) = 1) and (tokens[0].Kind = shStrings) then
      WriteLn('Double-quoted string: PASS')
    else
      WriteLn('Double-quoted string: FAIL');


    // Test hex color
    tokens := highlighter.Execute('#FF0000');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Hex color: PASS')
    else
      WriteLn('Hex color: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test 3-digit hex color
    tokens := highlighter.Execute('#F00');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('3-digit hex color: PASS')
    else
      WriteLn('3-digit hex color: FAIL');


    // Test multi-line comment
    tokens := highlighter.Execute('/* This is a comment */');
    if (Length(tokens) = 1) and (tokens[0].Kind = shComment) then
      WriteLn('Multi-line comment: PASS')
    else
      WriteLn('Multi-line comment: FAIL');


    // Test number with unit
    tokens := highlighter.Execute('16px');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Number with unit: PASS')
    else
      WriteLn('Number with unit: FAIL');


    // Test percentage
    tokens := highlighter.Execute('100%');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Percentage: PASS')
    else
      WriteLn('Percentage: FAIL');


    // Test URL function
    tokens := highlighter.Execute('url(image.png)');
    if (Length(tokens) = 1) and (tokens[0].Kind = shStrings) then
      WriteLn('URL function: PASS')
    else
      WriteLn('URL function: FAIL');


    // Test class selector
    tokens := highlighter.Execute('.myClass');
    if (Length(tokens) = 1) and (tokens[0].Kind = shDefault) then
      WriteLn('Class selector: PASS')
    else
      WriteLn('Class selector: FAIL');


    // Test ID selector
    tokens := highlighter.Execute('#myId');
    if (Length(tokens) = 1) then
      WriteLn('ID selector: PASS')
    else
      WriteLn('ID selector: FAIL');

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestCssRule;
var
  highlighter: TCssSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  cssRule: string;
begin
  WriteLn('Testing Complete CSS Rule:');
  WriteLn('=========================');

  cssRule := '.container { width: 100%; color: #333; }';

  highlighter := TCssSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(cssRule);

    WriteLn('Rule: ', cssRule);
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

procedure TestCssMediaQuery;
var
  highlighter: TCssSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  mediaQuery: string;
begin
  WriteLn('Testing CSS Media Query:');
  WriteLn('=======================');

  mediaQuery := '@media (max-width: 768px) { body { font-size: 14px; } }';

  highlighter := TCssSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(mediaQuery);

    WriteLn('Media Query: ', mediaQuery);
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

procedure TestCategorySystem;
var
  categories: TStringList;
  cssCategoryID: Integer;
  i: Integer;
begin
  WriteLn('Testing Category System for CSS:');
  WriteLn('================================');

  cssCategoryID := TSyntaxHighlighter.RegisterCategory('CSS');
  WriteLn('CSS category ID: ', cssCategoryID);

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
  WriteLn('CSS Syntax Highlighter Test');
  WriteLn('============================');
  WriteLn;

  TestCategorySystem;
  TestCssAtRules;
  TestCssProperties;
  TestCssTokenTypes;
  TestCssRule;
  TestCssMediaQuery;
end.

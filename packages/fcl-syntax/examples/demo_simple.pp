program test_simple;

{$mode objfpc}{$H+}

uses
  SysUtils, syntax.highlighter, syntax.pascal;

procedure TestKeyword(const keyword: string);
var
  tokens: TSyntaxTokenArray;
  highlighter: TSyntaxHighlighter;
begin
  highlighter := TSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(keyword);
  finally
    highlighter.Free;
  end;

  if Length(tokens) = 1 then begin
    if tokens[0].Kind = shKeyword then
      WriteLn(keyword + ': PASS - recognized as keyword')
    else
      WriteLn(keyword + ': FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));
  end else
    WriteLn(keyword + ': FAIL - token count=' + IntToStr(Length(tokens)));
end;

procedure TestAllKeywords;
const
  Keywords: array[0..60] of string = (
    'AND', 'ARRAY', 'ASM', 'ASSEMBLER',
    'BEGIN', 'BREAK',
    'CASE', 'CONST', 'CONSTRUCTOR', 'CLASS',
    'DEFAULT', 'DESTRUCTOR', 'DIV', 'DO', 'DOWNTO',
    'ELSE', 'END', 'EXCEPT', 'EXIT',
    'FINALIZATION', 'FINALLY', 'FOR', 'FUNCTION',
    'GOTO',
    'IF', 'IMPLEMENTATION', 'IN', 'INHERITED', 'INITIALIZATION', 'INTERFACE',
    'NIL', 'NOT',
    'OBJECT', 'OF', 'ON', 'OR', 'OVERRIDE',
    'PACKED', 'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PROPERTY', 'PROTECTED',
      'PUBLIC', 'PUBLISHED',
    'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING',
    'SET',
    'THEN', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES',
    'VAR', 'VIRTUAL',
    'WHILE', 'WITH',
    'XOR'
  );
var
  i: Integer;
begin
  WriteLn('Testing all keywords:');
  WriteLn('====================');

  for i := 0 to High(Keywords) do
    TestKeyword(LowerCase(Keywords[i]));

  WriteLn;
  WriteLn('Testing other token types:');
  WriteLn('==========================');
end;

procedure TestOtherTokens;
var
  tokens: TSyntaxTokenArray;
  highlighter: TSyntaxHighlighter;
begin
  highlighter := TSyntaxHighlighter.Create;
  try

    // Test comment
    tokens := highlighter.Execute('{ comment }');
    if (Length(tokens) = 1) and (tokens[0].Kind = shComment) then
      WriteLn('Comment: PASS')
    else
      WriteLn('Comment: FAIL');


    // Test string
    tokens := highlighter.Execute('''hello''');
    if (Length(tokens) = 1) and (tokens[0].Kind = shStrings) then
      WriteLn('String: PASS')
    else
      WriteLn('String: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test character
    tokens := highlighter.Execute('''A''');
    if (Length(tokens) = 1) and (tokens[0].Kind = shCharacters) then
      WriteLn('Character: PASS')
    else
      WriteLn('Character: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test number
    tokens := highlighter.Execute('123');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Number: PASS')
    else
      WriteLn('Number: FAIL');


    // Test hex number
    tokens := highlighter.Execute('$FF');
    if (Length(tokens) = 1) and (tokens[0].Kind = shNumbers) then
      WriteLn('Hex number: PASS')
    else
      WriteLn('Hex number: FAIL');


    // Test symbol
    tokens := highlighter.Execute(':=');
    if (Length(tokens) = 1) and (tokens[0].Kind = shSymbol) then
      WriteLn('Symbol: PASS')
    else
      WriteLn('Symbol: FAIL');


    // Test directive
    tokens := highlighter.Execute('{$MODE OBJFPC}');
    if (Length(tokens) = 1) and (tokens[0].Kind = shDirective) then
      WriteLn('Directive: PASS')
    else
      WriteLn('Directive: FAIL - kind=' + IntToStr(Ord(tokens[0].Kind)));


    // Test identifier
    tokens := highlighter.Execute('MyVariable');
    if (Length(tokens) = 1) and (tokens[0].Kind = shDefault) then
      WriteLn('Identifier: PASS')
    else
      WriteLn('Identifier: FAIL');

  finally
    highlighter.Free;
  end;
end;

begin
  WriteLn('Pascal Syntax Highlighter Test');
  WriteLn('==============================');
  WriteLn;

  TestAllKeywords;
  TestOtherTokens;

  WriteLn;
  WriteLn('Test completed. Press Enter to exit.');
  ReadLn;
end.
{
  Tests for WordsContain and TSyntaxWordTable.
}
unit unittest.wordtable;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.table;

type

  { TTestWordTable }

  TTestWordTable = class(TTestCase)
  published
    procedure TestWordsContainWholeWords;
    procedure TestWordsContainEdges;
    procedure TestWordsContainSpacedValue;
    procedure TestLookupKinds;
    procedure TestKeywordPrecedence;
    procedure TestCaseSensitive;
    procedure TestCaseInsensitive;
    procedure TestGrow;
    procedure TestEmpty;
    procedure TestLookupCharacters;
  end;

implementation

procedure TTestWordTable.TestWordsContainWholeWords;

begin
  AssertTrue('first word', WordsContain('if then else', 'if'));
  AssertTrue('middle word', WordsContain('if then else', 'then'));
  AssertTrue('last word', WordsContain('if then else', 'else'));
  AssertFalse('prefix of a word', WordsContain('if then else', 'the'));
  AssertFalse('suffix of a word', WordsContain('if then else', 'hen'));
  AssertFalse('longer than a word', WordsContain('if then else', 'elsewhere'));
  AssertTrue('match after a partial match', WordsContain('format for', 'for'));
end;


procedure TTestWordTable.TestWordsContainEdges;

begin
  AssertFalse('empty value', WordsContain('if then', ''));
  AssertFalse('empty word list', WordsContain('', 'if'));
  AssertFalse('value over 64 characters', WordsContain(StringOfChar('a', 65), StringOfChar('a', 65)));
  AssertTrue('value of 64 characters', WordsContain(StringOfChar('a', 64), StringOfChar('a', 64)));
  AssertTrue('extra spaces in the word list', WordsContain('  if   then  ', 'then'));
end;


procedure TTestWordTable.TestWordsContainSpacedValue;

begin
  AssertTrue('adjacent words', WordsContain('fn codefn struct', 'fn codefn'));
  AssertFalse('words that are not adjacent', WordsContain('fn codefn struct', 'fn struct'));
end;


procedure TTestWordTable.TestLookupKinds;

var
  lTable: TSyntaxWordTable;
begin
  lTable := TSyntaxWordTable.Create(True);
  try
    lTable.AddWords('if then else', shKeyword);
    lTable.AddWords('nil true', shKey);
    AssertEquals('word count', 5, lTable.Count);
    AssertEquals('keyword', shKeyword.ToString, lTable.Lookup('then').ToString);
    AssertEquals('builtin', shKey.ToString, lTable.Lookup('nil').ToString);
    AssertEquals('unknown word', shDefault.ToString, lTable.Lookup('foo').ToString);
    AssertEquals('unknown word with default', shInvalid.ToString, lTable.Lookup('foo', shInvalid).ToString);
    AssertTrue('contains a word', lTable.Contains('else'));
    AssertFalse('prefix is not contained', lTable.Contains('els'));
  finally
    lTable.Free;
  end;
end;


procedure TTestWordTable.TestKeywordPrecedence;

var
  lTable: TSyntaxWordTable;
begin
  lTable := TSyntaxWordTable.Create(True);
  try
    lTable.AddWords('self super', shKeyword);
    lTable.AddWords('self nil', shKey);
    AssertEquals('duplicate is counted once', 3, lTable.Count);
    AssertEquals('first kind wins', shKeyword.ToString, lTable.Lookup('self').ToString);
  finally
    lTable.Free;
  end;
end;


procedure TTestWordTable.TestCaseSensitive;

var
  lTable: TSyntaxWordTable;
begin
  lTable := TSyntaxWordTable.Create(True);
  try
    lTable.AddWords('True None', shKey);
    AssertTrue('exact case', lTable.Contains('True'));
    AssertFalse('other case', lTable.Contains('true'));
  finally
    lTable.Free;
  end;
end;


procedure TTestWordTable.TestCaseInsensitive;

var
  lTable: TSyntaxWordTable;
begin
  lTable := TSyntaxWordTable.Create(False);
  try
    lTable.AddWords('begin End', shKeyword);
    AssertTrue('lower case', lTable.Contains('begin'));
    AssertTrue('upper case', lTable.Contains('BEGIN'));
    AssertTrue('mixed case', lTable.Contains('BeGiN'));
    AssertTrue('word added in upper case', lTable.Contains('end'));
    AssertFalse('non-ASCII bytes are not folded', lTable.Contains('b'#$C3#$89'gin'));
  finally
    lTable.Free;
  end;
end;


procedure TTestWordTable.TestGrow;

var
  lTable: TSyntaxWordTable;
  lWords: string;
  I: Integer;
begin
  lWords := '';
  for I := 1 to 500 do
    lWords := lWords + 'w' + IntToStr(I) + ' ';
  lTable := TSyntaxWordTable.Create(True);
  try
    lTable.AddWords(lWords, shKeyword);
    AssertEquals('word count', 500, lTable.Count);
    for I := 1 to 500 do
      AssertTrue('word w' + IntToStr(I) + ' after growing', lTable.Contains('w' + IntToStr(I)));
    AssertFalse('word not added', lTable.Contains('w501'));
  finally
    lTable.Free;
  end;
end;


procedure TTestWordTable.TestEmpty;

var
  lTable: TSyntaxWordTable;
begin
  lTable := TSyntaxWordTable.Create(False);
  try
    lTable.AddWords('', shKeyword);
    lTable.AddWords('   ', shKeyword);
    AssertEquals('no words', 0, lTable.Count);
    AssertFalse('lookup in an empty table', lTable.Contains('if'));
    AssertFalse('empty value', lTable.Contains(''));
  finally
    lTable.Free;
  end;
end;

procedure TTestWordTable.TestLookupCharacters;

const
  Source = 'x:=BEGIN;endx';
var
  lTable: TSyntaxWordTable;
begin
  lTable := TSyntaxWordTable.Create(False);
  try
    lTable.AddWords('begin end', shKeyword);
    AssertEquals('word inside a source', shKeyword.ToString, lTable.Lookup(PChar(Source) + 3, 5).ToString);
    AssertEquals('prefix of a longer word', shKeyword.ToString, lTable.Lookup(PChar(Source) + 9, 3).ToString);
    AssertEquals('longer than the word', shDefault.ToString, lTable.Lookup(PChar(Source) + 9, 4).ToString);
    AssertEquals('shorter than the word', shInvalid.ToString, lTable.Lookup(PChar(Source) + 3, 4, shInvalid).ToString);
    AssertEquals('zero length', shDefault.ToString, lTable.Lookup(PChar(Source), 0).ToString);
  finally
    lTable.Free;
  end;
end;

initialization
  RegisterTest(TTestWordTable);
end.

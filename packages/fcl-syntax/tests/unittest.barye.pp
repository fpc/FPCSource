{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Barye highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.barye;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Types, fpcunit, testregistry,
  syntax.highlighter, syntax.table, syntax.barye, unittest.syntaxfixture;

type

  { TTestBaryeHighlighter }

  TTestBaryeHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestPreamble;
    procedure TestComments;
    procedure TestStrings;
    procedure TestInclude;
    procedure TestOperators;
    procedure TestRanges;
    procedure TestBlockDelimiters;
    procedure TestNumbers;
    procedure TestControlKeywords;
    procedure TestLetBindings;
    procedure TestMatch;
    procedure TestDeclarations;
    procedure TestTypes;
    procedure TestNumericExample;
    procedure TestStructuredExample;
    procedure TestOptionalExample;
    procedure TestAssembly;
    procedure TestMainBlockEnd;
    procedure TestImports;
    procedure TestReservedWords;
    procedure TestWithBlocks;
    procedure TestOpenBracesFast;
    procedure TestCloseBracesFast;
  end;

implementation

const
  NumericSample = 'program numeric_example' + #10 + 'fn transformed(value: u32): u32 {' + #10 + 'let mut count: u32 = 3' + #10
    + 'count := count + value * 2' + #10 + 'result := count % 7' + #10 + '}' + #10
    + 'fn split_example: (u32, bool) { return(5, true) }' + #10 + '[{ pass }].';
  StructuredSample = 'program structured_example' + #10 + 'struct Window { start: usize; end: usize }' + #10
    + 'fn (window: Window) valid: bool { result := window.start <= window.end }' + #10
    + 'fn make_window(start: usize, end: usize): Window { result := Window { start = start; end = end } }' + #10 + '[{ pass }].';
  AssemblySample = 'program assembly_example' + #10 + 'codefn machine_leaf {' + #10 + 'mov eax, 1' + #10 + 'jmp eax' + #10 + 'ret' + #10
    + '}' + #10 + 'fn ordinary_leaf [{ pass }]' + #10 + '[{ pass }].';
  OptionalSample = 'fn next_position(limit: usize, position: usize): ?usize {' + #10
    + 'if position < limit { result := Some(position + 1) }' + #10 + 'else { result := None }' + #10 + '}' + #10
    + 'fn has_position: bool {' + #10 + 'match first (selected) { Some(_) { result := true } None { result := false } }' + #10 + '}';

class function TTestBaryeHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('barye bry',
    'program sample' + #10 + 'fn f(value: u32): u32 { return(value) }' + #10 + '[{ pass }].',
    'return', '"return //"', '// return');
end;


procedure TTestBaryeHighlighter.TestPreamble;

begin
  AssertRole('barye', '# arbitrary preamble' + #13#10 + 'fn f [{ pass }]', '# arbitrary', shComment);
  AssertRole('barye', ' # not preamble', '#', shDefault);
  AssertRole('barye', #10 + '# not preamble', '#', shDefault);
  AssertRole('barye', #239#187#191 + '# not preamble', '#', shDefault);
end;


procedure TTestBaryeHighlighter.TestComments;

begin
  AssertRole('barye', '/* fn fake { /* nested-looking */ after', 'after', shDefault);
  AssertRole('barye', '// codefn fake {', 'codefn', shComment);
  AssertRole('barye', 'fn f {' + #10 + 'if /* gap' + #10 + '*/ condition [{ leave }] }', 'if', shDefault);
end;


procedure TTestBaryeHighlighter.TestStrings;

begin
  AssertRole('barye', '"fn { \\ \" \n \r \t \0 \q"', '\n', shEscape);
  AssertRole('barye', '"fn { \q"', '\q', shStrings);
  AssertRole('barye', '"unfinished' + #13#10 + 'fn real [{ pass }]', 'real', shAttribute);
  AssertRole('barye', '"unfinished' + #1 + ' fn real [{ pass }]', #1, shDefault);
end;


procedure TTestBaryeHighlighter.TestInclude;

begin
  AssertRole('barye', '[#include <helpers/return // café.bry>#]', 'return', shStrings);
  AssertRole('barye', '[#include "helpers/part name.bry"#]', 'include', shDirective);
  AssertRole('barye', '[#include "a\n.bry"#]', '\n', shStrings);
  AssertToken('barye', '[#include <helpers/range.bry>#]', '#]', shDirective);
  AssertToken('barye', '[#include "helpers/part name.bry"#]', '[#', shDirective);
end;


procedure TTestBaryeHighlighter.TestOperators;

var
  lOperator: string;
begin
  AssertToken('barye', '8 << 2 + 1', '<<', shOperator);
  AssertToken('barye', '8 << (2 + 1)', '(', shSymbol);
  AssertToken('barye', 'a << = b', '<<', shOperator);
  AssertToken('barye', 'a < /* gap */ < b', '<', shOperator);
  for lOperator in LanguageNames('<<= >>= *= /= %= -= += <= >= != == := << >>') do
    AssertToken('barye', 'a ' + lOperator + ' b', lOperator, shOperator);
  AssertToken('barye', '<<<', '<<', shOperator);
  AssertToken('barye', '<<<', '<', shOperator);
end;


procedure TTestBaryeHighlighter.TestRanges;

begin
  AssertToken('barye', '0..31', '0', shNumbers);
  AssertToken('barye', '0..31', '..', shSymbol);
  AssertToken('barye', '0..31', '31', shNumbers);
end;


procedure TTestBaryeHighlighter.TestBlockDelimiters;

begin
  AssertToken('barye', '[{ pass }]', '[{', shSymbol);
  AssertToken('barye', '[{ pass }]', '}]', shSymbol);
  AssertToken('barye', '[ { pass } ]', '[', shSymbol);
  AssertToken('barye', '[ { pass } ]', '}', shSymbol);
end;


procedure TTestBaryeHighlighter.TestNumbers;

begin
  AssertRole('barye', '0x10 1.5e2', 'x10', shDefault);
end;


procedure TTestBaryeHighlighter.TestControlKeywords;

begin
  AssertRole('barye', 'fn if [{ pass }]' + #10 + 'fn f {' + #10 + 'if(condition)' + #10 + 'let(value)' + #10 + 'with(value)' + #10
             + 'if condition [{ leave }] }', 'if', shAttribute);
  AssertRole('barye', 'fn f {' + #10 + 'if(condition)' + #10 + 'let(value)' + #10 + 'with(value) }', 'if', shDefault);
  AssertRole('barye', 'fn f {' + #10 + 'if condition [{ leave }] }', 'if', shKeyword);
  AssertRole('barye', 'fn f { if condition { pass } else { pass } }', 'else', shKeyword);
  AssertRole('barye', 'fn f { if condition { pass }; else { pass } }', 'else', shDefault);
  AssertRole('barye', 'fn f {' + #10 + 'with value [{ pass }] }', 'with', shKeyword);
end;


procedure TTestBaryeHighlighter.TestLetBindings;

begin
  AssertRole('barye', 'fn f {' + #10 + 'if(condition)' + #10 + 'let(value) }', 'let', shDefault);
  AssertRole('barye', 'fn f {' + #10 + 'let (value, valid) = pair(3) }', 'let', shKeyword);
  AssertRole('barye', 'fn f {' + #10 + 'let (value, valid) = pair(3) }', 'valid', shAttribute);
  AssertRole('barye', 'fn f {' + #10 + 'let: [{' + #10 + 'mut cursor: usize = 0' + #10 + '}] }', 'mut', shKeyword);
  AssertRole('barye', 'fn f {' + #10 + 'let: [{' + #10 + 'mut cursor: usize = 0' + #10 + '}] }', 'cursor', shAttribute);
end;


procedure TTestBaryeHighlighter.TestMatch;

begin
  AssertRole('barye', 'fn f { match first (x) { value if value == 65 { pass } } }', 'first', shKeyword);
  AssertRole('barye', 'fn first [{ pass }]' + #10 + 'fn f { first() }', 'first', shAttribute);
  AssertRole('barye', 'fn f { match first (x) {' + #10 + 'error { pass } } }', 'error', shDefault);
  AssertRole('barye', 'fn f { match 1 (x) {' + #10 + 'error { pass } } }', 'error', shKeyword);
end;


procedure TTestBaryeHighlighter.TestDeclarations;

begin
  AssertRole('barye', 'fn (receiver: Window) valid: bool', 'valid', shAttribute);
  AssertRole('barye', 'fn (receiver:', 'receiver', shAttribute);
  AssertRole('barye', 'fn()' + #10 + 'other()', 'other', shDefault);
  AssertRole('barye', 'program return', 'return', shKeyword);
  AssertRole('barye', 'unit geometry', 'unit', shKeyword);
  AssertRole('barye', 'pub fn get: u32', 'get', shAttribute);
end;


procedure TTestBaryeHighlighter.TestTypes;

begin
  AssertRole('barye', 'value: ?usize', 'usize', shKey);
  AssertRole('barye', 'fn f: (u32, bool) { result := 1 }', 'result', shKey);
  AssertRole('barye', 'fn f: Result<usize,usize> { return(1) }', 'Result', shKey);
  AssertRole('barye', 'fn f: Result<usize,usize> { return(1) }', 'usize', shKey, 2);
  AssertRole('barye', 'Result<usize,usize>', 'Result', shDefault);
end;


procedure TTestBaryeHighlighter.TestNumericExample;

begin
  AssertRole('barye', NumericSample, 'transformed', shAttribute);
  AssertRole('barye', NumericSample, 'u32', shKey);
  AssertRole('barye', NumericSample, 'result', shKey);
  AssertRole('barye', NumericSample, 'mut', shKeyword);
end;


procedure TTestBaryeHighlighter.TestStructuredExample;

begin
  AssertRole('barye', StructuredSample, 'Window', shAttribute);
  AssertRole('barye', StructuredSample, 'valid', shAttribute);
  AssertRole('barye', StructuredSample, 'start', shAttribute);
end;


procedure TTestBaryeHighlighter.TestOptionalExample;

begin
  AssertRole('barye', OptionalSample, 'Some', shKey);
  AssertRole('barye', OptionalSample, 'None', shKey);
end;


procedure TTestBaryeHighlighter.TestAssembly;

begin
  AssertRole('barye', AssemblySample, 'mov', shAssembler);
  AssertRole('barye', AssemblySample, 'eax', shKey);
  AssertRole('barye', AssemblySample, 'eax', shAttribute, 2);
  AssertRole('barye', AssemblySample, 'ret', shAssembler);
  AssertRole('barye', AssemblySample, 'ordinary_leaf', shAttribute);
  AssertRole('barye', 'fn mov [{ pass }]' + #10 + 'fn f { mov() }', 'mov', shDefault, 2);
  AssertRole('barye', 'codefn broken(value: u32 { mov eax, 1 }' + #10 + 'fn real [{ pass }]', 'mov', shDefault);
  AssertRole('barye', 'codefn bad [{ pass }]', 'pass', shKeyword);
  AssertRole('barye', 'codefn bad [ { mov eax, 1 } ]', 'mov', shDefault);
end;


procedure TTestBaryeHighlighter.TestMainBlockEnd;

begin
  AssertRole('barye', 'program minimal' + #10 + '[{ pass }].fn fake { "unfinished', 'fake', shDefault);
  AssertRole('barye', 'program minimal' + #10 + '[{ pass }].fn fake { "unfinished', '"unfinished', shDefault);
  AssertRole('barye', 'program minimal' + #10 + '{ pass }.fn fake {', 'fake', shDefault);
  AssertRole('barye', 'fn f [{ pass }].' + #10 + 'fn real [{ pass }]', 'real', shAttribute);
  AssertRole('barye', 'program minimal' + #10 + '[{ pass }] .' + #10 + 'fn real [{ pass }]', 'real', shAttribute);
  AssertRole('barye', 'program minimal' + #10 + '[ { pass }].' + #10 + 'return', 'return', shKeyword);
  AssertRole('barye', 'program minimal' + #10 + '[{ leave }].' + #10 + 'return', 'return', shKeyword);
  AssertRole('barye', 'program minimal' + #10 + '[{ pass; extra }].' + #10 + 'return', 'return', shKeyword);
  AssertRole('barye', 'program minimal' + #10 + '[{ /* gap */ pass; }].' + #10 + 'return', 'return', shDefault);
end;


procedure TTestBaryeHighlighter.TestImports;

begin
  AssertRole('barye', 'uses a, b;', 'uses', shKeyword);
  AssertRole('barye', 'use a.get, b.*;', '*', shSymbol);
  AssertRole('barye', 'from a use get;', 'use', shKeyword);
  AssertRole('barye', 'from a use *;', '*', shSymbol);
  AssertRole('barye', 'use a.get as alias;', 'as', shKeyword);
end;


procedure TTestBaryeHighlighter.TestReservedWords;

begin
  AssertRole('barye', 'next# next #', 'next', shDefault);
  AssertRole('barye', 'return defer pass leave return_value', 'return_value', shDefault);
end;


procedure TTestBaryeHighlighter.TestWithBlocks;

begin
  AssertRole('barye', 'fn f { defer { emit(1 + 2) }' + #10 + 'with outer [{ with inner { .start := 1 } .end := 2 }] }', 'end', shAttribute);
end;


procedure TTestBaryeHighlighter.TestOpenBracesFast;

begin
  AssertFast('barye', StringOfChar('{', 1024 * 1024));
end;


procedure TTestBaryeHighlighter.TestCloseBracesFast;

begin
  AssertFast('barye', StringOfChar('}', 1024 * 1024));
end;

initialization
  RegisterTest(TTestBaryeHighlighter);
end.

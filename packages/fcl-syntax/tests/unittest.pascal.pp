{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Pascal highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.pascal;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.pascal;

type
  TTestPascalHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
  published
    procedure TestBasicKeywords;
    procedure TestANDKeyword;
    procedure TestARRAYKeyword;
    procedure TestASMKeyword;
    procedure TestASSEMBLERKeyword;
    procedure TestBEGINKeyword;
    procedure TestBREAKKeyword;
    procedure TestCASEKeyword;
    procedure TestCONSTKeyword;
    procedure TestCONSTRUCTORKeyword;
    procedure TestCLASSKeyword;
    procedure TestDEFAULTKeyword;
    procedure TestDESTRUCTORKeyword;
    procedure TestDIVKeyword;
    procedure TestDOKeyword;
    procedure TestDOWNTOKeyword;
    procedure TestELSEKeyword;
    procedure TestENDKeyword;
    procedure TestEXCEPTKeyword;
    procedure TestEXITKeyword;
    procedure TestFINALIZATIONKeyword;
    procedure TestFINALLYKeyword;
    procedure TestFORKeyword;
    procedure TestFUNCTIONKeyword;
    procedure TestGOTOKeyword;
    procedure TestIFKeyword;
    procedure TestIMPLEMENTATIONKeyword;
    procedure TestINKeyword;
    procedure TestINHERITEDKeyword;
    procedure TestINITIALIZATIONKeyword;
    procedure TestINTERFACEKeyword;
    procedure TestNILKeyword;
    procedure TestNOTKeyword;
    procedure TestOBJECTKeyword;
    procedure TestOFKeyword;
    procedure TestONKeyword;
    procedure TestORKeyword;
    procedure TestOVERRIDEKeyword;
    procedure TestPACKEDKeyword;
    procedure TestPRIVATEKeyword;
    procedure TestPROCEDUREKeyword;
    procedure TestPROGRAMKeyword;
    procedure TestPROPERTYKeyword;
    procedure TestPROTECTEDKeyword;
    procedure TestPUBLICKeyword;
    procedure TestPUBLISHEDKeyword;
    procedure TestRAISEKeyword;
    procedure TestRECORDKeyword;
    procedure TestREPEATKeyword;
    procedure TestRESOURCESTRINGKeyword;
    procedure TestSETKeyword;
    procedure TestTHENKeyword;
    procedure TestTRYKeyword;
    procedure TestTYPEKeyword;
    procedure TestUNITKeyword;
    procedure TestUNTILKeyword;
    procedure TestUSESKeyword;
    procedure TestVARKeyword;
    procedure TestVIRTUALKeyword;
    procedure TestWHILEKeyword;
    procedure TestWITHKeyword;
    procedure TestXORKeyword;
    procedure TestComments;
    procedure TestStrings;
    procedure TestNumbers;
    procedure TestSymbols;
    procedure TestDirectives;
    procedure TestIdentifiers;
  end;

implementation

procedure TTestPascalHighlighter.SetUp;
begin

end;

procedure TTestPascalHighlighter.TearDown;
begin
  // Nothing to do
end;

procedure TTestPascalHighlighter.TestBasicKeywords;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('begin end');
  AssertEquals('Should have 3 tokens', 3, Length(tokens));
  AssertEquals('First token should be BEGIN', 'begin', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
  AssertEquals('Second token should be space', ' ', tokens[1].Text);
  AssertEquals('Second token should be default', Ord(shDefault), Ord(tokens[1].Kind));
  AssertEquals('Third token should be END', 'end', tokens[2].Text);
  AssertEquals('Third token should be keyword', Ord(shKeyword), Ord(tokens[2].Kind));
end;

procedure TTestPascalHighlighter.TestANDKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('and');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be AND', 'and', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestARRAYKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('array');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be ARRAY', 'array', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestASMKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('asm end');
  AssertTrue('Should have at least 2 tokens', Length(tokens) >= 2);
  AssertEquals('First token should be ASM', 'asm', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
  // After ASM keyword, we should have assembler highlighting until END
end;

procedure TTestPascalHighlighter.TestASSEMBLERKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('assembler');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be ASSEMBLER', 'assembler', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestBEGINKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('begin');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be BEGIN', 'begin', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestBREAKKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('break');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be BREAK', 'break', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestCASEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('case');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be CASE', 'case', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestCONSTKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('const');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be CONST', 'const', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestCONSTRUCTORKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('constructor');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be CONSTRUCTOR', 'constructor', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestCLASSKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('class');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be CLASS', 'class', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestDEFAULTKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('default');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be DEFAULT', 'default', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestDESTRUCTORKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('destructor');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be DESTRUCTOR', 'destructor', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestDIVKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('div');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be DIV', 'div', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestDOKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('do');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be DO', 'do', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestDOWNTOKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('downto');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be DOWNTO', 'downto', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestELSEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('else');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be ELSE', 'else', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestENDKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('end');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be END', 'end', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestEXCEPTKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('except');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be EXCEPT', 'except', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestEXITKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('exit');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be EXIT', 'exit', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestFINALIZATIONKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('finalization');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be FINALIZATION', 'finalization', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestFINALLYKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('finally');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be FINALLY', 'finally', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestFORKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('for');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be FOR', 'for', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestFUNCTIONKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('function');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be FUNCTION', 'function', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestGOTOKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('goto');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be GOTO', 'goto', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestIFKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('if');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be IF', 'if', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestIMPLEMENTATIONKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('implementation');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be IMPLEMENTATION', 'implementation', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestINKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('in');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be IN', 'in', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestINHERITEDKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('inherited');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be INHERITED', 'inherited', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestINITIALIZATIONKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('initialization');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be INITIALIZATION', 'initialization', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestINTERFACEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('interface');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be INTERFACE', 'interface', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestNILKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('nil');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be NIL', 'nil', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestNOTKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('not');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be NOT', 'not', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestOBJECTKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('object');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be OBJECT', 'object', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestOFKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('of');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be OF', 'of', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestONKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('on');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be ON', 'on', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestORKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('or');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be OR', 'or', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestOVERRIDEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('override');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be OVERRIDE', 'override', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPACKEDKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('packed');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PACKED', 'packed', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPRIVATEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('private');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PRIVATE', 'private', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPROCEDUREKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('procedure');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PROCEDURE', 'procedure', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPROGRAMKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('program');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PROGRAM', 'program', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPROPERTYKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('property');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PROPERTY', 'property', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPROTECTEDKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('protected');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PROTECTED', 'protected', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPUBLICKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('public');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PUBLIC', 'public', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestPUBLISHEDKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('published');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be PUBLISHED', 'published', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestRAISEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('raise');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be RAISE', 'raise', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestRECORDKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('record');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be RECORD', 'record', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestREPEATKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('repeat');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be REPEAT', 'repeat', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestRESOURCESTRINGKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('resourcestring');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be RESOURCESTRING', 'resourcestring', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestSETKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('set');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be SET', 'set', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestTHENKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('then');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be THEN', 'then', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestTRYKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('try');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be TRY', 'try', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestTYPEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('type');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be TYPE', 'type', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestUNITKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('unit');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be UNIT', 'unit', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestUNTILKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('until');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be UNTIL', 'until', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestUSESKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('uses');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be USES', 'uses', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestVARKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('var');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be VAR', 'var', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestVIRTUALKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('virtual');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be VIRTUAL', 'virtual', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestWHILEKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('while');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be WHILE', 'while', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestWITHKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('with');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be WITH', 'with', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestXORKeyword;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('xor');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be XOR', 'xor', tokens[0].Text);
  AssertEquals('Token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestComments;
var
  tokens: TSyntaxTokenArray;
begin
  // Test { } comment
  tokens := DoPascalHighlighting('{ this is a comment }');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be comment text', '{ this is a comment }', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));

  // Test (* *) comment
  tokens := DoPascalHighlighting('(* this is a comment *)');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be comment text', '(* this is a comment *)', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));

  // Test // comment
  tokens := DoPascalHighlighting('// this is a comment');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be comment text', '// this is a comment', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestStrings;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('''Hello World''');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be string', '''Hello World''', tokens[0].Text);
  AssertEquals('Token should be string type', Ord(shStrings), Ord(tokens[0].Kind));

  // Test character literal
  tokens := DoPascalHighlighting('''A''');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be char', '''A''', tokens[0].Text);
  AssertEquals('Token should be character type', Ord(shCharacters), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestNumbers;
var
  tokens: TSyntaxTokenArray;
begin
  // Test decimal number
  tokens := DoPascalHighlighting('123');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be number', '123', tokens[0].Text);
  AssertEquals('Token should be number type', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test hex number
  tokens := DoPascalHighlighting('$FF');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be hex number', '$FF', tokens[0].Text);
  AssertEquals('Token should be number type', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestSymbols;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting(':=');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be assignment', ':=', tokens[0].Text);
  AssertEquals('Token should be symbol type', Ord(shSymbol), Ord(tokens[0].Kind));

  tokens := DoPascalHighlighting(';');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be semicolon', ';', tokens[0].Text);
  AssertEquals('Token should be symbol type', Ord(shSymbol), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestDirectives;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('{$MODE OBJFPC}');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be directive', '{$MODE OBJFPC}', tokens[0].Text);
  AssertEquals('Token should be directive type', Ord(shDirective), Ord(tokens[0].Kind));
end;

procedure TTestPascalHighlighter.TestIdentifiers;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting('MyVariable');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be identifier', 'MyVariable', tokens[0].Text);
  AssertEquals('Token should be default type', Ord(shDefault), Ord(tokens[0].Kind));
end;

initialization
  RegisterTest(TTestPascalHighlighter);
end.

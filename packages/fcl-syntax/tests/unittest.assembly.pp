{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Assembly language highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.assembly;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.assembly, unittest.syntaxfixture;

type

  { TTestAssemblyHighlighter }

  TTestAssemblyHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestRegister;
    procedure TestHashComment;
    procedure TestImmediate;
  end;

implementation

class function TTestAssemblyHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('asm nasm gas x86 arm',
    'mov eax, 42' + #10 + 'add x0, x1, #1' + #10 + 'ret',
    'mov', '"mov ;"', '; mov');
end;


procedure TTestAssemblyHighlighter.TestRegister;

begin
  AssertRole('asm', 'mov eax, 42' + #10 + 'ldr x0, [sp, #8]', 'x0', shKey);
end;


procedure TTestAssemblyHighlighter.TestHashComment;

begin
  AssertRole('asm', 'mov %eax, %ebx # mov', '# mov', shComment);
end;


procedure TTestAssemblyHighlighter.TestImmediate;

begin
  AssertRole('asm', 'ldr x0, [sp, #8]', '8', shNumbers);
end;

initialization
  RegisterTest(TTestAssemblyHighlighter);
end.

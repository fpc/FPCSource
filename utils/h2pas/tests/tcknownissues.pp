{
  h2pas test suite: expected output for defects of the current h2pas; these tests fail.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcKnownIssues;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestKnownPointerIssues }

  TTestKnownPointerIssues = class(TH2PasTestCase)
  published
    procedure TestPointerPrefixPointerToPointer;
  end;

  { TTestKnownDeclarationIssues }

  TTestKnownDeclarationIssues = class(TH2PasTestCase)
  published
    procedure TestReferenceParam;
    procedure TestFunctionTypedef;
  end;

  { TTestKnownMacroIssues }

  TTestKnownMacroIssues = class(TH2PasTestCase)
  published
    procedure TestLineContinuation;
    procedure TestParenthesizedParameter;
    procedure TestElif;
    procedure TestIfExpression;
  end;

implementation


procedure TTestKnownPointerIssues.TestPointerPrefixPointerToPointer;

begin
  Convert(['struct s { int a; };','void g(struct s **pp);'],['-p','-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestKnownDeclarationIssues.TestReferenceParam;

begin
  Convert(['void f(int &r);']);
  AssertConverted;
  AssertInterface('C++ reference parameter becomes a var parameter',['procedure f(var r:longint);']);
end;


procedure TTestKnownDeclarationIssues.TestFunctionTypedef;

begin
  Convert(['typedef int (func_t)(int);']);
  AssertConverted;
  AssertInterface('function typedef',['func_t = function (_para1:longint):longint;cdecl;']);
end;


procedure TTestKnownMacroIssues.TestLineContinuation;

begin
  Convert(['#define LONGDEF 1 + \','  2']);
  AssertConverted;
  AssertInterface('define continued on the next line',['LONGDEF = 1+2;']);
end;


procedure TTestKnownMacroIssues.TestParenthesizedParameter;

begin
  Convert(['#define PAR1(a) ((a) + 1)']);
  AssertConverted;
  AssertInterface('parenthesized parameter is no typecast',['function PAR1(a : longint) : longint;']);
  AssertImplementation('parenthesized parameter body',['PAR1:=a+1;']);
end;


procedure TTestKnownMacroIssues.TestElif;

begin
  Convert(['#if A','int x;','#elif B','int y;','#endif']);
  AssertConverted;
  AssertOutput('#elif becomes $elseif',['{$elseif B}']);
end;


procedure TTestKnownMacroIssues.TestIfExpression;

begin
  Convert(['#if defined(A) && B','int x;','#endif']);
  AssertConverted;
  AssertNotOutput('C operators are translated in conditions','&&');
end;


initialization
  RegisterTest('KnownIssues',TTestKnownPointerIssues);
  RegisterTest('KnownIssues',TTestKnownDeclarationIssues);
  RegisterTest('KnownIssues',TTestKnownMacroIssues);
end.

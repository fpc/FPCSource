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

  { TTestKnownMacroIssues }

  TTestKnownMacroIssues = class(TH2PasTestCase)
  published
    procedure TestParenthesizedParameter;
    procedure TestElif;
    procedure TestIfExpression;
  end;

implementation


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
  RegisterTest('KnownIssues',TTestKnownMacroIssues);
end.

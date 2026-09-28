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
    procedure TestParenthesizedProduct;
    procedure TestElif;
    procedure TestIfExpression;
  end;

implementation


procedure TTestKnownMacroIssues.TestParenthesizedProduct;

begin
  Convert(['#define N4 (X * 2)']);
  AssertConverted;
  AssertInterface('parenthesized product of a name is no pointer cast',['N4 = X*2;']);
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

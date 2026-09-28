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
    procedure TestIfExpression;
  end;

implementation


procedure TTestKnownMacroIssues.TestIfExpression;

begin
  Convert(['#if defined(A) && B','int x;','#endif']);
  AssertConverted;
  AssertNotOutput('C operators are translated in conditions','&&');
end;


initialization
  RegisterTest('KnownIssues',TTestKnownMacroIssues);
end.

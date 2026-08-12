{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the human-readable text output adapter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstOutputText;

{ The human-readable text output adapter. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Output.Text;

type
  { Text output adapter tests. }
  TOutputTextTest = class(TTestCase)
  private
    function SyntheticIssues: TFpSonarIssueArray;
    function MutedIssues: TFpSonarIssueArray;
  published
    procedure HeaderCarriesVersionStamp;
    procedure PositionedIssueLineIsGreppable;
    procedure AbsentPositionRendersPathAlone;
    procedure CountFooterPresent;
    procedure OutputIsDeterministic;
    procedure ActiveIssuesCarryNoAnnotationOrSuppressedFooter;
    procedure SuppressedIssueAnnotatedWithSourceAndFooter;
  end;


implementation

// Two synthetic issues: a positioned Minor CodeSmell and a position-less Major
// (the kind of ParseError a live run can fold). Fingerprint is set by hand
// (TFpSonarIssue.Make leaves it empty; the text adapter ignores it anyway).
function TOutputTextTest.SyntheticIssues: TFpSonarIssueArray;

begin
  RegisterMessage('rule.LowercaseKeywords.message',
    'keyword "%s" should be lowercase');

  SetLength(Result, 2);
  Result[0] := TFpSonarIssue.Make('LowercaseKeywords', 'src/foo.pas', 4, 7, 4, 11,
    sevMinor, itCodeSmell, cfHigh, 'rule.LowercaseKeywords.message', ['begin']);
  Result[0].Fingerprint := 'abc1230000000000';
  Result[1] := TFpSonarIssue.Make('ParseError', 'src/bad.pas', 0, 0, 0, 0,
    sevMajor, itCodeSmell, cfHigh, 'rule.ParseError.message', ['boom']);
  Result[1].Fingerprint := 'def4560000000000';
end;


// One active + one baseline-muted issue (reportability).
function TOutputTextTest.MutedIssues: TFpSonarIssueArray;

begin
  RegisterMessage('rule.LowercaseKeywords.message',
    'keyword "%s" should be lowercase');

  SetLength(Result, 2);
  Result[0] := TFpSonarIssue.Make('LowercaseKeywords', 'src/foo.pas', 4, 7, 4, 11,
    sevMinor, itCodeSmell, cfHigh, 'rule.LowercaseKeywords.message', ['begin']);
  Result[0].SuppressionSource := ssActive;
  Result[1] := TFpSonarIssue.Make('LowercaseKeywords', 'src/bar.pas', 9, 1, 9, 5,
    sevMinor, itCodeSmell, cfHigh, 'rule.LowercaseKeywords.message', ['end']);
  Result[1].SuppressionSource := ssBaseline;
end;


procedure TOutputTextTest.HeaderCarriesVersionStamp;

var
  lOut: string;

begin
  lOut := FormatText(SyntheticIssues);
  AssertTrue('header carries the tool version',
    Pos(FpSonarVersion, lOut) > 0);
  AssertTrue('header carries the text-report version stamp',
    Pos('report v' + FpSonarTextReportVersion, lOut) > 0);
end;


procedure TOutputTextTest.PositionedIssueLineIsGreppable;

var
  lOut: string;

begin
  lOut := FormatText(SyntheticIssues);
  // path:line:col, lowercase severity, bare RuleId, and the RESOLVED message.
  AssertTrue('path:line:col prefix', Pos('src/foo.pas:4:7:', lOut) > 0);
  AssertTrue('lowercase severity', Pos(' minor ', lOut) > 0);
  AssertTrue('bare RuleId', Pos('LowercaseKeywords:', lOut) > 0);
  AssertTrue('resolved message, not the bare key',
    Pos('keyword "begin" should be lowercase', lOut) > 0);
  AssertTrue('the bare message key never leaks',
    Pos('rule.LowercaseKeywords.message', lOut) = 0);
end;


procedure TOutputTextTest.AbsentPositionRendersPathAlone;

var
  lOut: string;

begin
  lOut := FormatText(SyntheticIssues);
  // Documented rule: an absent position renders as the path alone (no :line:col).
  AssertTrue('absent-position issue renders path alone',
    Pos('src/bad.pas: major ParseError: Parse error: boom', lOut) > 0);
  AssertTrue('no spurious :0 position suffix',
    Pos('src/bad.pas:0', lOut) = 0);
end;


procedure TOutputTextTest.CountFooterPresent;

var
  lOut: string;

begin
  lOut := FormatText(SyntheticIssues);
  AssertTrue('count footer present', Pos('2 issue(s).', lOut) > 0);
end;


procedure TOutputTextTest.OutputIsDeterministic;

var
  lIssues: TFpSonarIssueArray;

begin
  lIssues := SyntheticIssues;
  AssertEquals('two calls produce byte-identical output',
    FormatText(lIssues), FormatText(lIssues));
end;


procedure TOutputTextTest.ActiveIssuesCarryNoAnnotationOrSuppressedFooter;

var
  lOut: string;

begin
  // The all-active default set stays byte-identical to the pre-suppression output: no
  // [suppressed: ...] annotation and the plain 'N issue(s).' footer (no clause).
  lOut := FormatText(SyntheticIssues);
  AssertTrue('no suppression annotation on active issues',
    Pos('[suppressed:', lOut) = 0);
  AssertTrue('plain count footer', Pos('2 issue(s).', lOut) > 0);
  AssertTrue('no suppressed clause in the footer',
    Pos('suppressed)', lOut) = 0);
end;


procedure TOutputTextTest.SuppressedIssueAnnotatedWithSourceAndFooter;

var
  lOut: string;

begin
  lOut := FormatText(MutedIssues);
  // The muted issue carries the source annotation; the active one does not.
  AssertTrue('muted issue annotated with its source',
    Pos('[suppressed: baseline]', lOut) > 0);
  AssertTrue('the active issue line carries no annotation',
    Pos('src/foo.pas:4:7: minor LowercaseKeywords: keyword "begin" should be lowercase'
      + LineEnding, lOut) > 0);
  // Footer carries the suppressed count.
  AssertTrue('footer carries the suppressed count',
    Pos('2 issue(s) (1 suppressed).', lOut) > 0);
end;


initialization
  RegisterTest(TOutputTextTest);

end.

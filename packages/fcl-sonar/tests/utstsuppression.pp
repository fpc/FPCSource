{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the suppression mechanism (NOSONAR + glob matcher)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstSuppression;

{ suppression-foundation tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PScanner, FpSonar.Ingest, FpSonar.Types, FpSonar.Config,
  FpSonar.Issues,
  FpSonar.RuleFramework, UtstFixtures;

type
  { A synthetic LEX rule that emits one issue on a NOSONAR line and one on a
    plain line of nosonarfixture.pas (rows pinned to the fixture). }
  TSynthTwoLineRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Suppression tests. }
  TSuppressionTest = class(TTestCase)
  private
    function MakeSynthIssue(const aRuleId, aFileName: string; aLine: Integer;
      const aFingerprint: string): TFpSonarIssue;
    function MakeGlob(const aRule, aPath: string): TFpSonarSuppressionGlob;
    function IndexOfFingerprint(const aIssues: TFpSonarIssueArray;
      const aFingerprint: string): Integer;
  published
    procedure GlobMatchStarQuestionLiteralAndEmpty;
    procedure FindNoSonarDetectsMarkerAndReason;
    procedure MapAddFileCapturesLinesAndReason;
    procedure ApplySuppressionsNoSonarLineDropKeepsOrder;
    procedure ApplySuppressionsConfigGlobRuleAndPath;
    procedure ApplySuppressionsUniformOnDiagnostics;
    procedure ApplySuppressionsNilMapAndEmptyGlobsIsIdentity;
    procedure ApplySuppressionsExemptsTrackNoSonarFromNoSonar;
    procedure EngineNoSonarSuppressesSyntheticRuleIssue;
  end;


implementation

const
  cMode = 'OBJFPC';
  // Pinned to nosonarfixture.pas: line 12 carries the trailing NOSONAR marker,
  // line 13 (implementation) carries none.
  cNoSonarRow = 12;
  cPlainRow = 13;
  // Embedded suppression fixture (Approach A): line i+1 == [i].

  cNoSonarFixture: array[0..14] of string = (
    'unit NoSonarFixture;',
    '',
    '{ clean-parsing fixture for the suppression engine-integration test.',
    '  A synthetic LEX rule emits one issue on the marked line below (line 12, which',
    '  carries a trailing marker => that issue must be SUPPRESSED) and one issue on an',
    '  unmarked line (line 13 => survives). The exact line numbers are asserted by the',
    '  test, so do NOT reformat this file. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '// a marked line  NOSONAR keep',
    'implementation',
    '',
    'end.');

procedure TSynthTwoLineRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, cNoSonarRow, 1,
    cNoSonarRow, 1, FMetadata.Severity, FMetadata.Category,
    FMetadata.DefaultConfidence, FMetadata.MessageKey, [], 'synth-nosonar');
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, cPlainRow, 1,
    cPlainRow, 1, FMetadata.Severity, FMetadata.Category,
    FMetadata.DefaultConfidence, FMetadata.MessageKey, [], 'synth-plain');
end;


function TSuppressionTest.MakeSynthIssue(const aRuleId, aFileName: string;
  aLine: Integer; const aFingerprint: string): TFpSonarIssue;

begin
  Result := TFpSonarIssue.Make(aRuleId, aFileName, aLine, 1, aLine, 1, sevMinor,
    itCodeSmell, cfHigh, 'rule.' + aRuleId + '.message', []);
  Result.Fingerprint := aFingerprint;
end;


function TSuppressionTest.MakeGlob(const aRule, aPath: string):
  TFpSonarSuppressionGlob;

begin
  Result.RulePattern := aRule;
  Result.PathPattern := aPath;
end;


function TSuppressionTest.IndexOfFingerprint(const aIssues: TFpSonarIssueArray;
  const aFingerprint: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to High(aIssues) do
    if aIssues[i].Fingerprint = aFingerprint then
      begin
        Result := i;
        Exit;
      end;
end;


procedure TSuppressionTest.GlobMatchStarQuestionLiteralAndEmpty;

begin
  AssertTrue('* matches anything', GlobMatch('*', 'anything'));
  AssertTrue('prefix star', GlobMatch('Line*', 'LineTooLong'));
  AssertTrue('suffix star', GlobMatch('*Long', 'LineTooLong'));
  AssertTrue('? matches one char', GlobMatch('L?ne*', 'LineX'));
  AssertTrue('empty pattern is a wildcard', GlobMatch('', 'x'));
  AssertTrue('star spans path separators',
    GlobMatch('*/legacy/*', '/a/legacy/b.pas'));
  AssertTrue('double star behaves like single',
    GlobMatch('**/generated/*.pas', '/x/y/generated/z.pas'));

  AssertFalse('no match', GlobMatch('Naming*', 'LineTooLong'));
  AssertFalse('case-sensitive', GlobMatch('line*', 'LineTooLong'));
end;


procedure TSuppressionTest.FindNoSonarDetectsMarkerAndReason;

var
  lHasReason: Boolean;
  lReason: string;

begin
  AssertTrue('bare marker detected',
    FindNoSonar('// NOSONAR', lHasReason, lReason));
  AssertFalse('bare marker has no reason', lHasReason);
  AssertEquals('bare marker reason empty', '', lReason);

  AssertTrue('marker with reason detected',
    FindNoSonar('// NOSONAR  reason: legacy', lHasReason, lReason));
  AssertTrue('reason flagged present', lHasReason);
  AssertEquals('reason captured + trimmed', 'reason: legacy', lReason);

  AssertTrue('block-comment marker detected',
    FindNoSonar('{ NOSONAR }', lHasReason, lReason));

  AssertFalse('lower-case is not a marker',
    FindNoSonar('// nosonar', lHasReason, lReason));
  AssertFalse('prose "no sonar" is not a marker',
    FindNoSonar('// no sonar', lHasReason, lReason));
  AssertFalse('plain comment is not a marker',
    FindNoSonar('// hello', lHasReason, lReason));
end;


procedure TSuppressionTest.MapAddFileCapturesLinesAndReason;

var
  lMap: TFpSonarSuppressionMap;
  lToks: TFpSonarTokenArray;

begin
  SetLength(lToks, 2);
  lToks[0].Kind := tkComment;
  lToks[0].Text := '// NOSONAR keep';
  lToks[0].Row := 7;
  lToks[0].Col := 1;
  lToks[0].FileName := 'a.pas';
  lToks[1].Kind := tkIdentifier;
  lToks[1].Text := 'foo';
  lToks[1].Row := 8;
  lToks[1].Col := 1;
  lToks[1].FileName := 'a.pas';

  lMap := TFpSonarSuppressionMap.Create;
  try
    lMap.AddFile('a.pas', lToks);
    AssertTrue('marker line suppressed', lMap.IsSuppressed('a.pas', 7));
    AssertFalse('next line not suppressed', lMap.IsSuppressed('a.pas', 8));
    AssertFalse('other file not suppressed', lMap.IsSuppressed('other.pas', 7));
    AssertEquals('exactly one marker captured', 1, Length(lMap.Markers));
    AssertTrue('marker has a reason', lMap.Markers[0].HasReason);
    AssertEquals('marker reason captured', 'keep', lMap.Markers[0].Reason);
  finally
    lMap.Free;
  end;
end;


procedure TSuppressionTest.ApplySuppressionsNoSonarLineDropKeepsOrder;

var
  lMap: TFpSonarSuppressionMap;
  lToks: TFpSonarTokenArray;
  lIssues, lResult: TFpSonarIssueArray;

begin
  // Build a map suppressing a.pas:7 from a synthetic comment token.
  SetLength(lToks, 1);
  lToks[0].Kind := tkComment;
  lToks[0].Text := '// NOSONAR';
  lToks[0].Row := 7;
  lToks[0].Col := 1;
  lToks[0].FileName := 'a.pas';

  SetLength(lIssues, 3);
  lIssues[0] := MakeSynthIssue('RuleA', 'a.pas', 5, 'fp-5');
  lIssues[1] := MakeSynthIssue('RuleB', 'a.pas', 7, 'fp-7');
  lIssues[2] := MakeSynthIssue('RuleC', 'a.pas', 9, 'fp-9');

  lMap := TFpSonarSuppressionMap.Create;
  try
    lMap.AddFile('a.pas', lToks);
    lResult := ApplySuppressions(lIssues, lMap, nil);
    AssertEquals('one issue dropped', 2, Length(lResult));
    AssertEquals('order kept: first survivor is line 5', 'fp-5',
      lResult[0].Fingerprint);
    AssertEquals('order kept: second survivor is line 9', 'fp-9',
      lResult[1].Fingerprint);
    AssertEquals('line-7 issue dropped', -1,
      IndexOfFingerprint(lResult, 'fp-7'));
  finally
    lMap.Free;
  end;
end;


procedure TSuppressionTest.ApplySuppressionsConfigGlobRuleAndPath;

var
  lIssues, lResult: TFpSonarIssueArray;
  lGlobs: TFpSonarSuppressionGlobArray;

begin
  SetLength(lIssues, 4);
  lIssues[0] := MakeSynthIssue('NamingClass', '/src/a.pas', 1, 'fp-name-src');
  lIssues[1] := MakeSynthIssue('LineTooLong', '/src/gen/b.pas', 2, 'fp-line-gen');
  lIssues[2] := MakeSynthIssue('LineTooLong', '/src/foo/c.pas', 3, 'fp-line-foo');
  lIssues[3] := MakeSynthIssue('OtherRule', '/src/d.pas', 4, 'fp-other-src');

  // rule-only glob: drops any Naming* issue regardless of path.
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('Naming*', '');
  lResult := ApplySuppressions(lIssues, nil, lGlobs);
  AssertEquals('rule glob drops the one Naming issue', 3, Length(lResult));
  AssertEquals('Naming issue gone', -1,
    IndexOfFingerprint(lResult, 'fp-name-src'));

  // path-only glob: drops any issue under a gen path regardless of rule.
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('', '*/gen/*');
  lResult := ApplySuppressions(lIssues, nil, lGlobs);
  AssertEquals('path glob drops the one gen-path issue', 3, Length(lResult));
  AssertEquals('gen-path issue gone', -1,
    IndexOfFingerprint(lResult, 'fp-line-gen'));

  // rule+path glob: drops only the intersection.
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('LineTooLong', '*foo*');
  lResult := ApplySuppressions(lIssues, nil, lGlobs);
  AssertEquals('rule+path glob drops only the intersection', 3,
    Length(lResult));
  AssertEquals('intersection issue gone', -1,
    IndexOfFingerprint(lResult, 'fp-line-foo'));
  AssertTrue('the other LineTooLong (gen) survives the foo-only glob',
    IndexOfFingerprint(lResult, 'fp-line-gen') >= 0);
end;


procedure TSuppressionTest.ApplySuppressionsUniformOnDiagnostics;

var
  lMap: TFpSonarSuppressionMap;
  lToks: TFpSonarTokenArray;
  lIssues, lResult: TFpSonarIssueArray;
  lGlobs: TFpSonarSuppressionGlobArray;

begin
  // A reserved ParseError diagnostic is suppressible like any other RuleId.
  SetLength(lIssues, 1);
  lIssues[0] := MakeSynthIssue('ParseError', 'bad.pas', 13, 'fp-parse');

  // (a) by NOSONAR on its line.
  SetLength(lToks, 1);
  lToks[0].Kind := tkComment;
  lToks[0].Text := '// NOSONAR';
  lToks[0].Row := 13;
  lToks[0].Col := 1;
  lToks[0].FileName := 'bad.pas';
  lMap := TFpSonarSuppressionMap.Create;
  try
    lMap.AddFile('bad.pas', lToks);
    lResult := ApplySuppressions(lIssues, lMap, nil);
    AssertEquals('NOSONAR suppresses the ParseError diagnostic', 0,
      Length(lResult));
  finally
    lMap.Free;
  end;

  // (b) separately, by a rule glob naming the reserved diagnostic.
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('ParseError', '');
  lResult := ApplySuppressions(lIssues, nil, lGlobs);
  AssertEquals('rule glob suppresses the ParseError diagnostic', 0,
    Length(lResult));
end;


procedure TSuppressionTest.ApplySuppressionsNilMapAndEmptyGlobsIsIdentity;

var
  lIssues, lResult: TFpSonarIssueArray;

begin
  SetLength(lIssues, 2);
  lIssues[0] := MakeSynthIssue('RuleA', 'a.pas', 1, 'fp-a');
  lIssues[1] := MakeSynthIssue('RuleB', 'b.pas', 2, 'fp-b');

  lResult := ApplySuppressions(lIssues, nil, nil);
  AssertEquals('identity length', 2, Length(lResult));
  AssertEquals('identity order 0', 'fp-a', lResult[0].Fingerprint);
  AssertEquals('identity order 1', 'fp-b', lResult[1].Fingerprint);
end;


procedure TSuppressionTest.ApplySuppressionsExemptsTrackNoSonarFromNoSonar;

var
  lMap: TFpSonarSuppressionMap;
  lToks: TFpSonarTokenArray;
  lIssues, lResult: TFpSonarIssueArray;
  lGlobs: TFpSonarSuppressionGlobArray;

begin
  // A NOSONAR marker on a.pas:10 (the line the tracker's own issue sits on).
  SetLength(lToks, 1);
  lToks[0].Kind := tkComment;
  lToks[0].Text := '// NOSONAR';
  lToks[0].Row := 10;
  lToks[0].Col := 1;
  lToks[0].FileName := 'a.pas';

  // Two issues on that exact line: the #121 tracker and an ordinary rule.
  SetLength(lIssues, 2);
  lIssues[0] := MakeSynthIssue('TrackNoSonar', 'a.pas', 10, 'fp-tracker');
  lIssues[1] := MakeSynthIssue('OtherRule', 'a.pas', 10, 'fp-other');

  lMap := TFpSonarSuppressionMap.Create;
  try
    lMap.AddFile('a.pas', lToks);

    // (a)+(b): the tracker survives the NOSONAR self-suppression; the ordinary
    // rule's issue on the same line is still dropped.
    lResult := ApplySuppressions(lIssues, lMap, nil);
    AssertEquals('only the tracker survives the NOSONAR line', 1,
      Length(lResult));
    AssertEquals('the survivor is the tracker', 'fp-tracker',
      lResult[0].Fingerprint);
    AssertEquals('the ordinary on-line issue is dropped', -1,
      IndexOfFingerprint(lResult, 'fp-other'));

    // (c): a config rule-glob still suppresses the tracker (the exemption is
    // ONLY for the NOSONAR-marker branch, not config globs).
    SetLength(lGlobs, 1);
    lGlobs[0] := MakeGlob('TrackNoSonar', '');
    lResult := ApplySuppressions(lIssues, lMap, lGlobs);
    AssertEquals('rule-glob drops the tracker too', -1,
      IndexOfFingerprint(lResult, 'fp-tracker'));
  finally
    lMap.Free;
  end;
end;


procedure TSuppressionTest.EngineNoSonarSuppressesSyntheticRuleIssue;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lCollector: TFpSonarIssueCollector;
  lMap: TFpSonarSuppressionMap;
  lResult: TFpSonarIssueArray;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  lMap := TFpSonarSuppressionMap.Create;
  try
    lPath := lFix.Add('nosonarfixture.pas', cNoSonarFixture);
    lReg.Register(TSynthTwoLineRule.Create(TRuleMetadata.Make('SuppLex', rtLex,
      rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, '')));
    lEngine.SuppressionMap := lMap;

    // Dispatch fills the map from the fixture's comment tokens during Analyze.
    lEngine.Analyze(lPath, cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lCollector);

    AssertEquals('rule emitted two issues', 2, lCollector.Count);
    AssertTrue('map captured the NOSONAR line',
      lMap.IsSuppressed(lPath, cNoSonarRow));

    lResult := ApplySuppressions(lCollector.Issues, lMap, nil);
    AssertEquals('one issue survives suppression', 1, Length(lResult));
    AssertEquals('the surviving issue is the unmarked line', cPlainRow,
      lResult[0].StartLine);
    AssertTrue('the NOSONAR-line issue is gone',
      lResult[0].StartLine <> cNoSonarRow);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lMap.Free;
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TSuppressionTest);

end.

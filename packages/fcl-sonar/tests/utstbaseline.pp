{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the snapshot baseline and new-code mode

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstBaseline;

{ Snapshot baseline + new-code mode tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Baseline;

type
  { Baseline: make/serialize/load round-trip, determinism golden, FilterNewCode
    membership, tolerant load, and CLI option parsing. }
  TBaselineTest = class(TTestCase)
  private
    function MakeFpIssue(const aFingerprint, aRuleId, aFile: string): TFpSonarIssue;
    function GoldenJSON: string;
    function HasFingerprint(const aIssues: TFpSonarIssueArray;
      const aFingerprint: string): Boolean;
  published
    procedure RoundTripPreservesFingerprintSet;
    procedure SerializationIsDeterministicGolden;
    procedure FilterNewCodeDropsBaselinedKeepsAbsent;
    procedure FilterNewCodeEmptyBaselineKeepsAll;
    procedure FilterNewCodeDropsAllDuplicateFingerprintIssues;
    procedure LoadMissingFileFails;
    procedure LoadMalformedJsonFails;
    procedure LoadEmptyIssuesIsEverythingNew;
  end;


implementation

function TBaselineTest.MakeFpIssue(const aFingerprint, aRuleId, aFile: string):
  TFpSonarIssue;

begin
  Result := TFpSonarIssue.Make(aRuleId, aFile, 1, 1, 1, 1, sevMinor, itCodeSmell,
    cfHigh, 'rule.' + aRuleId + '.message', []);
  Result.Fingerprint := aFingerprint;
end;


function TBaselineTest.GoldenJSON: string;

const
  NL = #10; // FormatJSON emits LF line breaks (the canonical baseline bytes).

begin
  // FROZEN serialized form of the unsorted/duplicate issue set built in
  // SerializationIsDeterministicGolden. Any change to sort order, dedup, key
  // order, or the version stamp trips this and silently invalidates every
  // committed baseline — treat a change here as a breaking decision.
  Result :=
    '{' + NL +
    '  "_fpsonar" : {' + NL +
    '    "baseline" : "1",' + NL +
    '    "version" : "0.1.0"' + NL +
    '  },' + NL +
    '  "issues" : [' + NL +
    '    {' + NL +
    '      "fingerprint" : "0a1b2c3d4e5f6071",' + NL +
    '      "ruleId" : "AnotherRule",' + NL +
    '      "file" : "src/alpha.pas"' + NL +
    '    },' + NL +
    '    {' + NL +
    '      "fingerprint" : "ffff000011112222",' + NL +
    '      "ruleId" : "LineTooLong",' + NL +
    '      "file" : "src/zeta.pas"' + NL +
    '    }' + NL +
    '  ]' + NL +
    '}';
end;


function TBaselineTest.HasFingerprint(const aIssues: TFpSonarIssueArray;
  const aFingerprint: string): Boolean;

var
  i: Integer;

begin
  Result := False;
  for i := 0 to High(aIssues) do
    if aIssues[i].Fingerprint = aFingerprint then
      begin
        Result := True;
        Exit;
      end;
end;


procedure TBaselineTest.RoundTripPreservesFingerprintSet;

var
  lIssues: TFpSonarIssueArray;
  lBaseline, lReloaded: TFpSonarBaseline;
  lJson, lErr: string;

begin
  SetLength(lIssues, 3);
  lIssues[0] := MakeFpIssue('00000000aaaaaaaa', 'RuleA', 'src/a.pas');
  lIssues[1] := MakeFpIssue('11111111bbbbbbbb', 'RuleB', 'src/b.pas');
  lIssues[2] := MakeFpIssue('22222222cccccccc', 'RuleC', 'src/c.pas');

  lBaseline := TFpSonarBaseline.FromIssues(lIssues);
  lJson := lBaseline.ToJSON;
  AssertTrue('round-trip JSON loads', lReloaded.LoadFromJSON(lJson, lErr));
  AssertEquals('round-trip error is empty', '', lErr);
  AssertEquals('reloaded entry count', 3, Length(lReloaded.Entries));
  AssertTrue('fp A present after reload', lReloaded.Contains('00000000aaaaaaaa'));
  AssertTrue('fp B present after reload', lReloaded.Contains('11111111bbbbbbbb'));
  AssertTrue('fp C present after reload', lReloaded.Contains('22222222cccccccc'));
  AssertFalse('an absent fp is not contained',
    lReloaded.Contains('deadbeefdeadbeef'));
end;


procedure TBaselineTest.SerializationIsDeterministicGolden;

var
  lIssues: TFpSonarIssueArray;
  lBaseline: TFpSonarBaseline;

begin
  // Deliberately unsorted, with a duplicate fingerprint that must collapse to its
  // first entry by (fingerprint, ruleId, file) ordering (AnotherRule < TabIndentation).
  SetLength(lIssues, 4);
  lIssues[0] := MakeFpIssue('ffff000011112222', 'LineTooLong', 'src/zeta.pas');
  lIssues[1] := MakeFpIssue('0a1b2c3d4e5f6071', 'TabIndentation', 'src/alpha.pas');
  lIssues[2] := MakeFpIssue('ffff000011112222', 'LineTooLong', 'src/zeta.pas');
  lIssues[3] := MakeFpIssue('0a1b2c3d4e5f6071', 'AnotherRule', 'src/alpha.pas');

  lBaseline := TFpSonarBaseline.FromIssues(lIssues);
  AssertEquals('duplicate fingerprints collapsed', 2, Length(lBaseline.Entries));
  AssertEquals('serialized baseline matches golden', GoldenJSON,
    lBaseline.ToJSON);
  // Stable across calls (no timestamp / no nondeterminism).
  AssertEquals('two serializations are byte-identical',
    lBaseline.ToJSON, lBaseline.ToJSON);
end;


procedure TBaselineTest.FilterNewCodeDropsBaselinedKeepsAbsent;

var
  lAll, lBaselineIssues, lNew: TFpSonarIssueArray;
  lBaseline: TFpSonarBaseline;

begin
  SetLength(lAll, 3);
  lAll[0] := MakeFpIssue('aaaa000000000000', 'RuleA', 'src/a.pas');
  lAll[1] := MakeFpIssue('bbbb000000000000', 'RuleB', 'src/b.pas');
  lAll[2] := MakeFpIssue('cccc000000000000', 'RuleC', 'src/c.pas');

  // Baseline holds only the MIDDLE issue's fingerprint.
  SetLength(lBaselineIssues, 1);
  lBaselineIssues[0] := MakeFpIssue('bbbb000000000000', 'RuleB', 'src/b.pas');
  lBaseline := TFpSonarBaseline.FromIssues(lBaselineIssues);

  lNew := lBaseline.FilterNewCode(lAll);
  AssertEquals('one baselined issue dropped', 2, Length(lNew));
  // Order preserved: A then C.
  AssertEquals('first survivor is A', 'aaaa000000000000', lNew[0].Fingerprint);
  AssertEquals('second survivor is C', 'cccc000000000000', lNew[1].Fingerprint);
  AssertFalse('baselined fp B is gone', HasFingerprint(lNew, 'bbbb000000000000'));
end;


procedure TBaselineTest.FilterNewCodeEmptyBaselineKeepsAll;

var
  lAll, lNew: TFpSonarIssueArray;
  lEmpty: TFpSonarBaseline;
  lErr: string;

begin
  SetLength(lAll, 2);
  lAll[0] := MakeFpIssue('1111000000000000', 'RuleA', 'src/a.pas');
  lAll[1] := MakeFpIssue('2222000000000000', 'RuleB', 'src/b.pas');

  // An empty (everything-is-new) baseline from an empty issue set.
  lEmpty := TFpSonarBaseline.FromIssues(nil);
  AssertEquals('empty baseline has no entries', 0, Length(lEmpty.Entries));
  lNew := lEmpty.FilterNewCode(lAll);
  AssertEquals('empty baseline keeps everything', 2, Length(lNew));

  // An empty baseline parsed from an empty issues array behaves the same.
  AssertTrue('empty-issues JSON loads',
    lEmpty.LoadFromJSON('{"issues":[]}', lErr));
  AssertEquals('keeps all against parsed-empty baseline', 2,
    Length(lEmpty.FilterNewCode(lAll)));
end;


procedure TBaselineTest.FilterNewCodeDropsAllDuplicateFingerprintIssues;

var
  lAll, lBaselineIssues, lNew: TFpSonarIssueArray;
  lBaseline: TFpSonarBaseline;

begin
  // Set-membership (Open Question 2): N issues sharing one fingerprint all drop
  // when that fingerprint is baselined.
  SetLength(lAll, 3);
  lAll[0] := MakeFpIssue('dupe000000000000', 'RuleA', 'src/a.pas');
  lAll[1] := MakeFpIssue('keep000000000000', 'RuleB', 'src/b.pas');
  lAll[2] := MakeFpIssue('dupe000000000000', 'RuleA', 'src/a.pas');

  SetLength(lBaselineIssues, 1);
  lBaselineIssues[0] := MakeFpIssue('dupe000000000000', 'RuleA', 'src/a.pas');
  lBaseline := TFpSonarBaseline.FromIssues(lBaselineIssues);

  lNew := lBaseline.FilterNewCode(lAll);
  AssertEquals('both duplicate-fingerprint issues dropped', 1, Length(lNew));
  AssertEquals('the absent-fingerprint issue survives', 'keep000000000000',
    lNew[0].Fingerprint);
end;


procedure TBaselineTest.LoadMissingFileFails;

var
  lBaseline: TFpSonarBaseline;
  lErr: string;

begin
  AssertFalse('missing file fails to load',
    lBaseline.LoadFromFile('/no/such/baseline-file-5-1.json', lErr));
  AssertTrue('missing file yields a non-empty error', lErr <> '');
end;


procedure TBaselineTest.LoadMalformedJsonFails;

var
  lBaseline: TFpSonarBaseline;
  lErr: string;

begin
  AssertFalse('malformed JSON fails to load',
    lBaseline.LoadFromJSON('{ not json', lErr));
  AssertTrue('malformed JSON yields a non-empty error', lErr <> '');
  // A non-object root is also a clean failure, never a raise.
  AssertFalse('array root fails to load',
    lBaseline.LoadFromJSON('[1,2,3]', lErr));
  AssertTrue('array root yields a non-empty error', lErr <> '');
end;


procedure TBaselineTest.LoadEmptyIssuesIsEverythingNew;

var
  lBaseline: TFpSonarBaseline;
  lErr: string;
  lAll, lNew: TFpSonarIssueArray;

begin
  // A well-formed snapshot with zero fingerprints loads OK and means "all new".
  AssertTrue('version-stamped empty snapshot loads',
    lBaseline.LoadFromJSON('{"_fpsonar":{"baseline":"1","version":"0.1.0"}}', lErr));
  AssertEquals('empty snapshot error is empty', '', lErr);
  AssertEquals('empty snapshot has 0 fingerprints', 0, Length(lBaseline.Entries));

  SetLength(lAll, 1);
  lAll[0] := MakeFpIssue('aaaa111122223333', 'RuleA', 'src/a.pas');
  lNew := lBaseline.FilterNewCode(lAll);
  AssertEquals('everything is new against an empty snapshot', 1, Length(lNew));
end;

initialization
  RegisterTest(TBaselineTest);

end.

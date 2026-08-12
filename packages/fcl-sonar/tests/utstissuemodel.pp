{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the issue model, collector chokepoint and message catalog

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstIssueModel;

{ Issue model, collector chokepoint, message catalog and the diagnostics. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues;

type
  { Issue model + collector + messages + diagnostic-fold tests. }
  TIssueModelTest = class(TTestCase)
  published
    procedure MakeIssueFillsEveryFieldAndLeavesFingerprintEmpty;
    procedure CollectorIsSolePathPreservesOrderAndFingerprints;
    procedure MessageCatalogLookupArgsAndFallback;
    procedure DiagnosticFoldProducesRoutedIssue;
    procedure ScanErrorDiagnosticFoldsToScanErrorIssue;
  end;


implementation

procedure TIssueModelTest.MakeIssueFillsEveryFieldAndLeavesFingerprintEmpty;

var
  lIssue: TFpSonarIssue;

begin
  lIssue := TFpSonarIssue.Make('LEX001', 'src/foo.pas', 10, 3, 10, 8,
    sevMajor, itBug, cfMedium, 'rule.LEX001.message', ['alpha', 'beta']);

  AssertEquals('RuleId', 'LEX001', lIssue.RuleId);
  AssertEquals('FileName', 'src/foo.pas', lIssue.FileName);
  AssertEquals('StartLine', 10, lIssue.StartLine);
  AssertEquals('StartCol', 3, lIssue.StartCol);
  AssertEquals('EndLine', 10, lIssue.EndLine);
  AssertEquals('EndCol', 8, lIssue.EndCol);
  AssertEquals('Severity', Ord(sevMajor), Ord(lIssue.Severity));
  AssertEquals('IssueType', Ord(itBug), Ord(lIssue.IssueType));
  AssertEquals('Confidence', Ord(cfMedium), Ord(lIssue.Confidence));
  AssertEquals('MessageKey', 'rule.LEX001.message', lIssue.MessageKey);
  AssertEquals('MessageArgs length', 2, Length(lIssue.MessageArgs));
  AssertEquals('MessageArgs[0]', 'alpha', lIssue.MessageArgs[0]);
  AssertEquals('MessageArgs[1]', 'beta', lIssue.MessageArgs[1]);
  // TFpSonarIssue.Make never fingerprints; the collector is the sole authority.
  AssertEquals('Fingerprint left empty by TFpSonarIssue.Make', '', lIssue.Fingerprint);
end;


procedure TIssueModelTest.CollectorIsSolePathPreservesOrderAndFingerprints;

var
  lCollector: TFpSonarIssueCollector;

begin
  lCollector := TFpSonarIssueCollector.Create;
  try
    lCollector.AddIssue('LEX001', 'a.pas', 1, 1, 1, 1, sevInfo, itCodeSmell,
      cfHigh, 'rule.LEX001.message', [], 'first');
    lCollector.AddIssue('TOK002', 'b.pas', 2, 1, 2, 1, sevMinor, itBug,
      cfHigh, 'rule.TOK002.message', [], 'second');
    lCollector.AddIssue('AST003', 'c.pas', 3, 1, 3, 1, sevMajor, itVulnerability,
      cfLow, 'rule.AST003.message', [], 'third');

    AssertEquals('three issues collected', 3, lCollector.Count);
    AssertEquals('Issues array length matches Count', 3,
      Length(lCollector.Issues));

    // Insertion order preserved (deterministic).
    AssertEquals('order[0]', 'LEX001', lCollector.Issues[0].RuleId);
    AssertEquals('order[1]', 'TOK002', lCollector.Issues[1].RuleId);
    AssertEquals('order[2]', 'AST003', lCollector.Issues[2].RuleId);

    // Every issue carries a non-empty fingerprint equal to ComputeFingerprint
    // of its own inputs (the collector is the sole computer of it).
    AssertTrue('issue[0] fingerprint non-empty',
      lCollector.Issues[0].Fingerprint <> '');
    AssertEquals('issue[0] fingerprint matches inputs',
      ComputeFingerprint('LEX001', 'a.pas', 'first'),
      lCollector.Issues[0].Fingerprint);
    AssertEquals('issue[2] fingerprint matches inputs',
      ComputeFingerprint('AST003', 'c.pas', 'third'),
      lCollector.Issues[2].Fingerprint);
  finally
    lCollector.Free;
  end;
end;


procedure TIssueModelTest.MessageCatalogLookupArgsAndFallback;

var
  lUnknown: string;

begin
  // Known key + arg substitution (seeded in FpSonar.Types initialization).
  AssertEquals('parse-error message formats with its arg',
    'Parse error: boom', FormatMessage('rule.ParseError.message', ['boom']));

  // Unknown key => deterministic, obvious, NON-empty fallback, no exception.
  lUnknown := FormatMessage('rule.NoSuchKey.message', ['x']);
  AssertEquals('unknown-key fallback', '!rule.NoSuchKey.message!', lUnknown);
  AssertTrue('unknown-key fallback is non-empty', lUnknown <> '');

  // Arg-count mismatch must NOT raise; it falls back to the template verbatim.
  AssertEquals('arg-count mismatch falls back to template verbatim',
    'Parse error: %s', FormatMessage('rule.ParseError.message', []));
end;


procedure TIssueModelTest.DiagnosticFoldProducesRoutedIssue;

var
  lCollector: TFpSonarIssueCollector;
  lDiag: TFpSonarDiagnostic;
  lIssue: TFpSonarIssue;

begin
  // A parse failure at the source-file boundary produces it (position embedded in the
  // message, mirroring passrc's EParserError text).
  lDiag.FileName := 'src/faultbad.pas';
  lDiag.Row := 13;
  lDiag.Col := 10;
  lDiag.Kind := dkParseError;
  lDiag.Message := 'Identifier expected in file src/faultbad.pas at line 13 column 10';

  lCollector := TFpSonarIssueCollector.Create;
  try
    lCollector.CollectDiagnostic(lDiag);

    AssertEquals('exactly one folded issue', 1, lCollector.Count);
    lIssue := lCollector.Issues[0];

    AssertEquals('reserved ParseError RuleId', 'ParseError', lIssue.RuleId);
    AssertEquals('FileName carried through', 'src/faultbad.pas', lIssue.FileName);
    AssertEquals('mapped StartLine', 13, lIssue.StartLine);
    AssertEquals('mapped StartCol', 10, lIssue.StartCol);
    AssertEquals('severity sevMajor', Ord(sevMajor), Ord(lIssue.Severity));
    AssertEquals('type itCodeSmell', Ord(itCodeSmell), Ord(lIssue.IssueType));
    AssertEquals('confidence cfHigh', Ord(cfHigh), Ord(lIssue.Confidence));
    AssertEquals('message key', 'rule.ParseError.message', lIssue.MessageKey);

    // The message resolves through the catalog (full diagnostic text as the arg).
    AssertEquals('folded message resolves via the catalog',
      'Parse error: ' + lDiag.Message,
      FormatMessage(lIssue.MessageKey, lIssue.MessageArgs));

    // Routed through AddIssue => fingerprint set.
    AssertTrue('folded issue has a non-empty fingerprint',
      lIssue.Fingerprint <> '');

    // Line-independence of the fold: the SAME failure text at a
    // different line/col yields the SAME fingerprint, because the snippet is the
    // position-stripped message.
    lDiag.Row := 99;
    lDiag.Col := 1;
    lDiag.Message := 'Identifier expected in file src/faultbad.pas at line 99 column 1';
    lCollector.CollectDiagnostic(lDiag);
    AssertEquals('fold fingerprint is line-independent',
      lCollector.Issues[0].Fingerprint, lCollector.Issues[1].Fingerprint);
  finally
    lCollector.Free;
  end;
end;


procedure TIssueModelTest.ScanErrorDiagnosticFoldsToScanErrorIssue;

var
  lCollector: TFpSonarIssueCollector;
  lDiag: TFpSonarDiagnostic;
  lIssue: TFpSonarIssue;

begin
  // The scan-error half of the fold (dkScanError => reserved 'ScanError' RuleId
  // + 'rule.ScanError.message'). The scanner adapter emits dkScanError, so this
  // is a reachable path the engine will feed — assert it independently
  // of the parse-error path so the ScanError mapping cannot regress silently.
  lDiag.FileName := 'src/badtokens.pas';
  lDiag.Row := 7;
  lDiag.Col := 4;
  lDiag.Kind := dkScanError;
  lDiag.Message := 'Illegal character';

  lCollector := TFpSonarIssueCollector.Create;
  try
    lCollector.CollectDiagnostic(lDiag);

    AssertEquals('exactly one folded issue', 1, lCollector.Count);
    lIssue := lCollector.Issues[0];

    AssertEquals('reserved ScanError RuleId', 'ScanError', lIssue.RuleId);
    AssertEquals('FileName carried through', 'src/badtokens.pas', lIssue.FileName);
    AssertEquals('mapped StartLine', 7, lIssue.StartLine);
    AssertEquals('mapped StartCol', 4, lIssue.StartCol);
    AssertEquals('severity sevMajor', Ord(sevMajor), Ord(lIssue.Severity));
    AssertEquals('type itCodeSmell', Ord(itCodeSmell), Ord(lIssue.IssueType));
    AssertEquals('confidence cfHigh', Ord(cfHigh), Ord(lIssue.Confidence));
    AssertEquals('message key', 'rule.ScanError.message', lIssue.MessageKey);

    // The scan-error template seed resolves through the catalog.
    AssertEquals('folded message resolves via the catalog',
      'Scan error: Illegal character',
      FormatMessage(lIssue.MessageKey, lIssue.MessageArgs));

    // Routed through AddIssue => fingerprint set.
    AssertTrue('folded issue has a non-empty fingerprint',
      lIssue.Fingerprint <> '');
  finally
    lCollector.Free;
  end;
end;


initialization
  RegisterTest(TIssueModelTest);

end.

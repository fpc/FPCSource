{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the line-independent issue fingerprint

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstFingerprint;

{ Fingerprint tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Issues;

type
  { Fingerprint: line-independence, sensitivity, golden value, normalization. }
  TFingerprintTest = class(TTestCase)
  published
    procedure NormalizeCollapsesWhitespaceAndTrims;
    procedure LineIndependentAcrossIndentAndBlankLines;
    procedure SensitiveToRuleIdPathAndSnippet;
    procedure GoldenValueIsStable;
  end;


implementation

procedure TFingerprintTest.NormalizeCollapsesWhitespaceAndTrims;

begin
  // Leading/trailing trimmed; internal runs (spaces + tab) collapse to one space.
  AssertEquals('whitespace normalized', 'a b c',
    NormalizeSnippet('   a   b'#9'c '));
  // CR/LF dropped to a single gap.
  AssertEquals('CR/LF collapsed to a space', 'if X then',
    NormalizeSnippet('if'#13#10'X   then'));
  // Case is preserved (faithful + deterministic).
  AssertEquals('case preserved', 'IfThen', NormalizeSnippet('IfThen'));
end;


procedure TFingerprintTest.LineIndependentAcrossIndentAndBlankLines;

const
  cRule = 'LEX001';
  cPath = 'src/foo.pas';

var
  lBase, lIndented, lWithCRLF: string;

begin
  // The "same" offending code seen with different surrounding layout. The
  // fingerprint takes NO line, so all three must be byte-identical.
  lBase := ComputeFingerprint(cRule, cPath, 'if X then');
  lIndented := ComputeFingerprint(cRule, cPath, '    if X then  ');
  lWithCRLF := ComputeFingerprint(cRule, cPath, 'if'#13#10'X   then');

  AssertEquals('re-indentation does not change the fingerprint',
    lBase, lIndented);
  AssertEquals('CR/LF / blank-line layout does not change the fingerprint',
    lBase, lWithCRLF);
end;


procedure TFingerprintTest.SensitiveToRuleIdPathAndSnippet;

const
  cRule = 'LEX001';
  cPath = 'src/foo.pas';
  cSnip = 'if X then';

var
  lBase: string;

begin
  lBase := ComputeFingerprint(cRule, cPath, cSnip);
  AssertTrue('different ruleId => different fingerprint',
    lBase <> ComputeFingerprint('LEX002', cPath, cSnip));
  AssertTrue('different path => different fingerprint',
    lBase <> ComputeFingerprint(cRule, 'src/bar.pas', cSnip));
  AssertTrue('different snippet => different fingerprint',
    lBase <> ComputeFingerprint(cRule, cPath, 'if Y then'));
end;


procedure TFingerprintTest.GoldenValueIsStable;

const
  { FROZEN: FNV-1a-64 hex of 'LEX001'#31'src/foo.pas'#31'if X then'. Any change
    to the fingerprint algorithm trips this and silently invalidates every
    committed baseline — treat a change here as a breaking decision. }
  cGolden = '2de732b58a60c3a8';

var
  lFp: string;

begin
  lFp := ComputeFingerprint('LEX001', 'src/foo.pas', 'if X then');
  // 16-char lowercase hex.
  AssertEquals('fingerprint length is 16 hex chars', 16, Length(lFp));
  AssertEquals('fingerprint is lowercase', LowerCase(lFp), lFp);
  AssertEquals('golden fingerprint is stable', cGolden, lFp);
end;


initialization
  RegisterTest(TFingerprintTest);

end.

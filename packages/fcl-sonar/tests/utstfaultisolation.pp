{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for per-unit parse-fault isolation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstFaultIsolation;

{ Per-unit fault-isolation test. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.SourceFile, UtstFixtures, UtstCoreFixtures;

type
  { Fault-isolation test: a bad unit is isolated to a diagnostic; the run goes on. }
  TFaultIsolationTest = class(TTestCase)
  published
    procedure BadUnitIsolatedGoodUnitContinues;
    procedure MissingFileYieldsSingleFileNotFoundDiagnostic;
  end;


implementation

procedure TFaultIsolationTest.BadUnitIsolatedGoodUnitContinues;

const
  cBadErrorRow = 10;
  cBadErrorCol = 10;

var
  lFix: TTempFixtures;
  lFiles: array[0..1] of string;
  lSource: TFpSonarSourceFile;
  i: Integer;

begin
  lFix := TTempFixtures.Create;
  // One boundary instance, reused across the loop — each Analyze is isolated.
  lSource := TFpSonarSourceFile.Create;
  try
    lFiles[0] := lFix.Add('faultbad.pas', cFaultBad);
    lFiles[1] := lFix.Add('smokefixture.pas', cSmokeFixture);
    for i := Low(lFiles) to High(lFiles) do
      // The whole point: a bad unit must not raise out of the boundary, so the
      // loop reaches the good unit and completes.
      lSource.Analyze(lFiles[i], 'OBJFPC', ['FPC', 'CPUX86_64', 'UNIX', 'LINUX']);

    // After the loop the boundary holds the LAST (good) file's result.
    AssertTrue('good unit parsed', lSource.ParseSucceeded);
    AssertNotNull('good unit yields a Module', lSource.Module);
    AssertEquals('good unit has no diagnostics', 0,
      Length(lSource.Diagnostics));

    // Re-run the bad unit on its own to assert its isolated diagnostic.
    lSource.Analyze(lFiles[0], 'OBJFPC', ['FPC', 'CPUX86_64', 'UNIX', 'LINUX']);
    AssertFalse('bad unit does not parse', lSource.ParseSucceeded);
    AssertNull('bad unit yields no Module', lSource.Module);
    AssertEquals('bad unit yields exactly one diagnostic', 1,
      Length(lSource.Diagnostics));
    AssertEquals('diagnostic kind is dkParseError', Ord(dkParseError),
      Ord(lSource.Diagnostics[0].Kind));
    AssertTrue('diagnostic points at faultbad.pas',
      Pos('faultbad.pas', lSource.Diagnostics[0].FileName) > 0);
    AssertEquals('diagnostic row is the deliberate error line', cBadErrorRow,
      lSource.Diagnostics[0].Row);
    AssertEquals('diagnostic col is the deliberate error column', cBadErrorCol,
      lSource.Diagnostics[0].Col);
    // No silent all-clear: the diagnostic must carry a real message.
    AssertTrue('diagnostic carries a non-empty message',
      lSource.Diagnostics[0].Message <> '');
  finally
    lSource.Free;
    lFix.Free;
  end;
end;


procedure TFaultIsolationTest.MissingFileYieldsSingleFileNotFoundDiagnostic;

const
  // A path guaranteed not to exist: the resolver raises EFileNotFoundError.
  cMissing = '/nonexistent/fpsonar/missing-unit.pas';

var
  lSource: TFpSonarSourceFile;

begin
  lSource := TFpSonarSourceFile.Create;
  try
    lSource.Analyze(cMissing, 'OBJFPC', ['FPC', 'CPUX86_64', 'UNIX', 'LINUX']);
    AssertFalse('missing file does not parse', lSource.ParseSucceeded);
    // Reported ONCE (the parse is skipped for an unopenable file), NOT the old
    // scan+parse duplicate.
    AssertEquals('missing file yields exactly one diagnostic', 1,
      Length(lSource.Diagnostics));
    AssertEquals('diagnostic kind is dkFileNotFound', Ord(dkFileNotFound),
      Ord(lSource.Diagnostics[0].Kind));
    AssertTrue('diagnostic names the missing file',
      Pos('missing-unit.pas', lSource.Diagnostics[0].FileName) > 0);
    // No silent all-clear: the diagnostic must carry a real message.
    AssertTrue('diagnostic carries a non-empty message',
      lSource.Diagnostics[0].Message <> '');
  finally
    lSource.Free;
  end;
end;


initialization
  RegisterTest(TFaultIsolationTest);

end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the pas2js source dialect (parse-relevant switch subset)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstPas2jsDialect;

{ Tests that Dialect=dlPas2js accepts pas2js-only syntax (async methods,
  external classes) that the default FPC dialect rejects, while dlDefault
  stays byte-identical. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Ingest, UtstFixtures;

type
  { pas2js dialect test: the pas2js parse-relevant subset lets pas2js-only
    constructs parse, and the default dialect still rejects them. }
  TPas2jsDialectTest = class(TTestCase)
  private
    // Parses aLines under aDialect, returning True iff the parse succeeded.
    function ParsesUnder(const aName: string; const aLines: array of string;
      aDialect: TFpSonarDialect): boolean;
    // LEX-scans aLines under aDialect, returning True iff the scan succeeded.
    function ScansUnder(const aName: string; const aLines: array of string;
      aDialect: TFpSonarDialect): boolean;
  published
    procedure Pas2jsDialectParsesAsyncMethod;
    procedure DefaultDialectRejectsAsyncMethod;
    procedure Pas2jsDialectParsesExternalClass;
    procedure Pas2jsDialectScansAsmJavaScript;
    procedure DefaultDialectRejectsAsmJavaScript;
  end;


implementation

const
  // A unit with an async method — the reported wasm_worker_main defect: the
  // 'async' modifier is a ParseError unless po_AsyncProcs is enabled.
  cAsyncFixture: array[0..10] of string = (
    'unit AsyncFixture;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TWorker = class',
    '    procedure Run; async;',
    '  end;',
    'implementation',
    'procedure TWorker.Run; async;',
    'begin end;',
    'end.');

  // A unit with an external class — parses only with msExternalClass.
  cExternalClassFixture: array[0..7] of string = (
    'unit ExtFixture;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TJSObject = class external name ''Object'' end;',
    'implementation',
    'begin',
    'end.');

  // A unit with a pas2js asm block holding literal JavaScript (the '!' operator
  // is not a Pascal token). The LEX token scan must read the asm block whole,
  // like the parser, or it faults on '!'.
  cAsmJsFixture: array[0..9] of string = (
    'unit AsmFixture;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure P;',
    'begin',
    '  asm',
    '    if (!globalThis.x) { globalThis.x = 1; }',
    '  end;',
    'end.');

  cPas2jsDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');


function TPas2jsDialectTest.ParsesUnder(const aName: string;
  const aLines: array of string; aDialect: TFpSonarDialect): boolean;

var
  lFix: TTempFixtures;
  lParser: TFpSonarParser;
  lDiag: TFpSonarDiagnostic;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add(aName, aLines);
    lParser := TFpSonarParser.Create;
    try
      lParser.Dialect := aDialect;
      Result := lParser.TryParseFile(lPath, 'OBJFPC', cPas2jsDefines, lDiag);
    finally
      lParser.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TPas2jsDialectTest.Pas2jsDialectParsesAsyncMethod;

begin
  AssertTrue('async method parses under dlPas2js',
    ParsesUnder('asyncfixture.pas', cAsyncFixture, dlPas2js));
end;


procedure TPas2jsDialectTest.DefaultDialectRejectsAsyncMethod;

begin
  // The default dialect must still reject 'async' — proves dlPas2js is the
  // enabler and the default parse path is unchanged.
  AssertFalse('async method is a ParseError under dlDefault',
    ParsesUnder('asyncfixture.pas', cAsyncFixture, dlDefault));
end;


procedure TPas2jsDialectTest.Pas2jsDialectParsesExternalClass;

begin
  AssertTrue('external class parses under dlPas2js',
    ParsesUnder('extfixture.pas', cExternalClassFixture, dlPas2js));
end;


function TPas2jsDialectTest.ScansUnder(const aName: string;
  const aLines: array of string; aDialect: TFpSonarDialect): boolean;

var
  lFix: TTempFixtures;
  lScanner: TFpSonarScanner;
  lDiag: TFpSonarDiagnostic;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add(aName, aLines);
    lScanner := TFpSonarScanner.Create;
    try
      lScanner.Dialect := aDialect;
      Result := lScanner.TryScanFile(lPath, 'OBJFPC', cPas2jsDefines, lDiag);
    finally
      lScanner.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TPas2jsDialectTest.Pas2jsDialectScansAsmJavaScript;

begin
  // The LEX token feed must read the asm block whole (like the parser), so the
  // JS '!' does not fault the scan.
  AssertTrue('asm JavaScript scans clean under dlPas2js',
    ScansUnder('asmfixture.pas', cAsmJsFixture, dlPas2js));
end;


procedure TPas2jsDialectTest.DefaultDialectRejectsAsmJavaScript;

begin
  // Default: the per-token asm scan still faults on the JS '!' — proves the
  // asm-whole read is dialect-gated and the default scan path is unchanged.
  AssertFalse('asm JavaScript faults the scan under dlDefault',
    ScansUnder('asmfixture.pas', cAsmJsFixture, dlDefault));
end;


initialization
  RegisterTest(TPas2jsDialectTest);

end.

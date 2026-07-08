{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Foundation test: parse a trivial unit via TPasParser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstSmokeParse;

{ Foundation  test. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PScanner, PParser, PasTree, UtstFixtures, UtstCoreFixtures;

type
  { Minimal TPasTreeContainer engine required by TPasParser.
    Pattern copied from FPC's fcl-passrc examples/test_parser.pp. }
  TSmokeEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

  { Smoke test: parse one .pas fixture without error. }
  TSmokeParseTest = class(TTestCase)
  published
    procedure ParsesFixtureWithoutError;
  end;


implementation

function TSmokeEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;

begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
  { Register with the container so TPasTreeContainer.Destroy frees it. The
    parser never calls AddOwnedElement itself; without this the parsed tree
    leaks. This is the canonical fcl-passrc engine ownership pattern that the
    later per-rule test engines will reuse. }
  AddOwnedElement(Result);
end;


function TSmokeEngine.FindElement(const AName: String): TPasElement;

begin
  Result := nil;
end;


procedure TSmokeParseTest.ParsesFixtureWithoutError;

var
  lFix: TTempFixtures;
  lResolver: TFileResolver;
  lScanner: TPascalScanner;
  lParser: TPasParser;
  lEngine: TSmokeEngine;
  lModule: TPasModule;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('smokefixture.pas', cSmokeFixture);

    lModule := nil;
    lEngine := TSmokeEngine.Create;
    try
      lResolver := TFileResolver.Create;
      lScanner := TPascalScanner.Create(lResolver);
      lParser := TPasParser.Create(lScanner, lResolver, lEngine);
      try
        lScanner.SetCompilerMode('OBJFPC');
        lScanner.AddDefine('FPC');
        lScanner.AddDefine('CPUX86_64');
        lScanner.AddDefine('UNIX');
        lScanner.AddDefine('LINUX');
        // Enable the global parser prerequisite where the parser is invoked.
        lParser.Options := lParser.Options + [po_ArrayRangeExpr];
        lScanner.OpenFile(lPath);
        // Raises EParserError on any syntax error -> reported as a test error.
        lParser.ParseMain(lModule);
      finally
        lParser.Free;
        lScanner.Free;
        lResolver.Free;
      end;
      AssertNotNull('parser returned a TPasModule', lModule);
    finally
      // The engine owns every element it created (including lModule).
      lEngine.Free;
    end;
  finally
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TSmokeParseTest);

end.

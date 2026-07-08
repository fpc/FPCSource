{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the fcl-passrc parser adapter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstParser;

{ Test for the passrc parser adapter. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, FpSonar.Ingest, UtstFixtures, UtstCoreFixtures;

type
  { Parser adapter test: parses without error and owns the tree. }
  TParserTest = class(TTestCase)
  published
    procedure ParsesUnitReturningOwnedModule;
  end;


implementation

procedure TParserTest.ParsesUnitReturningOwnedModule;

var
  lFix: TTempFixtures;
  lParser: TFpSonarParser;
  lModule: TPasModule;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('smokefixture.pas', cSmokeFixture);
    lParser := TFpSonarParser.Create;
    try
      // Raises EParserError on any syntax error -> reported as a test error.
      lModule := lParser.ParseFile(lPath, 'OBJFPC',
        ['FPC', 'CPUX86_64', 'UNIX', 'LINUX']);
      AssertNotNull('parser returned a TPasModule', lModule);
      AssertSame('Module property matches returned module', lModule,
        lParser.Module);
    finally
      // Frees the engine and therefore the whole owned AST (heaptrc: 0 leaks).
      lParser.Free;
    end;
  finally
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TParserTest);

end.

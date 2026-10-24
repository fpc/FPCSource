{
  h2pas test suite: console test runner.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
program testh2pas;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcH2PasBase, tcDeclarations, tcTypeMapping, tcStructs,
  tcTypedefs, tcMacros, tcPreprocessor, tcOptions, tcPrefixes, tcErrorRecovery, tcCPreprocessor,
  tcOneTypeSection;

var
  Application: TTestRunner;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application:=TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='h2pas test suite';
  Application.Run;
  Application.Free;
end.

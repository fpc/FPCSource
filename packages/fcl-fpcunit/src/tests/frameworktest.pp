{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt

    an example of a console test runner of FPCUnit tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program frameworktest;

{$IFDEF FPC_DOTTEDUNITS}
uses
{$IFDEF Unix}
  UnixApi.CWString,
{$endif}
  FpcUnit.Runners.Console, System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.Report, asserttest,
  suitetest;
{$ELSE FPC_DOTTEDUNITS}
uses
{$IFDEF UNIX}
  cwstring,
{$endif}
  consoletestrunner, classes, SysUtils, fpcunit, testreport, asserttest,
  suitetest;
{$ENDIF FPC_DOTTEDUNITS}


Type
  TFPCUnitRunner = Class(TTestRunner);

Var
  App : TFPCUnitRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  App:=TFPCUnitRunner.Create(Nil);
  App.Initialize;
  App.Title := 'FPCUnit Test Suite';
  App.Run;
  App.Free;
end.

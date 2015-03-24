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

uses
  consoletestrunner, classes, SysUtils, fpcunit, testreport, asserttest,
  suitetest;

Const
  ShortOpts = 'alh';
  Longopts : Array[1..5] of String = (
    'all','list','format:','suite:','help');
  Version = 'Version 0.1';

Type
  TFPCUnitRunner = Class(TTestRunner)

  end;






Var
  App : TFPCUnitRunner;

begin
  App:=TFPCUnitRunner.Create(Nil);
  App.Initialize;
  App.Title := 'FPCUnit Test Suite';
  App.Run;
  App.Free;
end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Console test runner for fcl-async (Phase 4)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
program testasync;

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, fpcunit, testregistry, consoletestrunner,
  utasync;

type
  TAsyncTestRunner = class(TTestRunner)
  protected
  end;

var
  Application: TAsyncTestRunner;
begin
  DefaultFormat := fPlain;
  Application := TAsyncTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'fcl-async unit tests';
    Application.Run;
  finally
    Application.Free;
  end;
end.


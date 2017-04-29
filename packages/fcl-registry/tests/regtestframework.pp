program regtestframework;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  fpcunit,  testreport, testregistry,
// Units wich contains the tests
  tcxmlreg,
  testbasics, consoletestrunner;

Var
  A : TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  A:=TTestRunner.Create(Nil);
  try
    A.Initialize;
    A.Run;
  finally
    A.Free;
  end;
end.

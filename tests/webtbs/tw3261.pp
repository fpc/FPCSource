{ %norun }
{ %target=win32,wince }

{ Source provided for Free Pascal Bug Report 3261 }
{ Submitted by "Andreas Hausladen" on  2004-08-18 }
{ e-mail: Andreas.Hausladen@gmx.de }
program test;

{$mode delphi}

type
  TTest = class(TObject)
  public
    procedure myfunc(arg1: Integer; arg2: TTest); overload;
    procedure myfunc(arg1: TTest); overload;
  end;

procedure TTest.myfunc(arg1: Integer; arg2: TTest); external 'test.dll' name 'TTest_myfunc';

procedure TTest.myfunc(arg1: TTest);
begin
  if arg1 = nil then ;
end;

begin
end.

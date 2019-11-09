unit utrtl;

{$mode objfpc}

interface

uses punit;


Function SysUtilsTest(Const ATestName : ShortString; ARun : TTestRun) : PTest;
Function DosTest(Const ATestName : ShortString; ARun : TTestRun) : PTest;
Function EnsureSuite(Const AName : ShortString) : PSuite;
Function ShowDebugOutput : Boolean;

implementation

function DosTest(const ATestName: ShortString; ARun: TTestRun): PTest;
begin
  Result:=AddTest(ATestName,ARun,EnsureSuite('Dos'));
end;

Function EnsureSuite(Const AName : ShortString) : PSuite;

begin
  Result:=GetSuite(AName);
  if Result=Nil then
    Result:=AddSuite(AName);
end;

Function SysUtilsTest(Const ATestName : ShortString; ARun : TTestRun) : PTest;

begin
  Result:=AddTest(ATestName,ARun,EnsureSuite('SysUtils'));
end;

Var
  ReadDebug : Boolean;
  ShowDebug : Boolean;

function ShowDebugOutput: Boolean;
begin
  if Not ReadDebug then
    begin
    ReadDebug:=True;
    ShowDebug:=SysGetSetting('debug')='true';
    end;
  Result:=ShowDebug;
end;

end.


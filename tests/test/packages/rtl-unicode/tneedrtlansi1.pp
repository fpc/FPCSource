{ %target=linux }
{ NeedRTLAnsi follows the code page of cwstring, issue #41586 }
program tneedrtlansi1;

{$mode objfpc}{$h+}

uses
  cwstring, BaseUnix, SysUtils, UTF8Utils;


{ Runs this program with aEnv as complete environment, returns its exit code }
function RunWithEnv(const aEnv: array of AnsiString): Integer;

var
  lArgs: array[0..2] of PAnsiChar;
  lEnv: array of PAnsiChar;
  lPid: TPid;
  lStatus: cint;
  i: Integer;

begin
  lArgs[0]:=PAnsiChar(ParamStr(0));
  lArgs[1]:='child';
  lArgs[2]:=nil;
  SetLength(lEnv,Length(aEnv)+1);
  for i:=0 to High(aEnv) do
    lEnv[i]:=PAnsiChar(aEnv[i]);
  lEnv[Length(aEnv)]:=nil;
  lPid:=FpFork;
  if lPid=0 then
    begin
    FpExecve(lArgs[0],@lArgs[0],@lEnv[0]);
    FpExit(127);
    end;
  FpWaitPid(lPid,@lStatus,0);
  Result:=WEXITSTATUS(lStatus);
end;


begin
  if ParamStr(1)='child' then
    begin
    if NeedRTLAnsi then
      halt(1);
    halt(0);
    end;
  if RunWithEnv(['LC_ALL=C'])<>1 then
    halt(10);
  if RunWithEnv(['LC_ALL=C.UTF-8'])<>0 then
    halt(11);
  if RunWithEnv([])<>0 then
    halt(12);
  writeln('ok');
end.

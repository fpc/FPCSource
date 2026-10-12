{ %target=linux }
{ NeedRTLAnsi without cwstring falls back to LC_ALL, LC_MESSAGES and LANG, issue #41586 }
program tneedrtlansi2;

{$mode objfpc}{$h+}

uses
  BaseUnix, SysUtils, UTF8Utils;


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
  if RunWithEnv(['LANG=en_US.ISO-8859-1'])<>1 then
    halt(10);
  if RunWithEnv(['LANG=en_US.UTF-8'])<>0 then
    halt(11);
  if RunWithEnv(['LC_ALL=de_DE.ISO-8859-15','LANG=en_US.UTF-8'])<>1 then
    halt(12);
  if RunWithEnv([])<>0 then
    halt(13);
  writeln('ok');
end.

{ %target=linux }
{ %skiptarget=$nosharedlib }
{ %needlibrary }
{ %delfiles=tlibparam1a }

{ library parameters stop at a nil entry of argv shortened by the host, issue #20752 }
program tlibparam1b;

{$mode objfpc}{$h+}

uses
  SysUtils, DynLibs;

type
  TLibParamCount = function: Longint; cdecl;
  TLibParamStr = procedure(aIndex: Longint; aBuf: PAnsiChar; aSize: Longint); cdecl;

// Load the library after shortening argv to one parameter and check what it sees
procedure CheckLibrary;

var
  lLib: TLibHandle;
  lCount: TLibParamCount;
  lStr, lSysStr: TLibParamStr;
  lBuf: array[0..255] of AnsiChar;

begin
  argv[2]:=nil;
  lLib:=LoadLibrary(ExtractFilePath(ParamStr(0))+'libtlibparam1a.so');
  if lLib=NilHandle then
    halt(2);
  lCount:=TLibParamCount(GetProcedureAddress(lLib,'LibParamCount'));
  lStr:=TLibParamStr(GetProcedureAddress(lLib,'LibParamStr'));
  lSysStr:=TLibParamStr(GetProcedureAddress(lLib,'LibSysParamStr'));
  if not (Assigned(lCount) and Assigned(lStr) and Assigned(lSysStr)) then
    halt(3);
  if lCount()<>1 then
    halt(11);
  lStr(1,@lBuf[0],SizeOf(lBuf));
  if StrPas(PAnsiChar(@lBuf[0]))<>'a' then
    halt(12);
  lStr(2,@lBuf[0],SizeOf(lBuf));
  if StrPas(PAnsiChar(@lBuf[0]))<>'' then
    halt(13);
  lSysStr(1,@lBuf[0],SizeOf(lBuf));
  if StrPas(PAnsiChar(@lBuf[0]))<>'a' then
    halt(14);
  lSysStr(2,@lBuf[0],SizeOf(lBuf));
  if StrPas(PAnsiChar(@lBuf[0]))<>'' then
    halt(15);
  UnloadLibrary(lLib);
end;


var
  lExitCode: Integer;

begin
  if ParamCount=3 then
    CheckLibrary
  else
    begin
    lExitCode:=ExecuteProcess(ParamStr(0),['a','b','c']);
    if lExitCode<>0 then
      halt(lExitCode);
    writeln('ok');
    end;
end.

{$mode objfpc}
{$H+}
Unit Libuuid;

interface

uses SysUtils,dynlibs;

Var
  LibUUIDName : String = 'libuuid.so.1';
  ProcName    : String = 'uuid_generate_time';
  
function CCreateGUID(out Guid: TGUID): HResult;

Implementation


Type
  TGenProc = procedure (out Guid: TGUID);cdecl;

var
  Handle : TLibHandle;
  GenFunc : TGenProc;

Function InitLibrary : Boolean;

begin
  Result:=(Handle<>NilHandle);
  If Not result then
    begin
    Handle:=LoadLibrary(LibUUIDName);
    Result:=(Handle<>NilHandle);
    if Result then
      begin
      GenFunc:=TGenProc(GetProcedureAddress(Handle, ProcName));
      Result:=(GenFunc<>nil);
      end;
    end;
end;


function CCreateGUID(out Guid: TGUID): HResult;

begin
  Result := -1;
  if InitLibrary then
    begin
    GenFunc(Guid);
    Result := 0;
    end;
end;


initialization
  If InitLibrary then
    OnCreateGUID:=@CCreateGUID;
Finalization
  if (Handle<>NilHandle)  then
    UnLoadLibrary(Handle)
end.

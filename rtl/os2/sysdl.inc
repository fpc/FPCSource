{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS dependent part for loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


threadvar
 DynLibErrNo: cardinal;
 DynLibErrPath: array [0..259] of char;

function SysLoadLibraryA (const Name: RawbyteString): TLibHandle;
var
 Handle: longint;
begin
 DynLibErrPath [0] := #0;
 DynLibErrNo := DosLoadModule (@DynLibErrPath [0], SizeOf (DynLibErrPath),
                                                         PAnsiChar (Name), Handle);
 if DynLibErrNo = 0 then
  Result := Handle
 else
  begin
   Result := NilHandle;
   OSErrorWatch (DynLibErrNo);
  end;
end;

function SysLoadLibraryU (const Name: UnicodeString): TLibHandle;
begin
  Result := SysLoadLibraryA(ToSingleByteFileSystemEncodedFileName(Name));
end;

function SysGetProcedureAddress (Lib: TLibHandle; const ProcName: AnsiString): pointer;
var
 P: pointer;
begin
 DynLibErrPath [0] := #0;
 DynLibErrNo := DosQueryProcAddr (Lib, 0, PChar (ProcName), P);
 if DynLibErrNo = 0 then
  Result := P
 else
  begin
   Result := nil;
   OSErrorWatch (DynLibErrNo);
  end;
end;

function SysGetProcedureAddressOrdinal (Lib: TLibHandle; Ordinal: TOrdinalEntry): pointer;
var
 P: pointer;
begin
 DynLibErrPath [0] := #0;
 DynLibErrNo := DosQueryProcAddr (Lib, Ordinal, nil, P);
 if DynLibErrNo = 0 then
  Result := P
 else
  begin
   Result := nil;
   OSErrorWatch (DynLibErrNo);
  end;
end;

function SysUnloadLibrary (Lib: TLibHandle): boolean;
begin
 DynLibErrPath [0] := #0;
 DynLibErrNo := DosFreeModule (Lib);
 Result := DynLibErrNo = 0;
 if DynLibErrNo <> 0 then
  OSErrorWatch (DynLibErrNo);
end;

function GetDynLibsError: longint;
begin
 GetDynLibsError := DynLibErrNo;
end;

function SysGetDynLibsErrorStr: string;
const
 SysMsgFile: array [0..10] of char = 'OSO001.MSG'#0;
var
 VarArr: array [1..9] of PChar;
 OutBuf: array [0..999] of char;
 RetMsgSize: cardinal;
 RC: cardinal;
begin
 if DynLibErrNo = 0 then
  SysGetDynLibsErrorStr := ''
 else
  begin
   Result := '';
   VarArr [1] := @DynLibErrPath [0];
   RC := DosGetMessage (@VarArr, 1, @OutBuf [0], SizeOf (OutBuf),
                                     DynLibErrNo, @SysMsgFile [0], RetMsgSize);
   if RC = 0 then
    begin
     SetLength (Result, RetMsgSize);
     Move (OutBuf [0], Result [1], RetMsgSize);
    end
   else
    begin
     Str (DynLibErrNo, Result);
     Result := 'Error ' + Result;
     if DynLibErrPath [0] <> #0 then
      Result := StrPas (@DynLibErrPath [0]) + ' - ' + Result;
     OSErrorWatch (RC);
    end;
  end;
end;

function SysGetLoadErrorStr: string;
begin
 SysGetLoadErrorStr := SysGetDynLibsErrorStr;
end;

const
  SysDynLibsManager: TDynLibsManager = (
    LoadLibraryU: @SysLoadLibraryU;
    LoadLibraryA: @SysLoadLibraryA;
    GetProcAddress: @SysGetProcedureAddress;
    GetProcAddressOrdinal: @SysGetProcedureAddressOrdinal;
    UnloadLibrary: @SysUnloadLibrary;
    GetLoadErrorStr: @SysGetLoadErrorStr;
  );

procedure InitSystemDynLibs;
begin
  SetDynLibsManager(SysDynLibsManager);
end;


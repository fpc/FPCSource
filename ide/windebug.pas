{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Pierre Muller

    Win32 specific debugger routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit windebug;

interface

  type
    DebuggeeState = (Running_State,Stopped_State);

  procedure  ChangeDebuggeeWindowTitleTo(State : DebuggeeState);

  function CygDrivePrefix : string;

implementation

uses
  gdbint,
  strings,
  windows;

const
  CygDrivePrefixKey1 = 'Software';
  CygDrivePrefixKey2 = 'Cygnus Solutions';
  CygDrivePrefixKey3 = 'Cygwin';
  CygDrivePrefixKey4 = 'mounts v2';
  CygDrivePrefixKey = 'cygdrive prefix';

function CygDrivePrefix : string;
var
  i : longint;
  length : dword;
  Value : pchar;
  _type : dword;
  Key,NKey : HKey;
begin
  Length:=0;
  Key:=HKEY_CURRENT_USER;
  i := RegOpenKeyEx(Key, CygDrivePrefixKey1, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
  if i=ERROR_SUCCESS then
    begin
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey2, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey3, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey4, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegQueryValueEx( Key, CygDrivePrefixKey, nil, @_type, nil, @length);
    end;
  if i<>ERROR_SUCCESS then
    CygDrivePrefix:='/cygdrive'
  else
    Begin
      GetMem(Value,Length);
      i := RegQueryValueEx( Key, CygDrivePrefixKey, nil, @_type, LPByte(Value), @length);
      if i<>ERROR_SUCCESS then
        CygDrivePrefix:='/cygdrive'
      else
        CygDrivePrefix:=StrPas(Value);
      FreeMem(Value,Length);
    End;
  if Key<>HKEY_CURRENT_USER then
    RegCloseKey(Key);
end;

function GetWindowHandle(H : HWND; state : LPARAM) : WINBOOL;stdcall;
   var pTitle, pEnd, pNewTitle : pchar;
       len : longint;
   begin
     GetWindowHandle:=true;
     GetMem(pTitle,256);
     { we want all windows !! }
     if GetWindowThreadProcessId(H,nil)=inferior_pid then
       begin
         len:=GetWindowText(H,pTitle,256);
         if DebuggeeState(State) = Stopped_State then
           begin
             GetMem(pNewTitle,len+50);
             pEnd:=strpos(pTitle,'... running under FP debugger');
             if assigned(pEnd) then
               pEnd^:=#0;
             strcopy(pNewTitle,pTitle);
             strcat(pNewTitle,'... stopped by FP debugger');
             SetWindowText(H,pNewTitle);
             FreeMem(pNewTitle,len+50);
           end
         else if DebuggeeState(State) = Running_State then
           begin
             GetMem(pNewTitle,len+50);
             pEnd:=strpos(pTitle,'... stopped by FP debugger');
             if assigned(pEnd) then
               pEnd^:=#0;
             strcopy(pNewTitle,pTitle);
             strcat(pNewTitle,'... running under FP debugger');
             SetWindowText(H,pNewTitle);
             FreeMem(pNewTitle,len+50);
           end;
       end;
     FreeMem(pTitle,256);
   end;

 procedure  ChangeDebuggeeWindowTitleTo(State : DebuggeeState);
   begin
     EnumWindows(EnumWindowsProc(@GetWindowHandle),longint(State));
   end;

end.

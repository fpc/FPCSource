{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team.

    Contains missing wince functions present in win32 api

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  *What should contain this file*
  
  functions missing on wince api

}

{$ifdef read_interface}

function LoadLibraryA(lpLibFileName:LPCSTR):HINST;
function RegCreateKeyExA(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD;samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; var hkResult: HKEY; lpdwDisposition:LPDWORD):LONG;
function RegDeleteKeyA(hKey:HKEY; lpSubKey:LPCSTR):LONG;
function RegDeleteValueA(hKey:HKEY; lpValueName:LPCSTR):LONG;
function RegEnumKeyExA(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; var cbName:DWORD; lpReserved:LPDWORD;lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;
function RegEnumValueA(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; var cbValueName: DWORD; lpReserved:LPDWORD;lpType:LPDWORD; lpData:pointer; lpcbData:LPDWORD):LONG;
function RegQueryValueExA(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:pointer;lpcbData:LPDWORD):LONG;
function RegQueryInfoKeyA(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD;lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD;
  lpcbMaxValueLen:LPDWORD;lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;
function RegOpenKeyExA(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM; var hkResult: HKEY):LONG;
function RegSetValueExA(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; lpData:pointer;cbData:DWORD):LONG;

{$endif read_interface}


{$ifdef read_implementation}

function LoadLibraryA(lpLibFileName:LPCSTR):HINST;
var  ws: PWideChar;
begin
  ws:=PCharToPWideChar(lpLibFileName);
  Result:=Windows.LoadLibrary(ws);
  FreeMem(ws);
end;

function RegCreateKeyExA(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD;samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; var hkResult: HKEY; lpdwDisposition:LPDWORD):LONG;
var  lpwsSubKey,
     lpwsClass: PWideChar;
begin
 lpwsSubKey:=PCharToPWideChar(lpSubKey);
 lpwsClass:=PCharToPWideChar(lpClass);
 Result:=RegCreateKeyExW(hKey,lpwsSubKey,Reserved,lpwsClass,dwOptions,samDesired,lpSecurityAttributes, hkResult,lpdwDisposition);
 FreeMem(lpwsClass); FreeMem(lpwsSubKey);
end;

function RegDeleteKeyA(hKey:HKEY; lpSubKey:LPCSTR):LONG;
var  lpwsSubKey: PWideChar;
begin
 lpwsSubKey:=PCharToPWideChar(lpSubKey);
 Result:=RegDeleteKeyW(hKey,lpwsSubKey);
 FreeMem(lpwsSubKey);
end;

function RegDeleteValueA(hKey:HKEY; lpValueName:LPCSTR):LONG;
var  lpwsValueName: PWideChar;
begin
 lpwsValueName:=PCharToPWideChar(lpValueName);
 Result:=RegDeleteValueW(hKey,lpwsValueName);
 FreeMem(lpwsValueName);
end;

function RegEnumKeyExA(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; var cbName:DWORD; lpReserved:LPDWORD;lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;
var
  ClassBuf, NameBuf: array[0..255] of WideChar;
  csz, nsz: DWORD;
begin
 csz:=SizeOf(ClassBuf) div SizeOf(WideChar);
 nsz:=SizeOf(NameBuf) div SizeOf(WideChar);
 Result:=RegEnumKeyExW(hKey, dwIndex, NameBuf, nsz, lpReserved, ClassBuf, @csz, lpftLastWriteTime);
 if cbName > nsz then
   WideToAnsiBuf(NameBuf, nsz, lpName, cbName);
 cbName:=nsz;
 if (Result = 0) and (lpcbClass <> nil) then begin
   if (lpClass <> nil) and (lpcbClass^ > csz) then
     WideToAnsiBuf(ClassBuf, csz, lpClass, lpcbClass^);
   lpcbClass^:=csz;
 end;
end;

function RegEnumValueA(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; var cbValueName: DWORD; lpReserved:LPDWORD;lpType:LPDWORD; lpData:pointer; lpcbData:LPDWORD):LONG;
var
  t, sz, nsz: DWORD;
  DataBuf: pointer;
  NameBuf: array[0..255] of WideChar;
begin
 if lpcbData <> nil then
   sz:=lpcbData^
 else
   sz:=0;
 if lpData <> nil then begin
   sz:=sz*2;
   GetMem(DataBuf, sz);
 end
 else
   DataBuf:=nil;
 nsz:=SizeOf(NameBuf) div SizeOf(WideChar);
 Result:=RegEnumValueW(hKey, dwIndex, NameBuf, @nsz, lpReserved, @t, DataBuf, @sz);
 if cbValueName > nsz then
   WideToAnsiBuf(NameBuf, nsz, lpValueName, cbValueName);
 cbValueName:=nsz;
 if Result = 0 then begin
   if lpType <> nil then
     lpType^:=t;
   if lpcbData <> nil then begin
     if (lpData <> nil) and (t in [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ]) then
       sz:=WideCharToMultiByte(CP_ACP, 0, DataBuf, sz div SizeOf(WideChar), lpData, lpcbData^, nil, nil)
     else begin
       if sz > lpcbData^ then
         Result:=ERROR_MORE_DATA
       else
         if lpData <> nil then
           Move(DataBuf^, lpData^, sz);
     end;
     lpcbData^:=sz;
   end;
 end;
 if DataBuf <> nil then
   FreeMem(DataBuf);
end;

function RegQueryValueExA(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:pointer;lpcbData:LPDWORD):LONG;
var
  lpwsValueName: PWideChar;
  t, sz: DWORD;
  DataBuf: pointer;
begin
 if lpcbData <> nil then
   sz:=lpcbData^
 else
   sz:=0;
 if lpData <> nil then begin
   sz:=sz*2;
   GetMem(DataBuf, sz);
 end
 else
   DataBuf:=nil;
 lpwsValueName:=PCharToPWideChar(lpValueName);
 Result:=RegQueryValueExW(hKey, lpwsValueName, lpReserved, @t, DataBuf, @sz);
 FreeMem(lpwsValueName);
 if Result = 0 then begin
   if lpType <> nil then
     lpType^:=t;
   if lpcbData <> nil then begin
     if t in [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ] then begin
       sz:=sz div SizeOf(WideChar);
       if lpData <> nil then
         sz:=WideCharToMultiByte(CP_ACP, 0, DataBuf, sz, lpData, lpcbData^, nil, nil);
     end
     else begin
       if sz > lpcbData^ then
         Result:=ERROR_MORE_DATA
       else
         if lpData <> nil then
           Move(DataBuf^, lpData^, sz);
     end;
     lpcbData^:=sz;
   end;
 end;
 if DataBuf <> nil then
   FreeMem(DataBuf);
end;

function RegQueryInfoKeyA(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD;lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD;
  lpcbMaxValueLen:LPDWORD;lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;
var
  ClassBuf: array[0..255] of WideChar;
  csz: DWORD;
begin
 csz:=SizeOf(ClassBuf) div SizeOf(WideChar);
 Result:=RegQueryInfoKeyW(hKey, ClassBuf, @csz, lpReserved, lpcSubKeys , lpcbMaxSubKeyLen, lpcbMaxClassLen,
                          lpcValues, lpcbMaxValueNameLen, lpcbMaxValueLen, lpcbSecurityDescriptor, lpftLastWriteTime);
 if (Result = 0) and (lpcbClass <> nil) then begin
   if (lpClass <> nil) and (lpcbClass^ > csz) then
     WideToAnsiBuf(ClassBuf, csz, lpClass, lpcbClass^);
   lpcbClass^:=csz;
 end;
end;

function RegOpenKeyExA(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM; var hkResult: HKEY):LONG;
var  lpwsSubKey: PWideChar;
begin
 lpwsSubKey:=PCharToPWideChar(lpSubKey);
 Result:=RegOpenKeyExW(hKey, lpwsSubKey, ulOptions, samDesired, hkResult);
 FreeMem(lpwsSubKey);
end;

function RegSetValueExA(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; lpData:pointer;cbData:DWORD):LONG;
var
  lpwsValueName, ws: PWideChar;
  DataBuf: pointer;
  sz: DWORD;
begin
 lpwsValueName:=PCharToPWideChar(lpValueName);
 if dwType in [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ] then begin
   ws:=PCharToPWideChar(lpData, cbData, @sz);
   if (cbData > 0) and (PChar(lpData)[cbData - 1] <> #0) then
     Inc(sz, SizeOf(WideChar));
   cbData:=sz;
   DataBuf:=ws;
 end
 else begin
   DataBuf:=lpData;
   ws:=nil;
 end;
 Result:=RegSetValueExW(hKey, lpwsValueName, Reserved, dwType, DataBuf, cbData);
 FreeMem(lpwsValueName);
 if ws <> nil then
   FreeMem(ws);
end;

{$endif read_implementation}


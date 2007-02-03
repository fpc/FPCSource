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
  ws:=StringToPWideChar(lpLibFileName);
  Result:=Windows.LoadLibrary(ws);
  FreeMem(ws);
end;

function RegCreateKeyExA(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD;samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; var hkResult: HKEY; lpdwDisposition:LPDWORD):LONG;
var  lpwsSubKey,
     lpwsClass: PWideChar;
begin
 lpwsSubKey:=StringToPWideChar(lpSubKey);
 lpwsClass:=StringToPWideChar(lpClass);
 Result:=RegCreateKeyExW(hKey,lpwsSubKey,Reserved,lpwsClass,dwOptions,samDesired,lpSecurityAttributes, hkResult,lpdwDisposition);
 FreeMem(lpwsClass); FreeMem(lpwsSubKey);
end;

function RegDeleteKeyA(hKey:HKEY; lpSubKey:LPCSTR):LONG;
var  lpwsSubKey: PWideChar;
begin
 lpwsSubKey:=StringToPWideChar(lpSubKey);
 Result:=RegDeleteKeyW(hKey,lpwsSubKey);
 FreeMem(lpwsSubKey);
end;

function RegDeleteValueA(hKey:HKEY; lpValueName:LPCSTR):LONG;
var  lpwsValueName: PWideChar;
begin
 lpwsValueName:=StringToPWideChar(lpValueName);
 Result:=RegDeleteValueW(hKey,lpwsValueName);
 FreeMem(lpwsValueName);
end;

function RegEnumKeyExA(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; var cbName:DWORD; lpReserved:LPDWORD;lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;
var  lpwsName,
     lpwsClass: PWideChar;
begin
 lpwsName:=StringToPWideChar(lpName);
 lpwsClass:=StringToPWideChar(lpClass);
 Result:=RegEnumKeyExW(hKey, dwIndex, lpwsName, cbName, lpReserved, lpwsClass, lpcbClass, lpftLastWriteTime);
 FreeMem(lpwsName); FreeMem(lpwsClass);
end;

function RegEnumValueA(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; var cbValueName: DWORD; lpReserved:LPDWORD;lpType:LPDWORD; lpData:pointer; lpcbData:LPDWORD):LONG;
var  lpwsValueName: PWideChar;
begin
 lpwsValueName:=StringToPWideChar(lpValueName);
 Result:=RegEnumValueW(hKey, dwIndex, lpwsValueName, @cbValueName, lpReserved, lpType, lpData, lpcbData);
 FreeMem(lpwsValueName);
end;

function RegQueryValueExA(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:pointer;lpcbData:LPDWORD):LONG;
var  lpwsValueName: PWideChar;
begin
 lpwsValueName:=StringToPWideChar(lpValueName);
 Result:=RegQueryValueExW(hKey, lpwsValueName, lpReserved, lpType, lpData, lpcbData);
 FreeMem(lpwsValueName);
end;

function RegQueryInfoKeyA(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD;lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD;
  lpcbMaxValueLen:LPDWORD;lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;
var  lpwsClass: PWideChar;
begin
 lpwsClass:=StringToPWideChar(lpClass);
 Result:=RegQueryInfoKeyW(hKey, lpwsClass, lpcbClass, lpReserved, lpcSubKeys , lpcbMaxSubKeyLen, lpcbMaxClassLen,
  lpcValues, lpcbMaxValueNameLen, lpcbMaxValueLen, lpcbSecurityDescriptor, lpftLastWriteTime);
 FreeMem(lpwsClass);
end;

function RegOpenKeyExA(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM; var hkResult: HKEY):LONG;
var  lpwsSubKey: PWideChar;
begin
 lpwsSubKey:=StringToPWideChar(lpSubKey);
 Result:=RegOpenKeyExW(hKey, lpwsSubKey, ulOptions, samDesired, hkResult);
 FreeMem(lpwsSubKey);
end;

function RegSetValueExA(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; lpData:pointer;cbData:DWORD):LONG;
var  lpwsValueName: PWideChar;
begin
 lpwsValueName:=StringToPWideChar(lpValueName);
 Result:=RegSetValueExW(hKey, lpwsValueName, Reserved, dwType, lpData, cbData);
 FreeMem(lpwsValueName);
end;

{$endif read_implementation}


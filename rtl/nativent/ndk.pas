{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit NDK;

interface

{$I sysndk.inc}

function NtDelayExecution(aAlertable: Boolean; aInterval: PLargeInteger): NTSTATUS; stdcall; external ntdll;


function LdrGetProcedureAddress(hModule: THandle; psName: PNtUnicodeString; dwOrdinal: LongWord; var pProcedure: Pointer): NTSTATUS; stdcall; external ntdll;
function LdrLoadDll(pwPath : PWord; pdwFlags : LongWord; pusPath : PNtUnicodeString; var phModule : THandle): NTSTATUS; stdcall; external ntdll;
function LdrUnloadDll(hModule: THandle): NTSTATUS; stdcall; external ntdll;


implementation

end.


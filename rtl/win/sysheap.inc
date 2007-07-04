{
    Basic heap handling for windows platforms

    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2005 by Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

   { memory functions }
   function GetProcessHeap : THandle;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetProcessHeap';
   function HeapAlloc(hHeap : DWord; dwFlags : DWord; dwBytes : SIZE_T) : pointer;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'HeapAlloc';
   function HeapFree(hHeap : THandle; dwFlags : dword; lpMem: pointer) : boolean;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'HeapFree';
{$IFDEF SYSTEMDEBUG}
   function WinAPIHeapSize(hHeap : THandle; dwFlags : DWord; ptr : Pointer) : DWord;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external 'kernel32' name 'HeapSize';
{$ENDIF}


function SysOSAlloc(size: ptruint): pointer;
var
  p : pointer;
begin
  p := HeapAlloc(GetProcessHeap, 0, size);
{$ifdef DUMPGROW}
  Writeln('new heap part at $',hexstr(p), ' size = ',WinAPIHeapSize(GetProcessHeap()));
{$endif}
  SysOSAlloc := p;
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
begin
  HeapFree(GetProcessHeap, 0, p);
end;

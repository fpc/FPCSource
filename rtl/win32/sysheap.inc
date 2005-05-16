{
    $Id: sysheap.inc,v 1.1 2005/02/06 13:06:20 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

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
   function GetProcessHeap : DWord;
     stdcall;external 'kernel32' name 'GetProcessHeap';
   function HeapAlloc(hHeap : DWord; dwFlags : DWord; dwBytes : DWord) : Longint;
     stdcall;external 'kernel32' name 'HeapAlloc';
   function HeapFree(hHeap : dword; dwFlags : dword; lpMem: pointer) : boolean;
     stdcall;external 'kernel32' name 'HeapFree';
{$IFDEF SYSTEMDEBUG}
   function WinAPIHeapSize(hHeap : DWord; dwFlags : DWord; ptr : Pointer) : DWord;
     stdcall;external 'kernel32' name 'HeapSize';
{$ENDIF}


function SysOSAlloc(size: ptrint): pointer;
var
  l : longword;
begin
  l := HeapAlloc(GetProcessHeap, 0, size);
{$ifdef DUMPGROW}
  Writeln('new heap part at $',hexstr(l,8), ' size = ',WinAPIHeapSize(GetProcessHeap()));
{$endif}
  SysOSAlloc := pointer(l);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  HeapFree(GetProcessHeap, 0, p);
end;


{
   $Log: sysheap.inc,v $
   Revision 1.1  2005/02/06 13:06:20  peter
     * moved file and dir functions to sysfile/sysdir
     * win32 thread in systemunit

}


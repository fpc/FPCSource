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

{$IFDEF SYSTEMDEBUG}
   function WinAPIHeapSize(hHeap : THandle; dwFlags : DWord; ptr : Pointer) : SIZE_T;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'HeapSize';
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

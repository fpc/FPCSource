{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team.

    Win32 Memory Functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   { memory functions }
   function GlobalAlloc(mode,size:longint):longint;
     external 'kernel32' name 'GlobalAlloc';
   function GlobalHandle(p:pointer):longint;
     external 'kernel32' name 'GlobalHandle';
   function GlobalLock(handle:longint):pointer;
     external 'kernel32' name 'GlobalLock';
   function GlobalUnlock(h:longint):longint;
     external 'kernel32' name 'GlobalUnlock';
   function GlobalFree(h:longint):longint;
     external 'kernel32' name 'GlobalUnlock';
   procedure GlobalMemoryStatus(p:pointer);
     external 'kernel32' name 'GlobalMemoryStatus';
   function LocalAlloc(uFlags : UINT;uBytes :UINT) : HLOCAL;
     external 'kernel32' name 'LocalAlloc';
   function LocalFree(hMem:HLOCAL):HLOCAL;
     external 'kernel32' name 'LocalFree';


type
  errproc=function(size:longint):integer;

procedure MemError(size:longint);
const
  message:pchar='Abnormal Termination';
  caption:pchar='Memory Management Error!';
var
  res:integer;
begin
  repeat
    res:=errproc(heaperror)(size);
    if res=0 then
     begin;
       messagebox(0,caption,message,$10);
       halt(getlasterror);
     end;
  until res<>2;
end;


procedure getmem(var p:pointer;size:longint);[public,alias: 'GETMEM'];
begin
  p:=GlobalLock(GlobalAlloc(258,size));
  if p=nil then
   memerror(size)
end;


procedure freemem(var p:pointer;size:longint);[public,alias: 'FREEMEM'];
var
  h:longint;
begin
  h:=GlobalHandle(p);
  if h<>0 then
   if globalunlock(h)=0 then
    if GlobalFree(h)=0 then
     begin
      p:=nil;
      exit{allways if success!!!}
     end;
  p:=nil;
  memerror(size);
end;


function memmax(_maxavail:boolean):longint;
const
  status:record
   dwLength,
   dwMemoryLoad,
   dwTotalPhys,
   dwAvailPhys,
   dwTotalPageFile,
   dwAvailPageFile,
   dwTotalVirtual,
   dwAvailVirtual:longint;
  end=(dwLength:32);
begin
  GlobalMemoryStatus(@status);
  if _maxavail then
   memmax:=status.dwAvailPageFile
  else
   memmax:=status.dwAvailVirtual;
end;


function memavail:longint;
begin
  memavail:=memmax(false);
end;


function maxavail:longint;
begin
  maxavail:=memmax(true);
end;


function growheap(size:longint):integer;
begin
  growheap:=0;
end;

{
  $Log$
  Revision 1.3  1998-06-10 10:39:19  peter
    * working w32 rtl

}


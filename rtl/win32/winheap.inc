{
    $Id$
    This file is part of the Free Pascal run time library.
    FPC Pascal system unit for the Win32 API.
    Copyright (c) 1998 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
 errproc=function(size:longint):integer;

procedure MemError(size:longint);
 const
  message:array[1..21]of char=(
   'A','b','n','o','r','m','a','l',' ',
   'T','e','r','m','i','n','a','t','i','o','n',#0);
  caption:array[1..25]of char=(
   'M','e','m','o','r','y',' ',
   'M','a','n','a','g','e','m','e','n','t',' ',
   'E','r','r','o','r','!',#0);
 var
  res:integer;
 begin
  repeat
   res:=errproc(heaperror)(size);
   if res=0 then
    begin;
     messagebox(0,@caption,@message,$10);
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
  Revision 1.1  1998-03-25 11:18:47  root
  Initial revision

  Revision 1.2  1998/03/05 22:39:06  florian
    + log and history

}

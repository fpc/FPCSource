{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    Contains the stuff for writing .a files directly

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit owar;
interface

uses
  cobjects,owbase;

type
  tarhdr=packed record
    name : array[0..15] of char;
    date : array[0..11] of char;
    uid  : array[0..5] of char;
    gid  : array[0..5] of char;
    mode : array[0..7] of char;
    size : array[0..9] of char;
    fmag : array[0..1] of char;
  end;

  parobjectwriter=^tarobjectwriter;
  tarobjectwriter=object(tobjectwriter)
    constructor Init(const Aarfn:string);
    destructor  Done;virtual;
    procedure create(const fn:string);virtual;
    procedure close;virtual;
    procedure writesym(sym:string);virtual;
    procedure write(var b;len:longint);virtual;
  private
    arfn   : string;
    arhdr  : tarhdr;
    symreloc,
    symstr,
    lfnstr,
    ardata,
    objdata : PDynamicArray;
    objfixup : longint;
    objfn   : string;
    timestamp : string[12];
    procedure createarhdr(fn:string;size:longint;const gid,uid,mode:string);
    procedure writear;
  end;


implementation

uses
   verbose,
{$ifdef Delphi}
   dmisc;
{$else Delphi}
   dos;
{$endif Delphi}

const
{$ifdef TP}
  symrelocbufsize = 32;
  symstrbufsize = 256;
  lfnstrbufsize = 256;
  arbufsize  = 256;
  objbufsize = 256;
{$else}
  symrelocbufsize = 1024;
  symstrbufsize = 8192;
  lfnstrbufsize = 4096;
  arbufsize  = 65536;
  objbufsize = 16384;
{$endif}

{*****************************************************************************
                                   Helpers
*****************************************************************************}

const
  C1970=2440588;
  D0=1461;
  D1=146097;
  D2=1721119;
Function Gregorian2Julian(DT:DateTime):LongInt;
Var
  Century,XYear,Month : LongInt;
Begin
  Month:=DT.Month;
  If Month<=2 Then
   Begin
     Dec(DT.Year);
     Inc(Month,12);
   End;
  Dec(Month,3);
  Century:=(longint(DT.Year Div 100)*D1) shr 2;
  XYear:=(longint(DT.Year Mod 100)*D0) shr 2;
  Gregorian2Julian:=((((Month*153)+2) div 5)+DT.Day)+D2+XYear+Century;
End;

function DT2Unix(DT:DateTime):LongInt;
Begin
  DT2Unix:=(Gregorian2Julian(DT)-C1970)*86400+(LongInt(DT.Hour)*3600)+(DT.Min*60)+DT.Sec;
end;


{*****************************************************************************
                                TArObjectWriter
*****************************************************************************}

constructor tarobjectwriter.init(const Aarfn:string);
var
  time  : datetime;
  dummy : word;
begin
  arfn:=Aarfn;
  new(arData,init(1,arbufsize));
  new(symreloc,init(4,symrelocbufsize));
  new(symstr,init(1,symstrbufsize));
  new(lfnstr,init(1,lfnstrbufsize));
{ create timestamp }
  getdate(time.year,time.month,time.day,dummy);
  gettime(time.hour,time.min,time.sec,dummy);
  Str(DT2Unix(time),timestamp);
end;


destructor tarobjectwriter.done;
begin
  if Errorcount=0 then
   writear;
  dispose(arData,done);
  dispose(symreloc,done);
  dispose(symstr,done);
  dispose(lfnstr,done);
end;


procedure tarobjectwriter.createarhdr(fn:string;size:longint;const gid,uid,mode:string);
var
  tmp : string[9];
begin
  fillchar(arhdr,sizeof(tarhdr),' ');
{ create ar header }
  fn:=fn+'/';
  if length(fn)>16 then
   begin
     arhdr.name[0]:='/';
     str(lfnstr^.usedsize,tmp);
     move(tmp[1],arhdr.name[1],length(tmp));
     fn:=fn+#10;
     lfnstr^.write(fn[1],length(fn));
   end
  else
   move(fn[1],arhdr.name,length(fn));
  { don't write a date if also no gid/uid/mode is specified }
  if gid<>'' then
    move(timestamp[1],arhdr.date,sizeof(timestamp));
  str(size,tmp);
  move(tmp[1],arhdr.size,length(tmp));
  move(gid[1],arhdr.gid,length(gid));
  move(uid[1],arhdr.uid,length(uid));
  move(mode[1],arhdr.mode,length(mode));
  arhdr.fmag:='`'#10;
end;


procedure tarobjectwriter.create(const fn:string);
begin
  objfn:=fn;
  objfixup:=ardata^.usedsize;
{ reset size }
  new(objdata,init(1,objbufsize));
end;


procedure tarobjectwriter.close;
begin
  objdata^.align(2);
{ fix the size in the header }
  createarhdr(objfn,objdata^.usedsize,'42','42','644');
{ write the header }
  ardata^.write(arhdr,sizeof(tarhdr));
{ write the data of this objfile }
  ardata^.write(objdata^.data^,objdata^.usedsize);
{ free this object }
  dispose(objdata,done);
end;


procedure tarobjectwriter.writesym(sym:string);
begin
  sym:=sym+#0;
  symreloc^.write(objfixup,1);
  symstr^.write(sym[1],length(sym));
end;


procedure tarobjectwriter.write(var b;len:longint);
begin
  objdata^.write(b,len);
end;


procedure tarobjectwriter.writear;

  function lsb2msb(l:longint):longint;
  type
    bytearr=array[0..3] of byte;
  var
    l1 : longint;
  begin
    bytearr(l1)[0]:=bytearr(l)[3];
    bytearr(l1)[1]:=bytearr(l)[2];
    bytearr(l1)[2]:=bytearr(l)[1];
    bytearr(l1)[3]:=bytearr(l)[0];
    lsb2msb:=l1;
  end;

const
  armagic:array[1..8] of char='!<arch>'#10;
type
  plongint=^longint;
var
  arf : file;
  fixup,
  relocs,i : longint;
begin
  assign(arf,arfn);
  {$I-}
   rewrite(arf,1);
  {$I+}
  if ioresult<>0 then
   exit;
  blockwrite(arf,armagic,sizeof(armagic));
  { align first, because we need the size for the fixups of the symbol reloc }
  if lfnstr^.usedsize>0 then
   lfnstr^.align(2);
  if symreloc^.usedsize>0 then
   begin
     symstr^.align(2);
     fixup:=12+sizeof(tarhdr)+symreloc^.usedsize+symstr^.usedsize;
     if lfnstr^.usedsize>0 then
      inc(fixup,lfnstr^.usedsize+sizeof(tarhdr));
     relocs:=symreloc^.count;
     for i:=0to relocs-1 do
      plongint(@symreloc^.data[i*4])^:=lsb2msb(plongint(@symreloc^.data[i*4])^+fixup);
     createarhdr('',4+symreloc^.usedsize+symstr^.usedsize,'0','0','0');
     blockwrite(arf,arhdr,sizeof(tarhdr));
     relocs:=lsb2msb(relocs);
     blockwrite(arf,relocs,4);
     blockwrite(arf,symreloc^.data^,symreloc^.usedsize);
     blockwrite(arf,symstr^.data^,symstr^.usedsize);
   end;
  if lfnstr^.usedsize>0 then
   begin
     createarhdr('/',lfnstr^.usedsize,'','','');
     blockwrite(arf,arhdr,sizeof(tarhdr));
     blockwrite(arf,lfnstr^.data^,lfnstr^.usedsize);
   end;
  blockwrite(arf,ardata^.data^,ardata^.usedsize);
  system.close(arf);
end;


end.
{
  $Log$
  Revision 1.5  2000-01-07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.4  1999/07/18 10:19:59  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.3  1999/05/09 11:38:06  peter
    * don't write .o and link if errors occure during assembling

  Revision 1.2  1999/05/04 21:44:53  florian
    * changes to compile it with Delphi 4.0

  Revision 1.1  1999/05/01 13:24:26  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.1  1999/03/18 20:30:51  peter
    + .a writer

}

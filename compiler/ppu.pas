{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Routines to read/write ppu files

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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ppu;
interface

const
{ buffer sizes }
  maxentrysize = 1024;
{$ifdef TP}
  ppubufsize   = 1024;
{$else}
  ppubufsize   = 16384;
{$endif}

{ppu entries}
  mainentryid         = 1;
  subentryid          = 2;
  {special}
  iberror             = 0;
  ibenddefs           = 250;
  ibendsyms           = 251;
  ibendinterface      = 252;
  ibendimplementation = 253;
  ibendbrowser        = 254;
  ibend               = 255;
  {general}
  ibmodulename     = 1;
  ibsourcefiles    = 2;
  ibloadunit_int   = 3;
  ibloadunit_imp   = 4;
  ibinitunit       = 5;
  iblinkofiles     = 6;
  iblinksharedlibs = 7;
  iblinkstaticlibs = 8;
  ibdbxcount       = 9;
  ibsymref         = 10;
  ibdefref         = 11;
  {syms}
  ibtypesym       = 20;
  ibprocsym       = 21;
  ibvarsym        = 22;
  ibconstsym      = 23;
  ibenumsym       = 24;
  ibtypedconstsym = 25;
  ibabsolutesym   = 26;
  ibpropertysym   = 27;
  ibvarsym_C      = 28;
  {defenitions}
  iborddef        = 40;
  ibpointerdef    = 41;
  ibarraydef      = 42;
  ibprocdef       = 43;
  ibstringdef     = 44;
  ibrecorddef     = 45;
  ibfiledef       = 46;
  ibformaldef     = 47;
  ibobjectdef     = 48;
  ibenumdef       = 49;
  ibsetdef        = 50;
  ibprocvardef    = 51;
  ibfloatdef      = 52;
  ibclassrefdef   = 53;
  iblongstringdef = 54;
  ibansistringdef = 55;
  ibwidestringdef = 56;

{ unit flags }
  uf_init          = $1;
  uf_finalize      = $2;
  uf_big_endian    = $4;
  uf_has_dbx       = $8;
  uf_has_browser   = $10;
  uf_smartlink     = $20;
  uf_in_library    = $40; { is the file in another file than <ppufile>.* ? }
  uf_static_linked = $80;
  uf_shared_linked = $100;


type
{$ifdef m68k}
  ppureal=single;
{$else}
  ppureal=extended;
{$endif}

type
  tppuerror=(ppuentrytoobig,ppuentryerror);

  tppuheader=packed record
    id       : array[1..3] of char; { = 'PPU' }
    ver      : array[1..3] of char;
    compiler : word;
    cpu      : word;
    target   : word;
    flags    : longint;
    size     : longint; { size of the ppufile without header }
    checksum : longint; { checksum for this ppufile }
  end;

  tppuentry=packed record
    id   : byte;
    nr   : byte;
    size : longint;
  end;

  pppufile=^tppufile;
  tppufile=object
    f        : file;
    mode     : byte; {0 - Closed, 1 - Reading, 2 - Writing}
    error    : boolean;
    fname    : string;
    fsize    : longint;

    header   : tppuheader;
    size,crc : longint;
    do_crc,
    change_endian : boolean;

    buf      : pchar;
    bufstart,
    bufsize,
    bufidx   : longint;
    entrybufstart,
    entrystart,
    entryidx : longint;
    entry    : tppuentry;
    entrytyp : byte;

    constructor init(fn:string);
    destructor  done;
    procedure flush;
    procedure close;
    function  CheckPPUId:boolean;
    function  GetPPUVersion:longint;
    procedure NewHeader;
    procedure NewEntry;
  {read}
    function  open:boolean;
    procedure reloadbuf;
    procedure readdata(var b;len:longint);
    procedure skipdata(len:longint);
    function  readentry:byte;
    function  EndOfEntry:boolean;
    procedure getdatabuf(var b;len:longint;var result:longint);
    procedure getdata(var b;len:longint);
    function  getbyte:byte;
    function  getword:word;
    function  getlongint:longint;
    function  getreal:ppureal;
    function  getstring:string;
    function  skipuntilentry(untilb:byte):boolean;
  {write}
    function  create:boolean;
    procedure writeheader;
    procedure writebuf;
    procedure writedata(var b;len:longint);
    procedure writeentry(ibnr:byte);
    procedure putdata(var b;len:longint);
    procedure putbyte(b:byte);
    procedure putword(w:word);
    procedure putlongint(l:longint);
    procedure putreal(d:ppureal);
    procedure putstring(s:string);
  end;

implementation


{*****************************************************************************
                                   Crc 32
*****************************************************************************}

var
  Crc32Tbl : array[0..255] of longint;

procedure MakeCRC32Tbl;
var
  crc : longint;
  i,n : byte;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if odd(crc) then
       crc:=(crc shr 1) xor $edb88320
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;



{CRC 32}
Function Crc32(Const HStr:String):longint;
var
  i,InitCrc : longint;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  InitCrc:=$ffffffff;
  for i:=1to Length(Hstr) do
   InitCrc:=Crc32Tbl[byte(InitCrc) xor ord(Hstr[i])] xor (InitCrc shr 8);
  Crc32:=InitCrc;
end;



Function UpdateCrc32(InitCrc:longint;var InBuf;InLen:Longint):longint;
var
  i : word;
  p : pchar;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  p:=@InBuf;
  for i:=1to InLen do
   begin
     InitCrc:=Crc32Tbl[byte(InitCrc) xor byte(p^)] xor (InitCrc shr 8);
     inc(longint(p));
   end;
  UpdateCrc32:=InitCrc;
end;



Function UpdCrc32(InitCrc:longint;b:byte):longint;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  UpdCrc32:=Crc32Tbl[byte(InitCrc) xor b] xor (InitCrc shr 8);
end;


{*****************************************************************************
                                  TPPUFile
*****************************************************************************}

constructor tppufile.init(fn:string);
begin
  fname:=fn;
  change_endian:=false;
  Mode:=0;
  NewHeader;
  Error:=false;
  getmem(buf,ppubufsize);
end;


destructor tppufile.done;
begin
  close;
  freemem(buf,ppubufsize);
end;


procedure tppufile.flush;
begin
  if Mode=2 then
   writebuf;
end;


procedure tppufile.close;
var
  i : word;
begin
  if Mode<>0 then
   begin
     Flush;
     {$I-}
      system.close(f);
     {$I+}
     i:=ioresult;
     Mode:=0;
   end;
end;


function tppufile.CheckPPUId:boolean;
begin
  CheckPPUId:=((Header.Id[1]='P') and (Header.Id[2]='P') and (Header.Id[3]='U'));
end;


function tppufile.GetPPUVersion:longint;
var
  l    : longint;
  code : word;
begin
  Val(header.ver[1]+header.ver[2]+header.ver[3],l,code);
  if code=0 then
   GetPPUVersion:=l
  else
   GetPPUVersion:=0;
end;


procedure tppufile.NewHeader;
begin
  fillchar(header,sizeof(tppuheader),0);
  with header do
   begin
     Id[1]:='P';
     Id[2]:='P';
     Id[3]:='U';
     Ver[1]:='0';
     Ver[2]:='1';
     Ver[3]:='5';
   end;
end;


{*****************************************************************************
                                TPPUFile Reading
*****************************************************************************}

function tppufile.open:boolean;
var
  ofmode : byte;
  i      : word;
begin
  open:=false;
  assign(f,fname);
  ofmode:=filemode;
  filemode:=$0;
  {$I-}
   reset(f,1);
  {$I+}
  filemode:=ofmode;
  if ioresult<>0 then
   exit;
{read ppuheader}
  fsize:=filesize(f);
  if fsize<sizeof(tppuheader) then
   exit;
  blockread(f,header,sizeof(tppuheader),i);
{reset buffer}
  bufstart:=i;
  bufsize:=0;
  bufidx:=0;
  Mode:=1;
  FillChar(entry,sizeof(tppuentry),0);
  entryidx:=0;
  entrystart:=0;
  entrybufstart:=0;
  Error:=false;
  open:=true;
end;


procedure tppufile.reloadbuf;
{$ifdef TP}
var
  i : word;
{$endif}
begin
  inc(bufstart,bufsize);
{$ifdef TP}
  blockread(f,buf^,ppubufsize,i);
  bufsize:=i;
{$else}
  blockread(f,buf^,ppubufsize,bufsize);
{$endif}
  bufidx:=0;
end;


procedure tppufile.readdata(var b;len:longint);
var
  p   : pchar;
  left,
  idx : longint;
begin
  p:=pchar(@b);
  idx:=0;
  while len>0 do
   begin
     left:=bufsize-bufidx;
     if len>left then
      begin
        move(buf[bufidx],p[idx],left);
        dec(len,left);
        inc(idx,left);
        reloadbuf;
        if bufsize=0 then
         exit;
      end
     else
      begin
        move(buf[bufidx],p[idx],len);
        inc(bufidx,len);
        exit;
      end;
   end;
end;


procedure tppufile.skipdata(len:longint);
var
  left : longint;
begin
  while len>0 do
   begin
     left:=bufsize-bufidx;
     if len>left then
      begin
        dec(len,left);
        reloadbuf;
        if bufsize=0 then
         exit;
      end
     else
      begin
        inc(bufidx,len);
        exit;
      end;
   end;
end;


function tppufile.readentry:byte;
begin
  if entryidx<entry.size then
   skipdata(entry.size-entryidx);
  readdata(entry,sizeof(tppuentry));
  entrystart:=bufstart+bufidx;
  entryidx:=0;
  if not(entry.id in [mainentryid,subentryid]) then
   begin
     readentry:=iberror;
     error:=true;
     exit;
   end;
  readentry:=entry.nr;
end;


function tppufile.endofentry:boolean;
begin
  endofentry:=(entryidx>=entry.size);
end;


procedure tppufile.getdatabuf(var b;len:longint;var result:longint);
begin
  if entryidx+len>entry.size then
   result:=entry.size-entryidx
  else
   result:=len;
  readdata(b,result);
  inc(entryidx,result);
end;


procedure tppufile.getdata(var b;len:longint);
begin
  if entryidx+len>entry.size then
   begin
     error:=true;
     exit;
   end;
  readdata(b,len);
  inc(entryidx,len);
end;


function tppufile.getbyte:byte;
var
  b : byte;
begin
  if entryidx+1>entry.size then
   begin
     error:=true;
     getbyte:=0;
     exit;
   end;
  readdata(b,1);
  getbyte:=b;
  inc(entryidx);
end;


function tppufile.getword:word;
type
  pword = ^word;
var
  w : word;
begin
  if entryidx+2>entry.size then
   begin
     error:=true;
     getword:=0;
     exit;
   end;
  readdata(w,2);
  if change_endian then
   getword:=swap(w)
  else

   getword:=w;
  inc(entryidx,2);
end;


function tppufile.getlongint:longint;
type
  plongint = ^longint;
var
  l : longint;
begin
  if entryidx+4>entry.size then
   begin
     error:=true;
     getlongint:=0;
     exit;
   end;
  readdata(l,4);
  if change_endian then
   getlongint:=swap(l shr 16) or (longint(swap(l and $ffff)) shl 16)
  else

   getlongint:=l;
  inc(entryidx,4);
end;


function tppufile.getreal:ppureal;
type
  pppureal = ^ppureal;
var
  d : ppureal;
begin
  if entryidx+sizeof(ppureal)>entry.size then
   begin
     error:=true;
     getreal:=0;
     exit;
   end;
  readdata(d,sizeof(ppureal));
  getreal:=d;
  inc(entryidx,sizeof(ppureal));
end;


function tppufile.getstring:string;
var
  s : string;
begin
  s[0]:=chr(getbyte);
  if entryidx+length(s)>entry.size then
   begin
     error:=true;
     exit;
   end;
  ReadData(s[1],length(s));
  getstring:=s;
  inc(entryidx,length(s));
end;


function tppufile.skipuntilentry(untilb:byte):boolean;
var
  b : byte;
begin
  repeat
    b:=readentry;
  until (b in [ibend,iberror]) or ((b=untilb) and (entry.id=mainentryid));
  skipuntilentry:=(b=untilb);
end;

{*****************************************************************************
                                TPPUFile Writing
*****************************************************************************}

function tppufile.create:boolean;
begin
  create:=false;
  assign(f,fname);
  {$I-}
   rewrite(f,1);
  {$I+}
  if ioresult<>0 then
   exit;
  Mode:=2;
{write header for sure}
  blockwrite(f,header,sizeof(tppuheader));
  bufsize:=ppubufsize;
  bufstart:=sizeof(tppuheader);
  bufidx:=0;
{reset}
  crc:=$ffffffff;
  Error:=false;
  do_crc:=true;
  size:=0;
  entrytyp:=mainentryid;
{start}
  NewEntry;
  create:=true;
end;


procedure tppufile.writeheader;
var
  opos : longint;
begin
{ flush buffer }
  writebuf;
{ update size (w/o header!) in the header }
  header.size:=bufstart-sizeof(tppuheader);
{ write header and restore filepos after it }
  opos:=filepos(f);
  seek(f,0);
  blockwrite(f,header,sizeof(tppuheader));
  seek(f,opos);
end;


procedure tppufile.writebuf;
begin
  blockwrite(f,buf^,bufidx);
  inc(bufstart,bufidx);
  bufidx:=0;
end;


procedure tppufile.writedata(var b;len:longint);
var
  p   : pchar;
  left,
  idx : longint;
begin
  p:=pchar(@b);
  idx:=0;
  while len>0 do
   begin
     left:=bufsize-bufidx;
     if len>left then
      begin
        move(p[idx],buf[bufidx],left);
        dec(len,left);
        inc(idx,left);
        inc(bufidx,left);
        writebuf;
      end
     else
      begin
        move(p[idx],buf[bufidx],len);
        inc(bufidx,len);
        exit;
      end;
   end;
end;


procedure tppufile.NewEntry;
begin
  with entry do
   begin
     id:=entrytyp;
     nr:=ibend;
     size:=0;
   end;
{Reset Entry State}
  entryidx:=0;
  entrybufstart:=bufstart;
  entrystart:=bufstart+bufidx;
{Alloc in buffer}
  writedata(entry,sizeof(tppuentry));
end;


procedure tppufile.writeentry(ibnr:byte);
var
  opos : longint;
begin
{create entry}
  entry.id:=entrytyp;
  entry.nr:=ibnr;
  entry.size:=entryidx;
{it's already been sent to disk ?}
  if entrybufstart<>bufstart then
   begin
   {flush to be sure}
     WriteBuf;
   {write entry}
     opos:=filepos(f);
     seek(f,entrystart);
     blockwrite(f,entry,sizeof(tppuentry));
     seek(f,opos);
     entrybufstart:=bufstart;
   end
  else
   move(entry,buf[entrystart-bufstart],sizeof(entry));
{Add New Entry, which is ibend by default}
  entrystart:=bufstart+bufidx; {next entry position}
  NewEntry;
end;


procedure tppufile.putdata(var b;len:longint);
begin
  if do_crc then
   crc:=UpdateCrc32(crc,b,len);
  writedata(b,len);
  inc(entryidx,len);
end;



procedure tppufile.putbyte(b:byte);
begin
  writedata(b,1);
  inc(entryidx);
end;


procedure tppufile.putword(w:word);
begin
  if change_endian then
   w:=swap(w);
  putdata(w,2);
end;


procedure tppufile.putlongint(l:longint);
begin
  if change_endian then
   l:=swap(l shr 16) or (longint(swap(l and $ffff)) shl 16);
  putdata(l,4);
end;


procedure tppufile.putreal(d:ppureal);
begin
  putdata(d,sizeof(ppureal));
end;


procedure tppufile.putstring(s:string);
begin
  putdata(s,length(s)+1);
end;


end.
{
  $Log$
  Revision 1.11  1998-09-11 15:16:47  peter
    * merge fixes

  Revision 1.10.2.1  1998/09/11 15:15:04  peter
    * fixed not in [] bug

  Revision 1.10  1998/08/31 12:26:30  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.9  1998/08/17 09:17:51  peter
    * static/shared linking updates

  Revision 1.8  1998/08/11 15:31:40  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.7  1998/06/25 10:51:01  pierre
    * removed a remaining ifndef NEWPPU
      replaced by ifdef OLDPPU
    * added uf_finalize to ppu unit

  Revision 1.6  1998/06/16 08:56:26  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.5  1998/06/13 00:10:12  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.4  1998/06/09 16:01:48  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.3  1998/05/28 14:40:26  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.2  1998/05/27 19:45:08  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

  Revision 1.1  1998/05/12 10:56:07  peter
    + the ppufile object unit

}

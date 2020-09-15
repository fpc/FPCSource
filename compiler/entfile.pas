{
    Copyright (c) 1998-2013 by Free Pascal development team

    Routines to read/write entry based files (ppu, pcp)

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
unit entfile;

{$i fpcdefs.inc}

interface

  uses
    systems,globtype,constexp,cstreams;

const
{ buffer sizes }
  maxentrysize = 1024;
  entryfilebufsize   = 16384;

{ppu entries}
  mainentryid         = 1;
  subentryid          = 2;
  {special}
  iberror             = 0;
  ibpputable          = 243;
  ibstartrequireds    = 244;
  ibendrequireds      = 245;
  ibstartcontained    = 246;
  ibendcontained      = 247;
  ibstartdefs         = 248;
  ibenddefs           = 249;
  ibstartsyms         = 250;
  ibendsyms           = 251;
  ibendinterface      = 252;
  ibendimplementation = 253;
//  ibendbrowser        = 254;
  ibend               = 255;
  {general}
  ibmodulename           = 1;
  ibsourcefiles          = 2;
  ibloadunit             = 3;
  ibinitunit             = 4;
  iblinkunitofiles       = 5;
  iblinkunitstaticlibs   = 6;
  iblinkunitsharedlibs   = 7;
  iblinkotherofiles      = 8;
  iblinkotherstaticlibs  = 9;
  iblinkothersharedlibs  = 10;
  ibImportSymbols        = 11;
  ibsymref               = 12;
  ibdefref               = 13;
  ibfeatures             = 14;
{$IFDEF MACRO_DIFF_HINT}
  ibusedmacros           = 16;
{$ENDIF}
  ibderefdata            = 17;
  ibexportedmacros       = 18;
  ibderefmap             = 19;

  {syms}
  ibtypesym        = 20;
  ibprocsym        = 21;
  ibstaticvarsym   = 22;
  ibconstsym       = 23;
  ibenumsym        = 24;
//  ibtypedconstsym  = 25;
  ibabsolutevarsym = 26;
  ibpropertysym    = 27;
  ibfieldvarsym    = 28;
  ibunitsym        = 29;
  iblabelsym       = 30;
  ibsyssym         = 31;
  ibnamespacesym   = 32;
  iblocalvarsym    = 33;
  ibparavarsym     = 34;
  ibmacrosym       = 35;
  {definitions}
  iborddef         = 40;
  ibpointerdef     = 41;
  ibarraydef       = 42;
  ibprocdef        = 43;
  ibshortstringdef = 44;
  ibrecorddef      = 45;
  ibfiledef        = 46;
  ibformaldef      = 47;
  ibobjectdef      = 48;
  ibenumdef        = 49;
  ibsetdef         = 50;
  ibprocvardef     = 51;
  ibfloatdef       = 52;
  ibclassrefdef    = 53;
  iblongstringdef  = 54;
  ibansistringdef  = 55;
  ibwidestringdef  = 56;
  ibvariantdef     = 57;
  ibundefineddef   = 58;
  ibunicodestringdef = 59;
  {implementation/ObjData}
  ibnodetree       = 80;
  ibasmsymbols     = 81;
  ibresources      = 82;
  ibcreatedobjtypes = 83;
  ibwpofile         = 84;
  ibmoduleoptions   = 85;
  ibunitimportsyms  = 86;
  iborderedsymbols  = 87;

  ibmainname       = 90;
  ibsymtableoptions = 91;
  ibpackagefiles   = 92;
  ibpackagename    = 93;
  ibrecsymtableoptions = 94;
  { target-specific things }
  iblinkotherframeworks = 100;
  ibjvmnamespace = 101;

{$ifdef generic_cpu}
{ We need to use the correct size of aint and pint for
  the target CPU }
const
  CpuAddrBitSize : array[tsystemcpu] of longint =
    (
    {  0 } 32 {'none'},
    {  1 } 32 {'i386'},
    {  2 } 32 {'m68k'},
    {  3 } 32 {'alpha'},
    {  4 } 32 {'powerpc'},
    {  5 } 32 {'sparc'},
    {  6 } 32 {'vis'},
    {  7 } 64 {'ia64'},
    {  8 } 64 {'x86_64'},
    {  9 } 32 {'mipseb'},
    { 10 } 32 {'arm'},
    { 11 } 64 {'powerpc64'},
    { 12 } 16 {'avr'},
    { 13 } 32 {'mipsel'},
    { 14 } 32 {'jvm'},
    { 15 } 16 {'i8086'},
    { 16 } 64 {'aarch64'},
    { 17 } 32 {'wasm'},
    { 18 } 64 {'sparc64'}
    );
  CpuAluBitSize : array[tsystemcpu] of longint =
    (
    {  0 } 32 {'none'},
    {  1 } 32 {'i386'},
    {  2 } 32 {'m68k'},
    {  3 } 32 {'alpha'},
    {  4 } 32 {'powerpc'},
    {  5 } 32 {'sparc'},
    {  6 } 32 {'vis'},
    {  7 } 64 {'ia64'},
    {  8 } 64 {'x86_64'},
    {  9 } 32 {'mipseb'},
    { 10 } 32 {'arm'},
    { 11 } 64 {'powerpc64'},
    { 12 }  8 {'avr'},
    { 13 } 32 {'mipsel'},
    { 14 } 64 {'jvm'},
    { 15 } 16 {'i8086'},
    { 16 } 64 {'aarch64'},
    { 17 } 64 {'wasm'},
    { 18 } 64 {'sparc64'}
    );
{$endif generic_cpu}

type
  { bestreal is defined based on the target architecture }
  entryreal=bestreal;



  { common part of the header for all kinds of entry files }
  tentryheader=record
    id       : array[1..3] of char;
    ver      : array[1..3] of char;
    compiler : word;
    cpu      : word;
    target   : word;
    flags    : dword;
    size     : dword; { size of the ppufile without header }
  end;
  pentryheader=^tentryheader;

  tentry=packed record
    size : longint;
    id   : byte;
    nr   : byte;
  end;

  tentryfile=class
  private
    function getposition:longint;
    procedure setposition(value:longint);
  protected
    buf      : pchar;
    bufstart,
    bufsize,
    bufidx   : integer;
    entrybufstart,
    entrystart,
    entryidx : integer;
    entry    : tentry;
    closed,
    tempclosed : boolean;
    closepos : integer;
  protected
    f        : TCStream;
    mode     : byte; {0 - Closed, 1 - Reading, 2 - Writing}
    fisfile  : boolean;
    fname    : string;
    fsize    : integer;
    procedure newheader;virtual;abstract;
    function readheader:longint;virtual;abstract;
    function outputallowed:boolean;virtual;
    procedure resetfile;virtual;abstract;
    function getheadersize:longint;virtual;abstract;
    function getheaderaddr:pentryheader;virtual;abstract;
    procedure RaiseAssertion(Code: Longint); virtual;
  public
    entrytyp : byte;
    size             : integer;
    change_endian    : boolean; { Used in ppudump util }
{$ifdef generic_cpu}
    has_more,
{$endif not generic_cpu}
    error         : boolean;
    constructor create(const fn:string);
    destructor  destroy;override;
    function getversion:integer;
    procedure flush;
    procedure closefile;virtual;
    procedure newentry;
    property position:longint read getposition write setposition;
    { Warning: don't keep the stream open during a tempclose! }
    function substream(ofs,len:longint):TCStream;
    { Warning: don't use the put* or write* functions anymore when writing through this }
    property stream:TCStream read f;
  {read}
    function  openfile:boolean;
    function  openstream(strm:TCStream):boolean;
    procedure reloadbuf;
    procedure readdata(out b;len:integer);
    procedure skipdata(len:integer);
    function  readentry:byte;
    function  EndOfEntry:boolean;
    function  entrysize:longint;
    function  entryleft:longint;
    procedure getdatabuf(out b;len:integer;out res:integer);
    procedure getdata(out b;len:integer);
    function  getbyte:byte;
    function  getword:word;
    function  getdword:dword;
    function  getlongint:longint;
    function getint64:int64;
    function  getqword:qword;
    function getaint:{$ifdef generic_cpu}int64{$else}aint{$endif};
    function getasizeint:{$ifdef generic_cpu}int64{$else}asizeint{$endif};
    function getpuint:{$ifdef generic_cpu}qword{$else}puint{$endif};
    function getptruint:{$ifdef generic_cpu}qword{$else}TConstPtrUInt{$endif};
    function getaword:{$ifdef generic_cpu}qword{$else}aword{$endif};
    function  getreal:entryreal;
    function  getrealsize(sizeofreal : longint):entryreal;
    function  getboolean:boolean;inline;
    function  getstring:string;
    function  getpshortstring:pshortstring;
    function  getansistring:ansistring;
    procedure getnormalset(out b);
    procedure getsmallset(out b);
    function  skipuntilentry(untilb:byte):boolean;
  {write}
    function  createfile:boolean;virtual;
    function  createstream(strm:TCStream):boolean;
    procedure writeheader;virtual;abstract;
    procedure writebuf;
    procedure writedata(const b;len:integer);
    procedure writeentry(ibnr:byte);
    procedure putdata(const b;len:integer);virtual;
    procedure putbyte(b:byte);
    procedure putword(w:word);
    procedure putdword(w:dword);
    procedure putlongint(l:longint);
    procedure putint64(i:int64);
    procedure putqword(q:qword);
    procedure putaint(i:aint);
    procedure putasizeint(i:asizeint);
    procedure putpuint(i:puint);
    procedure putptruint(v:TConstPtrUInt);
    procedure putaword(i:aword);
    procedure putreal(d:entryreal);
    procedure putboolean(b:boolean);inline;
    procedure putstring(const s:string);
    procedure putansistring(const s:ansistring);
    procedure putnormalset(const b);
    procedure putsmallset(const b);
    procedure tempclose;        // MG: not used, obsolete?
    function  tempopen:boolean; // MG: not used, obsolete?
  end;

implementation

  uses
    cutils;


function swapendian_entryreal(d:entryreal):entryreal;
type
  entryreal_bytes=array[0..sizeof(d)-1] of byte;
var
  i:0..sizeof(d)-1;
begin
  for i:=low(entryreal_bytes) to high(entryreal_bytes) do
    entryreal_bytes(result)[i]:=entryreal_bytes(d)[high(entryreal_bytes)-i];
end;

{*****************************************************************************
                              tentryfile
*****************************************************************************}

function tentryfile.outputallowed: boolean;
begin
  result:=true;
end;


constructor tentryfile.create(const fn:string);
begin
  fname:=fn;
  fisfile:=false;
  change_endian:=false;
  mode:=0;
  newheader;
  error:=false;
  closed:=true;
  tempclosed:=false;
  getmem(buf,entryfilebufsize);
end;


destructor tentryfile.destroy;
begin
  closefile;
  if assigned(buf) then
    freemem(buf,entryfilebufsize);
end;

function tentryfile.getversion:integer;
  var
    l    : integer;
    code : integer;
    header : pentryheader;
  begin
    header:=getheaderaddr;
    Val(header^.ver[1]+header^.ver[2]+header^.ver[3],l,code);
    if code=0 then
     result:=l
    else
     result:=0;
  end;

procedure tentryfile.flush;
begin
  if mode=2 then
   writebuf;
end;


procedure tentryfile.RaiseAssertion(Code: Longint);
begin
  { It's down to descendent classes to raise an internal error as desired. [Kit] }
  error := true;
end;


procedure tentryfile.closefile;
begin
  if mode<>0 then
   begin
     flush;
     if fisfile then
       f.Free;
     mode:=0;
     closed:=true;
   end;
end;


procedure tentryfile.setposition(value:longint);
begin
  if assigned(f) then
    f.Position:=value
  else
    if tempclosed then
      closepos:=value;
end;


function tentryfile.getposition:longint;
begin
  if assigned(f) then
    result:=f.Position
  else
    if tempclosed then
      result:=closepos
    else
      result:=0;
end;


function tentryfile.substream(ofs,len:longint):TCStream;
begin
  result:=nil;
  if assigned(f) then
    result:=TCRangeStream.Create(f,ofs,len);
end;


{*****************************************************************************
                              tentryfile Reading
*****************************************************************************}

function tentryfile.openfile:boolean;
var
  strm : TCStream;
begin
  openfile:=false;
  try
    strm:=CFileStreamClass.Create(fname,fmOpenRead)
  except
    exit;
  end;
  openfile:=openstream(strm);
  fisfile:=result;
end;


function tentryfile.openstream(strm:TCStream):boolean;
var
  i : longint;
begin
  openstream:=false;
  f:=strm;
  closed:=false;
{read ppuheader}
  fsize:=f.Size;
  i:=readheader;
  if i<0 then
    exit;
{reset buffer}
  bufstart:=i;
  bufsize:=0;
  bufidx:=0;
  mode:=1;
  FillChar(entry,sizeof(tentry),0);
  entryidx:=0;
  entrystart:=0;
  entrybufstart:=0;
  error:=false;
  openstream:=true;
end;


procedure tentryfile.reloadbuf;
begin
  inc(bufstart,bufsize);
  bufsize:=f.Read(buf^,entryfilebufsize);
  bufidx:=0;
end;


procedure tentryfile.readdata(out b;len:integer);
var
  p,pbuf : pchar;
  left : integer;
begin
  p:=pchar(@b);
  pbuf:=@buf[bufidx];
  repeat
    left:=bufsize-bufidx;
    if len<left then
      break;
    move(pbuf^,p^,left);
    dec(len,left);
    inc(p,left);
    reloadbuf;
    pbuf:=@buf[bufidx];
    if bufsize=0 then
      exit;
  until false;
  move(pbuf^,p^,len);
  inc(bufidx,len);
end;


procedure tentryfile.skipdata(len:integer);
var
  left : integer;
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


function tentryfile.readentry:byte;
begin
  if entryidx<entry.size then
    begin
{$ifdef generic_cpu}
     has_more:=true;
{$endif not generic_cpu}
     skipdata(entry.size-entryidx);
    end;
  readdata(entry,sizeof(tentry));
  if change_endian then
    entry.size:=swapendian(entry.size);
  entrystart:=bufstart+bufidx;
  entryidx:=0;
{$ifdef generic_cpu}
  has_more:=false;
{$endif not generic_cpu}
  if not(entry.id in [mainentryid,subentryid]) then
   begin
     readentry:=iberror;
     error:=true;
     exit;
   end;
  readentry:=entry.nr;
end;


function tentryfile.endofentry:boolean;
begin
{$ifdef generic_cpu}
  endofentry:=(entryidx=entry.size);
{$else not generic_cpu}
  endofentry:=(entryidx>=entry.size);
{$endif not generic_cpu}
end;


function tentryfile.entrysize:longint;
begin
  entrysize:=entry.size;
end;

function tentryfile.entryleft:longint;
begin
  entryleft:=entry.size-entryidx;
end;


procedure tentryfile.getdatabuf(out b;len:integer;out res:integer);
begin
  if entryidx+len>entry.size then
   res:=entry.size-entryidx
  else
   res:=len;
  readdata(b,res);
  inc(entryidx,res);
end;


procedure tentryfile.getdata(out b;len:integer);
begin
  if entryidx+len>entry.size then
   begin
     error:=true;
     exit;
   end;
  readdata(b,len);
  inc(entryidx,len);
end;


function tentryfile.getbyte:byte;
begin
  if entryidx+1>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
  if bufsize-bufidx>=1 then
    begin
      result:=pbyte(@buf[bufidx])^;
      inc(bufidx);
    end
  else
    readdata(result,1);
  inc(entryidx);
end;


function tentryfile.getword:word;
begin
  if entryidx+2>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
  if bufsize-bufidx>=sizeof(word) then
    begin
      result:=Unaligned(pword(@buf[bufidx])^);
      inc(bufidx,sizeof(word));
    end
  else
    readdata(result,sizeof(word));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,2);
end;


function tentryfile.getlongint:longint;
begin
  if entryidx+4>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
  if bufsize-bufidx>=sizeof(longint) then
    begin
      result:=Unaligned(plongint(@buf[bufidx])^);
      inc(bufidx,sizeof(longint));
    end
  else
    readdata(result,sizeof(longint));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,4);
end;


function tentryfile.getdword:dword;
begin
  if entryidx+4>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
  if bufsize-bufidx>=sizeof(dword) then
    begin
      result:=Unaligned(pdword(@buf[bufidx])^);
      inc(bufidx,sizeof(longint));
    end
  else
    readdata(result,sizeof(dword));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,4);
end;


function tentryfile.getint64:int64;
begin
  if entryidx+8>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
  if bufsize-bufidx>=sizeof(int64) then
    begin
      result:=Unaligned(pint64(@buf[bufidx])^);
      inc(bufidx,sizeof(int64));
    end
  else
    readdata(result,sizeof(int64));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,8);
end;


function tentryfile.getqword:qword;
begin
  if entryidx+8>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
  if bufsize-bufidx>=sizeof(qword) then
    begin
      result:=Unaligned(pqword(@buf[bufidx])^);
      inc(bufidx,sizeof(qword));
    end
  else
    readdata(result,sizeof(qword));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,8);
end;


function tentryfile.getaint:{$ifdef generic_cpu}int64{$else}aint{$endif};
{$ifdef generic_cpu}
var
  header : pentryheader;
{$endif generic_cpu}
begin
{$ifdef generic_cpu}
  header:=getheaderaddr;
  if CpuAluBitSize[tsystemcpu(header^.cpu)]=64 then
    result:=getint64
  else if CpuAluBitSize[tsystemcpu(header^.cpu)]=32 then
    result:=getlongint
  else if CpuAluBitSize[tsystemcpu(header^.cpu)]=16 then
    result:=smallint(getword)
  else if CpuAluBitSize[tsystemcpu(header^.cpu)]=8 then
    result:=shortint(getbyte)
  else
    begin
      error:=true;
      result:=0;
    end;
{$else not generic_cpu}
  case sizeof(aint) of
    8: result:=getint64;
    4: result:=getlongint;
    2: result:=smallint(getword);
    1: result:=shortint(getbyte);
  else
    begin
      RaiseAssertion(2019041801);
      result:=0;
    end;
  end;
{$endif not generic_cpu}
end;


function tentryfile.getasizeint:{$ifdef generic_cpu}int64{$else}asizeint{$endif};
{$ifdef generic_cpu}
var
  header : pentryheader;
{$endif generic_cpu}
begin
{$ifdef generic_cpu}
  header:=getheaderaddr;
  if CpuAddrBitSize[tsystemcpu(header^.cpu)]=64 then
    result:=getint64
  else if CpuAddrBitSize[tsystemcpu(header^.cpu)]=32 then
    result:=getlongint
  else if CpuAddrBitSize[tsystemcpu(header^.cpu)]=16 then
    begin
      { result:=smallint(getword);
        would have been logical, but it contradicts
	definition of asizeint in globtype unit,
	which uses 32-bit lngint type even for 16-bit
	address size, to be able to cope with
	I8086 seg:ofs huge addresses }
      result:=getlongint;
    end
  else
    begin
      error:=true;
      result:=0;
    end;
{$else not generic_cpu}
  case sizeof(asizeint) of
    8: result:=asizeint(getint64);
    4: result:=asizeint(getlongint);
    2: result:=asizeint(getword);
    1: result:=asizeint(getbyte);
  else
    begin
      RaiseAssertion(2019041802);
      result:=0;
    end;
  end;
{$endif not generic_cpu}
end;


function tentryfile.getpuint:{$ifdef generic_cpu}qword{$else}puint{$endif};
{$ifdef generic_cpu}
var
header : pentryheader;
{$endif generic_cpu}
begin
{$ifdef generic_cpu}
  header:=getheaderaddr;
  if CpuAddrBitSize[tsystemcpu(header^.cpu)]=64 then
    result:=getqword
  else if CpuAddrBitSize[tsystemcpu(header^.cpu)]=32 then
    result:=getdword
  else if CpuAddrBitSize[tsystemcpu(header^.cpu)]=16 then
    result:=getword
  else
    begin
      error:=true;
      result:=0;
    end;
{$else not generic_cpu}
  case sizeof(puint) of
    8: result:=getqword;
    4: result:=getdword;
    2: result:=getword;
    1: result:=getbyte;
  else
    begin
      RaiseAssertion(2019041803);
      result:=0;
    end;
  end;
{$endif not generic_cpu}
end;


function tentryfile.getptruint:{$ifdef generic_cpu}qword{$else}TConstPtrUInt{$endif};
{$ifdef generic_cpu}
var
header : pentryheader;
{$endif generic_cpu}
begin
{$ifdef generic_cpu}
  header:=getheaderaddr;
  if CpuAddrBitSize[tsystemcpu(header^.cpu)]=64 then
    result:=getqword
  else result:=getdword;
{$else not generic_cpu}
  {$if sizeof(TConstPtrUInt)=8}
  result:=tconstptruint(getint64);
  {$else}
  result:=TConstPtrUInt(getlongint);
  {$endif}
{$endif not generic_cpu}
end;


function tentryfile.getaword:{$ifdef generic_cpu}qword{$else}aword{$endif};
{$ifdef generic_cpu}
var
header : pentryheader;
{$endif generic_cpu}
begin
{$ifdef generic_cpu}
  header:=getheaderaddr;
  if CpuAluBitSize[tsystemcpu(header^.cpu)]=64 then
    result:=getqword
  else if CpuAluBitSize[tsystemcpu(header^.cpu)]=32 then
    result:=getdword
  else if CpuAluBitSize[tsystemcpu(header^.cpu)]=16 then
    result:=getword
  else if CpuAluBitSize[tsystemcpu(header^.cpu)]=8 then
    result:=getbyte
  else
    begin
      error:=true;
      result:=0;
    end;
{$else not generic_cpu}
  case sizeof(aword) of
    8: result:=getqword;
    4: result:=getdword;
    2: result:=getword;
    1: result:=getbyte;
  else
    begin
      RaiseAssertion(2019041804);
      result:=0;
    end;
  end;
{$endif not generic_cpu}
end;

function tentryfile.getrealsize(sizeofreal : longint):entryreal;
var
  e : entryreal;
  d : double;
  s : single;
begin
  if sizeofreal=sizeof(e) then
    begin
      if entryidx+sizeof(e)>entry.size then
       begin
         error:=true;
         result:=0;
         exit;
       end;
      readdata(e,sizeof(e));
      if change_endian then
        result:=swapendian_entryreal(e)
      else
        result:=e;
      inc(entryidx,sizeof(e));
      exit;
    end;
  if sizeofreal=sizeof(d) then
    begin
      if entryidx+sizeof(d)>entry.size then
       begin
         error:=true;
         result:=0;
         exit;
       end;
      readdata(d,sizeof(d));
      if change_endian then
        result:=swapendian(pqword(@d)^)
      else
        result:=d;
      inc(entryidx,sizeof(d));
      result:=d;
      exit;
    end;
  if sizeofreal=sizeof(s) then
    begin
      if entryidx+sizeof(s)>entry.size then
       begin
         error:=true;
         result:=0;
         exit;
       end;
      readdata(s,sizeof(s));
      if change_endian then
        result:=swapendian(pdword(@s)^)
      else
        result:=s;
      inc(entryidx,sizeof(s));
      result:=s;
      exit;
    end;
  error:=true;
  result:=0.0;
end;


function tentryfile.getreal:entryreal;
var
  d : entryreal;
  hd : double;
begin
  if target_info.system=system_x86_64_win64 then
    begin
      hd:=getrealsize(sizeof(hd));
      getreal:=hd;
    end
  else
    begin
      d:=getrealsize(sizeof(d));
      getreal:=d;
    end;
end;


function tentryfile.getboolean:boolean;
begin
  result:=boolean(getbyte);
end;


function tentryfile.getstring:string;
begin
  result[0]:=chr(getbyte);
  if entryidx+length(result)>entry.size then
   begin
     error:=true;
     exit;
   end;
  ReadData(result[1],length(result));
  inc(entryidx,length(result));
end;

function tentryfile.getpshortstring:pshortstring;
var
  len: char;
begin
  result:=nil;
  len:=chr(getbyte);
  if entryidx+ord(len)>entry.size then
   begin
     error:=true;
     exit;
   end;
  getmem(result,ord(len)+1);
  result^[0]:=len;
  ReadData(result^[1],ord(len));
  inc(entryidx,ord(len));
end;

function tentryfile.getansistring:ansistring;
var
  len: longint;
begin
  len:=getlongint;
  if entryidx+len>entry.size then
   begin
     error:=true;
     result:='';
     exit;
   end;
  setlength(result,len);
  if len>0 then
    getdata(result[1],len);
end;


procedure tentryfile.getsmallset(out b);
var
  i : longint;
begin
  getdata(b,4);
  if change_endian then
    for i:=0 to 3 do
      Pbyte(@b)[i]:=reverse_byte(Pbyte(@b)[i]);
end;


procedure tentryfile.getnormalset(out b);
var
  i : longint;
begin
  getdata(b,32);
  if change_endian then
    for i:=0 to 31 do
      Pbyte(@b)[i]:=reverse_byte(Pbyte(@b)[i]);
end;


function tentryfile.skipuntilentry(untilb:byte):boolean;
var
  b : byte;
begin
  repeat
    b:=readentry;
  until (b in [ibend,iberror]) or ((b=untilb) and (entry.id=mainentryid));
  skipuntilentry:=(b=untilb);
end;


{*****************************************************************************
                              tentryfile Writing
*****************************************************************************}

function tentryfile.createfile:boolean;
var
  ok: boolean;
  strm : TCStream;
begin
  createfile:=false;
  strm:=nil;
  if outputallowed then
    begin
      {$ifdef MACOS}
      {FPas is FreePascal's creator code on MacOS. See systems/mac_crea.txt}
      SetDefaultMacOSCreator('FPas');
      SetDefaultMacOSFiletype('FPPU');
      {$endif}
      ok:=false;
      try
        strm:=CFileStreamClass.Create(fname,fmCreate);
        ok:=true;
      except
      end;
      {$ifdef MACOS}
      SetDefaultMacOSCreator('MPS ');
      SetDefaultMacOSFiletype('TEXT');
      {$endif}
      if not ok then
       exit;
    end;
  createfile:=createstream(strm);
  fisfile:=result;
end;

function tentryfile.createstream(strm:TCStream):boolean;
begin
  createstream:=false;
  if outputallowed then
    begin
      f:=strm;
      mode:=2;
      {write header for sure}
      f.Write(getheaderaddr^,getheadersize);
    end;
  bufsize:=entryfilebufsize;
  bufstart:=getheadersize;
  bufidx:=0;
{reset}
  resetfile;
  error:=false;
  size:=0;
  entrytyp:=mainentryid;
{start}
  newentry;
  createstream:=true;
end;


procedure tentryfile.writebuf;
begin
  if outputallowed and
     (bufidx <> 0) then
    f.Write(buf^,bufidx);
  inc(bufstart,bufidx);
  bufidx:=0;
end;


procedure tentryfile.writedata(const b;len:integer);
var
  p   : pchar;
  left,
  idx : integer;
begin
  if not outputallowed then
    exit;
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


procedure tentryfile.newentry;
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
  writedata(entry,sizeof(tentry));
end;


procedure tentryfile.writeentry(ibnr:byte);
var
  opos : integer;
begin
{create entry}
  entry.id:=entrytyp;
  entry.nr:=ibnr;
  entry.size:=entryidx;
{it's already been sent to disk ?}
  if entrybufstart<>bufstart then
   begin
    if outputallowed then
      begin
      {flush to be sure}
        WriteBuf;
      {write entry}
        opos:=f.Position;
        f.Position:=entrystart;
        f.write(entry,sizeof(tentry));
        f.Position:=opos;
      end;
     entrybufstart:=bufstart;
   end
  else
   move(entry,buf[entrystart-bufstart],sizeof(entry));
{Add New Entry, which is ibend by default}
  entrystart:=bufstart+bufidx; {next entry position}
  newentry;
end;


procedure tentryfile.putdata(const b;len:integer);
begin
  if outputallowed then
    writedata(b,len);
  inc(entryidx,len);
end;


procedure tentryfile.putbyte(b:byte);
begin
  putdata(b,1);
end;


procedure tentryfile.putword(w:word);
begin
  putdata(w,2);
end;


procedure tentryfile.putdword(w:dword);
begin
  putdata(w,4);
end;


procedure tentryfile.putlongint(l:longint);
begin
  putdata(l,4);
end;


procedure tentryfile.putint64(i:int64);
begin
  putdata(i,8);
end;


procedure tentryfile.putqword(q:qword);
begin
  putdata(q,sizeof(qword));
end;


procedure tentryfile.putaint(i:aint);
begin
  putdata(i,sizeof(aint));
end;


procedure tentryfile.putasizeint(i: asizeint);
begin
  putdata(i,sizeof(asizeint));
end;


procedure tentryfile.putpuint(i : puint);
begin
  putdata(i,sizeof(puint));
end;

procedure tentryfile.putptruint(v:TConstPtrUInt);
begin
  {$if sizeof(TConstPtrUInt)=8}
  putint64(int64(v));
  {$else}
  putlongint(longint(v));
  {$endif}
end;




procedure tentryfile.putaword(i:aword);
begin
  putdata(i,sizeof(aword));
end;


procedure tentryfile.putreal(d:entryreal);
var
  hd : double;
begin
  if target_info.system=system_x86_64_win64 then
    begin
      hd:=d;
      putdata(hd,sizeof(hd));
    end
  else
    putdata(d,sizeof(entryreal));
end;


procedure tentryfile.putboolean(b:boolean);
begin
  putbyte(byte(b));
end;


procedure tentryfile.putstring(const s:string);
  begin
    putdata(s,length(s)+1);
  end;


procedure tentryfile.putansistring(const s:ansistring);
  var
    len: longint;
  begin
    len:=length(s);
    putlongint(len);
    if len>0 then
      putdata(s[1],len);
  end;


procedure tentryfile.putsmallset(const b);
  var
    l : longint;
  begin
    l:=longint(b);
    putlongint(l);
  end;


procedure tentryfile.putnormalset(const b);
  begin
    putdata(b,32);
  end;


procedure tentryfile.tempclose;
  begin
    if not closed then
     begin
       closepos:=f.Position;
       f.Free;
       f:=nil;
       closed:=true;
       tempclosed:=true;
     end;
  end;


function tentryfile.tempopen:boolean;
  begin
    tempopen:=false;
    if not closed or not tempclosed then
     exit;
   { MG: not sure, if this is correct
     f.position:=0;
       No, f was freed in tempclose above, we need to
       recreate it.  PM 2011/06/06 }
    try
      f:=CFileStreamClass.Create(fname,fmOpenRead);
    except
      exit;
    end;
    closed:=false;
    tempclosed:=false;

  { restore state }
    f.Position:=closepos;
    tempopen:=true;
  end;

end.

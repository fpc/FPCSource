{
    Copyright (c) 1998-2002 by Florian Klaempfl

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
unit ppu;

{$i fpcdefs.inc}

interface

  uses
    globtype,constexp;

{ Also write the ppu if only crc if done, this can be used with ppudump to
  see the differences between the intf and implementation }
{ define INTFPPU}

{$ifdef Test_Double_checksum}
var
  CRCFile : text;
const
  CRC_array_Size = 200000;
type
  tcrc_array = array[0..crc_array_size] of longint;
  pcrc_array = ^tcrc_array;
{$endif Test_Double_checksum}

const
  CurrentPPUVersion = 94;

{ buffer sizes }
  maxentrysize = 1024;
  ppubufsize   = 16384;

{ppu entries}
  mainentryid         = 1;
  subentryid          = 2;
  {special}
  iberror             = 0;
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
//  ibendsymtablebrowser   = 14;
//  ibbeginsymtablebrowser = 15;
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
//  ibrttisym        = 32;
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

  ibmainname       = 90;
  { target-specific things }
  iblinkotherframeworks = 100;

{ unit flags }
  uf_init          = $1;
  uf_finalize      = $2;
  uf_big_endian    = $4;
//  uf_has_browser   = $10;
  uf_in_library    = $20;     { is the file in another file than <ppufile>.* ? }
  uf_smart_linked  = $40;     { the ppu can be smartlinked }
  uf_static_linked = $80;     { the ppu can be linked static }
  uf_shared_linked = $100;    { the ppu can be linked shared }
//  uf_local_browser = $200;
  uf_no_link       = $400;    { unit has no .o generated, but can still have
                                external linking! }
  uf_has_resourcestrings = $800;    { unit has resource string section }
  uf_little_endian = $1000;
  uf_release       = $2000;   { unit was compiled with -Ur option }
  uf_threadvars    = $4000;   { unit has threadvars }
  uf_fpu_emulation = $8000;   { this unit was compiled with fpu emulation on }
  uf_has_debuginfo = $10000;  { this unit has debuginfo generated }
  uf_local_symtable = $20000; { this unit has a local symtable stored }
  uf_uses_variants  = $40000; { this unit uses variants }
  uf_has_resourcefiles = $80000; { this unit has external resources (using $R directive)}
  uf_has_exports = $100000;   { this module or a used unit has exports }


type
  { bestreal is defined based on the target architecture }
  ppureal=bestreal;

  tppuerror=(ppuentrytoobig,ppuentryerror);

  tppuheader=record
    id       : array[1..3] of char; { = 'PPU' }
    ver      : array[1..3] of char;
    compiler : word;
    cpu      : word;
    target   : word;
    flags    : longint;
    size     : longint; { size of the ppufile without header }
    checksum : cardinal; { checksum for this ppufile }
    interface_checksum : cardinal;
    deflistsize,
    symlistsize : longint;
    future   : array[0..0] of longint;
  end;

  tppuentry=packed record
    size : longint;
    id   : byte;
    nr   : byte;
  end;

  tppufile=class
  private
    f        : file;
    mode     : byte; {0 - Closed, 1 - Reading, 2 - Writing}
    fname    : string;
    fsize    : integer;
{$ifdef Test_Double_checksum}
  public
    crcindex,
    crc_index,
    crcindex2,
    crc_index2 : cardinal;
    crc_test,
    crc_test2  : pcrc_array;
  private
{$endif def Test_Double_checksum}
    change_endian : boolean;
    buf      : pchar;
    bufstart,
    bufsize,
    bufidx   : integer;
    entrybufstart,
    entrystart,
    entryidx : integer;
    entry    : tppuentry;
    closed,
    tempclosed : boolean;
    closepos : integer;
  public
    entrytyp : byte;
    header           : tppuheader;
    size             : integer;
    crc,
    interface_crc    : cardinal;
    error,
    do_crc,
    do_interface_crc : boolean;
    crc_only         : boolean;    { used to calculate interface_crc before implementation }
    constructor Create(const fn:string);
    destructor  Destroy;override;
    procedure flush;
    procedure closefile;
    function  CheckPPUId:boolean;
    function  GetPPUVersion:integer;
    procedure NewHeader;
    procedure NewEntry;
  {read}
    function  openfile:boolean;
    procedure reloadbuf;
    procedure readdata(var b;len:integer);
    procedure skipdata(len:integer);
    function  readentry:byte;
    function  EndOfEntry:boolean;
    function  entrysize:longint;
    procedure getdatabuf(var b;len:integer;var res:integer);
    procedure getdata(var b;len:integer);
    function  getbyte:byte;
    function  getword:word;
    function  getlongint:longint;
    function getint64:int64;
    function getaint:aint;
    function  getreal:ppureal;
    function  getstring:string;
    procedure getnormalset(var b);
    procedure getsmallset(var b);
    function  skipuntilentry(untilb:byte):boolean;
  {write}
    function  createfile:boolean;
    procedure writeheader;
    procedure writebuf;
    procedure writedata(const b;len:integer);
    procedure writeentry(ibnr:byte);
    procedure putdata(const b;len:integer);
    procedure putbyte(b:byte);
    procedure putword(w:word);
    procedure putlongint(l:longint);
    procedure putint64(i:int64);
    procedure putaint(i:aint);
    procedure putreal(d:ppureal);
    procedure putstring(const s:string);
    procedure putnormalset(const b);
    procedure putsmallset(const b);
    procedure tempclose;
    function  tempopen:boolean;
  end;

implementation

  uses
    systems,
{$ifdef Test_Double_checksum}
    comphook,
{$endif def Test_Double_checksum}
    fpccrc,
    cutils;


function swapendian_ppureal(d:ppureal):ppureal;

type ppureal_bytes=array[0..sizeof(d)-1] of byte;

var i:0..sizeof(d)-1;

begin
  for i:=low(ppureal_bytes) to high(ppureal_bytes) do
    ppureal_bytes(swapendian_ppureal)[i]:=ppureal_bytes(d)[high(ppureal_bytes)-i];
end;


{*****************************************************************************
                                  TPPUFile
*****************************************************************************}

constructor tppufile.Create(const fn:string);
begin
  fname:=fn;
  change_endian:=false;
  crc_only:=false;
  Mode:=0;
  NewHeader;
  Error:=false;
  closed:=true;
  tempclosed:=false;
  getmem(buf,ppubufsize);
end;


destructor tppufile.destroy;
begin
  closefile;
  if assigned(buf) then
    freemem(buf,ppubufsize);
end;


procedure tppufile.flush;
begin
  if Mode=2 then
   writebuf;
end;


procedure tppufile.closefile;
begin
{$ifdef Test_Double_checksum}
  if mode=2 then
   begin
     if assigned(crc_test) then
      dispose(crc_test);
     if assigned(crc_test2) then
      dispose(crc_test2);
   end;
{$endif Test_Double_checksum}
  if Mode<>0 then
   begin
     Flush;
     {$I-}
      system.close(f);
     {$I+}
     if ioresult<>0 then;
     Mode:=0;
     closed:=true;
   end;
end;


function tppufile.CheckPPUId:boolean;
begin
  CheckPPUId:=((Header.Id[1]='P') and (Header.Id[2]='P') and (Header.Id[3]='U'));
end;


function tppufile.GetPPUVersion:integer;
var
  l    : integer;
  code : integer;
begin
  Val(header.ver[1]+header.ver[2]+header.ver[3],l,code);
  if code=0 then
   GetPPUVersion:=l
  else
   GetPPUVersion:=0;
end;


procedure tppufile.NewHeader;
var
  s : string;
begin
  fillchar(header,sizeof(tppuheader),0);
  str(currentppuversion,s);
  while length(s)<3 do
   s:='0'+s;
  with header do
   begin
     Id[1]:='P';
     Id[2]:='P';
     Id[3]:='U';
     Ver[1]:=s[1];
     Ver[2]:=s[2];
     Ver[3]:=s[3];
   end;
end;


{*****************************************************************************
                                TPPUFile Reading
*****************************************************************************}

function tppufile.openfile:boolean;
var
  ofmode : byte;
  i      : integer;
begin
  openfile:=false;
  assign(f,fname);
  ofmode:=filemode;
  filemode:=$0;
  {$I-}
   reset(f,1);
  {$I+}
  filemode:=ofmode;
  if ioresult<>0 then
   exit;
  closed:=false;
{read ppuheader}
  fsize:=filesize(f);
  if fsize<sizeof(tppuheader) then
   exit;
  blockread(f,header,sizeof(tppuheader),i);
  { The header is always stored in little endian order }
  { therefore swap if on a big endian machine          }
{$IFDEF ENDIAN_BIG}
  header.compiler := swapendian(header.compiler);
  header.cpu := swapendian(header.cpu);
  header.target := swapendian(header.target);
  header.flags := swapendian(header.flags);
  header.size := swapendian(header.size);
  header.checksum := swapendian(header.checksum);
  header.interface_checksum := swapendian(header.interface_checksum);
  header.deflistsize:=swapendian(header.deflistsize);
  header.symlistsize:=swapendian(header.symlistsize);
{$ENDIF}
  { the PPU DATA is stored in native order }
  if (header.flags and uf_big_endian) = uf_big_endian then
   Begin
{$IFDEF ENDIAN_LITTLE}
     change_endian := TRUE;
{$ELSE}
     change_endian := FALSE;
{$ENDIF}
   End
  else if (header.flags and uf_little_endian) = uf_little_endian then
   Begin
{$IFDEF ENDIAN_BIG}
     change_endian := TRUE;
{$ELSE}
     change_endian := FALSE;
{$ENDIF}
   End;
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
  openfile:=true;
end;


procedure tppufile.reloadbuf;
begin
  inc(bufstart,bufsize);
  blockread(f,buf^,ppubufsize,bufsize);
  bufidx:=0;
end;


procedure tppufile.readdata(var b;len:integer);
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


procedure tppufile.skipdata(len:integer);
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


function tppufile.readentry:byte;
begin
  if entryidx<entry.size then
   skipdata(entry.size-entryidx);
  readdata(entry,sizeof(tppuentry));
  if change_endian then
    entry.size:=swapendian(entry.size);
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


function tppufile.entrysize:longint;
begin
  entrysize:=entry.size;
end;


procedure tppufile.getdatabuf(var b;len:integer;var res:integer);
begin
  if entryidx+len>entry.size then
   res:=entry.size-entryidx
  else
   res:=len;
  readdata(b,res);
  inc(entryidx,res);
end;


procedure tppufile.getdata(var b;len:integer);
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


function tppufile.getword:word;
begin
  if entryidx+2>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
{$ifdef FPC_UNALIGNED_FIXED}
  if bufsize-bufidx>=sizeof(word) then
    begin
      result:=Unaligned(pword(@buf[bufidx])^);
      inc(bufidx,sizeof(word));
    end
  else
{$endif FPC_UNALIGNED_FIXED}
    readdata(result,sizeof(word));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,2);
end;


function tppufile.getlongint:longint;
begin
  if entryidx+4>entry.size then
   begin
     error:=true;
     getlongint:=0;
     exit;
   end;
{$ifdef FPC_UNALIGNED_FIXED}
  if bufsize-bufidx>=sizeof(longint) then
    begin
      result:=Unaligned(plongint(@buf[bufidx])^);
      inc(bufidx,sizeof(longint));
    end
  else
{$endif FPC_UNALIGNED_FIXED}
    readdata(result,sizeof(longint));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,4);
end;


function tppufile.getint64:int64;
begin
  if entryidx+8>entry.size then
   begin
     error:=true;
     result:=0;
     exit;
   end;
{$ifdef FPC_UNALIGNED_FIXED}
  if bufsize-bufidx>=sizeof(int64) then
    begin
      result:=Unaligned(pint64(@buf[bufidx])^);
      inc(bufidx,sizeof(int64));
    end
  else
{$endif FPC_UNALIGNED_FIXED}
    readdata(result,sizeof(int64));
  if change_endian then
   result:=swapendian(result);
  inc(entryidx,8);
end;


function tppufile.getaint:aint;
begin
{$ifdef cpu64bitalu}
  result:=getint64;
{$else cpu64bitalu}
  result:=getlongint;
{$endif cpu64bitalu}
end;


function tppufile.getreal:ppureal;
var
  d : ppureal;
  hd : double;
begin
  if target_info.system=system_x86_64_win64 then
    begin
      if entryidx+sizeof(hd)>entry.size then
       begin
         error:=true;
         getreal:=0;
         exit;
       end;
      readdata(hd,sizeof(hd));
      if change_endian then
        getreal:=swapendian(qword(hd))
      else
        getreal:=hd;
      inc(entryidx,sizeof(hd));
    end
  else
    begin
      if entryidx+sizeof(ppureal)>entry.size then
       begin
         error:=true;
         getreal:=0;
         exit;
       end;
      readdata(d,sizeof(ppureal));
      if change_endian then
        getreal:=swapendian_ppureal(d)
      else
        getreal:=d;
      inc(entryidx,sizeof(ppureal));
    end;
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


procedure tppufile.getsmallset(var b);
var
  i : longint;
begin
  getdata(b,4);
  if change_endian then
    for i:=0 to 3 do
      Pbyte(@b)[i]:=reverse_byte(Pbyte(@b)[i]);
end;


procedure tppufile.getnormalset(var b);
var
  i : longint;
begin
  getdata(b,32);
  if change_endian then
    for i:=0 to 31 do
      Pbyte(@b)[i]:=reverse_byte(Pbyte(@b)[i]);
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

function tppufile.createfile:boolean;
begin
  createfile:=false;
{$ifdef INTFPPU}
  if crc_only then
   begin
     fname:=fname+'.intf';
     crc_only:=false;
   end;
{$endif}
  if not crc_only then
    begin
      assign(f,fname);
      {$ifdef MACOS}
      {FPas is FreePascal's creator code on MacOS. See systems/mac_crea.txt}
      SetDefaultMacOSCreator('FPas');
      SetDefaultMacOSFiletype('FPPU');
      {$endif}
      {$I-}
      rewrite(f,1);
      {$I+}
      {$ifdef MACOS}
      SetDefaultMacOSCreator('MPS ');
      SetDefaultMacOSFiletype('TEXT');
      {$endif}
      if ioresult<>0 then
       exit;
      Mode:=2;
    {write header for sure}
      blockwrite(f,header,sizeof(tppuheader));
    end;
  bufsize:=ppubufsize;
  bufstart:=sizeof(tppuheader);
  bufidx:=0;
{reset}
  crc:=0;
  interface_crc:=0;
  do_interface_crc:=true;
  Error:=false;
  do_crc:=true;
  size:=0;
  entrytyp:=mainentryid;
{start}
  NewEntry;
  createfile:=true;
end;


procedure tppufile.writeheader;
var
  opos : integer;
begin
  if crc_only then
   exit;
  { flush buffer }
  writebuf;
  { update size (w/o header!) in the header }
  header.size:=bufstart-sizeof(tppuheader);
  { set the endian flag }
{$ifndef FPC_BIG_ENDIAN}
    header.flags := header.flags or uf_little_endian;
{$else not FPC_BIG_ENDIAN}
    header.flags := header.flags or uf_big_endian;
    { Now swap the header in the correct endian (always little endian) }
    header.compiler := swapendian(header.compiler);
    header.cpu := swapendian(header.cpu);
    header.target := swapendian(header.target);
    header.flags := swapendian(header.flags);
    header.size := swapendian(header.size);
    header.checksum := swapendian(header.checksum);
    header.interface_checksum := swapendian(header.interface_checksum);
    header.deflistsize:=swapendian(header.deflistsize);
    header.symlistsize:=swapendian(header.symlistsize);
{$endif not FPC_BIG_ENDIAN}
{ write header and restore filepos after it }
  opos:=filepos(f);
  seek(f,0);
  blockwrite(f,header,sizeof(tppuheader));
  seek(f,opos);
end;


procedure tppufile.writebuf;
begin
  if not crc_only and
     (bufidx <> 0) then
    blockwrite(f,buf^,bufidx);
  inc(bufstart,bufidx);
  bufidx:=0;
end;


procedure tppufile.writedata(const b;len:integer);
var
  p   : pchar;
  left,
  idx : integer;
begin
  if crc_only then
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
  opos : integer;
begin
{create entry}
  entry.id:=entrytyp;
  entry.nr:=ibnr;
  entry.size:=entryidx;
{it's already been sent to disk ?}
  if entrybufstart<>bufstart then
   begin
    if not crc_only then
      begin
      {flush to be sure}
        WriteBuf;
      {write entry}
        opos:=filepos(f);
        seek(f,entrystart);
        blockwrite(f,entry,sizeof(tppuentry));
        seek(f,opos);
      end;
     entrybufstart:=bufstart;
   end
  else
   move(entry,buf[entrystart-bufstart],sizeof(entry));
{Add New Entry, which is ibend by default}
  entrystart:=bufstart+bufidx; {next entry position}
  NewEntry;
end;


procedure tppufile.putdata(const b;len:integer);
begin
  if do_crc then
   begin
     crc:=UpdateCrc32(crc,b,len);
{$ifdef Test_Double_checksum}
     if crc_only then
       begin
         crc_test2^[crc_index2]:=crc;
{$ifdef Test_Double_checksum_write}
         Writeln(CRCFile,crc);
{$endif Test_Double_checksum_write}
         if crc_index2<crc_array_size then
          inc(crc_index2);
       end
     else
       begin
         if (crcindex2<crc_array_size) and (crcindex2<crc_index2) and
            (crc_test2^[crcindex2]<>crc) then
           Do_comment(V_Note,'impl CRC changed');
{$ifdef Test_Double_checksum_write}
         Writeln(CRCFile,crc);
{$endif Test_Double_checksum_write}
         inc(crcindex2);
       end;
{$endif def Test_Double_checksum}
     if do_interface_crc then
       begin
         interface_crc:=UpdateCrc32(interface_crc,b,len);
{$ifdef Test_Double_checksum}
        if crc_only then
          begin
            crc_test^[crc_index]:=interface_crc;
{$ifdef Test_Double_checksum_write}
            Writeln(CRCFile,interface_crc);
{$endif Test_Double_checksum_write}
            if crc_index<crc_array_size then
             inc(crc_index);
          end
        else
          begin
            if (crcindex<crc_array_size) and (crcindex<crc_index) and
               (crc_test^[crcindex]<>interface_crc) then
              Do_comment(V_Warning,'CRC changed');
{$ifdef Test_Double_checksum_write}
            Writeln(CRCFile,interface_crc);
{$endif Test_Double_checksum_write}
            inc(crcindex);
          end;
{$endif def Test_Double_checksum}
       end;
    end;
  if not crc_only then
    writedata(b,len);
  inc(entryidx,len);
end;


procedure tppufile.putbyte(b:byte);
begin
  putdata(b,1);
end;


procedure tppufile.putword(w:word);
begin
  putdata(w,2);
end;


procedure tppufile.putlongint(l:longint);
begin
  putdata(l,4);
end;


procedure tppufile.putint64(i:int64);
begin
  putdata(i,8);
end;


procedure tppufile.putaint(i:aint);
begin
  putdata(i,sizeof(aint));
end;

procedure tppufile.putreal(d:ppureal);
var
  hd : double;
begin
  if target_info.system=system_x86_64_win64 then
    begin
      hd:=d;
      putdata(hd,sizeof(hd));
    end
  else
    putdata(d,sizeof(ppureal));
end;


procedure tppufile.putstring(const s:string);
  begin
    putdata(s,length(s)+1);
  end;


procedure tppufile.putsmallset(const b);
  var
    l : longint;
  begin
    l:=longint(b);
    putlongint(l);
  end;


procedure tppufile.putnormalset(const b);
  type
    SetLongintArray = Array [0..7] of longint;
  begin
    putdata(b,32);
  end;


procedure tppufile.tempclose;
  begin
    if not closed then
     begin
       closepos:=filepos(f);
       {$I-}
        system.close(f);
       {$I+}
       if ioresult<>0 then;
       closed:=true;
       tempclosed:=true;
     end;
  end;


function tppufile.tempopen:boolean;
  var
    ofm : byte;
  begin
    tempopen:=false;
    if not closed or not tempclosed then
     exit;
    ofm:=filemode;
    filemode:=0;
    {$I-}
     reset(f,1);
    {$I+}
    filemode:=ofm;
    if ioresult<>0 then
     exit;
    closed:=false;
    tempclosed:=false;

  { restore state }
    seek(f,closepos);
    tempopen:=true;
  end;

end.

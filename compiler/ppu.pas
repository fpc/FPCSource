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

{$ifdef Test_Double_checksum}
var
  CRCFile : text;
const
  CRC_array_Size = 20000;
type
  tcrc_array = array[0..crc_array_size] of longint;
  pcrc_array = ^tcrc_array;
{$endif Test_Double_checksum}

const
{$ifdef OLDPPU}
  CurrentPPUVersion=15;
{$else}
  CurrentPPUVersion=16;
{$endif}

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
  ibstartdefs         = 248;
  ibenddefs           = 249;
  ibstartsyms         = 250;
  ibendsyms           = 251;
  ibendinterface      = 252;
  ibendimplementation = 253;
  ibendbrowser        = 254;
  ibend               = 255;
  {general}
  ibmodulename     = 1;
  ibsourcefiles    = 2;
  ibloadunit       = 3;
  ibinitunit       = 5;
  iblinkofiles     = 6;
  iblinksharedlibs = 7;
  iblinkstaticlibs = 8;
  ibdbxcount       = 9;
  ibsymref         = 10;
  ibdefref         = 11;
  ibendsymtablebrowser   = 12;
  ibbeginsymtablebrowser = 13;
  iblinkunitfiles  = 14;
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
  ibunitsym       = 29;  { needed for browser }
  iblabelsym      = 30;
  ibfuncretsym    = 31;
  ibsyssym        = 32;
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
  ibfarpointerdef  = 57;

{ unit flags }
  uf_init          = $1;
  uf_finalize      = $2;
  uf_big_endian    = $4;
  uf_has_dbx       = $8;
  uf_has_browser   = $10;
  uf_smartlink     = $20;  { the ppu is smartlinked }
  uf_in_library    = $40;  { is the file in another file than <ppufile>.* ? }
  uf_static_linked = $80;  { the ppu is linked in a static library }
  uf_shared_linked = $100; { the ppu is linked in a shared library }
  uf_local_browser = $200;
  uf_obj_linked    = $400; { the ppu is linked in a object file }

type
{$ifdef m68k}
  ppureal=single;
{$else}
  ppureal=extended;
{$endif}

  tppuerror=(ppuentrytoobig,ppuentryerror);

  tppuheader=packed record { 40 bytes }
    id       : array[1..3] of char; { = 'PPU' }
    ver      : array[1..3] of char;
    compiler : word;
    cpu      : word;
    target   : word;
    flags    : longint;
    size     : longint; { size of the ppufile without header }
    checksum : longint; { checksum for this ppufile }
{$ifndef OLDPPU}
    interface_checksum : longint;
    future   : array[0..2] of longint;
{$endif}
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
{$ifdef Test_Double_checksum}
    crcindex : longint;
    crc_index : longint;
    crc_test : pcrc_array;
{$endif def Test_Double_checksum}
    interface_crc : longint;
    do_interface_crc : boolean;
    crc_only : boolean;    { used to calculate interface_crc before implementation }
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

{$ifdef Test_Double_checksum}
  uses
    comphook;
{$endif def Test_Double_checksum}


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


{$ifopt R+}
{$define Range_check_on}
{$endif opt R+}

{$R- needed here }
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

{$ifdef Range_check_on}
{$R+}
{$undef Range_check_on}
{$endif Range_check_on}

{*****************************************************************************
                                  TPPUFile
*****************************************************************************}

constructor tppufile.init(fn:string);
begin
  fname:=fn;
  change_endian:=false;
  crc_only:=false;
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
{$ifdef OLDPPU}
     Ver[3]:='5';
{$else}
     Ver[3]:='6';
{$endif}
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
  { someone added swap(l : longint) in system unit
   this broke the following code !! }
   getlongint:=swap(word(l shr 16)) or (longint(swap(word(l and $ffff))) shl 16)
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
  {$ifndef TP}
    {$ifopt H+}
      setlength(s,getbyte);
    {$else}
      s[0]:=chr(getbyte);
    {$endif}
  {$else}
    s[0]:=chr(getbyte);
  {$endif}
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
  interface_crc:=$ffffffff;
  do_interface_crc:=true;
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
   begin
     crc:=UpdateCrc32(crc,b,len);
{$ifndef OLDPPU}
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
              Def_comment(V_Warning,'CRC changed');
{$ifdef Test_Double_checksum_write}
            Writeln(CRCFile,interface_crc);
{$endif Test_Double_checksum_write}
            inc(crcindex);
          end;
{$endif def Test_Double_checksum}
       end;
    end;
  if not crc_only then
{$else}
    end;
{$endif OLDPPU}
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
  { someone added swap(l : longint) in system unit
   this broke the following code !! }
   l:=swap(word(l shr 16)) or (longint(swap(word(l and $ffff))) shl 16);
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
  Revision 1.29  1999-04-26 13:31:41  peter
    * release storenumber,double_checksum

  Revision 1.28  1999/04/26 09:33:07  peter
    * header extended to 40 bytes so there is room for future

  Revision 1.27  1999/04/17 13:16:20  peter
    * fixes for storenumber

  Revision 1.26  1999/04/07 15:39:31  pierre
    + double_checksum code added

  Revision 1.25  1999/03/02 13:49:18  peter
    * renamed loadunit_int -> loadunit

  Revision 1.24  1999/02/22 13:07:00  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.23  1999/02/16 00:48:24  peter
    * save in the ppu if linked with obj file instead of using the
      library flag, so the .inc files are also checked

  Revision 1.22  1999/02/05 08:54:29  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.21  1998/12/30 22:15:50  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.20  1998/11/30 16:34:45  pierre
    * corrected problems with rangecheck
    + added needed code for no rangecheck  in CRC32 functions in ppu unit
    * enumdef lso need its rangenr reset to zero
      when calling reset_global_defs

  Revision 1.19  1998/11/16 15:41:42  peter
    * tp7 didn't like my ifopt H+ :(

  Revision 1.18  1998/11/16 12:18:03  peter
    * H+ fixes

  Revision 1.17  1998/10/14 10:45:08  pierre
    * ppu problems for m68k fixed (at least in cross compiling)
    * one last memory leak for sysamiga fixed
    * the amiga RTL compiles now completely !!

  Revision 1.16  1998/09/24 23:49:14  peter
    + aktmodeswitches

  Revision 1.15  1998/09/23 15:39:10  pierre
    * browser bugfixes
      was adding a reference when looking for the symbol
      if -bSYM_NAME was used

  Revision 1.14  1998/09/21 10:00:07  peter
    * store number of defs in ppu file

  Revision 1.13  1998/09/21 08:45:18  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.12  1998/09/18 08:01:37  pierre
    + improvement on the usebrowser part
      (does not work correctly for now)

  Revision 1.11  1998/09/11 15:16:47  peter
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

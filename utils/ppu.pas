{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
  CRC_array_Size = 200000;
type
  tcrc_array = array[0..crc_array_size] of longint;
  pcrc_array = ^tcrc_array;
{$endif Test_Double_checksum}

const
{$ifdef newcg}
{$ifdef ORDERSOURCES}
  CurrentPPUVersion=101;
{$else ORDERSOURCES}
  CurrentPPUVersion=100;
{$endif ORDERSOURCES}
{$else newcg}
{$ifdef ORDERSOURCES}
  CurrentPPUVersion=19;
{$else ORDERSOURCES}
  CurrentPPUVersion=18;
{$endif ORDERSOURCES}
{$endif newcg}

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
  ibdbxcount             = 11;
  ibsymref               = 12;
  ibdefref               = 13;
  ibendsymtablebrowser   = 14;
  ibbeginsymtablebrowser = 15;
  ibusedmacros           = 16;
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

{ unit flags }
  uf_init          = $1;
  uf_finalize      = $2;
  uf_big_endian    = $4;
  uf_has_dbx       = $8;
  uf_has_browser   = $10;
  uf_in_library    = $20;  { is the file in another file than <ppufile>.* ? }
  uf_smart_linked  = $40;  { the ppu can be smartlinked }
  uf_static_linked = $80;  { the ppu can be linked static }
  uf_shared_linked = $100; { the ppu can be linked shared }
  uf_local_browser = $200;
  uf_no_link       = $400; { unit has no .o generated, but can still have
                             external linking! }
  uf_has_resources = $800; { unit has resource section }

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
    interface_checksum : longint;
    future   : array[0..2] of longint;
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
    crcindex2 : longint;
    crc_index2 : longint;
    crc_test,crc_test2 : pcrc_array;

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
    closed,
    tempclosed : boolean;
    closepos : longint;
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
    procedure getnormalset(var b);
    procedure getsmallset(var b);
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
    procedure putnormalset(var b);
    procedure putsmallset(var b);
    procedure tempclose;
    function  tempopen:boolean;
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
{$ifdef Delphi}
  Crc32Tbl : array[0..255] of longword;
{$else Delphi}
  Crc32Tbl : array[0..255] of longint;
{$endif Delphi}

procedure MakeCRC32Tbl;
var
{$ifdef Delphi}
  crc : longword;
{$else Delphi}
  crc : longint;
{$endif Delphi}
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
  for i:=1 to InLen do
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
  closed:=true;
  tempclosed:=false;
  getmem(buf,ppubufsize);
end;


destructor tppufile.done;
begin
  close;
  if assigned(buf) then
    freemem(buf,ppubufsize);
end;


procedure tppufile.flush;
begin
  if Mode=2 then
   writebuf;
end;


procedure tppufile.close;
begin
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


function tppufile.GetPPUVersion:longint;
var
  l    : longint;
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

function tppufile.open:boolean;
var
  ofmode : byte;
{$ifdef delphi}
  i      : integer;
{$else delphi}
  i      : word;
{$endif delphi}
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
  closed:=false;
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


procedure tppufile.getsmallset(var b);
begin
  getdata(b,4);
end;


procedure tppufile.getnormalset(var b);
begin
  getdata(b,32);
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
  if not crc_only then
    begin
      assign(f,fname);
      {$I-}
       rewrite(f,1);
      {$I+}
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
  if not crc_only then
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
  opos : longint;
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


procedure tppufile.putdata(var b;len:longint);
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
           Do_comment(V_Warning,'impl CRC changed');
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
{  inc(entryidx);}
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


procedure tppufile.putsmallset(var b);
begin
  putdata(b,4);
end;


procedure tppufile.putnormalset(var b);
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
{
  $Log$
  Revision 1.1  2000-07-13 10:16:22  michael
  + Initial import

  Revision 1.9  2000/01/07 16:46:02  daniel
    * copyright 2000

  Revision 1.8  1999/11/30 10:35:36  peter
    * support new readtype

  Revision 1.51  1999/11/23 09:42:38  peter
    * makefile updates to work with new fpcmake

  Revision 1.50  1999/11/21 01:42:37  pierre
   * Nextoverloading ordering fix

  Revision 1.49  1999/11/18 15:34:48  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.48  1999/11/17 17:05:02  pierre
   * Notes/hints changes

  Revision 1.47  1999/11/06 14:34:23  peter
    * truncated log to 20 revs

  Revision 1.46  1999/09/17 09:14:56  peter
    * ppu header writting now uses currentppuversion

  Revision 1.45  1999/09/16 13:27:08  pierre
    + error if PPU modulename is different from what is searched
      (8+3 limitations!)
    + cond ORDERSOURCES to allow recompilation of FP
      if symppu.inc is changed (need PPUversion change!)

  Revision 1.44  1999/09/16 11:34:58  pierre
   * typo correction

  Revision 1.43  1999/09/10 18:48:09  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.42  1999/08/31 15:47:56  pierre
   + startup conditionals stored in PPU file for debug info

  Revision 1.41  1999/08/30 16:21:40  pierre
   * tempclosing of ppufiles under dos was wrong

  Revision 1.40  1999/08/27 10:48:40  pierre
    + tppufile.tempclose and tempopen added
    * some changes so that nothing is writtedn to disk while
      calculating CRC only

  Revision 1.39  1999/08/24 12:01:36  michael
  + changes for resourcestrings

  Revision 1.38  1999/08/15 10:47:48  peter
    + normalset,smallset writing

  Revision 1.37  1999/08/02 23:13:20  florian
    * more changes to compile for the Alpha

  Revision 1.36  1999/07/23 16:05:25  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.35  1999/07/05 16:21:30  peter
    * fixed linking for units without linking necessary

  Revision 1.34  1999/07/03 00:29:57  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.33  1999/05/13 21:59:36  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.32  1999/05/05 09:19:15  florian
    * more fixes to get it with delphi running

  Revision 1.31  1999/05/04 21:44:59  florian
    * changes to compile it with Delphi 4.0

  Revision 1.30  1999/04/26 18:30:00  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.29  1999/04/26 13:31:41  peter
    * release storenumber,double_checksum

  Revision 1.28  1999/04/26 09:33:07  peter
    * header extended to 40 bytes so there is room for future

  Revision 1.27  1999/04/17 13:16:20  peter
    * fixes for storenumber

}

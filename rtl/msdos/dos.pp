{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$inline on}

unit dos;

interface

Type
  searchrec = packed record
     fill : array[1..21] of byte;
     attr : byte;
     time : longint;
     { reserved : word; not in DJGPP V2 }
     size : longint;
     name : string[255]; { LFN Name, DJGPP uses only [12] but more can't hurt (PFV) }
  end;

{$DEFINE HAS_REGISTERS}
{$I registers.inc}

{$i dosh.inc}

{$IfDef SYSTEM_DEBUG_STARTUP}
  {$DEFINE FORCE_PROXY}
{$endif SYSTEM_DEBUG_STARTUP}
Const
  { This variable can be set to true
    to force use of !proxy command lines even for short
    strings, for debugging purposes mainly, as
    this might have negative impact if trying to
    call non-go32v2 programs }
  force_go32v2_proxy : boolean =
{$ifdef FORCE_PROXY}
  true;
{$DEFINE DEBUG_PROXY}
{$else not FORCE_PROXY}
  false;
{$endif not FORCE_PROXY}
  { This variable allows to use !proxy if command line is
    longer than 126 characters.
    This will only work if the called program knows how to handle
    those command lines.
    Luckily this is the case for Free Pascal compiled
    programs (even old versions)
    and go32v2 DJGPP programs.
    You can set this to false to get a warning to stderr
    if command line is too long. }
  Use_go32v2_proxy : boolean = true;

{ Added to interface so that there is no need to implement it
  both in dos and sysutils units }

procedure exec_ansistring(path : string;comline : ansistring);

procedure Intr(IntNo: Byte; var Regs: Registers); external name 'FPC_INTR';
procedure MsDos(var Regs: Registers); external name 'FPC_MSDOS';

implementation

uses
  strings;

type
  PFarByte = ^Byte;far;
  PFarChar = ^Char;far;
  PFarWord = ^Word;far;

{$DEFINE HAS_GETMSCOUNT}
{$DEFINE HAS_INTR}
{$DEFINE HAS_SETCBREAK}
{$DEFINE HAS_GETCBREAK}
{$DEFINE HAS_SETVERIFY}
{$DEFINE HAS_GETVERIFY}
{$DEFINE HAS_SWAPVECTORS}
{$DEFINE HAS_GETINTVEC}
{$DEFINE HAS_SETINTVEC}
{$DEFINE HAS_KEEP}
{$DEFINE HAS_GETSHORTNAME}
{$DEFINE HAS_GETLONGNAME}

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$I dos.inc}

{******************************************************************************
                           --- Dos Interrupt ---
******************************************************************************}

var
  dosregs : registers;

procedure LoadDosError;
var
  r : registers;
  SimpleDosError : word;
begin
  if (dosregs.flags and fcarry) <> 0 then
   begin
     { I got a extended error = 0
       while CarryFlag was set from Exec function }
     SimpleDosError:=dosregs.ax;
     r.ax:=$5900;
     r.bx:=$0;
     intr($21,r);
     { conversion from word to integer !!
       gave a Bound check error if ax is $FFFF !! PM }
     doserror:=integer(r.ax);
     case doserror of
      0  : DosError:=integer(SimpleDosError);
      19 : DosError:=150;
      21 : DosError:=152;
     end;
   end
  else
    doserror:=0;
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function dosversion : word;
begin
  dosregs.ax:=$3000;
  msdos(dosregs);
  dosversion:=dosregs.ax;
end;


procedure getdate(var year,month,mday,wday : word);
begin
  dosregs.ax:=$2a00;
  msdos(dosregs);
  wday:=dosregs.al;
  year:=dosregs.cx;
  month:=dosregs.dh;
  mday:=dosregs.dl;
end;


procedure setdate(year,month,day : word);
begin
   dosregs.cx:=year;
   dosregs.dh:=month;
   dosregs.dl:=day;
   dosregs.ah:=$2b;
   msdos(dosregs);
end;


procedure gettime(var hour,minute,second,sec100 : word);
begin
  dosregs.ah:=$2c;
  msdos(dosregs);
  hour:=dosregs.ch;
  minute:=dosregs.cl;
  second:=dosregs.dh;
  sec100:=dosregs.dl;
end;


procedure settime(hour,minute,second,sec100 : word);
begin
  dosregs.ch:=hour;
  dosregs.cl:=minute;
  dosregs.dh:=second;
  dosregs.dl:=sec100;
  dosregs.ah:=$2d;
  msdos(dosregs);
end;


function GetMsCount: int64;
begin
  GetMsCount := int64 (MemL [$40:$6c]) * 55;
end;


{******************************************************************************
                               --- Exec ---
******************************************************************************}

const
  DOS_MAX_COMMAND_LINE_LENGTH = 126;

procedure exec_ansistring(path : string;comline : ansistring);
type
  realptr = packed record
    ofs,seg : word;
  end;
  texecblock = packed record
    envseg    : word;
    comtail   : realptr;
    firstFCB  : realptr;
    secondFCB : realptr;
    iniStack  : realptr;
    iniCSIP   : realptr;
  end;
var
  execblock       : texecblock;
  c               : ansistring;
  p               : string;
  arg_ofs         : integer;
  fcb1            : array [0..15] of byte;
  fcb2            : array [0..15] of byte;
begin
  { create command line }
  c:=comline;
  if length(c)>DOS_MAX_COMMAND_LINE_LENGTH then
    begin
      writeln(stderr,'Dos.exec command line truncated to ',
              DOS_MAX_COMMAND_LINE_LENGTH,' chars');
      writeln(stderr,'Before: "',c,'"');
      setlength(c, DOS_MAX_COMMAND_LINE_LENGTH);
      writeln(stderr,'After: "',c,'"');
    end;
  p:=path;
  { allow slash as backslash }
  DoDirSeparators(p);
  if LFNSupport then
    GetShortName(p);
  { allocate FCB see dosexec code }
  arg_ofs:=1;
  while (c[arg_ofs] in [' ',#9]) and
   (arg_ofs<length(c)) do
    inc(arg_ofs);
  dosregs.ax:=$2901;
  dosregs.ds:=Seg(c[arg_ofs]);
  dosregs.si:=Ofs(c[arg_ofs]);
  dosregs.es:=Seg(fcb1);
  dosregs.di:=Ofs(fcb1);
  msdos(dosregs);
  { allocate second FCB see dosexec code }
  dosregs.ax:=$2901;
  dosregs.ds:=Seg(c[arg_ofs]);
  dosregs.si:=Ofs(c[arg_ofs]);
  dosregs.es:=Seg(fcb2);
  dosregs.di:=Ofs(fcb2);
  msdos(dosregs);

  c := Chr(Length(c)) + c + #13 + #0;
  with execblock do
  begin
    envseg:={la_env shr 4}0;
    comtail.seg:=Seg(c[1]);
    comtail.ofs:=Ofs(c[1]);
    firstFCB.seg:=Seg(fcb1);
    firstFCB.ofs:=Ofs(fcb1);
    secondFCB.seg:=Seg(fcb2);
    secondFCB.ofs:=Ofs(fcb2);
  end;

  p := p + #0;
  dosregs.dx:=Ofs(p[1]);
  dosregs.ds:=Seg(p[1]);
  dosregs.bx:=Ofs(execblock);
  dosregs.es:=Seg(execblock);
  dosregs.ax:=$4b00;
  msdos(dosregs);
  LoadDosError;
  if DosError=0 then
   begin
     dosregs.ax:=$4d00;
     msdos(dosregs);
     LastDosExitCode:=DosRegs.al
   end
  else
   LastDosExitCode:=0;
end;

procedure exec(const path : pathstr;const comline : comstr);
begin
  exec_ansistring(path, comline);
end;


procedure getcbreak(var breakvalue : boolean);
begin
  dosregs.ax:=$3300;
  msdos(dosregs);
  breakvalue:=dosregs.dl<>0;
end;


procedure setcbreak(breakvalue : boolean);
begin
  dosregs.ax:=$3301;
  dosregs.dl:=ord(breakvalue);
  msdos(dosregs);
end;


procedure getverify(var verify : boolean);
begin
  dosregs.ah:=$54;
  msdos(dosregs);
  verify:=dosregs.al<>0;
end;


procedure setverify(verify : boolean);
begin
  dosregs.ah:=$2e;
  dosregs.al:=ord(verify);
  msdos(dosregs);
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

type
  ExtendedFat32FreeSpaceRec = packed record
    RetSize           : word;      { $00 }
    Strucversion      : word;      { $02 }
    SecPerClus,                    { $04 }
    BytePerSec,                    { $08 }
    AvailClusters,                 { $0C }
    TotalClusters,                 { $10 }
    AvailPhysSect,                 { $14 }
    TotalPhysSect,                 { $18 }
    AvailAllocUnits,               { $1C }
    TotalAllocUnits   : longword;  { $20 }
    Dummy,                         { $24 }
    Dummy2            : longword;  { $28 }
  end;                             { $2C }

const
  IOCTL_INPUT = 3;       //For request header command field
  CDFUNC_SECTSIZE = 7;   //For cdrom control block func field
  CDFUNC_VOLSIZE  = 8;   //For cdrom control block func field

type
  TRequestHeader = packed record
    length     : byte;         { $00 }
    subunit    : byte;         { $01 }
    command    : byte;         { $02 }
    status     : word;         { $03 }
    reserved1  : longword;     { $05 }
    reserved2  : longword;     { $09 }
    media_desc : byte;         { $0D }
    transf_ofs : word;         { $0E }
    transf_seg : word;         { $10 }
    numbytes   : word;         { $12 }
  end;                         { $14 }

  TCDSectSizeReq = packed record
    func    : byte;            { $00 }
    mode    : byte;            { $01 }
    secsize : word;            { $02 }
  end;                         { $04 }

  TCDVolSizeReq = packed record
    func    : byte;            { $00 }
    size    : longword;        { $01 }
  end;                         { $05 }


function do_diskdata(drive : byte; Free : boolean) : Int64;
var
  blocksize, freeblocks, totblocks : longword;

  { Get disk data via old int21/36 (GET FREE DISK SPACE). It's always supported
    even if it returns wrong values for volumes > 2GB and for cdrom drives when
    in pure DOS. Note that it's also the only way to get some data on WinNTs. }
  function DiskData_36 : boolean;
  begin
    DiskData_36:=false;
    dosregs.dl:=drive;
    dosregs.ah:=$36;
    msdos(dosregs);
    if dosregs.ax=$FFFF then exit;

    blocksize:=dosregs.ax*dosregs.cx;
    freeblocks:=dosregs.bx;
    totblocks:=dosregs.dx;
    Diskdata_36:=true;
  end;

  { Get disk data via int21/7303 (FAT32 - GET EXTENDED FREE SPACE ON DRIVE).
    It is supported by win9x even in pure DOS }
  function DiskData_7303 : boolean;
  var
    s : shortstring;
    rec : ExtendedFat32FreeSpaceRec;
  begin
    DiskData_7303:=false;
    s:=chr(drive+$40)+':\'+#0;

    rec.Strucversion:=0;
    rec.RetSize := 0;
    dosregs.dx:=Ofs(s[1]);
    dosregs.ds:=Seg(s[1]);
    dosregs.di:=Ofs(Rec);
    dosregs.es:=Seg(Rec);
    dosregs.cx:=Sizeof(ExtendedFat32FreeSpaceRec);
    dosregs.ax:=$7303;
    msdos(dosregs);
    if (dosregs.flags and fcarry) <> 0 then
      exit;
    if Rec.RetSize = 0 then
      exit;

    blocksize:=rec.SecPerClus*rec.BytePerSec;
    freeblocks:=rec.AvailAllocUnits;
    totblocks:=rec.TotalAllocUnits;
    DiskData_7303:=true;
  end;

  { Get disk data asking to MSCDEX. Pure DOS returns wrong values with
    int21/7303 or int21/36 if the drive is a CDROM drive }
  function DiskData_CDROM : boolean;
  var req : TRequestHeader;
      sectreq : TCDSectSizeReq;
      sizereq : TCDVolSizeReq;
      i : integer;
      drnum : byte;
  begin
    DiskData_CDROM:=false;
    drnum:=drive-1; //for MSCDEX, 0 = a, 1 = b etc, unlike int21/36

    { Is this a CDROM drive? }
    dosregs.ax:=$150b;
    dosregs.cx:=drnum;
    intr($2f,dosregs);
    if (dosregs.bx<>$ADAD) or (dosregs.ax=0) then
      exit; // no, it isn't

    { Prepare the request header to send to the cdrom driver }
    FillByte(req,sizeof(req),0);
    req.length:=sizeof(req);
    req.command:=IOCTL_INPUT;
    req.transf_ofs:=Ofs(sectreq);
    req.transf_seg:=Seg(sectreq);
    req.numbytes:=sizeof(sectreq);

    { We're asking the sector size }
    sectreq.func:=CDFUNC_SECTSIZE;
    sectreq.mode:=0; //cooked
    sectreq.secsize:=0;

    for i:=1 to 2 do
    begin
      { Send the request to the cdrom driver }
      dosregs.ax:=$1510;
      dosregs.cx:=drnum;
      dosregs.es:=Seg(req);
      dosregs.bx:=Ofs(req);
      intr($2f,dosregs);
      { status = $800F means "disk changed". Try once more. }
      if (req.status and $800F) <> $800F then break;
    end;
    if (req.status<>$0100) or (req.numbytes<>sizeof(sectreq)) then
      exit; //An error occurred

  { Update the request header for the next request }
    FillByte(req,sizeof(req),0);
    req.length:=sizeof(req);
    req.command:=IOCTL_INPUT;
    req.transf_ofs:=Ofs(sizereq);
    req.transf_seg:=Seg(sizereq);
    req.numbytes:=sizeof(sizereq);

    { We're asking the volume size (in blocks) }
    sizereq.func:=CDFUNC_VOLSIZE;
    sizereq.size:=0;

    { Send the request to the cdrom driver }
    dosregs.ax:=$1510;
    dosregs.cx:=drnum;
    dosregs.es:=Seg(req);
    dosregs.bx:=Ofs(req);
    intr($2f,dosregs);
    if (req.status<>$0100) or (req.numbytes<>sizeof(sizereq)) then
      exit; //An error occurred

    blocksize:=sectreq.secsize;
    freeblocks:=0; //always 0 for a cdrom
    totblocks:=sizereq.size;
    DiskData_CDROM:=true;
  end;

begin
  if drive=0 then
  begin
    dosregs.ax:=$1900;    //get current default drive
    msdos(dosregs);
    drive:=dosregs.al+1;
  end;

  if not DiskData_CDROM then
  if not DiskData_7303 then
  if not DiskData_36 then
  begin
    do_diskdata:=-1;
    exit;
  end;
  do_diskdata:=blocksize;
  if free then
    do_diskdata:=do_diskdata*freeblocks
  else
    do_diskdata:=do_diskdata*totblocks;
end;

function diskfree(drive : byte) : int64;
begin
   diskfree:=Do_DiskData(drive,TRUE);
end;

function disksize(drive : byte) : int64;
begin
  disksize:=Do_DiskData(drive,false);
end;


{******************************************************************************
                      --- LFNFindfirst LFNFindNext ---
******************************************************************************}

type
  LFNSearchRec=packed record
    attr,
    crtime,
    crtimehi,
    actime,
    actimehi,
    lmtime,
    lmtimehi,
    sizehi,
    size      : longint;
    reserved  : array[0..7] of byte;
    name      : array[0..259] of byte;
    shortname : array[0..13] of byte;
  end;

procedure LFNSearchRec2Dos(const w:LFNSearchRec;hdl:longint;var d:Searchrec;from_findfirst : boolean);
var
  Len : longint;
begin
  With w do
   begin
     FillChar(d,sizeof(SearchRec),0);
     if DosError=0 then
      len:=StrLen(@Name)
     else
      len:=0;
     d.Name[0]:=chr(len);
     Move(Name[0],d.Name[1],Len);
     d.Time:=lmTime;
     d.Size:=Size;
     d.Attr:=Attr and $FF;
     if (DosError<>0) and from_findfirst then
       hdl:=-1;
     Move(hdl,d.Fill,4);
   end;
end;

{$ifdef DEBUG_LFN}
const
  LFNFileName : string = 'LFN.log';
  LFNOpenNb : longint = 0;
  LogLFN : boolean = false;
var
  lfnfile : text;
{$endif DEBUG_LFN}

procedure LFNFindFirst(path:pchar;attr:longint;var s:searchrec);
var
  i : longint;
  w : LFNSearchRec;
begin
  { allow slash as backslash }
  DoDirSeparators(path);
  dosregs.si:=1; { use ms-dos time }
  { don't include the label if not asked for it, needed for network drives }
  if attr=$8 then
   dosregs.cx:=8
  else
   dosregs.cx:=attr and (not 8);
  dosregs.dx:=Ofs(path^);
  dosregs.ds:=Seg(path^);
  dosregs.di:=Ofs(w);
  dosregs.es:=Seg(w);
  dosregs.ax:=$714e;
  msdos(dosregs);
  LoadDosError;
  if DosError=2 then
    DosError:=18;
{$ifdef DEBUG_LFN}
  if (DosError=0) and LogLFN then
    begin
      Append(lfnfile);
      inc(LFNOpenNb);
      Writeln(lfnfile,LFNOpenNb,' LFNFindFirst called ',path);
      close(lfnfile);
    end;
{$endif DEBUG_LFN}
  LFNSearchRec2Dos(w,dosregs.ax,s,true);
end;


procedure LFNFindNext(var s:searchrec);
var
  hdl : longint;
  w   : LFNSearchRec;
begin
  Move(s.Fill,hdl,4);
  dosregs.si:=1; { use ms-dos time }
  dosregs.di:=Ofs(w);
  dosregs.es:=Seg(w);
  dosregs.bx:=hdl;
  dosregs.ax:=$714f;
  msdos(dosregs);
  LoadDosError;
  LFNSearchRec2Dos(w,hdl,s,false);
end;


procedure LFNFindClose(var s:searchrec);
var
  hdl : longint;
begin
  Move(s.Fill,hdl,4);
  { Do not call MsDos if FindFirst returned with an error }
  if hdl=-1 then
    begin
      DosError:=0;
      exit;
    end;
  dosregs.bx:=hdl;
  dosregs.ax:=$71a1;
  msdos(dosregs);
  LoadDosError;
{$ifdef DEBUG_LFN}
  if (DosError=0) and LogLFN  then
    begin
      Append(lfnfile);
      Writeln(lfnfile,LFNOpenNb,' LFNFindClose called ');
      close(lfnfile);
      if LFNOpenNb>0 then
        dec(LFNOpenNb);
    end;
{$endif DEBUG_LFN}
end;


{******************************************************************************
                     --- DosFindfirst DosFindNext ---
******************************************************************************}

procedure dossearchrec2searchrec(var f : searchrec);
var
  len : longint;
begin
  { Check is necessary!! OS/2's VDM doesn't clear the name with #0 if the }
  { file doesn't exist! (JM)                                              }
  if dosError = 0 then
    len:=StrLen(@f.Name)
  else len := 0;
  Move(f.Name[0],f.Name[1],Len);
  f.Name[0]:=chr(len);
end;


procedure DosFindfirst(path : pchar;attr : word;var f : searchrec);
begin
  { allow slash as backslash }
  DoDirSeparators(path);
  dosregs.dx:=Ofs(f);
  dosregs.ds:=Seg(f);
  dosregs.ah:=$1a;
  msdos(dosregs);
  dosregs.cx:=attr;
  dosregs.dx:=Ofs(path^);
  dosregs.ds:=Seg(path^);
  dosregs.ah:=$4e;
  msdos(dosregs);
  LoadDosError;
  dossearchrec2searchrec(f);
end;


procedure Dosfindnext(var f : searchrec);
begin
  dosregs.dx:=Ofs(f);
  dosregs.ds:=Seg(f);
  dosregs.ah:=$1a;
  msdos(dosregs);
  dosregs.ah:=$4f;
  msdos(dosregs);
  LoadDosError;
  dossearchrec2searchrec(f);
end;


{******************************************************************************
                     --- Findfirst FindNext ---
******************************************************************************}

procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
var
  path0 : array[0..255] of char;
begin
  doserror:=0;
  strpcopy(path0,path);
  if LFNSupport then
   LFNFindFirst(path0,attr,f)
  else
   Dosfindfirst(path0,attr,f);
end;


procedure findnext(var f : searchRec);
begin
  doserror:=0;
  if LFNSupport then
   LFNFindnext(f)
  else
   Dosfindnext(f);
end;


Procedure FindClose(Var f: SearchRec);
begin
  DosError:=0;
  if LFNSupport then
   LFNFindClose(f);
end;


procedure SwapIntVec(IntNo: Byte; var Vector: FarPointer);
var
  tmpvec: FarPointer;
begin
  GetIntVec(IntNo, tmpvec);
  SetIntVec(IntNo, Vector);
  Vector := tmpvec;
end;

procedure SwapVectors;
begin
  SwapIntVec(0, SaveInt00);
end;


{******************************************************************************
                               --- File ---
******************************************************************************}


Function FSearch(path: pathstr; dirlist: string): pathstr;
var
  i,p1   : longint;
  s      : searchrec;
  newdir : pathstr;
begin
{ check if the file specified exists }
  findfirst(path,anyfile and not(directory),s);
  if doserror=0 then
   begin
     findclose(s);
     fsearch:=path;
     exit;
   end;
{ No wildcards allowed in these things }
  if (pos('?',path)<>0) or (pos('*',path)<>0) then
    fsearch:=''
  else
    begin
       { allow slash as backslash }
       DoDirSeparators(dirlist);
       repeat
         p1:=pos(';',dirlist);
         if p1<>0 then
          begin
            newdir:=copy(dirlist,1,p1-1);
            delete(dirlist,1,p1);
          end
         else
          begin
            newdir:=dirlist;
            dirlist:='';
          end;
         if (newdir<>'') and (not (newdir[length(newdir)] in ['\',':'])) then
          newdir:=newdir+'\';
         findfirst(newdir+path,anyfile and not(directory),s);
         if doserror=0 then
          newdir:=newdir+path
         else
          newdir:='';
       until (dirlist='') or (newdir<>'');
       fsearch:=newdir;
    end;
  findclose(s);
end;


{ change to short filename if successful DOS call PM }
function GetShortName(var p : String) : boolean;
var
  c : array[0..255] of char;
begin
  move(p[1],c[0],length(p));
  c[length(p)]:=#0;
  dosregs.ax:=$7160;
  dosregs.cx:=1;
  dosregs.ds:=Seg(c);
  dosregs.si:=Ofs(c);
  dosregs.es:=Seg(c);
  dosregs.di:=Ofs(c);
  msdos(dosregs);
  LoadDosError;
  if DosError=0 then
   begin
     move(c[0],p[1],strlen(c));
     p[0]:=char(strlen(c));
     GetShortName:=true;
   end
  else
   GetShortName:=false;
end;


{ change to long filename if successful DOS call PM }
function GetLongName(var p : String) : boolean;
var
  c : array[0..260] of char;
begin
  move(p[1],c[0],length(p));
  c[length(p)]:=#0;
  dosregs.ax:=$7160;
  dosregs.cx:=2;
  dosregs.ds:=Seg(c);
  dosregs.si:=Ofs(c);
  dosregs.es:=Seg(c);
  dosregs.di:=Ofs(c);
  msdos(dosregs);
  LoadDosError;
  if DosError=0 then
   begin
     c[255]:=#0;
     move(c[0],p[1],strlen(c));
     p[0]:=char(strlen(c));
     GetLongName:=true;
   end
  else
   GetLongName:=false;
end;


{******************************************************************************
                       --- Get/Set File Time,Attr ---
******************************************************************************}

procedure getftime(var f;var time : longint);
begin
  dosregs.bx:=textrec(f).handle;
  dosregs.ax:=$5700;
  msdos(dosregs);
  loaddoserror;
  time:=(dosregs.dx shl 16)+dosregs.cx;
end;


procedure setftime(var f;time : longint);
begin
  dosregs.bx:=textrec(f).handle;
  dosregs.cx:=time and $ffff;
  dosregs.dx:=time shr 16;
  dosregs.ax:=$5701;
  msdos(dosregs);
  loaddoserror;
end;


procedure getfattr(var f;var attr : word);
var
  path: pchar;
{$ifndef FPC_ANSI_TEXTFILEREC}
  r: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
begin
{$ifdef FPC_ANSI_TEXTFILEREC}
  path:=@filerec(f).Name;
{$else}
  r:=ToSingleByteFileSystemEncodedFileName(filerec(f).Name);
  path:=pchar(r);
{$endif}
  dosregs.dx:=Ofs(path^);
  dosregs.ds:=Seg(path^);
  if LFNSupport then
   begin
     dosregs.ax:=$7143;
     dosregs.bx:=0;
   end
  else
   dosregs.ax:=$4300;
  msdos(dosregs);
  LoadDosError;
  Attr:=dosregs.cx;
end;


procedure setfattr(var f;attr : word);
var
  path: pchar;
{$ifndef FPC_ANSI_TEXTFILEREC}
  r: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
begin
  { Fail for setting VolumeId. }
  if ((attr and VolumeID)<>0) then
  begin
    doserror:=5;
    exit;
  end;
{$ifdef FPC_ANSI_TEXTFILEREC}
  path:=@filerec(f).Name;
{$else}
  r:=ToSingleByteFileSystemEncodedFileName(filerec(f).Name);
  path:=pchar(r);
{$endif}
  dosregs.dx:=Ofs(path);
  dosregs.ds:=Seg(path);
  if LFNSupport then
   begin
     dosregs.ax:=$7143;
     dosregs.bx:=1;
   end
  else
   dosregs.ax:=$4301;
  dosregs.cx:=attr;
  msdos(dosregs);
  LoadDosError;
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

function GetEnvStr(EnvNo: Integer; var OutEnvStr: string): integer;
var
  dos_env_seg: Word;
  ofs: Word;
  Ch, Ch2: Char;
begin
  dos_env_seg := PFarWord(Ptr(dos_psp, $2C))^;
  GetEnvStr := 1;
  OutEnvStr := '';
  ofs := 0;
  repeat
    Ch := PFarChar(Ptr(dos_env_seg,ofs))^;
    Ch2 := PFarChar(Ptr(dos_env_seg,ofs + 1))^;
    if (Ch = #0) and (Ch2 = #0) then
      exit;

    if Ch = #0 then
      Inc(GetEnvStr);

    if (Ch <> #0) and (GetEnvStr = EnvNo) then
      OutEnvStr := OutEnvStr + Ch;

    Inc(ofs);
    if ofs = 0 then
      exit;
  until false;
end;


function envcount : longint;
var
  tmpstr: string;
begin
  envcount := GetEnvStr(-1, tmpstr);
end;


function envstr (Index: longint): string;
begin
  GetEnvStr(Index, envstr);
end;


Function  GetEnv(envvar: string): string;
var
  hs    : string;
  eqpos : longint;
  I     : integer;
begin
  envvar:=upcase(envvar);
  getenv:='';
  for I := 1 to envcount do
   begin
     hs:=envstr(I);
     eqpos:=pos('=',hs);
     if upcase(copy(hs,1,eqpos-1))=envvar then
      begin
        getenv:=copy(hs,eqpos+1,length(hs)-eqpos);
        break;
      end;
   end;
end;

{******************************************************************************
                             --- Get/SetIntVec ---
******************************************************************************}

procedure GetIntVec(intno: Byte; var vector: farpointer); assembler;
asm
  mov al, intno
  mov ah, 35h
  int 21h
  xchg ax, bx
  mov bx, vector
  mov [bx], ax
  mov ax, es
  mov [bx + 2], ax
end;

procedure SetIntVec(intno: Byte; vector: farpointer); assembler;
asm
  push ds
  mov al, intno
  mov ah, 25h
  lds dx, word [vector]
  int 21h
  pop ds
end;

{******************************************************************************
                                  --- Keep ---
******************************************************************************}

Procedure Keep(exitcode: word); assembler;
asm
  mov bx, dos_psp
  dec bx
  mov es, bx
  mov dx, es:[3]
  mov ax, exitcode
  mov ah, 31h
  int 21h
end;

{$ifdef DEBUG_LFN}
begin
  LogLFN:=(GetEnv('LOGLFN')<>'');
  assign(lfnfile,LFNFileName);
{$I-}
  Reset(lfnfile);
  if IOResult<>0 then
    begin
      Rewrite(lfnfile);
      Writeln(lfnfile,'New lfn.log');
    end;
  close(lfnfile);
{$endif DEBUG_LFN}

end.

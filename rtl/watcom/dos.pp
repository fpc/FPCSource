{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dos;
interface

Uses
  Watcom;


Const
  {Bitmasks for CPU Flags}
  fcarry     = $0001;
  fparity    = $0004;
  fauxiliary = $0010;
  fzero      = $0040;
  fsign      = $0080;
  foverflow  = $0800;

  {Bitmasks for file attribute}
  readonly  = $01;
  hidden    = $02;
  sysfile   = $04;
  volumeid  = $08;
  directory = $10;
  archive   = $20;
  anyfile   = $3F;

  {File Status}
  fmclosed = $D7B0;
  fminput  = $D7B1;
  fmoutput = $D7B2;
  fminout  = $D7B3;


Type
{ Needed for LFN Support }
  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];

{
  filerec.inc contains the definition of the filerec.
  textrec.inc contains the definition of the textrec.
  It is in a separate file to make it available in other units without
  having to use the DOS unit for it.
}
{$i filerec.inc}
{$i textrec.inc}

  DateTime = packed record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

  searchrec = packed record
     fill : array[1..21] of byte;
     attr : byte;
     time : longint;
     { reserved : word; not in DJGPP V2 }
     size : longint;
     name : string[255]; { LFN Name, DJGPP uses only [12] but more can't hurt (PFV) }
  end;

  Registers = Watcom.Registers;

Var
  DosError : integer;

{Interrupt}
Procedure Intr(intno: byte; var regs: registers);
Procedure MSDos(var regs: registers);

{Info/Date/Time}
Function  DosVersion: Word;
Procedure GetDate(var year, month, mday, wday: word);
Procedure GetTime(var hour, minute, second, sec100: word);
procedure SetDate(year,month,day: word);
Procedure SetTime(hour,minute,second,sec100: word);
Procedure UnpackTime(p: longint; var t: datetime);
Procedure PackTime(var t: datetime; var p: longint);

{Exec}
Procedure Exec(const path: pathstr; const comline: comstr);
Function  DosExitCode: word;

{Disk}
Function  DiskFree(drive: byte) : int64;
Function  DiskSize(drive: byte) : int64;
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);

{File}
Procedure GetFAttr(var f; var attr: word);
Procedure GetFTime(var f; var time: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);
function  GetShortName(var p : String) : boolean;
function  GetLongName(var p : String) : boolean;

{Environment}
Function  EnvCount: longint;
Function  EnvStr(index: integer): string;
Function  GetEnv(envvar: string): string;

{Misc}
Procedure SetFAttr(var f; attr: word);
Procedure SetFTime(var f; time: longint);
Procedure GetCBreak(var breakvalue: boolean);
Procedure SetCBreak(breakvalue: boolean);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);

{Do Nothing Functions}
Procedure SwapVectors;
Procedure GetIntVec(intno: byte; var vector: pointer);
Procedure SetIntVec(intno: byte; vector: pointer);
Procedure Keep(exitcode: word);


implementation

uses
  strings;

{$ASMMODE ATT}

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
     r.eax:=$5900;
     r.ebx:=$0;
     realintr($21,r);
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


procedure intr(intno : byte;var regs : registers);
begin
  realintr(intno,regs);
end;


procedure msdos(var regs : registers);
begin
  intr($21,regs);
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


Procedure packtime(var t : datetime;var p : longint);
Begin
  p:=(t.sec shr 1)+(t.min shl 5)+(t.hour shl 11)+(t.day shl 16)+(t.month shl 21)+((t.year-1980) shl 25);
End;


Procedure unpacktime(p : longint;var t : datetime);
Begin
  with t do
   begin
     sec:=(p and 31) shl 1;
     min:=(p shr 5) and 63;
     hour:=(p shr 11) and 31;
     day:=(p shr 16) and 31;
     month:=(p shr 21) and 15;
     year:=(p shr 25)+1980;
   end;
End;


{******************************************************************************
                               --- Exec ---
******************************************************************************}

var
  lastdosexitcode : word;

procedure exec(const path : pathstr;const comline : comstr);
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
  current_dos_buffer_pos,
  arg_ofs,
  i,la_env,
  la_p,la_c,la_e,
  fcb1_la,fcb2_la : longint;
  execblock       : texecblock;
  c,p             : string;

  function paste_to_dos(src : string) : boolean;
  var
    c : array[0..255] of char;
  begin
     paste_to_dos:=false;
     if current_dos_buffer_pos+length(src)+1>transfer_buffer+tb_size then
      RunError(217);
     move(src[1],c[0],length(src));
     c[length(src)]:=#0;
     seg_move(get_ds,longint(@c),dosmemselector,current_dos_buffer_pos,length(src)+1);
     current_dos_buffer_pos:=current_dos_buffer_pos+length(src)+1;
     paste_to_dos:=true;
  end;

begin
{ create command line }
  move(comline[0],c[1],length(comline)+1);
  c[length(comline)+2]:=#13;
  c[0]:=char(length(comline)+2);
{ create path }
  p:=path;
  for i:=1 to length(p) do
   if p[i]='/' then
    p[i]:='\';
  if LFNSupport then
    GetShortName(p);
{ create buffer }
  la_env:=transfer_buffer;
  while (la_env and 15)<>0 do
   inc(la_env);
  current_dos_buffer_pos:=la_env;
{ copy environment }
  for i:=1 to envcount do
   paste_to_dos(envstr(i));
  paste_to_dos(''); { adds a double zero at the end }
{ allow slash as backslash }
  la_p:=current_dos_buffer_pos;
  paste_to_dos(p);
  la_c:=current_dos_buffer_pos;
  paste_to_dos(c);
  la_e:=current_dos_buffer_pos;
  fcb1_la:=la_e;
  la_e:=la_e+16;
  fcb2_la:=la_e;
  la_e:=la_e+16;
{ allocate FCB see dosexec code }
  arg_ofs:=1;
  while (c[arg_ofs] in [' ',#9]) do
   inc(arg_ofs);
  dosregs.ax:=$2901;
  dosregs.ds:=(la_c+arg_ofs) shr 4;
  dosregs.esi:=(la_c+arg_ofs) and 15;
  dosregs.es:=fcb1_la shr 4;
  dosregs.edi:=fcb1_la and 15;
  msdos(dosregs);
{ allocate second FCB see dosexec code }
  repeat
    inc(arg_ofs);
  until (c[arg_ofs] in [' ',#9,#13]);
  if c[arg_ofs]<>#13 then
   begin
     repeat
       inc(arg_ofs);
     until not (c[arg_ofs] in [' ',#9]);
   end;
  dosregs.ax:=$2901;
  dosregs.ds:=(la_c+arg_ofs) shr 4;
  dosregs.si:=(la_c+arg_ofs) and 15;
  dosregs.es:=fcb2_la shr 4;
  dosregs.di:=fcb2_la and 15;
  msdos(dosregs);
  with execblock do
   begin
     envseg:=la_env shr 4;
     comtail.seg:=la_c shr 4;
     comtail.ofs:=la_c and 15;
     firstFCB.seg:=fcb1_la shr 4;
     firstFCB.ofs:=fcb1_la and 15;
     secondFCB.seg:=fcb2_la shr 4;
     secondFCB.ofs:=fcb2_la and 15;
   end;
  seg_move(get_ds,longint(@execblock),dosmemselector,la_e,sizeof(texecblock));
  dosregs.edx:=la_p and 15;
  dosregs.ds:=la_p shr 4;
  dosregs.ebx:=la_e and 15;
  dosregs.es:=la_e shr 4;
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


function dosexitcode : word;
begin
  dosexitcode:=lastdosexitcode;
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


TYPE  ExtendedFat32FreeSpaceRec=packed Record
         RetSize           : WORD; { (ret) size of returned structure}
         Strucversion      : WORD; {(call) structure version (0000h)
                                    (ret) actual structure version (0000h)}
         SecPerClus,               {number of sectors per cluster}
         BytePerSec,               {number of bytes per sector}
         AvailClusters,            {number of available clusters}
         TotalClusters,            {total number of clusters on the drive}
         AvailPhysSect,            {physical sectors available on the drive}
         TotalPhysSect,            {total physical sectors on the drive}
         AvailAllocUnits,          {Available allocation units}
         TotalAllocUnits : DWORD;  {Total allocation units}
         Dummy,Dummy2    : DWORD;  {8 bytes reserved}
         END;

function do_diskdata(drive : byte; Free : BOOLEAN) : Int64;
VAR
  S    : String;
  Rec  : ExtendedFat32FreeSpaceRec;
BEGIN
 if (swap(dosversion)>=$070A) AND LFNSupport then
  begin
   S:='C:\'#0;
   if Drive=0 then
    begin
     GetDir(Drive,S);
     Setlength(S,4);
     S[4]:=#0;
    end
   else
    S[1]:=chr(Drive+64);
   Rec.Strucversion:=0;
   dosmemput(tb_segment,tb_offset,Rec,SIZEOF(ExtendedFat32FreeSpaceRec));
   dosmemput(tb_segment,tb_offset+Sizeof(ExtendedFat32FreeSpaceRec)+1,S[1],4);
   dosregs.dx:=tb_offset+Sizeof(ExtendedFat32FreeSpaceRec)+1;
   dosregs.ds:=tb_segment;
   dosregs.di:=tb_offset;
   dosregs.es:=tb_segment;
   dosregs.cx:=Sizeof(ExtendedFat32FreeSpaceRec);
   dosregs.ax:=$7303;
   msdos(dosregs);
   if (dosregs.flags and fcarry) = 0 then {No error clausule in int except cf}
    begin
      copyfromdos(rec,Sizeof(ExtendedFat32FreeSpaceRec));
      if Free then
       Do_DiskData:=int64(rec.AvailAllocUnits)*rec.SecPerClus*rec.BytePerSec
      else
       Do_DiskData:=int64(rec.TotalAllocUnits)*rec.SecPerClus*rec.BytePerSec;
    end
   else
    Do_DiskData:=-1;
  end
 else
  begin
   dosregs.dl:=drive;
   dosregs.ah:=$36;
   msdos(dosregs);
   if dosregs.ax<>$FFFF then
    begin
     if Free then
      Do_DiskData:=int64(dosregs.ax)*dosregs.bx*dosregs.cx
     else
      Do_DiskData:=int64(dosregs.ax)*dosregs.cx*dosregs.dx;
    end
   else
    do_diskdata:=-1;
  end;
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


procedure LFNFindFirst(path:pchar;attr:longint;var s:searchrec);
var
  i : longint;
  w : LFNSearchRec;
begin
  { allow slash as backslash }
  for i:=0 to strlen(path) do
    if path[i]='/' then path[i]:='\';
  dosregs.si:=1; { use ms-dos time }
  { don't include the label if not asked for it, needed for network drives }
  if attr=$8 then
   dosregs.ecx:=8
  else
   dosregs.ecx:=attr and (not 8);
  dosregs.edx:=tb_offset+Sizeof(LFNSearchrec)+1;
  dosmemput(tb_segment,tb_offset+Sizeof(LFNSearchrec)+1,path^,strlen(path)+1);
  dosregs.ds:=tb_segment;
  dosregs.edi:=tb_offset;
  dosregs.es:=tb_segment;
  dosregs.ax:=$714e;
  msdos(dosregs);
  LoadDosError;
  copyfromdos(w,sizeof(LFNSearchRec));
  LFNSearchRec2Dos(w,dosregs.ax,s,true);
end;


procedure LFNFindNext(var s:searchrec);
var
  hdl : longint;
  w   : LFNSearchRec;
begin
  Move(s.Fill,hdl,4);
  dosregs.si:=1; { use ms-dos time }
  dosregs.edi:=tb_offset;
  dosregs.es:=tb_segment;
  dosregs.ebx:=hdl;
  dosregs.ax:=$714f;
  msdos(dosregs);
  LoadDosError;
  copyfromdos(w,sizeof(LFNSearchRec));
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
  dosregs.ebx:=hdl;
  dosregs.ax:=$71a1;
  msdos(dosregs);
  LoadDosError;
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
var
   i : longint;
begin
  { allow slash as backslash }
  for i:=0 to strlen(path) do
    if path[i]='/' then path[i]:='\';
  copytodos(f,sizeof(searchrec));
  dosregs.edx:=tb_offset;
  dosregs.ds:=tb_segment;
  dosregs.ah:=$1a;
  msdos(dosregs);
  dosregs.ecx:=attr;
  dosregs.edx:=tb_offset+Sizeof(searchrec)+1;
  dosmemput(tb_segment,tb_offset+Sizeof(searchrec)+1,path^,strlen(path)+1);
  dosregs.ds:=tb_segment;
  dosregs.ah:=$4e;
  msdos(dosregs);
  copyfromdos(f,sizeof(searchrec));
  LoadDosError;
  dossearchrec2searchrec(f);
end;


procedure Dosfindnext(var f : searchrec);
begin
  copytodos(f,sizeof(searchrec));
  dosregs.edx:=tb_offset;
  dosregs.ds:=tb_segment;
  dosregs.ah:=$1a;
  msdos(dosregs);
  dosregs.ah:=$4f;
  msdos(dosregs);
  copyfromdos(f,sizeof(searchrec));
  LoadDosError;
  dossearchrec2searchrec(f);
end;


{******************************************************************************
                     --- Findfirst FindNext ---
******************************************************************************}

procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
var
  path0 : array[0..256] of char;
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


type swap_proc = procedure;

//var
//  _swap_in  : swap_proc;external name '_swap_in';
//  _swap_out : swap_proc;external name '_swap_out';
//  _exception_exit : pointer;external name '_exception_exit';
//  _v2prt0_exceptions_on : longbool;external name '_v2prt0_exceptions_on';

procedure swapvectors;
begin
(*  if _exception_exit<>nil then
    if _v2prt0_exceptions_on then
      _swap_out()
    else
      _swap_in();*)
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;var ext : extstr);
var
   dotpos,p1,i : longint;
begin
  { allow slash as backslash }
  for i:=1 to length(path) do
   if path[i]='/' then path[i]:='\';
  { get drive name }
  p1:=pos(':',path);
  if p1>0 then
    begin
       dir:=path[1]+':';
       delete(path,1,p1);
    end
  else
    dir:='';
  { split the path and the name, there are no more path informtions }
  { if path contains no backslashes                                 }
  while true do
    begin
       p1:=pos('\',path);
       if p1=0 then
         break;
       dir:=dir+copy(path,1,p1);
       delete(path,1,p1);
    end;
  { try to find out a extension }
  if LFNSupport then
    begin
       Ext:='';
       i:=Length(Path);
       DotPos:=256;
       While (i>0) Do
         Begin
            If (Path[i]='.') Then
              begin
                 DotPos:=i;
                 break;
              end;
            Dec(i);
         end;
       Ext:=Copy(Path,DotPos,255);
       Name:=Copy(Path,1,DotPos - 1);
    end
  else
    begin
       p1:=pos('.',path);
       if p1>0 then
         begin
            ext:=copy(path,p1,4);
            delete(path,p1,length(path)-p1+1);
         end
       else
         ext:='';
       name:=path;
    end;
end;


(*
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
*)

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_DRIVES}
{$UNDEF FPC_FEXPAND_UNC}


Function FSearch(path: pathstr; dirlist: string): pathstr;
var
  i,p1   : longint;
  s      : searchrec;
  newdir : pathstr;
begin
{ check if the file specified exists }
  findfirst(path,anyfile,s);
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
       for i:=1 to length(dirlist) do
         if dirlist[i]='/' then dirlist[i]:='\';
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
         findfirst(newdir+path,anyfile,s);
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
  copytodos(c,length(p)+1);
  dosregs.ax:=$7160;
  dosregs.cx:=1;
  dosregs.ds:=tb_segment;
  dosregs.si:=tb_offset;
  dosregs.es:=tb_segment;
  dosregs.di:=tb_offset;
  msdos(dosregs);
  LoadDosError;
  if DosError=0 then
   begin
     copyfromdos(c,255);
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
  c : array[0..255] of char;
begin
  move(p[1],c[0],length(p));
  c[length(p)]:=#0;
  copytodos(c,length(p)+1);
  dosregs.ax:=$7160;
  dosregs.cx:=2;
  dosregs.ds:=tb_segment;
  dosregs.si:=tb_offset;
  dosregs.es:=tb_segment;
  dosregs.di:=tb_offset;
  msdos(dosregs);
  LoadDosError;
  if DosError=0 then
   begin
     copyfromdos(c,255);
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
begin
  copytodos(filerec(f).name,strlen(filerec(f).name)+1);
  dosregs.edx:=tb_offset;
  dosregs.ds:=tb_segment;
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
begin
  copytodos(filerec(f).name,strlen(filerec(f).name)+1);
  dosregs.edx:=tb_offset;
  dosregs.ds:=tb_segment;
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

function envcount : longint;
var
  hp : ppchar;
begin
  hp:=envp;
  envcount:=0;
  while assigned(hp^) do
   begin
     inc(envcount);
     inc(hp);
   end;
end;


function envstr(index : integer) : string;
begin
  if (index<=0) or (index>envcount) then
   begin
     envstr:='';
     exit;
   end;
  envstr:=strpas(ppchar(pointer(envp)+4*(index-1))^);
end;


Function  GetEnv(envvar: string): string;
var
  hp      : ppchar;
  hs    : string;
  eqpos : longint;
begin
  envvar:=upcase(envvar);
  hp:=envp;
  getenv:='';
  while assigned(hp^) do
   begin
     hs:=strpas(hp^);
     eqpos:=pos('=',hs);
     if upcase(copy(hs,1,eqpos-1))=envvar then
      begin
        getenv:=copy(hs,eqpos+1,255);
        exit;
      end;
     inc(hp);
   end;
end;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

Procedure keep(exitcode : word);
Begin
End;

Procedure getintvec(intno : byte;var vector : pointer);
Begin
End;

Procedure setintvec(intno : byte;vector : pointer);
Begin
End;


end.

{
  $Log$
  Revision 1.2  2003-09-07 22:29:26  hajny
    * syswat renamed to system, CVS log added

  Revision 1.1  2003/09/05 18:09:35  florian
    * added initial watcom extender files; they need to be cleaned up

}

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by the Free Pascal development team.

    DOS unit for BP7 compatible RTL, Human68k implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit dos;
{$ENDIF FPC_DOTTEDUNITS}

interface


type
  SearchRec = record
    { Replacement for Fill }
    IFD: Pointer;
    Fill: Array[1..17] of Byte; {future use}
    {End of replacement for fill}
    Attr : BYTE;        {attribute of found file}
    Time : LongInt;     {last modify date of found file}
    Size : LongInt;     {file size of found file}
    Name : String[255]; {name of found file}
  end;

{$i dosh.inc}

implementation

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$i dos.inc}

{$i h68kdos.inc}


procedure Error2DosError(errno: longint);
begin
  case errno of
    DOSE_NOENT: DosError:=2;   // File not found
    DOSE_NODIR: DosError:=3;   // Directory (folder/path) not found
    DOSE_ISDIR: DosError:=5;   // Access denied
    DOSE_BADF:  DosError:=6;   // Invalid file handle
    DOSE_NOMEM: DosError:=8;   // Insufficient memory
    DOSE_MFILE: DosError:=18;  // No more files can be opened
  else
    DosError:=errno;
  end;
end;


function DosVersion: Word;
begin
  DosVersion:=Swap(human68k_vernum);
end;


procedure GetDate(Var Year, Month, MDay, WDay: Word);
var
  OSDate: LongInt;
  D: DateTime;
begin
  OSDate:=h68kdos_getdate;

  { the time values will be invalid here,
    but it doesn't matter, we want the date }
  UnpackTime(OSDate shl 16,D);

  Year:=D.Year;
  Month:=D.Month;
  MDay:=D.Day;
  WDay:=OSDate shr 16;
end;

procedure SetDate(Year, Month, Day: Word);
var
  D: DateTime;
  OSDate: LongInt;
begin
  D.Year:=Year;
  D.Month:=Month;
  D.Day:=Day;
  PackTime(D,OSDate);
  h68kdos_setdate(hi(OSDate));
end;

procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
var
  OSTime: LongInt;
  T: DateTime;
begin
  OSTime:=h68kdos_gettime;

  { the date values will be invalid here,
    but it doesn't matter, we want the time }
  UnpackTime(OSTime,T);

  Hour:=T.Hour;
  Minute:=T.Min;
  Second:=T.Sec;
  Sec100:=0;
end;

procedure SetTime(Hour, Minute, Second, Sec100: Word);
var
  T: DateTime;
  OSTime: LongInt;
begin
  T.Hour:=Hour;
  T.Min:=Minute;
  T.Sec:=Second;
  PackTime(T,OSTime);
  h68kdos_settime(lo(OSTime));
end;

function h68kdos_exec0(const fil: pchar; p1: pointer; p2: pointer): longint; external name '_fpc_h68kdos_exec0';

procedure Exec(const Path: PathStr; const ComLine: ComStr);
var
  dosResult: LongInt;
  tmpPath: String;
begin
  tmpPath:=Path+#0;
  DoDirSeparators(tmpPath);

  { 1) If I understand the Human68k documentation, this will not execute
       programs in the PATH, but you need an exec, mode 2 call first.
       Not sure how the original DOS unit Exec() call behaves. (KB) }
  { 2) the zero offset for cmdline is actually correct here. exec() expects
       pascal formatted string for cmdline, so length in first byte }
  dosResult:=h68kdos_exec0(PAnsiChar(@tmpPath[1]),@ComLine[0],nil);
  if dosResult < 0 then
    Error2DosError(dosResult);
end;


function DiskSize(Drive: Byte): Int64;
var
  dosResult: longint;
  fi: Th68kdos_freeinfo;
begin
  DiskSize := -1;

  dosResult:=h68kdos_dskfre(drive,@fi);
  if dosResult < 0 then
    exit;

  DiskSize:=fi.max * fi.sectors * fi.bytes;
end;

function DiskFree(Drive: Byte): Int64;
var
  dosResult: longint;
  fi: Th68kdos_freeinfo;
begin
  DiskFree := -1;

  dosResult:=h68kdos_dskfre(drive,@fi);
  if dosResult < 0 then
    exit;

  DiskFree:=fi.free * fi.sectors * fi.bytes;
end;


type
  PInternalFindData = ^TInternalFindData;
  TInternalFindData = record
    filebuf: Th68kdos_filbuf;
  end;

procedure FindFirst(const Path: PathStr; Attr: Word; Var f: SearchRec);
var
  p: PathStr;
  r: RawByteString;
  dosResult: LongInt;
  IFD: PInternalFindData;
begin
  p:=Path;
  DoDirSeparators(p);
  r:=p;

  new(IFD);
  f.IFD:=IFD;

  dosResult:=h68kdos_files(@IFD^.filebuf, PAnsiChar(r), Attr and AnyFile);
  if dosResult < 0 then
    begin
      Error2DosError(dosResult);
      FindClose(f);
      exit;
    end;

  DosError:=0;
  with IFD^.filebuf do
    begin
      f.name:=name;
      f.time:=(date shl 16) + time;
      f.size:=filelen;
      f.attr:=atr;
    end;
end;

procedure FindNext(Var f: SearchRec);
var
  IFD: PInternalFindData;
  dosResult: LongInt;
begin
  IFD:=f.IFD;
  if not assigned(IFD) then
    begin
      DosError:=6;
      exit;
    end;

  dosResult:=h68kdos_nfiles(@IFD^.filebuf);
  if dosResult < 0 then
    begin
      Error2DosError(dosResult);
      exit;
    end;

  DosError:=0;
  with IFD^.filebuf do
    begin
      f.name:=name;
      f.time:=(date shl 16) + time;
      f.size:=filelen;
      f.attr:=atr;
    end;
end;

procedure FindClose(Var f: SearchRec);
var
  IFD: PInternalFindData;
begin
  IFD:=f.IFD;
  if not assigned(IFD) then
    exit;

  dispose(IFD);
  f.IFD:=nil;
end;

function FSearch(path: PathStr; dirlist: String) : PathStr;
var
  p1     : longint;
  s      : searchrec;
  newdir : pathstr;
begin
  { No wildcards allowed in these things }
  if (pos('?',path)<>0) or (pos('*',path)<>0) then
  begin
    fsearch:='';
    exit;
  end;
  { check if the file specified exists }
  findfirst(path,anyfile and not(directory),s);
  if doserror=0 then
    begin
     findclose(s);
     fsearch:=path;
     exit;
    end;
  findclose(s);
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
    findclose(s);
  until (dirlist='') or (newdir<>'');
  fsearch:=newdir;
end;

procedure GetFAttr(var f; var Attr : word);
var
  dosResult: LongInt;
  path: PAnsiChar;
{$ifndef FPC_ANSI_TEXTFILEREC}
  r: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
begin
{$ifdef FPC_ANSI_TEXTFILEREC}
  path:=@filerec(f).Name;
{$else}
  r:=ToSingleByteFileSystemEncodedFileName(filerec(f).Name);
  path:=PAnsiChar(r);
{$endif}

  Attr:=0;
  dosResult:=h68kdos_chmod(path,-1);
  if dosResult < 0 then
    Error2DosError(dosResult)
  else
    Attr:=word(dosResult);
end;

procedure GetFTime(var f; var Time : longint);
var
  dosResult: longint;
begin
  Time:=-1;

  if hi(human68k_vernum) <= 2 then
    dosResult:=h68kdos_filedate_v2(TextRec(f).Handle,0)
  else
    dosResult:=h68kdos_filedate_v3(TextRec(f).Handle,0);
  if hi(dosResult) = $ffff then
    begin
      Error2DosError(dosResult);
      exit;
    end;

  Time:=dosResult;
end;

procedure SetFAttr(var f; attr : word);
var
  dosResult: LongInt;
  path: PAnsiChar;
{$ifndef FPC_ANSI_TEXTFILEREC}
  r: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
begin
{$ifdef FPC_ANSI_TEXTFILEREC}
  path:=@filerec(f).Name;
{$else}
  r:=ToSingleByteFileSystemEncodedFileName(filerec(f).Name);
  path:=PAnsiChar(r);
{$endif}

  dosResult:=h68kdos_chmod(path,Attr);
  if dosResult < 0 then
    Error2DosError(dosResult);
end;

procedure SetFTime(var f; time : longint);
var
  dosResult: longint;
begin
  if hi(human68k_vernum) <= 2 then
    dosResult:=h68kdos_filedate_v2(TextRec(f).Handle,time)
  else
    dosResult:=h68kdos_filedate_v3(TextRec(f).Handle,time);
  if hi(dosResult) = $ffff then
    begin
      Error2DosError(dosResult);
      exit;
    end;
end;

function EnvCount: Longint;
var
  hp : PAnsiChar;
begin
  EnvCount:=0;
  hp:=''; // FIX ME!
  If (Hp<>Nil) then
    while hp^<>#0 do
      begin
      Inc(EnvCount);
      hp:=hp+strlen(hp)+1;
      end;
end;

function EnvStr(Index: LongInt): String;
var
  hp : PAnsiChar;
begin
  EnvStr:='';
  hp:=nil; // FIX ME!
  If (Hp<>Nil) then
    begin
      while (hp^<>#0) and (Index>1) do
        begin
          Dec(Index);
          hp:=hp+strlen(hp)+1;
        end;
    If (hp^<>#0) then
      begin
        EnvStr:=hp;
      end;
    end;
end;

function GetEnv(envvar : String): String;
begin
  GetEnv:='';
end;


end.

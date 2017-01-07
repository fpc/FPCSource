{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team.

    DOS unit for BP7 compatible RTL, Atari implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dos;

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

{$i gemdos.inc}

procedure Error2DosError(errno: longint);
begin
  case errno of
    EFILNF: DosError:=2;   // File not found
    EPTHNF: DosError:=3;   // Directory (folder/path) not found
    EACCDN: DosError:=5;   // Access denied
    EIHNDL: DosError:=6;   // Invalid file handle
    ENSMEM: DosError:=8;   // Insufficient memory
    ENMFIL: DosError:=18;  // No more files can be opened
  else
    DosError:=errno;
  end;
end;


function DosVersion: Word;
begin
  DosVersion:=0;
end;


function WeekDay (y,m,d:longint):longint;
{
  Calculates th day of the week. returns -1 on error
}
var
  u,v : longint;
begin
  if (m<1) or (m>12) or (y<1600) or (y>4000) or
     (d<1) or (d>30+((m+ord(m>7)) and 1)-ord(m=2)) or
     ((m*d=58) and (((y mod 4>0) or (y mod 100=0)) and (y mod 400>0))) then
   WeekDay:=-1
  else
   begin
     u:=m;
     v:=y;
     if m<3 then
      begin
        inc(u,12);
        dec(v);
      end;
     WeekDay:=(d+2*u+((3*(u+1)) div 5)+v+(v div 4)-(v div 100)+(v div 400)+1) mod 7;
   end;
end;


procedure GetDate(Var Year, Month, MDay, WDay: Word);
var
  TOSDate: LongInt;
  D: DateTime;
begin
  TOSDate:=gemdos_tgetdate shl 16;

  { the time values will be invalid here,
    but it doesn't matter, we want the date }
  UnpackTime(TOSDate,D);

  Year:=D.Year;
  Month:=D.Month;
  MDay:=D.Day;
  WDay:=WeekDay(Year,Month,MDay);
end;

procedure SetDate(Year, Month, Day: Word);
var
  D: DateTime;
  TOSDate: LongInt;
begin
  D.Year:=Year;
  D.Month:=Month;
  D.Day:=Day;
  PackTime(D,TOSDate);
  gemdos_tsetdate(hi(TOSDate));
end;

procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
var
  TOSTime: LongInt;
  T: DateTime;
begin
  TOSTime:=gemdos_tgettime;

  { the date values will be invalid here,
    but it doesn't matter, we want the time }
  UnpackTime(TOSTime,T);

  Hour:=T.Hour;
  Minute:=T.Min;
  Second:=T.Sec;
  Sec100:=0;
end;

procedure SetTime(Hour, Minute, Second, Sec100: Word);
var
  T: DateTime;
  TOSTime: LongInt;
begin
  T.Hour:=Hour;
  T.Min:=Minute;
  T.Sec:=Second;
  PackTime(T,TOSTime);
  gemdos_tsettime(lo(TOSTime));
end;

procedure Exec(const Path: PathStr; const ComLine: ComStr);
var
  dosResult: LongInt;
  tmpPath: String;
begin
  tmpPath:=Path+#0;
  DoDirSeparators(tmpPath);

  { the zero offset for cmdline is actually correct here. pexec() expects
    pascal formatted string for cmdline, so length in first byte }
  dosResult:=gemdos_pexec(0,PChar(@tmpPath[1]),@ComLine[0],nil);
  if dosResult < 0 then
    Error2DosError(dosResult);
end;

function DiskSize(Drive: Byte): Int64;
var
  dosResult: longint;
  di: TDISKINFO;
begin
  DiskSize := -1;

  dosResult:=gemdos_dfree(@di,drive);
  if dosResult < 0 then
    exit;

  DiskSize:=di.b_total * di.b_secsiz * di.b_clsiz;
end;

function DiskFree(Drive: Byte): Int64;
var
  dosResult: longint;
  di: TDISKINFO;
begin
  DiskFree := -1;

  dosResult:=gemdos_dfree(@di,drive);
  if dosResult < 0 then
    exit;

  DiskFree:=di.b_free * di.b_secsiz * di.b_clsiz;
end;


type
  PInternalFindData = ^TInternalFindData;
  TInternalFindData = record
    dta_original: pointer;
    dta_search: TDTA;
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
  IFD^.dta_original:=gemdos_getdta;
  gemdos_setdta(@IFD^.dta_search);

  f.IFD:=IFD;
  dosResult:=gemdos_fsfirst(pchar(r), Attr);
  if dosResult < 0 then
    begin
      Error2DosError(dosResult);
      exit;
    end;

  DosError:=0;
  with IFD^.dta_search do
    begin
      f.name:=d_fname;
      f.time:=(d_date shl 16) + d_time;
      f.size:=d_length;
      f.attr:=d_attrib;
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

  dosResult:=gemdos_fsnext;
  if dosResult < 0 then
    begin
      Error2DosError(dosResult);
      exit;
    end;

  DosError:=0;
  with IFD^.dta_search do
    begin
      f.name:=d_fname;
      f.time:=(d_date shl 16) + d_time;
      f.size:=d_length;
      f.attr:=d_attrib;
    end;
end;

procedure FindClose(Var f: SearchRec);
var
  IFD: PInternalFindData;
begin
  IFD:=f.IFD;
  if not assigned(IFD) then
    exit;

  gemdos_setdta(IFD^.dta_original);

  dispose(IFD);
  f.IFD:=nil;
end;

function FSearch(path: PathStr; dirlist: String) : PathStr;
begin
  FSearch:='';
end;

procedure GetFAttr(var f; var Attr : word);
var
  dosResult: LongInt;
  path: PChar;
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

  Attr:=0;
  dosResult:=gemdos_fattrib(path,0,0);
  if dosResult < 0 then
    Error2DosError(dosResult)
  else
    Attr:=word(dosResult);
end;

procedure GetFTime(var f; var Time : longint);
var
  td: TDOSTIME;
begin
  gemdos_fdatime(@td,TextRec(f).Handle,0);
  Time:=(td.date << 16) + td.time;
end;

procedure SetFAttr(var f; attr : word);
var
  dosResult: LongInt;
  path: PChar;
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

  dosResult:=gemdos_fattrib(pchar(@FileRec(f).name),1,Attr);
  if dosResult < 0 then
    Error2DosError(dosResult)
end;

procedure SetFTime(var f; time : longint);
var
  td: TDOSTIME;
begin
  td.date:=Hi(Time);
  td.time:=Lo(Time);

  gemdos_fdatime(@td,TextRec(f).Handle,1);
end;

function EnvCount: Longint;
begin
  EnvCount:=0;
end;

function EnvStr(Index: LongInt): String;
begin
  EnvStr:='';
end;

function GetEnv(envvar : String): String;
begin
  GetEnv:='';
end;


end.

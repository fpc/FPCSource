{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman,
    members of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit Dos;
Interface

Const
  FileNameLen = 255;

Type
  SearchRec =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    Record
  {Fill : array[1..21] of byte;  Fill replaced with below}
    SearchPos  : UInt64;      {directory position}
    SearchNum  : LongInt;     {to track which search this is}
    DirFD      : LongInt;     {directory fd handle for reading directory}
    SearchType : Byte;        {0=normal, 1=open will close, 2=only 1 file}
    SearchAttr : Byte;        {attribute we are searching for}
    Mode       : Word;
    Fill       : Array[1..1] of Byte; {future use}
  {End of fill}
    Attr       : Byte;        {attribute of found file}
    Time       : LongInt;     {last modify date of found file}
    Size       : LongInt;     {file size of found file}
    Reserved   : Word;        {future use}
    Name       : String[FileNameLen]; {name of found file}
    SearchSpec : String[FileNameLen]; {search pattern}
    NamePos    : Word;        {end of path, start of name position}
  End;

{$DEFINE HAS_FILENAMELEN}
{$i dosh.inc}

{Extra Utils}
function weekday(y,m,d : longint) : longint; platform;
Procedure WasiDateToDt(NanoSecsPast: UInt64; Var Dt: DateTime); platform;
Function DTToWasiDate(DT: DateTime): UInt64; platform;

{Disk}
//Function AddDisk(const path:string) : byte; platform;

Implementation

Uses
  WasiAPI, WasiUtil;

{$DEFINE HAS_GETMSCOUNT}

{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
{$DEFINE FPC_FEXPAND_GETENVPCHAR} { GetEnv result is a PChar }

{$I dos.inc}


{******************************************************************************
                           --- Link C Lib if set ---
******************************************************************************}


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

Function DosVersion:Word;
Begin
End;

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


Procedure GetDate(Var Year, Month, MDay, WDay: Word);
var
  NanoSecsPast: __wasi_timestamp_t;
  DT: DateTime;
begin
  if __wasi_clock_time_get(__WASI_CLOCKID_REALTIME,10000000,@NanoSecsPast)=__WASI_ERRNO_SUCCESS then
  begin
    WasiDateToDT(NanoSecsPast,DT);
    Year:=DT.Year;
    Month:=DT.Month;
    MDay:=DT.Day;
    WDay:=weekday(DT.Year,DT.Month,DT.Day);
  end
  else
  begin
    Year:=0;
    Month:=0;
    MDay:=0;
    WDay:=0;
  end;
end;


procedure  SetTime(Hour,Minute,Second,sec100:word);
begin
end;

procedure SetDate(Year,Month,Day:Word);
begin
end;


Function SetDateTime(Year,Month,Day,hour,minute,second:Word) : Boolean;
begin
end;


Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
var
  NanoSecsPast: __wasi_timestamp_t;
begin
  if __wasi_clock_time_get(__WASI_CLOCKID_REALTIME,10000000,@NanoSecsPast)=__WASI_ERRNO_SUCCESS then
  begin
    { todo: convert UTC to local time, as soon as we can get the local timezone
      from WASI: https://github.com/WebAssembly/WASI/issues/239 }
    NanoSecsPast:=NanoSecsPast div 10000000;
    Sec100:=NanoSecsPast mod 100;
    NanoSecsPast:=NanoSecsPast div 100;
    Second:=NanoSecsPast mod 60;
    NanoSecsPast:=NanoSecsPast div 60;
    Minute:=NanoSecsPast mod 60;
    NanoSecsPast:=NanoSecsPast div 60;
    Hour:=NanoSecsPast mod 24;
  end
  else
  begin
    Hour:=0;
    Minute:=0;
    Second:=0;
    Sec100:=0;
  end;
end;


Function DTToWasiDate(DT: DateTime): UInt64;
var
  res: Int64;
begin
  res:=WasiUtil.LocalToEpoch(DT.year,DT.month,DT.day,DT.hour,DT.min,DT.sec);
  if res<0 then
    DTToWasiDate:=0
  else
    DTToWasiDate:=res*1000000000;
end;


Procedure WasiDateToDt(NanoSecsPast: UInt64; Var Dt: DateTime);
Begin
  WasiUtil.EpochToLocal(NanoSecsPast div 1000000000,Dt.Year,Dt.Month,Dt.Day,Dt.Hour,Dt.Min,Dt.Sec);
End;


function GetMsCount: int64;
var
  NanoSecsPast: __wasi_timestamp_t;
begin
  if __wasi_clock_time_get(__WASI_CLOCKID_REALTIME,1000000,@NanoSecsPast)=__WASI_ERRNO_SUCCESS then
    GetMsCount:=NanoSecsPast div 1000000
  else
    GetMsCount:=0;
end;


{******************************************************************************
                               --- Exec ---
******************************************************************************}

Procedure Exec (Const Path: PathStr; Const ComLine: ComStr);
Begin
End;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

{
  The Diskfree and Disksize functions need a file on the specified drive, since this
  is required for the fpstatfs system call.
  These filenames are set in drivestr[0..26], and have been preset to :
   0 - '.'      (default drive - hence current dir is ok.)
   1 - '/fd0/.'  (floppy drive 1 - should be adapted to local system )
   2 - '/fd1/.'  (floppy drive 2 - should be adapted to local system )
   3 - '/'       (C: equivalent of dos is the root partition)
   4..26          (can be set by you're own applications)
  ! Use AddDisk() to Add new drives !
  They both return -1 when a failure occurs.
}
Const
  FixDriveStr : array[0..3] of pchar=(
    '.',
    '/fd0/.',
    '/fd1/.',
    '/.'
    );
const
  Drives   : byte = 4;
var
  DriveStr : array[4..26] of pchar;

Function AddDisk(const path:string) : byte;
begin
{  if not (DriveStr[Drives]=nil) then
   FreeMem(DriveStr[Drives]);
  GetMem(DriveStr[Drives],length(Path)+1);
  StrPCopy(DriveStr[Drives],path);
  AddDisk:=Drives;
  inc(Drives);
  if Drives>26 then
    Drives:=4;}
end;



Function DiskFree(Drive: Byte): int64;
{var
  fs : tstatfs;}
Begin
{  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (fpStatFS(fixdrivestr[drive],@fs)<>-1)) or
     ((not (drivestr[Drive]=nil)) and (fpStatFS(drivestr[drive],@fs)<>-1)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;}
End;



Function DiskSize(Drive: Byte): int64;
{var
  fs : tstatfs;}
Begin
{  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (fpStatFS(fixdrivestr[drive],@fs)<>-1)) or
     ((not (drivestr[Drive]=nil)) and (fpStatFS(drivestr[drive],@fs)<>-1)) then
   DiskSize:=int64(fs.blocks)*int64(fs.bsize)
  else
   DiskSize:=-1;}
End;



Procedure FreeDriveStr;
{var
  i: longint;}
begin
{  for i:=low(drivestr) to high(drivestr) do
    if assigned(drivestr[i]) then
      begin
        freemem(drivestr[i]);
        drivestr[i]:=nil;
      end;}
end;

{******************************************************************************
                       --- Findfirst FindNext ---
******************************************************************************}


procedure SearchRec2WasiSearchRec(const i: SearchRec; var o: TWasiSearchRec);
var
  DT: DateTime;
begin
  FillChar(o,SizeOf(o),0);
  o.SearchPos:=i.SearchPos;
  o.SearchNum:=i.SearchNum;
  o.DirFD:=i.DirFD;
  o.SearchType:=i.SearchType;
  o.SearchAttr:=i.SearchAttr;
  o.Attr:=i.Attr;
  UnpackTime(i.Time,DT);
  o.Time:=DTToWasiDate(DT);
  o.Size:=i.Size;
  o.Name:=i.Name;
  o.SearchSpec:=i.SearchSpec;
  o.NamePos:=i.NamePos;
end;


procedure WasiSearchRec2SearchRec(const i: TWasiSearchRec; var o: SearchRec);
var
  DT: DateTime;
begin
  FillChar(o,SizeOf(o),0);
  o.SearchPos:=i.SearchPos;
  o.SearchNum:=i.SearchNum;
  o.DirFD:=i.DirFD;
  o.SearchType:=i.SearchType;
  o.SearchAttr:=i.SearchAttr;
  o.Attr:=i.Attr;
  WasiDateToDt(i.Time,DT);
  PackTime(DT,o.Time);
  o.Size:=i.Size;
  o.Name:=i.Name;
  o.SearchSpec:=i.SearchSpec;
  o.NamePos:=i.NamePos;
end;


Procedure FindClose(Var f: SearchRec);
var
  wf: TWasiSearchRec;
Begin
  SearchRec2WasiSearchRec(f,wf);
  WasiFindClose(wf);
  WasiSearchRec2SearchRec(wf,f);
End;


Procedure FindNext(Var f: SearchRec);
var
  wf: TWasiSearchRec;
Begin
  SearchRec2WasiSearchRec(f,wf);
  doserror:=WasiFindNext(wf);
  WasiSearchRec2SearchRec(wf,f);
End;


Procedure FindFirst(Const Path: PathStr; Attr: Word; Var f: SearchRec);
var
  wf: TWasiSearchRec;
Begin
  SearchRec2WasiSearchRec(f,wf);
  doserror:=WasiFindFirst(Path,Attr,wf);
  WasiSearchRec2SearchRec(wf,f);
End;


{******************************************************************************
                               --- File ---
******************************************************************************}

Function FSearch(path: pathstr; dirlist: string): pathstr;
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
  //{ allow slash as backslash }
  //DoDirSeparators(dirlist);
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
   if (newdir<>'') and (not (newdir[length(newdir)] in (AllowDirectorySeparators+[':']))) then
    newdir:=newdir+DirectorySeparator;
   findfirst(newdir+path,anyfile and not(directory),s);
   if doserror=0 then
    newdir:=newdir+path
   else
    newdir:='';
   findclose(s);
 until (dirlist='') or (newdir<>'');
 fsearch:=newdir;
end;

Procedure GetFAttr(var f; var attr : word);
Var
  pr: RawByteString;
  fd: __wasi_fd_t;
  Info: __wasi_filestat_t;
Begin
  DosError:=0;
  Attr:=0;
  if ConvertToFdRelativePath(textrec(f).name,fd,pr)<>0 then
    begin
      DosError:=3;
      exit;
    end;
  if __wasi_path_filestat_get(fd,__WASI_LOOKUPFLAGS_SYMLINK_FOLLOW,PChar(pr),length(pr),@Info)<>__WASI_ERRNO_SUCCESS then
    begin
      DosError:=3;
      exit;
    end;
  if Info.filetype=__WASI_FILETYPE_DIRECTORY then
    Attr:=$10;
  if filerec(f).name[0]='.' then
    Attr:=Attr or $2;
end;

Procedure getftime (var f; var time : longint);
Var
  res: __wasi_errno_t;
  Info: __wasi_filestat_t;
  DT: DateTime;
Begin
  doserror:=0;
  res:=__wasi_fd_filestat_get(filerec(f).handle,@Info);
  if res<>__WASI_ERRNO_SUCCESS then
   begin
     Time:=0;
     case res of
       __WASI_ERRNO_ACCES,
       __WASI_ERRNO_NOTCAPABLE:
         doserror:=5;
       else
         doserror:=6;
     end;
     exit
   end
  else
   WasiDateToDt(Info.mtim,DT);
  PackTime(DT,Time);
End;

Procedure setftime(var f; time : longint);
Var
  DT: DateTime;
  modtime: UInt64;
  pr: RawByteString;
  fd: __wasi_fd_t;
Begin
  doserror:=0;
  UnPackTime(Time,DT);
  modtime:=DTToWasiDate(DT);
  if ConvertToFdRelativePath(textrec(f).name,fd,pr)<>0 then
    begin
      doserror:=3;
      exit;
    end;
  if __wasi_path_filestat_set_times(fd,0,PChar(pr),length(pr),0,modtime,
     __WASI_FSTFLAGS_MTIM or __WASI_FSTFLAGS_ATIM_NOW)<>__WASI_ERRNO_SUCCESS then
    doserror:=3;
End;

{******************************************************************************
                             --- Environment ---
******************************************************************************}

Function EnvCount: Longint;
var
  envcnt : longint;
  p      : ppchar;
Begin
  envcnt:=0;
  p:=envp;      {defined in system}
  if p<>nil then
    while p^<>nil do
      begin
        inc(envcnt);
        inc(p);
      end;
  EnvCount := envcnt
End;


Function EnvStr (Index: longint): String;
Var
  i : longint;
  p : ppchar;
Begin
  if (Index <= 0) or (envp=nil) then
    envstr:=''
  else
    begin
      p:=envp;      {defined in system}
      i:=1;
      while (i<Index) and (p^<>nil) do
        begin
          inc(i);
          inc(p);
        end;
      if p^=nil then
        envstr:=''
      else
        envstr:=strpas(p^)
    end;
end;


Function GetEnv(EnvVar: String): String;
var
  hp : ppchar;
  hs : string;
  eqpos : longint;
Begin
  getenv:='';
  hp:=envp;
  if hp<>nil then
    while assigned(hp^) do
      begin
        hs:=strpas(hp^);
        eqpos:=pos('=',hs);
        if copy(hs,1,eqpos-1)=envvar then
          begin
            getenv:=copy(hs,eqpos+1,length(hs)-eqpos);
            break;
          end;
        inc(hp);
      end;
End;


Procedure setfattr (var f;attr : word);
Begin
  {! No WASI equivalent !}
  { Fail for setting VolumeId }
  if (attr and VolumeID)<>0 then
   doserror:=5;
End;



{******************************************************************************
                            --- Initialization ---
******************************************************************************}

//Finalization
//  FreeDriveStr;
End.

{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,98 by Michael Van Canneyt and Peter Vreman,
    members of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit Dos;
Interface

{
  If you want to link to the C library, define crtlib.
  You can set it here, but it should be set through the makefile
}
{.$DEFINE CRTLIB}


Const
  {Max FileName Length for files}
  FileNameLen=255;

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
  ComStr  = String[FileNameLen];
  PathStr = String[FileNameLen];
  DirStr  = String[FileNameLen];
  NameStr = String[FileNameLen];
  ExtStr  = String[FileNameLen];


{$PACKRECORDS 1}
  SearchRec = Record
  {Fill : array[1..21] of byte;  Fill replaced with below}
    SearchNum  : LongInt;     {to track which search this is}
    SearchPos  : LongInt;     {directory position}
    DirPtr     : LongInt;     {directory pointer for reading directory}
    SearchType : Byte;        {0=normal, 1=open will close, 2=only 1 file}
    SearchAttr : Byte;        {attribute we are searching for}
    Fill       : Array[1..07] of Byte; {future use}
  {End of fill}
    Attr       : Byte;        {attribute of found file}
    Time       : LongInt;     {last modify date of found file}
    Size       : LongInt;     {file size of found file}
    Reserved   : Word;        {future use}
    Name       : String[FileNameLen]; {name of found file}
    SearchSpec : String[FileNameLen]; {search pattern}
    NamePos    : Word;        {end of path, start of name position}
  End;

{
  filerec.inc contains the definition of the filerec.
  textrec.inc contains the definition of the textrec.
  It is in a separate file to make it available in other units without
  having to use the DOS unit for it.
}
{$i filerec.inc}
{$i textrec.inc}

  Registers = record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax, ebx, ecx, edx, ebp, esi, edi : longint);
    End;

  DateTime = record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

Var
  DosError : integer;

{Utils}
function weekday(y,m,d : longint) : longint;
Procedure UnixDateToDt(SecsPast: LongInt; Var Dt: DateTime);
Function  DTToUnixDate(DT: DateTime): LongInt;

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
Procedure AddDisk(const path:string);
Function  DiskFree(drive: byte) : longint;
Function  DiskSize(drive: byte) : longint;
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);

{File}
Procedure GetFAttr(var f; var attr: word);
Procedure GetFTime(var f; var time: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);

{Environment}
Function  EnvCount: longint;
Function  EnvStr(index: integer): string;
Function  GetEnv (envvar: string): string;

{Do Nothing Functions, no Linux version}
Procedure Intr(intno: byte; var regs: registers);
Procedure MSDos(var regs: registers);
Procedure SwapVectors;
Procedure GetIntVec(intno: byte; var vector: pointer);
Procedure SetIntVec(intno: byte; vector: pointer);
Procedure Keep(exitcode: word);
Procedure SetFAttr(var f; attr: word);
Procedure SetFTime(var f; time: longint);
Procedure GetCBreak(var breakvalue: boolean);
Procedure SetCBreak(breakvalue: boolean);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);


Implementation

Uses
  Strings
{$ifndef crtlib}
  ,linux
{$endif}
  ;

{******************************************************************************
                           --- Link C Lib if set ---
******************************************************************************}

type
  RtlInfoType = Record
    FMode,
    FInode,
    FUid,
    FGid,
    FSize,
    FMTime : LongInt;
  End;

{$IFDEF CRTLIB}

  {Links to C library}
  Procedure _rtl_getenv(target: pchar; st: pchar); [ C ];
  Procedure _rtl_envstr(i: longint; st: pchar); [ C ];
  Function  _rtl_envcnt: longint; [ C ];
  Procedure _rtl_gettime(gt: longint); [ C ];
  Procedure _rtl_getversion(rel: pchar); [ C ];
  Function  _rtl_exec(cmdline: pchar; var exitst: integer): integer; [ C ];
  Procedure _rtl_closedir(dirptr: longint); [ C ];
  Procedure _rtl_seekdir(dirptr: longint; seekpos: longint); [ C ];
  Function  _rtl_telldir(dirptr: longint): longint; [ C ];
  Function  _rtl_opendir(path: pchar): longint; [ C ];
  Procedure _rtl_readdir(dirptr: longint; dname: pchar); [ C ];
  Procedure _rtl_stat(path: pchar; infoptr: longint); [ C ];
  Procedure _rtl_fstat(fd: longint; infoptr: longint); [ C ];

{$ENDIF CRTLIB}


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

Const
{Date Calculation}
  C1970 = 2440588;
  D0    = 1461;
  D1    = 146097;
  D2    = 1721119;
type
{$PACKRECORDS 1}
  GTRec = Record
    Year,
    Month,
    MDay,
    WDay,
    Hour,
    Minute,
    Second : Word;
  End;

Function DosVersion:Word;
Var
  Buffer : Array[0..255] of Char;
  Tmp2,
  TmpStr : String[40];
  TmpPos,
  SubRel,
  Rel    : LongInt;
  info   : utsname;
Begin
{$IFDEF CRTLIB}
  _rtl_getversion(buffer);
{$ELSE}
  UName(info);
  Move(info.release,buffer[0],40);
{$ENDIF}
  TmpStr:=StrPas(Buffer);
  SubRel:=0;
  TmpPos:=Pos('.',TmpStr);
  if TmpPos>0 then
   begin
     Tmp2:=Copy(TmpStr,TmpPos+1,40);
     Delete(TmpStr,TmpPos,40);
   end;
  TmpPos:=Pos('.',Tmp2);
  if TmpPos>0 then
   Delete(Tmp2,TmpPos,40);
  Val(TmpStr,Rel);
  Val(Tmp2,SubRel);
  DosVersion:=Rel+(SubRel shl 8);
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
{$IFDEF CRTLIB}
Var
  gt : GTRec;
{$ENDIF}
Begin
{$IFDEF CRTLIB}
  _rtl_gettime(longint(@gt));
  Year:=gt.year+1900;
  Month:=gt.month+1;
  MDay:=gt.mday;
  WDay:=gt.wday;
{$ELSE}
  Linux.GetDate(Year,Month,MDay);
  Wday:=weekday(Year,Month,MDay);
{$ENDIF}
end;



Procedure SetDate(Year, Month, Day: Word);
Begin
  {!!}
End;



Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
{$IFDEF CRTLIB}
Var
  gt : GTRec;
{$ENDIF}
Begin
{$IFDEF CRTLIB}
  _rtl_gettime(longint(@gt));
  Hour := GT.Hour;
  Minute := GT.Minute;
  Second := GT.Second;
{$ELSE}
  Linux.GetTime(Hour,Minute,Second);
{$ENDIF}
  Sec100 := 0;
end;



Procedure SetTime(Hour, Minute, Second, Sec100: Word);
Begin
  {!!}
End;



Procedure packtime(var t : datetime;var p : longint);
Begin
  p:=(t.sec shr 1)+(t.min shl 5)+(t.hour shl 11)+(t.day shl 16)+(t.month shl 21)+((t.year-1980) shl 25);
End;



Procedure unpacktime(p : longint;var t : datetime);
Begin
  t.sec:=(p and 31) shl 1;
  t.min:=(p shr 5) and 63;
  t.hour:=(p shr 11) and 31;
  t.day:=(p shr 16) and 31;
  t.month:=(p shr 21) and 15;
  t.year:=(p shr 25)+1980;
End;


Procedure UnixDateToDt(SecsPast: LongInt; Var Dt: DateTime);
Begin
  EpochToLocal(SecsPast,dt.Year,dt.Month,dt.Day,dt.Hour,dt.Min,dt.Sec);
End;



Function DTToUnixDate(DT: DateTime): LongInt;
Begin
  DTToUnixDate:=LocalToEpoch(dt.Year,dt.Month,dt.Day,dt.Hour,dt.Min,dt.Sec);
End;



{******************************************************************************
                               --- Exec ---
******************************************************************************}

var
  LastDosExitCode: word;

Procedure Exec (Const Path: PathStr; Const ComLine: ComStr);
var
{$IFDEF CRTLIB}
  Buf : Array[0..512] of Char;
  i   : Integer;
{$ELSE}  
  pid    : longint;
  status : integer;
{$ENDIF}  
Begin
{$IFDEF CRTLIB}
  i:=Length(Path);
  Move(Path[1],Buf[0],i);
  Buf[i]:=' ';
  Move(ComLine[1],Buf[i+1],Length(ComLine));
  Buf[i+Length(ComLine)+1]:=#0;
  i:=0;
  LastDosExitCode := _rtl_exec(pchar(@buf), i);
  Doserror:=i;
{$ELSE}
  pid:=Fork;
  if pid=0 then
   begin
   {The child does the actual exec, and then exits}
     Execl (Path+' '+ComLine);
   {If the execve fails, we return an exitvalue of 127, to let it be known}
     halt (127)
   end
  else
   if pid=-1 then         {Fork failed}
    begin
      DosError:=8;
      exit
    end;
{We're in the parent, let's wait.}
  Waitpid (pid,@status,0);
  if status=127 then {The child couldn't execve !!}
   DosError:=8 {We set this error, erroneously, since we cannot get to the real error}
  else
   begin
     LastDosExitCode:=status shr 8;
     DosError:=0
   end;
{$ENDIF}
End;



Function DosExitCode: Word;
Begin
  DosExitCode:=LastDosExitCode;
End;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

{
  The Diskfree and Disksize functions need a file on the specified drive, since this
  is required for the statfs system call.
  These filenames are set in drivestr[0..26], and have been preset to :
   0 - '.'      (default drive - hence current dir is ok.)
   1 - '/fd0/.'  (floppy drive 1 - should be adapted to local system )
   2 - '/fd1/.'  (floppy drive 2 - should be adapted to local system )
   3 - '/'       (C: equivalent of dos is the root partition)
   4..26          (can be set by you're own applications)
  ! Use AddDisk() to Add new drives !
  They both return -1 when a failure occurs.
}
var
  Drives   : byte;
  DriveStr : array[0..26] of pchar;

Procedure AddDisk(const path:string);
begin
  if not (DriveStr[Drives]=nil) then
   FreeMem(DriveStr[Drives],StrLen(DriveStr[Drives])+1);
  GetMem(DriveStr[Drives],length(Path)+1);
  StrPCopy(DriveStr[Drives],path);
  inc(Drives);
  if Drives>26 then
   Drives:=4;
end;



Function DiskFree(Drive: Byte): Longint;
{$IFNDEF CRTLIB}
var
  fs : statfs;
{$ENDIF}
Begin
{$IFNDEF CRTLIB}
  if (not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs) then
   Diskfree:=fs.bavail*fs.bsize
  else
   Diskfree:=-1;
{$ENDIF}
End;



Function DiskSize(Drive: Byte): Longint;
{$IFNDEF CRTLIB}
var
  fs : statfs;
{$ENDIF}
Begin
{$IFNDEF CRTLIB}
  if (not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs) then
   DiskSize:=fs.blocks*fs.bsize
  else
   DiskSize:=-1;
{$ENDIF}
End;


{******************************************************************************
                       --- Findfirst FindNext ---
******************************************************************************}

Const
  RtlFindSize = 15;
Type
  RtlFindRecType = Record
    SearchNum,
    DirPtr,
    LastUsed : LongInt;
  End;
Var
  RtlFindRecs   : Array[0..RtlFindSize-1] of RtlFindRecType;
  CurrSearchNum : LongInt;

Procedure FindClose(Var f: SearchRec);
{
  Closes dirptr if it is open
}
Var
  i : longint;
Begin
  if f.SearchType=0 then
   begin
     i:=0;
     repeat
       if (RtlFindRecs[i].SearchNum=f.SearchNum) then
        break;
       inc(i);  
     until (i>=RtlFindSize);
     If i<RtlFindSize Then
      Begin
        RtlFindRecs[i].SearchNum:=0;
        if f.dirptr>0 then
         begin
         {$IFDEF CRTLIB}   
           _rtl_closeDir(f.dirptr);
         {$ELSE}
           closedir(pdir(f.dirptr));
         {$ENDIF}  
         end;
      End;
   end;      
  f.dirptr:=0;
End;


Function FindGetFileInfo(const s:string;var f:SearchRec):boolean;
var
  DT   : DateTime;
  Info : RtlInfoType;
{$IFDEF CRTLIB}
  buf  : array[0..255] of char;
{$ELSE}  
  st   : stat;
{$ENDIF}  
begin
  FindGetFileInfo:=false;
{$IFDEF CRTLIB}
  move(s[1],buf,length(s));
  buf[length(s)]:=#0;
  _rtl_stat(@buf, LongInt(@Info));
{$ELSE}
  if not Fstat(s,st) then
   exit;
  info.FSize:=st.Size;
  info.FMTime:=st.mtime;
  if (st.mode and STAT_IFMT)=STAT_IFDIR then
   info.fmode:=$10
  else
   info.fmode:=$20;
  if (st.mode and STAT_IWUSR)=0 then
   info.fmode:=info.fmode or 1;
{$ENDIF}
  If ((Info.FMode and Not(f.searchattr))=0) Then
   Begin
     f.Name:=Copy(s,f.NamePos+1,255);
     f.Attr:=Info.FMode;
     f.Size:=Info.FSize;
     UnixDateToDT(Info.FMTime, DT);
     PackTime(DT,f.Time);
     FindGetFileInfo:=true;
   End;
end;


Function  FindLastUsed: Longint;
{
  Find unused or least recently used dirpointer slot in findrecs array
}
Var
  BestMatch,i : Longint;
  Found       : Boolean;
Begin
  BestMatch:=0;
  i:=0;
  Found:=False;
  While (i < RtlFindSize) And (Not Found) Do
   Begin
     If (RtlFindRecs[i].SearchNum = 0) Then
      Begin
        BestMatch := i;
        Found := True;
      End
     Else
      Begin
        If RtlFindRecs[i].LastUsed > RtlFindRecs[BestMatch].LastUsed Then
         BestMatch := i;
      End;
     Inc(i);
   End;
  FindLastUsed := BestMatch;
End;



Procedure FindNext(Var f: SearchRec);
{
  re-opens dir if not already in array and calls FindWorkProc
}
Var
  DirName  : Array[0..256] of Char;
  i,
  ArrayPos : Longint;
  FName,
  SName    : string;
  Found,
  Finished : boolean;
{$IFNDEF CRTLIB}
  p        : PDirEnt;
{$ENDIF}    
Begin
  If f.SearchType=0 Then
   Begin
     ArrayPos:=0;
     For i:=0 to RtlFindSize-1 Do
      Begin
        If RtlFindRecs[i].SearchNum = f.SearchNum Then
         ArrayPos:=i;
        Inc(RtlFindRecs[i].LastUsed);
      End;
     If ArrayPos=0 Then
      Begin
        If f.NamePos = 0 Then
         Begin
           DirName[0] := '.';
           DirName[1] := '/';
           DirName[2] := #0;
         End
        Else
         Begin
           Move(f.SearchSpec[1], DirName[0], f.NamePos);
           DirName[f.NamePos] := #0;
         End;
      {$IFDEF CRTLIB}
        f.DirPtr := _rtl_opendir(DirName);
      {$ELSE}
        f.DirPtr := longint(opendir(@(DirName)));
      {$ENDIF}
        If f.DirPtr > 0 Then
         begin
           ArrayPos:=FindLastUsed;
           If RtlFindRecs[ArrayPos].SearchNum > 0 Then
            Begin
            {$IFDEF CRTLIB}
              _rtl_closeDir(rtlfindrecs[arraypos].dirptr);
            {$ELSE}        
              CloseDir(pdir(rtlfindrecs[arraypos].dirptr));
            {$ENDIF}       
            End;
           RtlFindRecs[ArrayPos].SearchNum := f.SearchNum;
           RtlFindRecs[ArrayPos].DirPtr := f.DirPtr;
           if f.searchpos>0 then
            begin
            {$IFDEF CRTLIB}
              _rtl_seekdir(f.dirptr, f.searchpos);
            {$ELSE}   
              seekdir(pdir(f.dirptr), f.searchpos);
            {$ENDIF}  
            end;
         end;
      End;
     RtlFindRecs[ArrayPos].LastUsed:=0;
   end;
{Main loop}   
  SName:=Copy(f.SearchSpec,f.NamePos+1,255);
  Found:=False;
  Finished:=(f.dirptr=0);
  While Not Finished Do
   Begin
   {$IFDEF CRTLIB}
     _rtl_readdir(f.dirptr, @FBuf);
     FName:=StrPas(FBuf[0]);
   {$ELSE}
     p:=readdir(pdir(f.dirptr));
     if p=nil then
      FName:=''
     else
      FName:=Strpas(@p^.name);
   {$ENDIF}
     If FName='' Then
      Finished:=True
     Else
      Begin
        If FNMatch(SName,FName) Then
         Begin
           Found:=FindGetFileInfo(Copy(f.SearchSpec,1,f.NamePos)+FName,f);
           if Found then
            Finished:=true;
         End;
      End;
   End;
{Shutdown}   
  If Found Then
   Begin
   {$IFDEF CRTLIB}
     f.searchpos:=_rtl_telldir(f.dirptr);
   {$ELSE}
     f.searchpos:=telldir(pdir(f.dirptr));
   {$ENDIF}
     DosError:=0;
   End
  Else
   Begin
     FindClose(f);
     DosError:=18;
   End;
End;


Procedure FindFirst(Const Path: PathStr; Attr: Word; Var f: SearchRec);
{
  opens dir and calls FindWorkProc
}
Begin
  if Path='' then
   begin
     DosError:=3;
     exit;
   end;     
{Create Info}   
  f.SearchSpec := Path;
  f.SearchAttr := Attr;
  f.NamePos := Length(f.SearchSpec);
  while (f.NamePos>0) and (f.SearchSpec[f.NamePos]<>'/') do
   dec(f.NamePos);
{Wildcards?}   
  if (Pos('?',Path)=0)  and (Pos('*',Path)=0) then
   begin
     if FindGetFileInfo(Path,f) then
      DosError:=0
     else
      begin
        if ErrNo=Sys_ENOENT then
         DosError:=3
        else 
         DosError:=18; 
      end;       
     f.DirPtr:=0;
     f.SearchType:=1;
     f.searchnum:=-1;
   end
  else
{Find Entry}
   begin 
     Inc(CurrSearchNum);
     f.SearchNum:=CurrSearchNum;
     f.SearchType:=0;
     FindNext(f);
   end;     
End;


{******************************************************************************
                               --- File ---
******************************************************************************}

Procedure FSplit(Path: PathStr; Var Dir: DirStr; Var Name: NameStr;Var Ext: ExtStr);
Begin
  Linux.FSplit(Path,Dir,Name,Ext);
End;



Function FExpand(Const Path: PathStr): PathStr;
Begin
  FExpand:=Linux.FExpand(Path);
End;



Function FSearch(path : pathstr;dirlist : string) : pathstr;
Begin
  FSearch:=Linux.FSearch(path,dirlist);
End;



Procedure GetFAttr(var f; var attr : word);
Var
{$IFDEF CRTLIB}
  Info: RtlInfoType;
{$ELSE}
  info : stat;
{$ENDIF}
  LinAttr : longint;
Begin
{$IFDEF CRTLIB}
  _rtl_fstat(word(f), longint(@Info));
  attr := info.fmode;
{$ELSE}
  if not FStat(strpas(@textrec(f).name),info) then
   begin
     Attr:=0;
     DosError:=3;
     exit;
   end
  else
   LinAttr:=Info.Mode;
  if S_ISDIR(LinAttr) then
   Attr:=$10
  else 
   Attr:=$20;
  if not Access(strpas(@textrec(f).name),W_OK) then
   Attr:=Attr or $1;
  if (not S_ISDIR(LinAttr)) and (filerec(f).name[0]='.')  then
   Attr:=Attr or $2;
{$Endif}
end;



Procedure getftime (var f; var time : longint);
Var
{$IFDEF CRTLIB}
  Info: RtlInfoType;
{$ELSE}
  info : stat;
{$ENDIF}
  DT: DateTime;
Begin
  doserror:=0;
{$IFDEF CRTLIB}
  _rtl_fstat(word(f), longint(@Info));
  UnixDateToDT(Info.FMTime, DT);
{$ELSE}
  if not fstat(filerec(f).handle,info) then
   begin
     Time:=0;
     doserror:=3;
     exit
   end
  else
   UnixDateToDT(Info.mTime,DT);
{$ENDIF}
  PackTime(DT,Time);
End;



{******************************************************************************
                             --- Environment ---
******************************************************************************}

Function EnvCount: Longint;
var
  envcnt : longint;
  p      : ppchar;
Begin
{$IFDEF CRTLIB}
  EnvCount := _rtl_envcnt;
{$ELSE}
  envcnt:=0;
  p:=envp;      {defined in syslinux}
  while (p^<>nil) do
   begin
     inc(envcnt);
     p:=p+4
   end;
  EnvCount := envcnt
{$ENDIF}
End;



Function EnvStr(Index: Integer): String;
Var
{$IFDEF CRTLIB}
  Buffer: Array[0..255] of Char;
{$ELSE}
  i : longint;
  p : ppchar;
{$ENDIF}
Begin
{$IFDEF CRTLIB}
  Buffer[0]:=#0;        {Be sure there is at least nothing}
  _rtl_envstr(index, buffer);
  EnvStr:=StrPas(Buffer);
{$ELSE}
  p:=envp;      {defined in syslinux}
  i:=1;
  while (i<Index) and (p^<>nil) do
   begin
     inc(i);
     p:=p+4;
   end;
  if p=nil then
   envstr:=''
  else
   envstr:=strpas(p^)
{$ENDIF}
End;



Function GetEnv(EnvVar: String): String;
var
{$IFDEF CRTLIB}
  Buffer,
  OutStr : Array[0..255] of Char;
{$ELSE}
  p     : pchar;
{$ENDIF}
Begin
{$IFDEF CRTLIB}
  Move(EnvVar[1],Buffer,Length(EnvVar));
  Buffer[Length(EnvVar)]:=#0;
  OutStr[0]:=#0;
  _rtl_getenv(buffer,outstr);
  GetEnv:=StrPas(Buffer);
{$ELSE}
  p:=Linux.GetEnv(EnvVar);
  if p=nil then
   GetEnv:=''
  else
   GetEnv:=StrPas(p);
{$ENDIF}
End;


{******************************************************************************
                      --- Do Nothing Procedures/Functions ---
******************************************************************************}

Procedure Intr (intno: byte; var regs: registers);
Begin
  {! No Linux equivalent !}
End;



Procedure msdos(var regs : registers);
Begin
  {! No Linux equivalent !}
End;



Procedure getintvec(intno : byte;var vector : pointer);
Begin
  {! No Linux equivalent !}
End;



Procedure setintvec(intno : byte;vector : pointer);
Begin
  {! No Linux equivalent !}
End;



Procedure SwapVectors;
Begin
  {! No Linux equivalent !}
End;



Procedure keep(exitcode : word);
Begin
  {! No Linux equivalent !}
End;



Procedure setftime(var f; time : longint);
Begin
  {! No Linux equivalent !}
End;



Procedure setfattr (var f;attr : word);
Begin
  {! No Linux equivalent !}
End;



Procedure GetCBreak(Var BreakValue: Boolean);
Begin
{! No Linux equivalent !}
  breakvalue:=true
End;



Procedure SetCBreak(BreakValue: Boolean);
Begin
  {! No Linux equivalent !}
End;



Procedure GetVerify(Var Verify: Boolean);
Begin
  {! No Linux equivalent !}
  Verify:=true;
End;



Procedure SetVerify(Verify: Boolean);
Begin
  {! No Linux equivalent !}
End;


{******************************************************************************
                            --- Initialization ---
******************************************************************************}

Begin
{$IFNDEF CRTLIB}
{drivestr needs initialisation}
  AddDisk('.');
  AddDisk('/fd0/.');
  AddDisk('/fd1/.');
  AddDisk('/.');
{$ENDIF}
End.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.11  1998/03/10 14:46:09  michael
  + better checking in weekday function

  Revision 1.10  1998/01/27 17:47:42  peter
    * Speeeeedup of Findfirst with no wildcards

  Revision 1.9  1998/01/26 12:01:28  michael
  + Added log at the end
  
  Working file: rtl/linux/dos.pp
  description:
  ----------------------------
  revision 1.8
  date: 1998/01/19 16:21:33;  author: peter;  state: Exp;  lines: +58 -117
  + Weekday from mailinglist which is a lot smaller and faster
  * FExpand now uses the FExpand from linux.pp
  ----------------------------
  revision 1.7
  date: 1998/01/19 10:03:00;  author: michael;  state: Exp;  lines: +6 -1
  * BugFix for findfirst/findnext routines. (From Peter Vreman)
  ----------------------------
  revision 1.6
  date: 1998/01/13 17:14:37;  author: michael;  state: Exp;  lines: +3 -3
  + Entered new FStat call using File or Text var.
  + GetTme call in DOS now refers to Linux.gettime !
  ----------------------------
  revision 1.5
  date: 1998/01/09 13:12:38;  author: michael;  state: Exp;  lines: +20 -20
  * Fixed some bugs that showed when writing examples (From Peter Vreman)
  ----------------------------
  revision 1.4
  date: 1997/12/10 12:22:26;  author: michael;  state: Exp;  lines: +2 -2
  + changed longint(f) to word(f) in getfattr;
  ----------------------------
  revision 1.3
  date: 1997/12/04 13:43:50;  author: michael;  state: Exp;  lines: +23 -15
  * changed attribute and time functions.
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:31:14;  author: michael;  state: Exp;  lines: +14 -21
  + Added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:54;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:54;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
   Date          Version          Who         Comments
   1996          0.8              Michael     Initial implementation
   11/97         0.9              Peter Vreman <pfv@worldonline.nl>
                                              Unit now depends on the
                                              linux unit only.
                                              Cleaned up code.
}

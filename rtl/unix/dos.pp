{
    $Id$
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
  {Max FileName Length for files}
  FileNameLen=255;

Type

  SearchRec =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    Record
  {Fill : array[1..21] of byte;  Fill replaced with below}
    SearchNum  : LongInt;     {to track which search this is}
    SearchPos  : LongInt;     {directory position}
    DirPtr     : Pointer;     {directory pointer for reading directory}
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


{$ifdef cpui386}
  Registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax, ebx, ecx, edx, ebp, esi, edi : longint);
    End;
{$endif cpui386}

{$i dosh.inc}

{Extra Utils}
function weekday(y,m,d : longint) : longint;
Procedure UnixDateToDt(SecsPast: LongInt; Var Dt: DateTime);
Function  DTToUnixDate(DT: DateTime): LongInt;

{Disk}
Procedure AddDisk(const path:string);

Implementation

Uses
  Strings,UnixUtil,Unix,BaseUnix,UnixType;

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
  GTRec = packed Record
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
  FPUName(info);
  Move(info.release,buffer[0],40);
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
Begin
  Unix.GetDate(Year,Month,MDay);
  Wday:=weekday(Year,Month,MDay);
end;



Procedure SetDate(Year, Month, Day: Word);
Begin
  Unix.SetDate ( Year, Month, Day );
End;



Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
Begin
  Unix.GetTime(Hour,Minute,Second,Sec100);
end;



Procedure SetTime(Hour, Minute, Second, Sec100: Word);
Begin
  Unix.SetTime ( Hour, Minute, Second );
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

{$ifdef HASTHREADVAR}
{$ifdef VER1_9_2}
var
{$else VER1_9_2}
threadvar
{$endif VER1_9_2}
{$else HASTHREADVAR}
var
{$endif HASTHREADVAR}
  LastDosExitCode: word;

Procedure Exec (Const Path: PathStr; Const ComLine: ComStr);
var
  pid    : longint;
  // The Error-Checking in the previous Version failed, since halt($7F) gives an WaitPid-status of $7F00
Begin
  LastDosExitCode:=0;
  pid:=fpFork;
  if pid=0 then
   begin
   {The child does the actual exec, and then exits}
     if ComLine='' then
      Execl(Path)
     else
      Execl(Path+' '+ComLine);
   {If the execve fails, we return an exitvalue of 127, to let it be known}
     fpExit(127);
   end
  else
   if pid=-1 then         {Fork failed}
    begin
      DosError:=8;
      exit
    end;
{We're in the parent, let's wait.}
  LastDosExitCode:=WaitProcess(pid); // WaitPid and result-convert
  if (LastDosExitCode>=0) and (LastDosExitCode<>127) then
   DosError:=0
  else
   DosError:=8; // perhaps one time give an better error
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



Function DiskFree(Drive: Byte): int64;
var
  fs : tstatfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (StatFS(fixdrivestr[drive],fs)<>-1)) or
     ((not (drivestr[Drive]=nil)) and (StatFS(drivestr[drive],fs)<>-1)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;
End;



Function DiskSize(Drive: Byte): int64;
var
  fs : tstatfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (StatFS(fixdrivestr[drive],fs)<>-1)) or
     ((not (drivestr[Drive]=nil)) and (StatFS(drivestr[drive],fs)<>-1)) then
   DiskSize:=int64(fs.blocks)*int64(fs.bsize)
  else
   DiskSize:=-1;
End;


{******************************************************************************
                       --- Findfirst FindNext ---
******************************************************************************}

Const
  RtlFindSize = 15;
Type
  RtlFindRecType = Record
    DirPtr   : Pointer;
    SearchNum,
    LastUsed : LongInt;
  End;
Var
  RtlFindRecs   : Array[1..RtlFindSize] of RtlFindRecType;
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
     i:=1;
     repeat
       if (RtlFindRecs[i].SearchNum=f.SearchNum) then
        break;
       inc(i);
     until (i>RtlFindSize);
     If i<=RtlFindSize Then
      Begin
        RtlFindRecs[i].SearchNum:=0;
        if f.dirptr<>nil then
         fpclosedir(pdir(f.dirptr)^);
      End;
   end;
  f.dirptr:=nil;
End;


Function FindGetFileInfo(const s:string;var f:SearchRec):boolean;
var
  DT   : DateTime;
  Info : RtlInfoType;
  st   : baseunix.stat;
begin
  FindGetFileInfo:=false;
  if not fpstat(s,st)>=0 then
   exit;
  info.FSize:=st.st_Size;
  info.FMTime:=st.st_mtime;
  if (st.st_mode and STAT_IFMT)=STAT_IFDIR then
   info.fmode:=$10
  else
   info.fmode:=$0;
  if (st.st_mode and STAT_IWUSR)=0 then
   info.fmode:=info.fmode or 1;
  if s[f.NamePos+1]='.' then
   info.fmode:=info.fmode or $2;

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
  BestMatch:=1;
  i:=1;
  Found:=False;
  While (i <= RtlFindSize) And (Not Found) Do
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
  p        : PDirEnt;
Begin
  If f.SearchType=0 Then
   Begin
     ArrayPos:=0;
     For i:=1 to RtlFindSize Do
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
        f.DirPtr := fpopendir(@(DirName));
        If f.DirPtr <> nil Then
         begin
           ArrayPos:=FindLastUsed;
           If RtlFindRecs[ArrayPos].SearchNum > 0 Then
            FpCloseDir((pdir(rtlfindrecs[arraypos].dirptr)^));
           RtlFindRecs[ArrayPos].SearchNum := f.SearchNum;
           RtlFindRecs[ArrayPos].DirPtr := f.DirPtr;
           if f.searchpos>0 then
            seekdir(pdir(f.dirptr), f.searchpos);
         end;
      End;
     if ArrayPos>0 then
       RtlFindRecs[ArrayPos].LastUsed:=0;
   end;
{Main loop}
  SName:=Copy(f.SearchSpec,f.NamePos+1,255);
  Found:=False;
  Finished:=(f.dirptr=nil);
  While Not Finished Do
   Begin
     p:=fpreaddir(pdir(f.dirptr)^);
     if p=nil then
      FName:=''
     else
      FName:=Strpas(@p^.d_name);
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
     f.searchpos:=telldir(pdir(f.dirptr));
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
  fillchar(f,sizeof(f),0);
  if Path='' then
   begin
     DosError:=3;
     exit;
   end;
{Create Info}
  f.SearchSpec := Path;
  {We always also search for readonly and archive, regardless of Attr:}
  f.SearchAttr := Attr or archive or readonly;
  f.SearchPos  := 0;
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
        { According to tdos2 test it should return 18
        if ErrNo=Sys_ENOENT then
         DosError:=3
        else }
         DosError:=18;
      end;
     f.DirPtr:=nil;
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
  UnixUtil.FSplit(Path,Dir,Name,Ext);
End;



Function FExpand(Const Path: PathStr): PathStr;
Begin
  FExpand:=Unix.FExpand(Path);
End;


Function FSearch(path : pathstr;dirlist : string) : pathstr;
Var
  info : BaseUnix.stat;
Begin
  if (length(Path)>0) and (path[1]='/') and (fpStat(path,info)>=0) then
    FSearch:=path
  else
    FSearch:=Unix.FSearch(path,dirlist);
End;

Procedure GetFAttr(var f; var attr : word);
Var
  info    : baseunix.stat;
  LinAttr : longint;
Begin
  DosError:=0;
  if FPStat(@textrec(f).name,info)<0 then
   begin
     Attr:=0;
     DosError:=3;
     exit;
   end
  else
   LinAttr:=Info.st_Mode;
  if fpS_ISDIR(LinAttr) then
   Attr:=$10
  else
   Attr:=$0;
  if fpAccess(@textrec(f).name,W_OK)<0 then
   Attr:=Attr or $1;
  if filerec(f).name[0]='.' then
   Attr:=Attr or $2;
end;

Procedure getftime (var f; var time : longint);
Var
  Info: baseunix.stat;
  DT: DateTime;
Begin
  doserror:=0;
  if fpfstat(filerec(f).handle,info)<0 then
   begin
     Time:=0;
     doserror:=6;
     exit
   end
  else
   UnixDateToDT(Info.st_mTime,DT);
  PackTime(DT,Time);
End;

Procedure setftime(var f; time : longint);

Var
  utim: utimbuf;
  DT: DateTime;
  index: Integer;

Begin
  doserror:=0;
  with utim do
    begin
      actime:=getepochtime;
      UnPackTime(Time,DT);
      modtime:=DTToUnixDate(DT);
    end;
  if fputime(@filerec(f).name,@utim)<0 then
    begin
      Time:=0;
      doserror:=3;
    end;
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
  p:=envp;      {defined in syslinux}
  while (p^<>nil) do
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
  if Index <= 0 then
    envstr:=''
  else
    begin
      p:=envp;      {defined in syslinux}
      i:=1;
      while (i<Index) and (p^<>nil) do
        begin
          inc(i);
          inc(p);
        end;
      if p=nil then
        envstr:=''
      else
        envstr:=strpas(p^)
    end;
end;


Function GetEnv(EnvVar: String): String;
var
  p     : pchar;
Begin
  p:=BaseUnix.fpGetEnv(EnvVar);
  if p=nil then
   GetEnv:=''
  else
   GetEnv:=StrPas(p);
End;


{******************************************************************************
                      --- Do Nothing Procedures/Functions ---
******************************************************************************}

{$ifdef cpui386}
Procedure Intr (intno: byte; var regs: registers);
Begin
  {! No Unix equivalent !}
End;



Procedure msdos(var regs : registers);
Begin
  {! No Unix equivalent !}
End;
{$endif cpui386}



Procedure getintvec(intno : byte;var vector : pointer);
Begin
  {! No Unix equivalent !}
End;



Procedure setintvec(intno : byte;vector : pointer);
Begin
  {! No Unix equivalent !}
End;



Procedure SwapVectors;
Begin
  {! No Unix equivalent !}
End;



Procedure keep(exitcode : word);
Begin
  {! No Unix equivalent !}
End;



Procedure setfattr (var f;attr : word);
Begin
  {! No Unix equivalent !}
  { Fail for setting VolumeId }
  if (attr and VolumeID)<>0 then
   doserror:=5;
End;



Procedure GetCBreak(Var BreakValue: Boolean);
Begin
{! No Unix equivalent !}
  breakvalue:=true
End;



Procedure SetCBreak(BreakValue: Boolean);
Begin
  {! No Unix equivalent !}
End;



Procedure GetVerify(Var Verify: Boolean);
Begin
  {! No Unix equivalent !}
  Verify:=true;
End;



Procedure SetVerify(Verify: Boolean);
Begin
  {! No Unix equivalent !}
End;


function  GetShortName(var p : String) : boolean;

begin
 { short=long under *nix}
 GetShortName:=True;
end;

function  GetLongName(var p : String) : boolean;
begin
  { short=long under *nix}
 GetLongName:=True;
end;


{******************************************************************************
                            --- Initialization ---
******************************************************************************}

End.

{
  $Log$
  Revision 1.35  2004-09-25 15:09:57  peter
    * remove strpas() before syscalls so it chooses the pchar overload

  Revision 1.34  2004/08/14 14:22:17  florian
    * alignment for sparc fixed

  Revision 1.33  2004/07/25 22:46:34  olle
    * envstr now returns empty string when index out of bounds

  Revision 1.32  2004/03/14 18:42:39  peter
    * reset searchrec info in findfirst

  Revision 1.31  2004/03/04 22:15:16  marco
   * UnixType changes. Please report problems to me.

  Revision 1.30  2004/02/18 22:00:45  peter
    * dirptr changed to pointer

  Revision 1.29  2004/02/18 19:08:27  florian
    * fixed bootstrapping with 1.9.2

  Revision 1.28  2004/02/17 17:37:26  daniel
    * Enable threadvars again

  Revision 1.27  2004/02/16 22:18:44  hajny
    * LastDosExitCode changed back from threadvar temporarily

  Revision 1.26  2004/02/15 21:36:10  hajny
    * overloaded ExecuteProcess added, EnvStr param changed to longint

  Revision 1.25  2004/02/09 17:01:28  marco
   * fixes to get it working under FreeBSD, and probably Linux too

  Revision 1.24  2004/02/09 12:03:16  michael
  + Switched to single interface in dosh.inc

  Revision 1.23  2004/01/31 16:15:14  florian
    * packing of searchrec for arm fixed

  Revision 1.22  2003/12/29 21:15:04  jonas
    * fixed setftime (sorry Marco :)

  Revision 1.21  2003/12/03 20:17:03  olle
    * files are not pretended to have attr ARCHIVED anymore
    + FindFirst etc now also filters on attr HIDDEN
    * files with attr READONLY and ARCHIVE are always returned by FindFirst etc

  Revision 1.19  2003/10/17 22:13:30  olle
    * changed i386 to cpui386

  Revision 1.18  2003/09/27 12:51:33  peter
    * fpISxxx macros renamed to C compliant fpS_ISxxx

  Revision 1.17  2003/09/17 17:30:46  marco
   * Introduction of unixutil

  Revision 1.16  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.15  2003/05/16 20:56:06  florian
  no message

  Revision 1.14  2003/05/14 13:51:03  florian
    * ifdef'd code which i386 specific

  Revision 1.13  2002/12/08 16:05:34  peter
    * small error code fixes so tdos2 passes

  Revision 1.12  2002/09/07 16:01:27  peter
    * old logs removed and tabs fixed

}

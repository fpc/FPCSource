{
    $Id: sysutils.pp,v 1.59 2005/03/25 22:53:39 jonas Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysutils;
interface

{$MODE objfpc}
{ force ansistrings }
{$H+}

{$DEFINE HAS_SLEEP}
{$DEFINE HAS_OSERROR}
{$DEFINE HAS_OSCONFIG}
{$DEFINE HAS_TEMPDIR}
{$DEFINE HASUNIX}

uses
  Unix,errors,sysconst,Unixtype;

{ Include platform independent interface part }
{$i sysutilh.inc}

Procedure AddDisk(const path:string);


implementation

Uses
  {$ifdef FPC_USE_LIBC}initc{$ELSE}Syscall{$ENDIF}, Baseunix;

{$Define OS_FILEISREADONLY} // Specific implementation for Unix.

Function getenv(name:shortstring):Pchar; external name 'FPC_SYSC_FPGETENV';

Type
  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];


{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
{$DEFINE FPC_FEXPAND_GETENVPCHAR} { GetEnv result is a PChar }

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_GETENVPCHAR}
{$UNDEF FPC_FEXPAND_TILDE}

{ Include platform independent implementation part }
{$i sysutils.inc}

Const
{Date Translation}
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;


Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:Word);
Var
  YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp:=((JulianDN-D2) shl 2)-1;
  JulianDN:=Temp Div D1;
  XYear:=(Temp Mod D1) or 3;
  YYear:=(XYear Div D0);
  Temp:=((((XYear mod D0)+4) shr 2)*5)-3;
  Day:=((Temp Mod 153)+5) Div 5;
  TempMonth:=Temp Div 153;
  If TempMonth>=10 Then
   Begin
     inc(YYear);
     dec(TempMonth,12);
   End;
  inc(TempMonth,3);
  Month := TempMonth;
  Year:=YYear+(JulianDN*100);
end;



Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
{
  Transforms Epoch time into local time (hour, minute,seconds)
}
Var
  DateNum: LongInt;
Begin
  inc(Epoch,TZSeconds);
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Abs(Epoch Mod 86400);
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;




{****************************************************************************
                              File Functions
****************************************************************************}



Procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
Var
  DotPos,SlashPos,i : longint;
Begin
  SlashPos:=0;
  DotPos:=256;
  i:=Length(Path);
  While (i>0) and (SlashPos=0) Do
   Begin
     If (DotPos=256) and (Path[i]='.') Then
      begin
        DotPos:=i;
      end;
     If (Path[i]='/') Then
      SlashPos:=i;
     Dec(i);
   End;
  Ext:=Copy(Path,DotPos,255);
  Dir:=Copy(Path,1,SlashPos);
  Name:=Copy(Path,SlashPos + 1,DotPos - SlashPos - 1);
End;


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or O_RdOnly;
    1 : LinuxFlags:=LinuxFlags or O_WrOnly;
    2 : LinuxFlags:=LinuxFlags or O_RdWr;
  end;
  FileOpen:=fpOpen (FileName,LinuxFlags);
  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=fpOpen(FileName,O_RdWr or O_Creat or O_Trunc);
end;


Function FileCreate (Const FileName : String;Mode : Longint) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or O_RdOnly;
    1 : LinuxFlags:=LinuxFlags or O_WrOnly;
    2 : LinuxFlags:=LinuxFlags or O_RdWr;
  end;
  FileCreate:=fpOpen(FileName,LinuxFlags or O_Creat or O_Trunc);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  FileRead:=fpRead (Handle,Buffer,Count);
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  FileWrite:=fpWrite (Handle,Buffer,Count);
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  FileSeek:=fplSeek (Handle,FOffset,Origin);
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;

begin
  {$warning need to add 64bit call }
  FileSeek:=fplSeek (Handle,FOffset,Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  fpclose(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  FileTruncate:=fpftruncate(Handle,Size)>=0;
end;

Function UnixToWinAge(UnixAge : time_t): Longint;

Var
  Y,M,D,hh,mm,ss : word;

begin
  EpochToLocal(UnixAge,y,m,d,hh,mm,ss);
  Result:=DateTimeToFileDate(EncodeDate(y,m,d)+EncodeTime(hh,mm,ss,0));
end;


Function FileAge (Const FileName : String): Longint;

Var Info : Stat;

begin
  If  fpstat (FileName,Info)<0 then
    exit(-1)
  else
    Result:=UnixToWinAge(info.st_mtime);
end;


Function FileExists (Const FileName : String) : Boolean;

Var Info : Stat;

begin
  FileExists:=fpstat(filename,Info)>=0;
end;


Function DirectoryExists (Const Directory : String) : Boolean;

Var Info : Stat;

begin
  DirectoryExists:=(fpstat(Directory,Info)>=0) and fpS_ISDIR(Info.st_mode);
end;


Function LinuxToWinAttr (FN : Pchar; Const Info : Stat) : Longint;

begin
  Result:=faArchive;
  If fpS_ISDIR(Info.st_mode) then
    Result:=Result or faDirectory;
  If (FN[0]='.') and (not (FN[1] in [#0,'.']))  then
    Result:=Result or faHidden;
  If (Info.st_Mode and S_IWUSR)=0 Then
     Result:=Result or faReadOnly;
  If fpS_ISSOCK(Info.st_mode) or fpS_ISBLK(Info.st_mode) or fpS_ISCHR(Info.st_mode) or fpS_ISFIFO(Info.st_mode) Then
     Result:=Result or faSysFile;
end;

type

 pglob = ^tglob;
  tglob = record
    name : pchar;
    next : pglob;
  end;

Function Dirname(Const path:pathstr):pathstr;
{
  This function returns the directory part of a complete path.
  Unless the directory is root '/', The last character is not
  a slash.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if length(Dir)>1 then
   Delete(Dir,length(Dir),1);
  DirName:=Dir;
end;


Function Basename(Const path:pathstr;Const suf:pathstr):pathstr;
{
  This function returns the filename part of a complete path. If suf is
  supplied, it is cut off the filename.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if Suf<>Ext then
   Name:=Name+Ext;
  BaseName:=Name;
end;


Function FNMatch(const Pattern,Name:shortstring):Boolean;
Var
  LenPat,LenName : longint;

  Function DoFNMatch(i,j:longint):Boolean;
  Var
    Found : boolean;
  Begin
  Found:=true;
  While Found and (i<=LenPat) Do
   Begin
     Case Pattern[i] of
      '?' : Found:=(j<=LenName);
      '*' : Begin
            {find the next character in pattern, different of ? and *}
              while Found do
                begin
                inc(i);
                if i>LenPat then Break;
                case Pattern[i] of
                  '*' : ;
                  '?' : begin
                          if j>LenName then begin DoFNMatch:=false; Exit; end;
                          inc(j);
                        end;
                else
                  Found:=false;
                end;
               end;
              Assert((i>LenPat) or ( (Pattern[i]<>'*') and (Pattern[i]<>'?') ));
            {Now, find in name the character which i points to, if the * or ?
             wasn't the last character in the pattern, else, use up all the
             chars in name}
              Found:=false;
              if (i<=LenPat) then
              begin
                repeat
                  {find a letter (not only first !) which maches pattern[i]}
                  while (j<=LenName) and (name[j]<>pattern[i]) do
                    inc (j);
                  if (j<LenName) then
                  begin
                    if DoFnMatch(i+1,j+1) then
                    begin
                      i:=LenPat;
                      j:=LenName;{we can stop}
                      Found:=true;
                      Break;
                    end else
                      inc(j);{We didn't find one, need to look further}
                  end else
                  if j=LenName then
                  begin
                    Found:=true;
                    Break;
                  end;
                  { This 'until' condition must be j>LenName, not j>=LenName.
                    That's because when we 'need to look further' and
                    j = LenName then loop must not terminate. }
                until (j>LenName);
              end else
              begin
                j:=LenName;{we can stop}
                Found:=true;
              end;
            end;
     else {not a wildcard character in pattern}
       Found:=(j<=LenName) and (pattern[i]=name[j]);
     end;
     inc(i);
     inc(j);
   end;
  DoFnMatch:=Found and (j>LenName);
  end;

Begin {start FNMatch}
  LenPat:=Length(Pattern);
  LenName:=Length(Name);
  FNMatch:=DoFNMatch(1,1);
End;


Procedure Globfree(var p : pglob);
{
  Release memory occupied by pglob structure, and names in it.
  sets p to nil.
}
var
  temp : pglob;
begin
  while assigned(p) do
   begin
     temp:=p^.next;
     if assigned(p^.name) then
      freemem(p^.name);
     dispose(p);
     p:=temp;
   end;
end;


Function Glob(Const path:pathstr):pglob;
{
  Fills a tglob structure with entries matching path,
  and returns a pointer to it. Returns nil on error,
  linuxerror is set accordingly.
}
var
  temp,
  temp2   : string[255];
  thedir  : pdir;
  buffer  : pdirent;
  root,
  current : pglob;
begin
{ Get directory }
  temp:=dirname(path);
  if temp='' then
   temp:='.';
  temp:=temp+#0;
  thedir:=fpopendir(@temp[1]);
  if thedir=nil then
    exit(nil);
  temp:=basename(path,''); { get the pattern }
  if thedir^.dd_fd<0 then
     exit(nil);
{get the entries}
  root:=nil;
  current:=nil;
  repeat
    buffer:=fpreaddir(thedir^);
    if buffer=nil then
     break;
    temp2:=strpas(@(buffer^.d_name[0]));
    if fnmatch(temp,temp2) then
     begin
       if root=nil then
        begin
          new(root);
          current:=root;
        end
       else
        begin
          new(current^.next);
          current:=current^.next;
        end;
       if current=nil then
        begin
           fpseterrno(ESysENOMEM);
          globfree(root);
          break;
        end;
       current^.next:=nil;
       getmem(current^.name,length(temp2)+1);
       if current^.name=nil then
        begin
          fpseterrno(ESysENOMEM);
          globfree(root);
          break;
        end;
       move(buffer^.d_name[0],current^.name^,length(temp2)+1);
     end;
  until false;
  fpclosedir(thedir^);
  glob:=root;
end;


{
 GlobToSearch takes a glob entry, stats the file.
 The glob entry is removed.
 If FileAttributes match, the entry is reused
}

Type
  TGlobSearchRec = Record
    Path       : shortString;
    GlobHandle : PGlob;
  end;
  PGlobSearchRec = ^TGlobSearchRec;

Function GlobToTSearchRec (Var Info : TSearchRec) : Boolean;

Var SInfo : Stat;
    p     : Pglob;
    GlobSearchRec : PGlobSearchrec;

begin
  GlobSearchRec:=Info.FindHandle;
  P:=GlobSearchRec^.GlobHandle;
  Result:=P<>Nil;
  If Result then
    begin
    GlobSearchRec^.GlobHandle:=P^.Next;
    Result:=Fpstat(GlobSearchRec^.Path+StrPas(p^.name),SInfo)>=0;
    If Result then
      begin
      Info.Attr:=LinuxToWinAttr(p^.name,SInfo);
      Result:=(Info.ExcludeAttr and Info.Attr)=0;
      If Result Then
         With Info do
           begin
           Attr:=Info.Attr;
           If P^.Name<>Nil then
           Name:=strpas(p^.name);
           Time:=UnixToWinAge(Sinfo.st_mtime);
           Size:=Sinfo.st_Size;
           Mode:=Sinfo.st_mode;
           end;
      end;
    P^.Next:=Nil;
    GlobFree(P);
    end;
end;

Function DoFind(Var Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  Result:=-1;
  GlobSearchRec:=Rslt.FindHandle;
  If (GlobSearchRec^.GlobHandle<>Nil) then
    While (GlobSearchRec^.GlobHandle<>Nil) and not (Result=0) do
      If GlobToTSearchRec(Rslt) Then Result:=0;
end;



Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  New(GlobSearchRec);
  GlobSearchRec^.Path:=ExpandFileName(ExtractFilePath(Path));
  GlobSearchRec^.GlobHandle:=Glob(Path);
  Rslt.ExcludeAttr:=Not Attr and (faHidden or faSysFile or faVolumeID or faDirectory); //!! Not correct !!
  Rslt.FindHandle:=GlobSearchRec;
  Result:=DoFind (Rslt);
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  Result:=DoFind (Rslt);
end;


Procedure FindClose (Var F : TSearchrec);

Var
  GlobSearchRec : PGlobSearchRec;

begin
  GlobSearchRec:=F.FindHandle;
  GlobFree (GlobSearchRec^.GlobHandle);
  Dispose(GlobSearchRec);
end;


Function FileGetDate (Handle : Longint) : Longint;

Var Info : Stat;

begin
  If (fpFStat(Handle,Info))<0 then
    Result:=-1
  else
    Result:=Info.st_Mtime;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // Impossible under Linux from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

Var Info : Stat;

begin
  If  FpStat (FileName,Info)<0 then
    Result:=-1
  Else
    Result:=LinuxToWinAttr(Pchar(FileName),Info);
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:=fpUnLink (FileName)>=0;
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=BaseUnix.FpRename(OldNAme,NewName)>=0;
end;

Function FileIsReadOnly(const FileName: String): Boolean;

begin
  Result := fpAccess(PChar(FileName),W_OK)<>0;
end;

{****************************************************************************
                              Disk Functions
****************************************************************************}

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
var
  Drives   : byte;
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
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (statfs(StrPas(fixdrivestr[drive]),fs)<>-1)) or
     ((not (drivestr[Drive]=nil)) and (statfs(StrPas(drivestr[drive]),fs)<>-1)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;
End;



Function DiskSize(Drive: Byte): int64;
var
  fs : tstatfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (statfs(StrPas(fixdrivestr[drive]),fs)<>-1)) or
     ((not (drivestr[Drive]=nil)) and (statfs(StrPas(drivestr[drive]),fs)<>-1)) then
   DiskSize:=int64(fs.blocks)*int64(fs.bsize)
  else
   DiskSize:=-1;
End;


Function GetCurrentDir : String;
begin
  GetDir (0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   ChDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   MkDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
   RmDir(Dir);
  {$I+}
  result := (IOResult = 0);
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}


Function GetEpochTime: cint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
begin
  GetEpochTime:=fptime;
end;

procedure GetTime(var hour,min,sec,msec,usec:word);
{
  Gets the current time, adjusted to local time
}
var
  year,day,month:Word;
  tz:timeval;
begin
  fpgettimeofday(@tz,nil);
  EpochToLocal(tz.tv_sec,year,month,day,hour,min,sec);
  msec:=tz.tv_usec div 1000;
  usec:=tz.tv_usec mod 1000;
end;

procedure GetTime(var hour,min,sec,sec100:word);
{
  Gets the current time, adjusted to local time
}
var
  usec : word;
begin
  gettime(hour,min,sec,sec100,usec);
  sec100:=sec100 div 10;
end;

Procedure GetTime(Var Hour,Min,Sec:Word);
{
  Gets the current time, adjusted to local time
}
var
  msec,usec : Word;
Begin
  gettime(hour,min,sec,msec,usec);
End;

Procedure GetDate(Var Year,Month,Day:Word);
{
  Gets the current date, adjusted to local time
}
var
  hour,minute,second : word;
Begin
  EpochToLocal(fptime,year,month,day,hour,minute,second);
End;

Procedure GetDateTime(Var Year,Month,Day,hour,minute,second:Word);
{
  Gets the current date, adjusted to local time
}
Begin
  EpochToLocal(fptime,year,month,day,hour,minute,second);
End;




Procedure GetLocalTime(var SystemTime: TSystemTime);

var
  usecs : Word;
begin
  GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond, usecs);
  GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
//  SystemTime.MilliSecond := 0;
end ;


Procedure InitAnsi;
Var
  i : longint;
begin
  {  Fill table entries 0 to 127  }
  for i := 0 to 96 do
    UpperCaseTable[i] := chr(i);
  for i := 97 to 122 do
    UpperCaseTable[i] := chr(i - 32);
  for i := 123 to 191 do
    UpperCaseTable[i] := chr(i);
  Move (CPISO88591UCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));

  for i := 0 to 64 do
    LowerCaseTable[i] := chr(i);
  for i := 65 to 90 do
    LowerCaseTable[i] := chr(i + 32);
  for i := 91 to 191 do
    LowerCaseTable[i] := chr(i);
  Move (CPISO88591LCT,LowerCaseTable[192],SizeOf(CPISO88591UCT));
end;


Procedure InitInternational;
begin
  InitInternationalGeneric;
  InitAnsi;
end;

function SysErrorMessage(ErrorCode: Integer): String;

begin
  Result:=StrError(ErrorCode);
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=StrPas(BaseUnix.FPGetenv(PChar(EnvVar)));
end;

Function GetEnvironmentVariableCount : Integer;

begin
  Result:=FPCCountEnvVar(EnvP);
end;

Function GetEnvironmentString(Index : Integer) : String;

begin
  Result:=FPCGetEnvStrFromP(Envp,Index);
end;


{$define FPC_USE_FPEXEC}  // leave the old code under IFDEF for a while.
function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
var
  pid    : longint;
  e      : EOSError;
  CommandLine: AnsiString;
  cmdline2 : ppchar;

Begin
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes!
  }
  {$ifdef FPC_USE_FPEXEC}       // Only place we still parse
   cmdline2:=nil;
   if Comline<>'' Then
     begin
       CommandLine:=ComLine;
       cmdline2:=StringtoPPChar(CommandLine,1);
       cmdline2^:=pchar(Path);
     end
   else
     begin
       getmem(cmdline2,2*sizeof(pchar));
       cmdline2^:=pchar(Path);
       cmdline2[1]:=nil;
     end;
  {$else}
  if Pos ('"', Path) = 0 then
    CommandLine := '"' + Path + '"'
  else
    CommandLine := Path;
  if ComLine <> '' then
    CommandLine := Commandline + ' ' + ComLine;
  {$endif}
  pid:=fpFork;
  if pid=0 then
   begin
   {The child does the actual exec, and then exits}
    {$ifdef FPC_USE_FPEXEC}
      fpexecv(pchar(Path),Cmdline2);
    {$else}
      Execl(CommandLine);
    {$endif}
     { If the execve fails, we return an exitvalue of 127, to let it be known}
     fpExit(127);
   end
  else
   if pid=-1 then         {Fork failed}
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,-1]);
      e.ErrorCode:=-1;
      raise e;
    end;

  { We're in the parent, let's wait. }
  result:=WaitProcess(pid); // WaitPid and result-convert

  if (result<0) or (result=127) then
    begin
    E:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,result]);
    E.ErrorCode:=result;
    Raise E;
    end;
End;

function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array Of AnsiString):integer;

var
  pid    : longint;
  e : EOSError;

Begin
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes!
  }
  pid:=fpFork;
  if pid=0 then
   begin
     {The child does the actual exec, and then exits}
      fpexecl(Path,Comline);
     { If the execve fails, we return an exitvalue of 127, to let it be known}
     fpExit(127);
   end
  else
   if pid=-1 then         {Fork failed}
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,-1]);
      e.ErrorCode:=-1;
      raise e;
    end;

  { We're in the parent, let's wait. }
  result:=WaitProcess(pid); // WaitPid and result-convert

  if (result<0) or (result=127) then
    begin
    E:=EOSError.CreateFmt(SExecuteProcessFailed,[Path,result]);
    E.ErrorCode:=result;
    raise E;
    end;
End;


procedure Sleep(milliseconds: Cardinal);

Var
  fd : Integer;
  fds : TfdSet;
  timeout : TimeVal;

begin
  fd:=FileOpen('/dev/null',fmOpenRead);
  If Not(Fd<0) then
    try
      fpfd_zero(fds);
      fpfd_set(0,fds);
      timeout.tv_sec:=Milliseconds div 1000;
      timeout.tv_usec:=(Milliseconds mod 1000) * 1000;
      fpSelect(1,Nil,Nil,@fds,@timeout);
    finally
      FileClose(fd);
    end;
end;

Function GetLastOSError : Integer;

begin
  Result:=fpgetErrNo;
end;

{ ---------------------------------------------------------------------
    Application config files
  ---------------------------------------------------------------------}


Function GetHomeDir : String;

begin
  Result:=GetEnvironmentVariable('HOME');
  If (Result<>'') then
  Result:=IncludeTrailingPathDelimiter(Result);
end;

Function GetAppConfigDir(Global : Boolean) : String;

begin
  If Global then
    Result:=SysConfigDir
  else
    Result:=GetHomeDir+ApplicationName;
end;

Function GetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;

begin
  if Global then
    begin
    Result:=IncludeTrailingPathDelimiter(SysConfigDir);
    if SubDir then
      Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
    Result:=Result+ApplicationName+ConfigExtension;
    end
  else
    begin
    if SubDir then
      begin
      Result:=IncludeTrailingPathDelimiter(GetAppConfigDir(False));
      Result:=Result+ApplicationName+ConfigExtension;
      end
    else
      begin
      Result:=GetHomeDir;
      Result:=Result+'.'+ApplicationName;
      end;
    end;
end;

{****************************************************************************
                              Initialization code
****************************************************************************}


Function GetTempDir(Global : Boolean) : String;

begin
  If Assigned(OnGetTempDir) then
    Result:=OnGetTempDir(Global)
  else
    begin
    Result:=GetEnvironmentVariable('TEMP');
    If (Result='') Then
      Result:=GetEnvironmentVariable('TMP');
    if (Result='') then
      Result:='/tmp/' // fallback.
    end;
  if (Result<>'') then
    Result:=IncludeTrailingPathDelimiter(Result);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
  SysConfigDir:='/etc'; { Initialize system config dir }
Finalization
  DoneExceptions;
end.
{

  $Log: sysutils.pp,v $
  Revision 1.59  2005/03/25 22:53:39  jonas
    * fixed several warnings and notes about unused variables (mainly) or
      uninitialised use of variables/function results (a few)

  Revision 1.58  2005/02/26 14:38:14  florian
    + SysLocale

  Revision 1.57  2005/02/14 17:13:31  peter
    * truncate log

   * getenv had ansistring as param due to {$H+} now shortstring.

  Revision 1.52  2004/11/02 13:59:42  marco
   * timezone stuff back to unix

  Revision 1.51  2004/11/01 07:10:56  peter
    * 1.0.x bootstrap fix

  Revision 1.50  2004/10/31 22:25:31  olle
    * Fix for FPC_USE_LIBC

  Revision 1.49  2004/10/30 20:55:54  marco
   * unix interface cleanup

  Revision 1.48  2004/10/12 15:22:23  michael
  + Fixed sleep: file needs to be closed again

  Revision 1.47  2004/10/10 10:28:34  michael
  + Implementation of GetTempDir and GetTempFileName

  Revision 1.46  2004/08/30 11:20:39  michael
  + Give path, not comline in ExecuteProcess

  Revision 1.45  2004/08/30 11:13:20  michael
  + Fixed ExecuteProcess. Now returns the exit code or raises an exception on failure

  Revision 1.44  2004/08/05 07:32:51  michael
  Added getappconfig calls

  Revision 1.43  2004/07/03 21:50:31  daniel
    * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
      longer necessary

  Revision 1.42  2004/06/15 07:36:03  michael
  + Fixed Globtosearchrec to use unixtowinage

  Revision 1.41  2004/05/22 14:25:03  michael
  + Fixed FindFirst/FindNext so it treats the attributes correctly

  Revision 1.40  2004/04/28 20:48:20  peter
    * ordinal-pointer conversions fixed

  Revision 1.39  2004/04/26 14:50:19  peter
    * FileIsReadOnly fixed

  Revision 1.38  2004/04/20 18:24:32  marco
   * small fix for NIL arg ptr in first executeprocess

  Revision 1.37  2004/03/04 22:15:16  marco
   * UnixType changes. Please report problems to me.

  Revision 1.36  2004/02/13 10:50:23  marco
   * Hopefully last large changes to fpexec and friends.
        - naming conventions changes from Michael.
        - shell functions get alternative under ifdef.
        - arraystring function moves to unixutil
        - unixutil now regards quotes in stringtoppchar.
        - sysutils/unix get executeprocess(ansi,array of ansi), and
                both executeprocess functions are fixed
        - Sysutils/win32 get executeprocess(ansi,array of ansi)

  Revision 1.35  2004/02/12 15:31:06  marco
   * First version of fpexec change. Still under ifdef or silently overloaded

  Revision 1.34  2004/02/09 17:11:17  marco
   * fixed for 1.0 errno->fpgeterrno

  Revision 1.33  2004/02/08 14:50:51  michael
  + Added fileIsReadOnly

  Revision 1.32  2004/02/08 11:01:17  michael
  + Implemented getlastoserror

  Revision 1.31  2004/01/20 23:13:53  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.30  2004/01/10 17:34:36  michael
  + Implemented sleep() on Unix.

  Revision 1.29  2004/01/05 22:42:35  florian
    * compilation error fixed

  Revision 1.28  2004/01/05 22:37:15  florian
    * changed sysutils.exec to ExecuteProcess

  Revision 1.27  2004/01/03 09:09:11  marco
   * Unix exec(ansistring)

  Revision 1.26  2003/11/26 20:35:14  michael
  + Some fixes to have everything compile again

  Revision 1.25  2003/11/17 10:05:51  marco
   * threads for FreeBSD. Not working tho

  Revision 1.24  2003/10/25 23:43:59  hajny
    * THandle in sysutils common using System.THandle

  Revision 1.23  2003/10/07 08:28:49  marco
   * fix from Vincent to casetables

  Revision 1.22  2003/09/27 12:51:33  peter
    * fpISxxx macros renamed to C compliant fpS_ISxxx

  Revision 1.21  2003/09/17 19:07:44  marco
   * more fixes for Unix<->unixutil

  Revision 1.20  2003/09/17 12:41:31  marco
   * Uses more baseunix, less unix now

  Revision 1.19  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.18  2003/04/01 15:57:41  peter
    * made THandle platform dependent and unique type

  Revision 1.17  2003/03/30 10:38:00  armin
  * corrected typo in DirectoryExists

  Revision 1.16  2003/03/29 18:21:42  hajny
    * DirectoryExists declaration changed to that one from fixes branch

  Revision 1.15  2003/03/28 19:06:59  peter
    * directoryexists added

  Revision 1.14  2003/01/03 20:41:04  peter
    * FileCreate(string,mode) overload added

  Revision 1.13  2002/09/07 16:01:28  peter
    * old logs removed and tabs fixed

  Revision 1.12  2002/01/25 16:23:03  peter
    * merged filesearch() fix

}

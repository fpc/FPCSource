{
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

{$if (defined(BSD) or defined(SUNOS)) and defined(FPC_USE_LIBC)}
{$define USE_VFORK}
{$endif}

{$DEFINE OS_FILESETDATEBYNAME}
{$DEFINE HAS_SLEEP}
{$DEFINE HAS_OSERROR}
{$DEFINE HAS_OSCONFIG}
{$DEFINE HAS_TEMPDIR}
{$DEFINE HASUNIX}
{$DEFINE HASCREATEGUID}
{$DEFINE HAS_OSUSERDIR}

uses
  Unix,errors,sysconst,Unixtype;

{ Include platform independent interface part }
{$i sysutilh.inc}

Function AddDisk(const path:string) : Byte;

{ the following is Kylix compatibility stuff, it should be moved to a
  special compatibilty unit (FK) }
  const
    RTL_SIGINT     = 0;
    RTL_SIGFPE     = 1;
    RTL_SIGSEGV    = 2;
    RTL_SIGILL     = 3;
    RTL_SIGBUS     = 4;
    RTL_SIGQUIT    = 5;
    RTL_SIGLAST    = RTL_SIGQUIT;
    RTL_SIGDEFAULT = -1;

  type
    TSignalState = (ssNotHooked, ssHooked, ssOverridden);

function InquireSignal(RtlSigNum: Integer): TSignalState;
procedure AbandonSignalHandler(RtlSigNum: Integer);
procedure HookSignal(RtlSigNum: Integer);
procedure UnhookSignal(RtlSigNum: Integer; OnlyIfHooked: Boolean = True);

implementation

Uses
  {$ifdef FPC_USE_LIBC}initc{$ELSE}Syscall{$ENDIF}, Baseunix, unixutil;

function InquireSignal(RtlSigNum: Integer): TSignalState;
  begin
  end;


procedure AbandonSignalHandler(RtlSigNum: Integer);
  begin
  end;


procedure HookSignal(RtlSigNum: Integer);
  begin
  end;


procedure UnhookSignal(RtlSigNum: Integer; OnlyIfHooked: Boolean = True);
  begin
  end;

{$Define OS_FILEISREADONLY} // Specific implementation for Unix.

{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
{$DEFINE FPC_FEXPAND_GETENVPCHAR} { GetEnv result is a PChar }

{ Include platform independent implementation part }
{$i sysutils.inc}

{ Include SysCreateGUID function }
{$i suuid.inc}

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


Function DoFileLocking(Handle: Longint; Mode: Integer) : Longint;
var
  lockop: cint;
  lockres: cint;
begin
  DoFileLocking:=Handle;
{$ifndef beos}
  if (Handle>=0) then
    begin
{$ifdef solaris}
      { Solaris' flock is based on top of fcntl, which does not allow
        exclusive locks for files only opened for reading nor shared
        locks for files opened only for writing.
        
        If no locking is specified, we normally need an exclusive lock.
        So create an exclusive lock for fmOpenWrite and fmOpenReadWrite,
        but only a shared lock for fmOpenRead (since an exclusive lock
        is not possible in that case)
      }
      if ((mode and (fmShareCompat or fmShareExclusive or fmShareDenyWrite or fmShareDenyRead or fmShareDenyNone)) = 0) then
        begin
          if ((mode and (fmOpenRead or fmOpenWrite or fmOpenReadWrite)) = fmOpenRead) then
            mode := mode or fmShareDenyWrite
          else
            mode := mode or fmShareExclusive;
        end;
{$endif solaris}
      case (mode and (fmShareCompat or fmShareExclusive or fmShareDenyWrite or fmShareDenyRead or fmShareDenyNone)) of
        fmShareCompat,
        fmShareExclusive:
          lockop:=LOCK_EX or LOCK_NB;
        fmShareDenyWrite:
          lockop:=LOCK_SH or LOCK_NB;
        fmShareDenyNone:
          exit;
        else
          begin
            { fmShareDenyRead does not exit under *nix, only shared access
              (similar to fmShareDenyWrite) and exclusive access (same as
              fmShareExclusive)
            }
            FpClose(Handle);
            DoFileLocking:=-1;
            exit;
          end;
      end;
      repeat
        lockres:=fpflock(Handle,lockop);
      until (lockres=0) or
            (fpgeterrno<>ESysEIntr);
      if (lockres<>0) then
        begin
          FpClose(Handle);
          DoFileLocking:=-1;
          exit;
        end;
    end;
{$endif not beos}
end;


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var
  LinuxFlags : longint;
begin
  LinuxFlags:=0;
  case (Mode and (fmOpenRead or fmOpenWrite or fmOpenReadWrite)) of
    fmOpenRead : LinuxFlags:=LinuxFlags or O_RdOnly;
    fmOpenWrite : LinuxFlags:=LinuxFlags or O_WrOnly;
    fmOpenReadWrite : LinuxFlags:=LinuxFlags or O_RdWr;
  end;
  FileOpen:=fpOpen (pointer(FileName),LinuxFlags);

  FileOpen:=DoFileLocking(FileOpen, Mode);
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=fpOpen(pointer(FileName),O_RdWr or O_Creat or O_Trunc);
end;


Function FileCreate (Const FileName : String;Mode : Longint) : Longint;

begin
  FileCreate:=fpOpen(pointer(FileName),O_RdWr or O_Creat or O_Trunc,Mode);
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
  result:=longint(FileSeek(Handle,int64(FOffset),Origin));
end;


Function FileSeek (Handle : Longint; FOffset : Int64; Origin : Longint) : Int64;
begin
  FileSeek:=fplSeek (Handle,FOffset,Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  fpclose(Handle);
end;

Function FileTruncate (Handle: THandle; Size: Int64) : boolean;

begin
  if (SizeOf (TOff) < 8)   (* fpFTruncate only supporting signed 32-bit size *)
                         and (Size > high (longint)) then
    FileTruncate := false
  else
    FileTruncate:=fpftruncate(Handle,Size)>=0;
end;

{$ifndef FPUNONE}
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
  If  fpstat (pointer(FileName),Info)<0 then
    exit(-1)
  else
    Result:=UnixToWinAge(info.st_mtime);
end;
{$endif}


Function FileExists (Const FileName : String) : Boolean;

begin
  // Don't use stat. It fails on files >2 GB.
  // Access obeys the same access rules, so the result should be the same.
  FileExists:=fpAccess(pointer(filename),F_OK)=0;
end;


Function DirectoryExists (Const Directory : String) : Boolean;

Var Info : Stat;

begin
  DirectoryExists:=(fpstat(pointer(Directory),Info)>=0) and fpS_ISDIR(Info.st_mode);
end;


Function LinuxToWinAttr (FN : Pchar; Const Info : Stat) : Longint;

Var
  FNL : String;
  LinkInfo : Stat;

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
  If fpS_ISLNK(Info.st_mode) Then
    begin
    Result:=Result or faSymLink;
    // Windows reports if the link points to a directory.
    FNL:=StrPas(FN);
    if (fpstat(FNL,LinkInfo)>=0) and fpS_ISDIR(LinkInfo.st_mode) then
      Result := Result or faDirectory;
    end;
end;


Function FNMatch(const Pattern,Name:string):Boolean;
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


Type
  TUnixFindData = Record
    NamePos    : LongInt;     {to track which search this is}
    DirPtr     : Pointer;     {directory pointer for reading directory}
    SearchSpec : String;
    SearchType : Byte;        {0=normal, 1=open will close, 2=only 1 file}
    SearchAttr : Byte;        {attribute we are searching for}
  End;
  PUnixFindData = ^TUnixFindData;
Var
  CurrSearchNum : LongInt;

Procedure FindClose(Var f: TSearchRec);
var
  UnixFindData : PUnixFindData;
Begin
  UnixFindData:=PUnixFindData(f.FindHandle);
  if UnixFindData=nil then
    exit;
  if UnixFindData^.SearchType=0 then
    begin
      if UnixFindData^.dirptr<>nil then
        fpclosedir(pdir(UnixFindData^.dirptr)^);
    end;
  Dispose(UnixFindData);
  f.FindHandle:=nil;
End;


Function FindGetFileInfo(const s:string;var f:TSearchRec):boolean;
var
  st      : baseunix.stat;
  WinAttr : longint;
  
begin
  FindGetFileInfo:=false;
  If Assigned(F.FindHandle) and ((((PUnixFindData(f.FindHandle)^.searchattr)) and faSymlink) > 0) then
    FindGetFileInfo:=(fplstat(pointer(s),st)=0)    
  else
    FindGetFileInfo:=(fpstat(pointer(s),st)=0);
  If not FindGetFileInfo then 
    exit;  
  WinAttr:=LinuxToWinAttr(PChar(pointer(s)),st);
  If (f.FindHandle = nil) or ((WinAttr and Not(PUnixFindData(f.FindHandle)^.searchattr))=0) Then
   Begin
     f.Name:=ExtractFileName(s);
     f.Attr:=WinAttr;
     f.Size:=st.st_Size;
     f.Mode:=st.st_mode;
{$ifndef FPUNONE}
     f.Time:=UnixToWinAge(st.st_mtime);
{$endif}
     result:=true;
   End;
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
{
  re-opens dir if not already in array and calls FindGetFileInfo
}
Var
  DirName  : String;
  FName,
  SName    : string;
  Found,
  Finished : boolean;
  p        : pdirent;
  UnixFindData : PUnixFindData;
Begin
  Result:=-1;
  UnixFindData:=PUnixFindData(Rslt.FindHandle);
  if UnixFindData=nil then
    exit;
  if (UnixFindData^.SearchType=0) and
     (UnixFindData^.Dirptr=nil) then
    begin
      If UnixFindData^.NamePos = 0 Then
        DirName:='./'
      Else
        DirName:=Copy(UnixFindData^.SearchSpec,1,UnixFindData^.NamePos);
      UnixFindData^.DirPtr := fpopendir(Pchar(pointer(DirName)));
    end;
  SName:=Copy(UnixFindData^.SearchSpec,UnixFindData^.NamePos+1,Length(UnixFindData^.SearchSpec));
  Found:=False;
  Finished:=(UnixFindData^.dirptr=nil);
  While Not Finished Do
   Begin
     p:=fpreaddir(pdir(UnixFindData^.dirptr)^);
     if p=nil then
      FName:=''
     else
      FName:=p^.d_name;
     If FName='' Then
      Finished:=True
     Else
      Begin
        If FNMatch(SName,FName) Then
         Begin
           Found:=FindGetFileInfo(Copy(UnixFindData^.SearchSpec,1,UnixFindData^.NamePos)+FName,Rslt);
           if Found then
             begin
               Result:=0;
               exit;
             end;
         End;
      End;
   End;
End;


Function FindFirst (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;
{
  opens dir and calls FindNext if needed.
}
var
  UnixFindData : PUnixFindData;
Begin
  Result:=-1;
  fillchar(Rslt,sizeof(Rslt),0);
  if Path='' then
    exit;
  {Wildcards?}
  if (Pos('?',Path)=0)  and (Pos('*',Path)=0) then
   begin
     if FindGetFileInfo(Path,Rslt) then
       Result:=0;
   end
  else
   begin
     { Allocate UnixFindData }
     New(UnixFindData);
     FillChar(UnixFindData^,sizeof(UnixFindData^),0);
     Rslt.FindHandle:=UnixFindData;
     {Create Info}
     UnixFindData^.SearchSpec := Path;
     {We always also search for readonly and archive, regardless of Attr:}
     UnixFindData^.SearchAttr := Attr or faarchive or fareadonly;
     UnixFindData^.NamePos := Length(UnixFindData^.SearchSpec);
     while (UnixFindData^.NamePos>0) and (UnixFindData^.SearchSpec[UnixFindData^.NamePos]<>'/') do
       dec(UnixFindData^.NamePos);
     Result:=FindNext(Rslt);
   end;
End;


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
  If  FpStat (pointer(FileName),Info)<0 then
    Result:=-1
  Else
    Result:=LinuxToWinAttr(Pchar(ExtractFileName(FileName)),Info);
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:=fpUnLink (pointer(FileName))>=0;
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=BaseUnix.FpRename(pointer(OldNAme),pointer(NewName))>=0;
end;

Function FileIsReadOnly(const FileName: String): Boolean;

begin
  Result := fpAccess(PChar(pointer(FileName)),W_OK)<>0;
end;

Function FileSetDate (Const FileName : String;Age : Longint) : Longint;

var
  t: TUTimBuf;

begin
  Result := 0;
  t.actime := Age;
  t.modtime := Age;
  if fputime(PChar(pointer(FileName)), @t) = -1 then
    Result := fpgeterrno;
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

Function AddDisk(const path:string) : Byte;
begin
  if not (DriveStr[Drives]=nil) then
   FreeMem(DriveStr[Drives],StrLen(DriveStr[Drives])+1);
  GetMem(DriveStr[Drives],length(Path)+1);
  StrPCopy(DriveStr[Drives],path);
  inc(Drives);
  if Drives>26 then
   Drives:=4;
  Result:=Drives;
end;


Function DiskFree(Drive: Byte): int64;
var
  fs : tstatfs;
Begin
  if ((Drive in [Low(FixDriveStr)..High(FixDriveStr)]) and (not (fixdrivestr[Drive]=nil)) and (statfs(StrPas(fixdrivestr[drive]),fs)<>-1)) or
     ((Drive <= High(drivestr)) and (not (drivestr[Drive]=nil)) and (statfs(StrPas(drivestr[drive]),fs)<>-1)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;
End;



Function DiskSize(Drive: Byte): int64;
var
  fs : tstatfs;
Begin
  if ((Drive in [Low(FixDriveStr)..High(FixDriveStr)]) and (not (fixdrivestr[Drive]=nil)) and (statfs(StrPas(fixdrivestr[drive]),fs)<>-1)) or
     ((drive <= High(drivestr)) and (not (drivestr[Drive]=nil)) and (statfs(StrPas(drivestr[drive]),fs)<>-1)) then
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




{$ifndef FPUNONE}
Procedure GetLocalTime(var SystemTime: TSystemTime);

var
  usecs : Word;
begin
  GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond, usecs);
  GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
//  SystemTime.MilliSecond := 0;
end ;
{$endif}


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
  Result:=StrPas(BaseUnix.FPGetenv(PChar(pointer(EnvVar))));
end;

Function GetEnvironmentVariableCount : Integer;

begin
  Result:=FPCCountEnvVar(EnvP);
end;

Function GetEnvironmentString(Index : Integer) : String;

begin
  Result:=FPCGetEnvStrFromP(Envp,Index);
end;


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

   // Only place we still parse
   cmdline2:=nil;
   if Comline<>'' Then
     begin
       CommandLine:=ComLine;
       { Make an unique copy because stringtoppchar modifies the
         string }
       UniqueString(CommandLine);
       cmdline2:=StringtoPPChar(CommandLine,1);
       cmdline2^:=pchar(pointer(Path));
     end
   else
     begin
       getmem(cmdline2,2*sizeof(pchar));
       cmdline2^:=pchar(Path);
       cmdline2[1]:=nil;
     end;

  {$ifdef USE_VFORK}
  pid:=fpvFork;
  {$else USE_VFORK}
  pid:=fpFork;
  {$endif USE_VFORK}
  if pid=0 then
   begin
   {The child does the actual exec, and then exits}
      fpexecv(pchar(pointer(Path)),Cmdline2);
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

  if Comline<>'' Then
    freemem(cmdline2);

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
  timeout,timeoutresult : TTimespec;

begin
  timeout.tv_sec:=milliseconds div 1000;
  timeout.tv_nsec:=1000*1000*(milliseconds mod 1000);
  fpnanosleep(@timeout,@timeoutresult);
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

{ Follows base-dir spec,
  see [http://freedesktop.org/Standards/basedir-spec].
  Always ends with PathDelim. }
Function XdgConfigHome : String;
begin
  Result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result='') then
    Result:=GetHomeDir + '.config/'
  else
    Result:=IncludeTrailingPathDelimiter(Result);
end;

Function GetAppConfigDir(Global : Boolean) : String;

begin
  If Global then
    Result:=IncludeTrailingPathDelimiter(SysConfigDir)
  else
    Result:=IncludeTrailingPathDelimiter(XdgConfigHome);
  if VendorName<>'' then
    Result:=IncludeTrailingPathDelimiter(Result+VendorName);
  Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
end;

Function GetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;

begin
  If Global then
    Result:=IncludeTrailingPathDelimiter(SysConfigDir)
  else
    Result:=IncludeTrailingPathDelimiter(XdgConfigHome);
  if SubDir then
    begin
      if VendorName<>'' then
        Result:=IncludeTrailingPathDelimiter(Result+VendorName);
      Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
    end;
  Result:=Result+ApplicationName+ConfigExtension;
end;


{****************************************************************************
                              GetTempDir 
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
                              GetUserDir 
****************************************************************************}

Var
  TheUserDir : String;

Function GetUserDir : String;

begin
  If (TheUserDir='') then
    begin
    TheUserDir:=GetEnvironmentVariable('HOME'); 
    if (TheUserDir<>'') then
      TheUserDir:=IncludeTrailingPathDelimiter(TheUserDir)
    else
      TheUserDir:=GetTempDir(False);
    end;
  Result:=TheUserDir;    
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

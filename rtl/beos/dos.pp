{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by members of the Free Pascal
    development team

    DOS unit template based on POSIX

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit Dos;

Interface
{$goto on}

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

  SearchRec = packed Record
  {Fill : array[1..21] of byte;  Fill replaced with below}
    DirPtr     : pointer;        {directory pointer for reading directory}
    SearchAttr : Byte;        {attribute we are searching for}
    Fill       : Array[1..16] of Byte; {future use}
  {End of fill}
    Attr       : Byte;        {attribute of found file}
    Time       : LongInt;     {last modify date of found file}
    Size       : LongInt;     {file size of found file}
    Reserved   : Word;        {future use}
    Name       : String[FileNameLen]; {name of found file}
    SearchSpec : String[FileNameLen]; {search pattern}
    SearchDir  : String[FileNameLen]; { path we are searching in }
  End;

{
  filerec.inc contains the definition of the filerec.
  textrec.inc contains the definition of the textrec.
  It is in a separate file to make it available in other units without
  having to use the DOS unit for it.
}
{$i filerec.inc}
{$i textrec.inc}

  Registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax, ebx, ecx, edx, ebp, esi, edi : longint);
    End;

  DateTime = packed record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

Var
  DosError : integer;


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
Function  DiskFree(drive: byte) : int64;
Function  DiskSize(drive: byte) : int64;
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);

{File}
Procedure GetFAttr(var f; var attr: word);
Procedure GetFTime(var f; var time: longint);
Function  FSearch(const path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(const path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);

{Environment}
Function  EnvCount: longint;
Function  EnvStr(index: integer): string;
Function  GetEnv (envvar: string): string;

{Do Nothing Functions, no POSIX version}
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
  Strings,posix;


  { Used by AddDisk(), DiskFree() and DiskSize() }
const
  Drives   : byte = 4;
  MAX_DRIVES = 26;
var
  DriveStr : array[4..MAX_DRIVES] of pchar;


Function StringToPPChar(Var S:STring; var count : longint):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially usefull for creating an ArgV for Exec-calls
}
var
  nr  : longint;
  Buf : ^char;
  p   : ppchar;
begin
  s:=s+#0;
  buf:=@s[1];
  nr:=0;
  while(buf^<>#0) do
   begin
     while (buf^ in [' ',#8,#10]) do
      inc(buf);
     inc(nr);
     while not (buf^ in [' ',#0,#8,#10]) do
      inc(buf);
   end;
  getmem(p,nr*4);
  StringToPPChar:=p;
  if p=nil then
   begin
     Errno:=sys_enomem;
     count := 0;
     exit;
   end;
  buf:=@s[1];
  while (buf^<>#0) do
   begin
     while (buf^ in [' ',#8,#10]) do
      begin
        buf^:=#0;
        inc(buf);
      end;
     p^:=buf;
     inc(p);
     p^:=nil;
     while not (buf^ in [' ',#0,#8,#10]) do
      inc(buf);
   end;
   count := nr;
end;


{$i dos.inc}    { include OS specific stuff }




{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}
var
  TZSeconds : longint;   { offset to add/ subtract from Epoch to get local time }
  tzdaylight : boolean;
  tzname     : array[boolean] of pchar;


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
Const
{Date Calculation}
  C1970 = 2440588;
  D0    = 1461;
  D1    = 146097;
  D2    = 1721119;


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



Procedure EpochToLocal(epoch:time_t;var year,month,day,hour,minute,second:Word);
{
  Transforms Epoch time into local time (hour, minute,seconds)
}
Var
  DateNum: time_t;
Begin
  Epoch:=Epoch+TZSeconds;
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Abs(Epoch Mod 86400);
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;



Procedure GetDate(Var Year, Month, MDay, WDay: Word);
var
  hour,minute,second : word;
  timeval : time_t;
Begin
  timeval := sys_time(timeval);
  { convert the GMT time to local time }
  EpochToLocal(timeval,year,month,mday,hour,minute,second);
  Wday:=weekday(Year,Month,MDay);
end;



Procedure SetDate(Year, Month, Day: Word);
Begin
  {!!}
End;




Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
var
 timeval : time_t;
 year,month,day: word;
Begin
  timeval := sys_time(timeval);
  EpochToLocal(timeval,year,month,day,hour,minute,second);
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


{$ifndef DOS_HAS_EXEC}
{******************************************************************************
                               --- Exec ---
******************************************************************************}

var
  LastDosExitCode: word;


Function  InternalWaitProcess(Pid:pid_t):Longint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
var     r,s     : cint;
begin
  repeat
    s:=$7F00;
    r:=sys_WaitPid(Pid,s,0);
  until (r<>-1) or (Errno<>Sys_EINTR);
  { When r = -1 or r = 0, no status is available, so there was an error. }
  if (r=-1) or (r=0) then
    InternalWaitProcess:=-1 { return -1 to indicate an error }
  else
   begin
     { process terminated normally }
     if wifexited(s)<>0 then
       begin
         { get status code }
         InternalWaitProcess := wexitstatus(s);
         exit;
       end;
     { process terminated due to a signal }
     if wifsignaled(s)<>0 then
       begin
         { get signal number }
         InternalWaitProcess := wstopsig(s);
         exit;
       end;
     InternalWaitProcess:=-1;
   end;
end;




Procedure Exec (Const Path: PathStr; Const ComLine: ComStr);
var
  pid    : pid_t;
  tmp : string;
  p : ppchar;
  count: longint;
  // The Error-Checking in the previous Version failed, since halt($7F) gives an WaitPid-status of $7F00
  F: File;
Begin
{$IFOPT I+}
{$DEFINE IOCHECK}
{$ENDIF}
{$I-}
  { verify if the file to execute exists }
  Assign(F,Path);
  Reset(F,1);
  if IOResult <> 0 then
    { file not found }
    begin
      DosError := 2;
      exit;
    end
  else
    Close(F); 
{$IFDEF IOCHECK}
{$I+}
{$UNDEF IOCHECK}
{$ENDIF}
  LastDosExitCode:=0;
  { Fork the process }
  pid:=sys_Fork;
  if pid=0 then
   begin
   {The child does the actual execution, and then exits}
    tmp := Path+' '+ComLine;
    p:=StringToPPChar(tmp,count);
    if (p<>nil) and (p^<>nil) then
    begin
      sys_Execve(p^,p,Envp);
    end;  
   {If the execve fails, we return an exitvalue of 127, to let it be known}
     sys_exit(127);
   end
  else
   if pid=-1 then         {Fork failed - parent only}
    begin
      DosError:=8;
      exit
    end;
{We're in the parent, let's wait.}
  LastDosExitCode:=InternalWaitProcess(pid); // WaitPid and result-convert
  if (LastDosExitCode>=0) and (LastDosExitCode<>127) then DosError:=0 else
     DosError:=8; // perhaps one time give an better error
End;

Function DosExitCode: Word;
Begin
  DosExitCode:=LastDosExitCode;
End;
{$ENDIF}


{******************************************************************************
                               --- Disk ---
******************************************************************************}


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


{******************************************************************************
                       --- Findfirst FindNext ---
******************************************************************************}


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
              while Found and (i<LenPat) do
                begin
                inc(i);
                case Pattern[i] of
                  '*' : ;
                  '?' : begin
                          inc(j);
                          Found:=(j<=LenName);
                        end;
                else
                  Found:=false;
                end;
               end;
            {Now, find in name the character which i points to, if the * or ?
             wasn't the last character in the pattern, else, use up all the
             chars in name}
              Found:=true;
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
                     end
                    else
                     inc(j);{We didn't find one, need to look further}
                  end;
               until (j>=LenName);
                end
              else
                j:=LenName;{we can stop}
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


Procedure FindClose(Var f: SearchRec);
{
  Closes dirptr if it is open
}
Begin
  { could already have been closed }
  if assigned(f.dirptr) then
     sys_closedir(pdir(f.dirptr));
  f.dirptr := nil;
End;


{ Returns a filled in searchRec structure }
{ and TRUE if the specified file in s is  }
{ found.                                  }
Function FindGetFileInfo(s:string;var f:SearchRec):boolean;
var
  DT   : DateTime;
  st   : stat;
  Fmode : byte;
  res: string;    { overlaid variable }
  Dir : DirsTr;
  Name : NameStr;
  Ext: ExtStr;
begin
  FindGetFileInfo:=false;
  res := s + #0;
  if sys_stat(@res[1],st)<>0 then
   exit;
  if S_ISDIR(st.st_mode) then
   fmode:=directory
  else
   { contrary to DOS - all normal files have the archive bits set }
   { under POSIX.                                                 }
   fmode:=archive;
  if (st.st_mode and S_IWUSR)=0 then
   fmode:=fmode or readonly;
  If ((FMode and f.searchattr)<>0) or ((Fmode and Archive)<>0) Then
   Begin
     FSplit(s,Dir,Name,Ext);
     if Ext <> '' then
       res := Name + Ext
     else
       res := Name;
     f.Name:=res;
     f.Attr:=FMode;
     f.Size:=longint(st.st_size);
     UnixDateToDT(st.st_mtime, DT);
     PackTime(DT,f.Time);
     FindGetFileInfo:=true;
   End;
end;


Procedure FindNext(Var f: SearchRec);
{
  re-opens dir if not already in array and calls FindWorkProc
}
Var
  FName,
  SName    : string;
  Found,
  Finished : boolean;
  p        : PDirEnt;
Begin
{Main loop}
  SName:=f.SearchSpec;
  Found:=False;
  Finished:=(f.dirptr=nil);
  While Not Finished Do
   Begin
     p:=sys_readdir(pdir(f.dirptr));
     if p=nil then
     begin
      FName:=''
     end
     else
      FName:=Strpas(@p^.d_name);
     If FName='' Then
      Finished:=True
     Else
      Begin
        If FNMatch(SName,FName) Then
         Begin
           Found:=FindGetFileInfo(f.SearchDir+FName,f);
           if Found then
           begin
            Finished:=true;
           end;
         End;
      End;
   End;
{Shutdown}
  If Found Then
   Begin
     DosError:=0;
   End
  Else
   Begin
     FindClose(f);
     { FindClose() might be called thereafter also... }
     f.dirptr := nil;
     DosError:=18;
   End;
End;


Procedure FindFirst(Const Path: PathStr; Attr: Word; Var f: SearchRec);
{
  opens dir
}
var
 res: string;
  Dir : DirsTr;
  Name : NameStr;
  Ext: ExtStr;
Begin
  { initialize f.dirptr because it is used    }
  { to see if we need to close the dir stream }
  f.dirptr := nil;
  if Path='' then
   begin
     DosError:=3;
     exit;
   end;
  f.SearchAttr := Attr;
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
     f.DirPtr:=nil;
   end
  else
{Find Entry}
   begin
     FSplit(Path,Dir,Name,Ext);
     if Ext <> '' then
       res := Name + Ext
     else
       res := Name;
     f.SearchSpec := res;
     { if dir is an empty string }
     { then this indicates that  }
     { use the current working   }
     { directory.                }
     if dir = '' then
        dir := './';
     f.SearchDir := Dir;
     { add terminating null character }
     Dir := Dir + #0;
     f.dirptr := sys_opendir(@Dir[1]);
     if not assigned(f.dirptr) then
     begin
        DosError := 8;
        exit;
     end;
     FindNext(f);
   end;
End;


{******************************************************************************
                               --- File ---
******************************************************************************}

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



{
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
}
(*
{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
*)
const
  LFNSupport = true;
  FileNameCaseSensitive = true;

{$I fexpand.inc}



Function FSearch(const path:pathstr;dirlist:string):pathstr;
{
  Searches for a file 'path' in the list of direcories in 'dirlist'.
  returns an empty string if not found. Wildcards are NOT allowed.
  If dirlist is empty, it is set to '.'
}
Var
  NewDir : PathStr;
  p1     : Longint;
  Info   : Stat;
  buffer : array[0..FileNameLen+1] of char;
Begin
  Move(path[1], Buffer, Length(path));
  Buffer[Length(path)]:=#0;
  if (length(Path)>0) and (path[1]='/') and (sys_stat(pchar(@Buffer),info)=0) then
  begin
    FSearch:=path;
    exit;
  end;
{Replace ':' with ';'}
  for p1:=1to length(dirlist) do
   if dirlist[p1]=':' then
    dirlist[p1]:=';';
{Check for WildCards}
  If (Pos('?',Path) <> 0) or (Pos('*',Path) <> 0) Then
   FSearch:='' {No wildcards allowed in these things.}
  Else
   Begin
     Dirlist:='.;'+dirlist;{Make sure current dir is first to be searched.}
     Repeat
       p1:=Pos(';',DirList);
       If p1=0 Then
        p1:=255;
       NewDir:=Copy(DirList,1,P1 - 1);
       if NewDir[Length(NewDir)]<>'/' then
        NewDir:=NewDir+'/';
       NewDir:=NewDir+Path;
       Delete(DirList,1,p1);
       Move(NewDir[1], Buffer, Length(NewDir));
       Buffer[Length(NewDir)]:=#0;
       if sys_stat(pchar(@Buffer),Info)=0 then
        Begin
          If Pos('./',NewDir)=1 Then
           Delete(NewDir,1,2);
        {DOS strips off an initial .\}
        End
       Else
        NewDir:='';
     Until (DirList='') or (Length(NewDir) > 0);
     FSearch:=NewDir;
   End;
End;



Procedure GetFAttr(var f; var attr : word);
Var
  info : stat;
  LinAttr : mode_t;
Begin
  DosError:=0;
  if sys_stat(@textrec(f).name,info)<>0 then
   begin
     Attr:=0;
     DosError:=3;
     exit;
   end
  else
   LinAttr:=Info.st_Mode;
  if S_ISDIR(LinAttr) then
   Attr:=directory
  else
   Attr:=archive;
  if sys_Access(@textrec(f).name,W_OK)<>0 then
   Attr:=Attr or readonly;
  if (not S_ISDIR(LinAttr)) and (filerec(f).name[0]='.')  then
   Attr:=Attr or hidden;
end;



Procedure getftime (var f; var time : longint);
Var
  Info: stat;
  DT: DateTime;
Begin
  doserror:=0;
  if sys_fstat(filerec(f).handle,info)<>0 then
   begin
     Time:=0;
     doserror:=3;
     exit
   end
  else
   UnixDateToDT(Info.st_mtime,DT);
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
  envcnt:=0;
  p:=envp;      {defined in syslinux}
  while (p^<>nil) do
   begin
     inc(envcnt);
     inc(p);
   end;
  EnvCount := envcnt
End;



Function EnvStr(Index: Integer): String;
Var
  i : longint;
  p : ppchar;
Begin
  p:=envp;      {defined in syslinux}
  i:=1;
  envstr:='';
  if (index < 1) or (index > EnvCount) then
    exit;
  while (i<Index) and (p^<>nil) do
   begin
     inc(i);
     inc(p);
   end;
  if p<>nil then
   envstr:=strpas(p^)
End;


Function GetEnv(EnvVar:string):string;
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}
var
  ep    : ppchar;
  found : boolean;
  p1 : pchar;
Begin
  EnvVar:=EnvVar+'=';            {Else HOST will also find HOSTNAME, etc}
  ep:=envp;
  found:=false;
  if ep<>nil then
   begin
     while (not found) and (ep^<>nil) do
      begin
        if strlcomp(@EnvVar[1],(ep^),length(EnvVar))=0 then
         found:=true
        else
         inc(ep);
      end;
   end;
  if found then
   p1:=ep^+length(EnvVar)
  else
   p1:=nil;
  if p1 = nil then
    GetEnv := ''
  else
    GetEnv := StrPas(p1);
end;



{******************************************************************************
                      --- Do Nothing Procedures/Functions ---
******************************************************************************}

Procedure Intr (intno: byte; var regs: registers);
Begin
  {! No POSIX equivalent !}
End;



Procedure msdos(var regs : registers);
Begin
  {! No POSIX equivalent !}
End;



Procedure getintvec(intno : byte;var vector : pointer);
Begin
  {! No POSIX equivalent !}
End;



Procedure setintvec(intno : byte;vector : pointer);
Begin
  {! No POSIX equivalent !}
End;



Procedure SwapVectors;
Begin
  {! No POSIX equivalent !}
End;



Procedure keep(exitcode : word);
Begin
  {! No POSIX equivalent !}
End;



Procedure setftime(var f; time : longint);
Begin
  {! No POSIX equivalent !}
End;



Procedure setfattr (var f;attr : word);
Begin
  {! No POSIX equivalent !}
End;



Procedure GetCBreak(Var BreakValue: Boolean);
Begin
{! No POSIX equivalent !}
  breakvalue:=true
End;



Procedure SetCBreak(BreakValue: Boolean);
Begin
  {! No POSIX equivalent !}
End;



Procedure GetVerify(Var Verify: Boolean);
Begin
  {! No POSIX equivalent !}
  Verify:=true;
End;



Procedure SetVerify(Verify: Boolean);
Begin
  {! No POSIX equivalent !}
End;

{ Include timezone routines }
{$i timezone.inc}

{******************************************************************************
                            --- Initialization ---
******************************************************************************}

Initialization
  InitLocalTime;

finalization
  DoneLocalTime;
end.
{
  $Log$
  Revision 1.4  2003-01-08 22:32:28  marco
   * Small fixes and quick merge with 1.0.x. At least the compiler builds now,
      but it could crash hard, since there are lots of unimplemented funcs.

  Revision 1.1.2.14  2001/12/09 03:31:35  carl
  * Exec() fixed (was full of bugs) : No DosError=2 report fixed, status code error fixed.
  + MAX_DRIVES constant added

  Revision 1.1.2.13  2001/12/03 03:12:28  carl
  * update for new posix prototype (caused problem with other OS)
    readdir / closedir

  Revision 1.1.2.12  2001/09/28 01:11:14  carl
  * bugfix of pchar move in FSearch() (would give wrong results)

  Revision 1.1.2.11  2001/08/21 10:48:46  carl
  + add goto on

  Revision 1.1.2.10  2001/08/15 01:04:38  carl
  * instead include posix unit
  * corrected bug in DateNum type (should be time_t)

  Revision 1.1.2.9  2001/08/13 09:37:17  carl
  * changed prototype of sys_readdir

  Revision 1.1.2.8  2001/08/12 15:12:30  carl
  + added timezone information
  * bugfix of overflow in conversion of epoch to local
  * bugfix of index verification in getenv

  Revision 1.1.2.7  2001/08/08 01:58:18  carl
  * bugfix of problem with FindFirst() / FindNext()

  Revision 1.1.2.5  2001/08/04 05:24:21  carl
  + implemented FindFirst / FindNext (untested)
  + Exec()
  + split
  + Timezone support reinstated

  Revision 1.1.2.4  2001/07/08 04:46:01  carl
  * waitpid is now portable
  + fnmatch()

  Revision 1.1.2.3  2001/07/07 15:42:29  carl
  * compiler error corrections

  Revision 1.1.2.2  2001/07/07 03:49:53  carl
  + more POSIX compliance stuff

  Revision 1.1.2.1  2001/07/06 11:21:49  carl
  + add files for POSIX


}

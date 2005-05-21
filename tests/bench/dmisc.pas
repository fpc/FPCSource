{$H-}
unit dmisc;

interface

{$ifndef linux}
   {$define MSWindows}
{$endif}

uses
{$ifdef linux}
  Libc,
{$else}
  windows,
{$endif}
  sysutils;

{$ifdef VER100}
   type int64 = longint;
{$endif}

Const
  Max_Path = 255;

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
  DWord   = Cardinal;

{ Needed for Win95 LFN Support }
  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];

  FileRec = TFileRec;

  DateTime = packed record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

  SearchRec = Sysutils.TSearchRec;

  registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax,  ebx,  ecx,  edx,  ebp,  esi,  edi : longint);
    end;

Var
  DosError : integer;

{Interrupt}
Procedure Intr(intno: byte; var regs: registers);
Procedure MSDos(var regs: registers);

{Info/Date/Time}
Function  DosVersion: Word;
Procedure GetDate(var year, month, mday, wday: word);
Procedure GetTime(var hour, minute, second, sec100: word);
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
Procedure GetFTime(var f; var tim: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);

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

    function upper(const s : string) : string;
    {
      return uppercased string of s
    }
      var
         i  : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['a'..'z'] then
           upper[i]:=char(byte(s[i])-32)
          else
           upper[i]:=s[i];
        upper[0]:=s[0];
      end;

{******************************************************************************
                           --- Conversion ---
******************************************************************************}

{$ifdef MSWindows}
   function GetLastError : DWORD;stdcall;
     external 'Kernel32.dll' name 'GetLastError';
   function FileTimeToDosDateTime(const ft :TFileTime;var data,time : word) : boolean;stdcall;
     external 'Kernel32.dll' name 'FileTimeToDosDateTime';
   function DosDateTimeToFileTime(date,time : word;var ft :TFileTime) : boolean;stdcall;
     external 'Kernel32.dll' name 'DosDateTimeToFileTime';
   function FileTimeToLocalFileTime(const ft : TFileTime;var lft : TFileTime) : boolean;stdcall;
     external 'Kernel32.dll' name 'FileTimeToLocalFileTime';
   function LocalFileTimeToFileTime(const lft : TFileTime;var ft : TFileTime) : boolean;stdcall;
     external 'Kernel32.dll' name 'LocalFileTimeToFileTime';

type
  Longrec=packed record
    lo,hi : word;
  end;

function Last2DosError(d:dword):integer;
begin
  Last2DosError:=d;
end;


Function DosToWinAttr (Const Attr : Longint) : longint;
begin
  DosToWinAttr:=Attr;
end;


Function WinToDosAttr (Const Attr : Longint) : longint;
begin
  WinToDosAttr:=Attr;
end;


Function DosToWinTime (DTime:longint;Var Wtime : TFileTime):boolean;
var
  lft : TFileTime;
begin
  DosToWinTime:=DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,lft) and
                LocalFileTimeToFileTime(lft,Wtime);
end;


Function WinToDosTime (Const Wtime : TFileTime;var DTime:longint):boolean;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft) and
                FileTimeToDosDateTime(lft,longrec(dtime).hi,longrec(dtime).lo);
end;
{$endif}


{******************************************************************************
                           --- Dos Interrupt ---
******************************************************************************}

procedure intr(intno : byte;var regs : registers);
begin
  { !!!!!!!! }
end;

procedure msdos(var regs : registers);
begin
  { !!!!!!!! }
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function dosversion : word;
begin
  dosversion:=0;
end;


procedure getdate(var year,month,mday,wday : word);
begin
  DecodeDate(Now,Year,Month,MDay);
  WDay:=0;
//  DecodeDateFully(Now,Year,Month,MDay,WDay);
end;


procedure gettime(var hour,minute,second,sec100 : word);
begin
  DecodeTime(Now,Hour,Minute,Second,Sec100);
  Sec100:=Sec100 div 10;
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

{$ifdef MSWindows}
procedure exec(const path : pathstr;const comline : comstr);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWord;
  AppPath,
  AppParam : array[0..255] of char;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  Move(Path[1],AppPath,length(Path));
  AppPath[Length(Path)]:=#0;
  AppParam[0]:='-';
  AppParam[1]:=' ';
  Move(ComLine[1],AppParam[2],length(Comline));
  AppParam[Length(ComLine)+2]:=#0;
  if not CreateProcess(PChar(@AppPath), PChar(@AppParam), Nil, Nil, False,$20, Nil, Nil, SI, PI) then
   begin
     DosError:=Last2DosError(GetLastError);
     exit;
   end
  else
   DosError:=0;
  Proc:=PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, Infinite) <> $ffffffff then
    GetExitCodeProcess(Proc,l)
  else
    l:=$ffffffff;
  CloseHandle(Proc);
  LastDosExitCode:=l;
end;
{$endif MSWindows}
{$ifdef Linux}
Procedure Exec (Const Path: PathStr; Const ComLine: ComStr);
var
  pid,status : longint;
Begin
  LastDosExitCode:=0;
  pid:=Fork;
  if pid=0 then
   begin
   {The child does the actual exec, and then exits}
     Execl(@Path[1],@ComLine[1]);
   {If the execve fails, we return an exitvalue of 127, to let it be known}
     __exit(127);
   end
  else
   if pid=-1 then         {Fork failed}
    begin
      DosError:=8;
      exit
    end;
{We're in the parent, let's wait.}
  WaitPid(Pid,@Status,0);
  LastDosExitCode:=Status; // WaitPid and result-convert
  if (LastDosExitCode>=0) and (LastDosExitCode<>127) then
   DosError:=0
  else
   DosError:=8; // perhaps one time give an better error
End;
{$endif Linux}

function dosexitcode : word;
begin
  dosexitcode:=lastdosexitcode;
end;


procedure swapvectors;
begin
end;


procedure getcbreak(var breakvalue : boolean);
begin
{ !! No Win32 Function !! }
end;


procedure setcbreak(breakvalue : boolean);
begin
{ !! No Win32 Function !! }
end;


procedure getverify(var verify : boolean);
begin
{ !! No Win32 Function !! }
end;


procedure setverify(verify : boolean);
begin
{ !! No Win32 Function !! }
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

{$ifdef Linux]
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
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (statfs(fixdrivestr[drive],fs)=0)) or
     ((not (drivestr[Drive]=nil)) and (statfs(drivestr[drive],fs)=0)) then
   Diskfree:=int64(fs.f_bavail)*int64(fs.f_bsize)
  else
   Diskfree:=-1;
End;

Function DiskSize(Drive: Byte): int64;
var
  fs : tstatfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and (statfs(fixdrivestr[drive],fs)=0)) or
     ((not (drivestr[Drive]=nil)) and (statfs(drivestr[drive],fs)=0)) then
   Disksize:=int64(fs.f_blocks)*int64(fs.f_bsize)
  else
   Disksize:=-1;
End;

{$else linux}

function diskfree(drive : byte) : int64;
begin
  DiskFree:=SysUtils.DiskFree(drive);
end;


function disksize(drive : byte) : int64;
begin
  DiskSize:=SysUtils.DiskSize(drive);
end;

{$endif linux}

{******************************************************************************
                         --- Findfirst FindNext ---
******************************************************************************}

procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
begin
  DosError:=SysUtils.FindFirst(Path,Attr,f);
end;


procedure findnext(var f : searchRec);
begin
  DosError:=Sysutils.FindNext(f);
end;


Procedure FindClose(Var f: SearchRec);
begin
  Sysutils.FindClose(f);
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;var ext : extstr);
var
   p1,i : longint;
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


function fexpand(const path : pathstr) : pathstr;

var
   s,pa : string[79];
   i,j  : longint;
begin
   getdir(0,s);
   pa:=upper(path);
   { allow slash as backslash }
   for i:=1 to length(pa) do
    if pa[i]='/' then
     pa[i]:='\';

   if (length(pa)>1) and (pa[1] in ['A'..'Z']) and (pa[2]=':') then
     begin
        { we must get the right directory }
        getdir(ord(pa[1])-ord('A')+1,s);
        if (ord(pa[0])>2) and (pa[3]<>'\') then
          if pa[1]=s[1] then
            pa:=s+'\'+copy (pa,3,length(pa))
          else
            pa:=pa[1]+':\'+copy (pa,3,length(pa))
     end
   else
     if pa[1]='\' then
       pa:=s[1]+':'+pa
     else if s[0]=#3 then
       pa:=s+pa
     else
       pa:=s+'\'+pa;

 { Turbo Pascal gives current dir on drive if only drive given as parameter! }
 if length(pa) = 2 then
  begin
    getdir(byte(pa[1])-64,s);
    pa := s;
  end;

 {First remove all references to '\.\'}
   while pos ('\.\',pa)<>0 do
    delete (pa,pos('\.\',pa),2);
 {Now remove also all references to '\..\' + of course previous dirs..}
   repeat
     i:=pos('\..\',pa);
     if i<>0 then
      begin
        j:=i-1;
        while (j>1) and (pa[j]<>'\') do
         dec (j);
        if pa[j+1] = ':' then j := 3;
        delete (pa,j,i-j+3);
      end;
   until i=0;

   { Turbo Pascal gets rid of a \.. at the end of the path }
   { Now remove also any reference to '\..'  at end of line
     + of course previous dir.. }
   i:=pos('\..',pa);
   if i<>0 then
    begin
      if i = length(pa) - 2 then
       begin
         j:=i-1;
         while (j>1) and (pa[j]<>'\') do
          dec (j);
         delete (pa,j,i-j+3);
       end;
       pa := pa + '\';
     end;
   { Remove End . and \}
   if (length(pa)>0) and (pa[length(pa)]='.') then
    dec(byte(pa[0]));
   { if only the drive + a '\' is left then the '\' should be left to prevtn the program
     accessing the current directory on the drive rather than the root!}
   { if the last char of path = '\' then leave it in as this is what TP does! }
   if ((length(pa)>3) and (pa[length(pa)]='\')) and (path[length(path)] <> '\') then
    dec(byte(pa[0]));
   { if only a drive is given in path then there should be a '\' at the
     end of the string given back }
   if length(path) = 2 then pa := pa + '\';
   fexpand:=pa;
end;

Function FSearch(path: pathstr; dirlist: string): pathstr;
var
   i,p1   : longint;
   s      : searchrec;
   newdir : pathstr;
begin
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
          if p1=0 then
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
end;


procedure getftime(var f;var tim : longint);
begin
  tim:=FileGetDate(filerec(f).handle);
end;


procedure setftime(var f;time : longint);
begin
{$ifdef linux}
  FileSetDate(filerec(f).name,Time);
{$else}
  FileSetDate(filerec(f).handle,Time);
{$endif}
end;


{$ifdef linux}
procedure getfattr(var f;var attr : word);
Var
  info : tstatbuf;
  LinAttr : longint;
Begin
  DosError:=0;
  if (FStat(filerec(f).handle,info)<>0) then
   begin
     Attr:=0;
     DosError:=3;
     exit;
   end
  else
   LinAttr:=Info.st_Mode;
  if S_ISDIR(LinAttr) then
   Attr:=$10
  else
   Attr:=$20;
  if Access(@filerec(f).name,W_OK)<>0 then
   Attr:=Attr or $1;
  if (not S_ISDIR(LinAttr)) and (filerec(f).name[0]='.')  then
   Attr:=Attr or $2;
end;
{$else}
procedure getfattr(var f;var attr : word);
var
   l : longint;
begin
  l:=FileGetAttr(filerec(f).name);
  attr:=l;
end;
{$endif}


procedure setfattr(var f;attr : word);
begin
{$ifdef MSWindows}
  FileSetAttr(filerec(f).name,attr);
{$endif}
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

{
  The environment is a block of zero terminated strings
  terminated by a #0
}

{$ifdef MSWindows}
   function GetEnvironmentStrings : pchar;stdcall;
     external 'Kernel32.dll' name 'GetEnvironmentStringsA';
   function FreeEnvironmentStrings(p : pchar) : boolean;stdcall;
     external 'Kernel32.dll' name 'FreeEnvironmentStringsA';

function envcount : longint;
var
   hp,p : pchar;
   count : longint;
begin
   p:=GetEnvironmentStrings;
   hp:=p;
   count:=0;
   while  hp^<>#0 do
     begin
        { next string entry}
        hp:=hp+strlen(hp)+1;
        inc(count);
     end;
   FreeEnvironmentStrings(p);
   envcount:=count;
end;


Function  EnvStr(index: integer): string;
var
   hp,p : pchar;
   count,i : longint;
begin
   { envcount takes some time in win32 }
   count:=envcount;

   { range checking }
   if (index<=0) or (index>count) then
     begin
        envstr:='';
        exit;
     end;
   p:=GetEnvironmentStrings;
   hp:=p;

   { retrive the string with the given index }
   for i:=2 to index do
     hp:=hp+strlen(hp)+1;

   envstr:=strpas(hp);
   FreeEnvironmentStrings(p);
end;


Function  GetEnv(envvar: string): string;
var
   s : string;
   i : longint;
   hp,p : pchar;
begin
   getenv:='';
   p:=GetEnvironmentStrings;
   hp:=p;
   while hp^<>#0 do
     begin
        s:=strpas(hp);
        i:=pos('=',s);
        if copy(s,1,i-1)=envvar then
          begin
             getenv:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
   FreeEnvironmentStrings(p);
end;
{$else}

function envcount : longint;
begin
   envcount:=0;
end;


Function  EnvStr(index: integer): string;
begin
   envstr:='';
end;


Function  GetEnv(envvar: string): string;
begin
   getenv:=GetEnvironmentVariable(envvar);
end;

{$endif}


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

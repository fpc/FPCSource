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

Const
  Max_Path = 260;

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
{ Needed for Win95 LFN Support }
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

  TWin32Handle = longint;

  PWin32FileTime = ^TWin32FileTime;
  TWin32FileTime = record
    dwLowDateTime,
    dwHighDateTime : DWORD;
  end;

  PWin32FindData = ^TWin32FindData;
  TWin32FindData = record
    dwFileAttributes: Cardinal;
    ftCreationTime: TWin32FileTime;
    ftLastAccessTime: TWin32FileTime;
    ftLastWriteTime: TWin32FileTime;
    nFileSizeHigh: Cardinal;
    nFileSizeLow: Cardinal;
    dwReserved0: Cardinal;
    dwReserved1: Cardinal;
    cFileName: array[0..MAX_PATH - 1] of Char;
    cAlternateFileName: array[0..13] of Char;
    // The structure should be 320 bytes long...
    pad : system.integer;
  end;

  Searchrec = Packed Record
    FindHandle  : TWin32Handle;
    W32FindData : TWin32FindData;
    ExcludeAttr : longint;
    time : longint;
    size : longint;
    attr : longint;
    name : string;
  end;


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

Const
  { allow EXEC to inherited handles from calling process,
    needed for FPREDIR in ide/text
    now set to true by default because
    other OS also pass open handles to childs
    finally reset to false after Florian's response PM }
  ExecInheritsHandles : Longbool = false;


implementation

uses
   strings;

const
   INVALID_HANDLE_VALUE = longint($ffffffff);

   VER_PLATFORM_WIN32s = 0;
   VER_PLATFORM_WIN32_WINDOWS = 1;
   VER_PLATFORM_WIN32_NT = 2;

type
   OSVERSIONINFO = record
        dwOSVersionInfoSize : DWORD;
        dwMajorVersion : DWORD;
        dwMinorVersion : DWORD;
        dwBuildNumber : DWORD;
        dwPlatformId : DWORD;
        szCSDVersion : array[0..127] of char;
     end;

var
   versioninfo : OSVERSIONINFO;
   kernel32dll : TWin32Handle;

{******************************************************************************
                           --- Conversion ---
******************************************************************************}

   function GetLastError : DWORD;
     stdcall; external 'kernel32' name 'GetLastError';
   function FileTimeToDosDateTime(const ft :TWin32FileTime;var data,time : word) : longbool;
     stdcall; external 'kernel32' name 'FileTimeToDosDateTime';
   function DosDateTimeToFileTime(date,time : word;var ft :TWin32FileTime) : longbool;
     stdcall; external 'kernel32' name 'DosDateTimeToFileTime';
   function FileTimeToLocalFileTime(const ft : TWin32FileTime;var lft : TWin32FileTime) : longbool;
     stdcall; external 'kernel32' name 'FileTimeToLocalFileTime';
   function LocalFileTimeToFileTime(const lft : TWin32FileTime;var ft : TWin32FileTime) : longbool;
     stdcall; external 'kernel32' name 'LocalFileTimeToFileTime';

type
  Longrec=packed record
    lo,hi : word;
  end;

function Last2DosError(d:dword):integer;
begin
  case d of
    87 : { Parameter invalid -> Data invalid }
      Last2DosError:=13;
    else
      Last2DosError:=d;
  end;
end;


Function DosToWinAttr (Const Attr : Longint) : longint;
begin
  DosToWinAttr:=Attr;
end;


Function WinToDosAttr (Const Attr : Longint) : longint;
begin
  WinToDosAttr:=Attr;
end;


Function DosToWinTime (DTime:longint;Var Wtime : TWin32FileTime):longbool;
var
  lft : TWin32FileTime;
begin
  DosToWinTime:=DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,lft) and
                LocalFileTimeToFileTime(lft,Wtime);
end;


Function WinToDosTime (Const Wtime : TWin32FileTime;var DTime:longint):longbool;
var
  lft : TWin32FileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft) and
                FileTimeToDosDateTime(lft,longrec(dtime).hi,longrec(dtime).lo);
end;


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

type
  TSystemTime = record
    wYear,
    wMonth,
    wDayOfWeek,
    wDay,
    wHour,
    wMinute,
    wSecond,
    wMilliseconds: Word;
  end;

   function GetVersion : longint;
     stdcall; external 'kernel32' name 'GetVersion';
   procedure GetLocalTime(var t : TSystemTime);
     stdcall; external 'kernel32' name 'GetLocalTime';
   function SetLocalTime(const t : TSystemTime) : longbool;
     stdcall; external 'kernel32' name 'SetLocalTime';

function dosversion : word;
begin
  dosversion:=GetVersion and $ffff;
end;


procedure getdate(var year,month,mday,wday : word);
var
  t : TSystemTime;
begin
  GetLocalTime(t);
  year:=t.wYear;
  month:=t.wMonth;
  mday:=t.wDay;
  wday:=t.wDayOfWeek;
end;


procedure setdate(year,month,day : word);
var
  t : TSystemTime;
begin
  { we need the time set privilege   }
  { so this function crash currently }
  {!!!!!}
  GetLocalTime(t);
  t.wYear:=year;
  t.wMonth:=month;
  t.wDay:=day;
  { only a quite good solution, we can loose some ms }
  SetLocalTime(t);
end;


procedure gettime(var hour,minute,second,sec100 : word);
var
  t : TSystemTime;
begin
   GetLocalTime(t);
   hour:=t.wHour;
   minute:=t.wMinute;
   second:=t.wSecond;
   sec100:=t.wMilliSeconds div 10;
end;


procedure settime(hour,minute,second,sec100 : word);
var
   t : TSystemTime;
begin
   { we need the time set privilege   }
   { so this function crash currently }
   {!!!!!}
   GetLocalTime(t);
   t.wHour:=hour;
   t.wMinute:=minute;
   t.wSecond:=second;
   t.wMilliSeconds:=sec100*10;
   SetLocalTime(t);
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

type
  PProcessInformation = ^TProcessInformation;
  TProcessInformation = record
    hProcess: TWin32Handle;
    hThread: TWin32Handle;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
  end;

   function CreateProcess(lpApplicationName: PChar; lpCommandLine: PChar;
               lpProcessAttributes, lpThreadAttributes: Pointer;
               bInheritHandles: Longbool; dwCreationFlags: DWORD; lpEnvironment: Pointer;
               lpCurrentDirectory: PChar; const lpStartupInfo: TStartupInfo;
               var lpProcessInformation: TProcessInformation): longbool;
     stdcall; external 'kernel32' name 'CreateProcessA';
   function getExitCodeProcess(h:TWin32Handle;var code:longint):longbool;
     stdcall; external 'kernel32' name 'GetExitCodeProcess';
   function WaitForSingleObject(hHandle: TWin32Handle; dwMilliseconds: DWORD): DWORD;
     stdcall; external 'kernel32' name 'WaitForSingleObject';
   function CloseHandle(h : TWin32Handle) : longint;
     stdcall; external 'kernel32' name 'CloseHandle';

var
  lastdosexitcode : longint;

procedure exec(const path : pathstr;const comline : comstr);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : TWin32Handle;
  l    : Longint;
  CommandLine : array[0..511] of char;
  AppParam : array[0..255] of char;
  pathlocal : string;
begin
  DosError := 0;
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  { always surroound the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"',path) = 0 then
    pathlocal:='"'+path+'"'
  else
    pathlocal := path;
  Move(Pathlocal[1],CommandLine,length(Pathlocal));

  AppParam[0]:=' ';
  AppParam[1]:=' ';
  Move(ComLine[1],AppParam[2],length(Comline));
  AppParam[Length(ComLine)+2]:=#0;
  { concatenate both pathnames }
  Move(Appparam[0],CommandLine[length(Pathlocal)],strlen(Appparam)+1);
  if not CreateProcess(nil, PChar(@CommandLine),
           Nil, Nil, ExecInheritsHandles,$20, Nil, Nil, SI, PI) then
   begin
     DosError:=Last2DosError(GetLastError);
     exit;
   end;
  Proc:=PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, dword($ffffffff)) <> $ffffffff then
    GetExitCodeProcess(Proc,l)
  else
    l:=-1;
  CloseHandle(Proc);
  LastDosExitCode:=l;
end;


function dosexitcode : word;
begin
  dosexitcode:=lastdosexitcode and $ffff;
end;


procedure getcbreak(var breakvalue : boolean);
begin
{ !! No Win32 Function !! }
  breakvalue := true;
end;


procedure setcbreak(breakvalue : boolean);
begin
{ !! No Win32 Function !! }
end;


procedure getverify(var verify : boolean);
begin
{ !! No Win32 Function !! }
 verify := true;
end;


procedure setverify(verify : boolean);
begin
{ !! No Win32 Function !! }
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

   function GetDiskFreeSpace(drive:pchar;var sector_cluster,bytes_sector,
                             freeclusters,totalclusters:longint):longbool;
     stdcall; external 'kernel32' name 'GetDiskFreeSpaceA';
type
   TGetDiskFreeSpaceEx = function(drive:pchar;var availableforcaller,
                             total,free):longbool;stdcall;

var
   GetDiskFreeSpaceEx : TGetDiskFreeSpaceEx;

function diskfree(drive : byte) : int64;
var
  disk : array[1..4] of char;
  secs,bytes,
  free,total : longint;
  qwtotal,qwfree,qwcaller : int64;


begin
  if drive=0 then
   begin
     disk[1]:='\';
     disk[2]:=#0;
   end
  else
   begin
     disk[1]:=chr(drive+64);
     disk[2]:=':';
     disk[3]:='\';
     disk[4]:=#0;
   end;
  if assigned(GetDiskFreeSpaceEx) then
    begin
       if GetDiskFreeSpaceEx(@disk,qwcaller,qwtotal,qwfree) then
         diskfree:=qwfree
       else
         diskfree:=-1;
    end
  else
    begin
       if GetDiskFreeSpace(@disk,secs,bytes,free,total) then
         diskfree:=int64(free)*secs*bytes
       else
         diskfree:=-1;
    end;
end;


function disksize(drive : byte) : int64;
var
  disk : array[1..4] of char;
  secs,bytes,
  free,total : longint;
  qwtotal,qwfree,qwcaller : int64;

begin
  if drive=0 then
   begin
     disk[1]:='\';
     disk[2]:=#0;
   end
  else
   begin
     disk[1]:=chr(drive+64);
     disk[2]:=':';
     disk[3]:='\';
     disk[4]:=#0;
   end;
  if assigned(GetDiskFreeSpaceEx) then
    begin
       if GetDiskFreeSpaceEx(@disk,qwcaller,qwtotal,qwfree) then
         disksize:=qwtotal
       else
         disksize:=-1;
    end
  else
    begin
       if GetDiskFreeSpace(@disk,secs,bytes,free,total) then
         disksize:=int64(total)*secs*bytes
       else
         disksize:=-1;
    end;
end;


{******************************************************************************
                         --- Findfirst FindNext ---
******************************************************************************}

{ Needed kernel calls }

   function FindFirstFile (lpFileName: PChar; var lpFindFileData: TWIN32FindData): TWin32Handle;
     stdcall; external 'kernel32' name 'FindFirstFileA';
   function FindNextFile  (hFindFile: TWin32Handle; var lpFindFileData: TWIN32FindData): LongBool;
     stdcall; external 'kernel32' name 'FindNextFileA';
   function FindCloseFile (hFindFile: TWin32Handle): LongBool;
     stdcall; external 'kernel32' name 'FindClose';

Procedure StringToPchar (Var S : String);
Var L : Longint;
begin
  L:=ord(S[0]);
  Move (S[1],S[0],L);
  S[L]:=#0;
end;

Procedure PCharToString (Var S : String);
Var L : Longint;
begin
  L:=strlen(pchar(@S[0]));
  Move (S[0],S[1],L);
  S[0]:=char(l);
end;


procedure FindMatch(var f:searchrec);
begin
{ Find file with correct attribute }
  While (F.W32FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if not FindNextFile (F.FindHandle,F.W32FindData) then
      begin
        DosError:=Last2DosError(GetLastError);
        if DosError=2 then
         DosError:=18;
        exit;
      end;
   end;
{ Convert some attributes back }
  f.size:=F.W32FindData.NFileSizeLow;
  f.attr:=WinToDosAttr(F.W32FindData.dwFileAttributes);
  WinToDosTime(F.W32FindData.ftLastWriteTime,f.Time);
  f.Name:=StrPas(@F.W32FindData.cFileName);
end;


procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
begin
{ no error }
  doserror:=0;
  F.Name:=Path;
  F.Attr:=attr;
  F.ExcludeAttr:=(not Attr) and ($1e); {hidden,sys,dir,volume}
  StringToPchar(f.name);
{ FindFirstFile is a Win32 Call }
  F.FindHandle:=FindFirstFile (pchar(@f.Name),F.W32FindData);
  If longint(F.FindHandle)=Invalid_Handle_value then
   begin
     DosError:=Last2DosError(GetLastError);
     if DosError=2 then
      DosError:=18;
     exit;
   end;
{ Find file with correct attribute }
  FindMatch(f);
end;


procedure findnext(var f : searchRec);
begin
{ no error }
  doserror:=0;
  if not FindNextFile (F.FindHandle,F.W32FindData) then
   begin
     DosError:=Last2DosError(GetLastError);
     if DosError=2 then
      DosError:=18;
     exit;
   end;
{ Find file with correct attribute }
  FindMatch(f);
end;


procedure swapvectors;
begin
end;


Procedure FindClose(Var f: SearchRec);
begin
  If longint(F.FindHandle)<>Invalid_Handle_value then
   FindCloseFile(F.FindHandle);
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

   function GeTWin32FileTime(h : longint;creation,lastaccess,lastwrite : PWin32FileTime) : longbool;
     stdcall; external 'kernel32' name 'GetFileTime';
   function SeTWin32FileTime(h : longint;creation,lastaccess,lastwrite : PWin32FileTime) : longbool;
     stdcall; external 'kernel32' name 'SetFileTime';
   function SetFileAttributes(lpFileName : pchar;dwFileAttributes : longint) : longbool;
     stdcall; external 'kernel32' name 'SetFileAttributesA';
   function GetFileAttributes(lpFileName : pchar) : longint;
     stdcall; external 'kernel32' name 'GetFileAttributesA';

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
end;

{ <immobilizer> }

function GetFullPathName(lpFileName: PChar; nBufferLength: Longint; lpBuffer: PChar; var lpFilePart : PChar):DWORD;
    stdcall; external 'kernel32' name 'GetFullPathNameA';

function GetShortPathName(lpszLongPath:pchar; lpszShortPath:pchar; cchBuffer:DWORD):DWORD;
    stdcall; external 'kernel32' name 'GetShortPathNameA';


(*
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
*)

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_DRIVES}
{$UNDEF FPC_FEXPAND_UNC}


  function SearchPath(lpPath : PChar; lpFileName : PChar; lpExtension : PChar; nBufferLength : Longint; lpBuffer : PChar;
    var lpFilePart : PChar) : Longint; stdcall; external 'kernel32' name 'SearchPathA';

Function FSearch(path: pathstr; dirlist: string): pathstr;
var temp        : PChar;
    value       : Array [0..255] of char;
    i           : Longint;
    dir,dir2    : dirstr;
    lastchar    : char;
    name        : namestr;
    ext         : extstr;
    s           : SearchRec;
    found       : boolean;
begin
{ check if the file specified exists }
  findfirst(path,anyfile,s);
  found:=(doserror=0);
  findclose(s);
  if found then
   begin
     fsearch:=path;
     exit;
   end;
{ search the path }
  fsearch:='';

  for i:=1 to length(path) do
   if path[i]='/' then
    path[i]:='\';
  fsplit(path,dir,name,ext);
  for i:=1 to length(dirlist) do
   if dirlist[i]='/' then
    dirlist[i]:='\';
  { bugfix here : Win98SE returns a path, when the name is NULL! }
  { so if the name of the file to search is '' then simply exit  }
  { immediately (WinNT behavior is correct).                     }
  if name='' then
    exit;

  { allow slash as backslash }
  StringToPchar(name);
  StringToPchar(ext);

  StringToPchar(dir);
  if SearchPath(@dir, @name, @ext, 255, @value, temp)>0 then
    begin
      fsearch := strpas(value);
      exit;
    end;
  PCharToString(dir);

  repeat
    i:=pos(';',dirlist);
    while i=1 do
      begin
        delete(dirlist,1,1);
        i:=pos(';',dirlist);
      end;
    if i=0 then
      begin
        dir2:=dirlist;
        dirlist:='';
      end
    else
      begin
        dir2:=Copy(dirlist,1,i-1);
        dirlist:=Copy(dirlist,i+1,255);
      end;
  { don't add anything if dir2 is empty string }
  if dir2<>'' then
    lastchar:=dir2[length(dir2)]
  else
    lastchar:='\';
  if (lastchar<>'\') and (lastchar<>':') then
    dir2:=dir2+'\'+dir
  else
    dir2:=dir2+dir;
  StringToPchar(dir2);
  if SearchPath(@dir2, @name, @ext, 255, @value, temp)>0 then
    begin
      fsearch := strpas(value);
      exit;
    end;
  until dirlist='';

end;

{ </immobilizer> }

procedure getftime(var f;var time : longint);
var
   ft : TWin32FileTime;
begin
  doserror:=0;
  if GeTWin32FileTime(filerec(f).Handle,nil,nil,@ft) and
     WinToDosTime(ft,time) then
    exit
  else
    begin
      DosError:=Last2DosError(GetLastError);
      time:=0;
    end;
end;


procedure setftime(var f;time : longint);
var
  ft : TWin32FileTime;
begin
  doserror:=0;
  if DosToWinTime(time,ft) and
     SeTWin32FileTime(filerec(f).Handle,nil,nil,@ft) then
   exit
  else
   DosError:=Last2DosError(GetLastError);
end;


procedure getfattr(var f;var attr : word);
var
   l : longint;
begin
  doserror:=0;
  l:=GetFileAttributes(filerec(f).name);
  if l=longint($ffffffff) then
   begin
     doserror:=getlasterror;
     attr:=0;
   end
  else
   attr:=l and $ffff;
end;


procedure setfattr(var f;attr : word);
begin
  doserror:=0;
  if not(SetFileAttributes(filerec(f).name,attr)) then
    doserror:=getlasterror;
end;

{ change to short filename if successful win32 call PM }
function GetShortName(var p : String) : boolean;
var
  buffer   : array[0..255] of char;
  ret : longint;
begin
  {we can't mess with p, because we have to return it if call is
      unsuccesfully.}

  if Length(p)>0 then                   {copy p to array of char}
   move(p[1],buffer[0],length(p));
  buffer[length(p)]:=chr(0);

  {Should return value load loaddoserror?}

  ret:=GetShortPathName(@buffer,@buffer,255);
  if ret=0 then
   p:=strpas(buffer);
  GetShortName:=ret<>0;
end;

{ change to long filename if successful DOS call PM }
function GetLongName(var p : String) : boolean;

var
  lfn,sfn   : array[0..255] of char;
  filename  : pchar;
  ret       : longint;
begin
  {contrary to shortname, SDK does not mention input buffer can be equal
   to output.}

  if Length(p)>0 then                   {copy p to array of char}
   move(p[1],sfn[0],length(p));
  sfn[length(p)]:=chr(0);
  fillchar(lfn,sizeof(lfn),#0);
  filename:=nil;

  {Should return value load loaddoserror?}

  ret:=GetFullPathName(@sfn,255,@lfn,filename);
  if ret=0 then
   p:=strpas(lfn);              {lfn here returns full path, filename only fn}
  GetLongName:=ret<>0;
end;

{******************************************************************************
                             --- Environment ---
******************************************************************************}

{
  The environment is a block of zero terminated strings
  terminated by a #0
}

   function GetEnvironmentStrings : pchar;
     stdcall; external 'kernel32' name 'GetEnvironmentStringsA';
   function FreeEnvironmentStrings(p : pchar) : longbool;
     stdcall; external 'kernel32' name 'FreeEnvironmentStringsA';

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
        if upcase(copy(s,1,i-1))=upcase(envvar) then
          begin
             getenv:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
   FreeEnvironmentStrings(p);
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

function FreeLibrary(hLibModule : TWin32Handle) : longbool;
  stdcall; external 'kernel32' name 'FreeLibrary';
function GetVersionEx(var VersionInformation:OSVERSIONINFO) : longbool;
  stdcall; external 'kernel32' name 'GetVersionExA';
function LoadLibrary(lpLibFileName : pchar):TWin32Handle;
  stdcall; external 'kernel32' name 'LoadLibraryA';
function GetProcAddress(hModule : TWin32Handle;lpProcName : pchar) : pointer;
  stdcall; external 'kernel32' name 'GetProcAddress';

var
   oldexitproc : pointer;

procedure dosexitproc;

  begin
     exitproc:=oldexitproc;
     if kernel32dll<>0 then
       FreeLibrary(kernel32dll);
  end;

begin
   oldexitproc:=exitproc;
   exitproc:=@dosexitproc;
   versioninfo.dwOSVersionInfoSize:=sizeof(versioninfo);
   GetVersionEx(versioninfo);
   kernel32dll:=0;
   GetDiskFreeSpaceEx:=nil;
   if ((versioninfo.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS) and
     (versioninfo.dwBuildNUmber>=1000)) or
     (versioninfo.dwPlatformId=VER_PLATFORM_WIN32_NT) then
     begin
        kernel32dll:=LoadLibrary('kernel32');
        if kernel32dll<>0 then
          GetDiskFreeSpaceEx:=TGetDiskFreeSpaceEx(GetProcAddress(kernel32dll,'GetDiskFreeSpaceExA'));
     end;
end.
{
  $Log$
  Revision 1.20  2003-09-17 15:06:36  peter
    * stdcall patch

  Revision 1.19  2003/06/10 11:16:15  jonas
    * fix from Peter

  Revision 1.18  2002/12/24 15:35:15  peter
    * error code fixes

  Revision 1.17  2002/12/15 20:23:53  peter
    * map error 87 to 13 to be compatible with dos

  Revision 1.16  2002/12/04 21:35:50  carl
    * bugfixes for dos.exec() : it would not be able to execute 16-bit apps
    * doserror was not reset to zero in dos.exec

  Revision 1.15  2002/12/03 20:39:14  carl
     * fix for dos.exec with non-microsoft shells

  Revision 1.14  2002/09/07 16:01:28  peter
    * old logs removed and tabs fixed

  Revision 1.13  2002/07/06 11:48:09  carl
  + fsearch bugfix for Win9X systems

  Revision 1.12  2002/05/16 19:32:57  carl
  * fix range check error

}

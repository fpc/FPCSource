{
    $Id: dos.pp,v 1.30 2005/02/14 17:13:32 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team.

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
  Max_Path    = 260;

Type
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

{$i dosh.inc}

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

{$DEFINE HAS_GETMSCOUNT}
{$DEFINE HAS_GETSHORTNAME}
{$DEFINE HAS_GETLONGNAME}

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$I dos.inc}

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
   function GetTickCount : longint;
     stdcall;external 'kernel32' name 'GetTickCount';

function GetMsCount: int64;
begin
  GetMsCount := cardinal (GetTickCount);
end;

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
  fillchar(f,sizeof(f),0);
  { no error }
  doserror:=0;
  F.Name:=Path;
  F.Attr:=attr;
  F.ExcludeAttr:=(not Attr) and ($1e); {hidden,sys,dir,volume}
  StringToPchar(f.name);

  { FindFirstFile is a Win32 Call }
  F.W32FindData.dwFileAttributes:=DosToWinAttr(f.attr);
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


{ <immobilizer> }

function GetFullPathName(lpFileName: PChar; nBufferLength: Longint; lpBuffer: PChar; var lpFilePart : PChar):DWORD;
    stdcall; external 'kernel32' name 'GetFullPathNameA';

function GetShortPathName(lpszLongPath:pchar; lpszShortPath:pchar; cchBuffer:DWORD):DWORD;
    stdcall; external 'kernel32' name 'GetShortPathNameA';


Function FSearch(path: pathstr; dirlist: string): pathstr;
var
  i,p1   : longint;
  s      : searchrec;
  newdir : pathstr;
begin
  { check if the file specified exists }
  findfirst(path,anyfile and not(directory),s);
  if doserror=0 then
   begin
     findclose(s);
     fsearch:=path;
     exit;
   end;
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
       until (dirlist='') or (newdir<>'');
       fsearch:=newdir;
    end;
  findclose(s);
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
  { Fail for setting VolumeId }
  if (attr and VolumeID)<>0 then
    doserror:=5
  else
   if SetFileAttributes(filerec(f).name,attr) then
    doserror:=0
  else
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


Function EnvStr (Index: longint): string;
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
  $Log: dos.pp,v $
  Revision 1.30  2005/02/14 17:13:32  peter
    * truncate log

}

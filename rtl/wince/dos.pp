{
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

uses windows;

Const
  Max_Path = MaxPathLen;

Type
  Searchrec = Packed Record
    FindHandle  : THandle;
    W32FindData : TWin32FindData;
    ExcludeAttr : longint;
    time : longint;
    size : longint;
    attr : longint;
    name : string;
  end;

{$i dosh.inc}

//Const
  { allow EXEC to inherited handles from calling process,
    needed for FPREDIR in ide/text
    now set to true by default because
    other OS also pass open handles to childs
    finally reset to false after Florian's response PM }
//  ExecInheritsHandles : Longbool = false;


implementation

{$DEFINE HAS_GETMSCOUNT}

{$DEFINE FPC_FEXPAND_NO_DEFAULT_PATHS}
{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)

{$I dos.inc}

{******************************************************************************
                           --- Conversion ---
******************************************************************************}

function GetMsCount: int64;
begin
  GetMsCount := cardinal (GetTickCount);
end;

function Last2DosError(d:dword):integer;
begin
  case d of
    87 : { Parameter invalid -> Data invalid }
      Last2DosError:=13;
    else
      Last2DosError:=integer(d);
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


Function DosToWinTime (DTime:longint;Var Wtime : TFileTime):longbool;
begin
  DosToWinTime:=False;  //!!! fixme
end;


Function WinToDosTime (Const Wtime : TFileTime;var DTime:longint):longbool;
begin
  WinToDosTime:=False; //!!! fixme
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function dosversion : word;
var
  versioninfo : OSVERSIONINFO;
begin
  versioninfo.dwOSVersionInfoSize:=sizeof(versioninfo);
  GetVersionEx(versioninfo);
  dosversion:=versioninfo.dwMajorVersion and $FF or versioninfo.dwMinorVersion and $FF shl 8;
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

procedure exec(const path : pathstr;const comline : comstr);
var
  PI: TProcessInformation;
  Proc : THandle;
  l    : LongInt;
  PathW : array[0..FileNameLen] of WideChar;
  CmdLineW : array[0..FileNameLen] of WideChar;
begin
  DosError := 0;
  AnsiToWideBuf(@path[1], Length(path), PathW, SizeOf(PathW));
  AnsiToWideBuf(@comline[1], Length(comline), CmdLineW, SizeOf(CmdLineW));
  if not CreateProcess(PathW, CmdLineW,
           nil, nil, FALSE, 0, nil, nil, nil, PI) then
   begin
     DosError:=Last2DosError(GetLastError);
     exit;
   end;
  Proc:=PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, dword($ffffffff)) <> $ffffffff then
    GetExitCodeProcess(Proc, @l)
  else
    l:=-1;
  CloseHandle(Proc);
  LastDosExitCode:=l;
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

var
  DriveNames: array[1..24] of PWideChar;

function GetDriveName(drive: byte): PWideChar;
const
  dev_attr = FILE_ATTRIBUTE_TEMPORARY or FILE_ATTRIBUTE_DIRECTORY;

var
  h: THandle;
  fd: TWin32FindData;
  i, len: LongInt;
begin
  GetDriveName:=nil;
  if (drive < 3) or (drive > 26) then
    exit;
  if DriveNames[1] = nil then
  begin
    GetMem(DriveNames[1], 2*SizeOf(WideChar));
    DriveNames[1][0]:='\';
    DriveNames[1][1]:=#0;
    
    h:=FindFirstFile('\*', @fd);
    if h <> 0 then
    begin
      i:=2;
      repeat
        if fd.dwFileAttributes and dev_attr = dev_attr then begin
          len:=0;
          while fd.cFileName[len] <> 0 do
            Inc(len);
          len:=(len + 2)*SizeOf(WideChar);
          GetMem(DriveNames[i], len);
          DriveNames[i]^:='\';
          Move(fd.cFileName, DriveNames[i][1], len - SizeOf(WideChar));
          Inc(i);
        end;
      until (i > 24) or not FindNextFile(h, fd);
      Windows.FindClose(h);
    end;
  end;
  GetDriveName:=DriveNames[drive - 2];
end;

function diskfree(drive : byte) : int64;
var
  disk: PWideChar;
  qwtotal,qwfree,qwcaller : int64;
begin
  disk:=GetDriveName(drive);
  if (disk <> nil) and GetDiskFreeSpaceEx(disk, @qwcaller, @qwtotal, @qwfree) then
    diskfree:=qwfree
  else
    diskfree:=-1;
end;


function disksize(drive : byte) : int64;
var
  disk : PWideChar;
  qwtotal,qwfree,qwcaller : int64;
begin
  disk:=GetDriveName(drive);
  if (disk <> nil) and GetDiskFreeSpaceEx(disk, @qwcaller, @qwtotal, @qwfree) then
    disksize:=qwtotal
  else
    disksize:=-1;
end;


{******************************************************************************
                         --- Findfirst FindNext ---
******************************************************************************}

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
var
  buf: array[0..MaxPathLen] of char;
begin
  { Find file with correct attribute }
  While (F.W32FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if not FindNextFile (F.FindHandle, F.W32FindData) then
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
  WideToAnsiBuf(@F.W32FindData.cFileName, -1, buf, SizeOf(buf));
  f.Name:=StrPas(@buf);
end;


procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
var
  buf: array[0..MaxPathLen] of WideChar;
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
  AnsiToWideBuf(@f.Name, -1, buf, SizeOf(buf));
  F.FindHandle:=FindFirstFile (buf, F.W32FindData);

  If F.FindHandle = Invalid_Handle_value then
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
  if not FindNextFile (F.FindHandle, F.W32FindData) then
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
  If F.FindHandle <> Invalid_Handle_value then
    Windows.FindClose(F.FindHandle);
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

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
   ft : TFileTime;
begin
  doserror:=0;
  if GetFileTime(filerec(f).Handle,nil,nil,@ft) and
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
  ft : TFileTime;
begin
  doserror:=0;
  if DosToWinTime(time,ft) and
     SetFileTime(filerec(f).Handle,nil,nil,@ft) then
   exit
  else
   DosError:=Last2DosError(GetLastError);
end;


procedure getfattr(var f;var attr : word);
var
  l : longint;
  buf: array[0..MaxPathLen] of WideChar;
begin
  doserror:=0;
  AnsiToWideBuf(@filerec(f).name, -1, buf, SizeOf(buf));
  l:=GetFileAttributes(buf);
  if l=longint($ffffffff) then
   begin
     doserror:=Last2DosError(GetLastError);
     attr:=0;
   end
  else
   attr:=l and $ffff;
end;


procedure setfattr(var f;attr : word);
var
  buf: array[0..MaxPathLen] of WideChar;
begin
  { Fail for setting VolumeId }
  if (attr and VolumeID)<>0 then
    doserror:=5
  else
   AnsiToWideBuf(@filerec(f).name, -1, buf, SizeOf(buf));
   if SetFileAttributes(buf,attr) then
    doserror:=0
  else
    doserror:=Last2DosError(GetLastError);
end;

{******************************************************************************
                             --- Environment ---
******************************************************************************}

// WinCE does not have environment. It can be emulated via registry or file. (YS)

function envcount : longint;
begin
  envcount:=0; //!!! fixme
end;

Function EnvStr (Index: longint): string;
begin
  EnvStr:=''; //!!! fixme
end;

Function  GetEnv(envvar: string): string;
begin
  GetEnv:=''; //!!! fixme
end;

{
  The environment is a block of zero terminated strings
  terminated by a #0
}
(*
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
*)

var
  oldexitproc : pointer;

procedure dosexitproc;
var
  i: LongInt;
begin
  exitproc:=oldexitproc;
  if DriveNames[1] <> nil then
    for i:=1 to 24 do
      if DriveNames[i] <> nil then
        FreeMem(DriveNames[i])
      else
        break;
end;

begin
  oldexitproc:=exitproc;
  exitproc:=@dosexitproc;
end.

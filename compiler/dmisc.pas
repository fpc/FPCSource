{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Dos unit for BP7 compatible RTL for Delphi

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit dmisc;

interface

uses
   windows,sysutils;

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

  PWin32FindData = ^TWin32FindData;
  TWin32FindData = packed record
    dwFileAttributes: Cardinal;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: Cardinal;
    nFileSizeLow: Cardinal;
    dwReserved0: Cardinal;
    dwReserved1: Cardinal;
    cFileName: array[0..MAX_PATH - 1] of Char;
    cAlternateFileName: array[0..13] of Char;
  end;

  Searchrec = Packed Record
    FindHandle  : THandle;
    W32FindData : TWin32FindData;
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
uses globals;

{******************************************************************************
                           --- Conversion ---
******************************************************************************}

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

   function GetVersion : longint;stdcall;
     external 'Kernel32.dll' name 'GetVersion';
   procedure GetLocalTime(var t : TSystemTime);stdcall;
     external 'Kernel32.dll' name 'GetLocalTime';
   function SetLocalTime(const t : TSystemTime) : boolean;stdcall;
     external 'Kernel32.dll' name 'SetLocalTime';

function dosversion : word;
begin
  dosversion:=GetVersion;
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
var
  lastdosexitcode : word;

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
   end;
  Proc:=PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, Infinite) <> $ffffffff then
    GetExitCodeProcess(Proc,l)
  else
    l:=$ffffffff;
  CloseHandle(Proc);
  LastDosExitCode:=l;
end;


function dosexitcode : word;
begin
  dosexitcode:=lastdosexitcode;
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

function diskfree(drive : byte) : longint;
var
  disk : array[1..4] of char;
  secs,bytes,
  free,total : DWord;
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
  if GetDiskFreeSpace(@disk,secs,bytes,free,total) then
   diskfree:=free*secs*bytes
  else
   diskfree:=-1;
end;


function disksize(drive : byte) : longint;
var
  disk : array[1..4] of char;
  secs,bytes,
  free,total : DWord;
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
  if GetDiskFreeSpace(@disk,secs,bytes,free,total) then
   disksize:=total*secs*bytes
  else
   disksize:=-1;
end;


{******************************************************************************
                         --- Findfirst FindNext ---
******************************************************************************}

{ Needed kernel calls }
   function FindFirstFile (lpFileName: PChar; var lpFindFileData: TWIN32FindData): THandle;stdcall
     external 'Kernel32.dll' name 'FindFirstFileA';
   function FindNextFile  (hFindFile: THandle; var lpFindFileData: TWIN32FindData): Boolean;stdcall;
     external 'Kernel32.dll' name 'FindNextFileA';
   function FindCloseFile (hFindFile: THandle): Boolean;stdcall;
     external 'Kernel32.dll' name 'FindClose';

Procedure StringToPchar (Var S : String);
Var L : Longint;
begin
  L:=ord(S[0]);
  Move (S[1],S[0],L);
  S[L]:=#0;
end;


procedure FindMatch(var f:searchrec);
Var
  TheAttr : Longint;
begin
  TheAttr:=DosToWinAttr(F.Attr);
{ Find file with correct attribute }
  While (F.W32FindData.dwFileAttributes and TheAttr)=0 do
   begin
     if not FindNextFile (F.FindHandle,F.W32FindData) then
      begin
        DosError:=Last2DosError(GetLastError);
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
  StringToPchar(f.name);
{ FindFirstFile is a Win32 Call. }
  F.FindHandle:=FindFirstFile (pchar(@f.Name),F.W32FindData);
  If longint(F.FindHandle)=longint(Invalid_Handle_value) then
   begin
     DosError:=Last2DosError(GetLastError);
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
  If longint(F.FindHandle)<>longint(Invalid_Handle_value) then
   FindCloseFile(F.FindHandle);
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

   function GetFileTime(h : longint;creation,lastaccess,lastwrite : PFileTime) : boolean;stdcall;
     external 'Kernel32.dll' name 'GetFileTime';
   function SetFileTime(h : longint;creation,lastaccess,lastwrite : PFileTime) : boolean;stdcall;
     external 'Kernel32.dll' name 'SetFileTime';
   function SetFileAttributes(lpFileName : pchar;dwFileAttributes : longint) : boolean;stdcall;
     external 'Kernel32.dll' name 'SetFileAttributesA';
   function GetFileAttributes(lpFileName : pchar) : longint;stdcall;
     external 'Kernel32.dll' name 'GetFileAttributesA';

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


procedure getftime(var f;var time : longint);
var
   ft : TFileTime;
begin
  if GetFileTime(filerec(f).Handle,nil,nil,@ft) and
     WinToDosTime(ft,time) then
    exit
  else
    time:=0;
end;


procedure setftime(var f;time : longint);
var
  ft : TFileTime;
begin
  if DosToWinTime(time,ft) then
   SetFileTime(filerec(f).Handle,nil,nil,@ft);
end;


procedure getfattr(var f;var attr : word);
var
   l : longint;
begin
  l:=GetFileAttributes(filerec(f).name);
  if l=longint($ffffffff) then
   doserror:=getlasterror;
  attr:=l;
end;


procedure setfattr(var f;attr : word);
begin
  doserror:=0;
  if not(SetFileAttributes(filerec(f).name,attr)) then
    doserror:=getlasterror;
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

{
  The environment is a block of zero terminated strings
  terminated by a #0
}

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
{
  $Log$
  Revision 1.8  2000-05-11 09:56:20  pierre
    * fixed several compare problems between longints and
      const > $80000000 that are treated as int64 constanst
      by Delphi reported by Kovacs Attila Zoltan

  Revision 1.7  2000/02/09 13:22:52  peter
    * log truncated

  Revision 1.6  2000/01/07 01:14:23  peter
    * updated copyright to 2000

}
{

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for win32

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

uses
  windows;

{$DEFINE HAS_SLEEP}
{$DEFINE HAS_OSERROR}
{$DEFINE HAS_OSCONFIG}
{$DEFINE HAS_OSUSERDIR}
{$DEFINE HAS_CREATEGUID}

{ Include platform independent interface part }
{$i sysutilh.inc}

type
  TSystemTime = Windows.TSystemTime;

  EWin32Error = class(Exception)
  public
    ErrorCode : DWORD;
  end;


Var
  Win32Platform : Longint;
  Win32MajorVersion,
  Win32MinorVersion,
  Win32BuildNumber   : dword;
  Win32CSDVersion    : ShortString;   // CSD record is 128 bytes only?

{ Compatibility with Delphi }
function Win32Check(res:boolean):boolean;inline;
function WinCheck(res:boolean):boolean;
function CheckWin32Version(Major,Minor : Integer ): Boolean;
function CheckWin32Version(Major : Integer): Boolean;
Procedure RaiseLastWin32Error;

function GetFileVersion(const AFileName: RtlString): Cardinal;

procedure GetFormatSettings;

implementation

  uses
    sysconst;

function _W(const s: RtlString): PWideChar; inline;
begin
  Result:=PWideChar(UnicodeString(s));
end;

function WinCheck(res:boolean):boolean;
  begin
    if not res then
      RaiseLastOSError;
    result:=res;
  end;


function Win32Check(res:boolean):boolean;inline;
  begin
    result:=WinCheck(res);
  end;


procedure RaiseLastWin32Error;
  begin
    RaiseLastOSError;
  end;


function CheckWin32Version(Major : Integer): Boolean;
  begin
    Result:=CheckWin32Version(Major,0)
  end;


function CheckWin32Version(Major,Minor: Integer): Boolean;
  begin
    Result:=(Win32MajorVersion>dword(Major)) or
            ((Win32MajorVersion=dword(Major)) and (Win32MinorVersion>=dword(Minor)));
  end;


function GetFileVersion(const AFileName:RtlString):Cardinal;
  var
    { usefull only as long as we don't need to touch different stack pages }
    buf : array[0..3071] of byte;
    bufp : pointer;
    fn : RtlString;
    valsize,
    size : DWORD;
    h : DWORD;
    valrec : PVSFixedFileInfo;
  begin
    result:=$fffffff;
    fn:=AFileName;
    UniqueString(fn);
    size:=GetFileVersionInfoSizeW(_W(fn),@h);
    if size>sizeof(buf) then
      begin
        getmem(bufp,size);
        try
          if GetFileVersionInfoW(_W(fn),h,size,bufp) then
            if VerQueryValue(bufp,'\',valrec,valsize) then
              result:=valrec^.dwFileVersionMS;
        finally
          freemem(bufp);
        end;
      end
    else
      begin
        if GetFileVersionInfo(pchar(fn),h,size,@buf) then
          if VerQueryValue(@buf,'\',valrec,valsize) then
            result:=valrec^.dwFileVersionMS;
      end;
  end;


{$define HASCREATEGUID}
{$define HASEXPANDUNCFILENAME}

{$DEFINE FPC_NOGENERICANSIROUTINES}

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{ Include platform independent implementation part }
{$i sysutils.inc}
{
function SysGetTempFileName(lpPathName:LPWSTR;
                            lpPrefixString:LPWSTR;
                            uUnique:UINT;
                            lpTempFileName:LPWTR):UINT;stdcall;external 'kernel32' name 'GetTempFileNameW';
}
function GetTempFileName(Dir,Prefix: PRtlChar; uUnique: DWORD; TempFileName: PRtlChar):DWORD;
begin
  Result:=Windows.GetTempFileNameW(_W(Dir),_W(Prefix),uUnique,TempFileName);
end;


{ UUID generation. }

function CoCreateGuid(out guid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CoCreateGuid';

function SysCreateGUID(out Guid: TGUID): Integer;
begin
  Result := Integer(CoCreateGuid(Guid));
end;


function ExpandUNCFileName (const filename:RtlString) : RtlString;
{ returns empty string on errors }
var
  s    : RtlString;
  size : dword;
  rc   : dword;
  buf : PWideChar;
begin
  s := ExpandFileName (filename);

  s := s + #0;

  size := max_path;
  getmem(buf,size);

  try
    rc := WNetGetUniversalNameW(_W(s), UNIVERSAL_NAME_INFO_LEVEL, buf, @size);

    if rc=ERROR_MORE_DATA then
      begin
        buf:=reallocmem(buf,size);
        rc := WNetGetUniversalNameW(_W(s), UNIVERSAL_NAME_INFO_LEVEL, buf, @size);
      end;
    if rc = NO_ERROR then
      Result := buf
    else if rc = ERROR_NOT_CONNECTED then
      Result := filename
    else
      Result := '';
  finally
    freemem(buf);
  end;
end;


{****************************************************************************
                              File Functions
****************************************************************************}

var
  SetFilePointerEx : function(hFile : THandle;
    liDistanceToMove : int64;lpNewFilePointer : pint64;
    dwMoveMethod : DWord) : ByteBool;stdcall;

Function FileOpen (Const FileName : string; Mode : Integer) : THandle;
const
  AccessMode: array[0..2] of Cardinal  = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of Integer = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);
Var
  FN : string;
begin
  FN:=FileName+#0;
  result := CreateFile(@FN[1], dword(AccessMode[Mode and 3]),
                       dword(ShareMode[(Mode and $F0) shr 4]), nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
  //if fail api return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
end;


Function FileCreate (Const FileName : String) : THandle;
Var
  FN : string;
begin
  FN:=FileName+#0;
  Result := CreateFile(@FN[1], GENERIC_READ or GENERIC_WRITE,
                       0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileCreate (Const FileName : String; Mode:longint) : THandle;
begin
  FileCreate:=FileCreate(FileName);
end;


Function FileRead (Handle : THandle; Var Buffer; Count : longint) : Longint;
Var
  res : dword;
begin
  if ReadFile(Handle, Buffer, Count, res, nil) then
   FileRead:=Res
  else
   FileRead:=-1;
end;


Function FileWrite (Handle : THandle; const Buffer; Count : Longint) : Longint;
Var
  Res : dword;
begin
  if WriteFile(Handle, Buffer, Count, Res, nil) then
   FileWrite:=Res
  else
   FileWrite:=-1;
end;


Function FileSeek (Handle : THandle;FOffset,Origin : Longint) : Longint;
begin
  Result := longint(SetFilePointer(Handle, FOffset, nil, Origin));
end;


Function FileSeek (Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
begin
  if assigned(SetFilePointerEx) then
    begin
      if not(SetFilePointerEx(Handle, FOffset, @result, Origin)) then
        Result:=-1;
    end
  else
    Result:=longint(SetFilePointer(Handle, FOffset, nil, Origin));
end;


Procedure FileClose (Handle : THandle);
begin
  if Handle<=4 then
   exit;
  CloseHandle(Handle);
end;


Function FileTruncate (Handle : THandle;Size: Int64) : boolean;
begin
{
  Result:=longint(SetFilePointer(handle,Size,nil,FILE_BEGIN))<>-1;
}
  if FileSeek (Handle, Size, FILE_BEGIN) = Size then
   Result:=SetEndOfFile(handle)
  else
   Result := false;
end;

Function DosToWinTime (DTime:longint;Var Wtime : TFileTime):longbool;
var
  lft : TFileTime;
begin
  DosToWinTime:=DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,@lft) and
                LocalFileTimeToFileTime(lft,Wtime);
end;


Function WinToDosTime (Var Wtime : TFileTime;var DTime:longint):longbool;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft) and
                FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo);
end;


Function FileAge (Const FileName : String): Longint;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(Pchar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        If WinToDosTime(FindData.ftLastWriteTime,Result) then
          exit;
    end;
  Result := -1;
end;


Function FileExists (Const FileName : String) : Boolean;
var
  Attr:Dword;
begin
  Attr:=GetFileAttributes(PChar(FileName));
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result:=False;
end;


Function DirectoryExists (Const Directory : String) : Boolean;
var
  Attr:Dword;
begin
  Attr:=GetFileAttributes(PChar(Directory));
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result:=False;
end;


Function FindMatch(var f: TSearchRec) : Longint;
begin
  { Find file with correct attribute }
  While (F.FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if not FindNextFile (F.FindHandle,F.FindData) then
      begin
        Result:=GetLastError;
        exit;
      end;
   end;
  { Convert some attributes back }
  WinToDosTime(F.FindData.ftLastWriteTime,F.Time);
  f.size:=F.FindData.NFileSizeLow;
  f.attr:=F.FindData.dwFileAttributes;
  f.Name:=StrPas(@F.FindData.cFileName[0]);
  Result:=0;
end;


Function FindFirst (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;
begin
  Rslt.Name:=Path;
  Rslt.Attr:=attr;
  Rslt.ExcludeAttr:=(not Attr) and ($1e);
                 { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
  { FindFirstFile is a Win32 Call }
  Rslt.FindHandle:=FindFirstFile (PChar(Path),Rslt.FindData);
  If Rslt.FindHandle=Invalid_Handle_value then
   begin
     Result:=GetLastError;
     exit;
   end;
  { Find file with correct attribute }
  Result:=FindMatch(Rslt);
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
begin
  if FindNextFile(Rslt.FindHandle, Rslt.FindData) then
    Result := FindMatch(Rslt)
  else
    Result := GetLastError;
end;


Procedure FindClose (Var F : TSearchrec);
begin
   if F.FindHandle <> INVALID_HANDLE_VALUE then
    Windows.FindClose(F.FindHandle);
end;


Function FileGetDate (Handle : THandle) : Longint;
Var
  FT : TFileTime;
begin
  If GetFileTime(Handle,nil,nil,@ft) and
     WinToDosTime(FT,Result) then
    exit;
  Result:=-1;
end;


Function FileSetDate (Handle : THandle;Age : Longint) : Longint;
Var
  FT: TFileTime;
begin
  Result := 0;
  if DosToWinTime(Age,FT) and
    SetFileTime(Handle, nil, nil, @FT) then
    Exit;
  Result := GetLastError;
end;


Function FileGetAttr (Const FileName : String) : Longint;
begin
  Result:=GetFileAttributes(PChar(FileName));
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
begin
  if SetFileAttributes(PChar(FileName), Attr) then
    Result:=0
  else
    Result := GetLastError;
end;


Function DeleteFile (Const FileName : String) : Boolean;
begin
  Result:=Windows.DeleteFile(Pchar(FileName));
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;
begin
  Result := MoveFile(PChar(OldName), PChar(NewName));
end;


{****************************************************************************
                              Disk Functions
****************************************************************************}

function GetDiskFreeSpace(drive:pchar;var sector_cluster,bytes_sector,
                          freeclusters,totalclusters:longint):longbool;
         stdcall;external 'kernel32' name 'GetDiskFreeSpaceA';
type
   TGetDiskFreeSpaceEx = function(drive:pchar;var availableforcaller,total,free):longbool;stdcall;

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
       if GetDiskFreeSpaceEx(@disk[1],qwcaller,qwtotal,qwfree) then
         diskfree:=qwfree
       else
         diskfree:=-1;
    end
  else
    begin
       if GetDiskFreeSpace(@disk[1],secs,bytes,free,total) then
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
       if GetDiskFreeSpaceEx(@disk[1],qwcaller,qwtotal,qwfree) then
         disksize:=qwtotal
       else
         disksize:=-1;
    end
  else
    begin
       if GetDiskFreeSpace(@disk[1],secs,bytes,free,total) then
         disksize:=int64(total)*secs*bytes
       else
         disksize:=-1;
    end;
end;


Function GetCurrentDir : RtlString;
begin
  GetDir(0, result);
end;


Function SetCurrentDir (Const NewDir : RtlString) : Boolean;
begin
  Result:=SetCurrentDirectoryW(_W(NewDir));
end;


Function CreateDir (Const NewDir : RtlString) : Boolean;
begin
  Result:=CreateDirectoryW(_W(NewDir),nil);
end;


Function RemoveDir (Const Dir : RtlString) : Boolean;
begin
  Result:=RemoveDirectoryW(_W(Dir));
end;


{****************************************************************************
                              Time Functions
****************************************************************************}


Procedure GetLocalTime(var SystemTime: TSystemTime);
Var
  Syst : Windows.TSystemtime;
begin
  windows.Getlocaltime(@syst);
  SystemTime.year:=syst.wYear;
  SystemTime.month:=syst.wMonth;
  SystemTime.day:=syst.wDay;
  SystemTime.hour:=syst.wHour;
  SystemTime.minute:=syst.wMinute;
  SystemTime.second:=syst.wSecond;
  SystemTime.millisecond:=syst.wMilliSeconds;
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
  MessageBeep(0);
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

function GetLocaleStr(LID, LT: Longint; const Def: string): ShortString;
var
  L: Integer;
  Buf: array[0..255] of Char;
begin
  L := GetLocaleInfo(LID, LT, Buf, SizeOf(Buf));
  if L > 0 then
    SetString(Result, @Buf[0], L - 1)
  else
    Result := Def;
end;


function GetLocaleChar(LID, LT: Longint; Def: Char): Char;
var
  Buf: array[0..1] of Char;
begin
  if GetLocaleInfo(LID, LT, Buf, 2) > 0 then
    Result := Buf[0]
  else
    Result := Def;
end;


Function GetLocaleInt(LID,TP,Def: LongInt): LongInt;
Var
  S: String;
  C: Integer;
Begin
  S:=GetLocaleStr(LID,TP,'0');
  Val(S,Result,C);
  If C<>0 Then
    Result:=Def;
End;


procedure GetFormatSettings;
var
  HF  : Shortstring;
  LID : LCID;
  I,Day : longint;
begin
  LID := GetThreadLocale;
  { Date stuff }
  for I := 1 to 12 do
    begin
    ShortMonthNames[I]:=GetLocaleStr(LID,LOCALE_SABBREVMONTHNAME1+I-1,ShortMonthNames[i]);
    LongMonthNames[I]:=GetLocaleStr(LID,LOCALE_SMONTHNAME1+I-1,LongMonthNames[i]);
    end;
  for I := 1 to 7 do
    begin
    Day := (I + 5) mod 7;
    ShortDayNames[I]:=GetLocaleStr(LID,LOCALE_SABBREVDAYNAME1+Day,ShortDayNames[i]);
    LongDayNames[I]:=GetLocaleStr(LID,LOCALE_SDAYNAME1+Day,LongDayNames[i]);
    end;
  DateSeparator := GetLocaleChar(LID, LOCALE_SDATE, '/');
  ShortDateFormat := GetLocaleStr(LID, LOCALE_SSHORTDATE, 'm/d/yy');
  LongDateFormat := GetLocaleStr(LID, LOCALE_SLONGDATE, 'mmmm d, yyyy');
  { Time stuff }
  TimeSeparator := GetLocaleChar(LID, LOCALE_STIME, ':');
  TimeAMString := GetLocaleStr(LID, LOCALE_S1159, 'AM');
  TimePMString := GetLocaleStr(LID, LOCALE_S2359, 'PM');
  if StrToIntDef(GetLocaleStr(LID, LOCALE_ITLZERO, '0'), 0) = 0 then
    HF:='h'
  else
    HF:='hh';
  // No support for 12 hour stuff at the moment...
  ShortTimeFormat := HF+':nn';
  LongTimeFormat := HF + ':nn:ss';
  { Currency stuff }
  CurrencyString:=GetLocaleStr(LID, LOCALE_SCURRENCY, '');
  CurrencyFormat:=StrToIntDef(GetLocaleStr(LID, LOCALE_ICURRENCY, '0'), 0);
  NegCurrFormat:=StrToIntDef(GetLocaleStr(LID, LOCALE_INEGCURR, '0'), 0);
  { Number stuff }
  ThousandSeparator:=GetLocaleChar(LID, LOCALE_STHOUSAND, ',');
  DecimalSeparator:=GetLocaleChar(LID, LOCALE_SDECIMAL, '.');
  CurrencyDecimals:=StrToIntDef(GetLocaleStr(LID, LOCALE_ICURRDIGITS, '0'), 0);
end;


Procedure InitInternational;
var
  { A call to GetSystemMetrics changes the value of the 8087 Control Word on
    Pentium4 with WinXP SP2 }
  old8087CW: word;
begin
  InitInternationalGeneric;
  old8087CW:=Get8087CW;
  SysLocale.MBCS:=GetSystemMetrics(SM_DBCSENABLED)<>0;
  SysLocale.RightToLeft:=GetSystemMetrics(SM_MIDEASTENABLED)<>0;
  SysLocale.DefaultLCID := $0409;
  SysLocale.PriLangID := LANG_ENGLISH;
  SysLocale.SubLangID := SUBLANG_ENGLISH_US;
  // probably needs update with getthreadlocale. post 2.0.2

  Set8087CW(old8087CW);
  GetFormatSettings;
end;


{****************************************************************************
                           Target Dependent
****************************************************************************}

function FormatMessageA(dwFlags     : DWORD;
                        lpSource    : Pointer;
                        dwMessageId : DWORD;
                        dwLanguageId: DWORD;
                        lpBuffer    : PCHAR;
                        nSize       : DWORD;
                        Arguments   : Pointer): DWORD; stdcall;external 'kernel32' name 'FormatMessageA';

function SysErrorMessage(ErrorCode: Integer): String;
const
  MaxMsgSize = Format_Message_Max_Width_Mask;
var
  MsgBuffer: pChar;
begin
  GetMem(MsgBuffer, MaxMsgSize);
  FillChar(MsgBuffer^, MaxMsgSize, #0);
  FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM,
                 nil,
                 ErrorCode,
                 MakeLangId(LANG_NEUTRAL, SUBLANG_DEFAULT),
                 MsgBuffer,                 { This function allocs the memory }
                 MaxMsgSize,                           { Maximum message size }
                 nil);
  SysErrorMessage := StrPas(MsgBuffer);
  FreeMem(MsgBuffer, MaxMsgSize);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

var
   s : string;
   i : longint;
   hp,p : pchar;
begin
   Result:='';
   p:=GetEnvironmentStrings;
   hp:=p;
   while hp^<>#0 do
     begin
        s:=strpas(hp);
        i:=pos('=',s);
        if uppercase(copy(s,1,i-1))=upcase(envvar) then
          begin
             Result:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
   FreeEnvironmentStrings(p);
end;

Function GetEnvironmentVariableCount : Integer;

var
  hp,p : pchar;
begin
  Result:=0;
  p:=GetEnvironmentStrings;
  hp:=p;
  If (Hp<>Nil) then
    while hp^<>#0 do
      begin
      Inc(Result);
      hp:=hp+strlen(hp)+1;
      end;
  FreeEnvironmentStrings(p);
end;

Function GetEnvironmentString(Index : Integer) : String;

var
  hp,p : pchar;
begin
  Result:='';
  p:=GetEnvironmentStrings;
  hp:=p;
  If (Hp<>Nil) then
    begin
    while (hp^<>#0) and (Index>1) do
      begin
      Dec(Index);
      hp:=hp+strlen(hp)+1;
      end;
    If (hp^<>#0) then
      Result:=StrPas(HP);
    end;
  FreeEnvironmentStrings(p);
end;


function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWord;
  CommandLine : ansistring;
  e : EOSError;

begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"',path)=0 then
    CommandLine:='"'+path+'"'
  else
    CommandLine:=path;
  if ComLine <> '' then
    CommandLine:=Commandline+' '+ComLine+#0
  else
    CommandLine := CommandLine + #0;

  if not CreateProcess(nil, pchar(CommandLine),
    Nil, Nil, False,$20, Nil, Nil, SI, PI) then
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
      e.ErrorCode:=GetLastError;
      raise e;
    end;
  Proc:=PI.hProcess;
  if WaitForSingleObject(Proc, dword($ffffffff)) <> $ffffffff then
    begin
      GetExitCodeProcess(Proc,l);
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      result:=l;
    end
  else
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
      e.ErrorCode:=GetLastError;
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      raise e;
    end;
end;

function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array of AnsiString):integer;

var
  CommandLine: AnsiString;
  I: integer;

begin
  Commandline := '';
  for I := 0 to High (ComLine) do
   if Pos (' ', ComLine [I]) <> 0 then
    CommandLine := CommandLine + ' ' + '"' + ComLine [I] + '"'
   else
    CommandLine := CommandLine + ' ' + Comline [I];
  ExecuteProcess := ExecuteProcess (Path, CommandLine);
end;

Procedure Sleep(Milliseconds : Cardinal);

begin
  Windows.Sleep(MilliSeconds)
end;

Function GetLastOSError : Integer;

begin
  Result:=GetLastError;
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

var
   kernel32dll : THandle;

Procedure LoadVersionInfo;
// and getfreespaceex
Var
   versioninfo : TOSVERSIONINFO;
begin
  kernel32dll:=0;
  GetDiskFreeSpaceEx:=nil;
  versioninfo.dwOSVersionInfoSize:=sizeof(versioninfo);
  GetVersionEx(versioninfo);
  Win32Platform:=versionInfo.dwPlatformId;
  Win32MajorVersion:=versionInfo.dwMajorVersion;
  Win32MinorVersion:=versionInfo.dwMinorVersion;
  Win32BuildNumber:=versionInfo.dwBuildNumber;
  Move (versioninfo.szCSDVersion ,Win32CSDVersion[1],128);
  win32CSDVersion[0]:=chr(strlen(pchar(@versioninfo.szCSDVersion)));
  if ((versioninfo.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS) and
    (versioninfo.dwBuildNUmber>=1000)) or
    (versioninfo.dwPlatformId=VER_PLATFORM_WIN32_NT) then
    begin
       kernel32dll:=LoadLibrary('kernel32');
       if kernel32dll<>0 then
         GetDiskFreeSpaceEx:=TGetDiskFreeSpaceEx(GetProcAddress(kernel32dll,'GetDiskFreeSpaceExA'));
    end;
end;


function FreeLibrary(hLibModule : THANDLE) : longbool;
  stdcall;external 'kernel32' name 'FreeLibrary';
function GetVersionEx(var VersionInformation:TOSVERSIONINFO) : longbool;
  stdcall;external 'kernel32' name 'GetVersionExA';
function LoadLibrary(lpLibFileName : pchar):THandle;
  stdcall;external 'kernel32' name 'LoadLibraryA';
function GetProcAddress(hModule : THandle;lpProcName : pchar) : pointer;
  stdcall;external 'kernel32' name 'GetProcAddress';

Const
  CSIDL_PROGRAMS                = $0002; { %SYSTEMDRIVE%\Program Files                                      }
  CSIDL_PERSONAL                = $0005; { %USERPROFILE%\My Documents                                       }
  CSIDL_FAVORITES               = $0006; { %USERPROFILE%\Favorites                                          }
  CSIDL_STARTUP                 = $0007; { %USERPROFILE%\Start menu\Programs\Startup                        }
  CSIDL_RECENT                  = $0008; { %USERPROFILE%\Recent                                             }
  CSIDL_SENDTO                  = $0009; { %USERPROFILE%\Sendto                                             }
  CSIDL_STARTMENU               = $000B; { %USERPROFILE%\Start menu                                         }
  CSIDL_MYMUSIC                 = $000D; { %USERPROFILE%\Documents\My Music                                 }
  CSIDL_MYVIDEO                 = $000E; { %USERPROFILE%\Documents\My Videos                                }
  CSIDL_DESKTOPDIRECTORY        = $0010; { %USERPROFILE%\Desktop                                            }
  CSIDL_NETHOOD                 = $0013; { %USERPROFILE%\NetHood                                            }
  CSIDL_TEMPLATES               = $0015; { %USERPROFILE%\Templates                                          }
  CSIDL_COMMON_STARTMENU        = $0016; { %PROFILEPATH%\All users\Start menu                               }
  CSIDL_COMMON_PROGRAMS         = $0017; { %PROFILEPATH%\All users\Start menu\Programs                      }
  CSIDL_COMMON_STARTUP          = $0018; { %PROFILEPATH%\All users\Start menu\Programs\Startup              }
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019; { %PROFILEPATH%\All users\Desktop                                  }
  CSIDL_APPDATA                 = $001A; { %USERPROFILE%\Application Data (roaming)                         }
  CSIDL_PRINTHOOD               = $001B; { %USERPROFILE%\Printhood                                          }
  CSIDL_LOCAL_APPDATA           = $001C; { %USERPROFILE%\Local Settings\Application Data (non roaming)      }
  CSIDL_COMMON_FAVORITES        = $001F; { %PROFILEPATH%\All users\Favorites                                }
  CSIDL_INTERNET_CACHE          = $0020; { %USERPROFILE%\Local Settings\Temporary Internet Files            }
  CSIDL_COOKIES                 = $0021; { %USERPROFILE%\Cookies                                            }
  CSIDL_HISTORY                 = $0022; { %USERPROFILE%\Local settings\History                             }
  CSIDL_COMMON_APPDATA          = $0023; { %PROFILESPATH%\All Users\Application Data                        }
  CSIDL_WINDOWS                 = $0024; { %SYSTEMROOT%                                                     }
  CSIDL_SYSTEM                  = $0025; { %SYSTEMROOT%\SYSTEM32 (may be system on 95/98/ME)                }
  CSIDL_PROGRAM_FILES           = $0026; { %SYSTEMDRIVE%\Program Files                                      }
  CSIDL_MYPICTURES              = $0027; { %USERPROFILE%\My Documents\My Pictures                           }
  CSIDL_PROFILE                 = $0028; { %USERPROFILE%                                                    }
  CSIDL_PROGRAM_FILES_COMMON    = $002B; { %SYSTEMDRIVE%\Program Files\Common                               }
  CSIDL_COMMON_TEMPLATES        = $002D; { %PROFILEPATH%\All Users\Templates                                }
  CSIDL_COMMON_DOCUMENTS        = $002E; { %PROFILEPATH%\All Users\Documents                                }
  CSIDL_COMMON_ADMINTOOLS       = $002F; { %PROFILEPATH%\All Users\Start Menu\Programs\Administrative Tools }
  CSIDL_ADMINTOOLS              = $0030; { %USERPROFILE%\Start Menu\Programs\Administrative Tools           }
  CSIDL_COMMON_MUSIC            = $0035; { %PROFILEPATH%\All Users\Documents\my music                       }
  CSIDL_COMMON_PICTURES         = $0036; { %PROFILEPATH%\All Users\Documents\my pictures                    }
  CSIDL_COMMON_VIDEO            = $0037; { %PROFILEPATH%\All Users\Documents\my videos                      }
  CSIDL_CDBURN_AREA             = $003B; { %USERPROFILE%\Local Settings\Application Data\Microsoft\CD Burning }
  CSIDL_PROFILES                = $003E; { %PROFILEPATH%                                                    }

  CSIDL_FLAG_CREATE             = $8000; { (force creation of requested folder if it doesn't exist yet)     }


Type
  PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;


var
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;

Procedure InitDLL;

Var
  P : Pointer;

begin
  CFGDLLHandle:=LoadLibrary('shell32.dll');
  if (CFGDLLHandle<>0) then
    begin
    P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
    If (P=Nil) then
      begin
      FreeLibrary(CFGDLLHandle);
      CFGDllHandle:=0;
      end
    else
      SHGetFolderPath:=PFNSHGetFolderPath(P);
    end;
  If (P=Nil) then
    begin
    CFGDLLHandle:=LoadLibrary('shfolder.dll');
    if (CFGDLLHandle<>0) then
      begin
      P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
      If (P=Nil) then
        begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
        end
      else
        ShGetFolderPath:=PFNSHGetFolderPath(P);
      end;
    end;
  If (@ShGetFolderPath=Nil) then
    Raise Exception.Create('Could not determine SHGetFolderPath Function');
end;

Function GetSpecialDir(ID :  Integer) : String;

Var
  APath : Array[0..MAX_PATH] of char;

begin
  Result:='';
  if (CFGDLLHandle=0) then
    InitDLL;
  If (SHGetFolderPath<>Nil) then
    begin
    if SHGetFolderPath(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0])=S_OK then
      Result:=IncludeTrailingPathDelimiter(StrPas(@APath[0]));
    end;
end;

Function GetAppConfigDir(Global : Boolean) : String;
begin
  If Global then
    Result:=GetSpecialDir(CSIDL_COMMON_APPDATA)
  else
    Result:=GetSpecialDir(CSIDL_LOCAL_APPDATA);
  If (Result<>'') then
    begin
      if VendorName<>'' then
        Result:=IncludeTrailingPathDelimiter(Result+VendorName);
      Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
    end
  else
    Result:=IncludeTrailingPathDelimiter(DGetAppConfigDir(Global));
end;

Function GetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;

begin
  result:=DGetAppConfigFile(Global,SubDir);
end;

Function GetUserDir : String;

begin
  Result:=GetSpecialDir(CSIDL_PROFILE);
end;

Procedure InitSysConfigDir;

begin
  SetLength(SysConfigDir, MAX_PATH);
  SetLength(SysConfigDir, GetWindowsDirectory(PChar(SysConfigDir), MAX_PATH));
end;

{****************************************************************************
                    Target Dependent WideString stuff
****************************************************************************}

function Win32CompareWideString(const s1, s2 : WideString) : PtrInt;
  begin
    SetLastError(0);
    Result:=CompareStringW(LOCALE_USER_DEFAULT,0,pwidechar(s1),
      length(s1),pwidechar(s2),length(s2))-2;
    if GetLastError<>0 then
      RaiseLastOSError;
  end;


function Win32CompareTextWideString(const s1, s2 : WideString) : PtrInt;
  begin
    SetLastError(0);
    Result:=CompareStringW(LOCALE_USER_DEFAULT,NORM_IGNORECASE,pwidechar(s1),
      length(s1),pwidechar(s2),length(s2))-2;
    if GetLastError<>0 then
      RaiseLastOSError;
  end;


function Win32AnsiUpperCase(const s: string): string;
  begin
    if length(s)>0 then
      begin
        result:=s;
        UniqueString(result);
        CharUpperBuff(pchar(result),length(result));
      end
    else
      result:='';
  end;


function Win32AnsiLowerCase(const s: string): string;
  begin
    if length(s)>0 then
      begin
        result:=s;
        UniqueString(result);
        CharLowerBuff(pchar(result),length(result));
      end
    else
      result:='';
  end;


function Win32AnsiCompareStr(const S1, S2: string): PtrInt;
  begin
    result:=CompareString(LOCALE_USER_DEFAULT,0,pchar(s1),length(s1),
      pchar(s2),length(s2))-2;
  end;


function Win32AnsiCompareText(const S1, S2: string): PtrInt;
  begin
    result:=CompareString(LOCALE_USER_DEFAULT,NORM_IGNORECASE,pchar(s1),length(s1),
      pchar(s2),length(s2))-2;
  end;


function Win32AnsiStrComp(S1, S2: PChar): PtrInt;
  begin
    result:=CompareString(LOCALE_USER_DEFAULT,0,s1,-1,s2,-1)-2;
  end;


function Win32AnsiStrIComp(S1, S2: PChar): PtrInt;
  begin
    result:=CompareString(LOCALE_USER_DEFAULT,NORM_IGNORECASE,s1,-1,s2,-1)-2;
  end;


function Win32AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  begin
    result:=CompareString(LOCALE_USER_DEFAULT,0,s1,maxlen,s2,maxlen)-2;
  end;


function Win32AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  begin
    result:=CompareString(LOCALE_USER_DEFAULT,NORM_IGNORECASE,s1,maxlen,s2,maxlen)-2;
  end;


function Win32AnsiStrLower(Str: PChar): PChar;
  begin
    CharLower(str);
    result:=str;
  end;


function Win32AnsiStrUpper(Str: PChar): PChar;
  begin
    CharUpper(str);
    result:=str;
  end;


{ there is a similiar procedure in the system unit which inits the fields which
  are relevant already for the system unit }
procedure InitWin32Widestrings;
  begin
//!!!    CharLengthPCharProc : function(const Str: PChar): PtrInt;
    widestringmanager.CompareWideStringProc:=@Win32CompareWideString;
    widestringmanager.CompareTextWideStringProc:=@Win32CompareTextWideString;
    widestringmanager.UpperAnsiStringProc:=@Win32AnsiUpperCase;
    widestringmanager.LowerAnsiStringProc:=@Win32AnsiLowerCase;
    widestringmanager.CompareStrAnsiStringProc:=@Win32AnsiCompareStr;
    widestringmanager.CompareTextAnsiStringProc:=@Win32AnsiCompareText;
    widestringmanager.StrCompAnsiStringProc:=@Win32AnsiStrComp;
    widestringmanager.StrICompAnsiStringProc:=@Win32AnsiStrIComp;
    widestringmanager.StrLCompAnsiStringProc:=@Win32AnsiStrLComp;
    widestringmanager.StrLICompAnsiStringProc:=@Win32AnsiStrLIComp;
    widestringmanager.StrLowerAnsiStringProc:=@Win32AnsiStrLower;
    widestringmanager.StrUpperAnsiStringProc:=@Win32AnsiStrUpper;
  end;


procedure SetupProcVars;
  var
    hinstLib : THandle;
  begin
    SetFilePointerEx:=nil;
    hinstLib:=LoadLibrary(KernelDLL);
    if hinstLib<>0 then
      begin
        pointer(SetFilePointerEx):=GetProcAddress(hinstLib,'SetFilePointerEx');
        FreeLibrary(hinstLib);
      end;
  end;


Initialization
  InitWin32Widestrings;
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
  LoadVersionInfo;
  InitSysConfigDir;
  SetupProcVars;
Finalization
  DoneExceptions;
  if kernel32dll<>0 then
   FreeLibrary(kernel32dll);
 if CFGDLLHandle<>0 then
   FreeLibrary(CFGDllHandle);
end.

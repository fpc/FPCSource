{
    $Id$

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

{$IFNDEF VIRTUALPASCAL}
{$MODE objfpc}
{$ENDIF}
{ force ansistrings }
{$H+}

uses
  {$IFDEF VIRTUALPASCAL}
  vpglue,
  strings,
  crt,
  {$ENDIF}
  dos,
  windows;


{ Include platform independent interface part }
{$i sysutilh.inc}

type
  THandle = Windows.Handle;

  TSystemTime = Windows.TSystemTime;

  EWin32Error = class(Exception)
  public
    ErrorCode : DWORD;
  end;


Var
  Win32Platform : Longint;

implementation

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
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
end;


Function FileCreate (Const FileName : String) : Longint;
Var
  FN : string;
begin
  FN:=FileName+#0;
  Result := CreateFile(@FN[1], GENERIC_READ or GENERIC_WRITE,
                       0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileCreate (Const FileName : String; Mode:longint) : Longint;
begin
  FileCreate:=FileCreate(FileName);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;
Var
  res : dword;
begin
  if ReadFile(Handle, Buffer, Count, res, nil) then
   FileRead:=Res
  else
   FileRead:=-1;
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;
Var
  Res : dword;
begin
  if WriteFile(Handle, Buffer, Count, Res, nil) then
   FileWrite:=Res
  else
   FileWrite:=-1;
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;
begin
  Result := longint(SetFilePointer(Handle, FOffset, nil, Origin));
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;
begin
  {$warning need to add 64bit call }
  Result := longint(SetFilePointer(Handle, FOffset, nil, Origin));
end;


Procedure FileClose (Handle : Longint);
begin
  if Handle<=4 then
   exit;
  CloseHandle(Handle);
end;


Function FileTruncate (Handle,Size: Longint) : boolean;
begin
  Result:=longint(SetFilePointer(handle,Size,nil,FILE_BEGIN))<>-1;
  If Result then
    Result:=SetEndOfFile(handle);
end;

Function DosToWinTime (DTime:longint;Var Wtime : TFileTime):longbool;
var
  lft : TFileTime;
begin
  {$IFDEF VIRTUALPASCAL}
  DosToWinTime:=DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,lft) and
                LocalFileTimeToFileTime(lft,Wtime);
 {$ELSE}
  DosToWinTime:=DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,@lft) and
                LocalFileTimeToFileTime(lft,Wtime);
 {$ENDIF}
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
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(Pchar(FileName), FindData);
  Result:=Handle <> INVALID_HANDLE_VALUE;
  If Result then
    Windows.FindClose(Handle);
end;


Function DirectoryExists (Const Directory : String) : Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(Pchar(Directory), FindData);
  Result:=(Handle <> INVALID_HANDLE_VALUE) and
          ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY);
  If Result then
    Windows.FindClose(Handle);
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
  f.Name:=StrPas(@F.FindData.cFileName);
  Result:=0;
end;


Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;
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


Function FileGetDate (Handle : Longint) : Longint;
Var
  FT : TFileTime;
begin
  If GetFileTime(Handle,nil,nil,@ft) and
     WinToDosTime(FT,Result) then
    exit;
  Result:=-1;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;
Var
  FT: TFileTime;
begin
  {$IFDEF VIRTUALPASCAL}
    Result := 0;
  {$ELSE}
    Result := 0;
    if DosToWinTime(Age,FT) and
      SetFileTime(Handle, ft, ft, FT) then
      Exit;
  Result := GetLastError;
  {$ENDIF}
end;


Function FileGetAttr (Const FileName : String) : Longint;
begin
  Result:=GetFileAttributes(PChar(FileName));
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
begin
  if not SetFileAttributes(PChar(FileName), Attr) then
    Result := GetLastError
  else
    Result:=0;
end;


Function DeleteFile (Const FileName : String) : Boolean;
begin
  DeleteFile:=Windows.DeleteFile(Pchar(FileName));
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
  {$IFDEF VIRTUALPASCAL}
   {&StdCall+}
   TGetDiskFreeSpaceEx = function(drive:pchar;var availableforcaller,total,free):longbool;
   {&StdCall-}
  {$ELSE}
   TGetDiskFreeSpaceEx = function(drive:pchar;var availableforcaller,total,free):longbool;stdcall;
  {$ENDIF}

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


Function GetCurrentDir : String;
begin
  GetDir(0, result);
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
  Move (CPISO88591LCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));
end;


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
  I,Day,DateOrder : longint;
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
  DateOrder := GetLocaleInt(LID, LOCALE_IDate, 0);
  Case DateOrder Of
     1: Begin
        ShortDateFormat := 'dd/mm/yyyy';
        LongDateFormat := 'dddd, d. mmmm yyyy';
        End;
     2: Begin
        ShortDateFormat := 'yyyy/mm/dd';
        LongDateFormat := 'dddd, yyyy mmmm d.';
        End;
  else
    // Default american settings...
    ShortDateFormat := 'mm/dd/yyyy';
    LongDateFormat := 'dddd, mmmm d. yyyy';
  End;
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
begin
  InitAnsi;
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


{****************************************************************************
                              Initialization code
****************************************************************************}

var
   versioninfo : TOSVERSIONINFO;
   kernel32dll : THandle;

function FreeLibrary(hLibModule : THANDLE) : longbool;
  stdcall;external 'kernel32' name 'FreeLibrary';
function GetVersionEx(var VersionInformation:TOSVERSIONINFO) : longbool;
  stdcall;external 'kernel32' name 'GetVersionExA';
function LoadLibrary(lpLibFileName : pchar):THandle;
  stdcall;external 'kernel32' name 'LoadLibraryA';
function GetProcAddress(hModule : THandle;lpProcName : pchar) : pointer;
  stdcall;external 'kernel32' name 'GetProcAddress';


Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
  versioninfo.dwOSVersionInfoSize:=sizeof(versioninfo);
  GetVersionEx(versioninfo);
  kernel32dll:=0;
  GetDiskFreeSpaceEx:=nil;
  Win32Platform:=versionInfo.dwPlatformId;
  if ((versioninfo.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS) and
    (versioninfo.dwBuildNUmber>=1000)) or
    (versioninfo.dwPlatformId=VER_PLATFORM_WIN32_NT) then
    begin
       kernel32dll:=LoadLibrary('kernel32');
       if kernel32dll<>0 then
	{$IFDEF VIRTUALPASCAL}
         @GetDiskFreeSpaceEx:=GetProcAddress(0,'GetDiskFreeSpaceExA');
        {$ELSE}
         GetDiskFreeSpaceEx:=TGetDiskFreeSpaceEx(GetProcAddress(kernel32dll,'GetDiskFreeSpaceExA'));
        {$ENDIF}
    end;

Finalization
  DoneExceptions;
  if kernel32dll<>0 then
   FreeLibrary(kernel32dll);
end.
{
  $Log$
  Revision 1.24  2003-09-17 15:06:36  peter
    * stdcall patch

  Revision 1.23  2003/09/06 22:23:35  marco
   * VP fixes.

  Revision 1.22  2003/04/01 15:57:41  peter
    * made THandle platform dependent and unique type

  Revision 1.21  2003/03/29 18:21:42  hajny
    * DirectoryExists declaration changed to that one from fixes branch

  Revision 1.20  2003/03/28 19:06:59  peter
    * directoryexists added

  Revision 1.19  2003/01/03 20:41:04  peter
    * FileCreate(string,mode) overload added

  Revision 1.18  2003/01/01 20:56:57  florian
    + added invalid instruction exception

  Revision 1.17  2002/12/15 20:24:17  peter
    * some more C style functions

  Revision 1.16  2002/10/02 21:17:03  florian
    * we've to reimport TSystemTime time from the windows unit

  Revision 1.15  2002/09/07 16:01:29  peter
    * old logs removed and tabs fixed

  Revision 1.14  2002/05/09 08:28:23  carl
  * Merges from Fixes branch

  Revision 1.13  2002/03/24 19:26:49  marco
   * Added win32platform

  Revision 1.12  2002/01/25 16:23:04  peter
    * merged filesearch() fix

}

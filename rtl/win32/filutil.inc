{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    File utility calls

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

const
  AccessMode: array[0..2] of Integer = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of Integer = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);

Var FN : string;

begin
  FN:=FileName+#0;
  Result := CreateFile(@FN[1], AccessMode[Mode and 3],
                         ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileCreate (Const FileName : String) : Longint;

Var FN : string;

begin
  FN:=FileName+#0;
 Result := CreateFile(@FN[1], GENERIC_READ or GENERIC_WRITE,
     0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

Var res : Longint;

begin
  if not ReadFile(Handle, Buffer, Count, res, nil) then res := -1;
  FileRead:=Res;
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

Var Res : longint;

begin
  if not WriteFile(Handle, Buffer, Count, Res, nil) then Res:= -1;
  FileWrite:=Res;
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  Result := SetFilePointer(Handle, FOffset, nil, Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  if Handle<=4 then
   exit;
  CloseHandle(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  Result:=SetFilePointer(handle,Size,nil,FILE_BEGIN)<>-1;
  If Result then
    Result:=SetEndOfFile(handle);
end;

Function DosToWinTime (DTime:longint;Var Wtime : TFileTime):longbool;
var
  lft : TFileTime;
begin
  DosToWinTime:=DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,@lft) and
                LocalFileTimeToFileTime(lft,@Wtime);
end;


Function WinToDosTime (Var Wtime : TFileTime;var DTime:longint):longbool;
var
  lft : FileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,@lft) and
                FileTimeToDosDateTime(lft,@Longrec(Dtime).Hi,@LongRec(DTIME).lo);
end;


Function FileAge (Const FileName : String): Longint;

var
  Handle: THandle;
  FindData: TWin32FindData;

begin
  Handle := FindFirstFile(Pchar(FileName), @FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      If WinToDosTime(FindData.ftLastWriteTime,Result) then exit;
    end;
  Result := -1;
end;


Function FileExists (Const FileName : String) : Boolean;

var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(Pchar(FileName), @FindData);
  Result:=Handle <> INVALID_HANDLE_VALUE;
  If Result then
    Windows.FindClose(Handle);
end;

Function FindMatch(var f: TSearchRec) : Longint;

begin
  { Find file with correct attribute }
  While (F.FindData.dwFileAttributes and F.ExcludeAttr)<>0 do
   begin
     if not FindNextFile (F.FindHandle,@F.FindData) then
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
  Rslt.FindHandle:=FindFirstFile (PChar(Path),@Rslt.FindData);
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
  if FindNextFile(Rslt.FindHandle, @Rslt.FindData) then
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

Var FT : TFileTime;

begin
  If GetFileTime(Handle,nil,nil,@ft) and
     WinToDosTime(FT,Result) then exit;
  Result:=-1;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

Var FT: TFileTime;

begin
  Result := 0;
  if DosToWinTime(Age,FT) and
     SetFileTime(Handle, ft, ft, FT) then Exit;
  Result := GetLastError;
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


Function FileSearch (Const Name, DirList : String) : String;

Var I : longint;
    Temp : String;

begin
  Result:='';
  temp:=Dirlist;
  repeat
    I:=pos(';',Temp);
    If I<>0 then
      begin
      Result:=Copy (Temp,1,i-1);
      system.Delete(Temp,1,I);
      end
    else
      begin
      Result:=Temp;
      Temp:='';
      end;
    If result[length(result)]<>'\' then
      Result:=Result+'\';
    Result:=Result+name;
    If not FileExists(Result) Then
     Result:='';
  until (length(temp)=0) or (length(result)<>0);
end;

Procedure GetLocalTime(var ST: TSystemTime);

Var Syst:Systemtime;

begin
  windows.Getlocaltime(@syst);
  st.year:=syst.wYear;
  st.month:=syst.wMonth;
  st.day:=syst.wDay;
  st.hour:=syst.wHour;
  st.minute:=syst.wMinute;
  st.second:=syst.wSecond;
  st.millisecond:=syst.wMilliSeconds;
end;
Procedure InitAnsi;

Var i : longint;

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
  ShortTimeFormat := HF+':mm';
  LongTimeFormat := HF + ':mm:ss';
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

{
 called by sysutils initialization routines to set up
 internationalization support.
}

begin
 InitAnsi;
 GetFormatSettings;
end;

{
  $Log$
  Revision 1.17  2000-06-04 14:16:54  hajny
    * parameter name change in FileSeek

  Revision 1.16  2000/05/15 19:28:41  peter
    * int64 support for diskfree,disksize

  Revision 1.15  2000/02/17 22:16:05  sg
  * Changed the second argument of FileWrite from "var buffer" to
    "const buffer", like in Delphi.

  Revision 1.14  2000/02/09 16:59:34  peter
    * truncated log

  Revision 1.13  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.12  2000/01/07 16:41:52  daniel
    * copyright 2000

  Revision 1.11  1999/08/18 08:38:42  michael
  * Fixed bug 533, in findmatch

}

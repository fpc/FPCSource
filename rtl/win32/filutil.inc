{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

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
  if not ReadFile(Handle, @Buffer, Count, @res, nil) then res := -1;
  FileRead:=Res;
end;


Function FileWrite (Handle : Longint; Var Buffer; Count : Longint) : Longint;

Var Res : longint;

begin
  if not WriteFile(Handle, @Buffer, Count, @Res, nil) then Res:= -1;
  FileWrite:=Res;
end;


Function FileSeek (Handle,Offset,Origin : Longint) : Longint;

begin
  Result := SetFilePointer(Handle, Offset, nil, Origin);
end;


Procedure FileClose (Handle : Longint);

begin
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
  LocalFileTime: TFileTime;
  Fn : String;
  
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
  P : Pchar;
begin
  P:=Pchar(Filename);
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


Procedure InitInternational;

{
 called by sysutils initialization routines to set up 
 internationalization support. 
}

begin
end;

{
  $Log$
  Revision 1.5  1999-02-28 13:18:11  michael
  + Added internationalization support

  Revision 1.4  1999/02/24 15:57:30  michael
  + Moved getlocaltime to system-dependent files

  Revision 1.3  1999/02/09 12:01:03  michael
  + Implemented filetruncate

  Revision 1.2  1999/02/03 11:41:30  michael
  + Added filetruncate

  Revision 1.1  1998/10/11 12:21:01  michael
  Added file calls. Implemented for linux only

}
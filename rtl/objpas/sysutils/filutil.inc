{%MainUnit sysutils.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    File utility calls

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



{$ifndef SYSUTILS_HAS_UNICODESTR_FILEUTIL_IMPL}
Function FileOpen (Const FileName : unicodestring; Mode : Integer) : THandle;
begin
  Result:=FileOpen(ToSingleByteFileSystemEncodedFileName(FileName),Mode);
end;


Function FileCreate (Const FileName : UnicodeString) : THandle;
begin
  Result:=FileCreate(ToSingleByteFileSystemEncodedFileName(FileName));
end;


Function FileCreate (Const FileName : UnicodeString; Rights : Integer) : THandle;
begin
  Result:=FileCreate(ToSingleByteFileSystemEncodedFileName(FileName),Rights);
end;


Function FileCreate (Const FileName : UnicodeString; ShareMode : Integer; Rights : Integer) : THandle;
begin
  Result:=FileCreate(ToSingleByteFileSystemEncodedFileName(FileName),ShareMode,Rights);
end;


Function FileAge (Const FileName : UnicodeString): Int64;
begin
  Result:=FileAge(ToSingleByteFileSystemEncodedFileName(FileName));
end;


Function FileExists (Const FileName : UnicodeString; FollowLink : Boolean) : Boolean;
begin
  Result:=FileExists(ToSingleByteFileSystemEncodedFileName(FileName), FollowLink);
end;


Function DirectoryExists (Const Directory : UnicodeString; FollowLink : Boolean) : Boolean;
begin
  Result:=DirectoryExists(ToSingleByteFileSystemEncodedFileName(Directory), FollowLink);
end;


Function FileGetAttr (Const FileName : UnicodeString) : Longint;
begin
  Result:=FileGetAttr(ToSingleByteFileSystemEncodedFileName(FileName));
end;


Function FileSetAttr (Const Filename : UnicodeString; Attr: longint) : Longint;
begin
  Result:=FileSetAttr(ToSingleByteFileSystemEncodedFileName(FileName),Attr);
end;


Function DeleteFile (Const FileName : UnicodeString) : Boolean;
begin
  Result:=DeleteFile(ToSingleByteFileSystemEncodedFileName(FileName));
end;


Function RenameFile (Const OldName, NewName : UnicodeString) : Boolean;

begin
  Result:=RenameFile(ToSingleByteFileSystemEncodedFileName(OldName),
                     ToSingleByteFileSystemEncodedFileName(NewName));
end;

{$ifdef OS_FILEISREADONLY}
Function FileIsReadOnly(const FileName: UnicodeString): Boolean;
begin
  Result:=FileIsReadOnly(ToSingleByteFileSystemEncodedFileName(FileName));
end;
{$endif}


{$ifdef OS_FILESETDATEBYNAME}
Function FileSetDate (Const FileName : UnicodeString;Age : Int64) : Longint;
begin
  Result:=FileSetDate(ToSingleByteFileSystemEncodedFileName(FileName),Age);
end;
{$endif}


function FileAge(const FileName: RawByteString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
Var
  Info : TRawByteSearchRec;
  A : Integer;
begin
  for A:=1 to Length(FileName) do
    if CharInSet(FileName[A],['?','*']) then
      Exit(False);
  A:=0;
  if not FollowLink then
    A:=A or faSymLink;
  Result:=FindFirst(FileName,A,Info)=0;
  if Result then
    begin
      FileDateTime:=FileDatetoDateTime(Info.Time);
      FindClose(Info);
    end;
end;


function FileAgeUTC(const FileName: RawByteString; out FileDateTimeUTC: TDateTime; FollowLink: Boolean = True): Boolean;
Var
  Info : TRawByteSearchRec;
  A : Integer;
begin
  for A:=1 to Length(FileName) do
    if CharInSet(FileName[A],['?','*']) then
      Exit(False);
  A:=0;
  if not FollowLink then
    A:=A or faSymLink;
  Result:=FindFirst(FileName,A,Info)=0;
  if Result then
    begin
      FileDateTimeUTC:=FileDateToUniversal(Info.Time);
      FindClose(Info);
    end;
end;


Function FileAge(const FileName: UnicodeString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
begin
  Result:=FileAge(ToSingleByteFileSystemEncodedFileName(FileName),FileDateTime,FollowLink);
end;


Function FileAgeUTC(const FileName: UnicodeString; out FileDateTimeUTC: TDateTime; FollowLink: Boolean = True): Boolean;
begin
  Result:=FileAgeUTC(ToSingleByteFileSystemEncodedFileName(FileName),FileDateTimeUTC,FollowLink);
end;


function FileGetSymLinkTarget(const FileName: UnicodeString; out SymLinkRec: TUnicodeSymLinkRec): Boolean;
var
  sr: TRawbyteSymLinkRec;
begin
  Result := FileGetSymLinkTarget(ToSingleByteFileSystemEncodedFileName(FileName), sr);
  if Result then
    begin
      SymLinkRec.TargetName := UnicodeString(sr.TargetName);
      SymLinkRec.Size := sr.Size;
      SymLinkRec.Attr := sr.Attr;
{$ifdef SYMLINKREC_USEFINDDATA}
      SymLinkRec.FindData := sr.FindData;
{$endif}
{$ifdef unix}
      SymLinkRec.Mode := sr.Mode;
{$endif}
    end;
end;


Function FileSearch (Const Name, DirList : UnicodeString; Options : TFileSearchoptions = [sfoImplicitCurrentDir]) : UnicodeString;
begin
  Result:=UnicodeString(FileSearch(ToSingleByteFileSystemEncodedFileName(Name),
                                   ToSingleByteFileSystemEncodedFileName(Dirlist),Options));
end;


Function FileSearch (Const Name, DirList : UnicodeString; ImplicitCurrentDir : Boolean) : UnicodeString;
begin
  Result:=UnicodeString(FileSearch(ToSingleByteFileSystemEncodedFileName(Name),
                                   ToSingleByteFileSystemEncodedFileName(DirList),ImplicitCurrentDir));
end;


Function ExeSearch (Const Name : UnicodeString; Const DirList : UnicodeString ='' ) : UnicodeString;
begin
  Result:=UnicodeString(ExeSearch(ToSingleByteFileSystemEncodedFileName(Name),
                                  ToSingleByteFileSystemEncodedFileName(Dirlist)));
end;



Function FileSearch (Const Name, DirList : RawByteString; Options : TFileSearchoptions = [sfoImplicitCurrentDir]) : RawByteString;
Var
  I : longint;
  Temp : RawByteString;
begin
  Result:=Name;
  temp:=SetDirSeparators(DirList);
  // Start with checking the file in the current directory
  If (sfoImplicitCurrentDir in Options) and (Result <> '') and FileExists(Result) Then
    exit;
  while True do begin
    If Temp = '' then
      Break; // No more directories to search - fail
    I:=pos(PathSeparator,Temp);
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
    If Result<>'' then
      begin
      If (sfoStripQuotes in Options) and (Result[1]='"') and (Result[Length(Result)]='"') then
        Result:=Copy(Result,2,Length(Result)-2);
      if (Result<>'') then
        Result:=IncludeTrailingPathDelimiter(Result)+name;
      end;
    If (Result <> '') and FileExists(Result) Then
      exit;
  end;
  Result:='';
end;


Function FileSearch (Const Name, DirList : RawByteString; ImplicitCurrentDir : Boolean) : RawByteString;
begin
  if ImplicitCurrentDir then
    Result:=FileSearch(Name,DirList,[sfoImplicitCurrentDir])
  else
    Result:=FileSearch(Name,DirList,[]);
end;


Function ExeSearch (Const Name : RawByteString; Const DirList : RawByteString ='' ) : RawByteString;
Var
  D : RawByteString;
  O : TFileSearchOptions;
begin
  D:=DirList;
  if (D='') then
    D:=GetEnvironmentVariable('PATH');
{$ifdef unix}
  O:=[];
{$else unix}
  O:=[sfoImplicitCurrentDir,sfoStripQuotes];
{$endif unix}
  Result := FileSearch(Name, D, O);
end;

{$endif}

{$ifndef SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
Function FileOpen (Const FileName : rawbytestring; Mode : Integer) : THandle;
begin
  Result:=FileOpen(UnicodeString(FileName),Mode);
end;


Function FileCreate (Const FileName : RawByteString) : THandle;
begin
  Result:=FileCreate(UnicodeString(FileName));
end;


Function FileCreate (Const FileName : RawByteString; Rights : Integer) : THandle;
begin
  Result:=FileCreate(UnicodeString(FileName),Rights);
end;


Function FileCreate (Const FileName : RawByteString; ShareMode : Integer; Rights : Integer) : THandle;
begin
  Result:=FileCreate(UnicodeString(FileName),ShareMode,Rights);
end;


Function FileAge (Const FileName : RawByteString): Int64;
begin
  Result:=FileAge(UnicodeString(FileName));
end;


Function FileExists (Const FileName : RawByteString; FollowLink : Boolean) : Boolean;
begin
  Result:=FileExists(UnicodeString(FileName), FollowLink);
end;


Function DirectoryExists (Const Directory : RawByteString; FollowLink : Boolean) : Boolean;
begin
  Result:=DirectoryExists(UnicodeString(Directory), FollowLink);
end;


Function FileGetAttr (Const FileName : RawByteString) : Longint;
begin
  Result:=FileGetAttr(unicodestring(FileName));
end;


Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
begin
  Result:=FileSetAttr(unicodestring(FileName),Attr);
end;


Function DeleteFile (Const FileName : RawByteString) : Boolean;
begin
  Result:=DeleteFile(UnicodeString(FileName));
end;


Function RenameFile (Const OldName, NewName : RawByteString) : Boolean;
begin
  Result:=RenameFile(UnicodeString(OldName),UnicodeString(NewName));
end;

{$ifdef OS_FILEISREADONLY}
Function FileIsReadOnly(const FileName: RawByteString): Boolean;
begin
  Result:=FileIsReadOnly(UnicodeString(FileName));
end;
{$endif}

{$ifdef OS_FILESETDATEBYNAME}
Function FileSetDate (Const FileName : RawByteString;Age : Int64) : Longint;
begin
  Result:=FileSetDate(UnicodeString(FileName),Age);
end;
{$endif}


function FileAge(const FileName: UnicodeString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
Var
  Info : TUnicodeSearchRec;
  A : Integer;

begin
  for A:=1 to Length(FileName) do
    if CharInSet(FileName[A],['?','*']) then
      Exit(False);
  A:=0;
  if not FollowLink then
    A:=A or faSymLink;
  Result:=FindFirst(FileName,A,Info)=0;
  if Result then
    begin
      FileDateTime:=Info.TimeStamp;
      FindClose(Info);
    end;
end;


function FileAgeUTC(const FileName: UnicodeString; out FileDateTimeUTC: TDateTime; FollowLink: Boolean = True): Boolean;
Var
  Info : TUnicodeSearchRec;
  A : Integer;

begin
  for A:=1 to Length(FileName) do
    if CharInSet(FileName[A],['?','*']) then
      Exit(False);
  A:=0;
  if not FollowLink then
    A:=A or faSymLink;
  Result:=FindFirst(FileName,A,Info)=0;
  if Result then
    begin
      FileDateTimeUTC:=Info.TimeStampUTC;
      FindClose(Info);
    end;
end;


Function FileAge(const FileName: RawbyteString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
begin
  Result:=FileAge(UnicodeString(FileName),FileDateTime,FollowLink);
end;


function FileAgeUTC(const FileName: RawByteString; out FileDateTimeUTC: TDateTime; FollowLink: Boolean = True): Boolean;
begin
  Result:=FileAgeUTC(UnicodeString(FileName),FileDateTimeUTC,FollowLink);
end;


function FileGetSymLinkTarget(const FileName: RawByteString; out SymLinkRec: TRawbyteSymLinkRec): Boolean;
var
  sr: TUnicodeSymLinkRec;
begin
  Result := FileGetSymLinkTarget(UnicodeString(FileName), sr);
  if Result then
    begin
      SymLinkRec.TargetName := ToSingleByteFileSystemEncodedFileName(sr.TargetName);
      SymLinkRec.Size := sr.Size;
      SymLinkRec.Attr := sr.Attr;
{$ifdef SYMLINKREC_USEFINDDATA}
      SymLinkRec.FindData := sr.FindData;
{$endif}
{$ifdef unix}
      SymLinkRec.Mode := sr.Mode;
{$endif}
    end;
end;


Function FileSearch (Const Name, DirList : UnicodeString; Options : TFileSearchoptions = [sfoImplicitCurrentDir]) : UnicodeString;
Var
  I : longint;
  Temp : UnicodeString;
begin
  Result:=Name;
  temp:=SetDirSeparators(DirList);
  // Start with checking the file in the current directory
  If (sfoImplicitCurrentDir in Options) and (Result <> '') and FileExists(Result) Then
    exit;
  while True do begin
    If Temp = '' then
      Break; // No more directories to search - fail
    I:=pos(PathSeparator,Temp);
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
    If Result<>'' then
      begin
      If (sfoStripQuotes in Options) and (Result[1]='"') and (Result[Length(Result)]='"') then
        Result:=Copy(Result,2,Length(Result)-2);
      if (Result<>'') then
        Result:=IncludeTrailingPathDelimiter(Result)+name;
      end;
    If (Result <> '') and FileExists(Result) Then
      exit;
  end;
  Result:='';
end;


Function FileSearch (Const Name, DirList : RawbyteString; Options : TFileSearchoptions = [sfoImplicitCurrentDir]) : RawByteString;
begin
  Result:=ToSingleByteFileSystemEncodedFileName(FileSearch(unicodestring(name),unicodestring(dirlist),options));
end;


Function FileSearch (Const Name, DirList : RawbyteString; ImplicitCurrentDir : Boolean) : RawByteString;
begin
  Result:=ToSingleByteFileSystemEncodedFileName(FileSearch(unicodestring(name),unicodestring(dirlist),ImplicitCurrentDir));
end;


Function FileSearch (Const Name, DirList : UnicodeString; ImplicitCurrentDir : Boolean) : UnicodeString;
begin
  if ImplicitCurrentDir then
    Result:=FileSearch(Name,DirList,[sfoImplicitCurrentDir])
  else
    Result:=FileSearch(Name,DirList,[]);
end;


Function ExeSearch (Const Name : UnicodeString; Const DirList : UnicodeString ='' ) : UnicodeString;
Var
  D : UnicodeString;
  O : TFileSearchOptions;
begin
  D:=DirList;
  if (D='') then
    D:=UnicodeString(GetEnvironmentVariable('PATH'));
{$ifdef unix}
  O:=[];
{$else unix}
  O:=[sfoImplicitCurrentDir,sfoStripQuotes];
{$endif unix}
  Result := FileSearch(Name, D, O);
end;


Function ExeSearch (Const Name : RawbyteString; Const DirList : RawbyteString ='' ) : RawByteString;
begin
  Result:=ToSingleByteFileSystemEncodedFileName(ExeSearch(unicodestring(name),unicodestring(dirlist)));
end;
{$endif}


function FileGetSymLinkTarget(const FileName: UnicodeString; out TargetName: UnicodeString): Boolean;
var
  sr: TUnicodeSymLinkRec;
begin
  Result := FileGetSymLinkTarget(FileName, sr);
  if Result then
    TargetName := sr.TargetName;
end;


function FileGetSymLinkTarget(const FileName: RawByteString; out TargetName: RawByteString): Boolean;
var
  sr: TRawbyteSymLinkRec;
begin
  Result := FileGetSymLinkTarget(FileName, sr);
  if Result then
    TargetName := sr.TargetName;
end;


Function GetFileHandle(var f : File):THandle;
begin
  Result:=filerec(f).handle;
end;


Function GetFileHandle(var f : Text):THandle;
begin
  Result:=textrec(f).handle;
end;


{ FindFirst/FindNext. In order to avoid having to duplicate most code in th
  OS-specific implementations, we let those implementations fill in all
  fields of TRawbyte/UnicodeSearchRec, except for the name. That field is
  filled in by the OS-indepedent wrappers, which also takes care of setting
  the appropriate code page if applicable.
}

type
  TAbstractSearchRec = Record
    Time : Int64;
    Size : Int64;
    Attr : Longint;
    { this will be assigned by the generic code; it is actually either a
      rawbytestring or unicodestring; keep it a reference-counted type
      so that -gt doesn't overwrite it, the field name should be
      indication enough that you should not touch it }
    Name_do_not_touch : RawByteString;
    ExcludeAttr : Longint;
    FindHandle : {$ifdef FINDHANDLE_IS_POINTER}Pointer{$else}THandle{$endif};
  {$ifdef unix}
    Mode : TMode;
  {$endif unix}
  {$ifdef SEARCHREC_USEFINDDATA}
    FindData : TFindData;
  {$endif}
  end;


{$ifdef SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
Function InternalFindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: RawByteString) : Longint; forward;
Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : RawByteString) : Longint; forward;
{$endif SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}

{$ifdef SYSUTILS_HAS_UNICODESTR_FILEUTIL_IMPL}
Function InternalFindFirst (Const Path : UnicodeString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: UnicodeString) : Longint; forward;
Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : UnicodeString) : Longint; forward;
{$endif SYSUTILS_HAS_UNICODESTR_FILEUTIL_IMPL}

procedure InternalFindClose(var Handle: {$ifdef FINDHANDLE_IS_POINTER}Pointer{$else}THandle{$endif}{$ifdef SEARCHREC_USEFINDDATA};var FindData: TFindData{$endif}); forward;


{$ifndef SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
Function FindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TRawByteSearchRec) : Longint;
var
  Name: UnicodeString;
begin
  Result:=InternalFindFirst(UnicodeString(Path),Attr,TAbstractSearchRec(Rslt),Name);
  if Result=0 then
    widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(Name),Rslt.Name,DefaultRTLFileSystemCodePage,length(Name));
end;


Function FindNext (Var Rslt : TRawByteSearchRec) : Longint;
var
  Name: UnicodeString;
begin
  Result:=InternalFindNext(TAbstractSearchRec(Rslt),Name);
  if Result=0 then
    widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(Name),Rslt.Name,DefaultRTLFileSystemCodePage,length(Name));
end;

{$else not SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}

Function FindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TRawByteSearchRec) : Longint;
begin
  Result:=InternalFindFirst(Path,Attr,TAbstractSearchRec(Rslt),Rslt.Name);
  if Result=0 then
    SetCodePage(Rslt.Name,DefaultRTLFileSystemCodePage);
end;


Function FindNext (Var Rslt : TRawByteSearchRec) : Longint;
begin
  Result:=InternalFindNext(TAbstractSearchRec(Rslt),Rslt.Name);
  if Result=0 then
    SetCodePage(Rslt.Name,DefaultRTLFileSystemCodePage);
end;

{$endif not SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}


{$ifndef SYSUTILS_HAS_UNICODESTR_FILEUTIL_IMPL}
Function FindFirst (Const Path : UnicodeString; Attr : Longint; out Rslt : TUnicodeSearchRec) : Longint;
var
  Name: RawByteString;
begin
  Result:=InternalFindFirst(ToSingleByteFileSystemEncodedFileName(Path),Attr,TAbstractSearchRec(Rslt),Name);
  if Result=0 then
    Rslt.Name:=UnicodeString(Name);
end;


Function FindNext (Var Rslt : TUnicodeSearchRec) : Longint;
var
  Name: RawByteString;
begin
  Result:=InternalFindNext(TAbstractSearchRec(Rslt),Name);
  if Result=0 then
    Rslt.Name:=UnicodeString(Name);
end;

{$else not SYSUTILS_HAS_UNICODESTR_FILEUTIL_IMPL}

Function FindFirst (Const Path : UnicodeString; Attr : Longint; out Rslt : TUnicodeSearchRec) : Longint;
begin
  Result:=InternalFindFirst(Path,Attr,TAbstractSearchRec(Rslt),Rslt.Name);
end;


Function FindNext (Var Rslt : TUnicodeSearchRec) : Longint;
begin
  Result:=InternalFindNext(TAbstractSearchRec(Rslt),Rslt.Name);
end;

{$endif not SYSUTILS_HAS_UNICODESTR_FILEUTIL_IMPL}

Procedure FindClose(Var f: TRawByteSearchRec);
begin
  InternalFindClose(f.FindHandle{$ifdef SEARCHREC_USEFINDDATA},f.FindData{$endif});
end;


Procedure FindClose(Var f: TUnicodeSearchRec);
begin
  InternalFindClose(f.FindHandle{$ifdef SEARCHREC_USEFINDDATA},f.FindData{$endif});
end;


{$if defined(windows) and not defined(win16)}
function FindDataTimeToDateTime(constref aFileTime: FILETIME; out aResult: TDateTime): Boolean;
var
  st, lt: TSystemTime;
begin
  Result := FileTimeToSystemTime(aFileTime, st);
  if Result then begin
{$ifndef wince}
    if SystemTimeToTzSpecificLocalTime(Nil, st, lt) then
      aResult := SystemTimeToDateTime(lt)
    else
{$endif}
      aResult := SystemTimeToDateTime(st);
  end;
end;

function FindDataTimeToUTC(constref aFileTime: FILETIME; out aResult: TDateTime): Boolean;
var
  st: TSystemTime;
begin
  Result := FileTimeToSystemTime(aFileTime, st);
  if Result then
    aResult := SystemTimeToDateTime(st);
end;
{$endif}

{ TDateTimeInfoRec }

function TDateTimeInfoRec.GetCreationTime: TDateTime;
begin
  {$if defined(windows) and not defined(win16)}
    FindDataTimeToDateTime(Data.ftCreationTime,Result)
  {$else}
    {$ifdef unix}
      {$ifdef use_statx}
        Result:=FileDateToDateTime(data.stx_ctime.tv_sec)
      {$else}
        Result:=FileDateToDateTime(data.st_ctime)
      {$endif}
    {$else}
        Result:=FileDateToDateTime(Data.Time);
    {$ENDIF}
  {$ENDIF}
end;

function TDateTimeInfoRec.GetLastAccessTime: TDateTime;
begin
  {$if defined(windows) and not defined(win16)}
    FindDataTimeToDateTime(Data.ftLastAccessTime,Result)
  {$else}
    {$ifdef unix}
      {$ifdef use_statx}
        Result:=FileDateToDateTime(data.stx_atime.tv_sec)
      {$else}
        Result:=FileDateToDateTime(data.st_atime)
      {$endif}
    {$else}
      Result:=FileDateToDateTime(Data.Time);
    {$ENDIF}
  {$ENDIF}
end;

function TDateTimeInfoRec.GetTimeStamp: TDateTime;
begin
  {$if defined(windows) and not defined(win16)}
    FindDataTimeToDateTime(Data.ftLastWriteTime,Result)
  {$else}
    {$ifdef unix}
      {$ifdef use_statx}
        Result:=FileDateToDateTime(data.stx_mtime.tv_sec)
      {$else}
        Result:=FileDateToDateTime(data.st_mtime)
      {$endif}
    {$else}
       Result:=FileDateToDateTime(Data.Time);
    {$ENDIF}
  {$ENDIF}
end;


{ TUnicodeSearchRec }

function TUnicodeSearchRec.GetTimeStamp: TDateTime;
begin
{$if declared(FindDataTimeToDateTime)}
  if not FindDataTimeToDateTime(FindData.ftLastWriteTime, Result) then
{$endif}
    Result := FileDateToDateTime(Time);
end;

function TUnicodeSearchRec.GetTimeStampUTC: TDateTime;
begin
{$if declared(FindDataTimeToUTC)}
  if not FindDataTimeToUTC(FindData.ftLastWriteTime, Result) then
{$endif}
    Result := FileDateToUniversal(Time);
end;

function TUnicodeSearchRec.IsDirectory: Boolean;
begin
  Result:=(Attr and faDirectory)<>0;
end;

function TUnicodeSearchRec.IsCurrentOrParentDir: Boolean;
begin
  Result:=IsDirectory and ((Name='.') or (Name='..'));
end;

{ TRawbyteSearchRec }

function TRawbyteSearchRec.GetTimeStamp: TDateTime;
begin
{$if declared(FindDataTimeToDateTime)}
  if not FindDataTimeToDateTime(FindData.ftLastWriteTime, Result) then
{$endif}
    Result := FileDateToDateTime(Time);
end;

function TRawbyteSearchRec.GetTimeStampUTC: TDateTime;
begin
{$if declared(FindDataTimeToDateTime)}
  if not FindDataTimeToUTC(FindData.ftLastWriteTime, Result) then
{$endif}
    Result := FileDateToUniversal(Time);
end;

function TRawbyteSearchRec.IsDirectory: Boolean;
begin
  Result:=(Attr and faDirectory)<>0;
end;

function TRawbyteSearchRec.IsCurrentOrParentDir: Boolean;
begin
  Result:=IsDirectory and ((Name='.') or (Name='..'));
end;

{ TUnicodeSymLinkRec }

function TUnicodeSymLinkRec.GetTimeStamp: TDateTime;
begin
{$if declared(FindDataTimeToDateTime)}
  if not FindDataTimeToDateTime(FindData.ftLastWriteTime, Result) then
{$endif}
    Result := 0;
end;

{ TRawbyteSymLinkRec }

function TRawbyteSymLinkRec.GetTimeStamp: TDateTime;
begin
{$if declared(FindDataTimeToDateTime)}
  if not FindDataTimeToDateTime(FindData.ftLastWriteTime, Result) then
{$endif}
    Result := 0;
end;


{$ifndef SYSUTILS_HAS_FILEFLUSH_IMPL}
function FileFlush(Handle: THandle): Boolean;
begin
  Result:= False;
end;
{$endif}

Function GetFileContents(Const aFileName : RawByteString) : TBytes;

Var
  H : Thandle;

begin
  H:=FileOpen(aFileName,fmOpenRead or fmShareDenyWrite);
  if H<0 then
    Raise EFileNotFoundException.Create(SFileNotFound);
  try
    Result:=GetFileContents(H);
  finally
    FileClose(H);
  end;
end;

Function GetFileContents(Const aFileName : UnicodeString) : TBytes;

Var
  H : Thandle;

begin
  H:=FileOpen(aFileName,fmOpenRead or fmShareDenyWrite);
  if H<0 then
    Raise EFileNotFoundException.Create(SFileNotFound);
  try
    Result:=GetFileContents(H);
  finally
    FileClose(H);
  end;
end;

Function GetFileContents(Const aHandle : THandle) : TBytes;

Var
  aLen,aOffset,aRead : Int64;
  aBuf : PByte;

begin
  aLen:=FileSeek(aHandle,0,fsFromEnd);
  if aLen<0 then
    Raise EInOutError.Create(SSeekFailed);
  if FileSeek(aHandle,0,fsFromBeginning)<0 then
    Raise EInOutError.Create(SSeekFailed);
  SetLength(Result,aLen);
  if aLen=0 then
    Exit;
  aOffset:=0;
  Repeat
    aBuf:=@Result[aOffset];
    aRead:=FileRead(aHandle,aBuf^,aLen-aOffset);
    aOffset:=aOffset+aRead;
  Until (aOffset>=aLen) or (aRead<=0);
  if aRead<0 then
    RaiseLastOSError;
end;

Function GetFileAsString(Const aFileName : RawByteString; aEncoding : TEncoding) : RawByteString;

begin
  Result:=aEncoding.GetAnsiString(GetFileContents(aFileName));
end;

Function GetFileAsString(Const aFileName : RawByteString) : RawByteString;

begin
  Result:=GetFileAsString(aFileName,TEncoding.SystemEncoding);
end;

Function GetFileAsString(Const aFileName : UnicodeString) : UnicodeString;
begin
  Result:=GetFileAsString(aFileName, TEncoding.Unicode);
end;

Function GetFileAsString(Const aFileName : UnicodeString; aEncoding : TEncoding) : UnicodeString;
begin
  Result:=aEncoding.GetString(GetFileContents(aFileName))
end;

{$IFNDEF HAS_FILEDATETIME}
Function FileGetDate (Handle : THandle; out FileDateTime: TDateTime) : Boolean;
Var
  aDate : Int64;
begin
  aDate:=FileGetDate(Handle);
  Result:=aDate>=0;
  if Result then
    FileDateTime:=FileDateToDateTime(aDate);
end;

Function FileGetDateUTC (Handle : THandle; out FileDateTimeUTC: TDateTime) : Boolean;
Var
  aDate : Int64;
begin
  aDate:=FileGetDate(Handle);
  Result:=aDate>=0;
  if Result then
    FileDateTimeUTC:=FileDateToUniversal(aDate);
end;

Function FileSetDate (Handle : THandle; const FileDateTime: TDateTime) : Longint;
begin
  Result := FileSetDate(Handle,DateTimeToFileDate(FileDateTime));
end;

Function FileSetDateUTC (Handle : THandle; const FileDateTimeUTC: TDateTime) : Longint;
begin
  Result := FileSetDate(Handle,UniversalToFileDate(FileDateTimeUTC));
end;


Function FileSetDate (Const FileName : UnicodeString;const FileDateTime : TDateTime) : Longint;
begin
  Result:=FileSetDate(FileName,DateTimeToFileDate(FileDateTime));
end;

Function FileSetDateUTC (Const FileName : UnicodeString;const FileDateTimeUTC : TDateTime) : Longint;
begin
  Result:=FileSetDate(FileName,UniversalToFileDate(FileDateTimeUTC));
end;

{$ENDIF}

Function FileSetDate (Const FileName : RawByteString;const FileDateTime : TDateTime) : Longint;
begin
  Result:=FileSetDate(UnicodeString(FileName),FileDateTime);
end;

Function FileSetDateUTC (Const FileName : RawByteString;const FileDateTimeUTC : TDateTime) : Longint;
begin
  Result:=FileSetDateUTC(UnicodeString(FileName),FileDateTimeUTC);
end;

{$IFNDEF HAS_ISFILENAMECASESENSITIVE}
Function IsFileNameCaseSensitive(Const aFileName : RawByteString) : Boolean;
begin
  Result:=FileNameCaseSensitive;
end;

Function IsFileNameCaseSensitive(Const aFileName : UnicodeString) : Boolean;
begin
  Result:=FileNameCaseSensitive;
end;
{$ENDIF HAS_ISFILENAMECASESENSITIVE}

{$IFNDEF HAS_ISFILENAMECASEPRESERVING}
Function IsFileNameCasePreserving(Const aFileName : RawByteString) : Boolean;
begin
  Result:=FileNameCasePreserving;
end;

Function IsFileNameCasePreserving(Const aFileName : UnicodeString) : Boolean;
begin
  Result:=FileNameCasePreserving;
end;
{$ENDIF HAS_ISFILENAMECASEPRESERVING}

{$IFNDEF HAS_FILEGETDATETIMEINFO}
function FileGetDateTimeInfo(const FileName: string;
  out DateTime: TDateTimeInfoRec; FollowLink: Boolean = True): Boolean;

var
  Info : TSearchRec;

begin
  Result:=FindFirst(FileName,0,Info)=0;
  if Result then
     begin
     DateTime.data:=Info;
     FindClose(Info);
     end;
end;
{$ENDIF}



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

Function FileAge (Const FileName : UnicodeString): Longint;
begin
  Result:=FileAge(ToSingleByteFileSystemEncodedFileName(FileName));
end;

Function FileExists (Const FileName : UnicodeString) : Boolean;

begin
  Result:=FileExists(ToSingleByteFileSystemEncodedFileName(FileName));
end;

Function DirectoryExists (Const Directory : UnicodeString) : Boolean;
begin
  Result:=DirectoryExists(ToSingleByteFileSystemEncodedFileName(Directory));
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

Function FileIsReadOnly(const FileName: UnicodeString): Boolean;

begin
  Result:=FileIsReadOnly(ToSingleByteFileSystemEncodedFileName(FileName));
end;

Function FileSetDate (Const FileName : UnicodeString;Age : Longint) : Longint;

begin
  Result:=FileSetDate(ToSingleByteFileSystemEncodedFileName(FileName),Age);
end;

function FileAge(const FileName: RawByteString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
   
Var
  Info : TSearchRec;
  A : Integer;
      
begin
  for A:=1 to Length(FileName) do
    If (FileName[A] in ['?','*']) then
      Exit(False);
  A:=0;
  if Not FollowLink then
    A:=A or faSymLink;
  Result:=FindFirst(FileName,A,Info)=0;
  If Result then 
    FileDateTime:=FileDatetoDateTime (Info.Time);
  FindClose(Info);
end;

Function FileAge(const FileName: UnicodeString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
begin
  Result:=FileAge(ToSingleByteFileSystemEncodedFileName(FileName),FileDateTime,FollowLink);
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

Function FileAge (Const FileName : RawByteString): Longint;
begin
  Result:=FileAge(UnicodeString(FileName));
end;

Function FileExists (Const FileName : RawByteString) : Boolean;

begin
  Result:=FileExists(UnicodeString(FileName));
end;

Function DirectoryExists (Const Directory : RawByteString) : Boolean;
begin
  Result:=DirectoryExists(UnicodeString(Directory));
end;

Function FindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TSearchRec) : Longint;

begin
  Result:=FindFirst(unicodestring(path),attr,rslt);
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

Function FileIsReadOnly(const FileName: RawByteString): Boolean;

begin
  Result:=FileIsReadOnly(UnicodeString(FileName));
end;

Function FileSetDate (Const FileName : RawByteString;Age : Longint) : Longint;

begin
  Result:=FileSetDate(UnicodeString(FileName),Age);
end;

function FileAge(const FileName: UnicodeString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
   
Var
  Info : TSearchRec;
  A : Integer;
      
begin
  for A:=1 to Length(FileName) do
    If (FileName[A] in ['?','*']) then
      Exit(False);
  A:=0;
  if Not FollowLink then
    A:=A or faSymLink;
  Result:=FindFirst(FileName,A,Info)=0;
  If Result then 
    FileDateTime:=FileDatetoDateTime (Info.Time);
  FindClose(Info);
end;

Function FileAge(const FileName: RawbyteString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
begin
  Result:=FileAge(UnicodeString(FileName),FileDateTime,FollowLink);
end;


Function FileSearch (Const Name, DirList : RawByteString; Options : TFileSearchoptions = [sfoImplicitCurrentDir]) : RawByteString;

begin
  Result:=ToSingleByteFileSystemEncodedFileName(FileSearch(UnicodeString(Name),UnicodeString(Dirlist),Options));
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
  result:='';
end;

Function FileSearch (Const Name, DirList : RawByteString; ImplicitCurrentDir : Boolean) : RawByteString;

begin
  Result:=ToSingleByteFileSystemEncodedFileName(FileSearch(UnicodeString(Name),UnicodeString(DirList),ImplicitCurrentDir));
end;

Function FileSearch (Const Name, DirList : UnicodeString; ImplicitCurrentDir : Boolean) : UnicodeString;

begin
  if ImplicitCurrentDir then
    Result:=FileSearch(Name,DirList,[sfoImplicitCurrentDir])
  else  
    Result:=FileSearch(Name,DirList,[]);
end;
    
Function ExeSearch (Const Name : RawByteString; Const DirList : RawByteString ='' ) : RawByteString;

begin
  Result:=ToSingleByteFileSystemEncodedFileName(ExeSearch(UnicodeString(Name),UnicodeString(Dirlist)));
end;

Function ExeSearch (Const Name : UnicodeString; Const DirList : UnicodeString ='' ) : UnicodeString;

Var
  D : UnicodeString;
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
  result:='';
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

{$IFNDEF FPC_UNICODE_RTL}
function FileAge(const FileName: RawByteString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;

Var
  Info : TSearchRec;
  A : Integer;
   
begin
  for A:=1 to Length(FileName) do
    If (FileName[A] in ['?','*']) then
      Exit(False);
  A:=0;
  if Not FollowLink then
    A:=A or faSymLink;
  Result:=FindFirst(FileName,A,Info)=0;
  If Result then 
    FileDateTime:=FileDatetoDateTime (Info.Time);
  FindClose(Info);
end;
{$ENDIF}

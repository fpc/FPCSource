{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    File utility calls

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type

{$IFDEF FPC_UNICODE_RTL}

  // Some operating systems need extra find data.
{$if defined(Win32) or defined(WinCE) or defined(Win64)}
    TUnicodeSearchFindData = TWin32FindDataW;
{$ENDIF}
{$ifdef netware_clib}
    TUnicodeSearchFindData = TNetwareFindData;
{$endif}
{$ifdef netware_libc}
    TUnicodeSearchFindData = TNetwareLibcFindData;
{$endif}
{$ifdef MacOS}
    TUnicodeSearchFindData = TMacOSFindData;
{$endif}
{$ifdef nativent}
    TUnicodeSearchFindData = TNativeNTFindData;
{$endif}


{$ENDIF FPC_UNICODE_RTL}

// USEFINDDATA is only defined here.

{$if defined(Win32) or defined(Win64)}
    {$DEFINE USEFINDDATA}
    TRawByteSearchFindData = TWin32FindDataA;
{$ENDIF}
{$if defined(Wince)}
    {$DEFINE USEFINDDATA}
    TRawByteSearchFindData = TWin32FindDataW;
{$ENDIF}
{$ifdef netware_clib}
    TRawByteSearchFindData = TNetwareFindData;
    {$DEFINE USEFINDDATA}
{$endif}
{$ifdef netware_libc}
    TRawByteSearchFindData = TNetwareLibcFindData;
    {$DEFINE USEFINDDATA}
{$endif}
{$ifdef MacOS}
    TRawByteSearchFindData = TMacOSFindData;
    {$DEFINE USEFINDDATA}
{$endif}
{$ifdef nativent}
    TRawByteSearchFindData = TNativeNTFindData;
    {$DEFINE USEFINDDATA}
{$endif}

{$IFDEF FPC_UNICODE_RTL}
  // The actual unicode search record
  TUnicodeSearchRec = Record
    Time : Longint;
    Size : Int64;
    Attr : Longint;
    Name : UnicodeString;
    ExcludeAttr : Longint;
{$ifdef unix}
    FindHandle : Pointer;
    Mode : TMode;
{$else unix}
    FindHandle : THandle;
{$endif unix}
{$IFDEF USEFINDDATA}
    FindData : TUnicodeSearchFindData;
{$ENDIF}    
  end;
{$ENDIF}

  TRawbyteSearchRec = Record
    Time : Longint;
    Size : Int64;
    Attr : Longint;
    Name : RawByteString;
    ExcludeAttr : Longint;
{$ifdef unix}
    FindHandle : Pointer;
    Mode : TMode;
{$else unix}
    FindHandle : THandle;
{$endif unix}
{$IFDEF USEFINDDATA}
    FindData : TRawByteSearchFindData;
{$ENDIF}    
  end;

{$IFDEF FPC_UNICODE_RTL}
  TSearchRec = TUnicodeSearchRec;
{$ELSE}
  TSearchRec = TRawbyteSearchRec;
{$ENDIF}

Const
  { File attributes }
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeId  = $00000008;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faSymLink   = $00000040;
  faAnyFile   = $0000003f;

  { File open modes }
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;
  { Share modes}
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;

  { File seek origins }
  fsFromBeginning = 0;
  fsFromCurrent   = 1;
  fsFromEnd       = 2;

  { File errors }
  feInvalidHandle : THandle = THandle(-1);  //return value on FileOpen error

Type
  TFileSearchOption = (sfoImplicitCurrentDir,sfoStripQuotes);
  TFileSearchOptions = set of TFileSearchOption;

{$IFDEF FPC_UNICODE_RTL}

Function FileOpen (Const FileName : unicodestring; Mode : Integer) : THandle;
Function FileCreate (Const FileName : UnicodeString) : THandle;
Function FileCreate (Const FileName : UnicodeString; Rights : Integer) : THandle;
Function FileCreate (Const FileName : UnicodeString; ShareMode : Integer; Rights : Integer) : THandle;
{$IFNDEF FPUNONE}
Function FileAge (Const FileName : UnicodeString): Longint;
{$ENDIF}
Function FileExists (Const FileName : UnicodeString) : Boolean;
Function DirectoryExists (Const Directory : UnicodeString) : Boolean;
Function FileSetDate (Const FileName : UnicodeString;Age : Longint) : Longint;
Function FileGetAttr (Const FileName : UnicodeString) : Longint;
Function FileSetAttr (Const Filename : UnicodeString; Attr: longint) : Longint;
Function DeleteFile (Const FileName : UnicodeString) : Boolean;
Function RenameFile (Const OldName, NewName : UnicodeString) : Boolean;
Function FindFirst (Const Path : UnicodeString; Attr : Longint; out Rslt : TUnicodeSearchRec) : Longint;
Function FindNext (Var Rslt : TUnicodeSearchRec) : Longint;
Procedure FindClose (Var F : TUnicodeSearchrec);
Function FileSearch (Const Name, DirList : UnicodeString; Options : TFileSearchoptions = [sfoImplicitCurrentDir]) : UnicodeString;
Function FileSearch (Const Name, DirList : UnicodeString; ImplicitCurrentDir : Boolean) : UnicodeString;
Function ExeSearch  (Const Name : UnicodeString; Const DirList : UnicodeString = '') : UnicodeString;
Function FileIsReadOnly(const FileName : UnicodeString): Boolean;
function FileAge(const FileName: UnicodeString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
{$ENDIF UNICODERTL}

Function FileOpen (Const FileName : RawByteString; Mode : Integer) : THandle;
Function FileCreate (Const FileName : RawByteString) : THandle;
Function FileCreate (Const FileName : RawByteString; Rights : Integer) : THandle;
Function FileCreate (Const FileName : RawByteString; ShareMode : Integer; Rights : Integer) : THandle;
Function FileExists (Const FileName : RawByteString) : Boolean;
Function DirectoryExists (Const Directory : RawByteString) : Boolean;
Function FileSetDate (Const FileName : RawByteString;Age : Longint) : Longint;
Function FileGetAttr (Const FileName : RawByteString) : Longint;
Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
Function DeleteFile (Const FileName : RawByteString) : Boolean;
Function RenameFile (Const OldName, NewName : RawByteString) : Boolean;
Function FindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TRawByteSearchRec) : Longint;
Function FileSearch (Const Name, DirList : RawByteString; Options : TFileSearchoptions = [sfoImplicitCurrentDir]) : RawByteString;
Function FileSearch (Const Name, DirList : RawByteString; ImplicitCurrentDir : Boolean) : RawByteString;
Function ExeSearch  (Const Name : RawByteString; Const DirList : RawByteString = '') : RawByteString;
Function FileIsReadOnly(const FileName: RawByteString): Boolean;
function FileAge(const FileName: RawByteString; out FileDateTime: TDateTime; FollowLink: Boolean = True): Boolean;
{$ifndef FPUNONE}
Function FileAge (Const FileName : RawByteString): Longint;
{$endif}

Function FileRead (Handle : THandle; out Buffer; Count : longint) : Longint;
Function FileWrite (Handle : THandle; const Buffer; Count : Longint) : Longint;
Function FileSeek (Handle : THandle; FOffset, Origin: Longint) : Longint;
Function FileSeek (Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
Procedure FileClose (Handle : THandle);
Function FileTruncate (Handle : THandle;Size: Int64) : boolean;
Function FindNext (Var Rslt : TRawByteSearchRec) : Longint;
Procedure FindClose (Var F : TRawByteSearchrec);
Function FileGetDate (Handle : THandle) : Longint;
Function FileSetDate (Handle : THandle;Age : Longint) : Longint;
Function GetFileHandle(var f : File):THandle;
Function GetFileHandle(var f : Text):THandle;

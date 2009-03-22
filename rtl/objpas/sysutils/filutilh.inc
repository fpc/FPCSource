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
  TSearchRec = Record
    Time : Longint;
    Size : Int64;
    Attr : Longint;
    Name : TFileName;
    ExcludeAttr : Longint;
{$ifdef unix}
    FindHandle : Pointer;
    Mode : TMode;
    PathOnly : AnsiString {$ifndef VER2_2} deprecated{$endif};
{$else unix}
    FindHandle : THandle;
{$endif unix}
{$if defined(Win32) or defined(WinCE) or defined(Win64)}
    FindData : TWin32FindData;
{$endif}
{$ifdef netware_clib}
    FindData : TNetwareFindData;
{$endif}
{$ifdef netware_libc}
    FindData : TNetwareLibcFindData;
{$endif}
{$ifdef MacOS}
    FindData : TMacOSFindData;
{$endif}
  end;

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

Function FileOpen (Const FileName : string; Mode : Integer) : THandle;
Function FileCreate (Const FileName : String) : THandle;
Function FileCreate (Const FileName : String; Mode : Integer) : THandle;
Function FileRead (Handle : THandle; Var Buffer; Count : longint) : Longint;
Function FileWrite (Handle : THandle; const Buffer; Count : Longint) : Longint;
Function FileSeek (Handle : THandle; FOffset, Origin: Longint) : Longint;
Function FileSeek (Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
Procedure FileClose (Handle : THandle);
Function FileTruncate (Handle : THandle;Size: Int64) : boolean;
{$ifndef FPUNONE}
Function FileAge (Const FileName : String): Longint;
{$endif}
Function FileExists (Const FileName : String) : Boolean;
Function DirectoryExists (Const Directory : String) : Boolean;
Function FindFirst (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;
Function FindNext (Var Rslt : TSearchRec) : Longint;
Procedure FindClose (Var F : TSearchrec);
Function FileGetDate (Handle : THandle) : Longint;
Function FileSetDate (Handle : THandle;Age : Longint) : Longint;
Function FileSetDate (Const FileName : String;Age : Longint) : Longint;
Function FileGetAttr (Const FileName : String) : Longint;
Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
Function DeleteFile (Const FileName : String) : Boolean;
Function RenameFile (Const OldName, NewName : String) : Boolean;
Function FileSearch (Const Name, DirList : String; ImplicitCurrentDir : Boolean = True) : String;
Function ExeSearch  (Const Name, DirList : String) : String;
Function FileIsReadOnly(const FileName: String): Boolean;

Function GetFileHandle(var f : File):Longint;
Function GetFileHandle(var f : Text):Longint;


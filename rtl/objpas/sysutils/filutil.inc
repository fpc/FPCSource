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

Function ToSingleByteFileSystemEncodedFileName(const Str: UnicodeString): RawByteString;
Begin
  widestringmanager.Unicode2AnsiMoveProc(punicodechar(Str),Result,
    DefaultFileSystemCodePage,Length(Str));
End;


Function ToSingleByteFileSystemEncodedFileName(const Str: RawByteString): RawByteString;
Begin
  Result:=Str;
  SetCodePage(Result,DefaultFileSystemCodePage,True);
End;


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
{$endif}


{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WithOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                    subroutines For UnTyped File handling
****************************************************************************}

type
  UnTypedFile=File;

procedure InitFile(var f : file);
begin
  FillChar(f,SizeOf(FileRec),0);
  FileRec(f).Handle:=UnusedHandle;
  FileRec(f).mode:=fmClosed;
end;


{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
Procedure Assign(out f:File;const Name: UnicodeString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  InitFile(F);
{$ifdef FPC_ANSI_TEXTFILEREC}
  FileRec(f).Name:=ToSingleByteFileSystemEncodedFileName(Name);
{$else FPC_ANSI_TEXTFILEREC}
  FileRec(f).Name:=Name;
{$endif FPC_ANSI_TEXTFILEREC}
  { null terminate, since the name array is regularly used as p(wide)char }
  FileRec(f).Name[high(FileRec(f).Name)]:=#0;
End;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}


{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Procedure Assign(out f:File;const Name: RawByteString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  InitFile(F);
{$ifdef FPC_ANSI_TEXTFILEREC}
  { ensure the characters in the record's filename are encoded correctly }
  FileRec(f).Name:=ToSingleByteFileSystemEncodedFileName(Name);
{$else FPC_ANSI_TEXTFILEREC}
  FileRec(f).Name:=Name;
{$endif FPC_ANSI_TEXTFILEREC}
  { null terminate, since the name array is regularly used as p(wide)char }
  FileRec(f).Name[high(FileRec(f).Name)]:=#0;
End;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}

Procedure Assign(out f:File;const Name: ShortString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
  Assign(f,AnsiString(Name));
{$else FPC_HAS_FEATURE_ANSISTRINGS}
  InitFile(f);
  { warning: no encoding support }
  FileRec(f).Name:=Name;
  { null terminate, since the name array is regularly used as p(wide)char }
  FileRec(f).Name[high(FileRec(f).Name)]:=#0;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
End;

Procedure Assign(out f:File;const p: PAnsiChar);
Begin
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
  Assign(f,AnsiString(p));
{$else FPC_HAS_FEATURE_ANSISTRINGS}
  { no use in making this the one that does the work, since the name field is
    limited to 255 characters anyway }
  Assign(f,strpas(p));
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
End;

Procedure Assign(out f:File;const c: AnsiChar);
Begin
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
  Assign(f,AnsiString(c));
{$else FPC_HAS_FEATURE_ANSISTRINGS}
  Assign(f,ShortString(c));
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
End;

Procedure Rewrite(var f:File;l:Longint);[IOCheck];
{
  Create file f with recordsize of l
}
Begin
  If InOutRes <> 0 then
   exit;
  Case FileRec(f).mode Of
   fmInOut,fmInput,fmOutput : Close(f);
   fmClosed : ;
  else
   Begin
     InOutRes:=102;
     exit;
   End;
  End;
  If l=0 Then
   InOutRes:=2
  else
   Begin
     { Reopen with filemode 2, to be Tp compatible (PFV) }
     Do_Open(f,PFileTextRecChar(@FileRec(f).Name),$1002,false);
     FileRec(f).RecSize:=l;
   End;
End;


Procedure Reset(var f:File;l:Longint);[IOCheck];
{
  Open file f with recordsize of l and filemode
}
Begin
  If InOutRes <> 0 then
   Exit;
  Case FileRec(f).mode Of
   fmInOut,fmInput,fmOutput : Close(f);
   fmClosed : ;
  else
   Begin
     InOutRes:=102;
     exit;
   End;
  End;
  If l=0 Then
   InOutRes:=2
  else
   Begin
     Do_Open(f,PFileTextRecChar(@FileRec(f).Name),Filemode,false);
     FileRec(f).RecSize:=l;
   End;
End;


Procedure Rewrite(Var f:File);[IOCheck];
{
  Create file with (default) 128 byte records
}
Begin
  If InOutRes <> 0 then
   exit;
  Rewrite(f,128);
End;


Procedure Reset(Var f:File);[IOCheck];
{
  Open file with (default) 128 byte records
}
Begin
  If InOutRes <> 0 then
   exit;
  Reset(f,128);
End;


Procedure BlockWrite(Var f:File;Const Buf;Count:Int64;var Result:Int64);[IOCheck];
{
  Write Count records from Buf to file f, return written records in result
}
Begin
  Result:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmOutput :
      Result:=Do_Write(FileRec(f).Handle,@Buf,Count*FileRec(f).RecSize)
        div FileRec(f).RecSize;
    fmInPut: inOutRes := 105;
    else InOutRes:=103;
  end;
End;


Procedure BlockWrite(Var f:File;Const Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Write Count records from Buf to file f, return written records in result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=longint(l);
End;


Procedure BlockWrite(Var f:File;Const Buf;Count:Word;var Result:Word);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=word(l);
End;

Procedure BlockWrite(Var f:File;Const Buf;Count:Cardinal;var Result:Cardinal);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=l;
End;

Procedure BlockWrite(Var f:File;Const Buf;Count:Word;var Result:Integer);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=integer(l);
End;

Procedure BlockWrite(Var f:File;Const Buf;Count:Longint);[IOCheck];
{
  Write Count records from Buf to file f, if none a Read and Count>0 then
  InOutRes is set
}
var
  Result : Int64;
Begin
  BlockWrite(f,Buf,Count,Result);
  If (InOutRes=0) and (Result<Count) and (Count>0) Then
   InOutRes:=101;
End;

Procedure BlockRead(var f:File;var Buf;Count:Int64;var Result:Int64);[IOCheck];
{
  Read Count records from file f ro Buf, return number of read records in
  Result
}
Begin
  Result:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput :
      Result:=Do_Read(FileRec(f).Handle,@Buf,count*FileRec(f).RecSize)
        div FileRec(f).RecSize;
    fmOutput: inOutRes := 104;
    else InOutRes:=103;
  end;
End;

Procedure BlockRead(var f:File;var Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Read Count records from file f ro Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=longint(l);
End;

Procedure BlockRead(var f:File;var Buf;count:Word;var Result:Word);[IOCheck];
{
  Read Count records from file f to Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=word(l);
End;

Procedure BlockRead(var f:File;var Buf;count:Cardinal;var Result:Cardinal);[IOCheck];
{
  Read Count records from file f to Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=l;
End;

Procedure BlockRead(var f:File;var Buf;count:Word;var Result:Integer);[IOCheck];
{
  Read Count records from file f to Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=integer(l);
End;

Procedure BlockRead(Var f:File;Var Buf;Count:Int64);[IOCheck];
{
  Read Count records from file f to Buf, if none are read and Count>0 then
  InOutRes is set
}
var
  Result : int64;
Begin
  BlockRead(f,Buf,Count,Result);
  If (InOutRes=0) and (Result<Count) and (Count>0) Then
   InOutRes:=100;
End;


Function FilePos(var f:File):Int64;[IOCheck];
{
  Return current Position In file f in records
}
Begin
  FilePos:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      FilePos:=Do_FilePos(FileRec(f).Handle) div FileRec(f).RecSize;
    else
      InOutRes:=103;
  end;
End;


Function FileSize(var f:File):Int64;[IOCheck];
{
  Return the size of file f in records
}
Begin
  FileSize:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      begin
        if (FileRec(f).RecSize>0) then
          FileSize:=Do_FileSize(FileRec(f).Handle) div FileRec(f).RecSize;
      end;
    else InOutRes:=103;
  end;
End;


Function Eof(var f:File):Boolean;[IOCheck];
{
  Return True if we're at the end of the file f, else False is returned
}
Begin
  Eof:=false;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    {Can't use do_ routines because we need record support}
    fmInOut,fmInput,fmOutput : Eof:=(FileSize(f)<=FilePos(f));
    else InOutRes:=103;
  end;
End;


Procedure Seek(var f:File;Pos:Int64);[IOCheck];
{
  Goto record Pos in file f
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      Do_Seek(FileRec(f).Handle,Pos*FileRec(f).RecSize);
    else InOutRes:=103;
  end;
End;

Procedure Truncate(Var f:File);[IOCheck];
{
  Truncate/Cut file f at the current record Position
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmOutput :
      Do_Truncate(FileRec(f).Handle,FilePos(f)*FileRec(f).RecSize);
    else InOutRes:=103;
  end;
End;


Procedure Close(var f:File);[IOCheck];
{
  Close file f
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      begin
        Do_Close(FileRec(f).Handle);
        FileRec(f).mode:=fmClosed;
      end
    else InOutRes:=103;
  end;
End;


Procedure Erase(var f : File);[IOCheck];
Begin
  if InOutRes<>0 then
    exit;
  if FileRec(f).mode<>fmClosed then
    begin
      InOutRes:=102;
      exit;
    end;
  Do_Erase(PFileTextRecChar(@FileRec(f).Name),false);
End;


Procedure Rename(var f : File; const S : UnicodeString);[IOCheck];
{$ifdef FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
var
  fs: RawByteString;
{$endif FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
Begin
  if InOutRes<>0 then
    exit;
  if FileRec(f).mode<>fmClosed then
    begin
      InOutRes:=102;
      exit;
    end;
{$ifdef FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
  { it's slightly faster to convert the unicodestring here to rawbytestring
    than doing it in do_rename(), because here we still know the length }
  fs:=ToSingleByteFileSystemEncodedFileName(s);
  Do_Rename(PFileTextRecChar(@FileRec(f).Name),PAnsiChar(fs),false,true);
  If InOutRes=0 then
     FileRec(f).Name:=fs
{$else FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
  Do_Rename(PFileTextRecChar(@FileRec(f).Name),PUnicodeChar(S),false,false);
  If InOutRes=0 then
{$ifdef FPC_ANSI_TEXTFILEREC}
    FileRec(f).Name:=ToSingleByteFileSystemEncodedFileName(s);
{$else FPC_ANSI_TEXTFILEREC}
    FileRec(f).Name:=s
{$endif FPC_ANSI_TEXTFILEREC}
{$endif FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
End;


Procedure Rename(var f : File;const s : RawByteString);[IOCheck];
var
{$ifdef FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
  fs: RawByteString;
  pdst: PAnsiChar;
{$else FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
  fs: UnicodeString;
  pdst: PUnicodeChar;
{$endif FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
  dstchangeable: boolean;
Begin
  if InOutRes<>0 then
    exit;
  if FileRec(f).mode<>fmClosed then
    begin
      InOutRes:=102;
      exit;
    end;
{$ifdef FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
  dstchangeable:=false;
  pdst:=PAnsiChar(s);
  if StringCodePage(s)<>DefaultFileSystemCodePage then
    begin
      fs:=ToSingleByteFileSystemEncodedFileName(s);
      pdst:=PAnsiChar(fs);
      dstchangeable:=true;
    end
  else
    fs:=s;
{$else FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
   { it's slightly faster to convert the rawbytestring here to unicodestring
     than doing it in do_rename, because here we still know the length }
   fs:=unicodestring(s);
   pdst:=PUnicodeChar(fs);
   dstchangeable:=true;
{$endif FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
  Do_Rename(PFileTextRecChar(@FileRec(f).Name),pdst,false,dstchangeable);
  If InOutRes=0 then
{$if defined(FPC_ANSI_TEXTFILEREC) and not defined(FPCRTL_FILESYSTEM_SINGLE_BYTE_API)}
    FileRec(f).Name:=ToSingleByteFileSystemEncodedFileName(fs)
{$else FPC_ANSI_TEXTFILEREC and not FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
    FileRec(f).Name:=fs
{$endif FPC_ANSI_TEXTFILEREC and not FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
End;


Procedure Rename(var f : File;const s : ShortString);[IOCheck];
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Begin
  Rename(f,AnsiString(s));
End;
{$else FPC_HAS_FEATURE_ANSISTRINGS}
var
  p : array[0..255] Of Char;
Begin
  Move(s[1],p,Length(s));
  p[Length(s)]:=#0;
  Rename(f,Pchar(@p));
End;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}


Procedure Rename(var f:File;const p : PAnsiChar);[IOCheck];
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Begin
  Rename(f,AnsiString(p));
End;
{$else FPC_HAS_FEATURE_ANSISTRINGS}
var
  len: SizeInt
Begin
  if InOutRes<>0 then
    exit;
  if FileRec(f).mode<>fmClosed then
    begin
      InOutRes:=102;
      exit;
    end;
  Do_Rename(PFileTextRecChar(@FileRec(f).Name),p,false);
  { check error code of do_rename }
  if InOutRes=0 then
    begin
      len:=min(StrLen(p),high(FileRec(f).Name));
      Move(p^,FileRec(f).Name,len);
      FileRec(f).Name[len]:=#0;
    end;
End;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}


Procedure Rename(var f:File;const c : AnsiChar);[IOCheck];
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Begin
  Rename(f,AnsiString(c));
End;
{$else FPC_HAS_FEATURE_ANSISTRINGS}
var
  p : array[0..1] Of AnsiChar;
Begin
  p[0]:=c;
  p[1]:=#0;
  Rename(f,PAnsiChar(@p));
End;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}


{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                    subroutines for typed file handling
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
Procedure Assign(out f:TypedFile;const Name: UnicodeString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  Assign(UnTypedFile(f),Name);
End;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}


{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Procedure Assign(out f:TypedFile;const Name: RawByteString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  Assign(UnTypedFile(f),Name);
End;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}


Procedure Assign(out f:TypedFile;const Name: ShortString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  Assign(UnTypedFile(f),Name);
End;


Procedure Assign(out f:TypedFile;const p:PAnsiChar);
Begin
  Assign(UnTypedFile(f),p);
end;


Procedure Assign(out f:TypedFile;const c:AnsiChar);
Begin
  Assign(UnTypedFile(f),c);
end;

Procedure fpc_reset_typed(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias:'FPC_RESET_TYPED']; compilerproc;
Begin
  Reset(UnTypedFile(f),Size);
End;


Procedure fpc_rewrite_typed(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias:'FPC_REWRITE_TYPED']; compilerproc;
Begin
  Rewrite(UnTypedFile(f),Size);
End;

{$i isotmp.inc}


{$ifdef FPC_HAS_FEATURE_RANDOM}
{ this code is duplicated in the iso7185 unit }
Procedure DoAssign(var t : TypedFile);
Begin
  Assign(t,getTempDir+'fpc_'+HexStr(random(1000000000),8)+'.tmp');
End;
{$else FPC_HAS_FEATURE_RANDOM}
{ this code is duplicated in the iso7185 unit }
Procedure DoAssign(var t : TypedFile);
const
  start : dword = 0;
Begin
{$ifdef EXCLUDE_COMPLEX_PROCS}
  runerror(219);
{$else EXCLUDE_COMPLEX_PROCS}
  Assign(t,getTempDir+'fpc_'+HexStr(start,8)+'.tmp');
  inc(start);
{$endif EXCLUDE_COMPLEX_PROCS}
End;
{$endif FPC_HAS_FEATURE_RANDOM}


Procedure fpc_reset_typed_iso(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias:'FPC_RESET_TYPED_ISO']; compilerproc;
Begin
  If InOutRes <> 0 then
   exit;

  { create file name? }
  if FileRec(f).mode=0 then
    DoAssign(f);

  { use _private[1] to track eof }
  FileRec(f)._private[1]:=0;

  Reset(UnTypedFile(f),Size);
  BlockRead(UntypedFile(f),(pbyte(@f)+sizeof(FileRec))^,1);
End;


Procedure fpc_rewrite_typed_iso(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias:'FPC_REWRITE_TYPED_ISO']; compilerproc;
Begin
  If InOutRes <> 0 then
   exit;

  { create file name? }
  if FileRec(f).mode=0 then
    DoAssign(f);

  Rewrite(UnTypedFile(f),Size);
End;


Procedure fpc_reset_typed_name_iso(var f : TypedFile;const FileName : String;Size : Longint);[Public,IOCheck, Alias:'FPC_RESET_TYPED_NAME_ISO']; compilerproc;
Begin
  If InOutRes <> 0 then
   exit;

  { create file name? }
  if FileRec(f).mode=0 then
    Assign(f,FileName);

  { use _private[1] to track eof }
  FileRec(f)._private[1]:=0;

  Reset(UnTypedFile(f),Size);
  BlockRead(UntypedFile(f),(pbyte(@f)+sizeof(FileRec))^,1);
End;


Procedure fpc_rewrite_typed_name_iso(var f : TypedFile;const FileName : String;Size : Longint);[Public,IOCheck, Alias:'FPC_REWRITE_TYPED_NAME_ISO']; compilerproc;
Begin
  If InOutRes <> 0 then
   exit;

  { create file name? }
  if FileRec(f).mode=0 then
    Assign(f,FileName);

  Rewrite(UnTypedFile(f),Size);
End;


Procedure fpc_typed_write(TypeSize : Longint;var f : TypedFile;const Buf);[IOCheck, Public, Alias :'FPC_TYPED_WRITE']; compilerproc;
Begin
  If InOutRes <> 0 then
   exit;
  case fileRec(f).mode of
    fmOutPut,fmInOut:
      Do_Write(FileRec(f).Handle,@Buf,TypeSize);
    fmInput: inOutRes := 105;
    else inOutRes := 103;
  end;
End;

Procedure fpc_typed_read(TypeSize : Longint;var f : TypedFile;out Buf);[IOCheck, Public, Alias :'FPC_TYPED_READ']; compilerproc;
var
  Result : Longint;
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).mode of
    fmInput,fmInOut:
      begin
        Result:=Do_Read(FileRec(f).Handle,@Buf,TypeSize);
        If Result<TypeSize Then
         InOutRes:=100
      end;
    fmOutPut: inOutRes := 104
    else inOutRes := 103;
  end;
End;


Procedure fpc_typed_read_iso(TypeSize : Longint;var f : TypedFile;out Buf);[IOCheck, Public, Alias :'FPC_TYPED_READ_ISO']; compilerproc;
Begin
  move((pbyte(@f)+sizeof(TypedFile))^,Buf,TypeSize);
  if not(eof(f)) then
    BlockRead(f,(pbyte(@f)+sizeof(FileRec))^,1)
  else
    FileRec(f)._private[1]:=1;
End;


function fpc_GetBuf_TypedFile(var f : TypedFile) : pointer; [IOCheck]; compilerproc;
Begin
  Result:=pbyte(@f)+sizeof(TypedFile);
end;


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

Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:TypedFile;const Name:RtlString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  FillChar(f,SizeOF(FileRec),0);
  FileRec(f).Handle:=UnusedHandle;
  FileRec(f).mode:=fmClosed;
  Move(Name[1],FileRec(f).Name,Length(Name)*SizeOf(RtlChar));
End;


Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:TypedFile;p:PRtlChar);
{
  Assign Name to file f so it can be used with the file routines
}
begin
  Assign(f,RtlString(p));
end;


Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:TypedFile;c:RtlChar);
{
  Assign Name to file f so it can be used with the file routines
}
begin
  Assign(f,RtlString(c));
end;


Procedure fpc_reset_typed(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias:'FPC_RESET_TYPED']; compilerproc;
Begin
  If InOutRes <> 0 then
   exit;
  Reset(UnTypedFile(f),Size);
End;


Procedure fpc_rewrite_typed(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias:'FPC_REWRITE_TYPED']; compilerproc;
Begin
  If InOutRes <> 0 then
   exit;
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


{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                    subroutines for typed file handling
****************************************************************************}

Procedure assign(var f:TypedFile;const Name:string);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  FillChar(f,SizeOF(FileRec),0);
  FileRec(f).Handle:=UnusedHandle;
  FileRec(f).mode:=fmClosed;
  Move(Name[1],FileRec(f).Name,Length(Name));
End;


Procedure assign(var f:TypedFile;p:pchar);
{
  Assign Name to file f so it can be used with the file routines
}
begin
  Assign(f,StrPas(p));
end;


Procedure assign(var f:TypedFile;c:char);
{
  Assign Name to file f so it can be used with the file routines
}
begin
  Assign(f,string(c));
end;


Procedure Int_Typed_Reset(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias: {$ifdef FPCNAMES}'FPC_RESET_TYPED'{$else}'RESET_TYPED'{$endif}];
Begin
  If InOutRes <> 0 then
   exit;
  Reset(UnTypedFile(f),Size);
End;


Procedure Int_Typed_Rewrite(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias: {$ifdef FPCNAMES}'FPC_REWRITE_TYPED'{$else}'REWRITE_TYPED'{$endif}];
Begin
  If InOutRes <> 0 then
   exit;
  Rewrite(UnTypedFile(f),Size);
End;


Procedure Int_Typed_Write(TypeSize : Longint;var f : TypedFile;var Buf);[IOCheck, Public, Alias : {$ifdef FPCNAMES}'FPC_TYPED_WRITE'{$else}'TYPED_WRITE'{$endif}];
Begin
  If InOutRes <> 0 then
   exit;
  Do_Write(FileRec(f).Handle,Longint(@Buf),TypeSize);
End;


Procedure Int_Typed_Read(TypeSize : Longint;var f : TypedFile;var Buf);[IOCheck, Public, Alias : {$ifdef FPCNAMES}'FPC_TYPED_READ'{$else}'TYPED_READ'{$endif}];
var
  Result : Longint;
Begin
  If InOutRes <> 0 then
   exit;
  Result:=Do_Read(FileRec(f).Handle,Longint(@Buf),TypeSize);
  If Result<TypeSize Then
   InOutRes:=100;
End;

{
  $Log$
  Revision 1.5  1998-09-14 10:48:26  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.4  1998/07/02 12:16:28  carl
    * IoCheck routines now check for InOutRes before executing, just like TP

  Revision 1.3  1998/05/21 19:31:02  peter
    * objects compiles for linux
    + assign(pchar), assign(char), rename(pchar), rename(char)
    * fixed read_text_as_array
    + read_text_as_pchar which was not yet in the rtl

  Revision 1.2  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}

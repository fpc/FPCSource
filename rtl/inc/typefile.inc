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


Procedure Int_Typed_Reset(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias: 'RESET_TYPED'];
Begin
  Reset(UnTypedFile(f),Size);
End;


Procedure Int_Typed_Rewrite(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias: 'REWRITE_TYPED'];
Begin
  Rewrite(UnTypedFile(f),Size);
End;


Procedure Int_Typed_Write(TypeSize : Longint;var f : TypedFile;var Buf);[IOCheck, Public, Alias : 'TYPED_WRITE'];
Begin
  Do_Write(FileRec(f).Handle,Longint(@Buf),TypeSize);
End;


Procedure Int_Typed_Read(TypeSize : Longint;var f : TypedFile;var Buf);[IOCheck, Public, Alias : 'TYPED_READ'];
var
  Result : Longint;
Begin
  Result:=Do_Read(FileRec(f).Handle,Longint(@Buf),TypeSize);
  If Result<TypeSize Then
   InOutRes:=100;
End;

{
  $Log$
  Revision 1.3  1998-05-21 19:31:02  peter
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

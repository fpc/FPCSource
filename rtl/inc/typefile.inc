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

{$IFDEF TypedFile}

Procedure assign(var f:TypedFile;const Name:string);
Begin
  FillChar(f,SizeOF(FileRec),0);
  FileRec(f).Handle:=UnusedHandle;
  FileRec(f).mode:=fmClosed;
  Move(Name[1],FileRec(f).Name,Length(Name));
End;

{$IFDEF VER_ABOVE0_9_5}

Procedure Intern_Reset(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias: 'RESET_TYPED'];
Begin
  Reset(UnTypedFile(f),Size);
End;

Procedure Intern_Rewrite(var f : TypedFile;Size : Longint);[Public,IOCheck, Alias: 'REWRITE_TYPED'];
Begin
  Rewrite(UnTypedFile(f),Size);
End;

{$ELSE not VER_ABOVE0_9_5}

Procedure Rewrite(var f : TypedFile);[IOCheck];
Begin
  Rewrite(UnTypedFile(f),128);
End;

Procedure Reset(var f : TypedFile);[IOCheck];
Begin
  Reset(UnTypedFile(f),128);
End;

{$ENDIF VER_ABOVE0_9_5}


Procedure TypedWrite(TypeSize : Longint;var f : TypedFile;var Buf);[IOCheck, Public, Alias : 'TYPED_WRITE'];
Begin
  Do_Write(FileRec(f).Handle,Longint(@Buf),TypeSize);
End;


Procedure TypedRead(TypeSize : Longint;var f : TypedFile;var Buf);[IOCheck, Public, Alias : 'TYPED_READ'];
var
  Result : Longint;
Begin
  Result:=Do_Read(FileRec(f).Handle,Longint(@Buf),TypeSize);
  If Result<TypeSize Then
   InOutRes:=100;
End;

{$ENDIF TypedFile }

{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.3  1998/01/26 12:00:33  michael
  + Added log at the end


  
  Working file: rtl/inc/typefile.inc
  description:
  ----------------------------
  revision 1.2
  date: 1998/01/25 21:53:32;  author: peter;  state: Exp;  lines: +2 -2
    + Universal Handles support for StdIn/StdOut/StdErr
    * Updated layout of sysamiga.pas
  ----------------------------
  revision 1.1
  date: 1998/01/11 02:43:11;  author: michael;  state: Exp;
  + Initial implementation of these files (by Peter Vreman).
    file operations are now in separate files per type of file.
  =============================================================================
}

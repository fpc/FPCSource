{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    File utility calls

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Begin
  //!! Needs implementing
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  //!! Needs implementing
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  //!! Needs implementing
end;


Function FileWrite (Handle : Longint; Var Buffer; Count : Longint) : Longint;

begin
  //!! Needs implementing
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  //!! Needs implementing
end;


Procedure FileClose (Handle : Longint);

begin
  //!! Needs implementing
end;


Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  //!! Needs implementing
end;


Function FileAge (Const FileName : String): Longint;

begin
  //!! Needs implementing
end;


Function FileExists (Const FileName : String) : Boolean;

begin
  //!! Needs implementing
end;


Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

begin
  //!! Needs implementing
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  //!! Needs implementing
end;


Procedure FindClose (Var F : TSearchrec);

begin
  //!! Needs implementing
end;


Function FileGetDate (Handle : Longint) : Longint;

begin
  //!! Needs implementing
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  //!! Needs implementing
end;


Function FileGetAttr (Const FileName : String) : Longint;

begin
  //!! Needs implementing
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  //!! Needs implementing
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  //!! Needs implementing
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  //!! Needs implementing
end;


Function FileSearch (Const Name, DirList : String) : String;

begin
  //!! Needs implementing
end;


Procedure GetLocalTime(var SystemTime: TSystemTime);

begin
  //!! Needs implementing
end ;

Procedure InitAnsi;
begin
  //!! Needs implementing
end;

Procedure InitInternational;
begin
  InitAnsi;
end;


{
  $Log$
  Revision 1.1  2000-07-13 06:31:15  michael
  + Initial import

  Revision 1.3  2000/06/04 14:19:34  hajny
    * parameter name change in FileSeek

  Revision 1.2  2000/01/07 16:41:51  daniel
    * copyright 2000

  Revision 1.1  1999/11/08 22:45:01  peter
    * inserted as templates

  Revision 1.2  1998/10/12 08:35:22  pierre
   * wrong file commited by Michael corrected

  Revision 1.1  1998/10/11 12:21:01  michael
  Added file calls. Implemented for linux only

}

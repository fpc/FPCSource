{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Disk functions from Delphi's sysutils.pas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function DiskFree (Drive : Byte) : Longint;

begin
end;


Function DiskSize (Drive : Byte) : Longint;

begin
end;


Function GetCurrentDir : String;

begin
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;

begin
end;


Function CreateDir (Const NewDir : String) : Boolean;

begin
end;


Function RemoveDir (Const Dir : String) : Boolean;

begin
end;



{
 $Log$
 Revision 1.1  2000-07-13 06:31:15  michael
 + Initial import

 Revision 1.2  2000/01/07 16:41:51  daniel
   * copyright 2000

 Revision 1.1  1999/11/08 22:45:01  peter
   * inserted as templates

 Revision 1.1  1998/10/11 13:42:55  michael
 Added disk functions

}
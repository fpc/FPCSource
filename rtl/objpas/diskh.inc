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
Function DiskSize (Drive : Byte) : Longint;
Function GetCurrentDir : String;
Function SetCurrentDir (Const NewDir : String) : Boolean;
Function CreateDir (Const NewDir : String) : Boolean;
Function RemoveDir (Const Dir : String) : Boolean;

{
 $Log$
 Revision 1.4  2000-02-09 16:59:32  peter
   * truncated log

 Revision 1.3  2000/01/07 16:41:43  daniel
   * copyright 2000

 Revision 1.2  1999/11/06 14:41:30  peter
   * truncated log

}

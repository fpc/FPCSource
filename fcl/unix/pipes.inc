{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Linux specific part of pipe stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses linux;

Function CreatePipeHandles (Var Inhandle,OutHandle : Longint) : Boolean;

begin
  Result := AssignPipe (Inhandle,OutHandle);
end;

{
  $Log$
  Revision 1.1.2.1  2000-10-25 12:36:24  marco
   * Split up Freebsd/Unix/Linux for Fixes FCL

  Revision 1.1  2000/07/13 06:31:32  michael
  + Initial import

  Revision 1.5  2000/02/15 21:57:51  sg
  * Added copyright notice and CVS log tags where necessary

}

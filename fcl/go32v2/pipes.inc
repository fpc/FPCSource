{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    DOS/go32v2 specific part of pipe stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

// No pipes under dos, sorry...

Function CreatePipeHandles (Var Inhandle,OutHandle : Longint) : Boolean;

begin
  Result := False;
end;
{
  $Log$
  Revision 1.3  2000-07-22 12:27:48  jonas
    * added missing "open comment braces" for the log section

  Revision 1.2  2000/07/13 11:32:58  michael
  + removed logs

}

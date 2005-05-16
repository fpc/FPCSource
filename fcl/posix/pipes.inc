{
    $Id: pipes.inc,v 1.3 2005/02/14 17:13:17 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    POSIX specific part of pipe stream.

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
  $Log: pipes.inc,v $
  Revision 1.3  2005/02/14 17:13:17  peter
    * truncate log

}

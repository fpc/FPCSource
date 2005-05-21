{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Linux specific part of pipe stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Uses
{$ifdef ver1_0}
  Linux
{$else}
  Unix
{$endif}
  ;

Function CreatePipeHandles (Var Inhandle,OutHandle : Longint) : Boolean;

begin
{$ifdef ver1_0}
  Result := AssignPipe (Inhandle,OutHandle);
{$else}
  Result := (AssignPipe (Inhandle,OutHandle)<>-1);
{$endif}
end;

{
  $Log: pipes.inc,v $
  Revision 1.8  2005/02/14 17:13:18  peter
    * truncate log

}

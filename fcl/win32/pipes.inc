{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Michael Van Canneyt

    Win32 part of pipe stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses windows;

Const piInheritablePipe : TSecurityAttributes = (
                           nlength:SizeOF(TSecurityAttributes);
                           lpSecurityDescriptor:Nil;
                           Binherithandle:True);
      piNonInheritablePipe : TSecurityAttributes = (
                             nlength:SizeOF(TSecurityAttributes);
                             lpSecurityDescriptor:Nil;
                             Binherithandle:False);


      PipeBufSize = 1024;


Function CreatePipeHandles (Var Inhandle,OutHandle : Longint) : Boolean;

begin
  Result := CreatePipe (@Inhandle,@OutHandle,@piInheritablePipe,PipeBufSize);
end;
{
  $Log$
  Revision 1.3  2000-07-25 11:27:35  jonas
    * fixed missing comment openers for log section

  Revision 1.2  2000/07/13 11:33:07  michael
  + removed logs
 
}

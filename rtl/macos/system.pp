{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Olle Raab

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

interface

{$Y-}
type
   integer = -32768 .. 32767;
   byte =0..255;
   shortint=-128..127;
   word=0..65535;
   longint=+(-$7FFFFFFF-1)..$7FFFFFFF;
   pchar=^char;

implementation

procedure do_exit;[public,alias:'FPC_DO_EXIT'];

begin
end;

procedure fpc_initializeunits;[public,alias:'FPC_INITIALIZEUNITS'];

begin
end;

procedure _restf14;[public,alias:'_restf14'];

begin
end;

procedure _savef14;[public,alias:'_savef14'];

begin
end;

end.
{
  $Log$
  Revision 1.1  2002-10-02 21:34:31  florian
    * first dummy implementation
}
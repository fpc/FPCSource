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

{ This is a hack to make it work until powerpc.inc is fixed.

  This function is never called directly, it's a dummy to hold the register save/
  load subroutines
}
procedure saverestorereg;
assembler;
asm
	export ._restf14[PR]
	csect  ._restf14[PR]
._restf14:
                mtlr    r0
                blr
	export ._savef14[PR]
	csect  ._savef14[PR]
._savef14:
                blr
end;

end.
{
  $Log$
  Revision 1.2  2002-10-10 19:44:05  florian
    * changes from Olle to compile/link a simple program

  Revision 1.1  2002/10/02 21:34:31  florian
    * first dummy implementation
}
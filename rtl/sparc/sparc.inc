{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 2002-2004 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    Sparc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                           PowerPC specific stuff
****************************************************************************}
procedure fpc_cpuinit;
  begin
  end;


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
	mov %sp,%o0
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;nostackframe;
  asm
	{ framebp = %o0 }
	{ flush register windows, so they are stored in the stack }
        ta      3
	ld [%o0+60],%o0
	{ add 8 to skip jmpl and delay slot }
	add %o0,8,%o0
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;nostackframe;
  asm
	{ flush register windows, so they are stored in the stack }
        ta      3
	{ framebp = %o0 }
	ld [%o0+56],%o0
  end;


{$define FPC_SYSTEM_HAS_SPTR}
function Sptr:Pointer;assembler;nostackframe;
  asm
	mov %sp,%o0
  end;


{
  $Log$
  Revision 1.7  2004-05-30 20:03:05  florian
    * ?

  Revision 1.6  2004/05/27 23:34:37  peter
    * backtrace support

  Revision 1.5  2004/01/02 17:22:14  jonas
    + fpc_cpuinit procedure to allow cpu/fpu initialisation before any unit
      initialises
    + fpu exceptions for invalid operations and division by zero enabled for
      ppc

  Revision 1.4  2003/12/04 21:42:07  peter
    * register calling updates

  Revision 1.3  2003/03/17 14:30:11  peter
    * changed address parameter/return values to pointer instead
      of longint

  Revision 1.2  2003/02/05 21:48:34  mazen
  * fixing run time errors related to unimplemented abstract methods in CG
  + giving empty emplementations for some RTL functions

  Revision 1.1  2002/11/16 20:10:31  florian
    + sparc specific rtl skeleton added
}

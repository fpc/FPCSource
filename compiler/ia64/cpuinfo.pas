{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Basic Processor information

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
Unit CPUInfo;

{$i fpcdefs.inc}

Interface

Type
   AWord = QWord;

   { this must be an ordinal type with the same size as a pointer }
   { to allow some dirty type casts for example when using        }
   { tconstsym.value                                              }
   TPointerOrd = longint;

   bestreal = extended;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   { on the ia64 comp will be mapped to int64 }
   ts64comp = comp;

   pbestreal=^bestreal;


Const
   { Size of native extended type }
   extended_size = 10;

   c_countusableregsint = 95;
   c_countusableregsfpu = 95;
   c_countusableregsmm  = 0;
   c_countusableregsqp  = 48;

   { target cpu string (used by compiler options) }
   target_cpu_string = 'ia64';

Implementation

end.
{
  $Log$
  Revision 1.7  2002-09-07 15:25:11  peter
    * old logs removed and tabs fixed

  Revision 1.6  2002/08/12 15:08:42  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.5  2002/08/10 14:48:09  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.4  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.3  2002/04/07 13:42:40  carl
  - moved type constant

}

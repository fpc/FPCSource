{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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
Unit cpuinfo;

{$i fpcdefs.inc}

Interface

Type
   {# Natural integer register type and size for the target machine }
   AWord = Cardinal;
   PAWord = ^AWord;

   { this must be an ordinal type with the same size as a pointer }
   { Note: must be unsigned!! Otherwise, ugly code like           }
   { pointer(-1) will result in a pointer with the value          }
   { $fffffffffffffff on a 32bit machine if the compiler uses     }
   { int64 constants internally (JM)                              }
   TConstPtrUInt = cardinal;
   
   bestreal = extended;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts64comp = extended;
   
   pbestreal=^bestreal;
   
   { possible supported processors for this target }
   tprocessors = 
      (no_processor,
       Class386,
       ClassP5,
       ClassP6
      ); 
   

Const
   {# Size of native extended floating point type }
   extended_size = 10;
   {# Size of a pointer                           }
   pointer_size  = 4;
   {# Size of a multimedia register               }
   mmreg_size = 8;

   { target cpu string (used by compiler options) }
   target_cpu_string = 'i386';
   

Implementation

end.
{
  $Log$
  Revision 1.12  2002-08-12 15:08:41  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.11  2002/08/10 14:47:50  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.10  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.9  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.7  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.6  2002/04/07 13:41:50  carl
  - moved type constant

  Revision 1.5  2002/04/02 17:11:34  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}

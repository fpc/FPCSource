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

  uses
    globtype;

Type
   { Natural integer register type and size for the target machine }
   AWord = longword;
   PAWord = ^AWord;

   { This must be an ordinal type with the same size as a pointer
     Note: Must be unsigned! Otherwise, ugly code like
     pointer(-1) will result in a pointer with the value
     $fffffffffffffff on a 32bit machine if the compiler uses
     int64 constants internally (JM)                              }
   TConstPtrUInt = longword;

   bestreal = extended;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = type extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tprocessors =
      (no_processor,
       Class386,
       ClassPentium,
       ClassPentium2,
       ClassPentium3,
       ClassPentium4
      );

   tfputype =
     (no_fpuprocessor,
      fpu_soft,
      fpu_x87,
      fpu_sse,
      fpu_sse2,
      fpu_sse3
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
   { size of the buffer used for setjump/longjmp
     the size of this buffer is deduced from the
     jmp_buf structure in setjumph.inc file
   }
   jmp_buf_size = 24;

   { calling conventions supported by the code generator }
   supported_calling_conventions = [
     pocall_internproc,
     pocall_compilerproc,
     pocall_inline,
     pocall_register,
     pocall_safecall,
     pocall_stdcall,
     pocall_cdecl,
     pocall_cppdecl,
     pocall_far16,
     pocall_pascal,
     pocall_oldfpccall
   ];

   processorsstr : array[tprocessors] of string[10] = ('',
     '386',
     'PENTIUM',
     'PENTIUM2',
     'PENTIUM3',
     'PENTIUM4'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SOFT',
     'X87',
     'SSE',
     'SSE2',
     'SSE3'
   );

   sse_singlescalar : set of tfputype = [fpu_sse,fpu_sse2,fpu_sse3];
   sse_doublescalar : set of tfputype = [];

Implementation

end.
{
  $Log$
  Revision 1.21  2003-12-25 01:07:09  florian
    + $fputype directive support
    + single data type operations with sse unit
    * fixed more x86-64 stuff

  Revision 1.20  2003/12/01 18:43:31  peter
    * s128real type is not compatible with s80real

  Revision 1.19  2003/11/12 16:05:39  florian
    * assembler readers OOPed
    + typed currency constants
    + typed 128 bit float constants if the CPU supports it

  Revision 1.18  2003/11/07 15:58:32  florian
    * Florian's culmutative nr. 1; contains:
      - invalid calling conventions for a certain cpu are rejected
      - arm softfloat calling conventions
      - -Sp for cpu dependend code generation
      - several arm fixes
      - remaining code for value open array paras on heap

  Revision 1.17  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.16  2002/12/05 14:18:09  florian
    * two comments fixed

  Revision 1.15  2002/09/07 20:48:43  carl
    * cardinal -> longword

  Revision 1.14  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.13  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.12  2002/08/12 15:08:41  carl
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

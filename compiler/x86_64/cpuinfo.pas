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
Unit cpuinfo;

{$i fpcdefs.inc}

Interface

  uses
    globtype;

Type
   bestreal = extended;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = type extended;

   pbestreal=^bestreal;

   tprocessors =
      (no_processor,
       ClassAthlon64
      );

   tfputype =
     (no_fpuprocessor,
      fpu_sse64
     );

Const
   { Size of native extended type }
   extended_size = 10;
   { Size of a multimedia register }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'x86_64';
   { size of the buffer used for setjump/longjmp
     the size of this buffer is deduced from the
     jmp_buf structure in setjumph.inc file
   }
   jmp_buf_size = 64;


   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_compilerproc,
     pocall_inline,
     pocall_register,
     pocall_safecall,
     pocall_stdcall,
     pocall_cdecl,
     pocall_cppdecl
   ];

   processorsstr : array[tprocessors] of string[10] = ('',
     'ATHLON64'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SSE64'
   );

   sse_singlescalar : set of tfputype = [fpu_sse64];
   sse_doublescalar : set of tfputype = [fpu_sse64];

Implementation

end.
{
  $Log$
  Revision 1.13  2004-06-16 20:07:11  florian
    * dwarf branch merged

  Revision 1.12  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.11  2004/04/12 18:25:26  florian
    + AInt added

  Revision 1.10.2.4  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.10.2.3  2004/04/26 21:00:37  peter
    * AInt fixed, PAInt added

  Revision 1.10.2.2  2004/04/24 20:13:24  florian
    * fixed x86-64 exception handling

  Revision 1.10.2.1  2004/04/08 18:33:22  peter
    * rewrite of TAsmSection

  Revision 1.10  2003/12/25 01:07:09  florian
    + $fputype directive support
    + single data type operations with sse unit
    * fixed more x86-64 stuff

  Revision 1.9  2003/12/22 19:00:17  florian
    * fixed some x86-64 issues

  Revision 1.8  2003/12/20 12:38:51  florian
    * some x86-64 compilation fixe

  Revision 1.7  2003/09/24 17:12:02  florian
    * several fixes for new reg allocator

  Revision 1.6  2003/01/05 13:36:54  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.5  2002/09/07 15:25:15  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/08/12 15:08:45  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.3  2002/08/10 14:53:38  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.2  2002/07/25 22:55:34  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

}

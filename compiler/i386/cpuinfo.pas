{
    $Id$
    Copyright (c) 1998-2004 by Florian Klaempfl

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
      fpu_sse2
     );


Const
   {# Size of native extended floating point type }
   extended_size = 10;
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
   supported_calling_conventions : tproccalloptions = [
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
     'SSE2'
   );

   sse_singlescalar : set of tfputype = [fpu_sse,fpu_sse2];
   sse_doublescalar : set of tfputype = [fpu_sse2];

Implementation

end.
{
  $Log$
  Revision 1.26  2004-06-20 08:55:31  florian
    * logs truncated

  Revision 1.25  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.24  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.23.2.2  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.23.2.1  2004/04/26 21:00:37  peter
    * AInt fixed, PAInt added

  Revision 1.23  2004/02/27 10:21:05  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

}

{
    $Id$
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

Interface

  uses
    globtype;

Type
   bestreal = double;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = extended;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tprocessors =
      (no_processor,
       ppc601,
       ppc604
      );

   tfputype =
     (no_fpuprocessor,
      fpu_soft,
      fpu_standard
     );


Const
   {# Size of native extended floating point type }
   extended_size = 8;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'powerpc';
   { size of the buffer used for setjump/longjmp
     the size of this buffer is deduced from the
     jmp_buf structure in setjumph.inc file
   }
   { for linux: }
   jmp_buf_size = 232;

   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_compilerproc,
     pocall_inline,
     pocall_stdcall,
     { the difference to stdcall is only the name mangling }
     pocall_cdecl,
     { the difference to stdcall is only the name mangling }
     pocall_cppdecl
   ];

   processorsstr : array[tprocessors] of string[10] = ('',
     '603',
     '604'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SOFT',
     'STANDARD'
   );

Implementation

end.
{
  $Log$
  Revision 1.20  2004-06-20 08:55:32  florian
    * logs truncated

  Revision 1.19  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.17.2.1  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.17  2004/02/27 10:21:05  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

}

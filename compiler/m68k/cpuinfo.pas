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
   bestreal = real;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tprocessors =
      (no_processor,
       MC68000,
       MC68020,
       Coldfire
      );

   tfputype =
     (no_fpuprocessor,
      fpu_soft,
      fpu_libgcc,
      fpu_68881
     );

Const
   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_compilerproc,
     pocall_inline,
     pocall_stdcall,
     { the difference to stdcall is only the name mangling }
     pocall_cdecl,
     { the difference to stdcall is only the name mangling }
     pocall_cppdecl,
     { this used by the PalmOS port only }
     pocall_syscall
   ];

   processorsstr : array[tprocessors] of string[5] = ('',
     '68000',
     '68020',
     'COLDFIRE'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SOFT',
     'LIBGCC',
     '68881'
   );

Implementation

end.
{
  $Log$
  Revision 1.14  2004-11-09 22:32:59  peter
    * small m68k updates to bring it up2date
    * give better error for external local variable

  Revision 1.13  2004/06/20 08:55:31  florian
    * logs truncated

  Revision 1.12  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.11  2004/05/01 23:29:01  florian
    * continued to fix m68k compiler compilation

  Revision 1.10  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.9  2004/04/18 21:13:59  florian
    * more adaptions for m68k

  Revision 1.8.2.1  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

}

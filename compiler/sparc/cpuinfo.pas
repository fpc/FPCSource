{******************************************************************************
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

 ****************************************************************************}
unit cpuinfo;
{$INCLUDE fpcdefs.inc}

interface
uses
  globtype;
type
  bestreal = double;
  ts32real = single;
  ts64real = double;
  ts80real = extended;
  ts128real = type extended;
  ts64comp = type extended;
  pbestreal=^bestreal;

  { possible supported processors for this target }
  tprocessors=(no_processor,SPARC_V7,SPARC_V8,SPARC_V9);

  tfputype =(no_fpu,fpu_soft,fpu_hard);


const
{# Size of native extended floating point type }
{SPARC architecture uses IEEE double floating point numbers}
  extended_size = 8;
{# Size of a multimedia register               }
  mmreg_size = 8;
{ target cpu string (used by compiler options) }
  target_cpu_string = 'sparc';
{ size of the buffer used for setjump/longjmp
  the size of this buffer is deduced from the
  jmp_buf structure in setjumph.inc file }
  JMP_BUF_SIZE = 12;

  { calling conventions supported by the code generator }
  supported_calling_conventions : tproccalloptions = [
    pocall_internproc,
    pocall_compilerproc,
    pocall_inline,
    pocall_stdcall,
    pocall_cdecl,
    pocall_cppdecl
  ];

   processorsstr : array[tprocessors] of string[10] = ('',
     'SPARC V7',
     'SPARC V8',
     'SPARC V9'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SOFT',
     'HARD'
   );
implementation

end.
{
  $Log$
  Revision 1.18  2004-06-20 08:55:32  florian
    * logs truncated

  Revision 1.17  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.16  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.15.2.2  2004/05/30 21:42:13  peter
    * fix JMP_BUF_SIZE

  Revision 1.15.2.1  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.15  2004/03/12 08:18:11  mazen
  - revert '../' from include path

}

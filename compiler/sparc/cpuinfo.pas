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
{# Natural integer register type and size for the target machine }
  AWord=Longword;
  PAWord=^AWord;
{ the ordinal type used when evaluating constant integer expressions }
  TConstExprInt=int64;
{ this must be an ordinal type with the same size as a pointer }
{ Note: must be unsigned!! Otherwise, ugly code like           }
  TConstPtrUInt=cardinal;
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
{# Size of a pointer                           }
  pointer_size  = 4;
{# Size of a multimedia register               }
  mmreg_size = 8;
{ target cpu string (used by compiler options) }
  target_cpu_string = 'sparc';
{ size of the buffer used for setjump/longjmp
  the size of this buffer is deduced from the
  jmp_buf structure in setjumph.inc file }
{$warning jmp_buf_size not set!}
  JMP_BUF_SIZE = 4; // 4 is used temporary to remove AllocTemp warning

  { calling conventions supported by the code generator }
  supported_calling_conventions = [
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
  Revision 1.12  2003-12-01 18:43:32  peter
    * s128real type is not compatible with s80real

  Revision 1.11  2003/11/28 13:09:07  mazen
  + defintion of ts128real
  + globtype is needed in interface uses clause

  Revision 1.10  2003/11/07 15:58:33  florian
    * Florian's culmutative nr. 1; contains:
      - invalid calling conventions for a certain cpu are rejected
      - arm softfloat calling conventions
      - -Sp for cpu dependend code generation
      - several arm fixes
      - remaining code for value open array paras on heap

  Revision 1.9  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.8  2003/06/17 16:35:42  peter
    * JMP_BUF_SIZE changed to 4 to remove Alloctemp warnings

  Revision 1.7  2003/05/23 22:33:48  florian
    * fix some small flaws which prevent sparc linux system unit from compiling
    * some reformatting done

  Revision 1.6  2002/11/16 20:07:57  florian
    * made target_cpu_name lowercase

  Revision 1.5  2002/10/16 12:36:54  mazen
  * patch of Carl Eric added

}

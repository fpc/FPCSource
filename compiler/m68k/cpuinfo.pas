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

Type
   { Architecture word - Native unsigned type }
   aword  = longword;
   PAWord = ^AWord;

   { this must be an ordinal type with the same size as a pointer }
   { to allow some dirty type casts for example when using        }
   { tconstsym.value                                              }
   { Note: must be unsigned!! Otherwise, ugly code like           }
   { pointer(-1) will result in a pointer with the value          }
   { $fffffffffffffff on a 32bit machine if the compiler uses     }
   { int64 constants internally (JM)                              }
   TConstPtrUInt = longword;

   bestreal = real;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts64comp = extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tprocessors =
      (no_processor,
       MC68000,
       MC68020,
       Coldfire
      );

Const
   {# Size of native extended floating point type }
   extended_size = 8;
   {# Size of a pointer                           }
   pointer_size  = 4;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { size of the buffer used for setjump/longjmp
     the size of this buffer is deduced from the
     jmp_buf structure in setjumph.inc file
   }
   jmp_buf_size = 28;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'm68k';

Implementation

end.
{
  $Log$
  Revision 1.6  2002-12-14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.5  2002/09/07 20:53:28  carl
    * cardinal -> longword

  Revision 1.4  2002/09/07 15:25:13  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.2  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.1  2002/08/11 08:06:09  carl
    + try to commit this ** file again

  Revision 1.7  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.6  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.4  2002/05/13 19:52:46  peter
    * a ppcppc can be build again

  Revision 1.3  2002/04/07 13:43:11  carl
  - moved type constant

}

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
   AWord  = Cardinal;
   PAWord = ^AWord;

   { this must be an ordinal type with the same size as a pointer }
   { to allow some dirty type casts for example when using        }
   { tconstsym.value                                              }
   { Note: must be unsigned!! Otherwise, ugly code like           }
   { pointer(-1) will result in a pointer with the value          }
   { $fffffffffffffff on a 32bit machine if the compiler uses     }
   { int64 constants internally (JM)                              }
   TConstPtrUInt = Cardinal;

Const
   {# Size of native extended floating point type }
   extended_size = 8;
   {# Size of a pointer                           }
   pointer_size  = 4;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'powerpc';

Implementation

end.
{
  $Log$
  Revision 1.8  2002-08-10 14:52:52  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

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

{
    $Id$
    Copyright (c) 1998-2000 by the Free Pascal development team

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

Implementation

end.
{
  $Log$
  Revision 1.4  2002-05-13 19:52:46  peter
    * a ppcppc can be build again

  Revision 1.3  2002/04/07 13:43:11  carl
  - moved type constant

  Revision 1.2  2001/12/29 15:28:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.1  2001/08/26 13:31:04  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.2  2001/08/26 13:29:34  florian
    * some cg reorganisation
    * some PPC updates

}

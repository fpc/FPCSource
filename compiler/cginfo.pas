{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit exports some types and constants for the code generation

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
{# This unit exports some types which are used across the code generator }
unit cginfo;

{$i fpcdefs.inc}

interface

  uses cpuinfo,symconst;

    type
       {# Generic opcodes, which must be supported by all processors 
          The order of this table should not be changed, since table
          lookups are used in the different CPU code generators!
       }   
       TOpCg =
       (
          OP_NONE,
          OP_ADD,       { simple addition          }
          OP_AND,       { simple logical and       }
          OP_DIV,       { simple unsigned division }
          OP_IDIV,      { simple signed division   }
          OP_IMUL,      { simple signed multiply   }
          OP_MUL,       { simple unsigned multiply }
          OP_NEG,       { simple negate            }
          OP_NOT,       { simple logical not       }
          OP_OR,        { simple logical or        }
          OP_SAR,       { arithmetic shift-right   }
          OP_SHL,       { logical shift left       }
          OP_SHR,       { logical shift right      }
          OP_SUB,       { simple subtraction       }
          OP_XOR        { simple exclusive or      }
        );

       {# Generic flag values - used for jump locations }
       TOpCmp =
       (
          OC_NONE,
          OC_EQ,           { equality comparison              }
          OC_GT,           { greater than (signed)            }
          OC_LT,           { less than (signed)               }
          OC_GTE,          { greater or equal than (signed)   }
          OC_LTE,          { less or equal than (signed)      }
          OC_NE,           { not equal                        }
          OC_BE,           { less or equal than (unsigned)    }
          OC_B,            { less than (unsigned)             }
          OC_AE,           { greater or equal than (unsigned) }
          OC_A             { greater than (unsigned)          }
        );

       { OS_NO is also used memory references with large data that can
         not be loaded in a register directly }
       TCgSize = (OS_NO,
                 { integer registers }
                  OS_8,OS_16,OS_32,OS_64,OS_S8,OS_S16,OS_S32,OS_S64,
                 { single,double,extended,comp }
                  OS_F32,OS_F64,OS_F80,OS_C64,
                 { multi-media sizes: split in byte, word, dword, ... }
                 { entities, then the signed counterparts             }
                  OS_M8,OS_M16,OS_M32,OS_M64,OS_M128,OS_MS8,OS_MS16,OS_MS32,
                  OS_MS64,OS_MS128);

    const
       tcgsize2size : Array[tcgsize] of integer =
         { integer values }
        (0,1,2,4,8,1,2,4,8,
         { floating point values }
         4,8,EXTENDED_SIZE,8,
         { multimedia values }
         1,2,4,8,16,1,2,4,8,16);

       tfloat2tcgsize: array[tfloattype] of tcgsize =
         (OS_F32,OS_F64,OS_F80,OS_C64,OS_C64);

       tcgsize2tfloat: array[OS_F32..OS_C64] of tfloattype =
         (s32real,s64real,s80real,s64comp);

       { Table to convert tcgsize variables to the correspondending
         unsigned types }
       tcgsize2unsigned : array[tcgsize] of tcgsize = (OS_NO,
          OS_8,OS_16,OS_32,OS_64,OS_8,OS_16,OS_32,OS_64,
          OS_F32,OS_F64,OS_F80,OS_C64,
          OS_M8,OS_M16,OS_M32,OS_M64,OS_M128,OS_M8,OS_M16,OS_M32,
          OS_M64,OS_M128);

implementation

end.
{
  $Log$
  Revision 1.14  2002-08-04 19:06:41  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.13  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.12  2002/07/01 16:23:52  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.11  2002/05/27 19:16:08  carl
  + added comments to virtual comparison flags

  Revision 1.10  2002/05/18 13:34:05  peter
    * readded missing revisions

  Revision 1.9  2002/05/16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.7  2002/05/13 19:54:36  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.6  2002/05/06 19:48:26  carl
  + added more patches from Mazen for SPARC port

  Revision 1.5  2002/04/21 19:46:52  carl
  + added patch for SPARC from Mazen (to move to cpuinfo)

  Revision 1.4  2002/04/21 15:26:15  carl
  * move stuff to cpuinfo and cpubase
  + documented

  Revision 1.3  2002/04/20 21:32:23  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.2  2002/04/19 15:46:01  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.1  2002/04/02 18:09:47  jonas
    + initial implementation (Peter forgot to commit it)

}

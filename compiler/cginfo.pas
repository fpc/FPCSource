{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit cginfo;

{$i defines.inc}

interface

    type
       TOpCg = (OP_NONE,
                OP_ADD,OP_AND,OP_DIV,OP_IDIV,OP_IMUL,OP_MUL,OP_NEG,OP_NOT,
                OP_OR,OP_SAR,OP_SHL,OP_SHR,OP_SUB,OP_XOR
               );

       TOpCmp = (OC_NONE,OC_EQ,OC_GT,OC_LT,OC_GTE,OC_LTE,OC_NE,OC_BE,OC_B,
                 OC_AE,OC_A);

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
       { defines the default address size for a processor, }
       { the natural int size for a processor,             }
       { the maximum float size for a processor,           }
       { the size of a vector register for a processor     }
{$ifdef i386}
       OS_ADDR = OS_32;
       OS_INT = OS_32;
       OS_FLOAT = OS_F80;
       OS_VECTOR = OS_M64;
{$endif i386}
{$ifdef m68k}
       OS_ADDR = OS_32;
       OS_INT = OS_32;
       OS_FLOAT = OS_F??; { processor supports 64bit, but does the compiler? }
       OS_VECTOR = OS_NO;
{$endif m68k}
{$ifdef alpha}
       OS_ADDR = OS_64;
       OS_INT = OS_64;
       OS_FLOAT = OS_F??;
       OS_VECTOR = OS_NO;
{$endif alpha}
{$ifdef powerpc}
       OS_ADDR = OS_32;
       OS_INT = OS_32;
       OS_FLOAT = OS_F64;
       OS_VECTOR = OS_M128;
{$endif powercc}
{$ifdef ia64}
       OS_ADDR = OS_64;
       OS_INT = OS_64;
       OS_FLOAT = OS_F??;
       OS_VECTOR = OS_NO; { the normal registers can also be used as vectors }
{$endif ia64}

    const
      TCGSize2Size : Array[tcgsize] of integer =
        (0,1,2,4,8,1,2,4,8,
         4,8,10,8,
         1,2,4,8,16,1,2,4,8,16);


implementation

end.
{
  $Log$
  Revision 1.2  2002-04-19 15:46:01  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.1  2002/04/02 18:09:47  jonas
    + initial implementation (Peter forgot to commit it)

}

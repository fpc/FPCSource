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
       { Location types where value can be stored }
       TCGLoc=(
         LOC_INVALID,      { added for tracking problems}
         LOC_VOID,         { no value is available }
         LOC_CONSTANT,     { constant value }
         LOC_JUMP,         { boolean results only, jump to false or true label }
         LOC_FLAGS,        { boolean results only, flags are set }
         LOC_CREFERENCE,   { in memory constant value reference (cannot change) }
         LOC_REFERENCE,    { in memory value }
         LOC_REGISTER,     { in a processor register }
         LOC_CREGISTER,    { Constant register which shouldn't be modified }
         LOC_FPUREGISTER,  { FPU stack }
         LOC_CFPUREGISTER, { if it is a FPU register variable on the fpu stack }
         LOC_MMXREGISTER,  { MMX register }
         { MMX register variable }
         LOC_CMMXREGISTER,
         LOC_SSEREGISTER,
         LOC_CSSEREGISTER,
         { multimedia register }
         LOC_MMREGISTER,
         { Constant multimedia reg which shouldn't be modified }
         LOC_CMMREGISTER
       );

       {# Generic opcodes, which must be supported by all processors
       }
       topcg =
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
                 { single,double,extended,comp,float128 }
                  OS_F32,OS_F64,OS_F80,OS_C64,OS_F128,
                 { multi-media sizes: split in byte, word, dword, ... }
                 { entities, then the signed counterparts             }
                  OS_M8,OS_M16,OS_M32,OS_M64,OS_M128,OS_MS8,OS_MS16,OS_MS32,
                  OS_MS64,OS_MS128);

      { Register types }
      TRegisterType = (
        R_INVALIDREGISTER, { = 0 }
        R_INTREGISTER,     { = 1 }
        R_FPUREGISTER,     { = 2 }
        { used by Intel only }
        R_MMXREGISTER,     { = 3 }
        R_MMREGISTER,      { = 4 }
        R_SPECIALREGISTER, { = 5 }
        R_ADDRESSREGISTER  { = 6 }
      );

      { Sub registers }
      TSubRegister = (
        R_SUBNONE, { = 0; no sub register possible }
        R_SUBL,    { = 1; 8 bits, Like AL }
        R_SUBH,    { = 2; 8 bits, Like AH }
        R_SUBW,    { = 3; 16 bits, Like AX }
        R_SUBD,    { = 4; 32 bits, Like EAX }
        R_SUBQ     { = 5; 64 bits, Like RAX }
      );

      TSuperRegister = type byte;

      {
        The new register coding:

        SuperRegister   (bits 0..7)
        Unused          (bits 8..15)
        Subregister     (bits 16..23)
        Register type   (bits 24..31)
      }
      TRegister = type cardinal;
      TRegisterRec=packed record
{$ifdef FPC_BIG_ENDIAN}
         regtype : Tregistertype;
         subreg  : Tsubregister;
         unused  : byte;
         supreg  : Tsuperregister;
{$else FPC_BIG_ENDIAN}
         supreg  : Tsuperregister;
         unused  : byte;
         subreg  : Tsubregister;
         regtype : Tregistertype;
{$endif FPC_BIG_ENDIAN}
      end;

      { A type to store register locations for 64 Bit values. }
{$ifdef cpu64bit}
      tregister64 = tregister;
{$else cpu64bit}
      tregister64 = packed record
         reglo,reghi : tregister;
      end;
{$endif cpu64bit}

      { Set type definition for registers }
      tsuperregisterset = set of tsuperregister;

    const
       { Invalid register number }
       RS_INVALID    = $ff;

       tcgsize2size : Array[tcgsize] of integer =
         { integer values }
        (0,1,2,4,8,1,2,4,8,
         { floating point values }
         4,8,EXTENDED_SIZE,8,16,
         { multimedia values }
         1,2,4,8,16,1,2,4,8,16);

       tfloat2tcgsize: array[tfloattype] of tcgsize =
         (OS_F32,OS_F64,OS_F80,OS_C64,OS_C64,OS_F128);

       tcgsize2tfloat: array[OS_F32..OS_C64] of tfloattype =
         (s32real,s64real,s80real,s64comp);

       { Table to convert tcgsize variables to the correspondending
         unsigned types }
       tcgsize2unsigned : array[tcgsize] of tcgsize = (OS_NO,
          OS_8,OS_16,OS_32,OS_64,OS_8,OS_16,OS_32,OS_64,
          OS_F32,OS_F64,OS_F80,OS_C64,OS_F128,
          OS_M8,OS_M16,OS_M32,OS_M64,OS_M128,OS_M8,OS_M16,OS_M32,
          OS_M64,OS_M128);

       tcgloc2str : array[TCGLoc] of string[11] = (
            'LOC_INVALID',
            'LOC_VOID',
            'LOC_CONST',
            'LOC_JUMP',
            'LOC_FLAGS',
            'LOC_CREF',
            'LOC_REF',
            'LOC_REG',
            'LOC_CREG',
            'LOC_FPUREG',
            'LOC_CFPUREG',
            'LOC_MMXREG',
            'LOC_CMMXREG',
            'LOC_SSEREG',
            'LOC_CSSEREG',
            'LOC_MMREG',
            'LOC_CMMREG');

    function newreg(rt:tregistertype;sr:tsuperregister;sb:tsubregister):tregister;{$ifdef USEINLINE}inline;{$endif}
    function getsubreg(r:tregister):tsubregister;{$ifdef USEINLINE}inline;{$endif}
    function getsupreg(r:tregister):tsuperregister;{$ifdef USEINLINE}inline;{$endif}
    function getregtype(r:tregister):tregistertype;{$ifdef USEINLINE}inline;{$endif}
    procedure setsubreg(var r:tregister;sr:tsubregister);{$ifdef USEINLINE}inline;{$endif}
    procedure setsupreg(var r:tregister;sr:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
    function generic_regname(r:tregister):string;


implementation

    uses
      verbose;

    function newreg(rt:tregistertype;sr:tsuperregister;sb:tsubregister):tregister;{$ifdef USEINLINE}inline;{$endif}
      begin
        tregisterrec(result).regtype:=rt;
        tregisterrec(result).unused:=0;
        tregisterrec(result).supreg:=sr;
        tregisterrec(result).subreg:=sb;
      end;


    function getsubreg(r:tregister):tsubregister;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=tregisterrec(r).subreg;
      end;


    function getsupreg(r:tregister):tsuperregister;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=tregisterrec(r).supreg;
      end;


    function getregtype(r:tregister):tregistertype;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=tregisterrec(r).regtype;
      end;


    procedure setsubreg(var r:tregister;sr:tsubregister);{$ifdef USEINLINE}inline;{$endif}
      begin
        tregisterrec(r).subreg:=sr;
      end;


    procedure setsupreg(var r:tregister;sr:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
      begin
        tregisterrec(r).supreg:=sr;
      end;


    function generic_regname(r:tregister):string;
      var
        t,sub : char;
        nr    : string[12];
      begin
        case getregtype(r) of
          R_INTREGISTER:
            t:='i';
          R_FPUREGISTER:
            t:='f';
          R_MMXREGISTER:
            t:='x';
          R_MMREGISTER:
            t:='m';
          else
            begin
              result:='INVALID';
              exit;
            end;
        end;
        str(getsupreg(r),nr);
        case getsubreg(r) of
          R_SUBNONE:
            sub:=' ';
          R_SUBL:
            sub:='l';
          R_SUBH:
            sub:='h';
          R_SUBW:
            sub:='w';
          R_SUBD:
            sub:='d';
          R_SUBQ:
            sub:='q';
          else
            internalerror(200308252);
        end;
        if sub<>' ' then
          result:=t+'reg'+nr+sub
        else
          result:=t+'reg'+nr;
      end;

end.
{
  $Log$
  Revision 1.26  2003-09-14 19:30:58  daniel
    * Fixed endian problem in Tregisterrec record

  Revision 1.25  2003/09/04 21:07:03  florian
    * ARM compiler compiles again

  Revision 1.24  2003/09/03 21:06:19  peter
    * write INVALID as register name instead of IE

  Revision 1.23  2003/09/03 16:29:37  peter
    * superregisters also from .dat file

  Revision 1.22  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.21.2.4  2003/09/01 21:02:55  peter
    * sparc updates for new tregister

  Revision 1.21.2.3  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.21.2.2  2003/08/28 18:35:07  peter
    * tregister changed to cardinal

  Revision 1.21.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.21  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.20  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.19  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.18  2003/01/09 22:00:53  florian
    * fixed some PowerPC issues

  Revision 1.17  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.16  2002/09/07 15:25:01  peter
    * old logs removed and tabs fixed

  Revision 1.15  2002/08/05 18:27:48  carl
    + more more more documentation
    + first version include/exclude (can't test though, not enough scratch for i386 :()...

  Revision 1.14  2002/08/04 19:06:41  carl
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

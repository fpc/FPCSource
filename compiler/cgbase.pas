{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Some basic types and constants for the code generation

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
unit cgbase;

{$i fpcdefs.inc}

interface

    uses
      cpuinfo,
      symconst;

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
        R_SUBQ,    { = 5; 64 bits, Like RAX }
        R_SUBF64   { = 6; 64 bits float that allocates 2 FPU registers }
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

      { Temp types }
      ttemptype = (tt_none,
                   tt_free,tt_normal,tt_persistent,
                   tt_noreuse,tt_freenoreuse,
                   tt_ansistring,tt_freeansistring,
                   tt_widestring,tt_freewidestring,
                   tt_interfacecom,tt_freeinterfacecom);
      ttemptypeset = set of ttemptype;

      pmmshuffle = ^tmmshuffle;

      { this record describes shuffle operations for mm operations; if a pointer a shuffle record
        passed to an mm operation is nil, it means that the whole location is moved }
      tmmshuffle = record
        { describes how many shuffles are actually described, if len=0 then
          moving the scalar with index 0 to the scalar with index 0 is meant }
        len : byte;
        { lower nibble of each entry of this array describes index of the source data index while
          the upper nibble describes the destination index }
        shuffles : array[1..1] of byte;
      end;

    const
       { alias for easier understanding }
       R_SSEREGISTER = R_MMREGISTER;

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

    var
       mms_movescalar : pmmshuffle;

    function newreg(rt:tregistertype;sr:tsuperregister;sb:tsubregister):tregister;{$ifdef USEINLINE}inline;{$endif}
    function getsubreg(r:tregister):tsubregister;{$ifdef USEINLINE}inline;{$endif}
    function getsupreg(r:tregister):tsuperregister;{$ifdef USEINLINE}inline;{$endif}
    function getregtype(r:tregister):tregistertype;{$ifdef USEINLINE}inline;{$endif}
    procedure setsubreg(var r:tregister;sr:tsubregister);{$ifdef USEINLINE}inline;{$endif}
    procedure setsupreg(var r:tregister;sr:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
    function generic_regname(r:tregister):string;

    {# From a constant numeric value, return the abstract code generator
       size.
    }
    function int_cgsize(const a: aword): tcgsize;

    { return the inverse condition of opcmp }
    function inverse_opcmp(opcmp: topcmp): topcmp;

    { return whether op is commutative }
    function commutativeop(op: topcg): boolean;

    { returns true, if shuffle describes a real shuffle operation and not only a move }
    function realshuffle(shuffle : pmmshuffle) : boolean;

    { removes shuffling from shuffle, this means that the destenation index of each shuffle is copied to
      the source }
    procedure removeshuffles(var shuffle : tmmshuffle);

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


    function int_cgsize(const a: aword): tcgsize;
      begin
        if a > 8 then
          begin
            int_cgsize := OS_NO;
            exit;
          end;
        case byte(a) of
          1 :
            result := OS_8;
          2 :
            result := OS_16;
          3,4 :
            result := OS_32;
          5..8 :
            result := OS_64;
        end;
      end;


    function inverse_opcmp(opcmp: topcmp): topcmp;
      const
        list: array[TOpCmp] of TOpCmp =
          (OC_NONE,OC_NE,OC_LTE,OC_GTE,OC_LT,OC_GT,OC_EQ,OC_A,OC_AE,
           OC_B,OC_BE);
      begin
        inverse_opcmp := list[opcmp];
      end;


    function commutativeop(op: topcg): boolean;
      const
        list: array[topcg] of boolean =
          (true,true,true,false,false,true,true,false,false,
           true,false,false,false,false,true);
      begin
        commutativeop := list[op];
      end;


    function realshuffle(shuffle : pmmshuffle) : boolean;
      var
        i : longint;
      begin
        realshuffle:=true;
        if (shuffle=nil) or (shuffle^.len=0) then
          realshuffle:=false
        else
          begin
            for i:=1 to shuffle^.len do
              begin
                if (shuffle^.shuffles[i] and $f)<>((shuffle^.shuffles[i] and $f0) shr 8) then
                  exit;
              end;
            realshuffle:=false;
          end;
      end;


    procedure removeshuffles(var shuffle : tmmshuffle);
      var
        i : longint;
      begin
        if shuffle.len=0 then
          exit;
        for i:=1 to shuffle.len do
          shuffle.shuffles[i]:=(shuffle.shuffles[i] and $f0) or ((shuffle.shuffles[i] and $f0) shr 8);
      end;


initialization
  new(mms_movescalar);
  mms_movescalar^.len:=0;
finalization
  dispose(mms_movescalar);
end.
{
  $Log$
  Revision 1.70  2003-10-13 01:10:01  florian
    * some ideas for mm support implemented

  Revision 1.69  2003/10/11 16:06:42  florian
    * fixed some MMX<->SSE
    * started to fix ppc, needs an overhaul
    + stabs info improve for spilling, not sure if it works correctly/completly
    - MMX_SUPPORT removed from Makefile.fpc

  Revision 1.68  2003/10/09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.67  2003/10/01 20:34:48  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

}

{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the i386 specific class for the register
    allocator

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

    uses
      cpubase,
      cpuinfo,
      aasmbase,aasmtai,aasmcpu,
      cclasses,globtype,cgbase,cginfo,rgobj;

    type
       trgcpu = class(trgobj)

          { to keep the same allocation order as with the old routines }
          function getregisterint(list:Taasmoutput;size:Tcgsize):Tregister;override;
{$ifndef newra}
          procedure ungetregisterint(list:Taasmoutput;r:Tregister); override;
          function getexplicitregisterint(list:Taasmoutput;r:Tnewregister):Tregister;override;
{$endif newra}

          function getregisterfpu(list: taasmoutput) : tregister; override;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister); override;

          procedure ungetreference(list: taasmoutput; const ref : treference); override;

          {# Returns a subset register of the register r with the specified size.
             WARNING: There is no clearing of the upper parts of the register,
             if a 8-bit / 16-bit register is converted to a 32-bit register.
             It is up to the code generator to correctly zero fill the register
          }
          function makeregsize(reg: tregister; size: tcgsize): tregister; override;

          procedure resetusableregisters;override;

         { corrects the fpu stack register by ofs }
         function correct_fpuregister(r : tregister;ofs : byte) : tregister;

         fpuvaroffset : byte;
       end;


  implementation

    uses
       systems,
       globals,verbose,
       tgobj;

{************************************************************************}
{                         routine helpers                                }
{************************************************************************}

  const
    reg2reg64 : array[firstreg..lastreg] of toldregister = (R_NO,
      R_RAX,R_RCX,R_RDX,R_RBX,R_RSP,R_RBP,R_RSI,R_RDI,
      R_R8,R_R9,R_R10,R_R11,R_R12,R_R13,R_R14,R_R15,R_RIP,
      R_RAX,R_RCX,R_RDX,R_RBX,R_RSP,R_RBP,R_RSI,R_RDI,
      R_R8,R_R9,R_R10,R_R11,R_R12,R_R13,R_R14,R_R15,
      R_RAX,R_RCX,R_RDX,R_RBX,R_RSP,R_RBP,R_RSI,R_RDI,
      R_R8,R_R9,R_R10,R_R11,R_R12,R_R13,R_R14,R_R15,
      R_RAX,R_RCX,R_RDX,R_RBX,R_RSP,R_RBP,R_RSI,R_RDI,
      R_R8,R_R9,R_R10,R_R11,R_R12,R_R13,R_R14,R_R15,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO
    );

    reg2reg32 : array[firstreg..lastreg] of toldregister = (R_NO,
      R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
      R_R8D,R_R9D,R_R10D,R_R11D,R_R12D,R_R13D,R_R14D,R_R15D,R_NO,
      R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
      R_R8D,R_R9D,R_R10D,R_R11D,R_R12D,R_R13D,R_R14D,R_R15D,
      R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
      R_R8D,R_R9D,R_R10D,R_R11D,R_R12D,R_R13D,R_R14D,R_R15D,
      R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
      R_R8D,R_R9D,R_R10D,R_R11D,R_R12D,R_R13D,R_R14D,R_R15D,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO
    );

    reg2reg16 : array[firstreg..lastreg] of toldregister = (R_NO,
      R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
      R_R8W,R_R9W,R_R10W,R_R11W,R_R12W,R_R13W,R_R14W,R_R15W,R_NO,
      R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
      R_R8W,R_R9W,R_R10W,R_R11W,R_R12W,R_R13W,R_R14W,R_R15W,
      R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
      R_R8W,R_R9W,R_R10W,R_R11W,R_R12W,R_R13W,R_R14W,R_R15W,
      R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
      R_R8W,R_R9W,R_R10W,R_R11W,R_R12W,R_R13W,R_R14W,R_R15W,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO
    );

    reg2reg8 : array[firstreg..lastreg] of toldregister = (R_NO,
      R_AL,R_CL,R_DL,R_BL,R_SPL,R_BPL,R_SIL,R_DIL,
      R_R8B,R_R9B,R_R10B,R_R11B,R_R12B,R_R13B,R_R14B,R_R15B,R_NO,
      R_AL,R_CL,R_DL,R_BL,R_SPL,R_BPL,R_SIL,R_DIL,
      R_R8B,R_R9B,R_R10B,R_R11B,R_R12B,R_R13B,R_R14B,R_R15B,
      R_AL,R_CL,R_DL,R_BL,R_SPL,R_BPL,R_SIL,R_DIL,
      R_R8B,R_R9B,R_R10B,R_R11B,R_R12B,R_R13B,R_R14B,R_R15B,
      R_AL,R_CL,R_DL,R_BL,R_SPL,R_BPL,R_SIL,R_DIL,
      R_R8B,R_R9B,R_R10B,R_R11B,R_R12B,R_R13B,R_R14B,R_R15B,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO
    );

    { convert a register to a specfied register size }
    function changeregsize(r:tregister;size:topsize):tregister;
      var
        reg : tregister;
      begin
        case size of
          S_B :
            reg.enum:=reg2reg8[r.enum];
          S_W :
            reg.enum:=reg2reg16[r.enum];
          S_L :
            reg.enum:=reg2reg32[r.enum];
          S_Q :
            reg.enum:=reg2reg64[r.enum];
          else
            internalerror(200204101);
        end;
        if reg.enum=R_NO then
         internalerror(200204102);
        changeregsize:=reg;
      end;


{************************************************************************}
{                               trgcpu                                   }
{************************************************************************}

    function trgcpu.getregisterint(list: taasmoutput;size:Tcgsize): tregister;
    var subreg:Tsubregister;

    begin
      subreg:=cgsize2subreg(size);

      if countunusedregsint=0 then
        internalerror(10);
      result.enum:=R_INTREGISTER;
{$ifdef TEMPREGDEBUG}
      if curptree^.usableregsint-countunusedregsint>curptree^.registers32 then
        internalerror(10);
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
      if curptree^.usableregs-countunusedregistersint>curptree^^.reallyusedregs then
        curptree^.reallyusedregs:=curptree^^.usableregs-countunusedregistersint;
{$endif EXTTEMPREGDEBUG}
      if RS_RAX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_RAX);
          include(usedintinproc,RS_RAX);
          result.number:=RS_RAX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_RAX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else if RS_RDX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_RDX);
          include(usedintinproc,RS_RDX);
          result.number:=RS_RDX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_RDX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else if RS_RBX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_RBX);
          include(usedintinproc,RS_RBX);
          result.number:=RS_RBX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_RBX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else if RS_RCX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_RCX);
          include(usedintinproc,RS_RCX);
          result.number:=RS_RCX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_RCX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else
        internalerror(10);
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;



    procedure trgcpu.ungetregisterint(list: taasmoutput; r : tregister);
      var supreg:Tsuperregister;
      begin
         if r.enum=R_NO then
          exit;
         if r.enum<>R_INTREGISTER then
            internalerror(200301234);
         supreg:=r.number shr 8;
         if (supreg in [RS_RDI]) then
           begin
             list.concat(tai_regalloc.DeAlloc(r));
             exit;
           end;
         if not(supreg in [RS_RAX,RS_RBX,RS_RCX,RS_RDX,RS_RSI]) then
           exit;
         inherited ungetregisterint(list,r);
      end;


   function trgcpu.getexplicitregisterint(list: taasmoutput; r : tnewregister) : tregister;

   var r2:Tregister;

    begin
      if (r shr 8) in [RS_RDI] then
        begin
          r2.enum:=R_INTREGISTER;
          r2.number:=r;
          list.concat(Tai_regalloc.alloc(r2));
          getexplicitregisterint:=r2;
          exit;
        end;
      result:=inherited getexplicitregisterint(list,r);
    end;


    function trgcpu.getregisterfpu(list: taasmoutput) : tregister;

      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result.enum := R_ST;
      end;


    procedure trgcpu.ungetregisterfpu(list : taasmoutput; r : tregister);

      begin
        { nothing to do, fpu stack management is handled by the load/ }
        { store operations in cgcpu (JM)                              }
      end;


    procedure trgcpu.ungetreference(list: taasmoutput; const ref : treference);

      begin
         ungetregisterint(list,ref.base);
         ungetregisterint(list,ref.index);
      end;

   procedure trgcpu.resetusableregisters;

     begin
       inherited resetusableregisters;
       fpuvaroffset := 0;
     end;


   function trgcpu.correct_fpuregister(r : tregister;ofs : byte) : tregister;

     begin
        correct_fpuregister.enum:=toldregister(longint(r.enum)+ofs);
     end;


    function trgcpu.makeregsize(reg: tregister; size: tcgsize): tregister;

      var
        _result : topsize;
      begin
        case size of
          OS_32,OS_S32:
            begin
              _result := S_L;
            end;
          OS_8,OS_S8:
            begin
              _result := S_B;
            end;
          OS_16,OS_S16:
            begin
              _result := S_W;
            end;
          else
            internalerror(2001092312);
        end;
        makeregsize := changeregsize(reg,_result);
      end;



initialization
  rg := trgcpu.create(15);
end.

{
  $Log$
  Revision 1.4  2002-04-25 20:15:40  florian
    * block nodes within expressions shouldn't release the used registers,
      fixed using a flag till the new rg is ready

  Revision 1.3  2003/01/05 13:36:54  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.2  2002/07/25 22:55:34  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

  Revision 1.8  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.7  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.6  2002/05/12 16:53:18  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.5  2002/04/21 15:43:32  carl
  * changeregsize -> rg.makeregsize
  * changeregsize moved from cpubase to here

  Revision 1.4  2002/04/15 19:44:22  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.3  2002/04/04 19:06:13  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.2  2002/04/02 17:11:39  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.1  2002/03/31 20:26:40  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}

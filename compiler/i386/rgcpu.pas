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
      aasmbase,aasmtai,
      cclasses,globtype,cgbase,rgobj;

    type
       Tregisterallocatorcpu = class(Tregisterallocator)
          procedure add_constraints(reg:Tregister);override;
       end;


       Trgcpu = class(Trgobj)
          fpuvaroffset : byte;

          function getregisterfpu(list: taasmoutput;size:TCGSize) : tregister; override;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister); override;

          {# Returns a subset register of the register r with the specified size.
             WARNING: There is no clearing of the upper parts of the register,
             if a 8-bit / 16-bit register is converted to a 32-bit register.
             It is up to the code generator to correctly zero fill the register
          }
{          function makeregsize(reg: tregister; size: tcgsize): tregister; override;}

          { pushes and restores registers }
{$ifdef SUPPORT_MMX}
          procedure pushusedotherregisters(list:Taasmoutput;
                                           var pushed:Tpushedsavedother;
                                           const s:Totherregisterset);
{$endif SUPPORT_MMX}
{$ifdef SUPPORT_MMX}
          procedure popusedotherregisters(list:Taasmoutput;
                                          const pushed:Tpushedsavedother);
{$endif SUPPORT_MMX}

          procedure saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsavedother;
                                           const s:Totherregisterset);override;
          procedure restoreusedotherregisters(list:Taasmoutput;
                                              const saved:Tpushedsavedother);override;

          procedure resetusableregisters;override;

         { corrects the fpu stack register by ofs }
         function correct_fpuregister(r : tregister;ofs : byte) : tregister;
       end;

  implementation

    uses
       systems,
       globals,verbose;

{************************************************************************
                          tregisterallocatorcpu
*************************************************************************}

    procedure Tregisterallocatorcpu.add_constraints(reg:Tregister);
    var
      supreg : tsuperregister;
    begin
      if getsubreg(reg) in [R_SUBL,R_SUBH] then
        begin
          supreg:=getsupreg(reg);
          {These registers have no 8-bit subregister, so add interferences.}
          add_edge(supreg,RS_ESI);
          add_edge(supreg,RS_EDI);
          add_edge(supreg,RS_EBP);
        end;
    end;


{************************************************************************
                                   trgcpu
*************************************************************************}

    function trgcpu.getregisterfpu(list: taasmoutput;size: TCGSize) : tregister;

      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result:=NR_ST;
      end;


    procedure trgcpu.ungetregisterfpu(list : taasmoutput; r : tregister);

      begin
        { nothing to do, fpu stack management is handled by the load/ }
        { store operations in cgcpu (JM)                              }
      end;


{$ifdef SUPPORT_MMX}
    procedure trgcpu.pushusedotherregisters(list:Taasmoutput;
                                            var pushed:Tpushedsavedother;
                                            const s:Totherregisterset);

{    var r:Toldregister;
        r2:Tregister;
        hr:Treference;}

    begin
(*      used_in_proc_other:=used_in_proc_other+s;
      for r:=R_MM0 to R_MM6 do
        begin
          pushed[r].pushed:=false;
          { if the register is used by the calling subroutine    }
          if not is_reg_var_other[r] and
             (r in s) and
             { and is present in use }
             not(r in unusedregsmm) then
            begin
              r2.enum:=R_INTREGISTER;
              r2.number:=NR_ESP;
              list.concat(Taicpu.Op_const_reg(A_SUB,S_L,8,r2));
              reference_reset_base(hr,r2,0);
              r2.enum:=r;
              list.concat(Taicpu.Op_reg_ref(A_MOVQ,S_NO,r2,hr));
              include(unusedregsmm,r);
              pushed[r].pushed:=true;
            end;
        end;*)
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;
{$endif SUPPORT_MMX}


{$ifdef SUPPORT_MMX}
    procedure trgcpu.popusedotherregisters(list:Taasmoutput;
                                           const pushed:Tpushedsavedother);

{    var r:Toldregister;
        r2,r3:Tregister;
        hr:Treference;}

    begin
      { restore in reverse order: }
{      for r:=R_MM6 downto R_MM0 do
        if pushed[r].pushed then
          begin
            r2.enum:=R_INTREGISTER;
            r2.number:=NR_ESP;
            reference_reset_base(hr,r2,0);
            r3.enum:=r;
            list.concat(Taicpu.op_ref_reg(A_MOVQ,S_NO,hr,r3));
            list.concat(Taicpu.op_const_reg(A_ADD,S_L,8,r2));
            exclude(unusedregsmm,r);
          end;}
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;
{$endif SUPPORT_MMX}



    procedure trgcpu.saveusedotherregisters(list:Taasmoutput;var saved:Tpushedsavedother;
                                            const s:totherregisterset);

    begin
{$ifdef SUPPORT_MMX}
      if (aktoptprocessor in [class386,classP5]) or
         (CS_LittleSize in aktglobalswitches) then
        pushusedotherregisters(list,saved,s)
      else
{$endif SUPPORT_MMX}
        inherited saveusedotherregisters(list,saved,s);
    end;


    procedure trgcpu.restoreusedotherregisters(list:Taasmoutput;
                                               const saved:tpushedsavedother);

    begin
{$ifdef SUPPORT_MMX}
      if (aktoptprocessor in [class386,classP5]) or
         (CS_LittleSize in aktglobalswitches) then
        popusedotherregisters(list,saved)
      else
{$endif SUPPORT_MMX}
        inherited restoreusedotherregisters(list,saved);
    end;


   procedure trgcpu.resetusableregisters;

     begin
       inherited resetusableregisters;
       fpuvaroffset := 0;
     end;


   function trgcpu.correct_fpuregister(r : tregister;ofs : byte) : tregister;

     begin
        correct_fpuregister:=r;
        setsupreg(correct_fpuregister,ofs);
     end;

(*
    function trgcpu.makeregsize(reg: tregister; size: tcgsize): tregister;
      var
        subreg : tsubregister;
      begin
        if getregtype(reg)<>R_INTREGISTER then
          internalerror(200306032);
        subreg:=cgsize2subreg(size);
        result:=reg;
        setsubreg(result,subreg);
        add_constraints(result);
      end;
*)
end.
{
  $Log$
  Revision 1.37  2003-10-09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.36  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.35  2003/09/11 11:55:00  florian
    * improved arm code generation
    * move some protected and private field around
    * the temp. register for register parameters/arguments are now released
      before the move to the parameter register is done. This improves
      the code in a lot of cases.

  Revision 1.34  2003/09/09 20:59:27  daniel
    * Adding register allocation order

  Revision 1.33  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.32  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.31.2.3  2003/08/31 13:50:16  daniel
    * Remove sorting and use pregenerated indexes
    * Some work on making things compile

  Revision 1.31.2.2  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.31.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.31  2003/08/20 09:07:00  daniel
    * New register coding now mandatory, some more convert_registers calls
      removed.

  Revision 1.30  2003/08/17 08:48:02  daniel
   * Another register allocator bug fixed.
   * cpu_registers set to 6 for i386

  Revision 1.29  2003/06/17 16:51:30  peter
    * cycle fixes

  Revision 1.28  2003/06/17 16:34:44  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to volatile_intregisters and made it
      processor dependent

  Revision 1.27  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.26  2003/06/12 21:12:20  peter
    * size para for ungetregisterfpu

  Revision 1.25  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.24  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.23  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.22  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.21  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.20  2003/04/23 14:42:08  daniel
    * Further register allocator work. Compiler now smaller with new
      allocator than without.
    * Somebody forgot to adjust ppu version number

  Revision 1.19  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.18  2003/04/21 19:16:50  peter
    * count address regs separate

  Revision 1.17  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.16  2003/03/17 15:52:57  peter
    * SUPPORT_MMX define compile fix

  Revision 1.15  2003/03/08 13:59:17  daniel
    * Work to handle new register notation in ag386nsm
    + Added newra version of Ti386moddivnode

  Revision 1.14  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.13  2003/03/07 21:57:53  daniel
    * Improved getregisterint

  Revision 1.12  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.11  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.10  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.9  2002/08/17 09:23:48  florian
    * first part of procinfo rewrite

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

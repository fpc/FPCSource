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
       trgcpu = class(trgobj)
          procedure add_constraints(reg:Tregister);override;
       end;


       trgx86fpu = class
          { The "usableregsxxx" contain all registers of type "xxx" that }
          { aren't currently allocated to a regvar. The "unusedregsxxx"  }
          { contain all registers of type "xxx" that aren't currently    }
          { allocated                                                    }
          unusedregsfpu,usableregsfpu : Tsuperregisterset;
          { these counters contain the number of elements in the }
          { unusedregsxxx/usableregsxxx sets                     }
          countunusedregsfpu : byte;
          countusableregsfpu : byte;

          { Contains the registers which are really used by the proc itself.
            It doesn't take care of registers used by called procedures
          }
          used_in_proc_other : totherregisterset;

          {reg_pushes_other : regvarother_longintarray;
          is_reg_var_other : regvarother_booleanarray;
          regvar_loaded_other : regvarother_booleanarray;}

          { tries to hold the amount of times which the current tree is processed  }
          t_times: longint;

          fpuvaroffset : byte;

          constructor create;

          function getregisterfpu(list: taasmoutput) : tregister;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister);

          { pushes and restores registers }
          procedure saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsavedother;
                                           const s:Totherregisterset);
          procedure restoreusedotherregisters(list:Taasmoutput;
                                              const saved:Tpushedsavedother);

          { corrects the fpu stack register by ofs }
          function correct_fpuregister(r : tregister;ofs : byte) : tregister;
       end;


implementation

    uses
       systems,
       verbose;

{************************************************************************
                                 trgcpu
*************************************************************************}

    procedure trgcpu.add_constraints(reg:Tregister);
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


{******************************************************************************
                                    Trgobj
******************************************************************************}

    constructor Trgx86fpu.create;

      var i:Tsuperregister;

      begin
        used_in_proc_other:=[];
        t_times := 0;
        countusableregsfpu:=c_countusableregsfpu;
        unusedregsfpu:=usableregsfpu;
        countunusedregsfpu:=countusableregsfpu;
      end;


    function trgx86fpu.getregisterfpu(list: taasmoutput) : tregister;

      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result:=NR_ST;
      end;


    procedure trgx86fpu.ungetregisterfpu(list : taasmoutput; r : tregister);

      begin
        { nothing to do, fpu stack management is handled by the load/ }
        { store operations in cgcpu (JM)                              }
      end;



    function trgx86fpu.correct_fpuregister(r : tregister;ofs : byte) : tregister;

      begin
        correct_fpuregister:=r;
        setsupreg(correct_fpuregister,ofs);
      end;


    procedure trgx86fpu.saveusedotherregisters(list: taasmoutput;
        var saved : tpushedsavedother; const s: totherregisterset);

      var
         r : tregister;
         hr : treference;

      begin
        used_in_proc_other:=used_in_proc_other + s;

{$warning TODO firstsavefpureg}
(*
        { don't try to save the fpu registers if not desired (e.g. for }
        { the 80x86)                                                   }
        if firstsavefpureg <> R_NO then
          for r.enum:=firstsavefpureg to lastsavefpureg do
            begin
              saved[r.enum].ofs:=reg_not_saved;
              { if the register is used by the calling subroutine and if }
              { it's not a regvar (those are handled separately)         }
              if not is_reg_var_other[r.enum] and
                 (r.enum in s) and
                 { and is present in use }
                 not(r.enum in unusedregsfpu) then
                begin
                  { then save it }
                  tg.GetTemp(list,extended_size,tt_persistent,hr);
                  saved[r.enum].ofs:=hr.offset;
                  cg.a_loadfpu_reg_ref(list,OS_FLOAT,r,hr);
                  cg.a_reg_dealloc(list,r);
                  include(unusedregsfpu,r.enum);
                  inc(countunusedregsfpu);
                end;
            end;
*)
      end;


    procedure trgx86fpu.restoreusedotherregisters(list : taasmoutput;
        const saved : tpushedsavedother);

      var
         r,r2 : tregister;
         hr : treference;

      begin
{$warning TODO firstsavefpureg}
(*
        if firstsavefpureg <> R_NO then
          for r.enum:=lastsavefpureg downto firstsavefpureg do
            begin
              if saved[r.enum].ofs <> reg_not_saved then
                begin
                  r2.enum:=R_INTREGISTER;
                  r2.number:=NR_FRAME_POINTER_REG;
                  reference_reset_base(hr,r2,saved[r.enum].ofs);
                  cg.a_reg_alloc(list,r);
                  cg.a_loadfpu_ref_reg(list,OS_FLOAT,hr,r);
                  if not (r.enum in unusedregsfpu) then
                    { internalerror(10)
                      in n386cal we always save/restore the reg *state*
                      using save/restoreunusedstate -> the current state
                      may not be real (JM) }
                  else
                    begin
                      dec(countunusedregsfpu);
                      exclude(unusedregsfpu,r.enum);
                    end;
                  tg.UnGetTemp(list,hr);
                end;
            end;
*)
      end;

(*
    procedure Trgx86fpu.saveotherregvars(list: taasmoutput; const s: totherregisterset);
      var
        r: Tregister;
      begin
        if not(cs_regvars in aktglobalswitches) then
          exit;
        if firstsavefpureg <> NR_NO then
          for r.enum := firstsavefpureg to lastsavefpureg do
            if is_reg_var_other[r.enum] and
               (r.enum in s) then
              store_regvar(list,r);
      end;
*)

end.
{
  $Log$
  Revision 1.38  2003-10-10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.37  2003/10/09 21:31:37  daniel
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

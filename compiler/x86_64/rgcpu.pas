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
          fpuvaroffset : byte;

          function getregisterfpu(list: taasmoutput;size:TCGSize) : tregister; override;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister;size:TCGSize); override;

          {# Returns a subset register of the register r with the specified size.
             WARNING: There is no clearing of the upper parts of the register,
             if a 8-bit / 16-bit register is converted to a 32-bit register.
             It is up to the code generator to correctly zero fill the register
          }
          function makeregsize(reg: tregister; size: tcgsize): tregister; override;

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
       globals,verbose,
       tgobj;

    function trgcpu.getregisterfpu(list: taasmoutput;size: TCGSize) : tregister;

      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result:=NR_ST;
      end;


    procedure trgcpu.ungetregisterfpu(list : taasmoutput; r : tregister;size:TCGSize);

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

end.
{
  $Log$
  Revision 1.6  2003-09-24 17:12:02  florian
    * several fixes for new reg allocator

  Revision 1.5  2003/06/13 21:19:33  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.4  2002/04/25 20:15:40  florian
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

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

          { to keep the same allocation order as with the old routines }
{$ifdef newra}
          procedure add_constraints(reg:Tnewregister);override;
{$else}
          function getregisterint(list:Taasmoutput;size:Tcgsize):Tregister;override;
          function getaddressregister(list:Taasmoutput):Tregister;override;
          procedure ungetregisterint(list:Taasmoutput;r:Tregister); override;
          function getexplicitregisterint(list:Taasmoutput;r:Tnewregister):Tregister;override;
{$endif newra}

          function getregisterfpu(list: taasmoutput;size:TCGSize) : tregister; override;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister;size:TCGSize); override;

          procedure ungetreference(list: taasmoutput; const ref : treference); override;

          {# Returns a subset register of the register r with the specified size.
             WARNING: There is no clearing of the upper parts of the register,
             if a 8-bit / 16-bit register is converted to a 32-bit register.
             It is up to the code generator to correctly zero fill the register
          }
          function makeregsize(reg: tregister; size: tcgsize): tregister; override;

          { pushes and restores registers }
{$ifndef newra}
          procedure pushusedintregisters(list:Taasmoutput;
                                         var pushed:Tpushedsavedint;
                                         const s:Tsupregset);
{$endif}
{$ifdef SUPPORT_MMX}
          procedure pushusedotherregisters(list:Taasmoutput;
                                           var pushed:Tpushedsavedother;
                                           const s:Tregisterset);
{$endif SUPPORT_MMX}
{$ifndef newra}
          procedure popusedintregisters(list:Taasmoutput;
                                        const pushed:Tpushedsavedint);
{$endif}
{$ifdef SUPPORT_MMX}
          procedure popusedotherregisters(list:Taasmoutput;
                                          const pushed:Tpushedsavedother);
{$endif SUPPORT_MMX}

{$ifndef newra}
          procedure saveusedintregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedint;
                                         const s:Tsupregset);override;
{$endif}
          procedure saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsavedother;
                                           const s:Tregisterset);override;
{$ifndef newra}
          procedure restoreusedintregisters(list:Taasmoutput;
                                            const saved:Tpushedsavedint);override;
{$endif}
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

{************************************************************************}
{                               trgcpu                                   }
{************************************************************************}

{$ifdef newra}
    procedure Trgcpu.add_constraints(reg:Tnewregister);

    begin
      if reg and $ff in [R_SUBL,R_SUBH] then
        begin
          {These registers have no 8-bit subregister, so add interferences.}
          add_edge(reg shr 8,RS_ESI);
          add_edge(reg shr 8,RS_EDI);
          add_edge(reg shr 8,RS_EBP);
        end;
    end;
{$endif}

{$ifndef newra}
    function trgcpu.getregisterint(list:Taasmoutput;size:Tcgsize):Tregister;

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
      if RS_EAX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_EAX);
          include(usedintinproc,RS_EAX);
          result.number:=RS_EAX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_EAX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else if RS_EDX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_EDX);
          include(usedintinproc,RS_EDX);
          result.number:=RS_EDX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_EDX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else if RS_EBX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_EBX);
          include(usedintinproc,RS_EBX);
          result.number:=RS_EBX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_EBX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else if RS_ECX in unusedregsint then
        begin
          dec(countunusedregsint);
          exclude(unusedregsint,RS_ECX);
          include(usedintinproc,RS_ECX);
          result.number:=RS_ECX shl 8 or subreg;
{$ifdef TEMPREGDEBUG}
          reg_user[R_ECX]:=curptree^;
{$endif TEMPREGDEBUG}
          exprasmlist.concat(tai_regalloc.alloc(result));
        end
      else
        internalerror(10);
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;


    function trgcpu.getaddressregister(list:Taasmoutput):Tregister;
    begin
      if countunusedregsaddr>0 then
       begin
         result.enum:=R_INTREGISTER;
{$ifdef TEMPREGDEBUG}
         if curptree^.usableregsaddr-countunusedregsaddr>curptree^.registers32 then
           internalerror(10);
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
         if curptree^.usableregs-countunusedregistersaddr>curptree^^.reallyusedregs then
           curptree^.reallyusedregs:=curptree^^.usableregs-countunusedregistersaddr;
{$endif EXTTEMPREGDEBUG}
         if RS_ESI in unusedregsaddr then
           begin
             dec(countunusedregsaddr);
             exclude(unusedregsaddr,RS_ESI);
             include(usedaddrinproc,RS_ESI);
             result.number:=NR_ESI;
{$ifdef TEMPREGDEBUG}
             reg_user[R_ESI]:=curptree^;
{$endif TEMPREGDEBUG}
             exprasmlist.concat(tai_regalloc.alloc(result));
           end
         else
           internalerror(10);
       end
      else
       result:=getregisterint(list,OS_ADDR);
    end;


    procedure trgcpu.ungetregisterint(list: taasmoutput; r : tregister);

    var supreg:Tsuperregister;

      begin
         if r.enum=R_NO then
          exit;
         if r.enum<>R_INTREGISTER then
            internalerror(200301234);
         supreg:=r.number shr 8;
         if (supreg in [RS_EDI]) then
           begin
             list.concat(tai_regalloc.DeAlloc(r));
             exit;
           end;
         if not(supreg in [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI]) then
           exit;
         inherited ungetregisterint(list,r);
      end;


   function trgcpu.getexplicitregisterint(list:Taasmoutput;r:Tnewregister):Tregister;

   var r2:Tregister;

    begin
      if (r shr 8) in [RS_EDI] then
        begin
          r2.enum:=R_INTREGISTER;
          r2.number:=r;
          list.concat(Tai_regalloc.alloc(r2));
          getexplicitregisterint:=r2;
          exit;
        end;
      result:=inherited getexplicitregisterint(list,r);
    end;
{$endif newra}


    function trgcpu.getregisterfpu(list: taasmoutput;size: TCGSize) : tregister;

      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result.enum := R_ST;
      end;


    procedure trgcpu.ungetregisterfpu(list : taasmoutput; r : tregister;size:TCGSize);

      begin
        { nothing to do, fpu stack management is handled by the load/ }
        { store operations in cgcpu (JM)                              }
      end;


    procedure trgcpu.ungetreference(list: taasmoutput; const ref : treference);

      begin
         if (ref.base.number<>NR_NO) and (ref.base.number<>NR_FRAME_POINTER_REG) then
           ungetregisterint(list,ref.base);
         if (ref.index.number<>NR_NO) and (ref.index.number<>NR_FRAME_POINTER_REG) then
           ungetregisterint(list,ref.index);
      end;

{$ifndef newra}
    procedure trgcpu.pushusedintregisters(list:Taasmoutput;
                                         var pushed:Tpushedsavedint;
                                         const s:Tsupregset);

    var r:Tsuperregister;
        r2:Tregister;

    begin
      usedintinproc:=usedintinproc+s;
      for r:=firstsaveintreg to lastsaveintreg do
        begin
          r2.enum:=R_INTREGISTER;
          r2.number:=r shl 8 or R_SUBWHOLE;
          pushed[r].pushed:=false;
          { if the register is used by the calling subroutine    }
          if not(r in is_reg_var_int) and (r in s) and
             { and is present in use }
             not(r in unusedregsint) then
            begin
              { then save it }
              list.concat(Taicpu.Op_reg(A_PUSH,S_L,r2));
              include(unusedregsint,r);
            {$ifndef newra}
              inc(countunusedregsint);
            {$endif}
              pushed[r].pushed:=true;
            end;
        end;
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;
{$endif}

{$ifdef SUPPORT_MMX}
    procedure trgcpu.pushusedotherregisters(list:Taasmoutput;
                                            var pushed:Tpushedsavedother;
                                            const s:Tregisterset);

    var r:Toldregister;
        r2:Tregister;
        hr:Treference;

    begin
      usedinproc:=usedinproc+s;
      for r:=R_MM0 to R_MM6 do
        begin
          pushed[r].pushed:=false;
          { if the register is used by the calling subroutine    }
          if not is_reg_var_other[r] and
             (r in s) and
             { and is present in use }
             not(r in unusedregsmm) then
            begin
              r2.enum:=R_ESP;
              list.concat(Taicpu.Op_const_reg(A_SUB,S_L,8,r2));
              reference_reset_base(hr,r2,0);
              r2.enum:=r;
              list.concat(Taicpu.Op_reg_ref(A_MOVQ,S_NO,r2,hr));
              include(unusedregsmm,r);
            {$ifndef newra}
              inc(countunusedregsmm);
            {$endif}
              pushed[r].pushed:=true;
            end;
        end;
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;
{$endif SUPPORT_MMX}

{$ifndef newra}
    procedure trgcpu.popusedintregisters(list:Taasmoutput;
                                         const pushed:Tpushedsavedint);

    var r:Tsuperregister;
        r2:Tregister;
    begin
      { restore in reverse order: }
      for r:=lastsaveintreg downto firstsaveintreg do
        if pushed[r].pushed then
          begin
            r2.enum:=R_INTREGISTER;
            r2.number:=r shl 8 or R_SUBWHOLE;
            list.concat(Taicpu.op_reg(A_POP,S_L,r2));
            if not (r in unusedregsint) then
              { internalerror(10)
                in cg386cal we always restore regs
                that appear as used
                due to a unused tmep storage PM }
            else
              {$ifndef newra}dec(countunusedregsint){$endif};
            exclude(unusedregsint,r);
          end;
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;
{$endif}

{$ifdef SUPPORT_MMX}
    procedure trgcpu.popusedotherregisters(list:Taasmoutput;
                                           const pushed:Tpushedsavedother);

    var r:Toldregister;
        r2,r3:Tregister;
        hr:Treference;

    begin
      { restore in reverse order: }
      for r:=R_MM6 downto R_MM0 do
        if pushed[r].pushed then
          begin
            r2.enum:=R_ESP;
            reference_reset_base(hr,r2,0);
            r3.enum:=r;
            list.concat(Taicpu.op_ref_reg(A_MOVQ,S_NO,hr,r3));
            list.concat(Taicpu.op_const_reg(A_ADD,S_L,8,r2));
            if not (r in unusedregsmm) then
              { internalerror(10)
                in cg386cal we always restore regs
                that appear as used
                due to a unused tmep storage PM }
            else
              {$ifndef newra}dec(countunusedregsmm){$endif};
            exclude(unusedregsmm,r);
          end;
{$ifdef TEMPREGDEBUG}
      testregisters;
{$endif TEMPREGDEBUG}
    end;
{$endif SUPPORT_MMX}

{$ifndef newra}
    procedure trgcpu.saveusedintregisters(list:Taasmoutput;
                                          var saved:Tpushedsavedint;
                                          const s:Tsupregset);

    begin
      if (aktoptprocessor in [class386,classP5]) or
         (CS_LittleSize in aktglobalswitches) then
        pushusedintregisters(list,saved,s)
      else
        inherited saveusedintregisters(list,saved,s);
    end;
{$endif}


    procedure trgcpu.saveusedotherregisters(list:Taasmoutput;var saved:Tpushedsavedother;
                                            const s:tregisterset);

    begin
{$ifdef SUPPORT_MMX}
      if (aktoptprocessor in [class386,classP5]) or
         (CS_LittleSize in aktglobalswitches) then
        pushusedotherregisters(list,saved,s)
      else
{$endif SUPPORT_MMX}
        inherited saveusedotherregisters(list,saved,s);
    end;

{$ifndef newra}
    procedure trgcpu.restoreusedintregisters(list:Taasmoutput;
                                             const saved:tpushedsavedint);

    begin
      if (aktoptprocessor in [class386,classP5]) or
         (CS_LittleSize in aktglobalswitches) then
        popusedintregisters(list,saved)
      else
        inherited restoreusedintregisters(list,saved);
    end;
{$endif}

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
        correct_fpuregister.enum:=Toldregister(longint(r.enum)+ofs);
     end;


    function trgcpu.makeregsize(reg: tregister; size: tcgsize): tregister;
      begin
        if reg.enum<>R_INTREGISTER then
          internalerror(200306032);
        result.enum:=R_INTREGISTER;
        result.number:=(reg.number and (not $ff)) or cgsize2subreg(size);
      end;



initialization
  rg := trgcpu.create(7);   {We use 7 int registers on i386.}
end.

{
  $Log$
  Revision 1.26  2003-06-12 21:12:20  peter
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

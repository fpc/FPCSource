{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for in set/case nodes

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
unit n386set;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      node,nset,pass_1,ncgset;

    type
      ti386casenode = class(tcgcasenode)
         procedure optimizevalues(var max_linear_list:aint;var max_dist:aword);override;
         function  has_jumptable : boolean;override;
         procedure genjumptable(hp : pcaserecord;min_,max_ : aint);override;
         procedure genlinearlist(hp : pcaserecord);override;
      end;


implementation

    uses
      systems,
      verbose,globals,
      symconst,symdef,defutil,
      aasmbase,aasmtai,aasmcpu,
      cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,procinfo,
      cga,cgutils,cgobj,ncgutil,
      cgx86;


{*****************************************************************************
                            TI386CASENODE
*****************************************************************************}

    procedure ti386casenode.optimizevalues(var max_linear_list:aint;var max_dist:aword);
      begin
        { a jump table crashes the pipeline! }
        if aktoptprocessor=Class386 then
          inc(max_linear_list,3)
        else if aktoptprocessor=ClassPentium then
          inc(max_linear_list,6)
        else if aktoptprocessor in [ClassPentium2,ClassPentium3] then
          inc(max_linear_list,9)
        else if aktoptprocessor=ClassPentium4 then
          inc(max_linear_list,14);
      end;


    function ti386casenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure ti386casenode.genjumptable(hp : pcaserecord;min_,max_ : aint);
      var
        table : tasmlabel;
        last : TConstExprInt;
        indexreg : tregister;
        href : treference;
        jumpsegment : TAAsmOutput;

        procedure genitem(t : pcaserecord);
          var
            i : aint;
          begin
            if assigned(t^.less) then
              genitem(t^.less);
            { fill possible hole }
            for i:=last+1 to t^._low-1 do
              jumpSegment.concat(Tai_const.Create_sym(elselabel));
            for i:=t^._low to t^._high do
              jumpSegment.concat(Tai_const.Create_sym(t^.statement));
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(t^.greater);
          end;

      begin
        if (cs_create_smart in aktmoduleswitches) or
           (af_smartlink_sections in target_asm.flags) then
          jumpsegment:=current_procinfo.aktlocaldata
        else
          jumpsegment:=datasegment;
        if not(jumptable_no_range) then
          begin
             { case expr less than min_ => goto elselabel }
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_lt,aint(min_),hregister,elselabel);
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_gt,aint(max_),hregister,elselabel);
          end;
        objectlibrary.getlabel(table);
        { make it a 32bit register }
        indexreg:=cg.makeregsize(exprasmlist,hregister,OS_INT);
        cg.a_load_reg_reg(exprasmlist,opsize,OS_INT,hregister,indexreg);
        { create reference }
        reference_reset_symbol(href,table,0);
        href.offset:=(-aint(min_))*4;
        href.index:=indexreg;
        href.scalefactor:=4;
        emit_ref(A_JMP,S_NO,href);
        { generate jump table }
        if not(cs_littlesize in aktglobalswitches) then
          jumpSegment.concat(Tai_Align.Create_Op(4,0));
        jumpSegment.concat(Tai_label.Create(table));
        last:=min_;
        genitem(hp);
      end;


    procedure ti386casenode.genlinearlist(hp : pcaserecord);
      var
        first : boolean;
        lastrange : boolean;
        last : TConstExprInt;
        cond_lt,cond_le : tresflags;

        procedure genitem(t : pcaserecord);
          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { need we to test the first value }
             if first and (t^._low>get_min_value(left.resulttype.def)) then
               begin
                 cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_lt,aint(t^._low),hregister,elselabel);
               end;
             if t^._low=t^._high then
               begin
                  if t^._low-last=0 then
                    cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_EQ,0,hregister,t^.statement)
                  else
                    begin
                      cg.a_op_const_reg(exprasmlist, OP_SUB, opsize, aint(t^._low-last), hregister);
                      cg.a_jmp_flags(exprasmlist,F_E,t^.statement);
                    end;
                  last:=t^._low;
                  lastrange:=false;
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                }
                  if first then
                    begin
                       { have we to ajust the first value ? }
                       if (t^._low>get_min_value(left.resulttype.def)) then
                         cg.a_op_const_reg(exprasmlist, OP_SUB, opsize, aint(t^._low), hregister);
                    end
                  else
                    begin
                      { if there is no unused label between the last and the }
                      { present label then the lower limit can be checked    }
                      { immediately. else check the range in between:       }

                      cg.a_op_const_reg(exprasmlist, OP_SUB, opsize, aint(t^._low-last), hregister);
                      { no jump necessary here if the new range starts at }
                      { at the value following the previous one           }
                      if ((t^._low-last) <> 1) or
                         (not lastrange) then
                        cg.a_jmp_flags(exprasmlist,cond_lt,elselabel);
                    end;
                  {we need to use A_SUB, because A_DEC does not set the correct flags, therefor
                   using a_op_const_reg(OP_SUB) is not possible }
                  emit_const_reg(A_SUB,TCGSize2OpSize[opsize],aint(t^._high-t^._low),hregister);
                  cg.a_jmp_flags(exprasmlist,cond_le,t^.statement);
                  last:=t^._high;
                  lastrange:=true;
               end;
             first:=false;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        begin
           if with_sign then
             begin
                cond_lt:=F_L;
                cond_le:=F_LE;
             end
           else
              begin
                cond_lt:=F_B;
                cond_le:=F_BE;
             end;
           { do we need to generate cmps? }
           if (with_sign and (min_label<0)) then
             genlinearcmplist(hp)
           else
             begin
                last:=0;
                lastrange:=false;
                first:=true;
                genitem(hp);
                cg.a_jmp_always(exprasmlist,elselabel);
             end;
        end;

begin
   ccasenode:=ti386casenode;
end.
{
  $Log$
  Revision 1.75  2004-06-16 20:07:10  florian
    * dwarf branch merged

  Revision 1.74  2004/05/22 23:34:28  peter
  tai_regalloc.allocation changed to ratype to notify rgobj of register size changes

  Revision 1.73.2.5  2004/05/02 20:54:41  peter
    * compile fix

  Revision 1.73.2.4  2004/05/02 14:09:54  peter
    * fix case 64bit issues

  Revision 1.73.2.3  2004/04/29 23:30:28  peter
    * fix i386 compiler

  Revision 1.73.2.2  2004/04/12 14:45:11  peter
    * tai_const_symbol and tai_const merged

  Revision 1.73.2.1  2004/04/08 18:33:22  peter
    * rewrite of TAsmSection

  Revision 1.73  2004/02/27 10:21:05  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

  Revision 1.72  2004/02/22 12:04:04  florian
    + nx86set added
    * some more x86-64 fixes

  Revision 1.71  2004/02/03 22:32:54  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

  Revision 1.70  2003/11/07 15:58:32  florian
    * Florian's culmutative nr. 1; contains:
      - invalid calling conventions for a certain cpu are rejected
      - arm softfloat calling conventions
      - -Sp for cpu dependend code generation
      - several arm fixes
      - remaining code for value open array paras on heap

  Revision 1.69  2003/10/10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.68  2003/10/09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.67  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.66  2003/09/28 21:48:20  peter
    * fix register leaks

  Revision 1.65  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.64  2003/09/05 11:21:39  marco
   * applied Peter's patch. Now cycles.

  Revision 1.63  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.62.2.1  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.62  2003/06/12 22:10:44  jonas
    * t386innode.pass_2 already doesn't call a helper anymore since a long
      time

  Revision 1.61  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.60  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.59  2003/05/31 15:04:31  peter
    * load_loc_reg update

  Revision 1.58  2003/05/22 21:32:29  peter
    * removed some unit dependencies

  Revision 1.57  2003/04/27 11:21:35  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.56  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.55  2003/04/23 09:51:16  daniel
    * Removed usage of edi in a lot of places when new register allocator used
    + Added newra versions of g_concatcopy and secondadd_float

  Revision 1.54  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.53  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.52  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.51  2003/03/13 19:52:23  jonas
    * and more new register allocator fixes (in the i386 code generator this
      time). At least now the ppc cross compiler can compile the linux
      system unit again, but I haven't tested it.

  Revision 1.50  2003/02/26 23:06:13  daniel
    * Fixed an illegal use of makeregsize

  Revision 1.49  2003/02/19 22:39:56  daniel
    * Fixed a few issues

  Revision 1.48  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.47  2003/01/13 14:54:34  daniel
    * Further work to convert codegenerator register convention;
      internalerror bug fixed.

  Revision 1.46  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.45  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.44  2002/10/03 21:34:45  carl
    * range check error fixes

  Revision 1.43  2002/09/17 18:54:05  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.42  2002/09/16 18:08:26  peter
    * fix last optimization in genlinearlist, detected by bug tw1066
    * use generic casenode.pass2 routine and override genlinearlist
    * add jumptable support to generic casenode, by default there is
      no jumptable support

  Revision 1.41  2002/09/09 13:57:45  jonas
    * small optimization to case genlist() case statements

  Revision 1.40  2002/08/17 09:23:46  florian
    * first part of procinfo rewrite

  Revision 1.39  2002/08/12 15:08:42  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.38  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.37  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.36  2002/07/23 14:31:00  daniel
  * Added internal error when asked to generate code for 'if expr in []'

  Revision 1.35  2002/07/20 11:58:04  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.34  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.33  2002/07/06 20:27:26  carl
  + generic set handling

  Revision 1.32  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.31  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.30  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.28  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.27  2002/05/12 16:53:17  peter
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

  Revision 1.26  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.25  2002/04/21 19:02:07  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.24  2002/04/21 15:37:26  carl
  * changeregsize -> rg.makeregsize

  Revision 1.23  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.22  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.21  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.20  2002/03/31 20:26:39  jonas
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
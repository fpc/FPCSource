{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for nodes that influence the flow which are
    the same for all (most?) processors

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
unit ncgflw;

{$i fpcdefs.inc}

interface

    uses
      node,nflw;

    type
       tcgwhilerepeatnode = class(twhilerepeatnode)
          procedure pass_2;override;
       end;

       tcgifnode = class(tifnode)
          procedure pass_2;override;
       end;

       tcgfornode = class(tfornode)
          procedure pass_2;override;
       end;

       tcgexitnode = class(texitnode)
          procedure pass_2;override;
       end;

       tcgbreaknode = class(tbreaknode)
          procedure pass_2;override;
       end;

       tcgcontinuenode = class(tcontinuenode)
          procedure pass_2;override;
       end;

       tcggotonode = class(tgotonode)
          procedure pass_2;override;
       end;

       tcglabelnode = class(tlabelnode)
          procedure pass_2;override;
       end;

       tcgfailnode = class(tfailnode)
          procedure pass_2;override;
       end;

implementation

    uses
      verbose,globals,systems,globtype,
      symconst,symsym,aasm,types,
      cginfo,cgbase,pass_2,
      cpubase,cpuasm,cpuinfo,
      nld,ncon,
      tgobj,rgobj,
      ncgutil,
      regvars,cgobj,cgcpu,cg64f32;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure tcgwhilerepeatnode.pass_2;
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : tasmlabel;
         otlabel,oflabel : tasmlabel;

      begin
         getlabel(lloop);
         getlabel(lcont);
         getlabel(lbreak);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;

         load_all_regvars(exprasmlist);
         { handling code at the end as it is much more efficient, and makes
           while equal to repeat loop, only the end true/false is swapped (PFV) }
         if nodetype=whilen then
           cg.a_jmp_always(exprasmlist,lcont);

         { align loop target }
         exprasmList.concat(Tai_align.Create(aktalignment.loopalign));
         cg.a_label(exprasmlist,lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
         rg.cleartempgen;
         if assigned(right) then
           secondpass(right);

         load_all_regvars(exprasmlist);

         cg.a_label(exprasmlist,lcont);
         otlabel:=truelabel;
         oflabel:=falselabel;
         if nodetype=whilen then
          begin
            truelabel:=lloop;
            falselabel:=lbreak;
          end
         { repeatn }
         else
          begin
            truelabel:=lbreak;
            falselabel:=lloop;
          end;
         rg.cleartempgen;
         secondpass(left);

         maketojumpbool(exprasmlist,left,lr_load_regvars);
         cg.a_label(exprasmlist,lbreak);
         truelabel:=otlabel;
         falselabel:=oflabel;

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a while/repeat block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;


{*****************************************************************************
                               tcgIFNODE
*****************************************************************************}

    procedure tcgifnode.pass_2;

      var
         hl,otlabel,oflabel : tasmlabel;
         org_regvar_loaded,
         then_regvar_loaded,
         else_regvar_loaded : regvar_booleanarray;
         org_list,
         then_list,
         else_list : taasmoutput;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         rg.cleartempgen;
         secondpass(left);


         { save regvars loaded in the beginning so that we can restore them }
         { when processing the else-block                                   }
         if cs_regalloc in aktglobalswitches then
           begin
             org_list := exprasmlist;
             exprasmlist := taasmoutput.create;
           end;
         maketojumpbool(exprasmlist,left,lr_dont_load_regvars);

         if cs_regalloc in aktglobalswitches then
           org_regvar_loaded := rg.regvar_loaded;

         if assigned(right) then
           begin
              cg.a_label(exprasmlist,truelabel);
              rg.cleartempgen;
              secondpass(right);
           end;

         { save current asmlist (previous instructions + then-block) and }
         { loaded regvar state and create new clean ones                 }
         if cs_regalloc in aktglobalswitches then
           begin
             then_regvar_loaded := rg.regvar_loaded;
             rg.regvar_loaded := org_regvar_loaded;
             then_list := exprasmlist;
             exprasmlist := taasmoutput.create;
           end;

         if assigned(t1) then
           begin
              if assigned(right) then
                begin
                   getlabel(hl);
                   { do go back to if line !! }
                   if not(cs_regalloc in aktglobalswitches) then
                     aktfilepos:=exprasmList.getlasttaifilepos^
                   else
                     aktfilepos:=then_list.getlasttaifilepos^;
                   cg.a_jmp_always(exprasmlist,hl);
                end;
              cg.a_label(exprasmlist,falselabel);
              rg.cleartempgen;
              secondpass(t1);
              { save current asmlist (previous instructions + else-block) }
              { and loaded regvar state and create a new clean list       }
              if cs_regalloc in aktglobalswitches then
                begin
                  else_regvar_loaded := rg.regvar_loaded;
                  else_list := exprasmlist;
                  exprasmlist := taasmoutput.create;
                end;
              if assigned(right) then
                cg.a_label(exprasmlist,hl);
           end
         else
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                  else_regvar_loaded := rg.regvar_loaded;
                  else_list := exprasmlist;
                  exprasmlist := taasmoutput.create;
                end;
              cg.a_label(exprasmlist,falselabel);
           end;
         if not(assigned(right)) then
           begin
              cg.a_label(exprasmlist,truelabel);
           end;

         if cs_regalloc in aktglobalswitches then
           begin
             { add loads of regvars at the end of the then- and else-blocks  }
             { so that at the end of both blocks the same regvars are loaded }

             { no else block? }
             if not assigned(t1) then
               sync_regvars(org_list,then_list,org_regvar_loaded,
                 then_regvar_loaded)
             { no then block? }
             else if not assigned(right) then
               sync_regvars(org_list,else_list,org_regvar_loaded,
                 else_regvar_loaded)
             { both else and then blocks }
             else
               sync_regvars(then_list,else_list,then_regvar_loaded,
                 else_regvar_loaded);
             { add all lists together }
             org_list.concatlist(then_list);
             then_list.free;
             org_list.concatlist(else_list);
             else_list.free;
             org_list.concatlist(exprasmlist);
             exprasmlist.free;
             exprasmlist := org_list;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure tcgfornode.pass_2;
      var
         l3,oldclabel,oldblabel : tasmlabel;
         omitfirstcomp,temptovalue : boolean;
         hs : byte;
         temp1 : treference;
         hop : topcg;
         hcond : topcmp;
         opsize : tcgsize;
         count_var_is_signed : boolean;

      begin
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         getlabel(aktcontinuelabel);
         getlabel(aktbreaklabel);
         getlabel(l3);

         { could we spare the first comparison ? }
         omitfirstcomp:=false;
         if right.nodetype=ordconstn then
           if tassignmentnode(left).right.nodetype=ordconstn then
             omitfirstcomp:=((nf_backward in flags) and
               (tordconstnode(tassignmentnode(left).right).value>=tordconstnode(right).value))
               or (not(nf_backward in flags) and
                  (tordconstnode(tassignmentnode(left).right).value<=tordconstnode(right).value));

         { only calculate reference }
         rg.cleartempgen;
         secondpass(t2);
         hs := t2.resulttype.def.size;
         opsize := def_cgsize(t2.resulttype.def);

         { first set the to value
           because the count var can be in the expression !! }
         rg.cleartempgen;
         secondpass(right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                       }
         if right.nodetype<>ordconstn then
           begin
              temp1.symbol:=nil;
              tg.gettempofsizereference(exprasmlist,hs,temp1);
              temptovalue:=true;
              if (right.location.loc=LOC_REGISTER) or
                 (right.location.loc=LOC_CREGISTER) then
                begin
                   cg.a_load_reg_ref(exprasmlist,opsize,
                     right.location.register,temp1);
                   rg.ungetregister(exprasmlist,right.location.register);
                 end
              else
                cg.g_concatcopy(exprasmlist,right.location.reference,temp1,
                  hs,true,false);
           end
         else
           temptovalue:=false;

         { produce start assignment }
         rg.cleartempgen;
         secondpass(left);
         count_var_is_signed:=is_signed(t2.resulttype.def);

         if nf_backward in flags then
           if count_var_is_signed then
             hcond:=OC_LT
           else
             hcond:=OC_B
         else
           if count_var_is_signed then
             hcond:=OC_GT
           else
             hcond:=OC_A;

         load_all_regvars(exprasmlist);

         if temptovalue then
           begin
             cg.a_cmp_ref_loc_label(exprasmlist,opsize,hcond,
               temp1,t2.location,aktbreaklabel);
           end
         else
           begin
             if not(omitfirstcomp) then
               begin
                 cg.a_cmp_const_loc_label(exprasmlist,opsize,hcond,
                   aword(tordconstnode(right).value),
                   t2.location,aktbreaklabel);
               end;
           end;

         { align loop target }
         exprasmList.concat(Tai_align.Create(aktalignment.loopalign));
         cg.a_label(exprasmlist,l3);

         { help register must not be in instruction block }
         rg.cleartempgen;
         if assigned(t1) then
           begin
             secondpass(t1);
             load_all_regvars(exprasmlist);
           end;

         cg.a_label(exprasmlist,aktcontinuelabel);

         { makes no problems there }
         rg.cleartempgen;

         if nf_backward in flags then
           if count_var_is_signed then
             hcond:=OC_LTE
           else
             hcond:=OC_BE
          else
            if count_var_is_signed then
              hcond:=OC_GTE
            else
              hcond:=OC_AE;
         load_all_regvars(exprasmlist);

         { produce comparison and the corresponding }
         { jump                                     }
         if temptovalue then
           begin
             cg.a_cmp_ref_loc_label(exprasmlist,opsize,hcond,temp1,
               t2.location,aktbreaklabel);
           end
         else
           begin
             cg.a_cmp_const_loc_label(exprasmlist,opsize,hcond,
               aword(tordconstnode(right).value),t2.location,aktbreaklabel);
           end;
         { according to count direction DEC or INC... }
         { must be after the test because of 0 to 255 for bytes !! }
         if nf_backward in flags then
           hop:=OP_SUB
         else
           hop:=OP_ADD;
         cg.a_op_const_loc(exprasmlist,hop,1,t2.location);
         cg.a_jmp_always(exprasmlist,l3);

         if temptovalue then
           tg.ungetiftemp(exprasmlist,temp1);

         { this is the break label: }
         cg.a_label(exprasmlist,aktbreaklabel);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a for block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;


{*****************************************************************************
                              SecondExitN
*****************************************************************************}

    procedure tcgexitnode.pass_2;

      var
         {op : tasmop;
         s : topsize;}
         otlabel,oflabel : tasmlabel;
         cgsize : tcgsize;
         hreg : tregister;
         allocated_acc,
         allocated_acchigh: boolean;
      label
         do_jmp;
      begin
{         load_all_regvars(exprasmlist); }
         include(flowcontrol,fc_exit);
         if assigned(left) then
         if left.nodetype=assignn then
           begin
              { just do a normal assignment followed by exit }
              secondpass(left);
              cg.a_jmp_always(exprasmlist,aktexitlabel);
           end
         else
           begin
              allocated_acc := false;
              allocated_acchigh := false;
              otlabel:=truelabel;
              oflabel:=falselabel;
              getlabel(truelabel);
              getlabel(falselabel);
              secondpass(left);
              { the result of left is not needed anymore after this
                node }
              location_freetemp(exprasmlist,left.location);
              location_release(exprasmlist,left.location);
              case left.location.loc of
                LOC_FPUREGISTER :
                  goto do_jmp;
                LOC_FLAGS :
                  begin
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    allocated_acc := true;
                    cg.g_flags2reg(exprasmlist,left.location.resflags,accumulator);
                    goto do_jmp;
                  end;
                LOC_JUMP :
                  begin
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    { get an 8-bit register }
                    hreg:=rg.makeregsize(accumulator,OS_8);
                    allocated_acc := true;
                    cg.a_label(exprasmlist,truelabel);
                    cg.a_load_const_reg(exprasmlist,OS_8,1,hreg);
                    cg.a_jmp_always(exprasmlist,aktexit2label);
                    cg.a_label(exprasmlist,falselabel);
                    cg.a_load_const_reg(exprasmlist,OS_8,0,hreg);
                    goto do_jmp;
                  end;
              end;
              case aktprocdef.rettype.def.deftype of
                pointerdef,
                procvardef :
                  begin
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    allocated_acc := true;
                    cg.a_load_loc_reg(exprasmlist,left.location,accumulator);
                  end;
                floatdef :
                  begin
{$ifndef i386}
                    cg.a_reg_alloc(exprasmlist,fpuresultreg);
{$endif not i386}
                    cg.a_loadfpu_loc_reg(exprasmlist,left.location,fpuresultreg);
                  end;
                else
                  begin
                    cgsize:=def_cgsize(aktprocdef.rettype.def);
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    allocated_acc := true;
                    case cgsize of
                      OS_64,OS_S64 :
                        begin
                          cg.a_reg_alloc(exprasmlist,accumulatorhigh);
                          allocated_acchigh := true;
                          cg64.a_load64_loc_reg(exprasmlist,left.location,
                              joinreg64(accumulator,accumulatorhigh));
                        end
                      else
                        begin
                          hreg:=rg.makeregsize(accumulator,cgsize);
                          cg.a_load_loc_reg(exprasmlist,left.location,hreg);
                        end;
                    end;
                 end;
              end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              cg.a_jmp_always(exprasmlist,aktexit2label);
              if allocated_acc then
                cg.a_reg_dealloc(exprasmlist,accumulator);
              if allocated_acchigh then
                cg.a_reg_dealloc(exprasmlist,accumulatorhigh);
{$ifndef i386}
              if (aktprocdef.rettype.def.deftype = floatdef) then
                cg.a_reg_dealloc(exprasmlist,fpuresultreg);
{$endif not i386}
           end
         else
           cg.a_jmp_always(exprasmlist,aktexitlabel);
       end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure tcgbreaknode.pass_2;
      begin
         include(flowcontrol,fc_break);
         if aktbreaklabel<>nil then
           begin
             load_all_regvars(exprasmlist);
             cg.a_jmp_always(exprasmlist,aktbreaklabel)
           end
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure tcgcontinuenode.pass_2;
      begin
         include(flowcontrol,fc_continue);
         if aktcontinuelabel<>nil then
           begin
             load_all_regvars(exprasmlist);
             cg.a_jmp_always(exprasmlist,aktcontinuelabel)
           end
         else
           CGMessage(cg_e_continue_not_allowed);
      end;


{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure tcggotonode.pass_2;

       begin
         load_all_regvars(exprasmlist);
         cg.a_jmp_always(exprasmlist,labelnr)
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure tcglabelnode.pass_2;
      begin
         load_all_regvars(exprasmlist);
         cg.a_label(exprasmlist,labelnr);
         rg.cleartempgen;
         secondpass(left);
      end;


{*****************************************************************************
                             SecondFail
*****************************************************************************}

    procedure tcgfailnode.pass_2;
      begin
        cg.a_jmp_always(exprasmlist,faillabel);
      end;




begin
   cwhilerepeatnode:=tcgwhilerepeatnode;
   cifnode:=tcgifnode;
   cfornode:=tcgfornode;
   cexitnode:=tcgexitnode;
   cbreaknode:=tcgbreaknode;
   ccontinuenode:=tcgcontinuenode;
   cgotonode:=tcggotonode;
   clabelnode:=tcglabelnode;
   cfailnode:=tcgfailnode;
end.
{
  $Log$
  Revision 1.20  2002-07-01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.19  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.18  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.17  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.15  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.14  2002/05/12 16:53:07  peter
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

  Revision 1.13  2002/04/21 15:24:38  carl
  + a_jmp_cond -> a_jmp_always (a_jmp_cond is NOT portable)
  + changeregsize -> rg.makeregsize

  Revision 1.12  2002/04/15 19:44:19  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.11  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.10  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.9  2002/03/31 20:26:34  jonas
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

  Revision 1.8  2002/03/04 19:10:11  peter
    * removed compiler warnings

}


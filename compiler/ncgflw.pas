{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

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

implementation

    uses
      verbose,globals,systems,globtype,
      symconst,symdef,symsym,aasm,types,
      cgbase,temp_gen,pass_2,
      cpubase,cpuasm,cpuinfo,
      nld,ncon,
      cga,tgcpu,
      ncgutil,
      tainst,regvars,cgobj,cgcpu;

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
           cg.a_jmp_cond(exprasmlist,OC_None,lcont);

         { align loop target }
         exprasmList.concat(Tai_align.Create(aktalignment.loopalign));
         cg.a_label(exprasmlist,lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
         cleartempgen;
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
         cleartempgen;
         secondpass(left);

         maketojumpbool(left,lr_load_regvars);
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
         cleartempgen;
         secondpass(left);


         { save regvars loaded in the beginning so that we can restore them }
         { when processing the else-block                                   }
         if cs_regalloc in aktglobalswitches then
           begin
             org_list := exprasmlist;
             exprasmlist := taasmoutput.create;
           end;
         maketojumpbool(left,lr_dont_load_regvars);

         if cs_regalloc in aktglobalswitches then
           org_regvar_loaded := regvar_loaded;

         if assigned(right) then
           begin
              cg.a_label(exprasmlist,truelabel);
              cleartempgen;
              secondpass(right);
           end;

         { save current asmlist (previous instructions + then-block) and }
         { loaded regvar state and create new clean ones                 }
         if cs_regalloc in aktglobalswitches then
           begin
             then_regvar_loaded := regvar_loaded;
             regvar_loaded := org_regvar_loaded;
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
                   cg.a_jmp_cond(exprasmlist,OC_None,hl);
                end;
              cg.a_label(exprasmlist,falselabel);
              cleartempgen;
              secondpass(t1);
              { save current asmlist (previous instructions + else-block) }
              { and loaded regvar state and create a new clean list       }
              if cs_regalloc in aktglobalswitches then
                begin
                  else_regvar_loaded := regvar_loaded;
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
                  else_regvar_loaded := regvar_loaded;
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
         cleartempgen;
         secondpass(t2);
         hs := t2.resulttype.def.size;
         opsize := def_cgsize(t2.resulttype.def);

         { first set the to value
           because the count var can be in the expression !! }
         cleartempgen;
         secondpass(right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                       }
         if right.nodetype<>ordconstn then
           begin
              temp1.symbol:=nil;
              gettempofsizereference(hs,temp1);
              temptovalue:=true;
              if (right.location.loc=LOC_REGISTER) or
                 (right.location.loc=LOC_CREGISTER) then
                begin
                   cg.a_load_reg_ref(exprasmlist,opsize,
                     right.location.register,temp1);
                   ungetregister(right.location.register);
                 end
              else
                cg.g_concatcopy(exprasmlist,right.location.reference,temp1,
                  hs,true,false);
           end
         else
           temptovalue:=false;

         { produce start assignment }
         cleartempgen;
         secondpass(left);
         count_var_is_signed:=is_signed(torddef(t2.resulttype.def));

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
         cleartempgen;
         if assigned(t1) then
           begin
             secondpass(t1);
             load_all_regvars(exprasmlist);
           end;

         cg.a_label(exprasmlist,aktcontinuelabel);

         { makes no problems there }
         cleartempgen;

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
         cg.a_op_const_loc(exprasmlist,hop,opsize,1,t2.location);
         cg.a_jmp_cond(exprasmlist,OC_None,l3);

         if temptovalue then
           ungetiftemp(temp1);

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
         r : treference;
         is_mem,
         allocated_acc,
         allocated_acchigh: boolean;

      procedure cleanleft;
        begin
          if is_mem then
            begin
              del_reference(left.location.reference);
              ungetiftemp(left.location.reference);
            end
          else
            begin
              ungetregister(left.location.register);
              if left.location.registerhigh <> R_NO then
                ungetregister(left.location.registerhigh);
            end;
        end;

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
              cg.a_jmp_cond(exprasmlist,OC_NONE,aktexitlabel);
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
              case left.location.loc of
                 LOC_FPU : goto do_jmp;
                 LOC_MEM,
           LOC_REFERENCE : is_mem:=true;
           LOC_CREGISTER,
            LOC_REGISTER : is_mem:=false;
               LOC_FLAGS : begin
                             cg.a_reg_alloc(exprasmlist,accumulator);
                             allocated_acc := true;
                             cg.g_flags2reg(exprasmlist,left.location.resflags,accumulator);
                             goto do_jmp;
                           end;
                LOC_JUMP : begin
                             exprasmlist.concat(tairegalloc.alloc(accumulator));
                             allocated_acc := true;
                             cg.a_label(exprasmlist,truelabel);
                             cg.a_load_const_reg(exprasmlist,OS_8,1,
                               makereg8(accumulator));
                             cg.a_jmp_cond(exprasmlist,OC_NONE,aktexit2label);
                             cg.a_label(exprasmlist,falselabel);
                             cg.a_load_const_reg(exprasmlist,OS_8,0,
                               makereg8(accumulator));
                             goto do_jmp;
                           end;
              else
                internalerror(2001);
              end;
              case aktprocdef.rettype.def.deftype of
           pointerdef,
           procvardef : begin
                          cleanleft;
                          cg.a_reg_alloc(exprasmlist,accumulator);
                          allocated_acc := true;
                          if is_mem then
                            cg.a_load_ref_reg(exprasmlist,OS_ADDR,
                              left.location.reference,accumulator)
                          else
                            cg.a_load_reg_reg(exprasmlist,OS_ADDR,
                              left.location.register,accumulator);
                        end;
             floatdef : begin
                          cleanleft;
                          if is_mem then
                           floatload(tfloatdef(aktprocdef.rettype.def).typ,left.location.reference);
                        end;
              { orddef,
              enumdef : }
              else
              { it can be anything shorter than 4 bytes PM
              this caused form bug 711 }
                begin
                   cleanleft;
                   cg.a_reg_alloc(exprasmlist,accumulator);
                   allocated_acc := true;
                   case aktprocdef.rettype.def.size of
                    { it can be a qword/int64 too ... }
                    8 :
                      if is_mem then
                        begin
                           cg.a_load_ref_reg(exprasmlist,OS_32,
                             left.location.reference,accumulator);
                           r:=left.location.reference;
                           inc(r.offset,4);
                           cg.a_reg_alloc(exprasmlist,accumulatorhigh);
                           allocated_acchigh := true;
                           cg.a_load_ref_reg(exprasmlist,OS_32,r,accumulatorhigh);
                        end
                      else
                        begin
                           cg.a_load_reg_reg(exprasmlist,OS_32,left.location.registerlow,accumulator);
                           cg.a_reg_alloc(exprasmlist,accumulatorhigh);
                           allocated_acchigh := true;
                           cg.a_load_reg_reg(exprasmlist,OS_32,left.location.registerhigh,accumulatorhigh);
                        end;
                   { if its 3 bytes only we can still
                     copy one of garbage ! PM }
                    4,3 :
                      cg.a_load_loc_reg(exprasmlist,OS_32,left.location,
                        accumulator);
                    2 :
                      cg.a_load_loc_reg(exprasmlist,OS_16,left.location,
                        makereg16(accumulator));
                    1 :
                      cg.a_load_loc_reg(exprasmlist,OS_8,left.location,
                        makereg8(accumulator));
                    else internalerror(605001);
                   end;
                 end;
              end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              cg.a_jmp_cond(exprasmlist,OC_None,aktexit2label);
              if allocated_acc then
                cg.a_reg_dealloc(exprasmlist,accumulator);
              if allocated_acchigh then
                cg.a_reg_dealloc(exprasmlist,accumulator);
           end
         else
            cg.a_jmp_cond(exprasmlist,OC_None,aktexitlabel);
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
             cg.a_jmp_cond(exprasmlist,OC_None,aktbreaklabel)
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
             cg.a_jmp_cond(exprasmlist,OC_None,aktcontinuelabel)
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
         cg.a_jmp_cond(exprasmlist,OC_None,labelnr)
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure tcglabelnode.pass_2;
      begin
         load_all_regvars(exprasmlist);
         cg.a_label(exprasmlist,labelnr);
         cleartempgen;
         secondpass(left);
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
end.
{
  $Log$
  Revision 1.8  2002-03-04 19:10:11  peter
    * removed compiler warnings

  Revision 1.7  2001/12/30 17:24:48  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.6  2001/12/29 15:28:57  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.4  2001/11/02 22:58:01  peter
    * procsym definition rewrite

  Revision 1.3  2001/10/04 14:33:28  jonas
    * fixed range check errors

  Revision 1.2  2001/09/30 16:19:58  jonas
    - removed unused units

  Revision 1.1  2001/09/28 20:39:33  jonas
    * changed all flow control structures (except for exception handling
      related things) to processor independent code (in new ncgflw unit)
    + generic cgobj unit which contains lots of code generator helpers with
      global "cg" class instance variable
    + cgcpu unit for i386 (implements processor specific routines of the above
      unit)
    * updated cgbase and cpubase for the new code generator units
    * include ncgflw unit in cpunode unit

  Revision 1.4  2001/09/09 17:10:25  jonas
    * some more things implemented

  Revision 1.3  2001/09/06 15:25:55  jonas
    * changed type of tcg from object to class ->  abstract methods are now
      a lot cleaner :)
    + more updates: load_*_loc methods, op_*_* methods, g_flags2reg method
      (if possible with geenric implementation and necessary ppc
       implementations)
    * worked a bit further on cgflw, now working on exitnode

  Revision 1.2  2001/09/05 20:21:03  jonas
    * new cgflow based on n386flw with all nodes until forn "translated"
    + a_cmp_*_loc_label methods for tcg
    + base implementatino for a_cmp_ref_*_label methods
    * small bugfixes to powerpc cg


}


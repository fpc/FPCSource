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

       tcgraisenode = class(traisenode)
          procedure pass_2;override;
       end;

       tcgtryexceptnode = class(ttryexceptnode)
          procedure pass_2;override;
       end;

       tcgtryfinallynode = class(ttryfinallynode)
          procedure pass_2;override;
       end;

       tcgonnode = class(tonnode)
          procedure pass_2;override;
       end;



implementation

    uses
      verbose,globals,systems,globtype,
      symconst,symsym,aasmbase,aasmtai,aasmcpu,defutil,
      cginfo,cgbase,pass_2,
      cpubase,cpuinfo,
      nld,ncon,
      ncgutil,
      tgobj,rgobj,paramgr,
      regvars,cgobj
{$ifndef cpu64bit}
      ,cg64f32
{$endif cpu64bit}
      ;

    const
      EXCEPT_BUF_SIZE = 12;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure tcgwhilerepeatnode.pass_2;
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : tasmlabel;
         otlabel,oflabel : tasmlabel;

      begin
         location_reset(location,LOC_VOID,OS_NO);

         objectlibrary.getlabel(lloop);
         objectlibrary.getlabel(lcont);
         objectlibrary.getlabel(lbreak);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;

         load_all_regvars(exprasmlist);
         { handling code at the end as it is much more efficient, and makes
           while equal to repeat loop, only the end true/false is swapped (PFV) }
         if lnf_testatbegin in loopflags then
           cg.a_jmp_always(exprasmlist,lcont);

         if not(cs_littlesize in aktglobalswitches) then
            { align loop target }
            exprasmList.concat(Tai_align.Create(aktalignment.loopalign));

         cg.a_label(exprasmlist,lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}
         if assigned(right) then
           secondpass(right);

         load_all_regvars(exprasmlist);

         cg.a_label(exprasmlist,lcont);
         otlabel:=truelabel;
         oflabel:=falselabel;
         if lnf_checknegate in loopflags then
          begin
            truelabel:=lbreak;
            falselabel:=lloop;
          end
         else
          begin
            truelabel:=lloop;
            falselabel:=lbreak;
          end;
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}
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
{$ifdef i386}
         org_regvar_loaded_other,
         then_regvar_loaded_other,
         else_regvar_loaded_other : regvarother_booleanarray;
         org_regvar_loaded_int,
         then_regvar_loaded_int,
         else_regvar_loaded_int : Tsupregset;
         org_list,
         then_list,
         else_list : taasmoutput;
{$endif i386}

      begin
         location_reset(location,LOC_VOID,OS_NO);

         otlabel:=truelabel;
         oflabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}
         secondpass(left);

{$ifdef i386}
         { save regvars loaded in the beginning so that we can restore them }
         { when processing the else-block                                   }
         if cs_regalloc in aktglobalswitches then
           begin
             org_list := exprasmlist;
             exprasmlist := taasmoutput.create;
           end;
{$endif i386}
         maketojumpbool(exprasmlist,left,lr_dont_load_regvars);

{$ifdef i386}
         if cs_regalloc in aktglobalswitches then
           begin
             org_regvar_loaded_int := rg.regvar_loaded_int;
             org_regvar_loaded_other := rg.regvar_loaded_other;
           end;
{$endif i386}

         if assigned(right) then
           begin
              cg.a_label(exprasmlist,truelabel);
           {$ifndef newra}
              rg.cleartempgen;
           {$endif}
              secondpass(right);
           end;

{$ifdef i386}
         { save current asmlist (previous instructions + then-block) and }
         { loaded regvar state and create new clean ones                 }
         if cs_regalloc in aktglobalswitches then
           begin
             then_regvar_loaded_int := rg.regvar_loaded_int;
             then_regvar_loaded_other := rg.regvar_loaded_other;
             rg.regvar_loaded_int := org_regvar_loaded_int;
             rg.regvar_loaded_other := org_regvar_loaded_other;
             then_list := exprasmlist;
             exprasmlist := taasmoutput.create;
           end;
{$endif i386}

         if assigned(t1) then
           begin
              if assigned(right) then
                begin
                   objectlibrary.getlabel(hl);
                   { do go back to if line !! }
{$ifdef i386}
                   if not(cs_regalloc in aktglobalswitches) then
{$endif i386}
                     aktfilepos:=exprasmList.getlasttaifilepos^
{$ifdef i386}
                   else
                     aktfilepos:=then_list.getlasttaifilepos^
{$endif i386}
                   ;
                   cg.a_jmp_always(exprasmlist,hl);
                end;
              cg.a_label(exprasmlist,falselabel);
            {$ifndef newra}
              rg.cleartempgen;
            {$endif}
              secondpass(t1);
{$ifdef i386}
              { save current asmlist (previous instructions + else-block) }
              { and loaded regvar state and create a new clean list       }
              if cs_regalloc in aktglobalswitches then
                begin
                  else_regvar_loaded_int := rg.regvar_loaded_int;
                  else_regvar_loaded_other := rg.regvar_loaded_other;
                  else_list := exprasmlist;
                  exprasmlist := taasmoutput.create;
                end;
{$endif i386}
              if assigned(right) then
                cg.a_label(exprasmlist,hl);
           end
         else
           begin
{$ifdef i386}
              if cs_regalloc in aktglobalswitches then
                begin
                  else_regvar_loaded_int := rg.regvar_loaded_int;
                  else_regvar_loaded_other := rg.regvar_loaded_other;
                  else_list := exprasmlist;
                  exprasmlist := taasmoutput.create;
                end;
{$endif i386}
              cg.a_label(exprasmlist,falselabel);
           end;
         if not(assigned(right)) then
           begin
              cg.a_label(exprasmlist,truelabel);
           end;

{$ifdef i386}
         if cs_regalloc in aktglobalswitches then
           begin
             { add loads of regvars at the end of the then- and else-blocks  }
             { so that at the end of both blocks the same regvars are loaded }

             { no else block? }
             if not assigned(t1) then
               begin
                 sync_regvars_int(org_list,then_list,org_regvar_loaded_int,then_regvar_loaded_int);
                 sync_regvars_other(org_list,then_list,org_regvar_loaded_other,then_regvar_loaded_other);
               end
             { no then block? }
             else if not assigned(right) then
               begin
                 sync_regvars_int(org_list,else_list,org_regvar_loaded_int,else_regvar_loaded_int);
                 sync_regvars_other(org_list,else_list,org_regvar_loaded_other,else_regvar_loaded_other);
               end
             { both else and then blocks }
             else
               begin
                 sync_regvars_int(then_list,else_list,then_regvar_loaded_int,else_regvar_loaded_int);
                 sync_regvars_other(then_list,else_list,then_regvar_loaded_other,else_regvar_loaded_other);
               end;
             { add all lists together }
             org_list.concatlist(then_list);
             then_list.free;
             org_list.concatlist(else_list);
             else_list.free;
             org_list.concatlist(exprasmlist);
             exprasmlist.free;
             exprasmlist := org_list;
           end;
{$endif i386}
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure tcgfornode.pass_2;
      var
         l3,oldclabel,oldblabel : tasmlabel;
         temptovalue : boolean;
         hs : byte;
         temp1 : treference;
         hop : topcg;
         hcond : topcmp;
         opsize : tcgsize;
         count_var_is_signed,do_loopvar_at_end : boolean;
         cmp_const:Tconstexprint;

      begin
         location_reset(location,LOC_VOID,OS_NO);

         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         objectlibrary.getlabel(aktcontinuelabel);
         objectlibrary.getlabel(aktbreaklabel);
         objectlibrary.getlabel(l3);

         { only calculate reference }
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}
         secondpass(t2);
         hs := t2.resulttype.def.size;
         opsize := def_cgsize(t2.resulttype.def);

         { first set the to value
           because the count var can be in the expression !! }
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}

         do_loopvar_at_end:=lnf_dont_mind_loopvar_on_exit in loopflags;

         secondpass(right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                       }
         if right.nodetype<>ordconstn then
           begin
              do_loopvar_at_end:=false;
              tg.GetTemp(exprasmlist,hs,tt_normal,temp1);
              temptovalue:=true;
              if (right.location.loc=LOC_REGISTER) or
                 (right.location.loc=LOC_CREGISTER) then
                begin
                   cg.a_load_reg_ref(exprasmlist,opsize,opsize,right.location.register,temp1);
                   rg.ungetregisterint(exprasmlist,right.location.register);
                 end
              else
                cg.g_concatcopy(exprasmlist,right.location.reference,temp1,
                  hs,true,false);
           end
         else
           temptovalue:=false;

         { produce start assignment }
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}
         secondpass(left);
         count_var_is_signed:=is_signed(t2.resulttype.def);

         if lnf_backward in loopflags then
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
             if lnf_testatbegin in loopflags then
               begin
                 cg.a_cmp_const_loc_label(exprasmlist,opsize,hcond,
                   aword(tordconstnode(right).value),
                   t2.location,aktbreaklabel);
               end;
           end;

         {If the loopvar doesn't mind on exit, we avoid this ugly
          dec instruction and do the loopvar inc/dec after the loop
          body.}
         if not do_loopvar_at_end then
            begin
              if lnf_backward in loopflags then
                hop:=OP_ADD
              else
                hop:=OP_SUB;
              cg.a_op_const_loc(exprasmlist,hop,1,t2.location);
            end;

         if not(cs_littlesize in aktglobalswitches) then
            { align loop target }
            exprasmList.concat(Tai_align.Create(aktalignment.loopalign));
         cg.a_label(exprasmlist,l3);

         {If the loopvar doesn't mind on exit, we avoid the loopvar inc/dec
          after the loop body instead of here.}
         if not do_loopvar_at_end then
            begin
              { according to count direction DEC or INC... }
              if lnf_backward in loopflags then
                hop:=OP_SUB
              else
                hop:=OP_ADD;
              cg.a_op_const_loc(exprasmlist,hop,1,t2.location);
            end;

         { help register must not be in instruction block }
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}
         if assigned(t1) then
           begin
             secondpass(t1);
             load_all_regvars(exprasmlist);
           end;

         {If the loopvar doesn't mind on exit, we do the loopvar inc/dec
          after the loop body instead of here.}
         if do_loopvar_at_end then
            begin
              { according to count direction DEC or INC... }
              if lnf_backward in loopflags then
                hop:=OP_SUB
              else
                hop:=OP_ADD;
              cg.a_op_const_loc(exprasmlist,hop,1,t2.location);
            end;

         cg.a_label(exprasmlist,aktcontinuelabel);

         { makes no problems there }
      {$ifndef newra}
         rg.cleartempgen;
      {$endif}

         if do_loopvar_at_end then
           if lnf_backward in loopflags then
             if count_var_is_signed then
               hcond:=OC_GTE
             else
               hcond:=OC_AE
            else
              if count_var_is_signed then
                hcond:=OC_LTE
              else
                hcond:=OC_BE
         else
           if lnf_backward in loopflags then
             if count_var_is_signed then
               hcond:=OC_GT
             else
               hcond:=OC_A
            else
              if count_var_is_signed then
                hcond:=OC_LT
              else
                hcond:=OC_B;
         load_all_regvars(exprasmlist);

         cmp_const:=aword(Tordconstnode(right).value);
         if do_loopvar_at_end then
            begin
              {Watch out for wrap around 255 -> 0.}
              {Ugly: This code is way to long... Use tables?}
              case opsize of
                OS_8:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if byte(cmp_const)=low(byte) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(byte);
                          end
                      end
                    else
                      begin
                        if byte(cmp_const)=high(byte) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(byte);
                          end
                      end
                  end;
                OS_16:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if word(cmp_const)=high(word) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(word);
                          end
                      end
                    else
                      begin
                        if word(cmp_const)=low(word) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(word);
                          end
                      end
                  end;
                OS_32:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if cardinal(cmp_const)=high(cardinal) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(cardinal);
                          end
                      end
                    else
                      begin
                        if cardinal(cmp_const)=low(cardinal) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(cardinal);
                          end
                      end
                  end;
                OS_64:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if qword(cmp_const)=high(qword) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(qword);
                          end
                      end
                    else
                      begin
                        if qword(cmp_const)=low(qword) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(qword);
                          end
                      end
                  end;
                OS_S8:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if shortint(cmp_const)=low(shortint) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(shortint);
                          end
                      end
                    else
                      begin
                        if shortint(cmp_const)=high(shortint) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(shortint);
                          end
                      end
                  end;
                OS_S16:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if integer(cmp_const)=high(integer) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(integer);
                          end
                      end
                    else
                      begin
                        if integer(cmp_const)=low(integer) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(integer);
                          end
                      end
                  end;
                OS_S32:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if longint(cmp_const)=high(longint) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(longint);
                          end
                      end
                    else
                      begin
                        if longint(cmp_const)=low(longint) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(longint);
                          end
                      end
                  end;
                OS_S64:
                  begin
                    if lnf_backward in loopflags then
                      begin
                        if int64(cmp_const)=high(int64) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=low(int64);
                          end
                      end
                    else
                      begin
                        if int64(cmp_const)=low(int64) then
                          begin
                            hcond:=OC_NE;
                            cmp_const:=high(int64);
                          end
                      end
                  end;
                else
                  internalerror(200201021);
              end;
            end;

         { produce comparison and the corresponding }
         { jump                                     }
         if temptovalue then
           begin
             cg.a_cmp_ref_loc_label(exprasmlist,opsize,hcond,temp1,
               t2.location,l3);
           end
         else
           begin
             cg.a_cmp_const_loc_label(exprasmlist,opsize,hcond,
               cmp_const,t2.location,l3);
           end;

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
      begin
         location_reset(location,LOC_VOID,OS_NO);

         include(flowcontrol,fc_exit);
         if assigned(left) then
           secondpass(left);

         cg.a_jmp_always(exprasmlist,aktexitlabel);
       end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure tcgbreaknode.pass_2;
      begin
         location_reset(location,LOC_VOID,OS_NO);

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
         location_reset(location,LOC_VOID,OS_NO);

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
         location_reset(location,LOC_VOID,OS_NO);

         load_all_regvars(exprasmlist);
         cg.a_jmp_always(exprasmlist,labsym.lab)
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure tcglabelnode.pass_2;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         load_all_regvars(exprasmlist);
         cg.a_label(exprasmlist,labelnr);
      {$ifndef newra}
         rg.cleartempgen;
      {$endif newra}
         secondpass(left);
      end;


{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    procedure tcgraisenode.pass_2;

      var
         a : tasmlabel;
         href2: treference;
         r:Tregister;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         if assigned(left) then
           begin
              { multiple parameters? }
              if assigned(right) then
                begin
                  { push frame }
                  if assigned(frametree) then
                    begin
                      secondpass(frametree);
                      if codegenerror then
                       exit;
                      cg.a_param_loc(exprasmlist,frametree.location,paramanager.getintparaloc(2));
                    end
                  else
                    cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(2));
                  { push address }
                  secondpass(right);
                  if codegenerror then
                   exit;
                  cg.a_param_loc(exprasmlist,right.location,paramanager.getintparaloc(1));
                end
              else
                begin
                   { get current address }
                   objectlibrary.getaddrlabel(a);
                   cg.a_label(exprasmlist,a);
                   reference_reset_symbol(href2,a,0);
                   { push current frame }
                   r.enum:=R_INTREGISTER;
                   r.number:=NR_FRAME_POINTER_REG;
                   cg.a_param_reg(exprasmlist,OS_ADDR,r,paramanager.getintparaloc(2));
                   { push current address }
                   cg.a_paramaddr_ref(exprasmlist,href2,paramanager.getintparaloc(1));
                end;
              { push object }
              secondpass(left);
              if codegenerror then
                exit;
              cg.a_param_loc(exprasmlist,left.location,paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_RAISEEXCEPTION');
           end
         else
           begin
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.a_call_name(exprasmlist,'FPC_RERAISE');
           end;
       end;


{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       endexceptlabel : tasmlabel;


    procedure try_new_exception(list : taasmoutput;var jmpbuf,envbuf, href : treference;
      a : aword; exceptlabel : tasmlabel);
     begin
       tg.GetTemp(list,EXCEPT_BUF_SIZE,tt_persistent,envbuf);
       tg.GetTemp(list,JMP_BUF_SIZE,tt_persistent,jmpbuf);
       tg.GetTemp(list,sizeof(aword),tt_persistent,href);
       new_exception(list, jmpbuf,envbuf, href, a, exceptlabel);
     end;


    procedure try_free_exception(list : taasmoutput;var jmpbuf, envbuf, href : treference;
     a : aword ; endexceptlabel : tasmlabel; onlyfree : boolean);
     begin
         free_exception(list, jmpbuf, envbuf, href, a, endexceptlabel, onlyfree);
         tg.ungettemp(list,href);
         tg.Ungettemp(list,jmpbuf);
         tg.ungettemp(list,envbuf);
     end;



    { does the necessary things to clean up the object stack }
    { in the except block                                    }
    procedure cleanupobjectstack;

    var r:Tregister;

      begin
         cg.a_call_name(exprasmlist,'FPC_POPOBJECTSTACK');
         r.enum:=R_INTREGISTER;
         r.number:=NR_FUNCTION_RESULT_REG;
         cg.a_param_reg(exprasmlist,OS_ADDR,r,paramanager.getintparaloc(1));
         cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
      end;


    procedure tcgtryexceptnode.pass_2;

      var
         exceptlabel,doexceptlabel,oldendexceptlabel,
         lastonlabel,
         exitexceptlabel,
         continueexceptlabel,
         breakexceptlabel,
         exittrylabel,
         continuetrylabel,
         breaktrylabel,
         doobjectdestroy,
         doobjectdestroyandreraise,
         oldaktexitlabel,
         oldaktcontinuelabel,
         oldaktbreaklabel : tasmlabel;
         oldflowcontrol,tryflowcontrol,
         exceptflowcontrol : tflowcontrol;
         tempbuf,tempaddr : treference;
         href : treference;
         r:Tregister;

      label
         errorexit;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         { this can be called recursivly }
         oldendexceptlabel:=endexceptlabel;

         { save the old labels for control flow statements }
         oldaktexitlabel:=aktexitlabel;
         if assigned(aktbreaklabel) then
           begin
              oldaktcontinuelabel:=aktcontinuelabel;
              oldaktbreaklabel:=aktbreaklabel;
           end;

         { get new labels for the control flow statements }
         objectlibrary.getlabel(exittrylabel);
         objectlibrary.getlabel(exitexceptlabel);
         if assigned(aktbreaklabel) then
           begin
              objectlibrary.getlabel(breaktrylabel);
              objectlibrary.getlabel(continuetrylabel);
              objectlibrary.getlabel(breakexceptlabel);
              objectlibrary.getlabel(continueexceptlabel);
           end;

         objectlibrary.getlabel(exceptlabel);
         objectlibrary.getlabel(doexceptlabel);
         objectlibrary.getlabel(endexceptlabel);
         objectlibrary.getlabel(lastonlabel);

         try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,exceptlabel);

         { try block }
         { set control flow labels for the try block }
         aktexitlabel:=exittrylabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continuetrylabel;
            aktbreaklabel:=breaktrylabel;
          end;

         flowcontrol:=[];
         secondpass(left);
         tryflowcontrol:=flowcontrol;
         if codegenerror then
           goto errorexit;

         cg.a_label(exprasmlist,exceptlabel);

         try_free_exception(exprasmlist,tempbuf,tempaddr,href,0,endexceptlabel,false);

         cg.a_label(exprasmlist,doexceptlabel);

         { set control flow labels for the except block }
         { and the on statements                        }
         aktexitlabel:=exitexceptlabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continueexceptlabel;
            aktbreaklabel:=breakexceptlabel;
          end;

         flowcontrol:=[];
         { on statements }
         if assigned(right) then
           secondpass(right);

         cg.a_label(exprasmlist,lastonlabel);
         { default handling except handling }
         if assigned(t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              cg.a_param_const(exprasmlist,OS_ADDR,aword(-1),paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_CATCHES');

              { the destruction of the exception object must be also }
              { guarded by an exception frame                        }
              objectlibrary.getlabel(doobjectdestroy);
              objectlibrary.getlabel(doobjectdestroyandreraise);

              try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,doobjectdestroyandreraise);

              { here we don't have to reset flowcontrol           }
              { the default and on flowcontrols are handled equal }
              secondpass(t1);
              exceptflowcontrol:=flowcontrol;

              cg.a_label(exprasmlist,doobjectdestroyandreraise);

              try_free_exception(exprasmlist,tempbuf,tempaddr,href,0,doobjectdestroy,false);

              cg.a_call_name(exprasmlist,'FPC_POPSECONDOBJECTSTACK');

              r.enum:=R_INTREGISTER;
              r.number:=NR_FUNCTION_RESULT_REG;
              cg.a_param_reg(exprasmlist, OS_ADDR, r, paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
              { we don't need to restore esi here because reraise never }
              { returns                                                 }
              cg.a_call_name(exprasmlist,'FPC_RERAISE');

              cg.a_label(exprasmlist,doobjectdestroy);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,endexceptlabel);
           end
         else
           begin
              cg.a_call_name(exprasmlist,'FPC_RERAISE');
              exceptflowcontrol:=flowcontrol;
           end;

         if fc_exit in exceptflowcontrol then
           begin
              { do some magic for exit in the try block }
              cg.a_label(exprasmlist,exitexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktexitlabel);
           end;

         if fc_break in exceptflowcontrol then
           begin
              cg.a_label(exprasmlist,breakexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
           end;

         if fc_continue in exceptflowcontrol then
           begin
              cg.a_label(exprasmlist,continueexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
           end;

         if fc_exit in tryflowcontrol then
           begin
              { do some magic for exit in the try block }
              cg.a_label(exprasmlist,exittrylabel);
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cg.a_jmp_always(exprasmlist,oldaktexitlabel);
           end;

         if fc_break in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,breaktrylabel);
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
           end;

         if fc_continue in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,continuetrylabel);
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
           end;
         cg.a_label(exprasmlist,endexceptlabel);

       errorexit:
         { restore all saved labels }
         endexceptlabel:=oldendexceptlabel;

         { restore the control flow labels }
         aktexitlabel:=oldaktexitlabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;

         { return all used control flow statements }
         flowcontrol:=oldflowcontrol+exceptflowcontrol+
           tryflowcontrol;
      end;


    procedure tcgonnode.pass_2;
      var
         nextonlabel,
         exitonlabel,
         continueonlabel,
         breakonlabel,
         oldaktexitlabel,
         oldaktcontinuelabel,
         doobjectdestroyandreraise,
         doobjectdestroy,
         oldaktbreaklabel : tasmlabel;
         ref : treference;
         oldflowcontrol : tflowcontrol;
         tempbuf,tempaddr : treference;
         href : treference;
         href2: treference;
         r:Tregister;

      begin
         location_reset(location,LOC_VOID,OS_NO);

         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         objectlibrary.getlabel(nextonlabel);

         { send the vmt parameter }
         reference_reset_symbol(href2,objectlibrary.newasmsymboldata(excepttype.vmt_mangledname),0);
         cg.a_paramaddr_ref(exprasmlist,href2,paramanager.getintparaloc(1));
         cg.a_call_name(exprasmlist,'FPC_CATCHES');

         { is it this catch? No. go to next onlabel }
         r.enum:=R_INTREGISTER;
         r.number:=NR_FUNCTION_RESULT_REG;
         cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,r,nextonlabel);
         ref.symbol:=nil;
         tg.GetTemp(exprasmlist,pointer_size,tt_normal,ref);

         { what a hack ! }
         if assigned(exceptsymtable) then
           tvarsym(exceptsymtable.symindex.first).address:=ref.offset;
         cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,r,ref);

         { in the case that another exception is risen }
         { we've to destroy the old one                }
         objectlibrary.getlabel(doobjectdestroyandreraise);

         { call setjmp, and jump to finally label on non-zero result }
         try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,doobjectdestroyandreraise);

         if assigned(right) then
           begin
              oldaktexitlabel:=aktexitlabel;
              objectlibrary.getlabel(exitonlabel);
              aktexitlabel:=exitonlabel;
              if assigned(aktbreaklabel) then
               begin
                 oldaktcontinuelabel:=aktcontinuelabel;
                 oldaktbreaklabel:=aktbreaklabel;
                 objectlibrary.getlabel(breakonlabel);
                 objectlibrary.getlabel(continueonlabel);
                 aktcontinuelabel:=continueonlabel;
                 aktbreaklabel:=breakonlabel;
               end;

              secondpass(right);
           end;
         objectlibrary.getlabel(doobjectdestroy);
         cg.a_label(exprasmlist,doobjectdestroyandreraise);

         try_free_exception(exprasmlist,tempbuf,tempaddr,href,0,doobjectdestroy,false);

         cg.a_call_name(exprasmlist,'FPC_POPSECONDOBJECTSTACK');
         cg.a_param_reg(exprasmlist, OS_ADDR, r, paramanager.getintparaloc(1));
         cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
         { we don't need to restore esi here because reraise never }
         { returns                                                 }
         cg.a_call_name(exprasmlist,'FPC_RERAISE');

         cg.a_label(exprasmlist,doobjectdestroy);
         cleanupobjectstack;
         { clear some stuff }
         tg.ungetiftemp(exprasmlist,ref);
         cg.a_jmp_always(exprasmlist,endexceptlabel);

         if assigned(right) then
           begin
              { special handling for control flow instructions }
              if fc_exit in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,exitonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktexitlabel);
                end;

              if fc_break in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,breakonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
                end;

              if fc_continue in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,continueonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
                end;

              aktexitlabel:=oldaktexitlabel;
              if assigned(oldaktbreaklabel) then
               begin
                 aktcontinuelabel:=oldaktcontinuelabel;
                 aktbreaklabel:=oldaktbreaklabel;
               end;
           end;

         cg.a_label(exprasmlist,nextonlabel);
         flowcontrol:=oldflowcontrol+flowcontrol;
         { next on node }
         if assigned(left) then
           begin
            {$ifndef newra}
              rg.cleartempgen;
            {$endif newra}
              secondpass(left);
           end;
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure tcgtryfinallynode.pass_2;
      var
         reraiselabel,
         finallylabel,
         endfinallylabel,
         exitfinallylabel,
         continuefinallylabel,
         breakfinallylabel,
         oldaktexitlabel,
         oldaktcontinuelabel,
         oldaktbreaklabel : tasmlabel;
         oldflowcontrol,tryflowcontrol : tflowcontrol;
         decconst : longint;
         tempbuf,tempaddr : treference;
         href : treference;
         r:Tregister;

      begin
         location_reset(location,LOC_VOID,OS_NO);

         { check if child nodes do a break/continue/exit }
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         objectlibrary.getlabel(finallylabel);
         objectlibrary.getlabel(endfinallylabel);
         objectlibrary.getlabel(reraiselabel);

         { the finally block must catch break, continue and exit }
         { statements                                            }
         oldaktexitlabel:=aktexitlabel;
         if implicitframe then
           exitfinallylabel:=finallylabel
         else
           objectlibrary.getlabel(exitfinallylabel);
         aktexitlabel:=exitfinallylabel;
         if assigned(aktbreaklabel) then
          begin
            oldaktcontinuelabel:=aktcontinuelabel;
            oldaktbreaklabel:=aktbreaklabel;
            if implicitframe then
              begin
                breakfinallylabel:=finallylabel;
                continuefinallylabel:=finallylabel;
              end
            else
              begin
                objectlibrary.getlabel(breakfinallylabel);
                objectlibrary.getlabel(continuefinallylabel);
              end;
            aktcontinuelabel:=continuefinallylabel;
            aktbreaklabel:=breakfinallylabel;
          end;

         { call setjmp, and jump to finally label on non-zero result }
         try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,finallylabel);

         { try code }
         if assigned(left) then
           begin
              secondpass(left);
              tryflowcontrol:=flowcontrol;
              if codegenerror then
                exit;
           end;

         cg.a_label(exprasmlist,finallylabel);
         { just free the frame information }
         try_free_exception(exprasmlist,tempbuf,tempaddr,href,1,finallylabel,true);

         { finally code }
         flowcontrol:=[];
         secondpass(right);
         if flowcontrol<>[] then
           CGMessage(cg_e_control_flow_outside_finally);
         if codegenerror then
           exit;

         { the value should now be in the exception handler }
         cg.g_exception_reason_load(exprasmlist,href);
         r.enum:=R_INTREGISTER;
         r.number:=NR_FUNCTION_RESULT_REG;
         if implicitframe then
           begin
             cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,r,endfinallylabel);
             { finally code only needed to be executed on exception }
             flowcontrol:=[];
             secondpass(t1);
             if flowcontrol<>[] then
               CGMessage(cg_e_control_flow_outside_finally);
             if codegenerror then
               exit;
             cg.a_call_name(exprasmlist,'FPC_RERAISE');
           end
         else
           begin
             cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,r,endfinallylabel);
             cg.a_op_const_reg(exprasmlist,OP_SUB,OS_32,1,r);
             cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,r,reraiselabel);
             if fc_exit in tryflowcontrol then
               begin
                  cg.a_op_const_reg(exprasmlist,OP_SUB,OS_32,1,r);
                  cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,r,oldaktexitlabel);
                  decconst:=1;
               end
             else
               decconst:=2;
             if fc_break in tryflowcontrol then
               begin
                  cg.a_op_const_reg(exprasmlist,OP_SUB,OS_32,decconst,r);
                  cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,r,oldaktbreaklabel);
                  decconst:=1;
               end
             else
               inc(decconst);
             if fc_continue in tryflowcontrol then
               begin
                  cg.a_op_const_reg(exprasmlist,OP_SUB,OS_32,decconst,r);
                  cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,r,oldaktcontinuelabel);
               end;
             cg.a_label(exprasmlist,reraiselabel);
             cg.a_call_name(exprasmlist,'FPC_RERAISE');
             { do some magic for exit,break,continue in the try block }
             if fc_exit in tryflowcontrol then
               begin
                  cg.a_label(exprasmlist,exitfinallylabel);
                  cg.g_exception_reason_load(exprasmlist,href);
                  cg.g_exception_reason_save_const(exprasmlist,href,2);
                  cg.a_jmp_always(exprasmlist,finallylabel);
               end;
             if fc_break in tryflowcontrol then
              begin
                 cg.a_label(exprasmlist,breakfinallylabel);
                 cg.g_exception_reason_load(exprasmlist,href);
                 cg.g_exception_reason_save_const(exprasmlist,href,3);
                 cg.a_jmp_always(exprasmlist,finallylabel);
               end;
             if fc_continue in tryflowcontrol then
               begin
                  cg.a_label(exprasmlist,continuefinallylabel);
                  cg.g_exception_reason_load(exprasmlist,href);
                  cg.g_exception_reason_save_const(exprasmlist,href,4);
                  cg.a_jmp_always(exprasmlist,finallylabel);
               end;
           end;
         cg.a_label(exprasmlist,endfinallylabel);

         aktexitlabel:=oldaktexitlabel;
         if assigned(aktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;
         flowcontrol:=oldflowcontrol+tryflowcontrol;
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
   craisenode:=tcgraisenode;
   ctryexceptnode:=tcgtryexceptnode;
   ctryfinallynode:=tcgtryfinallynode;
   connode:=tcgonnode;
end.
{
  $Log$
  Revision 1.68  2003-06-03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.67  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.66  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.65  2003/05/30 18:55:21  jonas
    * fixed several regvar related bugs for non-i386. make cycle with -Or now
      works for ppc

  Revision 1.64  2003/05/26 21:17:17  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.63  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.62  2003/05/17 13:30:08  jonas
    * changed tt_persistant to tt_persistent :)
    * tempcreatenode now doesn't accept a boolean anymore for persistent
      temps, but a ttemptype, so you can also create ansistring temps etc

  Revision 1.61  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.60  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.59  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.58  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.57  2003/04/29 07:29:14  michael
  + Patch from peter to fix wrong pushing of ansistring function results in open array

  Revision 1.56  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.55  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.54  2003/04/17 07:50:24  daniel
    * Some work on interference graph construction

  Revision 1.53  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.52  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.51  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.50  2003/02/15 22:17:38  carl
   * bugfix of FPU emulation code

  Revision 1.49  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.48  2003/01/03 09:51:58  daniel
    * Compiler now cycles with var_notification

  Revision 1.47  2003/01/02 15:29:25  daniel
    * Some debugging on for loop optimization

  Revision 1.46  2002/12/31 09:55:58  daniel
   + Notification implementation complete
   + Add for loop code optimization using notifications
     results in 1.5-1.9% speed improvement in nestloop benchmark
     Optimization incomplete, compiler does not cycle yet with
     notifications enabled.

  Revision 1.45  2002/11/28 11:17:01  florian
    * loop node flags from node flags splitted

  Revision 1.44  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.43  2002/09/30 07:00:45  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.42  2002/09/07 15:25:02  peter
    * old logs removed and tabs fixed

  Revision 1.41  2002/09/01 18:47:00  peter
    * assignn check in exitnode changed to use a separate boolean as the
      assignn can be changed to a calln

  Revision 1.40  2002/09/01 14:41:47  peter
    * increase refcount in exit(arg) for arg

  Revision 1.39  2002/08/24 18:41:52  peter
    * fixed wrong label in jump of except block (was also in n386flw wrong)
    * fixed wrong pushing of raise parameters
    * fixed wrong compare in finally

  Revision 1.38  2002/08/23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.37  2002/08/19 19:36:43  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.36  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.35  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.34  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.33  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.32  2002/08/09 19:10:59  carl
    * fixed generic exception management

  Revision 1.31  2002/08/04 19:06:41  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.30  2002/07/27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.29  2002/07/25 17:56:29  carl
    + FPURESULTREG -> FPU_RESULT_REG

  Revision 1.28  2002/07/21 06:58:49  daniel
  * Changed booleans into flags

  Revision 1.27  2002/07/20 12:54:53  daniel
  * Optimized the code generated for for nodes. The shootout/nestloop benchmark
    now runs 5% faster on my computer.

  Revision 1.26  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.25  2002/07/20 11:15:51  daniel
  * The for node does a check if the first comparision can be skipped. I moved
    the check from the second pass to the resulttype pass. The advantage is
    that the state tracker can now decide to skip the first comparision too.

  Revision 1.24  2002/07/20 08:14:24  daniel
  * Loops should not be aligned when optimizing for size

  Revision 1.23  2002/07/19 11:41:35  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.22  2002/07/04 20:43:01  florian
    * first x86-64 patches

}

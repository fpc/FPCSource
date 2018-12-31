{
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
      globtype,
      aasmbase,aasmdata,nflw,
      pass_2,cgutils,ncgutil;

    type
       tcgwhilerepeatnode = class(twhilerepeatnode)
          usedregvars: tusedregvars;

          procedure pass_generate_code;override;
          procedure sync_regvars(checkusedregvars: boolean);
       end;

       tcgifnode = class(tifnode)
          procedure pass_generate_code;override;
       end;

       tcgfornode = class(tfornode)
          procedure pass_generate_code;override;
       end;

       tcgexitnode = class(texitnode)
          procedure pass_generate_code;override;
       end;

       tcgbreaknode = class(tbreaknode)
          procedure pass_generate_code;override;
       end;

       tcgcontinuenode = class(tcontinuenode)
          procedure pass_generate_code;override;
       end;

       tcggotonode = class(tgotonode)
          procedure pass_generate_code;override;
       end;

       tcglabelnode = class(tlabelnode)
       protected
          asmlabel : tasmlabel;
       public
          function getasmlabel : tasmlabel; virtual;
          procedure pass_generate_code;override;
       end;

       tcgraisenode = class(traisenode)
       end;

       { Utility class for exception handling state management that is used
         by tryexcept/tryfinally/on nodes (in a separate class so it can both
         be shared and overridden)

         Never instantiated. }
       tcgexceptionstatehandler = class
         type
           texceptiontemps=record
             jmpbuf,
             envbuf,
             reasonbuf  : treference;
           end;

          texceptionstate = record
            exceptionlabel: TAsmLabel;
            oldflowcontrol,
            newflowcontrol: tflowcontrol;
          end;

          class procedure get_exception_temps(list:TAsmList;var t:texceptiontemps); virtual;
          class procedure unget_exception_temps(list:TAsmList;const t:texceptiontemps); virtual;
          class procedure new_exception(list:TAsmList;const t:texceptiontemps; out exceptstate: texceptionstate); virtual;
          class procedure emit_except_label(list: TAsmList; var exceptstate: texceptionstate); virtual;
          class procedure free_exception(list:TAsmList;const t:texceptiontemps;a:aint;endexceptlabel:tasmlabel;onlyfree:boolean); virtual;
          class procedure cleanupobjectstack; virtual;
          class procedure handle_nested_exception(list:TAsmList;const t:texceptiontemps;var entrystate: texceptionstate); virtual;
       end;
       tcgexceptionstatehandlerclass = class of tcgexceptionstatehandler;


       tcgtryexceptnode = class(ttryexceptnode)
          procedure pass_generate_code;override;
       end;

       tcgtryfinallynode = class(ttryfinallynode)
          procedure handle_safecall_exception;
          procedure pass_generate_code;override;
       end;

       tcgonnode = class(tonnode)
          procedure pass_generate_code;override;
       end;


     var
       cexceptionstatehandler: tcgexceptionstatehandlerclass;

implementation

    uses
      cutils,
      verbose,globals,systems,
      symconst,symdef,symsym,symtable,symtype,aasmtai,aasmcpu,defutil,
      procinfo,cgbase,parabase,
      fmodule,
      cpubase,
      tgobj,paramgr,
      cgobj,hlcgobj,nutils
      ;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure tcgwhilerepeatnode.sync_regvars(checkusedregvars: boolean);
      begin
         if (cs_opt_regvar in current_settings.optimizerswitches) and
            not(pi_has_label in current_procinfo.flags) then
           begin
             if checkusedregvars then
               begin
                 usedregvars.intregvars.init;
                 usedregvars.addrregvars.init;
                 usedregvars.fpuregvars.init;
                 usedregvars.mmregvars.init;

                 { we have to synchronise both the regvars used in the loop }
                 { and the ones in the while/until condition                }
                 get_used_regvars(self,usedregvars);
                 gen_sync_regvars(current_asmdata.CurrAsmList,usedregvars);
               end
             else
               begin
                 gen_sync_regvars(current_asmdata.CurrAsmList,usedregvars);
                 usedregvars.intregvars.done;
                 usedregvars.addrregvars.done;
                 usedregvars.fpuregvars.done;
                 usedregvars.mmregvars.done;
               end;
           end;
      end;


    procedure tcgwhilerepeatnode.pass_generate_code;
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : tasmlabel;
         truelabel,falselabel : tasmlabel;
         oldflowcontrol : tflowcontrol;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         current_asmdata.getjumplabel(lloop);
         current_asmdata.getjumplabel(lcont);
         current_asmdata.getjumplabel(lbreak);
         { arrange continue and breaklabels: }
         oldflowcontrol:=flowcontrol;
         oldclabel:=current_procinfo.CurrContinueLabel;
         oldblabel:=current_procinfo.CurrBreakLabel;
         include(flowcontrol,fc_inflowcontrol);
         exclude(flowcontrol,fc_unwind_loop);

         sync_regvars(true);
{$ifdef OLDREGVARS}
         load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
         { handling code at the end as it is much more efficient, and makes
           while equal to repeat loop, only the end true/false is swapped (PFV) }
         if lnf_testatbegin in loopflags then
           hlcg.a_jmp_always(current_asmdata.CurrAsmList,lcont);

         if not(cs_opt_size in current_settings.optimizerswitches) then
            { align loop target }
            current_asmdata.CurrAsmList.concat(Tai_align.Create(current_settings.alignment.loopalign));

         hlcg.a_label(current_asmdata.CurrAsmList,lloop);

         current_procinfo.CurrContinueLabel:=lcont;
         current_procinfo.CurrBreakLabel:=lbreak;

         if assigned(right) then
           secondpass(right);

{$ifdef OLDREGVARS}
         load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}

         hlcg.a_label(current_asmdata.CurrAsmList,lcont);
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
         secondpass(left);

         hlcg.maketojumpboollabels(current_asmdata.CurrAsmList,left,truelabel,falselabel);
         hlcg.a_label(current_asmdata.CurrAsmList,lbreak);

         sync_regvars(false);

         current_procinfo.CurrContinueLabel:=oldclabel;
         current_procinfo.CurrBreakLabel:=oldblabel;
         { a break/continue in a while/repeat block can't be seen outside }
         flowcontrol:=oldflowcontrol+(flowcontrol-[fc_break,fc_continue,fc_inflowcontrol]);
      end;


{*****************************************************************************
                               tcgIFNODE
*****************************************************************************}

    procedure tcgifnode.pass_generate_code;

      var
         hl : tasmlabel;
         oldflowcontrol: tflowcontrol;
(*
         org_regvar_loaded_other,
         then_regvar_loaded_other,
         else_regvar_loaded_other : regvarother_booleanarray;
         org_regvar_loaded_int,
         then_regvar_loaded_int,
         else_regvar_loaded_int : Tsuperregisterset;
         org_list,
         then_list,
         else_list : TAsmList;
*)

      begin
         location_reset(location,LOC_VOID,OS_NO);
         hl:=nil;

         oldflowcontrol := flowcontrol;
         include(flowcontrol,fc_inflowcontrol);
         secondpass(left);

(*
         { save regvars loaded in the beginning so that we can restore them }
         { when processing the else-block                                   }
         if cs_opt_regvar in current_settings.optimizerswitches then
           begin
             org_list := current_asmdata.CurrAsmList;
             current_asmdata.CurrAsmList := TAsmList.create;
           end;
*)
         hlcg.maketojumpbool(current_asmdata.CurrAsmList,left);

(*
         if cs_opt_regvar in current_settings.optimizerswitches then
           begin
             org_regvar_loaded_int := rg.regvar_loaded_int;
             org_regvar_loaded_other := rg.regvar_loaded_other;
           end;
*)
         if assigned(right) then
           begin
              hlcg.a_label(current_asmdata.CurrAsmList,left.location.truelabel);
              secondpass(right);
           end;

         { save current asmlist (previous instructions + then-block) and }
         { loaded regvar state and create new clean ones                 }
{
         if cs_opt_regvar in current_settings.optimizerswitches then
           begin
             then_regvar_loaded_int := rg.regvar_loaded_int;
             then_regvar_loaded_other := rg.regvar_loaded_other;
             rg.regvar_loaded_int := org_regvar_loaded_int;
             rg.regvar_loaded_other := org_regvar_loaded_other;
             then_list := current_asmdata.CurrAsmList;
             current_asmdata.CurrAsmList := TAsmList.create;
           end;
}

         if assigned(t1) then
           begin
              if assigned(right) then
                begin
                   current_asmdata.getjumplabel(hl);
                   { do go back to if line !! }
(*
                   if not(cs_opt_regvar in current_settings.optimizerswitches) then
*)
                     current_filepos:=current_asmdata.CurrAsmList.getlasttaifilepos^
(*
                   else
                     current_filepos:=then_list.getlasttaifilepos^
*)
                   ;
                   hlcg.a_jmp_always(current_asmdata.CurrAsmList,hl);
                end;
              hlcg.a_label(current_asmdata.CurrAsmList,left.location.falselabel);
              secondpass(t1);
(*
              { save current asmlist (previous instructions + else-block) }
              { and loaded regvar state and create a new clean list       }
              if cs_opt_regvar in current_settings.optimizerswitches then
                begin
{                  else_regvar_loaded_int := rg.regvar_loaded_int;
                  else_regvar_loaded_other := rg.regvar_loaded_other;}
                  else_list := current_asmdata.CurrAsmList;
                  current_asmdata.CurrAsmList := TAsmList.create;
                end;
*)
              if assigned(right) then
                hlcg.a_label(current_asmdata.CurrAsmList,hl);
           end
         else
           begin
(*
              if cs_opt_regvar in current_settings.optimizerswitches then
                begin
{                  else_regvar_loaded_int := rg.regvar_loaded_int;
                  else_regvar_loaded_other := rg.regvar_loaded_other;}
                  else_list := current_asmdata.CurrAsmList;
                  current_asmdata.CurrAsmList := TAsmList.create;
                end;
*)
              current_asmdata.CurrAsmList.concat(cai_align.create(current_settings.alignment.jumpalign));
              hlcg.a_label(current_asmdata.CurrAsmList,left.location.falselabel);
           end;
         if not(assigned(right)) then
           begin
              hlcg.a_label(current_asmdata.CurrAsmList,left.location.truelabel);
           end;

(*
         if cs_opt_regvar in current_settings.optimizerswitches then
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
             org_list.concatlist(current_asmdata.CurrAsmList);
             current_asmdata.CurrAsmList.free;
             current_asmdata.CurrAsmList := org_list;
           end;
*)

         flowcontrol := oldflowcontrol + (flowcontrol - [fc_inflowcontrol]);
      end;


{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure tcgfornode.pass_generate_code;
      begin
        { for nodes are converted in pass_1 in a while loop }
        internalerror(2015082501);
      end;


{*****************************************************************************
                              SecondExitN
*****************************************************************************}

    procedure tcgexitnode.pass_generate_code;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         include(flowcontrol,fc_exit);
         if assigned(left) then
           secondpass(left);

         if (fc_unwind_exit in flowcontrol) then
           hlcg.g_local_unwind(current_asmdata.CurrAsmList,current_procinfo.CurrExitLabel)
         else
           hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrExitLabel);
       end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure tcgbreaknode.pass_generate_code;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         include(flowcontrol,fc_break);
         if current_procinfo.CurrBreakLabel<>nil then
           begin
{$ifdef OLDREGVARS}
             load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
             if (fc_unwind_loop in flowcontrol) then
               hlcg.g_local_unwind(current_asmdata.CurrAsmList,current_procinfo.CurrBreakLabel)
             else
               hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrBreakLabel)
           end
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure tcgcontinuenode.pass_generate_code;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         include(flowcontrol,fc_continue);
         if current_procinfo.CurrContinueLabel<>nil then
           begin
{$ifdef OLDREGVARS}
             load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
             if (fc_unwind_loop in flowcontrol) then
               hlcg.g_local_unwind(current_asmdata.CurrAsmList,current_procinfo.CurrContinueLabel)
             else
               hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrContinueLabel)
           end
         else
           CGMessage(cg_e_continue_not_allowed);
      end;


{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure tcggotonode.pass_generate_code;

       begin
         location_reset(location,LOC_VOID,OS_NO);

         include(flowcontrol,fc_gotolabel);
{$ifdef OLDREGVARS}
         load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
         hlcg.a_jmp_always(current_asmdata.CurrAsmList,tcglabelnode(labelnode).getasmlabel)
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    function tcglabelnode.getasmlabel : tasmlabel;
      begin
        if not(assigned(asmlabel)) then
          { labsym is not set in inlined procedures, but since assembler }
          { routines can't be inlined, that shouldn't matter             }
          if assigned(labsym) and
             labsym.nonlocal then
            current_asmdata.getglobaljumplabel(asmlabel)
          else
            current_asmdata.getjumplabel(asmlabel);
        result:=asmlabel
      end;


    procedure tcglabelnode.pass_generate_code;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         include(flowcontrol,fc_gotolabel);
{$ifdef OLDREGVARS}
         load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
         hlcg.a_label(current_asmdata.CurrAsmList,getasmlabel);

         { Write also extra label if this label was referenced from
           assembler block }
         if assigned(labsym) and
            assigned(labsym.asmblocklabel) then
           hlcg.a_label(current_asmdata.CurrAsmList,labsym.asmblocklabel);

         secondpass(left);
      end;


{*****************************************************************************
                     tcgexceptionstatehandler
*****************************************************************************}

    {  Allocate the buffers for exception management and setjmp environment.
       Return a pointer to these buffers, send them to the utility routine
       so they are registered, and then call setjmp.

       Then compare the result of setjmp with 0, and if not equal
       to zero, then jump to exceptlabel.

       Also store the result of setjmp to a temporary space by calling g_save_exception_reason

       It is to note that this routine may be called *after* the stackframe of a
       routine has been called, therefore on machines where the stack cannot
       be modified, all temps should be allocated on the heap instead of the
       stack. }


    class procedure tcgexceptionstatehandler.get_exception_temps(list:TAsmList;var t:texceptiontemps);
     begin
        tg.gethltemp(list,rec_exceptaddr,rec_exceptaddr.size,tt_persistent,t.envbuf);
        tg.gethltemp(list,rec_jmp_buf,rec_jmp_buf.size,tt_persistent,t.jmpbuf);
        tg.gethltemp(list,ossinttype,ossinttype.size,tt_persistent,t.reasonbuf);
      end;


    class procedure tcgexceptionstatehandler.unget_exception_temps(list:TAsmList;const t:texceptiontemps);
      begin
        tg.Ungettemp(list,t.jmpbuf);
        tg.ungettemp(list,t.envbuf);
        tg.ungettemp(list,t.reasonbuf);
      end;


    class procedure tcgexceptionstatehandler.new_exception(list:TAsmList;const t:texceptiontemps; out exceptstate: texceptionstate);
      var
        paraloc1, paraloc2, paraloc3, pushexceptres, setjmpres: tcgpara;
        pd: tprocdef;
        tmpresloc: tlocation;
      begin
        current_asmdata.getjumplabel(exceptstate.exceptionlabel);
        exceptstate.oldflowcontrol:=flowcontrol;

        paraloc1.init;
        paraloc2.init;
        paraloc3.init;

        { fpc_pushexceptaddr(exceptionframetype, setjmp_buffer, exception_address_chain_entry) }
        pd:=search_system_proc('fpc_pushexceptaddr');
        paramanager.getintparaloc(current_asmdata.CurrAsmList,pd,1,paraloc1);
        paramanager.getintparaloc(current_asmdata.CurrAsmList,pd,2,paraloc2);
        paramanager.getintparaloc(current_asmdata.CurrAsmList,pd,3,paraloc3);
        if pd.is_pushleftright then
          begin
            { type of exceptionframe }
            hlcg.a_load_const_cgpara(list,paraloc1.def,1,paraloc1);
            { setjmp buffer }
            hlcg.a_loadaddr_ref_cgpara(list,rec_jmp_buf,t.jmpbuf,paraloc2);
            { exception address chain entry }
            hlcg.a_loadaddr_ref_cgpara(list,rec_exceptaddr,t.envbuf,paraloc3);
          end
        else
          begin
            hlcg.a_loadaddr_ref_cgpara(list,rec_exceptaddr,t.envbuf,paraloc3);
            hlcg.a_loadaddr_ref_cgpara(list,rec_jmp_buf,t.jmpbuf,paraloc2);
            hlcg.a_load_const_cgpara(list,paraloc1.def,1,paraloc1);
          end;
        paramanager.freecgpara(list,paraloc3);
        paramanager.freecgpara(list,paraloc2);
        paramanager.freecgpara(list,paraloc1);
        { perform the fpc_pushexceptaddr call }
        pushexceptres:=hlcg.g_call_system_proc(list,pd,[@paraloc1,@paraloc2,@paraloc3],nil);
        paraloc1.done;
        paraloc2.done;
        paraloc3.done;

        { get the result }
        location_reset(tmpresloc,LOC_REGISTER,def_cgsize(pushexceptres.def));
        tmpresloc.register:=hlcg.getaddressregister(list,pushexceptres.def);
        hlcg.gen_load_cgpara_loc(list,pushexceptres.def,pushexceptres,tmpresloc,true);
        pushexceptres.resetiftemp;

        { fpc_setjmp(result_of_pushexceptaddr_call) }
        pd:=search_system_proc('fpc_setjmp');
        paramanager.getintparaloc(current_asmdata.CurrAsmList,pd,1,paraloc1);

        hlcg.a_load_reg_cgpara(list,pushexceptres.def,tmpresloc.register,paraloc1);
        paramanager.freecgpara(list,paraloc1);
        { perform the fpc_setjmp call }
        setjmpres:=hlcg.g_call_system_proc(list,pd,[@paraloc1],nil);
        paraloc1.done;
        location_reset(tmpresloc,LOC_REGISTER,def_cgsize(setjmpres.def));
        tmpresloc.register:=hlcg.getintregister(list,setjmpres.def);
        hlcg.gen_load_cgpara_loc(list,setjmpres.def,setjmpres,tmpresloc,true);
        hlcg.g_exception_reason_save(list,setjmpres.def,ossinttype,tmpresloc.register,t.reasonbuf);
        { if we get 0 here in the function result register, it means that we
          longjmp'd back here }
        hlcg.a_cmp_const_reg_label(list,setjmpres.def,OC_NE,0,tmpresloc.register,exceptstate.exceptionlabel);
        setjmpres.resetiftemp;

        flowcontrol:=[fc_inflowcontrol,fc_catching_exceptions];
     end;


    class procedure tcgexceptionstatehandler.emit_except_label(list: TAsmList; var exceptstate: texceptionstate);
      begin
        hlcg.a_label(list,exceptstate.exceptionlabel);
        exceptstate.newflowcontrol:=flowcontrol;
        flowcontrol:=exceptstate.oldflowcontrol;
      end;


    class procedure tcgexceptionstatehandler.free_exception(list:TAsmList;const t:texceptiontemps;a:aint;endexceptlabel:tasmlabel;onlyfree:boolean);
      var
        reasonreg: tregister;
      begin
         hlcg.g_call_system_proc(list,'fpc_popaddrstack',[],nil);
         if not onlyfree then
          begin
            reasonreg:=hlcg.getintregister(list,osuinttype);
            hlcg.g_exception_reason_load(list,osuinttype,osuinttype,t.reasonbuf,reasonreg);
            hlcg.a_cmp_const_reg_label(list,osuinttype,OC_EQ,a,reasonreg,endexceptlabel);
          end;
      end;


    { does the necessary things to clean up the object stack }
    { in the except block                                    }
    class procedure tcgexceptionstatehandler.cleanupobjectstack;
      begin
         hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil);
      end;

    { generates code to be executed when another exeception is raised while
      control is inside except block }
    class procedure tcgexceptionstatehandler.handle_nested_exception(list:TAsmList;const t:texceptiontemps;var entrystate: texceptionstate);
      var
         exitlabel: tasmlabel;
      begin
         { don't generate line info for internal cleanup }
         list.concat(tai_marker.create(mark_NoLineInfoStart));
         current_asmdata.getjumplabel(exitlabel);
         emit_except_label(current_asmdata.CurrAsmList,entrystate);
         free_exception(list,t,0,exitlabel,false);
         { we don't need to save/restore registers here because reraise never }
         { returns                                                            }
         hlcg.g_call_system_proc(list,'fpc_raise_nested',[],nil);
         hlcg.a_label(list,exitlabel);
         cleanupobjectstack;
      end;



{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       endexceptlabel : tasmlabel;


    procedure tcgtryexceptnode.pass_generate_code;

      var
         oldendexceptlabel,
         lastonlabel,
         exitexceptlabel,
         continueexceptlabel,
         breakexceptlabel,
         exittrylabel,
         continuetrylabel,
         breaktrylabel,
         oldCurrExitLabel,
         oldContinueLabel,
         oldBreakLabel : tasmlabel;
         destroytemps,
         excepttemps : tcgexceptionstatehandler.texceptiontemps;
         trystate,doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
      label
         errorexit;
      begin
         location_reset(location,LOC_VOID,OS_NO);
         continuetrylabel:=nil;
         breaktrylabel:=nil;
         continueexceptlabel:=nil;
         breakexceptlabel:=nil;
         doobjectdestroyandreraisestate:=Default(tcgexceptionstatehandler.texceptionstate);

         { this can be called recursivly }
         oldBreakLabel:=nil;
         oldContinueLabel:=nil;
         oldendexceptlabel:=endexceptlabel;

         { save the old labels for control flow statements }
         oldCurrExitLabel:=current_procinfo.CurrExitLabel;
         if assigned(current_procinfo.CurrBreakLabel) then
           begin
              oldContinueLabel:=current_procinfo.CurrContinueLabel;
              oldBreakLabel:=current_procinfo.CurrBreakLabel;
           end;

         { get new labels for the control flow statements }
         current_asmdata.getjumplabel(exittrylabel);
         current_asmdata.getjumplabel(exitexceptlabel);
         if assigned(current_procinfo.CurrBreakLabel) then
           begin
              current_asmdata.getjumplabel(breaktrylabel);
              current_asmdata.getjumplabel(continuetrylabel);
              current_asmdata.getjumplabel(breakexceptlabel);
              current_asmdata.getjumplabel(continueexceptlabel);
           end;

         current_asmdata.getjumplabel(endexceptlabel);
         current_asmdata.getjumplabel(lastonlabel);

         cexceptionstatehandler.get_exception_temps(current_asmdata.CurrAsmList,excepttemps);
         cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,trystate);

         { try block }
         { set control flow labels for the try block }
         current_procinfo.CurrExitLabel:=exittrylabel;
         if assigned(oldBreakLabel) then
          begin
            current_procinfo.CurrContinueLabel:=continuetrylabel;
            current_procinfo.CurrBreakLabel:=breaktrylabel;
          end;

         secondpass(left);
         if codegenerror then
           goto errorexit;

         { don't generate line info for internal cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

         cexceptionstatehandler.emit_except_label(current_asmdata.CurrAsmList,trystate);

         cexceptionstatehandler.free_exception(current_asmdata.CurrAsmList, excepttemps, 0, endexceptlabel, false);

         { end cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

         { set control flow labels for the except block }
         { and the on statements                        }
         current_procinfo.CurrExitLabel:=exitexceptlabel;
         if assigned(oldBreakLabel) then
          begin
            current_procinfo.CurrContinueLabel:=continueexceptlabel;
            current_procinfo.CurrBreakLabel:=breakexceptlabel;
          end;

         flowcontrol:=[fc_inflowcontrol];
         { on statements }
         if assigned(right) then
           secondpass(right);

         { don't generate line info for internal cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

         hlcg.a_label(current_asmdata.CurrAsmList,lastonlabel);
         { default handling except handling }
         if assigned(t1) then
           begin
              { FPC_CATCHES with 'default handler' flag (=-1) need no longer be called,
                it doesn't change any state and its return value is ignored (Sergei)
              }

              { the destruction of the exception object must be also }
              { guarded by an exception frame, but it can be omitted }
              { if there's no user code in 'except' block            }

              if not (has_no_code(t1)) then
               begin
                 cexceptionstatehandler.get_exception_temps(current_asmdata.CurrAsmList,destroytemps);
                 cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,destroytemps,doobjectdestroyandreraisestate);
                 { the flowcontrol from the default except-block must be merged
                   with the flowcontrol flags potentially set by the
                   on-statements handled above (secondpass(right)), as they are
                   at the same program level }
                 flowcontrol:=
                   flowcontrol+
                   doobjectdestroyandreraisestate.oldflowcontrol;


                 { except block needs line info }
                 current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

                 secondpass(t1);

                 cexceptionstatehandler.handle_nested_exception(current_asmdata.CurrAsmList,destroytemps,doobjectdestroyandreraisestate);

                 cexceptionstatehandler.unget_exception_temps(current_asmdata.CurrAsmList,destroytemps);
                 hlcg.a_jmp_always(current_asmdata.CurrAsmList,endexceptlabel);
               end
               else
                 begin
                   doobjectdestroyandreraisestate.newflowcontrol:=flowcontrol;
                   cexceptionstatehandler.cleanupobjectstack;
                   hlcg.a_jmp_always(current_asmdata.CurrAsmList,endexceptlabel);
                 end;
           end
         else
           begin
              hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_reraise',[],nil);
              doobjectdestroyandreraisestate.newflowcontrol:=flowcontrol;
           end;

         if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
           begin
              { do some magic for exit in the try block }
              hlcg.a_label(current_asmdata.CurrAsmList,exitexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_popaddrstack',[],nil);
              hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
              cexceptionstatehandler.cleanupobjectstack;
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldCurrExitLabel);
           end;

         if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
           begin
              hlcg.a_label(current_asmdata.CurrAsmList,breakexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_popaddrstack',[],nil);
              hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
              cexceptionstatehandler.cleanupobjectstack;
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldBreakLabel);
           end;

         if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
           begin
              hlcg.a_label(current_asmdata.CurrAsmList,continueexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_popaddrstack',[],nil);
              hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
              cexceptionstatehandler.cleanupobjectstack;
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldContinueLabel);
           end;

         if fc_exit in trystate.newflowcontrol then
           begin
              { do some magic for exit in the try block }
              hlcg.a_label(current_asmdata.CurrAsmList,exittrylabel);
              hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_popaddrstack',[],nil);
              hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldCurrExitLabel);
           end;

         if fc_break in trystate.newflowcontrol then
           begin
              hlcg.a_label(current_asmdata.CurrAsmList,breaktrylabel);
              hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_popaddrstack',[],nil);
              hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldBreakLabel);
           end;

         if fc_continue in trystate.newflowcontrol then
           begin
              hlcg.a_label(current_asmdata.CurrAsmList,continuetrylabel);
              hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_popaddrstack',[],nil);
              hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldContinueLabel);
           end;
         cexceptionstatehandler.unget_exception_temps(current_asmdata.CurrAsmList,excepttemps);
         hlcg.a_label(current_asmdata.CurrAsmList,endexceptlabel);

         { end cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

       errorexit:
         { restore all saved labels }
         endexceptlabel:=oldendexceptlabel;

         { restore the control flow labels }
         current_procinfo.CurrExitLabel:=oldCurrExitLabel;
         if assigned(oldBreakLabel) then
          begin
            current_procinfo.CurrContinueLabel:=oldContinueLabel;
            current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;

         { return all used control flow statements }
         flowcontrol:=trystate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol +
           trystate.newflowcontrol - [fc_inflowcontrol,fc_catching_exceptions]);
      end;


    procedure tcgonnode.pass_generate_code;
      var
         nextonlabel,
         exitonlabel,
         continueonlabel,
         breakonlabel,
         oldCurrExitLabel,
         oldContinueLabel,
         oldBreakLabel : tasmlabel;
         doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
         excepttemps : tcgexceptionstatehandler.texceptiontemps;
         href2: treference;
         paraloc1 : tcgpara;
         exceptvarsym : tlocalvarsym;
         pd : tprocdef;
         fpc_catches_res: TCGPara;
         fpc_catches_resloc: tlocation;
         otherunit,
         indirect : boolean;
      begin
         paraloc1.init;
         location_reset(location,LOC_VOID,OS_NO);
         oldCurrExitLabel:=nil;
         continueonlabel:=nil;
         breakonlabel:=nil;
         exitonlabel:=nil;

         current_asmdata.getjumplabel(nextonlabel);

         otherunit:=findunitsymtable(excepttype.owner).moduleid<>findunitsymtable(current_procinfo.procdef.owner).moduleid;
         indirect:=(tf_supports_packages in target_info.flags) and
                     (target_info.system in systems_indirect_var_imports) and
                     (cs_imported_data in current_settings.localswitches) and
                     otherunit;

         { send the vmt parameter }
         pd:=search_system_proc('fpc_catches');
         reference_reset_symbol(href2,current_asmdata.RefAsmSymbol(excepttype.vmt_mangledname,AT_DATA,indirect),0,sizeof(pint),[]);
         if otherunit then
           current_module.add_extern_asmsym(excepttype.vmt_mangledname,AB_EXTERNAL,AT_DATA);
         paramanager.getintparaloc(current_asmdata.CurrAsmList,pd,1,paraloc1);
         hlcg.a_loadaddr_ref_cgpara(current_asmdata.CurrAsmList,excepttype.vmt_def,href2,paraloc1);
         paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc1);
         fpc_catches_res:=hlcg.g_call_system_proc(current_asmdata.CurrAsmList,pd,[@paraloc1],nil);
         location_reset(fpc_catches_resloc,LOC_REGISTER,def_cgsize(fpc_catches_res.def));
         fpc_catches_resloc.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,fpc_catches_res.def);
         hlcg.gen_load_cgpara_loc(current_asmdata.CurrAsmList,fpc_catches_res.def,fpc_catches_res,fpc_catches_resloc,true);

         { is it this catch? No. go to next onlabel }
         hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,fpc_catches_res.def,OC_EQ,0,fpc_catches_resloc.register,nextonlabel);

         { Retrieve exception variable }
         if assigned(excepTSymtable) then
           exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
         else
           internalerror(2011020401);

         if assigned(exceptvarsym) then
           begin
             location_reset_ref(exceptvarsym.localloc,LOC_REFERENCE,def_cgsize(voidpointertype),voidpointertype.alignment,[]);
             tg.GetLocal(current_asmdata.CurrAsmList,exceptvarsym.vardef.size,exceptvarsym.vardef,exceptvarsym.localloc.reference);
             hlcg.a_load_reg_ref(current_asmdata.CurrAsmList,fpc_catches_res.def,exceptvarsym.vardef,fpc_catches_resloc.register,exceptvarsym.localloc.reference);
           end;

         { in the case that another exception is risen
           we've to destroy the old one:
           call setjmp, and jump to finally label on non-zero result }
         cexceptionstatehandler.get_exception_temps(current_asmdata.CurrAsmList,excepttemps);
         cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,doobjectdestroyandreraisestate);

         oldBreakLabel:=nil;
         oldContinueLabel:=nil;
         if assigned(right) then
           begin
              oldCurrExitLabel:=current_procinfo.CurrExitLabel;
              current_asmdata.getjumplabel(exitonlabel);
              current_procinfo.CurrExitLabel:=exitonlabel;
              if assigned(current_procinfo.CurrBreakLabel) then
               begin
                 oldContinueLabel:=current_procinfo.CurrContinueLabel;
                 oldBreakLabel:=current_procinfo.CurrBreakLabel;
                 current_asmdata.getjumplabel(breakonlabel);
                 current_asmdata.getjumplabel(continueonlabel);
                 current_procinfo.CurrContinueLabel:=continueonlabel;
                 current_procinfo.CurrBreakLabel:=breakonlabel;
               end;

              secondpass(right);
           end;

         cexceptionstatehandler.handle_nested_exception(current_asmdata.CurrAsmList,excepttemps,doobjectdestroyandreraisestate);

         { clear some stuff }
         if assigned(exceptvarsym) then
           begin
             tg.UngetLocal(current_asmdata.CurrAsmList,exceptvarsym.localloc.reference);
             exceptvarsym.localloc.loc:=LOC_INVALID;
           end;
         hlcg.a_jmp_always(current_asmdata.CurrAsmList,endexceptlabel);

         if assigned(right) then
           begin
              { special handling for control flow instructions }
              if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   hlcg.a_label(current_asmdata.CurrAsmList,exitonlabel);
                   hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldCurrExitLabel);
                end;

              if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   hlcg.a_label(current_asmdata.CurrAsmList,breakonlabel);
                   hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldBreakLabel);
                end;

              if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   hlcg.a_label(current_asmdata.CurrAsmList,continueonlabel);
                   hlcg.a_jmp_always(current_asmdata.CurrAsmList,oldContinueLabel);
                end;

              current_procinfo.CurrExitLabel:=oldCurrExitLabel;
              if assigned(oldBreakLabel) then
               begin
                 current_procinfo.CurrContinueLabel:=oldContinueLabel;
                 current_procinfo.CurrBreakLabel:=oldBreakLabel;
               end;
           end;

         cexceptionstatehandler.unget_exception_temps(current_asmdata.CurrAsmList,excepttemps);
         hlcg.a_label(current_asmdata.CurrAsmList,nextonlabel);
         flowcontrol:=doobjectdestroyandreraisestate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
         paraloc1.done;
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

         { next on node }
         if assigned(left) then
           secondpass(left);
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure tcgtryfinallynode.handle_safecall_exception;
      var
        cgpara: tcgpara;
        selfsym: tparavarsym;
        pd: tprocdef;
      begin
        { call fpc_safecallhandler, passing self for methods of classes,
          nil otherwise. }
        pd:=search_system_proc('fpc_safecallhandler');
        cgpara.init;
        paramanager.getintparaloc(current_asmdata.CurrAsmList,pd,1,cgpara);
        if is_class(current_procinfo.procdef.struct) then
          begin
            selfsym:=tparavarsym(current_procinfo.procdef.parast.Find('self'));
            if (selfsym=nil) or (selfsym.typ<>paravarsym) then
              InternalError(2011123101);
            cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,selfsym.localloc,cgpara);
          end
        else
          cg.a_load_const_cgpara(current_asmdata.CurrAsmList,OS_ADDR,0,cgpara);
        paramanager.freecgpara(current_asmdata.CurrAsmList,cgpara);
        cgpara.done;
        cg.g_call(current_asmdata.CurrAsmList,'FPC_SAFECALLHANDLER');
        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_FUNCTION_RESULT_REG, NR_FUNCTION_RETURN_REG);
      end;

    procedure tcgtryfinallynode.pass_generate_code;
      var
         endfinallylabel,
         exitfinallylabel,
         continuefinallylabel,
         breakfinallylabel,
         oldCurrExitLabel,
         oldContinueLabel,
         oldBreakLabel : tasmlabel;
         finallyexceptionstate: tcgexceptionstatehandler.texceptionstate;
         excepttemps : tcgexceptionstatehandler.texceptiontemps;
         reasonreg : tregister;
      begin
         location_reset(location,LOC_VOID,OS_NO);
         oldBreakLabel:=nil;
         oldContinueLabel:=nil;
         continuefinallylabel:=nil;
         breakfinallylabel:=nil;

         current_asmdata.getjumplabel(endfinallylabel);

         { call setjmp, and jump to finally label on non-zero result }
         cexceptionstatehandler.get_exception_temps(current_asmdata.CurrAsmList,excepttemps);
         cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,finallyexceptionstate);

         { the finally block must catch break, continue and exit }
         { statements                                            }
         oldCurrExitLabel:=current_procinfo.CurrExitLabel;
         if implicitframe then
           exitfinallylabel:=finallyexceptionstate.exceptionlabel
         else
           current_asmdata.getjumplabel(exitfinallylabel);
         current_procinfo.CurrExitLabel:=exitfinallylabel;
         if assigned(current_procinfo.CurrBreakLabel) then
          begin
            oldContinueLabel:=current_procinfo.CurrContinueLabel;
            oldBreakLabel:=current_procinfo.CurrBreakLabel;
            if implicitframe then
              begin
                breakfinallylabel:=finallyexceptionstate.exceptionlabel;
                continuefinallylabel:=finallyexceptionstate.exceptionlabel;
              end
            else
              begin
                current_asmdata.getjumplabel(breakfinallylabel);
                current_asmdata.getjumplabel(continuefinallylabel);
              end;
            current_procinfo.CurrContinueLabel:=continuefinallylabel;
            current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

         { try code }
         if assigned(left) then
           begin
              secondpass(left);
              if codegenerror then
                exit;
           end;

         { don't generate line info for internal cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

         cexceptionstatehandler.emit_except_label(current_asmdata.CurrAsmList,finallyexceptionstate);
         { just free the frame information }
         cexceptionstatehandler.free_exception(current_asmdata.CurrAsmList,excepttemps,1,finallyexceptionstate.exceptionlabel,true);

         { end cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

         { finally code (don't unconditionally set fc_inflowcontrol, since the
           finally code is unconditionally executed; we do have to filter out
           flags regarding break/contrinue/etc. because we have to give an
           error in case one of those is used in the finally-code }
         flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol];
         secondpass(right);
         { goto is allowed if it stays inside the finally block,
           this is checked using the exception block number }
         if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol]) then
           CGMessage(cg_e_control_flow_outside_finally);
         if codegenerror then
           exit;

         { don't generate line info for internal cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

         { the value should now be in the exception handler }
         reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,osuinttype);
         hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,osuinttype,osuinttype,excepttemps.reasonbuf,reasonreg);
         if implicitframe then
           begin
             hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,osuinttype,OC_EQ,0,reasonreg,endfinallylabel);
             { finally code only needed to be executed on exception (-> in
               if-branch -> fc_inflowcontrol) }
             flowcontrol:=[fc_inflowcontrol];
             if (tf_safecall_exceptions in target_info.flags) and
                (current_procinfo.procdef.proccalloption=pocall_safecall) then
               handle_safecall_exception
             else
                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_reraise',[],nil);
           end
         else
           begin
             hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,osuinttype,OC_EQ,0,reasonreg,endfinallylabel);
             if fc_exit in finallyexceptionstate.newflowcontrol then
               hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,osuinttype,OC_EQ,2,reasonreg,oldCurrExitLabel);
             if fc_break in finallyexceptionstate.newflowcontrol then
               hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,osuinttype,OC_EQ,3,reasonreg,oldBreakLabel);
             if fc_continue in finallyexceptionstate.newflowcontrol then
               hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,osuinttype,OC_EQ,4,reasonreg,oldContinueLabel);
             hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_reraise',[],nil);
             { do some magic for exit,break,continue in the try block }
             if fc_exit in finallyexceptionstate.newflowcontrol then
               begin
                  hlcg.a_label(current_asmdata.CurrAsmList,exitfinallylabel);
                  hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
                  hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,osuinttype,2,excepttemps.reasonbuf);
                  hlcg.a_jmp_always(current_asmdata.CurrAsmList,finallyexceptionstate.exceptionlabel);
               end;
             if fc_break in finallyexceptionstate.newflowcontrol then
              begin
                 hlcg.a_label(current_asmdata.CurrAsmList,breakfinallylabel);
                 hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
                 hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,osuinttype,3,excepttemps.reasonbuf);
                 hlcg.a_jmp_always(current_asmdata.CurrAsmList,finallyexceptionstate.exceptionlabel);
               end;
             if fc_continue in finallyexceptionstate.newflowcontrol then
               begin
                  hlcg.a_label(current_asmdata.CurrAsmList,continuefinallylabel);
                  hlcg.g_exception_reason_discard(current_asmdata.CurrAsmList,osuinttype,excepttemps.reasonbuf);
                  hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,osuinttype,4,excepttemps.reasonbuf);
                  hlcg.a_jmp_always(current_asmdata.CurrAsmList,finallyexceptionstate.exceptionlabel);
               end;
           end;
         cexceptionstatehandler.unget_exception_temps(current_asmdata.CurrAsmList,excepttemps);
         hlcg.a_label(current_asmdata.CurrAsmList,endfinallylabel);

         { end cleanup }
         current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

         current_procinfo.CurrExitLabel:=oldCurrExitLabel;
         if assigned(current_procinfo.CurrBreakLabel) then
          begin
            current_procinfo.CurrContinueLabel:=oldContinueLabel;
            current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;
         flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
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
   cexceptionstatehandler:=tcgexceptionstatehandler;
end.


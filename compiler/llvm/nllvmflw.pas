{
    Copyright (c) 2016 by Jonas Maebe

    Generate assembler for nodes that influence the flow for llvm

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
unit nllvmflw;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symtype,symdef,
      aasmbase,aasmdata,
      cgbase,
      node, nflw, ncgflw, ncgnstfl;

    type
      tllvmlabelnode = class(tcglabelnode)
        function getasmlabel: tasmlabel; override;
      end;

    tllvmexceptionstatehandler = class(tcgexceptionstatehandler)
      class procedure get_exception_temps(list: TAsmList; var t: texceptiontemps); override;
      class procedure unget_exception_temps(list: TAsmList; const t: texceptiontemps); override;
      class procedure new_exception(list: TAsmList; const t: texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate); override;
      class procedure emit_except_label(list: TAsmList; exceptframekind: texceptframekind; var exceptionstate: texceptionstate); override;
      class procedure end_try_block(list: TAsmList; exceptframekind: texceptframekind; const t: texceptiontemps; var exceptionstate: texceptionstate; endlabel: TAsmLabel); override;
      class procedure cleanupobjectstack(list: TAsmList); override;
      class procedure popaddrstack(list: TAsmList); override;
      class procedure handle_reraise(list: TAsmList; const t: texceptiontemps; const entrystate: texceptionstate; const exceptframekind: texceptframekind); override;
      class procedure begin_catch(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; out exceptlocdef: tdef; out exceptlocreg: tregister); override;
      class procedure end_catch(list: TAsmList); override;
      class procedure catch_all_start(list: TAsmList); override;
      class procedure catch_all_end(list: TAsmList); override;
     protected
      class procedure begin_catch_internal(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; add_catch: boolean; out exceptlocdef: tdef; out exceptlocreg: tregister);
      class procedure catch_all_start_internal(list: TAsmList; add_catch: boolean);
      class function use_cleanup(const exceptframekind: texceptframekind): boolean;
    end;

    tllvmtryexceptnode = class(tcgtryexceptnode)
    end;

    tllvmtryfinallynode = class(tcgtryfinallynode)
      function pass_1: tnode; override;
    end;

    tllvmraisenode = class(tcgraisenode)
      function pass_1: tnode; override;
      procedure pass_generate_code; override;
    end;


implementation

    uses
      systems,globals,verbose,
      symconst,symtable,symsym,llvmdef,defutil,
      pass_2,cgutils,hlcgobj,parabase,paramgr,tgobj,
      llvmbase,aasmtai,aasmllvm,
      procinfo,llvmpi;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    function tllvmlabelnode.getasmlabel: tasmlabel;
      begin
        { don't allocate global labels even if the label is accessed from
          another routine: we always have to refer to such labels using the
          blockaddress() construct, which works with local labels too.
          Additionally, LLVM does not support defining global labels in the
          middle of a routine -> jumping to such a label from assembler code
          from another function will not work anyway (have to handle that by
          passing a blockaddress as argument to an assembler block, although
          "some targets may provide defined semantics when using the value as
          the operand to an inline assembly") }
        if not(assigned(asmlabel)) then
          current_asmdata.getjumplabel(asmlabel);
        result:=asmlabel
      end;


{*****************************************************************************
                          tllvmtryfinallynode
*****************************************************************************}

    function tllvmtryfinallynode.pass_1: tnode;
      begin
        { make a copy of the "finally" code for the "no exception happened"
          case }
        if not assigned(third) then
          third:=right.getcopy;
        result:=inherited;
      end;


{*****************************************************************************
                     tllvmexceptionstatehandler
*****************************************************************************}

    class procedure tllvmexceptionstatehandler.get_exception_temps(list: TAsmList; var t: texceptiontemps);
      begin
        tg.gethltemp(list,ossinttype,ossinttype.size,tt_persistent,t.reasonbuf);
      end;


    class procedure tllvmexceptionstatehandler.unget_exception_temps(list: TAsmList; const t: texceptiontemps);
      begin
        tg.ungettemp(list,t.reasonbuf);
        tllvmprocinfo(current_procinfo).poppad;
      end;


    class procedure tllvmexceptionstatehandler.new_exception(list: TAsmList; const t: texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate);
      var
        reg: tregister;
      begin
        exceptstate.oldflowcontrol:=flowcontrol;
        if exceptframekind<>tek_except then
          current_asmdata.getjumplabel(exceptstate.finallycodelabel)
        else
          exceptstate.finallycodelabel:=nil;
        { all calls inside the exception block have to be invokes instead,
          which refer to the exception label:
            exceptionlabel:
              %reg = landingpad ..
              <exception handling code>
        }
        current_asmdata.getjumplabel(exceptstate.exceptionlabel);
        { for consistency checking when popping }
        tllvmprocinfo(current_procinfo).pushexceptlabel(exceptstate.exceptionlabel);
        flowcontrol:=[fc_inflowcontrol,fc_catching_exceptions];
        { the reasonbuf is set to 1 by the generic code if we got in
          the exception block by catching an exception -> do the same here, so
          we can share that generic code; llvm will optimise it away. The
          reasonbuf is later also used for break/continue/... }
        reg:=hlcg.getintregister(list,ossinttype);
        hlcg.a_load_const_reg(list,ossinttype,1,reg);
        hlcg.g_exception_reason_save(list,ossinttype,ossinttype,reg,t.reasonbuf);
        { There can only be a landingpad if there were any invokes in the try-block,
          as otherwise we get an error; we can also generate exceptions from
          invalid memory accesses and the like, but LLVM cannot model that
          --
          We cheat for now by adding an invoke to a dummy routine at the start and at
          the end of the try-block. That will not magically fix the state
          of all variables when the exception gets caught though. }
        hlcg.g_call_system_proc(list,'FPC_DUMMYPOTENTIALRAISE',[],nil).resetiftemp;
      end;


    class procedure tllvmexceptionstatehandler.emit_except_label(list: TAsmList; exceptframekind: texceptframekind; var exceptionstate: texceptionstate);
      var
        reg: tregister;
        landingpad: taillvm;
        landingpaddef: trecorddef;
      begin
        hlcg.g_unreachable(list);
        hlcg.a_label(list,exceptionstate.exceptionlabel);
        { use packrecords 1 because we don't want padding (LLVM 4.0+ requires
          exactly two fields in this struct) }
        landingpaddef:=llvmgettemprecorddef([voidpointertype,u32inttype],
          1,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        reg:=hlcg.getregisterfordef(list,landingpaddef);
        landingpad:=taillvm.landingpad(reg,landingpaddef,{clause}nil);
        list.concat(landingpad);
        if exceptframekind<>tek_except then
          begin
            if not assigned(exceptionstate.finallycodelabel) then
              internalerror(2018111102);
            if use_cleanup(exceptframekind) then
              landingpad.landingpad_add_clause(la_cleanup, nil, nil)
            else
              landingpad.landingpad_add_clause(la_catch, voidpointertype, nil);
            hlcg.a_label(list,exceptionstate.finallycodelabel);
            exceptionstate.finallycodelabel:=nil;
          end;
        { consistency check }
        tllvmprocinfo(current_procinfo).popexceptlabel(exceptionstate.exceptionlabel);
        tllvmprocinfo(current_procinfo).pushlandingpad(landingpad);
      end;


    class procedure tllvmexceptionstatehandler.end_try_block(list: TAsmList; exceptframekind: texceptframekind; const t: texceptiontemps; var exceptionstate: texceptionstate; endlabel: TAsmLabel);
      var
        reg: tregister;
      begin
        { llvm does not allow creating a landing pad if there are no invokes in
          the try block -> create a call to a dummy routine that cannot be
          analysed by llvm and that supposedly may raise an exception. Has to
          be combined with marking stores inside try blocks as volatile and the
          loads afterwards as well in order to guarantee correct optimizations
          in case an exception gets triggered inside a try-block though }
        hlcg.g_call_system_proc(list,'FPC_DUMMYPOTENTIALRAISE',[],nil).resetiftemp;

        { record that no exception happened in the reason buf }
        reg:=hlcg.getintregister(list,ossinttype);
        hlcg.a_load_const_reg(list,ossinttype,0,reg);
        hlcg.g_exception_reason_save(list,ossinttype,ossinttype,reg,t.reasonbuf);
        inherited;
        if exceptframekind=tek_except then
          hlcg.a_jmp_always(list,endlabel);
      end;

    class procedure tllvmexceptionstatehandler.cleanupobjectstack(list: TAsmList);
      var
        landingpad: taillvm;
      begin
        { if not a single catch block added -> catch all }
        landingpad:=tllvmprocinfo(current_procinfo).currlandingpad;
        if assigned(landingpad) and
           not assigned(landingpad.oper[2]^.ai) then
          begin
            landingpad.landingpad_add_clause(la_catch,voidpointertype,nil);
          end;
      end;

    class procedure tllvmexceptionstatehandler.popaddrstack(list: TAsmList);
      begin
        // nothing
      end;


    class procedure tllvmexceptionstatehandler.handle_reraise(list: TAsmList; const t: texceptiontemps; const entrystate: texceptionstate; const exceptframekind: texceptframekind);
      var
        landingpad: taillvm;
        landingpadres: tregister;
        landingpadresdef: tdef;
      begin
        { We use resume to propagate the exception to an outer function frame, and call
          reraise in case we are nested in another exception frame in the current function
          (because then we will emit an invoke which will tie this re-raise to that other
           exception frame; that is impossible to do with a resume instruction).

          Furthermore, the resume opcode only works for landingpads with a cleanup clause,
          which we only generate for outer implicitfinally frames }
        if not(fc_catching_exceptions in flowcontrol) and
           use_cleanup(exceptframekind) then
          begin
            { resume <result from catchpad> }
            landingpad:=tllvmprocinfo(current_procinfo).currlandingpad;
            landingpadres:=landingpad.oper[0]^.reg;
            landingpadresdef:=landingpad.oper[1]^.def;
            list.concat(taillvm.op_size_reg(la_resume,landingpadresdef,landingpadres));
          end
        else
          begin
            { Need a begin_catch so that the reraise will know what exception to throw.
              Don't need to add a "catch all" to the landing pad, as it contains one.
              We want to rethrow whatever exception was caught rather than guarantee
              that all possible kinds of exceptions get caught. }
            catch_all_start_internal(list,false);
            hlcg.g_call_system_proc(list,'fpc_reraise',[],nil).resetiftemp;
          end;
      end;


    class procedure tllvmexceptionstatehandler.begin_catch(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; out exceptlocdef: tdef; out exceptlocreg: tregister);
      begin
        begin_catch_internal(list,excepttype,nextonlabel,true,exceptlocdef,exceptlocreg);
      end;


    class procedure tllvmexceptionstatehandler.end_catch(list: TAsmList);
      begin
        hlcg.g_call_system_proc(list,'fpc_psabi_end_catch',[],nil).resetiftemp;
        inherited;
      end;


    class procedure tllvmexceptionstatehandler.catch_all_start(list: TAsmList);
      begin
        catch_all_start_internal(list,true);
      end;


    class procedure tllvmexceptionstatehandler.catch_all_end(list: TAsmList);
      begin
        hlcg.g_call_system_proc(list,'fpc_psabi_end_catch',[],nil).resetiftemp;
      end;


    class procedure tllvmexceptionstatehandler.begin_catch_internal(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; add_catch: boolean; out exceptlocdef: tdef; out exceptlocreg: tregister);
      var
        catchstartlab: tasmlabel;
        landingpad: taillvm;
        begincatchres,
        typeidres,
        paraloc1: tcgpara;
        pd: tprocdef;
        landingpadstructdef,
        landingpadtypeiddef: tdef;
        rttisym: TAsmSymbol;
        rttidef: tdef;
        rttiref: treference;
        wrappedexception,
        exceptiontypeidreg,
        landingpadres: tregister;
        exceptloc: tlocation;
        indirect: boolean;
        otherunit: boolean;
      begin
        paraloc1.init;
        landingpad:=tllvmprocinfo(current_procinfo).currlandingpad;
        rttidef:=nil;
        rttisym:=nil;
        if add_catch then
          begin
            if assigned(excepttype) then
              begin
                otherunit:=findunitsymtable(excepttype.owner).moduleid<>findunitsymtable(current_procinfo.procdef.owner).moduleid;
                indirect:=(tf_supports_packages in target_info.flags) and
                        (target_info.system in systems_indirect_var_imports) and
                        (cs_imported_data in current_settings.localswitches) and
                        otherunit;
                { add "catch exceptiontype" clause to the landing pad }
                rttidef:=cpointerdef.getreusable(excepttype.vmt_def);
                rttisym:=current_asmdata.RefAsmSymbol(excepttype.vmt_mangledname, AT_DATA, indirect);
                landingpad.landingpad_add_clause(la_catch,rttidef,rttisym);
              end
            else
              begin
                landingpad.landingpad_add_clause(la_catch,voidpointertype,nil);
              end;
          end;
        { pascal_exception := FPC_psabi_begin_catch(wrappedExceptionObject) where
          wrappedExceptionObject is the exception returned by the landingpad }
        landingpadres:=landingpad.oper[0]^.reg;
        landingpadstructdef:=landingpad.oper[1]^.def;
        { check if the exception is handled by this node }
        if assigned(excepttype) then
          begin
            landingpadtypeiddef:=tfieldvarsym(trecorddef(landingpadstructdef).symtable.symlist[1]).vardef;
            exceptiontypeidreg:=hlcg.getaddressregister(list,landingpadtypeiddef);
            pd:=search_system_proc('llvm_eh_typeid_for');
            paramanager.getintparaloc(list,pd,1,paraloc1);
            reference_reset_symbol(rttiref,rttisym,0,rttidef.alignment,[]);
            rttiref.refaddr:=addr_full;
            hlcg.a_load_ref_cgpara(list,cpointerdef.getreusable(rttidef),rttiref,paraloc1);
            typeidres:=hlcg.g_call_system_proc(list,pd,[@paraloc1],nil);
            location_reset(exceptloc, LOC_REGISTER, def_cgsize(landingpadtypeiddef));
            exceptloc.register:=hlcg.getintregister(list,landingpadtypeiddef);
            hlcg.gen_load_cgpara_loc(list, landingpadtypeiddef, typeidres, exceptloc, true);
            list.concat(taillvm.extract(la_extractvalue,exceptiontypeidreg,landingpadstructdef,landingpadres,1));
            current_asmdata.getjumplabel(catchstartlab);
            hlcg.a_cmp_reg_loc_label(list,typeidres.Def,OC_EQ,exceptiontypeidreg,exceptloc,catchstartlab);
            hlcg.a_jmp_always(list,nextonlabel);
            hlcg.a_label(list,catchstartlab);
            typeidres.resetiftemp;
          end;

        wrappedexception:=hlcg.getaddressregister(list,voidpointertype);
        list.concat(taillvm.extract(la_extractvalue,wrappedexception,landingpadstructdef,landingpadres,0));

        pd:=search_system_proc('fpc_psabi_begin_catch');
        paramanager.getintparaloc(list, pd, 1, paraloc1);
        hlcg.a_load_reg_cgpara(list,voidpointertype,wrappedexception,paraloc1);
        begincatchres:=hlcg.g_call_system_proc(list,pd,[@paraloc1],nil);
        location_reset(exceptloc, LOC_REGISTER, def_cgsize(begincatchres.def));
        exceptloc.register:=hlcg.getaddressregister(list, begincatchres.def);
        hlcg.gen_load_cgpara_loc(list, begincatchres.def, begincatchres, exceptloc, true);

        begincatchres.resetiftemp;
        paraloc1.done;

        exceptlocdef:=begincatchres.def;
        exceptlocreg:=exceptloc.register;
      end;


    class procedure tllvmexceptionstatehandler.catch_all_start_internal(list: TAsmList; add_catch: boolean);
      var
        exceptlocdef: tdef;
        exceptlocreg: tregister;
      begin
        begin_catch_internal(list,nil,nil,add_catch,exceptlocdef,exceptlocreg);
      end;


    class function tllvmexceptionstatehandler.use_cleanup(const exceptframekind: texceptframekind): boolean;
      begin
        { in case of an exception caught by the implicit exception frame of
          a safecall routine, this is not a cleanup frame but one that
          catches the exception and returns a value from the function }
        result:=
          (exceptframekind=tek_implicitfinally) and
          not((tf_safecall_exceptions in target_info.flags) and
             (current_procinfo.procdef.proccalloption=pocall_safecall));
      end;


{*****************************************************************************
                     tllvmexceptionstatehandler
*****************************************************************************}

    function tllvmraisenode.pass_1: tnode;
      begin
        if assigned(left) then
          result:=inherited
        else
          begin
            expectloc:=LOC_VOID;
            result:=nil;
          end;
      end;


    procedure tllvmraisenode.pass_generate_code;
      var
        currexceptlabel: tasmlabel;
      begin
        location_reset(location,LOC_VOID,OS_NO);
        currexceptlabel:=nil;
        { a reraise must raise the exception to the parent exception frame }
        if fc_catching_exceptions in flowcontrol then
          begin
            currexceptlabel:=tllvmprocinfo(current_procinfo).CurrExceptLabel;
            if tllvmprocinfo(current_procinfo).popexceptlabel(currexceptlabel) then
              exclude(flowcontrol,fc_catching_exceptions);
          end;
        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_reraise',[],nil).resetiftemp;
        if assigned(currexceptlabel) then
          begin
            tllvmprocinfo(current_procinfo).pushexceptlabel(currexceptlabel);
            include(flowcontrol,fc_catching_exceptions);
          end;
      end;


begin
  clabelnode:=tllvmlabelnode;
  ctryexceptnode:=tllvmtryexceptnode;
  ctryfinallynode:=tllvmtryfinallynode;
  cexceptionstatehandler:=tllvmexceptionstatehandler;
  craisenode:=tllvmraisenode;
end.


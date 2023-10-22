{
    Copyright (c) 2002-2010 by Florian Klaempfl and Jonas Maebe

    This unit contains the CPU specific part of tprocinfo

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
unit cpupi;

{$i fpcdefs.inc}

interface

  uses
    cutils,globtype,aasmdata,aasmcpu,aasmtai,
    procinfo,cpubase,cpuinfo, symtype,aasmbase,cgbase,
    psub, cclasses;

  type

    { tcpuprocinfo }

    tcpuprocinfo=class(tcgprocinfo)
    private
      FFirstFreeLocal: Integer;
      FAllocatedLocals: array of TWasmBasicType;
      FGotoTargets: TFPHashObjectList;

      function ConvertBranchTargetNumbersToLabels(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;
      function ConvertIfToBrIf(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;
      function ConvertLoopToBr(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;
      function StripBlockInstructions(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;

      { used for allocating locals during the postprocess_code stage (i.e. after register allocation) }
      function AllocWasmLocal(wbt: TWasmBasicType): Integer;
    public
      { label to the nearest local exception handler }
      CurrRaiseLabel : tasmlabel;

      constructor create(aparent: tprocinfo); override;
      destructor destroy; override;
      function calc_stackframe_size : longint;override;
      procedure setup_eh; override;
      procedure generate_exit_label(list: tasmlist); override;
      procedure postprocess_code; override;
      procedure set_first_temp_offset;override;
      procedure add_goto_target(l : tasmlabel);
      function is_goto_target(l : tasmsymbol): Boolean;
    end;

implementation

    uses
      systems,verbose,globals,tgcpu,cgexcept,
      tgobj,paramgr,symconst,symdef,symtable,symcpu,cgutils,pass_2,parabase,
      fmodule,hlcgobj,hlcgcpu,defutil,itcpugas;

{*****************************************************************************
                     twasmexceptionstatehandler_noexceptions
*****************************************************************************}

    type

      { twasmexceptionstatehandler_noexceptions }

      twasmexceptionstatehandler_noexceptions = class(tcgexceptionstatehandler)
        class procedure get_exception_temps(list:TAsmList;var t:texceptiontemps); override;
        class procedure unget_exception_temps(list:TAsmList;const t:texceptiontemps); override;
        class procedure new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate); override;
        class procedure free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean); override;
        class procedure handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate); override;
      end;

    class procedure twasmexceptionstatehandler_noexceptions.get_exception_temps(list:TAsmList;var t:texceptiontemps);
      begin
        if not assigned(exceptionreasontype) then
          exceptionreasontype:=search_system_proc('fpc_setjmp').returndef;
        reference_reset(t.envbuf,0,[]);
        reference_reset(t.jmpbuf,0,[]);
        tg.gethltemp(list,exceptionreasontype,exceptionreasontype.size,tt_persistent,t.reasonbuf);
      end;

    class procedure twasmexceptionstatehandler_noexceptions.unget_exception_temps(list:TAsmList;const t:texceptiontemps);
      begin
        tg.ungettemp(list,t.reasonbuf);
      end;

    class procedure twasmexceptionstatehandler_noexceptions.new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate);
      begin
        exceptstate.exceptionlabel:=nil;
        exceptstate.oldflowcontrol:=flowcontrol;
        exceptstate.finallycodelabel:=nil;

        flowcontrol:=[fc_inflowcontrol,fc_catching_exceptions];
      end;

    class procedure twasmexceptionstatehandler_noexceptions.free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean);
      begin
      end;

    class procedure twasmexceptionstatehandler_noexceptions.handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate);
      begin
        list.Concat(tai_comment.Create(strpnew('TODO: handle_nested_exception')));
      end;

{*****************************************************************************
                     twasmexceptionstatehandler_jsexceptions
*****************************************************************************}

    type
      twasmexceptionstatehandler_jsexceptions = class(tcgexceptionstatehandler)
        class procedure get_exception_temps(list:TAsmList;var t:texceptiontemps); override;
        class procedure unget_exception_temps(list:TAsmList;const t:texceptiontemps); override;
        class procedure new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate); override;
        class procedure free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean); override;
        class procedure handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate); override;
      end;

    class procedure twasmexceptionstatehandler_jsexceptions.get_exception_temps(list:TAsmList;var t:texceptiontemps);
      begin
        if not assigned(exceptionreasontype) then
          exceptionreasontype:=search_system_proc('fpc_setjmp').returndef;
        reference_reset(t.envbuf,0,[]);
        reference_reset(t.jmpbuf,0,[]);
        tg.gethltemp(list,exceptionreasontype,exceptionreasontype.size,tt_persistent,t.reasonbuf);
      end;

    class procedure twasmexceptionstatehandler_jsexceptions.unget_exception_temps(list:TAsmList;const t:texceptiontemps);
      begin
        tg.ungettemp(list,t.reasonbuf);
      end;

    class procedure twasmexceptionstatehandler_jsexceptions.new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate);
      begin
        exceptstate.exceptionlabel:=nil;
        exceptstate.oldflowcontrol:=flowcontrol;
        exceptstate.finallycodelabel:=nil;

        flowcontrol:=[fc_inflowcontrol,fc_catching_exceptions];
      end;

    class procedure twasmexceptionstatehandler_jsexceptions.free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean);
      begin
      end;

    class procedure twasmexceptionstatehandler_jsexceptions.handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate);
      begin
        list.Concat(tai_comment.Create(strpnew('TODO: handle_nested_exception')));
      end;

{*****************************************************************************
                     twasmexceptionstatehandler_nativeexceptions
*****************************************************************************}

    type

      { twasmexceptionstatehandler_nativeexceptions }

      twasmexceptionstatehandler_nativeexceptions = class(tcgexceptionstatehandler)
        class procedure new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate); override;
        class procedure free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean); override;
        class procedure handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate); override;
        { start of an "on" (catch) block }
        class procedure begin_catch(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; out exceptlocdef: tdef; out exceptlocreg: tregister); override;
        { end of an "on" (catch) block }
        class procedure end_catch(list: TAsmList); override;
      end;

    class procedure twasmexceptionstatehandler_nativeexceptions.new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate);
      begin
        exceptstate.exceptionlabel:=nil;
        exceptstate.oldflowcontrol:=flowcontrol;
        exceptstate.finallycodelabel:=nil;

        flowcontrol:=[fc_inflowcontrol,fc_catching_exceptions];
      end;

    class procedure twasmexceptionstatehandler_nativeexceptions.free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean);
      begin
      end;

    class procedure twasmexceptionstatehandler_nativeexceptions.handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate);
      begin
        Message1(parser_f_unsupported_feature,'nested exception');
      end;

    class procedure twasmexceptionstatehandler_nativeexceptions.begin_catch(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; out exceptlocdef: tdef; out exceptlocreg: tregister);
      var
        pd: tprocdef;
        href2: treference;
        fpc_catches_res,
        paraloc1: tcgpara;
        exceptloc: tlocation;
        indirect: boolean;
        otherunit: boolean;
      begin
        paraloc1.init;
        otherunit:=findunitsymtable(excepttype.owner).moduleid<>findunitsymtable(current_procinfo.procdef.owner).moduleid;
        indirect:=(tf_supports_packages in target_info.flags) and
                    (target_info.system in systems_indirect_var_imports) and
                    (cs_imported_data in current_settings.localswitches) and
                    otherunit;

        { send the vmt parameter }
        pd:=search_system_proc('fpc_catches');
        reference_reset_symbol(href2, current_asmdata.RefAsmSymbol(excepttype.vmt_mangledname, AT_DATA, indirect), 0, sizeof(pint), []);
        if otherunit then
          current_module.add_extern_asmsym(excepttype.vmt_mangledname, AB_EXTERNAL, AT_DATA);
        paramanager.getcgtempparaloc(list, pd, 1, paraloc1);
        hlcg.a_loadaddr_ref_cgpara(list, excepttype.vmt_def, href2, paraloc1);
        paramanager.freecgpara(list, paraloc1);
        fpc_catches_res:=hlcg.g_call_system_proc(list, pd, [@paraloc1], nil);
        location_reset(exceptloc, LOC_REGISTER, def_cgsize(fpc_catches_res.def));
        exceptloc.register:=hlcg.getaddressregister(list, fpc_catches_res.def);
        hlcg.gen_load_cgpara_loc(list, fpc_catches_res.def, fpc_catches_res, exceptloc, true);

        { is it this catch? }
        thlcgwasm(hlcg).a_cmp_const_reg_stack(list, fpc_catches_res.def, OC_NE, 0, exceptloc.register);

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_if));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

        paraloc1.done;

        exceptlocdef:=fpc_catches_res.def;
        exceptlocreg:=exceptloc.register;
      end;

    class procedure twasmexceptionstatehandler_nativeexceptions.end_catch(list: TAsmList);
      begin
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_if));
      end;

{*****************************************************************************
                     twasmexceptionstatehandler_bfexceptions
*****************************************************************************}

    type

      { twasmexceptionstatehandler_bfexceptions }

      twasmexceptionstatehandler_bfexceptions = class(tcgexceptionstatehandler)
        class procedure new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate); override;
        class procedure free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean); override;
        class procedure handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate); override;
        { start of an "on" (catch) block }
        class procedure begin_catch(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; out exceptlocdef: tdef; out exceptlocreg: tregister); override;
        { end of an "on" (catch) block }
        class procedure end_catch(list: TAsmList); override;
      end;

    class procedure twasmexceptionstatehandler_bfexceptions.new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate);
      begin
        exceptstate.exceptionlabel:=nil;
        exceptstate.oldflowcontrol:=flowcontrol;
        exceptstate.finallycodelabel:=nil;

        flowcontrol:=[fc_inflowcontrol,fc_catching_exceptions];
      end;

    class procedure twasmexceptionstatehandler_bfexceptions.free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean);
      begin
      end;

    class procedure twasmexceptionstatehandler_bfexceptions.handle_nested_exception(list:TAsmList;var t:texceptiontemps;var entrystate: texceptionstate);
      begin
        Message1(parser_f_unsupported_feature,'nested exception');
      end;

    class procedure twasmexceptionstatehandler_bfexceptions.begin_catch(list: TAsmList; excepttype: tobjectdef; nextonlabel: tasmlabel; out exceptlocdef: tdef; out exceptlocreg: tregister);
      var
        pd: tprocdef;
        href2: treference;
        fpc_catches_res,
        paraloc1: tcgpara;
        exceptloc: tlocation;
        indirect: boolean;
        otherunit: boolean;
      begin
        paraloc1.init;
        otherunit:=findunitsymtable(excepttype.owner).moduleid<>findunitsymtable(current_procinfo.procdef.owner).moduleid;
        indirect:=(tf_supports_packages in target_info.flags) and
                    (target_info.system in systems_indirect_var_imports) and
                    (cs_imported_data in current_settings.localswitches) and
                    otherunit;

        { send the vmt parameter }
        pd:=search_system_proc('fpc_catches');
        reference_reset_symbol(href2, current_asmdata.RefAsmSymbol(excepttype.vmt_mangledname, AT_DATA, indirect), 0, sizeof(pint), []);
        if otherunit then
          current_module.add_extern_asmsym(excepttype.vmt_mangledname, AB_EXTERNAL, AT_DATA);
        paramanager.getcgtempparaloc(list, pd, 1, paraloc1);
        hlcg.a_loadaddr_ref_cgpara(list, excepttype.vmt_def, href2, paraloc1);
        paramanager.freecgpara(list, paraloc1);
        fpc_catches_res:=hlcg.g_call_system_proc(list, pd, [@paraloc1], nil);
        location_reset(exceptloc, LOC_REGISTER, def_cgsize(fpc_catches_res.def));
        exceptloc.register:=hlcg.getaddressregister(list, fpc_catches_res.def);
        hlcg.gen_load_cgpara_loc(list, fpc_catches_res.def, fpc_catches_res, exceptloc, true);

        { is it this catch? }
        thlcgwasm(hlcg).a_cmp_const_reg_stack(list, fpc_catches_res.def, OC_NE, 0, exceptloc.register);

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_if));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

        paraloc1.done;

        exceptlocdef:=fpc_catches_res.def;
        exceptlocreg:=exceptloc.register;
      end;

    class procedure twasmexceptionstatehandler_bfexceptions.end_catch(list: TAsmList);
      begin
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_if));
      end;

{*****************************************************************************
                             twasmblockitem
*****************************************************************************}

    type

      { twasmblockitem }

      twasmblockitem = class(TLinkedListItem)
        blockstart: taicpu;
        elseinstr: taicpu;
        constructor Create(ablockstart: taicpu);
      end;

      constructor twasmblockitem.Create(ablockstart: taicpu);
        begin
          blockstart:=ablockstart;
        end;

{*****************************************************************************
                             twasmblockstack
*****************************************************************************}

    type

      { twasmblockstack }

      twasmblockstack = class(tlinkedlist)

      end;

{*****************************************************************************
                           tcpuprocinfo
*****************************************************************************}

    function tcpuprocinfo.ConvertBranchTargetNumbersToLabels(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;
      var
        instr: taicpu;
        bl: taicpu_wasm_structured_instruction;
        l: TAsmLabel;
      begin
        result.typ:=amfrtNoChange;
        if ai.typ<>ait_instruction then
          exit;
        instr:=taicpu(ai);
        if not (instr.opcode in [a_br,a_br_if]) then
          exit;
        if instr.ops<>1 then
          internalerror(2023101601);
        if instr.oper[0]^.typ<>top_const then
          exit;
        bl:=blockstack[instr.oper[0]^.val];
        l:=bl.getlabel;
        instr.loadsymbol(0,l,0);
      end;

    function tcpuprocinfo.ConvertIfToBrIf(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;
      begin
        result.typ:=amfrtNoChange;
        if (ai.typ=ait_wasm_structured_instruction) and (taicpu_wasm_structured_instruction(ai).wstyp=aitws_if) then
          begin
            result.typ:=amfrtNewList;
            result.newlist:=TAsmList.Create;
            tai_wasmstruc_if(ai).ConvertToBrIf(result.newlist,@AllocWasmLocal);
          end;
      end;

    function tcpuprocinfo.ConvertLoopToBr(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;
      begin
        result.typ:=amfrtNoChange;
        if (ai.typ=ait_wasm_structured_instruction) and (taicpu_wasm_structured_instruction(ai).wstyp=aitws_loop) then
          begin
            result.typ:=amfrtNewList;
            result.newlist:=TAsmList.Create;
            tai_wasmstruc_loop(ai).ConvertToBr(result.newlist);
          end;
      end;

    function tcpuprocinfo.StripBlockInstructions(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult;
      var
        instr: taicpu;
      begin
        result.typ:=amfrtNoChange;
        if ai.typ<>ait_instruction then
          exit;
        instr:=taicpu(ai);
        if instr.opcode in [a_block,a_end_block] then
          result.typ:=amfrtDeleteAi;
      end;

    function tcpuprocinfo.AllocWasmLocal(wbt: TWasmBasicType): Integer;
      begin
        SetLength(FAllocatedLocals,Length(FAllocatedLocals)+1);
        FAllocatedLocals[High(FAllocatedLocals)]:=wbt;
        result:=High(FAllocatedLocals)+FFirstFreeLocal;
      end;

    constructor tcpuprocinfo.create(aparent: tprocinfo);
      begin
        inherited create(aparent);
        FGotoTargets:=TFPHashObjectList.Create(false);
        if ts_wasm_bf_exceptions in current_settings.targetswitches then
          current_asmdata.getjumplabel(CurrRaiseLabel);
      end;

    destructor tcpuprocinfo.destroy;
      begin
        FGotoTargets.Free;
        inherited destroy;
      end;

    function tcpuprocinfo.calc_stackframe_size: longint;
      begin
        { the stack frame in WebAssembly should always have a 16-byte alignment }
        Result:=Align(inherited calc_stackframe_size,16);
      end;

    procedure tcpuprocinfo.setup_eh;
      begin
        if ts_wasm_native_exceptions in current_settings.targetswitches then
          cexceptionstatehandler:=twasmexceptionstatehandler_nativeexceptions
        else if ts_wasm_js_exceptions in current_settings.targetswitches then
          cexceptionstatehandler:=twasmexceptionstatehandler_jsexceptions
        else if ts_wasm_no_exceptions in current_settings.targetswitches then
          cexceptionstatehandler:=twasmexceptionstatehandler_noexceptions
        else if ts_wasm_bf_exceptions in current_settings.targetswitches then
          cexceptionstatehandler:=twasmexceptionstatehandler_bfexceptions
        else
          internalerror(2021091701);
      end;

    procedure tcpuprocinfo.generate_exit_label(list: tasmlist);
      begin
        list.concat(taicpu.op_none(a_end_block));
        inherited generate_exit_label(list);
      end;

    procedure tcpuprocinfo.postprocess_code;

      function findfirst_tai_functype(asmlist: TAsmList): tai_functype;
        var
          hp: tai;
        begin
          result:=nil;
          if not assigned(asmlist) then
            exit;
          hp:=tai(asmlist.first);
          while assigned(hp) do
            begin
              if hp.typ=ait_functype then
                begin
                  result:=tai_functype(hp);
                  exit;
                end;
              hp:=tai(hp.Next);
            end;
        end;

      procedure replace_local_frame_pointer(asmlist: TAsmList);
        var
          hp: tai;
          instr: taicpu;
          l: Integer;
        begin
          if not assigned(asmlist) then
            exit;
          hp:=tai(asmlist.first);
          while assigned(hp) do
            begin
              if hp.typ=ait_instruction then
                begin
                  instr:=taicpu(hp);
                  for l:=0 to instr.ops-1 do
                    if (instr.oper[l]^.typ=top_reg) and (instr.oper[l]^.reg=NR_LOCAL_FRAME_POINTER_REG) then
                      instr.loadref(l,tcpuprocdef(current_procinfo.procdef).frame_pointer_ref);
                end;
              hp:=tai(hp.Next);
            end;
        end;

      function FindNextInstruction(hp: tai): taicpu;
        begin
          result:=nil;
          if not assigned(hp) then
            exit;
          repeat
            hp:=tai(hp.next);
          until not assigned(hp) or (hp.typ=ait_instruction);
          if assigned(hp) then
            result:=taicpu(hp);
        end;

      procedure resolve_labels_pass1(asmlist: TAsmList);
        var
          hp: tai;
          lastinstr, nextinstr: taicpu;
          cur_nesting_depth: longint;
          lbl: tai_label;
          blockstack: twasmblockstack;
          cblock: twasmblockitem;
        begin
          blockstack:=twasmblockstack.create;
          cur_nesting_depth:=0;
          lastinstr:=nil;
          hp:=tai(asmlist.first);
          while assigned(hp) do
            begin
              case hp.typ of
                ait_instruction:
                  begin
                    lastinstr:=taicpu(hp);
                    case lastinstr.opcode of
                      a_block,
                      a_loop,
                      a_if,
                      a_try:
                        begin
                          blockstack.Concat(twasmblockitem.create(lastinstr));
                          inc(cur_nesting_depth);
                        end;

                      a_else:
                        begin
                          cblock:=twasmblockitem(blockstack.Last);
                          if (cblock=nil) or
                             (cblock.blockstart.opcode<>a_if) or
                             assigned(cblock.elseinstr) then
                            Message1(parser_f_unsupported_feature,'misplaced a_else');
                          cblock.elseinstr:=lastinstr;
                        end;

                      a_end_block,
                      a_end_loop,
                      a_end_if,
                      a_end_try:
                        begin
                          dec(cur_nesting_depth);
                          if cur_nesting_depth<0 then
                            Message1(parser_f_unsupported_feature,'negative nesting level');
                          cblock:=twasmblockitem(blockstack.GetLast);
                          if (cblock=nil) or
                             ((cblock.blockstart.opcode=a_block) and (lastinstr.opcode<>a_end_block)) or
                             ((cblock.blockstart.opcode=a_loop) and (lastinstr.opcode<>a_end_loop)) or
                             ((cblock.blockstart.opcode=a_if) and (lastinstr.opcode<>a_end_if)) or
                             ((cblock.blockstart.opcode=a_try) and (lastinstr.opcode<>a_end_try)) then
                            Message1(parser_f_unsupported_feature,'incompatible nesting level');
                          cblock.free;
                        end;

                      else
                        ;
                    end;
                  end;
                ait_label:
                  begin
                    lbl:=tai_label(hp);
                    lbl.labsym.nestingdepth:=-1;
                    nextinstr:=FindNextInstruction(hp);

                    if assigned(nextinstr) and (nextinstr.opcode in [a_end_block,a_end_try,a_end_if]) then
                      lbl.labsym.nestingdepth:=cur_nesting_depth
                    else if assigned(lastinstr) and (lastinstr.opcode=a_loop) then
                      lbl.labsym.nestingdepth:=cur_nesting_depth
                    else if assigned(lastinstr) and (lastinstr.opcode in [a_end_block,a_end_try,a_end_if]) then
                      lbl.labsym.nestingdepth:=cur_nesting_depth+1
                    else if assigned(nextinstr) and (nextinstr.opcode=a_loop) then
                      lbl.labsym.nestingdepth:=cur_nesting_depth+1;
                  end;
                else
                  ;
              end;
              hp:=tai(hp.Next);
            end;
          if cur_nesting_depth<>0 then
            Message1(parser_f_unsupported_feature,'unbalanced nesting level');
          blockstack.free;
        end;

      function resolve_labels_pass2(asmlist: TAsmList): Boolean;
        var
          hp: tai;
          instr: taicpu;
          hlabel: tasmsymbol;
          cur_nesting_depth: longint;
        begin
          Result:=true;
          cur_nesting_depth:=0;
          hp:=tai(asmlist.first);
          while assigned(hp) do
            begin
              if hp.typ=ait_instruction then
                begin
                  instr:=taicpu(hp);
                  case instr.opcode of
                    a_block,
                    a_loop,
                    a_if,
                    a_try:
                      inc(cur_nesting_depth);

                    a_end_block,
                    a_end_loop,
                    a_end_if,
                    a_end_try:
                      begin
                        dec(cur_nesting_depth);
                        if cur_nesting_depth<0 then
                          Message1(parser_f_unsupported_feature,'negative nesting level');
                      end;

                    a_br,
                    a_br_if:
                      begin
                        if instr.ops<>1 then
                          Message1(parser_f_unsupported_feature,'a_br or a_br_if with wrong operand count');
                        if instr.oper[0]^.typ=top_ref then
                          begin
                            if not assigned(instr.oper[0]^.ref^.symbol) then
                              Message1(parser_f_unsupported_feature,'a_br or a_br_if with wrong ref operand');
                            if (instr.oper[0]^.ref^.base<>NR_NO) or
                               (instr.oper[0]^.ref^.index<>NR_NO) or
                               (instr.oper[0]^.ref^.offset<>0) then
                              Message1(parser_f_unsupported_feature,'a_br or a_br_if with wrong ref type');
                            if (instr.oper[0]^.ref^.symbol.nestingdepth<>-1) and
                               (cur_nesting_depth>=instr.oper[0]^.ref^.symbol.nestingdepth) then
                              instr.loadconst(0,cur_nesting_depth-instr.oper[0]^.ref^.symbol.nestingdepth)
                            else
                              begin
                                result:=false;
                                hlabel:=tasmsymbol(instr.oper[0]^.ref^.symbol);
                                asmlist.insertafter(tai_comment.create(strpnew('Unable to find destination of label '+hlabel.name)),hp);
                              end;
                          end;
                      end;

                    else
                      ;
                  end;
                end;
              hp:=tai(hp.Next);
            end;
          if cur_nesting_depth<>0 then
            Message1(parser_f_unsupported_feature,'unbalanced nesting level');
        end;

      function resolve_labels_simple(asmlist: TAsmList): Boolean;
        begin
          if not assigned(asmlist) then
            exit(true);
          resolve_labels_pass1(asmlist);
          result:=resolve_labels_pass2(asmlist);
        end;

      procedure resolve_labels_via_state_machine(asmlist: TAsmList);
        var
          blocks: TFPHashObjectList;
          curr_block, tmplist: TAsmList;
          hp, hpnext: tai;
          block_nr, machine_state, target_block_index: Integer;
          state_machine_loop_start_label, state_machine_exit: TAsmLabel;
        begin
          blocks:=TFPHashObjectList.Create;
          curr_block:=TAsmList.Create;
          blocks.Add('.start',curr_block);
          repeat
            hp:=tai(asmlist.First);
            if assigned(hp) then
              begin
                asmlist.Remove(hp);
                if hp.typ=ait_label then
                  begin
                    curr_block:=TAsmList.Create;
                    blocks.Add(tai_label(hp).labsym.Name,curr_block);
                  end;
                curr_block.Concat(hp);
              end;
          until not assigned(hp);
          { asmlist is now empty }
          asmlist.Concat(tai_comment.Create(strpnew('labels resolved via state machine')));
          machine_state:=AllocWasmLocal(wbt_i32);
          asmlist.Concat(tai_comment.Create(strpnew('machine state is in local '+tostr(machine_state))));
          asmlist.Concat(taicpu.op_const(a_i32_const,0));
          asmlist.Concat(taicpu.op_const(a_local_set,machine_state));
          asmlist.Concat(taicpu.op_none(a_block));
          asmlist.Concat(taicpu.op_none(a_loop));
          current_asmdata.getjumplabel(state_machine_loop_start_label);
          asmlist.concat(tai_label.create(state_machine_loop_start_label));
          current_asmdata.getjumplabel(state_machine_exit);
          for block_nr:=0 to blocks.Count-1 do
            asmlist.Concat(taicpu.op_none(a_block));
          for block_nr:=0 to blocks.Count-1 do
            begin
              { TODO: this sequence can be replaced with a single br_table instruction }
              asmlist.Concat(taicpu.op_const(a_local_get,machine_state));
              asmlist.Concat(taicpu.op_const(a_i32_const,block_nr));
              asmlist.Concat(taicpu.op_none(a_i32_eq));
              asmlist.Concat(taicpu.op_const(a_br_if,block_nr));
            end;
          asmlist.Concat(taicpu.op_none(a_unreachable));
          tmplist:=TAsmList.Create;
          for block_nr:=0 to blocks.Count-1 do
            begin
              asmlist.Concat(taicpu.op_none(a_end_block));
              asmlist.Concat(tai_comment.Create(strpnew('block '+tostr(block_nr)+' for label '+blocks.NameOfIndex(block_nr))));
              curr_block:=TAsmList(blocks[block_nr]);
              hp:=tai(curr_block.First);
              while assigned(hp) do
                begin
                  hpnext:=tai(hp.next);
                  if (hp.typ=ait_instruction) and (taicpu(hp).opcode in [a_br,a_br_if]) and
                     (taicpu(hp).ops=1) and
                     (taicpu(hp).oper[0]^.typ=top_ref) and
                     assigned(taicpu(hp).oper[0]^.ref^.symbol) then
                    begin
                      target_block_index:=blocks.FindIndexOf(taicpu(hp).oper[0]^.ref^.symbol.Name);
                      curr_block.InsertBefore(tai_comment.Create(strpnew(
                        'branch '+gas_op2str[taicpu(hp).opcode]+
                        ' '+taicpu(hp).oper[0]^.ref^.symbol.Name+
                        ' target_block_index='+tostr(target_block_index))),hp);
                      if target_block_index<>-1 then
                        begin
                          tmplist.Clear;
                          if taicpu(hp).opcode=a_br_if then
                            tmplist.Concat(taicpu.op_none(a_if));
                          tmplist.Concat(taicpu.op_const(a_i32_const,target_block_index));
                          tmplist.Concat(taicpu.op_const(a_local_set,machine_state));
                          tmplist.Concat(taicpu.op_sym(a_br,state_machine_loop_start_label));
                          if taicpu(hp).opcode=a_br_if then
                            tmplist.Concat(taicpu.op_none(a_end_if));
                          curr_block.insertListAfter(hp,tmplist);
                          curr_block.Remove(hp);
                        end;
                    end;
                  hp:=hpnext;
                end;
              if block_nr<(blocks.Count-1) then
                begin
                  curr_block.Concat(taicpu.op_const(a_i32_const,block_nr+1));
                  curr_block.Concat(taicpu.op_const(a_local_set,machine_state));
                  curr_block.Concat(taicpu.op_sym(a_br,state_machine_loop_start_label));
                end
              else
                curr_block.Concat(taicpu.op_sym(a_br,state_machine_exit));
              asmlist.concatList(curr_block);
            end;
          tmplist.Free;
          asmlist.Concat(taicpu.op_none(a_end_loop));
          asmlist.Concat(taicpu.op_none(a_end_block));
          asmlist.concat(tai_label.create(state_machine_exit));
        end;

      procedure filter_start_exit_code(asmlist: TAsmList; out entry_code, proc_body, exit_code: TAsmList);
        var
          hp, hpnext, hpprev: tai;
        begin
          entry_code:=TAsmList.Create;
          proc_body:=TAsmList.Create;
          exit_code:=TAsmList.Create;
          repeat
            hp:=tai(asmlist.First);
            if assigned(hp) then
              begin
                hpnext:=tai(hp.next);
                if (hp.typ=ait_instruction) and (taicpu(hp).opcode=a_block) then
                  break;
                asmlist.Remove(hp);
                entry_code.Concat(hp);
                hp:=hpnext;
              end;
          until not assigned(hp);
          repeat
            hp:=tai(asmlist.Last);
            if assigned(hp) then
              begin
                hpprev:=tai(hp.Previous);
                if (hp.typ=ait_instruction) and (taicpu(hp).opcode=a_end_block) then
                  break;
                asmlist.Remove(hp);
                exit_code.Insert(hp);
                hp:=hpprev;
              end;
          until not assigned(hp);
          proc_body.insertList(asmlist);
        end;

      procedure resolve_labels_of_asmlist_with_try_blocks_recursive(asmlist: TAsmList);
        var
          hp: tai;
          i: Integer;
        begin
          if not assigned(asmlist) then
            exit;
          hp:=tai(asmlist.First);
          while assigned(hp) do
            begin
              if hp.typ=ait_wasm_structured_instruction then
                begin
                  if not (taicpu_wasm_structured_instruction(hp).wstyp in [aitws_try_catch,aitws_try_delegate]) then
                    internalerror(2023102201);
                  resolve_labels_of_asmlist_with_try_blocks_recursive(tai_wasmstruc_try(hp).try_asmlist);
                  if taicpu_wasm_structured_instruction(hp).wstyp=aitws_try_catch then
                    with tai_wasmstruc_try_catch(hp) do
                      begin
                        for i:=low(catch_list) to high(catch_list) do
                          resolve_labels_of_asmlist_with_try_blocks_recursive(catch_list[i].asmlist);
                        resolve_labels_of_asmlist_with_try_blocks_recursive(catch_all_asmlist);
                      end
                  else if taicpu_wasm_structured_instruction(hp).wstyp=aitws_try_delegate then
                    {nothing}
                  else
                    internalerror(2023102202);
                end;
              hp:=tai(hp.next);
            end;
          resolve_labels_via_state_machine(asmlist);
        end;

      procedure resolve_labels_complex(var asmlist: TAsmList);
        var
          entry_code, proc_body, exit_code: TAsmList;
        begin
          filter_start_exit_code(asmlist,entry_code,proc_body,exit_code);
          asmlist.Free;
          asmlist:=proc_body;
          proc_body:=nil;

          wasm_convert_to_structured_asmlist(asmlist);

          map_structured_asmlist(asmlist,@ConvertBranchTargetNumbersToLabels);
          map_structured_asmlist(asmlist,@ConvertIfToBrIf);
          map_structured_asmlist(asmlist,@ConvertLoopToBr);

          wasm_convert_to_flat_asmlist(asmlist);

          map_structured_asmlist(asmlist,@StripBlockInstructions);

          wasm_convert_to_structured_asmlist(asmlist);

          resolve_labels_of_asmlist_with_try_blocks_recursive(asmlist);

          wasm_convert_to_flat_asmlist(asmlist);

          asmlist.insertList(entry_code);
          entry_code.free;
          asmlist.concatList(exit_code);
          exit_code.free;

          if not resolve_labels_simple(asmlist) then
            internalerror(2023102101);
        end;

        function prepare_locals: TAsmList;
          var
            local: tai_local;
            first: Boolean;
            l : TWasmLocal;
          begin
            result:=TAsmList.create;
            local:=nil;
            first:=true;
            l:=ttgwasm(tg).localvars.first;
            FFirstFreeLocal:=Length(findfirst_tai_functype(aktproccode).functype.params);
            while Assigned(l) do
              begin
                local:=tai_local.create(l.typ);
                local.first:=first;
                first:=false;
                result.Concat(local);
                l:=l.nextseq;
                Inc(FFirstFreeLocal);
              end;
          end;

        procedure add_extra_allocated_locals(localslist: TAsmList);
          var
            t: TWasmBasicType;
          begin
            for t in FAllocatedLocals do
              localslist.Concat(tai_local.create(t));
          end;

        procedure insert_localslist(destlist,localslist: TAsmList);
          begin
            if assigned(localslist) then
              begin
                tai_local(localslist.Last).last:=true;
                destlist.insertListAfter(findfirst_tai_functype(destlist),localslist);
              end;
          end;

        procedure check_goto_br_instructions(list: TAsmList);
          var
            hp: tai;
          begin
            hp:=tai(list.first);
            while assigned(hp) do
              begin
                if (hp.typ=ait_instruction) and (taicpu(hp).is_br_generated_by_goto) then
                  begin
                    if (taicpu(hp).opcode<>a_br) or
                       (taicpu(hp).ops<>1) or
                       (taicpu(hp).oper[0]^.typ<>top_ref) or
                       (taicpu(hp).oper[0]^.ref^.offset<>0) or
                       (taicpu(hp).oper[0]^.ref^.base<>NR_NO) or
                       (taicpu(hp).oper[0]^.ref^.index<>NR_NO) or
                       (taicpu(hp).oper[0]^.ref^.symbol=nil) then
                      internalerror(2023102203);
                    if not is_goto_target(taicpu(hp).oper[0]^.ref^.symbol) then
                      internalerror(2023102204);
                  end;
                hp:=tai(hp.next);
              end;
          end;

      var
        localslist: TAsmList;
        labels_resolved: Boolean;
      begin
        check_goto_br_instructions(aktproccode);

        localslist:=prepare_locals;

        replace_local_frame_pointer(aktproccode);

        labels_resolved:=resolve_labels_simple(aktproccode);
{$ifndef DEBUG_WASM_GOTO}
        if not labels_resolved then
{$endif DEBUG_WASM_GOTO}
          resolve_labels_complex(aktproccode);

        add_extra_allocated_locals(localslist);
        insert_localslist(aktproccode,localslist);
        localslist.Free;

        inherited postprocess_code;
      end;

    procedure tcpuprocinfo.set_first_temp_offset;
      var
        sz : integer;
        i  : integer;
        sym: tsym;
      begin
        {
          Stackframe layout:
          sp:
            <incoming parameters>
            sp+first_temp_offset:
            <locals>
            <temp>
        }
        procdef.init_paraloc_info(calleeside);
        sz := procdef.calleeargareasize;
        tg.setfirsttemp(sz);
      end;

    procedure tcpuprocinfo.add_goto_target(l: tasmlabel);
      begin
        FGotoTargets.Add(l.Name,l);
      end;

    function tcpuprocinfo.is_goto_target(l: tasmsymbol): Boolean;
      begin
        result:=FGotoTargets.FindIndexOf(l.Name)<>-1;
      end;


initialization
  cprocinfo:=tcpuprocinfo;
end.

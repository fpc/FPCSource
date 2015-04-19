{
    Copyright (c) 2008,2015 by Peter Vreman, Florian Klaempfl and Jonas Maebe

    This units contains support for generating LLVM type info

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
{
  This units contains support for LLVM type info generation.

  It's based on the debug info system, since it's quite similar
}
unit llvmtype;

{$i fpcdefs.inc}
{$h+}

interface

    uses
      cclasses,globtype,
      aasmbase,aasmtai,aasmdata,
      symbase,symtype,symdef,symsym,
      finput,
      dbgbase;


    { TLLVMTypeInfo }
    type
      TLLVMTypeInfo = class(TDebugInfo)
      protected
        function record_def(def:tdef): tdef;

        procedure appenddef_array(list:TAsmList;def:tarraydef);override;
        procedure appenddef_abstractrecord(list:TAsmList;def:tabstractrecorddef);
        procedure appenddef_record(list:TAsmList;def:trecorddef);override;
        procedure appenddef_pointer(list:TAsmList;def:tpointerdef);override;
        procedure appenddef_procvar(list:TAsmList;def:tprocvardef);override;
        procedure appendprocdef(list:TAsmList;def:tprocdef);override;
        procedure appenddef_object(list:TAsmList;def: tobjectdef);override;
        procedure appenddef_variant(list:TAsmList;def: tvariantdef);override;

        procedure appendsym_var(list:TAsmList;sym:tabstractnormalvarsym);
        procedure appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);override;
        procedure appendsym_paravar(list:TAsmList;sym:tparavarsym);override;
        procedure appendsym_localvar(list:TAsmList;sym:tlocalvarsym);override;
        procedure appendsym_fieldvar(list:TAsmList;sym:tfieldvarsym);override;
        procedure appendsym_const(list:TAsmList;sym:tconstsym);override;
        procedure appendsym_absolute(list:TAsmList;sym:tabsolutevarsym);override;

        procedure enum_membersyms_callback(p:TObject;arg:pointer);

        procedure process_llvmins(deftypelist: tasmlist; p: tai);
        procedure process_tai(deftypelist: tasmlist; p: tai);
        procedure process_asmlist(deftypelist, asmlist: tasmlist);

      public
        constructor Create;override;
        destructor Destroy;override;
        procedure inserttypeinfo;override;
      end;

implementation

    uses
      sysutils,cutils,cfileutl,constexp,
      version,globals,verbose,systems,
      cpubase,cgbase,paramgr,
      fmodule,nobj,
      defutil,symconst,symtable,
      llvmbase, aasmllvm, aasmcnst;

{****************************************************************************
                              TDebugInfoDwarf
****************************************************************************}


    function TLLVMTypeInfo.record_def(def:tdef): tdef;
      begin
        result:=def;
        if def.dbg_state<>dbg_state_unused then
          exit;
        def.dbg_state:=dbg_state_used;
        deftowritelist.Add(def);
        defnumberlist.Add(def);
      end;


    constructor TLLVMTypeInfo.Create;
      begin
        inherited Create;
      end;


    destructor TLLVMTypeInfo.Destroy;
      begin
        inherited destroy;
      end;


    procedure TLLVMTypeInfo.enum_membersyms_callback(p:TObject; arg: pointer);
      begin
        case tsym(p).typ of
          fieldvarsym:
            appendsym_fieldvar(TAsmList(arg),tfieldvarsym(p));
        end;
      end;


    procedure TLLVMTypeInfo.process_llvmins(deftypelist: tasmlist; p: tai);
      var
        opidx, paraidx: longint;
        callpara: pllvmcallpara;
      begin
        for opidx:=0 to taillvm(p).ops-1 do
          case taillvm(p).oper[opidx]^.typ of
            top_def:
              appenddef(deftypelist,taillvm(p).oper[opidx]^.def);
            top_tai:
              process_tai(deftypelist,taillvm(p).oper[opidx]^.ai);
            top_para:
              for paraidx:=0 to taillvm(p).oper[opidx]^.paras.count-1 do
                begin
                  callpara:=pllvmcallpara(taillvm(p).oper[opidx]^.paras[paraidx]);
                  appenddef(deftypelist,callpara^.def);
                end;
          end;
      end;


    procedure TLLVMTypeInfo.process_tai(deftypelist: tasmlist; p: tai);
      begin
        case p.typ of
          ait_llvmalias:
            appenddef(deftypelist,taillvmalias(p).def);
          ait_llvmdecl:
            appenddef(deftypelist,taillvmdecl(p).def);
          ait_llvmins:
            process_llvmins(deftypelist,p);
          ait_typedconst:
            appenddef(deftypelist,tai_abstracttypedconst(p).def);
        end;
      end;


    procedure TLLVMTypeInfo.process_asmlist(deftypelist, asmlist: tasmlist);
      var
        hp: tai;
      begin
        if not assigned(asmlist) then
          exit;
        hp:=tai(asmlist.first);
        while assigned(hp) do
          begin
            process_tai(deftypelist,hp);
            hp:=tai(hp.next);
          end;
      end;


    procedure TLLVMTypeInfo.appenddef_array(list:TAsmList;def:tarraydef);
      begin
        appenddef(list,def.elementdef);
      end;


    procedure TLLVMTypeInfo.appenddef_abstractrecord(list:TAsmList;def:tabstractrecorddef);
      var
        symdeflist: tfpobjectlist;
        i: longint;
      begin
        symdeflist:=tabstractrecordsymtable(def.symtable).llvmst.symdeflist;
        for i:=0 to symdeflist.Count-1 do
          appenddef(list,tllvmshadowsymtableentry(symdeflist[i]).def);
        if assigned(def.typesym) then
          list.concat(taillvm.op_size(LA_TYPE,record_def(def)));
      end;


    procedure TLLVMTypeInfo.appenddef_record(list:TAsmList;def:trecorddef);
      begin
        appenddef_abstractrecord(list,def);
      end;


    procedure TLLVMTypeInfo.appenddef_pointer(list:TAsmList;def:tpointerdef);
      begin
        appenddef(list,def.pointeddef);
      end;


    procedure TLLVMTypeInfo.appenddef_procvar(list:TAsmList;def:tprocvardef);
      var
        i: longint;
      begin
        { todo: handle mantis #25551; there is no way to create a symbolic
          la_type for a procvardef (unless it's a procedure of object/record),
          which means that recursive references should become plain "procedure"
          types that are then casted to the real type when they are used }
        for i:=0 to def.paras.count-1 do
          appenddef(list,tparavarsym(def.paras[i]).vardef);
        appenddef(list,def.returndef);
        if assigned(def.typesym) and
           not def.is_addressonly then
          list.concat(taillvm.op_size(LA_TYPE,record_def(def)));
      end;


    procedure TLLVMTypeInfo.appendprocdef(list:TAsmList;def:tprocdef);
      begin
        { the procdef itself is already written by appendprocdef_implicit }
      
        { last write the types from this procdef }
        if assigned(def.parast) then
          write_symtable_defs(current_asmdata.asmlists[al_start],def.parast);
        if assigned(def.localst) and
           (def.localst.symtabletype=localsymtable) then
          write_symtable_defs(current_asmdata.asmlists[al_start],def.localst);
      end;


    procedure TLLVMTypeInfo.appendsym_var(list:TAsmList;sym:tabstractnormalvarsym);
      begin
        appenddef(list,sym.vardef);
      end;


    procedure TLLVMTypeInfo.appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);
      begin
        appendsym_var(list,sym);
      end;


    procedure TLLVMTypeInfo.appendsym_localvar(list:TAsmList;sym:tlocalvarsym);
      begin
        appendsym_var(list,sym);
      end;


    procedure TLLVMTypeInfo.appendsym_paravar(list:TAsmList;sym:tparavarsym);
      begin
        appendsym_var(list,sym);
      end;


    procedure TLLVMTypeInfo.appendsym_fieldvar(list:TAsmList;sym: tfieldvarsym);
      begin
        appenddef(list,sym.vardef);
      end;


    procedure TLLVMTypeInfo.appendsym_const(list:TAsmList;sym:tconstsym);
      begin
        appenddef(list,sym.constdef);
      end;


    procedure TLLVMTypeInfo.appendsym_absolute(list:TAsmList;sym:tabsolutevarsym);
      begin
        appenddef(list,sym.vardef);
      end;


    procedure TLLVMTypeInfo.inserttypeinfo;

      procedure write_defs_to_write;
        var
          n       : integer;
          looplist,
          templist: TFPObjectList;
          def     : tdef;
        begin
          templist := TFPObjectList.Create(False);
          looplist := deftowritelist;
          while looplist.count > 0 do
            begin
              deftowritelist := templist;
              for n := 0 to looplist.count - 1 do
                begin
                  def := tdef(looplist[n]);
                  case def.dbg_state of
                    dbg_state_written:
                      continue;
                    dbg_state_writing:
                      internalerror(200610052);
                    dbg_state_unused:
                      internalerror(200610053);
                    dbg_state_used:
                      appenddef(current_asmdata.asmlists[al_start],def)
                  else
                    internalerror(200610054);
                  end;
                end;
              looplist.clear;
              templist := looplist;
              looplist := deftowritelist;
            end;
          templist.free;
        end;


      var
        storefilepos: tfileposinfo;
        def: tdef;
        i: longint;
        hal: tasmlisttype;
      begin
        storefilepos:=current_filepos;
        current_filepos:=current_module.mainfilepos;

        defnumberlist:=TFPObjectList.create(false);
        deftowritelist:=TFPObjectList.create(false);

        { write all global/static variables, part of flaggin all required tdefs  }
        if assigned(current_module.globalsymtable) then
          write_symtable_syms(current_asmdata.asmlists[al_start],current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_syms(current_asmdata.asmlists[al_start],current_module.localsymtable);

        { write all procedures and methods, part of flagging all required tdefs }
        if assigned(current_module.globalsymtable) then
          write_symtable_procdefs(current_asmdata.asmlists[al_start],current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_procdefs(current_asmdata.asmlists[al_start],current_module.localsymtable);

        { process all llvm instructions, part of flagging all required tdefs }
        for hal:=low(TasmlistType) to high(TasmlistType) do
          if hal<>al_start then
            process_asmlist(current_asmdata.asmlists[al_start],current_asmdata.asmlists[hal]);

        { write all used defs }
        write_defs_to_write;

        { reset all def labels }
        for i:=0 to defnumberlist.count-1 do
          begin
            def := tdef(defnumberlist[i]);
            if assigned(def) then
              begin
                def.dbg_state:=dbg_state_unused;
              end;
          end;

        defnumberlist.free;
        defnumberlist:=nil;
        deftowritelist.free;
        deftowritelist:=nil;

        current_filepos:=storefilepos;
      end;


    procedure TLLVMTypeInfo.appenddef_object(list:TAsmList;def: tobjectdef);
      begin
        appenddef_abstractrecord(list,def);
      end;


    procedure TLLVMTypeInfo.appenddef_variant(list:TAsmList;def: tvariantdef);
      begin
        appenddef(list,tabstractrecorddef(search_system_type('TVARDATA').typedef));
      end;

end.

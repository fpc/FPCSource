{
    Copyright (c) 2008 by Peter Vreman, Florian Klaempfl and Jonas Maebe

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
unit llvmdef;

{$i fpcdefs.inc}
{$h+}

interface

    uses
      cclasses,globtype,
      aasmbase,aasmtai,aasmdata,
      symbase,symtype,symdef,symsym,
      finput,
      dbgbase;


      { TLLVMDefInfo }
    type
      TLLVMDefInfo = class(TDebugInfo)
        { collect all defs in one list so we can reset them easily }
        defnumberlist,
        deftowritelist   : TFPObjectList;

        function def_llvm_name(def:tdef) : tasmsymbol;
        function def_llvm_class_struct_name(def:tobjectdef) : tasmsymbol;
//        function def_llvm_class_meta_name(def:tobjectdef) : tasmsymbol;
      protected
        vardatadef: trecorddef;

        procedure record_def(def:tdef);

        procedure beforeappenddef(list:TAsmList;def:tdef);override;
        procedure afterappenddef(list:TAsmList;def:tdef);override;
        procedure appenddef_ord(list:TAsmList;def:torddef);override;
        procedure appenddef_float(list:TAsmList;def:tfloatdef);override;
        procedure appenddef_enum(list:TAsmList;def:tenumdef);override;
        procedure appenddef_array(list:TAsmList;def:tarraydef);override;
        procedure appenddef_abstractrecord(list:TAsmList;def:tabstractrecorddef);
        procedure appenddef_record(list:TAsmList;def:trecorddef);override;
        procedure appenddef_pointer(list:TAsmList;def:tpointerdef);override;
        procedure appenddef_classref(list:TAsmList;def: tclassrefdef);override;
        procedure appenddef_string(list:TAsmList;def:tstringdef);override;
        procedure appenddef_procvar(list:TAsmList;def:tprocvardef);override;
        procedure appendprocdef(list:TAsmList;def:tprocdef);override;
        procedure appenddef_formal(list:TAsmList;def: tformaldef);override;
        procedure appenddef_object(list:TAsmList;def: tobjectdef);override;
        procedure appenddef_set(list:TAsmList;def: tsetdef);override;
        procedure appenddef_undefined(list:TAsmList;def: tundefineddef);override;
        procedure appenddef_variant(list:TAsmList;def: tvariantdef);override;

        procedure appendsym_var(list:TAsmList;sym:tabstractnormalvarsym);
        procedure beforeappendsym(list:TAsmList;sym:tsym);override;
        procedure appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);override;
        procedure appendsym_paravar(list:TAsmList;sym:tparavarsym);override;
        procedure appendsym_localvar(list:TAsmList;sym:tlocalvarsym);override;
        procedure appendsym_fieldvar(list:TAsmList;sym:tfieldvarsym);override;
        procedure appendsym_const(list:TAsmList;sym:tconstsym);override;
        procedure appendsym_type(list:TAsmList;sym:ttypesym);override;
        procedure appendsym_label(list:TAsmList;sym:tlabelsym);override;
        procedure appendsym_absolute(list:TAsmList;sym:tabsolutevarsym);override;
        procedure appendsym_property(list:TAsmList;sym:tpropertysym);override;

        function symname(sym:tsym): String;

        procedure enum_membersyms_callback(p:TObject;arg:pointer);

      public
        constructor Create;override;
        destructor Destroy;override;
        procedure insertmoduleinfo;override;
        procedure inserttypeinfo;override;
        procedure referencesections(list:TAsmList);override;
      end;

implementation

    uses
      sysutils,cutils,cfileutl,constexp,
      version,globals,verbose,systems,
      cpubase,cgbase,paramgr,
      fmodule,nobj,
      defutil,symconst,symtable,
      llvmbase, aasmllvm;

{****************************************************************************
                              TDebugInfoDwarf
****************************************************************************}


    procedure TLLVMDefInfo.record_def(def:tdef);
      begin
        if (def.dbg_state <> dbg_state_unused) then
          exit;
        { the name syms are set automatically when requested }
        def.dbg_state:=dbg_state_used;
        deftowritelist.Add(def);
        defnumberlist.Add(def);
      end;


    function TLLVMDefInfo.def_llvm_name(def: tdef): tasmsymbol;
      begin
        record_def(def);
        result:=def.llvm_name_sym;
      end;


    function TLLVMDefInfo.def_llvm_class_struct_name(def: tobjectdef): tasmsymbol;
      begin
        record_def(def);
        result:=def.llvm_class_struct_name_sym;
      end;


    constructor TLLVMDefInfo.Create;
      begin
        inherited Create;
      end;


    destructor TLLVMDefInfo.Destroy;
      begin
        inherited destroy;
      end;


    procedure TLLVMDefInfo.enum_membersyms_callback(p:TObject; arg: pointer);
      begin
        case tsym(p).typ of
          fieldvarsym:
            appendsym_fieldvar(TAsmList(arg),tfieldvarsym(p));
        end;
      end;



    procedure TLLVMDefInfo.appenddef_ord(list:TAsmList;def:torddef);
      var
        basedef      : tdef;
        fullbytesize : byte;
      begin
        case def.ordtype of
          s8bit,
          s16bit,
          s32bit,
          s64bit,
          u8bit,
          u16bit,
          u32bit,
          u64bit,
          uchar,
          uwidechar,
          pasbool,
          bool8bit,
          bool16bit,
          bool32bit,
          bool64bit,
          scurrency:
            begin
              list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'i'+tostr(def.size*8)));
            end;
          uvoid :
            begin
              list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'void'));
            end
          else
            internalerror(2008032901);
        end;
      end;


    procedure TLLVMDefInfo.appenddef_float(list:TAsmList;def:tfloatdef);
      begin
        case def.floattype of
          s32real:
            list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'float'));
          s64real:
            list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'double'));
          s80real:
            list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'x86_fp80'));
          s64currency,
          s64comp:
            list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'i64'));
          else
            internalerror(200601289);
        end;
      end;


    procedure TLLVMDefInfo.appenddef_enum(list:TAsmList;def:tenumdef);
      var
        hp : tenumsym;
      begin
        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'i'+tostr(def.size*8)));
      end;


    procedure TLLVMDefInfo.appenddef_array(list:TAsmList;def:tarraydef);
      var
        typename: ansistring;
        endstr: ansistring;
        indexrange: aint;
{$ifndef llvm_has_packed_arrays}
      begin
        if is_packed_array(def) then
          begin
            { have to use an array of byte of the appropriate size, }
            { since llvm doesn't support packed arrays yet natively }
            typename:=def_llvm_name(s8inttype).name;
            indexrange:=def.size;
          end
        else
          begin
            typename:=def_llvm_name(def.elementdef).name;
            if not is_open_array(def) and
               not is_dynamic_array(def) then
              indexrange:=def.highrange-def.lowrange+1
            else
              indexrange:=0;
          end;
        if not is_dynamic_array(def) then
          endstr:=']'
        else
          endstr:=']*';
        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'['+tostr(indexrange)+' x '+typename+endstr))
{$else not llvm_has_packed_arrays}
      var
        arrstart, arrend, typename: ansistring;
      begin
        typename:='';
        if not is_packed_array(def) then
          begin
            { regular array: '[' nritems 'x' type ']' }
            arrstart:='[';
            arrend:=']'
          end
        else
          begin
            { packed array: '<' nritems 'x' type '>' }
            arrstart:='< [';
            arrend:='] >';
            if is_ordinal(def.elementdef) then
              typename:='i'+tostr(def.elepackedbitsize);
          end;
        if (typename='') then
          typename:=def_llvm_name(def.elementdef).name

        if is_open_array(def) then
          list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),arrstart+'0 x '+typename+arrend))
        else if is_dynamic_array(def) then
          list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'[0 x '+typename+']'+'*'))
        else
          list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),arrstart+tostr(def.highrange-def.lowrange+1)+' x '+typename+arrend))
{$endif not llvm_has_packed_arrays}
      end;


    procedure TLLVMDefInfo.appenddef_abstractrecord(list:TAsmList;def:tabstractrecorddef);
      var
        defstr, endstr: ansistring;
        symdeflist: tfpobjectlist;
        i: longint;
      begin
        if (tabstractrecordsymtable(def.symtable).usefieldalignment<>C_alignment) then
          begin
            { we handle the alignment/padding ourselves }
            defstr:='< ';
            endstr:='>'
          end
        else
          begin
            { let llvm do everything }
            defstr:= '{ ';
            endstr:= '}'
          end;
        if not assigned(tabstractrecordsymtable(def.symtable).llvmst) then
          tabstractrecordsymtable(def.symtable).llvmst:=tllvmshadowsymtable.create(trecordsymtable(def.symtable));
        symdeflist:=tabstractrecordsymtable(def.symtable).llvmst.symdeflist;
        for i:=0 to pred(symdeflist.count) do
          defstr:=defstr+def_llvm_name(tllvmshadowsymtableentry(symdeflist[i]).def).name+', ';
        { remove last ', ' }
        setlength(defstr,length(defstr)-2);
        defstr:=defstr+' }';
        if (def.typ <> objectdef) or
           not(tobjectdef(def).objecttype in [odt_interfacecom,odt_interfacecorba,odt_dispinterface,odt_class]) then
          list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),defstr))
        else
          list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_class_struct_name(tobjectdef(def)),defstr))
      end;


    procedure TLLVMDefInfo.appenddef_record(list:TAsmList;def:trecorddef);
      begin
        appenddef_abstractrecord(list,def);
      end;


    procedure TLLVMDefInfo.appenddef_pointer(list:TAsmList;def:tpointerdef);
      begin
        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),def_llvm_name(def.pointeddef).name+'*'));
      end;


    procedure TLLVMDefInfo.appenddef_classref(list:TAsmList;def: tclassrefdef);
      var
        defstr: ansistring;
        vmtbuilder: tvmtbuilder;
        i: longint;
      begin
        { a pointer to the VMT. Structure of the VMT: }
        {   InstanceSize  : ptrint  }
        {   -InstanceSize : ptrint  }
        {   Parent        : ^parent }
        {   ClassName     : pointer }
        {   DynamicTable  : pointer }
        {   MethodTable   : pointer }
        {   FieldTable    : pointer }
        {   TypeInfo      : pointer }
        {   InitTable     : pointer }
        {   AutoTable     : pointer }
        {   IntfTable     : pointer }
        {   MsgStrTable   : pointer }
        {   Methods       : X times procvar }
        defstr:=def_llvm_name(ptrsinttype).name+',';
        defstr:='< '+defstr+defstr;
{ needs to be pointer to the parent class' vmt!
        if assigned(tobjectdef(def.pointeddef).childof) then
          defstr:=defstr+def_llvm_name(tobjectdef(def.pointeddef).childof).name+'*,'
        else
}
          defstr:=defstr+'void*,';
        { class name (length+string) }
        defstr:=defstr+'['+tostr(length(tobjectdef(def.pointeddef).objrealname^)+1)+' x i8]*,';
        { the other fields }
        for i:=1 to 8 do
          defstr:=defstr+'void*,';
        if not assigned(tobjectdef(def.pointeddef).VMTEntries) then
          with TVMTBuilder.Create(tobjectdef(def.pointeddef)) do
            begin
              generate_vmt;
              free;
            end;
        for i:= 0 to tobjectdef(def.pointeddef).VMTEntries.Count-1 do
          defstr:=defstr+def_llvm_name(tprocdef(tobjectdef(def.pointeddef).VMTEntries[i])).name+'*,';
        setlength(defstr,length(defstr)-1);
        defstr:=defstr+' >*';
        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),defstr));
      end;


    procedure TLLVMDefInfo.appenddef_string(list:TAsmList;def:tstringdef);

      procedure addnormalstringdef(lendef: tdef);
        var
          defstr: ansistring;
        begin
          { record with length and array [maxlen x i8 ] }
          { (also ok for openstrings, as [0 x i8] means }
          {  "array of unspecified size" in llvm)       }
          defstr:='< '+def_llvm_name(lendef).name+', ['+tostr(def.len)+' x i8] >';
          list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),defstr));
        end;

      begin
        case def.stringtype of
          st_shortstring:
            begin
              addnormalstringdef(u8inttype);
            end;
          st_longstring:
            begin
{$ifdef cpu64bitaddr}
              addnormalstringdef(u64inttype);
{$else cpu64bitaddr}
              addnormalstringdef(u32inttype);
{$endif cpu64bitaddr}
           end;
         st_ansistring:
           begin
             { looks like a pchar }
             list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'i8*'));
           end;
         st_unicodestring,
         st_widestring:
           begin
             { looks like a pwidechar }
             list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'i16*'));
           end;
        end;
      end;

    procedure TLLVMDefInfo.appenddef_procvar(list:TAsmList;def:tprocvardef);

      procedure doappend;
        var
          i : longint;
        begin
(*
          if assigned(def.typesym) then
            append_entry(DW_TAG_subroutine_type,true,[
              DW_AT_name,DW_FORM_string,symname(def.typesym)+#0,
              DW_AT_prototyped,DW_FORM_flag,true
            ])
          else
            append_entry(DW_TAG_subroutine_type,true,[
              DW_AT_prototyped,DW_FORM_flag,true
            ]);
          if not(is_void(tprocvardef(def).returndef)) then
            append_labelentry_ref(DW_AT_type,def_dwarf_lab(tprocvardef(def).returndef));
          finish_entry;

          { write parameters }
          for i:=0 to def.paras.count-1 do
            begin
              append_entry(DW_TAG_formal_parameter,false,[
                DW_AT_name,DW_FORM_string,symname(tsym(def.paras[i]))+#0
              ]);
              append_labelentry_ref(DW_AT_type,def_dwarf_lab(tparavarsym(def.paras[i]).vardef));
              finish_entry;
            end;

          finish_children;
*)
        end;

      var
        proc : tasmlabel;

      begin
        if def.is_methodpointer then
          begin
            list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'methodpointer*'));
          end
        else
          list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'procvar*'));
      end;


    procedure TLLVMDefInfo.beforeappenddef(list:TAsmList;def:tdef);
      var
        labsym : tasmsymbol;
      begin
        list.concat(tai_comment.Create(strpnew('LLVM definition '+def.typename)));
(*
        labsym:=def_dwarf_lab(def);
        if ds_dwarf_dbg_info_written in def.defstates then
          current_asmdata.asmlists[al_dwarf_info].concat(tai_symbol.create_global(labsym,0))
        else
          current_asmdata.asmlists[al_dwarf_info].concat(tai_symbol.create(labsym,0));
*)
      end;


    procedure TLLVMDefInfo.afterappenddef(list:TAsmList;def:tdef);
      begin
      end;


    procedure TLLVMDefInfo.appendprocdef(list:TAsmList;def:tprocdef);
      var
        procendlabel   : tasmlabel;
        funcrettype    : tasmsymbol;
        procentry      : string;
        dreg           : byte;
      begin
        if not assigned(def.procstarttai) then
          exit;

        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'procedure/function'));
(*
        current_asmdata.asmlists[al_dwarf_info].concat(tai_comment.Create(strpnew('Procdef '+def.fullprocname(true))));
        append_entry(DW_TAG_subprogram,true,
          [DW_AT_name,DW_FORM_string,symname(def.procsym)+#0,
           DW_AT_external,DW_FORM_flag,po_global in def.procoptions
          { data continues below }
          { problem: base reg isn't known here
            DW_AT_frame_base,DW_FORM_block1,1
          }
          ]);
        { append block data }
        { current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(dwarf_reg(def.))); }

        if not(is_void(tprocdef(def).returndef)) then
          append_labelentry_ref(DW_AT_type,def_dwarf_lab(tprocdef(def).returndef));

        { mark end of procedure }
        current_asmdata.getlabel(procendlabel,alt_dbgtype);
        current_asmdata.asmlists[al_procedures].insertbefore(tai_label.create(procendlabel),def.procendtai);

        if (target_info.system = system_powerpc64_linux) then
          procentry := '.' + def.mangledname
        else
          procentry := def.mangledname;

        append_labelentry(DW_AT_low_pc,current_asmdata.RefAsmSymbol(procentry));
        append_labelentry(DW_AT_high_pc,procendlabel);

        if assigned(def.funcretsym) and
           (tabstractnormalvarsym(def.funcretsym).refs>0) then
          begin
            if tabstractnormalvarsym(def.funcretsym).localloc.loc=LOC_REFERENCE then
              begin
                finish_entry;

                if paramanager.ret_in_param(def.returndef,def.proccalloption) then
                  funcrettype:=def_dwarf_ref_lab(def.returndef)
                else
                  funcrettype:=def_dwarf_lab(def.returndef);

                append_entry(DW_TAG_formal_parameter,false,[
                  DW_AT_name,DW_FORM_string,def.procsym.name+#0,
                  {
                  DW_AT_decl_file,DW_FORM_data1,0,
                  DW_AT_decl_line,DW_FORM_data1,
                  }
                  { data continues below }
                  DW_AT_location,DW_FORM_block1,1+Lengthsleb128(tabstractnormalvarsym(def.funcretsym).localloc.reference.offset)
                ]);

                { append block data }
                dreg:=dwarf_reg(tabstractnormalvarsym(def.funcretsym).localloc.reference.base);
                current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(ord(DW_OP_breg0)+dreg));
                current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_sleb128bit(tabstractnormalvarsym(def.funcretsym).localloc.reference.offset));
                append_labelentry_ref(DW_AT_type,funcrettype);
              end;
          end;
        finish_entry;

        if assigned(def.parast) then
          write_symtable_syms(current_asmdata.asmlists[al_dwarf_info],def.parast);
        { local type defs and vars should not be written
          inside the main proc }
        if assigned(def.localst) and
           (def.localst.symtabletype=localsymtable) then
          write_symtable_syms(current_asmdata.asmlists[al_dwarf_info],def.localst);

        { last write the types from this procdef }
        if assigned(def.parast) then
          write_symtable_defs(current_asmdata.asmlists[al_dwarf_info],def.parast);
        if assigned(def.localst) and
           (def.localst.symtabletype=localsymtable) then
          write_symtable_defs(current_asmdata.asmlists[al_dwarf_info],def.localst);

        finish_children;
*)
      end;


    procedure TLLVMDefInfo.appendsym_var(list:TAsmList;sym:tabstractnormalvarsym);
      var
        templist : TAsmList;
        blocksize : longint;
        dreg : byte;
      begin      
        { external symbols can't be resolved at link time, so we
          can't generate stabs for them

          not sure if this applies to dwarf as well (FK)
        }
        if vo_is_external in sym.varoptions then
          exit;

        def_llvm_name(sym.vardef);

        { There is no space allocated for not referenced locals }
        if (sym.owner.symtabletype=localsymtable) and (sym.refs=0) then
          exit;
(*
        templist:=TAsmList.create;

        case sym.localloc.loc of
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER :
            begin
              templist.concat(tai_const.create_8bit(ord(DW_OP_regx)));
              dreg:=dwarf_reg(sym.localloc.register);
              templist.concat(tai_const.create_uleb128bit(dreg));
              blocksize:=1+Lengthuleb128(dreg);
            end;
          else
            begin
              case sym.typ of
                staticvarsym:
                  begin
                    if (vo_is_thread_var in sym.varoptions) then
                      begin
{$warning !!! FIXME: dwarf for thread vars !!!
}
                        blocksize:=0;
                      end
                    else
                      begin
                        templist.concat(tai_const.create_8bit(3));
                        templist.concat(tai_const.createname(sym.mangledname,0));
                        blocksize:=1+sizeof(puint);
                      end;
                  end;
                paravarsym,
                localvarsym:
                  begin
                    dreg:=dwarf_reg(sym.localloc.reference.base);
                    templist.concat(tai_const.create_8bit(ord(DW_OP_breg0)+dreg));
                    templist.concat(tai_const.create_sleb128bit(sym.localloc.reference.offset));
                    blocksize:=1+Lengthsleb128(sym.localloc.reference.offset);
                  end
                else
                  internalerror(200601288);
              end;
            end;
        end;

        if sym.typ=paravarsym then
          tag:=DW_TAG_formal_parameter
        else
          tag:=DW_TAG_variable;

        if not(sym.localloc.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_MMREGISTER,
                                 LOC_CMMREGISTER,LOC_FPUREGISTER,LOC_CFPUREGISTER]) and
           ((sym.owner.symtabletype = globalsymtable) or
            (sp_static in sym.symoptions) or
            (vo_is_public in sym.varoptions)) then
          append_entry(tag,false,[
            DW_AT_name,DW_FORM_string,symname(sym)+#0,
            {
            DW_AT_decl_file,DW_FORM_data1,0,
            DW_AT_decl_line,DW_FORM_data1,
            }
            DW_AT_external,DW_FORM_flag,true,
            { data continues below }
            DW_AT_location,DW_FORM_block1,blocksize
            ])
{$ifdef gdb_supports_DW_AT_variable_parameter}
        else if (sym.typ=paravarsym) and
            paramanager.push_addr_param(sym.varspez,sym.vardef,tprocdef(sym.owner.defowner).proccalloption) and
            not(vo_has_local_copy in sym.varoptions) and
            not is_open_string(sym.vardef) then
          append_entry(tag,false,[
            DW_AT_name,DW_FORM_string,symname(sym)+#0,
            DW_AT_variable_parameter,DW_FORM_flag,true,
            {
            DW_AT_decl_file,DW_FORM_data1,0,
            DW_AT_decl_line,DW_FORM_data1,
            }
            { data continues below }
            DW_AT_location,DW_FORM_block1,blocksize
            ])
{$endif gdb_supports_DW_AT_variable_parameter}
        else
          append_entry(tag,false,[
            DW_AT_name,DW_FORM_string,symname(sym)+#0,
            {
            DW_AT_decl_file,DW_FORM_data1,0,
            DW_AT_decl_line,DW_FORM_data1,
            }
            { data continues below }
            DW_AT_location,DW_FORM_block1,blocksize
            ]);
        { append block data }
        current_asmdata.asmlists[al_dwarf_info].concatlist(templist);
{$ifndef gdb_supports_DW_AT_variable_parameter}
        if (sym.typ=paravarsym) and
            paramanager.push_addr_param(sym.varspez,sym.vardef,tprocdef(sym.owner.defowner).proccalloption) and
            not(vo_has_local_copy in sym.varoptions) and
            not is_open_string(sym.vardef) then
          append_labelentry_ref(DW_AT_type,def_dwarf_ref_lab(sym.vardef))
        else
{$endif not gdb_supports_DW_AT_variable_parameter}
          append_labelentry_ref(DW_AT_type,def_dwarf_lab(sym.vardef));

        templist.free;

        finish_entry;
*)
      end;


    procedure TLLVMDefInfo.appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);
      begin
        appendsym_var(list,sym);
      end;


    procedure TLLVMDefInfo.appendsym_localvar(list:TAsmList;sym:tlocalvarsym);
      begin
        appendsym_var(list,sym);
      end;


    procedure TLLVMDefInfo.appendsym_paravar(list:TAsmList;sym:tparavarsym);
      begin
        appendsym_var(list,sym);
      end;


    procedure TLLVMDefInfo.appendsym_fieldvar(list:TAsmList;sym: tfieldvarsym);
      var
        bitoffset,
        fieldoffset,
        fieldnatsize: aint;
      begin
//        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE),'fieldvasym');
(*
        if sp_static in sym.symoptions then
          exit;

        if (tabstractrecordsymtable(sym.owner).usefieldalignment<>bit_alignment) or
           { only ordinals are bitpacked }
           not is_ordinal(sym.vardef) then
          begin
            { other kinds of fields can however also appear in a bitpacked   }
            { record, and then their offset is also specified in bits rather }
            { than in bytes                                                  }
            if (tabstractrecordsymtable(sym.owner).usefieldalignment<>bit_alignment) then
              fieldoffset:=sym.fieldoffset
            else
              fieldoffset:=sym.fieldoffset div 8;
            append_entry(DW_TAG_member,false,[
              DW_AT_name,DW_FORM_string,symname(sym)+#0,
              DW_AT_data_member_location,DW_FORM_block1,1+lengthuleb128(fieldoffset)
              ]);
          end
        else
          begin
            if (sym.vardef.packedbitsize > 255) then
              internalerror(2007061201);

            { we don't bitpack according to the ABI, but as close as }
            { possible, i.e., equivalent to gcc's                    }
            { __attribute__((__packed__)), which is also what gpc    }
            { does.                                                  }
            fieldnatsize:=max(sizeof(pint),sym.vardef.size);
            fieldoffset:=(sym.fieldoffset div (fieldnatsize*8)) * fieldnatsize;
            bitoffset:=sym.fieldoffset mod (fieldnatsize*8);
            if (target_info.endian=endian_little) then
              bitoffset:=(fieldnatsize*8)-bitoffset-sym.vardef.packedbitsize;
            append_entry(DW_TAG_member,false,[
              DW_AT_name,DW_FORM_string,symname(sym)+#0,
              { gcc also generates both a bit and byte size attribute }
              { we don't support ordinals >= 256 bits }
              DW_AT_byte_size,DW_FORM_data1,fieldnatsize,
              { nor >= 256 bits (not yet, anyway, see IE above) }
              DW_AT_bit_size,DW_FORM_data1,sym.vardef.packedbitsize,
              { data1 and data2 are unsigned, bitoffset can also be negative }
              DW_AT_bit_offset,DW_FORM_data4,bitoffset,
              DW_AT_data_member_location,DW_FORM_block1,1+lengthuleb128(fieldoffset)
              ]);
          end;
        current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(ord(DW_OP_plus_uconst)));
        current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_uleb128bit(fieldoffset));

        append_labelentry_ref(DW_AT_type,def_dwarf_lab(sym.vardef));
        finish_entry;
*)
      end;


    procedure TLLVMDefInfo.appendsym_const(list:TAsmList;sym:tconstsym);
      begin
//        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE),'constsym');
(*
        append_entry(DW_TAG_constant,false,[
          DW_AT_name,DW_FORM_string,symname(sym)+#0
          ]);
        { for string constants, constdef isn't set because they have no real type }
        if not(sym.consttyp in [conststring,constresourcestring,constwstring]) then
          append_labelentry_ref(DW_AT_type,def_dwarf_lab(sym.constdef));
        current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_AT_const_value)));
        case sym.consttyp of
          conststring:
            begin
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_string)));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_string.create(strpas(pchar(sym.value.valueptr))));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(0));
            end;
          constset,
          constwstring,
          constguid,
          constresourcestring:
            begin
              { write dummy for now }
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_string)));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_string.create(''));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(0));
            end;
          constord:
            begin
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_sdata)));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_sleb128bit(sym.value.valueord.svalue));
            end;
          constnil:
            begin
{$ifdef cpu64bitaddr}
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_data8)));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_64bit(0));
{$else cpu64bitaddr}
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_data4)));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_32bit(0));
{$endif cpu64bitaddr}
            end;
          constpointer:
            begin
{$ifdef cpu64bitaddr}
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_data8)));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_64bit(int64(sym.value.valueordptr)));
{$else cpu64bitaddr}
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_data4)));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_32bit(sym.value.valueordptr));
{$endif cpu64bitaddr}
            end;
          constreal:
            begin
              current_asmdata.asmlists[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_block1)));
              case tfloatdef(sym.constdef).floattype of
                s32real:
                  begin
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(4));
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_real_32bit.create(psingle(sym.value.valueptr)^));
                  end;
                s64comp,
                s64currency,
                s64real:
                  begin
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(8));
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_real_64bit.create(pdouble(sym.value.valueptr)^));
                  end;
                s80real:
                  begin
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(10));
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_real_80bit.create(pextended(sym.value.valueptr)^));
                  end;
                else
                  internalerror(200601291);
              end;
            end;
          else
            internalerror(200601292);
        end;
        finish_entry;
*)
      end;


    procedure TLLVMDefInfo.appendsym_label(list:TAsmList;sym: tlabelsym);
      begin
        { ignore label syms for now, the problem is that a label sym
          can have more than one label associated e.g. in case of
          an inline procedure expansion }
      end;


    procedure TLLVMDefInfo.appendsym_property(list:TAsmList;sym: tpropertysym);
      begin
        { ignored for now }
      end;


    procedure TLLVMDefInfo.appendsym_type(list:TAsmList;sym: ttypesym);
      begin
//        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,'typesym');
        record_def(sym.typedef);
      end;


    procedure TLLVMDefInfo.appendsym_absolute(list:TAsmList;sym:tabsolutevarsym);
      var
        templist : TAsmList;
        blocksize : longint;
        symlist : ppropaccesslistitem;
      begin
//        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE),'absolutesym'));
      end;


    procedure TLLVMDefInfo.beforeappendsym(list:TAsmList;sym:tsym);
      begin
      end;


    procedure TLLVMDefInfo.insertmoduleinfo;
      begin
      end;


    procedure TLLVMDefInfo.inserttypeinfo;

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
                      appenddef(current_asmdata.asmlists[al_dwarf_info],def)
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
        storefilepos  : tfileposinfo;
        lenstartlabel : tasmlabel;
        i : longint;
        def: tdef;
      begin
        storefilepos:=current_filepos;
        current_filepos:=current_module.mainfilepos;

        defnumberlist:=TFPObjectList.create(false);
        deftowritelist:=TFPObjectList.create(false);

        { not exported (FK)
            FILEREC
            TEXTREC
        }
        vardatadef:=trecorddef(search_system_type('TVARDATA').typedef);

        { write all global/local variables. This will flag all required tdefs  }
        if assigned(current_module.globalsymtable) then
          write_symtable_syms(current_asmdata.asmlists[al_dwarf_info],current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_syms(current_asmdata.asmlists[al_dwarf_info],current_module.localsymtable);

        { write all procedures and methods. This will flag all required tdefs }
        if assigned(current_module.globalsymtable) then
          write_symtable_procdefs(current_asmdata.asmlists[al_dwarf_info],current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_procdefs(current_asmdata.asmlists[al_dwarf_info],current_module.localsymtable);

        { reset unit type info flag }
        reset_unit_type_info;

        { write used types from the used units }
        write_used_unit_type_info(current_asmdata.asmlists[al_dwarf_info],current_module);

        { last write the types from this unit }
        if assigned(current_module.globalsymtable) then
          write_symtable_defs(current_asmdata.asmlists[al_dwarf_info],current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_defs(current_asmdata.asmlists[al_dwarf_info],current_module.localsymtable);

        { write defs not written yet }
        write_defs_to_write;

        { reset all def labels }
        for i:=0 to defnumberlist.count-1 do
          begin
            def := tdef(defnumberlist[i]);
            if assigned(def) then
              begin
                def.dwarf_lab:=nil;
                def.dbg_state:=dbg_state_unused;
              end;
          end;

        defnumberlist.free;
        defnumberlist:=nil;
        deftowritelist.free;
        deftowritelist:=nil;

        current_filepos:=storefilepos;
      end;


    procedure TLLVMDefInfo.referencesections(list:TAsmList);
      begin
      end;


    function TLLVMDefInfo.symname(sym: tsym): String;
      begin
        if (sym.typ=paravarsym) and
           (vo_is_self in tparavarsym(sym).varoptions) then
          result:='this'
        else
          result := sym.Name;
      end;



    procedure TLLVMDefInfo.appenddef_formal(list:TAsmList;def: tformaldef);
      begin
        { gdb 6.4 doesn't support DW_TAG_unspecified_type so we
          replace it with a unsigned type with size 0 (FK)
        }
        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'undef*'));
      end;


    procedure TLLVMDefInfo.appenddef_object(list:TAsmList;def: tobjectdef);
      procedure doappend;
        begin
          appenddef_abstractrecord(list,def);
        end;


      begin
        case def.objecttype of
          odt_cppclass,
          odt_object:
            doappend;
          odt_interfacecom,
          odt_interfacecorba,
          odt_dispinterface,
          odt_class:
            begin
              { implicit pointer }
              list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),def_llvm_class_struct_name(def).name+'*'));
              doappend;
            end;
          else
            internalerror(200602041);
        end;
      end;

    procedure TLLVMDefInfo.appenddef_set(list:TAsmList;def: tsetdef);
      begin
        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'[ '+tostr(def.size)+ 'x i8 ]'));
      end;

    procedure TLLVMDefInfo.appenddef_undefined(list:TAsmList;def: tundefineddef);
      begin
        { gdb 6.4 doesn't support DW_TAG_unspecified_type so we
          replace it with a unsigned type with size 0 (FK)
        }
        list.concat(tai_llvmcpu.op_ressym_string(LA_TYPE,def_llvm_name(def),'undef'));
      end;

    procedure TLLVMDefInfo.appenddef_variant(list:TAsmList;def: tvariantdef);
      begin
        { variants aren't known to dwarf2 but writting tvardata should be enough }
        appenddef_record(list,trecorddef(vardatadef));
      end;

end.

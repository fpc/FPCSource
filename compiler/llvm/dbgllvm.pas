{
    Copyright (c) 2021-2022 by Jonas Maebe,
    member of the Free Pascal Compiler development team

    This units contains support for LLVM debug info generation

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
  This units contains support for LLVM debug info generation.

  LLVM debug information is stored as metadata in the LLVM bitcode, and is
  loosely based on DWARF (it also reuses some DWARF constants)
}
unit dbgllvm;

{$i fpcdefs.inc}

interface

    uses
      cclasses,globtype,
      cgbase,
      aasmbase,aasmtai,aasmdata,aasmcnst,aasmllvm,aasmllvmmetadata,
      symbase,symconst,symtype,symdef,symsym,
      finput,
      DbgBase, dbgdwarfconst;

    type
      TLLVMMetaDefHashSetItem = record
        { HashSetItem.Data: LLVM metadata which other types reference when
          referring to this type (usually a typedef) }
        HashSetItem: THashSetItem;
        { in case of a class, the field layout (since a class itself is just a
          pointer }
        struct_metadef: tai_llvmspecialisedmetadatanode;
        { the metadata actually containing the type definition (usually
          referenced by HashSetItem.Data), filled in by appenddef_* }
        implmetadef: tai_llvmspecialisedmetadatanode;
      end;
      PLLVMMetaDefHashSetItem = ^TLLVMMetaDefHashSetItem;

      TLLVMMetaDefHashSet = class(THashSet)
        class function SizeOfItem: Integer; override;
      end;

      TDebugInfoLLVM = class(TDebugInfo)
       strict private
       var
        { lookup table for def -> LLVMMeta info }
        fdefmeta: TLLVMMetaDefHashSet;
        { lookup table for file -> LLVMMeta info (DIFile) }
        ffilemeta: THashSet;
        { lookup table for line,column,scope -> LLVMMeta info (DILocation) }
        flocationmeta: THashSet;
        { lookup table for scope,file -> LLVMMeta info (DILexicalBlockFile, for include files) }
        flexicalblockfilemeta: THashSet;
        { lookup table for tstaticvarsym -> taillvmdecl }
        fstaticvarsymdecl: THashSet;
        { lookup table for local/paravarsym -> metadata }
        flocalvarsymmeta: THashSet;

        fcunode: tai_llvmspecialisedmetadatanode;
        fenums: tai_llvmunnamedmetadatanode;
        fretainedtypes: tai_llvmunnamedmetadatanode;
        fglobals: tai_llvmunnamedmetadatanode;
        { reusable empty expression node }
        femptyexpression,
        { reusable deref node }
        fderefexpression  : tai_llvmspecialisedmetadatanode;

        fllvm_dbg_declare_pd: tprocdef;
        fllvm_dbg_addr_pd: tprocdef;

        function absolute_llvm_path(const s:tcmdstr):tcmdstr;
      protected
        vardatadef: trecorddef;

        procedure try_add_file_metaref(dinode: tai_llvmspecialisedmetadatanode; const fileinfo: tfileposinfo; includescope: boolean);
        function add_line_metanode(const fileinfo: tfileposinfo): tai_llvmspecialisedmetadatanode;

        function def_meta_impl(def: tdef) : tai_llvmspecialisedmetadatanode;
        function def_set_meta_impl(def: tdef; meta_kind: tspecialisedmetadatanodekind): tai_llvmspecialisedmetadatanode;
        function def_meta_class_struct(def: tobjectdef) : tai_llvmspecialisedmetadatanode;
        function def_meta_node(def: tdef): tai_llvmspecialisedmetadatanode;
        function def_meta_ref(def: tdef): tai_simpletypedconst;
        function file_getmetanode(moduleindex: tfileposmoduleindex; fileindex: tfileposfileindex): tai_llvmspecialisedmetadatanode;
        function filepos_getmetanode(const filepos: tfileposinfo; const functionfileinfo: tfileposinfo; const functionscope: tai_llvmspecialisedmetadatanode; nolineinfo: boolean): tai_llvmspecialisedmetadatanode;
        function get_def_metatai(def:tdef): PLLVMMetaDefHashSetItem;

        procedure staticvarsym_set_decl(sym: tsym; decl: taillvmdecl);
        function staticvarsym_get_decl(sym: tsym): taillvmdecl;

        function localvarsym_get_meta(sym: tsym; out is_new: boolean): tai_llvmspecialisedmetadatanode;

        procedure appenddef_array_internal(list: TAsmList; fordef: tdef; eledef: tdef; lowrange, highrange: asizeint);
        function getabstractprocdeftypes(list: TAsmList; def:tabstractprocdef): tai_llvmbasemetadatanode;

        procedure afterappenddef(list: TAsmList; def: tdef); override;
        procedure appenddef_ord(list:TAsmList;def:torddef);override;
        procedure appenddef_float(list:TAsmList;def:tfloatdef);override;
        procedure appenddef_enum(list:TAsmList;def:tenumdef);override;
        procedure appenddef_array(list:TAsmList;def:tarraydef);override;
        procedure appenddef_record_named(list: TAsmList; fordef: tdef; def: trecorddef; const name: TSymStr);
        procedure appenddef_struct_named(list: TAsmList; def: tabstractrecorddef; structdi: tai_llvmspecialisedmetadatanode; initialfieldlist: tai_llvmunnamedmetadatanode; const name: TSymStr);
        procedure appenddef_struct_fields(list: TAsmlist; def: tabstractrecorddef; defdinode: tai_llvmspecialisedmetadatanode; initialfieldlist: tai_llvmunnamedmetadatanode; cappedsize: asizeuint);
        procedure appenddef_record(list:TAsmList;def:trecorddef);override;
        procedure appenddef_pointer(list:TAsmList;def:tpointerdef);override;
        procedure appenddef_formal(list:TAsmList;def:tformaldef); override;
        procedure appenddef_string(list:TAsmList;def:tstringdef);override;
        procedure appenddef_procvar(list:TAsmList;def:tprocvardef);override;
        procedure appenddef_file(list:TAsmList;def:tfiledef); override;
        procedure appenddef_object(list:TAsmList;def:tobjectdef); override;
        procedure appenddef_set(list:TAsmList;def:tsetdef); override;
        procedure appenddef_undefined(list:TAsmList;def:tundefineddef); override;
        procedure appenddef_classref(list: TAsmList; def: tclassrefdef); override;
        procedure appenddef_variant(list:TAsmList;def:tvariantdef); override;

        procedure appendprocdef(list:TAsmList;def:tprocdef);override;

        procedure adddefinitionlocal(dinode: tai_llvmspecialisedmetadatanode; definition, local, usedispflags: boolean; out dispFlags: tsymstr);

        function  get_symlist_sym_offset(symlist: ppropaccesslistitem; out sym: tabstractvarsym; out offset: pint): boolean;
        procedure appendsym_var(list:TAsmList;sym:tabstractnormalvarsym);
        procedure appendsym_var_with_name_type_offset(list:TAsmList; sym:tabstractnormalvarsym; const name: TSymStr; def: tdef; offset: pint(*; const flags: tdwarfvarsymflags*));
        { used for fields and properties mapped to fields }
        procedure appendsym_fieldvar_with_name_offset(list:TAsmList;sym: tfieldvarsym;const name: string; def: tdef; offset: pint);
        procedure appendsym_const_member(list:TAsmList;sym:tconstsym;ismember:boolean);

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

        function symdebugname(sym:tsym): TSymStr;
        function symname(sym: tsym; manglename: boolean): TSymStr; virtual;
        function visibilitydiflag(vis: tvisibility): TSymStr;

        procedure ensuremetainit;
        procedure resetfornewmodule;

        procedure collectglobalsyms;
        procedure updatelocalvardbginfo(hp: taillvm; pd: tprocdef; functionscope: tai_llvmspecialisedmetadatanode);
      public
        constructor Create;override;
        destructor Destroy;override;
        procedure insertmoduleinfo;override;
        procedure inserttypeinfo;override;
        procedure insertlineinfo(list:TAsmList);override;
        function  dwarf_version: Word; virtual; abstract;
      end;

implementation

    uses
      sysutils,cutils,cfileutl,constexp,
      version,globals,verbose,systems,
      cpubase,cpuinfo,paramgr,
      fmodule,
      defutil,symtable,symcpu,ppu,
      llvminfo,llvmbase
      ;

{$push}
{$scopedenums on}
    type
      TLLVMDIFlags = (
        DIFlagNone = 0,
        DIFlagPrivate = 1,
        DIFlagProtected = 2,
        DIFlagPublic = 3,
        DIFlagFwdDecl = 1 shl 2,
        DIFlagAppleBlock = 1 shl 3,
        DIFlagReservedBit4 = 1 shl 4,
        { virtual inheritance at the C++ struct level, not at method level; use the SPFlag for that virtual methods) }
        DIFlagVirtual = 1 shl 5,
        DIFlagArtificial = 1 shl 6,
        DIFlagExplicit = 1 shl 7,
        DIFlagPrototyped = 1 shl 8,
        DIFlagObjcClassComplete = 1 shl 9,
        DIFlagObjectPointer = 1 shl 10,
        DIFlagVector = 1 shl 11,
        DIFlagStaticMember = 1 shl 12,
        DIFlagLValueReference = 1 shl 13,
        DIFlagRValueReference = 1 shl 14,
        DIFlagReserved = 1 shl 15,
        DIFlagSingleInheritance = 1 shl 16,
        DIFlagMultipleInheritance = 1 shl 17,
        DIFlagVirtualInheritance = 1 shl 18,
        DIFlagIntroducedVirtual = 1 shl 19,
        DIFlagBitField = 1 shl 20,
        DIFlagNoReturn = 1 shl 21,
        { at the type level, DWARF 5 DW_CC_pass_by_value }
        DIFlagTypePassByValue = 1 shl 22,
        { at the type level, DWARF 5 DW_CC_pass_by_reference }
        DIFlagTypePassByReference = 1 shl 23,
        DIFlagEnumClass = 1 shl 24,
        DIFlagThunk = 1 shl 25,

        { moved to DISPFlags in LLVM 8.0 }
        DIFlagMainSubprogram_Deprecated = 1 shl 21
    { introduced/renamed after LLVM 7.0, but nothing we need right now
        ,
        DIFlagNonTrivial,
        DIFlagBigEndian,
        DIFlagLittleEndian
    }
      );

      TLLVMDISPFlags = (
        DISPFlagVirtual = 1,
        DISPFlagPureVirtual = 2,
        DISPFlagLocalToUnit = 1 shl 2,
        DISPFlagDefinition = 1 shl 3,
        DISPFlagOptimized = 1 shl 4,
        DISPFlagPure = 1 shl 5,
        DISPFlagElemental = 1 shl 6,
        DISPFlagRecursive = 1 shl 7,
        DISPFlagMainSubprogram = 1 shl 8,
        DISPFlagDeleted = 1 shl 9,
        DISPFlagObjCDirect = 1 shl 11
      );


{$pop}

    TLLVMLocationAtom = (
      DW_OP_LLVM_fragment = $1000,         ///< Only used in LLVM metadata.
      DW_OP_LLVM_convert = $1001,          ///< Only used in LLVM metadata.
      DW_OP_LLVM_tag_offset = $1002,       ///< Only used in LLVM metadata.
      DW_OP_LLVM_entry_value = $1003,      ///< Only used in LLVM metadata.
      DW_OP_LLVM_implicit_pointer = $1004, ///< Only used in LLVM metadata.
      DW_OP_LLVM_arg = $1005              ///< Only used in LLVM metadata.
    );

{****************************************************************************
                           TLLVMMetaDefHashSet
****************************************************************************}

    class function TLLVMMetaDefHashSet.SizeOfItem: Integer;
      begin
        Result:=sizeof(TLLVMMetaDefHashSetItem);
      end;

{****************************************************************************
                              TDebugInfoLLVM
****************************************************************************}

    function TDebugInfoLLVM.absolute_llvm_path(const s:tcmdstr):tcmdstr;
      begin
        { Remove trailing / and ./ prefixes and always use a / }
        result:=BsToSlash(ExcludeTrailingPathDelimiter(FixFileName(ExpandFileName(s))));
      end;

    function TDebugInfoLLVM.get_def_metatai(def:tdef): PLLVMMetaDefHashSetItem;
      begin
        if def.dbg_state=dbg_state_unused then
          def.dbg_state:=dbg_state_used;
        { Need a new meta item? }
        result:=PLLVMMetaDefHashSetItem(fdefmeta.FindOrAdd(@def,sizeof(def)));
        { the other fields besides Data are not initialised }
        if not assigned(result^.HashSetItem.Data) then
          begin
            { will be turned into a pointerdef (in case of Objective-C types) or
              typedef later on. We only really need a typedef if this def has
              a typesym (to add the name), but it allows us to create a generic
              specialised metatype node that can represent any type. Otherwise
              we have to duplicate the logic here to determine whether it's a
              basic, derived or composite type.

              exception: procdefs because we cannot make typedefs for those}
            if def.typ<>procdef then
              begin
                result^.HashSetItem.Data:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIDerivedType);

                if is_implicit_pointer_object_type(def) then
                  result^.struct_metadef:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DICompositeType)
                else
                  result^.struct_metadef:=nil;
                result^.implmetadef:=nil;
              end
            else
              begin
                result^.HashSetItem.Data:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DISubprogram);
                result^.struct_metadef:=nil;
                result^.implmetadef:=nil;
              end;

            if def.dbg_state=dbg_state_used then
              deftowritelist.Add(def);
            defnumberlist.Add(def);
          end;
      end;

    procedure TDebugInfoLLVM.staticvarsym_set_decl(sym: tsym; decl: taillvmdecl);
      var
        entry: PHashSetItem;
      begin
        entry:=fstaticvarsymdecl.FindOrAdd(@sym,sizeof(sym));
        if assigned(entry^.Data) then
          internalerror(2022051701);
        entry^.Data:=decl;
      end;

    function TDebugInfoLLVM.staticvarsym_get_decl(sym: tsym): taillvmdecl;
      var
        entry: PHashSetItem;
      begin
        result:=nil;
        entry:=fstaticvarsymdecl.Find(@sym,sizeof(sym));
        if assigned(entry) then
          result:=taillvmdecl(entry^.Data);
      end;


    function TDebugInfoLLVM.localvarsym_get_meta(sym: tsym; out is_new: boolean): tai_llvmspecialisedmetadatanode;
      var
        entry: PHashSetItem;
      begin
        entry:=fstaticvarsymdecl.FindOrAdd(@sym,sizeof(sym));
        if not assigned(entry^.Data) then
          begin
            result:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DILocalVariable);
            current_asmdata.AsmLists[al_dwarf_info].concat(result);
            entry^.Data:=result;
            is_new:=true;
            exit;
          end;
        is_new:=false;
        result:=tai_llvmspecialisedmetadatanode(entry^.Data);
      end;


    procedure TDebugInfoLLVM.appenddef_array_internal(list: TAsmList; fordef: tdef; eledef: tdef; lowrange, highrange: asizeint);
      var
        dinode,
        subrangenode: tai_llvmspecialisedmetadatanode;
        arrayrangenode: tai_llvmunnamedmetadatanode;
      begin
        { range of the array }
        subrangenode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DISubrange);
        { include length }
        subrangenode.addqword('lowerBound',lowRange);
        if highrange>=0 then
          subrangenode.addqword('count',qword(highRange)+1)
        else
          subrangenode.addint64('count',highRange+1);
        list.concat(subrangenode);
        { collection containing the one range }
        arrayrangenode:=tai_llvmunnamedmetadatanode.create;
        arrayrangenode.addvalue(llvm_getmetadatareftypedconst(subrangenode));
        list.concat(arrayrangenode);
        { the array definition }
        dinode:=def_set_meta_impl(fordef,tspecialisedmetadatanodekind.DICompositeType);
        dinode.addenum('tag','DW_TAG_array_type');
        dinode.addmetadatarefto('baseType',def_meta_node(eledef));
        dinode.addqword('size',eledef.size*(highrange-lowrange+1)*8);
        dinode.addmetadatarefto('elements',arrayrangenode);
        list.concat(dinode);
      end;

    function TDebugInfoLLVM.getabstractprocdeftypes(list: TAsmList; def: tabstractprocdef): tai_llvmbasemetadatanode;
      var
        types: tai_llvmunnamedmetadatanode;
        i: longint;
      begin
        types:=tai_llvmunnamedmetadatanode.create;
        list.concat(types);
        { we still need a DISubProgramType in this case, but not the list of types }
        if not(cs_debuginfo in current_settings.moduleswitches) then
          exit;
        if is_void(def.returndef) then
          types.addvalue(tai_simpletypedconst.create(llvm_metadatatype,nil))
        else
          types.addvalue(def_meta_ref(def.returndef));
        for i:=0 to def.paras.count-1 do
          begin
            types.addvalue(def_meta_ref(tparavarsym(def.paras[i]).vardef));
          end;
        result:=types;
      end;

    function TDebugInfoLLVM.def_meta_impl(def: tdef): tai_llvmspecialisedmetadatanode;
      begin
        if assigned(def.typesym) then
          result:=get_def_metatai(def)^.implmetadef
        else
          result:=def_meta_node(def);
      end;

    function TDebugInfoLLVM.def_set_meta_impl(def: tdef; meta_kind: tspecialisedmetadatanodekind): tai_llvmspecialisedmetadatanode;
      begin
        if assigned(def.typesym) then
          begin
            result:=tai_llvmspecialisedmetadatanode.create(meta_kind);
            get_def_metatai(def)^.implmetadef:=result
          end
        else
          begin
            result:=def_meta_node(def);
            result.switchkind(meta_kind);
          end;
      end;

    function TDebugInfoLLVM.def_meta_class_struct(def: tobjectdef): tai_llvmspecialisedmetadatanode;
      begin
        result:=tai_llvmspecialisedmetadatanode(get_def_metatai(def)^.struct_metadef);
      end;

    function TDebugInfoLLVM.def_meta_node(def: tdef): tai_llvmspecialisedmetadatanode;
      begin
        if not is_void(def) then
          result:=tai_llvmspecialisedmetadatanode(get_def_metatai(def)^.HashSetItem.Data)
        else
          result:=nil;
      end;

    function TDebugInfoLLVM.def_meta_ref(def: tdef): tai_simpletypedconst;
      begin
        result:=llvm_getmetadatareftypedconst(def_meta_node(def));
      end;

    constructor TDebugInfoLLVM.Create;
      begin
        inherited Create;

        fenums:=nil;
        fretainedtypes:=nil;
        fglobals:=nil;
        femptyexpression:=nil;
        fderefexpression:=nil;
        fcunode:=nil;

        ffilemeta:=thashset.Create(10000,true,false);
        flocationmeta:=thashset.Create(10000,true,false);
        flexicalblockfilemeta:=thashset.Create(100,true,false);
        fdefmeta:=TLLVMMetaDefHashSet.Create(10000,true,false);
        fstaticvarsymdecl:=thashset.create(10000,true,false);

        defnumberlist:=TFPObjectList.create(false);
        deftowritelist:=TFPObjectList.create(false);

        vardatadef:=nil;
      end;


    destructor TDebugInfoLLVM.Destroy;
      begin
        // don't free fenums/fretainedtypes/fglobals, they get emitted in the assembler list
        ffilemeta.free;
        ffilemeta:=nil;
        flocationmeta.free;
        flocationmeta:=nil;
        flexicalblockfilemeta.free;
        flexicalblockfilemeta:=nil;
        fdefmeta.free;
        fdefmeta:=nil;
        fstaticvarsymdecl.free;
        fstaticvarsymdecl:=nil;
        flocalvarsymmeta.free;
        flocalvarsymmeta:=nil;
        defnumberlist.free;
        defnumberlist:=nil;
        deftowritelist.free;
        deftowritelist:=nil;
        fcunode.free;
        fcunode:=nil;
        inherited Destroy;
      end;


    procedure TDebugInfoLLVM.ensuremetainit;
      begin
        if not assigned(fllvm_dbg_declare_pd) then
          fllvm_dbg_declare_pd:=search_system_proc('llvm_dbg_declare');
        if not assigned(fllvm_dbg_addr_pd) then
          fllvm_dbg_addr_pd:=search_system_proc('llvm_dbg_addr');
        if not assigned(fenums) then
          begin
            fenums:=tai_llvmunnamedmetadatanode.create;
            fretainedtypes:=tai_llvmunnamedmetadatanode.create;
            fglobals:=tai_llvmunnamedmetadatanode.create;
            femptyexpression:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIExpression);
            fderefexpression:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIExpression);
            fderefexpression.addenum('','DW_OP_deref');
            fcunode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DICompileUnit);
          end;
      end;

    procedure TDebugInfoLLVM.resetfornewmodule;
      var
        i: longint;
      begin
        { for LLVM, we need to generate the procdef type info (or at least
          temporary references to it) already during the generation of the line
          info (all line info metadata needs a reference to its parent scope,
          the procdef). Since the line info is generated per procedure and
          the type info only at the end, we can't allocate the type info
          structures at the start of the type info generation like for other
          debug info producers. Instead, we have to initialise everything in the
          constructor, and then reset it at the end of the debug info pass
          (inserting the module info) }
        ffilemeta.Clear;
        flocationmeta.Clear;
        flexicalblockfilemeta.Clear;
        fdefmeta.free;
        fstaticvarsymdecl.Clear;
        { one item per def, plus some extra space in case of nested types,
          externally used types etc (it will grow further if necessary) }
        i:=current_module.localsymtable.DefList.count*4;
        if assigned(current_module.globalsymtable) then
          inc(i,current_module.globalsymtable.DefList.count*2);
        fdefmeta:=TLLVMMetaDefHashSet.Create(i,true,false);

        defnumberlist.Clear;
        deftowritelist.Clear;
        fcunode:=nil;
        fenums:=nil;
        fretainedtypes:=nil;
        fglobals:=nil;
        femptyexpression:=nil;
        fderefexpression:=nil;
      end;

    procedure TDebugInfoLLVM.collectglobalsyms;
      var
        i: TAsmListType;
        hp: tai;
      begin
        for i in globaldataasmlisttypes do
          begin
            if not assigned(current_asmdata.AsmLists[i]) then
              continue;
            hp:=tai(current_asmdata.AsmLists[i].First);
            while assigned(hp) do
              begin
                if (hp.typ=ait_llvmdecl) and
                   assigned(taillvmdecl(hp).sym) then
                     staticvarsym_set_decl(taillvmdecl(hp).sym,taillvmdecl(hp));
                hp:=tai(hp.next);
              end;
          end;
      end;


    procedure TDebugInfoLLVM.updatelocalvardbginfo(hp: taillvm; pd: tprocdef; functionscope: tai_llvmspecialisedmetadatanode);
      var
        opindex, callparaindex: longint;
        paras: tfplist;
        sympara,
        exprpara: pllvmcallpara;
        sym: tabstractnormalvarsym;
        dilocalvar: tai_llvmspecialisedmetadatanode;
        isnewlocalvardi,
        deref: boolean;
      begin
        { not really clean since hardcoding the structure of the call
          instruction's procdef encoding, but quick }
        if (hp.oper[taillvm.callpdopernr]^.def.typ<>pointerdef) or
           ((tpointerdef(hp.oper[taillvm.callpdopernr]^.def).pointeddef<>fllvm_dbg_declare_pd) and
            (tpointerdef(hp.oper[taillvm.callpdopernr]^.def).pointeddef<>fllvm_dbg_addr_pd)) then
          exit;
        deref:=false;

        sympara:=hp.getcallpara(1);
        exprpara:=hp.getcallpara(2);

        if sympara^.val.typ<>top_local then
          internalerror(2022052613);
        sym:=tabstractnormalvarsym(sympara^.val.localsym);
        dilocalvar:=localvarsym_get_meta(sym,isnewlocalvardi);
        sympara^.loadtai(llvm_getmetadatareftypedconst(dilocalvar));
        if isnewlocalvardi then
          begin
            dilocalvar.addstring('name',symname(sym,false));
            if sym.typ=paravarsym then
              begin
                dilocalvar.addint64('arg',tparavarsym(sym).paranr);
                if paramanager.push_addr_param(sym.varspez,sym.vardef,pd.proccalloption) then
                  deref:=true;
              end;
            dilocalvar.addmetadatarefto('scope',functionscope);
            try_add_file_metaref(dilocalvar,sym.fileinfo,false);
            dilocalvar.addmetadatarefto('type',def_meta_node(sym.vardef));
            if vo_is_self in sym.varoptions then
              dilocalvar.addenum('flags','DIFlagArtificial');
          end
        else
          begin
            if (sym.typ=paravarsym) and
               paramanager.push_addr_param(sym.varspez,sym.vardef,pd.proccalloption) then
              deref:=true;
          end;

        if not deref then
          exprpara^.loadtai(llvm_getmetadatareftypedconst(femptyexpression))
        else
          exprpara^.loadtai(llvm_getmetadatareftypedconst(fderefexpression));
      end;


    function TDebugInfoLLVM.file_getmetanode(moduleindex: tfileposmoduleindex; fileindex: tfileposfileindex): tai_llvmspecialisedmetadatanode;
      var
        infile: tinputfile;
        dirname: TSymStr;
        item: PHashSetItem;
        metaitem: tai_llvmspecialisedmetadatanode;
        modfileindex: packed record
          moduleindex: tfileposmoduleindex;
          fileindex: tfileposfileindex;
        end;
      begin
        modfileindex.moduleindex:=moduleindex;
        modfileindex.fileindex:=fileindex;
        item:=ffilemeta.FindOrAdd(@modfileindex,sizeof(modfileindex));
        if not assigned(item^.Data) then
          begin
            infile:=get_module(moduleindex).sourcefiles.get_file(fileindex);
            if not assigned(infile) then
              begin
                result:=nil;
                exit;
              end;
            if infile.path = '' then
              dirname:=absolute_llvm_path('.')
            else
              begin
                { add the canonical form here already to avoid problems with }
                { paths such as './' etc                                     }
                dirname:=absolute_llvm_path(infile.path);
              end;
            if dirname='' then
              dirname:='.';
            metaitem:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIFile);
            metaitem.addstring('filename',infile.name);
            metaitem.addstring('directory',dirname);
            current_asmdata.AsmLists[al_dwarf_line].concat(metaitem);
            item^.Data:=metaitem;
          end;
        result:=tai_llvmspecialisedmetadatanode(item^.Data);
      end;


    function TDebugInfoLLVM.filepos_getmetanode(const filepos: tfileposinfo; const functionfileinfo: tfileposinfo; const functionscope: tai_llvmspecialisedmetadatanode; nolineinfo: boolean): tai_llvmspecialisedmetadatanode;
      var
        item: PHashSetItem;
        filemeta,
        locationscopemeta: tai_llvmspecialisedmetadatanode;
        lexicalblockkey: packed record
          scopemeta,
          filemeta: tai_llvmspecialisedmetadatanode;
        end;
        locationkey: packed record
          scope: tai_llvmspecialisedmetadatanode;
          line: tfileposline;
          column: tfileposcolumn;
        end;

      begin
        result:=nil;
        if (filepos.fileindex<>0) then
          filemeta:=file_getmetanode(filepos.moduleindex,filepos.fileindex)
        else
          filemeta:=file_getmetanode(functionfileinfo.moduleindex,functionfileinfo.fileindex);
        if not assigned(filemeta) then
          exit;
        if (filepos.fileindex<>0) and
           (filepos.fileindex<>functionfileinfo.fileindex) then
          begin
            lexicalblockkey.scopemeta:=functionscope;
            lexicalblockkey.filemeta:=filemeta;
            item:=flexicalblockfilemeta.FindOrAdd(@lexicalblockkey,sizeof(lexicalblockkey));
            if not assigned(item^.Data) then
              begin
                locationscopemeta:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DILexicalBlockFile);
                locationscopemeta.addmetadatarefto('scope',functionscope);
                locationscopemeta.addmetadatarefto('file',filemeta);
                locationscopemeta.addint64('discriminator',0);
                current_asmdata.AsmLists[al_dwarf_line].concat(locationscopemeta);
                item^.Data:=locationscopemeta;
              end
            else
              locationscopemeta:=tai_llvmspecialisedmetadatanode(item^.Data);
          end
        else
          locationscopemeta:=functionscope;
        locationkey.scope:=locationscopemeta;
        if not nolineinfo then
          begin
            locationkey.line:=filepos.line;
            locationkey.column:=filepos.column;
          end
        else
          begin
            locationkey.line:=0;
            locationkey.column:=0;
          end;
        item:=flocationmeta.FindOrAdd(@locationkey,sizeof(locationkey));
        if not assigned(item^.Data) then
          begin
            result:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DILocation);
            if not nolineinfo then
              begin
                result.addqword('line',filepos.line);
                result.addqword('column',filepos.column);
              end
            else
              result.addqword('line',0);
            result.addmetadatarefto('scope',locationscopemeta);
            current_asmdata.AsmLists[al_dwarf_line].concat(result);
            item^.Data:=result;
          end
        else
          result:=tai_llvmspecialisedmetadatanode(item^.Data);
      end;


    procedure TDebugInfoLLVM.try_add_file_metaref(dinode: tai_llvmspecialisedmetadatanode; const fileinfo: tfileposinfo; includescope: boolean);
      var
        filemeta: tai_llvmbasemetadatanode;
      begin
        filemeta:=file_getmetanode(fileinfo.moduleindex,fileinfo.fileindex);
        if assigned(filemeta) then
          begin
            if includescope then
              begin
                dinode.addmetadatarefto('scope',filemeta);
              end;
            dinode.addmetadatarefto('file',filemeta);
            dinode.addqword('line',fileinfo.line);
          end;
      end;


    function TDebugInfoLLVM.add_line_metanode(const fileinfo: tfileposinfo): tai_llvmspecialisedmetadatanode;
      var
        filemeta: tai_llvmbasemetadatanode;
      begin
        filemeta:=file_getmetanode(fileinfo.moduleindex,fileinfo.fileindex);
        if not assigned(filemeta) then
          internalerror(2022041730);
        result:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DILocation);
        result.addqword('line',fileinfo.line);
        result.addqword('column',fileinfo.column);
        result.addmetadatarefto('scope',filemeta);
        current_asmdata.AsmLists[al_dwarf_line].concat(result);
      end;


    procedure TDebugInfoLLVM.appenddef_ord(list:TAsmList;def:torddef);
      var
        ordtype: tordtype;
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        { nothing, must be referenced as "null" in the using declaration }
        if is_void(def) then
          exit;

        ordtype:=def.ordtype;
        if ordtype=customint then
          ordtype:=range_to_basetype(def.low,def.high);

        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIBasicType);
        case ordtype of
          s8bit,
          s16bit,
          s32bit,
          u8bit,
          u16bit,
          u32bit,
          u64bit,
          s64bit,
          u128bit,
          s128bit:
            begin
              dinode.addqword('size',def.size*8);
              if def.alignment<>def.size then
                dinode.addqword('align',def.alignment*8);
              { generate proper signed/unsigned info for types like 0..3 }
              { these are s8bit, but should be identified as unsigned    }
              { because otherwise they are interpreted wrongly when used }
              { in a bitpacked record                                    }
              if def.low<0 then
                dinode.addenum('encoding','DW_ATE_signed')
              else
                dinode.addenum('encoding','DW_ATE_unsigned');
            end;
          uvoid :
            begin
              { checked above }
            end;
          uchar,
          uwidechar :
            begin
              dinode.addqword('size',def.size*8);
              dinode.addenum('encoding','DW_ATE_unsigned_char');
            end;
          pasbool1,
          pasbool8,
          bool8bit,
          pasbool16,
          bool16bit,
          pasbool32,
          bool32bit,
          pasbool64,
          bool64bit:
            begin
              dinode.addqword('size',def.size*8);
              dinode.addenum('encoding','DW_ATE_boolean');
            end;
          scurrency:
            begin
              { we should use DW_ATE_signed_fixed, however it isn't supported yet by LLVM }
              dinode.addqword('size',def.size*8);
              dinode.addenum('encoding','DW_ATE_signed');
            end;
          customint:
            internalerror(2021111502);
        end;
        list.concat(dinode);
      end;

    procedure TDebugInfoLLVM.appenddef_float(list:TAsmList;def:tfloatdef);
      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIBasicType);
        case def.floattype of
          s32real,
          s64real,
          s80real,
          sc80real,
          s128real:
            begin
              dinode.addqword('size',def.size*8);
              if def.alignment<>def.size then
                dinode.addqword('align',def.alignment*8);
              dinode.addenum('encoding','DW_ATE_float');
            end;
          s64currency:
            begin
              { we should use DW_ATE_signed_fixed, however it isn't supported yet by LLVM }
              dinode.addqword('size',def.size*8);
              dinode.addenum('encoding','DW_ATE_signed');
            end;
          s64comp:
            begin
              { we should use DW_ATE_signed_fixed, however it isn't supported yet by LLVM }
              dinode.addqword('size',def.size*8);
              dinode.addenum('encoding','DW_ATE_signed');
            end;
        end;
        list.concat(dinode);
      end;


    procedure TDebugInfoLLVM.appenddef_enum(list:TAsmList;def:tenumdef);
      var
        hp : tenumsym;
        i : longint;
        dinode: tai_llvmspecialisedmetadatanode;
        enumelem: tai_llvmspecialisedmetadatanode;
        enumlist: tai_llvmunnamedmetadatanode;
      begin
        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DICompositeType);
        dinode.addenum('tag','DW_TAG_enumeration_type');
        dinode.addqword('size',def.size*8);
        dinode.addstring('identifier',def.mangledparaname);

        { register in module's list of enums (to ensure the debug info gets
          emitted even if the enum is not used in the current module) }
        fenums.addvalue(llvm_getmetadatareftypedconst(dinode));

        enumlist:=tai_llvmunnamedmetadatanode.create;
        { add enum symbols }
        for i:=0 to def.symtable.SymList.Count-1 do
          begin
            hp:=tenumsym(def.symtable.SymList[i]);
            if hp.value<def.minval then
              continue
            else if hp.value>def.maxval then
              break;
            enumelem:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIEnumerator);
            enumelem.addstring('name',symname(hp, false));
            enumelem.addint64('value',hp.value);
            list.concat(enumelem);
            enumlist.addvalue(llvm_getmetadatareftypedconst(enumelem));
          end;
        if enumlist.valuecount<>0 then
          begin
            list.concat(enumlist);
            dinode.addmetadatarefto('elements',enumlist);
          end
        else
          begin
            enumlist.free;
          end;
        list.concat(dinode);
      end;


    procedure TDebugInfoLLVM.appenddef_array(list:TAsmList;def:tarraydef);
      var
        dinode,
        subrangenode,
        exprnode: tai_llvmspecialisedmetadatanode;
        arrayrangenode: tai_llvmunnamedmetadatanode;
        size : qword;
        nesteddef: tdef;
        power: longint;
        flags: TLLVMDIFlags;
      begin
        if is_dynamic_array(def) { and
           not(llvmflag_array_datalocation in llvmversion_properties[current_settings.llvmversion]) } then
          begin
            dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
            dinode.addenum('tag','DW_TAG_pointer_type');
            dinode.addmetadatarefto('baseType',def_meta_node(def.elementdef));
            dinode.addqword('size',def.size*8);
            list.concat(dinode);
            exit;
          end;

        { open arrays etc need to access the high parameter to define their range,
          which is not possible here since we need the parasym rather than the def }
        if is_open_array(def) then
          begin
            (*
            if llvmflag_array_datalocation in llvmversion_properties[current_settings.llvmversion] then
              begin
                dinode:=def_meta_impl(def);
                { should be generated as part of the parasym }
                if not assigned(dinode) then
                  internalerror(2021112002);
              end
            else *)
              begin
                { no idea about the size, generate an array of 1 element -- although it could be empty }
                appenddef_array_internal(list,def,def.elementdef,0,1);
              end;
              exit;
          end;

        if is_array_of_const(def) then
          begin
            { no idea about the size, generate an array of 1 element -- although it could be empty }
            appenddef_array_internal(list,def,def.elementdef,0,1);
            exit;
          end;

        if is_special_array(def)
           and not((llvmflag_array_datalocation in llvmversion_properties[current_settings.llvmversion]) and
                   is_dynamic_array(def)) then
          internalerror(2021121902);

        { todo: proper support for bitpacked arrays }
        if is_packed_array(def) and
           (((def.elementdef.packedbitsize mod 8)<>0) or
            not ispowerof2(def.elementdef.packedbitsize div 8,power)) then
          begin
            { for now just encode as an array of bytes }
            appenddef_array_internal(list,def,u8inttype,0,def.size-1);
            exit;
          end;

        { collection of all ranges of the array (to support multi-dimensional arrays) }
        arrayrangenode:=tai_llvmunnamedmetadatanode.create;
        list.concat(arrayrangenode);

        { range of the array }
        subrangenode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DISubrange);
        if is_dynamic_array(def) then
          begin
            exprnode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIExpression);
            exprnode.addenum('','DW_OP_push_object_address');
            exprnode.addenum('','DW_OP_constu');
            exprnode.addint64('',ord(sizeof(pint)));
            exprnode.addenum('','DW_OP_minus');
            exprnode.addenum('','DW_OP_deref');
            list.concat(exprnode);
            subrangenode.addmetadatarefto('upperBound',exprnode);
            subrangenode.addint64('lowerBound',def.lowrange);
          end
        else
          begin
            subrangenode.addqword('count',def.highrange-def.lowrange+1);
            subrangenode.addint64('lowerBound',def.lowrange);
          end;
          list.concat(subrangenode);
        nesteddef:=def.elementdef;
        arrayrangenode.addvalue(llvm_getmetadatareftypedconst(subrangenode));
        while (nesteddef.typ=arraydef) and
              not is_special_array(nesteddef) do
          begin
            subrangenode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DISubrange);
            subrangenode.addqword('count',tarraydef(nesteddef).highrange-tarraydef(nesteddef).lowrange+1);
            subrangenode.addint64('lowerBound',tarraydef(nesteddef).lowrange);
            list.concat(subrangenode);
            arrayrangenode.addvalue(llvm_getmetadatareftypedconst(subrangenode));
            nesteddef:=tarraydef(nesteddef).elementdef;
          end;
        { the array definition }
        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DICompositeType);
        dinode.addenum('tag','DW_TAG_array_type');
        dinode.addmetadatarefto('baseType',def_meta_node(nesteddef));
        dinode.addmetadatarefto('elements',arrayrangenode);
        if is_vector(def) then
          dinode.addenum('flags','DIFlagVector');
        if not is_dynamic_array(def) then
{$ifdef cpu64bitalu}
          if def.size>=(qword(1) shl 61) then
            { LLVM internally "only" supports sizes up to 1 shl 61, because they
              store all sizes in bits in a qword; the rationale is that there
              is no hardware supporting a full 64 bit address space either }
            dinode.addqword('size',((qword(1) shl 61) - 1)*8)
          else
{$endif def cpu64bitalu}
            dinode.addqword('size',def.size*8)
        else
          begin
            exprnode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIExpression);
            exprnode.addenum('','DW_OP_LLVM_implicit_pointer');
            list.concat(exprnode);
            dinode.addmetadatarefto('dataLocation',exprnode);
          end;
        list.concat(dinode);
      end;


    procedure TDebugInfoLLVM.appenddef_record(list:TAsmList;def:trecorddef);
      begin
        if assigned(def.objname) then
          appenddef_record_named(list,def,def,def.objname^)
        else
          appenddef_record_named(list,def,def,'');
      end;


    procedure TDebugInfoLLVM.appenddef_record_named(list:TAsmList; fordef: tdef; def:trecorddef; const name: TSymStr);
      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        dinode:=def_set_meta_impl(fordef,tspecialisedmetadatanodekind.DICompositeType);
        list.concat(dinode);
        dinode.addenum('tag','DW_TAG_structure_type');
        appenddef_struct_named(list,def,dinode,tai_llvmunnamedmetadatanode.create,name);
      end;


    procedure TDebugInfoLLVM.appenddef_struct_named(list: TAsmList; def: tabstractrecorddef; structdi: tai_llvmspecialisedmetadatanode; initialfieldlist: tai_llvmunnamedmetadatanode; const name: TSymStr);
      var
        cappedsize: asizeuint;
      begin
        if (name<>'') then
          structdi.addstring('name',name);
        if assigned(def.typesym) then
          try_add_file_metaref(structdi,def.typesym.fileinfo,false);
        if is_packed_record_or_object(def) then
          cappedsize:=tabstractrecordsymtable(def.symtable).datasize
{$ifdef cpu64bitalu}
        else if def.size>=(qword(1) shl 61) then
          { LLVM internally "only" supports sizes up to 1 shl 61, because they
            store all sizes in bits in a qword; the rationale is that there
            is no hardware supporting a full 64 bit address space either }
          cappedsize:=((qword(1) shl 61) - 1)*8
{$endif def cpu64bitalu}
        else
          cappedsize:=tabstractrecordsymtable(def.symtable).datasize*8;
        structdi.addqword('size',cappedsize);

        appenddef_struct_fields(list,def,structdi,initialfieldlist,cappedsize);
        write_symtable_procdefs(current_asmdata.asmlists[al_dwarf_info],def.symtable);
      end;


    procedure TDebugInfoLLVM.appenddef_struct_fields(list: TAsmlist; def: tabstractrecorddef; defdinode: tai_llvmspecialisedmetadatanode; initialfieldlist: tai_llvmunnamedmetadatanode; cappedsize: asizeuint);

      { returns whether we need to create a nested struct in the variant to hold
        multiple successive fields, or whether the next field starts at the
        same offset as the current one. I.e., it returns false for
          case byte of
            0: (b: byte);
            1: (l: longint);
          end

        but true for

          case byte of
            0: (b1,b2: byte);
          end

        and

          case byte of
            0: (b1: byte;
                case byte of 0:
                  b2: byte;
               )
          end
      }
      function variantfieldstartsnewstruct(field: tfieldvarsym; recst: tabstractrecordsymtable; fieldidx: longint): boolean;
        var
          nextfield: tfieldvarsym;
        begin
          result:=false;
          inc(fieldidx);
          if fieldidx>=recst.symlist.count then
            exit;
          { can't have properties or procedures between to start fields of the
            same variant }
          if tsym(recst.symlist[fieldidx]).typ<>fieldvarsym then
            exit;
          nextfield:=tfieldvarsym(recst.symlist[fieldidx]);
          if nextfield.fieldoffset=field.fieldoffset then
            exit;
          result:=true;
        end;

      type
        tvariantinfo = record
          startfield: tfieldvarsym;
          uniondi: tai_llvmspecialisedmetadatanode;
          variantfieldlist: tai_llvmunnamedmetadatanode;
          curvariantstructfieldlist: tai_llvmunnamedmetadatanode;
        end;
        pvariantinfo = ^tvariantinfo;

      function bitoffsetfromvariantstart(field: tfieldvarsym; variantinfolist: tfplist; totalbitsize: ASizeUInt): qword;
        var
          variantstartfield: tfieldvarsym;
        begin
          if not assigned(variantinfolist) then
            begin
              result:=field.bitoffset;
              exit;
            end;
          result:=0;
          if vo_is_first_field in field.varoptions then
            exit;
          variantstartfield:=pvariantinfo(variantinfolist[variantinfolist.count-1])^.startfield;
          { variant fields always start on a byte boundary, so no need for
            rounding/truncating }
          result:=field.bitoffset-variantstartfield.bitoffset;
        end;

      var
        variantinfolist: tfplist;
        variantinfo: pvariantinfo;
        recst: tabstractrecordsymtable;
        scope,
        fielddi,
        uniondi,
        structdi: tai_llvmspecialisedmetadatanode;
        fieldlist: tai_llvmunnamedmetadatanode;
        i, varindex: longint;
        field: tfieldvarsym;
        bitoffset: asizeuint;
        bpackedrecst,
        classorobject: boolean;
      begin
        recst:=tabstractrecordsymtable(def.symtable);
        bpackedrecst:=recst.fieldalignment=bit_alignment;
        scope:=defdinode;
        variantinfolist:=nil;
        classorobject:=is_class_or_interface_or_object(def);

        fieldlist:=initialfieldlist;
        list.concat(fieldlist);
        defdinode.addmetadatarefto('elements',fieldlist);

        for i:=0 to recst.symlist.count-1 do
          begin
            if (tsym(recst.symlist[i]).typ<>fieldvarsym) then
              continue;

            field:=tfieldvarsym(recst.symlist[i]);
            if (sp_static in field.symoptions) then
              exit;

            { start of a new variant part? }
            if vo_is_first_field in field.varoptions then
              begin
                if not assigned(variantinfolist) then
                  begin
                    variantinfolist:=tfplist.create;
                  end;
                varindex:=variantinfolist.count-1;
                if (varindex=-1) or
                   (pvariantinfo(variantinfolist[varindex])^.startfield.fieldoffset<field.fieldoffset) then
                  begin
                    { more deeply nested variant }
                    uniondi:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DICompositeType);

                    fielddi:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIDerivedType);
                    fielddi.addenum('tag','DW_TAG_member');
                    fielddi.addmetadatarefto('scope',scope);
                    try_add_file_metaref(fielddi,field.fileinfo,false);
                    fielddi.addmetadatarefto('baseType',uniondi);
                    fielddi.addint64('size',cappedsize-min(field.bitoffset,cappedsize));
                    bitoffset:=bitoffsetfromvariantstart(field,variantinfolist,cappedsize);
                    if bitoffset<>0 then
                      fielddi.addqword('offset',bitoffset);
                    list.concat(fielddi);
                    fieldlist.addvalue(llvm_getmetadatareftypedconst(fielddi));

                    list.concat(uniondi);
                    uniondi.addenum('tag','DW_TAG_union_type');
                    uniondi.addmetadatarefto('scope',scope);
                    try_add_file_metaref(uniondi,field.fileinfo,false);
                    { the size of this variant part is the total size of the
                      record minus the start of this field; not 100% correct
                      in case of multiple parallel nested variants, but not
                      really important since it's all padding anyway }
                    uniondi.addint64('size',cappedsize-min(field.bitoffset,cappedsize));
                    fieldlist:=tai_llvmunnamedmetadatanode.create;
                    list.concat(fieldlist);
                    uniondi.addmetadatarefto('elements',fieldlist);

                    scope:=uniondi;

                    new(variantinfo);
                    variantinfo^.startfield:=field;
                    variantinfo^.uniondi:=uniondi;
                    variantinfo^.variantfieldlist:=fieldlist;
                    variantinfo^.curvariantstructfieldlist:=nil;

                    variantinfolist.Add(variantinfo);
                    inc(varindex);
                  end
                else
                  begin
                    {finalise more deeply nested variants }
                    while (varindex>=0) and
                          (pvariantinfo(variantinfolist[varindex])^.startfield.fieldoffset>field.fieldoffset) do
                      begin
                        dispose(pvariantinfo(variantinfolist[varindex]));
                        dec(varindex);
                      end;
                    if (varindex<0) then
                      internalerror(2022060610);
                    variantinfo:=pvariantinfo(variantinfolist[varindex]);
                    if variantinfo^.startfield.fieldoffset<>field.fieldoffset then
                      internalerror(2022060611);

                    { a variant part is always the last part -> end of previous
                      struct, if any}
                    variantinfo^.curvariantstructfieldlist:=nil;

                    fieldlist:=variantinfo^.variantfieldlist;
                    scope:=variantinfo^.uniondi;

                    { variant at the same level as a previous one }
                    variantinfolist.count:=varindex+1;
                  end;

                if not variantfieldstartsnewstruct(field,recst,i) then
                  begin
                    variantinfo^.curvariantstructfieldlist:=nil;
                    fieldlist:=variantinfo^.variantfieldlist;
                    scope:=variantinfo^.uniondi;
                  end
                else
                  begin
                    structdi:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DICompositeType);
                    list.concat(structdi);
                    structdi.addenum('tag','DW_TAG_structure_type');
                    structdi.addmetadatarefto('scope',variantinfo^.uniondi);
                    structdi.addint64('size',cappedsize-min(field.bitoffset,cappedsize));
                    variantinfo^.curvariantstructfieldlist:=tai_llvmunnamedmetadatanode.create;
                    list.concat(variantinfo^.curvariantstructfieldlist);
                    structdi.addmetadatarefto('elements',variantinfo^.curvariantstructfieldlist);
                    fieldlist.addvalue(llvm_getmetadatareftypedconst(structdi));

                    fieldlist:=variantinfo^.curvariantstructfieldlist;
                    scope:=structdi;
                  end;
              end;

            fielddi:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIDerivedType);
            fielddi.addenum('tag','DW_TAG_member');
            fielddi.addstring('name',symname(field,false));
            fielddi.addmetadatarefto('scope',scope);
            try_add_file_metaref(fielddi,field.fileinfo,false);
            { the vmt field's type is voidpointerdef, because when it gets
              inserted we can't build the vmt's def yet }
            if classorobject and
               (field=tobjectdef(def).vmt_field) then
              fielddi.addmetadatarefto('baseType',def_meta_node(cpointerdef.getreusable(tobjectdef(def).vmt_def)))
            else
              fielddi.addmetadatarefto('baseType',def_meta_node(field.vardef));
            if bpackedrecst and
               is_ordinal(field.vardef) then
              fielddi.addqword('size',field.getpackedbitsize)
            else
              fielddi.addqword('size',min(asizeuint(field.getsize)*8,cappedsize));
            bitoffset:=bitoffsetfromvariantstart(field,variantinfolist,cappedsize);
            if bitoffset<>0 then
              fielddi.addqword('offset',bitoffset);
            { currently only vmt }
            if field.visibility=vis_hidden then
              fielddi.addenum('flags','DIFlagArtificial');
            fieldlist.addvalue(llvm_getmetadatareftypedconst(fielddi));
            list.concat(fielddi);
          end;
        if assigned(variantinfolist) then
          begin
            for i:=0 to variantinfolist.count-1 do
              begin
                dispose(pvariantinfo(variantinfolist[i]));
              end;
          end;
        variantinfolist.free;
      end;


    procedure TDebugInfoLLVM.appenddef_pointer(list:TAsmList;def:tpointerdef);
      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
        dinode.addenum('tag','DW_TAG_pointer_type');
        if not(is_voidpointer(def)) then
          dinode.addmetadatarefto('baseType',def_meta_node(def.pointeddef))
        else
          dinode.addmetadatarefto('baseType',nil);
        list.concat(dinode);
      end;


    procedure TDebugInfoLLVM.appenddef_formal(list: TAsmList; def: tformaldef);
      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
        dinode.addenum('tag','DW_TAG_pointer_type');
        dinode.addmetadatarefto('baseType',nil);
        list.concat(dinode);
      end;


    procedure TDebugInfoLLVM.appenddef_string(list:TAsmList;def:tstringdef);

      procedure addnormalstringdef(const name: TSymStr; lendef: tdef; maxlen: asizeuint);
        var
          dinode,
          subrangenode,
          exprnode: tai_llvmspecialisedmetadatanode;
          arrayrangenode: tai_aggregatetypedconst;
          { maxlen can be > high(int64) }
          slen : asizeuint;
          arr : tasmlabel;
        begin
          { fix length of openshortstring }
          slen:=aword(def.len);
          if (slen=0) or
             (slen>maxlen) then
            slen:=maxlen;
          appenddef_array_internal(list,def,cansichartype,0,slen);
        end;

      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        case def.stringtype of
          st_shortstring:
            begin
              addnormalstringdef('ShortString',u8inttype,255);
            end;
          st_longstring:
            begin
              { a) we don't actually support variables of this type currently
                b) this type is only used as the type for constant strings
                   > 255 characters
                c) in such a case, gdb will allocate and initialise enough
                   memory to hold the maximum size for such a string
                -> don't use high(qword)/high(cardinal) as maximum, since that
                 will cause exhausting the VM space, but some "reasonably high"
                 number that should be enough for most constant strings
              }
{$ifdef cpu64bitaddr}
              addnormalstringdef('LongString',u64inttype,qword(1024*1024));
{$endif cpu64bitaddr}
{$ifdef cpu32bitaddr}
              addnormalstringdef('LongString',u32inttype,cardinal(1024*1024));
{$endif cpu32bitaddr}
{$ifdef cpu16bitaddr}
              addnormalstringdef('LongString',u16inttype,cardinal(1024));
{$endif cpu16bitaddr}
           end;
         st_ansistring:
           begin
             // Todo: dynamic length "array"
             dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
             dinode.addenum('tag','DW_TAG_pointer_type');
             dinode.addmetadatarefto('baseType',def_meta_node(cansichartype));
             list.concat(dinode);
           end;
         st_unicodestring,
         st_widestring:
           begin
             // Todo: dynamic length "array"
             dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
             dinode.addenum('tag','DW_TAG_pointer_type');
             dinode.addmetadatarefto('baseType',def_meta_node(cwidechartype));
             list.concat(dinode);
           end;
        end;
      end;

    procedure TDebugInfoLLVM.appenddef_procvar(list:TAsmList;def:tprocvardef);
      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        { plain pointer for now }
        if def.is_addressonly then
          begin
            dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
            dinode.addenum('tag','DW_TAG_pointer_type');
            dinode.addmetadatarefto('baseType',nil);
            list.concat(dinode);
          end
        else
          begin
            appenddef_array_internal(list,def,voidcodepointertype,1,2);
          end;
      end;


  procedure TDebugInfoLLVM.appenddef_file(list: TAsmList; def: tfiledef);
    var
      dinode: tai_llvmspecialisedmetadatanode;
    begin
      dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DICompositeType);
      dinode.addenum('tag','DW_TAG_structure_type');
      if assigned(def.typesym) then
        dinode.addstring('name',symname(def.typesym, false));
      dinode.addqword('size',def.size*8);

      list.concat(dinode);
    end;


    procedure TDebugInfoLLVM.appenddef_object(list: TAsmList; def: tobjectdef);
      var
        dinode,
        structdi,
        inheritancedi: tai_llvmspecialisedmetadatanode;
        fields: tai_llvmunnamedmetadatanode;
      begin
        inheritancedi:=nil;
        fields:=tai_llvmunnamedmetadatanode.create;
        if assigned(def.childof) then
          begin
            inheritancedi:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIDerivedType);
            list.concat(inheritancedi);
            inheritancedi.addenum('tag','DW_TAG_inheritance');
            if is_implicit_pointer_object_type(def) then
              inheritancedi.addmetadatarefto('baseType',def_meta_class_struct(def.childof))
            else
              inheritancedi.addmetadatarefto('baseType',def_meta_node(def.childof));
            { Pascal only has public inheritance }
            if def.objecttype<>odt_cppclass then
              inheritancedi.addenum('flags','DIFlagPublic');
            fields.addvalue(llvm_getmetadatareftypedconst(inheritancedi));
          end;
        if is_implicit_pointer_object_type(def) then
          begin
            dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
            dinode.addenum('tag','DW_TAG_pointer_type');

            structdi:=def_meta_class_struct(def);
            list.concat(structdi);
            structdi.addenum('tag','DW_TAG_class_type');
            appenddef_struct_named(list,def,structdi,fields,def.objname^);

            { implicit pointer }
            dinode.addmetadatarefto('baseType',structdi);
          end
        else case def.objecttype of
          odt_cppclass,
          odt_object:
            begin
              dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DICompositeType);
              dinode.addenum('tag','DW_TAG_class_type');
              appenddef_struct_named(list,def,dinode,fields,def.objname^);
            end;
          odt_objcclass:
            begin
              { Objective-C class: same as regular class, except for
                  a) Apple-specific tag that identifies it as an Objective-C class
                  b) use extname^ instead of objname
              }
              dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DICompositeType);
              dinode.addenum('tag','DW_TAG_class_type');
              dinode.addenum('runtimeLang','DW_LANG_ObjC');
              appenddef_struct_named(list,def,dinode,fields,def.objextname^);
            end;
          odt_objcprotocol:
            begin
              dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
              dinode.addenum('tag','DW_TAG_pointer_type');
              dinode.addmetadatarefto('baseType',nil);
            end;
          else
            internalerror(2022060710);
        end;
        list.concat(dinode);
        if assigned(inheritancedi) then
          inheritancedi.addmetadatarefto('scope',dinode);
        write_symtable_procdefs(current_asmdata.asmlists[al_dwarf_info],def.symtable);
      end;


    procedure TDebugInfoLLVM.appenddef_set(list: TAsmList; def: tsetdef);
      begin
        appenddef_array_internal(list,def,u8inttype,0,def.size-1);
      end;


    procedure TDebugInfoLLVM.appenddef_undefined(list: TAsmList; def: tundefineddef);
      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
        dinode.addenum('tag','DW_TAG_pointer_type');
        dinode.addmetadatarefto('baseType',nil);
        list.concat(dinode);
      end;


    procedure TDebugInfoLLVM.appenddef_classref(list: TAsmList; def: tclassrefdef);
      var
        dinode: tai_llvmspecialisedmetadatanode;
      begin
        dinode:=def_set_meta_impl(def,tspecialisedmetadatanodekind.DIDerivedType);
        dinode.addenum('tag','DW_TAG_pointer_type');
        dinode.addmetadatarefto('baseType',nil);
        list.concat(dinode);
      end;


    procedure TDebugInfoLLVM.appenddef_variant(list: TAsmList; def: tvariantdef);
      begin
        if assigned(vardatadef) then
          appenddef_record_named(list,def,trecorddef(vardatadef),'Variant');
      end;


    procedure TDebugInfoLLVM.afterappenddef(list:TAsmList;def:tdef);
      var
        tempdinode,
        refdinode,
        impldinode: tai_llvmspecialisedmetadatanode;
      begin
        if def.typ=procdef then
          exit;

        if is_void(def) then
          exit;

        refdinode:=def_meta_node(def);
        impldinode:=def_meta_impl(def);
        if not assigned(impldinode) then
          internalerror(2021120501);

        if is_objc_class_or_protocol(def) then
          begin
            { for Objective-C classes, the named typedef must refer to the
              struct itself, not to the pointer of the struct; Objective-C
              classes are not implicit pointers in Objective-C itself, only
              in FPC. So make the def label point to a pointer to the
              typedef, which in turn refers to the actual struct (for Delphi-
              style classes, the def points to the typedef, which refers to
              a pointer to the actual struct) }

            { implicit pointer }
            tempdinode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIDerivedType);
            refdinode.addenum('tag','DW_TAG_pointer_type');
            refdinode.addmetadatarefto('baseType',tempdinode);
            list.concat(refdinode);
            { typedef }
            refdinode:=tempdinode;
          end;

        if assigned(def.typesym) and
           not(df_generic in def.defoptions) then
          begin
            if refdinode=impldinode then
              internalerror(2022110710);
            refdinode.addenum('tag','DW_TAG_typedef');
            refdinode.addstring('name',symname(def.typesym,false));
            try_add_file_metaref(refdinode,def.typesym.fileinfo,false);
            refdinode.addmetadatarefto('baseType',impldinode);
            list.concat(refdinode);
          end
        else if impldinode<>refdinode then
          internalerror(2022110711);
      end;


    procedure TDebugInfoLLVM.appendprocdef(list:TAsmList; def:tprocdef);

      procedure adddispflags(dinode: tai_llvmspecialisedmetadatanode; is_definition, is_virtual: boolean);
        var
          dispflags: TSymStr;
          islocal: boolean;
        begin
          islocal:=
            not((po_global in def.procoptions) and
                (def.parast.symtablelevel<=normal_function_level));
          adddefinitionlocal(dinode,is_definition,islocal,not(llvmflag_NoDISPFlags in llvmversion_properties[current_settings.llvmversion]),dispflags);
          if llvmflag_NoDISPFlags in llvmversion_properties[current_settings.llvmversion] then
            begin
              if is_virtual then
                begin
                  if not(po_abstractmethod in def.procoptions) then
                    dinode.addenum('virtuality','DW_VIRTUALITY_virtual')
                  else
                    dinode.addenum('virtuality','DW_VIRTUALITY_pure_virtual');
                end;
              exit;
            end;

          if is_virtual then
            begin
              if dispflags<>'' then
                dispflags:=dispflags+'|';
              if not(po_abstractmethod in def.procoptions) then
                dispflags:=dispflags+'DISPFlagVirtual'
              else
                dispflags:=dispflags+'DISPFlagPureVirtual';
            end
          else
            begin
              { this one will always be a definition, so no need to check
                whether result is empty }
              if not(llvmflag_NoDISPFlagMainSubprogram in llvmversion_properties[current_settings.llvmversion]) and
                 (def.proctypeoption=potype_proginit) then
                dispflags:=dispflags+'|DISPFlagMainSubprogram';
            end;
          if dispflags<>'' then
            dinode.addenum('spFlags',dispflags);
        end;

      procedure adddiflags(dinode: tai_llvmspecialisedmetadatanode; is_definition: boolean);
        var
          diflags: TSymStr;
        begin
          if (llvmflag_NoDISPFlagMainSubprogram in llvmversion_properties[current_settings.llvmversion]) and
             (def.proctypeoption=potype_proginit) then
            diflags:='DIFlagMainSubprogram'
          else if def.owner.symtabletype in [objectsymtable,recordsymtable] then
            diflags:=visibilitydiflag(def.visibility)
          else
            diflags:='';
          if diflags<>'' then
            dinode.addenum('flags',diflags);
        end;

      var
        dinode,
        ditypenode     : tai_llvmspecialisedmetadatanode;
        fileref        : tai_simpletypedconst;
        procdeftai     : tai;
        st             : tsymtable;
        vmtoffset      : pint;
        flags      : TSymStr;
        in_currentunit,
        is_virtual     : boolean;

      begin
        { only write debug info for procedures defined in the current module,
          except in case of methods (clang-compatible)
        }
        in_currentunit:=def.in_currentunit;

        if not in_currentunit and
           not (def.owner.symtabletype in [objectsymtable,recordsymtable]) then
          exit;

        { happens for init procdef of units without init section }
        if in_currentunit and
           not assigned(def.procstarttai) then
          exit;

        { These don't contain a taillvmdecl, they are completely generated
          in native assembly. If we want to add debug information to these,
          we have to do it using the regular debug info generation }
        if po_assembler in def.procoptions then
          exit;

        if df_generic in def.defoptions then
          exit;

        { Procdefs are not handled by the regular def writing code, so
          dbg_state is not set/checked for them. Do it here.  }
        if (def.dbg_state in [dbg_state_writing,dbg_state_written]) then
          exit;
        defnumberlist.Add(def);

        def.dbg_state:=dbg_state_writing;
        { difference compared to other kinds of defs: the DISubProgram gets
          created directly in get_def_metatai because a typedef for a
          DISubProgram does not make sense and is not supported by LLVM ->
          don't set the implementation of the metadata def here and just use
          the regular node }
        dinode:=def_meta_node(def);
        list.concat(dinode);

        { we have to attach the debug info to the definition instruction of the
          proc }
        procdeftai:=nil;
        if in_currentunit then
          begin
            procdeftai:=def.procstarttai;
            if (procdeftai.typ<>ait_llvmdecl) or
               (taillvmdecl(procdeftai).def<>def) then
              internalerror(2022022010);
            taillvmdecl(procdeftai).addinsmetadata(tai_llvmmetadatareferenceoperand.createreferenceto('dbg',dinode));
          end;

        dinode.addstring('name',symdebugname(def.procsym));
        if assigned(def.struct) and
           not is_objc_class_or_protocol(def.struct) then
          begin
            if is_implicit_pointer_object_type(def.struct) then
              dinode.addmetadatarefto('scope',def_meta_class_struct(tobjectdef(def.struct)))
             else
               dinode.addmetadatarefto('scope',def_meta_node(def.struct));
            try_add_file_metaref(dinode,def.fileinfo,false);
          end
        else
          try_add_file_metaref(dinode,def.fileinfo,true);
        if not(cs_debuginfo in current_settings.moduleswitches) then
          begin
            def.dbg_state:=dbg_state_written;
            exit;
          end;

        is_virtual:=
          (([po_abstractmethod, po_virtualmethod, po_overridingmethod]*def.procoptions)<>[]) and
          not is_objc_class_or_protocol(def.struct) and
          not is_objectpascal_helper(def.struct);
        adddispflags(dinode,in_currentunit,is_virtual);
        if is_virtual then
          begin
            { the sizeof(pint) is a bit iffy, since vmtmethodoffset() calculates
              using a combination of voidcodepointer.size, voidpointer.size, and
              sizeof(pint). But that's what the debugger will use }
            dinode.addint64('virtualIndex',tobjectdef(def.owner.defowner).vmtmethodoffset(def.extnumber) div sizeof(pint));
{$ifdef extdebug}
            if (tobjectdef(def.owner.defowner).vmtmethodoffset(def.extnumber) mod sizeof(pint))<>0 then
              internalerror(2022043001);
{$endif}
          end;
        adddiflags(dinode,in_currentunit);

        dinode.addmetadatarefto('unit',fcunode);
        ditypenode:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DISubroutineType);
        ditypenode.addmetadatarefto('types',getabstractprocdeftypes(list,def));
        list.concat(ditypenode);
        dinode.addmetadatarefto('type',ditypenode);

(*
        if assigned(def.parast) then
          begin
            { First insert self, because gdb uses the fact whether or not the
              first parameter of a method is artificial to distinguish static
              from regular methods.  }

            { fortunately, self is the always the first parameter in the
              paralist, since it has the lowest paranr. Note that this is not
              true for Objective-C, but those methods are detected in
              another way (by reading the ObjC run time information)  }
            write_symtable_parasyms(current_asmdata.asmlists[al_dwarf_info],def.paras);
          end;
        { local type defs and vars should not be written
          inside the main proc }
        if in_currentunit and
           assigned(def.localst) and
           (def.localst.symtabletype=localsymtable) then
          write_symtable_syms(current_asmdata.asmlists[al_dwarf_info],def.localst);

        { last write the types from this procdef }
        if assigned(def.parast) then
          write_symtable_defs(current_asmdata.asmlists[al_dwarf_info],def.parast);
        { only try to write the localst if the routine is implemented here }
        if in_currentunit and
           assigned(def.localst) and
           (def.localst.symtabletype=localsymtable) then
          begin
            write_symtable_defs(current_asmdata.asmlists[al_dwarf_info],def.localst);
            { Write nested procedures -- disabled, see scope check at the
              beginning; currently, these are still written in the global
              scope.  }
            // write_symtable_procdefs(current_asmdata.asmlists[al_dwarf_info],def.localst);
          end;

        finish_children;
*)
        def.dbg_state:=dbg_state_written;
      end;

    procedure TDebugInfoLLVM.adddefinitionlocal(dinode: tai_llvmspecialisedmetadatanode; definition, local, usedispflags: boolean; out dispFlags: tsymstr);
      begin
        dispflags:='';
        if not usedispflags then
          begin
            dinode.addboolean('isDefinition',definition);
            if definition then
              begin
                dinode.addboolean('isLocal',local);
              end;
            exit;
          end;

        if definition then
          begin
            dispflags:='DISPFlagDefinition';
            if local then
              dispflags:=dispflags+'|DISPFlagLocalToUnit';
          end;
    end;


    function TDebugInfoLLVM.get_symlist_sym_offset(symlist: ppropaccesslistitem; out sym: tabstractvarsym; out offset: pint): boolean;
(*
      var
        elesize : pint;
        currdef : tdef;
        indirection: boolean;
*)
      begin
        result:=false;
(*
        if not assigned(symlist) then
          exit;
        sym:=nil;
        offset:=0;
        currdef:=nil;
        indirection:=false;
        repeat
          case symlist^.sltype of
            sl_load:
              begin
                if assigned(sym) then
                  internalerror(2009031203);
                if not(symlist^.sym.typ in [paravarsym,localvarsym,staticvarsym,fieldvarsym]) then
                  { can't handle... }
                  exit;
                sym:=tabstractvarsym(symlist^.sym);
                currdef:=tabstractvarsym(sym).vardef;
                if ((sym.typ=paravarsym) and
                    paramanager.push_addr_param(tparavarsym(sym).varspez,sym.vardef,tprocdef(sym.owner.defowner).proccalloption)) then
                  indirection:=true;
              end;
            sl_subscript:
              begin
                if not assigned(currdef) then
                  internalerror(2009031301);
                if (symlist^.sym.typ<>fieldvarsym) then
                  internalerror(2009031202);
                { can't handle offsets with indirections yet }
                if indirection then
                  exit;
                if is_packed_record_or_object(currdef) then
                  begin
                    { can't calculate the address of a non-byte aligned field }
                    if (tfieldvarsym(symlist^.sym).fieldoffset mod 8) <> 0 then
                      exit;
                    inc(offset,tfieldvarsym(symlist^.sym).fieldoffset div 8)
                  end
                else
                  inc(offset,tfieldvarsym(symlist^.sym).fieldoffset);
                currdef:=tfieldvarsym(symlist^.sym).vardef;
              end;
            sl_absolutetype,
            sl_typeconv:
              begin
                currdef:=symlist^.def;
                { ignore, these don't change the address }
              end;
            sl_vec:
              begin
                if not assigned(currdef) or
                   (currdef.typ<>arraydef) then
                  internalerror(2009031201);
                { can't handle offsets with indirections yet }
                if indirection then
                  exit;
                if not is_packed_array(currdef) then
                  elesize:=tarraydef(currdef).elesize
                else
                  begin
                    elesize:=tarraydef(currdef).elepackedbitsize;
                    { can't calculate the address of a non-byte aligned element }
                    if (elesize mod 8)<>0 then
                      exit;
                    elesize:=elesize div 8;
                  end;
                inc(offset,(symlist^.value.svalue-tarraydef(currdef).lowrange)*elesize);
                currdef:=tarraydef(currdef).elementdef;
              end;
            else
              internalerror(2009031403);
          end;
          symlist:=symlist^.next;
        until not assigned(symlist);
        if not assigned(sym) then
          internalerror(2009031205);
        result:=true;
*)
      end;


    procedure TDebugInfoLLVM.appendsym_var(list:TAsmList;sym:tabstractnormalvarsym);
      begin
//        appendsym_var_with_name_type_offset(list,sym,symname(sym, false),sym.vardef,0,[]);
      end;


    procedure TDebugInfoLLVM.appendsym_var_with_name_type_offset(list:TAsmList; sym:tabstractnormalvarsym; const name: TSymStr; def: tdef; offset: pint(*; const flags: tdwarfvarsymflags*));
(*
      var
        templist : TAsmList;
        blocksize,size_of_int : longint;
        tag : tdwarf_tag;
        has_high_reg : boolean;
        dreg,dreghigh : shortint;
{$ifdef i8086}
        has_segment_sym_name : boolean=false;
        segment_sym_name : TSymStr='';
        segment_reg: TRegister=NR_NO;
{$endif i8086}
*)
      begin
(*
        if vo_is_external in sym.varoptions then
          exit;
        blocksize:=0;
        dreghigh:=0;

        { There is no space allocated for not referenced locals }
        if (sym.owner.symtabletype=localsymtable) and (sym.refs=0) then
          exit;

        templist:=TAsmList.create;

        case sym.localloc.loc of
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER :
            begin
              { dwarf_reg_no_error might return -1
                in case the register variable has been optimized out }
              dreg:=dwarf_reg_no_error(sym.localloc.register);
              has_high_reg:=(sym.localloc.loc in [LOC_REGISTER,LOC_CREGISTER]) and (sym.localloc.registerhi<>NR_NO);
              if has_high_reg then
                dreghigh:=dwarf_reg_no_error(sym.localloc.registerhi);
              if dreghigh=-1 then
                has_high_reg:=false;
              if (sym.localloc.loc in [LOC_REGISTER,LOC_CREGISTER]) and
                 (sym.typ=paravarsym) and
                  paramanager.push_addr_param(sym.varspez,sym.vardef,tprocdef(sym.owner.defowner).proccalloption) and
                  not(vo_has_local_copy in sym.varoptions) and
                  not is_open_string(sym.vardef) and (dreg>=0) then
                begin
                  templist.concat(tai_const.create_8bit(ord(DW_OP_bregx)));
                  templist.concat(tai_const.create_uleb128bit(dreg));
                  templist.concat(tai_const.create_sleb128bit(0));
                  blocksize:=1+Lengthuleb128(dreg)+LengthSleb128(0);
                end
              else
                begin
                  if has_high_reg then
                    begin
                      templist.concat(tai_comment.create(strpnew('high:low reg pair variable')));
                      size_of_int:=sizeof(aint);
                      templist.concat(tai_const.create_8bit(ord(DW_OP_regx)));
                      templist.concat(tai_const.create_uleb128bit(dreg));
                      blocksize:=1+Lengthuleb128(dreg);
                      templist.concat(tai_const.create_8bit(ord(DW_OP_piece)));
                      templist.concat(tai_const.create_uleb128bit(size_of_int));
                      blocksize:=blocksize+1+Lengthuleb128(size_of_int);
                      templist.concat(tai_const.create_8bit(ord(DW_OP_regx)));
                      templist.concat(tai_const.create_uleb128bit(dreghigh));
                      blocksize:=blocksize+1+Lengthuleb128(dreghigh);
                      templist.concat(tai_const.create_8bit(ord(DW_OP_piece)));
                      templist.concat(tai_const.create_uleb128bit(size_of_int));
                      blocksize:=blocksize+1+Lengthuleb128(size_of_int);
                    end
                  else if (dreg>=0) then
                    begin
                      templist.concat(tai_const.create_8bit(ord(DW_OP_regx)));
                      templist.concat(tai_const.create_uleb128bit(dreg));
                      blocksize:=1+Lengthuleb128(dreg);
                    end;
                 end;
            end;
          else
            begin
              case sym.typ of
                staticvarsym:
                  begin
                    if vo_is_thread_var in sym.varoptions then
                      begin
                        if tf_section_threadvars in target_info.flags then
                          begin
                            case sizeof(puint) of
                              2:
                                templist.concat(tai_const.create_8bit(ord(DW_OP_const2u)));
                              4:
                                templist.concat(tai_const.create_8bit(ord(DW_OP_const4u)));
                              8:
                                templist.concat(tai_const.create_8bit(ord(DW_OP_const8u)));
                              else
                                Internalerror(2019100501);
                            end;
{$push}
{$warn 6018 off}            { Unreachable code due to compile time evaluation }
                            templist.concat(tai_const.Create_type_name(aitconst_dtpoff,sym.mangledname,0));
                            { so far, aitconst_dtpoff is solely 32 bit }
                            if (sizeof(puint)=8) and (target_info.endian=endian_little) then
                              templist.concat(tai_const.create_32bit(0));
                            templist.concat(tai_const.create_8bit(ord(DW_OP_GNU_push_tls_address)));
                            if (sizeof(puint)=8) and (target_info.endian=endian_big) then
                              templist.concat(tai_const.create_32bit(0));
{$pop}

                            blocksize:=2+sizeof(puint);
                          end
                        else
                          begin
                            { TODO: !!! FIXME: dwarf for thread vars !!!}
                            { This is only a minimal change to at least be able to get a value
                              in only one thread is present PM 2014-11-21, like for stabs format }
                            templist.concat(tai_const.create_8bit(ord(DW_OP_addr)));
                            templist.concat(tai_const.Create_type_name(aitconst_ptr_unaligned,sym.mangledname,
                              offset+sizeof(pint)));
                            blocksize:=1+sizeof(puint);
                          end;
                      end
                    else
                      begin
                        templist.concat(tai_const.create_8bit(ord(DW_OP_addr)));
                        templist.concat(tai_const.Create_type_name(aitconst_ptr_unaligned,sym.mangledname,offset));
                        blocksize:=1+sizeof(puint);
{$ifdef i8086}
                        segment_sym_name:=sym.mangledname;
                        has_segment_sym_name:=true;
{$endif i8086}
                      end;
                  end;
                paravarsym,
                localvarsym:
                  begin
                    { Happens when writing debug info for paras of procdefs not
                      implemented in the current module. Can't add a general check
                      for LOC_INVALID above, because staticvarsyms may also have it.
                    }
                    if sym.localloc.loc<> LOC_INVALID then
                      begin
                        if is_fbreg(sym.localloc.reference.base) then
                          begin
                            templist.concat(tai_const.create_8bit(ord(DW_OP_fbreg)));
                            templist.concat(tai_const.create_sleb128bit(sym.localloc.reference.offset+offset));
                            blocksize:=1+Lengthsleb128(sym.localloc.reference.offset+offset);
                          end
                        else
                          begin
                            dreg:=dwarf_reg(sym.localloc.reference.base);
                            if dreg<=31 then
                              begin
                                templist.concat(tai_const.create_8bit(ord(DW_OP_breg0)+dreg));
                                templist.concat(tai_const.create_sleb128bit(sym.localloc.reference.offset+offset));
                                blocksize:=1+Lengthsleb128(sym.localloc.reference.offset+offset);
                              end
                            else
                              begin
                                templist.concat(tai_const.create_8bit(ord(DW_OP_bregx)));
                                templist.concat(tai_const.create_uleb128bit(dreg));
                                templist.concat(tai_const.create_sleb128bit(sym.localloc.reference.offset+offset));
                                blocksize:=1+Lengthuleb128(dreg)+LengthSleb128(sym.localloc.reference.offset+offset);
                              end;
                          end;
{$ifdef i8086}
                        segment_reg:=sym.localloc.reference.segment;
{$endif i8086}
{$ifndef gdb_supports_DW_AT_variable_parameter}
                        { Parameters which are passed by reference. (var and the like)
                          Hide the reference-pointer and dereference the pointer
                          in the DW_AT_location block.
                        }
                        if (sym.typ=paravarsym) and
                            paramanager.push_addr_param(sym.varspez,sym.vardef,tprocdef(sym.owner.defowner).proccalloption) and
                            not(vo_has_local_copy in sym.varoptions) and
                            not is_open_string(sym.vardef) then
                          begin
                            templist.concat(tai_const.create_8bit(ord(DW_OP_deref)));
                            inc(blocksize);
                          end
{$endif not gdb_supports_DW_AT_variable_parameter}
                      end;
                  end
                else
                  internalerror(200601288);
              end;
            end;
        end;

        { function results must not be added to the parameter list,
          as they are not part of the signature of the function
          (gdb automatically adds them according to the ABI specifications
           when calling the function)
        }
        if (sym.typ=paravarsym) and
           not(dvf_force_local_var in flags) and
           not(vo_is_funcret in sym.varoptions) then
          tag:=DW_TAG_formal_parameter
        else
          tag:=DW_TAG_variable;

        { must be parasym of externally implemented procdef, but
          the parasymtable can con also contain e.g. absolutevarsyms
          -> check symtabletype}
        if (sym.owner.symtabletype=parasymtable) and
           (sym.localloc.loc=LOC_INVALID) then
          begin
            if (sym.owner.symtabletype<>parasymtable) then
              internalerror(2009101001);
            append_entry(tag,false,[
              DW_AT_name,DW_FORM_string,name+#0
              {
              DW_AT_decl_file,DW_FORM_data1,0,
              DW_AT_decl_line,DW_FORM_data1,
              }
              ])
          end
        else if not(sym.localloc.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_MMREGISTER,
                                 LOC_CMMREGISTER,LOC_FPUREGISTER,LOC_CFPUREGISTER]) and
           ((sym.owner.symtabletype = globalsymtable) or
            (sp_static in sym.symoptions) or
            (vo_is_public in sym.varoptions)) then
          append_entry(tag,false,[
            DW_AT_name,DW_FORM_string,name+#0,
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
            DW_AT_name,DW_FORM_string,name+#0,
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
            DW_AT_name,DW_FORM_string,name+#0,
            {
            DW_AT_decl_file,DW_FORM_data1,0,
            DW_AT_decl_line,DW_FORM_data1,
            }
            { data continues below }
            DW_AT_location,DW_FORM_block1,blocksize
            ]);
        { append block data }
        current_asmdata.asmlists[al_dwarf_info].concatlist(templist);
        { Mark self as artificial for methods, because gdb uses the fact
          whether or not the first parameter of a method is artificial to
          distinguish regular from static methods (since there are no
          no vo_is_self parameters for static methods, we don't have to check
          that).  }
        if (vo_is_self in sym.varoptions) then
          append_attribute(DW_AT_artificial,DW_FORM_flag,[true]);
        append_labelentry_ref(DW_AT_type,def_dwarf_lab(def));
{$ifdef i8086}
        if has_segment_sym_name then
          append_seg_name(segment_sym_name)
        else if segment_reg<>NR_NO then
          append_seg_reg(segment_reg);
{$endif i8086}

        templist.free;

        finish_entry;
*)
      end;


    procedure TDebugInfoLLVM.appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);
      var
        decl: taillvmdecl;
        globalvarexpression, globalvar: tai_llvmspecialisedmetadatanode;
        dispflags: tsymstr;
        islocal: boolean;
      begin
        decl:=staticvarsym_get_decl(sym);
        if not assigned(decl) then
          begin
            list.concat(tai_comment.create(strpnew('no declaration found for '+sym.mangledname)));
            exit;
          end;
        globalvar:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIGlobalVariable);
        list.concat(globalvar);

        globalvarexpression:=tai_llvmspecialisedmetadatanode.create(tspecialisedmetadatanodekind.DIGlobalVariableExpression);
        globalvarexpression.addmetadatarefto('var',globalvar);
        globalvarexpression.addmetadatarefto('expr',femptyexpression);
        list.concat(globalvarexpression);
        fglobals.addvalue(llvm_getmetadatareftypedconst(globalvarexpression));

        decl.addinsmetadata(tai_llvmmetadatareferenceoperand.createreferenceto('dbg',globalvarexpression));

        globalvar.addstring('name',symname(sym,false));
        if not assigned(sym.owner.defowner) then
          globalvar.addmetadatarefto('scope',fcunode)
        else
          globalvar.addmetadatarefto('scope',def_meta_node(tdef(sym.owner.defowner)));
        try_add_file_metaref(globalvar,sym.fileinfo,false);
        globalvar.addmetadatarefto('type',def_meta_node(sym.vardef));

        islocal:=not(
          ((sym.owner.symtabletype = globalsymtable) or
          (sp_static in sym.symoptions) or
          (vo_is_public in sym.varoptions))
        );

        adddefinitionlocal(globalvar,not(vo_is_external in sym.varoptions),islocal,false,dispflags);
        if dispflags<>'' then
          globalvar.addenum('spFlags',dispflags);
      end;


    procedure TDebugInfoLLVM.appendsym_localvar(list:TAsmList;sym:tlocalvarsym);
      begin
//        appendsym_var(list,sym);
      end;


    procedure TDebugInfoLLVM.appendsym_paravar(list:TAsmList;sym:tparavarsym);
      begin
//        appendsym_var(list,sym);
      end;


    procedure TDebugInfoLLVM.appendsym_fieldvar(list:TAsmList;sym: tfieldvarsym);
      begin
        appendsym_fieldvar_with_name_offset(list,sym,symname(sym, false),sym.vardef,0);
      end;


    procedure TDebugInfoLLVM.appendsym_fieldvar_with_name_offset(list:TAsmList;sym: tfieldvarsym;const name: string; def: tdef; offset: pint);
      var
        bitoffset,
        fieldoffset,
        fieldnatsize: asizeint;
      begin
(*
        if (sp_static in sym.symoptions) or
           (sym.visibility=vis_hidden) then
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
            inc(fieldoffset,offset);
            append_entry(DW_TAG_member,false,[
              DW_AT_name,DW_FORM_string,name+#0,
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
            inc(fieldoffset,offset);
            bitoffset:=sym.fieldoffset mod (fieldnatsize*8);
            if (target_info.endian=endian_little) then
              bitoffset:=(fieldnatsize*8)-bitoffset-sym.vardef.packedbitsize;
            append_entry(DW_TAG_member,false,[
              DW_AT_name,DW_FORM_string,symname(sym, false)+#0,
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
        if (sym.owner.symtabletype in [objectsymtable,recordsymtable]) then
          append_visibility(sym.visibility);

        append_labelentry_ref(DW_AT_type,def_dwarf_lab(def));
        finish_entry;
*)
      end;

    procedure TDebugInfoLLVM.appendsym_const(list:TAsmList;sym:tconstsym);
    begin
      appendsym_const_member(list,sym,false);
    end;

    procedure TDebugInfoLLVM.appendsym_const_member(list:TAsmList;sym:tconstsym;ismember:boolean);
      var
        i,
        size: aint;
        usedef: tdef;
      begin
(*
        { These are default values of parameters. These should be encoded
          via DW_AT_default_value, not as a separate sym. Moreover, their
          type is not available when writing the debug info for external
          procedures.
        }
        if (sym.owner.symtabletype=parasymtable) then
          exit;

        if ismember then
          append_entry(DW_TAG_member,false,[
            DW_AT_name,DW_FORM_string,symname(sym, false)+#0,
          { The DW_AT_declaration tag is invalid according to the DWARF specifications.
            But gcc adds this to static const members and gdb checks
            for this flag. So we have to set it also.
          }
            DW_AT_declaration,DW_FORM_flag,true,
            DW_AT_external,DW_FORM_flag,true
            ])
        else
          append_entry(DW_TAG_variable,false,[
            DW_AT_name,DW_FORM_string,symname(sym, false)+#0
            ]);
        { for string constants, constdef isn't set because they have no real type }
        case sym.consttyp of
          conststring:
            begin
              { if DW_FORM_string is used below one day, this usedef should
                probably become nil }
              { note: < 255 instead of <= 255 because we have to store the
                entire length of the string as well, and 256 does not fit in
                a byte }
              if (sym.value.len<255) then
                usedef:=cshortstringtype
              else
                usedef:=clongstringtype;
            end;
          constresourcestring,
          constwstring:
            usedef:=nil;
          else
            usedef:=sym.constdef;
          end;
        if assigned(usedef) then
          append_labelentry_ref(DW_AT_type,def_dwarf_lab(usedef));
        AddConstToAbbrev(ord(DW_AT_const_value));
        case sym.consttyp of
          conststring:
            begin
              { DW_FORM_string isn't supported yet by the Pascal value printer
                -> create a string using raw bytes }
              if (sym.value.len<255) then
                begin
                  AddConstToAbbrev(ord(DW_FORM_block1));
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(sym.value.len+1));
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(sym.value.len));
                end
              else
                begin
                  AddConstToAbbrev(ord(DW_FORM_block));
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_uleb128bit(sym.value.len+sizesinttype.size));
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.Create_sizeint_unaligned(sym.value.len));
                end;
              i:=0;
              size:=sym.value.len;
              while(i<size) do
                begin
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit((pbyte(sym.value.valueptr+i)^)));
                  inc(i);
                end;
            end;
          constguid,
          constset:
            begin
              AddConstToAbbrev(ord(DW_FORM_block1));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(usedef.size));
              i:=0;
              size:=sym.constdef.size;
              while (i<size) do
                begin
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit((pbyte(sym.value.valueptr+i)^)));
                  inc(i);
                end;
            end;
          constwstring,
          constresourcestring:
            begin
              { write dummy for now }
              AddConstToAbbrev(ord(DW_FORM_string));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_string.create(''));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(0));
            end;
          constord:
            begin
              if (sym.value.valueord<0) then
                begin
                  AddConstToAbbrev(ord(DW_FORM_sdata));
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_sleb128bit(sym.value.valueord.svalue));
                end
              else
                begin
                  AddConstToAbbrev(ord(DW_FORM_udata));
                  current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_uleb128bit(sym.value.valueord.uvalue));
                end;
            end;
          constnil:
            begin
{$ifdef cpu64bitaddr}
              AddConstToAbbrev(ord(DW_FORM_data8));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_64bit_unaligned(0));
{$else cpu64bitaddr}
              AddConstToAbbrev(ord(DW_FORM_data4));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_32bit_unaligned(0));
{$endif cpu64bitaddr}
            end;
          constpointer:
            begin
{$ifdef cpu64bitaddr}
              AddConstToAbbrev(ord(DW_FORM_data8));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_64bit_unaligned(int64(sym.value.valueordptr)));
{$else cpu64bitaddr}
              AddConstToAbbrev(ord(DW_FORM_data4));
              current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_32bit_unaligned(longint(sym.value.valueordptr)));
{$endif cpu64bitaddr}
            end;
          constreal:
            begin
              AddConstToAbbrev(ord(DW_FORM_block1));
              case tfloatdef(sym.constdef).floattype of
                s32real:
                  begin
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(4));
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_realconst.create_s32real(pbestreal(sym.value.valueptr)^));
                  end;
                s64real:
                  begin
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(8));
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_realconst.create_s64real(pbestreal(sym.value.valueptr)^));
                  end;
                s64comp,
                s64currency:
                  begin
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(8));
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_64bit_unaligned(trunc(pbestreal(sym.value.valueptr)^)));
                  end;
                s80real,
                sc80real:
                  begin
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_const.create_8bit(sym.constdef.size));
                    current_asmdata.asmlists[al_dwarf_info].concat(tai_realconst.create_s80real(pextended(sym.value.valueptr)^,sym.constdef.size));
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


    procedure TDebugInfoLLVM.appendsym_label(list:TAsmList;sym: tlabelsym);
      begin
        { ignore label syms for now, the problem is that a label sym
          can have more than one label associated e.g. in case of
          an inline procedure expansion }
      end;


    procedure TDebugInfoLLVM.appendsym_property(list:TAsmList;sym: tpropertysym);
      var
        symlist: ppropaccesslistitem;
        tosym: tabstractvarsym;
        offset: pint;
      begin
(*
        if assigned(sym.propaccesslist[palt_read]) and
           not assigned(sym.propaccesslist[palt_read].procdef) then
          symlist:=sym.propaccesslist[palt_read].firstsym
        else
          { can't handle }
          exit;

        if not get_symlist_sym_offset(symlist,tosym,offset) then
          exit;

        if not (tosym.owner.symtabletype in [objectsymtable,recordsymtable]) then
          begin
            if (tosym.typ=fieldvarsym) then
              internalerror(2009031404);
            appendsym_var_with_name_type_offset(list,tabstractnormalvarsym(tosym),symname(sym, false),sym.propdef,offset,[])
          end
        else
          appendsym_fieldvar_with_name_offset(list,tfieldvarsym(tosym),symname(sym, false),sym.propdef,offset)
*)
      end;


    function TDebugInfoLLVM.symdebugname(sym: tsym): TSymStr;
    begin
      if ds_dwarf_cpp in current_settings.debugswitches then
        begin
          if sym.visibility=vis_hidden then
            result:=copy(sym.RealName,length('$hidden')+1,length(sym.RealName))
          else
            begin
              result:=sym.RealName;
              if (result<>'') and
                 (result[1]='$') then
                delete(result,1,1);
            end
        end
      else if sym.visibility=vis_hidden then
        result:=copy(sym.name,length('hidden')+1,length(sym.name))
      else
        result:=sym.name
    end;


    procedure TDebugInfoLLVM.appendsym_type(list:TAsmList;sym: ttypesym);
      begin
        { just queue the def if needed, beforeappenddef will
          emit the typedef if necessary }
        get_def_metatai(sym.typedef);
        {
        if FindUnitSymtable(sym.Owner).iscurrentunit then
          fretainedtypes.addvalue(def_meta_ref(sym.typedef));
        }
      end;


    procedure TDebugInfoLLVM.appendsym_absolute(list:TAsmList;sym:tabsolutevarsym);
(*
      var
        templist : TAsmList;
        blocksize : longint;
        symlist : ppropaccesslistitem;
        tosym: tabstractvarsym;
        offset: pint;
        flags: tdwarfvarsymflags;
*)
      begin
(*
        templist:=TAsmList.create;
        case tabsolutevarsym(sym).abstyp of
          toaddr :
            begin
               { MWE: replaced ifdef i368 }
               {
               if target_cpu = cpu_i386 then
                 begin
                  { in theory, we could write a DW_AT_segment entry here for sym.absseg,
                    however I doubt that gdb supports this (FK) }
                 end;
               }
               templist.concat(tai_const.create_8bit(3));
               {$ifdef avr}
               // Add $800000 to indicate that the address is in memory space
               templist.concat(tai_const.create_int_dataptr_unaligned(sym.addroffset + $800000, aitconst_ptr_unaligned));
               {$else}
               templist.concat(tai_const.create_int_dataptr_unaligned(sym.addroffset));
               {$endif}
               blocksize:=1+sizeof(puint);
            end;
          toasm :
            begin
              templist.concat(tai_const.create_8bit(3));
              templist.concat(tai_const.create_type_name(aitconst_ptr_unaligned,sym.mangledname,0));
              blocksize:=1+sizeof(puint);
            end;
          tovar:
            begin
              symlist:=tabsolutevarsym(sym).ref.firstsym;
              if get_symlist_sym_offset(symlist,tosym,offset) then
                begin
                  if (tosym.typ=fieldvarsym) then
                    internalerror(2009031402);
                  flags:=[];
                  if (sym.owner.symtabletype=localsymtable) then
                    include(flags,dvf_force_local_var);
                  appendsym_var_with_name_type_offset(list,tabstractnormalvarsym(tosym),symname(sym, false),tabstractvarsym(sym).vardef,offset,flags);
                end;
              templist.free;
              exit;
            end;
        end;

        append_entry(DW_TAG_variable,false,[
          DW_AT_name,DW_FORM_string,symname(sym, false)+#0,
          {
          DW_AT_decl_file,DW_FORM_data1,0,
          DW_AT_decl_line,DW_FORM_data1,
          }
          DW_AT_external,DW_FORM_flag,true,
          { data continues below }
          DW_AT_location,DW_FORM_block1,blocksize
          ]);
        { append block data }
        current_asmdata.asmlists[al_dwarf_info].concatlist(templist);
        append_labelentry_ref(DW_AT_type,def_dwarf_lab(sym.vardef));

        templist.free;

        finish_entry;
*)
      end;


    procedure TDebugInfoLLVM.beforeappendsym(list:TAsmList;sym:tsym);
      begin
        current_asmdata.asmlists[al_dwarf_info].concat(tai_comment.Create(strpnew('Symbol '+symname(sym, true))));
      end;


    procedure TDebugInfoLLVM.insertmoduleinfo;
      var
        culist: tai_llvmnamedmetadatanode;
        dwarfversionflag: tai_llvmbasemetadatanode;
        objcruntimeversion: longint;
      begin
        ensuremetainit;

        { debug info header }
        if ds_dwarf_cpp in current_settings.debugswitches then
          fcunode.addenum('language','DW_LANG_C_plus_plus')
        else
          fcunode.addenum('language','DW_LANG_Pascal83');
        fcunode.addmetadatarefto('file',file_getmetanode(current_filepos.moduleindex,current_filepos.fileindex));
        fcunode.addstring('producer','Free Pascal Compiler '+full_version_string);
        fcunode.addboolean('isOptimized',cs_opt_level2 in current_settings.optimizerswitches);
        if target_info.system in systems_objc_supported then
          begin
            if ([m_objectivec1,m_objectivec2]*current_settings.modeswitches)<>[] then
              if target_info.system in systems_objc_nfabi then
                objcruntimeversion:=2
              else
                objcruntimeversion:=1
            else
              objcruntimeversion:=0;
            fcunode.addint64('runtimeVersion',objcruntimeversion);
          end;
        if cs_debuginfo in current_settings.moduleswitches then
          fcunode.addenum('emissionKind','FullDebug')
        else
          fcunode.addenum('emissionKind','LineTablesOnly');
        if fenums.valuecount<>0 then
          begin
            fcunode.addmetadatarefto('enums',fenums);
            current_asmdata.AsmLists[al_dwarf_info].Concat(fenums);
          end
        else
          begin
            fcunode.addmetadatarefto('enums',nil);
            fenums.free;
          end;
        fenums:=nil;
        if fretainedtypes.valuecount<>0 then
          begin
            fcunode.addmetadatarefto('retainedTypes',fretainedtypes);
            current_asmdata.AsmLists[al_dwarf_info].Concat(fretainedtypes);
          end
        else
          begin
            fcunode.addmetadatarefto('retainedTypes',nil);
            fretainedtypes.free;
          end;
        fretainedtypes:=nil;
        if fglobals.valuecount<>0 then
          begin
            fcunode.addmetadatarefto('globals',fglobals);
            current_asmdata.AsmLists[al_dwarf_info].Concat(fglobals);
          end
        else
          begin
            fcunode.addmetadatarefto('globals',nil);
            fglobals.free;
          end;
        fglobals:=nil;
        current_asmdata.AsmLists[al_dwarf_info].Concat(femptyexpression);
        femptyexpression:=nil;
        current_asmdata.AsmLists[al_dwarf_info].Concat(fderefexpression);
        fderefexpression:=nil;

        if target_info.system in systems_darwin then
          fcunode.addenum('nameTableKind','GNU');
        current_asmdata.AsmLists[al_dwarf_info].Concat(fcunode);
        culist:=tai_llvmnamedmetadatanode.create('llvm.dbg.cu');
        current_asmdata.AsmLists[al_dwarf_info].Concat(culist);
        culist.addvalue(llvm_getmetadatareftypedconst(fcunode));

        resetfornewmodule;
      end;


    procedure TDebugInfoLLVM.inserttypeinfo;
      var
        storefilepos  : tfileposinfo;
        i : longint;
        def: tdef;
        vardatatype: ttypesym;
      begin
        ensuremetainit;
        storefilepos:=current_filepos;
        current_filepos:=current_module.mainfilepos;

        vardatatype:=try_search_system_type('TVARDATA');
        if assigned(vardatatype) then
          vardatadef:=trecorddef(vardatatype.typedef);

        collectglobalsyms;

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
        write_remaining_defs_to_write(current_asmdata.asmlists[al_dwarf_info]);

        { reset all def debug states for LLVMTypeInfo (which also uses this
          field, to track for which types type info has been inserted already }
        for i:=0 to defnumberlist.count-1 do
          begin
            def := tdef(defnumberlist[i]);
            if assigned(def) then
              def.dbg_state:=dbg_state_unused;
          end;

        current_filepos:=storefilepos;
      end;


    function TDebugInfoLLVM.symname(sym: tsym; manglename: boolean): TSymStr;
      begin
        if (sym.typ=paravarsym) and
           (vo_is_self in tparavarsym(sym).varoptions) then
          { We use 'this' for regular methods because that's what gdb triggers
            on to automatically search fields. Don't do this for class methods,
            because search class fields is not supported, and gdb 7.0+ fails
            in this case because "this" is not a record in that case (it's a
            pointer to a vmt) }
          if not is_objc_class_or_protocol(tdef(sym.owner.defowner.owner.defowner)) and
             not(po_classmethod in tabstractprocdef(sym.owner.defowner).procoptions) then
            result:='this'
          else
            result:='self'
        else if (sym.typ=typesym) and
                is_objc_class_or_protocol(ttypesym(sym).typedef) then
          result:=tobjectdef(ttypesym(sym).typedef).objextname^
        else if (ds_dwarf_method_class_prefix in current_settings.debugswitches) and
                (sym.typ=procsym) and
                (tprocsym(sym).owner.symtabletype in [objectsymtable,recordsymtable]) then
          begin
            result:=tprocsym(sym).owner.name^+'__';
            if manglename then
              result := result + sym.name
            else
              result := result + symdebugname(sym);
          end
        else
          begin
            if manglename then
              result := sym.name
            else
              result := symdebugname(sym);
          end;
      end;


    function TDebugInfoLLVM.visibilitydiflag(vis: tvisibility): TSymStr;
      begin
        case vis of
          vis_hidden,
          vis_private,
          vis_strictprivate:
            result:='DIFlagPrivate';
          vis_protected,
          vis_strictprotected:
            result:='DIFlagProtected';
          vis_published,
          vis_public:
            result:='DIFlagPublic';
          vis_none:
            internalerror(2022050101);
        end;
      end;


    procedure TDebugInfoLLVM.insertlineinfo(list:TAsmList);
      var
        hp: tai;
        functionscope,
        positionmeta: tai_llvmspecialisedmetadatanode;
        pd: tprocdef;
        procdeffileinfo: tfileposinfo;
        nolineinfolevel : longint;
        firstline: boolean;
      begin
        ensuremetainit;
        hp:=tai(list.first);
        while assigned(hp) and
              ((hp.typ<>ait_llvmdecl) or
               (taillvmdecl(hp).def.typ<>procdef)) do
           begin
             hp:=tai(hp.next);
           end;
        if not assigned(hp) then
          exit;
        pd:=tprocdef(taillvmdecl(hp).def);
        procdeffileinfo:=pd.fileinfo;
        { might trigger for certain kinds of internally generated code }
        if procdeffileinfo.fileindex=0 then
          exit;

        flocalvarsymmeta.free;
        flocalvarsymmeta:=THashSet.Create((pd.localst.SymList.count+pd.parast.SymList.count)*4+1,true,false);

        functionscope:=def_meta_node(pd);

        nolineinfolevel:=0;
        hp:=tai(hp.next);
        firstline:=true;
        while assigned(hp) do
          begin
            case hp.typ of
              ait_marker:
                begin
                  case tai_marker(hp).kind of
                    mark_NoLineInfoStart:
                      inc(nolineinfolevel);
                    mark_NoLineInfoEnd:
                      dec(nolineinfolevel);
                    else
                      ;
                  end;
                end;
              else
                ;
            end;

            if (hp.typ=ait_llvmins) and
               ((nolineinfolevel=0) or
                (taillvm(hp).llvmopcode in [la_call,la_invoke])) then
              begin
                positionmeta:=nil;
                { valid file -> add info }
                if (tailineinfo(hp).fileinfo.fileindex<>0) then
                  begin
                    if firstline and
                       (nolineinfolevel=0) then
                      begin
                        functionscope.addint64('scopeLine',tailineinfo(hp).fileinfo.line);
                        firstline:=false;
                      end;

                    positionmeta:=filepos_getmetanode(tailineinfo(hp).fileinfo,procdeffileinfo,functionscope,nolineinfolevel<>0);
                  end;

                { LLVM requires line info for call instructions that may
                  potentially be inlined }
                if (taillvm(hp).llvmopcode in [la_call,la_invoke]) and
                   not assigned(positionmeta) then
                  begin
                    positionmeta:=filepos_getmetanode(procdeffileinfo,procdeffileinfo,functionscope,true);
                  end;

                if assigned(positionmeta) then
                  taillvm(hp).addinsmetadata(tai_llvmmetadatareferenceoperand.createreferenceto('dbg',positionmeta));

                if (cs_debuginfo in current_settings.moduleswitches) and
                   (taillvm(hp).llvmopcode = la_call) then
                  updatelocalvardbginfo(taillvm(hp),pd,functionscope);
              end;
            hp:=tai(hp.next);
          end;
      end;


{****************************************************************************
****************************************************************************}
    const
      dbg_llvm_info : tdbginfo =
         (
           id     : dbg_llvm;
           idtxt  : 'LLVM';
         );


initialization
  RegisterDebugInfo(dbg_llvm_info,TDebugInfoLLVM);

end.

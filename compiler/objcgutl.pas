{
    Copyright (c) 2009 by Jonas Maebe

    This unit implements some Objective-C helper routines at the code generator
    level.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License,or
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

{$i fpcdefs.inc}

unit objcgutl;

interface

  uses
    cclasses,
    aasmbase,aasmdata,
    symbase,symdef;

  procedure objcfinishstringrefpoolentry(entry: phashsetitem; stringpool: tconstpooltype; refsec, stringsec: tasmsectiontype);
  procedure objcfinishclassrefnfpoolentry(entry: phashsetitem; classdef: tobjectdef);

  procedure MaybeGenerateObjectiveCImageInfo(globalst, localst: tsymtable);


implementation

  uses
    globtype,globals,fmodule,cutils,
    systems,
    aasmtai,
    cgbase,
    objcdef,objcutil,
    aasmcnst,
    symconst,symtype,symsym,symtable,
    verbose;

  type
    tobjcabi = (oa_fragile, oa_nonfragile);
(*    tivarlayouttype = (il_weak,il_strong); *)

    tobjcrttiwriter = class
     protected
      fabi: tobjcabi;
      classdefs,
      catdefs: tfpobjectlist;
      classrttidefs,
      catrttidefs: tfpobjectlist;
      classsyms,
      catsyms: tfpobjectlist;
      procedure gen_objc_methods(list: tasmlist; objccls: tobjectdef; out methodslabel: tasmsymbol; classmethods, iscategory: Boolean);
      procedure gen_objc_protocol_elements(list: tasmlist; protocol: tobjectdef; out reqinstsym, optinstsym, reqclssym, optclssym: TAsmLabel);
      procedure gen_objc_protocol_list(list:TAsmList; protolist: TFPObjectList; out protolistsym: TAsmLabel);
      procedure gen_objc_cat_methods(list:TAsmList; items: TFPObjectList; section: tasmsectiontype;const sectname: string; out listsym: TAsmLabel);

      procedure gen_objc_protocol(list:TAsmList; protocol: tobjectdef; out protocollabel: TAsmSymbol);virtual;abstract;
      procedure gen_objc_category_sections(list:TAsmList; objccat: tobjectdef; out catlabel: TAsmSymbol; out catlabeldef: tdef);virtual;abstract;
      procedure gen_objc_classes_sections(list:TAsmList; objclss: tobjectdef; out classlabel: TAsmSymbol; out classlabeldef: tdef);virtual;abstract;
      procedure gen_objc_info_sections(list: tasmlist);virtual;abstract;
     public
      constructor create(_abi: tobjcabi);
      destructor destroy;override;
      procedure gen_objc_rtti_sections(list:TAsmList; st:TSymtable);
      property abi: tobjcabi read fabi;
    end;


    { Used by by PowerPC/32 and i386 }
    tobjcrttiwriter_fragile = class(tobjcrttiwriter)
     protected
      function gen_objc_protocol_ext(list: TAsmList; optinstsym, optclssym: TAsmLabel): TAsmLabel;
      procedure gen_objc_ivars(list: TAsmList; objccls: tobjectdef; out ivarslabel: TAsmLabel);
      procedure gen_objc_protocol(list:TAsmList; protocol: tobjectdef; out protocollabel: TAsmSymbol);override;
      procedure gen_objc_category_sections(list:TAsmList; objccat: tobjectdef; out catlabel: TAsmSymbol; out catlabeldef: tdef);override;
      procedure gen_objc_classes_sections(list:TAsmList; objclss: tobjectdef; out classlabel: TAsmSymbol; out classlabeldef: tdef);override;
      procedure gen_objc_info_sections(list: tasmlist);override;
     public
      constructor create;
    end;


    { Used by PowerPC/64, ARM, x86_64 and AArch64 }
    tobjcrttiwriter_nonfragile = class(tobjcrttiwriter)
     protected
      ObjCEmptyCacheVar,
      ObjCEmptyVtableVar: TAsmSymbol;

      procedure gen_objc_class_ro_part(list: TAsmList; objclss: tobjectdef; protolistsym: TAsmSymbol; out classrolabel: TAsmSymbol; metaclass: boolean);
      procedure addclasslist(list: tasmlist; section: tasmsectiontype; const symname: string; classes: tfpobjectlist);

      procedure gen_objc_ivars(list: TAsmList; objccls: tobjectdef; out ivarslabel: TAsmLabel);
      procedure gen_objc_protocol(list:TAsmList; protocol: tobjectdef; out protocollabel: TAsmSymbol);override;
      procedure gen_objc_category_sections(list:TAsmList; objccat: tobjectdef; out catlabel: TAsmSymbol; out catlabeldef: tdef);override;
      procedure gen_objc_classes_sections(list:TAsmList; objclss: tobjectdef; out classlabel: TAsmSymbol; out classlabeldef: tdef);override;
      procedure gen_objc_info_sections(list: tasmlist);override;
     public
      constructor create;
    end;



{******************************************************************
                    Protocol declaration helpers
*******************************************************************}

function objcfindprotocolentry(const p: shortstring): TAsmSymbol;
  var
    item  : PHashSetItem;
  begin
    result:=nil;
    if not assigned(current_asmdata.ConstPools[sp_objcprotocolrefs]) then
      exit;
    item:=current_asmdata.constpools[sp_objcprotocolrefs].Find(@p[1], length(p));
    if not assigned(item) then
      exit;
    result:=TAsmSymbol(item^.Data);
  end;


function objcaddprotocolentry(const p: shortstring; ref: TAsmSymbol): Boolean;
  var
    item  : PHashSetItem;
  begin
    item:=current_asmdata.constpools[sp_objcprotocolrefs].FindOrAdd(@p[1], length(p));
    Result:=(item^.Data=nil);
    if Result then
      item^.Data:=ref;
  end;

{******************************************************************
                       Pool section helpers
*******************************************************************}

procedure objcreatestringpoolentryintern(p: pchar; len: longint; pooltype: tconstpooltype; stringsec: tasmsectiontype; out sym: TAsmLabel; out def: tdef);
  var
    entry  : PHashSetItem;
    strlab : tasmlabel;
    pc     : pchar;
    pool   : THashSet;
    tcb    : ttai_typedconstbuilder;
  begin
    pool := current_asmdata.constpools[pooltype];

    entry:=pool.FindOrAdd(p,len);
    if not assigned(entry^.data) then
      begin
        { create new entry }
        current_asmdata.getlabel(strlab,alt_data);
        entry^.Data:=strlab;
        getmem(pc,entry^.keylength+1);
        move(entry^.key^,pc^,entry^.keylength);
        pc[entry^.keylength]:=#0;

        { add the string to the approriate section }
        tcb:=ctai_typedconstbuilder.create([tcalo_is_lab,tcalo_new_section]);
        def:=tcb.emit_pchar_const(pc,entry^.keylength,false);
        current_asmdata.asmlists[al_objc_pools].concatList(
          tcb.get_final_asmlist(strlab,def,stringsec,strlab.name,1)
        );
        tcb.free;
        def:=cpointerdef.getreusable(def);
      end
    else
      def:=cpointerdef.getreusable(carraydef.getreusable(cansichartype,len+1));
    sym:=TAsmLabel(Entry^.Data);
  end;


procedure objcfinishstringrefpoolentry(entry: phashsetitem; stringpool: tconstpooltype; refsec, stringsec: tasmsectiontype);
  var
    reflab,
    strlab : tasmlabel;
    classname: string;
    tcb: ttai_typedconstbuilder;
    strdef: tdef;
  begin
    { have we already generated a reference for this string entry? }
    if not assigned(entry^.Data) then
      begin
        { no, add the string to the associated strings section }
        objcreatestringpoolentryintern(pchar(entry^.key),entry^.keylength,stringpool,stringsec,strlab,strdef);

        { and now finish the reference }
        current_asmdata.getlabel(reflab,alt_data);
        entry^.Data:=reflab;

        { add a pointer to the string in the string references section }
        tcb:=ctai_typedconstbuilder.create([tcalo_is_lab,tcalo_new_section,tcalo_no_dead_strip]);

        tcb.emit_tai(Tai_const.Create_sym(strlab),strdef);
        current_asmdata.asmlists[al_objc_pools].concatList(
          tcb.get_final_asmlist(reflab,strdef,refsec,reflab.name,sizeof(pint))
          );
        tcb.free;

        { in case of a class reference, also add a lazy symbol reference for
          the class (the linker requires this for the fragile ABI). }
        if (refsec=sec_objc_cls_refs) and
           not(target_info.system in systems_objc_nfabi) then
          begin
            classname:='';
            setlength(classname,entry^.keylength);
            move(entry^.key^,classname[1],entry^.keylength);
            { no way to express this in LLVM either, they also just emit
              module level assembly for it }
            current_asmdata.asmlists[al_pure_assembler].concat(tai_directive.Create(asd_lazy_reference,'.objc_class_name_'+classname));
          end;
      end;
  end;


procedure objcreatestringpoolentry(const s: string; pooltype: tconstpooltype; stringsec: tasmsectiontype; out sym: TAsmLabel; out def: tdef);
  begin
    objcreatestringpoolentryintern(@s[1],length(s),pooltype,stringsec,sym,def);
  end;


procedure objcfinishclassrefnfpoolentry(entry: phashsetitem; classdef: tobjectdef);
  var
    reflab: TAsmLabel;
    classym: TasmSymbol;
    tcb: ttai_typedconstbuilder;
  begin
    { have we already generated a reference for this class ref entry? }
    if not assigned(entry^.Data) then
      begin
        { no, add the classref to the sec_objc_cls_refs section }
        current_asmdata.getlabel(reflab,alt_data);
        entry^.Data:=reflab;

        { add a pointer to the class }
        classym:=current_asmdata.RefAsmSymbol(classdef.rtti_mangledname(objcclassrtti),AT_DATA);

        tcb:=ctai_typedconstbuilder.create([tcalo_is_lab,tcalo_new_section]);
        tcb.emit_tai(Tai_const.Create_sym(classym),voidpointertype);
        current_asmdata.asmlists[al_objc_pools].concatList(
          tcb.get_final_asmlist(reflab,voidpointertype,sec_objc_cls_refs,reflab.name,sizeof(pint))
        );
        tcb.free;
      end;
  end;

{******************************************************************
                    RTTI generation -- Helpers
*******************************************************************}

procedure ConcatSymOrNil(tcb: ttai_typedconstbuilder; sym: TAsmSymbol; def: tdef); inline;
begin
  if Assigned(sym) then
    tcb.emit_tai(tai_const.Create_sym(sym),def)
  else
    tcb.emit_tai(tai_const.Create_nil_dataptr,def);
end;


{******************************************************************
                 RTTI generation -- Common
*******************************************************************}

{ generate a method list, either of class methods or of instance methods,
  and both for obj-c classes and categories. }
procedure tobjcrttiwriter.gen_objc_methods(list: tasmlist; objccls: tobjectdef; out methodslabel: tasmsymbol; classmethods, iscategory: Boolean);
  const
                     {clas/cat inst/cls}
    SectType : array [Boolean, Boolean] of tasmsectiontype =
      ((sec_objc_inst_meth, sec_objc_cls_meth),
       (sec_objc_cat_inst_meth, sec_objc_cat_cls_meth));
                     {clas/cat inst/cls}
    SectName : array [Boolean, Boolean] of string[20] =
      (('_OBJC_INST_METH','_OBJC_CLS_METH'),
       ('_OBJC_CAT_INST_METH','_OBJC_CAT_CLS_METH'));
                   {frag/non-frag ABI}
    SectFlags : array [Boolean] of ttcasmlistoptions =
     ([tcalo_new_section],[tcalo_new_section,tcalo_no_dead_strip]);
                        {inst/cls}
    instclsName : array [Boolean] of string = ('INSTANCE','CLASS');
  type
    method_data = record
      def     : tprocdef;
      selsym  : TAsmLabel;
      seldef  : tdef;
      encsym  : TAsmLabel;
      encdef  : tdef;
    end;
  var
    i     : Integer;
    def   : tprocdef;
    defs  : array of method_data;
    mcnt  : integer;
    mtype : tdef;
    tcb   : ttai_typedconstbuilder;
    mdef  : tdef;
  begin
    methodslabel:=nil;
    mcnt:=0;
    defs:=nil;
    { collect all instance/class methods }
    SetLength(defs,objccls.vmtentries.count);
    for i:=0 to objccls.vmtentries.count-1 do
      begin
        def:=pvmtentry(objccls.vmtentries[i])^.procdef;
        if (def.owner.defowner=objccls) and
           (classmethods = (po_classmethod in def.procoptions)) then
          begin
            defs[mcnt].def:=def;
            objcreatestringpoolentry(def.messageinf.str^,sp_objcvarnames,sec_objc_meth_var_names,defs[mcnt].selsym,defs[mcnt].seldef);
            objcreatestringpoolentry(objcencodemethod(def),sp_objcvartypes,sec_objc_meth_var_types,defs[mcnt].encsym,defs[mcnt].encdef);
            inc(mcnt);
          end;
      end;
    if mcnt=0 then
      exit;

    tcb:=ctai_typedconstbuilder.create(SectFlags[target_info.system in systems_objc_nfabi]);
    tcb.begin_anonymous_record(internaltypeprefixName[itp_objc_method_list]+tostr(mcnt),
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    if (abi=oa_fragile) then
      { not used, always zero }
      tcb.emit_ord_const(0,u32inttype)
    else
      begin
        { size of each entry -- always 32 bit value }
        mtype:=search_named_unit_globaltype('OBJC','OBJC_METHOD',true).typedef;
        tcb.emit_ord_const(mtype.size,u32inttype);
      end;
    { number of objc_method entries in the method_list array -- always 32 bit}
    tcb.emit_ord_const(mcnt,u32inttype);
    for i:=0 to mcnt-1 do
      begin
        { reference to the selector name }
        tcb.queue_init(charpointertype);
        tcb.queue_emit_asmsym(defs[i].selsym,defs[i].seldef);
        { reference to the obj-c encoded function parameters (signature) }
        tcb.queue_init(charpointertype);
        tcb.queue_emit_asmsym(defs[i].encsym,defs[i].encdef);
        { mangled name of the method }
        tcb.queue_init(voidcodepointertype);
        tcb.queue_emit_proc(defs[i].def);
      end;

    mdef:=tcb.end_anonymous_record;
    if iscategory then
      begin
        methodslabel:=current_asmdata.DefineAsmSymbol('l_OBJC_$_CATEGORY_'+instclsName[classmethods]+'_METHODS_'+objccls.objextname^+'_$_'+objccls.childof.objextname^,AB_LOCAL,AT_DATA,mdef);
      end
    else
      begin
        methodslabel:=current_asmdata.DefineAsmSymbol('l_OBJC_$_'+instclsName[classmethods]+'_METHODS_'+objccls.objextname^,AB_LOCAL,AT_DATA,mdef);
      end;
    list.concatList(
      tcb.get_final_asmlist(methodslabel,mdef,
        SectType[iscategory,classmethods],
        SectName[iscategory,classmethods],sizeof(ptrint)
      )
    );
    tcb.free;
  end;


{ generate method (and in the future also property) info for protocols }
procedure tobjcrttiwriter.gen_objc_protocol_elements(list: tasmlist; protocol: tobjectdef; out reqinstsym, optinstsym, reqclssym, optclssym: TAsmLabel);
  var
    proc          : tprocdef;
    reqinstmlist,
    reqclsmlist,
    optinstmlist,
    optclsmlist   : TFPObjectList;
    i             : ptrint;
  begin
    reqinstmlist:=TFPObjectList.Create(false);
    reqclsmlist:=TFPObjectList.Create(false);
    optinstmlist:=TFPObjectList.Create(false);
    optclsmlist:=TFPObjectList.Create(false);
    for i:=0 to protocol.vmtentries.Count-1 do
      begin
        proc:=pvmtentry(protocol.vmtentries[i])^.procdef;
        if (po_classmethod in proc.procoptions) then
          if not(po_optional in proc.procoptions) then
            reqclsmlist.Add(proc)
          else
            optclsmlist.Add(proc)
        else if not(po_optional in proc.procoptions) then
          reqinstmlist.Add(proc)
        else
          optinstmlist.Add(proc);
      end;
    if reqinstmlist.Count > 0 then
      gen_objc_cat_methods(list,reqinstmlist,sec_objc_cat_inst_meth,'_OBJC_CAT_INST_METH',reqinstsym)
    else
      reqinstsym:=nil;
    if optinstmlist.Count > 0 then
      gen_objc_cat_methods(list,optinstmlist,sec_objc_cat_inst_meth,'_OBJC_CAT_INST_METH',optinstsym)
    else
      optinstsym:=nil;

    if reqclsmlist.Count>0 then
      gen_objc_cat_methods(list,reqclsmlist,sec_objc_cat_cls_meth,'_OBJC_CAT_CLS_METH',reqclssym)
    else
      reqclssym:=nil;
    if optclsmlist.Count>0 then
      gen_objc_cat_methods(list,optclsmlist,sec_objc_cat_cls_meth,'_OBJC_CAT_CLS_METH',optclssym)
    else
      optclssym:=nil;

    reqinstmlist.Free;
    reqclsmlist.Free;
    optinstmlist.Free;
    optclsmlist.Free;
end;


(*
From CLang:

  struct objc_protocol_list
  {
#ifdef FRAGILE_ABI
      struct objc_protocol_list *next;
      int count;
#else
      long count;
#endif
      Protocol *list[1];
  };
*)
procedure tobjcrttiwriter.gen_objc_protocol_list(list: tasmlist; protolist: tfpobjectlist; out protolistsym: tasmlabel);
  var
    i         : Integer;
    protosym  : TAsmSymbol;
    protodef  : tobjectdef;
    tcb       : ttai_typedconstbuilder;
  begin
    if not Assigned(protolist) or
       (protolist.Count=0) then
      begin
        protolistsym:=nil;
        Exit;
      end;

    for i:=0 to protolist.Count-1 do
      begin
        protodef:=TImplementedInterface(protolist[i]).IntfDef;
        protosym:=objcfindprotocolentry(protodef.objextname^);
        if not assigned(protosym) then
          begin
            gen_objc_protocol(list,protodef,protosym);
            objcaddprotocolentry(protodef.objextname^,protosym);
          end;
      end;

    tcb:=ctai_typedconstbuilder.create([tcalo_is_lab,tcalo_new_section]);
    tcb.begin_anonymous_record(internaltypeprefixName[itp_objc_proto_list]+tostr(protolist.Count),
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    { protocol lists are stored in .objc_cat_cls_meth section }
    current_asmdata.getlabel(protolistsym, alt_data);

    if (abi=oa_fragile) then
      { From Clang: next, always nil}
      tcb.emit_tai(tai_const.Create_nil_dataptr,ptruinttype);
    { From Clang: protocols count}
    tcb.emit_tai(Tai_const.Create_int_dataptr(protolist.Count),ptruinttype);
    for i:=0 to protolist.Count-1 do
      begin
        protodef:=(protolist[i] as TImplementedInterface).IntfDef;
        protosym:=objcfindprotocolentry(protodef.objextname^);
        if not Assigned(protosym) then
          begin
            { For some reason protosym is not declared, though must be!
              Probably gen_obcj1_protocol returned wrong protosym
            }
            InternalError(2009091602);
          end;
        tcb.emit_tai(tai_const.Create_sym(protosym),voidpointertype);
      end;
    list.concatList(
      tcb.get_final_asmlist(
        protolistsym,tcb.end_anonymous_record,
        sec_objc_cat_cls_meth,'_OBJC_PROTOCOLLIST',sizeof(pint)
      )
    );
    tcb.free;
    { the symbol will point to a record }
  end;


{ Generate rtti for an Objective-C methods (methods without implementation) }
{ items : TFPObjectList of Tprocdef }
procedure tobjcrttiwriter.gen_objc_cat_methods(list:TAsmList; items: TFPObjectList; section: tasmsectiontype;
  const sectname: string; out listsym: TAsmLabel);
var
  i     : integer;
  m     : tprocdef;
  lab   : tasmlabel;
  ldef  : tdef;
  mtype : tdef;
  tcb   : ttai_typedconstbuilder;
begin
  if not assigned(items) or
     (items.count=0) then
    exit;

  tcb:=ctai_typedconstbuilder.create([tcalo_is_lab,tcalo_new_section]);
  current_asmdata.getlabel(listsym,alt_data);
  tcb.begin_anonymous_record(
    internaltypeprefixName[itp_objc_cat_methods]+tostr(items.count),
    C_alignment,1,
    targetinfos[target_info.system]^.alignment.recordalignmin,
    targetinfos[target_info.system]^.alignment.maxCrecordalign);
  if (abi=oa_nonfragile) then
    begin
      { size of each entry -- always 32 bit value }
      mtype:=search_named_unit_globaltype('OBJC','OBJC_METHOD',true).typedef;
      tcb.emit_ord_const(mtype.size,u32inttype);
    end;
  tcb.emit_ord_const(items.count,u32inttype);
  for i:=0 to items.Count-1 do
    begin
      m:=tprocdef(items[i]);
      objcreatestringpoolentry(m.messageinf.str^,sp_objcvarnames,sec_objc_meth_var_names,lab,ldef);
      tcb.emit_tai(Tai_const.Create_sym(lab),ldef);
      objcreatestringpoolentry(objcencodemethod(m),sp_objcvartypes,sec_objc_meth_var_types,lab,ldef);
      tcb.emit_tai(Tai_const.Create_sym(lab),ldef);
      { placeholder for address of implementation? }
      if (abi=oa_nonfragile) then
        tcb.emit_tai(Tai_const.Create_nil_codeptr,codeptruinttype);
    end;
  list.concatList(
    tcb.get_final_asmlist(
      listsym,tcb.end_anonymous_record,section,sectname,sizeof(pint))
  );
  tcb.free;
end;


{ Generate the rtti sections for all obj-c classes defined in st, and return
  these classes in the classes list. }
procedure tobjcrttiwriter.gen_objc_rtti_sections(list:TAsmList; st:TSymtable);
  var
    i: longint;
    def,
    rttidef: tdef;
    sym: TAsmSymbol;
  begin
    if not Assigned(st) then
      exit;

    for i:=0 to st.DefList.Count-1 do
      begin
        def:=tdef(st.DefList[i]);
        { check whether all types used in Objective-C class/protocol/category
          declarations can be used with the Objective-C run time (can only be
          done now, because at parse-time some of these types can still be
          forwarddefs) }
        if is_objc_class_or_protocol(def) then
          if not tobjectdef(def).check_objc_types then
            continue;
        if is_objcclass(def) and
           not(oo_is_external in tobjectdef(def).objectoptions) then
          begin
            if not(oo_is_classhelper in tobjectdef(def).objectoptions) then
              begin
                gen_objc_classes_sections(list,tobjectdef(def),sym,rttidef);
                classsyms.add(sym);
                classrttidefs.add(rttidef);
                classdefs.add(def);
              end
            else
              begin
                gen_objc_category_sections(list,tobjectdef(def),sym,rttidef);
                catsyms.add(sym);
                catrttidefs.add(rttidef);
                catdefs.add(def);
              end
          end;
      end;
  end;


constructor tobjcrttiwriter.create(_abi: tobjcabi);
  begin
    fabi:=_abi;
    classdefs:=tfpobjectlist.create(false);
    classsyms:=tfpobjectlist.create(false);
    classrttidefs:=tfpobjectlist.create(false);
    catrttidefs:=tfpobjectlist.create(false);
    catdefs:=tfpobjectlist.create(false);
    catsyms:=tfpobjectlist.create(false);
  end;


destructor tobjcrttiwriter.destroy;
  begin
    classdefs.free;
    classsyms.free;
    classrttidefs.free;
    catrttidefs.free;
    catdefs.free;
    catsyms.free;
    inherited destroy;
  end;

{******************************************************************
                 RTTI generation -- Fragile ABI
*******************************************************************}

{ generate an instance variables list for an obj-c class. }
procedure tobjcrttiwriter_fragile.gen_objc_ivars(list: TAsmList; objccls: tobjectdef; out ivarslabel: TAsmLabel);
  type
    ivar_data = record
      vf      : tfieldvarsym;
      namesym : TAsmLabel;
      namedef : tdef;
      typesym : TAsmLabel;
      typedef : tdef;
    end;
  var
    i     : integer;
    vf    : tfieldvarsym;
    vars  : array of ivar_data;
    vcnt  : Integer;
    enctype : ansistring;
    encerr  : tdef;
  begin
    ivarslabel:=nil;

    vcnt:=0;
    vars:=nil;
    setLength(vars,objccls.symtable.SymList.Count);

    for i:=0 to objccls.symtable.SymList.Count-1 do
      if tsym(objccls.symtable.SymList[i]).typ=fieldvarsym then
        begin
          vf:=tfieldvarsym(objccls.symtable.SymList[i]);
          if objctryencodetype(vf.vardef,enctype,encerr) then
            begin
              vars[vcnt].vf:=vf;
              objcreatestringpoolentry(vf.RealName,sp_objcvarnames,sec_objc_meth_var_names,vars[vcnt].namesym,vars[vcnt].namedef);
              objcreatestringpoolentry(enctype,sp_objcvartypes,sec_objc_meth_var_types,vars[vcnt].typesym,vars[vcnt].typedef);
              inc(vcnt);
            end
          else
            { Should be caught during parsing }
            internalerror(2009090601);
        end;
    if vcnt=0 then
      exit;

    new_section(list,sec_objc_instance_vars,'_OBJC_INSTANCE_VARS',sizeof(pint));

    current_asmdata.getlabel(ivarslabel,alt_data);
    list.Concat(tai_label.Create(ivarslabel));

    { objc_ivar_list: first the number of elements }
    list.Concat(tai_const.Create_32bit(vcnt));

    for i:=0 to vcnt-1 do
      begin
        { reference to the instance variable name }
        list.Concat(tai_const.Create_sym(vars[i].namesym));
        { reference to the encoded type }
        list.Concat(tai_const.Create_sym(vars[i].typesym));
        { and the offset of the field }
        list.Concat(tai_const.Create_32bit(vars[i].vf.fieldoffset));
      end;
  end;


(* From GCC:

  struct _objc_protocol_extension
    {
      uint32_t size;	// sizeof (struct _objc_protocol_extension)
      struct objc_method_list	*optional_instance_methods;
      struct objc_method_list   *optional_class_methods;
      struct objc_prop_list	*instance_properties;
    }
*)
function tobjcrttiwriter_fragile.gen_objc_protocol_ext(list: TAsmList; optinstsym, optclssym: TAsmLabel): TAsmLabel;
  var
    tcb: ttai_typedconstbuilder;
  begin
    if assigned(optinstsym) or
       assigned(optclssym) then
      begin
        current_asmdata.getlabel(Result,alt_data);
        tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
        tcb.begin_anonymous_record(
          internaltypeprefixName[itb_objc_fr_protocol_ext],
          C_alignment,1,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        { size of this structure }
        tcb.emit_ord_const(16,u32inttype);
        { optional instance methods }
        ConcatSymOrNil(tcb,optinstsym,voidpointertype);
        { optional class methods }
        ConcatSymOrNil(tcb,optclssym,voidpointertype);
        { optional properties (todo) }
        ConcatSymOrNil(tcb,nil,voidpointertype);
        list.concatList(
          tcb.get_final_asmlist(
            result,tcb.end_anonymous_record,
            sec_objc_protocol_ext,'_OBJC_PROTOCOLEXT',sizeof(pint)
          )
        );
        tcb.free;
      end
    else
      Result:=nil;
  end;


{ Generate rtti for an Objective-C protocol  }
procedure tobjcrttiwriter_fragile.gen_objc_protocol(list:TAsmList; protocol: tobjectdef; out protocollabel: TAsmSymbol);
  var
    namesym     : TAsmLabel;
    namedef     : tdef;
    protolist   : TAsmLabel;
    reqinstsym,
    optinstsym,
    reqclssym,
    optclssym,
    protoext,
    lbl          : TAsmLabel;
    tcb          : ttai_typedconstbuilder;
  begin
    gen_objc_protocol_list(list,protocol.ImplementedInterfaces,protolist);
    gen_objc_protocol_elements(list,protocol,reqinstsym,optinstsym,reqclssym,optclssym);
    protoext:=gen_objc_protocol_ext(list,optinstsym,optclssym);

    current_asmdata.getlabel(lbl,alt_data);
    protocollabel:=lbl;

    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
    tcb.begin_anonymous_record(
      internaltypeprefixName[itb_objc_fr_protocol],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    { protocol's isa - points to information about optional methods/properties }
    ConcatSymOrNil(tcb,protoext,voidpointertype);
    { name }
    objcreatestringpoolentry(protocol.objextname^,sp_objcclassnames,sec_objc_class_names,namesym,namedef);
    tcb.queue_init(voidpointertype);
    tcb.queue_emit_asmsym(namesym,namedef);
    { protocol's list }
    ConcatSymOrNil(tcb,protolist,voidpointertype);
    { instance methods, in __cat_inst_meth }
    ConcatSymOrNil(tcb,reqinstsym,voidpointertype);
    { class methods, in __cat_cls_meth }
    ConcatSymOrNil(tcb,reqclssym,voidpointertype);
    list.concatList(
      tcb.get_final_asmlist(
        lbl,tcb.end_anonymous_record,
        sec_objc_protocol,'_OBJC_PROTOCOL',sizeof(pint)
      )
    );
    tcb.free;
  end;


(*
From Clang:

  struct _objc_category {
  char *category_name;
  char *class_name;
  struct _objc_method_list *instance_methods;
  struct _objc_method_list *class_methods;
  struct _objc_protocol_list *protocols;
  uint32_t size; // <rdar://4585769>
  struct _objc_property_list *instance_properties;
  };
*)

{ Generate rtti for an Objective-C class and its meta-class. }
procedure tobjcrttiwriter_fragile.gen_objc_category_sections(list:TAsmList; objccat: tobjectdef; out catlabel: TAsmSymbol; out catlabeldef: tdef);
  var
    catstrsym,
    clsstrsym,
    protolistsym  : TAsmLabel;
    instmthdlist,
    clsmthdlist,
    catsym        : TAsmSymbol;
    catstrdef,
    clsstrdef,
    catdef        : tdef;
    tcb           : ttai_typedconstbuilder;
  begin
    { the category name }
    objcreatestringpoolentry(objccat.objextname^,sp_objcclassnames,sec_objc_class_names,catstrsym,catstrdef);

    { the name of the class it extends }
    objcreatestringpoolentry(objccat.childof.objextname^,sp_objcclassnames,sec_objc_class_names,clsstrsym,clsstrdef);

    { generate the methods lists }
    gen_objc_methods(list,objccat,instmthdlist,false,true);
    gen_objc_methods(list,objccat,clsmthdlist,true,true);

    { generate implemented protocols list }
    gen_objc_protocol_list(list,objccat.ImplementedInterfaces,protolistsym);

    { category declaration section }
    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
    tcb.begin_anonymous_record(
      internaltypeprefixName[itb_objc_fr_category],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    tcb.queue_init(voidpointertype);
    tcb.queue_emit_asmsym(catstrsym,catstrdef);
    tcb.queue_init(voidpointertype);
    tcb.queue_emit_asmsym(clsstrsym,clsstrdef);
    ConcatSymOrNil(tcb,instmthdlist,voidpointertype);
    ConcatSymOrNil(tcb,clsmthdlist,voidpointertype);
    ConcatSymOrNil(tcb,protolistsym,voidpointertype);
    { size of this structure }
    tcb.emit_ord_const(28,u32inttype);
    { properties, not yet supported }
    tcb.emit_ord_const(0,u32inttype);
    catdef:=tcb.end_anonymous_record;
    catsym:=current_asmdata.DefineAsmSymbol(objccat.rtti_mangledname(objcclassrtti),AB_LOCAL,AT_DATA,catdef);
    list.concatList(
      tcb.get_final_asmlist(
        catsym,catdef,
        sec_objc_category,'_OBJC_CATEGORY',sizeof(pint)
      )
    );
    tcb.free;

    catlabel:=catsym;
    catlabeldef:=catdef;
  end;

(*
From Clang:

  struct _objc_class {
    Class isa;
    Class super_class;
    const char *name;
    long version;
    long info;
    long instance_size;
    struct _objc_ivar_list *ivars;
    struct _objc_method_list *methods;
    struct _objc_cache *cache;
    struct _objc_protocol_list *protocols;
    // Objective-C 1.0 extensions (<rdr://4585769>) -- for garbage collection
    const char *ivar_layout;
    struct _objc_class_ext *ext;
  };
*)



{ Generate rtti for an Objective-C class and its meta-class. }
procedure tobjcrttiwriter_fragile.gen_objc_classes_sections(list:TAsmList; objclss: tobjectdef; out classlabel: TAsmSymbol; out classlabeldef: tdef);
  const
    CLS_CLASS  = 1;
    CLS_META   = 2;
    CLS_HIDDEN = $20000;
    META_INST_SIZE = 40+8; // sizeof(objc_class) + 8
  var
    root          : tobjectdef;
    metasym,
    mthdlist,
    clssym        : TAsmSymbol;
    superStrDef,
    classStrDef,
    metaisaStrDef,
    metaDef,
    clsDef        : tdef;
    superStrSym,
    classStrSym,
    metaisaStrSym,
    ivarslist,
    protolistsym  : TAsmLabel;
    hiddenflag    : cardinal;
    tcb           : ttai_typedconstbuilder;

  begin
    { generate the class methods list }
    gen_objc_methods(list,objclss,mthdlist,true,false);

    { generate implemented protocols list }
    gen_objc_protocol_list(list,objclss.ImplementedInterfaces,protolistsym);

    { register necessary names }
    { 1) the superclass }
    if assigned(objclss.childof) then
      objcreatestringpoolentry(objclss.childof.objextname^,sp_objcclassnames,sec_objc_class_names,superStrSym,superStrDef)
    else
      begin
        { not empty string, but nil! }
        superStrSym:=nil;
        superStrDef:=voidpointertype;
      end;

    { 2) the current class }
    objcreatestringpoolentry(objclss.objextname^,sp_objcclassnames,sec_objc_class_names,classStrSym,classStrDef);
    { 3) the isa }
    { From Clang: The isa for the meta-class is the root of the hierarchy. }
    root:=objclss;
    while assigned(root.childof) do
      root:=root.childof;
    objcreatestringpoolentry(root.objextname^,sp_objcclassnames,sec_objc_class_names,metaisaStrSym,metaisaStrDef);

    { 4) the flags }
    { consider every class declared in the implementation section of a unit
      as "hidden"
    }
    hiddenflag:=0;
    if (objclss.owner.symtabletype=staticsymtable) and
       current_module.is_unit then
      hiddenflag:=CLS_HIDDEN;

    { class declaration section }

    { 1) meta-class declaration  }
    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
    tcb.begin_anonymous_record(internaltypeprefixName[itb_objc_fr_meta_class],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    tcb.queue_init(voidpointertype);
    tcb.queue_emit_asmsym(metaisaStrSym,metaisaStrDef);
    { pointer to the superclass name if any, otherwise nil }
    if assigned(superstrsym) then
      begin
        tcb.queue_init(voidpointertype);
        tcb.queue_emit_asmsym(superStrSym,superStrDef);
      end
    else
      tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
    { pointer to the class name }
    tcb.queue_init(voidpointertype);
    tcb.queue_emit_asmsym(classStrSym,classStrDef);

    { version is always 0 currently }
    tcb.emit_ord_const(0,u32inttype);
    { CLS_META for meta-classes }
    tcb.emit_ord_const(hiddenflag or CLS_META,u32inttype);
    { size of the meta-class instance: sizeof(objc_class) + 8 bytes }
    tcb.emit_ord_const(META_INST_SIZE,u32inttype);
    { meta-classes don't have ivars list (=0) }
    tcb.emit_ord_const(0,u32inttype);
    { class methods list (stored in "__cls_meth" section) }
    ConcatSymOrNil(tcb,mthdlist,voidpointertype);
    { From Clang: cache is always nil }
    tcb.emit_ord_const(0,u32inttype);
    { protocols }
    ConcatSymOrNil(tcb,protolistsym,voidpointertype);
    { From Clang: ivar_layout for meta-class is always NULL. }
    tcb.emit_ord_const(0,u32inttype);
    { From Clang: The class extension is always unused for meta-classes. }
    tcb.emit_ord_const(0,u32inttype);
    metaDef:=tcb.end_anonymous_record;
    metasym:=current_asmdata.DefineAsmSymbol(objclss.rtti_mangledname(objcmetartti),AB_LOCAL,AT_DATA,metadef);
    list.concatList(
      tcb.get_final_asmlist(
        metasym,metaDef,
        sec_objc_meta_class,'_OBJC_META_CLASS',sizeof(pint)
      )
    );
    tcb.free;


    { 2) regular class declaration }

    { generate the instance methods list }
    gen_objc_methods(list,objclss,mthdlist,false,false);
    { generate the instance variables list }
    gen_objc_ivars(list,objclss,ivarslist);

    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
    tcb.begin_anonymous_record(internaltypeprefixName[itb_objc_fr_class],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    { for class declaration: the isa points to the meta-class declaration }
    tcb.emit_tai(Tai_const.Create_sym(metasym),cpointerdef.getreusable(metaDef));
    { pointer to the super_class name if any, nil otherwise }
    if assigned(superStrSym) then
      begin
        tcb.queue_init(voidcodepointertype);
        tcb.queue_emit_asmsym(superStrSym,superStrDef)
      end
    else
      tcb.emit_tai(Tai_const.Create_nil_dataptr,voidcodepointertype);
    { pointer to the class name }
    tcb.queue_init(voidcodepointertype);
    tcb.queue_emit_asmsym(classStrSym,classStrDef);
    { version is always 0 currently }
    tcb.emit_ord_const(0,u32inttype);
    { CLS_CLASS for classes }
    tcb.emit_ord_const(hiddenflag or CLS_CLASS,u32inttype);
    { size of instance: total size of instance variables }
    tcb.emit_ord_const(tobjectsymtable(objclss.symtable).datasize,u32inttype);
    { objc_ivar_list (stored in "__instance_vars" section) }
    ConcatSymOrNil(tcb,ivarslist,voidpointertype);
    { instance methods list (stored in "__inst_meth" section) }
    ConcatSymOrNil(tcb,mthdlist,voidpointertype);
    { From Clang: cache is always NULL }
    tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
    { protocols, protolistsym has been created for meta-class, no need to create another one}
    ConcatSymOrNil(tcb, protolistsym,voidpointertype);
    { From Clang: strong ivar_layout, necessary for garbage collection support }
    tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
    { TODO: From Clang: weak ivar_layout, necessary for garbage collection support }
    tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);

    clsDef:=tcb.end_anonymous_record;
    clssym:=current_asmdata.DefineAsmSymbol(objclss.rtti_mangledname(objcclassrtti),AB_LOCAL,AT_DATA,clsDef);
    list.concatList(
      tcb.get_final_asmlist(
        clssym,clsDef,
        sec_objc_class,'_OBJC_CLASS',sizeof(pint)
      )
    );
    tcb.free;

    classlabel:=clssym;
    classlabeldef:=clsDef;
  end;


{ Generate the global information sections (objc_symbols and objc_module_info)
  for this module. }
procedure tobjcrttiwriter_fragile.gen_objc_info_sections(list: tasmlist);
  var
    i: longint;
    sym : TAsmSymbol;
    lab : TAsmLabel;
    symsdef,
    def : tdef;
    parent: tobjectdef;
    superclasses: tfpobjectlist;
    tcb: ttai_typedconstbuilder;
  begin
    if (classsyms.count<>0) or
       (catsyms.count<>0) then
      begin
        tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
        tcb.begin_anonymous_record('',
          C_alignment,1,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        { ??? (always 0 in Clang) }
        tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
        { ??? (From Clang: always 0, pointer to some selector) }
        tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
        { From Clang: number of defined classes }
        tcb.emit_ord_const(classsyms.count,u16inttype);
        { From Clang: number of defined categories }
        tcb.emit_ord_const(catsyms.count,u16inttype);
        { first all classes }
        for i:=0 to classsyms.count-1 do
           tcb.emit_tai(Tai_const.Create_sym(tasmsymbol(classsyms[i])),tdef(classrttidefs[i]));
        { then all categories }
        for i:=0 to catsyms.count-1 do
          tcb.emit_tai(Tai_const.Create_sym(tasmsymbol(catsyms[i])),tdef(catrttidefs[i]));
        symsdef:=tcb.end_anonymous_record;
        sym := current_asmdata.DefineAsmSymbol(target_asm.labelprefix+'_OBJC_SYMBOLS_$',AB_LOCAL,AT_DATA,symsdef);
        list.concatList(tcb.get_final_asmlist(sym,
          symsdef,
          sec_objc_symbols,'_OBJC_SYMBOLS',
          sizeof(pint)));
     end
    else
      begin
        sym:=nil;
        symsdef:=nil;
      end;

    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
    tcb.begin_anonymous_record('',
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);
    { version number = 7 (always, both for gcc and clang) }
    tcb.emit_ord_const(7,ptruinttype);
    { sizeof(objc_module): 4 pointer-size entities }
    tcb.emit_ord_const(sizeof(pint)*4,ptruinttype);
    { used to be file name, now unused (points to empty string) }
    objcreatestringpoolentry('',sp_objcclassnames,sec_objc_class_names,lab,def);
    tcb.emit_tai(Tai_const.Create_sym(lab),def);
    { pointer to classes/categories list declared in this module }
    if assigned(sym) then
      tcb.emit_tai(tai_const.Create_sym(sym),symsdef)
    else
      tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
    current_asmdata.getlabel(lab,alt_data);
    list.concatList(tcb.get_final_asmlist(lab,
      tcb.end_anonymous_record,sec_objc_module_info,'_OBJC_MODULE_INFO',4));


    { Add lazy references to parent classes of all classes defined in this unit }
    superclasses:=tfpobjectlist.create(false);
    for i:=0 to classdefs.count-1 do
      begin
        parent:=tobjectdef(classdefs[i]).childof;
        { warning: linear search, performance hazard if large number of subclasses }
        if assigned(parent) and
           (superclasses.indexof(parent)=-1) then
          begin
            list.concat(tai_directive.create(asd_lazy_reference,'.objc_class_name_'+parent.objextname^));
            superclasses.add(parent);
          end;
      end;
    for i:=0 to catdefs.count-1 do
      begin
        parent:=tobjectdef(catdefs[i]).childof;
        { warning: linear search, performance hazard if large number of subclasses }
        if assigned(parent) and
           (superclasses.indexof(parent)=-1) then
          begin
            list.concat(tai_directive.create(asd_lazy_reference,'.objc_class_name_'+parent.objextname^));
            superclasses.add(parent);
          end;
      end;
    superclasses.free;
    { reference symbols for all classes and categories defined in this unit }
    for i:=0 to classdefs.count-1 do
      list.concat(tai_symbol.Createname_global_value('.objc_class_name_'+tobjectdef(classdefs[i]).objextname^,AT_DATA,0,0,voidpointertype));
    for i:=0 to catdefs.count-1 do
      list.concat(tai_symbol.Createname_global_value('.objc_category_name_'+
        tobjectdef(catdefs[i]).childof.objextname^+'_'+
        tobjectdef(catdefs[i]).objextname^,AT_DATA,0,0,voidpointertype));
  end;


constructor tobjcrttiwriter_fragile.create;
  begin
    inherited create(oa_fragile);
  end;


{******************************************************************
                 RTTI generation -- Non-Fragile ABI
*******************************************************************}

(*
From Clang:
/// EmitIvarList - Emit the ivar list for the given
/// implementation. The return value has type
/// IvarListnfABIPtrTy.
///  struct _ivar_t {
///   unsigned long int *offset;  // pointer to ivar offset location
///   char *name;
///   char *type;
///   uint32_t alignment;
///   uint32_t size;
/// }
/// struct _ivar_list_t {
///   uint32 entsize;  // sizeof(struct _ivar_t)
///   uint32 count;
///   struct _iver_t list[count];
/// }
///
*)
procedure tobjcrttiwriter_nonfragile.gen_objc_ivars(list: tasmlist; objccls: tobjectdef; out ivarslabel: tasmlabel);
  type
    ivar_data = record
      vf      : tfieldvarsym;
      namesym : TAsmLabel;
      namedef : tdef;
      typesym : TAsmLabel;
      typedef : tdef;
      offssym : TAsmSymbol;
    end;
  var
    ivtype: tdef;
    vf    : tfieldvarsym;
    vars  : array of ivar_data;
    i     : integer;
    vcnt  : integer;
    enctype : ansistring;
    encerr  : tdef;
    prefix  : shortstring;
    vis     : TAsmsymbind;
    tcb     : ttai_typedconstbuilder;
    pptruinttype : tdef;
  begin
    ivarslabel:=nil;
    prefix:='';

    vcnt:=0;
    vars:=nil;
    setLength(vars,objccls.symtable.SymList.Count);

    tcb:=nil;
    prefix:='';

    for i:=0 to objccls.symtable.SymList.Count-1 do
      if tsym(objccls.symtable.SymList[i]).typ=fieldvarsym then
        begin
          vf:=tfieldvarsym(objccls.symtable.SymList[i]);
          if objctryencodetype(vf.vardef,enctype,encerr) then
            begin
              vars[vcnt].vf:=vf;
              objcreatestringpoolentry(vf.RealName,sp_objcvarnames,sec_objc_meth_var_names,vars[vcnt].namesym,vars[vcnt].namedef);
              objcreatestringpoolentry(enctype,sp_objcvartypes,sec_objc_meth_var_types,vars[vcnt].typesym,vars[vcnt].typedef);
              if (vcnt=0) then
                begin
                  tcb:=ctai_typedconstbuilder.create([tcalo_new_section]);
                  prefix:=target_info.cprefix+'OBJC_IVAR_$_'+objccls.objextname^+'.';
                end
              else
                tcb:=ctai_typedconstbuilder.create([tcalo_new_section]);
              { This matches gcc/Clang, but is strange: I would expect private
                fields to be local symbols rather than private_extern (which
                is "package-global") (JM)
              }
              if not(vf.visibility in [vis_public,vis_protected,vis_strictprotected]) then
                vis:=AB_PRIVATE_EXTERN
              else
                vis:=AB_GLOBAL;
              vars[vcnt].offssym:=current_asmdata.DefineAsmSymbol(prefix+vf.RealName,vis,AT_DATA,ptruinttype);
              tcb.emit_tai(tai_const.Create_int_dataptr(vf.fieldoffset),ptruinttype);
              list.concatList(
                tcb.get_final_asmlist(
                  vars[vcnt].offssym,ptruinttype,
                  sec_objc_const,'_OBJC_IVAR_OFFSETS',sizeof(pint)
                )
              );
              tcb.free;
              inc(vcnt);
            end
          else
            { must be caught during parsing }
            internalerror(2009092301);
        end;
    if vcnt=0 then
      exit;

    tcb:=ctai_typedconstbuilder.create([tcalo_is_lab,tcalo_new_section,tcalo_no_dead_strip]);

    current_asmdata.getlabel(ivarslabel,alt_data);

    tcb.begin_anonymous_record(
      internaltypeprefixName[itb_objc_nf_ivars]+tostr(vcnt),
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    { size of each entry -- always 32 bit value }
    ivtype:=search_named_unit_globaltype('OBJC','OBJC_IVAR',true).typedef;
    tcb.emit_ord_const(ivtype.size,u32inttype);
    { number of entries -- always 32 bit value }
    tcb.emit_ord_const(vcnt,u32inttype);

    { we use voidpointertype for all elements so that we can reuse the
      recorddef for all ivar tables with the same number of elements }
    pptruinttype:=cpointerdef.getreusable(ptruinttype);
    for i:=0 to vcnt-1 do
      begin
        { reference to the offset }
        tcb.emit_tai(tai_const.Create_sym(vars[i].offssym),pptruinttype);
        { reference to the instance variable name (}
        tcb.queue_init(voidpointertype);
        tcb.queue_emit_asmsym(vars[i].namesym,vars[i].namedef);
        { reference to the encoded type }
        tcb.queue_init(voidpointertype);
        tcb.queue_emit_asmsym(vars[i].typesym,vars[i].typedef);
        { alignment -- always 32 bit value }
        tcb.emit_ord_const(vars[i].vf.vardef.alignment,u32inttype);
        { size -- always 32 bit value }
        tcb.emit_ord_const(vars[i].vf.vardef.size,u32inttype);
      end;
    list.concatList(
      tcb.get_final_asmlist(
        ivarslabel,tcb.end_anonymous_record,
        sec_objc_instance_vars,'_OBJC_INSTANCE_VARS',sizeof(pint)
      )
    );
    tcb.free;
  end;


(*
From Clang:
/// GetOrEmitProtocol - Generate the protocol meta-data:
/// @code
/// struct _protocol_t {
///   id isa;  // NULL
///   const char * const protocol_name;
///   const struct _protocol_list_t * protocol_list; // super protocols
///   const struct method_list_t * const instance_methods;
///   const struct method_list_t * const class_methods;
///   const struct method_list_t *optionalInstanceMethods;
///   const struct method_list_t *optionalClassMethods;
///   const struct _prop_list_t * properties;
///   const uint32_t size;  // sizeof(struct _protocol_t)
///   const uint32_t flags;  // = 0
/// }
/// @endcode
*)
procedure tobjcrttiwriter_nonfragile.gen_objc_protocol(list: tasmlist; protocol: tobjectdef; out protocollabel: tasmsymbol);
  var
    lbl,
    listsym       : TAsmSymbol;
    namedef       : tdef;
    namesym,
    protolist     : TAsmLabel;
    reqinstsym,
    reqclssym,
    optinstsym,
    optclssym     : TAsmLabel;
    prottype      : tdef;
    tcb           : ttai_typedconstbuilder;
  begin
    gen_objc_protocol_list(list,protocol.ImplementedInterfaces,protolist);
    gen_objc_protocol_elements(list,protocol,reqinstsym,optinstsym,reqclssym,optclssym);

    { label for the protocol needs to be
        a) in a coalesced section (so multiple definitions of the same protocol
           can be merged by the linker)
        b) private_extern (should only be merged within the same module)
        c) weakly defined (so multiple definitions don't cause errors)
    }
    prottype:=search_named_unit_globaltype('OBJC','OBJC_PROTOCOL',true).typedef;
    lbl:=current_asmdata.DefineAsmSymbol(protocol.rtti_mangledname(objcclassrtti),AB_PRIVATE_EXTERN,AT_DATA,prottype);
    protocollabel:=lbl;

    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_weak]);
    tcb.maybe_begin_aggregate(prottype);
    { protocol's isa - always nil }
    tcb.emit_tai(Tai_const.Create_nil_dataptr,objc_idtype);
    { name }
    objcreatestringpoolentry(protocol.objextname^,sp_objcclassnames,sec_objc_class_names,namesym,namedef);
    tcb.queue_init(charpointertype);
    tcb.queue_emit_asmsym(namesym,namedef);
    { parent protocols list }
    ConcatSymOrNil(tcb,protolist,voidpointertype);
    { required instance methods }
    ConcatSymOrNil(tcb,reqinstsym,voidpointertype);
    { required class methods }
    ConcatSymOrNil(tcb,reqclssym,voidpointertype);
    { optional instance methods }
    ConcatSymOrNil(tcb,optinstsym,voidpointertype);
    { optional class methods }
    ConcatSymOrNil(tcb,optclssym,voidpointertype);
    { TODO: properties }
    tcb.emit_tai(Tai_const.Create_nil_dataptr,voidpointertype);
    { size of this type }
    tcb.emit_ord_const(prottype.size,u32inttype);
    { flags }
    tcb.emit_ord_const(0,u32inttype);
    tcb.maybe_end_aggregate(prottype);
    list.concatList(
      tcb.get_final_asmlist(
        lbl,prottype,
        sec_data_coalesced,'_OBJC_PROTOCOL',sizeof(pint)
      )
    );
    tcb.free;

    { also add an entry to the __DATA, __objc_protolist section, required to
      register the protocol with the runtime }
    listsym:=current_asmdata.DefineAsmSymbol(protocol.rtti_mangledname(objcmetartti),AB_PRIVATE_EXTERN,AT_DATA,cpointerdef.getreusable(prottype));
    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_weak,tcalo_no_dead_strip]);
    tcb.emit_tai(tai_const.Create_sym(lbl),cpointerdef.getreusable(prottype));
    list.concatList(
      tcb.get_final_asmlist(
        listsym,cpointerdef.getreusable(prottype),
        sec_objc_protolist,'_OBJC_PROTOLIST',sizeof(pint)
      )
    );
    tcb.free;
  end;

(*
From Clang:
/// struct _category_t {
///   const char * const name;
///   struct _class_t *const cls;
///   const struct _method_list_t * const instance_methods;
///   const struct _method_list_t * const class_methods;
///   const struct _protocol_list_t * const protocols;
///   const struct _prop_list_t * const properties;
/// }
*)
procedure tobjcrttiwriter_nonfragile.gen_objc_category_sections(list:TAsmList; objccat: tobjectdef; out catlabel: TAsmSymbol; out catlabeldef: tdef);
  var
    catstrsym,
    protolistsym  : TAsmLabel;
    instmthdlist,
    clsmthdlist,
    clssym,
    catsym        : TAsmSymbol;
    catstrdef,
    catdef        : tdef;
    tcb           : ttai_typedconstbuilder;
  begin
    { the category name }
    objcreatestringpoolentry(objccat.objextname^,sp_objcclassnames,sec_objc_class_names,catstrsym,catstrdef);

    { the class it extends }
    clssym:=current_asmdata.RefAsmSymbol(objccat.childof.rtti_mangledname(objcclassrtti),AT_DATA);

    { generate the methods lists }
    gen_objc_methods(list,objccat,instmthdlist,false,true);
    gen_objc_methods(list,objccat,clsmthdlist,true,true);

    { generate implemented protocols list }
    gen_objc_protocol_list(list,objccat.ImplementedInterfaces,protolistsym);

    { category declaration section }
    tcb:=ctai_typedconstbuilder.create([tcalo_new_section]);
    tcb.begin_anonymous_record(internaltypeprefixName[itb_objc_nf_category],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);
    tcb.queue_init(voidpointertype);
    tcb.queue_emit_asmsym(catstrsym,catstrdef);
    tcb.emit_tai(Tai_const.Create_sym(clssym),voidpointertype);
    ConcatSymOrNil(tcb,instmthdlist,voidpointertype);
    ConcatSymOrNil(tcb,clsmthdlist,voidpointertype);
    ConcatSymOrNil(tcb,protolistsym,voidpointertype);
    { properties, not yet supported }
    tcb.emit_tai(Tai_const.Create_nil_dataptr,voidpointertype);
    catdef:=tcb.end_anonymous_record;
    catsym:=current_asmdata.DefineAsmSymbol(objccat.rtti_mangledname(objcclassrtti),AB_LOCAL,AT_DATA,catdef);
    list.concatList(
      tcb.get_final_asmlist(
        catsym,catdef,
        sec_objc_const,'_OBJC_CATEGORY',sizeof(pint)
      )
    );
    tcb.free;

    catlabel:=catsym;
    catlabeldef:=catdef;
  end;


(*
From Clang:
/// BuildIvarLayout - Builds ivar layout bitmap for the class
/// implementation for the __strong or __weak case.
/// The layout map displays which words in ivar list must be skipped
/// and which must be scanned by GC (see below). String is built of bytes.
/// Each byte is divided up in two nibbles (4-bit each). Left nibble is count
/// of words to skip and right nibble is count of words to scan. So, each
/// nibble represents up to 15 workds to skip or scan. Skipping the rest is
/// represented by a 0x00 byte which also ends the string.
/// 1. when ForStrongLayout is true, following ivars are scanned:
/// - id, Class
/// - object *  // note: this "object" means "Objective-C object" (JM)
/// - __strong anything
///
/// 2. When ForStrongLayout is false, following ivars are scanned:
/// - __weak anything
*)
(*

Only required when supporting garbage collection

procedure tobjcrttiwriter_nonfragile.gen_objc_ivargc_recursive(st: tabstractrecordsymtable; ptrbset: tbitset; startoffset: puint; il: tivarlayouttype);
var
  i: longint;
  fs: tfieldvarsym;
  includelen: longint;
begin
  for i:=0 to st.SymList.count-1 do
    if (tsym(st.symlist[i]).typ=fieldvarsym) then
      begin
        fs:=tfieldvarsym(st.symlist[i]);
        includelen:=0;
        case fs.vardef.typ of
          pointerdef,
          classrefdef:
            if (fs.vardef=objc_idtype) or
               (fs.vardef=objc_metaclasstype) then
              includelen:=1;
          recorddef:
            TODO: bitpacking -> offset differences
            gen_objc_ivargc_recursive(tabstractrecordsymtable(trecorddef(fs.vardef).symtable),ptrbset,startoffset+fs.fieldoffset,il);
          arraydef:
            begin
              if not is_special_
            end;
          objectdef :
            begin
              case tobjectdef(fs.vardef).objecttype of
                odt_objcclass,
                odt_objcprotocol:
                  includelen:=1;
                odt_object:
                  gen_objc_ivargc_recursive(tabstractrecordsymtable(tobjectdef(fs.vardef).symtable),ptrbset,startoffset+fs.fieldoffset,il);
              end;
            end;
        end;
      end;
end;


function tobjcrttiwriter_nonfragile.gen_objc_ivargcstring(objclss: tobjectdef; il: tivarlayouttype): ansistring;
  var
    ptrbset: tbitset;
    parent: tobjectdef;
    size,
    startoffset: puint;
    i: longint;
  begin
    size:=tObjectSymtable(objclss.symtable).datasize;
    if assigned(objclss.childof) then
      startoffset:=tObjectSymtable(objclss.childof.symtable).datasize
    else
      startoffset:=0;
    size:=size-startoffset;
    ptrbset:=tbitset.create_bytesize((size+sizeof(ptruint)-1) div sizeof(ptruint));
    { has to include info for this class' fields and those of all parent
      classes as well
    }
    parent:=obclss;
    repeat
      gen_objc_ivargc_recursive(parent.symtable,ptrbset,0,il);
      parent:=parent.childof;
    until not assigned(parent);
    { convert bits set to encoded string }
  end;
*)

(*
From Clang:
/// struct _class_ro_t {
///   uint32_t const flags;
///   uint32_t const instanceStart;
///   uint32_t const instanceSize;
///   uint32_t const reserved;  // only when building for 64bit targets
///   const uint8_t * const ivarLayout;
///   const char *const name;
///   const struct _method_list_t * const baseMethods;
///   const struct _protocol_list_t *const baseProtocols;
///   const struct _ivar_list_t *const ivars;
///   const uint8_t * const weakIvarLayout;
///   const struct _prop_list_t * const properties;
/// }
*)

procedure tobjcrttiwriter_nonfragile.gen_objc_class_ro_part(list: tasmlist; objclss: tobjectdef; protolistsym: TAsmSymbol; out classrolabel: tasmsymbol; metaclass: boolean);
  const
    CLS_CLASS        = 0;
    CLS_META         = 1;
    CLS_ROOT         = 2;
    OBJC2_CLS_HIDDEN = $10;
    CLS_EXCEPTION    = $20;
  var
    methodssym,
    rosym        : TAsmSymbol;
    classStrDef  : tdef;
    classStrSym,
    ivarslab     : TAsmLabel;
    rodef,
    class_type   : tdef;
    start,
    size,
    flags        : cardinal;
    rttitype     : trttitype;
    firstfield   : tfieldvarsym;
    i            : longint;
    tcb          : ttai_typedconstbuilder;
  begin
    { consider every class declared in the implementation section of a unit
      as "hidden"
    }
    flags:=0;
    if (objclss.owner.symtabletype=staticsymtable) and
       current_module.is_unit then
      flags:=OBJC2_CLS_HIDDEN;
    if metaclass then
      begin
        flags:=flags or CLS_META;
        rttitype:=objcmetarortti;
        { metaclass size/start: always size of objc_object }
        class_type:=search_named_unit_globaltype('OBJC','OBJC_OBJECT',true).typedef;
        start:=class_type.size;
        size:=start;
      end
    else
      begin
        flags:=flags or CLS_CLASS;
        rttitype:=objcclassrortti;
        size:=tObjectSymtable(objclss.symtable).datasize;
        { can't simply use childof's datasize, because alignment may cause the
          first field to skip a couple of bytes after the previous end }
        firstfield:=nil;
        for i:=0 to objclss.symtable.SymList.Count-1 do
          if (tsym(objclss.symtable.SymList[i]).typ=fieldvarsym) then
            begin
              firstfield:=tfieldvarsym(objclss.symtable.SymList[i]);
              break;
            end;
        if assigned(firstfield) then
          start:=firstfield.fieldoffset
        else
          { no extra fields -> start = size }
          start:=size;
      end;
    if not assigned(objclss.childof) then
      flags:=flags or CLS_ROOT;

    objcreatestringpoolentry(objclss.objextname^,sp_objcclassnames,sec_objc_class_names,classStrSym,classStrDef);
    { generate methods list }
    gen_objc_methods(list,objclss,methodssym,metaclass,false);
    { generate ivars (nil for metaclass) }
    if metaclass then
      ivarslab:=nil
    else
      gen_objc_ivars(list,objclss,ivarslab);

    { class declaration section }
    tcb:=ctai_typedconstbuilder.create([tcalo_new_section]);
    tcb.begin_anonymous_record(
      internaltypeprefixName[itb_objc_nf_class_ro_part],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    tcb.emit_ord_const(flags,u32inttype);
    tcb.emit_ord_const(start,u32inttype);
    tcb.emit_ord_const(size,u32inttype);
    { strong ivar layout for garbage collection (deprecated) }
    tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
    tcb.queue_init(voidpointertype);
    tcb.queue_emit_asmsym(classStrSym,classStrDef);
    ConcatSymOrNil(tcb,methodssym,voidpointertype);
    ConcatSymOrNil(tcb,protolistsym,voidpointertype);
    ConcatSymOrNil(tcb,ivarslab,voidpointertype);
    { weak ivar layout for garbage collection (deprecated) }
    tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
    { TODO: properties }
    tcb.emit_tai(tai_const.Create_nil_dataptr,voidpointertype);
    rodef:=tcb.end_anonymous_record;
    rosym:=current_asmdata.DefineAsmSymbol(objclss.rtti_mangledname(rttitype),AB_LOCAL,AT_DATA,rodef);
    list.concatList(
      tcb.get_final_asmlist(
        rosym,rodef,
        sec_objc_const,'_OBJC_META_CLASS',sizeof(pint)
      )
    );
    tcb.free;
    classrolabel:=rosym;
  end;


(*
From Clang:

/// struct _class_t {
///   struct _class_t *isa;
///   struct _class_t * const superclass;
///   void *cache;
///   IMP *vtable;
///   struct class_ro_t *ro;
/// }
///
*)

{ Generate rtti for an Objective-C class and its meta-class. }
procedure tobjcrttiwriter_nonfragile.gen_objc_classes_sections(list:TAsmList; objclss: tobjectdef; out classlabel: TAsmSymbol; out classlabeldef: tdef);
  var
    root          : tobjectdef;
    superSym,
    superMetaSym,
    metaisaSym,
    metasym,
    clssym,
    metarosym,
    rosym         : TAsmSymbol;
    protolistsym  : TAsmLabel;
    vis           : TAsmsymbind;
    isatcb,
    metatcb       : ttai_typedconstbuilder;
    metadef,
    classdef      : tdef;
  begin
    { A) Register necessary names }

    { 1) the current class and metaclass }
    if (objclss.owner.symtabletype=globalsymtable) then
      vis:=AB_GLOBAL
    else
      vis:=AB_PRIVATE_EXTERN;

    { create the typed const builders so we can get the (provisional) types
      for the class and metaclass symbols }
    isatcb:=ctai_typedconstbuilder.create([]);
    classdef:=isatcb.begin_anonymous_record(
      internaltypeprefixName[itb_objc_nf_class],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    metatcb:=ctai_typedconstbuilder.create([tcalo_new_section]);
    metadef:=metatcb.begin_anonymous_record(
      internaltypeprefixName[itb_objc_nf_meta_class],
      C_alignment,1,
      targetinfos[target_info.system]^.alignment.recordalignmin,
      targetinfos[target_info.system]^.alignment.maxCrecordalign);

    clssym:=current_asmdata.DefineAsmSymbol(objclss.rtti_mangledname(objcclassrtti),vis,AT_DATA,classdef);
    metasym:=current_asmdata.DefineAsmSymbol(objclss.rtti_mangledname(objcmetartti),vis,AT_DATA,metadef);
    { 2) the superclass and meta superclass }
    if assigned(objclss.childof) then
      begin
        superSym:=current_asmdata.RefAsmSymbol(objclss.childof.rtti_mangledname(objcclassrtti),AT_DATA);
        superMetaSym:=current_asmdata.RefAsmSymbol(objclss.childof.rtti_mangledname(objcmetartti),AT_DATA);
      end
    else
      begin
        superSym:=nil;
        { the class itself }
        superMetaSym:=clssym;
      end;

    { 3) the isa }
    { From Clang: The isa for the meta-class is the root of the hierarchy. }
    root:=objclss;
    while assigned(root.childof) do
      root:=root.childof;
    metaisaSym:=current_asmdata.RefAsmSymbol(root.rtti_mangledname(objcmetartti),AT_DATA);

    { 4) the implemented protocols (same for metaclass and regular class) }
    gen_objc_protocol_list(list,objclss.ImplementedInterfaces,protolistsym);

    { 5) the read-only parts of the class definitions }
    gen_objc_class_ro_part(list,objclss,protolistsym,metarosym,true);
    gen_objc_class_ro_part(list,objclss,protolistsym,rosym,false);

    { B) Class declaration section }
    { both class and metaclass are in the objc_data section for obj-c 2 }

    { 1) meta-class declaration }

    { the isa }
    metatcb.emit_tai(Tai_const.Create_sym(metaisaSym),voidpointertype);
    { the superclass }
    metatcb.emit_tai(Tai_const.Create_sym(superMetaSym),voidpointertype);
    { pointer to cache }
    if not assigned(ObjCEmptyCacheVar) then
      ObjCEmptyCacheVar:=current_asmdata.RefAsmSymbol(target_info.Cprefix+'_objc_empty_cache',AT_DATA);
    metatcb.emit_tai(Tai_const.Create_sym(ObjCEmptyCacheVar),voidpointertype);
    { pointer to vtable }
    if not assigned(ObjCEmptyVtableVar) and
       not(target_info.system in [system_arm_ios,system_aarch64_ios,system_aarch64_darwin,system_i386_iphonesim,system_x86_64_iphonesim]) then
      ObjCEmptyVtableVar:=current_asmdata.RefAsmSymbol(target_info.Cprefix+'_objc_empty_vtable',AT_DATA);
    ConcatSymOrNil(metatcb,ObjCEmptyVtableVar,voidpointertype);
    { the read-only part }
    metatcb.emit_tai(Tai_const.Create_sym(metarosym),voidpointertype);
    metatcb.end_anonymous_record;
    list.concatList(
      metatcb.get_final_asmlist(
        metasym,metadef,
        sec_objc_data,'_OBJC_CLASS',sizeof(pint)
      )
    );
    metatcb.free;

    { 2) regular class declaration }
    { the isa }
    isatcb.emit_tai(Tai_const.Create_sym(metasym),cpointerdef.getreusable(metadef));
    { the superclass }
    ConcatSymOrNil(isatcb,supersym,voidpointertype);
    { pointer to cache }
    isatcb.emit_tai(Tai_const.Create_sym(ObjCEmptyCacheVar),voidpointertype);
    { pointer to vtable }
    ConcatSymOrNil(isatcb,ObjCEmptyVtableVar,voidpointertype);
    { the read-only part }
    isatcb.emit_tai(Tai_const.Create_sym(rosym),voidpointertype);
    isatcb.end_anonymous_record;
    list.concatList(
      isatcb.get_final_asmlist(
        clssym,classdef,
        sec_objc_data,'_OBJC_CLASS',sizeof(pint)
      )
    );
    isatcb.free;

    classlabel:=clssym;
    classlabeldef:=classdef;;
  end;


procedure tobjcrttiwriter_nonfragile.addclasslist(list: tasmlist; section: tasmsectiontype; const symname: string; classes: tfpobjectlist);
  var
    i: longint;
    sym: TAsmSymbol;
    tcb: ttai_typedconstbuilder;
    arrdef: tdef;
  begin
    if classes.count=0 then
      exit;
    tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
    arrdef:=carraydef.getreusable(voidpointertype,classes.count);
    sym:=current_asmdata.DefineAsmSymbol(symname,AB_LOCAL,AT_DATA,arrdef);
    tcb.maybe_begin_aggregate(arrdef);
    for i:=0 to classes.count-1 do
      tcb.emit_tai(
        tai_const.Create_sym(current_asmdata.RefAsmSymbol(tobjectdef(classes[i]).rtti_mangledname(objcclassrtti),AT_DATA)),
        voidpointertype
      );
    tcb.maybe_end_aggregate(arrdef);
    list.concatList(
      tcb.get_final_asmlist(
        sym,arrdef,
        section,symname,sizeof(pint)
      )
    );
  end;


procedure tobjcrttiwriter_nonfragile.gen_objc_info_sections(list: tasmlist);

  function collectnonlazyclasses(classes: tfpobjectlist): tfpobjectlist;
    var
      symentry : tsym;
      procdef  : tprocdef;
      i,j      : longint;
    begin
      { non-lazy classes are all classes that define a class method with the
        selector called "load" (simply inheriting this class method is not enough,
        they have to implement it themselves)

        -- TODO: this currently only works if the Pascal identifier is also 'load'! }
      result:=tfpobjectlist.create(false);
      for i:=0 to classes.count-1 do
        begin
          symentry:=tsym(tobjectsymtable(tobjectdef(classes[i]).symtable).find('LOAD'));
          if assigned(symentry) and
             (symentry.typ=procsym) then
            begin
              for j:=0 to tprocsym(symentry).ProcdefList.count do
                begin
                  procdef:=tprocdef(tprocsym(symentry).ProcdefList[0]);
                  if ((po_classmethod in procdef.procoptions) and
                      (procdef.messageinf.str^='load')) then
                    begin
                      result.add(classes[i]);
                      break;
                    end;
                end;
            end;
        end;
    end;

  var
    nonlazyclasses,
    nonlazycategories : tfpobjectlist;
  begin
    if (classdefs.count=0) and
       (catdefs.count=0) then
      exit;

    nonlazyclasses:=collectnonlazyclasses(classdefs);
    nonlazycategories:=collectnonlazyclasses(catdefs);

    { this list has to include all classes, also the non-lazy ones }
    addclasslist(list,sec_objc_classlist,target_asm.labelprefix+'_OBJC_LABEL_CLASS_$',classdefs);
    addclasslist(list,sec_objc_nlclasslist,target_asm.labelprefix+'_OBJC_LABEL_NONLAZY_CLASS_$',nonlazyclasses);
    { category and non-lazy category lists }
    addclasslist(list,sec_objc_catlist,target_asm.labelprefix+'_OBJC_LABEL_CATEGORY_$',catdefs);
    addclasslist(list,sec_objc_nlcatlist,target_asm.labelprefix+'_OBJC_LABEL_NONLAZY_CATEGORY_$',nonlazycategories);

    nonlazyclasses.free;
    nonlazycategories.free;
    { the non-fragile abi doesn't have any module info, nor lazy references
      to used classes or to parent classes }
  end;


constructor tobjcrttiwriter_nonfragile.create;
  begin
    inherited create(oa_nonfragile);
  end;


{******************************************************************
                 RTTI generation -- Main function
*******************************************************************}

procedure MaybeGenerateObjectiveCImageInfo(globalst, localst: tsymtable);
  var
    objcrttiwriter: tobjcrttiwriter;
    tcb: ttai_typedconstbuilder;
  begin
    if (m_objectivec1 in current_settings.modeswitches) then
      begin
        { first 4 bytes contain version information about this section (currently version 0),
          next 4 bytes contain flags (currently only regarding whether the code in the object
          file supports or requires garbage collection)
        }
        tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_no_dead_strip]);
        tcb.emit_ord_const(0,u64inttype);
        current_asmdata.asmlists[al_objc_data].concatList(
          tcb.get_final_asmlist(
            current_asmdata.DefineAsmSymbol(target_asm.labelprefix+'_OBJC_IMAGE_INFO',AB_LOCAL,AT_DATA,u64inttype),
            u64inttype,sec_objc_image_info,'_OBJC_IMAGE_INFO',sizeof(pint)
          )
        );
        tcb.free;

        { generate rtti for all obj-c classes, protocols and categories
          defined in this module. }
        if not(target_info.system in systems_objc_nfabi) then
          objcrttiwriter:=tobjcrttiwriter_fragile.create
        else
          objcrttiwriter:=tobjcrttiwriter_nonfragile.create;
        objcrttiwriter.gen_objc_rtti_sections(current_asmdata.asmlists[al_objc_data],globalst);
        objcrttiwriter.gen_objc_rtti_sections(current_asmdata.asmlists[al_objc_data],localst);
        objcrttiwriter.gen_objc_info_sections(current_asmdata.asmlists[al_objc_data]);
        objcrttiwriter.free;
      end;
  end;


end.

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
    symbase;

  procedure objcfinishstringrefpoolentry(entry: phashsetitem; stringpool: tconstpooltype; refsec, stringsec: tasmsectiontype);

  procedure MaybeGenerateObjectiveCImageInfo(globalst, localst: tsymtable);


implementation

  uses
    globtype,globals,
    systems,
    aasmtai,
    cgbase,cgutils,
    objcutil,
    symconst,symtype,symsym,symdef,symtable,
    verbose;

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
    if current_asmdata.ConstPools[sp_objcprotocolrefs]=nil then
      current_asmdata.ConstPools[sp_objcprotocolrefs]:=THashSet.Create(64, True, False);

    item:=current_asmdata.constpools[sp_objcprotocolrefs].FindOrAdd(@p[1], length(p));
    Result:=(item^.Data=nil);
    if Result then
      item^.Data:=ref;
  end;

{******************************************************************
                       String section helpers
*******************************************************************}

function objcreatestringpoolentryintern(p: pchar; len: longint; pooltype: tconstpooltype; stringsec: tasmsectiontype): TAsmSymbol;
  var
    entry  : PHashSetItem;
    strlab : tasmlabel;
    pc     : pchar;
    pool   : THashSet;
  begin
    if current_asmdata.ConstPools[pooltype]=nil then
       current_asmdata.ConstPools[pooltype]:=THashSet.Create(64, True, False);
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
        new_section(current_asmdata.asmlists[al_objc_pools],stringsec,strlab.name,sizeof(pint));
        current_asmdata.asmlists[al_objc_pools].concat(Tai_label.Create(strlab));
        current_asmdata.asmlists[al_objc_pools].concat(Tai_string.Create_pchar(pc,entry^.keylength+1));
        Result := strlab;
      end
    else
      Result := TAsmLabel(Entry^.Data);
  end;


procedure objcfinishstringrefpoolentry(entry: phashsetitem; stringpool: tconstpooltype; refsec, stringsec: tasmsectiontype);
  var
    reflab : tasmlabel;
    strlab : tasmsymbol;
    classname: string;
  begin
    { have we already generated a reference for this string entry? }
    if not assigned(entry^.Data) then
      begin
        { no, add the string to the associated strings section }
        strlab:=objcreatestringpoolentryintern(pchar(entry^.key),entry^.keylength,stringpool,stringsec);

        { and now finish the reference }
        current_asmdata.getlabel(reflab,alt_data);
        entry^.Data:=reflab;

        { add a pointer to the string in the string references section }
        new_section(current_asmdata.asmlists[al_objc_pools],refsec,reflab.name,sizeof(pint));
        current_asmdata.asmlists[al_objc_pools].concat(Tai_label.Create(reflab));
        current_asmdata.asmlists[al_objc_pools].concat(Tai_const.Create_sym(strlab));

        { in case of a class reference, also add a lazy symbol reference for
          the class (the linker requires this). }
        if (refsec=sec_objc_cls_refs) then
          begin
            setlength(classname,entry^.keylength);
            move(entry^.key^,classname[1],entry^.keylength);
            current_asmdata.asmlists[al_objc_pools].concat(tai_directive.Create(asd_lazy_reference,'.objc_class_name_'+classname));
          end;
      end;
  end;


function objcreatestringpoolentry(const s: string; pooltype: tconstpooltype; stringsec: tasmsectiontype): TAsmSymbol;
  begin
    result:=objcreatestringpoolentryintern(@s[1],length(s),pooltype,stringsec);
  end;


{******************************************************************
                        RTTI generation
*******************************************************************}

procedure ConcatSymOrNil(list: tasmlist; sym: TAsmSymbol); inline;
begin
  if Assigned(sym) then
    list.Concat(tai_const.Create_sym(sym))
  else
    list.Concat(tai_const.Create_pint(0));
end;


{ generate a method list, either of class methods or of instance methods,
  and both for obj-c classes and categories. }
procedure gen_objc1_methods(list: tasmlist; objccls: tobjectdef; out methodslabel: tasmlabel; classmethods, iscategory: Boolean);
  const
    clsSectType : array [Boolean] of tasmsectiontype = (sec_objc_inst_meth, sec_objc_cls_meth);
    clsSectName : array [Boolean] of string = ('_OBJC_INST_METH','_OBJC_CLS_METH');
    catSectType : array [Boolean] of tasmsectiontype = (sec_objc_cat_inst_meth, sec_objc_cat_cls_meth);
    catSectName : array [Boolean] of string = ('_OBJC_CAT_INST_METH','_OBJC_CAT_CLS_METH');
  type
    method_data = record
      def     : tprocdef;
      selsym  : TAsmSymbol;
      encsym  : TAsmSymbol;
    end;
  var
    i     : Integer;
    def   : tprocdef;
    defs  : array of method_data;
    mcnt  : integer;
    sym   : tasmsymbol;
  begin
    methodslabel:=nil;
    mcnt:=0;
    { collect all instance/class methods }
    SetLength(defs,objccls.vmtentries.count);
    for i:=0 to objccls.vmtentries.count-1 do
      begin
        def:=pvmtentry(objccls.vmtentries[i])^.procdef;
        if Assigned(def.procstarttai) and
           (classmethods = (po_classmethod in def.procoptions)) then
          begin
            defs[mcnt].def:=def;
            defs[mcnt].selsym:=objcreatestringpoolentry(def.messageinf.str^,sp_objcvarnames,sec_objc_meth_var_names);
            defs[mcnt].encsym:=objcreatestringpoolentry(objcencodemethod(def),sp_objcvartypes,sec_objc_meth_var_types);
            inc(mcnt);
          end;
      end;
    if mcnt=0 then
      exit;

    if iscategory then
      new_section(list,clsSectType[classmethods],clsSectName[classmethods],4)
    else
      new_section(list,catSectType[classmethods],catSectName[classmethods],4);

    current_asmdata.getlabel(methodslabel,alt_data);
    list.Concat(tai_label.Create(methodslabel));

    { not used, always zero }
    list.Concat(tai_const.Create_32bit(0));
    { number of objc_method entries in the method_list array }
    list.Concat(tai_const.Create_32bit(mcnt));
    for i:=0 to mcnt-1 do
      begin
        { reference to the selector name }
        list.Concat(tai_const.Create_sym(defs[i].selsym));
        { reference to the obj-c encoded function parameters (signature) }
        list.Concat(tai_const.Create_sym(defs[i].encsym));
        { mangled name of the method }
        sym:=current_asmdata.GetAsmSymbol(defs[i].def.mangledname);
        if not assigned(sym) then
          internalerror(2009091601);
        list.Concat(tai_const.Create_sym(sym));
      end;
  end;


{ generate an instance variables list for an obj-c class. }
procedure gen_objc1_ivars(list: TAsmList; objccls: tobjectdef; out ivarslabel: TAsmLabel);
  type
    ivar_data = record
      vf      : tfieldvarsym;
      namesym : TAsmSymbol;
      typesym : TAsmSymbol;
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
    setLength(vars,objccls.symtable.SymList.Count);

    for i:=0 to objccls.symtable.SymList.Count-1 do
      if tsym(objccls.symtable.SymList[i]).typ=fieldvarsym then
        begin
          vf:=tfieldvarsym(objccls.symtable.SymList[i]);
          if objctryencodetype(vf.vardef,enctype,encerr) then
            begin
              vars[vcnt].vf:=vf;
              vars[vcnt].namesym:=objcreatestringpoolentry(vf.RealName,sp_objcvarnames,sec_objc_meth_var_names);
              vars[vcnt].typesym:=objcreatestringpoolentry(enctype,sp_objcvartypes,sec_objc_meth_var_types);
              inc(vcnt);
            end
          else
            { must be caught during parsing }
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


{ Generate rtti for an Objective-C methods (methods without implementation) }
{ items : TFPObjectList of Tprocdef }
procedure gen_objc1_cat_methods(list:TAsmList; items: TFPObjectList; section: tasmsectiontype;
  const sectname: string; out listsym: TAsmLabel);
var
  i   : integer;
  m   : tprocdef;
begin
  if not assigned(items) or
     (items.count=0) then
    exit;

  new_section(list, section, sectname, sizeof(pint));
  current_asmdata.getlabel(listsym,alt_data);
  list.Concat(tai_label.Create(listsym));
  list.Concat(Tai_const.Create_32bit(items.count));
  for i:=0 to items.Count-1 do
    begin
      m:=tprocdef(items[i]);
      list.Concat(Tai_const.Create_sym(
        objcreatestringpoolentry(m.messageinf.str^,sp_objcvarnames,sec_objc_meth_var_names)));
      list.Concat(Tai_const.Create_sym(
        objcreatestringpoolentry(objcencodemethod(m),sp_objcvartypes,sec_objc_meth_var_types)));
    end;
end;

procedure gen_objc1_protocol_list(list:TAsmList; protolist: TFPObjectList; out protolistsym: TAsmLabel); forward;

{ Generate rtti for an Objective-C protocol  }
procedure gen_objc1_protocol(list:TAsmList; protocol: tobjectdef; out protocollabel: TAsmSymbol);
  var
    namesym,
    instlistsym,
    clslistsym  : TAsmSymbol;
    i           : Integer;
    protolist   : TAsmLabel;
    proc        : tprocdef;
    instmlist,
    clsmlist    : TFPObjectList;
    instsym,
    clssym,
    lbl          : TAsmLabel;
  begin
    instmlist:=TFPObjectList.Create(false);
    clsmlist:=TFPObjectList.Create(false);
    for i:=0 to protocol.vmtentries.Count-1 do
      begin
        proc:=pvmtentry(protocol.vmtentries[i])^.procdef;
        if (po_classmethod in proc.procoptions) then
          clsmlist.Add(proc)
        else
          instmlist.Add(proc);
      end;
      if instmlist.Count > 0 then
        gen_objc1_cat_methods(list,instmlist,sec_objc_cat_inst_meth,'_OBJC_CAT_INST_METH',instsym)
      else
        instsym:=nil;

      if clsmlist.Count>0 then
        gen_objc1_cat_methods(list,clsmlist,sec_objc_cat_cls_meth,'_OBJC_CAT_CLS_METH',clssym)
      else
        clssym:=nil;

    instmlist.Free;
    clsmlist.Free;

    gen_objc1_protocol_list(list,protocol.ImplementedInterfaces,protolist);

    new_section(list, sec_objc_protocol,'_OBJC_PROTOCOL',sizeof(pint));
    current_asmdata.getlabel(lbl,alt_data);
    list.Concat(tai_label.Create(lbl));
    protocollabel:=lbl;

    { protocol's isa - always nil }
    list.Concat(Tai_const.Create_pint(0));
    { name }
    namesym:=objcreatestringpoolentry(protocol.objextname^,sp_objcclassnames,sec_objc_class_names);
    list.Concat(Tai_const.Create_sym(namesym));
    { protocol's list }
    ConcatSymOrNil(list,protolist);
    { instance methods, in __cat_inst_meth }
    ConcatSymOrNil(list,instsym);
    { class methods, in __cat_cls_meth }
    ConcatSymOrNil(list,clssym);
  end;

(*
From CLang:

  struct objc_protocol_list
  {
      struct objc_protocol_list *next;
      int count;
      Protocol *list[1];
  };
*)
procedure gen_objc1_protocol_list(list:TAsmList; protolist: TFPObjectList; out protolistsym: TAsmLabel);
  var
    i         : Integer;
    protosym  : TAsmSymbol;
    protodef  : tobjectdef;
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
            gen_objc1_protocol(list,protodef,protosym);
            objcaddprotocolentry(protodef.objextname^,protosym);
          end;
      end;

    { protocol lists are stored in .objc_cat_cls_meth section }
    new_section(list,sec_objc_cat_cls_meth,'_OBJC_PROTOCOLLIST',sizeof(pint));
    current_asmdata.getlabel(protolistsym, alt_data);
    list.Concat(tai_label.Create(protolistsym));

    { From Clang: next, always nil}
    list.Concat(tai_const.Create_pint(0));
    { From Clang: protocols count}
    list.Concat(Tai_const.Create_32bit(protolist.Count));
    for i:=0 to protolist.Count-1 do
      begin
        protodef:=(protolist[i] as TImplementedInterface).IntfDef;
        protosym:=objcfindprotocolentry(protodef.objextname^);
        if not Assigned(protosym) then
          begin
            { For some reason protosym is not declared, though must be!
              Probably gen_obcj1_protocl returned wrong protosym
            }
            InternalError(2009091602);
          end;
        list.Concat(tai_const.Create_sym(protosym));
      end;
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
procedure gen_objc1_classes_sections(list:TAsmList; objclss: tobjectdef; out classlabel: TAsmSymbol);
  const
    CLS_CLASS = 1;
    CLS_META  = 2;
    META_INST_SIZE = 40+8; // sizeof(objc_class) + 8
  var
    root          : tobjectdef;
    superStrSym,
    classStrSym,
    metaisaStrSym,
    metasym,
    clssym        : TAsmSymbol;
    mthdlist,
    ivarslist,
    protolistsym  : TAsmLabel;
  begin
    { generate the class methods list }
    gen_objc1_methods(list,objclss,mthdlist,true,false);

    { generate implemented protocols list }
    gen_objc1_protocol_list(list,objclss.ImplementedInterfaces,protolistsym);

    { register necessary names }
    { 1) the superclass }
    if assigned(objclss.childof) then
      superStrSym:=objcreatestringpoolentry(objclss.childof.objextname^,sp_objcclassnames,sec_objc_class_names)
    else
      { not empty string, but nil! }
      superStrSym:=nil;

    { 2) the current class }
    classStrSym:=objcreatestringpoolentry(objclss.objextname^,sp_objcclassnames,sec_objc_class_names);
    { 3) the isa }
    { From Clang: The isa for the meta-class is the root of the hierarchy. }
    root:=objclss;
    while assigned(root.childof) do
      root:=root.childof;
    metaisaStrSym:=objcreatestringpoolentry(root.objextname^,sp_objcclassnames,sec_objc_class_names);

    { class declaration section }
    new_section(list,sec_objc_meta_class,'_OBJC_META_CLASS',sizeof(pint));

    { 1) meta-class declaration (warning: if name changed, also change tclassrefdef.rtti_mangledname) }
    metasym:=current_asmdata.DefineAsmSymbol(target_asm.labelprefix+'_OBJC_METACLASS_'+objclss.objextname^,AB_LOCAL,AT_DATA);
    list.Concat(tai_symbol.Create(metasym,0));

    list.Concat(Tai_const.Create_sym(metaisaStrSym));
    { pointer to the superclass name if any, otherwise nil }
    if assigned(superstrsym) then
      list.Concat(Tai_const.Create_sym(superStrSym))
    else
      list.concat(tai_const.create_32bit(0));
    { pointer to the class name }
    list.Concat(Tai_const.Create_sym(classStrSym));

    { version is always 0 currently }
    list.Concat(Tai_const.Create_32bit(0));
    { CLS_META for meta-classes }
    list.Concat(Tai_const.Create_32bit(CLS_META));
    { size of the meta-class instance: sizeof(objc_class) + 8 bytes }
    list.Concat(Tai_const.Create_32bit(META_INST_SIZE) );
    { meta-classes don't have ivars list (=0) }
    list.Concat(Tai_const.Create_32bit(0));
    { class methods list (stored in "__cls_meth" section) }
    if Assigned(mthdlist) then
      list.Concat(Tai_const.Create_sym(mthdlist))
    else
      list.Concat(Tai_const.Create_32bit(0));
    { From Clang: cache is always nil }
    list.Concat(Tai_const.Create_32bit(0));
    { protocols }
    ConcatSymOrNil(list, protolistsym);
    { From Clang: ivar_layout for meta-class is always NULL. }
    list.Concat(Tai_const.Create_32bit(0));
    { From Clang: The class extension is always unused for meta-classes. }
    list.Concat(Tai_const.Create_32bit(0));

    { 2) regular class declaration }

    { generate the instance methods list }
    gen_objc1_methods(list,objclss,mthdlist,false,false);
    { generate the instance variables list }
    gen_objc1_ivars(list,objclss,ivarslist);

    new_section(list,sec_objc_class,'_OBJC_CLASS',sizeof(pint));

    clssym:=current_asmdata.DefineAsmSymbol(objclss.rtti_mangledname(fullrtti),AB_LOCAL,AT_DATA);
    list.Concat(tai_symbol.Create(clssym,0));

    { for class declaration: the is points to the meta-class declaration }
    list.Concat(Tai_const.Create_sym(metasym));
    { pointer to the super_class name if any, nil otherwise }
    if assigned(superStrSym) then
      list.Concat(Tai_const.Create_sym(superStrSym))
    else
      list.Concat(Tai_const.Create_32bit(0));
    { pointer to the class name }
    list.Concat(Tai_const.Create_sym(classStrSym));
    { version is always 0 currently }
    list.Concat(Tai_const.Create_32bit(0));
    { CLS_CLASS for classes }
    list.Concat(Tai_const.Create_32bit(CLS_CLASS));
    { size of instance: total size of instance variables }
    list.Concat(Tai_const.Create_32bit(tobjectsymtable(objclss.symtable).datasize));
    { objc_ivar_list (stored in "__instance_vars" section) }
    if assigned(ivarslist) then
      list.Concat(Tai_const.Create_sym(ivarslist))
    else
      list.Concat(tai_const.create_32bit(0));
    { instance methods list (stored in "__inst_meth" section) }
    if Assigned(mthdlist) then
      list.Concat(Tai_const.Create_sym(mthdlist))
    else
      list.Concat(Tai_const.Create_32bit(0));
    { From Clang: cache is always NULL }
    list.Concat(Tai_const.Create_32bit(0));
    { protocols, protolistsym has been created for meta-class, no need to create another one}
    ConcatSymOrNil(list, protolistsym);
    { TODO: From Clang: strong ivar_layout, necessary for garbage collection support }
    list.Concat(Tai_const.Create_32bit(0));
    { TODO: From Clang: weak ivar_layout, necessary for garbage collection support }
    list.Concat(Tai_const.Create_32bit(0));

    classlabel:=clssym;
  end;


{ Generate the rtti sections for all obj-c classes defined in st, and return
  these classes in the classes list. }
procedure gen_objc1_rtti_sections(list:TAsmList; st:TSymtable; var classsyms, classdefs: tfpobjectlist);
  var
    i: longint;
    def: tdef;
    sym : TAsmSymbol;
  begin
    if not Assigned(st) then
      exit;

    for i:=0 to st.DefList.Count-1 do
      begin
        def:=tdef(st.DefList[i]);
        if is_objcclass(def) and
           not(oo_is_external in tobjectdef(def).objectoptions) then
          begin
            gen_objc1_classes_sections(list,tobjectdef(def),sym);
            classsyms.add(sym);
            classdefs.add(def);
          end;
      end;
  end;


{ Generate the global information sections (objc_symbols and objc_module_info)
  for this module. }
procedure gen_objc1_info_sections(list: tasmlist; classsyms,classdefs: tfpobjectlist);
  var
    i: longint;
    sym : TAsmSymbol;
    parent: tobjectdef;
    superclasses: tfpobjectlist;
  begin
    if (classsyms.count<>0) then
      begin
        new_section(list,sec_objc_symbols,'_OBJC_SYMBOLS',sizeof(pint));
        sym := current_asmdata.RefAsmSymbol(target_asm.labelprefix+'_OBJC_SYMBOLS');

        { symbol to refer to this information }
        list.Concat(tai_symbol.Create(sym,0));
        { ??? (always 0 in Clang) }
        list.Concat(Tai_const.Create_pint(0));
        { ??? (From Clang: always 0, pointer to some selector) }
        list.Concat(Tai_const.Create_pint(0));
        { From Clang: number of defined classes }
        list.Concat(Tai_const.Create_16bit(classsyms.count));
        { From Clang: number of defined categories }
        list.Concat(Tai_const.Create_16bit(0));
        { first all classes }
        for i:=0 to classsyms.count-1 do
          list.Concat(Tai_const.Create_sym(tasmsymbol(classsyms[i])));
        { then all categories }
     end
    else
      sym:=nil;

    new_section(list,sec_objc_module_info,'_OBJC_MODULE_INFO',4);
    { version number = 7 (always, both for gcc and clang, regardless of objc-1 or 2 }
    list.Concat(Tai_const.Create_pint(7));
    { sizeof(objc_module): 4 pointer-size entities }
    list.Concat(Tai_const.Create_pint(sizeof(pint)*4));
    { used to be file name, now unused (points to empty string) }
    list.Concat(Tai_const.Create_sym(objcreatestringpoolentry('',sp_objcclassnames,sec_objc_class_names)));
    { pointer to classes/categories list declared in this module }
    if assigned(sym) then
      list.Concat(Tai_const.Create_sym(sym))
    else
      list.concat(tai_const.create_pint(0));

    { Add lazy references to parent classes of all classes defined in this unit }
    superclasses:=tfpobjectlist.create(false);
    for i:=0 to classdefs.count-1 do
      begin
        parent:=tobjectdef(classdefs[i]).childof;
        if assigned(parent) and
           (superclasses.indexof(parent)=-1) then
          begin
            list.concat(tai_directive.create(asd_lazy_reference,'.objc_class_name_'+parent.objextname^));
            superclasses.add(parent);
          end;
      end;
    superclasses.free;
    { reference symbols for all classes declaredin this unit }
    for i:=0 to classdefs.count-1 do
      list.concat(tai_symbol.Createname_global_value('.objc_class_name_'+tobjectdef(classdefs[i]).objextname^,AT_DATA,0,0));
  end;


procedure MaybeGenerateObjectiveCImageInfo(globalst, localst: tsymtable);
  var
    classsyms,
    classdefs: tfpobjectlist;
  begin
    if (m_objectivec1 in current_settings.modeswitches) then
      begin
        { first 4 bytes contain version information about this section (currently version 0),
          next 4 bytes contain flags (currently only regarding whether the code in the object
          file supports or requires garbage collection)
        }
        new_section(current_asmdata.asmlists[al_objc_data],sec_objc_image_info,'_OBJC_IMAGE_INFO',sizeof(pint));
        current_asmdata.asmlists[al_objc_data].concat(Tai_symbol.Createname(target_asm.labelprefix+'_OBJC_IMAGE_INFO',AT_LABEL,sizeof(pint)));
        current_asmdata.asmlists[al_objc_data].concat(Tai_const.Create_64bit(0));

        { generate rtti for all obj-c classes, protocols (todo) and categories (todo)
          defined in this module. }
        classsyms:=tfpobjectlist.create(false);
        classdefs:=tfpobjectlist.create(false);
        gen_objc1_rtti_sections(current_asmdata.asmlists[al_objc_data],globalst,classsyms,classdefs);
        gen_objc1_rtti_sections(current_asmdata.asmlists[al_objc_data],localst,classsyms,classdefs);
        gen_objc1_info_sections(current_asmdata.asmlists[al_objc_data],classsyms,classdefs);
        classsyms.free;
        classdefs.free;
      end;
  end;

end.

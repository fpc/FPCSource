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
    pc     : pchar;
  begin
    { have we already generated a reference for this string entry? }
    if not assigned(entry^.Data) then
      begin
        { no, add the string to the associated strings section }
        strlab:=objcreatestringpoolentryintern(pchar(entry^.key),entry^.keylength,stringpool,stringsec);

        { and now finish the reference }
        current_asmdata.getlabel(reflab,alt_data);
        entry^.Data:=reflab;
        getmem(pc,entry^.keylength+1);
        move(entry^.key^,pc^,entry^.keylength);
        pc[entry^.keylength]:=#0;
        { add a pointer to the message name in the string references section }
        new_section(current_asmdata.asmlists[al_objc_pools],refsec,reflab.name,sizeof(pint));
        current_asmdata.asmlists[al_objc_pools].concat(Tai_label.Create(reflab));
        current_asmdata.asmlists[al_objc_pools].concat(Tai_const.Create_sym(strlab));
    end;
  end;



function objcreatestringpoolentry(const s: string; pooltype: tconstpooltype; stringsec: tasmsectiontype): TAsmSymbol;
  begin
    result:=objcreatestringpoolentryintern(@s[1],length(s),pooltype,stringsec);
  end;


{******************************************************************
                        RTTI generation
*******************************************************************}

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
    for i := 0 to mcnt - 1 do
      begin
        { reference to the selector name }
        list.Concat(tai_const.Create_sym(defs[i].selsym));
        { reference to the obj-c encoded function parameters (signature) }
        list.Concat(tai_const.Create_sym(defs[i].encsym));
        { mangled name of the method }
        list.Concat(tai_const.Create_sym(
          current_asmdata.GetAsmSymbol(defs[i].def.objcmangledname)));
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
    lbl, metalbl  : TAsmLabel;
    superStrSym,
    classStrSym,
    metaisaStrSym : TAsmSymbol;
    mthdlist,
    ivarslist     : TAsmLabel;
  begin
    { generate the class methods list }
    gen_objc1_methods(list,objclss,mthdlist,true,false);

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

    { 1) meta-class declaration }
    current_asmdata.getlabel(metalbl,alt_data);
    list.Concat(tai_label.Create(metalbl));

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
    { TODO: protocols }
    list.Concat(Tai_const.Create_32bit(0));
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

    current_asmdata.getlabel(lbl,alt_data);
    list.Concat(tai_label.Create(lbl));

    { for class declaration: the is points to the meta-class declaration }
    list.Concat(Tai_const.Create_sym(metalbl));
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
    { TODO: protocols }
    list.Concat(Tai_const.Create_32bit(0));
    { TODO: From Clang: strong ivar_layout, necessary for garbage collection support }
    list.Concat(Tai_const.Create_32bit(0));
    { TODO: From Clang: weak ivar_layout, necessary for garbage collection support }
    list.Concat(Tai_const.Create_32bit(0));

    classlabel:=lbl;
  end;


{ Generate the rtti sections for all obj-c classes defined in st, and return
  these classes in the classes list. }
procedure gen_objc1_rtti_sections(list:TAsmList; st:TSymtable; var classes: tfpobjectlist);
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
            classes.add(sym);
          end;
      end;
  end;


{ Generate the global information sections (objc_symbols and objc_module_info)
  for this module. }
procedure gen_objc1_info_sections(list: tasmlist; classes: tfpobjectlist);
  var
    i: longint;
    sym : TAsmSymbol;
  begin
    if (classes.count<>0) then
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
        list.Concat(Tai_const.Create_16bit(classes.count));
        { From Clang: number of defined categories }
        list.Concat(Tai_const.Create_16bit(0));
        { first all classes }
        for i:=0 to classes.count-1 do
          list.Concat(Tai_const.Create_sym(tasmsymbol(classes[i])));
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
  end;


procedure MaybeGenerateObjectiveCImageInfo(globalst, localst: tsymtable);
  var
    classes: tfpobjectlist;
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
        classes:=tfpobjectlist.create(false);
        gen_objc1_rtti_sections(current_asmdata.asmlists[al_objc_data],globalst,classes);
        gen_objc1_rtti_sections(current_asmdata.asmlists[al_objc_data],localst,classes);
        gen_objc1_info_sections(current_asmdata.asmlists[al_objc_data],classes);
        classes.free;
      end;
  end;


end.

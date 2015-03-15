{
    Copyright (c) 1998-2011 by Florian Klaempfl

    Generic version of some node tree helper routines that can be overridden
    by cpu-specific versions

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
unit ngenutil;

{$i fpcdefs.inc}

interface

  uses
    cclasses,globtype,
    aasmdata,
    node,nbas,symtype,symsym,symconst,symdef;


  type
    tnodeutils = class
      class function call_fail_node:tnode; virtual;
      class function initialize_data_node(p:tnode; force: boolean):tnode; virtual;
      class function finalize_data_node(p:tnode):tnode; virtual;
      { returns true if the unit requires an initialisation section (e.g.,
        to force class constructors for the JVM target to initialise global
        records/arrays) }
      class function force_init: boolean; virtual;
      { idem for finalization }
      class function force_final: boolean; virtual;

      { called after parsing a routine with the code of the entire routine
        as argument; can be used to modify the node tree. By default handles
        insertion of code for systems that perform the typed constant
        initialisation via the node tree }
      class function wrap_proc_body(pd: tprocdef; n: tnode): tnode; virtual;

      { trashes a paravarsym or localvarsym if possible (not a managed type,
        "out" in case of parameter, ...) }
      class procedure maybe_trash_variable(var stat: tstatementnode; p: tabstractnormalvarsym; trashn: tnode); virtual;
     strict protected
      { called from wrap_proc_body to insert the trashing for the wrapped
        routine's local variables and parameters }
      class function  maybe_insert_trashing(pd: tprocdef; n: tnode): tnode;
      class function  check_insert_trashing(pd: tprocdef): boolean; virtual;
      { callback called for every local variable and parameter by
        maybe_insert_trashing(), calls through to maybe_trash_variable() }
      class procedure maybe_trash_variable_callback(p: TObject; statn: pointer);
      { returns whether a particular sym can be trashed. If not,
        maybe_trash_variable won't do anything }
      class function  trashable_sym(p: tsym): boolean; virtual;
      { trashing for 1/2/3/4/8-byte sized variables }
      class procedure trash_small(var stat: tstatementnode; trashn: tnode; trashvaln: tnode); virtual;
      { trashing for differently sized variables that those handled by
        trash_small() }
      class procedure trash_large(var stat: tstatementnode; trashn, sizen: tnode; trashintval: int64); virtual;
      { insert a single bss sym, called by insert bssdata (factored out
        non-common part for llvm) }
      class procedure insertbsssym(list: tasmlist; sym: tstaticvarsym; size: asizeint; varalign: shortint); virtual;

      { initialization of iso styled program parameters }
      class procedure initialize_textrec(p : TObject; statn : pointer);
      { finalization of iso styled program parameters }
      class procedure finalize_textrec(p : TObject; statn : pointer);
     public
      class procedure insertbssdata(sym : tstaticvarsym); virtual;

      class function create_main_procdef(const name: string; potype:tproctypeoption; ps: tprocsym):tdef; virtual;
      class procedure InsertInitFinalTable; virtual;
     protected
      class procedure InsertRuntimeInits(const prefix:string;list:TLinkedList;unitflag:cardinal); virtual;
      class procedure InsertRuntimeInitsTablesTable(const prefix,tablename:string;unitflag:cardinal); virtual;
     public
      class procedure InsertThreadvarTablesTable; virtual;
      class procedure InsertThreadvars; virtual;
      class procedure InsertWideInitsTablesTable; virtual;
      class procedure InsertWideInits; virtual;
      class procedure InsertResStrInits; virtual;
      class procedure InsertResStrTablesTable; virtual;
      class procedure InsertResourceTablesTable; virtual;
      class procedure InsertResourceInfo(ResourcesUsed : boolean); virtual;

      class procedure InsertMemorySizes; virtual;

     strict protected
      class procedure add_main_procdef_paras(pd: tdef); virtual;
    end;
    tnodeutilsclass = class of tnodeutils;

  const
    cnodeutils: tnodeutilsclass = tnodeutils;


implementation

    uses
      verbose,version,globals,cutils,constexp,
      scanner,systems,procinfo,fmodule,
      aasmbase,aasmtai,
      symbase,symtable,defutil,
      nadd,ncal,ncnv,ncon,nflw,ninl,nld,nmem,nobj,nutils,
      ppu,
      pass_1;

  class function tnodeutils.call_fail_node:tnode;
    var
      para : tcallparanode;
      newstatement : tstatementnode;
      srsym : tsym;
    begin
      result:=internalstatements(newstatement);

      { call fail helper and exit normal }
      if is_class(current_structdef) then
        begin
          srsym:=search_struct_member(current_structdef,'FREEINSTANCE');
          if assigned(srsym) and
             (srsym.typ=procsym) then
            begin
              { if self<>0 and vmt<>0 then freeinstance }
              addstatement(newstatement,cifnode.create(
                  caddnode.create(andn,
                      caddnode.create(unequaln,
                          load_self_pointer_node,
                          cnilnode.create),
                      caddnode.create(unequaln,
                          load_vmt_pointer_node,
                          cnilnode.create)),
                  ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                  nil));
            end
          else
            internalerror(200305108);
        end
      else
        if is_object(current_structdef) then
          begin
            { parameter 3 : vmt_offset }
            { parameter 2 : pointer to vmt }
            { parameter 1 : self pointer }
            para:=ccallparanode.create(
                      cordconstnode.create(tobjectdef(current_structdef).vmt_offset,s32inttype,false),
                  ccallparanode.create(
                      ctypeconvnode.create_internal(
                          load_vmt_pointer_node,
                          voidpointertype),
                  ccallparanode.create(
                      ctypeconvnode.create_internal(
                          load_self_pointer_node,
                          voidpointertype),
                  nil)));
            addstatement(newstatement,
                ccallnode.createintern('fpc_help_fail',para));
          end
      else
        internalerror(200305132);
      { self:=nil }
      addstatement(newstatement,cassignmentnode.create(
          load_self_pointer_node,
          cnilnode.create));
      { exit }
      addstatement(newstatement,cexitnode.create(nil));
    end;


  class function tnodeutils.initialize_data_node(p:tnode; force: boolean):tnode;
    begin
      if not assigned(p.resultdef) then
        typecheckpass(p);
      if is_ansistring(p.resultdef) or
         is_wide_or_unicode_string(p.resultdef) or
         is_interfacecom_or_dispinterface(p.resultdef) or
         is_dynamic_array(p.resultdef) then
        begin
          result:=cassignmentnode.create(
             ctypeconvnode.create_internal(p,voidpointertype),
             cnilnode.create
             );
        end
      else if (p.resultdef.typ=variantdef) then
        begin
          result:=ccallnode.createintern('fpc_variant_init',
            ccallparanode.create(
              ctypeconvnode.create_internal(p,search_system_type('TVARDATA').typedef),
            nil));
        end
      else
        begin
          result:=ccallnode.createintern('fpc_initialize',
                ccallparanode.create(
                    caddrnode.create_internal(
                        crttinode.create(
                            tstoreddef(p.resultdef),initrtti,rdt_normal)),
                ccallparanode.create(
                    caddrnode.create_internal(p),
                nil)));
        end;
    end;


  class function tnodeutils.finalize_data_node(p:tnode):tnode;
    var
      hs : string;
    begin
      if not assigned(p.resultdef) then
        typecheckpass(p);
      { 'decr_ref' suffix is somewhat misleading, all these helpers
        set the passed pointer to nil now }
      if is_ansistring(p.resultdef) then
        hs:='fpc_ansistr_decr_ref'
      else if is_widestring(p.resultdef) then
        hs:='fpc_widestr_decr_ref'
      else if is_unicodestring(p.resultdef) then
        hs:='fpc_unicodestr_decr_ref'
      else if is_interfacecom_or_dispinterface(p.resultdef) then
        hs:='fpc_intf_decr_ref'
      else
        hs:='';
      if hs<>'' then
        result:=ccallnode.createintern(hs,
           ccallparanode.create(
             ctypeconvnode.create_internal(p,voidpointertype),
             nil))
      else if p.resultdef.typ=variantdef then
        begin
          result:=ccallnode.createintern('fpc_variant_clear',
            ccallparanode.create(
              ctypeconvnode.create_internal(p,search_system_type('TVARDATA').typedef),
            nil));
        end
      else
        result:=ccallnode.createintern('fpc_finalize',
              ccallparanode.create(
                  caddrnode.create_internal(
                      crttinode.create(
                          tstoreddef(p.resultdef),initrtti,rdt_normal)),
              ccallparanode.create(
                  caddrnode.create_internal(p),
              nil)));
    end;


  class function tnodeutils.force_init: boolean;
    begin
      result:=
        (target_info.system in systems_typed_constants_node_init) and
        assigned(current_module.tcinitcode);
    end;


  class function tnodeutils.force_final: boolean;
    begin
      result:=false;
    end;


  class procedure tnodeutils.initialize_textrec(p:TObject;statn:pointer);
    var
      stat: ^tstatementnode absolute statn;
    begin
      if (tsym(p).typ=staticvarsym) and
       (tstaticvarsym(p).vardef.typ=filedef) and
       (tfiledef(tstaticvarsym(p).vardef).filetyp=ft_text) and
       (tstaticvarsym(p).isoindex<>0) then
       begin
         addstatement(stat^,ccallnode.createintern('fpc_textinit_iso',
           ccallparanode.create(
             cordconstnode.create(tstaticvarsym(p).isoindex,uinttype,false),
           ccallparanode.create(
             cloadnode.create(tstaticvarsym(p),tstaticvarsym(p).Owner),
           nil))));
       end;
    end;


  class procedure tnodeutils.finalize_textrec(p:TObject;statn:pointer);
    var
      stat: ^tstatementnode absolute statn;
    begin
      if (tsym(p).typ=staticvarsym) and
       (tstaticvarsym(p).vardef.typ=filedef) and
       (tfiledef(tstaticvarsym(p).vardef).filetyp=ft_text) and
       (tstaticvarsym(p).isoindex<>0) then
       begin
         addstatement(stat^,ccallnode.createintern('fpc_textclose_iso',
           ccallparanode.create(
             cloadnode.create(tstaticvarsym(p),tstaticvarsym(p).Owner),
           nil)));
       end;
    end;


  class function tnodeutils.wrap_proc_body(pd: tprocdef; n: tnode): tnode;
    var
      stat: tstatementnode;
      block: tnode;
      psym: tsym;
    begin
      result:=maybe_insert_trashing(pd,n);

      if (m_iso in current_settings.modeswitches) and
        (pd.proctypeoption=potype_proginit) then
        begin
          block:=internalstatements(stat);
          pd.localst.SymList.ForEachCall(@initialize_textrec,@stat);
          addstatement(stat,result);
          pd.localst.SymList.ForEachCall(@finalize_textrec,@stat);
          result:=block;
        end;

      if target_info.system in systems_typed_constants_node_init then
        begin
          case pd.proctypeoption of
            potype_class_constructor:
              begin
                { even though the initialisation code for typed constants may
                  not yet be complete at this point (there may be more inside
                  method definitions coming after this class constructor), the
                  ones from inside the class definition have already been parsed.
                  in case of $j-, these are marked "final" in Java and such
                  static fields must be initialsed in the class constructor
                  itself -> add them here }
                block:=internalstatements(stat);
                if assigned(pd.struct.tcinitcode) then
                  begin
                    addstatement(stat,pd.struct.tcinitcode);
                    pd.struct.tcinitcode:=nil;
                  end;
                psym:=tsym(pd.struct.symtable.find('FPC_INIT_TYPED_CONSTS_HELPER'));
                if assigned(psym) then
                  begin
                    if (psym.typ<>procsym) or
                       (tprocsym(psym).procdeflist.count<>1) then
                      internalerror(2011040301);
                    addstatement(stat,ccallnode.create(nil,tprocsym(psym),
                      pd.struct.symtable,nil,[]));
                  end;
                addstatement(stat,result);
                result:=block
              end;
            potype_unitinit:
              begin
                if assigned(current_module.tcinitcode) then
                  begin
                    block:=internalstatements(stat);
                    addstatement(stat,tnode(current_module.tcinitcode));
                    current_module.tcinitcode:=nil;
                    addstatement(stat,result);
                    result:=block;
                  end;
              end;
            else case pd.synthetickind of
              tsk_tcinit:
                begin
                  if assigned(pd.struct.tcinitcode) then
                    begin
                      block:=internalstatements(stat);
                      addstatement(stat,pd.struct.tcinitcode);
                      pd.struct.tcinitcode:=nil;
                      addstatement(stat,result);
                      result:=block
                    end
                end;
            end;
          end;
        end;
    end;


  class function tnodeutils.maybe_insert_trashing(pd: tprocdef; n: tnode): tnode;
    var
      stat: tstatementnode;
    begin
      result:=n;
      if check_insert_trashing(pd) then
        begin
          result:=internalstatements(stat);
          pd.parast.SymList.ForEachCall(@maybe_trash_variable_callback,@stat);
          pd.localst.SymList.ForEachCall(@maybe_trash_variable_callback,@stat);
          addstatement(stat,n);
        end;
    end;

  class function tnodeutils.check_insert_trashing(pd: tprocdef): boolean;
    begin
      result:=
        (localvartrashing<>-1) and
        not(po_assembler in pd.procoptions);
    end;


  class function tnodeutils.trashable_sym(p: tsym): boolean;
    begin
      result:=
        ((p.typ=localvarsym) or
         ((p.typ=paravarsym) and
          ((vo_is_funcret in tabstractnormalvarsym(p).varoptions) or
           (tabstractnormalvarsym(p).varspez=vs_out)))) and
         not (vo_is_default_var in tabstractnormalvarsym(p).varoptions) and
         not is_managed_type(tabstractnormalvarsym(p).vardef) and
         not assigned(tabstractnormalvarsym(p).defaultconstsym);
    end;


  class procedure tnodeutils.maybe_trash_variable(var stat: tstatementnode; p: tabstractnormalvarsym; trashn: tnode);
    var
      size: asizeint;
      trashintval: int64;
    begin
      if trashable_sym(p) then
        begin
          trashintval:=trashintvalues[localvartrashing];
          if (p.vardef.typ=procvardef) and
             ([m_tp_procvar,m_mac_procvar]*current_settings.modeswitches<>[]) then
            begin
              if tprocvardef(p.vardef).is_addressonly then
                { in tp/delphi mode, you need @procvar to get at the contents of
                  a procvar ... }
                trashn:=caddrnode.create(trashn)
              else
                { ... but if it's a procedure of object, that will only return
                  the procedure address -> cast to tmethod instead }
                trashn:=ctypeconvnode.create_explicit(trashn,methodpointertype);
            end;
          if ((p.typ=localvarsym) and
              (not(vo_is_funcret in p.varoptions) or
               not is_shortstring(p.vardef))) or
             ((p.typ=paravarsym) and
              not is_shortstring(p.vardef)) then
            begin
              size:=p.getsize;
              case size of
                0:
                  begin
                    { open array -> at least size 1. Can also be zero-sized
                      record, so check it's actually an array }
                    if p.vardef.typ=arraydef then
                      trash_large(stat,trashn,caddnode.create(addn,cinlinenode.create(in_high_x,false,trashn.getcopy),genintconstnode(1)),trashintval)
                    else
                      trashn.free;
                  end;
                1: trash_small(stat,
                  ctypeconvnode.create_internal(trashn,s8inttype),
                    genintconstnode(shortint(trashintval)));
                2: trash_small(stat,
                  ctypeconvnode.create_internal(trashn,s16inttype),
                    genintconstnode(smallint(trashintval)));
                4: trash_small(stat,
                  ctypeconvnode.create_internal(trashn,s32inttype),
                    genintconstnode(longint(trashintval)));
                8: trash_small(stat,
                  ctypeconvnode.create_internal(trashn,s64inttype),
                    genintconstnode(int64(trashintval)));
                else
                  trash_large(stat,trashn,genintconstnode(size),trashintval);
              end;
            end
          else
            begin
              { may be an open string, even if is_open_string() returns false
                (for some helpers in the system unit)             }
              { an open string has at least size 2                      }
              trash_small(stat,
                cvecnode.create(trashn.getcopy,genintconstnode(0)),
                cordconstnode.create(tconstexprint(byte(trashintval)),cansichartype,false));
              trash_small(stat,
                cvecnode.create(trashn,genintconstnode(1)),
                cordconstnode.create(tconstexprint(byte(trashintval)),cansichartype,false));
            end;
        end
      else
        trashn.free;
    end;


  class procedure tnodeutils.maybe_trash_variable_callback(p:TObject;statn:pointer);
    var
      stat: ^tstatementnode absolute statn;
    begin
      if not(tsym(p).typ in [localvarsym,paravarsym]) then
        exit;
      maybe_trash_variable(stat^,tabstractnormalvarsym(p),cloadnode.create(tsym(p),tsym(p).owner));
    end;


  class procedure tnodeutils.trash_small(var stat: tstatementnode; trashn: tnode; trashvaln: tnode);
    begin
      addstatement(stat,cassignmentnode.create(trashn,trashvaln));
    end;


  class procedure tnodeutils.trash_large(var stat: tstatementnode; trashn, sizen: tnode; trashintval: int64);
    begin
      addstatement(stat,ccallnode.createintern('fpc_fillmem',
        ccallparanode.Create(cordconstnode.create(tconstexprint(byte(trashintval)),u8inttype,false),
        ccallparanode.Create(sizen,
        ccallparanode.Create(trashn,nil)))
        ));
    end;


  class procedure tnodeutils.insertbsssym(list: tasmlist; sym: tstaticvarsym; size: asizeint; varalign: shortint);
    begin
      if sym.globalasmsym then
        begin
          { on AIX/stabx, we cannot generate debug information that encodes
            the address of a global symbol, you need a symbol with the same
            name as the identifier -> create an extra *local* symbol.
            Moreover, such a local symbol will be removed if it's not
            referenced anywhere, so also create a reference }
          if (target_dbg.id=dbg_stabx) and
             (cs_debuginfo in current_settings.moduleswitches) and
             not assigned(current_asmdata.GetAsmSymbol(sym.name)) then
            begin
              list.concat(tai_symbol.Create(current_asmdata.DefineAsmSymbol(sym.name,AB_LOCAL,AT_DATA),0));
              list.concat(tai_directive.Create(asd_reference,sym.name));
            end;
          list.concat(Tai_datablock.create_global(sym.mangledname,size));
        end
      else
        list.concat(Tai_datablock.create(sym.mangledname,size));
    end;


  class procedure tnodeutils.insertbssdata(sym: tstaticvarsym);
    var
      l : asizeint;
      varalign : shortint;
      storefilepos : tfileposinfo;
      list : TAsmList;
      sectype : TAsmSectiontype;
    begin
      storefilepos:=current_filepos;
      current_filepos:=sym.fileinfo;
      l:=sym.getsize;
      varalign:=sym.vardef.alignment;
      if (varalign=0) then
        varalign:=var_align_size(l)
      else
        varalign:=var_align(varalign);
      if tf_section_threadvars in target_info.flags then
        begin
          if (vo_is_thread_var in sym.varoptions) then
            begin
              list:=current_asmdata.asmlists[al_threadvars];
              sectype:=sec_threadvar;
            end
          else
            begin
              list:=current_asmdata.asmlists[al_globals];
              sectype:=sec_bss;
            end;
        end
      else
        begin
          if (vo_is_thread_var in sym.varoptions) then
            begin
              inc(l,sizeof(pint));
              { it doesn't help to set a higher alignment, as  }
              { the first sizeof(pint) bytes field will offset }
              { everything anyway                              }
              varalign:=sizeof(pint);
            end;
          list:=current_asmdata.asmlists[al_globals];
          sectype:=sec_bss;
        end;
      maybe_new_object_file(list);
      if vo_has_section in sym.varoptions then
        new_section(list,sec_user,sym.section,varalign)
      else
        new_section(list,sectype,lower(sym.mangledname),varalign);
      insertbsssym(list,sym,l,varalign);
      current_filepos:=storefilepos;
    end;


  class function tnodeutils.create_main_procdef(const name: string; potype: tproctypeoption; ps: tprocsym): tdef;
    var
      pd: tprocdef;
    begin
      pd:=cprocdef.create(main_program_level);
      pd.procsym:=ps;
      ps.ProcdefList.Add(pd);
      include(pd.procoptions,po_global);
      { set procdef options }
      pd.proctypeoption:=potype;
      pd.proccalloption:=pocall_default;
      include(pd.procoptions,po_hascallingconvention);
      pd.forwarddef:=false;
      { may be required to calculate the mangled name }
      add_main_procdef_paras(pd);
      pd.setmangledname(name);
      pd.aliasnames.insert(pd.mangledname);
      result:=pd;
    end;


  procedure AddToStructInits(p:TObject;arg:pointer);
    var
      StructList: TFPList absolute arg;
    begin
      if (tdef(p).typ in [objectdef,recorddef]) and
         not (df_generic in tdef(p).defoptions) then
        begin
          { first add the class... }
          if ([oo_has_class_constructor,oo_has_class_destructor] * tabstractrecorddef(p).objectoptions <> []) then
            StructList.Add(p);
          { ... and then also add all subclasses }
          tabstractrecorddef(p).symtable.deflist.foreachcall(@AddToStructInits,arg);
        end;
    end;


  class procedure tnodeutils.InsertInitFinalTable;
    var
      hp : tused_unit;
      unitinits : TAsmList;
      count : longint;

      procedure write_struct_inits(u: tmodule);
        var
          i: integer;
          structlist: TFPList;
          pd: tprocdef;
        begin
          structlist := TFPList.Create;
          if assigned(u.globalsymtable) then
            u.globalsymtable.DefList.ForEachCall(@AddToStructInits,structlist);
          u.localsymtable.DefList.ForEachCall(@AddToStructInits,structlist);
          { write structures }
          for i := 0 to structlist.Count - 1 do
          begin
            pd := tabstractrecorddef(structlist[i]).find_procdef_bytype(potype_class_constructor);
            if assigned(pd) then
              unitinits.concat(Tai_const.Createname(pd.mangledname,AT_FUNCTION,0))
            else
              unitinits.concat(Tai_const.Create_nil_codeptr);
            pd := tabstractrecorddef(structlist[i]).find_procdef_bytype(potype_class_destructor);
            if assigned(pd) then
              unitinits.concat(Tai_const.Createname(pd.mangledname,AT_FUNCTION,0))
            else
              unitinits.concat(Tai_const.Create_nil_codeptr);
            inc(count);
          end;
          structlist.free;
        end;

    begin
      unitinits:=TAsmList.Create;
      count:=0;
      hp:=tused_unit(usedunits.first);
      while assigned(hp) do
       begin
         { insert class constructors/destructors of the unit }
         if (hp.u.flags and uf_classinits) <> 0 then
           write_struct_inits(hp.u);
         { call the unit init code and make it external }
         if (hp.u.flags and (uf_init or uf_finalize))<>0 then
           begin
             if (hp.u.flags and uf_init)<>0 then
               unitinits.concat(Tai_const.Createname(make_mangledname('INIT$',hp.u.globalsymtable,''),AT_FUNCTION,0))
             else
               unitinits.concat(Tai_const.Create_nil_codeptr);
             if (hp.u.flags and uf_finalize)<>0 then
               unitinits.concat(Tai_const.Createname(make_mangledname('FINALIZE$',hp.u.globalsymtable,''),AT_FUNCTION,0))
             else
               unitinits.concat(Tai_const.Create_nil_codeptr);
             inc(count);
           end;
         hp:=tused_unit(hp.next);
       end;
      { insert class constructors/destructor of the program }
      if (current_module.flags and uf_classinits) <> 0 then
        write_struct_inits(current_module);
      { Insert initialization/finalization of the program }
      if (current_module.flags and (uf_init or uf_finalize))<>0 then
        begin
          if (current_module.flags and uf_init)<>0 then
            unitinits.concat(Tai_const.Createname(make_mangledname('INIT$',current_module.localsymtable,''),AT_FUNCTION,0))
          else
            unitinits.concat(Tai_const.Create_nil_codeptr);
          if (current_module.flags and uf_finalize)<>0 then
            unitinits.concat(Tai_const.Createname(make_mangledname('FINALIZE$',current_module.localsymtable,''),AT_FUNCTION,0))
          else
            unitinits.concat(Tai_const.Create_nil_codeptr);
          inc(count);
        end;
      { Insert TableCount,InitCount at start }
      unitinits.insert(Tai_const.Create_pint(0));
      unitinits.insert(Tai_const.Create_pint(count));
      { Add to data segment }
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_data,'INITFINAL',const_align(sizeof(pint)));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('INITFINAL',AT_DATA,0));
      current_asmdata.asmlists[al_globals].concatlist(unitinits);
      current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname('INITFINAL'));
      unitinits.free;
    end;


  class procedure tnodeutils.InsertThreadvarTablesTable;
    var
      hp : tused_unit;
      ltvTables : TAsmList;
      count : longint;
    begin
      if (tf_section_threadvars in target_info.flags) then
        exit;
      ltvTables:=TAsmList.Create;
      count:=0;
      hp:=tused_unit(usedunits.first);
      while assigned(hp) do
       begin
         If (hp.u.flags and uf_threadvars)=uf_threadvars then
          begin
            ltvTables.concat(Tai_const.Createname(make_mangledname('THREADVARLIST',hp.u.globalsymtable,''),0));
            inc(count);
          end;
         hp:=tused_unit(hp.next);
       end;
      { Add program threadvars, if any }
      If (current_module.flags and uf_threadvars)=uf_threadvars then
       begin
         ltvTables.concat(Tai_const.Createname(make_mangledname('THREADVARLIST',current_module.localsymtable,''),0));
         inc(count);
       end;
      { Insert TableCount at start }
      ltvTables.insert(Tai_const.Create_32bit(count));
      { insert in data segment }
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_data,'FPC_THREADVARTABLES',const_align(sizeof(pint)));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('FPC_THREADVARTABLES',AT_DATA,0));
      current_asmdata.asmlists[al_globals].concatlist(ltvTables);
      current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname('FPC_THREADVARTABLES'));
      ltvTables.free;
    end;



  procedure AddToThreadvarList(p:TObject;arg:pointer);
    var
      ltvTable : TAsmList;
    begin
      ltvTable:=TAsmList(arg);
      if (tsym(p).typ=staticvarsym) and
         (vo_is_thread_var in tstaticvarsym(p).varoptions) then
       begin
         { address of threadvar }
         ltvTable.concat(tai_const.Createname(tstaticvarsym(p).mangledname,0));
         { size of threadvar }
         ltvTable.concat(tai_const.create_32bit(tstaticvarsym(p).getsize));
       end;
    end;


  class procedure tnodeutils.InsertThreadvars;
    var
      s : string;
      ltvTable : TAsmList;
    begin
       if (tf_section_threadvars in target_info.flags) then
         exit;
       ltvTable:=TAsmList.create;
       if assigned(current_module.globalsymtable) then
         current_module.globalsymtable.SymList.ForEachCall(@AddToThreadvarList,ltvTable);
       current_module.localsymtable.SymList.ForEachCall(@AddToThreadvarList,ltvTable);
       if not ltvTable.Empty then
        begin
          s:=make_mangledname('THREADVARLIST',current_module.localsymtable,'');
          { end of the list marker }
          ltvTable.concat(tai_const.create_sym(nil));
          { add to datasegment }
          maybe_new_object_file(current_asmdata.asmlists[al_globals]);
          new_section(current_asmdata.asmlists[al_globals],sec_data,s,sizeof(pint));
          current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
          current_asmdata.asmlists[al_globals].concatlist(ltvTable);
          current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname(s));
          current_module.flags:=current_module.flags or uf_threadvars;
        end;
       ltvTable.Free;
    end;


  class procedure tnodeutils.InsertRuntimeInitsTablesTable(const prefix,tablename:string;unitflag:cardinal);
    var
      hp: tused_unit;
      hlist: TAsmList;
      count: longint;
    begin
      hlist:=TAsmList.Create;
      count:=0;
      hp:=tused_unit(usedunits.first);
      while assigned(hp) do
       begin
         if (hp.u.flags and unitflag)=unitflag then
          begin
            hlist.concat(Tai_const.Createname(make_mangledname(prefix,hp.u.globalsymtable,''),0));
            inc(count);
          end;
         hp:=tused_unit(hp.next);
       end;
      { Add items from program, if any }
      if (current_module.flags and unitflag)=unitflag then
       begin
         hlist.concat(Tai_const.Createname(make_mangledname(prefix,current_module.localsymtable,''),0));
         inc(count);
       end;
      { Insert TableCount at start }
      hlist.insert(Tai_const.Create_pint(count));
      { insert in data segment }
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_data,tablename,const_align(sizeof(pint)));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(tablename,AT_DATA,0));
      current_asmdata.asmlists[al_globals].concatlist(hlist);
      current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname(tablename));
      hlist.free;
    end;


  class procedure tnodeutils.InsertRuntimeInits(const prefix:string;list:TLinkedList;unitflag:cardinal);
    var
      s: string;
      item: TTCInitItem;
    begin
      item:=TTCInitItem(list.First);
      if item=nil then
        exit;
      s:=make_mangledname(prefix,current_module.localsymtable,'');
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_data,s,sizeof(pint));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
      repeat
        { optimize away unused local/static symbols }
        if (item.sym.refs>0) or (item.sym.owner.symtabletype=globalsymtable) then
          begin
            { address to initialize }
            current_asmdata.asmlists[al_globals].concat(Tai_const.createname(item.sym.mangledname, item.offset));
            { value with which to initialize }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(item.datalabel));
          end;
        item:=TTCInitItem(item.Next);
      until item=nil;
      { end-of-list marker }
      current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname(s));
      current_module.flags:=current_module.flags or unitflag;
    end;


  class procedure tnodeutils.InsertWideInits;
    begin
      InsertRuntimeInits('WIDEINITS',current_asmdata.WideInits,uf_wideinits);
    end;


  class procedure tnodeutils.InsertResStrInits;
    begin
      InsertRuntimeInits('RESSTRINITS',current_asmdata.ResStrInits,uf_resstrinits);
    end;


  class procedure tnodeutils.InsertWideInitsTablesTable;
    begin
      InsertRuntimeInitsTablesTable('WIDEINITS','FPC_WIDEINITTABLES',uf_wideinits);
    end;


  class procedure tnodeutils.InsertResStrTablesTable;
    begin
      InsertRuntimeInitsTablesTable('RESSTRINITS','FPC_RESSTRINITTABLES',uf_resstrinits);
    end;


  class procedure tnodeutils.InsertResourceTablesTable;
    var
      hp : tmodule;
      ResourceStringTables : tasmlist;
      count : longint;
    begin
      ResourceStringTables:=tasmlist.Create;
      count:=0;
      hp:=tmodule(loaded_units.first);
      while assigned(hp) do
        begin
          If (hp.flags and uf_has_resourcestrings)=uf_has_resourcestrings then
            begin
              ResourceStringTables.concat(Tai_const.Createname(make_mangledname('RESSTR',hp.localsymtable,'START'),AT_DATA,0));
              ResourceStringTables.concat(Tai_const.Createname(make_mangledname('RESSTR',hp.localsymtable,'END'),AT_DATA,0));
              inc(count);
            end;
          hp:=tmodule(hp.next);
        end;
      { Insert TableCount at start }
      ResourceStringTables.insert(Tai_const.Create_pint(count));
      { Add to data segment }
      maybe_new_object_file(current_asmdata.AsmLists[al_globals]);
      new_section(current_asmdata.AsmLists[al_globals],sec_data,'FPC_RESOURCESTRINGTABLES',const_align(sizeof(pint)));
      current_asmdata.AsmLists[al_globals].concat(Tai_symbol.Createname_global('FPC_RESOURCESTRINGTABLES',AT_DATA,0));
      current_asmdata.AsmLists[al_globals].concatlist(ResourceStringTables);
      current_asmdata.AsmLists[al_globals].concat(Tai_symbol_end.Createname('FPC_RESOURCESTRINGTABLES'));
      ResourceStringTables.free;
    end;


  class procedure tnodeutils.InsertResourceInfo(ResourcesUsed: boolean);
    var
      ResourceInfo : TAsmList;
    begin
      if (target_res.id in [res_elf,res_macho,res_xcoff]) then
        begin
        ResourceInfo:=current_asmdata.asmlists[al_globals];

        maybe_new_object_file(ResourceInfo);
        new_section(ResourceInfo,sec_data,'FPC_RESLOCATION',sizeof(aint));
        ResourceInfo.concat(Tai_symbol.Createname_global('FPC_RESLOCATION',AT_DATA,0));
        if ResourcesUsed then
          { Valid pointer to resource information }
          ResourceInfo.concat(Tai_const.Createname('FPC_RESSYMBOL',0))
        else
          { Nil pointer to resource information }
          {$IFNDEF cpu64bitaddr}
          ResourceInfo.Concat(Tai_const.Create_32bit(0));
          {$ELSE}
          ResourceInfo.Concat(Tai_const.Create_64bit(0));
          {$ENDIF}
        end;
    end;


  class procedure tnodeutils.InsertMemorySizes;
{$IFDEF POWERPC}
    var
      stkcookie: string;
{$ENDIF POWERPC}
    begin
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      { Insert Ident of the compiler in the .fpc.version section }
      new_section(current_asmdata.asmlists[al_globals],sec_fpc,'version',const_align(32));
      current_asmdata.asmlists[al_globals].concat(Tai_string.Create('FPC '+full_version_string+
        ' ['+date_string+'] for '+target_cpu_string+' - '+target_info.shortname));
      if not(tf_no_generic_stackcheck in target_info.flags) then
        begin
          { stacksize can be specified and is now simulated }
          new_section(current_asmdata.asmlists[al_globals],sec_data,'__stklen', sizeof(pint));
          current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__stklen',AT_DATA,sizeof(pint)));
          current_asmdata.asmlists[al_globals].concat(Tai_const.Create_pint(stacksize));
        end;
{$IFDEF POWERPC}
      { AmigaOS4 "stack cookie" support }
      if ( target_info.system = system_powerpc_amiga ) then
       begin
         { this symbol is needed to ignite powerpc amigaos' }
         { stack allocation magic for us with the given stack size. }
         { note: won't work for m68k amigaos or morphos. (KB) }
         str(stacksize,stkcookie);
         stkcookie:='$STACK: '+stkcookie+#0;
         maybe_new_object_file(current_asmdata.asmlists[al_globals]);
         new_section(current_asmdata.asmlists[al_globals],sec_data,'__stack_cookie',length(stkcookie));
         current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__stack_cookie',AT_DATA,length(stkcookie)));
         current_asmdata.asmlists[al_globals].concat(Tai_string.Create(stkcookie));
       end;
{$ENDIF POWERPC}
      { Initial heapsize }
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_data,'__heapsize',const_align(sizeof(pint)));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__heapsize',AT_DATA,sizeof(pint)));
      current_asmdata.asmlists[al_globals].concat(Tai_const.Create_pint(heapsize));

      { allocate an initial heap on embedded systems }
      if target_info.system in systems_embedded then
        begin
          maybe_new_object_file(current_asmdata.asmlists[al_globals]);
          new_section(current_asmdata.asmlists[al_globals],sec_bss,'__fpc_initialheap',current_settings.alignment.varalignmax);
          current_asmdata.asmlists[al_globals].concat(tai_datablock.Create_global('__fpc_initialheap',heapsize));
        end;

      { Valgrind usage }
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_data,'__fpc_valgrind',const_align(sizeof(pint)));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__fpc_valgrind',AT_DATA,sizeof(boolean)));
      current_asmdata.asmlists[al_globals].concat(Tai_const.create_8bit(byte(cs_gdb_valgrind in current_settings.globalswitches)));
    end;


   class procedure tnodeutils.add_main_procdef_paras(pd: tdef);
     begin
       { no parameters by default }
     end;


end.

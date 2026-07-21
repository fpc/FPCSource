 {
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for nodes that handle loads and assignments which
    are the same for all (most) processors

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
unit ncgld;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symtype,symsym,
      aasmdata,
      node,nld,cgutils;

    type
       tcgloadnode = class(tloadnode)
         protected
          procedure generate_nested_access(vs: tsym;ctx:tpassgeneratecodecontext);virtual;
          procedure generate_absaddr_access(vs: tabsolutevarsym;ctx:tpassgeneratecodecontext); virtual;
          procedure generate_threadvar_access(gvs: tstaticvarsym;ctx:tpassgeneratecodecontext); virtual;
          function use_indirect_symbol(gvs: tstaticvarsym): boolean;
         public
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
          procedure changereflocation(const ref: treference;ctx:tpassgeneratecodecontext);
       end;

       tcgassignmentnode = class(tassignmentnode)
        protected
          function maybechangetemp(list: TAsmList; var n: tnode; const newref: treference;ctx:tpassgeneratecodecontext): boolean;virtual;
        public
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
       end;

       tcgarrayconstructornode = class(tarrayconstructornode)
         protected
          procedure makearrayref(var ref: treference; eledef: tdef; ctx:tpassgeneratecodecontext);virtual;
          procedure advancearrayoffset(var ref: treference; elesize: asizeint);virtual;
         public
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
       end;

       tcgrttinode = class(trttinode)
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
       end;


implementation

    uses
      cutils,
      systemstypes,systems,
      verbose,globals,constexp,fmodule,compiler,
      nutils,
      symtable,symconst,symdef,defutil,paramgr,ncon,nbas,ncgrtti,
      aasmbase,
      cgbase,pass_2,pass_2_context,
      procinfo,
      cpuinfo,
      cpubase,parabase,
      tgobj,
      cgobj,nodehelper,
      ncgbas,ncgflw,
      wpobase;

{*****************************************************************************
                   SSA (for memory temps) support
*****************************************************************************}

    type
      preplacerefrec = ^treplacerefrec;
      treplacerefrec = record
        old, new: preference;
        ressym: tsym;
        ctx:tpassgeneratecodecontext;
      end;

    function doreplaceref(var n: tnode; para: pointer): foreachnoderesult;
      var
        rr: preplacerefrec absolute para;
      begin
        result := fen_false;
        case n.nodetype of
          loadn:
            begin
                 { regular variable }
              if (tabstractvarsym(tloadnode(n).symtableentry).varoptions * [vo_is_dll_var, vo_is_thread_var] = []) and
                 not assigned(tloadnode(n).left) and
                 { not function result, or no exit in function }
                 (((tloadnode(n).symtableentry <> rr^.ressym) and
                   not(vo_is_funcret in tabstractvarsym(tloadnode(n).symtableentry).varoptions)) or
                  not(fc_exit in flowcontrol)) and
                 { stored in memory... }
                 (tabstractnormalvarsym(tloadnode(n).symtableentry).localloc.loc in [LOC_REFERENCE]) and
                 { ... at the place we are looking for }
                 references_equal(tabstractnormalvarsym(tloadnode(n).symtableentry).localloc.reference,rr^.old^) and
                 { its address cannot have escaped the current routine }
                 not(tabstractvarsym(tloadnode(n).symtableentry).addr_taken) and
                 { it is not accessed in nested procedures }
                 not(tabstractvarsym(tloadnode(n).symtableentry).different_scope) then
                begin
                  { relocate variable }
                  tcgloadnode(n).changereflocation(rr^.new^,rr^.ctx);
                  result := fen_norecurse_true;
                end;
            end;
          temprefn:
            begin
              if (ti_valid in ttemprefnode(n).tempflags) and
                 { memory temp... }
                 (ttemprefnode(n).tempinfo^.location.loc in [LOC_REFERENCE]) and
                 { ... at the place we are looking for }
                 references_equal(ttemprefnode(n).tempinfo^.location.reference,rr^.old^) and
                 { its address cannot have escaped the current routine }
                 not(ti_addr_taken in ttemprefnode(n).tempflags) then
                begin
                  { relocate the temp }
                  tcgtemprefnode(n).changelocation(rr^.new^,rr^.ctx);
                  result := fen_norecurse_true;
                end;
            end;
          { Subscriptn must be rejected, otherwise we may replace an
            an entire record with a temp for its first field, mantis #13948)
            Exception: the field's size is the same as the entire record

            The same goes for array indexing
          }
          subscriptn,
          vecn:
            if not(tunarynode(n).left.resultdef.typ in [recorddef,objectdef,arraydef,stringdef]) or
               { make sure we don't try to call resultdef.size for types that
                 don't have a compile-time size such as open arrays }
               is_special_array(tunarynode(n).left.resultdef) or
               (tunarynode(n).left.resultdef.size<>tunarynode(n).resultdef.size) then
              result := fen_norecurse_false;

          { optimize the searching a bit }
          derefn,addrn,
          calln,inlinen,casen,
          addn,subn,muln,
          andn,orn,xorn,
          ltn,lten,gtn,gten,equaln,unequaln,
          slashn,divn,shrn,shln,notn,
          inn,
          asn,isn:
            result := fen_norecurse_false;
          else
            ;
        end;
      end;


    function tcgassignmentnode.maybechangetemp(list: TAsmList; var n: tnode; const newref: treference;ctx:tpassgeneratecodecontext): boolean;
      var
        rr: treplacerefrec;
      begin
        result := false;

        { only do for -O2 or higher (breaks debugging since }
        { variables move to different memory locations)     }
        if not(cs_opt_level2 in compiler.globals.current_settings.optimizerswitches) or
           { must be a copy to a memory location ... }
           (n.location.loc <> LOC_REFERENCE) or
           { not inside a control flow statement and no goto's in sight }
           ([fc_inflowcontrol,fc_gotolabel] * flowcontrol <> []) or
           { not for refcounted types, because those locations are   }
           { still used later on in initialisation/finalisation code }
           is_managed_type(n.resultdef) or
           { source and destination are temps (= not global variables) }
           { and both point to the start of a temp, and the source is a }
           { non-persistent temp (otherwise we need some kind of copy-  }
           { on-write support in case later on both are still used)     }
           not ctx.tg.isstartoftemp(newref) or
           not ctx.tg.isstartoftemp(n.location.reference) or
           (ctx.tg.gettypeoftemp(newref) <> tt_normal) or
           not (ctx.tg.gettypeoftemp(n.location.reference) in [tt_normal,tt_persistent]) or
           { and both have the same size }
           (ctx.tg.sizeoftemp(ctx.CurrAsmList,newref) <> ctx.tg.sizeoftemp(ctx.CurrAsmList,n.location.reference)) then
          exit;

        { find the source of the old reference (loadnode or tempnode) }
        { and replace it with the new reference                       }
        rr.old := @n.location.reference;
        rr.new := @newref;
        rr.ressym := nil;
        rr.ctx := ctx;

        if assigned(compiler.current_procinfo.procdef.funcretsym) and
           (tabstractvarsym(compiler.current_procinfo.procdef.funcretsym).refs <> 0) then
          if (compiler.current_procinfo.procdef.proctypeoption=potype_constructor) then
            rr.ressym:=tsym(compiler.current_procinfo.procdef.parast.Find('self'))
         else
            rr.ressym:=compiler.current_procinfo.procdef.funcretsym;

        { if source not found, don't do anything }
        if not foreachnodestatic(n,@doreplaceref,@rr) then
          exit;

        n.location.reference := newref;
        result:=true;
      end;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure tcgloadnode.changereflocation(const ref: treference;ctx:tpassgeneratecodecontext);
      var
        oldtemptype: ttemptype;
      begin
        if (location.loc<>LOC_REFERENCE) then
          internalerror(2007020812);
        if not ctx.tg.istemp(location.reference) then
          internalerror(2007020813);
        oldtemptype:=ctx.tg.gettypeoftemp(location.reference);
        if (oldtemptype = tt_persistent) then
          ctx.tg.ChangeTempType(ctx.CurrAsmList,location.reference,tt_normal);
        ctx.tg.ungettemp(ctx.CurrAsmList,location.reference);
        location.reference:=ref;
        ctx.tg.ChangeTempType(ctx.CurrAsmList,location.reference,oldtemptype);
        tabstractnormalvarsym(symtableentry).localloc:=location;
        ctx.hlcg.recordnewsymloc(ctx.CurrAsmList,symtableentry,tabstractnormalvarsym(symtableentry).vardef,location.reference,false);
      end;


    procedure tcgloadnode.generate_nested_access(vs: tsym;ctx:tpassgeneratecodecontext);
      var
        { parameter declared as tsym to reduce interface unit dependencies }
        lvs: tabstractnormalvarsym absolute vs;
      begin
        secondpass(left,ctx);
        if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          internalerror(200309286);
        if lvs.localloc.loc<>LOC_REFERENCE then
          internalerror(200409241);
        ctx.hlcg.reference_reset_base(location.reference,left.resultdef,left.location.register,lvs.localloc.reference.offset,ctempposinvalid,lvs.localloc.reference.alignment,lvs.localloc.reference.volatility);
      end;


    procedure tcgloadnode.generate_absaddr_access(vs: tabsolutevarsym;ctx:tpassgeneratecodecontext);
      begin
        location.reference.offset:=asizeint(vs.addroffset);
        location.reference.volatility:=[vol_read,vol_write];
      end;


    procedure tcgloadnode.generate_threadvar_access(gvs: tstaticvarsym;ctx:tpassgeneratecodecontext);
      var
        respara,
        paraloc1 : tcgpara;
        fieldptrdef,
        pvd : tdef;
        endrelocatelab,
        norelocatelab : tasmlabel;
        tvref,
        href : treference;
        hregister, hreg_tv_rec : tregister;
        tv_rec : trecorddef;
        tv_index_field,
        tv_non_mt_data_field: tsym;
        tmpresloc: tlocation;
        issystemunit,
        indirect : boolean;
        size_opt : boolean;
      begin
         if (tf_section_threadvars in compiler.target.info.flags) then
           begin
             if gvs.localloc.loc=LOC_INVALID then
               if not(vo_is_weak_external in gvs.varoptions) then
                 reference_reset_symbol(location.reference,ctx.CurrAsmList.AsmData.RefAsmSymbol(gvs.mangledname,AT_TLS,use_indirect_symbol(gvs)),0,location.reference.alignment,[])
               else
                 reference_reset_symbol(location.reference,ctx.CurrAsmList.AsmData.WeakRefAsmSymbol(gvs.mangledname,AT_TLS),0,location.reference.alignment,[])
             else
               location:=gvs.localloc;
           end
         else
           begin
             {
               Thread var loading is optimized to first check if
               a relocate function is available. When the function
               is available it is called to retrieve the address.
               Otherwise the address is loaded with the symbol
             }

             tv_rec:=get_threadvar_record(resultdef,tv_index_field,tv_non_mt_data_field);
             fieldptrdef:=cpointerdef.getreusable(resultdef,compiler);
             ctx.CurrAsmList.AsmData.getjumplabel(norelocatelab);
             ctx.CurrAsmList.AsmData.getjumplabel(endrelocatelab);
             { make sure hregister can't allocate the register necessary for the parameter }
             pvd:=search_system_type('TRELOCATETHREADVARHANDLER').typedef;
             if pvd.typ<>procvardef then
               internalerror(2012120901);

             { FPC_THREADVAR_RELOCATE is nil? }
             issystemunit:=(
                             assigned(compiler.current_module.globalsymtable) and
                             (compiler.current_module.globalsymtable=compiler.systemunit)
                           ) or
                           (
                             not assigned(compiler.current_module.globalsymtable) and
                             (compiler.current_module.localsymtable=compiler.systemunit)
                           );
             indirect:=(tf_supports_packages in compiler.target.info.flags) and
                         (compiler.target.info.system in systems_indirect_var_imports) and
                         (cs_imported_data in localswitches) and
                         not issystemunit;
             if not(vo_is_weak_external in gvs.varoptions) then
               reference_reset_symbol(tvref,ctx.CurrAsmList.AsmData.RefAsmSymbol(gvs.mangledname,AT_DATA,use_indirect_symbol(gvs)),0,sizeof(pint),[])
             else
               reference_reset_symbol(tvref,ctx.CurrAsmList.AsmData.WeakRefAsmSymbol(gvs.mangledname,AT_DATA),0,sizeof(pint),[]);
             { Enable size optimization with -Os or PIC code is generated and PIC uses GOT }
             size_opt:={$if defined(RISCV)}
                         true
                       {$else defined(RISCV)}
                         (cs_opt_size in compiler.globals.current_settings.optimizerswitches)
                         or ((cs_create_pic in compiler.globals.current_settings.moduleswitches) and (tf_pic_uses_got in compiler.target.info.flags))
                       {$endif defined(RISCV)};
             hreg_tv_rec:=NR_INVALID;
             if size_opt then
               begin
                 { Load a pointer to the thread var record into a register. }
                 { This register will be used in both multithreaded and non-multithreaded cases. }
                 hreg_tv_rec:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,cpointerdef.getreusable(tv_rec,compiler));
                 ctx.hlcg.a_loadaddr_ref_reg(ctx.CurrAsmList,tv_rec,cpointerdef.getreusable(tv_rec,compiler),tvref,hreg_tv_rec);
                 reference_reset_base(tvref,hreg_tv_rec,0,ctempposinvalid,tvref.alignment,tvref.volatility)
               end;
             paraloc1.init(compiler.target);
             paramanager.getcgtempparaloc(ctx.CurrAsmList,tprocvardef(pvd),1,paraloc1);
             hregister:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,pvd);
             reference_reset_symbol(href,ctx.CurrAsmList.AsmData.RefAsmSymbol('FPC_THREADVAR_RELOCATE',AT_DATA,indirect),0,pvd.alignment,[]);
             if not issystemunit then
               compiler.current_module.add_extern_asmsym('FPC_THREADVAR_RELOCATE',AB_EXTERNAL,AT_DATA);
             ctx.hlcg.a_load_ref_reg(ctx.CurrAsmList,pvd,pvd,href,hregister);
             ctx.hlcg.a_cmp_const_reg_label(ctx.CurrAsmList,pvd,OC_EQ,0,hregister,norelocatelab);
             { no, call it with the index of the threadvar as parameter }
             href:=tvref;
             ctx.hlcg.g_set_addr_nonbitpacked_field_ref(ctx.CurrAsmList,
               tv_rec,
               tfieldvarsym(tv_index_field),href);
             ctx.hlcg.a_load_ref_cgpara(ctx.CurrAsmList,tfieldvarsym(tv_index_field).vardef,href,paraloc1);
             { Dealloc the threadvar record register before calling the helper function to allow  }
             { the register allocator to assign non-mandatory real registers for hreg_tv_rec. }
             if size_opt then
               ctx.cg.a_reg_dealloc(ctx.CurrAsmList,hreg_tv_rec);
             paramanager.freecgpara(ctx.CurrAsmList,paraloc1);
             ctx.cg.allocallcpuregisters(ctx.CurrAsmList);
             { result is the address of the threadvar }
             respara:=ctx.hlcg.a_call_reg(ctx.CurrAsmList,tprocvardef(pvd),hregister,[@paraloc1]);
             paraloc1.done;
             ctx.cg.deallocallcpuregisters(ctx.CurrAsmList);

             { load the address of the result in hregister }
             hregister:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,fieldptrdef);
             location_reset(tmpresloc,LOC_REGISTER,def_cgsize(fieldptrdef));
             tmpresloc.register:=hregister;
             ctx.hlcg.gen_load_cgpara_loc(ctx.CurrAsmList,fieldptrdef,respara,tmpresloc,true);
             respara.resetiftemp;
             ctx.hlcg.a_jmp_always(ctx.CurrAsmList,endrelocatelab);

             { no relocation needed, load the address of the variable only, the
               layout of a threadvar is:
                 0            - Threadvar index
                 sizeof(pint) - Threadvar value in single threading }
             ctx.hlcg.a_label(ctx.CurrAsmList,norelocatelab);
             href:=tvref;
             ctx.hlcg.g_set_addr_nonbitpacked_field_ref(ctx.CurrAsmList,
               tv_rec,
               tfieldvarsym(tv_non_mt_data_field),href);
             ctx.hlcg.a_loadaddr_ref_reg(ctx.CurrAsmList,resultdef,fieldptrdef,href,hregister);
             ctx.hlcg.a_label(ctx.CurrAsmList,endrelocatelab);

             ctx.hlcg.reference_reset_base(location.reference,fieldptrdef,hregister,0,ctempposinvalid,resultdef.alignment,[]);
           end;
       end;


    function tcgloadnode.use_indirect_symbol(gvs:tstaticvarsym):boolean;
      begin
        { we are using a direct reference if any of the following is true:
          - the target does not support packages
          - the target does not use indirect references
          - the variable is declared as (weak) external
          - G- is set
          - the variable is located inside the same unit }
        result:=(tf_supports_packages in compiler.target.info.flags) and
                (compiler.target.info.system in systems_indirect_var_imports) and
                (gvs.varoptions*[vo_is_external,vo_is_weak_external]=[]) and
                (gvs.owner.symtabletype in [globalsymtable,staticsymtable]) and
                (cs_imported_data in localswitches) and
                not sym_is_owned_by(gvs,compiler.current_module.globalsymtable) and
                (
                  (compiler.current_module.globalsymtable=compiler.current_module.localsymtable) or
                  not sym_is_owned_by(gvs,compiler.current_module.localsymtable)
                );
      end;

    procedure tcgloadnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
        hregister : tregister;
        vs   : tabstractnormalvarsym;
        gvs  : tstaticvarsym;
        vmtdef : tpointerdef;
        vmtentry: tfieldvarsym;
        pd   : tprocdef;
        href : treference;
        newsize : tcgsize;
        vd : tdef;
        alignment: longint;
        indirect : boolean;
        name : TSymStr;
      begin
        { we don't know the size of all arrays }
        newsize:=def_cgsize(resultdef);
        { alignment is overridden per case below }
        location_reset_ref(location,LOC_REFERENCE,newsize,resultdef.alignment,[]);
        case symtableentry.typ of
           absolutevarsym :
              begin
                 { this is only for toasm and toaddr }
                 case tabsolutevarsym(symtableentry).abstyp of
                   toaddr :
                     generate_absaddr_access(tabsolutevarsym(symtableentry),ctx);
                   toasm :
                     location.reference.symbol:=ctx.CurrAsmList.AsmData.RefAsmSymbol(tabsolutevarsym(symtableentry).mangledname,AT_DATA);
                   else
                     internalerror(200310283);
                 end;
              end;
           constsym:
             begin
                if tconstsym(symtableentry).consttyp in [constresourcestring,constwresourcestring] then
                  begin
                     location_reset_ref(location,LOC_CREFERENCE,def_cgsize(compiler.deftypes.cansistringtype),compiler.deftypes.cansistringtype.size,[]);
                     indirect:=(tf_supports_packages in compiler.target.info.flags) and
                                 (compiler.target.info.system in systems_indirect_var_imports) and
                                 (cs_imported_data in localswitches) and
                                 (symtableentry.owner.moduleid<>compiler.current_module.moduleid);
                     name:=make_mangledname('RESSTR',symtableentry.owner,symtableentry.name);
                     location.reference.symbol:=ctx.CurrAsmList.AsmData.RefAsmSymbol(name,AT_DATA,indirect);
                     if symtableentry.owner.moduleid<>compiler.current_module.moduleid then
                       compiler.current_module.addimportedsym(symtableentry);
                     vd:=search_system_type('TRESOURCESTRINGRECORD').typedef;
                     ctx.hlcg.g_set_addr_nonbitpacked_field_ref(
                       ctx.CurrAsmList,
                       trecorddef(vd),
                       tfieldvarsym(search_struct_member(trecorddef(vd),'CURRENTVALUE')),
                       location.reference);
                  end
                else
                  internalerror(22798);
             end;
           staticvarsym :
             begin
               gvs:=tstaticvarsym(symtableentry);

               if (vo_is_dll_var in gvs.varoptions) then
               { DLL variable }
                 begin
                   hregister:=ctx.cg.getaddressregister(ctx.CurrAsmList);
                   if not(vo_is_weak_external in gvs.varoptions) then
                     location.reference.symbol:=ctx.CurrAsmList.AsmData.RefAsmSymbol(tstaticvarsym(symtableentry).mangledname,AT_DATA)
                   else
                     location.reference.symbol:=ctx.CurrAsmList.AsmData.WeakRefAsmSymbol(tstaticvarsym(symtableentry).mangledname,AT_DATA);
                   ctx.cg.a_load_ref_reg(ctx.CurrAsmList,OS_ADDR,OS_ADDR,location.reference,hregister);
                   reference_reset_base(location.reference,hregister,0,ctempposinvalid,location.reference.alignment,[]);
                 end
               { Thread variable }
               else if (vo_is_thread_var in gvs.varoptions) then
                 generate_threadvar_access(gvs,ctx)
               { Normal (or external) variable }
               else
                 begin
                   if gvs.localloc.loc=LOC_INVALID then
                     begin
                       { static data is currently always volatile }
                       if not(vo_is_weak_external in gvs.varoptions) then
                         reference_reset_symbol(location.reference,ctx.CurrAsmList.AsmData.RefAsmSymbol(gvs.mangledname,AT_DATA,use_indirect_symbol(gvs)),0,location.reference.alignment,[])
                       else
                         reference_reset_symbol(location.reference,ctx.CurrAsmList.AsmData.WeakRefAsmSymbol(gvs.mangledname,AT_DATA),0,location.reference.alignment,[])
                     end
                   else
                      location:=gvs.localloc;
                  end;

                { make const a LOC_CREFERENCE }
                if (gvs.varspez=vs_const) and
                   (location.loc=LOC_REFERENCE) then
                  location.loc:=LOC_CREFERENCE;
              end;
            paravarsym,
            localvarsym :
              begin
                vs:=tabstractnormalvarsym(symtableentry);
                { Nested variable }
                if assigned(left) then
                  generate_nested_access(vs,ctx)
                else
                  location:=vs.localloc;

                { handle call by reference variables when they are not
                  already copied to local copies. Also ignore the reference
                  when we need to load the self pointer for objects }
                if is_addr_param_load then
                  begin
                    if (location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                      hregister:=location.register
                    else
                      begin
                        vd:=cpointerdef.getreusable(resultdef,compiler);
                        hregister:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,vd);
                        { we need to load only an address }
                        location.size:=int_cgsize(vd.size);
                        ctx.hlcg.a_load_loc_reg(ctx.CurrAsmList,vd,vd,location,hregister);
                      end;
                    { assume packed records may always be unaligned }
                    if not(resultdef.typ in [recorddef,objectdef]) or
                       (tabstractrecordsymtable(tabstractrecorddef(resultdef).symtable).usefieldalignment<>1) then
                      begin
                        alignment:=min(min(min(resultdef.alignment,compiler.globals.current_settings.alignment.localalignmax),compiler.globals.current_settings.alignment.constalignmax),compiler.globals.current_settings.alignment.varalignmax);
                        location_reset_ref(location,LOC_REFERENCE,newsize,alignment,[]);
                      end
                    else
                      location_reset_ref(location,LOC_REFERENCE,newsize,1,[]);
                    ctx.hlcg.reference_reset_base(location.reference,compiler.deftypes.voidpointertype,hregister,0,ctempposinvalid,location.reference.alignment,[]);
                  end;

                { make const a LOC_CREFERENCE }
                if (vs.varspez=vs_const) and
                   (location.loc=LOC_REFERENCE) then
                  location.loc:=LOC_CREFERENCE;
             end;
           procsym:
              begin
                 if not assigned(procdef) then
                   internalerror(200312011);
                 if assigned(left) and
                   (resultdef.typ in [symconst.procdef,procvardef]) and
                    not tabstractprocdef(resultdef).is_addressonly then
                   begin
                     location_reset(location,LOC_CREGISTER,int_cgsize(compiler.deftypes.voidpointertype.size*2));
{$if defined(CPU8BITALU) and defined(CPU16BITADDR)}
                     { cpus with 16 bit address registers don't use registerhi here, so allocate already here a register for all purposes }
                     location.register:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.s32inttype);
{$endif defined(CPU8BITALU) and defined(CPU16BITADDR)}
                     secondpass(left,ctx);

                     { load class instance/classrefdef address }
                     if left.location.loc=LOC_CONSTANT then
                       ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
                     { vd will contain the type of the self pointer (self in
                       case of a class/classref, address of self in case of
                       an object, frame pointer or pointer to parentfpstruct
                       in case of nested procsym load  }
                     vd:=nil;
                     case left.location.loc of
                        LOC_CREGISTER,
                        LOC_REGISTER:
                          begin
                             { this is not possible for objects }
                             if is_object(left.resultdef) then
                               internalerror(200304234);
{$if defined(CPU8BITALU) and defined(CPU16BITADDR)}
                             ctx.hlcg.a_load_reg_reg(ctx.CurrAsmList,left.resultdef,left.resultdef,left.location.register,ctx.cg.GetNextReg(ctx.cg.GetNextReg(location.register)));
{$else defined(CPU8BITALU) and defined(CPU16BITADDR)}
                             location.registerhi:=left.location.register;
{$endif defined(CPU8BITALU) and defined(CPU16BITADDR)}
                             vd:=left.resultdef;
                          end;
                        LOC_CREFERENCE,
                        LOC_REFERENCE:
                          begin
                             if is_implicit_pointer_object_type(left.resultdef) or
                                (left.resultdef.typ=classrefdef) or
                                is_nested_pd(procdef) then
                               begin
                                 vd:=left.resultdef;
{$if defined(CPU8BITALU) and defined(CPU16BITADDR)}
                                 ctx.hlcg.a_load_ref_reg(ctx.CurrAsmList,left.resultdef,left.resultdef,left.location.reference,ctx.cg.GetNextReg(ctx.cg.GetNextReg(location.register)))
{$else defined(CPU8BITALU) and defined(CPU16BITADDR)}
                                 location.registerhi:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,left.resultdef);
                                 ctx.hlcg.a_load_ref_reg(ctx.CurrAsmList,left.resultdef,left.resultdef,left.location.reference,location.registerhi)
{$endif defined(CPU8BITALU) and defined(CPU16BITADDR)}
                               end
                             else
                               begin
                                 vd:=cpointerdef.getreusable(left.resultdef,compiler);
{$if defined(CPU8BITALU) and defined(CPU16BITADDR)}
                                 ctx.hlcg.a_loadaddr_ref_reg(ctx.CurrAsmList,left.resultdef,vd,left.location.reference,ctx.cg.GetNextReg(ctx.cg.GetNextReg(location.register)));
{$else defined(CPU8BITALU) and defined(CPU16BITADDR)}
                                 location.registerhi:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,vd);
                                 ctx.hlcg.a_loadaddr_ref_reg(ctx.CurrAsmList,left.resultdef,vd,left.location.reference,location.registerhi);
{$endif defined(CPU8BITALU) and defined(CPU16BITADDR)}
                               end;
                             ctx.tg.location_freetemp(ctx.CurrAsmList,left.location);
                          end;
                        else
                          internalerror(200610311);
                     end;

                     { virtual method ? }
                     if (po_virtualmethod in procdef.procoptions) and
                        not(loadnf_inherited in loadnodeflags) and
                        not is_objectpascal_helper(procdef.struct) then
                       begin
                         if (not assigned(compiler.current_procinfo) or
                             compiler.wpoinfomanager.symbol_live(compiler.current_procinfo.procdef.mangledname)) then
                           tobjectdef(procdef.struct).register_vmt_call(procdef.extnumber);
            {$ifdef vtentry}
                         if not is_interface(procdef.struct) then
                           begin
                             inc(ctx.CurrAsmList.AsmData.NextVTEntryNr);
                             ctx.CurrAsmList.Concat(tai_symbol.CreateName('VTREF'+tostr(ctx.CurrAsmList.AsmData.NextVTEntryNr)+'_'+procdef._class.vmt_mangledname+'$$'+tostr(vmtoffset div sizeof(pint)),AT_FUNCTION,0,voidpointerdef));
                           end;
            {$endif vtentry}
                         if (left.resultdef.typ=objectdef) and
                            assigned(tobjectdef(left.resultdef).vmt_field) then
                           begin
                             { vmt pointer is a pointer to the vmt record }
                             ctx.hlcg.reference_reset_base(href,vd,location.registerhi,0,ctempposinvalid,vd.alignment,[]);
                             vmtdef:=cpointerdef.getreusable(tobjectdef(left.resultdef).vmt_def,compiler);
                             ctx.hlcg.g_set_addr_nonbitpacked_field_ref(ctx.CurrAsmList,tobjectdef(left.resultdef),tfieldvarsym(tobjectdef(left.resultdef).vmt_field),href);
                             hregister:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,vmtdef);
                             ctx.hlcg.a_load_ref_reg(ctx.CurrAsmList,tfieldvarsym(tobjectdef(left.resultdef).vmt_field).vardef,vmtdef,href,hregister);
                           end
                         else if left.resultdef.typ=classrefdef then
                           begin
                             vmtdef:=cpointerdef.getreusable(tobjectdef(tclassrefdef(left.resultdef).pointeddef).vmt_def,compiler);
                             { classrefdef is a pointer to the vmt already }
{$if defined(CPU8BITALU) and defined(CPU16BITADDR)}
                             hregister:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,vmtdef);
                             ctx.hlcg.a_load_reg_reg(ctx.CurrAsmList,left.resultdef,left.resultdef,ctx.cg.GetNextReg(ctx.cg.GetNextReg(location.register)),hregister);
{$else defined(CPU8BITALU) and defined(CPU16BITADDR)}
                             { targets with 32 bit method pointers got already a register assigned }
                             hregister:=location.registerhi;
{$endif defined(CPU8BITALU) and defined(CPU16BITADDR)}
                             ctx.hlcg.g_ptrtypecast_reg(ctx.CurrAsmList,left.resultdef,vmtdef,hregister);
                           end
                         else if is_any_interface_kind(left.resultdef) then
                           begin
                             { an interface is a pointer to a pointer to a vmt }
                             ctx.hlcg.reference_reset_base(href,vd,location.registerhi,0,ctempposinvalid,vd.alignment,[]);
                             vmtdef:=cpointerdef.getreusable(tobjectdef(left.resultdef).vmt_def,compiler);
                             hregister:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,vmtdef);
                             ctx.hlcg.a_load_ref_reg(ctx.CurrAsmList,vmtdef,vmtdef,href,hregister);
                           end
                         else
                           internalerror(2015112501);
                         { load method address }
                         vmtentry:=tabstractrecordsymtable(trecorddef(vmtdef.pointeddef).symtable).findfieldbyoffset(
                           tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber));
                         ctx.hlcg.reference_reset_base(href,vmtdef,hregister,0,ctempposinvalid,vmtdef.alignment,[]);
                         { targets with 32 bit method pointers got already a register assigned }
{$if not(defined(CPU8BITALU) and defined(CPU16BITADDR))}
                         location.register:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,vmtentry.vardef);
{$endif not(defined(CPU8BITALU) and defined(CPU16BITADDR))}
                         ctx.hlcg.g_set_addr_nonbitpacked_field_ref(ctx.CurrAsmList,tabstractrecorddef(vmtdef.pointeddef),vmtentry,href);
                         ctx.hlcg.a_load_ref_reg(ctx.CurrAsmList,vmtentry.vardef,vmtentry.vardef,href,location.register);
                       end
                     else
                       begin
                         { load address of the function }
                         reference_reset_symbol(href,ctx.CurrAsmList.AsmData.RefAsmSymbol(procdef.mangledname,AT_FUNCTION),0,procdef.address_type.alignment,[]);
                         { targets with 32 bit method pointers got already a register assigned }
{$if not(defined(CPU8BITALU) and defined(CPU16BITADDR))}
                         location.register:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,cprocvardef.getreusableprocaddr(procdef,pc_address_only,compiler));
{$endif not(defined(CPU8BITALU) and defined(CPU16BITADDR))}
                         ctx.hlcg.a_loadaddr_ref_reg(ctx.CurrAsmList,procdef,cprocvardef.getreusableprocaddr(procdef,pc_address_only,compiler),href,location.register);
                       end;

                     { to get methodpointers stored correctly, code and self register must be swapped on
                       big endian targets }
                     if compiler.target.info.endian=endian_big then
                       begin
                         { cpus with 16 bit address registers don't use registerhi here }
{$if defined(CPU8BITALU) and defined(CPU16BITADDR)}
                         Internalerror(2022091201);
{$endif defined(CPU8BITALU) and defined(CPU16BITADDR)}
                         hregister:=location.register;
                         location.register:=location.registerhi;
                         location.registerhi:=hregister;
                       end;
                   end
                 else
                   begin
                      pd:=tprocdef(tprocsym(symtableentry).ProcdefList[0]);
                      { def_cgsize does not work for tprocdef, so we use pd.address_type }
                      location.size:=def_cgsize(pd.address_type);
                      if not(po_weakexternal in pd.procoptions) then
                        location.reference.symbol:=ctx.CurrAsmList.AsmData.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)
                      else
                        location.reference.symbol:=ctx.CurrAsmList.AsmData.WeakRefAsmSymbol(procdef.mangledname,AT_FUNCTION);
                   end;
              end;
           labelsym :
             if assigned(tlabelsym(symtableentry).asmblocklabel) then
               location.reference.symbol:=tlabelsym(symtableentry).asmblocklabel
             else
               location.reference.symbol:=tcglabelnode((tlabelsym(symtableentry).code)).getasmlabel(ctx);
           else internalerror(200510032);
        end;
      end;


{*****************************************************************************
                             SecondAssignment
*****************************************************************************}

    procedure tcgassignmentnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
         shuffle : pmmshuffle;
         hlabel : tasmlabel;
         href : treference;
         releaseright : boolean;
         alignmentrequirement,
         len : tcgint;
         r : tregister;
         {$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
         r64 : tregister64;
         {$endif}
         oldflowcontrol : tflowcontrol;
      begin
        { previously, managed types were handled in firstpass
          newer FPCs however can identify situations when
          assignments of managed types require no special code and the
          value could be just copied so this could should be able also to handle
          managed types without any special "managing code"}

        location_reset(location,LOC_VOID,OS_NO);

        {
          in most cases we can process first the right node which contains
          the most complex code. Exceptions for this are:
            - result is in flags, loading left will then destroy the flags
            - result is a jump, loading left must be already done before the jump is made
            - result need reference count, when left points to a value used in
              right then decreasing the refcnt on left can possibly release
              the memory before right increased the refcnt, result is that an
              empty value is assigned

           But not when the result is in the flags, then
           loading the left node afterwards can destroy the flags.

           Neither if right contains conditional nodes: this might cause problems with
           temp. nodes with init code used by CSE, see e.g. #38129
        }
        if not(right.expectloc in [LOC_FLAGS,LOC_JUMP]) and
            (node_complexity(right)>node_complexity(left)) and not(has_conditional_nodes(right)) then
         begin
           secondpass(right,ctx);
           if compiler.verbose.codegenerror then
             exit;

           secondpass(left,ctx);
           if compiler.verbose.codegenerror then
             exit;
         end
        else
         begin
           { calculate left sides }
           secondpass(left,ctx);
           if compiler.verbose.codegenerror then
             exit;

           { tell the SSA/SSL code that the left side was handled first so
             ni SSL is done
           }
           oldflowcontrol:=flowcontrol;
           include(flowcontrol,fc_lefthandled);

           secondpass(right,ctx);
           flowcontrol:=oldflowcontrol;

           if compiler.verbose.codegenerror then
             exit;
         end;

        releaseright:=
          (left.nodetype<>temprefn) or
          not(ti_const in ttemprefnode(left).tempflags);

        { shortstring assignments are handled separately }
        if is_shortstring(left.resultdef) then
          begin
            {
              we can get here only in the following situations
              for the right node:
               - empty constant string
               - char
            }

            { The addn is replaced by a blockn or calln that already returns
              a shortstring }
            if is_shortstring(right.resultdef) and
               (right.nodetype in [blockn,calln]) then
              begin
                { verify that we indeed have nothing to do }
                if not(anf_assign_done_in_right in assignmentnodeflags) then
                  internalerror(2015042201);
              end
            { empty constant string }
            else if (right.nodetype=stringconstn) and
               (tstringconstnode(right).len=0) then
              begin
                ctx.hlcg.g_ptrtypecast_ref(ctx.CurrAsmList,cpointerdef.getreusable(left.resultdef,compiler),tpointerdef(compiler.deftypes.charpointertype),left.location.reference);
                ctx.hlcg.a_load_const_ref(ctx.CurrAsmList,compiler.deftypes.cansichartype,0,left.location.reference);
              end
            { char loading }
            else if is_char(right.resultdef) then
              begin
                if right.nodetype=ordconstn then
                  begin
                    ctx.hlcg.g_ptrtypecast_ref(ctx.CurrAsmList,cpointerdef.getreusable(left.resultdef,compiler),cpointerdef.getreusable(compiler.deftypes.u16inttype,compiler),left.location.reference);
                    if (compiler.target.info.endian = endian_little) then
                      ctx.hlcg.a_load_const_ref(ctx.CurrAsmList,compiler.deftypes.u16inttype,(tordconstnode(right).value.svalue shl 8) or 1,
                          setalignment(left.location.reference,1))
                    else
                      ctx.hlcg.a_load_const_ref(ctx.CurrAsmList,compiler.deftypes.u16inttype,tordconstnode(right).value.svalue or (1 shl 8),
                          setalignment(left.location.reference,1));
                  end
                else
                  begin
                    href:=left.location.reference;
                    ctx.hlcg.g_ptrtypecast_ref(ctx.CurrAsmList,cpointerdef.getreusable(left.resultdef,compiler),tpointerdef(compiler.deftypes.charpointertype),href);
                    ctx.hlcg.a_load_const_ref(ctx.CurrAsmList,compiler.deftypes.cansichartype,1,href);
                    inc(href.offset,1);
                    href.alignment:=1;
                    case right.location.loc of
                      LOC_REGISTER,
                      LOC_CREGISTER :
                        begin
{$ifndef cpuhighleveltarget}
                          r:=ctx.cg.makeregsize(ctx.CurrAsmList,right.location.register,OS_8);
{$else not cpuhighleveltarget}
                          r:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.u8inttype);
                          ctx.hlcg.a_load_reg_reg(ctx.CurrAsmList,compiler.deftypes.cansichartype,compiler.deftypes.u8inttype,right.location.register,r);
{$endif cpuhighleveltarget}
                          ctx.hlcg.a_load_reg_ref(ctx.CurrAsmList,compiler.deftypes.u8inttype,compiler.deftypes.u8inttype,r,href);
                        end;
                      LOC_REFERENCE,
                      LOC_CREFERENCE :
                        ctx.hlcg.a_load_ref_ref(ctx.CurrAsmList,compiler.deftypes.cansichartype,compiler.deftypes.cansichartype,right.location.reference,href);
                      else
                        internalerror(200205111);
                    end;
                  end;
              end
            else
              internalerror(2002042410);
          end
       { try to reuse memory locations instead of copying }
       { copy to a memory location ... }
        else if (right.location.loc = LOC_REFERENCE) and
           maybechangetemp(ctx.CurrAsmList,left,right.location.reference,ctx) then
          begin
            { if it worked, we're done }
          end
        else
          begin
            { SSA support }
            ctx.hlcg.maybe_change_load_node_reg(ctx.CurrAsmList,left,false);
            ctx.hlcg.maybe_change_load_node_reg(ctx.CurrAsmList,right,true);
            case right.location.loc of
              LOC_CONSTANT :
                begin
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                  if (left.location.size in [OS_64,OS_S64]) or (right.location.size in [OS_64,OS_S64]) then
                    ctx.cg64.a_load64_const_loc(ctx.CurrAsmList,right.location.value64,left.location)
                  else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                    ctx.hlcg.a_load_const_loc(ctx.CurrAsmList,left.resultdef,right.location.value,left.location);
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  case left.location.loc of
                    LOC_REGISTER,
                    LOC_CREGISTER :
                      begin
{$ifndef cpuhighleveltarget}
{$ifdef cpu64bitalu}
                        if left.location.size in [OS_128,OS_S128] then
                          ctx.cg128.a_load128_ref_reg(ctx.CurrAsmList,right.location.reference,left.location.register128)
                        else
{$else cpu64bitalu}
                        if left.location.size in [OS_64,OS_S64] then
                          ctx.cg64.a_load64_ref_reg(ctx.CurrAsmList,right.location.reference,left.location.register64)
                        else
{$endif cpu64bitalu}
{$endif not cpuhighleveltarget}
                          ctx.hlcg.a_load_ref_reg(ctx.CurrAsmList,right.resultdef,left.resultdef,right.location.reference,left.location.register);
                      end;
                    LOC_FPUREGISTER,
                    LOC_CFPUREGISTER :
                      begin
                        ctx.hlcg.a_loadfpu_ref_reg(ctx.CurrAsmList,
                            right.resultdef,left.resultdef,
                            right.location.reference,
                            left.location.register);
                      end;
                    LOC_REFERENCE,
                    LOC_CREFERENCE :
                      begin
                        if (left.resultdef.typ=floatdef) and
                           (right.resultdef.typ=floatdef) and
                           ((left.location.size<>right.location.size)
                           { on newer (1993+ :)) x86 cpus, use the fpu to copy extended values }
{$ifdef x86}
                            or ({$ifndef x86_64}(compiler.globals.current_settings.cputype>=cpu_Pentium) and{$endif x86_64}
                            (is_extended(right.resultdef) {$ifdef i386} or is_double(right.resultdef){$endif i386} ))
{$endif x86}
                           )then
                          begin
                            { assume that all float types can be handed by the
                              fpu if one can be handled by the fpu }
                            if not use_vectorfpu(left.resultdef) or
                               not use_vectorfpu(right.resultdef) then
                              ctx.hlcg.a_loadfpu_ref_ref(ctx.CurrAsmList,
                                right.resultdef,left.resultdef,
                                right.location.reference,left.location.reference)
                            else
                              ctx.hlcg.a_loadmm_ref_ref(ctx.CurrAsmList,
                                right.resultdef,left.resultdef,
                                right.location.reference,left.location.reference,mms_movescalar)
                          end
                        else
                          begin
{ TODO: HACK: unaligned test, maybe remove all unaligned locations (array of char) from the compiler}
                            { Use unaligned copy when the offset is not aligned }
                            len:=left.resultdef.size;
                            { can be 0 in case of formaldef on JVM target }
                            if len=0 then
                              len:=sizeof(pint);

                            { data smaller than an aint has less alignment requirements }
                            { max(1,...) avoids div by zero in case of an empty record  }
                            alignmentrequirement:=min(max(1,len),sizeof(aint));

                            if (right.location.reference.offset mod alignmentrequirement<>0) or
                              (left.location.reference.offset mod alignmentrequirement<>0) or
                              (right.resultdef.alignment<alignmentrequirement) or
                              ((right.location.reference.alignment<>0) and
                               (right.location.reference.alignment<alignmentrequirement)) or
                              ((left.location.reference.alignment<>0) and
                               (left.location.reference.alignment<alignmentrequirement)) then
                              ctx.hlcg.g_concatcopy_unaligned(ctx.CurrAsmList,left.resultdef,right.location.reference,left.location.reference)
                            else
                              ctx.hlcg.g_concatcopy(ctx.CurrAsmList,left.resultdef,right.location.reference,left.location.reference);
                          end;
                      end;
                    LOC_MMREGISTER,
                    LOC_CMMREGISTER:
                      begin
{$if defined(x86) and not defined(llvm)}
                        if (right.resultdef.typ=floatdef) and
                           not use_vectorfpu(right.resultdef) then
                          begin
                            { perform size conversion if needed (the mm-code cannot }
                            { convert an extended into a double/single, since sse   }
                            { doesn't support extended)                             }
                            r:=ctx.cg.getfpuregister(ctx.CurrAsmList,right.location.size);
                            ctx.tg.gethltemp(ctx.CurrAsmList,left.resultdef,left.resultdef.size,tt_normal,href);
                            ctx.cg.a_loadfpu_ref_reg(ctx.CurrAsmList,right.location.size,right.location.size,right.location.reference,r);
                            ctx.cg.a_loadfpu_reg_ref(ctx.CurrAsmList,right.location.size,left.location.size,r,href);
                            if releaseright then
                              ctx.tg.location_freetemp(ctx.CurrAsmList,right.location);
                            releaseright:=true;
                            location_reset_ref(right.location,LOC_REFERENCE,left.location.size,0,[]);
                            right.location.reference:=href;
                            right.resultdef:=left.resultdef;
                          end;
{$endif}
                        ctx.hlcg.a_loadmm_ref_reg(ctx.CurrAsmList,
                          right.resultdef,
                          left.resultdef,
                          right.location.reference,
                          left.location.register,mms_movescalar);
                      end;
                    LOC_SUBSETREG,
                    LOC_CSUBSETREG:
                      ctx.hlcg.a_load_ref_subsetreg(ctx.CurrAsmList,right.resultdef,left.resultdef,right.location.reference,left.location.sreg);
                    LOC_SUBSETREF,
                    LOC_CSUBSETREF:
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                      if right.location.size in [OS_64,OS_S64] then
                       ctx.cg64.a_load64_ref_subsetref(ctx.CurrAsmList,right.location.reference,left.location.sref)
                      else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                       ctx.hlcg.a_load_ref_subsetref(ctx.CurrAsmList,right.resultdef,left.resultdef,right.location.reference,left.location.sref);
                    else
                      internalerror(200203284);
                  end;
                end;
{$ifdef SUPPORT_MMX}
              LOC_CMMXREGISTER,
              LOC_MMXREGISTER:
                begin
                  if left.location.loc=LOC_CMMXREGISTER then
                    ctx.cg.a_loadmm_reg_reg(ctx.CurrAsmList,OS_M64,OS_M64,right.location.register,left.location.register,nil)
                  else
                    ctx.cg.a_loadmm_reg_ref(ctx.CurrAsmList,OS_M64,OS_M64,right.location.register,left.location.reference,nil);
                end;
{$endif SUPPORT_MMX}
              LOC_MMREGISTER,
              LOC_CMMREGISTER:
                begin
                  if (is_vector(left.resultdef)) then
                    shuffle := nil
                  else
                    shuffle := mms_movescalar;

                  case left.location.loc of
                    LOC_CMMREGISTER,
                    LOC_MMREGISTER:
                      ctx.hlcg.a_loadmm_reg_reg(ctx.CurrAsmList,right.resultdef,left.resultdef,right.location.register,left.location.register, shuffle);
                    LOC_REFERENCE,
                    LOC_CREFERENCE:
                      ctx.hlcg.a_loadmm_reg_ref(ctx.CurrAsmList,right.resultdef,left.resultdef,right.location.register,left.location.reference, shuffle);
                    else
                      internalerror(2009112601);
                  end;
                end;
              LOC_REGISTER,
              LOC_CREGISTER :
                begin
{$ifndef cpuhighleveltarget}
{$ifdef cpu64bitalu}
                  if left.location.size in [OS_128,OS_S128] then
                    ctx.cg128.a_load128_reg_loc(ctx.CurrAsmList,
                      right.location.register128,left.location)
                  else
{$else cpu64bitalu}
                  { also OS_F64 in case of mmreg -> intreg }
                  if left.location.size in [OS_64,OS_S64,OS_F64] then
                    ctx.cg64.a_load64_reg_loc(ctx.CurrAsmList,
                      right.location.register64,left.location)
                  else
{$endif cpu64bitalu}
{$endif not cpuhighleveltarget}
{$if defined(i8086) or defined(wasm32)}
                  { prefer a_load_loc_ref, because it supports i8086-specific types
                    that use registerhi (like 6-byte method pointers). The same
                    applies to WebAssembly, which has a 64-bit ALU, but keeps
                    method pointers in a register pair, because that's more
                    convenient.
                    (todo: maybe we should add a_load_loc_loc?) }
                  if left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
                    ctx.hlcg.a_load_loc_ref(ctx.CurrAsmList,right.resultdef,left.resultdef,right.location,left.location.reference)
                  else
{$endif}
                    ctx.hlcg.a_load_reg_loc(ctx.CurrAsmList,right.resultdef,left.resultdef,right.location.register,left.location);
                end;
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER :
                begin
                  { we can't do direct moves between fpu and mm registers }
                  if left.location.loc in [LOC_MMREGISTER,LOC_CMMREGISTER] then
                    begin
{$if defined(x86) and not defined(llvm)}
                      if not use_vectorfpu(right.resultdef) then
                        begin
                          { perform size conversion if needed (the mm-code cannot convert an   }
                          { extended into a double/single, since sse doesn't support extended) }
                          ctx.tg.gethltemp(ctx.CurrAsmList,left.resultdef,left.resultdef.size,tt_normal,href);
                          ctx.cg.a_loadfpu_reg_ref(ctx.CurrAsmList,right.location.size,left.location.size,right.location.register,href);
                          location_reset_ref(right.location,LOC_REFERENCE,left.location.size,0,[]);
                          right.location.reference:=href;
                          right.resultdef:=left.resultdef;
                        end;
{$endif}
                      ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,right.location,right.resultdef,false);
                      ctx.hlcg.a_loadmm_reg_reg(ctx.CurrAsmList,
                          right.resultdef,left.resultdef,
                          right.location.register,left.location.register,mms_movescalar);
                    end
                  else
                    ctx.hlcg.a_loadfpu_reg_loc(ctx.CurrAsmList,
                        right.resultdef,left.resultdef,
                        right.location.register,left.location);
                end;
              LOC_SUBSETREG,
              LOC_CSUBSETREG:
                begin
                  ctx.hlcg.a_load_subsetreg_loc(ctx.CurrAsmList,
                      right.resultdef,left.resultdef,right.location.sreg,left.location);
                end;
              LOC_SUBSETREF,
              LOC_CSUBSETREF:
                begin
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                  if right.location.size in [OS_64,OS_S64] then
                   ctx.cg64.a_load64_subsetref_loc(ctx.CurrAsmList,right.location.sref,left.location)
                  else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                  ctx.hlcg.a_load_subsetref_loc(ctx.CurrAsmList,
                      right.resultdef,left.resultdef,right.location.sref,left.location);
                end;
              LOC_JUMP :
                begin
                  ctx.CurrAsmList.AsmData.getjumplabel(hlabel);
                  ctx.hlcg.a_label(ctx.CurrAsmList,right.location.truelabel);
                  if is_pasbool(left.resultdef) then
                    begin
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                      if left.location.size in [OS_64,OS_S64] then
                        ctx.cg64.a_load64_const_loc(ctx.CurrAsmList,1,left.location)
                      else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                        ctx.hlcg.a_load_const_loc(ctx.CurrAsmList,left.resultdef,1,left.location)
                    end
                  else
                    begin
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                      if left.location.size in [OS_64,OS_S64] then
                        ctx.cg64.a_load64_const_loc(ctx.CurrAsmList,-1,left.location)
                      else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                        ctx.hlcg.a_load_const_loc(ctx.CurrAsmList,left.resultdef,-1,left.location);
                    end;

                  ctx.hlcg.a_jmp_always(ctx.CurrAsmList,hlabel);
                  ctx.hlcg.a_label(ctx.CurrAsmList,right.location.falselabel);
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                  if left.location.size in [OS_64,OS_S64] then
                    ctx.cg64.a_load64_const_loc(ctx.CurrAsmList,0,left.location)
                  else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                    ctx.hlcg.a_load_const_loc(ctx.CurrAsmList,left.resultdef,0,left.location);
                  ctx.hlcg.a_label(ctx.CurrAsmList,hlabel);
                end;
{$ifdef cpuflags}
              LOC_FLAGS :
                begin
                  { check for cbool here as booleans converted to other types shall be handled as pas booleans,
                    see also tests/webtbs/tw40908.pp }
                  if is_cbool(left.resultdef) then
                    begin
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                      if left.location.size in [OS_S64,OS_64] then
                        begin
                          r64.reglo:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
                          r64.reghi:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
                          ctx.cg.g_flags2reg(ctx.CurrAsmList,OS_32,right.location.resflags,r64.reglo);
                          ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                          ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,0,r64.reghi);
                          ctx.cg64.a_op64_reg_reg(ctx.CurrAsmList,OP_NEG,OS_S64,
                            r64,r64);
                          ctx.cg64.a_load64_reg_loc(ctx.CurrAsmList,r64,left.location);
                        end
                      else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                        begin
                          r:=ctx.cg.getintregister(ctx.CurrAsmList,left.location.size);
                          ctx.cg.g_flags2reg(ctx.CurrAsmList,left.location.size,right.location.resflags,r);
                          ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                          ctx.cg.a_op_reg_reg(ctx.CurrAsmList,OP_NEG,left.location.size,r,r);
                          ctx.hlcg.a_load_reg_loc(ctx.CurrAsmList,left.resultdef,left.resultdef,r,left.location);
                        end
                    end
                  else
                    begin
                      case left.location.loc of
                        LOC_REGISTER,LOC_CREGISTER:
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
                          if left.location.size in [OS_S64,OS_64] then
                            begin
                              ctx.cg.g_flags2reg(ctx.CurrAsmList,OS_32,right.location.resflags,left.location.register64.reglo);
                              ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                              ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,0,left.location.register64.reghi);
                            end
                          else
{$endif not cpu64bitalu and not cpuhighleveltarget}
                            begin
                              ctx.cg.g_flags2reg(ctx.CurrAsmList,left.location.size,right.location.resflags,left.location.register);
                              ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                            end;
                        LOC_CREFERENCE,
                        LOC_REFERENCE:
                        { i8086 and i386 have hacks in their code generators so that they can
                          deal with 64 bit locations in this particular case }
{$if not defined(cpu64bitalu) and not defined(x86) and not defined(cpuhighleveltarget)}
                          if left.location.size in [OS_S64,OS_64] then
                            begin
                              r64.reglo:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
                              r64.reghi:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
                              ctx.cg.g_flags2reg(ctx.CurrAsmList,OS_32,right.location.resflags,r64.reglo);
                              ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                              ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,0,r64.reghi);
                              ctx.cg64.a_load64_reg_ref(ctx.CurrAsmList,r64,left.location.reference);
                            end
                          else
{$endif not cpu64bitalu and not x86 and not cpuhighleveltarget}
                            begin
                              ctx.cg.g_flags2ref(ctx.CurrAsmList,left.location.size,right.location.resflags,left.location.reference);
                              ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                            end;
                        LOC_CSUBSETREG,LOC_SUBSETREG,LOC_SUBSETREF:
                          begin
                            r:=ctx.cg.getintregister(ctx.CurrAsmList,left.location.size);
                            ctx.cg.g_flags2reg(ctx.CurrAsmList,left.location.size,right.location.resflags,r);
                            ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                            ctx.hlcg.a_load_reg_loc(ctx.CurrAsmList,left.resultdef,left.resultdef,r,left.location);
                          end;
                        else
                          internalerror(200203273);
                      end;
                    end;
                end;
{$endif cpuflags}
              LOC_VOID:
                ;
              else
                internalerror(2019050706);
            end;
         end;

        if releaseright then
          ctx.tg.location_freetemp(ctx.CurrAsmList,right.location);
      end;


{*****************************************************************************
                           SecondArrayConstruct
*****************************************************************************}

      const
        vtInteger       = 0;
        vtBoolean       = 1;
        vtChar          = 2;
        vtExtended      = 3;
        vtString        = 4;
        vtPointer       = 5;
        vtPChar         = 6;
        vtObject        = 7;
        vtClass         = 8;
        vtWideChar      = 9;
        vtPWideChar     = 10;
        vtAnsiString32  = 11;
        vtCurrency      = 12;
        vtVariant       = 13;
        vtInterface     = 14;
        vtWideString    = 15;
        vtInt64         = 16;
        vtQWord         = 17;
        vtUnicodeString = 18;
        vtAnsiString16  = 19;
        vtAnsiString64  = 20;


    procedure tcgarrayconstructornode.makearrayref(var ref: treference; eledef: tdef; ctx:tpassgeneratecodecontext);
      begin
        { do nothing by default }
      end;


    procedure tcgarrayconstructornode.advancearrayoffset(var ref: treference; elesize: asizeint);
      begin
        ref.alignment:=newalignment(ref.alignment,elesize);
        inc(ref.offset,elesize);
      end;


    procedure tcgarrayconstructornode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
        hp    : tarrayconstructornode;
        href,
        fref  : treference;
        lt    : tdef;
        paraloc : tcgparalocation;
        varvtypefield,
        varfield : tfieldvarsym;
        vtype : longint;
        eledef: tdef;
        elesize : longint;
        tmpreg  : tregister;
        vaddr : boolean;
        freetemp,
        dovariant: boolean;
      begin
        if is_packed_array(resultdef) then
          internalerror(200608042);
        dovariant:=
          ((acnf_forcevaria in arrayconstructornodeflags) or is_variant_array(resultdef)) and
          not(compiler.target.info.system in systems_managed_vm);
        eledef:=tarraydef(resultdef).elementdef;
        elesize:=eledef.size;
        if dovariant then
          varvtypefield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VTYPE'))
        else
          varvtypefield:=nil;
        { alignment is filled in by ctx.tg.gethltemp below }
        location_reset_ref(location,LOC_CREFERENCE,OS_NO,0,[]);
        fillchar(paraloc,sizeof(paraloc),0);
        { Allocate always a temp, also if no elements are required, to
          be sure that location is valid (PFV) }
        { on the JVM platform, an array can have 0 elements; since the length
          of the array is part of the array itself, make sure we allocate one
          of the proper length to avoid getting unexpected results later --
          allocating a temp of size 0 also forces it to be size 4 on regular
          targets }
        ctx.tg.gethltemp(ctx.CurrAsmList,resultdef,(tarraydef(resultdef).highrange+1)*elesize,tt_normal,location.reference);
        href:=location.reference;
        makearrayref(href,eledef,ctx);
        { Process nodes in array constructor }
        hp:=self;
        while assigned(hp) do
         begin
           if assigned(hp.left) then
            begin
              freetemp:=true;
              secondpass(hp.left,ctx);
              if (hp.left.location.loc=LOC_JUMP)<>
                 (hp.left.expectloc=LOC_JUMP) then
                internalerror(2007103101);
              { Move flags and jump in register }
              if hp.left.location.loc in [LOC_FLAGS,LOC_JUMP] then
                ctx.hlcg.location_force_reg(ctx.CurrAsmList,hp.left.location,hp.left.resultdef,hp.left.resultdef,false);

              if dovariant then
               begin
                 { find the correct vtype value }
                 vtype:=$ff;
                 varfield:=nil;
                 vaddr:=false;
                 lt:=hp.left.resultdef;
                 case lt.typ of
                   enumdef,
                   orddef :
                     begin
                       if is_64bit(lt) then
                         begin
                            case torddef(lt).ordtype of
                              scurrency:
                                begin
                                  vtype:=vtCurrency;
                                  varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VCURRENCY'));
                                end;
                              s64bit:
                                begin
                                  vtype:=vtInt64;
                                  varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VINT64'));
                                end;
                              u64bit:
                                begin
                                  vtype:=vtQWord;
                                  varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VQWORD'));
                                end;
                              else
                                ;
                            end;
                            freetemp:=false;
                            vaddr:=true;
                         end
                       else if (lt.typ=enumdef) or
                           is_integer(lt) then
                         begin
                           vtype:=vtInteger;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VINTEGER'));
                         end
                       else
                         if is_boolean(lt) then
                           begin
                             vtype:=vtBoolean;
                             varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VINTEGER'));
                           end
                         else
                           if (lt.typ=orddef) then
                             begin
                               case torddef(lt).ordtype of
                                 uchar:
                                   begin
                                     vtype:=vtChar;
                                     varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VINTEGER'));
                                   end;
                                 uwidechar:
                                   begin
                                     vtype:=vtWideChar;
                                     varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VINTEGER'));
                                   end;
                                 else
                                   ;
                               end;
                             end;
                     end;
                   floatdef :
                     begin
                       if is_currency(lt) then
                         begin
                           vtype:=vtCurrency;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VCURRENCY'));
                         end
                       else
                         begin
                           vtype:=vtExtended;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VEXTENDED'));
                         end;
                       freetemp:=false;
                       vaddr:=true;
                     end;
                   procvardef,
                   pointerdef :
                     begin
                       if is_pchar(lt) then
                         begin
                           vtype:=vtPChar;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VPCHAR'));
                         end
                       else if is_pwidechar(lt) then
                         begin
                           vtype:=vtPWideChar;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VPWIDECHAR'));
                         end
                       else
                         begin
                           vtype:=vtPointer;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VPOINTER'));
                         end;
                     end;
                   variantdef :
                     begin
                        vtype:=vtVariant;
                        varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VVARIANT'));
                        vaddr:=true;
                        freetemp:=false;
                     end;
                   classrefdef :
                     begin
                       vtype:=vtClass;
                       varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VCLASS'));
                     end;
                   objectdef :
                     if is_interface(lt) then
                       begin
                         vtype:=vtInterface;
                         varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VINTERFACE'));
                       end
                     { vtObject really means a class based on TObject }
                     else if is_class(lt) then
                       begin
                         vtype:=vtObject;
                         varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VOBJECT'));
                       end
                     else
                       internalerror(200505171);
                   stringdef :
                     begin
                       if is_shortstring(lt) then
                        begin
                          vtype:=vtString;
                          varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VSTRING'));
                          vaddr:=true;
                          freetemp:=false;
                        end
                       else
                        if is_ansistring(lt) then
                         begin
                           vtype:=vtAnsiString;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VANSISTRING'));
                           freetemp:=false;
                         end
                       else
                        if is_widestring(lt) then
                         begin
                           vtype:=vtWideString;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VWIDESTRING'));
                           freetemp:=false;
                         end
                       else
                        if is_unicodestring(lt) then
                         begin
                           vtype:=vtUnicodeString;
                           varfield:=tfieldvarsym(search_struct_member_no_helper(trecorddef(eledef),'VUNICODESTRING'));
                           freetemp:=false;
                         end;
                     end;
                   else
                     ;
                 end;
                 if vtype=$ff then
                   internalerror(14357);
                 if not assigned(varfield) then
                   internalerror(2015102901);
                 { write changing field update href to the next element }
                 fref:=href;
                 ctx.hlcg.g_set_addr_nonbitpacked_field_ref(ctx.CurrAsmList,trecorddef(eledef),varfield,fref);
                 if vaddr then
                   begin
                     ctx.hlcg.location_force_mem(ctx.CurrAsmList,hp.left.location,lt);
                     tmpreg:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,cpointerdef.getreusable(lt,compiler));
                     ctx.hlcg.a_loadaddr_ref_reg(ctx.CurrAsmList,hp.left.resultdef,cpointerdef.getreusable(lt,compiler),hp.left.location.reference,tmpreg);
                     ctx.hlcg.a_load_reg_ref(ctx.CurrAsmList,cpointerdef.getreusable(lt,compiler),varfield.vardef,tmpreg,fref);
                   end
                 else
                   ctx.hlcg.a_load_loc_ref(ctx.CurrAsmList,hp.left.resultdef,varfield.vardef,hp.left.location,fref);
                 { update href to the vtype field and write it }
                 fref:=href;
                 ctx.hlcg.g_set_addr_nonbitpacked_field_ref(ctx.CurrAsmList,trecorddef(eledef),varvtypefield,fref);
                 ctx.hlcg.a_load_const_ref(ctx.CurrAsmList,varvtypefield.vardef,vtype,fref);
                 { goto next array element }
                 advancearrayoffset(href,elesize);
               end
              else
              { normal array constructor of the same type }
               begin
                 if is_managed_type(resultdef) then
                   freetemp:=false;
                 case hp.left.location.loc of
                   LOC_MMREGISTER,
                   LOC_CMMREGISTER:
                     ctx.hlcg.a_loadmm_reg_ref(ctx.CurrAsmList,hp.left.resultdef,hp.left.resultdef,
                       hp.left.location.register,href,mms_movescalar);
                   LOC_FPUREGISTER,
                   LOC_CFPUREGISTER :
                     ctx.hlcg.a_loadfpu_reg_ref(ctx.CurrAsmList,hp.left.resultdef,hp.left.resultdef,hp.left.location.register,href);
                   LOC_REFERENCE,
                   LOC_CREFERENCE :
                     begin
                       if is_shortstring(hp.left.resultdef) then
                         ctx.hlcg.g_copyshortstring(ctx.CurrAsmList,hp.left.location.reference,href,
                             Tstringdef(hp.left.resultdef))
                       else
                         ctx.hlcg.g_concatcopy(ctx.CurrAsmList,eledef,hp.left.location.reference,href);
                     end;
                   else
                     begin
{$ifndef cpuhighleveltarget}
{$ifdef cpu64bitalu}
                       if hp.left.location.size in [OS_128,OS_S128] then
                         ctx.cg128.a_load128_loc_ref(ctx.CurrAsmList,hp.left.location,href)
                       else
{$else cpu64bitalu}
                       if hp.left.location.size in [OS_64,OS_S64] then
                         ctx.cg64.a_load64_loc_ref(ctx.CurrAsmList,hp.left.location,href)
                       else
{$endif cpu64bitalu}
{$endif not cpuhighleveltarget}
                         ctx.hlcg.a_load_loc_ref(ctx.CurrAsmList,eledef,eledef,hp.left.location,href);
                     end;
                 end;
                 advancearrayoffset(href,elesize);
               end;
              if freetemp then
                ctx.tg.location_freetemp(ctx.CurrAsmList,hp.left.location);
            end;
           { load next entry }
           hp:=tarrayconstructornode(hp.right);
         end;
      end;


{*****************************************************************************
                           SecondRTTI
*****************************************************************************}

    procedure tcgrttinode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
        indirect : boolean;
      begin
        indirect := (tf_supports_packages in compiler.target.info.flags) and
                      (compiler.target.info.system in systems_indirect_var_imports) and
                      (cs_imported_data in localswitches) and
                      (rttidef.owner.moduleid<>compiler.current_module.moduleid);

        location_reset_ref(location,LOC_CREFERENCE,OS_NO,sizeof(pint),[]);
        case rttidatatype of
          rdt_normal:
            location.reference.symbol:=compiler.RTTIWriter.get_rtti_label(ctx.CurrAsmList.AsmData,rttidef,rttitype,indirect);
          rdt_ord2str:
            location.reference.symbol:=compiler.RTTIWriter.get_rtti_label_ord2str(ctx.CurrAsmList.AsmData,rttidef,rttitype,indirect);
          rdt_str2ord:
            location.reference.symbol:=compiler.RTTIWriter.get_rtti_label_str2ord(ctx.CurrAsmList.AsmData,rttidef,rttitype,indirect);
        end;
      end;



begin
   cloadnode:=tcgloadnode;
   cassignmentnode:=tcgassignmentnode;
   carrayconstructornode:=tcgarrayconstructornode;
   crttinode:=tcgrttinode;
end.

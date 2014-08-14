{
    Copyright (c) 20011 by Jonas Maebe

    JVM version of some node tree helper routines

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
unit njvmutil;

{$i fpcdefs.inc}

interface

  uses
    node,nbas,
    ngenutil,
    symtype,symconst,symsym,symdef;


  type
    tjvmnodeutils = class(tnodeutils)
      class function initialize_data_node(p:tnode; force: boolean):tnode; override;
      class function finalize_data_node(p:tnode):tnode; override;
      class function force_init: boolean; override;
      class procedure insertbssdata(sym: tstaticvarsym); override;
      class function create_main_procdef(const name: string; potype: tproctypeoption; ps: tprocsym): tdef; override;

      class function check_insert_trashing(pd: tprocdef): boolean; override;
      class function  trashable_sym(p: tsym): boolean; override;
      class procedure maybe_trash_variable(var stat: tstatementnode; p: tabstractnormalvarsym; trashn: tnode); override;

      class procedure InsertInitFinalTable; override;
      class procedure InsertThreadvarTablesTable; override;
      class procedure InsertThreadvars; override;
      class procedure InsertWideInitsTablesTable; override;
      class procedure InsertWideInits; override;
      class procedure InsertResourceTablesTable; override;
      class procedure InsertResourceInfo(ResourcesUsed : boolean); override;
      class procedure InsertMemorySizes; override;
     strict protected
       class procedure add_main_procdef_paras(pd: tdef); override;
    end;


implementation

    uses
      verbose,cutils,globtype,globals,constexp,fmodule,
      aasmdata,aasmtai,cpubase,aasmcpu,
      symbase,symcpu,symtable,defutil,jvmdef,
      ncnv,ncon,ninl,ncal,nld,nmem,
      ppu,
      pass_1;

  class function tjvmnodeutils.initialize_data_node(p:tnode; force: boolean):tnode;
    var
      normaldim: longint;
      temp: ttempcreatenode;
      stat: tstatementnode;
      def: tdef;
      paras: tcallparanode;
      proc: string;
    begin
      if not assigned(p.resultdef) then
        typecheckpass(p);
      if ((p.resultdef.typ=stringdef) and
          not is_shortstring(p.resultdef) and
          not is_longstring(p.resultdef)) or
         is_dynamic_array(p.resultdef) then
        begin
          { Always initialise with empty string/array rather than nil. Java
            makes a distinction between an empty string/array and a null
            string/array,  but we don't. We therefore have to pick which one we
            use to represent empty strings/arrays. I've chosen empty rather than
            null structures, because otherwise it becomes impossible to return
            an empty string to Java code (it would return null).

            On the consumer side, we do interpret both null and empty as the same
            thing, so Java code can pass in null strings/arrays and we'll
            interpret them correctly.
          }
          result:=cinlinenode.create(in_setlength_x,false,
            ccallparanode.create(genintconstnode(0),
              ccallparanode.create(p,nil)));
        end
      else if force then
        begin
          { an explicit call to initialize() }
          if p.resultdef.typ=recorddef then
            result:=ccallnode.createinternmethod(p,'FPCINITIALIZEREC',nil)
          else if p.resultdef.typ=arraydef then
            begin
              stat:=nil;
              { in case it's an open array whose elements are regular arrays, put the
                dimension of the regular arrays on the stack (otherwise pass 0) }
              normaldim:=0;
              def:=tarraydef(p.resultdef).elementdef;
              while (def.typ=arraydef) and
                    not is_dynamic_array(def) do
                begin
                  inc(normaldim);
                  def:=tarraydef(def).elementdef;
                end;
              if jvmimplicitpointertype(p.resultdef) then
                begin
                  p:=caddrnode.create(p);
                  include(p.flags,nf_typedaddr);
                end;
              paras:=ccallparanode.create(ctypeconvnode.create_explicit(p,
                search_system_type('TJOBJECTARRAY').typedef),nil);
              paras:=ccallparanode.create(genintconstnode(normaldim),paras);
              if is_wide_or_unicode_string(def) then
                proc:='fpc_initialize_array_unicodestring'
              else if is_ansistring(def) then
                proc:='fpc_initialize_array_ansistring'
              else if is_dynamic_array(def) then
                proc:='fpc_initialize_array_dynarr'
              else if is_record(def) then
                begin
                  result:=internalstatements(stat);
                  temp:=ctempcreatenode.create(def,def.size,tt_persistent,true);
                  addstatement(stat,temp);
                  paras:=ccallparanode.create(ctemprefnode.create(temp),paras);
                  proc:='fpc_initialize_array_record'
                end;
              if assigned(stat) then
                begin
                  addstatement(stat,ccallnode.createintern(proc,paras));
                  addstatement(stat,ctempdeletenode.create(temp));
                end
              else
                result:=ccallnode.createintern(proc,paras);
            end
          else
            result:=cassignmentnode.create(p,cnilnode.create);
        end
      else
        begin
          p.free;
          { records/arrays/... are automatically initialised }
          result:=cnothingnode.create;
        end;
    end;


  class function tjvmnodeutils.finalize_data_node(p:tnode):tnode;
    begin
      // do nothing
      p.free;
      result:=cnothingnode.create;
    end;


  class function tjvmnodeutils.force_init: boolean;
    begin
      { we need an initialisation in case the al_globals list is not empty
        (that's where the initialisation for global records etc is added) }
      { problem: some bss symbols are only registered while processing the main
        program (e.g. constant sets) -> cannot predict whether or not we'll
        need it in advance }
      result:=true;
    end;

  class procedure tjvmnodeutils.insertbssdata(sym: tstaticvarsym);
    var
      enuminitsym,
      vs: tstaticvarsym;
      block: tblocknode;
      stat: tstatementnode;
      temp: ttempcreatenode;
      initnode: tnode;
      eledef: tdef;
      ndim: longint;
      initnodefinished: boolean;
    begin
      { handled while generating the unit/program init code, or class
        constructor; add something to al_globals to indicate that we need to
        insert an init section though }
      if current_asmdata.asmlists[al_globals].empty and
         jvmimplicitpointertype(sym.vardef) then
        current_asmdata.asmlists[al_globals].concat(cai_align.Create(1));
      { in case of a threadvar, allocate a separate sym that's a subtype of the
        java.lang.ThreadLocal class which will wrap the actual variable value }
      if vo_is_thread_var in sym.varoptions then
        begin
          vs:=cstaticvarsym.create(sym.realname+'$threadvar',sym.varspez,
            jvmgetthreadvardef(sym.vardef),
            sym.varoptions - [vo_is_thread_var]);
          sym.owner.insert(vs);
          { make sure that the new sym does not get allocated (we will allocate
            it when encountering the original sym, because only then we know
            that it's a threadvar) }
          include(vs.symoptions,sp_static);
          { switch around the mangled names of sym and vs, since the wrapper
            should map to the declared name }
          sym.set_mangledbasename(vs.realname);
          vs.set_mangledbasename(sym.realname);

          { add initialization code for the wrapper }
          block:=internalstatements(stat);
          if assigned(current_module.tcinitcode) then
            addstatement(stat,tnode(current_module.tcinitcode));
          current_module.tcinitcode:=block;

          { create initialization value if necessary }
          initnode:=nil;
          initnodefinished:=false;
          temp:=nil;
          { in case of enum type, initialize with enum(0) if it exists }
          if sym.vardef.typ=enumdef then
            begin
              enuminitsym:=tstaticvarsym(tcpuenumdef(tenumdef(sym.vardef).getbasedef).classdef.symtable.Find('__FPC_ZERO_INITIALIZER'));
              if assigned(enuminitsym) then
                initnode:=cloadnode.create(enuminitsym,enuminitsym.owner);
            end
          { normal array -> include dimensions and element type so we can
            create a deep copy }
          else if (sym.vardef.typ=arraydef) and
             not is_dynamic_array(sym.vardef) then
            begin
              temp:=ctempcreatenode.create(sym.vardef,sym.vardef.size,tt_persistent,true);
              addstatement(stat,temp);
              initnode:=ccallparanode.create(
                ctypeconvnode.create_explicit(
                  caddrnode.create_internal(ctemprefnode.create(temp)),
                  java_jlobject),
                nil);
              jvmgetarraydimdef(sym.vardef,eledef,ndim);
              initnode:=ccallparanode.create(genintconstnode(ndim),initnode);
              initnode:=ccallparanode.create(
                cordconstnode.create(ord(jvmarrtype_setlength(eledef)),
                  cwidechartype,false),
                initnode);
              initnodefinished:=true;
            end
          { implicitpointertype -> allocate (get temp and assign address) }
          else if jvmimplicitpointertype(sym.vardef) then
            begin
              temp:=ctempcreatenode.create(sym.vardef,sym.vardef.size,tt_persistent,true);
              addstatement(stat,temp);
              initnode:=caddrnode.create_internal(ctemprefnode.create(temp));
            end
          { unicodestring/ansistring -> empty string }
          else if is_wide_or_unicode_string(sym.vardef) or
             is_ansistring(sym.vardef) then
            begin
              temp:=ctempcreatenode.create(sym.vardef,sym.vardef.size,tt_persistent,true);
              addstatement(stat,temp);
              addstatement(stat,cassignmentnode.create(
                ctemprefnode.create(temp),
                cstringconstnode.createstr('')));
              initnode:=ctemprefnode.create(temp);
            end
          { dynamic array -> empty array }
          else if is_dynamic_array(sym.vardef) then
            begin
              temp:=ctempcreatenode.create(sym.vardef,sym.vardef.size,tt_persistent,true);
              addstatement(stat,temp);
              addstatement(stat,cinlinenode.create(in_setlength_x,false,
                ccallparanode.create(genintconstnode(0),
                  ccallparanode.create(ctemprefnode.create(temp),nil))
                )
              );
              initnode:=ctemprefnode.create(temp);
            end;

          if assigned(initnode) and
             not initnodefinished then
            initnode:=ccallparanode.create(ctypeconvnode.create_explicit(initnode,java_jlobject),nil);
          addstatement(stat,cassignmentnode.create(
            cloadnode.create(vs,vs.owner),
            ccallnode.createinternmethod(
              cloadvmtaddrnode.create(ctypenode.create(vs.vardef)),
              'CREATE',initnode)));
          { deallocate the temp if we allocated one }
          if assigned(temp) then
            addstatement(stat,ctempdeletenode.create(temp));
        end;
    end;


  class function tjvmnodeutils.create_main_procdef(const name: string; potype: tproctypeoption; ps: tprocsym): tdef;
    begin
      if (potype=potype_proginit) then
        begin
          result:=inherited create_main_procdef('main', potype, ps);
          include(tprocdef(result).procoptions,po_global);
          tprocdef(result).visibility:=vis_public;
        end
      else
        result:=inherited create_main_procdef(name, potype, ps);
    end;


  class function tjvmnodeutils.check_insert_trashing(pd: tprocdef): boolean;
    begin
      { initialise locals with 0 }
      if ts_init_locals in current_settings.targetswitches then
        localvartrashing:=high(trashintvalues);
      result:=inherited;
    end;


  class function tjvmnodeutils.trashable_sym(p: tsym): boolean;
    begin
      result:=
        inherited and
        not jvmimplicitpointertype(tabstractnormalvarsym(p).vardef);
    end;


  class procedure tjvmnodeutils.maybe_trash_variable(var stat: tstatementnode; p: tabstractnormalvarsym; trashn: tnode);
    var
      enumdef: tenumdef;
      trashintval: int64;
      trashenumval: longint;
      trashable: boolean;
    begin
      trashable:=trashable_sym(p);
      trashintval:=trashintvalues[localvartrashing];
      { widechar is a separate type in the JVM, can't cast left hand to integer
        like in common code }
      if trashable and
         is_widechar(tabstractvarsym(p).vardef) then
        trash_small(stat,trashn,
          cordconstnode.create(word(trashintval),tabstractvarsym(p).vardef,false))
      { enums are class instances in the JVM -> create a valid instance }
      else if trashable and
         is_enum(tabstractvarsym(p).vardef) then
        begin
          enumdef:=tenumdef(tabstractvarsym(p).vardef);
          trashenumval:=longint(trashintval);
          if not assigned(enumdef.int2enumsym(trashenumval)) then
            trashintval:=longint(enumdef.min);
          trash_small(stat,trashn,
            cordconstnode.create(trashintval,enumdef,false))
        end
      { can't init pointers with arbitrary values; procvardef and objectdef are
        always pointer-sized here because tjvmnodeutils.trashablesym returns
        false for jvm implicit pointer types }
      else if trashable and
         (tabstractvarsym(p).vardef.typ in [pointerdef,classrefdef,objectdef,procvardef]) then
        trash_small(stat,trashn,cnilnode.create)
      else if trashable and
         is_real(tabstractvarsym(p).vardef) then
        trash_small(stat,trashn,crealconstnode.create(trashintval,tabstractvarsym(p).vardef))
      { don't use inherited routines because it typecasts left to the target
        type, and that doesn't always work in the JVM }
      else if trashable and
         (is_integer(tabstractvarsym(p).vardef) or
          is_cbool(tabstractvarsym(p).vardef) or
          is_anychar(tabstractvarsym(p).vardef) or
          is_currency(tabstractvarsym(p).vardef)) then
        trash_small(stat,trashn,cordconstnode.create(trashintval,tabstractvarsym(p).vardef,false))
      else if trashable and
         is_pasbool(tabstractvarsym(p).vardef) then
        trash_small(stat,trashn,cordconstnode.create(trashintval and 1,tabstractvarsym(p).vardef,false))
      else
        inherited;
    end;

  class procedure tjvmnodeutils.InsertInitFinalTable;
    var
      hp : tused_unit;
      unitinits : TAsmList;
      unitclassname: string;
      mainpsym: tsym;
      mainpd: tprocdef;
    begin
      unitinits:=TAsmList.Create;
      hp:=tused_unit(usedunits.first);
      while assigned(hp) do
        begin
          { class constructors are automatically handled by the JVM }

          { call the unit init code and make it external }
          if (hp.u.flags and (uf_init or uf_finalize))<>0 then
            begin
              { trigger init code by referencing the class representing the
                unit; if necessary, it will register the fini code to run on
                exit}
              unitclassname:='';
              if assigned(hp.u.namespace) then
                begin
                  unitclassname:=hp.u.namespace^+'/';
                  replace(unitclassname,'.','/');
                end;
              unitclassname:=unitclassname+hp.u.realmodulename^;
              unitinits.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(unitclassname)));
              unitinits.concat(taicpu.op_none(a_pop));
            end;
          hp:=tused_unit(hp.next);
        end;
      { insert in main program routine }
      mainpsym:=tsym(current_module.localsymtable.find(mainaliasname));
      if not assigned(mainpsym) or
         (mainpsym.typ<>procsym) then
        internalerror(2011041901);
      mainpd:=tprocsym(mainpsym).find_procdef_bytype(potype_proginit);
      if not assigned(mainpd) then
        internalerror(2011041902);
      tcpuprocdef(mainpd).exprasmlist.insertList(unitinits);
      unitinits.free;
    end;


  class procedure tjvmnodeutils.InsertThreadvarTablesTable;
    begin
      { not yet supported }
    end;


  class procedure tjvmnodeutils.InsertThreadvars;
    begin
      { not yet supported }
    end;


  class procedure tjvmnodeutils.InsertWideInitsTablesTable;
    begin
      { not required }
    end;


  class procedure tjvmnodeutils.InsertWideInits;
    begin
      { not required }
    end;


  class procedure tjvmnodeutils.InsertResourceTablesTable;
    begin
      { not supported }
    end;


  class procedure tjvmnodeutils.InsertResourceInfo(ResourcesUsed: boolean);
    begin
      { not supported }
    end;


  class procedure tjvmnodeutils.InsertMemorySizes;
    begin
      { not required }
    end;


  class procedure tjvmnodeutils.add_main_procdef_paras(pd: tdef);
    var
      pvs: tparavarsym;
    begin
      if (tprocdef(pd).proctypeoption=potype_proginit) then
        begin
          { add the args parameter }
          pvs:=cparavarsym.create('$args',1,vs_const,search_system_type('TJSTRINGARRAY').typedef,[]);
          tprocdef(pd).parast.insert(pvs);
          tprocdef(pd).calcparas;
        end;
    end;


begin
  cnodeutils:=tjvmnodeutils;
end.


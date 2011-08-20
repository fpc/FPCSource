{
    Copyright (c) 1998-20011 by Florian Klaempfl

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
    node,symsym,symdef;


  type
    tnodeutils = class
      class function call_fail_node:tnode; virtual;
      class function initialize_data_node(p:tnode):tnode; virtual;
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

      class procedure insertbssdata(sym : tstaticvarsym); virtual;

    end;
    tnodeutilsclass = class of tnodeutils;

  const
    cnodeutils: tnodeutilsclass = tnodeutils;


implementation

    uses
      verbose,globtype,globals,cutils,constexp,
      scanner,systems,procinfo,fmodule,
      aasmbase,aasmdata,aasmtai,
      symconst,symtype,symbase,symtable,defutil,
      nadd,nbas,ncal,ncnv,ncon,nflw,nld,nmem,nobj,nutils,

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


  class function tnodeutils.initialize_data_node(p:tnode):tnode;
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
      newstatement : tstatementnode;
    begin
      if not assigned(p.resultdef) then
        typecheckpass(p);
      if is_ansistring(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_ansistr_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
        end
      else if is_widestring(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_widestr_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
        end
      else if is_unicodestring(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_unicodestr_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
        end
      else if is_interfacecom_or_dispinterface(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_intf_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
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


  class function tnodeutils.wrap_proc_body(pd: tprocdef; n: tnode): tnode;
    var
      stat: tstatementnode;
      block: tnode;
      psym: tsym;
      tcinitproc: tprocdef;
    begin
      result:=n;
      if target_info.system in systems_typed_constants_node_init then
        begin
          case pd.proctypeoption of
            potype_class_constructor:
              begin
                { even though the initialisation code for typed constants may
                  not yet be complete at this point (there may be more inside
                  method definitions coming after this class constructor), the
                  ones from inside the class definition have already been parsed.
                  in case of {$j-}, these are marked "final" in Java and such
                  static fields must be initialsed in the class constructor
                  itself -> add them here }
                block:=internalstatements(stat);
                if assigned(tabstractrecorddef(pd.owner.defowner).tcinitcode) then
                  begin
                    addstatement(stat,tabstractrecorddef(pd.owner.defowner).tcinitcode);
                    tabstractrecorddef(pd.owner.defowner).tcinitcode:=nil;
                  end;
                psym:=tsym(tabstractrecorddef(pd.owner.defowner).symtable.find('FPC_INIT_TYPED_CONSTS_HELPER'));
                if not assigned(psym) or
                   (psym.typ<>procsym) or
                   (tprocsym(psym).procdeflist.count<>1) then
                  internalerror(2011040301);
                tcinitproc:=tprocdef(tprocsym(psym).procdeflist[0]);
                addstatement(stat,ccallnode.create(nil,tprocsym(psym),
                  tabstractrecorddef(pd.owner.defowner).symtable,nil,[]));
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
                  if assigned(tabstractrecorddef(pd.owner.defowner).tcinitcode) then
                    begin
                      block:=internalstatements(stat);
                      addstatement(stat,tabstractrecorddef(pd.owner.defowner).tcinitcode);
                      tabstractrecorddef(pd.owner.defowner).tcinitcode:=nil;
                      addstatement(stat,result);
                      result:=block
                    end
                end;
            end;
          end;
        end;
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
      if (sym.owner.symtabletype=globalsymtable) or
         create_smartlink or
         DLLSource or
         (assigned(current_procinfo) and
          (po_inline in current_procinfo.procdef.procoptions)) or
         (vo_is_public in sym.varoptions) then
        list.concat(Tai_datablock.create_global(sym.mangledname,l))
      else
        list.concat(Tai_datablock.create(sym.mangledname,l));
      current_filepos:=storefilepos;
    end;


end.

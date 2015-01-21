{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Reads typed constants

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
unit ptconst;

{$i fpcdefs.inc}

interface

   uses symtype,symsym,aasmdata;

    procedure read_typed_const(list:tasmlist;sym:tstaticvarsym;in_structure:boolean);


implementation

    uses
       globtype,systems,globals,verbose,cutils,tokens,
       aasmbase,aasmtai,
       procinfo,fmodule,
       scanner,pbase,pdecvar,
       node,nbas,ngtcon,
       symconst,symbase,symdef
       ;

    procedure read_typed_const(list:tasmlist;sym:tstaticvarsym;in_structure:boolean);
      var
        storefilepos : tfileposinfo;
        cursectype   : TAsmSectionType;
        section      : ansistring;
        tcbuilder    : ttypedconstbuilder;
        datalist,
        reslist      : tasmlist;
        restree,
        previnit     : tnode;
      begin
        { mark the staticvarsym as typedconst }
        include(sym.varoptions,vo_is_typed_const);
        { The variable has a value assigned }
        sym.varstate:=vs_initialised;
        { the variable can't be placed in a register }
        sym.varregable:=vr_none;

        { generate data for typed const }
        storefilepos:=current_filepos;
        current_filepos:=sym.fileinfo;

        if not(target_info.system in systems_typed_constants_node_init) then
          begin
            maybe_new_object_file(list);
            tcbuilder:=tasmlisttypedconstbuilderclass(ctypedconstbuilder).create(sym);
            tasmlisttypedconstbuilder(tcbuilder).parse_into_asmlist(reslist,datalist);
            { Certain types like windows WideString are initialized at runtime and cannot
              be placed into readonly memory }
            if (sym.varspez=vs_const) and not (vo_force_finalize in sym.varoptions) then
              cursectype:=sec_rodata
            else
              cursectype:=sec_data;
            tcbuilder.free;
          end
        else
          begin
            if assigned(current_structdef) then
              previnit:=current_structdef.tcinitcode
            else
              previnit:=tnode(current_module.tcinitcode);
            tcbuilder:=tnodetreetypedconstbuilderclass(ctypedconstbuilder).create(sym,previnit);
            restree:=tnodetreetypedconstbuilder(tcbuilder).parse_into_nodetree;
            if assigned(current_structdef) then
              current_structdef.tcinitcode:=restree
            else
              current_module.tcinitcode:=restree;
            tcbuilder.free;
            reslist:=nil;
            datalist:=nil;
            cursectype:=sec_none;
          end;

        { Parse hints }
        try_consume_hintdirective(sym.symoptions,sym.deprecatedmsg);

        consume(_SEMICOLON);

        { parse public/external/export/... }
        if not in_structure and
           (
            (
             (token = _ID) and
             (idtoken in [_EXPORT,_EXTERNAL,_WEAKEXTERNAL,_PUBLIC,_CVAR]) and
             (m_cvar_support in current_settings.modeswitches)
            ) or
            (
             (m_mac in current_settings.modeswitches) and
             (
              (cs_external_var in current_settings.localswitches) or
              (cs_externally_visible in current_settings.localswitches)
             )
            )
           ) then
          read_public_and_external(sym);


        { try to parse a section directive }
        if not in_structure and (target_info.system in systems_allow_section) and
          (symtablestack.top.symtabletype in [staticsymtable,globalsymtable]) and
           (idtoken=_SECTION) then
               begin
                 try_consume_sectiondirective(section);
                 if section<>'' then
                   begin
                     if (sym.varoptions *[vo_is_external,vo_is_weak_external])<>[] then
                       Message(parser_e_externals_no_section);
                     if sym.typ<>staticvarsym then
                       Message(parser_e_section_no_locals);
                     tstaticvarsym(sym).section:=section;
                     include(sym.varoptions, vo_has_section);
                   end;
               end;

        if not(target_info.system in systems_typed_constants_node_init) then
          begin
            { only now add items based on the symbolname, because it may }
            { have been modified by the directives parsed above          }
            if vo_has_section in sym.varoptions then
              new_section(list,sec_user,sym.section,const_align(sym.vardef.alignment))
            else
              new_section(list,cursectype,lower(sym.mangledname),const_align(sym.vardef.alignment));
            if sym.globalasmsym then
              begin
                { see same code in ncgutil.insertbssdata }
                if (target_dbg.id=dbg_stabx) and
                   (cs_debuginfo in current_settings.moduleswitches) and
                   not assigned(current_asmdata.GetAsmSymbol(sym.name)) then
                  begin
                    list.concat(tai_symbol.Create(current_asmdata.DefineAsmSymbol(sym.name,AB_LOCAL,AT_DATA),0));
                    list.concat(tai_directive.Create(asd_reference,sym.name));
                  end;
                list.concat(Tai_symbol.Createname_global(sym.mangledname,AT_DATA,0))
              end
            else
              list.concat(Tai_symbol.Createname(sym.mangledname,AT_DATA,0));

            { add the parsed value }
            list.concatlist(reslist);
            reslist.free;
            list.concat(tai_symbol_end.Createname(sym.mangledname));
            { and pointed data, if any }
            current_asmdata.asmlists[al_const].concatlist(datalist);
            datalist.free;
          end
        else
          begin
            { nothing to do }
          end;

        current_filepos:=storefilepos;
      end;

end.

{
    Copyright (c) 2003-2006 by Peter Vreman and Florian Klaempfl

    This units contains the base class for debug info generation

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
unit dbgbase;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      systems,
      symconst,symbase,symdef,symtype,symsym,symtable,
      fmodule,
      aasmtai,aasmdata;

    type
      TDebugInfo=class
      protected
        { definitions }
        procedure appenddef(list:TAsmList;def:tdef);
        procedure beforeappenddef(list:TAsmList;def:tdef);virtual;
        procedure afterappenddef(list:TAsmList;def:tdef);virtual;
        procedure appenddef_ord(list:TAsmList;def:torddef);virtual;
        procedure appenddef_float(list:TAsmList;def:tfloatdef);virtual;
        procedure appenddef_file(list:TAsmList;def:tfiledef);virtual;
        procedure appenddef_enum(list:TAsmList;def:tenumdef);virtual;
        procedure appenddef_array(list:TAsmList;def:tarraydef);virtual;
        procedure appenddef_record(list:TAsmList;def:trecorddef);virtual;
        procedure appenddef_object(list:TAsmList;def:tobjectdef);virtual;
        procedure appenddef_pointer(list:TAsmList;def:tpointerdef);virtual;
        procedure appenddef_string(list:TAsmList;def:tstringdef);virtual;
        procedure appenddef_procvar(list:TAsmList;def:tprocvardef);virtual;
        procedure appenddef_variant(list:TAsmList;def:tvariantdef);virtual;
        procedure appenddef_set(list:TAsmList;def:tsetdef);virtual;
        procedure appenddef_formal(list:TAsmList;def:tformaldef);virtual;
        procedure appenddef_undefined(list:TAsmList;def: tundefineddef);virtual;
        procedure appendprocdef(list:TAsmList;def:tprocdef);virtual;
        { symbols }
        procedure appendsym(list:TAsmList;sym:tsym);
        procedure beforeappendsym(list:TAsmList;sym:tsym);virtual;
        procedure afterappendsym(list:TAsmList;sym:tsym);virtual;
        procedure appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);virtual;
        procedure appendsym_paravar(list:TAsmList;sym:tparavarsym);virtual;
        procedure appendsym_localvar(list:TAsmList;sym:tlocalvarsym);virtual;
        procedure appendsym_fieldvar(list:TAsmList;sym:tfieldvarsym);virtual;
        procedure appendsym_unit(list:TAsmList;sym:tunitsym);virtual;
        procedure appendsym_const(list:TAsmList;sym:tconstsym);virtual;
        procedure appendsym_type(list:TAsmList;sym:ttypesym);virtual;
        procedure appendsym_label(list:TAsmList;sym:tlabelsym);virtual;
        procedure appendsym_absolute(list:TAsmList;sym:tabsolutevarsym);virtual;
        procedure appendsym_property(list:TAsmList;sym:tpropertysym);virtual;
        { symtable }
        procedure write_symtable_syms(list:TAsmList;st:TSymtable);
        procedure write_symtable_defs(list:TAsmList;st:TSymtable);
        procedure write_symtable_procdefs(list:TAsmList;st:TSymtable);
        procedure reset_unit_type_info;
        procedure write_used_unit_type_info(list:TAsmList;hp:tmodule);
      public
        constructor Create;virtual;
        procedure inserttypeinfo;virtual;
        procedure insertmoduleinfo;virtual;
        procedure insertlineinfo(list:TAsmList);virtual;
        procedure referencesections(list:TAsmList);virtual;
      end;
      TDebugInfoClass=class of TDebugInfo;

    var
      CDebugInfo : array[tdbg] of TDebugInfoClass;
      current_debuginfo : tdebuginfo;

    procedure InitDebugInfo(hp:tmodule);
    procedure DoneDebugInfo(hp:tmodule);
    procedure RegisterDebugInfo(const r:tdbginfo;c:TDebugInfoClass);


implementation

    uses
      cutils,
      verbose;


    constructor TDebugInfo.Create;
      begin
      end;


    procedure TDebugInfo.insertmoduleinfo;
      begin
      end;


    procedure TDebugInfo.inserttypeinfo;
      begin
      end;


    procedure TDebugInfo.insertlineinfo(list:TAsmList);
      begin
      end;


    procedure TDebugInfo.referencesections(list:TAsmList);
      begin
      end;


{**************************************
          Definition
**************************************}

    procedure TDebugInfo.appendprocdef(list:TAsmList;def:tprocdef);
      begin
      end;


    procedure TDebugInfo.beforeappenddef(list:TAsmList;def:tdef);
      begin
      end;


    procedure TDebugInfo.afterappenddef(list:TAsmList;def:tdef);
      begin
      end;


    procedure TDebugInfo.appenddef_ord(list:TAsmList;def:torddef);
      begin
      end;


    procedure TDebugInfo.appenddef_float(list:TAsmList;def:tfloatdef);
      begin
      end;


    procedure TDebugInfo.appenddef_formal(list:TAsmList;def: tformaldef);
      begin
      end;


    procedure TDebugInfo.appenddef_undefined(list:TAsmList;def: tundefineddef);
      begin
      end;


    procedure TDebugInfo.appenddef_set(list:TAsmList;def: tsetdef);
      begin
      end;


    procedure TDebugInfo.appenddef_object(list:TAsmList;def: tobjectdef);
      begin
      end;


    procedure TDebugInfo.appenddef_variant(list:TAsmList;def: tvariantdef);
      begin
      end;


    procedure TDebugInfo.appenddef_enum(list:TAsmList;def:tenumdef);
      begin
      end;


    procedure TDebugInfo.appenddef_file(list:TAsmList;def: tfiledef);
      begin
      end;


    procedure TDebugInfo.appenddef_array(list:TAsmList;def:tarraydef);
      begin
      end;


    procedure TDebugInfo.appenddef_record(list:TAsmList;def:trecorddef);
      begin
      end;


    procedure TDebugInfo.appenddef_pointer(list:TAsmList;def:tpointerdef);
      begin
      end;


    procedure TDebugInfo.appenddef_string(list:TAsmList;def:tstringdef);
      begin
      end;


    procedure TDebugInfo.appenddef_procvar(list:TAsmList;def:tprocvardef);
      begin
      end;


    procedure TDebugInfo.appenddef(list:TAsmList;def:tdef);
      begin
        if (def.dbg_state in [dbg_state_writing,dbg_state_written]) then
          exit;
        { never write generic template defs }
        if df_generic in def.defoptions then
          begin
            def.dbg_state:=dbg_state_written;
            exit;
          end;
        { to avoid infinite loops }
        def.dbg_state := dbg_state_writing;
        beforeappenddef(list,def);
        case def.typ of
          stringdef :
            appenddef_string(list,tstringdef(def));
          enumdef :
            appenddef_enum(list,tenumdef(def));
          orddef :
            appenddef_ord(list,torddef(def));
          pointerdef :
            appenddef_pointer(list,tpointerdef(def));
          floatdef :
            appenddef_float(list,tfloatdef(def));
          filedef :
            appenddef_file(list,tfiledef(def));
          recorddef :
            appenddef_record(list,trecorddef(def));
          variantdef :
            appenddef_variant(list,tvariantdef(def));
          classrefdef :
            appenddef_pointer(list,tpointerdef(pvmttype));
          setdef :
            appenddef_set(list,tsetdef(def));
          formaldef :
            appenddef_formal(list,tformaldef(def));
          arraydef :
            appenddef_array(list,tarraydef(def));
          procvardef :
            appenddef_procvar(list,tprocvardef(def));
          objectdef :
            appenddef_object(list,tobjectdef(def));
          undefineddef :
            appenddef_undefined(list,tundefineddef(def));
        else
          internalerror(200601281);
        end;
        afterappenddef(list,def);
        def.dbg_state := dbg_state_written;
      end;


{**************************************
          Symbols
**************************************}

    procedure TDebugInfo.beforeappendsym(list:TAsmList;sym:tsym);
      begin
      end;


    procedure TDebugInfo.afterappendsym(list:TAsmList;sym:tsym);
      begin
      end;


    procedure TDebugInfo.appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);
      begin
      end;


    procedure TDebugInfo.appendsym_paravar(list:TAsmList;sym: tparavarsym);
      begin
      end;


    procedure TDebugInfo.appendsym_localvar(list:TAsmList;sym: tlocalvarsym);
      begin
      end;


    procedure TDebugInfo.appendsym_fieldvar(list:TAsmList;sym: tfieldvarsym);
      begin
      end;


    procedure TDebugInfo.appendsym_const(list:TAsmList;sym:tconstsym);
      begin
      end;


    procedure TDebugInfo.appendsym_label(list:TAsmList;sym: tlabelsym);
      begin
      end;


    procedure TDebugInfo.appendsym_property(list:TAsmList;sym: tpropertysym);
      begin
      end;


    procedure TDebugInfo.appendsym_type(list:TAsmList;sym: ttypesym);
      begin
      end;


    procedure TDebugInfo.appendsym_unit(list:TAsmList;sym: tunitsym);
      begin
      end;


    procedure TDebugInfo.appendsym_absolute(list:TAsmList;sym:tabsolutevarsym);
      begin
      end;


    procedure TDebugInfo.appendsym(list:TAsmList;sym:tsym);
      begin
        if sym.isdbgwritten then
          exit;
        beforeappendsym(list,sym);
        case sym.typ of
          staticvarsym :
            appendsym_staticvar(list,tstaticvarsym(sym));
          unitsym:
            appendsym_unit(list,tunitsym(sym));
          labelsym :
            appendsym_label(list,tlabelsym(sym));
          localvarsym :
            appendsym_localvar(list,tlocalvarsym(sym));
          paravarsym :
            appendsym_paravar(list,tparavarsym(sym));
          constsym :
            appendsym_const(list,tconstsym(sym));
          typesym :
            appendsym_type(list,ttypesym(sym));
          enumsym :
            { ignore enum syms, they are written by the owner }
            ;
          syssym :
            { ignore sys syms, they are only of internal use }
            ;
          procsym :
            { ignore proc syms, they are written by procdefs }
            ;
          absolutevarsym :
            appendsym_absolute(list,tabsolutevarsym(sym));
          propertysym :
            appendsym_property(list,tpropertysym(sym));
          else
            internalerror(200601242);
        end;
        afterappendsym(list,sym);
        sym.isdbgwritten:=true;
      end;


{**************************************
          Symtables
**************************************}

    procedure TDebugInfo.write_symtable_defs(list:TAsmList;st:TSymtable);
      var
        def : tdef;
        i   : longint;
      begin
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - Begin Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - Begin unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
        for i:=0 to st.DefList.Count-1 do
          begin
            def:=tdef(st.DefList[i]);
            if (def.dbg_state=dbg_state_used) then
              appenddef(list,def);
          end;
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - End Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - End unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
      end;


    procedure TDebugInfo.write_symtable_syms(list:TAsmList;st:TSymtable);
      var
        i   : longint;
        sym : tsym;
      begin
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - Begin Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - Begin unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if not(sp_hidden in sym.symoptions) and
               (not sym.isdbgwritten) then
              appendsym(list,sym);
          end;
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - End Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - End unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
      end;


    procedure TDebugInfo.write_symtable_procdefs(list:TAsmList;st:TSymtable);
      var
        i   : longint;
        def : tdef;
      begin
        for i:=0 to st.DefList.Count-1 do
          begin
            def:=tdef(st.DefList[i]);
            case def.typ of
              procdef :
                begin
                  appendprocdef(list,tprocdef(def));
                  if assigned(tprocdef(def).localst) then
                    write_symtable_procdefs(list,tprocdef(def).localst);
                end;
              objectdef :
                begin
                  write_symtable_procdefs(list,tobjectdef(def).symtable);
                end;
            end;
          end;
      end;


    procedure TDebugInfo.reset_unit_type_info;
      var
        hp : tmodule;
      begin
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
          begin
            hp.is_dbginfo_written:=false;
            hp:=tmodule(hp.next);
          end;
      end;


    procedure TDebugInfo.write_used_unit_type_info(list:TAsmList;hp:tmodule);
      var
        pu : tused_unit;
      begin
        pu:=tused_unit(hp.used_units.first);
        while assigned(pu) do
          begin
            if not pu.u.is_dbginfo_written then
              begin
                { prevent infinte loop for circular dependencies }
                pu.u.is_dbginfo_written:=true;
                { write type info from used units, use a depth first
                  strategy to reduce the recursion in writing all
                  dependent stabs }
                write_used_unit_type_info(list,pu.u);
                if assigned(pu.u.globalsymtable) then
                  write_symtable_defs(list,pu.u.globalsymtable);
              end;
            pu:=tused_unit(pu.next);
          end;
      end;


{****************************************************************************
                           Init / Done
****************************************************************************}

    procedure InitDebugInfo(hp:tmodule);
      begin
        if not assigned(CDebugInfo[target_dbg.id]) then
          begin
            Comment(V_Fatal,'cg_f_debuginfo_output_not_supported');
            exit;
          end;
        hp.DebugInfo:=CDebugInfo[target_dbg.id].Create;
      end;


    procedure DoneDebugInfo(hp:tmodule);
      begin
        if assigned(hp.DebugInfo) then
          begin
            hp.DebugInfo.Free;
            hp.DebugInfo:=nil;
          end;
      end;


    procedure RegisterDebugInfo(const r:tdbginfo;c:TDebugInfoClass);
      var
        t : tdbg;
      begin
        t:=r.id;
        if assigned(dbginfos[t]) then
          writeln('Warning: DebugInfo is already registered!')
        else
          Getmem(dbginfos[t],sizeof(tdbginfo));
        dbginfos[t]^:=r;
        CDebugInfo[t]:=c;
      end;


    const
      dbg_none_info : tdbginfo =
         (
           id     : dbg_none;
           idtxt  : 'NONE';
         );

initialization
  RegisterDebugInfo(dbg_none_info,TDebugInfo);
end.

{
    Copyright (c) 1998-2005 by Florian Klaempfl

    This unit handles the exports parsing

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
unit pexports;

{$i fpcdefs.inc}

interface

    { reads an exports statement in a library }
    procedure read_exports;

implementation

    uses
       { common }
       cutils,
       { global }
       globals,tokens,verbose,constexp,
       systems,
       ppu,fmodule,
       { symtable }
       symconst,symbase,symdef,symtype,symsym,
       { pass 1 }
       node,
       ncon,
       { parser }
       scanner,
       pbase,pexpr,
       { link }
       gendef,export
       ;


    procedure read_exports;
      var
        hp        : texported_item;
        orgs,
        DefString,
        InternalProcName : string;
        pd         : tprocdef;
        pt         : tnode;
        srsym      : tsym;
        srsymtable : TSymtable;

        function IsGreater(hp1,hp2:texported_item):boolean;
        var
          i2 : boolean;
        begin
          i2:=(hp2.options and eo_index)<>0;
          if (hp1.options and eo_index)<>0 then
           begin
             if i2 then
               IsGreater:=hp1.index>hp2.index
             else
               IsGreater:=false;
           end
          else
            IsGreater:=i2;
        end;

      begin
         current_module.flags:=current_module.flags or uf_has_exports;
         DefString:='';
         InternalProcName:='';
         consume(_EXPORTS);
         repeat
           hp:=texported_item.create;
           if token=_ID then
             begin
                consume_sym_orgid(srsym,srsymtable,orgs);
                { orgpattern is still valid here }
                hp.sym:=srsym;
                InternalProcName:='';
                case srsym.typ of
                  staticvarsym :
                    InternalProcName:=tstaticvarsym(srsym).mangledname;
                  procsym :
                    begin
                      pd:=tprocdef(tprocsym(srsym).ProcdefList[0]);
                      if (Tprocsym(srsym).ProcdefList.Count>1) or
                         (po_kylixlocal in pd.procoptions) or
                         ((tf_need_export in target_info.flags) and
                          not(po_exports in pd.procoptions)) then
                        Message(parser_e_illegal_symbol_exported)
                      else
                        InternalProcName:=pd.mangledname;
                    end;
                  else
                    Message(parser_e_illegal_symbol_exported)
                end;
                if InternalProcName<>'' then
                 begin
                   { This is wrong if the first is not
                     an underline }
                   if InternalProcName[1]='_' then
                     delete(InternalProcName,1,1)
                   else if (target_info.system in [system_i386_win32,system_i386_wdosx,system_arm_wince,system_i386_wince]) and UseDeffileForExports then
                     begin
                       Message(parser_e_dlltool_unit_var_problem);
                       Message(parser_e_dlltool_unit_var_problem2);
                     end;
                   if length(InternalProcName)<2 then
                    Message(parser_e_procname_to_short_for_export);
                   DefString:=srsym.realname+'='+InternalProcName;
                 end;
                if try_to_consume(_INDEX) then
                 begin
                   pt:=comp_expr(true);
                   if pt.nodetype=ordconstn then
                    if (Tordconstnode(pt).value<int64(low(hp.index))) or
                       (Tordconstnode(pt).value>int64(high(hp.index))) then
                      begin
                        hp.index:=0;
                        message(parser_e_range_check_error)
                      end
                    else
                      hp.index:=Tordconstnode(pt).value.svalue
                   else
                    begin
                      hp.index:=0;
                      consume(_INTCONST);
                    end;
                   hp.options:=hp.options or eo_index;
                   pt.free;
                   if target_info.system in [system_i386_win32,system_i386_wdosx,system_arm_wince,system_i386_wince] then
                    DefString:=srsym.realname+'='+InternalProcName+' @ '+tostr(hp.index)
                   else
                    DefString:=srsym.realname+'='+InternalProcName; {Index ignored!}
                 end;
                if try_to_consume(_NAME) then
                 begin
                   pt:=comp_expr(true);
                   if pt.nodetype=stringconstn then
                    hp.name:=stringdup(strpas(tstringconstnode(pt).value_str))
                   else
                    begin
                      hp.name:=stringdup('');
                      consume(_CSTRING);
                    end;
                   hp.options:=hp.options or eo_name;
                   pt.free;
                   DefString:=hp.name^+'='+InternalProcName;
                 end;
                if try_to_consume(_RESIDENT) then
                 begin
                   hp.options:=hp.options or eo_resident;
                   DefString:=srsym.realname+'='+InternalProcName;{Resident ignored!}
                 end;
                if (DefString<>'') and UseDeffileForExports then
                 DefFile.AddExport(DefString);
                { Default to generate a name entry with the provided name }
                if not assigned(hp.name) then
                 begin
                   hp.name:=stringdup(orgs);
                   hp.options:=hp.options or eo_name;
                 end;
                if hp.sym.typ=procsym then
                  exportlib.exportprocedure(hp)
                else
                  exportlib.exportvar(hp);
             end
           else
             consume(_ID);
         until not try_to_consume(_COMMA);
         consume(_SEMICOLON);
        if not DefFile.empty then
         DefFile.writefile;
      end;

end.

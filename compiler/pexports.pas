{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    { reads an exports statement in a library }
    procedure read_exports;


implementation

    uses
       { common }
       cutils,
       { global }
       globals,tokens,verbose,
       systems,
       { symtable }
       symconst,symdef,symsym,symtable,
       { pass 1 }
       node,pass_1,
       ncon,
       { parser }
       scanner,
       pbase,pexpr,
       { link }
       gendef,export
       ;


    procedure read_exports;
      var
         hp        : pexported_item;
         DefString : string;
         ProcName  : string;
         InternalProcName : string;
         pt        : tnode;
      begin
         DefString:='';
         InternalProcName:='';
         consume(_EXPORTS);
         while true do
           begin
              hp:=new(pexported_item,init);
              if token=_ID then
                begin
                   getsym(pattern,true);
                   if srsym^.typ=unitsym then
                     begin
                        consume(_ID);
                        consume(_POINT);
                        getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                     end;
                   consume(_ID);
                   if assigned(srsym) then
                     begin
                        hp^.sym:=srsym;
                        if ((hp^.sym^.typ<>procsym) or
                            ((tf_need_export in target_info.flags) and
                             not(po_exports in pprocdef(pprocsym(srsym)^.definition)^.procoptions)
                            )
                           ) and
                           (srsym^.typ<>varsym) and (srsym^.typ<>typedconstsym) then
                         Message(parser_e_illegal_symbol_exported)
                        else
                         begin
                          ProcName:=hp^.sym^.name;
                          InternalProcName:=hp^.sym^.mangledname;
                          { This is wrong if the first is not
                            an underline }
                          if InternalProcName[1]='_' then
                            delete(InternalProcName,1,1)
                          else if (target_os.id=os_i386_win32) and UseDeffileForExport then
                            begin
                              Message(parser_e_dlltool_unit_var_problem);
                              Message(parser_e_dlltool_unit_var_problem2);
                            end;
                          if length(InternalProcName)<2 then
                           Message(parser_e_procname_to_short_for_export);
                          DefString:=ProcName+'='+InternalProcName;
                         end;
                        if (idtoken=_INDEX) then
                          begin
                             consume(_INDEX);
                             pt:=comp_expr(true);
                             do_firstpass(pt);
                             if pt.nodetype=ordconstn then
                               hp^.index:=tordconstnode(pt).value
                             else
                                begin
                                   hp^.index:=0;
                                   consume(_INTCONST);
                                end;
                             hp^.options:=hp^.options or eo_index;
                             pt.free;
                             if target_os.id=os_i386_win32 then
                               DefString:=ProcName+'='+InternalProcName+' @ '+tostr(hp^.index)
                             else
                               DefString:=ProcName+'='+InternalProcName; {Index ignored!}
                          end;
                        if (idtoken=_NAME) then
                          begin
                             consume(_NAME);
                             pt:=comp_expr(true);
                             do_firstpass(pt);
                             if pt.nodetype=stringconstn then
                               hp^.name:=stringdup(strpas(tstringconstnode(pt).value_str))
                             else
                                begin
                                   hp^.name:=stringdup('');
                                   consume(_CSTRING);
                                end;
                             hp^.options:=hp^.options or eo_name;
                             pt.free;
                             DefString:=hp^.name^+'='+InternalProcName;
                          end;
                        if (idtoken=_RESIDENT) then
                          begin
                             consume(_RESIDENT);
                             hp^.options:=hp^.options or eo_resident;
                             DefString:=ProcName+'='+InternalProcName;{Resident ignored!}
                          end;
                        if (DefString<>'') and UseDeffileForExport then
                         DefFile.AddExport(DefString);
                        if hp^.sym^.typ=procsym then
                          exportlib^.exportprocedure(hp)
                        else
                          exportlib^.exportvar(hp);
                     end;
                end
              else
                consume(_ID);
              if token=_COMMA then
                consume(_COMMA)
              else
                break;
           end;
         consume(_SEMICOLON);
        if not DefFile.empty then
         DefFile.writefile;
      end;

end.

{
  $Log$
  Revision 1.8  2000-11-29 00:30:36  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.7  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.6  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.5  2000/09/24 21:19:50  peter
    * delphi compile fixes

  Revision 1.4  2000/09/24 15:06:21  peter
    * use defines.inc

  Revision 1.3  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs

}

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

{$i fpcdefs.inc}

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
       symconst,symbase,symtype,symsym,
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
         DefString : string;
         InternalProcName : string;
         pt               : tnode;
         srsym            : tsym;
         srsymtable : tsymtable;
      begin
         DefString:='';
         InternalProcName:='';
         consume(_EXPORTS);
         while true do
           begin
              hp:=texported_item.create;
              if token=_ID then
                begin
                   orgs:=orgpattern;
                   consume_sym(srsym,srsymtable);
                   hp.sym:=srsym;
                   InternalProcName:='';
                   case srsym.typ of
                     varsym :
                       InternalProcName:=tvarsym(srsym).mangledname;
                     typedconstsym :
                       InternalProcName:=ttypedconstsym(srsym).mangledname;
                     procsym :
                       begin
                         if assigned(tprocsym(srsym).defs^.next) or
                            ((tf_need_export in target_info.flags) and
                             not(po_exports in tprocsym(srsym).defs^.def.procoptions)) then
                           Message(parser_e_illegal_symbol_exported)
                         else
                           InternalProcName:=tprocsym(srsym).defs^.def.mangledname;
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
                      else if (target_info.target in [target_i386_win32,target_i386_wdosx]) and UseDeffileForExport then
                        begin
                          Message(parser_e_dlltool_unit_var_problem);
                          Message(parser_e_dlltool_unit_var_problem2);
                        end;
                      if length(InternalProcName)<2 then
                       Message(parser_e_procname_to_short_for_export);
                      DefString:=srsym.realname+'='+InternalProcName;
                    end;
                   if (idtoken=_INDEX) then
                    begin
                      consume(_INDEX);
                      pt:=comp_expr(true);
                      if pt.nodetype=ordconstn then
                       hp.index:=tordconstnode(pt).value
                      else
                       begin
                         hp.index:=0;
                         consume(_INTCONST);
                       end;
                      hp.options:=hp.options or eo_index;
                      pt.free;
                      if target_info.target in [target_i386_win32,target_i386_wdosx] then
                       DefString:=srsym.realname+'='+InternalProcName+' @ '+tostr(hp.index)
                      else
                       DefString:=srsym.realname+'='+InternalProcName; {Index ignored!}
                    end;
                   if (idtoken=_NAME) then
                    begin
                      consume(_NAME);
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
                   if (idtoken=_RESIDENT) then
                    begin
                      consume(_RESIDENT);
                      hp.options:=hp.options or eo_resident;
                      DefString:=srsym.realname+'='+InternalProcName;{Resident ignored!}
                    end;
                   if (DefString<>'') and UseDeffileForExport then
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
  Revision 1.20  2002-05-16 19:46:43  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.18  2002/04/04 19:06:03  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.17  2002/04/04 18:41:07  carl
  + added wdosx support (patch from Pavel)

  Revision 1.16  2001/11/02 22:58:04  peter
    * procsym definition rewrite

  Revision 1.15  2001/04/18 22:01:57  peter
    * registration of targets and assemblers

  Revision 1.14  2001/04/13 01:22:12  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.13  2001/04/04 22:43:52  peter
    * remove unnecessary calls to firstpass

  Revision 1.12  2001/03/11 22:58:50  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.11  2001/01/03 13:12:50  jonas
    * fixed copy/past bugs

  Revision 1.10  2000/12/30 22:53:25  peter
    * export with the case provided in the exports section

  Revision 1.9  2000/12/25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.8  2000/11/29 00:30:36  florian
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

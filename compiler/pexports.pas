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

  interface

    { reads an exports statement in a library }
    procedure read_exports;

  implementation

    uses
      globtype,systems,tokens,
      strings,cobjects,globals,verbose,
      scanner,symconst,symtable,pbase,
      export,GenDef,tree,pass_1,pexpr;

    procedure read_exports;

      var
         hp : pexported_item;
         DefString:string;
         ProcName:string;
         InternalProcName:string;
         pt : ptree;

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
                             if pt^.treetype=ordconstn then
                               hp^.index:=pt^.value
                             else
                                begin
                                   hp^.index:=0;
                                   consume(_INTCONST);
                                end;
                             hp^.options:=hp^.options or eo_index;
                             disposetree(pt);
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
                             if pt^.treetype=stringconstn then
                               hp^.name:=stringdup(strpas(pt^.value_str))
                             else
                                begin
                                   hp^.name:=stringdup('');
                                   consume(_CSTRING);
                                end;
                             hp^.options:=hp^.options or eo_name;
                             disposetree(pt);
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
  Revision 1.22  2000-04-24 12:47:27  peter
    * fixed check for exporting var or proc

  Revision 1.21  2000/02/24 18:41:39  peter
    * removed warnings/notes

  Revision 1.20  2000/02/23 23:06:39  florian
    + the expr for names and indizies of exports sections support now
      every type of expressions which evalute to a constant

  Revision 1.19  2000/02/09 13:22:56  peter
    * log truncated

  Revision 1.18  2000/01/07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.17  1999/12/20 23:23:30  pierre
   + $description $version

  Revision 1.16  1999/12/08 10:40:01  pierre
    + allow use of unit var in exports of DLL for win32
      by using direct export writing by default instead of use of DEFFILE
      that does not allow assembler labels that do not
      start with an underscore.
      Use -WD to force use of Deffile for Win32 DLL

  Revision 1.15  1999/11/22 22:20:43  pierre
    * Def file syntax for win32 with index corrected
    * direct output of .edata leads to same indexes
      (index 5 leads to next export being 6 unless otherwise
      specified like for enums)

  Revision 1.14  1999/11/20 01:19:10  pierre
    * DLL index used for win32 target with DEF file
    + DLL initialization/finalization support

  Revision 1.13  1999/10/26 12:30:44  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.12  1999/08/10 12:51:19  pierre
    * bind_win32_dll removed (Relocsection used instead)
    * now relocsection is true by default ! (needs dlltool
      for DLL generation)

  Revision 1.11  1999/08/04 13:02:54  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.10  1999/08/03 22:02:58  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
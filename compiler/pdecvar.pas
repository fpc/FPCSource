{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Parses variable declarations. Used for var statement and record
    definitions

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
unit pdecvar;

{$i fpcdefs.inc}

interface

    procedure read_var_decs(is_record,is_object,is_threadvar:boolean);


implementation

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,tokens,verbose,
       systems,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,symtable,defutil,
       fmodule,paramgr,
       { pass 1 }
       node,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,ptype,ptconst,pdecsub,
       { link }
       import
{$ifdef Delphi}
       ,dmisc
       ,sysutils
{$endif}
       ;

    const
       variantrecordlevel : longint = 0;

    procedure read_var_decs(is_record,is_object,is_threadvar:boolean);
    { reads the filed of a record into a        }
    { symtablestack, if record=false        }
    { variants are forbidden, so this procedure }
    { can be used to read object fields  }
    { if absolute is true, ABSOLUTE and file    }
    { types are allowed                  }
    { => the procedure is also used to read     }
    { a sequence of variable declaration        }

      procedure insert_syms(sc : tsinglelist;tt : ttype;is_threadvar : boolean; addsymopts : tsymoptions);
      { inserts the symbols of sc in st with def as definition or sym as ttypesym, sc is disposed }
        var
          vs,vs2 : tvarsym;
        begin
           vs:=tvarsym(sc.first);
           while assigned(vs) do
             begin
                vs.vartype:=tt;
                { insert any additional hint directives }
                vs.symoptions := vs.symoptions + addsymopts;
                if (sp_static in current_object_option) then
                  include(vs.symoptions,sp_static);
                if is_threadvar then
                  include(vs.varoptions,vo_is_thread_var);
                { static data fields are inserted in the globalsymtable }
                if (symtablestack.symtabletype=objectsymtable) and
                   (sp_static in current_object_option) then
                  begin
                     vs2:=tvarsym.create('$'+lower(symtablestack.name^)+'_'+vs.name,vs_value,tt);
                     symtablestack.defowner.owner.insert(vs2);
                     symtablestack.defowner.owner.insertvardata(vs2);
                  end
                else
                  begin
                    { external data is not possible here }
                    symtablestack.insertvardata(vs);
                  end;
                vs:=tvarsym(vs.listnext);
             end;
        end;

      var
         sc : tsinglelist;
         old_block_type : tblock_type;
         oldsymtablestack : tsymtable;
         symdone : boolean;
         { to handle absolute }
         abssym : tabsolutesym;
         { c var }
         newtype : ttypesym;
         is_dll,
         is_gpc_name,is_cdecl,
         extern_var,export_var : boolean;
         old_current_object_option : tsymoptions;
         hs,sorg,C_name,dll_name : string;
         tt,casetype : ttype;
         { Delphi initialized vars }
         tconstsym : ttypedconstsym;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize, startvarrecsize : longint;
         usedalign,
         minalignment,maxalignment,startvarrecalign : byte;
         pt : tnode;
         vs    : tvarsym;
         srsym : tsym;
         srsymtable : tsymtable;
         unionsymtable : tsymtable;
         offset : longint;
         uniondef : trecorddef;
         unionsym : tvarsym;
         uniontype : ttype;
         dummysymoptions : tsymoptions;
      begin
         old_current_object_option:=current_object_option;
         { all variables are public if not in a object declaration }
         if not is_object then
          current_object_option:=[sp_public];
         old_block_type:=block_type;
         block_type:=bt_type;
         is_gpc_name:=false;
         { Force an expected ID error message }
         if not (token in [_ID,_CASE,_END]) then
          consume(_ID);
         { read vars }
         sc:=tsinglelist.create;
         while (token=_ID) and
               not(is_object and (idtoken in [_PUBLIC,_PRIVATE,_PUBLISHED,_PROTECTED])) do
           begin
             sorg:=orgpattern;
             sc.reset;
             repeat
               vs:=tvarsym.create(orgpattern,vs_value,generrortype);
               symtablestack.insert(vs);
               if assigned(vs.owner) then
                sc.insert(vs)
               else
                vs.free;
               consume(_ID);
             until not try_to_consume(_COMMA);
             consume(_COLON);
             if (m_gpc in aktmodeswitches) and
                not(is_record or is_object or is_threadvar) and
                (token=_ID) and (orgpattern='__asmname__') then
               begin
                 consume(_ID);
                 C_name:=get_stringconst;
                 Is_gpc_name:=true;
               end;
             { this is needed for Delphi mode at least
               but should be OK for all modes !! (PM) }
             ignore_equal:=true;
             if is_record then
              begin
                { for records, don't search the recordsymtable for
                  the symbols of the types }
                oldsymtablestack:=symtablestack;
                symtablestack:=symtablestack.next;
                read_type(tt,'');
                symtablestack:=oldsymtablestack;
              end
             else
              read_type(tt,'');
             { types that use init/final are not allowed in variant parts, but
               classes are allowed }
             if (variantrecordlevel>0) and
                (tt.def.needs_inittable and not is_class(tt.def)) then
               Message(parser_e_cant_use_inittable_here);
             ignore_equal:=false;
             symdone:=false;
             if is_gpc_name then
               begin
                  vs:=tvarsym(sc.first);
                  if assigned(vs.listnext) then
                    Message(parser_e_absolute_only_one_var);
                  vs.vartype:=tt;
                  include(vs.varoptions,vo_is_C_var);
                  vs.set_mangledname(target_info.Cprefix+sorg);
                  include(vs.varoptions,vo_is_external);
                  symdone:=true;
               end;
             { check for absolute }
             if not symdone and
                (idtoken=_ABSOLUTE) and not(is_record or is_object or is_threadvar) then
              begin
                consume(_ABSOLUTE);
                abssym:=nil;
                { only allowed for one var }
                vs:=tvarsym(sc.first);
                if assigned(vs.listnext) then
                  Message(parser_e_absolute_only_one_var);
                { parse the rest }
                pt:=expr;
                { transform a procvar calln to loadn }
                if pt.nodetype=calln then
                  load_procvar_from_calln(pt);
                { check allowed absolute types }
                if (pt.nodetype=stringconstn) or
                   (is_constcharnode(pt)) then
                 begin
                   abssym:=tabsolutesym.create(vs.realname,tt);
                   abssym.fileinfo:=vs.fileinfo;
                   if pt.nodetype=stringconstn then
                     hs:=strpas(tstringconstnode(pt).value_str)
                   else
                     hs:=chr(tordconstnode(pt).value);
                   consume(token);
                   abssym.abstyp:=toasm;
                   abssym.asmname:=stringdup(hs);
                   { replace the varsym }
                   symtablestack.replace(vs,abssym);
                   vs.free;
                 end
                { variable }
                else if (pt.nodetype=loadn) then
                 begin
                   { we should check the result type of srsym }
                   if not (tloadnode(pt).symtableentry.typ in [varsym,typedconstsym]) then
                     Message(parser_e_absolute_only_to_var_or_const);
                   abssym:=tabsolutesym.create(vs.realname,tt);
                   abssym.fileinfo:=vs.fileinfo;
                   abssym.abstyp:=tovar;
                   abssym.ref:=tstoredsym(tloadnode(pt).symtableentry);
                   symtablestack.replace(vs,abssym);
                   vs.free;
                   { the variable cannot be put into a register
                     if the size definition is not the same.
                   }
                   if tloadnode(pt).symtableentry.typ = varsym then
                     begin
                       if abssym.vartype.def <>
                          tvarsym(tloadnode(pt).symtableentry).vartype.def then
                          tvarsym(tloadnode(pt).symtableentry).varoptions:=
                          tvarsym(tloadnode(pt).symtableentry).varoptions-[vo_regable,vo_fpuregable]
                     end;
                 end
                { address }
                else if is_constintnode(pt) and
                        ((target_info.system=system_i386_go32v2) or
                         (m_objfpc in aktmodeswitches) or
                         (m_delphi in aktmodeswitches)) then
                 begin
                   abssym:=tabsolutesym.create(vs.realname,tt);
                   abssym.fileinfo:=vs.fileinfo;
                   abssym.abstyp:=toaddr;
                   abssym.absseg:=false;
                   abssym.address:=tordconstnode(pt).value;
                   if (target_info.system=system_i386_go32v2) and
                      try_to_consume(_COLON) then
                    begin
                      pt.free;
                      pt:=expr;
                      if is_constintnode(pt) then
                        begin
                          abssym.address:=abssym.address shl 4+tordconstnode(pt).value;
                          abssym.absseg:=true;
                        end
                      else
                         Message(type_e_ordinal_expr_expected);
                    end;
                   symtablestack.replace(vs,abssym);
                   vs.free;
                 end
                else
                 Message(parser_e_absolute_only_to_var_or_const);
                if assigned(abssym) then
                 begin
                   { try to consume the hint directives with absolute symbols }
                   dummysymoptions:=[];
                   try_consume_hintdirective(dummysymoptions);
                   abssym.symoptions := abssym.symoptions + dummysymoptions;
                 end;
                pt.free;
                symdone:=true;
              end;
             { Handling of Delphi typed const = initialized vars ! }
             { When should this be rejected ?
               - in parasymtable
               - in record or object
               - ... (PM) }
             if (token=_EQUAL) and
                not(m_tp7 in aktmodeswitches) and
                not(symtablestack.symtabletype in [parasymtable]) and
                not is_record and
                not is_object then
               begin
                  vs:=tvarsym(sc.first);
                  if assigned(vs.listnext) then
                    Message(parser_e_initialized_only_one_var);
                  if is_threadvar then
                    Message(parser_e_initialized_not_for_threadvar);
                  if symtablestack.symtabletype=localsymtable then
                   begin
                     consume(_EQUAL);
                     tconstsym:=ttypedconstsym.createtype('default'+vs.realname,tt,false);
                     vs.defaultconstsym:=tconstsym;
                     symtablestack.insert(tconstsym);
                     symtablestack.insertconstdata(tconstsym);
                     readtypedconst(tt,tconstsym,false);
                   end
                  else
                   begin
                     tconstsym:=ttypedconstsym.createtype(vs.realname,tt,true);
                     tconstsym.fileinfo:=vs.fileinfo;
                     symtablestack.replace(vs,tconstsym);
                     vs.free;
                     symtablestack.insertconstdata(tconstsym);
                     consume(_EQUAL);
                     readtypedconst(tt,tconstsym,true);
                     symdone:=true;
                   end;
               end;
             { if the symbol is not completely handled, then try to parse the
               hint directives }
             if not symdone then
               begin
                 dummysymoptions:=[];
                 try_consume_hintdirective(dummysymoptions);
               end;
             { for a record there doesn't need to be a ; before the END or ) }
             if not((is_record or is_object) and (token in [_END,_RKLAMMER])) then
               consume(_SEMICOLON);
             { procvar handling }
             if (tt.def.deftype=procvardef) and (tt.def.typesym=nil) then
               begin
                  newtype:=ttypesym.create('unnamed',tt);
                  parse_var_proc_directives(tsym(newtype));
                  paramanager.create_param_loc_info(tabstractprocdef(tt.def));
                  newtype.restype.def:=nil;
                  tt.def.typesym:=nil;
                  newtype.free;
               end;
             { Check for variable directives }
             if not symdone and (token=_ID) then
              begin
                { Check for C Variable declarations }
                if (m_cvar_support in aktmodeswitches) and
                   not(is_record or is_object or is_threadvar) and
                   (idtoken in [_EXPORT,_EXTERNAL,_PUBLIC,_CVAR]) then
                 begin
                   { only allowed for one var }
                   vs:=tvarsym(sc.first);
                   if assigned(vs.listnext) then
                     Message(parser_e_absolute_only_one_var);
                   { set type of the var }
                   vs.vartype:=tt;
                   vs.symoptions := vs.symoptions + dummysymoptions;
                   { defaults }
                   is_dll:=false;
                   is_cdecl:=false;
                   extern_var:=false;
                   export_var:=false;
                   C_name:=sorg;
                   { cdecl }
                   if idtoken=_CVAR then
                    begin
                      consume(_CVAR);
                      consume(_SEMICOLON);
                      is_cdecl:=true;
                      C_name:=target_info.Cprefix+sorg;
                    end;
                   { external }
                   if idtoken=_EXTERNAL then
                    begin
                      consume(_EXTERNAL);
                      extern_var:=true;
                    end;
                   { export }
                   if idtoken in [_EXPORT,_PUBLIC] then
                    begin
                      consume(_ID);
                      if extern_var or
                         (symtablestack.symtabletype in [parasymtable,localsymtable]) then
                       Message(parser_e_not_external_and_export)
                      else
                       export_var:=true;
                    end;
                   { external and export need a name after when no cdecl is used }
                   if not is_cdecl then
                    begin
                      { dll name ? }
                      if (extern_var) and (idtoken<>_NAME) then
                       begin
                         is_dll:=true;
                         dll_name:=get_stringconst;
                       end;
                      consume(_NAME);
                      C_name:=get_stringconst;
                    end;
                   { consume the ; when export or external is used }
                   if extern_var or export_var then
                    consume(_SEMICOLON);
                   { set some vars options }
                   if is_dll then
                    include(vs.varoptions,vo_is_dll_var)
                   else
                    include(vs.varoptions,vo_is_C_var);
                   vs.set_mangledname(C_Name);
                   if export_var then
                    begin
                      inc(vs.refs);
                      include(vs.varoptions,vo_is_exported);
                    end;
                   if extern_var then
                    include(vs.varoptions,vo_is_external);
                   { insert in the datasegment when it is not external }
                   if not extern_var then
                     symtablestack.insertvardata(vs);
                   { now we can insert it in the import lib if its a dll, or
                     add it to the externals }
                   if extern_var then
                    begin
                      if is_dll then
                       begin
                         if not(current_module.uses_imports) then
                          begin
                            current_module.uses_imports:=true;
                            importlib.preparelib(current_module.modulename^);
                          end;
                         importlib.importvariable(vs,C_name,dll_name);
                       end
                      else
                       if target_info.DllScanSupported then
                        current_module.Externals.insert(tExternalsItem.create(vs.mangledname));
                    end;
                   symdone:=true;
                 end
                else
                 if (is_object) and (cs_static_keyword in aktmoduleswitches) and (idtoken=_STATIC) then
                  begin
                    include(current_object_option,sp_static);
                    insert_syms(sc,tt,false,dummysymoptions);
                    exclude(current_object_option,sp_static);
                    consume(_STATIC);
                    consume(_SEMICOLON);
                    symdone:=true;
                  end;
              end;
             { insert it in the symtable, if not done yet }
             if not symdone then
               begin
                  { save object option, because we can turn of the sp_published }
                  if (sp_published in current_object_option) and
                    not(is_class(tt.def)) then
                   begin
                     Message(parser_e_cant_publish_that);
                     exclude(current_object_option,sp_published);
                   end
                  else
                   if (sp_published in current_object_option) and
                      not(oo_can_have_published in tobjectdef(tt.def).objectoptions) then
                    begin
                      Message(parser_e_only_publishable_classes_can__be_published);
                      exclude(current_object_option,sp_published);
                    end;
                  insert_syms(sc,tt,is_threadvar,dummysymoptions);
                  current_object_option:=old_current_object_option;
               end;
           end;
         { Check for Case }
         if is_record and (token=_CASE) then
           begin
              maxsize:=0;
              maxalignment:=0;
              consume(_CASE);
              sorg:=orgpattern;
              hs:=pattern;
              searchsym(hs,srsym,srsymtable);
              { may be only a type: }
              if assigned(srsym) and (srsym.typ in [typesym,unitsym]) then
               begin
                 { for records, don't search the recordsymtable for
                   the symbols of the types }
                 oldsymtablestack:=symtablestack;
                 symtablestack:=symtablestack.next;
                 read_type(casetype,'');
                 symtablestack:=oldsymtablestack;
               end
              else
                begin
                  consume(_ID);
                  consume(_COLON);
                  { for records, don't search the recordsymtable for
                    the symbols of the types }
                  oldsymtablestack:=symtablestack;
                  symtablestack:=symtablestack.next;
                  read_type(casetype,'');
                  symtablestack:=oldsymtablestack;
                  vs:=tvarsym.create(sorg,vs_value,casetype);
                  symtablestack.insert(vs);
                  symtablestack.insertvardata(vs);
                end;
              if not(is_ordinal(casetype.def)) or is_64bitint(casetype.def)  then
               Message(type_e_ordinal_expr_expected);
              consume(_OF);
              UnionSymtable:=trecordsymtable.create;
              Unionsymtable.next:=symtablestack;
              registerdef:=false;
              UnionDef:=trecorddef.create(unionsymtable);
              uniondef.isunion:=true;
              if assigned(symtablestack.defowner) then
                Uniondef.owner:=symtablestack.defowner.owner;
              registerdef:=true;
              symtablestack:=UnionSymtable;
              startvarrecsize:=symtablestack.datasize;
              startvarrecalign:=symtablestack.dataalignment;
              repeat
                repeat
                  pt:=comp_expr(true);
                  if not(pt.nodetype=ordconstn) then
                    Message(cg_e_illegal_expression);
                  pt.free;
                  if token=_COMMA then
                   consume(_COMMA)
                  else
                   break;
                until false;
                consume(_COLON);
                { read the vars }
                consume(_LKLAMMER);
                inc(variantrecordlevel);
                if token<>_RKLAMMER then
                  read_var_decs(true,false,false);
                dec(variantrecordlevel);
                consume(_RKLAMMER);
                { calculates maximal variant size }
                maxsize:=max(maxsize,symtablestack.datasize);
                maxalignment:=max(maxalignment,symtablestack.dataalignment);
                { the items of the next variant are overlayed }
                symtablestack.datasize:=startvarrecsize;
                symtablestack.dataalignment:=startvarrecalign;
                if (token<>_END) and (token<>_RKLAMMER) then
                  consume(_SEMICOLON)
                else
                  break;
              until (token=_END) or (token=_RKLAMMER);
              { at last set the record size to that of the biggest variant }
              symtablestack.datasize:=maxsize;
              symtablestack.dataalignment:=maxalignment;
              uniontype.def:=uniondef;
              uniontype.sym:=nil;
              UnionSym:=tvarsym.create('$case',vs_value,uniontype);
              symtablestack:=symtablestack.next;
              { we do NOT call symtablestack.insert
               on purpose PM }
              if aktalignment.recordalignmax=-1 then
               begin
{$ifdef i386}
                 if maxalignment>2 then
                  minalignment:=4
                 else if maxalignment>1 then
                  minalignment:=2
                 else
                  minalignment:=1;
{$else}
{$ifdef m68k}
                 minalignment:=2;
{$endif}
                 minalignment:=1;
{$endif}
               end
              else
               minalignment:=maxalignment;
              usedalign:=used_align(maxalignment,minalignment,maxalignment);
              offset:=align(symtablestack.datasize,usedalign);
              symtablestack.datasize:=offset+unionsymtable.datasize;
              if maxalignment>symtablestack.dataalignment then
                symtablestack.dataalignment:=maxalignment;
              trecordsymtable(Unionsymtable).Insert_in(symtablestack,offset);
              Unionsym.owner:=nil;
              unionsym.free;
              uniondef.owner:=nil;
              uniondef.free;
           end;
         block_type:=old_block_type;
         current_object_option:=old_current_object_option;
         { free the list }
         sc.free;
      end;

end.
{
  $Log$
  Revision 1.47  2003-05-09 17:47:03  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.46  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.45  2003/03/17 18:56:02  peter
    * fix crash with duplicate id

  Revision 1.44  2003/01/02 11:14:02  michael
  + Patch from peter to support initial values for local variables

  Revision 1.43  2002/12/27 15:22:20  peter
    * don't allow initialized threadvars

  Revision 1.42  2002/12/07 14:04:59  carl
   * convert some vars from longint -> byte

  Revision 1.41  2002/11/29 22:31:19  carl
    + unimplemented hint directive added
    * hint directive parsing implemented
    * warning on these directives

  Revision 1.40  2002/11/25 17:43:21  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.39  2002/11/15 16:29:31  peter
    * made tasmsymbol.refs private (merged)

  Revision 1.38  2002/11/15 01:58:53  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.37  2002/10/05 15:18:43  carl
    * fix heap leaks

  Revision 1.36  2002/10/05 12:43:26  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.35  2002/10/04 20:53:05  carl
    * bugfix of crash

  Revision 1.34  2002/10/03 21:22:01  carl
    * don't make the vars regable if they are absolute and their definitions
      are not the same.

  Revision 1.33  2002/09/16 18:08:45  peter
    * fix setting of sp_static

  Revision 1.32  2002/09/09 17:34:15  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.31  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.30  2002/07/29 21:23:44  florian
    * more fixes for the ppc
    + wrappers for the tcnvnode.first_* stuff introduced

  Revision 1.29  2002/07/26 21:15:40  florian
    * rewrote the system handling

  Revision 1.28  2002/07/20 11:57:55  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.27  2002/06/10 13:41:26  jonas
    * fixed bug 1985

  Revision 1.26  2002/05/18 13:34:12  peter
    * readded missing revisions

  Revision 1.25  2002/05/16 19:46:43  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.23  2002/04/21 18:57:24  peter
    * fixed memleaks when file can't be opened

}

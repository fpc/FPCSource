{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl, Daniel Mantione

    Does the parsing of the procedures/functions

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
unit pdecsub;

{$i defines.inc}

interface

    uses
      tokens,symconst,symtype,symdef,symsym;

    const
      pd_global    = $1;    { directive must be global }
      pd_body      = $2;    { directive needs a body }
      pd_implemen  = $4;    { directive can be used implementation section }
      pd_interface = $8;    { directive can be used interface section }
      pd_object    = $10;   { directive can be used object declaration }
      pd_procvar   = $20;   { directive can be used procvar declaration }
      pd_notobject = $40;   { directive can not be used object declaration }
      pd_notobjintf= $80;   { directive can not be used interface declaration }

    function  is_proc_directive(tok:ttoken):boolean;

    procedure parameter_dec(aktprocdef:tabstractprocdef);

    procedure parse_proc_directives(var pdflags:word);

    procedure handle_calling_convention(sym:tprocsym;def:tabstractprocdef);

    procedure parse_proc_head(options:tproctypeoption);
    procedure parse_proc_dec;
    procedure parse_var_proc_directives(var sym : tsym);
    procedure parse_object_proc_directives(var sym : tprocsym);

    function proc_add_definition(aprocsym:tprocsym;var aprocdef : tprocdef) : boolean;


implementation

    uses
{$ifdef delphi}
       sysutils,
{$else delphi}
       strings,
{$endif delphi}
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,verbose,
       systems,
       { aasm }
       aasm,
       { symtable }
       symbase,symtable,types,
       { pass 1 }
       node,htypechk,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       fmodule,scanner,
       pbase,pexpr,ptype,pdecl,
       { linking }
       import,gendef,
       { codegen }
       cgbase
       ;


    procedure resetvaluepara(p:tnamedindexitem);
      begin
        if tsym(p).typ=varsym then
         with tvarsym(p) do
          if copy(name,1,3)='val' then
           aktprocdef.parast.symsearch.rename(name,copy(name,4,length(name)));
      end;



    procedure parameter_dec(aktprocdef:tabstractprocdef);
      {
        handle_procvar needs the same changes
      }
      var
        is_procvar : boolean;
        sc      : tidstringlist;
        s       : string;
        hpos,
        storetokenpos : tfileposinfo;
        htype,
        tt      : ttype;
        hvs,
        vs      : tvarsym;
        srsym   : tsym;
        hs1,hs2 : string;
        varspez : Tvarspez;
        inserthigh : boolean;
        tdefaultvalue : tconstsym;
        defaultrequired : boolean;
        old_object_option : tsymoptions;
      begin
        { reset }
        defaultrequired:=false;
        { parsing a proc or procvar ? }
        is_procvar:=(aktprocdef.deftype=procvardef);
        if not is_procvar then
          hs2:=tprocdef(aktprocdef).mangledname;
        consume(_LKLAMMER);
        { Delphi/Kylix supports nonsense like }
        { procedure p();                      }
        if try_to_consume(_RKLAMMER) and
          not(m_tp7 in aktmodeswitches) then
          exit;
        { the variables are always public }
        old_object_option:=current_object_option;
        current_object_option:=[sp_public];
        inc(testcurobject);
        repeat
          if try_to_consume(_VAR) then
            varspez:=vs_var
          else
            if try_to_consume(_CONST) then
              varspez:=vs_const
          else
            if (idtoken=_OUT) and (m_out in aktmodeswitches) then
              begin
                 consume(_OUT);
                 varspez:=vs_out
              end
          else
              varspez:=vs_value;
          inserthigh:=false;
          tdefaultvalue:=nil;
          tt.reset;
          { self is only allowed in procvars and class methods }
          if (idtoken=_SELF) and
             (is_procvar or
              (assigned(procinfo^._class) and is_class(procinfo^._class))) then
            begin
              if varspez <> vs_value then
                 CGMessage(parser_e_self_call_by_value);
              if not is_procvar then
               begin
{$ifndef UseNiceNames}
                 hs2:=hs2+'$'+'self';
{$else UseNiceNames}
                 hs2:=hs2+tostr(length('self'))+'self';
{$endif UseNiceNames}
                 htype.setdef(procinfo^._class);
                 vs:=tvarsym.create('@',htype);
                 vs.varspez:=vs_var;
               { insert the sym in the parasymtable }
                 tprocdef(aktprocdef).parast.insert(vs);
                 inc(procinfo^.selfpointer_offset,vs.address);
               end
              else
               vs:=nil;
              { must also be included for procvars to allow the proc2procvar }
              { type conversions (po_containsself is in po_comp) (JM)        }
              include(aktprocdef.procoptions,po_containsself);
              consume(idtoken);
              consume(_COLON);
              single_type(tt,hs1,false);
              { this must be call-by-value, but we generate already an }
              { an error above if that's not the case (JM)             }
              aktprocdef.concatpara(tt,vs,varspez,nil);
              { check the types for procedures only }
              if not is_procvar then
               CheckTypes(tt.def,procinfo^._class);
            end
          else
            begin
             { read identifiers }
               sc:=consume_idlist;
{$ifdef fixLeaksOnError}
               strContStack.push(sc);
{$endif fixLeaksOnError}
             { read type declaration, force reading for value and const paras }
               if (token=_COLON) or (varspez=vs_value) then
                begin
                  consume(_COLON);
                { check for an open array }
                  if token=_ARRAY then
                   begin
                     consume(_ARRAY);
                     consume(_OF);
                   { define range and type of range }
                     tt.setdef(tarraydef.create(0,-1,s32bittype));
                   { array of const ? }
                     if (token=_CONST) and (m_objpas in aktmodeswitches) then
                      begin
                        consume(_CONST);
                        srsym:=searchsymonlyin(systemunit,'TVARREC');
                        if not assigned(srsym) then
                         InternalError(1234124);
                        tarraydef(tt.def).elementtype:=ttypesym(srsym).restype;
                        tarraydef(tt.def).IsArrayOfConst:=true;
                        hs1:='array_of_const';
                      end
                     else
                      begin
                        { define field type }
                        single_type(tarraydef(tt.def).elementtype,hs1,false);
                        hs1:='array_of_'+hs1;
                      end;
                     inserthigh:=true;
                   end
                  else
                   begin
                     { open string ? }
                     if (varspez=vs_var) and
                             (
                               (
                                 ((token=_STRING) or (idtoken=_SHORTSTRING)) and
                                 (cs_openstring in aktmoduleswitches) and
                                 not(cs_ansistrings in aktlocalswitches)
                               ) or
                             (idtoken=_OPENSTRING)) then
                      begin
                        consume(token);
                        tt:=openshortstringtype;
                        hs1:='openstring';
                        inserthigh:=true;
                      end
                     else
                      begin
                        { everything else }
                        single_type(tt,hs1,false);
                      end;
                     { default parameter }
                     if (m_default_para in aktmodeswitches) then
                      begin
                        if try_to_consume(_EQUAL) then
                         begin
                           s:=sc.get(hpos);
                           if not sc.empty then
                            Comment(V_Error,'default value only allowed for one parameter');
                           sc.add(s,hpos);
                           { prefix 'def' to the parameter name }
                           tdefaultvalue:=ReadConstant('$def'+Upper(s),hpos);
                           if assigned(tdefaultvalue) then
                            tprocdef(aktprocdef).parast.insert(tdefaultvalue);
                           defaultrequired:=true;
                         end
                        else
                         begin
                           if defaultrequired then
                            Comment(V_Error,'default parameter required');
                         end;
                      end;
                   end;
                end
               else
                begin
{$ifndef UseNiceNames}
                  hs1:='$$$';
{$else UseNiceNames}
                  hs1:='var';
{$endif UseNiceNames}
                  tt:=cformaltype;
                end;
               storetokenpos:=akttokenpos;
               while not sc.empty do
                begin
                  s:=sc.get(akttokenpos);
                  { For proc vars we only need the definitions }
                  if not is_procvar then
                   begin
{$ifndef UseNiceNames}
                     hs2:=hs2+'$'+hs1;
{$else UseNiceNames}
                     hs2:=hs2+tostr(length(hs1))+hs1;
{$endif UseNiceNames}
                     vs:=tvarsym.create(s,tt);
                     vs.varspez:=varspez;
                   { we have to add this to avoid var param to be in registers !!!}
                   { I don't understand the comment above,                          }
                   { but I suppose the comment is wrong and                         }
                   { it means that the address of var parameters can be placed      }
                   { in a register (FK)                                             }
                     if (varspez in [vs_var,vs_const,vs_out]) and push_addr_param(tt.def) then
                       include(vs.varoptions,vo_regable);

                   { insert the sym in the parasymtable }
                     tprocdef(aktprocdef).parast.insert(vs);

                   { do we need a local copy? Then rename the varsym, do this after the
                     insert so the dup id checking is done correctly }
                     if (varspez=vs_value) and
                        push_addr_param(tt.def) and
                        not(is_open_array(tt.def) or is_array_of_const(tt.def)) then
                       tprocdef(aktprocdef).parast.rename(vs.name,'val'+vs.name);

                   { also need to push a high value? }
                     if inserthigh then
                      begin
                        hvs:=tvarsym.create('$high'+Upper(s),s32bittype);
                        hvs.varspez:=vs_const;
                        tprocdef(aktprocdef).parast.insert(hvs);
                      end;

                   end
                  else
                   vs:=nil;

                  aktprocdef.concatpara(tt,vs,varspez,tdefaultvalue);
                end;
{$ifdef fixLeaksOnError}
               if PStringContainer(strContStack.pop) <> sc then
                  writeln('problem with strContStack in pdecl (1)');
{$endif fixLeaksOnError}
               sc.free;
               akttokenpos:=storetokenpos;
            end;
          { set the new mangled name }
        until not try_to_consume(_SEMICOLON);
        if not is_procvar then
          tprocdef(aktprocdef).setmangledname(hs2);
        dec(testcurobject);
        current_object_option:=old_object_option;
        consume(_RKLAMMER);
      end;


    procedure parse_proc_head(options:tproctypeoption);
      var
        orgsp,sp:stringid;
        paramoffset:longint;
        sym:tsym;
        hs:string;
        doinsert : boolean;
        st : tsymtable;
        srsymtable : tsymtable;
        pdl     : pprocdeflist;
        overloaded_level:word;
        storepos,procstartfilepos : tfileposinfo;
        i: longint;
        procdefs : pprocdeflist;
      begin
        { Save the position where this procedure really starts }
        procstartfilepos:=akttokenpos;

        aktprocdef:=nil;

        if (options=potype_operator) then
          begin
            sp:=overloaded_names[optoken];
            orgsp:=sp;
          end
        else
          begin
            sp:=pattern;
            orgsp:=orgpattern;
            consume(_ID);
          end;

          { examine interface map: function/procedure iname.functionname=locfuncname }
          if parse_only and
             assigned(procinfo^._class) and
             assigned(procinfo^._class.implementedinterfaces) and
             (procinfo^._class.implementedinterfaces.count>0) and
             try_to_consume(_POINT) then
            begin
               storepos:=akttokenpos;
               akttokenpos:=procstartfilepos;
               { get interface syms}
               searchsym(sp,sym,srsymtable);
               if not assigned(sym) then
                begin
                  identifier_not_found(orgsp);
                  sym:=generrorsym;
                end;
               akttokenpos:=storepos;
               { load proc name }
               if sym.typ=typesym then
                 i:=procinfo^._class.implementedinterfaces.searchintf(ttypesym(sym).restype.def);
               { qualifier is interface name? }
               if (sym.typ<>typesym) or (ttypesym(sym).restype.def.deftype<>objectdef) or
                  (i=-1) then
                 begin
                    Message(parser_e_interface_id_expected);
                    aktprocsym:=nil;
                 end
               else
                 begin
                    aktprocsym:=tprocsym(procinfo^._class.implementedinterfaces.interfaces(i).symtable.search(sp));
                    { the method can be declared after the mapping FK
                      if not(assigned(aktprocsym)) then
                        Message(parser_e_methode_id_expected);
                    }
                 end;
               consume(_ID);
               consume(_EQUAL);
               if (token=_ID) { and assigned(aktprocsym) } then
                 procinfo^._class.implementedinterfaces.addmappings(i,sp,pattern);
               consume(_ID);
               exit;
          end;

        { method  ? }
        if not(parse_only) and
           (lexlevel=normal_function_level) and
           try_to_consume(_POINT) then
         begin
           { search for object name }
           storepos:=akttokenpos;
           akttokenpos:=procstartfilepos;
           searchsym(sp,sym,srsymtable);
           if not assigned(sym) then
            begin
              identifier_not_found(orgsp);
              sym:=generrorsym;
            end;
           akttokenpos:=storepos;
           { consume proc name }
           sp:=pattern;
           orgsp:=orgpattern;
           procstartfilepos:=akttokenpos;
           consume(_ID);
           { qualifier is class name ? }
           if (sym.typ<>typesym) or
              (ttypesym(sym).restype.def.deftype<>objectdef) then
             begin
                Message(parser_e_class_id_expected);
                aktprocsym:=nil;
                aktprocdef:=nil;
             end
           else
             begin
                { used to allow private syms to be seen }
                aktobjectdef:=tobjectdef(ttypesym(sym).restype.def);
                procinfo^._class:=tobjectdef(ttypesym(sym).restype.def);
                aktprocsym:=tprocsym(procinfo^._class.symtable.search(sp));
                {The procedure has been found. So it is
                 a global one. Set the flags to mark this.}
                procinfo^.flags:=procinfo^.flags or pi_is_global;
                aktobjectdef:=nil;
                { we solve this below }
                if not(assigned(aktprocsym)) then
                  Message(parser_e_methode_id_expected);
             end;
         end
        else
         begin
           { check for constructor/destructor which is not allowed here }
           if (not parse_only) and
              (options in [potype_constructor,potype_destructor]) then
              Message(parser_e_constructors_always_objects);

           akttokenpos:=procstartfilepos;
           aktprocsym:=tprocsym(symtablestack.search(sp));

           if not(parse_only) then
             begin
               {The procedure we prepare for is in the implementation
                part of the unit we compile. It is also possible that we
                are compiling a program, which is also some kind of
                implementaion part.

                We need to find out if the procedure is global. If it is
                global, it is in the global symtable.}
               if not assigned(aktprocsym) and
                  (symtablestack.symtabletype=staticsymtable) and
                  assigned(symtablestack.next) and
                  (symtablestack.next.unitid=0) then
                begin
                  {Search the procedure in the global symtable.}
                  aktprocsym:=tprocsym(symtablestack.next.search(sp));
                  if assigned(aktprocsym) then
                   begin
                     {Check if it is a procedure.}
                     if aktprocsym.typ<>procsym then
                      DuplicateSym(aktprocsym);
                     {The procedure has been found. So it is
                      a global one. Set the flags to mark this.}
                     procinfo^.flags:=procinfo^.flags or pi_is_global;
                   end;
                end;
             end;
         end;

      { Create the mangledname }
      {$ifndef UseNiceNames}
        if assigned(procinfo^._class) then
         begin
           if (pos('_$$_',procprefix)=0) then
            hs:=procprefix+'_$$_'+upper(procinfo^._class.objname^)+'_$$_'+sp
           else
            hs:=procprefix+'_$'+sp;
         end
        else
         begin
           if lexlevel=normal_function_level then
            hs:=procprefix+'_'+sp
           else
            hs:=procprefix+'_$'+sp;
         end;
      {$else UseNiceNames}
        if assigned(procinfo^._class) then
         begin
           if (pos('_5Class_',procprefix)=0) then
            hs:=procprefix+'_5Class_'+procinfo^._class.name^+'_'+tostr(length(sp))+sp
           else
            hs:=procprefix+'_'+tostr(length(sp))+sp;
         end
        else
         begin
           if lexlevel=normal_function_level then
            hs:=procprefix+'_'+tostr(length(sp))+sp
           else
            hs:=lowercase(procprefix)+'_'+tostr(length(sp))+sp;
         end;
      {$endif UseNiceNames}

        doinsert:=true;
        if assigned(aktprocsym) then
         begin
           { Check if overloaded is a procsym }
           if aktprocsym.typ<>procsym then
            begin
              { when the other symbol is a unit symbol then hide the unit
                symbol. Only in tp mode because it's bad programming }
              if (m_tp in aktmodeswitches) and
                 (aktprocsym.typ=unitsym) then
               begin
                 aktprocsym.owner.rename(aktprocsym.name,'hidden'+aktprocsym.name);
               end
              else
               begin
                 {  we use a different error message for tp7 so it looks more compatible }
                 if (m_fpc in aktmodeswitches) then
                  Message1(parser_e_overloaded_no_procedure,aktprocsym.realname)
                 else
                  DuplicateSym(aktprocsym);
                 { don't reinsert as that will generated another error }
                 doinsert:=false;
               end;
              { generate a new aktprocsym }
              aktprocsym:=nil;
            end;
         end;

        { test again if assigned, it can be reset to recover }
        if not assigned(aktprocsym) then
         begin
           { create a new procsym and set the real filepos }
           akttokenpos:=procstartfilepos;
           { for operator we have only one procsym for each overloaded
             operation }
           if (options=potype_operator) then
             begin
               { is the current overload sym already in the current unit }
               if assigned(overloaded_operators[optoken]) and
                  (overloaded_operators[optoken].owner=symtablestack) then
                 aktprocsym:=overloaded_operators[optoken]
               else
                 begin
                   { create the procsym with saving the original case }
                   aktprocsym:=tprocsym.create('$'+sp);
                   { add already known overloaded defs }
                   if assigned(overloaded_operators[optoken]) then
                    begin
                      pdl:=overloaded_operators[optoken].defs;
                      while assigned(pdl) do
                       begin
                         aktprocsym.addprocdef(pdl^.def);
                         pdl:=pdl^.next;
                       end;
                    end;
                 end;
             end
            else
             aktprocsym:=tprocsym.create(orgsp);
            if doinsert then
             symtablestack.insert(aktprocsym);
         end;

        st:=symtablestack;
        aktprocdef:=tprocdef.create;
        aktprocdef.symtablelevel:=symtablestack.symtablelevel;

        if assigned(procinfo^._class) then
          aktprocdef._class := procinfo^._class;

        { set the options from the caller (podestructor or poconstructor) }
        aktprocdef.proctypeoption:=options;

        { calculate the offset of the parameters }
        paramoffset:=8;

        { calculate frame pointer offset }
        if lexlevel>normal_function_level then
          begin
            procinfo^.framepointer_offset:=paramoffset;
            inc(paramoffset,target_info.size_of_pointer);
            { this is needed to get correct framepointer push for local
              forward functions !! }
            aktprocdef.parast.symtablelevel:=lexlevel;
          end;

        if assigned (procinfo^._Class)  and
           is_object(procinfo^._Class) and
           (aktprocdef.proctypeoption in [potype_constructor,potype_destructor]) then
          inc(paramoffset,target_info.size_of_pointer);

        { self pointer offset                       }
        { self isn't pushed in nested procedure of methods }
        if assigned(procinfo^._class) and (lexlevel=normal_function_level) then
          begin
            procinfo^.selfpointer_offset:=paramoffset;
            if assigned(aktprocdef) and
               not(po_containsself in aktprocdef.procoptions) then
              inc(paramoffset,target_info.size_of_pointer);
          end;

        { con/-destructor flag ? }
        if assigned (procinfo^._Class) and
           is_class(procinfo^._class) and
           (aktprocdef.proctypeoption in [potype_destructor,potype_constructor]) then
          inc(paramoffset,target_info.size_of_pointer);

        procinfo^.para_offset:=paramoffset;

        aktprocdef.parast.datasize:=0;

        { add procsym to the procdef }
        aktprocdef.procsym:=aktprocsym;

        { save file position }
        aktprocdef.fileinfo:=procstartfilepos;

        { store mangledname }
        aktprocdef.setmangledname(hs);

        if not parse_only then
          begin
             overloaded_level:=1;
             { we need another procprefix !!! }
             { count, but only those in the same unit !!}
             procdefs:=aktprocsym.defs;
             while assigned(procdefs) and
                   (procdefs^.def.owner.symtabletype in [globalsymtable,staticsymtable]) do
               begin
                  { only count already implemented functions }
                  if not(procdefs^.def.forwarddef) then
                    inc(overloaded_level);
                  procdefs:=procdefs^.next;
               end;
             if overloaded_level>0 then
               procprefix:=hs+'$'+tostr(overloaded_level)+'$'
             else
               procprefix:=hs+'$';
          end;

        { this must also be inserted in the right symtable !! PM }
        { otherwise we get subbtle problems with
          definitions of args defs in staticsymtable for
          implementation of a global method }
        if token=_LKLAMMER then
          parameter_dec(aktprocdef);

        { so we only restore the symtable now }
        symtablestack:=st;
        if (options=potype_operator) then
          overloaded_operators[optoken]:=aktprocsym;
      end;


    procedure parse_proc_dec;
      var
        hs : string;
        isclassmethod : boolean;
      begin
        inc(lexlevel);
      { read class method }
        if token=_CLASS then
         begin
           consume(_CLASS);
           isclassmethod:=true;
         end
        else
         isclassmethod:=false;
        case token of
           _FUNCTION : begin
                         consume(_FUNCTION);
                         parse_proc_head(potype_none);
                         if token<>_COLON then
                          begin
                             if assigned(aktprocsym) and
                                not(is_interface(aktprocdef._class)) and
                                not(aktprocdef.forwarddef) or
                               (m_repeat_forward in aktmodeswitches) then
                             begin
                               consume(_COLON);
                               consume_all_until(_SEMICOLON);
                             end;
                          end
                         else
                          begin
                            consume(_COLON);
                            inc(testcurobject);
                            single_type(aktprocdef.rettype,hs,false);
                            aktprocdef.test_if_fpu_result;
                            dec(testcurobject);
                          end;
                       end;
          _PROCEDURE : begin
                         consume(_PROCEDURE);
                         parse_proc_head(potype_none);
                         if assigned(aktprocsym) then
                           aktprocdef.rettype:=voidtype;
                       end;
        _CONSTRUCTOR : begin
                         consume(_CONSTRUCTOR);
                         parse_proc_head(potype_constructor);
                         if assigned(procinfo^._class) and
                            is_class(procinfo^._class) then
                          begin
                            { CLASS constructors return the created instance }
                            aktprocdef.rettype.setdef(procinfo^._class);
                          end
                         else
                          begin
                            { OBJECT constructors return a boolean }
                            aktprocdef.rettype:=booltype;
                          end;
                       end;
         _DESTRUCTOR : begin
                         consume(_DESTRUCTOR);
                         parse_proc_head(potype_destructor);
                         aktprocdef.rettype:=voidtype;
                       end;
           _OPERATOR : begin
                         if lexlevel>normal_function_level then
                           Message(parser_e_no_local_operator);
                         consume(_OPERATOR);
                         if (token in [first_overloaded..last_overloaded]) then
                          begin
                            procinfo^.flags:=procinfo^.flags or pi_operator;
                            optoken:=token;
                          end
                         else
                          begin
                            Message(parser_e_overload_operator_failed);
                            { Use the dummy NOTOKEN that is also declared
                              for the overloaded_operator[] }
                            optoken:=NOTOKEN;
                          end;
                         consume(Token);
                         parse_proc_head(potype_operator);
                         if token<>_ID then
                           begin
                              otsym:=nil;
                              if not(m_result in aktmodeswitches) then
                                consume(_ID);
                           end
                         else
                           begin
                             otsym:=tvarsym.create(pattern,voidtype);
                             consume(_ID);
                           end;
                         if not try_to_consume(_COLON) then
                           begin
                             consume(_COLON);
                             aktprocdef.rettype:=generrortype;
                             consume_all_until(_SEMICOLON);
                           end
                         else
                          begin
                            single_type(aktprocdef.rettype,hs,false);
                            aktprocdef.test_if_fpu_result;
                            if (optoken in [_EQUAL,_GT,_LT,_GTE,_LTE]) and
                               ((aktprocdef.rettype.def.deftype<>
                               orddef) or (torddef(aktprocdef.
                               rettype.def).typ<>bool8bit)) then
                              Message(parser_e_comparative_operator_return_boolean);
                             if assigned(otsym) then
                               otsym.vartype.def:=aktprocdef.rettype.def;
                             { We need to add the return type in the mangledname
                               to allow overloading with just different results !! (PM) }
                             aktprocdef.setmangledname(
                               aktprocdef.mangledname+'$$'+hs);
                             if (optoken=_ASSIGNMENT) and
                                is_equal(aktprocdef.rettype.def,
                                   tvarsym(aktprocdef.parast.symindex.first).vartype.def) then
                               message(parser_e_no_such_assignment)
                             else if not isoperatoracceptable(aktprocdef,optoken) then
                               Message(parser_e_overload_impossible);
                           end;
                       end;
        end;
        if isclassmethod and
           assigned(aktprocsym) then
          include(aktprocdef.procoptions,po_classmethod);
        { support procedure proc;stdcall export; in Delphi mode only }
        if not((m_delphi in aktmodeswitches) and
           is_proc_directive(token)) then
         consume(_SEMICOLON);
        dec(lexlevel);
      end;


{****************************************************************************
                        Procedure directive handlers
****************************************************************************}

procedure pd_far;
begin
  Message(parser_w_proc_far_ignored);
end;

procedure pd_near;
begin
  Message(parser_w_proc_near_ignored);
end;

procedure pd_export;
begin
  if assigned(procinfo^._class) then
    Message(parser_e_methods_dont_be_export);
  if lexlevel<>normal_function_level then
    Message(parser_e_dont_nest_export);
  { only os/2 needs this }
  if target_info.target=target_i386_os2 then
   begin
     aktprocdef.aliasnames.insert(aktprocsym.realname);
     procinfo^.exported:=true;
     if cs_link_deffile in aktglobalswitches then
       deffile.AddExport(aktprocdef.mangledname);
   end;
end;

procedure pd_forward;
begin
  aktprocdef.forwarddef:=true;
end;

procedure pd_alias;
begin
  consume(_COLON);
  aktprocdef.aliasnames.insert(get_stringconst);
end;

procedure pd_asmname;
begin
  aktprocdef.setmangledname(target_info.Cprefix+pattern);
  aktprocdef.has_mangledname:=true;
  if token=_CCHAR then
    consume(_CCHAR)
  else
    consume(_CSTRING);
  { we don't need anything else }
  aktprocdef.forwarddef:=false;
end;

procedure pd_intern;
begin
  consume(_COLON);
  aktprocdef.extnumber:=get_intconst;
end;

procedure pd_interrupt;
begin
{$ifndef i386}
  Message(parser_w_proc_interrupt_ignored);
{$else i386}
  if lexlevel<>normal_function_level then
    Message(parser_e_dont_nest_interrupt);
{$endif i386}
end;

procedure pd_abstract;
begin
  if (po_virtualmethod in aktprocdef.procoptions) then
    include(aktprocdef.procoptions,po_abstractmethod)
  else
    Message(parser_e_only_virtual_methods_abstract);
  { the method is defined }
  aktprocdef.forwarddef:=false;
end;

procedure pd_virtual;
{$ifdef WITHDMT}
var
  pt : tnode;
{$endif WITHDMT}
begin
  if (aktprocdef.proctypeoption=potype_constructor) and
     is_object(aktprocdef._class) then
    Message(parser_e_constructor_cannot_be_not_virtual);
{$ifdef WITHDMT}
  if is_object(aktprocdef._class) and
    (token<>_SEMICOLON) then
    begin
       { any type of parameter is allowed here! }
       pt:=comp_expr(true);
       if is_constintnode(pt) then
         begin
           include(aktprocdef.procoptions,po_msgint);
           aktprocdef.messageinf.i:=pt^.value;
         end
       else
         Message(parser_e_ill_msg_expr);
       disposetree(pt);
    end;
{$endif WITHDMT}
end;

procedure pd_static;
begin
  if (cs_static_keyword in aktmoduleswitches) then
    begin
      include(aktprocsym.symoptions,sp_static);
      include(aktprocdef.procoptions,po_staticmethod);
    end;
end;

procedure pd_override;
begin
  if not(is_class_or_interface(aktprocdef._class)) then
    Message(parser_e_no_object_override);
end;

procedure pd_overload;
begin
   include(aktprocsym.symoptions,sp_has_overloaded);
end;

procedure pd_message;
var
  pt : tnode;
begin
  { check parameter type }
  if not(po_containsself in aktprocdef.procoptions) and
     ((aktprocdef.minparacount<>1) or
      (aktprocdef.maxparacount<>1) or
      (TParaItem(aktprocdef.Para.first).paratyp<>vs_var)) then
   Message(parser_e_ill_msg_param);
  pt:=comp_expr(true);
  if pt.nodetype=stringconstn then
    begin
      include(aktprocdef.procoptions,po_msgstr);
      aktprocdef.messageinf.str:=strnew(tstringconstnode(pt).value_str);
    end
  else
   if is_constintnode(pt) then
    begin
      include(aktprocdef.procoptions,po_msgint);
      aktprocdef.messageinf.i:=tordconstnode(pt).value;
    end
  else
    Message(parser_e_ill_msg_expr);
  pt.free;
end;


procedure pd_reintroduce;
begin
  Message1(parser_w_proc_directive_ignored,'REINTRODUCE');
end;


procedure pd_syscall;
begin
  aktprocdef.forwarddef:=false;
  aktprocdef.extnumber:=get_intconst;
end;


procedure pd_external;
{
  If import_dll=nil the procedure is assumed to be in another
  object file. In that object file it should have the name to
  which import_name is pointing to. Otherwise, the procedure is
  assumed to be in the DLL to which import_dll is pointing to. In
  that case either import_nr<>0 or import_name<>nil is true, so
  the procedure is either imported by number or by name. (DM)
}
var
  import_dll,
  import_name : string;
  import_nr   : word;
begin
  aktprocdef.forwarddef:=false;
{ forbid local external procedures }
  if lexlevel>normal_function_level then
   Message(parser_e_no_local_external);
{ If the procedure should be imported from a DLL, a constant string follows.
  This isn't really correct, an contant string expression follows
  so we check if an semicolon follows, else a string constant have to
  follow (FK) }
  import_nr:=0;
  import_name:='';
  if not(token=_SEMICOLON) and not(idtoken=_NAME) then
    begin
      import_dll:=get_stringconst;
      if (idtoken=_NAME) then
       begin
         consume(_NAME);
         import_name:=get_stringconst;
       end;
      if (idtoken=_INDEX) then
       begin
         {After the word index follows the index number in the DLL.}
         consume(_INDEX);
         import_nr:=get_intconst;
       end;
      { default is to used the realname of the procedure }
      if (import_nr=0) and (import_name='') then
        import_name:=aktprocsym.realname;
      { create importlib if not already done }
      if not(current_module.uses_imports) then
       begin
         current_module.uses_imports:=true;
         importlib.preparelib(current_module.modulename^);
       end;
{$ifdef notused}
      if not(m_repeat_forward in aktmodeswitches) and
         { if the procedure is declared with the overload option     }
         { it requires a full declaration in the implementation part }
         not(sp_has_overloaded in aktprocsym.symoptions) then
        begin
          { we can only have one overloaded here ! }
          if assigned(aktprocdef.defs.next) then
            importlib.importprocedure(aktprocdef.defs.next.mangledname,
              import_dll,import_nr,import_name)
          else
            importlib.importprocedure(aktprocdef.mangledname,import_dll,import_nr,import_name);
        end
      else
{$endif notused}
      importlib.importprocedure(aktprocdef.mangledname,import_dll,import_nr,import_name);
    end
  else
    begin
      if (idtoken=_NAME) then
       begin
         consume(_NAME);
         import_name:=get_stringconst;
         aktprocdef.setmangledname(import_name);
         aktprocdef.has_mangledname:=true;
       end;
    end;
end;

type
   pd_handler=procedure;
   proc_dir_rec=record
     idtok     : ttoken;
     pd_flags  : longint;
     handler   : pd_handler;
     pocall    : tproccalloption;
     pooption  : tprocoptions;
     mutexclpocall : tproccalloptions;
     mutexclpotype : tproctypeoptions;
     mutexclpo     : tprocoptions;
   end;
const
  {Should contain the number of procedure directives we support.}
  num_proc_directives=36;
  proc_direcdata:array[1..num_proc_directives] of proc_dir_rec=
   (
    (
      idtok:_ABSTRACT;
      pd_flags : pd_interface+pd_object+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_abstract;
      pocall   : pocall_none;
      pooption : [po_abstractmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_ALIAS;
      pd_flags : pd_implemen+pd_body+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_alias;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASMNAME;
      pd_flags : pd_interface+pd_implemen+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_asmname;
      pocall   : pocall_cdecl;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASSEMBLER;
      pd_flags : pd_implemen+pd_body+pd_notobjintf;
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_assembler];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_CDECL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_cdecl;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_DYNAMIC;
      pd_flags : pd_interface+pd_object+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_EXPORT;
      pd_flags : pd_body+pd_global+pd_interface+pd_implemen{??}+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_export;
      pocall   : pocall_none;
      pooption : [po_exports];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt]
    ),(
      idtok:_EXTERNAL;
      pd_flags : pd_implemen+pd_interface+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_external;
      pocall   : pocall_none;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline,pocall_palmossyscall];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_assembler]
    ),(
      idtok:_FAR;
      pd_flags : pd_implemen+pd_body+pd_interface+pd_procvar+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_far;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_FAR16;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_far16;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_leftright]
    ),(
      idtok:_FORWARD;
      pd_flags : pd_implemen+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_forward;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_FPCCALL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_fpccall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_leftright]
    ),(
      idtok:_INLINE;
      pd_flags : pd_implemen+pd_body+pd_notobjintf;
      handler  : nil;
      pocall   : pocall_inline;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_external,po_interrupt]
    ),(
      idtok:_INTERNCONST;
      pd_flags : pd_implemen+pd_body+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_intern;
      pocall   : pocall_internconst;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_operator];
      mutexclpo     : []
    ),(
      idtok:_INTERNPROC;
      pd_flags : pd_implemen+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_intern;
      pocall   : pocall_internproc;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_assembler,po_iocheck,po_leftright]
    ),(
      idtok:_INTERRUPT;
      pd_flags : pd_implemen+pd_body+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_interrupt;
      pocall   : pocall_none;
      pooption : [po_interrupt];
      mutexclpocall : [pocall_internproc,pocall_cdecl,pocall_cppdecl,
                       pocall_inline,pocall_pascal,pocall_system,pocall_far16,pocall_fpccall];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_external,po_leftright,po_clearstack]
    ),(
      idtok:_IOCHECK;
      pd_flags : pd_implemen+pd_body+pd_notobjintf;
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_iocheck];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_MESSAGE;
      pd_flags : pd_interface+pd_object+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_message;
      pocall   : pocall_none;
      pooption : []; { can be po_msgstr or po_msgint }
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_interrupt,po_external]
    ),(
      idtok:_NEAR;
      pd_flags : pd_implemen+pd_body+pd_procvar+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_near;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERLOAD;
      pd_flags : pd_implemen+pd_interface+pd_body;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_overload;
      pocall   : pocall_none;
      pooption : [po_overload];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERRIDE;
      pd_flags : pd_interface+pd_object+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_override;
      pocall   : pocall_none;
      pooption : [po_overridingmethod,po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_external,po_interrupt]
    ),(
      idtok:_PASCAL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_pascal;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_POPSTACK;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_clearstack];
      mutexclpocall : [pocall_inline,pocall_internproc,pocall_stdcall];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_PUBLIC;
      pd_flags : pd_implemen+pd_body+pd_global+pd_notobject+pd_notobjintf;
      handler  : nil;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_REGISTER;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_register;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_REINTRODUCE;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_reintroduce;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_SAFECALL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_safecall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_SAVEREGISTERS;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar+pd_notobjintf;
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_saveregisters];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_STATIC;
      pd_flags : pd_interface+pd_object+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_static;
      pocall   : pocall_none;
      pooption : [po_staticmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt,po_exports]
    ),(
      idtok:_STDCALL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_stdcall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_SYSCALL;
      pd_flags : pd_interface+pd_implemen+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_syscall;
      pocall   : pocall_palmossyscall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_assembler,po_interrupt,po_exports]
    ),(
      idtok:_SYSTEM;
      pd_flags : pd_implemen+pd_notobjintf;
      handler  : nil;
      pocall   : pocall_system;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_assembler,po_interrupt]
    ),(
      idtok:_VIRTUAL;
      pd_flags : pd_interface+pd_object+pd_notobjintf;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports]
    ),(
      idtok:_CPPDECL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : pocall_cppdecl;
      pooption : [po_savestdregs];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_VARARGS;
      pd_flags : pd_interface+pd_implemen+pd_procvar;
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_varargs];
      mutexclpocall : [pocall_internproc,pocall_stdcall,pocall_register,
                       pocall_inline,pocall_far16,pocall_fpccall];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_interrupt,po_leftright]
    ),(
      idtok:_COMPILERPROC;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_notobjintf;
      handler  : nil;
      pocall   : pocall_compilerproc;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_interrupt]
    )
   );


    function is_proc_directive(tok:ttoken):boolean;
      var
        i : longint;
      begin
        is_proc_directive:=false;
        for i:=1 to num_proc_directives do
         if proc_direcdata[i].idtok=idtoken then
          begin
            is_proc_directive:=true;
            exit;
          end;
      end;


    function parse_proc_direc(var pdflags:word):boolean;
      {
        Parse the procedure directive, returns true if a correct directive is found
      }
      var
        p     : longint;
        found : boolean;
        name  : stringid;
      begin
        parse_proc_direc:=false;
        name:=tokeninfo^[idtoken].str;
        found:=false;

      { Hint directive? Then exit immediatly }
        if (m_hintdirective in aktmodeswitches) then
         begin
           case idtoken of
             _LIBRARY,
             _PLATFORM,
             _DEPRECATED :
               exit;
           end;
         end;

      { retrieve data for directive if found }
        for p:=1 to num_proc_directives do
         if proc_direcdata[p].idtok=idtoken then
          begin
            found:=true;
            break;
          end;

      { Check if the procedure directive is known }
        if not found then
         begin
            { parsing a procvar type the name can be any
              next variable !! }
            if (pdflags and (pd_procvar or pd_object))=0 then
              Message1(parser_w_unknown_proc_directive_ignored,name);
            exit;
         end;

        { static needs a special treatment }
        if (idtoken=_STATIC) and not (cs_static_keyword in aktmoduleswitches) then
          exit;

      { Conflicts between directives ? }
        if (aktprocdef.proctypeoption in proc_direcdata[p].mutexclpotype) or
           (aktprocdef.proccalloption in proc_direcdata[p].mutexclpocall) or
           ((aktprocdef.procoptions*proc_direcdata[p].mutexclpo)<>[]) then
         begin
           Message1(parser_e_proc_dir_conflict,name);
           exit;
         end;

      { set calling convention }
        if proc_direcdata[p].pocall<>pocall_none then
         begin
           if aktprocdef.proccalloption<>pocall_none then
            begin
              Message2(parser_w_proc_overriding_calling,
                proccalloptionStr[aktprocdef.proccalloption],
                proccalloptionStr[proc_direcdata[p].pocall]);
            end;
           aktprocdef.proccalloption:=proc_direcdata[p].pocall;
         end;

        if aktprocdef.deftype=procdef then
         begin
           { Check if the directive is only for objects }
           if ((proc_direcdata[p].pd_flags and pd_object)<>0) and
              not assigned(aktprocdef._class) then
            exit;

           { check if method and directive not for object public }
           if ((proc_direcdata[p].pd_flags and pd_notobject)<>0) and
              assigned(aktprocdef._class) then
            exit;

           { check if method and directive not for interface }
           if ((proc_direcdata[p].pd_flags and pd_notobjintf)<>0) and
              is_interface(aktprocdef._class) then
            exit;
         end;

      { consume directive, and turn flag on }
        consume(token);
        parse_proc_direc:=true;

      { Check the pd_flags if the directive should be allowed }
        if ((pdflags and pd_interface)<>0) and
           ((proc_direcdata[p].pd_flags and pd_interface)=0) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_interface,name);
            exit;
          end;
        if ((pdflags and pd_implemen)<>0) and
           ((proc_direcdata[p].pd_flags and pd_implemen)=0) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_implementation,name);
            exit;
          end;
        if ((pdflags and pd_procvar)<>0) and
           ((proc_direcdata[p].pd_flags and pd_procvar)=0) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_procvar,name);
            exit;
          end;

      { Return the new pd_flags }
        if (proc_direcdata[p].pd_flags and pd_body)=0 then
          pdflags:=pdflags and (not pd_body);
        if (proc_direcdata[p].pd_flags and pd_global)<>0 then
          pdflags:=pdflags or pd_global;

      { Add the correct flag }
        aktprocdef.procoptions:=aktprocdef.procoptions+proc_direcdata[p].pooption;

      { Call the handler }
        if pointer({$ifndef FPCPROCVAR}@{$endif}proc_direcdata[p].handler)<>nil then
          proc_direcdata[p].handler{$ifdef FPCPROCVAR}(){$endif};
      end;


    procedure handle_calling_convention(sym:tprocsym;def:tabstractprocdef);
      var
        st,parast : tsymtable;
        lastps,ps : tsym;
      begin
      { set the default calling convention }
        if def.proccalloption=pocall_none then
         def.proccalloption:=aktdefproccall;
        case def.proccalloption of
          pocall_cdecl :
            begin
              { use popstack and save std registers }
              include(def.procoptions,po_clearstack);
              include(def.procoptions,po_savestdregs);
              { set mangledname }
              if (def.deftype=procdef) then
               begin
                 if not tprocdef(def).has_mangledname then
                  tprocdef(def).setmangledname(target_info.Cprefix+sym.realname);
                 if not assigned(tprocdef(def).parast) then
                  internalerror(200110234);
                 { do not copy on local !! }
                 tprocdef(def).parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}resetvaluepara);
                 { Adjust positions of args for cdecl or stdcall }
                 tparasymtable(tprocdef(def).parast).set_alignment(target_info.size_of_longint);
               end;
            end;
          pocall_cppdecl :
            begin
              if not assigned(sym) then
               internalerror(200110231);
              { use popstack and save std registers }
              include(def.procoptions,po_clearstack);
              include(def.procoptions,po_savestdregs);
              { set mangledname }
              if (def.deftype=procdef) then
               begin
                 if not tprocdef(def).has_mangledname then
                  tprocdef(def).setmangledname(target_info.Cprefix+tprocdef(def).cplusplusmangledname);
                 if not assigned(tprocdef(def).parast) then
                  internalerror(200110235);
                 { do not copy on local !! }
                 tprocdef(def).parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}resetvaluepara);
                 { Adjust positions of args for cdecl or stdcall }
                 tparasymtable(tprocdef(def).parast).set_alignment(target_info.size_of_longint);
               end;
            end;
          pocall_stdcall :
            begin
              include(def.procoptions,po_savestdregs);
              if (def.deftype=procdef) and
                 assigned(tprocdef(def).parast) then
               begin
                 { Adjust positions of args for cdecl or stdcall }
                 tparasymtable(tprocdef(def).parast).set_alignment(target_info.size_of_longint);
               end;
            end;
          pocall_safecall :
            begin
              include(def.procoptions,po_savestdregs);
            end;
          pocall_compilerproc :
            begin
              if (not assigned(sym)) or
                 (def.deftype<>procdef) then
               internalerror(200110232);
              tprocdef(def).setmangledname(lower(sym.name));
            end;
          pocall_pascal :
            begin
              include(def.procoptions,po_leftright);
              st:=tparasymtable.create;
              parast:=tprocdef(def).parast;
              lastps:=nil;
              while assigned(parast.symindex.first) and (lastps<>tsym(parast.symindex.first)) do
                begin
                  ps:=tsym(parast.symindex.first);
                  while assigned(ps.indexnext) and (tsym(ps.indexnext)<>lastps) do
                    ps:=tsym(ps.indexnext);
                  ps.owner:=st;
                  { recalculate the corrected offset }
                  { the really_insert_in_data procedure
                    for parasymtable should only calculateoffset PM }
                  tstoredsym(ps).insert_in_data;
                  { reset the owner correctly }
                  ps.owner:=parast;
                  lastps:=ps;
                end;
            end;
          pocall_register :
            begin
              Message1(parser_w_proc_directive_ignored,'REGISTER');
            end;
          pocall_far16 :
            begin
              { Temporary stub, must be rewritten to support OS/2 far16 }
              Message1(parser_w_proc_directive_ignored,'FAR16');
            end;
          pocall_system :
            begin
              include(def.procoptions,po_clearstack);
              if (not assigned(sym)) or
                 (def.deftype<>procdef) then
               internalerror(200110233);
              if not tprocdef(def).has_mangledname then
               tprocdef(def).setmangledname(sym.realname);
            end;
          pocall_palmossyscall :
            begin
              { use popstack and save std registers }
              include(def.procoptions,po_clearstack);
              include(def.procoptions,po_savestdregs);
              if (def.deftype=procdef) then
               begin
                 if not assigned(tprocdef(def).parast) then
                  internalerror(200110236);
                 { do not copy on local !! }
                 tprocdef(def).parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}resetvaluepara);
                 { Adjust positions of args for cdecl or stdcall }
                 tparasymtable(tprocdef(def).parast).set_alignment(target_info.size_of_longint);
               end;
            end;
          pocall_inline :
            begin
              if not(cs_support_inline in aktmoduleswitches) then
               begin
                 Message(parser_e_proc_inline_not_supported);
                 def.proccalloption:=pocall_fpccall;
               end;
            end;
        end;

        { add mangledname to external list }
        if (def.deftype=procdef) and
           (po_external in def.procoptions) and
           target_info.DllScanSupported then
           current_module.externals.insert(tExternalsItem.create(tprocdef(def).mangledname));
      end;


    procedure parse_proc_directives(var pdflags:word);
      {
        Parse the procedure directives. It does not matter if procedure directives
        are written using ;procdir; or ['procdir'] syntax.
      }
      var
        res : boolean;
      begin
        while token in [_ID,_LECKKLAMMER] do
         begin
           if try_to_consume(_LECKKLAMMER) then
            begin
              repeat
                parse_proc_direc(pdflags);
              until not try_to_consume(_COMMA);
              consume(_RECKKLAMMER);
              { we always expect at least '[];' }
              res:=true;
            end
           else
            begin
              res:=parse_proc_direc(pdflags);
            end;
         { A procedure directive normally followed by a semicolon, but in
           a const section we should stop when _EQUAL is found }
           if res then
            begin
              if (block_type=bt_const) and
                 (token=_EQUAL) then
               break;
              { support procedure proc;stdcall export; in Delphi mode only }
              if not((m_delphi in aktmodeswitches) and
                     is_proc_directive(token)) then
               consume(_SEMICOLON);
            end
           else
            break;
         end;
        handle_calling_convention(aktprocsym,aktprocdef);
      end;


    procedure parse_var_proc_directives(var sym : tsym);
      var
        pdflags : word;
        oldsym  : tprocsym;
        olddef  : tprocdef;
        pd      : tabstractprocdef;
      begin
        oldsym:=aktprocsym;
        olddef:=aktprocdef;
        pdflags:=pd_procvar;
        { we create a temporary aktprocsym to read the directives }
        aktprocsym:=tprocsym.create(sym.name);
        case sym.typ of
          varsym :
            pd:=tabstractprocdef(tvarsym(sym).vartype.def);
          typedconstsym :
            pd:=tabstractprocdef(ttypedconstsym(sym).typedconsttype.def);
          typesym :
            pd:=tabstractprocdef(ttypesym(sym).restype.def);
          else
            internalerror(994932432);
        end;
        if pd.deftype<>procvardef then
         internalerror(994932433);
        tabstractprocdef(aktprocdef):=pd;
        { names should never be used anyway }
        inc(lexlevel);
        parse_proc_directives(pdflags);
        dec(lexlevel);
        aktprocsym.free;
        aktprocsym:=oldsym;
        aktprocdef:=olddef;
      end;


    procedure parse_object_proc_directives(var sym : tprocsym);
      var
        pdflags : word;
      begin
        pdflags:=pd_object;
        inc(lexlevel);
        parse_proc_directives(pdflags);
        dec(lexlevel);
        if (po_containsself in aktprocdef.procoptions) and
           (([po_msgstr,po_msgint]*aktprocdef.procoptions)=[]) then
          Message(parser_e_self_in_non_message_handler);
      end;


    function proc_add_definition(aprocsym:tprocsym;var aprocdef : tprocdef) : boolean;
      {
        Add definition aprocdef to the overloaded definitions of aprocsym. If a
        forwarddef is found and reused it returns true
      }
      var
        hd    : tprocdef;
        pdl   : pprocdeflist;
        ad,fd : tsym;
        forwardfound : boolean;
      begin
        forwardfound:=false;

        { check overloaded functions if the same function already exists }
        pdl:=aprocsym.defs;
        while assigned(pdl) do
         begin
           hd:=pdl^.def;

           { check the parameters, for delphi/tp it is possible to
             leave the parameters away in the implementation (forwarddef=false).
             But for an overload declared function this is not allowed }
           if { check if empty implementation arguments match is allowed }
              (
               not(m_repeat_forward in aktmodeswitches) and
               not(aprocdef.forwarddef) and
               (aprocdef.maxparacount=0) and
               not(po_overload in hd.procoptions)
              ) or
              { check arguments }
              (
               equal_paras(aprocdef.para,hd.para,cp_none) and
               { for operators equal_paras is not enough !! }
               ((aprocdef.proctypeoption<>potype_operator) or (optoken<>_ASSIGNMENT) or
                is_equal(hd.rettype.def,aprocdef.rettype.def))
              ) then
             begin
               { Check if we've found the forwarddef, if found then
                 we need to update the forward def with the current
                 implementation settings }
               if hd.forwarddef then
                 begin
                   { Check if the procedure type and return type are correct }
                   if (hd.proctypeoption<>aprocdef.proctypeoption) or
                      (not(is_equal(hd.rettype.def,aprocdef.rettype.def)) and
                      (m_repeat_forward in aktmodeswitches)) then
                     begin
                       MessagePos1(aprocdef.fileinfo,parser_e_header_dont_match_forward,
                                   aprocdef.fullprocname);
                       break;
                     end;

                   { Check if both are declared forward }
                   if hd.forwarddef and aprocdef.forwarddef then
                    begin
                      MessagePos1(aprocdef.fileinfo,parser_e_function_already_declared_public_forward,
                                  aprocdef.fullprocname);
                    end;

                   { internconst or internproc only need to be defined once }
                   if (hd.proccalloption in [pocall_internconst,pocall_internproc]) then
                    aprocdef.proccalloption:=hd.proccalloption
                   else
                    if (aprocdef.proccalloption in [pocall_internconst,pocall_internproc]) then
                     hd.proccalloption:=aprocdef.proccalloption;

                   { Check calling convention }
                   if (hd.proccalloption<>aprocdef.proccalloption) then
                    begin
                      { For delphi check if the current implementation has no proccalloption, then
                        take the options from the interface }
                      if (m_delphi in aktmodeswitches) then
                       begin
                         if (aprocdef.proccalloption=pocall_none) then
                          aprocdef.proccalloption:=hd.proccalloption
                         else
                          MessagePos(aprocdef.fileinfo,parser_e_call_convention_dont_match_forward);
                       end
                      else
                       MessagePos(aprocdef.fileinfo,parser_e_call_convention_dont_match_forward);
                      { restore interface settings for error recovery }
                      aprocdef.proccalloption:=hd.proccalloption;
                      aprocdef.setmangledname(hd.mangledname);
                    end;

                   { Check manglednames }
                   hd.count:=false;
                   if (m_repeat_forward in aktmodeswitches) or
                      aprocdef.haspara then
                    begin
                      if (hd.mangledname<>aprocdef.mangledname) then
                       begin
                         if not(po_external in aprocdef.procoptions) then
                           MessagePos2(aprocdef.fileinfo,parser_n_interface_name_diff_implementation_name,
                                       hd.mangledname,aprocdef.mangledname);
                         { if the mangledname is already used, then rename it to the
                           new mangledname of the implementation }
                         if hd.is_used then
                           renameasmsymbol(hd.mangledname,aprocdef.mangledname);
                         { reset the mangledname of the interface }
                         hd.setmangledname(aprocdef.mangledname);
                       end
                      else
                       begin
                         { If mangled names are equal then they have the same amount of arguments }
                         { We can check the names of the arguments }
                         { both symtables are in the same order from left to right }
                         ad:=tsym(hd.parast.symindex.first);
                         fd:=tsym(aprocdef.parast.symindex.first);
                         while assigned(ad) and assigned(fd) do
                          begin
                            if ad.name<>fd.name then
                             begin
                               { don't give an error if the default parameter is not
                                 specified in the implementation }
                               if ((copy(fd.name,1,3)='def') and
                                   (copy(ad.name,1,3)<>'def')) then
                                 MessagePos3(aprocdef.fileinfo,parser_e_header_different_var_names,
                                             aprocsym.name,ad.name,fd.name);
                               break;
                             end;
                            ad:=tsym(ad.indexnext);
                            fd:=tsym(fd.indexnext);
                          end;
                       end;
                    end;
                   hd.count:=true;

                   { Everything is checked, now we can update the forward declaration
                     with the new data from the implementation }
                   hd.forwarddef:=aprocdef.forwarddef;
                   hd.hasforward:=true;
                   hd.parast.address_fixup:=aprocdef.parast.address_fixup;
                   hd.procoptions:=hd.procoptions+aprocdef.procoptions;
                   if hd.extnumber=-1 then
                     hd.extnumber:=aprocdef.extnumber;
                   while not aprocdef.aliasnames.empty do
                    hd.aliasnames.insert(aprocdef.aliasnames.getfirst);

                   { for compilerproc defines we need to rename and update the
                     symbolname to lowercase }
                   if (aprocdef.proccalloption=pocall_compilerproc) then
                    begin
                      { rename to lowercase so users can't access it }
                      aprocsym.owner.rename(aprocsym.name,lower(aprocsym.name));
                      { also update the realname that is stored in the ppu }
                      stringdispose(aprocsym._realname);
                      aprocsym._realname:=stringdup('$'+aprocsym.name);
                      { the mangeled name is already changed by the pd_compilerproc }
                      { handler. It must be done immediately because if we have a   }
                      { call to a compilerproc before it's implementation is        }
                      { encountered, it must already use the new mangled name (JM)  }
                    end;

                   { return the forwarddef }
                   aprocdef:=hd;

                   forwardfound:=true;
                 end
               else
                begin
                  { abstract methods aren't forward defined, but this }
                  { needs another error message                   }
                  if (po_abstractmethod in hd.procoptions) then
                    MessagePos(aprocdef.fileinfo,parser_e_abstract_no_definition)
                  else
                    MessagePos(aprocdef.fileinfo,parser_e_overloaded_have_same_parameters);
                 end;

               { we found one proc with the same arguments, there are no others
                 so we can stop }
               break;
             end;

           { check for allowing overload directive }
           if not(m_fpc in aktmodeswitches) then
            begin
              { overload directive turns on overloading }
              if ((po_overload in aprocdef.procoptions) or
                  (po_overload in hd.procoptions)) then
               begin
                 { check if all procs have overloading, but not if the proc was
                   already declared forward, then the check is already done }
                 if not(hd.hasforward or
                        (aprocdef.forwarddef<>hd.forwarddef) or
                        ((po_overload in aprocdef.procoptions) and
                         (po_overload in hd.procoptions))) then
                  begin
                    MessagePos1(aprocdef.fileinfo,parser_e_no_overload_for_all_procs,aprocsym.realname);
                    break;
                  end;
               end
              else
               begin
                 if not(hd.forwarddef) then
                  begin
                    MessagePos(aprocdef.fileinfo,parser_e_procedure_overloading_is_off);
                    break;
                  end;
               end;
            end; { equal arguments }

           { try next overloaded }
           pdl:=pdl^.next;
         end;

        { if we didn't reuse a forwarddef then we add the procdef to the overloaded
          list }
        if not forwardfound then
         aprocsym.addprocdef(aprocdef);

        { insert otsym only in the right symtable }
        if ((procinfo^.flags and pi_operator)<>0) and
           assigned(otsym) and
           not parse_only then
          begin
            if ret_in_param(aprocdef.rettype.def) then
              begin
                aprocdef.parast.insert(otsym);
                { this increases the data size }
                { correct this to get the right ret $value }
                dec(aprocdef.parast.datasize,
                    align(otsym.getpushsize,aktprocdef.parast.dataalignment));
                { this allows to read the funcretoffset }
                otsym.address:=-4;
                otsym.varspez:=vs_var;
              end
            else
              aprocdef.localst.insert(otsym);
          end;

        proc_add_definition:=forwardfound;
      end;

end.
{
  $Log$
  Revision 1.45  2002-01-09 07:38:03  michael
  + Patch from peter for library imports

  Revision 1.44  2002/01/06 21:54:07  peter
    * fixed external <dll> name <c-name> manglednames

  Revision 1.43  2001/12/31 16:59:42  peter
    * protected/private symbols parsing fixed

  Revision 1.42  2001/12/06 17:57:36  florian
    + parasym to tparaitem added

  Revision 1.41  2001/11/02 22:58:03  peter
    * procsym definition rewrite

  Revision 1.40  2001/10/25 21:22:37  peter
    * calling convention rewrite

  Revision 1.39  2001/10/23 21:49:42  peter
    * $calling directive and -Cc commandline patch added
      from Pavel Ozerski

  Revision 1.38  2001/10/01 13:38:44  jonas
    * allow self parameter for normal procedures again (because Kylix allows
      it too) ("merged")

  Revision 1.37  2001/09/10 10:26:26  jonas
    * fixed web bug 1593
    * writing of procvar headers is more complete (mention var/const/out for
      paras, add "of object" if applicable)
    + error if declaring explicit self para as var/const
    * fixed mangled name of procedures which contain an explicit self para
    * parsing para's should be slightly faster because mangled name of
      procedure is only updated once instead of after parsing each para
      (all merged from fixes)

  Revision 1.36  2001/08/26 13:36:45  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.35  2001/08/23 14:28:36  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.34  2001/08/22 21:16:21  florian
    * some interfaces related problems regarding
      mapping of interface implementions fixed

  Revision 1.33  2001/08/19 21:11:20  florian
    * some bugs fix:
      - overload; with external procedures fixed
      - better selection of routine to do an overloaded
        type case
      - ... some more

  Revision 1.32  2001/08/19 11:22:23  peter
    * palmos support from v10 merged

  Revision 1.31  2001/08/05 13:18:50  peter
    * turn pocall_inline off when inline is not supported

  Revision 1.30  2001/08/01 15:07:29  jonas
    + "compilerproc" directive support, which turns both the public and mangled
      name to lowercase(declaration_name). This prevents a normal user from
      accessing the routine, but they can still be easily looked up within
      the compiler. This is used for helper procedures and should facilitate
      the writing of more processor independent code in the code generator
      itself (mostly written by Peter)
    + new "createintern" constructor for tcal nodes to create a call to
      helper exported using the "compilerproc" directive
    + support for high(dynamic_array) using the the above new things
    + definition of 'HASCOMPILERPROC' symbol (to be able to check in the
      compiler and rtl whether the "compilerproc" directive is supported)

  Revision 1.29  2001/07/09 21:11:14  peter
    * fixed overload checking for delphi. Empty parameters are only
      allowed in implementation and not when the forward declaration
      contains overload directive

  Revision 1.28  2001/07/01 20:16:16  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.27  2001/06/04 18:12:26  peter
    * fixed crash with procvar directive parsing. Be carefull as the procvar
      directive parsing uses aktprocdef that is a tprocdef, but
      for procvar the type is tprocvardef. So some fields are not available

  Revision 1.26  2001/06/04 11:53:13  peter
    + varargs directive

  Revision 1.25  2001/06/03 21:57:36  peter
    + hint directive parsing support

  Revision 1.24  2001/05/08 21:06:31  florian
    * some more support for widechars commited especially
      regarding type casting and constants

  Revision 1.23  2001/05/08 14:32:58  jonas
    * fixed bug for overloaded operators with a return type that has a size
      which isn't a multiple of the target_os.stackalignment (main branch
      patch from Peter)

  Revision 1.22  2001/05/04 15:52:03  florian
    * some Delphi incompatibilities fixed:
       - out, dispose and new can be used as idenfiers now
       - const p = apointerype(nil); is supported now
    + support for const p = apointertype(pointer(1234)); added

  Revision 1.21  2001/04/18 22:01:57  peter
    * registration of targets and assemblers

  Revision 1.20  2001/04/13 20:05:16  peter
    * better check for globalsymtable

  Revision 1.19  2001/04/13 18:03:16  peter
    * give error with local external procedure

  Revision 1.18  2001/04/13 01:22:11  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.17  2001/04/04 22:43:52  peter
    * remove unnecessary calls to firstpass

  Revision 1.16  2001/04/02 21:20:33  peter
    * resulttype rewrite

  Revision 1.15  2001/03/24 12:18:11  florian
    * procedure p(); is now allowed in all modes except TP

  Revision 1.14  2001/03/22 22:35:42  florian
    + support for type a = (a=1); in Delphi mode added
    + procedure p(); in Delphi mode supported
    + on isn't keyword anymore, it can be used as
      id etc. now

  Revision 1.13  2001/03/11 22:58:50  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.12  2001/03/06 18:28:02  peter
    * patch from Pavel with a new and much faster DLL Scanner for
      automatic importing so $linklib works for DLLs. Thanks Pavel!

  Revision 1.11  2001/01/08 21:40:26  peter
    * fixed crash with unsupported token overloading

  Revision 1.10  2000/12/25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.9  2000/11/29 00:30:35  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.8  2000/11/26 23:45:34  florian
    * pascal modifier in interfaces of units works now

  Revision 1.7  2000/11/06 20:30:55  peter
    * more fixes to get make cycle working

  Revision 1.6  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.5  2000/11/01 23:04:37  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.4  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.2  2000/10/15 07:47:51  peter
    * unit names and procedure names are stored mixed case

  Revision 1.1  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite
}

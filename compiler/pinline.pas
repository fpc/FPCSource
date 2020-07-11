{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generates nodes for routines that need compiler support

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
unit pinline;

{$i fpcdefs.inc}

interface

    uses
      symtype,
      node,
      globals;

    function new_dispose_statement(is_new:boolean) : tnode;
    function new_function : tnode;

    function inline_setlength : tnode;
    function inline_setstring : tnode;
    function inline_initialize : tnode;
    function inline_finalize : tnode;
    function inline_copy : tnode;
    function inline_insert : tnode;
    function inline_delete : tnode;
    function inline_concat : tnode;


implementation

    uses
       { global }
       globtype,tokens,verbose,constexp,
       systems,compinnr,
       { symtable }
       symbase,symconst,symdef,symsym,symtable,defutil,
       { pass 1 }
       pass_1,htypechk,
       ncal,nmem,ncnv,ninl,ncon,nld,nbas,ngenutil,nutils,
       { parser }
       scanner,
       pbase,pexpr;


    function new_dispose_statement(is_new:boolean) : tnode;
      var
        newstatement : tstatementnode;
        temp         : ttempcreatenode;
        para         : tcallparanode;
        p,p2     : tnode;
        again    : boolean; { dummy for do_proc_call }
        destructorname : TIDString;
        sym      : tsym;
        classh   : tobjectdef;
        callflag : tcallnodeflag;
        destructorpos,
        storepos : tfileposinfo;
        variantdesc : pvariantrecdesc;
        found : boolean;
        j,i : longint;
        variantselectsymbol : tfieldvarsym;
      begin
        if target_info.system in systems_managed_vm then
          message(parser_e_feature_unsupported_for_vm);
        consume(_LKLAMMER);
        p:=comp_expr([ef_accept_equal]);
        { calc return type }
        if is_new then
          begin
            set_varstate(p,vs_written,[]);
            valid_for_var(p,true);
          end
        else
          set_varstate(p,vs_readwritten,[vsf_must_be_valid]);
        if (m_mac in current_settings.modeswitches) and
           is_class(p.resultdef) then
          begin
            classh:=tobjectdef(p.resultdef);

            { make sure we call ObjPas.TObject.Create/Free and not a random }
            { create/free method in a macpas descendent object (since those }
            { are not supposed to be called automatically when you call     }
            { new/dispose)                                                  }
            while assigned(classh.childof) do
              classh := classh.childof;
            if is_new then
              begin
                sym:=search_struct_member(classh,'CREATE');
                p2 := cloadvmtaddrnode.create(ctypenode.create(p.resultdef));
              end
            else
              begin
                sym:=search_struct_member(classh,'FREE');
                p2 := p;
             end;

            if not(assigned(sym)) then
              begin
                 p.free;
                 if is_new then
                   p2.free;
                 new_dispose_statement := cerrornode.create;
                 consume_all_until(_RKLAMMER);
                 consume(_RKLAMMER);
                 exit;
              end;

            do_member_read(classh,false,sym,p2,again,[],nil);

            { we need the real called method }
            do_typecheckpass(p2);

            if (p2.nodetype=calln) and
               assigned(tcallnode(p2).procdefinition) then
              begin
                if is_new then
                  begin
                    if (tcallnode(p2).procdefinition.proctypeoption<>potype_constructor) then
                      Message(parser_e_expr_have_to_be_constructor_call);
                    p2.resultdef:=p.resultdef;
                    p2:=cassignmentnode.create(p,p2);
                    typecheckpass(p2);
                  end
                else
                  begin
                   { Free is not a destructor
                    if (tcallnode(p2).procdefinition.proctypeoption<>potype_destructor) then
                      Message(parser_e_expr_have_to_be_destructor_call);
                   }
                  end
              end
            else
              internalerror(2005061202);
            new_dispose_statement := p2;
          end
        { constructor,destructor specified }
        else if (([m_mac,m_iso,m_extpas]*current_settings.modeswitches)=[]) and
                try_to_consume(_COMMA) then
          begin
            { extended syntax of new and dispose }
            { function styled new is handled in factor }
            { destructors have no parameters }
            destructorname:=pattern;
            destructorpos:=current_tokenpos;
            consume(_ID);

            if is_typeparam(p.resultdef) then
              begin
                 p.free;
                 p:=factor(false,[]);
                 p.free;
                 consume(_RKLAMMER);
                 new_dispose_statement:=cnothingnode.create;
                 exit;
              end;

            if (p.resultdef.typ<>pointerdef) then
              begin
                 Message1(type_e_pointer_type_expected,p.resultdef.typename);
                 p.free;
                 p:=factor(false,[]);
                 p.free;
                 consume(_RKLAMMER);
                 new_dispose_statement:=cerrornode.create;
                 exit;
              end;
            { first parameter must be an object or class }
            if tpointerdef(p.resultdef).pointeddef.typ<>objectdef then
              begin
                 Message(parser_e_pointer_to_class_expected);
                 p.free;
                 new_dispose_statement:=factor(false,[]);
                 consume_all_until(_RKLAMMER);
                 consume(_RKLAMMER);
                 exit;
              end;
            { check, if the first parameter is a pointer to a _class_ }
            classh:=tobjectdef(tpointerdef(p.resultdef).pointeddef);
            if is_class(classh) then
              begin
                 Message(parser_e_no_new_or_dispose_for_classes);
                 new_dispose_statement:=factor(false,[]);
                 consume_all_until(_RKLAMMER);
                 consume(_RKLAMMER);
                 exit;
              end;
            { search cons-/destructor, also in parent classes }
            storepos:=current_tokenpos;
            current_tokenpos:=destructorpos;
            sym:=search_struct_member(classh,destructorname);
            current_tokenpos:=storepos;

            { the second parameter of new/dispose must be a call }
            { to a cons-/destructor                              }
            if (not assigned(sym)) or (sym.typ<>procsym) then
              begin
                 if is_new then
                  Message(parser_e_expr_have_to_be_constructor_call)
                 else
                  Message(parser_e_expr_have_to_be_destructor_call);
                 p.free;
                 new_dispose_statement:=cerrornode.create;
              end
            else
              begin
                { For new(var,constructor) we need to take a copy because
                  p is also used in the assignmentn below }
                if is_new then
                  begin
                    p2:=cderefnode.create(p.getcopy);
                    include(p2.flags,nf_no_checkpointer);
                  end
                else
                  p2:=cderefnode.create(p);
                do_typecheckpass(p2);
                if is_new then
                  callflag:=cnf_new_call
                else
                  callflag:=cnf_dispose_call;
                if is_new then
                  do_member_read(classh,false,sym,p2,again,[callflag],nil)
                else
                  begin
                    if not(m_fpc in current_settings.modeswitches) then
                      do_member_read(classh,false,sym,p2,again,[callflag],nil)
                    else
                      begin
                        p2:=ccallnode.create(nil,tprocsym(sym),sym.owner,p2,[callflag],nil);
                        { support dispose(p,done()); }
                        if try_to_consume(_LKLAMMER) then
                          begin
                            if not try_to_consume(_RKLAMMER) then
                              begin
                                Message(parser_e_no_paras_for_destructor);
                                consume_all_until(_RKLAMMER);
                                consume(_RKLAMMER);
                              end;
                          end;
                      end;
                  end;

                { we need the real called method }
                do_typecheckpass(p2);

                if (p2.nodetype=calln) and
                   assigned(tcallnode(p2).procdefinition) then
                  begin
                    if is_new then
                     begin
                       if (tcallnode(p2).procdefinition.proctypeoption<>potype_constructor) then
                         Message(parser_e_expr_have_to_be_constructor_call);
                       p2.resultdef:=p.resultdef;
                       p2:=cassignmentnode.create(p,p2);
                     end
                    else
                     begin
                       if (tcallnode(p2).procdefinition.proctypeoption<>potype_destructor) then
                         Message(parser_e_expr_have_to_be_destructor_call);
                     end;
                  end
                else
                  begin
                    if is_new then
                      CGMessage(parser_e_expr_have_to_be_constructor_call)
                    else
                      CGMessage(parser_e_expr_have_to_be_destructor_call);
                  end;

                result:=p2;
              end;
          end
        else
          begin
             if (p.resultdef.typ<>pointerdef) then
               Begin
                 if is_typeparam(p.resultdef) then
                   begin
                      p.free;
                      consume(_RKLAMMER);
                      new_dispose_statement:=cnothingnode.create;
                      exit;
                   end
                 else
                   begin
                     Message1(type_e_pointer_type_expected,p.resultdef.typename);
                     new_dispose_statement:=cerrornode.create;
                   end;
               end
             else
               begin
                  if (tpointerdef(p.resultdef).pointeddef.typ=objectdef) and
                     (oo_has_vmt in tobjectdef(tpointerdef(p.resultdef).pointeddef).objectoptions) then
                    Message(parser_w_use_extended_syntax_for_objects);
                  if (tpointerdef(p.resultdef).pointeddef.typ=orddef) and
                     (torddef(tpointerdef(p.resultdef).pointeddef).ordtype=uvoid) then
                    begin
                      if (m_tp7 in current_settings.modeswitches) or
                         (m_delphi in current_settings.modeswitches) then
                       Message(parser_w_no_new_dispose_on_void_pointers)
                      else
                       Message(parser_e_no_new_dispose_on_void_pointers);
                    end;

                  { create statements with call to getmem+initialize or
                    finalize+freemem }
                  new_dispose_statement:=internalstatements(newstatement);

                  if is_new then
                   begin
                     { create temp for result }
                     temp := ctempcreatenode.create(p.resultdef,p.resultdef.size,tt_persistent,true);
                     addstatement(newstatement,temp);

                     { create call to fpc_getmem }
                     para := ccallparanode.create(cordconstnode.create
                         (tpointerdef(p.resultdef).pointeddef.size,s32inttype,true),nil);
                     addstatement(newstatement,cassignmentnode.create(
                         ctemprefnode.create(temp),
                         ccallnode.createintern('fpc_getmem',para)));

                     { create call to fpc_initialize }
                     if is_managed_type(tpointerdef(p.resultdef).pointeddef) or
                       ((m_isolike_io in current_settings.modeswitches) and (tpointerdef(p.resultdef).pointeddef.typ=filedef)) then
                       addstatement(newstatement,cnodeutils.initialize_data_node(cderefnode.create(ctemprefnode.create(temp)),false));

                     { copy the temp to the destination }
                     addstatement(newstatement,cassignmentnode.create(
                         p,
                         ctemprefnode.create(temp)));

                     if (([m_iso,m_extpas]*current_settings.modeswitches)<>[]) and (is_record(tpointerdef(p.resultdef).pointeddef)) then
                       begin
                         variantdesc:=trecorddef(tpointerdef(p.resultdef).pointeddef).variantrecdesc;
                         while (token=_COMMA) and assigned(variantdesc) do
                           begin
                             consume(_COMMA);
                             p2:=factor(false,[]);
                             do_typecheckpass(p2);
                             if p2.nodetype=ordconstn then
                               begin
                                 found:=false;
                                 { we do not have dynamic dfa, so avoid warning on variantselectsymbol below }
                                 variantselectsymbol:=nil;
                                 for i:=0 to high(variantdesc^.branches) do
                                   begin
                                     for j:=0 to high(variantdesc^.branches[i].values) do
                                       if variantdesc^.branches[i].values[j]=tordconstnode(p2).value then
                                         begin
                                           found:=true;
                                           variantselectsymbol:=tfieldvarsym(variantdesc^.variantselector);
                                           variantdesc:=variantdesc^.branches[i].nestedvariant;
                                           break;
                                         end;
                                     if found then
                                       break;
                                   end;
                                 if found then
                                   begin
                                     { if no tag-field is given, do not create an assignment statement for it }
                                     if assigned(variantselectsymbol) then
                                       { setup variant selector }
                                       addstatement(newstatement,cassignmentnode.create(
                                           csubscriptnode.create(variantselectsymbol,
                                             cderefnode.create(ctemprefnode.create(temp))),
                                           p2));
                                   end
                                 else
                                   Message(parser_e_illegal_expression);
                               end
                             else
                               Message(parser_e_illegal_expression);
                           end;
                       end;
                     { release temp }
                     addstatement(newstatement,ctempdeletenode.create(temp));
                   end
                  else
                   begin
                     temp:=nil;
                     { create call to fpc_finalize }
                     if is_managed_type(tpointerdef(p.resultdef).pointeddef) then
                       if might_have_sideeffects(p) then
                         begin
                           { ensure that p gets evaluated only once, in case it is e.g. a call }
                           temp:=ctempcreatenode.create_value(p.resultdef,p.resultdef.size,tt_persistent,true,p);
                           addstatement(newstatement,temp);
                           addstatement(newstatement,cnodeutils.finalize_data_node(cderefnode.create(ctemprefnode.create(temp))));
                         end
                       else
                         addstatement(newstatement,cnodeutils.finalize_data_node(cderefnode.create(p.getcopy)));

                     { create call to fpc_freemem }
                     if not assigned(temp) then
                       para := ccallparanode.create(p,nil)
                     else
                       para := ccallparanode.create(ctemprefnode.create(temp),nil);
                     addstatement(newstatement,ccallnode.createintern('fpc_freemem',para));
                     if assigned(temp) then
                       addstatement(newstatement,ctempdeletenode.create(temp));
                   end;
               end;
          end;
        consume(_RKLAMMER);
      end;


    function new_function : tnode;
      var
        p1,p2  : tnode;
        classh : tobjectdef;
        srsym    : tsym;
        srsymtable : TSymtable;
        again  : boolean; { dummy for do_proc_call }
      begin
        if target_info.system in systems_managed_vm then
          message(parser_e_feature_unsupported_for_vm);
        consume(_LKLAMMER);
        p1:=factor(false,[]);
        if p1.nodetype<>typen then
         begin
           Message(type_e_type_id_expected);
           consume_all_until(_RKLAMMER);
           consume(_RKLAMMER);
           p1.destroy;
           new_function:=cerrornode.create;
           exit;
         end;

        if (p1.resultdef.typ<>pointerdef) then
         begin
           Message1(type_e_pointer_type_expected,p1.resultdef.typename);
           consume_all_until(_RKLAMMER);
           consume(_RKLAMMER);
           p1.destroy;
           new_function:=cerrornode.create;
           exit;
         end;

        if try_to_consume(_RKLAMMER) then
          begin
            if (tpointerdef(p1.resultdef).pointeddef.typ=objectdef) and
               (oo_has_vmt in tobjectdef(tpointerdef(p1.resultdef).pointeddef).objectoptions)  then
              Message(parser_w_use_extended_syntax_for_objects);

            if p1.nodetype=typen then
              ttypenode(p1).allowed:=true;

            p1:=cinlinenode.create(in_new_x,false,p1);
          end
        else
          begin
            consume(_COMMA);
            if tpointerdef(p1.resultdef).pointeddef.typ<>objectdef then
             begin
               Message(parser_e_pointer_to_class_expected);
               consume_all_until(_RKLAMMER);
               consume(_RKLAMMER);
               p1.destroy;
               new_function:=cerrornode.create;
               exit;
             end;
            classh:=tobjectdef(tpointerdef(p1.resultdef).pointeddef);
            { use the objectdef for loading the VMT }
            p2:=p1;
            p1:=ctypenode.create(tpointerdef(p1.resultdef).pointeddef);
            do_typecheckpass(p1);
            { search the constructor also in the symbol tables of
              the parents }
            afterassignment:=false;
            searchsym_in_class(classh,classh,pattern,srsym,srsymtable,[ssf_search_helper]);
            consume(_ID);
            do_member_read(classh,false,srsym,p1,again,[cnf_new_call],nil);
            { we need to know which procedure is called }
            do_typecheckpass(p1);
            if not(
                   (p1.nodetype=calln) and
                   assigned(tcallnode(p1).procdefinition) and
                   (tcallnode(p1).procdefinition.proctypeoption=potype_constructor)
                  ) then
              Message(parser_e_expr_have_to_be_constructor_call);
            { constructors return boolean, update resultdef to return
              the pointer to the object }
            p1.resultdef:=p2.resultdef;
            p2.free;
            consume(_RKLAMMER);
          end;
        new_function:=p1;
      end;


    function inline_setlength : tnode;
      var
        paras: tnode;
      begin
        consume(_LKLAMMER);
        paras:=parse_paras(false,false,_RKLAMMER);
        consume(_RKLAMMER);
        if not assigned(paras) then
         begin
           result:=cerrornode.create;
           CGMessage1(parser_e_wrong_parameter_size,'SetLength');
           exit;
         end;
        result:=cinlinenode.create(in_setlength_x,false,paras);
      end;


    function inline_setstring : tnode;
      var
        paras, strpara, pcharpara: tnode;
        procname: string;
        cp: tstringencoding;
      begin
        consume(_LKLAMMER);
        paras:=parse_paras(false,false,_RKLAMMER);
        consume(_RKLAMMER);
        procname:='';
        if assigned(paras) and
           assigned(tcallparanode(paras).right) and
           assigned(tcallparanode(tcallparanode(paras).right).right) then
          begin
            do_typecheckpass(tcallparanode(tcallparanode(paras).right).left);
            do_typecheckpass(tcallparanode(tcallparanode(tcallparanode(paras).right).right).left);
            pcharpara:=tcallparanode(tcallparanode(paras).right).left;
            strpara:=tcallparanode(tcallparanode(tcallparanode(paras).right).right).left;
            if strpara.resultdef.typ=stringdef then
              begin
                { if there are three parameters and the first parameter
                  ( = paras.right.right) is an ansistring, add a codepage
                  parameter }
                if is_ansistring(strpara.resultdef) then
                  begin
                    cp:=tstringdef(strpara.resultdef).encoding;
                    if (cp=globals.CP_NONE) then
                      cp:=0;
                    paras:=ccallparanode.create(genintconstnode(cp),paras);
                  end;
                procname:='fpc_setstring_'+tstringdef(strpara.resultdef).stringtypname;
                { decide which version to call based on the second parameter }
                if not is_shortstring(strpara.resultdef) then
                  if is_pwidechar(pcharpara.resultdef) or
                     is_widechar(pcharpara.resultdef) or
                     ((pcharpara.resultdef.typ=arraydef) and
                      is_widechar(tarraydef(pcharpara.resultdef).elementdef)) then
                    procname:=procname+'_pwidechar'
                  else
                    procname:=procname+'_pansichar';
              end;
          end;
        { default version (for error message) in case of missing or wrong
          parameters }
        if procname='' then
          if m_default_unicodestring in current_settings.modeswitches then
            procname:='fpc_setstring_unicodestr_pwidechar'
          else if m_default_ansistring in current_settings.modeswitches then
            procname:='fpc_setstring_ansistr_pansichar'
          else
            procname:='fpc_setstring_shortstr';
        result:=ccallnode.createintern(procname,paras)
      end;


    function inline_initfinal(isinit: boolean): tnode;
      var
        newblock,
        paras   : tnode;
        npara,
        destppn,
        ppn     : tcallparanode;
      begin
        { for easy exiting if something goes wrong }
        result := cerrornode.create;

        consume(_LKLAMMER);
        paras:=parse_paras(false,false,_RKLAMMER);
        consume(_RKLAMMER);
        ppn:=tcallparanode(paras);

        if not assigned(paras) or
           (assigned(ppn.right) and
            assigned(tcallparanode(ppn.right).right)) then
         begin
           if isinit then
             CGMessage1(parser_e_wrong_parameter_size,'Initialize')
           else
             CGMessage1(parser_e_wrong_parameter_size,'Finalize');
           exit;
         end;

        { 2 arguments? }
        if assigned(ppn.right) then
         begin
           destppn:=tcallparanode(ppn.right);
           { create call to fpc_initialize/finalize_array }
           npara:=ccallparanode.create(ctypeconvnode.create
                     (ppn.left,s32inttype),
                  ccallparanode.create(caddrnode.create_internal
                     (crttinode.create(tstoreddef(destppn.left.resultdef),initrtti,rdt_normal)),
                  ccallparanode.create(caddrnode.create_internal
                     (destppn.left),nil)));
           if isinit then
             newblock:=ccallnode.createintern('fpc_initialize_array',npara)
           else
             newblock:=ccallnode.createintern('fpc_finalize_array',npara);
           destppn.left:=nil;
         end
        else
         begin
           if isinit then
             newblock:=cnodeutils.initialize_data_node(ppn.left,true)
           else
             newblock:=cnodeutils.finalize_data_node(ppn.left);
         end;
        ppn.left:=nil;
        paras.free;
        result.free;
        result:=newblock;
      end;


    function inline_initialize : tnode;
      begin
        result:=inline_initfinal(true);
      end;


    function inline_finalize : tnode;
      begin
        result:=inline_initfinal(false);
      end;


    function inline_copy_insert_delete(nr:tinlinenumber;name:string;checkempty:boolean) : tnode;
      var
        paras   : tnode;
        { for easy exiting if something goes wrong }
      begin
        result := cerrornode.create;

        consume(_LKLAMMER);
        paras:=parse_paras(false,false,_RKLAMMER);
        consume(_RKLAMMER);
        if not assigned(paras) and checkempty then
          begin
            CGMessage1(parser_e_wrong_parameter_size,name);
            exit;
          end;
        result.free;
        result:=cinlinenode.create(nr,false,paras);
      end;


    function inline_copy: tnode;
      begin
        result:=inline_copy_insert_delete(in_copy_x,'Copy',true);
      end;


    function inline_insert: tnode;
      begin
        result:=inline_copy_insert_delete(in_insert_x_y_z,'Insert',false);
      end;


    function inline_delete: tnode;
      begin
        result:=inline_copy_insert_delete(in_delete_x_y_z,'Delete',false);
      end;


    function inline_concat: tnode;
      begin
        result:=inline_copy_insert_delete(in_concat_x,'Concat',false);
      end;


end.

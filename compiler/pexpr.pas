{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    Does parsing of expression for Free Pascal

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
unit pexpr;

  interface

    uses symtable,tree;

    { reads a whole expression }
    function expr : ptree;

    { reads an expression without assignements and .. }
    function comp_expr(accept_equal : boolean):Ptree;

    { reads a single factor }
    function factor(getaddr : boolean) : ptree;

    { the ID token has to be consumed before calling this function }
    procedure do_member_read(getaddr : boolean;const sym : psym;var p1 : ptree;
      var pd : pdef;var again : boolean);

    function get_intconst:longint;

    function get_stringconst:string;

  implementation

    uses
       globtype,systems,tokens,
       cobjects,globals,scanner,
       symconst,aasm,
{$ifdef newcg}
       cgbase,
{$else}
       hcodegen,
{$endif}
       types,verbose,strings,
{$ifndef newcg}
       tccal,
{$endif newcg}
       pass_1,
       { parser specific stuff }
       pbase,ptype,
       { processor specific stuff }
       cpubase,cpuinfo;

    const
      allow_type : boolean = true;
      got_addrn  : boolean = false;

    function parse_paras(__colon,in_prop_paras : boolean) : ptree;

      var
         p1,p2 : ptree;
         end_of_paras : ttoken;

      begin
         if in_prop_paras  then
           end_of_paras:=_RECKKLAMMER
         else
           end_of_paras:=_RKLAMMER;
         if token=end_of_paras then
           begin
              parse_paras:=nil;
              exit;
           end;
         p2:=nil;
         inc(parsing_para_level);
         while true do
           begin
              p1:=comp_expr(true);
              p2:=gencallparanode(p1,p2);
              { it's for the str(l:5,s); }
              if __colon and (token=_COLON) then
                begin
                   consume(_COLON);
                   p1:=comp_expr(true);
                   p2:=gencallparanode(p1,p2);
                   p2^.is_colon_para:=true;
                   if token=_COLON then
                     begin
                        consume(_COLON);
                        p1:=comp_expr(true);
                        p2:=gencallparanode(p1,p2);
                        p2^.is_colon_para:=true;
                     end
                end;
              if token=_COMMA then
                consume(_COMMA)
              else
                break;
           end;
         dec(parsing_para_level);
         parse_paras:=p2;
      end;


    procedure check_tp_procvar(var p : ptree);
      var
         p1 : ptree;

      begin
         if (m_tp_procvar in aktmodeswitches) and
            (not got_addrn) and
            (not in_args) and
            (p^.treetype=loadn) then
            begin
               { support if procvar then for tp7 and many other expression like this }
               do_firstpass(p);
               set_varstate(p,false);
               { reset varstateset to maybe set used state later web bug769 PM }
               p^.varstateset:=false;
               if not(getprocvar) and (p^.resulttype^.deftype=procvardef) then
                 begin
                    p1:=gencallnode(nil,nil);
                    p1^.right:=p;
                    p1^.resulttype:=pprocvardef(p^.resulttype)^.rettype.def;
                    firstpass(p1);
                    p:=p1;
                 end;
            end;
      end;


    function statement_syssym(l : longint;var pd : pdef) : ptree;
      var
        p1,p2,paras  : ptree;
        prev_in_args : boolean;
      begin
        prev_in_args:=in_args;
        case l of
          in_ord_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_RKLAMMER);
              do_firstpass(p1);
              p1:=geninlinenode(in_ord_x,false,p1);
              do_firstpass(p1);
              statement_syssym := p1;
              pd:=p1^.resulttype;
            end;

          in_break :
            begin
              statement_syssym:=genzeronode(breakn);
              pd:=voiddef;
            end;

          in_continue :
            begin
              statement_syssym:=genzeronode(continuen);
              pd:=voiddef;
            end;

          in_typeof_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              {allow_type:=true;}
              p1:=comp_expr(true);
              {allow_type:=false;}
              consume(_RKLAMMER);
              pd:=voidpointerdef;
              if p1^.treetype=typen then
               begin
                 if (p1^.typenodetype=nil) then
                  begin
                    Message(type_e_mismatch);
                    statement_syssym:=genzeronode(errorn);
                  end
                 else
                  if p1^.typenodetype^.deftype=objectdef then
                   begin
                      { we can use resulttype in pass_2 (PM) }
                      p1^.resulttype:=p1^.typenodetype;
                      statement_syssym:=geninlinenode(in_typeof_x,false,p1);
                   end
                 else
                  begin
                    Message(type_e_mismatch);
                    disposetree(p1);
                    statement_syssym:=genzeronode(errorn);
                  end;
               end
              else { not a type node }
               begin
                 do_firstpass(p1);
                 set_varstate(p1,false);
                 if (p1^.resulttype=nil) then
                  begin
                    Message(type_e_mismatch);
                    disposetree(p1);
                    statement_syssym:=genzeronode(errorn)
                  end
                 else
                  if p1^.resulttype^.deftype=objectdef then
                   statement_syssym:=geninlinenode(in_typeof_x,false,p1)
                 else
                  begin
                    Message(type_e_mismatch);
                    statement_syssym:=genzeronode(errorn);
                    disposetree(p1);
                  end;
               end;
            end;

          in_sizeof_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              {allow_type:=true;}
              p1:=comp_expr(true);
              {allow_type:=false; }
              consume(_RKLAMMER);
              pd:=s32bitdef;
              if p1^.treetype=typen then
               begin
                 statement_syssym:=genordinalconstnode(p1^.typenodetype^.size,pd);
                 { p1 not needed !}
                 disposetree(p1);
               end
              else
               begin
                 do_firstpass(p1);
                 if ((p1^.resulttype^.deftype=objectdef) and
                     (oo_has_constructor in pobjectdef(p1^.resulttype)^.objectoptions)) or
                    is_open_array(p1^.resulttype) or
                    is_open_string(p1^.resulttype) then
                  statement_syssym:=geninlinenode(in_sizeof_x,false,p1)
                 else
                  begin
                    statement_syssym:=genordinalconstnode(p1^.resulttype^.size,pd);
                    { p1 not needed !}
                    disposetree(p1);
                  end;
               end;
            end;

          in_assigned_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              do_firstpass(p1);
              if not codegenerror then
               begin
                 case p1^.resulttype^.deftype of
                   pointerdef,
                   procvardef,
                   classrefdef : ;
                   objectdef :
                     if not(pobjectdef(p1^.resulttype)^.is_class) then
                       Message(parser_e_illegal_parameter_list);
                   else
                     Message(parser_e_illegal_parameter_list);
                 end;
               end;
              p2:=gencallparanode(p1,nil);
              p2:=geninlinenode(in_assigned_x,false,p2);
              consume(_RKLAMMER);
              pd:=booldef;
              statement_syssym:=p2;
            end;

          in_ofs_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p1:=gensinglenode(addrn,p1);
              do_firstpass(p1);

              { Ofs() returns a longint, not a pointer }
              p1^.resulttype:=u32bitdef;
              pd:=p1^.resulttype;
              consume(_RKLAMMER);
              statement_syssym:=p1;
            end;

          in_addr_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p1:=gensinglenode(addrn,p1);
              do_firstpass(p1);
              pd:=p1^.resulttype;
              consume(_RKLAMMER);
              statement_syssym:=p1;
            end;

          in_seg_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              do_firstpass(p1);
              set_varstate(p1,false);
              if p1^.location.loc<>LOC_REFERENCE then
                Message(cg_e_illegal_expression);
              p1:=genordinalconstnode(0,s32bitdef);
              pd:=s32bitdef;
              consume(_RKLAMMER);
              statement_syssym:=p1;
            end;

          in_high_x,
          in_low_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              {allow_type:=true;}
              p1:=comp_expr(true);
              {allow_type:=false;}
              do_firstpass(p1);
              if p1^.treetype=typen then
                p1^.resulttype:=p1^.typenodetype;
              p2:=geninlinenode(l,false,p1);
              consume(_RKLAMMER);
              pd:=s32bitdef;
              statement_syssym:=p2;
            end;

          in_succ_x,
          in_pred_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              do_firstpass(p1);
              p2:=geninlinenode(l,false,p1);
              consume(_RKLAMMER);
              pd:=p1^.resulttype;
              statement_syssym:=p2;
            end;

          in_inc_x,
          in_dec_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if token=_COMMA then
               begin
                 consume(_COMMA);
                 p2:=gencallparanode(comp_expr(true),nil);
               end
              else
               p2:=nil;
              p2:=gencallparanode(p1,p2);
              statement_syssym:=geninlinenode(l,false,p2);
              consume(_RKLAMMER);
              pd:=voiddef;
            end;

          in_concat_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p2:=nil;
              while true do
               begin
                 p1:=comp_expr(true);
                 do_firstpass(p1);
                 set_varstate(p1,true);
                 if not((p1^.resulttype^.deftype=stringdef) or
                        ((p1^.resulttype^.deftype=orddef) and
                         (porddef(p1^.resulttype)^.typ=uchar))) then
                   Message(parser_e_illegal_parameter_list);
                 if p2<>nil then
                  p2:=gennode(addn,p2,p1)
                 else
                  p2:=p1;
                 if token=_COMMA then
                  consume(_COMMA)
                 else
                  break;
               end;
              consume(_RKLAMMER);
              pd:=cshortstringdef;
              statement_syssym:=p2;
            end;

          in_read_x,
          in_readln_x :
            begin
              if token=_LKLAMMER then
               begin
                 consume(_LKLAMMER);
                 in_args:=true;
                 paras:=parse_paras(false,false);
                 consume(_RKLAMMER);
               end
              else
               paras:=nil;
              pd:=voiddef;
              p1:=geninlinenode(l,false,paras);
              do_firstpass(p1);
              statement_syssym := p1;
            end;

          in_write_x,
          in_writeln_x :
            begin
              if token=_LKLAMMER then
               begin
                 consume(_LKLAMMER);
                 in_args:=true;
                 paras:=parse_paras(true,false);
                 consume(_RKLAMMER);
               end
              else
               paras:=nil;
              pd:=voiddef;
              p1 := geninlinenode(l,false,paras);
              do_firstpass(p1);
              statement_syssym := p1;
            end;

          in_str_x_string :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              paras:=parse_paras(true,false);
              consume(_RKLAMMER);
              p1 := geninlinenode(l,false,paras);
              do_firstpass(p1);
              statement_syssym := p1;
              pd:=voiddef;
            end;

          in_val_x:
            Begin
              consume(_LKLAMMER);
              in_args := true;
              p1:= gencallparanode(comp_expr(true), nil);
              consume(_COMMA);
              p2 := gencallparanode(comp_expr(true),p1);
              if (token = _COMMA) then
                Begin
                  consume(_COMMA);
                  p2 := gencallparanode(comp_expr(true),p2)
                End;
              consume(_RKLAMMER);
              p2 := geninlinenode(l,false,p2);
              do_firstpass(p2);
              statement_syssym := p2;
              pd := voiddef;
            End;

          in_include_x_y,
          in_exclude_x_y :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_COMMA);
              p2:=comp_expr(true);
              statement_syssym:=geninlinenode(l,false,gencallparanode(p1,gencallparanode(p2,nil)));
              consume(_RKLAMMER);
              pd:=voiddef;
            end;

          in_assert_x_y :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if token=_COMMA then
               begin
                 consume(_COMMA);
                 p2:=comp_expr(true);
               end
              else
               begin
                 { then insert an empty string }
                 p2:=genstringconstnode('');
               end;
              statement_syssym:=geninlinenode(l,false,gencallparanode(p1,gencallparanode(p2,nil)));
              consume(_RKLAMMER);
              pd:=voiddef;
            end;

          else
            internalerror(15);

        end;
        in_args:=prev_in_args;
      end;


    { reads the parameter for a subroutine call }
    procedure do_proc_call(getaddr : boolean;var again : boolean;var p1:Ptree;var pd:Pdef);
      var
         prev_in_args : boolean;
         prevafterassn : boolean;
      begin
         prev_in_args:=in_args;
         prevafterassn:=afterassignment;
         afterassignment:=false;
         { want we only determine the address of }
         { a subroutine ?                       }
         if not(getaddr) then
           begin
              if token=_LKLAMMER then
                begin
                   consume(_LKLAMMER);
                   in_args:=true;
                   p1^.left:=parse_paras(false,false);
                   consume(_RKLAMMER);
                end
              else p1^.left:=nil;
              { do firstpass because we need the  }
              { result type                       }
              do_firstpass(p1);
              {set_var_state is handled inside firstcalln }
           end
        else
           begin
              { address operator @: }
              p1^.left:=nil;
              { forget pd }
              pd:=nil;
              if (p1^.symtableproc^.symtabletype=withsymtable) and
                (p1^.symtableproc^.defowner^.deftype=objectdef) then
                begin
                   p1^.methodpointer:=getcopy(pwithsymtable(p1^.symtableproc)^.withrefnode);
                end
              else if not(assigned(p1^.methodpointer)) then
                begin
                   { we must provide a method pointer, if it isn't given, }
                   { it is self                                           }
                   if assigned(procinfo) then
                    begin
                      p1^.methodpointer:=genselfnode(procinfo^._class);
                      p1^.methodpointer^.resulttype:=procinfo^._class;
                    end
                   else
                    begin
                      p1^.methodpointer:=genselfnode(nil);
                      p1^.methodpointer^.resulttype:=nil;
                    end;
                end;
              { no postfix operators }
              again:=false;
           end;
         pd:=p1^.resulttype;
         in_args:=prev_in_args;
         afterassignment:=prevafterassn;
      end;

    procedure handle_procvar(pv : pprocvardef;var p2 : ptree);

        procedure doconv(procvar : pprocvardef;var t : ptree);
        var
          hp : ptree;
        begin
          hp:=nil;
          if (proc_to_procvar_equal(pprocsym(t^.symtableentry)^.definition,procvar)) then
           begin
             if (po_methodpointer in procvar^.procoptions) then
               hp:=genloadmethodcallnode(pprocsym(t^.symtableprocentry),t^.symtable,getcopy(t^.methodpointer))
             else
               hp:=genloadcallnode(pprocsym(t^.symtableprocentry),t^.symtable);
           end;
          if assigned(hp) then
           begin
             disposetree(t);
             t:=hp;
           end;
        end;

      begin
        if (p2^.treetype=calln) then
         doconv(pv,p2)
        else
         if (p2^.treetype=typeconvn) and
            (p2^.left^.treetype=calln) then
          doconv(pv,p2^.left);
      end;


    { the following procedure handles the access to a property symbol }
    procedure handle_propertysym(sym : psym;st : psymtable;var p1 : ptree;
      var pd : pdef);

      var
         paras : ptree;
         p2 : ptree;
         plist : psymlistitem;

      begin
         paras:=nil;
         { property parameters? read them only if the property really }
         { has parameters                                             }
         if ppo_hasparameters in ppropertysym(sym)^.propoptions then
           begin
              if token=_LECKKLAMMER then
                begin
                   consume(_LECKKLAMMER);
                   paras:=parse_paras(false,true);
                   consume(_RECKKLAMMER);
                end;
              { indexed property }
              if (ppo_indexed in ppropertysym(sym)^.propoptions) then
                begin
                   p2:=genordinalconstnode(ppropertysym(sym)^.index,ppropertysym(sym)^.indextype.def);
                   paras:=gencallparanode(p2,paras);
                end;
           end;
         { we need only a write property if a := follows }
         { if not(afterassignment) and not(in_args) then }
         if token=_ASSIGNMENT then
           begin
              { write property: }
              { no result }
              pd:=voiddef;
              if not ppropertysym(sym)^.writeaccess^.empty then
                begin
                   case ppropertysym(sym)^.writeaccess^.firstsym^.sym^.typ of
                     procsym :
                       begin
                         { generate the method call }
                         p1:=genmethodcallnode(pprocsym(ppropertysym(sym)^.writeaccess^.firstsym^.sym),st,p1);
                         { we know the procedure to call, so
                           force the usage of that procedure }
                         p1^.procdefinition:=pprocdef(ppropertysym(sym)^.writeaccess^.def);
                         p1^.left:=paras;
                         consume(_ASSIGNMENT);
                         { read the expression }
                         getprocvar:=ppropertysym(sym)^.proptype.def^.deftype=procvardef;
                         p2:=comp_expr(true);
                         if getprocvar then
                           handle_procvar(pprocvardef(ppropertysym(sym)^.proptype.def),p2);
                         p1^.left:=gencallparanode(p2,p1^.left);
                         p1^.isproperty:=true;
                         getprocvar:=false;
                       end;
                     varsym :
                       begin
                         if assigned(paras) then
                           message(parser_e_no_paras_allowed);
                         { subscribed access? }
                         plist:=ppropertysym(sym)^.writeaccess^.firstsym;
                         while assigned(plist) do
                          begin
                            if p1=nil then
                              p1:=genloadnode(pvarsym(plist^.sym),st)
                            else
                              p1:=gensubscriptnode(pvarsym(plist^.sym),p1);
                            plist:=plist^.next;
                          end;
                         p1^.isproperty:=true;
                         consume(_ASSIGNMENT);
                         { read the expression }
                         p2:=comp_expr(true);
                         p1:=gennode(assignn,p1,p2);
                      end
                    else
                      begin
                        p1:=genzeronode(errorn);
                        Message(parser_e_no_procedure_to_access_property);
                      end;
                  end;
                end
              else
                begin
                   p1:=genzeronode(errorn);
                   Message(parser_e_no_procedure_to_access_property);
                end;
           end
         else
           begin
              { read property: }
              pd:=ppropertysym(sym)^.proptype.def;
              if not ppropertysym(sym)^.readaccess^.empty then
                begin
                   case ppropertysym(sym)^.readaccess^.firstsym^.sym^.typ of
                     varsym :
                       begin
                          if assigned(paras) then
                            message(parser_e_no_paras_allowed);
                          { subscribed access? }
                          plist:=ppropertysym(sym)^.readaccess^.firstsym;
                          while assigned(plist) do
                           begin
                             if p1=nil then
                               p1:=genloadnode(pvarsym(plist^.sym),st)
                             else
                               p1:=gensubscriptnode(pvarsym(plist^.sym),p1);
                             plist:=plist^.next;
                           end;
                          p1^.isproperty:=true;
                       end;
                     procsym :
                       begin
                          { generate the method call }
                          p1:=genmethodcallnode(pprocsym(ppropertysym(sym)^.readaccess^.firstsym^.sym),st,p1);
                          { we know the procedure to call, so
                            force the usage of that procedure }
                          p1^.procdefinition:=pprocdef(ppropertysym(sym)^.readaccess^.def);
                          { insert paras }
                          p1^.left:=paras;
                          p1^.isproperty:=true;
                       end
                     else
                       begin
                          p1:=genzeronode(errorn);
                          Message(type_e_mismatch);
                       end;
                  end;
                end
              else
                begin
                   { error, no function to read property }
                   p1:=genzeronode(errorn);
                   Message(parser_e_no_procedure_to_access_property);
                end;
           end;
      end;


    { the ID token has to be consumed before calling this function }
    procedure do_member_read(getaddr : boolean;const sym : psym;var p1 : ptree;
      var pd : pdef;var again : boolean);

      var
         static_name : string;
         isclassref : boolean;

      begin
         if sym=nil then
           begin
              { pattern is still valid unless
              there is another ID just after the ID of sym }
              Message1(sym_e_id_no_member,pattern);
              disposetree(p1);
              p1:=genzeronode(errorn);
              { try to clean up }
              pd:=generrordef;
              again:=false;
           end
         else
           begin
              isclassref:=pd^.deftype=classrefdef;

              { check protected and private members        }
              { please leave this code as it is,           }
              { it has now the same behaviaor as TP/Delphi }
              if (sp_private in sym^.symoptions) and
                 (pobjectdef(pd)^.owner^.symtabletype=unitsymtable) then
               Message(parser_e_cant_access_private_member);

              if (sp_protected in sym^.symoptions) and
                 (pobjectdef(pd)^.owner^.symtabletype=unitsymtable) then
                begin
                  if assigned(aktprocsym^.definition^._class) then
                    begin
                       if not aktprocsym^.definition^._class^.is_related(pobjectdef(sym^.owner^.defowner)) then
                         Message(parser_e_cant_access_protected_member);
                    end
                  else
                    Message(parser_e_cant_access_protected_member);
                end;

              { we assume, that only procsyms and varsyms are in an object }
              { symbol table, for classes, properties are allowed         }
              case sym^.typ of
                 procsym:
                   begin
                      p1:=genmethodcallnode(pprocsym(sym),srsymtable,p1);
                      do_proc_call(getaddr or
                        (getprocvar and
                         ((block_type=bt_const) or
                          ((m_tp_procvar in aktmodeswitches) and
                           proc_to_procvar_equal(pprocsym(sym)^.definition,getprocvardef)
                          )
                         )
                        ),again,p1,pd);
                      if (block_type=bt_const) and
                         getprocvar then
                        handle_procvar(getprocvardef,p1);
                      { now we know the real method e.g. we can check for a class method }
                      if isclassref and
                         assigned(p1^.procdefinition) and
                         not(po_classmethod in p1^.procdefinition^.procoptions) and
                         not(p1^.procdefinition^.proctypeoption=potype_constructor) then
                        Message(parser_e_only_class_methods_via_class_ref);
                   end;
                 varsym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      if (sp_static in sym^.symoptions) then
                        begin
                           { static_name:=lower(srsymtable^.name^)+'_'+sym^.name;
                             this is wrong for static field in with symtable (PM) }
                           static_name:=lower(srsym^.owner^.name^)+'_'+sym^.name;
                           getsym(static_name,true);
                           disposetree(p1);
                           p1:=genloadnode(pvarsym(srsym),srsymtable);
                        end
                      else
                        p1:=gensubscriptnode(pvarsym(sym),p1);
                      pd:=pvarsym(sym)^.vartype.def;
                   end;
                 propertysym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      handle_propertysym(sym,srsymtable,p1,pd);
                   end;
                 else internalerror(16);
              end;
           end;
      end;


{****************************************************************************
                               Factor
****************************************************************************}

    function factor(getaddr : boolean) : ptree;
      var
         l      : longint;
         oldp1,
         p1,p2,p3 : ptree;
         code     : integer;
         pd,pd2   : pdef;
         possible_error,
         unit_specific,
         again    : boolean;
         sym      : pvarsym;
         classh   : pobjectdef;
         d      : bestreal;
         static_name : string;
         propsym  : ppropertysym;
         filepos  : tfileposinfo;

         {---------------------------------------------
                         Is_func_ret
         ---------------------------------------------}

        function is_func_ret(sym : psym) : boolean;
        var
           p : pprocinfo;
           storesymtablestack : psymtable;

        begin
          is_func_ret:=false;
          if not assigned(procinfo) or
             ((sym^.typ<>funcretsym) and ((procinfo^.flags and pi_operator)=0)) then
            exit;
          p:=procinfo;
          while assigned(p) do
            begin
               { is this an access to a function result? Accessing _RESULT is
                 always allowed and funcretn is generated }
               if assigned(p^.funcretsym) and
                  ((pfuncretsym(sym)=p^.resultfuncretsym) or
                   ((pfuncretsym(sym)=p^.funcretsym) or
                    ((pvarsym(sym)=opsym) and ((p^.flags and pi_operator)<>0))) and
                   (p^.returntype.def<>pdef(voiddef)) and
                   (token<>_LKLAMMER) and
                   (not ((m_tp in aktmodeswitches) and (afterassignment or in_args)))
                  ) then
                 begin
                    if ((pvarsym(sym)=opsym) and
                       ((p^.flags and pi_operator)<>0)) then
                       inc(opsym^.refs);
                    p1:=genzeronode(funcretn);
                    pd:=p^.returntype.def;
                    p1^.funcretprocinfo:=p;
                    p1^.rettype.def:=pd;
                    is_func_ret:=true;
                    if p^.funcret_state=vs_declared then
                      begin
                        p^.funcret_state:=vs_declared_and_first_found;
                        p1^.is_first_funcret:=true;
                      end;
                    exit;
                 end;
               p:=p^.parent;
            end;
          { we must use the function call }
          if (sym^.typ=funcretsym) then
            begin
               storesymtablestack:=symtablestack;
               symtablestack:=srsymtable^.next;
               getsym(sym^.name,true);
               if srsym^.typ<>procsym then
                 Message(cg_e_illegal_expression);
               symtablestack:=storesymtablestack;
            end;
        end;

         {---------------------------------------------
                         Factor_read_id
         ---------------------------------------------}

       procedure factor_read_id;
         var
           pc : pchar;
           len : longint;
         begin
           { allow post fix operators }
           again:=true;
            begin
              if lastsymknown then
               begin
                 srsym:=lastsrsym;
                 srsymtable:=lastsrsymtable;
                 lastsymknown:=false;
               end
              else
               getsym(pattern,true);
              consume(_ID);
               if not is_func_ret(srsym) then
              { else it's a normal symbol }
                begin
                { is it defined like UNIT.SYMBOL ? }
                  if srsym^.typ=unitsym then
                   begin
                     consume(_POINT);
                     getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                     unit_specific:=true;
                     consume(_ID);
                   end
                  else
                   unit_specific:=false;
                  if not assigned(srsym) then
                   Begin
                     p1:=genzeronode(errorn);
                     { try to clean up }
                     pd:=generrordef;
                   end
                  else
                   Begin
                     { check semantics of private }
                     if (srsym^.typ in [propertysym,procsym,varsym]) and
                        (srsymtable^.symtabletype=objectsymtable) then
                      begin
                         if (sp_private in srsym^.symoptions) and
                            (pobjectdef(srsym^.owner^.defowner)^.owner^.symtabletype=unitsymtable) then
                            Message(parser_e_cant_access_private_member);
                      end;
                     case srsym^.typ of
              absolutesym : begin
                              p1:=genloadnode(pvarsym(srsym),srsymtable);
                              pd:=pabsolutesym(srsym)^.vartype.def;
                            end;
                   varsym : begin
                              { are we in a class method ? }
                              if (srsymtable^.symtabletype=objectsymtable) and
                                 assigned(aktprocsym) and
                                 (po_classmethod in aktprocsym^.definition^.procoptions) then
                                Message(parser_e_only_class_methods);
                              if (sp_static in srsym^.symoptions) then
                               begin
                                 static_name:=lower(srsym^.owner^.name^)+'_'+srsym^.name;
                                 getsym(static_name,true);
                               end;
                              p1:=genloadnode(pvarsym(srsym),srsymtable);
                              if pvarsym(srsym)^.varstate=vs_declared then
                               begin
                                 p1^.is_first := true;
                                 { set special between first loaded until checked in firstpass }
                                 pvarsym(srsym)^.varstate:=vs_declared_and_first_found;
                               end;
                              pd:=pvarsym(srsym)^.vartype.def;
                            end;
            typedconstsym : begin
                              p1:=gentypedconstloadnode(ptypedconstsym(srsym),srsymtable);
                              pd:=ptypedconstsym(srsym)^.typedconsttype.def;
                            end;
                   syssym : p1:=statement_syssym(psyssym(srsym)^.number,pd);
                  typesym : begin
                              pd:=ptypesym(srsym)^.restype.def;
                              if not assigned(pd) then
                               begin
                                 pd:=generrordef;
                                 again:=false;
                               end
                              else
                               begin
                                 { if we read a type declaration  }
                                 { we have to return the type and }
                                 { nothing else               }
                                  if block_type=bt_type then
                                   begin
                                     { we don't need sym reference when it's in the
                                       current unit or system unit, because those
                                       units are always loaded (PFV) }
                                     if not(assigned(pd^.owner)) or
                                        (pd^.owner^.unitid=0) or
                                        (pd^.owner^.unitid=1) then
                                      p1:=gentypenode(pd,nil)
                                     else
                                      p1:=gentypenode(pd,ptypesym(srsym));
                                     { here we can also set resulttype !! }
                                     p1^.resulttype:=pd;
                                     pd:=voiddef;
                                   end
                                 else { not type block }
                                  begin
                                    if token=_LKLAMMER then
                                     begin
                                       consume(_LKLAMMER);
                                       p1:=comp_expr(true);
                                       consume(_RKLAMMER);
                                       p1:=gentypeconvnode(p1,pd);
                                       p1^.explizit:=true;
                                     end
                                    else { not LKLAMMER}
                                     if (token=_POINT) and
                                        (pd^.deftype=objectdef) and
                                        not(pobjectdef(pd)^.is_class) then
                                       begin
                                         consume(_POINT);
                                         if assigned(procinfo) and
                                            assigned(procinfo^._class) and
                                            not(getaddr) then
                                          begin
                                            if procinfo^._class^.is_related(pobjectdef(pd)) then
                                             begin
                                               p1:=gentypenode(pd,ptypesym(srsym));
                                               p1^.resulttype:=pd;
                                               { search also in inherited methods }
                                               repeat
                                                 srsymtable:=pobjectdef(pd)^.symtable;
                                                 sym:=pvarsym(srsymtable^.search(pattern));
                                                 if assigned(sym) then
                                                  break;
                                                 pd:=pobjectdef(pd)^.childof;
                                               until not assigned(pd);
                                               consume(_ID);
                                               do_member_read(false,sym,p1,pd,again);
                                             end
                                            else
                                             begin
                                               Message(parser_e_no_super_class);
                                               pd:=generrordef;
                                               again:=false;
                                             end;
                                          end
                                         else
                                          begin
                                            { allows @TObject.Load }
                                            { also allows static methods and variables }
                                            p1:=genzeronode(typen);
                                            p1^.resulttype:=pd;
                                            { TP allows also @TMenu.Load if Load is only }
                                            { defined in an anchestor class              }
                                            sym:=pvarsym(search_class_member(pobjectdef(pd),pattern));
                                            if not assigned(sym) then
                                              Message1(sym_e_id_no_member,pattern)
                                            else if not(getaddr) and not(sp_static in sym^.symoptions) then
                                              Message(sym_e_only_static_in_static)
                                            else
                                             begin
                                               consume(_ID);
                                               do_member_read(getaddr,sym,p1,pd,again);
                                             end;
                                          end;
                                       end
                                     else
                                       begin
                                          { class reference ? }
                                          if (pd^.deftype=objectdef)
                                            and pobjectdef(pd)^.is_class then
                                            begin
                                               p1:=gentypenode(pd,nil);
                                               p1^.resulttype:=pd;
                                               pd:=new(pclassrefdef,init(pd));
                                               p1:=gensinglenode(loadvmtn,p1);
                                               p1^.resulttype:=pd;
                                            end
                                          else
                                            begin
                                               { generate a type node }
                                               { (for typeof etc)     }
                                               if allow_type then
                                                 begin
                                                    p1:=gentypenode(pd,nil);
                                                    { here we must use typenodetype explicitly !! PM
                                                    p1^.resulttype:=pd; }
                                                    pd:=voiddef;
                                                 end
                                               else
                                                 Message(parser_e_no_type_not_allowed_here);
                                            end;
                                       end;
                                  end;
                               end;
                            end;
                  enumsym : begin
                              p1:=genenumnode(penumsym(srsym));
                              pd:=p1^.resulttype;
                            end;
                 constsym : begin
                              case pconstsym(srsym)^.consttyp of
                                constint :
                                  p1:=genordinalconstnode(pconstsym(srsym)^.value,s32bitdef);
                                conststring :
                                  begin
                                    len:=pconstsym(srsym)^.len;
                                    if not(cs_ansistrings in aktlocalswitches) and (len>255) then
                                     len:=255;
                                    getmem(pc,len+1);
                                    move(pchar(pconstsym(srsym)^.value)^,pc^,len);
                                    pc[len]:=#0;
                                    p1:=genpcharconstnode(pc,len);
                                  end;
                                constchar :
                                  p1:=genordinalconstnode(pconstsym(srsym)^.value,cchardef);
                                constreal :
                                  p1:=genrealconstnode(pbestreal(pconstsym(srsym)^.value)^,bestrealdef^);
                                constbool :
                                  p1:=genordinalconstnode(pconstsym(srsym)^.value,booldef);
                                constset :
                                  p1:=gensetconstnode(pconstset(pconstsym(srsym)^.value),
                                        psetdef(pconstsym(srsym)^.consttype.def));
                                constord :
                                  p1:=genordinalconstnode(pconstsym(srsym)^.value,
                                        pconstsym(srsym)^.consttype.def);
                                constpointer :
                                  p1:=genpointerconstnode(pconstsym(srsym)^.value,
                                        pconstsym(srsym)^.consttype.def);
                                constnil :
                                  p1:=genzeronode(niln);
                                constresourcestring:
                                  begin
                                     p1:=genloadnode(pvarsym(srsym),srsymtable);
                                     p1^.resulttype:=cansistringdef;
                                  end;
                              end;
                              pd:=p1^.resulttype;
                            end;
                  procsym : begin
                              { are we in a class method ? }
                              possible_error:=(srsymtable^.symtabletype=objectsymtable) and
                                              assigned(aktprocsym) and
                                              (po_classmethod in aktprocsym^.definition^.procoptions);
                              p1:=gencallnode(pprocsym(srsym),srsymtable);
                              p1^.unit_specific:=unit_specific;
                              do_proc_call(getaddr or
                                (getprocvar and
                                 ((block_type=bt_const) or
                                  ((m_tp_procvar in aktmodeswitches) and
                                   proc_to_procvar_equal(pprocsym(srsym)^.definition,getprocvardef)
                                  )
                                 )
                                ),again,p1,pd);
                              if (block_type=bt_const) and
                                 getprocvar then
                                handle_procvar(getprocvardef,p1);
                              if possible_error and
                                 not(po_classmethod in p1^.procdefinition^.procoptions) then
                                Message(parser_e_only_class_methods);
                            end;
              propertysym : begin
                              { access to property in a method }
                              { are we in a class method ? }
                              if (srsymtable^.symtabletype=objectsymtable) and
                                 assigned(aktprocsym) and
                                 (po_classmethod in aktprocsym^.definition^.procoptions) then
                               Message(parser_e_only_class_methods);
                              { no method pointer }
                              p1:=nil;
                              handle_propertysym(srsym,srsymtable,p1,pd);
                            end;
                 errorsym : begin
                              p1:=genzeronode(errorn);
                              p1^.resulttype:=generrordef;
                              pd:=generrordef;
                              if token=_LKLAMMER then
                               begin
                                 consume(_LKLAMMER);
                                 parse_paras(false,false);
                                 consume(_RKLAMMER);
                               end;
                            end;
                     else
                       begin
                         p1:=genzeronode(errorn);
                         pd:=generrordef;
                         Message(cg_e_illegal_expression);
                       end;
                     end; { end case }
                   end;
                end;
            end;
         end;

         {---------------------------------------------
                         Factor_Read_Set
         ---------------------------------------------}

         { Read a set between [] }
         function factor_read_set:ptree;
         var
           p1,
           lastp,
           buildp : ptree;
         begin
           buildp:=nil;
         { be sure that a least one arrayconstructn is used, also for an
           empty [] }
           if token=_RECKKLAMMER then
            buildp:=gennode(arrayconstructn,nil,buildp)
           else
            begin
              while true do
               begin
                 p1:=comp_expr(true);
                 if token=_POINTPOINT then
                  begin
                    consume(_POINTPOINT);
                    p2:=comp_expr(true);
                    p1:=gennode(arrayconstructrangen,p1,p2);
                  end;
               { insert at the end of the tree, to get the correct order }
                 if not assigned(buildp) then
                  begin
                    buildp:=gennode(arrayconstructn,p1,nil);
                    lastp:=buildp;
                  end
                 else
                  begin
                    lastp^.right:=gennode(arrayconstructn,p1,nil);
                    lastp:=lastp^.right;
                  end;
               { there could be more elements }
                 if token=_COMMA then
                   consume(_COMMA)
                 else
                   break;
               end;
            end;
           factor_read_set:=buildp;
         end;

         {---------------------------------------------
                           Helpers
         ---------------------------------------------}

        procedure check_tokenpos;
        begin
          if (p1<>oldp1) then
           begin
             if assigned(p1) then
              set_tree_filepos(p1,filepos);
             oldp1:=p1;
             filepos:=tokenpos;
           end;
        end;



         {---------------------------------------------
                        PostFixOperators
         ---------------------------------------------}

      procedure postfixoperators;
        var
           store_static : boolean;

        { p1 and p2 must contain valid value_str }
        begin
          check_tokenpos;
          while again do
           begin
             { prevent crashes with unknown types }
             if not assigned(pd) then
              begin
                { try to recover }
                repeat
                  case token of
                  _CARET:
                     consume(_CARET);

                  _POINT:
                     begin
                        consume(_POINT);
                        consume(_ID);
                     end;

                  _LECKKLAMMER:
                     begin
                        repeat
                          consume(token);
                          until token in [_RECKKLAMMER,_SEMICOLON];
                     end;
                  else
                    break;
                  end;
                until false;
                exit;
              end;
           { handle token }
             case token of
               _CARET:
                  begin
                    consume(_CARET);
                    if (pd^.deftype<>pointerdef) then
                      begin
                         { ^ as binary operator is a problem!!!! (FK) }
                         again:=false;
                         Message(cg_e_invalid_qualifier);
                         disposetree(p1);
                         p1:=genzeronode(errorn);
                      end
                    else
                      begin
                         p1:=gensinglenode(derefn,p1);
                         pd:=ppointerdef(pd)^.pointertype.def;
                      end;
                  end;

               _LECKKLAMMER:
                  begin
                    if (pd^.deftype=objectdef) and pobjectdef(pd)^.is_class then
                      begin
                        { default property }
                        propsym:=search_default_property(pobjectdef(pd));
                        if not(assigned(propsym)) then
                          begin
                             disposetree(p1);
                             p1:=genzeronode(errorn);
                             again:=false;
                             message(parser_e_no_default_property_available);
                          end
                        else
                          handle_propertysym(propsym,propsym^.owner,p1,pd);
                      end
                    else
                      begin
                        consume(_LECKKLAMMER);
                        repeat
                          case pd^.deftype of
                            pointerdef:
                                begin
                                   p2:=comp_expr(true);
                                   p1:=gennode(vecn,p1,p2);
                                   pd:=ppointerdef(pd)^.pointertype.def;
                                 end;

                     stringdef : begin
                                   p2:=comp_expr(true);
                                   p1:=gennode(vecn,p1,p2);
                                   pd:=cchardef
                                 end;
                      arraydef : begin
                                   p2:=comp_expr(true);
                                 { support SEG:OFS for go32v2 Mem[] }
                                   if (target_info.target=target_i386_go32v2) and
                                      (p1^.treetype=loadn) and
                                      assigned(p1^.symtableentry) and
                                      assigned(p1^.symtableentry^.owner^.name) and
                                      (p1^.symtableentry^.owner^.name^='SYSTEM') and
                                      ((p1^.symtableentry^.name='MEM') or
                                       (p1^.symtableentry^.name='MEMW') or
                                       (p1^.symtableentry^.name='MEML')) then
                                     begin
                                       if (token=_COLON) then
                                        begin
                                          consume(_COLON);
                                          p3:=gennode(muln,genordinalconstnode($10,s32bitdef),p2);
                                          p2:=comp_expr(true);
                                          p2:=gennode(addn,p2,p3);
                                          p1:=gennode(vecn,p1,p2);
                                          p1^.memseg:=true;
                                          p1^.memindex:=true;
                                        end
                                       else
                                        begin
                                          p1:=gennode(vecn,p1,p2);
                                          p1^.memindex:=true;
                                        end;
                                     end
                                   else
                                     p1:=gennode(vecn,p1,p2);
                                   pd:=parraydef(pd)^.elementtype.def;
                                 end;
                          else
                            begin
                              Message(cg_e_invalid_qualifier);
                              disposetree(p1);
                              p1:=genzeronode(errorn);
                              again:=false;
                            end;
                          end;
                          if token=_COMMA then
                            consume(_COMMA)
                          else
                            break;
                        until false;
                        consume(_RECKKLAMMER);
                      end;
                  end;
         _POINT : begin
                    consume(_POINT);
                    if (pd^.deftype=pointerdef) and
                      (m_autoderef in aktmodeswitches) then
                      begin
                         p1:=gensinglenode(derefn,p1);
                         pd:=ppointerdef(pd)^.pointertype.def;
                      end;
                    case pd^.deftype of
                       recorddef:
                         begin
                            sym:=pvarsym(precorddef(pd)^.symtable^.search(pattern));
                            if sym=nil then
                              begin
                                Message1(sym_e_illegal_field,pattern);
                                disposetree(p1);
                                p1:=genzeronode(errorn);
                              end
                            else
                              begin
                                p1:=gensubscriptnode(sym,p1);
                                pd:=sym^.vartype.def;
                              end;
                            consume(_ID);
                          end;

                        classrefdef:
                          begin
                             classh:=pobjectdef(pclassrefdef(pd)^.pointertype.def);
                             sym:=nil;
                             while assigned(classh) do
                              begin
                                sym:=pvarsym(classh^.symtable^.search(pattern));
                                srsymtable:=classh^.symtable;
                                if assigned(sym) then
                                 break;
                                classh:=classh^.childof;
                              end;
                             consume(_ID);
                             do_member_read(getaddr,sym,p1,pd,again);
                           end;

                         objectdef:
                           begin
                             classh:=pobjectdef(pd);
                             sym:=nil;
                             store_static:=allow_only_static;
                             allow_only_static:=false;
                             while assigned(classh) do
                              begin
                                sym:=pvarsym(classh^.symtable^.search(pattern));
                                srsymtable:=classh^.symtable;
                                if assigned(sym) then
                                 break;
                                classh:=classh^.childof;
                              end;
                             allow_only_static:=store_static;
                             consume(_ID);
                             do_member_read(getaddr,sym,p1,pd,again);
                           end;

                         pointerdef:
                           begin
                             Message(cg_e_invalid_qualifier);
                             if ppointerdef(pd)^.pointertype.def^.deftype in [recorddef,objectdef,classrefdef] then
                              Message(parser_h_maybe_deref_caret_missing);
                           end;
                    else
                      begin
                        Message(cg_e_invalid_qualifier);
                        disposetree(p1);
                        p1:=genzeronode(errorn);
                      end;
                    end;
                  end;
             else
               begin
               { is this a procedure variable ? }
                 if assigned(pd) then
                  begin
                    if (pd^.deftype=procvardef) then
                     begin
                       if getprocvar and is_equal(pd,getprocvardef) then
                         again:=false
                       else
                         if (token=_LKLAMMER) or
                            ((pprocvardef(pd)^.para^.empty) and
                             (not((token in [_ASSIGNMENT,_UNEQUAL,_EQUAL]))) and
                             (not afterassignment) and
                             (not in_args)) then
                           begin
                              { do this in a strange way  }
                              { it's not a clean solution }
                              p2:=p1;
                              p1:=gencallnode(nil,nil);
                              p1^.right:=p2;
                              p1^.unit_specific:=unit_specific;
                              p1^.symtableprocentry:=pprocsym(sym);
                              if token=_LKLAMMER then
                                begin
                                   consume(_LKLAMMER);
                                   p1^.left:=parse_paras(false,false);
                                   consume(_RKLAMMER);
                                end;
                              pd:=pprocvardef(pd)^.rettype.def;
                           { proc():= is never possible }
                              if token=_ASSIGNMENT then
                               begin
                                 Message(cg_e_illegal_expression);
                                 p1:=genzeronode(errorn);
                                 again:=false;
                               end;
                              p1^.resulttype:=pd;
                           end
                       else
                         again:=false;
                       p1^.resulttype:=pd;
                     end
                    else
                     again:=false;
                  end
                 else
                  again:=false;
                end;
             end;
             check_tokenpos;
           end; { while again }
        end;


      {---------------------------------------------
                      Factor (Main)
      ---------------------------------------------}

      begin
        oldp1:=nil;
        p1:=nil;
        filepos:=tokenpos;
        if token=_ID then
         begin
           factor_read_id;
           { handle post fix operators }
           postfixoperators;
         end
        else
         case token of
        _NEW : begin
                 consume(_NEW);
                 consume(_LKLAMMER);
                 {allow_type:=true;}
                 p1:=factor(false);
                 {allow_type:=false;}
                 if p1^.treetype<>typen then
                  begin
                    Message(type_e_type_id_expected);
                    disposetree(p1);
                    pd:=generrordef;
                  end
                 else
                  pd:=p1^.typenodetype;
                 pd2:=pd;

                 if (pd^.deftype<>pointerdef) then
                   Message1(type_e_pointer_type_expected,pd^.typename)
                 else
                  if token=_RKLAMMER then
                   begin
                     if (ppointerdef(pd)^.pointertype.def^.deftype=objectdef) and
                        (oo_has_vmt in pobjectdef(ppointerdef(pd)^.pointertype.def)^.objectoptions)  then
                      Message(parser_w_use_extended_syntax_for_objects);
                     p1:=gensinglenode(newn,nil);
                     p1^.resulttype:=pd2;
                     consume(_RKLAMMER);
                   end
                 else
                   begin
                     disposetree(p1);
                     p1:=genzeronode(hnewn);
                     p1^.resulttype:=ppointerdef(pd)^.pointertype.def;
                     consume(_COMMA);
                     afterassignment:=false;
                     { determines the current object defintion }
                     classh:=pobjectdef(ppointerdef(pd)^.pointertype.def);
                     if classh^.deftype<>objectdef then
                      Message(parser_e_pointer_to_class_expected)
                     else
                      begin
                        { check for an abstract class }
                        if (oo_has_abstract in classh^.objectoptions) then
                         Message(sym_e_no_instance_of_abstract_object);
                        { search the constructor also in the symbol tables of
                          the parents }
                        sym:=nil;
                        while assigned(classh) do
                         begin
                           sym:=pvarsym(classh^.symtable^.search(pattern));
                           srsymtable:=classh^.symtable;
                           if assigned(sym) then
                            break;
                           classh:=classh^.childof;
                         end;
                        consume(_ID);
                        do_member_read(false,sym,p1,pd,again);
                        if (p1^.treetype<>calln) or
                           (assigned(p1^.procdefinition) and
                           (p1^.procdefinition^.proctypeoption<>potype_constructor)) then
                         Message(parser_e_expr_have_to_be_constructor_call);
                      end;
                     p1:=gensinglenode(newn,p1);
                     { set the resulttype }
                     p1^.resulttype:=pd2;
                     consume(_RKLAMMER);
                   end;
                 postfixoperators;
               end;
       _SELF : begin
                 again:=true;
                 consume(_SELF);
                 if not assigned(procinfo^._class) then
                  begin
                    p1:=genzeronode(errorn);
                    pd:=generrordef;
                    again:=false;
                    Message(parser_e_self_not_in_method);
                  end
                 else
                  begin
                    if (po_classmethod in aktprocsym^.definition^.procoptions) then
                     begin
                       { self in class methods is a class reference type }
                       pd:=new(pclassrefdef,init(procinfo^._class));
                       p1:=genselfnode(pd);
                       p1^.resulttype:=pd;
                     end
                    else
                     begin
                       p1:=genselfnode(procinfo^._class);
                       p1^.resulttype:=procinfo^._class;
                     end;
                    pd:=p1^.resulttype;
                    postfixoperators;
                  end;
               end;
  _INHERITED : begin
                 again:=true;
                 consume(_INHERITED);
                 if assigned(procinfo^._class) then
                  begin
                    classh:=procinfo^._class^.childof;
                    while assigned(classh) do
                     begin
                       srsymtable:=pobjectdef(classh)^.symtable;
                       sym:=pvarsym(srsymtable^.search(pattern));
                       if assigned(sym) then
                        begin
                          { only for procsyms we need to set the type (PFV) }
                          if sym^.typ=procsym then
                           begin
                             p1:=genzeronode(typen);
                             p1^.resulttype:=classh;
                             pd:=p1^.resulttype;
                           end
                          else
                           p1:=nil;
                          consume(_ID);
                          do_member_read(false,sym,p1,pd,again);
                          break;
                        end;
                       classh:=classh^.childof;
                     end;
                    if classh=nil then
                     begin
                       Message1(sym_e_id_no_member,pattern);
                       again:=false;
                       pd:=generrordef;
                       p1:=genzeronode(errorn);
                     end;
                  end
                 else
                   begin
                      Message(parser_e_generic_methods_only_in_methods);
                      again:=false;
                      pd:=generrordef;
                      p1:=genzeronode(errorn);
                   end;
                 postfixoperators;
               end;
   _INTCONST : begin
                 valint(pattern,l,code);
                 if code<>0 then
                  begin
                    val(pattern,d,code);
                    if code<>0 then
                     begin
                       Message(cg_e_invalid_integer);
                       consume(_INTCONST);
                       l:=1;
                       p1:=genordinalconstnode(l,s32bitdef);
                     end
                    else
                     begin
                       consume(_INTCONST);
                       p1:=genrealconstnode(d,bestrealdef^);
                     end;
                  end
                 else
                  begin
                    consume(_INTCONST);
                    p1:=genordinalconstnode(l,s32bitdef);
                  end;
               end;
 _REALNUMBER : begin
                 val(pattern,d,code);
                 if code<>0 then
                  begin
                    Message(parser_e_error_in_real);
                    d:=1.0;
                  end;
                 consume(_REALNUMBER);
                 p1:=genrealconstnode(d,bestrealdef^);
               end;
     _STRING : begin
                 pd:=string_dec;
                 { STRING can be also a type cast }
                 if token=_LKLAMMER then
                  begin
                    consume(_LKLAMMER);
                    p1:=comp_expr(true);
                    consume(_RKLAMMER);
                    p1:=gentypeconvnode(p1,pd);
                    p1^.explizit:=true;
                    { handle postfix operators here e.g. string(a)[10] }
                    again:=true;
                    postfixoperators;
                  end
                 else
                  p1:=gentypenode(pd,nil);
               end;
       _FILE : begin
                 pd:=cfiledef;
                 consume(_FILE);
                 { FILE can be also a type cast }
                 if token=_LKLAMMER then
                  begin
                    consume(_LKLAMMER);
                    p1:=comp_expr(true);
                    consume(_RKLAMMER);
                    p1:=gentypeconvnode(p1,pd);
                    p1^.explizit:=true;
                    { handle postfix operators here e.g. string(a)[10] }
                    again:=true;
                    postfixoperators;
                  end
                 else
                  p1:=gentypenode(pd,nil);
               end;
    _CSTRING : begin
                 p1:=genstringconstnode(pattern);
                 consume(_CSTRING);
               end;
      _CCHAR : begin
                 p1:=genordinalconstnode(ord(pattern[1]),cchardef);
                 consume(_CCHAR);
               end;
_KLAMMERAFFE : begin
                 consume(_KLAMMERAFFE);
                 got_addrn:=true;
                 { support both @<x> and @(<x>) }
                 if token=_LKLAMMER then
                  begin
                    consume(_LKLAMMER);
                    p1:=factor(true);
                    consume(_RKLAMMER);
                    if token in [_CARET,_POINT,_LECKKLAMMER] then
                     begin
                       { we need the resulttype  }
                       { of the expression in pd }
                       do_firstpass(p1);
                       pd:=p1^.resulttype;
                       again:=true;
                       postfixoperators;
                     end;
                  end
                 else
                  p1:=factor(true);
                 got_addrn:=false;
                 p1:=gensinglenode(addrn,p1);
               end;
   _LKLAMMER : begin
                 consume(_LKLAMMER);
                 p1:=comp_expr(true);
                 consume(_RKLAMMER);
                 { it's not a good solution     }
                 { but (a+b)^ makes some problems  }
                 if token in [_CARET,_POINT,_LECKKLAMMER] then
                  begin
                    { we need the resulttype  }
                    { of the expression in pd }
                    do_firstpass(p1);
                    pd:=p1^.resulttype;
                    again:=true;
                    postfixoperators;
                  end;
               end;
_LECKKLAMMER : begin
                 consume(_LECKKLAMMER);
                 p1:=factor_read_set;
                 consume(_RECKKLAMMER);
               end;
       _PLUS : begin
                 consume(_PLUS);
                 p1:=factor(false);
               end;
      _MINUS : begin
                 consume(_MINUS);
                 p1:=factor(false);
                 p1:=gensinglenode(unaryminusn,p1);
               end;
        _NOT : begin
                 consume(_NOT);
                 p1:=factor(false);
                 p1:=gensinglenode(notn,p1);
               end;
       _TRUE : begin
                 consume(_TRUE);
                 p1:=genordinalconstnode(1,booldef);
               end;
      _FALSE : begin
                 consume(_FALSE);
                 p1:=genordinalconstnode(0,booldef);
               end;
        _NIL : begin
                 consume(_NIL);
                 p1:=genzeronode(niln);
               end;
        else
          begin
            p1:=genzeronode(errorn);
            consume(token);
            Message(cg_e_illegal_expression);
          end;
        end;
        { generate error node if no node is created }
        if not assigned(p1) then
          p1:=genzeronode(errorn);
        { tp7 procvar handling, but not if the next token
          will be a := }
        if (m_tp_procvar in aktmodeswitches) and
           (token<>_ASSIGNMENT) then
          check_tp_procvar(p1);
        factor:=p1;
        check_tokenpos;
      end;


{****************************************************************************
                             Sub_Expr
****************************************************************************}

    type
      Toperator_precedence=(opcompare,opaddition,opmultiply);
      Ttok2nodeRec=record
        tok : ttoken;
        nod : ttreetyp;
      end;

    const
      tok2nodes=23;
      tok2node:array[1..tok2nodes] of ttok2noderec=(
        (tok:_PLUS    ;nod:addn),
        (tok:_MINUS   ;nod:subn),
        (tok:_STAR    ;nod:muln),
        (tok:_SLASH   ;nod:slashn),
        (tok:_EQUAL   ;nod:equaln),
        (tok:_GT      ;nod:gtn),
        (tok:_LT      ;nod:ltn),
        (tok:_GTE     ;nod:gten),
        (tok:_LTE     ;nod:lten),
        (tok:_SYMDIF  ;nod:symdifn),
        (tok:_STARSTAR;nod:starstarn),
        (tok:_OP_AS     ;nod:asn),
        (tok:_OP_IN     ;nod:inn),
        (tok:_OP_IS     ;nod:isn),
        (tok:_OP_OR     ;nod:orn),
        (tok:_OP_AND    ;nod:andn),
        (tok:_OP_DIV    ;nod:divn),
        (tok:_OP_MOD    ;nod:modn),
        (tok:_OP_SHL    ;nod:shln),
        (tok:_OP_SHR    ;nod:shrn),
        (tok:_OP_XOR    ;nod:xorn),
        (tok:_CARET   ;nod:caretn),
        (tok:_UNEQUAL ;nod:unequaln)
      );
      { Warning these stay be ordered !! }
      operator_levels:array[Toperator_precedence] of set of Ttoken=
         ([_LT,_LTE,_GT,_GTE,_EQUAL,_UNEQUAL,_OP_IN,_OP_IS],
          [_PLUS,_MINUS,_OP_OR,_OP_XOR],
          [_CARET,_SYMDIF,_STARSTAR,_STAR,_SLASH,
           _OP_AS,_OP_AND,_OP_DIV,_OP_MOD,_OP_SHL,_OP_SHR]);

    function sub_expr(pred_level:Toperator_precedence;accept_equal : boolean):Ptree;
    {Reads a subexpression while the operators are of the current precedence
     level, or any higher level. Replaces the old term, simpl_expr and
     simpl2_expr.}
      var
        low,high,mid : longint;
        p1,p2   : Ptree;
        oldt    : Ttoken;
        filepos : tfileposinfo;
      begin
        if pred_level=opmultiply then
          p1:=factor(false)
        else
          p1:=sub_expr(succ(pred_level),true);
        repeat
          if (token in operator_levels[pred_level]) and
             ((token<>_EQUAL) or accept_equal) then
           begin
             oldt:=token;
             filepos:=tokenpos;
             consume(token);
             if pred_level=opmultiply then
               p2:=factor(false)
             else
               p2:=sub_expr(succ(pred_level),true);
             low:=1;
             high:=tok2nodes;
             while (low<high) do
              begin
                mid:=(low+high+1) shr 1;
                if oldt<tok2node[mid].tok then
                 high:=mid-1
                else
                 low:=mid;
              end;
             if tok2node[high].tok=oldt then
              p1:=gennode(tok2node[high].nod,p1,p2)
             else
              p1:=gennode(nothingn,p1,p2);
             set_tree_filepos(p1,filepos);
           end
          else
           break;
        until false;
        sub_expr:=p1;
      end;


    function comp_expr(accept_equal : boolean):Ptree;
      var
         oldafterassignment : boolean;
         p1 : ptree;
      begin
         oldafterassignment:=afterassignment;
         afterassignment:=true;
         p1:=sub_expr(opcompare,accept_equal);
         afterassignment:=oldafterassignment;
         comp_expr:=p1;
      end;

    function expr : ptree;

      var
         p1,p2 : ptree;
         oldafterassignment : boolean;
         oldp1 : ptree;
         filepos : tfileposinfo;

      begin
         oldafterassignment:=afterassignment;
         p1:=sub_expr(opcompare,true);
         filepos:=tokenpos;
         if (m_tp_procvar in aktmodeswitches) and
            (token<>_ASSIGNMENT) then
           check_tp_procvar(p1);
         if token in [_ASSIGNMENT,_PLUSASN,_MINUSASN,_STARASN,_SLASHASN] then
           afterassignment:=true;
         oldp1:=p1;
         case token of
           _POINTPOINT : begin
                            consume(_POINTPOINT);
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(rangen,p1,p2);
                         end;
           _ASSIGNMENT : begin
                            consume(_ASSIGNMENT);
                            { avoid a firstpass of a procedure if
                            it must be assigned to a procvar }
                            { should be recursive for a:=b:=c !!! }
                            if (p1^.resulttype<>nil) and (p1^.resulttype^.deftype=procvardef) then
                              begin
                                 getprocvar:=true;
                                 getprocvardef:=pprocvardef(p1^.resulttype);
                              end;
                            p2:=sub_expr(opcompare,true);
                            if getprocvar then
                              handle_procvar(getprocvardef,p2);
                            getprocvar:=false;
                            p1:=gennode(assignn,p1,p2);
                         end;
                         { this is the code for C like assignements }
                         { from an improvement of Peter Schaefer    }
            _PLUSASN   : begin
                            consume(_PLUSASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(addn,getcopy(p1),p2));
                            { was first
                              p1:=gennode(assignn,p1,gennode(addn,p1,p2));
                              but disposetree assumes that we have a real
                              *** tree *** }
                         end;

            _MINUSASN   : begin
                            consume(_MINUSASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(subn,getcopy(p1),p2));
                         end;
            _STARASN   : begin
                            consume(_STARASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(muln,getcopy(p1),p2));
                         end;
            _SLASHASN   : begin
                            consume(_SLASHASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(slashn,getcopy(p1),p2));
                         end;
         end;
         afterassignment:=oldafterassignment;
         if p1<>oldp1 then
           set_tree_filepos(p1,filepos);
         expr:=p1;
      end;


    function get_intconst:longint;
    {Reads an expression, tries to evalute it and check if it is an integer
     constant. Then the constant is returned.}
    var
      p:Ptree;
    begin
      p:=comp_expr(true);
      do_firstpass(p);
      if not codegenerror then
       begin
         if (p^.treetype<>ordconstn) and
            (p^.resulttype^.deftype=orddef) and
           not(Porddef(p^.resulttype)^.typ in [uvoid,uchar,bool8bit,bool16bit,bool32bit]) then
          Message(cg_e_illegal_expression)
         else
          get_intconst:=p^.value;
       end;
      disposetree(p);
    end;


    function get_stringconst:string;
    {Reads an expression, tries to evaluate it and checks if it is a string
     constant. Then the constant is returned.}
    var
      p:Ptree;
    begin
      get_stringconst:='';
      p:=comp_expr(true);
      do_firstpass(p);
      if p^.treetype<>stringconstn then
        begin
          if (p^.treetype=ordconstn) and is_char(p^.resulttype) then
            get_stringconst:=char(p^.value)
          else
            Message(cg_e_illegal_expression);
        end
      else
        get_stringconst:=strpas(p^.value_str);
      disposetree(p);
    end;

end.
{
  $Log$
  Revision 1.164  1999-12-20 21:24:29  pierre
   * web bug769 fix

  Revision 1.163  1999/12/01 12:42:32  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.162  1999/11/30 10:40:44  peter
    + ttype, tsymlist

  Revision 1.161  1999/11/18 15:34:47  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.160  1999/11/17 17:05:01  pierre
   * Notes/hints changes

  Revision 1.159  1999/11/15 17:52:59  pierre
    + one field added for ttoken record for operator
      linking the id to the corresponding operator token that
      can now now all be overloaded
    * overloaded operators are resetted to nil in InitSymtable
      (bug when trying to compile a uint that overloads operators twice)

  Revision 1.158  1999/11/14 15:57:35  peter
    * fixed crash with an errordef

  Revision 1.157  1999/11/08 14:02:16  florian
    * problem with "index X"-properties solved
    * typed constants of class references are now allowed

  Revision 1.156  1999/11/07 23:21:30  florian
    * previous fix for 517 was imcomplete: there was a problem if the property
      had only an index

  Revision 1.155  1999/11/07 23:16:49  florian
    * finally bug 517 solved ...

  Revision 1.154  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.153  1999/11/05 00:10:30  peter
    * fixed inherited with properties

  Revision 1.152  1999/10/27 16:06:19  peter
    * check for object in extended new

  Revision 1.151  1999/10/26 12:30:44  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.150  1999/10/22 14:37:30  peter
    * error when properties are passed to var parameters

  Revision 1.149  1999/10/22 10:39:34  peter
    * split type reading from pdecl to ptype unit
    * parameter_dec routine is now used for procedure and procvars

  Revision 1.148  1999/10/14 14:57:52  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.147  1999/09/28 11:03:54  peter
    * fixed result access in 'if result = XXX then'
    * fixed const cr=chr(13)

  Revision 1.146  1999/09/27 23:44:54  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.145  1999/09/27 11:59:42  peter
    * fix for pointer reading in const with @type.method

  Revision 1.144  1999/09/26 21:30:19  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.143  1999/09/15 20:35:41  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.142  1999/09/13 16:26:32  peter
    * fix crash with empty object as childs

  Revision 1.141  1999/09/11 19:47:26  florian
    * bug fix for @tobject.method, fixes bug 557, 605 and 606

  Revision 1.140  1999/09/11 09:08:33  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.139  1999/09/10 18:48:07  florian
    * some bug fixes (e.g. must_be_valid and procinfo^.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.138  1999/09/07 08:01:20  peter
    * @(<x>) support

  Revision 1.137  1999/09/01 22:08:58  peter
    * fixed crash with assigned()

  Revision 1.136  1999/08/15 22:47:45  peter
    * fixed property writeaccess which was buggy after my previous
      subscribed property access

  Revision 1.135  1999/08/14 00:38:56  peter
    * hack to support property with record fields

  Revision 1.134  1999/08/09 22:16:29  peter
    * fixed crash after wrong para's with class contrustor

}

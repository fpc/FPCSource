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
       hcodegen,types,verbose,strings,
{$ifndef newcg}
       tccal,
{$endif newcg}
       pass_1,
       { parser specific stuff }
       pbase,pdecl,
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
         Store_valid : boolean;

      begin
         if (m_tp_procvar in aktmodeswitches) and
            (not got_addrn) and
            (not in_args) and
            (p^.treetype=loadn) then
            begin
               { support if procvar then for tp7 and many other expression like this }
               Store_valid:=Must_be_valid;
               Must_be_valid:=false;
               do_firstpass(p);
               Must_be_valid:=Store_valid;
               if not(getprocvar) and (p^.resulttype^.deftype=procvardef) then
                 begin
                    p1:=gencallnode(nil,nil);
                    p1^.right:=p;
                    p1^.resulttype:=pprocvardef(p^.resulttype)^.retdef;
                    firstpass(p1);
                    p:=p1;
                 end;
            end;
      end;


    function statement_syssym(l : longint;var pd : pdef) : ptree;
      var
        p1,p2,paras  : ptree;
        prev_in_args : boolean;
        Store_valid  : boolean;
      begin
        prev_in_args:=in_args;
        Store_valid:=Must_be_valid;
        case l of
          in_ord_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              Must_be_valid:=true;
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
                 Must_be_valid:=false;
                 do_firstpass(p1);
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
                 Must_be_valid:=false;
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
              Must_be_valid:=true;
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
              Must_be_valid:=false;
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
              Must_be_valid:=false;
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
              if p1^.location.loc<>LOC_REFERENCE then
                Message(cg_e_illegal_expression);
              p1:=genordinalconstnode(0,s32bitdef);
              Must_be_valid:=false;
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
              Must_be_valid:=false;
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
              Must_be_valid:=false;
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
              Must_be_valid:=false;
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
                 Must_be_valid:=true;
                 do_firstpass(p1);
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
                 Must_be_valid:=false;
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
                 Must_be_valid:=true;
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
              Must_be_valid := False;
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
              Must_be_valid:=false;
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
        Must_be_valid:=Store_valid;
      end;


    { reads the parameter for a subroutine call }
    procedure do_proc_call(getaddr : boolean;var again : boolean;var p1:Ptree;var pd:Pdef);
      var
         prev_in_args : boolean;
         prevafterassn : boolean;
         Store_valid : boolean;
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
              Store_valid:=Must_be_valid;
              Must_be_valid:=false;
              do_firstpass(p1);
              Must_be_valid:=Store_valid;
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
                   p1^.methodpointer:=genselfnode(procinfo._class);
                   p1^.methodpointer^.resulttype:=procinfo._class;
                end;
              { no postfix operators }
              again:=false;
           end;
         pd:=p1^.resulttype;
         in_args:=prev_in_args;
         afterassignment:=prevafterassn;
      end;

    procedure handle_procvar(procvar : pprocvardef;var t : ptree);
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

    { the following procedure handles the access to a property symbol }
    procedure handle_propertysym(sym : psym;st : psymtable;var p1 : ptree;
      var pd : pdef);

      var
         paras : ptree;
         p2 : ptree;
         plist : ppropsymlist;

      begin
         paras:=nil;
         { property parameters? }
         if token=_LECKKLAMMER then
           begin
              consume(_LECKKLAMMER);
              paras:=parse_paras(false,true);
              consume(_RECKKLAMMER);
           end;
         { indexed property }
         if (ppo_indexed in ppropertysym(sym)^.propoptions) then
           begin
              p2:=genordinalconstnode(ppropertysym(sym)^.index,s32bitdef);
              paras:=gencallparanode(p2,paras);
           end;
         { we need only a write property if a := follows }
         { if not(afterassignment) and not(in_args) then }
         if token=_ASSIGNMENT then
           begin
              { write property: }
              { no result }
              pd:=voiddef;
              if assigned(ppropertysym(sym)^.writeaccesssym) then
                begin
                   case ppropertysym(sym)^.writeaccesssym^.sym^.typ of
                     procsym :
                       begin
                         { generate the method call }
                         p1:=genmethodcallnode(pprocsym(ppropertysym(sym)^.writeaccesssym^.sym),st,p1);
                         { we know the procedure to call, so
                           force the usage of that procedure }
                         p1^.procdefinition:=pprocdef(ppropertysym(sym)^.writeaccessdef);
                         p1^.left:=paras;
                         consume(_ASSIGNMENT);
                         { read the expression }
                         getprocvar:=ppropertysym(sym)^.proptype^.deftype=procvardef;
                         p2:=comp_expr(true);
                         if getprocvar then
                          begin
                            if (p2^.treetype=calln) then
                             handle_procvar(pprocvardef(ppropertysym(sym)^.proptype),p2)
                            else
                             if (p2^.treetype=typeconvn) and
                                (p2^.left^.treetype=calln) then
                              handle_procvar(pprocvardef(ppropertysym(sym)^.proptype),p2^.left);
                          end;
                         p1^.left:=gencallparanode(p2,p1^.left);
                         getprocvar:=false;
                       end;
                     varsym :
                       begin
                         if assigned(paras) then
                           message(parser_e_no_paras_allowed);
                         { subscribed access? }
                         plist:=ppropertysym(sym)^.writeaccesssym;
                         while assigned(plist) do
                          begin
                            if p1=nil then
                              p1:=genloadnode(pvarsym(plist^.sym),st)
                            else
                              p1:=gensubscriptnode(pvarsym(plist^.sym),p1);
                            plist:=plist^.next;
                          end;
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
              pd:=ppropertysym(sym)^.proptype;
              if assigned(ppropertysym(sym)^.readaccesssym) then
                begin
                   case ppropertysym(sym)^.readaccesssym^.sym^.typ of
                     varsym :
                       begin
                          if assigned(paras) then
                            message(parser_e_no_paras_allowed);
                          { subscribed access? }
                          plist:=ppropertysym(sym)^.readaccesssym;
                          while assigned(plist) do
                           begin
                             if p1=nil then
                               p1:=genloadnode(pvarsym(plist^.sym),st)
                             else
                               p1:=gensubscriptnode(pvarsym(plist^.sym),p1);
                             plist:=plist^.next;
                           end;
                       end;
                     procsym :
                       begin
                          { generate the method call }
                          p1:=genmethodcallnode(pprocsym(ppropertysym(sym)^.readaccesssym^.sym),st,p1);
                          { we know the procedure to call, so
                            force the usage of that procedure }
                          p1^.procdefinition:=pprocdef(ppropertysym(sym)^.readaccessdef);
                          { insert paras }
                          p1^.left:=paras;
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
                         (m_tp_procvar in aktmodeswitches) and
                         proc_to_procvar_equal(pprocsym(sym)^.definition,getprocvardef))
                        ,again,p1,pd);
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
                      pd:=pvarsym(sym)^.definition;
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
          if (sym^.typ<>funcretsym) and ((procinfo.flags and pi_operator)=0) then
            exit;
          p:=@procinfo;
          while system.assigned(p) do
            begin
               { is this an access to a function result ? }
               if assigned(p^.funcretsym) and
                  ((pfuncretsym(sym)=p^.funcretsym) or
                   ((pvarsym(sym)=opsym) and
                    ((p^.flags and pi_operator)<>0))) and
                  (p^.retdef<>pdef(voiddef)) and
                  (token<>_LKLAMMER) and
                  (not ((m_tp in aktmodeswitches) and
                  (afterassignment or in_args))) then
                 begin
                    if ((pvarsym(sym)=opsym) and
                       ((p^.flags and pi_operator)<>0)) then
                       inc(opsym^.refs);
                    if ((pvarsym(sym)=opsym) and
                       ((p^.flags and pi_operator)<>0)) then
                       inc(opsym^.refs);
                    p1:=genzeronode(funcretn);
                    pd:=p^.retdef;
                    p1^.funcretprocinfo:=p;
                    p1^.retdef:=pd;
                    is_func_ret:=true;
                    exit;
                 end;
               p:=p^.parent;
            end;
          { we must use the function call }
          if(sym^.typ=funcretsym) then
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
           if (m_result in aktmodeswitches) and
              (idtoken=_RESULT) and
              assigned(aktprocsym) and
              (procinfo.retdef<>pdef(voiddef)) then
            begin
              consume(_ID);
              p1:=genzeronode(funcretn);
              pd:=procinfo.retdef;
              p1^.funcretprocinfo:=pointer(@procinfo);
              p1^.retdef:=pd;
            end
           else
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
                              pd:=pabsolutesym(srsym)^.definition;
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
                                 pvarsym(srsym)^.varstate:=vs_declared2;
                               end;
                              pd:=pvarsym(srsym)^.definition;
                            end;
            typedconstsym : begin
                              p1:=gentypedconstloadnode(ptypedconstsym(srsym),srsymtable);
                              pd:=ptypedconstsym(srsym)^.definition;
                            end;
                   syssym : p1:=statement_syssym(psyssym(srsym)^.number,pd);
                  typesym : begin
                              pd:=ptypesym(srsym)^.definition;
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
                                     if (pd^.owner^.unitid=0) or
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
                                         if assigned(procinfo._class) and
                                           not(getaddr) then
                                          begin
                                            if procinfo._class^.is_related(pobjectdef(pd)) then
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
                                            { srsymtable:=pobjectdef(pd)^.symtable;
                                              sym:=pvarsym(srsymtable^.search(pattern)); }

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
                              case pconstsym(srsym)^.consttype of
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
                                        psetdef(pconstsym(srsym)^.definition));
                                constord :
                                  p1:=genordinalconstnode(pconstsym(srsym)^.value,
                                        pconstsym(srsym)^.definition);
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
                                 (m_tp_procvar in aktmodeswitches) and
                                 proc_to_procvar_equal(pprocsym(srsym)^.definition,getprocvardef)),
                                again,p1,pd);
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
                         pd:=ppointerdef(pd)^.definition;
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
                                   pd:=ppointerdef(pd)^.definition;
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
                                   pd:=parraydef(pd)^.definition;
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
                         pd:=ppointerdef(pd)^.definition;
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
                                pd:=sym^.definition;
                              end;
                            consume(_ID);
                          end;

                        classrefdef:
                          begin
                             classh:=pobjectdef(pclassrefdef(pd)^.definition);
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
                             if ppointerdef(pd)^.definition^.deftype in [recorddef,objectdef,classrefdef] then
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
                            ((pprocvardef(pd)^.para1=nil) and
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
                              pd:=pprocvardef(pd)^.retdef;
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
                 else if {(ppointerdef(pd)^.definition^.deftype<>objectdef)}
                       token=_RKLAMMER then
                  begin
                    if (ppointerdef(pd)^.definition^.deftype=objectdef) and
                       (oo_has_vmt in pobjectdef(ppointerdef(pd)^.definition)^.objectoptions)  then
                     Message(parser_w_use_extended_syntax_for_objects);
                    p1:=gensinglenode(newn,nil);
                    p1^.resulttype:=pd2;
                    consume(_RKLAMMER);
                    (*Message(parser_e_pointer_to_class_expected);
                    { if an error occurs, read til the end of
                      the new statement }
                    p1:=genzeronode(errorn);
                    l:=1;
                    while true do
                     begin
                       case token of
                        _LKLAMMER : inc(l);
                        _RKLAMMER : dec(l);
                       end;
                       consume(token);
                       if l=0 then
                        break;
                     end;*)
                  end
                 else
                  begin
                    disposetree(p1);
                    p1:=genzeronode(hnewn);
                    p1^.resulttype:=ppointerdef(pd)^.definition;
                    consume(_COMMA);
                    afterassignment:=false;
                    { determines the current object defintion }
                    classh:=pobjectdef(ppointerdef(pd)^.definition);
                    { check for an abstract class }
                    if (oo_has_abstract in classh^.objectoptions) then
                      Message(sym_e_no_instance_of_abstract_object);

                    { search the constructor also in the symbol tables of
                      the parents }

                    { no constructor found }
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
                 if not assigned(procinfo._class) then
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
                       pd:=new(pclassrefdef,init(procinfo._class));
                       p1:=genselfnode(pd);
                       p1^.resulttype:=pd;
                     end
                    else
                     begin
                       p1:=genselfnode(procinfo._class);
                       p1^.resulttype:=procinfo._class;
                     end;
                    pd:=p1^.resulttype;
                    postfixoperators;
                  end;
               end;
  _INHERITED : begin
                 again:=true;
                 consume(_INHERITED);
                 if assigned(procinfo._class) then
                  begin
                    classh:=procinfo._class^.childof;
                    while assigned(classh) do
                     begin
                       srsymtable:=pobjectdef(classh)^.symtable;
                       sym:=pvarsym(srsymtable^.search(pattern));
                       if assigned(sym) then
                        begin
                          p1:=genzeronode(typen);
                          p1^.resulttype:=classh;
                          pd:=p1^.resulttype;
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
                 pd:=stringtype;
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
                 p1:=gensinglenode(umminusn,p1);
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
        (tok:_CARET   ;nod:caretn),
        (tok:_UNEQUAL ;nod:unequaln),
        (tok:_AS     ;nod:asn),
        (tok:_IN     ;nod:inn),
        (tok:_IS     ;nod:isn),
        (tok:_OR     ;nod:orn),
        (tok:_AND    ;nod:andn),
        (tok:_DIV    ;nod:divn),
        (tok:_MOD    ;nod:modn),
        (tok:_SHL    ;nod:shln),
        (tok:_SHR    ;nod:shrn),
        (tok:_XOR    ;nod:xorn)
      );
      operator_levels:array[Toperator_precedence] of set of Ttoken=
         ([_LT,_LTE,_GT,_GTE,_EQUAL,_UNEQUAL,_IN,_IS],
          [_PLUS,_MINUS,_OR,_XOR],
          [_CARET,_SYMDIF,_STARSTAR,_STAR,_SLASH,_DIV,_MOD,_AND,_SHL,_SHR,_AS]);

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
                             begin
                               if (p2^.treetype=calln) then
                                handle_procvar(getprocvardef,p2)
                               else
                                { also allow p:= proc(t); !! (PM) }
                                if (p2^.treetype=typeconvn) and
                                   (p2^.left^.treetype=calln) then
                                 handle_procvar(getprocvardef,p2^.left);
                             end;
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
  Revision 1.143  1999-09-15 20:35:41  florian
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
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
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

  Revision 1.133  1999/08/05 16:53:04  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.132  1999/08/04 13:49:45  florian
    * new(...)^. is now allowed

  Revision 1.131  1999/08/04 13:02:55  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.130  1999/08/04 00:23:12  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.129  1999/08/03 22:02:59  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.128  1999/08/03 13:50:17  michael
  + Changes for alpha

  Revision 1.127  1999/08/01 18:28:13  florian
    * modifications for the new code generator

  Revision 1.126  1999/07/30 12:28:40  peter
    * fixed crash with unknown id and colon parameter in write

  Revision 1.125  1999/07/27 23:42:14  peter
    * indirect type referencing is now allowed

  Revision 1.124  1999/07/23 21:31:42  peter
    * fixed crash with resourcestring

  Revision 1.123  1999/07/23 11:37:46  peter
    * error for illegal type reference, instead of 10998

  Revision 1.122  1999/07/22 09:37:52  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.121  1999/07/16 10:04:35  peter
    * merged

  Revision 1.120  1999/07/06 22:38:11  florian
    * another fix for TP/Delphi styled procedure variables

  Revision 1.119  1999/07/05 20:13:16  peter
    * removed temp defines

  Revision 1.118  1999/07/01 21:33:57  peter
    * merged

  Revision 1.117  1999/06/30 15:43:20  florian
    * two bugs regarding method variables fixed
      - if you take in a method the address of another method
        don't need self anymore
      - if the class pointer was in a register, wrong code for a method
        variable load was generated

  Revision 1.116  1999/06/26 00:24:53  pierre
   * mereg from fixes-0_99_12 branch

  Revision 1.112.2.8  1999/07/16 09:54:57  peter
    * @procvar support in tp7 mode works again

  Revision 1.112.2.7  1999/07/07 07:53:10  michael
  + Merged patches from florian

  Revision 1.112.2.6  1999/07/01 21:31:59  peter
    * procvar fixes again

  Revision 1.112.2.5  1999/07/01 15:17:17  peter
    * methoidpointer fixes from florian

  Revision 1.112.2.4  1999/06/26 00:22:30  pierre
   * wrong warnings in -So mode suppressed

  Revision 1.112.2.3  1999/06/17 12:51:44  pierre
   * changed is_assignment_overloaded into
      function assignment_overloaded : pprocdef
      to allow overloading of assignment with only different result type

  Revision 1.112.2.2  1999/06/15 18:54:52  peter
    * more procvar fixes

  Revision 1.112.2.1  1999/06/13 22:38:09  peter
    * tp_procvar check for loading of procvars when getaddr=false

  Revision 1.112  1999/06/02 22:44:11  pierre
   * previous wrong log corrected

  Revision 1.111  1999/06/02 22:25:43  pierre
  * changed $ifdef FPC @ into $ifndef TP
  * changes for correct procvar handling under tp mode

  Revision 1.110  1999/06/01 19:27:55  peter
    * better checks for procvar and methodpointer

  Revision 1.109  1999/05/27 19:44:46  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.108  1999/05/18 14:15:54  peter
    * containsself fixes
    * checktypes()

  Revision 1.107  1999/05/18 09:52:18  peter
    * procedure of object and addrn fixes

  Revision 1.106  1999/05/16 17:06:31  peter
    * remove firstcallparan which looks obsolete

  Revision 1.105  1999/05/12 22:36:09  florian
    * override isn't allowed in objects!

  Revision 1.104  1999/05/07 10:35:23  florian
    * first fix for a problem with method pointer properties, still doesn't work
      with WITH

  Revision 1.103  1999/05/06 21:40:16  peter
    * fixed crash

  Revision 1.101  1999/05/06 09:05:21  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.100  1999/05/04 21:44:57  florian
    * changes to compile it with Delphi 4.0

  Revision 1.99  1999/05/01 13:24:31  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.98  1999/04/26 18:29:56  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.97  1999/04/19 09:27:48  peter
    * removed my property fix

  Revision 1.96  1999/04/19 09:13:47  peter
    * class property without write support

  Revision 1.95  1999/04/19 06:10:08  florian
    * property problem fixed: a propertysym is only a write
      access if it is followed by a assignment token

  Revision 1.94  1999/04/17 13:12:17  peter
    * addr() internal

  Revision 1.93  1999/04/15 09:00:08  peter
    * fixed property write

  Revision 1.92  1999/04/08 20:59:43  florian
    * fixed problem with default properties which are a class
    * case bug (from the mailing list with -O2) fixed, the
      distance of the case labels can be greater than the positive
      range of a longint => it is now a dword for fpc

  Revision 1.91  1999/04/06 11:21:56  peter
    * more use of ttoken

  Revision 1.90  1999/03/31 13:55:12  peter
    * assembler inlining working for ag386bin

  Revision 1.89  1999/03/26 00:05:36  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.88  1999/03/24 23:17:15  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.87  1999/03/16 17:52:52  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in cgi386inl: also range checking for subrange types (compile with "-dreadrangecheck")
    * in cgai386: also small fixes to emitrangecheck

  Revision 1.86  1999/03/04 13:55:44  pierre
    * some m68k fixes (still not compilable !)
    * new(tobj) does not give warning if tobj has no VMT !

  Revision 1.85  1999/02/22 15:09:39  florian
    * behaviaor of PROTECTED and PRIVATE fixed, works now like TP/Delphi

  Revision 1.84  1999/02/22 02:15:26  peter
    * updates for ag386bin

  Revision 1.83  1999/02/11 09:46:25  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.82  1999/01/28 14:06:47  florian
    * small fix for method pointers
    * found the annoying strpas bug, mainly nested call to type cast which
      use ansistrings crash

  Revision 1.81  1999/01/27 00:13:55  florian
    * "procedure of object"-stuff fixed

  Revision 1.80  1999/01/21 16:41:01  pierre
   * fix for constructor inside with statements

  Revision 1.79  1998/12/30 22:15:48  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.78  1998/12/11 00:03:32  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.77  1998/12/04 10:18:09  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.76  1998/11/27 14:50:40  peter
    + open strings, $P switch support

  Revision 1.75  1998/11/25 19:12:51  pierre
    * var:=new(pointer_type) support added

  Revision 1.74  1998/11/13 10:18:11  peter
    + nil constants

  Revision 1.73  1998/11/05 12:02:52  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.72  1998/11/04 10:11:41  peter
    * ansistring fixes

  Revision 1.71  1998/10/22 23:57:29  peter
    * fixed filedef for typenodetype

  Revision 1.70  1998/10/21 15:12:54  pierre
    * bug fix for IOCHECK inside a procedure with iocheck modifier
    * removed the GPF for unexistant overloading
      (firstcall was called with procedinition=nil !)
    * changed typen to what Florian proposed
      gentypenode(p : pdef) sets the typenodetype field
      and resulttype is only set if inside bt_type block !

  Revision 1.69  1998/10/20 15:10:19  pierre
    * type ptree only allowed inside expression
      if following sizeof typeof low high or as first arg of new !!

  Revision 1.68  1998/10/20 11:15:44  pierre
   * calling of private method allowed inside child object method

  Revision 1.67  1998/10/19 08:54:57  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.66  1998/10/15 15:13:28  pierre
    + added oo_hasconstructor and oo_hasdestructor
      for objects options

  Revision 1.65  1998/10/13 13:10:24  peter
    * new style for m68k/i386 infos and enums

  Revision 1.64  1998/10/12 12:20:55  pierre
    + added tai_const_symbol_offset
      for r : pointer = @var.field;
    * better message for different arg names on implementation
      of function

  Revision 1.63  1998/10/12 10:28:30  florian
    + auto dereferencing of pointers to structured types in delphi mode

  Revision 1.62  1998/10/12 10:05:41  peter
    * fixed mem leak with arrayconstrutor

  Revision 1.61  1998/10/05 13:57:15  peter
    * crash preventions

  Revision 1.60  1998/10/05 12:32:46  peter
    + assert() support

  Revision 1.59  1998/10/01 14:56:24  peter
    * crash preventions

  Revision 1.58  1998/09/30 07:40:35  florian
    * better error recovering

  Revision 1.57  1998/09/28 16:18:16  florian
    * two fixes to get ansi strings work

  Revision 1.56  1998/09/26 17:45:36  peter
    + idtoken and only one token table

  Revision 1.55  1998/09/24 23:49:10  peter
    + aktmodeswitches

  Revision 1.54  1998/09/23 15:46:39  florian
    * problem with with and classes fixed

  Revision 1.53  1998/09/23 09:58:54  peter
    * first working array of const things

  Revision 1.52  1998/09/20 09:38:45  florian
    * hasharray for defs fixed
    * ansistring code generation corrected (init/final, assignement)

  Revision 1.51  1998/09/18 16:03:43  florian
    * some changes to compile with Delphi

  Revision 1.50  1998/09/17 13:41:18  pierre
  sizeof(TPOINT) problem

  Revision 1.49.2.1  1998/09/17 08:42:31  pierre
  TPOINT sizeof fix

  Revision 1.49  1998/09/09 11:50:53  pierre
    * forward def are not put in record or objects
    + added check for forwards also in record and objects
    * dummy parasymtable for unit initialization removed from
    symtable stack

  Revision 1.48  1998/09/07 22:25:53  peter
    * fixed str(boolean,string) which was allowed
    * fixed write(' ':<int expression>) only constants where allowed :(

  Revision 1.47  1998/09/07 18:46:10  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.46  1998/09/04 08:42:03  peter
    * updated some error messages

  Revision 1.45  1998/09/01 17:39:49  peter
    + internal constant functions

  Revision 1.44  1998/08/28 10:54:24  peter
    * fixed smallset generation from elements, it has never worked before!

  Revision 1.43  1998/08/23 16:07:24  florian
    * internalerror with mod/div fixed

  Revision 1.42  1998/08/21 14:08:50  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.41  1998/08/20 21:36:39  peter
    * fixed 'with object do' bug

  Revision 1.40  1998/08/20 09:26:41  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.39  1998/08/18 16:48:48  pierre
    * bug for -So proc assignment to p^rocvar fixed

  Revision 1.38  1998/08/18 14:17:09  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.37  1998/08/18 09:24:43  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.36  1998/08/15 16:50:29  peter
    * fixed proc()=expr which was not allowed anymore by my previous fix

  Revision 1.35  1998/08/14 18:18:46  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.34  1998/08/13 11:00:12  peter
    * fixed procedure<>procedure construct

  Revision 1.33  1998/08/11 15:31:39  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.32  1998/08/11 14:05:32  peter
    * fixed sizeof(array of char)

  Revision 1.31  1998/08/10 14:50:11  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.30  1998/07/28 21:52:54  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.29  1998/07/27 21:57:13  florian
    * fix to allow tv like stream registration:
        @tmenu.load doesn't work if load had parameters or if load was only
        declared in an anchestor class of tmenu

  Revision 1.28  1998/07/14 21:46:51  peter
    * updated messages file

  Revision 1.27  1998/06/25 14:04:23  peter
    + internal inc/dec

  Revision 1.26  1998/06/09 16:01:46  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.25  1998/06/05 14:37:33  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.24  1998/06/04 23:51:52  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.23  1998/06/04 09:55:40  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.22  1998/06/02 17:03:03  pierre
    *  with node corrected for objects
    * small bugs for SUPPORT_MMX fixed

  Revision 1.21  1998/05/27 19:45:05  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

  Revision 1.20  1998/05/26 07:53:59  pierre
    * bug fix for empty sets (nil pd was dereferenced )

  Revision 1.19  1998/05/25 17:11:43  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.18  1998/05/23 01:21:20  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.17  1998/05/22 12:37:03  carl
    * crash bugfix (patched msanually to main branch)

  Revision 1.16  1998/05/21 19:33:32  peter
    + better procedure directive handling and only one table

  Revision 1.15  1998/05/20 09:42:35  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.14  1998/05/11 13:07:56  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.13  1998/05/06 08:38:45  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.12  1998/05/05 12:05:42  florian
    * problems with properties fixed
    * crash fixed:  i:=l when i and l are undefined, was a problem with
      implementation of private/protected

  Revision 1.11  1998/05/04 11:22:26  florian
    * problem with DOM solved: it crashes when accessing a property in a method

  Revision 1.10  1998/05/01 16:38:45  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.9  1998/04/29 10:33:58  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.8  1998/04/14 23:27:03  florian
    + exclude/include with constant second parameter added

  Revision 1.7  1998/04/09 23:02:15  florian
    * small problems solved to get remake3 work

  Revision 1.6  1998/04/09 22:16:35  florian
    * problem with previous REGALLOC solved
    * improved property support

  Revision 1.5  1998/04/08 10:26:09  florian
    * correct error handling of virtual constructors
    * problem with new type declaration handling fixed

  Revision 1.4  1998/04/07 22:45:05  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.3  1998/04/07 13:19:46  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)
}

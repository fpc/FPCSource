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
    procedure do_member_read(const sym : psym;var p1 : ptree;
      var pd : pdef;var again : boolean);

    function get_intconst:longint;

    function get_stringconst:string;

  implementation

    uses
       cobjects,globals,scanner,aasm,pass_1,systems,
       hcodegen,types,verbose
       { parser specific stuff }
       ,pbase,pdecl
       { processor specific stuff }
{$ifdef i386}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    function parse_paras(_colon,in_prop_paras : boolean) : ptree;

      var
         p1,p2 : ptree;
         end_of_paras : ttoken;

      begin
         if in_prop_paras  then
           end_of_paras:=RECKKLAMMER
         else
           end_of_paras:=RKLAMMER;
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
              if _colon and (token=COLON) then
                begin
                   consume(COLON);
                   p1:=comp_expr(true);
                   p2:=gencallparanode(p1,p2);
                   p2^.is_colon_para:=true;
                   if token=COLON then
                     begin
                        consume(COLON);
                        p1:=comp_expr(true);
                        p2:=gencallparanode(p1,p2);
                        p2^.is_colon_para:=true;
                     end
                end;
              if token=COMMA then
                consume(COMMA)
              else
                break;
           end;
         dec(parsing_para_level);
         parse_paras:=p2;
      end;

    function statement_syssym(l : longint;var pd : pdef) : ptree;
{   const   regnames:array[R_EAX..R_EDI] of string[3]=
             ('EAX','ECX','EDX','EBX','','','ESI','EDI'); }

      var
         p1,p2 : ptree;
         paras : ptree;
         prev_in_args : boolean;
         Store_valid : boolean;

      begin
         prev_in_args:=in_args;
         Store_valid:=Must_be_valid;
         case l of
            in_ord_x:
              begin
                 consume(LKLAMMER);
                 in_args:=true;
                 Must_be_valid:=true;
                 p1:=comp_expr(true);
                 consume(RKLAMMER);
                 do_firstpass(p1);
                 p1:=geninlinenode(in_ord_x,p1);
                 do_firstpass(p1);
                 statement_syssym := p1;
                 pd:=p1^.resulttype;
              end;
            in_break:
              begin
                 statement_syssym:=genzeronode(breakn);
                 pd:=voiddef;
              end;
            in_continue:
              begin
                 statement_syssym:=genzeronode(continuen);
                 pd:=voiddef;
              end;
            in_typeof_x : begin
                             consume(LKLAMMER);
                             in_args:=true;
                             p1:=comp_expr(true);
                             consume(RKLAMMER);
                             pd:=voidpointerdef;
                             if p1^.treetype=typen then
                               begin
                                  if (p1^.resulttype=nil) then
                                    begin
                                       Message(sym_e_type_mismatch);
                                       statement_syssym:=genzeronode(errorn);
                                    end
                                  else
                                  if p1^.resulttype^.deftype=objectdef then
                                    statement_syssym:=geninlinenode(in_typeof_x,p1)
                                  else
                                    begin
                                       Message(sym_e_type_mismatch);
                                       statement_syssym:=genzeronode(errorn);
                                    end;
                               end
                             else
                               begin
                                  Must_be_valid:=false;
                                  do_firstpass(p1);
                                  if (p1^.resulttype=nil) then
                                    begin
                                       Message(sym_e_type_mismatch);
                                       statement_syssym:=genzeronode(errorn)
                                    end
                                  else
                                  if p1^.resulttype^.deftype=objectdef then
                                    statement_syssym:=geninlinenode(in_typeof_x,p1)
                                  else
                                    begin
                                       Message(sym_e_type_mismatch);
                                       statement_syssym:=genzeronode(errorn)
                                    end;
                               end;
                          end;
            in_sizeof_x : begin
                             consume(LKLAMMER);
                             in_args:=true;
                             p1:=comp_expr(true);
                             consume(RKLAMMER);
                             pd:=s32bitdef;
                             if p1^.treetype=typen then
                               begin
                                  statement_syssym:=genordinalconstnode(
                                    p1^.resulttype^.size,pd);
                                  { p1 not needed !}
                                  disposetree(p1);
                               end
                             else
                               begin
                                  Must_be_valid:=false;
                                  do_firstpass(p1);
                                  if p1^.resulttype^.deftype<>objectdef then
                                    begin
                                       statement_syssym:=genordinalconstnode(
                                         p1^.resulttype^.size,pd);
                                       { p1 not needed !}
                                       disposetree(p1);
                                    end
                                  else
                                    begin
                                       statement_syssym:=geninlinenode(in_sizeof_x,p1);
                                    end;
                               end;
                          end;
            in_assigned_x : begin
                               consume(LKLAMMER);
                               in_args:=true;
                               p1:=comp_expr(true);
                               Must_be_valid:=true;
                               do_firstpass(p1);
                               case p1^.resulttype^.deftype of
                                 pointerdef,procvardef,
                                 classrefdef:
                                   ;
                                 objectdef:
                                   if not(pobjectdef(p1^.resulttype)^.isclass) then
                                     Message(parser_e_illegal_parameter_list);
                                 else Message(parser_e_illegal_parameter_list);
                               end;
                               p2:=gencallparanode(p1,nil);
                               p2:=geninlinenode(in_assigned_x,p2);
                               consume(RKLAMMER);
                               pd:=booldef;
                               statement_syssym:=p2;
                            end;
            in_ofs_x : begin
                          consume(LKLAMMER);
                          in_args:=true;
                          p1:=comp_expr(true);
                          p1:=gensinglenode(addrn,p1);
                          Must_be_valid:=false;
                          do_firstpass(p1);
                        { Ofs() returns a longint, not a pointer }
                          p1^.resulttype:=u32bitdef;
                          pd:=p1^.resulttype;
                          consume(RKLAMMER);
                          statement_syssym:=p1;
                       end;
            in_seg_x : begin
                          consume(LKLAMMER);
                          in_args:=true;
                          p1:=comp_expr(true);
                          do_firstpass(p1);
                          if p1^.location.loc<>LOC_REFERENCE then
                            Message(cg_e_illegal_expression);
                          p1:=genordinalconstnode(0,s32bitdef);
                          Must_be_valid:=false;
                          pd:=s32bitdef;
                          consume(RKLAMMER);
                          statement_syssym:=p1;
                       end;
            in_high_x,
            in_low_x : begin
                          consume(LKLAMMER);
                          in_args:=true;
                          p1:=comp_expr(true);
                          do_firstpass(p1);
                          Must_be_valid:=false;
                          p2:=geninlinenode(l,p1);
                          consume(RKLAMMER);
                          pd:=s32bitdef;
                          statement_syssym:=p2;
                       end;
            in_succ_x,
            in_pred_x : begin
                          consume(LKLAMMER);
                          in_args:=true;
                          p1:=comp_expr(true);
                          do_firstpass(p1);
                          Must_be_valid:=false;
                          p2:=geninlinenode(l,p1);
                          consume(RKLAMMER);
                          pd:=p1^.resulttype;
                          statement_syssym:=p2;
                       end;
            in_inc_x,
            in_dec_x : begin
                          consume(LKLAMMER);
                          in_args:=true;
                          p1:=comp_expr(true);
                          p2:=gencallparanode(p1,nil);
                          Must_be_valid:=false;
                          if token=COMMA then
                            begin
                               consume(COMMA);
                               p1:=comp_expr(true);
                               p2:=gencallparanode(p1,p2);
                            end;
                          statement_syssym:=geninlinenode(l,p2);
                          consume(RKLAMMER);
                          pd:=voiddef;
                       end;
            in_concat_x : begin
                             consume(LKLAMMER);
                             in_args:=true;
                             p2:=nil;
                             while true do
                               begin
                                  p1:=comp_expr(true);
                                  Must_be_valid:=true;
                                  do_firstpass(p1);
                                  if not((p1^.resulttype^.deftype=stringdef) or
                                         ((p1^.resulttype^.deftype=orddef) and
                                          (porddef(p1^.resulttype)^.typ=uchar)
                                         )
                                    ) then Message(parser_e_illegal_parameter_list);
                                  if p2<>nil then
                                    p2:=gennode(addn,p2,p1)
                                  else p2:=p1;
                                  if token=COMMA then
                                    consume(COMMA)
                                  else break;
                               end;
                             consume(RKLAMMER);
                             pd:=cstringdef;
                             statement_syssym:=p2;
                          end;
            in_read_x,
            in_readln_x : begin
                             if token=LKLAMMER then
                               begin
                                  consume(LKLAMMER);
                                  in_args:=true;
                                  Must_be_valid:=false;
                                  paras:=parse_paras(false,false);
                                  consume(RKLAMMER);
                               end
                             else
                               paras:=nil;
                             pd:=voiddef;
                             p1:=geninlinenode(l,paras);
                             do_firstpass(p1);
                             statement_syssym := p1;
                          end;
            in_write_x,
            in_writeln_x : begin
                             if token=LKLAMMER then
                               begin
                                  consume(LKLAMMER);
                                  in_args:=true;
                                  Must_be_valid:=true;
                                  paras:=parse_paras(true,false);
                                  consume(RKLAMMER);
                               end
                             else
                               paras:=nil;
                             pd:=voiddef;
                             p1 := geninlinenode(l,paras);
                             do_firstpass(p1);
                             statement_syssym := p1;
                          end;
            in_str_x_string : begin
                                 consume(LKLAMMER);
                                 in_args:=true;
                                 paras:=parse_paras(true,false);
                                 consume(RKLAMMER);
                                 p1 := geninlinenode(l,paras);
                                 do_firstpass(p1);
                                 statement_syssym := p1;
                                 pd:=voiddef;
                              end;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 consume(LKLAMMER);
                 in_args:=true;
                 p1:=comp_expr(true);
                 Must_be_valid:=false;
                 consume(COMMA);
                 p2:=comp_expr(true);
                 { just a bit lisp feeling }
                 statement_syssym:=geninlinenode(l,
                   gencallparanode(p1,gencallparanode(p2,nil)));
                 consume(RKLAMMER);
                 pd:=voiddef;
              end;
            {in_val_x :        begin
                                 consume(LKLAMMER);
                                 paras:=parse_paras(false);
                                 consume(RKLAMMER);
                                 p1 := geninlinenode(l,paras);
                                 do_firstpass(p1);
                                 statement_syssym := p1;
                                 pd:=voiddef;
                              end;    }
            else internalerror(15);
         end;
         in_args:=prev_in_args;
         Must_be_valid:=Store_valid;
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
         { a subroutine                          }
         if not(getaddr) then
           begin
              if token=LKLAMMER then
                begin
                   consume(LKLAMMER);
                   in_args:=true;
                   p1^.left:=parse_paras(false,false);
                   consume(RKLAMMER);
                end
              else p1^.left:=nil;

              { do firstpass because we need the  }
              { result type                       }
              do_firstpass(p1);
           end
         else
           begin
              { address operator @: }
              p1^.left:=nil;
              { forget pd }
              pd:=nil;
              { no postfix operators }
              again:=false;
           end;
         pd:=p1^.resulttype;
         in_args:=prev_in_args;
         afterassignment:=prevafterassn;
      end;

    { the following procedure handles the access to a property symbol }
    procedure handle_propertysym(sym : psym;var p1 : ptree;
      var pd : pdef);

      var
         paras : ptree;
         p2 : ptree;

      begin
         paras:=nil;
         { property parameters? }
         if token=LECKKLAMMER then
           begin
              consume(LECKKLAMMER);
              paras:=parse_paras(false,true);
              consume(RECKKLAMMER);
           end;
         { indexed property }
         if (ppropertysym(sym)^.options and ppo_indexed)<>0 then
           begin
              p2:=genordinalconstnode(ppropertysym(sym)^.index,s32bitdef);
              paras:=gencallparanode(p2,paras);
           end;
         if not(afterassignment) and not(in_args) then
           begin
              { write property: }
              { no result }
              pd:=voiddef;
              if assigned(ppropertysym(sym)^.writeaccesssym) then
                begin
                   if ppropertysym(sym)^.writeaccesssym^.typ=procsym then
                     begin
                        { generate the method call }
                        p1:=genmethodcallnode(pprocsym(
                          ppropertysym(sym)^.writeaccesssym),
                          ppropertysym(sym)^.writeaccesssym^.owner,p1);
                        { we know the procedure to call, so
                          force the usage of that procedure }
                        p1^.procdefinition:=pprocdef(ppropertysym(sym)^.writeaccessdef);
                        p1^.left:=paras;
                        consume(ASSIGNMENT);
                        { read the expression }
                        p2:=comp_expr(true);
                        p1^.left:=gencallparanode(p2,p1^.left);
                     end
                   else if ppropertysym(sym)^.writeaccesssym^.typ=varsym then
                     begin
                        if assigned(paras) then
                          message(parser_e_no_paras_allowed);
                        { subscribed access? }
                        if p1=nil then
                          begin
                             p1:=genloadnode(pvarsym(ppropertysym(sym)^.readaccesssym),
                               ppropertysym(sym)^.readaccesssym^.owner);
                          end
                        else
                          p1:=gensubscriptnode(pvarsym(
                            ppropertysym(sym)^.readaccesssym),p1);
                        consume(ASSIGNMENT);
                        { read the expression }
                        p2:=comp_expr(true);
                        p1:=gennode(assignn,p1,p2);
                     end
                   else
                     begin
                        p1:=genzeronode(errorn);
                        Message(parser_e_no_procedure_to_access_property);
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
                   if ppropertysym(sym)^.readaccesssym^.typ=varsym then
                     begin
                        if assigned(paras) then
                          message(parser_e_no_paras_allowed);
                        { subscribed access? }
                        if p1=nil then
                          begin
                             p1:=genloadnode(pvarsym(ppropertysym(sym)^.readaccesssym),
                               ppropertysym(sym)^.readaccesssym^.owner);
                          end
                        else
                          p1:=gensubscriptnode(pvarsym(
                            ppropertysym(sym)^.readaccesssym),p1);
                     end
                   else if ppropertysym(sym)^.readaccesssym^.typ=procsym then
                     begin
                        { generate the method call }
                        p1:=genmethodcallnode(pprocsym(
                          ppropertysym(sym)^.readaccesssym),
                          ppropertysym(sym)^.readaccesssym^.owner,p1);
                        { we know the procedure to call, so
                          force the usage of that procedure }
                        p1^.procdefinition:=pprocdef(ppropertysym(sym)^.readaccessdef);
                        { insert paras }
                        p1^.left:=paras;
                        { if we should be delphi compatible  }
                        { then force type conversion         }
                        {                                    }
                        { isn't neccessary, the result types }
                        { have to match excatly              }
                        {if cs_delphi2_compatible in aktswitches then
                          p1:=gentypeconvnode(p1,pd);
                        }
                     end
                   else
                     begin
                        p1:=genzeronode(errorn);
                        Message(sym_e_type_mismatch);
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
    procedure do_member_read(const sym : psym;var p1 : ptree;
      var pd : pdef;var again : boolean);

      var
         static_name : string;
         isclassref : boolean;

      begin
         if sym=nil then
           begin
              Message(sym_e_id_no_member);
              disposetree(p1);
              p1:=genzeronode(errorn);
              { try to clean up }
              pd:=generrordef;
              again:=false;
           end
         else
           begin
              isclassref:=pd^.deftype=classrefdef;
              { check protected and private members }
              if ((sym^.properties and sp_private)<>0) and
                 (pobjectdef(sym^.owner^.defowner)^.owner^.symtabletype=unitsymtable) then
                Message(parser_e_cant_access_private_member);
              if ((sym^.properties and sp_protected)<>0) and
                (pobjectdef(pd)^.owner^.symtabletype=unitsymtable) then
               Message(parser_e_cant_access_protected_member);
              { we assume, that only procsyms and varsyms are in an object }
              { symbol table, for classes, properties are allowed          }
              case sym^.typ of
                 procsym:
                   begin
                      p1:=genmethodcallnode(pprocsym(sym),srsymtable,p1);
                      do_proc_call(false,again,p1,pd);
                      { now we know the real method e.g. we can check for }
                      { a class method                                    }
                      if isclassref and ((p1^.procdefinition^.options and (poclassmethod or poconstructor))=0) then
                        Message(parser_e_only_class_methods_via_class_ref);
                   end;
                 varsym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      if (sym^.properties and sp_static)<>0 then
                        begin
                           static_name:=lowercase(srsymtable^.name^)+'_'+sym^.name;
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
                      handle_propertysym(sym,p1,pd);
                   end;
                 else internalerror(16);
              end;
           end;
      end;

    function factor(getaddr : boolean) : ptree;

      var
         l : longint;
         p1,p2,p3 : ptree;
         code : word;
         pd,pd2 : pdef;
         unit_specific, again : boolean;
         static_name : string;
         sym : pvarsym;
         classh : pobjectdef;
         d : bestreal;
         constset : pconstset;
         propsym : ppropertysym;
{$ifdef UseTokenInfo}
         oldp1 : ptree;
         filepos : tfileposinfo;
{$endif UseTokenInfo}


{$ifdef UseTokenInfo}
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
{$endif UseTokenInfo}

      { p1 and p2 must contain valid values }
      procedure postfixoperators;

        begin
{$ifdef UseTokenInfo}
             check_tokenpos;
{$endif UseTokenInfo}
           while again do
             begin
                case token of
                   CARET:
                     begin
                        consume(CARET);
                        if pd^.deftype<>pointerdef then
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
                   LECKKLAMMER:
                     begin
                        if (pd^.deftype=objectdef) and
                          pobjectdef(pd)^.isclass then
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
                               begin
                                  p1:=nil;
                                  handle_propertysym(propsym,p1,pd);
                               end;
                          end
                        else
                          begin
                             consume(LECKKLAMMER);
                             repeat
                               if (pd^.deftype<>arraydef) and
                                  (pd^.deftype<>stringdef) and
                                  (pd^.deftype<>pointerdef) then
                                 begin
                                    Message(cg_e_invalid_qualifier);
                                    disposetree(p1);
                                    p1:=genzeronode(errorn);
                                    again:=false;
                                 end
                               else if (pd^.deftype=pointerdef) then
                                 begin
                                    p2:=comp_expr(true);
                                    p1:=gennode(vecn,p1,p2);
                                    pd:=ppointerdef(pd)^.definition;
                                 end
                               else
                                 begin
                                    p2:=comp_expr(true);
                                  { support SEG:OFS for go32v2 Mem[] }
                                    if (target_info.target=target_GO32V2) and
                                       (p1^.treetype=loadn) and
                                       assigned(p1^.symtableentry) and
                                       assigned(p1^.symtableentry^.owner^.name) and
                                       (p1^.symtableentry^.owner^.name^='SYSTEM') and
                                       ((p1^.symtableentry^.name='MEM') or
                                        (p1^.symtableentry^.name='MEMW') or
                                        (p1^.symtableentry^.name='MEML')) then
                                      begin
                                        if (token=COLON) then
                                         begin
                                           consume(COLON);
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
                                    { else
                                    if (target_info.target=target_GO32V2) and
                                       assigned(p1^.symtableentry) and
                                       assigned(p1^.symtableentry^.owner^.name) and
                                       (p1^.symtableentry^.owner^.name^='SYSTEM') and
                                       ((p1^.symtableentry^.name='PORT') or
                                        (p1^.symtableentry^.name='PORTW') or
                                        (p1^.symtableentry^.name='PORTL')) then
                                         begin
                                           p1:=gennode(vecn,p1,p2);
                                           p1^.portindex:=true;
                                           p
                                         end;
                                      end      }
                                    else
                                      p1:=gennode(vecn,p1,p2);
                                    if pd^.deftype=stringdef then
                                      pd:=cchardef
                                    else
                                      pd:=parraydef(pd)^.definition;
                                 end;
                               if token=COMMA then consume(COMMA)
                                 else break;
                             until false;
                             consume(RECKKLAMMER);
                          end;
                     end;
                   POINT:
                     begin
                        consume(POINT);
                        case pd^.deftype of
                           recorddef:
                                 begin
                                    sym:=pvarsym(precdef(pd)^.symtable^.search(pattern));
                                    consume(ID);
                                    if sym=nil then
                                      begin
                                         Message(sym_e_illegal_field);
                                         disposetree(p1);
                                         p1:=genzeronode(errorn);
                                      end
                                    else
                                      begin
                                         p1:=gensubscriptnode(sym,p1);
                                         pd:=sym^.definition;
                                      end;
                                 end;
                           classrefdef:
                             begin
                                classh:=pobjectdef(pclassrefdef(pd)^.definition);
                                sym:=nil;
                                while assigned(classh) do
                                  begin
                                     sym:=pvarsym(classh^.publicsyms^.search(pattern));
                                     srsymtable:=classh^.publicsyms;
                                     if assigned(sym) then
                                       break;
                                     classh:=classh^.childof;
                                  end;
                                consume(ID);
                                do_member_read(sym,p1,pd,again);
                             end;
                           objectdef:
                                 begin
                                    classh:=pobjectdef(pd);
                                    sym:=nil;
                                    while assigned(classh) do
                                      begin
                                         sym:=pvarsym(classh^.publicsyms^.search(pattern));
                                         srsymtable:=classh^.publicsyms;
                                         if assigned(sym) then
                                           break;
                                         classh:=classh^.childof;
                                      end;
                                    consume(ID);
                                    do_member_read(sym,p1,pd,again);
                                 end;
                           pointerdef:
                              begin
                                 if ppointerdef(pd)^.definition^.deftype
                                    in [recorddef,objectdef,classrefdef] then
                                    begin
                                       Message(cg_e_invalid_qualifier);
                                       { exterror:=strpnew(' may be pointer deref ^ is missing');
                                       error(invalid_qualifizier); }
                                       Comment(V_hint,' may be pointer deref ^ is missing');
                                    end
                                 else
                                    Message(cg_e_invalid_qualifier);
                              end
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
                          if  (pd^.deftype=procvardef) then
                          begin
                             if getprocvar then
                               again:=false
                             else
                             if (token=LKLAMMER) or
                                ((pprocvardef(pd)^.para1=nil) and
                                (token<>ASSIGNMENT) and (not in_args)) then
                               begin
                                  { do this in a strange way  }
                                  { it's not a clean solution }
                                  p2:=p1;
                                  p1:=gencallnode(nil,
                                    nil);
                                  p1^.right:=p2;
                                  p1^.unit_specific:=unit_specific;
                                  if token=LKLAMMER then
                                    begin
                                       consume(LKLAMMER);
                                       p1^.left:=parse_paras(false,false);
                                       consume(RKLAMMER);
                                    end;
                                  pd:=pprocvardef(pd)^.retdef;
                                  p1^.resulttype:=pd;
                               end
                             else again:=false;
                             p1^.resulttype:=pd;
                          end
                          else again:=false;
                        end
                        else again:=false;
                     end;
                end;
{$ifdef UseTokenInfo}
             check_tokenpos;
{$endif UseTokenInfo}
           end;
      end;

    procedure do_set(p : pconstset;pos : longint);

      var
         l : longint;

      begin
         if (pos>255) or
            (pos<0) then
           Message(parser_e_illegal_set_expr);
         l:=pos div 8;
         { do we allow the same twice }
         if (p^[l] and (1 shl (pos mod 8)))<>0 then
           Message(parser_e_illegal_set_expr);
         p^[l]:=p^[l] or (1 shl (pos mod 8));
      end;

      var
         possible_error : boolean;

      begin
{$ifdef UseTokenInfo}
         oldp1:=nil;
         filepos:=tokenpos;
{$endif UseTokenInfo}
         case token of
            ID:
              begin
                 { allow post fix operators }
                 again:=true;
                 if (cs_delphi2_compatible in aktswitches) and
                    (pattern='RESULT') and
                   assigned(aktprocsym) and
                   (procinfo.retdef<>pdef(voiddef)) then
                   begin
                      consume(ID);
                      p1:=genzeronode(funcretn);
                      pd:=procinfo.retdef;
{$ifdef TEST_FUNCRET}
                      p1^.funcretprocinfo:=pointer(@procinfo);
                      p1^.retdef:=pd;
{$endif TEST_FUNCRET}
                   end
                 else
                   begin
                      getsym(pattern,true);
                      consume(ID);
                      { is this an access to a function result ? }
                       if assigned(aktprocsym) and
                        ((srsym^.name=aktprocsym^.name) or
                        ((pvarsym(srsym)=opsym) and
                        ((procinfo.flags and pi_operator)<>0))) and
                        (procinfo.retdef<>pdef(voiddef)) and
                        (token<>LKLAMMER) and
                        (not ((cs_tp_compatible in aktswitches) and
                        (afterassignment or in_args))) then
                        begin
                           p1:=genzeronode(funcretn);
                           pd:=procinfo.retdef;
{$ifdef TEST_FUNCRET}
                           p1^.funcretprocinfo:=pointer(@procinfo);
                           p1^.retdef:=pd;
{$endif TEST_FUNCRET}
                        end
                      else
                        { else it's a normal symbol }
                        begin
                           if srsym^.typ=unitsym then
                             begin
                                consume(POINT);
                                getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                                unit_specific:=true;
                                consume(ID);
                             end
                           else
                             unit_specific:=false;
                           { check semantics of private }
                           if (srsym^.typ in [propertysym,procsym,varsym]) and
                              (srsymtable^.symtabletype=objectsymtable) then
                             begin
                                if ((srsym^.properties and sp_private)<>0) and
                                  (pobjectdef(srsym^.owner^.defowner)^.
                                  owner^.symtabletype=unitsymtable) then
                                  Message(parser_e_cant_access_private_member);
                             end;
                           case srsym^.typ of
                              absolutesym:
                                begin
                                   p1:=genloadnode(pvarsym(srsym),srsymtable);
                                   pd:=pabsolutesym(srsym)^.definition;
                                end;
                              varsym:
                                begin
                                   { are we in a class method ? }
                                   if (srsymtable^.symtabletype=objectsymtable) and
                                     assigned(aktprocsym) and
                                     ((aktprocsym^.definition^.options and poclassmethod)<>0) then
                                     Message(parser_e_only_class_methods);

                                     if (srsym^.properties and sp_static)<>0 then
                                       begin
                                          static_name:=lowercase(srsymtable^.name^)+'_'+srsym^.name;
                                          getsym(static_name,true);
                                       end;
                                     p1:=genloadnode(pvarsym(srsym),srsymtable);
                                     if pvarsym(srsym)^.is_valid=0 then
                                       begin
                                          p1^.is_first := true;
                                          { set special between first loaded
                                            until checked in firstpass }
                                          pvarsym(srsym)^.is_valid:=2;
                                       end;
                                     pd:=pvarsym(srsym)^.definition;
                                  end;
                              typedconstsym:
                                begin
                                   p1:=gentypedconstloadnode(ptypedconstsym(srsym),srsymtable);
                                   pd:=ptypedconstsym(srsym)^.definition;
                                end;
                              syssym:
                                p1:=statement_syssym(psyssym(srsym)^.number,pd);
                              typesym:
                                begin
                                   pd:=ptypesym(srsym)^.definition;
                                   { if we read a type declaration  }
                                   { we have to return the type and }
                                   { nothing else                   }
                                   if block_type=bt_type then
                                     begin
                                        p1:=genzeronode(typen);
                                        p1^.resulttype:=pd;
                                        pd:=voiddef;
                                     end
                                   else
                                     begin
                                        if token=LKLAMMER then
                                          begin
                                             consume(LKLAMMER);
                                             p1:=comp_expr(true);
                                             consume(RKLAMMER);
                                             p1:=gentypeconvnode(p1,pd);
                                             p1^.explizit:=true;
                                          end
                                        else if (token=POINT) and
                                          (pd^.deftype=objectdef) and
                                          ((pobjectdef(pd)^.options and oois_class)=0) then
                                          begin
                                             consume(POINT);
                                             if assigned(procinfo._class) then
                                               begin
                                                  if procinfo._class^.isrelated(pobjectdef(pd)) then
                                                    begin
                                                       p1:=genzeronode(typen);
                                                       p1^.resulttype:=pd;
                                                       srsymtable:=pobjectdef(pd)^.publicsyms;
                                                       sym:=pvarsym(srsymtable^.search(pattern));
                                                       consume(ID);
                                                       do_member_read(sym,p1,pd,again);
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
                                                   srsymtable:=pobjectdef(pd)^.publicsyms;
                                                   sym:=pvarsym(srsymtable^.search(pattern));
                                                   if not(getaddr) and
                                                     ((sym^.properties and sp_static)=0) then
                                                     Message(sym_e_only_static_in_static)
                                                   else
                                                     begin
                                                        consume(ID);
                                                        do_member_read(sym,p1,pd,again);
                                                     end;
                                               end
                                          end
                                        else
                                          begin
                                             { class reference ? }
                                             if (pd^.deftype=objectdef)
                                               and ((pobjectdef(pd)^.options and oois_class)<>0) then
                                               begin
                                                  p1:=genzeronode(typen);
                                                  p1^.resulttype:=pd;
                                                  pd:=new(pclassrefdef,init(pd));
                                                  p1:=gensinglenode(loadvmtn,p1);
                                                  p1^.resulttype:=pd;
                                               end
                                             else
                                               begin
                                                  { generate a type node }
                                                  { (for typeof etc)     }
                                                  p1:=genzeronode(typen);
                                                  p1^.resulttype:=pd;
                                                  pd:=voiddef;
                                               end;
                                          end;
                                     end;
                                end;
                              enumsym:
                                begin
                                   p1:=genenumnode(penumsym(srsym));
                                   pd:=p1^.resulttype;
                                end;
                              constsym:
                                begin
                                   case pconstsym(srsym)^.consttype of
                                      constint:
                                        p1:=genordinalconstnode(pconstsym(srsym)^.value,s32bitdef);
                                      conststring:
                                        p1:=genstringconstnode(pstring(pconstsym(srsym)^.value)^);
                                      constchar:
                                        p1:=genordinalconstnode(pconstsym(srsym)^.value,cchardef);
                                      constreal:
                                        p1:=genrealconstnode(pdouble(pconstsym(srsym)^.value)^);
                                      constbool:
                                        p1:=genordinalconstnode(pconstsym(srsym)^.value,booldef);
                                      constseta:
                                        p1:=gensetconstruktnode(pconstset(pconstsym(srsym)^.value),
                                          psetdef(pconstsym(srsym)^.definition));
                                      constord:
                                        p1:=genordinalconstnode(pconstsym(srsym)^.value,
                                          pconstsym(srsym)^.definition);
                                   end;
                                   pd:=p1^.resulttype;
                                end;
                              procsym:
                                begin
                                   { are we in a class method ? }
                                   possible_error:=(srsymtable^.symtabletype=objectsymtable) and
                                     assigned(aktprocsym) and
                                     ((aktprocsym^.definition^.options and poclassmethod)<>0);
                                   p1:=gencallnode(pprocsym(srsym),srsymtable);
                                   p1^.unit_specific:=unit_specific;
                                   do_proc_call(getaddr,again,p1,pd);
                                   if possible_error and
                                     ((p1^.procdefinition^.options and poclassmethod)=0) then
                                     Message(parser_e_only_class_methods);
                                end;
                              propertysym:
                                begin
                                   { access to property in a method }

                                   { are we in a class method ? }
                                   if (srsymtable^.symtabletype=objectsymtable) and
                                     assigned(aktprocsym) and
                                     ((aktprocsym^.definition^.options and poclassmethod)<>0) then
                                     Message(parser_e_only_class_methods);
                                   { no method pointer }
                                   p1:=nil;
                                   handle_propertysym(srsym,p1,pd);
                                end;
                              errorsym:
                                begin
                                   p1:=genzeronode(errorn);
                                   pd:=generrordef;
                                   if token=LKLAMMER then
                                     begin
                                        consume(LKLAMMER);
                                        parse_paras(false,false);
                                        consume(RKLAMMER);
                                     end;
                                end;
                              else
                                begin
                                   p1:=genzeronode(errorn);
                                   pd:=generrordef;
                                   Message(cg_e_illegal_expression);
                                end;
                           end;
                        end;
                   end;
                 { handle post fix operators }
                 postfixoperators;
              end;
            _NEW : begin
                      consume(_NEW);
                      consume(LKLAMMER);
                      p1:=factor(false);
                      if p1^.treetype<>typen then
                        Message(sym_e_type_id_expected);
                      pd:=p1^.resulttype;
                      pd2:=pd;
                      if (pd^.deftype<>pointerdef) or
                         (ppointerdef(pd)^.definition^.deftype<>objectdef) then
                        begin
                           Message(parser_e_pointer_to_class_expected);

                           { if an error occurs, read til the end of the new }
                           { statement                                       }
                           p1:=genzeronode(errorn);
                           l:=1;
                           while true do
                             begin
                                case token of
                                   LKLAMMER : inc(l);
                                   RKLAMMER : dec(l);
                                end;
                                consume(token);
                                if l=0 then
                                  break;
                             end;
                        end
                      else
                        begin
                           disposetree(p1);
                           p1:=genzeronode(hnewn);
                           p1^.resulttype:=ppointerdef(pd)^.definition;
                           consume(COMMA);

                           afterassignment:=false;
                           { determines the current object defintion }
                           classh:=pobjectdef(ppointerdef(pd)^.definition);

                           { check for an abstract class }
                           if (classh^.options and oois_abstract)<>0 then
                             Message(sym_e_no_instance_of_abstract_object);

                           { search the constructor also in the symbol tables of }
                           { the parents                                          }

                           { no constructor found }
                           sym:=nil;
                           while assigned(classh) do
                             begin
                                sym:=pvarsym(classh^.publicsyms^.search(pattern));
                                srsymtable:=classh^.publicsyms;
                                if assigned(sym) then
                                  break;
                                classh:=classh^.childof;
                             end;

                           consume(ID);
                           do_member_read(sym,p1,pd,again);
                           if (p1^.treetype<>calln) or
                              (assigned(p1^.procdefinition) and
                               ((p1^.procdefinition^.options and poconstructor)=0)) then
                             Message(parser_e_expr_have_to_be_constructor_call);
                           p1:=gensinglenode(newn,p1);

                           { set the resulttype }
                           p1^.resulttype:=pd2;
                           consume(RKLAMMER);
                        end;
                   end;
            _SELF:
              begin
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
                      if (aktprocsym^.definition^.options and poclassmethod)<>0 then
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
                                      srsymtable:=pobjectdef(classh)^.publicsyms;
                                      sym:=pvarsym(srsymtable^.search(pattern));
                                      if assigned(sym) then
                                        begin
                                           p1:=genzeronode(typen);
                                           p1^.resulttype:=classh;
                                           pd:=p1^.resulttype;
                                           consume(ID);
                                           do_member_read(sym,p1,pd,again);
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
                              Message(parser_e_generic_methods_only_in_methods);
                            postfixoperators;
                         end;
            INTCONST : begin
                          valint(pattern,l,code);
                          if code<>0 then
                            begin
                               val(pattern,d,code);
                               if code<>0 then
                                 begin
                                    Message(cg_e_invalid_integer);
                                    l:=1;
                                    consume(INTCONST);
                                    p1:=genordinalconstnode(l,s32bitdef);
                                 end
                               else
                                 begin
                                    consume(INTCONST);
                                    p1:=genrealconstnode(d);
                                 end;
                            end
                          else
                            begin
                               consume(INTCONST);
                               p1:=genordinalconstnode(l,s32bitdef);
                            end;
                       end;
            REALNUMBER : begin
                          val(pattern,d,code);
                          if code<>0 then
                            begin
                               Message(parser_e_error_in_real);
                               d:=1.0;
                            end;
                          consume(REALNUMBER);
                          p1:=genrealconstnode(d);
                        end;
            { FILE and STRING can be also a type cast }
            _STRING:
              begin
                 pd:=stringtype;
                 consume(LKLAMMER);
                 p1:=comp_expr(true);
                 consume(RKLAMMER);
                 p1:=gentypeconvnode(p1,pd);
                 p1^.explizit:=true;
                 { handle postfix operators here e.g. string(a)[10] }
                 again:=true;
                 postfixoperators;
              end;
            _FILE:
              begin
                 pd:=cfiledef;
                 consume(_FILE);
                 consume(LKLAMMER);
                 p1:=comp_expr(true);
                 consume(RKLAMMER);
                 p1:=gentypeconvnode(p1,pd);
                 p1^.explizit:=true;
                 { handle postfix operators here e.g. string(a)[10] }
                 again:=true;
                 postfixoperators;
              end;
            CSTRING:
              begin
                 p1:=genstringconstnode(pattern);
                 consume(CSTRING);
              end;
            CCHAR:
              begin
                 p1:=genordinalconstnode(ord(pattern[1]),cchardef);
                 consume(CCHAR);
              end;
            KLAMMERAFFE : begin
                             consume(KLAMMERAFFE);
                             p1:=factor(true);
                             p1:=gensinglenode(addrn,p1);
                          end;
            LKLAMMER : begin
                          consume(LKLAMMER);
                          p1:=comp_expr(true);
                          consume(RKLAMMER);
                          { it's not a good solution        }
                          { but (a+b)^ makes some problems  }
                          case token of
                             CARET,POINT,LECKKLAMMER:
                               begin
                                  { we need the resulttype  }
                                  { of the expression in pd }
                                  do_firstpass(p1);
                                  pd:=p1^.resulttype;

                                  again:=true;
                                  postfixoperators;
                               end;
                          end;
                       end;
            LECKKLAMMER : begin
                             consume(LECKKLAMMER);
                             new(constset);
                             for l:=0 to 31 do
                               constset^[l]:=0;
                             p2:=nil;
                             pd:=nil;
                             if token<>RECKKLAMMER then
                               while true do
                                 begin
                                    p1:=comp_expr(true);
                                    do_firstpass(p1);
                                    case p1^.treetype of
                                       ordconstn : begin
                                                      if pd=nil then
                                                        pd:=p1^.resulttype;
                                                     if not(is_equal(pd,p1^.resulttype)) then
                                                       Message(parser_e_typeconflict_in_set)
                                                     else
                                                       if token=POINTPOINT then
                                                         begin
                                                            consume(POINTPOINT);
                                                            p3:=comp_expr(true);
                                                            do_firstpass(p3);
                                                            if not(is_equal(pd,p3^.resulttype)) then
                                                              Message(parser_e_typeconflict_in_set)
                                                            else
                                                              for l:=p1^.value to p3^.value do
                                                                do_set(constset,l);
                                                            disposetree(p3);
                                                         end
                                                       else
                                                         do_set(constset,p1^.value);
                                                     disposetree(p1);
                                                   end;
                                       stringconstn : begin
                                                         if pd=nil then
                                                           pd:=cchardef;
                                                   if not(is_equal(pd,cchardef)) then
                                                     Message(parser_e_typeconflict_in_set)
                                                   else
                                                     for l:=1 to length(pstring(p1^.values)^) do
                                                       do_set(constset,ord(pstring(p1^.values)^[l]));
                                                   disposetree(p1);
                                                end;
                                       else
                                          begin
                                             if pd=nil then
                                               pd:=p1^.resulttype;
                                             if not(is_equal(pd,p1^.resulttype)) then
                                               Message(parser_e_typeconflict_in_set);
                                             p2:=gennode(setelen,p1,p2);
                                          end;
                                    end;
                                    if token=COMMA then
                                      consume(COMMA)
                                    else break;
                                 end;
                             consume(RECKKLAMMER);
                             p1:=gensinglenode(setconstrn,p2);
                             p1^.resulttype:=new(psetdef,init(pd,255));
                             p1^.constset:=constset;
                          end;
            PLUS     : begin
                          consume(PLUS);
                          p1:=factor(false);
                       end;
            MINUS    : begin
                          consume(MINUS);
                          p1:=factor(false);
                          p1:=gensinglenode(umminusn,p1);
                       end;
            _NOT     : begin
                          consume(_NOT);
                          p1:=factor(false);
                          p1:=gensinglenode(notn,p1);
                       end;
            _TRUE    : begin
                          consume(_TRUE);
                          p1:=genordinalconstnode(1,booldef);
                       end;
            _FALSE    : begin
                          consume(_FALSE);
                          p1:=genordinalconstnode(0,booldef);
                       end;
            _NIL      : begin
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
         factor:=p1;
{$ifdef UseTokenInfo}
         check_tokenpos;
{$endif UseTokenInfo}
      end;

    type    Toperator_precedence=(opcompare,opaddition,opmultiply);

    const   tok2node:array[PLUS.._XOR] of Ttreetyp=
                    (addn,subn,muln,slashn,equaln,gtn,ltn,gten,lten,
                     isn,asn,inn,
                     symdifn,starstarn,nothingn,caretn,unequaln,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,andn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,divn,nothingn,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     modn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,orn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,shln,shrn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,nothingn,nothingn,nothingn,nothingn,
                     nothingn,xorn);
            operator_levels:array[Toperator_precedence] of set of Ttoken=
                    ([LT,LTE,GT,GTE,EQUAL,UNEQUAL,_IN,_IS],
                     [PLUS,MINUS,_OR,_XOR],
                     [CARET,SYMDIF,STARSTAR,STAR,SLASH,_DIV,_MOD,_AND,_SHL,_SHR,_AS]);

    function sub_expr(pred_level:Toperator_precedence;accept_equal : boolean):Ptree;

    {Reads a subexpression while the operators are of the current precedence
     level, or any higher level. Replaces the old term, simpl_expr and
     simpl2_expr.}

    var p1,p2:Ptree;
        oldt:Ttoken;
{$ifdef UseTokenInfo}
         filepos : tfileposinfo;
{$endif UseTokenInfo}


    begin
{        if pred_level=high(Toperator_precedence) then }
         if pred_level=opmultiply then
            p1:=factor(getprocvar)
        else
            p1:=sub_expr(succ(pred_level),true);
        repeat
            { aweful hack to support const a : 1..2=1; }
            { disadvantage of tables :) FK             }
            if (token in operator_levels[pred_level]) and
               ((token<>EQUAL) or accept_equal) then
                begin
                    oldt:=token;
{$ifdef UseTokenInfo}
                    filepos:=tokenpos;
{$endif UseTokenInfo}

                    consume(token);
{                    if pred_level=high(Toperator_precedence) then }
                    if pred_level=opmultiply then
                        p2:=factor(getprocvar)
                    else
                        p2:=sub_expr(succ(pred_level),true);
                    p1:=gennode(tok2node[oldt],p1,p2);
{$ifdef UseTokenInfo}
                    set_tree_filepos(p1,filepos);
{$endif UseTokenInfo}

                end
            else
                break;
        until false;
        sub_expr:=p1;
    end;

    function comp_expr(accept_equal : boolean):Ptree;

      var
         oldafterassignment : boolean;

      begin
         oldafterassignment:=afterassignment;
         afterassignment:=true;
         comp_expr:=sub_expr(opcompare,accept_equal);
         afterassignment:=oldafterassignment;
      end;

    function expr : ptree;

      var
         p1,p2 : ptree;
         oldafterassignment : boolean;
{$ifdef UseTokenInfo}
         oldp1 : ptree;
         filepos : tfileposinfo;
{$endif UseTokenInfo}

      begin
         oldafterassignment:=afterassignment;
         p1:=sub_expr(opcompare,true);
         if token in [ASSIGNMENT,_PLUSASN,_MINUSASN,_STARASN,_SLASHASN] then
           afterassignment:=true;
{$ifdef UseTokenInfo}
         filepos:=tokenpos;
         oldp1:=p1;
{$endif UseTokenInfo}
         case token of
            POINTPOINT : begin
                            consume(POINTPOINT);
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(rangen,p1,p2);
                         end;
            ASSIGNMENT : begin
                            consume(ASSIGNMENT);
                            { avoid a firstpass of a procedure if
                            it must be assigned to a procvar }
                            { should be recursive for a:=b:=c !!! }
                            if (p1^.resulttype<>nil) and (p1^.resulttype^.deftype=procvardef) then
                              getprocvar:=true;
                            p2:=sub_expr(opcompare,true);
                            if getprocvar and (p2^.treetype=calln) then
                              begin
                                 p2^.treetype:=loadn;
                                 p2^.resulttype:=pprocsym(p2^.symtableprocentry)^.definition;
                                 p2^.symtableentry:=p2^.symtableprocentry;
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
{$ifdef UseTokenInfo}
         if p1<>oldp1 then
           set_tree_filepos(p1,filepos);
{$endif UseTokenInfo}
         expr:=p1;
      end;

    function get_intconst:longint;

    {Reads an expression, tries to evalute it and check if it is an integer
     constant. Then the constant is returned.}

    var p:Ptree;

    begin
        p:=comp_expr(true);
        do_firstpass(p);
        if (p^.treetype<>ordconstn) and
         (p^.resulttype^.deftype=orddef) and
         not (Porddef(p^.resulttype)^.typ in
         [uvoid,uchar,bool8bit]) then
            Message(cg_e_illegal_expression)
        else
            get_intconst:=p^.value;
        disposetree(p);
    end;

    function get_stringconst:string;

    {Reads an expression, tries to evaluate it and checks if it is a string
     constant. Then the constant is returned.}

    var p:Ptree;

    begin
        get_stringconst:='';
        p:=comp_expr(true);
        do_firstpass(p);
        if p^.treetype<>stringconstn then
            if (p^.treetype=ordconstn) and
             (p^.resulttype^.deftype=orddef) and
             (Porddef(p^.resulttype)^.typ=uchar) then
                get_stringconst:=char(p^.value)
            else
                Message(cg_e_illegal_expression)
        else
            get_stringconst:=p^.values^;
        disposetree(p);
    end;

end.
{
  $Log$
  Revision 1.14  1998-05-11 13:07:56  peter
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

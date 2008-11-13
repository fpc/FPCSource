{
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
      symtype,symdef,symbase,
      node,ncal,
      tokens,globtype,globals;

    { reads a whole expression }
    function expr : tnode;

    { reads an expression without assignements and .. }
    function comp_expr(accept_equal : boolean):tnode;

    { reads a single factor }
    function factor(getaddr : boolean) : tnode;

    procedure string_dec(var def: tdef);

    procedure propaccesslist_to_node(var p1:tnode;st:TSymtable;pl:tpropaccesslist);

    function node_to_propaccesslist(p1:tnode):tpropaccesslist;

    function parse_paras(__colon,__namedpara : boolean;end_of_paras : ttoken) : tnode;

    { the ID token has to be consumed before calling this function }
    procedure do_member_read(classh:tobjectdef;getaddr : boolean;sym : tsym;var p1 : tnode;var again : boolean;callflags:tcallnodeflags);

    function get_intconst:TConstExprInt;
    function get_stringconst:string;

implementation

    uses
       { common }
       cutils,
       { global }
       verbose,
       systems,widestr,
       { symtable }
       symconst,symtable,symsym,defutil,defcmp,
       { module }
       fmodule,ppu,
       { pass 1 }
       pass_1,htypechk,
       nmat,nadd,nmem,nset,ncnv,ninl,ncon,nld,nflw,nbas,nutils,
       { parser }
       scanner,
       pbase,pinline,ptype,
       { codegen }
       cgbase,procinfo,cpuinfo
       ;

    { sub_expr(opmultiply) is need to get -1 ** 4 to be
      read as - (1**4) and not (-1)**4 PM }
    type
      Toperator_precedence=(opcompare,opaddition,opmultiply,oppower);

    const
      highest_precedence = oppower;

    function sub_expr(pred_level:Toperator_precedence;accept_equal : boolean):tnode;forward;

    const
       { true, if the inherited call is anonymous }
       anon_inherited : boolean = false;
       { last def found, only used by anon. inherited calls to insert proper type casts }
       srdef : tdef = nil;

    procedure string_dec(var def:tdef);
    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
      var
         p : tnode;
      begin
         def:=cshortstringtype;
         consume(_STRING);
         if try_to_consume(_LECKKLAMMER) then
           begin
              p:=comp_expr(true);
              if not is_constintnode(p) then
                begin
                  Message(parser_e_illegal_expression);
                  { error recovery }
                  consume(_RECKKLAMMER);
                end
              else
                begin
                 if (tordconstnode(p).value<=0) then
                   begin
                      Message(parser_e_invalid_string_size);
                      tordconstnode(p).value:=255;
                   end;
                 consume(_RECKKLAMMER);
                 if tordconstnode(p).value>255 then
                  begin
                    { longstring is currently unsupported (CEC)! }
{                   t:=tstringdef.createlong(tordconstnode(p).value))}
                     Message(parser_e_invalid_string_size);
                     tordconstnode(p).value:=255;
                     def:=tstringdef.createshort(tordconstnode(p).value);
                  end
                 else
                   if tordconstnode(p).value<>255 then
                     def:=tstringdef.createshort(tordconstnode(p).value);
               end;
              p.free;
           end
          else
            begin
               if cs_ansistrings in current_settings.localswitches then
                 def:=cansistringtype
               else
                 def:=cshortstringtype;
            end;
       end;


    procedure propaccesslist_to_node(var p1:tnode;st:TSymtable;pl:tpropaccesslist);
      var
        plist : ppropaccesslistitem;
      begin
        plist:=pl.firstsym;
        while assigned(plist) do
         begin
           case plist^.sltype of
             sl_load :
               begin
                 addsymref(plist^.sym);
                 if not assigned(st) then
                   st:=plist^.sym.owner;
                 { p1 can already contain the loadnode of
                   the class variable. When there is no tree yet we
                   may need to load it for with or objects }
                 if not assigned(p1) then
                  begin
                    case st.symtabletype of
                      withsymtable :
                        p1:=tnode(twithsymtable(st).withrefnode).getcopy;
                      ObjectSymtable :
                        p1:=load_self_node;
                    end;
                  end;
                 if assigned(p1) then
                  p1:=csubscriptnode.create(plist^.sym,p1)
                 else
                  p1:=cloadnode.create(plist^.sym,st);
               end;
             sl_subscript :
               begin
                 addsymref(plist^.sym);
                 p1:=csubscriptnode.create(plist^.sym,p1);
               end;
             sl_typeconv :
               p1:=ctypeconvnode.create_explicit(p1,plist^.def);
             sl_absolutetype :
               begin
                 p1:=ctypeconvnode.create(p1,plist^.def);
                 include(p1.flags,nf_absolute);
               end;
             sl_vec :
               p1:=cvecnode.create(p1,cordconstnode.create(plist^.value,plist^.valuedef,true));
             else
               internalerror(200110205);
           end;
           plist:=plist^.next;
         end;
      end;


    function node_to_propaccesslist(p1:tnode):tpropaccesslist;
      var
        sl : tpropaccesslist;

        procedure addnode(p:tnode);
        begin
          case p.nodetype of
            subscriptn :
              begin
                addnode(tsubscriptnode(p).left);
                sl.addsym(sl_subscript,tsubscriptnode(p).vs);
              end;
            typeconvn :
              begin
                addnode(ttypeconvnode(p).left);
                if nf_absolute in ttypeconvnode(p).flags then
                  sl.addtype(sl_absolutetype,ttypeconvnode(p).totypedef)
                else
                  sl.addtype(sl_typeconv,ttypeconvnode(p).totypedef);
              end;
            vecn :
              begin
                addnode(tvecnode(p).left);
                if tvecnode(p).right.nodetype=ordconstn then
                  sl.addconst(sl_vec,tordconstnode(tvecnode(p).right).value,tvecnode(p).right.resultdef)
                else
                  begin
                    Message(parser_e_illegal_expression);
                    { recovery }
                    sl.addconst(sl_vec,0,tvecnode(p).right.resultdef);
                  end;
             end;
            loadn :
              sl.addsym(sl_load,tloadnode(p).symtableentry);
            else
              internalerror(200310282);
          end;
        end;

      begin
        sl:=tpropaccesslist.create;
        addnode(p1);
        result:=sl;
      end;


    function parse_paras(__colon,__namedpara : boolean;end_of_paras : ttoken) : tnode;
      var
         p1,p2,argname : tnode;
         prev_in_args,
         old_named_args_allowed,
         old_allow_array_constructor : boolean;
      begin
         if token=end_of_paras then
           begin
              parse_paras:=nil;
              exit;
           end;
         { save old values }
         prev_in_args:=in_args;
         old_allow_array_constructor:=allow_array_constructor;
         old_named_args_allowed:=named_args_allowed;
         { set para parsing values }
         in_args:=true;
         named_args_allowed:=false;
         inc(parsing_para_level);
         allow_array_constructor:=true;
         p2:=nil;
         repeat
           if __namedpara then
             begin
               if token=_COMMA then
                 begin
                   { empty parameter }
                   p2:=ccallparanode.create(cnothingnode.create,p2);
                 end
               else
                 begin
                   named_args_allowed:=true;
                   p1:=comp_expr(true);
                   named_args_allowed:=false;
                   if found_arg_name then
                     begin
                       argname:=p1;
                       p1:=comp_expr(true);
                       p2:=ccallparanode.create(p1,p2);
                       tcallparanode(p2).parametername:=argname;
                     end
                   else
                     p2:=ccallparanode.create(p1,p2);
                   found_arg_name:=false;
                 end;
             end
           else
             begin
               p1:=comp_expr(true);
               p2:=ccallparanode.create(p1,p2);
             end;
           { it's for the str(l:5,s); }
           if __colon and (token=_COLON) then
             begin
               consume(_COLON);
               p1:=comp_expr(true);
               p2:=ccallparanode.create(p1,p2);
               include(tcallparanode(p2).callparaflags,cpf_is_colon_para);
               if try_to_consume(_COLON) then
                 begin
                   p1:=comp_expr(true);
                   p2:=ccallparanode.create(p1,p2);
                   include(tcallparanode(p2).callparaflags,cpf_is_colon_para);
                 end
             end;
         until not try_to_consume(_COMMA);
         allow_array_constructor:=old_allow_array_constructor;
         dec(parsing_para_level);
         in_args:=prev_in_args;
         named_args_allowed:=old_named_args_allowed;
         parse_paras:=p2;
      end;


     function gen_c_style_operator(ntyp:tnodetype;p1,p2:tnode) : tnode;
       var
         hp    : tnode;
         hdef  : tdef;
         temp  : ttempcreatenode;
         newstatement : tstatementnode;
       begin
         { Properties are not allowed, because the write can
           be different from the read }
         if (nf_isproperty in p1.flags) then
           begin
             Message(type_e_variable_id_expected);
             { We can continue with the loading,
               it'll not create errors. Only the expected
               result can be wrong }
           end;

         hp:=p1;
         while assigned(hp) and
               (hp.nodetype in [derefn,subscriptn,vecn,typeconvn]) do
           hp:=tunarynode(hp).left;
         if not assigned(hp) then
           internalerror(200410121);
         if (hp.nodetype=calln) then
           begin
             typecheckpass(p1);
             result:=internalstatements(newstatement);
             hdef:=tpointerdef.create(p1.resultdef);
             temp:=ctempcreatenode.create(hdef,sizeof(aint),tt_persistent,false);
             addstatement(newstatement,temp);
             addstatement(newstatement,cassignmentnode.create(ctemprefnode.create(temp),caddrnode.create_internal(p1)));
             addstatement(newstatement,cassignmentnode.create(
                 cderefnode.create(ctemprefnode.create(temp)),
                 caddnode.create(ntyp,
                     cderefnode.create(ctemprefnode.create(temp)),
                     p2)));
             addstatement(newstatement,ctempdeletenode.create(temp));
           end
         else
           result:=cassignmentnode.create(p1,caddnode.create(ntyp,p1.getcopy,p2));
       end;


     function statement_syssym(l : byte) : tnode;
      var
        p1,p2,paras  : tnode;
        err,
        prev_in_args : boolean;
      begin
        prev_in_args:=in_args;
        case l of

          in_new_x :
            begin
              if afterassignment or in_args then
               statement_syssym:=new_function
              else
               statement_syssym:=new_dispose_statement(true);
            end;

          in_dispose_x :
            begin
              statement_syssym:=new_dispose_statement(false);
            end;

          in_ord_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_RKLAMMER);
              p1:=geninlinenode(in_ord_x,false,p1);
              statement_syssym := p1;
            end;

          in_exit :
            begin
              if try_to_consume(_LKLAMMER) then
                begin
                  if not (m_mac in current_settings.modeswitches) then
                    begin
                      if not(try_to_consume(_RKLAMMER)) then
                        begin
                          p1:=comp_expr(true);
                          consume(_RKLAMMER);
                          if (block_type=bt_except) then
                            begin
                              Message(parser_e_exit_with_argument_not__possible);
                              { recovery }
                              p1.free;
                              p1:=nil;
                            end
                          else if (not assigned(current_procinfo) or
                              is_void(current_procinfo.procdef.returndef)) then
                            begin
                              Message(parser_e_void_function);
                              { recovery }
                              p1.free;
                              p1:=nil;
                            end;
                        end
                      else
                        p1:=nil;
                    end
                  else
                    begin
                      if not (current_procinfo.procdef.procsym.name = pattern) then
                        Message(parser_e_macpas_exit_wrong_param);
                      consume(_ID);
                      consume(_RKLAMMER);
                      p1:=nil;
                    end
                end
              else
                p1:=nil;
              statement_syssym:=cexitnode.create(p1);
            end;

          in_break :
            begin
              statement_syssym:=cbreaknode.create
            end;

          in_continue :
            begin
              statement_syssym:=ccontinuenode.create
            end;

          in_leave :
            begin
              if m_mac in current_settings.modeswitches then
                statement_syssym:=cbreaknode.create
              else
                begin
                  Message1(sym_e_id_not_found, orgpattern);
                  statement_syssym:=cerrornode.create;
                end;
            end;

          in_cycle :
            begin
              if m_mac in current_settings.modeswitches then
                statement_syssym:=ccontinuenode.create
              else
                begin
                  Message1(sym_e_id_not_found, orgpattern);
                  statement_syssym:=cerrornode.create;
                end;
            end;

          in_typeof_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_RKLAMMER);
              if p1.nodetype=typen then
                ttypenode(p1).allowed:=true;
              { Allow classrefdef, which is required for
                Typeof(self) in static class methods }
              if (p1.resultdef.typ = objectdef) or
                 (assigned(current_procinfo) and
                  ((po_classmethod in current_procinfo.procdef.procoptions) or
                   (po_staticmethod in current_procinfo.procdef.procoptions)) and
                  (p1.resultdef.typ=classrefdef)) then
               statement_syssym:=geninlinenode(in_typeof_x,false,p1)
              else
               begin
                 Message(parser_e_class_id_expected);
                 p1.destroy;
                 statement_syssym:=cerrornode.create;
               end;
            end;

          in_sizeof_x,
          in_bitsizeof_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_RKLAMMER);
              if (p1.nodetype<>typen) and
                 (
                  (is_object(p1.resultdef) and
                   (oo_has_constructor in tobjectdef(p1.resultdef).objectoptions)) or
                  is_open_array(p1.resultdef) or
                  is_array_of_const(p1.resultdef) or
                  is_open_string(p1.resultdef)
                 ) then
                begin
                  statement_syssym:=geninlinenode(in_sizeof_x,false,p1);
                  { no packed bit support for these things }
                  if (l = in_bitsizeof_x) then
                    statement_syssym:=caddnode.create(muln,statement_syssym,cordconstnode.create(8,sinttype,true));
                end
              else
               begin
                 if (l = in_sizeof_x) or
                    (not((p1.nodetype = vecn) and
                         is_packed_array(tvecnode(p1).left.resultdef)) and
                     not((p1.nodetype = subscriptn) and
                         is_packed_record_or_object(tsubscriptnode(p1).left.resultdef))) then
                   begin
                     statement_syssym:=cordconstnode.create(p1.resultdef.size,sinttype,true);
                     if (l = in_bitsizeof_x) then
                       statement_syssym:=caddnode.create(muln,statement_syssym,cordconstnode.create(8,sinttype,true));
                   end
                 else
                   statement_syssym:=cordconstnode.create(p1.resultdef.packedbitsize,sinttype,true);
                 { p1 not needed !}
                 p1.destroy;
               end;
            end;

          in_typeinfo_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              { When reading a class type it is parsed as loadvmtaddrn,
                typeinfo only needs the type so we remove the loadvmtaddrn }
              if p1.nodetype=loadvmtaddrn then
                begin
                  p2:=tloadvmtaddrnode(p1).left;
                  tloadvmtaddrnode(p1).left:=nil;
                  p1.free;
                  p1:=p2;
                end;
              if p1.nodetype=typen then
                ttypenode(p1).allowed:=true
              else
                begin
                   p1.destroy;
                   p1:=cerrornode.create;
                   Message(parser_e_illegal_parameter_list);
                end;
              consume(_RKLAMMER);
              p2:=geninlinenode(in_typeinfo_x,false,p1);
              statement_syssym:=p2;
            end;

{$ifdef SUPPORT_UNALIGNED}
          in_unaligned_x :
            begin
              err:=false;
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p2:=ccallparanode.create(p1,nil);
              p2:=geninlinenode(in_unaligned_x,false,p2);
              consume(_RKLAMMER);
              statement_syssym:=p2;
            end;
{$endif SUPPORT_UNALIGNED}

          in_assigned_x :
            begin
              err:=false;
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              { When reading a class type it is parsed as loadvmtaddrn,
                typeinfo only needs the type so we remove the loadvmtaddrn }
              if p1.nodetype=loadvmtaddrn then
                begin
                  p2:=tloadvmtaddrnode(p1).left;
                  tloadvmtaddrnode(p1).left:=nil;
                  p1.free;
                  p1:=p2;
                end;
              if not codegenerror then
               begin
                 case p1.resultdef.typ of
                   procdef, { procvar }
                   pointerdef,
                   procvardef,
                   classrefdef : ;
                   objectdef :
                     if not is_class_or_interface(p1.resultdef) then
                       begin
                         Message(parser_e_illegal_parameter_list);
                         err:=true;
                       end;
                   arraydef :
                     if not is_dynamic_array(p1.resultdef) then
                       begin
                         Message(parser_e_illegal_parameter_list);
                         err:=true;
                       end;
                   else
                     begin
                       Message(parser_e_illegal_parameter_list);
                       err:=true;
                     end;
                 end;
               end
              else
               err:=true;
              if not err then
               begin
                 p2:=ccallparanode.create(p1,nil);
                 p2:=geninlinenode(in_assigned_x,false,p2);
               end
              else
               begin
                 p1.free;
                 p2:=cerrornode.create;
               end;
              consume(_RKLAMMER);
              statement_syssym:=p2;
            end;

          in_addr_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p1:=caddrnode.create(p1);
              if cs_typed_addresses in current_settings.localswitches then
                include(p1.flags,nf_typedaddr);
              consume(_RKLAMMER);
              statement_syssym:=p1;
            end;

          in_ofs_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p1:=caddrnode.create(p1);
              do_typecheckpass(p1);
              { Ofs() returns a cardinal/qword, not a pointer }
              p1.resultdef:=uinttype;
              consume(_RKLAMMER);
              statement_syssym:=p1;
            end;

          in_seg_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p1:=geninlinenode(in_seg_x,false,p1);
              consume(_RKLAMMER);
              statement_syssym:=p1;
            end;

          in_high_x,
          in_low_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p2:=geninlinenode(l,false,p1);
              consume(_RKLAMMER);
              statement_syssym:=p2;
            end;

          in_succ_x,
          in_pred_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p2:=geninlinenode(l,false,p1);
              consume(_RKLAMMER);
              statement_syssym:=p2;
            end;

          in_inc_x,
          in_dec_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if try_to_consume(_COMMA) then
                p2:=ccallparanode.create(comp_expr(true),nil)
              else
                p2:=nil;
              p2:=ccallparanode.create(p1,p2);
              statement_syssym:=geninlinenode(l,false,p2);
              consume(_RKLAMMER);
            end;

          in_slice_x:
            begin
              if not(in_args) then
                begin
                  message(parser_e_illegal_slice);
                  consume(_LKLAMMER);
                  in_args:=true;
                  comp_expr(true).free;
                  if try_to_consume(_COMMA) then
                    comp_expr(true).free;
                  statement_syssym:=cerrornode.create;
                  consume(_RKLAMMER);
                end
              else
                begin
                  consume(_LKLAMMER);
                  in_args:=true;
                  p1:=comp_expr(true);
                  Consume(_COMMA);
                  if not(codegenerror) then
                    p2:=ccallparanode.create(comp_expr(true),nil)
                  else
                    p2:=cerrornode.create;
                  p2:=ccallparanode.create(p1,p2);
                  statement_syssym:=geninlinenode(l,false,p2);
                  consume(_RKLAMMER);
                end;
            end;

          in_initialize_x:
            begin
              statement_syssym:=inline_initialize;
            end;

          in_finalize_x:
            begin
              statement_syssym:=inline_finalize;
            end;

          in_copy_x:
            begin
              statement_syssym:=inline_copy;
            end;

          in_concat_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              { Translate to x:=x+y[+z]. The addnode will do the
                type checking }
              p2:=nil;
              repeat
                p1:=comp_expr(true);
                if p2<>nil then
                  p2:=caddnode.create(addn,p2,p1)
                else
                  begin
                    { Force string type if it isn't yet }
                    if not(
                           (p1.resultdef.typ=stringdef) or
                           is_chararray(p1.resultdef) or
                           is_char(p1.resultdef)
                          ) then
                      inserttypeconv(p1,cshortstringtype);
                    p2:=p1;
                  end;
              until not try_to_consume(_COMMA);
              consume(_RKLAMMER);
              statement_syssym:=p2;
            end;

          in_read_x,
          in_readln_x :
            begin
              if try_to_consume(_LKLAMMER) then
               begin
                 paras:=parse_paras(false,false,_RKLAMMER);
                 consume(_RKLAMMER);
               end
              else
               paras:=nil;
              p1:=geninlinenode(l,false,paras);
              statement_syssym := p1;
            end;

          in_setlength_x:
            begin
              statement_syssym := inline_setlength;
            end;

          in_length_x:
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p2:=geninlinenode(l,false,p1);
              consume(_RKLAMMER);
              statement_syssym:=p2;
            end;

          in_write_x,
          in_writeln_x :
            begin
              if try_to_consume(_LKLAMMER) then
               begin
                 paras:=parse_paras(true,false,_RKLAMMER);
                 consume(_RKLAMMER);
               end
              else
               paras:=nil;
              p1 := geninlinenode(l,false,paras);
              statement_syssym := p1;
            end;

          in_str_x_string :
            begin
              consume(_LKLAMMER);
              paras:=parse_paras(true,false,_RKLAMMER);
              consume(_RKLAMMER);
              p1 := geninlinenode(l,false,paras);
              statement_syssym := p1;
            end;

          in_val_x:
            Begin
              consume(_LKLAMMER);
              in_args := true;
              p1:= ccallparanode.create(comp_expr(true), nil);
              consume(_COMMA);
              p2 := ccallparanode.create(comp_expr(true),p1);
              if try_to_consume(_COMMA) then
                p2 := ccallparanode.create(comp_expr(true),p2);
              consume(_RKLAMMER);
              p2 := geninlinenode(l,false,p2);
              statement_syssym := p2;
            End;

          in_include_x_y,
          in_exclude_x_y :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_COMMA);
              p2:=comp_expr(true);
              statement_syssym:=geninlinenode(l,false,ccallparanode.create(p1,ccallparanode.create(p2,nil)));
              consume(_RKLAMMER);
            end;

          in_pack_x_y_z,
          in_unpack_x_y_z :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_COMMA);
              p2:=comp_expr(true);
              consume(_COMMA);
              paras:=comp_expr(true);
              statement_syssym:=geninlinenode(l,false,ccallparanode.create(p1,ccallparanode.create(p2,ccallparanode.create(paras,nil))));
              consume(_RKLAMMER);
            end;

          in_assert_x_y :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if try_to_consume(_COMMA) then
                 p2:=comp_expr(true)
              else
               begin
                 { then insert an empty string }
                 p2:=cstringconstnode.createstr('');
               end;
              statement_syssym:=geninlinenode(l,false,ccallparanode.create(p1,ccallparanode.create(p2,nil)));
              consume(_RKLAMMER);
            end;
          in_get_frame:
            begin
              statement_syssym:=geninlinenode(l,false,nil);
            end;
(*
          in_get_caller_frame:
            begin
              if try_to_consume(_LKLAMMER) then
                begin
                  {You used to call get_caller_frame as get_caller_frame(get_frame),
                   however, as a stack frame may not exist, it does more harm than
                   good, so ignore it.}
                  in_args:=true;
                  p1:=comp_expr(true);
                  p1.destroy;
                  consume(_RKLAMMER);
                end;
              statement_syssym:=geninlinenode(l,false,nil);
            end;
*)
          else
            internalerror(15);

        end;
        in_args:=prev_in_args;
      end;


    function maybe_load_methodpointer(st:TSymtable;var p1:tnode):boolean;
      begin
        maybe_load_methodpointer:=false;
        if not assigned(p1) then
         begin
           case st.symtabletype of
             withsymtable :
               begin
                 if (st.defowner.typ=objectdef) then
                   p1:=tnode(twithsymtable(st).withrefnode).getcopy;
               end;
             ObjectSymtable :
               begin
                 p1:=load_self_node;
                 { We are calling a member }
                 maybe_load_methodpointer:=true;
               end;
           end;
         end;
      end;


    { reads the parameter for a subroutine call }
    procedure do_proc_call(sym:tsym;st:TSymtable;obj:tobjectdef;getaddr:boolean;var again : boolean;var p1:tnode;callflags:tcallnodeflags);
      var
         membercall,
         prevafterassn : boolean;
         i        : integer;
         para,p2  : tnode;
         currpara : tparavarsym;
         aprocdef : tprocdef;
      begin
         prevafterassn:=afterassignment;
         afterassignment:=false;
         membercall:=false;
         aprocdef:=nil;

         { when it is a call to a member we need to load the
           methodpointer first }
         membercall:=maybe_load_methodpointer(st,p1);

         { When we are expecting a procvar we also need
           to get the address in some cases }
         if assigned(getprocvardef) then
          begin
            if (block_type=bt_const) or
               getaddr then
             begin
               aprocdef:=Tprocsym(sym).Find_procdef_byprocvardef(getprocvardef);
               getaddr:=true;
             end
            else
             if (m_tp_procvar in current_settings.modeswitches) or
                (m_mac_procvar in current_settings.modeswitches) then
              begin
                aprocdef:=Tprocsym(sym).Find_procdef_byprocvardef(getprocvardef);
                if assigned(aprocdef) then
                 getaddr:=true;
              end;
          end;

         { only need to get the address of the procedure? }
         if getaddr then
           begin
             { Retrieve info which procvar to call. For tp_procvar the
               aprocdef is already loaded above so we can reuse it }
             if not assigned(aprocdef) and
                assigned(getprocvardef) then
               aprocdef:=Tprocsym(sym).Find_procdef_byprocvardef(getprocvardef);

             { generate a methodcallnode or proccallnode }
             { we shouldn't convert things like @tcollection.load }
             p2:=cloadnode.create_procvar(sym,aprocdef,st);
             if assigned(p1) then
              begin
                { for loading methodpointer of an inherited function
                  we use self as instance and load the address of
                  the function directly and not through the vmt (PFV) }
                if (cnf_inherited in callflags) then
                  begin
                    include(p2.flags,nf_inherited);
                    p1.free;
                    p1:=load_self_node;
                  end;
                if (p1.nodetype<>typen) then
                  tloadnode(p2).set_mp(p1)
                else
                  p1.free;
              end;
             p1:=p2;

             { no postfix operators }
             again:=false;
           end
         else
           begin
             para:=nil;
             if anon_inherited then
              begin
                if not assigned(current_procinfo) then
                  internalerror(200305054);
                for i:=0 to current_procinfo.procdef.paras.count-1 do
                  begin
                    currpara:=tparavarsym(current_procinfo.procdef.paras[i]);
                    if not(vo_is_hidden_para in currpara.varoptions) then
                      begin
                        { inheritance by msgint? }
                        if assigned(srdef) then
                          { anonymous inherited via msgid calls only require a var parameter for
                            both methods, so we need some type casting here }
                          para:=ccallparanode.create(ctypeconvnode.create_internal(ctypeconvnode.create_internal(
                            cloadnode.create(currpara,currpara.owner),cformaltype),tparavarsym(tprocdef(srdef).paras[i]).vardef),
                          para)
                        else
                          para:=ccallparanode.create(cloadnode.create(currpara,currpara.owner),para);
                      end;
                 end;
              end
             else
              begin
                if try_to_consume(_LKLAMMER) then
                 begin
                   para:=parse_paras(false,false,_RKLAMMER);
                   consume(_RKLAMMER);
                 end;
              end;
             { indicate if this call was generated by a member and
               no explicit self is used, this is needed to determine
               how to handle a destructor call (PFV) }
             if membercall then
               include(callflags,cnf_member_call);
             if assigned(obj) then
               begin
                 if (st.symtabletype<>ObjectSymtable) then
                   internalerror(200310031);
                 p1:=ccallnode.create(para,tprocsym(sym),obj.symtable,p1,callflags);
               end
             else
               p1:=ccallnode.create(para,tprocsym(sym),st,p1,callflags);
           end;
         afterassignment:=prevafterassn;
      end;


    procedure handle_procvar(pv : tprocvardef;var p2 : tnode);
      var
        hp,hp2 : tnode;
        hpp    : ^tnode;
        currprocdef : tprocdef;
      begin
        if not assigned(pv) then
         internalerror(200301121);
        if (m_tp_procvar in current_settings.modeswitches) or
           (m_mac_procvar in current_settings.modeswitches) then
         begin
           hp:=p2;
           hpp:=@p2;
           while assigned(hp) and
                 (hp.nodetype=typeconvn) do
            begin
              hp:=ttypeconvnode(hp).left;
              { save orignal address of the old tree so we can replace the node }
              hpp:=@hp;
            end;
           if (hp.nodetype=calln) and
              { a procvar can't have parameters! }
              not assigned(tcallnode(hp).left) then
            begin
              currprocdef:=tcallnode(hp).symtableprocentry.Find_procdef_byprocvardef(pv);
              if assigned(currprocdef) then
               begin
                 hp2:=cloadnode.create_procvar(tprocsym(tcallnode(hp).symtableprocentry),currprocdef,tcallnode(hp).symtableproc);
                 if (po_methodpointer in pv.procoptions) then
                   tloadnode(hp2).set_mp(tcallnode(hp).get_load_methodpointer);
                 hp.destroy;
                 { replace the old callnode with the new loadnode }
                 hpp^:=hp2;
               end;
            end;
         end;
      end;


    { the following procedure handles the access to a property symbol }
    procedure handle_propertysym(propsym : tpropertysym;st : TSymtable;var p1 : tnode);
      var
         paras : tnode;
         p2    : tnode;
         membercall : boolean;
         callflags  : tcallnodeflags;
         propaccesslist : tpropaccesslist;

         function getpropaccesslist(pap:tpropaccesslisttypes):boolean;
         var
           hpropsym : tpropertysym;
         begin
           result:=false;
           { find property in the overriden list }
           hpropsym:=propsym;
           repeat
             propaccesslist:=hpropsym.propaccesslist[pap];
             if not propaccesslist.empty then
               begin
                 result:=true;
                 exit;
               end;
             hpropsym:=hpropsym.overridenpropsym;
           until not assigned(hpropsym);
         end;

      begin
         { property parameters? read them only if the property really }
         { has parameters                                             }
         paras:=nil;
         if (ppo_hasparameters in propsym.propoptions) then
           begin
             if try_to_consume(_LECKKLAMMER) then
               begin
                 paras:=parse_paras(false,false,_RECKKLAMMER);
                 consume(_RECKKLAMMER);
               end;
           end;
         { indexed property }
         if (ppo_indexed in propsym.propoptions) then
           begin
             p2:=cordconstnode.create(propsym.index,propsym.indexdef,true);
             paras:=ccallparanode.create(p2,paras);
           end;
         { we need only a write property if a := follows }
         { if not(afterassignment) and not(in_args) then }
         if token=_ASSIGNMENT then
           begin
              if getpropaccesslist(palt_write) then
                begin
                   case propaccesslist.firstsym^.sym.typ of
                     procsym :
                       begin
                         callflags:=[];
                         { generate the method call }
                         membercall:=maybe_load_methodpointer(st,p1);
                         if membercall then
                           include(callflags,cnf_member_call);
                         p1:=ccallnode.create(paras,tprocsym(propaccesslist.firstsym^.sym),st,p1,callflags);
                         addsymref(propaccesslist.firstsym^.sym);
                         paras:=nil;
                         consume(_ASSIGNMENT);
                         { read the expression }
                         if propsym.propdef.typ=procvardef then
                           getprocvardef:=tprocvardef(propsym.propdef);
                         p2:=comp_expr(true);
                         if assigned(getprocvardef) then
                           handle_procvar(getprocvardef,p2);
                         tcallnode(p1).left:=ccallparanode.create(p2,tcallnode(p1).left);
                         { mark as property, both the tcallnode and the real call block }
                         include(p1.flags,nf_isproperty);
                         getprocvardef:=nil;
                       end;
                     fieldvarsym :
                       begin
                         { generate access code }
                         propaccesslist_to_node(p1,st,propaccesslist);
                         include(p1.flags,nf_isproperty);
                         consume(_ASSIGNMENT);
                         { read the expression }
                         p2:=comp_expr(true);
                         p1:=cassignmentnode.create(p1,p2);
                      end
                    else
                      begin
                        p1:=cerrornode.create;
                        Message(parser_e_no_procedure_to_access_property);
                      end;
                  end;
                end
              else
                begin
                   p1:=cerrornode.create;
                   Message(parser_e_no_procedure_to_access_property);
                end;
           end
         else
           begin
              if getpropaccesslist(palt_read) then
                begin
                   case propaccesslist.firstsym^.sym.typ of
                     fieldvarsym :
                       begin
                          { generate access code }
                          propaccesslist_to_node(p1,st,propaccesslist);
                          include(p1.flags,nf_isproperty);
                       end;
                     procsym :
                       begin
                          callflags:=[];
                          { generate the method call }
                          membercall:=maybe_load_methodpointer(st,p1);
                          if membercall then
                            include(callflags,cnf_member_call);
                          p1:=ccallnode.create(paras,tprocsym(propaccesslist.firstsym^.sym),st,p1,callflags);
                          paras:=nil;
                          include(p1.flags,nf_isproperty);
                       end
                     else
                       begin
                          p1:=cerrornode.create;
                          Message(type_e_mismatch);
                       end;
                  end;
                end
              else
                begin
                   { error, no function to read property }
                   p1:=cerrornode.create;
                   Message(parser_e_no_procedure_to_access_property);
                end;
           end;
        { release paras if not used }
        if assigned(paras) then
         paras.free;
      end;


    { the ID token has to be consumed before calling this function }
    procedure do_member_read(classh:tobjectdef;getaddr : boolean;sym : tsym;var p1 : tnode;var again : boolean;callflags:tcallnodeflags);
      var
         static_name : string;
         isclassref  : boolean;
         srsymtable  : TSymtable;
      begin
         if sym=nil then
           begin
              { pattern is still valid unless
              there is another ID just after the ID of sym }
              Message1(sym_e_id_no_member,orgpattern);
              p1.free;
              p1:=cerrornode.create;
              { try to clean up }
              again:=false;
           end
         else
           begin
              if assigned(p1) then
               begin
                 if not assigned(p1.resultdef) then
                   do_typecheckpass(p1);
                 isclassref:=(p1.resultdef.typ=classrefdef);
               end
              else
               isclassref:=false;

              { we assume, that only procsyms and varsyms are in an object }
              { symbol table, for classes, properties are allowed          }
              case sym.typ of
                 procsym:
                   begin
                      do_proc_call(sym,sym.owner,classh,
                                   (getaddr and not(token in [_CARET,_POINT])),
                                   again,p1,callflags);
                      { we need to know which procedure is called }
                      do_typecheckpass(p1);
                      { calling using classref? }
                      if isclassref and
                         (p1.nodetype=calln) and
                         assigned(tcallnode(p1).procdefinition) and
                         not(po_classmethod in tcallnode(p1).procdefinition.procoptions) and
                         not(tcallnode(p1).procdefinition.proctypeoption=potype_constructor) then
                        Message(parser_e_only_class_methods_via_class_ref);
                   end;
                 fieldvarsym:
                   begin
                      if (sp_static in sym.symoptions) then
                        begin
                           static_name:=lower(sym.owner.name^)+'_'+sym.name;
                           searchsym(static_name,sym,srsymtable);
                           if assigned(sym) then
                             check_hints(sym,sym.symoptions);
                           p1.free;
                           p1:=cloadnode.create(sym,srsymtable);
                        end
                      else
                        begin
                          if isclassref then
                            if assigned(p1) and
                               is_self_node(p1) then
                              Message(parser_e_only_class_methods)
                            else
                              Message(parser_e_only_class_methods_via_class_ref);
                          p1:=csubscriptnode.create(sym,p1);
                        end;
                   end;
                 propertysym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      handle_propertysym(tpropertysym(sym),sym.owner,p1);
                   end;
                 typesym:
                   begin
                     p1:=ctypenode.create(ttypesym(sym).typedef);
                   end;
                 else internalerror(16);
              end;
           end;
      end;


{****************************************************************************
                               Factor
****************************************************************************}


    function is_member_read(sym: tsym; st: tsymtable; var p1: tnode;
                            out memberparentdef: tdef): boolean;
      var
        hdef : tdef;
      begin
        result:=true;
        memberparentdef:=nil;

        case st.symtabletype of
          ObjectSymtable:
            begin
              memberparentdef:=tdef(st.defowner);
              exit;
            end;
          WithSymtable:
            begin
              if assigned(p1) then
               internalerror(2007012002);

              hdef:=tnode(twithsymtable(st).withrefnode).resultdef;
              p1:=tnode(twithsymtable(st).withrefnode).getcopy;

              if not(hdef.typ in [objectdef,classrefdef]) then
                exit;

              if (hdef.typ=classrefdef) then
                hdef:=tclassrefdef(hdef).pointeddef;
              memberparentdef:=hdef;
            end;
          else
            result:=false;
        end;
      end;

  {$maxfpuregisters 0}

    function factor(getaddr : boolean) : tnode;

         {---------------------------------------------
                         Factor_read_id
         ---------------------------------------------}

       procedure factor_read_id(var p1:tnode;var again:boolean);
         var
           pc    : pchar;
           srsym : tsym;
           unit_found : boolean;
           srsymtable : TSymtable;
           hdef  : tdef;
           orgstoredpattern,
           storedpattern : string;
           len   : longint;
           t : ttoken;
         begin
           { allow post fix operators }
           again:=true;

           { first check for identifier }
           if token<>_ID then
             begin
               srsym:=generrorsym;
               srsymtable:=nil;
               consume(_ID);
             end
           else
             begin
               searchsym(pattern,srsym,srsymtable);

               { handle unit specification like System.Writeln }
               unit_found:=try_consume_unitsym(srsym,srsymtable,t);
               storedpattern:=pattern;
               orgstoredpattern:=orgpattern;
               consume(t);

               { named parameter support }
               found_arg_name:=false;

               if not(unit_found) and
                  named_args_allowed and
                  (token=_ASSIGNMENT) then
                  begin
                    found_arg_name:=true;
                    p1:=cstringconstnode.createstr(storedpattern);
                    consume(_ASSIGNMENT);
                    exit;
                  end;
               { if nothing found give error and return errorsym }
               if assigned(srsym) then
                 check_hints(srsym,srsym.symoptions)
               else
                 begin
                   identifier_not_found(orgstoredpattern);
                   srsym:=generrorsym;
                   srsymtable:=nil;
                 end;
             end;

           { Access to funcret or need to call the function? }
           if (srsym.typ in [absolutevarsym,localvarsym,paravarsym]) and
              (vo_is_funcret in tabstractvarsym(srsym).varoptions) and
              (
               (token=_LKLAMMER) or
               (
                (
                 (m_tp7 in current_settings.modeswitches) or
                 (m_delphi in current_settings.modeswitches)
                ) and
                (afterassignment or in_args) and
                not(vo_is_result in tabstractvarsym(srsym).varoptions)
               )
              ) then
            begin
              hdef:=tdef(srsym.owner.defowner);
              if assigned(hdef) and
                 (hdef.typ=procdef) then
                srsym:=tprocdef(hdef).procsym
              else
                begin
                  Message(parser_e_illegal_expression);
                  srsym:=generrorsym;
                end;
              srsymtable:=srsym.owner;
            end;

            begin
              case srsym.typ of
                absolutevarsym :
                  begin
                    if (tabsolutevarsym(srsym).abstyp=tovar) then
                      begin
                        p1:=nil;
                        propaccesslist_to_node(p1,nil,tabsolutevarsym(srsym).ref);
                        p1:=ctypeconvnode.create(p1,tabsolutevarsym(srsym).vardef);
                        include(p1.flags,nf_absolute);
                      end
                    else
                      p1:=cloadnode.create(srsym,srsymtable);
                  end;

                staticvarsym,
                localvarsym,
                paravarsym,
                fieldvarsym :
                  begin
                    { check if we are reading a field of an object/class/   }
                    { record. is_member_read() will deal with withsymtables }
                    { if needed.                                            }
                    if is_member_read(srsym,srsymtable,p1,hdef) then
                      begin
                        { if the field was originally found in an    }
                        { objectsymtable, it means it's part of self }
                        if (srsymtable.symtabletype=ObjectSymtable) then
                          p1:=load_self_node;
                        { now, if the field itself is part of an objectsymtab }
                        { (it can be even if it was found in a withsymtable,  }
                        {  e.g., "with classinstance do field := 5"), then    }
                        { let do_member_read handle it                        }
                        if (srsym.owner.symtabletype=ObjectSymtable) then
                          do_member_read(tobjectdef(hdef),getaddr,srsym,p1,again,[])
                        else
                          { otherwise it's a regular record subscript }
                          p1:=csubscriptnode.create(srsym,p1);
                      end
                    else
                      { regular non-field load }
                      p1:=cloadnode.create(srsym,srsymtable);
                  end;

                syssym :
                  begin
                    p1:=statement_syssym(tsyssym(srsym).number);
                  end;

                typesym :
                  begin
                    hdef:=ttypesym(srsym).typedef;
                    if not assigned(hdef) then
                     begin
                       again:=false;
                     end
                    else
                     begin
                       { We need to know if this unit uses Variants }
                       if (hdef=cvarianttype) and
                          not(cs_compilesystem in current_settings.moduleswitches) then
                         current_module.flags:=current_module.flags or uf_uses_variants;
                       if (block_type<>bt_specialize) and
                          try_to_consume(_LKLAMMER) then
                        begin
                          p1:=comp_expr(true);
                          consume(_RKLAMMER);
                          p1:=ctypeconvnode.create_explicit(p1,hdef);
                        end
                       else { not LKLAMMER }
                        if (token=_POINT) and
                           is_object(hdef) then
                         begin
                           consume(_POINT);
                           if assigned(current_procinfo) and
                              assigned(current_procinfo.procdef._class) and
                              not(getaddr) then
                            begin
                              if current_procinfo.procdef._class.is_related(tobjectdef(hdef)) then
                               begin
                                 p1:=ctypenode.create(hdef);
                                 { search also in inherited methods }
                                 searchsym_in_class(tobjectdef(hdef),current_procinfo.procdef._class,pattern,srsym,srsymtable);
                                 if assigned(srsym) then
                                   check_hints(srsym,srsym.symoptions);
                                 consume(_ID);
                                 do_member_read(tobjectdef(hdef),false,srsym,p1,again,[]);
                               end
                              else
                               begin
                                 Message(parser_e_no_super_class);
                                 again:=false;
                               end;
                            end
                           else
                            begin
                              { allows @TObject.Load }
                              { also allows static methods and variables }
                              p1:=ctypenode.create(hdef);
                              { TP allows also @TMenu.Load if Load is only }
                              { defined in an anchestor class              }
                              srsym:=search_class_member(tobjectdef(hdef),pattern);
                              if assigned(srsym) then
                                begin
                                  check_hints(srsym,srsym.symoptions);
                                  if not(getaddr) and not(sp_static in srsym.symoptions) then
                                    Message(sym_e_only_static_in_static)
                                  else
                                    begin
                                      consume(_ID);
                                      do_member_read(tobjectdef(hdef),getaddr,srsym,p1,again,[]);
                                    end;
                                end
                              else
                                Message1(sym_e_id_no_member,orgpattern);
                            end;
                         end
                       else
                        begin
                          { class reference ? }
                          if is_class(hdef) then
                           begin
                             if getaddr and (token=_POINT) then
                              begin
                                consume(_POINT);
                                { allows @Object.Method }
                                { also allows static methods and variables }
                                p1:=ctypenode.create(hdef);
                                { TP allows also @TMenu.Load if Load is only }
                                { defined in an anchestor class              }
                                srsym:=search_class_member(tobjectdef(hdef),pattern);
                                if assigned(srsym) then
                                 begin
                                   check_hints(srsym,srsym.symoptions);
                                   consume(_ID);
                                   do_member_read(tobjectdef(hdef),getaddr,srsym,p1,again,[]);
                                 end
                                else
                                 begin
                                   Message1(sym_e_id_no_member,orgpattern);
                                   consume(_ID);
                                 end;
                              end
                             else
                              begin
                                p1:=ctypenode.create(hdef);
                                { For a type block we simply return only
                                  the type. For all other blocks we return
                                  a loadvmt node }
                                if not(block_type in [bt_type,bt_specialize]) then
                                  p1:=cloadvmtaddrnode.create(p1);
                              end;
                           end
                          else
                           p1:=ctypenode.create(hdef);
                        end;
                     end;
                  end;

                enumsym :
                  begin
                    p1:=genenumnode(tenumsym(srsym));
                  end;

                constsym :
                  begin
                    case tconstsym(srsym).consttyp of
                      constord :
                        begin
                          if tconstsym(srsym).constdef=nil then
                            internalerror(200403232);
                          p1:=cordconstnode.create(tconstsym(srsym).value.valueord,tconstsym(srsym).constdef,true);
                        end;
                      conststring :
                        begin
                          len:=tconstsym(srsym).value.len;
                          if not(cs_ansistrings in current_settings.localswitches) and (len>255) then
                           len:=255;
                          getmem(pc,len+1);
                          move(pchar(tconstsym(srsym).value.valueptr)^,pc^,len);
                          pc[len]:=#0;
                          p1:=cstringconstnode.createpchar(pc,len);
                        end;
                      constwstring :
                        p1:=cstringconstnode.createwstr(pcompilerwidestring(tconstsym(srsym).value.valueptr));
                      constreal :
                        p1:=crealconstnode.create(pbestreal(tconstsym(srsym).value.valueptr)^,pbestrealtype^);
                      constset :
                        p1:=csetconstnode.create(pconstset(tconstsym(srsym).value.valueptr),tconstsym(srsym).constdef);
                      constpointer :
                        p1:=cpointerconstnode.create(tconstsym(srsym).value.valueordptr,tconstsym(srsym).constdef);
                      constnil :
                        p1:=cnilnode.create;
                      constresourcestring:
                        begin
                          p1:=cloadnode.create(srsym,srsymtable);
                          do_typecheckpass(p1);
                          p1.resultdef:=cansistringtype;
                        end;
                      constguid :
                        p1:=cguidconstnode.create(pguid(tconstsym(srsym).value.valueptr)^);
                      else
                        internalerror(200507181);
                    end;
                  end;

                procsym :
                  begin
                    { check if it's a method/class method }
                    if is_member_read(srsym,srsymtable,p1,hdef) then
                      begin
                        { not srsymtable.symtabletype since that can be }
                        { withsymtable as well                          }
                        if (srsym.owner.symtabletype=ObjectSymtable) then
                          do_member_read(tobjectdef(hdef),getaddr,srsym,p1,again,[])
                        else
                          { no procsyms in records (yet) }
                          internalerror(2007012006);
                      end
                    else
                      { regular procedure/function call }
                      do_proc_call(srsym,srsymtable,nil,
                                   (getaddr and not(token in [_CARET,_POINT])),
                                   again,p1,[]);
                  end;

                propertysym :
                  begin
                    { property of a class/object? }
                    if is_member_read(srsym,srsymtable,p1,hdef) then
                      begin
                        if (srsymtable.symtabletype=ObjectSymtable) then
                          p1:=load_self_node;
                        { not srsymtable.symtabletype since that can be }
                        { withsymtable as well                          }
                        if (srsym.owner.symtabletype=ObjectSymtable) then
                          do_member_read(tobjectdef(hdef),getaddr,srsym,p1,again,[])
                        else
                          { no propertysyms in records (yet) }
                          internalerror(2007012006);
                      end
                    else
                    { no method pointer }
                      begin
                        p1:=nil;
                        handle_propertysym(tpropertysym(srsym),srsymtable,p1);
                      end;
                  end;

                labelsym :
                  begin
                    { Support @label }
                    if getaddr then
                      begin
                        if srsym.owner<>current_procinfo.procdef.localst then
                          CGMessage(parser_e_label_outside_proc);
                        p1:=cloadnode.create(srsym,srsym.owner)
                      end
                    else
                      begin
                        consume(_COLON);
                        if tlabelsym(srsym).defined then
                          Message(sym_e_label_already_defined);
                        if symtablestack.top.symtablelevel<>srsymtable.symtablelevel then
                          begin
                            tlabelsym(srsym).nonlocal:=true;
                            exclude(current_procinfo.procdef.procoptions,po_inline);
                          end;
                        tlabelsym(srsym).defined:=true;
                        p1:=clabelnode.create(nil,tlabelsym(srsym));
                        tlabelsym(srsym).code:=p1;
                      end;
                  end;

                errorsym :
                  begin
                    p1:=cerrornode.create;
                    if try_to_consume(_LKLAMMER) then
                     begin
                       parse_paras(false,false,_RKLAMMER);
                       consume(_RKLAMMER);
                     end;
                  end;

                else
                  begin
                    p1:=cerrornode.create;
                    Message(parser_e_illegal_expression);
                  end;
              end; { end case }
            end;
         end;

         {---------------------------------------------
                         Factor_Read_Set
         ---------------------------------------------}

         { Read a set between [] }
         function factor_read_set:tnode;
         var
           p1,p2 : tnode;
           lastp,
           buildp : tarrayconstructornode;
         begin
           buildp:=nil;
         { be sure that a least one arrayconstructn is used, also for an
           empty [] }
           if token=_RECKKLAMMER then
             buildp:=carrayconstructornode.create(nil,buildp)
           else
            repeat
              p1:=comp_expr(true);
              if try_to_consume(_POINTPOINT) then
                begin
                  p2:=comp_expr(true);
                  p1:=carrayconstructorrangenode.create(p1,p2);
                end;
               { insert at the end of the tree, to get the correct order }
             if not assigned(buildp) then
               begin
                 buildp:=carrayconstructornode.create(p1,nil);
                 lastp:=buildp;
               end
             else
               begin
                 lastp.right:=carrayconstructornode.create(p1,nil);
                 lastp:=tarrayconstructornode(lastp.right);
               end;
           { there could be more elements }
           until not try_to_consume(_COMMA);
           factor_read_set:=buildp;
         end;


         {---------------------------------------------
                        PostFixOperators
         ---------------------------------------------}

      procedure postfixoperators(var p1:tnode;var again:boolean);

        { tries to avoid syntax errors after invalid qualifiers }
        procedure recoverconsume_postfixops;
          begin
            repeat
              if not try_to_consume(_CARET) then
                if try_to_consume(_POINT) then
                  try_to_consume(_ID)
                else if try_to_consume(_LECKKLAMMER) then
                  begin
                    repeat
                      comp_expr(true);
                    until not try_to_consume(_COMMA);
                    consume(_RECKKLAMMER);
                  end
                else if try_to_consume(_LKLAMMER) then
                  begin
                    repeat
                      comp_expr(true);
                    until not try_to_consume(_COMMA);
                    consume(_RKLAMMER);
                  end
                else
                  break;
            until false;
          end;


        procedure handle_variantarray;
          var
            p4 : tnode;
            newstatement : tstatementnode;
            tempresultvariant,
            temp    : ttempcreatenode;
            paras : tcallparanode;
            newblock : tnode;
            countindices : aint;
          begin
            { create statements with call initialize the arguments and
              call fpc_dynarr_setlength }
            newblock:=internalstatements(newstatement);

            { get temp for array of indicies,
              we set the real size later }
            temp:=ctempcreatenode.create(s32inttype,4,tt_persistent,false);
            addstatement(newstatement,temp);

            countindices:=0;
            repeat
              p4:=comp_expr(true);

              addstatement(newstatement,cassignmentnode.create(
                ctemprefnode.create_offset(temp,countindices*s32inttype.size),p4));
               inc(countindices);
            until not try_to_consume(_COMMA);

            { set real size }
            temp.size:=countindices*s32inttype.size;

            consume(_RECKKLAMMER);

            { we need only a write access if a := follows }
            if token=_ASSIGNMENT then
              begin
                consume(_ASSIGNMENT);
                p4:=comp_expr(true);

                { create call to fpc_vararray_put }
                paras:=ccallparanode.create(cordconstnode.create
                      (countindices,s32inttype,true),
                   ccallparanode.create(caddrnode.create_internal
                  (ctemprefnode.create(temp)),
                   ccallparanode.create(ctypeconvnode.create_internal(p4,cvarianttype),
                   ccallparanode.create(ctypeconvnode.create_internal(p1,cvarianttype)
                     ,nil))));

                addstatement(newstatement,ccallnode.createintern('fpc_vararray_put',paras));
                addstatement(newstatement,ctempdeletenode.create(temp));
              end
            else
              begin
                { create temp for result }
                tempresultvariant:=ctempcreatenode.create(cvarianttype,cvarianttype.size,tt_persistent,true);
                addstatement(newstatement,tempresultvariant);

                { create call to fpc_vararray_get }
                paras:=ccallparanode.create(cordconstnode.create
                      (countindices,s32inttype,true),
                   ccallparanode.create(caddrnode.create_internal
                  (ctemprefnode.create(temp)),
                   ccallparanode.create(p1,
                   ccallparanode.create(
                       ctemprefnode.create(tempresultvariant)
                     ,nil))));

                addstatement(newstatement,ccallnode.createintern('fpc_vararray_get',paras));
                addstatement(newstatement,ctempdeletenode.create(temp));
                { the last statement should return the value as
                  location and type, this is done be referencing the
                  temp and converting it first from a persistent temp to
                  normal temp }
                addstatement(newstatement,ctempdeletenode.create_normal_temp(tempresultvariant));
                addstatement(newstatement,ctemprefnode.create(tempresultvariant));
              end;
            p1:=newblock;
          end;

        var
          protsym  : tpropertysym;
          p2,p3  : tnode;
          srsym  : tsym;
          srsymtable : TSymtable;
          classh     : tobjectdef;
          { shouldn't be used that often, so the extra overhead is ok to save
            stack space }
          dispatchstring : ansistring;
        label
          skipreckklammercheck;
        begin
          again:=true;
          while again do
           begin
             { we need the resultdef }
             do_typecheckpass(p1);

             if codegenerror then
              begin
                recoverconsume_postfixops;
                exit;
              end;
             { handle token }
             case token of
               _CARET:
                  begin
                    consume(_CARET);

                    { support tp/mac procvar^ if the procvar returns a
                      pointer type }
                    if ((m_tp_procvar in current_settings.modeswitches) or
                        (m_mac_procvar in current_settings.modeswitches)) and
                       (p1.resultdef.typ=procvardef) and
                       (tprocvardef(p1.resultdef).returndef.typ=pointerdef) then
                      begin
                        p1:=ccallnode.create_procvar(nil,p1);
                        typecheckpass(p1);
                      end;

                    if (p1.resultdef.typ<>pointerdef) then
                      begin
                         { ^ as binary operator is a problem!!!! (FK) }
                         again:=false;
                         Message(parser_e_invalid_qualifier);
                         recoverconsume_postfixops;
                         p1.destroy;
                         p1:=cerrornode.create;
                      end
                    else
                      p1:=cderefnode.create(p1);
                  end;

               _LECKKLAMMER:
                  begin
                    if is_class_or_interface_or_object(p1.resultdef) then
                      begin
                        { default property }
                        protsym:=search_default_property(tobjectdef(p1.resultdef));
                        if not(assigned(protsym)) then
                          begin
                             p1.destroy;
                             p1:=cerrornode.create;
                             again:=false;
                             message(parser_e_no_default_property_available);
                          end
                        else
                          begin
                            { The property symbol is referenced indirect }
                            protsym.IncRefCount;
                            handle_propertysym(protsym,protsym.owner,p1);
                          end;
                      end
                    else
                      begin
                        consume(_LECKKLAMMER);
                        repeat
                          case p1.resultdef.typ of
                            pointerdef:
                              begin
                                 { support delphi autoderef }
                                 if (tpointerdef(p1.resultdef).pointeddef.typ=arraydef) and
                                    (m_autoderef in current_settings.modeswitches) then
                                   p1:=cderefnode.create(p1);
                                 p2:=comp_expr(true);
                                 { Support Pbytevar[0..9] which returns array [0..9].}
                                 if try_to_consume(_POINTPOINT) then
                                   p2:=crangenode.create(p2,comp_expr(true));
                                 p1:=cvecnode.create(p1,p2);
                              end;
                            variantdef:
                              begin
                                handle_variantarray;
                                { the RECKKLAMMER is already read }
                                goto skipreckklammercheck;
                              end;
                            stringdef :
                              begin
                                p2:=comp_expr(true);
                                { Support string[0..9] which returns array [0..9] of char.}
                                if try_to_consume(_POINTPOINT) then
                                  p2:=crangenode.create(p2,comp_expr(true));
                                p1:=cvecnode.create(p1,p2);
                              end;
                            arraydef:
                              begin
                                p2:=comp_expr(true);
                                { support SEG:OFS for go32v2 Mem[] }
                                if (target_info.system in [system_i386_go32v2,system_i386_watcom]) and
                                   (p1.nodetype=loadn) and
                                   assigned(tloadnode(p1).symtableentry) and
                                   assigned(tloadnode(p1).symtableentry.owner.name) and
                                   (tloadnode(p1).symtableentry.owner.name^='SYSTEM') and
                                   ((tloadnode(p1).symtableentry.name='MEM') or
                                    (tloadnode(p1).symtableentry.name='MEMW') or
                                    (tloadnode(p1).symtableentry.name='MEML')) then
                                  begin
                                    if try_to_consume(_COLON) then
                                     begin
                                       p3:=caddnode.create(muln,cordconstnode.create($10,s32inttype,false),p2);
                                       p2:=comp_expr(true);
                                       p2:=caddnode.create(addn,p2,p3);
                                       if try_to_consume(_POINTPOINT) then
                                         { Support mem[$a000:$0000..$07ff] which returns array [0..$7ff] of memtype.}
                                         p2:=crangenode.create(p2,caddnode.create(addn,comp_expr(true),p3.getcopy));
                                       p1:=cvecnode.create(p1,p2);
                                       include(tvecnode(p1).flags,nf_memseg);
                                       include(tvecnode(p1).flags,nf_memindex);
                                     end
                                    else
                                     begin
                                       if try_to_consume(_POINTPOINT) then
                                         { Support mem[$80000000..$80000002] which returns array [0..2] of memtype.}
                                         p2:=crangenode.create(p2,comp_expr(true));
                                       p1:=cvecnode.create(p1,p2);
                                       include(tvecnode(p1).flags,nf_memindex);
                                     end;
                                  end
                                else
                                  begin
                                    if try_to_consume(_POINTPOINT) then
                                      { Support arrayvar[0..9] which returns array [0..9] of arraytype.}
                                      p2:=crangenode.create(p2,comp_expr(true));
                                    p1:=cvecnode.create(p1,p2);
                                  end;
                              end;
                            else
                              begin
                                if p1.resultdef.typ<>undefineddef then
                                  Message(parser_e_invalid_qualifier);
                                p1.destroy;
                                p1:=cerrornode.create;
                                comp_expr(true);
                                again:=false;
                              end;
                          end;
                          do_typecheckpass(p1);
                        until not try_to_consume(_COMMA);
                        consume(_RECKKLAMMER);
                        { handle_variantarray eats the RECKKLAMMER and jumps here }
                      skipreckklammercheck:
                      end;
                  end;

               _POINT :
                  begin
                    consume(_POINT);
                    if (p1.resultdef.typ=pointerdef) and
                       (m_autoderef in current_settings.modeswitches) then
                      begin
                        p1:=cderefnode.create(p1);
                        do_typecheckpass(p1);
                      end;
                    case p1.resultdef.typ of
                      recorddef:
                        begin
                          if token=_ID then
                            begin
                              srsym:=tsym(trecorddef(p1.resultdef).symtable.Find(pattern));
                              if assigned(srsym) and
                                 (srsym.typ=fieldvarsym) then
                                begin
                                  check_hints(srsym,srsym.symoptions);
                                  p1:=csubscriptnode.create(srsym,p1)
                                end
                              else
                                begin
                                  Message1(sym_e_illegal_field,pattern);
                                  p1.destroy;
                                  p1:=cerrornode.create;
                                end;
                             end;
                           consume(_ID);
                         end;
                       variantdef:
                         begin
                           { dispatch call? }
                           if token=_ID then
                             begin
                               dispatchstring:=orgpattern;
                               consume(_ID);
                               if try_to_consume(_LKLAMMER) then
                                 begin
                                   p2:=parse_paras(false,true,_RKLAMMER);
                                   consume(_RKLAMMER);
                                 end
                               else
                                 p2:=nil;
                               { property setter? }
                               if (token=_ASSIGNMENT) and not(afterassignment) then
                                 begin
                                   consume(_ASSIGNMENT);
                                   { read the expression }
                                   p3:=comp_expr(true);
                                   { concat value parameter too }
                                   p2:=ccallparanode.create(p3,p2);
                                   { passing p3 here is only for information purposes }
                                   p1:=translate_disp_call(p1,p2,p3,dispatchstring,0,false);
                                 end
                               else
                                 begin
                                   p1:=translate_disp_call(p1,p2,nil,dispatchstring,0,
                                     { this is only an approximation
                                       setting useresult if not necessary is only a waste of time, no more, no less (FK) }
                                     afterassignment or in_args or (token<>_SEMICOLON));
                                 end;
                             end
                           else { Error }
                             Consume(_ID);
                          end;
                       classrefdef:
                         begin
                           if token=_ID then
                             begin
                               classh:=tobjectdef(tclassrefdef(p1.resultdef).pointeddef);
                               searchsym_in_class(classh,classh,pattern,srsym,srsymtable);
                               if assigned(srsym) then
                                 begin
                                   check_hints(srsym,srsym.symoptions);
                                   consume(_ID);
                                   do_member_read(classh,getaddr,srsym,p1,again,[]);
                                 end
                               else
                                 begin
                                   Message1(sym_e_id_no_member,orgpattern);
                                   p1.destroy;
                                   p1:=cerrornode.create;
                                   { try to clean up }
                                   consume(_ID);
                                 end;
                             end
                           else { Error }
                             Consume(_ID);
                         end;
                       objectdef:
                         begin
                           if token=_ID then
                             begin
                               classh:=tobjectdef(p1.resultdef);
                               searchsym_in_class(classh,classh,pattern,srsym,srsymtable);
                               if assigned(srsym) then
                                 begin
                                    check_hints(srsym,srsym.symoptions);
                                    consume(_ID);
                                    do_member_read(classh,getaddr,srsym,p1,again,[]);
                                 end
                               else
                                 begin
                                    Message1(sym_e_id_no_member,orgpattern);
                                    p1.destroy;
                                    p1:=cerrornode.create;
                                    { try to clean up }
                                    consume(_ID);
                                 end;
                             end
                           else { Error }
                             Consume(_ID);
                         end;
                       pointerdef:
                         begin
                           Message(parser_e_invalid_qualifier);
                           if tpointerdef(p1.resultdef).pointeddef.typ in [recorddef,objectdef,classrefdef] then
                             Message(parser_h_maybe_deref_caret_missing);
                         end;
                       else
                         begin
                           if p1.resultdef.typ<>undefineddef then
                             Message(parser_e_invalid_qualifier);
                           p1.destroy;
                           p1:=cerrornode.create;
                           { Error }
                           consume(_ID);
                         end;
                    end;
                  end;

               else
                 begin
                   { is this a procedure variable ? }
                   if assigned(p1.resultdef) and
                      (p1.resultdef.typ=procvardef) then
                     begin
                       { Typenode for typecasting or expecting a procvar }
                       if (p1.nodetype=typen) or
                          (
                           assigned(getprocvardef) and
                           equal_defs(p1.resultdef,getprocvardef)
                          ) then
                         begin
                           if try_to_consume(_LKLAMMER) then
                             begin
                               p1:=comp_expr(true);
                               consume(_RKLAMMER);
                               p1:=ctypeconvnode.create_explicit(p1,p1.resultdef);
                             end
                           else
                             again:=false
                         end
                       else
                         begin
                           if try_to_consume(_LKLAMMER) then
                             begin
                               p2:=parse_paras(false,false,_RKLAMMER);
                               consume(_RKLAMMER);
                               p1:=ccallnode.create_procvar(p2,p1);
                               { proc():= is never possible }
                               if token=_ASSIGNMENT then
                                 begin
                                   Message(parser_e_illegal_expression);
                                   p1.free;
                                   p1:=cerrornode.create;
                                   again:=false;
                                 end;
                             end
                           else
                             again:=false;
                         end;
                     end
                   else
                     again:=false;
                  end;
             end;
           end; { while again }
        end;


      {---------------------------------------------
                      Factor (Main)
      ---------------------------------------------}

      var
         l          : longint;
         ic         : int64;
         qc         : qword;
{$ifndef cpu64}
         card       : cardinal;
{$endif cpu64}
         oldp1,
         p1         : tnode;
         code       : integer;
         again      : boolean;
         srsym      : tsym;
         srsymtable : TSymtable;
         pd         : tprocdef;
         hclassdef     : tobjectdef;
         d          : bestreal;
         cur        : currency;
         hs,hsorg   : string;
         hdef       : tdef;
         filepos    : tfileposinfo;
      begin
        oldp1:=nil;
        p1:=nil;
        filepos:=current_tokenpos;
        again:=false;
        if token=_ID then
         begin
           again:=true;
           { Handle references to self }
           if (idtoken=_SELF) and
              not(block_type in [bt_const,bt_type]) and
              assigned(current_procinfo) and
              assigned(current_procinfo.procdef._class) then
             begin
               p1:=load_self_node;
               consume(_ID);
               again:=true;
             end
           else
             factor_read_id(p1,again);

           if again then
            begin
              if (p1<>oldp1) then
               begin
                 if assigned(p1) then
                   p1.fileinfo:=filepos;
                 oldp1:=p1;
                 filepos:=current_tokenpos;
               end;
              { handle post fix operators }
              postfixoperators(p1,again);
            end;
         end
        else
         case token of
           _RETURN :
              begin
                consume(_RETURN);
                if not(token in [_SEMICOLON,_ELSE,_END]) then
                  p1 := cexitnode.create(comp_expr(true))
                else
                  p1 := cexitnode.create(nil);
              end;
           _INHERITED :
             begin
               again:=true;
               consume(_INHERITED);
               if assigned(current_procinfo) and
                  assigned(current_procinfo.procdef._class) then
                begin
                  hclassdef:=current_procinfo.procdef._class.childof;
                  { if inherited; only then we need the method with
                    the same name }
                  if token in endtokens then
                   begin
                     hs:=current_procinfo.procdef.procsym.name;
                     hsorg:=current_procinfo.procdef.procsym.realname;
                     anon_inherited:=true;
                     { For message methods we need to search using the message
                       number or string }
                     pd:=tprocdef(tprocsym(current_procinfo.procdef.procsym).ProcdefList[0]);
                     srdef:=nil;
                     if (po_msgint in pd.procoptions) then
                       searchsym_in_class_by_msgint(hclassdef,pd.messageinf.i,srdef,srsym,srsymtable)
                     else
                      if (po_msgstr in pd.procoptions) then
                        searchsym_in_class_by_msgstr(hclassdef,pd.messageinf.str^,srsym,srsymtable)
                     else
                       searchsym_in_class(hclassdef,current_procinfo.procdef._class,hs,srsym,srsymtable);
                   end
                  else
                   begin
                     hs:=pattern;
                     hsorg:=orgpattern;
                     consume(_ID);
                     anon_inherited:=false;
                     searchsym_in_class(hclassdef,current_procinfo.procdef._class,hs,srsym,srsymtable);
                   end;
                  if assigned(srsym) then
                   begin
                     check_hints(srsym,srsym.symoptions);
                     { load the procdef from the inherited class and
                       not from self }
                     case srsym.typ of
                       procsym:
                         begin
                           hdef:=hclassdef;
                           if (po_classmethod in current_procinfo.procdef.procoptions) or
                              (po_staticmethod in current_procinfo.procdef.procoptions) then
                             hdef:=tclassrefdef.create(hdef);
                           p1:=ctypenode.create(hdef);
                         end;
                       propertysym:
                         ;
                       else
                         begin
                           Message(parser_e_methode_id_expected);
                           p1:=cerrornode.create;
                         end;
                     end;
                     do_member_read(hclassdef,getaddr,srsym,p1,again,[cnf_inherited,cnf_anon_inherited]);
                   end
                  else
                   begin
                     if anon_inherited then
                      begin
                        { For message methods we need to call DefaultHandler }
                        if (po_msgint in pd.procoptions) or
                           (po_msgstr in pd.procoptions) then
                          begin
                            searchsym_in_class(hclassdef,hclassdef,'DEFAULTHANDLER',srsym,srsymtable);
                            if not assigned(srsym) or
                               (srsym.typ<>procsym) then
                              internalerror(200303171);
                            p1:=nil;
                            do_proc_call(srsym,srsym.owner,hclassdef,false,again,p1,[]);
                          end
                        else
                          begin
                            { we need to ignore the inherited; }
                            p1:=cnothingnode.create;
                          end;
                      end
                     else
                      begin
                        Message1(sym_e_id_no_member,hsorg);
                        p1:=cerrornode.create;
                      end;
                     again:=false;
                   end;
                  { turn auto inheriting off }
                  anon_inherited:=false;
                end
               else
                 begin
                    Message(parser_e_generic_methods_only_in_methods);
                    again:=false;
                    p1:=cerrornode.create;
                 end;
               postfixoperators(p1,again);
             end;

           _INTCONST :
             begin
{$ifdef cpu64}
               { when already running under 64bit must read int64 constant, because reading
                 cardinal first will also succeed (code=0) for values > maxcardinal, because
                 range checking is off by default (PFV) }
               val(pattern,ic,code);
               if code=0 then
                 begin
                    consume(_INTCONST);
                    int_to_type(ic,hdef);
                    p1:=cordconstnode.create(ic,hdef,true);
                 end
               else
                 begin
                   { try qword next }
                   val(pattern,qc,code);
                   if code=0 then
                     begin
                        consume(_INTCONST);
                        hdef:=u64inttype;
                        p1:=cordconstnode.create(qc,hdef,true);
                     end;
                 end;
{$else}
               { try cardinal first }
               val(pattern,card,code);
               if code=0 then
                 begin
                    consume(_INTCONST);
                    int_to_type(card,hdef);
                    p1:=cordconstnode.create(card,hdef,true);
                 end
               else
                 begin
                   { then longint }
                   val(pattern,l,code);
                   if code = 0 then
                     begin
                       consume(_INTCONST);
                       int_to_type(l,hdef);
                       p1:=cordconstnode.create(l,hdef,true);
                     end
                   else
                     begin
                       { then int64 }
                       val(pattern,ic,code);
                       if code=0 then
                         begin
                            consume(_INTCONST);
                            int_to_type(ic,hdef);
                            p1:=cordconstnode.create(ic,hdef,true);
                         end
                       else
                         begin
                           { try qword next }
                           val(pattern,qc,code);
                           if code=0 then
                             begin
                                consume(_INTCONST);
                                hdef:=u64inttype;
                                p1:=cordconstnode.create(tconstexprint(qc),hdef,true);
                             end;
                         end;
                     end;
                 end;
{$endif}
               if code<>0 then
                 begin
                   { finally float }
                   val(pattern,d,code);
                   if code<>0 then
                     begin
                        Message(parser_e_invalid_integer);
                        consume(_INTCONST);
                        l:=1;
                        p1:=cordconstnode.create(l,sinttype,true);
                     end
                   else
                     begin
                        consume(_INTCONST);
                        p1:=crealconstnode.create(d,pbestrealtype^);
                     end;
                 end
               else
                 { the necessary range checking has already been done by val }
                 tordconstnode(p1).rangecheck:=false;
             end;

           _REALNUMBER :
             begin
               val(pattern,d,code);
               if code<>0 then
                begin
                  Message(parser_e_error_in_real);
                  d:=1.0;
                end;
               consume(_REALNUMBER);
{$ifdef FPC_REAL2REAL_FIXED}
               if (current_settings.minfpconstprec=s32real) and
                  (d = single(d)) then
                 p1:=crealconstnode.create(d,s32floattype)
               else if (current_settings.minfpconstprec=s64real) and
                       (d = double(d)) then
                 p1:=crealconstnode.create(d,s64floattype)
               else
{$endif FPC_REAL2REAL_FIXED}
                 p1:=crealconstnode.create(d,pbestrealtype^);
{$ifdef FPC_HAS_STR_CURRENCY}
               val(pattern,cur,code);
               if code=0 then
                 trealconstnode(p1).value_currency:=cur;
{$endif FPC_HAS_STR_CURRENCY}
             end;

           _STRING :
             begin
               string_dec(hdef);
               { STRING can be also a type cast }
               if try_to_consume(_LKLAMMER) then
                begin
                  p1:=comp_expr(true);
                  consume(_RKLAMMER);
                  p1:=ctypeconvnode.create_explicit(p1,hdef);
                  { handle postfix operators here e.g. string(a)[10] }
                  again:=true;
                  postfixoperators(p1,again);
                end
               else
                p1:=ctypenode.create(hdef);
             end;

           _FILE :
             begin
               hdef:=cfiletype;
               consume(_FILE);
               { FILE can be also a type cast }
               if try_to_consume(_LKLAMMER) then
                begin
                  p1:=comp_expr(true);
                  consume(_RKLAMMER);
                  p1:=ctypeconvnode.create_explicit(p1,hdef);
                  { handle postfix operators here e.g. string(a)[10] }
                  again:=true;
                  postfixoperators(p1,again);
                end
               else
                begin
                  p1:=ctypenode.create(hdef);
                end;
             end;

           _CSTRING :
             begin
               p1:=cstringconstnode.createstr(pattern);
               consume(_CSTRING);
             end;

           _CCHAR :
             begin
               p1:=cordconstnode.create(ord(pattern[1]),cchartype,true);
               consume(_CCHAR);
             end;

           _CWSTRING:
             begin
               p1:=cstringconstnode.createwstr(patternw);
               consume(_CWSTRING);
             end;

           _CWCHAR:
             begin
               p1:=cordconstnode.create(ord(getcharwidestring(patternw,0)),cwidechartype,true);
               consume(_CWCHAR);
             end;

           _KLAMMERAFFE :
             begin
               consume(_KLAMMERAFFE);
               got_addrn:=true;
               { support both @<x> and @(<x>) }
               if try_to_consume(_LKLAMMER) then
                begin
                  p1:=factor(true);
                  if token in [_CARET,_POINT,_LECKKLAMMER] then
                   begin
                     again:=true;
                     postfixoperators(p1,again);
                   end;
                  consume(_RKLAMMER);
                end
               else
                p1:=factor(true);
               if token in [_CARET,_POINT,_LECKKLAMMER] then
                begin
                  again:=true;
                  postfixoperators(p1,again);
                end;
               got_addrn:=false;
               p1:=caddrnode.create(p1);
               if cs_typed_addresses in current_settings.localswitches then
                 include(p1.flags,nf_typedaddr);
               { Store the procvar that we are expecting, the
                 addrn will use the information to find the correct
                 procdef or it will return an error }
               if assigned(getprocvardef) and
                  (taddrnode(p1).left.nodetype = loadn) then
                 taddrnode(p1).getprocvardef:=getprocvardef;
             end;

           _LKLAMMER :
             begin
               consume(_LKLAMMER);
               p1:=comp_expr(true);
               consume(_RKLAMMER);
               { it's not a good solution     }
               { but (a+b)^ makes some problems  }
               if token in [_CARET,_POINT,_LECKKLAMMER] then
                begin
                  again:=true;
                  postfixoperators(p1,again);
                end;
             end;

           _LECKKLAMMER :
             begin
               consume(_LECKKLAMMER);
               p1:=factor_read_set;
               consume(_RECKKLAMMER);
             end;

           _PLUS :
             begin
               consume(_PLUS);
               p1:=factor(false);
               { we must generate a new node to do 0+<p1> otherwise the + will
                 not be checked }
               p1:=caddnode.create(addn,genintconstnode(0),p1);
             end;

           _MINUS :
             begin
               consume(_MINUS);
               if (token = _INTCONST) then
                  begin
                    { ugly hack, but necessary to be able to parse }
                    { -9223372036854775808 as int64 (JM)           }
                    pattern := '-'+pattern;
                    p1:=sub_expr(oppower,false);
                    {  -1 ** 4 should be - (1 ** 4) and not
                       (-1) ** 4
                       This was the reason of tw0869.pp test failure PM }
                    if p1.nodetype=starstarn then
                      begin
                        if tbinarynode(p1).left.nodetype=ordconstn then
                          begin
                            tordconstnode(tbinarynode(p1).left).value:=-tordconstnode(tbinarynode(p1).left).value;
                            p1:=cunaryminusnode.create(p1);
                          end
                        else if tbinarynode(p1).left.nodetype=realconstn then
                          begin
                            trealconstnode(tbinarynode(p1).left).value_real:=-trealconstnode(tbinarynode(p1).left).value_real;
                            trealconstnode(tbinarynode(p1).left).value_currency:=-trealconstnode(tbinarynode(p1).left).value_currency;
                            p1:=cunaryminusnode.create(p1);
                          end
                        else
                          internalerror(20021029);
                      end;
                  end
               else
                 begin
                   p1:=sub_expr(oppower,false);
                   p1:=cunaryminusnode.create(p1);
                 end;
             end;

           _OP_NOT :
             begin
               consume(_OP_NOT);
               p1:=factor(false);
               p1:=cnotnode.create(p1);
             end;

           _TRUE :
             begin
               consume(_TRUE);
               p1:=cordconstnode.create(1,booltype,false);
             end;

           _FALSE :
             begin
               consume(_FALSE);
               p1:=cordconstnode.create(0,booltype,false);
             end;

           _NIL :
             begin
               consume(_NIL);
               p1:=cnilnode.create;
               { It's really ugly code nil^, but delphi allows it }
               if token in [_CARET] then
                begin
                  again:=true;
                  postfixoperators(p1,again);
                end;
             end;

           else
             begin
               Message(parser_e_illegal_expression);
               p1:=cerrornode.create;
               { recover }
               consume(token);
             end;
        end;

        { generate error node if no node is created }
        if not assigned(p1) then
         begin
{$ifdef EXTDEBUG}
           Comment(V_Warning,'factor: p1=nil');
{$endif}
           p1:=cerrornode.create;
         end;

        { get the resultdef for the node }
        if (not assigned(p1.resultdef)) then
         do_typecheckpass(p1);

        if assigned(p1) and
           (p1<>oldp1) then
          p1.fileinfo:=filepos;
        factor:=p1;
      end;
  {$maxfpuregisters default}

{****************************************************************************
                             Sub_Expr
****************************************************************************}
   const
      { Warning these stay be ordered !! }
      operator_levels:array[Toperator_precedence] of set of Ttoken=
         ([_LT,_LTE,_GT,_GTE,_EQUAL,_UNEQUAL,_OP_IN],
          [_PLUS,_MINUS,_OP_OR,_PIPE,_OP_XOR],
          [_CARET,_SYMDIF,_STARSTAR,_STAR,_SLASH,
           _OP_AS,_OP_IS,_OP_AND,_AMPERSAND,_OP_DIV,_OP_MOD,_OP_SHL,_OP_SHR],
          [_STARSTAR] );

    function sub_expr(pred_level:Toperator_precedence;accept_equal : boolean):tnode;
    {Reads a subexpression while the operators are of the current precedence
     level, or any higher level. Replaces the old term, simpl_expr and
     simpl2_expr.}
      var
        p1,p2   : tnode;
        oldt    : Ttoken;
        filepos : tfileposinfo;
      begin
        if pred_level=highest_precedence then
          p1:=factor(false)
        else
          p1:=sub_expr(succ(pred_level),true);
        repeat
          if (token in operator_levels[pred_level]) and
             ((token<>_EQUAL) or accept_equal) then
           begin
             oldt:=token;
             filepos:=current_tokenpos;
             consume(token);
             if pred_level=highest_precedence then
               p2:=factor(false)
             else
               p2:=sub_expr(succ(pred_level),true);
             case oldt of
               _PLUS :
                 p1:=caddnode.create(addn,p1,p2);
               _MINUS :
                 p1:=caddnode.create(subn,p1,p2);
               _STAR :
                 p1:=caddnode.create(muln,p1,p2);
               _SLASH :
                 p1:=caddnode.create(slashn,p1,p2);
               _EQUAL :
                 p1:=caddnode.create(equaln,p1,p2);
               _GT :
                 p1:=caddnode.create(gtn,p1,p2);
               _LT :
                 p1:=caddnode.create(ltn,p1,p2);
               _GTE :
                 p1:=caddnode.create(gten,p1,p2);
               _LTE :
                 p1:=caddnode.create(lten,p1,p2);
               _SYMDIF :
                 p1:=caddnode.create(symdifn,p1,p2);
               _STARSTAR :
                 p1:=caddnode.create(starstarn,p1,p2);
               _OP_AS :
                 p1:=casnode.create(p1,p2);
               _OP_IN :
                 p1:=cinnode.create(p1,p2);
               _OP_IS :
                 p1:=cisnode.create(p1,p2);
               _OP_OR,
               _PIPE {macpas only} :
                 begin
                   p1:=caddnode.create(orn,p1,p2);
                   if (oldt = _PIPE) then
                     include(p1.flags,nf_short_bool);
                 end;
               _OP_AND,
               _AMPERSAND {macpas only} :
                 begin
                   p1:=caddnode.create(andn,p1,p2);
                   if (oldt = _AMPERSAND) then
                     include(p1.flags,nf_short_bool);
                 end;
               _OP_DIV :
                 p1:=cmoddivnode.create(divn,p1,p2);
               _OP_NOT :
                 p1:=cnotnode.create(p1);
               _OP_MOD :
                 p1:=cmoddivnode.create(modn,p1,p2);
               _OP_SHL :
                 p1:=cshlshrnode.create(shln,p1,p2);
               _OP_SHR :
                 p1:=cshlshrnode.create(shrn,p1,p2);
               _OP_XOR :
                 p1:=caddnode.create(xorn,p1,p2);
               _ASSIGNMENT :
                 p1:=cassignmentnode.create(p1,p2);
               _UNEQUAL :
                 p1:=caddnode.create(unequaln,p1,p2);
             end;
             p1.fileinfo:=filepos;
           end
          else
           break;
        until false;
        sub_expr:=p1;
      end;


    function comp_expr(accept_equal : boolean):tnode;
      var
         oldafterassignment : boolean;
         p1 : tnode;
      begin
         oldafterassignment:=afterassignment;
         afterassignment:=true;
         p1:=sub_expr(opcompare,accept_equal);
         { get the resultdef for this expression }
         if not assigned(p1.resultdef) then
          do_typecheckpass(p1);
         afterassignment:=oldafterassignment;
         comp_expr:=p1;
      end;


    function expr : tnode;

      var
         p1,p2 : tnode;
         oldafterassignment : boolean;
         oldp1 : tnode;
         filepos : tfileposinfo;

      begin
         oldafterassignment:=afterassignment;
         p1:=sub_expr(opcompare,true);
         { get the resultdef for this expression }
         if not assigned(p1.resultdef) then
          do_typecheckpass(p1);
         filepos:=current_tokenpos;
         if token in [_ASSIGNMENT,_PLUSASN,_MINUSASN,_STARASN,_SLASHASN] then
           afterassignment:=true;
         oldp1:=p1;
         case token of
           _POINTPOINT :
             begin
                consume(_POINTPOINT);
                p2:=sub_expr(opcompare,true);
                p1:=crangenode.create(p1,p2);
             end;
           _ASSIGNMENT :
             begin
                consume(_ASSIGNMENT);
                if (p1.resultdef.typ=procvardef) then
                  getprocvardef:=tprocvardef(p1.resultdef);
                p2:=sub_expr(opcompare,true);
                if assigned(getprocvardef) then
                  handle_procvar(getprocvardef,p2);
                getprocvardef:=nil;
                p1:=cassignmentnode.create(p1,p2);
             end;
           _PLUSASN :
             begin
               consume(_PLUSASN);
               p2:=sub_expr(opcompare,true);
               p1:=gen_c_style_operator(addn,p1,p2);
            end;
          _MINUSASN :
            begin
               consume(_MINUSASN);
               p2:=sub_expr(opcompare,true);
               p1:=gen_c_style_operator(subn,p1,p2);
            end;
          _STARASN :
            begin
               consume(_STARASN  );
               p2:=sub_expr(opcompare,true);
               p1:=gen_c_style_operator(muln,p1,p2);
            end;
          _SLASHASN :
            begin
               consume(_SLASHASN  );
               p2:=sub_expr(opcompare,true);
               p1:=gen_c_style_operator(slashn,p1,p2);
            end;
         end;
         { get the resultdef for this expression }
         if not assigned(p1.resultdef) then
          do_typecheckpass(p1);
         afterassignment:=oldafterassignment;
         if p1<>oldp1 then
           p1.fileinfo:=filepos;
         expr:=p1;
      end;

    function get_intconst:TConstExprInt;
    {Reads an expression, tries to evalute it and check if it is an integer
     constant. Then the constant is returned.}
    var
      p:tnode;
    begin
      result:=0;
      p:=comp_expr(true);
      if not codegenerror then
       begin
         if (p.nodetype<>ordconstn) or
            not(is_integer(p.resultdef)) then
          Message(parser_e_illegal_expression)
         else
          result:=tordconstnode(p).value;
       end;
      p.free;
    end;


    function get_stringconst:string;
    {Reads an expression, tries to evaluate it and checks if it is a string
     constant. Then the constant is returned.}
    var
      p:tnode;
    begin
      get_stringconst:='';
      p:=comp_expr(true);
      if p.nodetype<>stringconstn then
        begin
          if (p.nodetype=ordconstn) and is_char(p.resultdef) then
            get_stringconst:=char(tordconstnode(p).value)
          else
            Message(parser_e_illegal_expression);
        end
      else
        get_stringconst:=strpas(tstringconstnode(p).value_str);
      p.free;
    end;

end.

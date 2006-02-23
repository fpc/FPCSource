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

    procedure string_dec(var t: ttype);

    procedure symlist_to_node(var p1:tnode;st:tsymtable;pl:tsymlist);

    function node_to_symlist(p1:tnode):tsymlist;

    function parse_paras(__colon : boolean;end_of_paras : ttoken) : tnode;

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
       pbase,pinline,
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



    procedure string_dec(var t: ttype);
    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
      var
         p : tnode;
      begin
         t:=cshortstringtype;
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
{                   t.setdef(tstringdef.createlong(tordconstnode(p).value))}
                     Message(parser_e_invalid_string_size);
                     tordconstnode(p).value:=255;
                     t.setdef(tstringdef.createshort(tordconstnode(p).value));
                  end
                 else
                   if tordconstnode(p).value<>255 then
                     t.setdef(tstringdef.createshort(tordconstnode(p).value));
               end;
              p.free;
           end
          else
            begin
               if cs_ansistrings in aktlocalswitches then
                 {$ifdef ansistring_bits}
                 case aktansistring_bits of
                   sb_16:
                     t:=cansistringtype16;
                   sb_32:
                     t:=cansistringtype32;
                   sb_64:
                     t:=cansistringtype64;
                 end
                 {$else}
                 t:=cansistringtype
                 {$endif}
               else
                 t:=cshortstringtype;
            end;
       end;



    procedure symlist_to_node(var p1:tnode;st:tsymtable;pl:tsymlist);
      var
        plist : psymlistitem;
      begin
        plist:=pl.firstsym;
        while assigned(plist) do
         begin
           case plist^.sltype of
             sl_load :
               begin
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
                      objectsymtable :
                        p1:=load_self_node;
                    end;
                  end;
                 if assigned(p1) then
                  p1:=csubscriptnode.create(plist^.sym,p1)
                 else
                  p1:=cloadnode.create(plist^.sym,st);
               end;
             sl_subscript :
               p1:=csubscriptnode.create(plist^.sym,p1);
             sl_typeconv :
               p1:=ctypeconvnode.create_explicit(p1,plist^.tt);
             sl_absolutetype :
               begin
                 p1:=ctypeconvnode.create(p1,plist^.tt);
                 include(p1.flags,nf_absolute);
               end;
             sl_vec :
               p1:=cvecnode.create(p1,cordconstnode.create(plist^.value,plist^.valuett,true));
             else
               internalerror(200110205);
           end;
           plist:=plist^.next;
         end;
      end;


    function node_to_symlist(p1:tnode):tsymlist;
      var
        sl : tsymlist;

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
                  sl.addtype(sl_absolutetype,ttypeconvnode(p).totype)
                else
                  sl.addtype(sl_typeconv,ttypeconvnode(p).totype);
              end;
            vecn :
              begin
                addnode(tvecnode(p).left);
                if tvecnode(p).right.nodetype=ordconstn then
                  sl.addconst(sl_vec,tordconstnode(tvecnode(p).right).value,tvecnode(p).right.resulttype)
                else
                  begin
                    Message(parser_e_illegal_expression);
                    { recovery }
                    sl.addconst(sl_vec,0,tvecnode(p).right.resulttype);
                  end;
             end;
            loadn :
              sl.addsym(sl_load,tloadnode(p).symtableentry);
            else
              internalerror(200310282);
          end;
        end;

      begin
        sl:=tsymlist.create;
        addnode(p1);
        result:=sl;
      end;


    function parse_paras(__colon : boolean;end_of_paras : ttoken) : tnode;
      var
         p1,p2 : tnode;
         prev_in_args : boolean;
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
         { set para parsing values }
         in_args:=true;
         inc(parsing_para_level);
         allow_array_constructor:=true;
         p2:=nil;
         repeat
           p1:=comp_expr(true);
           p2:=ccallparanode.create(p1,p2);
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
         parse_paras:=p2;
      end;


     function gen_c_style_operator(ntyp:tnodetype;p1,p2:tnode) : tnode;
       var
         hp    : tnode;
         htype : ttype;
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
             resulttypepass(p1);
             result:=internalstatements(newstatement);
             htype.setdef(tpointerdef.create(p1.resulttype));
             temp:=ctempcreatenode.create(htype,sizeof(aint),tt_persistent,false);
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


     function statement_syssym(l : longint) : tnode;
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
                  if not (m_mac in aktmodeswitches) then
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
                              is_void(current_procinfo.procdef.rettype.def)) then
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
              if not (m_mac in aktmodeswitches) then
               statement_syssym:=cbreaknode.create
              else
                begin
                  Message1(sym_e_id_not_found, orgpattern);
                  statement_syssym:=cerrornode.create;
                end;
            end;

          in_continue :
            begin
              if not (m_mac in aktmodeswitches) then
                statement_syssym:=ccontinuenode.create
              else
                begin
                  Message1(sym_e_id_not_found, orgpattern);
                  statement_syssym:=cerrornode.create;
                end;
            end;

          in_leave :
            begin
              if m_mac in aktmodeswitches then
                statement_syssym:=cbreaknode.create
              else
                begin
                  Message1(sym_e_id_not_found, orgpattern);
                  statement_syssym:=cerrornode.create;
                end;
            end;

          in_cycle :
            begin
              if m_mac in aktmodeswitches then
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
              if (p1.resulttype.def.deftype = objectdef) or
                 (assigned(current_procinfo) and
                  ((po_classmethod in current_procinfo.procdef.procoptions) or
                   (po_staticmethod in current_procinfo.procdef.procoptions)) and
                  (p1.resulttype.def.deftype=classrefdef)) then
               statement_syssym:=geninlinenode(in_typeof_x,false,p1)
              else
               begin
                 Message(parser_e_class_id_expected);
                 p1.destroy;
                 statement_syssym:=cerrornode.create;
               end;
            end;

          in_sizeof_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_RKLAMMER);
              if (p1.nodetype<>typen) and
                 (
                  (is_object(p1.resulttype.def) and
                   (oo_has_constructor in tobjectdef(p1.resulttype.def).objectoptions)) or
                  is_open_array(p1.resulttype.def) or
                  is_array_of_const(p1.resulttype.def) or
                  is_open_string(p1.resulttype.def)
                 ) then
               statement_syssym:=geninlinenode(in_sizeof_x,false,p1)
              else
               begin
                 statement_syssym:=cordconstnode.create(p1.resulttype.def.size,sinttype,true);
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
                 case p1.resulttype.def.deftype of
                   procdef, { procvar }
                   pointerdef,
                   procvardef,
                   classrefdef : ;
                   objectdef :
                     if not is_class_or_interface(p1.resulttype.def) then
                       begin
                         Message(parser_e_illegal_parameter_list);
                         err:=true;
                       end;
                   arraydef :
                     if not is_dynamic_array(p1.resulttype.def) then
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
              if cs_typed_addresses in aktlocalswitches then
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
              do_resulttypepass(p1);
              { Ofs() returns a cardinal/qword, not a pointer }
              p1.resulttype:=uinttype;
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
                           (p1.resulttype.def.deftype=stringdef) or
                           is_chararray(p1.resulttype.def) or
                           is_char(p1.resulttype.def)
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
                 paras:=parse_paras(false,_RKLAMMER);
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
                 paras:=parse_paras(true,_RKLAMMER);
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
              paras:=parse_paras(true,_RKLAMMER);
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
                 p2:=cstringconstnode.createstr('',st_default);
               end;
              statement_syssym:=geninlinenode(l,false,ccallparanode.create(p1,ccallparanode.create(p2,nil)));
              consume(_RKLAMMER);
            end;

          else
            internalerror(15);

        end;
        in_args:=prev_in_args;
      end;


    function maybe_load_methodpointer(st:tsymtable;var p1:tnode):boolean;
      begin
        maybe_load_methodpointer:=false;
        if not assigned(p1) then
         begin
           case st.symtabletype of
             withsymtable :
               begin
                 if (st.defowner.deftype=objectdef) then
                   p1:=tnode(twithsymtable(st).withrefnode).getcopy;
               end;
             objectsymtable :
               begin
                 p1:=load_self_node;
                 { We are calling a member }
                 maybe_load_methodpointer:=true;
               end;
           end;
         end;
      end;


    { reads the parameter for a subroutine call }
    procedure do_proc_call(sym:tsym;st:tsymtable;obj:tobjectdef;getaddr:boolean;var again : boolean;var p1:tnode;callflags:tcallnodeflags);
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
               aprocdef:=Tprocsym(sym).search_procdef_byprocvardef(getprocvardef);
               getaddr:=true;
             end
            else
             if (m_tp_procvar in aktmodeswitches) or
                (m_mac_procvar in aktmodeswitches) then
              begin
                aprocdef:=Tprocsym(sym).search_procdef_byprocvardef(getprocvardef);
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
               aprocdef:=Tprocsym(sym).search_procdef_byprocvardef(getprocvardef);

             { generate a methodcallnode or proccallnode }
             { we shouldn't convert things like @tcollection.load }
             p2:=cloadnode.create_procvar(sym,aprocdef,st);
             if assigned(p1) then
              begin
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
                      para:=ccallparanode.create(cloadnode.create(currpara,currpara.owner),para);
                 end;
              end
             else
              begin
                if try_to_consume(_LKLAMMER) then
                 begin
                   para:=parse_paras(false,_RKLAMMER);
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
                 if (st.symtabletype<>objectsymtable) then
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
        if (m_tp_procvar in aktmodeswitches) or
           (m_mac_procvar in aktmodeswitches) then
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
              currprocdef:=tcallnode(hp).symtableprocentry.search_procdef_byprocvardef(pv);
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
    procedure handle_propertysym(sym : tsym;st : tsymtable;var p1 : tnode);
      var
         paras : tnode;
         p2    : tnode;
         membercall : boolean;
         callflags  : tcallnodeflags;
      begin
         paras:=nil;
         { property parameters? read them only if the property really }
         { has parameters                                             }
         if (ppo_hasparameters in tpropertysym(sym).propoptions) then
           begin
             if try_to_consume(_LECKKLAMMER) then
               begin
                 paras:=parse_paras(false,_RECKKLAMMER);
                 consume(_RECKKLAMMER);
               end;
           end;
         { indexed property }
         if (ppo_indexed in tpropertysym(sym).propoptions) then
           begin
             p2:=cordconstnode.create(tpropertysym(sym).index,tpropertysym(sym).indextype,true);
             paras:=ccallparanode.create(p2,paras);
           end;
         { we need only a write property if a := follows }
         { if not(afterassignment) and not(in_args) then }
         if token=_ASSIGNMENT then
           begin
              { write property: }
              if not tpropertysym(sym).writeaccess.empty then
                begin
                   case tpropertysym(sym).writeaccess.firstsym^.sym.typ of
                     procsym :
                       begin
                         callflags:=[];
                         { generate the method call }
                         membercall:=maybe_load_methodpointer(st,p1);
                         if membercall then
                           include(callflags,cnf_member_call);
                         p1:=ccallnode.create(paras,tprocsym(tpropertysym(sym).writeaccess.firstsym^.sym),st,p1,callflags);
                         paras:=nil;
                         consume(_ASSIGNMENT);
                         { read the expression }
                         if tpropertysym(sym).proptype.def.deftype=procvardef then
                           getprocvardef:=tprocvardef(tpropertysym(sym).proptype.def);
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
                         symlist_to_node(p1,st,tpropertysym(sym).writeaccess);
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
              { read property: }
              if not tpropertysym(sym).readaccess.empty then
                begin
                   case tpropertysym(sym).readaccess.firstsym^.sym.typ of
                     fieldvarsym :
                       begin
                          { generate access code }
                          symlist_to_node(p1,st,tpropertysym(sym).readaccess);
                          include(p1.flags,nf_isproperty);
                       end;
                     procsym :
                       begin
                          callflags:=[];
                          { generate the method call }
                          membercall:=maybe_load_methodpointer(st,p1);
                          if membercall then
                            include(callflags,cnf_member_call);
                          p1:=ccallnode.create(paras,tprocsym(tpropertysym(sym).readaccess.firstsym^.sym),st,p1,callflags);
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
         srsymtable  : tsymtable;
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
                 if not assigned(p1.resulttype.def) then
                  do_resulttypepass(p1);
                 isclassref:=(p1.resulttype.def.deftype=classrefdef);
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
                      do_resulttypepass(p1);
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
                            Message(parser_e_only_class_methods_via_class_ref);
                          p1:=csubscriptnode.create(sym,p1);
                        end;
                   end;
                 propertysym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      handle_propertysym(sym,sym.owner,p1);
                   end;
                 else internalerror(16);
              end;
           end;
      end;


{****************************************************************************
                               Factor
****************************************************************************}
{$ifdef fpc}
  {$maxfpuregisters 0}
{$endif fpc}

    function factor(getaddr : boolean) : tnode;

         {---------------------------------------------
                         Factor_read_id
         ---------------------------------------------}

       procedure factor_read_id(var p1:tnode;var again:boolean);
         var
           pc    : pchar;
           len   : longint;
           srsym : tsym;
           possible_error : boolean;
           srsymtable : tsymtable;
           storesymtablestack : tsymtable;
           htype : ttype;
           static_name : string;
         begin
           { allow post fix operators }
           again:=true;
           consume_sym(srsym,srsymtable);

           { Access to funcret or need to call the function? }
           if (srsym.typ in [absolutevarsym,localvarsym,paravarsym]) and
              (vo_is_funcret in tabstractvarsym(srsym).varoptions) and
              (
               (token=_LKLAMMER) or
               (
                (
                 (m_tp7 in aktmodeswitches) or
                 (m_delphi in aktmodeswitches)
                ) and
                (afterassignment or in_args) and
                not(vo_is_result in tabstractvarsym(srsym).varoptions)
               )
              ) then
            begin
              storesymtablestack:=symtablestack;
              symtablestack:=srsym.owner.next;
              searchsym(srsym.name,srsym,srsymtable);
              if not assigned(srsym) then
               srsym:=generrorsym;
              if (srsym.typ<>procsym) then
               Message(parser_e_illegal_expression);
              symtablestack:=storesymtablestack;
            end;

            begin
              case srsym.typ of
                absolutevarsym :
                  begin
                    if (tabsolutevarsym(srsym).abstyp=tovar) then
                      begin
                        p1:=nil;
                        symlist_to_node(p1,nil,tabsolutevarsym(srsym).ref);
                        p1:=ctypeconvnode.create(p1,tabsolutevarsym(srsym).vartype);
                        include(p1.flags,nf_absolute);
                      end
                    else
                      p1:=cloadnode.create(srsym,srsymtable);
                  end;

                globalvarsym,
                localvarsym,
                paravarsym,
                fieldvarsym :
                  begin
                    if (sp_static in srsym.symoptions) then
                     begin
                       static_name:=lower(srsym.owner.name^)+'_'+srsym.name;
                       searchsym(static_name,srsym,srsymtable);
		       if assigned(srsym) then
                         check_hints(srsym,srsym.symoptions);
                     end
                    else
                     begin
                       { are we in a class method, we check here the
                         srsymtable, because a field in another object
                         also has objectsymtable. And withsymtable is
                         not possible for self in class methods (PFV) }
                       if (srsymtable.symtabletype=objectsymtable) and
                          assigned(current_procinfo) and
                          (po_classmethod in current_procinfo.procdef.procoptions) then
                         Message(parser_e_only_class_methods);
                     end;

                    case srsymtable.symtabletype of
                      objectsymtable :
                        begin
                          p1:=csubscriptnode.create(srsym,load_self_node);
                          node_tree_set_filepos(p1,aktfilepos);
                        end;
                      withsymtable :
                        begin
                          p1:=csubscriptnode.create(srsym,tnode(twithsymtable(srsymtable).withrefnode).getcopy);
                          node_tree_set_filepos(p1,aktfilepos);
                        end;
                      else
                        p1:=cloadnode.create(srsym,srsymtable);
                    end;
                  end;

                typedconstsym :
                  begin
                    p1:=cloadnode.create(srsym,srsymtable);
                  end;

                syssym :
                  begin
                    p1:=statement_syssym(tsyssym(srsym).number);
                  end;

                typesym :
                  begin
                    htype.setsym(srsym);
                    if not assigned(htype.def) then
                     begin
                       again:=false;
                     end
                    else
                     begin
                       { We need to know if this unit uses Variants }
                       if (htype.def=cvarianttype.def) and
                          not(cs_compilesystem in aktmoduleswitches) then
                         current_module.flags:=current_module.flags or uf_uses_variants;
                       if try_to_consume(_LKLAMMER) then
                        begin
                          p1:=comp_expr(true);
                          consume(_RKLAMMER);
                          p1:=ctypeconvnode.create_explicit(p1,htype);
                        end
                       else { not LKLAMMER }
                        if (token=_POINT) and
                           is_object(htype.def) then
                         begin
                           consume(_POINT);
                           if assigned(current_procinfo) and
                              assigned(current_procinfo.procdef._class) and
                              not(getaddr) then
                            begin
                              if current_procinfo.procdef._class.is_related(tobjectdef(htype.def)) then
                               begin
                                 p1:=ctypenode.create(htype);
                                 { search also in inherited methods }
                                 srsym:=searchsym_in_class(tobjectdef(htype.def),pattern);
				 if assigned(srsym) then
                                   check_hints(srsym,srsym.symoptions);
                                 consume(_ID);
                                 do_member_read(tobjectdef(htype.def),false,srsym,p1,again,[]);
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
                              p1:=ctypenode.create(htype);
                              { TP allows also @TMenu.Load if Load is only }
                              { defined in an anchestor class              }
                              srsym:=search_class_member(tobjectdef(htype.def),pattern);
                              if assigned(srsym) then
			        begin
                                  check_hints(srsym,srsym.symoptions);
  				  if not(getaddr) and not(sp_static in srsym.symoptions) then
                                    Message(sym_e_only_static_in_static)
                                  else
                                    begin
                                      consume(_ID);
                                      do_member_read(tobjectdef(htype.def),getaddr,srsym,p1,again,[]);
				    end;  
                                end
			      else	
                                Message1(sym_e_id_no_member,orgpattern);
                            end;
                         end
                       else
                        begin
                          { class reference ? }
                          if is_class(htype.def) then
                           begin
                             if getaddr and (token=_POINT) then
                              begin
                                consume(_POINT);
                                { allows @Object.Method }
                                { also allows static methods and variables }
                                p1:=ctypenode.create(htype);
                                { TP allows also @TMenu.Load if Load is only }
                                { defined in an anchestor class              }
                                srsym:=search_class_member(tobjectdef(htype.def),pattern);
                                if assigned(srsym) then
                                 begin
                                   check_hints(srsym,srsym.symoptions);
                                   consume(_ID);
                                   do_member_read(tobjectdef(htype.def),getaddr,srsym,p1,again,[]);
                                 end
				else 
				 begin
                                   Message1(sym_e_id_no_member,orgpattern);
                                   consume(_ID);
				 end;
                              end
                             else
                              begin
                                p1:=ctypenode.create(htype);
                                { For a type block we simply return only
                                  the type. For all other blocks we return
                                  a loadvmt node }
                                if (block_type<>bt_type) then
                                  p1:=cloadvmtaddrnode.create(p1);
                              end;
                           end
                          else
                           p1:=ctypenode.create(htype);
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
                          if tconstsym(srsym).consttype.def=nil then
                            internalerror(200403232);
                          p1:=cordconstnode.create(tconstsym(srsym).value.valueord,tconstsym(srsym).consttype,true);
                        end;
                      conststring :
                        begin
                          len:=tconstsym(srsym).value.len;
                          if not(cs_ansistrings in aktlocalswitches) and (len>255) then
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
                        p1:=csetconstnode.create(pconstset(tconstsym(srsym).value.valueptr),tconstsym(srsym).consttype);
                      constpointer :
                        p1:=cpointerconstnode.create(tconstsym(srsym).value.valueordptr,tconstsym(srsym).consttype);
                      constnil :
                        p1:=cnilnode.create;
                      constresourcestring:
                        begin
                          p1:=cloadnode.create(srsym,srsymtable);
                          do_resulttypepass(p1);
                        {$ifdef ansistring_bits}
                          case aktansistring_bits of
                            sb_16:
                              p1.resulttype:=cansistringtype16;
                            sb_32:
                              p1.resulttype:=cansistringtype32;
                            sb_64:
                              p1.resulttype:=cansistringtype64;
                          end;
                        {$else}
                          p1.resulttype:=cansistringtype;
                        {$endif}
                        end;
                      constguid :
                        p1:=cguidconstnode.create(pguid(tconstsym(srsym).value.valueptr)^);
                      else
                        internalerror(200507181);
                    end;
                  end;

                procsym :
                  begin
                    { are we in a class method ? }
                    possible_error:=(srsymtable.symtabletype<>withsymtable) and
                                    (srsym.owner.symtabletype=objectsymtable) and
                                    not(is_interface(tdef(srsym.owner.defowner))) and
                                    assigned(current_procinfo) and
                                    (po_classmethod in current_procinfo.procdef.procoptions);
                    do_proc_call(srsym,srsymtable,nil,
                                 (getaddr and not(token in [_CARET,_POINT])),
                                 again,p1,[]);
                    { we need to know which procedure is called }
                    if possible_error then
                     begin
                       do_resulttypepass(p1);
                       if (p1.nodetype=calln) and
                          assigned(tcallnode(p1).procdefinition) and
                          not(tcallnode(p1).procdefinition.proctypeoption=potype_constructor) and
                          not(po_classmethod in tcallnode(p1).procdefinition.procoptions) then
                         Message(parser_e_only_class_methods);
                     end;
                  end;

                propertysym :
                  begin
                    { access to property in a method }
                    { are we in a class method ? }
                    if (srsymtable.symtabletype=objectsymtable) and
                       assigned(current_procinfo) and
                       (po_classmethod in current_procinfo.procdef.procoptions) then
                     Message(parser_e_only_class_methods);
                    { no method pointer }
                    p1:=nil;
                    handle_propertysym(srsym,srsymtable,p1);
                  end;

                labelsym :
                  begin
                    { Support @label }
                    if getaddr then
                      p1:=cloadnode.create(srsym,srsym.owner)
                    else
                      begin
                        consume(_COLON);
                        if tlabelsym(srsym).defined then
                          Message(sym_e_label_already_defined);
                        tlabelsym(srsym).defined:=true;
                        p1:=clabelnode.create(nil);
                        tlabelsym(srsym).code:=p1;
                      end;
                  end;

                errorsym :
                  begin
                    p1:=cerrornode.create;
                    if try_to_consume(_LKLAMMER) then
                     begin
                       parse_paras(false,_RKLAMMER);
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
            temp:=ctempcreatenode.create(sinttype,4,tt_persistent,false);
            addstatement(newstatement,temp);

            countindices:=0;
            repeat
              p4:=comp_expr(true);

              addstatement(newstatement,cassignmentnode.create(
                ctemprefnode.create_offset(temp,countindices*sinttype.def.size),p4));
               inc(countindices);
            until not try_to_consume(_COMMA);

            { set real size }
            temp.size:=countindices*sinttype.def.size;

            consume(_RECKKLAMMER);

            { we need only a write access if a := follows }
            if token=_ASSIGNMENT then
              begin
                consume(_ASSIGNMENT);
                p4:=comp_expr(true);

                { create call to fpc_vararray_put }
                paras:=ccallparanode.create(cordconstnode.create
                      (countindices,sinttype,true),
                   ccallparanode.create(caddrnode.create_internal
                  (ctemprefnode.create(temp)),
                   ccallparanode.create(ctypeconvnode.create_internal(p4,cvarianttype),
                   ccallparanode.create(p1
                     ,nil))));

                addstatement(newstatement,ccallnode.createintern('fpc_vararray_put',paras));
                addstatement(newstatement,ctempdeletenode.create(temp));
              end
            else
              begin
                { create temp for result }
                tempresultvariant:=ctempcreatenode.create(cvarianttype,cvarianttype.def.size,tt_persistent,true);
                addstatement(newstatement,tempresultvariant);

                { create call to fpc_vararray_get }
                paras:=ccallparanode.create(cordconstnode.create
                      (countindices,sinttype,true),
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
          store_static : boolean;
          protsym  : tpropertysym;
          p2,p3 : tnode;
          hsym  : tsym;
          classh : tobjectdef;

        label
          skipreckklammercheck;
        begin
          again:=true;
          while again do
           begin
             { we need the resulttype }
             do_resulttypepass(p1);

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
                    if ((m_tp_procvar in aktmodeswitches) or
                        (m_mac_procvar in aktmodeswitches)) and
                       (p1.resulttype.def.deftype=procvardef) and
                       (tprocvardef(p1.resulttype.def).rettype.def.deftype=pointerdef) then
                      begin
                        p1:=ccallnode.create_procvar(nil,p1);
                        resulttypepass(p1);
                      end;

                    if (p1.resulttype.def.deftype<>pointerdef) then
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
                    if is_class_or_interface(p1.resulttype.def) then
                      begin
                        { default property }
                        protsym:=search_default_property(tobjectdef(p1.resulttype.def));
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
                            inc(protsym.refs);
                            handle_propertysym(protsym,protsym.owner,p1);
                          end;
                      end
                    else
                      begin
                        consume(_LECKKLAMMER);
                        repeat
                          case p1.resulttype.def.deftype of
                            pointerdef:
                              begin
                                 { support delphi autoderef }
                                 if (tpointerdef(p1.resulttype.def).pointertype.def.deftype=arraydef) and
                                    (m_autoderef in aktmodeswitches) then
                                  begin
                                    p1:=cderefnode.create(p1);
                                  end;
                                 p2:=comp_expr(true);
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
                                p1:=cvecnode.create(p1,p2);
                              end;
                            arraydef :
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
                                       p1:=cvecnode.create(p1,p2);
                                       include(tvecnode(p1).flags,nf_memseg);
                                       include(tvecnode(p1).flags,nf_memindex);
                                     end
                                    else
                                     begin
                                       p1:=cvecnode.create(p1,p2);
                                       include(tvecnode(p1).flags,nf_memindex);
                                     end;
                                  end
                                else
                                  p1:=cvecnode.create(p1,p2);
                              end;
                            else
                              begin
                                Message(parser_e_invalid_qualifier);
                                p1.destroy;
                                p1:=cerrornode.create;
                                comp_expr(true);
                                again:=false;
                              end;
                          end;
                          do_resulttypepass(p1);
                        until not try_to_consume(_COMMA);
                        consume(_RECKKLAMMER);
                        { handle_variantarray eats the RECKKLAMMER and jumps here }
                      skipreckklammercheck:
                      end;
                  end;

               _POINT :
                  begin
                    consume(_POINT);
                    if (p1.resulttype.def.deftype=pointerdef) and
                       (m_autoderef in aktmodeswitches) then
                      begin
                        p1:=cderefnode.create(p1);
                        do_resulttypepass(p1);
                      end;
                    case p1.resulttype.def.deftype of
                      recorddef:
                        begin
                          if token=_ID then
                            begin
                              hsym:=tsym(trecorddef(p1.resulttype.def).symtable.search(pattern));
                              if assigned(hsym) and
                                 (hsym.typ=fieldvarsym) then
				begin 
                                  check_hints(hsym,hsym.symoptions);
                                  p1:=csubscriptnode.create(hsym,p1)
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
                         end;
                       classrefdef:
                         begin
                           if token=_ID then
                             begin
                               classh:=tobjectdef(tclassrefdef(p1.resulttype.def).pointertype.def);
                               hsym:=searchsym_in_class(classh,pattern);
                               if hsym=nil then
                                 begin
                                   Message1(sym_e_id_no_member,orgpattern);
                                   p1.destroy;
                                   p1:=cerrornode.create;
                                   { try to clean up }
                                   consume(_ID);
                                 end
                               else
                                 begin
                                   check_hints(hsym,hsym.symoptions);
                                   consume(_ID);
                                   do_member_read(classh,getaddr,hsym,p1,again,[]);
                                 end;
                             end
                           else { Error }
                             Consume(_ID);
                         end;
                       objectdef:
                         begin
                           if token=_ID then
                             begin
                               store_static:=allow_only_static;
                               allow_only_static:=false;
                               classh:=tobjectdef(p1.resulttype.def);
                               hsym:=searchsym_in_class(classh,pattern);
                               allow_only_static:=store_static;
                               if hsym=nil then
                                 begin
                                    Message1(sym_e_id_no_member,orgpattern);
                                    p1.destroy;
                                    p1:=cerrornode.create;
                                    { try to clean up }
                                    consume(_ID);
                                 end
                               else
                                 begin
                                    check_hints(hsym,hsym.symoptions);
                                    consume(_ID);
                                    do_member_read(classh,getaddr,hsym,p1,again,[]);
                                 end;
                             end
                           else { Error }
                             Consume(_ID);
                         end;
                       pointerdef:
                         begin
                           Message(parser_e_invalid_qualifier);
                           if tpointerdef(p1.resulttype.def).pointertype.def.deftype in [recorddef,objectdef,classrefdef] then
                             Message(parser_h_maybe_deref_caret_missing);
                         end;
                       else
                         begin
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
                   if assigned(p1.resulttype.def) and
                      (p1.resulttype.def.deftype=procvardef) then
                     begin
                       if assigned(getprocvardef) and
                          equal_defs(p1.resulttype.def,getprocvardef) then
                         again:=false
                       else
                         begin
                           if try_to_consume(_LKLAMMER) then
                             begin
                               p2:=parse_paras(false,_RKLAMMER);
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
         l        : longint;
         ic       : int64;
         qc       : qword;
{$ifndef cpu64}
         card     : cardinal;
{$endif cpu64}
         oldp1,
         p1       : tnode;
         code     : integer;
         again    : boolean;
         sym      : tsym;
         pd       : tprocdef;
         classh   : tobjectdef;
         d        : bestreal;
         hs,hsorg : string;
         htype    : ttype;
         filepos  : tfileposinfo;

         {---------------------------------------------
                           Helpers
         ---------------------------------------------}

        procedure check_tokenpos;
        begin
          if (p1<>oldp1) then
           begin
             if assigned(p1) then
               p1.fileinfo:=filepos;
             oldp1:=p1;
             filepos:=akttokenpos;
           end;
        end;

      begin
        oldp1:=nil;
        p1:=nil;
        filepos:=akttokenpos;
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
              check_tokenpos;

              { handle post fix operators }
              postfixoperators(p1,again);
            end;
         end
        else
         case token of
           _RETURN :
              begin
                consume(_RETURN);
                p1 := cexitnode.create(comp_expr(true));
              end;
           _INHERITED :
             begin
               again:=true;
               consume(_INHERITED);
               if assigned(current_procinfo) and
                  assigned(current_procinfo.procdef._class) then
                begin
                  classh:=current_procinfo.procdef._class.childof;
                  { if inherited; only then we need the method with
                    the same name }
                  if token in endtokens then
                   begin
                     hs:=current_procinfo.procdef.procsym.name;
                     hsorg:=current_procinfo.procdef.procsym.realname;
                     anon_inherited:=true;
                     { For message methods we need to search using the message
                       number or string }
                     pd:=tprocsym(current_procinfo.procdef.procsym).first_procdef;
                     if (po_msgint in pd.procoptions) then
                      sym:=searchsym_in_class_by_msgint(classh,pd.messageinf.i)
                     else
                      if (po_msgstr in pd.procoptions) then
                       sym:=searchsym_in_class_by_msgstr(classh,pd.messageinf.str)
                     else
                      sym:=searchsym_in_class(classh,hs);
                   end
                  else
                   begin
                     hs:=pattern;
                     hsorg:=orgpattern;
                     consume(_ID);
                     anon_inherited:=false;
                     sym:=searchsym_in_class(classh,hs);
                   end;
                  if assigned(sym) then
                   begin
                     check_hints(sym,sym.symoptions);
                     { load the procdef from the inherited class and
                       not from self }
                     if sym.typ in [procsym,propertysym] then
                      begin
                        if (sym.typ = procsym) then
                          begin
                            htype.setdef(classh);
                            if (po_classmethod in current_procinfo.procdef.procoptions) or
                               (po_staticmethod in current_procinfo.procdef.procoptions) then
                              htype.setdef(tclassrefdef.create(htype));
                            p1:=ctypenode.create(htype);
                          end;
                      end
                     else
                      begin
                        Message(parser_e_methode_id_expected);
                        p1:=cerrornode.create;
                      end;
                     do_member_read(classh,false,sym,p1,again,[cnf_inherited,cnf_anon_inherited]);
                   end
                  else
                   begin
                     if anon_inherited then
                      begin
                        { For message methods we need to call DefaultHandler }
                        if (po_msgint in pd.procoptions) or
                           (po_msgstr in pd.procoptions) then
                          begin
                            sym:=searchsym_in_class(classh,'DEFAULTHANDLER');
                            if not assigned(sym) or
                               (sym.typ<>procsym) then
                              internalerror(200303171);
                            p1:=nil;
                            do_proc_call(sym,sym.owner,classh,false,again,p1,[]);
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
                    int_to_type(ic,htype);
                    p1:=cordconstnode.create(ic,htype,true);
                 end
               else
                 begin
                   { try qword next }
                   val(pattern,qc,code);
                   if code=0 then
                     begin
                        consume(_INTCONST);
                        htype:=u64inttype;
                        p1:=cordconstnode.create(qc,htype,true);
                     end;
                 end;
{$else}
               { try cardinal first }
               val(pattern,card,code);
               if code=0 then
                 begin
                    consume(_INTCONST);
                    int_to_type(card,htype);
                    p1:=cordconstnode.create(card,htype,true);
                 end
               else
                 begin
                   { then longint }
                   val(pattern,l,code);
                   if code = 0 then
                     begin
                       consume(_INTCONST);
                       int_to_type(l,htype);
                       p1:=cordconstnode.create(l,htype,true);
                     end
                   else
                     begin
                       { then int64 }
                       val(pattern,ic,code);
                       if code=0 then
                         begin
                            consume(_INTCONST);
                            int_to_type(ic,htype);
                            p1:=cordconstnode.create(ic,htype,true);
                         end
                       else
                         begin
                           { try qword next }
                           val(pattern,qc,code);
                           if code=0 then
                             begin
                                consume(_INTCONST);
                                htype:=u64inttype;
                                p1:=cordconstnode.create(tconstexprint(qc),htype,true);
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
                 end;
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
               p1:=crealconstnode.create(d,pbestrealtype^);
             end;

           _STRING :
             begin
               string_dec(htype);
               { STRING can be also a type cast }
               if try_to_consume(_LKLAMMER) then
                begin
                  p1:=comp_expr(true);
                  consume(_RKLAMMER);
                  p1:=ctypeconvnode.create_explicit(p1,htype);
                  { handle postfix operators here e.g. string(a)[10] }
                  again:=true;
                  postfixoperators(p1,again);
                end
               else
                p1:=ctypenode.create(htype);
             end;

           _FILE :
             begin
               htype:=cfiletype;
               consume(_FILE);
               { FILE can be also a type cast }
               if try_to_consume(_LKLAMMER) then
                begin
                  p1:=comp_expr(true);
                  consume(_RKLAMMER);
                  p1:=ctypeconvnode.create_explicit(p1,htype);
                  { handle postfix operators here e.g. string(a)[10] }
                  again:=true;
                  postfixoperators(p1,again);
                end
               else
                begin
                  p1:=ctypenode.create(htype);
                end;
             end;

           _CSTRING :
             begin
               p1:=cstringconstnode.createstr(pattern,st_default);
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
               if cs_typed_addresses in aktlocalswitches then
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
               p1:=cerrornode.create;
               consume(token);
               Message(parser_e_illegal_expression);
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

        { get the resulttype for the node }
        if (not assigned(p1.resulttype.def)) then
         do_resulttypepass(p1);

        factor:=p1;
        check_tokenpos;
      end;
{$ifdef fpc}
  {$maxfpuregisters default}
{$endif fpc}

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
             filepos:=akttokenpos;
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
                 p1:=caddnode.create(orn,p1,p2);
               _OP_AND,
               _AMPERSAND {macpas only} :
                 p1:=caddnode.create(andn,p1,p2);
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
               _CARET :
                 p1:=caddnode.create(caretn,p1,p2);
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
         { get the resulttype for this expression }
         if not assigned(p1.resulttype.def) then
          do_resulttypepass(p1);
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
         { get the resulttype for this expression }
         if not assigned(p1.resulttype.def) then
          do_resulttypepass(p1);
         filepos:=akttokenpos;
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
                if (p1.resulttype.def.deftype=procvardef) then
                  getprocvardef:=tprocvardef(p1.resulttype.def);
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
         { get the resulttype for this expression }
         if not assigned(p1.resulttype.def) then
          do_resulttypepass(p1);
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
            not(is_integer(p.resulttype.def)) then
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
          if (p.nodetype=ordconstn) and is_char(p.resulttype.def) then
            get_stringconst:=char(tordconstnode(p).value)
          else
            Message(parser_e_illegal_expression);
        end
      else
        get_stringconst:=strpas(tstringconstnode(p).value_str);
      p.free;
    end;

end.

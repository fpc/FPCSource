{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
      symtype,
      node,
      cpuinfo;

    { reads a whole expression }
    function expr : tnode;

    { reads an expression without assignements and .. }
    function comp_expr(accept_equal : boolean):tnode;

    { reads a single factor }
    function factor(getaddr : boolean) : tnode;

    procedure string_dec(var t: ttype);

    { the ID token has to be consumed before calling this function }
    procedure do_member_read(getaddr : boolean;sym : tsym;var p1 : tnode;var again : boolean);

{$ifdef int64funcresok}
    function get_intconst:TConstExprInt;
{$else int64funcresok}
    function get_intconst:longint;
{$endif int64funcresok}

    function get_stringconst:string;

implementation

    uses
{$ifdef delphi}
       SysUtils,
{$endif}
       { common }
       cutils,
       { global }
       globtype,globals,tokens,verbose,
       systems,widestr,
       { symtable }
       symconst,symbase,symdef,symsym,symtable,types,
       { pass 1 }
       pass_1,htypechk,
       nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,nbas,
       { parser }
       scanner,
       pbase,
       { codegen }
       cgbase
       ;

    { sub_expr(opmultiply) is need to get -1 ** 4 to be
      read as - (1**4) and not (-1)**4 PM }
    type
      Toperator_precedence=(opcompare,opaddition,opmultiply,oppower);

    const
      highest_precedence = oppower;

    function sub_expr(pred_level:Toperator_precedence;accept_equal : boolean):tnode;forward;

    const
      got_addrn  : boolean = false;
      auto_inherited : boolean = false;

    procedure string_dec(var t: ttype);
    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
      var
         p : tnode;
      begin
         t:=cshortstringtype;
         consume(_STRING);
         if token=_LECKKLAMMER then
           begin
              consume(_LECKKLAMMER);
              p:=comp_expr(true);
              if not is_constintnode(p) then
                begin
                  Message(cg_e_illegal_expression);
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
                   t.setdef(tstringdef.createlong(tordconstnode(p).value))
                 else
                   if tordconstnode(p).value<>255 then
                     t.setdef(tstringdef.createshort(tordconstnode(p).value));
               end;
              p.free;
           end
          else
            begin
               if cs_ansistrings in aktlocalswitches then
                 t:=cansistringtype
               else
                 t:=cshortstringtype;
            end;
       end;



    function parse_paras(__colon,in_prop_paras : boolean) : tnode;

      var
         p1,p2 : tnode;
         end_of_paras : ttoken;
         prev_in_args : boolean;
         old_allow_array_constructor : boolean;
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
         { save old values }
         prev_in_args:=in_args;
         old_allow_array_constructor:=allow_array_constructor;
         { set para parsing values }
         in_args:=true;
         inc(parsing_para_level);
         allow_array_constructor:=true;
         p2:=nil;
         while true do
           begin
              p1:=comp_expr(true);
              p2:=ccallparanode.create(p1,p2);
              { it's for the str(l:5,s); }
              if __colon and (token=_COLON) then
                begin
                   consume(_COLON);
                   p1:=comp_expr(true);
                   p2:=ccallparanode.create(p1,p2);
                   include(tcallparanode(p2).callparaflags,cpf_is_colon_para);
                   if token=_COLON then
                     begin
                        consume(_COLON);
                        p1:=comp_expr(true);
                        p2:=ccallparanode.create(p1,p2);
                        include(tcallparanode(p2).callparaflags,cpf_is_colon_para);
                     end
                end;
              if token=_COMMA then
                consume(_COMMA)
              else
                break;
           end;
         allow_array_constructor:=old_allow_array_constructor;
         dec(parsing_para_level);
         in_args:=prev_in_args;
         parse_paras:=p2;
      end;


    procedure check_tp_procvar(var p : tnode);
      var
         p1 : tnode;
      begin
         if (m_tp_procvar in aktmodeswitches) and
            (not got_addrn) and
            (not in_args) and
            (p.nodetype=loadn) then
            begin
               { support if procvar then for tp7 and many other expression like this }
               do_resulttypepass(p);
               set_varstate(p,false);
               { reset varstateset to maybe set used state later web bug769 PM }
               unset_varstate(p);
               if (getprocvardef=nil) and (p.resulttype.def.deftype=procvardef) then
                 begin
                    p1:=ccallnode.create(nil,nil,nil,nil);
                    tcallnode(p1).set_procvar(p);
                    resulttypepass(p1);
                    p:=p1;
                 end;
            end;
      end;


    function new_dispose_statement(is_new:boolean) : tnode;
      var
        p,p2     : tnode;
        again    : boolean; { dummy for do_proc_call }
        destructorname : stringid;
        sym      : tsym;
        classh   : tobjectdef;
        destructorpos,
        storepos : tfileposinfo;
      begin
        consume(_LKLAMMER);
        p:=comp_expr(true);
        { calc return type }
        { cleartempgen; }
        set_varstate(p,(not is_new));
        { constructor,destructor specified }
        if try_to_consume(_COMMA) then
          begin
            { extended syntax of new and dispose }
            { function styled new is handled in factor }
            { destructors have no parameters }
            destructorname:=pattern;
            destructorpos:=akttokenpos;
            consume(_ID);

            if (p.resulttype.def.deftype<>pointerdef) then
              begin
                 Message1(type_e_pointer_type_expected,p.resulttype.def.typename);
                 p.free;
                 p:=factor(false);
                 p.free;
                 consume(_RKLAMMER);
                 new_dispose_statement:=cerrornode.create;
                 exit;
              end;
            { first parameter must be an object or class }
            if tpointerdef(p.resulttype.def).pointertype.def.deftype<>objectdef then
              begin
                 Message(parser_e_pointer_to_class_expected);
                 p.free;
                 new_dispose_statement:=factor(false);
                 consume_all_until(_RKLAMMER);
                 consume(_RKLAMMER);
                 exit;
              end;
            { check, if the first parameter is a pointer to a _class_ }
            classh:=tobjectdef(tpointerdef(p.resulttype.def).pointertype.def);
            if is_class(classh) then
              begin
                 Message(parser_e_no_new_or_dispose_for_classes);
                 new_dispose_statement:=factor(false);
                 consume_all_until(_RKLAMMER);
                 consume(_RKLAMMER);
                 exit;
              end;
            { search cons-/destructor, also in parent classes }
            storepos:=akttokenpos;
            akttokenpos:=destructorpos;
            sym:=search_class_member(classh,destructorname);
            akttokenpos:=storepos;

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
                if is_new then
                 p2:=chnewnode.create
                else
                 p2:=chdisposenode.create(p);
                do_resulttypepass(p2);
                p2.resulttype:=tpointerdef(p.resulttype.def).pointertype;
                if is_new then
                  do_member_read(false,sym,p2,again)
                else
                  begin
                    if (m_tp in aktmodeswitches) then
                      do_member_read(false,sym,p2,again)
                    else
                      begin
                        p2:=ccallnode.create(nil,tprocsym(sym),sym.owner,p2);
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
                { cleartempgen;}
                do_resulttypepass(p2);
                if not codegenerror then
                 begin
                   if is_new then
                    begin
                      if (tcallnode(p2).procdefinition.proctypeoption<>potype_constructor) then
                        Message(parser_e_expr_have_to_be_constructor_call);
                      p2:=cnewnode.create(p2);
                      do_resulttypepass(p2);
                      p2.resulttype:=p.resulttype;
                      p2:=cassignmentnode.create(p,p2);
                    end
                   else
                    begin
                      if (tcallnode(p2).procdefinition.proctypeoption<>potype_destructor) then
                        Message(parser_e_expr_have_to_be_destructor_call);
                    end;
                 end;
                new_dispose_statement:=p2;
              end;
          end
        else
          begin
             if (p.resulttype.def.deftype<>pointerdef) then
               Begin
                  Message1(type_e_pointer_type_expected,p.resulttype.def.typename);
                  new_dispose_statement:=cerrornode.create;
               end
             else
               begin
                  if (tpointerdef(p.resulttype.def).pointertype.def.deftype=objectdef) and
                     (oo_has_vmt in tobjectdef(tpointerdef(p.resulttype.def).pointertype.def).objectoptions) then
                    Message(parser_w_use_extended_syntax_for_objects);
                  if (tpointerdef(p.resulttype.def).pointertype.def.deftype=orddef) and
                     (torddef(tpointerdef(p.resulttype.def).pointertype.def).typ=uvoid) then
                    begin
                      if (m_tp in aktmodeswitches) or
                         (m_delphi in aktmodeswitches) then
                       Message(parser_w_no_new_dispose_on_void_pointers)
                      else
                       Message(parser_e_no_new_dispose_on_void_pointers);
                    end;

                  if is_new then
                    new_dispose_statement:=csimplenewdisposenode.create(simplenewn,p)
                  else
                    new_dispose_statement:=csimplenewdisposenode.create(simpledisposen,p);
               end;
          end;
        consume(_RKLAMMER);
      end;


    function new_function : tnode;
      var
        p1,p2  : tnode;
        classh : tobjectdef;
        sym    : tsym;
        again  : boolean; { dummy for do_proc_call }
      begin
        consume(_LKLAMMER);
        p1:=factor(false);
        if p1.nodetype<>typen then
         begin
           Message(type_e_type_id_expected);
           p1.destroy;
           p1:=cerrornode.create;
           do_resulttypepass(p1);
         end;

        if (p1.resulttype.def.deftype<>pointerdef) then
          Message1(type_e_pointer_type_expected,p1.resulttype.def.typename)
        else
         if token=_RKLAMMER then
          begin
            if (tpointerdef(p1.resulttype.def).pointertype.def.deftype=objectdef) and
               (oo_has_vmt in tobjectdef(tpointerdef(p1.resulttype.def).pointertype.def).objectoptions)  then
             Message(parser_w_use_extended_syntax_for_objects);
            p2:=cnewnode.create(nil);
            do_resulttypepass(p2);
            p2.resulttype:=p1.resulttype;
            p1.destroy;
            p1:=p2;
            consume(_RKLAMMER);
          end
        else
          begin
            p2:=chnewnode.create;
            do_resulttypepass(p2);
            p2.resulttype:=tpointerdef(p1.resulttype.def).pointertype;
            consume(_COMMA);
            afterassignment:=false;
            { determines the current object defintion }
            classh:=tobjectdef(p2.resulttype.def);
            if classh.deftype<>objectdef then
             Message(parser_e_pointer_to_class_expected)
            else
             begin
               { check for an abstract class }
               if (oo_has_abstract in classh.objectoptions) then
                Message(sym_e_no_instance_of_abstract_object);
               { search the constructor also in the symbol tables of
                 the parents }
               sym:=nil;
               while assigned(classh) do
                begin
                  sym:=tsym(classh.symtable.search(pattern));
                  if assigned(sym) then
                   break;
                  classh:=classh.childof;
                end;
               consume(_ID);
               do_member_read(false,sym,p2,again);
               { we need to know which procedure is called }
               do_resulttypepass(p2);
               if (p2.nodetype<>calln) or
                  (assigned(tcallnode(p2).procdefinition) and
                   (tcallnode(p2).procdefinition.proctypeoption<>potype_constructor)) then
                Message(parser_e_expr_have_to_be_constructor_call);
             end;
            p2:=cnewnode.create(p2);
            do_resulttypepass(p2);
            p2.resulttype:=p1.resulttype;
            p1.destroy;
            p1:=p2;
            consume(_RKLAMMER);
          end;
        new_function:=p1;
      end;


     function statement_syssym(l : longint) : tnode;
      var
        p1,p2,paras  : tnode;
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

          in_break :
            begin
              statement_syssym:=cbreaknode.create;
            end;

          in_continue :
            begin
              statement_syssym:=ccontinuenode.create;
            end;

          in_typeof_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              consume(_RKLAMMER);
              if p1.nodetype=typen then
                ttypenode(p1).allowed:=true;
              if p1.resulttype.def.deftype=objectdef then
               statement_syssym:=geninlinenode(in_typeof_x,false,p1)
              else
               begin
                 Message(type_e_mismatch);
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
                  ((p1.resulttype.def.deftype=objectdef) and
                   (oo_has_constructor in tobjectdef(p1.resulttype.def).objectoptions)) or
                  is_open_array(p1.resulttype.def) or
                  is_open_string(p1.resulttype.def)
                 ) then
               statement_syssym:=geninlinenode(in_sizeof_x,false,p1)
              else
               begin
                 statement_syssym:=cordconstnode.create(p1.resulttype.def.size,s32bittype);
                 { p1 not needed !}
                 p1.destroy;
               end;
            end;

          in_typeinfo_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if p1.nodetype=typen then
                ttypenode(p1).allowed:=true
              else
                begin
                   p1.destroy;
                   p1:=cerrornode.create;
                   Message(parser_e_illegal_parameter_list);
                end;
              consume(_RKLAMMER);
              p2:=ccallparanode.create(p1,nil);
              p2:=geninlinenode(in_typeinfo_x,false,p2);
              statement_syssym:=p2;
            end;

          in_assigned_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if not codegenerror then
               begin
                 case p1.resulttype.def.deftype of
                   pointerdef,
                   procvardef,
                   classrefdef : ;
                   objectdef :
                     if not is_class_or_interface(p1.resulttype.def) then
                       Message(parser_e_illegal_parameter_list);
                   else
                     Message(parser_e_illegal_parameter_list);
                 end;
               end;
              p2:=ccallparanode.create(p1,nil);
              p2:=geninlinenode(in_assigned_x,false,p2);
              consume(_RKLAMMER);
              statement_syssym:=p2;
            end;

          in_addr_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p1:=caddrnode.create(p1);
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
              { Ofs() returns a cardinal, not a pointer }
              p1.resulttype:=u32bittype;
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
              if token=_COMMA then
               begin
                 consume(_COMMA);
                 p2:=ccallparanode.create(comp_expr(true),nil);
               end
              else
               p2:=nil;
              p2:=ccallparanode.create(p1,p2);
              statement_syssym:=geninlinenode(l,false,p2);
              consume(_RKLAMMER);
            end;

          in_finalize_x:
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if token=_COMMA then
               begin
                 consume(_COMMA);
                 p2:=ccallparanode.create(comp_expr(true),nil);
               end
              else
               p2:=nil;
              p2:=ccallparanode.create(p1,p2);
              statement_syssym:=geninlinenode(in_finalize_x,false,p2);
              consume(_RKLAMMER);
            end;

          in_concat_x :
            begin
              consume(_LKLAMMER);
              in_args:=true;
              p2:=nil;
              while true do
               begin
                 p1:=comp_expr(true);
                 set_varstate(p1,true);
                 if not((p1.resulttype.def.deftype=stringdef) or
                        ((p1.resulttype.def.deftype=orddef) and
                         (torddef(p1.resulttype.def).typ=uchar))) then
                   Message(parser_e_illegal_parameter_list);
                 if p2<>nil then
                  p2:=caddnode.create(addn,p2,p1)
                 else
                  p2:=p1;
                 if token=_COMMA then
                  consume(_COMMA)
                 else
                  break;
               end;
              consume(_RKLAMMER);
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
              p1:=geninlinenode(l,false,paras);
              statement_syssym := p1;
            end;

          in_setlength_x:
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
              p1:=geninlinenode(l,false,paras);
              statement_syssym := p1;
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
              if token=_LKLAMMER then
               begin
                 consume(_LKLAMMER);
                 in_args:=true;
                 paras:=parse_paras(true,false);
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
              in_args:=true;
              paras:=parse_paras(true,false);
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
              if (token = _COMMA) then
                Begin
                  consume(_COMMA);
                  p2 := ccallparanode.create(comp_expr(true),p2)
                End;
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
              if token=_COMMA then
               begin
                 consume(_COMMA);
                 p2:=comp_expr(true);
               end
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


    { reads the parameter for a subroutine call }
    procedure do_proc_call(sym:tsym;st:tsymtable;getaddr:boolean;var again : boolean;var p1:tnode);
      var
         prevafterassn : boolean;
         hs,hs1 : tvarsym;
         para,p2 : tnode;
         hst : tsymtable;
         aprocdef : tprocdef;
      begin
         prevafterassn:=afterassignment;
         afterassignment:=false;
         { want we only determine the address of }
         { a subroutine ?                       }
         if not(getaddr) then
           begin
             para:=nil;
             if auto_inherited then
              begin
                hst:=symtablestack;
                while assigned(hst) and (hst.symtabletype<>parasymtable) do
                 hst:=hst.next;
                if assigned(hst) then
                 begin
                   hs:=tvarsym(hst.symindex.first);
                   while assigned(hs) do
                    begin
                      if hs.typ<>varsym then
                       internalerror(54382953);
                      { if there is a localcopy then use that }
                      if assigned(hs.localvarsym) then
                       hs1:=hs.localvarsym
                      else
                       hs1:=hs;
                      para:=ccallparanode.create(cloadnode.create(hs1,hs1.owner),para);
                      hs:=tvarsym(hs.indexnext);
                    end;
                 end
                else
                 internalerror(54382954);
              end
             else
              begin
                if token=_LKLAMMER then
                 begin
                   consume(_LKLAMMER);
                   para:=parse_paras(false,false);
                   consume(_RKLAMMER);
                 end;
              end;
             p1:=ccallnode.create(para,tprocsym(sym),st,p1);
           end
        else
           begin
              { address operator @: }
              if not assigned(p1) then
               begin
                 if (st.symtabletype=withsymtable) and
                    (st.defowner.deftype=objectdef) then
                   begin
                     p1:=tnode(twithsymtable(st).withrefnode).getcopy;
                   end
                 else
                   begin
                      { we must provide a method pointer, if it isn't given, }
                      { it is self                                           }
                      if (st.symtabletype=objectsymtable) then
                       p1:=cselfnode.create(tobjectdef(st.defowner));
                   end;
               end;

              { generate a methodcallnode or proccallnode }
              { we shouldn't convert things like @tcollection.load }
              if assigned(getprocvardef) then
               aprocdef:=get_proc_2_procvar_def(tprocsym(sym),getprocvardef)
              else
               aprocdef:=nil;
              p2:=cloadnode.create_procvar(sym,aprocdef,st);
              if assigned(p1) then
               tloadnode(p2).set_mp(p1);
              p1:=p2;

              { no postfix operators }
              again:=false;
           end;
         afterassignment:=prevafterassn;
      end;

    procedure handle_procvar(pv : tprocvardef;var p2 : tnode; getaddr: boolean);

        procedure doconv(procvar : tprocvardef;var t : tnode);
        var
          hp : tnode;
          currprocdef : tprocdef;
        begin
          hp:=nil;
          currprocdef:=get_proc_2_procvar_def(tcallnode(t).symtableprocentry,procvar);
          if assigned(currprocdef) then
           begin
             hp:=cloadnode.create_procvar(tprocsym(tcallnode(t).symtableprocentry),currprocdef,tcallnode(t).symtableproc);
             if (po_methodpointer in procvar.procoptions) then
               tloadnode(hp).set_mp(tnode(tcallnode(t).methodpointer).getcopy);
             t.destroy;
             t:=hp;
           end;
        end;

      begin
        if ((m_tp_procvar in aktmodeswitches) or
            not getaddr) then
          if (p2.nodetype=calln) then
           doconv(pv,p2)
          else
           if (p2.nodetype=typeconvn) and
              (ttypeconvnode(p2).left.nodetype=calln) then
            doconv(pv,ttypeconvnode(p2).left);
      end;


    { the following procedure handles the access to a property symbol }
    procedure handle_propertysym(sym : tsym;st : tsymtable;var p1 : tnode; getaddr: boolean);

        procedure symlist_to_node(var p1:tnode;pl:tsymlist);
        var
          plist : psymlistitem;
        begin
          plist:=pl.firstsym;
          while assigned(plist) do
           begin
             case plist^.sltype of
               sl_load :
                 begin
                   { p1 can already contain the loadnode of
                     the class variable. Then we need to use a
                     subscriptn. If no tree is found (with block), then
                     generate a loadn }
                   if assigned(p1) then
                    p1:=csubscriptnode.create(tvarsym(plist^.sym),p1)
                   else
                    p1:=cloadnode.create(tvarsym(plist^.sym),st);
                 end;
               sl_subscript :
                 p1:=csubscriptnode.create(tvarsym(plist^.sym),p1);
               sl_vec :
                 p1:=cvecnode.create(p1,cordconstnode.create(plist^.value,s32bittype));
               else
                 internalerror(200110205);
             end;
             plist:=plist^.next;
           end;
          include(p1.flags,nf_isproperty);
        end;

      var
         paras : tnode;
         p2    : tnode;
      begin
         paras:=nil;
         { property parameters? read them only if the property really }
         { has parameters                                             }
         if (ppo_hasparameters in tpropertysym(sym).propoptions) then
           begin
              if token=_LECKKLAMMER then
                begin
                   consume(_LECKKLAMMER);
                   paras:=parse_paras(false,true);
                   consume(_RECKKLAMMER);
                end;
           end;
         { indexed property }
         if (ppo_indexed in tpropertysym(sym).propoptions) then
           begin
              p2:=cordconstnode.create(tpropertysym(sym).index,tpropertysym(sym).indextype);
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
                         { generate the method call }
                         p1:=ccallnode.create(paras,
                                              tprocsym(tpropertysym(sym).writeaccess.firstsym^.sym),st,p1);
                         paras:=nil;
                         consume(_ASSIGNMENT);
                         { read the expression }
                         if tpropertysym(sym).proptype.def.deftype=procvardef then
                           getprocvardef:=tprocvardef(tpropertysym(sym).proptype.def);
                         p2:=comp_expr(true);
                         if assigned(getprocvardef) then
                           handle_procvar(getprocvardef,p2,getaddr);
                         tcallnode(p1).left:=ccallparanode.create(p2,tcallnode(p1).left);
                         include(tcallnode(p1).flags,nf_isproperty);
                         getprocvardef:=nil;
                       end;
                     varsym :
                       begin
                         { generate access code }
                         symlist_to_node(p1,tpropertysym(sym).writeaccess);
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
                     varsym :
                       begin
                          { generate access code }
                          symlist_to_node(p1,tpropertysym(sym).readaccess);
                       end;
                     procsym :
                       begin
                          { generate the method call }
                          p1:=ccallnode.create(paras,tprocsym(tpropertysym(sym).readaccess.firstsym^.sym),st,p1);
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
    procedure do_member_read(getaddr : boolean;sym : tsym;var p1 : tnode;var again : boolean);

      var
         static_name : string;
         isclassref : boolean;
         srsymtable : tsymtable;

      begin
         if sym=nil then
           begin
              { pattern is still valid unless
              there is another ID just after the ID of sym }
              Message1(sym_e_id_no_member,pattern);
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
                      do_proc_call(sym,sym.owner,
                                   getaddr or
                                   (assigned(getprocvardef) and
                                    ((block_type=bt_const) or
                                     ((m_tp_procvar in aktmodeswitches) and
                                      proc_to_procvar_equal(tprocsym(sym).defs^.def,getprocvardef,false)
                                     )
                                    )
                                   ),again,p1);
                      if (block_type=bt_const) and
                         assigned(getprocvardef) then
                        handle_procvar(getprocvardef,p1,getaddr);
                      { we need to know which procedure is called }
                      do_resulttypepass(p1);
                      { now we know the real method e.g. we can check for a class method }
                      if isclassref and
                         assigned(tcallnode(p1).procdefinition) and
                         not(po_classmethod in tcallnode(p1).procdefinition.procoptions) and
                         not(tcallnode(p1).procdefinition.proctypeoption=potype_constructor) then
                        Message(parser_e_only_class_methods_via_class_ref);
                   end;
                 varsym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      if (sp_static in sym.symoptions) then
                        begin
                           static_name:=lower(sym.owner.name^)+'_'+sym.name;
                           searchsym(static_name,sym,srsymtable);
                           p1.free;
                           p1:=cloadnode.create(tvarsym(sym),srsymtable);
                        end
                      else
                        p1:=csubscriptnode.create(tvarsym(sym),p1);
                   end;
                 propertysym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      handle_propertysym(sym,sym.owner,p1,getaddr);
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
                         Is_func_ret
         ---------------------------------------------}

        function is_func_ret(var p1:tnode;var sym : tsym;var srsymtable:tsymtable) : boolean;
        var
           p : pprocinfo;
           storesymtablestack : tsymtable;
        begin
          is_func_ret:=false;
          if not assigned(procinfo) or
             ((sym.typ<>funcretsym) and ((procinfo^.flags and pi_operator)=0)) then
            exit;
          p:=procinfo;
          while assigned(p) do
            begin
               { is this an access to a function result? Accessing _RESULT is
                 always allowed and funcretn is generated }
               if assigned(p^.procdef.funcretsym) and
                  ((tfuncretsym(sym)=p^.procdef.resultfuncretsym) or
                   ((tfuncretsym(sym)=p^.procdef.funcretsym) or
                    ((tvarsym(sym)=otsym) and ((p^.flags and pi_operator)<>0))) and
                   (not is_void(p^.procdef.rettype.def)) and
                   (token<>_LKLAMMER) and
                   (not ((m_tp in aktmodeswitches) and (afterassignment or in_args)))
                  ) then
                 begin
                    if ((tvarsym(sym)=otsym) and
                       ((p^.flags and pi_operator)<>0)) then
                       inc(otsym.refs);
                    p1:=cfuncretnode.create(p^.procdef.funcretsym);
                    is_func_ret:=true;
                    if tfuncretsym(p^.procdef.funcretsym).funcretstate=vs_declared then
                      begin
                        tfuncretsym(p^.procdef.funcretsym).funcretstate:=vs_declared_and_first_found;
                        include(p1.flags,nf_is_first_funcret);
                      end;
                    exit;
                 end;
               p:=p^.parent;
            end;
          { we must use the function call, update the
            sym to be the procsym }
          if (sym.typ=funcretsym) then
            begin
               storesymtablestack:=symtablestack;
               symtablestack:=sym.owner.next;
               searchsym(sym.name,sym,srsymtable);
               if not assigned(sym) then
                sym:=generrorsym;
               if (sym.typ<>procsym) then
                Message(cg_e_illegal_expression);
               symtablestack:=storesymtablestack;
            end;
        end;

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
           htype : ttype;
           static_name : string;
         begin
           { allow post fix operators }
           again:=true;
           consume_sym(srsym,srsymtable);
           if not is_func_ret(p1,srsym,srsymtable) then
            begin
              { check semantics of private }
              if (srsym.typ in [propertysym,procsym,varsym]) and
                 (srsym.owner.symtabletype=objectsymtable) then
               begin
                 if (sp_private in srsym.symoptions) and
                    (tobjectdef(srsym.owner.defowner).owner.symtabletype=globalsymtable) and
                    (tobjectdef(srsym.owner.defowner).owner.unitid<>0) then
                   Message(parser_e_cant_access_private_member);
               end;
              case srsym.typ of
                absolutesym :
                  begin
                    p1:=cloadnode.create(tvarsym(srsym),srsymtable);
                  end;

                varsym :
                  begin
                    { are we in a class method ? }
                    if (srsym.owner.symtabletype=objectsymtable) and
                       assigned(aktprocsym) and
                       (po_classmethod in aktprocdef.procoptions) then
                      Message(parser_e_only_class_methods);
                    if (sp_static in srsym.symoptions) then
                     begin
                       static_name:=lower(srsym.owner.name^)+'_'+srsym.name;
                       searchsym(static_name,srsym,srsymtable);
                     end;
                    p1:=cloadnode.create(tvarsym(srsym),srsymtable);
                    if tvarsym(srsym).varstate=vs_declared then
                     begin
                       include(p1.flags,nf_first);
                       { set special between first loaded until checked in resulttypepass }
                       tvarsym(srsym).varstate:=vs_declared_and_first_found;
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
                       if token=_LKLAMMER then
                        begin
                          consume(_LKLAMMER);
                          p1:=comp_expr(true);
                          consume(_RKLAMMER);
                          p1:=ctypeconvnode.create(p1,htype);
                          include(p1.flags,nf_explizit);
                        end
                       else { not LKLAMMER }
                        if (token=_POINT) and
                           is_object(htype.def) then
                         begin
                           consume(_POINT);
                           if assigned(procinfo) and
                              assigned(procinfo^._class) and
                              not(getaddr) then
                            begin
                              if procinfo^._class.is_related(tobjectdef(htype.def)) then
                               begin
                                 p1:=ctypenode.create(htype);
                                 { search also in inherited methods }
                                 repeat
                                   srsym:=tvarsym(tobjectdef(htype.def).symtable.search(pattern));
                                   if assigned(srsym) then
                                    break;
                                   htype.def:=tobjectdef(htype.def).childof;
                                 until not assigned(htype.def);
                                 consume(_ID);
                                 do_member_read(false,srsym,p1,again);
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
                              srsym:=tvarsym(search_class_member(tobjectdef(htype.def),pattern));
                              if not assigned(srsym) then
                               Message1(sym_e_id_no_member,pattern)
                              else if not(getaddr) and not(sp_static in srsym.symoptions) then
                               Message(sym_e_only_static_in_static)
                              else
                               begin
                                 consume(_ID);
                                 do_member_read(getaddr,srsym,p1,again);
                               end;
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
                                srsym:=tvarsym(search_class_member(tobjectdef(htype.def),pattern));
                                if not assigned(srsym) then
                                 Message1(sym_e_id_no_member,pattern)
                                else
                                 begin
                                   consume(_ID);
                                   do_member_read(getaddr,srsym,p1,again);
                                 end;
                              end
                             else
                              begin
                                p1:=ctypenode.create(htype);
                                { For a type block we simply return only
                                  the type. For all other blocks we return
                                  a loadvmt node }
                                if (block_type<>bt_type) then
                                 p1:=cloadvmtnode.create(p1);
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
                      constint :
                        begin
                          { do a very dirty trick to bootstrap this code }
                          if (tconstsym(srsym).valueord>=-(int64(2147483647)+int64(1))) and
                             (tconstsym(srsym).valueord<=2147483647) then
                           p1:=cordconstnode.create(tconstsym(srsym).valueord,s32bittype)
                          else if (tconstsym(srsym).valueord > maxlongint) and
                                  (tconstsym(srsym).valueord <= int64(maxlongint)+int64(maxlongint)+1) then
                           p1:=cordconstnode.create(tconstsym(srsym).valueord,u32bittype)
                          else
                           p1:=cordconstnode.create(tconstsym(srsym).valueord,cs64bittype);
                        end;
                      conststring :
                        begin
                          len:=tconstsym(srsym).len;
                          if not(cs_ansistrings in aktlocalswitches) and (len>255) then
                           len:=255;
                          getmem(pc,len+1);
                          move(pchar(tconstsym(srsym).valueptr)^,pc^,len);
                          pc[len]:=#0;
                          p1:=cstringconstnode.createpchar(pc,len);
                        end;
                      constchar :
                        p1:=cordconstnode.create(tconstsym(srsym).valueord,cchartype);
                      constreal :
                        p1:=crealconstnode.create(pbestreal(tconstsym(srsym).valueptr)^,pbestrealtype^);
                      constbool :
                        p1:=cordconstnode.create(tconstsym(srsym).valueord,booltype);
                      constset :
                        p1:=csetconstnode.create(pconstset(tconstsym(srsym).valueptr),tconstsym(srsym).consttype);
                      constord :
                        p1:=cordconstnode.create(tconstsym(srsym).valueord,tconstsym(srsym).consttype);
                      constpointer :
                        p1:=cpointerconstnode.create(tconstsym(srsym).valueordptr,tconstsym(srsym).consttype);
                      constnil :
                        p1:=cnilnode.create;
                      constresourcestring:
                        begin
                          p1:=cloadnode.create(tvarsym(srsym),srsymtable);
                          do_resulttypepass(p1);
                          p1.resulttype:=cansistringtype;
                        end;
                      constguid :
                        p1:=cguidconstnode.create(pguid(tconstsym(srsym).valueptr)^);
                    end;
                  end;

                procsym :
                  begin
                    { are we in a class method ? }
                    possible_error:=(srsym.owner.symtabletype=objectsymtable) and
                                    assigned(aktprocsym) and
                                    (po_classmethod in aktprocdef.procoptions);
                    do_proc_call(srsym,srsymtable,
                                 getaddr or
                                 (assigned(getprocvardef) and
                                  ((block_type=bt_const) or
                                   ((m_tp_procvar in aktmodeswitches) and
                                    proc_to_procvar_equal(tprocsym(srsym).defs^.def,getprocvardef,false)
                                   )
                                  )
                                 ),again,p1);
                    if (block_type=bt_const) and
                       assigned(getprocvardef) then
                     handle_procvar(getprocvardef,p1,getaddr);
                    { we need to know which procedure is called }
                    if possible_error then
                     begin
                       do_resulttypepass(p1);
                       if not(po_classmethod in tcallnode(p1).procdefinition.procoptions) then
                        Message(parser_e_only_class_methods);
                     end;
                  end;

                propertysym :
                  begin
                    { access to property in a method }
                    { are we in a class method ? }
                    if (srsym.owner.symtabletype=objectsymtable) and
                       assigned(aktprocsym) and
                       (po_classmethod in aktprocdef.procoptions) then
                     Message(parser_e_only_class_methods);
                    { no method pointer }
                    p1:=nil;
                    handle_propertysym(srsym,srsymtable,p1,getaddr);
                  end;

                labelsym :
                  begin
                    consume(_COLON);
                    if tlabelsym(srsym).defined then
                     Message(sym_e_label_already_defined);
                    tlabelsym(srsym).defined:=true;
                    p1:=clabelnode.create(tlabelsym(srsym),nil);
                  end;

                errorsym :
                  begin
                    p1:=cerrornode.create;
                    if token=_LKLAMMER then
                     begin
                       consume(_LKLAMMER);
                       parse_paras(false,false);
                       consume(_RKLAMMER);
                     end;
                  end;

                else
                  begin
                    p1:=cerrornode.create;
                    Message(cg_e_illegal_expression);
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
            begin
              while true do
               begin
                 p1:=comp_expr(true);
                 if token=_POINTPOINT then
                  begin
                    consume(_POINTPOINT);
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
                 if token=_COMMA then
                   consume(_COMMA)
                 else
                   break;
               end;
            end;
           factor_read_set:=buildp;
         end;


         {---------------------------------------------
                        PostFixOperators
         ---------------------------------------------}

      procedure postfixoperators(var p1:tnode;var again:boolean);

        { tries to avoid syntax errors after invalid qualifiers }
        procedure recoverconsume_postfixops;

          begin
             while true do
               begin
                  case token of
                     _CARET:
                        consume(_CARET);
                     _POINT:
                        begin
                           consume(_POINT);
                           if token=_ID then
                             consume(_ID);
                        end;
                     _LECKKLAMMER:
                        begin
                           consume(_LECKKLAMMER);
                           repeat
                             comp_expr(true);
                             if token=_COMMA then
                               consume(_COMMA)
                             else
                               break;
                           until false;
                           consume(_RECKKLAMMER);
                        end
                     else
                        break;
                  end;
               end;
          end;

        var
           store_static : boolean;
           protsym  : tpropertysym;
           p2,p3 : tnode;
           hsym  : tsym;
           classh : tobjectdef;
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
                    if (p1.resulttype.def.deftype<>pointerdef) then
                      begin
                         { ^ as binary operator is a problem!!!! (FK) }
                         again:=false;
                         Message(cg_e_invalid_qualifier);
                         recoverconsume_postfixops;
                         p1.destroy;
                         p1:=cerrornode.create;
                      end
                    else
                      begin
                         p1:=cderefnode.create(p1);
                      end;
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
                          handle_propertysym(protsym,protsym.owner,p1,getaddr);
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
                            stringdef :
                              begin
                                p2:=comp_expr(true);
                                p1:=cvecnode.create(p1,p2);
                              end;
                            arraydef :
                              begin
                                p2:=comp_expr(true);
                              { support SEG:OFS for go32v2 Mem[] }
                                if (target_info.target=target_i386_go32v2) and
                                   (p1.nodetype=loadn) and
                                   assigned(tloadnode(p1).symtableentry) and
                                   assigned(tloadnode(p1).symtableentry.owner.name) and
                                   (tloadnode(p1).symtableentry.owner.name^='SYSTEM') and
                                   ((tloadnode(p1).symtableentry.name='MEM') or
                                    (tloadnode(p1).symtableentry.name='MEMW') or
                                    (tloadnode(p1).symtableentry.name='MEML')) then
                                  begin
                                    if (token=_COLON) then
                                     begin
                                       consume(_COLON);
                                       p3:=caddnode.create(muln,cordconstnode.create($10,s32bittype),p2);
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
                                Message(cg_e_invalid_qualifier);
                                p1.destroy;
                                p1:=cerrornode.create;
                                comp_expr(true);
                                again:=false;
                              end;
                          end;
                          do_resulttypepass(p1);
                          if token=_COMMA then
                            consume(_COMMA)
                          else
                            break;
                        until false;
                        consume(_RECKKLAMMER);
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
                            hsym:=tsym(trecorddef(p1.resulttype.def).symtable.search(pattern));
                            if assigned(hsym) and
                               (hsym.typ=varsym) then
                              p1:=csubscriptnode.create(tvarsym(hsym),p1)
                            else
                              begin
                                Message1(sym_e_illegal_field,pattern);
                                p1.destroy;
                                p1:=cerrornode.create;
                              end;
                            consume(_ID);
                          end;
                        variantdef:
                          begin
                          end;
                        classrefdef:
                          begin
                             classh:=tobjectdef(tclassrefdef(p1.resulttype.def).pointertype.def);
                             hsym:=nil;
                             while assigned(classh) do
                              begin
                                hsym:=tsym(classh.symtable.search(pattern));
                                if assigned(hsym) then
                                 break;
                                classh:=classh.childof;
                              end;
                              if hsym=nil then
                                begin
                                   Message1(sym_e_id_no_member,pattern);
                                   p1.destroy;
                                   p1:=cerrornode.create;
                                   { try to clean up }
                                   consume(_ID);
                                end
                              else
                                begin
                                   consume(_ID);
                                   do_member_read(getaddr,hsym,p1,again);
                                end;
                           end;

                         objectdef:
                           begin
                              classh:=tobjectdef(p1.resulttype.def);
                              hsym:=nil;
                              store_static:=allow_only_static;
                              allow_only_static:=false;
                              while assigned(classh) do
                                begin
                                   hsym:=tsym(classh.symtable.search(pattern));
                                   if assigned(hsym) then
                                     break;
                                   classh:=classh.childof;
                                end;
                              allow_only_static:=store_static;
                              if hsym=nil then
                                begin
                                   Message1(sym_e_id_no_member,pattern);
                                   p1.destroy;
                                   p1:=cerrornode.create;
                                   { try to clean up }
                                   consume(_ID);
                                end
                              else
                                begin
                                   consume(_ID);
                                   do_member_read(getaddr,hsym,p1,again);
                                end;
                           end;

                         pointerdef:
                           begin
                             Message(cg_e_invalid_qualifier);
                             if tpointerdef(p1.resulttype.def).pointertype.def.deftype in [recorddef,objectdef,classrefdef] then
                              Message(parser_h_maybe_deref_caret_missing);
                           end;

                         else
                           begin
                             Message(cg_e_invalid_qualifier);
                             p1.destroy;
                             p1:=cerrornode.create;
                             consume(_ID);
                           end;
                    end;
                  end;

               else
                 begin
                 { is this a procedure variable ? }
                   if assigned(p1.resulttype.def) then
                    begin
                      if (p1.resulttype.def.deftype=procvardef) then
                       begin
                         if assigned(getprocvardef) and
                            is_equal(p1.resulttype.def,getprocvardef) then
                           again:=false
                         else
                           if (token=_LKLAMMER) or
                              ((tprocvardef(p1.resulttype.def).para.empty) and
                               (not((token in [_ASSIGNMENT,_UNEQUAL,_EQUAL]))) and
                               (not afterassignment) and
                               (not in_args)) then
                             begin
                                { do this in a strange way  }
                                { it's not a clean solution }
                                p2:=p1;
                                p1:=ccallnode.create(nil,nil,nil,nil);
                                tcallnode(p1).set_procvar(p2);
                                if token=_LKLAMMER then
                                  begin
                                     consume(_LKLAMMER);
                                     tcallnode(p1).left:=parse_paras(false,false);
                                     consume(_RKLAMMER);
                                  end;
                             { proc():= is never possible }
                                if token=_ASSIGNMENT then
                                 begin
                                   Message(cg_e_illegal_expression);
                                   p1:=cerrornode.create;
                                   again:=false;
                                 end;
                             end
                         else
                           again:=false;
                       end
                      else
                       again:=false;
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
         l      : longint;
         card   : cardinal;
         ic     : TConstExprInt;
         oldp1,
         p1     : tnode;
         code   : integer;
         again    : boolean;
         sym      : tsym;
         classh   : tobjectdef;
         d      : bestreal;
         hs : string;
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
              p1.set_tree_filepos(filepos);
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
           _SELF :
             begin
               again:=true;
               consume(_SELF);
               if not assigned(procinfo^._class) then
                begin
                  p1:=cerrornode.create;
                  again:=false;
                  Message(parser_e_self_not_in_method);
                end
               else
                begin
                  if (po_classmethod in aktprocdef.procoptions) then
                   begin
                     { self in class methods is a class reference type }
                     htype.setdef(procinfo^._class);
                     p1:=cselfnode.create(tobjectdef(tclassrefdef.create(htype)));
                   end
                  else
                   p1:=cselfnode.create(procinfo^._class);
                  postfixoperators(p1,again);
                end;
             end;

           _INHERITED :
             begin
               again:=true;
               consume(_INHERITED);
               if assigned(procinfo^._class) then
                begin
                  { if inherited; only then we need the method with
                    the same name }
                  if token=_SEMICOLON then
                   begin
                     hs:=aktprocsym.name;
                     auto_inherited:=true
                   end
                  else
                   begin
                     hs:=pattern;
                     consume(_ID);
                     auto_inherited:=false;
                   end;
                  classh:=procinfo^._class.childof;
                  while assigned(classh) do
                   begin
                     sym:=tsym(tobjectdef(classh).symtable.search(hs));
                     if assigned(sym) then
                      begin
                        if sym.typ=procsym then
                         begin
                           htype.setdef(classh);
                           p1:=ctypenode.create(htype);
                         end;
                        do_member_read(false,sym,p1,again);
                        break;
                      end;
                     classh:=classh.childof;
                   end;
                  if classh=nil then
                   begin
                     Message1(sym_e_id_no_member,hs);
                     again:=false;
                     p1:=cerrornode.create;
                   end;
                  { turn auto inheriting off }
                  auto_inherited:=false;
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
               { try cardinal first }
               val(pattern,card,code);
               if code<>0 then
                 begin
                   { then longint }
                   valint(pattern,l,code);
                   if code <> 0 then
                     begin
                       { then int64 }
                       val(pattern,ic,code);
                       if code<>0 then
                         begin
                            {finally float }
                            val(pattern,d,code);
                            if code<>0 then
                             begin
                                Message(cg_e_invalid_integer);
                                consume(_INTCONST);
                                l:=1;
                                p1:=cordconstnode.create(l,s32bittype);
                             end
                            else
                             begin
                                consume(_INTCONST);
                                p1:=crealconstnode.create(d,pbestrealtype^);
                             end;
                         end
                       else
                         begin
                            consume(_INTCONST);
                            p1:=cordconstnode.create(ic,cs64bittype);
                         end
                     end
                   else
                     begin
                       consume(_INTCONST);
                       p1:=cordconstnode.create(l,s32bittype)
                     end
                 end
               else
                begin
                   consume(_INTCONST);
                   { check whether the value isn't in the longint range as well }
                   { (longint is easier to perform calculations with) (JM)      }
                   if card <= $7fffffff then
                     { no sign extension necessary, so not longint typecast (JM) }
                     p1:=cordconstnode.create(card,s32bittype)
                   else
                     p1:=cordconstnode.create(card,u32bittype)
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
               if token=_LKLAMMER then
                begin
                  consume(_LKLAMMER);
                  p1:=comp_expr(true);
                  consume(_RKLAMMER);
                  p1:=ctypeconvnode.create(p1,htype);
                  include(p1.flags,nf_explizit);
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
               if token=_LKLAMMER then
                begin
                  consume(_LKLAMMER);
                  p1:=comp_expr(true);
                  consume(_RKLAMMER);
                  p1:=ctypeconvnode.create(p1,htype);
                  include(p1.flags,nf_explizit);
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
               p1:=cordconstnode.create(ord(pattern[1]),cchartype);
               consume(_CCHAR);
             end;

           _CWSTRING:
             begin
               p1:=cstringconstnode.createwstr(patternw);
               consume(_CWSTRING);
             end;

           _CWCHAR:
             begin
               p1:=cordconstnode.create(ord(getcharwidestring(patternw,0)),cwidechartype);
               consume(_CWCHAR);
             end;

           _KLAMMERAFFE :
             begin
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
                     again:=true;
                     postfixoperators(p1,again);
                   end;
                end
               else
                p1:=factor(true);
               got_addrn:=false;
               p1:=caddrnode.create(p1);
               if assigned(getprocvardef) then
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
             end;

           _MINUS :
             begin
               consume(_MINUS);
               p1:=sub_expr(oppower,false);
               p1:=cunaryminusnode.create(p1);
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
               p1:=cordconstnode.create(1,booltype);
             end;

           _FALSE :
             begin
               consume(_FALSE);
               p1:=cordconstnode.create(0,booltype);
             end;

           _NIL :
             begin
               consume(_NIL);
               p1:=cnilnode.create;
             end;

           else
             begin
               p1:=cerrornode.create;
               consume(token);
               Message(cg_e_illegal_expression);
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

        { tp7 procvar handling, but not if the next token
          will be a := }
        if (m_tp_procvar in aktmodeswitches) and
           (token<>_ASSIGNMENT) then
          check_tp_procvar(p1);

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
         ([_LT,_LTE,_GT,_GTE,_EQUAL,_UNEQUAL,_OP_IN,_OP_IS],
          [_PLUS,_MINUS,_OP_OR,_OP_XOR],
          [_CARET,_SYMDIF,_STARSTAR,_STAR,_SLASH,
           _OP_AS,_OP_AND,_OP_DIV,_OP_MOD,_OP_SHL,_OP_SHR],
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
               _OP_OR :
                 p1:=caddnode.create(orn,p1,p2);
               _OP_AND :
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
             p1.set_tree_filepos(filepos);
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
         if (m_tp_procvar in aktmodeswitches) and
            (token<>_ASSIGNMENT) then
           check_tp_procvar(p1);
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
                  handle_procvar(getprocvardef,p2,true);
                getprocvardef:=nil;
                p1:=cassignmentnode.create(p1,p2);
             end;
           _PLUSASN :
             begin
               consume(_PLUSASN);
               p2:=sub_expr(opcompare,true);
               p1:=cassignmentnode.create(p1,caddnode.create(addn,p1.getcopy,p2));
            end;
          _MINUSASN :
            begin
               consume(_MINUSASN);
               p2:=sub_expr(opcompare,true);
               p1:=cassignmentnode.create(p1,caddnode.create(subn,p1.getcopy,p2));
            end;
          _STARASN :
            begin
               consume(_STARASN  );
               p2:=sub_expr(opcompare,true);
               p1:=cassignmentnode.create(p1,caddnode.create(muln,p1.getcopy,p2));
            end;
          _SLASHASN :
            begin
               consume(_SLASHASN  );
               p2:=sub_expr(opcompare,true);
               p1:=cassignmentnode.create(p1,caddnode.create(slashn,p1.getcopy,p2));
            end;
         end;
         { get the resulttype for this expression }
         if not assigned(p1.resulttype.def) then
          do_resulttypepass(p1);
         afterassignment:=oldafterassignment;
         if p1<>oldp1 then
           p1.set_tree_filepos(filepos);
         expr:=p1;
      end;

{$ifdef int64funcresok}
    function get_intconst:TConstExprInt;
{$else int64funcresok}
    function get_intconst:longint;
{$endif int64funcresok}
    {Reads an expression, tries to evalute it and check if it is an integer
     constant. Then the constant is returned.}
    var
      p:tnode;
    begin
      p:=comp_expr(true);
      if not codegenerror then
       begin
         if (p.nodetype<>ordconstn) or
            not(is_integer(p.resulttype.def)) then
          Message(cg_e_illegal_expression)
         else
          get_intconst:=tordconstnode(p).value;
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
            Message(cg_e_illegal_expression);
        end
      else
        get_stringconst:=strpas(tstringconstnode(p).value_str);
      p.free;
    end;

end.
{
  $Log$
  Revision 1.54  2002-01-06 21:47:32  peter
    * removed getprocvar, use only getprocvardef

  Revision 1.53  2001/12/31 16:59:42  peter
    * protected/private symbols parsing fixed

  Revision 1.52  2001/12/06 17:57:36  florian
    + parasym to tparaitem added

  Revision 1.51  2001/11/14 01:12:44  florian
    * variant paramter passing and functions results fixed

  Revision 1.50  2001/11/02 23:16:51  peter
    * removed obsolete chainprocsym and test_procsym code

  Revision 1.49  2001/11/02 22:58:05  peter
    * procsym definition rewrite

  Revision 1.48  2001/10/28 17:22:25  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.47  2001/10/24 11:51:39  marco
   * Make new/dispose system functions instead of keywords

  Revision 1.46  2001/10/21 13:10:51  peter
    * better support for indexed properties

  Revision 1.45  2001/10/21 12:33:07  peter
    * array access for properties added

  Revision 1.44  2001/10/20 19:28:39  peter
    * interface 2 guid support
    * guid constants support

  Revision 1.43  2001/10/18 16:30:38  jonas
    * property parameters are now fully parsed by the firstcall code to
      check for the correct amount and types (merged)

  Revision 1.42  2001/09/02 21:18:28  peter
    * split constsym.value in valueord,valueordptr,valueptr. The valueordptr
      is used for holding target platform pointer values. As those can be
      bigger than the source platform.

  Revision 1.41  2001/08/26 13:36:45  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.40  2001/08/22 21:16:21  florian
    * some interfaces related problems regarding
      mapping of interface implementions fixed

  Revision 1.39  2001/08/06 21:40:47  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.38  2001/07/09 21:15:41  peter
    * Length made internal
    * Add array support for Length

  Revision 1.37  2001/06/29 14:16:57  jonas
    * fixed inconsistent handling of procvars in FPC mode (sometimes @ was
      required to assign the address of a procedure to a procvar, sometimes
      not. Now it is always required) (merged)

  Revision 1.36  2001/06/04 18:16:42  peter
    * fixed tp procvar support in parameters of a called procvar
    * typenode cleanup, no special handling needed anymore for bt_type

  Revision 1.35  2001/06/04 11:45:35  peter
    * parse const after .. using bt_const block to allow expressions, this
      is Delphi compatible

  Revision 1.34  2001/05/19 21:15:53  peter
    * allow typenodes for typeinfo and typeof
    * tp procvar fixes for properties

  Revision 1.33  2001/05/19 12:23:59  peter
    * fixed crash with auto dereferencing

  Revision 1.32  2001/05/09 19:52:51  peter
    * removed unused allow_type

  Revision 1.31  2001/05/04 15:52:03  florian
    * some Delphi incompatibilities fixed:
       - out, dispose and new can be used as idenfiers now
       - const p = apointerype(nil); is supported now
    + support for const p = apointertype(pointer(1234)); added

  Revision 1.30  2001/04/14 14:07:10  peter
    * moved more code from pass_1 to det_resulttype

  Revision 1.29  2001/04/13 23:50:24  peter
    * fpc mode now requires @ also when left of assignment is an procvardef

  Revision 1.28  2001/04/13 01:22:12  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.27  2001/04/04 22:43:52  peter
    * remove unnecessary calls to firstpass

  Revision 1.26  2001/04/02 21:20:33  peter
    * resulttype rewrite

  Revision 1.25  2001/03/11 22:58:50  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.24  2000/12/25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.23  2000/12/19 20:36:03  peter
    * cardinal const expr fix from jonas

  Revision 1.22  2000/12/17 14:00:18  peter
    * fixed static variables

  Revision 1.21  2000/12/15 13:26:01  jonas
    * only return int64's from functions if it int64funcresok is defined
    + added int64funcresok define to options.pas

  Revision 1.20  2000/12/15 12:13:52  michael
  + Fix from Peter

  Revision 1.19  2000/12/07 17:19:42  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.18  2000/11/29 00:30:36  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.17  2000/11/09 17:46:55  florian
    * System.TypeInfo fixed
    + System.Finalize implemented
    + some new keywords for interface support added

  Revision 1.16  2000/11/06 20:30:55  peter
    * more fixes to get make cycle working

  Revision 1.15  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.14  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.13  2000/10/26 23:40:54  peter
    * fixed crash with call from type decl which is not allowed (merged)

  Revision 1.12  2000/10/21 18:16:12  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.11  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.10  2000/10/01 19:48:25  peter
    * lot of compile updates for cg11

  Revision 1.9  2000/09/24 21:19:50  peter
    * delphi compile fixes

  Revision 1.8  2000/09/24 15:06:22  peter
    * use defines.inc

  Revision 1.7  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.6  2000/08/20 15:12:49  peter
    * auto derefence mode for array pointer (merged)

  Revision 1.5  2000/08/16 18:33:53  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.4  2000/08/16 13:06:06  florian
    + support of 64 bit integer constants

  Revision 1.3  2000/08/04 22:00:52  peter
    * merges from fixes

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs
}

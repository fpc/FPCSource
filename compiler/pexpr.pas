{
    $Id$
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
      symtype,
      node,
      globals,
      cpuinfo;

    { reads a whole expression }
    function expr : tnode;

    { reads an expression without assignements and .. }
    function comp_expr(accept_equal : boolean):tnode;

    { reads a single factor }
    function factor(getaddr : boolean) : tnode;

    procedure string_dec(var t: ttype);

    function parse_paras(__colon,in_prop_paras : boolean) : tnode;

    { the ID token has to be consumed before calling this function }
    procedure do_member_read(getaddr : boolean;sym : tsym;var p1 : tnode;var again : boolean;callnflags:tnodeflags);

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
       globtype,tokens,verbose,
       systems,widestr,
       { symtable }
       symconst,symbase,symdef,symsym,symtable,defutil,defcmp,
       { pass 1 }
       pass_1,htypechk,
       nutils,nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,nbas,
       { parser }
       scanner,
       pbase,pinline,
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
         hp,
         p1 : tnode;
      begin
         if (m_tp_procvar in aktmodeswitches) and
            (token<>_ASSIGNMENT) and
            (not got_addrn) and
            (block_type=bt_general) then
          begin
            { ignore vecn,subscriptn }
            hp:=p;
            repeat
              case hp.nodetype of
                vecn :
                  hp:=tvecnode(hp).left;
                subscriptn :
                  hp:=tsubscriptnode(hp).left;
                else
                  break;
              end;
            until false;
            if (hp.nodetype=loadn) then
               begin
                  { get the resulttype of p }
                  do_resulttypepass(p);
                  { convert the procvar load to a call:
                     - not expecting a procvar
                     - the procvar does not get arguments, when it
                       requires arguments the callnode will fail
                       Note: When arguments were passed there was no loadn }
                  if (getprocvardef=nil) and
                     (p.resulttype.def.deftype=procvardef) and
                     (tprocvardef(p.resulttype.def).minparacount=0) then
                    begin
                       p1:=ccallnode.create_procvar(nil,p);
                       resulttypepass(p1);
                       p:=p1;
                    end;
               end;
          end;
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
                    p1:=comp_expr(true);
                    consume(_RKLAMMER);
                    if (block_type=bt_except) then
                      Message(parser_e_exit_with_argument_not__possible);
                    if (not assigned(current_procinfo) or
                        is_void(current_procinfo.procdef.rettype.def)) then
                      Message(parser_e_void_function);
                 end
               else
                 p1:=nil;
               statement_syssym:=cexitnode.create(p1);
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
                  is_open_string(p1.resulttype.def)
                 ) then
               statement_syssym:=geninlinenode(in_sizeof_x,false,p1)
              else
               begin
                 statement_syssym:=cordconstnode.create(p1.resulttype.def.size,s32bittype,true);
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
              err:=false;
              consume(_LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if not codegenerror then
               begin
                 { With tp procvars we allways need to load a
                   procvar when it is passed }
                 if (m_tp_procvar in aktmodeswitches) and
                    (p1.nodetype=calln) then
                   load_procvar_from_calln(p1);

                 case p1.resulttype.def.deftype of
                   pointerdef,
                   procvardef,
                   classrefdef : ;
                   objectdef :
                     if not is_class_or_interface(p1.resulttype.def) then
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
              if token=_LKLAMMER then
               begin
                 consume(_LKLAMMER);
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
    procedure do_proc_call(sym:tsym;st:tsymtable;getaddr:boolean;var again : boolean;var p1:tnode);
      var
         membercall,
         prevafterassn : boolean;
         vs : tvarsym;
         para,p2 : tnode;
         currpara : tparaitem;
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
             if (m_tp_procvar in aktmodeswitches) then
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
                currpara:=tparaitem(current_procinfo.procdef.para.first);
                while assigned(currpara) do
                 begin
                   if not currpara.is_hidden then
                    begin
                      vs:=tvarsym(currpara.parasym);
                      { if there is a localcopy then use that }
                      if assigned(vs.localvarsym) then
                        vs:=vs.localvarsym;
                      para:=ccallparanode.create(cloadnode.create(vs,vs.owner),para);
                    end;
                   currpara:=tparaitem(currpara.next);
                 end;
              end
             else
              begin
                if try_to_consume(_LKLAMMER) then
                 begin
                   para:=parse_paras(false,false);
                   consume(_RKLAMMER);
                 end;
              end;
             p1:=ccallnode.create(para,tprocsym(sym),st,p1);
             { indicate if this call was generated by a member and
               no explicit self is used, this is needed to determine
               how to handle a destructor call (PFV) }
             if membercall then
               include(p1.flags,nf_member_call);
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
        if (m_tp_procvar in aktmodeswitches) then
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
                   tloadnode(hp2).set_mp(tnode(tcallnode(hp).methodpointer).getcopy);
                 hp.destroy;
                 { replace the old callnode with the new loadnode }
                 hpp^:=hp2;
               end;
            end;
         end;
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
               sl_vec :
                 p1:=cvecnode.create(p1,cordconstnode.create(plist^.value,s32bittype,true));
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
         membercall : boolean;
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
                         { generate the method call }
                         membercall:=maybe_load_methodpointer(st,p1);
                         p1:=ccallnode.create(paras,
                                              tprocsym(tpropertysym(sym).writeaccess.firstsym^.sym),st,p1);
                         if membercall then
                           include(tcallnode(p1).flags,nf_member_call);
                         paras:=nil;
                         consume(_ASSIGNMENT);
                         { read the expression }
                         if tpropertysym(sym).proptype.def.deftype=procvardef then
                           getprocvardef:=tprocvardef(tpropertysym(sym).proptype.def);
                         p2:=comp_expr(true);
                         if assigned(getprocvardef) then
                           handle_procvar(getprocvardef,p2);
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
                          membercall:=maybe_load_methodpointer(st,p1);
                          p1:=ccallnode.create(paras,tprocsym(tpropertysym(sym).readaccess.firstsym^.sym),st,p1);
                          if membercall then
                            include(tcallnode(p1).flags,nf_member_call);
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
    procedure do_member_read(getaddr : boolean;sym : tsym;var p1 : tnode;var again : boolean;callnflags:tnodeflags);

      var
         static_name : string;
         isclassref : boolean;
         srsymtable : tsymtable;
{$ifdef CHECKINHERITEDRESULT}
         newstatement : tstatementnode;
         newblock     : tblocknode;
{$endif CHECKINHERITEDRESULT}
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
                                   (getaddr and not(token in [_CARET,_POINT])),
                                   again,p1);
                      { add provided flags }
                      if (p1.nodetype=calln) then
                        p1.flags:=p1.flags+callnflags;
                      { we need to know which procedure is called }
                      do_resulttypepass(p1);
                      { now we know the method that is called }
                      if (p1.nodetype=calln) and
                         assigned(tcallnode(p1).procdefinition) then
                        begin
                          { calling using classref? }
                          if isclassref and
                             not(po_classmethod in tcallnode(p1).procdefinition.procoptions) and
                             not(tcallnode(p1).procdefinition.proctypeoption=potype_constructor) then
                            Message(parser_e_only_class_methods_via_class_ref);
{$ifdef CHECKINHERITEDRESULT}
                           { when calling inherited constructor we need to check the return value }
                           if (nf_inherited in callnflags) and
                              (tcallnode(p1).procdefinition.proctypeoption=potype_constructor) then
                             begin
                               {
                                 For Classes:

                                 self:=inherited constructor
                                 if self=nil then
                                   exit

                                 For objects:
                                 if inherited constructor=false then
                                   begin
                                     self:=nil;
                                     exit;
                                   end;
                               }
                               if is_class(tprocdef(tcallnode(p1).procdefinition)._class) then
                                 begin
                                   newblock:=internalstatements(newstatement,true);
                                   addstatement(newstatement,cassignmentnode.create(
                                       ctypeconvnode.create(
                                           load_self_pointer_node,
                                           voidpointertype),
                                       ctypeconvnode.create(
                                           p1,
                                           voidpointertype)));
                                   addstatement(newstatement,cifnode.create(
                                       caddnode.create(equaln,
                                           load_self_pointer_node,
                                           cnilnode.create),
                                       cexitnode.create(nil),
                                       nil));
                                   p1:=newblock;
                                 end
                               else
                                 if is_object(tprocdef(tcallnode(p1).procdefinition)._class) then
                                   begin
                                     newblock:=internalstatements(newstatement,true);
                                     addstatement(newstatement,call_fail_node);
                                     addstatement(newstatement,cexitnode.create(nil));
                                     p1:=cifnode.create(
                                         caddnode.create(equaln,
                                             cordconstnode.create(0,booltype,false),
                                             p1),
                                         newblock,
                                         nil);
                                   end
                                 else
                                   internalerror(200305133);
                             end;
{$endif CHECKINHERITEDRESULT}
                           do_resulttypepass(p1);
                        end;
                   end;
                 varsym:
                   begin
                      if (sp_static in sym.symoptions) then
                        begin
                           static_name:=lower(sym.owner.name^)+'_'+sym.name;
                           searchsym(static_name,sym,srsymtable);
                           check_hints(sym);
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
           if (srsym.typ in [absolutesym,varsym]) and
              (vo_is_funcret in tvarsym(srsym).varoptions) and
              (
               (token=_LKLAMMER) or
               (not(m_fpc in aktmodeswitches) and
                (afterassignment or in_args) and
                not(vo_is_result in tvarsym(srsym).varoptions))
              ) then
            begin
              storesymtablestack:=symtablestack;
              symtablestack:=srsym.owner.next;
              searchsym(srsym.name,srsym,srsymtable);
              if not assigned(srsym) then
               srsym:=generrorsym;
              if (srsym.typ<>procsym) then
               Message(cg_e_illegal_expression);
              symtablestack:=storesymtablestack;
            end;

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
                    p1:=cloadnode.create(srsym,srsymtable);
                  end;

                varsym :
                  begin
                    if (sp_static in srsym.symoptions) then
                     begin
                       static_name:=lower(srsym.owner.name^)+'_'+srsym.name;
                       searchsym(static_name,srsym,srsymtable);
                       check_hints(srsym);
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
                        p1:=csubscriptnode.create(srsym,load_self_node);
                      withsymtable :
                        p1:=csubscriptnode.create(srsym,tnode(twithsymtable(srsymtable).withrefnode).getcopy);
                      else
                        p1:=cloadnode.create(srsym,srsymtable);
                    end;

                    if tvarsym(srsym).varstate=vs_declared then
                     begin
                       include(p1.flags,nf_first_use);
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
                                 check_hints(srsym);
                                 consume(_ID);
                                 do_member_read(false,srsym,p1,again,[]);
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
                              check_hints(srsym);
                              if not assigned(srsym) then
                               Message1(sym_e_id_no_member,pattern)
                              else if not(getaddr) and not(sp_static in srsym.symoptions) then
                               Message(sym_e_only_static_in_static)
                              else
                               begin
                                 consume(_ID);
                                 do_member_read(getaddr,srsym,p1,again,[]);
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
                                srsym:=search_class_member(tobjectdef(htype.def),pattern);
                                check_hints(srsym);
                                if not assigned(srsym) then
                                 Message1(sym_e_id_no_member,pattern)
                                else
                                 begin
                                   consume(_ID);
                                   do_member_read(getaddr,srsym,p1,again,[]);
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
                      constint :
                        begin
                          { do a very dirty trick to bootstrap this code }
                          if (tconstsym(srsym).value.valueord>=-(int64(2147483647)+int64(1))) and
                             (tconstsym(srsym).value.valueord<=2147483647) then
                           p1:=cordconstnode.create(tconstsym(srsym).value.valueord,s32bittype,true)
                          else if (tconstsym(srsym).value.valueord > maxlongint) and
                                  (tconstsym(srsym).value.valueord <= int64(maxlongint)+int64(maxlongint)+1) then
                           p1:=cordconstnode.create(tconstsym(srsym).value.valueord,u32bittype,true)
                          else
                           p1:=cordconstnode.create(tconstsym(srsym).value.valueord,cs64bittype,true);
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
                      constchar :
                        p1:=cordconstnode.create(tconstsym(srsym).value.valueord,cchartype,true);
                      constreal :
                        p1:=crealconstnode.create(pbestreal(tconstsym(srsym).value.valueptr)^,pbestrealtype^);
                      constbool :
                        p1:=cordconstnode.create(tconstsym(srsym).value.valueord,booltype,true);
                      constset :
                        p1:=csetconstnode.create(pconstset(tconstsym(srsym).value.valueptr),tconstsym(srsym).consttype);
                      constord :
                        p1:=cordconstnode.create(tconstsym(srsym).value.valueord,tconstsym(srsym).consttype,true);
                      constpointer :
                        p1:=cpointerconstnode.create(tconstsym(srsym).value.valueordptr,tconstsym(srsym).consttype);
                      constnil :
                        p1:=cnilnode.create;
                      constresourcestring:
                        begin
                          p1:=cloadnode.create(srsym,srsymtable);
                          do_resulttypepass(p1);
                          p1.resulttype:=cansistringtype;
                        end;
                      constguid :
                        p1:=cguidconstnode.create(pguid(tconstsym(srsym).value.valueptr)^);
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
                    do_proc_call(srsym,srsymtable,
                                 (getaddr and not(token in [_CARET,_POINT])),
                                 again,p1);
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
                       assigned(current_procinfo) and
                       (po_classmethod in current_procinfo.procdef.procoptions) then
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
                          begin
                            { The property symbol is referenced indirect }
                            inc(protsym.refs);
                            handle_propertysym(protsym,protsym.owner,p1,getaddr);
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
                            stringdef :
                              begin
                                p2:=comp_expr(true);
                                p1:=cvecnode.create(p1,p2);
                              end;
                            arraydef :
                              begin
                                p2:=comp_expr(true);
                              { support SEG:OFS for go32v2 Mem[] }
                                if (target_info.system=system_i386_go32v2) and
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
                                       p3:=caddnode.create(muln,cordconstnode.create($10,s32bittype,false),p2);
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
                            check_hints(hsym);
                            if assigned(hsym) and
                               (hsym.typ=varsym) then
                              p1:=csubscriptnode.create(hsym,p1)
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
                             hsym:=searchsym_in_class(classh,pattern);
                             check_hints(hsym);
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
                                do_member_read(getaddr,hsym,p1,again,[]);
                              end;
                           end;

                         objectdef:
                           begin
                              store_static:=allow_only_static;
                              allow_only_static:=false;
                              classh:=tobjectdef(p1.resulttype.def);
                              hsym:=searchsym_in_class(classh,pattern);
                              check_hints(hsym);
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
                                   do_member_read(getaddr,hsym,p1,again,[]);
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
                            equal_defs(p1.resulttype.def,getprocvardef) then
                           again:=false
                         else
                           if (token=_LKLAMMER) or
                              ((tprocvardef(p1.resulttype.def).maxparacount=0) and
                               (not((token in [_ASSIGNMENT,_UNEQUAL,_EQUAL]))) and
                               (not afterassignment) and
                               (not in_args)) then
                             begin
                                if try_to_consume(_LKLAMMER) then
                                  begin
                                     p2:=parse_paras(false,false);
                                     consume(_RKLAMMER);
                                  end
                                else
                                  p2:=nil;
                                p1:=ccallnode.create_procvar(p2,p1);
                                { proc():= is never possible }
                                if token=_ASSIGNMENT then
                                 begin
                                   Message(cg_e_illegal_expression);
                                   p1.free;
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
         l        : longint;
         card     : cardinal;
         ic       : TConstExprInt;
         oldp1,
         p1       : tnode;
         code     : integer;
         again    : boolean;
         sym      : tsym;
         pd       : tprocdef;
         classh   : tobjectdef;
         d        : bestreal;
         hs       : string;
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
               if not(assigned(current_procinfo) and
                      assigned(current_procinfo.procdef._class)) then
                begin
                  p1:=cerrornode.create;
                  again:=false;
                  Message(parser_e_self_not_in_method);
                end
               else
                begin
                  p1:=load_self_node;
                  postfixoperators(p1,again);
                end;
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
                     consume(_ID);
                     anon_inherited:=false;
                     sym:=searchsym_in_class(classh,hs);
                   end;
                  if assigned(sym) then
                   begin
                     check_hints(sym);
                     { load the procdef from the inherited class and
                       not from self }
                     if sym.typ=procsym then
                      begin
                        htype.setdef(classh);
                        p1:=ctypenode.create(htype);
                      end;
                     do_member_read(false,sym,p1,again,[nf_inherited,nf_anon_inherited]);
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
                            do_proc_call(sym,sym.owner,false,again,p1);
                          end
                        else
                          begin
                            { we need to ignore the inherited; }
                            p1:=cnothingnode.create;
                          end;
                      end
                     else
                      begin
                        Message1(sym_e_id_no_member,hs);
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
                                p1:=cordconstnode.create(l,s32bittype,true);
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
                            p1:=cordconstnode.create(ic,cs64bittype,true);
                         end
                     end
                   else
                     begin
                       consume(_INTCONST);
                       p1:=cordconstnode.create(l,defaultordconsttype,true)
                     end
                 end
               else
                begin
                   consume(_INTCONST);
                   { check whether the value isn't in the longint range as well }
                   { (longint is easier to perform calculations with) (JM)      }
                   if card <= $7fffffff then
                     { no sign extension necessary, so not longint typecast (JM) }
                     p1:=cordconstnode.create(card,s32bittype,true)
                   else
                     p1:=cordconstnode.create(card,u32bittype,true)
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
               if token=_LKLAMMER then
                begin
                  consume(_LKLAMMER);
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
                  handle_procvar(getprocvardef,p2);
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
  Revision 1.125  2003-08-23 18:41:52  peter
    * allow typeof(self) in class methods

  Revision 1.124  2003/08/10 17:25:23  peter
    * fixed some reported bugs

  Revision 1.123  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.122  2003/06/03 21:02:57  peter
    * don't set nf_member when loaded from with symtable
    * allow static variables in class methods

  Revision 1.121  2003/05/22 17:43:21  peter
    * search defaulthandler only for message methods

  Revision 1.120  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.119  2003/05/13 20:54:39  peter
    * ifdef'd code that checked for failed inherited constructors

  Revision 1.118  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.117  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.116  2003/05/11 14:45:12  peter
    * tloadnode does not support objectsymtable,withsymtable anymore
    * withnode cleanup
    * direct with rewritten to use temprefnode

  Revision 1.115  2003/05/09 17:47:03  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.114  2003/05/01 07:59:42  florian
    * introduced defaultordconsttype to decribe the default size of ordinal constants
      on 64 bit CPUs it's equal to cs64bitdef while on 32 bit CPUs it's equal to s32bitdef
    + added defines CPU32 and CPU64 for 32 bit and 64 bit CPUs
    * int64s/qwords are allowed as for loop counter on 64 bit CPUs

  Revision 1.113  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.112  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.111  2003/04/26 00:33:07  peter
    * vo_is_result flag added for the special RESULT symbol

  Revision 1.110  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.109  2003/04/23 10:13:55  peter
    * firstaddr will check procvardef

  Revision 1.108  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.107  2003/04/11 15:49:01  peter
    * default property also increased the reference count for the
      property symbol

  Revision 1.106  2003/04/11 14:50:08  peter
    * fix tw2454

  Revision 1.105  2003/03/27 17:44:13  peter
    * fixed small mem leaks

  Revision 1.104  2003/03/17 18:55:30  peter
    * allow more tokens instead of only semicolon after inherited

  Revision 1.103  2003/03/17 16:54:41  peter
    * support DefaultHandler and anonymous inheritance fixed
      for message methods

  Revision 1.102  2003/01/30 21:46:57  peter
    * self fixes for static methods (merged)

  Revision 1.101  2003/01/16 22:12:22  peter
    * Find the correct procvar to load when using @ in fpc mode

  Revision 1.100  2003/01/15 01:44:32  peter
    * merged methodpointer fixes from 1.0.x

  Revision 1.98  2003/01/12 17:51:42  peter
    * tp procvar handling fix for tb0448

  Revision 1.97  2003/01/05 22:44:14  peter
    * remove a lot of code to support typen in loadn-procsym

  Revision 1.96  2002/12/11 22:40:36  peter
    * assigned(procvar) fix for delphi mode, fixes tb0430

  Revision 1.95  2002/11/30 11:12:48  carl
    + checking for symbols used with hint directives is done mostly in pexpr
      only now

  Revision 1.94  2002/11/27 15:33:47  peter
    * the never ending story of tp procvar hacks

  Revision 1.93  2002/11/26 22:58:24  peter
    * fix for tw2178. When a ^ or . follows a procsym then the procsym
      needs to be called

  Revision 1.92  2002/11/25 17:43:22  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.91  2002/11/22 22:48:10  carl
   * memory optimization with tconstsym (1.5%)

  Revision 1.90  2002/11/20 22:49:55  pierre
   * commented check code tht was invalid in 1.1

  Revision 1.89  2002/11/18 18:34:41  peter
    * fix crash with EXTDEBUG code

  Revision 1.88  2002/11/18 17:48:21  peter
    * fix tw2209 (merged)

  Revision 1.87  2002/11/18 17:31:58  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.86  2002/10/05 00:48:57  peter
    * support inherited; support for overload as it is handled by
      delphi. This is only for delphi mode as it is working is
      undocumented and hard to predict what is done

  Revision 1.85  2002/10/04 21:13:59  peter
    * ignore vecn,subscriptn when checking for a procvar loadn

  Revision 1.84  2002/10/02 20:51:22  peter
    * don't check interfaces for class methods

  Revision 1.83  2002/10/02 18:20:52  peter
    * Copy() is now internal syssym that calls compilerprocs

  Revision 1.82  2002/09/30 07:00:48  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.81  2002/09/16 19:06:14  peter
    * allow ^ after nil

  Revision 1.80  2002/09/07 15:25:07  peter
    * old logs removed and tabs fixed

  Revision 1.79  2002/09/07 12:16:03  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.78  2002/09/03 16:26:27  daniel
    * Make Tprocdef.defs protected

  Revision 1.77  2002/08/18 20:06:24  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.76  2002/08/17 09:23:39  florian
    * first part of procinfo rewrite

  Revision 1.75  2002/08/01 16:37:47  jonas
    - removed some superfluous "in_paras := true" statements

  Revision 1.74  2002/07/26 21:15:41  florian
    * rewrote the system handling

  Revision 1.73  2002/07/23 09:51:23  daniel
  * Tried to make Tprocsym.defs protected. I didn't succeed but the cleanups
    are worth comitting.

  Revision 1.72  2002/07/20 11:57:55  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.71  2002/07/16 15:34:20  florian
    * exit is now a syssym instead of a keyword

  Revision 1.70  2002/07/06 20:18:02  carl
  * longstring declaration now gives parser error since its not supported!

  Revision 1.69  2002/06/12 15:46:14  jonas
    * fixed web bug 1995

  Revision 1.68  2002/05/18 13:34:12  peter
    * readded missing revisions

  Revision 1.67  2002/05/16 19:46:43  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.65  2002/05/12 16:53:09  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.64  2002/04/23 19:16:34  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.63  2002/04/21 19:02:05  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.62  2002/04/16 16:11:17  peter
    * using inherited; without a parent having the same function
      will do nothing like delphi

  Revision 1.61  2002/04/07 13:31:36  carl
  + change unit use

  Revision 1.60  2002/04/01 20:57:13  jonas
    * fixed web bug 1907
    * fixed some other procvar related bugs (all related to accepting procvar
        constructs with either too many or too little parameters)
    (both merged, includes second typo fix of pexpr.pas)

  Revision 1.59  2002/03/31 20:26:35  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}

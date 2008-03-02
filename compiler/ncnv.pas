{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Type checking and register allocation for type converting nodes

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
unit ncnv;

{$i fpcdefs.inc}

interface

    uses
       node,
       symtype,
       defutil,defcmp,
       nld
       ;

    type
       ttypeconvnode = class(tunarynode)
          totypedef   : tdef;
          totypedefderef : tderef;
          convtype : tconverttype;
          warn_pointer_to_signed: boolean;
          constructor create(node : tnode;def:tdef);virtual;
          constructor create_explicit(node : tnode;def:tdef);
          constructor create_internal(node : tnode;def:tdef);
          constructor create_proc_to_procvar(node : tnode);
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function dogetcopy : tnode;override;
          function actualtargetnode: tnode;override;
          procedure printnodeinfo(var t : text);override;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function simplify:tnode; override;
          procedure mark_write;override;
          function docompare(p: tnode) : boolean; override;
          function assign_allowed:boolean;
          procedure second_call_helper(c : tconverttype);
       private
          function typecheck_int_to_int : tnode;
          function typecheck_cord_to_pointer : tnode;
          function typecheck_chararray_to_string : tnode;
          function typecheck_string_to_chararray : tnode;
          function typecheck_string_to_string : tnode;
          function typecheck_char_to_string : tnode;
          function typecheck_char_to_chararray : tnode;
          function typecheck_int_to_real : tnode;
          function typecheck_real_to_real : tnode;
          function typecheck_real_to_currency : tnode;
          function typecheck_cchar_to_pchar : tnode;
          function typecheck_cstring_to_pchar : tnode;
          function typecheck_cstring_to_int : tnode;
          function typecheck_char_to_char : tnode;
          function typecheck_arrayconstructor_to_set : tnode;
          function typecheck_set_to_set : tnode;
          function typecheck_pchar_to_string : tnode;
          function typecheck_interface_to_guid : tnode;
          function typecheck_dynarray_to_openarray : tnode;
          function typecheck_pwchar_to_string : tnode;
          function typecheck_variant_to_dynarray : tnode;
          function typecheck_dynarray_to_variant : tnode;
          function typecheck_call_helper(c : tconverttype) : tnode;
          function typecheck_variant_to_enum : tnode;
          function typecheck_enum_to_variant : tnode;
          function typecheck_proc_to_procvar : tnode;
          function typecheck_variant_to_interface : tnode;
          function typecheck_interface_to_variant : tnode;
          function typecheck_array_2_dynarray : tnode;
       protected
          function first_int_to_int : tnode;virtual;
          function first_cstring_to_pchar : tnode;virtual;
          function first_cstring_to_int : tnode;virtual;
          function first_string_to_chararray : tnode;virtual;
          function first_char_to_string : tnode;virtual;
          function first_nothing : tnode;virtual;
          function first_array_to_pointer : tnode;virtual;
          function first_int_to_real : tnode;virtual;
          function first_real_to_real : tnode;virtual;
          function first_pointer_to_array : tnode;virtual;
          function first_cchar_to_pchar : tnode;virtual;
          function first_bool_to_int : tnode;virtual;
          function first_int_to_bool : tnode;virtual;
          function first_bool_to_bool : tnode;virtual;
          function first_proc_to_procvar : tnode;virtual;
          function first_nil_to_methodprocvar : tnode;virtual;
          function first_set_to_set : tnode;virtual;
          function first_cord_to_pointer : tnode;virtual;
          function first_ansistring_to_pchar : tnode;virtual;
          function first_arrayconstructor_to_set : tnode;virtual;
          function first_class_to_intf : tnode;virtual;
          function first_char_to_char : tnode;virtual;
          function first_call_helper(c : tconverttype) : tnode;

          { these wrapper are necessary, because the first_* stuff is called }
          { through a table. Without the wrappers override wouldn't have     }
          { any effect                                                       }
          function _first_int_to_int : tnode;
          function _first_cstring_to_pchar : tnode;
          function _first_cstring_to_int : tnode;
          function _first_string_to_chararray : tnode;
          function _first_char_to_string : tnode;
          function _first_nothing : tnode;
          function _first_array_to_pointer : tnode;
          function _first_int_to_real : tnode;
          function _first_real_to_real: tnode;
          function _first_pointer_to_array : tnode;
          function _first_cchar_to_pchar : tnode;
          function _first_bool_to_int : tnode;
          function _first_int_to_bool : tnode;
          function _first_bool_to_bool : tnode;
          function _first_proc_to_procvar : tnode;
          function _first_nil_to_methodprocvar : tnode;
          function _first_cord_to_pointer : tnode;
          function _first_ansistring_to_pchar : tnode;
          function _first_arrayconstructor_to_set : tnode;
          function _first_class_to_intf : tnode;
          function _first_char_to_char : tnode;
          function _first_set_to_set : tnode;

          procedure _second_int_to_int;virtual;
          procedure _second_string_to_string;virtual;
          procedure _second_cstring_to_pchar;virtual;
          procedure _second_cstring_to_int;virtual;
          procedure _second_string_to_chararray;virtual;
          procedure _second_array_to_pointer;virtual;
          procedure _second_pointer_to_array;virtual;
          procedure _second_chararray_to_string;virtual;
          procedure _second_char_to_string;virtual;
          procedure _second_int_to_real;virtual;
          procedure _second_real_to_real;virtual;
          procedure _second_cord_to_pointer;virtual;
          procedure _second_proc_to_procvar;virtual;
          procedure _second_nil_to_methodprocvar;virtual;
          procedure _second_bool_to_int;virtual;
          procedure _second_int_to_bool;virtual;
          procedure _second_bool_to_bool;virtual;
          procedure _second_set_to_set;virtual;
          procedure _second_ansistring_to_pchar;virtual;
          procedure _second_class_to_intf;virtual;
          procedure _second_char_to_char;virtual;
          procedure _second_nothing; virtual;

          procedure second_int_to_int;virtual;abstract;
          procedure second_string_to_string;virtual;abstract;
          procedure second_cstring_to_pchar;virtual;abstract;
          procedure second_cstring_to_int;virtual;abstract;
          procedure second_string_to_chararray;virtual;abstract;
          procedure second_array_to_pointer;virtual;abstract;
          procedure second_pointer_to_array;virtual;abstract;
          procedure second_chararray_to_string;virtual;abstract;
          procedure second_char_to_string;virtual;abstract;
          procedure second_int_to_real;virtual;abstract;
          procedure second_real_to_real;virtual;abstract;
          procedure second_cord_to_pointer;virtual;abstract;
          procedure second_proc_to_procvar;virtual;abstract;
          procedure second_nil_to_methodprocvar;virtual;abstract;
          procedure second_bool_to_int;virtual;abstract;
          procedure second_int_to_bool;virtual;abstract;
          procedure second_bool_to_bool;virtual;abstract;
          procedure second_set_to_set;virtual;abstract;
          procedure second_ansistring_to_pchar;virtual;abstract;
          procedure second_class_to_intf;virtual;abstract;
          procedure second_char_to_char;virtual;abstract;
          procedure second_nothing; virtual;abstract;
       end;
       ttypeconvnodeclass = class of ttypeconvnode;

       tasnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function dogetcopy: tnode;override;
          destructor destroy; override;
          call: tnode;
       end;
       tasnodeclass = class of tasnode;

       tisnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          procedure pass_generate_code;override;
       end;
       tisnodeclass = class of tisnode;

    var
       ctypeconvnode : ttypeconvnodeclass;
       casnode : tasnodeclass;
       cisnode : tisnodeclass;

    procedure inserttypeconv(var p:tnode;def:tdef);
    procedure inserttypeconv_internal(var p:tnode;def:tdef);
    procedure arrayconstructor_to_set(var p : tnode);
    procedure insert_varargstypeconv(var p : tnode; iscvarargs: boolean);


implementation

   uses
      globtype,systems,constexp,
      cutils,verbose,globals,widestr,
      symconst,symdef,symsym,symbase,symtable,
      ncon,ncal,nset,nadd,ninl,nmem,nmat,nbas,nutils,
      cgbase,procinfo,
      htypechk,pass_1,cpuinfo;


{*****************************************************************************
                                   Helpers
*****************************************************************************}

    procedure inserttypeconv(var p:tnode;def:tdef);

      begin
        if not assigned(p.resultdef) then
         begin
           typecheckpass(p);
           if codegenerror then
            exit;
         end;

        { don't insert obsolete type conversions }
        if equal_defs(p.resultdef,def) and
           not ((p.resultdef.typ=setdef) and
                (tsetdef(p.resultdef).settype <>
                 tsetdef(def).settype)) then
         begin
           p.resultdef:=def;
         end
        else
         begin
           p:=ctypeconvnode.create(p,def);
           typecheckpass(p);
         end;
      end;


    procedure inserttypeconv_internal(var p:tnode;def:tdef);

      begin
        if not assigned(p.resultdef) then
         begin
           typecheckpass(p);
           if codegenerror then
            exit;
         end;

        { don't insert obsolete type conversions }
        if equal_defs(p.resultdef,def) and
           not ((p.resultdef.typ=setdef) and
                (tsetdef(p.resultdef).settype <>
                 tsetdef(def).settype)) then
         begin
           p.resultdef:=def;
         end
        else
         begin
           p:=ctypeconvnode.create_internal(p,def);
           typecheckpass(p);
         end;
      end;


{*****************************************************************************
                    Array constructor to Set Conversion
*****************************************************************************}

    procedure arrayconstructor_to_set(var p : tnode);

      var
        constp      : tsetconstnode;
        buildp,
        p2,p3,p4    : tnode;
        hdef        : tdef;
        constset    : Pconstset;
        constsetlo,
        constsethi  : TConstExprInt;

        procedure update_constsethi(def:tdef; maybetruncenumrange: boolean);
          begin
            if (def.typ=orddef) and
               ((torddef(def).high>=constsethi) or
                (torddef(def).low <=constsetlo)) then
              begin
                if torddef(def).ordtype=uwidechar then
                  begin
                    constsethi:=255;
                    constsetlo:=0;
                    if hdef=nil then
                      hdef:=def;
                  end
                else
                  begin
                    if (torddef(def).high>=constsethi) then
                      constsethi:=torddef(def).high;
                    if (torddef(def).low<=constsetlo) then
                      constsetlo:=torddef(def).low;
                    if hdef=nil then
                      begin
                         if (constsethi>255) or
                            (torddef(def).low<0) then
                           hdef:=u8inttype
                         else
                           hdef:=def;
                      end;
                    if constsethi>255 then
                      constsethi:=255;
                    if constsetlo<0 then
                      constsetlo:=0;
                  end;
              end
            else if (def.typ=enumdef) and
                    ((tenumdef(def).max>=constsethi) or
                     (tenumdef(def).min<=constsetlo)) then
              begin
                 if hdef=nil then
                   hdef:=def;
                 if (tenumdef(def).max>=constsethi) then
                   constsethi:=tenumdef(def).max;
                 if (tenumdef(def).min<=constsetlo) then
                   constsetlo:=tenumdef(def).min;
                 { for constant set elements, delphi allows the usage of elements of enumerations which
                   have value>255 if there is no element with a value > 255 used }
                 if (maybetruncenumrange) and
                    (m_delphi in current_settings.modeswitches) then
                   begin
                    if constsethi>255 then
                      constsethi:=255;
                    if constsetlo<0 then
                      constsetlo:=0;
                   end;
              end;
          end;

        procedure do_set(pos : longint);
          begin
            if (pos and not $ff)<>0 then
             Message(parser_e_illegal_set_expr);
            if pos>constsethi then
             constsethi:=pos;
            if pos<constsetlo then
             constsetlo:=pos;
            if pos in constset^ then
              Message(parser_e_illegal_set_expr);
            include(constset^,pos);
          end;

      var
        l : Longint;
        lr,hr : TConstExprInt;
        hp : tarrayconstructornode;
      begin
        if p.nodetype<>arrayconstructorn then
          internalerror(200205105);
        new(constset);
        constset^:=[];
        hdef:=nil;
        { make sure to set constsetlo correctly for empty sets }
        if assigned(tarrayconstructornode(p).left) then
          constsetlo:=high(aint)
        else
          constsetlo:=0;
        constsethi:=0;
        constp:=csetconstnode.create(nil,hdef);
        constp.value_set:=constset;
        buildp:=constp;
        hp:=tarrayconstructornode(p);
        if assigned(hp.left) then
         begin
           while assigned(hp) do
            begin
              p4:=nil; { will contain the tree to create the set }
            {split a range into p2 and p3 }
              if hp.left.nodetype=arrayconstructorrangen then
               begin
                 p2:=tarrayconstructorrangenode(hp.left).left;
                 p3:=tarrayconstructorrangenode(hp.left).right;
                 tarrayconstructorrangenode(hp.left).left:=nil;
                 tarrayconstructorrangenode(hp.left).right:=nil;
               end
              else
               begin
                 p2:=hp.left;
                 hp.left:=nil;
                 p3:=nil;
               end;
              typecheckpass(p2);
              set_varstate(p2,vs_read,[vsf_must_be_valid]);
              if assigned(p3) then
                begin
                  typecheckpass(p3);
                  set_varstate(p3,vs_read,[vsf_must_be_valid]);
                end;
              if codegenerror then
               break;
              current_filepos:=p2.fileinfo;
              case p2.resultdef.typ of
                 enumdef,
                 orddef:
                   begin
                      { widechars are not yet supported }
                      if is_widechar(p2.resultdef) then
                        begin
                          inserttypeconv(p2,cchartype);
                          if (p2.nodetype<>ordconstn) then
                            incompatibletypes(cwidechartype,cchartype);
                        end;

                      getrange(p2.resultdef,lr,hr);
                      if assigned(p3) then
                       begin
                         if is_widechar(p3.resultdef) then
                           begin
                             inserttypeconv(p3,cchartype);
                             if (p3.nodetype<>ordconstn) then
                               begin
                                 current_filepos:=p3.fileinfo;
                                 incompatibletypes(cwidechartype,cchartype);
                               end;
                           end;
                         { this isn't good, you'll get problems with
                           type t010 = 0..10;
                                ts = set of t010;
                           var  s : ts;b : t010
                           begin  s:=[1,2,b]; end.
                         if is_integer(p3^.resultdef) then
                          begin
                            inserttypeconv(p3,u8bitdef);
                          end;
                         }
                         if assigned(hdef) and not(equal_defs(hdef,p3.resultdef)) then
                           begin
                              CGMessagePos(p3.fileinfo,type_e_typeconflict_in_set);
                           end
                         else
                           begin
                             if (p2.nodetype=ordconstn) and (p3.nodetype=ordconstn) then
                              begin
                                 if not(is_integer(p3.resultdef)) then
                                   hdef:=p3.resultdef
                                 else
                                   begin
                                     inserttypeconv(p3,u8inttype);
                                     inserttypeconv(p2,u8inttype);
                                   end;

                                for l:=tordconstnode(p2).value.svalue to tordconstnode(p3).value.svalue do
                                  do_set(l);
                                p2.free;
                                p3.free;
                              end
                             else
                              begin
                                update_constsethi(p2.resultdef,false);
                                inserttypeconv(p2,hdef);

                                update_constsethi(p3.resultdef,false);
                                inserttypeconv(p3,hdef);

                                if assigned(hdef) then
                                  inserttypeconv(p3,hdef)
                                else
                                  inserttypeconv(p3,u8inttype);
                                p4:=csetelementnode.create(p2,p3);
                              end;
                           end;
                       end
                      else
                       begin
                         { Single value }
                         if p2.nodetype=ordconstn then
                          begin
                            if not(is_integer(p2.resultdef)) then
                              update_constsethi(p2.resultdef,true);

                            if assigned(hdef) then
                              inserttypeconv(p2,hdef)
                            else
                              inserttypeconv(p2,u8inttype);

                            do_set(tordconstnode(p2).value.svalue);
                            p2.free;
                          end
                         else
                          begin
                            update_constsethi(p2.resultdef,false);

                            if assigned(hdef) then
                              inserttypeconv(p2,hdef)
                            else
                              inserttypeconv(p2,u8inttype);

                            p4:=csetelementnode.create(p2,nil);
                          end;
                       end;
                    end;

                  stringdef :
                    begin
                        { if we've already set elements which are constants }
                        { throw an error                                    }
                        if ((hdef=nil) and assigned(buildp)) or
                          not(is_char(hdef)) then
                          CGMessage(type_e_typeconflict_in_set)
                        else
                         for l:=1 to length(pshortstring(tstringconstnode(p2).value_str)^) do
                          do_set(ord(pshortstring(tstringconstnode(p2).value_str)^[l]));
                        if hdef=nil then
                         hdef:=cchartype;
                        p2.free;
                      end;

                    else
                      CGMessage(type_e_ordinal_expr_expected);
              end;
              { insert the set creation tree }
              if assigned(p4) then
               buildp:=caddnode.create(addn,buildp,p4);
              { load next and dispose current node }
              p2:=hp;
              hp:=tarrayconstructornode(tarrayconstructornode(p2).right);
              tarrayconstructornode(p2).right:=nil;
              p2.free;
            end;
           if (hdef=nil) then
            hdef:=u8inttype;
         end
        else
         begin
           { empty set [], only remove node }
           p.free;
         end;
        { set the initial set type }
        constp.resultdef:=tsetdef.create(hdef,constsetlo.svalue,constsethi.svalue);
        { determine the resultdef for the tree }
        typecheckpass(buildp);
        { set the new tree }
        p:=buildp;
      end;


    procedure insert_varargstypeconv(var p : tnode; iscvarargs: boolean);
      begin
        if not(iscvarargs) and
           (p.nodetype=stringconstn) then
          p:=ctypeconvnode.create_internal(p,cansistringtype)
        else
          case p.resultdef.typ of
            enumdef :
              p:=ctypeconvnode.create_internal(p,s32inttype);
            arraydef :
              begin
                if is_chararray(p.resultdef) then
                  p:=ctypeconvnode.create_internal(p,charpointertype)
                else
                  if is_widechararray(p.resultdef) then
                    p:=ctypeconvnode.create_internal(p,widecharpointertype)
                else
                  CGMessagePos1(p.fileinfo,type_e_wrong_type_in_array_constructor,p.resultdef.typename);
              end;
            orddef :
              begin
                if is_integer(p.resultdef) and
                   not(is_64bitint(p.resultdef)) then
                  p:=ctypeconvnode.create(p,s32inttype)
                else if is_void(p.resultdef) then
                  CGMessagePos1(p.fileinfo,type_e_wrong_type_in_array_constructor,p.resultdef.typename)
                else if iscvarargs and is_currency(p.resultdef)
                    and (current_settings.fputype<>fpu_none) then
                  p:=ctypeconvnode.create(p,s64floattype);
              end;
            floatdef :
              if not(iscvarargs) then
                begin
                  if not(is_currency(p.resultdef)) then
                    p:=ctypeconvnode.create(p,pbestrealtype^);
                end
              else
                begin
                  if is_constrealnode(p) and
                     not(nf_explicit in p.flags) then
                    MessagePos(p.fileinfo,type_w_double_c_varargs);
                  if (tfloatdef(p.resultdef).floattype in [s32real,s64currency]) or
                     (is_constrealnode(p) and
                      not(nf_explicit in p.flags)) then
                    p:=ctypeconvnode.create(p,s64floattype);
                end;
            procvardef :
              p:=ctypeconvnode.create(p,voidpointertype);
            stringdef:
              if iscvarargs then
                p:=ctypeconvnode.create(p,charpointertype);
            variantdef:
              if iscvarargs then
                CGMessagePos1(p.fileinfo,type_e_wrong_type_in_array_constructor,p.resultdef.typename);
            pointerdef:
              ;
            classrefdef:
              if iscvarargs then
                p:=ctypeconvnode.create(p,voidpointertype);
            objectdef :
              if iscvarargs or
                 is_object(p.resultdef) then
                CGMessagePos1(p.fileinfo,type_e_wrong_type_in_array_constructor,p.resultdef.typename);
            else
              CGMessagePos1(p.fileinfo,type_e_wrong_type_in_array_constructor,p.resultdef.typename);
          end;
        typecheckpass(p);
      end;

{*****************************************************************************
                           TTYPECONVNODE
*****************************************************************************}


    constructor ttypeconvnode.create(node : tnode;def:tdef);

      begin
         inherited create(typeconvn,node);
         convtype:=tc_none;
         totypedef:=def;
         if def=nil then
          internalerror(200103281);
         fileinfo:=node.fileinfo;
         {An attempt to convert the result of a floating point division
          (with the / operator) to an integer type will fail. Give a hint
          to use the div operator.}
         if (node.nodetype=slashn) and (def.typ=orddef) then
           cgmessage(type_h_use_div_for_int);
         {In expressions like int64:=longint+longint, an integer overflow could be avoided
          by simply converting the operands to int64 first. Give a hint to do this.}
         if (node.nodetype in [addn,subn,muln]) and
            (def.typ=orddef) and (node.resultdef<>nil) and (node.resultdef.typ=orddef) and
            ((Torddef(node.resultdef).low>=Torddef(def).low) and (Torddef(node.resultdef).high<=Torddef(def).high)) and
            ((Torddef(node.resultdef).low>Torddef(def).low) or (Torddef(node.resultdef).high<Torddef(def).high)) then
           case node.nodetype of
             addn:
               cgmessage1(type_h_convert_add_operands_to_prevent_overflow,def.gettypename);
             subn:
               cgmessage1(type_h_convert_sub_operands_to_prevent_overflow,def.gettypename);
             muln:
               cgmessage1(type_h_convert_mul_operands_to_prevent_overflow,def.gettypename);
           end;
      end;


    constructor ttypeconvnode.create_explicit(node : tnode;def:tdef);

      begin
         self.create(node,def);
         include(flags,nf_explicit);
      end;


    constructor ttypeconvnode.create_internal(node : tnode;def:tdef);

      begin
         self.create(node,def);
         { handle like explicit conversions }
         include(flags,nf_explicit);
         include(flags,nf_internal);
      end;


    constructor ttypeconvnode.create_proc_to_procvar(node : tnode);

      begin
         self.create(node,voidtype);
         convtype:=tc_proc_2_procvar;
      end;


    constructor ttypeconvnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(totypedefderef);
        convtype:=tconverttype(ppufile.getbyte);
      end;


    procedure ttypeconvnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(totypedefderef);
        ppufile.putbyte(byte(convtype));
      end;


    procedure ttypeconvnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        totypedefderef.build(totypedef);
      end;


    procedure ttypeconvnode.derefimpl;
      begin
        inherited derefimpl;
        totypedef:=tdef(totypedefderef.resolve);
      end;


    function ttypeconvnode.dogetcopy : tnode;
      var
         n : ttypeconvnode;
      begin
         n:=ttypeconvnode(inherited dogetcopy);
         n.convtype:=convtype;
         n.totypedef:=totypedef;
         dogetcopy:=n;
      end;

    procedure ttypeconvnode.printnodeinfo(var t : text);
      const
        convtyp2str : array[tconverttype] of pchar = (
          'tc_none',
          'tc_equal',
          'tc_not_possible',
          'tc_string_2_string',
          'tc_char_2_string',
          'tc_char_2_chararray',
          'tc_pchar_2_string',
          'tc_cchar_2_pchar',
          'tc_cstring_2_pchar',
          'tc_cstring_2_int',
          'tc_ansistring_2_pchar',
          'tc_string_2_chararray',
          'tc_chararray_2_string',
          'tc_array_2_pointer',
          'tc_pointer_2_array',
          'tc_int_2_int',
          'tc_int_2_bool',
          'tc_bool_2_bool',
          'tc_bool_2_int',
          'tc_real_2_real',
          'tc_int_2_real',
          'tc_real_2_currency',
          'tc_proc_2_procvar',
          'tc_nil_2_methodprocvar',
          'tc_arrayconstructor_2_set',
          'tc_set_2_set',
          'tc_cord_2_pointer',
          'tc_intf_2_string',
          'tc_intf_2_guid',
          'tc_class_2_intf',
          'tc_char_2_char',
          'tc_dynarray_2_openarray',
          'tc_pwchar_2_string',
          'tc_variant_2_dynarray',
          'tc_dynarray_2_variant',
          'tc_variant_2_enum',
          'tc_enum_2_variant',
          'tc_interface_2_variant',
          'tc_variant_2_interface',
          'tc_array_2_dynarray'
        );
      begin
        inherited printnodeinfo(t);
        write(t,', convtype = ',strpas(convtyp2str[convtype]));
      end;


    function ttypeconvnode.typecheck_cord_to_pointer : tnode;

      var
        t : tnode;

      begin
        result:=nil;
        if left.nodetype=ordconstn then
          begin
            { check if we have a valid pointer constant (JM) }
            if (sizeof(pointer) > sizeof(TConstPtrUInt)) then
              if (sizeof(TConstPtrUInt) = 4) then
                begin
                  if (tordconstnode(left).value < int64(low(longint))) or
                     (tordconstnode(left).value > int64(high(cardinal))) then
                  CGMessage(parser_e_range_check_error);
                end
              else if (sizeof(TConstPtrUInt) = 8) then
                begin
                  if (tordconstnode(left).value < int64(low(int64))) or
                     (tordconstnode(left).value > int64(high(qword))) then
                  CGMessage(parser_e_range_check_error);
                end
              else
                internalerror(2001020801);
            t:=cpointerconstnode.create(TConstPtrUInt(tordconstnode(left).value.uvalue),resultdef);
            result:=t;
          end
         else
          internalerror(200104023);
      end;


    function ttypeconvnode.typecheck_chararray_to_string : tnode;
      var
        chartype : string[8];
        newblock : tblocknode;
        newstat  : tstatementnode;
        restemp  : ttempcreatenode;
      begin
        if is_widechar(tarraydef(left.resultdef).elementdef) then
          chartype:='widechar'
        else
          chartype:='char';
        if tstringdef(resultdef).stringtype=st_shortstring then
          begin
            newblock:=internalstatements(newstat);
            restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
            addstatement(newstat,restemp);
            addstatement(newstat,ccallnode.createintern('fpc_'+chartype+'array_to_shortstr',
              ccallparanode.create(cordconstnode.create(
                ord(tarraydef(left.resultdef).lowrange=0),booltype,false),
              ccallparanode.create(left,ccallparanode.create(
              ctemprefnode.create(restemp),nil)))));
            addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
            addstatement(newstat,ctemprefnode.create(restemp));
            result:=newblock;
          end
        else
          result:=ccallnode.createinternres(
            'fpc_'+chartype+'array_to_'+tstringdef(resultdef).stringtypname,
            ccallparanode.create(cordconstnode.create(
               ord(tarraydef(left.resultdef).lowrange=0),booltype,false),
             ccallparanode.create(left,nil)),resultdef);
        left:=nil;
      end;


    function ttypeconvnode.typecheck_string_to_chararray : tnode;
      var
        newblock : tblocknode;
        newstat  : tstatementnode;
        restemp  : ttempcreatenode;
        pchtemp  : pchar;
        arrsize  : aint;
        chartype : string[8];
      begin
        result := nil;
        with tarraydef(resultdef) do
          begin
            if highrange<lowrange then
             internalerror(200501051);
            arrsize := highrange-lowrange+1;
          end;
        if (left.nodetype = stringconstn) and
           (tstringconstnode(left).cst_type=cst_conststring) then
           begin
             { if the array of char is large enough we can use the string
               constant directly. This is handled in ncgcnv }
             if (arrsize>=tstringconstnode(left).len) and
                is_char(tarraydef(resultdef).elementdef) then
               begin
                 { pad the constant string with #0 to the array len }
                 { (2.0.x compatible)                               }
                 if (arrsize>tstringconstnode(left).len) then
                   begin
                     pchtemp:=concatansistrings(tstringconstnode(left).value_str,pchar(StringOfChar(#0,arrsize-tstringconstnode(left).len)),tstringconstnode(left).len,arrsize-tstringconstnode(left).len);
                     left.free;
                     left:=cstringconstnode.createpchar(pchtemp,arrsize);
                     typecheckpass(left);
                   end;
                 exit;
               end;
             { Convert to wide/short/ansistring and call default helper }
             if is_widechar(tarraydef(resultdef).elementdef) then
               inserttypeconv(left,cwidestringtype)
             else
               begin
                 if tstringconstnode(left).len>255 then
                   inserttypeconv(left,cansistringtype)
                 else
                   inserttypeconv(left,cshortstringtype);
               end;
           end;
        if is_widechar(tarraydef(resultdef).elementdef) then
          chartype:='widechar'
        else
          chartype:='char';
        newblock:=internalstatements(newstat);
        restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
        addstatement(newstat,restemp);
        addstatement(newstat,ccallnode.createintern('fpc_'+tstringdef(left.resultdef).stringtypname+
          '_to_'+chartype+'array',ccallparanode.create(left,ccallparanode.create(
          ctemprefnode.create(restemp),nil))));
        addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
        addstatement(newstat,ctemprefnode.create(restemp));
        result:=newblock;
        left:=nil;
      end;


    function ttypeconvnode.typecheck_string_to_string : tnode;
      var
        procname: string[31];
        newblock : tblocknode;
        newstat  : tstatementnode;
        restemp  : ttempcreatenode;
      begin
         result:=nil;
         if (left.nodetype=stringconstn) and
            ((not is_widechararray(left.resultdef) and
              not is_widestring(left.resultdef)) or
             (tstringdef(resultdef).stringtype=st_widestring) or
             { non-ascii chars would be replaced with '?' -> loses info }
             not hasnonasciichars(pcompilerwidestring(tstringconstnode(left).value_str))) then
           begin
             tstringconstnode(left).changestringtype(resultdef);
             result:=left;
             left:=nil;
           end
         else
           begin
             { get the correct procedure name }
             procname := 'fpc_'+tstringdef(left.resultdef).stringtypname+
                         '_to_'+tstringdef(resultdef).stringtypname;

             if tstringdef(resultdef).stringtype=st_shortstring then
               begin
                 newblock:=internalstatements(newstat);
                 restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
                 addstatement(newstat,restemp);
                 addstatement(newstat,ccallnode.createintern(procname,ccallparanode.create(left,ccallparanode.create(
                   ctemprefnode.create(restemp),nil))));
                 addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
                 addstatement(newstat,ctemprefnode.create(restemp));
                 result:=newblock;
               end
             else
               result := ccallnode.createinternres(procname,ccallparanode.create(left,nil),resultdef);
             left:=nil;
           end;
      end;


    function ttypeconvnode.typecheck_char_to_string : tnode;

      var
         procname: string[31];
         para : tcallparanode;
         hp : tstringconstnode;
         ws : pcompilerwidestring;
         newblock : tblocknode;
         newstat  : tstatementnode;
         restemp  : ttempcreatenode;

      begin
         result:=nil;
         { we can't do widechar to ansichar conversions at compile time, since }
         { this maps all non-ascii chars to '?' -> loses information           }
         if (left.nodetype=ordconstn) and
            ((tstringdef(resultdef).stringtype=st_widestring) or
             (torddef(left.resultdef).ordtype=uchar) or
             { >=128 is destroyed }
             (tordconstnode(left).value.uvalue<128)) then
           begin
              if tstringdef(resultdef).stringtype=st_widestring then
               begin
                 initwidestring(ws);
                 if torddef(left.resultdef).ordtype=uwidechar then
                   concatwidestringchar(ws,tcompilerwidechar(tordconstnode(left).value.uvalue))
                 else
                   concatwidestringchar(ws,tcompilerwidechar(chr(tordconstnode(left).value.uvalue)));
                 hp:=cstringconstnode.createwstr(ws);
                 donewidestring(ws);
               end
              else
                begin
                  if torddef(left.resultdef).ordtype=uwidechar then
                    hp:=cstringconstnode.createstr(unicode2asciichar(tcompilerwidechar(tordconstnode(left).value.uvalue)))
                  else
                    hp:=cstringconstnode.createstr(chr(tordconstnode(left).value.uvalue));
                  tstringconstnode(hp).changestringtype(resultdef);
                end;
              result:=hp;
           end
         else
           { shortstrings are handled 'inline' (except for widechars) }
           if (tstringdef(resultdef).stringtype <> st_shortstring) or
              (torddef(left.resultdef).ordtype = uwidechar) then
             begin
               if (tstringdef(resultdef).stringtype <> st_shortstring) then
                 begin
                   { create the procname }
                   if torddef(left.resultdef).ordtype<>uwidechar then
                     procname := 'fpc_char_to_'
                   else
                     procname := 'fpc_wchar_to_';
                   procname:=procname+tstringdef(resultdef).stringtypname;

                   { and the parameter }
                   para := ccallparanode.create(left,nil);

                   { and finally the call }
                   result := ccallnode.createinternres(procname,para,resultdef);
                 end
               else
                 begin
                   newblock:=internalstatements(newstat);
                   restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
                   addstatement(newstat,restemp);
                   addstatement(newstat,ccallnode.createintern('fpc_wchar_to_shortstr',ccallparanode.create(left,ccallparanode.create(
                     ctemprefnode.create(restemp),nil))));
                   addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
                   addstatement(newstat,ctemprefnode.create(restemp));
                   result:=newblock;
                 end;
               left := nil;
             end
           else
             begin
               { create word(byte(char) shl 8 or 1) for litte endian machines }
               { and word(byte(char) or 256) for big endian machines          }
               left := ctypeconvnode.create_internal(left,u8inttype);
               if (target_info.endian = endian_little) then
                 left := caddnode.create(orn,
                   cshlshrnode.create(shln,left,cordconstnode.create(8,s32inttype,false)),
                   cordconstnode.create(1,s32inttype,false))
               else
                 left := caddnode.create(orn,left,
                   cordconstnode.create(1 shl 8,s32inttype,false));
               left := ctypeconvnode.create_internal(left,u16inttype);
               typecheckpass(left);
             end;
      end;


    function ttypeconvnode.typecheck_char_to_chararray : tnode;

      begin
        if resultdef.size <> 1 then
          begin
            { convert first to string, then to chararray }
            inserttypeconv(left,cshortstringtype);
            inserttypeconv(left,resultdef);
            result:=left;
            left := nil;
            exit;
          end;
        result := nil;
      end;


    function ttypeconvnode.typecheck_char_to_char : tnode;

      var
         hp : tordconstnode;

      begin
         result:=nil;
         if (left.nodetype=ordconstn) and
            ((torddef(resultdef).ordtype<>uchar) or
             (torddef(left.resultdef).ordtype<>uwidechar) or
             { >= 128 is replaced by '?' currently -> loses information }
             (tordconstnode(left).value.uvalue<128)) then
           begin
             if (torddef(resultdef).ordtype=uchar) and
                (torddef(left.resultdef).ordtype=uwidechar) then
              begin
                hp:=cordconstnode.create(
                      ord(unicode2asciichar(tcompilerwidechar(tordconstnode(left).value.uvalue))),
                      cchartype,true);
                result:=hp;
              end
             else if (torddef(resultdef).ordtype=uwidechar) and
                     (torddef(left.resultdef).ordtype=uchar) then
              begin
                hp:=cordconstnode.create(
                      asciichar2unicode(chr(tordconstnode(left).value.uvalue)),
                      cwidechartype,true);
                result:=hp;
              end
             else
              internalerror(200105131);
             exit;
           end;
      end;


    function ttypeconvnode.typecheck_int_to_int : tnode;
      var
        v : TConstExprInt;
      begin
        result:=nil;
        if left.nodetype=ordconstn then
         begin
           v:=tordconstnode(left).value;
           if is_currency(resultdef) then
             v:=v*10000;
           if (resultdef.typ=pointerdef) then
             result:=cpointerconstnode.create(TConstPtrUInt(v.uvalue),resultdef)
           else
             begin
               if is_currency(left.resultdef) then
                 v:=v div 10000;
               result:=cordconstnode.create(v,resultdef,false);
             end;
         end
        else if left.nodetype=pointerconstn then
         begin
           v:=tpointerconstnode(left).value;
           if (resultdef.typ=pointerdef) then
             result:=cpointerconstnode.create(v.uvalue,resultdef)
           else
             begin
               if is_currency(resultdef) then
                 v:=v*10000;
               result:=cordconstnode.create(v,resultdef,false);
             end;
         end
        else
         begin
           { multiply by 10000 for currency. We need to use getcopy to pass
             the argument because the current node is always disposed. Only
             inserting the multiply in the left node is not possible because
             it'll get in an infinite loop to convert int->currency }
           if is_currency(resultdef) then
            begin
              result:=caddnode.create(muln,getcopy,cordconstnode.create(10000,resultdef,false));
              include(result.flags,nf_is_currency);
            end
           else if is_currency(left.resultdef) then
            begin
              result:=cmoddivnode.create(divn,getcopy,cordconstnode.create(10000,resultdef,false));
              include(result.flags,nf_is_currency);
            end;
         end;
      end;


    function ttypeconvnode.typecheck_int_to_real : tnode;
      var
        rv : bestreal;
      begin
        result:=nil;
        if left.nodetype=ordconstn then
         begin
           rv:=tordconstnode(left).value;
           if is_currency(resultdef) then
             rv:=rv*10000.0
           else if is_currency(left.resultdef) then
             rv:=rv/10000.0;
           result:=crealconstnode.create(rv,resultdef);
         end
        else
         begin
           { multiply by 10000 for currency. We need to use getcopy to pass
             the argument because the current node is always disposed. Only
             inserting the multiply in the left node is not possible because
             it'll get in an infinite loop to convert int->currency }
           if is_currency(resultdef) then
            begin
              result:=caddnode.create(muln,getcopy,crealconstnode.create(10000.0,resultdef));
              include(result.flags,nf_is_currency);
            end
           else if is_currency(left.resultdef) then
            begin
              result:=caddnode.create(slashn,getcopy,crealconstnode.create(10000.0,resultdef));
              include(result.flags,nf_is_currency);
            end;
         end;
      end;


    function ttypeconvnode.typecheck_real_to_currency : tnode;
      begin
        if not is_currency(resultdef) then
          internalerror(200304221);
        result:=nil;
        left:=caddnode.create(muln,left,crealconstnode.create(10000.0,left.resultdef));
        include(left.flags,nf_is_currency);
        typecheckpass(left);
        { Convert constants directly, else call Round() }
        if left.nodetype=realconstn then
          result:=cordconstnode.create(round(trealconstnode(left).value_real),resultdef,false)
        else
          begin
            result:=ccallnode.createinternres('fpc_round_real',
              ccallparanode.create(left,nil),resultdef);
            left:=nil;
          end;
      end;


    function ttypeconvnode.typecheck_real_to_real : tnode;
      begin
         result:=nil;
         if is_currency(left.resultdef) and not(is_currency(resultdef)) then
           begin
             left:=caddnode.create(slashn,left,crealconstnode.create(10000.0,left.resultdef));
             include(left.flags,nf_is_currency);
             typecheckpass(left);
           end
         else
           if is_currency(resultdef) and not(is_currency(left.resultdef)) then
             begin
               left:=caddnode.create(muln,left,crealconstnode.create(10000.0,left.resultdef));
               include(left.flags,nf_is_currency);
               typecheckpass(left);
             end;
      end;


    function ttypeconvnode.typecheck_cchar_to_pchar : tnode;

      begin
         result:=nil;
         if is_pwidechar(resultdef) then
          inserttypeconv(left,cwidestringtype)
         else
          inserttypeconv(left,cshortstringtype);
         { evaluate again, reset resultdef so the convert_typ
           will be calculated again and cstring_to_pchar will
           be used for futher conversion }
         convtype:=tc_none;
         result:=pass_typecheck;
      end;


    function ttypeconvnode.typecheck_cstring_to_pchar : tnode;

      begin
         result:=nil;
         if is_pwidechar(resultdef) then
           inserttypeconv(left,cwidestringtype)
         else
           if is_pchar(resultdef) and
              is_widestring(left.resultdef) then
             begin
               inserttypeconv(left,cansistringtype);
               { the second pass of second_cstring_to_pchar expects a  }
               { strinconstn, but this may become a call to the        }
               { widestring manager in case left contains "high ascii" }
               if (left.nodetype<>stringconstn) then
                 begin
                   result:=left;
                   left:=nil;
                 end;
             end;
      end;


    function ttypeconvnode.typecheck_cstring_to_int : tnode;
      var
        fcc : cardinal;
        pb  : pbyte;
      begin
         result:=nil;
         if left.nodetype<>stringconstn then
           internalerror(200510012);
         if tstringconstnode(left).len=4 then
           begin
             pb:=pbyte(tstringconstnode(left).value_str);
             fcc:=(pb[0] shl 24) or (pb[1] shl 16) or (pb[2] shl 8) or pb[3];
             result:=cordconstnode.create(fcc,u32inttype,false);
           end
         else
           CGMessage2(type_e_illegal_type_conversion,left.resultdef.GetTypeName,resultdef.GetTypeName);
      end;


    function ttypeconvnode.typecheck_arrayconstructor_to_set : tnode;
      var
        hp : tnode;
      begin
        result:=nil;
        if left.nodetype<>arrayconstructorn then
         internalerror(5546);
        { remove typeconv node }
        hp:=left;
        left:=nil;
        { create a set constructor tree }
        arrayconstructor_to_set(hp);
        result:=hp;
      end;


    function ttypeconvnode.typecheck_set_to_set : tnode;
      begin
        result:=nil;
        { constant sets can be converted by changing the type only }
        if (left.nodetype=setconstn) then
         begin
           left.resultdef:=resultdef;
           result:=left;
           left:=nil;
           exit;
         end;
      end;


    function ttypeconvnode.typecheck_pchar_to_string : tnode;
      var
        newblock : tblocknode;
        newstat  : tstatementnode;
        restemp  : ttempcreatenode;
      begin
        if tstringdef(resultdef).stringtype=st_shortstring then
          begin
            newblock:=internalstatements(newstat);
            restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
            addstatement(newstat,restemp);
            addstatement(newstat,ccallnode.createintern('fpc_pchar_to_shortstr',ccallparanode.create(left,ccallparanode.create(
              ctemprefnode.create(restemp),nil))));
            addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
            addstatement(newstat,ctemprefnode.create(restemp));
            result:=newblock;
          end
        else
          result := ccallnode.createinternres(
            'fpc_pchar_to_'+tstringdef(resultdef).stringtypname,
            ccallparanode.create(left,nil),resultdef);
        left:=nil;
      end;


    function ttypeconvnode.typecheck_interface_to_guid : tnode;
      begin
        if assigned(tobjectdef(left.resultdef).iidguid) then
          begin
            if not(oo_has_valid_guid in tobjectdef(left.resultdef).objectoptions) then
              CGMessage1(type_interface_has_no_guid,tobjectdef(left.resultdef).typename);
            result:=cguidconstnode.create(tobjectdef(left.resultdef).iidguid^);
          end;
      end;


    function ttypeconvnode.typecheck_dynarray_to_openarray : tnode;
      begin
        { a dynamic array is a pointer to an array, so to convert it to }
        { an open array, we have to dereference it (JM)                 }
        result := ctypeconvnode.create_internal(left,voidpointertype);
        typecheckpass(result);
        { left is reused }
        left := nil;
        result := cderefnode.create(result);
        include(result.flags,nf_no_checkpointer);
        result.resultdef := resultdef;
      end;


    function ttypeconvnode.typecheck_pwchar_to_string : tnode;
      var
        newblock : tblocknode;
        newstat  : tstatementnode;
        restemp  : ttempcreatenode;
      begin
        if tstringdef(resultdef).stringtype=st_shortstring then
          begin
            newblock:=internalstatements(newstat);
            restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
            addstatement(newstat,restemp);
            addstatement(newstat,ccallnode.createintern('fpc_pwidechar_to_shortstr',ccallparanode.create(left,ccallparanode.create(
              ctemprefnode.create(restemp),nil))));
            addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
            addstatement(newstat,ctemprefnode.create(restemp));
            result:=newblock;
          end
        else
          result := ccallnode.createinternres(
            'fpc_pwidechar_to_'+tstringdef(resultdef).stringtypname,
            ccallparanode.create(left,nil),resultdef);
        left:=nil;
      end;


    function ttypeconvnode.typecheck_variant_to_dynarray : tnode;
      begin
        result := ccallnode.createinternres(
          'fpc_variant_to_dynarray',
          ccallparanode.create(caddrnode.create_internal(crttinode.create(tstoreddef(resultdef),initrtti,rdt_normal)),
            ccallparanode.create(left,nil)
          ),resultdef);
        typecheckpass(result);
        left:=nil;
      end;


    function ttypeconvnode.typecheck_dynarray_to_variant : tnode;
      begin
        result := ccallnode.createinternres(
          'fpc_dynarray_to_variant',
          ccallparanode.create(caddrnode.create_internal(crttinode.create(tstoreddef(left.resultdef),initrtti,rdt_normal)),
            ccallparanode.create(ctypeconvnode.create_explicit(left,voidpointertype),nil)
          ),resultdef);
        typecheckpass(result);
        left:=nil;
      end;


    function ttypeconvnode.typecheck_variant_to_interface : tnode;
      begin
        if tobjectdef(resultdef).is_related(tobjectdef(search_system_type('IDISPATCH').typedef)) then
          result := ccallnode.createinternres(
            'fpc_variant_to_idispatch',
              ccallparanode.create(left,nil)
            ,resultdef)
        else
          result := ccallnode.createinternres(
            'fpc_variant_to_interface',
              ccallparanode.create(left,nil)
            ,resultdef);
        typecheckpass(result);
        left:=nil;
      end;


    function ttypeconvnode.typecheck_interface_to_variant : tnode;
      begin
        if tobjectdef(left.resultdef).is_related(tobjectdef(search_system_type('IDISPATCH').typedef)) then
          result := ccallnode.createinternres(
            'fpc_idispatch_to_variant',
              ccallparanode.create(left,nil)
            ,resultdef)
        else
          result := ccallnode.createinternres(
            'fpc_interface_to_variant',
              ccallparanode.create(left,nil)
            ,resultdef);
        typecheckpass(result);
        left:=nil;
      end;


    function ttypeconvnode.typecheck_variant_to_enum : tnode;

      begin
        result := ctypeconvnode.create_internal(left,sinttype);
        result := ctypeconvnode.create_internal(result,resultdef);
        typecheckpass(result);
        { left is reused }
        left := nil;
      end;


    function ttypeconvnode.typecheck_enum_to_variant : tnode;

      begin
        result := ctypeconvnode.create_internal(left,sinttype);
        result := ctypeconvnode.create_internal(result,cvarianttype);
        typecheckpass(result);
        { left is reused }
        left := nil;
      end;


    function ttypeconvnode.typecheck_array_2_dynarray : tnode;
      var
        newstatement : tstatementnode;
        temp         : ttempcreatenode;
        temp2        : ttempcreatenode;
      begin
        { create statements with call to getmem+initialize }
        result:=internalstatements(newstatement);

        { create temp for result }
        temp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,true);
        addstatement(newstatement,temp);

        { get temp for array of lengths }
        temp2:=ctempcreatenode.create(sinttype,sinttype.size,tt_persistent,false);
        addstatement(newstatement,temp2);

        { one dimensional }
        addstatement(newstatement,cassignmentnode.create(
            ctemprefnode.create_offset(temp2,0),
            cordconstnode.create
               (tarraydef(left.resultdef).highrange+1,s32inttype,true)));
        { create call to fpc_dynarr_setlength }
        addstatement(newstatement,ccallnode.createintern('fpc_dynarray_setlength',
            ccallparanode.create(caddrnode.create_internal
                  (ctemprefnode.create(temp2)),
               ccallparanode.create(cordconstnode.create
                  (1,s32inttype,true),
               ccallparanode.create(caddrnode.create_internal
                  (crttinode.create(tstoreddef(resultdef),initrtti,rdt_normal)),
               ccallparanode.create(
                 ctypeconvnode.create_internal(
                   ctemprefnode.create(temp),voidpointertype),
                 nil))))

          ));
        addstatement(newstatement,ctempdeletenode.create(temp2));

        { copy ... }
        addstatement(newstatement,cassignmentnode.create(
          ctypeconvnode.create_internal(cderefnode.create(ctypeconvnode.create_internal(ctemprefnode.create(temp),voidpointertype)),left.resultdef),
          left
        ));
        { left is reused }
        left:=nil;
        { the last statement should return the value as
          location and type, this is done be referencing the
          temp and converting it first from a persistent temp to
          normal temp }
        addstatement(newstatement,ctempdeletenode.create_normal_temp(temp));
        addstatement(newstatement,ctemprefnode.create(temp));
      end;


    procedure copyparasym(p:TObject;arg:pointer);
      var
        newparast : TSymtable absolute arg;
        vs : tparavarsym;
      begin
        if tsym(p).typ<>paravarsym then
          exit;
        with tparavarsym(p) do
          begin
            vs:=tparavarsym.create(realname,paranr,varspez,vardef,varoptions);
            vs.defaultconstsym:=defaultconstsym;
            newparast.insert(vs);
          end;
      end;


    function ttypeconvnode.typecheck_proc_to_procvar : tnode;
      var
        pd : tabstractprocdef;
      begin
        result:=nil;
        pd:=tabstractprocdef(left.resultdef);

        { create procvardef }
        resultdef:=tprocvardef.create(pd.parast.symtablelevel);
        tprocvardef(resultdef).proctypeoption:=pd.proctypeoption;
        tprocvardef(resultdef).proccalloption:=pd.proccalloption;
        tprocvardef(resultdef).procoptions:=pd.procoptions;
        tprocvardef(resultdef).returndef:=pd.returndef;

        { method ? then set the methodpointer flag }
        if (pd.owner.symtabletype=ObjectSymtable) then
          include(tprocvardef(resultdef).procoptions,po_methodpointer);

        { was it a local procedure? }
        if (pd.owner.symtabletype=localsymtable) then
          include(tprocvardef(resultdef).procoptions,po_local);

        { only need the address of the method? this is needed
          for @tobject.create. In this case there will be a loadn without
          a methodpointer. }
        if (left.nodetype=loadn) and
           not assigned(tloadnode(left).left) then
          include(tprocvardef(resultdef).procoptions,po_addressonly);

        { Add parameters use only references, we don't need to keep the
          parast. We use the parast from the original function to calculate
          our parameter data and reset it afterwards }
        pd.parast.SymList.ForEachCall(@copyparasym,tprocvardef(resultdef).parast);
        tprocvardef(resultdef).calcparas;
      end;


    function ttypeconvnode.typecheck_call_helper(c : tconverttype) : tnode;
      const
         resultdefconvert : array[tconverttype] of pointer = (
          {none} nil,
          {equal} nil,
          {not_possible} nil,
          { string_2_string } @ttypeconvnode.typecheck_string_to_string,
          { char_2_string } @ttypeconvnode.typecheck_char_to_string,
          { char_2_chararray } @ttypeconvnode.typecheck_char_to_chararray,
          { pchar_2_string } @ttypeconvnode.typecheck_pchar_to_string,
          { cchar_2_pchar } @ttypeconvnode.typecheck_cchar_to_pchar,
          { cstring_2_pchar } @ttypeconvnode.typecheck_cstring_to_pchar,
          { cstring_2_int } @ttypeconvnode.typecheck_cstring_to_int,
          { ansistring_2_pchar } nil,
          { string_2_chararray } @ttypeconvnode.typecheck_string_to_chararray,
          { chararray_2_string } @ttypeconvnode.typecheck_chararray_to_string,
          { array_2_pointer } nil,
          { pointer_2_array } nil,
          { int_2_int } @ttypeconvnode.typecheck_int_to_int,
          { int_2_bool } nil,
          { bool_2_bool } nil,
          { bool_2_int } nil,
          { real_2_real } @ttypeconvnode.typecheck_real_to_real,
          { int_2_real } @ttypeconvnode.typecheck_int_to_real,
          { real_2_currency } @ttypeconvnode.typecheck_real_to_currency,
          { proc_2_procvar } @ttypeconvnode.typecheck_proc_to_procvar,
          { nil_2_methodprocvar } nil,
          { arrayconstructor_2_set } @ttypeconvnode.typecheck_arrayconstructor_to_set,
          { set_to_set } @ttypeconvnode.typecheck_set_to_set,
          { cord_2_pointer } @ttypeconvnode.typecheck_cord_to_pointer,
          { intf_2_string } nil,
          { intf_2_guid } @ttypeconvnode.typecheck_interface_to_guid,
          { class_2_intf } nil,
          { char_2_char } @ttypeconvnode.typecheck_char_to_char,
          { dynarray_2_openarray} @ttypeconvnode.typecheck_dynarray_to_openarray,
          { pwchar_2_string} @ttypeconvnode.typecheck_pwchar_to_string,
          { variant_2_dynarray} @ttypeconvnode.typecheck_variant_to_dynarray,
          { dynarray_2_variant} @ttypeconvnode.typecheck_dynarray_to_variant,
          { variant_2_enum} @ttypeconvnode.typecheck_variant_to_enum,
          { enum_2_variant} @ttypeconvnode.typecheck_enum_to_variant,
          { variant_2_interface} @ttypeconvnode.typecheck_interface_to_variant,
          { interface_2_variant} @ttypeconvnode.typecheck_variant_to_interface,
          { array_2_dynarray} @ttypeconvnode.typecheck_array_2_dynarray
         );
      type
         tprocedureofobject = function : tnode of object;
      var
         r : packed record
                proc : pointer;
                obj : pointer;
             end;
      begin
         result:=nil;
         { this is a little bit dirty but it works }
         { and should be quite portable too        }
         r.proc:=resultdefconvert[c];
         r.obj:=self;
         if assigned(r.proc) then
          result:=tprocedureofobject(r)();
      end;


    function ttypeconvnode.actualtargetnode: tnode;
      begin
        result:=self;
        while (result.nodetype=typeconvn) and
              (nf_absolute in result.flags) and
              (resultdef.size=left.resultdef.size) do
          result:=ttypeconvnode(result).left;
      end;


    function ttypeconvnode.pass_typecheck:tnode;

      var
        hdef : tdef;
        hp : tnode;
        currprocdef : tabstractprocdef;
        aprocdef : tprocdef;
        eq : tequaltype;
        cdoptions : tcompare_defs_options;
        newblock: tblocknode;
        newstatement: tstatementnode;
        tempnode: ttempcreatenode;
      begin
        result:=nil;
        resultdef:=totypedef;

        typecheckpass(left);
        if codegenerror then
         exit;

        { When absolute force tc_equal }
        if (nf_absolute in flags) then
          begin
            convtype:=tc_equal;
            if not(tstoreddef(resultdef).is_intregable) and
               not(tstoreddef(resultdef).is_fpuregable) then
              make_not_regable(left,[ra_addr_regable]);
            exit;
          end;

        { tp procvar support. Skip typecasts to procvar, record or set. Those
          convert on the procvar value. This is used to access the
          fields of a methodpointer }
        if not(nf_load_procvar in flags) and
           not(resultdef.typ in [procvardef,recorddef,setdef]) then
          maybe_call_procvar(left,true);

        { convert array constructors to sets, because there is no conversion
          possible for array constructors }
        if (resultdef.typ<>arraydef) and
           is_array_constructor(left.resultdef) then
          begin
            arrayconstructor_to_set(left);
            typecheckpass(left);
          end;

        if convtype=tc_none then
          begin
            cdoptions:=[cdo_check_operator,cdo_allow_variant];
            if nf_explicit in flags then
              include(cdoptions,cdo_explicit);
            if nf_internal in flags then
              include(cdoptions,cdo_internal);
            eq:=compare_defs_ext(left.resultdef,resultdef,left.nodetype,convtype,aprocdef,cdoptions);
            case eq of
              te_exact,
              te_equal :
                begin
                  result := simplify;
                  if assigned(result) then
                    exit;

                  { Only leave when there is no conversion to do.
                    We can still need to call a conversion routine,
                    like the routine to convert a stringconstnode }
                  if convtype in [tc_equal,tc_not_possible] then
                   begin
                     left.resultdef:=resultdef;
                     if (nf_explicit in flags) and (left.nodetype = addrn) then
                       include(left.flags, nf_typedaddr);
                     result:=left;
                     left:=nil;
                     exit;
                   end;
                end;

              te_convert_l1,
              te_convert_l2,
              te_convert_l3,
              te_convert_l4,
              te_convert_l5:
                begin
                  result := simplify;
                  if assigned(result) then
                    exit;
                  { nothing to do }
                end;

              te_convert_operator :
                begin
                  include(current_procinfo.flags,pi_do_call);
                  aprocdef.procsym.IncRefCountBy(1);
                  hp:=ccallnode.create(ccallparanode.create(left,nil),Tprocsym(aprocdef.procsym),nil,nil,[]);
                  { tell explicitly which def we must use !! (PM) }
                  tcallnode(hp).procdefinition:=aprocdef;
                  left:=nil;
                  result:=hp;
                  exit;
                end;

              te_incompatible :
                begin
                  { Procedures have a resultdef of voiddef and functions of their
                    own resultdef. They will therefore always be incompatible with
                    a procvar. Because isconvertable cannot check for procedures we
                    use an extra check for them.}
                  if (left.nodetype=calln) and
                     (tcallnode(left).para_count=0) and
                     (resultdef.typ=procvardef) and
                     (
                      (m_tp_procvar in current_settings.modeswitches) or
                      (m_mac_procvar in current_settings.modeswitches)
                     ) then
                   begin
                     if assigned(tcallnode(left).right) then
                      begin
                        { this is already a procvar, if it is really equal
                          is checked below }
                        convtype:=tc_equal;
                        hp:=tcallnode(left).right.getcopy;
                        currprocdef:=tabstractprocdef(hp.resultdef);
                      end
                     else
                      begin
                        convtype:=tc_proc_2_procvar;
                        currprocdef:=Tprocsym(Tcallnode(left).symtableprocentry).Find_procdef_byprocvardef(Tprocvardef(resultdef));
                        hp:=cloadnode.create_procvar(tprocsym(tcallnode(left).symtableprocentry),
                            tprocdef(currprocdef),tcallnode(left).symtableproc);
                        if (tcallnode(left).symtableprocentry.owner.symtabletype=ObjectSymtable) then
                         begin
                           if assigned(tcallnode(left).methodpointer) then
                             tloadnode(hp).set_mp(tcallnode(left).methodpointer.getcopy)
                           else
                             tloadnode(hp).set_mp(load_self_node);
                         end;
                        typecheckpass(hp);
                      end;
                     left.free;
                     left:=hp;
                     { Now check if the procedure we are going to assign to
                       the procvar, is compatible with the procvar's type }
                     if not(nf_explicit in flags) and
                        (proc_to_procvar_equal(currprocdef,tprocvardef(resultdef))=te_incompatible) then
                       IncompatibleTypes(left.resultdef,resultdef);
                     exit;
                   end;

                  { Handle explicit type conversions }
                  if nf_explicit in flags then
                   begin
                     { do common tc_equal cast }
                     convtype:=tc_equal;

                     { ordinal constants can be resized to 1,2,4,8 bytes }
                     if (left.nodetype=ordconstn) then
                       begin
                         { Insert typeconv for ordinal to the correct size first on left, after
                           that the other conversion can be done }
                         hdef:=nil;
                         case longint(resultdef.size) of
                           1 :
                             hdef:=s8inttype;
                           2 :
                             hdef:=s16inttype;
                           4 :
                             hdef:=s32inttype;
                           8 :
                             hdef:=s64inttype;
                         end;
                         { we need explicit, because it can also be an enum }
                         if assigned(hdef) then
                           inserttypeconv_internal(left,hdef)
                         else
                           CGMessage2(type_e_illegal_type_conversion,left.resultdef.GetTypeName,resultdef.GetTypeName);
                       end;

                     { check if the result could be in a register }
                     if (not(tstoreddef(resultdef).is_intregable) and
                         not(tstoreddef(resultdef).is_fpuregable)) or
                        ((left.resultdef.typ = floatdef) and
                         (resultdef.typ <> floatdef))  then
                       make_not_regable(left,[ra_addr_regable]);

                     { class/interface to class/interface, with checkobject support }
                     if is_class_or_interface(resultdef) and
                        is_class_or_interface(left.resultdef) then
                       begin
                         { check if the types are related }
                         if not(nf_internal in flags) and
                            (not(tobjectdef(left.resultdef).is_related(tobjectdef(resultdef)))) and
                            (not(tobjectdef(resultdef).is_related(tobjectdef(left.resultdef)))) then
                           begin
                             { Give an error when typecasting class to interface, this is compatible
                               with delphi }
                             if is_interface(resultdef) and
                                not is_interface(left.resultdef) then
                               CGMessage2(type_e_classes_not_related,
                                 FullTypeName(left.resultdef,resultdef),
                                 FullTypeName(resultdef,left.resultdef))
                             else
                               CGMessage2(type_w_classes_not_related,
                                 FullTypeName(left.resultdef,resultdef),
                                 FullTypeName(resultdef,left.resultdef))
                           end;

                         { Add runtime check? }
                         if (cs_check_object in current_settings.localswitches) and
                            not(nf_internal in flags) then
                           begin
                             { we can translate the typeconvnode to 'as' when
                               typecasting to a class or interface }
                             { we need to make sure the result can still be
                               passed as a var parameter                    }
                             newblock:=internalstatements(newstatement);
                             if (valid_for_var(left,false)) then
                               begin
                                 tempnode:=ctempcreatenode.create(voidpointertype,voidpointertype.size,tt_persistent,true);
                                 addstatement(newstatement,tempnode);
                                 addstatement(newstatement,cassignmentnode.create(
                                   ctemprefnode.create(tempnode),
                                   caddrnode.create_internal(left)));
                                 left:=ctypeconvnode.create_internal(cderefnode.create(ctemprefnode.create(tempnode)),left.resultdef);
                               end
                             else
                               begin
                                 tempnode:=ctempcreatenode.create(left.resultdef,left.resultdef.size,tt_persistent,true);
                                 addstatement(newstatement,tempnode);
                                 addstatement(newstatement,cassignmentnode.create(
                                   ctemprefnode.create(tempnode),
                                   left));
                                 left:=ctemprefnode.create(tempnode);
                               end;
                             addstatement(newstatement,casnode.create(left.getcopy,cloadvmtaddrnode.create(ctypenode.create(resultdef))));
                             addstatement(newstatement,ctempdeletenode.create_normal_temp(tempnode));
                             addstatement(newstatement,ctypeconvnode.create_internal(left,resultdef));
                             left:=nil;
                             result:=newblock;
                             exit;
                           end;
                       end

                      else
                       begin
                         { only if the same size or formal def, and }
                         { don't allow type casting of constants to }
                         { structured types                         }
                         if not(
                                (left.resultdef.typ=formaldef) or
                                (
                                 not(is_open_array(left.resultdef)) and
                                 not(is_array_constructor(left.resultdef)) and
                                 (left.resultdef.size=resultdef.size) and
                                 { disallow casts of const nodes }
                                 (not is_constnode(left) or
                                   { however, there are some exceptions }
                                   (not(resultdef.typ in [arraydef,recorddef,setdef,stringdef,
                                                          filedef,variantdef,objectdef]) or
                                   is_class_or_interface(resultdef) or
                                   { the softfloat code generates casts <const. float> to record }
                                   (nf_internal in flags)
                                 ))
                                ) or
                                (
                                 is_void(left.resultdef)  and
                                 (left.nodetype=derefn)
                                )
                               ) then
                           CGMessage2(type_e_illegal_type_conversion,left.resultdef.GetTypeName,resultdef.GetTypeName);
                       end;
                   end
                  else
                   IncompatibleTypes(left.resultdef,resultdef);
                end;

              else
                internalerror(200211231);
            end;
          end;
        { Give hint or warning for unportable code, exceptions are
           - typecasts from constants
           - void }
        if not(nf_internal in flags) and
           (left.nodetype<>ordconstn) and
           not(is_void(left.resultdef)) and
           (((left.resultdef.typ=orddef) and
             (resultdef.typ in [pointerdef,procvardef,classrefdef])) or
            ((resultdef.typ=orddef) and
             (left.resultdef.typ in [pointerdef,procvardef,classrefdef]))) then
          begin
            {Converting pointers to signed integers is a bad idea. Warn.}
            warn_pointer_to_signed:=(resultdef.typ=orddef) and (Torddef(resultdef).ordtype in [s8bit,s16bit,s32bit,s64bit]);
            { Give a warning when sizes don't match, because then info will be lost }
            if left.resultdef.size=resultdef.size then
              CGMessage(type_h_pointer_to_longint_conv_not_portable)
            else
              CGMessage(type_w_pointer_to_longint_conv_not_portable);
          end;

        result := simplify;
        if assigned(result) then
          exit;

        { now call the resultdef helper to do constant folding }
        result:=typecheck_call_helper(convtype);
      end;


{$ifndef cpu64bitaddr}

    { checks whether we can safely remove 64 bit typeconversions }
    { in case range and overflow checking are off, and in case   }
    { the result of this node tree is downcasted again to a      }
    { 8/16/32 bit value afterwards                               }
    function checkremove64bittypeconvs(n: tnode): boolean;
      var
        gotmuldivmod, gotsint: boolean;

      { checks whether a node is either an u32bit, or originally }
      { was one but was implicitly converted to s64bit           }
      function wasoriginallyint32(n: tnode): boolean;
        begin
          if (n.resultdef.typ<>orddef) then
            exit(false);
          if (torddef(n.resultdef).ordtype in [s32bit,u32bit]) then
            begin
              if (torddef(n.resultdef).ordtype=s32bit) then
                gotsint:=true;
              exit(true);
            end;
          if (torddef(n.resultdef).ordtype=s64bit) and
             { nf_explicit is also set for explicitly typecasted }
             { ordconstn's                                       }
             ([nf_internal,nf_explicit]*n.flags=[]) and
             { either a typeconversion node coming from u32bit }
             (((n.nodetype=typeconvn) and
               (ttypeconvnode(n).left.resultdef.typ=orddef) and
               (torddef(ttypeconvnode(n).left.resultdef).ordtype in [s32bit,u32bit])) or
             { or an ordconstnode which was/is a valid cardinal }
              ((n.nodetype=ordconstn) and
               (tordconstnode(n).value>=low(longint)) and
               (tordconstnode(n).value<=high(cardinal)))) then
            begin
              if ((n.nodetype=typeconvn) and
                  (torddef(ttypeconvnode(n).left.resultdef).ordtype=s32bit)) or
                 ((n.nodetype=ordconstn) and
                  (tordconstnode(n).value<0)) then
                gotsint:=true;
              exit(true);
            end;
          result:=false;
        end;
  
  
      function docheckremove64bittypeconvs(n: tnode): boolean;
        begin
          result:=false;
          if wasoriginallyint32(n) then
            exit(true);
          case n.nodetype of
            subn,orn,xorn:
              begin
                { nf_internal is set by taddnode.typecheckpass in  }
                { case the arguments of this subn were u32bit, but }
                { upcasted to s64bit for calculation correctness   }
                { (normally only needed when range checking, but   }
                {  also done otherwise so there is no difference   }
                {  in overload choosing etc between $r+ and $r-)   }
                if (nf_internal in n.flags) then
                  result:=true
                else
                  result:=
                    docheckremove64bittypeconvs(tbinarynode(n).left) and
                    docheckremove64bittypeconvs(tbinarynode(n).right);
              end;
            addn,muln,divn,modn,andn:
              begin
                if n.nodetype in [muln,divn,modn] then
                  gotmuldivmod:=true;
                result:=
                  docheckremove64bittypeconvs(tbinarynode(n).left) and
                  docheckremove64bittypeconvs(tbinarynode(n).right);
              end;
          end;
        end;

      begin { checkremove64bittypeconvs }
        gotmuldivmod:=false;
        gotsint:=false;
        result:=
          docheckremove64bittypeconvs(n) and
          not(gotmuldivmod and gotsint);
      end;


    procedure doremove64bittypeconvs(var n: tnode; todef: tdef);
      begin
        case n.nodetype of
          subn,addn,muln,divn,modn,xorn,andn,orn:
            begin
              exclude(n.flags,nf_internal);
              if is_signed(n.resultdef) then
                begin
                  doremove64bittypeconvs(tbinarynode(n).left,s32inttype);
                  doremove64bittypeconvs(tbinarynode(n).right,s32inttype);
                  n.resultdef:=s32inttype
                end
              else
                begin
                  doremove64bittypeconvs(tbinarynode(n).left,u32inttype);
                  doremove64bittypeconvs(tbinarynode(n).right,u32inttype);
                  n.resultdef:=u32inttype
                end;
            end;
          ordconstn:
            inserttypeconv_internal(n,todef);
          typeconvn:
            n.resultdef:=todef;
        end;
      end;
{$endif not cpu64bitaddr}


    function ttypeconvnode.simplify: tnode;
      var
        hp: tnode;
      begin
        result := nil;
        { Constant folding and other node transitions to
          remove the typeconv node }
        case left.nodetype of
          realconstn :
            begin
              if (convtype = tc_real_2_currency) then
                result := typecheck_real_to_currency
              else if (convtype = tc_real_2_real) then
                result := typecheck_real_to_real
              else
                exit;
              if not(assigned(result)) then
                begin
                  result := left;
                  left := nil;
                end;
              if (result.nodetype = realconstn) then
                begin
                  hp:=result;
                  result:=crealconstnode.create(trealconstnode(hp).value_real,resultdef);
                  if ([nf_explicit,nf_internal] * flags <> []) then
                    include(result.flags, nf_explicit);
                  hp.free;
                end;
            end;
          niln :
            begin
              { nil to ordinal node }
              if (resultdef.typ=orddef) then
               begin
                 hp:=cordconstnode.create(0,resultdef,true);
                 if ([nf_explicit,nf_internal] * flags <> []) then
                   include(hp.flags, nf_explicit);
                 result:=hp;
                 exit;
               end
              else
               { fold nil to any pointer type }
               if (resultdef.typ=pointerdef) then
                begin
                  hp:=cnilnode.create;
                  hp.resultdef:=resultdef;
                  if ([nf_explicit,nf_internal] * flags <> []) then
                    include(hp.flags, nf_explicit);
                  result:=hp;
                  exit;
                end
              else
               { remove typeconv after niln, but not when the result is a
                 methodpointer. The typeconv of the methodpointer will then
                 take care of updateing size of niln to OS_64 }
               if not((resultdef.typ=procvardef) and
                      (po_methodpointer in tprocvardef(resultdef).procoptions)) then
                 begin
                   left.resultdef:=resultdef;
                   if ([nf_explicit,nf_internal] * flags <> []) then
                     include(left.flags, nf_explicit);
                   result:=left;
                   left:=nil;
                   exit;
                 end;
            end;

          ordconstn :
            begin
              { ordinal contants can be directly converted }
              { but not char to char because it is a widechar to char or via versa }
              { which needs extra code to do the code page transistion             }
              { constant ordinal to pointer }
              if (resultdef.typ=pointerdef) and
                 (convtype<>tc_cchar_2_pchar) then
                begin
                   hp:=cpointerconstnode.create(TConstPtrUInt(tordconstnode(left).value.uvalue),resultdef);
                   if ([nf_explicit,nf_internal] * flags <> []) then
                     include(hp.flags, nf_explicit);
                   result:=hp;
                   exit;
                end
              else if is_ordinal(resultdef) and
                      not(convtype=tc_char_2_char) then
                begin
                   { replace the resultdef and recheck the range }
                   if ([nf_explicit,nf_internal] * flags <> []) then
                     include(left.flags, nf_explicit)
                   else
                     { no longer an ordconst with an explicit typecast }
                     exclude(left.flags, nf_explicit);
                   { when converting from one boolean type to another, force }
                   { booleans to 0/1, and byte/word/long/qwordbool to 0/-1   }
                   { (Delphi-compatibile)                                    }
                   if is_boolean(left.resultdef) and
                      is_boolean(resultdef) and
                      (is_cbool(left.resultdef) or
                       is_cbool(resultdef)) then
                     begin
                       if is_pasbool(resultdef) then
                         tordconstnode(left).value:=ord(tordconstnode(left).value<>0)
                       else
{$ifdef VER2_2}
                         tordconstnode(left).value:=ord(tordconstnode(left).value<>0);
                         tordconstnode(left).value:=-tordconstnode(left).value;
{$else}
                         tordconstnode(left).value:=-ord(tordconstnode(left).value<>0);
{$endif VER2_2}
                     end
                   else
                     testrange(resultdef,tordconstnode(left).value,(nf_explicit in flags));
                   left.resultdef:=resultdef;
                   result:=left;
                   left:=nil;
                   exit;
                end;
            end;

          pointerconstn :
            begin
              { pointerconstn to any pointer is folded too }
              if (resultdef.typ=pointerdef) then
                begin
                   left.resultdef:=resultdef;
                   if ([nf_explicit,nf_internal] * flags <> []) then
                     include(left.flags, nf_explicit)
                   else
                     { no longer an ordconst with an explicit typecast }
                     exclude(left.flags, nf_explicit);
                   result:=left;
                   left:=nil;
                   exit;
                end
              { constant pointer to ordinal }
              else if is_ordinal(resultdef) then
                begin
                   hp:=cordconstnode.create(TConstExprInt(tpointerconstnode(left).value),
                     resultdef,not(nf_explicit in flags));
                   if ([nf_explicit,nf_internal] * flags <> []) then
                     include(hp.flags, nf_explicit);
                   result:=hp;
                   exit;
                end;
            end;
        end;

{$ifndef cpu64bitaddr}
        { must be done before code below, because we need the
          typeconversions for ordconstn's as well }
        case convtype of
          tc_int_2_int:
            begin
              if (localswitches * [cs_check_range,cs_check_overflow] = []) and
                 (resultdef.typ in [pointerdef,orddef,enumdef]) and
                 (resultdef.size <= 4) and
                 is_64bitint(left.resultdef) and
                 (left.nodetype in [subn,addn,muln,divn,modn,xorn,andn,orn]) and
                 checkremove64bittypeconvs(left) then
                begin
                  { avoid unnecessary widening of intermediary calculations }
                  { to 64 bit                                               }
                  doremove64bittypeconvs(left,generrordef);
                end;
            end;
        end;
{$endif not cpu64bitaddr}

      end;


    procedure Ttypeconvnode.mark_write;
      begin
        left.mark_write;
      end;

    function ttypeconvnode.first_cord_to_pointer : tnode;

      begin
        result:=nil;
        internalerror(200104043);
      end;


    function ttypeconvnode.first_int_to_int : tnode;

      begin
        first_int_to_int:=nil;
        expectloc:=left.expectloc;
        if not is_void(left.resultdef) then
          begin
            if (left.expectloc<>LOC_REGISTER) and
               (resultdef.size>left.resultdef.size) then
              expectloc:=LOC_REGISTER
            else
              if (left.expectloc=LOC_CREGISTER) and
                 (resultdef.size<left.resultdef.size) then
                expectloc:=LOC_REGISTER;
          end;
      end;


    function ttypeconvnode.first_cstring_to_pchar : tnode;

      begin
         result:=nil;
         expectloc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_cstring_to_int : tnode;

      begin
        result:=nil;
        internalerror(200510014);
      end;


    function ttypeconvnode.first_string_to_chararray : tnode;

      begin
         first_string_to_chararray:=nil;
         expectloc:=left.expectloc;
      end;


    function ttypeconvnode.first_char_to_string : tnode;

      begin
         first_char_to_string:=nil;
         expectloc:=LOC_REFERENCE;
      end;


    function ttypeconvnode.first_nothing : tnode;
      begin
         first_nothing:=nil;
      end;


    function ttypeconvnode.first_array_to_pointer : tnode;

      begin
         first_array_to_pointer:=nil;
         expectloc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_int_to_real: tnode;
      var
        fname: string[32];
      begin
        if target_info.system in system_wince then
          begin
            { converting a 64bit integer to a float requires a helper }
            if is_64bitint(left.resultdef) or
              is_currency(left.resultdef) then
              begin
                { hack to avoid double division by 10000, as it's
                  already done by typecheckpass.resultdef_int_to_real }
                if is_currency(left.resultdef) then
                  left.resultdef := s64inttype;
                if is_signed(left.resultdef) then
                  fname:='I64TO'
                else
                  fname:='UI64TO';
              end
            else
              { other integers are supposed to be 32 bit }
              begin
                if is_signed(left.resultdef) then
                  fname:='ITO'
                else
                  fname:='UTO';
                firstpass(left);
              end;
            if tfloatdef(resultdef).floattype=s64real then
              fname:=fname+'D'
            else
              fname:=fname+'S';
            result:=ccallnode.createintern(fname,ccallparanode.create(
              left,nil));
            left:=nil;
            firstpass(result);
            exit;
          end
        else
          begin
            { converting a 64bit integer to a float requires a helper }
            if is_64bitint(left.resultdef) or
              is_currency(left.resultdef) then
              begin
                { hack to avoid double division by 10000, as it's
                  already done by typecheckpass.resultdef_int_to_real }
                if is_currency(left.resultdef) then
                  left.resultdef := s64inttype;
                if is_signed(left.resultdef) then
                  fname:='int64_to_'
                else
                  { we can't do better currently }
                  fname:='int64_to_';
              end
            else
              { other integers are supposed to be 32 bit }
              begin
                if is_signed(left.resultdef) then
                  fname:='int32_to_'
                else
                  { we can't do better currently }
                  fname:='int32_to_';
                firstpass(left);
              end;
            if tfloatdef(resultdef).floattype=s64real then
              fname:=fname+'float64'
            else
              fname:=fname+'float32';
            result:=ctypeconvnode.create_internal(ccallnode.createintern(fname,ccallparanode.create(
              left,nil)),resultdef);
            left:=nil;
            firstpass(result);
            exit;
          end;
      end;


    function ttypeconvnode.first_real_to_real : tnode;
      begin
{$ifdef cpufpemu}
        if cs_fp_emulation in current_settings.moduleswitches then
          begin
            if target_info.system in system_wince then
              begin
                case tfloatdef(left.resultdef).floattype of
                  s32real:
                    case tfloatdef(resultdef).floattype of
                      s64real:
                        result:=ccallnode.createintern('STOD',ccallparanode.create(left,nil));
                      s32real:
                        begin
                          result:=left;
                          left:=nil;
                        end;
                      else
                        internalerror(2005082704);
                    end;
                  s64real:
                    case tfloatdef(resultdef).floattype of
                      s32real:
                        result:=ccallnode.createintern('DTOS',ccallparanode.create(left,nil));
                      s64real:
                        begin
                          result:=left;
                          left:=nil;
                        end;
                      else
                        internalerror(2005082703);
                    end;
                  else
                    internalerror(2005082702);
                end;
                left:=nil;
                firstpass(result);
                exit;
              end
            else
              begin
                case tfloatdef(left.resultdef).floattype of
                  s32real:
                    case tfloatdef(resultdef).floattype of
                      s64real:
                        result:=ctypeconvnode.create_explicit(ccallnode.createintern('float32_to_float64',ccallparanode.create(
                          ctypeconvnode.create_internal(left,search_system_type('FLOAT32REC').typedef),nil)),resultdef);
                      s32real:
                        begin
                          result:=left;
                          left:=nil;
                        end;
                      else
                        internalerror(200610151);
                    end;
                  s64real:
                    case tfloatdef(resultdef).floattype of
                      s32real:
                        result:=ctypeconvnode.create_explicit(ccallnode.createintern('float64_to_float32',ccallparanode.create(
                          ctypeconvnode.create_internal(left,search_system_type('FLOAT64').typedef),nil)),resultdef);
                      s64real:
                        begin
                          result:=left;
                          left:=nil;
                        end;
                      else
                        internalerror(200610152);
                    end;
                  else
                    internalerror(200610153);
                end;
                left:=nil;
                firstpass(result);
                exit;
              end;
          end
        else
{$endif cpufpemu}
          begin
            first_real_to_real:=nil;
            expectloc:=LOC_FPUREGISTER;
          end;
      end;


    function ttypeconvnode.first_pointer_to_array : tnode;

      begin
         first_pointer_to_array:=nil;
         expectloc:=LOC_REFERENCE;
      end;


    function ttypeconvnode.first_cchar_to_pchar : tnode;

      begin
         first_cchar_to_pchar:=nil;
         internalerror(200104021);
      end;


    function ttypeconvnode.first_bool_to_int : tnode;

      begin
         first_bool_to_int:=nil;
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (nf_explicit in flags) and
            (left.resultdef.size=resultdef.size) and
            (left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           exit;
         { when converting to 64bit, first convert to a 32bit int and then   }
         { convert to a 64bit int (only necessary for 32bit processors) (JM) }
         if resultdef.size > sizeof(aint) then
           begin
             result := ctypeconvnode.create_internal(left,s32inttype);
             result := ctypeconvnode.create(result,resultdef);
             left := nil;
             firstpass(result);
             exit;
           end;
         expectloc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_int_to_bool : tnode;

      begin
         first_int_to_bool:=nil;
         { byte(boolean) or word(wordbool) or longint(longbool) must
           be accepted for var parameters }
         if (nf_explicit in flags) and
            (left.resultdef.size=resultdef.size) and
            (left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           exit;
         expectloc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_bool_to_bool : tnode;
      begin
         first_bool_to_bool:=nil;
         if (left.expectloc in [LOC_FLAGS,LOC_JUMP]) then
           expectloc := left.expectloc
         else
           expectloc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_char_to_char : tnode;
      var
        fname: string[18];
      begin
        if (torddef(resultdef).ordtype=uchar) and
           (torddef(left.resultdef).ordtype=uwidechar) then
          fname := 'fpc_wchar_to_char'
        else if (torddef(resultdef).ordtype=uwidechar) and
           (torddef(left.resultdef).ordtype=uchar) then
          fname := 'fpc_char_to_wchar'
        else
          internalerror(2007081201);

        result := ccallnode.createintern(fname,ccallparanode.create(left,nil));
        left:=nil;
        firstpass(result);
      end;


    function ttypeconvnode.first_proc_to_procvar : tnode;
      begin
         first_proc_to_procvar:=nil;
         { if we take the address of a nested function, it'll  }
         { probably be used in a foreach() construct and then  }
         { the parent needs a stackframe                       }
         if (tprocdef(left.resultdef).parast.symtablelevel>=normal_function_level) then
           include(current_procinfo.flags,pi_needs_stackframe);
         if tabstractprocdef(resultdef).is_addressonly then
           expectloc:=LOC_REGISTER
         else
           begin
             if not(left.expectloc in [LOC_CREFERENCE,LOC_REFERENCE]) then
               CGMessage(parser_e_illegal_expression);
             expectloc:=left.expectloc;
           end;
      end;


    function ttypeconvnode.first_nil_to_methodprocvar : tnode;
      begin
        first_nil_to_methodprocvar:=nil;
        expectloc:=LOC_REFERENCE;
      end;


    function ttypeconvnode.first_set_to_set : tnode;
      var
        newstatement : tstatementnode;
        temp         : ttempcreatenode;
      begin
        { in theory, we should do range checking here,
          but Delphi doesn't do it either (FK) }

        if left.nodetype=setconstn then
          begin
            left.resultdef:=resultdef;
            result:=left;
          end
        { equal sets for the code generator? }
        else if (left.resultdef.size=resultdef.size) and
          (tsetdef(left.resultdef).setbase=tsetdef(resultdef).setbase) then
          {$warning This causes wrong (but Delphi-compatible) results for disjoint subsets}
          { e.g., this prints true because of this:
              var
                sa: set of 1..2;
                sb: set of 5..6;
                b: byte;
              begin
                b:=1;
                sa:=[1..2];
                sb:=sa;
                writeln(b in sb);
              end.
          }
          result:=left
        else
        // if is_varset(resultdef) then
          begin
            result:=internalstatements(newstatement);

            { in case left is a smallset expression, it can be an addn or so. }
            { fpc_varset_load expects a formal const parameter, which doesn't }
            { accept set addn's -> assign to a temp first and pass the temp   }
            if not(left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE]) then
              begin
                temp:=ctempcreatenode.create(left.resultdef,left.resultdef.size,tt_persistent,false);
                addstatement(newstatement,temp);
                { temp := left }
                addstatement(newstatement,cassignmentnode.create(
                  ctemprefnode.create(temp),left));
                addstatement(newstatement,ctempdeletenode.create_normal_temp(temp));
                addstatement(newstatement,ctemprefnode.create(temp));
                left:=result;
                firstpass(left);
                { recreate the result's internalstatements list }
                result:=internalstatements(newstatement);
              end;

            { create temp for result }
            temp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,true);
            addstatement(newstatement,temp);

            addstatement(newstatement,ccallnode.createintern('fpc_varset_load',
              ccallparanode.create(cordconstnode.create(tsetdef(left.resultdef).setbase div 8 - tsetdef(resultdef).setbase div 8,sinttype,false),
              ccallparanode.create(cordconstnode.create(resultdef.size,sinttype,false),
              ccallparanode.create(ctemprefnode.create(temp),
              ccallparanode.create(cordconstnode.create(left.resultdef.size,sinttype,false),
              ccallparanode.create(left,nil))))))
            );
            addstatement(newstatement,ctempdeletenode.create_normal_temp(temp));
            addstatement(newstatement,ctemprefnode.create(temp));
          end;
        {
        else
          begin
            srsym:=search_system_type('FPC_SMALL_SET');
            result :=
              ccallnode.createinternres('fpc_set_load_small',
                ccallparanode.create(ctypeconvnode.create_internal(left,srsym.typedef),nil),resultdef);
          end;
        }
        { reused }
        left:=nil;
      end;


    function ttypeconvnode.first_ansistring_to_pchar : tnode;
      begin
         first_ansistring_to_pchar:=nil;
         expectloc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_arrayconstructor_to_set : tnode;
      begin
        first_arrayconstructor_to_set:=nil;
        internalerror(200104022);
      end;


    function ttypeconvnode.first_class_to_intf : tnode;
      var
        hd : tobjectdef;
        ImplIntf : TImplementedInterface;
      begin
         result:=nil;
         expectloc:=LOC_REGISTER;
         hd:=tobjectdef(left.resultdef);
         while assigned(hd) do
           begin
             ImplIntf:=hd.find_implemented_interface(tobjectdef(resultdef));
             if assigned(ImplIntf) then
               begin
                 case ImplIntf.IType of
                   etStandard:
                     { handle in pass 2 }
                     ;
                   etFieldValue:
                     if is_interface(tobjectdef(resultdef)) then
                       begin
                         result:=left;
                         propaccesslist_to_node(result,tpropertysym(implintf.implementsgetter).owner,tpropertysym(implintf.implementsgetter).propaccesslist[palt_read]);
                         left:=nil;
                       end
                     else
                       begin
                         internalerror(200802213);
                       end;
                   etStaticMethodResult,
                   etVirtualMethodResult:
                     if is_interface(tobjectdef(resultdef)) then
                       begin
                         { constructor create(l:tnode; v : tprocsym;st : TSymtable; mp: tnode; callflags:tcallnodeflags); }
                         result:=ccallnode.create(nil,tprocsym(tpropertysym(implintf.implementsgetter).propaccesslist[palt_read].firstsym^.sym),
                           tprocsym(tpropertysym(implintf.implementsgetter).propaccesslist[palt_read].firstsym^.sym).owner,
                           left,[]);
                         addsymref(tpropertysym(implintf.implementsgetter).propaccesslist[palt_read].firstsym^.sym);
                         left:=nil;
                       end
                     else if is_class(tobjectdef(resultdef)) then
                       begin
                         internalerror(200802211);
                       end
                     else
                       internalerror(200802231);
                   else
                     internalerror(200802165);
                 end;
                 break;
               end;
             hd:=hd.childof;
           end;
         if hd=nil then
           internalerror(200802164);
      end;


    function ttypeconvnode._first_int_to_int : tnode;
      begin
         result:=first_int_to_int;
      end;

    function ttypeconvnode._first_cstring_to_pchar : tnode;
      begin
         result:=first_cstring_to_pchar;
      end;

    function ttypeconvnode._first_cstring_to_int : tnode;
      begin
         result:=first_cstring_to_int;
      end;

    function ttypeconvnode._first_string_to_chararray : tnode;
      begin
         result:=first_string_to_chararray;
      end;

    function ttypeconvnode._first_char_to_string : tnode;
      begin
         result:=first_char_to_string;
      end;

    function ttypeconvnode._first_nothing : tnode;
      begin
         result:=first_nothing;
      end;

    function ttypeconvnode._first_array_to_pointer : tnode;
      begin
         result:=first_array_to_pointer;
      end;

    function ttypeconvnode._first_int_to_real : tnode;
      begin
         result:=first_int_to_real;
      end;

    function ttypeconvnode._first_real_to_real : tnode;
      begin
         result:=first_real_to_real;
      end;

    function ttypeconvnode._first_pointer_to_array : tnode;
      begin
         result:=first_pointer_to_array;
      end;

    function ttypeconvnode._first_cchar_to_pchar : tnode;
      begin
         result:=first_cchar_to_pchar;
      end;

    function ttypeconvnode._first_bool_to_int : tnode;
      begin
         result:=first_bool_to_int;
      end;

    function ttypeconvnode._first_int_to_bool : tnode;
      begin
         result:=first_int_to_bool;
      end;

    function ttypeconvnode._first_bool_to_bool : tnode;
      begin
         result:=first_bool_to_bool;
      end;

    function ttypeconvnode._first_proc_to_procvar : tnode;
      begin
         result:=first_proc_to_procvar;
      end;

    function ttypeconvnode._first_nil_to_methodprocvar : tnode;
      begin
         result:=first_nil_to_methodprocvar;
      end;

    function ttypeconvnode._first_set_to_set : tnode;
      begin
         result:=first_set_to_set;
      end;

    function ttypeconvnode._first_cord_to_pointer : tnode;
      begin
         result:=first_cord_to_pointer;
      end;

    function ttypeconvnode._first_ansistring_to_pchar : tnode;
      begin
         result:=first_ansistring_to_pchar;
      end;

    function ttypeconvnode._first_arrayconstructor_to_set : tnode;
      begin
         result:=first_arrayconstructor_to_set;
      end;

    function ttypeconvnode._first_class_to_intf : tnode;
      begin
         result:=first_class_to_intf;
      end;

    function ttypeconvnode._first_char_to_char : tnode;
      begin
         result:=first_char_to_char;
      end;

    function ttypeconvnode.first_call_helper(c : tconverttype) : tnode;

      const
         firstconvert : array[tconverttype] of pointer = (
           nil, { none }
           @ttypeconvnode._first_nothing, {equal}
           @ttypeconvnode._first_nothing, {not_possible}
           nil, { removed in typecheck_string_to_string }
           @ttypeconvnode._first_char_to_string,
           @ttypeconvnode._first_nothing, { char_2_chararray, needs nothing extra }
           nil, { removed in typecheck_chararray_to_string }
           @ttypeconvnode._first_cchar_to_pchar,
           @ttypeconvnode._first_cstring_to_pchar,
           @ttypeconvnode._first_cstring_to_int,
           @ttypeconvnode._first_ansistring_to_pchar,
           @ttypeconvnode._first_string_to_chararray,
           nil, { removed in typecheck_chararray_to_string }
           @ttypeconvnode._first_array_to_pointer,
           @ttypeconvnode._first_pointer_to_array,
           @ttypeconvnode._first_int_to_int,
           @ttypeconvnode._first_int_to_bool,
           @ttypeconvnode._first_bool_to_bool,
           @ttypeconvnode._first_bool_to_int,
           @ttypeconvnode._first_real_to_real,
           @ttypeconvnode._first_int_to_real,
           nil, { removed in typecheck_real_to_currency }
           @ttypeconvnode._first_proc_to_procvar,
           @ttypeconvnode._first_nil_to_methodprocvar,
           @ttypeconvnode._first_arrayconstructor_to_set,
           @ttypeconvnode._first_set_to_set,
           @ttypeconvnode._first_cord_to_pointer,
           @ttypeconvnode._first_nothing,
           @ttypeconvnode._first_nothing,
           @ttypeconvnode._first_class_to_intf,
           @ttypeconvnode._first_char_to_char,
           @ttypeconvnode._first_nothing,
           @ttypeconvnode._first_nothing,
           nil,
           nil,
           nil,
           nil,
           nil,
           nil,
           nil
         );
      type
         tprocedureofobject = function : tnode of object;
      var
         r : packed record
                proc : pointer;
                obj : pointer;
             end;
      begin
         { this is a little bit dirty but it works }
         { and should be quite portable too        }
         r.proc:=firstconvert[c];
         r.obj:=self;
         if not assigned(r.proc) then
           internalerror(200312081);
         first_call_helper:=tprocedureofobject(r)()
      end;


    function ttypeconvnode.pass_1 : tnode;
      begin
        if warn_pointer_to_signed then
          cgmessage(type_w_pointer_to_signed);
        result:=nil;
        firstpass(left);
        if codegenerror then
         exit;
        expectloc:=left.expectloc;
        result:=first_call_helper(convtype);
      end;


    function ttypeconvnode.assign_allowed:boolean;
      begin
        result:=(convtype=tc_equal) or
                { typecasting from void is always allowed }
                is_void(left.resultdef) or
                (left.resultdef.typ=formaldef) or
                { int 2 int with same size reuses same location, or for
                  tp7 mode also allow size < orignal size }
                (
                 (convtype=tc_int_2_int) and
                 (
                  (resultdef.size=left.resultdef.size) or
                  ((m_tp7 in current_settings.modeswitches) and
                   (resultdef.size<left.resultdef.size))
                 )
                ) or
                { int 2 bool/bool 2 int, explicit typecast, see also nx86cnv }
                ((convtype in [tc_int_2_bool,tc_bool_2_int]) and
                 (nf_explicit in flags) and
                 (resultdef.size=left.resultdef.size));

        { When using only a part of the value it can't be in a register since
          that will load the value in a new register first }
        if (resultdef.size<left.resultdef.size) then
          make_not_regable(left,[ra_addr_regable]);
      end;


    function ttypeconvnode.docompare(p: tnode) : boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (convtype = ttypeconvnode(p).convtype);
      end;


    procedure ttypeconvnode._second_int_to_int;
      begin
        second_int_to_int;
      end;


    procedure ttypeconvnode._second_string_to_string;
      begin
        second_string_to_string;
      end;


    procedure ttypeconvnode._second_cstring_to_pchar;
      begin
        second_cstring_to_pchar;
      end;


    procedure ttypeconvnode._second_cstring_to_int;
      begin
        second_cstring_to_int;
      end;


    procedure ttypeconvnode._second_string_to_chararray;
      begin
        second_string_to_chararray;
      end;


    procedure ttypeconvnode._second_array_to_pointer;
      begin
        second_array_to_pointer;
      end;


    procedure ttypeconvnode._second_pointer_to_array;
      begin
        second_pointer_to_array;
      end;


    procedure ttypeconvnode._second_chararray_to_string;
      begin
        second_chararray_to_string;
      end;


    procedure ttypeconvnode._second_char_to_string;
      begin
        second_char_to_string;
      end;


    procedure ttypeconvnode._second_int_to_real;
      begin
        second_int_to_real;
      end;


    procedure ttypeconvnode._second_real_to_real;
      begin
        second_real_to_real;
      end;


    procedure ttypeconvnode._second_cord_to_pointer;
      begin
        second_cord_to_pointer;
      end;


    procedure ttypeconvnode._second_proc_to_procvar;
      begin
        second_proc_to_procvar;
      end;

    procedure ttypeconvnode._second_nil_to_methodprocvar;
      begin
        second_nil_to_methodprocvar;
      end;

    procedure ttypeconvnode._second_bool_to_int;
      begin
        second_bool_to_int;
      end;


    procedure ttypeconvnode._second_int_to_bool;
      begin
        second_int_to_bool;
      end;


    procedure ttypeconvnode._second_bool_to_bool;
      begin
        second_bool_to_bool;
      end;


    procedure ttypeconvnode._second_set_to_set;
      begin
        second_set_to_set;
      end;


    procedure ttypeconvnode._second_ansistring_to_pchar;
      begin
        second_ansistring_to_pchar;
      end;


    procedure ttypeconvnode._second_class_to_intf;
      begin
        second_class_to_intf;
      end;


    procedure ttypeconvnode._second_char_to_char;
      begin
        second_char_to_char;
      end;


    procedure ttypeconvnode._second_nothing;
      begin
        second_nothing;
      end;


    procedure ttypeconvnode.second_call_helper(c : tconverttype);
      const
         secondconvert : array[tconverttype] of pointer = (
           @ttypeconvnode._second_nothing, {none}
           @ttypeconvnode._second_nothing, {equal}
           @ttypeconvnode._second_nothing, {not_possible}
           @ttypeconvnode._second_nothing, {second_string_to_string, handled in resultdef pass }
           @ttypeconvnode._second_char_to_string,
           @ttypeconvnode._second_nothing, {char_to_charray}
           @ttypeconvnode._second_nothing, { pchar_to_string, handled in resultdef pass }
           @ttypeconvnode._second_nothing, {cchar_to_pchar}
           @ttypeconvnode._second_cstring_to_pchar,
           @ttypeconvnode._second_cstring_to_int,
           @ttypeconvnode._second_ansistring_to_pchar,
           @ttypeconvnode._second_string_to_chararray,
           @ttypeconvnode._second_nothing, { chararray_to_string, handled in resultdef pass }
           @ttypeconvnode._second_array_to_pointer,
           @ttypeconvnode._second_pointer_to_array,
           @ttypeconvnode._second_int_to_int,
           @ttypeconvnode._second_int_to_bool,
           @ttypeconvnode._second_bool_to_bool,
           @ttypeconvnode._second_bool_to_int,
           @ttypeconvnode._second_real_to_real,
           @ttypeconvnode._second_int_to_real,
           @ttypeconvnode._second_nothing, { real_to_currency, handled in resultdef pass }
           @ttypeconvnode._second_proc_to_procvar,
           @ttypeconvnode._second_nil_to_methodprocvar,
           @ttypeconvnode._second_nothing, { arrayconstructor_to_set }
           @ttypeconvnode._second_nothing, { second_set_to_set, handled in first pass }
           @ttypeconvnode._second_cord_to_pointer,
           @ttypeconvnode._second_nothing, { interface 2 string }
           @ttypeconvnode._second_nothing, { interface 2 guid   }
           @ttypeconvnode._second_class_to_intf,
           @ttypeconvnode._second_char_to_char,
           @ttypeconvnode._second_nothing,  { dynarray_2_openarray }
           @ttypeconvnode._second_nothing,  { pwchar_2_string }
           @ttypeconvnode._second_nothing,  { variant_2_dynarray }
           @ttypeconvnode._second_nothing,  { dynarray_2_variant}
           @ttypeconvnode._second_nothing,  { variant_2_enum }
           @ttypeconvnode._second_nothing,  { enum_2_variant }
           @ttypeconvnode._second_nothing,  { variant_2_interface }
           @ttypeconvnode._second_nothing,  { interface_2_variant }
           @ttypeconvnode._second_nothing   { array_2_dynarray }
         );
      type
         tprocedureofobject = procedure of object;

      var
         r : packed record
                proc : pointer;
                obj : pointer;
             end;

      begin
         { this is a little bit dirty but it works }
         { and should be quite portable too        }
         r.proc:=secondconvert[c];
         r.obj:=self;
         tprocedureofobject(r)();
      end;


{*****************************************************************************
                                TISNODE
*****************************************************************************}

    constructor tisnode.create(l,r : tnode);

      begin
         inherited create(isn,l,r);
      end;


    function tisnode.pass_typecheck:tnode;
      var
        paras: tcallparanode;
      begin
         result:=nil;
         typecheckpass(left);
         typecheckpass(right);

         set_varstate(left,vs_read,[vsf_must_be_valid]);
         set_varstate(right,vs_read,[vsf_must_be_valid]);

         if codegenerror then
           exit;

         if (right.resultdef.typ=classrefdef) then
          begin
            { left must be a class }
            if is_class(left.resultdef) then
             begin
               { the operands must be related }
               if (not(tobjectdef(left.resultdef).is_related(
                  tobjectdef(tclassrefdef(right.resultdef).pointeddef)))) and
                  (not(tobjectdef(tclassrefdef(right.resultdef).pointeddef).is_related(
                  tobjectdef(left.resultdef)))) then
                 CGMessage2(type_e_classes_not_related,left.resultdef.typename,
                            tclassrefdef(right.resultdef).pointeddef.typename);
             end
            else
             CGMessage1(type_e_class_type_expected,left.resultdef.typename);

            { call fpc_do_is helper }
            paras := ccallparanode.create(
                         left,
                     ccallparanode.create(
                         right,nil));
            result := ccallnode.createintern('fpc_do_is',paras);
            left := nil;
            right := nil;
          end
         else if is_interface(right.resultdef) then
          begin
            { left is a class }
            if is_class(left.resultdef) then
             begin
               { the class must implement the interface }
               if tobjectdef(left.resultdef).find_implemented_interface(tobjectdef(right.resultdef))=nil then
                 CGMessage2(type_e_classes_not_related,
                    FullTypeName(left.resultdef,right.resultdef),
                    FullTypeName(right.resultdef,left.resultdef))
             end
            { left is an interface }
            else if is_interface(left.resultdef) then
             begin
               { the operands must be related }
               if (not(tobjectdef(left.resultdef).is_related(tobjectdef(right.resultdef)))) and
                  (not(tobjectdef(right.resultdef).is_related(tobjectdef(left.resultdef)))) then
                 CGMessage2(type_e_classes_not_related,
                    FullTypeName(left.resultdef,right.resultdef),
                    FullTypeName(right.resultdef,left.resultdef));
             end
            else
             CGMessage1(type_e_class_type_expected,left.resultdef.typename);
            { call fpc_do_is helper }
            paras := ccallparanode.create(
                         left,
                     ccallparanode.create(
                         right,nil));
            result := ccallnode.createintern('fpc_do_is',paras);
            left := nil;
            right := nil;
          end
         else
          CGMessage1(type_e_class_or_interface_type_expected,right.resultdef.typename);

         resultdef:=booltype;
      end;


    function tisnode.pass_1 : tnode;
      begin
        internalerror(200204254);
        result:=nil;
      end;

    { dummy pass_2, it will never be called, but we need one since }
    { you can't instantiate an abstract class                      }
    procedure tisnode.pass_generate_code;
      begin
      end;


{*****************************************************************************
                                TASNODE
*****************************************************************************}

    constructor tasnode.create(l,r : tnode);

      begin
         inherited create(asn,l,r);
         call := nil;
      end;


    destructor tasnode.destroy;

      begin
        call.free;
        inherited destroy;
      end;


    function tasnode.pass_typecheck:tnode;
      var
        hp : tnode;
      begin
         result:=nil;
         typecheckpass(right);
         typecheckpass(left);

         set_varstate(right,vs_read,[vsf_must_be_valid]);
         set_varstate(left,vs_read,[vsf_must_be_valid]);

         if codegenerror then
           exit;

         if (right.resultdef.typ=classrefdef) then
          begin
            { left must be a class }
            if is_class(left.resultdef) then
             begin
               { the operands must be related }
               if (not(tobjectdef(left.resultdef).is_related(
                  tobjectdef(tclassrefdef(right.resultdef).pointeddef)))) and
                  (not(tobjectdef(tclassrefdef(right.resultdef).pointeddef).is_related(
                  tobjectdef(left.resultdef)))) then
                 CGMessage2(type_e_classes_not_related,
                    FullTypeName(left.resultdef,tclassrefdef(right.resultdef).pointeddef),
                    FullTypeName(tclassrefdef(right.resultdef).pointeddef,left.resultdef));
             end
            else
             CGMessage1(type_e_class_type_expected,left.resultdef.typename);
            resultdef:=tclassrefdef(right.resultdef).pointeddef;
          end
         else if is_interface(right.resultdef) then
          begin
            { left is a class }
            if not(is_class(left.resultdef) or
                   is_interfacecom(left.resultdef)) then
              CGMessage1(type_e_class_or_cominterface_type_expected,left.resultdef.typename);

            resultdef:=right.resultdef;

            { load the GUID of the interface }
            if (right.nodetype=typen) then
             begin
               if assigned(tobjectdef(right.resultdef).iidguid) then
                 begin
                   if not(oo_has_valid_guid in tobjectdef(right.resultdef).objectoptions) then
                     CGMessage1(type_interface_has_no_guid,tobjectdef(right.resultdef).typename);
                   hp:=cguidconstnode.create(tobjectdef(right.resultdef).iidguid^);
                   right.free;
                   right:=hp;
                 end
               else
                 internalerror(200206282);
               typecheckpass(right);
             end;
          end
         else
          CGMessage1(type_e_class_or_interface_type_expected,right.resultdef.typename);
      end;


    function tasnode.dogetcopy: tnode;
      begin
        result := inherited dogetcopy;
        if assigned(call) then
          tasnode(result).call := call.getcopy
        else
          tasnode(result).call := nil;
      end;


    function tasnode.pass_1 : tnode;
      var
        procname: string;
      begin
        result:=nil;
        if not assigned(call) then
          begin
            if is_class(left.resultdef) and
               (right.resultdef.typ=classrefdef) then
              call := ccallnode.createinternres('fpc_do_as',
                ccallparanode.create(left,ccallparanode.create(right,nil)),
                resultdef)
            else
              begin
                if is_class(left.resultdef) then
                  procname := 'fpc_class_as_intf'
                else
                  procname := 'fpc_intf_as';
                call := ccallnode.createintern(procname,
                   ccallparanode.create(right,ccallparanode.create(left,nil)));
                call := ctypeconvnode.create_internal(call,resultdef);
              end;
            left := nil;
            right := nil;
            firstpass(call);
            if codegenerror then
              exit;
           expectloc:=call.expectloc;
         end;
      end;


begin
   ctypeconvnode:=ttypeconvnode;
   casnode:=tasnode;
   cisnode:=tisnode;
end.

{
    $Id$
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
       symtype,symppu,defbase,
       nld;

    type
       ttypeconvnode = class(tunarynode)
          totype   : ttype;
          convtype : tconverttype;
          constructor create(node : tnode;const t : ttype);virtual;
          constructor create_explicit(node : tnode;const t : ttype);
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       {$ifdef var_notification}
          procedure mark_write;override;
       {$endif}
          function docompare(p: tnode) : boolean; override;
       private
          function resulttype_cord_to_pointer : tnode;
          function resulttype_chararray_to_string : tnode;
          function resulttype_string_to_chararray : tnode;
          function resulttype_string_to_string : tnode;
          function resulttype_char_to_string : tnode;
          function resulttype_char_to_chararray : tnode;
          function resulttype_int_to_real : tnode;
          function resulttype_real_to_real : tnode;
          function resulttype_cchar_to_pchar : tnode;
          function resulttype_cstring_to_pchar : tnode;
          function resulttype_char_to_char : tnode;
          function resulttype_arrayconstructor_to_set : tnode;
          function resulttype_pchar_to_string : tnode;
          function resulttype_interface_to_guid : tnode;
          function resulttype_dynarray_to_openarray : tnode;
          function resulttype_call_helper(c : tconverttype) : tnode;
       protected
          function first_int_to_int : tnode;virtual;
          function first_cstring_to_pchar : tnode;virtual;
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
          function first_load_smallset : tnode;virtual;
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
          function _first_string_to_chararray : tnode;
          function _first_char_to_string : tnode;
          function _first_nothing : tnode;
          function _first_array_to_pointer : tnode;
          function _first_int_to_real : tnode;
          function _first_real_to_real : tnode;
          function _first_pointer_to_array : tnode;
          function _first_cchar_to_pchar : tnode;
          function _first_bool_to_int : tnode;
          function _first_int_to_bool : tnode;
          function _first_bool_to_bool : tnode;
          function _first_proc_to_procvar : tnode;
          function _first_load_smallset : tnode;
          function _first_cord_to_pointer : tnode;
          function _first_ansistring_to_pchar : tnode;
          function _first_arrayconstructor_to_set : tnode;
          function _first_class_to_intf : tnode;
          function _first_char_to_char : tnode;

          procedure second_int_to_int;virtual;abstract;
          procedure second_string_to_string;virtual;abstract;
          procedure second_cstring_to_pchar;virtual;abstract;
          procedure second_string_to_chararray;virtual;abstract;
          procedure second_array_to_pointer;virtual;abstract;
          procedure second_pointer_to_array;virtual;abstract;
          procedure second_chararray_to_string;virtual;abstract;
          procedure second_char_to_string;virtual;abstract;
          procedure second_int_to_real;virtual;abstract;
          procedure second_real_to_real;virtual;abstract;
          procedure second_cord_to_pointer;virtual;abstract;
          procedure second_proc_to_procvar;virtual;abstract;
          procedure second_bool_to_int;virtual;abstract;
          procedure second_int_to_bool;virtual;abstract;
          procedure second_bool_to_bool;virtual;abstract;
          procedure second_load_smallset;virtual;abstract;
          procedure second_ansistring_to_pchar;virtual;abstract;
          procedure second_pchar_to_string;virtual;abstract;
          procedure second_class_to_intf;virtual;abstract;
          procedure second_char_to_char;virtual;abstract;
          procedure second_nothing; virtual;abstract;
       end;
       ttypeconvnodeclass = class of ttypeconvnode;

       tasnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function getcopy: tnode;override;
          destructor destroy; override;
         protected
          call: tnode;
       end;
       tasnodeclass = class of tasnode;

       tisnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          procedure pass_2;override;
       end;
       tisnodeclass = class of tisnode;

    var
       ctypeconvnode : ttypeconvnodeclass;
       casnode : tasnodeclass;
       cisnode : tisnodeclass;

    procedure inserttypeconv(var p:tnode;const t:ttype);
    procedure inserttypeconv_explicit(var p:tnode;const t:ttype);
    procedure arrayconstructor_to_set(var p : tnode);


implementation

   uses
      globtype,systems,tokens,
      cutils,verbose,globals,widestr,
      symconst,symdef,symsym,symtable,
      ncon,ncal,nset,nadd,ninl,nmem,nmat,
      cgbase,
      htypechk,pass_1,cpubase,cpuinfo;


{*****************************************************************************
                                   Helpers
*****************************************************************************}

    procedure inserttypeconv(var p:tnode;const t:ttype);

      begin
        if not assigned(p.resulttype.def) then
         begin
           resulttypepass(p);
           if codegenerror then
            exit;
         end;

        { don't insert obsolete type conversions }
        if is_equal(p.resulttype.def,t.def) and
           not ((p.resulttype.def.deftype=setdef) and
                (tsetdef(p.resulttype.def).settype <>
                 tsetdef(t.def).settype)) then
         begin
           p.resulttype:=t;
         end
        else
         begin
           p:=ctypeconvnode.create(p,t);
           resulttypepass(p);
         end;
      end;


    procedure inserttypeconv_explicit(var p:tnode;const t:ttype);

      begin
        if not assigned(p.resulttype.def) then
         begin
           resulttypepass(p);
           if codegenerror then
            exit;
         end;

        { don't insert obsolete type conversions }
        if is_equal(p.resulttype.def,t.def) and
           not ((p.resulttype.def.deftype=setdef) and
                (tsetdef(p.resulttype.def).settype <>
                 tsetdef(t.def).settype)) then
         begin
           p.resulttype:=t;
         end
        else
         begin
           p:=ctypeconvnode.create_explicit(p,t);
           resulttypepass(p);
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
        htype       : ttype;
        constset    : Pconstset;
        constsetlo,
        constsethi  : longint;

        procedure update_constsethi(t:ttype);
        begin
          if ((t.def.deftype=orddef) and
              (torddef(t.def).high>=constsethi)) then
            begin
               constsethi:=torddef(t.def).high;
               if htype.def=nil then
                 begin
                    if (constsethi>255) or
                       (torddef(t.def).low<0) then
                      htype:=u8bittype
                    else
                      htype:=t;
                 end;
               if constsethi>255 then
                 constsethi:=255;
            end
          else if ((t.def.deftype=enumdef) and
                  (tenumdef(t.def).max>=constsethi)) then
            begin
               if htype.def=nil then
                 htype:=t;
               constsethi:=tenumdef(t.def).max;
            end;
        end;

        procedure do_set(pos : longint);

  {$ifdef oldset}
        var
          mask,l : longint;
  {$endif}

        begin
          if (pos and not $ff)<>0 then
           Message(parser_e_illegal_set_expr);
          if pos>constsethi then
           constsethi:=pos;
          if pos<constsetlo then
           constsetlo:=pos;
  {$ifdef oldset}
          { to do this correctly we use the 32bit array }
          l:=pos shr 5;
          mask:=1 shl (pos mod 32);
          { do we allow the same twice }
          if (pconst32bitset(constset)^[l] and mask)<>0 then
           Message(parser_e_illegal_set_expr);
          pconst32bitset(constset)^[l]:=pconst32bitset(constset)^[l] or mask;
  {$else}
    include(constset^,pos);
  {$endif}
        end;

      var
        l : Longint;
        lr,hr : TConstExprInt;
        hp : tarrayconstructornode;
      begin
        if p.nodetype<>arrayconstructorn then
         internalerror(200205105);
  new(constset);
      {$ifdef oldset}
        FillChar(constset^,sizeof(constset^),0);
      {$else}
  constset^:=[];
      {$endif}
        htype.reset;
        constsetlo:=0;
        constsethi:=0;
        constp:=csetconstnode.create(nil,htype);
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
              resulttypepass(p2);
              if assigned(p3) then
               resulttypepass(p3);
              if codegenerror then
               break;
              case p2.resulttype.def.deftype of
                 enumdef,
                 orddef:
                   begin
                      getrange(p2.resulttype.def,lr,hr);
                      if assigned(p3) then
                       begin
                         { this isn't good, you'll get problems with
                           type t010 = 0..10;
                                ts = set of t010;
                           var  s : ts;b : t010
                           begin  s:=[1,2,b]; end.
                         if is_integer(p3^.resulttype.def) then
                          begin
                            inserttypeconv(p3,u8bitdef);
                          end;
                         }
                         if assigned(htype.def) and not(is_equal(htype.def,p3.resulttype.def)) then
                           begin
                              aktfilepos:=p3.fileinfo;
                              CGMessage(type_e_typeconflict_in_set);
                           end
                         else
                           begin
                             if (p2.nodetype=ordconstn) and (p3.nodetype=ordconstn) then
                              begin
                                 if not(is_integer(p3.resulttype.def)) then
                                   htype:=p3.resulttype
                                 else
                                   begin
                                     inserttypeconv(p3,u8bittype);
                                     inserttypeconv(p2,u8bittype);
                                   end;

                                for l:=tordconstnode(p2).value to tordconstnode(p3).value do
                                  do_set(l);
                                p2.free;
                                p3.free;
                              end
                             else
                              begin
                                update_constsethi(p2.resulttype);
                                inserttypeconv(p2,htype);

                                update_constsethi(p3.resulttype);
                                inserttypeconv(p3,htype);

                                if assigned(htype.def) then
                                  inserttypeconv(p3,htype)
                                else
                                  inserttypeconv(p3,u8bittype);
                                p4:=csetelementnode.create(p2,p3);
                              end;
                           end;
                       end
                      else
                       begin
                      { Single value }
                         if p2.nodetype=ordconstn then
                          begin
                            if not(is_integer(p2.resulttype.def)) then
                              update_constsethi(p2.resulttype)
                            else
                              inserttypeconv(p2,u8bittype);

                            do_set(tordconstnode(p2).value);
                            p2.free;
                          end
                         else
                          begin
                            update_constsethi(p2.resulttype);

                            if assigned(htype.def) then
                              inserttypeconv(p2,htype)
                            else
                              inserttypeconv(p2,u8bittype);

                            p4:=csetelementnode.create(p2,nil);
                          end;
                       end;
                    end;

                  stringdef :
                    begin
                        { if we've already set elements which are constants }
                        { throw an error                                    }
                        if ((htype.def=nil) and assigned(buildp)) or
                          not(is_char(htype.def)) then
                          CGMessage(type_e_typeconflict_in_set)
                        else
                         for l:=1 to length(pstring(tstringconstnode(p2).value_str)^) do
                          do_set(ord(pstring(tstringconstnode(p2).value_str)^[l]));
                        if htype.def=nil then
                         htype:=cchartype;
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
           if (htype.def=nil) then
            htype:=u8bittype;
         end
        else
         begin
           { empty set [], only remove node }
           p.free;
         end;
        { set the initial set type }
        constp.resulttype.setdef(tsetdef.create(htype,constsethi));
        { determine the resulttype for the tree }
        resulttypepass(buildp);
        { set the new tree }
        p:=buildp;
      end;


{*****************************************************************************
                           TTYPECONVNODE
*****************************************************************************}


    constructor ttypeconvnode.create(node : tnode;const t:ttype);

      begin
         inherited create(typeconvn,node);
         convtype:=tc_not_possible;
         totype:=t;
         if t.def=nil then
          internalerror(200103281);
         set_file_line(node);
      end;


    constructor ttypeconvnode.create_explicit(node : tnode;const t:ttype);

      begin
         self.create(node,t);
         toggleflag(nf_explizit);
      end;


    constructor ttypeconvnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.gettype(totype);
        convtype:=tconverttype(ppufile.getbyte);
      end;


    procedure ttypeconvnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.puttype(totype);
        ppufile.putbyte(byte(convtype));
      end;


    procedure ttypeconvnode.derefimpl;
      begin
        inherited derefimpl;
        totype.resolve;
      end;


    function ttypeconvnode.getcopy : tnode;

      var
         n : ttypeconvnode;

      begin
         n:=ttypeconvnode(inherited getcopy);
         n.convtype:=convtype;
         getcopy:=n;
      end;


    function ttypeconvnode.resulttype_cord_to_pointer : tnode;

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
                  if (tordconstnode(left).value < low(longint)) or
                     (tordconstnode(left).value > high(cardinal)) then
                  CGMessage(parser_e_range_check_error);
                end
              else if (sizeof(TConstPtrUInt) = 8) then
                begin
                  if (tordconstnode(left).value < low(int64)) or
                     (tordconstnode(left).value > high(qword)) then
                  CGMessage(parser_e_range_check_error);
                end
              else
                internalerror(2001020801);
            t:=cpointerconstnode.create(TConstPtrUInt(tordconstnode(left).value),resulttype);
            result:=t;
          end
         else
          internalerror(200104023);
      end;

    function ttypeconvnode.resulttype_chararray_to_string : tnode;

      begin
        result := ccallnode.createinternres(
          'fpc_chararray_to_'+tstringdef(resulttype.def).stringtypname,
          ccallparanode.create(left,nil),resulttype);
        left := nil;
      end;

    function ttypeconvnode.resulttype_string_to_chararray : tnode;

      var
        arrsize: longint;

      begin
         with tarraydef(resulttype.def) do
          begin
            if highrange<lowrange then
             internalerror(75432653);
            arrsize := highrange-lowrange+1;
          end;
         if (left.nodetype = stringconstn) and
            { left.length+1 since there's always a terminating #0 character (JM) }
            (tstringconstnode(left).len+1 >= arrsize) and
            (tstringdef(left.resulttype.def).string_typ=st_shortstring) then
           begin
             { handled separately }
             result := nil;
             exit;
           end;
        result := ccallnode.createinternres(
          'fpc_'+tstringdef(left.resulttype.def).stringtypname+
          '_to_chararray',ccallparanode.create(left,ccallparanode.create(
          cordconstnode.create(arrsize,s32bittype,true),nil)),resulttype);
        left := nil;
      end;


    function ttypeconvnode.resulttype_string_to_string : tnode;

      var
        procname: string[31];
        stringpara : tcallparanode;
        pw : pcompilerwidestring;
        pc : pchar;

      begin
         result:=nil;
         if left.nodetype=stringconstn then
          begin
             { convert ascii 2 unicode }
             if (tstringdef(resulttype.def).string_typ=st_widestring) and
                (tstringconstnode(left).st_type in [st_ansistring,st_shortstring,st_longstring]) then
              begin
                initwidestring(pw);
                ascii2unicode(tstringconstnode(left).value_str,tstringconstnode(left).len,pw);
                ansistringdispose(tstringconstnode(left).value_str,tstringconstnode(left).len);
                pcompilerwidestring(tstringconstnode(left).value_str):=pw;
              end
             else
             { convert unicode 2 ascii }
             if (tstringconstnode(left).st_type=st_widestring) and
                (tstringdef(resulttype.def).string_typ in [st_ansistring,st_shortstring,st_longstring]) then
              begin
                pw:=pcompilerwidestring(tstringconstnode(left).value_str);
                getmem(pc,getlengthwidestring(pw)+1);
                unicode2ascii(pw,pc);
                donewidestring(pw);
                tstringconstnode(left).value_str:=pc;
              end;
             tstringconstnode(left).st_type:=tstringdef(resulttype.def).string_typ;
             tstringconstnode(left).resulttype:=resulttype;
             result:=left;
             left:=nil;
          end
         else
           begin
             { get the correct procedure name }
             procname := 'fpc_'+tstringdef(left.resulttype.def).stringtypname+
                         '_to_'+tstringdef(resulttype.def).stringtypname;

             { create parameter (and remove left node from typeconvnode }
             { since it's reused as parameter)                          }
             stringpara := ccallparanode.create(left,nil);
             left := nil;

             { when converting to shortstrings, we have to pass high(destination) too }
             if (tstringdef(resulttype.def).string_typ = st_shortstring) then
               stringpara.right := ccallparanode.create(cinlinenode.create(
                 in_high_x,false,self.getcopy),nil);

             { and create the callnode }
             result := ccallnode.createinternres(procname,stringpara,resulttype);
           end;
      end;


    function ttypeconvnode.resulttype_char_to_string : tnode;

      var
         procname: string[31];
         para : tcallparanode;
         hp : tstringconstnode;
         ws : pcompilerwidestring;

      begin
         result:=nil;
         if left.nodetype=ordconstn then
           begin
              if tstringdef(resulttype.def).string_typ=st_widestring then
               begin
                 initwidestring(ws);
                 concatwidestringchar(ws,tcompilerwidechar(chr(tordconstnode(left).value)));
                 hp:=cstringconstnode.createwstr(ws);
                 donewidestring(ws);
               end
              else
               hp:=cstringconstnode.createstr(chr(tordconstnode(left).value),tstringdef(resulttype.def).string_typ);
              result:=hp;
           end
         else
           { shortstrings are handled 'inline' }
           if tstringdef(resulttype.def).string_typ <> st_shortstring then
             begin
               { create the parameter }
               para := ccallparanode.create(left,nil);
               left := nil;

               { and the procname }
               procname := 'fpc_char_to_' +tstringdef(resulttype.def).stringtypname;

               { and finally the call }
               result := ccallnode.createinternres(procname,para,resulttype);
             end
           else
             begin
               { create word(byte(char) shl 8 or 1) for litte endian machines }
               { and word(byte(char) or 256) for big endian machines          }
               left := ctypeconvnode.create(left,u8bittype);
               left.toggleflag(nf_explizit);
               if (target_info.endian = endian_little) then
                 left := caddnode.create(orn,
                   cshlshrnode.create(shln,left,cordconstnode.create(8,s32bittype,false)),
                   cordconstnode.create(1,s32bittype,false))
               else
                 left := caddnode.create(orn,left,
                   cordconstnode.create(1 shl 8,s32bittype,false));
               left := ctypeconvnode.create(left,u16bittype);
               left.toggleflag(nf_explizit);
               resulttypepass(left);
             end;
      end;


    function ttypeconvnode.resulttype_char_to_chararray : tnode;

      begin
        if resulttype.def.size <> 1 then
          begin
            { convert first to string, then to chararray }
            inserttypeconv(left,cshortstringtype);
            inserttypeconv(left,resulttype);
            result:=left;
            left := nil;
            exit;
          end;
        result := nil;
      end;


    function ttypeconvnode.resulttype_char_to_char : tnode;

      var
         hp : tordconstnode;

      begin
         result:=nil;
         if left.nodetype=ordconstn then
           begin
             if (torddef(resulttype.def).typ=uchar) and
                (torddef(left.resulttype.def).typ=uwidechar) then
              begin
                hp:=cordconstnode.create(
                      ord(unicode2asciichar(tcompilerwidechar(tordconstnode(left).value))),
                      cchartype,true);
                result:=hp;
              end
             else if (torddef(resulttype.def).typ=uwidechar) and
                     (torddef(left.resulttype.def).typ=uchar) then
              begin
                hp:=cordconstnode.create(
                      asciichar2unicode(chr(tordconstnode(left).value)),
                      cwidechartype,true);
                result:=hp;
              end
             else
              internalerror(200105131);
             exit;
           end;
      end;


    function ttypeconvnode.resulttype_int_to_real : tnode;

      var
        t : trealconstnode;

      begin
        result:=nil;
        if left.nodetype=ordconstn then
         begin
           t:=crealconstnode.create(tordconstnode(left).value,resulttype);
           result:=t;
         end;
      end;


    function ttypeconvnode.resulttype_real_to_real : tnode;

      var
        t : tnode;

      begin
         result:=nil;
         if is_currency(left.resulttype.def) and not(is_currency(resulttype.def)) then
           begin
           end
         else
           if is_currency(resulttype.def) then
             begin
             end;
         if left.nodetype=realconstn then
           begin
             t:=crealconstnode.create(trealconstnode(left).value_real,resulttype);
             result:=t;
           end;
      end;


    function ttypeconvnode.resulttype_cchar_to_pchar : tnode;

      begin
         result:=nil;
         if is_pwidechar(resulttype.def) then
          inserttypeconv(left,cwidestringtype)
         else
          inserttypeconv(left,cshortstringtype);
         { evaluate again, reset resulttype so the convert_typ
           will be calculated again and cstring_to_pchar will
           be used for futher conversion }
         result:=det_resulttype;
      end;


    function ttypeconvnode.resulttype_cstring_to_pchar : tnode;

      begin
         result:=nil;
         if is_pwidechar(resulttype.def) then
           inserttypeconv(left,cwidestringtype);
      end;


    function ttypeconvnode.resulttype_arrayconstructor_to_set : tnode;

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


    function ttypeconvnode.resulttype_pchar_to_string : tnode;

      begin
        result := ccallnode.createinternres(
          'fpc_pchar_to_'+tstringdef(resulttype.def).stringtypname,
          ccallparanode.create(left,nil),resulttype);
        left := nil;
      end;


    function ttypeconvnode.resulttype_interface_to_guid : tnode;

      begin
        if tobjectdef(left.resulttype.def).isiidguidvalid then
          result:=cguidconstnode.create(tobjectdef(left.resulttype.def).iidguid);
      end;


    function ttypeconvnode.resulttype_dynarray_to_openarray : tnode;

      begin
        { a dynamic array is a pointer to an array, so to convert it to }
        { an open array, we have to dereference it (JM)                 }
        result := ctypeconvnode.create(left,voidpointertype);
        { left is reused }
        left := nil;
        result.toggleflag(nf_explizit);
        result := cderefnode.create(result);
        result.resulttype := resulttype;
      end;


    function ttypeconvnode.resulttype_call_helper(c : tconverttype) : tnode;

      const
         resulttypeconvert : array[tconverttype] of pointer = (
          {equal} nil,
          {not_possible} nil,
          { string_2_string } @ttypeconvnode.resulttype_string_to_string,
          { char_2_string } @ttypeconvnode.resulttype_char_to_string,
          { char_2_chararray } @ttypeconvnode.resulttype_char_to_chararray,
          { pchar_2_string } @ttypeconvnode.resulttype_pchar_to_string,
          { cchar_2_pchar } @ttypeconvnode.resulttype_cchar_to_pchar,
          { cstring_2_pchar } @ttypeconvnode.resulttype_cstring_to_pchar,
          { ansistring_2_pchar } nil,
          { string_2_chararray } @ttypeconvnode.resulttype_string_to_chararray,
          { chararray_2_string } @ttypeconvnode.resulttype_chararray_to_string,
          { array_2_pointer } nil,
          { pointer_2_array } nil,
          { int_2_int } nil,
          { int_2_bool } nil,
          { bool_2_bool } nil,
          { bool_2_int } nil,
          { real_2_real } @ttypeconvnode.resulttype_real_to_real,
          { int_2_real } @ttypeconvnode.resulttype_int_to_real,
          { proc_2_procvar } nil,
          { arrayconstructor_2_set } @ttypeconvnode.resulttype_arrayconstructor_to_set,
          { load_smallset } nil,
          { cord_2_pointer } @ttypeconvnode.resulttype_cord_to_pointer,
          { intf_2_string } nil,
          { intf_2_guid } @ttypeconvnode.resulttype_interface_to_guid,
          { class_2_intf } nil,
          { char_2_char } @ttypeconvnode.resulttype_char_to_char,
          { normal_2_smallset} nil,
          { dynarray_2_openarray} @resulttype_dynarray_to_openarray
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
         r.proc:=resulttypeconvert[c];
         r.obj:=self;
         if assigned(r.proc) then
          result:=tprocedureofobject(r){$ifdef FPC}();{$endif FPC}
      end;


    function ttypeconvnode.det_resulttype:tnode;

      var
        hp : tnode;
        currprocdef,
        aprocdef : tprocdef;

      begin
        result:=nil;
        resulttype:=totype;

        resulttypepass(left);
        if codegenerror then
         exit;

        { remove obsolete type conversions }
        if is_equal(left.resulttype.def,resulttype.def) then
          begin
          { becuase is_equal only checks the basetype for sets we need to
            check here if we are loading a smallset into a normalset }
            if (resulttype.def.deftype=setdef) and
               (left.resulttype.def.deftype=setdef) and
               ((tsetdef(resulttype.def).settype = smallset) xor
                (tsetdef(left.resulttype.def).settype = smallset)) then
              begin
                { constant sets can be converted by changing the type only }
                if (left.nodetype=setconstn) then
                 begin
                   tsetdef(left.resulttype.def).changesettype(tsetdef(resulttype.def).settype);
                   result:=left;
                   left:=nil;
                   exit;
                 end;

                if (tsetdef(resulttype.def).settype <> smallset) then
                 convtype:=tc_load_smallset
                else
                 convtype := tc_normal_2_smallset;
                exit;
              end
            else
             begin
               left.resulttype:=resulttype;
               result:=left;
               left:=nil;
               exit;
             end;
          end;
        aprocdef:=assignment_overloaded(left.resulttype.def,resulttype.def);
        if assigned(aprocdef) then
          begin
             procinfo.flags:=procinfo.flags or pi_do_call;
             hp:=ccallnode.create(ccallparanode.create(left,nil),
                                  overloaded_operators[_assignment],nil,nil);
             { tell explicitly which def we must use !! (PM) }
             tcallnode(hp).procdefinition:=aprocdef;
             left:=nil;
             result:=hp;
             exit;
          end;

        if isconvertable(left.resulttype.def,resulttype.def,convtype,left.nodetype,nf_explizit in flags)=0 then
         begin
           {Procedures have a resulttype.def of voiddef and functions of their
           own resulttype.def. They will therefore always be incompatible with
           a procvar. Because isconvertable cannot check for procedures we
           use an extra check for them.}
           if (m_tp_procvar in aktmodeswitches) then
            begin
              if (resulttype.def.deftype=procvardef) and
                 (is_procsym_load(left) or is_procsym_call(left)) then
               begin
                 if is_procsym_call(left) then
                  begin
        currprocdef:=Tprocsym(Tcallnode(left).symtableprocentry).search_procdef_byprocvardef(Tprocvardef(resulttype.def));
                    hp:=cloadnode.create_procvar(tprocsym(tcallnode(left).symtableprocentry),
                        currprocdef,tcallnode(left).symtableproc);
                    if (tcallnode(left).symtableprocentry.owner.symtabletype=objectsymtable) and
                       assigned(tcallnode(left).methodpointer) then
                      tloadnode(hp).set_mp(tcallnode(left).methodpointer.getcopy);
                    resulttypepass(hp);
                    left.free;
                    left:=hp;
                    aprocdef:=tprocdef(left.resulttype.def);
                  end
                 else
                  begin
                    if (left.nodetype<>addrn) then
                      aprocdef:=tprocsym(tloadnode(left).symtableentry).first_procdef;
                  end;
                 convtype:=tc_proc_2_procvar;
                 { Now check if the procedure we are going to assign to
                   the procvar,  is compatible with the procvar's type }
                 if assigned(aprocdef) then
                  begin
                    if not proc_to_procvar_equal(aprocdef,tprocvardef(resulttype.def),false) then
                     CGMessage2(type_e_incompatible_types,aprocdef.typename,resulttype.def.typename);
                  end
                 else
                  CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                 exit;
               end;
            end;
           if nf_explizit in flags then
            begin
              { check if the result could be in a register }
              if not(tstoreddef(resulttype.def).is_intregable) and
                not(tstoreddef(resulttype.def).is_fpuregable) then
                make_not_regable(left);
              { boolean to byte are special because the
                location can be different }

              if is_integer(resulttype.def) and
                 is_boolean(left.resulttype.def) then
               begin
                  convtype:=tc_bool_2_int;
                  exit;
               end;

              if is_char(resulttype.def) and
                 is_boolean(left.resulttype.def) then
               begin
                  convtype:=tc_bool_2_int;
                  exit;
               end;

              { ansistring to pchar }
              if is_pchar(resulttype.def) and
                 is_ansistring(left.resulttype.def) then
               begin
                 convtype:=tc_ansistring_2_pchar;
                 exit;
               end;
              { do common tc_equal cast }
              convtype:=tc_equal;

              { enum to ordinal will always be s32bit }
              if (left.resulttype.def.deftype=enumdef) and
                 is_ordinal(resulttype.def) then
               begin
                 if left.nodetype=ordconstn then
                  begin
                    hp:=cordconstnode.create(tordconstnode(left).value,
                       resulttype,true);
                    result:=hp;
                    exit;
                  end
                 else
                  begin
                    if isconvertable(s32bittype.def,resulttype.def,convtype,ordconstn,false)=0 then
                      CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                  end;
               end

              { ordinal to enumeration }
              else
               if (resulttype.def.deftype=enumdef) and
                  is_ordinal(left.resulttype.def) then
                begin
                  if left.nodetype=ordconstn then
                   begin
                     hp:=cordconstnode.create(tordconstnode(left).value,
                       resulttype,true);
                     result:=hp;
                     exit;
                   end
                  else
                   begin
                     if IsConvertable(left.resulttype.def,s32bittype.def,convtype,ordconstn,false)=0 then
                       CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                   end;
                end

               { nil to ordinal node }
               else if (left.nodetype=niln) and is_ordinal(resulttype.def) then
                  begin
                     hp:=cordconstnode.create(0,resulttype,true);
                     result:=hp;
                     exit;
                  end

              { constant pointer to ordinal }
              else if is_ordinal(resulttype.def) and
                      (left.nodetype=pointerconstn) then
                begin
                   hp:=cordconstnode.create(tpointerconstnode(left).value,
                     resulttype,true);
                   result:=hp;
                   exit;
                end

              { class to class or object to object, with checkobject support }
              else if (resulttype.def.deftype=objectdef) and
                      (left.resulttype.def.deftype=objectdef) then
                begin
                  if (cs_check_object in aktlocalswitches) then
                   begin
                     if is_class_or_interface(resulttype.def) then
                      begin
                        { we can translate the typeconvnode to 'as' when
                          typecasting to a class or interface }
                        hp:=casnode.create(left,cloadvmtnode.create(ctypenode.create(resulttype)));
                        left:=nil;
                        result:=hp;
                        exit;
                      end;
                   end
                  else
                   begin
                     { check if the types are related }
                     if (not(tobjectdef(left.resulttype.def).is_related(tobjectdef(resulttype.def)))) and
                        (not(tobjectdef(resulttype.def).is_related(tobjectdef(left.resulttype.def)))) then
                       CGMessage2(type_w_classes_not_related,left.resulttype.def.typename,resulttype.def.typename);
                   end;
                end

              {Are we typecasting an ordconst to a char?}
              else
                if is_char(resulttype.def) and
                   is_ordinal(left.resulttype.def) then
                 begin
                   if left.nodetype=ordconstn then
                    begin
                      hp:=cordconstnode.create(tordconstnode(left).value,
                        resulttype,true);
                      result:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(left.resulttype.def,u8bittype.def,convtype,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                    end;
                 end

              {Are we typecasting an ordconst to a wchar?}
              else
                if is_widechar(resulttype.def) and
                   is_ordinal(left.resulttype.def) then
                 begin
                   if left.nodetype=ordconstn then
                    begin
                      hp:=cordconstnode.create(tordconstnode(left).value,
                        resulttype,true);
                      result:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(left.resulttype.def,u16bittype.def,convtype,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                    end;
                 end

              { char to ordinal }
              else
                if is_char(left.resulttype.def) and
                   is_ordinal(resulttype.def) then
                 begin
                   if left.nodetype=ordconstn then
                    begin
                      hp:=cordconstnode.create(tordconstnode(left).value,
                        resulttype,true);
                      result:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(u8bittype.def,resulttype.def,convtype,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                    end;
                 end
              { widechar to ordinal }
              else
                if is_widechar(left.resulttype.def) and
                   is_ordinal(resulttype.def) then
                 begin
                   if left.nodetype=ordconstn then
                    begin
                      hp:=cordconstnode.create(tordconstnode(left).value,
                         resulttype,true);
                      result:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(u16bittype.def,resulttype.def,convtype,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                    end;
                 end

              { ordinal to pointer }
              else
                if (m_delphi in aktmodeswitches) and
                   is_ordinal(left.resulttype.def) and
                   (resulttype.def.deftype=pointerdef) then
                 begin
                   if left.nodetype=pointerconstn then
                    begin
                      hp:=cordconstnode.create(tpointerconstnode(left).value,
                         resulttype,true);
                      result:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(left.resulttype.def,ordpointertype.def,convtype,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                    end;
                 end

               { only if the same size or formal def }
               { why do we allow typecasting of voiddef ?? (PM) }
               else
                begin
                  if not(
                     (left.resulttype.def.deftype=formaldef) or
                     (not(is_open_array(left.resulttype.def)) and
                      (left.resulttype.def.size=resulttype.def.size)) or
                     (is_void(left.resulttype.def)  and
                      (left.nodetype=derefn))
                     ) then
                    CGMessage(cg_e_illegal_type_conversion);
                  if ((left.resulttype.def.deftype=orddef) and
                      (resulttype.def.deftype=pointerdef)) or
                      ((resulttype.def.deftype=orddef) and
                       (left.resulttype.def.deftype=pointerdef)) then
                    CGMessage(cg_d_pointer_to_longint_conv_not_portable);
                end;

               { the conversion into a strutured type is only }
               { possible, if the source is not a register    }
               if ((resulttype.def.deftype in [recorddef,stringdef,arraydef]) or
                   ((resulttype.def.deftype=objectdef) and not(is_class(resulttype.def)))
                  ) and (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) { and
                   it also works if the assignment is overloaded
                   YES but this code is not executed if assignment is overloaded (PM)
                  not assigned(assignment_overloaded(left.resulttype.def,resulttype.def))} then
                 CGMessage(cg_e_illegal_type_conversion);
            end
           else
            CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
         end;

       { tp7 procvar support, when right is not a procvardef and we got a
         loadn of a procvar then convert to a calln, the check for the
         result is already done in is_convertible, also no conflict with
         @procvar is here because that has an extra addrn }
         if (m_tp_procvar in aktmodeswitches) and
            (resulttype.def.deftype<>procvardef) and
            (left.resulttype.def.deftype=procvardef) and
            (left.nodetype=loadn) then
          begin
            hp:=ccallnode.create(nil,nil,nil,nil);
            tcallnode(hp).set_procvar(left);
            resulttypepass(hp);
            left:=hp;
          end;

        { remove typeconv after niln, but not when the result is a
          methodpointer. The typeconv of the methodpointer will then
          take care of updateing size of niln to OS_64 }
        if (left.nodetype=niln) and
           not((resulttype.def.deftype=procvardef) and
               (po_methodpointer in tprocvardef(resulttype.def).procoptions)) then
          begin
            left.resulttype:=resulttype;
            result:=left;
            left:=nil;
            exit;
          end;

        { ordinal contants can be directly converted }
        if (left.nodetype=ordconstn) and is_ordinal(resulttype.def) and
        { but not char to char because it is a widechar to char or via versa }
        { which needs extra code to do the code page transistion             }
          not(convtype=tc_char_2_char) then
          begin
             { replace the resulttype and recheck the range }
             left.resulttype:=resulttype;
             testrange(left.resulttype.def,tordconstnode(left).value,(nf_explizit in flags));
             result:=left;
             left:=nil;
             exit;
          end;

        { fold nil to any pointer type }
        if (left.nodetype=niln) and (resulttype.def.deftype=pointerdef) then
          begin
             hp:=cnilnode.create;
             hp.resulttype:=resulttype;
             result:=hp;
             exit;
          end;

        { further, pointerconstn to any pointer is folded too }
        if (left.nodetype=pointerconstn) and (resulttype.def.deftype=pointerdef) then
          begin
             left.resulttype:=resulttype;
             result:=left;
             left:=nil;
             exit;
          end;

        { now call the resulttype helper to do constant folding }
        result:=resulttype_call_helper(convtype);
      end;

    {$ifdef var_notification}
      procedure Ttypeconvnode.mark_write;

      begin
        left.mark_write;
      end;
    {$endif}


    function ttypeconvnode.first_cord_to_pointer : tnode;

      begin
        result:=nil;
        internalerror(200104043);
      end;


    function ttypeconvnode.first_int_to_int : tnode;

      begin
        first_int_to_int:=nil;
        if (left.location.loc<>LOC_REGISTER) and
           (resulttype.def.size>left.resulttype.def.size) then
           location.loc:=LOC_REGISTER;
        if is_64bitint(resulttype.def) then
          registers32:=max(registers32,2)
        else
          registers32:=max(registers32,1);
      end;


    function ttypeconvnode.first_cstring_to_pchar : tnode;

      begin
         first_cstring_to_pchar:=nil;
         registers32:=1;
         location.loc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_string_to_chararray : tnode;

      begin
         first_string_to_chararray:=nil;
         registers32:=1;
         location.loc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_char_to_string : tnode;

      begin
         first_char_to_string:=nil;
         location.loc:=LOC_CREFERENCE;
      end;


    function ttypeconvnode.first_nothing : tnode;
      begin
         first_nothing:=nil;
      end;


    function ttypeconvnode.first_array_to_pointer : tnode;

      begin
         first_array_to_pointer:=nil;
         if registers32<1 then
           registers32:=1;
         location.loc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_int_to_real: tnode;
      var
        fname: string[19];
        typname : string[12];
      begin
        { Get the type name  }
        {  Normally the typename should be one of the following:
            single, double - carl
        }
        typname := lower(pbestrealtype^.def.gettypename);
        { converting a 64bit integer to a float requires a helper }
        if is_64bitint(left.resulttype.def) then
          begin
            if is_signed(left.resulttype.def) then
              fname := 'fpc_int64_to_'+typname
            else
              fname := 'fpc_qword_to_'+typname;
            result := ccallnode.createintern(fname,ccallparanode.create(
              left,nil));
            left:=nil;
            firstpass(result);
            exit;
          end
        else
          { other integers are supposed to be 32 bit }
          begin
            if is_signed(left.resulttype.def) then
              fname := 'fpc_longint_to_'+typname
            else
              fname := 'fpc_longword_to_'+typname;
            result := ccallnode.createintern(fname,ccallparanode.create(
              left,nil));
            left:=nil;
            firstpass(result);
            exit;
          end;
      end;




    function ttypeconvnode.first_real_to_real : tnode;
      begin
         first_real_to_real:=nil;
        { comp isn't a floating type }
{$ifdef i386}
         if (tfloatdef(resulttype.def).typ=s64comp) and
            (tfloatdef(left.resulttype.def).typ<>s64comp) and
            not (nf_explizit in flags) then
           CGMessage(type_w_convert_real_2_comp);
{$endif}
         if registersfpu<1 then
           registersfpu:=1;
         location.loc:=LOC_FPUREGISTER;
      end;


    function ttypeconvnode.first_pointer_to_array : tnode;

      begin
         first_pointer_to_array:=nil;
         if registers32<1 then
           registers32:=1;
         location.loc:=LOC_REFERENCE;
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
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           exit;
         { when converting to 64bit, first convert to a 32bit int and then   }
         { convert to a 64bit int (only necessary for 32bit processors) (JM) }
         if resulttype.def.size > sizeof(aword) then
           begin
             result := ctypeconvnode.create(left,u32bittype);
             result.toggleflag(nf_explizit);
             result := ctypeconvnode.create(result,resulttype);
             left := nil;
             firstpass(result);
             exit;
           end;
         location.loc:=LOC_REGISTER;
         if registers32<1 then
           registers32:=1;
      end;


    function ttypeconvnode.first_int_to_bool : tnode;

      begin
         first_int_to_bool:=nil;
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           exit;
         location.loc:=LOC_REGISTER;
         { need if bool to bool !!
           not very nice !!
         insertypeconv(left,s32bittype);
         left.explizit:=true;
         firstpass(left);  }
         if registers32<1 then
           registers32:=1;
      end;


    function ttypeconvnode.first_bool_to_bool : tnode;
      begin
         first_bool_to_bool:=nil;
         location.loc:=LOC_REGISTER;
         if registers32<1 then
           registers32:=1;
      end;


    function ttypeconvnode.first_char_to_char : tnode;

      begin
         first_char_to_char:=nil;
         location.loc:=LOC_REGISTER;
         if registers32<1 then
           registers32:=1;
      end;


    function ttypeconvnode.first_proc_to_procvar : tnode;
      begin
         first_proc_to_procvar:=nil;
         if (left.location.loc<>LOC_REFERENCE) then
           CGMessage(cg_e_illegal_expression);
         registers32:=left.registers32;
         if registers32<1 then
           registers32:=1;
         location.loc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_load_smallset : tnode;

      var
        srsym: ttypesym;
        p: tcallparanode;

      begin
        if not searchsystype('FPC_SMALL_SET',srsym) then
          internalerror(200108313);
        p := ccallparanode.create(left,nil);
        { reused }
        left := nil;
        { convert parameter explicitely to fpc_small_set }
        p.left := ctypeconvnode.create(p.left,srsym.restype);
        p.left.toggleflag(nf_explizit);
        { create call, adjust resulttype }
        result :=
          ccallnode.createinternres('fpc_set_load_small',p,resulttype);
        firstpass(result);
      end;


    function ttypeconvnode.first_ansistring_to_pchar : tnode;

      begin
         first_ansistring_to_pchar:=nil;
         location.loc:=LOC_REGISTER;
         if registers32<1 then
           registers32:=1;
      end;


    function ttypeconvnode.first_arrayconstructor_to_set : tnode;
      begin
        first_arrayconstructor_to_set:=nil;
        internalerror(200104022);
      end;

    function ttypeconvnode.first_class_to_intf : tnode;

      begin
         first_class_to_intf:=nil;
         location.loc:=LOC_REFERENCE;
         if registers32<1 then
           registers32:=1;
      end;

    function ttypeconvnode._first_int_to_int : tnode;
      begin
         result:=first_int_to_int;
      end;

    function ttypeconvnode._first_cstring_to_pchar : tnode;
      begin
         result:=first_cstring_to_pchar;
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

    function ttypeconvnode._first_load_smallset : tnode;
      begin
         result:=first_load_smallset;
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
           @ttypeconvnode._first_nothing, {equal}
           @ttypeconvnode._first_nothing, {not_possible}
           nil, { removed in resulttype_string_to_string }
           @ttypeconvnode._first_char_to_string,
           @ttypeconvnode._first_nothing, { char_2_chararray, needs nothing extra }
           nil, { removed in resulttype_chararray_to_string }
           @ttypeconvnode._first_cchar_to_pchar,
           @ttypeconvnode._first_cstring_to_pchar,
           @ttypeconvnode._first_ansistring_to_pchar,
           @ttypeconvnode._first_string_to_chararray,
           nil, { removed in resulttype_chararray_to_string }
           @ttypeconvnode._first_array_to_pointer,
           @ttypeconvnode._first_pointer_to_array,
           @ttypeconvnode._first_int_to_int,
           @ttypeconvnode._first_int_to_bool,
           @ttypeconvnode._first_bool_to_bool,
           @ttypeconvnode._first_bool_to_int,
           @ttypeconvnode._first_real_to_real,
           @ttypeconvnode._first_int_to_real,
           @ttypeconvnode._first_proc_to_procvar,
           @ttypeconvnode._first_arrayconstructor_to_set,
           @ttypeconvnode._first_load_smallset,
           @ttypeconvnode._first_cord_to_pointer,
           @ttypeconvnode._first_nothing,
           @ttypeconvnode._first_nothing,
           @ttypeconvnode._first_class_to_intf,
           @ttypeconvnode._first_char_to_char,
           @ttypeconvnode._first_nothing,
           @ttypeconvnode._first_nothing
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
         first_call_helper:=tprocedureofobject(r){$ifdef FPC}();{$endif FPC}
      end;


    function ttypeconvnode.pass_1 : tnode;
      begin
        result:=nil;
        firstpass(left);
        if codegenerror then
         exit;

        { load the value_str from the left part }
        registers32:=left.registers32;
        registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif}
        location.loc:=left.location.loc;

        if nf_explizit in flags then
         begin
           { check if the result could be in a register }
           if not(tstoreddef(resulttype.def).is_intregable) and
              not(tstoreddef(resulttype.def).is_fpuregable) then
            make_not_regable(left);
         end;

        result:=first_call_helper(convtype);
      end;


    function ttypeconvnode.docompare(p: tnode) : boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (convtype = ttypeconvnode(p).convtype);
      end;


{*****************************************************************************
                                TISNODE
*****************************************************************************}

    constructor tisnode.create(l,r : tnode);

      begin
         inherited create(isn,l,r);
      end;


    function tisnode.det_resulttype:tnode;
      var
        paras: tcallparanode;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);

         set_varstate(left,true);
         set_varstate(right,true);

         if codegenerror then
           exit;

         if (right.resulttype.def.deftype=classrefdef) then
          begin
            { left must be a class }
            if is_class(left.resulttype.def) then
             begin
               { the operands must be related }
               if (not(tobjectdef(left.resulttype.def).is_related(
                  tobjectdef(tclassrefdef(right.resulttype.def).pointertype.def)))) and
                  (not(tobjectdef(tclassrefdef(right.resulttype.def).pointertype.def).is_related(
                  tobjectdef(left.resulttype.def)))) then
                 CGMessage2(type_e_classes_not_related,left.resulttype.def.typename,
                            tclassrefdef(right.resulttype.def).pointertype.def.typename);
             end
            else
             CGMessage1(type_e_class_type_expected,left.resulttype.def.typename);

            { call fpc_do_is helper }
            paras := ccallparanode.create(
                         left,
                     ccallparanode.create(
                         right,nil));
            result := ccallnode.createintern('fpc_do_is',paras);
            left := nil;
            right := nil;
          end
         else if is_interface(right.resulttype.def) then
          begin
            { left is a class }
            if is_class(left.resulttype.def) then
             begin
               { the operands must be related }
               if not(assigned(tobjectdef(left.resulttype.def).implementedinterfaces) and
                      (tobjectdef(left.resulttype.def).implementedinterfaces.searchintf(right.resulttype.def)<>-1)) then
                 CGMessage2(type_e_classes_not_related,left.resulttype.def.typename,right.resulttype.def.typename);
             end
            { left is an interface }
            else if is_interface(left.resulttype.def) then
             begin
               { the operands must be related }
               if (not(tobjectdef(left.resulttype.def).is_related(tobjectdef(right.resulttype.def)))) and
                  (not(tobjectdef(right.resulttype.def).is_related(tobjectdef(left.resulttype.def)))) then
                 CGMessage(type_e_mismatch);
             end
            else
             CGMessage1(type_e_class_type_expected,left.resulttype.def.typename);

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
          CGMessage1(type_e_class_or_interface_type_expected,right.resulttype.def.typename);

         resulttype:=booltype;
      end;


    function tisnode.pass_1 : tnode;
      begin
        internalerror(200204254);
        result:=nil;
      end;

    { dummy pass_2, it will never be called, but we need one since }
    { you can't instantiate an abstract class                      }
    procedure tisnode.pass_2;
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


    function tasnode.det_resulttype:tnode;
      var
        hp : tnode;
      begin
         result:=nil;
         resulttypepass(right);
         resulttypepass(left);

         set_varstate(right,true);
         set_varstate(left,true);

         if codegenerror then
           exit;

         if (right.resulttype.def.deftype=classrefdef) then
          begin
            { left must be a class }
            if is_class(left.resulttype.def) then
             begin
               { the operands must be related }
               if (not(tobjectdef(left.resulttype.def).is_related(
                  tobjectdef(tclassrefdef(right.resulttype.def).pointertype.def)))) and
                  (not(tobjectdef(tclassrefdef(right.resulttype.def).pointertype.def).is_related(
                  tobjectdef(left.resulttype.def)))) then
                 CGMessage2(type_e_classes_not_related,left.resulttype.def.typename,
                            tclassrefdef(right.resulttype.def).pointertype.def.typename);
             end
            else
             CGMessage1(type_e_class_type_expected,left.resulttype.def.typename);
            resulttype:=tclassrefdef(right.resulttype.def).pointertype;
          end
         else if is_interface(right.resulttype.def) then
          begin
            { left is a class }
            if is_class(left.resulttype.def) then
             begin
               { the operands must be related }
               if not(assigned(tobjectdef(left.resulttype.def).implementedinterfaces) and
                      (tobjectdef(left.resulttype.def).implementedinterfaces.searchintf(right.resulttype.def)<>-1)) then
                 CGMessage2(type_e_classes_not_related,left.resulttype.def.typename,right.resulttype.def.typename);
             end
            { left is an interface }
            else if is_interface(left.resulttype.def) then
             begin
               { the operands must be related }
               if (not(tobjectdef(left.resulttype.def).is_related(tobjectdef(right.resulttype.def)))) and
                  (not(tobjectdef(right.resulttype.def).is_related(tobjectdef(left.resulttype.def)))) then
                 CGMessage2(type_e_classes_not_related,left.resulttype.def.typename,right.resulttype.def.typename);
             end
            else
             CGMessage1(type_e_class_type_expected,left.resulttype.def.typename);

            resulttype:=right.resulttype;

            { load the GUID of the interface }
            if (right.nodetype=typen) then
             begin
               if tobjectdef(right.resulttype.def).isiidguidvalid then
                 begin
                   hp:=cguidconstnode.create(tobjectdef(right.resulttype.def).iidguid);
                   right.free;
                   right:=hp;
                 end
               else
                 internalerror(200206282);
               resulttypepass(right);
             end;
          end
         else
          CGMessage1(type_e_class_or_interface_type_expected,right.resulttype.def.typename);
      end;


    function tasnode.getcopy: tnode;

      begin
        result := inherited getcopy;
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
            if is_class(left.resulttype.def) and
               (right.resulttype.def.deftype=classrefdef) then
              call := ccallnode.createinternres('fpc_do_as',
                ccallparanode.create(left,ccallparanode.create(right,nil)),
                resulttype)
            else
              begin
                if is_class(left.resulttype.def) then
                  procname := 'fpc_class_as_intf'
                else
                  procname := 'fpc_intf_as';
                call := ccallnode.createinternres(procname,
                   ccallparanode.create(right,ccallparanode.create(left,nil)),
                   resulttype);
              end;
            left := nil;
            right := nil;
            firstpass(call);
            if codegenerror then
              exit;
           location.loc:=call.location.loc;
           registers32:=call.registers32;
           registersfpu:=call.registersfpu;
{$ifdef SUPPORT_MMX}
           registersmmx:=call.registersmmx;
{$endif SUPPORT_MMX}
         end;
      end;


begin
   ctypeconvnode:=ttypeconvnode;
   casnode:=tasnode;
   cisnode:=tisnode;
end.
{
  $Log$
  Revision 1.81  2002-09-16 14:11:13  peter
    * add argument to equal_paras() to support default values or not

  Revision 1.80  2002/09/07 20:40:23  carl
    * cardinal -> longword

  Revision 1.79  2002/09/07 15:25:03  peter
    * old logs removed and tabs fixed

  Revision 1.78  2002/09/07 12:16:04  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.77  2002/09/05 05:56:07  jonas
    - reverted my last commit, it was completely bogus :(

  Revision 1.75  2002/09/02 19:24:42  peter
    * array of char support for Str()

  Revision 1.74  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.73  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.72  2002/08/20 18:23:33  jonas
    * the as node again uses a compilerproc
    + (untested) support for interface "as" statements

  Revision 1.71  2002/08/19 19:36:43  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.70  2002/08/17 09:23:36  florian
    * first part of procinfo rewrite

  Revision 1.69  2002/08/14 19:26:55  carl
    + generic int_to_real type conversion
    + generic unaryminus node

  Revision 1.68  2002/08/11 16:08:55  florian
    + support of explicit type case boolean->char

  Revision 1.67  2002/08/11 15:28:00  florian
    + support of explicit type case <any ordinal type>->pointer
      (delphi mode only)

  Revision 1.66  2002/08/09 07:33:01  florian
    * a couple of interface related fixes

  Revision 1.65  2002/07/29 21:23:42  florian
    * more fixes for the ppc
    + wrappers for the tcnvnode.first_* stuff introduced

  Revision 1.64  2002/07/23 12:34:30  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.63  2002/07/23 09:51:22  daniel
  * Tried to make Tprocsym.defs protected. I didn't succeed but the cleanups
    are worth comitting.

  Revision 1.62  2002/07/22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.61  2002/07/20 17:16:02  florian
    + source code page support

  Revision 1.60  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.59  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.58  2002/05/18 13:34:09  peter
    * readded missing revisions

}

{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       node,
       symtype,types,
       nld;

    type
       ttypeconvnode = class(tunarynode)
          totype   : ttype;
          convtype : tconverttype;
          constructor create(node : tnode;const t : ttype);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
       private
          function resulttype_cord_to_pointer : tnode;
          function resulttype_string_to_string : tnode;
          function resulttype_char_to_string : tnode;
          function resulttype_int_to_real : tnode;
          function resulttype_real_to_real : tnode;
          function resulttype_cchar_to_pchar : tnode;
          function resulttype_cstring_to_pchar : tnode;
          function resulttype_char_to_char : tnode;
          function resulttype_arrayconstructor_to_set : tnode;
          function resulttype_call_helper(c : tconverttype) : tnode;
       protected
          function first_int_to_int : tnode;virtual;
          function first_cstring_to_pchar : tnode;virtual;
          function first_string_to_chararray : tnode;virtual;
          function first_string_to_string : tnode;virtual;
          function first_char_to_string : tnode;virtual;
          function first_nothing : tnode;virtual;
          function first_array_to_pointer : tnode;virtual;
          function first_int_to_real : tnode;virtual;
          function first_real_to_real : tnode;virtual;
          function first_pointer_to_array : tnode;virtual;
          function first_chararray_to_string : tnode;virtual;
          function first_cchar_to_pchar : tnode;virtual;
          function first_bool_to_int : tnode;virtual;
          function first_int_to_bool : tnode;virtual;
          function first_bool_to_bool : tnode;virtual;
          function first_proc_to_procvar : tnode;virtual;
          function first_load_smallset : tnode;virtual;
          function first_cord_to_pointer : tnode;virtual;
          function first_pchar_to_string : tnode;virtual;
          function first_ansistring_to_pchar : tnode;virtual;
          function first_arrayconstructor_to_set : tnode;virtual;
          function first_class_to_intf : tnode;virtual;
          function first_char_to_char : tnode;virtual;
          function first_call_helper(c : tconverttype) : tnode;
       end;

       tasnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;

       tisnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;

    var
       ctypeconvnode : class of ttypeconvnode;
       casnode : class of tasnode;
       cisnode : class of tisnode;

    procedure inserttypeconv(var p:tnode;const t:ttype);
    procedure arrayconstructor_to_set(var p : tarrayconstructornode);

implementation

   uses
      globtype,systems,tokens,
      cutils,verbose,globals,widestr,
      symconst,symdef,symsym,symtable,
      ncon,ncal,nset,nadd,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
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
        if is_equal(p.resulttype.def,t.def) then
         begin
           p.resulttype:=t;
         end
        else
         begin
           p:=ctypeconvnode.create(p,t);
           resulttypepass(p);
         end;
      end;


{*****************************************************************************
                    Array constructor to Set Conversion
*****************************************************************************}

    procedure arrayconstructor_to_set(var p : tarrayconstructornode);

      var
        constp      : tsetconstnode;
        buildp,
        p2,p3,p4    : tnode;
        htype       : ttype;
        constset    : pconstset;
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
        var
          mask,l : longint;
        begin
          if (pos>255) or (pos<0) then
           Message(parser_e_illegal_set_expr);
          if pos>constsethi then
           constsethi:=pos;
          if pos<constsetlo then
           constsetlo:=pos;
          { to do this correctly we use the 32bit array }
          l:=pos shr 5;
          mask:=1 shl (pos mod 32);
          { do we allow the same twice }
          if (pconst32bitset(constset)^[l] and mask)<>0 then
           Message(parser_e_illegal_set_expr);
          pconst32bitset(constset)^[l]:=pconst32bitset(constset)^[l] or mask;
        end;

      var
        l : longint;
        lr,hr : longint;

      begin
        new(constset);
        FillChar(constset^,sizeof(constset^),0);
        htype.reset;
        constsetlo:=0;
        constsethi:=0;
        constp:=csetconstnode.create(nil,htype);
        constp.value_set:=constset;
        buildp:=constp;
        if assigned(p.left) then
         begin
           while assigned(p) do
            begin
              p4:=nil; { will contain the tree to create the set }
            {split a range into p2 and p3 }
              if p.left.nodetype=arrayconstructorrangen then
               begin
                 p2:=tarrayconstructorrangenode(p.left).left;
                 p3:=tarrayconstructorrangenode(p.left).right;
                 tarrayconstructorrangenode(p.left).left:=nil;
                 tarrayconstructorrangenode(p.left).right:=nil;
               end
              else
               begin
                 p2:=p.left;
                 p.left:=nil;
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
              p2:=p;
              p:=tarrayconstructornode(tarrayconstructornode(p2).right);
              tarrayconstructornode(p2).right:=nil;
              p2.free;
            end;
          if (htype.def=nil) then
            begin
               htype:=u8bittype;
               constsethi:=255;
            end;
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
        p:=tarrayconstructornode(buildp);
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
            if (sizeof(tordconstnode) > sizeof(tpointerord)) then
              if (sizeof(tpointerord) = 4) then
                begin
                  if (tordconstnode(left).value < low(longint)) or
                     (tordconstnode(left).value > high(cardinal)) then
                  CGMessage(parser_e_range_check_error);
                end
              else if (sizeof(tpointerord) = 8) then
                begin
                  if (tordconstnode(left).value < low(int64)) or
                     (tordconstnode(left).value > high(qword)) then
                  CGMessage(parser_e_range_check_error);
                end
              else
                internalerror(2001020801);
            t:=cpointerconstnode.create(tpointerord(tordconstnode(left).value),resulttype);
            resulttypepass(t);
            result:=t;
          end
         else
          internalerror(200104023);
      end;


    function ttypeconvnode.resulttype_string_to_string : tnode;
      var
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
          end;
      end;


    function ttypeconvnode.resulttype_char_to_string : tnode;
      var
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
              resulttypepass(hp);
              result:=hp;
           end;
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
                      ord(unicode2asciichar(tcompilerwidechar(tordconstnode(left).value))),cchartype);
                resulttypepass(hp);
                result:=hp;
              end
             else if (torddef(resulttype.def).typ=uwidechar) and
                     (torddef(left.resulttype.def).typ=uchar) then
              begin
                hp:=cordconstnode.create(
                      asciichar2unicode(chr(tordconstnode(left).value)),cwidechartype);
                resulttypepass(hp);
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
           resulttypepass(t);
           result:=t;
           exit;
         end;
      end;


    function ttypeconvnode.resulttype_real_to_real : tnode;
      var
        t : tnode;
      begin
         result:=nil;
         if left.nodetype=realconstn then
           begin
             t:=crealconstnode.create(trealconstnode(left).value_real,resulttype);
             resulttypepass(t);
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
        arrayconstructor_to_set(tarrayconstructornode(hp));
      { now resulttypepass the set }
        resulttypepass(hp);
        result:=hp;
      end;


    function ttypeconvnode.resulttype_call_helper(c : tconverttype) : tnode;

      const
         resulttypeconvert : array[tconverttype] of pointer = (
          {equal} nil,
          {not_possible} nil,
          { string_2_string } @ttypeconvnode.resulttype_string_to_string,
          { char_2_string } @ttypeconvnode.resulttype_char_to_string,
          { pchar_2_string } nil,
          { cchar_2_pchar } @ttypeconvnode.resulttype_cchar_to_pchar,
          { cstring_2_pchar } @ttypeconvnode.resulttype_cstring_to_pchar,
          { ansistring_2_pchar } nil,
          { string_2_chararray } nil,
          { chararray_2_string } nil,
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
          { intf_2_guid } nil,
          { class_2_intf } nil,
          { char_2_char } @ttypeconvnode.resulttype_char_to_char
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
               (tsetdef(resulttype.def).settype<>smallset) and
               (tsetdef(left.resulttype.def).settype=smallset) then
             begin
             { try to define the set as a normalset if it's a constant set }
               if left.nodetype=setconstn then
                begin
                  resulttype:=left.resulttype;
                  tsetdef(resulttype.def).settype:=normset
                end
               else
                convtype:=tc_load_smallset;
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
             procinfo^.flags:=procinfo^.flags or pi_do_call;
             hp:=ccallnode.create(ccallparanode.create(left,nil),
                                  overloaded_operators[_assignment],nil,nil);
             { tell explicitly which def we must use !! (PM) }
             tcallnode(hp).procdefinition:=aprocdef;
             left:=nil;
             resulttypepass(hp);
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
                    hp:=cloadnode.create(tprocsym(tcallnode(left).symtableprocentry),
                        tcallnode(left).symtableproc);
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
                      aprocdef:=tprocsym(tloadnode(left).symtableentry).definition;
                  end;
                 convtype:=tc_proc_2_procvar;
                 { Now check if the procedure we are going to assign to
                   the procvar,  is compatible with the procvar's type }
                 if assigned(aprocdef) then
                  begin
                    if not proc_to_procvar_equal(aprocdef,tprocvardef(resulttype.def)) then
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
                    hp:=cordconstnode.create(tordconstnode(left).value,resulttype);
                    resulttypepass(hp);
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
                     hp:=cordconstnode.create(tordconstnode(left).value,resulttype);
                     resulttypepass(hp);
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
                     hp:=cordconstnode.create(0,resulttype);
                     resulttypepass(hp);
                     result:=hp;
                     exit;
                  end

              { constant pointer to ordinal }
              else if is_ordinal(resulttype.def) and
                (left.nodetype=pointerconstn) then
                begin
                   hp:=cordconstnode.create(tpointerconstnode(left).value,resulttype);
                   resulttypepass(hp);
                   result:=hp;
                   exit;
                end

              {Are we typecasting an ordconst to a char?}
              else
                if is_char(resulttype.def) and
                   is_ordinal(left.resulttype.def) then
                 begin
                   if left.nodetype=ordconstn then
                    begin
                      hp:=cordconstnode.create(tordconstnode(left).value,resulttype);
                      resulttypepass(hp);
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
                      hp:=cordconstnode.create(tordconstnode(left).value,resulttype);
                      resulttypepass(hp);
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
                      hp:=cordconstnode.create(tordconstnode(left).value,resulttype);
                      resulttypepass(hp);
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
                      hp:=cordconstnode.create(tordconstnode(left).value,resulttype);
                      resulttypepass(hp);
                      result:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(u16bittype.def,resulttype.def,convtype,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype.def.typename,resulttype.def.typename);
                    end;
                 end

               { only if the same size or formal def }
               { why do we allow typecasting of voiddef ?? (PM) }
               else
                begin
                  if not(
                     (left.resulttype.def.deftype=formaldef) or
                     (left.resulttype.def.size=resulttype.def.size) or
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

        { remove typeconv after niln }
        if (left.nodetype=niln) then
          begin
            left.resulttype:=resulttype;
            result:=left;
            left:=nil;
            exit;
          end;

        { ordinal contants can be directly converted }
        if (left.nodetype=ordconstn) and is_ordinal(resulttype.def)  then
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
             resulttypepass(hp);
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


    function ttypeconvnode.first_string_to_string : tnode;
      begin
         first_string_to_string:=nil;
         if tstringdef(resulttype.def).string_typ<>
            tstringdef(left.resulttype.def).string_typ then
           begin
             procinfo^.flags:=procinfo^.flags or pi_do_call;
           end;
         { for simplicity lets first keep all ansistrings
           as LOC_MEM, could also become LOC_REGISTER }
         if tstringdef(resulttype.def).string_typ in [st_ansistring,st_widestring] then
           { we may use ansistrings so no fast exit here }
           procinfo^.no_fast_exit:=true;
         location.loc:=LOC_MEM;
      end;


    function ttypeconvnode.first_char_to_string : tnode;
      begin
         first_char_to_string:=nil;
         location.loc:=LOC_MEM;
      end;


    function ttypeconvnode.first_nothing : tnode;
      begin
         first_nothing:=nil;
         location.loc:=LOC_MEM;
      end;


    function ttypeconvnode.first_array_to_pointer : tnode;
      begin
         first_array_to_pointer:=nil;
         if registers32<1 then
           registers32:=1;
         location.loc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_int_to_real : tnode;
      begin
        first_int_to_real:=nil;
{$ifdef m68k}
         if (cs_fp_emulation in aktmoduleswitches) or
            (tfloatdef(resulttype.def).typ=s32real) then
           begin
             if registers32<1 then
               registers32:=1;
           end
         else
           if registersfpu<1 then
             registersfpu:=1;
{$else not m68k}
         if registersfpu<1 then
          registersfpu:=1;
{$endif not m68k}
        location.loc:=LOC_FPU;
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
         location.loc:=LOC_FPU;
      end;


    function ttypeconvnode.first_pointer_to_array : tnode;
      begin
         first_pointer_to_array:=nil;
         if registers32<1 then
           registers32:=1;
         location.loc:=LOC_REFERENCE;
      end;


    function ttypeconvnode.first_chararray_to_string : tnode;
      begin
         first_chararray_to_string:=nil;
         { the only important information is the location of the }
         { result                                               }
         { other stuff is done by firsttypeconv           }
         location.loc:=LOC_MEM;
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
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           exit;
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
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
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
      begin
         first_load_smallset:=nil;
      end;


    function ttypeconvnode.first_pchar_to_string : tnode;
      begin
         first_pchar_to_string:=nil;
         location.loc:=LOC_REFERENCE;
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

    function ttypeconvnode.first_call_helper(c : tconverttype) : tnode;

      const
         firstconvert : array[tconverttype] of pointer = (
           @ttypeconvnode.first_nothing, {equal}
           @ttypeconvnode.first_nothing, {not_possible}
           @ttypeconvnode.first_string_to_string,
           @ttypeconvnode.first_char_to_string,
           @ttypeconvnode.first_pchar_to_string,
           @ttypeconvnode.first_cchar_to_pchar,
           @ttypeconvnode.first_cstring_to_pchar,
           @ttypeconvnode.first_ansistring_to_pchar,
           @ttypeconvnode.first_string_to_chararray,
           @ttypeconvnode.first_chararray_to_string,
           @ttypeconvnode.first_array_to_pointer,
           @ttypeconvnode.first_pointer_to_array,
           @ttypeconvnode.first_int_to_int,
           @ttypeconvnode.first_int_to_bool,
           @ttypeconvnode.first_bool_to_bool,
           @ttypeconvnode.first_bool_to_int,
           @ttypeconvnode.first_real_to_real,
           @ttypeconvnode.first_int_to_real,
           @ttypeconvnode.first_proc_to_procvar,
           @ttypeconvnode.first_arrayconstructor_to_set,
           @ttypeconvnode.first_load_smallset,
           @ttypeconvnode.first_cord_to_pointer,
           @ttypeconvnode.first_nothing,
           @ttypeconvnode.first_nothing,
           @ttypeconvnode.first_class_to_intf,
           @ttypeconvnode.first_char_to_char
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
        set_location(location,left.location);

        if nf_explizit in flags then
         begin
           { check if the result could be in a register }
           if not(tstoreddef(resulttype.def).is_intregable) and
              not(tstoreddef(resulttype.def).is_fpuregable) then
            make_not_regable(left);
         end;

        if convtype=tc_equal then
         begin
           { remove typeconv node if left is a const. For other nodes we can't
             remove it because the secondpass can still depend on the old type (PFV) }
           if is_constnode(left) then
            begin
              left.resulttype:=resulttype;
              result:=left;
              left:=nil;
            end;
         end
        else
         begin
           result:=first_call_helper(convtype);
         end;
      end;


{*****************************************************************************
                                TISNODE
*****************************************************************************}

    constructor tisnode.create(l,r : tnode);

      begin
         inherited create(isn,l,r);
      end;


    function tisnode.det_resulttype:tnode;
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
                 CGMessage(type_e_mismatch);
             end
            else
             CGMessage(type_e_mismatch);
          end
         else
          CGMessage(type_e_mismatch);

         resulttype:=booltype;
      end;


    function tisnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         left_right_max;

         location.loc:=LOC_FLAGS;
      end;


{*****************************************************************************
                                TASNODE
*****************************************************************************}

    constructor tasnode.create(l,r : tnode);

      begin
         inherited create(asn,l,r);
      end;


    function tasnode.det_resulttype:tnode;
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
                 CGMessage(type_e_mismatch);
             end
            else
             CGMessage(type_e_mismatch);
            resulttype:=tclassrefdef(right.resulttype.def).pointertype;
          end
         else
          CGMessage(type_e_mismatch);
      end;


    function tasnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(right);
         firstpass(left);
         if codegenerror then
           exit;

         left_right_max;

         set_location(location,left.location);
      end;


    function ttypeconvnode.docompare(p: tnode) : boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (convtype = ttypeconvnode(p).convtype);
      end;

begin
   ctypeconvnode:=ttypeconvnode;
   casnode:=tasnode;
   cisnode:=tisnode;
end.
{
  $Log$
  Revision 1.31  2001-08-05 13:19:51  peter
    * partly fix for proc of obj=nil

  Revision 1.30  2001/07/30 20:59:27  peter
    * m68k updates from v10 merged

  Revision 1.29  2001/07/08 21:00:15  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.28  2001/05/13 15:43:46  florian
    * made resultype_char_to_char a little bit robuster

  Revision 1.27  2001/05/08 21:06:30  florian
    * some more support for widechars commited especially
      regarding type casting and constants

  Revision 1.26  2001/05/04 15:52:03  florian
    * some Delphi incompatibilities fixed:
       - out, dispose and new can be used as idenfiers now
       - const p = apointerype(nil); is supported now
    + support for const p = apointertype(pointer(1234)); added

  Revision 1.25  2001/04/13 22:20:58  peter
    * remove wrongly placed first_call_helper

  Revision 1.24  2001/04/13 01:22:08  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.23  2001/04/04 22:42:39  peter
    * move constant folding into det_resulttype

  Revision 1.22  2001/04/02 21:20:30  peter
    * resulttype rewrite

  Revision 1.21  2001/03/08 17:44:47  jonas
    * fixed web bug 1430

  Revision 1.20  2001/02/21 11:49:50  jonas
    * evaluate typecasts of const pointers to ordinals inline ('merged')

  Revision 1.19  2001/02/20 18:37:10  peter
    * removed unused code

  Revision 1.18  2001/02/20 13:14:18  marco
   * Fix from Peter for passing a procedure of method to a other method in a method

  Revision 1.17  2001/02/08 13:09:03  jonas
    * fixed web bug 1396: tpointerord is now a cardinal instead of a longint,
      but added a hack in ncnv so that pointer(-1) still works

  Revision 1.16  2000/12/31 11:14:10  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.15  2000/12/08 12:41:01  jonas
    * fixed bug in sign extension patch

  Revision 1.14  2000/12/07 17:19:42  jonas
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

  Revision 1.13  2000/11/29 00:30:32  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.12  2000/11/20 16:06:04  jonas
    + allow evaluation of 64bit constant expressions at compile time
    * disable range checking for explicit typecasts of constant expressions

  Revision 1.11  2000/11/12 23:24:11  florian
    * interfaces are basically running

  Revision 1.10  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.9  2000/10/31 22:02:48  peter
    * symtable splitted, no real code changes

  Revision 1.8  2000/10/14 21:52:55  peter
    * fixed memory leaks

  Revision 1.7  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.6  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.5  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.4  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.3  2000/09/26 20:06:13  florian
    * hmm, still a lot of work to get things compilable

  Revision 1.2  2000/09/26 14:59:34  florian
    * more conversion work done

  Revision 1.1  2000/09/25 15:37:14  florian
    * more fixes
}

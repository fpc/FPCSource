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
          convtype : tconverttype;
          constructor create(node : tnode;t : pdef);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function first_int_to_int : tnode;virtual;
          function first_cstring_to_pchar : tnode;virtual;
          function first_string_to_chararray : tnode;virtual;
          function first_string_to_string : tnode;virtual;
          function first_char_to_string : tnode;virtual;
          function first_nothing : tnode;virtual;
          function first_array_to_pointer : tnode;virtual;
          function first_int_to_real : tnode;virtual;
          function first_int_to_fix : tnode;virtual;
          function first_real_to_fix : tnode;virtual;
          function first_fix_to_real : tnode;virtual;
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
          function first_call_helper(c : tconverttype) : tnode;
       end;

       tasnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tisnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

    var
       ctypeconvnode : class of ttypeconvnode;
       casnode : class of tasnode;
       cisnode : class of tisnode;

    function gentypeconvnode(node : tnode;t : pdef) : ttypeconvnode;
    procedure arrayconstructor_to_set(var p : tarrayconstructornode);

implementation

   uses
      globtype,systems,tokens,
      cutils,cobjects,verbose,globals,
      symconst,symdef,symsym,symtable,aasm,
      ncon,ncal,nset,nadd,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      htypechk,pass_1,cpubase;


    function gentypeconvnode(node : tnode;t : pdef) : ttypeconvnode;

      begin
         gentypeconvnode:=ctypeconvnode.create(node,t);
      end;

{*****************************************************************************
                    Array constructor to Set Conversion
*****************************************************************************}

    procedure arrayconstructor_to_set(var p : tarrayconstructornode);

      var
        constp      : tsetconstnode;
        buildp,
        p2,p3,p4    : tnode;
        pd        : pdef;
        constset    : pconstset;
        constsetlo,
        constsethi  : longint;

        procedure update_constsethi(p:pdef);
        begin
          if ((p^.deftype=orddef) and
             (porddef(p)^.high>=constsethi)) then
            begin
               constsethi:=porddef(p)^.high;
               if pd=nil then
                 begin
                    if (constsethi>255) or
                      (porddef(p)^.low<0) then
                      pd:=u8bitdef
                    else
                      pd:=p;
                 end;
               if constsethi>255 then
                 constsethi:=255;
            end
          else if ((p^.deftype=enumdef) and
            (penumdef(p)^.max>=constsethi)) then
            begin
               if pd=nil then
                 pd:=p;
               constsethi:=penumdef(p)^.max;
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
          l:=pos shr 3;
          mask:=1 shl (pos mod 8);
          { do we allow the same twice }
          if (constset^[l] and mask)<>0 then
           Message(parser_e_illegal_set_expr);
          constset^[l]:=constset^[l] or mask;
        end;

      var
        l : longint;
        lr,hr : longint;

      begin
        new(constset);
        FillChar(constset^,sizeof(constset^),0);
        pd:=nil;
        constsetlo:=0;
        constsethi:=0;
        constp:=csetconstnode.create(nil,nil);
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
              firstpass(p2);
              if assigned(p3) then
               firstpass(p3);
              if codegenerror then
               break;
              case p2.resulttype^.deftype of
                 enumdef,
                 orddef:
                   begin
                      getrange(p2.resulttype,lr,hr);
                      if assigned(p3) then
                       begin
                         { this isn't good, you'll get problems with
                           type t010 = 0..10;
                                ts = set of t010;
                           var  s : ts;b : t010
                           begin  s:=[1,2,b]; end.
                         if is_integer(p3^.resulttype) then
                          begin
                            p3:=gentypeconvnode(p3,u8bitdef);
                            firstpass(p3);
                          end;
                         }
                         if assigned(pd) and not(is_equal(pd,p3.resulttype)) then
                           begin
                              aktfilepos:=p3.fileinfo;
                              CGMessage(type_e_typeconflict_in_set);
                           end
                         else
                           begin
                             if (p2.nodetype=ordconstn) and (p3.nodetype=ordconstn) then
                              begin
                                 if not(is_integer(p3.resulttype)) then
                                   pd:=p3.resulttype
                                 else
                                   begin
                                      p3:=gentypeconvnode(p3,u8bitdef);
                                      p2:=gentypeconvnode(p2,u8bitdef);
                                      firstpass(p2);
                                      firstpass(p3);
                                   end;

                                for l:=tordconstnode(p2).value to tordconstnode(p3).value do
                                  do_set(l);
                                p2.free;
                                p3.free;
                              end
                             else
                              begin
                                update_constsethi(p2.resulttype);
                                p2:=gentypeconvnode(p2,pd);
                                firstpass(p2);

                                update_constsethi(p3.resulttype);
                                p3:=gentypeconvnode(p3,pd);
                                firstpass(p3);


                                if assigned(pd) then
                                  p3:=gentypeconvnode(p3,pd)
                                else
                                  p3:=gentypeconvnode(p3,u8bitdef);
                                firstpass(p3);
                                p4:=csetelementnode.create(p2,p3);
                              end;
                           end;
                       end
                      else
                       begin
                      { Single value }
                         if p2.nodetype=ordconstn then
                          begin
                            if not(is_integer(p2.resulttype)) then
                              update_constsethi(p2.resulttype)
                            else
                              begin
                                 p2:=gentypeconvnode(p2,u8bitdef);
                                 firstpass(p2);
                              end;

                            do_set(tordconstnode(p2).value);
                            p2.free;
                          end
                         else
                          begin
                            update_constsethi(p2.resulttype);

                            if assigned(pd) then
                              p2:=gentypeconvnode(p2,pd)
                            else
                              p2:=gentypeconvnode(p2,u8bitdef);
                            firstpass(p2);

                            p4:=csetelementnode.create(p2,nil);
                          end;
                       end;
                    end;
          stringdef : begin
                        { if we've already set elements which are constants }
                        { throw an error                                    }
                        if ((pd=nil) and assigned(buildp)) or
                          not(is_equal(pd,cchardef)) then
                          CGMessage(type_e_typeconflict_in_set)
                        else
                         for l:=1 to length(pstring(tstringconstnode(p2).value_str)^) do
                          do_set(ord(pstring(tstringconstnode(p2).value_str)^[l]));
                        if pd=nil then
                         pd:=cchardef;
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
          if (pd=nil) then
            begin
               pd:=u8bitdef;
               constsethi:=255;
            end;
         end
        else
         begin
         { empty set [], only remove node }
           p.free;
         end;
      { set the initial set type }
        constp.resulttype:=new(psetdef,init(pd,constsethi));
      { set the new tree }
        p:=tarrayconstructornode(buildp);
      end;


{*****************************************************************************
                           TTYPECONVNODE
*****************************************************************************}


    constructor ttypeconvnode.create(node : tnode;t : pdef);

      begin
         inherited create(typeconvn,node);
         convtype:=tc_not_possible;
         resulttype:=t;
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


    function ttypeconvnode.first_int_to_int : tnode;
      begin
        first_int_to_int:=nil;
        if (left.location.loc<>LOC_REGISTER) and
           (resulttype^.size>left.resulttype^.size) then
           location.loc:=LOC_REGISTER;
        if is_64bitint(resulttype) then
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
         if pstringdef(resulttype)^.string_typ<>
            pstringdef(left.resulttype)^.string_typ then
           begin
              if left.nodetype=stringconstn then
                begin
                   tstringconstnode(left).stringtype:=pstringdef(resulttype)^.string_typ;
                   tstringconstnode(left).resulttype:=resulttype;
                   { remove typeconv node }
                   first_string_to_string:=left;
                   left:=nil;
                   exit;
                end
              else
                procinfo^.flags:=procinfo^.flags or pi_do_call;
           end;
         { for simplicity lets first keep all ansistrings
           as LOC_MEM, could also become LOC_REGISTER }
         if pstringdef(resulttype)^.string_typ in [st_ansistring,st_widestring] then
           { we may use ansistrings so no fast exit here }
           procinfo^.no_fast_exit:=true;
         location.loc:=LOC_MEM;
      end;


    function ttypeconvnode.first_char_to_string : tnode;
      var
         hp : tstringconstnode;
      begin
         first_char_to_string:=nil;
         if left.nodetype=ordconstn then
           begin
              hp:=genstringconstnode(chr(tordconstnode(left).value),st_default);
              hp.stringtype:=pstringdef(resulttype)^.string_typ;
              firstpass(hp);
              first_char_to_string:=hp;
           end
         else
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
      var
        t : trealconstnode;
      begin
        first_int_to_real:=nil;
        if left.nodetype=ordconstn then
         begin
           t:=genrealconstnode(tordconstnode(left).value,pfloatdef(resulttype));
           firstpass(t);
           first_int_to_real:=t;
           exit;
         end;
        if registersfpu<1 then
         registersfpu:=1;
        location.loc:=LOC_FPU;
      end;


    function ttypeconvnode.first_int_to_fix : tnode;
      var
        t : tnode;
      begin
        first_int_to_fix:=nil;
        if left.nodetype=ordconstn then
         begin
           t:=genfixconstnode(tordconstnode(left).value shl 16,resulttype);
           firstpass(t);
           first_int_to_fix:=t;
           exit;
         end;
        if registers32<1 then
         registers32:=1;
        location.loc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_real_to_fix : tnode;
      var
        t : tnode;
      begin
        first_real_to_fix:=nil;
        if left.nodetype=realconstn then
         begin
           t:=genfixconstnode(round(trealconstnode(left).value_real*65536),resulttype);
           firstpass(t);
           first_real_to_fix:=t;
           exit;
         end;
        { at least one fpu and int register needed }
        if registers32<1 then
          registers32:=1;
        if registersfpu<1 then
          registersfpu:=1;
        location.loc:=LOC_REGISTER;
      end;


    function ttypeconvnode.first_fix_to_real : tnode;
      var
        t : tnode;
      begin
        first_fix_to_real:=nil;
        if left.nodetype=fixconstn then
          begin
            t:=genrealconstnode(round(tfixconstnode(left).value_fix/65536.0),resulttype);
            firstpass(t);
            first_fix_to_real:=t;
            exit;
          end;
        if registersfpu<1 then
          registersfpu:=1;
        location.loc:=LOC_FPU;
      end;


    function ttypeconvnode.first_real_to_real : tnode;
      var
        t : tnode;
      begin
         first_real_to_real:=nil;
         if left.nodetype=realconstn then
           begin
             t:=genrealconstnode(trealconstnode(left).value_real,resulttype);
             firstpass(t);
             first_real_to_real:=t;
             exit;
           end;
        { comp isn't a floating type }
{$ifdef i386}
         if (pfloatdef(resulttype)^.typ=s64comp) and
            (pfloatdef(left.resulttype)^.typ<>s64comp) and
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
         left:=gentypeconvnode(left,cshortstringdef);
         { convert constant char to constant string }
         firstpass(left);
         { evalute tree }
         first_cchar_to_pchar:=pass_1;
      end;


    function ttypeconvnode.first_bool_to_int : tnode;
      begin
         first_bool_to_int:=nil;
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (nf_explizit in flags) and
            (left.resulttype^.size=resulttype^.size) and
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
            (left.resulttype^.size=resulttype^.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           exit;
         location.loc:=LOC_REGISTER;
         { need if bool to bool !!
           not very nice !!
         left:=gentypeconvnode(left,s32bitdef);
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


    function ttypeconvnode.first_proc_to_procvar : tnode;
      begin
         first_proc_to_procvar:=nil;
         { hmmm, I'am not sure if that is necessary (FK) }
         firstpass(left);
         if codegenerror then
           exit;

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


    function ttypeconvnode.first_cord_to_pointer : tnode;
      var
        t : tnode;
      begin
        first_cord_to_pointer:=nil;
        if left.nodetype=ordconstn then
          begin
            t:=genpointerconstnode(tordconstnode(left).value,resulttype);
            firstpass(t);
            first_cord_to_pointer:=t;
            exit;
          end
        else
          internalerror(432472389);
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
      var
        hp : tnode;
      begin
        first_arrayconstructor_to_set:=nil;
        if left.nodetype<>arrayconstructorn then
         internalerror(5546);
      { remove typeconv node }
        hp:=left;
        left:=nil;
      { create a set constructor tree }
        arrayconstructor_to_set(tarrayconstructornode(hp));
      { now firstpass the set }
        firstpass(hp);
        first_arrayconstructor_to_set:=hp;
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
           @ttypeconvnode.first_int_to_fix,
           @ttypeconvnode.first_real_to_fix,
           @ttypeconvnode.first_fix_to_real,
           @ttypeconvnode.first_proc_to_procvar,
           @ttypeconvnode.first_arrayconstructor_to_set,
           @ttypeconvnode.first_load_smallset,
           @ttypeconvnode.first_cord_to_pointer,
           @ttypeconvnode.first_nothing,
           @ttypeconvnode.first_nothing,
           @ttypeconvnode.first_class_to_intf
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
    var
      hp : tnode;
      aprocdef : pprocdef;
     begin
       pass_1:=nil;
       aprocdef:=nil;
       { if explicite type cast, then run firstpass }
       if (nf_explizit in flags) or not assigned(left.resulttype) then
         firstpass(left);
       if (left.nodetype=typen) and (left.resulttype=generrordef) then
         begin
            codegenerror:=true;
            Message(parser_e_no_type_not_allowed_here);
         end;
       if codegenerror then
         begin
           resulttype:=generrordef;
           exit;
         end;

       if not assigned(left.resulttype) then
        begin
          codegenerror:=true;
          internalerror(52349);
          exit;
        end;

       { load the value_str from the left part }
       registers32:=left.registers32;
       registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
       registersmmx:=left.registersmmx;
{$endif}
       set_location(location,left.location);

       { remove obsolete type conversions }
       if is_equal(left.resulttype,resulttype) then
         begin
         { becuase is_equal only checks the basetype for sets we need to
           check here if we are loading a smallset into a normalset }
           if (resulttype^.deftype=setdef) and
              (left.resulttype^.deftype=setdef) and
              (psetdef(resulttype)^.settype<>smallset) and
              (psetdef(left.resulttype)^.settype=smallset) then
            begin
            { try to define the set as a normalset if it's a constant set }
              if left.nodetype=setconstn then
               begin
                 resulttype:=left.resulttype;
                 psetdef(resulttype)^.settype:=normset
               end
              else
               convtype:=tc_load_smallset;
              exit;
            end
           else
            begin
              pass_1:=left;
              left.resulttype:=resulttype;
              left:=nil;
              exit;
            end;
         end;
       aprocdef:=assignment_overloaded(left.resulttype,resulttype);
       if assigned(aprocdef) then
         begin
            procinfo^.flags:=procinfo^.flags or pi_do_call;
            hp:=gencallnode(overloaded_operators[_assignment],nil);
            { tell explicitly which def we must use !! (PM) }
            tcallnode(hp).procdefinition:=aprocdef;
            tcallnode(hp).left:=gencallparanode(left,nil);
            left:=nil;
            firstpass(hp);
            pass_1:=hp;
            exit;
         end;

       if isconvertable(left.resulttype,resulttype,convtype,left,left.nodetype,nf_explizit in flags)=0 then
         begin
           {Procedures have a resulttype of voiddef and functions of their
           own resulttype. They will therefore always be incompatible with
           a procvar. Because isconvertable cannot check for procedures we
           use an extra check for them.}
           if (m_tp_procvar in aktmodeswitches) then
            begin
              if (resulttype^.deftype=procvardef) and
                 (is_procsym_load(left) or is_procsym_call(left)) then
               begin
                 if is_procsym_call(left) then
                  begin
                    {if left.right=nil then
                     begin}
                       if (tcallnode(left).symtableprocentry^.owner^.symtabletype=objectsymtable){ and
                          (pobjectdef(left.symtableprocentry^.owner^.defowner)^.is_class) }then
                        hp:=genloadmethodcallnode(pprocsym(tcallnode(left).symtableprocentry),
                          tcallnode(left).symtableproc,
                              tcallnode(left).methodpointer.getcopy)
                       else
                        hp:=genloadcallnode(pprocsym(tcallnode(left).symtableprocentry),
                          tcallnode(left).symtableproc);
                       firstpass(hp);
                       left.free;
                       left:=hp;
                       aprocdef:=pprocdef(left.resulttype);
                   (*  end
                    else
                     begin
                       left.right.nodetype:=loadn;
                       left.right.symtableentry:=left.right.symtableentry;
                       left.right.resulttype:=pvarsym(left.symtableentry)^.definition;
                       hp:=left.right;
                       putnode(left);
                       left:=hp;
                       { should we do that ? }
                       firstpass(left);
                       if not is_equal(left.resulttype,resulttype) then
                        begin
                          CGMessage(type_e_mismatch);
                          exit;
                        end
                       else
                        begin
                          hp:=p;
                          p:=left;
                          resulttype:=hp.resulttype;
                          putnode(hp);
                          exit;
                        end;
                     end; *)
                  end
                 else
                  begin
                    if (left.nodetype<>addrn) then
                      aprocdef:=pprocsym(tloadnode(left).symtableentry)^.definition;
                  end;
                 convtype:=tc_proc_2_procvar;
                 { Now check if the procedure we are going to assign to
                   the procvar,  is compatible with the procvar's type }
                 if assigned(aprocdef) then
                  begin
                    if not proc_to_procvar_equal(aprocdef,pprocvardef(resulttype)) then
                     CGMessage2(type_e_incompatible_types,aprocdef^.typename,resulttype^.typename);
                    pass_1:=first_call_helper(convtype);
                  end
                 else
                  CGMessage2(type_e_incompatible_types,left.resulttype^.typename,resulttype^.typename);
                 exit;
               end;
            end;
           if nf_explizit in flags then
            begin
              { check if the result could be in a register }
              if not(pstoreddef(resulttype)^.is_intregable) and
                not(pstoreddef(resulttype)^.is_fpuregable) then
                make_not_regable(left);
              { boolean to byte are special because the
                location can be different }

              if is_integer(resulttype) and
                 is_boolean(left.resulttype) then
               begin
                  convtype:=tc_bool_2_int;
                  pass_1:=first_call_helper(convtype);
                  exit;
               end;
              { ansistring to pchar }
              if is_pchar(resulttype) and
                 is_ansistring(left.resulttype) then
               begin
                 convtype:=tc_ansistring_2_pchar;
                 pass_1:=first_call_helper(convtype);
                 exit;
               end;
              { do common tc_equal cast }
              convtype:=tc_equal;

              { enum to ordinal will always be s32bit }
              if (left.resulttype^.deftype=enumdef) and
                 is_ordinal(resulttype) then
               begin
                 if left.nodetype=ordconstn then
                  begin
                    hp:=genordinalconstnode(tordconstnode(left).value,resulttype);
                    firstpass(hp);
                    pass_1:=hp;
                    exit;
                  end
                 else
                  begin
                    if isconvertable(s32bitdef,resulttype,convtype,nil,ordconstn,false)=0 then
                      CGMessage2(type_e_incompatible_types,left.resulttype^.typename,resulttype^.typename);
                  end;
               end

              { ordinal to enumeration }
              else
               if (resulttype^.deftype=enumdef) and
                  is_ordinal(left.resulttype) then
                begin
                  if left.nodetype=ordconstn then
                   begin
                     hp:=genordinalconstnode(tordconstnode(left).value,resulttype);
                     firstpass(hp);
                     pass_1:=hp;
                     exit;
                   end
                  else
                   begin
                     if IsConvertable(left.resulttype,s32bitdef,convtype,nil,ordconstn,false)=0 then
                       CGMessage2(type_e_incompatible_types,left.resulttype^.typename,resulttype^.typename);
                   end;
                end

              { nil to ordinal node }
              else if is_ordinal(resulttype) and
                (left.nodetype=niln) then
                begin
                   hp:=genordinalconstnode(0,resulttype);
                   firstpass(hp);
                   pass_1:=hp;
                   exit;
                end

              {Are we typecasting an ordconst to a char?}
              else
                if is_char(resulttype) and
                   is_ordinal(left.resulttype) then
                 begin
                   if left.nodetype=ordconstn then
                    begin
                      hp:=genordinalconstnode(tordconstnode(left).value,resulttype);
                      firstpass(hp);
                      pass_1:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(left.resulttype,u8bitdef,convtype,nil,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype^.typename,resulttype^.typename);
                    end;
                 end

              { Are we char to ordinal }
              else
                if is_char(left.resulttype) and
                   is_ordinal(resulttype) then
                 begin
                   if left.nodetype=ordconstn then
                    begin
                      hp:=genordinalconstnode(tordconstnode(left).value,resulttype);
                      firstpass(hp);
                      pass_1:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(u8bitdef,resulttype,convtype,nil,ordconstn,false)=0 then
                        CGMessage2(type_e_incompatible_types,left.resulttype^.typename,resulttype^.typename);
                    end;
                 end

               { only if the same size or formal def }
               { why do we allow typecasting of voiddef ?? (PM) }
               else
                begin
                  if not(
                     (left.resulttype^.deftype=formaldef) or
                     (left.resulttype^.size=resulttype^.size) or
                     (is_equal(left.resulttype,voiddef)  and
                     (left.nodetype=derefn))
                     ) then
                    CGMessage(cg_e_illegal_type_conversion);
                  if ((left.resulttype^.deftype=orddef) and
                      (resulttype^.deftype=pointerdef)) or
                      ((resulttype^.deftype=orddef) and
                       (left.resulttype^.deftype=pointerdef))
                       {$ifdef extdebug}and (firstpasscount=0){$endif} then
                    CGMessage(cg_d_pointer_to_longint_conv_not_portable);
                end;

               { the conversion into a strutured type is only }
               { possible, if the source is no register    }
               if ((resulttype^.deftype in [recorddef,stringdef,arraydef]) or
                   ((resulttype^.deftype=objectdef) and not(is_class(resulttype)))
                  ) and (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) { and
                   it also works if the assignment is overloaded
                   YES but this code is not executed if assignment is overloaded (PM)
                  not assigned(assignment_overloaded(left.resulttype,resulttype))} then
                 CGMessage(cg_e_illegal_type_conversion);
            end
           else
            CGMessage2(type_e_incompatible_types,left.resulttype^.typename,resulttype^.typename);
         end;

       { tp7 procvar support, when right is not a procvardef and we got a
         loadn of a procvar then convert to a calln, the check for the
         result is already done in is_convertible, also no conflict with
         @procvar is here because that has an extra addrn }
         if (m_tp_procvar in aktmodeswitches) and
            (resulttype^.deftype<>procvardef) and
            (left.resulttype^.deftype=procvardef) and
            (left.nodetype=loadn) then
          begin
            hp:=gencallnode(nil,nil);
            tcallnode(hp).right:=left;
            firstpass(hp);
            left:=hp;
          end;


        { ordinal contants can be directly converted }
        { but not int64/qword                        }
        if (left.nodetype=ordconstn) and is_ordinal(resulttype) and
          not(is_64bitint(resulttype)) then
          begin
             { range checking is done in genordinalconstnode (PFV) }
             hp:=genordinalconstnode(tordconstnode(left).value,resulttype);
             firstpass(hp);
             pass_1:=hp;
             exit;
          end;
        if convtype<>tc_equal then
          pass_1:=first_call_helper(convtype);
      end;


{*****************************************************************************
                                TISNODE
*****************************************************************************}

    constructor tisnode.create(l,r : tnode);

      begin
         inherited create(isn,l,r);
      end;

    function tisnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(left,true);
         firstpass(right);
         set_varstate(right,true);
         if codegenerror then
           exit;

         if (right.resulttype^.deftype<>classrefdef) then
           CGMessage(type_e_mismatch);

         left_right_max;

         { left must be a class }
         if (left.resulttype^.deftype<>objectdef) or
            not(is_class(left.resulttype)) then
           CGMessage(type_e_mismatch);

         { the operands must be related }
         if (not(pobjectdef(left.resulttype)^.is_related(
           pobjectdef(pclassrefdef(right.resulttype)^.pointertype.def)))) and
           (not(pobjectdef(pclassrefdef(right.resulttype)^.pointertype.def)^.is_related(
           pobjectdef(left.resulttype)))) then
           CGMessage(type_e_mismatch);

         location.loc:=LOC_FLAGS;
         resulttype:=booldef;
      end;


{*****************************************************************************
                                TASNODE
*****************************************************************************}

    constructor tasnode.create(l,r : tnode);

      begin
         inherited create(asn,l,r);
      end;

    function tasnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         firstpass(right);
         set_varstate(right,true);
         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         if (right.resulttype^.deftype<>classrefdef) then
           CGMessage(type_e_mismatch);

         left_right_max;

         { left must be a class }
         if (left.resulttype^.deftype<>objectdef) or
           not(is_class(left.resulttype)) then
           CGMessage(type_e_mismatch);

         { the operands must be related }
         if (not(pobjectdef(left.resulttype)^.is_related(
           pobjectdef(pclassrefdef(right.resulttype)^.pointertype.def)))) and
           (not(pobjectdef(pclassrefdef(right.resulttype)^.pointertype.def)^.is_related(
           pobjectdef(left.resulttype)))) then
           CGMessage(type_e_mismatch);

         set_location(location,left.location);
         resulttype:=pclassrefdef(right.resulttype)^.pointertype.def;
      end;


begin
   ctypeconvnode:=ttypeconvnode;
   casnode:=tasnode;
   cisnode:=tisnode;
end.
{
  $Log$
  Revision 1.11  2000-11-12 23:24:11  florian
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
{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
{$ifdef TP}
  {$E+,F+,N+,D+,L+,Y+}
{$endif}
unit tccnv;
interface

    uses
      tree;

    procedure arrayconstructor_to_set(var p:ptree);

    procedure firsttypeconv(var p : ptree);
    procedure firstas(var p : ptree);
    procedure firstis(var p : ptree);


implementation

   uses
      globtype,systems,tokens,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      htypechk,pass_1,cpubase;


{*****************************************************************************
                    Array constructor to Set Conversion
*****************************************************************************}

    procedure arrayconstructor_to_set(var p:ptree);
      var
        constp,
        buildp,
        p2,p3,p4    : ptree;
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
        constp:=gensinglenode(setconstn,nil);
        constp^.value_set:=constset;
        buildp:=constp;
        if assigned(p^.left) then
         begin
           while assigned(p) do
            begin
              p4:=nil; { will contain the tree to create the set }
            { split a range into p2 and p3 }
              if p^.left^.treetype=arrayconstructrangen then
               begin
                 p2:=p^.left^.left;
                 p3:=p^.left^.right;
               { node is not used anymore }
                 putnode(p^.left);
               end
              else
               begin
                 p2:=p^.left;
                 p3:=nil;
               end;
              firstpass(p2);
              if assigned(p3) then
               firstpass(p3);
              if codegenerror then
               break;
              case p2^.resulttype^.deftype of
                 enumdef,
                 orddef:
                   begin
                      getrange(p2^.resulttype,lr,hr);
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

                         if assigned(pd) and not(is_equal(pd,p3^.resulttype)) then
                           begin
                              aktfilepos:=p3^.fileinfo;
                              CGMessage(type_e_typeconflict_in_set);
                           end
                         else
                           begin
                             if (p2^.treetype=ordconstn) and (p3^.treetype=ordconstn) then
                              begin
                                 if not(is_integer(p3^.resulttype)) then
                                   pd:=p3^.resulttype
                                 else
                                   begin
                                      p3:=gentypeconvnode(p3,u8bitdef);
                                      p2:=gentypeconvnode(p2,u8bitdef);
                                      firstpass(p2);
                                      firstpass(p3);
                                   end;

                                for l:=p2^.value to p3^.value do
                                  do_set(l);
                                disposetree(p3);
                                disposetree(p2);
                              end
                             else
                              begin
                                update_constsethi(p2^.resulttype);
                                p2:=gentypeconvnode(p2,pd);
                                firstpass(p2);

                                update_constsethi(p3^.resulttype);
                                p3:=gentypeconvnode(p3,pd);
                                firstpass(p3);


                                if assigned(pd) then
                                  p3:=gentypeconvnode(p3,pd)
                                else
                                  p3:=gentypeconvnode(p3,u8bitdef);
                                firstpass(p3);
                                p4:=gennode(setelementn,p2,p3);
                              end;
                           end;
                       end
                      else
                       begin
                      { Single value }
                         if p2^.treetype=ordconstn then
                          begin
                            if not(is_integer(p2^.resulttype)) then
                              update_constsethi(p2^.resulttype)
                            else
                              begin
                                 p2:=gentypeconvnode(p2,u8bitdef);
                                 firstpass(p2);
                              end;

                            do_set(p2^.value);
                            disposetree(p2);
                          end
                         else
                          begin
                            update_constsethi(p2^.resulttype);

                            if assigned(pd) then
                              p2:=gentypeconvnode(p2,pd)
                            else
                              p2:=gentypeconvnode(p2,u8bitdef);
                            firstpass(p2);

                            p4:=gennode(setelementn,p2,nil);
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
                         for l:=1 to length(pstring(p2^.value_str)^) do
                          do_set(ord(pstring(p2^.value_str)^[l]));
                        if pd=nil then
                         pd:=cchardef;
                        disposetree(p2);
                      end;
              else
               CGMessage(type_e_ordinal_expr_expected);
              end;
            { insert the set creation tree }
              if assigned(p4) then
               buildp:=gennode(addn,buildp,p4);
            { load next and dispose current node }
              p2:=p;
              p:=p^.right;
              putnode(p2);
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
           putnode(p);
         end;
      { set the initial set type }
        constp^.resulttype:=new(psetdef,init(pd,constsethi));
      { set the new tree }
        p:=buildp;
      end;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    type
       tfirstconvproc = procedure(var p : ptree);

    procedure first_int_to_int(var p : ptree);
      begin
        if (p^.left^.location.loc<>LOC_REGISTER) and
           (p^.resulttype^.size>p^.left^.resulttype^.size) then
           p^.location.loc:=LOC_REGISTER;
        if is_64bitint(p^.resulttype) then
          p^.registers32:=max(p^.registers32,2)
        else
          p^.registers32:=max(p^.registers32,1);
      end;


    procedure first_cstring_to_pchar(var p : ptree);
      begin
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_string_to_chararray(var p : ptree);
      begin
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_string_to_string(var p : ptree);
      var
        hp : ptree;
      begin
         if pstringdef(p^.resulttype)^.string_typ<>
            pstringdef(p^.left^.resulttype)^.string_typ then
           begin
              if p^.left^.treetype=stringconstn then
                begin
                   p^.left^.stringtype:=pstringdef(p^.resulttype)^.string_typ;
                   p^.left^.resulttype:=p^.resulttype;
                   { remove typeconv node }
                   hp:=p;
                   p:=p^.left;
                   putnode(hp);
                   exit;
                end
              else
                procinfo^.flags:=procinfo^.flags or pi_do_call;
           end;
         { for simplicity lets first keep all ansistrings
           as LOC_MEM, could also become LOC_REGISTER }
         if pstringdef(p^.resulttype)^.string_typ in [st_ansistring,st_widestring] then
           { we may use ansistrings so no fast exit here }
           procinfo^.no_fast_exit:=true;
         p^.location.loc:=LOC_MEM;
      end;


    procedure first_char_to_string(var p : ptree);
      var
         hp : ptree;
      begin
         if p^.left^.treetype=ordconstn then
           begin
              hp:=genstringconstnode(chr(p^.left^.value),st_default);
              hp^.stringtype:=pstringdef(p^.resulttype)^.string_typ;
              firstpass(hp);
              disposetree(p);
              p:=hp;
           end
         else
           p^.location.loc:=LOC_MEM;
      end;


    procedure first_nothing(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
      end;


    procedure first_array_to_pointer(var p : ptree);
      begin
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_int_to_real(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=ordconstn then
         begin
           t:=genrealconstnode(p^.left^.value,pfloatdef(p^.resulttype));
           firstpass(t);
           disposetree(p);
           p:=t;
           exit;
         end;
        if p^.registersfpu<1 then
         p^.registersfpu:=1;
        p^.location.loc:=LOC_FPU;
      end;


    procedure first_int_to_fix(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=ordconstn then
         begin
           t:=genfixconstnode(p^.left^.value shl 16,p^.resulttype);
           firstpass(t);
           disposetree(p);
           p:=t;
           exit;
         end;
        if p^.registers32<1 then
         p^.registers32:=1;
        p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_real_to_fix(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=fixconstn then
         begin
           t:=genfixconstnode(round(p^.left^.value_real*65536),p^.resulttype);
           firstpass(t);
           disposetree(p);
           p:=t;
           exit;
         end;
        { at least one fpu and int register needed }
        if p^.registers32<1 then
          p^.registers32:=1;
        if p^.registersfpu<1 then
          p^.registersfpu:=1;
        p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_fix_to_real(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=fixconstn then
          begin
            t:=genrealconstnode(round(p^.left^.value_fix/65536.0),p^.resulttype);
            firstpass(t);
            disposetree(p);
            p:=t;
            exit;
          end;
        if p^.registersfpu<1 then
          p^.registersfpu:=1;
        p^.location.loc:=LOC_FPU;
      end;


    procedure first_real_to_real(var p : ptree);
      var
        t : ptree;
      begin
         if p^.left^.treetype=realconstn then
           begin
             t:=genrealconstnode(p^.left^.value_real,p^.resulttype);
             firstpass(t);
             disposetree(p);
             p:=t;
             exit;
           end;
        { comp isn't a floating type }
{$ifdef i386}
         if (pfloatdef(p^.resulttype)^.typ=s64comp) and
            (pfloatdef(p^.left^.resulttype)^.typ<>s64comp) and
            not (p^.explizit) then
           CGMessage(type_w_convert_real_2_comp);
{$endif}
         if p^.registersfpu<1 then
           p^.registersfpu:=1;
         p^.location.loc:=LOC_FPU;
      end;


    procedure first_pointer_to_array(var p : ptree);
      begin
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REFERENCE;
      end;


    procedure first_chararray_to_string(var p : ptree);
      begin
         { the only important information is the location of the }
         { result                                               }
         { other stuff is done by firsttypeconv           }
         p^.location.loc:=LOC_MEM;
      end;


    procedure first_cchar_to_pchar(var p : ptree);
      begin
         p^.left:=gentypeconvnode(p^.left,cshortstringdef);
         { convert constant char to constant string }
         firstpass(p^.left);
         { evalute tree }
         firstpass(p);
      end;


    procedure first_bool_to_int(var p : ptree);
      begin
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (p^.explizit) and
            (p^.left^.resulttype^.size=p^.resulttype^.size) and
            (p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           exit;
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_int_to_bool(var p : ptree);
      begin
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (p^.explizit) and
            (p^.left^.resulttype^.size=p^.resulttype^.size) and
            (p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           exit;
         p^.location.loc:=LOC_REGISTER;
         { need if bool to bool !!
           not very nice !!
         p^.left:=gentypeconvnode(p^.left,s32bitdef);
         p^.left^.explizit:=true;
         firstpass(p^.left);  }
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_bool_to_bool(var p : ptree);
      begin
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_proc_to_procvar(var p : ptree);
      begin
         { hmmm, I'am not sure if that is necessary (FK) }
         firstpass(p^.left);
         if codegenerror then
           exit;

         if (p^.left^.location.loc<>LOC_REFERENCE) then
           CGMessage(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_load_smallset(var p : ptree);
      begin
      end;


    procedure first_cord_to_pointer(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=ordconstn then
          begin
            t:=genpointerconstnode(p^.left^.value,p^.resulttype);
            firstpass(t);
            disposetree(p);
            p:=t;
            exit;
          end
        else
          internalerror(432472389);
      end;


    procedure first_pchar_to_string(var p : ptree);
      begin
         p^.location.loc:=LOC_REFERENCE;
      end;


    procedure first_ansistring_to_pchar(var p : ptree);
      begin
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_arrayconstructor_to_set(var p:ptree);
      var
        hp : ptree;
      begin
        if p^.left^.treetype<>arrayconstructn then
         internalerror(5546);
      { remove typeconv node }
        hp:=p;
        p:=p^.left;
        putnode(hp);
      { create a set constructor tree }
        arrayconstructor_to_set(p);
      { now firstpass the set }
        firstpass(p);
      end;


  procedure firsttypeconv(var p : ptree);
    var
      hp : ptree;
      aprocdef : pprocdef;
    const
       firstconvert : array[tconverttype] of tfirstconvproc = (
         first_nothing, {equal}
         first_nothing, {not_possible}
         first_string_to_string,
         first_char_to_string,
         first_pchar_to_string,
         first_cchar_to_pchar,
         first_cstring_to_pchar,
         first_ansistring_to_pchar,
         first_string_to_chararray,
         first_chararray_to_string,
         first_array_to_pointer,
         first_pointer_to_array,
         first_int_to_int,
         first_int_to_bool,
         first_bool_to_bool,
         first_bool_to_int,
         first_real_to_real,
         first_int_to_real,
         first_int_to_fix,
         first_real_to_fix,
         first_fix_to_real,
         first_proc_to_procvar,
         first_arrayconstructor_to_set,
         first_load_smallset,
         first_cord_to_pointer
       );
     begin
       aprocdef:=nil;
       { if explicite type cast, then run firstpass }
       if (p^.explizit) or not assigned(p^.left^.resulttype) then
         firstpass(p^.left);
       if (p^.left^.treetype=typen) and (p^.left^.resulttype=generrordef) then
         begin
            codegenerror:=true;
            Message(parser_e_no_type_not_allowed_here);
         end;
       if codegenerror then
         begin
           p^.resulttype:=generrordef;
           exit;
         end;

       if not assigned(p^.left^.resulttype) then
        begin
          codegenerror:=true;
          internalerror(52349);
          exit;
        end;

       { load the value_str from the left part }
       p^.registers32:=p^.left^.registers32;
       p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
       p^.registersmmx:=p^.left^.registersmmx;
{$endif}
       set_location(p^.location,p^.left^.location);

       { remove obsolete type conversions }
       if is_equal(p^.left^.resulttype,p^.resulttype) then
         begin
         { becuase is_equal only checks the basetype for sets we need to
           check here if we are loading a smallset into a normalset }
           if (p^.resulttype^.deftype=setdef) and
              (p^.left^.resulttype^.deftype=setdef) and
              (psetdef(p^.resulttype)^.settype<>smallset) and
              (psetdef(p^.left^.resulttype)^.settype=smallset) then
            begin
            { try to define the set as a normalset if it's a constant set }
              if p^.left^.treetype=setconstn then
               begin
                 p^.resulttype:=p^.left^.resulttype;
                 psetdef(p^.resulttype)^.settype:=normset
               end
              else
               p^.convtyp:=tc_load_smallset;
              exit;
            end
           else
            begin
              hp:=p;
              p:=p^.left;
              p^.resulttype:=hp^.resulttype;
              putnode(hp);
              exit;
            end;
         end;
       aprocdef:=assignment_overloaded(p^.left^.resulttype,p^.resulttype);
       if assigned(aprocdef) then
         begin
            procinfo^.flags:=procinfo^.flags or pi_do_call;
            hp:=gencallnode(overloaded_operators[_assignment],nil);
            { tell explicitly which def we must use !! (PM) }
            hp^.procdefinition:=aprocdef;
            hp^.left:=gencallparanode(p^.left,nil);
            putnode(p);
            p:=hp;
            firstpass(p);
            exit;
         end;

       if isconvertable(p^.left^.resulttype,p^.resulttype,p^.convtyp,p^.left^.treetype,p^.explizit)=0 then
         begin
           {Procedures have a resulttype of voiddef and functions of their
           own resulttype. They will therefore always be incompatible with
           a procvar. Because isconvertable cannot check for procedures we
           use an extra check for them.}
           if (m_tp_procvar in aktmodeswitches) then
            begin
              if (p^.resulttype^.deftype=procvardef) and
                 (is_procsym_load(p^.left) or is_procsym_call(p^.left)) then
               begin
                 if is_procsym_call(p^.left) then
                  begin
                    {if p^.left^.right=nil then
                     begin}
                       if (p^.left^.symtableprocentry^.owner^.symtabletype=objectsymtable){ and
                          (pobjectdef(p^.left^.symtableprocentry^.owner^.defowner)^.is_class) }then
                        hp:=genloadmethodcallnode(pprocsym(p^.left^.symtableprocentry),p^.left^.symtableproc,
                              getcopy(p^.left^.methodpointer))
                       else
                        hp:=genloadcallnode(pprocsym(p^.left^.symtableprocentry),p^.left^.symtableproc);
                       disposetree(p^.left);
                       firstpass(hp);
                       p^.left:=hp;
                       aprocdef:=pprocdef(p^.left^.resulttype);
                   (*  end
                    else
                     begin
                       p^.left^.right^.treetype:=loadn;
                       p^.left^.right^.symtableentry:=p^.left^.right^.symtableentry;
                       P^.left^.right^.resulttype:=pvarsym(p^.left^.symtableentry)^.definition;
                       hp:=p^.left^.right;
                       putnode(p^.left);
                       p^.left:=hp;
                       { should we do that ? }
                       firstpass(p^.left);
                       if not is_equal(p^.left^.resulttype,p^.resulttype) then
                        begin
                          CGMessage(type_e_mismatch);
                          exit;
                        end
                       else
                        begin
                          hp:=p;
                          p:=p^.left;
                          p^.resulttype:=hp^.resulttype;
                          putnode(hp);
                          exit;
                        end;
                     end; *)
                  end
                 else
                  begin
                    if (p^.left^.treetype<>addrn) then
                      aprocdef:=pprocsym(p^.left^.symtableentry)^.definition;
                  end;
                 p^.convtyp:=tc_proc_2_procvar;
                 { Now check if the procedure we are going to assign to
                   the procvar,  is compatible with the procvar's type }
                 if assigned(aprocdef) then
                  begin
                    if not proc_to_procvar_equal(aprocdef,pprocvardef(p^.resulttype)) then
                     CGMessage2(type_e_incompatible_types,aprocdef^.typename,p^.resulttype^.typename);
                    firstconvert[p^.convtyp](p);
                  end
                 else
                  CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
                 exit;
               end;
            end;
           if p^.explizit then
            begin
              { check if the result could be in a register }
              if not(p^.resulttype^.is_intregable) and
                not(p^.resulttype^.is_fpuregable) then
                make_not_regable(p^.left);
              { boolean to byte are special because the
                location can be different }

              if is_integer(p^.resulttype) and
                 is_boolean(p^.left^.resulttype) then
               begin
                  p^.convtyp:=tc_bool_2_int;
                  firstconvert[p^.convtyp](p);
                  exit;
               end;
              { ansistring to pchar }
              if is_pchar(p^.resulttype) and
                 is_ansistring(p^.left^.resulttype) then
               begin
                 p^.convtyp:=tc_ansistring_2_pchar;
                 firstconvert[p^.convtyp](p);
                 exit;
               end;
              { do common tc_equal cast }
              p^.convtyp:=tc_equal;

              { enum to ordinal will always be s32bit }
              if (p^.left^.resulttype^.deftype=enumdef) and
                 is_ordinal(p^.resulttype) then
               begin
                 if p^.left^.treetype=ordconstn then
                  begin
                    hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
                    disposetree(p);
                    firstpass(hp);
                    p:=hp;
                    exit;
                  end
                 else
                  begin
                    if isconvertable(s32bitdef,p^.resulttype,p^.convtyp,ordconstn,false)=0 then
                      CGMessage(cg_e_illegal_type_conversion);
                  end;
               end

              { ordinal to enumeration }
              else
               if (p^.resulttype^.deftype=enumdef) and
                  is_ordinal(p^.left^.resulttype) then
                begin
                  if p^.left^.treetype=ordconstn then
                   begin
                     hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
                     disposetree(p);
                     firstpass(hp);
                     p:=hp;
                     exit;
                   end
                  else
                   begin
                     if IsConvertable(p^.left^.resulttype,s32bitdef,p^.convtyp,ordconstn,false)=0 then
                       CGMessage(cg_e_illegal_type_conversion);
                   end;
                end

              { nil to ordinal node }
              else if is_ordinal(p^.resulttype) and
                (p^.left^.treetype=niln) then
                begin
                   hp:=genordinalconstnode(0,p^.resulttype);
                   firstpass(hp);
                   disposetree(p);
                   p:=hp;
                   exit;
                end

              {Are we typecasting an ordconst to a char?}
              else
                if is_char(p^.resulttype) and
                   is_ordinal(p^.left^.resulttype) then
                 begin
                   if p^.left^.treetype=ordconstn then
                    begin
                      hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
                      firstpass(hp);
                      disposetree(p);
                      p:=hp;
                      exit;
                    end
                   else
                    begin
                      { this is wrong because it converts to a 4 byte long var !!
                      if not isconvertable(p^.left^.resulttype,s32bitdef,p^.convtyp,ordconstn  nur Dummy ) then }
                      if IsConvertable(p^.left^.resulttype,u8bitdef,p^.convtyp,ordconstn,false)=0 then
                        CGMessage(cg_e_illegal_type_conversion);
                    end;
                 end

               { only if the same size or formal def }
               { why do we allow typecasting of voiddef ?? (PM) }
               else
                begin
                  if not(
                     (p^.left^.resulttype^.deftype=formaldef) or
                     (p^.left^.resulttype^.size=p^.resulttype^.size) or
                     (is_equal(p^.left^.resulttype,voiddef)  and
                     (p^.left^.treetype=derefn))
                     ) then
                    CGMessage(cg_e_illegal_type_conversion);
                  if ((p^.left^.resulttype^.deftype=orddef) and
                      (p^.resulttype^.deftype=pointerdef)) or
                      ((p^.resulttype^.deftype=orddef) and
                       (p^.left^.resulttype^.deftype=pointerdef))
                       {$ifdef extdebug}and (p^.firstpasscount=0){$endif} then
                    CGMessage(cg_d_pointer_to_longint_conv_not_portable);
                end;

               { the conversion into a strutured type is only }
               { possible, if the source is no register    }
               if ((p^.resulttype^.deftype in [recorddef,stringdef,arraydef]) or
                   ((p^.resulttype^.deftype=objectdef) and not(pobjectdef(p^.resulttype)^.is_class))
                  ) and (p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) { and
                   it also works if the assignment is overloaded
                   YES but this code is not executed if assignment is overloaded (PM)
                  not assigned(assignment_overloaded(p^.left^.resulttype,p^.resulttype))} then
                 CGMessage(cg_e_illegal_type_conversion);
            end
           else
            CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
         end;

       { tp7 procvar support, when right is not a procvardef and we got a
         loadn of a procvar then convert to a calln, the check for the
         result is already done in is_convertible, also no conflict with
         @procvar is here because that has an extra addrn }
         if (m_tp_procvar in aktmodeswitches) and
            (p^.resulttype^.deftype<>procvardef) and
            (p^.left^.resulttype^.deftype=procvardef) and
            (p^.left^.treetype=loadn) then
          begin
            hp:=gencallnode(nil,nil);
            hp^.right:=p^.left;
            firstpass(hp);
            p^.left:=hp;
          end;


        { ordinal contants can be directly converted }
        { but not int64/qword                        }
        if (p^.left^.treetype=ordconstn) and is_ordinal(p^.resulttype) and
          not(is_64bitint(p^.resulttype)) then
          begin
             { range checking is done in genordinalconstnode (PFV) }
             hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
             disposetree(p);
             firstpass(hp);
             p:=hp;
             exit;
          end;
        if p^.convtyp<>tc_equal then
          firstconvert[p^.convtyp](p);
      end;


{*****************************************************************************
                                FirstIs
*****************************************************************************}

    procedure firstis(var p : ptree);
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;

         if (p^.right^.resulttype^.deftype<>classrefdef) then
           CGMessage(type_e_mismatch);

         left_right_max(p);

         { left must be a class }
         if (p^.left^.resulttype^.deftype<>objectdef) or
            not(pobjectdef(p^.left^.resulttype)^.is_class) then
           CGMessage(type_e_mismatch);

         { the operands must be related }
         if (not(pobjectdef(p^.left^.resulttype)^.is_related(
           pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)))) and
           (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)^.is_related(
           pobjectdef(p^.left^.resulttype)))) then
           CGMessage(type_e_mismatch);

         p^.location.loc:=LOC_FLAGS;
         p^.resulttype:=booldef;
      end;


{*****************************************************************************
                                FirstAs
*****************************************************************************}

    procedure firstas(var p : ptree);
      begin
         firstpass(p^.right);
         set_varstate(p^.right,true);
         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;

         if (p^.right^.resulttype^.deftype<>classrefdef) then
           CGMessage(type_e_mismatch);

         left_right_max(p);

         { left must be a class }
         if (p^.left^.resulttype^.deftype<>objectdef) or
           not(pobjectdef(p^.left^.resulttype)^.is_class) then
           CGMessage(type_e_mismatch);

         { the operands must be related }
         if (not(pobjectdef(p^.left^.resulttype)^.is_related(
           pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)))) and
           (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)^.is_related(
           pobjectdef(p^.left^.resulttype)))) then
           CGMessage(type_e_mismatch);

         set_location(p^.location,p^.left^.location);
         p^.resulttype:=pclassrefdef(p^.right^.resulttype)^.pointertype.def;
      end;


end.
{
  $Log$
  Revision 1.5  2000-08-02 07:20:32  jonas
      - undid my changes from the previous two commits because it was a bug
        in cg386cnv which I've now fixed (previous changes only masked it in
        some cases) (merged from fixes branch)

  Revision 1.4  2000/08/01 10:41:35  jonas
    * refined my previous IE(10) fix (in some cases, too many registers could
      be reserved) (merged from fixes branch)

  Revision 1.3  2000/07/21 09:23:47  jonas
    * merged from fixes branch

  Revision 1.2  2000/07/13 11:32:51  michael
  + removed logs

}

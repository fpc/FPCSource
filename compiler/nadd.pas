{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for add nodes

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
unit nadd;

{$i defines.inc}

interface

    uses
      node;

    type
       taddnode = class(tbinopnode)
          procedure make_bool_equal_size;
          function pass_1 : tnode;override;
       end;

    var
       { caddnode is used to create nodes of the add type }
       { the virtual constructor allows to assign         }
       { another class type to caddnode => processor      }
       { specific node types can be created               }
       caddnode : class of taddnode;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symtype,symdef,types,
      cpuinfo,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      htypechk,pass_1,
      nmat,ncnv,nld,ncon,nset,nopt,
      cpubase;


{*****************************************************************************
                                TADDNODE
*****************************************************************************}

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}

    procedure taddnode.make_bool_equal_size;

      begin
        if porddef(left.resulttype)^.typ>porddef(right.resulttype)^.typ then
         begin
           right:=gentypeconvnode(right,porddef(left.resulttype));
           ttypeconvnode(right).convtype:=tc_bool_2_int;
           include(right.flags,nf_explizit);
           firstpass(right);
         end
        else
         if porddef(left.resulttype)^.typ<porddef(right.resulttype)^.typ then
          begin
            left:=gentypeconvnode(left,porddef(right.resulttype));
            ttypeconvnode(left).convtype:=tc_bool_2_int;
            include(left.flags,nf_explizit);
            firstpass(left);
          end;
      end;

    function taddnode.pass_1 : tnode;

      var
         t,hp    : tnode;
         ot,
         lt,rt   : tnodetype;
         rv,lv   : tconstexprint;
         rvd,lvd : bestreal;
         resdef,
         rd,ld   : pdef;
         tempdef : pdef;
         concatstrings : boolean;

         { to evalute const sets }
         resultset : pconstset;
         i : longint;
         b : boolean;
         convdone : boolean;
         s1,s2 : pchar;
         l1,l2 : longint;

      begin
         pass_1:=nil;
         { first do the two subtrees }
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         { convert array constructors to sets, because there is no other operator
           possible for array constructors }
         if is_array_constructor(left.resulttype) then
           arrayconstructor_to_set(tarrayconstructornode(left));
         if is_array_constructor(right.resulttype) then
           arrayconstructor_to_set(tarrayconstructornode(right));

         { both left and right need to be valid }
         set_varstate(left,true);
         set_varstate(right,true);

         { load easier access variables }
         lt:=left.nodetype;
         rt:=right.nodetype;
         rd:=right.resulttype;
         ld:=left.resulttype;
         convdone:=false;

         hp:=self;
         if isbinaryoverloaded(hp) then
           begin
              pass_1:=hp;
              exit;
           end;
         { compact consts }

         { convert int consts to real consts, if the }
         { other operand is a real const             }
         if (rt=realconstn) and is_constintnode(left) then
           begin
              t:=genrealconstnode(tordconstnode(left).value,right.resulttype);
              left.free;
              left:=t;
              lt:=realconstn;
           end;
         if (lt=realconstn) and is_constintnode(right) then
           begin
              t:=genrealconstnode(tordconstnode(right).value,left.resulttype);
              right.free;
              right:=t;
              rt:=realconstn;
           end;

       { both are int constants, also allow operations on two equal enums
         in fpc mode (Needed for conversion of C code) }
         if ((lt=ordconstn) and (rt=ordconstn)) and
            ((is_constintnode(left) and is_constintnode(right)) or
             (is_constboolnode(left) and is_constboolnode(right) and
              (nodetype in [ltn,lten,gtn,gten,equaln,unequaln,andn,xorn,orn]))) then
           begin
              { xor, and, or are handled different from arithmetic }
              { operations regarding the result type               }
              { return a boolean for boolean operations (and,xor,or) }
              if is_constboolnode(left) then
               resdef:=booldef
              else if is_64bitint(rd) or is_64bitint(ld) then
                resdef:=cs64bitdef
              else
                resdef:=s32bitdef;
              lv:=tordconstnode(left).value;
              rv:=tordconstnode(right).value;
              case nodetype of
                addn : t:=genintconstnode(lv+rv);
                subn : t:=genintconstnode(lv-rv);
                muln : t:=genintconstnode(lv*rv);
                xorn : t:=genordinalconstnode(lv xor rv,resdef);
                 orn: t:=genordinalconstnode(lv or rv,resdef);
                andn: t:=genordinalconstnode(lv and rv,resdef);
                 ltn : t:=genordinalconstnode(ord(lv<rv),booldef);
                lten : t:=genordinalconstnode(ord(lv<=rv),booldef);
                 gtn : t:=genordinalconstnode(ord(lv>rv),booldef);
                gten : t:=genordinalconstnode(ord(lv>=rv),booldef);
              equaln : t:=genordinalconstnode(ord(lv=rv),booldef);
            unequaln : t:=genordinalconstnode(ord(lv<>rv),booldef);
              slashn : begin
                       { int/int becomes a real }
                         if int(rv)=0 then
                          begin
                            Message(parser_e_invalid_float_operation);
                            t:=genrealconstnode(0,bestrealdef^);
                          end
                         else
                          t:=genrealconstnode(int(lv)/int(rv),bestrealdef^);
                         firstpass(t);
                       end;
              else
                CGMessage(type_e_mismatch);
              end;
              pass_1:=t;
              exit;
           end;

       { both real constants ? }
         if (lt=realconstn) and (rt=realconstn) then
           begin
              lvd:=trealconstnode(left).value_real;
              rvd:=trealconstnode(right).value_real;
              case nodetype of
                 addn : t:=genrealconstnode(lvd+rvd,bestrealdef^);
                 subn : t:=genrealconstnode(lvd-rvd,bestrealdef^);
                 muln : t:=genrealconstnode(lvd*rvd,bestrealdef^);
               starstarn,
               caretn : begin
                          if lvd<0 then
                           begin
                             Message(parser_e_invalid_float_operation);
                             t:=genrealconstnode(0,bestrealdef^);
                           end
                          else if lvd=0 then
                            t:=genrealconstnode(1.0,bestrealdef^)
                          else
                            t:=genrealconstnode(exp(ln(lvd)*rvd),bestrealdef^);
                        end;
               slashn :
                        begin
                          if rvd=0 then
                           begin
                             Message(parser_e_invalid_float_operation);
                             t:=genrealconstnode(0,bestrealdef^);
                           end
                          else
                           t:=genrealconstnode(lvd/rvd,bestrealdef^);
                        end;
                  ltn : t:=genordinalconstnode(ord(lvd<rvd),booldef);
                 lten : t:=genordinalconstnode(ord(lvd<=rvd),booldef);
                  gtn : t:=genordinalconstnode(ord(lvd>rvd),booldef);
                 gten : t:=genordinalconstnode(ord(lvd>=rvd),booldef);
               equaln : t:=genordinalconstnode(ord(lvd=rvd),booldef);
             unequaln : t:=genordinalconstnode(ord(lvd<>rvd),booldef);
              else
                CGMessage(type_e_mismatch);
              end;
              pass_1:=t;
              exit;
           end;

       { concating strings ? }
         concatstrings:=false;
         s1:=nil;
         s2:=nil;
         if (lt=ordconstn) and (rt=ordconstn) and
            is_char(ld) and is_char(rd) then
           begin
              s1:=strpnew(char(byte(tordconstnode(left).value)));
              s2:=strpnew(char(byte(tordconstnode(right).value)));
              l1:=1;
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=stringconstn) and (rt=ordconstn) and is_char(rd) then
           begin
              s1:=tstringconstnode(left).getpcharcopy;
              l1:=tstringconstnode(left).len;
              s2:=strpnew(char(byte(tordconstnode(right).value)));
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=ordconstn) and (rt=stringconstn) and is_char(ld) then
           begin
              s1:=strpnew(char(byte(tordconstnode(left).value)));
              l1:=1;
              s2:=tstringconstnode(right).getpcharcopy;
              l2:=tstringconstnode(right).len;
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
              s1:=tstringconstnode(left).getpcharcopy;
              l1:=tstringconstnode(left).len;
              s2:=tstringconstnode(right).getpcharcopy;
              l2:=tstringconstnode(right).len;
              concatstrings:=true;
           end;

         { I will need to translate all this to ansistrings !!! }
         if concatstrings then
           begin
              case nodetype of
                 addn :
                   t:=genpcharconstnode(concatansistrings(s1,s2,l1,l2),l1+l2);
                 ltn :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)<0),booldef);
                 lten :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)<=0),booldef);
                 gtn :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)>0),booldef);
                 gten :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)>=0),booldef);
                 equaln :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)=0),booldef);
                 unequaln :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)<>0),booldef);
              end;
              ansistringdispose(s1,l1);
              ansistringdispose(s2,l2);
              pass_1:=t;
              exit;
           end;

       { if both are orddefs then check sub types }
         if (ld^.deftype=orddef) and (rd^.deftype=orddef) then
           begin
           { 2 booleans ? }
             if is_boolean(ld) and is_boolean(rd) then
              begin
                if (cs_full_boolean_eval in aktlocalswitches) or
                   (nodetype in [xorn,ltn,lten,gtn,gten]) then
                  begin
                     make_bool_equal_size;
                     if (left.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                       (left.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                       calcregisters(self,2,0,0)
                     else
                       calcregisters(self,1,0,0);
                  end
                else
                  case nodetype of
                    andn,
                    orn:
                      begin
                        make_bool_equal_size;
                        calcregisters(self,0,0,0);
                        location.loc:=LOC_JUMP;
                      end;
                    unequaln,
                    equaln:
                      begin
                        make_bool_equal_size;
                        { Remove any compares with constants }
                        if (left.nodetype=ordconstn) then
                         begin
                           hp:=right;
                           b:=(tordconstnode(left).value<>0);
                           ot:=nodetype;
                           left.free;
                           left:=nil;
                           right:=nil;
                           if (not(b) and (ot=equaln)) or
                              (b and (ot=unequaln)) then
                            begin
                              hp:=cnotnode.create(hp);
                              firstpass(hp);
                            end;
                           pass_1:=hp;
                           exit;
                         end;
                        if (right.nodetype=ordconstn) then
                         begin
                           hp:=left;
                           b:=(tordconstnode(right).value<>0);
                           ot:=nodetype;
                           right.free;
                           right:=nil;
                           left:=nil;

                           if (not(b) and (ot=equaln)) or
                              (b and (ot=unequaln)) then
                            begin
                              hp:=cnotnode.create(hp);
                              firstpass(hp);
                            end;
                           pass_1:=hp;
                           exit;
                         end;
                        if (left.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                          (left.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                          calcregisters(self,2,0,0)
                        else
                        calcregisters(self,1,0,0);
                      end;
                  else
                    CGMessage(type_e_mismatch);
                  end;
(*
                { these one can't be in flags! }

                Yes they can, secondadd converts the loc_flags to a register.
                The typeconversions below are simply removed by firsttypeconv()
                because the resulttype of left = left.resulttype
                (surprise! :) (JM)

                if nodetype in [xorn,unequaln,equaln] then
                  begin
                     if left.location.loc=LOC_FLAGS then
                       begin
                          left:=gentypeconvnode(left,porddef(left.resulttype));
                          left.convtype:=tc_bool_2_int;
                          left.explizit:=true;
                          firstpass(left);
                       end;
                     if right.location.loc=LOC_FLAGS then
                       begin
                          right:=gentypeconvnode(right,porddef(right.resulttype));
                          right.convtype:=tc_bool_2_int;
                          right.explizit:=true;
                          firstpass(right);
                       end;
                     { readjust registers }
                     calcregisters(p,1,0,0);
                  end;
*)
                convdone:=true;
              end
             else
             { Both are chars? only convert to shortstrings for addn }
              if is_char(rd) and is_char(ld) then
               begin
                 if nodetype=addn then
                   begin
                     left:=gentypeconvnode(left,cshortstringdef);
                     firstpass(left);
                     hp := genaddsstringcharoptnode(self);
                     firstpass(hp);
                     pass_1 := hp;
                     exit;
                   end
                 else
                   calcregisters(self,1,0,0);
                 convdone:=true;
               end
              { is there a 64 bit type ? }
             else if ((porddef(rd)^.typ=s64bit) or (porddef(ld)^.typ=s64bit)) and
               { the / operator is handled later }
               (nodetype<>slashn) then
               begin
                  if (porddef(ld)^.typ<>s64bit) then
                    begin
                      left:=gentypeconvnode(left,cs64bitdef);
                      firstpass(left);
                    end;
                  if (porddef(rd)^.typ<>s64bit) then
                    begin
                       right:=gentypeconvnode(right,cs64bitdef);
                       firstpass(right);
                    end;
                  calcregisters(self,2,0,0);
                  convdone:=true;
               end
             else if ((porddef(rd)^.typ=u64bit) or (porddef(ld)^.typ=u64bit)) and
               { the / operator is handled later }
               (nodetype<>slashn) then
               begin
                  if (porddef(ld)^.typ<>u64bit) then
                    begin
                      left:=gentypeconvnode(left,cu64bitdef);
                      firstpass(left);
                    end;
                  if (porddef(rd)^.typ<>u64bit) then
                    begin
                       right:=gentypeconvnode(right,cu64bitdef);
                       firstpass(right);
                    end;
                  calcregisters(self,2,0,0);
                  convdone:=true;
               end
             else
              { is there a cardinal? }
              if ((porddef(rd)^.typ=u32bit) or (porddef(ld)^.typ=u32bit)) and
               { the / operator is handled later }
               (nodetype<>slashn) then
               begin
                 if is_signed(ld) and
                    { then rd = u32bit }
                    { convert positive constants to u32bit }
                    not(is_constintnode(left) and
                        (tordconstnode(left).value >= 0)) and
                    { range/overflow checking on mixed signed/cardinal expressions }
                    { is only possible if you convert everything to 64bit (JM)     }
                    ((aktlocalswitches * [cs_check_overflow,cs_check_range] <> []) and
                     (nodetype in [addn,subn,muln])) then
                   begin
                     { perform the operation in 64bit }
                     CGMessage(type_w_mixed_signed_unsigned);
                     left := gentypeconvnode(left,cs64bitdef);
                     firstpass(left);
                     right := gentypeconvnode(right,cs64bitdef);
                     firstpass(right);
                   end
                 else
                   begin
                     if is_signed(ld) and
                        not(is_constintnode(left) and
                            (tordconstnode(left).value >= 0)) and
                        (cs_check_range in aktlocalswitches) then
                       CGMessage(type_w_mixed_signed_unsigned2);
                     left := gentypeconvnode(left,u32bitdef);
                     firstpass(left);

                     if is_signed(rd) and
                        { then ld = u32bit }
                        { convert positive constants to u32bit }
                        not(is_constintnode(right) and
                            (tordconstnode(right).value >= 0)) and
                        ((aktlocalswitches * [cs_check_overflow,cs_check_range] <> []) and
                         (nodetype in [addn,subn,muln])) then
                       begin
                         { perform the operation in 64bit }
                         CGMessage(type_w_mixed_signed_unsigned);
                         left := gentypeconvnode(left,cs64bitdef);
                         firstpass(left);
                         right := gentypeconvnode(right,cs64bitdef);
                         firstpass(right);
                       end
                     else
                       begin
                         if is_signed(rd) and
                            not(is_constintnode(right) and
                                (tordconstnode(right).value >= 0)) and
                            (cs_check_range in aktlocalswitches) then
                           CGMessage(type_w_mixed_signed_unsigned2);
                         right := gentypeconvnode(right,u32bitdef);
                         firstpass(right);
                       end;
                   end;
                 { did we convert things to 64bit? }
                 if porddef(left.resulttype)^.typ = s64bit then
                   calcregisters(self,2,0,0)
                 else
                   begin
                     calcregisters(self,1,0,0);
                 { for unsigned mul we need an extra register }
                     if nodetype=muln then
                       inc(registers32);
                   end;
                 convdone:=true;
               end;
           end
         else

         { left side a setdef, must be before string processing,
           else array constructor can be seen as array of char (PFV) }
           if (ld^.deftype=setdef) {or is_array_constructor(ld)} then
             begin
             { trying to add a set element? }
                if (nodetype=addn) and (rd^.deftype<>setdef) then
                 begin
                   if (rt=setelementn) then
                    begin
                      if not(is_equal(psetdef(ld)^.elementtype.def,rd)) then
                       CGMessage(type_e_set_element_are_not_comp);
                    end
                   else
                    CGMessage(type_e_mismatch)
                 end
                else
                 begin
                   if not(nodetype in [addn,subn,symdifn,muln,equaln,unequaln
{$IfNDef NoSetInclusion}
                                          ,lten,gten
{$EndIf NoSetInclusion}
                   ]) then
                    CGMessage(type_e_set_operation_unknown);
                 { right def must be a also be set }
                   if (rd^.deftype<>setdef) or not(is_equal(rd,ld)) then
                    CGMessage(type_e_set_element_are_not_comp);
                 end;

                { ranges require normsets }
                if (psetdef(ld)^.settype=smallset) and
                   (rt=setelementn) and
                   assigned(tsetelementnode(right).right) then
                 begin
                   { generate a temporary normset def, it'll be destroyed
                     when the symtable is unloaded }
                   tempdef:=new(psetdef,init(psetdef(ld)^.elementtype.def,255));
                   left:=gentypeconvnode(left,tempdef);
                   firstpass(left);
                   ld:=left.resulttype;
                 end;

                { if the destination is not a smallset then insert a typeconv
                  which loads a smallset into a normal set }
                if (psetdef(ld)^.settype<>smallset) and
                   (psetdef(rd)^.settype=smallset) then
                 begin
                   if (right.nodetype=setconstn) then
                     begin
                        t:=gensetconstnode(tsetconstnode(right).value_set,psetdef(left.resulttype));
                        tsetconstnode(t).left:=tsetconstnode(right).left;
                        tsetconstnode(right).left:=nil;
                        right.free;
                        right:=t;
                     end
                   else
                     right:=gentypeconvnode(right,psetdef(left.resulttype));
                   firstpass(right);
                 end;

                { do constant evaluation }
                if (right.nodetype=setconstn) and
                   not assigned(tsetconstnode(right).left) and
                   (left.nodetype=setconstn) and
                   not assigned(tsetconstnode(left).left) then
                  begin
                     new(resultset);
                     case nodetype of
                        addn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      tsetconstnode(right).value_set^[i] or tsetconstnode(left).value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        muln : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      tsetconstnode(right).value_set^[i] and tsetconstnode(left).value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        subn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      tsetconstnode(left).value_set^[i] and not(tsetconstnode(right).value_set^[i]);
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                     symdifn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      tsetconstnode(left).value_set^[i] xor tsetconstnode(right).value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                    unequaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if tsetconstnode(right).value_set^[i]=tsetconstnode(left).value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef);
                               end;
                      equaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if tsetconstnode(right).value_set^[i]<>tsetconstnode(left).value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef);
                               end;
{$IfNDef NoSetInclusion}
                       lten : Begin
                                b := true;
                                For i := 0 to 31 Do
                                  If (tsetconstnode(right).value_set^[i] And tsetconstnode(left).value_set^[i]) <>
                                      tsetconstnode(left).value_set^[i] Then
                                    Begin
                                      b := false;
                                      Break
                                    End;
                                t := genordinalconstnode(ord(b),booldef);
                              End;
                       gten : Begin
                                b := true;
                                For i := 0 to 31 Do
                                  If (tsetconstnode(left).value_set^[i] And tsetconstnode(right).value_set^[i]) <>
                                      tsetconstnode(right).value_set^[i] Then
                                    Begin
                                      b := false;
                                      Break
                                    End;
                                t := genordinalconstnode(ord(b),booldef);
                              End;
{$EndIf NoSetInclusion}
                     end;
                     dispose(resultset);
                     firstpass(t);
                     pass_1:=t;
                     exit;
                  end
                else
                 if psetdef(ld)^.settype=smallset then
                  begin
                     { are we adding set elements ? }
                     if right.nodetype=setelementn then
                       calcregisters(self,2,0,0)
                     else
                       calcregisters(self,1,0,0);
                     location.loc:=LOC_REGISTER;
                  end
                 else
                  begin
                     calcregisters(self,0,0,0);
                     { here we call SET... }
                     procinfo^.flags:=procinfo^.flags or pi_do_call;
                     location.loc:=LOC_MEM;
                  end;
              convdone:=true;
            end
         else
           { compare pchar to char arrays by addresses
             like BP/Delphi }
           if (is_pchar(ld) and is_chararray(rd)) or
              (is_pchar(rd) and is_chararray(ld)) then
             begin
               if is_chararray(rd) then
                 begin
                   right:=gentypeconvnode(right,ld);
                   firstpass(right);
                 end
               else
                 begin
                   left:=gentypeconvnode(left,rd);
                   firstpass(left);
                 end;
               location.loc:=LOC_REGISTER;
               calcregisters(self,1,0,0);
               convdone:=true;
             end
         else
           { is one of the operands a string?,
             chararrays are also handled as strings (after conversion) }
           if (rd^.deftype=stringdef) or (ld^.deftype=stringdef) or
              ((is_chararray(rd) or is_char(rd)) and
               (is_chararray(ld) or is_char(ld))) then
            begin
              if is_widestring(rd) or is_widestring(ld) then
                begin
                   if not(is_widestring(rd)) then
                     right:=gentypeconvnode(right,cwidestringdef);
                   if not(is_widestring(ld)) then
                     left:=gentypeconvnode(left,cwidestringdef);
                   resulttype:=cwidestringdef;
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_REGISTER;
                end
              else if is_ansistring(rd) or is_ansistring(ld) then
                begin
                   if not(is_ansistring(rd)) then
                     right:=gentypeconvnode(right,cansistringdef);
                   if not(is_ansistring(ld)) then
                     left:=gentypeconvnode(left,cansistringdef);
                   { we use ansistrings so no fast exit here }
                   procinfo^.no_fast_exit:=true;
                   resulttype:=cansistringdef;
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_REGISTER;
                end
              else if is_longstring(rd) or is_longstring(ld) then
                begin
                   if not(is_longstring(rd)) then
                     right:=gentypeconvnode(right,clongstringdef);
                   if not(is_longstring(ld)) then
                     left:=gentypeconvnode(left,clongstringdef);
                   resulttype:=clongstringdef;
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_MEM;
                end
              else
                begin
                   if canbeaddsstringcharoptnode(self) then
                     begin
                       hp := genaddsstringcharoptnode(self);
                       firstpass(hp);
                       pass_1 := hp;
                       exit;
                     end;
                   if canbeaddsstringcsstringoptnode(self) then
                     begin
                       hp := genaddsstringcsstringoptnode(self);
                       firstpass(hp);
                       pass_1 := hp;
                       exit;
                     end;
                   if not(is_shortstring(ld)) then
                     left:=gentypeconvnode(left,cshortstringdef);
                   if not(is_shortstring(rd)) then
                      right:=gentypeconvnode(right,cshortstringdef);
                   resulttype:=left.resulttype;
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_MEM;
                end;
              { only if there is a type cast we need to do again }
              { the first pass                             }
              if left.nodetype=typeconvn then
                firstpass(left);
              if right.nodetype=typeconvn then
                firstpass(right);
              { here we call STRCONCAT or STRCMP or STRCOPY }
              procinfo^.flags:=procinfo^.flags or pi_do_call;
              if location.loc=LOC_MEM then
                calcregisters(self,0,0,0)
              else
                calcregisters(self,1,0,0);
              convdone:=true;
           end
         else

         { is one a real float ? }
           if (rd^.deftype=floatdef) or (ld^.deftype=floatdef) then
            begin
            { if one is a fixed, then convert to f32bit }
              if ((rd^.deftype=floatdef) and (pfloatdef(rd)^.typ=f32bit)) or
                 ((ld^.deftype=floatdef) and (pfloatdef(ld)^.typ=f32bit)) then
               begin
                 if not is_integer(rd) or (nodetype<>muln) then
                   right:=gentypeconvnode(right,s32fixeddef);
                 if not is_integer(ld) or (nodetype<>muln) then
                   left:=gentypeconvnode(left,s32fixeddef);
                 firstpass(left);
                 firstpass(right);
                 calcregisters(self,1,0,0);
                 location.loc:=LOC_REGISTER;
               end
              else
              { convert both to bestreal }
                begin
                  right:=gentypeconvnode(right,bestrealdef^);
                  left:=gentypeconvnode(left,bestrealdef^);
                  firstpass(left);
                  firstpass(right);
                  calcregisters(self,0,1,0);
                  location.loc:=LOC_FPU;
                end;
              convdone:=true;
            end
         else

         { pointer comperation and subtraction }
           if (rd^.deftype=pointerdef) and (ld^.deftype=pointerdef) then
            begin
              location.loc:=LOC_REGISTER;
              { right:=gentypeconvnode(right,ld); }
              { firstpass(right); }
              calcregisters(self,1,0,0);
              case nodetype of
                 equaln,unequaln :
                   begin
                      if is_equal(right.resulttype,voidpointerdef) then
                        begin
                           right:=gentypeconvnode(right,ld);
                           firstpass(right);
                        end
                      else if is_equal(left.resulttype,voidpointerdef) then
                        begin
                           left:=gentypeconvnode(left,rd);
                           firstpass(left);
                        end
                      else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                   end;
                 ltn,lten,gtn,gten:
                   begin
                      if is_equal(right.resulttype,voidpointerdef) then
                        begin
                           right:=gentypeconvnode(right,ld);
                           firstpass(right);
                        end
                      else if is_equal(left.resulttype,voidpointerdef) then
                        begin
                           left:=gentypeconvnode(left,rd);
                           firstpass(left);
                        end
                      else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                      if not(cs_extsyntax in aktmoduleswitches) then
                        CGMessage(type_e_mismatch);
                   end;
                 subn:
                   begin
                      if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                      if not(cs_extsyntax in aktmoduleswitches) then
                        CGMessage(type_e_mismatch);
                      resulttype:=s32bitdef;
                      exit;
                   end;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else
           if is_class_or_interface(rd) or is_class_or_interface(ld) then
            begin
              location.loc:=LOC_REGISTER;
              if is_class_or_interface(rd) and is_class_or_interface(ld) then
                begin
                   if pobjectdef(rd)^.is_related(pobjectdef(ld)) then
                     right:=gentypeconvnode(right,ld)
                   else
                     left:=gentypeconvnode(left,rd);
                end
              else if is_class_or_interface(rd) then
                left:=gentypeconvnode(left,rd)
              else
                right:=gentypeconvnode(right,ld);

              firstpass(right);
              firstpass(left);
              calcregisters(self,1,0,0);
              case nodetype of
                 equaln,unequaln:
                   ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) and (ld^.deftype=classrefdef) then
            begin
              location.loc:=LOC_REGISTER;
              if pobjectdef(pclassrefdef(rd)^.pointertype.def)^.is_related(pobjectdef(
                pclassrefdef(ld)^.pointertype.def)) then
                right:=gentypeconvnode(right,ld)
              else
                left:=gentypeconvnode(left,rd);
              firstpass(right);
              firstpass(left);
              calcregisters(self,1,0,0);
              case nodetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

         { allows comperasion with nil pointer }
           if is_class_or_interface(rd) then
            begin
              location.loc:=LOC_REGISTER;
              left:=gentypeconvnode(left,rd);
              firstpass(left);
              calcregisters(self,1,0,0);
              case nodetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if is_class_or_interface(ld) then
            begin
              location.loc:=LOC_REGISTER;
              right:=gentypeconvnode(right,ld);
              firstpass(right);
              calcregisters(self,1,0,0);
              case nodetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) then
            begin
              left:=gentypeconvnode(left,rd);
              firstpass(left);
              calcregisters(self,1,0,0);
              case nodetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=classrefdef) then
            begin
              right:=gentypeconvnode(right,ld);
              firstpass(right);
              calcregisters(self,1,0,0);
              case nodetype of
                equaln,unequaln : ;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

         { support procvar=nil,procvar<>nil }
           if ((ld^.deftype=procvardef) and (rt=niln)) or
              ((rd^.deftype=procvardef) and (lt=niln)) then
            begin
              calcregisters(self,1,0,0);
              location.loc:=LOC_REGISTER;
              case nodetype of
                 equaln,unequaln : ;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and is_mmx_able_array(ld) and
             is_mmx_able_array(rd) and is_equal(ld,rd) then
            begin
              firstpass(right);
              firstpass(left);
              case nodetype of
                addn,subn,xorn,orn,andn:
                  ;
                { mul is a little bit restricted }
                muln:
                  if not(mmx_type(left.resulttype) in
                    [mmxu16bit,mmxs16bit,mmxfixed16]) then
                    CGMessage(type_e_mismatch);
                else
                  CGMessage(type_e_mismatch);
              end;
              location.loc:=LOC_MMXREGISTER;
              calcregisters(self,0,0,1);
              convdone:=true;
            end
          else
{$endif SUPPORT_MMX}

           { this is a little bit dangerous, also the left type }
           { should be checked! This broke the mmx support      }
           if (rd^.deftype=pointerdef) or
             is_zero_based_array(rd) then
            begin
              if is_zero_based_array(rd) then
                begin
                   resulttype:=new(ppointerdef,init(parraydef(rd)^.elementtype));
                   right:=gentypeconvnode(right,resulttype);
                   firstpass(right);
                end;
              location.loc:=LOC_REGISTER;
              left:=gentypeconvnode(left,s32bitdef);
              firstpass(left);
              calcregisters(self,1,0,0);
              if nodetype=addn then
                begin
                  if not(cs_extsyntax in aktmoduleswitches) or
                    (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                    CGMessage(type_e_mismatch);
                  { Dirty hack, to support multiple firstpasses (PFV) }
                  if (resulttype=nil) and
                     (rd^.deftype=pointerdef) and
                     (ppointerdef(rd)^.pointertype.def^.size>1) then
                   begin
                     left:=caddnode.create(muln,left,genordinalconstnode(ppointerdef(rd)^.pointertype.def^.size,s32bitdef));
                     firstpass(left);
                   end;
                end
              else
                CGMessage(type_e_mismatch);
              convdone:=true;
            end
         else

           if (ld^.deftype=pointerdef) or
             is_zero_based_array(ld) then
            begin
              if is_zero_based_array(ld) then
                begin
                   resulttype:=new(ppointerdef,init(parraydef(ld)^.elementtype));
                   left:=gentypeconvnode(left,resulttype);
                   firstpass(left);
                end;
              location.loc:=LOC_REGISTER;
              right:=gentypeconvnode(right,s32bitdef);
              firstpass(right);
              calcregisters(self,1,0,0);
              case nodetype of
                addn,subn : begin
                              if not(cs_extsyntax in aktmoduleswitches) or
                                 (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                               CGMessage(type_e_mismatch);
                              { Dirty hack, to support multiple firstpasses (PFV) }
                              if (resulttype=nil) and
                                 (ld^.deftype=pointerdef) and
                                 (ppointerdef(ld)^.pointertype.def^.size>1) then
                               begin
                                 right:=caddnode.create(muln,right,
                                   genordinalconstnode(ppointerdef(ld)^.pointertype.def^.size,s32bitdef));
                                 firstpass(right);
                               end;
                            end;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

           if (rd^.deftype=procvardef) and (ld^.deftype=procvardef) and is_equal(rd,ld) then
            begin
              calcregisters(self,1,0,0);
              location.loc:=LOC_REGISTER;
              case nodetype of
                 equaln,unequaln : ;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=enumdef) and (rd^.deftype=enumdef) then
            begin
              if not(is_equal(ld,rd)) then
                begin
                   right:=gentypeconvnode(right,ld);
                   firstpass(right);
                end;
              calcregisters(self,1,0,0);
              case nodetype of
                 equaln,unequaln,
                 ltn,lten,gtn,gten : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end;

         { the general solution is to convert to 32 bit int }
         if not convdone then
           begin
              { but an int/int gives real/real! }
              if nodetype=slashn then
                begin
                   CGMessage(type_h_use_div_for_int);
                   right:=gentypeconvnode(right,bestrealdef^);
                   left:=gentypeconvnode(left,bestrealdef^);
                   firstpass(left);
                   firstpass(right);
                   { maybe we need an integer register to save }
                   { a reference                               }
                   if ((left.location.loc<>LOC_FPU) or
                       (right.location.loc<>LOC_FPU)) and
                       (left.registers32=right.registers32) then
                     calcregisters(self,1,1,0)
                   else
                     calcregisters(self,0,1,0);
                   location.loc:=LOC_FPU;
                end
              else
                begin
                   right:=gentypeconvnode(right,s32bitdef);
                   left:=gentypeconvnode(left,s32bitdef);
                   firstpass(left);
                   firstpass(right);
                   calcregisters(self,1,0,0);
                   location.loc:=LOC_REGISTER;
                end;
           end;

         if codegenerror then
           exit;

         { determines result type for comparions }
         { here the is a problem with multiple passes }
         { example length(s)+1 gets internal 'longint' type first }
         { if it is a arg it is converted to 'LONGINT' }
         { but a second first pass will reset this to 'longint' }
         case nodetype of
            ltn,lten,gtn,gten,equaln,unequaln:
              begin
                 if (not assigned(resulttype)) or
                   (resulttype^.deftype=stringdef) then
                   resulttype:=booldef;
                 if is_64bitint(left.resulttype) then
                   location.loc:=LOC_JUMP
                 else
                   location.loc:=LOC_FLAGS;
              end;
            xorn:
              begin
                if not assigned(resulttype) then
                  resulttype:=left.resulttype;
                 location.loc:=LOC_REGISTER;
              end;
            addn:
              begin
                if not assigned(resulttype) then
                 begin
                 { for strings, return is always a 255 char string }
                   if is_shortstring(left.resulttype) then
                     resulttype:=cshortstringdef
                   else
                    resulttype:=left.resulttype;
                 end;
              end;
            else
              if not assigned(resulttype) then
                resulttype:=left.resulttype;
         end;
      end;

begin
   caddnode:=taddnode;
end.
{
  $Log$
  Revision 1.20  2000-12-31 11:14:10  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.19  2000/12/16 15:55:32  jonas
    + warning when there is a chance to get a range check error because of
      automatic type conversion to u32bit
    * arithmetic operations with a cardinal and a signed operand are carried
      out in 64bit when range checking is on ("merged" from fixes branch)

  Revision 1.18  2000/11/29 00:30:31  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.17  2000/11/20 15:30:42  jonas
    * changed types of values used for constant expression evaluation to
      tconstexprint

  Revision 1.16  2000/11/13 11:30:55  florian
    * some bugs with interfaces and NIL fixed

  Revision 1.15  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.14  2000/10/31 22:02:47  peter
    * symtable splitted, no real code changes

  Revision 1.13  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.12  2000/10/01 19:48:23  peter
    * lot of compile updates for cg11

  Revision 1.11  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.10  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.9  2000/09/27 21:33:22  florian
    * finally nadd.pas compiles

  Revision 1.8  2000/09/27 20:25:44  florian
    * more stuff fixed

  Revision 1.7  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.6  2000/09/24 15:06:19  peter
    * use defines.inc

  Revision 1.5  2000/09/22 22:42:52  florian
    * more fixes

  Revision 1.4  2000/09/21 12:22:42  jonas
    * put piece of code between -dnewoptimizations2 since it wasn't
      necessary otherwise
    + support for full boolean evaluation (from tcadd)

  Revision 1.3  2000/09/20 21:50:59  florian
    * updated

  Revision 1.2  2000/08/29 08:24:45  jonas
    * some modifications to -dcardinalmulfix code

  Revision 1.1  2000/08/26 12:24:20  florian
    * initial release
}

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
interface

    uses
      node;

    type
       taddnode = class(tbinopnode)
          procedure make_bool_equal_size;
          function firstpass : tnode;override;
       end;
       tcaddnode : class of taddnode;

    var
       { caddnode is used to create nodes of the add type }
       { the virtual constructor allows to assign         }
       { another class type to caddnode => processor      }
       { specific node types can be created               }
       caddnode : tcaddnode;

    function isbinaryoverloaded(var p : ptree) : boolean;

implementation

    uses
      globtype,systems,tokens,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      cpuinfo,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      htypechk,pass_1,
      cpubase,tccnv
      ;

    function isbinaryoverloaded(var p : ptree) : boolean;

     var
         rd,ld   : pdef;
         t : ptree;
         optoken : ttoken;

      begin
        isbinaryoverloaded:=false;
        { overloaded operator ? }
        { load easier access variables }
        rd:=p^.right^.resulttype;
        ld:=p^.left^.resulttype;
        if isbinaryoperatoroverloadable(ld,rd,voiddef,p^.treetype) then
          begin
             isbinaryoverloaded:=true;
             {!!!!!!!!! handle paras }
             case p^.treetype of
                { the nil as symtable signs firstcalln that this is
                  an overloaded operator }
                addn:
                  optoken:=_PLUS;
                subn:
                  optoken:=_MINUS;
                muln:
                  optoken:=_STAR;
                starstarn:
                  optoken:=_STARSTAR;
                slashn:
                  optoken:=_SLASH;
                ltn:
                  optoken:=tokens._lt;
                gtn:
                  optoken:=tokens._gt;
                lten:
                  optoken:=_lte;
                gten:
                  optoken:=_gte;
                equaln,unequaln :
                  optoken:=_EQUAL;
                symdifn :
                  optoken:=_SYMDIF;
                modn :
                  optoken:=_OP_MOD;
                orn :
                  optoken:=_OP_OR;
                xorn :
                  optoken:=_OP_XOR;
                andn :
                  optoken:=_OP_AND;
                divn :
                  optoken:=_OP_DIV;
                shln :
                  optoken:=_OP_SHL;
                shrn :
                  optoken:=_OP_SHR;
                else
                  exit;
             end;
             t:=gencallnode(overloaded_operators[optoken],nil);
             { we have to convert p^.left and p^.right into
              callparanodes }
             if t^.symtableprocentry=nil then
               begin
                  CGMessage(parser_e_operator_not_overloaded);
                  putnode(t);
               end
             else
               begin
                  inc(t^.symtableprocentry^.refs);
                  t^.left:=gencallparanode(p^.left,nil);
                  t^.left:=gencallparanode(p^.right,t^.left);
                  if p^.treetype=unequaln then
                   t:=gensinglenode(notn,t);
                  firstpass(t);
                  putnode(p);
                  p:=t;
               end;
          end;
      end;

{*****************************************************************************
                                FirstAdd
*****************************************************************************}

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}

    procedure taddnode.make_bool_equal_size;

      begin
        if porddef(left^.resulttype)^.typ>porddef(right^.resulttype)^.typ then
         begin
           right:=gentypeconvnode(right,porddef(left^.resulttype));
           right^.convtyp:=tc_bool_2_int;
           right^.explizit:=true;
           firstpass(right);
         end
        else
         if porddef(left^.resulttype)^.typ<porddef(right^.resulttype)^.typ then
          begin
            left:=gentypeconvnode(left,porddef(right^.resulttype));
            left^.convtyp:=tc_bool_2_int;
            left^.explizit:=true;
            firstpass(left);
          end;
      end;

    function taddnode.pass_1 : tnode;

      var
         t,hp    : ptree;
         ot,
         lt,rt   : ttreetyp;
         rv,lv   : TConstExprInt;
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
         if is_array_constructor(left^.resulttype) then
           arrayconstructor_to_set(left);
         if is_array_constructor(right^.resulttype) then
           arrayconstructor_to_set(right);

         { both left and right need to be valid }
         set_varstate(left,true);
         set_varstate(right,true);

         { load easier access variables }
         lt:=left^.treetype;
         rt:=right^.treetype;
         rd:=right^.resulttype;
         ld:=left^.resulttype;
         convdone:=false;

         if isbinaryoverloaded(p) then
           exit;
         { compact consts }

         { convert int consts to real consts, if the }
         { other operand is a real const             }
         if (rt=realconstn) and is_constintnode(left) then
           begin
              t:=genrealconstnode(left^.value,right^.resulttype);
              disposetree(left);
              left:=t;
              lt:=realconstn;
           end;
         if (lt=realconstn) and is_constintnode(right) then
           begin
              t:=genrealconstnode(right^.value,left^.resulttype);
              disposetree(right);
              right:=t;
              rt:=realconstn;
           end;

       { both are int constants, also allow operations on two equal enums
         in fpc mode (Needed for conversion of C code) }
         if ((lt=ordconstn) and (rt=ordconstn)) and
            ((is_constintnode(left) and is_constintnode(right)) or
             (is_constboolnode(left) and is_constboolnode(right) and
              (treetype in [ltn,lten,gtn,gten,equaln,unequaln,andn,xorn,orn]))) then
           begin
              { return a boolean for boolean operations (and,xor,or) }
              if is_constboolnode(left) then
               resdef:=booldef
              else
               resdef:=s32bitdef;
              lv:=left^.value;
              rv:=right^.value;
              case treetype of
                addn : t:=genordinalconstnode(lv+rv,resdef);
                subn : t:=genordinalconstnode(lv-rv,resdef);
                muln : t:=genordinalconstnode(lv*rv,resdef);
                xorn : t:=genordinalconstnode(lv xor rv,resdef);
                 orn : t:=genordinalconstnode(lv or rv,resdef);
                andn : t:=genordinalconstnode(lv and rv,resdef);
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
              firstpass(t);
              { the caller disposes the old tree }
              pass_1:=t;
              exit;
           end;

       { both real constants ? }
         if (lt=realconstn) and (rt=realconstn) then
           begin
              lvd:=left^.value_real;
              rvd:=right^.value_real;
              case treetype of
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
              firstpass(t);
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
              s1:=strpnew(char(byte(left^.value)));
              s2:=strpnew(char(byte(right^.value)));
              l1:=1;
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=stringconstn) and (rt=ordconstn) and is_char(rd) then
           begin
              s1:=getpcharcopy(left);
              l1:=left^.length;
              s2:=strpnew(char(byte(right^.value)));
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=ordconstn) and (rt=stringconstn) and is_char(ld) then
           begin
              s1:=strpnew(char(byte(left^.value)));
              l1:=1;
              s2:=getpcharcopy(right);
              l2:=right^.length;
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
              s1:=getpcharcopy(left);
              l1:=left^.length;
              s2:=getpcharcopy(right);
              l2:=right^.length;
              concatstrings:=true;
           end;

         { I will need to translate all this to ansistrings !!! }
         if concatstrings then
           begin
              case treetype of
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
              firstpass(t);
              pass_1:=t;
              exit;
           end;

       { if both are orddefs then check sub types }
         if (ld^.deftype=orddef) and (rd^.deftype=orddef) then
           begin
           { 2 booleans ? }
             if is_boolean(ld) and is_boolean(rd) then
              begin
                case treetype of
                  andn,
                  orn:
                    begin
                      make_bool_equal_size(p);
                      calcregisters(p,0,0,0);
                      location.loc:=LOC_JUMP;
                    end;
                  xorn,ltn,lten,gtn,gten:
                    begin
                      make_bool_equal_size(p);
                      if (left^.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                        (left^.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                        calcregisters(p,2,0,0)
                      else
                        calcregisters(p,1,0,0);
                    end;
                  unequaln,
                  equaln:
                    begin
                      make_bool_equal_size(p);
                      { Remove any compares with constants }
                      if (left^.treetype=ordconstn) then
                       begin
                         hp:=right;
                         b:=(left^.value<>0);
                         ot:=treetype;
                         disposetree(left);
                         putnode(p);
                         p:=hp;
                         if (not(b) and (ot=equaln)) or
                            (b and (ot=unequaln)) then
                          begin
                            p:=gensinglenode(notn,p);
                            firstpass(p);
                          end;
                         exit;
                       end;
                      if (right^.treetype=ordconstn) then
                       begin
                         hp:=left;
                         b:=(right^.value<>0);
                         ot:=treetype;
                         disposetree(right);
                         putnode(p);
                         p:=hp;
                         if (not(b) and (ot=equaln)) or
                            (b and (ot=unequaln)) then
                          begin
                            p:=gensinglenode(notn,p);
                            firstpass(p);
                          end;
                         exit;
                       end;
                      if (left^.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                        (left^.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                        calcregisters(p,2,0,0)
                      else
                        calcregisters(p,1,0,0);
                    end;
                else
                  CGMessage(type_e_mismatch);
                end;

                { these one can't be in flags! }
                if treetype in [xorn,unequaln,equaln] then
                  begin
                     if left^.location.loc=LOC_FLAGS then
                       begin
                          left:=gentypeconvnode(left,porddef(left^.resulttype));
                          left^.convtyp:=tc_bool_2_int;
                          left^.explizit:=true;
                          firstpass(left);
                       end;
                     if right^.location.loc=LOC_FLAGS then
                       begin
                          right:=gentypeconvnode(right,porddef(right^.resulttype));
                          right^.convtyp:=tc_bool_2_int;
                          right^.explizit:=true;
                          firstpass(right);
                       end;
                     { readjust registers }
                     calcregisters(p,1,0,0);
                  end;
                convdone:=true;
              end
             else
             { Both are chars? only convert to shortstrings for addn }
              if is_char(rd) and is_char(ld) then
               begin
                 if treetype=addn then
                   begin
                     left:=gentypeconvnode(left,cshortstringdef);
                     right:=gentypeconvnode(right,cshortstringdef);
                     firstpass(left);
                     firstpass(right);
                     { here we call STRCOPY }
                     procinfo^.flags:=procinfo^.flags or pi_do_call;
                     calcregisters(p,0,0,0);
                     location.loc:=LOC_MEM;
                   end
                 else
                   calcregisters(p,1,0,0);
                 convdone:=true;
               end
              { is there a 64 bit type ? }
             else if ((porddef(rd)^.typ=s64bit) or (porddef(ld)^.typ=s64bit)) and
               { the / operator is handled later }
               (treetype<>slashn) then
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
                  calcregisters(p,2,0,0);
                  convdone:=true;
               end
             else if ((porddef(rd)^.typ=u64bit) or (porddef(ld)^.typ=u64bit)) and
               { the / operator is handled later }
               (treetype<>slashn) then
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
                  calcregisters(p,2,0,0);
                  convdone:=true;
               end
             else
              { is there a cardinal? }
              if ((porddef(rd)^.typ=u32bit) or (porddef(ld)^.typ=u32bit)) and
               { the / operator is handled later }
               (treetype<>slashn) then
               begin
                 { convert constants to u32bit }
{$ifndef cardinalmulfix}
                 if (porddef(ld)^.typ<>u32bit) then
                  begin
                    { s32bit will be used for when the other is also s32bit }

  { the following line doesn't make any sense: it's the same as        }
  {  if ((porddef(rd)^.typ=u32bit) or (porddef(ld)^.typ=u32bit)) and   }
  {      (porddef(ld)^.typ<>u32bit) and (porddef(rd)^.typ=s32bit) then }
  { which can be simplified to                                         }
  {  if ((porddef(rd)^.typ=u32bit) and (porddef(rd)^.typ=s32bit) then  }
  { which can never be true (JM)                                       }
                    if (porddef(rd)^.typ=s32bit) and (lt<>ordconstn) then
                     left:=gentypeconvnode(left,s32bitdef)
                    else
                     left:=gentypeconvnode(left,u32bitdef);
                    firstpass(left);
                  end;
                 if (porddef(rd)^.typ<>u32bit) then
                  begin
                    { s32bit will be used for when the other is also s32bit }
                    if (porddef(ld)^.typ=s32bit) and (rt<>ordconstn) then
                     right:=gentypeconvnode(right,s32bitdef)
                    else
                     right:=gentypeconvnode(right,u32bitdef);
                    firstpass(right);
                  end;
{$else cardinalmulfix}
                 { only do a conversion if the nodes have different signs }
                 if (porddef(rd)^.typ=u32bit) xor (porddef(ld)^.typ=u32bit) then
                   if (porddef(rd)^.typ=u32bit) then
                     begin
                     { can we make them both unsigned? }
                       if is_constintnode(left) and
                          ((treetype <> subn) and
                           (left^.value > 0)) then
                         left:=gentypeconvnode(left,u32bitdef)
                       else
                         left:=gentypeconvnode(left,s32bitdef);
                       firstpass(left);
                     end
                   else {if (porddef(ld)^.typ=u32bit) then}
                     begin
                     { can we make them both unsigned? }
                       if is_constintnode(right) and
                          (right^.value > 0) then
                         right:=gentypeconvnode(right,u32bitdef)
                       else
                         right:=gentypeconvnode(right,s32bitdef);
                       firstpass(right);
                     end;
{$endif cardinalmulfix}
                 calcregisters(p,1,0,0);
                 { for unsigned mul we need an extra register }
{                 registers32:=left^.registers32+right^.registers32; }
                 if treetype=muln then
                  inc(registers32);
                 convdone:=true;
               end;
           end
         else

         { left side a setdef, must be before string processing,
           else array constructor can be seen as array of char (PFV) }
           if (ld^.deftype=setdef) {or is_array_constructor(ld)} then
             begin
             { trying to add a set element? }
                if (treetype=addn) and (rd^.deftype<>setdef) then
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
                   if not(treetype in [addn,subn,symdifn,muln,equaln,unequaln
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
                   assigned(right^.right) then
                 begin
                   { generate a temporary normset def }
                   tempdef:=new(psetdef,init(psetdef(ld)^.elementtype.def,255));
                   left:=gentypeconvnode(left,tempdef);
                   firstpass(left);
                   dispose(tempdef,done);
                   ld:=left^.resulttype;
                 end;

                { if the destination is not a smallset then insert a typeconv
                  which loads a smallset into a normal set }
                if (psetdef(ld)^.settype<>smallset) and
                   (psetdef(rd)^.settype=smallset) then
                 begin
                   if (right^.treetype=setconstn) then
                     begin
                        t:=gensetconstnode(right^.value_set,psetdef(left^.resulttype));
                        t^.left:=right^.left;
                        putnode(right);
                        right:=t;
                     end
                   else
                     right:=gentypeconvnode(right,psetdef(left^.resulttype));
                   firstpass(right);
                 end;

                { do constant evaluation }
                if (right^.treetype=setconstn) and
                   not assigned(right^.left) and
                   (left^.treetype=setconstn) and
                   not assigned(left^.left) then
                  begin
                     new(resultset);
                     case treetype of
                        addn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      right^.value_set^[i] or left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        muln : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      right^.value_set^[i] and left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        subn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      left^.value_set^[i] and not(right^.value_set^[i]);
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                     symdifn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      left^.value_set^[i] xor right^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                    unequaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if right^.value_set^[i]=left^.value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef);
                               end;
                      equaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if right^.value_set^[i]<>left^.value_set^[i] then
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
                                  If (right^.value_set^[i] And left^.value_set^[i]) <>
                                      left^.value_set^[i] Then
                                    Begin
                                      b := false;
                                      Break
                                    End;
                                t := genordinalconstnode(ord(b),booldef);
                              End;
                       gten : Begin
                                b := true;
                                For i := 0 to 31 Do
                                  If (left^.value_set^[i] And right^.value_set^[i]) <>
                                      right^.value_set^[i] Then
                                    Begin
                                      b := false;
                                      Break
                                    End;
                                t := genordinalconstnode(ord(b),booldef);
                              End;
{$EndIf NoSetInclusion}
                     end;
                     dispose(resultset);
                     disposetree(p);
                     p:=t;
                     firstpass(p);
                     exit;
                  end
                else
                 if psetdef(ld)^.settype=smallset then
                  begin
                     { are we adding set elements ? }
                     if right^.treetype=setelementn then
                       calcregisters(p,2,0,0)
                     else
                       calcregisters(p,1,0,0);
                     location.loc:=LOC_REGISTER;
                  end
                 else
                  begin
                     calcregisters(p,0,0,0);
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
               calcregisters(p,1,0,0);
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
                   if not(is_shortstring(rd))
{$ifdef newoptimizations2}
{$ifdef i386}
                      { shortstring + char handled seperately  (JM) }
                      and (not(cs_optimize in aktglobalswitches) or
                           (treetype <> addn) or not(is_char(rd)))
{$endif i386}
{$endif newoptimizations2}
                    then
                      right:=gentypeconvnode(right,cshortstringdef);
                   if not(is_shortstring(ld)) then
                     left:=gentypeconvnode(left,cshortstringdef);
                   resulttype:=cshortstringdef;
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_MEM;
                end;
              { only if there is a type cast we need to do again }
              { the first pass                             }
              if left^.treetype=typeconvn then
                firstpass(left);
              if right^.treetype=typeconvn then
                firstpass(right);
              { here we call STRCONCAT or STRCMP or STRCOPY }
              procinfo^.flags:=procinfo^.flags or pi_do_call;
              if location.loc=LOC_MEM then
                calcregisters(p,0,0,0)
              else
                calcregisters(p,1,0,0);
{$ifdef i386}
              { not always necessary, only if it is not a constant char and }
              { not a regvar, but don't know how to check this here (JM)    }
              if is_char(rd) then
                inc(registers32);
{$endif i386}
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
                 if not is_integer(rd) or (treetype<>muln) then
                   right:=gentypeconvnode(right,s32fixeddef);
                 if not is_integer(ld) or (treetype<>muln) then
                   left:=gentypeconvnode(left,s32fixeddef);
                 firstpass(left);
                 firstpass(right);
                 calcregisters(p,1,0,0);
                 location.loc:=LOC_REGISTER;
               end
              else
              { convert both to bestreal }
                begin
                  right:=gentypeconvnode(right,bestrealdef^);
                  left:=gentypeconvnode(left,bestrealdef^);
                  firstpass(left);
                  firstpass(right);
                  calcregisters(p,0,1,0);
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
              calcregisters(p,1,0,0);
              case treetype of
                 equaln,unequaln :
                   begin
                      if is_equal(right^.resulttype,voidpointerdef) then
                        begin
                           right:=gentypeconvnode(right,ld);
                           firstpass(right);
                        end
                      else if is_equal(left^.resulttype,voidpointerdef) then
                        begin
                           left:=gentypeconvnode(left,rd);
                           firstpass(left);
                        end
                      else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                   end;
                 ltn,lten,gtn,gten:
                   begin
                      if is_equal(right^.resulttype,voidpointerdef) then
                        begin
                           right:=gentypeconvnode(right,ld);
                           firstpass(right);
                        end
                      else if is_equal(left^.resulttype,voidpointerdef) then
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

           if (rd^.deftype=objectdef) and (ld^.deftype=objectdef) and
              pobjectdef(rd)^.is_class and pobjectdef(ld)^.is_class then
            begin
              location.loc:=LOC_REGISTER;
              if pobjectdef(rd)^.is_related(pobjectdef(ld)) then
                right:=gentypeconvnode(right,ld)
              else
                left:=gentypeconvnode(left,rd);
              firstpass(right);
              firstpass(left);
              calcregisters(p,1,0,0);
              case treetype of
                 equaln,unequaln : ;
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
              calcregisters(p,1,0,0);
              case treetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

         { allows comperasion with nil pointer }
           if (rd^.deftype=objectdef) and
              pobjectdef(rd)^.is_class then
            begin
              location.loc:=LOC_REGISTER;
              left:=gentypeconvnode(left,rd);
              firstpass(left);
              calcregisters(p,1,0,0);
              case treetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=objectdef) and
              pobjectdef(ld)^.is_class then
            begin
              location.loc:=LOC_REGISTER;
              right:=gentypeconvnode(right,ld);
              firstpass(right);
              calcregisters(p,1,0,0);
              case treetype of
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
              calcregisters(p,1,0,0);
              case treetype of
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
              calcregisters(p,1,0,0);
              case treetype of
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
              calcregisters(p,1,0,0);
              location.loc:=LOC_REGISTER;
              case treetype of
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
              case treetype of
                addn,subn,xorn,orn,andn:
                  ;
                { mul is a little bit restricted }
                muln:
                  if not(mmx_type(left^.resulttype) in
                    [mmxu16bit,mmxs16bit,mmxfixed16]) then
                    CGMessage(type_e_mismatch);
                else
                  CGMessage(type_e_mismatch);
              end;
              location.loc:=LOC_MMXREGISTER;
              calcregisters(p,0,0,1);
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
              calcregisters(p,1,0,0);
              if treetype=addn then
                begin
                  if not(cs_extsyntax in aktmoduleswitches) or
                    (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                    CGMessage(type_e_mismatch);
                  { Dirty hack, to support multiple firstpasses (PFV) }
                  if (resulttype=nil) and
                     (rd^.deftype=pointerdef) and
                     (ppointerdef(rd)^.pointertype.def^.size>1) then
                   begin
                     left:=gennode(muln,left,genordinalconstnode(ppointerdef(rd)^.pointertype.def^.size,s32bitdef));
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
              calcregisters(p,1,0,0);
              case treetype of
                addn,subn : begin
                              if not(cs_extsyntax in aktmoduleswitches) or
                                 (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                               CGMessage(type_e_mismatch);
                              { Dirty hack, to support multiple firstpasses (PFV) }
                              if (resulttype=nil) and
                                 (ld^.deftype=pointerdef) and
                                 (ppointerdef(ld)^.pointertype.def^.size>1) then
                               begin
                                 right:=gennode(muln,right,
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
              calcregisters(p,1,0,0);
              location.loc:=LOC_REGISTER;
              case treetype of
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
              calcregisters(p,1,0,0);
              case treetype of
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
              if treetype=slashn then
                begin
                   CGMessage(type_h_use_div_for_int);
                   right:=gentypeconvnode(right,bestrealdef^);
                   left:=gentypeconvnode(left,bestrealdef^);
                   firstpass(left);
                   firstpass(right);
                   { maybe we need an integer register to save }
                   { a reference                               }
                   if ((left^.location.loc<>LOC_FPU) or
                       (right^.location.loc<>LOC_FPU)) and
                       (left^.registers32=right^.registers32) then
                     calcregisters(p,1,1,0)
                   else
                     calcregisters(p,0,1,0);
                   location.loc:=LOC_FPU;
                end
              else
                begin
                   right:=gentypeconvnode(right,s32bitdef);
                   left:=gentypeconvnode(left,s32bitdef);
                   firstpass(left);
                   firstpass(right);
                   calcregisters(p,1,0,0);
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
         case treetype of
            ltn,lten,gtn,gten,equaln,unequaln:
              begin
                 if (not assigned(resulttype)) or
                   (resulttype^.deftype=stringdef) then
                   resulttype:=booldef;
                 if is_64bitint(left^.resulttype) then
                   location.loc:=LOC_JUMP
                 else
                   location.loc:=LOC_FLAGS;
              end;
            xorn:
              begin
                if not assigned(resulttype) then
                  resulttype:=left^.resulttype;
                 location.loc:=LOC_REGISTER;
              end;
            addn:
              begin
                if not assigned(resulttype) then
                 begin
                 { for strings, return is always a 255 char string }
                   if is_shortstring(left^.resulttype) then
                     resulttype:=cshortstringdef
                   else
                    resulttype:=left^.resulttype;
                 end;
              end;
{$ifdef cardinalmulfix}
            muln:
  { if we multiply an unsigned with a signed number, the result is signed  }
  { in the other cases, the result remains signed or unsigned depending on }
  { the multiplication factors (JM)                                        }
              if (left^.resulttype^.deftype = orddef) and
                 (right^.resulttype^.deftype = orddef) and
                 is_signed(right^.resulttype) then
                resulttype := right^.resulttype
              else resulttype := left^.resulttype;
(*
            subn:
 { if we substract a u32bit from a positive constant, the result becomes }
 { s32bit as well (JM)                                                   }
              begin
                if (right^.resulttype^.deftype = orddef) and
                   (left^.resulttype^.deftype = orddef) and
                   (porddef(right^.resulttype)^.typ = u32bit) and
                   is_constintnode(left) and
{                   (porddef(left^.resulttype)^.typ <> u32bit) and}
                   (left^.value > 0) then
                  begin
                    left := gentypeconvnode(left,u32bitdef);
                    firstpass(left);
                  end;
                resulttype:=left^.resulttype;
              end;
*)
{$endif cardinalmulfix}
            else
              resulttype:=left^.resulttype;
         end;
      end;

begin
   caddnode:=taddnode;
end.
{
  $Log$
  Revision 1.1  2000-08-26 12:24:20  florian
    * initial release

}
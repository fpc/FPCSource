{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for add node

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
unit tcadd;
interface

    uses
      tree;

    procedure firstadd(var p : ptree);
    function isbinaryoverloaded(var p : ptree) : boolean;


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
        if (p^.treetype=starstarn) or
           (ld^.deftype=recorddef) or
           (rd^.deftype=recorddef) or
           { array def, but not mmx or chararray+[char,string,chararray] }
           ((ld^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(ld)) and
            not(is_chararray(ld) and
                (is_char(rd) or
		 is_pchar(rd) or
                 (rd^.deftype=stringdef) or
                 is_chararray(rd)))
           ) or
           ((rd^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(rd)) and
            not(is_chararray(rd) and
                (is_char(ld) or
		 is_pchar(ld) or
                 (ld^.deftype=stringdef) or
                 is_chararray(ld)))
           ) or
           { <> and = are defined for classes }
           ((ld^.deftype=objectdef) and
            (not(pobjectdef(ld)^.is_class) or
             not(p^.treetype in [equaln,unequaln])
            )
           ) or
           ((rd^.deftype=objectdef) and
            (not(pobjectdef(rd)^.is_class) or
             not(p^.treetype in [equaln,unequaln])
            )
           ) then
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

    procedure firstadd(var p : ptree);

      procedure make_bool_equal_size(var p:ptree);
      begin
        if porddef(p^.left^.resulttype)^.typ>porddef(p^.right^.resulttype)^.typ then
         begin
           p^.right:=gentypeconvnode(p^.right,porddef(p^.left^.resulttype));
           p^.right^.convtyp:=tc_bool_2_int;
           p^.right^.explizit:=true;
           firstpass(p^.right);
         end
        else
         if porddef(p^.left^.resulttype)^.typ<porddef(p^.right^.resulttype)^.typ then
          begin
            p^.left:=gentypeconvnode(p^.left,porddef(p^.right^.resulttype));
            p^.left^.convtyp:=tc_bool_2_int;
            p^.left^.explizit:=true;
            firstpass(p^.left);
          end;
      end;

      var
         t,hp    : ptree;
         ot,
         lt,rt   : ttreetyp;
         rv,lv   : longint;
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
         { first do the two subtrees }
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
           exit;

         { convert array constructors to sets, because there is no other operator
           possible for array constructors }
         if is_array_constructor(p^.left^.resulttype) then
           arrayconstructor_to_set(p^.left);
         if is_array_constructor(p^.right^.resulttype) then
           arrayconstructor_to_set(p^.right);

         { both left and right need to be valid }
         set_varstate(p^.left,true);
         set_varstate(p^.right,true);

         { load easier access variables }
         lt:=p^.left^.treetype;
         rt:=p^.right^.treetype;
         rd:=p^.right^.resulttype;
         ld:=p^.left^.resulttype;
         convdone:=false;

         if isbinaryoverloaded(p) then
           exit;
         { compact consts }

         { convert int consts to real consts, if the }
         { other operand is a real const             }
         if (rt=realconstn) and is_constintnode(p^.left) then
           begin
              t:=genrealconstnode(p^.left^.value,p^.right^.resulttype);
              disposetree(p^.left);
              p^.left:=t;
              lt:=realconstn;
           end;
         if (lt=realconstn) and is_constintnode(p^.right) then
           begin
              t:=genrealconstnode(p^.right^.value,p^.left^.resulttype);
              disposetree(p^.right);
              p^.right:=t;
              rt:=realconstn;
           end;

       { both are int constants, also allow operations on two equal enums
         in fpc mode (Needed for conversion of C code) }
         if ((lt=ordconstn) and (rt=ordconstn)) and
            ((is_constintnode(p^.left) and is_constintnode(p^.right)) or
             (is_constboolnode(p^.left) and is_constboolnode(p^.right) and
              (p^.treetype in [ltn,lten,gtn,gten,equaln,unequaln,andn,xorn,orn]))) then
           begin
              { return a boolean for boolean operations (and,xor,or) }
              if is_constboolnode(p^.left) then
               resdef:=booldef
              else
               resdef:=s32bitdef;
              lv:=p^.left^.value;
              rv:=p^.right^.value;
              case p^.treetype of
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
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;

       { both real constants ? }
         if (lt=realconstn) and (rt=realconstn) then
           begin
              lvd:=p^.left^.value_real;
              rvd:=p^.right^.value_real;
              case p^.treetype of
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
              disposetree(p);
              p:=t;
              firstpass(p);
              exit;
           end;

       { concating strings ? }
         concatstrings:=false;
         s1:=nil;
         s2:=nil;
         if (lt=ordconstn) and (rt=ordconstn) and
            is_char(ld) and is_char(rd) then
           begin
              s1:=strpnew(char(byte(p^.left^.value)));
              s2:=strpnew(char(byte(p^.right^.value)));
              l1:=1;
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=stringconstn) and (rt=ordconstn) and is_char(rd) then
           begin
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=strpnew(char(byte(p^.right^.value)));
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=ordconstn) and (rt=stringconstn) and is_char(ld) then
           begin
              s1:=strpnew(char(byte(p^.left^.value)));
              l1:=1;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
              concatstrings:=true;
           end;

         { I will need to translate all this to ansistrings !!! }
         if concatstrings then
           begin
              case p^.treetype of
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
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;

       { if both are orddefs then check sub types }
         if (ld^.deftype=orddef) and (rd^.deftype=orddef) then
           begin
           { 2 booleans ? }
             if is_boolean(ld) and is_boolean(rd) then
              begin
                case p^.treetype of
                  andn,
                  orn:
                    begin
                      make_bool_equal_size(p);
                      calcregisters(p,0,0,0);
                      p^.location.loc:=LOC_JUMP;
                    end;
                  xorn,ltn,lten,gtn,gten:
                    begin
                      make_bool_equal_size(p);
                      if (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                        (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                        calcregisters(p,2,0,0)
                      else
                        calcregisters(p,1,0,0);
                    end;
                  unequaln,
                  equaln:
                    begin
                      make_bool_equal_size(p);
                      { Remove any compares with constants }
                      if (p^.left^.treetype=ordconstn) then
                       begin
                         hp:=p^.right;
                         b:=(p^.left^.value<>0);
                         ot:=p^.treetype;
                         disposetree(p^.left);
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
                      if (p^.right^.treetype=ordconstn) then
                       begin
                         hp:=p^.left;
                         b:=(p^.right^.value<>0);
                         ot:=p^.treetype;
                         disposetree(p^.right);
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
                      if (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                        (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                        calcregisters(p,2,0,0)
                      else
                        calcregisters(p,1,0,0);
                    end;
                else
                  CGMessage(type_e_mismatch);
                end;

                { these one can't be in flags! }
                if p^.treetype in [xorn,unequaln,equaln] then
                  begin
                     if p^.left^.location.loc=LOC_FLAGS then
                       begin
                          p^.left:=gentypeconvnode(p^.left,porddef(p^.left^.resulttype));
                          p^.left^.convtyp:=tc_bool_2_int;
                          p^.left^.explizit:=true;
                          firstpass(p^.left);
                       end;
                     if p^.right^.location.loc=LOC_FLAGS then
                       begin
                          p^.right:=gentypeconvnode(p^.right,porddef(p^.right^.resulttype));
                          p^.right^.convtyp:=tc_bool_2_int;
                          p^.right^.explizit:=true;
                          firstpass(p^.right);
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
                 if p^.treetype=addn then
                   begin
                     p^.left:=gentypeconvnode(p^.left,cshortstringdef);
                     p^.right:=gentypeconvnode(p^.right,cshortstringdef);
                     firstpass(p^.left);
                     firstpass(p^.right);
                     { here we call STRCOPY }
                     procinfo^.flags:=procinfo^.flags or pi_do_call;
                     calcregisters(p,0,0,0);
                     p^.location.loc:=LOC_MEM;
                   end
                 else
                   calcregisters(p,1,0,0);
                 convdone:=true;
               end
              { is there a 64 bit type ? }
             else if ((porddef(rd)^.typ=s64bit) or (porddef(ld)^.typ=s64bit)) and
               { the / operator is handled later }
               (p^.treetype<>slashn) then
               begin
                  if (porddef(ld)^.typ<>s64bit) then
                    begin
                      p^.left:=gentypeconvnode(p^.left,cs64bitdef);
                      firstpass(p^.left);
                    end;
                  if (porddef(rd)^.typ<>s64bit) then
                    begin
                       p^.right:=gentypeconvnode(p^.right,cs64bitdef);
                       firstpass(p^.right);
                    end;
                  calcregisters(p,2,0,0);
                  convdone:=true;
               end
             else if ((porddef(rd)^.typ=u64bit) or (porddef(ld)^.typ=u64bit)) and
               { the / operator is handled later }
               (p^.treetype<>slashn) then
               begin
                  if (porddef(ld)^.typ<>u64bit) then
                    begin
                      p^.left:=gentypeconvnode(p^.left,cu64bitdef);
                      firstpass(p^.left);
                    end;
                  if (porddef(rd)^.typ<>u64bit) then
                    begin
                       p^.right:=gentypeconvnode(p^.right,cu64bitdef);
                       firstpass(p^.right);
                    end;
                  calcregisters(p,2,0,0);
                  convdone:=true;
               end
             else
              { is there a cardinal? }
              if ((porddef(rd)^.typ=u32bit) or (porddef(ld)^.typ=u32bit)) and
               { the / operator is handled later }
               (p^.treetype<>slashn) then
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
                     p^.left:=gentypeconvnode(p^.left,s32bitdef)
                    else
                     p^.left:=gentypeconvnode(p^.left,u32bitdef);
                    firstpass(p^.left);
                  end;
                 if (porddef(rd)^.typ<>u32bit) then
                  begin
                    { s32bit will be used for when the other is also s32bit }
                    if (porddef(ld)^.typ=s32bit) and (rt<>ordconstn) then
                     p^.right:=gentypeconvnode(p^.right,s32bitdef)
                    else
                     p^.right:=gentypeconvnode(p^.right,u32bitdef);
                    firstpass(p^.right);
                  end;
{$else cardinalmulfix}
                 { only do a conversion if the nodes have different signs }
                 if (porddef(rd)^.typ=u32bit) xor (porddef(ld)^.typ=u32bit) then
                   if (porddef(rd)^.typ=u32bit) then
                     begin
                     { can we make them both unsigned? }
                       if is_constintnode(p^.left) and
                          ((p^.treetype <> subn) and
                           (p^.left^.value > 0)) then
                         p^.left:=gentypeconvnode(p^.left,u32bitdef)
                       else
                         p^.left:=gentypeconvnode(p^.left,s32bitdef);
                       firstpass(p^.left);
                     end
                   else {if (porddef(ld)^.typ=u32bit) then}
                     begin
                     { can we make them both unsigned? }
                       if is_constintnode(p^.right) and
                          (p^.right^.value > 0) then
                         p^.right:=gentypeconvnode(p^.right,u32bitdef)
                       else
                         p^.right:=gentypeconvnode(p^.right,s32bitdef);
                       firstpass(p^.right);
                     end;
{$endif cardinalmulfix}
                 calcregisters(p,1,0,0);
                 { for unsigned mul we need an extra register }
{                 p^.registers32:=p^.left^.registers32+p^.right^.registers32; }
                 if p^.treetype=muln then
                  inc(p^.registers32);
                 convdone:=true;
               end;
           end
         else

         { left side a setdef, must be before string processing,
           else array constructor can be seen as array of char (PFV) }
           if (ld^.deftype=setdef) {or is_array_constructor(ld)} then
             begin
             { trying to add a set element? }
                if (p^.treetype=addn) and (rd^.deftype<>setdef) then
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
                   if not(p^.treetype in [addn,subn,symdifn,muln,equaln,unequaln
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
                   assigned(p^.right^.right) then
                 begin
                   { generate a temporary normset def }
                   tempdef:=new(psetdef,init(psetdef(ld)^.elementtype.def,255));
                   p^.left:=gentypeconvnode(p^.left,tempdef);
                   firstpass(p^.left);
                   dispose(tempdef,done);
                   ld:=p^.left^.resulttype;
                 end;

                { if the destination is not a smallset then insert a typeconv
                  which loads a smallset into a normal set }
                if (psetdef(ld)^.settype<>smallset) and
                   (psetdef(rd)^.settype=smallset) then
                 begin
                   if (p^.right^.treetype=setconstn) then
                     begin
                        t:=gensetconstnode(p^.right^.value_set,psetdef(p^.left^.resulttype));
                        t^.left:=p^.right^.left;
                        putnode(p^.right);
                        p^.right:=t;
                     end
                   else
                     p^.right:=gentypeconvnode(p^.right,psetdef(p^.left^.resulttype));
                   firstpass(p^.right);
                 end;

                { do constant evaluation }
                if (p^.right^.treetype=setconstn) and
                   not assigned(p^.right^.left) and
                   (p^.left^.treetype=setconstn) and
                   not assigned(p^.left^.left) then
                  begin
                     new(resultset);
                     case p^.treetype of
                        addn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.value_set^[i] or p^.left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        muln : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.value_set^[i] and p^.left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        subn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.value_set^[i] and not(p^.right^.value_set^[i]);
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                     symdifn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.value_set^[i] xor p^.right^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                    unequaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if p^.right^.value_set^[i]=p^.left^.value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef);
                               end;
                      equaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if p^.right^.value_set^[i]<>p^.left^.value_set^[i] then
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
                                  If (p^.right^.value_set^[i] And p^.left^.value_set^[i]) <>
                                      p^.left^.value_set^[i] Then
                                    Begin
                                      b := false;
                                      Break
                                    End;
                                t := genordinalconstnode(ord(b),booldef);
                              End;
                       gten : Begin
                                b := true;
                                For i := 0 to 31 Do
                                  If (p^.left^.value_set^[i] And p^.right^.value_set^[i]) <>
                                      p^.right^.value_set^[i] Then
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
                     if p^.right^.treetype=setelementn then
                       calcregisters(p,2,0,0)
                     else
                       calcregisters(p,1,0,0);
                     p^.location.loc:=LOC_REGISTER;
                  end
                 else
                  begin
                     calcregisters(p,0,0,0);
                     { here we call SET... }
                     procinfo^.flags:=procinfo^.flags or pi_do_call;
                     p^.location.loc:=LOC_MEM;
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
                   p^.right:=gentypeconvnode(p^.right,ld);
                   firstpass(p^.right);
                 end
               else
                 begin
                   p^.left:=gentypeconvnode(p^.left,rd);
                   firstpass(p^.left);
                 end;
               p^.location.loc:=LOC_REGISTER;
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
                     p^.right:=gentypeconvnode(p^.right,cwidestringdef);
                   if not(is_widestring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,cwidestringdef);
                   p^.resulttype:=cwidestringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_REGISTER;
                end
              else if is_ansistring(rd) or is_ansistring(ld) then
                begin
                   if not(is_ansistring(rd)) then
                     p^.right:=gentypeconvnode(p^.right,cansistringdef);
                   if not(is_ansistring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,cansistringdef);
                   { we use ansistrings so no fast exit here }
                   procinfo^.no_fast_exit:=true;
                   p^.resulttype:=cansistringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_REGISTER;
                end
              else if is_longstring(rd) or is_longstring(ld) then
                begin
                   if not(is_longstring(rd)) then
                     p^.right:=gentypeconvnode(p^.right,clongstringdef);
                   if not(is_longstring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,clongstringdef);
                   p^.resulttype:=clongstringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_MEM;
                end
              else
                begin
                   if not(is_shortstring(rd))
{$ifdef newoptimizations2}
{$ifdef i386}
                      { shortstring + char handled seperately  (JM) }
                      and (not(cs_optimize in aktglobalswitches) or
                           (p^.treetype <> addn) or not(is_char(rd)))
{$endif i386}
{$endif newoptimizations2}
                    then
                      p^.right:=gentypeconvnode(p^.right,cshortstringdef);
                   if not(is_shortstring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,cshortstringdef);
                   p^.resulttype:=cshortstringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_MEM;
                end;
              { only if there is a type cast we need to do again }
              { the first pass                             }
              if p^.left^.treetype=typeconvn then
                firstpass(p^.left);
              if p^.right^.treetype=typeconvn then
                firstpass(p^.right);
              { here we call STRCONCAT or STRCMP or STRCOPY }
              procinfo^.flags:=procinfo^.flags or pi_do_call;
              if p^.location.loc=LOC_MEM then
                calcregisters(p,0,0,0)
              else
                calcregisters(p,1,0,0);
{$ifdef newoptimizations}
{$ifdef i386}
              { not always necessary, only if it is not a constant char and }
              { not a regvar, but don't know how to check this here (JM)    }
              if is_char(rd) then
                inc(p^.registers32);
{$endif i386}
{$endif newoptimizations}
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
                 if not is_integer(rd) or (p^.treetype<>muln) then
                   p^.right:=gentypeconvnode(p^.right,s32fixeddef);
                 if not is_integer(ld) or (p^.treetype<>muln) then
                   p^.left:=gentypeconvnode(p^.left,s32fixeddef);
                 firstpass(p^.left);
                 firstpass(p^.right);
                 calcregisters(p,1,0,0);
                 p^.location.loc:=LOC_REGISTER;
               end
              else
              { convert both to bestreal }
                begin
                  p^.right:=gentypeconvnode(p^.right,bestrealdef^);
                  p^.left:=gentypeconvnode(p^.left,bestrealdef^);
                  firstpass(p^.left);
                  firstpass(p^.right);
                  calcregisters(p,0,1,0);
                  p^.location.loc:=LOC_FPU;
                end;
              convdone:=true;
            end
         else

         { pointer comperation and subtraction }
           if (rd^.deftype=pointerdef) and (ld^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              { p^.right:=gentypeconvnode(p^.right,ld); }
              { firstpass(p^.right); }
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln :
                   begin
                      if is_equal(p^.right^.resulttype,voidpointerdef) then
                        begin
                           p^.right:=gentypeconvnode(p^.right,ld);
                           firstpass(p^.right);
                        end
                      else if is_equal(p^.left^.resulttype,voidpointerdef) then
                        begin
                           p^.left:=gentypeconvnode(p^.left,rd);
                           firstpass(p^.left);
                        end
                      else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                   end;
                 ltn,lten,gtn,gten:
                   begin
                      if is_equal(p^.right^.resulttype,voidpointerdef) then
                        begin
                           p^.right:=gentypeconvnode(p^.right,ld);
                           firstpass(p^.right);
                        end
                      else if is_equal(p^.left^.resulttype,voidpointerdef) then
                        begin
                           p^.left:=gentypeconvnode(p^.left,rd);
                           firstpass(p^.left);
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
                      p^.resulttype:=s32bitdef;
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
              p^.location.loc:=LOC_REGISTER;
              if pobjectdef(rd)^.is_related(pobjectdef(ld)) then
                p^.right:=gentypeconvnode(p^.right,ld)
              else
                p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.right);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) and (ld^.deftype=classrefdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              if pobjectdef(pclassrefdef(rd)^.pointertype.def)^.is_related(pobjectdef(
                pclassrefdef(ld)^.pointertype.def)) then
                p^.right:=gentypeconvnode(p^.right,ld)
              else
                p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.right);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
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
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=objectdef) and
              pobjectdef(ld)^.is_class then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) then
            begin
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=classrefdef) then
            begin
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
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
              p^.location.loc:=LOC_REGISTER;
              case p^.treetype of
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
              firstpass(p^.right);
              firstpass(p^.left);
              case p^.treetype of
                addn,subn,xorn,orn,andn:
                  ;
                { mul is a little bit restricted }
                muln:
                  if not(mmx_type(p^.left^.resulttype) in
                    [mmxu16bit,mmxs16bit,mmxfixed16]) then
                    CGMessage(type_e_mismatch);
                else
                  CGMessage(type_e_mismatch);
              end;
              p^.location.loc:=LOC_MMXREGISTER;
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
                   p^.resulttype:=new(ppointerdef,init(parraydef(rd)^.elementtype));
                   p^.right:=gentypeconvnode(p^.right,p^.resulttype);
                   firstpass(p^.right);
                end;
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              if p^.treetype=addn then
                begin
                  if not(cs_extsyntax in aktmoduleswitches) or
                    (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                    CGMessage(type_e_mismatch);
                  { Dirty hack, to support multiple firstpasses (PFV) }
                  if (p^.resulttype=nil) and
                     (rd^.deftype=pointerdef) and
                     (ppointerdef(rd)^.pointertype.def^.size>1) then
                   begin
                     p^.left:=gennode(muln,p^.left,genordinalconstnode(ppointerdef(rd)^.pointertype.def^.size,s32bitdef));
                     firstpass(p^.left);
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
                   p^.resulttype:=new(ppointerdef,init(parraydef(ld)^.elementtype));
                   p^.left:=gentypeconvnode(p^.left,p^.resulttype);
                   firstpass(p^.left);
                end;
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,s32bitdef);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                addn,subn : begin
                              if not(cs_extsyntax in aktmoduleswitches) or
                                 (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                               CGMessage(type_e_mismatch);
                              { Dirty hack, to support multiple firstpasses (PFV) }
                              if (p^.resulttype=nil) and
                                 (ld^.deftype=pointerdef) and
                                 (ppointerdef(ld)^.pointertype.def^.size>1) then
                               begin
                                 p^.right:=gennode(muln,p^.right,
                                   genordinalconstnode(ppointerdef(ld)^.pointertype.def^.size,s32bitdef));
                                 firstpass(p^.right);
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
              p^.location.loc:=LOC_REGISTER;
              case p^.treetype of
                 equaln,unequaln : ;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=enumdef) and (rd^.deftype=enumdef) and (is_equal(ld,rd)) then
            begin
              calcregisters(p,1,0,0);
              case p^.treetype of
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
              if p^.treetype=slashn then
                begin
                   CGMessage(type_h_use_div_for_int);
                   p^.right:=gentypeconvnode(p^.right,bestrealdef^);
                   p^.left:=gentypeconvnode(p^.left,bestrealdef^);
                   firstpass(p^.left);
                   firstpass(p^.right);
                   { maybe we need an integer register to save }
                   { a reference                               }
                   if ((p^.left^.location.loc<>LOC_FPU) or
                       (p^.right^.location.loc<>LOC_FPU)) and
                       (p^.left^.registers32=p^.right^.registers32) then
                     calcregisters(p,1,1,0)
                   else
                     calcregisters(p,0,1,0);
                   p^.location.loc:=LOC_FPU;
                end
              else
                begin
                   p^.right:=gentypeconvnode(p^.right,s32bitdef);
                   p^.left:=gentypeconvnode(p^.left,s32bitdef);
                   firstpass(p^.left);
                   firstpass(p^.right);
                   calcregisters(p,1,0,0);
                   p^.location.loc:=LOC_REGISTER;
                end;
           end;

         if codegenerror then
           exit;

         { determines result type for comparions }
         { here the is a problem with multiple passes }
         { example length(s)+1 gets internal 'longint' type first }
         { if it is a arg it is converted to 'LONGINT' }
         { but a second first pass will reset this to 'longint' }
         case p^.treetype of
            ltn,lten,gtn,gten,equaln,unequaln:
              begin
                 if (not assigned(p^.resulttype)) or
                   (p^.resulttype^.deftype=stringdef) then
                   p^.resulttype:=booldef;
                 if is_64bitint(p^.left^.resulttype) then
                   p^.location.loc:=LOC_JUMP
                 else
                   p^.location.loc:=LOC_FLAGS;
              end;
            xorn:
              begin
                if not assigned(p^.resulttype) then
                  p^.resulttype:=p^.left^.resulttype;
                 p^.location.loc:=LOC_REGISTER;
              end;
            addn:
              begin
                if not assigned(p^.resulttype) then
                 begin
                 { for strings, return is always a 255 char string }
                   if is_shortstring(p^.left^.resulttype) then
                     p^.resulttype:=cshortstringdef
                   else
                    p^.resulttype:=p^.left^.resulttype;
                 end;
              end;
{$ifdef cardinalmulfix}
            muln:
  { if we multiply an unsigned with a signed number, the result is signed  }
  { in the other cases, the result remains signed or unsigned depending on }
  { the multiplication factors (JM)                                        }
              if (p^.left^.resulttype^.deftype = orddef) and
                 (p^.right^.resulttype^.deftype = orddef) and
                 is_signed(p^.right^.resulttype) then
                p^.resulttype := p^.right^.resulttype
              else p^.resulttype := p^.left^.resulttype;
(*
            subn:
 { if we substract a u32bit from a positive constant, the result becomes }
 { s32bit as well (JM)                                                   }
              begin
                if (p^.right^.resulttype^.deftype = orddef) and
                   (p^.left^.resulttype^.deftype = orddef) and
                   (porddef(p^.right^.resulttype)^.typ = u32bit) and
                   is_constintnode(p^.left) and
{                   (porddef(p^.left^.resulttype)^.typ <> u32bit) and}
                   (p^.left^.value > 0) then
                  begin
                    p^.left := gentypeconvnode(p^.left,u32bitdef);
                    firstpass(p^.left);
                  end;
                p^.resulttype:=p^.left^.resulttype;
              end;
*)
{$endif cardinalmulfix}
            else
              p^.resulttype:=p^.left^.resulttype;
         end;
      end;


end.
{
  $Log$
  Revision 1.77  2000-05-11 17:53:40  peter
    * small fix for previous commit

  Revision 1.76  2000/05/11 16:47:37  peter
    * fixed check for overloaded operator with array and chararray check

  Revision 1.75  2000/04/25 14:43:36  jonas
    - disabled "string_var := string_var + ... " and "string_var + char_var"
      optimizations (were only active with -dnewoptimizations) because of
      several internal issues

  Revision 1.74  2000/04/21 12:35:05  jonas
    + special code for string + char, between -dnewoptimizations

  Revision 1.73  2000/03/28 21:14:18  pierre
   * fix for bug 891

  Revision 1.72  2000/03/20 10:16:51  florian
   * fixed <dword>/<dword>, <int64>/<int64> and <qword>/<qword>

  Revision 1.71  2000/03/18 15:01:19  jonas
    * moved a $maxfpuregisters directive a bit up because it was being
      ignored

  Revision 1.70  2000/02/19 10:12:48  florian
    * fixed one more internalerror 10

  Revision 1.69  2000/02/17 14:53:42  florian
    * some updates for the newcg

  Revision 1.68  2000/02/14 22:34:28  florian
    * fixed another internalerror

  Revision 1.67  2000/02/13 22:46:28  florian
    * fixed an internalerror with writeln
    * fixed arrayconstructor_to_set to force the generation of better code
      and added a more strict type checking

  Revision 1.66  2000/02/13 14:21:51  jonas
    * modifications to make the compiler functional when compiled with
      -Or

  Revision 1.65  2000/02/09 13:23:06  peter
    * log truncated

  Revision 1.64  2000/02/04 08:47:10  florian
    * better register variable allocation in -Or mode

  Revision 1.63  2000/01/07 01:14:43  peter
    * updated copyright to 2000

  Revision 1.62  2000/01/04 20:10:20  florian
    * mmx support fixed

  Revision 1.61  1999/12/11 18:53:31  jonas
    * fixed type conversions of results of operations with cardinals
      (between -dcardinalmulfix)

  Revision 1.60  1999/12/09 23:18:04  pierre
   * no_fast_exit if procedure contains implicit termination code

  Revision 1.59  1999/12/01 12:42:33  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.58  1999/11/30 10:40:56  peter
    + ttype, tsymlist

  Revision 1.57  1999/11/26 13:51:29  pierre
   * fix for overloading of shr shl mod and div

  Revision 1.56  1999/11/18 15:34:48  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.55  1999/11/17 17:05:06  pierre
   * Notes/hints changes

  Revision 1.54  1999/11/16 23:45:28  pierre
   * global var token was changed by overload code (form bug 707)

  Revision 1.53  1999/11/15 21:53:42  peter
    * fixed constant eval for bool xor/or/and bool

  Revision 1.52  1999/11/15 17:53:00  pierre
    + one field added for ttoken record for operator
      linking the id to the corresponding operator token that
      can now now all be overloaded
    * overloaded operators are resetted to nil in InitSymtable
      (bug when trying to compile a uint that overloads operators twice)

  Revision 1.51  1999/11/06 14:34:29  peter
    * truncated log to 20 revs

  Revision 1.50  1999/09/27 23:45:00  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.49  1999/09/16 13:39:14  peter
    * arrayconstructor 2 set conversion is now called always in the
      beginning of firstadd

  Revision 1.48  1999/09/15 20:35:45  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.47  1999/09/13 16:28:05  peter
    * typo in previous commit open_array -> chararray :(

  Revision 1.46  1999/09/10 15:40:46  peter
    * fixed array check for operators, becuase array can also be a set

  Revision 1.45  1999/09/08 16:05:29  peter
    * pointer add/sub is now as expected and the same results as inc/dec

}
{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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


implementation

    uses
      cobjects,verbose,globals,systems,
      symtable,aasm,types,
      hcodegen,htypechk,pass_1
{$ifdef i386}
      ,i386
{$endif}
{$ifdef m68k}
      ,m68k
{$endif}
      ;

{*****************************************************************************
                                FirstAdd
*****************************************************************************}

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
         t       : ptree;
         lt,rt   : ttreetyp;
         rv,lv   : longint;
         rvd,lvd : bestreal;
         rd,ld   : pdef;
         tempdef : pdef;
         concatstrings : boolean;

         { to evalute const sets }
         resultset : pconstset;
         i : longint;
         b : boolean;
         convdone : boolean;
{$ifndef UseAnsiString}
         s1,s2:^string;
{$else UseAnsiString}
         s1,s2 : pchar;
         l1,l2 : longint;
{$endif UseAnsiString}

         { this totally forgets to set the pi_do_call flag !! }
      label
         no_overload;

      begin
         { first do the two subtrees }
         firstpass(p^.left);
         firstpass(p^.right);
         lt:=p^.left^.treetype;
         rt:=p^.right^.treetype;
         rd:=p^.right^.resulttype;
         ld:=p^.left^.resulttype;
         convdone:=false;

         if codegenerror then
           exit;

         { overloaded operator ? }
         if (p^.treetype=starstarn) or
            (ld^.deftype=recorddef) or
            { <> and = are defined for classes }
            ((ld^.deftype=objectdef) and
             (not(pobjectdef(ld)^.isclass) or
              not(p^.treetype in [equaln,unequaln])
             )
            ) or
            (rd^.deftype=recorddef) or
            { <> and = are defined for classes }
            ((rd^.deftype=objectdef) and
             (not(pobjectdef(rd)^.isclass) or
              not(p^.treetype in [equaln,unequaln])
             )
            ) then
           begin
              {!!!!!!!!! handle paras }
              case p^.treetype of
                 { the nil as symtable signs firstcalln that this is
                   an overloaded operator }
                 addn:
                   t:=gencallnode(overloaded_operators[plus],nil);
                 subn:
                   t:=gencallnode(overloaded_operators[minus],nil);
                 muln:
                   t:=gencallnode(overloaded_operators[star],nil);
                 starstarn:
                   t:=gencallnode(overloaded_operators[starstar],nil);
                 slashn:
                   t:=gencallnode(overloaded_operators[slash],nil);
                 ltn:
                   t:=gencallnode(overloaded_operators[globals.lt],nil);
                 gtn:
                   t:=gencallnode(overloaded_operators[gt],nil);
                 lten:
                   t:=gencallnode(overloaded_operators[lte],nil);
                 gten:
                   t:=gencallnode(overloaded_operators[gte],nil);
                 equaln,unequaln :
                   t:=gencallnode(overloaded_operators[equal],nil);
                 else goto no_overload;
              end;
              { we have to convert p^.left and p^.right into
               callparanodes }
              t^.left:=gencallparanode(p^.left,nil);
              t^.left:=gencallparanode(p^.right,t^.left);
              if t^.symtableprocentry=nil then
               CGMessage(parser_e_operator_not_overloaded);
              if p^.treetype=unequaln then
               t:=gensinglenode(notn,t);
              firstpass(t);
              putnode(p);
              p:=t;
              exit;
           end;
         no_overload:
         { compact consts }

         { convert int consts to real consts, if the }
         { other operand is a real const             }
         if (rt=realconstn) and is_constintnode(p^.left) then
           begin
              t:=genrealconstnode(p^.left^.value);
              disposetree(p^.left);
              p^.left:=t;
              lt:=realconstn;
           end;
         if (lt=realconstn) and is_constintnode(p^.right) then
           begin
              t:=genrealconstnode(p^.right^.value);
              disposetree(p^.right);
              p^.right:=t;
              rt:=realconstn;
           end;

       { both are int constants ? }
         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              lv:=p^.left^.value;
              rv:=p^.right^.value;
              case p^.treetype of
                addn : t:=genordinalconstnode(lv+rv,s32bitdef);
                subn : t:=genordinalconstnode(lv-rv,s32bitdef);
                muln : t:=genordinalconstnode(lv*rv,s32bitdef);
                xorn : t:=genordinalconstnode(lv xor rv,s32bitdef);
                 orn : t:=genordinalconstnode(lv or rv,s32bitdef);
                andn : t:=genordinalconstnode(lv and rv,s32bitdef);
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
                            t:=genrealconstnode(0);
                          end
                         else
                          t:=genrealconstnode(int(lv)/int(rv));
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
                 addn : t:=genrealconstnode(lvd+rvd);
                 subn : t:=genrealconstnode(lvd-rvd);
                 muln : t:=genrealconstnode(lvd*rvd);
               caretn : t:=genrealconstnode(exp(ln(lvd)*rvd));
               slashn : begin
                          if rvd=0 then
                           begin
                             Message(parser_e_invalid_float_operation);
                             t:=genrealconstnode(0);
                           end
                          else
                           t:=genrealconstnode(lvd/rvd);
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
{$ifdef UseAnsiString}
         s1:=nil;
         s2:=nil;
{$else UseAnsiString}
         new(s1);
         new(s2);
{$endif UseAnsiString}
         if (lt=ordconstn) and (rt=ordconstn) and
            is_char(ld) and is_char(rd) then
           begin
{$ifdef UseAnsiString}
              s1:=strpnew(char(byte(p^.left^.value)));
              s2:=strpnew(char(byte(p^.right^.value)));
              l1:=1;l2:=1;
{$else UseAnsiString}
              s1^:=char(byte(p^.left^.value));
              s2^:=char(byte(p^.right^.value));
{$endif UseAnsiString}
              concatstrings:=true;
           end
         else
           if (lt=stringconstn) and (rt=ordconstn) and is_char(rd) then
           begin
{$ifdef UseAnsiString}
              { here there is allways the damn #0 problem !! }
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=strpnew(char(byte(p^.right^.value)));
              l2:=1;
{$else UseAnsiString}
              s1^:=p^.left^.value_str^;
              s2^:=char(byte(p^.right^.value));
{$endif UseAnsiString}
              concatstrings:=true;
           end
         else if (lt=ordconstn) and (rt=stringconstn) and
           (ld^.deftype=orddef) and
           (porddef(ld)^.typ=uchar) then
           begin
{$ifdef UseAnsiString}
              { here there is allways the damn #0 problem !! }
              s1:=strpnew(char(byte(p^.left^.value)));
              l1:=1;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
{$else UseAnsiString}
              s1^:=char(byte(p^.left^.value));
              s2^:=p^.right^.value_str^;
{$endif UseAnsiString}
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
{$ifdef UseAnsiString}
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
{$else UseAnsiString}
              s1^:=p^.left^.value_str^;
              s2^:=p^.right^.value_str^;
{$endif UseAnsiString}
              concatstrings:=true;
           end;

         { I will need to translate all this to ansistrings !!! }
         if concatstrings then
           begin
              case p^.treetype of
{$ifndef UseAnsiString}
                 addn : t:=genstringconstnode(s1^+s2^);
                 ltn : t:=genordinalconstnode(byte(s1^<s2^),booldef);
                 lten : t:=genordinalconstnode(byte(s1^<=s2^),booldef);
                 gtn : t:=genordinalconstnode(byte(s1^>s2^),booldef);
                 gten : t:=genordinalconstnode(byte(s1^>=s2^),booldef);
                 equaln : t:=genordinalconstnode(byte(s1^=s2^),booldef);
                 unequaln : t:=genordinalconstnode(byte(s1^<>s2^),booldef);
{$else UseAnsiString}
                 addn : t:=genpcharconstnode(
                             concatansistrings(s1,s2,l1,l2),l1+l2);
                 ltn : t:=genordinalconstnode(
                           byte(compareansistrings(s1,s2,l1,l2)<0),booldef);
                 lten : t:=genordinalconstnode(
                            byte(compareansistrings(s1,s2,l1,l2)<=0),booldef);
                 gtn : t:=genordinalconstnode(
                            byte(compareansistrings(s1,s2,l1,l2)>0),booldef);
                 gten : t:=genordinalconstnode(
                             byte(compareansistrings(s1,s2,l1,l2)>=0),booldef);
                 equaln : t:=genordinalconstnode(
                               byte(compareansistrings(s1,s2,l1,l2)=0),booldef);
                 unequaln : t:=genordinalconstnode(
                                 byte(compareansistrings(s1,s2,l1,l2)<>0),booldef);
{$endif UseAnsiString}
              end;
{$ifdef UseAnsiString}
              ansistringdispose(s1,l1);
              ansistringdispose(s2,l2);
{$else UseAnsiString}
              dispose(s1);
              dispose(s2);
{$endif UseAnsiString}
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
{$ifdef UseAnsiString}
         ansistringdispose(s1,l1);
         ansistringdispose(s2,l2);
{$else UseAnsiString}
         dispose(s1);
         dispose(s2);
{$endif UseAnsiString}

       { if both are orddefs then check sub types }
         if (ld^.deftype=orddef) and (rd^.deftype=orddef) then
           begin
           { 2 booleans ? }
             if is_boolean(ld) and is_boolean(rd) then
              begin
                case p^.treetype of
             andn,orn : begin
                          calcregisters(p,0,0,0);
                          make_bool_equal_size(p);
                          p^.location.loc:=LOC_JUMP;
                        end;
             unequaln,
          equaln,xorn : begin
                          { this forces a better code generation (TEST }
                          { instead of CMP)                            }
                          if p^.treetype<>xorn then
                            begin
                               if (p^.left^.treetype=ordconstn) and
                                 (p^.left^.value<>0) then
                                 begin
                                    p^.left^.value:=0;
                                    if p^.treetype=equaln then
                                      p^.treetype:=unequaln
                                    else
                                      p^.treetype:=equaln;
                                 end;
                               if (p^.right^.treetype=ordconstn) and
                                 (p^.right^.value<>0) then
                                 begin
                                    p^.right^.value:=0;
                                    if p^.treetype=equaln then
                                      p^.treetype:=unequaln
                                    else
                                      p^.treetype:=equaln;
                                 end;
                            end;
                          make_bool_equal_size(p);
                          calcregisters(p,1,0,0);
                        end
                else
                  CGMessage(type_e_mismatch);
                end;
                convdone:=true;
              end
             else
             { Both are chars? only convert to strings for addn }
              if (porddef(rd)^.typ=uchar) and (porddef(ld)^.typ=uchar) then
               begin
                 if p^.treetype=addn then
                   begin
                      p^.left:=gentypeconvnode(p^.left,cstringdef);
                      firstpass(p^.left);
                      p^.right:=gentypeconvnode(p^.right,cstringdef);
                      firstpass(p^.right);
                      { here we call STRCOPY }
                      procinfo.flags:=procinfo.flags or pi_do_call;
                      calcregisters(p,0,0,0);
                      p^.location.loc:=LOC_MEM;
                   end
                 else
                  calcregisters(p,1,0,0);
                 convdone:=true;
               end;
           end
         else

         { is one of the sides a shortstring ? }
           if (rd^.deftype=stringdef) or (ld^.deftype=stringdef) then
            begin
              {
              if is_widestring(rd) or is_widestring(ld) then
                begin
                end
              else if is_ansistring(rd) or is_ansistring(ld) then
                begin
                end
              else if is_longstring(rd) or is_longstring(ld) then
                begin
                end
              }
              if not((rd^.deftype=stringdef) and (ld^.deftype=stringdef)) then
               begin
                 if ld^.deftype=stringdef then
                  p^.right:=gentypeconvnode(p^.right,cstringdef)
                 else
                  p^.left:=gentypeconvnode(p^.left,cstringdef);
                 firstpass(p^.left);
                 firstpass(p^.right);
               end;
            { here we call STRCONCAT or STRCMP or STRCOPY }
              procinfo.flags:=procinfo.flags or pi_do_call;
              calcregisters(p,0,0,0);
              p^.location.loc:=LOC_MEM;
              convdone:=true;
           end
         else

         { left side a setdef ? }
           if (ld^.deftype=setdef) then
             begin
             { trying to add a set element? }
                if (p^.treetype=addn) and (rd^.deftype<>setdef) then
                 begin
                   if (rt=setelementn) then
                    begin
                      if not(is_equal(psetdef(ld)^.setof,rd)) then
                       CGMessage(type_e_set_element_are_not_comp);
                    end
                   else
                    CGMessage(type_e_mismatch)
                 end
                else
                 begin
                   if not(p^.treetype in [addn,subn,symdifn,muln,equaln,unequaln]) then
                    CGMessage(type_e_mismatch);
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
                   tempdef:=new(psetdef,init(psetdef(ld)^.setof,255));
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
                   p^.right:=gentypeconvnode(p^.right,psetdef(p^.left^.resulttype));
                   firstpass(p^.right);
                 end;

                { do constant evalution }
                if (p^.right^.treetype=setconstn) and
                   (p^.left^.treetype=setconstn) then
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
                     calcregisters(p,1,0,0);
                     p^.location.loc:=LOC_REGISTER;
                  end
                 else
                  begin
                     calcregisters(p,0,0,0);
                     { here we call SET... }
                     procinfo.flags:=procinfo.flags or pi_do_call;
                     p^.location.loc:=LOC_MEM;
                  end;
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
                 if not(porddef(rd)^.typ in [u8bit,s8bit,u16bit,s16bit,s32bit,u32bit]) or (p^.treetype<>muln) then
                   p^.right:=gentypeconvnode(p^.right,s32fixeddef);
                 if not(porddef(rd)^.typ in [u8bit,s8bit,u16bit,s16bit,s32bit,u32bit]) or (p^.treetype<>muln) then
                   p^.left:=gentypeconvnode(p^.left,s32fixeddef);
                 firstpass(p^.left);
                 firstpass(p^.right);
                 calcregisters(p,1,0,0);
                 p^.location.loc:=LOC_REGISTER;
               end
              else
              { convert both to c64float }
                begin
                  p^.right:=gentypeconvnode(p^.right,c64floatdef);
                  p^.left:=gentypeconvnode(p^.left,c64floatdef);
                  firstpass(p^.left);
                  firstpass(p^.right);
                  calcregisters(p,1,1,0);
                  p^.location.loc:=LOC_FPU;
                end;
              convdone:=true;
            end
         else

         { pointer comperation and subtraction }
           if (rd^.deftype=pointerdef) and (ld^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 ltn,lten,gtn,gten:
                   begin
                      if not(cs_extsyntax in aktmoduleswitches) then
                        CGMessage(type_e_mismatch);
                   end;
                 subn:
                   begin
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
              pobjectdef(rd)^.isclass and pobjectdef(ld)^.isclass then
            begin
              p^.location.loc:=LOC_REGISTER;
              if pobjectdef(rd)^.isrelated(pobjectdef(ld)) then
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
              if pobjectdef(pclassrefdef(rd)^.definition)^.isrelated(pobjectdef(
                pclassrefdef(ld)^.definition)) then
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
              pobjectdef(rd)^.isclass then
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
              pobjectdef(ld)^.isclass then
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

           if (rd^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              if p^.treetype=addn then
                begin
                  if not(cs_extsyntax in aktmoduleswitches) then
                    CGMessage(type_e_mismatch);
                end
              else
                CGMessage(type_e_mismatch);
              convdone:=true;
            end
         else

           if (ld^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,s32bitdef);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                addn,subn : if not(cs_extsyntax in aktmoduleswitches) then
                              CGMessage(type_e_mismatch);
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
                   CGMessage(type_w_int_slash_int);
                   CGMessage(type_h_use_div_for_int);
                   p^.right:=gentypeconvnode(p^.right,c64floatdef);
                   p^.left:=gentypeconvnode(p^.left,c64floatdef);
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
                 if not assigned(p^.resulttype) then
                   p^.resulttype:=booldef;
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
                 { the result of a string addition is a string of length 255 }
                 if (p^.left^.resulttype^.deftype=stringdef) or
                    (p^.right^.resulttype^.deftype=stringdef) then
                   begin
{$ifndef UseAnsiString}
                      if not assigned(p^.resulttype) then
                        p^.resulttype:=cstringdef
{$else UseAnsiString}
                      if is_ansistring(p^.left^.resulttype) or
                         is_ansistring(p^.right^.resulttype) then
                        p^.resulttype:=cansistringdef
                      else
                        p^.resulttype:=cstringdef;
{$endif UseAnsiString}
                   end
                 else
                   if not assigned(p^.resulttype) then
                     p^.resulttype:=p^.left^.resulttype;
              end;
            else if not assigned(p^.resulttype) then
              p^.resulttype:=p^.left^.resulttype;
         end;
      end;


end.
{
  $Log$
  Revision 1.4  1998-10-14 12:53:39  peter
    * fixed small tp7 things
    * boolean:=longbool and longbool fixed

  Revision 1.3  1998/10/11 14:31:19  peter
    + checks for division by zero

  Revision 1.2  1998/10/05 21:33:31  peter
    * fixed 161,165,166,167,168

  Revision 1.1  1998/09/23 20:42:24  peter
    * splitted pass_1

}

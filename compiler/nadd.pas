{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
      node;

    type
       taddnode = class(tbinopnode)
          constructor create(tt : tnodetype;l,r : tnode);override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
    {$ifdef state_tracking}
      function track_state_pass(exec_known:boolean):boolean;override;
    {$endif}
         protected
          { override the following if you want to implement }
          { parts explicitely in the code generator (JM)    }
          function first_addstring: tnode; virtual;
          function first_addset: tnode; virtual;
          { only implements "muln" nodes, the rest always has to be done in }
          { the code generator for performance reasons (JM)                 }
          function first_add64bitint: tnode; virtual;
          { This routine calls internal runtime library helpers
            for all floating point arithmetic in the case
            where the emulation switches is on. Otherwise
            returns nil, and everything must be done in
            the code generation phase.
          }
          function first_addfloat : tnode; virtual;
       end;
       taddnodeclass = class of taddnode;

    var
       { caddnode is used to create nodes of the add type }
       { the virtual constructor allows to assign         }
       { another class type to caddnode => processor      }
       { specific node types can be created               }
       caddnode : taddnodeclass;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,widestr,
      symconst,symtype,symdef,symsym,symtable,defbase,
      cgbase,
      htypechk,pass_1,
      nmat,ncnv,ncon,nset,nopt,ncal,ninl,
      {$ifdef state_tracking}
      nstate,
      {$endif}
      cpubase,cpuinfo;


{*****************************************************************************
                                TADDNODE
*****************************************************************************}

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}

    constructor taddnode.create(tt : tnodetype;l,r : tnode);
      begin
         inherited create(tt,l,r);
      end;


    function taddnode.det_resulttype:tnode;
      var
         hp,t    : tnode;
         lt,rt   : tnodetype;
         rd,ld   : tdef;
         htype   : ttype;
         ot      : tnodetype;
         concatstrings : boolean;
         resultset : Tconstset;
         i       : longint;
         b       : boolean;
         s1,s2   : pchar;
         ws1,ws2 : pcompilerwidestring;
         l1,l2   : longint;
         rv,lv   : tconstexprint;
         rvd,lvd : bestreal;
{$ifdef state_tracking}
     factval : Tnode;
     change  : boolean;
{$endif}

      begin
         result:=nil;

         { first do the two subtrees }
         resulttypepass(left);
         resulttypepass(right);
         { both left and right need to be valid }
         set_varstate(left,true);
         set_varstate(right,true);
         if codegenerror then
           exit;

         { convert array constructors to sets, because there is no other operator
           possible for array constructors }
         if is_array_constructor(left.resulttype.def) then
          begin
            arrayconstructor_to_set(left);
            resulttypepass(left);
          end;
         if is_array_constructor(right.resulttype.def) then
          begin
            arrayconstructor_to_set(right);
            resulttypepass(right);
          end;

         { allow operator overloading }
         hp:=self;
         if isbinaryoverloaded(hp) then
           begin
              result:=hp;
              exit;
           end;

         { Kylix allows enum+ordconstn in an enum declaration (blocktype
           is bt_type), we need to do the conversion here before the
           constant folding }
         if (m_delphi in aktmodeswitches) and
            (blocktype=bt_type) then
          begin
            if (left.resulttype.def.deftype=enumdef) and
               (right.resulttype.def.deftype=orddef) then
             begin
               { insert explicit typecast to s32bit }
               left:=ctypeconvnode.create(left,s32bittype);
               left.toggleflag(nf_explizit);
               resulttypepass(left);
             end
            else
             if (left.resulttype.def.deftype=orddef) and
                (right.resulttype.def.deftype=enumdef) then
              begin
                { insert explicit typecast to s32bit }
                right:=ctypeconvnode.create(right,s32bittype);
                include(right.flags,nf_explizit);
                resulttypepass(right);
              end;
          end;

         { is one a real float, then both need to be floats, this
           need to be done before the constant folding so constant
           operation on a float and int are also handled }
         if (right.resulttype.def.deftype=floatdef) or (left.resulttype.def.deftype=floatdef) then
          begin
            inserttypeconv(right,pbestrealtype^);
            inserttypeconv(left,pbestrealtype^);
          end;

         { if one operand is a widechar or a widestring, both operands    }
         { are converted to widestring. This must be done before constant }
         { folding to allow char+widechar etc.                            }
         if is_widestring(right.resulttype.def) or
            is_widestring(left.resulttype.def) or
            is_widechar(right.resulttype.def) or
            is_widechar(left.resulttype.def) then
           begin
              inserttypeconv(right,cwidestringtype);
              inserttypeconv(left,cwidestringtype);
           end;

         { load easier access variables }
         rd:=right.resulttype.def;
         ld:=left.resulttype.def;
         rt:=right.nodetype;
         lt:=left.nodetype;

       if (nodetype = slashn) and
          (((rt = ordconstn) and
            (tordconstnode(right).value = 0)) or
           ((rt = realconstn) and
            (trealconstnode(right).value_real = 0.0))) then
         begin
           Message(parser_e_division_by_zero);
           case rt of
             ordconstn:
                tordconstnode(right).value := 1;  
             realconstn:   
                trealconstnode(right).value_real := 1.0;
           end;
         end;


         { both are int constants }
         if (((is_constintnode(left) and is_constintnode(right)) or
              (is_constboolnode(left) and is_constboolnode(right) and
               (nodetype in [slashn,ltn,lten,gtn,gten,equaln,unequaln,andn,xorn,orn])))) or
            { support pointer arithmetics on constants (JM) }
            ((lt = pointerconstn) and is_constintnode(right) and
             (nodetype in [addn,subn])) or
            (((lt = pointerconstn) or (lt = niln)) and
             ((rt = pointerconstn) or (rt = niln)) and
             (nodetype in [ltn,lten,gtn,gten,equaln,unequaln,subn])) then
           begin
              { when comparing/substracting  pointers, make sure they are }
              { of the same  type (JM)                                    }
              if (lt = pointerconstn) and (rt = pointerconstn) then
               begin
                 if not(cs_extsyntax in aktmoduleswitches) and
                    not(nodetype in [equaln,unequaln]) then
                   CGMessage(type_e_mismatch)
                 else
                   if (nodetype <> subn) and
                      is_voidpointer(rd) then
                     inserttypeconv(right,left.resulttype)
                   else if (nodetype <> subn) and
                           is_voidpointer(ld) then
                     inserttypeconv(left,right.resulttype)
                   else if not(is_equal(ld,rd)) then
                     CGMessage(type_e_mismatch);
                end
              else if (lt=ordconstn) and (rt=ordconstn) then
                begin
                  { make left const type the biggest (u32bit is bigger than
                    s32bit for or,and,xor) }
                  if (rd.size>ld.size) or
                     ((torddef(rd).typ=u32bit) and
                      (torddef(ld).typ=s32bit) and
                      (nodetype in [orn,andn,xorn])) then
                    inserttypeconv(left,right.resulttype);
                end;

              { load values }
              case lt of
                ordconstn:
                  lv:=tordconstnode(left).value;
                pointerconstn:
                  lv:=tpointerconstnode(left).value;
                niln:
                  lv:=0;
                else
                  internalerror(2002080202);
              end;
              case rt of
                ordconstn:
                  rv:=tordconstnode(right).value;
                pointerconstn:
                  rv:=tpointerconstnode(right).value;
                niln:
                  rv:=0;
                else
                  internalerror(2002080203);
              end;
              if (lt = pointerconstn) and
                 (rt <> pointerconstn) then
                rv := rv * tpointerdef(left.resulttype.def).pointertype.def.size;
              if (rt = pointerconstn) and
                 (lt <> pointerconstn) then
                lv := lv * tpointerdef(right.resulttype.def).pointertype.def.size;
              case nodetype of
                addn :
                  if (lt <> pointerconstn) then
                    t := genintconstnode(lv+rv)
                  else
                    t := cpointerconstnode.create(lv+rv,left.resulttype);
                subn :
                  if (lt <> pointerconstn) or (rt = pointerconstn) then
                    t := genintconstnode(lv-rv)
                  else
                    t := cpointerconstnode.create(lv-rv,left.resulttype);
                muln :
                  t:=genintconstnode(lv*rv);
                xorn :
                  t:=cordconstnode.create(lv xor rv,left.resulttype,true);
                orn :
                  t:=cordconstnode.create(lv or rv,left.resulttype,true);
                andn :
                  t:=cordconstnode.create(lv and rv,left.resulttype,true);
                ltn :
                  t:=cordconstnode.create(ord(lv<rv),booltype,true);
                lten :
                  t:=cordconstnode.create(ord(lv<=rv),booltype,true);
                gtn :
                  t:=cordconstnode.create(ord(lv>rv),booltype,true);
                gten :
                  t:=cordconstnode.create(ord(lv>=rv),booltype,true);
                equaln :
                  t:=cordconstnode.create(ord(lv=rv),booltype,true);
                unequaln :
                  t:=cordconstnode.create(ord(lv<>rv),booltype,true);
                slashn :
                  begin
                    { int/int becomes a real }
                    rvd:=rv;
                    lvd:=lv;
                    t:=crealconstnode.create(lvd/rvd,pbestrealtype^);
                  end;
                else
                  CGMessage(type_e_mismatch);
              end;
              result:=t;
              exit;
           end;

       { both real constants ? }
         if (lt=realconstn) and (rt=realconstn) then
           begin
              lvd:=trealconstnode(left).value_real;
              rvd:=trealconstnode(right).value_real;
              case nodetype of
                 addn :
                   t:=crealconstnode.create(lvd+rvd,pbestrealtype^);
                 subn :
                   t:=crealconstnode.create(lvd-rvd,pbestrealtype^);
                 muln :
                   t:=crealconstnode.create(lvd*rvd,pbestrealtype^);
                 starstarn,
                 caretn :
                   begin
                     if lvd<0 then
                      begin
                        Message(parser_e_invalid_float_operation);
                        t:=crealconstnode.create(0,pbestrealtype^);
                      end
                     else if lvd=0 then
                       t:=crealconstnode.create(1.0,pbestrealtype^)
                     else
                       t:=crealconstnode.create(exp(ln(lvd)*rvd),pbestrealtype^);
                   end;
                 slashn :
                   begin
                     t:=crealconstnode.create(lvd/rvd,pbestrealtype^);
                   end;
                 ltn :
                   t:=cordconstnode.create(ord(lvd<rvd),booltype,true);
                 lten :
                   t:=cordconstnode.create(ord(lvd<=rvd),booltype,true);
                 gtn :
                   t:=cordconstnode.create(ord(lvd>rvd),booltype,true);
                 gten :
                   t:=cordconstnode.create(ord(lvd>=rvd),booltype,true);
                 equaln :
                   t:=cordconstnode.create(ord(lvd=rvd),booltype,true);
                 unequaln :
                   t:=cordconstnode.create(ord(lvd<>rvd),booltype,true);
                 else
                   CGMessage(type_e_mismatch);
              end;
              result:=t;
              exit;
           end;

         { first, we handle widestrings, so we can check later for }
         { stringconstn only                                       }

         { widechars are converted above to widestrings too }
         { this isn't veryy efficient, but I don't think    }
         { that it does matter that much (FK)               }
         if (lt=stringconstn) and (rt=stringconstn) and
           (tstringconstnode(left).st_type=st_widestring) and
           (tstringconstnode(right).st_type=st_widestring) then
           begin
              initwidestring(ws1);
              initwidestring(ws2);
              copywidestring(pcompilerwidestring(tstringconstnode(left).value_str),ws1);
              copywidestring(pcompilerwidestring(tstringconstnode(right).value_str),ws2);
              case nodetype of
                 addn :
                   begin
                      concatwidestrings(ws1,ws2);
                      t:=cstringconstnode.createwstr(ws1);
                   end;
                 ltn :
                   t:=cordconstnode.create(byte(comparewidestrings(ws1,ws2)<0),booltype,true);
                 lten :
                   t:=cordconstnode.create(byte(comparewidestrings(ws1,ws2)<=0),booltype,true);
                 gtn :
                   t:=cordconstnode.create(byte(comparewidestrings(ws1,ws2)>0),booltype,true);
                 gten :
                   t:=cordconstnode.create(byte(comparewidestrings(ws1,ws2)>=0),booltype,true);
                 equaln :
                   t:=cordconstnode.create(byte(comparewidestrings(ws1,ws2)=0),booltype,true);
                 unequaln :
                   t:=cordconstnode.create(byte(comparewidestrings(ws1,ws2)<>0),booltype,true);
              end;
              donewidestring(ws1);
              donewidestring(ws2);
              result:=t;
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
         if concatstrings then
           begin
              case nodetype of
                 addn :
                   t:=cstringconstnode.createpchar(concatansistrings(s1,s2,l1,l2),l1+l2);
                 ltn :
                   t:=cordconstnode.create(byte(compareansistrings(s1,s2,l1,l2)<0),booltype,true);
                 lten :
                   t:=cordconstnode.create(byte(compareansistrings(s1,s2,l1,l2)<=0),booltype,true);
                 gtn :
                   t:=cordconstnode.create(byte(compareansistrings(s1,s2,l1,l2)>0),booltype,true);
                 gten :
                   t:=cordconstnode.create(byte(compareansistrings(s1,s2,l1,l2)>=0),booltype,true);
                 equaln :
                   t:=cordconstnode.create(byte(compareansistrings(s1,s2,l1,l2)=0),booltype,true);
                 unequaln :
                   t:=cordconstnode.create(byte(compareansistrings(s1,s2,l1,l2)<>0),booltype,true);
              end;
              ansistringdispose(s1,l1);
              ansistringdispose(s2,l2);
              result:=t;
              exit;
           end;

         { set constant evaluation }
         if (right.nodetype=setconstn) and
            not assigned(tsetconstnode(right).left) and
            (left.nodetype=setconstn) and
            not assigned(tsetconstnode(left).left) then
           begin
              { check if size adjusting is needed, only for left
                to right as the other way is checked in the typeconv }
              if (tsetdef(right.resulttype.def).settype=smallset) and
                 (tsetdef(left.resulttype.def).settype<>smallset) then
                tsetdef(right.resulttype.def).changesettype(normset);
              { check base types }
              inserttypeconv(left,right.resulttype);

              if codegenerror then
               begin
                 { recover by only returning the left part }
                 result:=left;
                 left:=nil;
                 exit;
               end;
{$ifdef oldset}
          case nodetype of
        addn :
           begin
              for i:=0 to 31 do
                resultset[i]:=tsetconstnode(right).value_set^[i] or tsetconstnode(left).value_set^[i];
              t:=csetconstnode.create(@resultset,left.resulttype);
           end;
        muln :
           begin
              for i:=0 to 31 do
                resultset[i]:=tsetconstnode(right).value_set^[i] and tsetconstnode(left).value_set^[i];
              t:=csetconstnode.create(@resultset,left.resulttype);
           end;
        subn :
           begin
              for i:=0 to 31 do
                resultset[i]:=tsetconstnode(left).value_set^[i] and not(tsetconstnode(right).value_set^[i]);
              t:=csetconstnode.create(@resultset,left.resulttype);
           end;
        symdifn :
           begin
              for i:=0 to 31 do
                resultset[i]:=tsetconstnode(left).value_set^[i] xor tsetconstnode(right).value_set^[i];
              t:=csetconstnode.create(@resultset,left.resulttype);
           end;
        unequaln :
           begin
              b:=true;
              for i:=0 to 31 do
               if tsetconstnode(right).value_set^[i]=tsetconstnode(left).value_set^[i] then
                begin
                  b:=false;
                  break;
                end;
              t:=cordconstnode.create(ord(b),booltype,true);
           end;
        equaln :
           begin
              b:=true;
              for i:=0 to 31 do
               if tsetconstnode(right).value_set^[i]<>tsetconstnode(left).value_set^[i] then
                begin
                  b:=false;
                  break;
                end;
              t:=cordconstnode.create(ord(b),booltype,true);
           end;
        lten :
           begin
             b := true;
             for i := 0 to 31 Do
               if (tsetconstnode(right).value_set^[i] And tsetconstnode(left).value_set^[i]) <>
                   tsetconstnode(left).value_set^[i] Then
                 begin
                   b := false;
                   break
                 end;
             t := cordconstnode.create(ord(b),booltype,true);
           end;
            gten :
                   begin
                    b := true;
                    for i := 0 to 31 Do
                       If (tsetconstnode(left).value_set^[i] And tsetconstnode(right).value_set^[i]) <>
                       tsetconstnode(right).value_set^[i] Then
                         begin
                          b := false;
                          break
                         end;
                     t := cordconstnode.create(ord(b),booltype,true);
                   end;
              end;

{$else}
           case nodetype of
           addn :
             begin
                  resultset:=tsetconstnode(right).value_set^ + tsetconstnode(left).value_set^;
                        t:=csetconstnode.create(@resultset,left.resulttype);
             end;
            muln :
              begin
            resultset:=tsetconstnode(right).value_set^ * tsetconstnode(left).value_set^;
                        t:=csetconstnode.create(@resultset,left.resulttype);
              end;
           subn :
              begin
                resultset:=tsetconstnode(left).value_set^ - tsetconstnode(right).value_set^;
                        t:=csetconstnode.create(@resultset,left.resulttype);
              end;
           symdifn :
              begin
                resultset:=tsetconstnode(right).value_set^ >< tsetconstnode(left).value_set^;
                    t:=csetconstnode.create(@resultset,left.resulttype);
              end;
           unequaln :
              begin
                b:=tsetconstnode(right).value_set^ <> tsetconstnode(left).value_set^;
                t:=cordconstnode.create(byte(b),booltype,true);
              end;
           equaln :
              begin
                b:=tsetconstnode(right).value_set^ = tsetconstnode(left).value_set^;
                t:=cordconstnode.create(byte(b),booltype,true);
              end;
           lten :
              begin
                b:=tsetconstnode(left).value_set^ <= tsetconstnode(right).value_set^;
                t:=cordconstnode.create(byte(b),booltype,true);
              end;
           gten :
              begin
                b:=tsetconstnode(left).value_set^ >= tsetconstnode(right).value_set^;
                t:=cordconstnode.create(byte(b),booltype,true);
              end;
           end;
{$endif}
              result:=t;
              exit;
           end;

         { but an int/int gives real/real! }
         if nodetype=slashn then
          begin
            if (left.resulttype.def.deftype <> floatdef) and
               (right.resulttype.def.deftype <> floatdef) then
              CGMessage(type_h_use_div_for_int);
            inserttypeconv(right,pbestrealtype^);
            inserttypeconv(left,pbestrealtype^);
          end

         { if both are orddefs then check sub types }
         else if (ld.deftype=orddef) and (rd.deftype=orddef) then
           begin
             { optimize multiplacation by a power of 2 }
             if not(cs_check_overflow in aktlocalswitches) and
                (nodetype = muln) and
                (((left.nodetype = ordconstn) and
                  ispowerof2(tordconstnode(left).value,i)) or
                 ((right.nodetype = ordconstn) and
                  ispowerof2(tordconstnode(right).value,i))) then
               begin
                 if left.nodetype = ordconstn then
                   begin
                     tordconstnode(left).value := i;
                     result := cshlshrnode.create(shln,right,left);
                   end
                 else
                   begin
                     tordconstnode(right).value := i;
                     result := cshlshrnode.create(shln,left,right);
                   end;
                 left := nil;
                 right := nil;
                 exit;
               end;

             { 2 booleans? Make them equal to the largest boolean }
             if is_boolean(ld) and is_boolean(rd) then
              begin
                if torddef(left.resulttype.def).size>torddef(right.resulttype.def).size then
                 begin
                   right:=ctypeconvnode.create(right,left.resulttype);
                   ttypeconvnode(right).convtype:=tc_bool_2_int;
                   right.toggleflag(nf_explizit);
                   resulttypepass(right);
                 end
                else if torddef(left.resulttype.def).size<torddef(right.resulttype.def).size then
                 begin
                   left:=ctypeconvnode.create(left,right.resulttype);
                   ttypeconvnode(left).convtype:=tc_bool_2_int;
                   left.toggleflag(nf_explizit);
                   resulttypepass(left);
                 end;
                case nodetype of
                  xorn,
                  ltn,
                  lten,
                  gtn,
                  gten,
                  andn,
                  orn:
                    begin
                    end;
                  unequaln,
                  equaln:
                    begin
                      if not(cs_full_boolean_eval in aktlocalswitches) then
                       begin
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
                             end;
                            result:=hp;
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
                             end;
                            result:=hp;
                            exit;
                          end;
                       end;
                    end;
                  else
                    CGMessage(type_e_mismatch);
                end;
              end
             { Both are chars? }
             else if is_char(rd) and is_char(ld) then
               begin
                 if nodetype=addn then
                  begin
                    resulttype:=cshortstringtype;
                    if not(is_constcharnode(left) and is_constcharnode(right)) then
                     begin
                       inserttypeconv(left,cshortstringtype);
                       hp := genaddsstringcharoptnode(self);
                       result := hp;
                       exit;
                     end;
                  end;
               end
             { is there a signed 64 bit type ? }
             else if ((torddef(rd).typ=s64bit) or (torddef(ld).typ=s64bit)) then
               begin
                  if (torddef(ld).typ<>s64bit) then
                   inserttypeconv(left,cs64bittype);
                  if (torddef(rd).typ<>s64bit) then
                   inserttypeconv(right,cs64bittype);
               end
             { is there a unsigned 64 bit type ? }
             else if ((torddef(rd).typ=u64bit) or (torddef(ld).typ=u64bit)) then
               begin
                  if (torddef(ld).typ<>u64bit) then
                   inserttypeconv(left,cu64bittype);
                  if (torddef(rd).typ<>u64bit) then
                   inserttypeconv(right,cu64bittype);
               end
             { is there a cardinal? }
             else if ((torddef(rd).typ=u32bit) or (torddef(ld).typ=u32bit)) then
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
                     inserttypeconv(left,cs64bittype);
                     inserttypeconv(right,cs64bittype);
                   end
                 else
                   begin
                     { and,or,xor work on bit patterns and don't care
                       about the sign }
                     if nodetype in [andn,orn,xorn] then
                      inserttypeconv_explicit(left,u32bittype)
                     else
                      begin
                        if is_signed(ld) and
                           not(is_constintnode(left) and
                               (tordconstnode(left).value >= 0)) and
                           (cs_check_range in aktlocalswitches) then
                          CGMessage(type_w_mixed_signed_unsigned2);
                        inserttypeconv(left,u32bittype);
                      end;

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
                         inserttypeconv(left,cs64bittype);
                         inserttypeconv(right,cs64bittype);
                       end
                     else
                       begin
                         { and,or,xor work on bit patterns and don't care
                           about the sign }
                         if nodetype in [andn,orn,xorn] then
                          inserttypeconv_explicit(left,u32bittype)
                         else
                          begin
                            if is_signed(rd) and
                               not(is_constintnode(right) and
                                   (tordconstnode(right).value >= 0)) and
                               (cs_check_range in aktlocalswitches) then
                              CGMessage(type_w_mixed_signed_unsigned2);
                            inserttypeconv(right,u32bittype);
                          end;
                       end;
                   end;
               end
             { generic ord conversion is s32bit }
             else
               begin
                 inserttypeconv(right,s32bittype);
                 inserttypeconv(left,s32bittype);
               end;
           end

         { if both are floatdefs, conversion is already done before constant folding }
         else if (ld.deftype=floatdef) then
           begin
             { already converted }
           end

         { left side a setdef, must be before string processing,
           else array constructor can be seen as array of char (PFV) }
         else if (ld.deftype=setdef) then
          begin
            { trying to add a set element? }
            if (nodetype=addn) and (rd.deftype<>setdef) then
             begin
               if (rt=setelementn) then
                begin
                  if not(is_equal(tsetdef(ld).elementtype.def,rd)) then
                   CGMessage(type_e_set_element_are_not_comp);
                end
               else
                CGMessage(type_e_mismatch)
             end
            else
             begin
               if not(nodetype in [addn,subn,symdifn,muln,equaln,unequaln,lten,gten]) then
                CGMessage(type_e_set_operation_unknown);
               { right def must be a also be set }
               if (rd.deftype<>setdef) or not(is_equal(rd,ld)) then
                CGMessage(type_e_set_element_are_not_comp);
             end;

            { ranges require normsets }
            if (tsetdef(ld).settype=smallset) and
               (rt=setelementn) and
               assigned(tsetelementnode(right).right) then
             begin
               { generate a temporary normset def, it'll be destroyed
                 when the symtable is unloaded }
               htype.setdef(tsetdef.create(tsetdef(ld).elementtype,255));
               inserttypeconv(left,htype);
             end;

            { if the right side is also a setdef then the settype must
              be the same as the left setdef }
            if (rd.deftype=setdef) and
               (tsetdef(ld).settype<>tsetdef(rd).settype) then
              inserttypeconv(right,left.resulttype);
          end

         { compare pchar to char arrays by addresses like BP/Delphi }
         else if (is_pchar(ld) and is_chararray(rd)) or
                 (is_pchar(rd) and is_chararray(ld)) then
           begin
             if is_chararray(rd) then
              inserttypeconv(right,left.resulttype)
             else
              inserttypeconv(left,right.resulttype);
           end

         { is one of the operands a string?,
           chararrays are also handled as strings (after conversion), also take
           care of chararray+chararray and chararray+char }
         else if (rd.deftype=stringdef) or (ld.deftype=stringdef) or
                 ((is_chararray(rd) or is_char(rd)) and
                  (is_chararray(ld) or is_char(ld))) then
          begin
            if is_widestring(rd) or is_widestring(ld) then
              begin
                 if not(is_widestring(rd)) then
                   inserttypeconv(right,cwidestringtype);
                 if not(is_widestring(ld)) then
                   inserttypeconv(left,cwidestringtype);
              end
            else if is_ansistring(rd) or is_ansistring(ld) then
              begin
                 if not(is_ansistring(rd)) then
                   inserttypeconv(right,cansistringtype);
                 if not(is_ansistring(ld)) then
                   inserttypeconv(left,cansistringtype);
              end
            else if is_longstring(rd) or is_longstring(ld) then
              begin
                 if not(is_longstring(rd)) then
                   inserttypeconv(right,clongstringtype);
                 if not(is_longstring(ld)) then
                   inserttypeconv(left,clongstringtype);
                 location.loc:=LOC_CREFERENCE;
              end
            else
              begin
                 if not(is_shortstring(ld)) then
                   inserttypeconv(left,cshortstringtype);
                 { don't convert char, that can be handled by the optimized node }
                 if not(is_shortstring(rd) or is_char(rd)) then
                   inserttypeconv(right,cshortstringtype);
              end;

          end

         { pointer comparision and subtraction }
         else if (rd.deftype=pointerdef) and (ld.deftype=pointerdef) then
          begin
            case nodetype of
               equaln,unequaln :
                 begin
                    if is_voidpointer(right.resulttype.def) then
                      inserttypeconv(right,left.resulttype)
                    else if is_voidpointer(left.resulttype.def) then
                      inserttypeconv(left,right.resulttype)
                    else if not(is_equal(ld,rd)) then
                      CGMessage(type_e_mismatch);
                 end;
               ltn,lten,gtn,gten:
                 begin
                    if (cs_extsyntax in aktmoduleswitches) then
                     begin
                       if is_voidpointer(right.resulttype.def) then
                        inserttypeconv(right,left.resulttype)
                       else if is_voidpointer(left.resulttype.def) then
                        inserttypeconv(left,right.resulttype)
                       else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                     end
                    else
                     CGMessage(type_e_mismatch);
                 end;
               subn:
                 begin
                    if (cs_extsyntax in aktmoduleswitches) then
                     begin
                       if is_voidpointer(right.resulttype.def) then
                        inserttypeconv(right,left.resulttype)
                       else if is_voidpointer(left.resulttype.def) then
                        inserttypeconv(left,right.resulttype)
                       else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                     end
                    else
                     CGMessage(type_e_mismatch);
                    resulttype:=s32bittype;
                    exit;
                 end;
               addn:
                 begin
                    if (cs_extsyntax in aktmoduleswitches) then
                     begin
                       if is_voidpointer(right.resulttype.def) then
                        inserttypeconv(right,left.resulttype)
                       else if is_voidpointer(left.resulttype.def) then
                        inserttypeconv(left,right.resulttype)
                       else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                     end
                    else
                     CGMessage(type_e_mismatch);
                    resulttype:=s32bittype;
                    exit;
                 end;
               else
                 CGMessage(type_e_mismatch);
            end;
          end

         { class or interface equation }
         else if is_class_or_interface(rd) or is_class_or_interface(ld) then
          begin
            if is_class_or_interface(rd) and is_class_or_interface(ld) then
             begin
               if tobjectdef(rd).is_related(tobjectdef(ld)) then
                inserttypeconv(right,left.resulttype)
               else
                inserttypeconv(left,right.resulttype);
             end
            else if is_class_or_interface(rd) then
              inserttypeconv(left,right.resulttype)
            else
              inserttypeconv(right,left.resulttype);

            if not(nodetype in [equaln,unequaln]) then
             CGMessage(type_e_mismatch);
          end

         else if (rd.deftype=classrefdef) and (ld.deftype=classrefdef) then
          begin
            if tobjectdef(tclassrefdef(rd).pointertype.def).is_related(
                    tobjectdef(tclassrefdef(ld).pointertype.def)) then
              inserttypeconv(right,left.resulttype)
            else
              inserttypeconv(left,right.resulttype);

            if not(nodetype in [equaln,unequaln]) then
             CGMessage(type_e_mismatch);
          end

         { allows comperasion with nil pointer }
         else if is_class_or_interface(rd) or (rd.deftype=classrefdef) then
          begin
            inserttypeconv(left,right.resulttype);
            if not(nodetype in [equaln,unequaln]) then
             CGMessage(type_e_mismatch);
          end

         else if is_class_or_interface(ld) or (ld.deftype=classrefdef) then
          begin
            inserttypeconv(right,left.resulttype);
            if not(nodetype in [equaln,unequaln]) then
             CGMessage(type_e_mismatch);
          end

       { support procvar=nil,procvar<>nil }
         else if ((ld.deftype=procvardef) and (rt=niln)) or
                 ((rd.deftype=procvardef) and (lt=niln)) then
          begin
            if not(nodetype in [equaln,unequaln]) then
             CGMessage(type_e_mismatch);
          end

       { support dynamicarray=nil,dynamicarray<>nil }
         else if (is_dynamic_array(ld) and (rt=niln)) or
                 (is_dynamic_array(rd) and (lt=niln)) then
          begin
            if not(nodetype in [equaln,unequaln]) then
             CGMessage(type_e_mismatch);
          end

{$ifdef SUPPORT_MMX}
       { mmx support, this must be before the zero based array
         check }
         else if (cs_mmx in aktlocalswitches) and
                 is_mmx_able_array(ld) and
                 is_mmx_able_array(rd) and
                 is_equal(ld,rd) then
            begin
              case nodetype of
                addn,subn,xorn,orn,andn:
                  ;
                { mul is a little bit restricted }
                muln:
                  if not(mmx_type(ld) in [mmxu16bit,mmxs16bit,mmxfixed16]) then
                    CGMessage(type_e_mismatch);
                else
                  CGMessage(type_e_mismatch);
              end;
            end
{$endif SUPPORT_MMX}

         { this is a little bit dangerous, also the left type }
         { pointer to should be checked! This broke the mmx support      }
         else if (rd.deftype=pointerdef) or is_zero_based_array(rd) then
          begin
            if is_zero_based_array(rd) then
              begin
                resulttype.setdef(tpointerdef.create(tarraydef(rd).elementtype));
                inserttypeconv(right,resulttype);
              end;
            inserttypeconv(left,s32bittype);
            if nodetype=addn then
              begin
                if not(cs_extsyntax in aktmoduleswitches) or
                   (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                  CGMessage(type_e_mismatch);
                if (rd.deftype=pointerdef) and
                   (tpointerdef(rd).pointertype.def.size>1) then
                  left:=caddnode.create(muln,left,
                      cordconstnode.create(tpointerdef(rd).pointertype.def.size,s32bittype,true));
              end
            else
              CGMessage(type_e_mismatch);
          end

         else if (ld.deftype=pointerdef) or is_zero_based_array(ld) then
          begin
            if is_zero_based_array(ld) then
              begin
                 resulttype.setdef(tpointerdef.create(tarraydef(ld).elementtype));
                 inserttypeconv(left,resulttype);
              end;
            inserttypeconv(right,s32bittype);
            if nodetype in [addn,subn] then
              begin
                if not(cs_extsyntax in aktmoduleswitches) or
                   (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                  CGMessage(type_e_mismatch);
                if (ld.deftype=pointerdef) and
                   (tpointerdef(ld).pointertype.def.size>1) then
                  right:=caddnode.create(muln,right,
                    cordconstnode.create(tpointerdef(ld).pointertype.def.size,s32bittype,true));
              end
            else
              CGMessage(type_e_mismatch);
         end

         else if (rd.deftype=procvardef) and (ld.deftype=procvardef) and is_equal(rd,ld) then
          begin
            if not (nodetype in [equaln,unequaln]) then
             CGMessage(type_e_mismatch);
          end

         { enums }
         else if (ld.deftype=enumdef) and (rd.deftype=enumdef) then
          begin
            if not(is_equal(ld,rd)) then
             inserttypeconv(right,left.resulttype);
            if not(nodetype in [equaln,unequaln,ltn,lten,gtn,gten]) then
             CGMessage(type_e_mismatch);
          end

         { generic conversion, this is for error recovery }
         else
          begin
            inserttypeconv(left,s32bittype);
            inserttypeconv(right,s32bittype);
          end;

         { set resulttype if not already done }
         if not assigned(resulttype.def) then
          begin
             case nodetype of
                ltn,lten,gtn,gten,equaln,unequaln :
                  resulttype:=booltype;
                slashn :
                  resulttype:=pbestrealtype^;
                addn:
                  begin
                    { for strings, return is always a 255 char string }
                    if is_shortstring(left.resulttype.def) then
                     resulttype:=cshortstringtype
                    else
                     resulttype:=left.resulttype;
                  end;
                else
                  resulttype:=left.resulttype;
             end;
          end;
      end;


    function taddnode.first_addstring: tnode;
      var
        p: tnode;
      begin
        { when we get here, we are sure that both the left and the right }
        { node are both strings of the same stringtype (JM)              }
        case nodetype of
          addn:
            begin
              { note: if you implemented an fpc_shortstr_concat similar to the    }
              { one in i386.inc, you have to override first_addstring like in     }
              { ti386addnode.first_string and implement the shortstring concat    }
              { manually! The generic routine is different from the i386 one (JM) }

              { create the call to the concat routine both strings as arguments }
              result := ccallnode.createintern('fpc_'+
                tstringdef(resulttype.def).stringtypname+'_concat',
                ccallparanode.create(right,ccallparanode.create(left,nil)));
              { we reused the arguments }
              left := nil;
              right := nil;
              firstpass(result);
            end;
          ltn,lten,gtn,gten,equaln,unequaln :
            begin
              { generate better code for s='' and s<>'' }
              if (nodetype in [equaln,unequaln]) and
                 (((left.nodetype=stringconstn) and (str_length(left)=0)) or
                  ((right.nodetype=stringconstn) and (str_length(right)=0))) then
                begin
                  { switch so that the constant is always on the right }
                  if left.nodetype = stringconstn then
                    begin
                      p := left;
                      left := right;
                      right := p;
                    end;
                  if is_shortstring(left.resulttype.def) then
                    { compare the length with 0 }
                    result := caddnode.create(nodetype,
                      cinlinenode.create(in_length_x,false,left),
                      cordconstnode.create(0,s32bittype,false))
                  else
                    begin
                      { compare the pointer with nil (for ansistrings etc), }
                      { faster than getting the length (JM)                 }
                      result:= caddnode.create(nodetype,
                        ctypeconvnode.create(left,voidpointertype),
                        cpointerconstnode.create(0,voidpointertype));
                      taddnode(result).left.toggleflag(nf_explizit);
                    end;
                  { left is reused }
                  left := nil;
                  { right isn't }
                  right.free;
                  right := nil;
                  firstpass(result);
                  exit;
                end;
              { no string constant -> call compare routine }
              result := ccallnode.createintern('fpc_'+
                tstringdef(left.resulttype.def).stringtypname+'_compare',
                ccallparanode.create(right,ccallparanode.create(left,nil)));
              { and compare its result with 0 according to the original operator }
              result := caddnode.create(nodetype,result,
                cordconstnode.create(0,s32bittype,false));
              left := nil;
              right := nil;
              firstpass(result);
            end;
        end;
      end;


    function taddnode.first_addset: tnode;
      var
        procname: string[31];
        tempn: tnode;
        paras: tcallparanode;
        srsym: ttypesym;
      begin
        { get the sym that represents the fpc_normal_set type }
        if not searchsystype('FPC_NORMAL_SET',srsym) then
          internalerror(200108313);

        case nodetype of
          equaln,unequaln,lten,gten:
            begin
              case nodetype of
                equaln,unequaln:
                  procname := 'fpc_set_comp_sets';
                lten,gten:
                  begin
                    procname := 'fpc_set_contains_sets';
                    { (left >= right) = (right <= left) }
                    if nodetype = gten then
                      begin
                        tempn := left;
                        left := right;
                        right := tempn;
                      end;
                   end;
               end;
               { convert the arguments (explicitely) to fpc_normal_set's }
               left := ctypeconvnode.create_explicit(left,srsym.restype);
               right := ctypeconvnode.create_explicit(right,srsym.restype);
               result := ccallnode.createintern(procname,ccallparanode.create(right,
                 ccallparanode.create(left,nil)));
               { left and right are reused as parameters }
               left := nil;
               right := nil;
               { for an unequaln, we have to negate the result of comp_sets }
               if nodetype = unequaln then
                 result := cnotnode.create(result);
            end;
          addn:
            begin
              { optimize first loading of a set }
              if (right.nodetype=setelementn) and
                 not(assigned(tsetelementnode(right).right)) and
                 is_emptyset(left) then
                begin
                  { type cast the value to pass as argument to a byte, }
                  { since that's what the helper expects               }
                  tsetelementnode(right).left :=
                    ctypeconvnode.create(tsetelementnode(right).left,u8bittype);
                  tsetelementnode(right).left.toggleflag(nf_explizit);
                  { set the resulttype to the actual one (otherwise it's }
                  { "fpc_normal_set")                                    }
                  result := ccallnode.createinternres('fpc_set_create_element',
                    ccallparanode.create(tsetelementnode(right).left,nil),
                    resulttype);
                  { reused }
                  tsetelementnode(right).left := nil;
                end
              else
                begin
                  if right.nodetype=setelementn then
                   begin
                     { convert the arguments to bytes, since that's what }
                     { the helper expects                               }
                     tsetelementnode(right).left :=
                       ctypeconvnode.create(tsetelementnode(right).left,
                       u8bittype);
                     tsetelementnode(right).left.toggleflag(nf_explizit);

                     { convert the original set (explicitely) to an   }
                     { fpc_normal_set so we can pass it to the helper }
                     left := ctypeconvnode.create(left,srsym.restype);
                     left.toggleflag(nf_explizit);

                     { add a range or a single element? }
                     if assigned(tsetelementnode(right).right) then
                       begin
                         tsetelementnode(right).right :=
                           ctypeconvnode.create(tsetelementnode(right).right,
                           u8bittype);
                         tsetelementnode(right).right.toggleflag(nf_explizit);

                         { create the call }
                         result := ccallnode.createinternres('fpc_set_set_range',
                           ccallparanode.create(tsetelementnode(right).right,
                           ccallparanode.create(tsetelementnode(right).left,
                           ccallparanode.create(left,nil))),resulttype);
                       end
                     else
                       begin
                         result := ccallnode.createinternres('fpc_set_set_byte',
                           ccallparanode.create(tsetelementnode(right).left,
                           ccallparanode.create(left,nil)),resulttype);
                       end;
                     { remove reused parts from original node }
                     tsetelementnode(right).right := nil;
                     tsetelementnode(right).left := nil;
                     left := nil;
                   end
                  else
                   begin
                     { add two sets }

                     { convert the sets to fpc_normal_set's }
                     left := ctypeconvnode.create(left,srsym.restype);
                     left.toggleflag(nf_explizit);
                     right := ctypeconvnode.create(right,srsym.restype);
                     right.toggleflag(nf_explizit);
                     result := ccallnode.createinternres('fpc_set_add_sets',
                       ccallparanode.create(right,
                       ccallparanode.create(left,nil)),resulttype);
                     { remove reused parts from original node }
                     left := nil;
                     right := nil;
                   end;
                end
            end;
          subn,symdifn,muln:
            begin
              { convert the sets to fpc_normal_set's }
              left := ctypeconvnode.create(left,srsym.restype);
              left.toggleflag(nf_explizit);
              right := ctypeconvnode.create(right,srsym.restype);
              right.toggleflag(nf_explizit);
              paras := ccallparanode.create(right,
                ccallparanode.create(left,nil));
              case nodetype of
                subn:
                  result := ccallnode.createinternres('fpc_set_sub_sets',
                    paras,resulttype);
                symdifn:
                  result := ccallnode.createinternres('fpc_set_symdif_sets',
                    paras,resulttype);
                muln:
                  result := ccallnode.createinternres('fpc_set_mul_sets',
                    paras,resulttype);
              end;
              { remove reused parts from original node }
              left := nil;
              right := nil;
            end;
          else
            internalerror(200108311);
        end;
        firstpass(result);
      end;


    function taddnode.first_add64bitint: tnode;
      var
        procname: string[31];
        temp: tnode;
        power: longint;
      begin
        result := nil;
        { create helper calls mul }
        if nodetype <> muln then
          exit;

        { make sure that if there is a constant, that it's on the right }
        if left.nodetype = ordconstn then
          begin
            temp := right;
            right := left;
            left := temp;
          end;

        { can we use a shift instead of a mul? }
        if (right.nodetype = ordconstn) and
           ispowerof2(tordconstnode(right).value,power) then
          begin
            tordconstnode(right).value := power;
            result := cshlshrnode.create(shln,left,right);
            { left and right are reused }
            left := nil;
            right := nil;
            { return firstpassed new node }
            firstpass(result);
            exit;
          end;

        { otherwise, create the parameters for the helper }
        right := ccallparanode.create(
          cordconstnode.create(ord(cs_check_overflow in aktlocalswitches),booltype,true),
          ccallparanode.create(right,ccallparanode.create(left,nil)));
        left := nil;
        if torddef(resulttype.def).typ = s64bit then
          procname := 'fpc_mul_int64'
        else
          procname := 'fpc_mul_qword';
        result := ccallnode.createintern(procname,right);
        right := nil;
        firstpass(result);
      end;


    function taddnode.first_addfloat: tnode;
      var
        procname: string[31];
        temp: tnode;
        power: longint;
        { do we need to reverse the result ? }
        notnode : boolean;
      begin
        result := nil;
        notnode := false;
        { In non-emulation mode, real opcodes are
          emitted for floating point values.
        }
        if not (cs_fp_emulation in aktmoduleswitches) then
          exit;

        procname := 'FPC_REAL_';
        case nodetype of
          addn : procname := procname + 'ADD';
          muln : procname := procname + 'MUL';
          subn : procname := procname + 'SUB';
          slashn : procname := procname + 'DIV';
          ltn : procname := procname + 'LESS_THAN';
          lten: procname := procname + 'LESS_EQUAL_THAN';
          gtn:
            begin
             procname := procname + 'LESS_EQUAL_THAN';
             notnode := true;
            end;
          gten:
            begin
              procname := procname + 'LESS_THAN';
              notnode := true;
            end;
          equaln: procname := procname + 'EQUAL';
          unequaln :
            begin
              procname := procname + 'EQUAL';
              notnode := true;
            end;
          else
            CGMessage(type_e_mismatch);
        end;
        { otherwise, create the parameters for the helper }
        right := ccallparanode.create(right,ccallparanode.create(left,nil));
        left := nil;
        { do we need to reverse the result }
        if notnode then
           result := cnotnode.create(ccallnode.createintern(procname,right))
        else
           result := ccallnode.createintern(procname,right);
        right := nil;
        firstpass(result);
      end;


    function taddnode.pass_1 : tnode;
      var
         hp      : tnode;
         lt,rt   : tnodetype;
         rd,ld   : tdef;
      begin
         result:=nil;
         { first do the two subtrees }
         firstpass(left);
         firstpass(right);

         if codegenerror then
           exit;

         { load easier access variables }
         rd:=right.resulttype.def;
         ld:=left.resulttype.def;
         rt:=right.nodetype;
         lt:=left.nodetype;

         { int/int gives real/real! }
         if nodetype=slashn then
           begin
             result := first_addfloat;
             if assigned(result) then
               exit;
             location.loc:=LOC_FPUREGISTER;
             { maybe we need an integer register to save }
             { a reference                               }
             if ((left.location.loc<>LOC_FPUREGISTER) or
                 (right.location.loc<>LOC_FPUREGISTER)) and
                (left.registers32=right.registers32) then
               calcregisters(self,1,1,0)
             else
               calcregisters(self,0,1,0);
              { an add node always first loads both the left and the    }
              { right in the fpu before doing the calculation. However, }
              { calcregisters(0,2,0) will overestimate the number of    }
              { necessary registers (it will make it 3 in case one of   }
              { the operands is already in the fpu) (JM)                }
              if ((left.location.loc <> LOC_FPUREGISTER) or
                  (right.location.loc <> LOC_FPUREGISTER)) and
                 (registersfpu < 2) then
                inc(registersfpu);
           end

         { if both are orddefs then check sub types }
         else if (ld.deftype=orddef) and (rd.deftype=orddef) then
           begin
           { 2 booleans ? }
             if is_boolean(ld) and is_boolean(rd) then
              begin
                if not(cs_full_boolean_eval in aktlocalswitches) and
                   (nodetype in [andn,orn]) then
                 begin
                   location.loc:=LOC_JUMP;
                   calcregisters(self,0,0,0);
                 end
                else
                 begin
                   location.loc := LOC_FLAGS;
                   if (left.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                      (left.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                     calcregisters(self,2,0,0)
                   else
                     calcregisters(self,1,0,0);
                 end;
              end
             else
             { Both are chars? only convert to shortstrings for addn }
              if is_char(ld) then
               begin
                 if nodetype=addn then
                  internalerror(200103291);
                 location.loc := LOC_FLAGS;
                 calcregisters(self,1,0,0);
               end
              { is there a 64 bit type ? }
             else if (torddef(ld).typ in [s64bit,u64bit]) then
               begin
                 result := first_add64bitint;
                 if assigned(result) then
                   exit;
                  if nodetype in [addn,subn,muln,andn,orn,xorn] then
                    location.loc := LOC_REGISTER
                  else
                    location.loc := LOC_JUMP;
                  calcregisters(self,2,0,0)
               end
             { is there a cardinal? }
             else if (torddef(ld).typ=u32bit) then
               begin
                  if nodetype in [addn,subn,muln,andn,orn,xorn] then
                    location.loc := LOC_REGISTER
                  else
                    location.loc := LOC_FLAGS;
                 calcregisters(self,1,0,0);
                 { for unsigned mul we need an extra register }
                 if nodetype=muln then
                  inc(registers32);
               end
             { generic s32bit conversion }
             else
               begin
                  if nodetype in [addn,subn,muln,andn,orn,xorn] then
                    location.loc := LOC_REGISTER
                  else
                    location.loc := LOC_FLAGS;
                 calcregisters(self,1,0,0);
               end;
           end

         { left side a setdef, must be before string processing,
           else array constructor can be seen as array of char (PFV) }
         else if (ld.deftype=setdef) then
           begin
             if tsetdef(ld).settype=smallset then
              begin
                 location.loc:=LOC_REGISTER;
                 { are we adding set elements ? }
                 if right.nodetype=setelementn then
                   calcregisters(self,2,0,0)
                 else
                   calcregisters(self,1,0,0);
              end
             else
              begin
                 result := first_addset;
                 if assigned(result) then
                   exit;
                 location.loc:=LOC_CREFERENCE;
                 calcregisters(self,0,0,0);
                 { here we call SET... }
                 if assigned(procinfo) then
                    procinfo.flags:=procinfo.flags or pi_do_call;
              end;
           end

         { compare pchar by addresses like BP/Delphi }
         else if is_pchar(ld) then
           begin
             location.loc:=LOC_REGISTER;
             calcregisters(self,1,0,0);
           end

         { is one of the operands a string }
         else if (ld.deftype=stringdef) then
            begin
              if is_widestring(ld) then
                begin
                   { we use reference counted widestrings so no fast exit here }
                   if assigned(procinfo) then
                     procinfo.no_fast_exit:=true;
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_REGISTER;
                end
              else if is_ansistring(ld) then
                begin
                   { we use ansistrings so no fast exit here }
                   if assigned(procinfo) then
                     procinfo.no_fast_exit:=true;
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_REGISTER;
                end
              else if is_longstring(ld) then
                begin
                   { this is only for add, the comparisaion is handled later }
                   location.loc:=LOC_CREFERENCE;
                end
              else
                begin
                   if canbeaddsstringcharoptnode(self) then
                     begin
                       hp := genaddsstringcharoptnode(self);
                       firstpass(hp);
                       pass_1 := hp;
                       exit;
                     end
                   else
                     begin
                       { Fix right to be shortstring }
                       if is_char(right.resulttype.def) then
                        begin
                          inserttypeconv(right,cshortstringtype);
                          firstpass(right);
                        end;
                     end;
                   if canbeaddsstringcsstringoptnode(self) then
                     begin
                       hp := genaddsstringcsstringoptnode(self);
                       firstpass(hp);
                       pass_1 := hp;
                       exit;
                     end;
                end;
             { otherwise, let addstring convert everything }
              result := first_addstring;
              exit;
           end

         { is one a real float ? }
         else if (rd.deftype=floatdef) or (ld.deftype=floatdef) then
            begin
              result := first_addfloat;
              if assigned(result) then
                exit;
              location.loc:=LOC_FPUREGISTER;
              calcregisters(self,0,1,0);
              { an add node always first loads both the left and the    }
              { right in the fpu before doing the calculation. However, }
              { calcregisters(0,2,0) will overestimate the number of    }
              { necessary registers (it will make it 3 in case one of   }
              { the operands is already in the fpu) (JM)                }
              if ((left.location.loc <> LOC_FPUREGISTER) or
                  (right.location.loc <> LOC_FPUREGISTER)) and
                 (registersfpu < 2) then
                inc(registersfpu);
            end

         { pointer comperation and subtraction }
         else if (ld.deftype=pointerdef) then
            begin
              location.loc:=LOC_REGISTER;
              calcregisters(self,1,0,0);
           end

         else if is_class_or_interface(ld) then
            begin
              location.loc:=LOC_REGISTER;
              calcregisters(self,1,0,0);
            end

         else if (ld.deftype=classrefdef) then
            begin
              location.loc:=LOC_REGISTER;
              calcregisters(self,1,0,0);
            end

         { support procvar=nil,procvar<>nil }
         else if ((ld.deftype=procvardef) and (rt=niln)) or
                 ((rd.deftype=procvardef) and (lt=niln)) then
            begin
              location.loc:=LOC_REGISTER;
              calcregisters(self,1,0,0);
            end

{$ifdef SUPPORT_MMX}
       { mmx support, this must be before the zero based array
         check }
         else if (cs_mmx in aktlocalswitches) and is_mmx_able_array(ld) and
                 is_mmx_able_array(rd) then
            begin
              location.loc:=LOC_MMXREGISTER;
              calcregisters(self,0,0,1);
            end
{$endif SUPPORT_MMX}

         else if (rd.deftype=pointerdef) or (ld.deftype=pointerdef) then
            begin
              location.loc:=LOC_REGISTER;
              calcregisters(self,1,0,0);
            end

         else  if (rd.deftype=procvardef) and (ld.deftype=procvardef) and is_equal(rd,ld) then
           begin
             location.loc:=LOC_REGISTER;
             calcregisters(self,1,0,0);
           end

         else if (ld.deftype=enumdef) then
           begin
              location.loc := LOC_FLAGS;
              calcregisters(self,1,0,0);
           end

{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
                 is_mmx_able_array(ld) and
                 is_mmx_able_array(rd) then
            begin
              location.loc:=LOC_MMXREGISTER;
              calcregisters(self,0,0,1);
            end
{$endif SUPPORT_MMX}

         { the general solution is to convert to 32 bit int }
         else
           begin
             location.loc:=LOC_REGISTER;
             calcregisters(self,1,0,0);
           end;
      end;

{$ifdef state_tracking}
    function Taddnode.track_state_pass(exec_known:boolean):boolean;

    var factval:Tnode;

    begin
    track_state_pass:=false;
    if left.track_state_pass(exec_known) then
        begin
        track_state_pass:=true;
        left.resulttype.def:=nil;
        do_resulttypepass(left);
        end;
    factval:=aktstate.find_fact(left);
    if factval<>nil then
        begin
        track_state_pass:=true;
            left.destroy;
            left:=factval.getcopy;
        end;
    if right.track_state_pass(exec_known) then
        begin
        track_state_pass:=true;
        right.resulttype.def:=nil;
        do_resulttypepass(right);
        end;
    factval:=aktstate.find_fact(right);
    if factval<>nil then
        begin
        track_state_pass:=true;
            right.destroy;
            right:=factval.getcopy;
        end;
    end;
{$endif}

begin
   caddnode:=taddnode;
end.
{
  $Log$
  Revision 1.68  2002-10-08 16:50:43  jonas
    * fixed web bug 2136

  Revision 1.67  2002/10/05 00:47:03  peter
    * support dynamicarray<>nil

  Revision 1.66  2002/10/04 21:19:28  jonas
    * fixed web bug 2139: checking for division by zero fixed

  Revision 1.65  2002/09/07 15:25:02  peter
    * old logs removed and tabs fixed

  Revision 1.64  2002/09/07 12:16:05  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.63  2002/09/04 19:32:56  jonas
    * changed some ctypeconvnode/toggleflag(nf_explizit) combo's to
      ctypeconvnode.create_explicit() statements

  Revision 1.62  2002/08/17 09:23:34  florian
    * first part of procinfo rewrite

  Revision 1.61  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.60  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.59  2002/08/02 07:44:30  jonas
    * made assigned() handling generic
    * add nodes now can also evaluate constant expressions at compile time
      that contain nil nodes

  Revision 1.58  2002/07/26 11:17:52  jonas
    * the optimization of converting a multiplication with a power of two to
      a shl is moved from n386add/secondpass to nadd/resulttypepass

  Revision 1.57  2002/07/23 13:08:16  jonas
    * fixed constant set evaluation of new set handling for non-commutative
      operators

  Revision 1.56  2002/07/23 12:34:29  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.55  2002/07/22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.54  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.53  2002/07/19 11:41:34  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.52  2002/07/14 18:00:43  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.51  2002/05/18 13:34:08  peter
    * readded missing revisions

  Revision 1.50  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.48  2002/05/13 19:54:36  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.47  2002/05/12 16:53:06  peter
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

  Revision 1.46  2002/04/23 19:16:34  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.45  2002/04/04 19:05:56  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.44  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}

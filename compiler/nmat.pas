{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Type checking and register allocation for math nodes

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
unit nmat;

{$i defines.inc}

interface

    uses
       node;

    type
       tmoddivnode = class(tbinopnode)
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;

       tshlshrnode = class(tbinopnode)
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;

       tunaryminusnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;

       tnotnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;

    var
       cmoddivnode : class of tmoddivnode;
       cshlshrnode : class of tshlshrnode;
       cunaryminusnode : class of tunaryminusnode;
       cnotnode : class of tnotnode;


implementation

    uses
      systems,tokens,
      verbose,globals,
{$ifdef support_mmx}
      globtype,
{$endif}
      symconst,symtype,symtable,symdef,types,
      htypechk,pass_1,cpubase,cpuinfo,
{$ifdef newcg}
      cgbase,
{$endif newcg}
      hcodegen,
      ncon,ncnv,ncal;

{****************************************************************************
                              TMODDIVNODE
 ****************************************************************************}

    function tmoddivnode.det_resulttype:tnode;
      var
         t : tnode;
         rd,ld : pdef;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(left,true);
         set_varstate(right,true);
         if codegenerror then
           exit;

         { allow operator overloading }
         t:=self;
         if isbinaryoverloaded(t) then
           begin
              resulttypepass(t);
              result:=t;
              exit;
           end;

         { if one operand is a cardinal and the other is a positive constant, convert the }
         { constant to a cardinal as well so we don't have to do a 64bit division (JM)    }

         { Do the same for qwords and positive constants as well, otherwise things like   }
         { "qword mod 10" are evaluated with int64 as result, which is wrong if the       }
         { "qword" was > high(int64) (JM)                                                 }
         if (left.resulttype.def^.deftype=orddef) and (right.resulttype.def^.deftype=orddef) then
           if (porddef(right.resulttype.def)^.typ in [u32bit,u64bit]) and
              is_constintnode(left) and
              (tordconstnode(left).value >= 0) then
             inserttypeconv(left,right.resulttype)
           else if (porddef(left.resulttype.def)^.typ in [u32bit,u64bit]) and
              is_constintnode(right) and
              (tordconstnode(right).value >= 0) then
             inserttypeconv(right,left.resulttype);

         if (left.resulttype.def^.deftype=orddef) and (right.resulttype.def^.deftype=orddef) and
            (is_64bitint(left.resulttype.def) or is_64bitint(right.resulttype.def) or
             { when mixing cardinals and signed numbers, convert everythign to 64bit (JM) }
             ((porddef(right.resulttype.def)^.typ = u32bit) and
              is_signed(left.resulttype.def)) or
             ((porddef(left.resulttype.def)^.typ = u32bit) and
              is_signed(right.resulttype.def))) then
           begin
              rd:=right.resulttype.def;
              ld:=left.resulttype.def;
              { issue warning if necessary }
              if not (is_64bitint(left.resulttype.def) or is_64bitint(right.resulttype.def)) then
                CGMessage(type_w_mixed_signed_unsigned);
              if is_signed(rd) or is_signed(ld) then
                begin
                   if (porddef(ld)^.typ<>s64bit) then
                     inserttypeconv(left,cs64bittype);
                   if (porddef(rd)^.typ<>s64bit) then
                     inserttypeconv(right,cs64bittype);
                end
              else
                begin
                   if (porddef(ld)^.typ<>u64bit) then
                     inserttypeconv(left,cu64bittype);
                   if (porddef(rd)^.typ<>u64bit) then
                     inserttypeconv(right,cu64bittype);
                end;
              resulttype:=left.resulttype;
           end
         else
           begin
              if not(right.resulttype.def^.deftype=orddef) or
                 not(porddef(right.resulttype.def)^.typ in [s32bit,u32bit]) then
                inserttypeconv(right,s32bittype);

              if not(left.resulttype.def^.deftype=orddef) or
                 not(porddef(left.resulttype.def)^.typ in [s32bit,u32bit]) then
                inserttypeconv(left,s32bittype);

              { the resulttype.def depends on the right side, because the left becomes }
              { always 64 bit                                                      }
              resulttype:=right.resulttype;
           end;
      end;


    function tmoddivnode.pass_1 : tnode;
      var
         t : tnode;
         rv,lv : tconstexprint;

      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         if is_constintnode(left) and is_constintnode(right) then
           begin
              rv:=tordconstnode(right).value;
              lv:=tordconstnode(left).value;

              { check for division by zero }
              if (rv=0) then
               begin
                 Message(parser_e_division_by_zero);
                 { recover }
                 rv:=1;
               end;

              case nodetype of
                modn:
                  t:=genintconstnode(lv mod rv);
                divn:
                  t:=genintconstnode(lv div rv);
              end;
              firstpass(t);
              result:=t;
              exit;
           end;

         { 64bit }
         if (left.resulttype.def^.deftype=orddef) and (right.resulttype.def^.deftype=orddef) and
            (is_64bitint(left.resulttype.def) or is_64bitint(right.resulttype.def)) then
           begin
             calcregisters(self,2,0,0);
           end
         else
           begin
             left_right_max;
             if left.registers32<=right.registers32 then
              inc(registers32);
           end;
         location.loc:=LOC_REGISTER;
      end;



{****************************************************************************
                              TSHLSHRNODE
 ****************************************************************************}

    function tshlshrnode.det_resulttype:tnode;
      var
         t : tnode;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(right,true);
         set_varstate(left,true);
         if codegenerror then
           exit;

         { allow operator overloading }
         t:=self;
         if isbinaryoverloaded(t) then
           begin
              resulttypepass(t);
              result:=t;
              exit;
           end;

         { 64 bit ints have their own shift handling }
         if not(is_64bitint(left.resulttype.def)) then
           begin
              if porddef(left.resulttype.def)^.typ <> u32bit then
               inserttypeconv(left,s32bittype);
           end;

         inserttypeconv(right,s32bittype);

         resulttype:=left.resulttype;
      end;


    function tshlshrnode.pass_1 : tnode;
      var
         t : tnode;
         regs : longint;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         if is_constintnode(left) and is_constintnode(right) then
           begin
              case nodetype of
                 shrn:
                   t:=genintconstnode(tordconstnode(left).value shr tordconstnode(right).value);
                 shln:
                   t:=genintconstnode(tordconstnode(left).value shl tordconstnode(right).value);
              end;
              firstpass(t);
              result:=t;
              exit;
           end;

         { 64 bit ints have their own shift handling }
         if not(is_64bitint(left.resulttype.def)) then
          regs:=1
         else
          regs:=2;

         if (right.nodetype<>ordconstn) then
          inc(regs);
         calcregisters(self,regs,0,0);

         location.loc:=LOC_REGISTER;
      end;


{****************************************************************************
                            TUNARYMINUSNODE
 ****************************************************************************}

    constructor tunaryminusnode.create(expr : tnode);
      begin
         inherited create(unaryminusn,expr);
      end;

    function tunaryminusnode.det_resulttype : tnode;
      var
         t : tnode;
         minusdef : pprocdef;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;
         resulttype:=left.resulttype;

         if (left.resulttype.def^.deftype=floatdef) then
           begin
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(left.resulttype.def) then
             begin
               { if saturation is on, left.resulttype.def isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in aktlocalswitches^) and
                 (porddef(parraydef(resulttype.def)^.definition)^.typ in
                 [s32bit,u32bit]) then
                 CGMessage(type_e_mismatch);
               }
             end
{$endif SUPPORT_MMX}
         else if is_64bitint(left.resulttype.def) then
           begin
           end
         else if (left.resulttype.def^.deftype=orddef) then
           begin
              inserttypeconv(left,s32bittype);
              resulttype:=left.resulttype;
           end
         else
           begin
              if assigned(overloaded_operators[_minus]) then
                minusdef:=overloaded_operators[_minus]^.definition
              else
                minusdef:=nil;
              while assigned(minusdef) do
                begin
                   if is_equal(tparaitem(minusdef^.para.first).paratype.def,left.resulttype.def) and
                      (tparaitem(minusdef^.para.first).next=nil) then
                     begin
                        t:=ccallnode.create(ccallparanode.create(left,nil),
                                            overloaded_operators[_minus],nil,nil);
                        left:=nil;
                        resulttypepass(t);
                        result:=t;
                        exit;
                     end;
                   minusdef:=minusdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;
      end;


    function tunaryminusnode.pass_1 : tnode;
      var
         t : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
           exit;

         if is_constintnode(left) then
           begin
              t:=cordconstnode.create(-tordconstnode(left).value,resulttype);
              firstpass(t);
              result:=t;
              exit;
           end;
         if is_constrealnode(left) then
           begin
              t:=crealconstnode.create(-trealconstnode(left).value_real,resulttype);
              firstpass(t);
              result:=t;
              exit;
           end;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         if (left.resulttype.def^.deftype=floatdef) then
           begin
             location.loc:=LOC_FPU;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(left.resulttype.def) then
             begin
               if (left.location.loc<>LOC_MMXREGISTER) and
                  (registersmmx<1) then
                 registersmmx:=1;
             end
{$endif SUPPORT_MMX}
         else if is_64bitint(left.resulttype.def) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<2) then
                registers32:=2;
              location.loc:=LOC_REGISTER;
           end
         else if (left.resulttype.def^.deftype=orddef) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<1) then
                registers32:=1;
              location.loc:=LOC_REGISTER;
           end;
      end;


{****************************************************************************
                               TNOTNODE
 ****************************************************************************}

    constructor tnotnode.create(expr : tnode);

      begin
         inherited create(notn,expr);
      end;

    function tnotnode.det_resulttype : tnode;
      var
         t : tnode;
         notdef : pprocdef;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         resulttype:=left.resulttype;
         if is_boolean(resulttype.def) then
           begin
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(left.resulttype.def) then
             begin
             end
         else
{$endif SUPPORT_MMX}
           if is_64bitint(left.resulttype.def) then
             begin
             end
         else if is_integer(left.resulttype.def) then
           begin
              if (porddef(left.resulttype.def)^.typ <> u32bit) then
                inserttypeconv(left,s32bittype);
           end
         else
           begin
              if assigned(overloaded_operators[_op_not]) then
                notdef:=overloaded_operators[_op_not]^.definition
              else
                notdef:=nil;
              while assigned(notdef) do
                begin
                   if is_equal(tparaitem(notdef^.para.first).paratype.def,left.resulttype.def) and
                      (tparaitem(notdef^.para.first).next=nil) then
                     begin
                        t:=ccallnode.create(ccallparanode.create(left,nil),
                                            overloaded_operators[_op_not],nil,nil);
                        left:=nil;
                        resulttypepass(t);
                        result:=t;
                        exit;
                     end;
                   notdef:=notdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;
      end;


    function tnotnode.pass_1 : tnode;
      var
         t : tnode;
      begin
         result:=nil;
         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         if (left.nodetype=ordconstn) then
           begin
              if is_boolean(left.resulttype.def) then
                { here we do a boolena(byte(..)) type cast because }
                { boolean(<int64>) is buggy in 1.00                }
                t:=cordconstnode.create(byte(not(boolean(byte(tordconstnode(left).value)))),left.resulttype)
              else
                t:=cordconstnode.create(not(tordconstnode(left).value),left.resulttype);
              firstpass(t);
              result:=t;
              exit;
           end;

         location.loc:=left.location.loc;
         resulttype:=left.resulttype;
         registers32:=left.registers32;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if is_boolean(resulttype.def) then
           begin
             if (location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
              begin
                location.loc:=LOC_REGISTER;
                if (registers32<1) then
                 registers32:=1;
              end;
            { before loading it into flags we need to load it into
              a register thus 1 register is need PM }
{$ifdef i386}
             if left.location.loc<>LOC_JUMP then
               location.loc:=LOC_FLAGS;
{$endif def i386}
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(left.resulttype.def) then
             begin
               if (left.location.loc<>LOC_MMXREGISTER) and
                 (registersmmx<1) then
                 registersmmx:=1;
             end
         else
{$endif SUPPORT_MMX}
           if is_64bitint(left.resulttype.def) then
             begin
                if (location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
                 begin
                   location.loc:=LOC_REGISTER;
                   if (registers32<2) then
                    registers32:=2;
                 end;
             end
         else if is_integer(left.resulttype.def) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<1) then
                registers32:=1;
              location.loc:=LOC_REGISTER;
           end
      end;

begin
   cmoddivnode:=tmoddivnode;
   cshlshrnode:=tshlshrnode;
   cunaryminusnode:=tunaryminusnode;
   cnotnode:=tnotnode;
end.
{
  $Log$
  Revision 1.17  2001-04-02 21:20:31  peter
    * resulttype rewrite

  Revision 1.16  2001/03/20 18:11:03  jonas
    * not (cardinal) now has cardinal instead of longint result (bug reported
      in mailinglist) ("merged")

  Revision 1.15  2001/03/04 10:38:55  jonas
    * fixed 'qword mod/div pos_const' to have qword result

  Revision 1.14  2001/02/20 21:48:17  peter
    * remove nasm hack

  Revision 1.13  2001/01/06 18:28:39  peter
    * fixed wrong notes about locals

  Revision 1.12  2001/01/05 17:36:57  florian
  * the info about exception frames is stored now on the stack
  instead on the heap

  Revision 1.11  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.10  2000/12/16 15:54:01  jonas
    * 'resulttype.def of cardinal shl/shr x' is cardinal instead of longint

  Revision 1.9  2000/11/29 00:30:34  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.8  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.7  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.6  2000/09/27 21:33:22  florian
    * finally nadd.pas compiles

  Revision 1.5  2000/09/27 20:25:44  florian
    * more stuff fixed

  Revision 1.4  2000/09/24 15:06:19  peter
    * use defines.inc

  Revision 1.3  2000/09/22 22:48:54  florian
    * some fixes

  Revision 1.2  2000/09/22 22:09:54  florian
    * more stuff converted

  Revision 1.1  2000/09/20 21:35:12  florian
    * initial revision
}

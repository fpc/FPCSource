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
       node,symtable;

    type
       tmoddivnode = class(tbinopnode)
          function pass_1 : tnode;override;
       end;

       tshlshrnode = class(tbinopnode)
          function pass_1 : tnode;override;
       end;

       tunaryminusnode = class(tunarynode)
         constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tnotnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
       end;

    var
       cmoddivnode : class of tmoddivnode;
       cshlshrnode : class of tshlshrnode;
       cunaryminusnode : class of tunaryminusnode;
       cnotnode : class of tnotnode;

  implementation

    uses
      globtype,systems,tokens,
      cobjects,verbose,globals,
      symconst,aasm,types,
      htypechk,pass_1,cpubase,cpuinfo,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      { for isbinaryoverloaded function }
      nadd,
      ncon,ncnv,ncal;

{****************************************************************************
                              TMODDIVNODE
 ****************************************************************************}
    function tmoddivnode.pass_1 : tnode;
      var
         t : tnode;
         rv,lv : tconstexprint;
         rd,ld : pdef;

      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(right,true);
         firstpass(right);
         set_varstate(right,true);
         if codegenerror then
           exit;

         t:=self;
         if isbinaryoverloaded(t) then
           begin
              pass_1:=t;
              exit;
           end;

         { check for division by zero }
         rv:=tordconstnode(right).value;
         lv:=tordconstnode(left).value;
         if is_constintnode(right) and (rv=0) then
          begin
            Message(parser_e_division_by_zero);
            { recover }
            rv:=1;
          end;

         if is_constintnode(left) and is_constintnode(right) then
           begin
              case nodetype of
                modn:
                  t:=genintconstnode(lv mod rv);
                divn:
                  t:=genintconstnode(lv div rv);
              end;
              firstpass(t);
              pass_1:=t;
              exit;
           end;
         if (left.resulttype^.deftype=orddef) and (right.resulttype^.deftype=orddef) and
            (is_64bitint(left.resulttype) or is_64bitint(right.resulttype)) then
           begin
              rd:=right.resulttype;
              ld:=left.resulttype;
              if (porddef(rd)^.typ=s64bit) or (porddef(ld)^.typ=s64bit) then
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
                end
              else if (porddef(rd)^.typ=u64bit) or (porddef(ld)^.typ=u64bit) then
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
                end;
              resulttype:=left.resulttype;
           end
         else
           begin
              if not(right.resulttype^.deftype=orddef) or
                not(porddef(right.resulttype)^.typ in [s32bit,u32bit]) then
                right:=gentypeconvnode(right,s32bitdef);

              if not(left.resulttype^.deftype=orddef) or
                not(porddef(left.resulttype)^.typ in [s32bit,u32bit]) then
                left:=gentypeconvnode(left,s32bitdef);

              firstpass(left);
              firstpass(right);

{$ifdef cardinalmulfix}
{ if we divide a u32bit by a positive constant, the result is also u32bit (JM) }
              if (left.resulttype^.deftype = orddef) and
                 (left.resulttype^.deftype = orddef) then
                begin
                  if (porddef(left.resulttype)^.typ = u32bit) and
                     is_constintnode(right) and
{                     (porddef(right.resulttype)^.typ <> u32bit) and}
                     (right.value > 0) then
                    begin
                      right := gentypeconvnode(right,u32bitdef);
                      firstpass(right);
                    end;
{ adjust also the left resulttype if necessary }
                  if (porddef(right.resulttype)^.typ = u32bit) and
                     is_constintnode(left) and
    {                 (porddef(left.resulttype)^.typ <> u32bit) and}
                     (left.value > 0) then
                    begin
                      left := gentypeconvnode(left,u32bitdef);
                      firstpass(left);
                    end;
                end;
{$endif cardinalmulfix}

              { the resulttype depends on the right side, because the left becomes }
              { always 64 bit                                                      }
              resulttype:=right.resulttype;

              if codegenerror then
                exit;

              left_right_max;
              if left.registers32<=right.registers32 then
                inc(registers32);
           end;
         location.loc:=LOC_REGISTER;
      end;



{****************************************************************************
                              TSHLSHRNODE
 ****************************************************************************}

    function tshlshrnode.pass_1 : tnode;
      var
         t : tnode;
         regs : longint;
      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(left,true);
         firstpass(right);
         set_varstate(right,true);
         if codegenerror then
           exit;

         t:=self;
         if isbinaryoverloaded(t) then
           begin
              pass_1:=t;
              exit;
           end;

         if is_constintnode(left) and is_constintnode(right) then
           begin
              case nodetype of
                 shrn:
                   t:=genintconstnode(tordconstnode(left).value shr tordconstnode(right).value);
                 shln:
                   t:=genintconstnode(tordconstnode(left).value shl tordconstnode(right).value);
              end;
              firstpass(t);
              pass_1:=t;
              exit;
           end;
         { 64 bit ints have their own shift handling }
         if not(is_64bitint(left.resulttype)) then
           begin
              left:=gentypeconvnode(left,s32bitdef);
              firstpass(left);
              regs:=1;
              resulttype:=s32bitdef;
           end
         else
           begin
              resulttype:=left.resulttype;
              regs:=2;
           end;

         right:=gentypeconvnode(right,s32bitdef);
         firstpass(right);

         if codegenerror then
           exit;

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

   function tunaryminusnode.pass_1 : tnode;
      var
         t : tnode;
         minusdef : pprocdef;
      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(left,true);
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         resulttype:=left.resulttype;
         if codegenerror then
           exit;
         if is_constintnode(left) then
           begin
              t:=genintconstnode(-tordconstnode(left).value);
              firstpass(t);
              pass_1:=t;
              exit;
           end;
           { nasm can not cope with negativ reals !! }
         if is_constrealnode(left)
{$ifdef i386}
           and not(aktoutputformat in [as_i386_nasmcoff,as_i386_nasmelf,as_i386_nasmobj])
{$endif i386}
             then
           begin
              t:=genrealconstnode(-trealconstnode(left).value_real,bestrealdef^);
              firstpass(t);
              pass_1:=t;
              exit;
           end;
         if (left.resulttype^.deftype=floatdef) then
           begin
              if pfloatdef(left.resulttype)^.typ=f32bit then
                begin
                   if (left.location.loc<>LOC_REGISTER) and
                     (registers32<1) then
                     registers32:=1;
                   location.loc:=LOC_REGISTER;
                end
              else
                location.loc:=LOC_FPU;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(left.resulttype) then
             begin
               if (left.location.loc<>LOC_MMXREGISTER) and
                 (registersmmx<1) then
                 registersmmx:=1;
               { if saturation is on, left.resulttype isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in aktlocalswitches^) and
                 (porddef(parraydef(resulttype)^.definition)^.typ in
                 [s32bit,u32bit]) then
                 CGMessage(type_e_mismatch);
               }
             end
{$endif SUPPORT_MMX}
         else if is_64bitint(left.resulttype) then
           begin
              firstpass(left);
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
              registers32:=left.registers32;
              if codegenerror then
                exit;
              if (left.location.loc<>LOC_REGISTER) and
                (registers32<2) then
              registers32:=2;
              location.loc:=LOC_REGISTER;
              resulttype:=left.resulttype;
           end
         else if (left.resulttype^.deftype=orddef) then
           begin
              left:=gentypeconvnode(left,s32bitdef);
              firstpass(left);
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
              registers32:=left.registers32;
              if codegenerror then
                exit;
              if (left.location.loc<>LOC_REGISTER) and
                (registers32<1) then
              registers32:=1;
              location.loc:=LOC_REGISTER;
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
                   if is_equal(pparaitem(minusdef^.para^.first)^.paratype.def,left.resulttype) and
                      (pparaitem(minusdef^.para^.first)^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[_minus],nil);
                        tcallnode(t).left:=gencallparanode(left,nil);
                        left:=nil;
                        firstpass(t);
                        pass_1:=t;
                        exit;
                     end;
                   minusdef:=minusdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;
      end;


{****************************************************************************
                               TNOTNODE
 ****************************************************************************}

    constructor tnotnode.create(expr : tnode);

      begin
         inherited create(notn,expr);
      end;

    function tnotnode.pass_1 : tnode;
      var
         t : tnode;
         notdef : pprocdef;
      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         if (left.nodetype=ordconstn) then
           begin
              if is_boolean(left.resulttype) then
                { here we do a boolena(byte(..)) type cast because }
                { boolean(<int64>) is buggy in 1.00                }
                t:=genordinalconstnode(byte(not(boolean(byte(tordconstnode(left).value)))),left.resulttype)
              else
                t:=genordinalconstnode(not(tordconstnode(left).value),left.resulttype);
              firstpass(t);
              pass_1:=t;
              exit;
           end;
         resulttype:=left.resulttype;
         location.loc:=left.location.loc;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if is_boolean(resulttype) then
           begin
             registers32:=left.registers32;
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
             is_mmx_able_array(left.resulttype) then
             begin
               if (left.location.loc<>LOC_MMXREGISTER) and
                 (registersmmx<1) then
                 registersmmx:=1;
             end
         else
{$endif SUPPORT_MMX}
           if is_64bitint(left.resulttype) then
             begin
                registers32:=left.registers32;
                if (location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
                 begin
                   location.loc:=LOC_REGISTER;
                   if (registers32<2) then
                    registers32:=2;
                 end;
             end
         else if is_integer(left.resulttype) then
           begin
              left:=gentypeconvnode(left,s32bitdef);
              firstpass(left);
              if codegenerror then
                exit;

              resulttype:=left.resulttype;
              registers32:=left.registers32;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<1) then
                registers32:=1;
              location.loc:=LOC_REGISTER;
           end
         else
           begin
              if assigned(overloaded_operators[_op_not]) then
                notdef:=overloaded_operators[_op_not]^.definition
              else
                notdef:=nil;
              while assigned(notdef) do
                begin
                   if is_equal(pparaitem(notdef^.para^.first)^.paratype.def,left.resulttype) and
                      (pparaitem(notdef^.para^.first)^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[_op_not],nil);
                        tcallnode(t).left:=gencallparanode(left,nil);
                        left:=nil;
                        firstpass(t);
                        pass_1:=t;
                        exit;
                     end;
                   notdef:=notdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;

         registersfpu:=left.registersfpu;
      end;


begin
   cmoddivnode:=tmoddivnode;
   cshlshrnode:=tshlshrnode;
   cunaryminusnode:=tunaryminusnode;
   cnotnode:=tnotnode;
end.
{
  $Log$
  Revision 1.7  2000-10-01 19:48:24  peter
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
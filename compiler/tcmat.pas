{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit tcmat;

{$i defines.inc}

interface

    uses
      tree;

    procedure firstmoddiv(var p : ptree);
    procedure firstshlshr(var p : ptree);
    procedure firstunaryminus(var p : ptree);
    procedure firstnot(var p : ptree);


implementation

    uses
      globtype,systems,tokens,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      htypechk,pass_1,cpubase,cpuinfo,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      { for isbinaryoverloaded function }
      tcadd;

{*****************************************************************************
                             FirstModDiv
*****************************************************************************}

    procedure firstmoddiv(var p : ptree);
      var
         t : ptree;
         rv,lv : tconstexprint;
         rd,ld : pdef;

      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;

         if isbinaryoverloaded(p) then
           exit;

         { check for division by zero }
         rv:=p^.right^.value;
         lv:=p^.left^.value;
         if is_constintnode(p^.right) and (rv=0) then
          begin
            Message(parser_e_division_by_zero);
            { recover }
            rv:=1;
          end;

         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              case p^.treetype of
                modn:
                  t:=genintconstnode(lv mod rv);
                divn:
                  t:=genintconstnode(lv div rv);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         if (p^.left^.resulttype^.deftype=orddef) and (p^.right^.resulttype^.deftype=orddef) and
            (is_64bitint(p^.left^.resulttype) or is_64bitint(p^.right^.resulttype)) then
           begin
              rd:=p^.right^.resulttype;
              ld:=p^.left^.resulttype;
              if (porddef(rd)^.typ=s64bit) or (porddef(ld)^.typ=s64bit) then
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
                end
              else if (porddef(rd)^.typ=u64bit) or (porddef(ld)^.typ=u64bit) then
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
                end;
              p^.resulttype:=p^.left^.resulttype;
           end
         else
           begin
              if not(p^.right^.resulttype^.deftype=orddef) or
                not(porddef(p^.right^.resulttype)^.typ in [s32bit,u32bit]) then
                p^.right:=gentypeconvnode(p^.right,s32bitdef);

              if not(p^.left^.resulttype^.deftype=orddef) or
                not(porddef(p^.left^.resulttype)^.typ in [s32bit,u32bit]) then
                p^.left:=gentypeconvnode(p^.left,s32bitdef);

              firstpass(p^.left);
              firstpass(p^.right);

{$ifdef cardinalmulfix}
{ if we divide a u32bit by a positive constant, the result is also u32bit (JM) }
              if (p^.left^.resulttype^.deftype = orddef) and
                 (p^.left^.resulttype^.deftype = orddef) then
                begin
                  if (porddef(p^.left^.resulttype)^.typ = u32bit) and
                     is_constintnode(p^.right) and
{                     (porddef(p^.right^.resulttype)^.typ <> u32bit) and}
                     (p^.right^.value > 0) then
                    begin
                      p^.right := gentypeconvnode(p^.right,u32bitdef);
                      firstpass(p^.right);
                    end;
{ adjust also the left resulttype if necessary }
                  if (porddef(p^.right^.resulttype)^.typ = u32bit) and
                     is_constintnode(p^.left) and
    {                 (porddef(p^.left^.resulttype)^.typ <> u32bit) and}
                     (p^.left^.value > 0) then
                    begin
                      p^.left := gentypeconvnode(p^.left,u32bitdef);
                      firstpass(p^.left);
                    end;
                end;
{$endif cardinalmulfix}

              { the resulttype depends on the right side, because the left becomes }
              { always 64 bit                                                      }
              p^.resulttype:=p^.right^.resulttype;

              if codegenerror then
                exit;

              left_right_max(p);
              if p^.left^.registers32<=p^.right^.registers32 then
                inc(p^.registers32);
           end;
         p^.location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             FirstShlShr
*****************************************************************************}

    procedure firstshlshr(var p : ptree);
      var
         t : ptree;
         regs : longint;
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;

         if isbinaryoverloaded(p) then
           exit;

         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              case p^.treetype of
                 shrn:
                   t:=genintconstnode(p^.left^.value shr p^.right^.value);
                 shln:
                   t:=genintconstnode(p^.left^.value shl p^.right^.value);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         { 64 bit ints have their own shift handling }
         if not(is_64bitint(p^.left^.resulttype)) then
           begin
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              regs:=1;
              p^.resulttype:=s32bitdef;
           end
         else
           begin
              p^.resulttype:=p^.left^.resulttype;
              regs:=2;
           end;

         p^.right:=gentypeconvnode(p^.right,s32bitdef);
         firstpass(p^.right);

         if codegenerror then
           exit;

         if (p^.right^.treetype<>ordconstn) then
          inc(regs);
         calcregisters(p,regs,0,0);

         p^.location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             FirstUnaryMinus
*****************************************************************************}

    procedure firstunaryminus(var p : ptree);
      var
         t : ptree;
         minusdef : pprocdef;
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=p^.left^.resulttype;
         if codegenerror then
           exit;
         if is_constintnode(p^.left) then
           begin
              t:=genintconstnode(-p^.left^.value);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
           { nasm can not cope with negativ reals !! }
         if is_constrealnode(p^.left)
{$ifdef i386}
           and not(aktoutputformat in [as_i386_nasmcoff,as_i386_nasmelf,as_i386_nasmobj])
{$endif i386}
             then
           begin
              t:=genrealconstnode(-p^.left^.value_real,bestrealdef^);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         if (p^.left^.resulttype^.deftype=floatdef) then
           begin
              if pfloatdef(p^.left^.resulttype)^.typ=f32bit then
                begin
                   if (p^.left^.location.loc<>LOC_REGISTER) and
                     (p^.registers32<1) then
                     p^.registers32:=1;
                   p^.location.loc:=LOC_REGISTER;
                end
              else
                p^.location.loc:=LOC_FPU;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(p^.left^.resulttype) then
             begin
               if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                 (p^.registersmmx<1) then
                 p^.registersmmx:=1;
               { if saturation is on, p^.left^.resulttype isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in aktlocalswitches^) and
                 (porddef(parraydef(p^.resulttype)^.definition)^.typ in
                 [s32bit,u32bit]) then
                 CGMessage(type_e_mismatch);
               }
             end
{$endif SUPPORT_MMX}
         else if is_64bitint(p^.left^.resulttype) then
           begin
              firstpass(p^.left);
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              p^.registers32:=p^.left^.registers32;
              if codegenerror then
                exit;
              if (p^.left^.location.loc<>LOC_REGISTER) and
                (p^.registers32<2) then
              p^.registers32:=2;
              p^.location.loc:=LOC_REGISTER;
              p^.resulttype:=p^.left^.resulttype;
           end
         else if (p^.left^.resulttype^.deftype=orddef) then
           begin
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              p^.registers32:=p^.left^.registers32;
              if codegenerror then
                exit;
              if (p^.left^.location.loc<>LOC_REGISTER) and
                (p^.registers32<1) then
              p^.registers32:=1;
              p^.location.loc:=LOC_REGISTER;
              p^.resulttype:=p^.left^.resulttype;
           end
         else
           begin
              if assigned(overloaded_operators[_minus]) then
                minusdef:=overloaded_operators[_minus]^.definition
              else
                minusdef:=nil;
              while assigned(minusdef) do
                begin
                   if is_equal(pparaitem(minusdef^.para^.first)^.paratype.def,p^.left^.resulttype) and
                      (pparaitem(minusdef^.para^.first)^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[_minus],nil);
                        t^.left:=gencallparanode(p^.left,nil);
                        putnode(p);
                        p:=t;
                        firstpass(p);
                        exit;
                     end;
                   minusdef:=minusdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;
      end;


{*****************************************************************************
                               FirstNot
*****************************************************************************}

    procedure firstnot(var p : ptree);
      var
         t : ptree;
         notdef : pprocdef;
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;

         if (p^.left^.treetype=ordconstn) then
           begin
              if is_boolean(p^.left^.resulttype) then
                { here we do a boolena(byte(..)) type cast because }
                { boolean(<int64>) is buggy in 1.00                }
                t:=genordinalconstnode(byte(not(boolean(byte(p^.left^.value)))),p^.left^.resulttype)
              else
                t:=genordinalconstnode(not(p^.left^.value),p^.left^.resulttype);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         p^.resulttype:=p^.left^.resulttype;
         p^.location.loc:=p^.left^.location.loc;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if is_boolean(p^.resulttype) then
           begin
             p^.registers32:=p^.left^.registers32;
             if (p^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
              begin
                p^.location.loc:=LOC_REGISTER;
                if (p^.registers32<1) then
                 p^.registers32:=1;
              end;
            { before loading it into flags we need to load it into
              a register thus 1 register is need PM }
{$ifdef i386}
             if p^.left^.location.loc<>LOC_JUMP then
               p^.location.loc:=LOC_FLAGS;
{$endif def i386}
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(p^.left^.resulttype) then
             begin
               if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                 (p^.registersmmx<1) then
                 p^.registersmmx:=1;
             end
         else
{$endif SUPPORT_MMX}
           if is_64bitint(p^.left^.resulttype) then
             begin
                p^.registers32:=p^.left^.registers32;
                if (p^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
                 begin
                   p^.location.loc:=LOC_REGISTER;
                   if (p^.registers32<2) then
                    p^.registers32:=2;
                 end;
             end
         else if is_integer(p^.left^.resulttype) then
           begin
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              if codegenerror then
                exit;

              p^.resulttype:=p^.left^.resulttype;
              p^.registers32:=p^.left^.registers32;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

              if (p^.left^.location.loc<>LOC_REGISTER) and
                 (p^.registers32<1) then
                p^.registers32:=1;
              p^.location.loc:=LOC_REGISTER;
           end
         else
           begin
              if assigned(overloaded_operators[_op_not]) then
                notdef:=overloaded_operators[_op_not]^.definition
              else
                notdef:=nil;
              while assigned(notdef) do
                begin
                   if is_equal(pparaitem(notdef^.para^.first)^.paratype.def,p^.left^.resulttype) and
                      (pparaitem(notdef^.para^.first)^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[_op_not],nil);
                        t^.left:=gencallparanode(p^.left,nil);
                        putnode(p);
                        p:=t;
                        firstpass(p);
                        exit;
                     end;
                   notdef:=notdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;

         p^.registersfpu:=p^.left^.registersfpu;
      end;



end.
{
  $Log$
  Revision 1.5  2000-09-24 21:19:53  peter
    * delphi compile fixes

  Revision 1.4  2000/08/17 12:03:48  florian
    * fixed several problems with the int64 constants

  Revision 1.3  2000/08/16 13:06:07  florian
    + support of 64 bit integer constants

  Revision 1.2  2000/07/13 11:32:52  michael
  + removed logs

}

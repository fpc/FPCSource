{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
interface

    uses
      tree;

    procedure firstmoddiv(var p : ptree);
    procedure firstshlshr(var p : ptree);
    procedure firstumminus(var p : ptree);
    procedure firstnot(var p : ptree);


implementation

    uses
      globtype,systems,tokens,
      cobjects,verbose,globals,
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
                             FirstModDiv
*****************************************************************************}

    procedure firstmoddiv(var p : ptree);
      var
         t : ptree;
         rv,lv : longint;
      begin
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
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
                modn : t:=genordinalconstnode(lv mod rv,s32bitdef);
                divn : t:=genordinalconstnode(lv div rv,s32bitdef);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         if not(p^.right^.resulttype^.deftype=orddef) or
           not(porddef(p^.right^.resulttype)^.typ in [s32bit,u32bit]) then
           p^.right:=gentypeconvnode(p^.right,s32bitdef);

         if not(p^.left^.resulttype^.deftype=orddef) or
           not(porddef(p^.left^.resulttype)^.typ in [s32bit,u32bit]) then
           p^.left:=gentypeconvnode(p^.left,s32bitdef);

         firstpass(p^.left);
         firstpass(p^.right);

         { the resulttype depends on the right side, because the left becomes }
         { always 64 bit                                                      }
         p^.resulttype:=p^.right^.resulttype;

         if codegenerror then
           exit;

         left_right_max(p);
         if p^.left^.registers32<=p^.right^.registers32 then
           inc(p^.registers32);
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
         firstpass(p^.right);
         if codegenerror then
           exit;

         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              case p^.treetype of
                 shrn : t:=genordinalconstnode(p^.left^.value shr p^.right^.value,s32bitdef);
                 shln : t:=genordinalconstnode(p^.left^.value shl p^.right^.value,s32bitdef);
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
                             FirstUmMinus
*****************************************************************************}

    procedure firstumminus(var p : ptree);
      var
         t : ptree;
         minusdef : pprocdef;
      begin
         firstpass(p^.left);
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
              t:=genordinalconstnode(-p^.left^.value,s32bitdef);
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
              t:=genrealconstnode(-p^.left^.value_real);
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
              if assigned(overloaded_operators[minus]) then
                minusdef:=overloaded_operators[minus]^.definition
              else
                minusdef:=nil;
              while assigned(minusdef) do
                begin
                   if (minusdef^.para1^.data=p^.left^.resulttype) and
                     (minusdef^.para1^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[minus],nil);
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
      begin
         firstpass(p^.left);
         if codegenerror then
           exit;

         if (p^.left^.treetype=ordconstn) then
           begin
              if is_boolean(p^.left^.resulttype) then
               t:=genordinalconstnode(byte(not(boolean(p^.left^.value))),p^.left^.resulttype)
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
           end;
         p^.registersfpu:=p^.left^.registersfpu;
      end;


end.
{
  $Log$
  Revision 1.9  1998-12-11 16:10:12  florian
    + shifting for 64 bit ints added
    * bug in getexplicitregister32 fixed: usableregs wasn't decremented !!

  Revision 1.8  1998/12/11 00:03:56  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.7  1998/11/13 10:16:38  peter
    * fixed constant not(boolean)

  Revision 1.6  1998/11/05 14:26:01  peter
    * fixed shlshr which would push ecx when not needed

  Revision 1.5  1998/10/20 13:12:39  peter
    * fixed 'not not boolean', the location was not set to register

  Revision 1.4  1998/10/13 16:50:25  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.3  1998/10/13 13:10:33  peter
    * new style for m68k/i386 infos and enums

  Revision 1.2  1998/10/11 14:31:20  peter
    + checks for division by zero

  Revision 1.1  1998/09/23 20:42:24  peter
    * splitted pass_1

}

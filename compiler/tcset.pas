{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Type checking and register allocation for set/case nodes

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
unit tcset;
interface

    uses
      tree;

    procedure firstsetelement(var p : ptree);
    procedure firstin(var p : ptree);
    procedure firstrange(var p : ptree);
    procedure firstcase(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symtable,aasm,types,
      hcodegen,htypechk,pass_1,
      tccnv
{$ifdef i386}
{$ifndef OLDASM}
      ,i386base
{$else}
      ,i386
{$endif}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,m68k,tgen68k
{$endif}
      ;

{*****************************************************************************
                           FirstSetElement
*****************************************************************************}

    procedure firstsetelement(var p : ptree);
      begin
         firstpass(p^.left);
         if codegenerror then
          exit;

         if assigned(p^.right) then
          begin
            firstpass(p^.right);
            if codegenerror then
             exit;
          end;

         calcregisters(p,0,0,0);
         p^.resulttype:=p^.left^.resulttype;
         set_location(p^.location,p^.left^.location);
      end;


{*****************************************************************************
                              FirstIn
*****************************************************************************}

    procedure firstin(var p : ptree);
      type
        byteset = set of byte;
      var
        t : ptree;
      begin
         p^.location.loc:=LOC_FLAGS;
         p^.resulttype:=booldef;

         firstpass(p^.right);
         if codegenerror then
          exit;

         { Convert array constructor first to set }
         if is_array_constructor(p^.right^.resulttype) then
          begin
            arrayconstructor_to_set(p^.right);
            firstpass(p^.right);
            if codegenerror then
             exit;
          end;

         if p^.right^.resulttype^.deftype<>setdef then
           CGMessage(sym_e_set_expected);

         firstpass(p^.left);
         if codegenerror then
           exit;

         { empty set then return false }
         if not assigned(psetdef(p^.right^.resulttype)^.setof) then
          begin
            t:=genordinalconstnode(0,booldef);
            disposetree(p);
            firstpass(t);
            p:=t;
            exit;
          end;

         { type conversion/check }
         p^.left:=gentypeconvnode(p^.left,psetdef(p^.right^.resulttype)^.setof);
         firstpass(p^.left);
         if codegenerror then
           exit;

         { constant evaulation }
         if (p^.left^.treetype=ordconstn) and (p^.right^.treetype=setconstn) then
          begin
            t:=genordinalconstnode(byte(p^.left^.value in byteset(p^.right^.value_set^)),booldef);
            disposetree(p);
            firstpass(t);
            p:=t;
            exit;
          end;

         left_right_max(p);
         { this is not allways true due to optimization }
         { but if we don't set this we get problems with optimizing self code }
         if psetdef(p^.right^.resulttype)^.settype<>smallset then
           procinfo.flags:=procinfo.flags or pi_do_call
         else
           begin
              { a smallset needs maybe an misc. register }
              if (p^.left^.treetype<>ordconstn) and
                not(p^.right^.location.loc in [LOC_CREGISTER,LOC_REGISTER]) and
                (p^.right^.registers32<1) then
                inc(p^.registers32);
           end;
      end;


{*****************************************************************************
                              FirstRange
*****************************************************************************}

    procedure firstrange(var p : ptree);
      var
         ct : tconverttype;
      begin
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
           exit;
         { both types must be compatible }
         if not(is_equal(p^.left^.resulttype,p^.right^.resulttype)) and
            (isconvertable(p^.left^.resulttype,p^.right^.resulttype,ct,ordconstn,false)=0) then
           CGMessage(type_e_mismatch);
         { Check if only when its a constant set }
         if (p^.left^.treetype=ordconstn) and (p^.right^.treetype=ordconstn) then
          begin
          { upper limit must be greater or equal than lower limit }
          { not if u32bit }
            if (p^.left^.value>p^.right^.value) and
               (( p^.left^.value<0) or (p^.right^.value>=0)) then
              CGMessage(cg_e_upper_lower_than_lower);
          end;
        left_right_max(p);
        p^.resulttype:=p^.left^.resulttype;
        set_location(p^.location,p^.left^.location);
      end;


{*****************************************************************************
                              FirstCase
*****************************************************************************}

    procedure firstcase(var p : ptree);
      var
         old_t_times : longint;
         hp : ptree;
      begin
         { evalutes the case expression }
         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);
         if codegenerror then
           exit;
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { walk through all instructions }

         {   estimates the repeat of each instruction }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           begin
              t_times:=t_times div case_count_labels(p^.nodes);
              if t_times<1 then
                t_times:=1;
           end;
         {   first case }
         hp:=p^.right;
         while assigned(hp) do
           begin
              cleartempgen;
              firstpass(hp^.right);

              { searchs max registers }
              if hp^.right^.registers32>p^.registers32 then
                p^.registers32:=hp^.right^.registers32;
              if hp^.right^.registersfpu>p^.registersfpu then
                p^.registersfpu:=hp^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp^.right^.registersmmx>p^.registersmmx then
                p^.registersmmx:=hp^.right^.registersmmx;
{$endif SUPPORT_MMX}

              hp:=hp^.left;
           end;

         { may be handle else tree }
         if assigned(p^.elseblock) then
           begin
              cleartempgen;
              firstpass(p^.elseblock);
              if codegenerror then
                exit;
              if p^.registers32<p^.elseblock^.registers32 then
                p^.registers32:=p^.elseblock^.registers32;
              if p^.registersfpu<p^.elseblock^.registersfpu then
                p^.registersfpu:=p^.elseblock^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.elseblock^.registersmmx then
                p^.registersmmx:=p^.elseblock^.registersmmx;
{$endif SUPPORT_MMX}
           end;
         t_times:=old_t_times;

         { there is one register required for the case expression }
         if p^.registers32<1 then p^.registers32:=1;
      end;


end.
{
  $Log$
  Revision 1.9  1999-05-01 13:24:58  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.8  1999/04/14 15:00:13  peter
    * forgot firstpass after array->set conversion

  Revision 1.7  1999/03/02 18:22:36  peter
    * arrayconstructor convert for in

  Revision 1.6  1999/02/22 02:15:55  peter
    * updates for ag386bin

  Revision 1.5  1998/12/18 17:15:40  peter
    * added 'in []' support

  Revision 1.4  1998/12/11 00:03:58  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.3  1998/11/13 10:17:06  peter
    + constant eval for in

  Revision 1.2  1998/10/06 20:49:13  peter
    * m68k compiler compiles again

  Revision 1.1  1998/09/23 20:42:24  peter
    * splitted pass_1

}


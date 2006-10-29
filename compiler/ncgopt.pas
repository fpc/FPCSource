{
    Copyright (c) 1998-2003 by Jonas Maebe

    This unit implements the generic implementation of optimized nodes

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
unit ncgopt;

{$i fpcdefs.inc}

interface
uses node, nopt;

type
  tcgaddsstringcharoptnode = class(taddsstringcharoptnode)
     function pass_typecheck: tnode; override;
     function pass_1: tnode; override;
     procedure pass_generate_code; override;
  end;


implementation

uses
  globtype,globals,
  pass_1,defutil,htypechk,
  symdef,paramgr,
  aasmbase,aasmtai,aasmdata,
  ncnv, ncon, pass_2,
  cgbase, cpubase,
  tgobj, cgobj, cgutils,ncgutil;


{*****************************************************************************
                             TCGADDOPTNODE
*****************************************************************************}

function tcgaddsstringcharoptnode.pass_typecheck: tnode;
begin
  pass_typecheck := nil;
  typecheckpass(left);
  typecheckpass(right);
  if codegenerror then
    exit;
  { update the curmaxlen field (before converting to a string!) }
  updatecurmaxlen;
  if not is_shortstring(left.resultdef) then
    inserttypeconv(left,cshortstringtype);
  resultdef:=left.resultdef;
end;


function tcgaddsstringcharoptnode.pass_1: tnode;
begin
  pass_1 := nil;
  firstpass(left);
  firstpass(right);
  if codegenerror then
    exit;
  expectloc:=LOC_REFERENCE;
  if not is_constcharnode(right) then
    { it's not sure we need the register, but we can't know it here yet }
    calcregisters(self,2,0,0)
  else
    calcregisters(self,1,0,0);
end;


procedure tcgaddsstringcharoptnode.pass_generate_code;
var
  l: tasmlabel;
  href,href2 :  treference;
  hreg, lengthreg: tregister;
  checklength: boolean;
  len : integer;
begin
  { first, we have to more or less replicate some code from }
  { ti386addnode.pass_generate_code                                     }
  secondpass(left);
  if not(tg.istemp(left.location.reference) and
         (tg.sizeoftemp(current_asmdata.CurrAsmList,left.location.reference) = 256)) then
    begin
       tg.Gettemp(current_asmdata.CurrAsmList,256,tt_normal,href);
       cg.g_copyshortstring(current_asmdata.CurrAsmList,left.location.reference,href,255);
       location_freetemp(current_asmdata.CurrAsmList,left.location);
       { return temp reference }
       location_reset(left.location,LOC_REFERENCE,def_cgsize(resultdef));
       left.location.reference:=href;
    end;
  secondpass(right);
  { special case for string := string + char (JM) }
  hreg:=NR_NO;

  { we have to load the char before checking the length, because we }
  { may need registers from the reference                           }

  { is it a constant char? }
  if not is_constcharnode(right) then
    { no, make sure it is in a register }
    if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
      begin
        { get register for the char }
        hreg := cg.getintregister(current_asmdata.CurrAsmList,OS_8);
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_8,OS_8,right.location.reference,hreg);
        { I don't think a temp char exists, but it won't hurt (JM) }
        tg.ungetiftemp(current_asmdata.CurrAsmList,right.location.reference);
      end
    else hreg := right.location.register;

  { load the current string length }
  lengthreg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
  cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_8,OS_INT,left.location.reference,lengthreg);

  { do we have to check the length ? }
  if tg.istemp(left.location.reference) then
    checklength := curmaxlen = 255
  else
    checklength := curmaxlen >= tstringdef(left.resultdef).len;
  if checklength then
    begin
      { is it already maximal? }
      current_asmdata.getjumplabel(l);
      if tg.istemp(left.location.reference) then
        len:=255
      else
        len:=tstringdef(left.resultdef).len;
      cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_EQ,len,lengthreg,l)
    end;

  { no, so increase the length and add the new character }
  href2 := left.location.reference;

  { we need a new reference to store the character }
  { at the end of the string. Check if the base or }
  { index register is still free                   }
  if (href2.base <> NR_NO) and
     (href2.index <> NR_NO) then
    begin
      { they're not free, so add the base reg to       }
      { the string length (since the index can         }
      { have a scalefactor) and use lengthreg as base  }
      cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_INT,href2.base,lengthreg);
      href2.base := lengthreg;
    end
  else
    { at least one is still free, so put EDI there }
    if href2.base = NR_NO then
      href2.base := lengthreg
    else
      begin
        href2.index := lengthreg;
{$ifdef x86}
        href2.scalefactor := 1;
{$endif x86}
      end;
  { we need to be one position after the last char }
  inc(href2.offset);
  { store the character at the end of the string }
  if (right.nodetype <> ordconstn) then
    begin
      { no new_reference(href2) because it's only }
      { used once (JM)                            }
      cg.a_load_reg_ref(current_asmdata.CurrAsmList,OS_8,OS_8,hreg,href2);
    end
  else
    cg.a_load_const_ref(current_asmdata.CurrAsmList,OS_8,tordconstnode(right).value,href2);
  lengthreg:=cg.makeregsize(current_asmdata.CurrAsmList,lengthreg,OS_8);
  { increase the string length }
  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_ADD,OS_8,1,lengthreg);
  cg.a_load_reg_ref(current_asmdata.CurrAsmList,OS_8,OS_8,lengthreg,left.location.reference);
  if checklength then
    cg.a_label(current_asmdata.CurrAsmList,l);
  location_copy(location,left.location);
end;

begin
  caddsstringcharoptnode := tcgaddsstringcharoptnode;
end.

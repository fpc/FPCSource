{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe

    This unit implements the 80x86 implementation of optimized nodes

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
unit n386opt;

{$i defines.inc}

interface
uses node, nopt;

type
  ti386addsstringcharoptnode = class(taddsstringcharoptnode)
     function det_resulttype: tnode; override;
     function pass_1: tnode; override;
     procedure pass_2; override;
  end;

  ti386addsstringcsstringoptnode = class(taddsstringcsstringoptnode)
     { must be duplicated from ti386addnode :( }
     procedure pass_2; override;
  end;

implementation

uses pass_1, types, htypechk, cgbase, temp_gen, cpubase, cga,
     tgcpu, aasm, ncnv, ncon, pass_2, symdef;


{*****************************************************************************
                             TI386ADDOPTNODE
*****************************************************************************}

function ti386addsstringcharoptnode.det_resulttype: tnode;
begin
  det_resulttype := nil;
  resulttypepass(left);
  resulttypepass(right);
  if codegenerror then
    exit;
  { update the curmaxlen field (before converting to a string!) }
  updatecurmaxlen;
  if not is_shortstring(left.resulttype.def) then
    inserttypeconv(left,cshortstringtype);
  resulttype:=left.resulttype;
end;


function ti386addsstringcharoptnode.pass_1: tnode;
begin
  pass_1 := nil;
  firstpass(left);
  firstpass(right);
  if codegenerror then
    exit;
  location.loc := LOC_MEM;
  if not is_constcharnode(right) then
    { it's not sure we need the register, but we can't know it here yet }
    calcregisters(self,2,0,0)
  else
    calcregisters(self,1,0,0);
end;


procedure ti386addsstringcharoptnode.pass_2;
var
  l: tasmlabel;
  href2: preference;
  href:  treference;
  hreg, lengthreg: tregister;
  checklength: boolean;
begin
  { first, we have to more or less replicate some code from }
  { ti386addnode.pass_2                                     }
  secondpass(left);
  if not(istemp(left.location.reference) and
         (getsizeoftemp(left.location.reference) = 256)) and
     not(nf_use_strconcat in flags) then
    begin
       gettempofsizereference(256,href);
       copyshortstring(href,left.location.reference,255,false,true);
       { release the registers }
       ungetiftemp(left.location.reference);
       { does not hurt: }
       clear_location(left.location);
       left.location.loc:=LOC_MEM;
       left.location.reference:=href;
    end;
  secondpass(right);
  { special case for string := string + char (JM) }
  hreg := R_NO;

  { we have to load the char before checking the length, because we }
  { may need registers from the reference                           }

  { is it a constant char? }
  if not is_constcharnode(right) then
    { no, make sure it is in a register }
    if right.location.loc in [LOC_REFERENCE,LOC_MEM] then
      begin
        { free the registers of right }
        del_reference(right.location.reference);
        { get register for the char }
        hreg := reg32toreg8(getregisterint);
        emit_ref_reg(A_MOV,S_B,
          newreference(right.location.reference),hreg);
       { I don't think a temp char exists, but it won't hurt (JM) }
       ungetiftemp(right.location.reference);
      end
    else hreg := right.location.register;

  { load the current string length }
  lengthreg := getregisterint;
  emit_ref_reg(A_MOVZX,S_BL,newreference(left.location.reference),lengthreg);

  { do we have to check the length ? }
  if istemp(left.location.reference) then
    checklength := curmaxlen = 255
  else
    checklength := curmaxlen >= tstringdef(left.resulttype.def).len;
  if checklength then
    begin
      { is it already maximal? }
      getlabel(l);
      if istemp(left.location.reference) then
        emit_const_reg(A_CMP,S_L,255,lengthreg)
      else
        emit_const_reg(A_CMP,S_L,tstringdef(left.resulttype.def).len,lengthreg);
      emitjmp(C_E,l);
    end;

  { no, so increase the length and add the new character }
  href2 := newreference(left.location.reference);

  { we need a new reference to store the character }
  { at the end of the string. Check if the base or }
  { index register is still free                   }
  if (href2^.base <> R_NO) and
     (href2^.index <> R_NO) then
    begin
      { they're not free, so add the base reg to       }
      { the string length (since the index can         }
      { have a scalefactor) and use lengthreg as base  }
      emit_reg_reg(A_ADD,S_L,href2^.base,lengthreg);
      href2^.base := lengthreg;
    end
  else
    { at least one is still free, so put EDI there }
    if href2^.base = R_NO then
      href2^.base := lengthreg
    else
      begin
        href2^.index := lengthreg;
        href2^.scalefactor := 1;
      end;
  { we need to be one position after the last char }
  inc(href2^.offset);
  { store the character at the end of the string }
  if (right.nodetype <> ordconstn) then
    begin
      { no new_reference(href2) because it's only }
      { used once (JM)                            }
      emit_reg_ref(A_MOV,S_B,hreg,href2);
      ungetregister(hreg);
    end
  else
    emit_const_ref(A_MOV,S_B,tordconstnode(right).value,href2);
  { increase the string length }
  emit_reg(A_INC,S_B,reg32toreg8(lengthreg));
  emit_reg_ref(A_MOV,S_B,reg32toreg8(lengthreg),
                 newreference(left.location.reference));
  ungetregister32(lengthreg);
  if checklength then
    emitlab(l);
  set_location(location,left.location);
end;

procedure ti386addsstringcsstringoptnode.pass_2;
var
  href: treference;
  pushedregs: tpushed;
  regstopush: byte;
begin
  { first, we have to more or less replicate some code from }
  { ti386addnode.pass_2                                     }
  secondpass(left);
  if not(istemp(left.location.reference) and
         (getsizeoftemp(left.location.reference) = 256)) and
     not(nf_use_strconcat in flags) then
    begin
       gettempofsizereference(256,href);
       copyshortstring(href,left.location.reference,255,false,true);
       { release the registers }
       ungetiftemp(left.location.reference);
       { does not hurt: }
       clear_location(left.location);
       left.location.loc:=LOC_MEM;
       left.location.reference:=href;
    end;
  secondpass(right);
  { on the right we do not need the register anymore too }
  { Instead of releasing them already, simply do not }
  { push them (so the release is in the right place, }
  { because emitpushreferenceaddr doesn't need extra }
  { registers) (JM)                                  }
  regstopush := $ff;
  remove_non_regvars_from_loc(right.location,
    regstopush);
  pushusedregisters(pushedregs,regstopush);
  { push the maximum possible length of the result }
  emitpushreferenceaddr(left.location.reference);
  { the optimizer can more easily put the          }
  { deallocations in the right place if it happens }
  { too early than when it happens too late (if    }
  { the pushref needs a "lea (..),edi; push edi")  }
  del_reference(right.location.reference);
  emitpushreferenceaddr(right.location.reference);
  saveregvars(regstopush);
  emitcall('FPC_SHORTSTR_CONCAT');
  ungetiftemp(right.location.reference);
  maybe_loadself;
  popusedregisters(pushedregs);
  set_location(location,left.location);
end;

begin
  caddsstringcharoptnode := ti386addsstringcharoptnode;
  caddsstringcsstringoptnode := ti386addsstringcsstringoptnode
end.

{
  $Log$
  Revision 1.6  2001-12-31 09:53:15  jonas
    * changed remaining "getregister32" calls to "getregisterint"

  Revision 1.5  2001/08/26 13:37:00  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.4  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.3  2001/04/02 21:20:38  peter
    * resulttype rewrite

  Revision 1.2  2001/01/06 19:12:31  jonas
    * fixed IE 10 (but code is less efficient now :( )

  Revision 1.1  2001/01/04 11:24:19  jonas
    + initial implementation (still needs to be made more modular)

}

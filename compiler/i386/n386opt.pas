{
    $Id$
    Copyright (c) 1998-2002 by Jonas Maebe

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

{$i fpcdefs.inc}

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

uses
  pass_1,defutil,htypechk,
  symdef,paramgr,
  aasmbase,aasmtai,aasmcpu,
  ncnv, ncon, pass_2,
  cginfo, cgbase, cpubase,
  tgobj, rgobj, cgobj, ncgutil;


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
  location.loc := LOC_CREFERENCE;
  if not is_constcharnode(right) then
    { it's not sure we need the register, but we can't know it here yet }
    calcregisters(self,2,0,0)
  else
    calcregisters(self,1,0,0);
end;


procedure ti386addsstringcharoptnode.pass_2;
var
  l: tasmlabel;
  href,href2 :  treference;
  hreg, lengthreg: tregister;
  checklength: boolean;
  len : integer;
begin
  { first, we have to more or less replicate some code from }
  { ti386addnode.pass_2                                     }
  secondpass(left);
  if not(tg.istemp(left.location.reference) and
         (tg.sizeoftemp(exprasmlist,left.location.reference) = 256)) and
     not(nf_use_strconcat in flags) then
    begin
       tg.Gettemp(exprasmlist,256,tt_normal,href);
       cg.g_copyshortstring(exprasmlist,left.location.reference,href,255,true,false);
       { location is released by copyshortstring }
       location_freetemp(exprasmlist,left.location);
       { return temp reference }
       location_reset(left.location,LOC_CREFERENCE,def_cgsize(resulttype.def));
       left.location.reference:=href;
    end;
  secondpass(right);
  { special case for string := string + char (JM) }
  hreg.enum := R_NO;

  { we have to load the char before checking the length, because we }
  { may need registers from the reference                           }

  { is it a constant char? }
  if not is_constcharnode(right) then
    { no, make sure it is in a register }
    if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
      begin
        { free the registers of right }
        reference_release(exprasmlist,right.location.reference);
        { get register for the char }
        hreg := rg.makeregsize(rg.getregisterint(exprasmlist),OS_8);
        cg.a_load_ref_reg(exprasmlist,OS_8,right.location.reference,hreg);
        { I don't think a temp char exists, but it won't hurt (JM) }
        tg.ungetiftemp(exprasmlist,right.location.reference);
      end
    else hreg := right.location.register;

  { load the current string length }
  lengthreg := rg.getregisterint(exprasmlist);
  cg.a_load_ref_reg(exprasmlist,OS_8,left.location.reference,lengthreg);

  { do we have to check the length ? }
  if tg.istemp(left.location.reference) then
    checklength := curmaxlen = 255
  else
    checklength := curmaxlen >= tstringdef(left.resulttype.def).len;
  if checklength then
    begin
      { is it already maximal? }
      objectlibrary.getlabel(l);
      if tg.istemp(left.location.reference) then
        len:=255
      else
        len:=tstringdef(left.resulttype.def).len;
      cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_EQ,len,lengthreg,l)
    end;

  { no, so increase the length and add the new character }
  href2 := left.location.reference;

  { we need a new reference to store the character }
  { at the end of the string. Check if the base or }
  { index register is still free                   }
  if (href2.base.enum <> R_NO) and
     (href2.index.enum <> R_NO) then
    begin
      { they're not free, so add the base reg to       }
      { the string length (since the index can         }
      { have a scalefactor) and use lengthreg as base  }
      cg.a_op_reg_reg(exprasmlist,OP_ADD,OS_INT,href2.base,lengthreg);
      href2.base := lengthreg;
    end
  else
    { at least one is still free, so put EDI there }
    if href2.base.enum = R_NO then
      href2.base := lengthreg
    else
      begin
        href2.index := lengthreg;
        href2.scalefactor := 1;
      end;
  { we need to be one position after the last char }
  inc(href2.offset);
  { store the character at the end of the string }
  if (right.nodetype <> ordconstn) then
    begin
      { no new_reference(href2) because it's only }
      { used once (JM)                            }
      cg.a_load_reg_ref(exprasmlist,OS_8,hreg,href2);
      rg.ungetregister(exprasmlist,hreg);
    end
  else
    cg.a_load_const_ref(exprasmlist,OS_8,tordconstnode(right).value,href2);
  { increase the string length }
  cg.a_op_const_reg(exprasmlist,OP_ADD,1,rg.makeregsize(lengthreg,OS_8));
  cg.a_load_reg_ref(exprasmlist,OS_8,rg.makeregsize(lengthreg,OS_8),left.location.reference);
  rg.ungetregisterint(exprasmlist,lengthreg);
  if checklength then
    cg.a_label(exprasmlist,l);
  location_copy(location,left.location);
end;

procedure ti386addsstringcsstringoptnode.pass_2;
var
  href: treference;
  pushedregs: tpushedsaved;
  regstopush: tregisterset;
begin
  { first, we have to more or less replicate some code from }
  { ti386addnode.pass_2                                     }
  secondpass(left);
  if not(tg.istemp(left.location.reference) and
         (tg.sizeoftemp(exprasmlist,left.location.reference) = 256)) and
     not(nf_use_strconcat in flags) then
    begin
       tg.GetTemp(exprasmlist,256,tt_normal,href);
       cg.g_copyshortstring(exprasmlist,left.location.reference,href,255,true,false);
       { release the registers }
       location_freetemp(exprasmlist,left.location);
       { return temp reference }
       location_reset(left.location,LOC_CREFERENCE,def_cgsize(resulttype.def));
       left.location.reference:=href;
    end;
  secondpass(right);
  { on the right we do not need the register anymore too }
  { Instead of releasing them already, simply do not }
  { push them (so the release is in the right place, }
  { because emitpushreferenceaddr doesn't need extra }
  { registers) (JM)                                  }
  regstopush := all_registers;
  remove_non_regvars_from_loc(right.location,regstopush);
  rg.saveusedregisters(exprasmlist,pushedregs,regstopush);
  { push the maximum possible length of the result }
  cg.a_paramaddr_ref(exprasmlist,left.location.reference,paramanager.getintparaloc(2));
  { the optimizer can more easily put the          }
  { deallocations in the right place if it happens }
  { too early than when it happens too late (if    }
  { the pushref needs a "lea (..),edi; push edi")  }
  reference_release(exprasmlist,right.location.reference);
  cg.a_paramaddr_ref(exprasmlist,right.location.reference,paramanager.getintparaloc(1));
  rg.saveregvars(exprasmlist,regstopush);
  cg.a_call_name(exprasmlist,'FPC_SHORTSTR_CONCAT');
  tg.ungetiftemp(exprasmlist,right.location.reference);
  cg.g_maybe_loadself(exprasmlist);
  rg.restoreusedregisters(exprasmlist,pushedregs);
  location_copy(location,left.location);
end;

begin
  caddsstringcharoptnode := ti386addsstringcharoptnode;
  caddsstringcsstringoptnode := ti386addsstringcsstringoptnode
end.

{
  $Log$
  Revision 1.27  2003-01-08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.26  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.25  2002/11/15 01:58:57  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.24  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.23  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.22  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.21  2002/07/20 11:58:04  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.20  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.19  2002/07/07 09:52:34  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.18  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.17  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.16  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.14  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.13  2002/05/12 16:53:17  peter
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

  Revision 1.12  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.11  2002/04/21 15:36:40  carl
  * changeregsize -> rg.makeregsize

  Revision 1.10  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.9  2002/04/04 19:06:12  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.8  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.7  2002/03/31 20:26:39  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}

{
    $Id$
    Copyright (c) 1998-2002 by Kovacs Attila Zoltan

    Generate i386 assembly wrapper code interface implementor objects

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
unit n386obj;

{$i fpcdefs.inc}

interface


implementation

uses
  systems,
  verbose,globals,globtype,
  aasmbase,aasmtai,
  symconst,symtype,symdef,symsym,
  fmodule,
  nobj,
  cpubase,cginfo,
  cga,tgobj,rgobj,cgobj;

   type
     ti386classheader=class(tclassheader)
     protected
       procedure cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);override;
     end;

{
possible calling conventions:
              default stdcall cdecl pascal register saveregisters
default(0):      OK     OK    OK(1)  OK       OK          OK
virtual(2):      OK     OK    OK(3)  OK       OK          OK(4)

(0):
    set self parameter to correct value
    jmp mangledname

(1): The code is the following
     set self parameter to correct value
     call mangledname
     set self parameter to interface value

(2): The wrapper code use %eax to reach the virtual method address
     set self to correct value
     move self,%eax
     mov  0(%eax),%eax ; load vmt
     jmp  vmtoffs(%eax) ; method offs

(3): The wrapper code use %eax to reach the virtual method address
     set self to correct value
     move self,%eax
     mov  0(%eax),%eax ; load vmt
     jmp  vmtoffs(%eax) ; method offs
     set self parameter to interface value


(4): Virtual use eax to reach the method address so the following code be generated:
     set self to correct value
     push %ebx ; allocate space for function address
     push %eax
     mov  self,%eax
     mov  0(%eax),%eax ; load vmt
     mov  vmtoffs(%eax),eax ; method offs
     mov  %eax,4(%esp)
     pop  %eax
     ret  0; jmp the address

}

function getselfoffsetfromsp(procdef: tprocdef): longint;
begin
  { framepointer is pushed for nested procs }
  if procdef.parast.symtablelevel>normal_function_level then
    getselfoffsetfromsp:=4
  else
    getselfoffsetfromsp:=0;
end;


procedure ti386classheader.cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);

  procedure getselftoeax(offs: longint);
  var
    href : treference;
  begin
    { mov offset(%esp),%eax }
    reference_reset_base(href,NR_ESP,getselfoffsetfromsp(procdef)+offs);
    cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,NR_EAX);
  end;

  procedure loadvmttoeax;
  var
    href : treference;
  begin
    { mov  0(%eax),%eax ; load vmt}
    reference_reset_base(href,NR_EAX,0);
    cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,NR_EAX);
  end;

  procedure op_oneaxmethodaddr(op: TAsmOp);
  var
    href : treference;
  begin
    if (procdef.extnumber=-1) then
      Internalerror(200006139);
    { call/jmp  vmtoffs(%eax) ; method offs }
    reference_reset_base(href,NR_EAX,procdef._class.vmtmethodoffset(procdef.extnumber));
    emit_ref(op,S_L,href);
  end;

  procedure loadmethodoffstoeax;
  var
    href : treference;
  begin
    if (procdef.extnumber=-1) then
      Internalerror(200006139);
    { mov vmtoffs(%eax),%eax ; method offs }
    reference_reset_base(href,NR_EAX,procdef._class.vmtmethodoffset(procdef.extnumber));
    cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,NR_EAX);
  end;

var
  oldexprasmlist: TAAsmoutput;
  lab : tasmsymbol;
  make_global : boolean;
  href : treference;
begin
  if procdef.proctypeoption<>potype_none then
    Internalerror(200006137);
  if not assigned(procdef._class) or
     (procdef.procoptions*[po_classmethod, po_staticmethod,
       po_methodpointer, po_interrupt, po_iocheck]<>[]) then
    Internalerror(200006138);
  if procdef.owner.symtabletype<>objectsymtable then
    Internalerror(200109191);

  oldexprasmlist:=exprasmlist;
  exprasmlist:=asmlist;

  make_global:=false;
  if (not current_module.is_unit) or
     (cs_create_smart in aktmoduleswitches) or
     (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
    make_global:=true;

  if make_global then
   exprasmList.concat(Tai_symbol.Createname_global(labelname,0))
  else
   exprasmList.concat(Tai_symbol.Createname(labelname,0));

  { set param1 interface to self  }
  adjustselfvalue(procdef,ioffset);

  { case 1 or 2 }
  if (procdef.proccalloption in clearstack_pocalls) then
    begin
      if po_virtualmethod in procdef.procoptions then
        begin { case 2 }
          getselftoeax(0);
          loadvmttoeax;
          op_oneaxmethodaddr(A_CALL);
        end
      else { case 1 }
        cg.a_call_name(exprasmlist,procdef.mangledname);
      { restore param1 value self to interface }
      adjustselfvalue(procdef,-ioffset);
    end
  { case 3 }
  else if [po_virtualmethod,po_saveregisters]*procdef.procoptions=[po_virtualmethod,po_saveregisters] then
    begin
      emit_reg(A_PUSH,S_L,NR_EBX); { allocate space for address}
      emit_reg(A_PUSH,S_L,NR_EAX);
      getselftoeax(8);
      loadvmttoeax;
      loadmethodoffstoeax;
      { mov %eax,4(%esp) }
      reference_reset_base(href,NR_ESP,4);
      emit_reg_ref(A_MOV,S_L,NR_EAX,href);
      { pop  %eax }
      emit_reg(A_POP,S_L,NR_EAX);
      { ret  ; jump to the address }
      emit_none(A_RET,S_L);
    end
  { case 4 }
  else if po_virtualmethod in procdef.procoptions then
    begin
      getselftoeax(0);
      loadvmttoeax;
      op_oneaxmethodaddr(A_JMP);
    end
  { case 0 }
  else
    begin
      lab:=objectlibrary.newasmsymbol(procdef.mangledname);
      emit_sym(A_JMP,S_NO,lab);
    end;
  exprasmlist:=oldexprasmlist;
end;


initialization
  cclassheader:=ti386classheader;
end.
{
  $Log$
  Revision 1.24  2003-09-25 14:59:06  peter
    * fix intf wrapper code

  Revision 1.23  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.22  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.21  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.20.2.1  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.20  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.19  2003/05/15 18:58:54  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.18  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.17  2003/01/13 14:54:34  daniel
    * Further work to convert codegenerator register convention;
      internalerror bug fixed.

  Revision 1.16  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.15  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.14  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.13  2002/08/09 07:33:04  florian
    * a couple of interface related fixes

  Revision 1.12  2002/07/16 15:34:21  florian
    * exit is now a syssym instead of a keyword

  Revision 1.11  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.10  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.9  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.7  2002/05/12 16:53:17  peter
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

  Revision 1.6  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.5  2002/03/31 20:26:39  jonas
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

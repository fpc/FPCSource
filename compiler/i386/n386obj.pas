{
    $Id$
    Copyright (c) 1998-2000 by Kovacs Attila Zoltan

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

{$i defines.inc }

interface


implementation

uses
  systems,
  verbose,globals,globtype,
  aasm,
  symconst,symtype,symdef,symsym,
  fmodule,
  nobj,
  cpubase,
  cga, tgobj;

   type
     ti386classheader=class(tclassheader)
     protected
       procedure cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);override;
     end;

{
possible calling conventions:
              default stdcall cdecl pascal popstack register saveregisters
default(0):      OK     OK    OK(1)  OK     OK(1)      OK          OK
virtual(2):      OK     OK    OK(3)  OK     OK(3)      OK          OK(4)

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
  if not assigned(procdef.parast.symindex.first) then
    getselfoffsetfromsp:=4
  else
    if tsym(procdef.parast.symindex.first).typ=varsym then
      getselfoffsetfromsp:=tvarsym(procdef.parast.symindex.first).address+4
    else
      Internalerror(2000061310);
end;


procedure ti386classheader.cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);

  procedure checkvirtual;
  begin
    if (procdef.extnumber=-1) then
      Internalerror(200006139);
  end;

  procedure adjustselfvalue(ioffset: longint);
  begin
    { sub $ioffset,offset(%esp) }
    emit_const_ref(A_SUB,S_L,ioffset,new_reference(R_ESP,getselfoffsetfromsp(procdef)));
  end;

  procedure getselftoeax(offs: longint);
  begin
    { mov offset(%esp),%eax }
    emit_ref_reg(A_MOV,S_L,new_reference(R_ESP,getselfoffsetfromsp(procdef)),R_EAX);
  end;

  procedure loadvmttoeax;
  begin
    checkvirtual;
    { mov  0(%eax),%eax ; load vmt}
    emit_ref_reg(A_MOV,S_L,new_reference(R_EAX,0),R_EAX);
  end;

  procedure op_oneaxmethodaddr(op: TAsmOp);
  begin
    { call/jmp  vmtoffs(%eax) ; method offs }
    emit_ref(op,S_L,new_reference(R_EAX,procdef._class.vmtmethodoffset(procdef.extnumber)));
  end;

  procedure loadmethodoffstoeax;
  begin
    { mov  vmtoffs(%eax),%eax ; method offs }
    emit_ref_reg(A_MOV,S_L,new_reference(R_EAX,procdef._class.vmtmethodoffset(procdef.extnumber)),R_EAX);
  end;

var
  oldexprasmlist: TAAsmoutput;
  lab : tasmsymbol;
  make_global : boolean;
begin
  if procdef.proctypeoption<>potype_none then
    Internalerror(200006137);
  if not assigned(procdef._class) or
     (procdef.procoptions*[po_containsself, po_classmethod, po_staticmethod,
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
  adjustselfvalue(ioffset);

  { case 1  or 2 }
  if (po_clearstack in procdef.procoptions) then
    begin
      if po_virtualmethod in procdef.procoptions then
        begin { case 2 }
          getselftoeax(0);
          loadvmttoeax;
          op_oneaxmethodaddr(A_CALL);
        end
      else { case 1 }
        begin
          emitcall(procdef.mangledname);
        end;
      { restore param1 value self to interface }
      adjustselfvalue(-ioffset);
    end
  { case 3 }
  else if [po_virtualmethod,po_saveregisters]*procdef.procoptions=[po_virtualmethod,po_saveregisters] then
    begin
      emit_reg(A_PUSH,S_L,R_EBX); { allocate space for address}
      emit_reg(A_PUSH,S_L,R_EAX);
      getselftoeax(8);
      loadvmttoeax;
      loadmethodoffstoeax;
      { mov %eax,4(%esp) }
      emit_reg_ref(A_MOV,S_L,R_EAX,new_reference(R_ESP,4));
      { pop  %eax }
      emit_reg(A_POP,S_L,R_EAX);
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
      lab:=newasmsymbol(procdef.mangledname);
      emit_sym(A_JMP,S_NO,lab);
    end;
  exprasmlist:=oldexprasmlist;
end;


initialization
  cclassheader:=ti386classheader;
end.
{
  $Log$
  Revision 1.5  2002-03-31 20:26:39  jonas
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

  Revision 1.4  2001/10/25 21:22:41  peter
    * calling convention rewrite

  Revision 1.3  2001/09/19 11:04:41  michael
  * Smartlinking with interfaces fixed
  * Better smartlinking for rtti and init tables

  Revision 1.2  2001/08/26 13:37:00  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.1  2001/04/21 13:37:17  peter
    * made tclassheader using class of to implement cpu dependent code

  Revision 1.5  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.4  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.3  2000/11/29 00:30:47  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.2  2000/11/12 23:24:15  florian
    * interfaces are basically running

  Revision 1.1  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.1.2.2  2000/06/15 15:05:30  kaz
    * An minor bug fix

  Revision 1.1.2.1  2000/06/15 06:26:34  kaz
    * Initial version
}

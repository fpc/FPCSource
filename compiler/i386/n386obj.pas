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
  symconst,symdef,
  fmodule,
  nobj,
  cpuinfo,cpubase,
  cga,cgutils,cgobj;

   type
     ti386classheader=class(tclassheader)
     protected
       procedure cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);override;
     end;

{
possible calling conventions:
              default stdcall cdecl pascal register
default(0):      OK     OK    OK(1)  OK       OK
virtual(2):      OK     OK    OK(3)  OK       OK

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


(4): Virtual use values pushed on stack to reach the method address
     so the following code be generated:
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
    getselfoffsetfromsp:=2*sizeof(aint)
  else
    getselfoffsetfromsp:=sizeof(aint);
end;


procedure ti386classheader.cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);

  procedure getselftoeax(offs: longint);
  var
    href : treference;
  begin
    { mov offset(%esp),%eax }
    if (procdef.proccalloption<>pocall_register) then
      begin
        reference_reset_base(href,NR_ESP,getselfoffsetfromsp(procdef)+offs);
        cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,NR_EAX);
      end;
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
    if (procdef.extnumber=$ffff) then
      Internalerror(200006139);
    { call/jmp  vmtoffs(%eax) ; method offs }
    reference_reset_base(href,NR_EAX,procdef._class.vmtmethodoffset(procdef.extnumber));
    emit_ref(op,S_L,href);
  end;

  procedure loadmethodoffstoeax;
  var
    href : treference;
  begin
    if (procdef.extnumber=$ffff) then
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
     (af_smartlink_sections in target_asm.flags) or
     (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
    make_global:=true;

  if make_global then
   exprasmList.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
  else
   exprasmList.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

  { set param1 interface to self  }
  adjustselfvalue(procdef,ioffset);

  { case 1 or 2 }
  if (procdef.proccalloption in clearstack_pocalls) then
    begin
      if po_virtualmethod in procdef.procoptions then
        begin
          { case 2 }
          getselftoeax(0);
          loadvmttoeax;
          op_oneaxmethodaddr(A_CALL);
        end
      else
        begin
          { case 1 }
          cg.a_call_name(exprasmlist,procdef.mangledname);
        end;
      { restore param1 value self to interface }
      adjustselfvalue(procdef,-ioffset);
    end
  else if po_virtualmethod in procdef.procoptions then
    begin
      if (procdef.proccalloption=pocall_register) then
        begin
          { case 4 }
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
      else
        begin
          { case 3 }
          getselftoeax(0);
          loadvmttoeax;
          op_oneaxmethodaddr(A_JMP);
        end;
    end
  { case 0 }
  else
    begin
      lab:=objectlibrary.newasmsymbol(procdef.mangledname,AB_EXTERNAL,AT_FUNCTION);
      emit_sym(A_JMP,S_NO,lab);
    end;

  exprasmList.concat(Tai_symbol_end.Createname(labelname));

  exprasmlist:=oldexprasmlist;
end;


initialization
  cclassheader:=ti386classheader;
end.
{
  $Log$
  Revision 1.35  2004-10-24 20:01:08  peter
    * remove saveregister calling convention

  Revision 1.34  2004/06/20 08:55:31  florian
    * logs truncated

  Revision 1.33  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.32.2.2  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.32.2.1  2004/04/08 18:33:22  peter
    * rewrite of TAsmSection

  Revision 1.32  2004/03/02 00:36:33  olle
    * big transformation of Tai_[const_]Symbol.Create[data]name*

  Revision 1.31  2004/02/27 13:42:52  olle
    + added Tai_symbol_end

}

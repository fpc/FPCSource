{
    $Id$
    Copyright (c) 1998-2002 by Kovacs Attila Zoltan

    Generate arm assembly wrapper code interface implementor objects

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
unit narmobj;

{$i fpcdefs.inc}

interface


implementation

uses
  systems,
  verbose,globals,globtype,
  aasmbase,aasmtai,aasmcpu,
  symconst,symdef,
  fmodule,
  nobj,
  cpuinfo,cpubase,
  cgutils,cgobj;

   type
     tarmclassheader=class(tclassheader)
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

procedure tarmclassheader.cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);

  procedure loadvmttor12;
  var
    href : treference;
  begin
    reference_reset_base(href,NR_R0,0);
    cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,NR_R12);
  end;

  procedure op_onr12methodaddr;
  var
    href : treference;
  begin
    if (procdef.extnumber=$ffff) then
      Internalerror(200006139);
    { call/jmp  vmtoffs(%eax) ; method offs }
    reference_reset_base(href,NR_R12,procdef._class.vmtmethodoffset(procdef.extnumber));
    cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,NR_R12);
    { using a_call here causes an internalerror because pi_do_call
      isn't set properly }
    exprasmlist.concat(taicpu.op_reg_reg(A_MOV,NR_R14,NR_PC));
    exprasmlist.concat(taicpu.op_reg_reg(A_MOV,NR_PC,NR_R12));
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
   exprasmList.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
  else
   exprasmList.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

  { set param1 interface to self  }
  adjustselfvalue(procdef,ioffset);

  { case 4 }
  if po_virtualmethod in procdef.procoptions then
    begin
      loadvmttor12;
      op_onr12methodaddr;
    end
  { case 0 }
  else
    asmlist.concat(taicpu.op_sym(A_B,objectlibrary.newasmsymbol(procdef.mangledname,AB_EXTERNAL,AT_FUNCTION)));

  exprasmList.concat(Tai_symbol_end.Createname(labelname));

  exprasmlist:=oldexprasmlist;
end;


initialization
  cclassheader:=tarmclassheader;
end.
{
  $Log$
  Revision 1.1  2004-03-21 22:40:15  florian
    + added interface support for the arm
    * added  FPC_REQUIRES_PROPER_ALIGNMENT define for targets which require proper alignment
}

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
unit nx64obj;

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
     tx8664classheader=class(tclassheader)
     protected
       procedure cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);override;
     end;


procedure tx8664classheader.cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);
var
  oldexprasmlist: TAAsmoutput;
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

  make_global:=false;
  if (not current_module.is_unit) or
     (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
    make_global:=true;

  oldexprasmlist:=exprasmlist;
  exprasmlist:=asmlist;

  if make_global then
    exprasmList.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
  else
    exprasmList.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

  { set param1 interface to self  }
  adjustselfvalue(procdef,ioffset);

  if po_virtualmethod in procdef.procoptions then
    begin
      if (procdef.extnumber=$ffff) then
        Internalerror(200006139);
      { mov  0(%rdi),%rax ; load vmt}
      reference_reset_base(href,NR_RDI,0);
      cg.a_load_ref_reg(asmlist,OS_ADDR,OS_ADDR,href,NR_RAX);
      { jmp *vmtoffs(%eax) ; method offs }
      reference_reset_base(href,NR_RAX,procdef._class.vmtmethodoffset(procdef.extnumber));
      asmlist.concat(taicpu.op_ref_reg(A_MOV,S_Q,href,NR_RAX));
      asmlist.concat(taicpu.op_reg(A_JMP,S_Q,NR_RAX));
    end
  else
    asmlist.concat(taicpu.op_sym(A_JMP,S_NO,objectlibrary.newasmsymbol(procdef.mangledname,AB_EXTERNAL,AT_FUNCTION)));

  exprasmList.concat(Tai_symbol_end.Createname(labelname));

  exprasmlist:=oldexprasmlist;
end;


initialization
  cclassheader:=tx8664classheader;
end.
{
  $Log$
  Revision 1.2  2004-06-16 20:07:11  florian
    * dwarf branch merged

  Revision 1.1.2.3  2004/05/10 21:28:35  peter
    * section_smartlink enabled for gas under linux

  Revision 1.1.2.2  2004/04/29 21:54:29  florian
    * interface wrappers fixed

  Revision 1.1.2.1  2004/04/22 21:14:34  peter
    * nx64obj added, untested
}

{
    $Id$
    Copyright (c) 1998-2002 by Kovacs Attila Zoltan

    Generate powerpc assembly wrapper code interface implementor objects

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
unit nppcobj;

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
     tppcclassheader=class(tclassheader)
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

procedure tppcclassheader.cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);

  procedure loadvmttor11;
  var
    href : treference;
  begin
    reference_reset_base(href,NR_R3,0);
    cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,NR_R11);
  end;

  procedure op_onr11methodaddr;
  var
    href : treference;
  begin
    if (procdef.extnumber=$ffff) then
      Internalerror(200006139);
    { call/jmp  vmtoffs(%eax) ; method offs }
    reference_reset_base(href,NR_R11,procdef._class.vmtmethodoffset(procdef.extnumber));
    if not((longint(href.offset) >= low(smallint)) and
           (longint(href.offset) <= high(smallint))) then
      begin
        asmlist.concat(taicpu.op_reg_reg_const(A_ADDIS,NR_R11,NR_R11,
          smallint((href.offset shr 16)+ord(smallint(href.offset and $ffff) < 0))));
        href.offset := smallint(href.offset and $ffff);
      end;
    asmlist.concat(taicpu.op_reg_ref(A_LWZ,NR_R11,href));
    asmlist.concat(taicpu.op_reg(A_MTCTR,NR_R11));
    asmlist.concat(taicpu.op_none(A_BCTR));
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
      loadvmttor11;
      op_onr11methodaddr;
    end
  { case 0 }
  else
    asmlist.concat(taicpu.op_sym(A_B,objectlibrary.newasmsymbol(procdef.mangledname,AB_EXTERNAL,AT_FUNCTION)));

  exprasmList.concat(Tai_symbol_end.Createname(labelname));

  exprasmlist:=oldexprasmlist;
end;


initialization
  cclassheader:=tppcclassheader;
end.
{
  $Log$
  Revision 1.6  2004-03-02 00:36:33  olle
    * big transformation of Tai_[const_]Symbol.Create[data]name*

  Revision 1.5  2004/02/27 13:42:56  olle
    + added Tai_symbol_end

  Revision 1.4  2004/02/27 10:21:05  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

  Revision 1.3  2003/12/28 15:14:14  jonas
    * hopefully fixed interfaces (untested)

  Revision 1.2  2003/12/23 23:12:44  peter
    * extnumber failure is $ffff instead of -1
    * fix non-vmt call for register calling on i386

  Revision 1.1  2003/12/10 01:10:47  florian
    + initial interface support added
}

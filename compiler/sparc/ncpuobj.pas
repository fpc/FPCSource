{
    $Id$
    Copyright (c) 1998-2004 by Kovacs Attila Zoltan and Florian Klaempfl

    Generate sparc assembly wrapper code interface implementor objects

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
unit ncpuobj;

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
      tsparcclassheader=class(tclassheader)
      protected
        procedure cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);override;
      end;


    procedure tsparcclassheader.cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);
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
            reference_reset_base(href,NR_O0,0);
            cg.a_load_ref_reg(asmlist,OS_ADDR,OS_ADDR,href,NR_L0);
            { jmp *vmtoffs(%eax) ; method offs }
            reference_reset_base(href,NR_L0,procdef._class.vmtmethodoffset(procdef.extnumber));
            asmlist.concat(taicpu.op_ref_reg(A_LD,href,NR_L1));
            asmlist.concat(taicpu.op_reg(A_JMP,NR_L1));
          end
        else
          asmlist.concat(taicpu.op_sym(A_BA,objectlibrary.newasmsymbol(procdef.mangledname,AB_EXTERNAL,AT_FUNCTION)));
        { Delay slot }
        asmlist.Concat(TAiCpu.Op_none(A_NOP));

        exprasmList.concat(Tai_symbol_end.Createname(labelname));

        exprasmlist:=oldexprasmlist;
      end;


initialization
  cclassheader:=tsparcclassheader;
end.
{
  $Log$
  Revision 1.2  2004-06-16 20:07:11  florian
    * dwarf branch merged

  Revision 1.1.2.4  2004/05/14 16:17:25  florian
    * the interface wrappers are called before save, so they must use o0 for self

  Revision 1.1.2.3  2004/05/13 20:58:47  florian
    * fixed register addressed jumps in interface wrappers

  Revision 1.1.2.2  2004/05/13 20:10:38  florian
    * released variant and interface support

  Revision 1.1.2.1  2004/05/13 19:41:10  florian
    + ncpuobj added
}

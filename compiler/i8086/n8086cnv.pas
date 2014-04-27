{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate code for i8086 assembler for type converting nodes

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
unit n8086cnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncgcnv,nx86cnv,defutil,defcmp;

    type

       { t8086typeconvnode }

       t8086typeconvnode = class(tx86typeconvnode)
       protected
         function typecheck_proc_to_procvar: tnode;override;
         procedure second_proc_to_procvar;override;
         procedure second_nil_to_methodprocvar;override;
       end;


implementation

   uses
      verbose,systems,globals,globtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symconst,symdef,symcpu,
      cgbase,cga,procinfo,pass_1,pass_2,
      ncon,ncal,ncnv,
      cpubase,cpuinfo,
      cgutils,cgobj,hlcgobj,cgx86,ncgutil,
      tgobj;

    function t8086typeconvnode.typecheck_proc_to_procvar: tnode;
      begin
        if (current_settings.x86memorymodel in x86_far_code_models) and
          not is_proc_far(tabstractprocdef(left.resultdef)) then
          CGMessage1(type_e_procedure_must_be_far,left.resultdef.GetTypeName);
        Result:=inherited typecheck_proc_to_procvar;
      end;


    procedure t8086typeconvnode.second_proc_to_procvar;
      begin
        if is_proc_far(tabstractprocdef(resultdef))<>
           (current_settings.x86memorymodel in x86_far_code_models) then
          internalerror(2014041302);
        if is_proc_far(tabstractprocdef(left.resultdef))<>
           (current_settings.x86memorymodel in x86_far_code_models) then
          internalerror(2014041303);
        inherited;
      end;


    procedure t8086typeconvnode.second_nil_to_methodprocvar;
      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        if current_settings.x86memorymodel in x86_far_data_models then
          begin
            location.registerhi:=cg.getintregister(current_asmdata.currasmlist,OS_32);
            cg.a_load_const_reg(current_asmdata.currasmlist,OS_32,0,location.registerhi);
          end
        else
          begin
            location.registerhi:=cg.getaddressregister(current_asmdata.currasmlist);
            cg.a_load_const_reg(current_asmdata.currasmlist,OS_ADDR,0,location.registerhi);
          end;
        if (resultdef.typ=procvardef) and is_proc_far(tprocvardef(resultdef)) then
          begin
            location.register:=cg.getintregister(current_asmdata.currasmlist,OS_32);
            cg.a_load_const_reg(current_asmdata.currasmlist,OS_32,0,location.register);
          end
        else
          begin
            location.register:=cg.getaddressregister(current_asmdata.currasmlist);
            cg.a_load_const_reg(current_asmdata.currasmlist,OS_ADDR,0,location.register);
          end;
      end;


begin
  ctypeconvnode:=t8086typeconvnode
end.

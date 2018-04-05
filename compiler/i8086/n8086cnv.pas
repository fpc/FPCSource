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
         function typecheck_int_to_int: tnode;override;
         function typecheck_proc_to_procvar: tnode;override;
         procedure second_proc_to_procvar;override;
       end;


implementation

   uses
      verbose,systems,globals,globtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symconst,symdef,symcpu,
      cgbase,cga,procinfo,pass_1,pass_2,
      ncon,ncal,ncnv,nmem,n8086mem,
      cpubase,cpuinfo,
      cgutils,cgobj,hlcgobj,cgx86,ncgutil,
      tgobj;

    function t8086typeconvnode.typecheck_int_to_int: tnode;
      begin
        Result:=inherited typecheck_int_to_int;
        if (is_16bitint(totypedef) or is_8bitint(totypedef)) and (left.nodetype=addrn) then
          begin
            if left.nodetype=addrn then
              ti8086addrnode(left).get_offset_only:=true;
          end;
      end;


    function t8086typeconvnode.typecheck_proc_to_procvar: tnode;
      begin
        Result:=inherited typecheck_proc_to_procvar;
        if tcnf_proc_2_procvar_get_offset_only in convnodeflags then
          begin
            if resultdef.typ<>procvardef then
              internalerror(2018040401);
            exclude(tprocvardef(resultdef).procoptions,po_far);
          end
        else if (tcnf_proc_2_procvar_2_voidpointer in convnodeflags) and
                (current_settings.x86memorymodel in x86_far_code_models) then
          begin
            if resultdef.typ<>procvardef then
              internalerror(2018040402);
            include(tprocvardef(resultdef).procoptions,po_far);
          end;
      end;


    procedure t8086typeconvnode.second_proc_to_procvar;
      begin
        if (tcnf_proc_2_procvar_get_offset_only in convnodeflags) and
            is_proc_far(tabstractprocdef(resultdef)) then
          internalerror(2018040403);
        inherited;
      end;


begin
  ctypeconvnode:=t8086typeconvnode
end.

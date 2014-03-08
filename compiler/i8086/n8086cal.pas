{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i8086 assembler for in call nodes

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
unit n8086cal;

{$i fpcdefs.inc}

interface

{ $define AnsiStrRef}

    uses
      nx86cal,cgutils;

    type
       ti8086callnode = class(tx86callnode)
       protected
          procedure pop_parasize(pop_size:longint);override;
          procedure extra_interrupt_code;override;
          procedure extra_call_ref_code(var ref: treference);override;
          procedure do_call_ref(ref: treference);override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      cgbase,
      cpubase,paramgr,
      aasmtai,aasmdata,aasmcpu,
      ncal,nbas,nmem,nld,ncnv,
      cga,cgobj,cgx86,cpuinfo;


{*****************************************************************************
                             TI8086CALLNODE
*****************************************************************************}


    procedure ti8086callnode.extra_interrupt_code;
      begin
        emit_none(A_PUSHF,S_W);
        emit_reg(A_PUSH,S_W,NR_CS);
      end;


    procedure ti8086callnode.pop_parasize(pop_size:longint);
      var
        hreg : tregister;
      begin
        if (paramanager.use_fixed_stack) then
          begin
            { very weird: in this case the callee does a "ret $4" and the }
            { caller immediately a "subl $4,%esp". Possibly this is for   }
            { use_fixed_stack code to be able to transparently call       }
            { old-style code (JM)                                         }
            dec(pop_size,pushedparasize);
            if (pop_size < 0) then
              current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_SUB,S_W,-pop_size,NR_SP));
            exit;
          end;

        { better than an add on all processors }
        if pop_size=2 then
          begin
            hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_POP,S_W,hreg));
          end
        { the pentium has two pipes and pop reg is pairable }
        { but the registers must be different!        }
        else
          if pop_size<>0 then
            current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_ADD,S_W,pop_size,NR_SP));
      end;


    procedure ti8086callnode.extra_call_ref_code(var ref: treference);
      begin
        if ref.base<>NR_NO then
          begin
            cg.getcpuregister(current_asmdata.CurrAsmList,NR_BX);
            cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,ref.base,NR_BX);
            ref.base:=NR_BX;
            cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_BX);
          end;
        if ref.index<>NR_NO then
          begin
            cg.getcpuregister(current_asmdata.CurrAsmList,NR_SI);
            cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,ref.index,NR_SI);
            ref.index:=NR_SI;
            cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_SI);
          end;
      end;


    procedure ti8086callnode.do_call_ref(ref: treference);
      begin
        if current_settings.x86memorymodel in x86_far_code_models then
          current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_CALL,S_FAR,ref))
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_CALL,S_NO,ref));
      end;


begin
   ccallnode:=ti8086callnode;
end.

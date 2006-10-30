{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for in call nodes

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
unit n386cal;

{$i fpcdefs.inc}

interface

{ $define AnsiStrRef}

    uses
      ncgcal;

    type
       ti386callnode = class(tcgcallnode)
       protected
          procedure pop_parasize(pop_size:longint);override;
          procedure extra_interrupt_code;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      cgbase,cgutils,
      cpubase,paramgr,
      aasmtai,aasmdata,aasmcpu,
      ncal,nbas,nmem,nld,ncnv,
      cga,cgobj,cpuinfo;


{*****************************************************************************
                             TI386CALLNODE
*****************************************************************************}


    procedure ti386callnode.extra_interrupt_code;
      begin
        if (target_info.system <> system_i386_darwin) then
          begin
            emit_none(A_PUSHF,S_L);
            emit_reg(A_PUSH,S_L,NR_CS);
          end;
      end;


    procedure ti386callnode.pop_parasize(pop_size:longint);
      var
        hreg : tregister;
      begin
        if (use_fixed_stack) then
          begin
            { very weird: in this case the callee does a "ret $4" and the }
            { caller immediately a "subl $4,%esp". Possibly this is for   }
            { use_fixed_stack code to be able to transparently call       }
            { old-style code (JM)                                         }
            dec(pop_size,pushedparasize);
            if (pop_size < 0) then
              current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_SUB,S_L,-pop_size,NR_ESP));
            exit;
          end;

        { better than an add on all processors }
        if pop_size=4 then
          begin
            hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_POP,S_L,hreg));
          end
        { the pentium has two pipes and pop reg is pairable }
        { but the registers must be different!        }
        else
          if (pop_size=8) and
             not(cs_opt_size in current_settings.optimizerswitches) and
             (current_settings.optimizecputype=cpu_Pentium) then
            begin
               hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
               current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_POP,S_L,hreg));
               hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
               current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_POP,S_L,hreg));
            end
        else
          if pop_size<>0 then
            current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_ADD,S_L,pop_size,NR_ESP));
      end;


begin
   ccallnode:=ti386callnode;
end.

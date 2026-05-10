{
    Copyright (c) 2002 by Florian Klaempfl

    Implements the x86-64 specific part of call nodes

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
unit nx64cal;

{$i fpcdefs.inc}

interface

    uses
      symdef,
      node,ncal,nx86cal;

    type
       tx8664callnode = class(tx86callnode)
       protected
         procedure gen_syscall_para(para: tcallparanode); override;
         procedure extra_call_code(ctx:tpassgeneratecodecontext);override;
         procedure set_result_location(realresdef: tstoreddef;ctx:tpassgeneratecodecontext);override;
       public
         procedure do_syscall(ctx:tpassgeneratecodecontext);override;
       end;


implementation

    uses
      globtype,
      systemstypes,systems,verbose,cutils,
      cpubase,cgbase,cgutils,cgobj,compiler,
      symconst,symcpu,nld,
      aasmtai,aasmdata,aasmcpu,
      cpupi,
      pass_2_context,nodehelper;

    procedure tx8664callnode.do_syscall(ctx:tpassgeneratecodecontext);
      var
        tmpref: treference;
      begin
        case compiler.target.info.system of
          system_x86_64_aros:
            begin
              if ([po_syscall_baselast,po_syscall_basereg] * tprocdef(procdefinition).procoptions) <> [] then
                begin
                  ctx.CurrAsmList.concat(tai_comment.create(strpnew('AROS SysCall')));

                  ctx.cg.getcpuregister(ctx.CurrAsmList,NR_R12);
                  get_syscall_call_ref(tmpref,NR_R12,ctx);

                  ctx.CurrAsmList.concat(taicpu.op_ref(A_CALL,S_NO,tmpref));
                  ctx.cg.ungetcpuregister(ctx.CurrAsmList,NR_R12);
                  exit;
                end;
              internalerror(2016120101);
            end;
          else
            internalerror(2015062801);
        end;
      end;


    procedure tx8664callnode.gen_syscall_para(para: tcallparanode);
      begin
        { lib parameter has no special type but proccalloptions must be a syscall }
        para.left:=compiler.cloadnode(tcpuprocdef(procdefinition).libsym,tcpuprocdef(procdefinition).libsym.owner);
      end;


    procedure tx8664callnode.extra_call_code(ctx:tpassgeneratecodecontext);
      var
        mmregs : aint;
      begin
        { x86_64 requires %al to contain the no. SSE regs passed }
        if (cnf_uses_varargs in callnodeflags) and not x86_64_use_ms_abi(procdefinition.proccalloption,compiler.target) then
          begin
            if assigned(varargsparas) then
              mmregs:=varargsparas.mmregsused
            else
              mmregs:=0;
            ctx.CurrAsmList.concat(taicpu.op_const_reg(A_MOV,S_Q,mmregs,NR_RAX))
          end;
      end;


    procedure tx8664callnode.set_result_location(realresdef: tstoreddef;ctx:tpassgeneratecodecontext);
      begin
        { avoid useless "movq %xmm0,%rax" and "movq %rax,%xmm0" instructions
          (which moreover for some reason are not supported by the Darwin
           x86-64 assembler) }
        if assigned(retloc.location) and
           not assigned(retloc.location^.next) and
           (retloc.location^.loc in [LOC_MMREGISTER,LOC_CMMREGISTER]) then
          begin
            location_reset(location,LOC_MMREGISTER,retloc.location^.size);
            location.register:=ctx.cg.getmmregister(ctx.CurrAsmList,retloc.location^.size);
          end
        else
          inherited
      end;

begin
   ccallnode:=tx8664callnode;
end.

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
      ncal,nx86cal;

    type
       tx8664callnode = class(tx86callnode)
       protected
         procedure gen_syscall_para(para: tcallparanode); override;
         procedure extra_call_code;override;
         procedure set_result_location(realresdef: tstoreddef);override;
       public
         procedure do_syscall;override;
       end;


implementation

    uses
      globtype,
      systems,verbose,cutils,
      cpubase,cgbase,cgutils,cgobj,
      symsym,symcpu,nld,
      aasmbase,aasmtai,aasmdata,aasmcpu;

{    uses
      globtype,systems,
      cutils,verbose,globals,
      cgbase,cgutils,
      cpubase,paramgr,
      aasmtai,aasmdata,aasmcpu,
      nbas,nmem,nld,ncnv,
      symdef,symsym,symcpu,
      cga,cgobj,cpuinfo;}


    procedure tx8664callnode.do_syscall;
      var
        tmpref: treference;
      begin
        case target_info.system of
          system_x86_64_aros:
            begin
              if ([po_syscall_baselast,po_syscall_basereg] * tprocdef(procdefinition).procoptions) <> [] then
                begin
                  current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('AROS SysCall')));

                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_RAX);
                  get_syscall_call_ref(tmpref,NR_RAX);

                  current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_CALL,S_NO,tmpref));
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_RAX);
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
        para.left:=cloadnode.create(tcpuprocdef(procdefinition).libsym,tcpuprocdef(procdefinition).libsym.owner);
      end;


    procedure tx8664callnode.extra_call_code;
      var
        mmregs : aint;
      begin
        { x86_64 requires %al to contain the no. SSE regs passed }
        if (cnf_uses_varargs in callnodeflags) and (target_info.system<>system_x86_64_win64) then
          begin
            if assigned(varargsparas) then
              mmregs:=varargsparas.mmregsused
            else
              mmregs:=0;
            current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_MOV,S_Q,mmregs,NR_RAX))
          end;
      end;


    procedure tx8664callnode.set_result_location(realresdef: tstoreddef);
      begin
        { avoid useless "movq %xmm0,%rax" and "movq %rax,%xmm0" instructions
          (which moreover for some reason are not supported by the Darwin
           x86-64 assembler) }
        if assigned(retloc.location) and
           not assigned(retloc.location^.next) and
           (retloc.location^.loc in [LOC_MMREGISTER,LOC_CMMREGISTER]) then
          begin
            location_reset(location,LOC_MMREGISTER,retloc.location^.size);
            location.register:=cg.getmmregister(current_asmdata.CurrAsmList,retloc.location^.size);
          end
        else
          inherited
      end;

begin
   ccallnode:=tx8664callnode;
end.

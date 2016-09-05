{
    Copyright (c) 2002 by Florian Klaempfl

    Implements the PowerPC specific part of call nodes

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
unit nppccal;

{$i fpcdefs.inc}

interface

    uses
      symdef,node,ncal,ncgcal;

    type
       tppccallnode = class(tcgcallnode)
        protected
         procedure gen_syscall_para(para: tcallparanode); override;
        public
         procedure extra_call_code;override;
         procedure do_syscall;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symtype,symbase,symsym,symcpu,symtable,defutil,paramgr,parabase,
      cgbase,pass_2,
      cpuinfo,cpubase,aasmbase,aasmtai,aasmdata,aasmcpu,
      nmem,nld,ncnv,
      ncgutil,cgutils,cgobj,tgobj,regvars,rgobj,rgcpu,
      cg64f32,cgcpu,cpupi,procinfo;

    procedure tppccallnode.gen_syscall_para(para: tcallparanode);
      begin
        { lib parameter has no special type but proccalloptions must be a syscall }
        para.left:=cloadnode.create(tcpuprocdef(procdefinition).libsym,tcpuprocdef(procdefinition).libsym.owner);
      end;


    procedure tppccallnode.extra_call_code;
      begin
        if assigned(varargsparas) then
          begin
            if (target_info.abi = abi_powerpc_sysv) then
              begin
                if va_uses_float_reg in varargsparas.varargsinfo then
                  current_asmdata.CurrAsmList.concat(taicpu.op_const_const_const(A_CREQV,6,6,6))
                else
                  current_asmdata.CurrAsmList.concat(taicpu.op_const_const_const(A_CRXOR,6,6,6));
              end;
          end;
      end;

    procedure tppccallnode.do_syscall;

      procedure do_call_ref(constref ref: treference);
        begin
          cg.getcpuregister(current_asmdata.CurrAsmList,NR_R0);
          cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,ref,NR_R0);
          cg.a_call_reg(current_asmdata.CurrAsmList,NR_R0);
          cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_R0);
        end;

      var
        tmpref: treference;
        libparaloc: pcgparalocation;
        hsym: tsym;
      begin
        case target_info.system of
          system_powerpc_amiga:
            begin
              { one syscall convention for AmigaOS/PowerPC
                which is very similar to basesysv on MorphOS }
              reference_reset_base(tmpref,NR_R3,tprocdef(procdefinition).extnumber,sizeof(pint));
              do_call_ref(tmpref);
            end;
          system_powerpc_morphos:
            begin
              { all conventions but legacy }
              if ([po_syscall_basesysv,po_syscall_sysv,
                   po_syscall_sysvbase,po_syscall_r12base] * tprocdef(procdefinition).procoptions) <> [] then
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_R12);

                  hsym:=tsym(procdefinition.parast.Find('syscalllib'));
                  if not assigned(hsym) then
                    internalerror(2016090501);
                  libparaloc:=tparavarsym(hsym).paraloc[callerside].location;
                  if not assigned(libparaloc) then
                    internalerror(2016090502);

                  case libparaloc^.loc of
                    LOC_REGISTER:
                      reference_reset_base(tmpref,libparaloc^.register,-tprocdef(procdefinition).extnumber,sizeof(pint));
                    LOC_REFERENCE:
                      begin
                        { this can happen for sysvbase; if we run out of regs, the libbase will be passed on the stack }
                        reference_reset_base(tmpref,libparaloc^.reference.index,libparaloc^.reference.offset,sizeof(pint));
                        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,tmpref,NR_R12);
                        reference_reset_base(tmpref,NR_R12,-tprocdef(procdefinition).extnumber,sizeof(pint));
                      end;
                    else
                      internalerror(2016090202);
                  end;

                  do_call_ref(tmpref);
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_R12);
                end
              else if po_syscall_legacy in tprocdef(procdefinition).procoptions then
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_R3);

                  { R3 must contain the call offset }
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI,NR_R3,-tprocdef(procdefinition).extnumber));
                  reference_reset_base(tmpref,NR_R2,100,4); { 100 ($64) is EmulDirectCallOS offset }

                  do_call_ref(tmpref);
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_R3);
                end
              else
                internalerror(2005010403);
            end;
          else
            internalerror(2004042901);
        end;
      end;


begin
   ccallnode:=tppccallnode;
end.

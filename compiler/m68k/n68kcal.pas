{
    Copyright (c) 2002 by Florian Klaempfl

    Implements the M68K specific part of call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
unit n68kcal;

{$i fpcdefs.inc}

interface

    uses
      symdef,node,ncal,ncgcal;

    type
       tm68kcallnode = class(tcgcallnode)
         procedure do_syscall;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defutil,paramgr,parabase,
      cgbase,pass_2,
      cpuinfo,cpubase,aasmbase,aasmtai,aasmdata,aasmcpu,
      nmem,nld,ncnv,
      ncgutil,cgutils,cgobj,tgobj,regvars,rgobj,rgcpu,
      cg64f32,cgcpu,cpupi,procinfo;


    procedure tm68kcallnode.do_syscall;
      var
        tmpref: treference;
	tmpref2: treference;
      begin
        case target_info.system of
          system_m68k_amiga:
            begin
              if po_syscall_legacy in tprocdef(procdefinition).procoptions then
                begin
		  { save base pointer on syscalls }
		  { FIXME: probably this will need to be extended to save all regs (KB) }
                  reference_reset_base(tmpref2, NR_STACK_POINTER_REG, 0, 4);
                  tmpref2.direction := dir_dec;
		  current_asmdata.CurrAsmList.concat(taicpu.op_reg_ref(A_MOVE,S_L,NR_FRAME_POINTER_REG,tmpref2));
		  
		  { the actuall call }
                  reference_reset_base(tmpref,NR_A6,-tprocdef(procdefinition).extnumber,4);
                  current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_JSR,S_NO,tmpref));
		  
		  { restore frame pointer }
                  reference_reset_base(tmpref2, NR_STACK_POINTER_REG, 0, 4);
                  tmpref2.direction := dir_inc;
		  current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_MOVE,S_L,tmpref2,NR_FRAME_POINTER_REG));
                end
              else
                internalerror(2005010403);
            end;
          else
            internalerror(2004042901);
        end;
      end;


begin
   ccallnode:=tm68kcallnode;
end.

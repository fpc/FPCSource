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
      node,
      parabase,
      nx86cal,cgutils;

    type
       ti8086callnode = class(tx86callnode)
       protected
          procedure pop_parasize(pop_size:longint);override;
          procedure extra_interrupt_code;override;
          function do_call_ref(ref: treference): tcgpara;override;
          function can_call_ref(var ref: treference): boolean; override;
        public
          function pass_typecheck: tnode; override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      htypechk,
      cgbase,
      cpubase,paramgr,
      aasmtai,aasmdata,aasmcpu,
      ncal,nbas,nmem,nld,ncnv,
      hlcgobj,
      symcpu,
      cga,cgobj,cgx86,cpuinfo;


{*****************************************************************************
                             TI8086CALLNODE
*****************************************************************************}


    procedure ti8086callnode.extra_interrupt_code;
      begin
        emit_none(A_PUSHF,S_W);
        if current_settings.x86memorymodel in x86_near_code_models then
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


    function ti8086callnode.do_call_ref(ref: treference): tcgpara;
      begin
        if is_proc_far(procdefinition) then
          current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_CALL,S_FAR,ref))
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_CALL,S_NO,ref));
        result:=hlcg.get_call_result_cgpara(procdefinition,typedef)
      end;


    function ti8086callnode.can_call_ref(var ref: treference): boolean;
      var
        reg,seg: tregister;
      begin
        tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,ref);
        if (procdefinition.proccalloption=pocall_register) and
           (getsupreg(ref.base)>=first_int_imreg) and
           (getsupreg(ref.index)>=first_int_imreg) then
          begin
            { Precalculate the ref address in case both ref base and index are
              imm registers and the register calling convention is used.
              Otherwise register allocation will fail if BX is used for a parameter. }
            reg:=cg.getaddressregister(current_asmdata.CurrAsmList);
            current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_LEA,S_W,ref,reg));
            seg:=ref.segment;
            reference_reset_base(ref,reg,0,ref.temppos,ref.alignment,[]);
            ref.segment:=seg;
          end;
        result:=true;
      end;


    function ti8086callnode.pass_typecheck: tnode;
      begin
        Result:=inherited pass_typecheck;
        { If calling a procvar allow the procvar address to be in a register only for
          the register calling convention and near code memory models.
          In other cases there are no available registers to perform the call.
          Additionally there is no CALL reg1:reg2 instruction. }
        if (right<>nil) then
          if is_proc_far(procdefinition) or
             (procdefinition.proccalloption<>pocall_register) then
            make_not_regable(right,[]);
      end;

begin
   ccallnode:=ti8086callnode;
end.

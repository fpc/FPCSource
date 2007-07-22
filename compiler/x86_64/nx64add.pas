{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the x86-64

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
unit nx64add;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,cpubase,nx86add;

    type
       tx8664addnode = class(tx86addnode)
          procedure second_mul;override;
       end;

  implementation

    uses
      globtype,globals,
      aasmbase,aasmtai,aasmdata,
      cgbase,cgutils,cga,cgobj,
      tgobj;

{*****************************************************************************
                                MUL
*****************************************************************************}

    procedure tx8664addnode.second_mul;

    var reg:Tregister;
        ref:Treference;
        use_ref:boolean;
        hl4 : tasmlabel;

    begin
      pass_left_right;

      { The location.register will be filled in later (JM) }
      location_reset(location,LOC_REGISTER,OS_INT);
      { Mul supports registers and references, so if not register/reference,
        load the location into a register}
      use_ref:=false;
      if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
        reg:=left.location.register
      else if left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
        begin
          ref:=left.location.reference;
          use_ref:=true;
        end
      else
        begin
          {LOC_CONSTANT for example.}
          reg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
          cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_INT,left.location,reg);
        end;
      { Allocate RAX. }
      cg.getcpuregister(current_asmdata.CurrAsmList,NR_RAX);
      { Load the right value. }
      cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_INT,right.location,NR_RAX);
      { Also allocate RDX, since it is also modified by a mul (JM). }
      cg.getcpuregister(current_asmdata.CurrAsmList,NR_RDX);
      if use_ref then
        emit_ref(A_MUL,S_Q,ref)
      else
        emit_reg(A_MUL,S_Q,reg);
      if cs_check_overflow in current_settings.localswitches  then
       begin
         current_asmdata.getjumplabel(hl4);
         cg.a_jmp_flags(current_asmdata.CurrAsmList,F_AE,hl4);
         cg.a_call_name(current_asmdata.CurrAsmList,'FPC_OVERFLOW');
         cg.a_label(current_asmdata.CurrAsmList,hl4);
       end;
      { Free RDX,RAX }
      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_RDX);
      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_RAX);
      { Allocate a new register and store the result in RAX in it. }
      location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
      emit_reg_reg(A_MOV,S_Q,NR_RAX,location.register);
      location_freetemp(current_asmdata.CurrAsmList,left.location);
      location_freetemp(current_asmdata.CurrAsmList,right.location);
    end;


begin
   caddnode:=tx8664addnode;
end.

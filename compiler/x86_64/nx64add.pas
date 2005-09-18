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
      aasmbase,aasmtai,
      cgbase,cgutils,cga,cgobj,
      tgobj;

{*****************************************************************************
                                MUL
*****************************************************************************}

    procedure tx8664addnode.second_mul;

    var r:Tregister;
        hl4 : tasmlabel;

    begin
      { The location.register will be filled in later (JM) }
      location_reset(location,LOC_REGISTER,OS_INT);
      { Get a temp register and load the left value into it
        and free the location. }
      r:=cg.getintregister(exprasmlist,OS_INT);
      cg.a_load_loc_reg(exprasmlist,OS_INT,left.location,r);
      { Allocate RAX. }
      cg.getcpuregister(exprasmlist,NR_RAX);
      { Load the right value. }
      cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,NR_RAX);
      { Also allocate RDX, since it is also modified by a mul (JM). }
      cg.getcpuregister(exprasmlist,NR_RDX);
      emit_reg(A_MUL,S_Q,r);
      if cs_check_overflow in aktlocalswitches  then
       begin
         objectlibrary.getjumplabel(hl4);
         cg.a_jmp_flags(exprasmlist,F_AE,hl4);
         cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
         cg.a_label(exprasmlist,hl4);
       end;
      { Free RDX,RAX }
      cg.ungetcpuregister(exprasmlist,NR_RDX);
      cg.ungetcpuregister(exprasmlist,NR_RAX);
      { Allocate a new register and store the result in RAX in it. }
      location.register:=cg.getintregister(exprasmlist,OS_INT);
      emit_reg_reg(A_MOV,S_Q,NR_RAX,location.register);
      location_freetemp(exprasmlist,left.location);
      location_freetemp(exprasmlist,right.location);
    end;


begin
   caddnode:=tx8664addnode;
end.

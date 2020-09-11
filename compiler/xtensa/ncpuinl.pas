{
    Copyright (c) 1998-2017 by Florian Klaempfl

    Generates Xtensa inline nodes

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
unit ncpuinl;

{$i fpcdefs.inc}

  interface

    uses
      node,ninl,ncginl, aasmbase;

    type
      tcpuinlineNode = class(tcginlinenode)
        function first_abs_real: tnode; override;
        procedure second_abs_long; override;
        procedure second_abs_real; override;
      end;

  implementation

    uses
      cpuinfo,
      verbose,globals,
      compinnr,
      aasmdata,
      aasmcpu,
      symdef,
      defutil,
      hlcgobj,
      pass_2,
      cgbase, cgobj, cgutils,
      cpubase;

    procedure tcpuinlinenode.second_abs_long;
      begin
        secondpass(left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

        location:=left.location;
        location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,left.resultdef);

        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_ABS,location.register,left.location.register));
      end;


    function tcpuinlinenode.first_abs_real : tnode;
      begin
        result:=nil;
        if is_single(left.resultdef) and (FPUXTENSA_SINGLE in fpu_capabilities[current_settings.fputype]) then
          expectloc:=LOC_FPUREGISTER
        else
          result:=inherited first_abs_real;
      end;


    procedure tcpuinlinenode.second_abs_real;
      begin
        if not(is_single(resultdef)) then
          InternalError(2020091101);
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_FPUREGISTER,OS_F32);
        location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
        current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_ABS,location.register,left.location.register),PF_S));
      end;



begin
  cinlinenode:=tcpuinlinenode;
end.

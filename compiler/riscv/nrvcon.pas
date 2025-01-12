{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Code generation for const nodes on the Risc-V

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
unit nrvcon;

{$i fpcdefs.inc}

interface

    uses
      node,ncgcon,cpubase;

    type
      trvrealconstnode = class(tcgrealconstnode)
        function pass_1 : tnode;override;

        procedure pass_generate_code;override;
      end;


implementation

    uses
      verbose,
      globals,
      aasmcpu,aasmdata,
      defutil,
      cpuinfo,
      cgbase,cgobj,cgutils,
      ncon;

{*****************************************************************************
                           TARMREALCONSTNODE
*****************************************************************************}

    function trvrealconstnode.pass_1 : tnode;
      begin
        result:=nil;
        if is_number_float(value_real) and (value_real=0.0) and (get_real_sign(value_real)=1) and
          (
            ((CPURV_HAS_F in cpu_capabilities[current_settings.cputype]) and is_single(resultdef))
{$ifdef RISCV64}
            or ((CPURV_HAS_D in cpu_capabilities[current_settings.cputype]) and is_double(resultdef))
{$endif RISCV64}
          ) then
           expectloc:=LOC_FPUREGISTER
         else
           expectloc:=LOC_CREFERENCE;
      end;


    procedure trvrealconstnode.pass_generate_code;
      begin
        if is_number_float(value_real) and (value_real=0.0) and (get_real_sign(value_real)=1) and
          (
            ((CPURV_HAS_F in cpu_capabilities[current_settings.cputype]) and is_single(resultdef))
{$ifdef RISCV64}
            or ((CPURV_HAS_D in cpu_capabilities[current_settings.cputype]) and is_double(resultdef))
{$endif RISCV64}
          ) then
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
            location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
            if is_single(resultdef) then
              current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg(A_FMV_W_X,location.register,NR_X0))
{$ifdef RISCV64}
            else if is_double(resultdef) then
              current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg(A_FMV_D_X,location.register,NR_X0))
{$endif RISCV64}
            else
              Internalerror(2025011103);
          end
        else
          inherited pass_generate_code;
      end;

begin
  crealconstnode:=trvrealconstnode;
end.

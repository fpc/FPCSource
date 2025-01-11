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
      globtype,globals,
      cpuinfo,
      aasmbase,aasmtai,aasmcpu,aasmdata,
      symdef,
      defutil,
      cgbase,cgobj,cgutils,
      procinfo,
      ncon;

{*****************************************************************************
                           TARMREALCONSTNODE
*****************************************************************************}

    function trvrealconstnode.pass_1 : tnode;
      begin
        result:=nil;
        if is_number_float(value_real) and (value_real=0.0) and (get_real_sign(value_real)=1) and
          (
            is_single(resultdef)
{$ifdef RISCV64}
            or is_double(resultdef)
{$endif RISCV64}
          ) then
           expectloc:=LOC_FPUREGISTER
         else
           expectloc:=LOC_CREFERENCE;
      end;


    procedure trvrealconstnode.pass_generate_code;
      { I suppose the parser/pass_1 must make sure the generated real  }
      { constants are actually supported by the target processor? (JM) }
      const
        floattype2ait:array[tfloattype] of tairealconsttype=
          (aitrealconst_s32bit,aitrealconst_s64bit,aitrealconst_s80bit,aitrealconst_s80bit,aitrealconst_s64comp,aitrealconst_s64comp,aitrealconst_s128bit);
      var
         lastlabel : tasmlabel;
         realait : tairealconsttype;

      begin
        if is_number_float(value_real) and (value_real=0.0) and (get_real_sign(value_real)=1) and
          (
            is_single(resultdef)
{$ifdef RISCV64}
            or is_double(resultdef)
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

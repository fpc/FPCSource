{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate RiscV assembler for math nodes

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
unit nrvmat;

{$I fpcdefs.inc}

  interface

    uses
      ncgmat;

    type
      trvunaryminusnode = class(tcgunaryminusnode)
        procedure second_float;override;
      end;

implementation

    uses
      globtype,
      verbose,
      nmat,
      cpubase,
      aasmdata,aasmcpu,
      cgbase,cgobj,cgcpu,cgutils,
      symdef,
      hlcgobj,
      defutil,
      pass_2;


    procedure trvunaryminusnode.second_float;
      begin
        secondpass(left);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        case tfloatdef(left.resultdef).floattype of
          s32real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEG_S,location.register,left.location.register));
          s64real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEG_D,location.register,left.location.register));
          s128real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEG_Q,location.register,left.location.register));
          else
            Internalerror(2025010901);
        end;
        cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
      end;


begin
  cunaryminusnode := trvunaryminusnode;
end.


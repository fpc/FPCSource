{
    Copyright (c) 2005 by Florian Klaempfl

    Code generation for const nodes on the AArch64

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
unit ncpucon;

{$i fpcdefs.inc}

interface

    uses
      node,ncgcon,cpubase;

    type
      taarch64realconstnode = class(tcgrealconstnode)
        function pass_1 : tnode;override;
        procedure pass_generate_code;override;
      end;

  implementation

    uses
      verbose,
      globtype,globals,
      cpuinfo,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symdef,
      defutil,
      cgbase,cgutils,cgobj,
      procinfo,
      ncon;

{*****************************************************************************
                           TARMREALCONSTNODE
*****************************************************************************}

    function taarch64realconstnode.pass_1 : tnode;
      begin
        result:=nil;
        if IsFloatImmediate(tfloatdef(resultdef).floattype,value_real) then
           expectloc:=LOC_MMREGISTER
         else
           result:=Inherited pass_1;
      end;


    procedure taarch64realconstnode.pass_generate_code;
      var
        hreg : TRegister;
      begin
        if IsFloatImmediate(tfloatdef(resultdef).floattype,value_real) then
          begin
            location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
            location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_realconst(A_FMOV,
              location.register,value_real));
          end
        { cast and compare the bit pattern as we cannot handle -0.0 }
        else if bestrealrec(value_real).Data=0 then
          begin
            location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
            location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
            hreg:=newreg(R_MMREGISTER,getsupreg(location.register),R_SUBMM16B);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_EOR,
              hreg,hreg,hreg));
          end
        else
          Inherited pass_generate_code;
      end;

begin
  crealconstnode:=taarch64realconstnode;
end.

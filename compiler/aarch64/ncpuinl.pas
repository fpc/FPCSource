{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generates ARM inline nodes

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
      node,ninl,ncginl;

    type
      taarch64inlinenode = class(tcgInlineNode)
        function first_abs_real: tnode; override;
        function first_sqr_real: tnode; override;
        function first_sqrt_real: tnode; override;
        function first_round_real: tnode; override;
        function first_trunc_real: tnode; override;
        procedure second_abs_real; override;
        procedure second_sqr_real; override;
        procedure second_sqrt_real; override;
        procedure second_abs_long; override;
        procedure second_round_real; override;
        procedure second_trunc_real; override;
      private
        procedure load_fpu_location;
      end;


implementation

    uses
      globtype,verbose,globals,
      cpuinfo, defutil,symdef,aasmdata,aasmcpu,
      cgbase,cgutils,pass_1,pass_2,
      cpubase,ncgutil,cgobj,cgcpu, hlcgobj;

{*****************************************************************************
                              taarch64inlinenode
*****************************************************************************}

    procedure taarch64inlinenode.load_fpu_location;
      begin
        secondpass(left);
        hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_copy(location,left.location);
        location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
        location.loc:=LOC_MMREGISTER;
      end;


    function taarch64inlinenode.first_abs_real : tnode;
      begin
        expectloc:=LOC_MMREGISTER;
        result:=nil;
      end;


    function taarch64inlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_MMREGISTER;
        result:=nil;
      end;


    function taarch64inlinenode.first_sqrt_real : tnode;
      begin
        expectloc:=LOC_MMREGISTER;
        result:=nil;
      end;


    function taarch64inlinenode.first_round_real: tnode;
      begin
        expectloc:=LOC_MMREGISTER;
        result:=nil;
      end;


    function taarch64inlinenode.first_trunc_real: tnode;
      begin
        expectloc:=LOC_MMREGISTER;
        result:=nil;
      end;


    procedure taarch64inlinenode.second_abs_real;
      begin
        load_fpu_location;
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FABS,location.register,left.location.register));
      end;


    procedure taarch64inlinenode.second_sqr_real;
      begin
        load_fpu_location;
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_FMUL,location.register,left.location.register,left.location.register));
      end;


    procedure taarch64inlinenode.second_sqrt_real;
      begin
        load_fpu_location;
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRT,location.register,left.location.register));
      end;


    procedure taarch64inlinenode.second_abs_long;
      var
        opsize : tcgsize;
        hp : taicpu;
      begin
        secondpass(left);
        opsize:=def_cgsize(left.resultdef);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
        location:=left.location;
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,opsize);

        current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_NEG,location.register,left.location.register),PF_S));
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_cond(A_CSEL,location.register,location.register,left.location.register,C_GE));
      end;


    procedure taarch64inlinenode.second_round_real;
      var
        hreg: tregister;
      begin
        secondpass(left);
        hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
        hreg:=cg.getmmregister(current_asmdata.CurrAsmList,left.location.size);
        { round as floating point using current rounding mode }
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FRINTX,hreg,left.location.register));
        { convert to signed integer rounding towards zero (there's no "round to
          integer using current rounding mode") }
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FCVTZS,location.register,hreg));
      end;


    procedure taarch64inlinenode.second_trunc_real;
      begin
        secondpass(left);
        hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
        { convert to signed integer rounding towards zero }
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FCVTZS,location.register,left.location.register));
      end;

begin
  cinlinenode:=taarch64inlinenode;
end.

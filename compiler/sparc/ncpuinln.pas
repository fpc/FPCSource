{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate SPARC inline nodes

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
unit ncpuinln;

{$i fpcdefs.inc}

interface

    uses
      node,ninl,ncginl;

    type
      tsparcinlinenode = class(tcgInlineNode)
        function first_abs_real: tnode; override;
        function first_sqr_real: tnode; override;
        function first_sqrt_real: tnode; override;
        procedure second_abs_real; override;
        procedure second_sqr_real; override;
        procedure second_sqrt_real; override;
      private
        procedure load_fpu_location;
      end;


implementation

    uses
      systems,globtype,
      cutils,verbose,
      symconst,symdef,
      aasmtai,aasmdata,aasmcpu,
      cgbase,pass_2,
      cpubase,paramgr,
      nbas,ncon,ncal,ncnv,nld,
      hlcgobj,ncgutil,cgobj,cgutils;

{*****************************************************************************
                              tsparcinlinenode
*****************************************************************************}

    procedure tsparcinlinenode.load_fpu_location;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_copy(location,left.location);
        if left.location.loc=LOC_CFPUREGISTER then
          begin
           location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
           location.loc := LOC_FPUREGISTER;
         end;
      end;


    function tsparcinlinenode.first_abs_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_abs_real := nil;
      end;


    function tsparcinlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sqr_real:=nil;
      end;


    function tsparcinlinenode.first_sqrt_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sqrt_real := nil;
      end;


    procedure tsparcinlinenode.second_abs_real;
      begin
        load_fpu_location;
        case tfloatdef(left.resultdef).floattype of
          s32real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FABSs,left.location.register,location.register));
          s64real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FABSd,left.location.register,location.register));
          s128real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FABSq,left.location.register,location.register));
          else
            internalerror(200410031);
        end;
      end;


    procedure tsparcinlinenode.second_sqr_real;
      begin
        load_fpu_location;
        case tfloatdef(left.resultdef).floattype of
          s32real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_FMULs,left.location.register,left.location.register,location.register));
          s64real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_FMULd,left.location.register,left.location.register,location.register));
          s128real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_FMULq,left.location.register,left.location.register,location.register));
          else
            internalerror(200410032);
        end;
      end;


    procedure tsparcinlinenode.second_sqrt_real;
      begin
        load_fpu_location;
        case tfloatdef(left.resultdef).floattype of
          s32real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRTs,left.location.register,location.register));
          s64real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRTd,left.location.register,location.register));
          s128real:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRTq,left.location.register,location.register));
          else
            internalerror(200410033);
        end;
      end;

begin
  cInlineNode:=tsparcinlinenode;
end.

{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate RiscV64 assembler for math nodes

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
unit nrv64mat;

{$I fpcdefs.inc}

  interface

    uses
      node,nmat, ncgmat,
      cgbase;

    type
      trv64moddivnode = class(tcgmoddivnode)
        function use_moddiv64bitint_helper: boolean; override;
        procedure emit_div_reg_reg(signed: boolean; denum, num: tregister); override;
        procedure emit_mod_reg_reg(signed: boolean; denum, num: tregister); override;
        function first_moddiv64bitint: tnode; override;
      end;

      trv64shlshrnode = class(tcgshlshrnode)
      end;

      trv64unaryminusnode = class(tcgunaryminusnode)
      end;

      trv64notnode = class(tcgnotnode)
        procedure second_boolean; override;
      end;

implementation

    uses
      nadd,ninl,ncal,ncnv,
      globtype,systems,constexp,
      cutils,verbose,globals,
      cpuinfo,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgutils,cgobj,hlcgobj,
      pass_1,pass_2,htypechk,
      ncon,procinfo,
      cpubase,
      ncgutil,cgcpu;

    procedure trv64notnode.second_boolean;
      var
        tlabel, flabel: tasmlabel;
      begin
        secondpass(left);
        if not handle_locjump then
          begin
            case left.location.loc of
              LOC_FLAGS :
                begin
                  Internalerror(2016060601);
                  //location_copy(location,left.location);
                  //inverse_flags(location.resflags);
                end;
              LOC_REGISTER, LOC_CREGISTER,
              LOC_REFERENCE, LOC_CREFERENCE,
              LOC_SUBSETREG, LOC_CSUBSETREG,
              LOC_SUBSETREF, LOC_CSUBSETREF:
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

                  location_reset(location,LOC_REGISTER,OS_INT);
                  location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,s64inttype);

                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_SLTIU,location.register,left.location.register,1));
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;


    function trv64moddivnode.use_moddiv64bitint_helper: boolean;
      begin
        Result:=true;
      end;


    procedure trv64moddivnode.emit_div_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_DIV
        else
          op:=A_DIVU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;


    procedure trv64moddivnode.emit_mod_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_REM
        else
          op:=A_REMU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;


    function trv64moddivnode.first_moddiv64bitint: tnode;
      var
        power: longint;
      begin
        {We can handle all cases of constant division}
        if not(cs_check_overflow in current_settings.localswitches) and
           (right.nodetype=ordconstn) and
           (nodetype=divn) and
           ((CPURV_HAS_MUL in cpu_capabilities[current_settings.cputype]) and
            (ispowerof2(tordconstnode(right).value,power) or
            (tordconstnode(right).value=1) or
            (tordconstnode(right).value=int64(-1))
            )
           ) then
          result:=nil
        else if (CPURV_HAS_MUL in cpu_capabilities[current_settings.cputype]) and
          (nodetype in [divn,modn]) then
          result:=nil
        else
          result:=inherited;

        { we may not change the result type here }
        if assigned(result) and (torddef(result.resultdef).ordtype<>torddef(resultdef).ordtype) then
          inserttypeconv(result,resultdef);
      end;

begin
  cmoddivnode := trv64moddivnode;
  cshlshrnode := trv64shlshrnode;
  cunaryminusnode := trv64unaryminusnode;
  cnotnode := trv64notnode;
end.


{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate LoongArch64 assembler for math nodes

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
unit ncpumat;

{$I fpcdefs.inc}

  interface

    uses
      node,nmat, ncgmat,
      cgbase;

    type
      tloongarch64moddivnode = class(tcgmoddivnode)
        function use_moddiv64bitint_helper: boolean; override;
        procedure emit_div_reg_reg(signed: boolean; denum, num: tregister); override;
        procedure emit_mod_reg_reg(signed: boolean; denum, num: tregister); override;
        function first_moddiv64bitint: tnode; override;
      end;

      tloongarch64shlshrnode = class(tcgshlshrnode)
      end;

      tloongarch64unaryminusnode = class(tcgunaryminusnode)
      end;

      tloongarch64notnode = class(tcgnotnode)
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

    procedure tloongarch64notnode.second_boolean;
      var
        tlabel, flabel: tasmlabel;
      begin
        secondpass(left);
        if not handle_locjump then
          begin
            case left.location.loc of
              LOC_FLAGS :
                begin
                  Internalerror(2022111907);
                end;
              LOC_REGISTER, LOC_CREGISTER,
              LOC_REFERENCE, LOC_CREFERENCE,
              LOC_SUBSETREG, LOC_CSUBSETREG,
              LOC_SUBSETREF, LOC_CSUBSETREF:
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

                  location_reset(location,LOC_REGISTER,OS_INT);
                  location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,s64inttype);

                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_SLTUI,location.register,left.location.register,1));
               end;
              else
                internalerror(2022111906);
            end;
          end;
      end;


    function tloongarch64moddivnode.use_moddiv64bitint_helper: boolean;
      begin
        Result:=true;
      end;


    procedure tloongarch64moddivnode.emit_div_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_DIV_D
        else
          op:=A_DIV_DU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;


    procedure tloongarch64moddivnode.emit_mod_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_MOD_D
        else
          op:=A_MOD_DU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;


    function tloongarch64moddivnode.first_moddiv64bitint: tnode;
      begin
        {We can handle all cases of constant division}
        if not(cs_check_overflow in current_settings.localswitches) and
           (right.nodetype=ordconstn) and
           (nodetype=divn) then
          result:=nil
        else if (nodetype in [divn,modn]) then
          result:=nil
        else
          result:=inherited;

        { we may not change the result type here }
        if assigned(result) and (torddef(result.resultdef).ordtype<>torddef(resultdef).ordtype) then
          inserttypeconv(result,resultdef);
      end;

begin
  cmoddivnode := tloongarch64moddivnode;
  cshlshrnode := tloongarch64shlshrnode;
  cunaryminusnode := tloongarch64unaryminusnode;
  cnotnode := tloongarch64notnode;
end.


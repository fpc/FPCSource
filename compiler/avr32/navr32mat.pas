{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate avr32 assembler for math nodes

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
unit navr32mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tavr32moddivnode = class(tmoddivnode)
        function first_moddivint: tnode;override;
        procedure pass_generate_code;override;
      end;

      tavr32notnode = class(tcgnotnode)
        procedure second_boolean;override;
      end;

      tavr32unaryminusnode = class(tcgunaryminusnode)
        procedure second_float;override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,constexp,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,cgutils,
      pass_2,procinfo,
      ncon,
      cpubase,cpuinfo,
      ncgutil,cgcpu,
      nadd,pass_1,symdef;

{*****************************************************************************
                             Tavr32MODDIVNODE
*****************************************************************************}

    function tavr32moddivnode.first_moddivint: tnode;
      var
        power  : longint;
      begin
        if (right.nodetype=ordconstn) and
          (nodetype=divn) and
          (ispowerof2(tordconstnode(right).value,power) or
           (tordconstnode(right).value=1) or
           (tordconstnode(right).value=int64(-1))
          ) and
          not(is_64bitint(resultdef)) then
          result:=nil
        else if (nodetype=divn) and
          not(is_64bitint(resultdef)) then
          result:=nil
        else if (nodetype=modn) and
          not(is_64bitint(resultdef)) then
          begin
            if (right.nodetype=ordconstn) and
              ispowerof2(tordconstnode(right).value,power) and
              (tordconstnode(right).value>0) then
              result:=caddnode.create(andn,left,cordconstnode.create(tordconstnode(right).value-1,sinttype,false))
            else
              result:=nil;
          end
        else
          result:=inherited first_moddivint;
      end;


    procedure tavr32moddivnode.pass_generate_code;
      var
        power  : longint;
        helper1,helper2,
        numerator,
        resultreg  : tregister;
        size       : Tcgsize;

      procedure genOrdConstNodeDiv;
        begin
          if tordconstnode(right).value=0 then
            internalerror(2005061701)
          else if tordconstnode(right).value=1 then
            cg.a_load_reg_reg(current_asmdata.CurrAsmList, OS_INT, OS_INT, numerator, resultreg)
          else if (tordconstnode(right).value = int64(-1)) then
            begin
              cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NEG,OS_INT,numerator,resultreg);
            end
          else if ispowerof2(tordconstnode(right).value,power) then
            begin
              if (is_signed(right.resultdef)) then
                begin
                   helper1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                   helper2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                   current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ASR,helper1,numerator,31));
                   current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LSR,helper1,32-power));
                   current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADD,helper2,numerator,helper1));
                   current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_ASR,helper2,power));
                   cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,helper2,resultreg);
                 end
              else
                cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_INT,power,numerator,resultreg)
            end;
        end;

      begin
        secondpass(left);
        secondpass(right);

        location_force_reg(current_asmdata.CurrAsmList,left.location,OS_INT,true);
        location_force_reg(current_asmdata.CurrAsmList,right.location,OS_INT,true);

        if (nodetype=divn) and
           not(is_64bitint(resultdef)) then
          begin
            size:=def_cgsize(left.resultdef);

            if (right.nodetype=ordconstn) and
               ((tordconstnode(right).value=1) or
                (tordconstnode(right).value=int64(-1)) or
                (tordconstnode(right).value=0) or
                ispowerof2(tordconstnode(right).value,power)) then
              begin
                location_copy(location,left.location);
                location.loc := LOC_REGISTER;
                location.register := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                resultreg:=location.register;

                numerator:=left.location.register;

                genOrdConstNodeDiv;
              end
            else
              begin
                location_copy(location,left.location);
                location.loc := LOC_REGISTER;
                location.register := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                cg.alloccpuregisters(current_asmdata.CurrAsmList, R_INTREGISTER, [RS_R10,RS_R11]);

                if is_signed(left.resultdef) or
                   is_signed(right.resultdef) then
                  cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_IDIV,OS_INT,right.location.register,left.location.register,NR_R10)
                else
                  cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_DIV,OS_INT,right.location.register,left.location.register,NR_R10);

                cg.dealloccpuregisters(current_asmdata.CurrAsmList, R_INTREGISTER, [RS_R10,RS_R11]);

                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_R10,location.register);
              end;
          end
        else if (nodetype=modn) and
           not(is_64bitint(resultdef)) then
          begin
            location_copy(location,left.location);
            location.loc := LOC_REGISTER;
            location.register := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            cg.alloccpuregisters(current_asmdata.CurrAsmList, R_INTREGISTER, [RS_R10,RS_R11]);

            if is_signed(left.resultdef) or
               is_signed(right.resultdef) then
              cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_IDIV,OS_INT,right.location.register,left.location.register,NR_R10)
            else
              cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_DIV,OS_INT,right.location.register,left.location.register,NR_R10);

            cg.dealloccpuregisters(current_asmdata.CurrAsmList, R_INTREGISTER, [RS_R10,RS_R11]);

            cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_R11,location.register);
          end
        else
          internalerror(2012090701);

        { unsigned division/module can only overflow in case of division by zero }
        { (but checking this overflow flag is more convoluted than performing a  }
        {  simple comparison with 0)                                             }
        if is_signed(right.resultdef) then
          cg.g_overflowcheck(current_asmdata.CurrAsmList,location,resultdef);
      end;

{*****************************************************************************
                               Tavr32NOTNODE
*****************************************************************************}

    procedure tavr32notnode.second_boolean;
      var
        hl : tasmlabel;
      begin
        { if the location is LOC_JUMP, we do the secondpass after the
          labels are allocated
        }
        if left.expectloc=LOC_JUMP then
          begin
            hl:=current_procinfo.CurrTrueLabel;
            current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
            current_procinfo.CurrFalseLabel:=hl;
            secondpass(left);
            maketojumpbool(current_asmdata.CurrAsmList,left,lr_load_regvars);
            hl:=current_procinfo.CurrTrueLabel;
            current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
            current_procinfo.CurrFalseLabel:=hl;
            location.loc:=LOC_JUMP;
          end
        else
          begin
            secondpass(left);
            case left.location.loc of
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_REGISTER,LOC_CREGISTER,LOC_REFERENCE,LOC_CREFERENCE,
              LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF :
                begin
                  location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
                  current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_const(A_CP,left.location.register,0), PF_W));
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_EQ;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;

{*****************************************************************************
                               Tavr32UNARYMINUSNODE
*****************************************************************************}

    procedure tavr32unaryminusnode.second_float;
      var
        op: tasmop;
      begin
        secondpass(left);
        case current_settings.fputype of
          fpu_avr32:
            begin
              location_force_reg(current_asmdata.CurrAsmList,left.location,OS_F32,true);
              location:=left.location;
              if (left.location.loc=LOC_CREGISTER) then
                location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);

              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOV,location.register,left.location.register));
              current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_NEG,location.register));
            end;
          else
            internalerror(2009112602);
        end;
      end;


begin
  cmoddivnode:=tavr32moddivnode;
  cnotnode:=tavr32notnode;
  cunaryminusnode:=tavr32unaryminusnode;
end.

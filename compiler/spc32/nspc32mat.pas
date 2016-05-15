{
    Copyright (c) 1998-2008 by Florian Klaempfl

    Generates spc32 assembler for math nodes

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
unit nspc32mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tspc32moddivnode = class(tmoddivnode)
        function first_moddivint: tnode;override;
        procedure pass_generate_code;override;
      end;

      tspc32notnode = class(tcgnotnode)
        procedure second_boolean;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,constexp,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,hlcgobj,cgutils,
      pass_2,procinfo,
      ncon,
      cpubase,
      ncgutil,cgcpu;

{*****************************************************************************
                             Tspc32MODDIVNODE
*****************************************************************************}

    function tspc32moddivnode.first_moddivint: tnode;
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
        else
          result:=inherited first_moddivint;
      end;


    procedure tspc32moddivnode.pass_generate_code;
      var
        size: TCgSize;
        numerator, resultreg: TRegister;
      begin
        secondpass(left);
        secondpass(right);
        location_copy(location,left.location);

        { put numerator in register }
        size:=def_cgsize(left.resultdef);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,
          left.resultdef,left.resultdef,true);
        location_copy(location,left.location);
        numerator:=location.register;
        resultreg:=location.register;
        if location.loc=LOC_CREGISTER then
          begin
            location.loc := LOC_REGISTER;
            location.register := cg.getintregister(current_asmdata.CurrAsmList,size);
            resultreg:=location.register;
          end
        else if (nodetype=modn) or (right.nodetype=ordconstn) then
          begin
            // for a modulus op, and for const nodes we need the result register
            // to be an extra register
            resultreg:=cg.getintregister(current_asmdata.CurrAsmList,size);
          end;

        location.register:=resultreg;

        { unsigned division/module can only overflow in case of division by zero }
        { (but checking this overflow flag is more convoluted than performing a  }
        {  simple comparison with 0)                                             }
        if is_signed(right.resultdef) then
          cg.g_overflowcheck(current_asmdata.CurrAsmList,location,resultdef);
      end;

{*****************************************************************************
                               Tspc32NOTNODE
*****************************************************************************}

    procedure tspc32notnode.second_boolean;
      var
        hl : tasmlabel;
        tmpreg : tregister;
        i : longint;
      begin
        { if the location is LOC_JUMP, we do the secondpass after the
          labels are allocated
        }
        if not handle_locjump then
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
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_LD,left.location.register));
                  current_asmdata.CurrAsmList.concat(taicpu.op_const(A_SUB,0));
                  tmpreg:=left.location.register;

                  { spc32 has no cpci, so we use the first register as "zero" register }
                  if tcgsize2size[left.location.size] > 4 then
                    begin
                      current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_LD,left.location.registerhi));
                      current_asmdata.CurrAsmList.concat(taicpu.op_const(A_SBC,0));
                    end;
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_EQ;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;

begin
  cmoddivnode:=tspc32moddivnode;
  cnotnode:=tspc32notnode;
end.

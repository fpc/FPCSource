{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate ARM assembler for math nodes

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
unit narmmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tarmnotnode = class(tcgnotnode)
        procedure second_boolean;override;
      end;


      tarmunaryminusnode = class(tcgunaryminusnode)
        procedure second_float;override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,cgutils,
      pass_1,pass_2,procinfo,
      ncon,
      cpubase,cpuinfo,
      ncgutil,cgcpu,cg64f32,rgobj;

{*****************************************************************************
                               TARMNOTNODE
*****************************************************************************}

    procedure tarmnotnode.second_boolean;
      var
        hl : tasmlabel;
        ins : taicpu;
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
              LOC_REGISTER,LOC_CREGISTER,LOC_REFERENCE,LOC_CREFERENCE :
                begin
                  location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_CMP,left.location.register,0));
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_EQ;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;

{*****************************************************************************
                               TARMUNARYMINUSNODE
*****************************************************************************}

    procedure tarmunaryminusnode.second_float;
      begin
        secondpass(left);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location_force_fpureg(current_asmdata.CurrAsmList,left.location,false);
        location:=left.location;
        current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_const(A_RSF,
          location.register,left.location.register,0),
          cgsize2fpuoppostfix[def_cgsize(resultdef)]));
      end;


begin
   cnotnode:=tarmnotnode;
   cunaryminusnode:=tarmunaryminusnode;
end.

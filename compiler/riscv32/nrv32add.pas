{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the Risc-V32

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
unit nrv32add;

{$i fpcdefs.inc}

  interface

    uses
      node, ncgadd, aasmbase, nrvadd, cpubase,
      cgbase;

    type
      trv32addnode = class(trvaddnode)
      private
        procedure cmp64_le(left_reg, right_reg: TRegister64; unsigned: boolean);
        procedure cmp64_lt(left_reg, right_reg: TRegister64; unsigned: boolean);
      protected
        function use_generic_mul32to64: boolean; override;

        procedure second_cmp64bit; override;
      end;

  implementation

    uses
      systems,
      cutils,verbose,
      paramgr,procinfo,
      aasmtai,aasmdata,aasmcpu,defutil,
      cgcpu,cgutils,nadd,
      cpupara,
      ncon,nset,
      hlcgobj, ncgutil,cgobj;

    const
      cmpops: array[boolean] of TOpCmp = (OC_LT,OC_B);

    procedure trv32addnode.cmp64_lt(left_reg, right_reg: TRegister64;unsigned: boolean);
      begin
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,cmpops[unsigned],right_reg.reghi,left_reg.reghi,location.truelabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.falselabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_B,right_reg.reglo,left_reg.reglo,location.truelabel);
        cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
      end;


    procedure trv32addnode.cmp64_le(left_reg, right_reg: TRegister64;unsigned: boolean);
      begin
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,cmpops[unsigned],left_reg.reghi,right_reg.reghi,location.falselabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.truelabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_B,left_reg.reglo,right_reg.reglo,location.falselabel);
        cg.a_jmp_always(current_asmdata.CurrAsmList,location.truelabel);
      end;

    function trv32addnode.use_generic_mul32to64: boolean;
      begin
        result:=true;
      end;

    procedure trv32addnode.second_cmp64bit;
      var
        truelabel,
        falselabel: tasmlabel;
        unsigned: boolean;
        left_reg,right_reg: TRegister64;
      begin
        current_asmdata.getjumplabel(truelabel);
        current_asmdata.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        left_reg:=left.location.register64;
        if (right.location.loc=LOC_CONSTANT) then
          begin
            if lo(right.location.value64)=0 then
              right_reg.reglo:=NR_X0
            else
              begin
                right_reg.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,lo(right.location.value64),right_reg.reglo);
              end;
            if hi(right.location.value64)=0 then
              right_reg.reghi:=NR_X0
            else
              begin
                right_reg.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,hi(right.location.value64),right_reg.reghi);
              end;
          end
        else
          right_reg:=right.location.register64;

        case NodeType of
          equaln:
            begin
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.falselabel);
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,location.falselabel);
              cg.a_jmp_always(current_asmdata.CurrAsmList,location.truelabel);
            end;
          unequaln:
            begin
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.truelabel);
              cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,location.truelabel);
              cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
            end;
        else
          if nf_swapped in flags then
            case NodeType of
              ltn:
                cmp64_lt(right_reg, left_reg,unsigned);
              lten:
                cmp64_le(right_reg, left_reg,unsigned);
              gtn:
                cmp64_lt(left_reg, right_reg,unsigned);
              gten:
                cmp64_le(left_reg, right_reg,unsigned);
              else
                internalerror(2019051034);
            end
          else
            case NodeType of
              ltn:
                cmp64_lt(left_reg, right_reg,unsigned);
              lten:
                cmp64_le(left_reg, right_reg,unsigned);
              gtn:
                cmp64_lt(right_reg, left_reg,unsigned);
              gten:
                cmp64_le(right_reg, left_reg,unsigned);
              else
                internalerror(2019051033);
            end;
        end;
      end;

begin
   caddnode:=trv32addnode;
end.

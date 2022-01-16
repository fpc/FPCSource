{
    Copyright (c) 2020 by Sven Barth

    Generates Z80 inline nodes

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
unit nz80inl;

{$i fpcdefs.inc}

  interface

    uses
      node,ninl,ncginl, aasmbase;

    type
      tz80inlinenode = class(tcginlinenode)
        function pass_typecheck_cpu:tnode;override;
        function first_cpu : tnode;override;
        procedure pass_generate_code_cpu;override;
      end;

  implementation

    uses
      compinnr,
      aasmdata,
      aasmcpu,
      symdef,
      defutil,
      hlcgobj,
      pass_2,
      ncal,
      cgbase, cgobj, cgutils,
      cpubase;


    function tz80inlinenode.pass_typecheck_cpu : tnode;
      begin
        Result:=nil;
        case inlinenumber of
          in_z80_inport:
            begin
              CheckParameters(1);
              resultdef:=u8inttype;
            end;
          in_z80_outport:
            begin
              CheckParameters(2);
              resultdef:=voidtype;
            end;
          else
            Result:=inherited pass_typecheck_cpu;
        end;
      end;


    function tz80inlinenode.first_cpu : tnode;
      begin
        Result:=nil;
        case inlinenumber of
          in_z80_inport:
            expectloc:=LOC_REGISTER;
          in_z80_outport:
            expectloc:=LOC_VOID;
          else
            Result:=inherited first_cpu;
        end;
      end;


    procedure tz80inlinenode.pass_generate_code_cpu;

      procedure inport;
        var
          portnumber : tnode;
          dreg : tregister;
          ref : treference;
        begin
          portnumber:=left;
          secondpass(portnumber);
          if (portnumber.location.loc=LOC_CONSTANT) and
              (portnumber.location.value>=0) and
              (portnumber.location.value<=$ff) then
            begin
              { data needs to be put into A }
              dreg:=NR_A;
              reference_reset_base(ref,NR_NO,portnumber.location.value,ctempposinvalid,1,[]);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_ref(A_IN,dreg,ref));
            end
          else
            begin
              { data can be put anywhere, but port number must be in C }
              hlcg.getcpuregister(current_asmdata.CurrAsmList,NR_C);
              dreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_8);
              hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,portnumber.resultdef,u8inttype,portnumber.location,NR_C);
              reference_reset_base(ref,NR_C,0,ctempposinvalid,1,[]);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_ref(A_IN,dreg,ref));
              hlcg.ungetcpuregister(current_asmdata.CurrAsmList,NR_C);
            end;
            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
            location.register:=dreg;
        end;

      procedure outport;
        var
          portnumber,
          portdata : tnode;
          dreg : tregister;
          ref : treference;
        begin
          portnumber:=tcallparanode(tcallparanode(left).right).left;
          portdata:=tcallparanode(left).left;
          secondpass(portdata);
          secondpass(portnumber);
          if (portnumber.location.loc=LOC_CONSTANT) and
              (portnumber.location.value>=0) and
              (portnumber.location.value<=$ff) then
            begin
              { data needs to reside in A }
              dreg:=NR_A;
              hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,portdata.resultdef,u8inttype,portdata.location,dreg);
              hlcg.getcpuregister(current_asmdata.CurrAsmList,dreg);
              reference_reset_base(ref,NR_NO,portnumber.location.value,ctempposinvalid,1,[]);
              current_asmdata.currasmlist.concat(taicpu.op_ref_reg(A_OUT,ref,dreg));
              hlcg.ungetcpuregister(current_asmdata.CurrAsmList,dreg);
            end
          else
            begin
              { data can reside anywhere, but port number must be in C }
              hlcg.getcpuregister(current_asmdata.CurrAsmList,NR_C);
              dreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_8);
              hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,portdata.resultdef,u8inttype,portdata.location,dreg);
              hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,portnumber.resultdef,u8inttype,portnumber.location,NR_C);
              reference_reset_base(ref,NR_C,0,ctempposinvalid,1,[]);
              current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_OUT,ref,dreg));
              hlcg.ungetcpuregister(current_asmdata.CurrAsmList,NR_C);
            end;
        end;

      begin
        case inlinenumber of
          in_z80_inport:
            inport;
          in_z80_outport:
            outport;
          else
            inherited pass_generate_code_cpu;
        end;
      end;


begin
  cinlinenode:=tz80inlinenode;
end.

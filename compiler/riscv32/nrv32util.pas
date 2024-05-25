{
    Copyright (c) 2024

    RISCV32 version of some node tree helper routines

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
unit nrv32util;

{$i fpcdefs.inc}

interface

  uses
    cclasses,
    fmodule,
    node,nbas,
    ngenutil,
    symtype,symconst,symsym,symdef;


  type
    trv32nodeutils = class(tnodeutils)
    protected
      class procedure insert_init_final_table(main: tmodule; entries:tfplist); override;
    end;

implementation

    uses
      verbose,cutils,globtype,globals,constexp,
      aasmdata,aasmtai,aasmcpu,aasmcnst,aasmbase,
      cpubase,
      cgutils,
      symbase,symcpu,symtable,defutil,
      ncnv,ncon,ninl,ncal,nld,nmem,
      systems,
      CPUInfo,
      ppu,
      pass_1;


  class procedure trv32nodeutils.insert_init_final_table(main: tmodule; entries:tfplist);

    procedure genentry(list : TAsmList);
      var
        ref: treference;
      begin
        // addi sp,sp,-16
        list.Concat(taicpu.op_reg_reg_const(A_ADDI,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,-16));

        // sw ra,12(sp)
        reference_reset(ref,4,[]);
        ref.base:=NR_STACK_POINTER_REG;
        ref.offset:=12;
        list.Concat(taicpu.op_reg_ref(A_SW,NR_RETURN_ADDRESS_REG,ref));
      end;

    procedure genexit(list : TAsmList);
      var
        ref: treference;
      begin
        // lw ra,12(sp)
        reference_reset(ref,4,[]);
        ref.base:=NR_STACK_POINTER_REG;
        ref.offset:=12;
        list.Concat(taicpu.op_reg_ref(A_LW,NR_RETURN_ADDRESS_REG,ref));

        // addi sp,sp,16
        list.Concat(taicpu.op_reg_reg_const(A_ADDI,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,16));

        // ret
        list.Concat(taicpu.op_none(A_RET));
      end;

    var
      initList, finalList, header: TAsmList;
      entry : pinitfinalentry;
      i : longint;
      ref:treference;
    begin
      if not(tf_init_final_units_by_calls in target_info.flags) then
        begin
          inherited insert_init_final_table(main,entries);
          exit;
        end;
      initList:=TAsmList.create;
      finalList:=TAsmList.create;

      genentry(initList);
      genentry(finalList);

      for i:=0 to entries.count-1 do
        begin
          entry:=pinitfinalentry(entries[i]);
          if entry^.finifunc<>'' then
            finalList.Concat(taicpu.op_sym(A_CALL,current_asmdata.RefAsmSymbol(entry^.finifunc,AT_FUNCTION)));
          if entry^.initfunc<>'' then
            initList.Concat(taicpu.op_sym(A_CALL,current_asmdata.RefAsmSymbol(entry^.initfunc,AT_FUNCTION)));
        end;

      genexit(initList);
      genexit(finalList);

      begin
        header:=TAsmList.create;
        new_section(header, sec_code, 'FPC_INIT_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_INIT_FUNC_TABLE',AT_FUNCTION,0,voidcodepointertype));

        initList.insertList(header);
        header.free;

        current_asmdata.AsmLists[al_procedures].concatList(initList);
      end;

      begin
        header:=TAsmList.create;
        new_section(header, sec_code, 'FPC_FINALIZE_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_FINALIZE_FUNC_TABLE',AT_FUNCTION,0,voidcodepointertype));

        finalList.insertList(header);
        header.free;

        current_asmdata.AsmLists[al_procedures].concatList(finalList);
      end;

      initList.Free;
      finalList.Free;

      inherited insert_init_final_table(main,entries);
    end;

begin
  cnodeutils:=trv32nodeutils;
end.


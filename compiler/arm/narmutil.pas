{
    Copyright (c) 2019 by Florian Klämpfl

    ARM version of some node tree helper routines

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
unit narmutil;

{$i fpcdefs.inc}

interface

  uses
    cclasses,ngenutil,fmodule,
    aasmdata,
    compilerbase;

  type
    tarmnodeutils = class(tnodeutils)
      procedure InsertObjectInfo(AsmData: TAsmData); override;
      procedure insert_init_final_table(main : tmodule; entries: tfplist); override;
    end;


  implementation

    uses
      verbose,
      systemstypes,systems,
      globals,
      cpuinfo,cpubase,
      cgbase,cgutils,
      aasmbase,aasmtai,aasmcpu,
      symdef,
      compiler;

    const
      Tag_File = 1;
      Tag_Section = 2;
      Tag_Symbol = 3;
      Tag_CPU_raw_name = 4;
      Tag_CPU_name = 5;
      Tag_CPU_arch = 6;
      Tag_CPU_arch_profile = 7;
      Tag_ARM_ISA_use = 8;
      Tag_THUMB_ISA_use = 9;
      Tag_FP_Arch = 10;
      Tag_WMMX_arch = 11;
      Tag_Advanced_SIMD_arch = 12;
      Tag_PCS_config = 13;
      Tag_ABI_PCS_R9_use = 14;
      Tag_ABI_PCS_RW_data = 15;
      Tag_ABI_PCS_RO_data = 16;
      Tag_ABI_PCS_GOT_use = 17;
      Tag_ABI_PCS_wchar_t = 18;
      Tag_ABI_FP_rounding = 19;
      Tag_ABI_FP_denormal = 20;
      Tag_ABI_FP_exceptions = 21;
      Tag_ABI_FP_user_exceptions = 22;
      Tag_ABI_FP_number_model = 23;
      Tag_ABI_align_needed = 24;
      Tag_ABI_align8_preserved = 25;
      Tag_ABI_enum_size = 26;
      Tag_ABI_HardFP_use = 27;
      Tag_ABI_VFP_args = 28;
      Tag_ABI_WMMX_args = 29;
      Tag_ABI_optimization_goals = 30;
      Tag_ABI_FP_optimization_goals = 31;
      Tag_compatiblity = 32;
      Tag_CPU_unaligned_access = 34;
      Tag_FP_HP_extension = 36;
      Tag_ABI_FP_16bit_format = 38;
      Tag_MPextension_use = 42;
      Tag_DIV_use = 44;
      Tag_nodefaults = 64;
      Tag_also_compatible_with = 65;
      Tag_conformance = 67;
      Tag_T2EE_use = 66;
      Tag_Virtualization_use = 68;

    procedure tarmnodeutils.InsertObjectInfo(AsmData: TAsmData);
      begin
        inherited;
        { write eabi attributes to object file? }
        if (compiler.target.info.system in [system_arm_linux]) and (compiler.target.info.abi in [abi_eabihf,abi_eabi]) then
          begin
            case compiler.globals.current_settings.cputype of
              cpu_armv2,
              cpu_armv3:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,''));
                end;
              cpu_armv4:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,1));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'4'));
                end;
              cpu_armv4t:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,2));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'4T'));
                end;
              cpu_armv5t:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,3));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'5T'));
                end;
              cpu_armv5te:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,4));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'5TE'));
                end;
              cpu_armv5tej:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,5));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'5TEJ'));
                end;
              cpu_armv6:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,6));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'6'));
                end;
              cpu_armv6k:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,9));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'6K'));
                end;
              cpu_armv6t2:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,8));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'T2'));
                end;
              cpu_armv6z:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,7));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'6Z'));
                end;
              cpu_armv6m:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,11));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'6-M'));
                end;
              cpu_armv7:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,10));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,0));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'7'));
                end;
              cpu_armv7a:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,10));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,$41));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'7-A'));
                end;
              cpu_armv7r:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,10));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,$52));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'7-R'));
                end;
              cpu_armv7m:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,10));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,$4D));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'7-M'));
                end;
              cpu_armv7em:
                begin
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch,13));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_arch_profile,$4D));
                  AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_CPU_name,'7E-M'));
                end;
              else
                Internalerror(2019100602);
            end;
            case compiler.globals.current_settings.fputype of
              fpu_none,
              fpu_soft,
              fpu_libgcc,
              fpu_fpa,
              fpu_fpa10,
              fpu_fpa11:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,0));
              fpu_vfpv2:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,2));
              fpu_vfpv3,
              fpu_neon_vfpv3:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,3));
              fpu_vfpv3_d16:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,4));
              fpu_fpv4_sp_d16,
              fpu_fpv4_s16:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,6));
              fpu_vfpv4,
              fpu_neon_vfpv4:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,5));
              fpu_fpv5_sp_d16,
              fpu_fpv5_d16:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,8));
              fpu_fp_armv8:
                AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_FP_Arch,7));
              { else not needed anymore PM 2020/04/13
                Internalerror(2019100603); }
            end;
            if FPUARM_HAS_FMA in fpu_capabilities[compiler.globals.current_settings.fputype] then
              AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_Advanced_SIMD_arch,2))
            else if FPUARM_HAS_NEON in fpu_capabilities[compiler.globals.current_settings.fputype] then
              AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_Advanced_SIMD_arch,1))
            else
              AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_Advanced_SIMD_arch,0));
            AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ARM_ISA_use,1));
            if CPUARM_HAS_THUMB2 in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype] then
              AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_THUMB_ISA_use,2))
            else
              AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_THUMB_ISA_use,1));
            if compiler.target.info.abi=abi_eabihf then
              AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ABI_VFP_args,1))
            else
              AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ABI_VFP_args,0));
            AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ABI_FP_denormal,1));
            AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ABI_FP_exceptions,1));
            AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ABI_FP_number_model,3));
            AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ABI_align_needed,0));
            AsmData.asmlists[al_start].Concat(tai_attribute.create(ait_eabi_attribute,Tag_ABI_align8_preserved,1));
            { gcc typically writes more like enum size, wchar size, optimization goal, however, this
              is normally not module global in FPC }
          end;
      end;

    procedure tarmnodeutils.insert_init_final_table(main : tmodule; entries:tfplist);

      var
        main_asmdata: TAsmData;

      procedure genentry(list : TAsmList);
        var
          ref: treference;
        begin
          if GenerateThumbCode then
            list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R14]))
          else
            begin
              reference_reset(ref,4,[]);
              ref.index:=NR_STACK_POINTER_REG;
              ref.addressmode:=AM_PREINDEXED;
              list.concat(setoppostfix(taicpu.op_ref_regset(A_STM,ref,R_INTREGISTER,R_SUBWHOLE,[RS_R14]),PF_FD));
            end;
        end;

      procedure genexit(list : TAsmList);
        var
          ref: treference;
        begin
          if GenerateThumbCode then
            list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R15]))
          else
            begin
              reference_reset(ref,4,[]);
              ref.index:=NR_STACK_POINTER_REG;
              ref.addressmode:=AM_PREINDEXED;
              list.concat(setoppostfix(taicpu.op_ref_regset(A_LDM,ref,R_INTREGISTER,R_SUBWHOLE,[RS_R15]),PF_FD));
            end;
        end;

      var
        initList, finalList, header: TAsmList;
        entry : pinitfinalentry;
        i : longint;
      begin
        main_asmdata:=TAsmData(main.AsmData);
        if not(tf_init_final_units_by_calls in compiler.target.info.flags) then
          begin
            inherited insert_init_final_table(main,entries);
            exit;
          end;
        initList:=TAsmList.create(main_asmdata);
        finalList:=TAsmList.create(main_asmdata);

        genentry(finalList);
        genentry(initList);

        for i:=0 to entries.count-1 do
          begin
            entry:=pinitfinalentry(entries[i]);
            if entry^.finifunc<>'' then
              finalList.Concat(taicpu.op_sym(A_BL,main_asmdata.RefAsmSymbol(entry^.finifunc,AT_FUNCTION)));
            if entry^.initfunc<>'' then
              initList.Concat(taicpu.op_sym(A_BL,main_asmdata.RefAsmSymbol(entry^.initfunc,AT_FUNCTION)));
          end;

        genexit(finalList);
        genexit(initList);

        header:=TAsmList.create(main_asmdata);
        new_section(header, sec_code, 'FPC_INIT_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_INIT_FUNC_TABLE',AT_FUNCTION,0,compiler.deftypes.voidcodepointertype));

        initList.insertList(header);
        header.free;

        main_asmdata.AsmLists[al_procedures].concatList(initList);

        header:=TAsmList.create(main_asmdata);
        new_section(header, sec_code, 'FPC_FINALIZE_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_FINALIZE_FUNC_TABLE',AT_FUNCTION,0,compiler.deftypes.voidcodepointertype));

        finalList.insertList(header);
        header.free;

        main_asmdata.AsmLists[al_procedures].concatList(finalList);

        initList.Free;
        finalList.Free;

        inherited insert_init_final_table(main,entries);
      end;

  begin
    cnodeutils:=tarmnodeutils;
  end.


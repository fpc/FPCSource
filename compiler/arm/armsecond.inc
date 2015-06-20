in_arm_nop
,in_arm_yield
,in_arm_wfi
,in_arm_wfe
,in_arm_sev
,in_arm_clrex
: //
  begin
    case inlinenumber of
      in_arm_clrex: begin op:=A_clrex; pf:=PF_None; end;
      in_arm_sev: begin op:=A_sev; pf:=PF_None; end;
      in_arm_wfe: begin op:=A_wfe; pf:=PF_None; end;
      in_arm_wfi: begin op:=A_wfi; pf:=PF_None; end;
      in_arm_yield: begin op:=A_yield; pf:=PF_None; end;
      in_arm_nop: begin op:=A_nop; pf:=PF_None; end;
    end;

    GetParameters(0);

    for i := 1 to 0 do secondpass(paraarray[i]);

    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_none(op),pf));
  end;
in_arm_setend
: //endian:i32;
  begin
    case inlinenumber of
      in_arm_setend: begin op:=A_setend; pf:=PF_None; end;
    end;

    GetParameters(1);

    for i := 1 to 1 do secondpass(paraarray[i]);

    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_const(op,GetConstInt(paraarray[1])),pf));
  end;
in_arm_ldrex
,in_arm_ldrexb
,in_arm_ldrexh
: //out rt:r32;ref:ptr32;
  begin
    case inlinenumber of
      in_arm_ldrexh: begin op:=A_ldrexh; pf:=PF_None; end;
      in_arm_ldrexb: begin op:=A_ldrexb; pf:=PF_None; end;
      in_arm_ldrex: begin op:=A_ldrex; pf:=PF_None; end;
    end;

    GetParameters(1);

    for i := 1 to 1 do secondpass(paraarray[i]);

    location_make_ref(paraarray[1].location);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_ref(op,location.register,paraarray[1].location.reference),pf));
  end;
in_arm_strex
,in_arm_strexb
,in_arm_strexh
: //out rd:r32;rt:r32;ref:ptr32;
  begin
    case inlinenumber of
      in_arm_strexh: begin op:=A_strexh; pf:=PF_None; end;
      in_arm_strexb: begin op:=A_strexb; pf:=PF_None; end;
      in_arm_strex: begin op:=A_strex; pf:=PF_None; end;
    end;

    GetParameters(2);

    for i := 1 to 2 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    location_make_ref(paraarray[2].location);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_ref(op,location.register,paraarray[1].location.register,paraarray[2].location.reference),pf));
  end;
in_arm_bfi
: //var rd:r32;rn:r32;lsb:i32;msb:i32;
  begin
    case inlinenumber of
      in_arm_bfi: begin op:=A_bfi; pf:=PF_None; end;
    end;

    GetParameters(4);

    for i := 1 to 4 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,false);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location:=paraarray[1].location;
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_const_const(op,paraarray[1].location.register,paraarray[2].location.register,GetConstInt(paraarray[3]),GetConstInt(paraarray[4])),pf));
  end;
in_arm_bfc
: //var rd:r32;lsb:i32;msb:i32;
  begin
    case inlinenumber of
      in_arm_bfc: begin op:=A_bfc; pf:=PF_None; end;
    end;

    GetParameters(3);

    for i := 1 to 3 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,false);
    location:=paraarray[1].location;
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_const_const(op,paraarray[1].location.register,GetConstInt(paraarray[2]),GetConstInt(paraarray[3])),pf));
  end;
in_arm_smmul
,in_arm_smuad
,in_arm_smuadx
,in_arm_smulbb
,in_arm_smulbt
,in_arm_smultb
,in_arm_smultt
,in_arm_smulwb
,in_arm_smulwt
,in_arm_smusd
,in_arm_smusdx
,in_arm_pkhbt
,in_arm_pkhtb
: //out rd:r32;rn:r32;rm:r32;
  begin
    case inlinenumber of
      in_arm_pkhtb: begin op:=A_pkhtb; pf:=PF_None; end;
      in_arm_pkhbt: begin op:=A_pkhbt; pf:=PF_None; end;
      in_arm_smusdx: begin op:=A_smusd; pf:=PF_x; end;
      in_arm_smusd: begin op:=A_smusd; pf:=PF_None; end;
      in_arm_smulwt: begin op:=A_smulwt; pf:=PF_None; end;
      in_arm_smulwb: begin op:=A_smulwb; pf:=PF_None; end;
      in_arm_smultt: begin op:=A_smultt; pf:=PF_None; end;
      in_arm_smultb: begin op:=A_smultb; pf:=PF_None; end;
      in_arm_smulbt: begin op:=A_smulbt; pf:=PF_None; end;
      in_arm_smulbb: begin op:=A_smulbb; pf:=PF_None; end;
      in_arm_smuadx: begin op:=A_smuad; pf:=PF_x; end;
      in_arm_smuad: begin op:=A_smuad; pf:=PF_None; end;
      in_arm_smmul: begin op:=A_smmul; pf:=PF_None; end;
    end;

    GetParameters(2);

    for i := 1 to 2 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(op,location.register,paraarray[1].location.register,paraarray[2].location.register),pf));
  end;
in_arm_mla
,in_arm_mls
,in_arm_smlabb
,in_arm_smlabt
,in_arm_smlatb
,in_arm_smlatt
,in_arm_smlad
,in_arm_smladx
,in_arm_smlawb
,in_arm_smlawt
,in_arm_smlsd
,in_arm_smlsdx
,in_arm_smmla
,in_arm_smmlar
,in_arm_smmls
,in_arm_smmlsr
,in_arm_usada8
: //out rd:r32;rn:r32;rm:r32;ra:r32;
  begin
    case inlinenumber of
      in_arm_usada8: begin op:=A_usada8; pf:=PF_None; end;
      in_arm_smmlsr: begin op:=A_smmls; pf:=PF_r; end;
      in_arm_smmls: begin op:=A_smmls; pf:=PF_None; end;
      in_arm_smmlar: begin op:=A_smmla; pf:=PF_r; end;
      in_arm_smmla: begin op:=A_smmla; pf:=PF_None; end;
      in_arm_smlsdx: begin op:=A_smlsd; pf:=PF_x; end;
      in_arm_smlsd: begin op:=A_smlsd; pf:=PF_None; end;
      in_arm_smlawt: begin op:=A_smlawt; pf:=PF_None; end;
      in_arm_smlawb: begin op:=A_smlawb; pf:=PF_None; end;
      in_arm_smladx: begin op:=A_smlad; pf:=PF_x; end;
      in_arm_smlad: begin op:=A_smlad; pf:=PF_None; end;
      in_arm_smlatt: begin op:=A_smlatt; pf:=PF_None; end;
      in_arm_smlatb: begin op:=A_smlatb; pf:=PF_None; end;
      in_arm_smlabt: begin op:=A_smlabt; pf:=PF_None; end;
      in_arm_smlabb: begin op:=A_smlabb; pf:=PF_None; end;
      in_arm_mls: begin op:=A_mls; pf:=PF_None; end;
      in_arm_mla: begin op:=A_mla; pf:=PF_None; end;
    end;

    GetParameters(3);

    for i := 1 to 3 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[3].location, paraarray[3].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_reg(op,location.register,paraarray[1].location.register,paraarray[2].location.register,paraarray[3].location.register),pf));
  end;
in_arm_smull
: //out ro:rs64;rn:r32;rm:r32;
  begin
    case inlinenumber of
      in_arm_smull: begin op:=A_smull; pf:=PF_None; end;
    end;

    GetParameters(2);

    for i := 1 to 2 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_S64);
    location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList, OS_32); location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_reg(op,location.register64.reglo,location.register64.reghi,paraarray[1].location.register,paraarray[2].location.register),pf));
  end;
in_arm_umull
: //out ro:r64;rn:r32;rm:r32;
  begin
    case inlinenumber of
      in_arm_umull: begin op:=A_umull; pf:=PF_None; end;
    end;

    GetParameters(2);

    for i := 1 to 2 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_64);
    location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList, OS_32); location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_reg(op,location.register64.reglo,location.register64.reghi,paraarray[1].location.register,paraarray[2].location.register),pf));
  end;
in_arm_smlal
,in_arm_smlalbb
,in_arm_smlalbt
,in_arm_smlaltb
,in_arm_smlaltt
,in_arm_smlald
,in_arm_smlaldx
,in_arm_smlsldx
,in_arm_umlal
: //var rd:r64;rn:r32;rm:r32;
  begin
    case inlinenumber of
      in_arm_umlal: begin op:=A_umlal; pf:=PF_None; end;
      in_arm_smlsldx: begin op:=A_smlsld; pf:=PF_x; end;
      in_arm_smlaldx: begin op:=A_smlald; pf:=PF_x; end;
      in_arm_smlald: begin op:=A_smlald; pf:=PF_None; end;
      in_arm_smlaltt: begin op:=A_smlaltt; pf:=PF_None; end;
      in_arm_smlaltb: begin op:=A_smlaltb; pf:=PF_None; end;
      in_arm_smlalbt: begin op:=A_smlalbt; pf:=PF_None; end;
      in_arm_smlalbb: begin op:=A_smlalbb; pf:=PF_None; end;
      in_arm_smlal: begin op:=A_smlal; pf:=PF_None; end;
    end;

    GetParameters(3);

    for i := 1 to 3 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u64inttype,false);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[3].location, paraarray[3].resultdef,u32inttype,true);
    location:=paraarray[1].location;
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_reg(op,paraarray[1].location.register64.reglo,paraarray[1].location.register64.reghi,paraarray[2].location.register,paraarray[3].location.register),pf));
  end;
in_arm_umaal
: //out rl-rh:r64;rl:r32;rh:r32;rn:r32;rm:r32;
  begin
    case inlinenumber of
      in_arm_umaal: begin op:=A_umaal; pf:=PF_None; end;
    end;

    GetParameters(4);

    for i := 1 to 4 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[3].location, paraarray[3].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[4].location, paraarray[4].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_64);
    location.register64.reglo:=paraarray[1].location.register;
    location.register64.reghi:=paraarray[2].location.register;
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_reg(op,paraarray[1].location.register,paraarray[2].location.register,paraarray[3].location.register,paraarray[4].location.register),pf));
  end;
in_arm_pkhbt_shift
: //out rd:r32;rn:r32;rm:r32;ls:i32;
  begin
    case inlinenumber of
      in_arm_pkhbt_shift: begin op:=A_pkhbt; pf:=PF_None; end;
    end;

    GetParameters(3);

    for i := 1 to 3 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_const(op,location.register,paraarray[1].location.register,paraarray[2].location.register,GetConstInt(paraarray[3])),pf));
  end;
in_arm_pkhtb_shift
: //out rd:r32;rn:r32;rm:r32;rs:i32;
  begin
    case inlinenumber of
      in_arm_pkhtb_shift: begin op:=A_pkhtb; pf:=PF_None; end;
    end;

    GetParameters(3);

    for i := 1 to 3 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_const(op,location.register,paraarray[1].location.register,paraarray[2].location.register,GetConstInt(paraarray[3])),pf));
  end;
in_arm_qadd
,in_arm_qadd16
,in_arm_qadd8
,in_arm_qasx
,in_arm_qdadd
,in_arm_qdsub
,in_arm_qsax
,in_arm_qsub
,in_arm_qsub16
,in_arm_qsub8
,in_arm_sadd16
,in_arm_sadd8
,in_arm_sasx
,in_arm_shadd16
,in_arm_shadd8
,in_arm_shasx
,in_arm_shsax
,in_arm_shsub16
,in_arm_shsub8
,in_arm_ssax
,in_arm_ssub16
,in_arm_ssub8
,in_arm_uadd16
,in_arm_uadd8
,in_arm_uasx
,in_arm_uhadd16
,in_arm_uhadd8
,in_arm_uhasx
,in_arm_uhsax
,in_arm_uhsub16
,in_arm_uhsub8
,in_arm_uqadd16
,in_arm_uqadd8
,in_arm_uqasx
,in_arm_uqsax
,in_arm_uqsub16
,in_arm_uqsub8
,in_arm_usax
,in_arm_usub16
,in_arm_usub8
,in_arm_usad8
,in_arm_sxtab
,in_arm_sxtab16
,in_arm_sxtah
,in_arm_uxtab
,in_arm_uxtab16
,in_arm_uxtah
: //out rd:r32;rm:r32;rn:r32;
  begin
    case inlinenumber of
      in_arm_uxtah: begin op:=A_uxtah; pf:=PF_None; end;
      in_arm_uxtab16: begin op:=A_uxtab16; pf:=PF_None; end;
      in_arm_uxtab: begin op:=A_uxtab; pf:=PF_None; end;
      in_arm_sxtah: begin op:=A_sxtah; pf:=PF_None; end;
      in_arm_sxtab16: begin op:=A_sxtab16; pf:=PF_None; end;
      in_arm_sxtab: begin op:=A_sxtab; pf:=PF_None; end;
      in_arm_usad8: begin op:=A_usad8; pf:=PF_None; end;
      in_arm_usub8: begin op:=A_usub8; pf:=PF_None; end;
      in_arm_usub16: begin op:=A_usub16; pf:=PF_None; end;
      in_arm_usax: begin op:=A_usax; pf:=PF_None; end;
      in_arm_uqsub8: begin op:=A_uqsub8; pf:=PF_None; end;
      in_arm_uqsub16: begin op:=A_uqsub16; pf:=PF_None; end;
      in_arm_uqsax: begin op:=A_uqsax; pf:=PF_None; end;
      in_arm_uqasx: begin op:=A_uqasx; pf:=PF_None; end;
      in_arm_uqadd8: begin op:=A_uqadd8; pf:=PF_None; end;
      in_arm_uqadd16: begin op:=A_uqadd16; pf:=PF_None; end;
      in_arm_uhsub8: begin op:=A_uhsub8; pf:=PF_None; end;
      in_arm_uhsub16: begin op:=A_uhsub16; pf:=PF_None; end;
      in_arm_uhsax: begin op:=A_uhsax; pf:=PF_None; end;
      in_arm_uhasx: begin op:=A_uhasx; pf:=PF_None; end;
      in_arm_uhadd8: begin op:=A_uhadd8; pf:=PF_None; end;
      in_arm_uhadd16: begin op:=A_uhadd16; pf:=PF_None; end;
      in_arm_uasx: begin op:=A_uasx; pf:=PF_None; end;
      in_arm_uadd8: begin op:=A_uadd8; pf:=PF_None; end;
      in_arm_uadd16: begin op:=A_uadd16; pf:=PF_None; end;
      in_arm_ssub8: begin op:=A_ssub8; pf:=PF_None; end;
      in_arm_ssub16: begin op:=A_ssub16; pf:=PF_None; end;
      in_arm_ssax: begin op:=A_ssax; pf:=PF_None; end;
      in_arm_shsub8: begin op:=A_shsub8; pf:=PF_None; end;
      in_arm_shsub16: begin op:=A_shsub16; pf:=PF_None; end;
      in_arm_shsax: begin op:=A_shsax; pf:=PF_None; end;
      in_arm_shasx: begin op:=A_shasx; pf:=PF_None; end;
      in_arm_shadd8: begin op:=A_shadd8; pf:=PF_None; end;
      in_arm_shadd16: begin op:=A_shadd16; pf:=PF_None; end;
      in_arm_sasx: begin op:=A_sasx; pf:=PF_None; end;
      in_arm_sadd8: begin op:=A_sadd8; pf:=PF_None; end;
      in_arm_sadd16: begin op:=A_sadd16; pf:=PF_None; end;
      in_arm_qsub8: begin op:=A_qsub8; pf:=PF_None; end;
      in_arm_qsub16: begin op:=A_qsub16; pf:=PF_None; end;
      in_arm_qsub: begin op:=A_qsub; pf:=PF_None; end;
      in_arm_qsax: begin op:=A_qsax; pf:=PF_None; end;
      in_arm_qdsub: begin op:=A_qdsub; pf:=PF_None; end;
      in_arm_qdadd: begin op:=A_qdadd; pf:=PF_None; end;
      in_arm_qasx: begin op:=A_qasx; pf:=PF_None; end;
      in_arm_qadd8: begin op:=A_qadd8; pf:=PF_None; end;
      in_arm_qadd16: begin op:=A_qadd16; pf:=PF_None; end;
      in_arm_qadd: begin op:=A_qadd; pf:=PF_None; end;
    end;

    GetParameters(2);

    for i := 1 to 2 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(op,location.register,paraarray[1].location.register,paraarray[2].location.register),pf));
  end;
in_arm_sxtab_ror
,in_arm_sxtab16_ror
,in_arm_sxtah_ror
,in_arm_uxtab_ror
,in_arm_uxtab16_ror
,in_arm_uxtah_ror
: //out rd:r32;rm:r32;rn:r32;ror:i32;
  begin
    case inlinenumber of
      in_arm_uxtah_ror: begin op:=A_uxtah; pf:=PF_None; end;
      in_arm_uxtab16_ror: begin op:=A_uxtab16; pf:=PF_None; end;
      in_arm_uxtab_ror: begin op:=A_uxtab; pf:=PF_None; end;
      in_arm_sxtah_ror: begin op:=A_sxtah; pf:=PF_None; end;
      in_arm_sxtab16_ror: begin op:=A_sxtab16; pf:=PF_None; end;
      in_arm_sxtab_ror: begin op:=A_sxtab; pf:=PF_None; end;
    end;

    GetParameters(3);

    for i := 1 to 3 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg_const(op,location.register,paraarray[1].location.register,paraarray[2].location.register,GetConstInt(paraarray[3])),pf));
  end;
in_arm_sxtb
,in_arm_sxtb16
,in_arm_sxth
,in_arm_uxtb
,in_arm_uxtb16
,in_arm_uxth
,in_arm_clz
,in_arm_rbit
,in_arm_rev
,in_arm_rev16
,in_arm_revsh
: //out rd:r32;rm:r32;
  begin
    case inlinenumber of
      in_arm_revsh: begin op:=A_revsh; pf:=PF_None; end;
      in_arm_rev16: begin op:=A_rev16; pf:=PF_None; end;
      in_arm_rev: begin op:=A_rev; pf:=PF_None; end;
      in_arm_rbit: begin op:=A_rbit; pf:=PF_None; end;
      in_arm_clz: begin op:=A_clz; pf:=PF_None; end;
      in_arm_uxth: begin op:=A_uxth; pf:=PF_None; end;
      in_arm_uxtb16: begin op:=A_uxtb16; pf:=PF_None; end;
      in_arm_uxtb: begin op:=A_uxtb; pf:=PF_None; end;
      in_arm_sxth: begin op:=A_sxth; pf:=PF_None; end;
      in_arm_sxtb16: begin op:=A_sxtb16; pf:=PF_None; end;
      in_arm_sxtb: begin op:=A_sxtb; pf:=PF_None; end;
    end;

    GetParameters(1);

    for i := 1 to 1 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(op,location.register,paraarray[1].location.register),pf));
  end;
in_arm_sxtb_ror
,in_arm_sxtb16_ror
,in_arm_sxth_ror
,in_arm_uxtb_ror
,in_arm_uxtb16_ror
,in_arm_uxth_ror
: //out rd:r32;rm:r32;ror:i32;
  begin
    case inlinenumber of
      in_arm_uxth_ror: begin op:=A_uxth; pf:=PF_None; end;
      in_arm_uxtb16_ror: begin op:=A_uxtb16; pf:=PF_None; end;
      in_arm_uxtb_ror: begin op:=A_uxtb; pf:=PF_None; end;
      in_arm_sxth_ror: begin op:=A_sxth; pf:=PF_None; end;
      in_arm_sxtb16_ror: begin op:=A_sxtb16; pf:=PF_None; end;
      in_arm_sxtb_ror: begin op:=A_sxtb; pf:=PF_None; end;
    end;

    GetParameters(2);

    for i := 1 to 2 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_const(op,location.register,paraarray[1].location.register,GetConstInt(paraarray[2])),pf));
  end;
in_arm_sbfx
,in_arm_ubfx
: //out rd:r32;rn:r32;lsb:i32;width:i32;
  begin
    case inlinenumber of
      in_arm_ubfx: begin op:=A_ubfx; pf:=PF_None; end;
      in_arm_sbfx: begin op:=A_sbfx; pf:=PF_None; end;
    end;

    GetParameters(3);

    for i := 1 to 3 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[1].location, paraarray[1].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_const_const(op,location.register,paraarray[1].location.register,GetConstInt(paraarray[2]),GetConstInt(paraarray[3])),pf));
  end;
in_arm_ssat
,in_arm_usat
,in_arm_ssat16
,in_arm_usat16
: //out rd:r32;sat:i32;rn:r32;
  begin
    case inlinenumber of
      in_arm_usat16: begin op:=A_usat16; pf:=PF_None; end;
      in_arm_ssat16: begin op:=A_ssat16; pf:=PF_None; end;
      in_arm_usat: begin op:=A_usat; pf:=PF_None; end;
      in_arm_ssat: begin op:=A_ssat; pf:=PF_None; end;
    end;

    GetParameters(2);

    for i := 1 to 2 do secondpass(paraarray[i]);

    hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[2].location, paraarray[2].resultdef,u32inttype,true);
    location_reset(location,LOC_REGISTER,OS_32);
    location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_const_reg(op,location.register,GetConstInt(paraarray[1]),paraarray[2].location.register),pf));
  end;

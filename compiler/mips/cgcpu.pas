{
    Copyright (c) 1998-2009 by Florian Klaempfl and David Zhang

    This unit implements the code generator for the MIPSEL

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
unit cgcpu;

{$i fpcdefs.inc}

interface

uses
  globtype, parabase,
  cgbase, cgutils, cgobj, cg64f32,
  aasmbase, aasmtai, aasmcpu, aasmdata,
  cpubase, cpuinfo,
  node, symconst, SymType, symdef,
  rgcpu;

type
  TCgMPSel = class(tcg)
  public
    procedure init_register_allocators; override;
    procedure done_register_allocators; override;
    function getfpuregister(list: tasmlist; size: Tcgsize): Tregister; override;
///    { needed by cg64 }
    procedure make_simple_ref(list: tasmlist; var ref: treference);
    procedure make_simple_ref_fpu(list: tasmlist; var ref: treference);
    procedure handle_load_store(list: tasmlist; isstore: boolean; op: tasmop; reg: tregister; ref: treference);
    procedure handle_load_store_fpu(list: tasmlist; isstore: boolean; op: tasmop; reg: tregister; ref: treference);
    procedure handle_reg_const_reg(list: tasmlist; op: Tasmop; src: tregister; a: aint; dst: tregister);

    { parameter }
    procedure a_param_const(list: tasmlist; size: tcgsize; a: aint; const paraloc: TCGPara); override;
    procedure a_param_ref(list: tasmlist; sz: tcgsize; const r: TReference; const paraloc: TCGPara); override;
    procedure a_paramaddr_ref(list: tasmlist; const r: TReference; const paraloc: TCGPara); override;
    procedure a_paramfpu_reg(list: tasmlist; size: tcgsize; const r: tregister; const paraloc: TCGPara); override;
    procedure a_paramfpu_ref(list: tasmlist; size: tcgsize; const ref: treference; const paraloc: TCGPara); override;
    procedure a_call_name(list: tasmlist; const s: string; weak : boolean); override;
    procedure a_call_reg(list: tasmlist; Reg: TRegister); override;
    { General purpose instructions }
    procedure a_op_const_reg(list: tasmlist; Op: TOpCG; size: tcgsize; a: aint; reg: TRegister); override;
    procedure a_op_reg_reg(list: tasmlist; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;
    procedure a_op_const_reg_reg(list: tasmlist; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister); override;
    procedure a_op_reg_reg_reg(list: tasmlist; op: TOpCg; size: tcgsize; src1, src2, dst: tregister); override;
    procedure a_op_const_reg_reg_checkoverflow(list: tasmlist; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister; setflags: boolean; var ovloc: tlocation); override;
    procedure a_op_reg_reg_reg_checkoverflow(list: tasmlist; op: TOpCg; size: tcgsize; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation); override;
    { move instructions }
    procedure a_load_const_reg(list: tasmlist; size: tcgsize; a: aint; reg: tregister); override;
    procedure a_load_const_ref(list: tasmlist; size: tcgsize; a: aint; const ref: TReference); override;
    procedure a_load_reg_ref(list: tasmlist; FromSize, ToSize: TCgSize; reg: TRegister; const ref: TReference); override;
    procedure a_load_ref_reg(list: tasmlist; FromSize, ToSize: TCgSize; const ref: TReference; reg: tregister); override;
    procedure a_load_reg_reg(list: tasmlist; FromSize, ToSize: TCgSize; reg1, reg2: tregister); override;
    procedure a_loadaddr_ref_reg(list: tasmlist; const ref: TReference; r: tregister); override;
    { fpu move instructions }
    procedure a_loadfpu_reg_reg(list: tasmlist; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
    procedure a_loadfpu_ref_reg(list: tasmlist; fromsize, tosize: tcgsize; const ref: TReference; reg: tregister); override;
    procedure a_loadfpu_reg_ref(list: tasmlist; fromsize, tosize: tcgsize; reg: tregister; const ref: TReference); override;
    { comparison operations }
    procedure a_cmp_const_reg_label(list: tasmlist; size: tcgsize; cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel); override;
    procedure a_cmp_reg_reg_label(list: tasmlist; size: tcgsize; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;
    procedure a_jmp_always(List: tasmlist; l: TAsmLabel); override;
    procedure a_jmp_name(list: tasmlist; const s: string); override;
    procedure a_jmp_cond(list: tasmlist; cond: TOpCmp; l: tasmlabel); { override;}
    procedure g_overflowCheck(List: tasmlist; const Loc: TLocation; def: TDef); override;
    procedure g_overflowCheck_loc(List: tasmlist; const Loc: TLocation; def: TDef; ovloc: tlocation); override;
    procedure g_proc_entry(list: tasmlist; localsize: longint; nostackframe: boolean); override;
    procedure g_proc_exit(list: tasmlist; parasize: longint; nostackframe: boolean); override;
    procedure g_concatcopy(list: tasmlist; const Source, dest: treference; len: aint); override;
    procedure g_concatcopy_unaligned(list: tasmlist; const Source, dest: treference; len: aint); override;
    procedure g_concatcopy_move(list: tasmlist; const Source, dest: treference; len: aint);
    procedure g_intf_wrapper(list: tasmlist; procdef: tprocdef; const labelname: string; ioffset: longint); override;
  end;

  TCg64MPSel = class(tcg64f32)
  public
    procedure a_load64_reg_ref(list: tasmlist; reg: tregister64; const ref: treference); override;
    procedure a_load64_ref_reg(list: tasmlist; const ref: treference; reg: tregister64); override;
    procedure a_param64_ref(list: tasmlist; const r: treference; const paraloc: tcgpara); override;
    procedure a_op64_reg_reg(list: tasmlist; op: TOpCG; size: tcgsize; regsrc, regdst: TRegister64); override;
    procedure a_op64_const_reg(list: tasmlist; op: TOpCG; size: tcgsize; Value: int64; regdst: TRegister64); override;
    procedure a_op64_const_reg_reg(list: tasmlist; op: TOpCG; size: tcgsize; Value: int64; regsrc, regdst: tregister64); override;
    procedure a_op64_reg_reg_reg(list: tasmlist; op: TOpCG; size: tcgsize; regsrc1, regsrc2, regdst: tregister64); override;
    procedure a_op64_const_reg_reg_checkoverflow(list: tasmlist; op: TOpCG; size: tcgsize; Value: int64; regsrc, regdst: tregister64; setflags: boolean; var ovloc: tlocation); override;
    procedure a_op64_reg_reg_reg_checkoverflow(list: tasmlist; op: TOpCG; size: tcgsize; regsrc1, regsrc2, regdst: tregister64; setflags: boolean; var ovloc: tlocation); override;
  end;

  procedure create_codegen;

implementation

uses
  globals, verbose, systems, cutils,
  paramgr, fmodule,
  tgobj,
  procinfo, cpupi;

var
  cgcpu_calc_stackframe_size: aint;


  function f_TOpCG2AsmOp(op: TOpCG; size: tcgsize): TAsmOp;
  begin
    if size = OS_32 then
      case op of
        OP_ADD:       { simple addition          }
          f_TOpCG2AsmOp := A_ADDU;
        OP_AND:       { simple logical and       }
          f_TOpCG2AsmOp := A_AND;
        OP_DIV:       { simple unsigned division }
          f_TOpCG2AsmOp := A_DIVU;
        OP_IDIV:      { simple signed division   }
          f_TOpCG2AsmOp := A_DIV;
        OP_IMUL:      { simple signed multiply   }
          f_TOpCG2AsmOp := A_MULT;
        OP_MUL:       { simple unsigned multiply }
          f_TOpCG2AsmOp := A_MULTU;
        OP_NEG:       { simple negate            }
          f_TOpCG2AsmOp := A_NEGU;
        OP_NOT:       { simple logical not       }
          f_TOpCG2AsmOp := A_NOT;
        OP_OR:        { simple logical or        }
          f_TOpCG2AsmOp := A_OR;
        OP_SAR:       { arithmetic shift-right   }
          f_TOpCG2AsmOp := A_SRA;
        OP_SHL:       { logical shift left       }
          f_TOpCG2AsmOp := A_SLL;
        OP_SHR:       { logical shift right      }
          f_TOpCG2AsmOp := A_SRL;
        OP_SUB:       { simple subtraction       }
          f_TOpCG2AsmOp := A_SUBU;
        OP_XOR:       { simple exclusive or      }
          f_TOpCG2AsmOp := A_XOR;
        else
          InternalError(2007070401);
      end{ case }
    else
      case op of
        OP_ADD:       { simple addition          }
          f_TOpCG2AsmOp := A_ADDU;
        OP_AND:       { simple logical and       }
          f_TOpCG2AsmOp := A_AND;
        OP_DIV:       { simple unsigned division }
          f_TOpCG2AsmOp := A_DIVU;
        OP_IDIV:      { simple signed division   }
          f_TOpCG2AsmOp := A_DIV;
        OP_IMUL:      { simple signed multiply   }
          f_TOpCG2AsmOp := A_MULT;
        OP_MUL:       { simple unsigned multiply }
          f_TOpCG2AsmOp := A_MULTU;
        OP_NEG:       { simple negate            }
          f_TOpCG2AsmOp := A_NEGU;
        OP_NOT:       { simple logical not       }
          f_TOpCG2AsmOp := A_NOT;
        OP_OR:        { simple logical or        }
          f_TOpCG2AsmOp := A_OR;
        OP_SAR:       { arithmetic shift-right   }
          f_TOpCG2AsmOp := A_SRA;
        OP_SHL:       { logical shift left       }
          f_TOpCG2AsmOp := A_SLL;
        OP_SHR:       { logical shift right      }
          f_TOpCG2AsmOp := A_SRL;
        OP_SUB:       { simple subtraction       }
          f_TOpCG2AsmOp := A_SUBU;
        OP_XOR:       { simple exclusive or      }
          f_TOpCG2AsmOp := A_XOR;
        else
          InternalError(2007010701);
      end;{ case }
  end;

  function f_TOpCG2AsmOp_ovf(op: TOpCG; size: tcgsize): TAsmOp;
  begin
    if size = OS_32 then
      case op of
        OP_ADD:       { simple addition          }
          f_TOpCG2AsmOp_ovf := A_ADD;
        OP_AND:       { simple logical and       }
          f_TOpCG2AsmOp_ovf := A_AND;
        OP_DIV:       { simple unsigned division }
          f_TOpCG2AsmOp_ovf := A_DIVU;
        OP_IDIV:      { simple signed division   }
          f_TOpCG2AsmOp_ovf := A_DIV;
        OP_IMUL:      { simple signed multiply   }
          f_TOpCG2AsmOp_ovf := A_MULO;
        OP_MUL:       { simple unsigned multiply }
          f_TOpCG2AsmOp_ovf := A_MULOU;
        OP_NEG:       { simple negate            }
          f_TOpCG2AsmOp_ovf := A_NEG;
        OP_NOT:       { simple logical not       }
          f_TOpCG2AsmOp_ovf := A_NOT;
        OP_OR:        { simple logical or        }
          f_TOpCG2AsmOp_ovf := A_OR;
        OP_SAR:       { arithmetic shift-right   }
          f_TOpCG2AsmOp_ovf := A_SRA;
        OP_SHL:       { logical shift left       }
          f_TOpCG2AsmOp_ovf := A_SLL;
        OP_SHR:       { logical shift right      }
          f_TOpCG2AsmOp_ovf := A_SRL;
        OP_SUB:       { simple subtraction       }
          f_TOpCG2AsmOp_ovf := A_SUB;
        OP_XOR:       { simple exclusive or      }
          f_TOpCG2AsmOp_ovf := A_XOR;
        else
          InternalError(2007070403);
      end{ case }
    else
      case op of
        OP_ADD:       { simple addition          }
          f_TOpCG2AsmOp_ovf := A_ADD;
        OP_AND:       { simple logical and       }
          f_TOpCG2AsmOp_ovf := A_AND;
        OP_DIV:       { simple unsigned division }
          f_TOpCG2AsmOp_ovf := A_DIVU;
        OP_IDIV:      { simple signed division   }
          f_TOpCG2AsmOp_ovf := A_DIV;
        OP_IMUL:      { simple signed multiply   }
          f_TOpCG2AsmOp_ovf := A_MULO;
        OP_MUL:       { simple unsigned multiply }
          f_TOpCG2AsmOp_ovf := A_MULOU;
        OP_NEG:       { simple negate            }
          f_TOpCG2AsmOp_ovf := A_NEG;
        OP_NOT:       { simple logical not       }
          f_TOpCG2AsmOp_ovf := A_NOT;
        OP_OR:        { simple logical or        }
          f_TOpCG2AsmOp_ovf := A_OR;
        OP_SAR:       { arithmetic shift-right   }
          f_TOpCG2AsmOp_ovf := A_SRA;
        OP_SHL:       { logical shift left       }
          f_TOpCG2AsmOp_ovf := A_SLL;
        OP_SHR:       { logical shift right      }
          f_TOpCG2AsmOp_ovf := A_SRL;
        OP_SUB:       { simple subtraction       }
          f_TOpCG2AsmOp_ovf := A_SUB;
        OP_XOR:       { simple exclusive or      }
          f_TOpCG2AsmOp_ovf := A_XOR;
        else
          InternalError(2007010703);
      end;{ case }
  end;

  function f_TOp64CG2AsmOp(op: TOpCG): TAsmOp;
  begin
    case op of
      OP_ADD:       { simple addition          }
        f_TOp64CG2AsmOp := A_DADDU;
      OP_AND:       { simple logical and       }
        f_TOp64CG2AsmOp := A_AND;
      OP_DIV:       { simple unsigned division }
        f_TOp64CG2AsmOp := A_DDIVU;
      OP_IDIV:      { simple signed division   }
        f_TOp64CG2AsmOp := A_DDIV;
      OP_IMUL:      { simple signed multiply   }
        f_TOp64CG2AsmOp := A_DMULO;
      OP_MUL:       { simple unsigned multiply }
        f_TOp64CG2AsmOp := A_DMULOU;
      OP_NEG:       { simple negate            }
        f_TOp64CG2AsmOp := A_DNEGU;
      OP_NOT:       { simple logical not       }
        f_TOp64CG2AsmOp := A_NOT;
      OP_OR:        { simple logical or        }
        f_TOp64CG2AsmOp := A_OR;
      OP_SAR:       { arithmetic shift-right   }
        f_TOp64CG2AsmOp := A_DSRA;
      OP_SHL:       { logical shift left       }
        f_TOp64CG2AsmOp := A_DSLL;
      OP_SHR:       { logical shift right      }
        f_TOp64CG2AsmOp := A_DSRL;
      OP_SUB:       { simple subtraction       }
        f_TOp64CG2AsmOp := A_DSUBU;
      OP_XOR:       { simple exclusive or      }
        f_TOp64CG2AsmOp := A_XOR;
      else
        InternalError(2007010702);
    end;{ case }
  end;



procedure TCgMPSel.make_simple_ref(list: tasmlist; var ref: treference);
var
  tmpreg, tmpreg1: tregister;
  tmpref: treference;
begin
  tmpreg := NR_NO;
  { Be sure to have a base register }
  if (ref.base = NR_NO) then
  begin
    ref.base  := ref.index;
    ref.index := NR_NO;
  end;
  if (cs_create_pic in current_settings.moduleswitches) and
    assigned(ref.symbol) then
  begin
    tmpreg := GetIntRegister(list, OS_INT);
    reference_reset(tmpref,sizeof(aint));
    tmpref.symbol  := ref.symbol;
    tmpref.refaddr := addr_pic;
    if not (pi_needs_got in current_procinfo.flags) then
      internalerror(200501161);
    tmpref.index := current_procinfo.got;
    list.concat(taicpu.op_reg_ref(A_LW, tmpreg, tmpref));
    ref.symbol := nil;
    if (ref.index <> NR_NO) then
    begin
      list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg, ref.index, tmpreg));
      ref.index := tmpreg;
    end
    else
    begin
      if ref.base <> NR_NO then
        ref.index := tmpreg
      else
        ref.base  := tmpreg;
    end;
  end;
  { When need to use LUI, do it first }
  if assigned(ref.symbol) or
    (ref.offset < simm16lo) or
    (ref.offset > simm16hi) then
  begin
    tmpreg := GetIntRegister(list, OS_INT);
    reference_reset(tmpref,sizeof(aint));
    tmpref.symbol  := ref.symbol;
    tmpref.offset  := ref.offset;
    tmpref.refaddr := addr_high;
    list.concat(taicpu.op_reg_ref(A_LUI, tmpreg, tmpref));
    if (ref.offset = 0) and (ref.index = NR_NO) and
      (ref.base = NR_NO) then
    begin
      ref.refaddr := addr_low;
    end
    else
    begin
      { Load the low part is left }
      tmpref.refaddr := addr_low;
      list.concat(taicpu.op_reg_reg_ref(A_ADDIU, tmpreg, tmpreg, tmpref));
      ref.offset := 0;
      { symbol is loaded }
      ref.symbol := nil;
    end;
    if (ref.index <> NR_NO) then
    begin
      list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg, ref.index, tmpreg));
      ref.index := tmpreg;
    end
    else
    begin
      if ref.base <> NR_NO then
        ref.index := tmpreg
      else
        ref.base  := tmpreg;
    end;
  end;
  if (ref.base <> NR_NO) then
  begin
    if (ref.index <> NR_NO) and (ref.offset = 0) then
    begin
        tmpreg1 := GetIntRegister(list, OS_INT);
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg1, ref.base, ref.index));
        ref.base  := tmpreg1;
        ref.index := NR_NO;
    end
    else if (ref.index <> NR_NO) and
      ((ref.offset <> 0) or assigned(ref.symbol)) then
    begin
      if tmpreg = NR_NO then
        tmpreg := GetIntRegister(list, OS_INT);
      list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg, ref.base, ref.index));
      ref.base  := tmpreg;
      ref.index := NR_NO;
    end;
  end;
end;

procedure TCgMPSel.make_simple_ref_fpu(list: tasmlist; var ref: treference);
var
  tmpreg, tmpreg1: tregister;
  tmpref: treference;
begin
  tmpreg := NR_NO;
  { Be sure to have a base register }
  if (ref.base = NR_NO) then
  begin
    ref.base  := ref.index;
    ref.index := NR_NO;
  end;
  if (cs_create_pic in current_settings.moduleswitches) and
    assigned(ref.symbol) then
  begin
    tmpreg := GetIntRegister(list, OS_INT);
    reference_reset(tmpref,sizeof(aint));
    tmpref.symbol  := ref.symbol;
    tmpref.refaddr := addr_pic;
    if not (pi_needs_got in current_procinfo.flags) then
      internalerror(200501161);
    tmpref.index := current_procinfo.got;
    list.concat(taicpu.op_reg_ref(A_LW, tmpreg, tmpref));
    ref.symbol := nil;
    if (ref.index <> NR_NO) then
    begin
      list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg, ref.index, tmpreg));
      ref.index := tmpreg;
    end
    else
    begin
      if ref.base <> NR_NO then
        ref.index := tmpreg
      else
        ref.base  := tmpreg;
    end;
  end;
  { When need to use LUI, do it first }
  if (not assigned(ref.symbol)) and (ref.index = NR_NO) and
    (ref.offset > simm16lo + 1000) and (ref.offset < simm16hi - 1000)
  then
    exit;

  tmpreg1 := GetIntRegister(list, OS_INT);
  if assigned(ref.symbol) then
  begin
    reference_reset(tmpref,sizeof(aint));
    tmpref.symbol  := ref.symbol;
    tmpref.offset  := ref.offset;
    tmpref.refaddr := addr_high;
    list.concat(taicpu.op_reg_ref(A_LUI, tmpreg1, tmpref));
    { Load the low part }

    tmpref.refaddr := addr_low;
    list.concat(taicpu.op_reg_reg_ref(A_ADDIU, tmpreg1, tmpreg1, tmpref));
    { symbol is loaded }
    ref.symbol := nil;
  end
  else
    list.concat(taicpu.op_reg_const(A_LI, tmpreg1, ref.offset));

  if (ref.index <> NR_NO) then
  begin
    list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg1, ref.index, tmpreg1));
    ref.index := NR_NO
  end;
  if ref.base <> NR_NO then
    list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg1, ref.base, tmpreg1));
  ref.base := tmpreg1;
  ref.offset := 0;
end;

procedure TCgMPSel.handle_load_store(list: tasmlist; isstore: boolean; op: tasmop; reg: tregister; ref: treference);
begin
  make_simple_ref(list, ref);
  list.concat(taicpu.op_reg_ref(op, reg, ref));
end;

procedure TCgMPSel.handle_load_store_fpu(list: tasmlist; isstore: boolean; op: tasmop; reg: tregister; ref: treference);
begin
  make_simple_ref_fpu(list, ref);
  list.concat(taicpu.op_reg_ref(op, reg, ref));
end;


procedure TCgMPSel.handle_reg_const_reg(list: tasmlist; op: Tasmop; src: tregister; a: aint; dst: tregister);
var
  tmpreg: tregister;
begin
  if (a < simm16lo) or
    (a > simm16hi) then
  begin
    tmpreg := GetIntRegister(list, OS_INT);
    a_load_const_reg(list, OS_INT, a, tmpreg);
    list.concat(taicpu.op_reg_reg_reg(op, dst, src, tmpreg));
  end
  else
    list.concat(taicpu.op_reg_reg_const(op, dst, src, a));
end;


{****************************************************************************
                              Assembler code
****************************************************************************}

procedure TCgMPSel.init_register_allocators;
begin
  inherited init_register_allocators;

  if (cs_create_pic in current_settings.moduleswitches) and
    (pi_needs_got in current_procinfo.flags) then
  begin
    current_procinfo.got := NR_GP;
    rg[R_INTREGISTER]    := Trgcpu.Create(R_INTREGISTER, R_SUBD,
      [RS_R4, RS_R5, RS_R6, RS_R7, RS_R8, RS_R9, RS_R10, RS_R11,
       RS_R12, RS_R13, RS_R14 {, RS_R15 for tmp_const in ncpuadd.pas} {, RS_R24, RS_R25}],
      first_int_imreg, []);
  end
  else
    rg[R_INTREGISTER] := Trgcpu.Create(R_INTREGISTER, R_SUBD,
      [RS_R4, RS_R5, RS_R6, RS_R7, RS_R8, RS_R9, RS_R10, RS_R11,
       RS_R12, RS_R13, RS_R14 {, RS_R15 for tmp_const in ncpuadd.pas} {, RS_R24=VMT, RS_R25=PIC jump}],
      first_int_imreg, []);

  rg[R_FPUREGISTER] := trgcpu.Create(R_FPUREGISTER, R_SUBFS{R_SUBFD},
    [RS_F0, RS_F2, RS_F4, RS_F6,
    RS_F8, RS_F10, RS_F12, RS_F14,
    RS_F16, RS_F18, RS_F20, RS_F22,
    RS_F24, RS_F26, RS_F28, RS_F30],
    first_fpu_imreg, []);
end;



procedure TCgMPSel.done_register_allocators;
begin
  rg[R_INTREGISTER].Free;
  rg[R_FPUREGISTER].Free;
  inherited done_register_allocators;
end;


function TCgMPSel.getfpuregister(list: tasmlist; size: Tcgsize): Tregister;
begin
  if size = OS_F64 then
    Result := rg[R_FPUREGISTER].getregister(list, R_SUBFD)
  else
    Result := rg[R_FPUREGISTER].getregister(list, R_SUBFS);
end;


procedure TCgMPSel.a_param_const(list: tasmlist; size: tcgsize; a: aint; const paraloc: TCGPara);
var
  Ref: TReference;
begin
  paraloc.check_simple_location;
  case paraloc.location^.loc of
    LOC_REGISTER, LOC_CREGISTER:
      a_load_const_reg(list, size, a, paraloc.location^.Register);
    LOC_REFERENCE:
    begin
      with paraloc.location^.Reference do
      begin
        if (Index = NR_SP) and (Offset < Target_info.first_parm_offset) then
          InternalError(2002081104);
        reference_reset_base(ref, index, offset, sizeof(aint));
      end;
      a_load_const_ref(list, size, a, ref);
    end;
    else
      InternalError(2002122200);
  end;
end;


procedure TCgMPSel.a_param_ref(list: tasmlist; sz: TCgSize; const r: TReference; const paraloc: TCGPara);
var
  ref:    treference;
  tmpreg: TRegister;
begin
  paraloc.check_simple_location;
  with paraloc.location^ do
  begin
    case loc of
      LOC_REGISTER, LOC_CREGISTER:
        a_load_ref_reg(list, sz, sz, r, Register);
      LOC_REFERENCE:
      begin
        with Reference do
        begin
          if (Index = NR_SP) and (Offset < Target_info.first_parm_offset) then
            InternalError(2002081104);
          reference_reset_base(ref, index, offset, sizeof(aint));
        end;
        tmpreg := GetIntRegister(list, OS_INT);
        a_load_ref_reg(list, sz, sz, r, tmpreg);
        a_load_reg_ref(list, sz, sz, tmpreg, ref);
      end;
      else
        internalerror(2002081103);
    end;
  end;
end;


procedure TCgMPSel.a_paramaddr_ref(list: tasmlist; const r: TReference; const paraloc: TCGPara);
var
  Ref:    TReference;
  TmpReg: TRegister;
begin
  paraloc.check_simple_location;
  with paraloc.location^ do
  begin
    case loc of
      LOC_REGISTER, LOC_CREGISTER:
        a_loadaddr_ref_reg(list, r, Register);
      LOC_REFERENCE:
      begin
        reference_reset(ref,sizeof(aint));
        ref.base   := reference.index;
        ref.offset := reference.offset;
        tmpreg     := GetAddressRegister(list);
        a_loadaddr_ref_reg(list, r, tmpreg);
        a_load_reg_ref(list, OS_ADDR, OS_ADDR, tmpreg, ref);
      end;
      else
        internalerror(2002080701);
    end;
  end;
end;


procedure TCgMPSel.a_paramfpu_ref(list: tasmlist; size: tcgsize; const ref: treference; const paraloc: TCGPara);
var
  href, href2: treference;
  hloc: pcgparalocation;
begin
  href := ref;
  hloc := paraloc.location;
  while assigned(hloc) do
  begin
    case hloc^.loc of
      LOC_REGISTER:
        a_load_ref_reg(list, hloc^.size, hloc^.size, href, hloc^.Register);
      LOC_REFERENCE:
      begin
        reference_reset_base(href2, hloc^.reference.index, hloc^.reference.offset, sizeof(aint));
        a_load_ref_ref(list, hloc^.size, hloc^.size, href, href2);
      end;
      else
        internalerror(200408241);
    end;
    Inc(href.offset, tcgsize2size[hloc^.size]);
    hloc := hloc^.Next;
  end;
end;


procedure TCgMPSel.a_paramfpu_reg(list: tasmlist; size: tcgsize; const r: tregister; const paraloc: TCGPara);
var
  href: treference;
begin
  tg.GetTemp(list, TCGSize2Size[size], sizeof(aint), tt_normal, href);
  a_loadfpu_reg_ref(list, size, size, r, href);
  a_paramfpu_ref(list, size, href, paraloc);
  tg.Ungettemp(list, href);
end;


procedure TCgMPSel.a_call_name(list: tasmlist; const s: string; weak: boolean);
begin
  list.concat(taicpu.op_sym(A_JAL,current_asmdata.RefAsmSymbol(s)));
  { Delay slot }
  list.concat(taicpu.op_none(A_NOP));
end;


procedure TCgMPSel.a_call_reg(list: tasmlist; Reg: TRegister);
begin
  list.concat(taicpu.op_reg(A_JALR, reg));
  { Delay slot }
  list.concat(taicpu.op_none(A_NOP));
end;


{********************** load instructions ********************}

procedure TCgMPSel.a_load_const_reg(list: tasmlist; size: TCGSize; a: aint; reg: TRegister);
begin
  if (a = 0) then
    list.concat(taicpu.op_reg_reg(A_MOVE, reg, NR_R0))
  { LUI allows to set the upper 16 bits, so we'll take full advantage of it }
  else if (a and aint($ffff)) = 0 then
    list.concat(taicpu.op_reg_const(A_LUI, reg, a shr 16))
  else if (a >= simm16lo) and (a <= simm16hi) then
    list.concat(taicpu.op_reg_reg_const(A_ADDIU, reg, NR_R0, a))
  else if (a>=0) and (a <= 65535) then
    list.concat(taicpu.op_reg_reg_const(A_ORI, reg, NR_R0, a))
  else
  begin
    list.concat(taicpu.op_reg_const(A_LI, reg, a ));
  end;
end;


procedure TCgMPSel.a_load_const_ref(list: tasmlist; size: tcgsize; a: aint; const ref: TReference);
begin
  if a = 0 then
    a_load_reg_ref(list, size, size, NR_R0, ref)
  else
    inherited a_load_const_ref(list, size, a, ref);
end;


procedure TCgMPSel.a_load_reg_ref(list: tasmlist; FromSize, ToSize: TCGSize; reg: tregister; const Ref: TReference);
var
  op: tasmop;
begin

  if (TCGSize2Size[fromsize] >= TCGSize2Size[tosize]) then
    fromsize := tosize;
  case fromsize of
    { signed integer registers }
    OS_8,
    OS_S8:
      Op := A_SB;
    OS_16,
    OS_S16:
      Op := A_SH;
    OS_32,
    OS_S32:
      Op := A_SW;
    else
      InternalError(2002122100);
  end;
  handle_load_store(list, True, op, reg, ref);
end;


procedure TCgMPSel.a_load_ref_reg(list: tasmlist; FromSize, ToSize: TCgSize; const ref: TReference; reg: tregister);
var
  op: tasmop;
begin
  if (TCGSize2Size[fromsize] >= TCGSize2Size[tosize]) then
    fromsize := tosize;
  case fromsize of
    OS_S8:
      Op := A_LB;{Load Signed Byte}
    OS_8:
      Op := A_LBU;{Load Unsigned Byte}
    OS_S16:
      Op := A_LH;{Load Signed Halfword}
    OS_16:
      Op := A_LHU;{Load Unsigned Halfword}
    OS_S32:
      Op := A_LW;{Load Word}
    OS_32:
      Op := A_LW;//A_LWU;{Load Unsigned Word}
    OS_S64,
    OS_64:
      Op := A_LD;{Load a Long Word}
    else
      InternalError(2002122101);
  end;
  handle_load_store(list, False, op, reg, ref);
end;


procedure TCgMPSel.a_load_reg_reg(list: tasmlist; fromsize, tosize: tcgsize; reg1, reg2: tregister);
var
  instr: taicpu;
begin
  if (tcgsize2size[tosize] < tcgsize2size[fromsize]) or
    (
    (tcgsize2size[tosize] = tcgsize2size[fromsize]) and
    (tosize <> fromsize) and not (fromsize in [OS_32, OS_S32])
    ) then
  begin
    case tosize of
      OS_8:
        a_op_const_reg_reg(list, OP_AND, tosize, $ff, reg1, reg2);
      OS_16:
        a_op_const_reg_reg(list, OP_AND, tosize, $ffff, reg1, reg2);
      OS_32,
      OS_S32:
      begin
        instr := taicpu.op_reg_reg(A_MOVE, reg2, reg1);
        list.Concat(instr);
                  { Notify the register allocator that we have written a move instruction so
                   it can try to eliminate it. }
        add_move_instruction(instr);
      end;
      OS_S8:
      begin
        list.concat(taicpu.op_reg_reg_const(A_SLL, reg2, reg1, 24));
        list.concat(taicpu.op_reg_reg_const(A_SRA, reg2, reg2, 24));
      end;
      OS_S16:
      begin
        list.concat(taicpu.op_reg_reg_const(A_SLL, reg2, reg1, 16));
        list.concat(taicpu.op_reg_reg_const(A_SRA, reg2, reg2, 16));
      end;
      else
        internalerror(2002090901);
    end;
  end
  else
  begin
    if reg1 <> reg2 then
    begin
      { same size, only a register mov required }
      instr := taicpu.op_reg_reg(A_MOVE, reg2, reg1);
      list.Concat(instr);
//                { Notify the register allocator that we have written a move instruction so
//                  it can try to eliminate it. }

      add_move_instruction(instr);
    end;
  end;
end;


procedure TCgMPSel.a_loadaddr_ref_reg(list: tasmlist; const ref: TReference; r: tregister);
var
  tmpref, href: treference;
  hreg, tmpreg: tregister;
  r_used: boolean;
begin
  r_used := false;
  href := ref;
  if (href.base = NR_NO) and (href.index <> NR_NO) then
    internalerror(200306171);

  if (cs_create_pic in current_settings.moduleswitches) and
    assigned(href.symbol) then
  begin
    tmpreg := r; //GetIntRegister(list, OS_ADDR);
    r_used := true;
    reference_reset(tmpref,sizeof(aint));
    tmpref.symbol  := href.symbol;
    tmpref.refaddr := addr_pic;
    if not (pi_needs_got in current_procinfo.flags) then
      internalerror(200501161);
    tmpref.base := current_procinfo.got;
    list.concat(taicpu.op_reg_ref(A_LW, tmpreg, tmpref));
    href.symbol := nil;
    if (href.index <> NR_NO) then
    begin
      list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg, href.index, tmpreg));
      href.index := tmpreg;
    end
    else
    begin
      if href.base <> NR_NO then
        href.index := tmpreg
      else
        href.base  := tmpreg;
    end;
  end;


  if assigned(href.symbol) or
    (href.offset < simm16lo) or
    (href.offset > simm16hi) then
  begin
    if (href.base = NR_NO) and (href.index = NR_NO) then
      hreg := r
    else
      hreg := GetAddressRegister(list);
    reference_reset(tmpref,sizeof(aint));
    tmpref.symbol  := href.symbol;
    tmpref.offset  := href.offset;
    tmpref.refaddr := addr_high;
    list.concat(taicpu.op_reg_ref(A_LUI, hreg, tmpref));
    { Only the low part is left }
    tmpref.refaddr := addr_low;
    list.concat(taicpu.op_reg_reg_ref(A_ADDIU, hreg, hreg, tmpref));
    if href.base <> NR_NO then
    begin
      if href.index <> NR_NO then
      begin
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, hreg, href.base, hreg));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, r, hreg, href.index));
      end
      else
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, r, hreg, href.base));
    end;
  end
  else
  { At least small offset, maybe base and maybe index }
  if  (href.offset >= simm16lo) and
    (href.offset <= simm16hi) then
  begin
    if href.index <> NR_NO then   { Both base and index }
    begin
      if href.offset = 0 then
      begin
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, r, href.base, href.index));
      end
      else
      begin
        if r_used then
          hreg := GetAddressRegister(list)
        else
          hreg := r;
        list.concat(taicpu.op_reg_reg_const(A_ADDIU, hreg, href.base, href.offset));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, r, hreg, href.index));
      end
    end
    else if href.base <> NR_NO then   { Only base }
    begin
      list.concat(taicpu.op_reg_reg_const(A_ADDIU, r, href.base, href.offset));
    end
    else
      { only offset, can be generated by absolute }
      a_load_const_reg(list, OS_ADDR, href.offset, r);
  end
  else
    internalerror(200703111);
end;

procedure TCgMPSel.a_loadfpu_reg_reg(list: tasmlist; fromsize, tosize: tcgsize; reg1, reg2: tregister);
const
  FpuMovInstr: array[OS_F32..OS_F64] of TAsmOp =
    (A_MOV_S, A_MOV_D);
var
  instr: taicpu;
begin
  if reg1 <> reg2 then
  begin
    instr := taicpu.op_reg_reg(fpumovinstr[tosize], reg2, reg1);
    list.Concat(instr);
    { Notify the register allocator that we have written a move instruction so
      it can try to eliminate it. }
    add_move_instruction(instr);
  end;
end;


procedure TCgMPSel.a_loadfpu_ref_reg(list: tasmlist; fromsize, tosize: tcgsize; const ref: TReference; reg: tregister);
var
  tmpref: treference;
  tmpreg: tregister;
begin
  case tosize of
    OS_F32:
      handle_load_store_fpu(list, False, A_LWC1, reg, ref);
    OS_F64:
      handle_load_store_fpu(list, False, A_LDC1, reg, ref);
    else
      InternalError(2007042701);
  end;
end;

procedure TCgMPSel.a_loadfpu_reg_ref(list: tasmlist; fromsize, tosize: tcgsize; reg: tregister; const ref: TReference);
var
  tmpref: treference;
  tmpreg: tregister;
begin
  case tosize of
    OS_F32:
      handle_load_store_fpu(list, True, A_SWC1, reg, ref);
    OS_F64:
      handle_load_store_fpu(list, True, A_SDC1, reg, ref);
    else
      InternalError(2007042702);
  end;
end;

procedure TCgMPSel.a_op_const_reg(list: tasmlist; Op: TOpCG; size: tcgsize; a: aint; reg: TRegister);
var
  power: longint;
  tmpreg1: tregister;
begin
  if ((op = OP_MUL) or (op = OP_IMUL)) then
  begin
    if ispowerof2(a, power) then
    begin
      { can be done with a shift }
      if power < 32 then
      begin
        list.concat(taicpu.op_reg_reg_const(A_SLL, reg, reg, power));
        exit;
      end;
    end;
  end;
  if ((op = OP_SUB) or (op = OP_ADD)) then
  begin
    if (a = 0) then
      exit;
  end;

  if Op in [OP_NEG, OP_NOT] then
    internalerror(200306011);
  if (a = 0) then
  begin
    if (Op = OP_IMUL) or (Op = OP_MUL) then
      list.concat(taicpu.op_reg_reg(A_MOVE, reg, NR_R0))
    else
      list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp(op, size), reg, reg, NR_R0))
  end
  else
  begin
    if op = OP_IMUL then
    begin
      tmpreg1 := GetIntRegister(list, OS_INT);
      a_load_const_reg(list, OS_INT, a, tmpreg1);
      list.concat(taicpu.op_reg_reg(A_MULT, reg, tmpreg1));
      list.concat(taicpu.op_reg(A_MFLO, reg));
    end
    else if op = OP_MUL then
    begin
      tmpreg1 := GetIntRegister(list, OS_INT);
      a_load_const_reg(list, OS_INT, a, tmpreg1);
      list.concat(taicpu.op_reg_reg(A_MULTU, reg, tmpreg1));
      list.concat(taicpu.op_reg(A_MFLO, reg));
    end
    else
      handle_reg_const_reg(list, f_TOpCG2AsmOp(op, size), reg, a, reg);
  end;
end;


procedure TCgMPSel.a_op_reg_reg(list: tasmlist; Op: TOpCG; size: TCGSize; src, dst: TRegister);
var
  a: aint;
begin
  case Op of
    OP_NEG:
      list.concat(taicpu.op_reg_reg(A_NEG, dst, src));
    OP_NOT:
    begin
      list.concat(taicpu.op_reg_reg(A_NOT, dst, src));
    end;
    else
    begin
      if op = OP_IMUL then
      begin
        list.concat(taicpu.op_reg_reg(A_MULT, dst, src));
        list.concat(taicpu.op_reg(A_MFLO, dst));
      end
      else if op = OP_MUL then
      begin
        list.concat(taicpu.op_reg_reg(A_MULTU, dst, src));
        list.concat(taicpu.op_reg(A_MFLO, dst));
      end
      else
      begin
        list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp(op, size), dst, dst, src));
      end;
    end;
  end;
end;


procedure TCgMPSel.a_op_const_reg_reg(list: tasmlist; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister);
var
  power: longint;
  tmpreg1: tregister;
begin
  case op of
    OP_MUL,
    OP_IMUL:
    begin
      if ispowerof2(a, power) then
      begin
        { can be done with a shift }
        if power < 32 then
          list.concat(taicpu.op_reg_reg_const(A_SLL, dst, src, power))
        else
          inherited a_op_const_reg_reg(list, op, size, a, src, dst);
        exit;
      end;
    end;
    OP_SUB,
    OP_ADD:
    begin
      if (a = 0) then
      begin
        a_load_reg_reg(list, size, size, src, dst);
        exit;
      end;
    end;
  end;
  if op = OP_IMUL then
  begin
    tmpreg1 := GetIntRegister(list, OS_INT);
    a_load_const_reg(list, OS_INT, a, tmpreg1);
    list.concat(taicpu.op_reg_reg(A_MULT, src, tmpreg1));
    list.concat(taicpu.op_reg(A_MFLO, dst));
  end
  else if op = OP_MUL then
  begin
    tmpreg1 := GetIntRegister(list, OS_INT);
    a_load_const_reg(list, OS_INT, a, tmpreg1);
    list.concat(taicpu.op_reg_reg(A_MULTU, src, tmpreg1));
    list.concat(taicpu.op_reg(A_MFLO, dst));
  end
  else
    handle_reg_const_reg(list, f_TOpCG2AsmOp(op, size), src, a, dst);
end;


procedure TCgMPSel.a_op_reg_reg_reg(list: tasmlist; op: TOpCg; size: tcgsize; src1, src2, dst: tregister);
begin

  list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp(op, size), dst, src2, src1));
end;


procedure TCgMPSel.a_op_const_reg_reg_checkoverflow(list: tasmlist; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister; setflags: boolean; var ovloc: tlocation);
var
  tmpreg1: tregister;
begin
  ovloc.loc := LOC_VOID;
  case op of
    OP_SUB,
    OP_ADD:
    begin
      if (a = 0) then
      begin
        a_load_reg_reg(list, size, size, src, dst);
        exit;
      end;
    end;
  end;{case}

  case op of
    OP_ADD:
      begin
        if setflags then
          handle_reg_const_reg(list, f_TOpCG2AsmOp_ovf(op, size), src, a, dst)
        else
          handle_reg_const_reg(list, f_TOpCG2AsmOp(op, size), src, a, dst);
      end;
    OP_SUB:
      begin
        if setflags then
          handle_reg_const_reg(list, f_TOpCG2AsmOp_ovf(op, size), src, a, dst)
        else
          handle_reg_const_reg(list, f_TOpCG2AsmOp(op, size), src, a, dst);
      end;
    OP_MUL:
      begin
        if setflags then
          handle_reg_const_reg(list, f_TOpCG2AsmOp_ovf(op, size), src, a, dst)
        else
        begin
          tmpreg1 := GetIntRegister(list, OS_INT);
          a_load_const_reg(list, OS_INT, a, tmpreg1);
          list.concat(taicpu.op_reg_reg(f_TOpCG2AsmOp(op, size),src, tmpreg1));
          list.concat(taicpu.op_reg(A_MFLO, dst));
        end;
      end;
    OP_IMUL:
      begin
        if setflags then
          handle_reg_const_reg(list, f_TOpCG2AsmOp_ovf(op, size), src, a, dst)
        else
        begin
          tmpreg1 := GetIntRegister(list, OS_INT);
          a_load_const_reg(list, OS_INT, a, tmpreg1);
          list.concat(taicpu.op_reg_reg(f_TOpCG2AsmOp(op, size),src, tmpreg1));
          list.concat(taicpu.op_reg(A_MFLO, dst));
        end;
      end;
    OP_XOR, OP_OR, OP_AND:
      begin
        handle_reg_const_reg(list, f_TOpCG2AsmOp_ovf(op, size), src, a, dst);
      end;
    else
      internalerror(2007012601);
  end;
end;


procedure TCgMPSel.a_op_reg_reg_reg_checkoverflow(list: tasmlist; op: TOpCg; size: tcgsize; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation);
begin
  ovloc.loc := LOC_VOID;
  case op of
    OP_ADD:
      begin
        if setflags then
          list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp_ovf(op, size), dst, src2, src1))
        else
          list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp(op, size), dst, src2, src1));
      end;
    OP_SUB:
      begin
        if setflags then
          list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp_ovf(op, size), dst, src2, src1))
        else
          list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp(op, size), dst, src2, src1));
      end;
    OP_MUL:
      begin
        if setflags then
          list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp_ovf(op, size), dst, src2, src1))
        else
        begin
          list.concat(taicpu.op_reg_reg(f_TOpCG2AsmOp(op, size), src2, src1));
          list.concat(taicpu.op_reg(A_MFLO, dst));
        end;
      end;
    OP_IMUL:
      begin
        if setflags then
          list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp_ovf(op, size), dst, src2, src1))
        else
        begin
          list.concat(taicpu.op_reg_reg(f_TOpCG2AsmOp(op, size), src2, src1));
          list.concat(taicpu.op_reg(A_MFLO, dst));
        end;
      end;
    OP_XOR, OP_OR, OP_AND:
      begin
        list.concat(taicpu.op_reg_reg_reg(f_TOpCG2AsmOp_ovf(op, size), dst, src2, src1));
      end;
    else
      internalerror(2007012602);
  end;
end;



{*************** compare instructructions ****************}

procedure TCgMPSel.a_cmp_const_reg_label(list: tasmlist; size: tcgsize; cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel);
var
  tmpreg: tregister;
begin
if a = 0 then
  tmpreg := NR_R0
else
begin
  tmpreg := GetIntRegister(list, OS_INT);
  list.concat(taicpu.op_reg_const(A_LI, tmpreg, a));
end;
  case cmp_op of
    OC_EQ:           { equality comparison              }
      list.concat(taicpu.op_reg_reg_sym(A_BEQ, reg, tmpreg, l));
    OC_GT:           { greater than (signed)            }
      list.concat(taicpu.op_reg_reg_sym(A_BGT, reg, tmpreg, l));
    OC_LT:           { less than (signed)               }
      list.concat(taicpu.op_reg_reg_sym(A_BLT, reg, tmpreg, l));
    OC_GTE:          { greater or equal than (signed)   }
      list.concat(taicpu.op_reg_reg_sym(A_BGE, reg, tmpreg, l));
    OC_LTE:          { less or equal than (signed)      }
      list.concat(taicpu.op_reg_reg_sym(A_BLE, reg, tmpreg, l));
    OC_NE:           { not equal                        }
      list.concat(taicpu.op_reg_reg_sym(A_BNE, reg, tmpreg, l));
    OC_BE:           { less or equal than (unsigned)    }
      list.concat(taicpu.op_reg_reg_sym(A_BLEU, reg, tmpreg, l));
    OC_B:            { less than (unsigned)             }
      list.concat(taicpu.op_reg_reg_sym(A_BLTU, reg, tmpreg, l));
    OC_AE:           { greater or equal than (unsigned) }
      list.concat(taicpu.op_reg_reg_sym(A_BGEU, reg, tmpreg, l));
    OC_A:             { greater than (unsigned)          }
      list.concat(taicpu.op_reg_reg_sym(A_BGTU, reg, tmpreg, l));
    else
      internalerror(200701071);
  end;
  list.Concat(TAiCpu.Op_none(A_NOP));
end;


procedure TCgMPSel.a_cmp_reg_reg_label(list: tasmlist; size: tcgsize; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
begin
  case cmp_op of
    OC_EQ:           { equality comparison              }
      list.concat(taicpu.op_reg_reg_sym(A_BEQ, reg2, reg1, l));
    OC_GT:           { greater than (signed)            }
      list.concat(taicpu.op_reg_reg_sym(A_BGT, reg2, reg1, l));
    OC_LT:           { less than (signed)               }
      list.concat(taicpu.op_reg_reg_sym(A_BLT, reg2, reg1, l));
    OC_GTE:          { greater or equal than (signed)   }
      list.concat(taicpu.op_reg_reg_sym(A_BGE, reg2, reg1, l));
    OC_LTE:          { less or equal than (signed)      }
      list.concat(taicpu.op_reg_reg_sym(A_BLE, reg2, reg1, l));
    OC_NE:           { not equal                        }
      list.concat(taicpu.op_reg_reg_sym(A_BNE, reg2, reg1, l));
    OC_BE:           { less or equal than (unsigned)    }
      list.concat(taicpu.op_reg_reg_sym(A_BLEU, reg2, reg1, l));
    OC_B:            { less than (unsigned)             }
      list.concat(taicpu.op_reg_reg_sym(A_BLTU, reg2, reg1, l));
    OC_AE:           { greater or equal than (unsigned) }
      list.concat(taicpu.op_reg_reg_sym(A_BGEU, reg2, reg1, l));
    OC_A:             { greater than (unsigned)          }
      list.concat(taicpu.op_reg_reg_sym(A_BGTU, reg2, reg1, l));
    else
      internalerror(200701072);
    end;{ case }
  list.Concat(TAiCpu.Op_none(A_NOP));
end;


procedure TCgMPSel.a_jmp_always(List: tasmlist; l: TAsmLabel);
begin
  List.Concat(TAiCpu.op_sym(A_J,l));
  { Delay slot }
  list.Concat(TAiCpu.Op_none(A_NOP));
end;


procedure TCgMPSel.a_jmp_name(list: tasmlist; const s: string);
begin
  List.Concat(TAiCpu.op_sym(A_J, current_asmdata.RefAsmSymbol(s)));
  { Delay slot }
  list.Concat(TAiCpu.Op_none(A_NOP));
end;


procedure TCgMPSel.a_jmp_cond(list: tasmlist; cond: TOpCmp; l: TAsmLabel);
begin
  internalerror(200701181);
end;


procedure TCgMPSel.g_overflowCheck(List: tasmlist; const Loc: TLocation; def: TDef);
begin
// this is an empty procedure
end;

procedure TCgMPSel.g_overflowCheck_loc(List: tasmlist; const Loc: TLocation; def: TDef; ovloc: tlocation);
begin

// this is an empty procedure

end;

{ *********** entry/exit code and address loading ************ }

procedure TCgMPSel.g_proc_entry(list: tasmlist; localsize: longint; nostackframe: boolean);
var
  regcounter, firstregfpu, firstreggpr: TSuperRegister;
  href:  treference;
  usesfpr, usesgpr, gotgot: boolean;
  regcounter2, firstfpureg: Tsuperregister;
  cond:  tasmcond;
  instr: taicpu;

begin
  if STK2_dummy <> 0 then
  begin
    list.concat(Taicpu.Op_reg_reg_const(A_P_STK2, STK2_PTR, STK2_PTR, -STK2_dummy));
  end;

  if nostackframe then
    exit;

    usesfpr := False;
  if not (po_assembler in current_procinfo.procdef.procoptions) then
    case target_info.abi of
      abi_powerpc_aix:
        firstfpureg := RS_F14;
      abi_powerpc_sysv:
        firstfpureg := RS_F14;
      abi_default:
        firstfpureg := RS_F14;
      else
        internalerror(2003122903);
    end;
  for regcounter := firstfpureg to RS_F31 do
  begin
    if regcounter in rg[R_FPUREGISTER].used_in_proc then
    begin
      usesfpr     := True;
      firstregfpu := regcounter;
      break;
    end;
  end;

  usesgpr := False;
  if not (po_assembler in current_procinfo.procdef.procoptions) then
    for regcounter2 := RS_R13 to RS_R31 do
    begin
      if regcounter2 in rg[R_INTREGISTER].used_in_proc then
      begin
        usesgpr     := True;
        firstreggpr := regcounter2;
        break;
      end;
    end;


  LocalSize := align(LocalSize, 8);

  cgcpu_calc_stackframe_size := LocalSize;
  list.concat(Taicpu.Op_reg_reg_const(A_P_FRAME, NR_FRAME_POINTER_REG, NR_R31, LocalSize));
  list.concat(Taicpu.op_none(A_P_SET_NOREORDER));
  list.concat(Taicpu.op_none(A_P_SET_NOMACRO));
  list.concat(Taicpu.Op_reg_reg_const(A_P_SW, NR_FRAME_POINTER_REG, NR_STACK_POINTER_REG, -LocalSize));
  list.concat(Taicpu.Op_reg_reg_const(A_P_SW, NR_R31, NR_STACK_POINTER_REG, -LocalSize + 4));
  list.concat(Taicpu.op_reg_reg(A_MOVE, NR_FRAME_POINTER_REG, NR_STACK_POINTER_REG));
  list.concat(Taicpu.Op_reg_reg_const(A_ADDIU, NR_STACK_POINTER_REG, NR_STACK_POINTER_REG, -LocalSize));
  if (cs_create_pic in current_settings.moduleswitches) and
    (pi_needs_got in current_procinfo.flags) then
  begin
    current_procinfo.got := NR_GP;
  end;
end;




procedure TCgMPSel.g_proc_exit(list: tasmlist; parasize: longint; nostackframe: boolean);
var
  hr: treference;
  localsize: aint;
begin
  localsize := cgcpu_calc_stackframe_size;
  if paramanager.ret_in_param(current_procinfo.procdef.returndef, current_procinfo.procdef.proccalloption) then
  begin
    reference_reset(hr,sizeof(aint));
    hr.offset  := 12;
    hr.refaddr := addr_full;
    if nostackframe then
    begin
      if STK2_dummy <> 0 then
        list.concat(Taicpu.Op_reg_reg_const(A_P_STK2, STK2_PTR, STK2_PTR, STK2_dummy));
      list.concat(taicpu.op_reg(A_J, NR_R31));
      list.concat(Taicpu.op_none(A_NOP));
    end
    else
    begin

      list.concat(Taicpu.Op_reg_reg_const(A_P_LW, NR_FRAME_POINTER_REG, NR_STACK_POINTER_REG, 0));
      list.concat(Taicpu.Op_reg_reg_const(A_P_LW, NR_R31, NR_STACK_POINTER_REG, 4));
      list.concat(Taicpu.Op_reg_reg_const(A_ADDIU, NR_STACK_POINTER_REG, NR_STACK_POINTER_REG, localsize));
      if STK2_dummy <> 0 then
        list.concat(Taicpu.Op_reg_reg_const(A_P_STK2, STK2_PTR, STK2_PTR, STK2_dummy));
      list.concat(taicpu.op_reg(A_J, NR_R31));
      list.concat(Taicpu.op_none(A_NOP));
      list.concat(Taicpu.op_none(A_P_SET_MACRO));
      list.concat(Taicpu.op_none(A_P_SET_REORDER));

    end;
  end
  else
  begin
    if nostackframe then
    begin
     if STK2_dummy <> 0 then
        list.concat(Taicpu.Op_reg_reg_const(A_P_STK2, STK2_PTR, STK2_PTR, STK2_dummy));
       list.concat(taicpu.op_reg(A_J, NR_R31));
      list.concat(Taicpu.op_none(A_NOP));
      list.concat(Taicpu.op_none(A_P_SET_MACRO));
      list.concat(Taicpu.op_none(A_P_SET_REORDER));
    end
    else
    begin
      list.concat(Taicpu.Op_reg_reg_const(A_P_LW, NR_FRAME_POINTER_REG, NR_STACK_POINTER_REG, 0));
      list.concat(Taicpu.Op_reg_reg_const(A_P_LW, NR_R31, NR_STACK_POINTER_REG, 4));
      list.concat(Taicpu.Op_reg_reg_const(A_ADDIU, NR_STACK_POINTER_REG, NR_STACK_POINTER_REG, localsize));
     if STK2_dummy <> 0 then
        list.concat(Taicpu.Op_reg_reg_const(A_P_STK2, STK2_PTR, STK2_PTR, STK2_dummy));
       list.concat(taicpu.op_reg(A_J, NR_R31));
      list.concat(Taicpu.op_none(A_NOP));
      list.concat(Taicpu.op_none(A_P_SET_MACRO));
      list.concat(Taicpu.op_none(A_P_SET_REORDER));
    end;
  end;
end;



{ ************* concatcopy ************ }

procedure TCgMPSel.g_concatcopy_move(list: tasmlist; const Source, dest: treference; len: aint);
var
  paraloc1, paraloc2, paraloc3: TCGPara;
begin
  paraloc1.init;
  paraloc2.init;
  paraloc3.init;
  paramanager.getintparaloc(pocall_default, 1, paraloc1);
  paramanager.getintparaloc(pocall_default, 2, paraloc2);
  paramanager.getintparaloc(pocall_default, 3, paraloc3);
  paramanager.allocparaloc(list, paraloc3);
  a_param_const(list, OS_INT, len, paraloc3);
  paramanager.allocparaloc(list, paraloc2);
  a_paramaddr_ref(list, dest, paraloc2);
  paramanager.allocparaloc(list, paraloc2);
  a_paramaddr_ref(list, Source, paraloc1);
  paramanager.freeparaloc(list, paraloc3);
  paramanager.freeparaloc(list, paraloc2);
  paramanager.freeparaloc(list, paraloc1);
  alloccpuregisters(list, R_INTREGISTER, paramanager.get_volatile_registers_int(pocall_default));
  alloccpuregisters(list, R_FPUREGISTER, paramanager.get_volatile_registers_fpu(pocall_default));
  a_call_name(list, 'FPC_MOVE', false);
  dealloccpuregisters(list, R_FPUREGISTER, paramanager.get_volatile_registers_fpu(pocall_default));
  dealloccpuregisters(list, R_INTREGISTER, paramanager.get_volatile_registers_int(pocall_default));
  paraloc3.done;
  paraloc2.done;
  paraloc1.done;
end;


procedure TCgMPSel.g_concatcopy(list: tasmlist; const Source, dest: treference; len: aint);
var
  tmpreg1, hreg, countreg: TRegister;
  src, dst: TReference;
  lab:      tasmlabel;
  Count, count2: aint;
begin
  if len > high(longint) then
    internalerror(2002072704);
  { anybody wants to determine a good value here :)? }
  if len > 100 then
    g_concatcopy_move(list, Source, dest, len)
  else
  begin
    reference_reset(src,sizeof(aint));
    reference_reset(dst,sizeof(aint));
    { load the address of source into src.base }
    src.base := GetAddressRegister(list);
    a_loadaddr_ref_reg(list, Source, src.base);
    { load the address of dest into dst.base }
    dst.base := GetAddressRegister(list);
    a_loadaddr_ref_reg(list, dest, dst.base);
    { generate a loop }
    Count := len div 4;
    if Count > 4 then
    begin
      { the offsets are zero after the a_loadaddress_ref_reg and just }
      { have to be set to 8. I put an Inc there so debugging may be   }
      { easier (should offset be different from zero here, it will be }
      { easy to notice in the generated assembler                     }
      countreg := GetIntRegister(list, OS_INT);
      tmpreg1  := GetIntRegister(list, OS_INT);
      a_load_const_reg(list, OS_INT, Count, countreg);
      { explicitely allocate R_O0 since it can be used safely here }
      { (for holding date that's being copied)                    }
      current_asmdata.getjumplabel(lab);
      a_label(list, lab);
      list.concat(taicpu.op_reg_ref(A_LW, tmpreg1, src));
      list.concat(taicpu.op_reg_ref(A_SW, tmpreg1, dst));
      list.concat(taicpu.op_reg_reg_const(A_ADDIU, src.base, src.base, 4));
      list.concat(taicpu.op_reg_reg_const(A_ADDIU, dst.base, dst.base, 4));
      list.concat(taicpu.op_reg_reg_const(A_ADDIU, countreg, countreg, -1));
      list.concat(taicpu.op_reg_sym(A_BGTZ, countreg, lab));
      list.concat(taicpu.op_none(A_NOP));
      len := len mod 4;
    end;
    { unrolled loop }
    Count := len div 4;
    if Count > 0 then
    begin
      tmpreg1 := GetIntRegister(list, OS_INT);
      for count2 := 1 to Count do
      begin
        list.concat(taicpu.op_reg_ref(A_LW, tmpreg1, src));
        list.concat(taicpu.op_reg_ref(A_SW, tmpreg1, dst));
        Inc(src.offset, 4);
        Inc(dst.offset, 4);
      end;
      len := len mod 4;
    end;
    if (len and 4) <> 0 then
    begin
      hreg := GetIntRegister(list, OS_INT);
      a_load_ref_reg(list, OS_32, OS_32, src, hreg);
      a_load_reg_ref(list, OS_32, OS_32, hreg, dst);
      Inc(src.offset, 4);
      Inc(dst.offset, 4);
    end;
    { copy the leftovers }
    if (len and 2) <> 0 then
    begin
      hreg := GetIntRegister(list, OS_INT);
      a_load_ref_reg(list, OS_16, OS_16, src, hreg);
      a_load_reg_ref(list, OS_16, OS_16, hreg, dst);
      Inc(src.offset, 2);
      Inc(dst.offset, 2);
    end;
    if (len and 1) <> 0 then
    begin
      hreg := GetIntRegister(list, OS_INT);
      a_load_ref_reg(list, OS_8, OS_8, src, hreg);
      a_load_reg_ref(list, OS_8, OS_8, hreg, dst);
    end;
  end;
end;


procedure TCgMPSel.g_concatcopy_unaligned(list: tasmlist; const Source, dest: treference; len: aint);
var
  src, dst: TReference;
  tmpreg1, countreg: TRegister;
  i:   aint;
  lab: tasmlabel;
begin
  if len > 31 then
    g_concatcopy_move(list, Source, dest, len)
  else
  begin
    reference_reset(src,sizeof(aint));
    reference_reset(dst,sizeof(aint));
    { load the address of source into src.base }
    src.base := GetAddressRegister(list);
    a_loadaddr_ref_reg(list, Source, src.base);
    { load the address of dest into dst.base }
    dst.base := GetAddressRegister(list);
    a_loadaddr_ref_reg(list, dest, dst.base);
    { generate a loop }
    if len > 4 then
    begin
      { the offsets are zero after the a_loadaddress_ref_reg and just }
      { have to be set to 8. I put an Inc there so debugging may be   }
      { easier (should offset be different from zero here, it will be }
      { easy to notice in the generated assembler                     }
      countreg := GetIntRegister(list, OS_INT);
      tmpreg1  := GetIntRegister(list, OS_INT);
      a_load_const_reg(list, OS_INT, len, countreg);
      { explicitely allocate R_O0 since it can be used safely here }
      { (for holding date that's being copied)                    }
      current_asmdata.getjumplabel(lab);
      a_label(list, lab);
      list.concat(taicpu.op_reg_ref(A_LBU, tmpreg1, src));
      list.concat(taicpu.op_reg_ref(A_SB, tmpreg1, dst));
      list.concat(taicpu.op_reg_reg_const(A_ADDIU, src.base, src.base, 1));
      list.concat(taicpu.op_reg_reg_const(A_ADDIU, dst.base, dst.base, 1));
      list.concat(taicpu.op_reg_reg_const(A_ADDIU, countreg, countreg, -1));
      list.concat(taicpu.op_reg_sym(A_BGTZ, countreg, lab));
      list.concat(taicpu.op_none(A_NOP));
    end
    else
    begin
      { unrolled loop }
      tmpreg1 := GetIntRegister(list, OS_INT);
      for i := 1 to len do
      begin
        list.concat(taicpu.op_reg_ref(A_LBU, tmpreg1, src));
        list.concat(taicpu.op_reg_ref(A_SB, tmpreg1, dst));
        Inc(src.offset);
        Inc(dst.offset);
      end;
    end;
  end;
end;


procedure TCgMPSel.g_intf_wrapper(list: tasmlist; procdef: tprocdef; const labelname: string; ioffset: longint);
      procedure loadvmttor24;
        var
          href: treference;
        begin
          reference_reset_base(href, NR_R2, 0, sizeof(aint));  { return value }
          cg.a_load_ref_reg(list, OS_ADDR, OS_ADDR, href, NR_R24);
        end;


      procedure op_onr24methodaddr;
        var
          href : treference;
        begin
          if (procdef.extnumber=$ffff) then
            Internalerror(200006139);
          { call/jmp  vmtoffs(%eax) ; method offs }
          reference_reset_base(href, NR_R24, procdef._class.vmtmethodoffset(procdef.extnumber), sizeof(aint));
          cg.a_load_ref_reg(list, OS_ADDR, OS_ADDR, href, NR_R24);
          list.concat(taicpu.op_reg(A_JR, NR_R24));
        end;
var
  make_global: boolean;
  href: treference;
begin
  if procdef.proctypeoption <> potype_none then
    Internalerror(200006137);
  if not assigned(procdef._class) or
    (procdef.procoptions * [po_classmethod, po_staticmethod,
    po_methodpointer, po_interrupt, po_iocheck] <> []) then
    Internalerror(200006138);
  if procdef.owner.symtabletype <> objectsymtable then
    Internalerror(200109191);

  make_global := False;
  if (not current_module.is_unit) or
    (procdef.owner.defowner.owner.symtabletype = globalsymtable) then
    make_global := True;

  if make_global then
    List.concat(Tai_symbol.Createname_global(labelname, AT_FUNCTION, 0))
  else
    List.concat(Tai_symbol.Createname(labelname, AT_FUNCTION, 0));

  { set param1 interface to self  }
  g_adjust_self_value(list, procdef, ioffset);

  if po_virtualmethod in procdef.procoptions then
  begin
    loadvmttor24;
    op_onr24methodaddr;
  end
  else
   list.concat(taicpu.op_sym(A_B,current_asmdata.RefAsmSymbol(procdef.mangledname)));
  { Delay slot }
  list.Concat(TAiCpu.Op_none(A_NOP));

  List.concat(Tai_symbol_end.Createname(labelname));
end;

{****************************************************************************
                               TCG64_MIPSel
****************************************************************************}


procedure TCg64MPSel.a_load64_reg_ref(list: tasmlist; reg: tregister64; const ref: treference);
var
  tmpref: treference;
begin
  { Override this function to prevent loading the reference twice }
  tmpref := ref;
  cg.a_load_reg_ref(list, OS_S32, OS_S32, reg.reglo, tmpref);
  Inc(tmpref.offset, 4);
  cg.a_load_reg_ref(list, OS_S32, OS_S32, reg.reghi, tmpref);
end;


procedure TCg64MPSel.a_load64_ref_reg(list: tasmlist; const ref: treference; reg: tregister64);
var
  tmpref: treference;
begin
  { Override this function to prevent loading the reference twice }
  tmpref := ref;
  cg.a_load_ref_reg(list, OS_S32, OS_S32, tmpref, reg.reglo);
  Inc(tmpref.offset, 4);
  cg.a_load_ref_reg(list, OS_S32, OS_S32, tmpref, reg.reghi);
end;


procedure TCg64MPSel.a_param64_ref(list: tasmlist; const r: treference; const paraloc: tcgpara);
var
  hreg64: tregister64;
begin
        { Override this function to prevent loading the reference twice.
          Use here some extra registers, but those are optimized away by the RA }
  hreg64.reglo := cg.GetIntRegister(list, OS_S32);
  hreg64.reghi := cg.GetIntRegister(list, OS_S32);
  a_load64_ref_reg(list, r, hreg64);
  a_param64_reg(list, hreg64, paraloc);
end;




procedure TCg64MPSel.a_op64_reg_reg(list: tasmlist; op: TOpCG; size: tcgsize; regsrc, regdst: TRegister64);
var
  op1, op2, op_call64: TAsmOp;
  tmpreg1, tmpreg2: TRegister;
begin
  tmpreg1 := NR_TCR12; //GetIntRegister(list, OS_INT);
  tmpreg2 := NR_TCR13; //GetIntRegister(list, OS_INT);

  case op of
    OP_ADD:
      begin
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg1, regsrc.reglo, regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR10, tmpreg1, regsrc.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg2, regsrc.reghi, regdst.reghi));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, NR_TCR10, NR_TCR10, tmpreg2));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reglo, tmpreg1));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reghi, NR_TCR10));
        exit;
      end;
    OP_AND:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_AND, regdst.reglo, regsrc.reglo, regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_AND, regdst.reghi, regsrc.reghi, regdst.reghi));
        exit;
    end;

    OP_NEG:
      begin
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, regdst.reglo, NR_R0, regsrc.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR10, NR_R0, regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, regdst.reghi, NR_R0, regsrc.reghi));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, NR_TCR10, regdst.reghi, NR_TCR10));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reghi, NR_TCR10));
        exit;
      end;
    OP_NOT:
    begin
      list.concat(taicpu.op_reg_reg_reg(A_NOR, regdst.reglo, NR_R0, regsrc.reglo));
      list.concat(taicpu.op_reg_reg_reg(A_NOR, regdst.reghi, NR_R0, regsrc.reghi));
      exit;
    end;
    OP_OR:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_OR, regdst.reglo, regsrc.reglo, regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_OR, regdst.reghi, regsrc.reghi, regdst.reghi));
        exit;
    end;
    OP_SUB:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, tmpreg1, regdst.reglo, regsrc.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR10, regdst.reglo, tmpreg1));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, tmpreg2, regdst.reghi, regsrc.reghi));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, tmpreg2, tmpreg2, NR_TCR10));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reglo, tmpreg1));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reghi, tmpreg2));
        exit;
    end;
    OP_XOR:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_XOR, regdst.reglo, regdst.reglo, regsrc.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_XOR, regdst.reghi, regsrc.reghi, regdst.reghi));
        exit;
    end;
    else
      internalerror(200306017);

  end; {case}



end;


procedure TCg64MPSel.a_op64_const_reg(list: tasmlist; op: TOpCG; size: tcgsize; Value: int64; regdst: TRegister64);
var
  op1, op2: TAsmOp;
begin
  case op of
    OP_NEG,
    OP_NOT:
      internalerror(200306017);
  end;
  a_op64_const_reg_reg(list, op, size, value, regdst, regdst);

end;


procedure TCg64MPSel.a_op64_const_reg_reg(list: tasmlist; op: TOpCG; size: tcgsize; Value: int64; regsrc, regdst: tregister64);
var
  l: tlocation;
begin
  a_op64_const_reg_reg_checkoverflow(list, op, size, Value, regsrc, regdst, False, l);
end;


procedure TCg64MPSel.a_op64_reg_reg_reg(list: tasmlist; op: TOpCG; size: tcgsize; regsrc1, regsrc2, regdst: tregister64);
var
  l: tlocation;
begin
  a_op64_reg_reg_reg_checkoverflow(list, op, size, regsrc1, regsrc2, regdst, False, l);
end;


procedure TCg64MPSel.a_op64_const_reg_reg_checkoverflow(list: tasmlist; op: TOpCG; size: tcgsize; Value: int64; regsrc, regdst: tregister64; setflags: boolean; var ovloc: tlocation);
var
  op1, op2: TAsmOp;
  tmpreg1: TRegister;
begin
  tmpreg1 := NR_TCR12;
  case op of
    OP_NEG,
    OP_NOT:
      internalerror(200306017);
  end;

  list.concat(taicpu.op_reg_const(A_LI, NR_TCR10, aint(hi(Value))));
  list.concat(taicpu.op_reg_const(A_LI, NR_TCR11, aint(lo(Value))));
  case op of
    OP_ADD:
      begin
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, regdst.reglo, regsrc.reglo, NR_TCR10));
        list.concat(taicpu.op_reg_reg_reg(A_SLTU, tmpreg1, regdst.reglo, regsrc.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, regdst.reghi, regsrc.reghi, NR_TCR11));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg1, tmpreg1, regdst.reghi));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reghi, tmpreg1));
        exit;
      end;
    OP_AND:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_AND, regdst.reglo, regsrc.reglo, NR_TCR10));
        list.concat(taicpu.op_reg_reg_reg(A_AND, regdst.reghi, regsrc.reghi, NR_TCR11));
        exit;
    end;

    OP_OR:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_OR, regdst.reglo, regsrc.reglo, NR_TCR10));
        list.concat(taicpu.op_reg_reg_reg(A_OR, regdst.reghi, regsrc.reghi, NR_TCR11));
        exit;
    end;
    OP_SUB:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, regdst.reglo, regsrc.reglo, NR_TCR10));
        list.concat(taicpu.op_reg_reg_reg(A_SLTU, tmpreg1, regsrc.reglo, regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, regdst.reghi, regsrc.reghi, NR_TCR11));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, tmpreg1, regdst.reghi, tmpreg1));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reghi, tmpreg1));
        exit;
    end;
    OP_XOR:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_XOR, regdst.reglo, regsrc.reglo, NR_TCR10));
        list.concat(taicpu.op_reg_reg_reg(A_XOR, regdst.reghi, regsrc.reghi, NR_TCR11));
        exit;
    end;
    else
      internalerror(200306017);
  end;

end;


procedure TCg64MPSel.a_op64_reg_reg_reg_checkoverflow(list: tasmlist; op: TOpCG; size: tcgsize; regsrc1, regsrc2, regdst: tregister64; setflags: boolean; var ovloc: tlocation);
var
  op1, op2: TAsmOp;
  tmpreg1, tmpreg2: TRegister;

begin
  tmpreg1 := NR_TCR12;
  tmpreg2 := NR_TCR13;

  case op of
    OP_NEG,
    OP_NOT:
      internalerror(200306017);
  end;
  case op of
    OP_ADD:
      begin
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg1, regsrc2.reglo, regsrc1.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR10, tmpreg1, regsrc2.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, tmpreg2, regsrc2.reghi, regsrc1.reghi));
        list.concat(taicpu.op_reg_reg_reg(A_ADDU, regdst.reghi, NR_TCR10, tmpreg2));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reglo, tmpreg1));
        exit;
      end;
    OP_AND:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_AND, regdst.reglo, regsrc2.reglo, regsrc1.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_AND, regdst.reghi, regsrc2.reghi, regsrc1.reghi));
        exit;
    end;
    OP_OR:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_OR, regdst.reglo, regsrc2.reglo, regsrc1.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_OR, regdst.reghi, regsrc2.reghi, regsrc1.reghi));
        exit;
    end;
    OP_SUB:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, tmpreg1, regsrc2.reglo, regsrc1.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR10, regsrc2.reglo, tmpreg1));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, tmpreg2, regsrc2.reghi, regsrc1.reghi));
        list.concat(taicpu.op_reg_reg_reg(A_SUBU, regdst.reghi, tmpreg2, NR_TCR10));
        list.concat(Taicpu.Op_reg_reg(A_MOVE, regdst.reglo, tmpreg1));
        exit;
    end;
    OP_XOR:
    begin
        list.concat(taicpu.op_reg_reg_reg(A_XOR, regdst.reglo, regsrc2.reglo, regsrc1.reglo));
        list.concat(taicpu.op_reg_reg_reg(A_XOR, regdst.reghi, regsrc2.reghi, regsrc1.reghi));
        exit;
    end;
    else
      internalerror(200306017);

  end; {case}

end;


    procedure create_codegen;
      begin
        cg:=TCgMPSel.Create;
        cg64:=TCg64MPSel.Create;
      end;

end.

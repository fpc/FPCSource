{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the code generator for the PowerPC

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

{$I fpcdefs.inc}

interface

uses
  globtype, symtype, symdef,
  cgbase, cgobj,
  aasmbase, aasmcpu, aasmtai,
  cpubase, cpuinfo, cgutils, rgcpu,
  parabase;

type
  tcgppc = class(tcg)
    procedure init_register_allocators; override;
    procedure done_register_allocators; override;

    { passing parameters, per default the parameter is pushed }
    { nr gives the number of the parameter (enumerated from   }
    { left to right), this allows to move the parameter to    }
    { register, if the cpu supports register calling          }
    { conventions                                             }
    procedure a_param_const(list: taasmoutput; size: tcgsize; a: aint; const
      paraloc: tcgpara); override;
    procedure a_param_ref(list: taasmoutput; size: tcgsize; const r: treference;
      const paraloc: tcgpara); override;
    procedure a_paramaddr_ref(list: taasmoutput; const r: treference; const
      paraloc: tcgpara); override;

    procedure a_call_name(list: taasmoutput; const s: string); override;
    procedure a_call_reg(list: taasmoutput; reg: tregister); override;

    procedure a_op_const_reg(list: taasmoutput; Op: TOpCG; size: TCGSize; a:
      aint; reg: TRegister); override;
    procedure a_op_reg_reg(list: taasmoutput; Op: TOpCG; size: TCGSize; src,
      dst: TRegister); override;

    procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
      size: tcgsize; a: aint; src, dst: tregister); override;
    procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
      size: tcgsize; src1, src2, dst: tregister); override;

    { move instructions }
    procedure a_load_const_reg(list: taasmoutput; size: tcgsize; a: aint; reg:
      tregister); override;
    { stores the contents of register reg to the memory location described by
    ref }
    procedure a_load_reg_ref(list: taasmoutput; fromsize, tosize: tcgsize; reg:
      tregister; const ref: treference); override;
    { loads the memory pointed to by ref into register reg }
    procedure a_load_ref_reg(list: taasmoutput; fromsize, tosize: tcgsize; const
      Ref: treference; reg: tregister); override;
    procedure a_load_reg_reg(list: taasmoutput; fromsize, tosize: tcgsize; reg1,
      reg2: tregister); override;

    { fpu move instructions }
    procedure a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; reg1, reg2:
      tregister); override;

    procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref:
      treference; reg: tregister); override;
    procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg:
      tregister; const ref: treference); override;

    {  comparison operations }
    procedure a_cmp_const_reg_label(list: taasmoutput; size: tcgsize; cmp_op:
      topcmp; a: aint; reg: tregister;
      l: tasmlabel); override;
    procedure a_cmp_reg_reg_label(list: taasmoutput; size: tcgsize; cmp_op:
      topcmp; reg1, reg2: tregister; l: tasmlabel); override;

    procedure a_jmp_name(list: taasmoutput; const s: string); override;
    procedure a_jmp_always(list: taasmoutput; l: tasmlabel); override;
    procedure a_jmp_flags(list: taasmoutput; const f: TResFlags; l: tasmlabel);
      override;

    procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: TResFlags;
      reg: TRegister); override;

    procedure g_proc_entry(list: taasmoutput; localsize: longint; nostackframe:
      boolean); override;
    procedure g_proc_exit(list: taasmoutput; parasize: longint; nostackframe:
      boolean); override;
    procedure g_save_standard_registers(list: Taasmoutput); override;
    procedure g_restore_standard_registers(list: Taasmoutput); override;

    procedure a_loadaddr_ref_reg(list: taasmoutput; const ref: treference; r:
      tregister); override;

    procedure g_concatcopy(list: taasmoutput; const source, dest: treference;
      len: aint); override;

    procedure g_overflowcheck(list: taasmoutput; const l: tlocation; def: tdef);
      override;
    procedure a_jmp_cond(list: taasmoutput; cond: TOpCmp; l: tasmlabel);

    procedure g_intf_wrapper(list: TAAsmoutput; procdef: tprocdef; const
      labelname: string; ioffset: longint); override;

  private

    { Make sure ref is a valid reference for the PowerPC and sets the }
    { base to the value of the index if (base = R_NO).                }
    { Returns true if the reference contained a base, index and an    }
    { offset or symbol, in which case the base will have been changed }
    { to a tempreg (which has to be freed by the caller) containing   }
    { the sum of part of the original reference                       }
    function fixref(list: taasmoutput; var ref: treference; const size : TCgsize): boolean;

    { returns whether a reference can be used immediately in a powerpc }
    { instruction                                                      }
    function issimpleref(const ref: treference): boolean;

    { contains the common code of a_load_reg_ref and a_load_ref_reg }
    procedure a_load_store(list: taasmoutput; op: tasmop; reg: tregister;
      ref: treference);

    { creates the correct branch instruction for a given combination }
    { of asmcondflags and destination addressing mode                }
    procedure a_jmp(list: taasmoutput; op: tasmop;
      c: tasmcondflag; crval: longint; l: tasmlabel);

    { returns the lowest numbered FP register in use, and the number of used FP registers 
      for the current procedure }
    procedure calcFirstUsedFPR(out firstfpr : TSuperRegister; out fprcount : aint);
    { returns the lowest numbered GP register in use, and the number of used GP registers
      for the current procedure }
    procedure calcFirstUsedGPR(out firstgpr : TSuperRegister; out gprcount : aint);

    { returns true if the offset of the given reference can not be represented by a 16 bit
    immediate as required by some PowerPC instructions }
    function hasLargeOffset(const ref : TReference) : Boolean; inline;

    procedure a_call_name_direct(list: taasmoutput; s: string; prependDot : boolean; addNOP : boolean);
  end;

const
  TShiftOpCG2AsmOpConst : array[boolean, OP_SAR..OP_SHR] of TAsmOp = (
    (A_SRAWI, A_SLWI, A_SRWI), (A_SRADI, A_SLDI, A_SRDI)
    );

  TOpCmp2AsmCond: array[topcmp] of TAsmCondFlag = (C_NONE, C_EQ, C_GT,
    C_LT, C_GE, C_LE, C_NE, C_LE, C_LT, C_GE, C_GT);

implementation

uses
  sysutils,
  globals, verbose, systems, cutils,
  symconst, symsym, fmodule,
  rgobj, tgobj, cpupi, procinfo, paramgr;

{ helper function which calculate "magic" values for replacement of unsigned 
 division by constant operation by multiplication. See the PowerPC compiler
 developer manual for more information }
procedure getmagic_unsignedN(const N : byte; const d : aWord; 
  out magic_m : aWord; out magic_add : boolean; out magic_shift : byte);
var
    p : aInt;
    nc, delta, q1, r1, q2, r2, two_N_minus_1 : aWord;
begin
  assert(d > 0);

  two_N_minus_1 := aWord(1) shl (N-1);
    
  magic_add := false;
  nc := - 1 - (-d) mod d;
  p := N-1; { initialize p }
  q1 := two_N_minus_1 div nc; { initialize q1 = 2p/nc }
  r1 := two_N_minus_1 - q1*nc; { initialize r1 = rem(2p,nc) }
  q2 := (two_N_minus_1-1) div d; { initialize q2 = (2p-1)/d }
  r2 := (two_N_minus_1-1) - q2*d; { initialize r2 = rem((2p-1),d) }
  repeat
    inc(p);
    if (r1 >= (nc - r1)) then begin
      q1 := 2 * q1 + 1; { update q1 }
      r1 := 2*r1 - nc; { update r1 }
    end else begin
      q1 := 2*q1; { update q1 }
      r1 := 2*r1; { update r1 }
    end;
    if ((r2 + 1) >= (d - r2)) then begin
      if (q2 >= (two_N_minus_1-1)) then
        magic_add := true;
      q2 := 2*q2 + 1; { update q2 }
      r2 := 2*r2 + 1 - d; { update r2 }
    end else begin
      if (q2 >= two_N_minus_1) then 
        magic_add := true;
      q2 := 2*q2; { update q2 }
      r2 := 2*r2 + 1; { update r2 }
    end;
    delta := d - 1 - r2;
  until not ((p < (2*N)) and ((q1 < delta) or ((q1 = delta) and (r1 = 0))));
  magic_m := q2 + 1; { resulting magic number }
  magic_shift := p - N; { resulting shift }
end;

{ helper function which calculate "magic" values for replacement of signed 
 division by constant operation by multiplication. See the PowerPC compiler
 developer manual for more information }
procedure getmagic_signedN(const N : byte; const d : aInt; 
  out magic_m : aInt; out magic_s : aInt);
var
  p : aInt;
  ad, anc, delta, q1, r1, q2, r2, t : aWord;
  two_N_minus_1 : aWord;
    
begin
  assert((d < -1) or (d > 1));

  two_N_minus_1 := aWord(1) shl (N-1);

  ad := abs(d);
  t := two_N_minus_1 + (aWord(d) shr (N-1));
  anc := t - 1 - t mod ad; { absolute value of nc }
  p := (N-1); { initialize p }
  q1 := two_N_minus_1 div anc; { initialize q1 = 2p/abs(nc) }
  r1 := two_N_minus_1 - q1*anc; { initialize r1 = rem(2p,abs(nc)) }
  q2 := two_N_minus_1 div ad; { initialize q2 = 2p/abs(d) }
  r2 := two_N_minus_1 - q2*ad; { initialize r2 = rem(2p,abs(d)) }
  repeat 
    inc(p);
    q1 := 2*q1; { update q1 = 2p/abs(nc) }
    r1 := 2*r1; { update r1 = rem(2p/abs(nc)) }
    if (r1 >= anc) then begin { must be unsigned comparison }
      inc(q1);
      dec(r1, anc);
    end;
    q2 := 2*q2; { update q2 = 2p/abs(d) }
    r2 := 2*r2; { update r2 = rem(2p/abs(d)) }
    if (r2 >= ad) then begin { must be unsigned comparison }
      inc(q2);
      dec(r2, ad);
    end;
    delta := ad - r2;
  until not ((q1 < delta) or ((q1 = delta) and (r1 = 0)));
  magic_m := q2 + 1;
  if (d < 0) then begin
    magic_m := -magic_m; { resulting magic number }
  end;
  magic_s := p - N; { resulting shift }
end;

{ finds positive and negative powers of two of the given value, returning the
 power and whether it's a negative power or not in addition to the actual result
 of the function }
function ispowerof2(value : aInt; out power : byte; out neg : boolean) : boolean;
var
  i : longint;
  hl : aInt;
begin
  neg := false;
  { also try to find negative power of two's by negating if the 
   value is negative. low(aInt) is special because it can not be
   negated. Simply return the appropriate values for it }
  if (value < 0) then begin
    neg := true;
    if (value = low(aInt)) then begin
      power := sizeof(aInt)*8-1;
      result := true;
      exit;
    end;
    value := -value;
  end;

  if ((value and (value-1)) <> 0) then begin
    result := false;
    exit;
  end;
  hl := 1;
  for i := 0 to (sizeof(aInt)*8-1) do begin
    if (hl = value) then begin
      result := true;
      power := i;
      exit;
    end;
    hl := hl shl 1;
  end;
end;


procedure tcgppc.init_register_allocators;
begin
  inherited init_register_allocators;
  rg[R_INTREGISTER] := trgcpu.create(R_INTREGISTER, R_SUBWHOLE,
    [RS_R3, RS_R4, RS_R5, RS_R6, RS_R7, RS_R8,
      RS_R9, RS_R10, RS_R11, RS_R12, RS_R31, RS_R30, RS_R29,
      RS_R28, RS_R27, RS_R26, RS_R25, RS_R24, RS_R23, RS_R22,
      RS_R21, RS_R20, RS_R19, RS_R18, RS_R17, RS_R16, RS_R15,
      RS_R14, RS_R13], first_int_imreg, []);
  rg[R_FPUREGISTER] := trgcpu.create(R_FPUREGISTER, R_SUBNONE,
    [RS_F0, RS_F1, RS_F2, RS_F3, RS_F4, RS_F5, RS_F6, RS_F7, RS_F8, RS_F9,
    RS_F10, RS_F11, RS_F12, RS_F13, RS_F31, RS_F30, RS_F29, RS_F28, RS_F27,
      RS_F26, RS_F25, RS_F24, RS_F23, RS_F22, RS_F21, RS_F20, RS_F19, RS_F18,
      RS_F17, RS_F16, RS_F15, RS_F14], first_fpu_imreg, []);
{$WARNING FIX ME}
  rg[R_MMREGISTER] := trgcpu.create(R_MMREGISTER, R_SUBNONE,
    [RS_M0, RS_M1, RS_M2], first_mm_imreg, []);
end;

procedure tcgppc.done_register_allocators;
begin
  rg[R_INTREGISTER].free;
  rg[R_FPUREGISTER].free;
  rg[R_MMREGISTER].free;
  inherited done_register_allocators;
end;

procedure tcgppc.a_param_const(list: taasmoutput; size: tcgsize; a: aint; const
  paraloc: tcgpara);
var
  ref: treference;
begin
  paraloc.check_simple_location;
  case paraloc.location^.loc of
    LOC_REGISTER, LOC_CREGISTER:
      a_load_const_reg(list, size, a, paraloc.location^.register);
    LOC_REFERENCE:
      begin
        reference_reset(ref);
        ref.base := paraloc.location^.reference.index;
        ref.offset := paraloc.location^.reference.offset;
        a_load_const_ref(list, size, a, ref);
      end;
  else
    internalerror(2002081101);
  end;
end;

procedure tcgppc.a_param_ref(list: taasmoutput; size: tcgsize; const r:
  treference; const paraloc: tcgpara);

var
  tmpref, ref: treference;
  location: pcgparalocation;
  sizeleft: aint;
  adjusttail : boolean;

begin
  location := paraloc.location;
  tmpref := r;
  sizeleft := paraloc.intsize;
  adjusttail := false;
  while assigned(location) do begin
    case location^.loc of
      LOC_REGISTER, LOC_CREGISTER:
        begin
          if (size <> OS_NO) then
            a_load_ref_reg(list, size, location^.size, tmpref,
              location^.register)
          else
          {$IFDEF extdebug}
            list.concat(tai_comment.create(strpnew('a_param_ref with OS_NO')));
          {$ENDIF extdebug}

            { load non-integral sized memory location into register. This 
             memory location be 1-sizeleft byte sized.
             Always assume that this memory area is properly aligned, eg. start
             loading the larger quantities for "odd" quantities first }
            case sizeleft of
              1,2,4,8 :
                a_load_ref_reg(list, int_cgsize(sizeleft), location^.size, tmpref,
                  location^.register); 
              3 : begin
                a_reg_alloc(list, NR_R12); 
                a_load_ref_reg(list, OS_16, location^.size, tmpref, 
                  NR_R12);
                inc(tmpref.offset, tcgsize2size[OS_16]);
                a_load_ref_reg(list, OS_8, location^.size, tmpref,
                  location^.register);
                list.concat(taicpu.op_reg_reg_const_const(A_RLDIMI, location^.register, NR_R12, 8, 40));
                a_reg_dealloc(list, NR_R12);
              end;
              5 : begin
                a_reg_alloc(list, NR_R12);
                a_load_ref_reg(list, OS_32, location^.size, tmpref, NR_R12);
                inc(tmpref.offset, tcgsize2size[OS_32]);
                a_load_ref_reg(list, OS_8, location^.size, tmpref, location^.register);
                list.concat(taicpu.op_reg_reg_const_const(A_RLDIMI, location^.register, NR_R12, 8, 24));
                a_reg_dealloc(list, NR_R12);
              end;
              6 : begin
                a_reg_alloc(list, NR_R12);
                a_load_ref_reg(list, OS_32, location^.size, tmpref, NR_R12);
                inc(tmpref.offset, tcgsize2size[OS_32]);
                a_load_ref_reg(list, OS_16, location^.size, tmpref, location^.register);
                list.concat(taicpu.op_reg_reg_const_const(A_RLDIMI, location^.register, NR_R12, 16, 16));                
                a_reg_dealloc(list, NR_R12);
              end;
              7 : begin
                a_reg_alloc(list, NR_R12);
                a_reg_alloc(list, NR_R0);
                a_load_ref_reg(list, OS_32, location^.size, tmpref, NR_R12);
                inc(tmpref.offset, tcgsize2size[OS_32]);
                a_load_ref_reg(list, OS_16, location^.size, tmpref, NR_R0);
                inc(tmpref.offset, tcgsize2size[OS_16]);
                a_load_ref_reg(list, OS_8, location^.size, tmpref, location^.register);
                list.concat(taicpu.op_reg_reg_const_const(A_RLDIMI, NR_R0, NR_R12, 16, 16));
                list.concat(taicpu.op_reg_reg_const_const(A_RLDIMI, location^.register, NR_R0, 8, 8));
                a_reg_dealloc(list, NR_R0);
                a_reg_dealloc(list, NR_R12);
              end;
              else
                { still > 8 bytes to load, so load data single register now }
                a_load_ref_reg(list, location^.size, location^.size, tmpref,
                  location^.register);
                { the block is > 8 bytes, so we have to store any bytes not
                 a multiple of the register size beginning with the MSB }
                adjusttail := true;
            end; 
(*          
            { Comment this in (for gcc compat) and be prepared for a whole bunch of errors :/ }
            
            if (adjusttail) and (sizeleft < tcgsize2size[OS_INT]) then
              a_op_const_reg(list, OP_SHL, OS_INT, 
                (tcgsize2size[OS_INT] - sizeleft) * tcgsize2size[OS_INT], 
                location^.register);
*)
        end;
      LOC_REFERENCE:
        begin
          reference_reset_base(ref, location^.reference.index,
            location^.reference.offset);
          g_concatcopy(list, tmpref, ref, sizeleft);
          if assigned(location^.next) then
            internalerror(2005010710);
        end;
      LOC_FPUREGISTER, LOC_CFPUREGISTER:
        case location^.size of
          OS_F32, OS_F64:
            a_loadfpu_ref_reg(list, location^.size, tmpref, location^.register);
        else
          internalerror(2002072801);
        end;
      LOC_VOID: 
        { nothing to do }
        ;
    else
      internalerror(2002081103);
    end;
    inc(tmpref.offset, tcgsize2size[location^.size]);
    dec(sizeleft, tcgsize2size[location^.size]);
    location := location^.next;
  end;
end;

procedure tcgppc.a_paramaddr_ref(list: taasmoutput; const r: treference; const
  paraloc: tcgpara);
var
  ref: treference;
  tmpreg: tregister;

begin
  paraloc.check_simple_location;
  case paraloc.location^.loc of
    LOC_REGISTER, LOC_CREGISTER:
      a_loadaddr_ref_reg(list, r, paraloc.location^.register);
    LOC_REFERENCE:
      begin
        reference_reset(ref);
        ref.base := paraloc.location^.reference.index;
        ref.offset := paraloc.location^.reference.offset;
        tmpreg := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
        a_loadaddr_ref_reg(list, r, tmpreg);
        a_load_reg_ref(list, OS_ADDR, OS_ADDR, tmpreg, ref);
      end;
  else
    internalerror(2002080701);
  end;
end;

{ calling a procedure by name }

procedure tcgppc.a_call_name(list: taasmoutput; const s: string);
begin
    a_call_name_direct(list, s, true, true);
end;

procedure tcgppc.a_call_name_direct(list: taasmoutput; s: string; prependDot : boolean; addNOP : boolean);
begin
  if (prependDot) then
    s := '.' + s;
  list.concat(taicpu.op_sym(A_BL, objectlibrary.newasmsymbol(s, AB_EXTERNAL,
    AT_FUNCTION)));
  if (addNOP) then
    list.concat(taicpu.op_none(A_NOP));
  { the compiler does not properly set this flag anymore in pass 1, and
   for now we only need it after pass 2 (I hope) (JM) }
  include(current_procinfo.flags, pi_do_call);
end;


{ calling a procedure by address }

procedure tcgppc.a_call_reg(list: taasmoutput; reg: tregister);
var
  tmpref: treference;
begin
  if (not (cs_littlesize in aktglobalswitches)) then begin
    { load actual function entry (reg contains the reference to the function descriptor)
    into R0 }
    reference_reset_base(tmpref, reg, 0);
    a_load_ref_reg(list, OS_ADDR, OS_ADDR, tmpref, NR_R0);

    { save TOC pointer in stackframe }
    reference_reset_base(tmpref, NR_STACK_POINTER_REG, LA_RTOC_ELF);
    a_load_reg_ref(list, OS_ADDR, OS_ADDR, NR_RTOC, tmpref);

    { move actual function pointer to CTR register }
    list.concat(taicpu.op_reg(A_MTCTR, NR_R0));

    { load new TOC pointer from function descriptor into RTOC register }
    reference_reset_base(tmpref, reg, tcgsize2size[OS_ADDR]);
    a_load_ref_reg(list, OS_ADDR, OS_ADDR, tmpref, NR_RTOC);

    { load new environment pointer from function descriptor into R11 register }
    reference_reset_base(tmpref, reg, 2*tcgsize2size[OS_ADDR]);
    a_load_ref_reg(list, OS_ADDR, OS_ADDR, tmpref, NR_R11);

    { call function }
    list.concat(taicpu.op_none(A_BCTRL));
  end else begin
    { call ptrgl helper routine which expects the pointer to the function descriptor
    in R11 }
    a_load_reg_reg(list, OS_ADDR, OS_ADDR, reg, NR_R11);
    a_call_name_direct(list, '.ptrgl', false, false);
  end;

  { we need to load the old RTOC from stackframe because we changed it}
  reference_reset_base(tmpref, NR_STACK_POINTER_REG, LA_RTOC_ELF);
  a_load_ref_reg(list, OS_ADDR, OS_ADDR, tmpref, NR_RTOC);

  include(current_procinfo.flags, pi_do_call);
end;

{********************** load instructions ********************}

procedure tcgppc.a_load_const_reg(list: taasmoutput; size: TCGSize; a: aint;
  reg: TRegister);

  { loads a 32 bit constant into the given register, using an optimal instruction sequence.
    This is either LIS, LI or LI+ADDIS.
    Returns true if during these operations the upper 32 bits were filled with 1 bits (e.g.
    sign extension was performed) }
  function load32bitconstant(list : taasmoutput; size : TCGSize; a : longint;
    reg : TRegister) : boolean;
  var 
    is_half_signed : byte;
  begin
    { if the lower 16 bits are zero, do a single LIS }
    if (smallint(a) = 0) and ((a shr 16) <> 0) then begin
      list.concat(taicpu.op_reg_const(A_LIS, reg, smallint(hi(a))));
      load32bitconstant := longint(a) < 0;
    end else begin
      is_half_signed := ord(smallint(lo(a)) < 0);
      list.concat(taicpu.op_reg_const(A_LI, reg, smallint(a and $ffff)));
      if smallint(hi(a) + is_half_signed) <> 0 then begin
        list.concat(taicpu.op_reg_reg_const(A_ADDIS, reg, reg, smallint(hi(a) + is_half_signed)));
      end;
      load32bitconstant := (smallint(a) < 0) or (a < 0);
    end;
  end;

  { R0-safe version of the above (ADDIS doesn't work the same way with R0 as base), without
   the return value. Unused until further testing shows that it is not really necessary;
   loading the upper 32 bits of a value is now done using R12, which does not require
   special treatment }
  procedure load32bitconstantR0(list : taasmoutput; size : TCGSize; a : longint;
    reg : TRegister);
  begin
    { only 16 bit constant? (-2^15 <= a <= +2^15-1) }
    if (a >= low(smallint)) and (a <= high(smallint)) then begin
      list.concat(taicpu.op_reg_const(A_LI, reg, smallint(a)));
    end else begin
      { check if we have to start with LI or LIS, load as 32 bit constant }
      if ((a and $FFFF) <> 0) then begin
        list.concat(taicpu.op_reg_const(A_LIS, reg, smallint(a shr 16)));
        list.concat(taicpu.op_reg_reg_const(A_ORI, reg, reg, word(a)));
      end else begin
        list.concat(taicpu.op_reg_const(A_LIS, reg, smallint(a shr 16)));
      end;
    end;
  end;

var
  extendssign : boolean;
  {$IFDEF EXTDEBUG}
  astring : string;
  {$ENDIF EXTDEBUG}

begin
  {$IFDEF EXTDEBUG}
  astring := 'a_load_const reg ' + inttostr(hi(a)) + ' ' + inttostr(lo(a)) + ' ' + inttostr(ord(size)) + ' ' + inttostr(tcgsize2size[size]);
  list.concat(tai_comment.create(strpnew(astring)));
  {$ENDIF EXTDEBUG}

  if not (size in [OS_8, OS_S8, OS_16, OS_S16, OS_32, OS_S32, OS_64, OS_S64]) then
    internalerror(2002090902);
  if (lo(a) = 0) and (hi(a) <> 0) then begin
    { load only upper 32 bits, and shift }
    load32bitconstant(list, size, hi(a), reg);
    list.concat(taicpu.op_reg_reg_const(A_SLDI, reg, reg, 32));    
  end else begin
    { load lower 32 bits }
    extendssign := load32bitconstant(list, size, lo(a), reg);
    if (extendssign) and (hi(a) = 0) then
      { if upper 32 bits are zero, but loading the lower 32 bit resulted in automatic 
        sign extension, clear those bits }
      a_load_reg_reg(list, OS_32, OS_64, reg, reg)
    else if (not 
      ((extendssign and (longint(hi(a)) = -1)) or 
       ((not extendssign) and (hi(a)=0)))
      ) then begin
      { only load the upper 32 bits, if the automatic sign extension is not okay,
        that is, _not_ if 
        - loading the lower 32 bits resulted in -1 in the upper 32 bits, and the upper 
         32 bits should contain -1
        - loading the lower 32 bits resulted in 0 in the upper 32 bits, and the upper
         32 bits should contain 0 }
      load32bitconstant(list, size, hi(a), NR_R12);
      { combine both registers }
      list.concat(taicpu.op_reg_reg_const_const(A_RLDIMI, reg, NR_R12, 32, 0));
    end;
  end;
end;

procedure tcgppc.a_load_reg_ref(list: taasmoutput; fromsize, tosize: TCGSize;
  reg: tregister; const ref: treference);

const
  StoreInstr: array[OS_8..OS_64, boolean, boolean] of TAsmOp =
  { indexed? updating?}
  (((A_STB, A_STBU), (A_STBX, A_STBUX)),
    ((A_STH, A_STHU), (A_STHX, A_STHUX)),
    ((A_STW, A_STWU), (A_STWX, A_STWUX)),
    ((A_STD, A_STDU), (A_STDX, A_STDUX))
    );
var
  op: TAsmOp;
  ref2: TReference;
begin
  ref2 := ref;
  fixref(list, ref2, tosize);
  if tosize in [OS_S8..OS_S64] then
    { storing is the same for signed and unsigned values }
    tosize := tcgsize(ord(tosize) - (ord(OS_S8) - ord(OS_8)));
  op := storeinstr[tcgsize2unsigned[tosize], ref2.index <> NR_NO, false];
  a_load_store(list, op, reg, ref2);
end;

procedure tcgppc.a_load_ref_reg(list: taasmoutput; fromsize, tosize: tcgsize;
  const ref: treference; reg: tregister);

const
  LoadInstr: array[OS_8..OS_S64, boolean, boolean] of TAsmOp =
  { indexed? updating? }
  (((A_LBZ, A_LBZU), (A_LBZX, A_LBZUX)),
    ((A_LHZ, A_LHZU), (A_LHZX, A_LHZUX)),
    ((A_LWZ, A_LWZU), (A_LWZX, A_LWZUX)),
    ((A_LD, A_LDU), (A_LDX, A_LDUX)),
    { 128bit stuff too }
    ((A_NONE, A_NONE), (A_NONE, A_NONE)),
    { there's no load-byte-with-sign-extend :( }
    ((A_LBZ, A_LBZU), (A_LBZX, A_LBZUX)),
    ((A_LHA, A_LHAU), (A_LHAX, A_LHAUX)),
    { there's no load-word-arithmetic-indexed with update, simulate it in code :( }
    ((A_LWA, A_NOP), (A_LWAX, A_LWAUX)),
    ((A_LD, A_LDU), (A_LDX, A_LDUX))
    );
var
  op: tasmop;
  ref2: treference;

begin
  if not (fromsize in [OS_8, OS_S8, OS_16, OS_S16, OS_32, OS_S32, OS_64, OS_S64]) then
    internalerror(2002090902);
  ref2 := ref;
  fixref(list, ref2, tosize);
  { the caller is expected to have adjusted the reference already
   in this case }
  if (TCGSize2Size[fromsize] >= TCGSize2Size[tosize]) then
    fromsize := tosize;
  op := loadinstr[fromsize, ref2.index <> NR_NO, false];
  { there is no LWAU instruction, simulate using ADDI and LWA }
  if (op = A_NOP) then begin
    list.concat(taicpu.op_reg_reg_const(A_ADDI, reg, reg, ref2.offset));
    ref2.offset := 0;
    op := A_LWA;
  end;
  a_load_store(list, op, reg, ref2);
  { sign extend shortint if necessary, since there is no
   load instruction that does that automatically (JM) }
  if fromsize = OS_S8 then
    list.concat(taicpu.op_reg_reg(A_EXTSB, reg, reg));
end;

procedure tcgppc.a_load_reg_reg(list: taasmoutput; fromsize, tosize: tcgsize;
  reg1, reg2: tregister);

const
  movemap : array[OS_8..OS_S128, OS_8..OS_S128] of tasmop = (
{     to  -> OS_8      OS_16     OS_32     OS_64     OS_128    OS_S8     OS_S16    OS_S32    OS_S64    OS_S128 }
{ from }
{ OS_8    } (A_MR,     A_RLDICL, A_RLDICL, A_RLDICL, A_NONE,   A_RLDICL, A_RLDICL, A_RLDICL, A_RLDICL, A_NOP   ),
{ OS_16   } (A_RLDICL, A_MR,     A_RLDICL, A_RLDICL, A_NONE,   A_RLDICL, A_RLDICL, A_RLDICL, A_RLDICL, A_NOP   ),
{ OS_32   } (A_RLDICL, A_RLDICL, A_MR,     A_RLDICL, A_NONE,   A_RLDICL, A_RLDICL, A_RLDICL, A_RLDICL, A_NOP   ),
{ OS_64   } (A_RLDICL, A_RLDICL, A_RLDICL, A_MR,     A_NONE,   A_RLDICL, A_RLDICL, A_RLDICL, A_RLDICL, A_NOP   ),
{ OS_128  } (A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NOP   ),
{ OS_S8   } (A_EXTSB,  A_EXTSB,  A_EXTSB,  A_EXTSB,  A_NONE,   A_MR,     A_EXTSB,  A_EXTSB,  A_EXTSB,  A_NOP   ),
{ OS_S16  } (A_RLDICL, A_EXTSH,  A_EXTSH,  A_EXTSH,  A_NONE,   A_EXTSB,  A_MR,     A_EXTSH,  A_EXTSH,  A_NOP   ),
{ OS_S32  } (A_RLDICL, A_RLDICL, A_EXTSW,  A_EXTSW,  A_NONE,   A_EXTSB,  A_EXTSH,  A_MR,     A_EXTSW,  A_NOP   ),
{ OS_S64  } (A_RLDICL, A_RLDICL, A_RLDICL, A_MR,     A_NONE,   A_EXTSB,  A_EXTSH,  A_EXTSW,  A_MR,     A_NOP   ),
{ OS_S128 } (A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NONE,   A_NOP   )
);

var
  instr: taicpu;
  op : tasmop;
begin
  op := movemap[fromsize, tosize];
  case op of
    A_MR, A_EXTSB, A_EXTSH, A_EXTSW : instr := taicpu.op_reg_reg(op, reg2, reg1);
    A_RLDICL : instr := taicpu.op_reg_reg_const_const(A_RLDICL, reg2, reg1, 0, (8-tcgsize2size[fromsize])*8);
  else
    internalerror(2002090901);
  end;
  list.concat(instr);
  rg[R_INTREGISTER].add_move_instruction(instr);
end;

procedure tcgppc.a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; 
  reg1, reg2: tregister);
var
  instr: taicpu;
begin
  instr := taicpu.op_reg_reg(A_FMR, reg2, reg1);
  list.concat(instr);
  rg[R_FPUREGISTER].add_move_instruction(instr);
end;

procedure tcgppc.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; 
  const ref: treference; reg: tregister);
const
  FpuLoadInstr: array[OS_F32..OS_F64, boolean, boolean] of TAsmOp =
  { indexed? updating?}
  (((A_LFS, A_LFSU), (A_LFSX, A_LFSUX)),
   ((A_LFD, A_LFDU), (A_LFDX, A_LFDUX)));
var
  op: tasmop;
  ref2: treference;

begin
  { several functions call this procedure with OS_32 or OS_64
   so this makes life easier (FK) }
  case size of
    OS_32, OS_F32:
      size := OS_F32;
    OS_64, OS_F64, OS_C64:
      size := OS_F64;
  else
    internalerror(200201121);
  end;
  ref2 := ref;
  fixref(list, ref2, size);
  op := fpuloadinstr[size, ref2.index <> NR_NO, false];
  a_load_store(list, op, reg, ref2);
end;

procedure tcgppc.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg:
  tregister; const ref: treference);
const
  FpuStoreInstr: array[OS_F32..OS_F64, boolean, boolean] of TAsmOp =
  { indexed? updating? }
  (((A_STFS, A_STFSU), (A_STFSX, A_STFSUX)),
   ((A_STFD, A_STFDU), (A_STFDX, A_STFDUX)));
var
  op: tasmop;
  ref2: treference;

begin
  if not (size in [OS_F32, OS_F64]) then
    internalerror(200201122);
  ref2 := ref;
  fixref(list, ref2, size);
  op := fpustoreinstr[size, ref2.index <> NR_NO, false];
  a_load_store(list, op, reg, ref2);
end;

procedure tcgppc.a_op_const_reg(list: taasmoutput; Op: TOpCG; size: TCGSize; a:
  aint; reg: TRegister);
begin
  a_op_const_reg_reg(list, op, size, a, reg, reg);
end;

procedure tcgppc.a_op_reg_reg(list: taasmoutput; Op: TOpCG; size: TCGSize; src,
  dst: TRegister);
begin
  a_op_reg_reg_reg(list, op, size, src, dst, dst);
end;

procedure tcgppc.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
  size: tcgsize; a: aint; src, dst: tregister);
var
  useReg : boolean;

  procedure do_lo_hi(loOp, hiOp : TAsmOp);
  begin
    { Optimization for logical ops (excluding AND), trying to do this as efficiently
     as possible by only generating code for the affected halfwords. Note that all
     the instructions handled here must have "X op 0 = X" for every halfword. }
    usereg := false;
    if (aword(a) > high(dword)) then begin
      usereg := true;
    end else begin
      if (word(a) <> 0) then begin
        list.concat(taicpu.op_reg_reg_const(loOp, dst, src, word(a)));
        if (word(a shr 16) <> 0) then
          list.concat(taicpu.op_reg_reg_const(hiOp, dst, dst, word(a shr 16)));
      end else if (word(a shr 16) <> 0) then
        list.concat(taicpu.op_reg_reg_const(hiOp, dst, src, word(a shr 16)));
    end;
  end;

  procedure do_lo_hi_and;
  begin
    { optimization logical and with immediate: only use "andi." for 16 bit
     ands, otherwise use register method. Doing this for 32 bit constants
     would not give any advantage to the register method (via useReg := true), 
     requiring a scratch register and three instructions. }
    usereg := false;
    if (aword(a) > high(word)) then
      usereg := true
    else
      list.concat(taicpu.op_reg_reg_const(A_ANDI_, dst, src, word(a)));
  end;

  procedure do_constant_div(list : taasmoutput; size : TCgSize; a : aint; src, dst : TRegister;
    signed : boolean);
  const
    negops : array[boolean] of tasmop = (A_NEG, A_NEGO);
  var
    magic, shift : int64;
    u_magic : qword;
    u_shift : byte;
    u_add : boolean;
    power : byte;
    isNegPower : boolean;
             
    divreg : tregister;
  begin
    if (a = 0) then begin
      internalerror(2005061701);
    end else if (a = 1) then begin
      cg.a_load_reg_reg(exprasmlist, OS_INT, OS_INT, src, dst);
    end else if (a = -1) then begin
      { note: only in the signed case possible..., may overflow }
      exprasmlist.concat(taicpu.op_reg_reg(negops[cs_check_overflow in aktlocalswitches], dst, src));
    end else if (ispowerof2(a, power, isNegPower)) then begin
      if (signed) then begin
        { From "The PowerPC Compiler Writer's Guide", pg. 52ff          }
        cg.a_op_const_reg_reg(exprasmlist, OP_SAR, OS_INT, power,
          src, dst);
        exprasmlist.concat(taicpu.op_reg_reg(A_ADDZE, dst, dst));
        if (isNegPower) then
          exprasmlist.concat(taicpu.op_reg_reg(A_NEG, dst, dst));
      end else begin
        cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, power, src, dst)
      end;
    end else begin
      { replace division by multiplication, both implementations }
      { from "The PowerPC Compiler Writer's Guide" pg. 53ff      }
      divreg := cg.getintregister(exprasmlist, OS_INT);
      if (signed) then begin
        getmagic_signedN(sizeof(aInt)*8, a, magic, shift);
        { load magic value }
        cg.a_load_const_reg(exprasmlist, OS_INT, magic, divreg);
        { multiply }
        exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULHD, dst, src, divreg));
        { add/subtract numerator }
        if (a > 0) and (magic < 0) then begin
          cg.a_op_reg_reg_reg(exprasmlist, OP_ADD, OS_INT, src, dst, dst);
        end else if (a < 0) and (magic > 0) then begin
          cg.a_op_reg_reg_reg(exprasmlist, OP_SUB, OS_INT, src, dst, dst);
        end;
        { shift shift places to the right (arithmetic) }
        cg.a_op_const_reg_reg(exprasmlist, OP_SAR, OS_INT, shift, dst, dst);                     
        { extract and add sign bit }
        if (a >= 0) then begin
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, 63, src, divreg);
        end else begin
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, 63, dst, divreg);
        end;                     
        cg.a_op_reg_reg_reg(exprasmlist, OP_ADD, OS_INT, dst, divreg, dst);
      end else begin
        getmagic_unsignedN(sizeof(aWord)*8, a, u_magic, u_add, u_shift);
        { load magic in divreg }
        cg.a_load_const_reg(exprasmlist, OS_INT, u_magic, divreg);
        exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULHDU, dst, src, divreg));
        if (u_add) then begin
          cg.a_op_reg_reg_reg(exprasmlist, OP_SUB, OS_INT, dst, src, divreg);
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT,  1, divreg, divreg);
          cg.a_op_reg_reg_reg(exprasmlist, OP_ADD, OS_INT, divreg, dst, divreg);
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, u_shift-1, divreg, dst);
        end else begin
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, u_shift, dst, dst);
        end;
      end;
    end;
  end;

var
  scratchreg: tregister;
  shift : byte;
  shiftmask : longint;
  isneg : boolean;

begin
  { subtraction is the same as addition with negative constant }
  if op = OP_SUB then begin
    a_op_const_reg_reg(list, OP_ADD, size, -a, src, dst);
    exit;
  end;
  { This case includes some peephole optimizations for the various operations,
   (e.g. AND, OR, XOR, ..) - can't this be done at some higher level, 
   independent of architecture? }

  { assume that we do not need a scratch register for the operation }
  useReg := false;
  case (op) of
    OP_DIV, OP_IDIV:
      if (cs_slowoptimize in aktglobalswitches) then
        do_constant_div(list, size, a, src, dst, op = OP_IDIV)
      else
        usereg := true; 
    OP_IMUL, OP_MUL:
      { idea: factorize constant multiplicands and use adds/shifts with few factors;
       however, even a 64 bit multiply is already quite fast on PPC64 }
      if (a = 0) then
        a_load_const_reg(list, size, 0, dst)
      else if (a = -1) then
        list.concat(taicpu.op_reg_reg(A_NEG, dst, dst))
      else if (a = 1) then
        a_load_reg_reg(list, OS_INT, OS_INT, src, dst)
      else if ispowerof2(a, shift, isneg) then begin
        list.concat(taicpu.op_reg_reg_const(A_SLDI, dst, src, shift));
        if (isneg) then
          exprasmlist.concat(taicpu.op_reg_reg(A_NEG, dst, dst));
      end else if (a >= low(smallint)) and (a <= high(smallint)) then
        list.concat(taicpu.op_reg_reg_const(A_MULLI, dst, src,
          smallint(a)))
      else
        usereg := true;
    OP_ADD:
      if (a = 0) then
        a_load_reg_reg(list, size, size, src, dst)
      else if (a >= low(smallint)) and (a <= high(smallint)) then
        list.concat(taicpu.op_reg_reg_const(A_ADDI, dst, src, smallint(a)))
      else
        useReg := true;
    OP_OR:
      if (a = 0) then
        a_load_reg_reg(list, size, size, src, dst)
      else if (a = -1) then
        a_load_const_reg(list, size, -1, dst)
      else
        do_lo_hi(A_ORI, A_ORIS);
    OP_AND:
      if (a = 0) then
        a_load_const_reg(list, size, 0, dst)
      else if (a = -1) then
        a_load_reg_reg(list, size, size, src, dst)
      else
        do_lo_hi_and;
    OP_XOR:
      if (a = 0) then
        a_load_reg_reg(list, size, size, src, dst)
      else if (a = -1) then
        list.concat(taicpu.op_reg_reg(A_NOT, dst, src))
      else
        do_lo_hi(A_XORI, A_XORIS);
    OP_SHL, OP_SHR, OP_SAR:
      begin
        if (size in [OS_64, OS_S64]) then 
          shift := 6
        else
          shift := 5;
        
        shiftmask := (1 shl shift)-1;
        if (a and shiftmask) <> 0 then
          list.concat(taicpu.op_reg_reg_const(
            TShiftOpCG2AsmOpConst[size in [OS_64, OS_S64], op], dst, src, a and shiftmask))
        else
          a_load_reg_reg(list, size, size, src, dst);
        if ((a shr shift) <> 0) then
          internalError(68991);
      end
    else
      internalerror(200109091);
  end;
  { if all else failed, load the constant in a register and then
   perform the operation }
  if (useReg) then begin
    scratchreg := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
    a_load_const_reg(list, size, a, scratchreg);
    a_op_reg_reg_reg(list, op, size, scratchreg, src, dst);
  end;
end;

procedure tcgppc.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
  size: tcgsize; src1, src2, dst: tregister);
const
  op_reg_reg_opcg2asmop32: array[TOpCG] of tasmop =
  (A_NONE, A_ADD, A_AND, A_DIVWU, A_DIVW, A_MULLW, A_MULLW, A_NEG, A_NOT, A_OR,
   A_SRAW, A_SLW, A_SRW, A_SUB, A_XOR);
  op_reg_reg_opcg2asmop64: array[TOpCG] of tasmop =
  (A_NONE, A_ADD, A_AND, A_DIVDU, A_DIVD, A_MULLD, A_MULLD, A_NEG, A_NOT, A_OR,
   A_SRAD, A_SLD, A_SRD, A_SUB, A_XOR);
begin
  case op of
    OP_NEG, OP_NOT:
      begin
        list.concat(taicpu.op_reg_reg(op_reg_reg_opcg2asmop64[op], dst, src1));
        if (op = OP_NOT) and not (size in [OS_64, OS_S64]) then
          { zero/sign extend result again, fromsize is not important here }
          a_load_reg_reg(list, OS_S64, size, dst, dst)
      end;
    else
      if (size in [OS_64, OS_S64]) then begin
        list.concat(taicpu.op_reg_reg_reg(op_reg_reg_opcg2asmop64[op], dst, src2,
          src1));
      end else begin
        list.concat(taicpu.op_reg_reg_reg(op_reg_reg_opcg2asmop32[op], dst, src2,
          src1));
      end;
  end;
end;

{*************** compare instructructions ****************}

procedure tcgppc.a_cmp_const_reg_label(list: taasmoutput; size: tcgsize; 
  cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel);
var
  scratch_register: TRegister;
  signed: boolean;
begin
  signed := cmp_op in [OC_GT, OC_LT, OC_GTE, OC_LTE];
  { in the following case, we generate more efficient code when }
  { signed is true                                              }
  if (cmp_op in [OC_EQ, OC_NE]) and
    (aword(a) > $FFFF) then
    signed := true;
  if signed then
    if (a >= low(smallint)) and (a <= high(smallint)) then
      list.concat(taicpu.op_reg_reg_const(A_CMPDI, NR_CR0, reg, a))
    else begin
      scratch_register := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
      a_load_const_reg(list, OS_64, a, scratch_register);
      list.concat(taicpu.op_reg_reg_reg(A_CMPD, NR_CR0, reg, scratch_register));
    end
  else if (aword(a) <= $FFFF) then
    list.concat(taicpu.op_reg_reg_const(A_CMPLDI, NR_CR0, reg, aword(a)))
  else begin
    scratch_register := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
    a_load_const_reg(list, OS_64, a, scratch_register);
    list.concat(taicpu.op_reg_reg_reg(A_CMPLD, NR_CR0, reg,
      scratch_register));
  end;
  a_jmp(list, A_BC, TOpCmp2AsmCond[cmp_op], 0, l);
end;

procedure tcgppc.a_cmp_reg_reg_label(list: taasmoutput; size: tcgsize; 
  cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
var
  op: tasmop;
begin
  if cmp_op in [OC_GT, OC_LT, OC_GTE, OC_LTE] then
    if (size in [OS_64, OS_S64]) then
      op := A_CMPD
    else
      op := A_CMPW
  else
    if (size in [OS_64, OS_S64]) then
      op := A_CMPLD
    else
      op := A_CMPLW;
  list.concat(taicpu.op_reg_reg_reg(op, NR_CR0, reg2, reg1));
  a_jmp(list, A_BC, TOpCmp2AsmCond[cmp_op], 0, l);
end;

procedure tcgppc.a_jmp_cond(list: taasmoutput; cond: TOpCmp; l: tasmlabel);

begin
  a_jmp(list, A_BC, TOpCmp2AsmCond[cond], 0, l);
end;

procedure tcgppc.a_jmp_name(list: taasmoutput; const s: string);
var
  p: taicpu;
begin
  p := taicpu.op_sym(A_B, objectlibrary.newasmsymbol(s, AB_EXTERNAL,
    AT_LABEL));
  p.is_jmp := true;
  list.concat(p)
end;

procedure tcgppc.a_jmp_always(list: taasmoutput; l: tasmlabel);

begin
  a_jmp(list, A_B, C_None, 0, l);
end;

procedure tcgppc.a_jmp_flags(list: taasmoutput; const f: TResFlags; l:
  tasmlabel);

var
  c: tasmcond;
begin
  c := flags_to_cond(f);
  a_jmp(list, A_BC, c.cond, c.cr - RS_CR0, l);
end;

procedure tcgppc.g_flags2reg(list: taasmoutput; size: TCgSize; const f:
  TResFlags; reg: TRegister);
var
  testbit: byte;
  bitvalue: boolean;
begin
  { get the bit to extract from the conditional register + its requested value (0 or 1) }
  testbit := ((f.cr - RS_CR0) * 4);
  case f.flag of
    F_EQ, F_NE:
      begin
        inc(testbit, 2);
        bitvalue := f.flag = F_EQ;
      end;
    F_LT, F_GE:
      begin
        bitvalue := f.flag = F_LT;
      end;
    F_GT, F_LE:
      begin
        inc(testbit);
        bitvalue := f.flag = F_GT;
      end;
  else
    internalerror(200112261);
  end;
  { load the conditional register in the destination reg }
  list.concat(taicpu.op_reg(A_MFCR, reg));
  { we will move the bit that has to be tested to bit 0 by rotating left }
  testbit := (testbit + 1) and 31;
  { extract bit }
  list.concat(taicpu.op_reg_reg_const_const_const(
    A_RLWINM,reg,reg,testbit,31,31));

  { if we need the inverse, xor with 1 }
  if not bitvalue then
    list.concat(taicpu.op_reg_reg_const(A_XORI, reg, reg, 1));
end;

{ *********** entry/exit code and address loading ************ }

procedure tcgppc.g_save_standard_registers(list: Taasmoutput);
begin
  { this work is done in g_proc_entry; additionally it is not safe 
  to use it because it is called at some weird time }
end;

procedure tcgppc.g_restore_standard_registers(list: Taasmoutput);
begin
  { this work is done in g_proc_exit; mainly because it is not safe to
  put the register restore code here because it is called at some weird time }
end;

procedure tcgppc.calcFirstUsedFPR(out firstfpr : TSuperRegister; out fprcount : aint);
var
  reg : TSuperRegister;
begin
  fprcount := 0;
  firstfpr := RS_F31;
  if not (po_assembler in current_procinfo.procdef.procoptions) then
    for reg := RS_F14 to RS_F31 do
      if reg in rg[R_FPUREGISTER].used_in_proc then begin
        fprcount := ord(RS_F31)-ord(reg)+1;
        firstfpr := reg;
        break;
      end;
end;

procedure tcgppc.calcFirstUsedGPR(out firstgpr : TSuperRegister; out gprcount : aint);
var
  reg : TSuperRegister;
begin
  gprcount := 0;
  firstgpr := RS_R31;
  if not (po_assembler in current_procinfo.procdef.procoptions) then
    for reg := RS_R14 to RS_R31 do
      if reg in rg[R_INTREGISTER].used_in_proc then begin
        gprcount := ord(RS_R31)-ord(reg)+1;
        firstgpr := reg;
        break;
      end;
end;

{ Generates the entry code of a procedure/function. 
                                                                     
 This procedure may be called before, as well as after g_return_from_proc
 is called. localsize is the sum of the size necessary for local variables 
 and the maximum possible combined size of ALL the parameters of a procedure 
 called by the current one 

 IMPORTANT: registers are not to be allocated through the register
 allocator here, because the register colouring has already occured !! 
}
procedure tcgppc.g_proc_entry(list: taasmoutput; localsize: longint;
  nostackframe: boolean);
var
  firstregfpu, firstreggpr: TSuperRegister;
  needslinkreg: boolean;

  fprcount, gprcount : aint;

  { Save standard registers, both FPR and GPR; does not support VMX/Altivec }
  procedure save_standard_registers;
  var
    regcount : TSuperRegister;
    href : TReference;
    mayNeedLRStore : boolean;
  begin
    { there are two ways to do this: manually, by generating a few "std" instructions,
     or via the restore helper functions. The latter are selected by the -Og switch,
     i.e. "optimize for size" }
    if (cs_littlesize in aktglobalswitches) then begin
      mayNeedLRStore := false;
      if ((fprcount > 0) and (gprcount > 0)) then begin
        a_op_const_reg_reg(list, OP_SUB, OS_INT, 8 * fprcount, NR_R1, NR_R12);
        a_call_name_direct(list, '_savegpr1_' + intToStr(32-gprcount), false, false);
        a_call_name_direct(list, '_savefpr_' + intToStr(32-fprcount), false, false);
      end else if (gprcount > 0) then
        a_call_name_direct(list, '_savegpr0_' + intToStr(32-gprcount), false, false)
      else if (fprcount > 0) then
        a_call_name_direct(list, '_savefpr_' + intToStr(32-fprcount), false, false)
      else
        mayNeedLRStore := true;
    end else begin
      { save registers, FPU first, then GPR }
      reference_reset_base(href, NR_STACK_POINTER_REG, -8);
      if (fprcount > 0) then
        for regcount := RS_F31 downto firstregfpu do begin
          a_loadfpu_reg_ref(list, OS_FLOAT, newreg(R_FPUREGISTER, regcount,
            R_SUBNONE), href);
          dec(href.offset, tcgsize2size[OS_FLOAT]);
        end;
      if (gprcount > 0) then
        for regcount := RS_R31 downto firstreggpr do begin
          a_load_reg_ref(list, OS_INT, OS_INT, newreg(R_INTREGISTER, regcount,
            R_SUBNONE), href);
          dec(href.offset, tcgsize2size[OS_INT]);
        end;
      { VMX registers not supported by FPC atm }

      { in this branch we may always need to store LR ourselves}
      mayNeedLRStore := true;
    end;

    { we may need to store R0 (=LR) ourselves }
    if (mayNeedLRStore) and (needslinkreg) then begin
      reference_reset_base(href, NR_STACK_POINTER_REG, LA_LR_ELF);
      list.concat(taicpu.op_reg_ref(A_STD, NR_R0, href));
    end;
  end;

var
  href: treference;

begin
  calcFirstUsedFPR(firstregfpu, fprcount);
  calcFirstUsedGPR(firstreggpr, gprcount);

  { calculate real stack frame size }
  localsize := tppcprocinfo(current_procinfo).calc_stackframe_size(
    gprcount, fprcount);

  { determine whether we need to save the link register }
  needslinkreg := 
    ((not (po_assembler in current_procinfo.procdef.procoptions)) and (pi_do_call in current_procinfo.flags)) or 
    ((cs_littlesize in aktglobalswitches) and ((fprcount > 0) or (gprcount > 0)));

  a_reg_alloc(list, NR_STACK_POINTER_REG);
  a_reg_alloc(list, NR_R0);

  { move link register to r0 }
  if (needslinkreg) then
    list.concat(taicpu.op_reg(A_MFLR, NR_R0));

  save_standard_registers;

  { save old stack frame pointer }
  if (localsize > 0) then begin
    a_reg_alloc(list, NR_OLD_STACK_POINTER_REG);
    list.concat(taicpu.op_reg_reg(A_MR, NR_OLD_STACK_POINTER_REG, NR_STACK_POINTER_REG));
  end;

  { create stack frame }
  if (not nostackframe) and (localsize > 0) then begin
    if (localsize <= high(smallint)) then begin
      reference_reset_base(href, NR_STACK_POINTER_REG, -localsize);
      a_load_store(list, A_STDU, NR_STACK_POINTER_REG, href);
    end else begin
      reference_reset_base(href, NR_NO, -localsize);

      { Use R0 for loading the constant (which is definitely > 32k when entering
       this branch).

       Inlined at this position because it must not use temp registers because 
       register allocations have already been done  }
      { Code template:
      lis   r0,ofs@highest
      ori   r0,r0,ofs@higher
      sldi  r0,r0,32
      oris  r0,r0,ofs@h
      ori   r0,r0,ofs@l
      }
      list.concat(taicpu.op_reg_const(A_LIS, NR_R0, word(href.offset shr 48)));
      list.concat(taicpu.op_reg_reg_const(A_ORI, NR_R0, NR_R0, word(href.offset shr 32)));
      list.concat(taicpu.op_reg_reg_const(A_SLDI, NR_R0, NR_R0, 32));
      list.concat(taicpu.op_reg_reg_const(A_ORIS, NR_R0, NR_R0, word(href.offset shr 16)));
      list.concat(taicpu.op_reg_reg_const(A_ORI, NR_R0, NR_R0, word(href.offset)));

      list.concat(taicpu.op_reg_reg_reg(A_STDUX, NR_R1, NR_R1, NR_R0));
    end;
  end;

  { CR register not used by FPC atm }

  { keep R1 allocated??? }
  a_reg_dealloc(list, NR_R0);
end;

{ Generates the exit code for a method. 

 This procedure may be called before, as well as after g_stackframe_entry
 is called. 

 IMPORTANT: registers are not to be allocated through the register
 allocator here, because the register colouring has already occured !!
}
procedure tcgppc.g_proc_exit(list: taasmoutput; parasize: longint; nostackframe:
  boolean);
var
  firstregfpu, firstreggpr: TSuperRegister;
  needslinkreg : boolean;
  fprcount, gprcount: aint;

  { Restore standard registers, both FPR and GPR; does not support VMX/Altivec }
  procedure restore_standard_registers;
  var
    { flag indicating whether we need to manually add the exit code (e.g. blr instruction)
     or not }
    needsExitCode : Boolean;
    href : treference;
    regcount : TSuperRegister;
  begin
    { there are two ways to do this: manually, by generating a few "ld" instructions,
     or via the restore helper functions. The latter are selected by the -Og switch,
     i.e. "optimize for size" }
    if (cs_littlesize in aktglobalswitches) then begin
      needsExitCode := false;
      if ((fprcount > 0) and (gprcount > 0)) then begin
        a_op_const_reg_reg(list, OP_SUB, OS_INT, 8 * fprcount, NR_R1, NR_R12);
        a_call_name_direct(list, '_restgpr1_' + intToStr(32-gprcount), false, false);
        a_jmp_name(list, '_restfpr_' + intToStr(32-fprcount));
      end else if (gprcount > 0) then
        a_jmp_name(list, '_restgpr0_' + intToStr(32-gprcount))
      else if (fprcount > 0) then
        a_jmp_name(list, '_restfpr_' + intToStr(32-fprcount))
      else
        needsExitCode := true;
    end else begin
      needsExitCode := true;
      { restore registers, FPU first, GPR next }
      reference_reset_base(href, NR_STACK_POINTER_REG, -tcgsize2size[OS_FLOAT]);
      if (fprcount > 0) then
        for regcount := RS_F31 downto firstregfpu do begin
          a_loadfpu_ref_reg(list, OS_FLOAT, href, newreg(R_FPUREGISTER, regcount,
            R_SUBNONE));
          dec(href.offset, tcgsize2size[OS_FLOAT]);
        end;
      if (gprcount > 0) then
        for regcount := RS_R31 downto firstreggpr do begin
          a_load_ref_reg(list, OS_INT, OS_INT, href, newreg(R_INTREGISTER, regcount,
            R_SUBNONE));
          dec(href.offset, tcgsize2size[OS_INT]);
        end;

      { VMX not supported by FPC atm }
    end;

    if (needsExitCode) then begin

      { restore LR (if needed) }
      if (needslinkreg) then begin
        reference_reset_base(href, NR_STACK_POINTER_REG, LA_LR_ELF);
        list.concat(taicpu.op_reg_ref(A_LD, NR_R0, href));
        list.concat(taicpu.op_reg(A_MTLR, NR_R0));
      end;

      { generate return instruction }
      list.concat(taicpu.op_none(A_BLR));
    end;
  end;

var
  href: treference;
  localsize : aint;

begin
  calcFirstUsedFPR(firstregfpu, fprcount);
  calcFirstUsedGPR(firstreggpr, gprcount);

  { determine whether we need to restore the link register }
  needslinkreg := 
    ((not (po_assembler in current_procinfo.procdef.procoptions)) and (pi_do_call in current_procinfo.flags)) or
    ((cs_littlesize in aktglobalswitches) and ((fprcount > 0) or (gprcount > 0)));

  { calculate stack frame }
  localsize := tppcprocinfo(current_procinfo).calc_stackframe_size(
    gprcount, fprcount);

  { CR register not supported }

  { restore stack pointer }
  if (not nostackframe) and (localsize > 0) then begin
    if (localsize <= high(smallint)) then begin
      list.concat(taicpu.op_reg_reg_const(A_ADDI, NR_STACK_POINTER_REG, NR_STACK_POINTER_REG, localsize));
    end else begin
      reference_reset_base(href, NR_NO, localsize);

      { use R0 for loading the constant (which is definitely > 32k when entering
       this branch)
       Inlined because it must not use temp registers because register allocations
       have already been done
      }
      { Code template:
       lis   r0,ofs@highest
       ori   r0,ofs@higher
       sldi  r0,r0,32
       oris  r0,r0,ofs@h
       ori   r0,r0,ofs@l
      }
      list.concat(taicpu.op_reg_const(A_LIS, NR_R0, word(href.offset shr 48)));
      list.concat(taicpu.op_reg_reg_const(A_ORI, NR_R0, NR_R0, word(href.offset shr 32)));
      list.concat(taicpu.op_reg_reg_const(A_SLDI, NR_R0, NR_R0, 32));
      list.concat(taicpu.op_reg_reg_const(A_ORIS, NR_R0, NR_R0, word(href.offset shr 16)));
      list.concat(taicpu.op_reg_reg_const(A_ORI, NR_R0, NR_R0, word(href.offset)));

      list.concat(taicpu.op_reg_reg_reg(A_ADD, NR_R1, NR_R1, NR_R0));
    end;
  end;

  restore_standard_registers;
end;


procedure tcgppc.a_loadaddr_ref_reg(list: taasmoutput; const ref: treference; r:
  tregister);

var
  ref2, tmpref: treference;
  { register used to construct address }
  tempreg : TRegister;

begin
  ref2 := ref;
  fixref(list, ref2, OS_64);
  { load a symbol }
  if assigned(ref2.symbol) or (hasLargeOffset(ref2)) then begin
      { add the symbol's value to the base of the reference, and if the }
      { reference doesn't have a base, create one                       }
      reference_reset(tmpref);
      tmpref.offset := ref2.offset;
      tmpref.symbol := ref2.symbol;
      tmpref.relsymbol := ref2.relsymbol;
      { load 64 bit reference into r. If the reference already has a base register,
       first load the 64 bit value into a temp register, then add it to the result
       register rD }
      if (ref2.base <> NR_NO) then begin
        { already have a base register, so allocate a new one }
        tempreg := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
      end else begin
        tempreg := r;
      end;

      { code for loading a reference from a symbol into a register rD }
      (*
      lis   rX,SYM@highest
      ori   rX,SYM@higher
      sldi  rX,rX,32
      oris  rX,rX,SYM@h
      ori   rX,rX,SYM@l
      *)
      tmpref.refaddr := addr_highest;
      list.concat(taicpu.op_reg_ref(A_LIS, tempreg, tmpref));
      tmpref.refaddr := addr_higher;
      list.concat(taicpu.op_reg_reg_ref(A_ORI, tempreg, tempreg, tmpref));
      list.concat(taicpu.op_reg_reg_const(A_SLDI, tempreg, tempreg, 32));
      tmpref.refaddr := addr_high;
      list.concat(taicpu.op_reg_reg_ref(A_ORIS, tempreg, tempreg, tmpref));
      tmpref.refaddr := addr_low;
      list.concat(taicpu.op_reg_reg_ref(A_ORI, tempreg, tempreg, tmpref));

      { if there's already a base register, add the temp register contents to
       the base register }
      if (ref2.base <> NR_NO) then begin
        list.concat(taicpu.op_reg_reg_reg(A_ADD, r, tempreg, ref2.base));
      end;
  end else if ref2.offset <> 0 then begin
    { no symbol, but offset <> 0 }
    if ref2.base <> NR_NO then begin
      a_op_const_reg_reg(list, OP_ADD, OS_64, ref2.offset, ref2.base, r)
      { FixRef makes sure that "(ref.index <> R_NO) and (ref.offset <> 0)" never
       occurs, so now only ref.offset has to be loaded }
    end else begin
      a_load_const_reg(list, OS_64, ref2.offset, r)
    end;
  end else if ref.index <> NR_NO then
    list.concat(taicpu.op_reg_reg_reg(A_ADD, r, ref2.base, ref2.index))
  else if (ref2.base <> NR_NO) and
    (r <> ref2.base) then
    a_load_reg_reg(list, OS_ADDR, OS_ADDR, ref2.base, r)
  else begin
    list.concat(taicpu.op_reg_const(A_LI, r, 0));
  end;
end;

{ ************* concatcopy ************ }

const
  maxmoveunit = 8;


procedure tcgppc.g_concatcopy(list: taasmoutput; const source, dest: treference;
  len: aint);

var
  countreg, tempreg: TRegister;
  src, dst: TReference;
  lab: tasmlabel;
  count, count2: longint;
  size: tcgsize;

begin
{$IFDEF extdebug}
  if len > high(aint) then
    internalerror(2002072704);
  list.concat(tai_comment.create(strpnew('g_concatcopy')));
{$ENDIF extdebug}
  { make sure short loads are handled as optimally as possible;
   note that the data here never overlaps, so we can do a forward
   copy at all times.
   NOTE: maybe use some scratch registers to pair load/store instructions
  }

  if (len <= maxmoveunit) then begin
    src := source; dst := dest;
    while (len <> 0) do begin
      if (len = 8) then begin
        a_load_ref_ref(list, OS_64, OS_64, src, dst);    
        dec(len, 8);
      end else if (len >= 4) then begin
        a_load_ref_ref(list, OS_32, OS_32, src, dst);    
        inc(src.offset, 4); inc(dst.offset, 4);
        dec(len, 4);
      end else if (len >= 2) then begin
        a_load_ref_ref(list, OS_16, OS_16, src, dst);    
        inc(src.offset, 2); inc(dst.offset, 2);
        dec(len, 2);
      end else begin
        a_load_ref_ref(list, OS_8, OS_8, src, dst);    
        inc(src.offset, 1); inc(dst.offset, 1);
        dec(len, 1);
      end;
    end;
    exit;
  end;

  count := len div maxmoveunit;

  reference_reset(src);
  reference_reset(dst);
  { load the address of source into src.base }
  if (count > 4) or
    not issimpleref(source) or
    ((source.index <> NR_NO) and
    ((source.offset + len) > high(smallint))) then begin
    src.base := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
    a_loadaddr_ref_reg(list, source, src.base);
  end else begin
    src := source;
  end;
  { load the address of dest into dst.base }
  if (count > 4) or
    not issimpleref(dest) or
    ((dest.index <> NR_NO) and
    ((dest.offset + len) > high(smallint))) then begin
    dst.base := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
    a_loadaddr_ref_reg(list, dest, dst.base);
  end else begin
    dst := dest;
  end;

  { generate a loop }
  if count > 4 then begin
    { the offsets are zero after the a_loadaddress_ref_reg and just 
     have to be set to 8. I put an Inc there so debugging may be   
     easier (should offset be different from zero here, it will be 
     easy to notice in the generated assembler }
    inc(dst.offset, 8);
    inc(src.offset, 8);
    list.concat(taicpu.op_reg_reg_const(A_SUBI, src.base, src.base, 8));
    list.concat(taicpu.op_reg_reg_const(A_SUBI, dst.base, dst.base, 8));
    countreg := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
    a_load_const_reg(list, OS_64, count, countreg);
    { explicitely allocate F0 since it can be used safely here
     (for holding date that's being copied) }
    a_reg_alloc(list, NR_F0);
    objectlibrary.getjumplabel(lab);
    a_label(list, lab);
    list.concat(taicpu.op_reg_reg_const(A_SUBIC_, countreg, countreg, 1));
    list.concat(taicpu.op_reg_ref(A_LFDU, NR_F0, src));
    list.concat(taicpu.op_reg_ref(A_STFDU, NR_F0, dst));
    a_jmp(list, A_BC, C_NE, 0, lab);
    a_reg_dealloc(list, NR_F0);
    len := len mod 8;
  end;

  count := len div 8;
  { unrolled loop }
  if count > 0 then begin
    a_reg_alloc(list, NR_F0);
    for count2 := 1 to count do begin
      a_loadfpu_ref_reg(list, OS_F64, src, NR_F0);
      a_loadfpu_reg_ref(list, OS_F64, NR_F0, dst);
      inc(src.offset, 8);
      inc(dst.offset, 8);
    end;
    a_reg_dealloc(list, NR_F0);
    len := len mod 8;
  end;

  if (len and 4) <> 0 then begin
    a_reg_alloc(list, NR_R0);
    a_load_ref_reg(list, OS_32, OS_32, src, NR_R0);
    a_load_reg_ref(list, OS_32, OS_32, NR_R0, dst);
    inc(src.offset, 4);
    inc(dst.offset, 4);
    a_reg_dealloc(list, NR_R0);
  end;
  { copy the leftovers }
  if (len and 2) <> 0 then begin
    a_reg_alloc(list, NR_R0);
    a_load_ref_reg(list, OS_16, OS_16, src, NR_R0);
    a_load_reg_ref(list, OS_16, OS_16, NR_R0, dst);
    inc(src.offset, 2);
    inc(dst.offset, 2);
    a_reg_dealloc(list, NR_R0);
  end;
  if (len and 1) <> 0 then begin
    a_reg_alloc(list, NR_R0);
    a_load_ref_reg(list, OS_8, OS_8, src, NR_R0);
    a_load_reg_ref(list, OS_8, OS_8, NR_R0, dst);
    a_reg_dealloc(list, NR_R0);
  end;

end;

procedure tcgppc.g_overflowcheck(list: taasmoutput; const l: tlocation; def:
  tdef);
var
  hl: tasmlabel;
  flags : TResFlags;
begin
  if not (cs_check_overflow in aktlocalswitches) then
    exit;
  objectlibrary.getjumplabel(hl);
  if not ((def.deftype = pointerdef) or
    ((def.deftype = orddef) and
    (torddef(def).typ in [u64bit, u16bit, u32bit, u8bit, uchar,
    bool8bit, bool16bit, bool32bit]))) then
  begin
    { ... instructions setting overflow flag ...
     mfxerf R0
     mtcrf 128, R0
     ble cr0, label }
    list.concat(taicpu.op_reg(A_MFXER, NR_R0));
    list.concat(taicpu.op_const_reg(A_MTCRF, 128, NR_R0));
    flags.cr := RS_CR0;
    flags.flag := F_LE;
    a_jmp_flags(list, flags, hl);
  end else
    a_jmp_cond(list, OC_AE, hl);
  a_call_name(list, 'FPC_OVERFLOW');
  a_label(list, hl);
end;

procedure tcgppc.g_intf_wrapper(list: TAAsmoutput; procdef: tprocdef; const
  labelname: string; ioffset: longint);

  procedure loadvmttor11;
  var
    href: treference;
  begin
    reference_reset_base(href, NR_R3, 0);
    cg.a_load_ref_reg(list, OS_ADDR, OS_ADDR, href, NR_R11);
  end;

  procedure op_onr11methodaddr;
  var
    href: treference;
  begin
    if (procdef.extnumber = $FFFF) then
      Internalerror(200006139);
    { call/jmp  vmtoffs(%eax) ; method offs }
    reference_reset_base(href, NR_R11,
      procdef._class.vmtmethodoffset(procdef.extnumber));
    if not (hasLargeOffset(href)) then begin
      list.concat(taicpu.op_reg_reg_const(A_ADDIS, NR_R11, NR_R11,
        smallint((href.offset shr 16) + ord(smallint(href.offset and $FFFF) <
        0))));
      href.offset := smallint(href.offset and $FFFF);
    end else
      { add support for offsets > 16 bit }
      internalerror(200510201);
    list.concat(taicpu.op_reg_ref(A_LD, NR_R11, href));
    { the loaded reference is a function descriptor reference, so deref again
     (at ofs 0 there's the real pointer) }
    {$warning ts:TODO: update GOT reference}
    reference_reset_base(href, NR_R11, 0);
    list.concat(taicpu.op_reg_ref(A_LD, NR_R11, href));

    list.concat(taicpu.op_reg(A_MTCTR, NR_R11));
    list.concat(taicpu.op_none(A_BCTR));
    { NOP needed for the linker...? }
    list.concat(taicpu.op_none(A_NOP));
  end;

var
  make_global: boolean;
begin
  if (not (procdef.proctypeoption in [potype_function, potype_procedure])) then
    Internalerror(200006137);
  if not assigned(procdef._class) or
    (procdef.procoptions * [po_classmethod, po_staticmethod,
    po_methodpointer, po_interrupt, po_iocheck] <> []) then
    Internalerror(200006138);
  if procdef.owner.symtabletype <> objectsymtable then
    Internalerror(200109191);

  make_global := false;
  if (not current_module.is_unit) or
    (cs_create_smart in aktmoduleswitches) or
    (procdef.owner.defowner.owner.symtabletype = globalsymtable) then
    make_global := true;

  if make_global then
    List.concat(Tai_symbol.Createname_global(labelname, AT_FUNCTION, 0))
  else
    List.concat(Tai_symbol.Createname(labelname, AT_FUNCTION, 0));

  { set param1 interface to self  }
  g_adjust_self_value(list, procdef, ioffset);

  if po_virtualmethod in procdef.procoptions then begin
    loadvmttor11;
    op_onr11methodaddr;
  end else
    {$note ts:todo add GOT change?? - think not needed :) }
    list.concat(taicpu.op_sym(A_B,
      objectlibrary.newasmsymbol('.' + procdef.mangledname, AB_EXTERNAL,
      AT_FUNCTION)));

  List.concat(Tai_symbol_end.Createname(labelname));
end;

{***************** This is private property, keep out! :) *****************}

function tcgppc.issimpleref(const ref: treference): boolean;

begin
  if (ref.base = NR_NO) and
    (ref.index <> NR_NO) then
    internalerror(200208101);
  result :=
    not (assigned(ref.symbol)) and
    (((ref.index = NR_NO) and
    (ref.offset >= low(smallint)) and
    (ref.offset <= high(smallint))) or
    ((ref.index <> NR_NO) and
    (ref.offset = 0)));
end;

function tcgppc.fixref(list: taasmoutput; var ref: treference; const size : TCgsize): boolean;
var
  tmpreg: tregister;
  needsAlign : boolean;
begin
  result := false;
  needsAlign := size in [OS_S32, OS_64, OS_S64];

  if (ref.base = NR_NO) then  begin
    ref.base := ref.index;
    ref.index := NR_NO;
  end;
  if (ref.base <> NR_NO) and (ref.index <> NR_NO) and
    ((ref.offset <> 0) or assigned(ref.symbol)) then begin
      result := true;
      tmpreg := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
      a_op_reg_reg_reg(list, OP_ADD, size, ref.base, ref.index, tmpreg);
      ref.index := NR_NO;
      ref.base := tmpreg;
  end;
end;

procedure tcgppc.a_load_store(list: taasmoutput; op: tasmop; reg: tregister;
  ref: treference);
var
  tmpreg, tmpreg2: tregister;
  tmpref: treference;
  largeOffset: Boolean;
begin
  { at this point there must not be a combination of values in the ref treference
    which is not possible to directly map to instructions of the PowerPC architecture }
  if (ref.index <> NR_NO) and ((ref.offset <> 0) or (assigned(ref.symbol))) then
    internalerror(200310131);
 
  { for some instructions we need to check that the offset is divisible by at
   least four. If not, add the bytes which are "off" to the base register and
   adjust the offset accordingly }
  case op of
    A_LD, A_LDU, A_STD, A_STDU, A_LWA :
     if ((ref.offset mod 4) <> 0) then begin
       tmpreg := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);

       if (ref.base <> NR_NO) then begin
         a_op_const_reg_reg(list, OP_ADD, OS_ADDR, ref.offset mod 4, ref.base, tmpreg);
         ref.base := tmpreg;
       end else begin
         list.concat(taicpu.op_reg_const(A_LI, tmpreg, ref.offset mod 4));
         ref.base := tmpreg;
       end;
       ref.offset := (ref.offset div 4) * 4;
     end;
  end;

  { if we have to load/store from a symbol or large addresses, use a temporary register
   containing the address }
  if assigned(ref.symbol) or (hasLargeOffset(ref)) then begin
    tmpreg := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);

    if (hasLargeOffset(ref) and (ref.base = NR_NO)) then begin
      ref.base := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
      a_load_const_reg(list, OS_ADDR, ref.offset, ref.base);
      ref.offset := 0;
    end;

    reference_reset(tmpref);
    tmpref.symbol := ref.symbol;
    tmpref.relsymbol := ref.relsymbol;
    tmpref.offset := ref.offset;

    if (ref.base <> NR_NO) then begin
      { As long as the TOC isn't working we try to achieve highest speed (in this
      case by allowing instructions execute in parallel) as possible at the cost
      of using another temporary register. So the code template when there is
      a base register and an offset is the following:

      lis rT1, SYM+offs@highest
      ori rT1, rT1, SYM+offs@higher
      lis rT2, SYM+offs@hi
      ori rT2, SYM+offs@lo
      rldimi rT2, rT1, 32

      <op>X reg, base, rT2
      }

      tmpreg2 := rg[R_INTREGISTER].getregister(list, R_SUBWHOLE);
      tmpref.refaddr := addr_highest;
      list.concat(taicpu.op_reg_ref(A_LIS, tmpreg, tmpref));
      tmpref.refaddr := addr_higher;
      list.concat(taicpu.op_reg_reg_ref(A_ORI, tmpreg, tmpreg, tmpref));

      tmpref.refaddr := addr_high;
      list.concat(taicpu.op_reg_ref(A_LIS, tmpreg2, tmpref));
      tmpref.refaddr := addr_low;
      list.concat(taicpu.op_reg_reg_ref(A_ORI, tmpreg2, tmpreg2, tmpref));

      list.concat(taicpu.op_reg_reg_const_const(A_RLDIMI, tmpreg2, tmpreg, 32, 0));

      reference_reset(tmpref);
      tmpref.base := ref.base;
      tmpref.index := tmpreg2;
      case op of
        { the code generator doesn't generate update instructions anyway, so 
        error out on those instructions }
        A_LBZ : op := A_LBZX;
        A_LHZ : op := A_LHZX;
        A_LWZ : op := A_LWZX;
        A_LD : op := A_LDX;
        A_LHA : op := A_LHAX;
        A_LWA : op := A_LWAX;
        A_LFS : op := A_LFSX;
        A_LFD : op := A_LFDX;

        A_STB : op := A_STBX;
        A_STH : op := A_STHX;
        A_STW : op := A_STWX;
        A_STD : op := A_STDX;

        A_STFS : op := A_STFSX;
        A_STFD : op := A_STFDX;
        else
          { unknown load/store opcode }
          internalerror(2005101302);
      end;
      list.concat(taicpu.op_reg_ref(op, reg, tmpref));
    end else begin
      { when accessing value from a reference without a base register, use the
        following code template:

        lis rT,SYM+offs@highesta
        ori rT,SYM+offs@highera
        sldi rT,rT,32
        oris rT,rT,SYM+offs@ha
        ld rD,SYM+offs@l(rT)
      }
      tmpref.refaddr := addr_highesta;
      list.concat(taicpu.op_reg_ref(A_LIS, tmpreg, tmpref));
      tmpref.refaddr := addr_highera;
      list.concat(taicpu.op_reg_reg_ref(A_ORI, tmpreg, tmpreg, tmpref));
      list.concat(taicpu.op_reg_reg_const(A_SLDI, tmpreg, tmpreg, 32));
      tmpref.refaddr := addr_higha;
      list.concat(taicpu.op_reg_reg_ref(A_ORIS, tmpreg, tmpreg, tmpref));

      tmpref.base := tmpreg;
      tmpref.refaddr := addr_low;
      list.concat(taicpu.op_reg_ref(op, reg, tmpref));
    end;
  end else begin
    list.concat(taicpu.op_reg_ref(op, reg, ref));
  end;
end;

procedure tcgppc.a_jmp(list: taasmoutput; op: tasmop; c: tasmcondflag;
  crval: longint; l: tasmlabel);
var
  p: taicpu;

begin
  p := taicpu.op_sym(op, objectlibrary.newasmsymbol(l.name, AB_EXTERNAL,
    AT_LABEL));
  if op <> A_B then
    create_cond_norm(c, crval, p.condition);
  p.is_jmp := true;
  list.concat(p)
end;

function tcgppc.hasLargeOffset(const ref : TReference) : Boolean;
begin
  { this rather strange calculation is required because offsets of TReferences are unsigned }
  result := aword(ref.offset-low(smallint)) > high(smallint)-low(smallint);
end;

begin
  cg := tcgppc.create;
end.

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 the Free Pascal development team.

    i386 specific signal handler structure

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*
 * Architecture-specific structure passed to signal handlers
 *}

{$PACKRECORDS C}
type
  packed_fp_stack = record
    st0 : array[0..9] of byte;
    st1 : array[0..9] of byte;
    st2 : array[0..9] of byte;
    st3 : array[0..9] of byte;
    st4 : array[0..9] of byte;
    st5 : array[0..9] of byte;
    st6 : array[0..9] of byte;
    st7 : array[0..9] of byte;
  end;

  packed_mmx_regs = record
    mm0 : array[0..9] of byte;
    mm1 : array[0..9] of byte;
    mm2 : array[0..9] of byte;
    mm3 : array[0..9] of byte;
    mm4 : array[0..9] of byte;
    mm5 : array[0..9] of byte;
    mm6 : array[0..9] of byte;
    mm7 : array[0..9] of byte;
  end;

  old_extended_regs = record
    fp_control: word;
    _reserved1: word;
    fp_status: word;
    _reserved2: word;
    fp_tag: word;
    _reserved3: word;
    fp_eip: cardinal;
    fp_cs: word;
    fp_opcode: word;
    fp_datap: dword;
    fp_ds: word;
    _reserved4: word;
    fp_mmx : record
      case fp_mmx : byte of
        0 : (fp: packed_fp_stack);
        1 : (mmx: packed_mmx_regs);
    end;
  end;

  fp_stack = record
    st0 : array[0..9] of byte;
    _reserved_42_47 : array[0..5] of byte;
    st1 : array[0..9] of byte;
    _reserved_58_63 : array[0..5] of byte;
    st2 : array[0..9] of byte;
    _reserved_74_79 : array[0..5] of byte;
    st3 : array[0..9] of byte;
    _reserved_90_95 : array[0..5] of byte;
    st4 : array[0..9] of byte;
    _reserved_106_111 : array[0..5] of byte;
    st5 : array[0..9] of byte;
    _reserved_122_127 : array[0..5] of byte;
    st6 : array[0..9] of byte;    
    _reserved_138_143 : array[0..5] of byte;
    st7 : array[0..9] of byte;        
    _reserved_154_159 : array[0..5] of byte;
  end;

  mmx_regs = record
    mm0 : array[0..9] of byte;
    _reserved_42_47 : array[0..5] of byte;
    mm1 : array[0..9] of byte;
    _reserved_58_63 : array[0..5] of byte;
    mm2 : array[0..9] of byte;
    _reserved_74_79 : array[0..5] of byte;
    mm3 : array[0..9] of byte;
    _reserved_90_95 : array[0..5] of byte;
    mm4 : array[0..9] of byte;
    _reserved_106_111 : array[0..5] of byte;
    mm5 : array[0..9] of byte;
    _reserved_122_127 : array[0..5] of byte;
    mm6 : array[0..9] of byte;
    _reserved_138_143 : array[0..5] of byte;
    mm7 : array[0..9] of byte;
    _reserved_154_159 : array[0..5] of byte;
  end;

  xmmx_regs = record
    xmm0 : array [0..15] of byte;
    xmm1 : array [0..15] of byte;
    xmm2 : array [0..15] of byte;
    xmm3 : array [0..15] of byte;
    xmm4 : array [0..15] of byte;
    xmm5 : array [0..15] of byte;
    xmm6 : array [0..15] of byte;
    xmm7 : array [0..15] of byte;
  end;

  new_extended_regs = record
    fp_control: word;
    fp_status: word;
    fp_tag: word;
    fp_opcode: word;
    fp_eip: dword;
    fp_cs: word;
    res_14_15: word;
    fp_datap: dword;
    fp_ds: word;
    _reserved_22_23: word;
    mxcsr: dword;
    _reserved_28_31: dword;
    fp_mmx : record
      case byte of
        0 : (fp : fp_stack);
        1 : (mmx : mmx_regs);
    end;
    xmmx: xmmx_regs;
    _reserved_288_511 : array[0..223] of byte;
  end;

  extended_regs = record
    state : record
      case byte of
        0 : (old_format : old_extended_regs);
        1 : (new_format : new_extended_regs);
      end;
    format: dword;
  end;

  vregs = record
    eip: dword;
    eflags: dword;
    eax: dword;
    ecx: dword;
    edx: dword;
    esp: dword;
    ebp: dword;
    _reserved_1: dword;
    xregs: extended_regs;
    edi: dword;
    esi: dword;
    ebx: dword;
  end;

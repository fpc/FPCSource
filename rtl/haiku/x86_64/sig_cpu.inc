{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 the Free Pascal development team.

    x86_64 specific signal handler structure

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
  fp_stack = record
    st0: array[0..9] of byte;
    _reserved_42_47: array[0..5] of byte;
    st1: array[0..9] of byte;
    _reserved_58_63: array[0..5] of byte;
    st2: array[0..9] of byte;
    _reserved_74_79: array[0..5] of byte;
    st3: array[0..9] of byte;
    _reserved_90_95: array[0..5] of byte;
    st4: array[0..9] of byte;
    _reserved_106_111: array[0..5] of byte;
    st5: array[0..9] of byte;
    _reserved_122_127: array[0..5] of byte;
    st6: array[0..9] of byte;
    _reserved_138_143: array[0..5] of byte;
    st7: array[0..9] of byte;
    _reserved_154_159: array[0..5] of byte;
  end;

  mmx_regs = record
    mm0: array[0..9] of byte;
    _reserved_42_47: array[0..5] of byte;
    mm1: array[0..9] of byte;
    _reserved_58_63: array[0..5] of byte;
    mm2: array[0..9] of byte;
    _reserved_74_79: array[0..5] of byte;
    mm3: array[0..9] of byte;
    _reserved_90_95: array[0..5] of byte;
    mm4: array[0..9] of byte;
    _reserved_106_111: array[0..5] of byte;
    mm5: array[0..9] of byte;
    _reserved_122_127: array[0..5] of byte;
    mm6: array[0..9] of byte;
    _reserved_138_143: array[0..5] of byte;
    mm7: array[0..9] of byte;
    _reserved_154_159: array[0..5] of byte;
  end;

  xmm_regs = record
    xmm0: array[0..15] of byte;
    xmm1: array[0..15] of byte;
    xmm2: array[0..15] of byte;
    xmm3: array[0..15] of byte;
    xmm4: array[0..15] of byte;
    xmm5: array[0..15] of byte;
    xmm6: array[0..15] of byte;
    xmm7: array[0..15] of byte;
    xmm8: array[0..15] of byte;
    xmm9: array[0..15] of byte;
    xmm10: array[0..15] of byte;
    xmm11: array[0..15] of byte;
    xmm12: array[0..15] of byte;
    xmm13: array[0..15] of byte;
    xmm14: array[0..15] of byte;
    xmm15: array[0..15] of byte;
  end;

  fpu_state = record
    control: word;
    status: word;
    tag: word;
    opcode: word;
    rip: qword;
    rdp: qword;
    mxcsr: dword;
    mscsr_mask: dword;
    fp_mmx : record
      case byte of
        0: (fp: fp_stack);
        1: (mmx: mmx_regs);
    end;
    xmm: xmm_regs;
    _reserved_416_511: array[0..95] of byte;
  end;

  vregs = record
    rax: qword;
    rbx: qword;
    rcx: qword;
    rdx: qword;
    rdi: qword;
    rsi: qword;
    rbp: qword;
    r8: qword;
    r9: qword;
    r10: qword;
    r11: qword;
    r12: qword;
    r13: qword;
    r14: qword;
    r15: qword;

    rsp: qword;
    rip: qword;
    rflags: qword;

    fpu: fpu_state;
  end;

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Thomas Schatzl,
    member of the FreePascal development team

    TSigContext and associated structures.

    See also in the *kernel* sources arch/ppc64/kernel/signal.c, 
    function setup_rt_sigframe()  for more information about the 
    passed structures.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

type
  gpr_reg = cULong;
  fpr_reg = double;
  vvr_reg = array[0..1] of cULong;

type
  { from include/asm-ppc64/ptrace.h }
  ppt_regs = ^pt_regs;
  pt_regs = record
    gpr : array[0..31] of gpr_reg;
    nip : gpr_reg;
    msr : gpr_reg;
    orig_gpr3 : gpr_reg; { Used for restarting system calls  }
    ctr : gpr_reg;
    link : gpr_reg;
    xer : gpr_reg;
    ccr : gpr_reg;
    softe : gpr_reg;     { Soft enabled/disabled  }
    trap : gpr_reg;      { Reason for being here  }
    dar : gpr_reg;       { Fault registers  }
    dsisr : gpr_reg;
    result : gpr_reg;    { Result of a system call  }
  end;

{ index constants for the different register set arrays in TSigContext.
  Comments were directly pasted from the sources.
}
const
  PT_R0 = 0;       
  PT_R1 = 1;  
  PT_R2 = 2;  
  PT_R3 = 3;  
  PT_R4 = 4;  
  PT_R5 = 5;  
  PT_R6 = 6;  
  PT_R7 = 7;  
  PT_R8 = 8;  
  PT_R9 = 9;  
  PT_R10 = 10;  
  PT_R11 = 11;  
  PT_R12 = 12;  
  PT_R13 = 13;  
  PT_R14 = 14;  
  PT_R15 = 15;  
  PT_R16 = 16;  
  PT_R17 = 17;  
  PT_R18 = 18;  
  PT_R19 = 19;  
  PT_R20 = 20;  
  PT_R21 = 21;  
  PT_R22 = 22;  
  PT_R23 = 23;  
  PT_R24 = 24;  
  PT_R25 = 25;  
  PT_R26 = 26;  
  PT_R27 = 27;  
  PT_R28 = 28;  
  PT_R29 = 29;  
  PT_R30 = 30;  
  PT_R31 = 31;  
  PT_NIP = 32;  
  PT_MSR = 33;  
  PT_CTR = 35;  
  PT_LNK = 36;  
  PT_XER = 37;  
  PT_CCR = 38;  
  PT_SOFTE = 39;  
  PT_RESULT = 43;  
  PT_FPR0 = 48;  
  PT_FPR31 = PT_FPR0+31;  
  { Kernel and userspace will both use this PT_FPSCR value.  32-bit apps will have
   visibility to the asm-ppc/ptrace.h header instead of this one. }
  { each FP reg occupies 1 slot in 64-bit space  }
  PT_FPSCR = PT_FPR0+32;  
  { each Vector reg occupies 2 slots in 64-bit  }
  PT_VR0 = 82;  
  PT_VSCR = (PT_VR0+(32*2))+1;  
  PT_VRSAVE = PT_VR0+(33*2);  
  
  { from include/asm-ppc64/signal.h }
type
  stack_t = record
    ss_sp : pointer;
    ss_flags : cInt;
    ss_size : size_t;
  end;

  { from include/asm-ppc64/sigcontext.h and 
    include/asm-ppc64/elf.h
  }
const
  ELF_NGREG = 48;    { includes nip, msr, lr, etc. }
  ELF_NFPREG = 33;   { includes fpscr }
  ELF_NVRREG = 34;   { includes vscr & vrsave in split vectors }

type
  elf_gregset_t = array[0..ELF_NGREG-1] of gpr_reg;
  elf_fpregset_t = array[0..ELF_NFPREG-1] of fpr_reg;
  
  elf_vrreg_t = array[0..ELF_NVRREG-1] of vvr_reg;

  TSigContext = record
    _unused : array[0..3] of cULong;
    signal : cInt;
    _pad0 : cInt;
    handler : cULong;
    oldmask : cULong;
    regs : ppt_regs;
    gp_regs : elf_gregset_t;
    fp_regs : elf_fpregset_t;

 { To maintain compatibility with current implementations the sigcontext is 
   extended by appending a pointer (v_regs) to a quadword type (elf_vrreg_t) 
   followed by an unstructured (vmx_reserve) field of 69 doublewords.  This 
   allows the array of vector registers to be quadword aligned independent of 
   the alignment of the containing sigcontext or ucontext. It is the 
   responsibility of the code setting the sigcontext to set this pointer to 
   either NULL (if this processor does not support the VMX feature) or the 
   address of the first quadword within the allocated (vmx_reserve) area.

   The pointer (v_regs) of vector type (elf_vrreg_t) is type compatible with 
   an array of 34 quadword entries (elf_vrregset_t).  The entries with 
   indexes 0-31 contain the corresponding vector registers.  The entry with 
   index 32 contains the vscr as the last word (offset 12) within the 
   quadword.  This allows the vscr to be stored as either a quadword (since 
   it must be copied via a vector register to/from storage) or as a word.  
   The entry with index 33 contains the vrsave as the first word (offset 0) 
   within the quadword. }
    v_regs : ^elf_vrreg_t;
    vmx_reserve : array[0..ELF_NVRREG+ELF_NVRREG] of cLong;
  end;

  { the kernel uses a different sigset_t type for the ucontext structure and the
    sigset_t used for masking signals. To avoid name clash, and still use a dedicated
    type for the fields, use _sigset_t }
  _sigset_t = cULong;
  
  { from include/asm-ppc64/ucontext.h }
  pucontext = ^tucontext;
  tucontext = record
    uc_flags : cuLong;
    uc_link : pucontext;
    uc_stack : stack_t;
    uc_sigmask : _sigset_t;
    __unused : array[0..14] of _sigset_t; { Allow for uc_sigmask growth }
    uc_mcontext : TSigContext;            { last for extensibility }
  end;

  PSigContext = ^TUContext;

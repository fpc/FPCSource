{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Contains the basic declarations for the x86-64 architecture

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
{ This include file contains the basic declarations for the x86-64 architecture.
}

{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

type
  topsize = (S_NO,
    S_B,S_W,S_L,S_BW,S_BL,S_WL,S_BQ,S_WQ,S_LQ,
    S_IS,S_IL,S_IQ,
    S_FS,S_FL,S_FX,S_D,S_Q,S_FV,S_FXX,
    S_NEAR,S_FAR,S_SHORT
  );

{*****************************************************************************
                                  Registers
*****************************************************************************}

const
  { Standard opcode string table (for each tasmop enumeration). The
    opcode strings should conform to the names as defined by the
    processor manufacturer.
  }
  std_op2str:op2strtable={$i x86_64in.inc}

  { Standard register table (for each tregister enumeration). The
    register strings should conform to the the names as defined
    by the processor manufacturer
  }
  std_reg2str : reg2strtable = ('',
    'rax','rcx','rdx','rbx','rsp','rbp','rsi','rdi',
    'r8','r9','r10','r11','r12','r13','r14','r15','rip',
    'eax','ecx','edx','ebx','esp','ebp','esi','edi',
    'r8d','r9d','r10d','r11d','r12d','r13d','r14d','r15d',
    'ax','cx','dx','bx','sp','bp','si','di',
    'r8w','r9w','r10w','r11w','r12w','r13w','r14w','r15w',
    'al','cl','dl','bl','spl','bpl','sil','dil',
    'r8b','r9b','r10b','r11b','r12b','r13b','r14b','r15b',
    'ah','ch','bh','dh',
    'cs','ds','es','ss','fs','gs',
    'st','st(0)','st(1)','st(2)','st(3)','st(4)','st(5)','st(6)','st(7)',
    'dr0','dr1','dr2','dr3','dr6','dr7',
    'cr0','cr2','cr3','cr4',
    'tr3','tr4','tr5','tr6','tr7',
    'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
    'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7',
    'xmm8','xmm9','xmm10','xmm11','xmm12','xmm13','xmm14','xmm15');

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      firstsaveintreg = R_EAX;
      lastsaveintreg  = R_R15;
      firstsavefpureg = R_NO;
      lastsavefpureg  = R_NO;
      firstsavemmreg  = R_XMM0;
      lastsavemmreg   = R_XMM15;

      general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];
      general_superregisters = [RS_RAX,RS_RBX,RS_RCX,RS_RDX];

{$ifdef newra}
      usableregsint = [first_imreg..last_imreg];
{$else}
      usableregsint = [RS_RAX,RS_RBX,RS_RCX,RS_RDX];
{$endif}
      c_countusableregsint = 4;

      maxaddrregs = 1;
      addrregs    = [R_ESI];
      usableregsaddr = [RS_RSI];
      c_countusableregsaddr = 1;

      maxvarregs = 4;
      varregs : array[1..maxvarregs] of Toldregister =
         (R_EBX,R_EDX,R_ECX,R_EAX);

      maxfpuvarregs = 8;

      {# Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
      }
{$ifndef newra}
      max_scratch_regs = 1;
      scratch_regs : array[1..max_scratch_regs] of Tsuperregister = (RS_RDI);
{$endif}
{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      { Defines the default address size for a processor, }
      OS_ADDR = OS_64;
      { the natural int size for a processor,             }
      OS_INT = OS_64;
      { the maximum float size for a processor,           }
      OS_FLOAT = OS_F80;
      { the size of a vector register for a processor     }
      OS_VECTOR = OS_M64;

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      stack_pointer_reg = R_RSP;
      NR_STACK_POINTER_REG = NR_RSP;
      {# Frame pointer register }
      frame_pointer_reg = R_RBP;
      NR_FRAME_POINTER_REG = NR_RBP;
      { Register for addressing absolute data in a position independant way,
        such as in PIC code. The exact meaning is ABI specific. For
        further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      pic_offset_reg = R_EBX;
      accumulator = R_RAX;
      RS_ACCUMULATOR = RS_RAX;
      NR_ACCUMULATOR = NR_RAX;
      { the return_result_reg, is used inside the called function to store its return
        value when that is a scalar value otherwise a pointer to the address of the
        result is placed inside it }
      return_result_reg   = accumulator;
      RS_RETURN_RESULT_REG = RS_ACCUMULATOR;
      NR_RETURN_RESULT_REG = NR_ACCUMULATOR;

      { the function_result_reg contains the function result after a call to a scalar
        function othewise it contains a pointer to the returned result}
      function_result_reg = accumulator;

      fpu_result_reg = R_ST;
      mmresultreg = R_MM0;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

    const
      { Registers which must be saved when calling a routine declared as
        cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
        saved should be the ones as defined in the target ABI and / or GCC.

        This value can be deduced from the CALLED_USED_REGISTERS array in the
        GCC source.
      }
      std_saved_registers = [R_ESI,R_EDI,R_EBX];
      { Required parameter alignment when calling a routine declared as
        stdcall and cdecl. The alignment value should be the one defined
        by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 8;

{
  $Log$
  Revision 1.2  2002-04-25 16:12:09  florian
    * fixed more problems with cpubase and x86-64

  Revision 1.1  2003/04/25 11:12:09  florian
    * merged i386/cpubase and x86_64/cpubase to x86/cpubase;
      different stuff went to cpubase.inc
}

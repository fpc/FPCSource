{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Contains the basic declarations for the i386 architecture

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
{ This include file contains the basic declarations for the i386 architecture.
}


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

    type
      topsize = (S_NO,
        S_B,S_W,S_L,S_BW,S_BL,S_WL,
        S_IS,S_IL,S_IQ,
        S_FS,S_FL,S_FX,S_D,S_Q,S_FV,S_FXX,
        S_NEAR,S_FAR,S_SHORT
      );

{*****************************************************************************
                                Registers
*****************************************************************************}
  const
      {# Standard opcode string table (for each tasmop enumeration). The
         opcode strings should conform to the names as defined by the
         processor manufacturer.
      }
      std_op2str:op2strtable={$i i386int.inc}

      {# Standard register table (for each tregister enumeration). The
         register strings should conform to the the names as defined
         by the processor manufacturer
      }
      std_reg2str : reg2strtable = ('',
        'eax','ecx','edx','ebx','esp','ebp','esi','edi',
        'ax','cx','dx','bx','sp','bp','si','di',
        'al','cl','dl','bl','ah','ch','bh','dh',
        'cs','ds','es','ss','fs','gs',
        'st','st(0)','st(1)','st(2)','st(3)','st(4)','st(5)','st(6)','st(7)',
        'dr0','dr1','dr2','dr3','dr6','dr7',
        'cr0','cr2','cr3','cr4',
        'tr3','tr4','tr5','tr6','tr7',
        'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
        'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7'
      );

{*****************************************************************************
                                 Constants
*****************************************************************************}

      firstsaveintreg = RS_EAX;
      lastsaveintreg  = RS_EDX;
      firstsavefpureg = R_NO;
      lastsavefpureg  = R_NO;
      firstsavemmreg  = R_MM0;
      lastsavemmreg   = R_MM7;

      general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];
      general_superregisters = [RS_EAX,RS_EBX,RS_ECX,RS_EDX];

{$ifdef newra}
      usableregsint = [first_imreg..last_imreg];
{$else}
      usableregsint = [RS_EAX,RS_EBX,RS_ECX,RS_EDX];
{$endif}
      c_countusableregsint = 4;

      maxaddrregs = 1;
      addrregs    = [R_ESI];
      usableregsaddr = [RS_ESI];
      c_countusableregsaddr = 1;

      maxvarregs = 4;
      varregs : array[1..maxvarregs] of tnewregister =
         (RS_EBX,RS_EDX,RS_ECX,RS_EAX);

      maxfpuvarregs = 8;

      {# Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
      }
{$ifndef newra}
      max_scratch_regs = 1;
      scratch_regs : array[1..max_scratch_regs] of Tsuperregister = (RS_EDI);
{$endif}

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Taken from i386.c (dbx_register_map) and i386.h
          (FIXED_REGISTERS) from GCC 3.x source code

      }
      stab_regindex : array[firstreg..lastreg] of shortint =
        (-1,
        0,1,2,3,4,5,6,7,
        0,1,2,3,4,5,6,7,
        0,1,2,3,0,1,2,3,
        -1,-1,-1,-1,-1,-1,
        12,12,13,14,15,16,17,18,19,
        -1,-1,-1,-1,-1,-1,
        -1,-1,-1,-1,
        -1,-1,-1,-1,-1,
        29,30,31,32,33,34,35,36,
        21,22,23,24,25,26,27,28
      );

{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      {# Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      {# the natural int size for a processor,             }
      OS_INT = OS_32;
      {# the maximum float size for a processor,           }
      OS_FLOAT = OS_F80;
      {# the size of a vector register for a processor     }
      OS_VECTOR = OS_M64;

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_ESP;
      {# Frame pointer register }
      NR_FRAME_POINTER_REG = NR_EBP;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      NR_PIC_OFFSET_REG = NR_EBX;
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_EAX;
      RS_FUNCTION_RETURN_REG = RS_EAX;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_EAX;
      RS_FUNCTION_RETURN64_LOW_REG = RS_EAX;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_EDX;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_EDX;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;
      { The lowh part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_FUNCTION_RETURN64_LOW_REG;
      RS_FUNCTION_RESULT64_LOW_REG = RS_FUNCTION_RETURN64_LOW_REG;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_FUNCTION_RETURN64_HIGH_REG;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_FUNCTION_RETURN64_HIGH_REG;

      { WARNING: don't change to R_ST0!! See comments above implementation of }
      { a_loadfpu* methods in rgcpu (JM)                                      }
      fpu_result_reg = R_ST;
      mmresultreg = R_MM0;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

    const
      {# Registers which must be saved when calling a routine declared as
         cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
         saved should be the ones as defined in the target ABI and / or GCC.

         This value can be deduced from the CALLED_USED_REGISTERS array in the
         GCC source.
      }
      std_saved_registers = [R_ESI,R_EDI,R_EBX];
      {# Required parameter alignment when calling a routine declared as
         stdcall and cdecl. The alignment value should be the one defined
         by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;

{
  $Log$
  Revision 1.5  2003-05-31 15:05:28  peter
    * FUNCTION_RESULT64_LOW/HIGH_REG added for int64 results

  Revision 1.4  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.3  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.2  2002/04/25 16:12:09  florian
    * fixed more problems with cpubase and x86-64

  Revision 1.1  2003/04/25 11:12:09  florian
    * merged i386/cpubase and x86_64/cpubase to x86/cpubase;
      different stuff went to cpubase.inc
}

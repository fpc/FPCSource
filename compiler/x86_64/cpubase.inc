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

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      firstsaveintreg = RS_RAX;
      lastsaveintreg  = RS_R15;
      firstsavefpureg = RS_INVALID;
      lastsavefpureg  = RS_INVALID;
      firstsavemmreg  = RS_MM0;
      lastsavemmreg   = RS_MM15;

      general_registers = [RS_EAX,RS_EBX,RS_ECX,RS_EDX];
      general_superregisters = [RS_RAX,RS_RBX,RS_RCX,RS_RDX];

      usableregsint = [first_int_imreg..last_int_imreg];
      c_countusableregsint = 4;

      maxaddrregs = 0;
      usableregsaddr = [];
      c_countusableregsaddr = 0;

      maxvarregs = 4;
      varregs : array[1..maxvarregs] of tsuperregister =
         (RS_EBX,RS_EDX,RS_ECX,RS_EAX);

      maxfpuvarregs = 8;
      fpuvarregs : Array [1..maxfpuvarregs] of Tsuperregister =
                (RS_MM8,RS_MM9,RS_MM10,RS_MM11,RS_MM12,RS_MM13,RS_MM14,RS_MM15);

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Taken from i386.c (dbx_register_map) and i386.h
          (FIXED_REGISTERS) from GCC 3.x source code

      }
      stab_regindex : array[tregisterindex] of shortint = (
        {$i r8664stab.inc}
      );

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
      RS_STACK_POINTER_REG = RS_RSP;
      NR_STACK_POINTER_REG = NR_RSP;
      {# Frame pointer register }
      RS_FRAME_POINTER_REG = RS_EBP;
      NR_FRAME_POINTER_REG = NR_RBP;
      { Register for addressing absolute data in a position independant way,
        such as in PIC code. The exact meaning is ABI specific. For
        further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      NR_PIC_OFFSET_REG = NR_EBX;
      { Results are returned in this register (both 32 and 64 bits }
      NR_FUNCTION_RETURN_REG = NR_RAX;
      RS_FUNCTION_RETURN_REG = RS_RAX;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;

      { WARNING: don't change to R_ST0!! See comments above implementation of }
      { a_loadfpu* methods in rgcpu (JM)                                      }
      NR_FPU_RESULT_REG = NR_ST;
      NR_MM_RESULT_REG = NR_MM0;

      { Offset where the parent framepointer is pushed }
      PARENT_FRAMEPOINTER_OFFSET = 16;

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
      std_saved_registers = [RS_ESI,RS_EDI,RS_EBX];
      { Required parameter alignment when calling a routine declared as
        stdcall and cdecl. The alignment value should be the one defined
        by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 8;

{
  $Log$
  Revision 1.8  2003-09-25 13:13:32  florian
    * more x86-64 fixes

  Revision 1.7  2003/09/24 17:12:02  florian
    * several fixes for new reg allocator

  Revision 1.6  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.5  2003/05/31 15:05:28  peter
    * FUNCTION_RESULT64_LOW/HIGH_REG added for int64 results

  Revision 1.4  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.3  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.2  2002/04/25 16:12:09  florian
    * fixed more problems with cpubase and x86-64

  Revision 1.1  2003/04/25 11:12:09  florian
    * merged i386/cpubase and x86_64/cpubase to x86/cpubase;
      different stuff went to cpubase.inc
}

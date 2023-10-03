{
    Copyright (c) 1998-2004 by Florian Klaempfl

    Basic Processor information

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
Unit cpuinfo;

{$i fpcdefs.inc}

Interface

  uses
    globtype;

Type
   bestreal = extended;
   bestrealrec = TExtended80Rec;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = type extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       cpu_8086,
       cpu_186,
       cpu_286,
       cpu_386,
       cpu_486,
       cpu_Pentium,
       cpu_Pentium2,
       cpu_Pentium3,
       cpu_Pentium4,
       cpu_PentiumM
      );

   tfputype =
     (fpu_none,
//      fpu_soft,
      fpu_x87,
      fpu_sse,
      fpu_sse2,
      fpu_sse3,
      fpu_ssse3,
      fpu_sse41,
      fpu_sse42,
      fpu_avx,
      fpu_fma,
      fpu_avx2
     );

   tcontrollertype =
     (ct_none
     );

   tcontrollerdatatype = record
      controllertypestr, controllerunitstr: string[20];
      cputype: tcputype; fputype: tfputype;
      flashbase, flashsize, srambase, sramsize, eeprombase, eepromsize, bootbase, bootsize: dword;
   end;


Const
   { Is there support for dealing with multiple microcontrollers available }
   { for this platform? }
   ControllerSupport = false;

   { We know that there are fields after sramsize
     but we don't care about this warning }
   {$PUSH}
    {$WARN 3177 OFF}
   embedded_controllers : array [tcontrollertype] of tcontrollerdatatype =
   (
      (controllertypestr:''; controllerunitstr:''; cputype:cpu_none; fputype:fpu_none; flashbase:0; flashsize:0; srambase:0; sramsize:0));
   {$POP}

   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_register,
     pocall_safecall,
     pocall_stdcall,
     pocall_cdecl,
     pocall_cppdecl,
     pocall_pascal
   ];

   cputypestr : array[tcputype] of string[10] = ('',
     '8086',
     '80186',
     '80286',
     '80386',
     '80486',
     'PENTIUM',
     'PENTIUM2',
     'PENTIUM3',
     'PENTIUM4',
     'PENTIUMM'
   );

   fputypestr : array[tfputype] of string[6] = (
     'NONE',
//     'SOFT',
     'X87',
     'SSE',
     'SSE2',
     'SSE3',
     'SSSE3',
     'SSE41',
     'SSE42',
     'AVX',
     'FMA',
     'AVX2'
   );

   sse_singlescalar : set of tfputype = [fpu_sse..fpu_avx2];
   sse_doublescalar : set of tfputype = [fpu_sse2..fpu_avx2];

   fpu_avx_instructionsets = [fpu_avx,fpu_fma,fpu_avx2];

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_peephole,{$ifndef llvm}cs_opt_regvar,{$endif}cs_opt_stackframe,
                                  cs_opt_loopunroll,cs_opt_uncertain,
                                  cs_opt_tailrecursion,cs_opt_nodecse,cs_useebp,
				  cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches +
     [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion{,cs_opt_nodecse}];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches;
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [cs_useebp];

type
   tcpuflags =
      (CPUX86_HAS_BTX,          { Bit-test instructions (BT, BTC, BTR and BTS) are available }
       CPUX86_HAS_CMOV,         { CMOVcc instructions are available }
       CPUX86_HAS_SSEUNIT,      { SSE instructions are available }
       CPUX86_HAS_SSE2,         { SSE2 instructions are available }
       CPUX86_HAS_BSWAP         { BSWAP is available }
      );

   { Instruction optimisation hints }
   TCPUOptimizeFlags =
      (CPUX86_HINT_FAST_BT_REG_IMM,  { BT instructions with register source and immediate indices are at least as fast as logical instructions }
       CPUX86_HINT_FAST_BT_REG_REG,  { BT instructions with register source and register indices are at least as fast as equivalent logical instructions }
       CPUX86_HINT_FAST_BTX_REG_IMM, { BTC/R/S instructions with register source and immediate indices are at least as fast as logical instructions }
       CPUX86_HINT_FAST_BTX_REG_REG, { BTC/R/S instructions with register source and register indices are at least as fast as equivalent logical instructions }
       CPUX86_HINT_FAST_BT_MEM_IMM,  { BT instructions with memory sources and inmediate indices are at least as fast as logical instructions }
       CPUX86_HINT_FAST_BT_MEM_REG,  { BT instructions with memory sources and register indices and a register index are at least as fast as equivalent logical instructions }
       CPUX86_HINT_FAST_BTX_MEM_IMM, { BTC/R/S instructions with memory sources and immediate indices are at least as fast as logical instructions }
       CPUX86_HINT_FAST_BTX_MEM_REG, { BTC/R/S instructions with memory sources and register indices are at least as fast as equivalent logical instructions }
       CPUX86_HINT_FAST_XCHG,        { XCHG %reg,%reg executes in 2 cycles or less }
       CPUX86_HINT_FAST_3COMP_ADDR,  { A 3-component address (base, index and offset) has the same latency as the 2-component version (most notable with LEA instructions) }
       CPUX86_HINT_FAST_3COMP_ADDR_16{ As above, but with 16-bit addresses }
      );

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags = (
     { cpu_none      } [],
     { cpu_8086      } [],
     { cpu_186       } [],
     { cpu_286       } [],
     { cpu_386       } [CPUX86_HAS_BTX],
     { cpu_486       } [CPUX86_HAS_BSWAP,CPUX86_HAS_BTX],
     { cpu_Pentium   } [CPUX86_HAS_BSWAP,CPUX86_HAS_BTX],
     { cpu_Pentium2  } [CPUX86_HAS_BSWAP,CPUX86_HAS_BTX,CPUX86_HAS_CMOV],
     { cpu_Pentium3  } [CPUX86_HAS_BSWAP,CPUX86_HAS_BTX,CPUX86_HAS_CMOV,CPUX86_HAS_SSEUNIT],
     { cpu_Pentium4  } [CPUX86_HAS_BSWAP,CPUX86_HAS_BTX,CPUX86_HAS_CMOV,CPUX86_HAS_SSEUNIT,CPUX86_HAS_SSE2],
     { cpu_PentiumM  } [CPUX86_HAS_BSWAP,CPUX86_HAS_BTX,CPUX86_HAS_CMOV,CPUX86_HAS_SSEUNIT,CPUX86_HAS_SSE2]
   );

   cpu_optimization_hints : array[TCPUType] of set of TCPUOptimizeFlags = (
     { cpu_none      } [],
     { cpu_8086      } [CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_186       } [CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_286       } [CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_386       } [CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_486       } [CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_Pentium   } [CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_Pentium2  } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_Pentium3  } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16],
     { cpu_Pentium4  } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM],
     { cpu_PentiumM  } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR,CPUX86_HINT_FAST_3COMP_ADDR_16]
   );

   x86_near_code_models = [mm_tiny,mm_small,mm_compact];
   x86_far_code_models = [mm_medium,mm_large,mm_huge];
   x86_near_data_models = [mm_tiny,mm_small,mm_medium];
   x86_far_data_models = [mm_compact,mm_large,mm_huge];

Implementation

end.

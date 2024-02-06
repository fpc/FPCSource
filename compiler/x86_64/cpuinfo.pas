{
    Copyright (c) 1998-2000 by Florian Klaempfl

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
{$ifdef FPC_HAS_TYPE_EXTENDED}
   bestrealrec = TExtended80Rec;
{$else}
   bestrealrec = TDoubleRec;
{$endif}
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = type extended;

   pbestreal=^bestreal;

   tcputype =
      (cpu_none,
       cpu_x86_64,
       cpu_x86_64_v1,
       cpu_athlon64,
       cpu_x86_64_v2,
       cpu_core_i,
       cpu_bobcat,
       cpu_core_avx,
       cpu_jaguar,
       cpu_piledriver,
       cpu_excavator,
       cpu_core_avx2,
       cpu_x86_64_v3,
       cpu_zen,
       cpu_zen2,
       cpu_x86_64_v4,
       cpu_icelake,
       cpu_icelake_client,
       cpu_icelake_server,
       cpu_zen3,
       cpu_zen4
      );

   tfputype =
     (fpu_none,
//      fpu_soft,  { generic }
      fpu_sse64,
      fpu_x86_64_v1,
      fpu_sse3,
      fpu_ssse3,
      fpu_sse41,
      fpu_sse42,
      fpu_x86_64_v2,
      fpu_avx,
      fpu_fma,
      fpu_avx2,
      fpu_x86_64_v3,
      fpu_avx512f,
      fpu_x86_64_v4
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
   { Size of native extended type }
   extended_size = 10;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'x86_64';

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
{     pocall_compilerproc,
     pocall_inline,}
     pocall_register,
     pocall_safecall,
     pocall_stdcall,
     pocall_cdecl,
     pocall_cppdecl,
     pocall_mwpascal,
     pocall_sysv_abi_default,
     pocall_sysv_abi_cdecl,
     pocall_ms_abi_default,
     pocall_ms_abi_cdecl,
     pocall_vectorcall
   ];

   cputypestr : array[tcputype] of string[16] = ('',
     'ATHLON64',
     'X86-64',
     'X86-64-V1',
     'COREI',
     'X86-64-V2',
     'BOBCAT',
     'COREAVX',
     'JAGUAR',
     'PILEDRIVER',
     'EXCAVATOR',
     'COREAVX2',
     'X86-64-V3',
     'ZEN',
     'ZEN2',
     'X86-64-V4',
     'ICELAKE',
     'ICELAKE-CLIENT',
     'ICELAKE-SERVER',
     'ZEN3',
     'ZEN4'
   );

   fputypestr : array[tfputype] of string[9] = (
     'NONE',
//     'SOFT',
     'SSE64',
     'X86-64-V1',
     'SSE3',
     'SSSE3',
     'SSE41',
     'SSE42',
     'X86-64-V1',
     'AVX',
     'FMA',
     'AVX2',
     'X86-64-V3',
     'AVX512F',
     'X86-64-V4'
   );

   fputypestrllvm : array[tfputype] of string[9] = ('',
//     'SOFT',
     '',
     'x86-64-v1',
     'sse3',
     'ssse3',
     'sse4.1',
     'sse4.2',
     'x86-64-v2',
     'avx',
     'fma',
     'avx2',
     'x86-64-v3',
     'avx512f',
     'x86-64-v4'
   );

   sse_singlescalar = [fpu_sse64..fpu_avx512f];
   sse_doublescalar = [fpu_sse64..fpu_avx512f];

   fpu_avx_instructionsets = [fpu_avx,fpu_fma,fpu_avx2,fpu_avx512f];

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [{$ifndef llvm}cs_opt_regvar,{$endif}cs_opt_loopunroll,cs_opt_stackframe,cs_userbp,
				  cs_opt_tailrecursion,cs_opt_nodecse,cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + 
     [{$ifndef llvm}cs_opt_regvar,{$endif}cs_opt_stackframe,cs_opt_tailrecursion,cs_opt_nodecse,cs_opt_consts];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches;
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [cs_userbp];

type
   tcpuflags =
      (CPUX86_HAS_BTX,          { Bit-test instructions (BT, BTC, BTR and BTS) are available }
       CPUX86_HAS_CMOV,         { CMOVcc instructions are available }
       CPUX86_HAS_SSEUNIT,      { SSE instructions are available }
       CPUX86_HAS_SSE2,         { SSE2 instructions are available }
       CPUX86_HAS_BMI1,         { BMI1 instructions are available }
       CPUX86_HAS_BMI2,         { BMI2 instructions are available }
       CPUX86_HAS_CMPXCHG16B,   { CMPXCHG16B is available }
       CPUX86_HAS_LAHF_SAHF,    { LAHF/SAHF is available }
       CPUX86_HAS_POPCNT,       { POPCNT is available }
       CPUX86_HAS_LZCNT,        { LZCNT is available }
       CPUX86_HAS_MOVBE,        { MOVBE is available }
       CPUX86_HAS_BSWAP,        { BSWAP is available }
       CPUX86_HAS_OSXSAVE       { XGETBV is available }
      );

   tfpuflags =
      (FPUX86_HAS_SSE3,
       FPUX86_HAS_SSE4_1,
       FPUX86_HAS_SSE4_2,
       FPUX86_HAS_SSSE3,
       FPUX86_HAS_AVXUNIT,
       FPUX86_HAS_FMA,
       FPUX86_HAS_FMA4,
       FPUX86_HAS_F16C,
       FPUX86_HAS_AVX2,
       FPUX86_HAS_32MMREGS,
       FPUX86_HAS_AVX512F,
       FPUX86_HAS_AVX512BW,
       FPUX86_HAS_AVX512CD,
       FPUX86_HAS_AVX512VL,
       FPUX86_HAS_AVX512DQ
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
       CPUX86_HINT_FAST_XCHG,        { XCHG %reg,%reg executes in 2 cycles or fewer }
       CPUX86_HINT_FAST_PDEP_PEXT,   { The BMI2 instructions PDEP and PEXT execute in a single cycle }
       CPUX86_HINT_FAST_3COMP_ADDR   { A 3-component address (base, index and offset) has the same latency as the 2-component version (most notable with LEA instructions) }
      );

 const
   cpu_x86_64_v1_flags = [CPUX86_HAS_BSWAP,CPUX86_HAS_BTX,CPUX86_HAS_CMOV,CPUX86_HAS_SSEUNIT,CPUX86_HAS_SSE2];
   cpu_x86_64_v2_flags = cpu_x86_64_v1_flags+[CPUX86_HAS_CMPXCHG16B,CPUX86_HAS_LAHF_SAHF,CPUX86_HAS_POPCNT];
   cpu_x86_64_v3_flags = cpu_x86_64_v2_flags+[CPUX86_HAS_BMI1,CPUX86_HAS_BMI2,CPUX86_HAS_LZCNT,CPUX86_HAS_MOVBE,CPUX86_HAS_OSXSAVE]; { most is in the fpu flags here }
   cpu_x86_64_v4_flags = cpu_x86_64_v3_flags; { everything is in the fpu flags here }

   cpu_capabilities : array[tcputype] of set of tcpuflags = (
     { cpu_none      } [],
     { Athlon64      } cpu_x86_64_v1_flags,
     { cpu_x86_64    } cpu_x86_64_v1_flags,
     { cpu_x86_64_v1 } cpu_x86_64_v1_flags,
     { cpu_core_i    } cpu_x86_64_v1_flags+[CPUX86_HAS_POPCNT],
     { cpu_x86_64_v2 } cpu_x86_64_v2_flags,
     { cpu_bobcat    } cpu_x86_64_v1_flags+[CPUX86_HAS_POPCNT,CPUX86_HAS_LZCNT],
     { cpu_core_avx  } cpu_x86_64_v1_flags+[CPUX86_HAS_POPCNT],
     { cpu_jaguar    } cpu_x86_64_v2_flags+[CPUX86_HAS_BMI1,CPUX86_HAS_LZCNT,CPUX86_HAS_MOVBE],
     { cpu_piledriver} cpu_x86_64_v2_flags+[CPUX86_HAS_BMI1,CPUX86_HAS_LZCNT,CPUX86_HAS_MOVBE],
     { cpu_excavator } cpu_x86_64_v3_flags,
     { cpu_core_avx2 } cpu_x86_64_v3_flags,
     { cpu_x86_64_v3 } cpu_x86_64_v3_flags,
     { cpu_zen       } cpu_x86_64_v3_flags,
     { cpu_zen2      } cpu_x86_64_v3_flags,
     { cpu_x86_64_v4 } cpu_x86_64_v4_flags,
     { cpu_icelake   } cpu_x86_64_v3_flags,
     { cpu_icelake_client } cpu_x86_64_v3_flags,
     { cpu_icelake_server } cpu_x86_64_v3_flags,
     { cpu_zen3      } cpu_x86_64_v3_flags,
     { cpu_zen4      } cpu_x86_64_v4_flags
   );

   fpu_x86_64_v1_flags = [];
   fpu_x86_64_v2_flags = fpu_x86_64_v1_flags+[FPUX86_HAS_SSE3,FPUX86_HAS_SSE4_1,FPUX86_HAS_SSE4_2,FPUX86_HAS_SSSE3];
   fpu_x86_64_v3_flags = fpu_x86_64_v2_flags+[FPUX86_HAS_AVXUNIT,FPUX86_HAS_FMA,FPUX86_HAS_F16C,FPUX86_HAS_AVX2];
   fpu_x86_64_v4_flags = fpu_x86_64_v3_flags;

   fpu_capabilities : array[tfputype] of set of tfpuflags = (
      { fpu_none      }  [],
      { fpu_sse64     }  [],
      { fpu_x86_64_v1 }  fpu_x86_64_v1_flags,
      { fpu_sse3      }  fpu_x86_64_v1_flags+[FPUX86_HAS_SSE3],
      { fpu_ssse3     }  fpu_x86_64_v1_flags+[FPUX86_HAS_SSE3,FPUX86_HAS_SSSE3],
      { fpu_sse41     }  fpu_x86_64_v1_flags+[FPUX86_HAS_SSE3,FPUX86_HAS_SSE4_1],
      { fpu_sse42     }  fpu_x86_64_v1_flags+[FPUX86_HAS_SSE3,FPUX86_HAS_SSE4_1,FPUX86_HAS_SSE4_2],
      { fpu_x86_64_v2 }  fpu_x86_64_v2_flags,
      { fpu_avx       }  fpu_x86_64_v2_flags+[FPUX86_HAS_AVXUNIT],
      { fpu_fma       }  fpu_x86_64_v2_flags+[FPUX86_HAS_AVXUNIT,FPUX86_HAS_FMA],
      { fpu_avx2      }  fpu_x86_64_v2_flags+[FPUX86_HAS_AVXUNIT,FPUX86_HAS_FMA,FPUX86_HAS_AVX2],
      { fpu_x86_64_v3 }  fpu_x86_64_v3_flags,
      { fpu_avx512f   }  fpu_x86_64_v3_flags+[FPUX86_HAS_32MMREGS,FPUX86_HAS_AVX512F,FPUX86_HAS_AVX512VL,FPUX86_HAS_AVX512DQ],
      { fpu_x86_64_v4 }  fpu_x86_64_v4_flags

   );

   cpu_optimization_hints : array[TCPUType] of set of TCPUOptimizeFlags = (
      { cpu_none      } [],
      { cpu_Athlon64  } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_x86_64    } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_x86_64_v1 } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_core_i    } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_x86_64_v2 } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_bobcat    } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_core_avx  } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG], { From Sandy Bridge up to Ice Lake, complex LEA instructions are much slower }
      { cpu_jaguar    } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_piledriver} [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_excavator } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_core_avx2 } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT],
      { cpu_x86_64_v3 } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT],
      { cpu_zen       } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_zen2      } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_x86_64_v4 } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_icelake   } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_icelake_client } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_icelake_server } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_zen3      } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT,CPUX86_HINT_FAST_3COMP_ADDR],
      { cpu_zen4      } [CPUX86_HINT_FAST_BT_REG_IMM,CPUX86_HINT_FAST_BTX_REG_IMM,CPUX86_HINT_FAST_BT_MEM_IMM,CPUX86_HINT_FAST_XCHG,CPUX86_HINT_FAST_PDEP_PEXT,CPUX86_HINT_FAST_3COMP_ADDR]
   );

Implementation

end.

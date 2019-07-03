{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the m68k

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

{$i fpcdefs.inc}

Interface

  uses
    globtype;

Type
   bestreal = double;
{$if FPC_FULLVERSION>20700}
   bestrealrec = TDoubleRec;
{$endif FPC_FULLVERSION>20700}
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       cpu_MC68000,
       cpu_MC68020,
       cpu_MC68040,
       cpu_MC68060,
       cpu_isa_a,
       cpu_isa_a_p,
       cpu_isa_b,
       cpu_isa_c,
       cpu_cfv4e
      );

   tfputype =
     (fpu_none,
      fpu_soft,
      fpu_libgcc,
      fpu_68881,
      fpu_coldfire
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
     pocall_stdcall,
     pocall_safecall,
     { the difference to stdcall is only the name mangling }
     pocall_cdecl,
     { the difference to stdcall is only the name mangling }
     pocall_cppdecl,
     { this is used by PalmOS, Atari and Amiga-likes }
     pocall_syscall
   ];

   cputypestr : array[tcputype] of string[8] = ('',
     '68000',
     '68020',
     '68040',
     '68060',
     'ISAA',
     'ISAA+',
     'ISAB',
     'ISAC',
     'CFV4E'
   );

   gascputypestr : array[tcputype] of string[8] = ('',
     '68000',
     '68020',
     '68040',
     '68060',
     'isaa',
     'isaaplus',
     'isab',
     'isac',
     'cfv4e'
   );

   fputypestr : array[tfputype] of string[8] = (
     'NONE',
     'SOFT',
     'LIBGCC',
     '68881',
     'COLDFIRE'
   );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_regvar,cs_opt_stackframe,cs_opt_loopunroll,
                                  cs_opt_tailrecursion,cs_opt_nodecse,
                                  cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches +
     [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion,cs_opt_nodecse];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

type
  tcpuflags =
     (CPUM68K_HAS_DBRA,      { CPU supports the DBRA instruction                         }
      CPUM68K_HAS_CAS,       { CPU supports the CAS instruction                          }
      CPUM68K_HAS_TAS,       { CPU supports the TAS instruction                          }
      CPUM68K_HAS_BRAL,      { CPU supports the BRA.L/Bcc.L instructions                 }
      CPUM68K_HAS_ROLROR,    { CPU supports the ROL/ROR and ROXL/ROXR instructions       }
      CPUM68K_HAS_BYTEREV,   { CPU supports the BYTEREV instruction                      }
      CPUM68K_HAS_MVSMVZ,    { CPU supports the MVZ and MVS instructions                 }
      CPUM68K_HAS_MOVE16,    { CPU supports the MOVE16 instruction                       }
      CPUM68K_HAS_32BITMUL,  { CPU supports MULS/MULU 32x32 -> 32bit                     }
      CPUM68K_HAS_64BITMUL,  { CPU supports MULS/MULU 32x32 -> 64bit                     }
      CPUM68K_HAS_16BITDIV,  { CPU supports DIVS/DIVU 32/16 -> 16bit                     }
      CPUM68K_HAS_32BITDIV,  { CPU supports DIVS/DIVU 32/32 -> 32bit                     }
      CPUM68K_HAS_64BITDIV,  { CPU supports DIVS/DIVU 64/32 -> 32bit                     }
      CPUM68K_HAS_REMSREMU,  { CPU supports the REMS/REMU instructions                   }
      CPUM68K_HAS_UNALIGNED, { CPU supports unaligned access                             }
      CPUM68K_HAS_BASEDISP   { CPU supports addressing with 32bit base displacements     }
     );

const
  cpu_capabilities : array[tcputype] of set of tcpuflags =
    ( { cpu_none     } [],
      { cpu_68000    } [CPUM68K_HAS_DBRA,CPUM68K_HAS_TAS,CPUM68K_HAS_ROLROR,CPUM68K_HAS_16BITDIV],
      { cpu_68020    } [CPUM68K_HAS_DBRA,CPUM68K_HAS_CAS,CPUM68K_HAS_TAS,CPUM68K_HAS_BRAL,CPUM68K_HAS_ROLROR,CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_BASEDISP,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_64BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_64BITDIV],
      { cpu_68040    } [CPUM68K_HAS_DBRA,CPUM68K_HAS_CAS,CPUM68K_HAS_TAS,CPUM68K_HAS_BRAL,CPUM68K_HAS_ROLROR,CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_BASEDISP,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_64BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_64BITDIV,CPUM68K_HAS_MOVE16],
      { cpu_68060    } [CPUM68K_HAS_DBRA,CPUM68K_HAS_CAS,CPUM68K_HAS_TAS,CPUM68K_HAS_BRAL,CPUM68K_HAS_ROLROR,CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_BASEDISP,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_MOVE16],
      { cpu_isaa     } [CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_REMSREMU],
      { cpu_isaap    } [CPUM68K_HAS_BRAL,CPUM68K_HAS_BYTEREV,CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_REMSREMU],
      { cpu_isab     } [CPUM68K_HAS_TAS,CPUM68K_HAS_BRAL,CPUM68K_HAS_MVSMVZ,CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_REMSREMU],
      { cpu_isac     } [CPUM68K_HAS_TAS,CPUM68K_HAS_BYTEREV,CPUM68K_HAS_MVSMVZ,CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_REMSREMU],
      { cpu_cfv4e    } [CPUM68K_HAS_TAS,CPUM68K_HAS_BRAL,CPUM68K_HAS_MVSMVZ,CPUM68K_HAS_UNALIGNED,CPUM68K_HAS_32BITMUL,CPUM68K_HAS_16BITDIV,CPUM68K_HAS_32BITDIV,CPUM68K_HAS_REMSREMU]
    );

  { all CPUs commonly called "coldfire" }
  cpu_coldfire = [cpu_isa_a,cpu_isa_a_p,cpu_isa_b,cpu_isa_c,cpu_cfv4e];

  { all CPUs commonly called "68020+" }
  cpu_mc68020p = [cpu_mc68020,cpu_mc68040,cpu_mc68060];

Implementation

end.

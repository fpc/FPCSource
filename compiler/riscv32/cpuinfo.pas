{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the Risc-V32

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
   ts128real = extended;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       cpu_rv32ima,
       cpu_rv32im,
       cpu_rv32i
      );

   tfputype =
     (fpu_none,  
      fpu_libgcc,
      fpu_soft,
      fpu_fd
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
     pocall_stdcall,
     { the difference to stdcall is only the name mangling }
     pocall_cdecl,
     { the difference to stdcall is only the name mangling }
     pocall_cppdecl,
     { pass all const records by reference }
     pocall_mwpascal
   ];

   cputypestr : array[tcputype] of string[10] = ('',
     'RV32IMA',
     'RV32IM',
     'RV32I'
   );

   fputypestr : array[tfputype] of string[8] = (         
     'LIBGCC',
     'NONE',
     'SOFT',
     'FD'
   );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_regvar,cs_opt_loopunroll,cs_opt_nodecse,
                                  cs_opt_tailrecursion,cs_opt_reorder_fields,cs_opt_fastmath,
                                  cs_opt_stackframe];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + [cs_opt_regvar,cs_opt_nodecse,cs_opt_tailrecursion];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [cs_opt_stackframe]; 

 type
   tcpuflags =
      (CPURV_HAS_MUL,
       CPURV_HAS_ATOMIC,
       CPURV_HAS_COMPACT
      );

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none      } [],
       { cpu_rv32ima   } [CPURV_HAS_MUL,CPURV_HAS_ATOMIC],
       { cpu_rv32im    } [CPURV_HAS_MUL],
       { cpu_rv32i     } []     
     );

Implementation

end.

{
    Copyright (c) 2008 by the Free Pascal development team

    Basic Processor information for the AVR

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

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
   ts80real = type extended;
   ts128real = type extended;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       cpu_avr1,
       cpu_avr2,
       cpu_avr25,
       cpu_avr3,
       cpu_avr31,
       cpu_avr35,
       cpu_avr4,
       cpu_avr5,
       cpu_avr51,
       cpu_avr6
      );

   tfputype =
     (fpu_none,
      fpu_soft,
      fp_libgcc
     );

   tcontrollertype =
     (ct_none,

      ct_avrsim,

      ct_attiny24,
      ct_attiny44,
      ct_attiny84,

      ct_atmega48,
      ct_atmega88,
      ct_atmega168,
      ct_atmega368,

      ct_atmega8,
      ct_atmega16,
      ct_atmega32,
      ct_atmega64,
      ct_atmega128
     );

Const
   { Is there support for dealing with multiple microcontrollers available }
   { for this platform? }
   ControllerSupport = true;
   {# Size of native extended floating point type }
   extended_size = 12;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'avr';

   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_safecall,
     pocall_stdcall,
     { same as stdcall only different name mangling }
     pocall_cdecl,
     { same as stdcall only different name mangling }
     pocall_cppdecl,
     { same as stdcall but floating point numbers are handled like equal sized integers }
     pocall_softfloat
   ];

   cputypestr : array[tcputype] of string[5] = ('',
     'AVR1',
     'AVR2',
     'AVR25',
     'AVR3',
     'AVR31',
     'AVR35',
     'AVR4',
     'AVR5',
     'AVR51',
     'AVR6'
   );

   fputypestr : array[tfputype] of string[6] = (
     'NONE',
     'SOFT',
     'LIBGCC'
   );

    { We know that there are fields after sramsize
      but we don't care about this warning }
   {$WARN 3177 OFF}
   embedded_controllers : array [tcontrollertype] of tcontrollerdatatype =
   ((
        controllertypestr:'';
        controllerunitstr:'';
        flashbase:0;
        flashsize:0;
        srambase:0;
        sramsize:0;
        eeprombase:0;
        eepromsize:0
        ),
        (
        controllertypestr:'AVRSIM';
        controllerunitstr:'AVRSIM';
        flashbase:0;
        flashsize:$20000;
        srambase:0;
        sramsize:4096;
        eeprombase:0;
        eepromsize:4096;
        ),
        (
        controllertypestr:'ATTINY24';
        controllerunitstr:'ATTINYX4';
        flashbase:0;
        flashsize:2048;
        srambase:0;
        sramsize:128;
        eeprombase:0;
        eepromsize:128
        ),
        (
        controllertypestr:'ATTINY44';
        controllerunitstr:'ATTINYX4';
        flashbase:0;
        flashsize:4096;
        srambase:0;
        sramsize:256;
        eeprombase:0;
        eepromsize:256
        ),
        (
        controllertypestr:'ATTINY84';
        controllerunitstr:'ATTINYX4';
        flashbase:0;
        flashsize:8192;
        srambase:0;
        sramsize:512;
        eeprombase:0;
        eepromsize:512
        ),

        (
        controllertypestr:'ATMEGA48';
        controllerunitstr:'ATMEGA48FAM';
        flashbase:0;
        flashsize:4*1024;
        srambase:0;
        sramsize:512;
        eeprombase:0;
        eepromsize:256;
        ),
        (
        controllertypestr:'ATMEGA88';
        controllerunitstr:'ATMEGA48FAM';
        flashbase:0;
        flashsize:8*1024;
        srambase:0;
        sramsize:1024;
        eeprombase:0;
        eepromsize:512;
        ),
        (
        controllertypestr:'ATMEGA168';
        controllerunitstr:'ATMEGA168FAM';
        flashbase:0;
        flashsize:16*1024;
        srambase:0;
        sramsize:1024;
        eeprombase:0;
        eepromsize:512;
        ),
        (
        controllertypestr:'ATMEGA368';
        controllerunitstr:'ATMEGA168FAM';
        flashbase:0;
        flashsize:32*1024;
        srambase:0;
        sramsize:2*1024;
        eeprombase:0;
        eepromsize:1024;
        ),

        (
        controllertypestr:'ATMEGA8';
        controllerunitstr:'ATMEGA8';
        flashbase:0;
        flashsize:$2000;
        srambase:0;
        sramsize:1024;
        eeprombase:0;
        eepromsize:512
        ),
        (
        controllertypestr:'ATMEGA16';
        controllerunitstr:'ATMEGA16';
        flashbase:0;
        flashsize:$4000;
        srambase:0;
        sramsize:1024;
        eeprombase:0;
        eepromsize:512
        ),
        (
        controllertypestr:'ATMEGA32';
        controllerunitstr:'ATMEGA32';
        flashbase:0;
        flashsize:$8000;
        srambase:0;
        sramsize:1024;
        eeprombase:0;
        eepromsize:512
        ),
        (
        controllertypestr:'ATMEGA64';
        controllerunitstr:'ATMEGA64';
        flashbase:0;
        flashsize:$10000;
        srambase:0;
        sramsize:4096;
        eeprombase:0;
        eepromsize:2048;
        ),
        (
        controllertypestr:'ATMEGA128';
        controllerunitstr:'ATMEGA128';
        flashbase:0;
        flashsize:$20000;
        srambase:0;
        sramsize:4096;
        eeprombase:0;
        eepromsize:4096;
        )
   );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_regvar,cs_opt_loopunroll,cs_opt_tailrecursion,
                                  cs_opt_stackframe,cs_opt_nodecse,cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches +
     [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

 type
   tcpuflags =
      (CPUAVR_HAS_JMP_CALL,
       CPUAVR_HAS_MOVW,
       CPUAVR_HAS_LPMX,
       CPUAVR_HAS_MUL,
       CPUAVR_HAS_RAMPZ,
       CPUAVR_HAS_ELPM,
       CPUAVR_HAS_ELPMX,
       CPUAVR_2_BYTE_PC,
       CPUAVR_3_BYTE_PC
      );

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none } [],
       { cpu_avr1 } [CPUAVR_2_BYTE_PC],
       { cpu_avr2 } [CPUAVR_2_BYTE_PC],
       { cpu_avr25 } [CPUAVR_HAS_MOVW,CPUAVR_HAS_LPMX,CPUAVR_2_BYTE_PC],
       { cpu_avr3 } [CPUAVR_HAS_JMP_CALL,CPUAVR_2_BYTE_PC],
       { cpu_avr31 } [CPUAVR_HAS_JMP_CALL,CPUAVR_HAS_RAMPZ,CPUAVR_HAS_ELPM,CPUAVR_2_BYTE_PC],
       { cpu_avr35 } [CPUAVR_HAS_JMP_CALL,CPUAVR_HAS_MOVW,CPUAVR_HAS_LPMX,CPUAVR_2_BYTE_PC],
       { cpu_avr4 } [CPUAVR_HAS_MOVW,CPUAVR_HAS_LPMX,CPUAVR_HAS_MUL,CPUAVR_2_BYTE_PC],
       { cpu_avr5 } [CPUAVR_HAS_JMP_CALL,CPUAVR_HAS_MOVW,CPUAVR_HAS_LPMX,CPUAVR_HAS_MUL,CPUAVR_2_BYTE_PC],
       { cpu_avr51 } [CPUAVR_HAS_JMP_CALL,CPUAVR_HAS_MOVW,CPUAVR_HAS_LPMX,CPUAVR_HAS_MUL,CPUAVR_HAS_RAMPZ,CPUAVR_HAS_ELPM,CPUAVR_HAS_ELPMX,CPUAVR_2_BYTE_PC],
       { cpu_avr6 } [CPUAVR_HAS_JMP_CALL,CPUAVR_HAS_MOVW,CPUAVR_HAS_LPMX,CPUAVR_HAS_MUL,CPUAVR_HAS_RAMPZ,CPUAVR_HAS_ELPM,CPUAVR_HAS_ELPMX,CPUAVR_3_BYTE_PC]
     );

Implementation

end.

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

   tcpuflags =
      (AVR_HAS_JMP_CALL,
       AVR_HAS_MOVW,
       AVR_HAS_LPMX,
       AVR_HAS_MUL,
       AVR_HAS_RAMPZ,
       AVR_HAS_ELPM,
       AVR_HAS_ELPMX,
       AVR_2_BYTE_PC,
       AVR_3_BYTE_PC
      );

   tfputype =
     (fpu_none,
      fpu_soft,
      fp_libgcc
     );

   tcontrollertype =
     (ct_none,

      ct_atmega16,
      ct_atmega32,
      ct_atmega48,
      ct_atmega64,
      ct_atmega128
     );

Const
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

   embedded_controllers : array [tcontrollertype] of tcontrollerdatatype =
   ((
   	controllertypestr:'';
        controllerunitstr:'';
        interruptvectors:0;
        flashbase:0;
        flashsize:0;
        srambase:0;
        sramsize:0;
        eeprombase:0;
        eepromsize:0
   	),
        (
   	controllertypestr:'ATMEGA16';
        controllerunitstr:'ATMEGA16';
        interruptvectors:0;
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
        interruptvectors:0;
        flashbase:0;
        flashsize:$8000;
        srambase:0;
        sramsize:1024;
        eeprombase:0;
        eepromsize:512
        ),
   	(
        controllertypestr:'ATMEGA48';
        controllerunitstr:'ATMEGA48';
        interruptvectors:0;
        flashbase:0;
        flashsize:$1000;
        srambase:0;
        sramsize:512;
        eeprombase:0;
        eepromsize:256;
        ),
   	(
        controllertypestr:'ATMEGA64';
        controllerunitstr:'ATMEGA64';
        interruptvectors:0;
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
        interruptvectors:0;
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
   cpuflagsstr : array[tcpuflags] of string[20] =
      ('AVR_HAS_JMP_CALL',
       'AVR_HAS_MOVW',
       'AVR_HAS_LPMX',
       'AVR_HAS_MUL',
       'AVR_HAS_RAMPZ',
       'AVR_HAS_ELPM',
       'AVR_HAS_ELPMX',
       'AVR_2_BYTE_PC',
       'AVR_3_BYTE_PC'
      );


   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches +
     [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none } [],
       { cpu_avr1 } [],
       { cpu_avr2 } [],
       { cpu_avr25 } [],
       { cpu_avr3 } [],
       { cpu_avr31 } [],
       { cpu_avr35 } [],
       { cpu_avr4 } [],
       { cpu_avr5 } [],
       { cpu_avr51 } [],
       { cpu_avr6 } []
     );

Implementation

end.

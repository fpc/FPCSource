{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the ARM

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
       cpu_avr32a,
       cpu_avr32b
      );

Type
   tfputype =
     (fpu_none,
      fpu_libgcc,
      fpu_soft,
      fpu_avr32
     );

   tcontrollertype =
     (ct_none,

      ct_at32uc3l016,
      ct_at32uc3l032,
      ct_at32uc3l064,
      ct_at32uc3b1256
     );

Const
   {# Size of native extended floating point type }
   extended_size = 12;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'avr32';

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
     pocall_softfloat,
     { same as stdcall (requires that all const records are passed by
       reference, but that's already done for stdcall) }
     pocall_mwpascal
   ];

   cputypestr : array[tcputype] of string[8] = ('',
     'AVR32A',
     'AVR32B'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'LIBGCC',
     'SOFT',
     'AVR32'
   );

   controllertypestr : array[tcontrollertype] of string[20] =
     ('',
      'AT32UC3L016',
      'AT32UC3L032',
      'AT32UC3L064',
      'AT32UC3B1256'
     );

   controllerunitstr : array[tcontrollertype] of string[20] =
     ('',
      'AT32UC3L016',
      'AT32UC3L032',
      'AT32UC3L064',
      'AT32UC3B1256'
     );

   interruptvectors : array[tcontrollertype] of longint =
     (0,
      18,
      18,
      18,
      18
     );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_regvar,cs_opt_loopunroll,cs_opt_tailrecursion,
								  cs_opt_stackframe,cs_opt_nodecse];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches +
     [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion,cs_opt_nodecse];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];

Implementation

end.

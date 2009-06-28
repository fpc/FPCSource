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
       cpu_armv3,
       cpu_armv4,
       cpu_armv5
      );

   tfputype =
     (fpu_none,
      fpu_soft,
      fpu_libgcc,
      fpu_fpa,
      fpu_fpa10,
      fpu_fpa11,
      fpu_vfp
     );

   tcontrollertype =
     (ct_none,

      { Phillips }
      ct_lpc2114,
      ct_lpc2124,
      ct_lpc2194,

      { ATMEL }
      ct_at91sam7s256,
      ct_at91sam7se256,
      ct_at91sam7x256,
      ct_at91sam7xc256
     );

Const
   {# Size of native extended floating point type }
   extended_size = 12;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'arm';

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
     'ARMV3',
     'ARMV4',
     'ARMV5'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SOFT',
     'LIBGCC',
     'FPA',
     'FPA10',
     'FPA11',
     'VFP'
   );

   controllertypestr : array[tcontrollertype] of string[20] =
     ('',
      'LPC2114',
      'LPC2124',
      'LPC2194',
      'AT91SAM7S256',
      'AT91SAM7SE256',
      'AT91SAM7X256',
      'AT91SAM7XC256'
     );

   controllerunitstr : array[tcontrollertype] of string[20] =
     ('',
      'LPC21x4',
      'LPC21x4',
      'LPC21x4',
      'AT91SAM7x256',
      'AT91SAM7x256',
      'AT91SAM7x256',
      'AT91SAM7x256'
     );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_regvar,cs_opt_loopunroll,cs_opt_tailrecursion,cs_opt_stackframe];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];

Implementation

end.

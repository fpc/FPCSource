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
   ts80real = type double;
   ts128real = type double;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       mips32
      );

   tfputype =
     (fpu_none,
      fpu_fpu
     );

Const
   {# Size of native extended floating point type }
   extended_size = 8;
   {# Size of a multimedia register               }
   mmreg_size = 0;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'mips';

   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_stdcall,
     { same as stdcall only different name mangling }
     pocall_cdecl,
     { same as stdcall only different name mangling }
     pocall_cppdecl
   ];

   cputypestr : array[tcputype] of string[5] = ('',
     'MIPS32'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'FPU'
   );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = [cs_opt_regvar,cs_opt_loopunroll];

   level1optimizerswitches = [];
   level2optimizerswitches = level1optimizerswitches + [cs_opt_regvar,cs_opt_stackframe];
   level3optimizerswitches = level2optimizerswitches + [cs_opt_loopunroll];

Implementation

end.

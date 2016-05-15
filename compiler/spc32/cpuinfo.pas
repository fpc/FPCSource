{
    Copyright (c) 2008 by the Free Pascal development team

    Basic Processor information for the SPC32

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
       cpu_spc32v1
      );

   tfputype =
     (fpu_none,
      fpu_soft,
      fpu_libgcc
     );

   tcontrollertype =
     (ct_none,

      ct_spc32v1
     );

   tcontrollerdatatype = record
      controllertypestr, controllerunitstr: string[20];
      cputype: tcputype; fputype: tfputype;
      flashbase, flashsize, srambase, sramsize, eeprombase, eepromsize, bootbase, bootsize: dword;
   end;

Const
   {# Size of native extended floating point type }
   extended_size = 12;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'spc32';

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
	 
   ControllerSupport = true;

   cputypestr : array[tcputype] of string[7] = ('',
     'SPC32v1'
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
				cputype:cpu_none;
				fputype:fpu_none;
        flashbase:0;
        flashsize:0;
        srambase:0;
        sramsize:0;
        eeprombase:0;
        eepromsize:0;
        bootbase:0;
        bootsize:0;
   	),
        (
   	controllertypestr:'SPC32V1';
        controllerunitstr:'SPC32V1';
				cputype:cpu_spc32v1;
				fputype:fpu_soft;
        flashbase:0;
        flashsize:0;
        srambase:0;
        sramsize:$4000;
        eeprombase:0;
        eepromsize:512;
        bootbase:0;
        bootsize:0;
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
      (CPUSPC32_HAS_MUL
      );

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none }    [],
       { cpu_spc32v1 } [CPUSPC32_HAS_MUL]
     );

Implementation

end.

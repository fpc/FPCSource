{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the Generic CPU
    This file is used by PPUDump program from utils subdirectory.

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
   bestreal = extended;
{$ifdef FPC_HAS_TYPE_EXTENDED}
   bestrealrec = TExtended80Rec;
{$else}
   bestrealrec = TDoubleRec;
{$endif}
   ts32real = single;
   ts64real = double;
   ts80real = type extended;
   ts128real = type extended;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       cpu_generic
      );

   { copied from arm/cpuinfo unit for arm specific
     TSettings field }
   tinstructionset = (is_thumb,is_arm);


Type
   tfputype =
     (fpu_none,
      fpu_soft
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

   cputypestr : array[tcputype] of string[8] = ('none','generic');
   fputypestr : array[tfputype] of string[6] = ('none','soft');

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

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches;
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches;
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches;

Implementation

end.

{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the MIPS

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
       cpu_mips1,
       cpu_mips2,
       cpu_mips3,
       cpu_mips4,
       cpu_mips5,
       cpu_mips32,
       cpu_mips32r2
      );

   tfputype =(fpu_none,fpu_soft,fpu_mips2,fpu_mips3);

   tabitype = 
     (
     abi_none,
     abi_default,
     abi_o32,
     abi_n32,
     abi_o64,
     abi_n64,
     abi_eabi
     );

Const
   {# Size of native extended floating point type }
   extended_size = 8;
   {# Size of a multimedia register               }
   mmreg_size = 0;
   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_stdcall,
     { same as stdcall only different name mangling }
     pocall_cdecl,
     { same as stdcall only different name mangling }
     pocall_cppdecl
   ];

   { cpu strings as accepted by 
     GNU assembler in -arch=XXX option 
     this ilist needs to be uppercased }
   cputypestr : array[tcputype] of string[8] = ('',
     { cpu_mips1        } 'MIPS1',
     { cpu_mips2        } 'MIPS2',
     { cpu_mips3        } 'MIPS3',
     { cpu_mips4        } 'MIPS4',
     { cpu_mips5        } 'MIPS5',
     { cpu_mips32       } 'MIPS32',
     { cpu_mips32r2     } 'MIPS32R2'
   );

   fputypestr : array[tfputype] of string[9] = ('',
     'SOFT',
     'FPU_MIPS2','FPU_MIPS3'
   );

   { abi strings as accepted by 
     GNU assembler in -abi=XXX option }
   abitypestr : array[tabitype] of string[4] =
     ({ abi_none    } '',
      { abi_default } '32',
      { abi_o32     } '32',
      { abi_n32     } 'n32',
      { abi_o64     } 'o64',
      { abi_n64     } '64',
      { abi_eabi    } 'eabi'
     );

   mips_abi : tabitype = abi_default;

   { Supported optimizations, only used for information }
   supported_optimizerswitches = [cs_opt_regvar,cs_opt_loopunroll,cs_opt_nodecse,
                                  cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = [cs_opt_level1];
   level2optimizerswitches = level1optimizerswitches + [cs_opt_regvar,cs_opt_stackframe,cs_opt_nodecse];
   level3optimizerswitches = level2optimizerswitches + [cs_opt_loopunroll];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

function SetMipsABIType(const s : string) : boolean;

Implementation

uses
  cutils;

function SetMipsABIType(const s : string) : boolean;

  var
    abi : tabitype;
  begin
    SetMipsABIType:=false;
    for abi := low(tabitype) to high(tabitype) do
      if (lower(s)=abitypestr[abi]) then
        begin
          mips_abi:=abi;
          SetMipsABIType:=true;
          break;
        end;
  end;
           
end.

{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the Risc-V64

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit CPUInfo;

{$i fpcdefs.inc}

interface

uses
  globtype;

type
  bestreal = double;
  bestrealrec = TDoubleRec;
  ts32real = single;
  ts64real = double;
  ts80real = extended;
  ts128real = extended;
  ts64comp = comp;

  pbestreal = ^bestreal;

  { possible supported processors for this target }
  tcputype = (cpu_none,
    cpu_rv64imac,
    cpu_rv64ima,
    cpu_rv64im,
    cpu_rv64i,
    cpu_rv64imafdc,
    cpu_rv64imafd,
    cpu_rv64gc,
    cpu_rv64gcb
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
  supported_calling_conventions: tproccalloptions = [
    pocall_internproc,
    pocall_safecall,
    pocall_stdcall,
    { the difference to stdcall is only the name mangling }
    pocall_cdecl,
    { the difference to stdcall is only the name mangling }
    pocall_cppdecl,
    { the difference with stdcall is that all const record
      parameters are passed by reference }
    pocall_mwpascal
    ];

  cputypestr: array[tcputype] of string[10] = ('',
    'RV64IMAC',
    'RV64IMA',
    'RV64IM',
    'RV64I',
    'RV64IMAFDC',
    'RV64IMAFD',
    'RV64GC',
    'RV64GCB'
    );

  fputypestr: array[tfputype] of string[8] = (
    'NONE',
    'LIBGCC',
    'SOFT',
    'FD'
    );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [{$ifndef llvm}cs_opt_regvar,{$endif}cs_opt_loopunroll,cs_opt_nodecse,
                                  cs_opt_tailrecursion,cs_opt_reorder_fields,cs_opt_fastmath,
                                  cs_opt_stackframe];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + 
     [{$ifndef llvm}cs_opt_regvar,{$endif}cs_opt_stackframe,cs_opt_nodecse,cs_opt_tailrecursion,cs_opt_consts];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches;
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [cs_opt_stackframe];

 type
   tcpuflags =
      (CPURV_HAS_MUL,
       CPURV_HAS_ATOMIC,
       CPURV_HAS_COMPACT,
       CPURV_HAS_16REGISTERS,
       CPURV_HAS_ZBA,
       CPURV_HAS_ZBB,
       CPURV_HAS_ZBC,
       CPURV_HAS_ZBS,
       CPURV_HAS_ZBKB,
       CPURV_HAS_ZBKC,
       CPURV_HAS_ZBKX,
       CPURV_HAS_CSR_INSTRUCTIONS,   { extension Zicsr    }
       CPURV_HAS_FETCH_FENCE,        { extension Zifencei }
       CPURV_HAS_F,
       CPURV_HAS_D,
       CPURV_HAS_Q,
       CPURV_HAS_ZFH,
       CPURV_HAS_ZFHMIN,
       CPURV_HAS_ZFA,
       CPURV_HAS_ZFINX,
       CPURV_HAS_ZDINX,
       CPURV_HAS_ZHINX,
       CPURV_HAS_ZHINXMIN,
       CPURV_HAS_ZICOND,
       CPURV_HAS_ZMMUL
      );

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none       } [],
       { cpu_rv64imac   } [CPURV_HAS_MUL,CPURV_HAS_ATOMIC,CPURV_HAS_COMPACT],
       { cpu_rv64ima    } [CPURV_HAS_MUL,CPURV_HAS_ATOMIC],
       { cpu_rv64im     } [CPURV_HAS_MUL],
       { cpu_rv64i      } [],
       { cpu_rv64imafdc } [CPURV_HAS_MUL,CPURV_HAS_ATOMIC,CPURV_HAS_COMPACT,CPURV_HAS_F,CPURV_HAS_D],
       { cpu_rv64imafd  } [CPURV_HAS_MUL,CPURV_HAS_ATOMIC,CPURV_HAS_F,CPURV_HAS_D],
       { cpu_rv64gc     } [CPURV_HAS_MUL,CPURV_HAS_ATOMIC,CPURV_HAS_COMPACT,CPURV_HAS_CSR_INSTRUCTIONS,CPURV_HAS_FETCH_FENCE,CPURV_HAS_F,CPURV_HAS_D],
       { cpu_rv64gcb    } [CPURV_HAS_MUL,CPURV_HAS_ATOMIC,CPURV_HAS_COMPACT,CPURV_HAS_CSR_INSTRUCTIONS,CPURV_HAS_FETCH_FENCE,CPURV_HAS_F,CPURV_HAS_D,CPURV_HAS_ZBA,CPURV_HAS_ZBB,CPURV_HAS_ZBS]
     );

implementation

end.


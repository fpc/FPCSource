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
       cpu_mips32r2,
       cpu_pic32mx
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
     pocall_safecall,
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
     { cpu_mips32r2     } 'MIPS32R2',
     { cpu_pic32mx      } 'PIC32MX'
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

type
   tcpuflags=(
     CPUMIPS_HAS_CMOV,             { conditional move instructions (mips4+) }
     CPUMIPS_HAS_ISA32R2           { mips32r2 instructions (also on PIC32)  }
   );

const
  cpu_capabilities : array[tcputype] of set of tcpuflags =
    ( { cpu_none }     [],
      { cpu_mips1 }    [],
      { cpu_mips2 }    [],
      { cpu_mips3 }    [],
      { cpu_mips4 }    [CPUMIPS_HAS_CMOV],
      { cpu_mips5 }    [CPUMIPS_HAS_CMOV],
      { cpu_mips32 }   [CPUMIPS_HAS_CMOV],
      { cpu_mips32r2 } [CPUMIPS_HAS_CMOV,CPUMIPS_HAS_ISA32R2],
      { cpu_pic32mx }  [CPUMIPS_HAS_CMOV,CPUMIPS_HAS_ISA32R2]
    );

{$ifdef MIPSEL}
type
   tcontrollertype =
     (ct_none,
      { pic32mx }
      ct_pic32mx110f016b,
      ct_pic32mx110f016c,
      ct_pic32mx110f016d,
      ct_pic32mx120f032b,
      ct_pic32mx120f032c,
      ct_pic32mx120f032d,
      ct_pic32mx130f064b,
      ct_pic32mx130f064c,
      ct_pic32mx130f064d,
      ct_pic32mx150f128b,
      ct_pic32mx150f128c,
      ct_pic32mx150f128d,
      ct_pic32mx170f256b,
      ct_pic32mx170f256c,
      ct_pic32mx170f256d,
      ct_pic32mx210f016b,
      ct_pic32mx210f016c,
      ct_pic32mx210f016d,
      ct_pic32mx220f032b,
      ct_pic32mx220f032c,
      ct_pic32mx220f032d,
      ct_pic32mx230f064b,
      ct_pic32mx230f064c,
      ct_pic32mx230f064d,
      ct_pic32mx250f128b,
      ct_pic32mx250f128c,
      ct_pic32mx250f128d,
      ct_pic32mx270f256b,
      ct_pic32mx270f256c,
      ct_pic32mx270f256d,
      ct_pic32mx320f032h,
      ct_pic32mx320f064h,
      ct_pic32mx320f128h,
      ct_pic32mx320f128l,
      ct_pic32mx330f064h,
      ct_pic32mx330f064l,
      ct_pic32mx340f128h,
      ct_pic32mx340f128l,
      ct_pic32mx340f256h,
      ct_pic32mx340f512h,
      ct_pic32mx350f128h,
      ct_pic32mx350f128l,
      ct_pic32mx350f256h,
      ct_pic32mx350f256l,
      ct_pic32mx360f256l,
      ct_pic32mx360f512l,
      ct_pic32mx370f512h,
      ct_pic32mx370f512l,
      ct_pic32mx420f032h,
      ct_pic32mx430f064h,
      ct_pic32mx430f064l,
      ct_pic32mx440f128h,
      ct_pic32mx440f128l,
      ct_pic32mx440f256h,
      ct_pic32mx440f512h,
      ct_pic32mx450f128h,
      ct_pic32mx450f128l,
      ct_pic32mx450f256h,
      ct_pic32mx450f256l,
      ct_pic32mx460f256l,
      ct_pic32mx460f512l,
      ct_pic32mx470f512h,
      ct_pic32mx470f512l,
      ct_pic32mx534f064h,
      ct_pic32mx534f064l,
      ct_pic32mx564f064h,
      ct_pic32mx564f064l,
      ct_pic32mx564f128h,
      ct_pic32mx564f128l,
      ct_pic32mx575f256h,
      ct_pic32mx575f256l,
      ct_pic32mx575f512h,
      ct_pic32mx575f512l,
      ct_pic32mx664f064h,
      ct_pic32mx664f064l,
      ct_pic32mx664f128h,
      ct_pic32mx664f128l,
      ct_pic32mx675f256h,
      ct_pic32mx675f256l,
      ct_pic32mx675f512h,
      ct_pic32mx675f512l,
      ct_pic32mx695f512h,
      ct_pic32mx695f512l,
      ct_pic32mx764f128h,
      ct_pic32mx764f128l,
      ct_pic32mx775f256h,
      ct_pic32mx775f256l,
      ct_pic32mx775f512h,
      ct_pic32mx775f512l,
      ct_pic32mx795f512h,
      ct_pic32mx795f512l
     );

    { We know that there are fields after sramsize
      but we don't care about this warning }
   {$WARN 3177 OFF}
const
   embedded_controllers : array [tcontrollertype] of tcontrollerdatatype =
   (
      (controllertypestr:'';		controllerunitstr:'';	flashbase:0;	flashsize:0;	srambase:0;	sramsize:0),

      { PIC32MX1xx Series}
      (controllertypestr:'PIC32MX110F016B';	controllerunitstr:'PIC32MX110F016B';	flashbase:$9d000000;	flashsize:$00004000;	srambase:$A0000000;	sramsize:$00001000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX110F016C';	controllerunitstr:'PIC32MX110F016C';	flashbase:$9d000000;	flashsize:$00004000;	srambase:$A0000000;	sramsize:$00001000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX110F016D';	controllerunitstr:'PIC32MX110F016D';	flashbase:$9d000000;	flashsize:$00004000;	srambase:$A0000000;	sramsize:$00001000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX120F032B';	controllerunitstr:'PIC32MX120F032B';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX120F032C';	controllerunitstr:'PIC32MX120F032C';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX120F032D';	controllerunitstr:'PIC32MX120F032D';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX130F064B';	controllerunitstr:'PIC32MX130F064B';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX130F064C';	controllerunitstr:'PIC32MX130F064C';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX130F064D';	controllerunitstr:'PIC32MX130F064D';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX150F128B';	controllerunitstr:'PIC32MX150F128B';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX150F128C';	controllerunitstr:'PIC32MX150F128C';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX150F128D';	controllerunitstr:'PIC32MX150F128D';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX170F256B';	controllerunitstr:'PIC32MX170F256B';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX170F256C';	controllerunitstr:'PIC32MX170F256C';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX170F256D';	controllerunitstr:'PIC32MX170F256D';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),

      { PIC32MX2xx Series}
      (controllertypestr:'PIC32MX210F016B';	controllerunitstr:'PIC32MX210F016B';	flashbase:$9d000000;	flashsize:$00004000;	srambase:$A0000000;	sramsize:$00001000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX210F016C';	controllerunitstr:'PIC32MX210F016C';	flashbase:$9d000000;	flashsize:$00004000;	srambase:$A0000000;	sramsize:$00001000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX210F016D';	controllerunitstr:'PIC32MX210F016D';	flashbase:$9d000000;	flashsize:$00004000;	srambase:$A0000000;	sramsize:$00001000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX220F032B';	controllerunitstr:'PIC32MX220F032B';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX220F032C';	controllerunitstr:'PIC32MX220F032C';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX220F032D';	controllerunitstr:'PIC32MX220F032D';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX230F064B';	controllerunitstr:'PIC32MX230F064B';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX230F064C';	controllerunitstr:'PIC32MX230F064C';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX230F064D';	controllerunitstr:'PIC32MX230F064D';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX250F128B';	controllerunitstr:'PIC32MX250F128B';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX250F128C';	controllerunitstr:'PIC32MX250F128C';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX250F128D';	controllerunitstr:'PIC32MX250F128D';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX270F256B';	controllerunitstr:'PIC32MX270F256B';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX270F256C';	controllerunitstr:'PIC32MX270F256C';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),
      (controllertypestr:'PIC32MX270F256D';	controllerunitstr:'PIC32MX270F256D';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00000BEF),

      { PIC32MX3xx Series}
      (controllertypestr:'PIC32MX320F032H';	controllerunitstr:'PIC32MX320F032H';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX320F064H';	controllerunitstr:'PIC32MX320F064H';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX320F128H';	controllerunitstr:'PIC32MX320F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX320F128L';	controllerunitstr:'PIC32MX320F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX330F064H';	controllerunitstr:'PIC32MX330F064H';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX330F064L';	controllerunitstr:'PIC32MX330F064L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX340F128H';	controllerunitstr:'PIC32MX340F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX340F128L';	controllerunitstr:'PIC32MX340F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX340F256H';	controllerunitstr:'PIC32MX340F256H';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX340F512H';	controllerunitstr:'PIC32MX340F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX350F128H';	controllerunitstr:'PIC32MX350F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX350F128L';	controllerunitstr:'PIC32MX350F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX350F256H';	controllerunitstr:'PIC32MX350F256H';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX350F256L';	controllerunitstr:'PIC32MX350F256L';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX360F256L';	controllerunitstr:'PIC32MX360F256L';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX360F512L';	controllerunitstr:'PIC32MX360F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX370F512H';	controllerunitstr:'PIC32MX370F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX370F512L';	controllerunitstr:'PIC32MX370F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),

      { PIC32MX4xx Series}
      (controllertypestr:'PIC32MX420F032H';	controllerunitstr:'PIC32MX420F032H';	flashbase:$9d000000;	flashsize:$00008000;	srambase:$A0000000;	sramsize:$00002000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX430F064H';	controllerunitstr:'PIC32MX430F064H';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX430F064L';	controllerunitstr:'PIC32MX430F064L';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX440F128H';	controllerunitstr:'PIC32MX440F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX440F128L';	controllerunitstr:'PIC32MX440F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX440F256H';	controllerunitstr:'PIC32MX440F256H';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX440F512H';	controllerunitstr:'PIC32MX440F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX450F128H';	controllerunitstr:'PIC32MX450F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX450F128L';	controllerunitstr:'PIC32MX450F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX450F256H';	controllerunitstr:'PIC32MX450F256H';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX450F256L';	controllerunitstr:'PIC32MX450F256L';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX460F256L';	controllerunitstr:'PIC32MX460F256L';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX460F512L';	controllerunitstr:'PIC32MX460F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX460F512H';	controllerunitstr:'PIC32MX460F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX460F512L';	controllerunitstr:'PIC32MX460F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),

      { PIC32MX5xx Series}
      (controllertypestr:'PIC32MX534F064H';	controllerunitstr:'PIC32MX534F064H';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX534F064L';	controllerunitstr:'PIC32MX534F064L';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX564F064H';	controllerunitstr:'PIC32MX564F064H';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX564F064L';	controllerunitstr:'PIC32MX564F064L';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00004000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX564F128H';	controllerunitstr:'PIC32MX564F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX564F128L';	controllerunitstr:'PIC32MX564F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX575F256H';	controllerunitstr:'PIC32MX575F256H';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX575F256L';	controllerunitstr:'PIC32MX575F256L';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX575F512H';	controllerunitstr:'PIC32MX575F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX575F512L';	controllerunitstr:'PIC32MX575F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),

      { PIC32MX6xx Series}
      (controllertypestr:'PIC32MX664F064H';	controllerunitstr:'PIC32MX664F064H';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX664F064L';	controllerunitstr:'PIC32MX664F064L';	flashbase:$9d000000;	flashsize:$00010000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX664F128H';	controllerunitstr:'PIC32MX664F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX664F128L';	controllerunitstr:'PIC32MX664F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX675F256H';	controllerunitstr:'PIC32MX675F256H';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX675F256L';	controllerunitstr:'PIC32MX675F256L';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX675F512H';	controllerunitstr:'PIC32MX675F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX675F512L';	controllerunitstr:'PIC32MX675F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX695F512H';	controllerunitstr:'PIC32MX695F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX695F512L';	controllerunitstr:'PIC32MX695F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),

      { PIC32MX7xx Series}
      (controllertypestr:'PIC32MX764F128H';	controllerunitstr:'PIC32MX764F128H';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX764F128L';	controllerunitstr:'PIC32MX764F128L';	flashbase:$9d000000;	flashsize:$00020000;	srambase:$A0000000;	sramsize:$00008000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX775F256H';	controllerunitstr:'PIC32MX775F256H';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX775F256L';	controllerunitstr:'PIC32MX775F256L';	flashbase:$9d000000;	flashsize:$00040000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX775F512H';	controllerunitstr:'PIC32MX775F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX775F512L';	controllerunitstr:'PIC32MX775F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00010000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX795F512H';	controllerunitstr:'PIC32MX795F512H';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF),
      (controllertypestr:'PIC32MX795F512L';	controllerunitstr:'PIC32MX795F512L';	flashbase:$9d000000;	flashsize:$00080000;	srambase:$A0000000;	sramsize:$00020000;  eeprombase:0;   eepromsize:0;    bootbase:$BFC00000;     bootsize:$00002FEF)
  );

{$endif MIPSEL}

   { Supported optimizations, only used for information }
   supported_optimizerswitches = [cs_opt_regvar,cs_opt_loopunroll,cs_opt_nodecse,
                                  cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = genericlevel1optimizerswitches;
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

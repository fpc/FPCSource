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
       cpu_armv4t,
       cpu_armv5,
       cpu_armv5t,
       cpu_armv5te,
       cpu_armv5tej,
       cpu_armv6,
       cpu_armv6k,
       cpu_armv6t2,
       cpu_armv6z,
       cpu_armv7,
       cpu_armv7a,
       cpu_armv7r,
       cpu_armv7m,
       cpu_armv7em
      );

Const
   cpu_arm = [cpu_none,cpu_armv3,cpu_armv4,cpu_armv4t,cpu_armv5];
   cpu_thumb = [];
   cpu_thumb2 = [cpu_armv7m];

Type
   tfputype =
     (fpu_none,
      fpu_soft,
      fpu_libgcc,
      fpu_fpa,
      fpu_fpa10,
      fpu_fpa11,
      fpu_vfpv2,
      fpu_vfpv3,
      fpu_vfpv3_d16,
      fpu_fpv4_s16
     );

   tcontrollertype =
     (ct_none,

      { Phillips }
      ct_lpc2114,
      ct_lpc2124,
      ct_lpc2194,
      ct_lpc1754,
      ct_lpc1756,
      ct_lpc1758,
      ct_lpc1764,
      ct_lpc1766,
      ct_lpc1768,

      { ATMEL }
      ct_at91sam7s256,
      ct_at91sam7se256,
      ct_at91sam7x256,
      ct_at91sam7xc256,
		
      { STMicroelectronics }
      ct_stm32f100x4, // LD&MD value line, 4=16,6=32,8=64,b=128
      ct_stm32f100x6,
      ct_stm32f100x8,
      ct_stm32f100xB,
      ct_stm32f100xC, // HD value line, r=512,d=384,c=256
      ct_stm32f100xD,
      ct_stm32f100xE,
      ct_stm32f101x4, // LD Access line, 4=16,6=32
      ct_stm32f101x6,
      ct_stm32f101x8, // MD Access line, 8=64,B=128
      ct_stm32f101xB,
      ct_stm32f101xC, // HD Access line, C=256,D=384,E=512
      ct_stm32f101xD,
      ct_stm32f101xE,
      ct_stm32f101xF, // XL Access line, F=768,G=1M
      ct_stm32f101xG,
      ct_stm32f102x4, // LD usb access line, 4=16,6=32
      ct_stm32f102x6,
      ct_stm32f102x8, // MD usb access line, 8=64,B=128
      ct_stm32f102xB,
      ct_stm32f103x4, // LD performance line, 4=16,6=32
      ct_stm32f103x6,
      ct_stm32f103x8, // MD performance line, 8=64,B=128
      ct_stm32f103xB,
      ct_stm32f103xC, // HD performance line, C=256,D=384,E=512
      ct_stm32f103xD,
      ct_stm32f103xE,
      ct_stm32f103xF, // XL performance line, F=768,G=1M
      ct_stm32f103xG,
      ct_stm32f107x8, // MD and HD connectivity line, 8=64,B=128,C=256
      ct_stm32f107xB,
      ct_stm32f107xC,

      { TI - Fury Class - 64 K Flash, 16 K SRAM Devices }
      ct_lm3s1110,
      ct_lm3s1133,
      ct_lm3s1138,
      ct_lm3s1150,
      ct_lm3s1162,
      ct_lm3s1165,
      ct_lm3s1166,
      ct_lm3s2110,
      ct_lm3s2139,
      ct_lm3s6100,
      ct_lm3s6110,

      { TI - Fury Class - 128K Flash, 32K SRAM devices }
      ct_lm3s1601,
      ct_lm3s1608,
      ct_lm3s1620,
      ct_lm3s1635,
      ct_lm3s1636,
      ct_lm3s1637,
      ct_lm3s1651,
      ct_lm3s2601,
      ct_lm3s2608,
      ct_lm3s2620,
      ct_lm3s2637,
      ct_lm3s2651,
      ct_lm3s6610,
      ct_lm3s6611,
      ct_lm3s6618,
      ct_lm3s6633,
      ct_lm3s6637,
      ct_lm3s8630,

      { TI - Fury Class - 256K Flash, 64K SRAM devices }
      ct_lm3s1911,
      ct_lm3s1918,
      ct_lm3s1937,
      ct_lm3s1958,
      ct_lm3s1960,
      ct_lm3s1968,
      ct_lm3s1969,
      ct_lm3s2911,
      ct_lm3s2918,
      ct_lm3s2919,
      ct_lm3s2939,
      ct_lm3s2948,
      ct_lm3s2950,
      ct_lm3s2965,
      ct_lm3s6911,
      ct_lm3s6918,
      ct_lm3s6938,
      ct_lm3s6950,
      ct_lm3s6952,
      ct_lm3s6965,
      ct_lm3s8930,
      ct_lm3s8933,
      ct_lm3s8938,
      ct_lm3s8962,
      ct_lm3s8970,
      ct_lm3s8971,

      { TI - Tempest Tempest - 256 K Flash, 64 K SRAM }
      ct_lm3s5951,
      ct_lm3s5956,
      ct_lm3s1b21,
      ct_lm3s2b93,
      ct_lm3s5b91,
      ct_lm3s9b81,
      ct_lm3s9b90,
      ct_lm3s9b92,
      ct_lm3s9b95,
      ct_lm3s9b96,
      
      { SAMSUNG }
      ct_sc32442b,

      // generic Thumb2 target
      ct_thumb2bare
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
     pocall_softfloat,
     { same as stdcall (requires that all const records are passed by
       reference, but that's already done for stdcall) }
     pocall_mwpascal,
     { used for interrupt handling }
     pocall_interrupt
   ];

   cputypestr : array[tcputype] of string[8] = ('',
     'ARMV3',
     'ARMV4',
     'ARMV4T',
     'ARMV5',
     'ARMV5T',
     'ARMV5TE',
     'ARMV5TEJ',
     'ARMV6',
     'ARMV6K',
     'ARMV6T2',
     'ARMV6Z',
     'ARMV7',
     'ARMV7A',
     'ARMV7R',
     'ARMV7M',
     'ARMV7EM'
   );

   fputypestr : array[tfputype] of string[9] = ('',
     'SOFT',
     'LIBGCC',
     'FPA',
     'FPA10',
     'FPA11',
     'VFPV2',
     'VFPV3',
     'VFPV3_D16',
     'FPV4_S16'
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
        sramsize:0
   	),

        (
    	controllertypestr:'LPC2114';
        controllerunitstr:'LPC21x4';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$40000000;
        sramsize:$00004000
        ),

        (
    	controllertypestr:'LPC2124';
        controllerunitstr:'LPC21x4';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$40000000;
        sramsize:$00004000
        ),

        (
    	controllertypestr:'LPC2194';
        controllerunitstr:'LPC21x4';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$40000000;
        sramsize:$00004000
    	),

        (
    	controllertypestr:'LPC1754';
        controllerunitstr:'LPC1754';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$10000000;
        sramsize:$00004000
    	),

        (
    	controllertypestr:'LPC1756';
        controllerunitstr:'LPC1756';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$10000000;
        sramsize:$00004000
    	),

        (
    	controllertypestr:'LPC1758';
        controllerunitstr:'LPC1758';
        flashbase:$00000000;
        flashsize:$00080000;
        srambase:$10000000;
        sramsize:$00008000
    	),

        (
    	controllertypestr:'LPC1764';
        controllerunitstr:'LPC1764';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$10000000;
        sramsize:$00004000
    	),

        (
    	controllertypestr:'LPC1766';
        controllerunitstr:'LPC1766';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$10000000;
        sramsize:$00008000
    	),

        (
    	controllertypestr:'LPC1768';
        controllerunitstr:'LPC1768';
        flashbase:$00000000;
        flashsize:$00080000;
        srambase:$10000000;
        sramsize:$00008000
    	),

        (
    	controllertypestr:'AT91SAM7S256';
        controllerunitstr:'AT91SAM7x256';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$00200000;
        sramsize:$00010000
        ),

        (
    	controllertypestr:'AT91SAM7SE256';
        controllerunitstr:'AT91SAM7x256';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$00200000;
        sramsize:$00010000
        ),

        (
    	controllertypestr:'AT91SAM7X256';
        controllerunitstr:'AT91SAM7x256';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$00200000;
        sramsize:$00010000
        ),

        (
    	controllertypestr:'AT91SAM7XC256';
        controllerunitstr:'AT91SAM7x256';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$00200000;
        sramsize:$00010000
        ),

      { STM32F1 series }
      	(controllertypestr:'STM32F100X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
        (controllertypestr:'STM32F100X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00001000),
        (controllertypestr:'STM32F100X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00002000),
        (controllertypestr:'STM32F100XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00002000),
        (controllertypestr:'STM32F100XC';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$00006000),
        (controllertypestr:'STM32F100XD';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00060000; srambase:$20000000; sramsize:$00008000),
        (controllertypestr:'STM32F100XE';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00080000; srambase:$20000000; sramsize:$00008000),
        (controllertypestr:'STM32F101X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
        (controllertypestr:'STM32F101X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00001800),
        (controllertypestr:'STM32F101X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00002800),
        (controllertypestr:'STM32F101XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00004000),
        (controllertypestr:'STM32F101XC';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$00008000),
        (controllertypestr:'STM32F101XD';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00060000; srambase:$20000000; sramsize:$0000C000),
        (controllertypestr:'STM32F101XE';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00080000; srambase:$20000000; sramsize:$0000C000),
        (controllertypestr:'STM32F101XF';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$000C0000; srambase:$20000000; sramsize:$00014000),
        (controllertypestr:'STM32F101XG';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$00100000; srambase:$20000000; sramsize:$00014000),
        (controllertypestr:'STM32F102X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
        (controllertypestr:'STM32F102X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00001800),
        (controllertypestr:'STM32F102X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00002800),
        (controllertypestr:'STM32F102XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00004000),
        (controllertypestr:'STM32F103X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
        (controllertypestr:'STM32F103X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00002800),
        (controllertypestr:'STM32F103X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00005000),
        (controllertypestr:'STM32F103XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00005000),
        (controllertypestr:'STM32F103XC';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$0000C000),
        (controllertypestr:'STM32F103XD';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00060000; srambase:$20000000; sramsize:$00010000),
        (controllertypestr:'STM32F103XE';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00080000; srambase:$20000000; sramsize:$00010000),
        (controllertypestr:'STM32F103XF';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$000C0000; srambase:$20000000; sramsize:$00018000),
        (controllertypestr:'STM32F103XG';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$00100000; srambase:$20000000; sramsize:$00018000),
        (controllertypestr:'STM32F107X8';     controllerunitstr:'STM32F10X_CONN';   flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00010000),
        (controllertypestr:'STM32F107XB';     controllerunitstr:'STM32F10X_CONN';   flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00010000),
        (controllertypestr:'STM32F107XC';     controllerunitstr:'STM32F10X_CONN';   flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$00010000),

      { TI - 64 K Flash, 16 K SRAM Devices }
      	// ct_lm3s1110,
        (
    	controllertypestr:'LM3S1110';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s1133,
        (
    	controllertypestr:'LM3S1133';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s1138,
        (
    	controllertypestr:'LM3S1138';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s1150,
        (
    	controllertypestr:'LM3S1150';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s1162,
        (
    	controllertypestr:'LM3S1162';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s1165,
        (
    	controllertypestr:'LM3S1165';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s1166,
        (
    	controllertypestr:'LM3S1166';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s2110,
        (
    	controllertypestr:'LM3S2110';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s2139,
        (
    	controllertypestr:'LM3S2139';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s6100,
        (
    	controllertypestr:'LM3S6100';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),
      	// ct_lm3s6110,
        (
    	controllertypestr:'LM3S6110';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00010000;
        srambase:$20000000;
        sramsize:$00004000
        ),

        { TI - 128K Flash, 32K SRAM devices }
      	// ct_lm3s1601,
        (
    	controllertypestr:'LM3S1601';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s1608,
        (
    	controllertypestr:'LM3S1608';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s1620,
        (
    	controllertypestr:'LM3S1620';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s1635,
        (
    	controllertypestr:'LM3S1635';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s1636,
        (
    	controllertypestr:'LM3S1636';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s1637,
        (
    	controllertypestr:'LM3S1637';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s1651,
        (
    	controllertypestr:'LM3S1651';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s2601,
        (
    	controllertypestr:'LM3S2601';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s2608,
        (
    	controllertypestr:'LM3S2608';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s2620,
        (
    	controllertypestr:'LM3S2620';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s2637,
        (
    	controllertypestr:'LM3S2637';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s2651,
        (
    	controllertypestr:'LM3S2651';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s6610,
        (
    	controllertypestr:'LM3S6610';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s6611,
        (
    	controllertypestr:'LM3S6611';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s6618,
        (
    	controllertypestr:'LM3S6618';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s6633,
        (
    	controllertypestr:'LM3S6633';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s6637,
        (
    	controllertypestr:'LM3S6637';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),
      	// ct_lm3s8630,
        (
    	controllertypestr:'LM3S8630';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00020000;
        srambase:$20000000;
        sramsize:$00008000
        ),

        { TI - 256K Flash, 64K SRAM devices }
      	// ct_lm3s1911,
        (
    	controllertypestr:'LM3S1911';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s1918,
        (
    	controllertypestr:'LM3S1918';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s1937,
        (
    	controllertypestr:'LM3S1937';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s1958,
        (
    	controllertypestr:'LM3S1958';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s1960,
        (
    	controllertypestr:'LM3S1960';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s1968,
        (
    	controllertypestr:'LM3S1968';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s1969,
        (
    	controllertypestr:'LM3S1969';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s2911,
        (
    	controllertypestr:'LM3S2911';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s2918,
        (
    	controllertypestr:'LM3S2918';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s2919,
        (
    	controllertypestr:'LM3S2919';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s2939,
        (
    	controllertypestr:'LM3S2939';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s2948,
        (
    	controllertypestr:'LM3S2948';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s2950,
        (
    	controllertypestr:'LM3S2950';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s2965,
        (
    	controllertypestr:'LM3S2965';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s6911,
        (
    	controllertypestr:'LM3S6911';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s6918,
        (
    	controllertypestr:'LM3S6918';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s6938,
        (
    	controllertypestr:'LM3S6938';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s6950,
        (
    	controllertypestr:'LM3S6950';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s6952,
        (
    	controllertypestr:'LM3S6952';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s6965,
        (
    	controllertypestr:'LM3S6965';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s8930,
        (
    	controllertypestr:'LM3S8930';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s8933,
        (
    	controllertypestr:'LM3S8933';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s8938,
        (
    	controllertypestr:'LM3S8938';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s8962,
        (
    	controllertypestr:'LM3S8962';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s8970,
        (
    	controllertypestr:'LM3S8970';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
      	// ct_lm3s8971,
        (
    	controllertypestr:'LM3S8971';
        controllerunitstr:'LM3FURY';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),

        { TI - Tempest parts - 256 K Flash, 64 K SRAM }
        // ct_lm3s5951,
        (
    	controllertypestr:'LM3S5951';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s5956,
        (
    	controllertypestr:'LM3S5956';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s1b21,
        (
    	controllertypestr:'LM3S1B21';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s2b93,
        (
    	controllertypestr:'LM3S2B93';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s5b91,
        (
    	controllertypestr:'LM3S5B91';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s9b81,
        (
    	controllertypestr:'LM3S9B81';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s9b90,
        (
    	controllertypestr:'LM3S9B90';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s9b92,
        (
    	controllertypestr:'LM3S9B92';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s9b95,
        (
    	controllertypestr:'LM3S9B95';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        // ct_lm3s9b96,
        (
    	controllertypestr:'LM3S9B96';
        controllerunitstr:'LM3TEMPEST';
        flashbase:$00000000;
        flashsize:$00040000;
        srambase:$20000000;
        sramsize:$00010000
        ),
        
        //ct_SC32442b,
        (
    	controllertypestr:'SC32442B';
        controllerunitstr:'sc32442b';
        flashbase:$00000000;
        flashsize:$00000000;
        srambase:$00000000;
        sramsize:$08000000
        ),
        
        // bare bones Thumb2
        (
    	controllertypestr:'THUMB2_BARE';
        controllerunitstr:'THUMB2_BARE';
        flashbase:$00000000;
        flashsize:$00100000;
        srambase:$20000000;
        sramsize:$00100000
        )
    );

   vfp_scalar = [fpu_vfpv2,fpu_vfpv3,fpu_vfpv3_d16,fpu_fpv4_s16];

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
     [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion,cs_opt_nodecse];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [cs_opt_scheduler{,cs_opt_loopunroll}];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

 type
   tcpuflags =
      (CPUARM_HAS_BX,         { CPU supports the BX instruction                           }
       CPUARM_HAS_BLX,        { CPU supports the BLX rX instruction                       }
       CPUARM_HAS_BLX_LABEL,  { CPU supports the BLX <label> instruction                  }
       CPUARM_HAS_CLZ,        { CPU supports the CLZ instruction                          }
       CPUARM_HAS_EDSP,       { CPU supports the PLD,STRD,LDRD,MCRR and MRRC instructions }
       CPUARM_HAS_REV,        { CPU supports the REV instruction                          }
       CPUARM_HAS_LDREX,
       CPUARM_HAS_IDIV
      );

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none     } [],
       { cpu_armv3    } [],
       { cpu_armv4    } [],
       { cpu_armv4t   } [CPUARM_HAS_BX],
       { cpu_armv5    } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ],
       { cpu_armv5t   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ],
       { cpu_armv5te  } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP],
       { cpu_armv5tej } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP],
       { cpu_armv6    } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv6k   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv6t2  } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv6z   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { the identifier armv7 is should not be used, it is considered being equal to armv7a }
       { cpu_armv7    } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv7a   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv7r   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv7m   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX,CPUARM_HAS_IDIV],
       { cpu_armv7em  } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX,CPUARM_HAS_IDIV]
     );

Implementation

end.


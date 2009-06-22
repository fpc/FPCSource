unit at91sam7x256;

{$goto on}

  interface

    type
      AT91_REG = DWord;
      TBitvector32 = bitpacked array[0..31] of 0..1;

    const
      // (CKGR)
      AT91C_CKGR_DIV      = dword($000000FF); // Divider Selected
      AT91C_CKGR_MOSCEN   = dword($00000001); // Main Oscillator Enable
      AT91C_CKGR_MUL      = dword($07FF0000); // PLL Multiplier
      AT91C_CKGR_OSCOUNT  = dword($0000FF00); // Main Oscillator Start-up Time
      AT91C_CKGR_OUT_0    = dword($00000000); // Please refer to the PLL datasheet
      AT91C_CKGR_PLLCOUNT = dword($00003F00); // PLL Counter

      AT91C_MC_FMCN       = dword($00FF0000); // (MC) Flash Microsecond Cycle Number
      AT91C_MC_FWS_1FWS   = dword($00000100);

      AT91C_WDTC_WDDIS     = dword($00008000);

    type
      TAT91C_Low_Lewel_Settings = record
        osc_div_factor:byte;
        osc_mul_factor:word;
      end;

    var
      AIC_SMR : array[0..31] of AT91_REG absolute $FFFFF000; // Source Mode Register
      AIC_SVR : array[0..31] of AT91_REG absolute $FFFFF020; // Source Vector Register
      AIC_IVR : AT91_REG absolute $FFFFF040; // IRQ Vector Register
      AIC_FVR : AT91_REG absolute $FFFFF044; // FIQ Vector Register
      AIC_ISR : AT91_REG absolute $FFFFF048; // Interrupt Status Register
      AIC_IPR : AT91_REG absolute $FFFFF04C; // Interrupt Pending Register
      AIC_IMR : AT91_REG absolute $FFFFF050; // Interrupt Mask Register
      AIC_CISR : AT91_REG absolute $FFFFF054; // Core Interrupt Status Register }
      // Reserved0 : array[0..1] of AT91_REG;
      AIC_IECR : AT91_REG absolute $FFFFF060; // Interrupt Enable Command Register
      AIC_IDCR : AT91_REG absolute $FFFFF064; // Interrupt Disable Command Register
      AIC_ICCR : AT91_REG absolute $FFFFF068; // Interrupt Clear Command Register
      AIC_ISCR : AT91_REG absolute $FFFFF06C; // Interrupt Set Command Register
      AIC_EOICR : AT91_REG absolute $FFFFF070; // End of Interrupt Command Register
      AIC_SPU : AT91_REG absolute $FFFFF074; // Spurious Vector Register
      AIC_DCR : AT91_REG absolute $FFFFF078; // Debug Control Register (Protect) }
      // Reserved1 : array[0..0] of AT91_REG;

    // ========== Register definition for PIOA peripheral ==========
      AT91C_PIOA_IMR     : DWord absolute $FFFFF448; // Interrupt Mask Register
      AT91C_PIOA_IER     : DWord absolute $FFFFF440; // Interrupt Enable Register
      AT91C_PIOA_OWDR    : DWord absolute $FFFFF4A4; // Output Write Disable Register
      AT91C_PIOA_ISR     : DWord absolute $FFFFF44C; // Interrupt Status Register
      AT91C_PIOA_PPUDR   : DWord absolute $FFFFF460; // Pull-up Disable Register
      AT91C_PIOA_MDSR    : DWord absolute $FFFFF458; // Multi-driver Status Register
      AT91C_PIOA_MDER    : DWord absolute $FFFFF450; // Multi-driver Enable Register
      AT91C_PIOA_PER     : DWord absolute $FFFFF400; // PIO Enable Register
      AT91C_PIOA_PSR     : DWord absolute $FFFFF408; // PIO Status Register
      AT91C_PIOA_OER     : DWord absolute $FFFFF410; // Output Enable Register
      AT91C_PIOA_BSR     : DWord absolute $FFFFF474; // Select B Register
      AT91C_PIOA_PPUER   : DWord absolute $FFFFF464; // Pull-up Enable Register
      AT91C_PIOA_MDDR    : DWord absolute $FFFFF454; // Multi-driver Disable Register
      AT91C_PIOA_PDR     : DWord absolute $FFFFF404; // PIO Disable Register
      AT91C_PIOA_ODR     : DWord absolute $FFFFF414; // Output Disable Registerr
      AT91C_PIOA_IFDR    : DWord absolute $FFFFF424; // Input Filter Disable Register
      AT91C_PIOA_ABSR    : DWord absolute $FFFFF478; // AB Select Status Register
      AT91C_PIOA_ASR     : DWord absolute $FFFFF470; // Select A Register
      AT91C_PIOA_PPUSR   : DWord absolute $FFFFF468; // Pull-up Status Register
      AT91C_PIOA_ODSR    : DWord absolute $FFFFF438; // Output Data Status Register
      AT91C_PIOA_SODR    : DWord absolute $FFFFF430; // Set Output Data Register
      AT91C_PIOA_IFSR    : DWord absolute $FFFFF428; // Input Filter Status Register
      AT91C_PIOA_IFER    : DWord absolute $FFFFF420; // Input Filter Enable Register
      AT91C_PIOA_OSR     : DWord absolute $FFFFF418; // Output Status Register
      AT91C_PIOA_IDR     : DWord absolute $FFFFF444; // Interrupt Disable Register
      AT91C_PIOA_PDSR    : DWord absolute $FFFFF43C; // Pin Data Status Register
      AT91C_PIOA_CODR    : DWord absolute $FFFFF434; // Clear Output Data Register
      AT91C_PIOA_OWSR    : DWord absolute $FFFFF4A8; // Output Write Status Register
      AT91C_PIOA_OWER    : DWord absolute $FFFFF4A0; // Output Write Enable Register
    // ========== Register definition for CKGR peripheral ==========
      AT91C_CKGR_PLLR    : DWord absolute $FFFFFC2C; // PLL Register
      AT91C_CKGR_MCFR    : DWord absolute $FFFFFC24; // Main Clock  Frequency Register
      AT91C_CKGR_MOR     : DWord absolute $FFFFFC20; // Main Oscillator Register
    // ========== Register definition for PMC peripheral ==========
      AT91C_PMC_SCSR     : DWord absolute $FFFFFC08; // System Clock Status Register
      AT91C_PMC_SCER     : DWord absolute $FFFFFC00; // System Clock Enable Register
      AT91C_PMC_IMR      : DWord absolute $FFFFFC6C; // Interrupt Mask Register
      AT91C_PMC_IDR      : DWord absolute $FFFFFC64; // Interrupt Disable Register
      AT91C_PMC_PCDR     : DWord absolute $FFFFFC14; // Peripheral Clock Disable Register
      AT91C_PMC_SCDR     : DWord absolute $FFFFFC04; // System Clock Disable Register
      AT91C_PMC_SR       : DWord absolute $FFFFFC68; // Status Register
      AT91C_PMC_IER      : DWord absolute $FFFFFC60; // Interrupt Enable Register
      AT91C_PMC_MCKR     : DWord absolute $FFFFFC30; // Master Clock Register
      AT91C_PMC_MOR      : DWord absolute $FFFFFC20; // Main Oscillator Register
      AT91C_PMC_PCER     : DWord absolute $FFFFFC10; // Peripheral Clock Enable Register
      AT91C_PMC_PCSR     : DWord absolute $FFFFFC18; // Peripheral Clock Status Register
      AT91C_PMC_PLLR     : DWord absolute $FFFFFC2C; // PLL Register
      AT91C_PMC_MCFR     : DWord absolute $FFFFFC24; // Main Clock  Frequency Register
      AT91C_PMC_PCKR     : DWord absolute $FFFFFC40; // Programmable Clock Register

    const
      AT91C_PMC_CSS_PLL_CLK = dword($3); // (PMC) Clock from PLL is selected
      AT91C_PMC_PRES_CLK_2  = dword($1) shl 2; // (PMC) Selected clock divided by 2
      AT91C_PMC_MOSCS       = dword($1) shl 0; // (PMC) MOSC Status/Enable/Disable/Mask
      AT91C_PMC_LOCK        = dword($1) shl 2; // (PMC) PLL Status/Enable/Disable/Mask
      AT91C_PMC_MCKRDY      = dword($1) shl 3; // (PMC) MCK_RDY Status/Enable/Disable/Mask

    var
    // ========== Register definition for RSTC peripheral ==========
      AT91C_RSTC_RSR     : DWord absolute $FFFFFD04; // Reset Status Register
      AT91C_RSTC_RMR     : DWord absolute $FFFFFD08; // Reset Mode Register
      AT91C_RSTC_RCR     : DWord absolute $FFFFFD00; // Reset Control Register

    // ========== Register definition for WDTC peripheral ==========
      AT91C_WDTC_WDMR    : DWord absolute $FFFFFD44; // Watchdog Mode Register
      AT91C_WDTC_WDSR    : DWord absolute $FFFFFD48; // Watchdog Status Register
      AT91C_WDTC_WDCR    : DWord absolute $FFFFFD40; // Watchdog Control Register
    // ========== Register definition for VREG peripheral ==========
      AT91C_VREG_MR      : DWord absolute $FFFFFD60; // Voltage Regulator Mode Register
    // ========== Register definition for MC peripheral ==========
      AT91C_MC_FCR       : DWord absolute $FFFFFF64; // MC Flash Command Register
      AT91C_MC_ASR       : DWord absolute $FFFFFF04; // MC Abort Status Register
      AT91C_MC_FSR       : DWord absolute $FFFFFF68; // MC Flash Status Register
      AT91C_MC_FMR       : DWord absolute $FFFFFF60; // MC Flash Mode Register
      AT91C_MC_AASR      : DWord absolute $FFFFFF08; // MC Abort Address Status Register
      AT91C_MC_RCR       : DWord absolute $FFFFFF00; // MC Remap Control Register


    // *****************************************************************************
    //               BASE ADDRESS DEFINITIONS FOR AT91SAM7X256
    // *****************************************************************************
      AT91C_BASE_SYS  : DWord absolute $FFFFF000; // (SYS) Base Address
      AT91C_BASE_AIC  : DWord absolute $FFFFF000; // (AIC) Base Address
      AT91C_BASE_PIOA : DWord absolute $FFFFF400; // (PIOA) Base Address
      AT91C_BASE_CKGR : DWord absolute $FFFFFC20; // (CKGR) Base Address
      AT91C_BASE_PMC  : DWord absolute $FFFFFC00; // (PMC) Base Address
      AT91C_BASE_WDTC : DWord absolute $FFFFFD40; // (WDTC) Base Address
      AT91C_BASE_VREG : DWord absolute $FFFFFD60; // (VREG) Base Address
      AT91C_BASE_MC   : DWord absolute $FFFFFF00; // (MC) Base Address

    procedure lowlevelinit(LowLewelValues:TAT91C_Low_Lewel_Settings);

    var
      Undefined_Handler,
      SWI_Handler,
      Prefetch_Handler,
      Abort_Handler,
      IRQ_Handler,
      FIQ_Handler : pointer;

  implementation


    procedure AT91F_Default_FIQ_handler; assembler; nostackframe; public name 'AT91F_Default_FIQ_handler';
      asm
      .Lloop:
        b .Lloop
      end;


    procedure AT91F_Default_IRQ_handler; assembler; nostackframe; public name 'AT91F_Default_IRQ_handler';
      asm
      .Lloop:
        b .Lloop
      end;


    procedure AT91F_Spurious_handler; assembler; nostackframe; public name 'AT91F_Spurious_handler';
      asm
      .Lloop:
        b .Lloop
      end;


    { Basic hardware initialization

      Note: see page 5 - 6 of Atmel's
      "Getting Started with AT91SAM7X Microcontrollers" for details.}
    procedure lowlevelinit(LowLewelValues:TAT91C_Low_Lewel_Settings);
      var
        i : Longint;
      begin
        {    Set Flash Wait state  (AT91C_MC_FMR = MC Flash Mode Register)}
        AT91C_MC_FMR := ((AT91C_MC_FMCN) and (50*$10000)) or AT91C_MC_FWS_1FWS;

        { Watchdog Disable  (AT91C_WDTC_WDMR = Watchdog Mode Register)}
        AT91C_WDTC_WDMR := AT91C_WDTC_WDDIS;

        {Enable the Main Oscillator (AT91C_PMC_MOR = Main Oscillator Register)}
        AT91C_PMC_MOR := (( AT91C_CKGR_OSCOUNT and ($0800) or AT91C_CKGR_MOSCEN ));

        { Wait the startup time (until PMC Status register MOSCEN bit is set)
          result: $FFFFFC68 bit 0 will set when main oscillator has stabilized}
      	while (AT91C_PMC_SR and AT91C_PMC_MOSCS)=0 do
           ;


        { PMC Clock Generator PLL Register setup }
        AT91C_PMC_PLLR :=((AT91C_CKGR_OUT_0) or
                         (AT91C_CKGR_DIV and LowLewelValues.osc_div_factor) or
                         (AT91C_CKGR_PLLCOUNT and (40 shl 10)) or
                         (AT91C_CKGR_MUL and (LowLewelValues.osc_mul_factor shl 16)));

        { Wait the startup time (until PMC Status register LOCK bit is set)
          result: 0xFFFFFC68 bit 2 will set when PLL has locked  }
        while (AT91C_PMC_SR and AT91C_PMC_LOCK)=0 do
           ;


        { PMC Master Clock Register setup (AT91C_PMC_MCKR = Master Clock Register)}
        AT91C_PMC_MCKR := AT91C_PMC_PRES_CLK_2;

        { Wait the startup time (until PMC Status register MCKRDY bit is set)
          result: $FFFFFC68 bit 3 will set when Master Clock has stabilized }

        while (AT91C_PMC_SR and AT91C_PMC_MCKRDY)=0 do
           ;

        {(AT91C_PMC_MCKR = Master Clock Register) }
        AT91C_PMC_MCKR := AT91C_PMC_MCKR or AT91C_PMC_CSS_PLL_CLK;

        { Wait the startup time (until PMC Status register MCKRDY bit is set)
          result: $FFFFFC68 bit 3 will set when Master Clock has stabilized }
        while (AT91C_PMC_SR and AT91C_PMC_MCKRDY)=0 do
          ;

        { Set up the default interrupts handler vectors }
        AIC_SVR[0]:=AT91_REG(@AT91F_Default_FIQ_handler);

        for i:=1 to 30 do
          AIC_SVR[i]:=AT91_REG(@AT91F_Default_IRQ_handler);

        AIC_SPU:=AT91_REG(@AT91F_Spurious_handler);
      end;


    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';
      asm
      .Lhalt:
        b .Lhalt
      end;

    var
      _data: record end; external name '_data';
      _edata: record end; external name '_edata';
      _etext: record end; external name '_etext';
      _bss_start: record end; external name '_bss_start';
      _bss_end: record end; external name '_bss_end';
      _stack_top: record end; external name '_stack_top';

    procedure _FPC_start; assembler; nostackframe;
      label
        _start;
      asm
        .init
        .align 16
        .globl _start
        b   _start
        b   .LUndefined_Addr  // Undefined Instruction vector
        b   .LSWI_Addr        // Software Interrupt vector
        b   .LPrefetch_Addr   // Prefetch abort vector
        b   .LAbort_Addr      // Data abort vector
        nop                   // reserved
        b   .LIRQ_Addr        // Interrupt Request (IRQ) vector
        b   .LFIQ_Addr        // Fast interrupt request (FIQ) vector

    .LUndefined_Addr:
        ldr r0,.L1
        ldr pc,[r0]
    .LSWI_Addr:
        ldr r0,.L2
        ldr pc,[r0]
    .LPrefetch_Addr:
        ldr r0,.L3
        ldr pc,[r0]
    .LAbort_Addr:
        ldr r0,.L4
        ldr pc,[r0]
    .LIRQ_Addr:
        ldr r0,.L5
        ldr pc,[r0]
    .LFIQ_Addr:
        ldr r0,.L5
        ldr pc,[r0]

    .L1:
        .long     Undefined_Handler
    .L2:
        .long     SWI_Handler
    .L3:
        .long     Prefetch_Handler
    .L4:
        .long     Abort_Handler
    .L5:
        .long     IRQ_Handler
    .L6:
        .long     FIQ_Handler

    _start:
        (*
          Set absolute stack top

          stack is already set by bootloader
          but if this point is entered by any
          other means than reset, the stack pointer
          needs to be set explicity
        *)
        ldr r0,.L_stack_top

        (*
          Setting up SP for IRQ and FIQ mode.
          Change mode before setting each one
          move back again to Supervisor mode
          Each interrupt has its own link
          register, stack pointer and program
          counter The stack pointers must be
          initialized for interrupts to be
          used later.
        *)

        (*
          setup irq and fiq stacks each 128 bytes
        *)
        msr cpsr_c, #0x12  // switch to irq mode
        mov sp, r0         // set irq stack pointer
        sub r0,r0,#128     // irq stack size
        msr cpsr_c, #0x11  // fiq mode
        mov sp, r0         // set fiq stack pointer
        sub r0,r0,#128     // fiq stack size
        msr cpsr_c, #0x13  // supervisor mode F,I enabled
        mov sp, r0         // stack

        // for now, all handlers are set to a default one
        ldr r1,.LDefaultHandlerAddr
        ldr r0,.L1
        str r1,[r0]
        ldr r0,.L2
        str r1,[r0]
        ldr r0,.L3
        str r1,[r0]
        ldr r0,.L4
        str r1,[r0]
        ldr r0,.L5
        str r1,[r0]
        ldr r0,.L6
        str r1,[r0]

        // copy initialized data from flash to ram
        ldr r1,.L_etext
        ldr r2,.L_data
        ldr r3,.L_edata
.Lcopyloop:
        cmp r2,r3
        ldrls r0,[r1],#4
        strls r0,[r2],#4
        bls .Lcopyloop

        // clear onboard ram
        ldr r1,.L_bss_start
        ldr r2,.L_bss_end
        mov r0,#0
.Lzeroloop:
        cmp r1,r2
        strls r0,[r1],#4
        bls .Lzeroloop

        bl PASCALMAIN
        bl _FPC_haltproc
.L_bss_start:
        .long _bss_start
.L_bss_end:
        .long _bss_end
.L_etext:
        .long _etext
.L_data:
        .long _data
.L_edata:
        .long _edata
.L_stack_top:
        .long _stack_top
.LDefaultHandlerAddr:
        .long .LDefaultHandler
        // default irq handler just returns
.LDefaultHandler:
        mov pc,r14
        .text
      end;

end.

end.


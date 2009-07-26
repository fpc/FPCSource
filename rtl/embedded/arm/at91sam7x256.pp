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
      AT91C_AIC_SMR : array[0..31] of AT91_REG absolute $FFFFF000; // Source Mode Register
      AT91C_AIC_SVR : array[0..31] of AT91_REG absolute $FFFFF020; // Source Vector Register
      AT91C_AIC_IVR : AT91_REG absolute $FFFFF040; // IRQ Vector Register
      AT91C_AIC_FVR : AT91_REG absolute $FFFFF044; // FIQ Vector Register
      AT91C_AIC_ISR : AT91_REG absolute $FFFFF048; // Interrupt Status Register
      AT91C_AIC_IPR : AT91_REG absolute $FFFFF04C; // Interrupt Pending Register
      AT91C_AIC_IMR : AT91_REG absolute $FFFFF050; // Interrupt Mask Register
      AT91C_AIC_CISR : AT91_REG absolute $FFFFF054; // Core Interrupt Status Register }
      // Reserved0 : array[0..1] of AT91_REG;
      AT91C_AIC_IECR : AT91_REG absolute $FFFFF060; // Interrupt Enable Command Register
      AT91C_AIC_IDCR : AT91_REG absolute $FFFFF064; // Interrupt Disable Command Register
      AT91C_AIC_ICCR : AT91_REG absolute $FFFFF068; // Interrupt Clear Command Register
      AT91C_AIC_ISCR : AT91_REG absolute $FFFFF06C; // Interrupt Set Command Register
      AT91C_AIC_EOICR : AT91_REG absolute $FFFFF070; // End of Interrupt Command Register
      AT91C_AIC_SPU : AT91_REG absolute $FFFFF074; // Spurious Vector Register
      AT91C_AIC_DCR : AT91_REG absolute $FFFFF078; // Debug Control Register (Protect) }
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
    // ========== Register definition for PIOB peripheral ==========
      AT91C_PIOB_PER     : DWord absolute $FFFFF600; //  PIO Enable Register
      AT91C_PIOB_PDR     : DWord absolute $FFFFF604; //  PIO Disable Register
      AT91C_PIOB_PSR     : DWord absolute $FFFFF608; //  PIO Status Register
      AT91C_PIOB_OER     : DWord absolute $FFFFF610; //  Output Enable Register
      AT91C_PIOB_ODR     : DWord absolute $FFFFF614; //  Output Disable Registerr
      AT91C_PIOB_OSR     : DWord absolute $FFFFF618; //  Output Status Register
      AT91C_PIOB_IFER    : DWord absolute $FFFFF620; //  Input Filter Enable Register
      AT91C_PIOB_IFDR    : DWord absolute $FFFFF624; //  Input Filter Disable Register
      AT91C_PIOB_IFSR    : DWord absolute $FFFFF628; //  Input Filter Status Register
      AT91C_PIOB_SODR    : DWord absolute $FFFFF630; //  Set Output Data Register
      AT91C_PIOB_CODR    : DWord absolute $FFFFF634; //  Clear Output Data Register
      AT91C_PIOB_ODSR    : DWord absolute $FFFFF638; //  Output Data Status Register
      AT91C_PIOB_PDSR    : DWord absolute $FFFFF63C; //  Pin Data Status Register
      AT91C_PIOB_IER     : DWord absolute $FFFFF640; //  Interrupt Enable Register
      AT91C_PIOB_IDR     : DWord absolute $FFFFF644; //  Interrupt Disable Register
      AT91C_PIOB_IMR     : DWord absolute $FFFFF648; //  Interrupt Mask Register
      AT91C_PIOB_ISR     : DWord absolute $FFFFF64C; //  Interrupt Status Register
      AT91C_PIOB_MDER    : DWord absolute $FFFFF650; //  Multi-driver Enable Register
      AT91C_PIOB_MDDR    : DWord absolute $FFFFF654; //  Multi-driver Disable Register
      AT91C_PIOB_MDSR    : DWord absolute $FFFFF658; //  Multi-driver Status Register
      AT91C_PIOB_PPUDR   : DWord absolute $FFFFF660; //  Pull-up Disable Register
      AT91C_PIOB_PPUER   : DWord absolute $FFFFF664; //  Pull-up Enable Register
      AT91C_PIOB_PPUSR   : DWord absolute $FFFFF668; //  Pull-up Status Register
      AT91C_PIOB_ASR     : DWord absolute $FFFFF670; //  Select A Register
      AT91C_PIOB_BSR     : DWord absolute $FFFFF674; //  Select B Register
      AT91C_PIOB_ABSR    : DWord absolute $FFFFF678; //  AB Select Status Register
      AT91C_PIOB_OWER    : DWord absolute $FFFFF6A0; //  Output Write Enable Register
      AT91C_PIOB_OWDR    : DWord absolute $FFFFF6A4; //  Output Write Disable Register
      AT91C_PIOB_OWSR    : DWord absolute $FFFFF6A8; //  Output Write Status Register

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

    // ========== Register definition for RTTC peripheral ==========
      AT91C_RTTC_RTMR    : DWord absolute $FFFFFD20; // Real-time Mode Register
      AT91C_RTTC_RTAR    : DWord absolute $FFFFFD24; // Real-time Alarm Register
      AT91C_RTTC_RTVR    : DWord absolute $FFFFFD28; // Real-time Value Register
      AT91C_RTTC_RTSR    : DWord absolute $FFFFFD2C; // Real-time Status Register

    // ========== Register definition for PITC peripheral ==========
      AT91C_PITC_PIMR    : DWord absolute $FFFFFD30; // Period Interval Mode Register
      AT91C_PITC_PISR    : DWord absolute $FFFFFD34; // Period Interval Status Register
      AT91C_PITC_PIVR    : DWord absolute $FFFFFD38; // Period Interval Value Register
      AT91C_PITC_PIIR    : DWord absolute $FFFFFD3C; // Period Interval Image Register

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

    // ========== Peripheral mapping ==========

    // ========== Register definition for TC0 peripheral ==========
      AT91C_TC0_CCR   : DWord absolute $FFFA0000; // Channel Control Register
      AT91C_TC0_CMR   : DWord absolute $FFFA0004; // Channel Mode Register (Capture Mode / Waveform Mode)
      AT91C_TC0_CV    : DWord absolute $FFFA0010; // Counter Value
      AT91C_TC0_RA    : DWord absolute $FFFA0014; // Register A
      AT91C_TC0_RB    : DWord absolute $FFFA0018; // Register B
      AT91C_TC0_RC    : DWord absolute $FFFA001C; // Register C
      AT91C_TC0_SR    : DWord absolute $FFFA0020; // Status Register
      AT91C_TC0_IMR   : DWord absolute $FFFA002C; // Interrupt Mask Register
      AT91C_TC0_IER   : DWord absolute $FFFA0024; // Interrupt Enable Register
      AT91C_TC0_IDR   : DWord absolute $FFFA0028; // Interrupt Disable Register
    // ========== Register definition for TC1 peripheral ==========
      AT91C_TC1_CCR   : DWord absolute $FFFA0040; // Channel Control Register
      AT91C_TC1_CMR   : DWord absolute $FFFA0044; // Channel Mode Register (Capture Mode / Waveform Mode)
      AT91C_TC1_CV    : DWord absolute $FFFA0050; // Counter Value
      AT91C_TC1_RA    : DWord absolute $FFFA0054; // Register A
      AT91C_TC1_RB    : DWord absolute $FFFA0058; // Register B
      AT91C_TC1_RC    : DWord absolute $FFFA005C; // Register C
      AT91C_TC1_SR    : DWord absolute $FFFA0060; // Status Register
      AT91C_TC1_IER   : DWord absolute $FFFA0064; // Interrupt Enable Register
      AT91C_TC1_IDR   : DWord absolute $FFFA0068; // Interrupt Disable Register
      AT91C_TC1_IMR   : DWord absolute $FFFA006C; // Interrupt Mask Register
    // ========== Register definition for TC2 peripheral ==========
      AT91C_TC2_CCR   : DWord absolute $FFFA0080; // Channel Control Register
      AT91C_TC2_CMR   : DWord absolute $FFFA0084; // Channel Mode Register (Capture Mode / Waveform Mode;
      AT91C_TC2_CV    : DWord absolute $FFFA0090; // Counter Value
      AT91C_TC2_RA    : DWord absolute $FFFA0094; // Register A
      AT91C_TC2_RB    : DWord absolute $FFFA0098; // Register B
      AT91C_TC2_RC    : DWord absolute $FFFA009C; // Register C
      AT91C_TC2_SR    : DWord absolute $FFFA00A0; // Status Register
      AT91C_TC2_IER   : DWord absolute $FFFA00A4; // Interrupt Enable Register
      AT91C_TC2_IDR   : DWord absolute $FFFA00A8; // Interrupt Disable Register
      AT91C_TC2_IMR   : DWord absolute $FFFA00AC; // Interrupt Mask Register
    // ========== Register definition for TCB peripheral ==========
      AT91C_TCB_BCR   : DWord absolute $FFFA00C0; // TC Block Control Register
      AT91C_TCB_BMR   : DWord absolute $FFFA00C4; // TC Block Mode Register
    // ========== Register definition for UDP peripheral ==========
      AT91C_UDP_NUM   : DWord absolute $FFFB0000; // Frame Number Register
      AT91C_UDP_GLBSTATE : DWord absolute $FFFB0004; // Global State Register
      AT91C_UDP_FADDR : DWord absolute $FFFB0008; // Function Address Register
      AT91C_UDP_IER   : DWord absolute $FFFB0010; // Interrupt Enable Register
      AT91C_UDP_IDR   : DWord absolute $FFFB0014; // Interrupt Disable Register
      AT91C_UDP_IMR   : DWord absolute $FFFB0018; // Interrupt Mask Register
      AT91C_UDP_ISR   : DWord absolute $FFFB001C; // Interrupt Status Register
      AT91C_UDP_ICR   : DWord absolute $FFFB0020; // Interrupt Clear Register
      AT91C_UDP_RSTEP : DWord absolute $FFFB0028; // Reset Endpoint Register
      AT91C_UDP_CSR   : DWord absolute $FFFB0030; // Endpoint Control and Status Register
      AT91C_UDP_FDR   : DWord absolute $FFFB0050; // Endpoint FIFO Data Register
      AT91C_UDP_TXVC  : DWord absolute $FFFB0074; // Transceiver Control Register
    // ========== Register definition for TWI peripheral ==========
      AT91C_TWI_CR    : DWord absolute $FFFB8000; // Control Register
      AT91C_TWI_MMR   : DWord absolute $FFFB8004; // Master Mode Register
      AT91C_TWI_IADR  : DWord absolute $FFFB800C; // Internal Address Register
      AT91C_TWI_CWGR  : DWord absolute $FFFB8010; // Clock Waveform Generator Register
      AT91C_TWI_SR    : DWord absolute $FFFB8020; // Status Register
      AT91C_TWI_IER   : DWord absolute $FFFB8024; // Interrupt Enable Register
      AT91C_TWI_IDR   : DWord absolute $FFFB8028; // Interrupt Disable Register
      AT91C_TWI_IMR   : DWord absolute $FFFB802C; // Interrupt Mask Register
      AT91C_TWI_RHR   : DWord absolute $FFFB8030; // Receive Holding Register
      AT91C_TWI_THR   : DWord absolute $FFFB8034; // Transmit Holding Register
    // ========== Register definition for US0 peripheral ==========
      AT91C_US0_CR    : DWord absolute $FFFC0000; // Control Register
      AT91C_US0_MR    : DWord absolute $FFFC0004; // Mode Register
      AT91C_US0_IER   : DWord absolute $FFFC0008; // Interrupt Enable Register
      AT91C_US0_IDR   : DWord absolute $FFFC000C; // Interrupt Disable Register
      AT91C_US0_IMR   : DWord absolute $FFFC0010; // Interrupt Mask Register
      AT91C_US0_CSR   : DWord absolute $FFFC0014; // Channel Status Register
      AT91C_US0_RHR   : DWord absolute $FFFC0018; // Receiver Holding Register
      AT91C_US0_THR   : DWord absolute $FFFC001C; // Transmitter Holding Register
      AT91C_US0_BRGR  : DWord absolute $FFFC0020; // Baud Rate Generator Register
      AT91C_US0_RTOR  : DWord absolute $FFFC0024; // Receiver Time-out Register
      AT91C_US0_TTGR  : DWord absolute $FFFC0028; // Transmitter Time-guard Register
      AT91C_US0_FIDI  : DWord absolute $FFFC0040; // FI_DI_Ratio Register
      AT91C_US0_NER   : DWord absolute $FFFC0044; // Nb Errors Register
      AT91C_US0_IF    : DWord absolute $FFFC004C; // IRDA_FILTER Register
    // ========== Register definition for PDC_US0 peripheral ==========
      AT91C_US0_RPR   : DWord absolute $FFFC0100; // Receive Pointer Register
      AT91C_US0_RCR   : DWord absolute $FFFC0104; // Receive Counter Register
      AT91C_US0_TPR   : DWord absolute $FFFC0108; // Transmit Pointer Register
      AT91C_US0_TCR   : DWord absolute $FFFC010C; // Transmit Counter Register
      AT91C_US0_RNPR  : DWord absolute $FFFC0110; // Receive Next Pointer Register
      AT91C_US0_RNCR  : DWord absolute $FFFC0114; // Receive Next Counter Register
      AT91C_US0_TNPR  : DWord absolute $FFFC0118; // Transmit Next Pointer Register
      AT91C_US0_TNCR  : DWord absolute $FFFC011C; // Transmit Next Counter Register
      AT91C_US0_PTCR  : DWord absolute $FFFC0120; // PDC Transfer Control Register
      AT91C_US0_PTSR  : DWord absolute $FFFC0124; // PDC Transfer Status Register
    // ========== Register definition for US1 peripheral ==========
      AT91C_US1_CR    : DWord absolute $FFFC4000; // Control Register
      AT91C_US1_MR    : DWord absolute $FFFC4004; // Mode Register
      AT91C_US1_IER   : DWord absolute $FFFC4008; // Interrupt Enable Register
      AT91C_US1_IDR   : DWord absolute $FFFC400C; // Interrupt Disable Register
      AT91C_US1_IMR   : DWord absolute $FFFC4010; // Interrupt Mask Register
      AT91C_US1_CSR   : DWord absolute $FFFC4014; // Channel Status Register
      AT91C_US1_THR   : DWord absolute $FFFC401C; // Transmitter Holding Register
      AT91C_US1_RHR   : DWord absolute $FFFC4018; // Receiver Holding Register
      AT91C_US1_BRGR  : DWord absolute $FFFC4020; // Baud Rate Generator Register
      AT91C_US1_RTOR  : DWord absolute $FFFC4024; // Receiver Time-out Register
      AT91C_US1_TTGR  : DWord absolute $FFFC4028; // Transmitter Time-guard Register
      AT91C_US1_FIDI  : DWord absolute $FFFC4040; // FI_DI_Ratio Register
      AT91C_US1_NER   : DWord absolute $FFFC4044; // Nb Errors Register
      AT91C_US1_IF    : DWord absolute $FFFC404C; // IRDA_FILTER Register
    // ========== Register definition for PDC_US1 peripheral ==========
      AT91C_US1_RPR   : DWord absolute $FFFC4100; // Receive Pointer Register
      AT91C_US1_RCR   : DWord absolute $FFFC4104; // Receive Counter Register
      AT91C_US1_TPR   : DWord absolute $FFFC4108; // Transmit Pointer Register
      AT91C_US1_TCR   : DWord absolute $FFFC410C; // Transmit Counter Register
      AT91C_US1_RNPR  : DWord absolute $FFFC4110; // Receive Next Pointer Register
      AT91C_US1_RNCR  : DWord absolute $FFFC4114; // Receive Next Counter Register
      AT91C_US1_TNPR  : DWord absolute $FFFC4118; // Transmit Next Pointer Register
      AT91C_US1_TNCR  : DWord absolute $FFFC411C; // Transmit Next Counter Register
      AT91C_US1_PTCR  : DWord absolute $FFFC4120; // PDC Transfer Control Register
      AT91C_US1_PTSR  : DWord absolute $FFFC4124; // PDC Transfer Status Register
    // ========== Register definition for PWMC peripheral ==========
      AT91C_PWMC_MR   : DWord absolute $FFFCC000; // PWMC Mode Register
      AT91C_PWMC_ENA  : DWord absolute $FFFCC004; // PWMC Enable Register
      AT91C_PWMC_DIS  : DWord absolute $FFFCC008; // PWMC Disable Register
      AT91C_PWMC_SR   : DWord absolute $FFFCC00C; // PWMC Status Register
      AT91C_PWMC_IER  : DWord absolute $FFFCC010; // PWMC Interrupt Enable Register
      AT91C_PWMC_ISR  : DWord absolute $FFFCC01C; // PWMC Interrupt Status Register
      AT91C_PWMC_IDR  : DWord absolute $FFFCC014; // PWMC Interrupt Disable Register
      AT91C_PWMC_IMR  : DWord absolute $FFFCC018; // PWMC Interrupt Mask Register
      AT91C_PWMC_VR   : DWord absolute $FFFCC0FC; // PWMC Version Register
    // ========== Register definition for PWMC_CH0 peripheral ==========
      AT91C_PWMC_CH0_CMR   : DWord absolute $FFFCC200; // Channel Mode Register
      AT91C_PWMC_CH0_CDTYR : DWord absolute $FFFCC204; // Channel Duty Cycle Register
      AT91C_PWMC_CH0_CPRDR : DWord absolute $FFFCC208; // Channel Period Register
      AT91C_PWMC_CH0_CCNTR : DWord absolute $FFFCC20C; // Channel Counter Register
      AT91C_PWMC_CH0_CUPDR : DWord absolute $FFFCC210; // Channel Update Register
    // ========== Register definition for PWMC_CH1 peripheral ==========
      AT91C_PWMC_CH1_CMR   : DWord absolute $FFFCC220; // Channel Mode Register
      AT91C_PWMC_CH1_CDTYR : DWord absolute $FFFCC224; // Channel Duty Cycle Register
      AT91C_PWMC_CH1_CPRDR : DWord absolute $FFFCC228; // Channel Period Register
      AT91C_PWMC_CH1_CCNTR : DWord absolute $FFFCC22C; // Channel Counter Register
      AT91C_PWMC_CH1_CUPDR : DWord absolute $FFFCC230; // Channel Update Register
    // ========== Register definition for PWMC_CH2 peripheral ==========
      AT91C_PWMC_CH2_CMR   : DWord absolute $FFFCC240; // Channel Mode Register
      AT91C_PWMC_CH2_CDTYR : DWord absolute $FFFCC244; // Channel Duty Cycle Register
      AT91C_PWMC_CH2_CPRDR : DWord absolute $FFFCC248; // Channel Period Register
      AT91C_PWMC_CH2_CCNTR : DWord absolute $FFFCC24C; // Channel Counter Register
      AT91C_PWMC_CH2_CUPDR : DWord absolute $FFFCC250; // Channel Update Register
    // ========== Register definition for PWMC_CH3 peripheral ==========
      AT91C_PWMC_CH3_CMR   : DWord absolute $FFFCC260; // Channel Mode Register
      AT91C_PWMC_CH3_CDTYR : DWord absolute $FFFCC264; // Channel Duty Cycle Register
      AT91C_PWMC_CH3_CPRDR : DWord absolute $FFFCC268; // Channel Period Register
      AT91C_PWMC_CH3_CCNTR : DWord absolute $FFFCC26C; // Channel Counter Register
      AT91C_PWMC_CH3_CUPDR : DWord absolute $FFFCC270; // Channel Update Register
    // ========== Register definition for ADC peripheral ==========
      AT91C_ADC_CR    : DWord absolute $FFFD8000; // ADC Control Register
      AT91C_ADC_MR    : DWord absolute $FFFD8004; // ADC Mode Register
      AT91C_ADC_CHER  : DWord absolute $FFFD8010; // ADC Channel Enable Register
      AT91C_ADC_CHDR  : DWord absolute $FFFD8014; // ADC Channel Disable Register
      AT91C_ADC_CHSR  : DWord absolute $FFFD8018; // ADC Channel Status Register
      AT91C_ADC_SR    : DWord absolute $FFFD801C; // ADC Status Register
      AT91C_ADC_LCDR  : DWord absolute $FFFD8020; // ADC Last Converted Data Register
      AT91C_ADC_IER   : DWord absolute $FFFD8024; // ADC Interrupt Enable Register
      AT91C_ADC_IDR   : DWord absolute $FFFD8028; // ADC Interrupt Disable Register
      AT91C_ADC_IMR   : DWord absolute $FFFD802C; // ADC Interrupt Mask Register
      AT91C_ADC_CDR0  : DWord absolute $FFFD8030; // ADC Channel Data Register 0
      AT91C_ADC_CDR1  : DWord absolute $FFFD8034; // ADC Channel Data Register 1
      AT91C_ADC_CDR2  : DWord absolute $FFFD8038; // ADC Channel Data Register 2
      AT91C_ADC_CDR3  : DWord absolute $FFFD803C; // ADC Channel Data Register 3
      AT91C_ADC_CDR4  : DWord absolute $FFFD8040; // ADC Channel Data Register 4
      AT91C_ADC_CDR5  : DWord absolute $FFFD8044; // ADC Channel Data Register 5
      AT91C_ADC_CDR6  : DWord absolute $FFFD8048; // ADC Channel Data Register 6
      AT91C_ADC_CDR7  : DWord absolute $FFFD804C; // ADC Channel Data Register 7
    // ========== Register definition for PDC_ADC peripheral ==========
      AT91C_ADC_RPR   : DWord absolute $FFFD8100; // Receive Pointer Register
      AT91C_ADC_RCR   : DWord absolute $FFFD8104; // Receive Counter Register
      AT91C_ADC_TPR   : DWord absolute $FFFD8108; // Transmit Pointer Register
      AT91C_ADC_TCR   : DWord absolute $FFFD810C; // Transmit Counter Register
      AT91C_ADC_RNPR  : DWord absolute $FFFD8110; // Receive Next Pointer Register
      AT91C_ADC_RNCR  : DWord absolute $FFFD8114; // Receive Next Counter Register
      AT91C_ADC_TNPR  : DWord absolute $FFFD8118; // Transmit Next Pointer Register
      AT91C_ADC_TNCR  : DWord absolute $FFFD811C; // Transmit Next Counter Register
      AT91C_ADC_PTCR  : DWord absolute $FFFD8120; // PDC Transfer Control Register
      AT91C_ADC_PTSR  : DWord absolute $FFFD8124; // PDC Transfer Status Register


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
        AT91C_AIC_SVR[0]:=AT91_REG(@AT91F_Default_FIQ_handler);

        for i:=1 to 30 do
          AT91C_AIC_SVR[i]:=AT91_REG(@AT91F_Default_IRQ_handler);

        AT91C_AIC_SPU:=AT91_REG(@AT91F_Spurious_handler);
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
          Setting up SP for the different CPU modes.
          Change mode before setting each one
          move back again to Supervisor mode
          Each interrupt has its own link
          register, stack pointer and program
          counter The stack pointers must be
          initialized for interrupts to be
          used later.
        *)
        msr   cpsr_c, #0xdb     // switch to Undefined Instruction Mode
        mov   sp, r0
        sub   r0, r0, #0x10

        msr   cpsr_c, #0xd7   // switch to Abort Mode
        mov   sp, r0
        sub   r0, r0, #0x10

        msr   CPSR_c, #0xd1   // switch to FIQ Mode
        mov   sp, r0
        sub   r0, r0, #0x80

        msr   CPSR_c, #0xd2   // switch to IRQ Mode
        mov   sp, r0
        sub   r0, r0, #0x80

        msr   CPSR_c, #0xd3   // switch to Supervisor Mode
        mov   sp, r0
        sub   r0, r0, #0x80

        msr   CPSR_c, #0x1f   // switch to System Mode, interrupts enabled
        mov   sp, r0

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


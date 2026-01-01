unit AVR16EB20;

interface

type
  TAC = object //Analog Comparator
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    MUXCTRL: byte;  //Mux Control A
    Reserved3: byte;
    Reserved4: byte;
    DACREF: byte;  //DAC Voltage Reference
    INTCTRL: byte;  //Interrupt Control
    STATUS: byte;  //Status
  const
    // Enable
    ENABLEbm = $01;
    // AC_HYSMODE
    HYSMODEmask = $06;
    HYSMODE_NONE = $00;
    HYSMODE_SMALL = $02;
    HYSMODE_MEDIUM = $04;
    HYSMODE_LARGE = $06;
    // AC_POWER
    POWERmask = $18;
    POWER_PROFILE0 = $00;
    POWER_PROFILE1 = $08;
    // Output Pad Enable
    OUTENbm = $40;
    // Run in Standby Mode
    RUNSTDBYbm = $80;
    // AC_WINSEL
    WINSELmask = $03;
    WINSEL_DISABLED = $00;
    WINSEL_UPSEL1 = $01;
    // AC_MUXNEG
    MUXNEGmask = $07;
    MUXNEG_AINN0 = $00;
    MUXNEG_AINN1 = $01;
    MUXNEG_AINN2 = $02;
    MUXNEG_AINN3 = $03;
    MUXNEG_DACREF = $04;
    // AC_MUXPOS
    MUXPOSmask = $38;
    MUXPOS_AINP0 = $00;
    MUXPOS_AINP1 = $08;
    MUXPOS_AINP2 = $10;
    MUXPOS_AINP3 = $18;
    MUXPOS_AINP4 = $20;
    MUXPOS_AINP5 = $28;
    MUXPOS_AINP6 = $30;
    // AC_INITVAL
    INITVALmask = $40;
    INITVAL_LOW = $00;
    INITVAL_HIGH = $40;
    // Invert AC Output
    INVERTbm = $80;
    // Analog Comparator Interrupt Flag
    CMPIFbm = $01;
    // Analog Comparator State
    CMPSTATEbm = $10;
    // AC_WINSTATE
    WINSTATEmask = $C0;
    WINSTATE_ABOVE = $00;
    WINSTATE_INSIDE = $40;
    WINSTATE_BELOW = $80;
  end;

  TADC = object //Analog to Digital Converter
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    CTRLD: byte;  //Control D
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    STATUS: byte;  //Status register
    DBGCTRL: byte;  //Debug Control
    CTRLE: byte;  //Control E
    CTRLF: byte;  //Control F
    COMMAND: byte;  //Command register
    PGACTRL: byte;  //PGA Control
    MUXPOS: byte;  //Positive Input Multiplexer
    MUXNEG: byte;  //Negative Input Multiplexer
    Reserved14: byte;
    Reserved15: byte;
    RESULT: dword;  //Result
    SAMPLE: word;  //Sample
    Reserved22: byte;
    Reserved23: byte;
    TEMP0: byte;  //Temporary Data 0
    TEMP1: byte;  //Temporary Data 1
    TEMP2: byte;  //Temporary Data 2
    Reserved27: byte;
    WINLT: word;  //Window Low Threshold
    WINHT: word;  //Window High Threshold
  const
    // ADC Enable
    ENABLEbm = $01;
    // Low Latency
    LOWLATbm = $20;
    // Run in Standby
    RUNSTDBYbm = $80;
    // ADC_PRESC
    PRESCmask = $0F;
    PRESC_DIV2 = $00;
    PRESC_DIV4 = $01;
    PRESC_DIV6 = $02;
    PRESC_DIV8 = $03;
    PRESC_DIV10 = $04;
    PRESC_DIV12 = $05;
    PRESC_DIV14 = $06;
    PRESC_DIV16 = $07;
    PRESC_DIV20 = $08;
    PRESC_DIV24 = $09;
    PRESC_DIV28 = $0A;
    PRESC_DIV32 = $0B;
    PRESC_DIV40 = $0C;
    PRESC_DIV48 = $0D;
    PRESC_DIV56 = $0E;
    PRESC_DIV64 = $0F;
    // ADC_REFSEL
    REFSELmask = $07;
    REFSEL_VDD = $00;
    REFSEL_VREFA = $02;
    REFSEL_1V024 = $04;
    REFSEL_2V048 = $05;
    REFSEL_4V096 = $06;
    REFSEL_2V500 = $07;
    // ADC_WINCM
    WINCMmask = $07;
    WINCM_NONE = $00;
    WINCM_BELOW = $01;
    WINCM_ABOVE = $02;
    WINCM_INSIDE = $03;
    WINCM_OUTSIDE = $04;
    // ADC_WINSRC
    WINSRCmask = $08;
    WINSRC_RESULT = $00;
    WINSRC_SAMPLE = $08;
    // Result Ready Interrupt Enable
    RESRDYbm = $01;
    // Sample Ready Interrupt Enable
    SAMPRDYbm = $02;
    // Window Comparator Interrupt Enable
    WCMPbm = $04;
    // Result Overwrite Interrupt Enable
    RESOVRbm = $08;
    // Sample Overwrite Interrupt Enable
    SAMPOVRbm = $10;
    // Trigger Overrun Interrupt Enable
    TRIGOVRbm = $20;
    // ADC Busy
    ADCBUSYbm = $01;
    // Run in Debug Mode
    DBGRUNbm = $01;
    // ADC_SAMPNUM
    SAMPNUMmask = $0F;
    SAMPNUM_NONE = $00;
    SAMPNUM_ACC2 = $01;
    SAMPNUM_ACC4 = $02;
    SAMPNUM_ACC8 = $03;
    SAMPNUM_ACC16 = $04;
    SAMPNUM_ACC32 = $05;
    SAMPNUM_ACC64 = $06;
    SAMPNUM_ACC128 = $07;
    SAMPNUM_ACC256 = $08;
    SAMPNUM_ACC512 = $09;
    SAMPNUM_ACC1024 = $0A;
    // Left Adjust
    LEFTADJbm = $10;
    // Free-Running mode
    FREERUNbm = $20;
    // ADC_CHOPPING
    CHOPPINGmask = $40;
    CHOPPING_DISABLE = $00;
    CHOPPING_ENABLE = $40;
    // ADC_START
    STARTmask = $07;
    START_STOP = $00;
    START_IMMEDIATE = $01;
    START_MUXPOS_WRITE = $02;
    START_MUXNEG_WRITE = $03;
    START_EVENT_TRIGGER = $04;
    // ADC_MODE
    MODEmask = $70;
    MODE_SINGLE_8BIT = $00;
    MODE_SINGLE_12BIT = $10;
    MODE_SERIES = $20;
    MODE_SERIES_SCALING = $30;
    MODE_BURST = $40;
    MODE_BURST_SCALING = $50;
    // Differential mode
    DIFFbm = $80;
    // PGA Enable
    PGAENbm = $01;
    // ADC_PGABIASSEL
    PGABIASSELmask = $18;
    PGABIASSEL_100PCT = $00;
    PGABIASSEL_75PCT = $08;
    PGABIASSEL_50PCT = $10;
    PGABIASSEL_25PCT = $18;
    // ADC_GAIN
    GAINmask = $E0;
    GAIN_1X = $00;
    GAIN_2X = $20;
    GAIN_4X = $40;
    GAIN_8X = $60;
    GAIN_16X = $80;
    // ADC_MUXPOS
    MUXPOSmask = $3F;
    MUXPOS_AIN4 = $04;
    MUXPOS_AIN5 = $05;
    MUXPOS_AIN6 = $06;
    MUXPOS_AIN7 = $07;
    MUXPOS_AIN22 = $16;
    MUXPOS_AIN23 = $17;
    MUXPOS_AIN24 = $18;
    MUXPOS_AIN25 = $19;
    MUXPOS_AIN26 = $1A;
    MUXPOS_AIN27 = $1B;
    MUXPOS_AIN28 = $1C;
    MUXPOS_AIN29 = $1D;
    MUXPOS_AIN30 = $1E;
    MUXPOS_AIN31 = $1F;
    MUXPOS_GND = $30;
    MUXPOS_VDD10 = $31;
    MUXPOS_TEMPSENSE = $32;
    // ADC_VIA
    VIAmask = $C0;
    VIA_DIRECT = $00;
    VIA_PGA = $40;
    // ADC_MUXNEG
    MUXNEGmask = $3F;
    MUXNEG_AIN4 = $04;
    MUXNEG_AIN5 = $05;
    MUXNEG_AIN6 = $06;
    MUXNEG_AIN7 = $07;
    MUXNEG_AIN22 = $16;
    MUXNEG_AIN23 = $17;
    MUXNEG_AIN24 = $18;
    MUXNEG_AIN25 = $19;
    MUXNEG_AIN26 = $1A;
    MUXNEG_AIN27 = $1B;
    MUXNEG_AIN28 = $1C;
    MUXNEG_AIN29 = $1D;
    MUXNEG_AIN30 = $1E;
    MUXNEG_AIN31 = $1F;
    MUXNEG_GND = $30;
    MUXNEG_DAC0 = $38;
    MUXNEG_DACREF0 = $39;
    MUXNEG_DACREF1 = $3A;
  end;

  TBOD = object //Bod interface
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    Reserved2: byte;
    Reserved3: byte;
    Reserved4: byte;
    Reserved5: byte;
    Reserved6: byte;
    Reserved7: byte;
    VLMCTRLA: byte;  //Voltage level monitor Control
    INTCTRL: byte;  //Voltage level monitor interrupt Control
    INTFLAGS: byte;  //Voltage level monitor interrupt Flags
    STATUS: byte;  //Voltage level monitor status
  const
    // BOD_SLEEP
    SLEEPmask = $03;
    SLEEP_DISABLE = $00;
    SLEEP_ENABLE = $01;
    SLEEP_SAMPLE = $02;
    // BOD_ACTIVE
    ACTIVEmask = $0C;
    ACTIVE_DISABLE = $00;
    ACTIVE_ENABLED = $04;
    ACTIVE_SAMPLED = $08;
    ACTIVE_ENABLEWAIT = $0C;
    // BOD_SAMPFREQ
    SAMPFREQmask = $10;
    SAMPFREQ_128HZ = $00;
    SAMPFREQ_32HZ = $10;
    // BOD_LVL
    LVLmask = $07;
    LVL_BODLEVEL0 = $00;
    LVL_BODLEVEL1 = $01;
    LVL_BODLEVEL2 = $02;
    LVL_BODLEVEL3 = $03;
    // BOD_VLMLVL
    VLMLVLmask = $03;
    VLMLVL_OFF = $00;
    VLMLVL_5ABOVE = $01;
    VLMLVL_15ABOVE = $02;
    VLMLVL_25ABOVE = $03;
    // voltage level monitor interrupt enable
    VLMIEbm = $01;
    // BOD_VLMCFG
    VLMCFGmask = $06;
    VLMCFG_FALLING = $00;
    VLMCFG_RISING = $02;
    VLMCFG_BOTH = $04;
    // Voltage level monitor interrupt flag
    VLMIFbm = $01;
    // BOD_VLMS
    VLMSmask = $01;
    VLMS_ABOVE = $00;
    VLMS_BELOW = $01;
  end;

  TBOOTROW = object //Boot Row
    BOOTROW: byte;  //Boot Row
  end;

  TCCL = object //Configurable Custom Logic
    CTRLA: byte;  //Control Register A
    SEQCTRL0: byte;  //Sequential Control 0
    SEQCTRL1: byte;  //Sequential Control 1
    Reserved3: byte;
    Reserved4: byte;
    INTCTRL0: byte;  //Interrupt Control 0
    Reserved6: byte;
    INTFLAGS: byte;  //Interrupt Flags
    LUT0CTRLA: byte;  //LUT 0 Control A
    LUT0CTRLB: byte;  //LUT 0 Control B
    LUT0CTRLC: byte;  //LUT 0 Control C
    TRUTH0: byte;  //Truth 0
    LUT1CTRLA: byte;  //LUT 1 Control A
    LUT1CTRLB: byte;  //LUT 1 Control B
    LUT1CTRLC: byte;  //LUT 1 Control C
    TRUTH1: byte;  //Truth 1
    LUT2CTRLA: byte;  //LUT 2 Control A
    LUT2CTRLB: byte;  //LUT 2 Control B
    LUT2CTRLC: byte;  //LUT 2 Control C
    TRUTH2: byte;  //Truth 2
    LUT3CTRLA: byte;  //LUT 3 Control A
    LUT3CTRLB: byte;  //LUT 3 Control B
    LUT3CTRLC: byte;  //LUT 3 Control C
    TRUTH3: byte;  //Truth 3
  const
    // Enable
    ENABLEbm = $01;
    // Run in Standby
    RUNSTDBYbm = $40;
    // CCL_SEQSEL
    SEQSELmask = $0F;
    SEQSEL_DISABLE = $00;
    SEQSEL_DFF = $01;
    SEQSEL_JK = $02;
    SEQSEL_LATCH = $03;
    SEQSEL_RS = $04;
    // CCL_INTMODE0
    INTMODE0mask = $03;
    INTMODE0_INTDISABLE = $00;
    INTMODE0_RISING = $01;
    INTMODE0_FALLING = $02;
    INTMODE0_BOTH = $03;
    // CCL_INTMODE1
    INTMODE1mask = $0C;
    INTMODE1_INTDISABLE = $00;
    INTMODE1_RISING = $04;
    INTMODE1_FALLING = $08;
    INTMODE1_BOTH = $0C;
    // CCL_INTMODE2
    INTMODE2mask = $30;
    INTMODE2_INTDISABLE = $00;
    INTMODE2_RISING = $10;
    INTMODE2_FALLING = $20;
    INTMODE2_BOTH = $30;
    // CCL_INTMODE3
    INTMODE3mask = $C0;
    INTMODE3_INTDISABLE = $00;
    INTMODE3_RISING = $40;
    INTMODE3_FALLING = $80;
    INTMODE3_BOTH = $C0;
    // Interrupt Flag
    INT0bm = $01;
    INT1bm = $02;
    INT2bm = $04;
    INT3bm = $08;
    // CCL_CLKSRC
    CLKSRCmask = $0E;
    CLKSRC_CLKPER = $00;
    CLKSRC_IN2 = $02;
    CLKSRC_OSCHF = $08;
    CLKSRC_OSC32K = $0A;
    CLKSRC_OSC1K = $0C;
    CLKSRC_PLL = $0E;
    // CCL_FILTSEL
    FILTSELmask = $30;
    FILTSEL_DISABLE = $00;
    FILTSEL_SYNCH = $10;
    FILTSEL_FILTER = $20;
    // Output Enable
    OUTENbm = $40;
    // CCL_EDGEDET
    EDGEDETmask = $80;
    EDGEDET_DIS = $00;
    EDGEDET_EN = $80;
    // CCL_INSEL0
    INSEL0mask = $0F;
    INSEL0_MASK = $00;
    INSEL0_FEEDBACK = $01;
    INSEL0_LINK = $02;
    INSEL0_EVENTA = $03;
    INSEL0_EVENTB = $04;
    INSEL0_IN0 = $05;
    INSEL0_AC0 = $06;
    INSEL0_USART0 = $07;
    INSEL0_SPI0 = $08;
    INSEL0_TCE0 = $09;
    INSEL0_TCB0 = $0A;
    INSEL0_TCF0 = $0B;
    INSEL0_WEX0 = $0C;
    // CCL_INSEL1
    INSEL1mask = $F0;
    INSEL1_MASK = $00;
    INSEL1_FEEDBACK = $10;
    INSEL1_LINK = $20;
    INSEL1_EVENTA = $30;
    INSEL1_EVENTB = $40;
    INSEL1_IN1 = $50;
    INSEL1_AC1 = $60;
    INSEL1_USART0 = $70;
    INSEL1_SPI0 = $80;
    INSEL1_TCE0 = $90;
    INSEL1_TCB1 = $A0;
    INSEL1_TCF0 = $B0;
    INSEL1_WEX0 = $C0;
    // CCL_INSEL2
    INSEL2mask = $0F;
    INSEL2_MASK = $00;
    INSEL2_FEEDBACK = $01;
    INSEL2_LINK = $02;
    INSEL2_EVENTA = $03;
    INSEL2_EVENTB = $04;
    INSEL2_IN2 = $05;
    INSEL2_AC1 = $06;
    INSEL2_USART0 = $07;
    INSEL2_SPI0 = $08;
    INSEL2_TCE0 = $09;
    INSEL2_TCB1 = $0A;
    INSEL2_TCF0 = $0B;
    INSEL2_WEX0 = $0C;
  end;

  TCLKCTRL = object //Clock controller
    MCLKCTRLA: byte;  //MCLK Control A
    MCLKCTRLB: byte;  //MCLK Control B
    Reserved2: byte;
    Reserved3: byte;
    Reserved4: byte;
    MCLKSTATUS: byte;  //MCLK Status
    MCLKTIMEBASE: byte;  //MCLK Timebase
    Reserved7: byte;
    OSCHFCTRLA: byte;  //OSCHF Control A
    OSCHFTUNE: byte;  //OSCHF Tune
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    PLLCTRLA: byte;  //PLL Control A
    PLLCTRLB: byte;  //PLL Control B
    Reserved18: byte;
    Reserved19: byte;
    Reserved20: byte;
    Reserved21: byte;
    Reserved22: byte;
    Reserved23: byte;
    OSC32KCTRLA: byte;  //OSC32K Control A
    Reserved25: byte;
    Reserved26: byte;
    Reserved27: byte;
    XOSC32KCTRLA: byte;  //XOSC32K Control A
  const
    // CLKCTRL_CLKSEL
    CLKSELmask = $0F;
    CLKSEL_OSCHF = $00;
    CLKSEL_OSC32K = $01;
    CLKSEL_XOSC32K = $02;
    CLKSEL_EXTCLK = $03;
    CLKSEL_PLL = $04;
    // System clock out
    CLKOUTbm = $80;
    // Prescaler enable
    PENbm = $01;
    // CLKCTRL_PDIV
    PDIVmask = $1E;
    PDIV_DIV2 = $00;
    PDIV_DIV4 = $02;
    PDIV_DIV8 = $04;
    PDIV_DIV16 = $06;
    PDIV_DIV32 = $08;
    PDIV_DIV64 = $0A;
    PDIV_DIV6 = $10;
    PDIV_DIV10 = $12;
    PDIV_DIV12 = $14;
    PDIV_DIV24 = $16;
    PDIV_DIV48 = $18;
    // CLKCTRL_PBDIV
    PBDIVmask = $20;
    PBDIV_NONE = $00;
    PBDIV_DIV4 = $20;
    // System Oscillator changing
    SOSCbm = $01;
    // High frequency oscillator status
    OSCHFSbm = $02;
    // 32KHz oscillator status
    OSC32KSbm = $04;
    // 32.768 kHz Crystal Oscillator status
    XOSC32KSbm = $08;
    // External Clock status
    EXTSbm = $10;
    // PLL status
    PLLSbm = $20;
    // Timebase
    TIMEBASE0bm = $01;
    TIMEBASE1bm = $02;
    TIMEBASE2bm = $04;
    TIMEBASE3bm = $08;
    TIMEBASE4bm = $10;
    // CLKCTRL_AUTOTUNE
    AUTOTUNEmask = $03;
    AUTOTUNE_OFF = $00;
    AUTOTUNE_XOSC32K = $01;
    // Run in standby
    RUNSTDBYbm = $80;
    // CLKCTRL_MULFAC
    MULFACmask = $03;
    MULFAC_OFF = $00;
    MULFAC_8X = $02;
    MULFAC_16X = $03;
    // CLKCTRL_SOURCEDIV
    SOURCEDIVmask = $18;
    SOURCEDIV_DIV1 = $00;
    SOURCEDIV_DIV2 = $08;
    SOURCEDIV_DIV4 = $10;
    SOURCEDIV_DIV6 = $18;
    // CLKCTRL_SOURCE
    SOURCEmask = $60;
    SOURCE_OSCHF = $00;
    SOURCE_EXTCLK = $20;
    // CLKCTRL_CLKDIV
    CLKDIVmask = $01;
    CLKDIV_NONE = $00;
    CLKDIV_DIV2 = $01;
    // Enable
    ENABLEbm = $01;
    // Low power mode
    LPMODEbm = $02;
    // Select
    SELbm = $04;
    // CLKCTRL_CSUT
    CSUTmask = $30;
    CSUT_1K = $00;
    CSUT_16K = $10;
    CSUT_32K = $20;
    CSUT_64K = $30;
  end;

  TCPU = object //CPU
    Reserved0: byte;
    Reserved1: byte;
    Reserved2: byte;
    Reserved3: byte;
    CCP: byte;  //Configuration Change Protection
    Reserved5: byte;
    Reserved6: byte;
    Reserved7: byte;
    Reserved8: byte;
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    SP: word;  //Stack Pointer
    SREG: byte;  //Status Register
  const
    // CPU_CCP
    CCPmask = $FF;
    CCP_SPM = $9D;
    CCP_IOREG = $D8;
    // Carry Flag
    Cbm = $01;
    // Zero Flag
    Zbm = $02;
    // Negative Flag
    Nbm = $04;
    // Two's Complement Overflow Flag
    Vbm = $08;
    // N Exclusive Or V Flag
    Sbm = $10;
    // Half Carry Flag
    Hbm = $20;
    // Transfer Bit
    Tbm = $40;
    // Global Interrupt Enable Flag
    Ibm = $80;
  end;

  TCPUINT = object //Interrupt Controller
    CTRLA: byte;  //Control A
    STATUS: byte;  //Status
    LVL0PRI: byte;  //Interrupt Level 0 Priority
    LVL1VEC: byte;  //Interrupt Level 1 Priority Vector
  const
    // Round-robin Scheduling Enable
    LVL0RRbm = $01;
    // Compact Vector Table
    CVTbm = $20;
    // Interrupt Vector Select
    IVSELbm = $40;
    // Level 0 Interrupt Executing
    LVL0EXbm = $01;
    // Level 1 Interrupt Executing
    LVL1EXbm = $02;
    // Non-maskable Interrupt Executing
    NMIEXbm = $80;
  end;

  TCRCSCAN = object //CRCSCAN
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    STATUS: byte;  //Status
  const
    // Enable CRC scan
    ENABLEbm = $01;
    // Enable NMI Trigger
    NMIENbm = $02;
    // Reset CRC scan
    RESETbm = $80;
    // CRCSCAN_SRC
    SRCmask = $03;
    SRC_FLASH = $00;
    SRC_APPLICATION = $01;
    SRC_BOOT = $02;
    // CRC Busy
    BUSYbm = $01;
    // CRC Ok
    OKbm = $02;
  end;

  TEVSYS = object //Event System
    SWEVENTA: byte;  //Software Event A
    Reserved1: byte;
    Reserved2: byte;
    Reserved3: byte;
    Reserved4: byte;
    Reserved5: byte;
    Reserved6: byte;
    Reserved7: byte;
    Reserved8: byte;
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    CHANNEL0: byte;  //Multiplexer Channel 0
    CHANNEL1: byte;  //Multiplexer Channel 1
    CHANNEL2: byte;  //Multiplexer Channel 2
    CHANNEL3: byte;  //Multiplexer Channel 3
    CHANNEL4: byte;  //Multiplexer Channel 4
    CHANNEL5: byte;  //Multiplexer Channel 5
    Reserved22: byte;
    Reserved23: byte;
    Reserved24: byte;
    Reserved25: byte;
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    Reserved30: byte;
    Reserved31: byte;
    USERCCLLUT0A: byte;  //CCL0 Event A
    USERCCLLUT0B: byte;  //CCL0 Event B
    USERCCLLUT1A: byte;  //CCL1 Event A
    USERCCLLUT1B: byte;  //CCL1 Event B
    USERCCLLUT2A: byte;  //CCL2 Event A
    USERCCLLUT2B: byte;  //CCL2 Event B
    USERCCLLUT3A: byte;  //CCL3 Event A
    USERCCLLUT3B: byte;  //CCL3 Event B
    USERADC0START: byte;  //ADC0 Start
    USEREVSYSEVOUTA: byte;  //EVOUTA
    USEREVSYSEVOUTC: byte;  //EVOUTC
    USEREVSYSEVOUTD: byte;  //EVOUTD
    USEREVSYSEVOUTF: byte;  //EVOUTF
    USERUSART0IRDA: byte;  //USART0 IrDA Event
    USERTCE0CNTA: byte;  //TCE0 Event A
    USERTCE0CNTB: byte;  //TCE0 Event B
    USERTCB0CAPT: byte;  //TCB0 Event A
    USERTCB0COUNT: byte;  //TCB0 Event B
    USERTCB1CAPT: byte;  //TCB1 Event A
    USERTCB1COUNT: byte;  //TCB1 Event B
    USERTCF0CNT: byte;  //TCF0 Clock Event
    USERTCF0ACT: byte;  //TCF0 Action Event
    USERWEXA: byte;  //WEX Event A
    USERWEXB: byte;  //WEX Event B
    USERWEXC: byte;  //WEX Event C
  const
    // EVSYS_SWEVENTA
    SWEVENTAmask = $FF;
    SWEVENTA_CH0 = $01;
    SWEVENTA_CH1 = $02;
    SWEVENTA_CH2 = $04;
    SWEVENTA_CH3 = $08;
    SWEVENTA_CH4 = $10;
    SWEVENTA_CH5 = $20;
    SWEVENTA_CH6 = $40;
    SWEVENTA_CH7 = $80;
    // EVSYS_CHANNEL
    CHANNELmask = $FF;
    CHANNEL_OFF = $00;
    CHANNEL_UPDI_SYNCH = $01;
    CHANNEL_RTC_OVF = $06;
    CHANNEL_RTC_CMP = $07;
    CHANNEL_RTC_PITEV0 = $08;
    CHANNEL_RTC_PITEV1 = $09;
    CHANNEL_CCL_LUT0 = $10;
    CHANNEL_CCL_LUT1 = $11;
    CHANNEL_CCL_LUT2 = $12;
    CHANNEL_CCL_LUT3 = $13;
    CHANNEL_AC0_OUT = $20;
    CHANNEL_AC1_OUT = $21;
    CHANNEL_ADC0_RES = $24;
    CHANNEL_ADC0_SAMP = $25;
    CHANNEL_ADC0_WCMP = $26;
    CHANNEL_PORTA_EV0 = $40;
    CHANNEL_PORTA_EV1 = $41;
    CHANNEL_PORTC_EV0 = $44;
    CHANNEL_PORTC_EV1 = $45;
    CHANNEL_PORTD_EV0 = $46;
    CHANNEL_PORTD_EV1 = $47;
    CHANNEL_PORTF_EV0 = $4A;
    CHANNEL_PORTF_EV1 = $4B;
    CHANNEL_USART0_XCK = $60;
    CHANNEL_SPI0_SCK = $68;
    CHANNEL_TCE0_OVF = $80;
    CHANNEL_TCE0_CMP0 = $84;
    CHANNEL_TCE0_CMP1 = $85;
    CHANNEL_TCE0_CMP2 = $86;
    CHANNEL_TCE0_CMP3 = $87;
    CHANNEL_TCB0_CAPT = $A0;
    CHANNEL_TCB0_OVF = $A1;
    CHANNEL_TCB1_CAPT = $A2;
    CHANNEL_TCB1_OVF = $A3;
    CHANNEL_TCF0_OVF = $B8;
    CHANNEL_TCF0_CMP0 = $B9;
    CHANNEL_TCF0_CMP1 = $BA;
    // EVSYS_USER
    USERmask = $FF;
    USER_OFF = $00;
    USER_CHANNEL0 = $01;
    USER_CHANNEL1 = $02;
    USER_CHANNEL2 = $03;
    USER_CHANNEL3 = $04;
    USER_CHANNEL4 = $05;
    USER_CHANNEL5 = $06;
  end;

  TFUSE = object //Fuses
    WDTCFG: byte;  //Watchdog Configuration
    BODCFG: byte;  //BOD Configuration
    OSCCFG: byte;  //Oscillator Configuration
    Reserved3: byte;
    Reserved4: byte;
    SYSCFG0: byte;  //System Configuration 0
    SYSCFG1: byte;  //System Configuration 1
    CODESIZE: byte;  //Code Section Size
    BOOTSIZE: byte;  //Boot Section Size
    Reserved9: byte;
    PDICFG: word;  //Programming and Debugging Interface Configuration
  const
    // FUSE_PERIOD
    PERIODmask = $0F;
    PERIOD_OFF = $00;
    PERIOD_8CLK = $01;
    PERIOD_16CLK = $02;
    PERIOD_32CLK = $03;
    PERIOD_64CLK = $04;
    PERIOD_128CLK = $05;
    PERIOD_256CLK = $06;
    PERIOD_512CLK = $07;
    PERIOD_1KCLK = $08;
    PERIOD_2KCLK = $09;
    PERIOD_4KCLK = $0A;
    PERIOD_8KCLK = $0B;
    // FUSE_WINDOW
    WINDOWmask = $F0;
    WINDOW_OFF = $00;
    WINDOW_8CLK = $10;
    WINDOW_16CLK = $20;
    WINDOW_32CLK = $30;
    WINDOW_64CLK = $40;
    WINDOW_128CLK = $50;
    WINDOW_256CLK = $60;
    WINDOW_512CLK = $70;
    WINDOW_1KCLK = $80;
    WINDOW_2KCLK = $90;
    WINDOW_4KCLK = $A0;
    WINDOW_8KCLK = $B0;
    // FUSE_SLEEP
    SLEEPmask = $03;
    SLEEP_DISABLE = $00;
    SLEEP_ENABLE = $01;
    SLEEP_SAMPLE = $02;
    // FUSE_ACTIVE
    ACTIVEmask = $0C;
    ACTIVE_DISABLE = $00;
    ACTIVE_ENABLED = $04;
    ACTIVE_SAMPLED = $08;
    ACTIVE_ENABLEWAIT = $0C;
    // FUSE_SAMPFREQ
    SAMPFREQmask = $10;
    SAMPFREQ_128HZ = $00;
    SAMPFREQ_32HZ = $10;
    // FUSE_LVL
    LVLmask = $E0;
    LVL_BODLEVEL0 = $00;
    LVL_BODLEVEL1 = $20;
    LVL_BODLEVEL2 = $40;
    LVL_BODLEVEL3 = $60;
    // FUSE_OSCHFFRQ
    OSCHFFRQmask = $08;
    OSCHFFRQ_20M = $00;
    OSCHFFRQ_16M = $08;
    // FUSE_EESAVE
    EESAVEmask = $01;
    EESAVE_DISABLE = $00;
    EESAVE_ENABLE = $01;
    // FUSE_RSTPINCFG
    RSTPINCFGmask = $08;
    RSTPINCFG_NONE = $00;
    RSTPINCFG_RESET = $08;
    // FUSE_UPDIPINCFG
    UPDIPINCFGmask = $10;
    UPDIPINCFG_GPIO = $00;
    UPDIPINCFG_UPDI = $10;
    // FUSE_CRCSEL
    CRCSELmask = $20;
    CRCSEL_CRC16 = $00;
    CRCSEL_CRC32 = $20;
    // FUSE_CRCSRC
    CRCSRCmask = $C0;
    CRCSRC_FLASH = $00;
    CRCSRC_BOOT = $40;
    CRCSRC_BOOTAPP = $80;
    CRCSRC_NOCRC = $C0;
    // FUSE_SUT
    SUTmask = $07;
    SUT_0MS = $00;
    SUT_1MS = $01;
    SUT_2MS = $02;
    SUT_4MS = $03;
    SUT_8MS = $04;
    SUT_16MS = $05;
    SUT_32MS = $06;
    SUT_64MS = $07;
    // FUSE_LEVEL
    LEVELmask = $03;
    LEVEL_NVMACCDIS = $02;
    LEVEL_BASIC = $03;
    // FUSE_KEY
    KEYmask = $FFF0;
    KEY_NOTACT = $00;
    KEY_NVMACT = $B450;
  end;

  TGPR = object //General Purpose Registers
    GPR0: byte;  //General Purpose Register 0
    GPR1: byte;  //General Purpose Register 1
    GPR2: byte;  //General Purpose Register 2
    GPR3: byte;  //General Purpose Register 3
  end;

  TLOCK = object //Lockbits
    KEY: dword;  //Lock Key Bits
  const
    // LOCK_KEY
    KEYmask = $FFFFFFFF;
    KEY_NOLOCK = $5CC5C55C;
    KEY_RWLOCK = $A33A3AA3;
  end;

  TNVMCTRL = object //Non-volatile Memory Controller
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    Reserved3: byte;
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    STATUS: byte;  //Status
    Reserved7: byte;
    DATA: word;  //Data
    Reserved10: byte;
    Reserved11: byte;
    ADDR: dword;  //Address
  const
    // NVMCTRL_CMD
    CMDmask = $7F;
    CMD_NOCMD = $00;
    CMD_NOOP = $01;
    CMD_FLPW = $04;
    CMD_FLPERW = $05;
    CMD_FLPER = $08;
    CMD_FLMPER2 = $09;
    CMD_FLMPER4 = $0A;
    CMD_FLMPER8 = $0B;
    CMD_FLMPER16 = $0C;
    CMD_FLMPER32 = $0D;
    CMD_FLPBCLR = $0F;
    CMD_EEPW = $14;
    CMD_EEPERW = $15;
    CMD_EEPER = $17;
    CMD_EEPBCLR = $1F;
    CMD_CHER = $20;
    CMD_EECHER = $30;
    // Application Code Write Protect
    APPCODEWPbm = $01;
    // Boot Read Protect
    BOOTRPbm = $02;
    // Application Data Write Protect
    APPDATAWPbm = $04;
    // EEPROM Write Protect
    EEWPbm = $08;
    // NVMCTRL_FLMAP
    FLMAPmask = $30;
    FLMAP_SECTION0 = $00;
    FLMAP_SECTION1 = $10;
    FLMAP_SECTION2 = $20;
    FLMAP_SECTION3 = $30;
    // Flash Mapping Lock
    FLMAPLOCKbm = $80;
    // User Row Write Protect
    UROWWPbm = $01;
    // Boot Row Write Protect
    BOOTROWWPbm = $02;
    // EEPROM Ready
    EEREADYbm = $01;
    // Flash Ready
    FLREADYbm = $02;
    // EEPROM busy
    EEBUSYbm = $01;
    // Flash busy
    FLBUSYbm = $02;
    // NVMCTRL_ERROR
    ERRORmask = $70;
    ERROR_NOERROR = $00;
    ERROR_WRITEPROTECT = $20;
    ERROR_CMDCOLLISION = $30;
    ERROR_WRONGSECTION = $40;
  end;

  TPORT = object //I/O Ports
    DIR: byte;  //Data Direction
    DIRSET: byte;  //Data Direction Set
    DIRCLR: byte;  //Data Direction Clear
    DIRTGL: byte;  //Data Direction Toggle
    OUT_: byte;  //Output Value
    OUTSET: byte;  //Output Value Set
    OUTCLR: byte;  //Output Value Clear
    OUTTGL: byte;  //Output Value Toggle
    IN_: byte;  //Input Value
    INTFLAGS: byte;  //Interrupt Flags
    PORTCTRL: byte;  //Port Control
    PINCONFIG: byte;  //Pin Control Config
    PINCTRLUPD: byte;  //Pin Control Update
    PINCTRLSET: byte;  //Pin Control Set
    PINCTRLCLR: byte;  //Pin Control Clear
    Reserved15: byte;
    PIN0CTRL: byte;  //Pin 0 Control
    PIN1CTRL: byte;  //Pin 1 Control
    PIN2CTRL: byte;  //Pin 2 Control
    PIN3CTRL: byte;  //Pin 3 Control
    PIN4CTRL: byte;  //Pin 4 Control
    PIN5CTRL: byte;  //Pin 5 Control
    PIN6CTRL: byte;  //Pin 6 Control
    PIN7CTRL: byte;  //Pin 7 Control
    EVGENCTRLA: byte;  //Event Generation Control A
  const
    // Slew Rate Limit Enable
    SRLbm = $01;
    // PORT_ISC
    ISCmask = $07;
    ISC_INTDISABLE = $00;
    ISC_BOTHEDGES = $01;
    ISC_RISING = $02;
    ISC_FALLING = $03;
    ISC_INPUT_DISABLE = $04;
    ISC_LEVEL = $05;
    // Pullup enable
    PULLUPENbm = $08;
    // PORT_INLVL
    INLVLmask = $40;
    INLVL_ST = $00;
    INLVL_TTL = $40;
    // Inverted I/O Enable
    INVENbm = $80;
    // PORT_EVGEN0SEL
    EVGEN0SELmask = $07;
    EVGEN0SEL_PIN0 = $00;
    EVGEN0SEL_PIN1 = $01;
    EVGEN0SEL_PIN2 = $02;
    EVGEN0SEL_PIN3 = $03;
    EVGEN0SEL_PIN4 = $04;
    EVGEN0SEL_PIN5 = $05;
    EVGEN0SEL_PIN6 = $06;
    EVGEN0SEL_PIN7 = $07;
    // PORT_EVGEN1SEL
    EVGEN1SELmask = $70;
    EVGEN1SEL_PIN0 = $00;
    EVGEN1SEL_PIN1 = $10;
    EVGEN1SEL_PIN2 = $20;
    EVGEN1SEL_PIN3 = $30;
    EVGEN1SEL_PIN4 = $40;
    EVGEN1SEL_PIN5 = $50;
    EVGEN1SEL_PIN6 = $60;
    EVGEN1SEL_PIN7 = $70;
  end;

  TPORTMUX = object //Port Multiplexer
    EVSYSROUTEA: byte;  //EVSYS route A
    CCLROUTEA: byte;  //CCL route A
    USARTROUTEA: byte;  //USART route A
    Reserved3: byte;
    Reserved4: byte;
    SPIROUTEA: byte;  //SPI route A
    TWIROUTEA: byte;  //TWI route A
    TCEROUTEA: byte;  //TCE route A
    TCBROUTEA: byte;  //TCB route A
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    TCFROUTEA: byte;  //TCF Route A
  const
    // PORTMUX_EVOUTA
    EVOUTAmask = $01;
    EVOUTA_DEFAULT = $00;
    EVOUTA_ALT1 = $01;
    // PORTMUX_EVOUTC
    EVOUTCmask = $04;
    EVOUTC_DEFAULT = $00;
    // PORTMUX_EVOUTD
    EVOUTDmask = $08;
    EVOUTD_DEFAULT = $00;
    EVOUTD_ALT1 = $08;
    // PORTMUX_EVOUTF
    EVOUTFmask = $20;
    EVOUTF_DEFAULT = $00;
    EVOUTF_ALT1 = $20;
    // PORTMUX_LUT0
    LUT0mask = $01;
    LUT0_DEFAULT = $00;
    LUT0_ALT1 = $01;
    // PORTMUX_LUT1
    LUT1mask = $02;
    LUT1_DEFAULT = $00;
    LUT1_ALT1 = $02;
    // PORTMUX_LUT2
    LUT2mask = $04;
    LUT2_DEFAULT = $00;
    LUT2_ALT1 = $04;
    // PORTMUX_USART0
    USART0mask = $07;
    USART0_DEFAULT = $00;
    USART0_ALT1 = $01;
    USART0_ALT2 = $02;
    USART0_ALT3 = $03;
    USART0_ALT4 = $04;
    USART0_ALT6 = $06;
    USART0_NONE = $07;
    // PORTMUX_SPI0
    SPI0mask = $07;
    SPI0_DEFAULT = $00;
    SPI0_ALT3 = $03;
    SPI0_ALT4 = $04;
    SPI0_ALT5 = $05;
    SPI0_ALT6 = $06;
    SPI0_NONE = $07;
    // PORTMUX_TWI0
    TWI0mask = $03;
    TWI0_DEFAULT = $00;
    TWI0_ALT1 = $01;
    TWI0_ALT2 = $02;
    TWI0_ALT3 = $03;
    // PORTMUX_TCE0
    TCE0mask = $0F;
    TCE0_PORTA = $00;
    TCE0_PORTC = $02;
    TCE0_PORTD = $03;
    TCE0_PORTC2 = $08;
    TCE0_PORTA2 = $09;
    // PORTMUX_TCB0
    TCB0mask = $01;
    TCB0_DEFAULT = $00;
    // PORTMUX_TCB1
    TCB1mask = $02;
    TCB1_DEFAULT = $00;
    // PORTMUX_TCF0
    TCF0mask = $03;
    TCF0_DEFAULT = $00;
    TCF0_ALT1 = $01;
  end;

  TRSTCTRL = object //Reset controller
    RSTFR: byte;  //Reset Flags
    SWRR: byte;  //Software Reset
  const
    // Power on Reset flag
    PORFbm = $01;
    // Brown out detector Reset flag
    BORFbm = $02;
    // External Reset flag
    EXTRFbm = $04;
    // Watch dog Reset flag
    WDRFbm = $08;
    // Software Reset flag
    SWRFbm = $10;
    // UPDI Reset flag
    UPDIRFbm = $20;
    // Software Reset Enable
    SWREbm = $01;
  end;

  TRTC = object //Real-Time Counter
    CTRLA: byte;  //Control A
    STATUS: byte;  //Status
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    TEMP: byte;  //Temporary
    DBGCTRL: byte;  //Debug control
    CALIB: byte;  //Calibration
    CLKSEL: byte;  //Clock Select
    CNT: word;  //Counter
    PER: word;  //Period
    CMP: word;  //Compare
    Reserved14: byte;
    Reserved15: byte;
    PITCTRLA: byte;  //PIT Control A
    PITSTATUS: byte;  //PIT Status
    PITINTCTRL: byte;  //PIT Interrupt Control
    PITINTFLAGS: byte;  //PIT Interrupt Flags
    Reserved20: byte;
    PITDBGCTRL: byte;  //PIT Debug control
    PITEVGENCTRLA: byte;  //PIT Event Generation Control A
  const
    // Enable
    RTCENbm = $01;
    // Correction enable
    CORRENbm = $04;
    // RTC_PRESCALER
    PRESCALERmask = $78;
    PRESCALER_DIV1 = $00;
    PRESCALER_DIV2 = $08;
    PRESCALER_DIV4 = $10;
    PRESCALER_DIV8 = $18;
    PRESCALER_DIV16 = $20;
    PRESCALER_DIV32 = $28;
    PRESCALER_DIV64 = $30;
    PRESCALER_DIV128 = $38;
    PRESCALER_DIV256 = $40;
    PRESCALER_DIV512 = $48;
    PRESCALER_DIV1024 = $50;
    PRESCALER_DIV2048 = $58;
    PRESCALER_DIV4096 = $60;
    PRESCALER_DIV8192 = $68;
    PRESCALER_DIV16384 = $70;
    PRESCALER_DIV32768 = $78;
    // Run In Standby
    RUNSTDBYbm = $80;
    // CTRLA Synchronization Busy Flag
    CTRLABUSYbm = $01;
    // Count Synchronization Busy Flag
    CNTBUSYbm = $02;
    // Period Synchronization Busy Flag
    PERBUSYbm = $04;
    // Comparator Synchronization Busy Flag
    CMPBUSYbm = $08;
    // Overflow Interrupt enable
    OVFbm = $01;
    // Compare Match Interrupt enable
    CMPbm = $02;
    // Run in debug
    DBGRUNbm = $01;
    // Error Correction Value
    ERROR0bm = $01;
    ERROR1bm = $02;
    ERROR2bm = $04;
    ERROR3bm = $08;
    ERROR4bm = $10;
    ERROR5bm = $20;
    ERROR6bm = $40;
    // Error Correction Sign Bit
    SIGNbm = $80;
    // RTC_CLKSEL
    CLKSELmask = $03;
    CLKSEL_OSC32K = $00;
    CLKSEL_OSC1K = $01;
    CLKSEL_XOSC32K = $02;
    CLKSEL_EXTCLK = $03;
    // Enable
    PITENbm = $01;
    // RTC_PERIOD
    PERIODmask = $78;
    PERIOD_OFF = $00;
    PERIOD_CYC4 = $08;
    PERIOD_CYC8 = $10;
    PERIOD_CYC16 = $18;
    PERIOD_CYC32 = $20;
    PERIOD_CYC64 = $28;
    PERIOD_CYC128 = $30;
    PERIOD_CYC256 = $38;
    PERIOD_CYC512 = $40;
    PERIOD_CYC1024 = $48;
    PERIOD_CYC2048 = $50;
    PERIOD_CYC4096 = $58;
    PERIOD_CYC8192 = $60;
    PERIOD_CYC16384 = $68;
    PERIOD_CYC32768 = $70;
    // CTRLA Synchronization Busy Flag
    CTRLBUSYbm = $01;
    // Periodic Interrupt
    PIbm = $01;
    // RTC_EVGEN0SEL
    EVGEN0SELmask = $0F;
    EVGEN0SEL_OFF = $00;
    EVGEN0SEL_DIV4 = $01;
    EVGEN0SEL_DIV8 = $02;
    EVGEN0SEL_DIV16 = $03;
    EVGEN0SEL_DIV32 = $04;
    EVGEN0SEL_DIV64 = $05;
    EVGEN0SEL_DIV128 = $06;
    EVGEN0SEL_DIV256 = $07;
    EVGEN0SEL_DIV512 = $08;
    EVGEN0SEL_DIV1024 = $09;
    EVGEN0SEL_DIV2048 = $0A;
    EVGEN0SEL_DIV4096 = $0B;
    EVGEN0SEL_DIV8192 = $0C;
    EVGEN0SEL_DIV16384 = $0D;
    EVGEN0SEL_DIV32768 = $0E;
    // RTC_EVGEN1SEL
    EVGEN1SELmask = $F0;
    EVGEN1SEL_OFF = $00;
    EVGEN1SEL_DIV4 = $10;
    EVGEN1SEL_DIV8 = $20;
    EVGEN1SEL_DIV16 = $30;
    EVGEN1SEL_DIV32 = $40;
    EVGEN1SEL_DIV64 = $50;
    EVGEN1SEL_DIV128 = $60;
    EVGEN1SEL_DIV256 = $70;
    EVGEN1SEL_DIV512 = $80;
    EVGEN1SEL_DIV1024 = $90;
    EVGEN1SEL_DIV2048 = $A0;
    EVGEN1SEL_DIV4096 = $B0;
    EVGEN1SEL_DIV8192 = $C0;
    EVGEN1SEL_DIV16384 = $D0;
    EVGEN1SEL_DIV32768 = $E0;
  end;

  TSIGROW = object //Signature row
    DEVICEID0: byte;  //Device ID Byte 0
    DEVICEID1: byte;  //Device ID Byte 1
    DEVICEID2: byte;  //Device ID Byte 2
    Reserved3: byte;
    TEMPSENSE0: word;  //Temperature Calibration 0
    TEMPSENSE1: word;  //Temperature Calibration 1
    Reserved8: byte;
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    SERNUM0: byte;  //Serial Number Byte 0
    SERNUM1: byte;  //Serial Number Byte 1
    SERNUM2: byte;  //Serial Number Byte 2
    SERNUM3: byte;  //Serial Number Byte 3
    SERNUM4: byte;  //Serial Number Byte 4
    SERNUM5: byte;  //Serial Number Byte 5
    SERNUM6: byte;  //Serial Number Byte 6
    SERNUM7: byte;  //Serial Number Byte 7
    SERNUM8: byte;  //Serial Number Byte 8
    SERNUM9: byte;  //Serial Number Byte 9
    SERNUM10: byte;  //Serial Number Byte 10
    SERNUM11: byte;  //Serial Number Byte 11
    SERNUM12: byte;  //Serial Number Byte 12
    SERNUM13: byte;  //Serial Number Byte 13
    SERNUM14: byte;  //Serial Number Byte 14
    SERNUM15: byte;  //Serial Number Byte 15
  end;

  TSLPCTRL = object //Sleep Controller
    CTRLA: byte;  //Control A
  const
    // Sleep enable
    SENbm = $01;
    // SLPCTRL_SMODE
    SMODEmask = $06;
    SMODE_IDLE = $00;
    SMODE_STDBY = $02;
    SMODE_PDOWN = $04;
  end;

  TSPI = object //Serial Peripheral Interface
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    DATA: byte;  //Data
  const
    // Enable Module
    ENABLEbm = $01;
    // SPI_PRESC
    PRESCmask = $06;
    PRESC_DIV4 = $00;
    PRESC_DIV16 = $02;
    PRESC_DIV64 = $04;
    PRESC_DIV128 = $06;
    // Enable Double Speed
    CLK2Xbm = $10;
    // Host Operation Enable
    MASTERbm = $20;
    // Data Order Setting
    DORDbm = $40;
    // SPI_MODE
    MODEmask = $03;
    MODE_0 = $00;
    MODE_1 = $01;
    MODE_2 = $02;
    MODE_3 = $03;
    // SPI Select Disable
    SSDbm = $04;
    // Buffer Mode Wait for Receive
    BUFWRbm = $40;
    // Buffer Mode Enable
    BUFENbm = $80;
    // Interrupt Enable
    IEbm = $01;
    // SPI Select Trigger Interrupt Enable
    SSIEbm = $10;
    // Data Register Empty Interrupt Enable
    DREIEbm = $20;
    // Transfer Complete Interrupt Enable
    TXCIEbm = $40;
    // Receive Complete Interrupt Enable
    RXCIEbm = $80;
  end;

  TSYSCFG = object //System Configuration Registers
    Reserved0: byte;
    REVID: byte;  //Revision ID
  const
    // Minor Revision
    MINOR0bm = $01;
    MINOR1bm = $02;
    MINOR2bm = $04;
    MINOR3bm = $08;
    // Major Revision
    MAJOR0bm = $10;
    MAJOR1bm = $20;
    MAJOR2bm = $40;
    MAJOR3bm = $80;
  end;

  TTCB = object //16-bit Timer/Counter Type B
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    Reserved3: byte;
    EVCTRL: byte;  //Event Control
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    STATUS: byte;  //Status
    DBGCTRL: byte;  //Debug Control
    TEMP: byte;  //Temporary Value
    CNT: word;  //Count
    CCMP: word;  //Compare or Capture
  const
    // Enable
    ENABLEbm = $01;
    // TCB_CLKSEL
    CLKSELmask = $0E;
    CLKSEL_DIV1 = $00;
    CLKSEL_DIV2 = $02;
    CLKSEL_TCE0 = $04;
    CLKSEL_EVENT = $0E;
    // Synchronize Update
    SYNCUPDbm = $10;
    // Cascade two timers
    CASCADEbm = $20;
    // Run Standby
    RUNSTDBYbm = $40;
    // TCB_CNTMODE
    CNTMODEmask = $07;
    CNTMODE_INT = $00;
    CNTMODE_TIMEOUT = $01;
    CNTMODE_CAPT = $02;
    CNTMODE_FRQ = $03;
    CNTMODE_PW = $04;
    CNTMODE_FRQPW = $05;
    CNTMODE_SINGLE = $06;
    CNTMODE_PWM8 = $07;
    // Pin Output Enable
    CCMPENbm = $10;
    // Pin Initial State
    CCMPINITbm = $20;
    // Asynchronous Enable
    ASYNCbm = $40;
    // TCB_EVGEN
    EVGENmask = $80;
    EVGEN_PULSE = $00;
    EVGEN_WAVEFORM = $80;
    // TCB_CNTSIZE
    CNTSIZEmask = $07;
    CNTSIZE_16BITS = $00;
    CNTSIZE_15BITS = $01;
    CNTSIZE_14BITS = $02;
    CNTSIZE_13BITS = $03;
    CNTSIZE_12BITS = $04;
    CNTSIZE_11BITS = $05;
    CNTSIZE_10BITS = $06;
    CNTSIZE_9BITS = $07;
    // Event Input Enable
    CAPTEIbm = $01;
    // Event Edge
    EDGEbm = $10;
    // Input Capture Noise Cancellation Filter
    FILTERbm = $40;
    // Capture or Timeout
    CAPTbm = $01;
    // Overflow
    OVFbm = $02;
    // Run
    RUNbm = $01;
    // Debug Run
    DBGRUNbm = $01;
  end;

  TTCE = object //16-bit Timer/Counter Type E
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    CTRLD: byte;  //Control D
    CTRLECLR: byte;  //Control E Clear
    CTRLESET: byte;  //Control E Set
    CTRLFCLR: byte;  //Control F Clear
    CTRLFSET: byte;  //Control F Set
    EVGENCTRL: byte;  //Event Generation Control
    EVCTRL: byte;  //Event Control
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    Reserved12: byte;
    Reserved13: byte;
    DBGCTRL: byte;  //Debug Control
    TEMP: byte;  //Temporary data for 16-bit Access
    Reserved16: byte;
    Reserved17: byte;
    Reserved18: byte;
    Reserved19: byte;
    Reserved20: byte;
    Reserved21: byte;
    Reserved22: byte;
    Reserved23: byte;
    Reserved24: byte;
    Reserved25: byte;
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    Reserved30: byte;
    Reserved31: byte;
    CNT: word;  //Count
    AMP: word;  //Amplitude
    OFFSET: word;  //Offset
    PER: word;  //Period
    CMP0: word;  //Compare 0
    CMP1: word;  //Compare 1
    CMP2: word;  //Compare 2
    CMP3: word;  //Compare 3
    Reserved48: byte;
    Reserved49: byte;
    Reserved50: byte;
    Reserved51: byte;
    Reserved52: byte;
    Reserved53: byte;
    PERBUF: word;  //Period Buffer
    CMP0BUF: word;  //Compare 0 Buffer
    CMP1BUF: word;  //Compare 1 Buffer
    CMP2BUF: word;  //Compare 2 Buffer
    CMP3BUF: word;  //Compare 3 Buffer
  const
    // Module Enable
    ENABLEbm = $01;
    // TCE_CLKSEL
    CLKSELmask = $0E;
    CLKSEL_DIV1 = $00;
    CLKSEL_DIV2 = $02;
    CLKSEL_DIV4 = $04;
    CLKSEL_DIV8 = $06;
    CLKSEL_DIV16 = $08;
    CLKSEL_DIV64 = $0A;
    CLKSEL_DIV256 = $0C;
    CLKSEL_DIV1024 = $0E;
    // Run in Standby
    RUNSTDBYbm = $80;
    // TCE_WGMODE
    WGMODEmask = $07;
    WGMODE_NORMAL = $00;
    WGMODE_FRQ = $01;
    WGMODE_SINGLESLOPE = $03;
    WGMODE_DSTOP = $05;
    WGMODE_DSBOTH = $06;
    WGMODE_DSBOTTOM = $07;
    // Auto Lock Update
    ALUPDbm = $08;
    // Compare 0 Enable
    CMP0ENbm = $10;
    // Compare 1 Enable
    CMP1ENbm = $20;
    // Compare 2 Enable
    CMP2ENbm = $40;
    // Compare 3 Enable
    CMP3ENbm = $80;
    // Compare 0 Waveform Output Value
    CMP0OVbm = $01;
    // Compare 1 Waveform Output Value
    CMP1OVbm = $02;
    // Compare 2 Waveform Output Value
    CMP2OVbm = $04;
    // Compare 3 Waveform Output Value
    CMP3OVbm = $08;
    // Compare 0 Polarity
    CMP0POLbm = $10;
    // Compare 1 Polarity
    CMP1POLbm = $20;
    // Compare 2 Polarity
    CMP2POLbm = $40;
    // Compare 3 Polarity
    CMP3POLbm = $80;
    // TCE_SCALE
    SCALEmask = $04;
    SCALE_NORMAL = $00;
    SCALE_FRACTIONAL = $04;
    // Amplitude Control Enable
    AMPENbm = $08;
    // TCE_SCALEMODE
    SCALEMODEmask = $30;
    SCALEMODE_CENTER = $00;
    SCALEMODE_BOTTOM = $10;
    SCALEMODE_TOP = $20;
    SCALEMODE_TOPBOTTOM = $30;
    // TCE_HREN
    HRENmask = $C0;
    HREN_OFF = $00;
    HREN_4X = $40;
    HREN_8X = $80;
    // Direction
    DIRbm = $01;
    // Lock Update
    LUPDbm = $02;
    // TCE_CMD
    CMDmask = $0C;
    CMD_NONE = $00;
    CMD_UPDATE = $04;
    CMD_RESTART = $08;
    CMD_RESET = $0C;
    // Period Buffer Valid
    PERBVbm = $01;
    // Compare 0 Buffer Valid
    CMP0BVbm = $02;
    // Compare 1 Buffer Valid
    CMP1BVbm = $04;
    // Compare 2 Buffer Valid
    CMP2BVbm = $08;
    // Compare 3 Buffer Valid
    CMP3BVbm = $10;
    // CMP0EV
    CMP0EVmask = $10;
    CMP0EVPULSE = $00;
    CMP0EVWAVEFORM = $10;
    // CMP1EV
    CMP1EVmask = $20;
    CMP1EVPULSE = $00;
    CMP1EVWAVEFORM = $20;
    // CMP2EV
    CMP2EVmask = $40;
    CMP2EVPULSE = $00;
    CMP2EVWAVEFORM = $40;
    // CMP3EV
    CMP3EVmask = $80;
    CMP3EVPULSE = $00;
    CMP3EVWAVEFORM = $80;
    // Count on Event Input A
    CNTAEIbm = $01;
    // TCE_EVACTA
    EVACTAmask = $0E;
    EVACTA_CNT_POSEDGE = $00;
    EVACTA_CNT_ANYEDGE = $02;
    EVACTA_CNT_HIGHLVL = $04;
    EVACTA_UPDOWN = $06;
    // Count on Event Input B
    CNTBEIbm = $10;
    // TCE_EVACTB
    EVACTBmask = $E0;
    EVACTB_NONE = $00;
    EVACTB_UPDOWN = $60;
    EVACTB_RESTART_POSEDGE = $80;
    EVACTB_RESTART_ANYEDGE = $A0;
    EVACTB_RESTART_HIGHLVL = $C0;
    // Overflow Interrupt Enable
    OVFbm = $01;
    // Compare 0 Interrupt Enable
    CMP0bm = $10;
    // Compare 1 Interrupt Enable
    CMP1bm = $20;
    // Compare 2 Interrupt Enable
    CMP2bm = $40;
    // Compare 3 Interrupt Enable
    CMP3bm = $80;
    // Debug Run
    DBGRUNbm = $01;
  end;

  TTCF = object //24-bit Timer/Counter for frequency generation
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    CTRLD: byte;  //Control D
    EVCTRL: byte;  //Event Control
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    STATUS: byte;  //Status
    Reserved8: byte;
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    DBGCTRL: byte;  //Debug Control
    Reserved14: byte;
    Reserved15: byte;
    CNT: dword;  //Count
    CMP: dword;  //Compare
  const
    // Enable
    ENABLEbm = $01;
    // TCF_PRESC
    PRESCmask = $0E;
    PRESC_DIV1 = $00;
    PRESC_DIV2 = $02;
    PRESC_DIV4 = $04;
    PRESC_DIV8 = $06;
    PRESC_DIV16 = $08;
    PRESC_DIV32 = $0A;
    PRESC_DIV64 = $0C;
    PRESC_DIV128 = $0E;
    // Run Standby
    RUNSTDBYbm = $80;
    // TCF_WGMODE
    WGMODEmask = $07;
    WGMODE_FRQ = $00;
    WGMODE_NCOPF = $01;
    WGMODE_NCOFDC = $02;
    WGMODE_PWM8 = $07;
    // TCF_CLKSEL
    CLKSELmask = $38;
    CLKSEL_CLKPER = $00;
    CLKSEL_EVENT = $08;
    CLKSEL_OSCHF = $10;
    CLKSEL_OSC32K = $18;
    CLKSEL_PLL = $28;
    // CMP0EV
    CMP0EVmask = $40;
    CMP0EVPULSE = $00;
    CMP0EVWAVEFORM = $40;
    // CMP1EV
    CMP1EVmask = $80;
    CMP1EVPULSE = $00;
    CMP1EVWAVEFORM = $80;
    // Waveform Output 0 Enable
    WO0ENbm = $01;
    // Waveform Output 1 Enable
    WO1ENbm = $02;
    // WO0POL
    WO0POLmask = $04;
    WO0POLNORMAL = $00;
    WO0POLINVERSE = $04;
    // WO1POL
    WO1POLmask = $08;
    WO1POLNORMAL = $00;
    WO1POLINVERSE = $08;
    // TCF_WGPULSE
    WGPULSEmask = $70;
    WGPULSE_CLK1 = $00;
    WGPULSE_CLK2 = $10;
    WGPULSE_CLK4 = $20;
    WGPULSE_CLK8 = $30;
    WGPULSE_CLK16 = $40;
    WGPULSE_CLK32 = $50;
    WGPULSE_CLK64 = $60;
    WGPULSE_CLK128 = $70;
    // TCF_CMD
    CMDmask = $03;
    CMD_NONE = $00;
    CMD_UPDATE = $01;
    CMD_RESTART = $02;
    // Event A Input Enable
    CNTAEIbm = $01;
    // TCF_EVACTA
    EVACTAmask = $06;
    EVACTA_RESTART = $00;
    EVACTA_BLANK = $02;
    // Event A Filter
    FILTERAbm = $08;
    // Overflow
    OVFbm = $01;
    // Compare 0 Interrupt Enable
    CMP0bm = $02;
    // Compare 1 Interrupt Enable
    CMP1bm = $04;
    // Control A Synchronization Busy
    CTRLABUSYbm = $02;
    // Control B Synchronization Busy
    CTRLCBUSYbm = $04;
    // Control D Synchronization Busy
    CTRLDBUSYbm = $08;
    // Counter Synchronization Busy
    CNTBUSYbm = $10;
    // Period Synchronization Busy
    PERBUSYbm = $20;
    // Compare 0 Synchronization Busy
    CMP0BUSYbm = $40;
    // Compare 1 Synchronization Busy
    CMP1BUSYbm = $80;
    // Debug Run
    DBGRUNbm = $01;
  end;

  TTWI = object //Two-Wire Interface
    CTRLA: byte;  //Control A
    DUALCTRL: byte;  //Dual Mode Control
    DBGCTRL: byte;  //Debug Control
    MCTRLA: byte;  //Host Control A
    MCTRLB: byte;  //Host Control B
    MSTATUS: byte;  //Host STATUS
    MBAUD: byte;  //Host Baud Rate
    MADDR: byte;  //Host Address
    MDATA: byte;  //Host Data
    SCTRLA: byte;  //Client Control A
    SCTRLB: byte;  //Client Control B
    SSTATUS: byte;  //Client Status
    SADDR: byte;  //Client Address
    SDATA: byte;  //Client Data
    SADDRMASK: byte;  //Client Address Mask
  const
    // TWI_FMEN
    FMENmask = $01;
    FMEN_OFF = $00;
    FMEN_ON = $01;
    // TWI_FMPEN
    FMPENmask = $02;
    FMPEN_OFF = $00;
    FMPEN_ON = $02;
    // TWI_SDAHOLD
    SDAHOLDmask = $0C;
    SDAHOLD_OFF = $00;
    SDAHOLD_50NS = $04;
    SDAHOLD_300NS = $08;
    SDAHOLD_500NS = $0C;
    // TWI_SDASETUP
    SDASETUPmask = $10;
    SDASETUP_4CYC = $00;
    SDASETUP_8CYC = $10;
    // TWI_INPUTLVL
    INPUTLVLmask = $40;
    INPUTLVL_I2C = $00;
    INPUTLVL_SMBUS = $40;
    // Enable
    ENABLEbm = $01;
    // TWI_DBGRUN
    DBGRUNmask = $01;
    DBGRUN_HALT = $00;
    DBGRUN_RUN = $01;
    // Smart Mode Enable
    SMENbm = $02;
    // TWI_TIMEOUT
    TIMEOUTmask = $0C;
    TIMEOUT_DISABLED = $00;
    TIMEOUT_50US = $04;
    TIMEOUT_100US = $08;
    TIMEOUT_200US = $0C;
    // Quick Command Enable
    QCENbm = $10;
    // Write Interrupt Enable
    WIENbm = $40;
    // Read Interrupt Enable
    RIENbm = $80;
    // TWI_MCMD
    MCMDmask = $03;
    MCMD_NOACT = $00;
    MCMD_REPSTART = $01;
    MCMD_RECVTRANS = $02;
    MCMD_STOP = $03;
    // TWI_ACKACT
    ACKACTmask = $04;
    ACKACT_ACK = $00;
    ACKACT_NACK = $04;
    // Flush
    FLUSHbm = $08;
    // TWI_BUSSTATE
    BUSSTATEmask = $03;
    BUSSTATE_UNKNOWN = $00;
    BUSSTATE_IDLE = $01;
    BUSSTATE_OWNER = $02;
    BUSSTATE_BUSY = $03;
    // Bus Error
    BUSERRbm = $04;
    // Arbitration Lost
    ARBLOSTbm = $08;
    // Received Acknowledge
    RXACKbm = $10;
    // Clock Hold
    CLKHOLDbm = $20;
    // Write Interrupt Flag
    WIFbm = $40;
    // Read Interrupt Flag
    RIFbm = $80;
    // Address Recognition Mode
    PMENbm = $04;
    // Stop Interrupt Enable
    PIENbm = $20;
    // Address or Stop Interrupt Enable
    APIENbm = $40;
    // Data Interrupt Enable
    DIENbm = $80;
    // TWI_SCMD
    SCMDmask = $03;
    SCMD_NOACT = $00;
    SCMD_COMPTRANS = $02;
    SCMD_RESPONSE = $03;
    // TWI_AP
    APmask = $01;
    AP_STOP = $00;
    AP_ADR = $01;
    // Read/Write Direction
    DIRbm = $02;
    // Collision
    COLLbm = $08;
    // Address or Stop Interrupt Flag
    APIFbm = $40;
    // Data Interrupt Flag
    DIFbm = $80;
    // Address Mask Enable
    ADDRENbm = $01;
    // Address Mask
    ADDRMASK0bm = $02;
    ADDRMASK1bm = $04;
    ADDRMASK2bm = $08;
    ADDRMASK3bm = $10;
    ADDRMASK4bm = $20;
    ADDRMASK5bm = $40;
    ADDRMASK6bm = $80;
  end;

  TUSART = object //Universal Synchronous and Asynchronous Receiver and Transmitter
    RXDATAL: byte;  //Receive Data Low Byte
    RXDATAH: byte;  //Receive Data High Byte
    TXDATAL: byte;  //Transmit Data Low Byte
    TXDATAH: byte;  //Transmit Data High Byte
    STATUS: byte;  //Status
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    BAUD: word;  //Baud Rate
    CTRLD: byte;  //Control D
    DBGCTRL: byte;  //Debug Control
    EVCTRL: byte;  //Event Control
    TXPLCTRL: byte;  //IRCOM Transmitter Pulse Length Control
    RXPLCTRL: byte;  //IRCOM Receiver Pulse Length Control
  const
    // Receiver Data Register
    DATA8bm = $01;
    // Parity Error
    PERRbm = $02;
    // Frame Error
    FERRbm = $04;
    // Buffer Overflow
    BUFOVFbm = $40;
    // Receive Complete Interrupt Flag
    RXCIFbm = $80;
    // Wait For Break
    WFBbm = $01;
    // Break Detected Flag
    BDFbm = $02;
    // Inconsistent Sync Field Interrupt Flag
    ISFIFbm = $08;
    // Receive Start Interrupt
    RXSIFbm = $10;
    // Data Register Empty Flag
    DREIFbm = $20;
    // Transmit Interrupt Flag
    TXCIFbm = $40;
    // USART_RS485
    RS485mask = $01;
    RS485_DISABLE = $00;
    RS485_ENABLE = $01;
    // Auto-baud Error Interrupt Enable
    ABEIEbm = $04;
    // Loop-back Mode Enable
    LBMEbm = $08;
    // Receiver Start Frame Interrupt Enable
    RXSIEbm = $10;
    // Data Register Empty Interrupt Enable
    DREIEbm = $20;
    // Transmit Complete Interrupt Enable
    TXCIEbm = $40;
    // Receive Complete Interrupt Enable
    RXCIEbm = $80;
    // Multi-processor Communication Mode
    MPCMbm = $01;
    // USART_RXMODE
    RXMODEmask = $06;
    RXMODE_NORMAL = $00;
    RXMODE_CLK2X = $02;
    RXMODE_GENAUTO = $04;
    RXMODE_LINAUTO = $06;
    // Open Drain Mode Enable
    ODMEbm = $08;
    // Start Frame Detection Enable
    SFDENbm = $10;
    // Transmitter Enable
    TXENbm = $40;
    // Reciever enable
    RXENbm = $80;
    // USART_ABW
    ABWmask = $C0;
    ABW_WDW0 = $00;
    ABW_WDW1 = $40;
    ABW_WDW2 = $80;
    ABW_WDW3 = $C0;
    // Debug Run
    DBGRUNbm = $01;
    // IrDA Event Input Enable
    IREIbm = $01;
    // Receiver Pulse Lenght
    RXPL0bm = $01;
    RXPL1bm = $02;
    RXPL2bm = $04;
    RXPL3bm = $08;
    RXPL4bm = $10;
    RXPL5bm = $20;
    RXPL6bm = $40;
  end;

  TUSERROW = object //User Row
    USERROW0: byte;  //User Row Byte 0
    USERROW1: byte;  //User Row Byte 1
    USERROW2: byte;  //User Row Byte 2
    USERROW3: byte;  //User Row Byte 3
    USERROW4: byte;  //User Row Byte 4
    USERROW5: byte;  //User Row Byte 5
    USERROW6: byte;  //User Row Byte 6
    USERROW7: byte;  //User Row Byte 7
    USERROW8: byte;  //User Row Byte 8
    USERROW9: byte;  //User Row Byte 9
    USERROW10: byte;  //User Row Byte 10
    USERROW11: byte;  //User Row Byte 11
    USERROW12: byte;  //User Row Byte 12
    USERROW13: byte;  //User Row Byte 13
    USERROW14: byte;  //User Row Byte 14
    USERROW15: byte;  //User Row Byte 15
    USERROW16: byte;  //User Row Byte 16
    USERROW17: byte;  //User Row Byte 17
    USERROW18: byte;  //User Row Byte 18
    USERROW19: byte;  //User Row Byte 19
    USERROW20: byte;  //User Row Byte 20
    USERROW21: byte;  //User Row Byte 21
    USERROW22: byte;  //User Row Byte 22
    USERROW23: byte;  //User Row Byte 23
    USERROW24: byte;  //User Row Byte 24
    USERROW25: byte;  //User Row Byte 25
    USERROW26: byte;  //User Row Byte 26
    USERROW27: byte;  //User Row Byte 27
    USERROW28: byte;  //User Row Byte 28
    USERROW29: byte;  //User Row Byte 29
    USERROW30: byte;  //User Row Byte 30
    USERROW31: byte;  //User Row Byte 31
    USERROW32: byte;  //User Row Byte 32
    USERROW33: byte;  //User Row Byte 33
    USERROW34: byte;  //User Row Byte 34
    USERROW35: byte;  //User Row Byte 35
    USERROW36: byte;  //User Row Byte 36
    USERROW37: byte;  //User Row Byte 37
    USERROW38: byte;  //User Row Byte 38
    USERROW39: byte;  //User Row Byte 39
    USERROW40: byte;  //User Row Byte 40
    USERROW41: byte;  //User Row Byte 41
    USERROW42: byte;  //User Row Byte 42
    USERROW43: byte;  //User Row Byte 43
    USERROW44: byte;  //User Row Byte 44
    USERROW45: byte;  //User Row Byte 45
    USERROW46: byte;  //User Row Byte 46
    USERROW47: byte;  //User Row Byte 47
    USERROW48: byte;  //User Row Byte 48
    USERROW49: byte;  //User Row Byte 49
    USERROW50: byte;  //User Row Byte 50
    USERROW51: byte;  //User Row Byte 51
    USERROW52: byte;  //User Row Byte 52
    USERROW53: byte;  //User Row Byte 53
    USERROW54: byte;  //User Row Byte 54
    USERROW55: byte;  //User Row Byte 55
    USERROW56: byte;  //User Row Byte 56
    USERROW57: byte;  //User Row Byte 57
    USERROW58: byte;  //User Row Byte 58
    USERROW59: byte;  //User Row Byte 59
    USERROW60: byte;  //User Row Byte 60
    USERROW61: byte;  //User Row Byte 61
    USERROW62: byte;  //User Row Byte 62
    USERROW63: byte;  //User Row Byte 63
  end;

  TVPORT = object //Virtual Ports
    DIR: byte;  //Data Direction
    OUT_: byte;  //Output Value
    IN_: byte;  //Input Value
    INTFLAGS: byte;  //Interrupt Flags
  end;

  TVREF = object //Voltage reference
    Reserved0: byte;
    Reserved1: byte;
    DAC0REF: byte;  //DAC0 Reference
    Reserved3: byte;
    ACREF: byte;  //AC Reference
  const
    // VREF_REFSEL
    REFSELmask = $07;
    REFSEL_1V024 = $00;
    REFSEL_2V048 = $01;
    REFSEL_4V096 = $02;
    REFSEL_2V500 = $03;
    REFSEL_VDD = $05;
    REFSEL_VREFA = $06;
    // Always on
    ALWAYSONbm = $80;
  end;

  TWDT = object //Watch-Dog Timer
    CTRLA: byte;  //Control A
    STATUS: byte;  //Status
  const
    // WDT_PERIOD
    PERIODmask = $0F;
    PERIOD_OFF = $00;
    PERIOD_8CLK = $01;
    PERIOD_16CLK = $02;
    PERIOD_32CLK = $03;
    PERIOD_64CLK = $04;
    PERIOD_128CLK = $05;
    PERIOD_256CLK = $06;
    PERIOD_512CLK = $07;
    PERIOD_1KCLK = $08;
    PERIOD_2KCLK = $09;
    PERIOD_4KCLK = $0A;
    PERIOD_8KCLK = $0B;
    // WDT_WINDOW
    WINDOWmask = $F0;
    WINDOW_OFF = $00;
    WINDOW_8CLK = $10;
    WINDOW_16CLK = $20;
    WINDOW_32CLK = $30;
    WINDOW_64CLK = $40;
    WINDOW_128CLK = $50;
    WINDOW_256CLK = $60;
    WINDOW_512CLK = $70;
    WINDOW_1KCLK = $80;
    WINDOW_2KCLK = $90;
    WINDOW_4KCLK = $A0;
    WINDOW_8KCLK = $B0;
    // Synchronization busy
    SYNCBUSYbm = $01;
    // Lock enable
    LOCKbm = $80;
  end;

  TWEX = object //Waveform Extension
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    Reserved3: byte;
    EVCTRLA: byte;  //Event Control A
    EVCTRLB: byte;  //Event Control B
    EVCTRLC: byte;  //Event Control C
    BUFCTRL: byte;  //Buffer Valid Control
    BLANKCTRL: byte;  //Blanking Control
    BLANKTIME: byte;  //Blanking Time
    FAULTCTRL: byte;  //Fault Control
    FAULTDRV: byte;  //Fault Drive
    FAULTOUT: byte;  //Fault Output
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    STATUS: byte;  //Status
    DTLS: byte;  //Dead-time Low Side
    DTHS: byte;  //Dead-time High Side
    DTBOTH: byte;  //Dead-time Both Sides
    SWAP: byte;  //DTI Swap
    PGMOVR: byte;  //Pattern Generation Override
    PGMOUT: byte;  //Pattern Generation Output
    Reserved22: byte;
    OUTOVEN: byte;  //Output Override Enable
    DTLSBUF: byte;  //Dead-time Low Side Buffer
    DTHSBUF: byte;  //Dead-time High Side Buffer
    DTBOTHBUF: byte;  //Dead-time Both Sides Buffer
    SWAPBUF: byte;  //DTI Swap Buffer
    PGMOVRBUF: byte;  //Pattern Generation Override Buffer
    PGMOUTBUF: byte;  //Pattern Generation Output Buffer
  const
    // Dead-Time Insertion CMP0 Enable
    DTI0ENbm = $01;
    // Dead-Time Insertion CMP1 Enable
    DTI1ENbm = $02;
    // Dead-Time Insertion CMP2 Enable
    DTI2ENbm = $04;
    // Dead-Time Insertion CMP3 Enable
    DTI3ENbm = $08;
    // WEX_INMX
    INMXmask = $70;
    INMX_DIRECT = $00;
    INMX_CWCMA = $20;
    INMX_CWCMB = $30;
    // Pattern Generation Mode
    PGMbm = $80;
    // WEX_UPDSRC
    UPDSRCmask = $03;
    UPDSRC_TCPWM0 = $00;
    UPDSRC_SW = $03;
    // WEX_CMD
    CMDmask = $07;
    CMD_NONE = $00;
    CMD_UPDATE = $01;
    CMD_FAULTSET = $02;
    CMD_FAULTCLR = $03;
    CMD_BLANKSET = $04;
    CMD_BLANKCLR = $05;
    // Fault Event Input Enable
    FAULTEIbm = $01;
    // Fault Event Blanking Enable
    BLANKbm = $02;
    // WEX_FILTER
    FILTERmask = $1C;
    FILTER_ZERO = $00;
    FILTER_SAMPLE1 = $04;
    FILTER_SAMPLE2 = $08;
    FILTER_SAMPLE3 = $0C;
    FILTER_SAMPLE4 = $10;
    FILTER_SAMPLE5 = $14;
    FILTER_SAMPLE6 = $18;
    FILTER_SAMPLE7 = $1C;
    // Dead-time Low Side Buffer Valid
    DTLSBVbm = $01;
    // Dead-time High Side Buffer Valid
    DTHSBVbm = $02;
    // Swap Buffer Valid
    SWAPBVbm = $04;
    // PGM Override Buffer Valid
    PGMOVRBVbm = $08;
    // PGM Output Value Buffer Valid
    PGMOUTBVbm = $10;
    // WEX_BLANKTRIG
    BLANKTRIGmask = $1F;
    BLANKTRIG_NONE = $00;
    BLANKTRIG_TCE0UPD = $04;
    BLANKTRIG_TCE0CMP0 = $08;
    BLANKTRIG_TCE0CMP1 = $0C;
    BLANKTRIG_TCE0CMP2 = $10;
    BLANKTRIG_TCE0CMP3 = $14;
    // WEX_BLANKPRESC
    BLANKPRESCmask = $60;
    BLANKPRESC_DIV1 = $00;
    BLANKPRESC_DIV4 = $20;
    BLANKPRESC_DIV16 = $40;
    BLANKPRESC_DIV64 = $60;
    // WEX_FDACT
    FDACTmask = $03;
    FDACT_NONE = $00;
    FDACT_LOW = $01;
    FDACT_CUSTOM = $03;
    // WEX_FDMODE
    FDMODEmask = $04;
    FDMODE_LATCHED = $00;
    FDMODE_CBC = $04;
    // WEX_FDDBD
    FDDBDmask = $80;
    FDDBD_FAULT = $00;
    FDDBD_IGNORE = $80;
    // Fault Drive Enable Bit 0
    FAULTDRV0bm = $01;
    // Fault Drive Enable Bit 1
    FAULTDRV1bm = $02;
    // Fault Drive Enable Bit 2
    FAULTDRV2bm = $04;
    // Fault Drive Enable Bit 3
    FAULTDRV3bm = $08;
    // Fault Drive Enable Bit 4
    FAULTDRV4bm = $10;
    // Fault Drive Enable Bit 5
    FAULTDRV5bm = $20;
    // Fault Drive Enable Bit 6
    FAULTDRV6bm = $40;
    // Fault Drive Enable Bit 7
    FAULTDRV7bm = $80;
    // Fault Output Value Bit 0
    FAULTOUT0bm = $01;
    // Fault Output Value Bit 1
    FAULTOUT1bm = $02;
    // Fault Output Value Bit 2
    FAULTOUT2bm = $04;
    // Fault Output Value Bit 3
    FAULTOUT3bm = $08;
    // Fault Output Value Bit 4
    FAULTOUT4bm = $10;
    // Fault Output Value Bit 5
    FAULTOUT5bm = $20;
    // Fault Output Value Bit 6
    FAULTOUT6bm = $40;
    // Fault Output Value Bit 7
    FAULTOUT7bm = $80;
    // Fault Detection Interrupt Enable
    FAULTDETbm = $01;
    // Fault Detection Flag Event Input A
    FDFEVAbm = $04;
    // Fault Detection Flag Event Input B
    FDFEVBbm = $08;
    // Fault Detection Flag Event Input C
    FDFEVCbm = $10;
    // WEX_FDSTATE
    FDSTATEmask = $01;
    FDSTATE_NORMAL = $00;
    FDSTATE_FAULT = $01;
    // Fault Detection State Event A
    FDSEVAbm = $04;
    // Fault Detection State Event B
    FDSEVBbm = $08;
    // Fault Detection State Event C
    FDSEVCbm = $10;
    // WEX_BLANKSTATE
    BLANKSTATEmask = $80;
    BLANKSTATE_OFF = $00;
    BLANKSTATE_ON = $80;
    // Swap DTI Output Pair 0
    SWAP0bm = $01;
    // Swap DTI Output Pair 1
    SWAP1bm = $02;
    // Swap DTI Output Pair 2
    SWAP2bm = $04;
    // Swap DTI Output Pair 3
    SWAP3bm = $08;
    // Pattern Generation Override Enable Bit 0
    PGMOVR0bm = $01;
    // Pattern Generation Override Enable Bit 1
    PGMOVR1bm = $02;
    // Pattern Generation Override Enable Bit 2
    PGMOVR2bm = $04;
    // Pattern Generation Override Enable Bit 3
    PGMOVR3bm = $08;
    // Pattern Generation Override Enable Bit 4
    PGMOVR4bm = $10;
    // Pattern Generation Override Enable Bit 5
    PGMOVR5bm = $20;
    // Pattern Generation Override Enable Bit 6
    PGMOVR6bm = $40;
    // Pattern Generation Override Enable Bit 7
    PGMOVR7bm = $80;
    // Pattern Generation Output Value Bit 0
    PGMOUT0bm = $01;
    // Pattern Generation Output Value Bit 1
    PGMOUT1bm = $02;
    // Pattern Generation Output Value Bit 2
    PGMOUT2bm = $04;
    // Pattern Generation Output Value Bit 3
    PGMOUT3bm = $08;
    // Pattern Generation Output Value Bit 4
    PGMOUT4bm = $10;
    // Pattern Generation Output Value Bit 5
    PGMOUT5bm = $20;
    // Pattern Generation Output Value Bit 6
    PGMOUT6bm = $40;
    // Pattern Generation Output Value Bit 7
    PGMOUT7bm = $80;
    // Output Override Enable Bit 0
    OUTOVEN0bm = $01;
    // Output Override Enable Bit 1
    OUTOVEN1bm = $02;
    // Output Override Enable Bit 2
    OUTOVEN2bm = $04;
    // Output Override Enable Bit 3
    OUTOVEN3bm = $08;
    // Output Override Enable Bit 4
    OUTOVEN4bm = $10;
    // Output Override Enable Bit 5
    OUTOVEN5bm = $20;
    // Output Override Enable Bit 6
    OUTOVEN6bm = $40;
    // Output Override Enable Bit 7
    OUTOVEN7bm = $80;
    // Swap DTI Output Pair 0 Buffer
    SWAPBUF0bm = $01;
    // Swap DTI Output Pair 1 Buffer
    SWAPBUF1bm = $02;
    // Swap DTI Output Pair 2 Buffer
    SWAPBUF2bm = $04;
    // Swap DTI Output Pair 3 Buffer
    SWAPBUF3bm = $08;
    // Pattern Generation Override Enable Buffer Bit 0
    PGMOVRBUF0bm = $01;
    // Pattern Generation Override Enable Buffer Bit 1
    PGMOVRBUF1bm = $02;
    // Pattern Generation Override Enable Buffer Bit 2
    PGMOVRBUF2bm = $04;
    // Pattern Generation Override Enable Buffer Bit 3
    PGMOVRBUF3bm = $08;
    // Pattern Generation Override Enable Buffer Bit 4
    PGMOVRBUF4bm = $10;
    // Pattern Generation Override Enable Buffer Bit 5
    PGMOVRBUF5bm = $20;
    // Pattern Generation Override Enable Buffer Bit 6
    PGMOVRBUF6bm = $40;
    // Pattern Generation Override Enable Buffer Bit 7
    PGMOVRBUF7bm = $80;
    // Pattern Generation Output Value Buffer Bit 0
    PGMOUTBUF0bm = $01;
    // Pattern Generation Output Value Buffer Bit 1
    PGMOUTBUF1bm = $02;
    // Pattern Generation Output Value Buffer Bit 2
    PGMOUTBUF2bm = $04;
    // Pattern Generation Output Value Buffer Bit 3
    PGMOUTBUF3bm = $08;
    // Pattern Generation Output Value Buffer Bit 4
    PGMOUTBUF4bm = $10;
    // Pattern Generation Output Value Buffer Bit 5
    PGMOUTBUF5bm = $20;
    // Pattern Generation Output Value Buffer Bit 6
    PGMOUTBUF6bm = $40;
    // Pattern Generation Output Value Buffer Bit 7
    PGMOUTBUF7bm = $80;
  end;


const
 Pin0idx = 0;  Pin0bm = 1;
 Pin1idx = 1;  Pin1bm = 2;
 Pin2idx = 2;  Pin2bm = 4;
 Pin3idx = 3;  Pin3bm = 8;
 Pin4idx = 4;  Pin4bm = 16;
 Pin5idx = 5;  Pin5bm = 32;
 Pin6idx = 6;  Pin6bm = 64;
 Pin7idx = 7;  Pin7bm = 128;

var
  VPORTA: TVPORT absolute $0000;
  VPORTC: TVPORT absolute $0008;
  VPORTD: TVPORT absolute $000C;
  VPORTF: TVPORT absolute $0014;
  GPR: TGPR absolute $001C;
  CPU: TCPU absolute $0030;
  RSTCTRL: TRSTCTRL absolute $0040;
  SLPCTRL: TSLPCTRL absolute $0050;
  CLKCTRL: TCLKCTRL absolute $0060;
  BOD: TBOD absolute $00A0;
  VREF: TVREF absolute $00B0;
  WDT: TWDT absolute $0100;
  CPUINT: TCPUINT absolute $0110;
  CRCSCAN: TCRCSCAN absolute $0120;
  RTC: TRTC absolute $0140;
  CCL: TCCL absolute $01C0;
  EVSYS: TEVSYS absolute $0200;
  PORTA: TPORT absolute $0400;
  PORTC: TPORT absolute $0440;
  PORTD: TPORT absolute $0460;
  PORTF: TPORT absolute $04A0;
  PORTMUX: TPORTMUX absolute $05E0;
  ADC0: TADC absolute $0600;
  AC0: TAC absolute $0680;
  AC1: TAC absolute $0688;
  USART0: TUSART absolute $0800;
  TWI0: TTWI absolute $0900;
  SPI0: TSPI absolute $0940;
  TCE0: TTCE absolute $0A00;
  TCB0: TTCB absolute $0B00;
  TCB1: TTCB absolute $0B10;
  TCF0: TTCF absolute $0C00;
  WEX0: TWEX absolute $0C80;
  SYSCFG: TSYSCFG absolute $0F00;
  NVMCTRL: TNVMCTRL absolute $1000;
  LOCK: TLOCK absolute $1040;
  FUSE: TFUSE absolute $1050;
  SIGROW: TSIGROW absolute $1080;
  BOOTROW: TBOOTROW absolute $1100;
  USERROW: TUSERROW absolute $1200;

implementation

{$i avrcommon.inc}

procedure CRCSCAN_NMI_ISR; external name 'CRCSCAN_NMI_ISR'; // Interrupt 1
procedure BOD_VLM_ISR; external name 'BOD_VLM_ISR'; // Interrupt 2
procedure RTC_CNT_ISR; external name 'RTC_CNT_ISR'; // Interrupt 3
procedure RTC_PIT_ISR; external name 'RTC_PIT_ISR'; // Interrupt 4
procedure CCL_CCL_ISR; external name 'CCL_CCL_ISR'; // Interrupt 5
procedure PORTA_PORT_ISR; external name 'PORTA_PORT_ISR'; // Interrupt 6
procedure WEX0_FAULTDET_ISR; external name 'WEX0_FAULTDET_ISR'; // Interrupt 7
//procedure WEX0_FDFEVA_ISR; external name 'WEX0_FDFEVA_ISR'; // Interrupt 7
//procedure WEX0_FDFEVB_ISR; external name 'WEX0_FDFEVB_ISR'; // Interrupt 7
//procedure WEX0_FDFEVC_ISR; external name 'WEX0_FDFEVC_ISR'; // Interrupt 7
procedure TCE0_OVF_ISR; external name 'TCE0_OVF_ISR'; // Interrupt 8
procedure TCE0_CMP0_ISR; external name 'TCE0_CMP0_ISR'; // Interrupt 9
procedure TCE0_CMP1_ISR; external name 'TCE0_CMP1_ISR'; // Interrupt 10
procedure TCE0_CMP2_ISR; external name 'TCE0_CMP2_ISR'; // Interrupt 11
procedure TCE0_CMP3_ISR; external name 'TCE0_CMP3_ISR'; // Interrupt 12
procedure TCB0_INT_ISR; external name 'TCB0_INT_ISR'; // Interrupt 13
procedure TCB1_INT_ISR; external name 'TCB1_INT_ISR'; // Interrupt 14
procedure TWI0_TWIS_ISR; external name 'TWI0_TWIS_ISR'; // Interrupt 15
procedure TWI0_TWIM_ISR; external name 'TWI0_TWIM_ISR'; // Interrupt 16
procedure SPI0_INT_ISR; external name 'SPI0_INT_ISR'; // Interrupt 17
procedure USART0_RXC_ISR; external name 'USART0_RXC_ISR'; // Interrupt 18
procedure USART0_DRE_ISR; external name 'USART0_DRE_ISR'; // Interrupt 19
procedure USART0_TXC_ISR; external name 'USART0_TXC_ISR'; // Interrupt 20
procedure PORTD_PORT_ISR; external name 'PORTD_PORT_ISR'; // Interrupt 21
procedure TCF0_INT_ISR; external name 'TCF0_INT_ISR'; // Interrupt 22
procedure AC0_AC_ISR; external name 'AC0_AC_ISR'; // Interrupt 23
procedure ADC0_ERROR_ISR; external name 'ADC0_ERROR_ISR'; // Interrupt 24
procedure ADC0_RESRDY_ISR; external name 'ADC0_RESRDY_ISR'; // Interrupt 25
procedure ADC0_SAMPRDY_ISR; external name 'ADC0_SAMPRDY_ISR'; // Interrupt 26
procedure AC1_AC_ISR; external name 'AC1_AC_ISR'; // Interrupt 27
procedure PORTC_PORT_ISR; external name 'PORTC_PORT_ISR'; // Interrupt 28
procedure PORTF_PORT_ISR; external name 'PORTF_PORT_ISR'; // Interrupt 29
procedure NVMCTRL_EEREADY_ISR; external name 'NVMCTRL_EEREADY_ISR'; // Interrupt 30
//procedure NVMCTRL_FLREADY_ISR; external name 'NVMCTRL_FLREADY_ISR'; // Interrupt 30
//procedure NVMCTRL_NVMREADY_ISR; external name 'NVMCTRL_NVMREADY_ISR'; // Interrupt 30

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
asm
  jmp __dtors_end
  jmp CRCSCAN_NMI_ISR
  jmp BOD_VLM_ISR
  jmp RTC_CNT_ISR
  jmp RTC_PIT_ISR
  jmp CCL_CCL_ISR
  jmp PORTA_PORT_ISR
  jmp WEX0_FAULTDET_ISR
//  jmp WEX0_FDFEVA_ISR
//  jmp WEX0_FDFEVB_ISR
//  jmp WEX0_FDFEVC_ISR
  jmp TCE0_OVF_ISR
  jmp TCE0_CMP0_ISR
  jmp TCE0_CMP1_ISR
  jmp TCE0_CMP2_ISR
  jmp TCE0_CMP3_ISR
  jmp TCB0_INT_ISR
  jmp TCB1_INT_ISR
  jmp TWI0_TWIS_ISR
  jmp TWI0_TWIM_ISR
  jmp SPI0_INT_ISR
  jmp USART0_RXC_ISR
  jmp USART0_DRE_ISR
  jmp USART0_TXC_ISR
  jmp PORTD_PORT_ISR
  jmp TCF0_INT_ISR
  jmp AC0_AC_ISR
  jmp ADC0_ERROR_ISR
  jmp ADC0_RESRDY_ISR
  jmp ADC0_SAMPRDY_ISR
  jmp AC1_AC_ISR
  jmp PORTC_PORT_ISR
  jmp PORTF_PORT_ISR
  jmp NVMCTRL_EEREADY_ISR
//  jmp NVMCTRL_FLREADY_ISR
//  jmp NVMCTRL_NVMREADY_ISR

  .weak CRCSCAN_NMI_ISR
  .weak BOD_VLM_ISR
  .weak RTC_CNT_ISR
  .weak RTC_PIT_ISR
  .weak CCL_CCL_ISR
  .weak PORTA_PORT_ISR
  .weak WEX0_FAULTDET_ISR
//  .weak WEX0_FDFEVA_ISR
//  .weak WEX0_FDFEVB_ISR
//  .weak WEX0_FDFEVC_ISR
  .weak TCE0_OVF_ISR
  .weak TCE0_CMP0_ISR
  .weak TCE0_CMP1_ISR
  .weak TCE0_CMP2_ISR
  .weak TCE0_CMP3_ISR
  .weak TCB0_INT_ISR
  .weak TCB1_INT_ISR
  .weak TWI0_TWIS_ISR
  .weak TWI0_TWIM_ISR
  .weak SPI0_INT_ISR
  .weak USART0_RXC_ISR
  .weak USART0_DRE_ISR
  .weak USART0_TXC_ISR
  .weak PORTD_PORT_ISR
  .weak TCF0_INT_ISR
  .weak AC0_AC_ISR
  .weak ADC0_ERROR_ISR
  .weak ADC0_RESRDY_ISR
  .weak ADC0_SAMPRDY_ISR
  .weak AC1_AC_ISR
  .weak PORTC_PORT_ISR
  .weak PORTF_PORT_ISR
  .weak NVMCTRL_EEREADY_ISR
//  .weak NVMCTRL_FLREADY_ISR
//  .weak NVMCTRL_NVMREADY_ISR

  .set CRCSCAN_NMI_ISR, Default_IRQ_handler
  .set BOD_VLM_ISR, Default_IRQ_handler
  .set RTC_CNT_ISR, Default_IRQ_handler
  .set RTC_PIT_ISR, Default_IRQ_handler
  .set CCL_CCL_ISR, Default_IRQ_handler
  .set PORTA_PORT_ISR, Default_IRQ_handler
  .set WEX0_FAULTDET_ISR, Default_IRQ_handler
//  .set WEX0_FDFEVA_ISR, Default_IRQ_handler
//  .set WEX0_FDFEVB_ISR, Default_IRQ_handler
//  .set WEX0_FDFEVC_ISR, Default_IRQ_handler
  .set TCE0_OVF_ISR, Default_IRQ_handler
  .set TCE0_CMP0_ISR, Default_IRQ_handler
  .set TCE0_CMP1_ISR, Default_IRQ_handler
  .set TCE0_CMP2_ISR, Default_IRQ_handler
  .set TCE0_CMP3_ISR, Default_IRQ_handler
  .set TCB0_INT_ISR, Default_IRQ_handler
  .set TCB1_INT_ISR, Default_IRQ_handler
  .set TWI0_TWIS_ISR, Default_IRQ_handler
  .set TWI0_TWIM_ISR, Default_IRQ_handler
  .set SPI0_INT_ISR, Default_IRQ_handler
  .set USART0_RXC_ISR, Default_IRQ_handler
  .set USART0_DRE_ISR, Default_IRQ_handler
  .set USART0_TXC_ISR, Default_IRQ_handler
  .set PORTD_PORT_ISR, Default_IRQ_handler
  .set TCF0_INT_ISR, Default_IRQ_handler
  .set AC0_AC_ISR, Default_IRQ_handler
  .set ADC0_ERROR_ISR, Default_IRQ_handler
  .set ADC0_RESRDY_ISR, Default_IRQ_handler
  .set ADC0_SAMPRDY_ISR, Default_IRQ_handler
  .set AC1_AC_ISR, Default_IRQ_handler
  .set PORTC_PORT_ISR, Default_IRQ_handler
  .set PORTF_PORT_ISR, Default_IRQ_handler
  .set NVMCTRL_EEREADY_ISR, Default_IRQ_handler
//  .set NVMCTRL_FLREADY_ISR, Default_IRQ_handler
//  .set NVMCTRL_NVMREADY_ISR, Default_IRQ_handler
end;

end.

unit AVR32DA48S;

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
    POWER_PROFILE2 = $10;
    // Output Pad Enable
    OUTENbm = $40;
    // Run in Standby Mode
    RUNSTDBYbm = $80;
    // AC_WINSEL
    WINSELmask = $03;
    WINSEL_DISABLED = $00;
    WINSEL_UPSEL1 = $01;
    WINSEL_UPSEL2 = $02;
    // AC_MUXNEG
    MUXNEGmask = $07;
    MUXNEG_AINN0 = $00;
    MUXNEG_AINN1 = $01;
    MUXNEG_AINN2 = $02;
    MUXNEG_DACREF = $03;
    // AC_MUXPOS
    MUXPOSmask = $38;
    MUXPOS_AINP0 = $00;
    MUXPOS_AINP1 = $08;
    MUXPOS_AINP2 = $10;
    MUXPOS_AINP3 = $18;
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
    CTRLE: byte;  //Control E
    SAMPCTRL: byte;  //Sample Control
    Reserved6: byte;
    Reserved7: byte;
    MUXPOS: byte;  //Positive mux input
    MUXNEG: byte;  //Negative mux input
    COMMAND: byte;  //Command
    EVCTRL: byte;  //Event Control
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    DBGCTRL: byte;  //Debug Control
    TEMP: byte;  //Temporary Data
    RES: word;  //ADC Accumulator Result
    WINLT: word;  //Window comparator low threshold
    WINHT: word;  //Window comparator high threshold
  const
    // ADC Enable
    ENABLEbm = $01;
    // Free running mode
    FREERUNbm = $02;
    // ADC_RESSEL
    RESSELmask = $0C;
    RESSEL_12BIT = $00;
    RESSEL_10BIT = $04;
    // Left adjust result
    LEFTADJbm = $10;
    // ADC_CONVMODE
    CONVMODEmask = $20;
    CONVMODE_SINGLEENDED = $00;
    CONVMODE_DIFF = $20;
    // Run standby mode
    RUNSTBYbm = $80;
    // ADC_SAMPNUM
    SAMPNUMmask = $07;
    SAMPNUM_NONE = $00;
    SAMPNUM_ACC2 = $01;
    SAMPNUM_ACC4 = $02;
    SAMPNUM_ACC8 = $03;
    SAMPNUM_ACC16 = $04;
    SAMPNUM_ACC32 = $05;
    SAMPNUM_ACC64 = $06;
    SAMPNUM_ACC128 = $07;
    // ADC_PRESC
    PRESCmask = $0F;
    PRESC_DIV2 = $00;
    PRESC_DIV4 = $01;
    PRESC_DIV8 = $02;
    PRESC_DIV12 = $03;
    PRESC_DIV16 = $04;
    PRESC_DIV20 = $05;
    PRESC_DIV24 = $06;
    PRESC_DIV28 = $07;
    PRESC_DIV32 = $08;
    PRESC_DIV48 = $09;
    PRESC_DIV64 = $0A;
    PRESC_DIV96 = $0B;
    PRESC_DIV128 = $0C;
    PRESC_DIV256 = $0D;
    // ADC_SAMPDLY
    SAMPDLYmask = $0F;
    SAMPDLY_DLY0 = $00;
    SAMPDLY_DLY1 = $01;
    SAMPDLY_DLY2 = $02;
    SAMPDLY_DLY3 = $03;
    SAMPDLY_DLY4 = $04;
    SAMPDLY_DLY5 = $05;
    SAMPDLY_DLY6 = $06;
    SAMPDLY_DLY7 = $07;
    SAMPDLY_DLY8 = $08;
    SAMPDLY_DLY9 = $09;
    SAMPDLY_DLY10 = $0A;
    SAMPDLY_DLY11 = $0B;
    SAMPDLY_DLY12 = $0C;
    SAMPDLY_DLY13 = $0D;
    SAMPDLY_DLY14 = $0E;
    SAMPDLY_DLY15 = $0F;
    // ADC_INITDLY
    INITDLYmask = $E0;
    INITDLY_DLY0 = $00;
    INITDLY_DLY16 = $20;
    INITDLY_DLY32 = $40;
    INITDLY_DLY64 = $60;
    INITDLY_DLY128 = $80;
    INITDLY_DLY256 = $A0;
    // ADC_WINCM
    WINCMmask = $07;
    WINCM_NONE = $00;
    WINCM_BELOW = $01;
    WINCM_ABOVE = $02;
    WINCM_INSIDE = $03;
    WINCM_OUTSIDE = $04;
    // ADC_MUXPOS
    MUXPOSmask = $7F;
    MUXPOS_AIN0 = $00;
    MUXPOS_AIN1 = $01;
    MUXPOS_AIN2 = $02;
    MUXPOS_AIN3 = $03;
    MUXPOS_AIN4 = $04;
    MUXPOS_AIN5 = $05;
    MUXPOS_AIN6 = $06;
    MUXPOS_AIN7 = $07;
    MUXPOS_AIN8 = $08;
    MUXPOS_AIN9 = $09;
    MUXPOS_AIN10 = $0A;
    MUXPOS_AIN11 = $0B;
    MUXPOS_AIN16 = $10;
    MUXPOS_AIN17 = $11;
    MUXPOS_AIN18 = $12;
    MUXPOS_AIN19 = $13;
    MUXPOS_AIN20 = $14;
    MUXPOS_AIN21 = $15;
    MUXPOS_GND = $40;
    MUXPOS_TEMPSENSE = $42;
    MUXPOS_DAC0 = $48;
    // ADC_MUXNEG
    MUXNEGmask = $7F;
    MUXNEG_AIN0 = $00;
    MUXNEG_AIN1 = $01;
    MUXNEG_AIN2 = $02;
    MUXNEG_AIN3 = $03;
    MUXNEG_AIN4 = $04;
    MUXNEG_AIN5 = $05;
    MUXNEG_AIN6 = $06;
    MUXNEG_AIN7 = $07;
    MUXNEG_AIN8 = $08;
    MUXNEG_AIN9 = $09;
    MUXNEG_AIN10 = $0A;
    MUXNEG_AIN11 = $0B;
    MUXNEG_GND = $40;
    MUXNEG_DAC0 = $48;
    // Start Conversion
    STCONVbm = $01;
    // Stop Conversion
    SPCONVbm = $02;
    // Start Event Input Enable
    STARTEIbm = $01;
    // Result Ready Interrupt Enable
    RESRDYbm = $01;
    // Window Comparator Interrupt Enable
    WCMPbm = $02;
    // Debug run
    DBGRUNbm = $01;
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
    SLEEP_DIS = $00;
    SLEEP_ENABLED = $01;
    SLEEP_SAMPLED = $02;
    // BOD_ACTIVE
    ACTIVEmask = $0C;
    ACTIVE_DIS = $00;
    ACTIVE_ENABLED = $04;
    ACTIVE_SAMPLED = $08;
    ACTIVE_ENWAKE = $0C;
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
    // voltage level monitor interrrupt enable
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

  TCCL = object //Configurable Custom Logic
    CTRLA: byte;  //Control Register A
    SEQCTRL0: byte;  //Sequential Control 0
    SEQCTRL1: byte;  //Sequential Control 1
    SEQCTRL2: byte;  //Sequential Control 2
    Reserved4: byte;
    INTCTRL0: byte;  //Interrupt Control 0
    INTCTRL1: byte;  //Interrupt Control 1
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
    LUT4CTRLA: byte;  //LUT 4 Control A
    LUT4CTRLB: byte;  //LUT 4 Control B
    LUT4CTRLC: byte;  //LUT 4 Control C
    TRUTH4: byte;  //Truth 4
    LUT5CTRLA: byte;  //LUT 5 Control A
    LUT5CTRLB: byte;  //LUT 5 Control B
    LUT5CTRLC: byte;  //LUT 5 Control C
    TRUTH5: byte;  //Truth 5
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
    // CCL_INTMODE4
    INTMODE4mask = $03;
    INTMODE4_INTDISABLE = $00;
    INTMODE4_RISING = $01;
    INTMODE4_FALLING = $02;
    INTMODE4_BOTH = $03;
    // CCL_INTMODE5
    INTMODE5mask = $0C;
    INTMODE5_INTDISABLE = $00;
    INTMODE5_RISING = $04;
    INTMODE5_FALLING = $08;
    INTMODE5_BOTH = $0C;
    // Interrupt Flag
    INT0bm = $01;
    INT1bm = $02;
    INT2bm = $04;
    INT3bm = $08;
    INT4bm = $10;
    INT5bm = $20;
    // CCL_CLKSRC
    CLKSRCmask = $0E;
    CLKSRC_CLKPER = $00;
    CLKSRC_IN2 = $02;
    CLKSRC_OSCHF = $08;
    CLKSRC_OSC32K = $0A;
    CLKSRC_OSC1K = $0C;
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
    INSEL0_ZCD0 = $07;
    INSEL0_USART0 = $08;
    INSEL0_SPI0 = $09;
    INSEL0_TCA0 = $0A;
    INSEL0_TCA1 = $0B;
    INSEL0_TCB0 = $0C;
    INSEL0_TCD0 = $0D;
    // CCL_INSEL1
    INSEL1mask = $F0;
    INSEL1_MASK = $00;
    INSEL1_FEEDBACK = $10;
    INSEL1_LINK = $20;
    INSEL1_EVENTA = $30;
    INSEL1_EVENTB = $40;
    INSEL1_IN1 = $50;
    INSEL1_AC1 = $60;
    INSEL1_ZCD1 = $70;
    INSEL1_USART1 = $80;
    INSEL1_SPI0 = $90;
    INSEL1_TCA0 = $A0;
    INSEL1_TCA1 = $B0;
    INSEL1_TCB1 = $C0;
    INSEL1_TCD0 = $D0;
    // CCL_INSEL2
    INSEL2mask = $0F;
    INSEL2_MASK = $00;
    INSEL2_FEEDBACK = $01;
    INSEL2_LINK = $02;
    INSEL2_EVENTA = $03;
    INSEL2_EVENTB = $04;
    INSEL2_IN2 = $05;
    INSEL2_AC2 = $06;
    INSEL2_USART2 = $08;
    INSEL2_SPI0 = $09;
    INSEL2_TCA0 = $0A;
    INSEL2_TCA1 = $0B;
    INSEL2_TCB2 = $0C;
    INSEL2_TCD0 = $0D;
  end;

  TCLKCTRL = object //Clock controller
    MCLKCTRLA: byte;  //MCLK Control A
    MCLKCTRLB: byte;  //MCLK Control B
    MCLKLOCK: byte;  //MCLK Lock
    MCLKSTATUS: byte;  //MCLK Status
    Reserved4: byte;
    Reserved5: byte;
    Reserved6: byte;
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
    Reserved17: byte;
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
    CLKSELmask = $07;
    CLKSEL_OSCHF = $00;
    CLKSEL_OSC32K = $01;
    CLKSEL_XOSC32K = $02;
    CLKSEL_EXTCLK = $03;
    // System clock out
    CLKOUTbm = $80;
    // Prescaler enable
    PENbm = $01;
    // CLKCTRL_PDIV
    PDIVmask = $1E;
    PDIV_2X = $00;
    PDIV_4X = $02;
    PDIV_8X = $04;
    PDIV_16X = $06;
    PDIV_32X = $08;
    PDIV_64X = $0A;
    PDIV_6X = $10;
    PDIV_10X = $12;
    PDIV_12X = $14;
    PDIV_24X = $16;
    PDIV_48X = $18;
    // lock ebable
    LOCKENbm = $01;
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
    // PLL oscillator status
    PLLSbm = $20;
    // Autotune
    AUTOTUNEbm = $01;
    // CLKCTRL_FRQSEL
    FRQSELmask = $3C;
    FRQSEL_1M = $00;
    FRQSEL_2M = $04;
    FRQSEL_3M = $08;
    FRQSEL_4M = $0C;
    FRQSEL_8M = $14;
    FRQSEL_12M = $18;
    FRQSEL_16M = $1C;
    FRQSEL_20M = $20;
    FRQSEL_24M = $24;
    // Run standby
    RUNSTDBYbm = $80;
    // CLKCTRL_MULFAC
    MULFACmask = $03;
    MULFAC_DISABLE = $00;
    MULFAC_2x = $01;
    MULFAC_3x = $02;
    // Source
    SOURCEbm = $40;
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

  TDAC = object //Digital to Analog Converter
    CTRLA: byte;  //Control Register A
    Reserved1: byte;
    DATA: word;  //DATA Register
  const
    // DAC Enable
    ENABLEbm = $01;
    // Output Buffer Enable
    OUTENbm = $40;
    // Run in Standby Mode
    RUNSTDBYbm = $80;
  end;

  TEVSYS = object //Event System
    SWEVENTA: byte;  //Software Event A
    SWEVENTB: byte;  //Software Event B
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
    CHANNEL6: byte;  //Multiplexer Channel 6
    CHANNEL7: byte;  //Multiplexer Channel 7
    CHANNEL8: byte;  //Multiplexer Channel 8
    CHANNEL9: byte;  //Multiplexer Channel 9
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    Reserved30: byte;
    Reserved31: byte;
    USERCCLLUT0A: byte;  //User 0 - CCL0 Event A
    USERCCLLUT0B: byte;  //User 1 - CCL0 Event B
    USERCCLLUT1A: byte;  //User 2 - CCL1 Event A
    USERCCLLUT1B: byte;  //User 3 - CCL1 Event B
    USERCCLLUT2A: byte;  //User 4 - CCL2 Event A
    USERCCLLUT2B: byte;  //User 5 - CCL2 Event B
    USERCCLLUT3A: byte;  //User 6 - CCL3 Event A
    USERCCLLUT3B: byte;  //User 7 - CCL3 Event B
    USERCCLLUT4A: byte;  //User 8 - CCL4 Event A
    USERCCLLUT4B: byte;  //User 9 - CCL4 Event B
    USERCCLLUT5A: byte;  //User 10 - CCL5 Event A
    USERCCLLUT5B: byte;  //User 11 - CCL5 Event B
    USERADC0START: byte;  //User 12 - ADC0
    USERPTCSTART: byte;  //User 13 - PTC
    USEREVSYSEVOUTA: byte;  //User 14 - EVOUTA
    USEREVSYSEVOUTB: byte;  //User 15 - EVOUTB
    USEREVSYSEVOUTC: byte;  //User 16 - EVOUTC
    USEREVSYSEVOUTD: byte;  //User 17 - EVOUTD
    USEREVSYSEVOUTE: byte;  //User 18 - EVOUTE
    USEREVSYSEVOUTF: byte;  //User 19 - EVOUTF
    Reserved52: byte;
    USERUSART0IRDA: byte;  //User 21 - USART0
    USERUSART1IRDA: byte;  //User 22 - USART1
    USERUSART2IRDA: byte;  //User 23 - USART2
    USERUSART3IRDA: byte;  //User 24 - USART3
    USERUSART4IRDA: byte;  //User 25 - USART4
    Reserved58: byte;
    USERTCA0CNTA: byte;  //User 27 - TCA0 Event A
    USERTCA0CNTB: byte;  //User 28 - TCA0 Event B
    USERTCA1CNTA: byte;  //User 29 - TCA1 Event A
    USERTCA1CNTB: byte;  //User 30 - TCA1 Event B
    USERTCB0CAPT: byte;  //User 31 - TCB0 Event A
    USERTCB0COUNT: byte;  //User 32 - TCB0 Event B
    USERTCB1CAPT: byte;  //User 33 - TCB1 Event A
    USERTCB1COUNT: byte;  //User 34 - TCB1 Event B
    USERTCB2CAPT: byte;  //User 35 - TCB2 Event A
    USERTCB2COUNT: byte;  //User 36 - TCB2 Event B
    USERTCB3CAPT: byte;  //User 37 - TCB3 Event A
    USERTCB3COUNT: byte;  //User 38 - TCB3 Event B
    Reserved71: byte;
    Reserved72: byte;
    USERTCD0INPUTA: byte;  //User 41 - TCD0 Event A
    USERTCD0INPUTB: byte;  //User 42 - TCD0 Event B
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
    // EVSYS_SWEVENTB
    SWEVENTBmask = $03;
    SWEVENTB_CH8 = $01;
    SWEVENTB_CH9 = $02;
    // EVSYS_CHANNEL0
    CHANNEL0mask = $FF;
    CHANNEL0_OFF = $00;
    CHANNEL0_UPDI_SYNCH = $01;
    CHANNEL0_RTC_OVF = $06;
    CHANNEL0_RTC_CMP = $07;
    CHANNEL0_RTC_PIT_DIV8192 = $08;
    CHANNEL0_RTC_PIT_DIV4096 = $09;
    CHANNEL0_RTC_PIT_DIV2048 = $0A;
    CHANNEL0_RTC_PIT_DIV1024 = $0B;
    CHANNEL0_CCL_LUT0 = $10;
    CHANNEL0_CCL_LUT1 = $11;
    CHANNEL0_CCL_LUT2 = $12;
    CHANNEL0_CCL_LUT3 = $13;
    CHANNEL0_CCL_LUT4 = $14;
    CHANNEL0_CCL_LUT5 = $15;
    CHANNEL0_AC0_OUT = $20;
    CHANNEL0_AC1_OUT = $21;
    CHANNEL0_AC2_OUT = $22;
    CHANNEL0_ADC0_RESRDY = $24;
    CHANNEL0_PTC_RESRDY = $28;
    CHANNEL0_ZCD0 = $30;
    CHANNEL0_ZCD1 = $31;
    CHANNEL0_PORTA_PIN0 = $40;
    CHANNEL0_PORTA_PIN1 = $41;
    CHANNEL0_PORTA_PIN2 = $42;
    CHANNEL0_PORTA_PIN3 = $43;
    CHANNEL0_PORTA_PIN4 = $44;
    CHANNEL0_PORTA_PIN5 = $45;
    CHANNEL0_PORTA_PIN6 = $46;
    CHANNEL0_PORTA_PIN7 = $47;
    CHANNEL0_PORTB_PIN0 = $48;
    CHANNEL0_PORTB_PIN1 = $49;
    CHANNEL0_PORTB_PIN2 = $4A;
    CHANNEL0_PORTB_PIN3 = $4B;
    CHANNEL0_PORTB_PIN4 = $4C;
    CHANNEL0_PORTB_PIN5 = $4D;
    CHANNEL0_USART0_XCK = $60;
    CHANNEL0_USART1_XCK = $61;
    CHANNEL0_USART2_XCK = $62;
    CHANNEL0_USART3_XCK = $63;
    CHANNEL0_USART4_XCK = $64;
    CHANNEL0_SPI0_SCK = $68;
    CHANNEL0_SPI1_SCK = $69;
    CHANNEL0_TCA0_OVF_LUNF = $80;
    CHANNEL0_TCA0_HUNF = $81;
    CHANNEL0_TCA0_CMP0_LCMP0 = $84;
    CHANNEL0_TCA0_CMP1_LCMP1 = $85;
    CHANNEL0_TCA0_CMP2_LCMP2 = $86;
    CHANNEL0_TCA1_OVF_LUNF = $88;
    CHANNEL0_TCA1_HUNF = $89;
    CHANNEL0_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL0_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL0_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL0_TCB0_CAPT = $A0;
    CHANNEL0_TCB0_OVF = $A1;
    CHANNEL0_TCB1_CAPT = $A2;
    CHANNEL0_TCB1_OVF = $A3;
    CHANNEL0_TCB2_CAPT = $A4;
    CHANNEL0_TCB2_OVF = $A5;
    CHANNEL0_TCB3_CAPT = $A6;
    CHANNEL0_TCB3_OVF = $A7;
    CHANNEL0_TCD0_CMPBCLR = $B0;
    CHANNEL0_TCD0_CMPASET = $B1;
    CHANNEL0_TCD0_CMPBSET = $B2;
    CHANNEL0_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL1
    CHANNEL1mask = $FF;
    CHANNEL1_OFF = $00;
    CHANNEL1_UPDI_SYNCH = $01;
    CHANNEL1_RTC_OVF = $06;
    CHANNEL1_RTC_CMP = $07;
    CHANNEL1_RTC_PIT_DIV512 = $08;
    CHANNEL1_RTC_PIT_DIV256 = $09;
    CHANNEL1_RTC_PIT_DIV128 = $0A;
    CHANNEL1_RTC_PIT_DIV64 = $0B;
    CHANNEL1_CCL_LUT0 = $10;
    CHANNEL1_CCL_LUT1 = $11;
    CHANNEL1_CCL_LUT2 = $12;
    CHANNEL1_CCL_LUT3 = $13;
    CHANNEL1_CCL_LUT4 = $14;
    CHANNEL1_CCL_LUT5 = $15;
    CHANNEL1_AC0_OUT = $20;
    CHANNEL1_AC1_OUT = $21;
    CHANNEL1_AC2_OUT = $22;
    CHANNEL1_ADC0_RESRDY = $24;
    CHANNEL1_PTC_RESRDY = $28;
    CHANNEL1_ZCD0 = $30;
    CHANNEL1_ZCD1 = $31;
    CHANNEL1_PORTA_PIN0 = $40;
    CHANNEL1_PORTA_PIN1 = $41;
    CHANNEL1_PORTA_PIN2 = $42;
    CHANNEL1_PORTA_PIN3 = $43;
    CHANNEL1_PORTA_PIN4 = $44;
    CHANNEL1_PORTA_PIN5 = $45;
    CHANNEL1_PORTA_PIN6 = $46;
    CHANNEL1_PORTA_PIN7 = $47;
    CHANNEL1_PORTB_PIN0 = $48;
    CHANNEL1_PORTB_PIN1 = $49;
    CHANNEL1_PORTB_PIN2 = $4A;
    CHANNEL1_PORTB_PIN3 = $4B;
    CHANNEL1_PORTB_PIN4 = $4C;
    CHANNEL1_PORTB_PIN5 = $4D;
    CHANNEL1_USART0_XCK = $60;
    CHANNEL1_USART1_XCK = $61;
    CHANNEL1_USART2_XCK = $62;
    CHANNEL1_USART3_XCK = $63;
    CHANNEL1_USART4_XCK = $64;
    CHANNEL1_SPI0_SCK = $68;
    CHANNEL1_SPI1_SCK = $69;
    CHANNEL1_TCA0_OVF_LUNF = $80;
    CHANNEL1_TCA0_HUNF = $81;
    CHANNEL1_TCA0_CMP0_LCMP0 = $84;
    CHANNEL1_TCA0_CMP1_LCMP1 = $85;
    CHANNEL1_TCA0_CMP2_LCMP2 = $86;
    CHANNEL1_TCA1_OVF_LUNF = $88;
    CHANNEL1_TCA1_HUNF = $89;
    CHANNEL1_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL1_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL1_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL1_TCB0_CAPT = $A0;
    CHANNEL1_TCB0_OVF = $A1;
    CHANNEL1_TCB1_CAPT = $A2;
    CHANNEL1_TCB1_OVF = $A3;
    CHANNEL1_TCB2_CAPT = $A4;
    CHANNEL1_TCB2_OVF = $A5;
    CHANNEL1_TCB3_CAPT = $A6;
    CHANNEL1_TCB3_OVF = $A7;
    CHANNEL1_TCD0_CMPBCLR = $B0;
    CHANNEL1_TCD0_CMPASET = $B1;
    CHANNEL1_TCD0_CMPBSET = $B2;
    CHANNEL1_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL2
    CHANNEL2mask = $FF;
    CHANNEL2_OFF = $00;
    CHANNEL2_UPDI_SYNCH = $01;
    CHANNEL2_RTC_OVF = $06;
    CHANNEL2_RTC_CMP = $07;
    CHANNEL2_RTC_PIT_DIV8192 = $08;
    CHANNEL2_RTC_PIT_DIV4096 = $09;
    CHANNEL2_RTC_PIT_DIV2048 = $0A;
    CHANNEL2_RTC_PIT_DIV1024 = $0B;
    CHANNEL2_CCL_LUT0 = $10;
    CHANNEL2_CCL_LUT1 = $11;
    CHANNEL2_CCL_LUT2 = $12;
    CHANNEL2_CCL_LUT3 = $13;
    CHANNEL2_CCL_LUT4 = $14;
    CHANNEL2_CCL_LUT5 = $15;
    CHANNEL2_AC0_OUT = $20;
    CHANNEL2_AC1_OUT = $21;
    CHANNEL2_AC2_OUT = $22;
    CHANNEL2_ADC0_RESRDY = $24;
    CHANNEL2_PTC_RESRDY = $28;
    CHANNEL2_ZCD0 = $30;
    CHANNEL2_ZCD1 = $31;
    CHANNEL2_PORTC_PIN0 = $40;
    CHANNEL2_PORTC_PIN1 = $41;
    CHANNEL2_PORTC_PIN2 = $42;
    CHANNEL2_PORTC_PIN3 = $43;
    CHANNEL2_PORTC_PIN4 = $44;
    CHANNEL2_PORTC_PIN5 = $45;
    CHANNEL2_PORTC_PIN6 = $46;
    CHANNEL2_PORTC_PIN7 = $47;
    CHANNEL2_PORTD_PIN0 = $48;
    CHANNEL2_PORTD_PIN1 = $49;
    CHANNEL2_PORTD_PIN2 = $4A;
    CHANNEL2_PORTD_PIN3 = $4B;
    CHANNEL2_PORTD_PIN4 = $4C;
    CHANNEL2_PORTD_PIN5 = $4D;
    CHANNEL2_PORTD_PIN6 = $4E;
    CHANNEL2_PORTD_PIN7 = $4F;
    CHANNEL2_USART0_XCK = $60;
    CHANNEL2_USART1_XCK = $61;
    CHANNEL2_USART2_XCK = $62;
    CHANNEL2_USART3_XCK = $63;
    CHANNEL2_USART4_XCK = $64;
    CHANNEL2_SPI0_SCK = $68;
    CHANNEL2_SPI1_SCK = $69;
    CHANNEL2_TCA0_OVF_LUNF = $80;
    CHANNEL2_TCA0_HUNF = $81;
    CHANNEL2_TCA0_CMP0_LCMP0 = $84;
    CHANNEL2_TCA0_CMP1_LCMP1 = $85;
    CHANNEL2_TCA0_CMP2_LCMP2 = $86;
    CHANNEL2_TCA1_OVF_LUNF = $88;
    CHANNEL2_TCA1_HUNF = $89;
    CHANNEL2_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL2_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL2_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL2_TCB0_CAPT = $A0;
    CHANNEL2_TCB0_OVF = $A1;
    CHANNEL2_TCB1_CAPT = $A2;
    CHANNEL2_TCB1_OVF = $A3;
    CHANNEL2_TCB2_CAPT = $A4;
    CHANNEL2_TCB2_OVF = $A5;
    CHANNEL2_TCB3_CAPT = $A6;
    CHANNEL2_TCB3_OVF = $A7;
    CHANNEL2_TCD0_CMPBCLR = $B0;
    CHANNEL2_TCD0_CMPASET = $B1;
    CHANNEL2_TCD0_CMPBSET = $B2;
    CHANNEL2_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL3
    CHANNEL3mask = $FF;
    CHANNEL3_OFF = $00;
    CHANNEL3_UPDI_SYNCH = $01;
    CHANNEL3_RTC_OVF = $06;
    CHANNEL3_RTC_CMP = $07;
    CHANNEL3_RTC_PIT_DIV512 = $08;
    CHANNEL3_RTC_PIT_DIV256 = $09;
    CHANNEL3_RTC_PIT_DIV128 = $0A;
    CHANNEL3_RTC_PIT_DIV64 = $0B;
    CHANNEL3_CCL_LUT0 = $10;
    CHANNEL3_CCL_LUT1 = $11;
    CHANNEL3_CCL_LUT2 = $12;
    CHANNEL3_CCL_LUT3 = $13;
    CHANNEL3_CCL_LUT4 = $14;
    CHANNEL3_CCL_LUT5 = $15;
    CHANNEL3_AC0_OUT = $20;
    CHANNEL3_AC1_OUT = $21;
    CHANNEL3_AC2_OUT = $22;
    CHANNEL3_ADC0_RESRDY = $24;
    CHANNEL3_PTC_RESRDY = $28;
    CHANNEL3_ZCD0 = $30;
    CHANNEL3_ZCD1 = $31;
    CHANNEL3_PORTC_PIN0 = $40;
    CHANNEL3_PORTC_PIN1 = $41;
    CHANNEL3_PORTC_PIN2 = $42;
    CHANNEL3_PORTC_PIN3 = $43;
    CHANNEL3_PORTC_PIN4 = $44;
    CHANNEL3_PORTC_PIN5 = $45;
    CHANNEL3_PORTC_PIN6 = $46;
    CHANNEL3_PORTC_PIN7 = $47;
    CHANNEL3_PORTD_PIN0 = $48;
    CHANNEL3_PORTD_PIN1 = $49;
    CHANNEL3_PORTD_PIN2 = $4A;
    CHANNEL3_PORTD_PIN3 = $4B;
    CHANNEL3_PORTD_PIN4 = $4C;
    CHANNEL3_PORTD_PIN5 = $4D;
    CHANNEL3_PORTD_PIN6 = $4E;
    CHANNEL3_PORTD_PIN7 = $4F;
    CHANNEL3_USART0_XCK = $60;
    CHANNEL3_USART1_XCK = $61;
    CHANNEL3_USART2_XCK = $62;
    CHANNEL3_USART3_XCK = $63;
    CHANNEL3_USART4_XCK = $64;
    CHANNEL3_SPI0_SCK = $68;
    CHANNEL3_SPI1_SCK = $69;
    CHANNEL3_TCA0_OVF_LUNF = $80;
    CHANNEL3_TCA0_HUNF = $81;
    CHANNEL3_TCA0_CMP0_LCMP0 = $84;
    CHANNEL3_TCA0_CMP1_LCMP1 = $85;
    CHANNEL3_TCA0_CMP2_LCMP2 = $86;
    CHANNEL3_TCA1_OVF_LUNF = $88;
    CHANNEL3_TCA1_HUNF = $89;
    CHANNEL3_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL3_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL3_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL3_TCB0_CAPT = $A0;
    CHANNEL3_TCB0_OVF = $A1;
    CHANNEL3_TCB1_CAPT = $A2;
    CHANNEL3_TCB1_OVF = $A3;
    CHANNEL3_TCB2_CAPT = $A4;
    CHANNEL3_TCB2_OVF = $A5;
    CHANNEL3_TCB3_CAPT = $A6;
    CHANNEL3_TCB3_OVF = $A7;
    CHANNEL3_TCD0_CMPBCLR = $B0;
    CHANNEL3_TCD0_CMPASET = $B1;
    CHANNEL3_TCD0_CMPBSET = $B2;
    CHANNEL3_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL4
    CHANNEL4mask = $FF;
    CHANNEL4_OFF = $00;
    CHANNEL4_UPDI_SYNCH = $01;
    CHANNEL4_RTC_OVF = $06;
    CHANNEL4_RTC_CMP = $07;
    CHANNEL4_RTC_PIT_DIV8192 = $08;
    CHANNEL4_RTC_PIT_DIV4096 = $09;
    CHANNEL4_RTC_PIT_DIV2048 = $0A;
    CHANNEL4_RTC_PIT_DIV1024 = $0B;
    CHANNEL4_CCL_LUT0 = $10;
    CHANNEL4_CCL_LUT1 = $11;
    CHANNEL4_CCL_LUT2 = $12;
    CHANNEL4_CCL_LUT3 = $13;
    CHANNEL4_CCL_LUT4 = $14;
    CHANNEL4_CCL_LUT5 = $15;
    CHANNEL4_AC0_OUT = $20;
    CHANNEL4_AC1_OUT = $21;
    CHANNEL4_AC2_OUT = $22;
    CHANNEL4_ADC0_RESRDY = $24;
    CHANNEL4_PTC_RESRDY = $28;
    CHANNEL4_ZCD0 = $30;
    CHANNEL4_ZCD1 = $31;
    CHANNEL4_PORTE_PIN0 = $40;
    CHANNEL4_PORTE_PIN1 = $41;
    CHANNEL4_PORTE_PIN2 = $42;
    CHANNEL4_PORTE_PIN3 = $43;
    CHANNEL4_PORTF_PIN0 = $48;
    CHANNEL4_PORTF_PIN1 = $49;
    CHANNEL4_PORTF_PIN2 = $4A;
    CHANNEL4_PORTF_PIN3 = $4B;
    CHANNEL4_PORTF_PIN4 = $4C;
    CHANNEL4_PORTF_PIN5 = $4D;
    CHANNEL4_PORTF_PIN6 = $4E;
    CHANNEL4_USART0_XCK = $60;
    CHANNEL4_USART1_XCK = $61;
    CHANNEL4_USART2_XCK = $62;
    CHANNEL4_USART3_XCK = $63;
    CHANNEL4_USART4_XCK = $64;
    CHANNEL4_SPI0_SCK = $68;
    CHANNEL4_SPI1_SCK = $69;
    CHANNEL4_TCA0_OVF_LUNF = $80;
    CHANNEL4_TCA0_HUNF = $81;
    CHANNEL4_TCA0_CMP0_LCMP0 = $84;
    CHANNEL4_TCA0_CMP1_LCMP1 = $85;
    CHANNEL4_TCA0_CMP2_LCMP2 = $86;
    CHANNEL4_TCA1_OVF_LUNF = $88;
    CHANNEL4_TCA1_HUNF = $89;
    CHANNEL4_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL4_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL4_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL4_TCB0_CAPT = $A0;
    CHANNEL4_TCB0_OVF = $A1;
    CHANNEL4_TCB1_CAPT = $A2;
    CHANNEL4_TCB1_OVF = $A3;
    CHANNEL4_TCB2_CAPT = $A4;
    CHANNEL4_TCB2_OVF = $A5;
    CHANNEL4_TCB3_CAPT = $A6;
    CHANNEL4_TCB3_OVF = $A7;
    CHANNEL4_TCD0_CMPBCLR = $B0;
    CHANNEL4_TCD0_CMPASET = $B1;
    CHANNEL4_TCD0_CMPBSET = $B2;
    CHANNEL4_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL5
    CHANNEL5mask = $FF;
    CHANNEL5_OFF = $00;
    CHANNEL5_UPDI_SYNCH = $01;
    CHANNEL5_RTC_OVF = $06;
    CHANNEL5_RTC_CMP = $07;
    CHANNEL5_RTC_PIT_DIV512 = $08;
    CHANNEL5_RTC_PIT_DIV256 = $09;
    CHANNEL5_RTC_PIT_DIV128 = $0A;
    CHANNEL5_RTC_PIT_DIV64 = $0B;
    CHANNEL5_CCL_LUT0 = $10;
    CHANNEL5_CCL_LUT1 = $11;
    CHANNEL5_CCL_LUT2 = $12;
    CHANNEL5_CCL_LUT3 = $13;
    CHANNEL5_CCL_LUT4 = $14;
    CHANNEL5_CCL_LUT5 = $15;
    CHANNEL5_AC0_OUT = $20;
    CHANNEL5_AC1_OUT = $21;
    CHANNEL5_AC2_OUT = $22;
    CHANNEL5_ADC0_RESRDY = $24;
    CHANNEL5_PTC_RESRDY = $28;
    CHANNEL5_ZCD0 = $30;
    CHANNEL5_ZCD1 = $31;
    CHANNEL5_PORTE_PIN0 = $40;
    CHANNEL5_PORTE_PIN1 = $41;
    CHANNEL5_PORTE_PIN2 = $42;
    CHANNEL5_PORTE_PIN3 = $43;
    CHANNEL5_PORTF_PIN0 = $48;
    CHANNEL5_PORTF_PIN1 = $49;
    CHANNEL5_PORTF_PIN2 = $4A;
    CHANNEL5_PORTF_PIN3 = $4B;
    CHANNEL5_PORTF_PIN4 = $4C;
    CHANNEL5_PORTF_PIN5 = $4D;
    CHANNEL5_PORTF_PIN6 = $4E;
    CHANNEL5_USART0_XCK = $60;
    CHANNEL5_USART1_XCK = $61;
    CHANNEL5_USART2_XCK = $62;
    CHANNEL5_USART3_XCK = $63;
    CHANNEL5_USART4_XCK = $64;
    CHANNEL5_SPI0_SCK = $68;
    CHANNEL5_SPI1_SCK = $69;
    CHANNEL5_TCA0_OVF_LUNF = $80;
    CHANNEL5_TCA0_HUNF = $81;
    CHANNEL5_TCA0_CMP0_LCMP0 = $84;
    CHANNEL5_TCA0_CMP1_LCMP1 = $85;
    CHANNEL5_TCA0_CMP2_LCMP2 = $86;
    CHANNEL5_TCA1_OVF_LUNF = $88;
    CHANNEL5_TCA1_HUNF = $89;
    CHANNEL5_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL5_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL5_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL5_TCB0_CAPT = $A0;
    CHANNEL5_TCB0_OVF = $A1;
    CHANNEL5_TCB1_CAPT = $A2;
    CHANNEL5_TCB1_OVF = $A3;
    CHANNEL5_TCB2_CAPT = $A4;
    CHANNEL5_TCB2_OVF = $A5;
    CHANNEL5_TCB3_CAPT = $A6;
    CHANNEL5_TCB3_OVF = $A7;
    CHANNEL5_TCD0_CMPBCLR = $B0;
    CHANNEL5_TCD0_CMPASET = $B1;
    CHANNEL5_TCD0_CMPBSET = $B2;
    CHANNEL5_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL6
    CHANNEL6mask = $FF;
    CHANNEL6_OFF = $00;
    CHANNEL6_UPDI_SYNCH = $01;
    CHANNEL6_RTC_OVF = $06;
    CHANNEL6_RTC_CMP = $07;
    CHANNEL6_RTC_PIT_DIV8192 = $08;
    CHANNEL6_RTC_PIT_DIV4096 = $09;
    CHANNEL6_RTC_PIT_DIV2048 = $0A;
    CHANNEL6_RTC_PIT_DIV1024 = $0B;
    CHANNEL6_CCL_LUT0 = $10;
    CHANNEL6_CCL_LUT1 = $11;
    CHANNEL6_CCL_LUT2 = $12;
    CHANNEL6_CCL_LUT3 = $13;
    CHANNEL6_CCL_LUT4 = $14;
    CHANNEL6_CCL_LUT5 = $15;
    CHANNEL6_AC0_OUT = $20;
    CHANNEL6_AC1_OUT = $21;
    CHANNEL6_AC2_OUT = $22;
    CHANNEL6_ADC0_RESRDY = $24;
    CHANNEL6_PTC_RESRDY = $28;
    CHANNEL6_ZCD0 = $30;
    CHANNEL6_ZCD1 = $31;
    CHANNEL6_USART0_XCK = $60;
    CHANNEL6_USART1_XCK = $61;
    CHANNEL6_USART2_XCK = $62;
    CHANNEL6_USART3_XCK = $63;
    CHANNEL6_USART4_XCK = $64;
    CHANNEL6_SPI0_SCK = $68;
    CHANNEL6_SPI1_SCK = $69;
    CHANNEL6_TCA0_OVF_LUNF = $80;
    CHANNEL6_TCA0_HUNF = $81;
    CHANNEL6_TCA0_CMP0_LCMP0 = $84;
    CHANNEL6_TCA0_CMP1_LCMP1 = $85;
    CHANNEL6_TCA0_CMP2_LCMP2 = $86;
    CHANNEL6_TCA1_OVF_LUNF = $88;
    CHANNEL6_TCA1_HUNF = $89;
    CHANNEL6_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL6_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL6_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL6_TCB0_CAPT = $A0;
    CHANNEL6_TCB0_OVF = $A1;
    CHANNEL6_TCB1_CAPT = $A2;
    CHANNEL6_TCB1_OVF = $A3;
    CHANNEL6_TCB2_CAPT = $A4;
    CHANNEL6_TCB2_OVF = $A5;
    CHANNEL6_TCB3_CAPT = $A6;
    CHANNEL6_TCB3_OVF = $A7;
    CHANNEL6_TCD0_CMPBCLR = $B0;
    CHANNEL6_TCD0_CMPASET = $B1;
    CHANNEL6_TCD0_CMPBSET = $B2;
    CHANNEL6_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL7
    CHANNEL7mask = $FF;
    CHANNEL7_OFF = $00;
    CHANNEL7_UPDI_SYNCH = $01;
    CHANNEL7_RTC_OVF = $06;
    CHANNEL7_RTC_CMP = $07;
    CHANNEL7_RTC_PIT_DIV512 = $08;
    CHANNEL7_RTC_PIT_DIV256 = $09;
    CHANNEL7_RTC_PIT_DIV128 = $0A;
    CHANNEL7_RTC_PIT_DIV64 = $0B;
    CHANNEL7_CCL_LUT0 = $10;
    CHANNEL7_CCL_LUT1 = $11;
    CHANNEL7_CCL_LUT2 = $12;
    CHANNEL7_CCL_LUT3 = $13;
    CHANNEL7_CCL_LUT4 = $14;
    CHANNEL7_CCL_LUT5 = $15;
    CHANNEL7_AC0_OUT = $20;
    CHANNEL7_AC1_OUT = $21;
    CHANNEL7_AC2_OUT = $22;
    CHANNEL7_ADC0_RESRDY = $24;
    CHANNEL7_PTC_RESRDY = $28;
    CHANNEL7_ZCD0 = $30;
    CHANNEL7_ZCD1 = $31;
    CHANNEL7_USART0_XCK = $60;
    CHANNEL7_USART1_XCK = $61;
    CHANNEL7_USART2_XCK = $62;
    CHANNEL7_USART3_XCK = $63;
    CHANNEL7_USART4_XCK = $64;
    CHANNEL7_SPI0_SCK = $68;
    CHANNEL7_SPI1_SCK = $69;
    CHANNEL7_TCA0_OVF_LUNF = $80;
    CHANNEL7_TCA0_HUNF = $81;
    CHANNEL7_TCA0_CMP0_LCMP0 = $84;
    CHANNEL7_TCA0_CMP1_LCMP1 = $85;
    CHANNEL7_TCA0_CMP2_LCMP2 = $86;
    CHANNEL7_TCA1_OVF_LUNF = $88;
    CHANNEL7_TCA1_HUNF = $89;
    CHANNEL7_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL7_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL7_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL7_TCB0_CAPT = $A0;
    CHANNEL7_TCB0_OVF = $A1;
    CHANNEL7_TCB1_CAPT = $A2;
    CHANNEL7_TCB1_OVF = $A3;
    CHANNEL7_TCB2_CAPT = $A4;
    CHANNEL7_TCB2_OVF = $A5;
    CHANNEL7_TCB3_CAPT = $A6;
    CHANNEL7_TCB3_OVF = $A7;
    CHANNEL7_TCD0_CMPBCLR = $B0;
    CHANNEL7_TCD0_CMPASET = $B1;
    CHANNEL7_TCD0_CMPBSET = $B2;
    CHANNEL7_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL8
    CHANNEL8mask = $FF;
    CHANNEL8_OFF = $00;
    CHANNEL8_UPDI_SYNCH = $01;
    CHANNEL8_RTC_OVF = $06;
    CHANNEL8_RTC_CMP = $07;
    CHANNEL8_RTC_PIT_DIV8192 = $08;
    CHANNEL8_RTC_PIT_DIV4096 = $09;
    CHANNEL8_RTC_PIT_DIV2048 = $0A;
    CHANNEL8_RTC_PIT_DIV1024 = $0B;
    CHANNEL8_CCL_LUT0 = $10;
    CHANNEL8_CCL_LUT1 = $11;
    CHANNEL8_CCL_LUT2 = $12;
    CHANNEL8_CCL_LUT3 = $13;
    CHANNEL8_CCL_LUT4 = $14;
    CHANNEL8_CCL_LUT5 = $15;
    CHANNEL8_AC0_OUT = $20;
    CHANNEL8_AC1_OUT = $21;
    CHANNEL8_AC2_OUT = $22;
    CHANNEL8_ADC0_RESRDY = $24;
    CHANNEL8_PTC_RESRDY = $28;
    CHANNEL8_ZCD0 = $30;
    CHANNEL8_ZCD1 = $31;
    CHANNEL8_USART0_XCK = $60;
    CHANNEL8_USART1_XCK = $61;
    CHANNEL8_USART2_XCK = $62;
    CHANNEL8_USART3_XCK = $63;
    CHANNEL8_USART4_XCK = $64;
    CHANNEL8_SPI0_SCK = $68;
    CHANNEL8_SPI1_SCK = $69;
    CHANNEL8_TCA0_OVF_LUNF = $80;
    CHANNEL8_TCA0_HUNF = $81;
    CHANNEL8_TCA0_CMP0_LCMP0 = $84;
    CHANNEL8_TCA0_CMP1_LCMP1 = $85;
    CHANNEL8_TCA0_CMP2_LCMP2 = $86;
    CHANNEL8_TCA1_OVF_LUNF = $88;
    CHANNEL8_TCA1_HUNF = $89;
    CHANNEL8_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL8_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL8_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL8_TCB0_CAPT = $A0;
    CHANNEL8_TCB0_OVF = $A1;
    CHANNEL8_TCB1_CAPT = $A2;
    CHANNEL8_TCB1_OVF = $A3;
    CHANNEL8_TCB2_CAPT = $A4;
    CHANNEL8_TCB2_OVF = $A5;
    CHANNEL8_TCB3_CAPT = $A6;
    CHANNEL8_TCB3_OVF = $A7;
    CHANNEL8_TCD0_CMPBCLR = $B0;
    CHANNEL8_TCD0_CMPASET = $B1;
    CHANNEL8_TCD0_CMPBSET = $B2;
    CHANNEL8_TCD0_PROGEV = $B3;
    // EVSYS_CHANNEL9
    CHANNEL9mask = $FF;
    CHANNEL9_OFF = $00;
    CHANNEL9_UPDI_SYNCH = $01;
    CHANNEL9_RTC_OVF = $06;
    CHANNEL9_RTC_CMP = $07;
    CHANNEL9_RTC_PIT_DIV512 = $08;
    CHANNEL9_RTC_PIT_DIV256 = $09;
    CHANNEL9_RTC_PIT_DIV128 = $0A;
    CHANNEL9_RTC_PIT_DIV64 = $0B;
    CHANNEL9_CCL_LUT0 = $10;
    CHANNEL9_CCL_LUT1 = $11;
    CHANNEL9_CCL_LUT2 = $12;
    CHANNEL9_CCL_LUT3 = $13;
    CHANNEL9_CCL_LUT4 = $14;
    CHANNEL9_CCL_LUT5 = $15;
    CHANNEL9_AC0_OUT = $20;
    CHANNEL9_AC1_OUT = $21;
    CHANNEL9_AC2_OUT = $22;
    CHANNEL9_ADC0_RESRDY = $24;
    CHANNEL9_PTC_RESRDY = $28;
    CHANNEL9_ZCD0 = $30;
    CHANNEL9_ZCD1 = $31;
    CHANNEL9_USART0_XCK = $60;
    CHANNEL9_USART1_XCK = $61;
    CHANNEL9_USART2_XCK = $62;
    CHANNEL9_USART3_XCK = $63;
    CHANNEL9_USART4_XCK = $64;
    CHANNEL9_SPI0_SCK = $68;
    CHANNEL9_SPI1_SCK = $69;
    CHANNEL9_TCA0_OVF_LUNF = $80;
    CHANNEL9_TCA0_HUNF = $81;
    CHANNEL9_TCA0_CMP0_LCMP0 = $84;
    CHANNEL9_TCA0_CMP1_LCMP1 = $85;
    CHANNEL9_TCA0_CMP2_LCMP2 = $86;
    CHANNEL9_TCA1_OVF_LUNF = $88;
    CHANNEL9_TCA1_HUNF = $89;
    CHANNEL9_TCA1_CMP0_LCMP0 = $8C;
    CHANNEL9_TCA1_CMP1_LCMP1 = $8D;
    CHANNEL9_TCA1_CMP2_LCMP2 = $8E;
    CHANNEL9_TCB0_CAPT = $A0;
    CHANNEL9_TCB0_OVF = $A1;
    CHANNEL9_TCB1_CAPT = $A2;
    CHANNEL9_TCB1_OVF = $A3;
    CHANNEL9_TCB2_CAPT = $A4;
    CHANNEL9_TCB2_OVF = $A5;
    CHANNEL9_TCB3_CAPT = $A6;
    CHANNEL9_TCB3_OVF = $A7;
    CHANNEL9_TCD0_CMPBCLR = $B0;
    CHANNEL9_TCD0_CMPASET = $B1;
    CHANNEL9_TCD0_CMPBSET = $B2;
    CHANNEL9_TCD0_PROGEV = $B3;
    // EVSYS_USER
    USERmask = $FF;
    USER_OFF = $00;
    USER_CHANNEL0 = $01;
    USER_CHANNEL1 = $02;
    USER_CHANNEL2 = $03;
    USER_CHANNEL3 = $04;
    USER_CHANNEL4 = $05;
    USER_CHANNEL5 = $06;
    USER_CHANNEL6 = $07;
    USER_CHANNEL7 = $08;
    USER_CHANNEL8 = $09;
    USER_CHANNEL9 = $0A;
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
    ACTIVE_ENABLE = $04;
    ACTIVE_SAMPLE = $08;
    ACTIVE_ENABLEWAIT = $0C;
    // FUSE_SAMPFREQ
    SAMPFREQmask = $10;
    SAMPFREQ_128Hz = $00;
    SAMPFREQ_32Hz = $10;
    // FUSE_LVL
    LVLmask = $E0;
    LVL_BODLEVEL0 = $00;
    LVL_BODLEVEL1 = $20;
    LVL_BODLEVEL2 = $40;
    LVL_BODLEVEL3 = $60;
    // FUSE_CLKSEL
    CLKSELmask = $07;
    CLKSEL_OSCHF = $00;
    CLKSEL_OSC32K = $01;
    // EEPROM Save
    EESAVEbm = $01;
    // FUSE_RSTPINCFG
    RSTPINCFGmask = $0C;
    RSTPINCFG_GPIO = $00;
    RSTPINCFG_RST = $08;
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
    STATUS: byte;  //Status
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    Reserved5: byte;
    DATA: word;  //Data
    ADDR: dword;  //Address
  const
    // NVMCTRL_CMD
    CMDmask = $7F;
    CMD_NONE = $00;
    CMD_NOOP = $01;
    CMD_FLWR = $02;
    CMD_FLPER = $08;
    CMD_FLMPER2 = $09;
    CMD_FLMPER4 = $0A;
    CMD_FLMPER8 = $0B;
    CMD_FLMPER16 = $0C;
    CMD_FLMPER32 = $0D;
    CMD_EEWR = $12;
    CMD_EEERWR = $13;
    CMD_EEBER = $18;
    CMD_EEMBER2 = $19;
    CMD_EEMBER4 = $1A;
    CMD_EEMBER8 = $1B;
    CMD_EEMBER16 = $1C;
    CMD_EEMBER32 = $1D;
    CMD_CHER = $20;
    CMD_EECHER = $30;
    // Application Code Write Protect
    APPCODEWPbm = $01;
    // Boot Read Protect
    BOOTRPbm = $02;
    // Application Data Write Protect
    APPDATAWPbm = $04;
    // NVMCTRL_FLMAP
    FLMAPmask = $30;
    FLMAP_SECTION0 = $00;
    FLMAP_SECTION1 = $10;
    FLMAP_SECTION2 = $20;
    FLMAP_SECTION3 = $30;
    // Flash Mapping Lock
    FLMAPLOCKbm = $80;
    // Flash busy
    FBUSYbm = $01;
    // EEPROM busy
    EEBUSYbm = $02;
    // NVMCTRL_ERROR
    ERRORmask = $70;
    ERROR_NOERROR = $00;
    ERROR_ILLEGALCMD = $10;
    ERROR_ILLEGALSADDR = $20;
    ERROR_DOUBLESELECT = $30;
    ERROR_ONGOINGPROG = $40;
    // EEPROM Ready
    EEREADYbm = $01;
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
    // Inverted I/O Enable
    INVENbm = $80;
  end;

  TPORTMUX = object //Port Multiplexer
    EVSYSROUTEA: byte;  //EVSYS route A
    CCLROUTEA: byte;  //CCL route A
    USARTROUTEA: byte;  //USART route A
    USARTROUTEB: byte;  //USART route B
    SPIROUTEA: byte;  //SPI route A
    TWIROUTEA: byte;  //TWI route A
    TCAROUTEA: byte;  //TCA route A
    TCBROUTEA: byte;  //TCB route A
    TCDROUTEA: byte;  //TCD route A
    ACROUTEA: byte;  //AC route A
    ZCDROUTEA: byte;  //ZCD route A
  const
    // PORTMUX_EVOUTA
    EVOUTAmask = $01;
    EVOUTA_DEFAULT = $00;
    EVOUTA_ALT1 = $01;
    // PORTMUX_EVOUTB
    EVOUTBmask = $02;
    EVOUTB_DEFAULT = $00;
    // PORTMUX_EVOUTC
    EVOUTCmask = $04;
    EVOUTC_DEFAULT = $00;
    EVOUTC_ALT1 = $04;
    // PORTMUX_EVOUTD
    EVOUTDmask = $08;
    EVOUTD_DEFAULT = $00;
    EVOUTD_ALT1 = $08;
    // PORTMUX_EVOUTE
    EVOUTEmask = $10;
    EVOUTE_DEFAULT = $00;
    // PORTMUX_EVOUTF
    EVOUTFmask = $20;
    EVOUTF_DEFAULT = $00;
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
    // PORTMUX_LUT3
    LUT3mask = $08;
    LUT3_DEFAULT = $00;
    // PORTMUX_LUT4
    LUT4mask = $10;
    LUT4_DEFAULT = $00;
    LUT4_ALT1 = $10;
    // PORTMUX_USART0
    USART0mask = $03;
    USART0_DEFAULT = $00;
    USART0_ALT1 = $01;
    USART0_NONE = $03;
    // PORTMUX_USART1
    USART1mask = $0C;
    USART1_DEFAULT = $00;
    USART1_ALT1 = $04;
    USART1_NONE = $0C;
    // PORTMUX_USART2
    USART2mask = $30;
    USART2_DEFAULT = $00;
    USART2_ALT1 = $10;
    USART2_NONE = $30;
    // PORTMUX_USART3
    USART3mask = $C0;
    USART3_DEFAULT = $00;
    USART3_ALT1 = $40;
    USART3_NONE = $C0;
    // PORTMUX_USART4
    USART4mask = $03;
    USART4_DEFAULT = $00;
    USART4_NONE = $03;
    // PORTMUX_SPI0
    SPI0mask = $03;
    SPI0_DEFAULT = $00;
    SPI0_ALT1 = $01;
    SPI0_NONE = $03;
    // PORTMUX_SPI1
    SPI1mask = $0C;
    SPI1_DEFAULT = $00;
    SPI1_ALT1 = $04;
    SPI1_ALT2 = $08;
    SPI1_NONE = $0C;
    // PORTMUX_TWI0
    TWI0mask = $03;
    TWI0_DEFAULT = $00;
    TWI0_ALT1 = $01;
    TWI0_ALT2 = $02;
    // PORTMUX_TWI1
    TWI1mask = $0C;
    TWI1_DEFAULT = $00;
    TWI1_ALT1 = $04;
    TWI1_ALT2 = $08;
    // PORTMUX_TCA0
    TCA0mask = $07;
    TCA0_PORTA = $00;
    TCA0_PORTB = $01;
    TCA0_PORTC = $02;
    TCA0_PORTD = $03;
    TCA0_PORTE = $04;
    TCA0_PORTF = $05;
    // PORTMUX_TCA1
    TCA1mask = $38;
    TCA1_PORTB = $00;
    TCA1_PORTC = $08;
    // PORTMUX_TCB0
    TCB0mask = $01;
    TCB0_DEFAULT = $00;
    TCB0_ALT1 = $01;
    // PORTMUX_TCB1
    TCB1mask = $02;
    TCB1_DEFAULT = $00;
    TCB1_ALT1 = $02;
    // PORTMUX_TCB2
    TCB2mask = $04;
    TCB2_DEFAULT = $00;
    TCB2_ALT1 = $04;
    // PORTMUX_TCB3
    TCB3mask = $08;
    TCB3_DEFAULT = $00;
    TCB3_ALT1 = $08;
    // PORTMUX_TCD0
    TCD0mask = $07;
    TCD0_DEFAULT = $00;
    TCD0_ALT1 = $01;
    TCD0_ALT2 = $02;
    // PORTMUX_AC0
    AC0mask = $01;
    AC0_DEFAULT = $00;
    AC0_ALT1 = $01;
    // PORTMUX_AC1
    AC1mask = $02;
    AC1_DEFAULT = $00;
    AC1_ALT1 = $02;
    // PORTMUX_AC2
    AC2mask = $04;
    AC2_DEFAULT = $00;
    AC2_ALT1 = $04;
    // PORTMUX_ZCD0
    ZCD0mask = $01;
    ZCD0_DEFAULT = $00;
    ZCD0_ALT1 = $01;
    // PORTMUX_ZCD1
    ZCD1mask = $02;
    ZCD1_DEFAULT = $00;
    ZCD1_ALT1 = $02;
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
    // Software reset enable
    SWRSTbm = $01;
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
    SERNUM0: byte;  //LOTNUM0
    SERNUM1: byte;  //LOTNUM1
    SERNUM2: byte;  //LOTNUM2
    SERNUM3: byte;  //LOTNUM3
    SERNUM4: byte;  //LOTNUM4
    SERNUM5: byte;  //LOTNUM5
    SERNUM6: byte;  //RANDOM
    SERNUM7: byte;  //SCRIBE
    SERNUM8: byte;  //XPOS0
    SERNUM9: byte;  //XPOS1
    SERNUM10: byte;  //YPOS0
    SERNUM11: byte;  //YPOS1
    SERNUM12: byte;  //RES0
    SERNUM13: byte;  //RES1
    SERNUM14: byte;  //RES2
    SERNUM15: byte;  //RES3
  end;

  TSLPCTRL = object //Sleep Controller
    CTRLA: byte;  //Control A
    VREGCTRL: byte;  //Control B
  const
    // Sleep enable
    SENbm = $01;
    // SLPCTRL_SMODE
    SMODEmask = $06;
    SMODE_IDLE = $00;
    SMODE_STDBY = $02;
    SMODE_PDOWN = $04;
    // SLPCTRL_PMODE
    PMODEmask = $07;
    PMODE_AUTO = $00;
    PMODE_FULL = $01;
    // SLPCTRL_HTLLEN
    HTLLENmask = $10;
    HTLLEN_OFF = $00;
    HTLLEN_ON = $10;
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
    Reserved16: byte;
    Reserved17: byte;
    Reserved18: byte;
    Reserved19: byte;
    Reserved20: byte;
    Reserved21: byte;
    Reserved22: byte;
    Reserved23: byte;
    OCDMCTRL: byte;  //OCD Message Control
    OCDMSTATUS: byte;  //OCD Message Status
  const
    // OCD Message Read
    OCDMRbm = $01;
  end;

  TTCA = object //16-bit Timer/Counter Type A
  end;

  TTCB = object //16-bit Timer Type B
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control Register B
    Reserved2: byte;
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
    CLKSEL_TCA0 = $04;
    CLKSEL_TCA1 = $06;
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

  TTCD = object //Timer Counter D
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    CTRLC: byte;  //Control C
    CTRLD: byte;  //Control D
    CTRLE: byte;  //Control E
    Reserved5: byte;
    Reserved6: byte;
    Reserved7: byte;
    EVCTRLA: byte;  //EVCTRLA
    EVCTRLB: byte;  //EVCTRLB
    Reserved10: byte;
    Reserved11: byte;
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    STATUS: byte;  //Status
    Reserved15: byte;
    INPUTCTRLA: byte;  //Input Control A
    INPUTCTRLB: byte;  //Input Control B
    FAULTCTRL: byte;  //Fault Control
    Reserved19: byte;
    DLYCTRL: byte;  //Delay Control
    DLYVAL: byte;  //Delay value
    Reserved22: byte;
    Reserved23: byte;
    DITCTRL: byte;  //Dither Control A
    DITVAL: byte;  //Dither value
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    DBGCTRL: byte;  //Debug Control
    Reserved31: byte;
    Reserved32: byte;
    Reserved33: byte;
    CAPTUREA: word;  //Capture A
    CAPTUREB: word;  //Capture B
    Reserved38: byte;
    Reserved39: byte;
    CMPASET: word;  //Compare A Set
    CMPACLR: word;  //Compare A Clear
    CMPBSET: word;  //Compare B Set
    CMPBCLR: word;  //Compare B Clear
  const
    // Enable
    ENABLEbm = $01;
    // TCD_SYNCPRES
    SYNCPRESmask = $06;
    SYNCPRES_DIV1 = $00;
    SYNCPRES_DIV2 = $02;
    SYNCPRES_DIV4 = $04;
    SYNCPRES_DIV8 = $06;
    // TCD_CNTPRES
    CNTPRESmask = $18;
    CNTPRES_DIV1 = $00;
    CNTPRES_DIV4 = $08;
    CNTPRES_DIV32 = $10;
    // TCD_CLKSEL
    CLKSELmask = $60;
    CLKSEL_OSCHF = $00;
    CLKSEL_PLL = $20;
    CLKSEL_EXTCLK = $40;
    CLKSEL_CLKPER = $60;
    // TCD_WGMODE
    WGMODEmask = $03;
    WGMODE_ONERAMP = $00;
    WGMODE_TWORAMP = $01;
    WGMODE_FOURRAMP = $02;
    WGMODE_DS = $03;
    // Compare output value override
    CMPOVRbm = $01;
    // Auto update
    AUPDATEbm = $02;
    // Fifty percent waveform
    FIFTYbm = $08;
    // TCD_CMPCSEL
    CMPCSELmask = $40;
    CMPCSEL_PWMA = $00;
    CMPCSEL_PWMB = $40;
    // TCD_CMPDSEL
    CMPDSELmask = $80;
    CMPDSEL_PWMA = $00;
    CMPDSEL_PWMB = $80;
    // Compare A value
    CMPAVAL0bm = $01;
    CMPAVAL1bm = $02;
    CMPAVAL2bm = $04;
    CMPAVAL3bm = $08;
    // Compare B value
    CMPBVAL0bm = $10;
    CMPBVAL1bm = $20;
    CMPBVAL2bm = $40;
    CMPBVAL3bm = $80;
    // Synchronize end of cycle strobe
    SYNCEOCbm = $01;
    // synchronize strobe
    SYNCbm = $02;
    // Restart strobe
    RESTARTbm = $04;
    // Software Capture A Strobe
    SCAPTUREAbm = $08;
    // Software Capture B Strobe
    SCAPTUREBbm = $10;
    // Disable at end of cycle
    DISEOCbm = $80;
    // Trigger event enable
    TRIGEIbm = $01;
    // TCD_ACTION
    ACTIONmask = $04;
    ACTION_FAULT = $00;
    ACTION_CAPTURE = $04;
    // TCD_EDGE
    EDGEmask = $10;
    EDGE_FALL_LOW = $00;
    EDGE_RISE_HIGH = $10;
    // TCD_CFG
    CFGmask = $C0;
    CFG_NEITHER = $00;
    CFG_FILTER = $40;
    CFG_ASYNC = $80;
    // Overflow interrupt enable
    OVFbm = $01;
    // Trigger A interrupt enable
    TRIGAbm = $04;
    // Trigger B interrupt enable
    TRIGBbm = $08;
    // Enable ready
    ENRDYbm = $01;
    // Command ready
    CMDRDYbm = $02;
    // PWM activity on A
    PWMACTAbm = $40;
    // PWM activity on B
    PWMACTBbm = $80;
    // TCD_INPUTMODE
    INPUTMODEmask = $0F;
    INPUTMODE_NONE = $00;
    INPUTMODE_JMPWAIT = $01;
    INPUTMODE_EXECWAIT = $02;
    INPUTMODE_EXECFAULT = $03;
    INPUTMODE_FREQ = $04;
    INPUTMODE_EXECDT = $05;
    INPUTMODE_WAIT = $06;
    INPUTMODE_WAITSW = $07;
    INPUTMODE_EDGETRIG = $08;
    INPUTMODE_EDGETRIGFREQ = $09;
    INPUTMODE_LVLTRIGFREQ = $0A;
    // Compare A value
    CMPAbm = $01;
    // Compare B value
    CMPBbm = $02;
    // Compare C value
    CMPCbm = $04;
    // Compare D vaule
    CMPDbm = $08;
    // Compare A enable
    CMPAENbm = $10;
    // Compare B enable
    CMPBENbm = $20;
    // Compare C enable
    CMPCENbm = $40;
    // Compare D enable
    CMPDENbm = $80;
    // TCD_DLYSEL
    DLYSELmask = $03;
    DLYSEL_OFF = $00;
    DLYSEL_INBLANK = $01;
    DLYSEL_EVENT = $02;
    // TCD_DLYTRIG
    DLYTRIGmask = $0C;
    DLYTRIG_CMPASET = $00;
    DLYTRIG_CMPACLR = $04;
    DLYTRIG_CMPBSET = $08;
    DLYTRIG_CMPBCLR = $0C;
    // TCD_DLYPRESC
    DLYPRESCmask = $30;
    DLYPRESC_DIV1 = $00;
    DLYPRESC_DIV2 = $10;
    DLYPRESC_DIV4 = $20;
    DLYPRESC_DIV8 = $30;
    // TCD_DITHERSEL
    DITHERSELmask = $03;
    DITHERSEL_ONTIMEB = $00;
    DITHERSEL_ONTIMEAB = $01;
    DITHERSEL_DEADTIMEB = $02;
    DITHERSEL_DEADTIMEAB = $03;
    // Dither value
    DITHER0bm = $01;
    DITHER1bm = $02;
    DITHER2bm = $04;
    DITHER3bm = $08;
    // Debug run
    DBGRUNbm = $01;
    // Fault detection
    FAULTDETbm = $04;
  end;

  TTWI = object //Two-Wire Interface
    CTRLA: byte;  //Control A
    DUALCTRL: byte;  //Dual Control
    DBGCTRL: byte;  //Debug Control Register
    MCTRLA: byte;  //Host Control A
    MCTRLB: byte;  //Host Control B
    MSTATUS: byte;  //Host Status
    MBAUD: byte;  //Host Baud Rate Control
    MADDR: byte;  //Host Address
    MDATA: byte;  //Host Data
    SCTRLA: byte;  //Client Control A
    SCTRLB: byte;  //Client Control B
    SSTATUS: byte;  //Client Status
    SADDR: byte;  //Client Address
    SDATA: byte;  //Client Data
    SADDRMASK: byte;  //Client Address Mask
  const
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
    // Dual Control Enable
    ENABLEbm = $01;
    // Debug Run
    DBGRUNbm = $01;
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
    // Promiscuous Mode Enable
    PMENbm = $04;
    // Stop Interrupt Enable
    PIENbm = $20;
    // Address/Stop Interrupt Enable
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
    // Address/Stop Interrupt Flag
    APIFbm = $40;
    // Data Interrupt Flag
    DIFbm = $80;
    // Address Enable
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
  end;

  TVPORT = object //Virtual Ports
    DIR: byte;  //Data Direction
    OUT_: byte;  //Output Value
    IN_: byte;  //Input Value
    INTFLAGS: byte;  //Interrupt Flags
  end;

  TVREF = object //Voltage reference
    ADC0REF: byte;  //ADC0 Reference
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
    // Syncronization busy
    SYNCBUSYbm = $01;
    // Lock enable
    LOCKbm = $80;
  end;

  TZCD = object //Zero Cross Detect
    CTRLA: byte;  //Control A
    Reserved1: byte;
    INTCTRL: byte;  //Interrupt Control
    STATUS: byte;  //Status
  const
    // Enable
    ENABLEbm = $01;
    // Invert signal from pin
    INVERTbm = $08;
    // Output Pad Enable
    OUTENbm = $40;
    // Run in Standby Mode
    RUNSTDBYbm = $80;
    // ZCD_INTMODE
    INTMODEmask = $03;
    INTMODE_NONE = $00;
    INTMODE_RISING = $01;
    INTMODE_FALLING = $02;
    INTMODE_BOTH = $03;
    // ZCD Interrupt Flag
    CROSSIFbm = $01;
    // ZCD_STATE
    STATEmask = $10;
    STATE_LOW = $00;
    STATE_HIGH = $10;
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
  VPORTB: TVPORT absolute $0004;
  VPORTC: TVPORT absolute $0008;
  VPORTD: TVPORT absolute $000C;
  VPORTE: TVPORT absolute $0010;
  VPORTF: TVPORT absolute $0014;
  GPR: TGPR absolute $001C;
  CPU: TCPU absolute $0030;
  RSTCTRL: TRSTCTRL absolute $0040;
  SLPCTRL: TSLPCTRL absolute $0050;
  CLKCTRL: TCLKCTRL absolute $0060;
  BOD: TBOD absolute $0080;
  VREF: TVREF absolute $00A0;
  WDT: TWDT absolute $0100;
  CPUINT: TCPUINT absolute $0110;
  CRCSCAN: TCRCSCAN absolute $0120;
  RTC: TRTC absolute $0140;
  CCL: TCCL absolute $01C0;
  EVSYS: TEVSYS absolute $0200;
  PORTA: TPORT absolute $0400;
  PORTB: TPORT absolute $0420;
  PORTC: TPORT absolute $0440;
  PORTD: TPORT absolute $0460;
  PORTE: TPORT absolute $0480;
  PORTF: TPORT absolute $04A0;
  PORTMUX: TPORTMUX absolute $05E0;
  ADC0: TADC absolute $0600;
  AC0: TAC absolute $0680;
  AC1: TAC absolute $0688;
  AC2: TAC absolute $0690;
  DAC0: TDAC absolute $06A0;
  ZCD0: TZCD absolute $06C0;
  ZCD1: TZCD absolute $06C8;
  USART0: TUSART absolute $0800;
  USART1: TUSART absolute $0820;
  USART2: TUSART absolute $0840;
  USART3: TUSART absolute $0860;
  USART4: TUSART absolute $0880;
  TWI0: TTWI absolute $0900;
  TWI1: TTWI absolute $0920;
  SPI0: TSPI absolute $0940;
  SPI1: TSPI absolute $0960;
  TCA0: TTCA absolute $0A00;
  TCA1: TTCA absolute $0A40;
  TCB0: TTCB absolute $0B00;
  TCB1: TTCB absolute $0B10;
  TCB2: TTCB absolute $0B20;
  TCB3: TTCB absolute $0B30;
  TCB4: TTCB absolute $0B40;
  TCD0: TTCD absolute $0B80;
  SYSCFG: TSYSCFG absolute $0F00;
  NVMCTRL: TNVMCTRL absolute $1000;
  LOCK: TLOCK absolute $1040;
  FUSE: TFUSE absolute $1050;
  USERROW: TUSERROW absolute $1080;
  SIGROW: TSIGROW absolute $1100;

implementation

{$i avrcommon.inc}

procedure CRCSCAN_NMI_ISR; external name 'CRCSCAN_NMI_ISR'; // Interrupt 1 
procedure BOD_VLM_ISR; external name 'BOD_VLM_ISR'; // Interrupt 2 
procedure RTC_CNT_ISR; external name 'RTC_CNT_ISR'; // Interrupt 3 
procedure RTC_PIT_ISR; external name 'RTC_PIT_ISR'; // Interrupt 4 
procedure CCL_CCL_ISR; external name 'CCL_CCL_ISR'; // Interrupt 5 
procedure PORTA_PORT_ISR; external name 'PORTA_PORT_ISR'; // Interrupt 6 
procedure TCA0_LUNF_ISR; external name 'TCA0_LUNF_ISR'; // Interrupt 7 
//procedure TCA0_OVF_ISR; external name 'TCA0_OVF_ISR'; // Interrupt 7 
procedure TCA0_HUNF_ISR; external name 'TCA0_HUNF_ISR'; // Interrupt 8 
procedure TCA0_CMP0_ISR; external name 'TCA0_CMP0_ISR'; // Interrupt 9 
//procedure TCA0_LCMP0_ISR; external name 'TCA0_LCMP0_ISR'; // Interrupt 9 
procedure TCA0_CMP1_ISR; external name 'TCA0_CMP1_ISR'; // Interrupt 10 
//procedure TCA0_LCMP1_ISR; external name 'TCA0_LCMP1_ISR'; // Interrupt 10 
procedure TCA0_CMP2_ISR; external name 'TCA0_CMP2_ISR'; // Interrupt 11 
//procedure TCA0_LCMP2_ISR; external name 'TCA0_LCMP2_ISR'; // Interrupt 11 
procedure TCB0_INT_ISR; external name 'TCB0_INT_ISR'; // Interrupt 12 
procedure TCB1_INT_ISR; external name 'TCB1_INT_ISR'; // Interrupt 13 
procedure TCD0_OVF_ISR; external name 'TCD0_OVF_ISR'; // Interrupt 14 
procedure TCD0_TRIG_ISR; external name 'TCD0_TRIG_ISR'; // Interrupt 15 
procedure TWI0_TWIS_ISR; external name 'TWI0_TWIS_ISR'; // Interrupt 16 
procedure TWI0_TWIM_ISR; external name 'TWI0_TWIM_ISR'; // Interrupt 17 
procedure SPI0_INT_ISR; external name 'SPI0_INT_ISR'; // Interrupt 18 
procedure USART0_RXC_ISR; external name 'USART0_RXC_ISR'; // Interrupt 19 
procedure USART0_DRE_ISR; external name 'USART0_DRE_ISR'; // Interrupt 20 
procedure USART0_TXC_ISR; external name 'USART0_TXC_ISR'; // Interrupt 21 
procedure PORTD_PORT_ISR; external name 'PORTD_PORT_ISR'; // Interrupt 22 
procedure AC0_AC_ISR; external name 'AC0_AC_ISR'; // Interrupt 23 
procedure ADC0_RESRDY_ISR; external name 'ADC0_RESRDY_ISR'; // Interrupt 24 
procedure ADC0_WCMP_ISR; external name 'ADC0_WCMP_ISR'; // Interrupt 25 
procedure ZCD0_ZCD_ISR; external name 'ZCD0_ZCD_ISR'; // Interrupt 26 
procedure PTC_PTC_ISR; external name 'PTC_PTC_ISR'; // Interrupt 27 
procedure AC1_AC_ISR; external name 'AC1_AC_ISR'; // Interrupt 28 
procedure PORTC_PORT_ISR; external name 'PORTC_PORT_ISR'; // Interrupt 29 
procedure TCB2_INT_ISR; external name 'TCB2_INT_ISR'; // Interrupt 30 
procedure USART1_RXC_ISR; external name 'USART1_RXC_ISR'; // Interrupt 31 
procedure USART1_DRE_ISR; external name 'USART1_DRE_ISR'; // Interrupt 32 
procedure USART1_TXC_ISR; external name 'USART1_TXC_ISR'; // Interrupt 33 
procedure PORTF_PORT_ISR; external name 'PORTF_PORT_ISR'; // Interrupt 34 
procedure NVMCTRL_EE_ISR; external name 'NVMCTRL_EE_ISR'; // Interrupt 35 
procedure SPI1_INT_ISR; external name 'SPI1_INT_ISR'; // Interrupt 36 
procedure USART2_RXC_ISR; external name 'USART2_RXC_ISR'; // Interrupt 37 
procedure USART2_DRE_ISR; external name 'USART2_DRE_ISR'; // Interrupt 38 
procedure USART2_TXC_ISR; external name 'USART2_TXC_ISR'; // Interrupt 39 
procedure AC2_AC_ISR; external name 'AC2_AC_ISR'; // Interrupt 40 
procedure TCB3_INT_ISR; external name 'TCB3_INT_ISR'; // Interrupt 41 
procedure TWI1_TWIS_ISR; external name 'TWI1_TWIS_ISR'; // Interrupt 42 
procedure TWI1_TWIM_ISR; external name 'TWI1_TWIM_ISR'; // Interrupt 43 
procedure PORTB_PORT_ISR; external name 'PORTB_PORT_ISR'; // Interrupt 44 
procedure PORTE_PORT_ISR; external name 'PORTE_PORT_ISR'; // Interrupt 45 
procedure TCA1_LUNF_ISR; external name 'TCA1_LUNF_ISR'; // Interrupt 46 
//procedure TCA1_OVF_ISR; external name 'TCA1_OVF_ISR'; // Interrupt 46 
procedure TCA1_HUNF_ISR; external name 'TCA1_HUNF_ISR'; // Interrupt 47 
procedure TCA1_CMP0_ISR; external name 'TCA1_CMP0_ISR'; // Interrupt 48 
//procedure TCA1_LCMP0_ISR; external name 'TCA1_LCMP0_ISR'; // Interrupt 48 
procedure TCA1_CMP1_ISR; external name 'TCA1_CMP1_ISR'; // Interrupt 49 
//procedure TCA1_LCMP1_ISR; external name 'TCA1_LCMP1_ISR'; // Interrupt 49 
procedure TCA1_CMP2_ISR; external name 'TCA1_CMP2_ISR'; // Interrupt 50 
//procedure TCA1_LCMP2_ISR; external name 'TCA1_LCMP2_ISR'; // Interrupt 50 
procedure ZCD1_ZCD_ISR; external name 'ZCD1_ZCD_ISR'; // Interrupt 51 
procedure USART3_RXC_ISR; external name 'USART3_RXC_ISR'; // Interrupt 52 
procedure USART3_DRE_ISR; external name 'USART3_DRE_ISR'; // Interrupt 53 
procedure USART3_TXC_ISR; external name 'USART3_TXC_ISR'; // Interrupt 54 
procedure USART4_RXC_ISR; external name 'USART4_RXC_ISR'; // Interrupt 55 
procedure USART4_DRE_ISR; external name 'USART4_DRE_ISR'; // Interrupt 56 
procedure USART4_TXC_ISR; external name 'USART4_TXC_ISR'; // Interrupt 57 
procedure TCB4_INT_ISR; external name 'TCB4_INT_ISR'; // Interrupt 60 

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
asm
  jmp __dtors_end
  jmp CRCSCAN_NMI_ISR
  jmp BOD_VLM_ISR
  jmp RTC_CNT_ISR
  jmp RTC_PIT_ISR
  jmp CCL_CCL_ISR
  jmp PORTA_PORT_ISR
  jmp TCA0_LUNF_ISR
//  jmp TCA0_OVF_ISR
  jmp TCA0_HUNF_ISR
  jmp TCA0_CMP0_ISR
//  jmp TCA0_LCMP0_ISR
  jmp TCA0_CMP1_ISR
//  jmp TCA0_LCMP1_ISR
  jmp TCA0_CMP2_ISR
//  jmp TCA0_LCMP2_ISR
  jmp TCB0_INT_ISR
  jmp TCB1_INT_ISR
  jmp TCD0_OVF_ISR
  jmp TCD0_TRIG_ISR
  jmp TWI0_TWIS_ISR
  jmp TWI0_TWIM_ISR
  jmp SPI0_INT_ISR
  jmp USART0_RXC_ISR
  jmp USART0_DRE_ISR
  jmp USART0_TXC_ISR
  jmp PORTD_PORT_ISR
  jmp AC0_AC_ISR
  jmp ADC0_RESRDY_ISR
  jmp ADC0_WCMP_ISR
  jmp ZCD0_ZCD_ISR
  jmp PTC_PTC_ISR
  jmp AC1_AC_ISR
  jmp PORTC_PORT_ISR
  jmp TCB2_INT_ISR
  jmp USART1_RXC_ISR
  jmp USART1_DRE_ISR
  jmp USART1_TXC_ISR
  jmp PORTF_PORT_ISR
  jmp NVMCTRL_EE_ISR
  jmp SPI1_INT_ISR
  jmp USART2_RXC_ISR
  jmp USART2_DRE_ISR
  jmp USART2_TXC_ISR
  jmp AC2_AC_ISR
  jmp TCB3_INT_ISR
  jmp TWI1_TWIS_ISR
  jmp TWI1_TWIM_ISR
  jmp PORTB_PORT_ISR
  jmp PORTE_PORT_ISR
  jmp TCA1_LUNF_ISR
//  jmp TCA1_OVF_ISR
  jmp TCA1_HUNF_ISR
  jmp TCA1_CMP0_ISR
//  jmp TCA1_LCMP0_ISR
  jmp TCA1_CMP1_ISR
//  jmp TCA1_LCMP1_ISR
  jmp TCA1_CMP2_ISR
//  jmp TCA1_LCMP2_ISR
  jmp ZCD1_ZCD_ISR
  jmp USART3_RXC_ISR
  jmp USART3_DRE_ISR
  jmp USART3_TXC_ISR
  jmp USART4_RXC_ISR
  jmp USART4_DRE_ISR
  jmp USART4_TXC_ISR
  jmp TCB4_INT_ISR

  .weak CRCSCAN_NMI_ISR
  .weak BOD_VLM_ISR
  .weak RTC_CNT_ISR
  .weak RTC_PIT_ISR
  .weak CCL_CCL_ISR
  .weak PORTA_PORT_ISR
  .weak TCA0_LUNF_ISR
//  .weak TCA0_OVF_ISR
  .weak TCA0_HUNF_ISR
  .weak TCA0_CMP0_ISR
//  .weak TCA0_LCMP0_ISR
  .weak TCA0_CMP1_ISR
//  .weak TCA0_LCMP1_ISR
  .weak TCA0_CMP2_ISR
//  .weak TCA0_LCMP2_ISR
  .weak TCB0_INT_ISR
  .weak TCB1_INT_ISR
  .weak TCD0_OVF_ISR
  .weak TCD0_TRIG_ISR
  .weak TWI0_TWIS_ISR
  .weak TWI0_TWIM_ISR
  .weak SPI0_INT_ISR
  .weak USART0_RXC_ISR
  .weak USART0_DRE_ISR
  .weak USART0_TXC_ISR
  .weak PORTD_PORT_ISR
  .weak AC0_AC_ISR
  .weak ADC0_RESRDY_ISR
  .weak ADC0_WCMP_ISR
  .weak ZCD0_ZCD_ISR
  .weak PTC_PTC_ISR
  .weak AC1_AC_ISR
  .weak PORTC_PORT_ISR
  .weak TCB2_INT_ISR
  .weak USART1_RXC_ISR
  .weak USART1_DRE_ISR
  .weak USART1_TXC_ISR
  .weak PORTF_PORT_ISR
  .weak NVMCTRL_EE_ISR
  .weak SPI1_INT_ISR
  .weak USART2_RXC_ISR
  .weak USART2_DRE_ISR
  .weak USART2_TXC_ISR
  .weak AC2_AC_ISR
  .weak TCB3_INT_ISR
  .weak TWI1_TWIS_ISR
  .weak TWI1_TWIM_ISR
  .weak PORTB_PORT_ISR
  .weak PORTE_PORT_ISR
  .weak TCA1_LUNF_ISR
//  .weak TCA1_OVF_ISR
  .weak TCA1_HUNF_ISR
  .weak TCA1_CMP0_ISR
//  .weak TCA1_LCMP0_ISR
  .weak TCA1_CMP1_ISR
//  .weak TCA1_LCMP1_ISR
  .weak TCA1_CMP2_ISR
//  .weak TCA1_LCMP2_ISR
  .weak ZCD1_ZCD_ISR
  .weak USART3_RXC_ISR
  .weak USART3_DRE_ISR
  .weak USART3_TXC_ISR
  .weak USART4_RXC_ISR
  .weak USART4_DRE_ISR
  .weak USART4_TXC_ISR
  .weak TCB4_INT_ISR

  .set CRCSCAN_NMI_ISR, Default_IRQ_handler
  .set BOD_VLM_ISR, Default_IRQ_handler
  .set RTC_CNT_ISR, Default_IRQ_handler
  .set RTC_PIT_ISR, Default_IRQ_handler
  .set CCL_CCL_ISR, Default_IRQ_handler
  .set PORTA_PORT_ISR, Default_IRQ_handler
  .set TCA0_LUNF_ISR, Default_IRQ_handler
//  .set TCA0_OVF_ISR, Default_IRQ_handler
  .set TCA0_HUNF_ISR, Default_IRQ_handler
  .set TCA0_CMP0_ISR, Default_IRQ_handler
//  .set TCA0_LCMP0_ISR, Default_IRQ_handler
  .set TCA0_CMP1_ISR, Default_IRQ_handler
//  .set TCA0_LCMP1_ISR, Default_IRQ_handler
  .set TCA0_CMP2_ISR, Default_IRQ_handler
//  .set TCA0_LCMP2_ISR, Default_IRQ_handler
  .set TCB0_INT_ISR, Default_IRQ_handler
  .set TCB1_INT_ISR, Default_IRQ_handler
  .set TCD0_OVF_ISR, Default_IRQ_handler
  .set TCD0_TRIG_ISR, Default_IRQ_handler
  .set TWI0_TWIS_ISR, Default_IRQ_handler
  .set TWI0_TWIM_ISR, Default_IRQ_handler
  .set SPI0_INT_ISR, Default_IRQ_handler
  .set USART0_RXC_ISR, Default_IRQ_handler
  .set USART0_DRE_ISR, Default_IRQ_handler
  .set USART0_TXC_ISR, Default_IRQ_handler
  .set PORTD_PORT_ISR, Default_IRQ_handler
  .set AC0_AC_ISR, Default_IRQ_handler
  .set ADC0_RESRDY_ISR, Default_IRQ_handler
  .set ADC0_WCMP_ISR, Default_IRQ_handler
  .set ZCD0_ZCD_ISR, Default_IRQ_handler
  .set PTC_PTC_ISR, Default_IRQ_handler
  .set AC1_AC_ISR, Default_IRQ_handler
  .set PORTC_PORT_ISR, Default_IRQ_handler
  .set TCB2_INT_ISR, Default_IRQ_handler
  .set USART1_RXC_ISR, Default_IRQ_handler
  .set USART1_DRE_ISR, Default_IRQ_handler
  .set USART1_TXC_ISR, Default_IRQ_handler
  .set PORTF_PORT_ISR, Default_IRQ_handler
  .set NVMCTRL_EE_ISR, Default_IRQ_handler
  .set SPI1_INT_ISR, Default_IRQ_handler
  .set USART2_RXC_ISR, Default_IRQ_handler
  .set USART2_DRE_ISR, Default_IRQ_handler
  .set USART2_TXC_ISR, Default_IRQ_handler
  .set AC2_AC_ISR, Default_IRQ_handler
  .set TCB3_INT_ISR, Default_IRQ_handler
  .set TWI1_TWIS_ISR, Default_IRQ_handler
  .set TWI1_TWIM_ISR, Default_IRQ_handler
  .set PORTB_PORT_ISR, Default_IRQ_handler
  .set PORTE_PORT_ISR, Default_IRQ_handler
  .set TCA1_LUNF_ISR, Default_IRQ_handler
//  .set TCA1_OVF_ISR, Default_IRQ_handler
  .set TCA1_HUNF_ISR, Default_IRQ_handler
  .set TCA1_CMP0_ISR, Default_IRQ_handler
//  .set TCA1_LCMP0_ISR, Default_IRQ_handler
  .set TCA1_CMP1_ISR, Default_IRQ_handler
//  .set TCA1_LCMP1_ISR, Default_IRQ_handler
  .set TCA1_CMP2_ISR, Default_IRQ_handler
//  .set TCA1_LCMP2_ISR, Default_IRQ_handler
  .set ZCD1_ZCD_ISR, Default_IRQ_handler
  .set USART3_RXC_ISR, Default_IRQ_handler
  .set USART3_DRE_ISR, Default_IRQ_handler
  .set USART3_TXC_ISR, Default_IRQ_handler
  .set USART4_RXC_ISR, Default_IRQ_handler
  .set USART4_DRE_ISR, Default_IRQ_handler
  .set USART4_TXC_ISR, Default_IRQ_handler
  .set TCB4_INT_ISR, Default_IRQ_handler
end;

end.

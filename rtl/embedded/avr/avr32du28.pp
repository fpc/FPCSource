unit AVR32DU28;

interface

type
  TAC = object //Analog Comparator
    CTRLA: byte;  //Control A
    Reserved1: byte;
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
    POWER_PROFILE3 = $18;
    // Output Pad Enable
    OUTENbm = $40;
    // Run in Standby Mode
    RUNSTDBYbm = $80;
    // AC_MUXNEG
    MUXNEGmask = $07;
    MUXNEG_AINN0 = $00;
    MUXNEG_AINN1 = $01;
    MUXNEG_AINN2 = $02;
    MUXNEG_DACREF = $04;
    // AC_MUXPOS
    MUXPOSmask = $38;
    MUXPOS_AINP0 = $00;
    MUXPOS_AINP3 = $18;
    MUXPOS_AINP4 = $20;
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
  end;

  TADC = object //Analog to Digital Converter
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control C
    CTRLC: byte;  //Control B
    CTRLD: byte;  //Control E
    CTRLE: byte;  //Control F
    CTRLF: byte;  //Control D
    INTCTRL: byte;  //Interrupt Control
    INTFLAGS: byte;  //Interrupt Flags
    STATUS: byte;  //Status
    DBGCTRL: byte;  //Debug Control
    COMMAND: byte;  //Command
    MUXPOS: byte;  //Positive mux input
    RESULT: word;  //ADC Accumulator Result
    SAMPLE: word;  //ADC Sample
    WINLT: word;  //Window comparator low threshold
    WINHT: word;  //Window comparator high threshold
    TEMP: byte;  //Temporary Data
  const
    // ADC Enable
    ENABLEbm = $01;
    // Conversion mode
    LOWLATbm = $20;
    // Run standby mode
    RUNSTBYbm = $80;
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
    // Window Mode Source
    WINSRCbm = $08;
    // ADC_SAMPNUM
    SAMPNUMmask = $07;
    SAMPNUM_NONE = $00;
    SAMPNUM_ACC2 = $01;
    SAMPNUM_ACC4 = $02;
    SAMPNUM_ACC8 = $03;
    SAMPNUM_ACC16 = $04;
    SAMPNUM_ACC32 = $05;
    SAMPNUM_ACC64 = $06;
    // Left Adjust
    LEFTADJbm = $10;
    // Free Running
    FREERUNbm = $20;
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
    // Debug run
    DBGRUNbm = $01;
    // ADC_START
    STARTmask = $07;
    START_STOP = $00;
    START_IMMEDIATE = $01;
    START_MUXPOS_WRITE = $02;
    START_EVENT_TRIGGER = $04;
    // ADC_MODE
    MODEmask = $70;
    MODE_SINGLE_8BIT = $00;
    MODE_SINGLE_10BIT = $10;
    MODE_SERIES = $20;
    MODE_BURST = $30;
    MODE_ACCTEST = $70;
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
    MUXPOS_AIN16 = $10;
    MUXPOS_AIN17 = $11;
    MUXPOS_AIN18 = $12;
    MUXPOS_AIN19 = $13;
    MUXPOS_AIN20 = $14;
    MUXPOS_AIN21 = $15;
    MUXPOS_AIN22 = $16;
    MUXPOS_AIN23 = $17;
    MUXPOS_AIN24 = $18;
    MUXPOS_AIN25 = $19;
    MUXPOS_AIN26 = $1A;
    MUXPOS_AIN27 = $1B;
    MUXPOS_AIN31 = $1F;
    MUXPOS_GND = $40;
    MUXPOS_TEMPSENSE = $42;
    MUXPOS_VDDDIV10 = $44;
    MUXPOS_DACREF0 = $49;
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
    BOOTROW: byte;  //Boot row
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
    INSEL0_TCA0 = $09;
    INSEL0_TCB0 = $0A;
    // CCL_INSEL1
    INSEL1mask = $F0;
    INSEL1_MASK = $00;
    INSEL1_FEEDBACK = $10;
    INSEL1_LINK = $20;
    INSEL1_EVENTA = $30;
    INSEL1_EVENTB = $40;
    INSEL1_IN1 = $50;
    INSEL1_AC0 = $60;
    INSEL1_USART1 = $70;
    INSEL1_SPI0 = $80;
    INSEL1_TCA0 = $90;
    INSEL1_TCB1 = $A0;
    // CCL_INSEL2
    INSEL2mask = $0F;
    INSEL2_MASK = $00;
    INSEL2_FEEDBACK = $01;
    INSEL2_LINK = $02;
    INSEL2_EVENTA = $03;
    INSEL2_EVENTB = $04;
    INSEL2_IN2 = $05;
    INSEL2_AC0 = $06;
    INSEL2_USART1 = $07;
    INSEL2_SPI0 = $08;
    INSEL2_TCA0 = $09;
    INSEL2_TCB1 = $0A;
  end;

  TCLKCTRL = object //Clock controller
    MCLKCTRLA: byte;  //MCLK Control A
    MCLKCTRLB: byte;  //MCLK Control B
    MCLKCTRLC: byte;  //MCLK Control C
    MCLKINTCTRL: byte;  //MCLK Interrupt Control
    MCLKINTFLAGS: byte;  //MCLK Interrupt Flags
    MCLKSTATUS: byte;  //MCLK Status
    MCLKTIMEBASE: byte;  //Timebase
    Reserved7: byte;
    OSCHFCTRLA: byte;  //OSCHF Control A
    OSCHFTUNE: byte;  //OSCHF Tune
    OSCHFSTATUS: byte;  //OSCHF Status
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
    OSC32KCTRLA: byte;  //OSC32K Control A
    Reserved25: byte;
    Reserved26: byte;
    Reserved27: byte;
    XOSC32KCTRLA: byte;  //XOSC32K Control A
    Reserved29: byte;
    Reserved30: byte;
    Reserved31: byte;
    XOSCHFCTRLA: byte;  //XOSCHF Control A
    Reserved33: byte;
    Reserved34: byte;
    Reserved35: byte;
    Reserved36: byte;
    USBPLLSTATUS: byte;  //PLL Status
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
    // Clock Failure Detect Enable
    CFDENbm = $01;
    // Clock Failure Detect Test
    CFDTSTbm = $02;
    // CLKCTRL_CFDSRC
    CFDSRCmask = $0C;
    CFDSRC_CLKMAIN = $00;
    CFDSRC_XOSCHF = $04;
    CFDSRC_XOSC32K = $08;
    // Clock Failure Detect Interrupt Enable
    CFDbm = $01;
    // CLKCTRL_INTTYPE
    INTTYPEmask = $80;
    INTTYPE_INT = $00;
    INTTYPE_NMI = $80;
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
    // Timebase
    TIMEBASE0bm = $01;
    TIMEBASE1bm = $02;
    TIMEBASE2bm = $04;
    TIMEBASE3bm = $08;
    TIMEBASE4bm = $10;
    // CLKCTRL_AUTOTUNE
    AUTOTUNEmask = $03;
    AUTOTUNE_OFF = $00;
    AUTOTUNE_32K = $01;
    AUTOTUNE_SOF = $02;
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
    // CLKCTRL_ALGSEL
    ALGSELmask = $40;
    ALGSEL_BIN = $00;
    ALGSEL_INCR = $40;
    // Run standby
    RUNSTDBYbm = $80;
    // Tune
    TUNE0bm = $01;
    TUNE1bm = $02;
    TUNE2bm = $04;
    TUNE3bm = $08;
    TUNE4bm = $10;
    TUNE5bm = $20;
    TUNE6bm = $40;
    // Autotune in Lock
    ATSYNCbm = $01;
    // Autotune Synchronized
    ATLOCKbm = $02;
    // Enable
    ENABLEbm = $01;
    // Low power mode
    LPMODEbm = $02;
    // CLKCTRL_SEL
    SELmask = $04;
    SEL_XTAL = $00;
    SEL_EXTCLK = $04;
    // CLKCTRL_CSUT
    CSUTmask = $30;
    CSUT_1K = $00;
    CSUT_16K = $10;
    CSUT_32K = $20;
    CSUT_64K = $30;
    // CLKCTRL_SELHF
    SELHFmask = $02;
    SELHF_XTAL = $00;
    SELHF_EXTCLK = $02;
    // CLKCTRL_FRQRANGE
    FRQRANGEmask = $0C;
    FRQRANGE_8M = $00;
    FRQRANGE_16M = $04;
    FRQRANGE_24M = $08;
    FRQRANGE_32M = $0C;
    // CLKCTRL_CSUTHF
    CSUTHFmask = $30;
    CSUTHF_256 = $00;
    CSUTHF_1K = $10;
    CSUTHF_4K = $20;
    // Run Standby
    RUNSTBYbm = $80;
    // PLL Stable
    PLLSbm = $01;
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
    USERADC0START: byte;  //ADC0
    USEREVSYSEVOUTA: byte;  //EVOUTA
    USEREVSYSEVOUTD: byte;  //EVOUTD
    USEREVSYSEVOUTF: byte;  //EVOUTF
    USERUSART0IRDA: byte;  //USART0
    USERUSART1IRDA: byte;  //USART1
    USERTCA0CNTA: byte;  //TCA0 Event A
    USERTCA0CNTB: byte;  //TCA0 Event B
    USERTCB0CAPT: byte;  //TCB0 Event A
    USERTCB0COUNT: byte;  //TCB0 Event B
    USERTCB1CAPT: byte;  //TCB1 Event A
    USERTCB1COUNT: byte;  //TCB1 Event B
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
    CHANNEL_RTC_EVGEN0 = $08;
    CHANNEL_RTC_EVGEN1 = $09;
    CHANNEL_CCL_LUT0 = $10;
    CHANNEL_CCL_LUT1 = $11;
    CHANNEL_CCL_LUT2 = $12;
    CHANNEL_CCL_LUT3 = $13;
    CHANNEL_AC0_OUT = $20;
    CHANNEL_ADC0_RESRDY = $24;
    CHANNEL_ADC0_SAMPRDY = $25;
    CHANNEL_ADC0_WCMP = $26;
    CHANNEL_PORTA_EVGEN0 = $40;
    CHANNEL_PORTA_EVGEN1 = $41;
    CHANNEL_PORTC_EVGEN0 = $44;
    CHANNEL_PORTC_EVGEN1 = $45;
    CHANNEL_PORTD_EVGEN0 = $46;
    CHANNEL_PORTD_EVGEN1 = $47;
    CHANNEL_PORTF_EVGEN0 = $4A;
    CHANNEL_PORTF_EVGEN1 = $4B;
    CHANNEL_USART0_XCK = $60;
    CHANNEL_USART1_XCK = $61;
    CHANNEL_SPI0_SCK = $68;
    CHANNEL_TCA0_OVF_LUNF = $80;
    CHANNEL_TCA0_HUNF = $81;
    CHANNEL_TCA0_CMP0_LCMP0 = $84;
    CHANNEL_TCA0_CMP1_LCMP1 = $85;
    CHANNEL_TCA0_CMP2_LCMP2 = $86;
    CHANNEL_TCB0_CAPT = $A0;
    CHANNEL_TCB0_OVF = $A1;
    CHANNEL_TCB1_CAPT = $A2;
    CHANNEL_TCB1_OVF = $A3;
    CHANNEL_USB0_SETUP = $C0;
    CHANNEL_USB0_SOF = $C1;
    CHANNEL_USB0_CRC = $C2;
    CHANNEL_USB0_UNFOVF = $C3;
    CHANNEL_USB0_RX = $C4;
    CHANNEL_USB0_TX = $C5;
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
    // Boot Row Save
    BROWSAVEbm = $02;
    // FUSE_RSTPINCFG
    RSTPINCFGmask = $08;
    RSTPINCFG_GPIO = $00;
    RSTPINCFG_RST = $08;
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
    // FUSE_USBSINK
    USBSINKmask = $08;
    USBSINK_DISABLE = $00;
    USBSINK_ENABLE = $08;
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
    DATA: dword;  //Data
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
    ERROR_INVALIDCMD = $10;
    ERROR_WRITEPROTECT = $20;
    ERROR_CMDCOLLISION = $30;
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
    TCAROUTEA: byte;  //TCA route A
    TCBROUTEA: byte;  //TCB route A
  const
    // PORTMUX_EVOUTA
    EVOUTAmask = $01;
    EVOUTA_DEFAULT = $00;
    EVOUTA_ALT1 = $01;
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
    // PORTMUX_LUT2
    LUT2mask = $04;
    LUT2_DEFAULT = $00;
    LUT2_ALT1 = $04;
    // PORTMUX_LUT3
    LUT3mask = $08;
    LUT3_DEFAULT = $00;
    // PORTMUX_USART0
    USART0mask = $07;
    USART0_DEFAULT = $00;
    USART0_ALT1 = $01;
    USART0_ALT2 = $02;
    USART0_ALT3 = $03;
    USART0_NONE = $05;
    // PORTMUX_USART1
    USART1mask = $18;
    USART1_DEFAULT = $00;
    USART1_ALT2 = $10;
    // PORTMUX_SPI0
    SPI0mask = $07;
    SPI0_DEFAULT = $00;
    SPI0_ALT4 = $04;
    SPI0_NONE = $07;
    // PORTMUX_TWI0
    TWI0mask = $03;
    TWI0_DEFAULT = $00;
    TWI0_ALT1 = $01;
    TWI0_ALT3 = $03;
    // PORTMUX_TCA0
    TCA0mask = $07;
    TCA0_PORTA = $00;
    TCA0_PORTC = $02;
    TCA0_PORTD = $03;
    TCA0_PORTF = $05;
    // PORTMUX_TCB0
    TCB0mask = $01;
    TCB0_DEFAULT = $00;
    // PORTMUX_TCB1
    TCB1mask = $02;
    TCB1_DEFAULT = $00;
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
    VUSBCTRL: byte;  //USB Voltage System Control
  const
    // SYSCFG_USBVREG
    USBVREGmask = $01;
    USBVREG_DISABLE = $00;
    USBVREG_ENABLE = $01;
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

  TTWI = object //Two-Wire Interface
    CTRLA: byte;  //Control A
    Reserved1: byte;
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
    // TWI_DBGRUN
    DBGRUNmask = $01;
    DBGRUN_HALT = $00;
    DBGRUN_RUN = $01;
    // Enable
    ENABLEbm = $01;
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

  TUSB_EP = object //USB Device Controller EP
    STATUS: byte;  //Endpoint Status
    CTRL: byte;  //Endpoint Control
    CNT: word;  //Endpoint Byte Count
    DATAPTR: word;  //Endpoint Data Pointer
    MCNT: word;  //Endpoint Multi-packet Byte Count
  end;

  TUSB_EP_PAIR = object //USB Device Controller EP_PAIR
    OUT_: TUSB_EP;  //USB Device Controller OUT
    IN_: TUSB_EP;  //USB Device Controller IN
  end;

  TUSB_EP_TABLE = object //USB Device Controller EP_TABLE
    FIFO: byte;  //FIFO Pointer Table
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
    EP: TUSB_EP_PAIR;  //USB Device Controller EP
    Reserved48: byte;
    Reserved49: byte;
    Reserved50: byte;
    Reserved51: byte;
    Reserved52: byte;
    Reserved53: byte;
    Reserved54: byte;
    Reserved55: byte;
    Reserved56: byte;
    Reserved57: byte;
    Reserved58: byte;
    Reserved59: byte;
    Reserved60: byte;
    Reserved61: byte;
    Reserved62: byte;
    Reserved63: byte;
    Reserved64: byte;
    Reserved65: byte;
    Reserved66: byte;
    Reserved67: byte;
    Reserved68: byte;
    Reserved69: byte;
    Reserved70: byte;
    Reserved71: byte;
    Reserved72: byte;
    Reserved73: byte;
    Reserved74: byte;
    Reserved75: byte;
    Reserved76: byte;
    Reserved77: byte;
    Reserved78: byte;
    Reserved79: byte;
    Reserved80: byte;
    Reserved81: byte;
    Reserved82: byte;
    Reserved83: byte;
    Reserved84: byte;
    Reserved85: byte;
    Reserved86: byte;
    Reserved87: byte;
    Reserved88: byte;
    Reserved89: byte;
    Reserved90: byte;
    Reserved91: byte;
    Reserved92: byte;
    Reserved93: byte;
    Reserved94: byte;
    Reserved95: byte;
    Reserved96: byte;
    Reserved97: byte;
    Reserved98: byte;
    Reserved99: byte;
    Reserved100: byte;
    Reserved101: byte;
    Reserved102: byte;
    Reserved103: byte;
    Reserved104: byte;
    Reserved105: byte;
    Reserved106: byte;
    Reserved107: byte;
    Reserved108: byte;
    Reserved109: byte;
    Reserved110: byte;
    Reserved111: byte;
    Reserved112: byte;
    Reserved113: byte;
    Reserved114: byte;
    Reserved115: byte;
    Reserved116: byte;
    Reserved117: byte;
    Reserved118: byte;
    Reserved119: byte;
    Reserved120: byte;
    Reserved121: byte;
    Reserved122: byte;
    Reserved123: byte;
    Reserved124: byte;
    Reserved125: byte;
    Reserved126: byte;
    Reserved127: byte;
    Reserved128: byte;
    Reserved129: byte;
    Reserved130: byte;
    Reserved131: byte;
    Reserved132: byte;
    Reserved133: byte;
    Reserved134: byte;
    Reserved135: byte;
    Reserved136: byte;
    Reserved137: byte;
    Reserved138: byte;
    Reserved139: byte;
    Reserved140: byte;
    Reserved141: byte;
    Reserved142: byte;
    Reserved143: byte;
    Reserved144: byte;
    Reserved145: byte;
    Reserved146: byte;
    Reserved147: byte;
    Reserved148: byte;
    Reserved149: byte;
    Reserved150: byte;
    Reserved151: byte;
    Reserved152: byte;
    Reserved153: byte;
    Reserved154: byte;
    Reserved155: byte;
    Reserved156: byte;
    Reserved157: byte;
    Reserved158: byte;
    Reserved159: byte;
    Reserved160: byte;
    Reserved161: byte;
    Reserved162: byte;
    Reserved163: byte;
    Reserved164: byte;
    Reserved165: byte;
    Reserved166: byte;
    Reserved167: byte;
    Reserved168: byte;
    Reserved169: byte;
    Reserved170: byte;
    Reserved171: byte;
    Reserved172: byte;
    Reserved173: byte;
    Reserved174: byte;
    Reserved175: byte;
    Reserved176: byte;
    Reserved177: byte;
    Reserved178: byte;
    Reserved179: byte;
    Reserved180: byte;
    Reserved181: byte;
    Reserved182: byte;
    Reserved183: byte;
    Reserved184: byte;
    Reserved185: byte;
    Reserved186: byte;
    Reserved187: byte;
    Reserved188: byte;
    Reserved189: byte;
    Reserved190: byte;
    Reserved191: byte;
    Reserved192: byte;
    Reserved193: byte;
    Reserved194: byte;
    Reserved195: byte;
    Reserved196: byte;
    Reserved197: byte;
    Reserved198: byte;
    Reserved199: byte;
    Reserved200: byte;
    Reserved201: byte;
    Reserved202: byte;
    Reserved203: byte;
    Reserved204: byte;
    Reserved205: byte;
    Reserved206: byte;
    Reserved207: byte;
    Reserved208: byte;
    Reserved209: byte;
    Reserved210: byte;
    Reserved211: byte;
    Reserved212: byte;
    Reserved213: byte;
    Reserved214: byte;
    Reserved215: byte;
    Reserved216: byte;
    Reserved217: byte;
    Reserved218: byte;
    Reserved219: byte;
    Reserved220: byte;
    Reserved221: byte;
    Reserved222: byte;
    Reserved223: byte;
    Reserved224: byte;
    Reserved225: byte;
    Reserved226: byte;
    Reserved227: byte;
    Reserved228: byte;
    Reserved229: byte;
    Reserved230: byte;
    Reserved231: byte;
    Reserved232: byte;
    Reserved233: byte;
    Reserved234: byte;
    Reserved235: byte;
    Reserved236: byte;
    Reserved237: byte;
    Reserved238: byte;
    Reserved239: byte;
    Reserved240: byte;
    Reserved241: byte;
    Reserved242: byte;
    Reserved243: byte;
    Reserved244: byte;
    Reserved245: byte;
    Reserved246: byte;
    Reserved247: byte;
    Reserved248: byte;
    Reserved249: byte;
    Reserved250: byte;
    Reserved251: byte;
    Reserved252: byte;
    Reserved253: byte;
    Reserved254: byte;
    Reserved255: byte;
    Reserved256: byte;
    Reserved257: byte;
    Reserved258: byte;
    Reserved259: byte;
    Reserved260: byte;
    Reserved261: byte;
    Reserved262: byte;
    Reserved263: byte;
    Reserved264: byte;
    Reserved265: byte;
    Reserved266: byte;
    Reserved267: byte;
    Reserved268: byte;
    Reserved269: byte;
    Reserved270: byte;
    Reserved271: byte;
    Reserved272: byte;
    Reserved273: byte;
    Reserved274: byte;
    Reserved275: byte;
    Reserved276: byte;
    Reserved277: byte;
    Reserved278: byte;
    Reserved279: byte;
    Reserved280: byte;
    Reserved281: byte;
    Reserved282: byte;
    Reserved283: byte;
    Reserved284: byte;
    Reserved285: byte;
    Reserved286: byte;
    Reserved287: byte;
    FRAMENUM: word;  //FRAMENUM count
  const
    // USB_DIR
    DIRmask = $08;
    DIR_OUT = $00;
    DIR_IN = $08;
    // Endpoint Number
    EPNUM0bm = $10;
    EPNUM1bm = $20;
    EPNUM2bm = $40;
    EPNUM3bm = $80;
  end;

  TUSB_STATUS = object //USB Device Controller STATUS
    OUTCLR: byte;  //Endpoint n OUT Status Clear
    OUTSET: byte;  //Endpoint n OUT Status Set
    INCLR: byte;  //Endpoint n IN Status Clear
    INSET: byte;  //Endpoint n IN Status Set
  end;

  TUSB = object //USB Device Controller
    CTRLA: byte;  //Control A
    CTRLB: byte;  //Control B
    BUSSTATE: byte;  //Bus State
    ADDR: byte;  //Address
    FIFOWP: byte;  //FIFO Write Pointer
    FIFORP: byte;  //FIFO Read Pointer
    EPPTR: word;  //Endpoint Configuration Table Pointer
    INTCTRLA: byte;  //Interrupt Control A
    INTCTRLB: byte;  //Interrupt Control B
    INTFLAGSA: byte;  //Interrupt Flags A
    INTFLAGSB: byte;  //Interrupt Flags B
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
    Reserved24: byte;
    Reserved25: byte;
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    Reserved30: byte;
    Reserved31: byte;
    Reserved32: byte;
    Reserved33: byte;
    Reserved34: byte;
    Reserved35: byte;
    Reserved36: byte;
    Reserved37: byte;
    Reserved38: byte;
    Reserved39: byte;
    Reserved40: byte;
    Reserved41: byte;
    Reserved42: byte;
    Reserved43: byte;
    Reserved44: byte;
    Reserved45: byte;
    Reserved46: byte;
    Reserved47: byte;
    Reserved48: byte;
    Reserved49: byte;
    Reserved50: byte;
    Reserved51: byte;
    Reserved52: byte;
    Reserved53: byte;
    Reserved54: byte;
    Reserved55: byte;
    Reserved56: byte;
    Reserved57: byte;
    Reserved58: byte;
    Reserved59: byte;
    Reserved60: byte;
    Reserved61: byte;
    Reserved62: byte;
    Reserved63: byte;
    STATUS: dword;  //USB Device Controller STATUS
  const
    // Maximum Endpoint Address
    MAXEP0bm = $01;
    MAXEP1bm = $02;
    MAXEP2bm = $04;
    MAXEP3bm = $08;
    // Store Frame Number Enable
    STFRNUMbm = $10;
    // Transaction Complete FIFO Enable
    FIFOENbm = $20;
    // USB Enable
    ENABLEbm = $80;
    // Attach
    ATTACHbm = $01;
    // Respond with NAK on all Endpoints
    GNAKbm = $02;
    // Set GNAK Automatically after SETUP
    GNAUTObm = $04;
    // Send Upstream Resume
    URESUMEbm = $08;
    // Bus Reset
    BUSRSTbm = $01;
    // Bus Suspended
    SUSPENDEDbm = $02;
    // Downstream Resume
    DRESUMEbm = $04;
    // Wait Time Resume
    WTRSMbm = $10;
    // Device Address
    ADDR0bm = $01;
    ADDR1bm = $02;
    ADDR2bm = $04;
    ADDR3bm = $08;
    ADDR4bm = $10;
    ADDR5bm = $20;
    ADDR6bm = $40;
    // FIFO Write Pointer
    FIFOWP0bm = $01;
    FIFOWP1bm = $02;
    FIFOWP2bm = $04;
    FIFOWP3bm = $08;
    FIFOWP4bm = $10;
    // FIFO Read Pointer
    FIFORP0bm = $01;
    FIFORP1bm = $02;
    FIFORP2bm = $04;
    FIFORP3bm = $08;
    FIFORP4bm = $10;
    // Overflow Interrupt Enable
    OVFbm = $02;
    // Underflow Interrupt Enable
    UNFbm = $04;
    // STALL Interrupt Enable
    STALLEDbm = $08;
    // Reset Interrupt Enable
    RESETbm = $10;
    // Resume Interrupt Enable
    RESUMEbm = $20;
    // Suspend Interrupt Enable
    SUSPENDbm = $40;
    // Start Of Frame Interrupt Enable
    SOFbm = $80;
    // SETUP Transaction Complete Interrupt Enable
    SETUPbm = $01;
    // GNAK Operation Done Interrupt Enable
    GNDONEbm = $02;
    // Transaction Complete Interrupt Enable
    TRNCOMPLbm = $20;
    // RMW Busy Flag
    RMWBUSYbm = $04;
  end;

  TUSERROW = object //User Row
    USERROW: byte;  //User Row
  end;

  TVPORT = object //Virtual Ports
    DIR: byte;  //Data Direction
    OUT_: byte;  //Output Value
    IN_: byte;  //Input Value
    INTFLAGS: byte;  //Interrupt Flags
  end;

  TVREF = object //Voltage reference
    ACREF: byte;  //ADC0 Reference
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
  USART0: TUSART absolute $0800;
  USART1: TUSART absolute $0820;
  TWI0: TTWI absolute $0900;
  SPI0: TSPI absolute $0940;
  TCA0: TTCA absolute $0A00;
  TCB0: TTCB absolute $0B00;
  TCB1: TTCB absolute $0B10;
  USB0: TUSB absolute $0C00;
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
procedure CLKCTRL_CFD_ISR; external name 'CLKCTRL_CFD_ISR'; // Interrupt 3
procedure RTC_CNT_ISR; external name 'RTC_CNT_ISR'; // Interrupt 4
procedure RTC_PIT_ISR; external name 'RTC_PIT_ISR'; // Interrupt 5
procedure CCL_CCL_ISR; external name 'CCL_CCL_ISR'; // Interrupt 6
procedure USB0_BUSEVENT_ISR; external name 'USB0_BUSEVENT_ISR'; // Interrupt 7
procedure USB0_TRNCOMPL_ISR; external name 'USB0_TRNCOMPL_ISR'; // Interrupt 8
procedure PORTA_PORT_ISR; external name 'PORTA_PORT_ISR'; // Interrupt 9
procedure TCA0_LUNF_ISR; external name 'TCA0_LUNF_ISR'; // Interrupt 10
//procedure TCA0_OVF_ISR; external name 'TCA0_OVF_ISR'; // Interrupt 10
procedure TCA0_HUNF_ISR; external name 'TCA0_HUNF_ISR'; // Interrupt 11
procedure TCA0_CMP0_ISR; external name 'TCA0_CMP0_ISR'; // Interrupt 12
//procedure TCA0_LCMP0_ISR; external name 'TCA0_LCMP0_ISR'; // Interrupt 12
procedure TCA0_CMP1_ISR; external name 'TCA0_CMP1_ISR'; // Interrupt 13
//procedure TCA0_LCMP1_ISR; external name 'TCA0_LCMP1_ISR'; // Interrupt 13
procedure TCA0_CMP2_ISR; external name 'TCA0_CMP2_ISR'; // Interrupt 14
//procedure TCA0_LCMP2_ISR; external name 'TCA0_LCMP2_ISR'; // Interrupt 14
procedure TCB0_INT_ISR; external name 'TCB0_INT_ISR'; // Interrupt 15
procedure TWI0_TWIS_ISR; external name 'TWI0_TWIS_ISR'; // Interrupt 16
procedure TWI0_TWIM_ISR; external name 'TWI0_TWIM_ISR'; // Interrupt 17
procedure SPI0_INT_ISR; external name 'SPI0_INT_ISR'; // Interrupt 18
procedure USART0_RXC_ISR; external name 'USART0_RXC_ISR'; // Interrupt 19
procedure USART0_DRE_ISR; external name 'USART0_DRE_ISR'; // Interrupt 20
procedure USART0_TXC_ISR; external name 'USART0_TXC_ISR'; // Interrupt 21
procedure PORTD_PORT_ISR; external name 'PORTD_PORT_ISR'; // Interrupt 22
procedure PORTC_PORT_ISR; external name 'PORTC_PORT_ISR'; // Interrupt 23
procedure PORTF_PORT_ISR; external name 'PORTF_PORT_ISR'; // Interrupt 24
procedure NVMCTRL_NVMREADY_ISR; external name 'NVMCTRL_NVMREADY_ISR'; // Interrupt 25
procedure USART1_RXC_ISR; external name 'USART1_RXC_ISR'; // Interrupt 26
procedure USART1_DRE_ISR; external name 'USART1_DRE_ISR'; // Interrupt 27
procedure USART1_TXC_ISR; external name 'USART1_TXC_ISR'; // Interrupt 28
procedure TCB1_INT_ISR; external name 'TCB1_INT_ISR'; // Interrupt 29
procedure AC0_AC_ISR; external name 'AC0_AC_ISR'; // Interrupt 30
procedure ADC0_ERROR_ISR; external name 'ADC0_ERROR_ISR'; // Interrupt 31
procedure ADC0_RESRDY_ISR; external name 'ADC0_RESRDY_ISR'; // Interrupt 32
procedure ADC0_SAMPRDY_ISR; external name 'ADC0_SAMPRDY_ISR'; // Interrupt 33

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
asm
  jmp __dtors_end
  jmp CRCSCAN_NMI_ISR
  jmp BOD_VLM_ISR
  jmp CLKCTRL_CFD_ISR
  jmp RTC_CNT_ISR
  jmp RTC_PIT_ISR
  jmp CCL_CCL_ISR
  jmp USB0_BUSEVENT_ISR
  jmp USB0_TRNCOMPL_ISR
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
  jmp TWI0_TWIS_ISR
  jmp TWI0_TWIM_ISR
  jmp SPI0_INT_ISR
  jmp USART0_RXC_ISR
  jmp USART0_DRE_ISR
  jmp USART0_TXC_ISR
  jmp PORTD_PORT_ISR
  jmp PORTC_PORT_ISR
  jmp PORTF_PORT_ISR
  jmp NVMCTRL_NVMREADY_ISR
  jmp USART1_RXC_ISR
  jmp USART1_DRE_ISR
  jmp USART1_TXC_ISR
  jmp TCB1_INT_ISR
  jmp AC0_AC_ISR
  jmp ADC0_ERROR_ISR
  jmp ADC0_RESRDY_ISR
  jmp ADC0_SAMPRDY_ISR

  .weak CRCSCAN_NMI_ISR
  .weak BOD_VLM_ISR
  .weak CLKCTRL_CFD_ISR
  .weak RTC_CNT_ISR
  .weak RTC_PIT_ISR
  .weak CCL_CCL_ISR
  .weak USB0_BUSEVENT_ISR
  .weak USB0_TRNCOMPL_ISR
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
  .weak TWI0_TWIS_ISR
  .weak TWI0_TWIM_ISR
  .weak SPI0_INT_ISR
  .weak USART0_RXC_ISR
  .weak USART0_DRE_ISR
  .weak USART0_TXC_ISR
  .weak PORTD_PORT_ISR
  .weak PORTC_PORT_ISR
  .weak PORTF_PORT_ISR
  .weak NVMCTRL_NVMREADY_ISR
  .weak USART1_RXC_ISR
  .weak USART1_DRE_ISR
  .weak USART1_TXC_ISR
  .weak TCB1_INT_ISR
  .weak AC0_AC_ISR
  .weak ADC0_ERROR_ISR
  .weak ADC0_RESRDY_ISR
  .weak ADC0_SAMPRDY_ISR

  .set CRCSCAN_NMI_ISR, Default_IRQ_handler
  .set BOD_VLM_ISR, Default_IRQ_handler
  .set CLKCTRL_CFD_ISR, Default_IRQ_handler
  .set RTC_CNT_ISR, Default_IRQ_handler
  .set RTC_PIT_ISR, Default_IRQ_handler
  .set CCL_CCL_ISR, Default_IRQ_handler
  .set USB0_BUSEVENT_ISR, Default_IRQ_handler
  .set USB0_TRNCOMPL_ISR, Default_IRQ_handler
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
  .set TWI0_TWIS_ISR, Default_IRQ_handler
  .set TWI0_TWIM_ISR, Default_IRQ_handler
  .set SPI0_INT_ISR, Default_IRQ_handler
  .set USART0_RXC_ISR, Default_IRQ_handler
  .set USART0_DRE_ISR, Default_IRQ_handler
  .set USART0_TXC_ISR, Default_IRQ_handler
  .set PORTD_PORT_ISR, Default_IRQ_handler
  .set PORTC_PORT_ISR, Default_IRQ_handler
  .set PORTF_PORT_ISR, Default_IRQ_handler
  .set NVMCTRL_NVMREADY_ISR, Default_IRQ_handler
  .set USART1_RXC_ISR, Default_IRQ_handler
  .set USART1_DRE_ISR, Default_IRQ_handler
  .set USART1_TXC_ISR, Default_IRQ_handler
  .set TCB1_INT_ISR, Default_IRQ_handler
  .set AC0_AC_ISR, Default_IRQ_handler
  .set ADC0_ERROR_ISR, Default_IRQ_handler
  .set ADC0_RESRDY_ISR, Default_IRQ_handler
  .set ADC0_SAMPRDY_ISR, Default_IRQ_handler
end;

end.

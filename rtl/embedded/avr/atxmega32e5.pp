unit ATxmega32E5;

interface

type
  TGPIO = object //General Purpose IO Registers
    GPIOR0: byte;  //General Purpose IO Register 0
    GPIOR1: byte;  //General Purpose IO Register 1
    GPIOR2: byte;  //General Purpose IO Register 2
    GPIOR3: byte;  //General Purpose IO Register 3
  end;

  TVPORT = object //Virtual Port
    DIR: byte;  //I/O Port Data Direction
    OUT_: byte;  //I/O Port Output
    IN_: byte;  //I/O Port Input
    INTFLAGS: byte;  //Interrupt Flag Register
  const
    // Interrupt Pin 7 Flag
    INT7IFbm = $80;
    // Interrupt Pin 6 Flag
    INT6IFbm = $40;
    // Interrupt Pin 5 Flag
    INT5IFbm = $20;
    // Interrupt Pin 4 Flag
    INT4IFbm = $10;
    // Interrupt Pin 3 Flag
    INT3IFbm = $08;
    // Interrupt Pin 2 Flag
    INT2IFbm = $04;
    // Interrupt Pin 1 Flag
    INT1IFbm = $02;
    // Interrupt Pin 0 Flag
    INT0IFbm = $01;
  end;

  TOCD = object //On-Chip Debug System
    OCDR0: byte;  //OCD Register 0
    OCDR1: byte;  //OCD Register 1
  end;

  TCPU = object //CPU registers
    Reserved0: byte;
    Reserved1: byte;
    Reserved2: byte;
    Reserved3: byte;
    CCP: byte;  //Configuration Change Protection
    Reserved5: byte;
    Reserved6: byte;
    Reserved7: byte;
    RAMPD: byte;  //Ramp D
    RAMPX: byte;  //Ramp X
    RAMPY: byte;  //Ramp Y
    RAMPZ: byte;  //Ramp Z
    EIND: byte;  //Extended Indirect Jump
    SPL: byte;  //Stack Pointer Low
    SPH: byte;  //Stack Pointer High
    SREG: byte;  //Status Register
  const
    // CCP
    CCPmask = $FF;
    CCP_SPM = $9D;
    CCP_IOREG = $D8;
    // Global Interrupt Enable Flag
    Ibm = $80;
    // Transfer Bit
    Tbm = $40;
    // Half Carry Flag
    Hbm = $20;
    // N Exclusive Or V Flag
    Sbm = $10;
    // Two's Complement Overflow Flag
    Vbm = $08;
    // Negative Flag
    Nbm = $04;
    // Zero Flag
    Zbm = $02;
    // Carry Flag
    Cbm = $01;
  end;

  TCLK = object //Clock System
    CTRL: byte;  //Control Register
    PSCTRL: byte;  //Prescaler Control Register
    LOCK: byte;  //Lock register
    RTCCTRL: byte;  //RTC Control Register
  const
    // CLK_SCLKSEL
    SCLKSELmask = $07;
    SCLKSEL_RC2M = $00;
    SCLKSEL_RC32M = $01;
    SCLKSEL_RC32K = $02;
    SCLKSEL_XOSC = $03;
    SCLKSEL_PLL = $04;
    SCLKSEL_RC8M = $05;
    // CLK_PSADIV
    PSADIVmask = $7C;
    PSADIV_1 = $00;
    PSADIV_2 = $04;
    PSADIV_4 = $0C;
    PSADIV_8 = $14;
    PSADIV_16 = $1C;
    PSADIV_32 = $24;
    PSADIV_64 = $2C;
    PSADIV_128 = $34;
    PSADIV_256 = $3C;
    PSADIV_512 = $44;
    PSADIV_6 = $4C;
    PSADIV_10 = $54;
    PSADIV_12 = $5C;
    PSADIV_24 = $64;
    PSADIV_48 = $6C;
    // CLK_PSBCDIV
    PSBCDIVmask = $03;
    PSBCDIV_1_1 = $00;
    PSBCDIV_1_2 = $01;
    PSBCDIV_4_1 = $02;
    PSBCDIV_2_2 = $03;
    // Clock System Lock
    LOCKbm = $01;
    // CLK_RTCSRC
    RTCSRCmask = $0E;
    RTCSRC_ULP = $00;
    RTCSRC_TOSC = $02;
    RTCSRC_RCOSC = $04;
    RTCSRC_TOSC32 = $0A;
    RTCSRC_RCOSC32 = $0C;
    RTCSRC_EXTCLK = $0E;
    // Clock Source Enable
    RTCENbm = $01;
  end;

  TPR = object //Power Reduction
    PRGEN: byte;  //General Power Reduction
    PRPA: byte;  //Power Reduction Port A
    Reserved2: byte;
    PRPC: byte;  //Power Reduction Port C
    PRPD: byte;  //Power Reduction Port D
  const
    // XMEGA Custom Logic
    XCLbm = $80;
    // Real-time Counter
    RTCbm = $04;
    // Event System
    EVSYSbm = $02;
    // Enhanced DMA-Controller
    EDMAbm = $01;
    // Port A DAC
    DACbm = $04;
    // Port A ADC
    ADCbm = $02;
    // Port A Analog Comparator
    ACbm = $01;
    // Port C Two-wire Interface
    TWIbm = $40;
    // Port C USART0
    USART0bm = $10;
    // Port C SPI
    SPIbm = $08;
    // Port C WEX
    HIRESbm = $04;
    // Port C Timer/Counter5
    TC5bm = $02;
    // Port C Timer/Counter4
    TC4bm = $01;
  end;

  TSLEEP = object //Sleep Controller
    CTRL: byte;  //Control Register
  const
    // SLEEP_SMODE
    SMODEmask = $0E;
    SMODE_IDLE = $00;
    SMODE_PDOWN = $04;
    SMODE_PSAVE = $06;
    SMODE_STDBY = $0C;
    SMODE_ESTDBY = $0E;
    // Sleep Enable
    SENbm = $01;
  end;

  TOSC = object //Oscillator
    CTRL: byte;  //Control Register
    STATUS: byte;  //Status Register
    XOSCCTRL: byte;  //External Oscillator Control Register
    XOSCFAIL: byte;  //Oscillator Failure Detection Register
    RC32KCAL: byte;  //32.768 kHz Internal Oscillator Calibration Register
    PLLCTRL: byte;  //PLL Control Register
    DFLLCTRL: byte;  //DFLL Control Register
    RC8MCAL: byte;  //Internal 8 MHz RC Oscillator Calibration Register
  const
    // Internal 8 MHz RC Low Power Mode Enable
    RC8MLPMbm = $40;
    // Internal 8 MHz RC Oscillator Enable
    RC8MENbm = $20;
    // PLL Enable
    PLLENbm = $10;
    // External Oscillator Enable
    XOSCENbm = $08;
    // Internal 32.768 kHz RC Oscillator Enable
    RC32KENbm = $04;
    // Internal 32 MHz RC Oscillator Enable
    RC32MENbm = $02;
    // Internal 2 MHz RC Oscillator Enable
    RC2MENbm = $01;
    // Internal 8 MHz RC Oscillator Ready
    RC8MRDYbm = $20;
    // PLL Ready
    PLLRDYbm = $10;
    // External Oscillator Ready
    XOSCRDYbm = $08;
    // Internal 32.768 kHz RC Oscillator Ready
    RC32KRDYbm = $04;
    // Internal 32 MHz RC Oscillator Ready
    RC32MRDYbm = $02;
    // Internal 2 MHz RC Oscillator Ready
    RC2MRDYbm = $01;
    // OSC_FRQRANGE
    FRQRANGEmask = $C0;
    FRQRANGE_04TO2 = $00;
    FRQRANGE_2TO9 = $40;
    FRQRANGE_9TO12 = $80;
    FRQRANGE_12TO16 = $C0;
    // 32.768 kHz XTAL OSC Low-power Mode
    X32KLPMbm = $20;
    // 16 MHz Crystal Oscillator High Power mode
    XOSCPWRbm = $10;
    // OSC_XOSCSEL
    XOSCSELmask = $1F;
    XOSCSEL_EXTCLK = $00;
    XOSCSEL_32KHz = $02;
    XOSCSEL_XTAL_256CLK = $03;
    XOSCSEL_XTAL_1KCLK = $07;
    XOSCSEL_XTAL_16KCLK = $0B;
    XOSCSEL_EXTCLK_C4 = $14;
    // PLL Failure Detection Interrupt Flag
    PLLFDIFbm = $08;
    // PLL Failure Detection Enable
    PLLFDENbm = $04;
    // XOSC Failure Detection Interrupt Flag
    XOSCFDIFbm = $02;
    // XOSC Failure Detection Enable
    XOSCFDENbm = $01;
    // OSC_PLLSRC
    PLLSRCmask = $C0;
    PLLSRC_RC2M = $00;
    PLLSRC_RC8M = $40;
    PLLSRC_RC32M = $80;
    PLLSRC_XOSC = $C0;
    // Divide by 2
    PLLDIVbm = $20;
    // Multiplication Factor
    PLLFAC0bm = $01;
    PLLFAC1bm = $02;
    PLLFAC2bm = $04;
    PLLFAC3bm = $08;
    PLLFAC4bm = $10;
    // OSC_RC32MCREF
    RC32MCREFmask = $06;
    RC32MCREF_RC32K = $00;
    RC32MCREF_XOSC32K = $02;
  end;

  TDFLL = object //DFLL
    CTRL: byte;  //Control Register
    Reserved1: byte;
    CALA: byte;  //Calibration Register A
    CALB: byte;  //Calibration Register B
    COMP0: byte;  //Oscillator Compare Register 0
    COMP1: byte;  //Oscillator Compare Register 1
    COMP2: byte;  //Oscillator Compare Register 2
  const
    // DFLL Enable
    ENABLEbm = $01;
    // DFLL Calibration Value A
    CALL0bm = $01;
    CALL1bm = $02;
    CALL2bm = $04;
    CALL3bm = $08;
    CALL4bm = $10;
    CALL5bm = $20;
    CALL6bm = $40;
    // DFLL Calibration Value B
    CALH0bm = $01;
    CALH1bm = $02;
    CALH2bm = $04;
    CALH3bm = $08;
    CALH4bm = $10;
    CALH5bm = $20;
  end;

  TRST = object //Reset
    STATUS: byte;  //Status Register
    CTRL: byte;  //Control Register
  const
    // Spike Detection Reset Flag
    SDRFbm = $40;
    // Software Reset Flag
    SRFbm = $20;
    // Programming and Debug Interface Interface Reset Flag
    PDIRFbm = $10;
    // Watchdog Reset Flag
    WDRFbm = $08;
    // Brown-out Reset Flag
    BORFbm = $04;
    // External Reset Flag
    EXTRFbm = $02;
    // Power-on Reset Flag
    PORFbm = $01;
    // Software Reset
    SWRSTbm = $01;
  end;

  TWDT = object //Watch-Dog Timer
    CTRL: byte;  //Control
    WINCTRL: byte;  //Windowed Mode Control
    STATUS: byte;  //Status
  const
    // WDT_PER
    PERmask = $3C;
    PER_8CLK = $00;
    PER_16CLK = $04;
    PER_32CLK = $08;
    PER_64CLK = $0C;
    PER_128CLK = $10;
    PER_256CLK = $14;
    PER_512CLK = $18;
    PER_1KCLK = $1C;
    PER_2KCLK = $20;
    PER_4KCLK = $24;
    PER_8KCLK = $28;
    // Enable
    ENABLEbm = $02;
    // Change Enable
    CENbm = $01;
    // WDT_WPER
    WPERmask = $3C;
    WPER_8CLK = $00;
    WPER_16CLK = $04;
    WPER_32CLK = $08;
    WPER_64CLK = $0C;
    WPER_128CLK = $10;
    WPER_256CLK = $14;
    WPER_512CLK = $18;
    WPER_1KCLK = $1C;
    WPER_2KCLK = $20;
    WPER_4KCLK = $24;
    WPER_8KCLK = $28;
    // Windowed Mode Enable
    WENbm = $02;
    // Windowed Mode Change Enable
    WCENbm = $01;
    // Synchronization busy
    SYNCBUSYbm = $01;
  end;

  TMCU = object //MCU Control
    DEVID0: byte;  //Device ID byte 0
    DEVID1: byte;  //Device ID byte 1
    DEVID2: byte;  //Device ID byte 2
    REVID: byte;  //Revision ID
    Reserved4: byte;
    Reserved5: byte;
    Reserved6: byte;
    ANAINIT: byte;  //Analog Startup Delay
    EVSYSLOCK: byte;  //Event System Lock
    WEXLOCK: byte;  //WEX Lock
    FAULTLOCK: byte;  //FAULT Lock
  const
    // Analog startup delay Port A
    STARTUPDLYA0bm = $01;
    STARTUPDLYA1bm = $02;
    // Event Channel 4-7 Lock
    EVSYS1LOCKbm = $10;
    // Event Channel 0-3 Lock
    EVSYS0LOCKbm = $01;
    // WeX on T/C C4 Lock
    WEXCLOCKbm = $01;
    // Fault on T/C C5 Lock
    FAULTC5LOCKbm = $02;
    // Fault on T/C C4 Lock
    FAULTC4LOCKbm = $01;
  end;

  TPMIC = object //Programmable Multi-level Interrupt Controller
    STATUS: byte;  //Status Register
    INTPRI: byte;  //Interrupt Priority
    CTRL: byte;  //Control Register
  const
    // Non-maskable Interrupt Executing
    NMIEXbm = $80;
    // High Level Interrupt Executing
    HILVLEXbm = $04;
    // Medium Level Interrupt Executing
    MEDLVLEXbm = $02;
    // Low Level Interrupt Executing
    LOLVLEXbm = $01;
    // Round-Robin Priority Enable
    RRENbm = $80;
    // Interrupt Vector Select
    IVSELbm = $40;
    // High Level Enable
    HILVLENbm = $04;
    // Medium Level Enable
    MEDLVLENbm = $02;
    // Low Level Enable
    LOLVLENbm = $01;
  end;

  TPORTCFG = object //I/O port Configuration
    MPCMASK: byte;  //Multi-pin Configuration Mask
    Reserved1: byte;
    Reserved2: byte;
    Reserved3: byte;
    CLKOUT: byte;  //Clock Out Register
    Reserved5: byte;
    ACEVOUT: byte;  //Analog Comparator and Event Out Register
    SRLCTRL: byte;  //Slew Rate Limit Control Register
  const
    // PORTCFG_CLKEVPIN
    CLKEVPINmask = $80;
    CLKEVPIN_PIN7 = $00;
    CLKEVPIN_PIN4 = $80;
    // RTCOUT
    RTCOUTmask = $60;
    RTCOUTOFF = $00;
    RTCOUTPC6 = $20;
    RTCOUTPD6 = $40;
    RTCOUTPR0 = $60;
    // PORTCFG_CLKOUTSEL
    CLKOUTSELmask = $0C;
    CLKOUTSEL_CLK1X = $00;
    CLKOUTSEL_CLK2X = $04;
    CLKOUTSEL_CLK4X = $08;
    // PORTCFG_CLKOUT
    CLKOUTmask = $03;
    CLKOUT_OFF = $00;
    CLKOUT_PC7 = $01;
    CLKOUT_PD7 = $02;
    CLKOUT_PR0 = $03;
    // PORTCFG_ACOUT
    ACOUTmask = $C0;
    ACOUT_PA = $00;
    ACOUT_PC = $40;
    ACOUT_PD = $80;
    ACOUT_PR = $C0;
    // PORTCFG_EVOUT
    EVOUTmask = $30;
    EVOUT_OFF = $00;
    EVOUT_PC7 = $10;
    EVOUT_PD7 = $20;
    EVOUT_PR0 = $30;
    // Asynchronous Event Enabled
    EVASYENbm = $08;
    // PORTCFG_EVOUTSEL
    EVOUTSELmask = $07;
    EVOUTSEL_0 = $00;
    EVOUTSEL_1 = $01;
    EVOUTSEL_2 = $02;
    EVOUTSEL_3 = $03;
    EVOUTSEL_4 = $04;
    EVOUTSEL_5 = $05;
    EVOUTSEL_6 = $06;
    EVOUTSEL_7 = $07;
    // Slew Rate Limit Enable on PORTA
    SRLENRAbm = $01;
    // Slew Rate Limit Enable on PORTC
    SRLENRCbm = $04;
    // Slew Rate Limit Enable on PORTD
    SRLENRDbm = $08;
    // Slew Rate Limit Enable on PORTR
    SRLENRRbm = $80;
  end;

  TCRC = object //Cyclic Redundancy Checker
    CTRL: byte;  //Control Register
    STATUS: byte;  //Status Register
    Reserved2: byte;
    DATAIN: byte;  //Data Input
    CHECKSUM0: byte;  //Checksum byte 0
    CHECKSUM1: byte;  //Checksum byte 1
    CHECKSUM2: byte;  //Checksum byte 2
    CHECKSUM3: byte;  //Checksum byte 3
  const
    // CRC_RESET
    RESETmask = $C0;
    RESET_NO = $00;
    RESET_RESET0 = $80;
    RESET_RESET1 = $C0;
    // CRC Mode
    CRC32bm = $20;
    // CRC_SOURCE
    SOURCEmask = $0F;
    SOURCE_DISABLE = $00;
    SOURCE_IO = $01;
    SOURCE_FLASH = $02;
    SOURCE_DMAC0 = $04;
    SOURCE_DMAC1 = $05;
    SOURCE_DMAC2 = $06;
    SOURCE_DMAC3 = $07;
    // Zero detection
    ZERObm = $02;
    // Busy
    BUSYbm = $01;
  end;

  TEDMA_CH = object //EDMA Channel
    CTRLA: byte;  //Channel Control A
    CTRLB: byte;  //Channel Control
    ADDRCTRL: byte;  //Memory Address Control for Peripheral Ch., or Source Address Control for Standard Ch.
    DESTADDRCTRL: byte;  //Destination Address Control for Standard Channels Only.
    TRIGSRC: byte;  //Channel Trigger Source
    Reserved5: byte;
    TRFCNT: word;  //Channel Block Transfer Count for Peripheral Ch., or Channel Block Transfer Count Low for Standard Ch.
    ADDR: word;  //Channel Memory Address for Peripheral Ch., or Channel Source Address Low for Standard Ch.
    Reserved10: byte;
    Reserved11: byte;
    DESTADDR: word;  //Channel Destination Address for Standard Channels Only.
  const
    // Channel Enable
    ENABLEbm = $80;
    // Channel Software Reset
    RESETbm = $40;
    // Channel Repeat Mode
    REPEATbm = $20;
    // Channel Transfer Request
    TRFREQbm = $10;
    // Channel Single Shot Data Transfer
    SINGLEbm = $04;
    // Channel 2-bytes Burst Length
    BURSTLENbm = $01;
    // Channel Block Transfer Busy
    CHBUSYbm = $80;
    // Channel Block Transfer Pending
    CHPENDbm = $40;
    // Channel Transaction Error Interrupt Flag
    ERRIFbm = $20;
    // Channel Transaction Complete Interrupt Flag
    TRNIFbm = $10;
    // ERRINTLVL
    ERRINTLVLmask = $0C;
    ERRINTLVLOFF = $00;
    ERRINTLVLLO = $04;
    ERRINTLVLMED = $08;
    ERRINTLVLHI = $0C;
    // TRNINTLVL
    TRNINTLVLmask = $03;
    TRNINTLVLOFF = $00;
    TRNINTLVLLO = $01;
    TRNINTLVLMED = $02;
    TRNINTLVLHI = $03;
    // RELOAD
    RELOADmask = $30;
    RELOADNONE = $00;
    RELOADBLOCK = $10;
    RELOADBURST = $20;
    RELOADTRANSACTION = $30;
    // DIR
    DIRmask = $07;
    DIRFIXED = $00;
    DIRINC = $01;
    DIRMP1 = $04;
    DIRMP2 = $05;
    DIRMP3 = $06;
    // DESTRELOAD
    DESTRELOADmask = $30;
    DESTRELOADNONE = $00;
    DESTRELOADBLOCK = $10;
    DESTRELOADBURST = $20;
    DESTRELOADTRANSACTION = $30;
    // DESTDIR
    DESTDIRmask = $07;
    DESTDIRFIXED = $00;
    DESTDIRINC = $01;
    DESTDIRMP1 = $04;
    DESTDIRMP2 = $05;
    DESTDIRMP3 = $06;
    // TRIGSRC
    TRIGSRCmask = $FF;
    TRIGSRCOFF = $00;
    TRIGSRCEVSYS_CH0 = $01;
    TRIGSRCEVSYS_CH1 = $02;
    TRIGSRCEVSYS_CH2 = $03;
    TRIGSRCADCA_CH0 = $10;
    TRIGSRCDACA_CH0 = $15;
    TRIGSRCDACA_CH1 = $16;
    TRIGSRCTCC4_OVF = $40;
    TRIGSRCTCC4_ERR = $41;
    TRIGSRCTCC4_CCA = $42;
    TRIGSRCTCC4_CCB = $43;
    TRIGSRCTCC4_CCC = $44;
    TRIGSRCTCC4_CCD = $45;
    TRIGSRCTCC5_OVF = $46;
    TRIGSRCTCC5_ERR = $47;
    TRIGSRCTCC5_CCA = $48;
    TRIGSRCTCC5_CCB = $49;
    TRIGSRCSPIC_RXC = $4A;
    TRIGSRCSPIC_DRE = $4B;
    TRIGSRCUSARTC0_RXC = $4C;
    TRIGSRCUSARTC0_DRE = $4D;
    TRIGSRCTCD5_OVF = $66;
    TRIGSRCTCD5_ERR = $67;
    TRIGSRCTCD5_CCA = $68;
    TRIGSRCTCD5_CCB = $69;
    TRIGSRCUSARTD0_RXC = $6C;
    TRIGSRCUSARTD0_DRE = $6D;
  end;

  TEDMA = object //Enhanced DMA Controller
    CTRL: byte;  //Control
    Reserved1: byte;
    Reserved2: byte;
    INTFLAGS: byte;  //Transfer Interrupt Status
    STATUS: byte;  //Status
    Reserved5: byte;
    TEMP: byte;  //Temporary Register For 16-bit Access
    Reserved7: byte;
    Reserved8: byte;
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    CH0: TEDMA_CH;  //EDMA Channel 0
    CH1: TEDMA_CH;  //EDMA Channel 1
    CH2: TEDMA_CH;  //EDMA Channel 2
    CH3: TEDMA_CH;  //EDMA Channel 3
  const
    // Enable
    ENABLEbm = $80;
    // Software Reset
    RESETbm = $40;
    // EDMA_CHMODE
    CHMODEmask = $30;
    CHMODE_PER0123 = $00;
    CHMODE_STD0 = $10;
    CHMODE_STD2 = $20;
    CHMODE_STD02 = $30;
    // EDMA_DBUFMODE
    DBUFMODEmask = $0C;
    DBUFMODE_DISABLE = $00;
    DBUFMODE_BUF01 = $04;
    DBUFMODE_BUF23 = $08;
    DBUFMODE_BUF0123 = $0C;
    // EDMA_PRIMODE
    PRIMODEmask = $03;
    PRIMODE_RR0123 = $00;
    PRIMODE_RR123 = $01;
    PRIMODE_RR23 = $02;
    PRIMODE_CH0123 = $03;
    // Channel 3 Transaction Error Interrupt Flag
    CH3ERRIFbm = $80;
    // Channel 2 Transaction Error Interrupt Flag
    CH2ERRIFbm = $40;
    // Channel 1 Transaction Error Interrupt Flag
    CH1ERRIFbm = $20;
    // Channel 0 Transaction Error Interrupt Flag
    CH0ERRIFbm = $10;
    // Channel 3 Transaction Complete Interrupt Flag
    CH3TRNFIFbm = $08;
    // Channel 2 Transaction Complete Interrupt Flag
    CH2TRNFIFbm = $04;
    // Channel 1 Transaction Complete Interrupt Flag
    CH1TRNFIFbm = $02;
    // Channel 0 Transaction Complete Interrupt Flag
    CH0TRNFIFbm = $01;
    // Channel 3 Busy Flag
    CH3BUSYbm = $80;
    // Channel 2 Busy Flag
    CH2BUSYbm = $40;
    // Channel 1 Busy Flag
    CH1BUSYbm = $20;
    // Channel 0 Busy Flag
    CH0BUSYbm = $10;
    // Channel 3 Pending Flag
    CH3PENDbm = $08;
    // Channel 2 Pending Flag
    CH2PENDbm = $04;
    // Channel 1 Pending Flag
    CH1PENDbm = $02;
    // Channel 0 Pending Flag
    CH0PENDbm = $01;
  end;

  TEVSYS = object //Event System
    CH0MUX: byte;  //Event Channel 0 Multiplexer
    CH1MUX: byte;  //Event Channel 1 Multiplexer
    CH2MUX: byte;  //Event Channel 2 Multiplexer
    CH3MUX: byte;  //Event Channel 3 Multiplexer
    CH4MUX: byte;  //Event Channel 4 Multiplexer
    CH5MUX: byte;  //Event Channel 5 Multiplexer
    CH6MUX: byte;  //Event Channel 6 Multiplexer
    CH7MUX: byte;  //Event Channel 7 Multiplexer
    CH0CTRL: byte;  //Channel 0 Control Register
    CH1CTRL: byte;  //Channel 1 Control Register
    CH2CTRL: byte;  //Channel 2 Control Register
    CH3CTRL: byte;  //Channel 3 Control Register
    CH4CTRL: byte;  //Channel 4 Control Register
    CH5CTRL: byte;  //Channel 5 Control Register
    CH6CTRL: byte;  //Channel 6 Control Register
    CH7CTRL: byte;  //Channel 7 Control Register
    STROBE: byte;  //Event Strobe
    DATA: byte;  //Event Data
    DFCTRL: byte;  //Digital Filter Control Register
  const
    // EVSYS_CHMUX
    CHMUXmask = $FF;
    CHMUX_OFF = $00;
    CHMUX_RTC_OVF = $08;
    CHMUX_RTC_CMP = $09;
    CHMUX_ACA_CH0 = $10;
    CHMUX_ACA_CH1 = $11;
    CHMUX_ACA_WIN = $12;
    CHMUX_ADCA_CH0 = $20;
    CHMUX_PORTA_PIN0 = $50;
    CHMUX_PORTA_PIN1 = $51;
    CHMUX_PORTA_PIN2 = $52;
    CHMUX_PORTA_PIN3 = $53;
    CHMUX_PORTA_PIN4 = $54;
    CHMUX_PORTA_PIN5 = $55;
    CHMUX_PORTA_PIN6 = $56;
    CHMUX_PORTA_PIN7 = $57;
    CHMUX_PORTC_PIN0 = $60;
    CHMUX_PORTC_PIN1 = $61;
    CHMUX_PORTC_PIN2 = $62;
    CHMUX_PORTC_PIN3 = $63;
    CHMUX_PORTC_PIN4 = $64;
    CHMUX_PORTC_PIN5 = $65;
    CHMUX_PORTC_PIN6 = $66;
    CHMUX_PORTC_PIN7 = $67;
    CHMUX_PORTD_PIN0 = $68;
    CHMUX_PORTD_PIN1 = $69;
    CHMUX_PORTD_PIN2 = $6A;
    CHMUX_PORTD_PIN3 = $6B;
    CHMUX_PORTD_PIN4 = $6C;
    CHMUX_PORTD_PIN5 = $6D;
    CHMUX_PORTD_PIN6 = $6E;
    CHMUX_PORTD_PIN7 = $6F;
    CHMUX_PRESCALER_1 = $80;
    CHMUX_PRESCALER_2 = $81;
    CHMUX_PRESCALER_4 = $82;
    CHMUX_PRESCALER_8 = $83;
    CHMUX_PRESCALER_16 = $84;
    CHMUX_PRESCALER_32 = $85;
    CHMUX_PRESCALER_64 = $86;
    CHMUX_PRESCALER_128 = $87;
    CHMUX_PRESCALER_256 = $88;
    CHMUX_PRESCALER_512 = $89;
    CHMUX_PRESCALER_1024 = $8A;
    CHMUX_PRESCALER_2048 = $8B;
    CHMUX_PRESCALER_4096 = $8C;
    CHMUX_PRESCALER_8192 = $8D;
    CHMUX_PRESCALER_16384 = $8E;
    CHMUX_PRESCALER_32768 = $8F;
    CHMUX_XCL_UNF0 = $B0;
    CHMUX_XCL_UNF1 = $B1;
    CHMUX_XCL_CC0 = $B2;
    CHMUX_XCL_CC1 = $B3;
    CHMUX_XCL_PEC0 = $B4;
    CHMUX_XCL_PEC1 = $B5;
    CHMUX_XCL_LUT0 = $B6;
    CHMUX_XCL_LUT1 = $B7;
    CHMUX_TCC4_OVF = $C0;
    CHMUX_TCC4_ERR = $C1;
    CHMUX_TCC4_CCA = $C4;
    CHMUX_TCC4_CCB = $C5;
    CHMUX_TCC4_CCC = $C6;
    CHMUX_TCC4_CCD = $C7;
    CHMUX_TCC5_OVF = $C8;
    CHMUX_TCC5_ERR = $C9;
    CHMUX_TCC5_CCA = $CC;
    CHMUX_TCC5_CCB = $CD;
    CHMUX_TCD5_OVF = $D8;
    CHMUX_TCD5_ERR = $D9;
    CHMUX_TCD5_CCA = $DC;
    CHMUX_TCD5_CCB = $DD;
    // Rotary Decoder Enable
    ROTARYbm = $80;
    // EVSYS_QDIRM
    QDIRMmask = $60;
    QDIRM_00 = $00;
    QDIRM_01 = $20;
    QDIRM_10 = $40;
    QDIRM_11 = $60;
    // Quadrature Decoder Index Enable
    QDIENbm = $10;
    // Quadrature Decoder Enable
    QDENbm = $08;
    // EVSYS_DIGFILT
    DIGFILTmask = $07;
    DIGFILT_1SAMPLE = $00;
    DIGFILT_2SAMPLES = $01;
    DIGFILT_3SAMPLES = $02;
    DIGFILT_4SAMPLES = $03;
    DIGFILT_5SAMPLES = $04;
    DIGFILT_6SAMPLES = $05;
    DIGFILT_7SAMPLES = $06;
    DIGFILT_8SAMPLES = $07;
    // EVSYS_PRESCFILT
    PRESCFILTmask = $F0;
    PRESCFILT_CH04 = $10;
    PRESCFILT_CH15 = $20;
    PRESCFILT_CH26 = $40;
    PRESCFILT_CH37 = $80;
    // Prescaler Filter Select
    FILTSELbm = $08;
    // PRESC
    PRESCmask = $07;
    PRESCCLKPER_8 = $00;
    PRESCCLKPER_64 = $01;
    PRESCCLKPER_512 = $02;
    PRESCCLKPER_4096 = $03;
    PRESCCLKPER_32768 = $04;
  end;

  TNVM = object //Non-volatile Memory Controller
    ADDR0: byte;  //Address Register 0
    ADDR1: byte;  //Address Register 1
    ADDR2: byte;  //Address Register 2
    Reserved3: byte;
    DATA0: byte;  //Data Register 0
    DATA1: byte;  //Data Register 1
    DATA2: byte;  //Data Register 2
    Reserved7: byte;
    Reserved8: byte;
    Reserved9: byte;
    CMD: byte;  //Command
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    INTCTRL: byte;  //Interrupt Control
    Reserved14: byte;
    STATUS: byte;  //Status
    LOCKBITS: byte;  //Lock Bits
  const
    // NVM_CMD
    CMDmask = $7F;
    CMD_NO_OPERATION = $00;
    CMD_READ_USER_SIG_ROW = $01;
    CMD_READ_CALIB_ROW = $02;
    CMD_READ_FUSES = $07;
    CMD_WRITE_LOCK_BITS = $08;
    CMD_ERASE_USER_SIG_ROW = $18;
    CMD_WRITE_USER_SIG_ROW = $1A;
    CMD_ERASE_APP = $20;
    CMD_ERASE_APP_PAGE = $22;
    CMD_LOAD_FLASH_BUFFER = $23;
    CMD_WRITE_APP_PAGE = $24;
    CMD_ERASE_WRITE_APP_PAGE = $25;
    CMD_ERASE_FLASH_BUFFER = $26;
    CMD_ERASE_BOOT_PAGE = $2A;
    CMD_ERASE_FLASH_PAGE = $2B;
    CMD_WRITE_BOOT_PAGE = $2C;
    CMD_ERASE_WRITE_BOOT_PAGE = $2D;
    CMD_WRITE_FLASH_PAGE = $2E;
    CMD_ERASE_WRITE_FLASH_PAGE = $2F;
    CMD_ERASE_EEPROM = $30;
    CMD_ERASE_EEPROM_PAGE = $32;
    CMD_WRITE_EEPROM_PAGE = $34;
    CMD_ERASE_WRITE_EEPROM_PAGE = $35;
    CMD_ERASE_EEPROM_BUFFER = $36;
    CMD_APP_CRC = $38;
    CMD_BOOT_CRC = $39;
    CMD_FLASH_RANGE_CRC = $3A;
    CMD_CHIP_ERASE = $40;
    CMD_READ_NVM = $43;
    CMD_WRITE_FUSE = $4C;
    CMD_ERASE_BOOT = $68;
    CMD_FLASH_CRC = $78;
    // Command Execute
    CMDEXbm = $01;
    // EEPROM Power Reduction Enable
    EPRMbm = $02;
    // SPM Lock
    SPMLOCKbm = $01;
    // NVM_SPMLVL
    SPMLVLmask = $0C;
    SPMLVL_OFF = $00;
    SPMLVL_LO = $04;
    SPMLVL_MED = $08;
    SPMLVL_HI = $0C;
    // NVM_EELVL
    EELVLmask = $03;
    EELVL_OFF = $00;
    EELVL_LO = $01;
    EELVL_MED = $02;
    EELVL_HI = $03;
    // Non-volatile Memory Busy
    NVMBUSYbm = $80;
    // Flash Memory Busy
    FBUSYbm = $40;
    // EEPROM Page Buffer Active Loading
    EELOADbm = $02;
    // Flash Page Buffer Active Loading
    FLOADbm = $01;
    // NVM_BLBB
    BLBBmask = $C0;
    BLBB_RWLOCK = $00;
    BLBB_RLOCK = $40;
    BLBB_WLOCK = $80;
    BLBB_NOLOCK = $C0;
    // NVM_BLBA
    BLBAmask = $30;
    BLBA_RWLOCK = $00;
    BLBA_RLOCK = $10;
    BLBA_WLOCK = $20;
    BLBA_NOLOCK = $30;
    // NVM_BLBAT
    BLBATmask = $0C;
    BLBAT_RWLOCK = $00;
    BLBAT_RLOCK = $04;
    BLBAT_WLOCK = $08;
    BLBAT_NOLOCK = $0C;
    // NVM_LB
    LBmask = $03;
    LB_RWLOCK = $00;
    LB_WLOCK = $02;
    LB_NOLOCK = $03;
  end;

  TADC_CH = object //ADC Channel
    CTRL: byte;  //Control Register
    MUXCTRL: byte;  //MUX Control
    INTCTRL: byte;  //Channel Interrupt Control Register
    INTFLAGS: byte;  //Interrupt Flags
    RES: word;  //Channel Result
    SCAN: byte;  //Input Channel Scan
    CORRCTRL: byte;  //Correction Control Register
    OFFSETCORR0: byte;  //Offset Correction Register 0
    OFFSETCORR1: byte;  //Offset Correction Register 1
    GAINCORR0: byte;  //Gain Correction Register 0
    GAINCORR1: byte;  //Gain Correction Register 1
    AVGCTRL: byte;  //Average Control Register
  const
    // Channel Start Conversion
    STARTbm = $80;
    // GAIN
    GAINmask = $1C;
    GAIN1X = $00;
    GAIN2X = $04;
    GAIN4X = $08;
    GAIN8X = $0C;
    GAIN16X = $10;
    GAIN32X = $14;
    GAIN64X = $18;
    GAINDIV2 = $1C;
    // INPUTMODE
    INPUTMODEmask = $03;
    INPUTMODEINTERNAL = $00;
    INPUTMODESINGLEENDED = $01;
    INPUTMODEDIFFWGAINL = $02;
    INPUTMODEDIFFWGAINH = $03;
    // MUXPOS
    MUXPOSmask = $78;
    MUXPOSPIN0 = $00;
    MUXPOSPIN1 = $08;
    MUXPOSPIN2 = $10;
    MUXPOSPIN3 = $18;
    MUXPOSPIN4 = $20;
    MUXPOSPIN5 = $28;
    MUXPOSPIN6 = $30;
    MUXPOSPIN7 = $38;
    MUXPOSPIN8 = $40;
    MUXPOSPIN9 = $48;
    MUXPOSPIN10 = $50;
    MUXPOSPIN11 = $58;
    MUXPOSPIN12 = $60;
    MUXPOSPIN13 = $68;
    MUXPOSPIN14 = $70;
    MUXPOSPIN15 = $78;
    // MUXINT
    MUXINTmask = $78;
    MUXINTTEMP = $00;
    MUXINTBANDGAP = $08;
    MUXINTSCALEDVCC = $10;
    MUXINTDAC = $18;
    // MUXNEG
    MUXNEGmask = $07;
    MUXNEGPIN0 = $00;
    MUXNEGPIN1 = $01;
    MUXNEGPIN2 = $02;
    MUXNEGPIN3 = $03;
    MUXNEGPIN4 = $00;
    MUXNEGPIN5 = $01;
    MUXNEGPIN6 = $02;
    MUXNEGPIN7 = $03;
    MUXNEGGND_MODE3 = $05;
    MUXNEGINTGND_MODE3 = $07;
    MUXNEGINTGND_MODE4 = $04;
    MUXNEGGND_MODE4 = $07;
    // INTMODE
    INTMODEmask = $0C;
    INTMODECOMPLETE = $00;
    INTMODEBELOW = $04;
    INTMODEABOVE = $0C;
    // INTLVL
    INTLVLmask = $03;
    INTLVLOFF = $00;
    INTLVLLO = $01;
    INTLVLMED = $02;
    INTLVLHI = $03;
    // Channel Interrupt Flag
    IFbm = $01;
    // Positive MUX Setting Offset
    INPUTOFFSET0bm = $10;
    INPUTOFFSET1bm = $20;
    INPUTOFFSET2bm = $40;
    INPUTOFFSET3bm = $80;
    // Number of Channels Included in Scan
    INPUTSCAN0bm = $01;
    INPUTSCAN1bm = $02;
    INPUTSCAN2bm = $04;
    INPUTSCAN3bm = $08;
    // Correction Enable
    CORRENbm = $01;
    // Offset Correction Byte 1
    OFFSETCORR0bm = $01;
    OFFSETCORR1bm = $02;
    OFFSETCORR2bm = $04;
    OFFSETCORR3bm = $08;
    // Gain Correction Byte 1
    GAINCORR0bm = $01;
    GAINCORR1bm = $02;
    GAINCORR2bm = $04;
    GAINCORR3bm = $08;
    // Right Shift
    RIGHTSHIFT0bm = $10;
    RIGHTSHIFT1bm = $20;
    RIGHTSHIFT2bm = $40;
    // ADC_SAMPNUM
    SAMPNUMmask = $0F;
    SAMPNUM_1X = $00;
    SAMPNUM_2X = $01;
    SAMPNUM_4X = $02;
    SAMPNUM_8X = $03;
    SAMPNUM_16X = $04;
    SAMPNUM_32X = $05;
    SAMPNUM_64X = $06;
    SAMPNUM_128X = $07;
    SAMPNUM_256X = $08;
    SAMPNUM_512X = $09;
    SAMPNUM_1024X = $0A;
  end;

  TADC = object //Analog-to-Digital Converter
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    REFCTRL: byte;  //Reference Control
    EVCTRL: byte;  //Event Control
    PRESCALER: byte;  //Clock Prescaler
    Reserved5: byte;
    INTFLAGS: byte;  //Interrupt Flags
    TEMP: byte;  //Temporary Register
    SAMPCTRL: byte;  //ADC Sampling Time Control Register
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    CAL: byte;  //Calibration Value
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    CH0RES: word;  //Channel 0 Result
    Reserved18: byte;
    Reserved19: byte;
    Reserved20: byte;
    Reserved21: byte;
    Reserved22: byte;
    Reserved23: byte;
    CMP: word;  //Compare Value
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    Reserved30: byte;
    Reserved31: byte;
    CH0: TADC_CH;  //ADC Channel 0
  const
    // Start Conversion
    STARTbm = $04;
    // ADC Flush
    FLUSHbm = $02;
    // Enable ADC
    ENABLEbm = $01;
    // ADC_CURRLIMIT
    CURRLIMITmask = $60;
    CURRLIMIT_NO = $00;
    CURRLIMIT_LOW = $20;
    CURRLIMIT_MED = $40;
    CURRLIMIT_HIGH = $60;
    // Conversion Mode
    CONMODEbm = $10;
    // Free Running Mode Enable
    FREERUNbm = $08;
    // ADC_RESOLUTION
    RESOLUTIONmask = $06;
    RESOLUTION_12BIT = $00;
    RESOLUTION_MT12BIT = $02;
    RESOLUTION_8BIT = $04;
    RESOLUTION_LEFT12BIT = $06;
    // ADC_REFSEL
    REFSELmask = $70;
    REFSEL_INT1V = $00;
    REFSEL_INTVCC = $10;
    REFSEL_AREFA = $20;
    REFSEL_AREFD = $30;
    REFSEL_INTVCC2 = $40;
    // Bandgap enable
    BANDGAPbm = $02;
    // Temperature Reference Enable
    TEMPREFbm = $01;
    // ADC_EVSEL
    EVSELmask = $38;
    EVSEL_0 = $00;
    EVSEL_1 = $08;
    EVSEL_2 = $10;
    EVSEL_3 = $18;
    EVSEL_4 = $20;
    EVSEL_5 = $28;
    EVSEL_6 = $30;
    EVSEL_7 = $38;
    // ADC_EVACT
    EVACTmask = $07;
    EVACT_NONE = $00;
    EVACT_CH0 = $01;
    EVACT_SYNCSWEEP = $06;
    // ADC_PRESCALER
    PRESCALERmask = $07;
    PRESCALER_DIV4 = $00;
    PRESCALER_DIV8 = $01;
    PRESCALER_DIV16 = $02;
    PRESCALER_DIV32 = $03;
    PRESCALER_DIV64 = $04;
    PRESCALER_DIV128 = $05;
    PRESCALER_DIV256 = $06;
    PRESCALER_DIV512 = $07;
    // Channel 0 Interrupt Flag
    CH0IFbm = $01;
    // Sampling time control register
    SAMPVAL0bm = $01;
    SAMPVAL1bm = $02;
    SAMPVAL2bm = $04;
    SAMPVAL3bm = $08;
    SAMPVAL4bm = $10;
    SAMPVAL5bm = $20;
  end;

  TDAC = object //Digital-to-Analog Converter
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control Register C
    EVCTRL: byte;  //Event Input Control
    Reserved4: byte;
    STATUS: byte;  //Status
    Reserved6: byte;
    Reserved7: byte;
    CH0GAINCAL: byte;  //Gain Calibration
    CH0OFFSETCAL: byte;  //Offset Calibration
    CH1GAINCAL: byte;  //Gain Calibration
    CH1OFFSETCAL: byte;  //Offset Calibration
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
    CH0DATA: word;  //Channel 0 Data
    CH1DATA: word;  //Channel 1 Data
  const
    // Internal Output Enable
    IDOENbm = $10;
    // Channel 1 Output Enable
    CH1ENbm = $08;
    // Channel 0 Output Enable
    CH0ENbm = $04;
    // Low Power Mode
    LPMODEbm = $02;
    // Enable
    ENABLEbm = $01;
    // DAC_CHSEL
    CHSELmask = $60;
    CHSEL_SINGLE = $00;
    CHSEL_SINGLE1 = $20;
    CHSEL_DUAL = $40;
    // Channel 1 Event Trig Enable
    CH1TRIGbm = $02;
    // Channel 0 Event Trig Enable
    CH0TRIGbm = $01;
    // DAC_REFSEL
    REFSELmask = $18;
    REFSEL_INT1V = $00;
    REFSEL_AVCC = $08;
    REFSEL_AREFA = $10;
    REFSEL_AREFD = $18;
    REFSEL_AREFB = $18;
    // Left-adjust Result
    LEFTADJbm = $01;
    // Separate Event Channel Input for Channel 1
    EVSPLITbm = $08;
    // DAC_EVSEL
    EVSELmask = $07;
    EVSEL_0 = $00;
    EVSEL_1 = $01;
    EVSEL_2 = $02;
    EVSEL_3 = $03;
    EVSEL_4 = $04;
    EVSEL_5 = $05;
    EVSEL_6 = $06;
    EVSEL_7 = $07;
    // Channel 1 Data Register Empty
    CH1DREbm = $02;
    // Channel 0 Data Register Empty
    CH0DREbm = $01;
    // Gain Calibration
    CH0GAINCAL0bm = $01;
    CH0GAINCAL1bm = $02;
    CH0GAINCAL2bm = $04;
    CH0GAINCAL3bm = $08;
    CH0GAINCAL4bm = $10;
    CH0GAINCAL5bm = $20;
    CH0GAINCAL6bm = $40;
    // Offset Calibration
    CH0OFFSETCAL0bm = $01;
    CH0OFFSETCAL1bm = $02;
    CH0OFFSETCAL2bm = $04;
    CH0OFFSETCAL3bm = $08;
    CH0OFFSETCAL4bm = $10;
    CH0OFFSETCAL5bm = $20;
    CH0OFFSETCAL6bm = $40;
    // Gain Calibration
    CH1GAINCAL0bm = $01;
    CH1GAINCAL1bm = $02;
    CH1GAINCAL2bm = $04;
    CH1GAINCAL3bm = $08;
    CH1GAINCAL4bm = $10;
    CH1GAINCAL5bm = $20;
    CH1GAINCAL6bm = $40;
    // Offset Calibration
    CH1OFFSETCAL0bm = $01;
    CH1OFFSETCAL1bm = $02;
    CH1OFFSETCAL2bm = $04;
    CH1OFFSETCAL3bm = $08;
    CH1OFFSETCAL4bm = $10;
    CH1OFFSETCAL5bm = $20;
    CH1OFFSETCAL6bm = $40;
  end;

  TAC = object //Analog Comparator
    AC0CTRL: byte;  //Analog Comparator 0 Control
    AC1CTRL: byte;  //Analog Comparator 1 Control
    AC0MUXCTRL: byte;  //Analog Comparator 0 MUX Control
    AC1MUXCTRL: byte;  //Analog Comparator 1 MUX Control
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    WINCTRL: byte;  //Window Mode Control
    STATUS: byte;  //Status
    CURRCTRL: byte;  //Current Source Control Register
    CURRCALIB: byte;  //Current Source Calibration Register
  const
    // AC_INTMODE
    INTMODEmask = $C0;
    INTMODE_BOTHEDGES = $00;
    INTMODE_FALLING = $80;
    INTMODE_RISING = $C0;
    // AC_INTLVL
    INTLVLmask = $30;
    INTLVL_OFF = $00;
    INTLVL_LO = $10;
    INTLVL_MED = $20;
    INTLVL_HI = $30;
    // AC_HYSMODE
    HYSMODEmask = $06;
    HYSMODE_NO = $00;
    HYSMODE_SMALL = $02;
    HYSMODE_LARGE = $04;
    // Enable
    ENABLEbm = $01;
    // AC_MUXPOS
    MUXPOSmask = $38;
    MUXPOS_PIN0 = $00;
    MUXPOS_PIN1 = $08;
    MUXPOS_PIN2 = $10;
    MUXPOS_PIN3 = $18;
    MUXPOS_PIN4 = $20;
    MUXPOS_PIN5 = $28;
    MUXPOS_PIN6 = $30;
    MUXPOS_DAC = $38;
    // AC_MUXNEG
    MUXNEGmask = $07;
    MUXNEG_PIN0 = $00;
    MUXNEG_PIN1 = $01;
    MUXNEG_PIN3 = $02;
    MUXNEG_PIN5 = $03;
    MUXNEG_PIN7 = $04;
    MUXNEG_DAC = $05;
    MUXNEG_BANDGAP = $06;
    MUXNEG_SCALER = $07;
    // Analog Comparator 1 Output Invert Enable
    AC1INVENbm = $08;
    // Analog Comparator 0 Output Invert Enable
    AC0INVENbm = $04;
    // Analog Comparator 1 Output Enable
    AC1OUTbm = $02;
    // Analog Comparator 0 Output Enable
    AC0OUTbm = $01;
    // VCC Voltage Scaler Factor
    SCALEFAC0bm = $01;
    SCALEFAC1bm = $02;
    SCALEFAC2bm = $04;
    SCALEFAC3bm = $08;
    SCALEFAC4bm = $10;
    SCALEFAC5bm = $20;
    // Window Mode Enable
    WENbm = $10;
    // AC_WINTMODE
    WINTMODEmask = $0C;
    WINTMODE_ABOVE = $00;
    WINTMODE_INSIDE = $04;
    WINTMODE_BELOW = $08;
    WINTMODE_OUTSIDE = $0C;
    // AC_WINTLVL
    WINTLVLmask = $03;
    WINTLVL_OFF = $00;
    WINTLVL_LO = $01;
    WINTLVL_MED = $02;
    WINTLVL_HI = $03;
    // AC_WSTATE
    WSTATEmask = $C0;
    WSTATE_ABOVE = $00;
    WSTATE_INSIDE = $40;
    WSTATE_BELOW = $80;
    // Analog Comparator 1 State
    AC1STATEbm = $20;
    // Analog Comparator 0 State
    AC0STATEbm = $10;
    // Window Mode Interrupt Flag
    WIFbm = $04;
    // Analog Comparator 1 Interrupt Flag
    AC1IFbm = $02;
    // Analog Comparator 0 Interrupt Flag
    AC0IFbm = $01;
    // Current Source Enable
    CURRENTbm = $80;
    // Current Mode
    CURRMODEbm = $40;
    // Analog Comparator 1 current source output
    AC1CURRbm = $02;
    // Analog Comparator 0 current source output
    AC0CURRbm = $01;
    // Current Source Calibration
    CALIB0bm = $01;
    CALIB1bm = $02;
    CALIB2bm = $04;
    CALIB3bm = $08;
  end;

  TRTC = object //Real-Time Counter
    CTRL: byte;  //Control Register
    STATUS: byte;  //Status Register
    INTCTRL: byte;  //Interrupt Control Register
    INTFLAGS: byte;  //Interrupt Flags
    TEMP: byte;  //Temporary register
    Reserved5: byte;
    CALIB: byte;  //Calibration Register
    Reserved7: byte;
    CNT: word;  //Count Register
    PER: word;  //Period Register
    COMP: word;  //Compare Register
  const
    // Correction Enable
    CORRENbm = $08;
    // RTC_PRESCALER
    PRESCALERmask = $07;
    PRESCALER_OFF = $00;
    PRESCALER_DIV1 = $01;
    PRESCALER_DIV2 = $02;
    PRESCALER_DIV8 = $03;
    PRESCALER_DIV16 = $04;
    PRESCALER_DIV64 = $05;
    PRESCALER_DIV256 = $06;
    PRESCALER_DIV1024 = $07;
    // Synchronization Busy Flag
    SYNCBUSYbm = $01;
    // RTC_COMPINTLVL
    COMPINTLVLmask = $0C;
    COMPINTLVL_OFF = $00;
    COMPINTLVL_LO = $04;
    COMPINTLVL_MED = $08;
    COMPINTLVL_HI = $0C;
    // RTC_OVFINTLVL
    OVFINTLVLmask = $03;
    OVFINTLVL_OFF = $00;
    OVFINTLVL_LO = $01;
    OVFINTLVL_MED = $02;
    OVFINTLVL_HI = $03;
    // Compare Match Interrupt Flag
    COMPIFbm = $02;
    // Overflow Interrupt Flag
    OVFIFbm = $01;
    // Correction Sign
    SIGNbm = $80;
    // Error Value
    ERROR0bm = $01;
    ERROR1bm = $02;
    ERROR2bm = $04;
    ERROR3bm = $08;
    ERROR4bm = $10;
    ERROR5bm = $20;
    ERROR6bm = $40;
  end;

  TXCL = object //XMEGA Custom Logic
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control Register C
    CTRLD: byte;  //Control Register D
    CTRLE: byte;  //Control Register E
    CTRLF: byte;  //Control Register F
    CTRLG: byte;  //Control Register G
    INTCTRL: byte;  //Interrupt Control Register
    INTFLAGS: byte;  //Interrupt Flag Register
    PLC: byte;  //Peripheral Length Control Register 
    CNTL: byte;  //Counter Register Low
    CNTH: byte;  //Counter Register High
    CMPL: byte;  //Compare Register Low
    CMPH: byte;  //Compare Register High
    PERCAPTL: byte;  //Period or Capture Register Low
    PERCAPTH: byte;  //Period or Capture Register High
  const
    // XCL_LUT0OUTEN
    LUT0OUTENmask = $C0;
    LUT0OUTEN_DISABLE = $00;
    LUT0OUTEN_PIN0 = $40;
    LUT0OUTEN_PIN4 = $80;
    // XCL_PORTSEL
    PORTSELmask = $30;
    PORTSEL_PC = $00;
    PORTSEL_PD = $10;
    // XCL_LUTCONF
    LUTCONFmask = $07;
    LUTCONF_2LUT2IN = $00;
    LUTCONF_2LUT1IN = $01;
    LUTCONF_2LUT3IN = $02;
    LUTCONF_1LUT3IN = $03;
    LUTCONF_MUX = $04;
    LUTCONF_DLATCH = $05;
    LUTCONF_RSLATCH = $06;
    LUTCONF_DFF = $07;
    // IN3SEL
    IN3SELmask = $C0;
    IN3SELEVSYS = $00;
    IN3SELXCL = $40;
    IN3SELPINL = $80;
    IN3SELPINH = $C0;
    // IN2SEL
    IN2SELmask = $30;
    IN2SELEVSYS = $00;
    IN2SELXCL = $10;
    IN2SELPINL = $20;
    IN2SELPINH = $30;
    // IN1SEL
    IN1SELmask = $0C;
    IN1SELEVSYS = $00;
    IN1SELXCL = $04;
    IN1SELPINL = $08;
    IN1SELPINH = $0C;
    // IN0SEL
    IN0SELmask = $03;
    IN0SELEVSYS = $00;
    IN0SELXCL = $01;
    IN0SELPINL = $02;
    IN0SELPINH = $03;
    // Asynchronous Event Line Selection for LUT1
    EVASYSEL1bm = $80;
    // Asynchronous Event Line Selection for LUT0
    EVASYSEL0bm = $40;
    // XCL_DLYSEL
    DLYSELmask = $30;
    DLYSEL_DLY11 = $00;
    DLYSEL_DLY12 = $10;
    DLYSEL_DLY21 = $20;
    DLYSEL_DLY22 = $30;
    // DLY1CONF
    DLY1CONFmask = $0C;
    DLY1CONFDISABLE = $00;
    DLY1CONFIN = $04;
    DLY1CONFOUT = $08;
    // DLY0CONF
    DLY0CONFmask = $03;
    DLY0CONFDISABLE = $00;
    DLY0CONFIN = $01;
    DLY0CONFOUT = $02;
    // Truth Table of LUT1
    TRUTH10bm = $10;
    TRUTH11bm = $20;
    TRUTH12bm = $40;
    TRUTH13bm = $80;
    // Truth Table of LUT0
    TRUTH00bm = $01;
    TRUTH01bm = $02;
    TRUTH02bm = $04;
    TRUTH03bm = $08;
    // XCL_CMDSEL
    CMDSELmask = $80;
    CMDSEL_NONE = $00;
    CMDSEL_RESTART = $80;
    // XCL_TCSEL
    TCSELmask = $70;
    TCSEL_TC16 = $00;
    TCSEL_BTC0 = $10;
    TCSEL_BTC01 = $20;
    TCSEL_BTC0PEC1 = $30;
    TCSEL_PEC0BTC1 = $40;
    TCSEL_PEC01 = $50;
    TCSEL_BTC0PEC2 = $60;
    // XCL_CLKSEL
    CLKSELmask = $0F;
    CLKSEL_OFF = $00;
    CLKSEL_DIV1 = $01;
    CLKSEL_DIV2 = $02;
    CLKSEL_DIV4 = $03;
    CLKSEL_DIV8 = $04;
    CLKSEL_DIV64 = $05;
    CLKSEL_DIV256 = $06;
    CLKSEL_DIV1024 = $07;
    CLKSEL_EVCH0 = $08;
    CLKSEL_EVCH1 = $09;
    CLKSEL_EVCH2 = $0A;
    CLKSEL_EVCH3 = $0B;
    CLKSEL_EVCH4 = $0C;
    CLKSEL_EVCH5 = $0D;
    CLKSEL_EVCH6 = $0E;
    CLKSEL_EVCH7 = $0F;
    // XCL_CMDEN
    CMDENmask = $C0;
    CMDEN_DISABLE = $00;
    CMDEN_CMD0 = $40;
    CMDEN_CMD1 = $80;
    CMDEN_CMD01 = $C0;
    // CMP1
    CMP1mask = $20;
    CMP1CLEAR = $00;
    CMP1SET = $20;
    // CMP0
    CMP0mask = $10;
    CMP0CLEAR = $00;
    CMP0SET = $10;
    // Compare or Capture Channel 1 Enable
    CCEN1bm = $08;
    // Compare or Capture Channel 0 Enable
    CCEN0bm = $04;
    // MODE
    MODEmask = $03;
    MODENORMAL = $00;
    MODECAPT = $01;
    MODEPWM = $02;
    MODE1SHOT = $03;
    // Event Action Enable
    EVACTENbm = $80;
    // EVACT1
    EVACT1mask = $60;
    EVACT1INPUT = $00;
    EVACT1FREQ = $20;
    EVACT1PW = $40;
    EVACT1RESTART = $60;
    // EVACT0
    EVACT0mask = $18;
    EVACT0INPUT = $00;
    EVACT0FREQ = $08;
    EVACT0PW = $10;
    EVACT0RESTART = $18;
    // XCL_EVSRC
    EVSRCmask = $07;
    EVSRC_EVCH0 = $00;
    EVSRC_EVCH1 = $01;
    EVSRC_EVCH2 = $02;
    EVSRC_EVCH3 = $03;
    EVSRC_EVCH4 = $04;
    EVSRC_EVCH5 = $05;
    EVSRC_EVCH6 = $06;
    EVSRC_EVCH7 = $07;
    // Underflow 1 Interrupt Enable
    UNF1IEbm = $80;
    // Peripheral Counter 1 Interrupt Enable
    PEC1IEbm = $80;
    // Peripheral High Counter 2 Interrupt Enable
    PEC21IEbm = $80;
    // Underflow 0 Interrupt Enable
    UNF0IEbm = $40;
    // Peripheral Counter 0 Interrupt Enable
    PEC0IEbm = $40;
    // Compare Or Capture 1 Interrupt Enable
    CC1IEbm = $20;
    // Peripheral Low Counter 2 Interrupt Enable
    PEC20IEbm = $20;
    // Compare Or Capture 0 Interrupt Enable
    CC0IEbm = $10;
    // UNFINTLVL
    UNFINTLVLmask = $0C;
    UNFINTLVLOFF = $00;
    UNFINTLVLLO = $04;
    UNFINTLVLMED = $08;
    UNFINTLVLHI = $0C;
    // CCINTLVL
    CCINTLVLmask = $03;
    CCINTLVLOFF = $00;
    CCINTLVLLO = $01;
    CCINTLVLMED = $02;
    CCINTLVLHI = $03;
    // Timer/Counter 1 Underflow Interrupt Flag
    UNF1IFbm = $80;
    // Peripheral Counter 1 Interrupt Flag
    PEC1IFbm = $80;
    // Peripheral High Counter 2 Interrupt Flag
    PEC21IFbm = $80;
    // Timer/Counter 0 Underflow Interrupt Flag
    UNF0IFbm = $40;
    // Peripheral Counter 0 Interrupt Flag
    PEC0IFbm = $40;
    // Compare or Capture Channel 1 Interrupt Flag
    CC1IFbm = $20;
    // Peripheral Low Counter 2 Interrupt Flag
    PEC20IFbm = $20;
    // Compare or Capture Channel 0 Interrupt Flag
    CC0IFbm = $10;
    // Peripheral High Counter 2 Bits
    PCNT210bm = $10;
    PCNT211bm = $20;
    PCNT212bm = $40;
    PCNT213bm = $80;
    // Peripheral Low Counter 2 Bits
    PCNT200bm = $01;
    PCNT201bm = $02;
    PCNT202bm = $04;
    PCNT203bm = $08;
  end;

  TTWI_MASTER = object //
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control Register C
    STATUS: byte;  //Status Register
    BAUD: byte;  //Baud Rate Control Register
    ADDR: byte;  //Address Register
    DATA: byte;  //Data Register
  const
    // INTLVL
    INTLVLmask = $C0;
    INTLVLOFF = $00;
    INTLVLLO = $40;
    INTLVLMED = $80;
    INTLVLHI = $C0;
    // Read Interrupt Enable
    RIENbm = $20;
    // Write Interrupt Enable
    WIENbm = $10;
    // Enable TWI Master
    ENABLEbm = $08;
    // TIMEOUT
    TIMEOUTmask = $0C;
    TIMEOUTDISABLED = $00;
    TIMEOUT50US = $04;
    TIMEOUT100US = $08;
    TIMEOUT200US = $0C;
    // Quick Command Enable
    QCENbm = $02;
    // Smart Mode Enable
    SMENbm = $01;
    // Ttimeout Enable
    TTOUTENbm = $10;
    // Slave Extend Timeout Enable
    TSEXTENbm = $20;
    // Master Extend Timeout Enable
    TMEXTENbm = $40;
    // Timeout Interrupt Enable
    TOIEbm = $80;
    // Acknowledge Action
    ACKACTbm = $04;
    // CMD
    CMDmask = $03;
    CMDNOACT = $00;
    CMDREPSTART = $01;
    CMDRECVTRANS = $02;
    CMDSTOP = $03;
    // Read Interrupt Flag
    RIFbm = $80;
    // Write Interrupt Flag
    WIFbm = $40;
    // Clock Hold
    CLKHOLDbm = $20;
    // Received Acknowledge
    RXACKbm = $10;
    // Arbitration Lost
    ARBLOSTbm = $08;
    // Bus Error
    BUSERRbm = $04;
    // BUSSTATE
    BUSSTATEmask = $03;
    BUSSTATEUNKNOWN = $00;
    BUSSTATEIDLE = $01;
    BUSSTATEOWNER = $02;
    BUSSTATEBUSY = $03;
  end;

  TTWI_SLAVE = object //
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    STATUS: byte;  //Status Register
    ADDR: byte;  //Address Register
    DATA: byte;  //Data Register
    ADDRMASK: byte;  //Address Mask Register
  const
    // INTLVL
    INTLVLmask = $C0;
    INTLVLOFF = $00;
    INTLVLLO = $40;
    INTLVLMED = $80;
    INTLVLHI = $C0;
    // Data Interrupt Enable
    DIENbm = $20;
    // Address/Stop Interrupt Enable
    APIENbm = $10;
    // Enable TWI Slave
    ENABLEbm = $08;
    // Stop Interrupt Enable
    PIENbm = $04;
    // Promiscuous Mode Enable
    PMENbm = $02;
    // Smart Mode Enable
    SMENbm = $01;
    // Acknowledge Action
    ACKACTbm = $04;
    // CMD
    CMDmask = $03;
    CMDNOACT = $00;
    CMDCOMPTRANS = $02;
    CMDRESPONSE = $03;
    // Ttimeout Enable
    TTOUTENbm = $10;
    // Timeout Interrupt Enable
    TOIEbm = $80;
    // Data Interrupt Flag
    DIFbm = $80;
    // Address/Stop Interrupt Flag
    APIFbm = $40;
    // Clock Hold
    CLKHOLDbm = $20;
    // Received Acknowledge
    RXACKbm = $10;
    // Collision
    COLLbm = $08;
    // Bus Error
    BUSERRbm = $04;
    // Read/Write Direction
    DIRbm = $02;
    // Slave Address or Stop
    APbm = $01;
    // Address Mask
    ADDRMASK0bm = $02;
    ADDRMASK1bm = $04;
    ADDRMASK2bm = $08;
    ADDRMASK3bm = $10;
    ADDRMASK4bm = $20;
    ADDRMASK5bm = $40;
    ADDRMASK6bm = $80;
    // Address Enable
    ADDRENbm = $01;
  end;

  TTWI_TIMEOUT = object //
    TOS: byte;  //Timeout Status Register
    TOCONF: byte;  //Timeout Configuration Register
  const
    // Master Ttimeout Interrupt Flag
    TTOUTMIFbm = $01;
    // Slave Extend Interrupt Flag
    TSEXTIFbm = $02;
    // Master Extend Interrupt Flag
    TMEXTIFbm = $04;
    // Slave Ttimeout Interrupt Flag
    TTOUTSIFbm = $10;
    // TTOUTMSEL
    TTOUTMSELmask = $07;
    TTOUTMSEL25MS = $00;
    TTOUTMSEL24MS = $01;
    TTOUTMSEL23MS = $02;
    TTOUTMSEL22MS = $03;
    TTOUTMSEL26MS = $04;
    TTOUTMSEL27MS = $05;
    TTOUTMSEL28MS = $06;
    TTOUTMSEL29MS = $07;
    // TMSEXTSEL
    TMSEXTSELmask = $18;
    TMSEXTSEL10MS25MS = $00;
    TMSEXTSEL9MS24MS = $08;
    TMSEXTSEL11MS26MS = $10;
    TMSEXTSEL12MS27MS = $18;
    // TTOUTSSEL
    TTOUTSSELmask = $E0;
    TTOUTSSEL25MS = $00;
    TTOUTSSEL24MS = $20;
    TTOUTSSEL23MS = $40;
    TTOUTSSEL22MS = $60;
    TTOUTSSEL26MS = $80;
    TTOUTSSEL27MS = $A0;
    TTOUTSSEL28MS = $C0;
    TTOUTSSEL29MS = $E0;
  end;

  TTWI = object //Two-Wire Interface
    CTRL: byte;  //TWI Common Control Register
    MASTER: TTWI_MASTER;  //TWI master module
    SLAVE: TTWI_SLAVE;  //TWI slave module
    TIMEOUT: word;  //TWI SMBUS timeout module
  const
    // Bridge Enable
    BRIDGEENbm = $80;
    // Slave Fast Mode Plus Enable
    SFMPENbm = $40;
    // SSDAHOLD
    SSDAHOLDmask = $30;
    SSDAHOLDOFF = $00;
    SSDAHOLD50NS = $10;
    SSDAHOLD300NS = $20;
    SSDAHOLD400NS = $30;
    // FMPLUS Enable
    FMPENbm = $08;
    // TWI_SDAHOLD
    SDAHOLDmask = $06;
    SDAHOLD_OFF = $00;
    SDAHOLD_50NS = $02;
    SDAHOLD_300NS = $04;
    SDAHOLD_400NS = $06;
    // External Driver Interface Enable
    EDIENbm = $01;
  end;

  TPORT = object //I/O Ports
    DIR: byte;  //I/O Port Data Direction
    DIRSET: byte;  //I/O Port Data Direction Set
    DIRCLR: byte;  //I/O Port Data Direction Clear
    DIRTGL: byte;  //I/O Port Data Direction Toggle
    OUT_: byte;  //I/O Port Output
    OUTSET: byte;  //I/O Port Output Set
    OUTCLR: byte;  //I/O Port Output Clear
    OUTTGL: byte;  //I/O Port Output Toggle
    IN_: byte;  //I/O port Input
    INTCTRL: byte;  //Interrupt Control Register
    INTMASK: byte;  //Port Interrupt Mask
    Reserved11: byte;
    INTFLAGS: byte;  //Interrupt Flag Register
    Reserved13: byte;
    REMAP: byte;  //Pin Remap Register
    Reserved15: byte;
    PIN0CTRL: byte;  //Pin 0 Control Register
    PIN1CTRL: byte;  //Pin 1 Control Register
    PIN2CTRL: byte;  //Pin 2 Control Register
    PIN3CTRL: byte;  //Pin 3 Control Register
    PIN4CTRL: byte;  //Pin 4 Control Register
    PIN5CTRL: byte;  //Pin 5 Control Register
    PIN6CTRL: byte;  //Pin 6 Control Register
    PIN7CTRL: byte;  //Pin 7 Control Register
  const
    // PORT_INTLVL
    INTLVLmask = $03;
    INTLVL_OFF = $00;
    INTLVL_LO = $01;
    INTLVL_MED = $02;
    INTLVL_HI = $03;
    // Pin 7 Interrupt Flag
    INT7IFbm = $80;
    // Pin 6 Interrupt Flag
    INT6IFbm = $40;
    // Pin 5 Interrupt Flag
    INT5IFbm = $20;
    // Pin 4 Interrupt Flag
    INT4IFbm = $10;
    // Pin 3 Interrupt Flag
    INT3IFbm = $08;
    // Pin 2 Interrupt Flag
    INT2IFbm = $04;
    // Pin 1 Interrupt Flag
    INT1IFbm = $02;
    // Pin 0 Interrupt Flag
    INT0IFbm = $01;
    // Usart0
    USART0bm = $10;
    // Timer/Counter 4 Output Compare D
    TC4Dbm = $08;
    // Timer/Counter 4 Output Compare C
    TC4Cbm = $04;
    // Timer/Counter 4 Output Compare B
    TC4Bbm = $02;
    // Timer/Counter 4 Output Compare A
    TC4Abm = $01;
    // Inverted I/O Enable
    INVENbm = $40;
    // PORT_OPC
    OPCmask = $38;
    OPC_TOTEM = $00;
    OPC_BUSKEEPER = $08;
    OPC_PULLDOWN = $10;
    OPC_PULLUP = $18;
    OPC_WIREDOR = $20;
    OPC_WIREDAND = $28;
    OPC_WIREDORPULL = $30;
    OPC_WIREDANDPULL = $38;
    // PORT_ISC
    ISCmask = $07;
    ISC_BOTHEDGES = $00;
    ISC_RISING = $01;
    ISC_FALLING = $02;
    ISC_LEVEL = $03;
    ISC_FORCE_ENABLE = $06;
    ISC_INPUT_DISABLE = $07;
  end;

  TTC4 = object //16-bit Timer/Counter 4
    CTRLA: byte;  //Control  Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control register C
    CTRLD: byte;  //Control Register D
    CTRLE: byte;  //Control Register E
    CTRLF: byte;  //Control Register F
    INTCTRLA: byte;  //Interrupt Control Register A
    INTCTRLB: byte;  //Interrupt Control Register B
    CTRLGCLR: byte;  //Control Register G Clear
    CTRLGSET: byte;  //Control Register G Set
    CTRLHCLR: byte;  //Control Register H Clear
    CTRLHSET: byte;  //Control Register H Set
    INTFLAGS: byte;  //Interrupt Flag Register
    Reserved13: byte;
    Reserved14: byte;
    TEMP: byte;  //Temporary Register For 16-bit Access
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
    Reserved34: byte;
    Reserved35: byte;
    Reserved36: byte;
    Reserved37: byte;
    PER: word;  //Period
    CCA: word;  //Compare or Capture A
    CCB: word;  //Compare or Capture B
    CCC: word;  //Compare or Capture C
    CCD: word;  //Compare or Capture D
    Reserved48: byte;
    Reserved49: byte;
    Reserved50: byte;
    Reserved51: byte;
    Reserved52: byte;
    Reserved53: byte;
    PERBUF: word;  //Period Buffer
    CCABUF: word;  //Compare Or Capture A Buffer
    CCBBUF: word;  //Compare Or Capture B Buffer
    CCCBUF: word;  //Compare Or Capture C Buffer
    CCDBUF: word;  //Compare Or Capture D Buffer
  const
    // Synchronization Enabled
    SYNCHENbm = $40;
    // Start on Next Event
    EVSTARTbm = $20;
    // Stop on Next Update
    UPSTOPbm = $10;
    // TC45_CLKSEL
    CLKSELmask = $0F;
    CLKSEL_OFF = $00;
    CLKSEL_DIV1 = $01;
    CLKSEL_DIV2 = $02;
    CLKSEL_DIV4 = $03;
    CLKSEL_DIV8 = $04;
    CLKSEL_DIV64 = $05;
    CLKSEL_DIV256 = $06;
    CLKSEL_DIV1024 = $07;
    CLKSEL_EVCH0 = $08;
    CLKSEL_EVCH1 = $09;
    CLKSEL_EVCH2 = $0A;
    CLKSEL_EVCH3 = $0B;
    CLKSEL_EVCH4 = $0C;
    CLKSEL_EVCH5 = $0D;
    CLKSEL_EVCH6 = $0E;
    CLKSEL_EVCH7 = $0F;
    // TC45_BYTEM
    BYTEMmask = $C0;
    BYTEM_NORMAL = $00;
    BYTEM_BYTEMODE = $40;
    // TC45_CIRCEN
    CIRCENmask = $30;
    CIRCEN_DISABLE = $00;
    CIRCEN_PER = $10;
    CIRCEN_CCA = $20;
    CIRCEN_BOTH = $30;
    // TC45_WGMODE
    WGMODEmask = $07;
    WGMODE_NORMAL = $00;
    WGMODE_FRQ = $01;
    WGMODE_SINGLESLOPE = $03;
    WGMODE_DSTOP = $05;
    WGMODE_DSBOTH = $06;
    WGMODE_DSBOTTOM = $07;
    // Channel D Output Polarity
    POLDbm = $80;
    // Channel C Output Polarity
    POLCbm = $40;
    // Channel B Output Polarity
    POLBbm = $20;
    // Channel A Output Polarity
    POLAbm = $10;
    // Channel D Compare Output Value
    CMPDbm = $08;
    // Channel C Compare Output Value
    CMPCbm = $04;
    // Channel B Compare Output Value
    CMPBbm = $02;
    // Channel A Compare Output Value
    CMPAbm = $01;
    // High Channel D Compare Output Value
    HCMPDbm = $80;
    // High Channel C Compare Output Value
    HCMPCbm = $40;
    // High Channel B Compare Output Value
    HCMPBbm = $20;
    // High Channel A Compare Output Value
    HCMPAbm = $10;
    // Low Channel D Compare Output Value
    LCMPDbm = $08;
    // Low Channel C Compare Output Value
    LCMPCbm = $04;
    // Low Channel B Compare Output Value
    LCMPBbm = $02;
    // Low Channel A Compare Output Value
    LCMPAbm = $01;
    // TC45_EVACT
    EVACTmask = $E0;
    EVACT_OFF = $00;
    EVACT_FMODE1 = $20;
    EVACT_FMODE2 = $40;
    EVACT_UPDOWN = $60;
    EVACT_QDEC = $80;
    EVACT_RESTART = $A0;
    EVACT_PWF = $C0;
    // Event Delay
    EVDLYbm = $10;
    // TC45_EVSEL
    EVSELmask = $0F;
    EVSEL_OFF = $00;
    EVSEL_CH0 = $08;
    EVSEL_CH1 = $09;
    EVSEL_CH2 = $0A;
    EVSEL_CH3 = $0B;
    EVSEL_CH4 = $0C;
    EVSEL_CH5 = $0D;
    EVSEL_CH6 = $0E;
    EVSEL_CH7 = $0F;
    // TC45_CCDMODE
    CCDMODEmask = $C0;
    CCDMODE_DISABLE = $00;
    CCDMODE_COMP = $40;
    CCDMODE_CAPT = $80;
    CCDMODE_BOTHCC = $C0;
    // TC45_CCCMODE
    CCCMODEmask = $30;
    CCCMODE_DISABLE = $00;
    CCCMODE_COMP = $10;
    CCCMODE_CAPT = $20;
    CCCMODE_BOTHCC = $30;
    // TC45_CCBMODE
    CCBMODEmask = $0C;
    CCBMODE_DISABLE = $00;
    CCBMODE_COMP = $04;
    CCBMODE_CAPT = $08;
    CCBMODE_BOTHCC = $0C;
    // TC45_CCAMODE
    CCAMODEmask = $03;
    CCAMODE_DISABLE = $00;
    CCAMODE_COMP = $01;
    CCAMODE_CAPT = $02;
    CCAMODE_BOTHCC = $03;
    // TC45_LCCDMODE
    LCCDMODEmask = $C0;
    LCCDMODE_DISABLE = $00;
    LCCDMODE_COMP = $40;
    LCCDMODE_CAPT = $80;
    LCCDMODE_BOTHCC = $C0;
    // TC45_LCCCMODE
    LCCCMODEmask = $30;
    LCCCMODE_DISABLE = $00;
    LCCCMODE_COMP = $10;
    LCCCMODE_CAPT = $20;
    LCCCMODE_BOTHCC = $30;
    // TC45_LCCBMODE
    LCCBMODEmask = $0C;
    LCCBMODE_DISABLE = $00;
    LCCBMODE_COMP = $04;
    LCCBMODE_CAPT = $08;
    LCCBMODE_BOTHCC = $0C;
    // TC45_LCCAMODE
    LCCAMODEmask = $03;
    LCCAMODE_DISABLE = $00;
    LCCAMODE_COMP = $01;
    LCCAMODE_CAPT = $02;
    LCCAMODE_BOTHCC = $03;
    // TC45_HCCDMODE
    HCCDMODEmask = $C0;
    HCCDMODE_DISABLE = $00;
    HCCDMODE_COMP = $40;
    HCCDMODE_CAPT = $80;
    HCCDMODE_BOTHCC = $C0;
    // TC45_HCCCMODE
    HCCCMODEmask = $30;
    HCCCMODE_DISABLE = $00;
    HCCCMODE_COMP = $10;
    HCCCMODE_CAPT = $20;
    HCCCMODE_BOTHCC = $30;
    // TC45_HCCBMODE
    HCCBMODEmask = $0C;
    HCCBMODE_DISABLE = $00;
    HCCBMODE_COMP = $04;
    HCCBMODE_CAPT = $08;
    HCCBMODE_BOTHCC = $0C;
    // TC45_HCCAMODE
    HCCAMODEmask = $03;
    HCCAMODE_DISABLE = $00;
    HCCAMODE_COMP = $01;
    HCCAMODE_CAPT = $02;
    HCCAMODE_BOTHCC = $03;
    // TC45_TRGINTLVL
    TRGINTLVLmask = $30;
    TRGINTLVL_OFF = $00;
    TRGINTLVL_LO = $10;
    TRGINTLVL_MED = $20;
    TRGINTLVL_HI = $30;
    // TC45_ERRINTLVL
    ERRINTLVLmask = $0C;
    ERRINTLVL_OFF = $00;
    ERRINTLVL_LO = $04;
    ERRINTLVL_MED = $08;
    ERRINTLVL_HI = $0C;
    // TC45_OVFINTLVL
    OVFINTLVLmask = $03;
    OVFINTLVL_OFF = $00;
    OVFINTLVL_LO = $01;
    OVFINTLVL_MED = $02;
    OVFINTLVL_HI = $03;
    // TC45_CCDINTLVL
    CCDINTLVLmask = $C0;
    CCDINTLVL_OFF = $00;
    CCDINTLVL_LO = $40;
    CCDINTLVL_MED = $80;
    CCDINTLVL_HI = $C0;
    // TC45_CCCINTLVL
    CCCINTLVLmask = $30;
    CCCINTLVL_OFF = $00;
    CCCINTLVL_LO = $10;
    CCCINTLVL_MED = $20;
    CCCINTLVL_HI = $30;
    // TC45_CCBINTLVL
    CCBINTLVLmask = $0C;
    CCBINTLVL_OFF = $00;
    CCBINTLVL_LO = $04;
    CCBINTLVL_MED = $08;
    CCBINTLVL_HI = $0C;
    // TC45_CCAINTLVL
    CCAINTLVLmask = $03;
    CCAINTLVL_OFF = $00;
    CCAINTLVL_LO = $01;
    CCAINTLVL_MED = $02;
    CCAINTLVL_HI = $03;
    // TC45_LCCDINTLVL
    LCCDINTLVLmask = $C0;
    LCCDINTLVL_OFF = $00;
    LCCDINTLVL_LO = $40;
    LCCDINTLVL_MED = $80;
    LCCDINTLVL_HI = $C0;
    // TC45_LCCCINTLVL
    LCCCINTLVLmask = $30;
    LCCCINTLVL_OFF = $00;
    LCCCINTLVL_LO = $10;
    LCCCINTLVL_MED = $20;
    LCCCINTLVL_HI = $30;
    // TC45_LCCBINTLVL
    LCCBINTLVLmask = $0C;
    LCCBINTLVL_OFF = $00;
    LCCBINTLVL_LO = $04;
    LCCBINTLVL_MED = $08;
    LCCBINTLVL_HI = $0C;
    // TC45_LCCAINTLVL
    LCCAINTLVLmask = $03;
    LCCAINTLVL_OFF = $00;
    LCCAINTLVL_LO = $01;
    LCCAINTLVL_MED = $02;
    LCCAINTLVL_HI = $03;
    // Timer/Counter Stop
    STOPbm = $20;
    // TC45_CMD
    CMDmask = $0C;
    CMD_NONE = $00;
    CMD_UPDATE = $04;
    CMD_RESTART = $08;
    CMD_RESET = $0C;
    // Lock Update
    LUPDbm = $02;
    // Counter Direction
    DIRbm = $01;
    // Channel D Compare or Capture Buffer Valid
    CCDBVbm = $10;
    // Channel C Compare or Capture Buffer Valid
    CCCBVbm = $08;
    // Channel B Compare or Capture Buffer Valid
    CCBBVbm = $04;
    // Channel A Compare or Capture Buffer Valid
    CCABVbm = $02;
    // Period Buffer Valid
    PERBVbm = $01;
    // Channel Low D Compare or Capture Buffer Valid
    LCCDBVbm = $10;
    // Channel Low C Compare or Capture Buffer Valid
    LCCCBVbm = $08;
    // Channel Low B Compare or Capture Buffer Valid
    LCCBBVbm = $04;
    // Channel Low A Compare or Capture Buffer Valid
    LCCABVbm = $02;
    // Period Low Buffer Valid
    LPERBVbm = $01;
    // Channel D Compare or Capture Interrupt Flag
    CCDIFbm = $80;
    // Channel C Compare or Capture Interrupt Flag
    CCCIFbm = $40;
    // Channel B Compare or Capture Interrupt Flag
    CCBIFbm = $20;
    // Channel A Compare or Capture Interrupt Flag
    CCAIFbm = $10;
    // Trigger Restart Interrupt Flag
    TRGIFbm = $04;
    // Error Interrupt Flag
    ERRIFbm = $02;
    // Overflow/Underflow Interrupt Flag
    OVFIFbm = $01;
    // Channel Low D Compare or Capture Interrupt Flag
    LCCDIFbm = $80;
    // Channel Low C Compare or Capture Interrupt Flag
    LCCCIFbm = $40;
    // Channel Low B Compare or Capture Interrupt Flag
    LCCBIFbm = $20;
    // Channel Low A Compare or Capture Interrupt Flag
    LCCAIFbm = $10;
  end;

  TTC5 = object //16-bit Timer/Counter 5
    CTRLA: byte;  //Control  Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control register C
    CTRLD: byte;  //Control Register D
    CTRLE: byte;  //Control Register E
    CTRLF: byte;  //Control Register F
    INTCTRLA: byte;  //Interrupt Control Register A
    INTCTRLB: byte;  //Interrupt Control Register B
    CTRLGCLR: byte;  //Control Register G Clear
    CTRLGSET: byte;  //Control Register G Set
    CTRLHCLR: byte;  //Control Register H Clear
    CTRLHSET: byte;  //Control Register H Set
    INTFLAGS: byte;  //Interrupt Flag Register
    Reserved13: byte;
    Reserved14: byte;
    TEMP: byte;  //Temporary Register For 16-bit Access
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
    Reserved34: byte;
    Reserved35: byte;
    Reserved36: byte;
    Reserved37: byte;
    PER: word;  //Period
    CCA: word;  //Compare or Capture A
    CCB: word;  //Compare or Capture B
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
    PERBUF: word;  //Period Buffer
    CCABUF: word;  //Compare Or Capture A Buffer
    CCBBUF: word;  //Compare Or Capture B Buffer
  const
    // Synchronization Enabled
    SYNCHENbm = $40;
    // Start on Next Event
    EVSTARTbm = $20;
    // Stop on Next Update
    UPSTOPbm = $10;
    // TC45_CLKSEL
    CLKSELmask = $0F;
    CLKSEL_OFF = $00;
    CLKSEL_DIV1 = $01;
    CLKSEL_DIV2 = $02;
    CLKSEL_DIV4 = $03;
    CLKSEL_DIV8 = $04;
    CLKSEL_DIV64 = $05;
    CLKSEL_DIV256 = $06;
    CLKSEL_DIV1024 = $07;
    CLKSEL_EVCH0 = $08;
    CLKSEL_EVCH1 = $09;
    CLKSEL_EVCH2 = $0A;
    CLKSEL_EVCH3 = $0B;
    CLKSEL_EVCH4 = $0C;
    CLKSEL_EVCH5 = $0D;
    CLKSEL_EVCH6 = $0E;
    CLKSEL_EVCH7 = $0F;
    // TC45_BYTEM
    BYTEMmask = $C0;
    BYTEM_NORMAL = $00;
    BYTEM_BYTEMODE = $40;
    // TC45_CIRCEN
    CIRCENmask = $30;
    CIRCEN_DISABLE = $00;
    CIRCEN_PER = $10;
    CIRCEN_CCA = $20;
    CIRCEN_BOTH = $30;
    // TC45_WGMODE
    WGMODEmask = $07;
    WGMODE_NORMAL = $00;
    WGMODE_FRQ = $01;
    WGMODE_SINGLESLOPE = $03;
    WGMODE_DSTOP = $05;
    WGMODE_DSBOTH = $06;
    WGMODE_DSBOTTOM = $07;
    // Channel B Output Polarity
    POLBbm = $20;
    // Channel A Output Polarity
    POLAbm = $10;
    // Channel B Compare Output Value
    CMPBbm = $02;
    // Channel A Compare Output Value
    CMPAbm = $01;
    // High Channel B Compare Output Value
    HCMPBbm = $20;
    // High Channel A Compare Output Value
    HCMPAbm = $10;
    // Low Channel B Compare Output Value
    LCMPBbm = $02;
    // Low Channel A Compare Output Value
    LCMPAbm = $01;
    // TC45_EVACT
    EVACTmask = $E0;
    EVACT_OFF = $00;
    EVACT_FMODE1 = $20;
    EVACT_FMODE2 = $40;
    EVACT_UPDOWN = $60;
    EVACT_QDEC = $80;
    EVACT_RESTART = $A0;
    EVACT_PWF = $C0;
    // Event Delay
    EVDLYbm = $10;
    // TC45_EVSEL
    EVSELmask = $0F;
    EVSEL_OFF = $00;
    EVSEL_CH0 = $08;
    EVSEL_CH1 = $09;
    EVSEL_CH2 = $0A;
    EVSEL_CH3 = $0B;
    EVSEL_CH4 = $0C;
    EVSEL_CH5 = $0D;
    EVSEL_CH6 = $0E;
    EVSEL_CH7 = $0F;
    // TC45_CCBMODE
    CCBMODEmask = $0C;
    CCBMODE_DISABLE = $00;
    CCBMODE_COMP = $04;
    CCBMODE_CAPT = $08;
    CCBMODE_BOTHCC = $0C;
    // TC45_CCAMODE
    CCAMODEmask = $03;
    CCAMODE_DISABLE = $00;
    CCAMODE_COMP = $01;
    CCAMODE_CAPT = $02;
    CCAMODE_BOTHCC = $03;
    // TC45_LCCBMODE
    LCCBMODEmask = $0C;
    LCCBMODE_DISABLE = $00;
    LCCBMODE_COMP = $04;
    LCCBMODE_CAPT = $08;
    LCCBMODE_BOTHCC = $0C;
    // TC45_LCCAMODE
    LCCAMODEmask = $03;
    LCCAMODE_DISABLE = $00;
    LCCAMODE_COMP = $01;
    LCCAMODE_CAPT = $02;
    LCCAMODE_BOTHCC = $03;
    // TC45_HCCBMODE
    HCCBMODEmask = $0C;
    HCCBMODE_DISABLE = $00;
    HCCBMODE_COMP = $04;
    HCCBMODE_CAPT = $08;
    HCCBMODE_BOTHCC = $0C;
    // TC45_HCCAMODE
    HCCAMODEmask = $03;
    HCCAMODE_DISABLE = $00;
    HCCAMODE_COMP = $01;
    HCCAMODE_CAPT = $02;
    HCCAMODE_BOTHCC = $03;
    // TC45_TRGINTLVL
    TRGINTLVLmask = $30;
    TRGINTLVL_OFF = $00;
    TRGINTLVL_LO = $10;
    TRGINTLVL_MED = $20;
    TRGINTLVL_HI = $30;
    // TC45_ERRINTLVL
    ERRINTLVLmask = $0C;
    ERRINTLVL_OFF = $00;
    ERRINTLVL_LO = $04;
    ERRINTLVL_MED = $08;
    ERRINTLVL_HI = $0C;
    // TC45_OVFINTLVL
    OVFINTLVLmask = $03;
    OVFINTLVL_OFF = $00;
    OVFINTLVL_LO = $01;
    OVFINTLVL_MED = $02;
    OVFINTLVL_HI = $03;
    // TC45_CCBINTLVL
    CCBINTLVLmask = $0C;
    CCBINTLVL_OFF = $00;
    CCBINTLVL_LO = $04;
    CCBINTLVL_MED = $08;
    CCBINTLVL_HI = $0C;
    // TC45_CCAINTLVL
    CCAINTLVLmask = $03;
    CCAINTLVL_OFF = $00;
    CCAINTLVL_LO = $01;
    CCAINTLVL_MED = $02;
    CCAINTLVL_HI = $03;
    // TC45_LCCBINTLVL
    LCCBINTLVLmask = $0C;
    LCCBINTLVL_OFF = $00;
    LCCBINTLVL_LO = $04;
    LCCBINTLVL_MED = $08;
    LCCBINTLVL_HI = $0C;
    // TC45_LCCAINTLVL
    LCCAINTLVLmask = $03;
    LCCAINTLVL_OFF = $00;
    LCCAINTLVL_LO = $01;
    LCCAINTLVL_MED = $02;
    LCCAINTLVL_HI = $03;
    // Timer/Counter Stop
    STOPbm = $20;
    // TC45_CMD
    CMDmask = $0C;
    CMD_NONE = $00;
    CMD_UPDATE = $04;
    CMD_RESTART = $08;
    CMD_RESET = $0C;
    // Lock Update
    LUPDbm = $02;
    // Counter Direction
    DIRbm = $01;
    // Channel B Compare or Capture Buffer Valid
    CCBBVbm = $04;
    // Channel A Compare or Capture Buffer Valid
    CCABVbm = $02;
    // Period Buffer Valid
    PERBVbm = $01;
    // Channel Low B Compare or Capture Buffer Valid
    LCCBBVbm = $04;
    // Channel Low A Compare or Capture Buffer Valid
    LCCABVbm = $02;
    // Period Low Buffer Valid
    LPERBVbm = $01;
    // Channel B Compare or Capture Interrupt Flag
    CCBIFbm = $20;
    // Channel A Compare or Capture Interrupt Flag
    CCAIFbm = $10;
    // Trigger Restart Interrupt Flag
    TRGIFbm = $04;
    // Error Interrupt Flag
    ERRIFbm = $02;
    // Overflow/Underflow Interrupt Flag
    OVFIFbm = $01;
    // Channel Low B Compare or Capture Interrupt Flag
    LCCBIFbm = $20;
    // Channel Low A Compare or Capture Interrupt Flag
    LCCAIFbm = $10;
  end;

  TFAULT = object //Fault Extension
    CTRLA: byte;  //Control A Register
    CTRLB: byte;  //Control B Register
    CTRLC: byte;  //Control C Register
    CTRLD: byte;  //Control D Register
    CTRLE: byte;  //Control E Register
    STATUS: byte;  //Status Register
    CTRLGCLR: byte;  //Control Register G Clear
    CTRLGSET: byte;  //Control Register G set
  const
    // FAULT_RAMP
    RAMPmask = $C0;
    RAMP_RAMP1 = $00;
    RAMP_RAMP2 = $80;
    // Fault on Debug Break Detection
    FDDBDbm = $20;
    // Port Control Mode
    PORTCTRLbm = $10;
    // Fuse State 
    FUSEbm = $08;
    // Fault E Digital Filter Selection
    FILTEREbm = $04;
    // FAULT_SRCE
    SRCEmask = $03;
    SRCE_DISABLE = $00;
    SRCE_CHN = $01;
    SRCE_CHN1 = $02;
    SRCE_CHN2 = $03;
    // Fault A Software Mode
    SOFTAbm = $80;
    // FAULT_HALTA
    HALTAmask = $60;
    HALTA_DISABLE = $00;
    HALTA_HW = $20;
    HALTA_SW = $40;
    // Fault A Restart Action
    RESTARTAbm = $10;
    // Fault A Keep Action
    KEEPAbm = $08;
    // FAULT_SRCA
    SRCAmask = $03;
    SRCA_DISABLE = $00;
    SRCA_CHN = $01;
    SRCA_CHN1 = $02;
    SRCA_LINK = $03;
    // Fault A Capture
    CAPTAbm = $20;
    // Fault A Digital Filter Selection
    FILTERAbm = $04;
    // Fault A Blanking
    BLANKAbm = $02;
    // Fault A Qualification
    QUALAbm = $01;
    // Fault B Software Mode
    SOFTBbm = $80;
    // FAULT_HALTB
    HALTBmask = $60;
    HALTB_DISABLE = $00;
    HALTB_HW = $20;
    HALTB_SW = $40;
    // Fault B Restart Action
    RESTARTBbm = $10;
    // Fault B Keep Action
    KEEPBbm = $08;
    // FAULT_SRCB
    SRCBmask = $03;
    SRCB_DISABLE = $00;
    SRCB_CHN = $01;
    SRCB_CHN1 = $02;
    SRCB_LINK = $03;
    // Fault B Capture
    CAPTBbm = $20;
    // Fault B Digital Filter Selection
    FILTERBbm = $04;
    // Fault B Blanking
    BLANKBbm = $02;
    // Fault B Qualification
    QUALBbm = $01;
    // Fault B State
    STATEBbm = $80;
    // Fault A State
    STATEAbm = $40;
    // Fault E State
    STATEEbm = $20;
    // Channel Index Flag
    IDXbm = $08;
    // Fault B Flag
    FAULTBINbm = $04;
    // Fault A Flag
    FAULTAINbm = $02;
    // Fault E Flag
    FAULTEINbm = $01;
    // State B Clear
    HALTBCLRbm = $80;
    // State A Clear
    HALTACLRbm = $40;
    // State E Clear
    STATEECLRbm = $20;
    // Fault B Flag
    FAULTBbm = $04;
    // Fault A Flag
    FAULTAbm = $02;
    // Fault E Flag
    FAULTEbm = $01;
    // Software Fault B
    FAULTBSWbm = $80;
    // Software Fault A
    FAULTASWbm = $40;
    // Software Fault E
    FAULTESWbm = $20;
    // FAULT_IDXCMD
    IDXCMDmask = $18;
    IDXCMD_DISABLE = $00;
    IDXCMD_SET = $08;
    IDXCMD_CLEAR = $10;
    IDXCMD_HOLD = $18;
  end;

  TWEX = object //Waveform Extension
    CTRL: byte;  //Control Register
    DTBOTH: byte;  //Dead-time Concurrent Write to Both Sides Register
    DTLS: byte;  //Dead-time Low Side Register
    DTHS: byte;  //Dead-time High Side Register
    STATUSCLR: byte;  //Status Clear Register
    STATUSSET: byte;  //Status Set Register
    SWAP: byte;  //Swap Register
    PGO: byte;  //Pattern Generation Override Register
    PGV: byte;  //Pattern Generation Value Register
    Reserved9: byte;
    SWAPBUF: byte;  //Dead Time Low Side Buffer
    PGOBUF: byte;  //Pattern Generation Overwrite Buffer Register
    PGVBUF: byte;  //Pattern Generation Value Buffer Register
    Reserved13: byte;
    Reserved14: byte;
    OUTOVDIS: byte;  //Output Override Disable Register 
  const
    // Update Source Selection
    UPSELbm = $80;
    // WEX_OTMX
    OTMXmask = $70;
    OTMX_DEFAULT = $00;
    OTMX_FIRST = $10;
    OTMX_SECOND = $20;
    OTMX_THIRD = $30;
    OTMX_FOURTH = $40;
    // Dead-Time Insertion Generator 3 Enable
    DTI3ENbm = $08;
    // Dead-Time Insertion Generator 2 Enable
    DTI2ENbm = $04;
    // Dead-Time Insertion Generator 1 Enable
    DTI1ENbm = $02;
    // Dead-Time Insertion Generator 0 Enable
    DTI0ENbm = $01;
    // Swap Buffer Valid 
    SWAPBUFbm = $04;
    // Pattern Generator Value Buffer Valid 
    PGVBUFVbm = $02;
    // Pattern Generator Overwrite Buffer Valid
    PGOBUFVbm = $01;
    // Swap DTI output pair 3
    SWAP3bm = $08;
    // Swap DTI output pair 2
    SWAP2bm = $04;
    // Swap DTI output pair 1
    SWAP1bm = $02;
    // Swap DTI output pair 0
    SWAP0bm = $01;
    // Swap DTI output pair 3 
    SWAP3BUFbm = $08;
    // Swap DTI output pair 2
    SWAP2BUFbm = $04;
    // Swap DTI output pair 1 
    SWAP1BUFbm = $02;
    // Swap DTI output pair 0
    SWAP0BUFbm = $01;
  end;

  THIRES = object //High-Resolution Extension
    CTRLA: byte;  //Control Register A
  const
    // HIRES_HRPLUS
    HRPLUSmask = $0C;
    HRPLUS_NONE = $00;
    HRPLUS_HRP4 = $04;
    HRPLUS_HRP5 = $08;
    HRPLUS_BOTH = $0C;
    // HIRES_HREN
    HRENmask = $03;
    HREN_NONE = $00;
    HREN_HRP4 = $01;
    HREN_HRP5 = $02;
    HREN_BOTH = $03;
  end;

  TUSART = object //Universal Synchronous/Asynchronous Receiver/Transmitter
    DATA: byte;  //Data Register
    STATUS: byte;  //Status Register
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control Register C
    CTRLD: byte;  //Control Register D
    BAUDCTRLA: byte;  //Baud Rate Control Register A
    BAUDCTRLB: byte;  //Baud Rate Control Register B
  const
    // Receive Interrupt Flag
    RXCIFbm = $80;
    // Transmit Interrupt Flag
    TXCIFbm = $40;
    // Data Register Empty Flag
    DREIFbm = $20;
    // Frame Error
    FERRbm = $10;
    // Buffer Overflow
    BUFOVFbm = $08;
    // Parity Error
    PERRbm = $04;
    // Receive Start Bit Interrupt Flag
    RXSIFbm = $02;
    // Receive Bit 8
    RXB8bm = $01;
    // Data Reception Flag
    DRIFbm = $01;
    // Receive Start Interrupt Enable
    RXSIEbm = $80;
    // Data Reception Interrupt Enable
    DRIEbm = $40;
    // USART_RXCINTLVL
    RXCINTLVLmask = $30;
    RXCINTLVL_OFF = $00;
    RXCINTLVL_LO = $10;
    RXCINTLVL_MED = $20;
    RXCINTLVL_HI = $30;
    // USART_TXCINTLVL
    TXCINTLVLmask = $0C;
    TXCINTLVL_OFF = $00;
    TXCINTLVL_LO = $04;
    TXCINTLVL_MED = $08;
    TXCINTLVL_HI = $0C;
    // USART_DREINTLVL
    DREINTLVLmask = $03;
    DREINTLVL_OFF = $00;
    DREINTLVL_LO = $01;
    DREINTLVL_MED = $02;
    DREINTLVL_HI = $03;
    // One Wire Mode
    ONEWIREbm = $80;
    // Start Frame Detection Enable
    SFDENbm = $40;
    // Receiver Enable
    RXENbm = $10;
    // Transmitter Enable
    TXENbm = $08;
    // Double transmission speed
    CLK2Xbm = $04;
    // Multi-processor Communication Mode
    MPCMbm = $02;
    // Transmit bit 8
    TXB8bm = $01;
    // USART_CMODE
    CMODEmask = $C0;
    CMODE_ASYNCHRONOUS = $00;
    CMODE_SYNCHRONOUS = $40;
    CMODE_IRDA = $80;
    CMODE_MSPI = $C0;
    // USART_PMODE
    PMODEmask = $30;
    PMODE_DISABLED = $00;
    PMODE_EVEN = $20;
    PMODE_ODD = $30;
    // Stop Bit Mode
    SBMODEbm = $08;
    // USART_CHSIZE
    CHSIZEmask = $07;
    CHSIZE_5BIT = $00;
    CHSIZE_6BIT = $01;
    CHSIZE_7BIT = $02;
    CHSIZE_8BIT = $03;
    CHSIZE_9BIT = $07;
    // USART_DECTYPE
    DECTYPEmask = $30;
    DECTYPE_DATA = $00;
    DECTYPE_SDATA = $20;
    DECTYPE_NOTSDATA = $30;
    // USART_LUTACT
    LUTACTmask = $0C;
    LUTACT_OFF = $00;
    LUTACT_RX = $04;
    LUTACT_TX = $08;
    LUTACT_BOTH = $0C;
    // USART_PECACT
    PECACTmask = $03;
    PECACT_OFF = $00;
    PECACT_PEC0 = $01;
    PECACT_PEC1 = $02;
    PECACT_PERC01 = $03;
    // Baud Rate Scale
    BSCALE0bm = $10;
    BSCALE1bm = $20;
    BSCALE2bm = $40;
    BSCALE3bm = $80;
  end;

  TSPI = object //Serial Peripheral Interface with Buffer Modes
    CTRL: byte;  //Control Register
    INTCTRL: byte;  //Interrupt Control Register
    STATUS: byte;  //Status Register
    DATA: byte;  //Data Register
    CTRLB: byte;  //Control Register B
  const
    // Enable Double Speed
    CLK2Xbm = $80;
    // Enable SPI Module
    ENABLEbm = $40;
    // Data Order Setting
    DORDbm = $20;
    // Master Operation Enable
    MASTERbm = $10;
    // SPI_MODE
    MODEmask = $0C;
    MODE_0 = $00;
    MODE_1 = $04;
    MODE_2 = $08;
    MODE_3 = $0C;
    // SPI_PRESCALER
    PRESCALERmask = $03;
    PRESCALER_DIV4 = $00;
    PRESCALER_DIV16 = $01;
    PRESCALER_DIV64 = $02;
    PRESCALER_DIV128 = $03;
    // Receive Complete Interrupt Enable (In Buffer Modes Only).
    RXCIEbm = $80;
    // Transmit Complete Interrupt Enable (In Buffer Modes Only).
    TXCIEbm = $40;
    // Data Register Empty Interrupt Enable (In Buffer Modes Only).
    DREIEbm = $20;
    // Slave Select Trigger Interrupt Enable (In Buffer Modes Only).
    SSIEbm = $10;
    // SPI_INTLVL
    INTLVLmask = $03;
    INTLVL_OFF = $00;
    INTLVL_LO = $01;
    INTLVL_MED = $02;
    INTLVL_HI = $03;
    // Interrupt Flag (In Standard Mode Only).
    IFbm = $80;
    // Receive Complete Interrupt Flag (In Buffer Modes Only).
    RXCIFbm = $80;
    // Write Collision Flag (In Standard Mode Only).
    WRCOLbm = $40;
    // Transmit Complete Interrupt Flag (In Buffer Modes Only).
    TXCIFbm = $40;
    // Data Register Empty Interrupt Flag (In Buffer Modes Only).
    DREIFbm = $20;
    // Slave Select Trigger Interrupt Flag (In Buffer Modes Only).
    SSIFbm = $10;
    // Buffer Overflow (In Buffer Modes Only).
    BUFOVFbm = $01;
    // SPI_BUFMODE
    BUFMODEmask = $C0;
    BUFMODE_OFF = $00;
    BUFMODE_BUFMODE1 = $80;
    BUFMODE_BUFMODE2 = $C0;
    // Slave Select Disable
    SSDbm = $04;
  end;

  TIRCOM = object //IR Communication Module
    CTRL: byte;  //Control Register
    TXPLCTRL: byte;  //IrDA Transmitter Pulse Length Control Register
    RXPLCTRL: byte;  //IrDA Receiver Pulse Length Control Register
  const
    // IRDA_EVSEL
    EVSELmask = $0F;
    EVSEL_OFF = $00;
    EVSEL_0 = $08;
    EVSEL_1 = $09;
    EVSEL_2 = $0A;
    EVSEL_3 = $0B;
    EVSEL_4 = $0C;
    EVSEL_5 = $0D;
    EVSEL_6 = $0E;
    EVSEL_7 = $0F;
  end;

  TNVM_LOCKBITS = object //Lock Bits
    LOCKBITS: byte;  //Lock Bits
  const
    // FUSE_BLBB
    BLBBmask = $C0;
    BLBB_RWLOCK = $00;
    BLBB_RLOCK = $40;
    BLBB_WLOCK = $80;
    BLBB_NOLOCK = $C0;
    // FUSE_BLBA
    BLBAmask = $30;
    BLBA_RWLOCK = $00;
    BLBA_RLOCK = $10;
    BLBA_WLOCK = $20;
    BLBA_NOLOCK = $30;
    // FUSE_BLBAT
    BLBATmask = $0C;
    BLBAT_RWLOCK = $00;
    BLBAT_RLOCK = $04;
    BLBAT_WLOCK = $08;
    BLBAT_NOLOCK = $0C;
    // FUSE_LB
    LBmask = $03;
    LB_RWLOCK = $00;
    LB_WLOCK = $02;
    LB_NOLOCK = $03;
  end;

  TNVM_FUSES = object //Fuses
    Reserved0: byte;
    FUSEBYTE1: byte;  //Watchdog Configuration
    FUSEBYTE2: byte;  //Reset Configuration
    Reserved3: byte;
    FUSEBYTE4: byte;  //Start-up Configuration
    FUSEBYTE5: byte;  //EESAVE and BOD Level
    FUSEBYTE6: byte;  //Fault State
  const
    // WDWPER
    WDWPERmask = $F0;
    WDWPER_8CLK = $00;
    WDWPER_16CLK = $10;
    WDWPER_32CLK = $20;
    WDWPER_64CLK = $30;
    WDWPER_128CLK = $40;
    WDWPER_256CLK = $50;
    WDWPER_512CLK = $60;
    WDWPER_1KCLK = $70;
    WDWPER_2KCLK = $80;
    WDWPER_4KCLK = $90;
    WDWPER_8KCLK = $A0;
    // WDPER
    WDPERmask = $0F;
    WDPER_8CLK = $00;
    WDPER_16CLK = $01;
    WDPER_32CLK = $02;
    WDPER_64CLK = $03;
    WDPER_128CLK = $04;
    WDPER_256CLK = $05;
    WDPER_512CLK = $06;
    WDPER_1KCLK = $07;
    WDPER_2KCLK = $08;
    WDPER_4KCLK = $09;
    WDPER_8KCLK = $0A;
    // BOOTRST
    BOOTRSTmask = $40;
    BOOTRST_BOOTLDR = $00;
    BOOTRST_APPLICATION = $40;
    // BODPD
    BODPDmask = $03;
    BODPD_SAMPLED = $01;
    BODPD_CONTINUOUS = $02;
    BODPD_DISABLED = $03;
    // External Reset Disable
    RSTDISBLbm = $10;
    // STARTUPTIME
    STARTUPTIMEmask = $0C;
    STARTUPTIME0MS = $0C;
    STARTUPTIME4MS = $04;
    STARTUPTIME64MS = $00;
    // Watchdog Timer Lock
    WDLOCKbm = $02;
    // BODACT
    BODACTmask = $30;
    BODACT_SAMPLED = $10;
    BODACT_CONTINUOUS = $20;
    BODACT_DISABLED = $30;
    // Preserve EEPROM Through Chip Erase
    EESAVEbm = $08;
    // BODLEVEL
    BODLEVELmask = $07;
    BODLEVEL1V6 = $07;
    BODLEVEL1V8 = $06;
    BODLEVEL2V0 = $05;
    BODLEVEL2V2 = $04;
    BODLEVEL2V4 = $03;
    BODLEVEL2V6 = $02;
    BODLEVEL2V8 = $01;
    BODLEVEL3V0 = $00;
    // Fault Detection Action on TC5
    FDACT5bm = $80;
    // Fault Detection Action on TC4
    FDACT4bm = $40;
    // Port Pin Value
    VALUE0bm = $01;
    VALUE1bm = $02;
    VALUE2bm = $04;
    VALUE3bm = $08;
    VALUE4bm = $10;
    VALUE5bm = $20;
  end;

  TNVM_PROD_SIGNATURES = object //Production Signatures
    RCOSC8M: byte;  //RCOSC 8MHz Calibration Value
    Reserved1: byte;
    RCOSC32K: byte;  //RCOSC 32.768 kHz Calibration Value
    RCOSC32M: byte;  //RCOSC 32 MHz Calibration Value B
    RCOSC32MA: byte;  //RCOSC 32 MHz Calibration Value A
    Reserved5: byte;
    Reserved6: byte;
    Reserved7: byte;
    LOTNUM0: byte;  //Lot Number Byte 0, ASCII
    LOTNUM1: byte;  //Lot Number Byte 1, ASCII
    LOTNUM2: byte;  //Lot Number Byte 2, ASCII
    LOTNUM3: byte;  //Lot Number Byte 3, ASCII
    LOTNUM4: byte;  //Lot Number Byte 4, ASCII
    LOTNUM5: byte;  //Lot Number Byte 5, ASCII
    Reserved14: byte;
    Reserved15: byte;
    WAFNUM: byte;  //Wafer Number
    Reserved17: byte;
    COORDX0: byte;  //Wafer Coordinate X Byte 0
    COORDX1: byte;  //Wafer Coordinate X Byte 1
    COORDY0: byte;  //Wafer Coordinate Y Byte 0
    COORDY1: byte;  //Wafer Coordinate Y Byte 1
    Reserved22: byte;
    Reserved23: byte;
    Reserved24: byte;
    Reserved25: byte;
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    ROOMTEMP: byte;  //Temperature corresponds to TEMPSENSE3/2
    HOTTEMP: byte;  //Temperature corresponds to TEMPSENSE1/0
    ADCACAL0: byte;  //ADCA Calibration Byte 0
    ADCACAL1: byte;  //ADCA Calibration Byte 1
    Reserved34: byte;
    Reserved35: byte;
    Reserved36: byte;
    Reserved37: byte;
    Reserved38: byte;
    Reserved39: byte;
    ACACURRCAL: byte;  //ACA Current Calibration Byte
    Reserved41: byte;
    Reserved42: byte;
    Reserved43: byte;
    TEMPSENSE2: byte;  //Temperature Sensor Calibration Byte 2
    TEMPSENSE3: byte;  //Temperature Sensor Calibration Byte 3
    TEMPSENSE0: byte;  //Temperature Sensor Calibration Byte 0
    TEMPSENSE1: byte;  //Temperature Sensor Calibration Byte 1
    DACA0OFFCAL: byte;  //DACA0 Calibration Byte 0
    DACA0GAINCAL: byte;  //DACA0 Calibration Byte 1
    Reserved50: byte;
    Reserved51: byte;
    DACA1OFFCAL: byte;  //DACA1 Calibration Byte 0
    DACA1GAINCAL: byte;  //DACA1 Calibration Byte 1
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
  GPIO: TGPIO absolute $0000;
  VPORT0: TVPORT absolute $0010;
  VPORT1: TVPORT absolute $0014;
  VPORT2: TVPORT absolute $0018;
  VPORT3: TVPORT absolute $001C;
  OCD: TOCD absolute $002E;
  CPU: TCPU absolute $0030;
  CLK: TCLK absolute $0040;
  SLEEP: TSLEEP absolute $0048;
  OSC: TOSC absolute $0050;
  DFLLRC32M: TDFLL absolute $0060;
  PR: TPR absolute $0070;
  RST: TRST absolute $0078;
  WDT: TWDT absolute $0080;
  MCU: TMCU absolute $0090;
  PMIC: TPMIC absolute $00A0;
  PORTCFG: TPORTCFG absolute $00B0;
  CRC: TCRC absolute $00D0;
  EDMA: TEDMA absolute $0100;
  EVSYS: TEVSYS absolute $0180;
  NVM: TNVM absolute $01C0;
  ADCA: TADC absolute $0200;
  DACA: TDAC absolute $0300;
  ACA: TAC absolute $0380;
  RTC: TRTC absolute $0400;
  XCL: TXCL absolute $0460;
  TWIC: TTWI absolute $0480;
  PORTA: TPORT absolute $0600;
  PORTC: TPORT absolute $0640;
  PORTD: TPORT absolute $0660;
  PORTR: TPORT absolute $07E0;
  TCC4: TTC4 absolute $0800;
  TCC5: TTC5 absolute $0840;
  FAULTC4: TFAULT absolute $0880;
  FAULTC5: TFAULT absolute $0890;
  WEXC: TWEX absolute $08A0;
  HIRESC: THIRES absolute $08B0;
  USARTC0: TUSART absolute $08C0;
  SPIC: TSPI absolute $08E0;
  IRCOM: TIRCOM absolute $08F8;
  TCD5: TTC5 absolute $0940;
  USARTD0: TUSART absolute $09C0;

implementation

{$i avrcommon.inc}

procedure OSC_OSCF_ISR; external name 'OSC_OSCF_ISR'; // Interrupt 1 Oscillator Failure Interrupt (NMI)
procedure PORTR_INT_ISR; external name 'PORTR_INT_ISR'; // Interrupt 2 External Interrupt
procedure EDMA_CH0_ISR; external name 'EDMA_CH0_ISR'; // Interrupt 3 EDMA Channel 0 Interrupt
procedure EDMA_CH1_ISR; external name 'EDMA_CH1_ISR'; // Interrupt 4 EDMA Channel 1 Interrupt
procedure EDMA_CH2_ISR; external name 'EDMA_CH2_ISR'; // Interrupt 5 EDMA Channel 2 Interrupt
procedure EDMA_CH3_ISR; external name 'EDMA_CH3_ISR'; // Interrupt 6 EDMA Channel 3 Interrupt
procedure RTC_OVF_ISR; external name 'RTC_OVF_ISR'; // Interrupt 7 Overflow Interrupt
procedure RTC_COMP_ISR; external name 'RTC_COMP_ISR'; // Interrupt 8 Compare Interrupt
procedure PORTC_INT_ISR; external name 'PORTC_INT_ISR'; // Interrupt 9 External Interrupt
procedure TWIC_TWIS_ISR; external name 'TWIC_TWIS_ISR'; // Interrupt 10 TWI Slave Interrupt
procedure TWIC_TWIM_ISR; external name 'TWIC_TWIM_ISR'; // Interrupt 11 TWI Master Interrupt
procedure TCC4_OVF_ISR; external name 'TCC4_OVF_ISR'; // Interrupt 12 Overflow Interrupt
procedure TCC4_ERR_ISR; external name 'TCC4_ERR_ISR'; // Interrupt 13 Error Interrupt
procedure TCC4_CCA_ISR; external name 'TCC4_CCA_ISR'; // Interrupt 14 Channel A Compare or Capture Interrupt
procedure TCC4_CCB_ISR; external name 'TCC4_CCB_ISR'; // Interrupt 15 Channel B Compare or Capture Interrupt
procedure TCC4_CCC_ISR; external name 'TCC4_CCC_ISR'; // Interrupt 16 Channel C Compare or Capture Interrupt
procedure TCC4_CCD_ISR; external name 'TCC4_CCD_ISR'; // Interrupt 17 Channel D Compare or Capture Interrupt
procedure TCC5_OVF_ISR; external name 'TCC5_OVF_ISR'; // Interrupt 18 Overflow Interrupt
procedure TCC5_ERR_ISR; external name 'TCC5_ERR_ISR'; // Interrupt 19 Error Interrupt
procedure TCC5_CCA_ISR; external name 'TCC5_CCA_ISR'; // Interrupt 20 Channel A Compare or Capture Interrupt
procedure TCC5_CCB_ISR; external name 'TCC5_CCB_ISR'; // Interrupt 21 Channel B Compare or Capture Interrupt
procedure SPIC_INT_ISR; external name 'SPIC_INT_ISR'; // Interrupt 22 SPI Interrupt
procedure USARTC0_RXC_ISR; external name 'USARTC0_RXC_ISR'; // Interrupt 23 Reception Complete Interrupt
procedure USARTC0_DRE_ISR; external name 'USARTC0_DRE_ISR'; // Interrupt 24 Data Register Empty Interrupt
procedure USARTC0_TXC_ISR; external name 'USARTC0_TXC_ISR'; // Interrupt 25 Transmission Complete Interrupt
procedure NVM_EE_ISR; external name 'NVM_EE_ISR'; // Interrupt 26 EE Interrupt
procedure NVM_SPM_ISR; external name 'NVM_SPM_ISR'; // Interrupt 27 SPM Interrupt
procedure XCL_UNF_ISR; external name 'XCL_UNF_ISR'; // Interrupt 28 Timer/Counter Underflow Interrupt
procedure XCL_CC_ISR; external name 'XCL_CC_ISR'; // Interrupt 29 Timer/Counter Compare or Capture Interrupt
procedure PORTA_INT_ISR; external name 'PORTA_INT_ISR'; // Interrupt 30 External Interrupt
procedure ACA_AC0_ISR; external name 'ACA_AC0_ISR'; // Interrupt 31 AC0 Interrupt
procedure ACA_AC1_ISR; external name 'ACA_AC1_ISR'; // Interrupt 32 AC1 Interrupt
procedure ACA_ACW_ISR; external name 'ACA_ACW_ISR'; // Interrupt 33 ACW Window Mode Interrupt
procedure ADCA_CH0_ISR; external name 'ADCA_CH0_ISR'; // Interrupt 34 ADC Channel Interrupt
procedure PORTD_INT_ISR; external name 'PORTD_INT_ISR'; // Interrupt 35 External Interrupt
procedure TCD5_OVF_ISR; external name 'TCD5_OVF_ISR'; // Interrupt 36 Overflow Interrupt
procedure TCD5_ERR_ISR; external name 'TCD5_ERR_ISR'; // Interrupt 37 Error Interrupt
procedure TCD5_CCA_ISR; external name 'TCD5_CCA_ISR'; // Interrupt 38 Channel A Compare or Capture Interrupt
procedure TCD5_CCB_ISR; external name 'TCD5_CCB_ISR'; // Interrupt 39 Channel B Compare or Capture Interrupt
procedure USARTD0_RXC_ISR; external name 'USARTD0_RXC_ISR'; // Interrupt 40 Reception Complete Interrupt
procedure USARTD0_DRE_ISR; external name 'USARTD0_DRE_ISR'; // Interrupt 41 Data Register Empty Interrupt
procedure USARTD0_TXC_ISR; external name 'USARTD0_TXC_ISR'; // Interrupt 42 Transmission Complete Interrupt

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
asm
  jmp __dtors_end
  jmp OSC_OSCF_ISR
  jmp PORTR_INT_ISR
  jmp EDMA_CH0_ISR
  jmp EDMA_CH1_ISR
  jmp EDMA_CH2_ISR
  jmp EDMA_CH3_ISR
  jmp RTC_OVF_ISR
  jmp RTC_COMP_ISR
  jmp PORTC_INT_ISR
  jmp TWIC_TWIS_ISR
  jmp TWIC_TWIM_ISR
  jmp TCC4_OVF_ISR
  jmp TCC4_ERR_ISR
  jmp TCC4_CCA_ISR
  jmp TCC4_CCB_ISR
  jmp TCC4_CCC_ISR
  jmp TCC4_CCD_ISR
  jmp TCC5_OVF_ISR
  jmp TCC5_ERR_ISR
  jmp TCC5_CCA_ISR
  jmp TCC5_CCB_ISR
  jmp SPIC_INT_ISR
  jmp USARTC0_RXC_ISR
  jmp USARTC0_DRE_ISR
  jmp USARTC0_TXC_ISR
  jmp NVM_EE_ISR
  jmp NVM_SPM_ISR
  jmp XCL_UNF_ISR
  jmp XCL_CC_ISR
  jmp PORTA_INT_ISR
  jmp ACA_AC0_ISR
  jmp ACA_AC1_ISR
  jmp ACA_ACW_ISR
  jmp ADCA_CH0_ISR
  jmp PORTD_INT_ISR
  jmp TCD5_OVF_ISR
  jmp TCD5_ERR_ISR
  jmp TCD5_CCA_ISR
  jmp TCD5_CCB_ISR
  jmp USARTD0_RXC_ISR
  jmp USARTD0_DRE_ISR
  jmp USARTD0_TXC_ISR

  .weak OSC_OSCF_ISR
  .weak PORTR_INT_ISR
  .weak EDMA_CH0_ISR
  .weak EDMA_CH1_ISR
  .weak EDMA_CH2_ISR
  .weak EDMA_CH3_ISR
  .weak RTC_OVF_ISR
  .weak RTC_COMP_ISR
  .weak PORTC_INT_ISR
  .weak TWIC_TWIS_ISR
  .weak TWIC_TWIM_ISR
  .weak TCC4_OVF_ISR
  .weak TCC4_ERR_ISR
  .weak TCC4_CCA_ISR
  .weak TCC4_CCB_ISR
  .weak TCC4_CCC_ISR
  .weak TCC4_CCD_ISR
  .weak TCC5_OVF_ISR
  .weak TCC5_ERR_ISR
  .weak TCC5_CCA_ISR
  .weak TCC5_CCB_ISR
  .weak SPIC_INT_ISR
  .weak USARTC0_RXC_ISR
  .weak USARTC0_DRE_ISR
  .weak USARTC0_TXC_ISR
  .weak NVM_EE_ISR
  .weak NVM_SPM_ISR
  .weak XCL_UNF_ISR
  .weak XCL_CC_ISR
  .weak PORTA_INT_ISR
  .weak ACA_AC0_ISR
  .weak ACA_AC1_ISR
  .weak ACA_ACW_ISR
  .weak ADCA_CH0_ISR
  .weak PORTD_INT_ISR
  .weak TCD5_OVF_ISR
  .weak TCD5_ERR_ISR
  .weak TCD5_CCA_ISR
  .weak TCD5_CCB_ISR
  .weak USARTD0_RXC_ISR
  .weak USARTD0_DRE_ISR
  .weak USARTD0_TXC_ISR

  .set OSC_OSCF_ISR, Default_IRQ_handler
  .set PORTR_INT_ISR, Default_IRQ_handler
  .set EDMA_CH0_ISR, Default_IRQ_handler
  .set EDMA_CH1_ISR, Default_IRQ_handler
  .set EDMA_CH2_ISR, Default_IRQ_handler
  .set EDMA_CH3_ISR, Default_IRQ_handler
  .set RTC_OVF_ISR, Default_IRQ_handler
  .set RTC_COMP_ISR, Default_IRQ_handler
  .set PORTC_INT_ISR, Default_IRQ_handler
  .set TWIC_TWIS_ISR, Default_IRQ_handler
  .set TWIC_TWIM_ISR, Default_IRQ_handler
  .set TCC4_OVF_ISR, Default_IRQ_handler
  .set TCC4_ERR_ISR, Default_IRQ_handler
  .set TCC4_CCA_ISR, Default_IRQ_handler
  .set TCC4_CCB_ISR, Default_IRQ_handler
  .set TCC4_CCC_ISR, Default_IRQ_handler
  .set TCC4_CCD_ISR, Default_IRQ_handler
  .set TCC5_OVF_ISR, Default_IRQ_handler
  .set TCC5_ERR_ISR, Default_IRQ_handler
  .set TCC5_CCA_ISR, Default_IRQ_handler
  .set TCC5_CCB_ISR, Default_IRQ_handler
  .set SPIC_INT_ISR, Default_IRQ_handler
  .set USARTC0_RXC_ISR, Default_IRQ_handler
  .set USARTC0_DRE_ISR, Default_IRQ_handler
  .set USARTC0_TXC_ISR, Default_IRQ_handler
  .set NVM_EE_ISR, Default_IRQ_handler
  .set NVM_SPM_ISR, Default_IRQ_handler
  .set XCL_UNF_ISR, Default_IRQ_handler
  .set XCL_CC_ISR, Default_IRQ_handler
  .set PORTA_INT_ISR, Default_IRQ_handler
  .set ACA_AC0_ISR, Default_IRQ_handler
  .set ACA_AC1_ISR, Default_IRQ_handler
  .set ACA_ACW_ISR, Default_IRQ_handler
  .set ADCA_CH0_ISR, Default_IRQ_handler
  .set PORTD_INT_ISR, Default_IRQ_handler
  .set TCD5_OVF_ISR, Default_IRQ_handler
  .set TCD5_ERR_ISR, Default_IRQ_handler
  .set TCD5_CCA_ISR, Default_IRQ_handler
  .set TCD5_CCB_ISR, Default_IRQ_handler
  .set USARTD0_RXC_ISR, Default_IRQ_handler
  .set USARTD0_DRE_ISR, Default_IRQ_handler
  .set USARTD0_TXC_ISR, Default_IRQ_handler
end;

end.

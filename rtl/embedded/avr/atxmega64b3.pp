unit ATxmega64B3;

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
    // Port Interrupt 1 Flag
    INT1IFbm = $02;
    // Port Interrupt 0 Flag
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
    USBCTRL: byte;  //USB Control Register
  const
    // CLK_SCLKSEL
    SCLKSELmask = $07;
    SCLKSEL_RC2M = $00;
    SCLKSEL_RC32M = $01;
    SCLKSEL_RC32K = $02;
    SCLKSEL_XOSC = $03;
    SCLKSEL_PLL = $04;
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
    // CLK_USBPSDIV
    USBPSDIVmask = $38;
    USBPSDIV_1 = $00;
    USBPSDIV_2 = $08;
    USBPSDIV_4 = $10;
    USBPSDIV_8 = $18;
    USBPSDIV_16 = $20;
    USBPSDIV_32 = $28;
    // CLK_USBSRC
    USBSRCmask = $06;
    USBSRC_PLL = $00;
    USBSRC_RC32M = $02;
    // Clock Source Enable
    USBSENbm = $01;
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
  const
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
    XOSCSEL_EXTCLK_C0 = $01;
    XOSCSEL_EXTCLK_C1 = $05;
    XOSCSEL_EXTCLK_C2 = $09;
    XOSCSEL_EXTCLK_C3 = $0D;
    XOSCSEL_EXTCLK_C4 = $11;
    XOSCSEL_EXTCLK_C5 = $15;
    XOSCSEL_EXTCLK_C6 = $19;
    XOSCSEL_EXTCLK_C7 = $1D;
    XOSCSEL_32KHz = $02;
    XOSCSEL_XTAL_256CLK = $03;
    XOSCSEL_XTAL_1KCLK = $07;
    XOSCSEL_XTAL_16KCLK = $0B;
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
    RC32MCREF_USBSOF = $04;
    // OSC_RC2MCREF
    RC2MCREFmask = $01;
    RC2MCREF_RC32K = $00;
    RC2MCREF_XOSC32K = $01;
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

  TPR = object //Power Reduction
    PRGEN: byte;  //General Power Reduction
    PRPA: byte;  //Power Reduction Port A
    PRPB: byte;  //Power Reduction Port B
    PRPC: byte;  //Power Reduction Port C
    Reserved4: byte;
    PRPE: byte;  //Power Reduction Port E
  const
    // LCD Module
    LCDbm = $80;
    // USB
    USBbm = $40;
    // AES
    AESbm = $10;
    // Real-time Counter
    RTCbm = $04;
    // Event System
    EVSYSbm = $02;
    // DMA-Controller
    DMAbm = $01;
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
    // Port C AWEX
    HIRESbm = $04;
    // Port C Timer/Counter1
    TC1bm = $02;
    // Port C Timer/Counter0
    TC0bm = $01;
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
    JTAGUID: byte;  //JTAG User ID
    Reserved5: byte;
    MCUCR: byte;  //MCU Control
    ANAINIT: byte;  //Analog Startup Delay
    EVSYSLOCK: byte;  //Event System Lock
    AWEXLOCK: byte;  //AWEX Lock
  const
    // JTAG Disable
    JTAGDbm = $01;
    // Analog startup delay Port B
    STARTUPDLYB0bm = $04;
    STARTUPDLYB1bm = $08;
    // Analog startup delay Port A
    STARTUPDLYA0bm = $01;
    STARTUPDLYA1bm = $02;
    // Event Channel 4-7 Lock
    EVSYS1LOCKbm = $10;
    // Event Channel 0-3 Lock
    EVSYS0LOCKbm = $01;
    // AWeX on T/C C0 Lock
    AWEXCLOCKbm = $01;
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
    VPCTRLA: byte;  //Virtual Port Control Register A
    VPCTRLB: byte;  //Virtual Port Control Register B
    CLKEVOUT: byte;  //Clock and Event Out Register
    Reserved5: byte;
    EVOUTSEL: byte;  //Event Output Select
  const
    // VP1MAP
    VP1MAPmask = $F0;
    VP1MAPPORTA = $00;
    VP1MAPPORTB = $10;
    VP1MAPPORTC = $20;
    VP1MAPPORTD = $30;
    VP1MAPPORTE = $40;
    VP1MAPPORTG = $60;
    VP1MAPPORTM = $B0;
    VP1MAPPORTR = $F0;
    // VP0MAP
    VP0MAPmask = $0F;
    VP0MAPPORTA = $00;
    VP0MAPPORTB = $01;
    VP0MAPPORTC = $02;
    VP0MAPPORTD = $03;
    VP0MAPPORTE = $04;
    VP0MAPPORTG = $06;
    VP0MAPPORTM = $0B;
    VP0MAPPORTR = $0F;
    // VP3MAP
    VP3MAPmask = $F0;
    VP3MAPPORTA = $00;
    VP3MAPPORTB = $10;
    VP3MAPPORTC = $20;
    VP3MAPPORTD = $30;
    VP3MAPPORTE = $40;
    VP3MAPPORTG = $60;
    VP3MAPPORTM = $B0;
    VP3MAPPORTR = $F0;
    // VP2MAP
    VP2MAPmask = $0F;
    VP2MAPPORTA = $00;
    VP2MAPPORTB = $01;
    VP2MAPPORTC = $02;
    VP2MAPPORTD = $03;
    VP2MAPPORTE = $04;
    VP2MAPPORTG = $06;
    VP2MAPPORTM = $0B;
    VP2MAPPORTR = $0F;
    // PORTCFG_CLKOUT
    CLKOUTmask = $03;
    CLKOUT_OFF = $00;
    CLKOUT_PC = $01;
    CLKOUT_PE = $03;
    // PORTCFG_CLKOUTSEL
    CLKOUTSELmask = $0C;
    CLKOUTSEL_CLK1X = $00;
    CLKOUTSEL_CLK2X = $04;
    CLKOUTSEL_CLK4X = $08;
    // PORTCFG_EVOUT
    EVOUTmask = $30;
    EVOUT_OFF = $00;
    EVOUT_PC = $10;
    EVOUT_PE = $30;
    // RTC Clock Output
    RTCOUTbm = $40;
    // PORTCFG_CLKEVPIN
    CLKEVPINmask = $80;
    CLKEVPIN_PIN7 = $00;
    CLKEVPIN_PIN4 = $80;
// **** Inconsistency between mask and values for EVOUTSEL
    // PORTCFG_EVOUTSEL
    EVOUTSELmask = $04;
    EVOUTSEL_0 = $00;
    EVOUTSEL_1 = $04;
    EVOUTSEL_2 = $08;
    EVOUTSEL_3 = $0C;
  end;

  TAES = object //AES Module
    CTRL: byte;  //AES Control Register
    STATUS: byte;  //AES Status Register
    STATE: byte;  //AES State Register
    KEY: byte;  //AES Key Register
    INTCTRL: byte;  //AES Interrupt Control Register
  const
    // Start/Run
    STARTbm = $80;
    // Auto Start Trigger
    AUTObm = $40;
    // AES Software Reset
    RESETbm = $20;
    // Decryption / Direction
    DECRYPTbm = $10;
    // State XOR Load Enable
    XORbm = $04;
    // AES Error
    ERRORbm = $80;
    // State Ready Interrupt Flag
    SRIFbm = $01;
    // AES_INTLVL
    INTLVLmask = $03;
    INTLVL_OFF = $00;
    INTLVL_LO = $01;
    INTLVL_MED = $02;
    INTLVL_HI = $03;
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
    // Zero detection
    ZERObm = $02;
    // Busy
    BUSYbm = $01;
  end;

  TDMA_CH = object //DMA Channel
    CTRLA: byte;  //Channel Control
    CTRLB: byte;  //Channel Control
    ADDRCTRL: byte;  //Address Control
    TRIGSRC: byte;  //Channel Trigger Source
    TRFCNT: word;  //Channel Block Transfer Count
    REPCNT: byte;  //Channel Repeat Count
    Reserved7: byte;
    SRCADDR0: byte;  //Channel Source Address 0
    SRCADDR1: byte;  //Channel Source Address 1
    Reserved10: byte;
    Reserved11: byte;
    DESTADDR0: byte;  //Channel Destination Address 0
    DESTADDR1: byte;  //Channel Destination Address 1
  const
    // Channel Enable
    CHENbm = $80;
    // Channel Software Reset
    CHRSTbm = $40;
    // Channel Repeat Mode
    REPEATbm = $20;
    // Channel Transfer Request
    TRFREQbm = $10;
    // Channel Single Shot Data Transfer
    SINGLEbm = $04;
    // BURSTLEN
    BURSTLENmask = $03;
    BURSTLEN1BYTE = $00;
    BURSTLEN2BYTE = $01;
    BURSTLEN4BYTE = $02;
    BURSTLEN8BYTE = $03;
    // Block Transfer Busy
    CHBUSYbm = $80;
    // Block Transfer Pending
    CHPENDbm = $40;
    // Block Transfer Error Interrupt Flag
    ERRIFbm = $20;
    // Transaction Complete Interrupt Flag
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
    // SRCRELOAD
    SRCRELOADmask = $C0;
    SRCRELOADNONE = $00;
    SRCRELOADBLOCK = $40;
    SRCRELOADBURST = $80;
    SRCRELOADTRANSACTION = $C0;
    // SRCDIR
    SRCDIRmask = $30;
    SRCDIRFIXED = $00;
    SRCDIRINC = $10;
    SRCDIRDEC = $20;
    // DESTRELOAD
    DESTRELOADmask = $0C;
    DESTRELOADNONE = $00;
    DESTRELOADBLOCK = $04;
    DESTRELOADBURST = $08;
    DESTRELOADTRANSACTION = $0C;
    // DESTDIR
    DESTDIRmask = $03;
    DESTDIRFIXED = $00;
    DESTDIRINC = $01;
    DESTDIRDEC = $02;
    // TRIGSRC
    TRIGSRCmask = $FF;
    TRIGSRCOFF = $00;
    TRIGSRCEVSYS_CH0 = $01;
    TRIGSRCEVSYS_CH1 = $02;
    TRIGSRCEVSYS_CH2 = $03;
    TRIGSRCAES = $04;
    TRIGSRCADCA_CH0 = $10;
    TRIGSRCADCB_CH0 = $20;
    TRIGSRCTCC0_OVF = $40;
    TRIGSRCTCC0_ERR = $41;
    TRIGSRCTCC0_CCA = $42;
    TRIGSRCTCC0_CCB = $43;
    TRIGSRCTCC0_CCC = $44;
    TRIGSRCTCC0_CCD = $45;
    TRIGSRCTCC1_OVF = $46;
    TRIGSRCTCC1_ERR = $47;
    TRIGSRCTCC1_CCA = $48;
    TRIGSRCTCC1_CCB = $49;
    TRIGSRCSPIC = $4A;
    TRIGSRCUSARTC0_RXC = $4B;
    TRIGSRCUSARTC0_DRE = $4C;
    TRIGSRCTCE0_OVF = $80;
    TRIGSRCTCE0_ERR = $81;
    TRIGSRCTCE0_CCA = $82;
    TRIGSRCTCE0_CCB = $83;
    TRIGSRCTCE0_CCC = $84;
    TRIGSRCTCE0_CCD = $85;
    TRIGSRCUSARTE0_RXC = $8B;
    TRIGSRCUSARTE0_DRE = $8C;
  end;

  TDMA = object //DMA Controller
    CTRL: byte;  //Control
    Reserved1: byte;
    Reserved2: byte;
    INTFLAGS: byte;  //Transfer Interrupt Status
    STATUS: byte;  //Status
    Reserved5: byte;
    TEMP: word;  //Temporary Register For 16-bit Access
    Reserved8: byte;
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    CH0: TDMA_CH;  //DMA Channel 0
    CH1: TDMA_CH;  //DMA Channel 1
  const
    // Enable
    ENABLEbm = $80;
    // Software Reset
    RESETbm = $40;
    // DMA_DBUFMODE
    DBUFMODEmask = $04;
    DBUFMODE_DISABLED = $00;
    DBUFMODE_CH01 = $04;
    // DMA_PRIMODE
    PRIMODEmask = $01;
    PRIMODE_RR01 = $00;
    PRIMODE_CH0RR1 = $01;
    // Channel 1 Block Transfer Error Interrupt Flag
    CH1ERRIFbm = $20;
    // Channel 0 Block Transfer Error Interrupt Flag
    CH0ERRIFbm = $10;
    // Channel 1 Transaction Complete Interrupt Flag
    CH1TRNIFbm = $02;
    // Channel 0 Transaction Complete Interrupt Flag
    CH0TRNIFbm = $01;
    // Channel 1 Block Transfer Busy
    CH1BUSYbm = $20;
    // Channel 0 Block Transfer Busy
    CH0BUSYbm = $10;
    // Channel 1 Block Transfer Pending
    CH1PENDbm = $02;
    // Channel 0 Block Transfer Pending
    CH0PENDbm = $01;
  end;

  TEVSYS = object //Event System
    CH0MUX: byte;  //Event Channel 0 Multiplexer
    CH1MUX: byte;  //Event Channel 1 Multiplexer
    CH2MUX: byte;  //Event Channel 2 Multiplexer
    CH3MUX: byte;  //Event Channel 3 Multiplexer
    Reserved4: byte;
    Reserved5: byte;
    Reserved6: byte;
    Reserved7: byte;
    CH0CTRL: byte;  //Channel 0 Control Register
    CH1CTRL: byte;  //Channel 1 Control Register
    CH2CTRL: byte;  //Channel 2 Control Register
    CH3CTRL: byte;  //Channel 3 Control Register
    Reserved12: byte;
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    STROBE: byte;  //Event Strobe
    DATA: byte;  //Event Data
  const
    // EVSYS_CHMUX
    CHMUXmask = $FF;
    CHMUX_OFF = $00;
    CHMUX_RTC_OVF = $08;
    CHMUX_RTC_CMP = $09;
    CHMUX_USB = $0A;
    CHMUX_ACA_CH0 = $10;
    CHMUX_ACA_CH1 = $11;
    CHMUX_ACA_WIN = $12;
    CHMUX_ACB_CH0 = $13;
    CHMUX_ACB_CH1 = $14;
    CHMUX_ACB_WIN = $15;
    CHMUX_ADCA_CH0 = $20;
    CHMUX_ADCB_CH0 = $24;
    CHMUX_PORTA_PIN0 = $50;
    CHMUX_PORTA_PIN1 = $51;
    CHMUX_PORTA_PIN2 = $52;
    CHMUX_PORTA_PIN3 = $53;
    CHMUX_PORTA_PIN4 = $54;
    CHMUX_PORTA_PIN5 = $55;
    CHMUX_PORTA_PIN6 = $56;
    CHMUX_PORTA_PIN7 = $57;
    CHMUX_PORTB_PIN0 = $58;
    CHMUX_PORTB_PIN1 = $59;
    CHMUX_PORTB_PIN2 = $5A;
    CHMUX_PORTB_PIN3 = $5B;
    CHMUX_PORTB_PIN4 = $5C;
    CHMUX_PORTB_PIN5 = $5D;
    CHMUX_PORTB_PIN6 = $5E;
    CHMUX_PORTB_PIN7 = $5F;
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
    CHMUX_PORTE_PIN0 = $70;
    CHMUX_PORTE_PIN1 = $71;
    CHMUX_PORTE_PIN2 = $72;
    CHMUX_PORTE_PIN3 = $73;
    CHMUX_PORTE_PIN4 = $74;
    CHMUX_PORTE_PIN5 = $75;
    CHMUX_PORTE_PIN6 = $76;
    CHMUX_PORTE_PIN7 = $77;
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
    CHMUX_TCC0_OVF = $C0;
    CHMUX_TCC0_ERR = $C1;
    CHMUX_TCC0_CCA = $C4;
    CHMUX_TCC0_CCB = $C5;
    CHMUX_TCC0_CCC = $C6;
    CHMUX_TCC0_CCD = $C7;
    CHMUX_TCC1_OVF = $C8;
    CHMUX_TCC1_ERR = $C9;
    CHMUX_TCC1_CCA = $CC;
    CHMUX_TCC1_CCB = $CD;
    CHMUX_TCE0_OVF = $E0;
    CHMUX_TCE0_ERR = $E1;
    CHMUX_TCE0_CCA = $E4;
    CHMUX_TCE0_CCB = $E5;
    CHMUX_TCE0_CCC = $E6;
    CHMUX_TCE0_CCD = $E7;
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
    CMD_READ_EEPROM = $06;
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
    CMD_LOAD_EEPROM_BUFFER = $33;
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
    // EEPROM Mapping Enable
    EEMAPENbm = $08;
    // Flash Power Reduction Enable
    FPRMbm = $04;
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
    INPUTMODEDIFF = $02;
    INPUTMODEDIFFWGAIN = $03;
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
    // MUXNEGL
    MUXNEGLmask = $07;
    MUXNEGLPIN0 = $00;
    MUXNEGLPIN1 = $01;
    MUXNEGLPIN2 = $02;
    MUXNEGLPIN3 = $03;
    MUXNEGLGND = $05;
    MUXNEGLINTGND = $07;
    // MUXNEGH
    MUXNEGHmask = $07;
    MUXNEGHPIN4 = $00;
    MUXNEGHPIN5 = $01;
    MUXNEGHPIN6 = $02;
    MUXNEGHPIN7 = $03;
    MUXNEGHINTGND = $04;
    MUXNEGHGND = $07;
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
    CHIFbm = $01;
    // Positive MUX setting offset
    OFFSET0bm = $10;
    OFFSET1bm = $20;
    OFFSET2bm = $40;
    OFFSET3bm = $80;
    // Number of Channels included in scan
    COUNT0bm = $01;
    COUNT1bm = $02;
    COUNT2bm = $04;
    COUNT3bm = $08;
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
    CAL: word;  //Calibration Value
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
    // Channel 0 Start Conversion
    CH0STARTbm = $04;
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
    RESOLUTION_8BIT = $04;
    RESOLUTION_LEFT12BIT = $06;
    // ADC_REFSEL
    REFSELmask = $70;
    REFSEL_INT1V = $00;
    REFSEL_INTVCC = $10;
    REFSEL_AREFA = $20;
    REFSEL_AREFB = $30;
    REFSEL_INTVCC2 = $40;
    // Bandgap enable
    BANDGAPbm = $02;
    // Temperature Reference Enable
    TEMPREFbm = $01;
    // ADC_EVSEL
    EVSELmask = $18;
    EVSEL_0 = $00;
    EVSEL_1 = $08;
    EVSEL_2 = $10;
    EVSEL_3 = $18;
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
    // AC_MUXNEG
    MUXNEGmask = $07;
    MUXNEG_PIN0 = $00;
    MUXNEG_PIN1 = $01;
    MUXNEG_PIN3 = $02;
    MUXNEG_PIN5 = $03;
    MUXNEG_PIN7 = $04;
    MUXNEG_BANDGAP = $06;
    MUXNEG_SCALER = $07;
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
    CURRENbm = $80;
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
    Reserved6: byte;
    Reserved7: byte;
    CNT: word;  //Count Register
    PER: word;  //Period Register
    COMP: word;  //Compare Register
  const
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
  end;

  TTWI_MASTER = object //
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control Register C
    STATUS: byte;  //Status Register
    BAUD: byte;  //Baurd Rate Control Register
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

  TTWI = object //Two-Wire Interface
    CTRL: byte;  //TWI Common Control Register
    MASTER: TTWI_MASTER;  //TWI master module
    SLAVE: TTWI_SLAVE;  //TWI slave module
  const
    // TWI_SDAHOLD
    SDAHOLDmask = $06;
    SDAHOLD_OFF = $00;
    SDAHOLD_50NS = $02;
    SDAHOLD_300NS = $04;
    SDAHOLD_400NS = $06;
    // External Driver Interface Enable
    EDIENbm = $01;
  end;

  TUSB_EP = object //USB Endpoint
    STATUS: byte;  //Endpoint Status
    CTRL: byte;  //Endpoint Control
    CNT: word;  //USB Endpoint Counter
    DATAPTR: word;  //Data Pointer
    AUXDATA: word;  //Auxiliary Data
  const
    // Endpoint Stall Flag
    STALLFbm = $80;
    // CRC Error Flag
    CRCbm = $80;
    // Underflow Enpoint FLag
    UNFbm = $40;
    // Overflow Enpoint Flag for Output Endpoints
    OVFbm = $40;
    // Transaction Complete 0 Flag
    TRNCOMPL0bm = $20;
    // Transaction Complete 1 Flag
    TRNCOMPL1bm = $10;
    // SETUP Transaction Complete Flag
    SETUPbm = $10;
    // Bank Select
    BANKbm = $08;
    // Data Buffer 1 Not Acknowledge
    BUSNACK1bm = $04;
    // Data Buffer 0 Not Acknowledge
    BUSNACK0bm = $02;
    // Data Toggle
    TOGGLEbm = $01;
    // TYPE
    TYPEmask = $C0;
    TYPEDISABLE = $00;
    TYPECONTROL = $40;
    TYPEBULK = $80;
    TYPEISOCHRONOUS = $C0;
    // Multi Packet Transfer Enable
    MULTIPKTbm = $20;
    // Ping-Pong Enable
    PINGPONGbm = $10;
    // Interrupt Disable
    INTDSBLbm = $08;
    // Data Stall
    STALLbm = $04;
    // BUFSIZE
    BUFSIZEmask = $07;
    BUFSIZE8 = $00;
    BUFSIZE16 = $01;
    BUFSIZE32 = $02;
    BUFSIZE64 = $03;
    BUFSIZE128 = $04;
    BUFSIZE256 = $05;
    BUFSIZE512 = $06;
    BUFSIZE1023 = $07;
  end;

  TUSB = object //Universal Serial Bus
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    STATUS: byte;  //Status Register
    ADDR: byte;  //Address Register
    FIFOWP: byte;  //FIFO Write Pointer Register
    FIFORP: byte;  //FIFO Read Pointer Register
    EPPTR: word;  //Endpoint Configuration Table Pointer
    INTCTRLA: byte;  //Interrupt Control Register A
    INTCTRLB: byte;  //Interrupt Control Register B
    INTFLAGSACLR: byte;  //Clear Interrupt Flag Register A
    INTFLAGSASET: byte;  //Set Interrupt Flag Register A
    INTFLAGSBCLR: byte;  //Clear Interrupt Flag Register B
    INTFLAGSBSET: byte;  //Set Interrupt Flag Register B
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
    CAL0: byte;  //Calibration Byte 0
    CAL1: byte;  //Calibration Byte 1
  const
    // USB Enable
    ENABLEbm = $80;
    // Speed Select
    SPEEDbm = $40;
    // USB FIFO Enable
    FIFOENbm = $20;
    // Store Frame Number Enable
    STFRNUMbm = $10;
    // Maximum Endpoint Addresses
    MAXEP0bm = $01;
    MAXEP1bm = $02;
    MAXEP2bm = $04;
    MAXEP3bm = $08;
    // Pull during Reset
    PULLRSTbm = $10;
    // Remote Wake-up
    RWAKEUPbm = $04;
    // Global NACK
    GNACKbm = $02;
    // Attach
    ATTACHbm = $01;
    // Upstream Resume
    URESUMEbm = $08;
    // Resume
    RESUMEbm = $04;
    // Bus Suspended
    SUSPENDbm = $02;
    // Bus Reset
    BUSRSTbm = $01;
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
    // Start Of Frame Interrupt Enable
    SOFIEbm = $80;
    // Bus Event Interrupt Enable
    BUSEVIEbm = $40;
    // Bus Error Interrupt Enable
    BUSERRIEbm = $20;
    // STALL Interrupt Enable
    STALLIEbm = $10;
    // USB_INTLVL
    INTLVLmask = $03;
    INTLVL_OFF = $00;
    INTLVL_LO = $01;
    INTLVL_MED = $02;
    INTLVL_HI = $03;
    // Transaction Complete Interrupt Enable
    TRNIEbm = $02;
    // SETUP Transaction Complete Interrupt Enable
    SETUPIEbm = $01;
    // Start Of Frame Interrupt Flag
    SOFIFbm = $80;
    // Suspend Interrupt Flag
    SUSPENDIFbm = $40;
    // Resume Interrupt Flag
    RESUMEIFbm = $20;
    // Reset Interrupt Flag
    RSTIFbm = $10;
    // Isochronous CRC Error Interrupt Flag
    CRCIFbm = $08;
    // Underflow Interrupt Flag
    UNFIFbm = $04;
    // Overflow Interrupt Flag
    OVFIFbm = $02;
    // STALL Interrupt Flag
    STALLIFbm = $01;
    // Transaction Complete Interrupt Flag
    TRNIFbm = $02;
    // SETUP Transaction Complete Interrupt Flag
    SETUPIFbm = $01;
  end;

  TUSB_EP_TABLE = object //USB Endpoint Table
    EP0OUT: TUSB_EP;  //Endpoint 0
    EP0IN: TUSB_EP;  //Endpoint 0
    EP1OUT: TUSB_EP;  //Endpoint 1
    EP1IN: TUSB_EP;  //Endpoint 1
    EP2OUT: TUSB_EP;  //Endpoint 2
    EP2IN: TUSB_EP;  //Endpoint 2
    EP3OUT: TUSB_EP;  //Endpoint 3
    EP3IN: TUSB_EP;  //Endpoint 3
    EP4OUT: TUSB_EP;  //Endpoint 4
    EP4IN: TUSB_EP;  //Endpoint 4
    EP5OUT: TUSB_EP;  //Endpoint 5
    EP5IN: TUSB_EP;  //Endpoint 5
    EP6OUT: TUSB_EP;  //Endpoint 6
    EP6IN: TUSB_EP;  //Endpoint 6
    EP7OUT: TUSB_EP;  //Endpoint 7
    EP7IN: TUSB_EP;  //Endpoint 7
    EP8OUT: TUSB_EP;  //Endpoint 8
    EP8IN: TUSB_EP;  //Endpoint 8
    EP9OUT: TUSB_EP;  //Endpoint 9
    EP9IN: TUSB_EP;  //Endpoint 9
    EP10OUT: TUSB_EP;  //Endpoint 10
    EP10IN: TUSB_EP;  //Endpoint 10
    EP11OUT: TUSB_EP;  //Endpoint 11
    EP11IN: TUSB_EP;  //Endpoint 11
    EP12OUT: TUSB_EP;  //Endpoint 12
    EP12IN: TUSB_EP;  //Endpoint 12
    EP13OUT: TUSB_EP;  //Endpoint 13
    EP13IN: TUSB_EP;  //Endpoint 13
    EP14OUT: TUSB_EP;  //Endpoint 14
    EP14IN: TUSB_EP;  //Endpoint 14
    EP15OUT: TUSB_EP;  //Endpoint 15
    EP15IN: TUSB_EP;  //Endpoint 15
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
    FRAMENUML: byte;  //Frame Number Low Byte
    FRAMENUMH: byte;  //Frame Number High Byte
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
    INT0MASK: byte;  //Port Interrupt 0 Mask
    INT1MASK: byte;  //Port Interrupt 1 Mask
    INTFLAGS: byte;  //Interrupt Flag Register
    Reserved13: byte;
    REMAP: byte;  //I/O Port Pin Remap Register
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
    // PORT_INT1LVL
    INT1LVLmask = $0C;
    INT1LVL_OFF = $00;
    INT1LVL_LO = $04;
    INT1LVL_MED = $08;
    INT1LVL_HI = $0C;
    // PORT_INT0LVL
    INT0LVLmask = $03;
    INT0LVL_OFF = $00;
    INT0LVL_LO = $01;
    INT0LVL_MED = $02;
    INT0LVL_HI = $03;
    // Port Interrupt 1 Flag
    INT1IFbm = $02;
    // Port Interrupt 0 Flag
    INT0IFbm = $01;
    // SPI
    SPIbm = $20;
    // USART0
    USART0bm = $10;
    // Timer/Counter 0 Output Compare D
    TC0Dbm = $08;
    // Timer/Counter 0 Output Compare C
    TC0Cbm = $04;
    // Timer/Counter 0 Output Compare B
    TC0Bbm = $02;
    // Timer/Counter 0 Output Compare A
    TC0Abm = $01;
    // Slew Rate Enable
    SRLENbm = $80;
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
    ISC_INPUT_DISABLE = $07;
  end;

  TTC0 = object //16-bit Timer/Counter 0
    CTRLA: byte;  //Control  Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control register C
    CTRLD: byte;  //Control Register D
    CTRLE: byte;  //Control Register E
    Reserved5: byte;
    INTCTRLA: byte;  //Interrupt Control Register A
    INTCTRLB: byte;  //Interrupt Control Register B
    CTRLFCLR: byte;  //Control Register F Clear
    CTRLFSET: byte;  //Control Register F Set
    CTRLGCLR: byte;  //Control Register G Clear
    CTRLGSET: byte;  //Control Register G Set
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
    // TC_CLKSEL
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
    // Compare or Capture D Enable
    CCDENbm = $80;
    // Compare or Capture C Enable
    CCCENbm = $40;
    // Compare or Capture B Enable
    CCBENbm = $20;
    // Compare or Capture A Enable
    CCAENbm = $10;
    // TC_WGMODE
    WGMODEmask = $07;
    WGMODE_NORMAL = $00;
    WGMODE_FRQ = $01;
    WGMODE_SINGLESLOPE = $03;
    WGMODE_SS = $03;
    WGMODE_DSTOP = $05;
    WGMODE_DS_T = $05;
    WGMODE_DSBOTH = $06;
    WGMODE_DS_TB = $06;
    WGMODE_DSBOTTOM = $07;
    WGMODE_DS_B = $07;
    // Compare D Output Value
    CMPDbm = $08;
    // Compare C Output Value
    CMPCbm = $04;
    // Compare B Output Value
    CMPBbm = $02;
    // Compare A Output Value
    CMPAbm = $01;
    // TC_EVACT
    EVACTmask = $E0;
    EVACT_OFF = $00;
    EVACT_CAPT = $20;
    EVACT_UPDOWN = $40;
    EVACT_QDEC = $60;
    EVACT_RESTART = $80;
    EVACT_FRQ = $A0;
    EVACT_PW = $C0;
    // Event Delay
    EVDLYbm = $10;
    // TC_EVSEL
    EVSELmask = $0F;
    EVSEL_OFF = $00;
    EVSEL_CH0 = $08;
    EVSEL_CH1 = $09;
    EVSEL_CH2 = $0A;
    EVSEL_CH3 = $0B;
    // TC_BYTEM
    BYTEMmask = $03;
    BYTEM_NORMAL = $00;
    BYTEM_BYTEMODE = $01;
    BYTEM_SPLITMODE = $02;
    // TC_ERRINTLVL
    ERRINTLVLmask = $0C;
    ERRINTLVL_OFF = $00;
    ERRINTLVL_LO = $04;
    ERRINTLVL_MED = $08;
    ERRINTLVL_HI = $0C;
    // TC_OVFINTLVL
    OVFINTLVLmask = $03;
    OVFINTLVL_OFF = $00;
    OVFINTLVL_LO = $01;
    OVFINTLVL_MED = $02;
    OVFINTLVL_HI = $03;
    // TC_CCDINTLVL
    CCDINTLVLmask = $C0;
    CCDINTLVL_OFF = $00;
    CCDINTLVL_LO = $40;
    CCDINTLVL_MED = $80;
    CCDINTLVL_HI = $C0;
    // TC_CCCINTLVL
    CCCINTLVLmask = $30;
    CCCINTLVL_OFF = $00;
    CCCINTLVL_LO = $10;
    CCCINTLVL_MED = $20;
    CCCINTLVL_HI = $30;
    // TC_CCBINTLVL
    CCBINTLVLmask = $0C;
    CCBINTLVL_OFF = $00;
    CCBINTLVL_LO = $04;
    CCBINTLVL_MED = $08;
    CCBINTLVL_HI = $0C;
    // TC_CCAINTLVL
    CCAINTLVLmask = $03;
    CCAINTLVL_OFF = $00;
    CCAINTLVL_LO = $01;
    CCAINTLVL_MED = $02;
    CCAINTLVL_HI = $03;
    // Command
    CMD0bm = $04;
    CMD1bm = $08;
    // Lock Update
    LUPDbm = $02;
    // Direction
    DIRbm = $01;
    // Compare or Capture D Buffer Valid
    CCDBVbm = $10;
    // Compare or Capture C Buffer Valid
    CCCBVbm = $08;
    // Compare or Capture B Buffer Valid
    CCBBVbm = $04;
    // Compare or Capture A Buffer Valid
    CCABVbm = $02;
    // Period Buffer Valid
    PERBVbm = $01;
    // Compare or Capture D Interrupt Flag
    CCDIFbm = $80;
    // Compare or Capture C Interrupt Flag
    CCCIFbm = $40;
    // Compare or Capture B Interrupt Flag
    CCBIFbm = $20;
    // Compare or Capture A Interrupt Flag
    CCAIFbm = $10;
    // Error Interrupt Flag
    ERRIFbm = $02;
    // Overflow Interrupt Flag
    OVFIFbm = $01;
  end;

  TTC1 = object //16-bit Timer/Counter 1
    CTRLA: byte;  //Control  Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control register C
    CTRLD: byte;  //Control Register D
    CTRLE: byte;  //Control Register E
    Reserved5: byte;
    INTCTRLA: byte;  //Interrupt Control Register A
    INTCTRLB: byte;  //Interrupt Control Register B
    CTRLFCLR: byte;  //Control Register F Clear
    CTRLFSET: byte;  //Control Register F Set
    CTRLGCLR: byte;  //Control Register G Clear
    CTRLGSET: byte;  //Control Register G Set
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
    // TC_CLKSEL
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
    // Compare or Capture B Enable
    CCBENbm = $20;
    // Compare or Capture A Enable
    CCAENbm = $10;
    // TC_WGMODE
    WGMODEmask = $07;
    WGMODE_NORMAL = $00;
    WGMODE_FRQ = $01;
    WGMODE_SINGLESLOPE = $03;
    WGMODE_SS = $03;
    WGMODE_DSTOP = $05;
    WGMODE_DS_T = $05;
    WGMODE_DSBOTH = $06;
    WGMODE_DS_TB = $06;
    WGMODE_DSBOTTOM = $07;
    WGMODE_DS_B = $07;
    // Compare B Output Value
    CMPBbm = $02;
    // Compare A Output Value
    CMPAbm = $01;
    // TC_EVACT
    EVACTmask = $E0;
    EVACT_OFF = $00;
    EVACT_CAPT = $20;
    EVACT_UPDOWN = $40;
    EVACT_QDEC = $60;
    EVACT_RESTART = $80;
    EVACT_FRQ = $A0;
    EVACT_PW = $C0;
    // Event Delay
    EVDLYbm = $10;
    // TC_EVSEL
    EVSELmask = $0F;
    EVSEL_OFF = $00;
    EVSEL_CH0 = $08;
    EVSEL_CH1 = $09;
    EVSEL_CH2 = $0A;
    EVSEL_CH3 = $0B;
    // Byte Mode
    BYTEMbm = $01;
    // TC_ERRINTLVL
    ERRINTLVLmask = $0C;
    ERRINTLVL_OFF = $00;
    ERRINTLVL_LO = $04;
    ERRINTLVL_MED = $08;
    ERRINTLVL_HI = $0C;
    // TC_OVFINTLVL
    OVFINTLVLmask = $03;
    OVFINTLVL_OFF = $00;
    OVFINTLVL_LO = $01;
    OVFINTLVL_MED = $02;
    OVFINTLVL_HI = $03;
    // TC_CCBINTLVL
    CCBINTLVLmask = $0C;
    CCBINTLVL_OFF = $00;
    CCBINTLVL_LO = $04;
    CCBINTLVL_MED = $08;
    CCBINTLVL_HI = $0C;
    // TC_CCAINTLVL
    CCAINTLVLmask = $03;
    CCAINTLVL_OFF = $00;
    CCAINTLVL_LO = $01;
    CCAINTLVL_MED = $02;
    CCAINTLVL_HI = $03;
    // Command
    CMD0bm = $04;
    CMD1bm = $08;
    // Lock Update
    LUPDbm = $02;
    // Direction
    DIRbm = $01;
    // Compare or Capture B Buffer Valid
    CCBBVbm = $04;
    // Compare or Capture A Buffer Valid
    CCABVbm = $02;
    // Period Buffer Valid
    PERBVbm = $01;
    // Compare or Capture B Interrupt Flag
    CCBIFbm = $20;
    // Compare or Capture A Interrupt Flag
    CCAIFbm = $10;
    // Error Interrupt Flag
    ERRIFbm = $02;
    // Overflow Interrupt Flag
    OVFIFbm = $01;
  end;

  TTC2 = object //16-bit Timer/Counter type 2
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control register C
    Reserved3: byte;
    CTRLE: byte;  //Control Register E
    Reserved5: byte;
    INTCTRLA: byte;  //Interrupt Control Register A
    INTCTRLB: byte;  //Interrupt Control Register B
    Reserved8: byte;
    CTRLF: byte;  //Control Register F
    Reserved10: byte;
    Reserved11: byte;
    INTFLAGS: byte;  //Interrupt Flag Register
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
    LCNT: byte;  //Low Byte Count
    HCNT: byte;  //High Byte Count
    Reserved34: byte;
    Reserved35: byte;
    Reserved36: byte;
    Reserved37: byte;
    LPER: byte;  //Low Byte Period
    HPER: byte;  //High Byte Period
    LCMPA: byte;  //Low Byte Compare A
    HCMPA: byte;  //High Byte Compare A
    LCMPB: byte;  //Low Byte Compare B
    HCMPB: byte;  //High Byte Compare B
    LCMPC: byte;  //Low Byte Compare C
    HCMPC: byte;  //High Byte Compare C
    LCMPD: byte;  //Low Byte Compare D
    HCMPD: byte;  //High Byte Compare D
  const
    // TC2_CLKSEL
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
    // High Byte Compare D Enable
    HCMPDENbm = $80;
    // High Byte Compare C Enable
    HCMPCENbm = $40;
    // High Byte Compare B Enable
    HCMPBENbm = $20;
    // High Byte Compare A Enable
    HCMPAENbm = $10;
    // Low Byte Compare D Enable
    LCMPDENbm = $08;
    // Low Byte Compare C Enable
    LCMPCENbm = $04;
    // Low Byte Compare B Enable
    LCMPBENbm = $02;
    // Low Byte Compare A Enable
    LCMPAENbm = $01;
    // High Byte Compare D Output Value
    HCMPDbm = $80;
    // High Byte Compare C Output Value
    HCMPCbm = $40;
    // High Byte Compare B Output Value
    HCMPBbm = $20;
    // High Byte Compare A Output Value
    HCMPAbm = $10;
    // Low Byte Compare D Output Value
    LCMPDbm = $08;
    // Low Byte Compare C Output Value
    LCMPCbm = $04;
    // Low Byte Compare B Output Value
    LCMPBbm = $02;
    // Low Byte Compare A Output Value
    LCMPAbm = $01;
    // TC2_BYTEM
    BYTEMmask = $03;
    BYTEM_NORMAL = $00;
    BYTEM_BYTEMODE = $01;
    BYTEM_SPLITMODE = $02;
    // TC2_HUNFINTLVL
    HUNFINTLVLmask = $0C;
    HUNFINTLVL_OFF = $00;
    HUNFINTLVL_LO = $04;
    HUNFINTLVL_MED = $08;
    HUNFINTLVL_HI = $0C;
    // TC2_LUNFINTLVL
    LUNFINTLVLmask = $03;
    LUNFINTLVL_OFF = $00;
    LUNFINTLVL_LO = $01;
    LUNFINTLVL_MED = $02;
    LUNFINTLVL_HI = $03;
    // TC2_LCMPDINTLVL
    LCMPDINTLVLmask = $C0;
    LCMPDINTLVL_OFF = $00;
    LCMPDINTLVL_LO = $40;
    LCMPDINTLVL_MED = $80;
    LCMPDINTLVL_HI = $C0;
    // TC2_LCMPCINTLVL
    LCMPCINTLVLmask = $30;
    LCMPCINTLVL_OFF = $00;
    LCMPCINTLVL_LO = $10;
    LCMPCINTLVL_MED = $20;
    LCMPCINTLVL_HI = $30;
    // TC2_LCMPBINTLVL
    LCMPBINTLVLmask = $0C;
    LCMPBINTLVL_OFF = $00;
    LCMPBINTLVL_LO = $04;
    LCMPBINTLVL_MED = $08;
    LCMPBINTLVL_HI = $0C;
    // TC2_LCMPAINTLVL
    LCMPAINTLVLmask = $03;
    LCMPAINTLVL_OFF = $00;
    LCMPAINTLVL_LO = $01;
    LCMPAINTLVL_MED = $02;
    LCMPAINTLVL_HI = $03;
    // TC2_CMD
    CMDmask = $0C;
    CMD_NONE = $00;
    CMD_RESTART = $08;
    CMD_RESET = $0C;
    // TC2_CMDEN
    CMDENmask = $03;
    CMDEN_LOW = $01;
    CMDEN_HIGH = $02;
    CMDEN_BOTH = $03;
    // Low Byte Compare D Interrupt Flag
    LCMPDIFbm = $80;
    // Low Byte Compare C Interrupt Flag
    LCMPCIFbm = $40;
    // Low Byte Compare B Interrupt Flag
    LCMPBIFbm = $20;
    // Low Byte Compare A Interrupt Flag
    LCMPAIFbm = $10;
    // High Byte Underflow Interrupt Flag
    HUNFIFbm = $02;
    // Low Byte Underflow Interrupt Flag
    LUNFIFbm = $01;
  end;

  TAWEX = object //Advanced Waveform Extension
    CTRL: byte;  //Control Register
    Reserved1: byte;
    FDEMASK: byte;  //Fault Detection Event Mask
    FDCTRL: byte;  //Fault Detection Control Register
    STATUS: byte;  //Status Register
    STATUSSET: byte;  //Status Set Register
    DTBOTH: byte;  //Dead Time Both Sides
    DTBOTHBUF: byte;  //Dead Time Both Sides Buffer
    DTLS: byte;  //Dead Time Low Side
    DTHS: byte;  //Dead Time High Side
    DTLSBUF: byte;  //Dead Time Low Side Buffer
    DTHSBUF: byte;  //Dead Time High Side Buffer
    OUTOVEN: byte;  //Output Override Enable
  const
    // Pattern Generation Mode
    PGMbm = $20;
    // Common Waveform Channel Mode
    CWCMbm = $10;
    // Dead Time Insertion Compare Channel D Enable
    DTICCDENbm = $08;
    // Dead Time Insertion Compare Channel C Enable
    DTICCCENbm = $04;
    // Dead Time Insertion Compare Channel B Enable
    DTICCBENbm = $02;
    // Dead Time Insertion Compare Channel A Enable
    DTICCAENbm = $01;
    // Fault Detect on Disable Break Disable
    FDDBDbm = $10;
    // Fault Detect Mode
    FDMODEbm = $04;
    // AWEX_FDACT
    FDACTmask = $03;
    FDACT_NONE = $00;
    FDACT_CLEAROE = $01;
    FDACT_CLEARDIR = $03;
    // Fault Detect Flag
    FDFbm = $04;
    // Dead Time High Side Buffer Valid
    DTHSBUFVbm = $02;
    // Dead Time Low Side Buffer Valid
    DTLSBUFVbm = $01;
  end;

  THIRES = object //High-Resolution Extension
    CTRLA: byte;  //Control Register
  const
    // High Resolution Plus
    HRPLUSbm = $04;
    // HIRES_HREN
    HRENmask = $03;
    HREN_NONE = $00;
    HREN_TC0 = $01;
    HREN_TC1 = $02;
    HREN_BOTH = $03;
  end;

  TUSART = object //Universal Synchronous/Asynchronous Receiver/Transmitter
    DATA: byte;  //Data Register
    STATUS: byte;  //Status Register
    Reserved2: byte;
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control Register C
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
    // Receive Bit 8
    RXB8bm = $01;
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
    // Baud Rate Scale
    BSCALE0bm = $10;
    BSCALE1bm = $20;
    BSCALE2bm = $40;
    BSCALE3bm = $80;
  end;

  TSPI = object //Serial Peripheral Interface
    CTRL: byte;  //Control Register
    INTCTRL: byte;  //Interrupt Control Register
    STATUS: byte;  //Status Register
    DATA: byte;  //Data Register
  const
    // Enable Double Speed
    CLK2Xbm = $80;
    // Enable Module
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
    // SPI_INTLVL
    INTLVLmask = $03;
    INTLVL_OFF = $00;
    INTLVL_LO = $01;
    INTLVL_MED = $02;
    INTLVL_HI = $03;
    // Interrupt Flag
    IFbm = $80;
    // Write Collision
    WRCOLbm = $40;
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
  end;

  TLCD = object //LCD Controller
    CTRLA: byte;  //Control Register A
    CTRLB: byte;  //Control Register B
    CTRLC: byte;  //Control Register C
    INTCTRL: byte;  //Interrupt Enable Register
    INTFLAG: byte;  //Interrupt Flag Register
    CTRLD: byte;  //Control Register D
    CTRLE: byte;  //Control Register E
    CTRLF: byte;  //Control Register F
    CTRLG: byte;  //Control Register G
    CTRLH: byte;  //Control Register H
    Reserved10: byte;
    Reserved11: byte;
    Reserved12: byte;
    Reserved13: byte;
    Reserved14: byte;
    Reserved15: byte;
    DATA0: byte;  //LCD Data Register 0
    DATA1: byte;  //LCD Data Register 1
    DATA2: byte;  //LCD Data Register 2
    DATA3: byte;  //LCD Data Register 3
    DATA4: byte;  //LCD Data Register 4
    DATA5: byte;  //LCD Data Register 5
    DATA6: byte;  //LCD Data Register 6
    DATA7: byte;  //LCD Data Register 7
    DATA8: byte;  //LCD Data Register 8
    DATA9: byte;  //LCD Data Register 9
    DATA10: byte;  //LCD Data Register 10
    DATA11: byte;  //LCD Data Register 11
    DATA12: byte;  //LCD Data Register 12
    DATA13: byte;  //LCD Data Register 13
    DATA14: byte;  //LCD Data Register 14
    DATA15: byte;  //LCD Data Register 15
    DATA16: byte;  //LCD Data Register 16
    DATA17: byte;  //LCD Data Register 17
    DATA18: byte;  //LCD Data Register 18
    DATA19: byte;  //LCD Data Register 19
  const
    // LCD Enable
    ENABLEbm = $80;
    // External Register Bias Generation
    XBIASbm = $40;
    // Data Register Lock
    DATCLKbm = $20;
    // Common Bus Swap
    COMSWPbm = $10;
    // Segment Bus Swap
    SEGSWPbm = $08;
    // Clear Data Register
    CLRDTbm = $04;
    // Segments On
    SEGONbm = $02;
    // Blanking Display Mode
    BLANKbm = $01;
    // LCD_PRESC
    PRESCmask = $80;
    PRESC_8 = $00;
    PRESC_16 = $80;
    // LCD_CLKDIV
    CLKDIVmask = $70;
    CLKDIV_DivBy1 = $00;
    CLKDIV_DivBy2 = $10;
    CLKDIV_DivBy3 = $20;
    CLKDIV_DivBy4 = $30;
    CLKDIV_DivBy5 = $40;
    CLKDIV_DivBy6 = $50;
    CLKDIV_DivBy7 = $60;
    CLKDIV_DivBy8 = $70;
    // Low Power Waveform
    LPWAVbm = $08;
    // LCD_DUTY
    DUTYmask = $03;
    DUTY_1_4 = $00;
    DUTY_Static = $01;
    DUTY_1_2 = $02;
    DUTY_1_3 = $03;
    // LCD Port Mask
    PMSK0bm = $01;
    PMSK1bm = $02;
    PMSK2bm = $04;
    PMSK3bm = $08;
    PMSK4bm = $10;
    PMSK5bm = $20;
    // eXtended Interrupt Mode Enable
    XIME0bm = $08;
    XIME1bm = $10;
    XIME2bm = $20;
    XIME3bm = $40;
    XIME4bm = $80;
    // FCINTLVL
    FCINTLVLmask = $03;
    FCINTLVLOFF = $00;
    FCINTLVLLO = $01;
    FCINTLVLMED = $02;
    FCINTLVLHI = $03;
    // LCD Frame Completed Interrupt Flag
    FCIFbm = $01;
    // Blink Enable
    BLINKENbm = $08;
    // LCD_BLINKRATE
    BLINKRATEmask = $03;
    BLINKRATE_4Hz = $00;
    BLINKRATE_2Hz = $01;
    BLINKRATE_1Hz = $02;
    BLINKRATE_0Hz5 = $03;
    // Blink Pixel Selection 1
    BPS10bm = $10;
    BPS11bm = $20;
    BPS12bm = $40;
    BPS13bm = $80;
    // Blink Pixel Selection 0
    BPS00bm = $01;
    BPS01bm = $02;
    BPS02bm = $04;
    BPS03bm = $08;
    // Fine Contrast
    FCONT0bm = $01;
    FCONT1bm = $02;
    FCONT2bm = $04;
    FCONT3bm = $08;
    FCONT4bm = $10;
    FCONT5bm = $20;
    // LCD_TDG
    TDGmask = $C0;
    TDG_7S_3C = $00;
    TDG_7S_4C = $40;
    TDG_14S_4C = $80;
    TDG_16S_3C = $C0;
    // Start Segment
    STSEG0bm = $01;
    STSEG1bm = $02;
    STSEG2bm = $04;
    STSEG3bm = $08;
    STSEG4bm = $10;
    STSEG5bm = $20;
    // Decrement of Start Segment
    DECbm = $80;
    // Display Code
    DCODE0bm = $01;
    DCODE1bm = $02;
    DCODE2bm = $04;
    DCODE3bm = $08;
    DCODE4bm = $10;
    DCODE5bm = $20;
    DCODE6bm = $40;
  end;

  TNVM_FUSES = object //Fuses
    FUSEBYTE0: byte;  //JTAG User ID
    FUSEBYTE1: byte;  //Watchdog Configuration
    FUSEBYTE2: byte;  //Reset Configuration
    Reserved3: byte;
    FUSEBYTE4: byte;  //Start-up Configuration
    FUSEBYTE5: byte;  //EESAVE and BOD Level
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
    // TOSCSEL
    TOSCSELmask = $20;
    TOSCSEL_ALTERNATE = $00;
    TOSCSEL_XTAL = $20;
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
    // JTAG Interface Enable
    JTAGENbm = $01;
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

  TNVM_PROD_SIGNATURES = object //Production Signatures
    RCOSC2M: byte;  //RCOSC 2 MHz Calibration Value B
    RCOSC2MA: byte;  //RCOSC 2 MHz Calibration Value A
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
    USBCAL0: byte;  //USB Calibration Byte 0
    USBCAL1: byte;  //USB Calibration Byte 1
    USBRCOSC: byte;  //USB RCOSC Calibration Value B
    USBRCOSCA: byte;  //USB RCOSC Calibration Value A
    Reserved30: byte;
    Reserved31: byte;
    ADCACAL0: byte;  //ADCA Calibration Byte 0
    ADCACAL1: byte;  //ADCA Calibration Byte 1
    Reserved34: byte;
    Reserved35: byte;
    ADCBCAL0: byte;  //ADCB Calibration Byte 0
    ADCBCAL1: byte;  //ADCB Calibration Byte 1
    Reserved38: byte;
    Reserved39: byte;
    Reserved40: byte;
    Reserved41: byte;
    Reserved42: byte;
    Reserved43: byte;
    Reserved44: byte;
    Reserved45: byte;
    TEMPSENSE0: byte;  //Temperature Sensor Calibration Byte 0
    TEMPSENSE1: byte;  //Temperature Sensor Calibration Byte 1
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
  DFLLRC2M: TDFLL absolute $0068;
  PR: TPR absolute $0070;
  RST: TRST absolute $0078;
  WDT: TWDT absolute $0080;
  MCU: TMCU absolute $0090;
  PMIC: TPMIC absolute $00A0;
  PORTCFG: TPORTCFG absolute $00B0;
  AES: TAES absolute $00C0;
  CRC: TCRC absolute $00D0;
  DMA: TDMA absolute $0100;
  EVSYS: TEVSYS absolute $0180;
  NVM: TNVM absolute $01C0;
  ADCB: TADC absolute $0240;
  ACB: TAC absolute $0390;
  RTC: TRTC absolute $0400;
  TWIC: TTWI absolute $0480;
  USB: TUSB absolute $04C0;
  PORTB: TPORT absolute $0620;
  PORTC: TPORT absolute $0640;
  PORTD: TPORT absolute $0660;
  PORTG: TPORT absolute $06C0;
  PORTM: TPORT absolute $0760;
  PORTR: TPORT absolute $07E0;
  TCC0: TTC0 absolute $0800;
  TCC2: TTC2 absolute $0800;
  TCC1: TTC1 absolute $0840;
  AWEXC: TAWEX absolute $0880;
  HIRESC: THIRES absolute $0890;
  USARTC0: TUSART absolute $08A0;
  SPIC: TSPI absolute $08C0;
  IRCOM: TIRCOM absolute $08F8;
  LCD: TLCD absolute $0D00;

implementation

{$i avrcommon.inc}

procedure OSC_OSCF_ISR; external name 'OSC_OSCF_ISR'; // Interrupt 1 Oscillator Failure Interrupt (NMI)
procedure PORTC_INT0_ISR; external name 'PORTC_INT0_ISR'; // Interrupt 2 External Interrupt 0
procedure PORTC_INT1_ISR; external name 'PORTC_INT1_ISR'; // Interrupt 3 External Interrupt 1
procedure PORTR_INT0_ISR; external name 'PORTR_INT0_ISR'; // Interrupt 4 External Interrupt 0
procedure PORTR_INT1_ISR; external name 'PORTR_INT1_ISR'; // Interrupt 5 External Interrupt 1
procedure DMA_CH0_ISR; external name 'DMA_CH0_ISR'; // Interrupt 6 Channel 0 Interrupt
procedure DMA_CH1_ISR; external name 'DMA_CH1_ISR'; // Interrupt 7 Channel 1 Interrupt
procedure RTC_OVF_ISR; external name 'RTC_OVF_ISR'; // Interrupt 10 Overflow Interrupt
procedure RTC_COMP_ISR; external name 'RTC_COMP_ISR'; // Interrupt 11 Compare Interrupt
procedure TWIC_TWIS_ISR; external name 'TWIC_TWIS_ISR'; // Interrupt 12 TWI Slave Interrupt
procedure TWIC_TWIM_ISR; external name 'TWIC_TWIM_ISR'; // Interrupt 13 TWI Master Interrupt
procedure TCC2_LUNF_ISR; external name 'TCC2_LUNF_ISR'; // Interrupt 14 Low Byte Underflow Interrupt
procedure TCC2_HUNF_ISR; external name 'TCC2_HUNF_ISR'; // Interrupt 15 High Byte Underflow Interrupt
procedure TCC2_LCMPA_ISR; external name 'TCC2_LCMPA_ISR'; // Interrupt 16 Low Byte Compare A Interrupt
procedure TCC2_LCMPB_ISR; external name 'TCC2_LCMPB_ISR'; // Interrupt 17 Low Byte Compare B Interrupt
procedure TCC2_LCMPC_ISR; external name 'TCC2_LCMPC_ISR'; // Interrupt 18 Low Byte Compare C Interrupt
procedure TCC2_LCMPD_ISR; external name 'TCC2_LCMPD_ISR'; // Interrupt 19 Low Byte Compare D Interrupt
procedure TCC1_OVF_ISR; external name 'TCC1_OVF_ISR'; // Interrupt 20 Overflow Interrupt
procedure TCC1_ERR_ISR; external name 'TCC1_ERR_ISR'; // Interrupt 21 Error Interrupt
procedure TCC1_CCA_ISR; external name 'TCC1_CCA_ISR'; // Interrupt 22 Compare or Capture A Interrupt
procedure TCC1_CCB_ISR; external name 'TCC1_CCB_ISR'; // Interrupt 23 Compare or Capture B Interrupt
procedure SPIC_INT_ISR; external name 'SPIC_INT_ISR'; // Interrupt 24 SPI Interrupt
procedure USARTC0_RXC_ISR; external name 'USARTC0_RXC_ISR'; // Interrupt 25 Reception Complete Interrupt
procedure USARTC0_DRE_ISR; external name 'USARTC0_DRE_ISR'; // Interrupt 26 Data Register Empty Interrupt
procedure USARTC0_TXC_ISR; external name 'USARTC0_TXC_ISR'; // Interrupt 27 Transmission Complete Interrupt
procedure USB_BUSEVENT_ISR; external name 'USB_BUSEVENT_ISR'; // Interrupt 31 SOF, suspend, resume, reset bus event interrupts, crc, underflow, overflow and stall error interrupts
procedure USB_TRNCOMPL_ISR; external name 'USB_TRNCOMPL_ISR'; // Interrupt 32 Transaction complete interrupt
procedure LCD_INT_ISR; external name 'LCD_INT_ISR'; // Interrupt 35 LCD Interrupt
procedure AES_INT_ISR; external name 'AES_INT_ISR'; // Interrupt 36 AES Interrupt
procedure NVM_EE_ISR; external name 'NVM_EE_ISR'; // Interrupt 37 EE Interrupt
procedure NVM_SPM_ISR; external name 'NVM_SPM_ISR'; // Interrupt 38 SPM Interrupt
procedure PORTB_INT0_ISR; external name 'PORTB_INT0_ISR'; // Interrupt 39 External Interrupt 0
procedure PORTB_INT1_ISR; external name 'PORTB_INT1_ISR'; // Interrupt 40 External Interrupt 1
procedure ACB_AC0_ISR; external name 'ACB_AC0_ISR'; // Interrupt 41 AC0 Interrupt
procedure ACB_AC1_ISR; external name 'ACB_AC1_ISR'; // Interrupt 42 AC1 Interrupt
procedure ACB_ACW_ISR; external name 'ACB_ACW_ISR'; // Interrupt 43 ACW Window Mode Interrupt
procedure ADCB_CH0_ISR; external name 'ADCB_CH0_ISR'; // Interrupt 44 Interrupt 0
procedure PORTD_INT0_ISR; external name 'PORTD_INT0_ISR'; // Interrupt 48 External Interrupt 0
procedure PORTD_INT1_ISR; external name 'PORTD_INT1_ISR'; // Interrupt 49 External Interrupt 1
procedure PORTG_INT0_ISR; external name 'PORTG_INT0_ISR'; // Interrupt 50 External Interrupt 0
procedure PORTG_INT1_ISR; external name 'PORTG_INT1_ISR'; // Interrupt 51 External Interrupt 1
procedure PORTM_INT0_ISR; external name 'PORTM_INT0_ISR'; // Interrupt 52 External Interrupt 0
procedure PORTM_INT1_ISR; external name 'PORTM_INT1_ISR'; // Interrupt 53 External Interrupt 1

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
asm
  jmp __dtors_end
  jmp OSC_OSCF_ISR
  jmp PORTC_INT0_ISR
  jmp PORTC_INT1_ISR
  jmp PORTR_INT0_ISR
  jmp PORTR_INT1_ISR
  jmp DMA_CH0_ISR
  jmp DMA_CH1_ISR
  jmp RTC_OVF_ISR
  jmp RTC_COMP_ISR
  jmp TWIC_TWIS_ISR
  jmp TWIC_TWIM_ISR
  jmp TCC2_LUNF_ISR
  jmp TCC2_HUNF_ISR
  jmp TCC2_LCMPA_ISR
  jmp TCC2_LCMPB_ISR
  jmp TCC2_LCMPC_ISR
  jmp TCC2_LCMPD_ISR
  jmp TCC1_OVF_ISR
  jmp TCC1_ERR_ISR
  jmp TCC1_CCA_ISR
  jmp TCC1_CCB_ISR
  jmp SPIC_INT_ISR
  jmp USARTC0_RXC_ISR
  jmp USARTC0_DRE_ISR
  jmp USARTC0_TXC_ISR
  jmp USB_BUSEVENT_ISR
  jmp USB_TRNCOMPL_ISR
  jmp LCD_INT_ISR
  jmp AES_INT_ISR
  jmp NVM_EE_ISR
  jmp NVM_SPM_ISR
  jmp PORTB_INT0_ISR
  jmp PORTB_INT1_ISR
  jmp ACB_AC0_ISR
  jmp ACB_AC1_ISR
  jmp ACB_ACW_ISR
  jmp ADCB_CH0_ISR
  jmp PORTD_INT0_ISR
  jmp PORTD_INT1_ISR
  jmp PORTG_INT0_ISR
  jmp PORTG_INT1_ISR
  jmp PORTM_INT0_ISR
  jmp PORTM_INT1_ISR

  .weak OSC_OSCF_ISR
  .weak PORTC_INT0_ISR
  .weak PORTC_INT1_ISR
  .weak PORTR_INT0_ISR
  .weak PORTR_INT1_ISR
  .weak DMA_CH0_ISR
  .weak DMA_CH1_ISR
  .weak RTC_OVF_ISR
  .weak RTC_COMP_ISR
  .weak TWIC_TWIS_ISR
  .weak TWIC_TWIM_ISR
  .weak TCC2_LUNF_ISR
  .weak TCC2_HUNF_ISR
  .weak TCC2_LCMPA_ISR
  .weak TCC2_LCMPB_ISR
  .weak TCC2_LCMPC_ISR
  .weak TCC2_LCMPD_ISR
  .weak TCC1_OVF_ISR
  .weak TCC1_ERR_ISR
  .weak TCC1_CCA_ISR
  .weak TCC1_CCB_ISR
  .weak SPIC_INT_ISR
  .weak USARTC0_RXC_ISR
  .weak USARTC0_DRE_ISR
  .weak USARTC0_TXC_ISR
  .weak USB_BUSEVENT_ISR
  .weak USB_TRNCOMPL_ISR
  .weak LCD_INT_ISR
  .weak AES_INT_ISR
  .weak NVM_EE_ISR
  .weak NVM_SPM_ISR
  .weak PORTB_INT0_ISR
  .weak PORTB_INT1_ISR
  .weak ACB_AC0_ISR
  .weak ACB_AC1_ISR
  .weak ACB_ACW_ISR
  .weak ADCB_CH0_ISR
  .weak PORTD_INT0_ISR
  .weak PORTD_INT1_ISR
  .weak PORTG_INT0_ISR
  .weak PORTG_INT1_ISR
  .weak PORTM_INT0_ISR
  .weak PORTM_INT1_ISR

  .set OSC_OSCF_ISR, Default_IRQ_handler
  .set PORTC_INT0_ISR, Default_IRQ_handler
  .set PORTC_INT1_ISR, Default_IRQ_handler
  .set PORTR_INT0_ISR, Default_IRQ_handler
  .set PORTR_INT1_ISR, Default_IRQ_handler
  .set DMA_CH0_ISR, Default_IRQ_handler
  .set DMA_CH1_ISR, Default_IRQ_handler
  .set RTC_OVF_ISR, Default_IRQ_handler
  .set RTC_COMP_ISR, Default_IRQ_handler
  .set TWIC_TWIS_ISR, Default_IRQ_handler
  .set TWIC_TWIM_ISR, Default_IRQ_handler
  .set TCC2_LUNF_ISR, Default_IRQ_handler
  .set TCC2_HUNF_ISR, Default_IRQ_handler
  .set TCC2_LCMPA_ISR, Default_IRQ_handler
  .set TCC2_LCMPB_ISR, Default_IRQ_handler
  .set TCC2_LCMPC_ISR, Default_IRQ_handler
  .set TCC2_LCMPD_ISR, Default_IRQ_handler
  .set TCC1_OVF_ISR, Default_IRQ_handler
  .set TCC1_ERR_ISR, Default_IRQ_handler
  .set TCC1_CCA_ISR, Default_IRQ_handler
  .set TCC1_CCB_ISR, Default_IRQ_handler
  .set SPIC_INT_ISR, Default_IRQ_handler
  .set USARTC0_RXC_ISR, Default_IRQ_handler
  .set USARTC0_DRE_ISR, Default_IRQ_handler
  .set USARTC0_TXC_ISR, Default_IRQ_handler
  .set USB_BUSEVENT_ISR, Default_IRQ_handler
  .set USB_TRNCOMPL_ISR, Default_IRQ_handler
  .set LCD_INT_ISR, Default_IRQ_handler
  .set AES_INT_ISR, Default_IRQ_handler
  .set NVM_EE_ISR, Default_IRQ_handler
  .set NVM_SPM_ISR, Default_IRQ_handler
  .set PORTB_INT0_ISR, Default_IRQ_handler
  .set PORTB_INT1_ISR, Default_IRQ_handler
  .set ACB_AC0_ISR, Default_IRQ_handler
  .set ACB_AC1_ISR, Default_IRQ_handler
  .set ACB_ACW_ISR, Default_IRQ_handler
  .set ADCB_CH0_ISR, Default_IRQ_handler
  .set PORTD_INT0_ISR, Default_IRQ_handler
  .set PORTD_INT1_ISR, Default_IRQ_handler
  .set PORTG_INT0_ISR, Default_IRQ_handler
  .set PORTG_INT1_ISR, Default_IRQ_handler
  .set PORTM_INT0_ISR, Default_IRQ_handler
  .set PORTM_INT1_ISR, Default_IRQ_handler
end;

end.

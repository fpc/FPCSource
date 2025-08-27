unit ATxmega32D4;

interface

type
  TGPIO = object //General Purpose IO Registers
    GPIOR0: byte;  //General Purpose IO Register 0
    GPIOR1: byte;  //General Purpose IO Register 1
    GPIOR2: byte;  //General Purpose IO Register 2
    GPIOR3: byte;  //General Purpose IO Register 3
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
  end;

  TPR = object //Power Reduction
    PRGEN: byte;  //General Power Reduction
    PRPA: byte;  //Power Reduction Port A
    Reserved2: byte;
    PRPC: byte;  //Power Reduction Port C
    PRPD: byte;  //Power Reduction Port D
    PRPE: byte;  //Power Reduction Port E
    PRPF: byte;  //Power Reduction Port F
  const
    // Real-time Counter
    RTCbm = $04;
    // Event System
    EVSYSbm = $02;
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
    // Port C HIRES
    HIRESbm = $04;
    // Port C Timer/Counter1
    TC1bm = $02;
    // Port C Timer/Counter0
    TC0bm = $01;
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
    XOSCFAIL: byte;  //External Oscillator Failure Detection Register
    RC32KCAL: byte;  //32kHz Internal Oscillator Calibration Register
    PLLCTRL: byte;  //PLL Control REgister
    DFLLCTRL: byte;  //DFLL Control Register
  const
    // PLL Enable
    PLLENbm = $10;
    // External Oscillator Enable
    XOSCENbm = $08;
    // Internal 32kHz RC Oscillator Enable
    RC32KENbm = $04;
    // Internal 32MHz RC Oscillator Enable
    RC32MENbm = $02;
    // Internal 2MHz RC Oscillator Enable
    RC2MENbm = $01;
    // PLL Ready
    PLLRDYbm = $10;
    // External Oscillator Ready
    XOSCRDYbm = $08;
    // Internal 32kHz RC Oscillator Ready
    RC32KRDYbm = $04;
    // Internal 32MHz RC Oscillator Ready
    RC32MRDYbm = $02;
    // Internal 2MHz RC Oscillator Ready
    RC2MRDYbm = $01;
    // OSC_FRQRANGE
    FRQRANGEmask = $C0;
    FRQRANGE_04TO2 = $00;
    FRQRANGE_2TO9 = $40;
    FRQRANGE_9TO12 = $80;
    FRQRANGE_12TO16 = $C0;
    // 32kHz XTAL OSC Low-power Mode
    X32KLPMbm = $20;
    // OSC_XOSCSEL
    XOSCSELmask = $0F;
    XOSCSEL_EXTCLK = $00;
    XOSCSEL_32KHz = $02;
    XOSCSEL_XTAL_256CLK = $03;
    XOSCSEL_XTAL_1KCLK = $07;
    XOSCSEL_XTAL_16KCLK = $0B;
    // Failure Detection Interrupt Flag
    XOSCFDIFbm = $02;
    // Failure Detection Enable
    XOSCFDENbm = $01;
    // OSC_PLLSRC
    PLLSRCmask = $C0;
    PLLSRC_RC2M = $00;
    PLLSRC_RC32M = $80;
    PLLSRC_XOSC = $C0;
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
    // DFLL Calibration bits [6:0]
    CALL0bm = $01;
    CALL1bm = $02;
    CALL2bm = $04;
    CALL3bm = $08;
    CALL4bm = $10;
    CALL5bm = $20;
    CALL6bm = $40;
    // DFLL Calibration bits [12:7]
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
    JTAGUID: byte;  //JTAG User ID
    Reserved5: byte;
    MCUCR: byte;  //MCU Control
    Reserved7: byte;
    EVSYSLOCK: byte;  //Event System Lock
    AWEXLOCK: byte;  //AWEX Lock
  const
    // JTAG Disable
    JTAGDbm = $01;
    // Event Channel 4-7 Lock
    EVSYS1LOCKbm = $10;
    // Event Channel 0-3 Lock
    EVSYS0LOCKbm = $01;
    // AWeX on T/C E0 Lock
    AWEXELOCKbm = $04;
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
    // Zero detection
    ZERObm = $02;
    // Busy
    BUSYbm = $01;
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
    CHMUX_PORTD_PIN3 = $6B;
    CHMUX_PORTD_PIN4 = $6C;
    CHMUX_PORTD_PIN5 = $6D;
    CHMUX_PORTD_PIN6 = $6E;
    CHMUX_PORTD_PIN7 = $6F;
    CHMUX_PORTE_PIN0 = $70;
    CHMUX_PORTE_PIN1 = $71;
    CHMUX_PORTE_PIN2 = $72;
    CHMUX_PORTE_PIN3 = $73;
    CHMUX_PORTE_PIN4 = $74;
    CHMUX_PORTE_PIN5 = $75;
    CHMUX_PORTE_PIN6 = $76;
    CHMUX_PORTE_PIN7 = $77;
    CHMUX_PORTF_PIN0 = $78;
    CHMUX_PORTF_PIN1 = $79;
    CHMUX_PORTF_PIN2 = $7A;
    CHMUX_PORTF_PIN3 = $7B;
    CHMUX_PORTF_PIN4 = $7C;
    CHMUX_PORTF_PIN5 = $7D;
    CHMUX_PORTF_PIN6 = $7E;
    CHMUX_PORTF_PIN7 = $7F;
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
    CHMUX_TCD0_OVF = $D0;
    CHMUX_TCD0_ERR = $D1;
    CHMUX_TCD0_CCA = $D4;
    CHMUX_TCD0_CCB = $D5;
    CHMUX_TCD0_CCC = $D6;
    CHMUX_TCD0_CCD = $D7;
    CHMUX_TCE0_OVF = $E0;
    CHMUX_TCE0_ERR = $E1;
    CHMUX_TCE0_CCA = $E4;
    CHMUX_TCE0_CCB = $E5;
    CHMUX_TCE0_CCC = $E6;
    CHMUX_TCE0_CCD = $E7;
    CHMUX_TCF0_OVF = $F0;
    CHMUX_TCF0_ERR = $F1;
    CHMUX_TCF0_CCA = $F4;
    CHMUX_TCF0_CCB = $F5;
    CHMUX_TCF0_CCC = $F6;
    CHMUX_TCF0_CCD = $F7;
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
    CMD_READ_CALIB_ROW = $02;
    CMD_READ_USER_SIG_ROW = $01;
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
    BLBB_NOLOCK = $C0;
    BLBB_WLOCK = $80;
    BLBB_RLOCK = $40;
    BLBB_RWLOCK = $00;
    // NVM_BLBA
    BLBAmask = $30;
    BLBA_NOLOCK = $30;
    BLBA_WLOCK = $20;
    BLBA_RLOCK = $10;
    BLBA_RWLOCK = $00;
    // NVM_BLBAT
    BLBATmask = $0C;
    BLBAT_NOLOCK = $0C;
    BLBAT_WLOCK = $08;
    BLBAT_RLOCK = $04;
    BLBAT_RWLOCK = $00;
    // NVM_LB
    LBmask = $03;
    LB_NOLOCK = $03;
    LB_WLOCK = $02;
    LB_RWLOCK = $00;
  end;

  TNVM_LOCKBITS = object //Lock Bits
    LOCKBITS: byte;  //Lock Bits
  const
    // NVM_BLBB
    BLBBmask = $C0;
    BLBB_NOLOCK = $C0;
    BLBB_WLOCK = $80;
    BLBB_RLOCK = $40;
    BLBB_RWLOCK = $00;
    // NVM_BLBA
    BLBAmask = $30;
    BLBA_NOLOCK = $30;
    BLBA_WLOCK = $20;
    BLBA_RLOCK = $10;
    BLBA_RWLOCK = $00;
    // NVM_BLBAT
    BLBATmask = $0C;
    BLBAT_NOLOCK = $0C;
    BLBAT_WLOCK = $08;
    BLBAT_RLOCK = $04;
    BLBAT_RWLOCK = $00;
    // NVM_LB
    LBmask = $03;
    LB_NOLOCK = $03;
    LB_WLOCK = $02;
    LB_RWLOCK = $00;
  end;

  TNVM_FUSES = object //Fuses
    Reserved0: byte;
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
    BODPD_INSAMPLEDMODE = $01;
    BODPD_CONTINOUSLY = $02;
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
    BODACT_INSAMPLEDMODE = $10;
    BODACT_CONTINOUSLY = $20;
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

  TNVM_PROD_SIGNATURES = object //Production Signatures
    RCOSC2M: byte;  //RCOSC 2MHz Calibration Value B
    RCOSC2MA: byte;  //RCOSC 2MHz Calibration Value A
    RCOSC32K: byte;  //RCOSC 32kHz Calibration Value
    RCOSC32M: byte;  //RCOSC 32MHz Calibration Value B
    RCOSC32MA: byte;  //RCOSC 32MHz Calibration Value A
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
    TEMPSENSE1: byte;  //Temperature Sensor Calibration Byte 0
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
    // High-speed Mode
    HSMODEbm = $08;
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
    CHIFbm = $01;
    // Positive MUX setting offset
    OFFSET0bm = $10;
    OFFSET1bm = $20;
    OFFSET2bm = $40;
    OFFSET3bm = $80;
    // Number of Channels included in scan(Legacy name)
    SCANNUM0bm = $01;
    SCANNUM1bm = $02;
    SCANNUM2bm = $04;
    SCANNUM3bm = $08;
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
    SAMPCTRL: byte;  //Sampling Time Control Register
    Reserved9: byte;
    Reserved10: byte;
    Reserved11: byte;
    CAL: word;  //Calibration Value
    Reserved14: byte;
    Reserved15: byte;
    CH0RES: word;  //Channel 0 Result
    CH1RES: word;  //Channel 1 Result
    CH2RES: word;  //Channel 2 Result
    CH3RES: word;  //Channel 3 Result
    CMP: word;  //Compare Value
    Reserved26: byte;
    Reserved27: byte;
    Reserved28: byte;
    Reserved29: byte;
    Reserved30: byte;
    Reserved31: byte;
    CH0: TADC_CH;  //ADC Channel 0
    CH1: TADC_CH;  //ADC Channel 1
    CH2: TADC_CH;  //ADC Channel 2
    CH3: TADC_CH;  //ADC Channel 3
  const
    // ADC_DMASEL
    DMASELmask = $C0;
    DMASEL_OFF = $00;
    DMASEL_CH01 = $40;
    DMASEL_CH012 = $80;
    DMASEL_CH0123 = $C0;
    // Channel 3 Start Conversion
    CH3STARTbm = $20;
    // Channel 2 Start Conversion
    CH2STARTbm = $10;
    // Channel 1 Start Conversion
    CH1STARTbm = $08;
    // Channel 0 Start Conversion
    CH0STARTbm = $04;
    // Flush Pipeline
    FLUSHbm = $02;
    // Enable ADC
    ENABLEbm = $01;
    // Gain Stage Impedance Mode
    IMPMODEbm = $80;
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
    EVSELmask = $38;
    EVSEL_0123 = $00;
    EVSEL_1234 = $08;
    EVSEL_2345 = $10;
    EVSEL_3456 = $18;
    EVSEL_4567 = $20;
    EVSEL_567 = $28;
    EVSEL_67 = $30;
    EVSEL_7 = $38;
    // ADC_EVACT
    EVACTmask = $07;
    EVACT_NONE = $00;
    EVACT_CH0 = $01;
    EVACT_CH01 = $02;
    EVACT_CH012 = $03;
    EVACT_CH0123 = $04;
    EVACT_SWEEP = $05;
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
    // Channel 3 Interrupt Flag
    CH3IFbm = $08;
    // Channel 2 Interrupt Flag
    CH2IFbm = $04;
    // Channel 1 Interrupt Flag
    CH1IFbm = $02;
    // Channel 0 Interrupt Flag
    CH0IFbm = $01;
    // Sampling Time Control
    SAMPVAL0bm = $01;
    SAMPVAL1bm = $02;
    SAMPVAL2bm = $04;
    SAMPVAL3bm = $08;
    SAMPVAL4bm = $10;
    SAMPVAL5bm = $20;
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

  TPORTCFG = object //I/O port Configuration
    MPCMASK: byte;  //Multi-pin Configuration Mask
    Reserved1: byte;
    VPCTRLA: byte;  //Virtual Port Control Register A
    VPCTRLB: byte;  //Virtual Port Control Register B
    CLKEVOUT: byte;  //Clock and Event Out Register
  const
    // PORTCFG_VP1MAP
    VP1MAPmask = $F0;
    VP1MAP_PORTA = $00;
    VP1MAP_PORTB = $10;
    VP1MAP_PORTC = $20;
    VP1MAP_PORTD = $30;
    VP1MAP_PORTE = $40;
    VP1MAP_PORTF = $50;
    VP1MAP_PORTG = $60;
    VP1MAP_PORTH = $70;
    VP1MAP_PORTJ = $80;
    VP1MAP_PORTK = $90;
    VP1MAP_PORTL = $A0;
    VP1MAP_PORTM = $B0;
    VP1MAP_PORTN = $C0;
    VP1MAP_PORTP = $D0;
    VP1MAP_PORTQ = $E0;
    VP1MAP_PORTR = $F0;
    // PORTCFG_VP0MAP
    VP0MAPmask = $0F;
    VP0MAP_PORTA = $00;
    VP0MAP_PORTB = $01;
    VP0MAP_PORTC = $02;
    VP0MAP_PORTD = $03;
    VP0MAP_PORTE = $04;
    VP0MAP_PORTF = $05;
    VP0MAP_PORTG = $06;
    VP0MAP_PORTH = $07;
    VP0MAP_PORTJ = $08;
    VP0MAP_PORTK = $09;
    VP0MAP_PORTL = $0A;
    VP0MAP_PORTM = $0B;
    VP0MAP_PORTN = $0C;
    VP0MAP_PORTP = $0D;
    VP0MAP_PORTQ = $0E;
    VP0MAP_PORTR = $0F;
    // PORTCFG_VP3MAP
    VP3MAPmask = $F0;
    VP3MAP_PORTA = $00;
    VP3MAP_PORTB = $10;
    VP3MAP_PORTC = $20;
    VP3MAP_PORTD = $30;
    VP3MAP_PORTE = $40;
    VP3MAP_PORTF = $50;
    VP3MAP_PORTG = $60;
    VP3MAP_PORTH = $70;
    VP3MAP_PORTJ = $80;
    VP3MAP_PORTK = $90;
    VP3MAP_PORTL = $A0;
    VP3MAP_PORTM = $B0;
    VP3MAP_PORTN = $C0;
    VP3MAP_PORTP = $D0;
    VP3MAP_PORTQ = $E0;
    VP3MAP_PORTR = $F0;
    // PORTCFG_VP2MAP
    VP2MAPmask = $0F;
    VP2MAP_PORTA = $00;
    VP2MAP_PORTB = $01;
    VP2MAP_PORTC = $02;
    VP2MAP_PORTD = $03;
    VP2MAP_PORTE = $04;
    VP2MAP_PORTF = $05;
    VP2MAP_PORTG = $06;
    VP2MAP_PORTH = $07;
    VP2MAP_PORTJ = $08;
    VP2MAP_PORTK = $09;
    VP2MAP_PORTL = $0A;
    VP2MAP_PORTM = $0B;
    VP2MAP_PORTN = $0C;
    VP2MAP_PORTP = $0D;
    VP2MAP_PORTQ = $0E;
    VP2MAP_PORTR = $0F;
    // PORTCFG_CLKOUT
    CLKOUTmask = $03;
    CLKOUT_OFF = $00;
    CLKOUT_PC7 = $01;
    CLKOUT_PD7 = $02;
    CLKOUT_PE7 = $03;
    // PORTCFG_EVOUT
    EVOUTmask = $30;
    EVOUT_OFF = $00;
    EVOUT_PC7 = $10;
    EVOUT_PD7 = $20;
    EVOUT_PE7 = $30;
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
    REMAP: byte;  //Pin Remap Register (available for PORTC to PORTF only)
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
    // SPI Remap
    SPIbm = $20;
    // USART0 Remap
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
    CTRLA: byte;  //Control Register A
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
    CLKSEL_EVCH4 = $0C;
    CLKSEL_EVCH5 = $0D;
    CLKSEL_EVCH6 = $0E;
    CLKSEL_EVCH7 = $0F;
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
    WGMODE_SS = $03;
    WGMODE_DS_T = $05;
    WGMODE_DS_TB = $06;
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
    EVSEL_CH4 = $0C;
    EVSEL_CH5 = $0D;
    EVSEL_CH6 = $0E;
    EVSEL_CH7 = $0F;
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
    CLKSEL_EVCH4 = $0C;
    CLKSEL_EVCH5 = $0D;
    CLKSEL_EVCH6 = $0E;
    CLKSEL_EVCH7 = $0F;
    // Compare or Capture B Enable
    CCBENbm = $20;
    // Compare or Capture A Enable
    CCAENbm = $10;
    // TC_WGMODE
    WGMODEmask = $07;
    WGMODE_NORMAL = $00;
    WGMODE_FRQ = $01;
    WGMODE_SS = $03;
    WGMODE_DS_T = $05;
    WGMODE_DS_TB = $06;
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
    EVSEL_CH4 = $0C;
    EVSEL_CH5 = $0D;
    EVSEL_CH6 = $0E;
    EVSEL_CH7 = $0F;
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

  TAWEX = object //Advanced Waveform Extension
    CTRL: byte;  //Control Register
    Reserved1: byte;
    FDEMASK: byte;  //Fault Detection Event Mask
    FDCTRL: byte;  //Fault Detection Control Register
    STATUS: byte;  //Status Register
    Reserved5: byte;
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
    // HIRES_HREN
    HRENmask = $03;
    HREN_NONE = $00;
    HREN_TC0 = $01;
    HREN_TC1 = $02;
    HREN_BOTH = $03;
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
    // SPI Master Mode, Data Order
    UDORDbm = $04;
    // SPI Master Mode, Clock Phase
    UCPHAbm = $02;
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
    EVSEL_4 = $0C;
    EVSEL_5 = $0D;
    EVSEL_6 = $0E;
    EVSEL_7 = $0F;
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
  CRC: TCRC absolute $00D0;
  EVSYS: TEVSYS absolute $0180;
  NVM: TNVM absolute $01C0;
  ADCA: TADC absolute $0200;
  ACA: TAC absolute $0380;
  RTC: TRTC absolute $0400;
  TWIC: TTWI absolute $0480;
  TWIE: TTWI absolute $04A0;
  PORTA: TPORT absolute $0600;
  PORTB: TPORT absolute $0620;
  PORTC: TPORT absolute $0640;
  PORTD: TPORT absolute $0660;
  PORTE: TPORT absolute $0680;
  PORTR: TPORT absolute $07E0;
  TCC0: TTC0 absolute $0800;
  TCC2: TTC2 absolute $0800;
  TCC1: TTC1 absolute $0840;
  AWEXC: TAWEX absolute $0880;
  HIRESC: THIRES absolute $0890;
  USARTC0: TUSART absolute $08A0;
  SPIC: TSPI absolute $08C0;
  IRCOM: TIRCOM absolute $08F8;
  TCD0: TTC0 absolute $0900;
  USARTD0: TUSART absolute $09A0;
  SPID: TSPI absolute $09C0;
  TCE0: TTC0 absolute $0A00;

implementation

{$i avrcommon.inc}

procedure OSC_OSCF_ISR; external name 'OSC_OSCF_ISR'; // Interrupt 1 External Oscillator Failure Interrupt (NMI)
procedure PORTC_INT0_ISR; external name 'PORTC_INT0_ISR'; // Interrupt 2 External Interrupt 0
procedure PORTC_INT1_ISR; external name 'PORTC_INT1_ISR'; // Interrupt 3 External Interrupt 1
procedure PORTR_INT0_ISR; external name 'PORTR_INT0_ISR'; // Interrupt 4 External Interrupt 0
procedure PORTR_INT1_ISR; external name 'PORTR_INT1_ISR'; // Interrupt 5 External Interrupt 1
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
procedure NVM_EE_ISR; external name 'NVM_EE_ISR'; // Interrupt 32 EE Interrupt
procedure NVM_SPM_ISR; external name 'NVM_SPM_ISR'; // Interrupt 33 SPM Interrupt
procedure PORTB_INT0_ISR; external name 'PORTB_INT0_ISR'; // Interrupt 34 External Interrupt 0
procedure PORTB_INT1_ISR; external name 'PORTB_INT1_ISR'; // Interrupt 35 External Interrupt 1
procedure PORTE_INT0_ISR; external name 'PORTE_INT0_ISR'; // Interrupt 43 External Interrupt 0
procedure PORTE_INT1_ISR; external name 'PORTE_INT1_ISR'; // Interrupt 44 External Interrupt 1
procedure TWIE_TWIS_ISR; external name 'TWIE_TWIS_ISR'; // Interrupt 45 TWI Slave Interrupt
procedure TWIE_TWIM_ISR; external name 'TWIE_TWIM_ISR'; // Interrupt 46 TWI Master Interrupt
procedure TCE0_OVF_ISR; external name 'TCE0_OVF_ISR'; // Interrupt 47 Overflow Interrupt
procedure TCE0_ERR_ISR; external name 'TCE0_ERR_ISR'; // Interrupt 48 Error Interrupt
procedure TCE0_CCA_ISR; external name 'TCE0_CCA_ISR'; // Interrupt 49 Compare or Capture A Interrupt
procedure TCE0_CCB_ISR; external name 'TCE0_CCB_ISR'; // Interrupt 50 Compare or Capture B Interrupt
procedure TCE0_CCC_ISR; external name 'TCE0_CCC_ISR'; // Interrupt 51 Compare or Capture C Interrupt
procedure TCE0_CCD_ISR; external name 'TCE0_CCD_ISR'; // Interrupt 52 Compare or Capture D Interrupt
procedure PORTD_INT0_ISR; external name 'PORTD_INT0_ISR'; // Interrupt 64 External Interrupt 0
procedure PORTD_INT1_ISR; external name 'PORTD_INT1_ISR'; // Interrupt 65 External Interrupt 1
procedure PORTA_INT0_ISR; external name 'PORTA_INT0_ISR'; // Interrupt 66 External Interrupt 0
procedure PORTA_INT1_ISR; external name 'PORTA_INT1_ISR'; // Interrupt 67 External Interrupt 1
procedure ACA_AC0_ISR; external name 'ACA_AC0_ISR'; // Interrupt 68 AC0 Interrupt
procedure ACA_AC1_ISR; external name 'ACA_AC1_ISR'; // Interrupt 69 AC1 Interrupt
procedure ACA_ACW_ISR; external name 'ACA_ACW_ISR'; // Interrupt 70 ACW Window Mode Interrupt
procedure ADCA_CH0_ISR; external name 'ADCA_CH0_ISR'; // Interrupt 71 Interrupt 0
procedure ADCA_CH1_ISR; external name 'ADCA_CH1_ISR'; // Interrupt 72 Interrupt 1
procedure ADCA_CH2_ISR; external name 'ADCA_CH2_ISR'; // Interrupt 73 Interrupt 2
procedure ADCA_CH3_ISR; external name 'ADCA_CH3_ISR'; // Interrupt 74 Interrupt 3
procedure TCD0_OVF_ISR; external name 'TCD0_OVF_ISR'; // Interrupt 77 Overflow Interrupt
procedure TCD0_ERR_ISR; external name 'TCD0_ERR_ISR'; // Interrupt 78 Error Interrupt
procedure TCD0_CCA_ISR; external name 'TCD0_CCA_ISR'; // Interrupt 79 Compare or Capture A Interrupt
procedure TCD0_CCB_ISR; external name 'TCD0_CCB_ISR'; // Interrupt 80 Compare or Capture B Interrupt
procedure TCD0_CCC_ISR; external name 'TCD0_CCC_ISR'; // Interrupt 81 Compare or Capture C Interrupt
procedure TCD0_CCD_ISR; external name 'TCD0_CCD_ISR'; // Interrupt 82 Compare or Capture D Interrupt
procedure SPID_INT_ISR; external name 'SPID_INT_ISR'; // Interrupt 87 SPI Interrupt
procedure USARTD0_RXC_ISR; external name 'USARTD0_RXC_ISR'; // Interrupt 88 Reception Complete Interrupt
procedure USARTD0_DRE_ISR; external name 'USARTD0_DRE_ISR'; // Interrupt 89 Data Register Empty Interrupt
procedure USARTD0_TXC_ISR; external name 'USARTD0_TXC_ISR'; // Interrupt 90 Transmission Complete Interrupt

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
asm
  jmp __dtors_end
  jmp OSC_OSCF_ISR
  jmp PORTC_INT0_ISR
  jmp PORTC_INT1_ISR
  jmp PORTR_INT0_ISR
  jmp PORTR_INT1_ISR
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
  jmp NVM_EE_ISR
  jmp NVM_SPM_ISR
  jmp PORTB_INT0_ISR
  jmp PORTB_INT1_ISR
  jmp PORTE_INT0_ISR
  jmp PORTE_INT1_ISR
  jmp TWIE_TWIS_ISR
  jmp TWIE_TWIM_ISR
  jmp TCE0_OVF_ISR
  jmp TCE0_ERR_ISR
  jmp TCE0_CCA_ISR
  jmp TCE0_CCB_ISR
  jmp TCE0_CCC_ISR
  jmp TCE0_CCD_ISR
  jmp PORTD_INT0_ISR
  jmp PORTD_INT1_ISR
  jmp PORTA_INT0_ISR
  jmp PORTA_INT1_ISR
  jmp ACA_AC0_ISR
  jmp ACA_AC1_ISR
  jmp ACA_ACW_ISR
  jmp ADCA_CH0_ISR
  jmp ADCA_CH1_ISR
  jmp ADCA_CH2_ISR
  jmp ADCA_CH3_ISR
  jmp TCD0_OVF_ISR
  jmp TCD0_ERR_ISR
  jmp TCD0_CCA_ISR
  jmp TCD0_CCB_ISR
  jmp TCD0_CCC_ISR
  jmp TCD0_CCD_ISR
  jmp SPID_INT_ISR
  jmp USARTD0_RXC_ISR
  jmp USARTD0_DRE_ISR
  jmp USARTD0_TXC_ISR

  .weak OSC_OSCF_ISR
  .weak PORTC_INT0_ISR
  .weak PORTC_INT1_ISR
  .weak PORTR_INT0_ISR
  .weak PORTR_INT1_ISR
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
  .weak NVM_EE_ISR
  .weak NVM_SPM_ISR
  .weak PORTB_INT0_ISR
  .weak PORTB_INT1_ISR
  .weak PORTE_INT0_ISR
  .weak PORTE_INT1_ISR
  .weak TWIE_TWIS_ISR
  .weak TWIE_TWIM_ISR
  .weak TCE0_OVF_ISR
  .weak TCE0_ERR_ISR
  .weak TCE0_CCA_ISR
  .weak TCE0_CCB_ISR
  .weak TCE0_CCC_ISR
  .weak TCE0_CCD_ISR
  .weak PORTD_INT0_ISR
  .weak PORTD_INT1_ISR
  .weak PORTA_INT0_ISR
  .weak PORTA_INT1_ISR
  .weak ACA_AC0_ISR
  .weak ACA_AC1_ISR
  .weak ACA_ACW_ISR
  .weak ADCA_CH0_ISR
  .weak ADCA_CH1_ISR
  .weak ADCA_CH2_ISR
  .weak ADCA_CH3_ISR
  .weak TCD0_OVF_ISR
  .weak TCD0_ERR_ISR
  .weak TCD0_CCA_ISR
  .weak TCD0_CCB_ISR
  .weak TCD0_CCC_ISR
  .weak TCD0_CCD_ISR
  .weak SPID_INT_ISR
  .weak USARTD0_RXC_ISR
  .weak USARTD0_DRE_ISR
  .weak USARTD0_TXC_ISR

  .set OSC_OSCF_ISR, Default_IRQ_handler
  .set PORTC_INT0_ISR, Default_IRQ_handler
  .set PORTC_INT1_ISR, Default_IRQ_handler
  .set PORTR_INT0_ISR, Default_IRQ_handler
  .set PORTR_INT1_ISR, Default_IRQ_handler
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
  .set NVM_EE_ISR, Default_IRQ_handler
  .set NVM_SPM_ISR, Default_IRQ_handler
  .set PORTB_INT0_ISR, Default_IRQ_handler
  .set PORTB_INT1_ISR, Default_IRQ_handler
  .set PORTE_INT0_ISR, Default_IRQ_handler
  .set PORTE_INT1_ISR, Default_IRQ_handler
  .set TWIE_TWIS_ISR, Default_IRQ_handler
  .set TWIE_TWIM_ISR, Default_IRQ_handler
  .set TCE0_OVF_ISR, Default_IRQ_handler
  .set TCE0_ERR_ISR, Default_IRQ_handler
  .set TCE0_CCA_ISR, Default_IRQ_handler
  .set TCE0_CCB_ISR, Default_IRQ_handler
  .set TCE0_CCC_ISR, Default_IRQ_handler
  .set TCE0_CCD_ISR, Default_IRQ_handler
  .set PORTD_INT0_ISR, Default_IRQ_handler
  .set PORTD_INT1_ISR, Default_IRQ_handler
  .set PORTA_INT0_ISR, Default_IRQ_handler
  .set PORTA_INT1_ISR, Default_IRQ_handler
  .set ACA_AC0_ISR, Default_IRQ_handler
  .set ACA_AC1_ISR, Default_IRQ_handler
  .set ACA_ACW_ISR, Default_IRQ_handler
  .set ADCA_CH0_ISR, Default_IRQ_handler
  .set ADCA_CH1_ISR, Default_IRQ_handler
  .set ADCA_CH2_ISR, Default_IRQ_handler
  .set ADCA_CH3_ISR, Default_IRQ_handler
  .set TCD0_OVF_ISR, Default_IRQ_handler
  .set TCD0_ERR_ISR, Default_IRQ_handler
  .set TCD0_CCA_ISR, Default_IRQ_handler
  .set TCD0_CCB_ISR, Default_IRQ_handler
  .set TCD0_CCC_ISR, Default_IRQ_handler
  .set TCD0_CCD_ISR, Default_IRQ_handler
  .set SPID_INT_ISR, Default_IRQ_handler
  .set USARTD0_RXC_ISR, Default_IRQ_handler
  .set USARTD0_DRE_ISR, Default_IRQ_handler
  .set USARTD0_TXC_ISR, Default_IRQ_handler
end;

end.

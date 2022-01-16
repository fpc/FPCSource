unit AT90USB82;

{$goto on}

interface

var
  // PORTB
  PORTB : byte absolute $00+$25; // Port B Data Register
  DDRB : byte absolute $00+$24; // Port B Data Direction Register
  PINB : byte absolute $00+$23; // Port B Input Pins
  // PORTD
  PORTD : byte absolute $00+$2B; // Port D Data Register
  DDRD : byte absolute $00+$2A; // Port D Data Direction Register
  PIND : byte absolute $00+$29; // Port D Input Pins
  // SPI
  SPCR : byte absolute $00+$4C; // SPI Control Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPDR : byte absolute $00+$4E; // SPI Data Register
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Address Register Low Bytes
  EEARL : byte absolute $00+$41; // EEPROM Address Register Low Bytes
  EEARH : byte absolute $00+$41+1; // EEPROM Address Register Low Bytes
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // TIMER_COUNTER_0
  OCR0B : byte absolute $00+$48; // Timer/Counter0 Output Compare Register
  OCR0A : byte absolute $00+$47; // Timer/Counter0 Output Compare Register
  TCNT0 : byte absolute $00+$46; // Timer/Counter0
  TCCR0B : byte absolute $00+$45; // Timer/Counter Control Register B
  TCCR0A : byte absolute $00+$44; // Timer/Counter  Control Register A
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag register
  GTCCR : byte absolute $00+$43; // General Timer/Counter Control Register
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$80; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$81; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$82; // Timer/Counter 1 Control Register C
  TCNT1 : word absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$84+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AL : byte absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AH : byte absolute $00+$88+1; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1B : word absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BL : byte absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BH : byte absolute $00+$8A+1; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1C : word absolute $00+$8C; // Timer/Counter1 Output Compare Register C  Bytes
  OCR1CL : byte absolute $00+$8C; // Timer/Counter1 Output Compare Register C  Bytes
  OCR1CH : byte absolute $00+$8C+1; // Timer/Counter1 Output Compare Register C  Bytes
  ICR1 : word absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$86+1; // Timer/Counter1 Input Capture Register  Bytes
  TIMSK1 : byte absolute $00+$6F; // Timer/Counter1 Interrupt Mask Register
  TIFR1 : byte absolute $00+$36; // Timer/Counter1 Interrupt Flag register
  // PLL
  PLLCSR : byte absolute $00+$49; // PLL Status and Control register
  // USB_DEVICE
  UEINT : byte absolute $00+$F4; // 
  UEBCLX : byte absolute $00+$F2; // 
  UEDATX : byte absolute $00+$F1; // 
  UEIENX : byte absolute $00+$F0; // 
  UESTA1X : byte absolute $00+$EF; // 
  UESTA0X : byte absolute $00+$EE; // 
  UECFG1X : byte absolute $00+$ED; // 
  UECFG0X : byte absolute $00+$EC; // 
  UECONX : byte absolute $00+$EB; // 
  UERST : byte absolute $00+$EA; // 
  UENUM : byte absolute $00+$E9; // 
  UEINTX : byte absolute $00+$E8; // 
  UDMFN : byte absolute $00+$E6; // 
  UDFNUM : word absolute $00+$E4; // 
  UDFNUML : byte absolute $00+$E4; // 
  UDFNUMH : byte absolute $00+$E4+1; // 
  UDADDR : byte absolute $00+$E3; // 
  UDIEN : byte absolute $00+$E2; // 
  UDINT : byte absolute $00+$E1; // 
  UDCON : byte absolute $00+$E0; // 
  USBCON : byte absolute $00+$D8; // USB General Control Register
  REGCR : byte absolute $00+$63; // Regulator Control Register
  // PS2
  UPOE : byte absolute $00+$FB; // 
  PS2CON : byte absolute $00+$FA; // PS2 Pad Enable register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // 
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  EIND : byte absolute $00+$5C; // Extended Indirect Register
  GPIOR2 : byte absolute $00+$4B; // General Purpose IO Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose IO Register 0
  PRR1 : byte absolute $00+$65; // Power Reduction Register1
  PRR0 : byte absolute $00+$64; // Power Reduction Register0
  CLKSTA : byte absolute $00+$D2; // 
  CLKSEL1 : byte absolute $00+$D1; // 
  CLKSEL0 : byte absolute $00+$D0; // 
  DWDR : byte absolute $00+$51; // debugWire communication register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register A
  EICRB : byte absolute $00+$6A; // External Interrupt Control Register B
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCMSK0 : byte absolute $00+$6B; // Pin Change Mask Register 0
  PCMSK1 : byte absolute $00+$6C; // Pin Change Mask Register 1
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  // USART1
  UDR1 : byte absolute $00+$CE; // USART I/O Data Register
  UCSR1A : byte absolute $00+$C8; // USART Control and Status Register A
  UCSR1B : byte absolute $00+$C9; // USART Control and Status Register B
  UCSR1C : byte absolute $00+$CA; // USART Control and Status Register C
  UCSR1D : byte absolute $00+$CB; // USART Control and Status Register D
  UBRR1 : word absolute $00+$CC; // USART Baud Rate Register  Bytes
  UBRR1L : byte absolute $00+$CC; // USART Baud Rate Register  Bytes
  UBRR1H : byte absolute $00+$CC+1; // USART Baud Rate Register  Bytes
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  WDTCKD : byte absolute $00+$62; // Watchdog Timer Clock Divider
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$50; // Analog Comparator Control And Status Register
  DIDR1 : byte absolute $00+$7F; // 
  // PORTC
  PORTC : byte absolute $00+$28; // Port C Data Register
  DDRC : byte absolute $00+$27; // Port C Data Direction Register
  PINC : byte absolute $00+$26; // Port C Input Pins

const
  // SPCR
  SPIE = 7; // SPI Interrupt Enable
  SPE = 6; // SPI Enable
  DORD = 5; // Data Order
  MSTR = 4; // Master/Slave Select
  CPOL = 3; // Clock polarity
  CPHA = 2; // Clock Phase
  SPR = 0; // SPI Clock Rate Selects
  // SPSR
  SPIF = 7; // SPI Interrupt Flag
  WCOL = 6; // Write Collision Flag
  SPI2X = 0; // Double SPI Speed Bit
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  SIGRD = 5; // Signature Row Read
  RWWSRE = 4; // Read While Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  FOC0B = 6; // Force Output Compare B
  WGM02 = 3; // 
  CS0 = 0; // Clock Select
  // TCCR0A
  COM0A = 6; // Compare Output Mode, Phase Correct PWM Mode
  COM0B = 4; // Compare Output Mode, Fast PWm
  WGM0 = 0; // Waveform Generation Mode
  // TIMSK0
  OCIE0B = 2; // Timer/Counter0 Output Compare Match B Interrupt Enable
  OCIE0A = 1; // Timer/Counter0 Output Compare Match A Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0B = 2; // Timer/Counter0 Output Compare Flag 0B
  OCF0A = 1; // Timer/Counter0 Output Compare Flag 0A
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSRSYNC = 0; // Prescaler Reset Timer/Counter1 and Timer/Counter0
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  COM1C = 2; // Compare Output Mode 1C, bits
  WGM1 = 0; // Waveform Generation Mode
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Prescaler source of Timer/Counter 1
  // TCCR1C
  FOC1A = 7; // Force Output Compare 1A
  FOC1B = 6; // Force Output Compare 1B
  FOC1C = 5; // Force Output Compare 1C
  // TIMSK1
  ICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  OCIE1C = 3; // Timer/Counter1 Output Compare C Match Interrupt Enable
  OCIE1B = 2; // Timer/Counter1 Output Compare B Match Interrupt Enable
  OCIE1A = 1; // Timer/Counter1 Output Compare A Match Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 5; // Input Capture Flag 1
  OCF1C = 3; // Output Compare Flag 1C
  OCF1B = 2; // Output Compare Flag 1B
  OCF1A = 1; // Output Compare Flag 1A
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // PLLCSR
  PLLP = 2; // PLL prescaler Bits
  PLLE = 1; // PLL Enable Bit
  PLOCK = 0; // PLL Lock Status Bit
  // UEIENX
  FLERRE = 7; // 
  NAKINE = 6; // 
  NAKOUTE = 4; // 
  RXSTPE = 3; // 
  RXOUTE = 2; // 
  STALLEDE = 1; // 
  TXINE = 0; // 
  // UESTA1X
  CTRLDIR = 2; // 
  CURRBK = 0; // 
  // UESTA0X
  CFGOK = 7; // 
  OVERFI = 6; // 
  UNDERFI = 5; // 
  DTSEQ = 2; // 
  NBUSYBK = 0; // 
  // UECFG1X
  EPSIZE = 4; // 
  EPBK = 2; // 
  ALLOC = 1; // 
  // UECFG0X
  EPTYPE = 6; // 
  EPDIR = 0; // 
  // UECONX
  STALLRQ = 5; // 
  STALLRQC = 4; // 
  RSTDT = 3; // 
  EPEN = 0; // 
  // UERST
  EPRST = 0; // 
  // UEINTX
  FIFOCON = 7; // 
  NAKINI = 6; // 
  RWAL = 5; // 
  NAKOUTI = 4; // 
  RXSTPI = 3; // 
  RXOUTI = 2; // 
  STALLEDI = 1; // 
  TXINI = 0; // 
  // UDMFN
  FNCERR = 4; // 
  // UDADDR
  ADDEN = 7; // 
  UADD = 0; // 
  // UDIEN
  UPRSME = 6; // 
  EORSME = 5; // 
  WAKEUPE = 4; // 
  EORSTE = 3; // 
  SOFE = 2; // 
  SUSPE = 0; // 
  // UDINT
  UPRSMI = 6; // 
  EORSMI = 5; // 
  WAKEUPI = 4; // 
  EORSTI = 3; // 
  SOFI = 2; // 
  SUSPI = 0; // 
  // UDCON
  RSTCPU = 2; // 
  RMWKUP = 1; // 
  DETACH = 0; // 
  // USBCON
  USBE = 7; // 
  FRZCLK = 5; // 
  // REGCR
  REGDIS = 0; // 
  // UPOE
  UPWE = 6; // 
  UPDRV = 4; // 
  SCKI = 3; // 
  DATAI = 2; // 
  DPI = 1; // 
  DMI = 0; // 
  // PS2CON
  PS2EN = 0; // Enable
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // MCUCR
  PUD = 4; // Pull-up disable
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUSR
  USBRF = 5; // USB reset flag
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // CLKPR
  CLKPCE = 7; // 
  CLKPS = 0; // 
  // SMCR
  SM = 1; // Sleep Mode Select bits
  SE = 0; // Sleep Enable
  // GPIOR2
  GPIOR = 0; // General Purpose IO Register 2 bis
  // GPIOR1
  // GPIOR0
  GPIOR07 = 7; // General Purpose IO Register 0 bit 7
  GPIOR06 = 6; // General Purpose IO Register 0 bit 6
  GPIOR05 = 5; // General Purpose IO Register 0 bit 5
  GPIOR04 = 4; // General Purpose IO Register 0 bit 4
  GPIOR03 = 3; // General Purpose IO Register 0 bit 3
  GPIOR02 = 2; // General Purpose IO Register 0 bit 2
  GPIOR01 = 1; // General Purpose IO Register 0 bit 1
  GPIOR00 = 0; // General Purpose IO Register 0 bit 0
  // PRR1
  PRUSB = 7; // Power Reduction USB
  PRUSART1 = 0; // Power Reduction USART1
  // PRR0
  PRTIM0 = 5; // Power Reduction Timer/Counter0
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  // CLKSTA
  RCON = 1; // 
  EXTON = 0; // 
  // CLKSEL1
  RCCKSEL = 4; // 
  EXCKSEL = 0; // 
  // CLKSEL0
  RCSUT = 6; // 
  EXSUT = 4; // 
  RCE = 3; // 
  EXTE = 2; // 
  CLKS = 0; // 
  // EICRA
  ISC3 = 6; // External Interrupt Sense Control Bit
  ISC2 = 4; // External Interrupt Sense Control Bit
  ISC1 = 2; // External Interrupt Sense Control Bit
  ISC0 = 0; // External Interrupt Sense Control Bit
  // EICRB
  ISC7 = 6; // External Interrupt 7-4 Sense Control Bit
  ISC6 = 4; // External Interrupt 7-4 Sense Control Bit
  ISC5 = 2; // External Interrupt 7-4 Sense Control Bit
  ISC4 = 0; // External Interrupt 7-4 Sense Control Bit
  // EIMSK
  INT = 0; // External Interrupt Request 7 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // PCMSK0
  PCINT = 0; // Pin Change Enable Masks
  // PCMSK1
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enables
  // UCSR1A
  RXC1 = 7; // USART Receive Complete
  TXC1 = 6; // USART Transmitt Complete
  UDRE1 = 5; // USART Data Register Empty
  FE1 = 4; // Framing Error
  DOR1 = 3; // Data overRun
  UPE1 = 2; // Parity Error
  U2X1 = 1; // Double the USART transmission speed
  MPCM1 = 0; // Multi-processor Communication Mode
  // UCSR1B
  RXCIE1 = 7; // RX Complete Interrupt Enable
  TXCIE1 = 6; // TX Complete Interrupt Enable
  UDRIE1 = 5; // USART Data register Empty Interrupt Enable
  RXEN1 = 4; // Receiver Enable
  TXEN1 = 3; // Transmitter Enable
  UCSZ12 = 2; // Character Size
  RXB81 = 1; // Receive Data Bit 8
  TXB81 = 0; // Transmit Data Bit 8
  // UCSR1C
  UMSEL1 = 6; // USART Mode Select
  UPM1 = 4; // Parity Mode Bits
  USBS1 = 3; // Stop Bit Select
  UCSZ1 = 1; // Character Size
  UCPOL1 = 0; // Clock Polarity
  // UCSR1D
  CTSEN = 1; // CTS Enable
  RTSEN = 0; // RTS Enable
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // WDTCKD
  WDEWIF = 3; // Watchdog Early Warning Interrupt Flag
  WDEWIE = 2; // Watchdog Early Warning Interrupt Enable
  WCLKD = 0; // Watchdog Timer Clock Dividers
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // DIDR1
  AIN1D = 1; // AIN1 Digital Input Disable
  AIN0D = 0; // AIN0 Digital Input Disable
  // PORTC
  // DDRC
  DDC = 4; // Port C Data Direction Register bits
  // PINC

implementation

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 2 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 4 External Interrupt Request 1
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 6 External Interrupt Request 2
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 8 External Interrupt Request 3
procedure INT4_ISR; external name 'INT4_ISR'; // Interrupt 10 External Interrupt Request 4
procedure INT5_ISR; external name 'INT5_ISR'; // Interrupt 12 External Interrupt Request 5
procedure INT6_ISR; external name 'INT6_ISR'; // Interrupt 14 External Interrupt Request 6
procedure INT7_ISR; external name 'INT7_ISR'; // Interrupt 16 External Interrupt Request 7
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 18 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 20 Pin Change Interrupt Request 1
procedure USB_GEN_ISR; external name 'USB_GEN_ISR'; // Interrupt 22 USB General Interrupt Request
procedure USB_COM_ISR; external name 'USB_COM_ISR'; // Interrupt 24 USB Endpoint/Pipe Interrupt Communication Request
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 26 Watchdog Time-out Interrupt
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 28 Timer/Counter2 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 30 Timer/Counter2 Compare Match B
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 32 Timer/Counter2 Compare Match B
procedure TIMER1_COMPC_ISR; external name 'TIMER1_COMPC_ISR'; // Interrupt 34 Timer/Counter2 Compare Match C
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 36 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 38 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 40 Timer/Counter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 42 Timer/Counter0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 44 SPI Serial Transfer Complete
procedure USART1__RX_ISR; external name 'USART1__RX_ISR'; // Interrupt 46 USART1, Rx Complete
procedure USART1__UDRE_ISR; external name 'USART1__UDRE_ISR'; // Interrupt 48 USART1 Data register Empty
procedure USART1__TX_ISR; external name 'USART1__TX_ISR'; // Interrupt 50 USART1, Tx Complete
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 52 Analog Comparator
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 54 EEPROM Ready
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 56 Store Program Memory Read

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   jmp _start
   jmp INT0_ISR
   jmp INT1_ISR
   jmp INT2_ISR
   jmp INT3_ISR
   jmp INT4_ISR
   jmp INT5_ISR
   jmp INT6_ISR
   jmp INT7_ISR
   jmp PCINT0_ISR
   jmp PCINT1_ISR
   jmp USB_GEN_ISR
   jmp USB_COM_ISR
   jmp WDT_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_COMPC_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMPA_ISR
   jmp TIMER0_COMPB_ISR
   jmp TIMER0_OVF_ISR
   jmp SPI__STC_ISR
   jmp USART1__RX_ISR
   jmp USART1__UDRE_ISR
   jmp USART1__TX_ISR
   jmp ANALOG_COMP_ISR
   jmp EE_READY_ISR
   jmp SPM_READY_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak INT2_ISR
   .weak INT3_ISR
   .weak INT4_ISR
   .weak INT5_ISR
   .weak INT6_ISR
   .weak INT7_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak USB_GEN_ISR
   .weak USB_COM_ISR
   .weak WDT_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_COMPC_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI__STC_ISR
   .weak USART1__RX_ISR
   .weak USART1__UDRE_ISR
   .weak USART1__TX_ISR
   .weak ANALOG_COMP_ISR
   .weak EE_READY_ISR
   .weak SPM_READY_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set INT3_ISR, Default_IRQ_handler
   .set INT4_ISR, Default_IRQ_handler
   .set INT5_ISR, Default_IRQ_handler
   .set INT6_ISR, Default_IRQ_handler
   .set INT7_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set USB_GEN_ISR, Default_IRQ_handler
   .set USB_COM_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_COMPC_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART1__RX_ISR, Default_IRQ_handler
   .set USART1__UDRE_ISR, Default_IRQ_handler
   .set USART1__TX_ISR, Default_IRQ_handler
   .set ANALOG_COMP_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
 end;

end.

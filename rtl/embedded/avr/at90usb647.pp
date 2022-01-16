unit AT90USB647;

{$goto on}

interface

var
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  // PORTA
  PORTA : byte absolute $00+$22; // Port A Data Register
  DDRA : byte absolute $00+$21; // Port A Data Direction Register
  PINA : byte absolute $00+$20; // Port A Input Pins
  // PORTB
  PORTB : byte absolute $00+$25; // Port B Data Register
  DDRB : byte absolute $00+$24; // Port B Data Direction Register
  PINB : byte absolute $00+$23; // Port B Input Pins
  // PORTC
  PORTC : byte absolute $00+$28; // Port C Data Register
  DDRC : byte absolute $00+$27; // Port C Data Direction Register
  PINC : byte absolute $00+$26; // Port C Input Pins
  // PORTD
  PORTD : byte absolute $00+$2B; // Port D Data Register
  DDRD : byte absolute $00+$2A; // Port D Data Direction Register
  PIND : byte absolute $00+$29; // Port D Input Pins
  // PORTE
  PORTE : byte absolute $00+$2E; // Data Register, Port E
  DDRE : byte absolute $00+$2D; // Data Direction Register, Port E
  PINE : byte absolute $00+$2C; // Input Pins, Port E
  // PORTF
  PORTF : byte absolute $00+$31; // Data Register, Port F
  DDRF : byte absolute $00+$30; // Data Direction Register, Port F
  PINF : byte absolute $00+$2F; // Input Pins, Port F
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  XMCRA : byte absolute $00+$74; // External Memory Control Register A
  XMCRB : byte absolute $00+$75; // External Memory Control Register B
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // 
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  EIND : byte absolute $00+$5C; // Extended Indirect Register
  RAMPZ : byte absolute $00+$5B; // RAM Page Z Select Register
  GPIOR2 : byte absolute $00+$4B; // General Purpose IO Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose IO Register 0
  PRR1 : byte absolute $00+$65; // Power Reduction Register1
  PRR0 : byte absolute $00+$64; // Power Reduction Register0
  // TWI
  TWAMR : byte absolute $00+$BD; // TWI (Slave) Address Mask Register
  TWBR : byte absolute $00+$B8; // TWI Bit Rate register
  TWCR : byte absolute $00+$BC; // TWI Control Register
  TWSR : byte absolute $00+$B9; // TWI Status Register
  TWDR : byte absolute $00+$BB; // TWI Data register
  TWAR : byte absolute $00+$BA; // TWI (Slave) Address register
  // SPI
  SPCR : byte absolute $00+$4C; // SPI Control Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPDR : byte absolute $00+$4E; // SPI Data Register
  // USART1
  UDR1 : byte absolute $00+$CE; // USART I/O Data Register
  UCSR1A : byte absolute $00+$C8; // USART Control and Status Register A
  UCSR1B : byte absolute $00+$C9; // USART Control and Status Register B
  UCSR1C : byte absolute $00+$CA; // USART Control and Status Register C
  UBRR1 : word absolute $00+$CC; // USART Baud Rate Register  Bytes
  UBRR1L : byte absolute $00+$CC; // USART Baud Rate Register  Bytes
  UBRR1H : byte absolute $00+$CC+1; // USART Baud Rate Register  Bytes
  // USB_DEVICE
  UEINT : byte absolute $00+$F4; // 
  UEBCHX : byte absolute $00+$F3; // 
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
  // USB_GLOBAL
  OTGINT : byte absolute $00+$DF; // 
  OTGIEN : byte absolute $00+$DE; // 
  OTGCON : byte absolute $00+$DD; // 
  OTGTCON : byte absolute $00+$F9; // 
  USBINT : byte absolute $00+$DA; // 
  USBSTA : byte absolute $00+$D9; // 
  USBCON : byte absolute $00+$D8; // USB General Control Register
  UHWCON : byte absolute $00+$D7; // USB Hardware Configuration Register
  // USB_HOST
  UPERRX : byte absolute $00+$F5; // 
  UPINT : byte absolute $00+$F8; // 
  UPBCHX : byte absolute $00+$F7; // 
  UPBCLX : byte absolute $00+$F6; // 
  UPDATX : byte absolute $00+$AF; // 
  UPIENX : byte absolute $00+$AE; // 
  UPCFG2X : byte absolute $00+$AD; // 
  UPSTAX : byte absolute $00+$AC; // 
  UPCFG1X : byte absolute $00+$AB; // 
  UPCFG0X : byte absolute $00+$AA; // 
  UPCONX : byte absolute $00+$A9; // 
  UPRST : byte absolute $00+$A8; // 
  UPNUM : byte absolute $00+$A7; // 
  UPINTX : byte absolute $00+$A6; // 
  UPINRQX : byte absolute $00+$A5; // 
  UHFLEN : byte absolute $00+$A4; // 
  UHFNUM : word absolute $00+$A2; // 
  UHFNUML : byte absolute $00+$A2; // 
  UHFNUMH : byte absolute $00+$A2+1; // 
  UHADDR : byte absolute $00+$A1; // 
  UHIEN : byte absolute $00+$A0; // 
  UHINT : byte absolute $00+$9F; // 
  UHCON : byte absolute $00+$9E; // 
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
  // TIMER_COUNTER_2
  TIMSK2 : byte absolute $00+$70; // Timer/Counter Interrupt Mask register
  TIFR2 : byte absolute $00+$37; // Timer/Counter Interrupt Flag Register
  TCCR2A : byte absolute $00+$B0; // Timer/Counter2 Control Register A
  TCCR2B : byte absolute $00+$B1; // Timer/Counter2 Control Register B
  TCNT2 : byte absolute $00+$B2; // Timer/Counter2
  OCR2B : byte absolute $00+$B4; // Timer/Counter2 Output Compare Register B
  OCR2A : byte absolute $00+$B3; // Timer/Counter2 Output Compare Register A
  ASSR : byte absolute $00+$B6; // Asynchronous Status Register
  // TIMER_COUNTER_3
  TCCR3A : byte absolute $00+$90; // Timer/Counter3 Control Register A
  TCCR3B : byte absolute $00+$91; // Timer/Counter3 Control Register B
  TCCR3C : byte absolute $00+$92; // Timer/Counter 3 Control Register C
  TCNT3 : word absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3L : byte absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3H : byte absolute $00+$94+1; // Timer/Counter3  Bytes
  OCR3A : word absolute $00+$98; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AL : byte absolute $00+$98; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AH : byte absolute $00+$98+1; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3B : word absolute $00+$9A; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BL : byte absolute $00+$9A; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BH : byte absolute $00+$9A+1; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3C : word absolute $00+$9C; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3CL : byte absolute $00+$9C; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3CH : byte absolute $00+$9C+1; // Timer/Counter3 Output Compare Register B  Bytes
  ICR3 : word absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3L : byte absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3H : byte absolute $00+$96+1; // Timer/Counter3 Input Capture Register  Bytes
  TIMSK3 : byte absolute $00+$71; // Timer/Counter3 Interrupt Mask Register
  TIFR3 : byte absolute $00+$38; // Timer/Counter3 Interrupt Flag register
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
  // JTAG
  OCDR : byte absolute $00+$51; // On-Chip Debug Related Register in I/O Memory
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register A
  EICRB : byte absolute $00+$6A; // External Interrupt Control Register B
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCMSK0 : byte absolute $00+$6B; // Pin Change Mask Register 0
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$7C; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$7B; // ADC Control and Status Register B
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register 1
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$50; // Analog Comparator Control And Status Register
  DIDR1 : byte absolute $00+$7F; // 
  // PLL
  PLLCSR : byte absolute $00+$49; // PLL Status and Control register

const
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
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
  JTD = 7; // JTAG Interface Disable
  PUD = 4; // Pull-up disable
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUSR
  JTRF = 4; // JTAG Reset Flag
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // XMCRA
  SRE = 7; // External SRAM Enable
  SRL = 4; // Wait state page limit
  SRW1 = 2; // Wait state select bit upper page
  SRW0 = 0; // Wait state select bit lower page
  // XMCRB
  XMBK = 7; // External Memory Bus Keeper Enable
  XMM = 0; // External Memory High Mask
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
  PRTIM3 = 3; // Power Reduction Timer/Counter3
  PRUSART1 = 0; // Power Reduction USART1
  // PRR0
  PRTWI = 7; // Power Reduction TWI
  PRTIM2 = 6; // Power Reduction Timer/Counter2
  PRTIM0 = 5; // Power Reduction Timer/Counter0
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  PRADC = 0; // Power Reduction ADC
  // TWAMR
  TWAM = 1; // 
  // TWCR
  TWINT = 7; // TWI Interrupt Flag
  TWEA = 6; // TWI Enable Acknowledge Bit
  TWSTA = 5; // TWI Start Condition Bit
  TWSTO = 4; // TWI Stop Condition Bit
  TWWC = 3; // TWI Write Collition Flag
  TWEN = 2; // TWI Enable Bit
  TWIE = 0; // TWI Interrupt Enable
  // TWSR
  TWS = 3; // TWI Status
  TWPS = 0; // TWI Prescaler
  // TWAR
  TWA = 1; // TWI (Slave) Address register Bits
  TWGCE = 0; // TWI General Call Recognition Enable Bit
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
  LSM = 2; // 
  RMWKUP = 1; // 
  DETACH = 0; // 
  // OTGINT
  STOI = 5; // 
  HNPERRI = 4; // 
  ROLEEXI = 3; // 
  BCERRI = 2; // 
  VBERRI = 1; // 
  SRPI = 0; // 
  // OTGIEN
  STOE = 5; // 
  HNPERRE = 4; // 
  ROLEEXE = 3; // 
  BCERRE = 2; // 
  VBERRE = 1; // 
  SRPE = 0; // 
  // OTGCON
  HNPREQ = 5; // 
  SRPREQ = 4; // 
  SRPSEL = 3; // 
  VBUSHWC = 2; // 
  VBUSREQ = 1; // 
  VBUSRQC = 0; // 
  // OTGTCON
  OTGTCON_7 = 7; // 
  PAGE = 5; // 
  VALUE_2 = 0; // 
  // USBINT
  IDTI = 1; // 
  VBUSTI = 0; // 
  // USBSTA
  SPEED = 3; // 
  ID = 1; // 
  VBUS = 0; // 
  // USBCON
  USBE = 7; // 
  HOST = 6; // 
  FRZCLK = 5; // 
  OTGPADE = 4; // 
  IDTE = 1; // 
  VBUSTE = 0; // 
  // UHWCON
  UIMOD = 7; // 
  UIDE = 6; // 
  UVCONE = 4; // 
  UVREGE = 0; // 
  // UPERRX
  COUNTER = 5; // 
  CRC16 = 4; // 
  TIMEOUT = 3; // 
  PID = 2; // 
  DATAPID = 1; // 
  DATATGL = 0; // 
  // UPIENX
  NAKEDE = 6; // 
  PERRE = 4; // 
  TXSTPE = 3; // 
  TXOUTE = 2; // 
  RXSTALLE = 1; // 
  RXINE = 0; // 
  // UPSTAX
  NBUSYK = 0; // 
  // UPCFG1X
  PSIZE = 4; // 
  PBK = 2; // 
  // UPCFG0X
  PTYPE = 6; // 
  PTOKEN = 4; // 
  PEPNUM = 0; // 
  // UPCONX
  PFREEZE = 6; // 
  INMODE = 5; // 
  PEN = 0; // 
  // UPRST
  PRST = 0; // 
  // UPINTX
  NAKEDI = 6; // 
  PERRI = 4; // 
  TXSTPI = 3; // 
  TXOUTI = 2; // 
  RXSTALLI = 1; // 
  RXINI = 0; // 
  // UHIEN
  HWUPE = 6; // 
  HSOFE = 5; // 
  RXRSME = 4; // 
  RSMEDE = 3; // 
  RSTE = 2; // 
  DDISCE = 1; // 
  DCONNE = 0; // 
  // UHINT
  UHUPI = 6; // 
  HSOFI = 5; // 
  RXRSMI = 4; // 
  RSMEDI = 3; // 
  RSTI = 2; // 
  DDISCI = 1; // 
  DCONNI = 0; // 
  // UHCON
  RESUME = 2; // 
  RESET = 1; // 
  SOFEN = 0; // 
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
  // TIMSK2
  OCIE2B = 2; // Timer/Counter2 Output Compare Match B Interrupt Enable
  OCIE2A = 1; // Timer/Counter2 Output Compare Match A Interrupt Enable
  TOIE2 = 0; // Timer/Counter2 Overflow Interrupt Enable
  // TIFR2
  OCF2B = 2; // Output Compare Flag 2B
  OCF2A = 1; // Output Compare Flag 2A
  TOV2 = 0; // Timer/Counter2 Overflow Flag
  // TCCR2A
  COM2A = 6; // Compare Output Mode bits
  COM2B = 4; // Compare Output Mode bits
  WGM2 = 0; // Waveform Genration Mode
  // TCCR2B
  FOC2A = 7; // Force Output Compare A
  FOC2B = 6; // Force Output Compare B
  WGM22 = 3; // Waveform Generation Mode
  CS2 = 0; // Clock Select bits
  // ASSR
  EXCLK = 6; // Enable External Clock Input
  AS2 = 5; // Asynchronous Timer/Counter2
  TCN2UB = 4; // Timer/Counter2 Update Busy
  OCR2AUB = 3; // Output Compare Register2 Update Busy
  OCR2BUB = 2; // Output Compare Register 2 Update Busy
  TCR2AUB = 1; // Timer/Counter Control Register2 Update Busy
  TCR2BUB = 0; // Timer/Counter Control Register2 Update Busy
  // GTCCR
  PSRASY = 1; // Prescaler Reset Timer/Counter2
  // TCCR3A
  COM3A = 6; // Compare Output Mode 1A, bits
  COM3B = 4; // Compare Output Mode 3B, bits
  COM3C = 2; // Compare Output Mode 3C, bits
  WGM3 = 0; // Waveform Generation Mode
  // TCCR3B
  ICNC3 = 7; // Input Capture 3 Noise Canceler
  ICES3 = 6; // Input Capture 3 Edge Select
  CS3 = 0; // Prescaler source of Timer/Counter 3
  // TCCR3C
  FOC3A = 7; // Force Output Compare 3A
  FOC3B = 6; // Force Output Compare 3B
  FOC3C = 5; // Force Output Compare 3C
  // TIMSK3
  ICIE3 = 5; // Timer/Counter3 Input Capture Interrupt Enable
  OCIE3C = 3; // Timer/Counter3 Output Compare C Match Interrupt Enable
  OCIE3B = 2; // Timer/Counter3 Output Compare B Match Interrupt Enable
  OCIE3A = 1; // Timer/Counter3 Output Compare A Match Interrupt Enable
  TOIE3 = 0; // Timer/Counter3 Overflow Interrupt Enable
  // TIFR3
  ICF3 = 5; // Input Capture Flag 3
  OCF3C = 3; // Output Compare Flag 3C
  OCF3B = 2; // Output Compare Flag 3B
  OCF3A = 1; // Output Compare Flag 3A
  TOV3 = 0; // Timer/Counter3 Overflow Flag
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
  // MCUCR
  // MCUSR
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
  // PCIFR
  PCIF0 = 0; // Pin Change Interrupt Flag 0
  // PCICR
  PCIE0 = 0; // Pin Change Interrupt Enable 0
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // Left Adjust Result
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ADCSRB
  ADHSM = 7; // ADC High Speed Mode
  ADTS = 0; // ADC Auto Trigger Sources
  // DIDR0
  ADC7D = 7; // ADC7 Digital input Disable
  ADC6D = 6; // ADC6 Digital input Disable
  ADC5D = 5; // ADC5 Digital input Disable
  ADC4D = 4; // ADC4 Digital input Disable
  ADC3D = 3; // ADC3 Digital input Disable
  ADC2D = 2; // ADC2 Digital input Disable
  ADC1D = 1; // ADC1 Digital input Disable
  ADC0D = 0; // ADC0 Digital input Disable
  // ADCSRB
  ACME = 6; // Analog Comparator Multiplexer Enable
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
  // PLLCSR
  PLLP = 2; // PLL prescaler Bits
  PLLE = 1; // PLL Enable Bit
  PLOCK = 0; // PLL Lock Status Bit

implementation

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 3 External Interrupt Request 2
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 4 External Interrupt Request 3
procedure INT4_ISR; external name 'INT4_ISR'; // Interrupt 5 External Interrupt Request 4
procedure INT5_ISR; external name 'INT5_ISR'; // Interrupt 6 External Interrupt Request 5
procedure INT6_ISR; external name 'INT6_ISR'; // Interrupt 7 External Interrupt Request 6
procedure INT7_ISR; external name 'INT7_ISR'; // Interrupt 8 External Interrupt Request 7
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 9 Pin Change Interrupt Request 0
procedure USB_GEN_ISR; external name 'USB_GEN_ISR'; // Interrupt 10 USB General Interrupt Request
procedure USB_COM_ISR; external name 'USB_COM_ISR'; // Interrupt 11 USB Endpoint/Pipe Interrupt Communication Request
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 12 Watchdog Time-out Interrupt
procedure TIMER2_COMPA_ISR; external name 'TIMER2_COMPA_ISR'; // Interrupt 13 Timer/Counter2 Compare Match A
procedure TIMER2_COMPB_ISR; external name 'TIMER2_COMPB_ISR'; // Interrupt 14 Timer/Counter2 Compare Match B
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 15 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 16 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 17 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 18 Timer/Counter1 Compare Match B
procedure TIMER1_COMPC_ISR; external name 'TIMER1_COMPC_ISR'; // Interrupt 19 Timer/Counter1 Compare Match C
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 20 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 21 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 22 Timer/Counter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 23 Timer/Counter0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 24 SPI Serial Transfer Complete
procedure USART1__RX_ISR; external name 'USART1__RX_ISR'; // Interrupt 25 USART1, Rx Complete
procedure USART1__UDRE_ISR; external name 'USART1__UDRE_ISR'; // Interrupt 26 USART1 Data register Empty
procedure USART1__TX_ISR; external name 'USART1__TX_ISR'; // Interrupt 27 USART1, Tx Complete
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 28 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 29 ADC Conversion Complete
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 30 EEPROM Ready
procedure TIMER3_CAPT_ISR; external name 'TIMER3_CAPT_ISR'; // Interrupt 31 Timer/Counter3 Capture Event
procedure TIMER3_COMPA_ISR; external name 'TIMER3_COMPA_ISR'; // Interrupt 32 Timer/Counter3 Compare Match A
procedure TIMER3_COMPB_ISR; external name 'TIMER3_COMPB_ISR'; // Interrupt 33 Timer/Counter3 Compare Match B
procedure TIMER3_COMPC_ISR; external name 'TIMER3_COMPC_ISR'; // Interrupt 34 Timer/Counter3 Compare Match C
procedure TIMER3_OVF_ISR; external name 'TIMER3_OVF_ISR'; // Interrupt 35 Timer/Counter3 Overflow
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 36 2-wire Serial Interface        
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 37 Store Program Memory Read

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
   jmp USB_GEN_ISR
   jmp USB_COM_ISR
   jmp WDT_ISR
   jmp TIMER2_COMPA_ISR
   jmp TIMER2_COMPB_ISR
   jmp TIMER2_OVF_ISR
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
   jmp ADC_ISR
   jmp EE_READY_ISR
   jmp TIMER3_CAPT_ISR
   jmp TIMER3_COMPA_ISR
   jmp TIMER3_COMPB_ISR
   jmp TIMER3_COMPC_ISR
   jmp TIMER3_OVF_ISR
   jmp TWI_ISR
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
   .weak USB_GEN_ISR
   .weak USB_COM_ISR
   .weak WDT_ISR
   .weak TIMER2_COMPA_ISR
   .weak TIMER2_COMPB_ISR
   .weak TIMER2_OVF_ISR
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
   .weak ADC_ISR
   .weak EE_READY_ISR
   .weak TIMER3_CAPT_ISR
   .weak TIMER3_COMPA_ISR
   .weak TIMER3_COMPB_ISR
   .weak TIMER3_COMPC_ISR
   .weak TIMER3_OVF_ISR
   .weak TWI_ISR
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
   .set USB_GEN_ISR, Default_IRQ_handler
   .set USB_COM_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set TIMER2_COMPA_ISR, Default_IRQ_handler
   .set TIMER2_COMPB_ISR, Default_IRQ_handler
   .set TIMER2_OVF_ISR, Default_IRQ_handler
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
   .set ADC_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set TIMER3_CAPT_ISR, Default_IRQ_handler
   .set TIMER3_COMPA_ISR, Default_IRQ_handler
   .set TIMER3_COMPB_ISR, Default_IRQ_handler
   .set TIMER3_COMPC_ISR, Default_IRQ_handler
   .set TIMER3_OVF_ISR, Default_IRQ_handler
   .set TWI_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
 end;

end.

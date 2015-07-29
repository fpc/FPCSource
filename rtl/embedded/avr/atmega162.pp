unit ATmega162;

{$goto on}

interface

var
  // TIMER_COUNTER_1
  TIMSK : byte absolute $00+$59; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$58; // Timer/Counter Interrupt Flag register
  TCCR1A : byte absolute $00+$4F; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$4E; // Timer/Counter1 Control Register B
  TCNT1 : word absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$4C+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$4A; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AL : byte absolute $00+$4A; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AH : byte absolute $00+$4A+1; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1B : word absolute $00+$48; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BL : byte absolute $00+$48; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BH : byte absolute $00+$48+1; // Timer/Counter1 Output Compare Register B  Bytes
  ICR1 : word absolute $00+$44; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$44; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$44+1; // Timer/Counter1 Input Capture Register  Bytes
  // TIMER_COUNTER_2
  TCCR2 : byte absolute $00+$47; // Timer/Counter Control Register
  TCNT2 : byte absolute $00+$43; // Timer/Counter Register
  OCR2 : byte absolute $00+$42; // Output Compare Register
  ASSR : byte absolute $00+$46; // Asynchronous Status Register
  // TIMER_COUNTER_3
  ETIMSK : byte absolute $00+$7D; // Extended Timer/Counter Interrupt Mask Register
  ETIFR : byte absolute $00+$7C; // Extended Timer/Counter Interrupt Flag register
  TCCR3A : byte absolute $00+$8B; // Timer/Counter3 Control Register A
  TCCR3B : byte absolute $00+$8A; // Timer/Counter3 Control Register B
  TCNT3 : word absolute $00+$88; // Timer/Counter3  Bytes
  TCNT3L : byte absolute $00+$88; // Timer/Counter3  Bytes
  TCNT3H : byte absolute $00+$88+1; // Timer/Counter3  Bytes
  OCR3A : word absolute $00+$86; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AL : byte absolute $00+$86; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AH : byte absolute $00+$86+1; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3B : word absolute $00+$84; // Timer/Counte3 Output Compare Register B  Bytes
  OCR3BL : byte absolute $00+$84; // Timer/Counte3 Output Compare Register B  Bytes
  OCR3BH : byte absolute $00+$84+1; // Timer/Counte3 Output Compare Register B  Bytes
  ICR3 : word absolute $00+$80; // Timer/Counter3 Input Capture Register  Bytes
  ICR3L : byte absolute $00+$80; // Timer/Counter3 Input Capture Register  Bytes
  ICR3H : byte absolute $00+$80+1; // Timer/Counter3 Input Capture Register  Bytes
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  // USART0
  UDR0 : byte absolute $00+$2C; // USART I/O Data Register
  UCSR0A : byte absolute $00+$2B; // USART Control and Status Register A
  UCSR0B : byte absolute $00+$2A; // USART Control and Status Register B
  UCSR0C : byte absolute $00+$40; // USART Control and Status Register C
  UBRR0H : byte absolute $00+$40; // USART Baud Rate Register Hight Byte
  UBRR0L : byte absolute $00+$29; // USART Baud Rate Register Low Byte
  // USART1
  UDR : byte absolute $00+$23; // USART I/O Data Register
  UCSR1A : byte absolute $00+$22; // USART Control and Status Register A
  UCSR1B : byte absolute $00+$21; // USART Control and Status Register B
  UCSR1C : byte absolute $00+$5C; // USART Control and Status Register C
  UBRR1H : byte absolute $00+$5C; // USART Baud Rate Register Highg Byte
  UBRR1L : byte absolute $00+$20; // USART Baud Rate Register Low Byte
  // SPI
  SPCR : byte absolute $00+$2D; // SPI Control Register
  SPSR : byte absolute $00+$2E; // SPI Status Register
  SPDR : byte absolute $00+$2F; // SPI Data Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUCSR : byte absolute $00+$54; // MCU Control And Status Register
  EMCUCR : byte absolute $00+$56; // Extended MCU Control Register
  OSCCAL : byte absolute $00+$24; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // Clock prescale register
  SFIOR : byte absolute $00+$50; // Special Function IO Register
  // JTAG
  OCDR : byte absolute $00+$24; // On-Chip Debug Related Register in I/O Memory
  // BOOT_LOAD
  SPMCR : byte absolute $00+$57; // Store Program Memory Control Register
  // EEPROM
  EEAR : word absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$3E+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // PORTA
  PORTA : byte absolute $00+$3B; // Port A Data Register
  DDRA : byte absolute $00+$3A; // Port A Data Direction Register
  PINA : byte absolute $00+$39; // Port A Input Pins
  // PORTB
  PORTB : byte absolute $00+$38; // Port B Data Register
  DDRB : byte absolute $00+$37; // Port B Data Direction Register
  PINB : byte absolute $00+$36; // Port B Input Pins
  // PORTC
  PORTC : byte absolute $00+$35; // Port C Data Register
  DDRC : byte absolute $00+$34; // Port C Data Direction Register
  PINC : byte absolute $00+$33; // Port C Input Pins
  // PORTD
  PORTD : byte absolute $00+$32; // Port D Data Register
  DDRD : byte absolute $00+$31; // Port D Data Direction Register
  PIND : byte absolute $00+$30; // Port D Input Pins
  // TIMER_COUNTER_0
  TCCR0 : byte absolute $00+$53; // Timer/Counter 0 Control Register
  TCNT0 : byte absolute $00+$52; // Timer/Counter 0 Register
  OCR0 : byte absolute $00+$51; // Timer/Counter 0 Output Compare Register
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // PORTE
  PORTE : byte absolute $00+$27; // Data Register, Port E
  DDRE : byte absolute $00+$26; // Data Direction Register, Port E
  PINE : byte absolute $00+$25; // Input Pins, Port E
  // EXTERNAL_INTERRUPT
  GICR : byte absolute $00+$5B; // General Interrupt Control Register
  GIFR : byte absolute $00+$5A; // General Interrupt Flag Register
  PCMSK1 : byte absolute $00+$6C; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$6B; // Pin Change Enable Mask

const
  // TIMSK
  TOIE1 = 7; // Timer/Counter1 Overflow Interrupt Enable
  OCIE1A = 6; // Timer/Counter1 Output CompareA Match Interrupt Enable
  OCIE1B = 5; // Timer/Counter1 Output CompareB Match Interrupt Enable
  TICIE1 = 3; // Timer/Counter1 Input Capture Interrupt Enable
  // TIFR
  TOV1 = 7; // Timer/Counter1 Overflow Flag
  OCF1A = 6; // Output Compare Flag 1A
  OCF1B = 5; // Output Compare Flag 1B
  ICF1 = 3; // Input Capture Flag 1
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  FOC1A = 3; // Force Output Compare for Channel A
  FOC1B = 2; // Force Output Compare for Channel B
  WGM1 = 0; // Pulse Width Modulator Select Bits
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Clock Select1 bits
  // TCCR2
  FOC2 = 7; // Forde Output Compare
  WGM20 = 6; // Pulse Width Modulator Select Bit 0
  COM2 = 4; // Compare Match Output Mode
  WGM21 = 3; // Pulse Width Modulator Select Bit 1
  CS2 = 0; // Clock Select
  // TIMSK
  OCIE2 = 4; // Timer/Counter2 Output Compare Match Interrupt Enable
  TOIE2 = 2; // Timer/Counter2 Overflow Interrupt Enable
  // TIFR
  OCF2 = 4; // Output Compare Flag 2
  TOV2 = 2; // Timer/Counter2 Overflow Flag
  // ASSR
  AS2 = 3; // Asynchronous Timer 2
  TCN2UB = 2; // Timer/Counter2 Update Busy
  OCR2UB = 1; // Output Compare Register2 Update Busy
  TCR2UB = 0; // Timer/Counter Control Register2 Update Busy
  // ETIMSK
  TICIE3 = 5; // Timer/Counter3 Input Capture Interrupt Enable
  OCIE3A = 4; // Timer/Counter3 Output CompareA Match Interrupt Enable
  OCIE3B = 3; // Timer/Counter3 Output CompareB Match Interrupt Enable
  TOIE3 = 2; // Timer/Counter3 Overflow Interrupt Enable
  // ETIFR
  ICF3 = 5; // Input Capture Flag 3
  OCF3A = 4; // Output Compare Flag 3A
  OCF3B = 3; // Output Compare Flag 3B
  TOV3 = 2; // Timer/Counter3 Overflow Flag
  // TCCR3A
  COM3A = 6; // Compare Output Mode 3A, bits
  COM3B = 4; // Compare Output Mode 3B, bits
  FOC3A = 3; // Force Output Compare for Channel A
  FOC3B = 2; // Force Output Compare for Channel B
  WGM3 = 0; // Pulse Width Modulator Select Bits
  // TCCR3B
  ICNC3 = 7; // Input Capture 3 Noise Canceler
  ICES3 = 6; // Input Capture 3 Edge Select
  CS3 = 0; // Clock Select3 bits
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // UCSR0A
  RXC0 = 7; // USART Receive Complete
  TXC0 = 6; // USART Transmitt Complete
  UDRE0 = 5; // USART Data Register Empty
  FE0 = 4; // Framing Error
  DOR0 = 3; // Data overRun
  UPE0 = 2; // Parity Error
  U2X0 = 1; // Double the USART transmission speed
  MPCM0 = 0; // Multi-processor Communication Mode
  // UCSR0B
  RXCIE0 = 7; // RX Complete Interrupt Enable
  TXCIE0 = 6; // TX Complete Interrupt Enable
  UDRIE0 = 5; // USART Data register Empty Interrupt Enable
  RXEN0 = 4; // Receiver Enable
  TXEN0 = 3; // Transmitter Enable
  UCSZ02 = 2; // Character Size
  RXB80 = 1; // Receive Data Bit 8
  TXB80 = 0; // Transmit Data Bit 8
  // UCSR0C
  URSEL0 = 7; // Register Select
  UMSEL0 = 6; // USART Mode Select
  UPM0 = 4; // Parity Mode Bits
  USBS0 = 3; // Stop Bit Select
  UCSZ0 = 1; // Character Size
  UCPOL0 = 0; // Clock Polarity
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
  URSEL1 = 7; // Register Select
  UMSEL1 = 6; // USART Mode Select
  UPM1 = 4; // Parity Mode Bits
  USBS1 = 3; // Stop Bit Select
  UCSZ1 = 1; // Character Size
  UCPOL1 = 0; // Clock Polarity
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
  SRE = 7; // External SRAM Enable
  SRW10 = 6; // External SRAM Wait State Select
  SE = 5; // Sleep Enable
  SM1 = 4; // Sleep Mode Select
  ISC1 = 2; // Interrupt Sense Control 1 bits
  ISC0 = 0; // Interrupt Sense Control 0 bits
  // MCUCSR
  JDT = 7; // JTAG Interface Disable
  SM2 = 5; // Sleep Mode Select Bit 2
  JTRF = 4; // JTAG Reset Flag
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // EMCUCR
  SM0 = 7; // Sleep mode Select Bit 0
  SRL = 4; // Wait State Sector Limit Bits
  SRW0 = 2; // Wait State Select Bit 1 for Lower Sector
  SRW11 = 1; // Wait State Select Bit 1 for Upper Sector
  ISC2 = 0; // Interrupt Sense Control 2
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
  // SFIOR
  TSM = 7; // Timer/Counter Synchronization Mode
  XMBK = 6; // External Memory Bus Keeper Enable
  XMM = 3; // External Memory High Mask Bits
  PUD = 2; // Pull-up Disable
  PSR2 = 1; // Prescaler Reset Timer/Counter2
  PSR310 = 0; // Prescaler Reset Timer/Counter3, Timer/Counter1 and Timer/Counter0
  // OCDR
  // MCUCSR
  JTD = 7; // JTAG Interface Disable
  // SPMCR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  RWWSRE = 4; // Read While Write secion read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // EECR
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // TCCR0
  FOC0 = 7; // Force Output Compare
  WGM00 = 6; // Waveform Generation Mode 0
  COM0 = 4; // Compare Match Output Modes
  WGM01 = 3; // Waveform Generation Mode 1
  CS0 = 0; // Clock Selects
  // TIMSK
  TOIE0 = 1; // Timer/Counter0 Overflow Interrupt Enable
  OCIE0 = 0; // Timer/Counter0 Output Compare Match Interrupt register
  // TIFR
  TOV0 = 1; // Timer/Counter0 Overflow Flag
  OCF0 = 0; // Output Compare Flag 0
  // WDTCR
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  WDP = 0; // Watch Dog Timer Prescaler bits
  // MCUCR
  // EMCUCR
  // GICR
  INT = 6; // External Interrupt Request 1 Enable
  INT2 = 5; // External Interrupt Request 2 Enable
  PCIE = 3; // Pin Change Interrupt Enables
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // GIFR
  INTF = 6; // External Interrupt Flags
  INTF2 = 5; // External Interrupt Flag 2
  PCIF = 3; // Pin Change Interrupt Flags

implementation

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 3 External Interrupt Request 2
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 4 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 5 Pin Change Interrupt Request 1
procedure TIMER3_CAPT_ISR; external name 'TIMER3_CAPT_ISR'; // Interrupt 6 Timer/Counter3 Capture Event
procedure TIMER3_COMPA_ISR; external name 'TIMER3_COMPA_ISR'; // Interrupt 7 Timer/Counter3 Compare Match A
procedure TIMER3_COMPB_ISR; external name 'TIMER3_COMPB_ISR'; // Interrupt 8 Timer/Counter3 Compare Match B
procedure TIMER3_OVF_ISR; external name 'TIMER3_OVF_ISR'; // Interrupt 9 Timer/Counter3 Overflow
procedure TIMER2_COMP_ISR; external name 'TIMER2_COMP_ISR'; // Interrupt 10 Timer/Counter2 Compare Match
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 11 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 12 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 13 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 14 Timer/Counter Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 15 Timer/Counter1 Overflow
procedure TIMER0_COMP_ISR; external name 'TIMER0_COMP_ISR'; // Interrupt 16 Timer/Counter0 Compare Match
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 17 Timer/Counter0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 18 SPI Serial Transfer Complete
procedure USART0__RXC_ISR; external name 'USART0__RXC_ISR'; // Interrupt 19 USART0, Rx Complete
procedure USART1__RXC_ISR; external name 'USART1__RXC_ISR'; // Interrupt 20 USART1, Rx Complete
procedure USART0__UDRE_ISR; external name 'USART0__UDRE_ISR'; // Interrupt 21 USART0 Data register Empty
procedure USART1__UDRE_ISR; external name 'USART1__UDRE_ISR'; // Interrupt 22 USART1, Data register Empty
procedure USART0__TXC_ISR; external name 'USART0__TXC_ISR'; // Interrupt 23 USART0, Tx Complete
procedure USART1__TXC_ISR; external name 'USART1__TXC_ISR'; // Interrupt 24 USART1, Tx Complete
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 25 EEPROM Ready
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 26 Analog Comparator
procedure SPM_RDY_ISR; external name 'SPM_RDY_ISR'; // Interrupt 27 Store Program Memory Read

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
   jmp PCINT0_ISR
   jmp PCINT1_ISR
   jmp TIMER3_CAPT_ISR
   jmp TIMER3_COMPA_ISR
   jmp TIMER3_COMPB_ISR
   jmp TIMER3_OVF_ISR
   jmp TIMER2_COMP_ISR
   jmp TIMER2_OVF_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMP_ISR
   jmp TIMER0_OVF_ISR
   jmp SPI__STC_ISR
   jmp USART0__RXC_ISR
   jmp USART1__RXC_ISR
   jmp USART0__UDRE_ISR
   jmp USART1__UDRE_ISR
   jmp USART0__TXC_ISR
   jmp USART1__TXC_ISR
   jmp EE_RDY_ISR
   jmp ANA_COMP_ISR
   jmp SPM_RDY_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak INT2_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak TIMER3_CAPT_ISR
   .weak TIMER3_COMPA_ISR
   .weak TIMER3_COMPB_ISR
   .weak TIMER3_OVF_ISR
   .weak TIMER2_COMP_ISR
   .weak TIMER2_OVF_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMP_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI__STC_ISR
   .weak USART0__RXC_ISR
   .weak USART1__RXC_ISR
   .weak USART0__UDRE_ISR
   .weak USART1__UDRE_ISR
   .weak USART0__TXC_ISR
   .weak USART1__TXC_ISR
   .weak EE_RDY_ISR
   .weak ANA_COMP_ISR
   .weak SPM_RDY_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set TIMER3_CAPT_ISR, Default_IRQ_handler
   .set TIMER3_COMPA_ISR, Default_IRQ_handler
   .set TIMER3_COMPB_ISR, Default_IRQ_handler
   .set TIMER3_OVF_ISR, Default_IRQ_handler
   .set TIMER2_COMP_ISR, Default_IRQ_handler
   .set TIMER2_OVF_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMP_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART0__RXC_ISR, Default_IRQ_handler
   .set USART1__RXC_ISR, Default_IRQ_handler
   .set USART0__UDRE_ISR, Default_IRQ_handler
   .set USART1__UDRE_ISR, Default_IRQ_handler
   .set USART0__TXC_ISR, Default_IRQ_handler
   .set USART1__TXC_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set SPM_RDY_ISR, Default_IRQ_handler
 end;

end.

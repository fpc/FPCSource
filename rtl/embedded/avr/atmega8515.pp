unit ATmega8515;

{$goto on}

interface

var
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  // USART
  UDR : byte absolute $00+$2C; // USART I/O Data Register
  UCSRA : byte absolute $00+$2B; // USART Control and Status Register A
  UCSRB : byte absolute $00+$2A; // USART Control and Status Register B
  UCSRC : byte absolute $00+$40; // USART Control and Status Register C
  UBRRH : byte absolute $00+$40; // USART Baud Rate Register High Byte
  UBRRL : byte absolute $00+$29; // USART Baud Rate Register Low Byte
  // SPI
  SPDR : byte absolute $00+$2F; // SPI Data Register
  SPSR : byte absolute $00+$2E; // SPI Status Register
  SPCR : byte absolute $00+$2D; // SPI Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  EMCUCR : byte absolute $00+$56; // Extended MCU Control Register
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUCSR : byte absolute $00+$54; // MCU Control And Status Register
  OSCCAL : byte absolute $00+$24; // Oscillator Calibration Value
  SPMCR : byte absolute $00+$57; // Store Program Memory Control Register
  SFIOR : byte absolute $00+$50; // Special Function IO Register
  // EXTERNAL_INTERRUPT
  GICR : byte absolute $00+$5B; // General Interrupt Control Register
  GIFR : byte absolute $00+$5A; // General Interrupt Flag Register
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // TIMER_COUNTER_0
  TCCR0 : byte absolute $00+$53; // Timer/Counter 0 Control Register
  TCNT0 : byte absolute $00+$52; // Timer/Counter 0 Register
  OCR0 : byte absolute $00+$51; // Timer/Counter 0 Output Compare Register
  TIMSK : byte absolute $00+$59; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$58; // Timer/Counter Interrupt Flag register
  // TIMER_COUNTER_1
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
  // PORTE
  PORTE : byte absolute $00+$27; // Port E Data Register
  DDRE : byte absolute $00+$26; // Port E Data Direction Register
  PINE : byte absolute $00+$25; // Port E Input Pins
  // EEPROM
  EEAR : word absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$3E+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register

const
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // UCSRA
  RXC = 7; // USART Receive Complete
  TXC = 6; // USART Transmitt Complete
  UDRE = 5; // USART Data Register Empty
  FE = 4; // Framing Error
  DOR = 3; // Data overRun
  UPE = 2; // Parity Error
  U2X = 1; // Double the USART transmission speed
  MPCM = 0; // Multi-processor Communication Mode
  // UCSRB
  RXCIE = 7; // RX Complete Interrupt Enable
  TXCIE = 6; // TX Complete Interrupt Enable
  UDRIE = 5; // USART Data register Empty Interrupt Enable
  RXEN = 4; // Receiver Enable
  TXEN = 3; // Transmitter Enable
  UCSZ2 = 2; // Character Size Bit 2
  RXB8 = 1; // Receive Data Bit 8
  TXB8 = 0; // Transmit Data Bit 8
  // UCSRC
  URSEL = 7; // Register Select
  UMSEL = 6; // USART Mode Select
  UPM = 4; // Parity Mode Bits
  USBS = 3; // Stop Bit Select
  UCSZ = 1; // Character Size Bits
  UCPOL = 0; // Clock Polarity
  // UBRRH
  UBRR1 = 2; // USART Baud Rate Register bit 11
  UBRR = 0; // USART Baud Rate Register bits
  // SPSR
  SPIF = 7; // SPI Interrupt Flag
  WCOL = 6; // Write Collision Flag
  SPI2X = 0; // Double SPI Speed Bit
  // SPCR
  SPIE = 7; // SPI Interrupt Enable
  SPE = 6; // SPI Enable
  DORD = 5; // Data Order
  MSTR = 4; // Master/Slave Select
  CPOL = 3; // Clock polarity
  CPHA = 2; // Clock Phase
  SPR = 0; // SPI Clock Rate Selects
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // EMCUCR
  SM0 = 7; // Sleep Mode Select Bit 0
  SRL = 4; // Wait State Selector Limit bits
  SRW0 = 2; // Wait State Select Bits for Lower Sector, bits
  SRW11 = 1; // Wait State Select Bits for Upper Sector, bit 1
  ISC2 = 0; // Interrupt Sense Control 2
  // MCUCR
  SRE = 7; // External SRAM/XMEM Enable
  SRW10 = 6; // Wait State Select Bits for Upper Sector, bit 0
  SE = 5; // Sleep Enable
  SM1 = 4; // Sleep Mode Select Bit 1
  ISC1 = 2; // Interrupt Sense Control 1 Bits
  ISC0 = 0; // Interrupt Sense Control 0 Bits
  // MCUCSR
  SM2 = 5; // Sleep Mode Select Bit 2
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // SPMCR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read-While-Write Section Busy
  RWWSRE = 4; // Read-While-Write Section Read Enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // SFIOR
  XMBK = 6; // External Memory Bus Keeper Enable
  XMM = 3; // External Memory High Mask Bits
  PUD = 2; // Pull-up Disable
  PSR10 = 0; // Prescaler Reset Timer / Counter 1 and Timer / Counter 0
  // GICR
  INT = 6; // External Interrupt Request 1 Enable
  INT2 = 5; // External Interrupt Request 2 Enable
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // GIFR
  INTF = 6; // External Interrupt Flags
  INTF2 = 5; // External Interrupt Flag 2
  // WDTCR
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  WDP = 0; // Watch Dog Timer Prescaler bits
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
  // EECR
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 3 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 4 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 5 Timer/Counter1 Compare MatchB
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 6 Timer/Counter1 Overflow
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 7 Timer/Counter0 Overflow
procedure SPI_STC_ISR; external name 'SPI_STC_ISR'; // Interrupt 8 Serial Transfer Complete
procedure USART_RX_ISR; external name 'USART_RX_ISR'; // Interrupt 9 USART, Rx Complete
procedure USART_UDRE_ISR; external name 'USART_UDRE_ISR'; // Interrupt 10 USART Data Register Empty
procedure USART__TX_ISR; external name 'USART__TX_ISR'; // Interrupt 11 USART, Tx Complete
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 12 Analog Comparator
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 13 External Interrupt Request 2
procedure TIMER0_COMP_ISR; external name 'TIMER0_COMP_ISR'; // Interrupt 14 Timer 0 Compare Match
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 15 EEPROM Ready
procedure SPM_RDY_ISR; external name 'SPM_RDY_ISR'; // Interrupt 16 Store Program Memory Ready

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp INT1_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_OVF_ISR
   rjmp SPI_STC_ISR
   rjmp USART_RX_ISR
   rjmp USART_UDRE_ISR
   rjmp USART__TX_ISR
   rjmp ANA_COMP_ISR
   rjmp INT2_ISR
   rjmp TIMER0_COMP_ISR
   rjmp EE_RDY_ISR
   rjmp SPM_RDY_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI_STC_ISR
   .weak USART_RX_ISR
   .weak USART_UDRE_ISR
   .weak USART__TX_ISR
   .weak ANA_COMP_ISR
   .weak INT2_ISR
   .weak TIMER0_COMP_ISR
   .weak EE_RDY_ISR
   .weak SPM_RDY_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI_STC_ISR, Default_IRQ_handler
   .set USART_RX_ISR, Default_IRQ_handler
   .set USART_UDRE_ISR, Default_IRQ_handler
   .set USART__TX_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set TIMER0_COMP_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set SPM_RDY_ISR, Default_IRQ_handler
 end;

end.

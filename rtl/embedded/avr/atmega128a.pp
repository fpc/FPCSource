unit ATmega128A;

{$goto on}

interface

var
  // ANALOG_COMPARATOR
  SFIOR : byte absolute $00+$40; // Special Function IO Register
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  // SPI
  SPDR : byte absolute $00+$2F; // SPI Data Register
  SPSR : byte absolute $00+$2E; // SPI Status Register
  SPCR : byte absolute $00+$2D; // SPI Control Register
  // TWI
  TWBR : byte absolute $00+$70; // TWI Bit Rate register
  TWCR : byte absolute $00+$74; // TWI Control Register
  TWSR : byte absolute $00+$71; // TWI Status Register
  TWDR : byte absolute $00+$73; // TWI Data register
  TWAR : byte absolute $00+$72; // TWI (Slave) Address register
  // USART0
  UDR0 : byte absolute $00+$2C; // USART I/O Data Register
  UCSR0A : byte absolute $00+$2B; // USART Control and Status Register A
  UCSR0B : byte absolute $00+$2A; // USART Control and Status Register B
  UCSR0C : byte absolute $00+$95; // USART Control and Status Register C
  UBRR0H : byte absolute $00+$90; // USART Baud Rate Register Hight Byte
  UBRR0L : byte absolute $00+$29; // USART Baud Rate Register Low Byte
  // USART1
  UDR1 : byte absolute $00+$9C; // USART I/O Data Register
  UCSR1A : byte absolute $00+$9B; // USART Control and Status Register A
  UCSR1B : byte absolute $00+$9A; // USART Control and Status Register B
  UCSR1C : byte absolute $00+$9D; // USART Control and Status Register C
  UBRR1H : byte absolute $00+$98; // USART Baud Rate Register Hight Byte
  UBRR1L : byte absolute $00+$99; // USART Baud Rate Register Low Byte
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUCSR : byte absolute $00+$54; // MCU Control And Status Register
  XMCRA : byte absolute $00+$6D; // External Memory Control Register A
  XMCRB : byte absolute $00+$6C; // External Memory Control Register B
  OSCCAL : byte absolute $00+$6F; // Oscillator Calibration Value
  XDIV : byte absolute $00+$5C; // XTAL Divide Control Register
  RAMPZ : byte absolute $00+$5B; // RAM Page Z Select Register
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$68; // Store Program Memory Control Register
  // JTAG
  OCDR : byte absolute $00+$42; // On-Chip Debug Related Register in I/O Memory
  // MISC
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$6A; // External Interrupt Control Register A
  EICRB : byte absolute $00+$5A; // External Interrupt Control Register B
  EIMSK : byte absolute $00+$59; // External Interrupt Mask Register
  EIFR : byte absolute $00+$58; // External Interrupt Flag Register
  // EEPROM
  EEAR : word absolute $00+$3E; // EEPROM Read/Write Access  Bytes
  EEARL : byte absolute $00+$3E; // EEPROM Read/Write Access  Bytes
  EEARH : byte absolute $00+$3E+1; // EEPROM Read/Write Access  Bytes
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
  // PORTE
  PORTE : byte absolute $00+$23; // Data Register, Port E
  DDRE : byte absolute $00+$22; // Data Direction Register, Port E
  PINE : byte absolute $00+$21; // Input Pins, Port E
  // PORTF
  PORTF : byte absolute $00+$62; // Data Register, Port F
  DDRF : byte absolute $00+$61; // Data Direction Register, Port F
  PINF : byte absolute $00+$20; // Input Pins, Port F
  // PORTG
  PORTG : byte absolute $00+$65; // Data Register, Port G
  DDRG : byte absolute $00+$64; // Data Direction Register, Port G
  PING : byte absolute $00+$63; // Input Pins, Port G
  // TIMER_COUNTER_0
  TCCR0 : byte absolute $00+$53; // Timer/Counter Control Register
  TCNT0 : byte absolute $00+$52; // Timer/Counter Register
  OCR0 : byte absolute $00+$51; // Output Compare Register
  ASSR : byte absolute $00+$50; // Asynchronus Status Register
  TIMSK : byte absolute $00+$57; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$56; // Timer/Counter Interrupt Flag register
  // TIMER_COUNTER_1
  ETIMSK : byte absolute $00+$7D; // Extended Timer/Counter Interrupt Mask Register
  ETIFR : byte absolute $00+$7C; // Extended Timer/Counter Interrupt Flag register
  TCCR1A : byte absolute $00+$4F; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$4E; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$7A; // Timer/Counter1 Control Register C
  TCNT1 : word absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$4C+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$4A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AL : byte absolute $00+$4A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AH : byte absolute $00+$4A+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1B : word absolute $00+$48; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BL : byte absolute $00+$48; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BH : byte absolute $00+$48+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1C : word absolute $00+$78; // Timer/Counter1 Output Compare Register  Bytes
  OCR1CL : byte absolute $00+$78; // Timer/Counter1 Output Compare Register  Bytes
  OCR1CH : byte absolute $00+$78+1; // Timer/Counter1 Output Compare Register  Bytes
  ICR1 : word absolute $00+$46; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$46; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$46+1; // Timer/Counter1 Input Capture Register  Bytes
  // TIMER_COUNTER_2
  TCCR2 : byte absolute $00+$45; // Timer/Counter Control Register
  TCNT2 : byte absolute $00+$44; // Timer/Counter Register
  OCR2 : byte absolute $00+$43; // Output Compare Register
  // TIMER_COUNTER_3
  TCCR3A : byte absolute $00+$8B; // Timer/Counter3 Control Register A
  TCCR3B : byte absolute $00+$8A; // Timer/Counter3 Control Register B
  TCCR3C : byte absolute $00+$8C; // Timer/Counter3 Control Register C
  TCNT3 : word absolute $00+$88; // Timer/Counter3  Bytes
  TCNT3L : byte absolute $00+$88; // Timer/Counter3  Bytes
  TCNT3H : byte absolute $00+$88+1; // Timer/Counter3  Bytes
  OCR3A : word absolute $00+$86; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AL : byte absolute $00+$86; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AH : byte absolute $00+$86+1; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3B : word absolute $00+$84; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BL : byte absolute $00+$84; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BH : byte absolute $00+$84+1; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3C : word absolute $00+$82; // Timer/Counter3 Output compare Register C  Bytes
  OCR3CL : byte absolute $00+$82; // Timer/Counter3 Output compare Register C  Bytes
  OCR3CH : byte absolute $00+$82+1; // Timer/Counter3 Output compare Register C  Bytes
  ICR3 : word absolute $00+$80; // Timer/Counter3 Input Capture Register  Bytes
  ICR3L : byte absolute $00+$80; // Timer/Counter3 Input Capture Register  Bytes
  ICR3H : byte absolute $00+$80+1; // Timer/Counter3 Input Capture Register  Bytes
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$27; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$26; // The ADC Control and Status register
  ADC : word absolute $00+$24; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$24; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$24+1; // ADC Data Register  Bytes

const
  // SFIOR
  ACME = 3; // Analog Comparator Multiplexer Enable
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
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
  UMSEL1 = 6; // USART Mode Select
  UPM1 = 4; // Parity Mode Bits
  USBS1 = 3; // Stop Bit Select
  UCSZ1 = 1; // Character Size
  UCPOL1 = 0; // Clock Polarity
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
  SM = 3; // Sleep Mode Select
  SM2 = 2; // Sleep Mode Select
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUCSR
  JTD = 7; // JTAG Interface Disable
  JTRF = 4; // JTAG Reset Flag
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // XMCRA
  SRL = 4; // Wait state page limit
  SRW0 = 2; // Wait state select bit lower page
  SRW11 = 1; // Wait state select bit upper page
  // XMCRB
  XMBK = 7; // External Memory Bus Keeper Enable
  XMM = 0; // External Memory High Mask
  // RAMPZ
  RAMPZ0 = 0; // RAM Page Z Select Register Bit 0
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  RWWSRE = 4; // Read While Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // OCDR
  // MCUCSR
  // SFIOR
  TSM = 7; // Timer/Counter Synchronization Mode
  PUD = 2; // Pull Up Disable
  PSR0 = 1; // Prescaler Reset Timer/Counter0
  PSR321 = 0; // Prescaler Reset Timer/Counter3, Timer/Counter2, and Timer/Counter1
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
  // ASSR
  AS0 = 3; // Asynchronus Timer/Counter 0
  TCN0UB = 2; // Timer/Counter0 Update Busy
  OCR0UB = 1; // Output Compare register 0 Busy
  TCR0UB = 0; // Timer/Counter Control Register 0 Update Busy
  // TIMSK
  OCIE0 = 1; // Timer/Counter0 Output Compare Match Interrupt register
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR
  OCF0 = 1; // Output Compare Flag 0
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // SFIOR
  // TIMSK
  TICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  OCIE1A = 4; // Timer/Counter1 Output CompareA Match Interrupt Enable
  OCIE1B = 3; // Timer/Counter1 Output CompareB Match Interrupt Enable
  TOIE1 = 2; // Timer/Counter1 Overflow Interrupt Enable
  // ETIMSK
  OCIE1C = 0; // Timer/Counter 1, Output Compare Match C Interrupt Enable
  // TIFR
  ICF1 = 5; // Input Capture Flag 1
  OCF1A = 4; // Output Compare Flag 1A
  OCF1B = 3; // Output Compare Flag 1B
  TOV1 = 2; // Timer/Counter1 Overflow Flag
  // ETIFR
  OCF1C = 0; // Timer/Counter 1, Output Compare C Match Flag
  // SFIOR
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  COM1C = 2; // Compare Output Mode 1C, bits
  WGM1 = 0; // Waveform Generation Mode Bits
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Clock Select1 bits
  // TCCR1C
  FOC1A = 7; // Force Output Compare for channel A
  FOC1B = 6; // Force Output Compare for channel B
  FOC1C = 5; // Force Output Compare for channel C
  // TCCR2
  FOC2 = 7; // Force Output Compare
  WGM20 = 6; // Wafeform Generation Mode
  COM2 = 4; // Compare Match Output Mode
  WGM21 = 3; // Waveform Generation Mode
  CS2 = 0; // Clock Select
  // TIFR
  OCF2 = 7; // Output Compare Flag 2
  TOV2 = 6; // Timer/Counter2 Overflow Flag
  // TIMSK
  OCIE2 = 7; // 
  TOIE2 = 6; // 
  // ETIMSK
  TICIE3 = 5; // Timer/Counter3 Input Capture Interrupt Enable
  OCIE3A = 4; // Timer/Counter3 Output CompareA Match Interrupt Enable
  OCIE3B = 3; // Timer/Counter3 Output CompareB Match Interrupt Enable
  TOIE3 = 2; // Timer/Counter3 Overflow Interrupt Enable
  OCIE3C = 1; // Timer/Counter3, Output Compare Match Interrupt Enable
  // ETIFR
  ICF3 = 5; // Input Capture Flag 1
  OCF3A = 4; // Output Compare Flag 1A
  OCF3B = 3; // Output Compare Flag 1B
  TOV3 = 2; // Timer/Counter3 Overflow Flag
  OCF3C = 1; // Timer/Counter3 Output Compare C Match Flag
  // SFIOR
  // TCCR3A
  COM3A = 6; // Compare Output Mode 3A, bits
  COM3B = 4; // Compare Output Mode 3B, bits
  COM3C = 2; // Compare Output Mode 3C, bits
  WGM3 = 0; // Waveform Generation Mode Bits
  // TCCR3B
  ICNC3 = 7; // Input Capture 3  Noise Canceler
  ICES3 = 6; // Input Capture 3 Edge Select
  CS3 = 0; // Clock Select3 bits
  // TCCR3C
  FOC3A = 7; // Force Output Compare for channel A
  FOC3B = 6; // Force Output Compare for channel B
  FOC3C = 5; // Force Output Compare for channel C
  // WDTCR
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  WDP = 0; // Watch Dog Timer Prescaler bits
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // Left Adjust Result
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADFR = 5; // ADC  Free Running Select
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits

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
procedure TIMER2_COMP_ISR; external name 'TIMER2_COMP_ISR'; // Interrupt 9 Timer/Counter2 Compare Match
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 10 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 11 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 12 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 13 Timer/Counter Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 14 Timer/Counter1 Overflow
procedure TIMER0_COMP_ISR; external name 'TIMER0_COMP_ISR'; // Interrupt 15 Timer/Counter0 Compare Match
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 16 Timer/Counter0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 17 SPI Serial Transfer Complete
procedure USART0__RX_ISR; external name 'USART0__RX_ISR'; // Interrupt 18 USART0, Rx Complete
procedure USART0__UDRE_ISR; external name 'USART0__UDRE_ISR'; // Interrupt 19 USART0 Data Register Empty
procedure USART0__TX_ISR; external name 'USART0__TX_ISR'; // Interrupt 20 USART0, Tx Complete
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 21 ADC Conversion Complete
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 22 EEPROM Ready
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 23 Analog Comparator
procedure TIMER1_COMPC_ISR; external name 'TIMER1_COMPC_ISR'; // Interrupt 24 Timer/Counter1 Compare Match C
procedure TIMER3_CAPT_ISR; external name 'TIMER3_CAPT_ISR'; // Interrupt 25 Timer/Counter3 Capture Event
procedure TIMER3_COMPA_ISR; external name 'TIMER3_COMPA_ISR'; // Interrupt 26 Timer/Counter3 Compare Match A
procedure TIMER3_COMPB_ISR; external name 'TIMER3_COMPB_ISR'; // Interrupt 27 Timer/Counter3 Compare Match B
procedure TIMER3_COMPC_ISR; external name 'TIMER3_COMPC_ISR'; // Interrupt 28 Timer/Counter3 Compare Match C
procedure TIMER3_OVF_ISR; external name 'TIMER3_OVF_ISR'; // Interrupt 29 Timer/Counter3 Overflow
procedure USART1__RX_ISR; external name 'USART1__RX_ISR'; // Interrupt 30 USART1, Rx Complete
procedure USART1__UDRE_ISR; external name 'USART1__UDRE_ISR'; // Interrupt 31 USART1, Data Register Empty
procedure USART1__TX_ISR; external name 'USART1__TX_ISR'; // Interrupt 32 USART1, Tx Complete
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 33 2-wire Serial Interface
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 34 Store Program Memory Read

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
   jmp TIMER2_COMP_ISR
   jmp TIMER2_OVF_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMP_ISR
   jmp TIMER0_OVF_ISR
   jmp SPI__STC_ISR
   jmp USART0__RX_ISR
   jmp USART0__UDRE_ISR
   jmp USART0__TX_ISR
   jmp ADC_ISR
   jmp EE_READY_ISR
   jmp ANALOG_COMP_ISR
   jmp TIMER1_COMPC_ISR
   jmp TIMER3_CAPT_ISR
   jmp TIMER3_COMPA_ISR
   jmp TIMER3_COMPB_ISR
   jmp TIMER3_COMPC_ISR
   jmp TIMER3_OVF_ISR
   jmp USART1__RX_ISR
   jmp USART1__UDRE_ISR
   jmp USART1__TX_ISR
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
   .weak TIMER2_COMP_ISR
   .weak TIMER2_OVF_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMP_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI__STC_ISR
   .weak USART0__RX_ISR
   .weak USART0__UDRE_ISR
   .weak USART0__TX_ISR
   .weak ADC_ISR
   .weak EE_READY_ISR
   .weak ANALOG_COMP_ISR
   .weak TIMER1_COMPC_ISR
   .weak TIMER3_CAPT_ISR
   .weak TIMER3_COMPA_ISR
   .weak TIMER3_COMPB_ISR
   .weak TIMER3_COMPC_ISR
   .weak TIMER3_OVF_ISR
   .weak USART1__RX_ISR
   .weak USART1__UDRE_ISR
   .weak USART1__TX_ISR
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
   .set TIMER2_COMP_ISR, Default_IRQ_handler
   .set TIMER2_OVF_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMP_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART0__RX_ISR, Default_IRQ_handler
   .set USART0__UDRE_ISR, Default_IRQ_handler
   .set USART0__TX_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set ANALOG_COMP_ISR, Default_IRQ_handler
   .set TIMER1_COMPC_ISR, Default_IRQ_handler
   .set TIMER3_CAPT_ISR, Default_IRQ_handler
   .set TIMER3_COMPA_ISR, Default_IRQ_handler
   .set TIMER3_COMPB_ISR, Default_IRQ_handler
   .set TIMER3_COMPC_ISR, Default_IRQ_handler
   .set TIMER3_OVF_ISR, Default_IRQ_handler
   .set USART1__RX_ISR, Default_IRQ_handler
   .set USART1__UDRE_ISR, Default_IRQ_handler
   .set USART1__TX_ISR, Default_IRQ_handler
   .set TWI_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
 end;

end.

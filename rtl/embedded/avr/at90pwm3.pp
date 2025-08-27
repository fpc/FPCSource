unit AT90PWM3;

interface

var
  PINB: byte absolute $23;  // Port B Input Pins
  DDRB: byte absolute $24;  // Port B Data Direction Register
  PORTB: byte absolute $25;  // Port B Data Register
  PINC: byte absolute $26;  // Port C Input Pins
  DDRC: byte absolute $27;  // Port C Data Direction Register
  PORTC: byte absolute $28;  // Port C Data Register
  PIND: byte absolute $29;  // Port D Input Pins
  DDRD: byte absolute $2A;  // Port D Data Direction Register
  PORTD: byte absolute $2B;  // Port D Data Register
  PINE: byte absolute $2C;  // Port E Input Pins
  DDRE: byte absolute $2D;  // Port E Data Direction Register
  PORTE: byte absolute $2E;  // Port E Data Register
  TIFR0: byte absolute $35;  // Timer/Counter0 Interrupt Flag register
  TIFR1: byte absolute $36;  // Timer/Counter Interrupt Flag register
  GPIOR1: byte absolute $39;  // General Purpose IO Register 1
  GPIOR2: byte absolute $3A;  // General Purpose IO Register 2
  GPIOR3: byte absolute $3B;  // General Purpose IO Register 3
  EIFR: byte absolute $3C;  // External Interrupt Flag Register
  EIMSK: byte absolute $3D;  // External Interrupt Mask Register
  GPIOR0: byte absolute $3E;  // General Purpose IO Register 0
  EECR: byte absolute $3F;  // EEPROM Control Register
  EEDR: byte absolute $40;  // EEPROM Data Register
  EEAR: word absolute $41;  // EEPROM Read/Write Access Bytes
  EEARL: byte absolute $41;  // EEPROM Read/Write Access Bytes
  EEARH: byte absolute $42;  // EEPROM Read/Write Access Bytes;
  GTCCR: byte absolute $43;  // General Timer/Counter Control Register
  TCCR0A: byte absolute $44;  // Timer/Counter  Control Register A
  TCCR0B: byte absolute $45;  // Timer/Counter Control Register B
  TCNT0: byte absolute $46;  // Timer/Counter0
  OCR0A: byte absolute $47;  // Timer/Counter0 Output Compare Register
  OCR0B: byte absolute $48;  // Timer/Counter0 Output Compare Register
  PLLCSR: byte absolute $49;  // PLL Control And Status Register
  SPCR: byte absolute $4C;  // SPI Control Register
  SPSR: byte absolute $4D;  // SPI Status Register
  SPDR: byte absolute $4E;  // SPI Data Register
  ACSR: byte absolute $50;  // Analog Comparator Status Register
  SMCR: byte absolute $53;  // Sleep Mode Control Register
  MCUSR: byte absolute $54;  // MCU Status Register
  MCUCR: byte absolute $55;  // MCU Control Register
  SPMCSR: byte absolute $57;  // Store Program Memory Control Register
  SP: word absolute $5D;  // Stack Pointer 
  SPL: byte absolute $5D;  // Stack Pointer 
  SPH: byte absolute $5E;  // Stack Pointer ;
  SREG: byte absolute $5F;  // Status Register
  WDTCSR: byte absolute $60;  // Watchdog Timer Control Register
  CLKPR: byte absolute $61;
  PRR: byte absolute $64;  // Power Reduction Register
  OSCCAL: byte absolute $66;  // Oscillator Calibration Value
  EICRA: byte absolute $69;  // External Interrupt Control Register A
  TIMSK0: byte absolute $6E;  // Timer/Counter0 Interrupt Mask Register
  TIMSK1: byte absolute $6F;  // Timer/Counter Interrupt Mask Register
  AMP0CSR: byte absolute $76;
  AMP1CSR: byte absolute $77;
  ADC: word absolute $78;  // ADC Data Register Bytes
  ADCL: byte absolute $78;  // ADC Data Register Bytes
  ADCH: byte absolute $79;  // ADC Data Register Bytes;
  ADCSRA: byte absolute $7A;  // The ADC Control and Status register
  ADCSRB: byte absolute $7B;  // ADC Control and Status Register B
  ADMUX: byte absolute $7C;  // The ADC multiplexer Selection Register
  DIDR0: byte absolute $7E;  // Digital Input Disable Register 0
  DIDR1: byte absolute $7F;  // Digital Input Disable Register 1
  TCCR1A: byte absolute $80;  // Timer/Counter1 Control Register A
  TCCR1B: byte absolute $81;  // Timer/Counter1 Control Register B
  TCCR1C: byte absolute $82;  // Timer/Counter1 Control Register C
  TCNT1: word absolute $84;  // Timer/Counter1 Bytes
  TCNT1L: byte absolute $84;  // Timer/Counter1 Bytes
  TCNT1H: byte absolute $85;  // Timer/Counter1 Bytes;
  ICR1: word absolute $86;  // Timer/Counter1 Input Capture Register Bytes
  ICR1L: byte absolute $86;  // Timer/Counter1 Input Capture Register Bytes
  ICR1H: byte absolute $87;  // Timer/Counter1 Input Capture Register Bytes;
  OCR1A: word absolute $88;  // Timer/Counter1 Output Compare Register Bytes
  OCR1AL: byte absolute $88;  // Timer/Counter1 Output Compare Register Bytes
  OCR1AH: byte absolute $89;  // Timer/Counter1 Output Compare Register Bytes;
  OCR1B: word absolute $8A;  // Timer/Counter1 Output Compare Register Bytes
  OCR1BL: byte absolute $8A;  // Timer/Counter1 Output Compare Register Bytes
  OCR1BH: byte absolute $8B;  // Timer/Counter1 Output Compare Register Bytes;
  PIFR0: byte absolute $A0;  // PSC0 Interrupt Flag Register
  PIM0: byte absolute $A1;  // PSC0 Interrupt Mask Register
  PIFR1: byte absolute $A2;  // PSC1 Interrupt Flag Register
  PIM1: byte absolute $A3;  // PSC1 Interrupt Mask Register
  PIFR2: byte absolute $A4;  // PSC2 Interrupt Flag Register
  PIM2: byte absolute $A5;  // PSC2 Interrupt Mask Register
  DACON: byte absolute $AA;  // DAC Control Register
  DAC: word absolute $AB;  // DAC Data Register
  DACL: byte absolute $AB;  // DAC Data Register
  DACH: byte absolute $AC;  // DAC Data Register;
  AC0CON: byte absolute $AD;  // Analog Comparator 0 Control Register
  AC1CON: byte absolute $AE;  // Analog Comparator 1 Control Register
  AC2CON: byte absolute $AF;  // Analog Comparator 2 Control Register
  UCSRA: byte absolute $C0;  // USART Control and Status register A
  UCSRB: byte absolute $C1;  // USART Control an Status register B
  UCSRC: byte absolute $C2;  // USART Control an Status register C
  UBRR: word absolute $C4;  // USART Baud Rate Register
  UBRRL: byte absolute $C4;  // USART Baud Rate Register
  UBRRH: byte absolute $C5;  // USART Baud Rate Register;
  UDR: byte absolute $C6;  // USART I/O Data Register
  EUCSRA: byte absolute $C8;  // EUSART Control and Status Register A
  EUCSRB: byte absolute $C9;  // EUSART Control Register B
  EUCSRC: byte absolute $CA;  // EUSART Status Register C
  MUBRR: word absolute $CC;  // Manchester Receiver Baud Rate Register
  MUBRRL: byte absolute $CC;  // Manchester Receiver Baud Rate Register
  MUBRRH: byte absolute $CD;  // Manchester Receiver Baud Rate Register;
  EUDR: byte absolute $CE;  // EUSART I/O Data Register
  PSOC0: byte absolute $D0;  // PSC0 Synchro and Output Configuration
  OCR0SA: word absolute $D2;  // Output Compare 0 SA Register
  OCR0SAL: byte absolute $D2;  // Output Compare 0 SA Register
  OCR0SAH: byte absolute $D3;  // Output Compare 0 SA Register;
  OCR0RA: word absolute $D4;  // Output Compare 0 RA Register
  OCR0RAL: byte absolute $D4;  // Output Compare 0 RA Register
  OCR0RAH: byte absolute $D5;  // Output Compare 0 RA Register;
  OCR0SB: word absolute $D6;  // Output Compare 0 SB Register
  OCR0SBL: byte absolute $D6;  // Output Compare 0 SB Register
  OCR0SBH: byte absolute $D7;  // Output Compare 0 SB Register;
  OCR0RB: word absolute $D8;  // Output Compare 0 RB Register
  OCR0RBL: byte absolute $D8;  // Output Compare 0 RB Register
  OCR0RBH: byte absolute $D9;  // Output Compare 0 RB Register;
  PCNF0: byte absolute $DA;  // PSC 0 Configuration Register
  PCTL0: byte absolute $DB;  // PSC 0 Control Register
  PFRC0A: byte absolute $DC;  // PSC 0 Input A Control
  PFRC0B: byte absolute $DD;  // PSC 0 Input B Control
  PICR0: word absolute $DE;  // PSC 0 Input Capture Register 
  PICR0L: byte absolute $DE;  // PSC 0 Input Capture Register 
  PICR0H: byte absolute $DF;  // PSC 0 Input Capture Register ;
  PSOC1: byte absolute $E0;  // PSC1 Synchro and Output Configuration
  OCR1SA: word absolute $E2;  // Output Compare SA Register 
  OCR1SAL: byte absolute $E2;  // Output Compare SA Register 
  OCR1SAH: byte absolute $E3;  // Output Compare SA Register ;
  OCR1RA: word absolute $E4;  // Output Compare RA Register 
  OCR1RAL: byte absolute $E4;  // Output Compare RA Register 
  OCR1RAH: byte absolute $E5;  // Output Compare RA Register ;
  OCR1SB: word absolute $E6;  // Output Compare SB Register 
  OCR1SBL: byte absolute $E6;  // Output Compare SB Register 
  OCR1SBH: byte absolute $E7;  // Output Compare SB Register ;
  OCR1RB: word absolute $E8;  // Output Compare RB Register 
  OCR1RBL: byte absolute $E8;  // Output Compare RB Register 
  OCR1RBH: byte absolute $E9;  // Output Compare RB Register ;
  PCNF1: byte absolute $EA;  // PSC 1 Configuration Register
  PCTL1: byte absolute $EB;  // PSC 1 Control Register
  PFRC1A: byte absolute $EC;  // PSC 1 Input B Control
  PFRC1B: byte absolute $ED;  // PSC 1 Input B Control
  PICR1: word absolute $EE;  // PSC 1 Input Capture Register 
  PICR1L: byte absolute $EE;  // PSC 1 Input Capture Register 
  PICR1H: byte absolute $EF;  // PSC 1 Input Capture Register ;
  PSOC2: byte absolute $F0;  // PSC2 Synchro and Output Configuration
  POM2: byte absolute $F1;  // PSC 2 Output Matrix
  OCR2SA: word absolute $F2;  // Output Compare 2 SA Register
  OCR2SAL: byte absolute $F2;  // Output Compare 2 SA Register
  OCR2SAH: byte absolute $F3;  // Output Compare 2 SA Register;
  OCR2RA: word absolute $F4;  // Output Compare 2 RA Register
  OCR2RAL: byte absolute $F4;  // Output Compare 2 RA Register
  OCR2RAH: byte absolute $F5;  // Output Compare 2 RA Register;
  OCR2SB: word absolute $F6;  // Output Compare 2 SB Register
  OCR2SBL: byte absolute $F6;  // Output Compare 2 SB Register
  OCR2SBH: byte absolute $F7;  // Output Compare 2 SB Register;
  OCR2RB: word absolute $F8;  // Output Compare 2 RB Register
  OCR2RBL: byte absolute $F8;  // Output Compare 2 RB Register
  OCR2RBH: byte absolute $F9;  // Output Compare 2 RB Register;
  PCNF2: byte absolute $FA;  // PSC 2 Configuration Register
  PCTL2: byte absolute $FB;  // PSC 2 Control Register
  PFRC2A: byte absolute $FC;  // PSC 2 Input B Control
  PFRC2B: byte absolute $FD;  // PSC 2 Input B Control
  PICR2: word absolute $FE;  // PSC 2 Input Capture Register 
  PICR2L: byte absolute $FE;  // PSC 2 Input Capture Register 
  PICR2H: byte absolute $FF;  // PSC 2 Input Capture Register ;

const
  // Port B Data Register
  PB0 = $00;
  PB1 = $01;
  PB2 = $02;
  PB3 = $03;
  PB4 = $04;
  PB5 = $05;
  PB6 = $06;
  PB7 = $07;
  // Port C Data Register
  PC0 = $00;
  PC1 = $01;
  PC2 = $02;
  PC3 = $03;
  PC4 = $04;
  PC5 = $05;
  PC6 = $06;
  PC7 = $07;
  // Port D Data Register
  PD0 = $00;
  PD1 = $01;
  PD2 = $02;
  PD3 = $03;
  PD4 = $04;
  PD5 = $05;
  PD6 = $06;
  PD7 = $07;
  // Port E Data Register
  PE0 = $00;
  PE1 = $01;
  PE2 = $02;
  // Timer/Counter0 Interrupt Flag register
  TOV0 = $00;
  OCF0A = $01;
  OCF0B = $02;
  // Timer/Counter Interrupt Flag register
  TOV1 = $00;
  OCF1A = $01;
  OCF1B = $02;
  ICF1 = $05;
  // General Purpose IO Register 3
  GPIOR30 = $00;  // General Purpose IO Register 3 bis
  GPIOR31 = $01;  // General Purpose IO Register 3 bis
  GPIOR32 = $02;  // General Purpose IO Register 3 bis
  GPIOR33 = $03;  // General Purpose IO Register 3 bis
  GPIOR34 = $04;  // General Purpose IO Register 3 bis
  GPIOR35 = $05;  // General Purpose IO Register 3 bis
  GPIOR36 = $06;  // General Purpose IO Register 3 bis
  GPIOR37 = $07;  // General Purpose IO Register 3 bis
  // External Interrupt Flag Register
  INTF0 = $00;  // External Interrupt Flags
  INTF1 = $01;  // External Interrupt Flags
  INTF2 = $02;  // External Interrupt Flags
  INTF3 = $03;  // External Interrupt Flags
  // External Interrupt Mask Register
  INT0 = $00;  // External Interrupt Request Enable
  INT1 = $01;  // External Interrupt Request Enable
  INT2 = $02;  // External Interrupt Request Enable
  INT3 = $03;  // External Interrupt Request Enable
  // General Purpose IO Register 0
  GPIOR00 = $00;
  GPIOR01 = $01;
  GPIOR02 = $02;
  GPIOR03 = $03;
  GPIOR04 = $04;
  GPIOR05 = $05;
  GPIOR06 = $06;
  GPIOR07 = $07;
  // EEPROM Control Register
  EERE = $00;
  EEWE = $01;
  EEMWE = $02;
  EERIE = $03;
  EEPM0 = $04;  // EEPROM Programming Mode
  EEPM1 = $05;  // EEPROM Programming Mode
  // EEPROM Data Register
  EEDR0 = $00;  // EEPROM Data Bits
  EEDR1 = $01;  // EEPROM Data Bits
  EEDR2 = $02;  // EEPROM Data Bits
  EEDR3 = $03;  // EEPROM Data Bits
  EEDR4 = $04;  // EEPROM Data Bits
  EEDR5 = $05;  // EEPROM Data Bits
  EEDR6 = $06;  // EEPROM Data Bits
  EEDR7 = $07;  // EEPROM Data Bits
  // EEPROM Read/Write Access Bytes
  EEAR0 = $00;  // EEPROM Address bytes
  EEAR1 = $01;  // EEPROM Address bytes
  EEAR2 = $02;  // EEPROM Address bytes
  EEAR3 = $03;  // EEPROM Address bytes
  EEAR4 = $04;  // EEPROM Address bytes
  EEAR5 = $05;  // EEPROM Address bytes
  EEAR6 = $06;  // EEPROM Address bytes
  EEAR7 = $07;  // EEPROM Address bytes
  // General Timer/Counter Control Register
  PSR10 = $00;
  PSRSYNC = $00;
  ICPSEL1 = $06;
  TSM = $07;
  // Timer/Counter  Control Register A
  WGM00 = $00;  // Waveform Generation Mode
  WGM01 = $01;  // Waveform Generation Mode
  COM0B0 = $04;  // Compare Output Mode, Fast PWm
  COM0B1 = $05;  // Compare Output Mode, Fast PWm
  COM0A0 = $06;  // Compare Output Mode, Phase Correct PWM Mode
  COM0A1 = $07;  // Compare Output Mode, Phase Correct PWM Mode
  // Timer/Counter Control Register B
  CS00 = $00;  // Clock Select
  CS01 = $01;  // Clock Select
  CS02 = $02;  // Clock Select
  WGM02 = $03;
  FOC0B = $06;
  FOC0A = $07;
  // Timer/Counter0
  TCNT00 = $00;  // Timer Counter 0 value
  TCNT01 = $01;  // Timer Counter 0 value
  TCNT02 = $02;  // Timer Counter 0 value
  TCNT03 = $03;  // Timer Counter 0 value
  TCNT04 = $04;  // Timer Counter 0 value
  TCNT05 = $05;  // Timer Counter 0 value
  TCNT06 = $06;  // Timer Counter 0 value
  TCNT07 = $07;  // Timer Counter 0 value
  // Timer/Counter0 Output Compare Register
  OCR0A0 = $00;  // Timer/Counter0 Output Compare A
  OCR0A1 = $01;  // Timer/Counter0 Output Compare A
  OCR0A2 = $02;  // Timer/Counter0 Output Compare A
  OCR0A3 = $03;  // Timer/Counter0 Output Compare A
  OCR0A4 = $04;  // Timer/Counter0 Output Compare A
  OCR0A5 = $05;  // Timer/Counter0 Output Compare A
  OCR0A6 = $06;  // Timer/Counter0 Output Compare A
  OCR0A7 = $07;  // Timer/Counter0 Output Compare A
  // Timer/Counter0 Output Compare Register
  OCR0B0 = $00;  // Timer/Counter0 Output Compare B
  OCR0B1 = $01;  // Timer/Counter0 Output Compare B
  OCR0B2 = $02;  // Timer/Counter0 Output Compare B
  OCR0B3 = $03;  // Timer/Counter0 Output Compare B
  OCR0B4 = $04;  // Timer/Counter0 Output Compare B
  OCR0B5 = $05;  // Timer/Counter0 Output Compare B
  OCR0B6 = $06;  // Timer/Counter0 Output Compare B
  OCR0B7 = $07;  // Timer/Counter0 Output Compare B
  // PLL Control And Status Register
  PLOCK = $00;
  PLLE = $01;
  PLLF = $02;
  // SPI Control Register
  SPR0 = $00;  // SPI Clock Rate Selects
  SPR1 = $01;  // SPI Clock Rate Selects
  CPHA = $02;
  CPOL = $03;
  MSTR = $04;
  DORD = $05;
  SPE = $06;
  SPIE = $07;
  // SPI Status Register
  SPI2X = $00;
  WCOL = $06;
  SPIF = $07;
  // SPI Data Register
  SPD0 = $00;  // SPI Data bits
  SPD1 = $01;  // SPI Data bits
  SPD2 = $02;  // SPI Data bits
  SPD3 = $03;  // SPI Data bits
  SPD4 = $04;  // SPI Data bits
  SPD5 = $05;  // SPI Data bits
  SPD6 = $06;  // SPI Data bits
  SPD7 = $07;  // SPI Data bits
  // Analog Comparator Status Register
  AC0O = $00;
  AC1O = $01;
  AC2O = $02;
  AC0IF = $04;
  AC1IF = $05;
  AC2IF = $06;
  ACCKDIV = $07;
  // Sleep Mode Control Register
  SE = $00;
  SM0 = $01;  // Sleep Mode Select bits
  SM1 = $02;  // Sleep Mode Select bits
  SM2 = $03;  // Sleep Mode Select bits
  // MCU Status Register
  PORF = $00;
  EXTRF = $01;
  BORF = $02;
  WDRF = $03;
  // MCU Control Register
  IVCE = $00;
  IVSEL = $01;
  PUD = $04;
  SPIPS = $07;
  // Store Program Memory Control Register
  SPMEN = $00;
  PGERS = $01;
  PGWRT = $02;
  BLBSET = $03;
  RWWSRE = $04;
  RWWSB = $06;
  SPMIE = $07;
  // Status Register
  C = $00;
  Z = $01;
  N = $02;
  V = $03;
  S = $04;
  H = $05;
  T = $06;
  I = $07;
  // Watchdog Timer Control Register
  WDE = $03;
  WDCE = $04;
  WDP0 = $00;  // Watchdog Timer Prescaler Bits
  WDP1 = $01;  // Watchdog Timer Prescaler Bits
  WDP2 = $02;  // Watchdog Timer Prescaler Bits
  WDP3 = $05;  // Watchdog Timer Prescaler Bits
  WDIE = $06;
  WDIF = $07;
  CLKPS0 = $00;
  CLKPS1 = $01;
  CLKPS2 = $02;
  CLKPS3 = $03;
  CLKPCE = $07;
  // Power Reduction Register
  PRADC = $00;
  PRUSART0 = $01;
  PRSPI = $02;
  PRTIM0 = $03;
  PRTIM1 = $04;
  PRPSC0 = $05;
  PRPSC1 = $06;
  PRPSC2 = $07;
  // Oscillator Calibration Value
  OSCCAL0 = $00;  // Oscillator Calibration 
  OSCCAL1 = $01;  // Oscillator Calibration 
  OSCCAL2 = $02;  // Oscillator Calibration 
  OSCCAL3 = $03;  // Oscillator Calibration 
  OSCCAL4 = $04;  // Oscillator Calibration 
  OSCCAL5 = $05;  // Oscillator Calibration 
  OSCCAL6 = $06;  // Oscillator Calibration 
  OSCCAL7 = $07;  // Oscillator Calibration 
  // External Interrupt Control Register A
  ISC00 = $00;  // External Interrupt Sense Control Bit
  ISC01 = $01;  // External Interrupt Sense Control Bit
  ISC10 = $02;  // External Interrupt Sense Control Bit
  ISC11 = $03;  // External Interrupt Sense Control Bit
  ISC20 = $04;  // External Interrupt Sense Control Bit
  ISC21 = $05;  // External Interrupt Sense Control Bit
  ISC30 = $06;  // External Interrupt Sense Control Bit
  ISC31 = $07;  // External Interrupt Sense Control Bit
  // Timer/Counter0 Interrupt Mask Register
  TOIE0 = $00;
  OCIE0A = $01;
  OCIE0B = $02;
  // Timer/Counter Interrupt Mask Register
  TOIE1 = $00;
  OCIE1A = $01;
  OCIE1B = $02;
  ICIE1 = $05;
  AMP0TS0 = $00;
  AMP0TS1 = $01;
  AMP0G0 = $04;
  AMP0G1 = $05;
  AMP0IS = $06;
  AMP0EN = $07;
  AMP1TS0 = $00;
  AMP1TS1 = $01;
  AMP1G0 = $04;
  AMP1G1 = $05;
  AMP1IS = $06;
  AMP1EN = $07;
  // ADC Data Register Bytes
  ADC0 = $00;  // ADC Data Register
  ADC1 = $01;  // ADC Data Register
  ADC2 = $02;  // ADC Data Register
  ADC3 = $03;  // ADC Data Register
  ADC4 = $04;  // ADC Data Register
  ADC5 = $05;  // ADC Data Register
  ADC6 = $06;  // ADC Data Register
  ADC7 = $07;  // ADC Data Register
  // The ADC Control and Status register
  ADPS0 = $00;  // ADC  Prescaler Select Bits
  ADPS1 = $01;  // ADC  Prescaler Select Bits
  ADPS2 = $02;  // ADC  Prescaler Select Bits
  ADIE = $03;
  ADIF = $04;
  ADATE = $05;
  ADSC = $06;
  ADEN = $07;
  // ADC Control and Status Register B
  ADTS0 = $00;  // ADC Auto Trigger Source
  ADTS1 = $01;  // ADC Auto Trigger Source
  ADTS2 = $02;  // ADC Auto Trigger Source
  ADTS3 = $03;  // ADC Auto Trigger Source
  ADASCR = $04;
  ADHSM = $07;
  // The ADC multiplexer Selection Register
  MUX0 = $00;  // Analog Channel and Gain Selection Bits
  MUX1 = $01;  // Analog Channel and Gain Selection Bits
  MUX2 = $02;  // Analog Channel and Gain Selection Bits
  MUX3 = $03;  // Analog Channel and Gain Selection Bits
  ADLAR = $05;
  REFS0 = $06;  // Reference Selection Bits
  REFS1 = $07;  // Reference Selection Bits
  // Digital Input Disable Register 0
  ADC0D = $00;
  ADC1D = $01;
  ADC2D = $02;
  ADC3D = $03;
  ADC4D = $04;
  ADC5D = $05;
  ADC6D = $06;
  ADC7D = $07;
  // Digital Input Disable Register 1
  ADC8D = $00;
  ADC9D = $01;
  ADC10D = $02;
  AMP0ND = $03;
  AMP0PD = $04;
  ACMP0D = $05;
  // Timer/Counter1 Control Register A
  WGM10 = $00;  // Waveform Generation Mode
  WGM11 = $01;  // Waveform Generation Mode
  COM1B0 = $04;  // Compare Output Mode 1B, bits
  COM1B1 = $05;  // Compare Output Mode 1B, bits
  COM1A0 = $06;  // Compare Output Mode 1A, bits
  COM1A1 = $07;  // Compare Output Mode 1A, bits
  // Timer/Counter1 Control Register B
  CS10 = $00;  // Prescaler source of Timer/Counter 1
  CS11 = $01;  // Prescaler source of Timer/Counter 1
  CS12 = $02;  // Prescaler source of Timer/Counter 1
  ICES1 = $06;
  ICNC1 = $07;
  // Timer/Counter1 Control Register C
  FOC1B = $06;
  FOC1A = $07;
  // Timer/Counter1 Bytes
  TCNT10 = $00;  // Timer/Counter1
  TCNT11 = $01;  // Timer/Counter1
  TCNT12 = $02;  // Timer/Counter1
  TCNT13 = $03;  // Timer/Counter1
  TCNT14 = $04;  // Timer/Counter1
  TCNT15 = $05;  // Timer/Counter1
  TCNT16 = $06;  // Timer/Counter1
  TCNT17 = $07;  // Timer/Counter1
  // Timer/Counter1 Input Capture Register Bytes
  ICR10 = $00;  // Timer/Counter1 Input Capture
  ICR11 = $01;  // Timer/Counter1 Input Capture
  ICR12 = $02;  // Timer/Counter1 Input Capture
  ICR13 = $03;  // Timer/Counter1 Input Capture
  ICR14 = $04;  // Timer/Counter1 Input Capture
  ICR15 = $05;  // Timer/Counter1 Input Capture
  ICR16 = $06;  // Timer/Counter1 Input Capture
  ICR17 = $07;  // Timer/Counter1 Input Capture
  // Timer/Counter1 Output Compare Register Bytes
  OCR1A0 = $00;  // Timer/Counter1 Output Capture A
  OCR1A1 = $01;  // Timer/Counter1 Output Capture A
  OCR1A2 = $02;  // Timer/Counter1 Output Capture A
  OCR1A3 = $03;  // Timer/Counter1 Output Capture A
  OCR1A4 = $04;  // Timer/Counter1 Output Capture A
  OCR1A5 = $05;  // Timer/Counter1 Output Capture A
  OCR1A6 = $06;  // Timer/Counter1 Output Capture A
  OCR1A7 = $07;  // Timer/Counter1 Output Capture A
  // Timer/Counter1 Output Compare Register Bytes
  OCR1B0 = $00;  // Timer/Counter1 Output Capture B
  OCR1B1 = $01;  // Timer/Counter1 Output Capture B
  OCR1B2 = $02;  // Timer/Counter1 Output Capture B
  OCR1B3 = $03;  // Timer/Counter1 Output Capture B
  OCR1B4 = $04;  // Timer/Counter1 Output Capture B
  OCR1B5 = $05;  // Timer/Counter1 Output Capture B
  OCR1B6 = $06;  // Timer/Counter1 Output Capture B
  OCR1B7 = $07;  // Timer/Counter1 Output Capture B
  // PSC0 Interrupt Flag Register
  PEOP0 = $00;
  PRN00 = $01;  // Ramp Number
  PRN01 = $02;  // Ramp Number
  PEV0A = $03;
  PEV0B = $04;
  PSEI0 = $05;
  // PSC0 Interrupt Mask Register
  PEOPE0 = $00;
  PEVE0A = $03;
  PEVE0B = $04;
  PSEIE0 = $05;
  // PSC1 Interrupt Flag Register
  PEOP1 = $00;
  PRN10 = $01;  // Ramp Number
  PRN11 = $02;  // Ramp Number
  PEV1A = $03;
  PEV1B = $04;
  PSEI1 = $05;
  // PSC1 Interrupt Mask Register
  PEOPE1 = $00;
  PEVE1A = $03;
  PEVE1B = $04;
  PSEIE1 = $05;
  // PSC2 Interrupt Flag Register
  PEOP2 = $00;
  PRN20 = $01;  // Ramp Number
  PRN21 = $02;  // Ramp Number
  PEV2A = $03;
  PEV2B = $04;
  PSEI2 = $05;
  // PSC2 Interrupt Mask Register
  PEOPE2 = $00;
  PEVE2A = $03;
  PEVE2B = $04;
  PSEIE2 = $05;
  // DAC Control Register
  DAEN = $00;
  DAOE = $01;
  DALA = $02;
  DATS0 = $04;  // DAC Trigger Selection Bits
  DATS1 = $05;  // DAC Trigger Selection Bits
  DATS2 = $06;  // DAC Trigger Selection Bits
  DAATE = $07;
  // DAC Data Register
  DAC0 = $00;  // DAC Data Register Bits
  DAC1 = $01;  // DAC Data Register Bits
  DAC2 = $02;  // DAC Data Register Bits
  DAC3 = $03;  // DAC Data Register Bits
  DAC4 = $04;  // DAC Data Register Bits
  DAC5 = $05;  // DAC Data Register Bits
  DAC6 = $06;  // DAC Data Register Bits
  DAC7 = $07;  // DAC Data Register Bits
  // Analog Comparator 0 Control Register
  AC0M0 = $00;  // Analog Comparator 0 Multiplexer Register
  AC0M1 = $01;  // Analog Comparator 0 Multiplexer Register
  AC0M2 = $02;  // Analog Comparator 0 Multiplexer Register
  AC0IS0 = $04;  // Analog Comparator 0  Interrupt Select Bit
  AC0IS1 = $05;  // Analog Comparator 0  Interrupt Select Bit
  AC0IE = $06;
  AC0EN = $07;
  // Analog Comparator 1 Control Register
  AC1M0 = $00;  // Analog Comparator 1 Multiplexer Register
  AC1M1 = $01;  // Analog Comparator 1 Multiplexer Register
  AC1M2 = $02;  // Analog Comparator 1 Multiplexer Register
  AC1ICE = $03;
  AC1IS0 = $04;  // Analog Comparator 1  Interrupt Select Bit
  AC1IS1 = $05;  // Analog Comparator 1  Interrupt Select Bit
  AC1IE = $06;
  AC1EN = $07;
  // Analog Comparator 2 Control Register
  AC2M0 = $00;  // Analog Comparator 2 Multiplexer Register
  AC2M1 = $01;  // Analog Comparator 2 Multiplexer Register
  AC2M2 = $02;  // Analog Comparator 2 Multiplexer Register
  AC2IS0 = $04;  // Analog Comparator 2  Interrupt Select Bit
  AC2IS1 = $05;  // Analog Comparator 2  Interrupt Select Bit
  AC2IE = $06;
  AC2EN = $07;
  // USART Control and Status register A
  MPCM = $00;
  U2X = $01;
  UPE = $02;
  DOR = $03;
  FE = $04;
  UDRE = $05;
  TXC = $06;
  RXC = $07;
  // USART Control an Status register B
  TXB8 = $00;
  RXB8 = $01;
  UCSZ2 = $02;
  TXEN = $03;
  RXEN = $04;
  UDRIE = $05;
  TXCIE = $06;
  RXCIE = $07;
  // USART Control an Status register C
  UCPOL = $00;
  UCSZ0 = $01;  // Character Size Bits
  UCSZ1 = $02;  // Character Size Bits
  USBS = $03;
  UPM0 = $04;  // Parity Mode Bits
  UPM1 = $05;  // Parity Mode Bits
  UMSEL0 = $06;
  // USART Baud Rate Register
  UBRR0 = $00;  // USART Baud Rate Register Bits
  UBRR1 = $01;  // USART Baud Rate Register Bits
  UBRR2 = $02;  // USART Baud Rate Register Bits
  UBRR3 = $03;  // USART Baud Rate Register Bits
  UBRR4 = $04;  // USART Baud Rate Register Bits
  UBRR5 = $05;  // USART Baud Rate Register Bits
  UBRR6 = $06;  // USART Baud Rate Register Bits
  UBRR7 = $07;  // USART Baud Rate Register Bits
  // USART I/O Data Register
  UDR0 = $00;  // USART I/O Data
  UDR1 = $01;  // USART I/O Data
  UDR2 = $02;  // USART I/O Data
  UDR3 = $03;  // USART I/O Data
  UDR4 = $04;  // USART I/O Data
  UDR5 = $05;  // USART I/O Data
  UDR6 = $06;  // USART I/O Data
  UDR7 = $07;  // USART I/O Data
  // EUSART Control and Status Register A
  URxS0 = $00;  // EUSART Control and Status Register A Bits
  URxS1 = $01;  // EUSART Control and Status Register A Bits
  URxS2 = $02;  // EUSART Control and Status Register A Bits
  URxS3 = $03;  // EUSART Control and Status Register A Bits
  UTxS0 = $04;  // EUSART Control and Status Register A Bits
  UTxS1 = $05;  // EUSART Control and Status Register A Bits
  UTxS2 = $06;  // EUSART Control and Status Register A Bits
  UTxS3 = $07;  // EUSART Control and Status Register A Bits
  // EUSART Control Register B
  BODR = $00;
  EMCH = $01;
  EUSBS = $03;
  EUSART = $04;
  // EUSART Status Register C
  STP0 = $00;  // Stop Bits
  STP1 = $01;  // Stop Bits
  F1617 = $02;
  FEM = $03;
  // Manchester Receiver Baud Rate Register
  MUBRR0 = $00;  // Manchester Receiver Baud Rate Register Bits
  MUBRR1 = $01;  // Manchester Receiver Baud Rate Register Bits
  MUBRR2 = $02;  // Manchester Receiver Baud Rate Register Bits
  MUBRR3 = $03;  // Manchester Receiver Baud Rate Register Bits
  MUBRR4 = $04;  // Manchester Receiver Baud Rate Register Bits
  MUBRR5 = $05;  // Manchester Receiver Baud Rate Register Bits
  MUBRR6 = $06;  // Manchester Receiver Baud Rate Register Bits
  MUBRR7 = $07;  // Manchester Receiver Baud Rate Register Bits
  // EUSART I/O Data Register
  EUDR0 = $00;  // EUSART Extended data bits
  EUDR1 = $01;  // EUSART Extended data bits
  EUDR2 = $02;  // EUSART Extended data bits
  EUDR3 = $03;  // EUSART Extended data bits
  EUDR4 = $04;  // EUSART Extended data bits
  EUDR5 = $05;  // EUSART Extended data bits
  EUDR6 = $06;  // EUSART Extended data bits
  EUDR7 = $07;  // EUSART Extended data bits
  // PSC0 Synchro and Output Configuration
  POEN0A = $00;
  POEN0B = $02;
  PSYNC00 = $04;  // Synchronization Out for ADC Selection
  PSYNC01 = $05;  // Synchronization Out for ADC Selection
  // Output Compare 0 SA Register
  OCR0SA0 = $00;  // Output Compare SA
  OCR0SA1 = $01;  // Output Compare SA
  OCR0SA2 = $02;  // Output Compare SA
  OCR0SA3 = $03;  // Output Compare SA
  OCR0SA4 = $04;  // Output Compare SA
  OCR0SA5 = $05;  // Output Compare SA
  OCR0SA6 = $06;  // Output Compare SA
  OCR0SA7 = $07;  // Output Compare SA
  // Output Compare 0 RA Register
  OCR0RA0 = $00;  // Output Compare RA
  OCR0RA1 = $01;  // Output Compare RA
  OCR0RA2 = $02;  // Output Compare RA
  OCR0RA3 = $03;  // Output Compare RA
  OCR0RA4 = $04;  // Output Compare RA
  OCR0RA5 = $05;  // Output Compare RA
  OCR0RA6 = $06;  // Output Compare RA
  OCR0RA7 = $07;  // Output Compare RA
  // Output Compare 0 SB Register
  OCR0SB0 = $00;  // Output Compare SB
  OCR0SB1 = $01;  // Output Compare SB
  OCR0SB2 = $02;  // Output Compare SB
  OCR0SB3 = $03;  // Output Compare SB
  OCR0SB4 = $04;  // Output Compare SB
  OCR0SB5 = $05;  // Output Compare SB
  OCR0SB6 = $06;  // Output Compare SB
  OCR0SB7 = $07;  // Output Compare SB
  // Output Compare 0 RB Register
  OCR0RB0 = $00;  // Output Compare RB
  OCR0RB1 = $01;  // Output Compare RB
  OCR0RB2 = $02;  // Output Compare RB
  OCR0RB3 = $03;  // Output Compare RB
  OCR0RB4 = $04;  // Output Compare RB
  OCR0RB5 = $05;  // Output Compare RB
  OCR0RB6 = $06;  // Output Compare RB
  OCR0RB7 = $07;  // Output Compare RB
  // PSC 0 Configuration Register
  PCLKSEL0 = $01;
  POP0 = $02;
  PMODE00 = $03;  // PSC 0 Mode
  PMODE01 = $04;  // PSC 0 Mode
  PLOCK0 = $05;
  PALOCK0 = $06;
  PFIFTY0 = $07;
  // PSC 0 Control Register
  PRUN0 = $00;
  PCCYC0 = $01;
  PARUN0 = $02;
  PAOC0A = $03;
  PAOC0B = $04;
  PBFM0 = $05;
  PPRE00 = $06;  // PSC 0 Prescaler Selects
  PPRE01 = $07;  // PSC 0 Prescaler Selects
  // PSC 0 Input A Control
  PRFM0A0 = $00;  // PSC 0 Retrigger and Fault Mode for Part A
  PRFM0A1 = $01;  // PSC 0 Retrigger and Fault Mode for Part A
  PRFM0A2 = $02;  // PSC 0 Retrigger and Fault Mode for Part A
  PRFM0A3 = $03;  // PSC 0 Retrigger and Fault Mode for Part A
  PFLTE0A = $04;
  PELEV0A = $05;
  PISEL0A = $06;
  PCAE0A = $07;
  // PSC 0 Input B Control
  PRFM0B0 = $00;  // PSC 0 Retrigger and Fault Mode for Part B
  PRFM0B1 = $01;  // PSC 0 Retrigger and Fault Mode for Part B
  PRFM0B2 = $02;  // PSC 0 Retrigger and Fault Mode for Part B
  PRFM0B3 = $03;  // PSC 0 Retrigger and Fault Mode for Part B
  PFLTE0B = $04;
  PELEV0B = $05;
  PISEL0B = $06;
  PCAE0B = $07;
  // PSC 0 Input Capture Register 
  PICR00 = $00;  // PSC 0 Input Capture Bytes
  PICR01 = $01;  // PSC 0 Input Capture Bytes
  PICR02 = $02;  // PSC 0 Input Capture Bytes
  PICR03 = $03;  // PSC 0 Input Capture Bytes
  PICR04 = $04;  // PSC 0 Input Capture Bytes
  PICR05 = $05;  // PSC 0 Input Capture Bytes
  PICR06 = $06;  // PSC 0 Input Capture Bytes
  PICR07 = $07;  // PSC 0 Input Capture Bytes
  // PSC1 Synchro and Output Configuration
  POEN1A = $00;
  POEN1B = $02;
  PSYNC1_0 = $04;  // Synchronization Out for ADC Selection
  PSYNC1_1 = $05;  // Synchronization Out for ADC Selection
  // PSC 1 Configuration Register
  PCLKSEL1 = $01;
  POP1 = $02;
  PMODE10 = $03;  // PSC 1 Mode
  PMODE11 = $04;  // PSC 1 Mode
  PLOCK1 = $05;
  PALOCK1 = $06;
  PFIFTY1 = $07;
  // PSC 1 Control Register
  PRUN1 = $00;
  PCCYC1 = $01;
  PARUN1 = $02;
  PAOC1A = $03;
  PAOC1B = $04;
  PBFM1 = $05;
  PPRE10 = $06;  // PSC 1 Prescaler Selects
  PPRE11 = $07;  // PSC 1 Prescaler Selects
  // PSC 1 Input B Control
  PRFM1A0 = $00;  // PSC 1 Retrigger and Fault Mode for Part A
  PRFM1A1 = $01;  // PSC 1 Retrigger and Fault Mode for Part A
  PRFM1A2 = $02;  // PSC 1 Retrigger and Fault Mode for Part A
  PRFM1A3 = $03;  // PSC 1 Retrigger and Fault Mode for Part A
  PFLTE1A = $04;
  PELEV1A = $05;
  PISEL1A = $06;
  PCAE1A = $07;
  // PSC 1 Input B Control
  PRFM1B0 = $00;  // PSC 1 Retrigger and Fault Mode for Part B
  PRFM1B1 = $01;  // PSC 1 Retrigger and Fault Mode for Part B
  PRFM1B2 = $02;  // PSC 1 Retrigger and Fault Mode for Part B
  PRFM1B3 = $03;  // PSC 1 Retrigger and Fault Mode for Part B
  PFLTE1B = $04;
  PELEV1B = $05;
  PISEL1B = $06;
  PCAE1B = $07;
  // PSC2 Synchro and Output Configuration
  POEN2A = $00;
  POEN2C = $01;
  POEN2B = $02;
  POEN2D = $03;
  PSYNC2_0 = $04;  // Synchronization Out for ADC Selection
  PSYNC2_1 = $05;  // Synchronization Out for ADC Selection
  POS22 = $06;  // PSC 2 Output 23 Select
  POS23 = $07;  // PSC 2 Output 23 Select
  // PSC 2 Output Matrix
  POMV2A0 = $00;  // Output Matrix Output A Ramps
  POMV2A1 = $01;  // Output Matrix Output A Ramps
  POMV2A2 = $02;  // Output Matrix Output A Ramps
  POMV2A3 = $03;  // Output Matrix Output A Ramps
  POMV2B0 = $04;  // Output Matrix Output B Ramps
  POMV2B1 = $05;  // Output Matrix Output B Ramps
  POMV2B2 = $06;  // Output Matrix Output B Ramps
  POMV2B3 = $07;  // Output Matrix Output B Ramps
  // Output Compare 2 SA Register
  OCR2SA0 = $00;  // Output Compare SA
  OCR2SA1 = $01;  // Output Compare SA
  OCR2SA2 = $02;  // Output Compare SA
  OCR2SA3 = $03;  // Output Compare SA
  OCR2SA4 = $04;  // Output Compare SA
  OCR2SA5 = $05;  // Output Compare SA
  OCR2SA6 = $06;  // Output Compare SA
  OCR2SA7 = $07;  // Output Compare SA
  // Output Compare 2 RA Register
  OCR2RA0 = $00;  // Output Compare RA
  OCR2RA1 = $01;  // Output Compare RA
  OCR2RA2 = $02;  // Output Compare RA
  OCR2RA3 = $03;  // Output Compare RA
  OCR2RA4 = $04;  // Output Compare RA
  OCR2RA5 = $05;  // Output Compare RA
  OCR2RA6 = $06;  // Output Compare RA
  OCR2RA7 = $07;  // Output Compare RA
  // Output Compare 2 SB Register
  OCR2SB0 = $00;  // Output Compare SB
  OCR2SB1 = $01;  // Output Compare SB
  OCR2SB2 = $02;  // Output Compare SB
  OCR2SB3 = $03;  // Output Compare SB
  OCR2SB4 = $04;  // Output Compare SB
  OCR2SB5 = $05;  // Output Compare SB
  OCR2SB6 = $06;  // Output Compare SB
  OCR2SB7 = $07;  // Output Compare SB
  // Output Compare 2 RB Register
  OCR2RB0 = $00;  // Output Compare RB
  OCR2RB1 = $01;  // Output Compare RB
  OCR2RB2 = $02;  // Output Compare RB
  OCR2RB3 = $03;  // Output Compare RB
  OCR2RB4 = $04;  // Output Compare RB
  OCR2RB5 = $05;  // Output Compare RB
  OCR2RB6 = $06;  // Output Compare RB
  OCR2RB7 = $07;  // Output Compare RB
  // PSC 2 Configuration Register
  POME2 = $00;
  PCLKSEL2 = $01;
  POP2 = $02;
  PMODE20 = $03;  // PSC 2 Mode
  PMODE21 = $04;  // PSC 2 Mode
  PLOCK2 = $05;
  PALOCK2 = $06;
  PFIFTY2 = $07;
  // PSC 2 Control Register
  PRUN2 = $00;
  PCCYC2 = $01;
  PARUN2 = $02;
  PAOC2A = $03;
  PAOC2B = $04;
  PBFM2 = $05;
  PPRE20 = $06;  // PSC 2 Prescaler Selects
  PPRE21 = $07;  // PSC 2 Prescaler Selects
  // PSC 2 Input B Control
  PRFM2A0 = $00;  // PSC 2 Retrigger and Fault Mode for Part A
  PRFM2A1 = $01;  // PSC 2 Retrigger and Fault Mode for Part A
  PRFM2A2 = $02;  // PSC 2 Retrigger and Fault Mode for Part A
  PRFM2A3 = $03;  // PSC 2 Retrigger and Fault Mode for Part A
  PFLTE2A = $04;
  PELEV2A = $05;
  PISEL2A = $06;
  PCAE2A = $07;
  // PSC 2 Input B Control
  PRFM2B0 = $00;  // PSC 2 Retrigger and Fault Mode for Part B
  PRFM2B1 = $01;  // PSC 2 Retrigger and Fault Mode for Part B
  PRFM2B2 = $02;  // PSC 2 Retrigger and Fault Mode for Part B
  PRFM2B3 = $03;  // PSC 2 Retrigger and Fault Mode for Part B
  PFLTE2B = $04;
  PELEV2B = $05;
  PISEL2B = $06;
  PCAE2B = $07;
  // PSC 2 Input Capture Register 
  PICR20 = $00;  // PSC 2 Input Capture Bytes
  PICR21 = $01;  // PSC 2 Input Capture Bytes
  PICR22 = $02;  // PSC 2 Input Capture Bytes
  PICR23 = $03;  // PSC 2 Input Capture Bytes
  PICR24 = $04;  // PSC 2 Input Capture Bytes
  PICR25 = $05;  // PSC 2 Input Capture Bytes
  PICR26 = $06;  // PSC 2 Input Capture Bytes
  PICR27 = $07;  // PSC 2 Input Capture Bytes


implementation
{$define RELBRANCHES}
{$i avrcommon.inc}

procedure PSC2_CAPT_ISR; external name 'PSC2_CAPT_ISR'; // Interrupt 1 PSC2 Capture Event
procedure PSC2_EC_ISR; external name 'PSC2_EC_ISR'; // Interrupt 2 PSC2 End Cycle
procedure PSC1_CAPT_ISR; external name 'PSC1_CAPT_ISR'; // Interrupt 3 PSC1 Capture Event
procedure PSC1_EC_ISR; external name 'PSC1_EC_ISR'; // Interrupt 4 PSC1 End Cycle
procedure PSC0_CAPT_ISR; external name 'PSC0_CAPT_ISR'; // Interrupt 5 PSC0 Capture Event
procedure PSC0_EC_ISR; external name 'PSC0_EC_ISR'; // Interrupt 6 PSC0 End Cycle
procedure ANALOG_COMP_0_ISR; external name 'ANALOG_COMP_0_ISR'; // Interrupt 7 Analog Comparator 0
procedure ANALOG_COMP_1_ISR; external name 'ANALOG_COMP_1_ISR'; // Interrupt 8 Analog Comparator 1
procedure ANALOG_COMP_2_ISR; external name 'ANALOG_COMP_2_ISR'; // Interrupt 9 Analog Comparator 2
procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 10 External Interrupt Request 0
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 11 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 12 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 13 Timer/Counter Compare Match B
procedure RESERVED15_ISR; external name 'RESERVED15_ISR'; // Interrupt 14 
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 15 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 16 Timer/Counter0 Compare Match A
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 17 Timer/Counter0 Overflow
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 18 ADC Conversion Complete
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 19 External Interrupt Request 1
procedure SPI_STC_ISR; external name 'SPI_STC_ISR'; // Interrupt 20 SPI Serial Transfer Complete
procedure USART_RX_ISR; external name 'USART_RX_ISR'; // Interrupt 21 USART, Rx Complete
procedure USART_UDRE_ISR; external name 'USART_UDRE_ISR'; // Interrupt 22 USART Data Register Empty
procedure USART_TX_ISR; external name 'USART_TX_ISR'; // Interrupt 23 USART, Tx Complete
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 24 External Interrupt Request 2
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 25 Watchdog Timeout Interrupt
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 26 EEPROM Ready
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 27 Timer Counter 0 Compare Match B
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 28 External Interrupt Request 3
procedure RESERVED30_ISR; external name 'RESERVED30_ISR'; // Interrupt 29 
procedure RESERVED31_ISR; external name 'RESERVED31_ISR'; // Interrupt 30 
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 31 Store Program Memory Read

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
asm
  rjmp __dtors_end
  rjmp PSC2_CAPT_ISR
  rjmp PSC2_EC_ISR
  rjmp PSC1_CAPT_ISR
  rjmp PSC1_EC_ISR
  rjmp PSC0_CAPT_ISR
  rjmp PSC0_EC_ISR
  rjmp ANALOG_COMP_0_ISR
  rjmp ANALOG_COMP_1_ISR
  rjmp ANALOG_COMP_2_ISR
  rjmp INT0_ISR
  rjmp TIMER1_CAPT_ISR
  rjmp TIMER1_COMPA_ISR
  rjmp TIMER1_COMPB_ISR
  rjmp RESERVED15_ISR
  rjmp TIMER1_OVF_ISR
  rjmp TIMER0_COMPA_ISR
  rjmp TIMER0_OVF_ISR
  rjmp ADC_ISR
  rjmp INT1_ISR
  rjmp SPI_STC_ISR
  rjmp USART_RX_ISR
  rjmp USART_UDRE_ISR
  rjmp USART_TX_ISR
  rjmp INT2_ISR
  rjmp WDT_ISR
  rjmp EE_READY_ISR
  rjmp TIMER0_COMPB_ISR
  rjmp INT3_ISR
  rjmp RESERVED30_ISR
  rjmp RESERVED31_ISR
  rjmp SPM_READY_ISR

  .weak PSC2_CAPT_ISR
  .weak PSC2_EC_ISR
  .weak PSC1_CAPT_ISR
  .weak PSC1_EC_ISR
  .weak PSC0_CAPT_ISR
  .weak PSC0_EC_ISR
  .weak ANALOG_COMP_0_ISR
  .weak ANALOG_COMP_1_ISR
  .weak ANALOG_COMP_2_ISR
  .weak INT0_ISR
  .weak TIMER1_CAPT_ISR
  .weak TIMER1_COMPA_ISR
  .weak TIMER1_COMPB_ISR
  .weak RESERVED15_ISR
  .weak TIMER1_OVF_ISR
  .weak TIMER0_COMPA_ISR
  .weak TIMER0_OVF_ISR
  .weak ADC_ISR
  .weak INT1_ISR
  .weak SPI_STC_ISR
  .weak USART_RX_ISR
  .weak USART_UDRE_ISR
  .weak USART_TX_ISR
  .weak INT2_ISR
  .weak WDT_ISR
  .weak EE_READY_ISR
  .weak TIMER0_COMPB_ISR
  .weak INT3_ISR
  .weak RESERVED30_ISR
  .weak RESERVED31_ISR
  .weak SPM_READY_ISR

  .set PSC2_CAPT_ISR, Default_IRQ_handler
  .set PSC2_EC_ISR, Default_IRQ_handler
  .set PSC1_CAPT_ISR, Default_IRQ_handler
  .set PSC1_EC_ISR, Default_IRQ_handler
  .set PSC0_CAPT_ISR, Default_IRQ_handler
  .set PSC0_EC_ISR, Default_IRQ_handler
  .set ANALOG_COMP_0_ISR, Default_IRQ_handler
  .set ANALOG_COMP_1_ISR, Default_IRQ_handler
  .set ANALOG_COMP_2_ISR, Default_IRQ_handler
  .set INT0_ISR, Default_IRQ_handler
  .set TIMER1_CAPT_ISR, Default_IRQ_handler
  .set TIMER1_COMPA_ISR, Default_IRQ_handler
  .set TIMER1_COMPB_ISR, Default_IRQ_handler
  .set RESERVED15_ISR, Default_IRQ_handler
  .set TIMER1_OVF_ISR, Default_IRQ_handler
  .set TIMER0_COMPA_ISR, Default_IRQ_handler
  .set TIMER0_OVF_ISR, Default_IRQ_handler
  .set ADC_ISR, Default_IRQ_handler
  .set INT1_ISR, Default_IRQ_handler
  .set SPI_STC_ISR, Default_IRQ_handler
  .set USART_RX_ISR, Default_IRQ_handler
  .set USART_UDRE_ISR, Default_IRQ_handler
  .set USART_TX_ISR, Default_IRQ_handler
  .set INT2_ISR, Default_IRQ_handler
  .set WDT_ISR, Default_IRQ_handler
  .set EE_READY_ISR, Default_IRQ_handler
  .set TIMER0_COMPB_ISR, Default_IRQ_handler
  .set INT3_ISR, Default_IRQ_handler
  .set RESERVED30_ISR, Default_IRQ_handler
  .set RESERVED31_ISR, Default_IRQ_handler
  .set SPM_READY_ISR, Default_IRQ_handler
end;

end.

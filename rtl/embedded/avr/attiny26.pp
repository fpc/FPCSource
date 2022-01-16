unit ATtiny26;

{$goto on}

interface

var
  // AD_CONVERTER
  ADMUX : byte absolute $00+$27; // The ADC multiplexer Selection Register
  ADCSR : byte absolute $00+$26; // The ADC Control and Status register
  ADC : word absolute $00+$24; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$24; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$24+1; // ADC Data Register  Bytes
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  // USI
  USIDR : byte absolute $00+$2F; // USI Data Register
  USISR : byte absolute $00+$2E; // USI Status Register
  USICR : byte absolute $00+$2D; // USI Control Register
  // PORTA
  PORTA : byte absolute $00+$3B; // Port A Data Register
  DDRA : byte absolute $00+$3A; // Port A Data Direction Register
  PINA : byte absolute $00+$39; // Port A Input Pins
  // PORTB
  PORTB : byte absolute $00+$38; // Port B Data Register
  DDRB : byte absolute $00+$37; // Port B Data Direction Register
  PINB : byte absolute $00+$36; // Port B Input Pins
  // EEPROM
  EEAR : byte absolute $00+$3E; // EEPROM Read/Write Access
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : byte absolute $00+$5D; // Stack Pointer
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status register
  OSCCAL : byte absolute $00+$51; // Status Register
  // TIMER_COUNTER_0
  TIMSK : byte absolute $00+$59; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$58; // Timer/Counter Interrupt Flag register
  TCCR0 : byte absolute $00+$53; // Timer/Counter0 Control Register
  TCNT0 : byte absolute $00+$52; // Timer Counter 0
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$50; // Timer/Counter Control Register A
  TCCR1B : byte absolute $00+$4F; // Timer/Counter Control Register B
  TCNT1 : byte absolute $00+$4E; // Timer/Counter Register
  OCR1A : byte absolute $00+$4D; // Output Compare Register
  OCR1B : byte absolute $00+$4C; // Output Compare Register
  OCR1C : byte absolute $00+$4B; // Output Compare Register
  PLLCSR : byte absolute $00+$49; // PLL Control and Status Register
  // EXTERNAL_INTERRUPT
  GIMSK : byte absolute $00+$5B; // General Interrupt Mask Register
  GIFR : byte absolute $00+$5A; // General Interrupt Flag register

const
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // Left Adjust Result
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSR
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADFR = 5; // ADC  Free Running Select
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACME = 2; // Analog Comparator Multiplexer Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // USISR
  USISIF = 7; // Start Condition Interrupt Flag
  USIOIF = 6; // Counter Overflow Interrupt Flag
  USIPF = 5; // Stop Condition Flag
  USIDC = 4; // Data Output Collision
  USICNT = 0; // USI Counter Value Bits
  // USICR
  USISIE = 7; // Start Condition Interrupt Enable
  USIOIE = 6; // Counter Overflow Interrupt Enable
  USIWM = 4; // USI Wire Mode Bits
  USICS = 2; // USI Clock Source Select Bits
  USICLK = 1; // Clock Strobe
  USITC = 0; // Toggle Clock Port Pin
  // EECR
  EERIE = 3; // EEProm Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // WDTCR
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  WDP = 0; // Watch Dog Timer Prescaler bits
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
  PUD = 6; // Pull-up Disable
  SE = 5; // Sleep Enable
  SM = 3; // Sleep Mode Select Bits
  ISC0 = 0; // Interrupt Sense Control 0 bits
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-On Reset Flag
  // TIMSK
  TOIE0 = 1; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR
  TOV0 = 1; // Timer/Counter0 Overflow Flag
  // TCCR0
  PSR0 = 3; // Prescaler Reset Timer/Counter0
  CS0 = 0; // Clock Select0 bits
  // TCCR1A
  COM1A = 6; // Comparator A Output Mode Bits
  COM1B = 4; // Comparator B Output Mode Bits
  FOC1A = 3; // Force Output Compare Match 1A
  FOC1B = 2; // Force Output Compare Match 1B
  PWM1A = 1; // Pulse Width Modulator A Enable
  PWM1B = 0; // Pulse Width Modulator B Enable
  // TCCR1B
  CTC1 = 7; // Clear Timer/Counter on Compare Match
  PSR1 = 6; // Prescaler Reset Timer/Counter1
  CS1 = 0; // Clock Select Bits
  // TIMSK
  OCIE1A = 6; // Timer/Counter1 Output Compare Interrupt Enable
  OCIE1B = 5; // Timer/Counter1 Output Compare Interrupt Enable
  TOIE1 = 2; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR
  OCF1A = 6; // Timer/Counter1 Output Compare Flag 1A
  OCF1B = 5; // Timer/Counter1 Output Compare Flag 1B
  TOV1 = 2; // Timer/Counter1 Overflow Flag
  // PLLCSR
  PCKE = 2; // PCK Enable
  PLLE = 1; // PLL Enable
  PLOCK = 0; // PLL Lock Detector
  // GIMSK
  INT0 = 6; // External Interrupt Request 0 Enable
  PCIE = 4; // Pin Change Interrupt Enables
  // GIFR
  INTF0 = 6; // External Interrupt Flag 0
  PCIF = 5; // Pin Change Interrupt Flag

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt 0
procedure IO_PINS_ISR; external name 'IO_PINS_ISR'; // Interrupt 2 External Interrupt Request 0
procedure TIMER1_CMPA_ISR; external name 'TIMER1_CMPA_ISR'; // Interrupt 3 Timer/Counter1 Compare Match 1A
procedure TIMER1_CMPB_ISR; external name 'TIMER1_CMPB_ISR'; // Interrupt 4 Timer/Counter1 Compare Match 1B
procedure TIMER1_OVF1_ISR; external name 'TIMER1_OVF1_ISR'; // Interrupt 5 Timer/Counter1 Overflow
procedure TIMER0_OVF0_ISR; external name 'TIMER0_OVF0_ISR'; // Interrupt 6 Timer/Counter0 Overflow
procedure USI_STRT_ISR; external name 'USI_STRT_ISR'; // Interrupt 7 USI Start
procedure USI_OVF_ISR; external name 'USI_OVF_ISR'; // Interrupt 8 USI Overflow
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 9 EEPROM Ready
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 10 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 11 ADC Conversion Complete

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp IO_PINS_ISR
   rjmp TIMER1_CMPA_ISR
   rjmp TIMER1_CMPB_ISR
   rjmp TIMER1_OVF1_ISR
   rjmp TIMER0_OVF0_ISR
   rjmp USI_STRT_ISR
   rjmp USI_OVF_ISR
   rjmp EE_RDY_ISR
   rjmp ANA_COMP_ISR
   rjmp ADC_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak IO_PINS_ISR
   .weak TIMER1_CMPA_ISR
   .weak TIMER1_CMPB_ISR
   .weak TIMER1_OVF1_ISR
   .weak TIMER0_OVF0_ISR
   .weak USI_STRT_ISR
   .weak USI_OVF_ISR
   .weak EE_RDY_ISR
   .weak ANA_COMP_ISR
   .weak ADC_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set IO_PINS_ISR, Default_IRQ_handler
   .set TIMER1_CMPA_ISR, Default_IRQ_handler
   .set TIMER1_CMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF1_ISR, Default_IRQ_handler
   .set TIMER0_OVF0_ISR, Default_IRQ_handler
   .set USI_STRT_ISR, Default_IRQ_handler
   .set USI_OVF_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
 end;

end.

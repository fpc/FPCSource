unit ATtiny13A;

{$goto on}

interface

var
  // AD_CONVERTER
  ADMUX : byte absolute $00+$27; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$26; // The ADC Control and Status register
  ADC : word absolute $00+$24; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$24; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$24+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$23; // ADC Control and Status Register B
  DIDR0 : byte absolute $00+$34; // Digital Input Disable Register 0
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  // EEPROM
  EEAR : byte absolute $00+$3E; // EEPROM Read/Write Access
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // PORTB
  PORTB : byte absolute $00+$38; // Data Register, Port B
  DDRB : byte absolute $00+$37; // Data Direction Register, Port B
  PINB : byte absolute $00+$36; // Input Pins, Port B
  // EXTERNAL_INTERRUPT
  MCUCR : byte absolute $00+$55; // MCU Control Register
  GIMSK : byte absolute $00+$5B; // General Interrupt Mask Register
  GIFR : byte absolute $00+$5A; // General Interrupt Flag register
  PCMSK : byte absolute $00+$35; // Pin Change Enable Mask
  // TIMER_COUNTER_0
  TIMSK0 : byte absolute $00+$59; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$58; // Timer/Counter0 Interrupt Flag register
  OCR0A : byte absolute $00+$56; // Timer/Counter0 Output Compare Register
  TCCR0A : byte absolute $00+$4F; // Timer/Counter  Control Register A
  TCNT0 : byte absolute $00+$52; // Timer/Counter0
  TCCR0B : byte absolute $00+$53; // Timer/Counter Control Register B
  OCR0B : byte absolute $00+$49; // Timer/Counter0 Output Compare Register
  GTCCR : byte absolute $00+$48; // General Timer Conuter Register
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SPL : byte absolute $00+$5D; // Stack Pointer Low Byte
  MCUSR : byte absolute $00+$54; // MCU Status register
  OSCCAL : byte absolute $00+$51; // Oscillator Calibration Register
  CLKPR : byte absolute $00+$46; // Clock Prescale Register
  DWDR : byte absolute $00+$4E; // Debug Wire Data Register
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control and Status Register
  PRR : byte absolute $00+$45; // Power Reduction Register
  BODCR : byte absolute $00+$50; // BOD Control Register

const
  // ADMUX
  REFS0 = 6; // Reference Selection Bit 0
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
  ADTS = 0; // ADC Auto Trigger Sources
  // DIDR0
  ADC0D = 5; // ADC0 Digital input Disable
  ADC2D = 4; // ADC2 Digital input Disable
  ADC3D = 3; // ADC3 Digital input Disable
  ADC1D = 2; // ADC2 Digital input Disable
  // ADCSRB
  ACME = 6; // Analog Comparator Multiplexer Enable
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // DIDR0
  AIN1D = 1; // AIN1 Digital Input Disable
  AIN0D = 0; // AIN0 Digital Input Disable
  // EECR
  EEPM = 4; // 
  EERIE = 3; // EEProm Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // MCUCR
  ISC01 = 1; // Interrupt Sense Control 0 Bit 1
  ISC00 = 0; // Interrupt Sense Control 0 Bit 0
  // GIMSK
  INT0 = 6; // External Interrupt Request 0 Enable
  PCIE = 5; // Pin Change Interrupt Enable
  // GIFR
  INTF0 = 6; // External Interrupt Flag 0
  PCIF = 5; // Pin Change Interrupt Flag
  // TIMSK0
  OCIE0B = 3; // Timer/Counter0 Output Compare Match B Interrupt Enable
  OCIE0A = 2; // Timer/Counter0 Output Compare Match A Interrupt Enable
  TOIE0 = 1; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0B = 3; // Timer/Counter0 Output Compare Flag 0B
  OCF0A = 2; // Timer/Counter0 Output Compare Flag 0A
  TOV0 = 1; // Timer/Counter0 Overflow Flag
  // TCCR0A
  COM0A = 6; // Compare Match Output A Mode
  COM0B = 4; // Compare Match Output B Mode
  WGM0 = 0; // Waveform Generation Mode
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  FOC0B = 6; // Force Output Compare B
  WGM02 = 3; // Waveform Generation Mode
  CS0 = 0; // Clock Select
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSR10 = 0; // Prescaler Reset Timer/Counter0
  // WDTCR
  WDTIF = 7; // Watchdog Timeout Interrupt Flag
  WDTIE = 6; // Watchdog Timeout Interrupt Enable
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
  PUD = 6; // Pull-up Disable
  SE = 5; // Sleep Enable
  SM = 3; // Sleep Mode Select Bits
  ISC0 = 0; // Interrupt Sense Control 0 bits
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-On Reset Flag
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
  // SPMCSR
  CTPB = 4; // Clear Temporary Page Buffer
  RFLB = 3; // Read Fuse and Lock Bits
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store program Memory Enable
  // PRR
  PRTIM0 = 1; // Power Reduction Timer/Counter0
  PRADC = 0; // Power Reduction ADC
  // BODCR
  BPDS = 1; // BOD Power-Down in Power-Down Sleep
  BPDSE = 0; // BOD Power-Down Sleep Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt 0
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 2 External Interrupt Request 0
procedure TIM0_OVF_ISR; external name 'TIM0_OVF_ISR'; // Interrupt 3 Timer/Counter0 Overflow
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 4 EEPROM Ready
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 5 Analog Comparator
procedure TIM0_COMPA_ISR; external name 'TIM0_COMPA_ISR'; // Interrupt 6 Timer/Counter Compare Match A
procedure TIM0_COMPB_ISR; external name 'TIM0_COMPB_ISR'; // Interrupt 7 Timer/Counter Compare Match B
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 8 Watchdog Time-out
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 9 ADC Conversion Complete

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp PCINT0_ISR
   rjmp TIM0_OVF_ISR
   rjmp EE_RDY_ISR
   rjmp ANA_COMP_ISR
   rjmp TIM0_COMPA_ISR
   rjmp TIM0_COMPB_ISR
   rjmp WDT_ISR
   rjmp ADC_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak PCINT0_ISR
   .weak TIM0_OVF_ISR
   .weak EE_RDY_ISR
   .weak ANA_COMP_ISR
   .weak TIM0_COMPA_ISR
   .weak TIM0_COMPB_ISR
   .weak WDT_ISR
   .weak ADC_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set TIM0_OVF_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set TIM0_COMPA_ISR, Default_IRQ_handler
   .set TIM0_COMPB_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
 end;

end.

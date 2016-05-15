unit ATtiny5;

{$goto on}

interface

var
  // AD_CONVERTER
  ADMUX : byte absolute $00+$1B; // The ADC multiplexer Selection Register
  ADCL : byte absolute $00+$19; // ADC Data Register
  ADCSRA : byte absolute $00+$1D; // The ADC Control and Status register A
  ADCSRB : byte absolute $00+$1C; // The ADC Control and Status register B
  DIDR0 : byte absolute $00+$17; // Digital Input Disable Register
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$1F; // Analog Comparator Control And Status Register
  // CPU
  CCP : byte absolute $00+$3C; // Configuration Change Protection
  SP : word absolute $00+$3D; // Stack Pointer 
  SPL : byte absolute $00+$3D; // Stack Pointer 
  SPH : byte absolute $00+$3D+1; // Stack Pointer 
  SREG : byte absolute $00+$3F; // Status Register
  CLKMSR : byte absolute $00+$37; // Clock Main Settings Register
  CLKPSR : byte absolute $00+$36; // Clock Prescale Register
  OSCCAL : byte absolute $00+$39; // Oscillator Calibration Value
  SMCR : byte absolute $00+$3A; // Sleep Mode Control Register
  PRR : byte absolute $00+$35; // Power Reduction Register
  VLMCSR : byte absolute $00+$34; // Vcc Level Monitoring Control and Status Register
  RSTFLR : byte absolute $00+$3B; // Reset Flag Register
  NVMCSR : byte absolute $00+$32; // Non-Volatile Memory Control and Status Register
  NVMCMD : byte absolute $00+$33; // Non-Volatile Memory Command
  // PORTB
  PORTCR : byte absolute $00+$0C; // Port Control Register
  PUEB : byte absolute $00+$03; // Pull-up Enable Control Register
  DDRB : byte absolute $00+$01; // Data Direction Register, Port B
  PINB : byte absolute $00+$00; // Port B Data register
  PORTB : byte absolute $00+$02; // Input Pins, Port B
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$15; // External Interrupt Control Register A
  EIMSK : byte absolute $00+$13; // External Interrupt Mask register
  EIFR : byte absolute $00+$14; // External Interrupt Flag register
  PCICR : byte absolute $00+$12; // Pin Change Interrupt Control Register
  PCIFR : byte absolute $00+$11; // Pin Change Interrupt Flag Register
  PCMSK : byte absolute $00+$10; // Pin Change Mask Register
  // TIMER_COUNTER_0
  TCCR0A : byte absolute $00+$2E; // Timer/Counter 0 Control Register A
  TCCR0B : byte absolute $00+$2D; // Timer/Counter 0 Control Register B
  TCCR0C : byte absolute $00+$2C; // Timer/Counter 0 Control Register C
  TCNT0 : word absolute $00+$28; // Timer/Counter0 
  TCNT0L : byte absolute $00+$28; // Timer/Counter0 
  TCNT0H : byte absolute $00+$28+1; // Timer/Counter0 
  OCR0A : word absolute $00+$26; // Timer/Counter 0 Output Compare Register A 
  OCR0AL : byte absolute $00+$26; // Timer/Counter 0 Output Compare Register A 
  OCR0AH : byte absolute $00+$26+1; // Timer/Counter 0 Output Compare Register A 
  OCR0B : word absolute $00+$24; // Timer/Counter0 Output Compare Register B 
  OCR0BL : byte absolute $00+$24; // Timer/Counter0 Output Compare Register B 
  OCR0BH : byte absolute $00+$24+1; // Timer/Counter0 Output Compare Register B 
  ICR0 : word absolute $00+$22; // Input Capture Register  Bytes
  ICR0L : byte absolute $00+$22; // Input Capture Register  Bytes
  ICR0H : byte absolute $00+$22+1; // Input Capture Register  Bytes
  TIMSK0 : byte absolute $00+$2B; // Timer Interrupt Mask Register 0
  TIFR0 : byte absolute $00+$2A; // Overflow Interrupt Enable
  GTCCR : byte absolute $00+$2F; // General Timer/Counter Control Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$31; // Watchdog Timer Control and Status Register

const
  // ADMUX
  MUX = 0; // Analog Channel Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC  Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ADCSRB
  ADTS = 0; // ADC Auto Trigger Source bits
  // DIDR0
  ADC3D = 3; // 
  ADC2D = 2; // 
  ADC1D = 1; // 
  ADC0D = 0; // 
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture  Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // DIDR0
  AIN1D = 1; // AIN1 Digital Input Disable
  AIN0D = 0; // AIN0 Digital Input Disable
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // CLKMSR
  CLKMS = 0; // Clock Main Select Bits
  // CLKPSR
  CLKPS = 0; // Clock Prescaler Select Bits
  // SMCR
  SM = 1; // Sleep Mode Select Bits
  SE = 0; // Sleep Enable
  // PRR
  PRADC = 1; // Power Reduction ADC
  PRTIM0 = 0; // Power Reduction Timer/Counter0
  // VLMCSR
  VLMF = 7; // VLM Flag
  VLMIE = 6; // VLM Interrupt Enable
  VLM = 0; // Trigger Level of Voltage Level Monitor bits
  // RSTFLR
  WDRF = 3; // Watchdog Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on Reset Flag
  // NVMCSR
  NVMBSY = 7; // Non-Volatile Memory Busy
  // PORTCR
  BBMB = 1; // Break-Before-Make Mode Enable
  // EICRA
  ISC01 = 1; // Interrupt Sense Control 0 Bit 1
  ISC00 = 0; // Interrupt Sense Control 0 Bit 0
  // EIMSK
  INT0 = 0; // External Interrupt Request 0 Enable
  // EIFR
  INTF0 = 0; // External Interrupt Flag 0
  // PCICR
  PCIE0 = 0; // Pin Change Interrupt Enable 0
  // PCIFR
  PCIF0 = 0; // Pin Change Interrupt Flag 0
  // PCMSK
  PCINT = 0; // Pin Change Enable Masks
  // TCCR0A
  COM0A = 6; // Compare Output Mode for Channel A bits
  COM0B = 4; // Compare Output Mode for Channel B bits
  WGM0 = 0; // Waveform Generation Mode
  // TCCR0B
  ICNC0 = 7; // Input Capture Noise Canceler
  ICES0 = 6; // Input Capture Edge Select
  CS0 = 0; // Clock Select
  // TCCR0C
  FOC0A = 7; // Force Output Compare for Channel A
  FOC0B = 6; // Force Output Compare for Channel B
  // TIMSK0
  ICIE0 = 5; // Input Capture Interrupt Enable
  OCIE0B = 2; // Output Compare B Match Interrupt Enable
  OCIE0A = 1; // Output Compare A Match Interrupt Enable
  TOIE0 = 0; // Overflow Interrupt Enable
  // TIFR0
  ICF0 = 5; // Input Capture Flag
  OCF0B = 2; // Timer Output Compare Flag 0B
  OCF0A = 1; // Timer Output Compare Flag 0A
  TOV0 = 0; // Timer Overflow Flag
  // GTCCR
  TSM = 7; // Timer Synchronization Mode
  PSR = 0; // Prescaler Reset
  // WDTCSR
  WDIF = 7; // Watchdog Timer Interrupt Flag
  WDIE = 6; // Watchdog Timer Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDE = 3; // Watch Dog Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 2 Pin Change Interrupt Request 0
procedure TIM0_CAPT_ISR; external name 'TIM0_CAPT_ISR'; // Interrupt 3 Timer/Counter0 Input Capture
procedure TIM0_OVF_ISR; external name 'TIM0_OVF_ISR'; // Interrupt 4 Timer/Counter0 Overflow
procedure TIM0_COMPA_ISR; external name 'TIM0_COMPA_ISR'; // Interrupt 5 Timer/Counter Compare Match A
procedure TIM0_COMPB_ISR; external name 'TIM0_COMPB_ISR'; // Interrupt 6 Timer/Counter Compare Match B
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 7 Analog Comparator
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 8 Watchdog Time-out
procedure VLM_ISR; external name 'VLM_ISR'; // Interrupt 9 Vcc Voltage Level Monitor
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 10 ADC Conversion Complete

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp PCINT0_ISR
   rjmp TIM0_CAPT_ISR
   rjmp TIM0_OVF_ISR
   rjmp TIM0_COMPA_ISR
   rjmp TIM0_COMPB_ISR
   rjmp ANA_COMP_ISR
   rjmp WDT_ISR
   rjmp VLM_ISR
   rjmp ADC_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak PCINT0_ISR
   .weak TIM0_CAPT_ISR
   .weak TIM0_OVF_ISR
   .weak TIM0_COMPA_ISR
   .weak TIM0_COMPB_ISR
   .weak ANA_COMP_ISR
   .weak WDT_ISR
   .weak VLM_ISR
   .weak ADC_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set TIM0_CAPT_ISR, Default_IRQ_handler
   .set TIM0_OVF_ISR, Default_IRQ_handler
   .set TIM0_COMPA_ISR, Default_IRQ_handler
   .set TIM0_COMPB_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set VLM_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
 end;

end.

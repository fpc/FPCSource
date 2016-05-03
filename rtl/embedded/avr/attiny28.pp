unit ATtiny28;

{$goto on}

interface

var
  // PORTD
  PORTD : byte absolute $00+$12; // Port D Data Register
  DDRD : byte absolute $00+$11; // Port D Data Direction Register
  PIND : byte absolute $00+$10; // Port D Input Pins
  // CPU
  SREG : byte absolute $00+$3F; // Status Register
  ICR : byte absolute $00+$06; // Interrupt Control Register
  MCUCS : byte absolute $00+$07; // MCU Control and Status Register
  OSCCAL : byte absolute $00+$00; // Status Register
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$08; // Analog Comparator Control And Status Register
  // TIMER_COUNTER_0
  IFR : byte absolute $00+$05; // Interrupt Flag register
  TCCR0 : byte absolute $00+$04; // Timer/Counter0 Control Register
  TCNT0 : byte absolute $00+$03; // Timer Counter 0
  // WATCHDOG
  WDTCR : byte absolute $00+$01; // Watchdog Timer Control Register
  // EXTERNAL_INTERRUPT
  // PORTA
  PORTA : byte absolute $00+$1B; // Port A Data Register
  PACR : byte absolute $00+$1A; // Port A Control Register
  PINA : byte absolute $00+$19; // Port A Input Pins
  // PORTB
  PINB : byte absolute $00+$16; // Port B Input Pins
  // MODULATOR
  MODCR : byte absolute $00+$02; // Modulation Control Register

const
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // ICR
  ICS1 = 2; // Interrupt Sense Control 1 bits
  ISC0 = 0; // Interrupt Sense Control 0 bits
  // MCUCS
  PLUPB = 7; // Pull-up Enable Port B
  SE = 5; // Sleep Enable
  SM = 4; // Sleep Mode
  WDRF = 3; // Watchdog Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-On Reset Flag
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACO = 5; // Analog Comparator Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // ICR
  TOIE0 = 4; // Timer/Counter0 Overflow Interrupt Enable
  // IFR
  TOV0 = 4; // Timer/Counter0 Overflow Flag
  // TCCR0
  FOV0 = 7; // Force Overflow
  OOM0 = 3; // Overflow Output Mode, Bits
  CS0 = 0; // Clock Select0 bits
  // WDTCR
  WDTOE = 4; // RW
  WDE = 3; // Watch Dog Enable
  WDP = 0; // Watch Dog Timer Prescaler bits
  // ICR
  INT = 6; // External Interrupt Request 1 Enable
  LLIE = 5; // Low-level Input Interrupt Enable
  // IFR
  INTF = 6; // External Interrupt Flags
  // MODCR
  ONTIM4 = 7; // Modulation On-time Bit 4
  OTIM3 = 6; // Modulation On-time Bit 3
  ONTIM = 3; // Modulation On-time Bits
  MCONF = 0; // Modulation Configuration Bits

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt 1
procedure LOW_LEVEL_IO_PINS_ISR; external name 'LOW_LEVEL_IO_PINS_ISR'; // Interrupt 3 Low-level Input on Port B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 4 Timer/Counter0 Overflow
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 5 Analog Comparator

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp INT1_ISR
   rjmp LOW_LEVEL_IO_PINS_ISR
   rjmp TIMER0_OVF_ISR
   rjmp ANA_COMP_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak LOW_LEVEL_IO_PINS_ISR
   .weak TIMER0_OVF_ISR
   .weak ANA_COMP_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set LOW_LEVEL_IO_PINS_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
 end;

end.

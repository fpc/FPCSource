unit ATA6285;

{$goto on}

interface

var
  // SENSOR_INTERFACE
  MSVCAL : byte absolute $00+$67; // Motion Sensor Voltage Calibration Register
  SCR : byte absolute $00+$48; // Sensor Control Register
  SCCR : byte absolute $00+$49; // Sensor Capacitor Control Register
  SVCR : byte absolute $00+$47; // Sensor Voltage Control Register
  SIMSK : byte absolute $00+$61; // Sensor Interrupt Mask register
  SSFR : byte absolute $00+$39; // Sensor Status + Flag Register
  TSCR : byte absolute $00+$64; // Temperature Sensor Control Register
  // SPI
  SPDR : byte absolute $00+$4E; // SPI Data Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPCR : byte absolute $00+$4C; // SPI Control Register
  // CPU
  CLKPR : byte absolute $00+$5C; // Clock Prescaler Register
  CMCR : byte absolute $00+$2F; // Clock Management Control Register
  CMSR : byte absolute $00+$30; // Clock Management Status Register
  CMIMR : byte absolute $00+$5B; // Clock Management Interrupt Mask Register
  FRCCAL : byte absolute $00+$66; // FRC-Oscillator Calibration Register
  SRCCAL : byte absolute $00+$65; // SRC-Oscillator Calibration Register
  VMCSR : byte absolute $00+$36; // Voltage Monitor Control and Status Register
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  GPIOR2 : byte absolute $00+$4B; // General Purpose I/O Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose I/O Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose I/O Register 0
  // LFRX
  LFRCR : byte absolute $00+$82; // Low Frequency Receiver Control Register
  LFCDR : byte absolute $00+$52; // LF receiver Control und Data Register
  LFRB : byte absolute $00+$56; // Low Frequency Receive data Buffer
  LFRR : byte absolute $00+$50; // LF RSSI Data Register
  LFHCR : byte absolute $00+$83; // LF Header Compare Register
  LFIDC : word absolute $00+$84; // LF ID Compare Register 
  LFIDCL : byte absolute $00+$84; // LF ID Compare Register 
  LFIDCH : byte absolute $00+$84+1; // LF ID Compare Register 
  LFIMR : byte absolute $00+$81; // Low Frequency Interrupt Mask Register
  LFFR : byte absolute $00+$38; // Low Frequency Flag Register
  LFCAL : word absolute $00+$86; // LF Calibration Register  Bytes
  LFCALL : byte absolute $00+$86; // LF Calibration Register  Bytes
  LFCALH : byte absolute $00+$86+1; // LF Calibration Register  Bytes
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register
  EIMSK : byte absolute $00+$44; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3D; // External Interrupt Flag Register
  PCMSK0 : byte absolute $00+$6A; // Pin Change Mask Register 0
  PCMSK1 : byte absolute $00+$6B; // Pin Change Mask Register 1
  PCMSK2 : byte absolute $00+$6C; // Pin Change Mask Register 2
  PCIFR : byte absolute $00+$37; // Pin Change Interrupt Flag Register
  PCICR : byte absolute $00+$43; // Pin Change Interrupt Control Register
  // PORTB
  PORTB : byte absolute $00+$25; // Port B Data Register
  DDRB : byte absolute $00+$24; // Port B Data Direction Register
  PINB : byte absolute $00+$23; // Port B Input Pins
  // PORTD
  PORTD : byte absolute $00+$2B; // Port D Data Register
  DDRD : byte absolute $00+$2A; // Port D Data Direction Register
  PIND : byte absolute $00+$29; // Port D Input Pins
  // TIMER_COUNTER_1
  T1CR : byte absolute $00+$58; // Timer 1 Control Register
  T10IFR : byte absolute $00+$3A; // Timer1/0 Interrupt Flag Register
  // TIMER_COUNTER_2
  T2CRA : byte absolute $00+$31; // Timer 2 Control Register A
  T2CRB : byte absolute $00+$32; // Timer 2 Control Register B
  T2MDR : byte absolute $00+$4F; // Timer 2 Modulator Data Register
  T2ICR : byte absolute $00+$6F; // Timer 2 Input Capture Register High Byte
  T2ICRL : byte absolute $00+$6E; // Timer 2 Input Capture Register Low Byte
  T2COR : word absolute $00+$70; // Timer2 Compare Register  Bytes
  T2CORL : byte absolute $00+$70; // Timer2 Compare Register  Bytes
  T2CORH : byte absolute $00+$70+1; // Timer2 Compare Register  Bytes
  T2IFR : byte absolute $00+$3B; // Timer2 Interrupt Flag Register
  T2IMR : byte absolute $00+$74; // Timer 2 Interrupt Mask Register
  T2MRA : byte absolute $00+$72; // Timer 2 Mode Register A
  T2MRB : byte absolute $00+$73; // Timer 2 Mode Register B
  // TIMER_COUNTER_3
  T3CRA : byte absolute $00+$34; // Timer 3 Control Register A
  T3CRB : byte absolute $00+$7E; // Timer 3 Control Register B
  T3MRA : byte absolute $00+$7C; // Timer 3 Mode Register A
  T3IFR : byte absolute $00+$3C; // Timer3 Interrupt Flag Register
  T3IMR : byte absolute $00+$7F; // Timer3 Interrupt Mask Register
  T3MRB : byte absolute $00+$7D; // Timer 3 Mode Register B
  T3ICR : word absolute $00+$76; // Timer3 Input Capture Register  Bytes
  T3ICRL : byte absolute $00+$76; // Timer3 Input Capture Register  Bytes
  T3ICRH : byte absolute $00+$76+1; // Timer3 Input Capture Register  Bytes
  T3CORA : word absolute $00+$78; // Timer3 COmpare Register A  Bytes
  T3CORAL : byte absolute $00+$78; // Timer3 COmpare Register A  Bytes
  T3CORAH : byte absolute $00+$78+1; // Timer3 COmpare Register A  Bytes
  T3CORB : word absolute $00+$7A; // Timer3 COmpare Register B  Bytes
  T3CORBL : byte absolute $00+$7A; // Timer3 COmpare Register B  Bytes
  T3CORBH : byte absolute $00+$7A+1; // Timer3 COmpare Register B  Bytes
  // WATCHDOG
  WDTCR : byte absolute $00+$60; // Watchdog Timer Control Register
  // TIMER_COUNTER_0
  T0CR : byte absolute $00+$59; // Timer 0 Control Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$41; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$41+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // PORTC
  PORTC : byte absolute $00+$28; // Port C Data Register
  DDRC : byte absolute $00+$27; // Port C Data Direction Register
  PINC : byte absolute $00+$26; // Port C Input Pins

const
  // SCR
  SMEN = 3; // Sensor Motion Enable Bit
  SEN = 1; // Sensor enable Bits
  SMS = 0; // Sensor Measurement Start Bit
  // SCCR
  SCCS = 2; // Sensor Capacitor Channel Select Bit2
  SRCC = 0; // Sensor Reference Charge Current Bit1
  // SIMSK
  MSIE = 0; // Motion Sensor Interrupt Enable Bit
  // SSFR
  MSENO = 1; // Motion Sensor Output
  MSENF = 0; // Motion Sensor Flag
  // TSCR
  TSSD = 0; // Temperature Sensor Shutdown mode Disable
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
  // CLKPR
  CLPCE = 7; // Clock Prescaler Change Enable Bit
  CLTPS = 3; // Clock Timer Prescaler Select Bits
  CLKPS = 0; // Clock system Prescaler Select Bits
  // CMCR
  CMCCE = 7; // Clock Management Control Change Enable Bit
  ECINS = 5; // External Clock Input Select Bit
  CCS = 4; // Core Clock Select Bit
  CMONEN = 3; // Clock Monitoring Enable
  SRCD = 2; // Slow RC-oscillator Disable Bit
  CMM = 0; // Clock Management Mode Bitss
  // CMSR
  ECF = 0; // External Clock input Flag Bit
  // CMIMR
  ECIE = 0; // External Clock input Interrupt Enable Bit
  // VMCSR
  BODLS = 7; // Brown-Out Detection Level Select Bit
  BODPD = 6; // Brown-Out Detection on Power-Down Bit
  VMF = 5; // Voltage Monitor Flag
  VMIM = 4; // Voltage Monitor Interrupt Mask Bit
  VMLS = 1; // Voltage Monitor Level Select Bits
  VMEN = 0; // Voltage Monitor Enable Bit
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read-While-Write Section Busy
  RWWSRE = 4; // Read-While-Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SELFPRGEN = 0; // Self Programming Enable
  // MCUCR
  PUD = 4; // 
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUSR
  TSRF = 5; // Temperature Shutdown Reset Flag
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // SMCR
  SM = 1; // 
  SE = 0; // 
  // LFRCR
  LFCS = 5; // LF receiver Capacitor Select Bits
  LFRSS = 4; // LF Receiver Sensitivity Select Bit
  LFWM = 2; // LF receiver Wake-up Mode Bits
  LFBM = 1; // LF receiver Burst Mode enable Bit
  LFEN = 0; // LF receiver Enable Bit
  // LFCDR
  LFSCE = 7; // LF receiver RSSI Software Capture Enable Bit
  LFRST = 6; // LF receiver Reset Bit
  LFDO = 0; // LF receiver Data Output Bit
  // LFIMR
  LFEIM = 2; // LF receiver End of data Interrupt Mask bit
  LFBIM = 1; // LF receiver data Buffer Interrupt Mask bit
  LFWIM = 0; // LF receiver Wake-up Interrupt Mask bit
  // LFFR
  LFRF = 3; // LF receiver Rssi data Flag
  LFEDF = 2; // LF receiver End of data Flag
  LFBF = 1; // LF receiver data Buffer full Flag
  LFWPF = 0; // LF receiver Wake-up Flag
  // EICRA
  ISC1 = 2; // External Interrupt Sense Control 1 Bits
  ISC0 = 0; // External Interrupt Sense Control 0 Bits
  // EIMSK
  INT = 0; // External Interrupt Request 1 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // PCMSK0
  PCINT = 0; // Pin Change Enable Masks
  // PCMSK1
  // PCMSK2
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enables
  // T1CR
  T1IE = 7; // Timer 1 Interrupt Enable Bit
  T1CS = 3; // Timer 1 Clock Select Bits
  T1PS = 0; // Timer 1 Prescaler Select Bits
  // T10IFR
  T1F = 1; // Timer 1 Flag Bit
  T0F = 0; // Timer 0 Flag Bit
  // T2CRA
  T2E = 7; // Timer 2 Enable Bit
  T2TS = 6; // Timer 2 Toggle with Start Bit
  T2ICS = 5; // Timer Input Capture Select Bit
  T2CRM = 3; // Timer 2 Compare Reset Mask Bit
  T2CR = 2; // Timer2 Counter Reset
  T2CTM = 1; // Timer 2 Compare Toggle Mask Bit
  T2OTM = 0; // Timer 2 Overflow Toggle Mask Bit
  // T2CRB
  T2SCE = 0; // Timer 2 Software Capture Enable Bit
  // T2IFR
  T2TCF = 5; // Timer2 SSI Transmit Complete Flag Bit
  T2TXF = 4; // Timer2 SSI Transmit Flag Bit
  T2RXF = 3; // Timer2 SSI Receive Flag Bit
  T2ICF = 2; // Timer2 Input Capture Flag Bit
  T2COF = 1; // Timer 2 Compare Flag Bit
  T2OFF = 0; // Timer 2 Overflow Flag Bit
  // T2IMR
  T2TCIM = 5; // Timer2 SSI Transmit Complete Interrupt Mask Bit
  T2TXIM = 4; // Timer2 SSI Transmit Interrupt Mask Bit
  T2RXIM = 3; // Timer2 SSI Receive Interrupt Mask Bit
  T2CPIM = 2; // Timer 2 Capture Interrupt Mask Bit
  T2CIM = 1; // Timer 2 Compare Interrupt Mask Bit
  T2OIM = 0; // Timer 2 Overflow Interrupt Mask Bit
  // T2MRA
  T2TP = 6; // Timer 2 Top select Bits
  T2CNC = 5; // Timer 2 Input Capture Noise Canceler Bit
  T2CE = 3; // Timer 2 Capture Edge Select Bits
  T2CS = 0; // Timer 2 Clock Select Bits
  // T2MRB
  T2SSIE = 7; // Timer 2 SSI Enable Bit
  T2CPOL = 6; // Timer2 Clock Polarity for SSI shift clock
  T2TOP = 4; // Timer 2 Toggle Output Preset Bit
  T2M = 0; // Timer 2 Mode Bits
  // T3CRA
  T3E = 7; // Timer 3 Enable Bit
  T3TS = 6; // Timer 3 Toggle with Start Bit
  T3CR = 2; // Timer3 Counter Reset
  T3SCE = 1; // Timer 3 Software Capture Enable Bit
  T3AC = 0; // Timer 3 Alternate Compare register sequence bit
  // T3CRB
  T3CPRM = 6; // Timer 3 CaPture Reset Mask bit
  T3CRMB = 5; // Timer 3 Compare Reset Mask bit B
  T3SAMB = 4; // Timer 3 Single Action Mask bit B
  T3CTMB = 3; // Timer 3 Compare Toggle Mask bit B
  T3CRMA = 2; // Timer 3 Compare Reset Mask bit A
  T3SAMA = 1; // Timer 3 Single Action Mask bit A
  T3CTMA = 0; // Timer 3 Compare Toggle Mask bit A
  // T3MRA
  T3ICS = 6; // Timer 3 Input Capture Select Bits
  T3CNC = 5; // Timer 3 input Capture Noise Canceler Bit
  T3CE = 3; // Timer 3 Capture Edge select Bits
  T3CS = 0; // Timer 3 Clock Select Bits
  // T3IFR
  T3ICF = 3; // Timer3 Input Capture Flag bit
  T3COBF = 2; // Timer3 Compare B Flag bit
  T3COAF = 1; // Timer3 Compare A Flag bit
  T3OFF = 0; // Timer3 OverFlow Flag bit
  // T3IMR
  T3CPIM = 3; // Timer3 Capture Interrupt Mask bit
  T3CBIM = 2; // Timer3 Compare B Interrupt Mask bit
  T3CAIM = 1; // Timer3 Compare A Interrupt Mask bit
  T3OIM = 0; // Timer3 Overflow Interrupt Mask bit
  // T3MRB
  T3TOP = 4; // Timer 3 Toggle Output Preset Bit
  T3M = 0; // Timer 3 Mode Bits
  // WDTCR
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  WDPS = 0; // Watch Dog Timer Prescaler Select bits
  // T0CR
  T0PBS = 5; // Timer 0 Prescaler B Select Bits
  T0PR = 4; // Timer 0 Prescaler Reset Bit
  T0IE = 3; // Timer 0 Interrupt Enable Bit
  T0PAS = 0; // Timer 0 Prescaler A Select Bits
  // T10IFR
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 3 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 4 Pin Change Interrupt Request 1
procedure PCINT2_ISR; external name 'PCINT2_ISR'; // Interrupt 5 Pin Change Interrupt Request 2
procedure INTVM_ISR; external name 'INTVM_ISR'; // Interrupt 6 Voltage Monitor Interrupt
procedure SENINT_ISR; external name 'SENINT_ISR'; // Interrupt 7 Sensor Interface Interrupt
procedure INTT0_ISR; external name 'INTT0_ISR'; // Interrupt 8 Timer0 Interval Interrupt
procedure LFWP_ISR; external name 'LFWP_ISR'; // Interrupt 9 LF-Receiver Wake-up Interrupt
procedure T3CAP_ISR; external name 'T3CAP_ISR'; // Interrupt 10 Timer/Counter3 Capture Event
procedure T3COMA_ISR; external name 'T3COMA_ISR'; // Interrupt 11 Timer/Counter3 Compare Match A
procedure T3COMB_ISR; external name 'T3COMB_ISR'; // Interrupt 12 Timer/Counter3 Compare Match B
procedure T3OVF_ISR; external name 'T3OVF_ISR'; // Interrupt 13 Timer/Counter3 Overflow
procedure T2CAP_ISR; external name 'T2CAP_ISR'; // Interrupt 14 Timer/Counter2 Capture Event
procedure T2COM_ISR; external name 'T2COM_ISR'; // Interrupt 15 Timer/Counter2 Compare Match
procedure T2OVF_ISR; external name 'T2OVF_ISR'; // Interrupt 16 Timer/Counter2 Overflow
procedure SPISTC_ISR; external name 'SPISTC_ISR'; // Interrupt 17 SPI Serial Transfer Complete
procedure LFRXB_ISR; external name 'LFRXB_ISR'; // Interrupt 18 LF Receive Buffer Interrupt
procedure INTT1_ISR; external name 'INTT1_ISR'; // Interrupt 19 Timer1 Interval Interrupt
procedure T2RXB_ISR; external name 'T2RXB_ISR'; // Interrupt 20 Timer2 SSI Receive Buffer Interrupt
procedure T2TXB_ISR; external name 'T2TXB_ISR'; // Interrupt 21 Timer2 SSI Transmit Buffer Interrupt
procedure T2TXC_ISR; external name 'T2TXC_ISR'; // Interrupt 22 Timer2 SSI Transmit Complete Interrupt
procedure LFREOB_ISR; external name 'LFREOB_ISR'; // Interrupt 23 LF-Receiver End of Burst Interrupt
procedure EXCM_ISR; external name 'EXCM_ISR'; // Interrupt 24 External Input Clock break down Interrupt
procedure EEREADY_ISR; external name 'EEREADY_ISR'; // Interrupt 25 EEPROM Ready Interrupt
procedure SPM_RDY_ISR; external name 'SPM_RDY_ISR'; // Interrupt 26 Store Program Memory Ready

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp INT1_ISR
   rjmp PCINT0_ISR
   rjmp PCINT1_ISR
   rjmp PCINT2_ISR
   rjmp INTVM_ISR
   rjmp SENINT_ISR
   rjmp INTT0_ISR
   rjmp LFWP_ISR
   rjmp T3CAP_ISR
   rjmp T3COMA_ISR
   rjmp T3COMB_ISR
   rjmp T3OVF_ISR
   rjmp T2CAP_ISR
   rjmp T2COM_ISR
   rjmp T2OVF_ISR
   rjmp SPISTC_ISR
   rjmp LFRXB_ISR
   rjmp INTT1_ISR
   rjmp T2RXB_ISR
   rjmp T2TXB_ISR
   rjmp T2TXC_ISR
   rjmp LFREOB_ISR
   rjmp EXCM_ISR
   rjmp EEREADY_ISR
   rjmp SPM_RDY_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak PCINT2_ISR
   .weak INTVM_ISR
   .weak SENINT_ISR
   .weak INTT0_ISR
   .weak LFWP_ISR
   .weak T3CAP_ISR
   .weak T3COMA_ISR
   .weak T3COMB_ISR
   .weak T3OVF_ISR
   .weak T2CAP_ISR
   .weak T2COM_ISR
   .weak T2OVF_ISR
   .weak SPISTC_ISR
   .weak LFRXB_ISR
   .weak INTT1_ISR
   .weak T2RXB_ISR
   .weak T2TXB_ISR
   .weak T2TXC_ISR
   .weak LFREOB_ISR
   .weak EXCM_ISR
   .weak EEREADY_ISR
   .weak SPM_RDY_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set PCINT2_ISR, Default_IRQ_handler
   .set INTVM_ISR, Default_IRQ_handler
   .set SENINT_ISR, Default_IRQ_handler
   .set INTT0_ISR, Default_IRQ_handler
   .set LFWP_ISR, Default_IRQ_handler
   .set T3CAP_ISR, Default_IRQ_handler
   .set T3COMA_ISR, Default_IRQ_handler
   .set T3COMB_ISR, Default_IRQ_handler
   .set T3OVF_ISR, Default_IRQ_handler
   .set T2CAP_ISR, Default_IRQ_handler
   .set T2COM_ISR, Default_IRQ_handler
   .set T2OVF_ISR, Default_IRQ_handler
   .set SPISTC_ISR, Default_IRQ_handler
   .set LFRXB_ISR, Default_IRQ_handler
   .set INTT1_ISR, Default_IRQ_handler
   .set T2RXB_ISR, Default_IRQ_handler
   .set T2TXB_ISR, Default_IRQ_handler
   .set T2TXC_ISR, Default_IRQ_handler
   .set LFREOB_ISR, Default_IRQ_handler
   .set EXCM_ISR, Default_IRQ_handler
   .set EEREADY_ISR, Default_IRQ_handler
   .set SPM_RDY_ISR, Default_IRQ_handler
 end;

end.

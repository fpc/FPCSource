unit at32uc3b1256;

interface

const
 USB_BASE      = $FFFE0000;
 HMATRIX_BASE  = $FFFE1000;
 FLASHC_BASE   = $FFFE1400;
 PDCA_BASE     = $FFFF0000;
 INTC_BASE     = $FFFF0800;
 PM_BASE       = $FFFF0C00;
 RTC_BASE      = $FFFF0D00;
 WDT_BASE      = $FFFF0D30;
 EIC_BASE      = $FFFF0D80;
 GPIO_BASE     = $FFFF1000;
 USART0_BASE   = $FFFF1400;
 USART1_BAse   = $FFFF1800;
 USART2_BASE   = $FFFF1C00;
 SPI0_BASE     = $FFFF2400;
 TWI_BASE      = $FFFF2C00;
 PWM_BASE      = $FFFF3000;
 SSC_BASE      = $FFFF3400;
 TC_BASE       = $FFFF3800;
 ADC_BASE      = $FFFF3C00;
 ABDAC_BASE    = $FFFF4000;

var
 { Power management }
 PM_MCCTRL: longword       absolute PM_BASE+$000;
 PM_CKSEL: longword        absolute PM_BASE+$004;
 PM_CPUMASK: longword      absolute PM_BASE+$008;
 PM_HSBMASK: longword      absolute PM_BASE+$00C;
 PM_PBAMASK: longword      absolute PM_BASE+$010;
 PM_PBBMASK: longword      absolute PM_BASE+$014;
 PM_PLL0: longword         absolute PM_BASE+$020;
 PM_PLL1: longword         absolute PM_BASE+$024;
 PM_OSCCTRL0: longword     absolute PM_BASE+$028;
 PM_OSCCTRL1: longword     absolute PM_BASE+$02C;
 PM_OSCCTRL32: longword    absolute PM_BASE+$030;
 PM_IER: longword          absolute PM_BASE+$040;
 PM_IDR: longword          absolute PM_BASE+$044;
 PM_IMR: longword          absolute PM_BASE+$048;
 PM_ISR: longword          absolute PM_BASE+$04C;
 PM_ICR: longword          absolute PM_BASE+$050;
 PM_POSCSR: longword       absolute PM_BASE+$054;
 PM_GCCTRL: longword       absolute PM_BASE+$060;
 PM_RCCR: longword         absolute PM_BASE+$0C0;
 PM_BGCR: longword         absolute PM_BASE+$0C4;
 PM_VREGCR: longword       absolute PM_BASE+$0C8;
 PM_BOD: longword          absolute PM_BASE+$0D0;
 PM_RCAUSE: longword       absolute PM_BASE+$140;
 PM_AWEN: longword         absolute PM_BASE+$144;
 PM_GPLP0: longword        absolute PM_BASE+$200;
 PM_GPLP1: longword        absolute PM_BASE+$204;
 
 { RTC }
 RTC_CTRL: longword        absolute RTC_BASE+$000;
 RTC_VAL: longword         absolute RTC_BASE+$004;
 RTC_TOP: longword         absolute RTC_BASE+$008;
 RTC_IER: longword         absolute RTC_BASE+$010;
 RTC_IDR: longword         absolute RTC_BASE+$014;
 RTC_IMR: longword         absolute RTC_BASE+$018;
 RTC_ISR: longword         absolute RTC_BASE+$01C;
 RTC_ICR: longword         absolute RTC_BASE+$020;
 
 { WDT }
 WDT_CTRL: longword        absolute WDT_BASE+$000;
 WDT_CLR: longword         absolute WDT_BASE+$004;
 
 { INTC }
 INTC_IPR: array[0..63] of longword absolute INTC_BASE+$000;
 INTC_IRR: array[0..63] of longword absolute INTC_BASE+$100;
 INTC_ICR3: longword       absolute INTC_BASE+$200;
 INTC_ICR2: longword       absolute INTC_BASE+$204;
 INTC_ICR1: longword       absolute INTC_BASE+$208;
 INTC_ICR0: longword       absolute INTC_BASE+$20C;
 
 { EIC }
 EIC_IER: longword         absolute EIC_BASE+$000;
 EIC_IDR: longword         absolute EIC_BASE+$004;
 EIC_IMR: longword         absolute EIC_BASE+$008;
 EIC_ISR: longword         absolute EIC_BASE+$00C;
 EIC_ICR: longword         absolute EIC_BASE+$010;
 EIC_MODE: longword        absolute EIC_BASE+$014;
 EIC_EDGE: longword        absolute EIC_BASE+$018;
 EIC_LEVEL: longword       absolute EIC_BASE+$01C;
 EIC_FILTER: longword      absolute EIC_BASE+$020;
 EIC_TEST: longword        absolute EIC_BASE+$024;
 EIC_ASYNC: longword       absolute EIC_BASE+$028;
 EIC_SCAN: longword        absolute EIC_BASE+$02C;
 EIC_EN: longword          absolute EIC_BASE+$030;
 EIC_DIS: longword         absolute EIC_BASE+$034;
 EIC_CTRL: longword        absolute EIC_BASE+$038;
 
 { FLASHC }
 FLASHC_FCR: longword      absolute FLASHC_BASE+$000;
 FLASHC_FCMD: longword     absolute FLASHC_BASE+$004;
 FLASHC_FSR: longword      absolute FLASHC_BASE+$008;
 FLASHC_FGPFRHI: longword  absolute FLASHC_BASE+$00C;
 FLASHC_FGPFRLO: longword  absolute FLASHC_BASE+$010;
 
 { DMA }
type
 TDMA_registers = packed record
  MAR, PSR, TCR, MARR,
  TCRR, CR, MR, SR,
  IER, IDR, IMR, ISR: longword;
  res: array[0..3] of longword;
 end;

var
 DMA: array[0..6] of TDMA_registers absolute PDCA_BASE;
 
type
 TGPIO_Registers = packed record
  GPER,
  GPERS,
  GPERC,
  GPERT,
  PMR0,
  PMR0S,
  PMR0C,
  PMR0T,
  PMR1,
  PMR1S,
  PMR1C,
  PMR1T: longword;
  res0: array[0..3] of longword;
  ODER,
  ODERS,
  ODERC,
  ODERT,
  OVR,
  OVRS,
  OVRC,
  OVRT,
  PVR: longword;
  res1: array[0..2] of longword;
  PUER,
  PUERS,
  PUERC,
  PUERT: longword;
  res2: array[0..3] of longword;
  IER,
  IERS,
  IERC,
  IERT,
  IMR0,
  IMR0S,
  IMR0C,
  IMR0T,
  IMR1,
  IMR1S,
  IMR1C,
  IMR1T,
  GFER,
  GFERS,
  GFERC,
  GFERT,
  IFR,
  res3,
  IFRC,
  res4: longword;
 end;

var
 GPIO0: TGPIO_Registers absolute GPIO_BASE+$000;
 GPIO1: TGPIO_Registers absolute GPIO_BASE+$100;
 
 { USART }
type
 TUSART_registers = packed record
  CR,
  MR,
  IER,
  IDR,
  IMR,
  CSR,
  RHR,
  THR,
  BRGR,
  RTOR,
  TTGR: longword;
  res0: array[0..4] of longword;
  FIDI,
  NER,
  res1,
  IFR,
  MAN: longword;
  res2: array[0..41] of longword;
  VERSION: longword;
 end;

var
 USART0: TUSART_registers absolute USART0_BASE;
 USART1: TUSART_registers absolute USART1_BASE;
 USART2: TUSART_registers absolute USART2_BASE;

 { USBB }
type
 TUSB_DMA = packed record
  NEXTDESC,
  ADDR,
  CONTROL,
  STATUS: longword;
 end;

var
 USBB_UDCON: longword                     absolute USB_BASE+$0000;
 USBB_UDINT: longword                     absolute USB_BASE+$0004;
 USBB_UDINTCLR: longword                  absolute USB_BASE+$0008;
 USBB_UDINTSET: longword                  absolute USB_BASE+$000C;
 USBB_UDINTE: longword                    absolute USB_BASE+$0010;
 USBB_UDINTECLR: longword                 absolute USB_BASE+$0014;
 USBB_UDINTESET: longword                 absolute USB_BASE+$0018;
 USBB_UERST: longword                     absolute USB_BASE+$001C;
 USBB_UDFNUM: longword                    absolute USB_BASE+$0020;
 USBB_UECFG: array[0..6] of longword      absolute USB_BASE+$0100;
 USBB_UESTA: array[0..6] of longword      absolute USB_BASE+$0130;
 USBB_UESTACLR: array[0..7] of longword   absolute USB_BASE+$0160;
 USBB_UESTASET: array[0..7] of longword   absolute USB_BASE+$0190;
 USBB_UECON: array[0..7] of longword      absolute USB_BASE+$01C0;
 USBB_UECONSET: array[0..7] of longword   absolute USB_BASE+$01F0;
 USBB_UECONCLR: array[0..7] of longword   absolute USB_BASE+$0220;
 USBB_UDDMA: array[1..6] of TUSB_DMA      absolute USB_BASE+$0310;

implementation

procedure PASCALMAIN; external name 'PASCALMAIN';

var
 _data: record end; external name '_data';
 _edata: record end; external name '_edata';
 _etext: record end; external name '_etext';
 _bss_start: record end; external name '_bss_start';
 _bss_end: record end; external name '_bss_end';
 _stack_top: record end; external name '_stack_top';

procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lhalt:
	rjmp .Lhalt
end;

procedure StartCode; nostackframe; assembler; [public, alias: '_START'];// interrupt 0;
asm
   .init
   lda.w pc, .Lstart
   
   .text
.Lstart:
   // Update stack
   lddpc sp, .L_stack_top
   
   // Set EVBA
   lddpc r0, .L_evba_base
   mtsr 4, r0 // EVBA
   
   // copy initialized data from flash to ram
   lddpc r1,.L_etext
   lddpc r2,.L_data
   lddpc r3,.L_edata
.Lcopyloop:
   cp.w r2,r3
   brhi .Lecopyloop
   ld.w r0, r1++
   st.w r2++, r0
   bral .Lcopyloop
.Lecopyloop:

   // clear onboard ram
   lddpc r1,.L_bss_start
   lddpc r2,.L_bss_end
   mov r0, 0
.Lzeroloop:
   cp.w r1,r2
   brhi .Lezeroloop
   st.w r1++, r0
   bral .Lzeroloop
.Lezeroloop:

   bral PASCALMAIN
   
.L_bss_start:
   .long _bss_start
.L_bss_end:
   .long _bss_end
.L_etext:
   .long _etext
.L_data:
   .long _data
.L_edata:
   .long _edata
.L_evba_base:
   .long 0x0
.L_stack_top:
   .long _stack_top
end;

end.

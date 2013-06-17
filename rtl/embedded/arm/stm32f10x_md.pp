{
Register definitions and utility code for STM32F10x - Medium density

Created by Jeppe Johansen 2012 - jeppe@j-software.dk
}
unit stm32f10x_md;

{$goto on}
{$define stm32f10x_md}

interface

type
 TBitvector32 = bitpacked array[0..31] of 0..1;

{$PACKRECORDS 2}
const
 PeripheralBase 	= $40000000;

 FSMCBase			= $60000000;

 APB1Base 			= PeripheralBase;
 APB2Base 			= PeripheralBase+$10000;
 AHBBase 			= PeripheralBase+$20000;

 { FSMC }
 FSMCBank1NOR1		= FSMCBase+$00000000;
 FSMCBank1NOR2		= FSMCBase+$04000000;
 FSMCBank1NOR3		= FSMCBase+$08000000;
 FSMCBank1NOR4		= FSMCBase+$0C000000;

 FSMCBank1PSRAM1	= FSMCBase+$00000000;
 FSMCBank1PSRAM2	= FSMCBase+$04000000;
 FSMCBank1PSRAM3	= FSMCBase+$08000000;
 FSMCBank1PSRAM4	= FSMCBase+$0C000000;

 FSMCBank2NAND1	= FSMCBase+$10000000;
 FSMCBank3NAND2	= FSMCBase+$20000000;

 FSMCBank4PCCARD	= FSMCBase+$30000000;

type
 TTimerRegisters = record
  CR1, res1,
  CR2, res2,
  SMCR, res3,
  DIER, res4,
  SR, res5,
  EGR, res,
  CCMR1, res6,
  CCMR2, res7,
  CCER, res8,
  CNT, res9,
  PSC, res10,
  ARR, res11,
  RCR, res12,
  CCR1, res13,
  CCR2, res14,
  CCR3, res15,
  CCR4, res16,
  BDTR, res17,
  DCR, res18,
  DMAR, res19: Word;
 end;

 TRTCRegisters = record
  CRH, res1,
  CRL, res2,
  PRLH, res3,
  PRLL, res4,
  DIVH, res5,
  DIVL, res6,
  CNTH, res7,
  CNTL, res8,
  ALRH, res9,
  ALRL, res10: Word;
 end;

 TIWDGRegisters = record
  KR, res1,
  PR, res2,
  RLR, res3,
  SR, res4: word;
 end;

 TWWDGRegisters = record
  CR, res2,
  CFR, res3,
  SR, res4: word;
 end;

 TSPIRegisters = record
  CR1, res1,
  CR2, res2,
  SR, res3,
  DR, res4,
  CRCPR, res5,
  RXCRCR, res6,
  TXCRCR, res7,
  I2SCFGR, res8,
  I2SPR, res9: Word;
 end;

 TUSARTRegisters = record
  SR, res1,
  DR, res2,
  BRR, res3,
  CR1, res4,
  CR2, res5,
  CR3, res6,
  GTPR, res7: Word;
 end;

 TI2CRegisters = record
  CR1, res1,
  CR2, res2,
  OAR1, res3,
  OAR2, res4,
  DR, res5,
  SR1, res6,
  SR2, res7,
  CCR, res8: word;
  TRISE: byte;
 end;

 TUSBRegisters = record
  EPR: array[0..7] of longword;

  res: array[0..7] of longword;

  CNTR, res1,
  ISTR, res2,
  FNR, res3: Word;
  DADDR: byte; res4: word; res5: byte;
  BTABLE: Word;
 end;

 TUSBMem = packed array[0..511] of byte;

 TCANMailbox = record
  IR,
  DTR,
  DLR,
  DHR: longword;
 end;

 TCANRegisters = record
  MCR,
  MSR,
  TSR,
  RF0R,
  RF1R,
  IER,
  ESR,
  BTR: longword;

  res5: array[$020..$17F] of byte;

  TX: array[0..2] of TCANMailbox;
  RX: array[0..2] of TCANMailbox;

  res6: array[$1D0..$1FF] of byte;

  FMR,
  FM1R,
  res9: longword;
  FS1R, res10: word;
  res11: longword;
  FFA1R, res12: word;
  res13: longword;
  FA1R, res14: word;
  res15: array[$220..$23F] of byte;

  FOR1,
  FOR2: longword;

  FB: array[1..13] of array[1..2] of longword;
 end;

 TBKPRegisters = record
  DR: array[1..10] of record data, res: word; end;

  RTCCR,
  CR,
  CSR,
  res1,res2: longword;

  DR2: array[11..42] of record data, res: word; end;
 end;

 TPwrRegisters = record
  CR, res: word;
  CSR: Word;
 end;

 TDACRegisters = record
  CR,
  SWTRIGR: longword;

  DHR12R1, res2,
  DHR12L1, res3,
  DHR8R1, res4,
  DHR12R2, res5,
  DHR12L2, res6,
  DHR8R2, res7: word;

  DHR12RD,
  DHR12LD: longword;

  DHR8RD, res8,

  DOR1, res9,
  DOR2, res10: Word;
 end;

 TAFIORegisters = record
  EVCR,
  MAPR: longword;
  EXTICR: array[0..3] of longword;
 end;

 TEXTIRegisters = record
  IMR,
  EMR,
  RTSR,
  FTSR,
  SWIER,
  PR: longword;
 end;

 TPortRegisters = record
  CRL,
  CRH,
  IDR,
  ODR,
  BSRR,
  BRR,
  LCKR: longword;
 end;

 TADCRegisters = record
  SR,
  CR1,
  CR2,
  SMPR1,
  SMPR2: longword;
  JOFR1, res2,
  JOFR2, res3,
  JOFR3, res4,
  JOFR4, res5,
  HTR, res6,
  LTR, res7: word;
  SQR1,
  SQR2,
  SQR3,
  JSQR: longword;
  JDR1, res8,
  JDR2, res9,
  JDR3, res10,
  JDR4, res11: Word;
  DR: longword;
 end;

 TSDIORegisters = record
  POWER,
  CLKCR,
  ARG: longword;
  CMD, res3,
  RESPCMD, res4: Word;
  RESP1,
  RESP2,
  RESP3,
  RESP4,
  DTIMER,
  DLEN: longword;
  DCTRL, res5: word;
  DCOUNT,
  STA,
  ICR,
  MASK,
  FIFOCNT,
  FIFO: longword;
 end;

 TDMAChannel = record
  CCR, res1,
  CNDTR, res2: word;
  CPAR,
  CMAR,
  res: longword;
 end;

 TDMARegisters = record
  ISR,
  IFCR: longword;
  Channel: array[0..7] of TDMAChannel;
 end;

 TRCCRegisters = record
  CR,
  CFGR,
  CIR,
  APB2RSTR,
  APB1RSTR,
  AHBENR,
  APB2ENR,
  APB1ENR,
  BDCR,
  CSR: longword;
 end;

 TCRCRegisters = record
  DR: longword;
  IDR: byte; res1: word; res2: byte;
  CR: byte;
 end;

 TFlashRegisters = record
  ACR,
  KEYR,
  OPTKEYR,
  SR,
  CR,
  AR,
  res,
  OBR,
  WRPR: longword;
 end;

{$ALIGN 2}
var
 { Timers }
 Timer1: TTimerRegisters 	absolute (APB2Base+$2C00);
 Timer2: TTimerRegisters 	absolute (APB1Base+$0000);
 Timer3: TTimerRegisters 	absolute (APB1Base+$0400);
 Timer4: TTimerRegisters 	absolute (APB1Base+$0800);
 Timer5: TTimerRegisters 	absolute (APB1Base+$0C00);
 Timer6: TTimerRegisters 	absolute (APB1Base+$1000);
 Timer7: TTimerRegisters 	absolute (APB1Base+$1400);
 Timer8: TTimerRegisters 	absolute (APB2Base+$3400);

 { RTC }
 RTC: TRTCRegisters 			absolute (APB1Base+$2800);

 { WDG }
 WWDG: TWWDGRegisters 		absolute (APB1Base+$2C00);
 IWDG: TIWDGRegisters 		absolute (APB1Base+$3000);

 { SPI }
 SPI1: TSPIRegisters			absolute (APB2Base+$3000);
 SPI2: TSPIRegisters			absolute (APB1Base+$3800);
 SPI3: TSPIRegisters			absolute (APB1Base+$3C00);

 { USART/UART }
 USART1: TUSARTRegisters	absolute (APB2Base+$3800);
 USART2: TUSARTRegisters	absolute (APB1Base+$4400);
 USART3: TUSARTRegisters	absolute (APB1Base+$4800);
 UART4: TUSARTRegisters		absolute (APB1Base+$4C00);
 UART5: TUSARTRegisters		absolute (APB1Base+$5000);

 { I2C }
 I2C1: TI2CRegisters			absolute (APB1Base+$5400);
 I2C2: TI2CRegisters			absolute (APB1Base+$5800);

 { USB }
 USB: TUSBRegisters			absolute (APB1Base+$5C00);
 USBMem: TUSBMem                        absolute (APB1Base+$6000);

 { CAN }
 CAN: TCANRegisters			absolute (APB1Base+$6800);

 { BKP }
 BKP: TBKPRegisters			absolute (APB1Base+$6C00);

 { PWR }
 PWR: TPwrRegisters			absolute (APB1Base+$7000);

 { DAC }
 DAC: TDACRegisters			absolute (APB1Base+$7400);

 { GPIO }
 AFIO: TAFIORegisters		absolute (APB2Base+$0);
 EXTI: TEXTIRegisters		absolute (APB2Base+$0400);

 PortA: TPortRegisters		absolute (APB2Base+$0800);
 PortB: TPortRegisters		absolute (APB2Base+$0C00);
 PortC: TPortRegisters		absolute (APB2Base+$1000);
 PortD: TPortRegisters		absolute (APB2Base+$1400);
 PortE: TPortRegisters		absolute (APB2Base+$1800);
 PortF: TPortRegisters		absolute (APB2Base+$1C00);
 PortG: TPortRegisters		absolute (APB2Base+$2000);

 { ADC }
 ADC1: TADCRegisters			absolute (APB2Base+$2400);
 ADC2: TADCRegisters			absolute (APB2Base+$2800);
 ADC3: TADCRegisters			absolute (APB2Base+$3C00);

 { SDIO }
 SDIO: TSDIORegisters		absolute (APB2Base+$8000);

 { DMA }
 DMA1: TDMARegisters			absolute (AHBBase+$0000);
 DMA2: TDMARegisters			absolute (AHBBase+$0400);

 { RCC }
 RCC: TRCCRegisters			absolute (AHBBase+$1000);

 { Flash }
 Flash: TFlashRegisters		absolute (AHBBase+$2000);

 { CRC }
 CRC: TCRCRegisters			absolute (AHBBase+$3000);

implementation

procedure NMI_interrupt; external name 'NMI_interrupt';
procedure Hardfault_interrupt; external name 'Hardfault_interrupt';
procedure MemManage_interrupt; external name 'MemManage_interrupt';
procedure BusFault_interrupt; external name 'BusFault_interrupt';
procedure UsageFault_interrupt; external name 'UsageFault_interrupt';
procedure SWI_interrupt; external name 'SWI_interrupt';
procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
procedure PendingSV_interrupt; external name 'PendingSV_interrupt';
procedure SysTick_interrupt; external name 'SysTick_interrupt';
procedure Window_watchdog_interrupt; external name 'Window_watchdog_interrupt';
procedure PVD_through_EXTI_Line_detection_interrupt; external name 'PVD_through_EXTI_Line_detection_interrupt';
procedure Tamper_interrupt; external name 'Tamper_interrupt';
procedure RTC_global_interrupt; external name 'RTC_global_interrupt';
procedure Flash_global_interrupt; external name 'Flash_global_interrupt';
procedure RCC_global_interrupt; external name 'RCC_global_interrupt';
procedure EXTI_Line0_interrupt; external name 'EXTI_Line0_interrupt';
procedure EXTI_Line1_interrupt; external name 'EXTI_Line1_interrupt';
procedure EXTI_Line2_interrupt; external name 'EXTI_Line2_interrupt';
procedure EXTI_Line3_interrupt; external name 'EXTI_Line3_interrupt';
procedure EXTI_Line4_interrupt; external name 'EXTI_Line4_interrupt';
procedure DMA1_Channel1_global_interrupt; external name 'DMA1_Channel1_global_interrupt';
procedure DMA1_Channel2_global_interrupt; external name 'DMA1_Channel2_global_interrupt';
procedure DMA1_Channel3_global_interrupt; external name 'DMA1_Channel3_global_interrupt';
procedure DMA1_Channel4_global_interrupt; external name 'DMA1_Channel4_global_interrupt';
procedure DMA1_Channel5_global_interrupt; external name 'DMA1_Channel5_global_interrupt';
procedure DMA1_Channel6_global_interrupt; external name 'DMA1_Channel6_global_interrupt';
procedure DMA1_Channel7_global_interrupt; external name 'DMA1_Channel7_global_interrupt';
procedure ADC1_and_ADC2_global_interrupt; external name 'ADC1_and_ADC2_global_interrupt';
procedure USB_High_Priority_or_CAN_TX_interrupts; external name 'USB_High_Priority_or_CAN_TX_interrupts';
procedure USB_Low_Priority_or_CAN_RX0_interrupts; external name 'USB_Low_Priority_or_CAN_RX0_interrupts';
procedure CAN_RX1_interrupt; external name 'CAN_RX1_interrupt';
procedure CAN_SCE_interrupt; external name 'CAN_SCE_interrupt';
procedure EXTI_Line9_5_interrupts; external name 'EXTI_Line9_5_interrupts';
procedure TIM1_Break_interrupt; external name 'TIM1_Break_interrupt';
procedure TIM1_Update_interrupt; external name 'TIM1_Update_interrupt';
procedure TIM1_Trigger_and_Commutation_interrupts; external name 'TIM1_Trigger_and_Commutation_interrupts';
procedure TIM1_Capture_Compare_interrupt; external name 'TIM1_Capture_Compare_interrupt';
procedure TIM2_global_interrupt; external name 'TIM2_global_interrupt';
procedure TIM3_global_interrupt; external name 'TIM3_global_interrupt';
procedure TIM4_global_interrupt; external name 'TIM4_global_interrupt';
procedure I2C1_event_interrupt; external name 'I2C1_event_interrupt';
procedure I2C1_error_interrupt; external name 'I2C1_error_interrupt';
procedure I2C2_event_interrupt; external name 'I2C2_event_interrupt';
procedure I2C2_error_interrupt; external name 'I2C2_error_interrupt';
procedure SPI1_global_interrupt; external name 'SPI1_global_interrupt';
procedure SPI2_global_interrupt; external name 'SPI2_global_interrupt';
procedure USART1_global_interrupt; external name 'USART1_global_interrupt';
procedure USART2_global_interrupt; external name 'USART2_global_interrupt';
procedure USART3_global_interrupt; external name 'USART3_global_interrupt';
procedure EXTI_Line15_10_interrupts; external name 'EXTI_Line15_10_interrupts';
procedure RTC_alarm_through_EXTI_line_interrupt; external name 'RTC_alarm_through_EXTI_line_interrupt';
procedure USB_wakeup_from_suspend_through_EXTI_line_interrupt; external name 'USB_wakeup_from_suspend_through_EXTI_line_interrupt';
procedure TIM8_Break_interrupt; external name 'TIM8_Break_interrupt';
procedure TIM8_Update_interrupt; external name 'TIM8_Update_interrupt';
procedure TIM8_Trigger_and_Commutation_interrupts; external name 'TIM8_Trigger_and_Commutation_interrupts';
procedure TIM8_Capture_Compare_interrupt; external name 'TIM8_Capture_Compare_interrupt';
procedure ADC3_global_interrupt; external name 'ADC3_global_interrupt';
procedure FSMC_global_interrupt; external name 'FSMC_global_interrupt';
procedure SDIO_global_interrupt; external name 'SDIO_global_interrupt';
procedure TIM5_global_interrupt; external name 'TIM5_global_interrupt';
procedure SPI3_global_interrupt; external name 'SPI3_global_interrupt';
procedure UART4_global_interrupt; external name 'UART4_global_interrupt';
procedure UART5_global_interrupt; external name 'UART5_global_interrupt';
procedure TIM6_global_interrupt; external name 'TIM6_global_interrupt';
procedure TIM7_global_interrupt; external name 'TIM7_global_interrupt';
procedure DMA2_Channel1_global_interrupt; external name 'DMA2_Channel1_global_interrupt';
procedure DMA2_Channel2_global_interrupt; external name 'DMA2_Channel2_global_interrupt';
procedure DMA2_Channel3_global_interrupt; external name 'DMA2_Channel3_global_interrupt';
procedure DMA2_Channel4_and_DMA2_Channel5_global_interrupts; external name 'DMA2_Channel4_and_DMA2_Channel5_global_interrupts';

{$i cortexm3_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
   .section ".init.interrupt_vectors"
interrupt_vectors:
   .long _stack_top
   .long Startup
   .long NMI_interrupt
   .long Hardfault_interrupt
   .long MemManage_interrupt
   .long BusFault_interrupt
   .long UsageFault_interrupt
   .long 0
   .long 0
   .long 0
   .long 0
   .long SWI_interrupt
   .long DebugMonitor_interrupt
   .long 0
   .long PendingSV_interrupt
   .long SysTick_interrupt
   
   .long Window_watchdog_interrupt
   .long PVD_through_EXTI_Line_detection_interrupt
   .long Tamper_interrupt
   .long RTC_global_interrupt
   .long Flash_global_interrupt
   .long RCC_global_interrupt
   .long EXTI_Line0_interrupt
   .long EXTI_Line1_interrupt
   .long EXTI_Line2_interrupt
   .long EXTI_Line3_interrupt
   .long EXTI_Line4_interrupt
   .long DMA1_Channel1_global_interrupt
   .long DMA1_Channel2_global_interrupt
   .long DMA1_Channel3_global_interrupt
   .long DMA1_Channel4_global_interrupt
   .long DMA1_Channel5_global_interrupt
   .long DMA1_Channel6_global_interrupt
   .long DMA1_Channel7_global_interrupt
   .long ADC1_and_ADC2_global_interrupt
   .long USB_High_Priority_or_CAN_TX_interrupts
   .long USB_Low_Priority_or_CAN_RX0_interrupts
   .long CAN_RX1_interrupt
   .long CAN_SCE_interrupt
   .long EXTI_Line9_5_interrupts
   .long TIM1_Break_interrupt
   .long TIM1_Update_interrupt
   .long TIM1_Trigger_and_Commutation_interrupts
   .long TIM1_Capture_Compare_interrupt
   .long TIM2_global_interrupt
   .long TIM3_global_interrupt
   .long TIM4_global_interrupt
   .long I2C1_event_interrupt
   .long I2C1_error_interrupt
   .long I2C2_event_interrupt
   .long I2C2_error_interrupt
   .long SPI1_global_interrupt
   .long SPI2_global_interrupt
   .long USART1_global_interrupt
   .long USART2_global_interrupt
   .long USART3_global_interrupt
   .long EXTI_Line15_10_interrupts
   .long RTC_alarm_through_EXTI_line_interrupt
   .long USB_wakeup_from_suspend_through_EXTI_line_interrupt
   .long TIM8_Break_interrupt
   .long TIM8_Update_interrupt
   .long TIM8_Trigger_and_Commutation_interrupts
   .long TIM8_Capture_Compare_interrupt
   .long ADC3_global_interrupt
   .long FSMC_global_interrupt
   .long SDIO_global_interrupt
   .long TIM5_global_interrupt
   .long SPI3_global_interrupt
   .long UART4_global_interrupt
   .long UART5_global_interrupt
   .long TIM6_global_interrupt
   .long TIM7_global_interrupt
   .long DMA2_Channel1_global_interrupt
   .long DMA2_Channel2_global_interrupt
   .long DMA2_Channel3_global_interrupt
   .long DMA2_Channel4_and_DMA2_Channel5_global_interrupts
   
   .weak NMI_interrupt
   .weak Hardfault_interrupt
   .weak MemManage_interrupt
   .weak BusFault_interrupt
   .weak UsageFault_interrupt
   .weak SWI_interrupt
   .weak DebugMonitor_interrupt
   .weak PendingSV_interrupt
   .weak SysTick_interrupt
   
   .weak Window_watchdog_interrupt
   .weak PVD_through_EXTI_Line_detection_interrupt
   .weak Tamper_interrupt
   .weak RTC_global_interrupt
   .weak Flash_global_interrupt
   .weak RCC_global_interrupt
   .weak EXTI_Line0_interrupt
   .weak EXTI_Line1_interrupt
   .weak EXTI_Line2_interrupt
   .weak EXTI_Line3_interrupt
   .weak EXTI_Line4_interrupt
   .weak DMA1_Channel1_global_interrupt
   .weak DMA1_Channel2_global_interrupt
   .weak DMA1_Channel3_global_interrupt
   .weak DMA1_Channel4_global_interrupt
   .weak DMA1_Channel5_global_interrupt
   .weak DMA1_Channel6_global_interrupt
   .weak DMA1_Channel7_global_interrupt
   .weak ADC1_and_ADC2_global_interrupt
   .weak USB_High_Priority_or_CAN_TX_interrupts
   .weak USB_Low_Priority_or_CAN_RX0_interrupts
   .weak CAN_RX1_interrupt
   .weak CAN_SCE_interrupt
   .weak EXTI_Line9_5_interrupts
   .weak TIM1_Break_interrupt
   .weak TIM1_Update_interrupt
   .weak TIM1_Trigger_and_Commutation_interrupts
   .weak TIM1_Capture_Compare_interrupt
   .weak TIM2_global_interrupt
   .weak TIM3_global_interrupt
   .weak TIM4_global_interrupt
   .weak I2C1_event_interrupt
   .weak I2C1_error_interrupt
   .weak I2C2_event_interrupt
   .weak I2C2_error_interrupt
   .weak SPI1_global_interrupt
   .weak SPI2_global_interrupt
   .weak USART1_global_interrupt
   .weak USART2_global_interrupt
   .weak USART3_global_interrupt
   .weak EXTI_Line15_10_interrupts
   .weak RTC_alarm_through_EXTI_line_interrupt
   .weak USB_wakeup_from_suspend_through_EXTI_line_interrupt
   .weak TIM8_Break_interrupt
   .weak TIM8_Update_interrupt
   .weak TIM8_Trigger_and_Commutation_interrupts
   .weak TIM8_Capture_Compare_interrupt
   .weak ADC3_global_interrupt
   .weak FSMC_global_interrupt
   .weak SDIO_global_interrupt
   .weak TIM5_global_interrupt
   .weak SPI3_global_interrupt
   .weak UART4_global_interrupt
   .weak UART5_global_interrupt
   .weak TIM6_global_interrupt
   .weak TIM7_global_interrupt
   .weak DMA2_Channel1_global_interrupt
   .weak DMA2_Channel2_global_interrupt
   .weak DMA2_Channel3_global_interrupt
   .weak DMA2_Channel4_and_DMA2_Channel5_global_interrupts

   
   .set NMI_interrupt, HaltProc
   .set Hardfault_interrupt, HaltProc
   .set MemManage_interrupt, HaltProc
   .set BusFault_interrupt, HaltProc
   .set UsageFault_interrupt, HaltProc
   .set SWI_interrupt, HaltProc
   .set DebugMonitor_interrupt, HaltProc
   .set PendingSV_interrupt, HaltProc
   .set SysTick_interrupt, HaltProc

   .set Window_watchdog_interrupt, HaltProc
   .set PVD_through_EXTI_Line_detection_interrupt, HaltProc
   .set Tamper_interrupt, HaltProc
   .set RTC_global_interrupt, HaltProc
   .set Flash_global_interrupt, HaltProc
   .set RCC_global_interrupt, HaltProc
   .set EXTI_Line0_interrupt, HaltProc
   .set EXTI_Line1_interrupt, HaltProc
   .set EXTI_Line2_interrupt, HaltProc
   .set EXTI_Line3_interrupt, HaltProc
   .set EXTI_Line4_interrupt, HaltProc
   .set DMA1_Channel1_global_interrupt, HaltProc
   .set DMA1_Channel2_global_interrupt, HaltProc
   .set DMA1_Channel3_global_interrupt, HaltProc
   .set DMA1_Channel4_global_interrupt, HaltProc
   .set DMA1_Channel5_global_interrupt, HaltProc
   .set DMA1_Channel6_global_interrupt, HaltProc
   .set DMA1_Channel7_global_interrupt, HaltProc
   .set ADC1_and_ADC2_global_interrupt, HaltProc
   .set USB_High_Priority_or_CAN_TX_interrupts, HaltProc
   .set USB_Low_Priority_or_CAN_RX0_interrupts, HaltProc
   .set CAN_RX1_interrupt, HaltProc
   .set CAN_SCE_interrupt, HaltProc
   .set EXTI_Line9_5_interrupts, HaltProc
   .set TIM1_Break_interrupt, HaltProc
   .set TIM1_Update_interrupt, HaltProc
   .set TIM1_Trigger_and_Commutation_interrupts, HaltProc
   .set TIM1_Capture_Compare_interrupt, HaltProc
   .set TIM2_global_interrupt, HaltProc
   .set TIM3_global_interrupt, HaltProc
   .set TIM4_global_interrupt, HaltProc
   .set I2C1_event_interrupt, HaltProc
   .set I2C1_error_interrupt, HaltProc
   .set I2C2_event_interrupt, HaltProc
   .set I2C2_error_interrupt, HaltProc
   .set SPI1_global_interrupt, HaltProc
   .set SPI2_global_interrupt, HaltProc
   .set USART1_global_interrupt, HaltProc
   .set USART2_global_interrupt, HaltProc
   .set USART3_global_interrupt, HaltProc
   .set EXTI_Line15_10_interrupts, HaltProc
   .set RTC_alarm_through_EXTI_line_interrupt, HaltProc
   .set USB_wakeup_from_suspend_through_EXTI_line_interrupt, HaltProc
   .set TIM8_Break_interrupt, HaltProc
   .set TIM8_Update_interrupt, HaltProc
   .set TIM8_Trigger_and_Commutation_interrupts, HaltProc
   .set TIM8_Capture_Compare_interrupt, HaltProc
   .set ADC3_global_interrupt, HaltProc
   .set FSMC_global_interrupt, HaltProc
   .set SDIO_global_interrupt, HaltProc
   .set TIM5_global_interrupt, HaltProc
   .set SPI3_global_interrupt, HaltProc
   .set UART4_global_interrupt, HaltProc
   .set UART5_global_interrupt, HaltProc
   .set TIM6_global_interrupt, HaltProc
   .set TIM7_global_interrupt, HaltProc
   .set DMA2_Channel1_global_interrupt, HaltProc
   .set DMA2_Channel2_global_interrupt, HaltProc
   .set DMA2_Channel3_global_interrupt, HaltProc
   .set DMA2_Channel4_and_DMA2_Channel5_global_interrupts, HaltProc
   
   .text
end;

end.

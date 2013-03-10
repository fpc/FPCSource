{
Register definitions and utility code for XMC450x series

Created by Jeppe Johansen 2012 - jeppe@j-software.dk
}
unit xmc4500;

{$goto on}

interface

type
 TBitvector32 = bitpacked array[0..31] of 0..1;

{$PACKRECORDS 2}
const
 Peripheral0Base  = $40000000;
 Peripheral1Base  = $48000000;
 Peripheral2Base  = $50000000;
 Peripheral3Base  = $58000000;

 SCUBase          = Peripheral2Base+$4000;

 GCUBase          = SCUBase+$0000;
 PCUBase          = SCUBase+$0200;
 HCUBase          = SCUBase+$0300;
 RCUBase          = SCUBase+$0400;
 CCUBase          = SCUBase+$0600;

type
 TPBARegisters = record
  STS,
  WADDR: longword;
 end;

 TFLASHRegisters = record
  res1: array[0..1] of longword;
  ID: longword;
  res2: longword;
  FSR,
  FCON,
  MARP,
  PROCON0,
  PROCON1,
  PROCON2: longword;
 end;

 TWDTRegisters = record
  ID,
  CTR,
  SRV,
  TIM,
  WLB,
  WUB,
  WDTSTS,
  WDTCLR: longword;
 end;

 TRTCRegisters = record
  ID,
  CTR,
  RAWSTAT,
  STSSR,
  MSKSR,
  CLRSR,
  ATIM0,
  ATIM1,
  TIM0,
  TIM1: longword;
 end;

 TLEDTSRegisters = record
  ID,
  GLOBCTL,
  FNCTL,
  EVFR,
  TSVAL,
  LINE0,
  LINE1,
  LDCMP0,
  LDCMP1,
  TSCMP0,
  TSCMP1: longword;
 end;

 TEBURegisters = record
  CLC,
  MODCON,
  ID,
  USERCON,
  res0,res1,
  ADDRSEL0,
  ADDRSEL1,
  ADDRSEL2,
  ADDRSEL3,
  BUSRCON0,
  BUSRAP0,
  BUSWCON0,
  BUSWAP0,
  BUSRCON1,
  BUSRAP1,
  BUSWCON1,
  BUSWAP1,
  BUSRCON2,
  BUSRAP2,
  BUSWCON2,
  BUSWAP2,
  BUSRCON3,
  BUSRAP3,
  BUSWCON3,
  BUSWAP3,
  SDRMCON,
  SDRMOD,
  SDRMREF,
  SDRSTAT: longword;
 end;

 TETHRegisters = record
  MacConfiguration,
  MacFrameFilter,
  HashTableHigh,
  HashTableLow,
  GmiiAddress,
  GmiiData,
  FlowControl,
  VlanTag,
  Version,
  Debug,
  RemoteWakeUpFrameFilter,
  PmtControlStatus,
  res0,res1,
  InterruptStatus,
  InterruptMask,
  MacAddress0High,
  MacAddress0Low,
  MacAddress1High,
  MacAddress1Low,
  MacAddress2High,
  MacAddress2Low,
  MacAddress3High,
  MacAddress3Low: longword;
  res2: array[0..38] of longword;
  // $100 - $288 MMC Management Counters
  MmcRegs: array[0..96] of longword;
  res3: array[0..286] of longword;
  // $700 - $72C IEEE1588
  TimestampControl,
  SubSecondIncrement,
  SystemTimeSeconds,
  SystemTimeNanoseconds,
  SystemTimeSecondsUpdate,
  SystemTimeNanosecondsUpdate,
  TimestampAddend,
  TargetTimeSeconds,
  TargetTimeNanoseconds,
  SystemTimeHigherWordSeconds,
  TimestampStatus,
  PpsControl: longword;
  res4: array[0..563] of longword;
  // $1000 - $1024 DMA
  BusMode,
  TransmitPollDemand,
  ReceivePollDemand,
  ReceiveDescriptorListAddress,
  TransmitDescriptorListAddress,
  Status,
  OperationMode,
  InterruptEnable,
  MissedFrameAndBufferOverflowCounter,
  ReceiveInterruptWatchdogTimer,
  res5,
  AhbStatus: longword;
  doNotUse: array[0..5] of longword;
  CurrentHostTransmitDescriptor,
  CurrentHostReceiveDescriptor,
  CurrentHostTransmitBufferAddress,
  CurrentHostReceiveBufferAddress,
  HWFeatures: longword;
 end;

 TGPIORegisters = record
  Output,
  OMR,
  res0,res1: longword;

  IOCR: array[0..3] of longword;

  res2,
  Input: longword;
  res3: array[0..5] of longword;

  PDR: array[0..1] of longword;
  res5: array[0..5] of longword;

  PDISC: longword;
  res6: array[0..2] of longword;

  PPS,
  HWSel: longword;
  res7: array[0..33] of longword;
 end;

{$ALIGN 2}
var
 // Peripheral bus registers
 PBA0: TPBARegisters       absolute Peripheral0Base;
 PBA1: TPBARegisters       absolute Peripheral1Base;

 // PMU - Program memory unit
 PMU0_ID: longword         absolute Peripheral3Base+$0508;

 // PREF - Prefetch unit
 PREF_PCON: longword       absolute Peripheral3Base+$04000;

 FLASH0: TFLASHRegisters   absolute Peripheral3Base+$02000;

 WDT: TWDTRegisters        absolute Peripheral2Base+$08000;

 RTC: TRTCRegisters        absolute Peripheral2Base+$04A00;

 LEDTS0: TLEDTSRegisters   absolute Peripheral1Base+$10000;

 EBU: TEBURegisters        absolute Peripheral3Base+$08000;

 ETH: TETHRegisters        absolute Peripheral2Base+$0C000;

 // GPIO
 P0: TGPIORegisters        absolute Peripheral1Base+$28000;
 P1: TGPIORegisters        absolute Peripheral1Base+$28100;
 P2: TGPIORegisters        absolute Peripheral1Base+$28200;
 P3: TGPIORegisters        absolute Peripheral1Base+$28300;
 P4: TGPIORegisters        absolute Peripheral1Base+$28400;
 P5: TGPIORegisters        absolute Peripheral1Base+$28500;
 P6: TGPIORegisters        absolute Peripheral1Base+$28600;
 P14: TGPIORegisters       absolute Peripheral1Base+$28E00;
 P15: TGPIORegisters       absolute Peripheral1Base+$28F00;

 SDMMC_BLOCK_SIZE: longword         absolute Peripheral1Base+$1C004;
 SDMMC_BLOCK_COUNT: longword        absolute Peripheral1Base+$1C006;
 SDMMC_ARGUMENT1: longword          absolute Peripheral1Base+$1C008;
 SDMMC_TRANSFER_MODE: longword      absolute Peripheral1Base+$1C00C;
 SDMMC_COMMAND: longword            absolute Peripheral1Base+$1C00E;
 SDMMC_RESPONSE0: longword          absolute Peripheral1Base+$1C010;
 SDMMC_RESPONSE2: longword          absolute Peripheral1Base+$1C014;
 SDMMC_RESPONSE4: longword          absolute Peripheral1Base+$1C018;
 SDMMC_RESPONSE6: longword          absolute Peripheral1Base+$1C01C;
 SDMMC_DATA_BUFFER: longword        absolute Peripheral1Base+$1C020;
 SDMMC_PRESENT_STATE: longword      absolute Peripheral1Base+$1C024;
 SDMMC_HOST_CTRL: longword          absolute Peripheral1Base+$1C028;
 SDMMC_POWER_CTRL: longword         absolute Peripheral1Base+$1C029;
 SDMMC_BLOCK_GAP_CTRL: longword     absolute Peripheral1Base+$1C02A;
 SDMMC_WAKEUP_CTRL: longword        absolute Peripheral1Base+$1C02B;
 SDMMC_CLOCK_CTRL: longword         absolute Peripheral1Base+$1C02C;
 SDMMC_TIMEOUT_CTRL: longword       absolute Peripheral1Base+$1C02E;
 SDMMC_SW_RESET: longword           absolute Peripheral1Base+$1C02F;
 SDMMC_INT_STATUS_NORM: longword    absolute Peripheral1Base+$1C030;
 SDMMC_INT_STATUS_ERR: longword     absolute Peripheral1Base+$1C032;
 SDMMC_EN_INT_STATUS_NORM: longword absolute Peripheral1Base+$1C034;
 SDMMC_EN_INT_STATUS_ERR: longword  absolute Peripheral1Base+$1C036;
 SDMMC_EN_INT_SIGNAL_NORM: longword absolute Peripheral1Base+$1C038;
 SDMMC_ACMD_ERR_STATUS: longword    absolute Peripheral1Base+$1C03C;
 SDMMC_FORCE_EVENT_ACMD_ERR_STATUS: longword absolute Peripheral1Base+$1C050;
 SDMMC_FORCE_EVENT_ERR_STATUS: longword      absolute Peripheral1Base+$1C052;
 SDMMC_DEBUG_SEL: longword          absolute Peripheral1Base+$1C074;
 SDMMC_SPI: longword                absolute Peripheral1Base+$1C0F0;
 SDMMC_SLOT_INT_STATUS: longword    absolute Peripheral1Base+$1C0FC;

 GCU_ID: longword          absolute GCUBase+$000;
 GCU_IDCHIP: longword      absolute GCUBase+$004;
 GCU_IDMANUF: longword     absolute GCUBase+$008;
 GCU_STCON: longword       absolute GCUBase+$010;
 GCU_GPR0: longword        absolute GCUBase+$02C;
 GCU_GPR1: longword        absolute GCUBase+$030;
 GCU_ETH0_CON: longword    absolute GCUBase+$040;
 GCU_CCUCON: longword      absolute GCUBase+$04C;
 GCU_SRSTAT: longword      absolute GCUBase+$074;
 GCU_SRRAW: longword       absolute GCUBase+$078;
 GCU_SRMSK: longword       absolute GCUBase+$07C;
 GCU_SRCLR: longword       absolute GCUBase+$080;
 GCU_SRSET: longword       absolute GCUBase+$084;
 GCU_NMIREQEN: longword    absolute GCUBase+$088;
 GCU_DTSCON: longword      absolute GCUBase+$08C;
 GCU_DTSSTAT: longword     absolute GCUBase+$090;
 GCU_SDMMCDEL: longword    absolute GCUBase+$09C;
 GCU_G0RCEN: longword      absolute GCUBase+$0A0;
 GCU_G1RCEN: longword      absolute GCUBase+$0A4;
 GCU_MIRRSTS: longword     absolute GCUBase+$0C4;
 GCU_RMACR: longword       absolute GCUBase+$0C8;
 GCU_RMADATA: longword     absolute GCUBase+$0CC;
 GCU_PEEN: longword        absolute GCUBase+$13C;
 GCU_MCHKCON: longword     absolute GCUBase+$140;
 GCU_PETE: longword        absolute GCUBase+$144;
 GCU_PERSTEN: longword     absolute GCUBase+$147;
 GCU_PEFLAG: longword      absolute GCUBase+$150;
 GCU_PMTPR: longword       absolute GCUBase+$154;
 GCU_PMTSR: longword       absolute GCUBase+$158;
 GCU_TRAPSTAT: longword    absolute GCUBase+$160;
 GCU_TRAPRAW: longword     absolute GCUBase+$164;
 GCU_TRAPDIS: longword     absolute GCUBase+$168;
 GCU_TRAPCLR: longword     absolute GCUBase+$16C;
 GCU_TRAPSET: longword     absolute GCUBase+$170;

 PCU_PWRSTAT: longword     absolute PCUBase+$00;
 PCU_PWRSET: longword      absolute PCUBase+$04;
 PCU_PWRCLR: longword      absolute PCUBase+$08;
 PCU_EVRSTAT: longword     absolute PCUBase+$10;
 PCU_EVRVADCSTAT: longword absolute PCUBase+$14;
 PCU_PWRMON: longword      absolute PCUBase+$2C;

 HCU_HDSTAT:longword       absolute HCUBase+$00;
 HCU_HDCLR:longword        absolute HCUBase+$04;
 HCU_HDSET:longword        absolute HCUBase+$08;
 HCU_HDCR:longword         absolute HCUBase+$0C;
 HCU_OSCSICTRL:longword    absolute HCUBase+$14;
 HCU_OSCULSTAT:longword    absolute HCUBase+$18;
 HCU_OSCULCTRL:longword    absolute HCUBase+$1C;

 RCU_RSTSTAT: longword     absolute RCUBase+$00;
 RCU_RSTSET: longword      absolute RCUBase+$04;
 RCU_RSTCLR: longword      absolute RCUBase+$08;
 RCU_PRSTAT0: longword     absolute RCUBase+$0C;
 RCU_PRSET0: longword      absolute RCUBase+$10;
 RCU_PRCLR0: longword      absolute RCUBase+$14;
 RCU_PRSTAT1: longword     absolute RCUBase+$18;
 RCU_PRSET1: longword      absolute RCUBase+$1C;
 RCU_PRCLR1: longword      absolute RCUBase+$20;
 RCU_PRSTAT2: longword     absolute RCUBase+$24;
 RCU_PRSET2: longword      absolute RCUBase+$28;
 RCU_PRCLR2: longword      absolute RCUBase+$2C;
 RCU_PRSTAT3: longword     absolute RCUBase+$30;
 RCU_PRSET3: longword      absolute RCUBase+$34;
 RCU_PRCLR3: longword      absolute RCUBase+$38;

 CCU_CLKSTAT: longword     absolute CCUBase+$000;
 CCU_CLKSET: longword      absolute CCUBase+$004;
 CCU_CLKCLR: longword      absolute CCUBase+$008;
 CCU_SYSCLKCR: longword    absolute CCUBase+$00C;
 CCU_CPUCLKCR: longword    absolute CCUBase+$010;
 CCU_PBCLKCR: longword     absolute CCUBase+$014;
 CCU_USBCLKCR: longword    absolute CCUBase+$018;
 CCU_EBUCLKCR: longword    absolute CCUBase+$01C;
 CCU_CCUCLKCR: longword    absolute CCUBase+$020;
 CCU_WDTCLKCR: longword    absolute CCUBase+$024;
 CCU_EXTCLKCR: longword    absolute CCUBase+$028;
 CCU_SLEEPCR: longword     absolute CCUBase+$030;
 CCU_DSLEEPCR: longword    absolute CCUBase+$034;
 CCU_OSCHPSTAT: longword   absolute CCUBase+$100;
 CCU_OSCHPCTRL: longword   absolute CCUBase+$104;
 CCU_CLKCALCONST: longword absolute CCUBase+$10C;
 CCU_PLLSTAT: longword     absolute CCUBase+$110;
 CCU_PLLCON0: longword     absolute CCUBase+$114;
 CCU_PLLCON1: longword     absolute CCUBase+$118;
 CCU_PLLCON2: longword     absolute CCUBase+$11C;
 CCU_USBPLLSTAT: longword  absolute CCUBase+$120;
 CCU_USBPLLCON: longword   absolute CCUBase+$124;
 CCU_CLKMXSTAT: longword   absolute CCUBase+$138;

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

procedure SCU_SR0_irq; external name 'SCU_SR0_irq';
procedure ERU0_SR0_irq; external name 'ERU0_SR0_irq';
procedure ERU0_SR1_irq; external name 'ERU0_SR1_irq';
procedure ERU0_SR2_irq; external name 'ERU0_SR2_irq';
procedure ERU0_SR3_irq; external name 'ERU0_SR3_irq';
procedure ERU1_SR0_irq; external name 'ERU1_SR0_irq';
procedure ERU1_SR1_irq; external name 'ERU1_SR1_irq';
procedure ERU1_SR2_irq; external name 'ERU1_SR2_irq';
procedure ERU1_SR3_irq; external name 'ERU1_SR3_irq';
procedure PMU0_SR0_irq; external name 'PMU0_SR0_irq';
procedure CADC_C0SR0_irq; external name 'CADC_C0SR0_irq';
procedure CADC_C0SR1_irq; external name 'CADC_C0SR1_irq';
procedure CADC_C0SR2_irq; external name 'CADC_C0SR2_irq';
procedure CADC_C0SR3_irq; external name 'CADC_C0SR3_irq';
procedure CADC_G0SR0_irq; external name 'CADC_G0SR0_irq';
procedure CADC_G0SR1_irq; external name 'CADC_G0SR1_irq';
procedure CADC_G0SR2_irq; external name 'CADC_G0SR2_irq';
procedure CADC_G0SR3_irq; external name 'CADC_G0SR3_irq';
procedure CADC_G1SR0_irq; external name 'CADC_G1SR0_irq';
procedure CADC_G1SR1_irq; external name 'CADC_G1SR1_irq';
procedure CADC_G1SR2_irq; external name 'CADC_G1SR2_irq';
procedure CADC_G1SR3_irq; external name 'CADC_G1SR3_irq';
procedure CADC_G2SR0_irq; external name 'CADC_G2SR0_irq';
procedure CADC_G2SR1_irq; external name 'CADC_G2SR1_irq';
procedure CADC_G2SR2_irq; external name 'CADC_G2SR2_irq';
procedure CADC_G2SR3_irq; external name 'CADC_G2SR3_irq';
procedure CADC_G3SR0_irq; external name 'CADC_G3SR0_irq';
procedure CADC_G3SR1_irq; external name 'CADC_G3SR1_irq';
procedure CADC_G3SR2_irq; external name 'CADC_G3SR2_irq';
procedure CADC_G3SR3_irq; external name 'CADC_G3SR3_irq';
procedure DSD_SRM0_irq; external name 'DSD_SRM0_irq';
procedure DSD_SRM1_irq; external name 'DSD_SRM1_irq';
procedure DSD_SRM2_irq; external name 'DSD_SRM2_irq';
procedure DSD_SRM3_irq; external name 'DSD_SRM3_irq';
procedure DSD_SRA0_irq; external name 'DSD_SRA0_irq';
procedure DSD_SRA1_irq; external name 'DSD_SRA1_irq';
procedure DSD_SRA2_irq; external name 'DSD_SRA2_irq';
procedure DSD_SRA3_irq; external name 'DSD_SRA3_irq';
procedure DAC_SR0_irq; external name 'DAC_SR0_irq';
procedure DAC_SR1_irq; external name 'DAC_SR1_irq';
procedure CCU40_SR0_irq; external name 'CCU40_SR0_irq';
procedure CCU40_SR1_irq; external name 'CCU40_SR1_irq';
procedure CCU40_SR2_irq; external name 'CCU40_SR2_irq';
procedure CCU40_SR3_irq; external name 'CCU40_SR3_irq';
procedure CCU41_SR0_irq; external name 'CCU41_SR0_irq';
procedure CCU41_SR1_irq; external name 'CCU41_SR1_irq';
procedure CCU41_SR2_irq; external name 'CCU41_SR2_irq';
procedure CCU41_SR3_irq; external name 'CCU41_SR3_irq';
procedure CCU42_SR0_irq; external name 'CCU42_SR0_irq';
procedure CCU42_SR1_irq; external name 'CCU42_SR1_irq';
procedure CCU42_SR2_irq; external name 'CCU42_SR2_irq';
procedure CCU42_SR3_irq; external name 'CCU42_SR3_irq';
procedure CCU43_SR0_irq; external name 'CCU43_SR0_irq';
procedure CCU43_SR1_irq; external name 'CCU43_SR1_irq';
procedure CCU43_SR2_irq; external name 'CCU43_SR2_irq';
procedure CCU43_SR3_irq; external name 'CCU43_SR3_irq';
procedure CCU80_SR0_irq; external name 'CCU80_SR0_irq';
procedure CCU80_SR1_irq; external name 'CCU80_SR1_irq';
procedure CCU80_SR2_irq; external name 'CCU80_SR2_irq';
procedure CCU80_SR3_irq; external name 'CCU80_SR3_irq';
procedure CCU81_SR0_irq; external name 'CCU81_SR0_irq';
procedure CCU81_SR1_irq; external name 'CCU81_SR1_irq';
procedure CCU81_SR2_irq; external name 'CCU81_SR2_irq';
procedure CCU81_SR3_irq; external name 'CCU81_SR3_irq';
procedure POSIF0_SR0_irq; external name 'POSIF0_SR0_irq';
procedure POSIF0_SR1_irq; external name 'POSIF0_SR1_irq';
procedure POSIF1_SR0_irq; external name 'POSIF1_SR0_irq';
procedure POSIF1_SR1_irq; external name 'POSIF1_SR1_irq';
procedure CAN_SR0_irq; external name 'CAN_SR0_irq';
procedure CAN_SR1_irq; external name 'CAN_SR1_irq';
procedure CAN_SR2_irq; external name 'CAN_SR2_irq';
procedure CAN_SR3_irq; external name 'CAN_SR3_irq';
procedure CAN_SR4_irq; external name 'CAN_SR4_irq';
procedure CAN_SR5_irq; external name 'CAN_SR5_irq';
procedure CAN_SR6_irq; external name 'CAN_SR6_irq';
procedure CAN_SR7_irq; external name 'CAN_SR7_irq';
procedure USIC0_SR0_irq; external name 'USIC0_SR0_irq';
procedure USIC0_SR1_irq; external name 'USIC0_SR1_irq';
procedure USIC0_SR2_irq; external name 'USIC0_SR2_irq';
procedure USIC0_SR3_irq; external name 'USIC0_SR3_irq';
procedure USIC0_SR4_irq; external name 'USIC0_SR4_irq';
procedure USIC0_SR5_irq; external name 'USIC0_SR5_irq';
procedure USIC1_SR0_irq; external name 'USIC1_SR0_irq';
procedure USIC1_SR1_irq; external name 'USIC1_SR1_irq';
procedure USIC1_SR2_irq; external name 'USIC1_SR2_irq';
procedure USIC1_SR3_irq; external name 'USIC1_SR3_irq';
procedure USIC1_SR4_irq; external name 'USIC1_SR4_irq';
procedure USIC1_SR5_irq; external name 'USIC1_SR5_irq';
procedure USIC2_SR0_irq; external name 'USIC2_SR0_irq';
procedure USIC2_SR1_irq; external name 'USIC2_SR1_irq';
procedure USIC2_SR2_irq; external name 'USIC2_SR2_irq';
procedure USIC2_SR3_irq; external name 'USIC2_SR3_irq';
procedure USIC2_SR4_irq; external name 'USIC2_SR4_irq';
procedure USIC2_SR5_irq; external name 'USIC2_SR5_irq';
procedure LEDTS0_SR0_irq; external name 'LEDTS0_SR0_irq';
procedure FCE_SR0_irq; external name 'FCE_SR0_irq';
procedure GPDMA0_SR0_irq; external name 'GPDMA0_SR0_irq';
procedure SDMMC_SR0_irq; external name 'SDMMC_SR0_irq';
procedure USB0_SR0_irq; external name 'USB0_SR0_irq';
procedure ETH0_SR0_irq; external name 'ETH0_SR0_irq';
procedure GPDMA1_SR0_irq; external name 'GPDMA1_SR0_irq';

{$define REMAP_VECTTAB}

{$i cortexm4f_start.inc}

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

   .long SCU_SR0_irq
   .long ERU0_SR0_irq
   .long ERU0_SR1_irq
   .long ERU0_SR2_irq
   .long ERU0_SR3_irq
   .long ERU1_SR0_irq
   .long ERU1_SR1_irq
   .long ERU1_SR2_irq
   .long ERU1_SR3_irq
   .long 0
   .long 0
   .long 0
   .long PMU0_SR0_irq
   .long 0
   .long CADC_C0SR0_irq
   .long CADC_C0SR1_irq
   .long CADC_C0SR2_irq
   .long CADC_C0SR3_irq
   .long CADC_G0SR0_irq
   .long CADC_G0SR1_irq
   .long CADC_G0SR2_irq
   .long CADC_G0SR3_irq
   .long CADC_G1SR0_irq
   .long CADC_G1SR1_irq
   .long CADC_G1SR2_irq
   .long CADC_G1SR3_irq
   .long CADC_G2SR0_irq
   .long CADC_G2SR1_irq
   .long CADC_G2SR2_irq
   .long CADC_G2SR3_irq
   .long CADC_G3SR0_irq
   .long CADC_G3SR1_irq
   .long CADC_G3SR2_irq
   .long CADC_G3SR3_irq
   .long DSD_SRM0_irq
   .long DSD_SRM1_irq
   .long DSD_SRM2_irq
   .long DSD_SRM3_irq
   .long DSD_SRA0_irq
   .long DSD_SRA1_irq
   .long DSD_SRA2_irq
   .long DSD_SRA3_irq
   .long DAC_SR0_irq
   .long DAC_SR1_irq
   .long CCU40_SR0_irq
   .long CCU40_SR1_irq
   .long CCU40_SR2_irq
   .long CCU40_SR3_irq
   .long CCU41_SR0_irq
   .long CCU41_SR1_irq
   .long CCU41_SR2_irq
   .long CCU41_SR3_irq
   .long CCU42_SR0_irq
   .long CCU42_SR1_irq
   .long CCU42_SR2_irq
   .long CCU42_SR3_irq
   .long CCU43_SR0_irq
   .long CCU43_SR1_irq
   .long CCU43_SR2_irq
   .long CCU43_SR3_irq
   .long CCU80_SR0_irq
   .long CCU80_SR1_irq
   .long CCU80_SR2_irq
   .long CCU80_SR3_irq
   .long CCU81_SR0_irq
   .long CCU81_SR1_irq
   .long CCU81_SR2_irq
   .long CCU81_SR3_irq
   .long POSIF0_SR0_irq
   .long POSIF0_SR1_irq
   .long POSIF1_SR0_irq
   .long POSIF1_SR1_irq
   .long 0
   .long 0
   .long 0
   .long 0
   .long CAN_SR0_irq
   .long CAN_SR1_irq
   .long CAN_SR2_irq
   .long CAN_SR3_irq
   .long CAN_SR4_irq
   .long CAN_SR5_irq
   .long CAN_SR6_irq
   .long CAN_SR7_irq
   .long USIC0_SR0_irq
   .long USIC0_SR1_irq
   .long USIC0_SR2_irq
   .long USIC0_SR3_irq
   .long USIC0_SR4_irq
   .long USIC0_SR5_irq
   .long USIC1_SR0_irq
   .long USIC1_SR1_irq
   .long USIC1_SR2_irq
   .long USIC1_SR3_irq
   .long USIC1_SR4_irq
   .long USIC1_SR5_irq
   .long USIC2_SR0_irq
   .long USIC2_SR1_irq
   .long USIC2_SR2_irq
   .long USIC2_SR3_irq
   .long USIC2_SR4_irq
   .long USIC2_SR5_irq
   .long LEDTS0_SR0_irq
   .long 0
   .long FCE_SR0_irq
   .long GPDMA0_SR0_irq
   .long SDMMC_SR0_irq
   .long USB0_SR0_irq
   .long ETH0_SR0_irq
   .long 0
   .long GPDMA1_SR0_irq

   .weak NMI_interrupt
   .weak Hardfault_interrupt
   .weak MemManage_interrupt
   .weak BusFault_interrupt
   .weak UsageFault_interrupt
   .weak SWI_interrupt
   .weak DebugMonitor_interrupt
   .weak PendingSV_interrupt
   .weak SysTick_interrupt

   .weak SCU_SR0_irq
   .weak ERU0_SR0_irq
   .weak ERU0_SR1_irq
   .weak ERU0_SR2_irq
   .weak ERU0_SR3_irq
   .weak ERU1_SR0_irq
   .weak ERU1_SR1_irq
   .weak ERU1_SR2_irq
   .weak ERU1_SR3_irq
   .weak PMU0_SR0_irq
   .weak CADC_C0SR0_irq
   .weak CADC_C0SR1_irq
   .weak CADC_C0SR2_irq
   .weak CADC_C0SR3_irq
   .weak CADC_G0SR0_irq
   .weak CADC_G0SR1_irq
   .weak CADC_G0SR2_irq
   .weak CADC_G0SR3_irq
   .weak CADC_G1SR0_irq
   .weak CADC_G1SR1_irq
   .weak CADC_G1SR2_irq
   .weak CADC_G1SR3_irq
   .weak CADC_G2SR0_irq
   .weak CADC_G2SR1_irq
   .weak CADC_G2SR2_irq
   .weak CADC_G2SR3_irq
   .weak CADC_G3SR0_irq
   .weak CADC_G3SR1_irq
   .weak CADC_G3SR2_irq
   .weak CADC_G3SR3_irq
   .weak DSD_SRM0_irq
   .weak DSD_SRM1_irq
   .weak DSD_SRM2_irq
   .weak DSD_SRM3_irq
   .weak DSD_SRA0_irq
   .weak DSD_SRA1_irq
   .weak DSD_SRA2_irq
   .weak DSD_SRA3_irq
   .weak DAC_SR0_irq
   .weak DAC_SR1_irq
   .weak CCU40_SR0_irq
   .weak CCU40_SR1_irq
   .weak CCU40_SR2_irq
   .weak CCU40_SR3_irq
   .weak CCU41_SR0_irq
   .weak CCU41_SR1_irq
   .weak CCU41_SR2_irq
   .weak CCU41_SR3_irq
   .weak CCU42_SR0_irq
   .weak CCU42_SR1_irq
   .weak CCU42_SR2_irq
   .weak CCU42_SR3_irq
   .weak CCU43_SR0_irq
   .weak CCU43_SR1_irq
   .weak CCU43_SR2_irq
   .weak CCU43_SR3_irq
   .weak CCU80_SR0_irq
   .weak CCU80_SR1_irq
   .weak CCU80_SR2_irq
   .weak CCU80_SR3_irq
   .weak CCU81_SR0_irq
   .weak CCU81_SR1_irq
   .weak CCU81_SR2_irq
   .weak CCU81_SR3_irq
   .weak POSIF0_SR0_irq
   .weak POSIF0_SR1_irq
   .weak POSIF1_SR0_irq
   .weak POSIF1_SR1_irq
   .weak CAN_SR0_irq
   .weak CAN_SR1_irq
   .weak CAN_SR2_irq
   .weak CAN_SR3_irq
   .weak CAN_SR4_irq
   .weak CAN_SR5_irq
   .weak CAN_SR6_irq
   .weak CAN_SR7_irq
   .weak USIC0_SR0_irq
   .weak USIC0_SR1_irq
   .weak USIC0_SR2_irq
   .weak USIC0_SR3_irq
   .weak USIC0_SR4_irq
   .weak USIC0_SR5_irq
   .weak USIC1_SR0_irq
   .weak USIC1_SR1_irq
   .weak USIC1_SR2_irq
   .weak USIC1_SR3_irq
   .weak USIC1_SR4_irq
   .weak USIC1_SR5_irq
   .weak USIC2_SR0_irq
   .weak USIC2_SR1_irq
   .weak USIC2_SR2_irq
   .weak USIC2_SR3_irq
   .weak USIC2_SR4_irq
   .weak USIC2_SR5_irq
   .weak LEDTS0_SR0_irq
   .weak FCE_SR0_irq
   .weak GPDMA0_SR0_irq
   .weak SDMMC_SR0_irq
   .weak USB0_SR0_irq
   .weak ETH0_SR0_irq
   .weak GPDMA1_SR0_irq

   .set NMI_interrupt, HaltProc
   .set Hardfault_interrupt, HaltProc
   .set MemManage_interrupt, HaltProc
   .set BusFault_interrupt, HaltProc
   .set UsageFault_interrupt, HaltProc
   .set SWI_interrupt, HaltProc
   .set DebugMonitor_interrupt, HaltProc
   .set PendingSV_interrupt, HaltProc
   .set SysTick_interrupt, HaltProc

   .set SCU_SR0_irq, HaltProc
   .set ERU0_SR0_irq, HaltProc
   .set ERU0_SR1_irq, HaltProc
   .set ERU0_SR2_irq, HaltProc
   .set ERU0_SR3_irq, HaltProc
   .set ERU1_SR0_irq, HaltProc
   .set ERU1_SR1_irq, HaltProc
   .set ERU1_SR2_irq, HaltProc
   .set ERU1_SR3_irq, HaltProc
   .set PMU0_SR0_irq, HaltProc
   .set CADC_C0SR0_irq, HaltProc
   .set CADC_C0SR1_irq, HaltProc
   .set CADC_C0SR2_irq, HaltProc
   .set CADC_C0SR3_irq, HaltProc
   .set CADC_G0SR0_irq, HaltProc
   .set CADC_G0SR1_irq, HaltProc
   .set CADC_G0SR2_irq, HaltProc
   .set CADC_G0SR3_irq, HaltProc
   .set CADC_G1SR0_irq, HaltProc
   .set CADC_G1SR1_irq, HaltProc
   .set CADC_G1SR2_irq, HaltProc
   .set CADC_G1SR3_irq, HaltProc
   .set CADC_G2SR0_irq, HaltProc
   .set CADC_G2SR1_irq, HaltProc
   .set CADC_G2SR2_irq, HaltProc
   .set CADC_G2SR3_irq, HaltProc
   .set CADC_G3SR0_irq, HaltProc
   .set CADC_G3SR1_irq, HaltProc
   .set CADC_G3SR2_irq, HaltProc
   .set CADC_G3SR3_irq, HaltProc
   .set DSD_SRM0_irq, HaltProc
   .set DSD_SRM1_irq, HaltProc
   .set DSD_SRM2_irq, HaltProc
   .set DSD_SRM3_irq, HaltProc
   .set DSD_SRA0_irq, HaltProc
   .set DSD_SRA1_irq, HaltProc
   .set DSD_SRA2_irq, HaltProc
   .set DSD_SRA3_irq, HaltProc
   .set DAC_SR0_irq, HaltProc
   .set DAC_SR1_irq, HaltProc
   .set CCU40_SR0_irq, HaltProc
   .set CCU40_SR1_irq, HaltProc
   .set CCU40_SR2_irq, HaltProc
   .set CCU40_SR3_irq, HaltProc
   .set CCU41_SR0_irq, HaltProc
   .set CCU41_SR1_irq, HaltProc
   .set CCU41_SR2_irq, HaltProc
   .set CCU41_SR3_irq, HaltProc
   .set CCU42_SR0_irq, HaltProc
   .set CCU42_SR1_irq, HaltProc
   .set CCU42_SR2_irq, HaltProc
   .set CCU42_SR3_irq, HaltProc
   .set CCU43_SR0_irq, HaltProc
   .set CCU43_SR1_irq, HaltProc
   .set CCU43_SR2_irq, HaltProc
   .set CCU43_SR3_irq, HaltProc
   .set CCU80_SR0_irq, HaltProc
   .set CCU80_SR1_irq, HaltProc
   .set CCU80_SR2_irq, HaltProc
   .set CCU80_SR3_irq, HaltProc
   .set CCU81_SR0_irq, HaltProc
   .set CCU81_SR1_irq, HaltProc
   .set CCU81_SR2_irq, HaltProc
   .set CCU81_SR3_irq, HaltProc
   .set POSIF0_SR0_irq, HaltProc
   .set POSIF0_SR1_irq, HaltProc
   .set POSIF1_SR0_irq, HaltProc
   .set POSIF1_SR1_irq, HaltProc
   .set CAN_SR0_irq, HaltProc
   .set CAN_SR1_irq, HaltProc
   .set CAN_SR2_irq, HaltProc
   .set CAN_SR3_irq, HaltProc
   .set CAN_SR4_irq, HaltProc
   .set CAN_SR5_irq, HaltProc
   .set CAN_SR6_irq, HaltProc
   .set CAN_SR7_irq, HaltProc
   .set USIC0_SR0_irq, HaltProc
   .set USIC0_SR1_irq, HaltProc
   .set USIC0_SR2_irq, HaltProc
   .set USIC0_SR3_irq, HaltProc
   .set USIC0_SR4_irq, HaltProc
   .set USIC0_SR5_irq, HaltProc
   .set USIC1_SR0_irq, HaltProc
   .set USIC1_SR1_irq, HaltProc
   .set USIC1_SR2_irq, HaltProc
   .set USIC1_SR3_irq, HaltProc
   .set USIC1_SR4_irq, HaltProc
   .set USIC1_SR5_irq, HaltProc
   .set USIC2_SR0_irq, HaltProc
   .set USIC2_SR1_irq, HaltProc
   .set USIC2_SR2_irq, HaltProc
   .set USIC2_SR3_irq, HaltProc
   .set USIC2_SR4_irq, HaltProc
   .set USIC2_SR5_irq, HaltProc
   .set LEDTS0_SR0_irq, HaltProc
   .set FCE_SR0_irq, HaltProc
   .set GPDMA0_SR0_irq, HaltProc
   .set SDMMC_SR0_irq, HaltProc
   .set USB0_SR0_irq, HaltProc
   .set ETH0_SR0_irq, HaltProc
   .set GPDMA1_SR0_irq, HaltProc

   .text
end;

end.

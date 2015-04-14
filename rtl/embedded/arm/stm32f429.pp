{
Register definitions and utility code for STM32F429

Created by Jeppe Johansen 2015 - jeppe@j-software.dk
}
unit stm32f429;

{$goto on}

interface

{$PACKRECORDS 2}
const
 PeripheralBase 	= $40000000;

 FSMC_Base = $A0000000;
 RNG_Base = $50060800;
 HASH_Base = $50060400;
 CRYP_Base = $50060000;
 DCMI_Base = $50050000;
 USB_OTG_FS_Base = $50000000;
 USB_OTG_HS_Base = $40040000;
 DMA2D_Base = $4002B000;
 ETHERNET_Base = $40026400;
 DMA2_Base = $40026400;
 DMA1_Base = $40026000;
 BKPSRAM_Base = $40024000;
 FLASH_Base = $40023C00;
 RCC_Base = $40023800;
 CRC_Base = $40023000;
 GPIOK_Base = $40022800;
 GPIOJ_Base = $40022400;
 GPIOI_Base = $40022000;
 GPIOH_Base = $40021C00;
 GPIOG_Base = $40021800;
 GPIOF_Base = $40021400;
 GPIOE_Base = $40021000;
 GPIOD_Base = $40020C00;
 GPIOC_Base = $40020800;
 GPIOB_Base = $40020400;
 GPIOA_Base = $40020000;
 LCD_TFT_Base = $40016800;
 SAI1_Base = $40015800;
 SPI6_Base = $40015400;
 SPI5_Base = $40015000;
 TIM11_Base = $40014800;
 TIM10_Base = $40014400;
 TIM9_Base = $40014000;
 EXTI_Base = $40013C00;
 SYSCFG_Base = $40013800;
 SPI4_Base = $40013400;
 SPI1_Base = $40013000;
 SDIO_Base = $40012C00;
 ADC_Base = $40012000;
 USART6_Base = $40011600;
 USART1_Base = $40011000;
 TIM8_Base = $40010400;
 TIM1_Base = $40010000;
 UART8_Base = $40007C00;
 UART7_Base = $40007800;
 DAC_Base = $40007400;
 PWR_Base = $40007000;
 CAN2_Base = $40006800;
 CAN1_Base = $40006400;
 I2C3_Base = $40005C00;
 I2C2_Base = $40005800;
 I2C1_Base = $40005400;
 UART5_Base = $40005000;
 UART4_Base = $40004C00;
 USART3_Base = $40004800;
 USART2_Base = $40004400;
 I2S3ext_Base = $40004000;
 SPI3_Base = $40003C00;
 SPI2_Base = $40003800;
 I2S2ext_Base = $40003400;
 IWDG_Base = $40003000;
 WWDG_Base = $40002C00;
 RTC_Base = $40002800;
 TIM14_Base = $40002000;
 TIM13_Base = $40001C00;
 TIM12_Base = $40001800;
 TIM7_Base = $40001400;
 TIM6_Base = $40001000;
 TIM5_Base = $40000C00;
 TIM4_Base = $40000800;
 TIM3_Base = $40000400;
 TIM2_Base = $40000000;

type
 TPortRegisters = record
  MODER,
  OTYPER,
  OSPEEDER,
  PUPDR,
  IDR,
  ODR,
  BSRR,
  LCKR,
  AFRL,
  AFRH: longword;
 end;

 TRCCRegister = record
  CR,
  PLLCFGR,
  CFGR,
  CIR,
  AHB1RSTR,
  AHB2RSTR,
  AHB3RSTR,
  _res0,
  APB1RSTR,
  APB2RSTR,
  _res1,_res2,
  AHB1ENR,
  AHB2ENR,
  AHB3ENR,
  _res3,
  APB1ENR,
  APB2ENR,
  _res4,_res5,
  AHB1LPENR,
  AHB2LPENR,
  AHB3LPENR,
  _res6,
  APB1LPENR,
  APB2LPENR,
  _res7,_res8,
  BDCR,
  CSR,
  _res9,_res10,
  SSCGR,
  PLLI2SCFGR,
  PLLSAICFGR,
  DCKCFGR: longword;
 end;

 TPWRRegisters = record
  CR,
  CSR: longword;
 end;

{$ALIGN 2}
var
 { GPIO }
 PortA: TPortRegisters		absolute GPIOA_Base;
 PortB: TPortRegisters		absolute GPIOB_Base;
 PortC: TPortRegisters		absolute GPIOC_Base;
 PortD: TPortRegisters		absolute GPIOD_Base;
 PortE: TPortRegisters		absolute GPIOE_Base;
 PortF: TPortRegisters		absolute GPIOF_Base;
 PortG: TPortRegisters		absolute GPIOG_Base;
 PortH: TPortRegisters		absolute GPIOH_Base;
 PortI: TPortRegisters		absolute GPIOI_Base;
 PortJ: TPortRegisters		absolute GPIOJ_Base;
 PortK: TPortRegisters		absolute GPIOK_Base;

 { RCC }
 RCC: TRCCRegister        absolute RCC_Base;

 { PWR }
 PWR: TPWRRegisters       absolute PWR_Base;

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

procedure WWDG_interrupt; external name 'WWDG_interrupt';
procedure PVD_interrupt; external name 'PVD_interrupt';
procedure TAMP_STAMP_interrupt; external name 'TAMP_STAMP_interrupt';
procedure RTC_WKUP_interrupt; external name 'RTC_WKUP_interrupt';
procedure FLASH_interrupt; external name 'FLASH_interrupt';
procedure RCC_interrupt; external name 'RCC_interrupt';
procedure EXTI0_interrupt; external name 'EXTI0_interrupt';
procedure EXTI1_interrupt; external name 'EXTI1_interrupt';
procedure EXTI2_interrupt; external name 'EXTI2_interrupt';
procedure EXTI3_interrupt; external name 'EXTI3_interrupt';
procedure EXTI4_interrupt; external name 'EXTI4_interrupt';
procedure DMA1_Channel0_interrupt; external name 'DMA1_Channel0_interrupt';
procedure DMA1_Channel1_interrupt; external name 'DMA1_Channel1_interrupt';
procedure DMA1_Channel2_interrupt; external name 'DMA1_Channel2_interrupt';
procedure DMA1_Channel3_interrupt; external name 'DMA1_Channel3_interrupt';
procedure DMA1_Channel4_interrupt; external name 'DMA1_Channel4_interrupt';
procedure DMA1_Channel5_interrupt; external name 'DMA1_Channel5_interrupt';
procedure DMA1_Channel6_interrupt; external name 'DMA1_Channel6_interrupt';
procedure ADC1_2_3_interrupt; external name 'ADC1_2_3_interrupt';
procedure CAN1_TX_interrupt; external name 'CAN1_TX_interrupt';
procedure CAN1_RX0_interrupt; external name 'CAN1_RX0_interrupt';
procedure CAN1_RX1_interrupt; external name 'CAN1_RX1_interrupt';
procedure CAN1_SCE_interrupt; external name 'CAN1_SCE_interrupt';
procedure EXTI9_5_interrupt; external name 'EXTI9_5_interrupt';
procedure TIM1_BRK_TIM9_interrupt; external name 'TIM1_BRK_TIM9_interrupt';
procedure TIM1_UP_TIM10_interrupt; external name 'TIM1_UP_TIM10_interrupt';
procedure TIM1_TRG_COM_TIM11_interruptirq; external name 'TIM1_TRG_COM_TIM11_interruptirq';
procedure TIM1_CC_interrupt; external name 'TIM1_CC_interrupt';
procedure TIM2_interrupt; external name 'TIM2_interrupt';
procedure TIM3_interrupt; external name 'TIM3_interrupt';
procedure TIM4_interrupt; external name 'TIM4_interrupt';
procedure I2C1_EV_interrupt; external name 'I2C1_EV_interrupt';
procedure I2C1_ER_interrupt; external name 'I2C1_ER_interrupt';
procedure I2C2_EV_interrupt; external name 'I2C2_EV_interrupt';
procedure I2C2_ER_interrupt; external name 'I2C2_ER_interrupt';
procedure SPI1_interrupt; external name 'SPI1_interrupt';
procedure SPI2_interrupt; external name 'SPI2_interrupt';
procedure USART1_interrupt; external name 'USART1_interrupt';
procedure USART2_interrupt; external name 'USART2_interrupt';
procedure USART3_interrupt; external name 'USART3_interrupt';
procedure EXTI15_10_interrupt; external name 'EXTI15_10_interrupt';
procedure RTCAlarm_interrupt; external name 'RTCAlarm_interrupt';
procedure OTG_FS_WKUP_interrupt; external name 'OTG_FS_WKUP_interrupt';
procedure TIM8_BRK_TIM12_interrupt; external name 'TIM8_BRK_TIM12_interrupt';
procedure TIM8_UP_TIM13_interrupt; external name 'TIM8_UP_TIM13_interrupt';
procedure TIM8_TRG_COM_TIM14_interrupt; external name 'TIM8_TRG_COM_TIM14_interrupt';
procedure TIM8_CC_interrupt; external name 'TIM8_CC_interrupt';
procedure DMA1_Stream7_interrupt; external name 'DMA1_Stream7_interrupt';
procedure FSMC_interrupt; external name 'FSMC_interrupt';
procedure SDIO_interrupt; external name 'SDIO_interrupt';
procedure TIM5_interrupt; external name 'TIM5_interrupt';
procedure SPI3_interrupt; external name 'SPI3_interrupt';
procedure UART4_interrupt; external name 'UART4_interrupt';
procedure UART5_interrupt; external name 'UART5_interrupt';
procedure TIM6_DAC1_2_interrupt; external name 'TIM6_DAC1_2_interrupt';
procedure TIM7_interrupt; external name 'TIM7_interrupt';
procedure DMA2_Stream0_interrupt; external name 'DMA2_Stream0_interrupt';
procedure DMA2_Stream1_interrupt; external name 'DMA2_Stream1_interrupt';
procedure DMA2_Stream2_interrupt; external name 'DMA2_Stream2_interrupt';
procedure DMA2_Stream3_interrupt; external name 'DMA2_Stream3_interrupt';
procedure DMA2_Stream4_interrupt; external name 'DMA2_Stream4_interrupt';
procedure ETH_interrupt; external name 'ETH_interrupt';
procedure ETHWKUP_interrupt; external name 'ETHWKUP_interrupt';
procedure CAN2_TX_interrupt; external name 'CAN2_TX_interrupt';
procedure CAN2_RX0_interrupt; external name 'CAN2_RX0_interrupt';
procedure CAN2_RX1_interrupt; external name 'CAN2_RX1_interrupt';
procedure CAN2_SCE_interrupt; external name 'CAN2_SCE_interrupt';
procedure OTG_FS_interrupt; external name 'OTG_FS_interrupt';
procedure DMA2_Stream5_interrupt; external name 'DMA2_Stream5_interrupt';
procedure DMA2_Stream6_interrupt; external name 'DMA2_Stream6_interrupt';
procedure DMA2_Stream7_interrupt; external name 'DMA2_Stream7_interrupt';
procedure USART6_interrupt; external name 'USART6_interrupt';
procedure I2C3_EV_interrupt; external name 'I2C3_EV_interrupt';
procedure I2C3_ER_interrupt; external name 'I2C3_ER_interrupt';
procedure OTG_HS_EP1_OUT_interrupt; external name 'OTG_HS_EP1_OUT_interrupt';
procedure OTG_HS_EP1_IN_interrupt; external name 'OTG_HS_EP1_IN_interrupt';
procedure OTG_HS_WKUP_interrupt; external name 'OTG_HS_WKUP_interrupt';
procedure OTG_HS_interrupt; external name 'OTG_HS_interrupt';
procedure DCMI_interrupt; external name 'DCMI_interrupt';
procedure CRYP_interrupt; external name 'CRYP_interrupt';
procedure HASH_RNG_interrupt; external name 'HASH_RNG_interrupt';
procedure FPU_interrupt; external name 'FPU_interrupt';
procedure UART7_interrupt; external name 'UART7_interrupt';
procedure UART8_interrupt; external name 'UART8_interrupt';
procedure SPI4_interrupt; external name 'SPI4_interrupt';
procedure LTDC_interrupt; external name 'LTDC_interrupt';
procedure LTDC_ERR_interrupt; external name 'LTDC_ERR_interrupt';
procedure DMA2D_interrupt; external name 'DMA2D_interrupt';

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

   .long WWDG_interrupt
   .long PVD_interrupt
   .long TAMP_STAMP_interrupt
   .long RTC_WKUP_interrupt
   .long FLASH_interrupt
   .long RCC_interrupt
   .long EXTI0_interrupt
   .long EXTI1_interrupt
   .long EXTI2_interrupt
   .long EXTI3_interrupt
   .long EXTI4_interrupt
   .long DMA1_Channel0_interrupt
   .long DMA1_Channel1_interrupt
   .long DMA1_Channel2_interrupt
   .long DMA1_Channel3_interrupt
   .long DMA1_Channel4_interrupt
   .long DMA1_Channel5_interrupt
   .long DMA1_Channel6_interrupt
   .long ADC1_2_3_interrupt
   .long CAN1_TX_interrupt
   .long CAN1_RX0_interrupt
   .long CAN1_RX1_interrupt
   .long CAN1_SCE_interrupt
   .long EXTI9_5_interrupt
   .long TIM1_BRK_TIM9_interrupt
   .long TIM1_UP_TIM10_interrupt
   .long TIM1_TRG_COM_TIM11_interruptirq
   .long TIM1_CC_interrupt
   .long TIM2_interrupt
   .long TIM3_interrupt
   .long TIM4_interrupt
   .long I2C1_EV_interrupt
   .long I2C1_ER_interrupt
   .long I2C2_EV_interrupt
   .long I2C2_ER_interrupt
   .long SPI1_interrupt
   .long SPI2_interrupt
   .long USART1_interrupt
   .long USART2_interrupt
   .long USART3_interrupt
   .long EXTI15_10_interrupt
   .long RTCAlarm_interrupt
   .long OTG_FS_WKUP_interrupt
   .long TIM8_BRK_TIM12_interrupt
   .long TIM8_UP_TIM13_interrupt
   .long TIM8_TRG_COM_TIM14_interrupt
   .long TIM8_CC_interrupt
   .long DMA1_Stream7_interrupt
   .long FSMC_interrupt
   .long SDIO_interrupt
   .long TIM5_interrupt
   .long SPI3_interrupt
   .long UART4_interrupt
   .long UART5_interrupt
   .long TIM6_DAC1_2_interrupt
   .long TIM7_interrupt
   .long DMA2_Stream0_interrupt
   .long DMA2_Stream1_interrupt
   .long DMA2_Stream2_interrupt
   .long DMA2_Stream3_interrupt
   .long DMA2_Stream4_interrupt
   .long ETH_interrupt
   .long ETHWKUP_interrupt
   .long CAN2_TX_interrupt
   .long CAN2_RX0_interrupt
   .long CAN2_RX1_interrupt
   .long CAN2_SCE_interrupt
   .long OTG_FS_interrupt
   .long DMA2_Stream5_interrupt
   .long DMA2_Stream6_interrupt
   .long DMA2_Stream7_interrupt
   .long USART6_interrupt
   .long I2C3_EV_interrupt
   .long I2C3_ER_interrupt
   .long OTG_HS_EP1_OUT_interrupt
   .long OTG_HS_EP1_IN_interrupt
   .long OTG_HS_WKUP_interrupt
   .long OTG_HS_interrupt
   .long DCMI_interrupt
   .long CRYP_interrupt
   .long HASH_RNG_interrupt
   .long FPU_interrupt
   .long UART7_interrupt
   .long UART8_interrupt
   .long SPI4_interrupt
   .long LTDC_interrupt
   .long LTDC_ERR_interrupt
   .long DMA2D_interrupt
   
   .weak NMI_interrupt
   .weak Hardfault_interrupt
   .weak MemManage_interrupt
   .weak BusFault_interrupt
   .weak UsageFault_interrupt
   .weak SWI_interrupt
   .weak DebugMonitor_interrupt
   .weak PendingSV_interrupt
   .weak SysTick_interrupt

   .weak WWDG_interrupt
   .weak PVD_interrupt
   .weak TAMP_STAMP_interrupt
   .weak RTC_WKUP_interrupt
   .weak FLASH_interrupt
   .weak RCC_interrupt
   .weak EXTI0_interrupt
   .weak EXTI1_interrupt
   .weak EXTI2_interrupt
   .weak EXTI3_interrupt
   .weak EXTI4_interrupt
   .weak DMA1_Channel0_interrupt
   .weak DMA1_Channel1_interrupt
   .weak DMA1_Channel2_interrupt
   .weak DMA1_Channel3_interrupt
   .weak DMA1_Channel4_interrupt
   .weak DMA1_Channel5_interrupt
   .weak DMA1_Channel6_interrupt
   .weak ADC1_2_3_interrupt
   .weak CAN1_TX_interrupt
   .weak CAN1_RX0_interrupt
   .weak CAN1_RX1_interrupt
   .weak CAN1_SCE_interrupt
   .weak EXTI9_5_interrupt
   .weak TIM1_BRK_TIM9_interrupt
   .weak TIM1_UP_TIM10_interrupt
   .weak TIM1_TRG_COM_TIM11_interruptirq
   .weak TIM1_CC_interrupt
   .weak TIM2_interrupt
   .weak TIM3_interrupt
   .weak TIM4_interrupt
   .weak I2C1_EV_interrupt
   .weak I2C1_ER_interrupt
   .weak I2C2_EV_interrupt
   .weak I2C2_ER_interrupt
   .weak SPI1_interrupt
   .weak SPI2_interrupt
   .weak USART1_interrupt
   .weak USART2_interrupt
   .weak USART3_interrupt
   .weak EXTI15_10_interrupt
   .weak RTCAlarm_interrupt
   .weak OTG_FS_WKUP_interrupt
   .weak TIM8_BRK_TIM12_interrupt
   .weak TIM8_UP_TIM13_interrupt
   .weak TIM8_TRG_COM_TIM14_interrupt
   .weak TIM8_CC_interrupt
   .weak DMA1_Stream7_interrupt
   .weak FSMC_interrupt
   .weak SDIO_interrupt
   .weak TIM5_interrupt
   .weak SPI3_interrupt
   .weak UART4_interrupt
   .weak UART5_interrupt
   .weak TIM6_DAC1_2_interrupt
   .weak TIM7_interrupt
   .weak DMA2_Stream0_interrupt
   .weak DMA2_Stream1_interrupt
   .weak DMA2_Stream2_interrupt
   .weak DMA2_Stream3_interrupt
   .weak DMA2_Stream4_interrupt
   .weak ETH_interrupt
   .weak ETHWKUP_interrupt
   .weak CAN2_TX_interrupt
   .weak CAN2_RX0_interrupt
   .weak CAN2_RX1_interrupt
   .weak CAN2_SCE_interrupt
   .weak OTG_FS_interrupt
   .weak DMA2_Stream5_interrupt
   .weak DMA2_Stream6_interrupt
   .weak DMA2_Stream7_interrupt
   .weak USART6_interrupt
   .weak I2C3_EV_interrupt
   .weak I2C3_ER_interrupt
   .weak OTG_HS_EP1_OUT_interrupt
   .weak OTG_HS_EP1_IN_interrupt
   .weak OTG_HS_WKUP_interrupt
   .weak OTG_HS_interrupt
   .weak DCMI_interrupt
   .weak CRYP_interrupt
   .weak HASH_RNG_interrupt
   .weak FPU_interrupt
   .weak UART7_interrupt
   .weak UART8_interrupt
   .weak SPI4_interrupt
   .weak LTDC_interrupt
   .weak LTDC_ERR_interrupt
   .weak DMA2D_interrupt
   
   .set NMI_interrupt, HaltProc
   .set Hardfault_interrupt, HaltProc
   .set MemManage_interrupt, HaltProc
   .set BusFault_interrupt, HaltProc
   .set UsageFault_interrupt, HaltProc
   .set SWI_interrupt, HaltProc
   .set DebugMonitor_interrupt, HaltProc
   .set PendingSV_interrupt, HaltProc
   .set SysTick_interrupt, HaltProc

   .set WWDG_interrupt, HaltProc
   .set PVD_interrupt, HaltProc
   .set TAMP_STAMP_interrupt, HaltProc
   .set RTC_WKUP_interrupt, HaltProc
   .set FLASH_interrupt, HaltProc
   .set RCC_interrupt, HaltProc
   .set EXTI0_interrupt, HaltProc
   .set EXTI1_interrupt, HaltProc
   .set EXTI2_interrupt, HaltProc
   .set EXTI3_interrupt, HaltProc
   .set EXTI4_interrupt, HaltProc
   .set DMA1_Channel0_interrupt, HaltProc
   .set DMA1_Channel1_interrupt, HaltProc
   .set DMA1_Channel2_interrupt, HaltProc
   .set DMA1_Channel3_interrupt, HaltProc
   .set DMA1_Channel4_interrupt, HaltProc
   .set DMA1_Channel5_interrupt, HaltProc
   .set DMA1_Channel6_interrupt, HaltProc
   .set ADC1_2_3_interrupt, HaltProc
   .set CAN1_TX_interrupt, HaltProc
   .set CAN1_RX0_interrupt, HaltProc
   .set CAN1_RX1_interrupt, HaltProc
   .set CAN1_SCE_interrupt, HaltProc
   .set EXTI9_5_interrupt, HaltProc
   .set TIM1_BRK_TIM9_interrupt, HaltProc
   .set TIM1_UP_TIM10_interrupt, HaltProc
   .set TIM1_TRG_COM_TIM11_interruptirq, HaltProc
   .set TIM1_CC_interrupt, HaltProc
   .set TIM2_interrupt, HaltProc
   .set TIM3_interrupt, HaltProc
   .set TIM4_interrupt, HaltProc
   .set I2C1_EV_interrupt, HaltProc
   .set I2C1_ER_interrupt, HaltProc
   .set I2C2_EV_interrupt, HaltProc
   .set I2C2_ER_interrupt, HaltProc
   .set SPI1_interrupt, HaltProc
   .set SPI2_interrupt, HaltProc
   .set USART1_interrupt, HaltProc
   .set USART2_interrupt, HaltProc
   .set USART3_interrupt, HaltProc
   .set EXTI15_10_interrupt, HaltProc
   .set RTCAlarm_interrupt, HaltProc
   .set OTG_FS_WKUP_interrupt, HaltProc
   .set TIM8_BRK_TIM12_interrupt, HaltProc
   .set TIM8_UP_TIM13_interrupt, HaltProc
   .set TIM8_TRG_COM_TIM14_interrupt, HaltProc
   .set TIM8_CC_interrupt, HaltProc
   .set DMA1_Stream7_interrupt, HaltProc
   .set FSMC_interrupt, HaltProc
   .set SDIO_interrupt, HaltProc
   .set TIM5_interrupt, HaltProc
   .set SPI3_interrupt, HaltProc
   .set UART4_interrupt, HaltProc
   .set UART5_interrupt, HaltProc
   .set TIM6_DAC1_2_interrupt, HaltProc
   .set TIM7_interrupt, HaltProc
   .set DMA2_Stream0_interrupt, HaltProc
   .set DMA2_Stream1_interrupt, HaltProc
   .set DMA2_Stream2_interrupt, HaltProc
   .set DMA2_Stream3_interrupt, HaltProc
   .set DMA2_Stream4_interrupt, HaltProc
   .set ETH_interrupt, HaltProc
   .set ETHWKUP_interrupt, HaltProc
   .set CAN2_TX_interrupt, HaltProc
   .set CAN2_RX0_interrupt, HaltProc
   .set CAN2_RX1_interrupt, HaltProc
   .set CAN2_SCE_interrupt, HaltProc
   .set OTG_FS_interrupt, HaltProc
   .set DMA2_Stream5_interrupt, HaltProc
   .set DMA2_Stream6_interrupt, HaltProc
   .set DMA2_Stream7_interrupt, HaltProc
   .set USART6_interrupt, HaltProc
   .set I2C3_EV_interrupt, HaltProc
   .set I2C3_ER_interrupt, HaltProc
   .set OTG_HS_EP1_OUT_interrupt, HaltProc
   .set OTG_HS_EP1_IN_interrupt, HaltProc
   .set OTG_HS_WKUP_interrupt, HaltProc
   .set OTG_HS_interrupt, HaltProc
   .set DCMI_interrupt, HaltProc
   .set CRYP_interrupt, HaltProc
   .set HASH_RNG_interrupt, HaltProc
   .set FPU_interrupt, HaltProc
   .set UART7_interrupt, HaltProc
   .set UART8_interrupt, HaltProc
   .set SPI4_interrupt, HaltProc
   .set LTDC_interrupt, HaltProc
   .set LTDC_ERR_interrupt, HaltProc
   .set DMA2D_interrupt, HaltProc

   .text
end;

end.

unit stm32f407xx;
interface
{$PACKRECORDS 2}
{$GOTO ON}
{$MODESWITCH ADVANCEDRECORDS}
// *
// ******************************************************************************
// * @file    stm32f407xx.h
// * @author  MCD Application Team
// * @version V2.4.0
// * @date    14-August-2015
//   CMSIS STM32F407xx Device Peripheral Access Layer Header File.
// *
// *          This file contains:
// *           - Data structures and the address mapping for all peripherals
// *           - Peripheral's registers declarations and bits definition
// *           - Macros to access peripheral’s registers hardware
// *
// ******************************************************************************
// * @attention
// *
// * <h2><center>&copy; COPYRIGHT(c) 2015 STMicroelectronics</center></h2>
// *
// * Redistribution and use in source and binary forms, with or without modification,
// * are permitted provided that the following conditions are met:
// *   1. Redistributions of source code must retain the above copyright notice,
// *      this list of conditions and the following disclaimer.
// *   2. Redistributions in binary form must reproduce the above copyright notice,
// *      this list of conditions and the following disclaimer in the documentation
// *      and/or other materials provided with the distribution.
// *   3. Neither the name of STMicroelectronics nor the names of its contributors
// *      may be used to endorse or promote products derived from this software
// *      without specific prior written permission.
// *
// * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// *
// ******************************************************************************
// Configuration of the Cortex-M4 Processor and Core Peripherals
// STM32F4XX Interrupt Number Definition, according to the selected device
// *        in @ref Library_configuration_section

type
  TIRQn_Enum   = (
    NonMaskableInt_IRQn = -14,        // 2 Non Maskable Interrupt
    MemoryManagement_IRQn = -12,      // 4 Cortex-M4 Memory Management Interrupt
    BusFault_IRQn = -11,              // 5 Cortex-M4 Bus Fault Interrupt
    UsageFault_IRQn = -10,            // 6 Cortex-M4 Usage Fault Interrupt
    SVCall_IRQn = -5,                 // 11 Cortex-M4 SV Call Interrupt
    DebugMonitor_IRQn = -4,           // 12 Cortex-M4 Debug Monitor Interrupt
    PendSV_IRQn = -2,                 // 14 Cortex-M4 Pend SV Interrupt
    SysTick_IRQn = -1,                // 15 Cortex-M4 System Tick Interrupt
    WWDG_IRQn  = 0,                   // Window WatchDog Interrupt
    PVD_IRQn   = 1,                   // PVD through EXTI Line detection Interrupt
    TAMP_STAMP_IRQn = 2,              // Tamper and TimeStamp interrupts through the EXTI line
    RTC_WKUP_IRQn = 3,                // RTC Wakeup interrupt through the EXTI line
    FLASH_IRQn = 4,                   // FLASH global Interrupt
    RCC_IRQn   = 5,                   // RCC global Interrupt
    EXTI0_IRQn = 6,                   // EXTI Line0 Interrupt
    EXTI1_IRQn = 7,                   // EXTI Line1 Interrupt
    EXTI2_IRQn = 8,                   // EXTI Line2 Interrupt
    EXTI3_IRQn = 9,                   // EXTI Line3 Interrupt
    EXTI4_IRQn = 10,                  // EXTI Line4 Interrupt
    DMA1_Stream0_IRQn = 11,           // DMA1 Stream 0 global Interrupt
    DMA1_Stream1_IRQn = 12,           // DMA1 Stream 1 global Interrupt
    DMA1_Stream2_IRQn = 13,           // DMA1 Stream 2 global Interrupt
    DMA1_Stream3_IRQn = 14,           // DMA1 Stream 3 global Interrupt
    DMA1_Stream4_IRQn = 15,           // DMA1 Stream 4 global Interrupt
    DMA1_Stream5_IRQn = 16,           // DMA1 Stream 5 global Interrupt
    DMA1_Stream6_IRQn = 17,           // DMA1 Stream 6 global Interrupt
    ADC_IRQn   = 18,                  // ADC1, ADC2 and ADC3 global Interrupts
    CAN1_TX_IRQn = 19,                // CAN1 TX Interrupt
    CAN1_RX0_IRQn = 20,               // CAN1 RX0 Interrupt
    CAN1_RX1_IRQn = 21,               // CAN1 RX1 Interrupt
    CAN1_SCE_IRQn = 22,               // CAN1 SCE Interrupt
    EXTI9_5_IRQn = 23,                // External Line[9:5] Interrupts
    TIM1_BRK_TIM9_IRQn = 24,          // TIM1 Break interrupt and TIM9 global interrupt
    TIM1_UP_TIM10_IRQn = 25,          // TIM1 Update Interrupt and TIM10 global interrupt
    TIM1_TRG_COM_TIM11_IRQn = 26,     // TIM1 Trigger and Commutation Interrupt and TIM11 global interrupt
    TIM1_CC_IRQn = 27,                // TIM1 Capture Compare Interrupt
    TIM2_IRQn  = 28,                  // TIM2 global Interrupt
    TIM3_IRQn  = 29,                  // TIM3 global Interrupt
    TIM4_IRQn  = 30,                  // TIM4 global Interrupt
    I2C1_EV_IRQn = 31,                // I2C1 Event Interrupt
    I2C1_ER_IRQn = 32,                // I2C1 Error Interrupt
    I2C2_EV_IRQn = 33,                // I2C2 Event Interrupt
    I2C2_ER_IRQn = 34,                // I2C2 Error Interrupt
    SPI1_IRQn  = 35,                  // SPI1 global Interrupt
    SPI2_IRQn  = 36,                  // SPI2 global Interrupt
    USART1_IRQn = 37,                 // USART1 global Interrupt
    USART2_IRQn = 38,                 // USART2 global Interrupt
    USART3_IRQn = 39,                 // USART3 global Interrupt
    EXTI15_10_IRQn = 40,              // External Line[15:10] Interrupts
    RTC_Alarm_IRQn = 41,              // RTC Alarm (A and B) through EXTI Line Interrupt
    OTG_FS_WKUP_IRQn = 42,            // USB OTG FS Wakeup through EXTI line interrupt
    TIM8_BRK_TIM12_IRQn = 43,         // TIM8 Break Interrupt and TIM12 global interrupt
    TIM8_UP_TIM13_IRQn = 44,          // TIM8 Update Interrupt and TIM13 global interrupt
    TIM8_TRG_COM_TIM14_IRQn = 45,     // TIM8 Trigger and Commutation Interrupt and TIM14 global interrupt
    TIM8_CC_IRQn = 46,                // TIM8 Capture Compare Interrupt
    DMA1_Stream7_IRQn = 47,           // DMA1 Stream7 Interrupt
    FSMC_IRQn  = 48,                  // FSMC global Interrupt
    SDIO_IRQn  = 49,                  // SDIO global Interrupt
    TIM5_IRQn  = 50,                  // TIM5 global Interrupt
    SPI3_IRQn  = 51,                  // SPI3 global Interrupt
    UART4_IRQn = 52,                  // UART4 global Interrupt
    UART5_IRQn = 53,                  // UART5 global Interrupt
    TIM6_DAC_IRQn = 54,               // TIM6 global and DAC1&2 underrun error  interrupts
    TIM7_IRQn  = 55,                  // TIM7 global interrupt
    DMA2_Stream0_IRQn = 56,           // DMA2 Stream 0 global Interrupt
    DMA2_Stream1_IRQn = 57,           // DMA2 Stream 1 global Interrupt
    DMA2_Stream2_IRQn = 58,           // DMA2 Stream 2 global Interrupt
    DMA2_Stream3_IRQn = 59,           // DMA2 Stream 3 global Interrupt
    DMA2_Stream4_IRQn = 60,           // DMA2 Stream 4 global Interrupt
    ETH_IRQn   = 61,                  // Ethernet global Interrupt
    ETH_WKUP_IRQn = 62,               // Ethernet Wakeup through EXTI line Interrupt
    CAN2_TX_IRQn = 63,                // CAN2 TX Interrupt
    CAN2_RX0_IRQn = 64,               // CAN2 RX0 Interrupt
    CAN2_RX1_IRQn = 65,               // CAN2 RX1 Interrupt
    CAN2_SCE_IRQn = 66,               // CAN2 SCE Interrupt
    OTG_FS_IRQn = 67,                 // USB OTG FS global Interrupt
    DMA2_Stream5_IRQn = 68,           // DMA2 Stream 5 global interrupt
    DMA2_Stream6_IRQn = 69,           // DMA2 Stream 6 global interrupt
    DMA2_Stream7_IRQn = 70,           // DMA2 Stream 7 global interrupt
    USART6_IRQn = 71,                 // USART6 global interrupt
    I2C3_EV_IRQn = 72,                // I2C3 event interrupt
    I2C3_ER_IRQn = 73,                // I2C3 error interrupt
    OTG_HS_EP1_OUT_IRQn = 74,         // USB OTG HS End Point 1 Out global interrupt
    OTG_HS_EP1_IN_IRQn = 75,          // USB OTG HS End Point 1 In global interrupt
    OTG_HS_WKUP_IRQn = 76,            // USB OTG HS Wakeup through EXTI interrupt
    OTG_HS_IRQn = 77,                 // USB OTG HS global interrupt
    DCMI_IRQn  = 78,                  // DCMI global interrupt
    HASH_RNG_IRQn = 80,               // Hash and RNG global interrupt
    FPU_IRQn   = 81                   // FPU global interrupt
  );

  TADC_Registers = record
    SR         : longword;            // ADC status register
    CR1        : longword;            // ADC control register 1
    CR2        : longword;            // ADC control register 2
    SMPR1      : longword;            // ADC sample time register 1
    SMPR2      : longword;            // ADC sample time register 2
    JOFR1      : longword;            // ADC injected channel data offset register 1
    JOFR2      : longword;            // ADC injected channel data offset register 2
    JOFR3      : longword;            // ADC injected channel data offset register 3
    JOFR4      : longword;            // ADC injected channel data offset register 4
    HTR        : longword;            // ADC watchdog higher threshold register
    LTR        : longword;            // ADC watchdog lower threshold register
    SQR1       : longword;            // ADC regular sequence register 1
    SQR2       : longword;            // ADC regular sequence register 2
    SQR3       : longword;            // ADC regular sequence register 3
    JSQR       : longword;            // ADC injected sequence register
    JDR1       : longword;            // ADC injected data register 1
    JDR2       : longword;            // ADC injected data register 2
    JDR3       : longword;            // ADC injected data register 3
    JDR4       : longword;            // ADC injected data register 4
    DR         : longword;            // ADC regular data register
  end;

  TADC_COMMON_Registers = record
    CSR        : longword;            // ADC Common status register
    CCR        : longword;            // ADC common control register
    CDR        : longword;            // ADC common regular data register for dual
  end;

  TCAN_TXMAILBOX_Registers = record
    TIR        : longword;            // CAN TX mailbox identifier register
    TDTR       : longword;            // CAN mailbox data length control and time stamp register
    TDLR       : longword;            // CAN mailbox data low register
    TDHR       : longword;            // CAN mailbox data high register
  end;

  TCAN_FIFOMAILBOX_Registers = record
    RIR        : longword;            // CAN receive FIFO mailbox identifier register
    RDTR       : longword;            // CAN receive FIFO mailbox data length control and time stamp register
    RDLR       : longword;            // CAN receive FIFO mailbox data low register
    RDHR       : longword;            // CAN receive FIFO mailbox data high register
  end;

  TCAN_FILTERREGISTER_Registers = record
    FR1        : longword;            // CAN Filter bank register 1
    FR2        : longword;            // CAN Filter bank register 1
  end;

  TCAN_Registers = record
    MCR        : longword;            // CAN master control register
    MSR        : longword;            // CAN master status register
    TSR        : longword;            // CAN transmit status register
    RF0R       : longword;            // CAN receive FIFO 0 register
    RF1R       : longword;            // CAN receive FIFO 1 register
    IER        : longword;            // CAN interrupt enable register
    ESR        : longword;            // CAN error status register
    BTR        : longword;            // CAN bit timing register
    RESERVED0  : array[0..87] of longword; // Reserved, 0x020 - 0x17F
    sTxMailBox : array[0..2] of TCAN_TXMAILBOX_Registers; // CAN Tx MailBox
    sFIFOMailBox : array[0..1] of TCAN_FIFOMAILBOX_Registers; // CAN FIFO MailBox
    RESERVED1  : array[0..11] of longword; // Reserved, 0x1D0 - 0x1FF
    FMR        : longword;            // CAN filter master register
    FM1R       : longword;            // CAN filter mode register
    RESERVED2  : longword;            // Reserved, 0x208
    FS1R       : longword;            // CAN filter scale register
    RESERVED3  : longword;            // Reserved, 0x210
    FFA1R      : longword;            // CAN filter FIFO assignment register
    RESERVED4  : longword;            // Reserved, 0x218
    FA1R       : longword;            // CAN filter activation register
    RESERVED5  : array[0..7] of longword; // Reserved, 0x220-0x23F
    sFilterRegister : array[0..27] of TCAN_FILTERREGISTER_Registers; // CAN Filter Register
  end;

  TCRC_Registers = record
    DR         : longword;            // CRC Data register
    IDR        : byte;                // CRC Independent data register
    RESERVED0  : byte;                // Reserved, 0x05
    RESERVED1  : word;                // Reserved, 0x06
    CR         : longword;            // CRC Control register
  end;

  TDAC_Registers = record
    CR         : longword;            // DAC control register
    SWTRIGR    : longword;            // DAC software trigger register
    DHR12R1    : longword;            // DAC channel1 12-bit right-aligned data holding register
    DHR12L1    : longword;            // DAC channel1 12-bit left aligned data holding register
    DHR8R1     : longword;            // DAC channel1 8-bit right aligned data holding register
    DHR12R2    : longword;            // DAC channel2 12-bit right aligned data holding register
    DHR12L2    : longword;            // DAC channel2 12-bit left aligned data holding register
    DHR8R2     : longword;            // DAC channel2 8-bit right-aligned data holding register
    DHR12RD    : longword;            // Dual DAC 12-bit right-aligned data holding register
    DHR12LD    : longword;            // DUAL DAC 12-bit left aligned data holding register
    DHR8RD     : longword;            // DUAL DAC 8-bit right aligned data holding register
    DOR1       : longword;            // DAC channel1 data output register
    DOR2       : longword;            // DAC channel2 data output register
    SR         : longword;            // DAC status register
  end;

  TDBGMCU_Registers = record
    IDCODE     : longword;            // MCU device ID code
    CR         : longword;            // Debug MCU configuration register
    APB1FZ     : longword;            // Debug MCU APB1 freeze register
    APB2FZ     : longword;            // Debug MCU APB2 freeze register
  end;

  TDCMI_Registers = record
    CR         : longword;            // DCMI control register 1
    SR         : longword;            // DCMI status register
    RISR       : longword;            // DCMI raw interrupt status register
    IER        : longword;            // DCMI interrupt enable register
    MISR       : longword;            // DCMI masked interrupt status register
    ICR        : longword;            // DCMI interrupt clear register
    ESCR       : longword;            // DCMI embedded synchronization code register
    ESUR       : longword;            // DCMI embedded synchronization unmask register
    CWSTRTR    : longword;            // DCMI crop window start
    CWSIZER    : longword;            // DCMI crop window size
    DR         : longword;            // DCMI data register
  end;

  TDMA_STREAM_Registers = record
    CR         : longword;            // DMA stream x configuration register
    NDTR       : longword;            // DMA stream x number of data register
    PAR        : longword;            // DMA stream x peripheral address register
    M0AR       : longword;            // DMA stream x memory 0 address register
    M1AR       : longword;            // DMA stream x memory 1 address register
    FCR        : longword;            // DMA stream x FIFO control register
  end;

  TDMA_Registers = record
    LISR       : longword;            // DMA low interrupt status register
    HISR       : longword;            // DMA high interrupt status register
    LIFCR      : longword;            // DMA low interrupt flag clear register
    HIFCR      : longword;            // DMA high interrupt flag clear register
  end;

  TETH_Registers = record
    MACCR      : longword;
    MACFFR     : longword;
    MACHTHR    : longword;
    MACHTLR    : longword;
    MACMIIAR   : longword;
    MACMIIDR   : longword;
    MACFCR     : longword;
    MACVLANTR  : longword;            // 8
    RESERVED0  : array[0..1] of longword;
    MACRWUFFR  : longword;            // 11
    MACPMTCSR  : longword;
    RESERVED1  : array[0..1] of longword;
    MACSR      : longword;            // 15
    MACIMR     : longword;
    MACA0HR    : longword;
    MACA0LR    : longword;
    MACA1HR    : longword;
    MACA1LR    : longword;
    MACA2HR    : longword;
    MACA2LR    : longword;
    MACA3HR    : longword;
    MACA3LR    : longword;            // 24
    RESERVED2  : array[0..39] of longword;
    MMCCR      : longword;            // 65
    MMCRIR     : longword;
    MMCTIR     : longword;
    MMCRIMR    : longword;
    MMCTIMR    : longword;            // 69
    RESERVED3  : array[0..13] of longword;
    MMCTGFSCCR : longword;            // 84
    MMCTGFMSCCR : longword;
    RESERVED4  : array[0..4] of longword;
    MMCTGFCR   : longword;
    RESERVED5  : array[0..9] of longword;
    MMCRFCECR  : longword;
    MMCRFAECR  : longword;
    RESERVED6  : array[0..9] of longword;
    MMCRGUFCR  : longword;
    RESERVED7  : array[0..333] of longword;
    PTPTSCR    : longword;
    PTPSSIR    : longword;
    PTPTSHR    : longword;
    PTPTSLR    : longword;
    PTPTSHUR   : longword;
    PTPTSLUR   : longword;
    PTPTSAR    : longword;
    PTPTTHR    : longword;
    PTPTTLR    : longword;
    RESERVED8  : longword;
    PTPTSSR    : longword;
    RESERVED9  : array[0..564] of longword;
    DMABMR     : longword;
    DMATPDR    : longword;
    DMARPDR    : longword;
    DMARDLAR   : longword;
    DMATDLAR   : longword;
    DMASR      : longword;
    DMAOMR     : longword;
    DMAIER     : longword;
    DMAMFBOCR  : longword;
    DMARSWTR   : longword;
    RESERVED10 : array[0..7] of longword;
    DMACHTDR   : longword;
    DMACHRDR   : longword;
    DMACHTBAR  : longword;
    DMACHRBAR  : longword;
  end;

  TEXTI_Registers = record
    IMR        : longword;            // EXTI Interrupt mask register
    EMR        : longword;            // EXTI Event mask register
    RTSR       : longword;            // EXTI Rising trigger selection register
    FTSR       : longword;            // EXTI Falling trigger selection register
    SWIER      : longword;            // EXTI Software interrupt event register
    PR         : longword;            // EXTI Pending register
  end;

  TFLASH_Registers = record
    ACR        : longword;            // FLASH access control register
    KEYR       : longword;            // FLASH key register
    OPTKEYR    : longword;            // FLASH option key register
    SR         : longword;            // FLASH status register
    CR         : longword;            // FLASH control register
    OPTCR      : longword;            // FLASH option control register
    OPTCR1     : longword;            // FLASH option control register 1
  end;

  TFSMC_BANK1_Registers = record
    BTCR       : array[0..7] of longword; // NOR/PSRAM chip-select control register(BCR) and chip-select timing register(BTR)
  end;

  TFSMC_BANK1E_Registers = record
    BWTR       : array[0..6] of longword; // NOR/PSRAM write timing registers
  end;

  TFSMC_BANK2_3_Registers = record
    PCR2       : longword;            // NAND Flash control register 2
    SR2        : longword;            // NAND Flash FIFO status and interrupt register 2
    PMEM2      : longword;            // NAND Flash Common memory space timing register 2
    PATT2      : longword;            // NAND Flash Attribute memory space timing register 2
    RESERVED0  : longword;            // Reserved, 0x70
    ECCR2      : longword;            // NAND Flash ECC result registers 2
    RESERVED1  : longword;            // Reserved, 0x78
    RESERVED2  : longword;            // Reserved, 0x7C
    PCR3       : longword;            // NAND Flash control register 3
    SR3        : longword;            // NAND Flash FIFO status and interrupt register 3
    PMEM3      : longword;            // NAND Flash Common memory space timing register 3
    PATT3      : longword;            // NAND Flash Attribute memory space timing register 3
    RESERVED3  : longword;            // Reserved, 0x90
    ECCR3      : longword;            // NAND Flash ECC result registers 3
  end;

  TFSMC_BANK4_Registers = record
    PCR4       : longword;            // PC Card  control register 4
    SR4        : longword;            // PC Card  FIFO status and interrupt register 4
    PMEM4      : longword;            // PC Card  Common memory space timing register 4
    PATT4      : longword;            // PC Card  Attribute memory space timing register 4
    PIO4       : longword;            // PC Card  I/O space timing register 4
  end;

  TGPIO_Registers = record
    MODER      : longword;            // GPIO port mode register
    OTYPER     : longword;            // GPIO port output type register
    OSPEEDR    : longword;            // GPIO port output speed register
    PUPDR      : longword;            // GPIO port pull-up/pull-down register
    IDR        : longword;            // GPIO port input data register
    ODR        : longword;            // GPIO port output data register
    BSRR       : longword;            // GPIO port bit set/reset register
    LCKR       : longword;            // GPIO port configuration lock register
    AFR        : array[0..1] of longword; // GPIO alternate function registers
  end;

  TSYSCFG_Registers = record
    MEMRMP     : longword;            // SYSCFG memory remap register
    PMC        : longword;            // SYSCFG peripheral mode configuration register
    EXTICR     : array[0..3] of longword; // SYSCFG external interrupt configuration registers
    RESERVED   : array[0..1] of longword; // Reserved, 0x18-0x1C
    CMPCR      : longword;            // SYSCFG Compensation cell control register
  end;

  TI2C_Registers = record
    CR1        : longword;            // I2C Control register 1
    CR2        : longword;            // I2C Control register 2
    OAR1       : longword;            // I2C Own address register 1
    OAR2       : longword;            // I2C Own address register 2
    DR         : longword;            // I2C Data register
    SR1        : longword;            // I2C Status register 1
    SR2        : longword;            // I2C Status register 2
    CCR        : longword;            // I2C Clock control register
    TRISE      : longword;            // I2C TRISE register
    FLTR       : longword;            // I2C FLTR register
  end;

  TIWDG_Registers = record
    KR         : longword;            // IWDG Key register
    PR         : longword;            // IWDG Prescaler register
    RLR        : longword;            // IWDG Reload register
    SR         : longword;            // IWDG Status register
  end;

  TPWR_Registers = record
    CR         : longword;            // PWR power control register
    CSR        : longword;            // PWR power control/status register
  end;

  TRCC_Registers = record
    CR         : longword;            // RCC clock control register
    PLLCFGR    : longword;            // RCC PLL configuration register
    CFGR       : longword;            // RCC clock configuration register
    CIR        : longword;            // RCC clock interrupt register
    AHB1RSTR   : longword;            // RCC AHB1 peripheral reset register
    AHB2RSTR   : longword;            // RCC AHB2 peripheral reset register
    AHB3RSTR   : longword;            // RCC AHB3 peripheral reset register
    RESERVED0  : longword;            // Reserved, 0x1C
    APB1RSTR   : longword;            // RCC APB1 peripheral reset register
    APB2RSTR   : longword;            // RCC APB2 peripheral reset register
    RESERVED1  : array[0..1] of longword; // Reserved, 0x28-0x2C
    AHB1ENR    : longword;            // RCC AHB1 peripheral clock register
    AHB2ENR    : longword;            // RCC AHB2 peripheral clock register
    AHB3ENR    : longword;            // RCC AHB3 peripheral clock register
    RESERVED2  : longword;            // Reserved, 0x3C
    APB1ENR    : longword;            // RCC APB1 peripheral clock enable register
    APB2ENR    : longword;            // RCC APB2 peripheral clock enable register
    RESERVED3  : array[0..1] of longword; // Reserved, 0x48-0x4C
    AHB1LPENR  : longword;            // RCC AHB1 peripheral clock enable in low power mode register
    AHB2LPENR  : longword;            // RCC AHB2 peripheral clock enable in low power mode register
    AHB3LPENR  : longword;            // RCC AHB3 peripheral clock enable in low power mode register
    RESERVED4  : longword;            // Reserved, 0x5C
    APB1LPENR  : longword;            // RCC APB1 peripheral clock enable in low power mode register
    APB2LPENR  : longword;            // RCC APB2 peripheral clock enable in low power mode register
    RESERVED5  : array[0..1] of longword; // Reserved, 0x68-0x6C
    BDCR       : longword;            // RCC Backup domain control register
    CSR        : longword;            // RCC clock control & status register
    RESERVED6  : array[0..1] of longword; // Reserved, 0x78-0x7C
    SSCGR      : longword;            // RCC spread spectrum clock generation register
    PLLI2SCFGR : longword;            // RCC PLLI2S configuration register
    RESERVED7  : longword;            // Reserved, 0x88
    DCKCFGR    : longWord;            // RCC Dedicated Clocks Configuration Register
  end;

  TRTC_Registers = record
    TR         : longword;            // RTC time register
    DR         : longword;            // RTC date register
    CR         : longword;            // RTC control register
    ISR        : longword;            // RTC initialization and status register
    PRER       : longword;            // RTC prescaler register
    WUTR       : longword;            // RTC wakeup timer register
    CALIBR     : longword;            // RTC calibration register
    ALRMAR     : longword;            // RTC alarm A register
    ALRMBR     : longword;            // RTC alarm B register
    WPR        : longword;            // RTC write protection register
    SSR        : longword;            // RTC sub second register
    SHIFTR     : longword;            // RTC shift control register
    TSTR       : longword;            // RTC time stamp time register
    TSDR       : longword;            // RTC time stamp date register
    TSSSR      : longword;            // RTC time-stamp sub second register
    CALR       : longword;            // RTC calibration register
    TAFCR      : longword;            // RTC tamper and alternate function configuration register
    ALRMASSR   : longword;            // RTC alarm A sub second register
    ALRMBSSR   : longword;            // RTC alarm B sub second register
    RESERVED7  : longword;            // Reserved, 0x4C
    BKP0R      : longword;            // RTC backup register 1
    BKP1R      : longword;            // RTC backup register 1
    BKP2R      : longword;            // RTC backup register 2
    BKP3R      : longword;            // RTC backup register 3
    BKP4R      : longword;            // RTC backup register 4
    BKP5R      : longword;            // RTC backup register 5
    BKP6R      : longword;            // RTC backup register 6
    BKP7R      : longword;            // RTC backup register 7
    BKP8R      : longword;            // RTC backup register 8
    BKP9R      : longword;            // RTC backup register 9
    BKP10R     : longword;            // RTC backup register 10
    BKP11R     : longword;            // RTC backup register 11
    BKP12R     : longword;            // RTC backup register 12
    BKP13R     : longword;            // RTC backup register 13
    BKP14R     : longword;            // RTC backup register 14
    BKP15R     : longword;            // RTC backup register 15
    BKP16R     : longword;            // RTC backup register 16
    BKP17R     : longword;            // RTC backup register 17
    BKP18R     : longword;            // RTC backup register 18
    BKP19R     : longword;            // RTC backup register 19
  end;

  TSDIO_Registers = record
    POWER      : longword;            // SDIO power control register
    CLKCR      : longword;            // SDI clock control register
    ARG        : longword;            // SDIO argument register
    CMD        : longword;            // SDIO command register
    RESPCMD    : longword;            // SDIO command response register
    RESP1      : longword;            // SDIO response 1 register
    RESP2      : longword;            // SDIO response 2 register
    RESP3      : longword;            // SDIO response 3 register
    RESP4      : longword;            // SDIO response 4 register
    DTIMER     : longword;            // SDIO data timer register
    DLEN       : longword;            // SDIO data length register
    DCTRL      : longword;            // SDIO data control register
    DCOUNT     : longword;            // SDIO data counter register
    STA        : longword;            // SDIO status register
    ICR        : longword;            // SDIO interrupt clear register
    MASK       : longword;            // SDIO mask register
    RESERVED0  : array[0..1] of longword; // Reserved, 0x40-0x44
    FIFOCNT    : longword;            // SDIO FIFO counter register
    RESERVED1  : array[0..12] of longword; // Reserved, 0x4C-0x7C
    FIFO       : longword;            // SDIO data FIFO register
  end;

  TSPI_Registers = record
    CR1        : longword;            // SPI control register 1 (not used in I2S mode)
    CR2        : longword;            // SPI control register 2
    SR         : longword;            // SPI status register
    DR         : longword;            // SPI data register
    CRCPR      : longword;            // SPI CRC polynomial register (not used in I2S mode)
    RXCRCR     : longword;            // SPI RX CRC register (not used in I2S mode)
    TXCRCR     : longword;            // SPI TX CRC register (not used in I2S mode)
    I2SCFGR    : longword;            // SPI_I2S configuration register
    I2SPR      : longword;            // SPI_I2S prescaler register
  end;

  TTIM_Registers = record
    CR1        : longword;            // TIM control register 1
    CR2        : longword;            // TIM control register 2
    SMCR       : longword;            // TIM slave mode control register
    DIER       : longword;            // TIM DMA/interrupt enable register
    SR         : longword;            // TIM status register
    EGR        : longword;            // TIM event generation register
    CCMR1      : longword;            // TIM capture/compare mode register 1
    CCMR2      : longword;            // TIM capture/compare mode register 2
    CCER       : longword;            // TIM capture/compare enable register
    CNT        : longword;            // TIM counter register
    PSC        : longword;            // TIM prescaler
    ARR        : longword;            // TIM auto-reload register
    RCR        : longword;            // TIM repetition counter register
    CCR1       : longword;            // TIM capture/compare register 1
    CCR2       : longword;            // TIM capture/compare register 2
    CCR3       : longword;            // TIM capture/compare register 3
    CCR4       : longword;            // TIM capture/compare register 4
    BDTR       : longword;            // TIM break and dead-time register
    DCR        : longword;            // TIM DMA control register
    DMAR       : longword;            // TIM DMA address for full transfer
    &OR        : longword;            // TIM option register
  end;

  TUSART_Registers = record
    SR         : longword;            // USART Status register
    DR         : longword;            // USART Data register
    BRR        : longword;            // USART Baud rate register
    CR1        : longword;            // USART Control register 1
    CR2        : longword;            // USART Control register 2
    CR3        : longword;            // USART Control register 3
    GTPR       : longword;            // USART Guard time and prescaler register
  end;

  TWWDG_Registers = record
    CR         : longword;            // WWDG Control register
    CFR        : longword;            // WWDG Configuration register
    SR         : longword;            // WWDG Status register
  end;

  TRNG_Registers = record
    CR         : longword;            // RNG control register
    SR         : longword;            // RNG status register
    DR         : longword;            // RNG data register
  end;

  TUSB_OTG_GLOBAL_Registers = record
    GOTGCTL    : longword;            //  USB_OTG Control and Status Register    000h
    GOTGINT    : longword;            //  USB_OTG Interrupt Register             004h
    GAHBCFG    : longword;            //  Core AHB Configuration Register    008h
    GUSBCFG    : longword;            //  Core USB Configuration Register    00Ch
    GRSTCTL    : longword;            //  Core Reset Register                010h
    GINTSTS    : longword;            //  Core Interrupt Register            014h
    GINTMSK    : longword;            //  Core Interrupt Mask Register       018h
    GRXSTSR    : longword;            //  Receive Sts Q Read Register        01Ch
    GRXSTSP    : longword;            //  Receive Sts Q Read & POP Register  020h
    GRXFSIZ    : longword;            // Receive FIFO Size Register         024h
    DIEPTXF0_HNPTXFSIZ : longword;    //  EP0 / Non Periodic Tx FIFO Size Register 028h
    HNPTXSTS   : longword;            //  Non Periodic Tx FIFO/Queue Sts reg 02Ch
    RESERVED30 : array[0..1] of longword; // Reserved                           030h
    GCCFG      : longword;            // General Purpose IO Register        038h
    CID        : longword;            // User ID Register                   03Ch
    RESERVED40 : array[0..47] of longword; // Reserved                      040h-0FFh
    HPTXFSIZ   : longword;            // Host Periodic Tx FIFO Size Reg     100h
    DIEPTXF    : array[0..14] of longword; // dev Periodic Transmit FIFO
  end;

  TUSB_OTG_DEVICE_Registers = record
    DCFG       : longword;            // dev Configuration Register   800h
    DCTL       : longword;            // dev Control Register         804h
    DSTS       : longword;            // dev Status Register (RO)     808h
    RESERVED0C : longword;            // Reserved                     80Ch
    DIEPMSK    : longword;            // dev IN Endpoint Mask         810h
    DOEPMSK    : longword;            // dev OUT Endpoint Mask        814h
    DAINT      : longword;            // dev All Endpoints Itr Reg    818h
    DAINTMSK   : longword;            // dev All Endpoints Itr Mask   81Ch
    RESERVED20 : longword;            // Reserved                     820h
    RESERVED9  : longword;            // Reserved                     824h
    DVBUSDIS   : longword;            // dev VBUS discharge Register  828h
    DVBUSPULSE : longword;            // dev VBUS Pulse Register      82Ch
    DTHRCTL    : longword;            // dev thr                      830h
    DIEPEMPMSK : longword;            // dev empty msk             834h
    DEACHINT   : longword;            // dedicated EP interrupt       838h
    DEACHMSK   : longword;            // dedicated EP msk             83Ch
    RESERVED40 : longword;            // dedicated EP mask           840h
    DINEP1MSK  : longword;            // dedicated EP mask           844h
    RESERVED44 : array[0..14] of longword; // Reserved                 844-87Ch
    DOUTEP1MSK : longword;            // dedicated EP msk            884h
  end;

  TUSB_OTG_INENDPOINT_Registers = record
    DIEPCTL    : longword;            // dev IN Endpoint Control Reg 900h + (ep_num * 20h) + 00h
    RESERVED04 : longword;            // Reserved                       900h + (ep_num * 20h) + 04h
    DIEPINT    : longword;            // dev IN Endpoint Itr Reg     900h + (ep_num * 20h) + 08h
    RESERVED0C : longword;            // Reserved                       900h + (ep_num * 20h) + 0Ch
    DIEPTSIZ   : longword;            // IN Endpoint Txfer Size   900h + (ep_num * 20h) + 10h
    DIEPDMA    : longword;            // IN Endpoint DMA Address Reg    900h + (ep_num * 20h) + 14h
    DTXFSTS    : longword;            // IN Endpoint Tx FIFO Status Reg 900h + (ep_num * 20h) + 18h
    RESERVED18 : longword;            // Reserved  900h+(ep_num*20h)+1Ch-900h+ (ep_num * 20h) + 1Ch
  end;

  TUSB_OTG_OUTENDPOINT_Registers = record
    DOEPCTL    : longword;            // dev OUT Endpoint Control Reg  B00h + (ep_num * 20h) + 00h
    RESERVED04 : longword;            // Reserved                      B00h + (ep_num * 20h) + 04h
    DOEPINT    : longword;            // dev OUT Endpoint Itr Reg      B00h + (ep_num * 20h) + 08h
    RESERVED0C : longword;            // Reserved                      B00h + (ep_num * 20h) + 0Ch
    DOEPTSIZ   : longword;            // dev OUT Endpoint Txfer Size   B00h + (ep_num * 20h) + 10h
    DOEPDMA    : longword;            // dev OUT Endpoint DMA Address  B00h + (ep_num * 20h) + 14h
    RESERVED18 : array[0..1] of longword; // Reserved B00h + (ep_num * 20h) + 18h - B00h + (ep_num * 20h) + 1Ch
  end;

  TUSB_OTG_HOST_Registers = record
    HCFG       : longword;            // Host Configuration Register    400h
    HFIR       : longword;            // Host Frame Interval Register   404h
    HFNUM      : longword;            // Host Frame Nbr/Frame Remaining 408h
    RESERVED40C : longword;           // Reserved                       40Ch
    HPTXSTS    : longword;            // Host Periodic Tx FIFO/ Queue Status 410h
    HAINT      : longword;            // Host All Channels Interrupt Register 414h
    HAINTMSK   : longword;            // Host All Channels Interrupt Mask 418h
  end;

  TUSB_OTG_HOSTCHANNEL_Registers = record
    HCCHAR     : longword;
    HCSPLT     : longword;
    HCINT      : longword;
    HCINTMSK   : longword;
    HCTSIZ     : longword;
    HCDMA      : longword;
    RESERVED   : array[0..1] of longword;
  end;

const
  FLASH_BASE   = $08000000;           // FLASH(up to 1 MB) base address in the alias region
  CCMDATARAM_BASE = $10000000;        // CCM(core coupled memory) data RAM(64 KB) base address in the alias region
  SRAM1_BASE   = $20000000;           // SRAM1(112 KB) base address in the alias region
  SRAM2_BASE   = $2001C000;           // SRAM2(16 KB) base address in the alias region
  PERIPH_BASE  = $40000000;           // Peripheral base address in the alias region
  BKPSRAM_BASE = $40024000;           // Backup SRAM(4 KB) base address in the alias region
  FSMC_R_BASE  = $A0000000;           // FSMC registers base address
  SRAM1_BB_BASE = $22000000;          // SRAM1(112 KB) base address in the bit-band region
  SRAM2_BB_BASE = $22380000;          // SRAM2(16 KB) base address in the bit-band region
  PERIPH_BB_BASE = $42000000;         // Peripheral base address in the bit-band region
  BKPSRAM_BB_BASE = $42480000;        // Backup SRAM(4 KB) base address in the bit-band region
  SRAM_BASE    = $20000000;
  SRAM_BB_BASE = $22000000;
  APB1PERIPH_BASE = $40000000;
  APB2PERIPH_BASE = PERIPH_BASE + $00010000;
  AHB1PERIPH_BASE = PERIPH_BASE + $00020000;
  AHB2PERIPH_BASE = PERIPH_BASE + $10000000;
  TIM2_BASE    = APB1PERIPH_BASE + $0000;
  TIM3_BASE    = APB1PERIPH_BASE + $0400;
  TIM4_BASE    = APB1PERIPH_BASE + $0800;
  TIM5_BASE    = APB1PERIPH_BASE + $0C00;
  TIM6_BASE    = APB1PERIPH_BASE + $1000;
  TIM7_BASE    = APB1PERIPH_BASE + $1400;
  TIM12_BASE   = APB1PERIPH_BASE + $1800;
  TIM13_BASE   = APB1PERIPH_BASE + $1C00;
  TIM14_BASE   = APB1PERIPH_BASE + $2000;
  RTC_BASE     = APB1PERIPH_BASE + $2800;
  WWDG_BASE    = APB1PERIPH_BASE + $2C00;
  IWDG_BASE    = APB1PERIPH_BASE + $3000;
  I2S2ext_BASE = APB1PERIPH_BASE + $3400;
  SPI2_BASE    = APB1PERIPH_BASE + $3800;
  SPI3_BASE    = APB1PERIPH_BASE + $3C00;
  I2S3ext_BASE = APB1PERIPH_BASE + $4000;
  USART2_BASE  = APB1PERIPH_BASE + $4400;
  USART3_BASE  = APB1PERIPH_BASE + $4800;
  UART4_BASE   = APB1PERIPH_BASE + $4C00;
  UART5_BASE   = APB1PERIPH_BASE + $5000;
  I2C1_BASE    = APB1PERIPH_BASE + $5400;
  I2C2_BASE    = APB1PERIPH_BASE + $5800;
  I2C3_BASE    = APB1PERIPH_BASE + $5C00;
  CAN1_BASE    = APB1PERIPH_BASE + $6400;
  CAN2_BASE    = APB1PERIPH_BASE + $6800;
  PWR_BASE     = APB1PERIPH_BASE + $7000;
  DAC_BASE     = APB1PERIPH_BASE + $7400;
  TIM1_BASE    = APB2PERIPH_BASE + $0000;
  TIM8_BASE    = APB2PERIPH_BASE + $0400;
  USART1_BASE  = APB2PERIPH_BASE + $1000;
  USART6_BASE  = APB2PERIPH_BASE + $1400;
  ADC1_BASE    = APB2PERIPH_BASE + $2000;
  ADC2_BASE    = APB2PERIPH_BASE + $2100;
  ADC3_BASE    = APB2PERIPH_BASE + $2200;
  ADC_BASE     = APB2PERIPH_BASE + $2300;
  SDIO_BASE    = APB2PERIPH_BASE + $2C00;
  SPI1_BASE    = APB2PERIPH_BASE + $3000;
  SYSCFG_BASE  = APB2PERIPH_BASE + $3800;
  EXTI_BASE    = APB2PERIPH_BASE + $3C00;
  TIM9_BASE    = APB2PERIPH_BASE + $4000;
  TIM10_BASE   = APB2PERIPH_BASE + $4400;
  TIM11_BASE   = APB2PERIPH_BASE + $4800;
  GPIOA_BASE   = AHB1PERIPH_BASE + $0000;
  GPIOB_BASE   = AHB1PERIPH_BASE + $0400;
  GPIOC_BASE   = AHB1PERIPH_BASE + $0800;
  GPIOD_BASE   = AHB1PERIPH_BASE + $0C00;
  GPIOE_BASE   = AHB1PERIPH_BASE + $1000;
  GPIOF_BASE   = AHB1PERIPH_BASE + $1400;
  GPIOG_BASE   = AHB1PERIPH_BASE + $1800;
  GPIOH_BASE   = AHB1PERIPH_BASE + $1C00;
  GPIOI_BASE   = AHB1PERIPH_BASE + $2000;
  CRC_BASE     = AHB1PERIPH_BASE + $3000;
  RCC_BASE     = AHB1PERIPH_BASE + $3800;
  FLASH_R_BASE = AHB1PERIPH_BASE + $3C00;
  DMA1_BASE    = AHB1PERIPH_BASE + $6000;
  DMA1_Stream0_BASE = DMA1_BASE + $010;
  DMA1_Stream1_BASE = DMA1_BASE + $028;
  DMA1_Stream2_BASE = DMA1_BASE + $040;
  DMA1_Stream3_BASE = DMA1_BASE + $058;
  DMA1_Stream4_BASE = DMA1_BASE + $070;
  DMA1_Stream5_BASE = DMA1_BASE + $088;
  DMA1_Stream6_BASE = DMA1_BASE + $0A0;
  DMA1_Stream7_BASE = DMA1_BASE + $0B8;
  DMA2_BASE    = AHB1PERIPH_BASE + $6400;
  DMA2_Stream0_BASE = DMA2_BASE + $010;
  DMA2_Stream1_BASE = DMA2_BASE + $028;
  DMA2_Stream2_BASE = DMA2_BASE + $040;
  DMA2_Stream3_BASE = DMA2_BASE + $058;
  DMA2_Stream4_BASE = DMA2_BASE + $070;
  DMA2_Stream5_BASE = DMA2_BASE + $088;
  DMA2_Stream6_BASE = DMA2_BASE + $0A0;
  DMA2_Stream7_BASE = DMA2_BASE + $0B8;
  ETH_BASE     = AHB1PERIPH_BASE + $8000;
  ETH_MAC_BASE = AHB1PERIPH_BASE + $8000;
  ETH_MMC_BASE = ETH_BASE + $0100;
  ETH_PTP_BASE = ETH_BASE + $0700;
  ETH_DMA_BASE = ETH_BASE + $1000;
  DCMI_BASE    = AHB2PERIPH_BASE + $50000;
  RNG_BASE     = AHB2PERIPH_BASE + $60800;
  FSMC_Bank1_R_BASE = FSMC_R_BASE + $0000;
  FSMC_Bank1E_R_BASE = FSMC_R_BASE + $0104;
  FSMC_Bank2_3_R_BASE = FSMC_R_BASE + $0060;
  FSMC_Bank4_R_BASE = FSMC_R_BASE + $00A0;
  DBGMCU_BASE  = $E0042000;
  USB_OTG_HS_PERIPH_BASE = $40040000;
  USB_OTG_FS_PERIPH_BASE = $50000000;
  USB_OTG_GLOBAL_BASE = $000;
  USB_OTG_DEVICE_BASE = $800;
  USB_OTG_IN_ENDPOINT_BASE = $900;
  USB_OTG_OUT_ENDPOINT_BASE = $B00;
  USB_OTG_HOST_BASE = $400;
  USB_OTG_HOST_PORT_BASE = $440;
  USB_OTG_HOST_CHANNEL_BASE = $500;
  USB_OTG_PCGCCTL_BASE = $E00;
  USB_OTG_FIFO_BASE = $1000;

var
  TIM2         : TTIM_Registers absolute TIM2_BASE;
  TIM3         : TTIM_Registers absolute TIM3_BASE;
  TIM4         : TTIM_Registers absolute TIM4_BASE;
  TIM5         : TTIM_Registers absolute TIM5_BASE;
  TIM6         : TTIM_Registers absolute TIM6_BASE;
  TIM7         : TTIM_Registers absolute TIM7_BASE;
  TIM12        : TTIM_Registers absolute TIM12_BASE;
  TIM13        : TTIM_Registers absolute TIM13_BASE;
  TIM14        : TTIM_Registers absolute TIM14_BASE;
  RTC          : TRTC_Registers absolute RTC_BASE;
  WWDG         : TWWDG_Registers absolute WWDG_BASE;
  IWDG         : TIWDG_Registers absolute IWDG_BASE;
  I2S2ext      : TSPI_Registers absolute I2S2ext_BASE;
  SPI2         : TSPI_Registers absolute SPI2_BASE;
  SPI3         : TSPI_Registers absolute SPI3_BASE;
  I2S3ext      : TSPI_Registers absolute I2S3ext_BASE;
  USART2       : TUSART_Registers absolute USART2_BASE;
  USART3       : TUSART_Registers absolute USART3_BASE;
  UART4        : TUSART_Registers absolute UART4_BASE;
  UART5        : TUSART_Registers absolute UART5_BASE;
  I2C1         : TI2C_Registers absolute I2C1_BASE;
  I2C2         : TI2C_Registers absolute I2C2_BASE;
  I2C3         : TI2C_Registers absolute I2C3_BASE;
  CAN1         : TCAN_Registers absolute CAN1_BASE;
  CAN2         : TCAN_Registers absolute CAN2_BASE;
  PWR          : TPWR_Registers absolute PWR_BASE;
  DAC          : TDAC_Registers absolute DAC_BASE;
  TIM1         : TTIM_Registers absolute TIM1_BASE;
  TIM8         : TTIM_Registers absolute TIM8_BASE;
  USART1       : TUSART_Registers absolute USART1_BASE;
  USART6       : TUSART_Registers absolute USART6_BASE;
  ADC          : TADC_Common_Registers absolute ADC_BASE;
  ADC1         : TADC_Registers absolute ADC1_BASE;
  ADC2         : TADC_Registers absolute ADC2_BASE;
  ADC3         : TADC_Registers absolute ADC3_BASE;
  SDIO         : TSDIO_Registers absolute SDIO_BASE;
  SPI1         : TSPI_Registers absolute SPI1_BASE;
  SYSCFG       : TSYSCFG_Registers absolute SYSCFG_BASE;
  EXTI         : TEXTI_Registers absolute EXTI_BASE;
  TIM9         : TTIM_Registers absolute TIM9_BASE;
  TIM10        : TTIM_Registers absolute TIM10_BASE;
  TIM11        : TTIM_Registers absolute TIM11_BASE;
  GPIOA        : TGPIO_Registers absolute GPIOA_BASE;
  GPIOB        : TGPIO_Registers absolute GPIOB_BASE;
  GPIOC        : TGPIO_Registers absolute GPIOC_BASE;
  GPIOD        : TGPIO_Registers absolute GPIOD_BASE;
  GPIOE        : TGPIO_Registers absolute GPIOE_BASE;
  GPIOF        : TGPIO_Registers absolute GPIOF_BASE;
  GPIOG        : TGPIO_Registers absolute GPIOG_BASE;
  GPIOH        : TGPIO_Registers absolute GPIOH_BASE;
  GPIOI        : TGPIO_Registers absolute GPIOI_BASE;
  CRC          : TCRC_Registers absolute CRC_BASE;
  RCC          : TRCC_Registers absolute RCC_BASE;
  FLASH        : TFLASH_Registers absolute FLASH_R_BASE;
  DMA1         : TDMA_Registers absolute DMA1_BASE;
  DMA1_Stream0 : TDMA_Stream_Registers absolute DMA1_Stream0_BASE;
  DMA1_Stream1 : TDMA_Stream_Registers absolute DMA1_Stream1_BASE;
  DMA1_Stream2 : TDMA_Stream_Registers absolute DMA1_Stream2_BASE;
  DMA1_Stream3 : TDMA_Stream_Registers absolute DMA1_Stream3_BASE;
  DMA1_Stream4 : TDMA_Stream_Registers absolute DMA1_Stream4_BASE;
  DMA1_Stream5 : TDMA_Stream_Registers absolute DMA1_Stream5_BASE;
  DMA1_Stream6 : TDMA_Stream_Registers absolute DMA1_Stream6_BASE;
  DMA1_Stream7 : TDMA_Stream_Registers absolute DMA1_Stream7_BASE;
  DMA2         : TDMA_Registers absolute DMA2_BASE;
  DMA2_Stream0 : TDMA_Stream_Registers absolute DMA2_Stream0_BASE;
  DMA2_Stream1 : TDMA_Stream_Registers absolute DMA2_Stream1_BASE;
  DMA2_Stream2 : TDMA_Stream_Registers absolute DMA2_Stream2_BASE;
  DMA2_Stream3 : TDMA_Stream_Registers absolute DMA2_Stream3_BASE;
  DMA2_Stream4 : TDMA_Stream_Registers absolute DMA2_Stream4_BASE;
  DMA2_Stream5 : TDMA_Stream_Registers absolute DMA2_Stream5_BASE;
  DMA2_Stream6 : TDMA_Stream_Registers absolute DMA2_Stream6_BASE;
  DMA2_Stream7 : TDMA_Stream_Registers absolute DMA2_Stream7_BASE;
  ETH          : TETH_Registers absolute ETH_BASE;
  DCMI         : TDCMI_Registers absolute DCMI_BASE;
  RNG          : TRNG_Registers absolute RNG_BASE;
  FSMC_Bank1   : TFSMC_Bank1_Registers absolute FSMC_Bank1_R_BASE;
  FSMC_Bank1E  : TFSMC_Bank1E_Registers absolute FSMC_Bank1E_R_BASE;
  FSMC_Bank2_3 : TFSMC_Bank2_3_Registers absolute FSMC_Bank2_3_R_BASE;
  FSMC_Bank4   : TFSMC_Bank4_Registers absolute FSMC_Bank4_R_BASE;
  DBGMCU       : TDBGMCU_Registers absolute DBGMCU_BASE;

implementation

procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure MemoryManagement_interrupt; external name 'MemoryManagement_interrupt';
procedure BusFault_interrupt; external name 'BusFault_interrupt';
procedure UsageFault_interrupt; external name 'UsageFault_interrupt';
procedure SVCall_interrupt; external name 'SVCall_interrupt';
procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
procedure PendSV_interrupt; external name 'PendSV_interrupt';
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
procedure DMA1_Stream0_interrupt; external name 'DMA1_Stream0_interrupt';
procedure DMA1_Stream1_interrupt; external name 'DMA1_Stream1_interrupt';
procedure DMA1_Stream2_interrupt; external name 'DMA1_Stream2_interrupt';
procedure DMA1_Stream3_interrupt; external name 'DMA1_Stream3_interrupt';
procedure DMA1_Stream4_interrupt; external name 'DMA1_Stream4_interrupt';
procedure DMA1_Stream5_interrupt; external name 'DMA1_Stream5_interrupt';
procedure DMA1_Stream6_interrupt; external name 'DMA1_Stream6_interrupt';
procedure ADC_interrupt; external name 'ADC_interrupt';
procedure CAN1_TX_interrupt; external name 'CAN1_TX_interrupt';
procedure CAN1_RX0_interrupt; external name 'CAN1_RX0_interrupt';
procedure CAN1_RX1_interrupt; external name 'CAN1_RX1_interrupt';
procedure CAN1_SCE_interrupt; external name 'CAN1_SCE_interrupt';
procedure EXTI9_5_interrupt; external name 'EXTI9_5_interrupt';
procedure TIM1_BRK_TIM9_interrupt; external name 'TIM1_BRK_TIM9_interrupt';
procedure TIM1_UP_TIM10_interrupt; external name 'TIM1_UP_TIM10_interrupt';
procedure TIM1_TRG_COM_TIM11_interrupt; external name 'TIM1_TRG_COM_TIM11_interrupt';
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
procedure RTC_Alarm_interrupt; external name 'RTC_Alarm_interrupt';
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
procedure TIM6_DAC_interrupt; external name 'TIM6_DAC_interrupt';
procedure TIM7_interrupt; external name 'TIM7_interrupt';
procedure DMA2_Stream0_interrupt; external name 'DMA2_Stream0_interrupt';
procedure DMA2_Stream1_interrupt; external name 'DMA2_Stream1_interrupt';
procedure DMA2_Stream2_interrupt; external name 'DMA2_Stream2_interrupt';
procedure DMA2_Stream3_interrupt; external name 'DMA2_Stream3_interrupt';
procedure DMA2_Stream4_interrupt; external name 'DMA2_Stream4_interrupt';
procedure ETH_interrupt; external name 'ETH_interrupt';
procedure ETH_WKUP_interrupt; external name 'ETH_WKUP_interrupt';
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
procedure HASH_RNG_interrupt; external name 'HASH_RNG_interrupt';
procedure FPU_interrupt; external name 'FPU_interrupt';

{$i cortexm4f_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top
  .long Startup
  .long NonMaskableInt_interrupt
  .long 0
  .long MemoryManagement_interrupt
  .long BusFault_interrupt
  .long UsageFault_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long SVCall_interrupt
  .long DebugMonitor_interrupt
  .long 0
  .long PendSV_interrupt
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
  .long DMA1_Stream0_interrupt
  .long DMA1_Stream1_interrupt
  .long DMA1_Stream2_interrupt
  .long DMA1_Stream3_interrupt
  .long DMA1_Stream4_interrupt
  .long DMA1_Stream5_interrupt
  .long DMA1_Stream6_interrupt
  .long ADC_interrupt
  .long CAN1_TX_interrupt
  .long CAN1_RX0_interrupt
  .long CAN1_RX1_interrupt
  .long CAN1_SCE_interrupt
  .long EXTI9_5_interrupt
  .long TIM1_BRK_TIM9_interrupt
  .long TIM1_UP_TIM10_interrupt
  .long TIM1_TRG_COM_TIM11_interrupt
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
  .long RTC_Alarm_interrupt
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
  .long TIM6_DAC_interrupt
  .long TIM7_interrupt
  .long DMA2_Stream0_interrupt
  .long DMA2_Stream1_interrupt
  .long DMA2_Stream2_interrupt
  .long DMA2_Stream3_interrupt
  .long DMA2_Stream4_interrupt
  .long ETH_interrupt
  .long ETH_WKUP_interrupt
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
  .long 0
  .long HASH_RNG_interrupt
  .long FPU_interrupt
  .weak NonMaskableInt_interrupt
  .weak MemoryManagement_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SVCall_interrupt
  .weak DebugMonitor_interrupt
  .weak PendSV_interrupt
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
  .weak DMA1_Stream0_interrupt
  .weak DMA1_Stream1_interrupt
  .weak DMA1_Stream2_interrupt
  .weak DMA1_Stream3_interrupt
  .weak DMA1_Stream4_interrupt
  .weak DMA1_Stream5_interrupt
  .weak DMA1_Stream6_interrupt
  .weak ADC_interrupt
  .weak CAN1_TX_interrupt
  .weak CAN1_RX0_interrupt
  .weak CAN1_RX1_interrupt
  .weak CAN1_SCE_interrupt
  .weak EXTI9_5_interrupt
  .weak TIM1_BRK_TIM9_interrupt
  .weak TIM1_UP_TIM10_interrupt
  .weak TIM1_TRG_COM_TIM11_interrupt
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
  .weak RTC_Alarm_interrupt
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
  .weak TIM6_DAC_interrupt
  .weak TIM7_interrupt
  .weak DMA2_Stream0_interrupt
  .weak DMA2_Stream1_interrupt
  .weak DMA2_Stream2_interrupt
  .weak DMA2_Stream3_interrupt
  .weak DMA2_Stream4_interrupt
  .weak ETH_interrupt
  .weak ETH_WKUP_interrupt
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
  .weak HASH_RNG_interrupt
  .weak FPU_interrupt
  .set NonMaskableInt_interrupt, HaltProc
  .set MemoryManagement_interrupt, HaltProc
  .set BusFault_interrupt, HaltProc
  .set UsageFault_interrupt, HaltProc
  .set SVCall_interrupt, HaltProc
  .set DebugMonitor_interrupt, HaltProc
  .set PendSV_interrupt, HaltProc
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
  .set DMA1_Stream0_interrupt, HaltProc
  .set DMA1_Stream1_interrupt, HaltProc
  .set DMA1_Stream2_interrupt, HaltProc
  .set DMA1_Stream3_interrupt, HaltProc
  .set DMA1_Stream4_interrupt, HaltProc
  .set DMA1_Stream5_interrupt, HaltProc
  .set DMA1_Stream6_interrupt, HaltProc
  .set ADC_interrupt, HaltProc
  .set CAN1_TX_interrupt, HaltProc
  .set CAN1_RX0_interrupt, HaltProc
  .set CAN1_RX1_interrupt, HaltProc
  .set CAN1_SCE_interrupt, HaltProc
  .set EXTI9_5_interrupt, HaltProc
  .set TIM1_BRK_TIM9_interrupt, HaltProc
  .set TIM1_UP_TIM10_interrupt, HaltProc
  .set TIM1_TRG_COM_TIM11_interrupt, HaltProc
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
  .set RTC_Alarm_interrupt, HaltProc
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
  .set TIM6_DAC_interrupt, HaltProc
  .set TIM7_interrupt, HaltProc
  .set DMA2_Stream0_interrupt, HaltProc
  .set DMA2_Stream1_interrupt, HaltProc
  .set DMA2_Stream2_interrupt, HaltProc
  .set DMA2_Stream3_interrupt, HaltProc
  .set DMA2_Stream4_interrupt, HaltProc
  .set ETH_interrupt, HaltProc
  .set ETH_WKUP_interrupt, HaltProc
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
  .set HASH_RNG_interrupt, HaltProc
  .set FPU_interrupt, HaltProc
  .text
end;
end.

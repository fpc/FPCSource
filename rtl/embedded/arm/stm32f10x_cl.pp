unit stm32f10x_cl;
interface
{$goto on}
{$PACKRECORDS 2}
//*
//******************************************************************************
//* @file    stm32f0xx.h
//* @author  MCD Application Team
//* @version V1.0.1
//* @date    20-April-2012
//  CMSIS Cortex-M0 Device Peripheral Access Layer Header File.
//*          This file contains all the peripheral register's definitions, bits
//*          definitions and memory mapping for STM32F0xx devices.
//*
//*          The file is the unique include file that the application programmer
//*          is using in the C source code, usually in main.c. This file contains:
//*           - Configuration section that allows to select:
//*              - The device used in the target application
//*              - To use or not the peripheral’s drivers in application code(i.e.
//*                code will be based on direct access to peripheral’s registers
//*                rather than drivers API), this option is controlled by
//*                "#define USE_STDPERIPH_DRIVER"
//*              - To change few application-specific parameters such as the HSE
//*                crystal frequency
//*           - Data structures and the address mapping for all peripherals
//*           - Peripheral's registers declarations and bits definition
//*           - Macros to access peripheral’s registers hardware
//*
//******************************************************************************
//* @attention
//*
//* <h2><center>&copy; COPYRIGHT 2012 STMicroelectronics</center></h2>
//*
//* Licensed under MCD-ST Liberty SW License Agreement V2, (the "License");
//* You may not use this file except in compliance with the License.
//* You may obtain a copy of the License at:
//*
//*        http://www.st.com/software_license_agreement_liberty_v2
//*
//* Unless required by applicable law or agreed to in writing, software
//* distributed under the License is distributed on an "AS IS" BASIS,
//* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//* See the License for the specific language governing permissions and
//* limitations under the License.
//*
//******************************************************************************

//Uncomment the line below according to the target STM32F0 device used in your
//application

//Tip: To avoid modifying this file each time you need to switch between these
//devices, you can define the device in your toolchain compiler preprocessor.

//STM32F0xx devices are:
//- STM32F050xx microcontrollers where the Flash memory density can go up to 32 Kbytes.
//- STM32F051xx microcontrollers where the Flash memory density can go up to 64 Kbytes.

//Comment the line below if you will not use the peripherals drivers.
//In this case, these drivers will not be included and the application code will
//be based on direct access to peripherals registers

//#define USE_STDPERIPH_DRIVER

//In the following line adjust the value of External High Speed oscillator (HSE)
//used in your application

//Tip: To avoid modifying this file each time you need to use different HSE, you
//can define the HSE value in your toolchain compiler preprocessor.

const
  HSE_VALUE = longword(8000000);                  //Value of the External oscillator in Hz

//In the following line adjust the External High Speed oscillator (HSE) Startup
//Timeout value

  HSE_STARTUP_TIMEOUT = longword($0500);          //Time out for HSE start up

//In the following line adjust the Internal High Speed oscillator (HSI) Startup
//Timeout value

  HSI_STARTUP_TIMEOUT = longword($0500);          //Time out for HSI start up

  HSI_VALUE = longword(8000000);                  //Value of the Internal High Speed oscillator in Hz.
//The real value may vary depending on the variations
//in voltage and temperature.

  HSI14_VALUE = longword(14000000);               //Value of the Internal High Speed oscillator for ADC in Hz.
//The real value may vary depending on the variations
//in voltage and temperature.

  LSI_VALUE = longword(40000);                    //Value of the Internal Low Speed oscillator in Hz
//The real value may vary depending on the variations
//in voltage and temperature.

  LSE_VALUE = longword(32768);                    //Value of the External Low Speed oscillator in Hz

//STM32F0xx Standard Peripheral Library version number V1.0.1

  __STM32F0XX_STDPERIPH_VERSION_MAIN = $01;       //[31:24] main version
  __STM32F0XX_STDPERIPH_VERSION_SUB1 = $00;       //[23:16] sub1 version
  __STM32F0XX_STDPERIPH_VERSION_SUB2 = $01;       //[15:8]  sub2 version
  __STM32F0XX_STDPERIPH_VERSION_RC = $00;         //[7:0]  release candidate

//STM32F0xx Interrupt Number Definition, according to the selected device
//*        in @ref Library_configuration_section

  __CM0_REV = 0;                                  //Core Revision r0p0
  __MPU_PRESENT = 0;                              //STM32F0xx do not provide MPU
  __NVIC_PRIO_BITS = 2;                           //STM32F0xx uses 2 Bits for the Priority Levels
  __Vendor_SysTickConfig = 0;                     //Set to 1 if different SysTick Config is used

//Interrupt Number Definition
type
  TIRQn_Enum = (

//*****  Cortex-M0 Processor Exceptions Numbers *****************************************************
    NonMaskableInt_IRQn = -14,                    //2 Non Maskable Interrupt
    HardFault_IRQn = -13,                         //3 Cortex-M0 Hard Fault Interrupt
    SVC_IRQn = -5,                                //11 Cortex-M0 SV Call Interrupt
    PendSV_IRQn = -2,                             //14 Cortex-M0 Pend SV Interrupt
    SysTick_IRQn = -1,                            //15 Cortex-M0 System Tick Interrupt

//*****  STM32F-0 specific Interrupt Numbers ********************************************************
    WWDG_IRQn = 0,                                //Window WatchDog Interrupt
    PVD_IRQn = 1,                                 //PVD through EXTI Line detect Interrupt
    RTC_IRQn = 2,                                 //RTC through EXTI Line Interrupt
    FLASH_IRQn = 3,                               //FLASH Interrupt
    RCC_IRQn = 4,                                 //RCC Interrupt
    EXTI0_1_IRQn = 5,                             //EXTI Line 0 and 1 Interrupts
    EXTI2_3_IRQn = 6,                             //EXTI Line 2 and 3 Interrupts
    EXTI4_15_IRQn = 7,                            //EXTI Line 4 to 15 Interrupts
    TS_IRQn = 8,                                  //TS Interrupt
    DMA1_Channel1_IRQn = 9,                       //DMA1 Channel 1 Interrupt
    DMA1_Channel2_3_IRQn = 10,                    //DMA1 Channel 2 and Channel 3 Interrupts
    DMA1_Channel4_5_IRQn = 11,                    //DMA1 Channel 4 and Channel 5 Interrupts
    ADC1_COMP_IRQn = 12,                          //ADC1, COMP1 and COMP2 Interrupts
    TIM1_BRK_UP_TRG_COM_IRQn = 13,                //TIM1 Break, Update, Trigger and Commutation Interrupts
    TIM1_CC_IRQn = 14,                            //TIM1 Capture Compare Interrupt
    TIM2_IRQn = 15,                               //TIM2 Interrupt
    TIM3_IRQn = 16,                               //TIM3 Interrupt
    TIM6_DAC_IRQn = 17,                           //TIM6 and DAC Interrupts
    TIM14_IRQn = 19,                              //TIM14 Interrupt
    TIM15_IRQn = 20,                              //TIM15 Interrupt
    TIM16_IRQn = 21,                              //TIM16 Interrupt
    TIM17_IRQn = 22,                              //TIM17 Interrupt
    I2C1_IRQn = 23,                               //I2C1 Interrupt
    I2C2_IRQn = 24,                               //I2C2 Interrupt
    SPI1_IRQn = 25,                               //SPI1 Interrupt
    SPI2_IRQn = 26,                               //SPI2 Interrupt
    USART1_IRQn = 27,                             //USART1 Interrupt
    USART2_IRQn = 28,                             //USART2 Interrupt
    CEC_IRQn = 30                                 //CEC Interrupt
  );

//Analog to Digital Converter

  TADC_Registers = record

    ISR : longword;                               //ADC Interrupt and Status register,                          Address offset:0x00
    IER : longword;                               //ADC Interrupt Enable register,                              Address offset:0x04
    CR : longword;                                //ADC Control register,                                       Address offset:0x08
    CFGR1 : longword;                             //ADC Configuration register 1,                               Address offset:0x0C
    CFGR2 : longword;                             //ADC Configuration register 2,                               Address offset:0x10
    SMPR : longword;                              //ADC Sampling time register,                                 Address offset:0x14
    RESERVED1 : longword;                         //Reserved,                                                                  0x18
    RESERVED2 : longword;                         //Reserved,                                                                  0x1C
    TR : longword;                                //ADC watchdog threshold register,                            Address offset:0x20
    RESERVED3 : longword;                         //Reserved,                                                                  0x24
    CHSELR : longword;                            //ADC channel selection register,                             Address offset:0x28
    RESERVED4 : array[0..4] of longword;          //Reserved,                                                                  0x2C
    DR : longword;                                //ADC data register,                                          Address offset:0x40
  end;

  TADC_Common_Registers = record

    CCR : longword;
  end;

//HDMI-CEC

  TCEC_Registers = record

    CR : longword;                                //CEC control register,                                       Address offset:0x00
    CFGR : longword;                              //CEC configuration register,                                 Address offset:0x04
    TXDR : longword;                              //CEC Tx data register ,                                      Address offset:0x08
    RXDR : longword;                              //CEC Rx Data Register,                                       Address offset:0x0C
    ISR : longword;                               //CEC Interrupt and Status Register,                          Address offset:0x10
    IER : longword;                               //CEC interrupt enable register,                              Address offset:0x14
  end;

//Comparator

  TCOMP_Registers = record

    CSR : longword;                               //COMP comparator control and status register, Address offset: 0x1C
  end;

//CRC calculation unit

  TCRC_Registers = record

    DR : longword;                                //CRC Data register,                           Address offset: 0x00
    IDR : byte;                                   //CRC Independent data register,               Address offset: 0x04
    RESERVED0 : byte;                             //Reserved,                                                    0x05
    RESERVED1 : word;                             //Reserved,                                                    0x06
    CR : longword;                                //CRC Control register,                        Address offset: 0x08
    RESERVED2 : longword;                         //Reserved,                                                    0x0C
    INIT : longword;                              //Initial CRC value register,                  Address offset: 0x10
  end;

//Digital to Analog Converter

  TDAC_Registers = record

    CR : longword;                                //DAC control register,                                     Address offset: 0x00
    SWTRIGR : longword;                           //DAC software trigger register,                            Address offset: 0x04
    DHR12R1 : longword;                           //DAC channel1 12-bit right-aligned data holding register,  Address offset: 0x08
    DHR12L1 : longword;                           //DAC channel1 12-bit left aligned data holding register,   Address offset: 0x0C
    DHR8R1 : longword;                            //DAC channel1 8-bit right aligned data holding register,   Address offset: 0x10
    RESERVED : array[0..5] of longword;           //Reserved,                                                                 0x14
    DOR1 : longword;                              //DAC channel1 data output register,                        Address offset: 0x2C
    RESERVED1 : longword;                         //Reserved,                                                                 0x30
    SR : longword;                                //DAC status register,                                      Address offset: 0x34
  end;

//Debug MCU

  TDBGMCU_Registers = record

    IDCODE : longword;                            //MCU device ID code,                          Address offset: 0x00
    CR : longword;                                //Debug MCU configuration register,            Address offset: 0x04
    APB1FZ : longword;                            //Debug MCU APB1 freeze register,              Address offset: 0x08
    APB2FZ : longword;                            //Debug MCU APB2 freeze register,              Address offset: 0x0C
  end;

//DMA Controller

  TDMA_Channel_Registers = record

    CCR : longword;                               //DMA channel x configuration register
    CNDTR : longword;                             //DMA channel x number of data register
    CPAR : longword;                              //DMA channel x peripheral address register
    CMAR : longword;                              //DMA channel x memory address register
  end;

  TDMA_Registers = record

    ISR : longword;                               //DMA interrupt status register,                            Address offset: 0x00
    IFCR : longword;                              //DMA interrupt flag clear register,                        Address offset: 0x04
  end;

//External Interrupt/Event Controller

  TEXTI_Registers = record

    IMR : longword;                               //XTI Interrupt mask register,                             Address offset: 0x00
    EMR : longword;                               //XTI Event mask register,                                 Address offset: 0x04
    RTSR : longword;                              //XTI Rising trigger selection register ,                  Address offset: 0x08
    FTSR : longword;                              //XTI Falling trigger selection register,                  Address offset: 0x0C
    SWIER : longword;                             //XTI Software interrupt event register,                   Address offset: 0x10
    PR : longword;                                //XTI Pending register,                                    Address offset: 0x14
  end;

//FLASH Registers

  TFLASH_Registers = record

    ACR : longword;                               //LASH access control register,                 Address offset: 0x00
    KEYR : longword;                              //LASH key register,                            Address offset: 0x04
    OPTKEYR : longword;                           //LASH OPT key register,                        Address offset: 0x08
    SR : longword;                                //LASH status register,                         Address offset: 0x0C
    CR : longword;                                //LASH control register,                        Address offset: 0x10
    AR : longword;                                //LASH address register,                        Address offset: 0x14
    RESERVED : longword;                          //Reserved,                                                     0x18
    OBR : longword;                               //LASH option bytes register,                   Address offset: 0x1C
    WRPR : longword;                              //LASH option bytes register,                   Address offset: 0x20
  end;

//Option Bytes Registers

  TOB_Registers = record

    RDP : word;                                   //LASH option byte Read protection,             Address offset: 0x00
    USER : word;                                  //LASH option byte user options,                Address offset: 0x02
    RESERVED0 : word;                             //Reserved,                                                     0x04
    RESERVED1 : word;                             //Reserved,                                                     0x06
    WRP0 : word;                                  //LASH option byte write protection 0,          Address offset: 0x08
    WRP1 : word;                                  //LASH option byte write protection 1,          Address offset: 0x0C
  end;

//General Purpose IO

  TGPIO_Registers = record

    MODER : longword;                             //GPIO port mode register,                                  Address offset: 0x00
    OTYPER : word;                                //GPIO port output type register,                           Address offset: 0x04
    RESERVED0 : word;                             //Reserved,                                                                 0x06
    OSPEEDR : longword;                           //GPIO port output speed register,                          Address offset: 0x08
    PUPDR : longword;                             //GPIO port pull-up/pull-down register,                     Address offset: 0x0C
    IDR : word;                                   //GPIO port input data register,                            Address offset: 0x10
    RESERVED1 : word;                             //Reserved,                                                                 0x12
    ODR : word;                                   //GPIO port output data register,                           Address offset: 0x14
    RESERVED2 : word;                             //Reserved,                                                                 0x16
    BSRR : longword;                              //GPIO port bit set/reset registerBSRR,                     Address offset: 0x18
    LCKR : longword;                              //GPIO port configuration lock register,                    Address offset: 0x1C
    AFR : array[0..1] of longword;                //GPIO alternate function low register,                Address offset: 0x20-0x24
    BRR : word;                                   //GPIO bit reset register,                                  Address offset: 0x28
    RESERVED3 : word;                             //Reserved,                                                                 0x2A
  end;

//SysTem Configuration

  TSYSCFG_Registers = record

    CFGR1 : longword;                             //SYSCFG configuration register 1,                           Address offset: 0x00
    RESERVED : longword;                          //Reserved,                                                                  0x04
    EXTICR : array[0..3] of longword;             //SYSCFG external interrupt configuration register,     Address offset: 0x14-0x08
    CFGR2 : longword;                             //SYSCFG configuration register 2,                           Address offset: 0x18
  end;

//Inter-integrated Circuit Interface

  TI2C_Registers = record

    CR1 : longword;                               //I2C Control register 1,            Address offset: 0x00
    CR2 : longword;                               //I2C Control register 2,            Address offset: 0x04
    OAR1 : longword;                              //I2C Own address 1 register,        Address offset: 0x08
    OAR2 : longword;                              //I2C Own address 2 register,        Address offset: 0x0C
    TIMINGR : longword;                           //I2C Timing register,               Address offset: 0x10
    TIMEOUTR : longword;                          //I2C Timeout register,              Address offset: 0x14
    ISR : longword;                               //I2C Interrupt and status register, Address offset: 0x18
    ICR : longword;                               //I2C Interrupt clear register,      Address offset: 0x1C
    PECR : longword;                              //I2C PEC register,                  Address offset: 0x20
    RXDR : longword;                              //I2C Receive data register,         Address offset: 0x24
    TXDR : longword;                              //I2C Transmit data register,        Address offset: 0x28
  end;

//Independent WATCHDOG

  TIWDG_Registers = record

    KR : longword;                                //IWDG Key register,       Address offset: 0x00
    PR : longword;                                //IWDG Prescaler register, Address offset: 0x04
    RLR : longword;                               //IWDG Reload register,    Address offset: 0x08
    SR : longword;                                //IWDG Status register,    Address offset: 0x0C
    WINR : longword;                              //IWDG Window register,    Address offset: 0x10
  end;

//Power Control

  TPWR_Registers = record

    CR : longword;                                //PWR power control register,        Address offset: 0x00
    CSR : longword;                               //PWR power control/status register, Address offset: 0x04
  end;

//Reset and Clock Control

  TRCC_Registers = record

    CR : longword;                                //RCC clock control register,                                  Address offset: 0x00
    CFGR : longword;                              //RCC clock configuration register,                            Address offset: 0x04
    CIR : longword;                               //RCC clock interrupt register,                                Address offset: 0x08
    APB2RSTR : longword;                          //RCC APB2 peripheral reset register,                          Address offset: 0x0C
    APB1RSTR : longword;                          //RCC APB1 peripheral reset register,                          Address offset: 0x10
    AHBENR : longword;                            //RCC AHB peripheral clock register,                           Address offset: 0x14
    APB2ENR : longword;                           //RCC APB2 peripheral clock enable register,                   Address offset: 0x18
    APB1ENR : longword;                           //RCC APB1 peripheral clock enable register,                   Address offset: 0x1C
    BDCR : longword;                              //RCC Backup domain control register,                          Address offset: 0x20
    CSR : longword;                               //RCC clock control & status register,                         Address offset: 0x24
    AHBRSTR : longword;                           //RCC AHB peripheral reset register,                           Address offset: 0x28
    CFGR2 : longword;                             //RCC clock configuration register 2,                          Address offset: 0x2C
    CFGR3 : longword;                             //RCC clock configuration register 3,                          Address offset: 0x30
    CR2 : longword;                               //RCC clock control register 2,                                Address offset: 0x34
  end;

//Real-Time Clock

  TRTC_Registers = record

    TR : longword;                                //RTC time register,                                        Address offset: 0x00
    DR : longword;                                //RTC date register,                                        Address offset: 0x04
    CR : longword;                                //RTC control register,                                     Address offset: 0x08
    ISR : longword;                               //RTC initialization and status register,                   Address offset: 0x0C
    PRER : longword;                              //RTC prescaler register,                                   Address offset: 0x10
    RESERVED0 : longword;                         //Reserved,                                                 Address offset: 0x14
    RESERVED1 : longword;                         //Reserved,                                                 Address offset: 0x18
    ALRMAR : longword;                            //RTC alarm A register,                                     Address offset: 0x1C
    RESERVED2 : longword;                         //Reserved,                                                 Address offset: 0x20
    WPR : longword;                               //RTC write protection register,                            Address offset: 0x24
    SSR : longword;                               //RTC sub second register,                                  Address offset: 0x28
    SHIFTR : longword;                            //RTC shift control register,                               Address offset: 0x2C
    TSTR : longword;                              //RTC time stamp time register,                             Address offset: 0x30
    TSDR : longword;                              //RTC time stamp date register,                             Address offset: 0x34
    TSSSR : longword;                             //RTC time-stamp sub second register,                       Address offset: 0x38
    CAL : longword;                               //RTC calibration register,                                 Address offset: 0x3C
    TAFCR : longword;                             //RTC tamper and alternate function configuration register, Address offset: 0x40
    ALRMASSR : longword;                          //RTC alarm A sub second register,                          Address offset: 0x44
    RESERVED3 : longword;                         //Reserved,                                                 Address offset: 0x48
    RESERVED4 : longword;                         //Reserved,                                                 Address offset: 0x4C
    BKP0R : longword;                             //RTC backup register 0,                                    Address offset: 0x50
    BKP1R : longword;                             //RTC backup register 1,                                    Address offset: 0x54
    BKP2R : longword;                             //RTC backup register 2,                                    Address offset: 0x58
    BKP3R : longword;                             //RTC backup register 3,                                    Address offset: 0x5C
    BKP4R : longword;                             //RTC backup register 4,                                    Address offset: 0x60
  end;

//Serial Peripheral Interface

  TSPI_Registers = record

    CR1 : word;                                   //SPI Control register 1 (not used in I2S mode),       Address offset: 0x00
    RESERVED0 : word;                             //Reserved, 0x02
    CR2 : word;                                   //SPI Control register 2,                              Address offset: 0x04
    RESERVED1 : word;                             //Reserved, 0x06
    SR : word;                                    //SPI Status register,                                 Address offset: 0x08
    RESERVED2 : word;                             //Reserved, 0x0A
    DR : word;                                    //SPI data register,                                   Address offset: 0x0C
    RESERVED3 : word;                             //Reserved, 0x0E
    CRCPR : word;                                 //SPI CRC polynomial register (not used in I2S mode),  Address offset: 0x10
    RESERVED4 : word;                             //Reserved, 0x12
    RXCRCR : word;                                //SPI Rx CRC register (not used in I2S mode),          Address offset: 0x14
    RESERVED5 : word;                             //Reserved, 0x16
    TXCRCR : word;                                //SPI Tx CRC register (not used in I2S mode),          Address offset: 0x18
    RESERVED6 : word;                             //Reserved, 0x1A
    I2SCFGR : word;                               //SPI_I2S configuration register,                      Address offset: 0x1C
    RESERVED7 : word;                             //Reserved, 0x1E
    I2SPR : word;                                 //SPI_I2S prescaler register,                          Address offset: 0x20
    RESERVED8 : word;                             //Reserved, 0x22
  end;

//TIM

  TTIM_Registers = record

    CR1 : word;                                   //TIM control register 1,                      Address offset: 0x00
    RESERVED0 : word;                             //Reserved,                                                    0x02
    CR2 : word;                                   //TIM control register 2,                      Address offset: 0x04
    RESERVED1 : word;                             //Reserved,                                                    0x06
    SMCR : word;                                  //TIM slave Mode Control register,             Address offset: 0x08
    RESERVED2 : word;                             //Reserved,                                                    0x0A
    DIER : word;                                  //TIM DMA/interrupt enable register,           Address offset: 0x0C
    RESERVED3 : word;                             //Reserved,                                                    0x0E
    SR : word;                                    //TIM status register,                         Address offset: 0x10
    RESERVED4 : word;                             //Reserved,                                                    0x12
    EGR : word;                                   //TIM event generation register,               Address offset: 0x14
    RESERVED5 : word;                             //Reserved,                                                    0x16
    CCMR1 : word;                                 //TIM  capture/compare mode register 1,        Address offset: 0x18
    RESERVED6 : word;                             //Reserved,                                                    0x1A
    CCMR2 : word;                                 //TIM  capture/compare mode register 2,        Address offset: 0x1C
    RESERVED7 : word;                             //Reserved,                                                    0x1E
    CCER : word;                                  //TIM capture/compare enable register,         Address offset: 0x20
    RESERVED8 : word;                             //Reserved,                                                    0x22
    CNT : longword;                               //TIM counter register,                        Address offset: 0x24
    PSC : word;                                   //TIM prescaler register,                      Address offset: 0x28
    RESERVED10 : word;                            //Reserved,                                                    0x2A
    ARR : longword;                               //TIM auto-reload register,                    Address offset: 0x2C
    RCR : word;                                   //TIM  repetition counter register,            Address offset: 0x30
    RESERVED12 : word;                            //Reserved,                                                    0x32
    CCR1 : longword;                              //TIM capture/compare register 1,              Address offset: 0x34
    CCR2 : longword;                              //TIM capture/compare register 2,              Address offset: 0x38
    CCR3 : longword;                              //TIM capture/compare register 3,              Address offset: 0x3C
    CCR4 : longword;                              //TIM capture/compare register 4,              Address offset: 0x40
    BDTR : word;                                  //TIM break and dead-time register,            Address offset: 0x44
    RESERVED17 : word;                            //Reserved,                                                    0x26
    DCR : word;                                   //TIM DMA control register,                    Address offset: 0x48
    RESERVED18 : word;                            //Reserved,                                                    0x4A
    DMAR : word;                                  //TIM DMA address for full transfer register,  Address offset: 0x4C
    RESERVED19 : word;                            //Reserved,                                                    0x4E
    _OR : word;                                    //TIM option register,                         Address offset: 0x50
    RESERVED20 : word;                            //Reserved,                                                    0x52
  end;

//Touch Sensing Controller (TSC)

  TTSC_Registers = record

    CR : longword;                                //TSC control register,                                     Address offset: 0x00
    IER : longword;                               //TSC interrupt enable register,                            Address offset: 0x04
    ICR : longword;                               //TSC interrupt clear register,                             Address offset: 0x08
    ISR : longword;                               //TSC interrupt status register,                            Address offset: 0x0C
    IOHCR : longword;                             //TSC I/O hysteresis control register,                      Address offset: 0x10
    RESERVED1 : longword;                         //Reserved,                                                 Address offset: 0x14
    IOASCR : longword;                            //TSC I/O analog switch control register,                   Address offset: 0x18
    RESERVED2 : longword;                         //Reserved,                                                 Address offset: 0x1C
    IOSCR : longword;                             //TSC I/O sampling control register,                        Address offset: 0x20
    RESERVED3 : longword;                         //Reserved,                                                 Address offset: 0x24
    IOCCR : longword;                             //TSC I/O channel control register,                         Address offset: 0x28
    RESERVED4 : longword;                         //Reserved,                                                 Address offset: 0x2C
    IOGCSR : longword;                            //TSC I/O group control status register,                    Address offset: 0x30
    IOGXCR : array[0..5] of longword;             //TSC I/O group x counter register,                         Address offset: 0x34-48
  end;

//Universal Synchronous Asynchronous Receiver Transmitter

  TUSART_Registers = record

    CR1 : longword;                               //USART Control register 1,                 Address offset: 0x00
    CR2 : longword;                               //USART Control register 2,                 Address offset: 0x04
    CR3 : longword;                               //USART Control register 3,                 Address offset: 0x08
    BRR : word;                                   //USART Baud rate register,                 Address offset: 0x0C
    RESERVED1 : word;                             //Reserved, 0x0E
    GTPR : word;                                  //USART Guard time and prescaler register,  Address offset: 0x10
    RESERVED2 : word;                             //Reserved, 0x12
    RTOR : longword;                              //USART Receiver Time Out register,         Address offset: 0x14
    RQR : word;                                   //USART Request register,                   Address offset: 0x18
    RESERVED3 : word;                             //Reserved, 0x1A
    ISR : longword;                               //USART Interrupt and status register,      Address offset: 0x1C
    ICR : longword;                               //USART Interrupt flag Clear register,      Address offset: 0x20
    RDR : word;                                   //USART Receive Data register,              Address offset: 0x24
    RESERVED4 : word;                             //Reserved, 0x26
    TDR : word;                                   //USART Transmit Data register,             Address offset: 0x28
    RESERVED5 : word;                             //Reserved, 0x2A
  end;

//Window WATCHDOG

  TWWDG_Registers = record

    CR : longword;                                //WWDG Control register,       Address offset: 0x00
    CFR : longword;                               //WWDG Configuration register, Address offset: 0x04
    SR : longword;                                //WWDG Status register,        Address offset: 0x08
  end;

const
  FLASH_BASE = longword($08000000);               //FLASH base address in the alias region
  SRAM_BASE = longword($20000000);                //SRAM base address in the alias region
  PERIPH_BASE = longword($40000000);              //Peripheral base address in the alias region

//Peripheral memory map
  APBPERIPH_BASE = PERIPH_BASE;
  AHBPERIPH_BASE = (PERIPH_BASE + $00020000);
  AHB2PERIPH_BASE = (PERIPH_BASE + $08000000);

  TIM2_BASE = (APBPERIPH_BASE + $00000000);
  TIM3_BASE = (APBPERIPH_BASE + $00000400);
  TIM6_BASE = (APBPERIPH_BASE + $00001000);
  TIM14_BASE = (APBPERIPH_BASE + $00002000);
  RTC_BASE = (APBPERIPH_BASE + $00002800);
  WWDG_BASE = (APBPERIPH_BASE + $00002C00);
  IWDG_BASE = (APBPERIPH_BASE + $00003000);
  SPI2_BASE = (APBPERIPH_BASE + $00003800);
  USART2_BASE = (APBPERIPH_BASE + $00004400);
  I2C1_BASE = (APBPERIPH_BASE + $00005400);
  I2C2_BASE = (APBPERIPH_BASE + $00005800);
  PWR_BASE = (APBPERIPH_BASE + $00007000);
  DAC_BASE = (APBPERIPH_BASE + $00007400);
  CEC_BASE = (APBPERIPH_BASE + $00007800);

  SYSCFG_BASE = (APBPERIPH_BASE + $00010000);
  COMP_BASE = (APBPERIPH_BASE + $0001001C);
  EXTI_BASE = (APBPERIPH_BASE + $00010400);
  ADC1_BASE = (APBPERIPH_BASE + $00012400);
  ADC_BASE = (APBPERIPH_BASE + $00012708);
  TIM1_BASE = (APBPERIPH_BASE + $00012C00);
  SPI1_BASE = (APBPERIPH_BASE + $00013000);
  USART1_BASE = (APBPERIPH_BASE + $00013800);
  TIM15_BASE = (APBPERIPH_BASE + $00014000);
  TIM16_BASE = (APBPERIPH_BASE + $00014400);
  TIM17_BASE = (APBPERIPH_BASE + $00014800);
  DBGMCU_BASE = (APBPERIPH_BASE + $00015800);

  DMA1_BASE = (AHBPERIPH_BASE + $00000000);
  DMA1_Channel1_BASE = (DMA1_BASE + $00000008);
  DMA1_Channel2_BASE = (DMA1_BASE + $0000001C);
  DMA1_Channel3_BASE = (DMA1_BASE + $00000030);
  DMA1_Channel4_BASE = (DMA1_BASE + $00000044);
  DMA1_Channel5_BASE = (DMA1_BASE + $00000058);
  RCC_BASE = (AHBPERIPH_BASE + $00001000);
  FLASH_R_BASE = (AHBPERIPH_BASE + $00002000);    //FLASH registers base address
  OB_BASE = longword($1FFFF800);                  //FLASH Option Bytes base address
  CRC_BASE = (AHBPERIPH_BASE + $00003000);
  TSC_BASE = (AHBPERIPH_BASE + $00004000);

  GPIOA_BASE = (AHB2PERIPH_BASE + $00000000);
  GPIOB_BASE = (AHB2PERIPH_BASE + $00000400);
  GPIOC_BASE = (AHB2PERIPH_BASE + $00000800);
  GPIOD_BASE = (AHB2PERIPH_BASE + $00000C00);
  GPIOF_BASE = (AHB2PERIPH_BASE + $00001400);

var
  TIM2 : TTIM_Registers absolute TIM2_BASE;
  TIM3 : TTIM_Registers absolute TIM3_BASE;
  TIM6 : TTIM_Registers absolute TIM6_BASE;
  TIM14 : TTIM_Registers absolute TIM14_BASE;
  RTC : TRTC_Registers absolute RTC_BASE;
  WWDG : TWWDG_Registers absolute WWDG_BASE;
  IWDG : TIWDG_Registers absolute IWDG_BASE;
  SPI2 : TSPI_Registers absolute SPI2_BASE;
  USART2 : TUSART_Registers absolute USART2_BASE;
  I2C1 : TI2C_Registers absolute I2C1_BASE;
  I2C2 : TI2C_Registers absolute I2C2_BASE;
  PWR : TPWR_Registers absolute PWR_BASE;
  DAC : TDAC_Registers absolute DAC_BASE;
  CEC : TCEC_Registers absolute CEC_BASE;

  SYSCFG : TSYSCFG_Registers absolute SYSCFG_BASE;
  COMP : TCOMP_Registers absolute COMP_BASE;
  EXTI : TEXTI_Registers absolute EXTI_BASE;
  ADC1 : TADC_Registers absolute ADC1_BASE;
  ADC : TADC_Common_Registers absolute ADC_BASE;
  TIM1 : TTIM_Registers absolute TIM1_BASE;
  SPI1 : TSPI_Registers absolute SPI1_BASE;
  USART1 : TUSART_Registers absolute USART1_BASE;
  TIM15 : TTIM_Registers absolute TIM15_BASE;
  TIM16 : TTIM_Registers absolute TIM16_BASE;
  TIM17 : TTIM_Registers absolute TIM17_BASE;
  DBGMCU : TDBGMCU_Registers absolute DBGMCU_BASE;

  DMA1 : TDMA_Registers absolute DMA1_BASE;
  DMA1_Channel1 : TDMA_Channel_Registers absolute DMA1_Channel1_BASE;
  DMA1_Channel2 : TDMA_Channel_Registers absolute DMA1_Channel2_BASE;
  DMA1_Channel3 : TDMA_Channel_Registers absolute DMA1_Channel3_BASE;
  DMA1_Channel4 : TDMA_Channel_Registers absolute DMA1_Channel4_BASE;
  DMA1_Channel5 : TDMA_Channel_Registers absolute DMA1_Channel5_BASE;
  FLASH : TFLASH_Registers absolute FLASH_R_BASE;
  OB : TOB_Registers absolute OB_BASE;
  RCC : TRCC_Registers absolute RCC_BASE;
  CRC : TCRC_Registers absolute CRC_BASE;
  TSC : TTSC_Registers absolute TSC_BASE;

  GPIOA : TGPIO_Registers absolute GPIOA_BASE;
  GPIOB : TGPIO_Registers absolute GPIOB_BASE;
  GPIOC : TGPIO_Registers absolute GPIOC_BASE;
  GPIOD : TGPIO_Registers absolute GPIOD_BASE;
  GPIOF : TGPIO_Registers absolute GPIOF_BASE;

//****************************************************************************
//Peripheral Registers Bits Definition
//****************************************************************************
//****************************************************************************

//Analog to Digital Converter (ADC)

//****************************************************************************
//*******************  Bits definition for ADC_ISR register  *****************
const
  ADC_ISR_AWD = longword($00000080);              //Analog watchdog flag
  ADC_ISR_OVR = longword($00000010);              //Overrun flag
  ADC_ISR_EOSEQ = longword($00000008);            //End of Sequence flag
  ADC_ISR_EOC = longword($00000004);              //End of Conversion
  ADC_ISR_EOSMP = longword($00000002);            //End of sampling flag
  ADC_ISR_ADRDY = longword($00000001);            //ADC Ready

//Old EOSEQ bit definition, maintained for legacy purpose
  ADC_ISR_EOS = ADC_ISR_EOSEQ;

//*******************  Bits definition for ADC_IER register  *****************
  ADC_IER_AWDIE = longword($00000080);            //Analog Watchdog interrupt enable
  ADC_IER_OVRIE = longword($00000010);            //Overrun interrupt enable
  ADC_IER_EOSEQIE = longword($00000008);          //End of Sequence of conversion interrupt enable
  ADC_IER_EOCIE = longword($00000004);            //End of Conversion interrupt enable
  ADC_IER_EOSMPIE = longword($00000002);          //End of sampling interrupt enable
  ADC_IER_ADRDYIE = longword($00000001);          //ADC Ready interrupt enable

//Old EOSEQIE bit definition, maintained for legacy purpose
  ADC_IER_EOSIE = ADC_IER_EOSEQIE;

//*******************  Bits definition for ADC_CR register  ******************
  ADC_CR_ADCAL = longword($80000000);             //ADC calibration
  ADC_CR_ADSTP = longword($00000010);             //ADC stop of conversion command
  ADC_CR_ADSTART = longword($00000004);           //ADC start of conversion
  ADC_CR_ADDIS = longword($00000002);             //ADC disable command
  ADC_CR_ADEN = longword($00000001);              //ADC enable control

//******************  Bits definition for ADC_CFGR1 register  ****************
  ADC_CFGR1_AWDCH = longword($7C000000);          //AWDCH[4:0] bits (Analog watchdog channel select bits)
  ADC_CFGR1_AWDCH_0 = longword($04000000);        //Bit 0
  ADC_CFGR1_AWDCH_1 = longword($08000000);        //Bit 1
  ADC_CFGR1_AWDCH_2 = longword($10000000);        //Bit 2
  ADC_CFGR1_AWDCH_3 = longword($20000000);        //Bit 3
  ADC_CFGR1_AWDCH_4 = longword($40000000);        //Bit 4
  ADC_CFGR1_AWDEN = longword($00800000);          //Analog watchdog enable on regular channels
  ADC_CFGR1_AWDSGL = longword($00400000);         //Enable the watchdog on a single channel or on all channels
  ADC_CFGR1_DISCEN = longword($00010000);         //Discontinuous mode on regular channels
  ADC_CFGR1_AUTOFF = longword($00008000);         //ADC auto power off
  ADC_CFGR1_WAIT = longword($00004000);           //ADC wait conversion mode
  ADC_CFGR1_CONT = longword($00002000);           //Continuous Conversion
  ADC_CFGR1_OVRMOD = longword($00001000);         //Overrun mode
  ADC_CFGR1_EXTEN = longword($00000C00);          //EXTEN[1:0] bits (External Trigger Conversion mode for regular channels)
  ADC_CFGR1_EXTEN_0 = longword($00000400);        //Bit 0
  ADC_CFGR1_EXTEN_1 = longword($00000800);        //Bit 1
  ADC_CFGR1_EXTSEL = longword($000001C0);         //EXTSEL[2:0] bits (External Event Select for regular group)
  ADC_CFGR1_EXTSEL_0 = longword($00000040);       //Bit 0
  ADC_CFGR1_EXTSEL_1 = longword($00000080);       //Bit 1
  ADC_CFGR1_EXTSEL_2 = longword($00000100);       //Bit 2
  ADC_CFGR1_ALIGN = longword($00000020);          //Data Alignment
  ADC_CFGR1_RES = longword($00000018);            //RES[1:0] bits (Resolution)
  ADC_CFGR1_RES_0 = longword($00000008);          //Bit 0
  ADC_CFGR1_RES_1 = longword($00000010);          //Bit 1
  ADC_CFGR1_SCANDIR = longword($00000004);        //Sequence scan direction
  ADC_CFGR1_DMACFG = longword($00000002);         //Direct memory access configuration
  ADC_CFGR1_DMAEN = longword($00000001);          //Direct memory access enable

//Old WAIT bit definition, maintained for legacy purpose
  ADC_CFGR1_AUTDLY = ADC_CFGR1_WAIT;

//******************  Bits definition for ADC_CFGR2 register  ****************
  ADC_CFGR2_JITOFFDIV4 = longword($80000000);     //Jitter Off when ADC clocked by PCLK div4
  ADC_CFGR2_JITOFFDIV2 = longword($40000000);     //Jitter Off when ADC clocked by PCLK div2

//*****************  Bit definition for ADC_SMPR register  *******************
  ADC_SMPR1_SMPR = longword($00000007);           //SMPR[2:0] bits (Sampling time selection)
  ADC_SMPR1_SMPR_0 = longword($00000001);         //Bit 0
  ADC_SMPR1_SMPR_1 = longword($00000002);         //Bit 1
  ADC_SMPR1_SMPR_2 = longword($00000004);         //Bit 2

//******************  Bit definition for ADC_HTR register  *******************
  ADC_HTR_HT = longword($00000FFF);               //Analog watchdog high threshold

//******************  Bit definition for ADC_LTR register  *******************
  ADC_LTR_LT = longword($00000FFF);               //Analog watchdog low threshold

//*****************  Bit definition for ADC_CHSELR register  *****************
  ADC_CHSELR_CHSEL18 = longword($00040000);       //Channel 18 selection
  ADC_CHSELR_CHSEL17 = longword($00020000);       //Channel 17 selection
  ADC_CHSELR_CHSEL16 = longword($00010000);       //Channel 16 selection
  ADC_CHSELR_CHSEL15 = longword($00008000);       //Channel 15 selection
  ADC_CHSELR_CHSEL14 = longword($00004000);       //Channel 14 selection
  ADC_CHSELR_CHSEL13 = longword($00002000);       //Channel 13 selection
  ADC_CHSELR_CHSEL12 = longword($00001000);       //Channel 12 selection
  ADC_CHSELR_CHSEL11 = longword($00000800);       //Channel 11 selection
  ADC_CHSELR_CHSEL10 = longword($00000400);       //Channel 10 selection
  ADC_CHSELR_CHSEL9 = longword($00000200);        //Channel 9 selection
  ADC_CHSELR_CHSEL8 = longword($00000100);        //Channel 8 selection
  ADC_CHSELR_CHSEL7 = longword($00000080);        //Channel 7 selection
  ADC_CHSELR_CHSEL6 = longword($00000040);        //Channel 6 selection
  ADC_CHSELR_CHSEL5 = longword($00000020);        //Channel 5 selection
  ADC_CHSELR_CHSEL4 = longword($00000010);        //Channel 4 selection
  ADC_CHSELR_CHSEL3 = longword($00000008);        //Channel 3 selection
  ADC_CHSELR_CHSEL2 = longword($00000004);        //Channel 2 selection
  ADC_CHSELR_CHSEL1 = longword($00000002);        //Channel 1 selection
  ADC_CHSELR_CHSEL0 = longword($00000001);        //Channel 0 selection

//*******************  Bit definition for ADC_DR register  *******************
  ADC_DR_DATA = longword($0000FFFF);              //Regular data

//******************  Bit definition for ADC_CCR register  *******************
  ADC_CCR_VBATEN = longword($01000000);           //Voltage battery enable
  ADC_CCR_TSEN = longword($00800000);             //Tempurature sensore enable
  ADC_CCR_VREFEN = longword($00400000);           //Vrefint enable

//****************************************************************************

//HDMI-CEC (CEC)

//****************************************************************************

//******************  Bit definition for CEC_CR register  ********************
  CEC_CR_CECEN = longword($00000001);             //CEC Enable
  CEC_CR_TXSOM = longword($00000002);             //CEC Tx Start Of Message
  CEC_CR_TXEOM = longword($00000004);             //CEC Tx End Of Message

//******************  Bit definition for CEC_CFGR register  ******************
  CEC_CFGR_SFT = longword($00000007);             //CEC Signal Free Time
  CEC_CFGR_RXTOL = longword($00000008);           //CEC Tolerance
  CEC_CFGR_BRESTP = longword($00000010);          //CEC Rx Stop
  CEC_CFGR_BREGEN = longword($00000020);          //CEC Bit Rising Error generation
  CEC_CFGR_LREGEN = longword($00000040);          //CEC Long Period Error generation
  CEC_CFGR_BRDNOGEN = longword($00000080);        //CEC Broadcast no Error generation
  CEC_CFGR_SFTOPT = longword($00000100);          //CEC Signal Free Time optional
  CEC_CFGR_OAR = longword($7FFF0000);             //CEC Own Address
  CEC_CFGR_LSTN = longword($80000000);            //CEC Listen mode

//******************  Bit definition for CEC_TXDR register  ******************
  CEC_TXDR_TXD = longword($000000FF);             //CEC Tx Data

//******************  Bit definition for CEC_RXDR register  ******************
  CEC_TXDR_RXD = longword($000000FF);             //CEC Rx Data

//******************  Bit definition for CEC_ISR register  *******************
  CEC_ISR_RXBR = longword($00000001);             //CEC Rx-Byte Received
  CEC_ISR_RXEND = longword($00000002);            //CEC End Of Reception
  CEC_ISR_RXOVR = longword($00000004);            //CEC Rx-Overrun
  CEC_ISR_BRE = longword($00000008);              //CEC Rx Bit Rising Error
  CEC_ISR_SBPE = longword($00000010);             //CEC Rx Short Bit period Error
  CEC_ISR_LBPE = longword($00000020);             //CEC Rx Long Bit period Error
  CEC_ISR_RXACKE = longword($00000040);           //CEC Rx Missing Acknowledge
  CEC_ISR_ARBLST = longword($00000080);           //CEC Arbitration Lost
  CEC_ISR_TXBR = longword($00000100);             //CEC Tx Byte Request
  CEC_ISR_TXEND = longword($00000200);            //CEC End of Transmission
  CEC_ISR_TXUDR = longword($00000400);            //CEC Tx-Buffer Underrun
  CEC_ISR_TXERR = longword($00000800);            //CEC Tx-Error
  CEC_ISR_TXACKE = longword($00001000);           //CEC Tx Missing Acknowledge

//******************  Bit definition for CEC_IER register  *******************
  CEC_IER_RXBRIE = longword($00000001);           //CEC Rx-Byte Received IT Enable
  CEC_IER_RXENDIE = longword($00000002);          //CEC End Of Reception IT Enable
  CEC_IER_RXOVRIE = longword($00000004);          //CEC Rx-Overrun IT Enable
  CEC_IER_BREIEIE = longword($00000008);          //CEC Rx Bit Rising Error IT Enable
  CEC_IER_SBPEIE = longword($00000010);           //CEC Rx Short Bit period Error IT Enable
  CEC_IER_LBPEIE = longword($00000020);           //CEC Rx Long Bit period Error IT Enable
  CEC_IER_RXACKEIE = longword($00000040);         //CEC Rx Missing Acknowledge IT Enable
  CEC_IER_ARBLSTIE = longword($00000080);         //CEC Arbitration Lost IT Enable
  CEC_IER_TXBRIE = longword($00000100);           //CEC Tx Byte Request  IT Enable
  CEC_IER_TXENDIE = longword($00000200);          //CEC End of Transmission IT Enable
  CEC_IER_TXUDRIE = longword($00000400);          //CEC Tx-Buffer Underrun IT Enable
  CEC_IER_TXERRIE = longword($00000800);          //CEC Tx-Error IT Enable
  CEC_IER_TXACKEIE = longword($00001000);         //CEC Tx Missing Acknowledge IT Enable

//****************************************************************************

//Analog Comparators (COMP)

//****************************************************************************
//**********************  Bit definition for COMP_CSR register  **************
//COMP1 bits definition
  COMP_CSR_COMP1EN = longword($00000001);         //COMP1 enable
  COMP_CSR_COMP1SW1 = longword($00000002);        //SW1 switch control
  COMP_CSR_COMP1MODE = longword($0000000C);       //COMP1 power mode
  COMP_CSR_COMP1MODE_0 = longword($00000004);     //COMP1 power mode bit 0
  COMP_CSR_COMP1MODE_1 = longword($00000008);     //COMP1 power mode bit 1
  COMP_CSR_COMP1INSEL = longword($00000070);      //COMP1 inverting input select
  COMP_CSR_COMP1INSEL_0 = longword($00000010);    //COMP1 inverting input select bit 0
  COMP_CSR_COMP1INSEL_1 = longword($00000020);    //COMP1 inverting input select bit 1
  COMP_CSR_COMP1INSEL_2 = longword($00000040);    //COMP1 inverting input select bit 2
  COMP_CSR_COMP1OUTSEL = longword($00000700);     //COMP1 output select
  COMP_CSR_COMP1OUTSEL_0 = longword($00000100);   //COMP1 output select bit 0
  COMP_CSR_COMP1OUTSEL_1 = longword($00000200);   //COMP1 output select bit 1
  COMP_CSR_COMP1OUTSEL_2 = longword($00000400);   //COMP1 output select bit 2
  COMP_CSR_COMP1POL = longword($00000800);        //COMP1 output polarity
  COMP_CSR_COMP1HYST = longword($00003000);       //COMP1 hysteresis
  COMP_CSR_COMP1HYST_0 = longword($00001000);     //COMP1 hysteresis bit 0
  COMP_CSR_COMP1HYST_1 = longword($00002000);     //COMP1 hysteresis bit 1
  COMP_CSR_COMP1OUT = longword($00004000);        //COMP1 output level
  COMP_CSR_COMP1LOCK = longword($00008000);       //COMP1 lock
//COMP2 bits definition
  COMP_CSR_COMP2EN = longword($00010000);         //COMP2 enable
  COMP_CSR_COMP2MODE = longword($000C0000);       //COMP2 power mode
  COMP_CSR_COMP2MODE_0 = longword($00040000);     //COMP2 power mode bit 0
  COMP_CSR_COMP2MODE_1 = longword($00080000);     //COMP2 power mode bit 1
  COMP_CSR_COMP2INSEL = longword($00700000);      //COMP2 inverting input select
  COMP_CSR_COMP2INSEL_0 = longword($00100000);    //COMP2 inverting input select bit 0
  COMP_CSR_COMP2INSEL_1 = longword($00200000);    //COMP2 inverting input select bit 1
  COMP_CSR_COMP2INSEL_2 = longword($00400000);    //COMP2 inverting input select bit 2
  COMP_CSR_WNDWEN = longword($00800000);          //Comparators window mode enable
  COMP_CSR_COMP2OUTSEL = longword($07000000);     //COMP2 output select
  COMP_CSR_COMP2OUTSEL_0 = longword($01000000);   //COMP2 output select bit 0
  COMP_CSR_COMP2OUTSEL_1 = longword($02000000);   //COMP2 output select bit 1
  COMP_CSR_COMP2OUTSEL_2 = longword($04000000);   //COMP2 output select bit 2
  COMP_CSR_COMP2POL = longword($08000000);        //COMP2 output polarity
  COMP_CSR_COMP2HYST = longword($30000000);       //COMP2 hysteresis
  COMP_CSR_COMP2HYST_0 = longword($10000000);     //COMP2 hysteresis bit 0
  COMP_CSR_COMP2HYST_1 = longword($20000000);     //COMP2 hysteresis bit 1
  COMP_CSR_COMP2OUT = longword($40000000);        //COMP2 output level
  COMP_CSR_COMP2LOCK = longword($80000000);       //COMP2 lock

//****************************************************************************

//CRC calculation unit (CRC)

//****************************************************************************
//******************  Bit definition for CRC_DR register  ********************
  CRC_DR_DR = longword($FFFFFFFF);                //Data register bits

//******************  Bit definition for CRC_IDR register  *******************
  CRC_IDR_IDR = longword($FF);                    //General-purpose 8-bit data register bits

//*******************  Bit definition for CRC_CR register  *******************
  CRC_CR_RESET = longword($00000001);             //RESET the CRC computation unit bit
  CRC_CR_REV_IN = longword($00000060);            //REV_IN Reverse Input Data bits
  CRC_CR_REV_IN_0 = longword($00000020);          //REV_IN Bit 0
  CRC_CR_REV_IN_1 = longword($00000040);          //REV_IN Bit 1
  CRC_CR_REV_OUT = longword($00000080);           //REV_OUT Reverse Output Data bits

//******************  Bit definition for CRC_INIT register  ******************
  CRC_INIT_INIT = longword($FFFFFFFF);            //Initial CRC value bits

//****************************************************************************

//Digital to Analog Converter (DAC)

//****************************************************************************
//*******************  Bit definition for DAC_CR register  *******************
  DAC_CR_EN1 = longword($00000001);               //AC channel1 enable
  DAC_CR_BOFF1 = longword($00000002);             //AC channel1 output buffer disable
  DAC_CR_TEN1 = longword($00000004);              //AC channel1 Trigger enable

  DAC_CR_TSEL1 = longword($00000038);             //SEL1[2:0] (DAC channel1 Trigger selection)
  DAC_CR_TSEL1_0 = longword($00000008);           //it 0
  DAC_CR_TSEL1_1 = longword($00000010);           //it 1
  DAC_CR_TSEL1_2 = longword($00000020);           //it 2

  DAC_CR_DMAEN1 = longword($00001000);            //AC channel1 DMA enable
  DAC_CR_DMAUDRIE1 = longword($00002000);         //AC channel1 DMA Underrun Interrupt enable
//****************  Bit definition for DAC_SWTRIGR register  *****************
  DAC_SWTRIGR_SWTRIG1 = longword($00000001);      //AC channel1 software trigger

//****************  Bit definition for DAC_DHR12R1 register  *****************
  DAC_DHR12R1_DACC1DHR = longword($00000FFF);     //AC channel1 12-bit Right aligned data

//****************  Bit definition for DAC_DHR12L1 register  *****************
  DAC_DHR12L1_DACC1DHR = longword($0000FFF0);     //AC channel1 12-bit Left aligned data

//*****************  Bit definition for DAC_DHR8R1 register  *****************
  DAC_DHR8R1_DACC1DHR = longword($000000FF);      //AC channel1 8-bit Right aligned data

//******************  Bit definition for DAC_DOR1 register  ******************
  DAC_DOR1_DACC1DOR = longword($00000FFF);        //AC channel1 data output

//*******************  Bit definition for DAC_SR register  *******************
  DAC_SR_DMAUDR1 = longword($00002000);           //AC channel1 DMA underrun flag

//****************************************************************************

//Debug MCU (DBGMCU)

//****************************************************************************

//***************  Bit definition for DBGMCU_IDCODE register  ****************
  DBGMCU_IDCODE_DEV_ID = longword($00000FFF);     //Device Identifier

  DBGMCU_IDCODE_REV_ID = longword($FFFF0000);     //REV_ID[15:0] bits (Revision Identifier)
  DBGMCU_IDCODE_REV_ID_0 = longword($00010000);   //Bit 0
  DBGMCU_IDCODE_REV_ID_1 = longword($00020000);   //Bit 1
  DBGMCU_IDCODE_REV_ID_2 = longword($00040000);   //Bit 2
  DBGMCU_IDCODE_REV_ID_3 = longword($00080000);   //Bit 3
  DBGMCU_IDCODE_REV_ID_4 = longword($00100000);   //Bit 4
  DBGMCU_IDCODE_REV_ID_5 = longword($00200000);   //Bit 5
  DBGMCU_IDCODE_REV_ID_6 = longword($00400000);   //Bit 6
  DBGMCU_IDCODE_REV_ID_7 = longword($00800000);   //Bit 7
  DBGMCU_IDCODE_REV_ID_8 = longword($01000000);   //Bit 8
  DBGMCU_IDCODE_REV_ID_9 = longword($02000000);   //Bit 9
  DBGMCU_IDCODE_REV_ID_10 = longword($04000000);  //Bit 10
  DBGMCU_IDCODE_REV_ID_11 = longword($08000000);  //Bit 11
  DBGMCU_IDCODE_REV_ID_12 = longword($10000000);  //Bit 12
  DBGMCU_IDCODE_REV_ID_13 = longword($20000000);  //Bit 13
  DBGMCU_IDCODE_REV_ID_14 = longword($40000000);  //Bit 14
  DBGMCU_IDCODE_REV_ID_15 = longword($80000000);  //Bit 15

//*****************  Bit definition for DBGMCU_CR register  ******************
  DBGMCU_CR_DBG_STOP = longword($00000002);       //Debug Stop Mode
  DBGMCU_CR_DBG_STANDBY = longword($00000004);    //Debug Standby mode

//*****************  Bit definition for DBGMCU_APB1_FZ register  *************
  DBGMCU_APB1_FZ_DBG_TIM2_STOP = longword($00000001); //TIM2 counter stopped when core is halted
  DBGMCU_APB1_FZ_DBG_TIM3_STOP = longword($00000002); //TIM3 counter stopped when core is halted
  DBGMCU_APB1_FZ_DBG_TIM6_STOP = longword($00000010); //TIM6 counter stopped when core is halted
  DBGMCU_APB1_FZ_DBG_TIM14_STOP = longword($00000100); //TIM14 counter stopped when core is halted
  DBGMCU_APB1_FZ_DBG_RTC_STOP = longword($00000400); //RTC Calendar frozen when core is halted
  DBGMCU_APB1_FZ_DBG_WWDG_STOP = longword($00000800); //Debug Window Watchdog stopped when Core is halted
  DBGMCU_APB1_FZ_DBG_IWDG_STOP = longword($00001000); //Debug Independent Watchdog stopped when Core is halted
  DBGMCU_APB1_FZ_DBG_I2C1_SMBUS_TIMEOUT = longword($00200000); //I2C1 SMBUS timeout mode stopped when Core is halted

//*****************  Bit definition for DBGMCU_APB2_FZ register  *************
  DBGMCU_APB2_FZ_DBG_TIM1_STOP = longword($00000800); //TIM1 counter stopped when core is halted
  DBGMCU_APB2_FZ_DBG_TIM15_STOP = longword($00010000); //TIM15 counter stopped when core is halted
  DBGMCU_APB2_FZ_DBG_TIM16_STOP = longword($00020000); //TIM16 counter stopped when core is halted
  DBGMCU_APB2_FZ_DBG_TIM17_STOP = longword($00040000); //TIM17 counter stopped when core is halted

//****************************************************************************

//DMA Controller (DMA)

//****************************************************************************

//******************  Bit definition for DMA_ISR register  *******************
  DMA_ISR_GIF1 = longword($00000001);             //Channel 1 Global interrupt flag
  DMA_ISR_TCIF1 = longword($00000002);            //Channel 1 Transfer Complete flag
  DMA_ISR_HTIF1 = longword($00000004);            //Channel 1 Half Transfer flag
  DMA_ISR_TEIF1 = longword($00000008);            //Channel 1 Transfer Error flag
  DMA_ISR_GIF2 = longword($00000010);             //Channel 2 Global interrupt flag
  DMA_ISR_TCIF2 = longword($00000020);            //Channel 2 Transfer Complete flag
  DMA_ISR_HTIF2 = longword($00000040);            //Channel 2 Half Transfer flag
  DMA_ISR_TEIF2 = longword($00000080);            //Channel 2 Transfer Error flag
  DMA_ISR_GIF3 = longword($00000100);             //Channel 3 Global interrupt flag
  DMA_ISR_TCIF3 = longword($00000200);            //Channel 3 Transfer Complete flag
  DMA_ISR_HTIF3 = longword($00000400);            //Channel 3 Half Transfer flag
  DMA_ISR_TEIF3 = longword($00000800);            //Channel 3 Transfer Error flag
  DMA_ISR_GIF4 = longword($00001000);             //Channel 4 Global interrupt flag
  DMA_ISR_TCIF4 = longword($00002000);            //Channel 4 Transfer Complete flag
  DMA_ISR_HTIF4 = longword($00004000);            //Channel 4 Half Transfer flag
  DMA_ISR_TEIF4 = longword($00008000);            //Channel 4 Transfer Error flag
  DMA_ISR_GIF5 = longword($00010000);             //Channel 5 Global interrupt flag
  DMA_ISR_TCIF5 = longword($00020000);            //Channel 5 Transfer Complete flag
  DMA_ISR_HTIF5 = longword($00040000);            //Channel 5 Half Transfer flag
  DMA_ISR_TEIF5 = longword($00080000);            //Channel 5 Transfer Error flag

//******************  Bit definition for DMA_IFCR register  ******************
  DMA_IFCR_CGIF1 = longword($00000001);           //Channel 1 Global interrupt clear
  DMA_IFCR_CTCIF1 = longword($00000002);          //Channel 1 Transfer Complete clear
  DMA_IFCR_CHTIF1 = longword($00000004);          //Channel 1 Half Transfer clear
  DMA_IFCR_CTEIF1 = longword($00000008);          //Channel 1 Transfer Error clear
  DMA_IFCR_CGIF2 = longword($00000010);           //Channel 2 Global interrupt clear
  DMA_IFCR_CTCIF2 = longword($00000020);          //Channel 2 Transfer Complete clear
  DMA_IFCR_CHTIF2 = longword($00000040);          //Channel 2 Half Transfer clear
  DMA_IFCR_CTEIF2 = longword($00000080);          //Channel 2 Transfer Error clear
  DMA_IFCR_CGIF3 = longword($00000100);           //Channel 3 Global interrupt clear
  DMA_IFCR_CTCIF3 = longword($00000200);          //Channel 3 Transfer Complete clear
  DMA_IFCR_CHTIF3 = longword($00000400);          //Channel 3 Half Transfer clear
  DMA_IFCR_CTEIF3 = longword($00000800);          //Channel 3 Transfer Error clear
  DMA_IFCR_CGIF4 = longword($00001000);           //Channel 4 Global interrupt clear
  DMA_IFCR_CTCIF4 = longword($00002000);          //Channel 4 Transfer Complete clear
  DMA_IFCR_CHTIF4 = longword($00004000);          //Channel 4 Half Transfer clear
  DMA_IFCR_CTEIF4 = longword($00008000);          //Channel 4 Transfer Error clear
  DMA_IFCR_CGIF5 = longword($00010000);           //Channel 5 Global interrupt clear
  DMA_IFCR_CTCIF5 = longword($00020000);          //Channel 5 Transfer Complete clear
  DMA_IFCR_CHTIF5 = longword($00040000);          //Channel 5 Half Transfer clear
  DMA_IFCR_CTEIF5 = longword($00080000);          //Channel 5 Transfer Error clear

//******************  Bit definition for DMA_CCR register  *******************
  DMA_CCR_EN = longword($00000001);               //Channel enable
  DMA_CCR_TCIE = longword($00000002);             //Transfer complete interrupt enable
  DMA_CCR_HTIE = longword($00000004);             //Half Transfer interrupt enable
  DMA_CCR_TEIE = longword($00000008);             //Transfer error interrupt enable
  DMA_CCR_DIR = longword($00000010);              //Data transfer direction
  DMA_CCR_CIRC = longword($00000020);             //Circular mode
  DMA_CCR_PINC = longword($00000040);             //Peripheral increment mode
  DMA_CCR_MINC = longword($00000080);             //Memory increment mode

  DMA_CCR_PSIZE = longword($00000300);            //PSIZE[1:0] bits (Peripheral size)
  DMA_CCR_PSIZE_0 = longword($00000100);          //Bit 0
  DMA_CCR_PSIZE_1 = longword($00000200);          //Bit 1

  DMA_CCR_MSIZE = longword($00000C00);            //MSIZE[1:0] bits (Memory size)
  DMA_CCR_MSIZE_0 = longword($00000400);          //Bit 0
  DMA_CCR_MSIZE_1 = longword($00000800);          //Bit 1

  DMA_CCR_PL = longword($00003000);               //PL[1:0] bits(Channel Priority level)
  DMA_CCR_PL_0 = longword($00001000);             //Bit 0
  DMA_CCR_PL_1 = longword($00002000);             //Bit 1

  DMA_CCR_MEM2MEM = longword($00004000);          //Memory to memory mode

//*****************  Bit definition for DMA_CNDTR register  ******************
  DMA_CNDTR_NDT = longword($0000FFFF);            //Number of data to Transfer

//*****************  Bit definition for DMA_CPAR register  *******************
  DMA_CPAR_PA = longword($FFFFFFFF);              //Peripheral Address

//*****************  Bit definition for DMA_CMAR register  *******************
  DMA_CMAR_MA = longword($FFFFFFFF);              //Memory Address

//****************************************************************************

//External Interrupt/Event Controller (EXTI)

//****************************************************************************
//******************  Bit definition for EXTI_IMR register  ******************
  EXTI_IMR_MR0 = longword($00000001);             //Interrupt Mask on line 0
  EXTI_IMR_MR1 = longword($00000002);             //Interrupt Mask on line 1
  EXTI_IMR_MR2 = longword($00000004);             //Interrupt Mask on line 2
  EXTI_IMR_MR3 = longword($00000008);             //Interrupt Mask on line 3
  EXTI_IMR_MR4 = longword($00000010);             //Interrupt Mask on line 4
  EXTI_IMR_MR5 = longword($00000020);             //Interrupt Mask on line 5
  EXTI_IMR_MR6 = longword($00000040);             //Interrupt Mask on line 6
  EXTI_IMR_MR7 = longword($00000080);             //Interrupt Mask on line 7
  EXTI_IMR_MR8 = longword($00000100);             //Interrupt Mask on line 8
  EXTI_IMR_MR9 = longword($00000200);             //Interrupt Mask on line 9
  EXTI_IMR_MR10 = longword($00000400);            //Interrupt Mask on line 10
  EXTI_IMR_MR11 = longword($00000800);            //Interrupt Mask on line 11
  EXTI_IMR_MR12 = longword($00001000);            //Interrupt Mask on line 12
  EXTI_IMR_MR13 = longword($00002000);            //Interrupt Mask on line 13
  EXTI_IMR_MR14 = longword($00004000);            //Interrupt Mask on line 14
  EXTI_IMR_MR15 = longword($00008000);            //Interrupt Mask on line 15
  EXTI_IMR_MR16 = longword($00010000);            //Interrupt Mask on line 16
  EXTI_IMR_MR17 = longword($00020000);            //Interrupt Mask on line 17
  EXTI_IMR_MR19 = longword($00080000);            //Interrupt Mask on line 19
  EXTI_IMR_MR21 = longword($00200000);            //Interrupt Mask on line 21
  EXTI_IMR_MR22 = longword($00400000);            //Interrupt Mask on line 22
  EXTI_IMR_MR23 = longword($00800000);            //Interrupt Mask on line 23
  EXTI_IMR_MR25 = longword($02000000);            //Interrupt Mask on line 25
  EXTI_IMR_MR27 = longword($08000000);            //Interrupt Mask on line 27

//*****************  Bit definition for EXTI_EMR register  *******************
  EXTI_EMR_MR0 = longword($00000001);             //Event Mask on line 0
  EXTI_EMR_MR1 = longword($00000002);             //Event Mask on line 1
  EXTI_EMR_MR2 = longword($00000004);             //Event Mask on line 2
  EXTI_EMR_MR3 = longword($00000008);             //Event Mask on line 3
  EXTI_EMR_MR4 = longword($00000010);             //Event Mask on line 4
  EXTI_EMR_MR5 = longword($00000020);             //Event Mask on line 5
  EXTI_EMR_MR6 = longword($00000040);             //Event Mask on line 6
  EXTI_EMR_MR7 = longword($00000080);             //Event Mask on line 7
  EXTI_EMR_MR8 = longword($00000100);             //Event Mask on line 8
  EXTI_EMR_MR9 = longword($00000200);             //Event Mask on line 9
  EXTI_EMR_MR10 = longword($00000400);            //Event Mask on line 10
  EXTI_EMR_MR11 = longword($00000800);            //Event Mask on line 11
  EXTI_EMR_MR12 = longword($00001000);            //Event Mask on line 12
  EXTI_EMR_MR13 = longword($00002000);            //Event Mask on line 13
  EXTI_EMR_MR14 = longword($00004000);            //Event Mask on line 14
  EXTI_EMR_MR15 = longword($00008000);            //Event Mask on line 15
  EXTI_EMR_MR16 = longword($00010000);            //Event Mask on line 16
  EXTI_EMR_MR17 = longword($00020000);            //Event Mask on line 17
  EXTI_EMR_MR19 = longword($00080000);            //Event Mask on line 19
  EXTI_EMR_MR21 = longword($00200000);            //Event Mask on line 21
  EXTI_EMR_MR22 = longword($00400000);            //Event Mask on line 22
  EXTI_EMR_MR23 = longword($00800000);            //Event Mask on line 23
  EXTI_EMR_MR25 = longword($02000000);            //Event Mask on line 25
  EXTI_EMR_MR27 = longword($08000000);            //Event Mask on line 27

//******************  Bit definition for EXTI_RTSR register  *****************
  EXTI_RTSR_TR0 = longword($00000001);            //Rising trigger event configuration bit of line 0
  EXTI_RTSR_TR1 = longword($00000002);            //Rising trigger event configuration bit of line 1
  EXTI_RTSR_TR2 = longword($00000004);            //Rising trigger event configuration bit of line 2
  EXTI_RTSR_TR3 = longword($00000008);            //Rising trigger event configuration bit of line 3
  EXTI_RTSR_TR4 = longword($00000010);            //Rising trigger event configuration bit of line 4
  EXTI_RTSR_TR5 = longword($00000020);            //Rising trigger event configuration bit of line 5
  EXTI_RTSR_TR6 = longword($00000040);            //Rising trigger event configuration bit of line 6
  EXTI_RTSR_TR7 = longword($00000080);            //Rising trigger event configuration bit of line 7
  EXTI_RTSR_TR8 = longword($00000100);            //Rising trigger event configuration bit of line 8
  EXTI_RTSR_TR9 = longword($00000200);            //Rising trigger event configuration bit of line 9
  EXTI_RTSR_TR10 = longword($00000400);           //Rising trigger event configuration bit of line 10
  EXTI_RTSR_TR11 = longword($00000800);           //Rising trigger event configuration bit of line 11
  EXTI_RTSR_TR12 = longword($00001000);           //Rising trigger event configuration bit of line 12
  EXTI_RTSR_TR13 = longword($00002000);           //Rising trigger event configuration bit of line 13
  EXTI_RTSR_TR14 = longword($00004000);           //Rising trigger event configuration bit of line 14
  EXTI_RTSR_TR15 = longword($00008000);           //Rising trigger event configuration bit of line 15
  EXTI_RTSR_TR16 = longword($00010000);           //Rising trigger event configuration bit of line 16
  EXTI_RTSR_TR17 = longword($00020000);           //Rising trigger event configuration bit of line 17
  EXTI_RTSR_TR19 = longword($00080000);           //Rising trigger event configuration bit of line 19

//******************  Bit definition for EXTI_FTSR register ******************
  EXTI_FTSR_TR0 = longword($00000001);            //Falling trigger event configuration bit of line 0
  EXTI_FTSR_TR1 = longword($00000002);            //Falling trigger event configuration bit of line 1
  EXTI_FTSR_TR2 = longword($00000004);            //Falling trigger event configuration bit of line 2
  EXTI_FTSR_TR3 = longword($00000008);            //Falling trigger event configuration bit of line 3
  EXTI_FTSR_TR4 = longword($00000010);            //Falling trigger event configuration bit of line 4
  EXTI_FTSR_TR5 = longword($00000020);            //Falling trigger event configuration bit of line 5
  EXTI_FTSR_TR6 = longword($00000040);            //Falling trigger event configuration bit of line 6
  EXTI_FTSR_TR7 = longword($00000080);            //Falling trigger event configuration bit of line 7
  EXTI_FTSR_TR8 = longword($00000100);            //Falling trigger event configuration bit of line 8
  EXTI_FTSR_TR9 = longword($00000200);            //Falling trigger event configuration bit of line 9
  EXTI_FTSR_TR10 = longword($00000400);           //Falling trigger event configuration bit of line 10
  EXTI_FTSR_TR11 = longword($00000800);           //Falling trigger event configuration bit of line 11
  EXTI_FTSR_TR12 = longword($00001000);           //Falling trigger event configuration bit of line 12
  EXTI_FTSR_TR13 = longword($00002000);           //Falling trigger event configuration bit of line 13
  EXTI_FTSR_TR14 = longword($00004000);           //Falling trigger event configuration bit of line 14
  EXTI_FTSR_TR15 = longword($00008000);           //Falling trigger event configuration bit of line 15
  EXTI_FTSR_TR16 = longword($00010000);           //Falling trigger event configuration bit of line 16
  EXTI_FTSR_TR17 = longword($00020000);           //Falling trigger event configuration bit of line 17
  EXTI_FTSR_TR19 = longword($00080000);           //Falling trigger event configuration bit of line 19

//****************** Bit definition for EXTI_SWIER register ******************
  EXTI_SWIER_SWIER0 = longword($00000001);        //Software Interrupt on line 0
  EXTI_SWIER_SWIER1 = longword($00000002);        //Software Interrupt on line 1
  EXTI_SWIER_SWIER2 = longword($00000004);        //Software Interrupt on line 2
  EXTI_SWIER_SWIER3 = longword($00000008);        //Software Interrupt on line 3
  EXTI_SWIER_SWIER4 = longword($00000010);        //Software Interrupt on line 4
  EXTI_SWIER_SWIER5 = longword($00000020);        //Software Interrupt on line 5
  EXTI_SWIER_SWIER6 = longword($00000040);        //Software Interrupt on line 6
  EXTI_SWIER_SWIER7 = longword($00000080);        //Software Interrupt on line 7
  EXTI_SWIER_SWIER8 = longword($00000100);        //Software Interrupt on line 8
  EXTI_SWIER_SWIER9 = longword($00000200);        //Software Interrupt on line 9
  EXTI_SWIER_SWIER10 = longword($00000400);       //Software Interrupt on line 10
  EXTI_SWIER_SWIER11 = longword($00000800);       //Software Interrupt on line 11
  EXTI_SWIER_SWIER12 = longword($00001000);       //Software Interrupt on line 12
  EXTI_SWIER_SWIER13 = longword($00002000);       //Software Interrupt on line 13
  EXTI_SWIER_SWIER14 = longword($00004000);       //Software Interrupt on line 14
  EXTI_SWIER_SWIER15 = longword($00008000);       //Software Interrupt on line 15
  EXTI_SWIER_SWIER16 = longword($00010000);       //Software Interrupt on line 16
  EXTI_SWIER_SWIER17 = longword($00020000);       //Software Interrupt on line 17
  EXTI_SWIER_SWIER19 = longword($00080000);       //Software Interrupt on line 19

//*****************  Bit definition for EXTI_PR register  ********************
  EXTI_PR_PR0 = longword($00000001);              //Pending bit 0
  EXTI_PR_PR1 = longword($00000002);              //Pending bit 1
  EXTI_PR_PR2 = longword($00000004);              //Pending bit 2
  EXTI_PR_PR3 = longword($00000008);              //Pending bit 3
  EXTI_PR_PR4 = longword($00000010);              //Pending bit 4
  EXTI_PR_PR5 = longword($00000020);              //Pending bit 5
  EXTI_PR_PR6 = longword($00000040);              //Pending bit 6
  EXTI_PR_PR7 = longword($00000080);              //Pending bit 7
  EXTI_PR_PR8 = longword($00000100);              //Pending bit 8
  EXTI_PR_PR9 = longword($00000200);              //Pending bit 9
  EXTI_PR_PR10 = longword($00000400);             //Pending bit 10
  EXTI_PR_PR11 = longword($00000800);             //Pending bit 11
  EXTI_PR_PR12 = longword($00001000);             //Pending bit 12
  EXTI_PR_PR13 = longword($00002000);             //Pending bit 13
  EXTI_PR_PR14 = longword($00004000);             //Pending bit 14
  EXTI_PR_PR15 = longword($00008000);             //Pending bit 15
  EXTI_PR_PR16 = longword($00010000);             //Pending bit 16
  EXTI_PR_PR17 = longword($00020000);             //Pending bit 17
  EXTI_PR_PR19 = longword($00080000);             //Pending bit 19

//****************************************************************************

//FLASH and Option Bytes Registers

//****************************************************************************

//******************  Bit definition for FLASH_ACR register  *****************
  FLASH_ACR_LATENCY = longword($00000001);        //LATENCY bit (Latency)

  FLASH_ACR_PRFTBE = longword($00000010);         //Prefetch Buffer Enable
  FLASH_ACR_PRFTBS = longword($00000020);         //Prefetch Buffer Status

//*****************  Bit definition for FLASH_KEYR register  *****************
  FLASH_KEYR_FKEYR = longword($FFFFFFFF);         //FPEC Key

//****************  Bit definition for FLASH_OPTKEYR register  ***************
  FLASH_OPTKEYR_OPTKEYR = longword($FFFFFFFF);    //Option Byte Key

//*****************  FLASH Keys  *********************************************
  FLASH_FKEY1 = longword($45670123);              //Flash program erase key1
  FLASH_FKEY2 = longword($CDEF89AB);              //Flash program erase key2: used with FLASH_PEKEY1
//to unlock the write access to the FPEC.

  FLASH_OPTKEY1 = longword($45670123);            //Flash option key1
  FLASH_OPTKEY2 = longword($CDEF89AB);            //Flash option key2: used with FLASH_OPTKEY1 to
//unlock the write access to the option byte block

//*****************  Bit definition for FLASH_SR register  ******************
  FLASH_SR_BSY = longword($00000001);             //Busy
  FLASH_SR_PGERR = longword($00000004);           //Programming Error
  FLASH_SR_WRPERR = longword($00000010);          //Write Protection Error
  FLASH_SR_EOP = longword($00000020);             //End of operation

//******************  Bit definition for FLASH_CR register  ******************
  FLASH_CR_PG = longword($00000001);              //Programming
  FLASH_CR_PER = longword($00000002);             //Page Erase
  FLASH_CR_MER = longword($00000004);             //Mass Erase
  FLASH_CR_OPTPG = longword($00000010);           //Option Byte Programming
  FLASH_CR_OPTER = longword($00000020);           //Option Byte Erase
  FLASH_CR_STRT = longword($00000040);            //Start
  FLASH_CR_LOCK = longword($00000080);            //Lock
  FLASH_CR_OPTWRE = longword($00000200);          //Option Bytes Write Enable
  FLASH_CR_ERRIE = longword($00000400);           //Error Interrupt Enable
  FLASH_CR_EOPIE = longword($00001000);           //End of operation interrupt enable
  FLASH_CR_OBL_LAUNCH = longword($00002000);      //Option Bytes Loader Launch

//******************  Bit definition for FLASH_AR register  ******************
  FLASH_AR_FAR = longword($FFFFFFFF);             //Flash Address

//*****************  Bit definition for FLASH_OBR register  ******************
  FLASH_OBR_OPTERR = longword($00000001);         //Option Byte Error
  FLASH_OBR_RDPRT1 = longword($00000002);         //Read protection Level 1
  FLASH_OBR_RDPRT2 = longword($00000004);         //Read protection Level 2

  FLASH_OBR_USER = longword($00003700);           //User Option Bytes
  FLASH_OBR_IWDG_SW = longword($00000100);        //IWDG SW
  FLASH_OBR_nRST_STOP = longword($00000200);      //nRST_STOP
  FLASH_OBR_nRST_STDBY = longword($00000400);     //nRST_STDBY
  FLASH_OBR_nBOOT1 = longword($00001000);         //nBOOT1
  FLASH_OBR_VDDA_MONITOR = longword($00002000);   //VDDA power supply supervisor

//Old BOOT1 bit definition, maintained for legacy purpose
  FLASH_OBR_BOOT1 = FLASH_OBR_nBOOT1;

//Old BOOT1 bit definition, maintained for legacy purpose
  FLASH_OBR_VDDA_ANALOG = FLASH_OBR_VDDA_MONITOR;

//*****************  Bit definition for FLASH_WRPR register  *****************
  FLASH_WRPR_WRP = longword($0000FFFF);           //Write Protect

//----------------------------------------------------------------------------

//*****************  Bit definition for OB_RDP register  *********************
  OB_RDP_RDP = longword($000000FF);               //Read protection option byte
  OB_RDP_nRDP = longword($0000FF00);              //Read protection complemented option byte

//*****************  Bit definition for OB_USER register  ********************
  OB_USER_USER = longword($00FF0000);             //User option byte
  OB_USER_nUSER = longword($FF000000);            //User complemented option byte

//*****************  Bit definition for OB_WRP0 register  ********************
  OB_WRP0_WRP0 = longword($000000FF);             //Flash memory write protection option bytes
  OB_WRP0_nWRP0 = longword($0000FF00);            //Flash memory write protection complemented option bytes

//*****************  Bit definition for OB_WRP1 register  ********************
  OB_WRP1_WRP1 = longword($00FF0000);             //Flash memory write protection option bytes
  OB_WRP1_nWRP1 = longword($FF000000);            //Flash memory write protection complemented option bytes

//****************************************************************************

//General Purpose IOs (GPIO)

//****************************************************************************
//******************  Bit definition for GPIO_MODER register  ****************
  GPIO_MODER_MODER0 = longword($00000003);
  GPIO_MODER_MODER0_0 = longword($00000001);
  GPIO_MODER_MODER0_1 = longword($00000002);
  GPIO_MODER_MODER1 = longword($0000000C);
  GPIO_MODER_MODER1_0 = longword($00000004);
  GPIO_MODER_MODER1_1 = longword($00000008);
  GPIO_MODER_MODER2 = longword($00000030);
  GPIO_MODER_MODER2_0 = longword($00000010);
  GPIO_MODER_MODER2_1 = longword($00000020);
  GPIO_MODER_MODER3 = longword($000000C0);
  GPIO_MODER_MODER3_0 = longword($00000040);
  GPIO_MODER_MODER3_1 = longword($00000080);
  GPIO_MODER_MODER4 = longword($00000300);
  GPIO_MODER_MODER4_0 = longword($00000100);
  GPIO_MODER_MODER4_1 = longword($00000200);
  GPIO_MODER_MODER5 = longword($00000C00);
  GPIO_MODER_MODER5_0 = longword($00000400);
  GPIO_MODER_MODER5_1 = longword($00000800);
  GPIO_MODER_MODER6 = longword($00003000);
  GPIO_MODER_MODER6_0 = longword($00001000);
  GPIO_MODER_MODER6_1 = longword($00002000);
  GPIO_MODER_MODER7 = longword($0000C000);
  GPIO_MODER_MODER7_0 = longword($00004000);
  GPIO_MODER_MODER7_1 = longword($00008000);
  GPIO_MODER_MODER8 = longword($00030000);
  GPIO_MODER_MODER8_0 = longword($00010000);
  GPIO_MODER_MODER8_1 = longword($00020000);
  GPIO_MODER_MODER9 = longword($000C0000);
  GPIO_MODER_MODER9_0 = longword($00040000);
  GPIO_MODER_MODER9_1 = longword($00080000);
  GPIO_MODER_MODER10 = longword($00300000);
  GPIO_MODER_MODER10_0 = longword($00100000);
  GPIO_MODER_MODER10_1 = longword($00200000);
  GPIO_MODER_MODER11 = longword($00C00000);
  GPIO_MODER_MODER11_0 = longword($00400000);
  GPIO_MODER_MODER11_1 = longword($00800000);
  GPIO_MODER_MODER12 = longword($03000000);
  GPIO_MODER_MODER12_0 = longword($01000000);
  GPIO_MODER_MODER12_1 = longword($02000000);
  GPIO_MODER_MODER13 = longword($0C000000);
  GPIO_MODER_MODER13_0 = longword($04000000);
  GPIO_MODER_MODER13_1 = longword($08000000);
  GPIO_MODER_MODER14 = longword($30000000);
  GPIO_MODER_MODER14_0 = longword($10000000);
  GPIO_MODER_MODER14_1 = longword($20000000);
  GPIO_MODER_MODER15 = longword($C0000000);
  GPIO_MODER_MODER15_0 = longword($40000000);
  GPIO_MODER_MODER15_1 = longword($80000000);

//*****************  Bit definition for GPIO_OTYPER register  ****************
  GPIO_OTYPER_OT_0 = longword($00000001);
  GPIO_OTYPER_OT_1 = longword($00000002);
  GPIO_OTYPER_OT_2 = longword($00000004);
  GPIO_OTYPER_OT_3 = longword($00000008);
  GPIO_OTYPER_OT_4 = longword($00000010);
  GPIO_OTYPER_OT_5 = longword($00000020);
  GPIO_OTYPER_OT_6 = longword($00000040);
  GPIO_OTYPER_OT_7 = longword($00000080);
  GPIO_OTYPER_OT_8 = longword($00000100);
  GPIO_OTYPER_OT_9 = longword($00000200);
  GPIO_OTYPER_OT_10 = longword($00000400);
  GPIO_OTYPER_OT_11 = longword($00000800);
  GPIO_OTYPER_OT_12 = longword($00001000);
  GPIO_OTYPER_OT_13 = longword($00002000);
  GPIO_OTYPER_OT_14 = longword($00004000);
  GPIO_OTYPER_OT_15 = longword($00008000);

//***************  Bit definition for GPIO_OSPEEDR register  *****************
  GPIO_OSPEEDER_OSPEEDR0 = longword($00000003);
  GPIO_OSPEEDER_OSPEEDR0_0 = longword($00000001);
  GPIO_OSPEEDER_OSPEEDR0_1 = longword($00000002);
  GPIO_OSPEEDER_OSPEEDR1 = longword($0000000C);
  GPIO_OSPEEDER_OSPEEDR1_0 = longword($00000004);
  GPIO_OSPEEDER_OSPEEDR1_1 = longword($00000008);
  GPIO_OSPEEDER_OSPEEDR2 = longword($00000030);
  GPIO_OSPEEDER_OSPEEDR2_0 = longword($00000010);
  GPIO_OSPEEDER_OSPEEDR2_1 = longword($00000020);
  GPIO_OSPEEDER_OSPEEDR3 = longword($000000C0);
  GPIO_OSPEEDER_OSPEEDR3_0 = longword($00000040);
  GPIO_OSPEEDER_OSPEEDR3_1 = longword($00000080);
  GPIO_OSPEEDER_OSPEEDR4 = longword($00000300);
  GPIO_OSPEEDER_OSPEEDR4_0 = longword($00000100);
  GPIO_OSPEEDER_OSPEEDR4_1 = longword($00000200);
  GPIO_OSPEEDER_OSPEEDR5 = longword($00000C00);
  GPIO_OSPEEDER_OSPEEDR5_0 = longword($00000400);
  GPIO_OSPEEDER_OSPEEDR5_1 = longword($00000800);
  GPIO_OSPEEDER_OSPEEDR6 = longword($00003000);
  GPIO_OSPEEDER_OSPEEDR6_0 = longword($00001000);
  GPIO_OSPEEDER_OSPEEDR6_1 = longword($00002000);
  GPIO_OSPEEDER_OSPEEDR7 = longword($0000C000);
  GPIO_OSPEEDER_OSPEEDR7_0 = longword($00004000);
  GPIO_OSPEEDER_OSPEEDR7_1 = longword($00008000);
  GPIO_OSPEEDER_OSPEEDR8 = longword($00030000);
  GPIO_OSPEEDER_OSPEEDR8_0 = longword($00010000);
  GPIO_OSPEEDER_OSPEEDR8_1 = longword($00020000);
  GPIO_OSPEEDER_OSPEEDR9 = longword($000C0000);
  GPIO_OSPEEDER_OSPEEDR9_0 = longword($00040000);
  GPIO_OSPEEDER_OSPEEDR9_1 = longword($00080000);
  GPIO_OSPEEDER_OSPEEDR10 = longword($00300000);
  GPIO_OSPEEDER_OSPEEDR10_0 = longword($00100000);
  GPIO_OSPEEDER_OSPEEDR10_1 = longword($00200000);
  GPIO_OSPEEDER_OSPEEDR11 = longword($00C00000);
  GPIO_OSPEEDER_OSPEEDR11_0 = longword($00400000);
  GPIO_OSPEEDER_OSPEEDR11_1 = longword($00800000);
  GPIO_OSPEEDER_OSPEEDR12 = longword($03000000);
  GPIO_OSPEEDER_OSPEEDR12_0 = longword($01000000);
  GPIO_OSPEEDER_OSPEEDR12_1 = longword($02000000);
  GPIO_OSPEEDER_OSPEEDR13 = longword($0C000000);
  GPIO_OSPEEDER_OSPEEDR13_0 = longword($04000000);
  GPIO_OSPEEDER_OSPEEDR13_1 = longword($08000000);
  GPIO_OSPEEDER_OSPEEDR14 = longword($30000000);
  GPIO_OSPEEDER_OSPEEDR14_0 = longword($10000000);
  GPIO_OSPEEDER_OSPEEDR14_1 = longword($20000000);
  GPIO_OSPEEDER_OSPEEDR15 = longword($C0000000);
  GPIO_OSPEEDER_OSPEEDR15_0 = longword($40000000);
  GPIO_OSPEEDER_OSPEEDR15_1 = longword($80000000);

//******************  Bit definition for GPIO_PUPDR register *****************
  GPIO_PUPDR_PUPDR0 = longword($00000003);
  GPIO_PUPDR_PUPDR0_0 = longword($00000001);
  GPIO_PUPDR_PUPDR0_1 = longword($00000002);
  GPIO_PUPDR_PUPDR1 = longword($0000000C);
  GPIO_PUPDR_PUPDR1_0 = longword($00000004);
  GPIO_PUPDR_PUPDR1_1 = longword($00000008);
  GPIO_PUPDR_PUPDR2 = longword($00000030);
  GPIO_PUPDR_PUPDR2_0 = longword($00000010);
  GPIO_PUPDR_PUPDR2_1 = longword($00000020);
  GPIO_PUPDR_PUPDR3 = longword($000000C0);
  GPIO_PUPDR_PUPDR3_0 = longword($00000040);
  GPIO_PUPDR_PUPDR3_1 = longword($00000080);
  GPIO_PUPDR_PUPDR4 = longword($00000300);
  GPIO_PUPDR_PUPDR4_0 = longword($00000100);
  GPIO_PUPDR_PUPDR4_1 = longword($00000200);
  GPIO_PUPDR_PUPDR5 = longword($00000C00);
  GPIO_PUPDR_PUPDR5_0 = longword($00000400);
  GPIO_PUPDR_PUPDR5_1 = longword($00000800);
  GPIO_PUPDR_PUPDR6 = longword($00003000);
  GPIO_PUPDR_PUPDR6_0 = longword($00001000);
  GPIO_PUPDR_PUPDR6_1 = longword($00002000);
  GPIO_PUPDR_PUPDR7 = longword($0000C000);
  GPIO_PUPDR_PUPDR7_0 = longword($00004000);
  GPIO_PUPDR_PUPDR7_1 = longword($00008000);
  GPIO_PUPDR_PUPDR8 = longword($00030000);
  GPIO_PUPDR_PUPDR8_0 = longword($00010000);
  GPIO_PUPDR_PUPDR8_1 = longword($00020000);
  GPIO_PUPDR_PUPDR9 = longword($000C0000);
  GPIO_PUPDR_PUPDR9_0 = longword($00040000);
  GPIO_PUPDR_PUPDR9_1 = longword($00080000);
  GPIO_PUPDR_PUPDR10 = longword($00300000);
  GPIO_PUPDR_PUPDR10_0 = longword($00100000);
  GPIO_PUPDR_PUPDR10_1 = longword($00200000);
  GPIO_PUPDR_PUPDR11 = longword($00C00000);
  GPIO_PUPDR_PUPDR11_0 = longword($00400000);
  GPIO_PUPDR_PUPDR11_1 = longword($00800000);
  GPIO_PUPDR_PUPDR12 = longword($03000000);
  GPIO_PUPDR_PUPDR12_0 = longword($01000000);
  GPIO_PUPDR_PUPDR12_1 = longword($02000000);
  GPIO_PUPDR_PUPDR13 = longword($0C000000);
  GPIO_PUPDR_PUPDR13_0 = longword($04000000);
  GPIO_PUPDR_PUPDR13_1 = longword($08000000);
  GPIO_PUPDR_PUPDR14 = longword($30000000);
  GPIO_PUPDR_PUPDR14_0 = longword($10000000);
  GPIO_PUPDR_PUPDR14_1 = longword($20000000);
  GPIO_PUPDR_PUPDR15 = longword($C0000000);
  GPIO_PUPDR_PUPDR15_0 = longword($40000000);
  GPIO_PUPDR_PUPDR15_1 = longword($80000000);

//******************  Bit definition for GPIO_IDR register  ******************
  GPIO_IDR_0 = longword($00000001);
  GPIO_IDR_1 = longword($00000002);
  GPIO_IDR_2 = longword($00000004);
  GPIO_IDR_3 = longword($00000008);
  GPIO_IDR_4 = longword($00000010);
  GPIO_IDR_5 = longword($00000020);
  GPIO_IDR_6 = longword($00000040);
  GPIO_IDR_7 = longword($00000080);
  GPIO_IDR_8 = longword($00000100);
  GPIO_IDR_9 = longword($00000200);
  GPIO_IDR_10 = longword($00000400);
  GPIO_IDR_11 = longword($00000800);
  GPIO_IDR_12 = longword($00001000);
  GPIO_IDR_13 = longword($00002000);
  GPIO_IDR_14 = longword($00004000);
  GPIO_IDR_15 = longword($00008000);

//*****************  Bit definition for GPIO_ODR register  *******************
  GPIO_ODR_0 = longword($00000001);
  GPIO_ODR_1 = longword($00000002);
  GPIO_ODR_2 = longword($00000004);
  GPIO_ODR_3 = longword($00000008);
  GPIO_ODR_4 = longword($00000010);
  GPIO_ODR_5 = longword($00000020);
  GPIO_ODR_6 = longword($00000040);
  GPIO_ODR_7 = longword($00000080);
  GPIO_ODR_8 = longword($00000100);
  GPIO_ODR_9 = longword($00000200);
  GPIO_ODR_10 = longword($00000400);
  GPIO_ODR_11 = longword($00000800);
  GPIO_ODR_12 = longword($00001000);
  GPIO_ODR_13 = longword($00002000);
  GPIO_ODR_14 = longword($00004000);
  GPIO_ODR_15 = longword($00008000);

//***************** Bit definition for GPIO_BSRR register  *******************
  GPIO_BSRR_BS_0 = longword($00000001);
  GPIO_BSRR_BS_1 = longword($00000002);
  GPIO_BSRR_BS_2 = longword($00000004);
  GPIO_BSRR_BS_3 = longword($00000008);
  GPIO_BSRR_BS_4 = longword($00000010);
  GPIO_BSRR_BS_5 = longword($00000020);
  GPIO_BSRR_BS_6 = longword($00000040);
  GPIO_BSRR_BS_7 = longword($00000080);
  GPIO_BSRR_BS_8 = longword($00000100);
  GPIO_BSRR_BS_9 = longword($00000200);
  GPIO_BSRR_BS_10 = longword($00000400);
  GPIO_BSRR_BS_11 = longword($00000800);
  GPIO_BSRR_BS_12 = longword($00001000);
  GPIO_BSRR_BS_13 = longword($00002000);
  GPIO_BSRR_BS_14 = longword($00004000);
  GPIO_BSRR_BS_15 = longword($00008000);
  GPIO_BSRR_BR_0 = longword($00010000);
  GPIO_BSRR_BR_1 = longword($00020000);
  GPIO_BSRR_BR_2 = longword($00040000);
  GPIO_BSRR_BR_3 = longword($00080000);
  GPIO_BSRR_BR_4 = longword($00100000);
  GPIO_BSRR_BR_5 = longword($00200000);
  GPIO_BSRR_BR_6 = longword($00400000);
  GPIO_BSRR_BR_7 = longword($00800000);
  GPIO_BSRR_BR_8 = longword($01000000);
  GPIO_BSRR_BR_9 = longword($02000000);
  GPIO_BSRR_BR_10 = longword($04000000);
  GPIO_BSRR_BR_11 = longword($08000000);
  GPIO_BSRR_BR_12 = longword($10000000);
  GPIO_BSRR_BR_13 = longword($20000000);
  GPIO_BSRR_BR_14 = longword($40000000);
  GPIO_BSRR_BR_15 = longword($80000000);

//***************** Bit definition for GPIO_LCKR register  *******************
  GPIO_LCKR_LCK0 = longword($00000001);
  GPIO_LCKR_LCK1 = longword($00000002);
  GPIO_LCKR_LCK2 = longword($00000004);
  GPIO_LCKR_LCK3 = longword($00000008);
  GPIO_LCKR_LCK4 = longword($00000010);
  GPIO_LCKR_LCK5 = longword($00000020);
  GPIO_LCKR_LCK6 = longword($00000040);
  GPIO_LCKR_LCK7 = longword($00000080);
  GPIO_LCKR_LCK8 = longword($00000100);
  GPIO_LCKR_LCK9 = longword($00000200);
  GPIO_LCKR_LCK10 = longword($00000400);
  GPIO_LCKR_LCK11 = longword($00000800);
  GPIO_LCKR_LCK12 = longword($00001000);
  GPIO_LCKR_LCK13 = longword($00002000);
  GPIO_LCKR_LCK14 = longword($00004000);
  GPIO_LCKR_LCK15 = longword($00008000);
  GPIO_LCKR_LCKK = longword($00010000);

//***************** Bit definition for GPIO_AFRL register  *******************
  GPIO_AFRL_AFRL0 = longword($0000000F);
  GPIO_AFRL_AFRL1 = longword($000000F0);
  GPIO_AFRL_AFRL2 = longword($00000F00);
  GPIO_AFRL_AFRL3 = longword($0000F000);
  GPIO_AFRL_AFRL4 = longword($000F0000);
  GPIO_AFRL_AFRL5 = longword($00F00000);
  GPIO_AFRL_AFRL6 = longword($0F000000);
  GPIO_AFRL_AFRL7 = longword($F0000000);

//***************** Bit definition for GPIO_AFRH register  *******************
  GPIO_AFRH_AFRH0 = longword($0000000F);
  GPIO_AFRH_AFRH1 = longword($000000F0);
  GPIO_AFRH_AFRH2 = longword($00000F00);
  GPIO_AFRH_AFRH3 = longword($0000F000);
  GPIO_AFRH_AFRH4 = longword($000F0000);
  GPIO_AFRH_AFRH5 = longword($00F00000);
  GPIO_AFRH_AFRH6 = longword($0F000000);
  GPIO_AFRH_AFRH7 = longword($F0000000);

//***************** Bit definition for GPIO_BRR register  ********************
  GPIO_BRR_BR_0 = longword($00000001);
  GPIO_BRR_BR_1 = longword($00000002);
  GPIO_BRR_BR_2 = longword($00000004);
  GPIO_BRR_BR_3 = longword($00000008);
  GPIO_BRR_BR_4 = longword($00000010);
  GPIO_BRR_BR_5 = longword($00000020);
  GPIO_BRR_BR_6 = longword($00000040);
  GPIO_BRR_BR_7 = longword($00000080);
  GPIO_BRR_BR_8 = longword($00000100);
  GPIO_BRR_BR_9 = longword($00000200);
  GPIO_BRR_BR_10 = longword($00000400);
  GPIO_BRR_BR_11 = longword($00000800);
  GPIO_BRR_BR_12 = longword($00001000);
  GPIO_BRR_BR_13 = longword($00002000);
  GPIO_BRR_BR_14 = longword($00004000);
  GPIO_BRR_BR_15 = longword($00008000);

//****************************************************************************

//Inter-integrated Circuit Interface (I2C)

//****************************************************************************

//******************  Bit definition for I2C_CR1 register  ******************
  I2C_CR1_PE = longword($00000001);               //Peripheral enable
  I2C_CR1_TXIE = longword($00000002);             //TX interrupt enable
  I2C_CR1_RXIE = longword($00000004);             //RX interrupt enable
  I2C_CR1_ADDRIE = longword($00000008);           //Address match interrupt enable
  I2C_CR1_NACKIE = longword($00000010);           //NACK received interrupt enable
  I2C_CR1_STOPIE = longword($00000020);           //STOP detection interrupt enable
  I2C_CR1_TCIE = longword($00000040);             //Transfer complete interrupt enable
  I2C_CR1_ERRIE = longword($00000080);            //Errors interrupt enable
  I2C_CR1_DFN = longword($00000F00);              //Digital noise filter
  I2C_CR1_ANFOFF = longword($00001000);           //Analog noise filter OFF
  I2C_CR1_SWRST = longword($00002000);            //Software reset
  I2C_CR1_TXDMAEN = longword($00004000);          //DMA transmission requests enable
  I2C_CR1_RXDMAEN = longword($00008000);          //DMA reception requests enable
  I2C_CR1_SBC = longword($00010000);              //Slave byte control
  I2C_CR1_NOSTRETCH = longword($00020000);        //Clock stretching disable
  I2C_CR1_WUPEN = longword($00040000);            //Wakeup from STOP enable
  I2C_CR1_GCEN = longword($00080000);             //General call enable
  I2C_CR1_SMBHEN = longword($00100000);           //SMBus host address enable
  I2C_CR1_SMBDEN = longword($00200000);           //SMBus device default address enable
  I2C_CR1_ALERTEN = longword($00400000);          //SMBus alert enable
  I2C_CR1_PECEN = longword($00800000);            //PEC enable

//*****************  Bit definition for I2C_CR2 register  *******************
  I2C_CR2_SADD = longword($000003FF);             //Slave address (master mode)
  I2C_CR2_RD_WRN = longword($00000400);           //Transfer direction (master mode)
  I2C_CR2_ADD10 = longword($00000800);            //10-bit addressing mode (master mode)
  I2C_CR2_HEAD10R = longword($00001000);          //10-bit address header only read direction (master mode)
  I2C_CR2_START = longword($00002000);            //START generation
  I2C_CR2_STOP = longword($00004000);             //STOP generation (master mode)
  I2C_CR2_NACK = longword($00008000);             //NACK generation (slave mode)
  I2C_CR2_NBYTES = longword($00FF0000);           //Number of bytes
  I2C_CR2_RELOAD = longword($01000000);           //NBYTES reload mode
  I2C_CR2_AUTOEND = longword($02000000);          //Automatic end mode (master mode)
  I2C_CR2_PECBYTE = longword($04000000);          //Packet error checking byte

//******************  Bit definition for I2C_OAR1 register  *****************
  I2C_OAR1_OA1 = longword($000003FF);             //Interface own address 1
  I2C_OAR1_OA1MODE = longword($00000400);         //Own address 1 10-bit mode
  I2C_OAR1_OA1EN = longword($00008000);           //Own address 1 enable

//******************  Bit definition for I2C_OAR2 register  *****************
  I2C_OAR2_OA2 = longword($000000FE);             //Interface own address 2
  I2C_OAR2_OA2MSK = longword($00000700);          //Own address 2 masks
  I2C_OAR2_OA2EN = longword($00008000);           //Own address 2 enable

//******************  Bit definition for I2C_TIMINGR register ******************
  I2C_TIMINGR_SCLL = longword($000000FF);         //SCL low period (master mode)
  I2C_TIMINGR_SCLH = longword($0000FF00);         //SCL high period (master mode)
  I2C_TIMINGR_SDADEL = longword($000F0000);       //Data hold time
  I2C_TIMINGR_SCLDEL = longword($00F00000);       //Data setup time
  I2C_TIMINGR_PRESC = longword($F0000000);        //Timings prescaler

//****************** Bit definition for I2C_TIMEOUTR register ******************
  I2C_TIMEOUTR_TIMEOUTA = longword($00000FFF);    //Bus timeout A
  I2C_TIMEOUTR_TIDLE = longword($00001000);       //Idle clock timeout detection
  I2C_TIMEOUTR_TIMOUTEN = longword($00008000);    //Clock timeout enable
  I2C_TIMEOUTR_TIMEOUTB = longword($0FFF0000);    //Bus timeout B
  I2C_TIMEOUTR_TEXTEN = longword($80000000);      //Extended clock timeout enable

//*****************  Bit definition for I2C_ISR register  ********************
  I2C_ISR_TXE = longword($00000001);              //Transmit data register empty
  I2C_ISR_TXIS = longword($00000002);             //Transmit interrupt status
  I2C_ISR_RXNE = longword($00000004);             //Receive data register not empty
  I2C_ISR_ADDR = longword($00000008);             //Address matched (slave mode)
  I2C_ISR_NACKF = longword($00000010);            //NACK received flag
  I2C_ISR_STOPF = longword($00000020);            //STOP detection flag
  I2C_ISR_TC = longword($00000040);               //Transfer complete (master mode)
  I2C_ISR_TCR = longword($00000080);              //Transfer complete reload
  I2C_ISR_BERR = longword($00000100);             //Bus error
  I2C_ISR_ARLO = longword($00000200);             //Arbitration lost
  I2C_ISR_OVR = longword($00000400);              //Overrun/Underrun
  I2C_ISR_PECERR = longword($00000800);           //PEC error in reception
  I2C_ISR_TIMEOUT = longword($00001000);          //Timeout or Tlow detection flag
  I2C_ISR_ALERT = longword($00002000);            //SMBus alert
  I2C_ISR_BUSY = longword($00008000);             //Bus busy
  I2C_ISR_DIR = longword($00010000);              //Transfer direction (slave mode)
  I2C_ISR_ADDCODE = longword($00FE0000);          //Address match code (slave mode)

//*****************  Bit definition for I2C_ICR register  ********************
  I2C_ICR_ADDRCF = longword($00000008);           //Address matched clear flag
  I2C_ICR_NACKCF = longword($00000010);           //NACK clear flag
  I2C_ICR_STOPCF = longword($00000020);           //STOP detection clear flag
  I2C_ICR_BERRCF = longword($00000100);           //Bus error clear flag
  I2C_ICR_ARLOCF = longword($00000200);           //Arbitration lost clear flag
  I2C_ICR_OVRCF = longword($00000400);            //Overrun/Underrun clear flag
  I2C_ICR_PECCF = longword($00000800);            //PAC error clear flag
  I2C_ICR_TIMOUTCF = longword($00001000);         //Timeout clear flag
  I2C_ICR_ALERTCF = longword($00002000);          //Alert clear flag

//*****************  Bit definition for I2C_PECR register  ********************
  I2C_PECR_PEC = longword($000000FF);             //PEC register

//*****************  Bit definition for I2C_RXDR register  ********************
  I2C_RXDR_RXDATA = longword($000000FF);          //8-bit receive data

//*****************  Bit definition for I2C_TXDR register  ********************
  I2C_TXDR_TXDATA = longword($000000FF);          //8-bit transmit data

//****************************************************************************

//Independent WATCHDOG (IWDG)

//****************************************************************************
//******************  Bit definition for IWDG_KR register  *******************
  IWDG_KR_KEY = longword($FFFF);                  //Key value (write only, read 0000h)

//******************  Bit definition for IWDG_PR register  *******************
  IWDG_PR_PR = longword($07);                     //PR[2:0] (Prescaler divider)
  IWDG_PR_PR_0 = longword($01);                   //Bit 0
  IWDG_PR_PR_1 = longword($02);                   //Bit 1
  IWDG_PR_PR_2 = longword($04);                   //Bit 2

//******************  Bit definition for IWDG_RLR register  ******************
  IWDG_RLR_RL = longword($0FFF);                  //Watchdog counter reload value

//******************  Bit definition for IWDG_SR register  *******************
  IWDG_SR_PVU = longword($01);                    //Watchdog prescaler value update
  IWDG_SR_RVU = longword($02);                    //Watchdog counter reload value update
  IWDG_SR_WVU = longword($04);                    //Watchdog counter window value update

//******************  Bit definition for IWDG_KR register  *******************
  IWDG_WINR_WIN = longword($0FFF);                //Watchdog counter window value

//****************************************************************************

//Power Control (PWR)

//****************************************************************************

//*******************  Bit definition for PWR_CR register  *******************
  PWR_CR_LPSDSR = longword($0001);                //Low-power deepsleep/sleep/low power run
  PWR_CR_PDDS = longword($0002);                  //Power Down Deepsleep
  PWR_CR_CWUF = longword($0004);                  //Clear Wakeup Flag
  PWR_CR_CSBF = longword($0008);                  //Clear Standby Flag
  PWR_CR_PVDE = longword($0010);                  //Power Voltage Detector Enable

  PWR_CR_PLS = longword($00E0);                   //PLS[2:0] bits (PVD Level Selection)
  PWR_CR_PLS_0 = longword($0020);                 //Bit 0
  PWR_CR_PLS_1 = longword($0040);                 //Bit 1
  PWR_CR_PLS_2 = longword($0080);                 //Bit 2

//PVD level configuration
  PWR_CR_PLS_LEV0 = longword($0000);              //PVD level 0
  PWR_CR_PLS_LEV1 = longword($0020);              //PVD level 1
  PWR_CR_PLS_LEV2 = longword($0040);              //PVD level 2
  PWR_CR_PLS_LEV3 = longword($0060);              //PVD level 3
  PWR_CR_PLS_LEV4 = longword($0080);              //PVD level 4
  PWR_CR_PLS_LEV5 = longword($00A0);              //PVD level 5
  PWR_CR_PLS_LEV6 = longword($00C0);              //PVD level 6
  PWR_CR_PLS_LEV7 = longword($00E0);              //PVD level 7

  PWR_CR_DBP = longword($0100);                   //Disable Backup Domain write protection

//******************  Bit definition for PWR_CSR register  *******************
  PWR_CSR_WUF = longword($0001);                  //Wakeup Flag
  PWR_CSR_SBF = longword($0002);                  //Standby Flag
  PWR_CSR_PVDO = longword($0004);                 //PVD Output
  PWR_CSR_VREFINTRDYF = longword($0008);          //Internal voltage reference (VREFINT) ready flag

  PWR_CSR_EWUP1 = longword($0100);                //Enable WKUP pin 1
  PWR_CSR_EWUP2 = longword($0200);                //Enable WKUP pin 2

//****************************************************************************

//Reset and Clock Control

//****************************************************************************

//*******************  Bit definition for RCC_CR register  *******************
  RCC_CR_HSION = longword($00000001);             //Internal High Speed clock enable
  RCC_CR_HSIRDY = longword($00000002);            //Internal High Speed clock ready flag
  RCC_CR_HSITRIM = longword($000000F8);           //Internal High Speed clock trimming
  RCC_CR_HSICAL = longword($0000FF00);            //Internal High Speed clock Calibration
  RCC_CR_HSEON = longword($00010000);             //External High Speed clock enable
  RCC_CR_HSERDY = longword($00020000);            //External High Speed clock ready flag
  RCC_CR_HSEBYP = longword($00040000);            //External High Speed clock Bypass
  RCC_CR_CSSON = longword($00080000);             //Clock Security System enable
  RCC_CR_PLLON = longword($01000000);             //PLL enable
  RCC_CR_PLLRDY = longword($02000000);            //PLL clock ready flag

//******************  Bit definition for RCC_CFGR register  ******************
//SW configuration
  RCC_CFGR_SW = longword($00000003);              //SW[1:0] bits (System clock Switch)
  RCC_CFGR_SW_0 = longword($00000001);            //Bit 0
  RCC_CFGR_SW_1 = longword($00000002);            //Bit 1

  RCC_CFGR_SW_HSI = longword($00000000);          //HSI selected as system clock
  RCC_CFGR_SW_HSE = longword($00000001);          //HSE selected as system clock
  RCC_CFGR_SW_PLL = longword($00000002);          //PLL selected as system clock

//SWS configuration
  RCC_CFGR_SWS = longword($0000000C);             //SWS[1:0] bits (System Clock Switch Status)
  RCC_CFGR_SWS_0 = longword($00000004);           //Bit 0
  RCC_CFGR_SWS_1 = longword($00000008);           //Bit 1

  RCC_CFGR_SWS_HSI = longword($00000000);         //HSI oscillator used as system clock
  RCC_CFGR_SWS_HSE = longword($00000004);         //HSE oscillator used as system clock
  RCC_CFGR_SWS_PLL = longword($00000008);         //PLL used as system clock

//HPRE configuration
  RCC_CFGR_HPRE = longword($000000F0);            //HPRE[3:0] bits (AHB prescaler)
  RCC_CFGR_HPRE_0 = longword($00000010);          //Bit 0
  RCC_CFGR_HPRE_1 = longword($00000020);          //Bit 1
  RCC_CFGR_HPRE_2 = longword($00000040);          //Bit 2
  RCC_CFGR_HPRE_3 = longword($00000080);          //Bit 3

  RCC_CFGR_HPRE_DIV1 = longword($00000000);       //SYSCLK not divided
  RCC_CFGR_HPRE_DIV2 = longword($00000080);       //SYSCLK divided by 2
  RCC_CFGR_HPRE_DIV4 = longword($00000090);       //SYSCLK divided by 4
  RCC_CFGR_HPRE_DIV8 = longword($000000A0);       //SYSCLK divided by 8
  RCC_CFGR_HPRE_DIV16 = longword($000000B0);      //SYSCLK divided by 16
  RCC_CFGR_HPRE_DIV64 = longword($000000C0);      //SYSCLK divided by 64
  RCC_CFGR_HPRE_DIV128 = longword($000000D0);     //SYSCLK divided by 128
  RCC_CFGR_HPRE_DIV256 = longword($000000E0);     //SYSCLK divided by 256
  RCC_CFGR_HPRE_DIV512 = longword($000000F0);     //SYSCLK divided by 512

//PPRE configuration
  RCC_CFGR_PPRE = longword($00000700);            //PRE[2:0] bits (APB prescaler)
  RCC_CFGR_PPRE_0 = longword($00000100);          //Bit 0
  RCC_CFGR_PPRE_1 = longword($00000200);          //Bit 1
  RCC_CFGR_PPRE_2 = longword($00000400);          //Bit 2

  RCC_CFGR_PPRE_DIV1 = longword($00000000);       //HCLK not divided
  RCC_CFGR_PPRE_DIV2 = longword($00000400);       //HCLK divided by 2
  RCC_CFGR_PPRE_DIV4 = longword($00000500);       //HCLK divided by 4
  RCC_CFGR_PPRE_DIV8 = longword($00000600);       //HCLK divided by 8
  RCC_CFGR_PPRE_DIV16 = longword($00000700);      //HCLK divided by 16

//ADCPPRE configuration
  RCC_CFGR_ADCPRE = longword($00004000);          //ADCPRE bit (ADC prescaler)

  RCC_CFGR_ADCPRE_DIV2 = longword($00000000);     //PCLK divided by 2
  RCC_CFGR_ADCPRE_DIV4 = longword($00004000);     //PCLK divided by 4

  RCC_CFGR_PLLSRC = longword($00010000);          //PLL entry clock source

  RCC_CFGR_PLLXTPRE = longword($00020000);        //HSE divider for PLL entry

//PLLMUL configuration
  RCC_CFGR_PLLMULL = longword($003C0000);         //PLLMUL[3:0] bits (PLL multiplication factor)
  RCC_CFGR_PLLMULL_0 = longword($00040000);       //Bit 0
  RCC_CFGR_PLLMULL_1 = longword($00080000);       //Bit 1
  RCC_CFGR_PLLMULL_2 = longword($00100000);       //Bit 2
  RCC_CFGR_PLLMULL_3 = longword($00200000);       //Bit 3

  RCC_CFGR_PLLSRC_HSI_Div2 = longword($00000000); //HSI clock divided by 2 selected as PLL entry clock source
  RCC_CFGR_PLLSRC_PREDIV1 = longword($00010000);  //PREDIV1 clock selected as PLL entry clock source

  RCC_CFGR_PLLXTPRE_PREDIV1 = longword($00000000); //PREDIV1 clock not divided for PLL entry
  RCC_CFGR_PLLXTPRE_PREDIV1_Div2 = longword($00020000); //PREDIV1 clock divided by 2 for PLL entry

  RCC_CFGR_PLLMULL2 = longword($00000000);        //PLL input clock*2
  RCC_CFGR_PLLMULL3 = longword($00040000);        //PLL input clock*3
  RCC_CFGR_PLLMULL4 = longword($00080000);        //PLL input clock*4
  RCC_CFGR_PLLMULL5 = longword($000C0000);        //PLL input clock*5
  RCC_CFGR_PLLMULL6 = longword($00100000);        //PLL input clock*6
  RCC_CFGR_PLLMULL7 = longword($00140000);        //PLL input clock*7
  RCC_CFGR_PLLMULL8 = longword($00180000);        //PLL input clock*8
  RCC_CFGR_PLLMULL9 = longword($001C0000);        //PLL input clock*9
  RCC_CFGR_PLLMULL10 = longword($00200000);       //PLL input clock10
  RCC_CFGR_PLLMULL11 = longword($00240000);       //PLL input clock*11
  RCC_CFGR_PLLMULL12 = longword($00280000);       //PLL input clock*12
  RCC_CFGR_PLLMULL13 = longword($002C0000);       //PLL input clock*13
  RCC_CFGR_PLLMULL14 = longword($00300000);       //PLL input clock*14
  RCC_CFGR_PLLMULL15 = longword($00340000);       //PLL input clock*15
  RCC_CFGR_PLLMULL16 = longword($00380000);       //PLL input clock*16

//MCO configuration
  RCC_CFGR_MCO = longword($07000000);             //MCO[2:0] bits (Microcontroller Clock Output)
  RCC_CFGR_MCO_0 = longword($01000000);           //Bit 0
  RCC_CFGR_MCO_1 = longword($02000000);           //Bit 1
  RCC_CFGR_MCO_2 = longword($04000000);           //Bit 2

  RCC_CFGR_MCO_NOCLOCK = longword($00000000);     //No clock
  RCC_CFGR_MCO_HSI14 = longword($01000000);       //HSI14 clock selected as MCO source
  RCC_CFGR_MCO_LSI = longword($02000000);         //LSI clock selected as MCO source
  RCC_CFGR_MCO_LSE = longword($03000000);         //LSE clock selected as MCO source
  RCC_CFGR_MCO_SYSCLK = longword($04000000);      //System clock selected as MCO source
  RCC_CFGR_MCO_HSI = longword($05000000);         //HSI clock selected as MCO source
  RCC_CFGR_MCO_HSE = longword($06000000);         //HSE clock selected as MCO source
  RCC_CFGR_MCO_PLL = longword($07000000);         //PLL clock divided by 2 selected as MCO source

//*****************  Bit definition for RCC_CIR register  *******************
  RCC_CIR_LSIRDYF = longword($00000001);          //LSI Ready Interrupt flag
  RCC_CIR_LSERDYF = longword($00000002);          //LSE Ready Interrupt flag
  RCC_CIR_HSIRDYF = longword($00000004);          //HSI Ready Interrupt flag
  RCC_CIR_HSERDYF = longword($00000008);          //HSE Ready Interrupt flag
  RCC_CIR_PLLRDYF = longword($00000010);          //PLL Ready Interrupt flag
  RCC_CIR_HSI14RDYF = longword($00000020);        //HSI14 Ready Interrupt flag
  RCC_CIR_CSSF = longword($00000080);             //Clock Security System Interrupt flag
  RCC_CIR_LSIRDYIE = longword($00000100);         //LSI Ready Interrupt Enable
  RCC_CIR_LSERDYIE = longword($00000200);         //LSE Ready Interrupt Enable
  RCC_CIR_HSIRDYIE = longword($00000400);         //HSI Ready Interrupt Enable
  RCC_CIR_HSERDYIE = longword($00000800);         //HSE Ready Interrupt Enable
  RCC_CIR_PLLRDYIE = longword($00001000);         //PLL Ready Interrupt Enable
  RCC_CIR_HSI14RDYIE = longword($00002000);       //HSI14 Ready Interrupt Enable
  RCC_CIR_LSIRDYC = longword($00010000);          //LSI Ready Interrupt Clear
  RCC_CIR_LSERDYC = longword($00020000);          //LSE Ready Interrupt Clear
  RCC_CIR_HSIRDYC = longword($00040000);          //HSI Ready Interrupt Clear
  RCC_CIR_HSERDYC = longword($00080000);          //HSE Ready Interrupt Clear
  RCC_CIR_PLLRDYC = longword($00100000);          //PLL Ready Interrupt Clear
  RCC_CIR_HSI14RDYC = longword($00200000);        //HSI14 Ready Interrupt Clear
  RCC_CIR_CSSC = longword($00800000);             //Clock Security System Interrupt Clear

//****************  Bit definition for RCC_APB2RSTR register  ****************
  RCC_APB2RSTR_SYSCFGRST = longword($00000001);   //SYSCFG clock reset
  RCC_APB2RSTR_ADC1RST = longword($00000200);     //ADC1 clock reset
  RCC_APB2RSTR_TIM1RST = longword($00000800);     //TIM1 clock reset
  RCC_APB2RSTR_SPI1RST = longword($00001000);     //SPI1 clock reset
  RCC_APB2RSTR_USART1RST = longword($00004000);   //USART1 clock reset
  RCC_APB2RSTR_TIM15RST = longword($00010000);    //TIM15 clock reset
  RCC_APB2RSTR_TIM16RST = longword($00020000);    //TIM16 clock reset
  RCC_APB2RSTR_TIM17RST = longword($00040000);    //TIM17 clock reset
  RCC_APB2RSTR_DBGMCURST = longword($00400000);   //DBGMCU clock reset

//****************  Bit definition for RCC_APB1RSTR register  ****************
  RCC_APB1RSTR_TIM2RST = longword($00000001);     //Timer 2 clock reset
  RCC_APB1RSTR_TIM3RST = longword($00000002);     //Timer 3 clock reset
  RCC_APB1RSTR_TIM6RST = longword($00000010);     //Timer 6 clock reset
  RCC_APB1RSTR_TIM14RST = longword($00000100);    //Timer 14 clock reset
  RCC_APB1RSTR_WWDGRST = longword($00000800);     //Window Watchdog clock reset
  RCC_APB1RSTR_SPI2RST = longword($00004000);     //SPI2 clock reset
  RCC_APB1RSTR_USART2RST = longword($00020000);   //USART 2 clock reset
  RCC_APB1RSTR_I2C1RST = longword($00200000);     //I2C 1 clock reset
  RCC_APB1RSTR_I2C2RST = longword($00400000);     //I2C 2 clock reset
  RCC_APB1RSTR_PWRRST = longword($10000000);      //PWR clock reset
  RCC_APB1RSTR_DACRST = longword($20000000);      //DAC clock reset
  RCC_APB1RSTR_CECRST = longword($40000000);      //CEC clock reset

//*****************  Bit definition for RCC_AHBENR register  *****************
  RCC_AHBENR_DMA1EN = longword($00000001);        //DMA1 clock enable
  RCC_AHBENR_SRAMEN = longword($00000004);        //SRAM interface clock enable
  RCC_AHBENR_FLITFEN = longword($00000010);       //FLITF clock enable
  RCC_AHBENR_CRCEN = longword($00000040);         //CRC clock enable
  RCC_AHBENR_GPIOAEN = longword($00020000);       //GPIOA clock enable
  RCC_AHBENR_GPIOBEN = longword($00040000);       //GPIOB clock enable
  RCC_AHBENR_GPIOCEN = longword($00080000);       //GPIOC clock enable
  RCC_AHBENR_GPIODEN = longword($00100000);       //GPIOD clock enable
  RCC_AHBENR_GPIOFEN = longword($00400000);       //GPIOF clock enable
  RCC_AHBENR_TSEN = longword($01000000);          //TS clock enable

//****************  Bit definition for RCC_APB2ENR register  *****************
  RCC_APB2ENR_SYSCFGEN = longword($00000001);     //SYSCFG clock enable
  RCC_APB2ENR_ADC1EN = longword($00000200);       //ADC1 clock enable
  RCC_APB2ENR_TIM1EN = longword($00000800);       //TIM1 clock enable
  RCC_APB2ENR_SPI1EN = longword($00001000);       //SPI1 clock enable
  RCC_APB2ENR_USART1EN = longword($00004000);     //USART1 clock enable
  RCC_APB2ENR_TIM15EN = longword($00010000);      //TIM15 clock enable
  RCC_APB2ENR_TIM16EN = longword($00020000);      //TIM16 clock enable
  RCC_APB2ENR_TIM17EN = longword($00040000);      //TIM17 clock enable
  RCC_APB2ENR_DBGMCUEN = longword($00400000);     //DBGMCU clock enable

//****************  Bit definition for RCC_APB1ENR register  *****************
  RCC_APB1ENR_TIM2EN = longword($00000001);       //Timer 2 clock enable
  RCC_APB1ENR_TIM3EN = longword($00000002);       //Timer 3 clock enable
  RCC_APB1ENR_TIM6EN = longword($00000010);       //Timer 6 clock enable
  RCC_APB1ENR_TIM14EN = longword($00000100);      //Timer 14 clock enable
  RCC_APB1ENR_WWDGEN = longword($00000800);       //Window Watchdog clock enable
  RCC_APB1ENR_SPI2EN = longword($00004000);       //SPI2 clock enable
  RCC_APB1ENR_USART2EN = longword($00020000);     //USART2 clock enable
  RCC_APB1ENR_I2C1EN = longword($00200000);       //I2C1 clock enable
  RCC_APB1ENR_I2C2EN = longword($00400000);       //I2C2 clock enable
  RCC_APB1ENR_PWREN = longword($10000000);        //PWR clock enable
  RCC_APB1ENR_DACEN = longword($20000000);        //DAC clock enable
  RCC_APB1ENR_CECEN = longword($40000000);        //CEC clock enable

//******************  Bit definition for RCC_BDCR register  ******************
  RCC_BDCR_LSEON = longword($00000001);           //External Low Speed oscillator enable
  RCC_BDCR_LSERDY = longword($00000002);          //External Low Speed oscillator Ready
  RCC_BDCR_LSEBYP = longword($00000004);          //External Low Speed oscillator Bypass

  RCC_BDCR_LSEDRV = longword($00000018);          //LSEDRV[1:0] bits (LSE Osc. drive capability)
  RCC_BDCR_LSEDRV_0 = longword($00000008);        //Bit 0
  RCC_BDCR_LSEDRV_1 = longword($00000010);        //Bit 1

  RCC_BDCR_RTCSEL = longword($00000300);          //RTCSEL[1:0] bits (RTC clock source selection)
  RCC_BDCR_RTCSEL_0 = longword($00000100);        //Bit 0
  RCC_BDCR_RTCSEL_1 = longword($00000200);        //Bit 1

//RTC congiguration
  RCC_BDCR_RTCSEL_NOCLOCK = longword($00000000);  //No clock
  RCC_BDCR_RTCSEL_LSE = longword($00000100);      //LSE oscillator clock used as RTC clock
  RCC_BDCR_RTCSEL_LSI = longword($00000200);      //LSI oscillator clock used as RTC clock
  RCC_BDCR_RTCSEL_HSE = longword($00000300);      //HSE oscillator clock divided by 128 used as RTC clock

  RCC_BDCR_RTCEN = longword($00008000);           //RTC clock enable
  RCC_BDCR_BDRST = longword($00010000);           //Backup domain software reset

//******************  Bit definition for RCC_CSR register  *******************
  RCC_CSR_LSION = longword($00000001);            //Internal Low Speed oscillator enable
  RCC_CSR_LSIRDY = longword($00000002);           //Internal Low Speed oscillator Ready
  RCC_CSR_V18PWRRSTF = longword($00800000);       //V1.8 power domain reset flag
  RCC_CSR_RMVF = longword($01000000);             //Remove reset flag
  RCC_CSR_OBL = longword($02000000);              //OBL reset flag
  RCC_CSR_PINRSTF = longword($04000000);          //PIN reset flag
  RCC_CSR_PORRSTF = longword($08000000);          //POR/PDR reset flag
  RCC_CSR_SFTRSTF = longword($10000000);          //Software Reset flag
  RCC_CSR_IWDGRSTF = longword($20000000);         //Independent Watchdog reset flag
  RCC_CSR_WWDGRSTF = longword($40000000);         //Window watchdog reset flag
  RCC_CSR_LPWRRSTF = longword($80000000);         //Low-Power reset flag

//******************  Bit definition for RCC_AHBRSTR register  ***************
  RCC_AHBRSTR_GPIOARST = longword($00020000);     //GPIOA clock reset
  RCC_AHBRSTR_GPIOBRST = longword($00040000);     //GPIOB clock reset
  RCC_AHBRSTR_GPIOCRST = longword($00080000);     //GPIOC clock reset
  RCC_AHBRSTR_GPIODRST = longword($00010000);     //GPIOD clock reset
  RCC_AHBRSTR_GPIOFRST = longword($00040000);     //GPIOF clock reset
  RCC_AHBRSTR_TSRST = longword($00100000);        //TS clock reset

//******************  Bit definition for RCC_CFGR2 register  *****************
//PREDIV1 configuration
  RCC_CFGR2_PREDIV1 = longword($0000000F);        //PREDIV1[3:0] bits
  RCC_CFGR2_PREDIV1_0 = longword($00000001);      //Bit 0
  RCC_CFGR2_PREDIV1_1 = longword($00000002);      //Bit 1
  RCC_CFGR2_PREDIV1_2 = longword($00000004);      //Bit 2
  RCC_CFGR2_PREDIV1_3 = longword($00000008);      //Bit 3

  RCC_CFGR2_PREDIV1_DIV1 = longword($00000000);   //PREDIV1 input clock not divided
  RCC_CFGR2_PREDIV1_DIV2 = longword($00000001);   //PREDIV1 input clock divided by 2
  RCC_CFGR2_PREDIV1_DIV3 = longword($00000002);   //PREDIV1 input clock divided by 3
  RCC_CFGR2_PREDIV1_DIV4 = longword($00000003);   //PREDIV1 input clock divided by 4
  RCC_CFGR2_PREDIV1_DIV5 = longword($00000004);   //PREDIV1 input clock divided by 5
  RCC_CFGR2_PREDIV1_DIV6 = longword($00000005);   //PREDIV1 input clock divided by 6
  RCC_CFGR2_PREDIV1_DIV7 = longword($00000006);   //PREDIV1 input clock divided by 7
  RCC_CFGR2_PREDIV1_DIV8 = longword($00000007);   //PREDIV1 input clock divided by 8
  RCC_CFGR2_PREDIV1_DIV9 = longword($00000008);   //PREDIV1 input clock divided by 9
  RCC_CFGR2_PREDIV1_DIV10 = longword($00000009);  //PREDIV1 input clock divided by 10
  RCC_CFGR2_PREDIV1_DIV11 = longword($0000000A);  //PREDIV1 input clock divided by 11
  RCC_CFGR2_PREDIV1_DIV12 = longword($0000000B);  //PREDIV1 input clock divided by 12
  RCC_CFGR2_PREDIV1_DIV13 = longword($0000000C);  //PREDIV1 input clock divided by 13
  RCC_CFGR2_PREDIV1_DIV14 = longword($0000000D);  //PREDIV1 input clock divided by 14
  RCC_CFGR2_PREDIV1_DIV15 = longword($0000000E);  //PREDIV1 input clock divided by 15
  RCC_CFGR2_PREDIV1_DIV16 = longword($0000000F);  //PREDIV1 input clock divided by 16

//******************  Bit definition for RCC_CFGR3 register  *****************
//USART1 Clock source selection
  RCC_CFGR3_USART1SW = longword($00000003);       //USART1SW[1:0] bits
  RCC_CFGR3_USART1SW_0 = longword($00000001);     //Bit 0
  RCC_CFGR3_USART1SW_1 = longword($00000002);     //Bit 1
//I2C1 Clock source selection
  RCC_CFGR3_I2C1SW = longword($00000010);         //I2C1SW bits
  RCC_CFGR3_CECSW = longword($00000040);          //CECSW bits
  RCC_CFGR3_ADCSW = longword($00000100);          //ADCSW bits

//******************  Bit definition for RCC_CR2 register  *******************
  RCC_CR2_HSI14ON = longword($00000001);          //Internal High Speed 14MHz clock enable
  RCC_CR2_HSI14RDY = longword($00000002);         //Internal High Speed 14MHz clock ready flag
  RCC_CR2_HSI14DIS = longword($00000004);         //Internal High Speed 14MHz clock disable
  RCC_CR2_HSI14TRIM = longword($000000F8);        //Internal High Speed 14MHz clock trimming
  RCC_CR2_HSI14CAL = longword($0000FF00);         //Internal High Speed 14MHz clock Calibration

//****************************************************************************

//Real-Time Clock (RTC)

//****************************************************************************
//*******************  Bits definition for RTC_TR register  ******************
  RTC_TR_PM = longword($00400000);
  RTC_TR_HT = longword($00300000);
  RTC_TR_HT_0 = longword($00100000);
  RTC_TR_HT_1 = longword($00200000);
  RTC_TR_HU = longword($000F0000);
  RTC_TR_HU_0 = longword($00010000);
  RTC_TR_HU_1 = longword($00020000);
  RTC_TR_HU_2 = longword($00040000);
  RTC_TR_HU_3 = longword($00080000);
  RTC_TR_MNT = longword($00007000);
  RTC_TR_MNT_0 = longword($00001000);
  RTC_TR_MNT_1 = longword($00002000);
  RTC_TR_MNT_2 = longword($00004000);
  RTC_TR_MNU = longword($00000F00);
  RTC_TR_MNU_0 = longword($00000100);
  RTC_TR_MNU_1 = longword($00000200);
  RTC_TR_MNU_2 = longword($00000400);
  RTC_TR_MNU_3 = longword($00000800);
  RTC_TR_ST = longword($00000070);
  RTC_TR_ST_0 = longword($00000010);
  RTC_TR_ST_1 = longword($00000020);
  RTC_TR_ST_2 = longword($00000040);
  RTC_TR_SU = longword($0000000F);
  RTC_TR_SU_0 = longword($00000001);
  RTC_TR_SU_1 = longword($00000002);
  RTC_TR_SU_2 = longword($00000004);
  RTC_TR_SU_3 = longword($00000008);

//*******************  Bits definition for RTC_DR register  ******************
  RTC_DR_YT = longword($00F00000);
  RTC_DR_YT_0 = longword($00100000);
  RTC_DR_YT_1 = longword($00200000);
  RTC_DR_YT_2 = longword($00400000);
  RTC_DR_YT_3 = longword($00800000);
  RTC_DR_YU = longword($000F0000);
  RTC_DR_YU_0 = longword($00010000);
  RTC_DR_YU_1 = longword($00020000);
  RTC_DR_YU_2 = longword($00040000);
  RTC_DR_YU_3 = longword($00080000);
  RTC_DR_WDU = longword($0000E000);
  RTC_DR_WDU_0 = longword($00002000);
  RTC_DR_WDU_1 = longword($00004000);
  RTC_DR_WDU_2 = longword($00008000);
  RTC_DR_MT = longword($00001000);
  RTC_DR_MU = longword($00000F00);
  RTC_DR_MU_0 = longword($00000100);
  RTC_DR_MU_1 = longword($00000200);
  RTC_DR_MU_2 = longword($00000400);
  RTC_DR_MU_3 = longword($00000800);
  RTC_DR_DT = longword($00000030);
  RTC_DR_DT_0 = longword($00000010);
  RTC_DR_DT_1 = longword($00000020);
  RTC_DR_DU = longword($0000000F);
  RTC_DR_DU_0 = longword($00000001);
  RTC_DR_DU_1 = longword($00000002);
  RTC_DR_DU_2 = longword($00000004);
  RTC_DR_DU_3 = longword($00000008);

//*******************  Bits definition for RTC_CR register  ******************
  RTC_CR_COE = longword($00800000);
  RTC_CR_OSEL = longword($00600000);
  RTC_CR_OSEL_0 = longword($00200000);
  RTC_CR_OSEL_1 = longword($00400000);
  RTC_CR_POL = longword($00100000);
  RTC_CR_CALSEL = longword($00080000);
  RTC_CR_BCK = longword($00040000);
  RTC_CR_SUB1H = longword($00020000);
  RTC_CR_ADD1H = longword($00010000);
  RTC_CR_TSIE = longword($00008000);
  RTC_CR_ALRAIE = longword($00001000);
  RTC_CR_TSE = longword($00000800);
  RTC_CR_ALRAE = longword($00000100);
  RTC_CR_DCE = longword($00000080);
  RTC_CR_FMT = longword($00000040);
  RTC_CR_BYPSHAD = longword($00000020);
  RTC_CR_REFCKON = longword($00000010);
  RTC_CR_TSEDGE = longword($00000008);

//*******************  Bits definition for RTC_ISR register  *****************
  RTC_ISR_RECALPF = longword($00010000);
  RTC_ISR_TAMP2F = longword($00004000);
  RTC_ISR_TAMP1F = longword($00002000);
  RTC_ISR_TSOVF = longword($00001000);
  RTC_ISR_TSF = longword($00000800);
  RTC_ISR_ALRAF = longword($00000100);
  RTC_ISR_INIT = longword($00000080);
  RTC_ISR_INITF = longword($00000040);
  RTC_ISR_RSF = longword($00000020);
  RTC_ISR_INITS = longword($00000010);
  RTC_ISR_SHPF = longword($00000008);
  RTC_ISR_ALRAWF = longword($00000001);

//*******************  Bits definition for RTC_PRER register  ****************
  RTC_PRER_PREDIV_A = longword($007F0000);
  RTC_PRER_PREDIV_S = longword($00007FFF);

//*******************  Bits definition for RTC_ALRMAR register  **************
  RTC_ALRMAR_MSK4 = longword($80000000);
  RTC_ALRMAR_WDSEL = longword($40000000);
  RTC_ALRMAR_DT = longword($30000000);
  RTC_ALRMAR_DT_0 = longword($10000000);
  RTC_ALRMAR_DT_1 = longword($20000000);
  RTC_ALRMAR_DU = longword($0F000000);
  RTC_ALRMAR_DU_0 = longword($01000000);
  RTC_ALRMAR_DU_1 = longword($02000000);
  RTC_ALRMAR_DU_2 = longword($04000000);
  RTC_ALRMAR_DU_3 = longword($08000000);
  RTC_ALRMAR_MSK3 = longword($00800000);
  RTC_ALRMAR_PM = longword($00400000);
  RTC_ALRMAR_HT = longword($00300000);
  RTC_ALRMAR_HT_0 = longword($00100000);
  RTC_ALRMAR_HT_1 = longword($00200000);
  RTC_ALRMAR_HU = longword($000F0000);
  RTC_ALRMAR_HU_0 = longword($00010000);
  RTC_ALRMAR_HU_1 = longword($00020000);
  RTC_ALRMAR_HU_2 = longword($00040000);
  RTC_ALRMAR_HU_3 = longword($00080000);
  RTC_ALRMAR_MSK2 = longword($00008000);
  RTC_ALRMAR_MNT = longword($00007000);
  RTC_ALRMAR_MNT_0 = longword($00001000);
  RTC_ALRMAR_MNT_1 = longword($00002000);
  RTC_ALRMAR_MNT_2 = longword($00004000);
  RTC_ALRMAR_MNU = longword($00000F00);
  RTC_ALRMAR_MNU_0 = longword($00000100);
  RTC_ALRMAR_MNU_1 = longword($00000200);
  RTC_ALRMAR_MNU_2 = longword($00000400);
  RTC_ALRMAR_MNU_3 = longword($00000800);
  RTC_ALRMAR_MSK1 = longword($00000080);
  RTC_ALRMAR_ST = longword($00000070);
  RTC_ALRMAR_ST_0 = longword($00000010);
  RTC_ALRMAR_ST_1 = longword($00000020);
  RTC_ALRMAR_ST_2 = longword($00000040);
  RTC_ALRMAR_SU = longword($0000000F);
  RTC_ALRMAR_SU_0 = longword($00000001);
  RTC_ALRMAR_SU_1 = longword($00000002);
  RTC_ALRMAR_SU_2 = longword($00000004);
  RTC_ALRMAR_SU_3 = longword($00000008);

//*******************  Bits definition for RTC_WPR register  *****************
  RTC_WPR_KEY = longword($000000FF);

//*******************  Bits definition for RTC_SSR register  *****************
  RTC_SSR_SS = longword($0003FFFF);

//*******************  Bits definition for RTC_SHIFTR register  **************
  RTC_SHIFTR_SUBFS = longword($00007FFF);
  RTC_SHIFTR_ADD1S = longword($80000000);

//*******************  Bits definition for RTC_TSTR register  ****************
  RTC_TSTR_PM = longword($00400000);
  RTC_TSTR_HT = longword($00300000);
  RTC_TSTR_HT_0 = longword($00100000);
  RTC_TSTR_HT_1 = longword($00200000);
  RTC_TSTR_HU = longword($000F0000);
  RTC_TSTR_HU_0 = longword($00010000);
  RTC_TSTR_HU_1 = longword($00020000);
  RTC_TSTR_HU_2 = longword($00040000);
  RTC_TSTR_HU_3 = longword($00080000);
  RTC_TSTR_MNT = longword($00007000);
  RTC_TSTR_MNT_0 = longword($00001000);
  RTC_TSTR_MNT_1 = longword($00002000);
  RTC_TSTR_MNT_2 = longword($00004000);
  RTC_TSTR_MNU = longword($00000F00);
  RTC_TSTR_MNU_0 = longword($00000100);
  RTC_TSTR_MNU_1 = longword($00000200);
  RTC_TSTR_MNU_2 = longword($00000400);
  RTC_TSTR_MNU_3 = longword($00000800);
  RTC_TSTR_ST = longword($00000070);
  RTC_TSTR_ST_0 = longword($00000010);
  RTC_TSTR_ST_1 = longword($00000020);
  RTC_TSTR_ST_2 = longword($00000040);
  RTC_TSTR_SU = longword($0000000F);
  RTC_TSTR_SU_0 = longword($00000001);
  RTC_TSTR_SU_1 = longword($00000002);
  RTC_TSTR_SU_2 = longword($00000004);
  RTC_TSTR_SU_3 = longword($00000008);

//*******************  Bits definition for RTC_TSDR register  ****************
  RTC_TSDR_WDU = longword($0000E000);
  RTC_TSDR_WDU_0 = longword($00002000);
  RTC_TSDR_WDU_1 = longword($00004000);
  RTC_TSDR_WDU_2 = longword($00008000);
  RTC_TSDR_MT = longword($00001000);
  RTC_TSDR_MU = longword($00000F00);
  RTC_TSDR_MU_0 = longword($00000100);
  RTC_TSDR_MU_1 = longword($00000200);
  RTC_TSDR_MU_2 = longword($00000400);
  RTC_TSDR_MU_3 = longword($00000800);
  RTC_TSDR_DT = longword($00000030);
  RTC_TSDR_DT_0 = longword($00000010);
  RTC_TSDR_DT_1 = longword($00000020);
  RTC_TSDR_DU = longword($0000000F);
  RTC_TSDR_DU_0 = longword($00000001);
  RTC_TSDR_DU_1 = longword($00000002);
  RTC_TSDR_DU_2 = longword($00000004);
  RTC_TSDR_DU_3 = longword($00000008);

//*******************  Bits definition for RTC_TSSSR register  ***************
  RTC_TSSSR_SS = longword($0003FFFF);

//*******************  Bits definition for RTC_CAL register  ****************
  RTC_CAL_CALP = longword($00008000);
  RTC_CAL_CALW8 = longword($00004000);
  RTC_CAL_CALW16 = longword($00002000);
  RTC_CAL_CALM = longword($000001FF);
  RTC_CAL_CALM_0 = longword($00000001);
  RTC_CAL_CALM_1 = longword($00000002);
  RTC_CAL_CALM_2 = longword($00000004);
  RTC_CAL_CALM_3 = longword($00000008);
  RTC_CAL_CALM_4 = longword($00000010);
  RTC_CAL_CALM_5 = longword($00000020);
  RTC_CAL_CALM_6 = longword($00000040);
  RTC_CAL_CALM_7 = longword($00000080);
  RTC_CAL_CALM_8 = longword($00000100);

//*******************  Bits definition for RTC_TAFCR register  ***************
  RTC_TAFCR_ALARMOUTTYPE = longword($00040000);
  RTC_TAFCR_TAMPPUDIS = longword($00008000);
  RTC_TAFCR_TAMPPRCH = longword($00006000);
  RTC_TAFCR_TAMPPRCH_0 = longword($00002000);
  RTC_TAFCR_TAMPPRCH_1 = longword($00004000);
  RTC_TAFCR_TAMPFLT = longword($00001800);
  RTC_TAFCR_TAMPFLT_0 = longword($00000800);
  RTC_TAFCR_TAMPFLT_1 = longword($00001000);
  RTC_TAFCR_TAMPFREQ = longword($00000700);
  RTC_TAFCR_TAMPFREQ_0 = longword($00000100);
  RTC_TAFCR_TAMPFREQ_1 = longword($00000200);
  RTC_TAFCR_TAMPFREQ_2 = longword($00000400);
  RTC_TAFCR_TAMPTS = longword($00000080);
  RTC_TAFCR_TAMP2EDGE = longword($00000010);
  RTC_TAFCR_TAMP2E = longword($00000008);
  RTC_TAFCR_TAMPIE = longword($00000004);
  RTC_TAFCR_TAMP1TRG = longword($00000002);
  RTC_TAFCR_TAMP1E = longword($00000001);

//*******************  Bits definition for RTC_ALRMASSR register  ************
  RTC_ALRMASSR_MASKSS = longword($0F000000);
  RTC_ALRMASSR_MASKSS_0 = longword($01000000);
  RTC_ALRMASSR_MASKSS_1 = longword($02000000);
  RTC_ALRMASSR_MASKSS_2 = longword($04000000);
  RTC_ALRMASSR_MASKSS_3 = longword($08000000);
  RTC_ALRMASSR_SS = longword($00007FFF);

//*******************  Bits definition for RTC_BKP0R register  ***************
  RTC_BKP0R = longword($FFFFFFFF);

//*******************  Bits definition for RTC_BKP1R register  ***************
  RTC_BKP1R = longword($FFFFFFFF);

//*******************  Bits definition for RTC_BKP2R register  ***************
  RTC_BKP2R = longword($FFFFFFFF);

//*******************  Bits definition for RTC_BKP3R register  ***************
  RTC_BKP3R = longword($FFFFFFFF);

//*******************  Bits definition for RTC_BKP4R register  ***************
  RTC_BKP4R = longword($FFFFFFFF);

//****************************************************************************

//Serial Peripheral Interface (SPI)

//****************************************************************************
//******************  Bit definition for SPI_CR1 register  *******************
  SPI_CR1_CPHA = longword($0001);                 //Clock Phase
  SPI_CR1_CPOL = longword($0002);                 //Clock Polarity
  SPI_CR1_MSTR = longword($0004);                 //Master Selection
  SPI_CR1_BR = longword($0038);                   //BR[2:0] bits (Baud Rate Control)
  SPI_CR1_BR_0 = longword($0008);                 //Bit 0
  SPI_CR1_BR_1 = longword($0010);                 //Bit 1
  SPI_CR1_BR_2 = longword($0020);                 //Bit 2
  SPI_CR1_SPE = longword($0040);                  //SPI Enable
  SPI_CR1_LSBFIRST = longword($0080);             //Frame Format
  SPI_CR1_SSI = longword($0100);                  //Internal slave select
  SPI_CR1_SSM = longword($0200);                  //Software slave management
  SPI_CR1_RXONLY = longword($0400);               //Receive only
  SPI_CR1_CRCL = longword($0800);                 //CRC Length
  SPI_CR1_CRCNEXT = longword($1000);              //Transmit CRC next
  SPI_CR1_CRCEN = longword($2000);                //Hardware CRC calculation enable
  SPI_CR1_BIDIOE = longword($4000);               //Output enable in bidirectional mode
  SPI_CR1_BIDIMODE = longword($8000);             //Bidirectional data mode enable

//******************  Bit definition for SPI_CR2 register  *******************
  SPI_CR2_RXDMAEN = longword($0001);              //Rx Buffer DMA Enable
  SPI_CR2_TXDMAEN = longword($0002);              //Tx Buffer DMA Enable
  SPI_CR2_SSOE = longword($0004);                 //SS Output Enable
  SPI_CR2_NSSP = longword($0008);                 //NSS pulse management Enable
  SPI_CR2_FRF = longword($0010);                  //Frame Format Enable
  SPI_CR2_ERRIE = longword($0020);                //Error Interrupt Enable
  SPI_CR2_RXNEIE = longword($0040);               //RX buffer Not Empty Interrupt Enable
  SPI_CR2_TXEIE = longword($0080);                //Tx buffer Empty Interrupt Enable
  SPI_CR2_DS = longword($0F00);                   //DS[3:0] Data Size
  SPI_CR2_DS_0 = longword($0100);                 //Bit 0
  SPI_CR2_DS_1 = longword($0200);                 //Bit 1
  SPI_CR2_DS_2 = longword($0400);                 //Bit 2
  SPI_CR2_DS_3 = longword($0800);                 //Bit 3
  SPI_CR2_FRXTH = longword($1000);                //FIFO reception Threshold
  SPI_CR2_LDMARX = longword($2000);               //Last DMA transfer for reception
  SPI_CR2_LDMATX = longword($4000);               //Last DMA transfer for transmission

//*******************  Bit definition for SPI_SR register  *******************
  SPI_SR_RXNE = longword($0001);                  //Receive buffer Not Empty
  SPI_SR_TXE = longword($0002);                   //Transmit buffer Empty
  SPI_SR_CHSIDE = longword($0004);                //Channel side
  SPI_SR_UDR = longword($0008);                   //Underrun flag
  SPI_SR_CRCERR = longword($0010);                //CRC Error flag
  SPI_SR_MODF = longword($0020);                  //Mode fault
  SPI_SR_OVR = longword($0040);                   //Overrun flag
  SPI_SR_BSY = longword($0080);                   //Busy flag
  SPI_SR_FRE = longword($0100);                   //TI frame format error
  SPI_SR_FRLVL = longword($0600);                 //FIFO Reception Level
  SPI_SR_FRLVL_0 = longword($0200);               //Bit 0
  SPI_SR_FRLVL_1 = longword($0400);               //Bit 1
  SPI_SR_FTLVL = longword($1800);                 //FIFO Transmission Level
  SPI_SR_FTLVL_0 = longword($0800);               //Bit 0
  SPI_SR_FTLVL_1 = longword($1000);               //Bit 1

//*******************  Bit definition for SPI_DR register  *******************
  SPI_DR_DR = longword($FFFF);                    //Data Register

//******************  Bit definition for SPI_CRCPR register  *****************
  SPI_CRCPR_CRCPOLY = longword($FFFF);            //CRC polynomial register

//*****************  Bit definition for SPI_RXCRCR register  *****************
  SPI_RXCRCR_RXCRC = longword($FFFF);             //Rx CRC Register

//*****************  Bit definition for SPI_TXCRCR register  *****************
  SPI_TXCRCR_TXCRC = longword($FFFF);             //Tx CRC Register

//*****************  Bit definition for SPI_I2SCFGR register  ****************
  SPI_I2SCFGR_CHLEN = longword($0001);            //hannel length (number of bits per audio channel)
  SPI_I2SCFGR_DATLEN = longword($0006);           //ATLEN[1:0] bits (Data length to be transferred)
  SPI_I2SCFGR_DATLEN_0 = longword($0002);         //it 0
  SPI_I2SCFGR_DATLEN_1 = longword($0004);         //it 1
  SPI_I2SCFGR_CKPOL = longword($0008);            //teady state clock polarity
  SPI_I2SCFGR_I2SSTD = longword($0030);           //2SSTD[1:0] bits (I2S standard selection)
  SPI_I2SCFGR_I2SSTD_0 = longword($0010);         //it 0
  SPI_I2SCFGR_I2SSTD_1 = longword($0020);         //it 1
  SPI_I2SCFGR_PCMSYNC = longword($0080);          //CM frame synchronization
  SPI_I2SCFGR_I2SCFG = longword($0300);           //2SCFG[1:0] bits (I2S configuration mode)
  SPI_I2SCFGR_I2SCFG_0 = longword($0100);         //it 0
  SPI_I2SCFGR_I2SCFG_1 = longword($0200);         //it 1
  SPI_I2SCFGR_I2SE = longword($0400);             //2S Enable
  SPI_I2SCFGR_I2SMOD = longword($0800);           //2S mode selection

//*****************  Bit definition for SPI_I2SPR register  ******************
  SPI_I2SPR_I2SDIV = longword($00FF);             //2S Linear prescaler
  SPI_I2SPR_ODD = longword($0100);                //dd factor for the prescaler
  SPI_I2SPR_MCKOE = longword($0200);              //aster Clock Output Enable

//****************************************************************************

//System Configuration (SYSCFG)

//****************************************************************************
//****************  Bit definition for SYSCFG_CFGR1 register  ***************
  SYSCFG_CFGR1_MEM_MODE = longword($00000003);    //SYSCFG_Memory Remap Config
  SYSCFG_CFGR1_MEM_MODE_0 = longword($00000001);  //SYSCFG_Memory Remap Config Bit 0
  SYSCFG_CFGR1_MEM_MODE_1 = longword($00000002);  //SYSCFG_Memory Remap Config Bit 1
  SYSCFG_CFGR1_ADC_DMA_RMP = longword($00000100); //ADC DMA remap
  SYSCFG_CFGR1_USART1TX_DMA_RMP = longword($00000200); //USART1 TX DMA remap
  SYSCFG_CFGR1_USART1RX_DMA_RMP = longword($00000400); //USART1 RX DMA remap
  SYSCFG_CFGR1_TIM16_DMA_RMP = longword($00000800); //Timer 16 DMA remap
  SYSCFG_CFGR1_TIM17_DMA_RMP = longword($00001000); //Timer 17 DMA remap
  SYSCFG_CFGR1_I2C_FMP_PB6 = longword($00010000); //I2C PB6 Fast mode plus
  SYSCFG_CFGR1_I2C_FMP_PB7 = longword($00020000); //I2C PB7 Fast mode plus
  SYSCFG_CFGR1_I2C_FMP_PB8 = longword($00040000); //I2C PB8 Fast mode plus
  SYSCFG_CFGR1_I2C_FMP_PB9 = longword($00080000); //I2C PB9 Fast mode plus

//****************  Bit definition for SYSCFG_EXTICR1 register  **************
  SYSCFG_EXTICR1_EXTI0 = longword($000F);         //EXTI 0 configuration
  SYSCFG_EXTICR1_EXTI1 = longword($00F0);         //EXTI 1 configuration
  SYSCFG_EXTICR1_EXTI2 = longword($0F00);         //EXTI 2 configuration
  SYSCFG_EXTICR1_EXTI3 = longword($F000);         //EXTI 3 configuration

// EXTI0 configuration

  SYSCFG_EXTICR1_EXTI0_PA = longword($0000);      //PA[0] pin
  SYSCFG_EXTICR1_EXTI0_PB = longword($0001);      //PB[0] pin
  SYSCFG_EXTICR1_EXTI0_PC = longword($0002);      //PC[0] pin
  SYSCFG_EXTICR1_EXTI0_PF = longword($0003);      //PF[0] pin

// EXTI1 configuration

  SYSCFG_EXTICR1_EXTI1_PA = longword($0000);      //PA[1] pin
  SYSCFG_EXTICR1_EXTI1_PB = longword($0010);      //PB[1] pin
  SYSCFG_EXTICR1_EXTI1_PC = longword($0020);      //PC[1] pin
  SYSCFG_EXTICR1_EXTI1_PF = longword($0030);      //PF[1] pin

// EXTI2 configuration

  SYSCFG_EXTICR1_EXTI2_PA = longword($0000);      //PA[2] pin
  SYSCFG_EXTICR1_EXTI2_PB = longword($0100);      //PB[2] pin
  SYSCFG_EXTICR1_EXTI2_PC = longword($0200);      //PC[2] pin
  SYSCFG_EXTICR1_EXTI2_PD = longword($0300);      //PD[2] pin

// EXTI3 configuration

  SYSCFG_EXTICR1_EXTI3_PA = longword($0000);      //PA[3] pin
  SYSCFG_EXTICR1_EXTI3_PB = longword($1000);      //PB[3] pin
  SYSCFG_EXTICR1_EXTI3_PC = longword($2000);      //PC[3] pin

//****************  Bit definition for SYSCFG_EXTICR2 register  ****************
  SYSCFG_EXTICR2_EXTI4 = longword($000F);         //EXTI 4 configuration
  SYSCFG_EXTICR2_EXTI5 = longword($00F0);         //EXTI 5 configuration
  SYSCFG_EXTICR2_EXTI6 = longword($0F00);         //EXTI 6 configuration
  SYSCFG_EXTICR2_EXTI7 = longword($F000);         //EXTI 7 configuration

// EXTI4 configuration

  SYSCFG_EXTICR2_EXTI4_PA = longword($0000);      //PA[4] pin
  SYSCFG_EXTICR2_EXTI4_PB = longword($0001);      //PB[4] pin
  SYSCFG_EXTICR2_EXTI4_PC = longword($0002);      //PC[4] pin
  SYSCFG_EXTICR2_EXTI4_PF = longword($0003);      //PF[4] pin

// EXTI5 configuration

  SYSCFG_EXTICR2_EXTI5_PA = longword($0000);      //PA[5] pin
  SYSCFG_EXTICR2_EXTI5_PB = longword($0010);      //PB[5] pin
  SYSCFG_EXTICR2_EXTI5_PC = longword($0020);      //PC[5] pin
  SYSCFG_EXTICR2_EXTI5_PF = longword($0030);      //PF[5] pin

// EXTI6 configuration

  SYSCFG_EXTICR2_EXTI6_PA = longword($0000);      //PA[6] pin
  SYSCFG_EXTICR2_EXTI6_PB = longword($0100);      //PB[6] pin
  SYSCFG_EXTICR2_EXTI6_PC = longword($0200);      //PC[6] pin
  SYSCFG_EXTICR2_EXTI6_PF = longword($0300);      //PF[6] pin

// EXTI7 configuration

  SYSCFG_EXTICR2_EXTI7_PA = longword($0000);      //PA[7] pin
  SYSCFG_EXTICR2_EXTI7_PB = longword($1000);      //PB[7] pin
  SYSCFG_EXTICR2_EXTI7_PC = longword($2000);      //PC[7] pin
  SYSCFG_EXTICR2_EXTI7_PF = longword($3000);      //PF[7] pin

//****************  Bit definition for SYSCFG_EXTICR3 register  ****************
  SYSCFG_EXTICR3_EXTI8 = longword($000F);         //EXTI 8 configuration
  SYSCFG_EXTICR3_EXTI9 = longword($00F0);         //EXTI 9 configuration
  SYSCFG_EXTICR3_EXTI10 = longword($0F00);        //EXTI 10 configuration
  SYSCFG_EXTICR3_EXTI11 = longword($F000);        //EXTI 11 configuration

// EXTI8 configuration

  SYSCFG_EXTICR3_EXTI8_PA = longword($0000);      //PA[8] pin
  SYSCFG_EXTICR3_EXTI8_PB = longword($0001);      //PB[8] pin
  SYSCFG_EXTICR3_EXTI8_PC = longword($0002);      //PC[8] pin

// EXTI9 configuration

  SYSCFG_EXTICR3_EXTI9_PA = longword($0000);      //PA[9] pin
  SYSCFG_EXTICR3_EXTI9_PB = longword($0010);      //PB[9] pin
  SYSCFG_EXTICR3_EXTI9_PC = longword($0020);      //PC[9] pin

// EXTI10 configuration

  SYSCFG_EXTICR3_EXTI10_PA = longword($0000);     //PA[10] pin
  SYSCFG_EXTICR3_EXTI10_PB = longword($0100);     //PB[10] pin
  SYSCFG_EXTICR3_EXTI10_PC = longword($0200);     //PC[10] pin

// EXTI11 configuration

  SYSCFG_EXTICR3_EXTI11_PA = longword($0000);     //PA[11] pin
  SYSCFG_EXTICR3_EXTI11_PB = longword($1000);     //PB[11] pin
  SYSCFG_EXTICR3_EXTI11_PC = longword($2000);     //PC[11] pin

//****************  Bit definition for SYSCFG_EXTICR4 register  ****************
  SYSCFG_EXTICR4_EXTI12 = longword($000F);        //EXTI 12 configuration
  SYSCFG_EXTICR4_EXTI13 = longword($00F0);        //EXTI 13 configuration
  SYSCFG_EXTICR4_EXTI14 = longword($0F00);        //EXTI 14 configuration
  SYSCFG_EXTICR4_EXTI15 = longword($F000);        //EXTI 15 configuration

// EXTI12 configuration

  SYSCFG_EXTICR4_EXTI12_PA = longword($0000);     //PA[12] pin
  SYSCFG_EXTICR4_EXTI12_PB = longword($0001);     //PB[12] pin
  SYSCFG_EXTICR4_EXTI12_PC = longword($0002);     //PC[12] pin

// EXTI13 configuration

  SYSCFG_EXTICR4_EXTI13_PA = longword($0000);     //PA[13] pin
  SYSCFG_EXTICR4_EXTI13_PB = longword($0010);     //PB[13] pin
  SYSCFG_EXTICR4_EXTI13_PC = longword($0020);     //PC[13] pin

// EXTI14 configuration

  SYSCFG_EXTICR4_EXTI14_PA = longword($0000);     //PA[14] pin
  SYSCFG_EXTICR4_EXTI14_PB = longword($0100);     //PB[14] pin
  SYSCFG_EXTICR4_EXTI14_PC = longword($0200);     //PC[14] pin

// EXTI15 configuration

  SYSCFG_EXTICR4_EXTI15_PA = longword($0000);     //PA[15] pin
  SYSCFG_EXTICR4_EXTI15_PB = longword($1000);     //PB[15] pin
  SYSCFG_EXTICR4_EXTI15_PC = longword($2000);     //PC[15] pin

//****************  Bit definition for SYSCFG_CFGR2 register  ***************
  SYSCFG_CFGR2_LOCKUP_LOCK = longword($00000001); //Enables and locks the PVD connection with Timer1 Break Input and also the PVD_EN and PVDSEL[2:0] bits of the Power Control Interface
  SYSCFG_CFGR2_SRAM_PARITY_LOCK = longword($00000002); //Enables and locks the SRAM_PARITY error signal with Break Input of TIMER1
  SYSCFG_CFGR2_PVD_LOCK = longword($00000004);    //Enables and locks the LOCKUP (Hardfault) output of CortexM0 with Break Input of TIMER1
  SYSCFG_CFGR2_SRAM_PE = longword($00000100);     //SRAM Parity error flag

//****************************************************************************

//Timers (TIM)

//****************************************************************************
//******************  Bit definition for TIM_CR1 register  *******************
  TIM_CR1_CEN = longword($0001);                  //ounter enable
  TIM_CR1_UDIS = longword($0002);                 //pdate disable
  TIM_CR1_URS = longword($0004);                  //pdate request source
  TIM_CR1_OPM = longword($0008);                  //ne pulse mode
  TIM_CR1_DIR = longword($0010);                  //irection

  TIM_CR1_CMS = longword($0060);                  //MS[1:0] bits (Center-aligned mode selection)
  TIM_CR1_CMS_0 = longword($0020);                //it 0
  TIM_CR1_CMS_1 = longword($0040);                //it 1

  TIM_CR1_ARPE = longword($0080);                 //uto-reload preload enable

  TIM_CR1_CKD = longword($0300);                  //KD[1:0] bits (clock division)
  TIM_CR1_CKD_0 = longword($0100);                //it 0
  TIM_CR1_CKD_1 = longword($0200);                //it 1

//******************  Bit definition for TIM_CR2 register  *******************
  TIM_CR2_CCPC = longword($0001);                 //apture/Compare Preloaded Control
  TIM_CR2_CCUS = longword($0004);                 //apture/Compare Control Update Selection
  TIM_CR2_CCDS = longword($0008);                 //apture/Compare DMA Selection

  TIM_CR2_MMS = longword($0070);                  //MS[2:0] bits (Master Mode Selection)
  TIM_CR2_MMS_0 = longword($0010);                //it 0
  TIM_CR2_MMS_1 = longword($0020);                //it 1
  TIM_CR2_MMS_2 = longword($0040);                //it 2

  TIM_CR2_TI1S = longword($0080);                 //I1 Selection
  TIM_CR2_OIS1 = longword($0100);                 //utput Idle state 1 (OC1 output)
  TIM_CR2_OIS1N = longword($0200);                //utput Idle state 1 (OC1N output)
  TIM_CR2_OIS2 = longword($0400);                 //utput Idle state 2 (OC2 output)
  TIM_CR2_OIS2N = longword($0800);                //utput Idle state 2 (OC2N output)
  TIM_CR2_OIS3 = longword($1000);                 //utput Idle state 3 (OC3 output)
  TIM_CR2_OIS3N = longword($2000);                //utput Idle state 3 (OC3N output)
  TIM_CR2_OIS4 = longword($4000);                 //utput Idle state 4 (OC4 output)

//******************  Bit definition for TIM_SMCR register  ******************
  TIM_SMCR_SMS = longword($0007);                 //MS[2:0] bits (Slave mode selection)
  TIM_SMCR_SMS_0 = longword($0001);               //it 0
  TIM_SMCR_SMS_1 = longword($0002);               //it 1
  TIM_SMCR_SMS_2 = longword($0004);               //it 2

  TIM_SMCR_OCCS = longword($0008);                //OCREF clear selection

  TIM_SMCR_TS = longword($0070);                  //S[2:0] bits (Trigger selection)
  TIM_SMCR_TS_0 = longword($0010);                //it 0
  TIM_SMCR_TS_1 = longword($0020);                //it 1
  TIM_SMCR_TS_2 = longword($0040);                //it 2

  TIM_SMCR_MSM = longword($0080);                 //aster/slave mode

  TIM_SMCR_ETF = longword($0F00);                 //TF[3:0] bits (External trigger filter)
  TIM_SMCR_ETF_0 = longword($0100);               //it 0
  TIM_SMCR_ETF_1 = longword($0200);               //it 1
  TIM_SMCR_ETF_2 = longword($0400);               //it 2
  TIM_SMCR_ETF_3 = longword($0800);               //it 3

  TIM_SMCR_ETPS = longword($3000);                //TPS[1:0] bits (External trigger prescaler)
  TIM_SMCR_ETPS_0 = longword($1000);              //it 0
  TIM_SMCR_ETPS_1 = longword($2000);              //it 1

  TIM_SMCR_ECE = longword($4000);                 //xternal clock enable
  TIM_SMCR_ETP = longword($8000);                 //xternal trigger polarity

//******************  Bit definition for TIM_DIER register  ******************
  TIM_DIER_UIE = longword($0001);                 //pdate interrupt enable
  TIM_DIER_CC1IE = longword($0002);               //apture/Compare 1 interrupt enable
  TIM_DIER_CC2IE = longword($0004);               //apture/Compare 2 interrupt enable
  TIM_DIER_CC3IE = longword($0008);               //apture/Compare 3 interrupt enable
  TIM_DIER_CC4IE = longword($0010);               //apture/Compare 4 interrupt enable
  TIM_DIER_COMIE = longword($0020);               //OM interrupt enable
  TIM_DIER_TIE = longword($0040);                 //rigger interrupt enable
  TIM_DIER_BIE = longword($0080);                 //reak interrupt enable
  TIM_DIER_UDE = longword($0100);                 //pdate DMA request enable
  TIM_DIER_CC1DE = longword($0200);               //apture/Compare 1 DMA request enable
  TIM_DIER_CC2DE = longword($0400);               //apture/Compare 2 DMA request enable
  TIM_DIER_CC3DE = longword($0800);               //apture/Compare 3 DMA request enable
  TIM_DIER_CC4DE = longword($1000);               //apture/Compare 4 DMA request enable
  TIM_DIER_COMDE = longword($2000);               //OM DMA request enable
  TIM_DIER_TDE = longword($4000);                 //rigger DMA request enable

//*******************  Bit definition for TIM_SR register  *******************
  TIM_SR_UIF = longword($0001);                   //pdate interrupt Flag
  TIM_SR_CC1IF = longword($0002);                 //apture/Compare 1 interrupt Flag
  TIM_SR_CC2IF = longword($0004);                 //apture/Compare 2 interrupt Flag
  TIM_SR_CC3IF = longword($0008);                 //apture/Compare 3 interrupt Flag
  TIM_SR_CC4IF = longword($0010);                 //apture/Compare 4 interrupt Flag
  TIM_SR_COMIF = longword($0020);                 //OM interrupt Flag
  TIM_SR_TIF = longword($0040);                   //rigger interrupt Flag
  TIM_SR_BIF = longword($0080);                   //reak interrupt Flag
  TIM_SR_CC1OF = longword($0200);                 //apture/Compare 1 Overcapture Flag
  TIM_SR_CC2OF = longword($0400);                 //apture/Compare 2 Overcapture Flag
  TIM_SR_CC3OF = longword($0800);                 //apture/Compare 3 Overcapture Flag
  TIM_SR_CC4OF = longword($1000);                 //apture/Compare 4 Overcapture Flag

//******************  Bit definition for TIM_EGR register  *******************
  TIM_EGR_UG = longword($01);                     //pdate Generation
  TIM_EGR_CC1G = longword($02);                   //apture/Compare 1 Generation
  TIM_EGR_CC2G = longword($04);                   //apture/Compare 2 Generation
  TIM_EGR_CC3G = longword($08);                   //apture/Compare 3 Generation
  TIM_EGR_CC4G = longword($10);                   //apture/Compare 4 Generation
  TIM_EGR_COMG = longword($20);                   //apture/Compare Control Update Generation
  TIM_EGR_TG = longword($40);                     //rigger Generation
  TIM_EGR_BG = longword($80);                     //reak Generation

//*****************  Bit definition for TIM_CCMR1 register  ******************
  TIM_CCMR1_CC1S = longword($0003);               //C1S[1:0] bits (Capture/Compare 1 Selection)
  TIM_CCMR1_CC1S_0 = longword($0001);             //it 0
  TIM_CCMR1_CC1S_1 = longword($0002);             //it 1

  TIM_CCMR1_OC1FE = longword($0004);              //utput Compare 1 Fast enable
  TIM_CCMR1_OC1PE = longword($0008);              //utput Compare 1 Preload enable

  TIM_CCMR1_OC1M = longword($0070);               //C1M[2:0] bits (Output Compare 1 Mode)
  TIM_CCMR1_OC1M_0 = longword($0010);             //it 0
  TIM_CCMR1_OC1M_1 = longword($0020);             //it 1
  TIM_CCMR1_OC1M_2 = longword($0040);             //it 2

  TIM_CCMR1_OC1CE = longword($0080);              //utput Compare 1Clear Enable

  TIM_CCMR1_CC2S = longword($0300);               //C2S[1:0] bits (Capture/Compare 2 Selection)
  TIM_CCMR1_CC2S_0 = longword($0100);             //it 0
  TIM_CCMR1_CC2S_1 = longword($0200);             //it 1

  TIM_CCMR1_OC2FE = longword($0400);              //utput Compare 2 Fast enable
  TIM_CCMR1_OC2PE = longword($0800);              //utput Compare 2 Preload enable

  TIM_CCMR1_OC2M = longword($7000);               //C2M[2:0] bits (Output Compare 2 Mode)
  TIM_CCMR1_OC2M_0 = longword($1000);             //it 0
  TIM_CCMR1_OC2M_1 = longword($2000);             //it 1
  TIM_CCMR1_OC2M_2 = longword($4000);             //it 2

  TIM_CCMR1_OC2CE = longword($8000);              //utput Compare 2 Clear Enable

//----------------------------------------------------------------------------

  TIM_CCMR1_IC1PSC = longword($000C);             //C1PSC[1:0] bits (Input Capture 1 Prescaler)
  TIM_CCMR1_IC1PSC_0 = longword($0004);           //it 0
  TIM_CCMR1_IC1PSC_1 = longword($0008);           //it 1

  TIM_CCMR1_IC1F = longword($00F0);               //C1F[3:0] bits (Input Capture 1 Filter)
  TIM_CCMR1_IC1F_0 = longword($0010);             //it 0
  TIM_CCMR1_IC1F_1 = longword($0020);             //it 1
  TIM_CCMR1_IC1F_2 = longword($0040);             //it 2
  TIM_CCMR1_IC1F_3 = longword($0080);             //it 3

  TIM_CCMR1_IC2PSC = longword($0C00);             //C2PSC[1:0] bits (Input Capture 2 Prescaler)
  TIM_CCMR1_IC2PSC_0 = longword($0400);           //it 0
  TIM_CCMR1_IC2PSC_1 = longword($0800);           //it 1

  TIM_CCMR1_IC2F = longword($F000);               //C2F[3:0] bits (Input Capture 2 Filter)
  TIM_CCMR1_IC2F_0 = longword($1000);             //it 0
  TIM_CCMR1_IC2F_1 = longword($2000);             //it 1
  TIM_CCMR1_IC2F_2 = longword($4000);             //it 2
  TIM_CCMR1_IC2F_3 = longword($8000);             //it 3

//*****************  Bit definition for TIM_CCMR2 register  ******************
  TIM_CCMR2_CC3S = longword($0003);               //C3S[1:0] bits (Capture/Compare 3 Selection)
  TIM_CCMR2_CC3S_0 = longword($0001);             //it 0
  TIM_CCMR2_CC3S_1 = longword($0002);             //it 1

  TIM_CCMR2_OC3FE = longword($0004);              //utput Compare 3 Fast enable
  TIM_CCMR2_OC3PE = longword($0008);              //utput Compare 3 Preload enable

  TIM_CCMR2_OC3M = longword($0070);               //C3M[2:0] bits (Output Compare 3 Mode)
  TIM_CCMR2_OC3M_0 = longword($0010);             //it 0
  TIM_CCMR2_OC3M_1 = longword($0020);             //it 1
  TIM_CCMR2_OC3M_2 = longword($0040);             //it 2

  TIM_CCMR2_OC3CE = longword($0080);              //utput Compare 3 Clear Enable

  TIM_CCMR2_CC4S = longword($0300);               //C4S[1:0] bits (Capture/Compare 4 Selection)
  TIM_CCMR2_CC4S_0 = longword($0100);             //it 0
  TIM_CCMR2_CC4S_1 = longword($0200);             //it 1

  TIM_CCMR2_OC4FE = longword($0400);              //utput Compare 4 Fast enable
  TIM_CCMR2_OC4PE = longword($0800);              //utput Compare 4 Preload enable

  TIM_CCMR2_OC4M = longword($7000);               //C4M[2:0] bits (Output Compare 4 Mode)
  TIM_CCMR2_OC4M_0 = longword($1000);             //it 0
  TIM_CCMR2_OC4M_1 = longword($2000);             //it 1
  TIM_CCMR2_OC4M_2 = longword($4000);             //it 2

  TIM_CCMR2_OC4CE = longword($8000);              //utput Compare 4 Clear Enable

//----------------------------------------------------------------------------

  TIM_CCMR2_IC3PSC = longword($000C);             //C3PSC[1:0] bits (Input Capture 3 Prescaler)
  TIM_CCMR2_IC3PSC_0 = longword($0004);           //it 0
  TIM_CCMR2_IC3PSC_1 = longword($0008);           //it 1

  TIM_CCMR2_IC3F = longword($00F0);               //C3F[3:0] bits (Input Capture 3 Filter)
  TIM_CCMR2_IC3F_0 = longword($0010);             //it 0
  TIM_CCMR2_IC3F_1 = longword($0020);             //it 1
  TIM_CCMR2_IC3F_2 = longword($0040);             //it 2
  TIM_CCMR2_IC3F_3 = longword($0080);             //it 3

  TIM_CCMR2_IC4PSC = longword($0C00);             //C4PSC[1:0] bits (Input Capture 4 Prescaler)
  TIM_CCMR2_IC4PSC_0 = longword($0400);           //it 0
  TIM_CCMR2_IC4PSC_1 = longword($0800);           //it 1

  TIM_CCMR2_IC4F = longword($F000);               //C4F[3:0] bits (Input Capture 4 Filter)
  TIM_CCMR2_IC4F_0 = longword($1000);             //it 0
  TIM_CCMR2_IC4F_1 = longword($2000);             //it 1
  TIM_CCMR2_IC4F_2 = longword($4000);             //it 2
  TIM_CCMR2_IC4F_3 = longword($8000);             //it 3

//******************  Bit definition for TIM_CCER register  ******************
  TIM_CCER_CC1E = longword($0001);                //apture/Compare 1 output enable
  TIM_CCER_CC1P = longword($0002);                //apture/Compare 1 output Polarity
  TIM_CCER_CC1NE = longword($0004);               //apture/Compare 1 Complementary output enable
  TIM_CCER_CC1NP = longword($0008);               //apture/Compare 1 Complementary output Polarity
  TIM_CCER_CC2E = longword($0010);                //apture/Compare 2 output enable
  TIM_CCER_CC2P = longword($0020);                //apture/Compare 2 output Polarity
  TIM_CCER_CC2NE = longword($0040);               //apture/Compare 2 Complementary output enable
  TIM_CCER_CC2NP = longword($0080);               //apture/Compare 2 Complementary output Polarity
  TIM_CCER_CC3E = longword($0100);                //apture/Compare 3 output enable
  TIM_CCER_CC3P = longword($0200);                //apture/Compare 3 output Polarity
  TIM_CCER_CC3NE = longword($0400);               //apture/Compare 3 Complementary output enable
  TIM_CCER_CC3NP = longword($0800);               //apture/Compare 3 Complementary output Polarity
  TIM_CCER_CC4E = longword($1000);                //apture/Compare 4 output enable
  TIM_CCER_CC4P = longword($2000);                //apture/Compare 4 output Polarity
  TIM_CCER_CC4NP = longword($8000);               //apture/Compare 4 Complementary output Polarity

//******************  Bit definition for TIM_CNT register  *******************
  TIM_CNT_CNT = longword($FFFF);                  //ounter Value

//******************  Bit definition for TIM_PSC register  *******************
  TIM_PSC_PSC = longword($FFFF);                  //rescaler Value

//******************  Bit definition for TIM_ARR register  *******************
  TIM_ARR_ARR = longword($FFFF);                  //ctual auto-reload Value

//******************  Bit definition for TIM_RCR register  *******************
  TIM_RCR_REP = longword($FF);                    //epetition Counter Value

//******************  Bit definition for TIM_CCR1 register  ******************
  TIM_CCR1_CCR1 = longword($FFFF);                //apture/Compare 1 Value

//******************  Bit definition for TIM_CCR2 register  ******************
  TIM_CCR2_CCR2 = longword($FFFF);                //apture/Compare 2 Value

//******************  Bit definition for TIM_CCR3 register  ******************
  TIM_CCR3_CCR3 = longword($FFFF);                //apture/Compare 3 Value

//******************  Bit definition for TIM_CCR4 register  ******************
  TIM_CCR4_CCR4 = longword($FFFF);                //apture/Compare 4 Value

//******************  Bit definition for TIM_BDTR register  ******************
  TIM_BDTR_DTG = longword($00FF);                 //TG[0:7] bits (Dead-Time Generator set-up)
  TIM_BDTR_DTG_0 = longword($0001);               //it 0
  TIM_BDTR_DTG_1 = longword($0002);               //it 1
  TIM_BDTR_DTG_2 = longword($0004);               //it 2
  TIM_BDTR_DTG_3 = longword($0008);               //it 3
  TIM_BDTR_DTG_4 = longword($0010);               //it 4
  TIM_BDTR_DTG_5 = longword($0020);               //it 5
  TIM_BDTR_DTG_6 = longword($0040);               //it 6
  TIM_BDTR_DTG_7 = longword($0080);               //it 7

  TIM_BDTR_LOCK = longword($0300);                //OCK[1:0] bits (Lock Configuration)
  TIM_BDTR_LOCK_0 = longword($0100);              //it 0
  TIM_BDTR_LOCK_1 = longword($0200);              //it 1

  TIM_BDTR_OSSI = longword($0400);                //ff-State Selection for Idle mode
  TIM_BDTR_OSSR = longword($0800);                //ff-State Selection for Run mode
  TIM_BDTR_BKE = longword($1000);                 //reak enable
  TIM_BDTR_BKP = longword($2000);                 //reak Polarity
  TIM_BDTR_AOE = longword($4000);                 //utomatic Output enable
  TIM_BDTR_MOE = longword($8000);                 //ain Output enable

//******************  Bit definition for TIM_DCR register  *******************
  TIM_DCR_DBA = longword($001F);                  //BA[4:0] bits (DMA Base Address)
  TIM_DCR_DBA_0 = longword($0001);                //it 0
  TIM_DCR_DBA_1 = longword($0002);                //it 1
  TIM_DCR_DBA_2 = longword($0004);                //it 2
  TIM_DCR_DBA_3 = longword($0008);                //it 3
  TIM_DCR_DBA_4 = longword($0010);                //it 4

  TIM_DCR_DBL = longword($1F00);                  //BL[4:0] bits (DMA Burst Length)
  TIM_DCR_DBL_0 = longword($0100);                //it 0
  TIM_DCR_DBL_1 = longword($0200);                //it 1
  TIM_DCR_DBL_2 = longword($0400);                //it 2
  TIM_DCR_DBL_3 = longword($0800);                //it 3
  TIM_DCR_DBL_4 = longword($1000);                //it 4

//******************  Bit definition for TIM_DMAR register  ******************
  TIM_DMAR_DMAB = longword($FFFF);                //MA register for burst accesses

//******************  Bit definition for TIM_OR register  ********************
  TIM14_OR_TI1_RMP = longword($0003);             //I1_RMP[1:0] bits (TIM14 Input 4 remap)
  TIM14_OR_TI1_RMP_0 = longword($0001);           //it 0
  TIM14_OR_TI1_RMP_1 = longword($0002);           //it 1

//****************************************************************************

//Universal Synchronous Asynchronous Receiver Transmitter (USART)

//****************************************************************************
//*****************  Bit definition for USART_CR1 register  ******************
  USART_CR1_UE = longword($00000001);             //USART Enable
  USART_CR1_UESM = longword($00000002);           //USART Enable in STOP Mode
  USART_CR1_RE = longword($00000004);             //Receiver Enable
  USART_CR1_TE = longword($00000008);             //Transmitter Enable
  USART_CR1_IDLEIE = longword($00000010);         //IDLE Interrupt Enable
  USART_CR1_RXNEIE = longword($00000020);         //RXNE Interrupt Enable
  USART_CR1_TCIE = longword($00000040);           //Transmission Complete Interrupt Enable
  USART_CR1_TXEIE = longword($00000080);          //TXE Interrupt Enable
  USART_CR1_PEIE = longword($00000100);           //PE Interrupt Enable
  USART_CR1_PS = longword($00000200);             //Parity Selection
  USART_CR1_PCE = longword($00000400);            //Parity Control Enable
  USART_CR1_WAKE = longword($00000800);           //Receiver Wakeup method
  USART_CR1_M = longword($00001000);              //Word length
  USART_CR1_MME = longword($00002000);            //Mute Mode Enable
  USART_CR1_CMIE = longword($00004000);           //Character match interrupt enable
  USART_CR1_OVER8 = longword($00008000);          //Oversampling by 8-bit or 16-bit mode
  USART_CR1_DEDT = longword($001F0000);           //DEDT[4:0] bits (Driver Enable Deassertion Time)
  USART_CR1_DEDT_0 = longword($00010000);         //Bit 0
  USART_CR1_DEDT_1 = longword($00020000);         //Bit 1
  USART_CR1_DEDT_2 = longword($00040000);         //Bit 2
  USART_CR1_DEDT_3 = longword($00080000);         //Bit 3
  USART_CR1_DEDT_4 = longword($00100000);         //Bit 4
  USART_CR1_DEAT = longword($03E00000);           //DEAT[4:0] bits (Driver Enable Assertion Time)
  USART_CR1_DEAT_0 = longword($00200000);         //Bit 0
  USART_CR1_DEAT_1 = longword($00400000);         //Bit 1
  USART_CR1_DEAT_2 = longword($00800000);         //Bit 2
  USART_CR1_DEAT_3 = longword($01000000);         //Bit 3
  USART_CR1_DEAT_4 = longword($02000000);         //Bit 4
  USART_CR1_RTOIE = longword($04000000);          //Receive Time Out interrupt enable
  USART_CR1_EOBIE = longword($08000000);          //End of Block interrupt enable

//*****************  Bit definition for USART_CR2 register  ******************
  USART_CR2_ADDM7 = longword($00000010);          //7-bit or 4-bit Address Detection
  USART_CR2_LBDL = longword($00000020);           //LIN Break Detection Length
  USART_CR2_LBDIE = longword($00000040);          //LIN Break Detection Interrupt Enable
  USART_CR2_LBCL = longword($00000100);           //Last Bit Clock pulse
  USART_CR2_CPHA = longword($00000200);           //Clock Phase
  USART_CR2_CPOL = longword($00000400);           //Clock Polarity
  USART_CR2_CLKEN = longword($00000800);          //Clock Enable
  USART_CR2_STOP = longword($00003000);           //STOP[1:0] bits (STOP bits)
  USART_CR2_STOP_0 = longword($00001000);         //Bit 0
  USART_CR2_STOP_1 = longword($00002000);         //Bit 1
  USART_CR2_LINEN = longword($00004000);          //LIN mode enable
  USART_CR2_SWAP = longword($00008000);           //SWAP TX/RX pins
  USART_CR2_RXINV = longword($00010000);          //RX pin active level inversion
  USART_CR2_TXINV = longword($00020000);          //TX pin active level inversion
  USART_CR2_DATAINV = longword($00040000);        //Binary data inversion
  USART_CR2_MSBFIRST = longword($00080000);       //Most Significant Bit First
  USART_CR2_ABREN = longword($00100000);          //Auto Baud-Rate Enable
  USART_CR2_ABRMODE = longword($00600000);        //ABRMOD[1:0] bits (Auto Baud-Rate Mode)
  USART_CR2_ABRMODE_0 = longword($00200000);      //Bit 0
  USART_CR2_ABRMODE_1 = longword($00400000);      //Bit 1
  USART_CR2_RTOEN = longword($00800000);          //Receiver Time-Out enable
  USART_CR2_ADD = longword($FF000000);            //Address of the USART node

//*****************  Bit definition for USART_CR3 register  ******************
  USART_CR3_EIE = longword($00000001);            //Error Interrupt Enable
  USART_CR3_IREN = longword($00000002);           //IrDA mode Enable
  USART_CR3_IRLP = longword($00000004);           //IrDA Low-Power
  USART_CR3_HDSEL = longword($00000008);          //Half-Duplex Selection
  USART_CR3_NACK = longword($00000010);           //SmartCard NACK enable
  USART_CR3_SCEN = longword($00000020);           //SmartCard mode enable
  USART_CR3_DMAR = longword($00000040);           //DMA Enable Receiver
  USART_CR3_DMAT = longword($00000080);           //DMA Enable Transmitter
  USART_CR3_RTSE = longword($00000100);           //RTS Enable
  USART_CR3_CTSE = longword($00000200);           //CTS Enable
  USART_CR3_CTSIE = longword($00000400);          //CTS Interrupt Enable
  USART_CR3_ONEBIT = longword($00000800);         //One sample bit method enable
  USART_CR3_OVRDIS = longword($00001000);         //Overrun Disable
  USART_CR3_DDRE = longword($00002000);           //DMA Disable on Reception Error
  USART_CR3_DEM = longword($00004000);            //Driver Enable Mode
  USART_CR3_DEP = longword($00008000);            //Driver Enable Polarity Selection
  USART_CR3_SCARCNT = longword($000E0000);        //SCARCNT[2:0] bits (SmartCard Auto-Retry Count)
  USART_CR3_SCARCNT_0 = longword($00020000);      //Bit 0
  USART_CR3_SCARCNT_1 = longword($00040000);      //Bit 1
  USART_CR3_SCARCNT_2 = longword($00080000);      //Bit 2
  USART_CR3_WUS = longword($00300000);            //WUS[1:0] bits (Wake UP Interrupt Flag Selection)
  USART_CR3_WUS_0 = longword($00100000);          //Bit 0
  USART_CR3_WUS_1 = longword($00200000);          //Bit 1
  USART_CR3_WUFIE = longword($00400000);          //Wake Up Interrupt Enable

//*****************  Bit definition for USART_BRR register  ******************
  USART_BRR_DIV_FRACTION = longword($000F);       //Fraction of USARTDIV
  USART_BRR_DIV_MANTISSA = longword($FFF0);       //Mantissa of USARTDIV

//*****************  Bit definition for USART_GTPR register  *****************
  USART_GTPR_PSC = longword($00FF);               //PSC[7:0] bits (Prescaler value)
  USART_GTPR_GT = longword($FF00);                //GT[7:0] bits (Guard time value)

//******************  Bit definition for USART_RTOR register  ****************
  USART_RTOR_RTO = longword($00FFFFFF);           //Receiver Time Out Value
  USART_RTOR_BLEN = longword($FF000000);          //Block Length

//******************  Bit definition for USART_RQR register  *****************
  USART_RQR_ABRRQ = longword($0001);              //Auto-Baud Rate Request
  USART_RQR_SBKRQ = longword($0002);              //Send Break Request
  USART_RQR_MMRQ = longword($0004);               //Mute Mode Request
  USART_RQR_RXFRQ = longword($0008);              //Receive Data flush Request
  USART_RQR_TXFRQ = longword($0010);              //Transmit data flush Request

//******************  Bit definition for USART_ISR register  *****************
  USART_ISR_PE = longword($00000001);             //Parity Error
  USART_ISR_FE = longword($00000002);             //Framing Error
  USART_ISR_NE = longword($00000004);             //Noise detected Flag
  USART_ISR_ORE = longword($00000008);            //OverRun Error
  USART_ISR_IDLE = longword($00000010);           //IDLE line detected
  USART_ISR_RXNE = longword($00000020);           //Read Data Register Not Empty
  USART_ISR_TC = longword($00000040);             //Transmission Complete
  USART_ISR_TXE = longword($00000080);            //Transmit Data Register Empty
  USART_ISR_LBD = longword($00000100);            //LIN Break Detection Flag
  USART_ISR_CTSIF = longword($00000200);          //CTS interrupt flag
  USART_ISR_CTS = longword($00000400);            //CTS flag
  USART_ISR_RTOF = longword($00000800);           //Receiver Time Out
  USART_ISR_EOBF = longword($00001000);           //End Of Block Flag
  USART_ISR_ABRE = longword($00004000);           //Auto-Baud Rate Error
  USART_ISR_ABRF = longword($00008000);           //Auto-Baud Rate Flag
  USART_ISR_BUSY = longword($00010000);           //Busy Flag
  USART_ISR_CMF = longword($00020000);            //Character Match Flag
  USART_ISR_SBKF = longword($00040000);           //Send Break Flag
  USART_ISR_RWU = longword($00080000);            //Receive Wake Up from mute mode Flag
  USART_ISR_WUF = longword($00100000);            //Wake Up from stop mode Flag
  USART_ISR_TEACK = longword($00200000);          //Transmit Enable Acknowledge Flag
  USART_ISR_REACK = longword($00400000);          //Receive Enable Acknowledge Flag

//******************  Bit definition for USART_ICR register  *****************
  USART_ICR_PECF = longword($00000001);           //Parity Error Clear Flag
  USART_ICR_FECF = longword($00000002);           //Framing Error Clear Flag
  USART_ICR_NCF = longword($00000004);            //Noise detected Clear Flag
  USART_ICR_ORECF = longword($00000008);          //OverRun Error Clear Flag
  USART_ICR_IDLECF = longword($00000010);         //IDLE line detected Clear Flag
  USART_ICR_TCCF = longword($00000040);           //Transmission Complete Clear Flag
  USART_ICR_LBDCF = longword($00000100);          //LIN Break Detection Clear Flag
  USART_ICR_CTSCF = longword($00000200);          //CTS Interrupt Clear Flag
  USART_ICR_RTOCF = longword($00000800);          //Receiver Time Out Clear Flag
  USART_ICR_EOBCF = longword($00001000);          //End Of Block Clear Flag
  USART_ICR_CMCF = longword($00020000);           //Character Match Clear Flag
  USART_ICR_WUCF = longword($00100000);           //Wake Up from stop mode Clear Flag

//******************  Bit definition for USART_RDR register  *****************
  USART_RDR_RDR = longword($01FF);                //RDR[8:0] bits (Receive Data value)

//******************  Bit definition for USART_TDR register  *****************
  USART_TDR_TDR = longword($01FF);                //TDR[8:0] bits (Transmit Data value)

//****************************************************************************

//Window WATCHDOG (WWDG)

//****************************************************************************

//******************  Bit definition for WWDG_CR register  *******************
  WWDG_CR_T = longword($7F);                      //T[6:0] bits (7-Bit counter (MSB to LSB))
  WWDG_CR_T0 = longword($01);                     //Bit 0
  WWDG_CR_T1 = longword($02);                     //Bit 1
  WWDG_CR_T2 = longword($04);                     //Bit 2
  WWDG_CR_T3 = longword($08);                     //Bit 3
  WWDG_CR_T4 = longword($10);                     //Bit 4
  WWDG_CR_T5 = longword($20);                     //Bit 5
  WWDG_CR_T6 = longword($40);                     //Bit 6

  WWDG_CR_WDGA = longword($80);                   //Activation bit

//******************  Bit definition for WWDG_CFR register  ******************
  WWDG_CFR_W = longword($007F);                   //W[6:0] bits (7-bit window value)
  WWDG_CFR_W0 = longword($0001);                  //Bit 0
  WWDG_CFR_W1 = longword($0002);                  //Bit 1
  WWDG_CFR_W2 = longword($0004);                  //Bit 2
  WWDG_CFR_W3 = longword($0008);                  //Bit 3
  WWDG_CFR_W4 = longword($0010);                  //Bit 4
  WWDG_CFR_W5 = longword($0020);                  //Bit 5
  WWDG_CFR_W6 = longword($0040);                  //Bit 6

  WWDG_CFR_WDGTB = longword($0180);               //WDGTB[1:0] bits (Timer Base)
  WWDG_CFR_WDGTB0 = longword($0080);              //Bit 0
  WWDG_CFR_WDGTB1 = longword($0100);              //Bit 1

  WWDG_CFR_EWI = longword($0200);                 //Early Wakeup Interrupt

//******************  Bit definition for WWDG_SR register  *******************
  WWDG_SR_EWIF = longword($01);                   //Early Wakeup Interrupt Flag

//*********************** (C) COPYRIGHT STMicroelectronics *****END OF FILE***
implementation
  procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
  procedure HardFault_interrupt; external name 'HardFault_interrupt';
  procedure SVC_interrupt; external name 'SVC_interrupt';
  procedure PendSV_interrupt; external name 'PendSV_interrupt';
  procedure SysTick_interrupt; external name 'SysTick_interrupt';
  procedure WWDG_interrupt; external name 'WWDG_interrupt';
  procedure PVD_interrupt; external name 'PVD_interrupt';
  procedure RTC_interrupt; external name 'RTC_interrupt';
  procedure FLASH_interrupt; external name 'FLASH_interrupt';
  procedure RCC_interrupt; external name 'RCC_interrupt';
  procedure EXTI0_1_interrupt; external name 'EXTI0_1_interrupt';
  procedure EXTI2_3_interrupt; external name 'EXTI2_3_interrupt';
  procedure EXTI4_15_interrupt; external name 'EXTI4_15_interrupt';
  procedure TS_interrupt; external name 'TS_interrupt';
  procedure DMA1_Channel1_interrupt; external name 'DMA1_Channel1_interrupt';
  procedure DMA1_Channel2_3_interrupt; external name 'DMA1_Channel2_3_interrupt';
  procedure DMA1_Channel4_5_interrupt; external name 'DMA1_Channel4_5_interrupt';
  procedure ADC1_COMP_interrupt; external name 'ADC1_COMP_interrupt';
  procedure TIM1_BRK_UP_TRG_COM_interrupt; external name 'TIM1_BRK_UP_TRG_COM_interrupt';
  procedure TIM1_CC_interrupt; external name 'TIM1_CC_interrupt';
  procedure TIM2_interrupt; external name 'TIM2_interrupt';
  procedure TIM3_interrupt; external name 'TIM3_interrupt';
  procedure TIM6_DAC_interrupt; external name 'TIM6_DAC_interrupt';
  procedure TIM14_interrupt; external name 'TIM14_interrupt';
  procedure TIM15_interrupt; external name 'TIM15_interrupt';
  procedure TIM16_interrupt; external name 'TIM16_interrupt';
  procedure TIM17_interrupt; external name 'TIM17_interrupt';
  procedure I2C1_interrupt; external name 'I2C1_interrupt';
  procedure I2C2_interrupt; external name 'I2C2_interrupt';
  procedure SPI1_interrupt; external name 'SPI1_interrupt';
  procedure SPI2_interrupt; external name 'SPI2_interrupt';
  procedure USART1_interrupt; external name 'USART1_interrupt';
  procedure USART2_interrupt; external name 'USART2_interrupt';
  procedure CEC_interrupt; external name 'CEC_interrupt';

  {$i cortexm3_start.inc}

  procedure Vectors; assembler; nostackframe;
  label interrupt_vectors;
  asm
     .section ".init.interrupt_vectors"
  interrupt_vectors:
     .long _stack_top
     .long Startup
     .long NonMaskableInt_interrupt
     .long HardFault_interrupt
     .long 0
     .long 0
     .long 0
     .long 0
     .long 0
     .long 0
     .long 0
     .long SVC_interrupt
     .long 0
     .long 0
     .long PendSV_interrupt
     .long SysTick_interrupt
     .long WWDG_interrupt
     .long PVD_interrupt
     .long RTC_interrupt
     .long FLASH_interrupt
     .long RCC_interrupt
     .long EXTI0_1_interrupt
     .long EXTI2_3_interrupt
     .long EXTI4_15_interrupt
     .long TS_interrupt
     .long DMA1_Channel1_interrupt
     .long DMA1_Channel2_3_interrupt
     .long DMA1_Channel4_5_interrupt
     .long ADC1_COMP_interrupt
     .long TIM1_BRK_UP_TRG_COM_interrupt
     .long TIM1_CC_interrupt
     .long TIM2_interrupt
     .long TIM3_interrupt
     .long TIM6_DAC_interrupt
     .long 0
     .long TIM14_interrupt
     .long TIM15_interrupt
     .long TIM16_interrupt
     .long TIM17_interrupt
     .long I2C1_interrupt
     .long I2C2_interrupt
     .long SPI1_interrupt
     .long SPI2_interrupt
     .long USART1_interrupt
     .long USART2_interrupt
     .long 0
     .long CEC_interrupt
     .long 0

    .weak NonMaskableInt_interrupt
    .weak HardFault_interrupt
    .weak SVC_interrupt
    .weak PendSV_interrupt
    .weak SysTick_interrupt
    .weak WWDG_interrupt
    .weak PVD_interrupt
    .weak RTC_interrupt
    .weak FLASH_interrupt
    .weak RCC_interrupt
    .weak EXTI0_1_interrupt
    .weak EXTI2_3_interrupt
    .weak EXTI4_15_interrupt
    .weak TS_interrupt
    .weak DMA1_Channel1_interrupt
    .weak DMA1_Channel2_3_interrupt
    .weak DMA1_Channel4_5_interrupt
    .weak ADC1_COMP_interrupt
    .weak TIM1_BRK_UP_TRG_COM_interrupt
    .weak TIM1_CC_interrupt
    .weak TIM2_interrupt
    .weak TIM3_interrupt
    .weak TIM6_DAC_interrupt
    .weak TIM14_interrupt
    .weak TIM15_interrupt
    .weak TIM16_interrupt
    .weak TIM17_interrupt
    .weak I2C1_interrupt
    .weak I2C2_interrupt
    .weak SPI1_interrupt
    .weak SPI2_interrupt
    .weak USART1_interrupt
    .weak USART2_interrupt
    .weak CEC_interrupt

    .set NonMaskableInt_interrupt, HaltProc
    .set HardFault_interrupt, HaltProc
    .set SVC_interrupt, HaltProc
    .set PendSV_interrupt, HaltProc
    .set SysTick_interrupt, HaltProc
    .set WWDG_interrupt, HaltProc
    .set PVD_interrupt, HaltProc
    .set RTC_interrupt, HaltProc
    .set FLASH_interrupt, HaltProc
    .set RCC_interrupt, HaltProc
    .set EXTI0_1_interrupt, HaltProc
    .set EXTI2_3_interrupt, HaltProc
    .set EXTI4_15_interrupt, HaltProc
    .set TS_interrupt, HaltProc
    .set DMA1_Channel1_interrupt, HaltProc
    .set DMA1_Channel2_3_interrupt, HaltProc
    .set DMA1_Channel4_5_interrupt, HaltProc
    .set ADC1_COMP_interrupt, HaltProc
    .set TIM1_BRK_UP_TRG_COM_interrupt, HaltProc
    .set TIM1_CC_interrupt, HaltProc
    .set TIM2_interrupt, HaltProc
    .set TIM3_interrupt, HaltProc
    .set TIM6_DAC_interrupt, HaltProc
    .set TIM14_interrupt, HaltProc
    .set TIM15_interrupt, HaltProc
    .set TIM16_interrupt, HaltProc
    .set TIM17_interrupt, HaltProc
    .set I2C1_interrupt, HaltProc
    .set I2C2_interrupt, HaltProc
    .set SPI1_interrupt, HaltProc
    .set SPI2_interrupt, HaltProc
    .set USART1_interrupt, HaltProc
    .set USART2_interrupt, HaltProc
    .set CEC_interrupt, HaltProc
    .text
  end;
end.

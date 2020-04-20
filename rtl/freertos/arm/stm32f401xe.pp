unit stm32f401xe;
interface
{$PACKRECORDS C}
{$GOTO ON}
{$SCOPEDENUMS ON}

type
  TIRQn_Enum = (
    NonMaskableInt_IRQn = -14,        
    MemoryManagement_IRQn = -12,      
    BusFault_IRQn = -11,              
    UsageFault_IRQn = -10,            
    SVCall_IRQn = -5,                 
    DebugMonitor_IRQn = -4,           
    PendSV_IRQn = -2,                 
    SysTick_IRQn = -1,                
    WWDG_IRQn   = 0,                  
    PVD_IRQn    = 1,                  
    TAMP_STAMP_IRQn = 2,              
    RTC_WKUP_IRQn = 3,                
    FLASH_IRQn  = 4,                  
    RCC_IRQn    = 5,                  
    EXTI0_IRQn  = 6,                  
    EXTI1_IRQn  = 7,                  
    EXTI2_IRQn  = 8,                  
    EXTI3_IRQn  = 9,                  
    EXTI4_IRQn  = 10,                 
    DMA1_Stream0_IRQn = 11,           
    DMA1_Stream1_IRQn = 12,           
    DMA1_Stream2_IRQn = 13,           
    DMA1_Stream3_IRQn = 14,           
    DMA1_Stream4_IRQn = 15,           
    DMA1_Stream5_IRQn = 16,           
    DMA1_Stream6_IRQn = 17,           
    ADC_IRQn    = 18,                 
    EXTI9_5_IRQn = 23,                
    TIM1_BRK_TIM9_IRQn = 24,          
    TIM1_UP_TIM10_IRQn = 25,          
    TIM1_TRG_COM_TIM11_IRQn = 26,     
    TIM1_CC_IRQn = 27,                
    TIM2_IRQn   = 28,                 
    TIM3_IRQn   = 29,                 
    TIM4_IRQn   = 30,                 
    I2C1_EV_IRQn = 31,                
    I2C1_ER_IRQn = 32,                
    I2C2_EV_IRQn = 33,                
    I2C2_ER_IRQn = 34,                
    SPI1_IRQn   = 35,                 
    SPI2_IRQn   = 36,                 
    USART1_IRQn = 37,                 
    USART2_IRQn = 38,                 
    EXTI15_10_IRQn = 40,              
    RTC_Alarm_IRQn = 41,              
    OTG_FS_WKUP_IRQn = 42,            
    DMA1_Stream7_IRQn = 47,           
    SDIO_IRQn   = 49,                 
    TIM5_IRQn   = 50,                 
    SPI3_IRQn   = 51,                 
    DMA2_Stream0_IRQn = 56,           
    DMA2_Stream1_IRQn = 57,           
    DMA2_Stream2_IRQn = 58,           
    DMA2_Stream3_IRQn = 59,           
    DMA2_Stream4_IRQn = 60,           
    OTG_FS_IRQn = 67,                 
    DMA2_Stream5_IRQn = 68,           
    DMA2_Stream6_IRQn = 69,           
    DMA2_Stream7_IRQn = 70,           
    USART6_IRQn = 71,                 
    I2C3_EV_IRQn = 72,                
    I2C3_ER_IRQn = 73,                
    FPU_IRQn    = 81,                 
    SPI4_IRQn   = 84                  
  );

  TADC_Registers = record
    SR          : longword;
    CR1         : longword;
    CR2         : longword;
    SMPR1       : longword;
    SMPR2       : longword;
    JOFR1       : longword;
    JOFR2       : longword;
    JOFR3       : longword;
    JOFR4       : longword;
    HTR         : longword;
    LTR         : longword;
    SQR1        : longword;
    SQR2        : longword;
    SQR3        : longword;
    JSQR        : longword;
    JDR1        : longword;
    JDR2        : longword;
    JDR3        : longword;
    JDR4        : longword;
    DR          : longword;
  end;

  TADC_Common_Registers = record
    CSR         : longword;
    CCR         : longword;
    CDR         : longword;
  end;

  TCRC_Registers = record
    DR          : longword;
    IDR         : byte;
    RESERVED0   : byte;
    RESERVED1   : word;
    CR          : longword;
  end;

  TDBGMCU_Registers = record
    IDCODE      : longword;
    CR          : longword;
    APB1FZ      : longword;
    APB2FZ      : longword;
  end;

  TDMA_Stream_Registers = record
    CR          : longword;
    NDTR        : longword;
    PAR         : longword;
    M0AR        : longword;
    M1AR        : longword;
    FCR         : longword;
  end;

  TDMA_Registers = record
    LISR        : longword;
    HISR        : longword;
    LIFCR       : longword;
    HIFCR       : longword;
  end;

  TEXTI_Registers = record
    IMR         : longword;
    EMR         : longword;
    RTSR        : longword;
    FTSR        : longword;
    SWIER       : longword;
    PR          : longword;
  end;

  TFLASH_Registers = record
    ACR         : longword;
    KEYR        : longword;
    OPTKEYR     : longword;
    SR          : longword;
    CR          : longword;
    OPTCR       : longword;
    OPTCR1      : longword;
  end;

  TGPIO_Registers = record
    MODER       : longword;
    OTYPER      : longword;
    OSPEEDR     : longword;
    PUPDR       : longword;
    IDR         : longword;
    ODR         : longword;
    BSRR        : longword;
    LCKR        : longword;
    AFR         : array[0..1] of longword;
  end;

  TSYSCFG_Registers = record
    MEMRMP      : longword;
    PMC         : longword;
    EXTICR      : array[0..3] of longword;
    RESERVED    : array[0..1] of longword;
    CMPCR       : longword;
  end;

  TI2C_Registers = record
    CR1         : longword;
    CR2         : longword;
    OAR1        : longword;
    OAR2        : longword;
    DR          : longword;
    SR1         : longword;
    SR2         : longword;
    CCR         : longword;
    TRISE       : longword;
    FLTR        : longword;
  end;

  TIWDG_Registers = record
    KR          : longword;
    PR          : longword;
    RLR         : longword;
    SR          : longword;
  end;

  TPWR_Registers = record
    CR          : longword;
    CSR         : longword;
  end;

  TRCC_Registers = record
    CR          : longword;
    PLLCFGR     : longword;
    CFGR        : longword;
    CIR         : longword;
    AHB1RSTR    : longword;
    AHB2RSTR    : longword;
    AHB3RSTR    : longword;
    RESERVED0   : longword;
    APB1RSTR    : longword;
    APB2RSTR    : longword;
    RESERVED1   : array[0..1] of longword;
    AHB1ENR     : longword;
    AHB2ENR     : longword;
    AHB3ENR     : longword;
    RESERVED2   : longword;
    APB1ENR     : longword;
    APB2ENR     : longword;
    RESERVED3   : array[0..1] of longword;
    AHB1LPENR   : longword;
    AHB2LPENR   : longword;
    AHB3LPENR   : longword;
    RESERVED4   : longword;
    APB1LPENR   : longword;
    APB2LPENR   : longword;
    RESERVED5   : array[0..1] of longword;
    BDCR        : longword;
    CSR         : longword;
    RESERVED6   : array[0..1] of longword;
    SSCGR       : longword;
    PLLI2SCFGR  : longword;
    RESERVED7   : longword;
    DCKCFGR     : longword;
  end;

  TRTC_Registers = record
    TR          : longword;
    DR          : longword;
    CR          : longword;
    ISR         : longword;
    PRER        : longword;
    WUTR        : longword;
    CALIBR      : longword;
    ALRMAR      : longword;
    ALRMBR      : longword;
    WPR         : longword;
    SSR         : longword;
    SHIFTR      : longword;
    TSTR        : longword;
    TSDR        : longword;
    TSSSR       : longword;
    CALR        : longword;
    TAFCR       : longword;
    ALRMASSR    : longword;
    ALRMBSSR    : longword;
    RESERVED7   : longword;
    BKP0R       : longword;
    BKP1R       : longword;
    BKP2R       : longword;
    BKP3R       : longword;
    BKP4R       : longword;
    BKP5R       : longword;
    BKP6R       : longword;
    BKP7R       : longword;
    BKP8R       : longword;
    BKP9R       : longword;
    BKP10R      : longword;
    BKP11R      : longword;
    BKP12R      : longword;
    BKP13R      : longword;
    BKP14R      : longword;
    BKP15R      : longword;
    BKP16R      : longword;
    BKP17R      : longword;
    BKP18R      : longword;
    BKP19R      : longword;
  end;

  TSDIO_Registers = record
    POWER       : longword;
    CLKCR       : longword;
    ARG         : longword;
    CMD         : longword;
    RESPCMD     : longword;
    RESP1       : longword;
    RESP2       : longword;
    RESP3       : longword;
    RESP4       : longword;
    DTIMER      : longword;
    DLEN        : longword;
    DCTRL       : longword;
    DCOUNT      : longword;
    STA         : longword;
    ICR         : longword;
    MASK        : longword;
    RESERVED0   : array[0..1] of longword;
    FIFOCNT     : longword;
    RESERVED1   : array[0..12] of longword;
    FIFO        : longword;
  end;

  TSPI_Registers = record
    CR1         : longword;
    CR2         : longword;
    SR          : longword;
    DR          : longword;
    CRCPR       : longword;
    RXCRCR      : longword;
    TXCRCR      : longword;
    I2SCFGR     : longword;
    I2SPR       : longword;
  end;

  TTIM_Registers = record
    CR1         : longword;
    CR2         : longword;
    SMCR        : longword;
    DIER        : longword;
    SR          : longword;
    EGR         : longword;
    CCMR1       : longword;
    CCMR2       : longword;
    CCER        : longword;
    CNT         : longword;
    PSC         : longword;
    ARR         : longword;
    RCR         : longword;
    CCR1        : longword;
    CCR2        : longword;
    CCR3        : longword;
    CCR4        : longword;
    BDTR        : longword;
    DCR         : longword;
    DMAR        : longword;
    &OR         : longword;
  end;

  TUSART_Registers = record
    SR          : longword;
    DR          : longword;
    BRR         : longword;
    CR1         : longword;
    CR2         : longword;
    CR3         : longword;
    GTPR        : longword;
  end;

  TWWDG_Registers = record
    CR          : longword;
    CFR         : longword;
    SR          : longword;
  end;

  TUSB_OTG_Global_Registers = record
    GOTGCTL     : longword;
    GOTGINT     : longword;
    GAHBCFG     : longword;
    GUSBCFG     : longword;
    GRSTCTL     : longword;
    GINTSTS     : longword;
    GINTMSK     : longword;
    GRXSTSR     : longword;
    GRXSTSP     : longword;
    GRXFSIZ     : longword;
    DIEPTXF0_HNPTXFSIZ : longword;
    HNPTXSTS    : longword;
    Reserved30  : array[0..1] of longword;
    GCCFG       : longword;
    CID         : longword;
    Reserved40  : array[0..47] of longword;
    HPTXFSIZ    : longword;
    DIEPTXF     : array[0..14] of longword;
  end;

  TUSB_OTG_Device_Registers = record
    DCFG        : longword;
    DCTL        : longword;
    DSTS        : longword;
    Reserved0C  : longword;
    DIEPMSK     : longword;
    DOEPMSK     : longword;
    DAINT       : longword;
    DAINTMSK    : longword;
    Reserved20  : longword;
    Reserved9   : longword;
    DVBUSDIS    : longword;
    DVBUSPULSE  : longword;
    DTHRCTL     : longword;
    DIEPEMPMSK  : longword;
    DEACHINT    : longword;
    DEACHMSK    : longword;
    Reserved40  : longword;
    DINEP1MSK   : longword;
    Reserved44  : array[0..14] of longword;
    DOUTEP1MSK  : longword;
  end;

  TUSB_OTG_INEndpoint_Registers = record
    DIEPCTL     : longword;
    Reserved04  : longword;
    DIEPINT     : longword;
    Reserved0C  : longword;
    DIEPTSIZ    : longword;
    DIEPDMA     : longword;
    DTXFSTS     : longword;
    Reserved18  : longword;
  end;

  TUSB_OTG_OUTEndpoint_Registers = record
    DOEPCTL     : longword;
    Reserved04  : longword;
    DOEPINT     : longword;
    Reserved0C  : longword;
    DOEPTSIZ    : longword;
    DOEPDMA     : longword;
    Reserved18  : array[0..1] of longword;
  end;

  TUSB_OTG_Host_Registers = record
    HCFG        : longword;
    HFIR        : longword;
    HFNUM       : longword;
    Reserved40C : longword;
    HPTXSTS     : longword;
    HAINT       : longword;
    HAINTMSK    : longword;
  end;

  TUSB_OTG_HostChannel_Registers = record
    HCCHAR      : longword;
    HCSPLT      : longword;
    HCINT       : longword;
    HCINTMSK    : longword;
    HCTSIZ      : longword;
    HCDMA       : longword;
    Reserved    : array[0..1] of longword;
  end;

const
  FLASH_BASE    = $08000000;
  SRAM1_BASE    = $20000000;
  PERIPH_BASE   = $40000000;
  SRAM1_BB_BASE = $22000000;
  PERIPH_BB_BASE= $42000000;
  BKPSRAM_BB_BASE= $42480000;
  FLASH_OTP_BASE= $1FFF7800;
  SRAM_BASE     = SRAM1_BASE;
  SRAM_BB_BASE  = SRAM1_BB_BASE;
  APB1PERIPH_BASE= PERIPH_BASE;
  APB2PERIPH_BASE= PERIPH_BASE + $00010000;
  AHB1PERIPH_BASE= PERIPH_BASE + $00020000;
  AHB2PERIPH_BASE= PERIPH_BASE + $10000000;
  TIM2_BASE     = APB1PERIPH_BASE + $0000;
  TIM3_BASE     = APB1PERIPH_BASE + $0400;
  TIM4_BASE     = APB1PERIPH_BASE + $0800;
  TIM5_BASE     = APB1PERIPH_BASE + $0C00;
  RTC_BASE      = APB1PERIPH_BASE + $2800;
  WWDG_BASE     = APB1PERIPH_BASE + $2C00;
  IWDG_BASE     = APB1PERIPH_BASE + $3000;
  I2S2ext_BASE  = APB1PERIPH_BASE + $3400;
  SPI2_BASE     = APB1PERIPH_BASE + $3800;
  SPI3_BASE     = APB1PERIPH_BASE + $3C00;
  I2S3ext_BASE  = APB1PERIPH_BASE + $4000;
  USART2_BASE   = APB1PERIPH_BASE + $4400;
  I2C1_BASE     = APB1PERIPH_BASE + $5400;
  I2C2_BASE     = APB1PERIPH_BASE + $5800;
  I2C3_BASE     = APB1PERIPH_BASE + $5C00;
  PWR_BASE      = APB1PERIPH_BASE + $7000;
  TIM1_BASE     = APB2PERIPH_BASE + $0000;
  USART1_BASE   = APB2PERIPH_BASE + $1000;
  USART6_BASE   = APB2PERIPH_BASE + $1400;
  ADC1_BASE     = APB2PERIPH_BASE + $2000;
  ADC1_COMMON_BASE= APB2PERIPH_BASE + $2300;
  ADC_BASE      = ADC1_COMMON_BASE;
  SDIO_BASE     = APB2PERIPH_BASE + $2C00;
  SPI1_BASE     = APB2PERIPH_BASE + $3000;
  SPI4_BASE     = APB2PERIPH_BASE + $3400;
  SYSCFG_BASE   = APB2PERIPH_BASE + $3800;
  EXTI_BASE     = APB2PERIPH_BASE + $3C00;
  TIM9_BASE     = APB2PERIPH_BASE + $4000;
  TIM10_BASE    = APB2PERIPH_BASE + $4400;
  TIM11_BASE    = APB2PERIPH_BASE + $4800;
  GPIOA_BASE    = AHB1PERIPH_BASE + $0000;
  GPIOB_BASE    = AHB1PERIPH_BASE + $0400;
  GPIOC_BASE    = AHB1PERIPH_BASE + $0800;
  GPIOD_BASE    = AHB1PERIPH_BASE + $0C00;
  GPIOE_BASE    = AHB1PERIPH_BASE + $1000;
  GPIOH_BASE    = AHB1PERIPH_BASE + $1C00;
  CRC_BASE      = AHB1PERIPH_BASE + $3000;
  RCC_BASE      = AHB1PERIPH_BASE + $3800;
  FLASH_R_BASE  = AHB1PERIPH_BASE + $3C00;
  DMA1_BASE     = AHB1PERIPH_BASE + $6000;
  DMA1_Stream0_BASE= DMA1_BASE + $010;
  DMA1_Stream1_BASE= DMA1_BASE + $028;
  DMA1_Stream2_BASE= DMA1_BASE + $040;
  DMA1_Stream3_BASE= DMA1_BASE + $058;
  DMA1_Stream4_BASE= DMA1_BASE + $070;
  DMA1_Stream5_BASE= DMA1_BASE + $088;
  DMA1_Stream6_BASE= DMA1_BASE + $0A0;
  DMA1_Stream7_BASE= DMA1_BASE + $0B8;
  DMA2_BASE     = AHB1PERIPH_BASE + $6400;
  DMA2_Stream0_BASE= DMA2_BASE + $010;
  DMA2_Stream1_BASE= DMA2_BASE + $028;
  DMA2_Stream2_BASE= DMA2_BASE + $040;
  DMA2_Stream3_BASE= DMA2_BASE + $058;
  DMA2_Stream4_BASE= DMA2_BASE + $070;
  DMA2_Stream5_BASE= DMA2_BASE + $088;
  DMA2_Stream6_BASE= DMA2_BASE + $0A0;
  DMA2_Stream7_BASE= DMA2_BASE + $0B8;
  DBGMCU_BASE   = $E0042000;
  USB_OTG_FS_PERIPH_BASE= $50000000;
  USB_OTG_GLOBAL_BASE= $000;
  USB_OTG_DEVICE_BASE= $800;
  USB_OTG_IN_ENDPOINT_BASE= $900;
  USB_OTG_OUT_ENDPOINT_BASE= $B00;
  USB_OTG_HOST_BASE= $400;
  USB_OTG_HOST_PORT_BASE= $440;
  USB_OTG_HOST_CHANNEL_BASE= $500;
  USB_OTG_PCGCCTL_BASE= $E00;
  USB_OTG_FIFO_BASE= $1000;
  UID_BASE      = $1FFF7A10;
  FLASHSIZE_BASE= $1FFF7A22;
  PACKAGE_BASE  = $1FFF7BF0;

var
  TIM2          : TTIM_Registers absolute TIM2_BASE;
  TIM3          : TTIM_Registers absolute TIM3_BASE;
  TIM4          : TTIM_Registers absolute TIM4_BASE;
  TIM5          : TTIM_Registers absolute TIM5_BASE;
  RTC           : TRTC_Registers absolute RTC_BASE;
  WWDG          : TWWDG_Registers absolute WWDG_BASE;
  IWDG          : TIWDG_Registers absolute IWDG_BASE;
  I2S2ext       : TSPI_Registers absolute I2S2ext_BASE;
  SPI2          : TSPI_Registers absolute SPI2_BASE;
  SPI3          : TSPI_Registers absolute SPI3_BASE;
  I2S3ext       : TSPI_Registers absolute I2S3ext_BASE;
  USART2        : TUSART_Registers absolute USART2_BASE;
  I2C1          : TI2C_Registers absolute I2C1_BASE;
  I2C2          : TI2C_Registers absolute I2C2_BASE;
  I2C3          : TI2C_Registers absolute I2C3_BASE;
  PWR           : TPWR_Registers absolute PWR_BASE;
  TIM1          : TTIM_Registers absolute TIM1_BASE;
  USART1        : TUSART_Registers absolute USART1_BASE;
  USART6        : TUSART_Registers absolute USART6_BASE;
  ADC1          : TADC_Registers absolute ADC1_BASE;
  ADC1_COMMON   : TADC_Common_Registers absolute ADC1_COMMON_BASE;
  SDIO          : TSDIO_Registers absolute SDIO_BASE;
  SPI1          : TSPI_Registers absolute SPI1_BASE;
  SPI4          : TSPI_Registers absolute SPI4_BASE;
  SYSCFG        : TSYSCFG_Registers absolute SYSCFG_BASE;
  EXTI          : TEXTI_Registers absolute EXTI_BASE;
  TIM9          : TTIM_Registers absolute TIM9_BASE;
  TIM10         : TTIM_Registers absolute TIM10_BASE;
  TIM11         : TTIM_Registers absolute TIM11_BASE;
  GPIOA         : TGPIO_Registers absolute GPIOA_BASE;
  GPIOB         : TGPIO_Registers absolute GPIOB_BASE;
  GPIOC         : TGPIO_Registers absolute GPIOC_BASE;
  GPIOD         : TGPIO_Registers absolute GPIOD_BASE;
  GPIOE         : TGPIO_Registers absolute GPIOE_BASE;
  GPIOH         : TGPIO_Registers absolute GPIOH_BASE;
  CRC           : TCRC_Registers absolute CRC_BASE;
  RCC           : TRCC_Registers absolute RCC_BASE;
  FLASH         : TFLASH_Registers absolute FLASH_R_BASE;
  DMA1          : TDMA_Registers absolute DMA1_BASE;
  DMA1_Stream0  : TDMA_Stream_Registers absolute DMA1_Stream0_BASE;
  DMA1_Stream1  : TDMA_Stream_Registers absolute DMA1_Stream1_BASE;
  DMA1_Stream2  : TDMA_Stream_Registers absolute DMA1_Stream2_BASE;
  DMA1_Stream3  : TDMA_Stream_Registers absolute DMA1_Stream3_BASE;
  DMA1_Stream4  : TDMA_Stream_Registers absolute DMA1_Stream4_BASE;
  DMA1_Stream5  : TDMA_Stream_Registers absolute DMA1_Stream5_BASE;
  DMA1_Stream6  : TDMA_Stream_Registers absolute DMA1_Stream6_BASE;
  DMA1_Stream7  : TDMA_Stream_Registers absolute DMA1_Stream7_BASE;
  DMA2          : TDMA_Registers absolute DMA2_BASE;
  DMA2_Stream0  : TDMA_Stream_Registers absolute DMA2_Stream0_BASE;
  DMA2_Stream1  : TDMA_Stream_Registers absolute DMA2_Stream1_BASE;
  DMA2_Stream2  : TDMA_Stream_Registers absolute DMA2_Stream2_BASE;
  DMA2_Stream3  : TDMA_Stream_Registers absolute DMA2_Stream3_BASE;
  DMA2_Stream4  : TDMA_Stream_Registers absolute DMA2_Stream4_BASE;
  DMA2_Stream5  : TDMA_Stream_Registers absolute DMA2_Stream5_BASE;
  DMA2_Stream6  : TDMA_Stream_Registers absolute DMA2_Stream6_BASE;
  DMA2_Stream7  : TDMA_Stream_Registers absolute DMA2_Stream7_BASE;
  DBGMCU        : TDBGMCU_Registers absolute DBGMCU_BASE;

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
procedure EXTI15_10_interrupt; external name 'EXTI15_10_interrupt';
procedure RTC_Alarm_interrupt; external name 'RTC_Alarm_interrupt';
procedure OTG_FS_WKUP_interrupt; external name 'OTG_FS_WKUP_interrupt';
procedure DMA1_Stream7_interrupt; external name 'DMA1_Stream7_interrupt';
procedure SDIO_interrupt; external name 'SDIO_interrupt';
procedure TIM5_interrupt; external name 'TIM5_interrupt';
procedure SPI3_interrupt; external name 'SPI3_interrupt';
procedure DMA2_Stream0_interrupt; external name 'DMA2_Stream0_interrupt';
procedure DMA2_Stream1_interrupt; external name 'DMA2_Stream1_interrupt';
procedure DMA2_Stream2_interrupt; external name 'DMA2_Stream2_interrupt';
procedure DMA2_Stream3_interrupt; external name 'DMA2_Stream3_interrupt';
procedure DMA2_Stream4_interrupt; external name 'DMA2_Stream4_interrupt';
procedure OTG_FS_interrupt; external name 'OTG_FS_interrupt';
procedure DMA2_Stream5_interrupt; external name 'DMA2_Stream5_interrupt';
procedure DMA2_Stream6_interrupt; external name 'DMA2_Stream6_interrupt';
procedure DMA2_Stream7_interrupt; external name 'DMA2_Stream7_interrupt';
procedure USART6_interrupt; external name 'USART6_interrupt';
procedure I2C3_EV_interrupt; external name 'I2C3_EV_interrupt';
procedure I2C3_ER_interrupt; external name 'I2C3_ER_interrupt';
procedure FPU_interrupt; external name 'FPU_interrupt';
procedure SPI4_interrupt; external name 'SPI4_interrupt';


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
  .long 0
  .long 0
  .long 0
  .long 0
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
  .long 0
  .long EXTI15_10_interrupt
  .long RTC_Alarm_interrupt
  .long OTG_FS_WKUP_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long DMA1_Stream7_interrupt
  .long 0
  .long SDIO_interrupt
  .long TIM5_interrupt
  .long SPI3_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long DMA2_Stream0_interrupt
  .long DMA2_Stream1_interrupt
  .long DMA2_Stream2_interrupt
  .long DMA2_Stream3_interrupt
  .long DMA2_Stream4_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long OTG_FS_interrupt
  .long DMA2_Stream5_interrupt
  .long DMA2_Stream6_interrupt
  .long DMA2_Stream7_interrupt
  .long USART6_interrupt
  .long I2C3_EV_interrupt
  .long I2C3_ER_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long FPU_interrupt
  .long 0
  .long 0
  .long SPI4_interrupt

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
  .weak EXTI15_10_interrupt
  .weak RTC_Alarm_interrupt
  .weak OTG_FS_WKUP_interrupt
  .weak DMA1_Stream7_interrupt
  .weak SDIO_interrupt
  .weak TIM5_interrupt
  .weak SPI3_interrupt
  .weak DMA2_Stream0_interrupt
  .weak DMA2_Stream1_interrupt
  .weak DMA2_Stream2_interrupt
  .weak DMA2_Stream3_interrupt
  .weak DMA2_Stream4_interrupt
  .weak OTG_FS_interrupt
  .weak DMA2_Stream5_interrupt
  .weak DMA2_Stream6_interrupt
  .weak DMA2_Stream7_interrupt
  .weak USART6_interrupt
  .weak I2C3_EV_interrupt
  .weak I2C3_ER_interrupt
  .weak FPU_interrupt
  .weak SPI4_interrupt

  .set NonMaskableInt_interrupt, Haltproc
  .set MemoryManagement_interrupt, Haltproc
  .set BusFault_interrupt, Haltproc
  .set UsageFault_interrupt, Haltproc
  .set SVCall_interrupt, Haltproc
  .set DebugMonitor_interrupt, Haltproc
  .set PendSV_interrupt, Haltproc
  .set SysTick_interrupt, Haltproc
  .set WWDG_interrupt, Haltproc
  .set PVD_interrupt, Haltproc
  .set TAMP_STAMP_interrupt, Haltproc
  .set RTC_WKUP_interrupt, Haltproc
  .set FLASH_interrupt, Haltproc
  .set RCC_interrupt, Haltproc
  .set EXTI0_interrupt, Haltproc
  .set EXTI1_interrupt, Haltproc
  .set EXTI2_interrupt, Haltproc
  .set EXTI3_interrupt, Haltproc
  .set EXTI4_interrupt, Haltproc
  .set DMA1_Stream0_interrupt, Haltproc
  .set DMA1_Stream1_interrupt, Haltproc
  .set DMA1_Stream2_interrupt, Haltproc
  .set DMA1_Stream3_interrupt, Haltproc
  .set DMA1_Stream4_interrupt, Haltproc
  .set DMA1_Stream5_interrupt, Haltproc
  .set DMA1_Stream6_interrupt, Haltproc
  .set ADC_interrupt, Haltproc
  .set EXTI9_5_interrupt, Haltproc
  .set TIM1_BRK_TIM9_interrupt, Haltproc
  .set TIM1_UP_TIM10_interrupt, Haltproc
  .set TIM1_TRG_COM_TIM11_interrupt, Haltproc
  .set TIM1_CC_interrupt, Haltproc
  .set TIM2_interrupt, Haltproc
  .set TIM3_interrupt, Haltproc
  .set TIM4_interrupt, Haltproc
  .set I2C1_EV_interrupt, Haltproc
  .set I2C1_ER_interrupt, Haltproc
  .set I2C2_EV_interrupt, Haltproc
  .set I2C2_ER_interrupt, Haltproc
  .set SPI1_interrupt, Haltproc
  .set SPI2_interrupt, Haltproc
  .set USART1_interrupt, Haltproc
  .set USART2_interrupt, Haltproc
  .set EXTI15_10_interrupt, Haltproc
  .set RTC_Alarm_interrupt, Haltproc
  .set OTG_FS_WKUP_interrupt, Haltproc
  .set DMA1_Stream7_interrupt, Haltproc
  .set SDIO_interrupt, Haltproc
  .set TIM5_interrupt, Haltproc
  .set SPI3_interrupt, Haltproc
  .set DMA2_Stream0_interrupt, Haltproc
  .set DMA2_Stream1_interrupt, Haltproc
  .set DMA2_Stream2_interrupt, Haltproc
  .set DMA2_Stream3_interrupt, Haltproc
  .set DMA2_Stream4_interrupt, Haltproc
  .set OTG_FS_interrupt, Haltproc
  .set DMA2_Stream5_interrupt, Haltproc
  .set DMA2_Stream6_interrupt, Haltproc
  .set DMA2_Stream7_interrupt, Haltproc
  .set USART6_interrupt, Haltproc
  .set I2C3_EV_interrupt, Haltproc
  .set I2C3_ER_interrupt, Haltproc
  .set FPU_interrupt, Haltproc
  .set SPI4_interrupt, Haltproc

  .text
  end;
end.

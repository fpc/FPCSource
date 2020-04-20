unit stm32f103xe;
interface
{$PACKRECORDS C}
{$GOTO ON}
{$SCOPEDENUMS ON}

type
  TIRQn_Enum = (
    NonMaskableInt_IRQn = -14,        
    HardFault_IRQn = -13,             
    MemoryManagement_IRQn = -12,      
    BusFault_IRQn = -11,              
    UsageFault_IRQn = -10,            
    SVCall_IRQn = -5,                 
    DebugMonitor_IRQn = -4,           
    PendSV_IRQn = -2,                 
    SysTick_IRQn = -1,                
    WWDG_IRQn   = 0,                  
    PVD_IRQn    = 1,                  
    TAMPER_IRQn = 2,                  
    RTC_IRQn    = 3,                  
    FLASH_IRQn  = 4,                  
    RCC_IRQn    = 5,                  
    EXTI0_IRQn  = 6,                  
    EXTI1_IRQn  = 7,                  
    EXTI2_IRQn  = 8,                  
    EXTI3_IRQn  = 9,                  
    EXTI4_IRQn  = 10,                 
    DMA1_Channel1_IRQn = 11,          
    DMA1_Channel2_IRQn = 12,          
    DMA1_Channel3_IRQn = 13,          
    DMA1_Channel4_IRQn = 14,          
    DMA1_Channel5_IRQn = 15,          
    DMA1_Channel6_IRQn = 16,          
    DMA1_Channel7_IRQn = 17,          
    ADC1_2_IRQn = 18,                 
    USB_HP_CAN1_TX_IRQn = 19,         
    USB_LP_CAN1_RX0_IRQn = 20,        
    CAN1_RX1_IRQn = 21,               
    CAN1_SCE_IRQn = 22,               
    EXTI9_5_IRQn = 23,                
    TIM1_BRK_IRQn = 24,               
    TIM1_UP_IRQn = 25,                
    TIM1_TRG_COM_IRQn = 26,           
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
    USART3_IRQn = 39,                 
    EXTI15_10_IRQn = 40,              
    RTC_Alarm_IRQn = 41,              
    USBWakeUp_IRQn = 42,              
    TIM8_BRK_IRQn = 43,               
    TIM8_UP_IRQn = 44,                
    TIM8_TRG_COM_IRQn = 45,           
    TIM8_CC_IRQn = 46,                
    ADC3_IRQn   = 47,                 
    FSMC_IRQn   = 48,                 
    SDIO_IRQn   = 49,                 
    TIM5_IRQn   = 50,                 
    SPI3_IRQn   = 51,                 
    UART4_IRQn  = 52,                 
    UART5_IRQn  = 53,                 
    TIM6_IRQn   = 54,                 
    TIM7_IRQn   = 55,                 
    DMA2_Channel1_IRQn = 56,          
    DMA2_Channel2_IRQn = 57,          
    DMA2_Channel3_IRQn = 58,          
    DMA2_Channel4_5_IRQn = 59         
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
    SR          : longword;
    CR1         : longword;
    CR2         : longword;
    RESERVED    : array[0..15] of longword;
    DR          : longword;
  end;

  TBKP_Registers = record
    RESERVED0   : longword;
    DR1         : longword;
    DR2         : longword;
    DR3         : longword;
    DR4         : longword;
    DR5         : longword;
    DR6         : longword;
    DR7         : longword;
    DR8         : longword;
    DR9         : longword;
    DR10        : longword;
    RTCCR       : longword;
    CR          : longword;
    CSR         : longword;
    RESERVED13  : array[0..1] of longword;
    DR11        : longword;
    DR12        : longword;
    DR13        : longword;
    DR14        : longword;
    DR15        : longword;
    DR16        : longword;
    DR17        : longword;
    DR18        : longword;
    DR19        : longword;
    DR20        : longword;
    DR21        : longword;
    DR22        : longword;
    DR23        : longword;
    DR24        : longword;
    DR25        : longword;
    DR26        : longword;
    DR27        : longword;
    DR28        : longword;
    DR29        : longword;
    DR30        : longword;
    DR31        : longword;
    DR32        : longword;
    DR33        : longword;
    DR34        : longword;
    DR35        : longword;
    DR36        : longword;
    DR37        : longword;
    DR38        : longword;
    DR39        : longword;
    DR40        : longword;
    DR41        : longword;
    DR42        : longword;
  end;

  TCAN_TxMailBox_Registers = record
    TIR         : longword;
    TDTR        : longword;
    TDLR        : longword;
    TDHR        : longword;
  end;

  TCAN_FIFOMailBox_Registers = record
    RIR         : longword;
    RDTR        : longword;
    RDLR        : longword;
    RDHR        : longword;
  end;

  TCAN_FilterRegister_Registers = record
    FR1         : longword;
    FR2         : longword;
  end;

  TCAN_Registers = record
    MCR         : longword;
    MSR         : longword;
    TSR         : longword;
    RF0R        : longword;
    RF1R        : longword;
    IER         : longword;
    ESR         : longword;
    BTR         : longword;
    RESERVED0   : array[0..87] of longword;
    sTxMailBox  : array[0..2] of TCAN_TxMailBox_Registers;
    sFIFOMailBox : array[0..1] of TCAN_FIFOMailBox_Registers;
    RESERVED1   : array[0..11] of longword;
    FMR         : longword;
    FM1R        : longword;
    RESERVED2   : longword;
    FS1R        : longword;
    RESERVED3   : longword;
    FFA1R       : longword;
    RESERVED4   : longword;
    FA1R        : longword;
    RESERVED5   : array[0..7] of longword;
    sFilterRegister : array[0..13] of TCAN_FilterRegister_Registers;
  end;

  TCRC_Registers = record
    DR          : longword;
    IDR         : byte;
    RESERVED0   : byte;
    RESERVED1   : word;
    CR          : longword;
  end;

  TDAC_Registers = record
    CR          : longword;
    SWTRIGR     : longword;
    DHR12R1     : longword;
    DHR12L1     : longword;
    DHR8R1      : longword;
    DHR12R2     : longword;
    DHR12L2     : longword;
    DHR8R2      : longword;
    DHR12RD     : longword;
    DHR12LD     : longword;
    DHR8RD      : longword;
    DOR1        : longword;
    DOR2        : longword;
  end;

  TDBGMCU_Registers = record
    IDCODE      : longword;
    CR          : longword;
  end;

  TDMA_Channel_Registers = record
    CCR         : longword;
    CNDTR       : longword;
    CPAR        : longword;
    CMAR        : longword;
  end;

  TDMA_Registers = record
    ISR         : longword;
    IFCR        : longword;
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
    AR          : longword;
    RESERVED    : longword;
    OBR         : longword;
    WRPR        : longword;
  end;

  TOB_Registers = record
    RDP         : word;
    USER        : word;
    Data0       : word;
    Data1       : word;
    WRP0        : word;
    WRP1        : word;
    WRP2        : word;
    WRP3        : word;
  end;

  TFSMC_Bank1_Registers = record
    BTCR        : array[0..7] of longword;
  end;

  TFSMC_Bank1E_Registers = record
    BWTR        : array[0..6] of longword;
  end;

  TFSMC_Bank2_3_Registers = record
    PCR2        : longword;
    SR2         : longword;
    PMEM2       : longword;
    PATT2       : longword;
    RESERVED0   : longword;
    ECCR2       : longword;
    RESERVED1   : longword;
    RESERVED2   : longword;
    PCR3        : longword;
    SR3         : longword;
    PMEM3       : longword;
    PATT3       : longword;
    RESERVED3   : longword;
    ECCR3       : longword;
  end;

  TFSMC_Bank4_Registers = record
    PCR4        : longword;
    SR4         : longword;
    PMEM4       : longword;
    PATT4       : longword;
    PIO4        : longword;
  end;

  TGPIO_Registers = record
    CRL         : longword;
    CRH         : longword;
    IDR         : longword;
    ODR         : longword;
    BSRR        : longword;
    BRR         : longword;
    LCKR        : longword;
  end;

  TAFIO_Registers = record
    EVCR        : longword;
    MAPR        : longword;
    EXTICR      : array[0..3] of longword;
    RESERVED0   : longword;
    MAPR2       : longword;
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
    CFGR        : longword;
    CIR         : longword;
    APB2RSTR    : longword;
    APB1RSTR    : longword;
    AHBENR      : longword;
    APB2ENR     : longword;
    APB1ENR     : longword;
    BDCR        : longword;
    CSR         : longword;
  end;

  TRTC_Registers = record
    CRH         : longword;
    CRL         : longword;
    PRLH        : longword;
    PRLL        : longword;
    DIVH        : longword;
    DIVL        : longword;
    CNTH        : longword;
    CNTL        : longword;
    ALRH        : longword;
    ALRL        : longword;
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

  TUSB_Registers = record
    EP0R        : word;
    RESERVED0   : word;
    EP1R        : word;
    RESERVED1   : word;
    EP2R        : word;
    RESERVED2   : word;
    EP3R        : word;
    RESERVED3   : word;
    EP4R        : word;
    RESERVED4   : word;
    EP5R        : word;
    RESERVED5   : word;
    EP6R        : word;
    RESERVED6   : word;
    EP7R        : word;
    RESERVED7   : array[0..16] of word;
    CNTR        : word;
    RESERVED8   : word;
    ISTR        : word;
    RESERVED9   : word;
    FNR         : word;
    RESERVEDA   : word;
    DADDR       : word;
    RESERVEDB   : word;
    BTABLE      : word;
    RESERVEDC   : word;
  end;

  TWWDG_Registers = record
    CR          : longword;
    CFR         : longword;
    SR          : longword;
  end;

const
  FLASH_BASE    = $08000000;
  SRAM_BASE     = $20000000;
  PERIPH_BASE   = $40000000;
  SRAM_BB_BASE  = $22000000;
  PERIPH_BB_BASE= $42000000;
  FSMC_BASE     = $60000000;
  FSMC_R_BASE   = $A0000000;
  APB1PERIPH_BASE= PERIPH_BASE;
  APB2PERIPH_BASE= PERIPH_BASE + $00010000;
  AHBPERIPH_BASE= PERIPH_BASE + $00020000;
  TIM2_BASE     = APB1PERIPH_BASE + $00000000;
  TIM3_BASE     = APB1PERIPH_BASE + $00000400;
  TIM4_BASE     = APB1PERIPH_BASE + $00000800;
  TIM5_BASE     = APB1PERIPH_BASE + $00000C00;
  TIM6_BASE     = APB1PERIPH_BASE + $00001000;
  TIM7_BASE     = APB1PERIPH_BASE + $00001400;
  RTC_BASE      = APB1PERIPH_BASE + $00002800;
  WWDG_BASE     = APB1PERIPH_BASE + $00002C00;
  IWDG_BASE     = APB1PERIPH_BASE + $00003000;
  SPI2_BASE     = APB1PERIPH_BASE + $00003800;
  SPI3_BASE     = APB1PERIPH_BASE + $00003C00;
  USART2_BASE   = APB1PERIPH_BASE + $00004400;
  USART3_BASE   = APB1PERIPH_BASE + $00004800;
  UART4_BASE    = APB1PERIPH_BASE + $00004C00;
  UART5_BASE    = APB1PERIPH_BASE + $00005000;
  I2C1_BASE     = APB1PERIPH_BASE + $00005400;
  I2C2_BASE     = APB1PERIPH_BASE + $00005800;
  CAN1_BASE     = APB1PERIPH_BASE + $00006400;
  BKP_BASE      = APB1PERIPH_BASE + $00006C00;
  PWR_BASE      = APB1PERIPH_BASE + $00007000;
  DAC_BASE      = APB1PERIPH_BASE + $00007400;
  AFIO_BASE     = APB2PERIPH_BASE + $00000000;
  EXTI_BASE     = APB2PERIPH_BASE + $00000400;
  GPIOA_BASE    = APB2PERIPH_BASE + $00000800;
  GPIOB_BASE    = APB2PERIPH_BASE + $00000C00;
  GPIOC_BASE    = APB2PERIPH_BASE + $00001000;
  GPIOD_BASE    = APB2PERIPH_BASE + $00001400;
  GPIOE_BASE    = APB2PERIPH_BASE + $00001800;
  GPIOF_BASE    = APB2PERIPH_BASE + $00001C00;
  GPIOG_BASE    = APB2PERIPH_BASE + $00002000;
  ADC1_BASE     = APB2PERIPH_BASE + $00002400;
  ADC2_BASE     = APB2PERIPH_BASE + $00002800;
  TIM1_BASE     = APB2PERIPH_BASE + $00002C00;
  SPI1_BASE     = APB2PERIPH_BASE + $00003000;
  TIM8_BASE     = APB2PERIPH_BASE + $00003400;
  USART1_BASE   = APB2PERIPH_BASE + $00003800;
  ADC3_BASE     = APB2PERIPH_BASE + $00003C00;
  SDIO_BASE     = PERIPH_BASE + $00018000;
  DMA1_BASE     = AHBPERIPH_BASE + $00000000;
  DMA1_Channel1_BASE= AHBPERIPH_BASE + $00000008;
  DMA1_Channel2_BASE= AHBPERIPH_BASE + $0000001C;
  DMA1_Channel3_BASE= AHBPERIPH_BASE + $00000030;
  DMA1_Channel4_BASE= AHBPERIPH_BASE + $00000044;
  DMA1_Channel5_BASE= AHBPERIPH_BASE + $00000058;
  DMA1_Channel6_BASE= AHBPERIPH_BASE + $0000006C;
  DMA1_Channel7_BASE= AHBPERIPH_BASE + $00000080;
  DMA2_BASE     = AHBPERIPH_BASE + $00000400;
  DMA2_Channel1_BASE= AHBPERIPH_BASE + $00000408;
  DMA2_Channel2_BASE= AHBPERIPH_BASE + $0000041C;
  DMA2_Channel3_BASE= AHBPERIPH_BASE + $00000430;
  DMA2_Channel4_BASE= AHBPERIPH_BASE + $00000444;
  DMA2_Channel5_BASE= AHBPERIPH_BASE + $00000458;
  RCC_BASE      = AHBPERIPH_BASE + $00001000;
  CRC_BASE      = AHBPERIPH_BASE + $00003000;
  FLASH_R_BASE  = AHBPERIPH_BASE + $00002000;
  FLASHSIZE_BASE= $1FFFF7E0;
  UID_BASE      = $1FFFF7E8;
  OB_BASE       = $1FFFF800;
  FSMC_BANK1_R_BASE= FSMC_R_BASE + $00000000;
  FSMC_BANK1E_R_BASE= FSMC_R_BASE + $00000104;
  FSMC_BANK2_3_R_BASE= FSMC_R_BASE + $00000060;
  FSMC_BANK4_R_BASE= FSMC_R_BASE + $000000A0;
  DBGMCU_BASE   = $E0042000;
  USB_BASE      = APB1PERIPH_BASE + $00005C00;

var
  TIM2          : TTIM_Registers absolute TIM2_BASE;
  TIM3          : TTIM_Registers absolute TIM3_BASE;
  TIM4          : TTIM_Registers absolute TIM4_BASE;
  TIM5          : TTIM_Registers absolute TIM5_BASE;
  TIM6          : TTIM_Registers absolute TIM6_BASE;
  TIM7          : TTIM_Registers absolute TIM7_BASE;
  RTC           : TRTC_Registers absolute RTC_BASE;
  WWDG          : TWWDG_Registers absolute WWDG_BASE;
  IWDG          : TIWDG_Registers absolute IWDG_BASE;
  SPI2          : TSPI_Registers absolute SPI2_BASE;
  SPI3          : TSPI_Registers absolute SPI3_BASE;
  USART2        : TUSART_Registers absolute USART2_BASE;
  USART3        : TUSART_Registers absolute USART3_BASE;
  UART4         : TUSART_Registers absolute UART4_BASE;
  UART5         : TUSART_Registers absolute UART5_BASE;
  I2C1          : TI2C_Registers absolute I2C1_BASE;
  I2C2          : TI2C_Registers absolute I2C2_BASE;
  USB           : TUSB_Registers absolute USB_BASE;
  CAN1          : TCAN_Registers absolute CAN1_BASE;
  BKP           : TBKP_Registers absolute BKP_BASE;
  PWR           : TPWR_Registers absolute PWR_BASE;
  DAC1          : TDAC_Registers absolute DAC_BASE;
  DAC           : TDAC_Registers absolute DAC_BASE;
  AFIO          : TAFIO_Registers absolute AFIO_BASE;
  EXTI          : TEXTI_Registers absolute EXTI_BASE;
  GPIOA         : TGPIO_Registers absolute GPIOA_BASE;
  GPIOB         : TGPIO_Registers absolute GPIOB_BASE;
  GPIOC         : TGPIO_Registers absolute GPIOC_BASE;
  GPIOD         : TGPIO_Registers absolute GPIOD_BASE;
  GPIOE         : TGPIO_Registers absolute GPIOE_BASE;
  GPIOF         : TGPIO_Registers absolute GPIOF_BASE;
  GPIOG         : TGPIO_Registers absolute GPIOG_BASE;
  ADC1          : TADC_Registers absolute ADC1_BASE;
  ADC2          : TADC_Registers absolute ADC2_BASE;
  ADC3          : TADC_Registers absolute ADC3_BASE;
  ADC12_COMMON  : TADC_Common_Registers absolute ADC1_BASE;
  TIM1          : TTIM_Registers absolute TIM1_BASE;
  SPI1          : TSPI_Registers absolute SPI1_BASE;
  TIM8          : TTIM_Registers absolute TIM8_BASE;
  USART1        : TUSART_Registers absolute USART1_BASE;
  SDIO          : TSDIO_Registers absolute SDIO_BASE;
  DMA1          : TDMA_Registers absolute DMA1_BASE;
  DMA2          : TDMA_Registers absolute DMA2_BASE;
  DMA1_Channel1 : TDMA_Channel_Registers absolute DMA1_Channel1_BASE;
  DMA1_Channel2 : TDMA_Channel_Registers absolute DMA1_Channel2_BASE;
  DMA1_Channel3 : TDMA_Channel_Registers absolute DMA1_Channel3_BASE;
  DMA1_Channel4 : TDMA_Channel_Registers absolute DMA1_Channel4_BASE;
  DMA1_Channel5 : TDMA_Channel_Registers absolute DMA1_Channel5_BASE;
  DMA1_Channel6 : TDMA_Channel_Registers absolute DMA1_Channel6_BASE;
  DMA1_Channel7 : TDMA_Channel_Registers absolute DMA1_Channel7_BASE;
  DMA2_Channel1 : TDMA_Channel_Registers absolute DMA2_Channel1_BASE;
  DMA2_Channel2 : TDMA_Channel_Registers absolute DMA2_Channel2_BASE;
  DMA2_Channel3 : TDMA_Channel_Registers absolute DMA2_Channel3_BASE;
  DMA2_Channel4 : TDMA_Channel_Registers absolute DMA2_Channel4_BASE;
  DMA2_Channel5 : TDMA_Channel_Registers absolute DMA2_Channel5_BASE;
  RCC           : TRCC_Registers absolute RCC_BASE;
  CRC           : TCRC_Registers absolute CRC_BASE;
  FLASH         : TFLASH_Registers absolute FLASH_R_BASE;
  OB            : TOB_Registers absolute OB_BASE;
  FSMC_Bank1    : TFSMC_Bank1_Registers absolute FSMC_BANK1_R_BASE;
  FSMC_Bank1E   : TFSMC_Bank1E_Registers absolute FSMC_BANK1E_R_BASE;
  FSMC_Bank2_3  : TFSMC_Bank2_3_Registers absolute FSMC_BANK2_3_R_BASE;
  FSMC_Bank4    : TFSMC_Bank4_Registers absolute FSMC_BANK4_R_BASE;
  DBGMCU        : TDBGMCU_Registers absolute DBGMCU_BASE;

implementation

procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure HardFault_interrupt; external name 'HardFault_interrupt';
procedure MemoryManagement_interrupt; external name 'MemoryManagement_interrupt';
procedure BusFault_interrupt; external name 'BusFault_interrupt';
procedure UsageFault_interrupt; external name 'UsageFault_interrupt';
procedure SVCall_interrupt; external name 'SVCall_interrupt';
procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
procedure PendSV_interrupt; external name 'PendSV_interrupt';
procedure SysTick_interrupt; external name 'SysTick_interrupt';
procedure WWDG_interrupt; external name 'WWDG_interrupt';
procedure PVD_interrupt; external name 'PVD_interrupt';
procedure TAMPER_interrupt; external name 'TAMPER_interrupt';
procedure RTC_interrupt; external name 'RTC_interrupt';
procedure FLASH_interrupt; external name 'FLASH_interrupt';
procedure RCC_interrupt; external name 'RCC_interrupt';
procedure EXTI0_interrupt; external name 'EXTI0_interrupt';
procedure EXTI1_interrupt; external name 'EXTI1_interrupt';
procedure EXTI2_interrupt; external name 'EXTI2_interrupt';
procedure EXTI3_interrupt; external name 'EXTI3_interrupt';
procedure EXTI4_interrupt; external name 'EXTI4_interrupt';
procedure DMA1_Channel1_interrupt; external name 'DMA1_Channel1_interrupt';
procedure DMA1_Channel2_interrupt; external name 'DMA1_Channel2_interrupt';
procedure DMA1_Channel3_interrupt; external name 'DMA1_Channel3_interrupt';
procedure DMA1_Channel4_interrupt; external name 'DMA1_Channel4_interrupt';
procedure DMA1_Channel5_interrupt; external name 'DMA1_Channel5_interrupt';
procedure DMA1_Channel6_interrupt; external name 'DMA1_Channel6_interrupt';
procedure DMA1_Channel7_interrupt; external name 'DMA1_Channel7_interrupt';
procedure ADC1_2_interrupt; external name 'ADC1_2_interrupt';
procedure USB_HP_CAN1_TX_interrupt; external name 'USB_HP_CAN1_TX_interrupt';
procedure USB_LP_CAN1_RX0_interrupt; external name 'USB_LP_CAN1_RX0_interrupt';
procedure CAN1_RX1_interrupt; external name 'CAN1_RX1_interrupt';
procedure CAN1_SCE_interrupt; external name 'CAN1_SCE_interrupt';
procedure EXTI9_5_interrupt; external name 'EXTI9_5_interrupt';
procedure TIM1_BRK_interrupt; external name 'TIM1_BRK_interrupt';
procedure TIM1_UP_interrupt; external name 'TIM1_UP_interrupt';
procedure TIM1_TRG_COM_interrupt; external name 'TIM1_TRG_COM_interrupt';
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
procedure USBWakeUp_interrupt; external name 'USBWakeUp_interrupt';
procedure TIM8_BRK_interrupt; external name 'TIM8_BRK_interrupt';
procedure TIM8_UP_interrupt; external name 'TIM8_UP_interrupt';
procedure TIM8_TRG_COM_interrupt; external name 'TIM8_TRG_COM_interrupt';
procedure TIM8_CC_interrupt; external name 'TIM8_CC_interrupt';
procedure ADC3_interrupt; external name 'ADC3_interrupt';
procedure FSMC_interrupt; external name 'FSMC_interrupt';
procedure SDIO_interrupt; external name 'SDIO_interrupt';
procedure TIM5_interrupt; external name 'TIM5_interrupt';
procedure SPI3_interrupt; external name 'SPI3_interrupt';
procedure UART4_interrupt; external name 'UART4_interrupt';
procedure UART5_interrupt; external name 'UART5_interrupt';
procedure TIM6_interrupt; external name 'TIM6_interrupt';
procedure TIM7_interrupt; external name 'TIM7_interrupt';
procedure DMA2_Channel1_interrupt; external name 'DMA2_Channel1_interrupt';
procedure DMA2_Channel2_interrupt; external name 'DMA2_Channel2_interrupt';
procedure DMA2_Channel3_interrupt; external name 'DMA2_Channel3_interrupt';
procedure DMA2_Channel4_5_interrupt; external name 'DMA2_Channel4_5_interrupt';


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
  .long TAMPER_interrupt
  .long RTC_interrupt
  .long FLASH_interrupt
  .long RCC_interrupt
  .long EXTI0_interrupt
  .long EXTI1_interrupt
  .long EXTI2_interrupt
  .long EXTI3_interrupt
  .long EXTI4_interrupt
  .long DMA1_Channel1_interrupt
  .long DMA1_Channel2_interrupt
  .long DMA1_Channel3_interrupt
  .long DMA1_Channel4_interrupt
  .long DMA1_Channel5_interrupt
  .long DMA1_Channel6_interrupt
  .long DMA1_Channel7_interrupt
  .long ADC1_2_interrupt
  .long USB_HP_CAN1_TX_interrupt
  .long USB_LP_CAN1_RX0_interrupt
  .long CAN1_RX1_interrupt
  .long CAN1_SCE_interrupt
  .long EXTI9_5_interrupt
  .long TIM1_BRK_interrupt
  .long TIM1_UP_interrupt
  .long TIM1_TRG_COM_interrupt
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
  .long USBWakeUp_interrupt
  .long TIM8_BRK_interrupt
  .long TIM8_UP_interrupt
  .long TIM8_TRG_COM_interrupt
  .long TIM8_CC_interrupt
  .long ADC3_interrupt
  .long FSMC_interrupt
  .long SDIO_interrupt
  .long TIM5_interrupt
  .long SPI3_interrupt
  .long UART4_interrupt
  .long UART5_interrupt
  .long TIM6_interrupt
  .long TIM7_interrupt
  .long DMA2_Channel1_interrupt
  .long DMA2_Channel2_interrupt
  .long DMA2_Channel3_interrupt
  .long DMA2_Channel4_5_interrupt

  .weak NonMaskableInt_interrupt
  .weak HardFault_interrupt
  .weak MemoryManagement_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SVCall_interrupt
  .weak DebugMonitor_interrupt
  .weak PendSV_interrupt
  .weak SysTick_interrupt
  .weak WWDG_interrupt
  .weak PVD_interrupt
  .weak TAMPER_interrupt
  .weak RTC_interrupt
  .weak FLASH_interrupt
  .weak RCC_interrupt
  .weak EXTI0_interrupt
  .weak EXTI1_interrupt
  .weak EXTI2_interrupt
  .weak EXTI3_interrupt
  .weak EXTI4_interrupt
  .weak DMA1_Channel1_interrupt
  .weak DMA1_Channel2_interrupt
  .weak DMA1_Channel3_interrupt
  .weak DMA1_Channel4_interrupt
  .weak DMA1_Channel5_interrupt
  .weak DMA1_Channel6_interrupt
  .weak DMA1_Channel7_interrupt
  .weak ADC1_2_interrupt
  .weak USB_HP_CAN1_TX_interrupt
  .weak USB_LP_CAN1_RX0_interrupt
  .weak CAN1_RX1_interrupt
  .weak CAN1_SCE_interrupt
  .weak EXTI9_5_interrupt
  .weak TIM1_BRK_interrupt
  .weak TIM1_UP_interrupt
  .weak TIM1_TRG_COM_interrupt
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
  .weak USBWakeUp_interrupt
  .weak TIM8_BRK_interrupt
  .weak TIM8_UP_interrupt
  .weak TIM8_TRG_COM_interrupt
  .weak TIM8_CC_interrupt
  .weak ADC3_interrupt
  .weak FSMC_interrupt
  .weak SDIO_interrupt
  .weak TIM5_interrupt
  .weak SPI3_interrupt
  .weak UART4_interrupt
  .weak UART5_interrupt
  .weak TIM6_interrupt
  .weak TIM7_interrupt
  .weak DMA2_Channel1_interrupt
  .weak DMA2_Channel2_interrupt
  .weak DMA2_Channel3_interrupt
  .weak DMA2_Channel4_5_interrupt

  .set NonMaskableInt_interrupt, Haltproc
  .set HardFault_interrupt, Haltproc
  .set MemoryManagement_interrupt, Haltproc
  .set BusFault_interrupt, Haltproc
  .set UsageFault_interrupt, Haltproc
  .set SVCall_interrupt, Haltproc
  .set DebugMonitor_interrupt, Haltproc
  .set PendSV_interrupt, Haltproc
  .set SysTick_interrupt, Haltproc
  .set WWDG_interrupt, Haltproc
  .set PVD_interrupt, Haltproc
  .set TAMPER_interrupt, Haltproc
  .set RTC_interrupt, Haltproc
  .set FLASH_interrupt, Haltproc
  .set RCC_interrupt, Haltproc
  .set EXTI0_interrupt, Haltproc
  .set EXTI1_interrupt, Haltproc
  .set EXTI2_interrupt, Haltproc
  .set EXTI3_interrupt, Haltproc
  .set EXTI4_interrupt, Haltproc
  .set DMA1_Channel1_interrupt, Haltproc
  .set DMA1_Channel2_interrupt, Haltproc
  .set DMA1_Channel3_interrupt, Haltproc
  .set DMA1_Channel4_interrupt, Haltproc
  .set DMA1_Channel5_interrupt, Haltproc
  .set DMA1_Channel6_interrupt, Haltproc
  .set DMA1_Channel7_interrupt, Haltproc
  .set ADC1_2_interrupt, Haltproc
  .set USB_HP_CAN1_TX_interrupt, Haltproc
  .set USB_LP_CAN1_RX0_interrupt, Haltproc
  .set CAN1_RX1_interrupt, Haltproc
  .set CAN1_SCE_interrupt, Haltproc
  .set EXTI9_5_interrupt, Haltproc
  .set TIM1_BRK_interrupt, Haltproc
  .set TIM1_UP_interrupt, Haltproc
  .set TIM1_TRG_COM_interrupt, Haltproc
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
  .set USART3_interrupt, Haltproc
  .set EXTI15_10_interrupt, Haltproc
  .set RTC_Alarm_interrupt, Haltproc
  .set USBWakeUp_interrupt, Haltproc
  .set TIM8_BRK_interrupt, Haltproc
  .set TIM8_UP_interrupt, Haltproc
  .set TIM8_TRG_COM_interrupt, Haltproc
  .set TIM8_CC_interrupt, Haltproc
  .set ADC3_interrupt, Haltproc
  .set FSMC_interrupt, Haltproc
  .set SDIO_interrupt, Haltproc
  .set TIM5_interrupt, Haltproc
  .set SPI3_interrupt, Haltproc
  .set UART4_interrupt, Haltproc
  .set UART5_interrupt, Haltproc
  .set TIM6_interrupt, Haltproc
  .set TIM7_interrupt, Haltproc
  .set DMA2_Channel1_interrupt, Haltproc
  .set DMA2_Channel2_interrupt, Haltproc
  .set DMA2_Channel3_interrupt, Haltproc
  .set DMA2_Channel4_5_interrupt, Haltproc

  .text
  end;
end.

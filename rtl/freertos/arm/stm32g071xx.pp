unit stm32g071xx;
interface
{$PACKRECORDS C}
{$GOTO ON}
{$SCOPEDENUMS ON}

type
  TIRQn_Enum = (
    NonMaskableInt_IRQn = -14,        
    HardFault_IRQn = -13,             
    SVC_IRQn    = -5,                 
    PendSV_IRQn = -2,                 
    SysTick_IRQn = -1,                
    WWDG_IRQn   = 0,                  
    PVD_IRQn    = 1,                  
    RTC_TAMP_IRQn = 2,                
    FLASH_IRQn  = 3,                  
    RCC_IRQn    = 4,                  
    EXTI0_1_IRQn = 5,                 
    EXTI2_3_IRQn = 6,                 
    EXTI4_15_IRQn = 7,                
    UCPD1_2_IRQn = 8,                 
    DMA1_Channel1_IRQn = 9,           
    DMA1_Channel2_3_IRQn = 10,        
    DMA1_Ch4_7_DMAMUX1_OVR_IRQn = 11, 
    ADC1_COMP_IRQn = 12,              
    TIM1_BRK_UP_TRG_COM_IRQn = 13,    
    TIM1_CC_IRQn = 14,                
    TIM2_IRQn   = 15,                 
    TIM3_IRQn   = 16,                 
    TIM6_DAC_LPTIM1_IRQn = 17,        
    TIM7_LPTIM2_IRQn = 18,            
    TIM14_IRQn  = 19,                 
    TIM15_IRQn  = 20,                 
    TIM16_IRQn  = 21,                 
    TIM17_IRQn  = 22,                 
    I2C1_IRQn   = 23,                 
    I2C2_IRQn   = 24,                 
    SPI1_IRQn   = 25,                 
    SPI2_IRQn   = 26,                 
    USART1_IRQn = 27,                 
    USART2_IRQn = 28,                 
    USART3_4_LPUART1_IRQn = 29,       
    CEC_IRQn    = 30                  
  );

  TADC_Registers = record
    ISR         : longword;
    IER         : longword;
    CR          : longword;
    CFGR1       : longword;
    CFGR2       : longword;
    SMPR        : longword;
    RESERVED1   : longword;
    RESERVED2   : longword;
    TR1         : longword;
    TR2         : longword;
    CHSELR      : longword;
    TR3         : longword;
    RESERVED3   : array[0..3] of longword;
    DR          : longword;
    RESERVED4   : array[0..22] of longword;
    AWD2CR      : longword;
    AWD3CR      : longword;
    RESERVED5   : array[0..2] of longword;
    CALFACT     : longword;
  end;

  TADC_Common_Registers = record
    CCR         : longword;
  end;

  TCEC_Registers = record
    CR          : longword;
    CFGR        : longword;
    TXDR        : longword;
    RXDR        : longword;
    ISR         : longword;
    IER         : longword;
  end;

  TCOMP_Registers = record
    CSR         : longword;
  end;

  TCOMP_Common_Registers = record
    CSR_ODD     : longword;
    CSR_EVEN    : longword;
  end;

  TCRC_Registers = record
    DR          : longword;
    IDR         : longword;
    CR          : longword;
    RESERVED1   : longword;
    INIT        : longword;
    POL         : longword;
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
    SR          : longword;
    CCR         : longword;
    MCR         : longword;
    SHSR1       : longword;
    SHSR2       : longword;
    SHHR        : longword;
    SHRR        : longword;
  end;

  TDBG_Registers = record
    IDCODE      : longword;
    CR          : longword;
    APBFZ1      : longword;
    APBFZ2      : longword;
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

  TDMAMUX_Channel_Registers = record
    CCR         : longword;
  end;

  TDMAMUX_ChannelStatus_Registers = record
    CSR         : longword;
    CFR         : longword;
  end;

  TDMAMUX_RequestGen_Registers = record
    RGCR        : longword;
  end;

  TDMAMUX_RequestGenStatus_Registers = record
    RGSR        : longword;
    RGCFR       : longword;
  end;

  TEXTI_Registers = record
    RTSR1       : longword;
    FTSR1       : longword;
    SWIER1      : longword;
    RPR1        : longword;
    FPR1        : longword;
    RESERVED1   : array[0..2] of longword;
    RESERVED2   : array[0..4] of longword;
    RESERVED3   : array[0..10] of longword;
    EXTICR      : array[0..3] of longword;
    RESERVED4   : array[0..3] of longword;
    IMR1        : longword;
    EMR1        : longword;
    RESERVED5   : array[0..1] of longword;
    IMR2        : longword;
    EMR2        : longword;
  end;

  TFLASH_Registers = record
    ACR         : longword;
    RESERVED1   : longword;
    KEYR        : longword;
    OPTKEYR     : longword;
    SR          : longword;
    CR          : longword;
    ECCR        : longword;
    RESERVED2   : longword;
    OPTR        : longword;
    PCROP1ASR   : longword;
    PCROP1AER   : longword;
    WRP1AR      : longword;
    WRP1BR      : longword;
    PCROP1BSR   : longword;
    PCROP1BER   : longword;
    RESERVED3   : array[0..16] of longword;
    SECR        : longword;
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
    BRR         : longword;
  end;

  TI2C_Registers = record
    CR1         : longword;
    CR2         : longword;
    OAR1        : longword;
    OAR2        : longword;
    TIMINGR     : longword;
    TIMEOUTR    : longword;
    ISR         : longword;
    ICR         : longword;
    PECR        : longword;
    RXDR        : longword;
    TXDR        : longword;
  end;

  TIWDG_Registers = record
    KR          : longword;
    PR          : longword;
    RLR         : longword;
    SR          : longword;
    WINR        : longword;
  end;

  TLPTIM_Registers = record
    ISR         : longword;
    ICR         : longword;
    IER         : longword;
    CFGR        : longword;
    CR          : longword;
    CMP         : longword;
    ARR         : longword;
    CNT         : longword;
    RESERVED1   : longword;
    CFGR2       : longword;
  end;

  TPWR_Registers = record
    CR1         : longword;
    CR2         : longword;
    CR3         : longword;
    CR4         : longword;
    SR1         : longword;
    SR2         : longword;
    SCR         : longword;
    RESERVED1   : longword;
    PUCRA       : longword;
    PDCRA       : longword;
    PUCRB       : longword;
    PDCRB       : longword;
    PUCRC       : longword;
    PDCRC       : longword;
    PUCRD       : longword;
    PDCRD       : longword;
    RESERVED2   : longword;
    RESERVED3   : longword;
    PUCRF       : longword;
    PDCRF       : longword;
  end;

  TRCC_Registers = record
    CR          : longword;
    ICSCR       : longword;
    CFGR        : longword;
    PLLCFGR     : longword;
    RESERVED0   : longword;
    RESERVED1   : longword;
    CIER        : longword;
    CIFR        : longword;
    CICR        : longword;
    IOPRSTR     : longword;
    AHBRSTR     : longword;
    APBRSTR1    : longword;
    APBRSTR2    : longword;
    IOPENR      : longword;
    AHBENR      : longword;
    APBENR1     : longword;
    APBENR2     : longword;
    IOPSMENR    : longword;
    AHBSMENR    : longword;
    APBSMENR1   : longword;
    APBSMENR2   : longword;
    CCIPR       : longword;
    RESERVED2   : longword;
    BDCR        : longword;
    CSR         : longword;
  end;

  TRTC_Registers = record
    TR          : longword;
    DR          : longword;
    SSR         : longword;
    ICSR        : longword;
    PRER        : longword;
    WUTR        : longword;
    CR          : longword;
    RESERVED0   : longword;
    RESERVED1   : longword;
    WPR         : longword;
    CALR        : longword;
    SHIFTR      : longword;
    TSTR        : longword;
    TSDR        : longword;
    TSSSR       : longword;
    RESERVED2   : longword;
    ALRMAR      : longword;
    ALRMASSR    : longword;
    ALRMBR      : longword;
    ALRMBSSR    : longword;
    SR          : longword;
    MISR        : longword;
    RESERVED3   : longword;
    SCR         : longword;
    &OR         : longword;
  end;

  TTAMP_Registers = record
    CR1         : longword;
    CR2         : longword;
    RESERVED0   : longword;
    FLTCR       : longword;
    RESERVED1   : array[0..6] of longword;
    IER         : longword;
    SR          : longword;
    MISR        : longword;
    RESERVED2   : longword;
    SCR         : longword;
    RESERVED3   : array[0..47] of longword;
    BKP0R       : longword;
    BKP1R       : longword;
    BKP2R       : longword;
    BKP3R       : longword;
    BKP4R       : longword;
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

  TSYSCFG_Registers = record
    CFGR1       : longword;
    RESERVED0   : array[0..4] of longword;
    CFGR2       : longword;
    RESERVED1   : array[0..24] of longword;
    IT_LINE_SR  : array[0..31] of longword;
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
    OR1         : longword;
    CCMR3       : longword;
    CCR5        : longword;
    CCR6        : longword;
    AF1         : longword;
    AF2         : longword;
    TISEL       : longword;
  end;

  TUSART_Registers = record
    CR1         : longword;
    CR2         : longword;
    CR3         : longword;
    BRR         : longword;
    GTPR        : longword;
    RTOR        : longword;
    RQR         : longword;
    ISR         : longword;
    ICR         : longword;
    RDR         : longword;
    TDR         : longword;
    PRESC       : longword;
  end;

  TVREFBUF_Registers = record
    CSR         : longword;
    CCR         : longword;
  end;

  TWWDG_Registers = record
    CR          : longword;
    CFR         : longword;
    SR          : longword;
  end;

  TUCPD_Registers = record
    CFG1        : longword;
    CFG2        : longword;
    RESERVED0   : longword;
    CR          : longword;
    IMR         : longword;
    SR          : longword;
    ICR         : longword;
    TX_ORDSET   : longword;
    TX_PAYSZ    : longword;
    TXDR        : longword;
    RX_ORDSET   : longword;
    RX_PAYSZ    : longword;
    RXDR        : longword;
    RX_ORDEXT1  : longword;
    RX_ORDEXT2  : longword;
  end;

const
  FLASH_BASE    = $08000000;
  SRAM_BASE     = $20000000;
  PERIPH_BASE   = $40000000;
  IOPORT_BASE   = $50000000;
  APBPERIPH_BASE= PERIPH_BASE;
  AHBPERIPH_BASE= PERIPH_BASE + $00020000;
  TIM2_BASE     = APBPERIPH_BASE + 0;
  TIM3_BASE     = APBPERIPH_BASE + $00000400;
  TIM6_BASE     = APBPERIPH_BASE + $00001000;
  TIM7_BASE     = APBPERIPH_BASE + $00001400;
  TIM14_BASE    = APBPERIPH_BASE + $00002000;
  RTC_BASE      = APBPERIPH_BASE + $00002800;
  WWDG_BASE     = APBPERIPH_BASE + $00002C00;
  IWDG_BASE     = APBPERIPH_BASE + $00003000;
  SPI2_BASE     = APBPERIPH_BASE + $00003800;
  USART2_BASE   = APBPERIPH_BASE + $00004400;
  USART3_BASE   = APBPERIPH_BASE + $00004800;
  USART4_BASE   = APBPERIPH_BASE + $00004C00;
  I2C1_BASE     = APBPERIPH_BASE + $00005400;
  I2C2_BASE     = APBPERIPH_BASE + $00005800;
  PWR_BASE      = APBPERIPH_BASE + $00007000;
  DAC1_BASE     = APBPERIPH_BASE + $00007400;
  DAC_BASE      = APBPERIPH_BASE + $00007400;
  CEC_BASE      = APBPERIPH_BASE + $00007800;
  LPTIM1_BASE   = APBPERIPH_BASE + $00007C00;
  LPUART1_BASE  = APBPERIPH_BASE + $00008000;
  LPTIM2_BASE   = APBPERIPH_BASE + $00009400;
  UCPD1_BASE    = APBPERIPH_BASE + $0000A000;
  UCPD2_BASE    = APBPERIPH_BASE + $0000A400;
  TAMP_BASE     = APBPERIPH_BASE + $0000B000;
  SYSCFG_BASE   = APBPERIPH_BASE + $00010000;
  VREFBUF_BASE  = APBPERIPH_BASE + $00010030;
  COMP1_BASE    = SYSCFG_BASE + $0200;
  COMP2_BASE    = SYSCFG_BASE + $0204;
  ADC1_BASE     = APBPERIPH_BASE + $00012400;
  ADC1_COMMON_BASE= APBPERIPH_BASE + $00012708;
  ADC_BASE      = ADC1_COMMON_BASE;
  TIM1_BASE     = APBPERIPH_BASE + $00012C00;
  SPI1_BASE     = APBPERIPH_BASE + $00013000;
  USART1_BASE   = APBPERIPH_BASE + $00013800;
  TIM15_BASE    = APBPERIPH_BASE + $00014000;
  TIM16_BASE    = APBPERIPH_BASE + $00014400;
  TIM17_BASE    = APBPERIPH_BASE + $00014800;
  DBG_BASE      = APBPERIPH_BASE + $00015800;
  DMA1_BASE     = AHBPERIPH_BASE;
  DMAMUX1_BASE  = AHBPERIPH_BASE + $00000800;
  RCC_BASE      = AHBPERIPH_BASE + $00001000;
  EXTI_BASE     = AHBPERIPH_BASE + $00001800;
  FLASH_R_BASE  = AHBPERIPH_BASE + $00002000;
  CRC_BASE      = AHBPERIPH_BASE + $00003000;
  DMA1_Channel1_BASE= DMA1_BASE + $00000008;
  DMA1_Channel2_BASE= DMA1_BASE + $0000001C;
  DMA1_Channel3_BASE= DMA1_BASE + $00000030;
  DMA1_Channel4_BASE= DMA1_BASE + $00000044;
  DMA1_Channel5_BASE= DMA1_BASE + $00000058;
  DMA1_Channel6_BASE= DMA1_BASE + $0000006C;
  DMA1_Channel7_BASE= DMA1_BASE + $00000080;
  DMAMUX1_Channel0_BASE= DMAMUX1_BASE;
  DMAMUX1_Channel1_BASE= DMAMUX1_BASE + $00000004;
  DMAMUX1_Channel2_BASE= DMAMUX1_BASE + $00000008;
  DMAMUX1_Channel3_BASE= DMAMUX1_BASE + $0000000C;
  DMAMUX1_Channel4_BASE= DMAMUX1_BASE + $00000010;
  DMAMUX1_Channel5_BASE= DMAMUX1_BASE + $00000014;
  DMAMUX1_Channel6_BASE= DMAMUX1_BASE + $00000018;
  DMAMUX1_RequestGenerator0_BASE= DMAMUX1_BASE + $00000100;
  DMAMUX1_RequestGenerator1_BASE= DMAMUX1_BASE + $00000104;
  DMAMUX1_RequestGenerator2_BASE= DMAMUX1_BASE + $00000108;
  DMAMUX1_RequestGenerator3_BASE= DMAMUX1_BASE + $0000010C;
  DMAMUX1_ChannelStatus_BASE= DMAMUX1_BASE + $00000080;
  DMAMUX1_RequestGenStatus_BASE= DMAMUX1_BASE + $00000140;
  GPIOA_BASE    = IOPORT_BASE + $00000000;
  GPIOB_BASE    = IOPORT_BASE + $00000400;
  GPIOC_BASE    = IOPORT_BASE + $00000800;
  GPIOD_BASE    = IOPORT_BASE + $00000C00;
  GPIOF_BASE    = IOPORT_BASE + $00001400;
  PACKAGE_BASE  = $1FFF7500;
  UID_BASE      = $1FFF7590;
  FLASHSIZE_BASE= $1FFF75E0;

var
  TIM2          : TTIM_Registers absolute TIM2_BASE;
  TIM3          : TTIM_Registers absolute TIM3_BASE;
  TIM6          : TTIM_Registers absolute TIM6_BASE;
  TIM7          : TTIM_Registers absolute TIM7_BASE;
  TIM14         : TTIM_Registers absolute TIM14_BASE;
  RTC           : TRTC_Registers absolute RTC_BASE;
  TAMP          : TTAMP_Registers absolute TAMP_BASE;
  WWDG          : TWWDG_Registers absolute WWDG_BASE;
  IWDG          : TIWDG_Registers absolute IWDG_BASE;
  SPI2          : TSPI_Registers absolute SPI2_BASE;
  USART2        : TUSART_Registers absolute USART2_BASE;
  USART3        : TUSART_Registers absolute USART3_BASE;
  USART4        : TUSART_Registers absolute USART4_BASE;
  I2C1          : TI2C_Registers absolute I2C1_BASE;
  I2C2          : TI2C_Registers absolute I2C2_BASE;
  LPTIM1        : TLPTIM_Registers absolute LPTIM1_BASE;
  PWR           : TPWR_Registers absolute PWR_BASE;
  RCC           : TRCC_Registers absolute RCC_BASE;
  EXTI          : TEXTI_Registers absolute EXTI_BASE;
  DAC1          : TDAC_Registers absolute DAC1_BASE;
  DAC           : TDAC_Registers absolute DAC_BASE;
  LPUART1       : TUSART_Registers absolute LPUART1_BASE;
  LPTIM2        : TLPTIM_Registers absolute LPTIM2_BASE;
  CEC           : TCEC_Registers absolute CEC_BASE;
  SYSCFG        : TSYSCFG_Registers absolute SYSCFG_BASE;
  VREFBUF       : TVREFBUF_Registers absolute VREFBUF_BASE;
  COMP1         : TCOMP_Registers absolute COMP1_BASE;
  COMP2         : TCOMP_Registers absolute COMP2_BASE;
  COMP12_COMMON : TCOMP_Common_Registers absolute COMP1_BASE;
  TIM1          : TTIM_Registers absolute TIM1_BASE;
  SPI1          : TSPI_Registers absolute SPI1_BASE;
  USART1        : TUSART_Registers absolute USART1_BASE;
  TIM15         : TTIM_Registers absolute TIM15_BASE;
  TIM16         : TTIM_Registers absolute TIM16_BASE;
  TIM17         : TTIM_Registers absolute TIM17_BASE;
  DMA1          : TDMA_Registers absolute DMA1_BASE;
  FLASH         : TFLASH_Registers absolute FLASH_R_BASE;
  CRC           : TCRC_Registers absolute CRC_BASE;
  GPIOA         : TGPIO_Registers absolute GPIOA_BASE;
  GPIOB         : TGPIO_Registers absolute GPIOB_BASE;
  GPIOC         : TGPIO_Registers absolute GPIOC_BASE;
  GPIOD         : TGPIO_Registers absolute GPIOD_BASE;
  GPIOF         : TGPIO_Registers absolute GPIOF_BASE;
  ADC1          : TADC_Registers absolute ADC1_BASE;
  ADC1_COMMON   : TADC_Common_Registers absolute ADC1_COMMON_BASE;
  UCPD1         : TUCPD_Registers absolute UCPD1_BASE;
  UCPD2         : TUCPD_Registers absolute UCPD2_BASE;
  DMA1_Channel1 : TDMA_Channel_Registers absolute DMA1_Channel1_BASE;
  DMA1_Channel2 : TDMA_Channel_Registers absolute DMA1_Channel2_BASE;
  DMA1_Channel3 : TDMA_Channel_Registers absolute DMA1_Channel3_BASE;
  DMA1_Channel4 : TDMA_Channel_Registers absolute DMA1_Channel4_BASE;
  DMA1_Channel5 : TDMA_Channel_Registers absolute DMA1_Channel5_BASE;
  DMA1_Channel6 : TDMA_Channel_Registers absolute DMA1_Channel6_BASE;
  DMA1_Channel7 : TDMA_Channel_Registers absolute DMA1_Channel7_BASE;
  DMAMUX1       : TDMAMUX_Channel_Registers absolute DMAMUX1_BASE;
  DMAMUX1_Channel0: TDMAMUX_Channel_Registers absolute DMAMUX1_Channel0_BASE;
  DMAMUX1_Channel1: TDMAMUX_Channel_Registers absolute DMAMUX1_Channel1_BASE;
  DMAMUX1_Channel2: TDMAMUX_Channel_Registers absolute DMAMUX1_Channel2_BASE;
  DMAMUX1_Channel3: TDMAMUX_Channel_Registers absolute DMAMUX1_Channel3_BASE;
  DMAMUX1_Channel4: TDMAMUX_Channel_Registers absolute DMAMUX1_Channel4_BASE;
  DMAMUX1_Channel5: TDMAMUX_Channel_Registers absolute DMAMUX1_Channel5_BASE;
  DMAMUX1_Channel6: TDMAMUX_Channel_Registers absolute DMAMUX1_Channel6_BASE;
  DMAMUX1_RequestGenerator0: TDMAMUX_RequestGen_Registers absolute DMAMUX1_RequestGenerator0_BASE;
  DMAMUX1_RequestGenerator1: TDMAMUX_RequestGen_Registers absolute DMAMUX1_RequestGenerator1_BASE;
  DMAMUX1_RequestGenerator2: TDMAMUX_RequestGen_Registers absolute DMAMUX1_RequestGenerator2_BASE;
  DMAMUX1_RequestGenerator3: TDMAMUX_RequestGen_Registers absolute DMAMUX1_RequestGenerator3_BASE;
  DMAMUX1_ChannelStatus: TDMAMUX_ChannelStatus_Registers absolute DMAMUX1_ChannelStatus_BASE;
  DMAMUX1_RequestGenStatus: TDMAMUX_RequestGenStatus_Registers absolute DMAMUX1_RequestGenStatus_BASE;
  DBG           : TDBG_Registers absolute DBG_BASE;

implementation

procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure HardFault_interrupt; external name 'HardFault_interrupt';
procedure SVC_interrupt; external name 'SVC_interrupt';
procedure PendSV_interrupt; external name 'PendSV_interrupt';
procedure SysTick_interrupt; external name 'SysTick_interrupt';
procedure WWDG_interrupt; external name 'WWDG_interrupt';
procedure PVD_interrupt; external name 'PVD_interrupt';
procedure RTC_TAMP_interrupt; external name 'RTC_TAMP_interrupt';
procedure FLASH_interrupt; external name 'FLASH_interrupt';
procedure RCC_interrupt; external name 'RCC_interrupt';
procedure EXTI0_1_interrupt; external name 'EXTI0_1_interrupt';
procedure EXTI2_3_interrupt; external name 'EXTI2_3_interrupt';
procedure EXTI4_15_interrupt; external name 'EXTI4_15_interrupt';
procedure UCPD1_2_interrupt; external name 'UCPD1_2_interrupt';
procedure DMA1_Channel1_interrupt; external name 'DMA1_Channel1_interrupt';
procedure DMA1_Channel2_3_interrupt; external name 'DMA1_Channel2_3_interrupt';
procedure DMA1_Ch4_7_DMAMUX1_OVR_interrupt; external name 'DMA1_Ch4_7_DMAMUX1_OVR_interrupt';
procedure ADC1_COMP_interrupt; external name 'ADC1_COMP_interrupt';
procedure TIM1_BRK_UP_TRG_COM_interrupt; external name 'TIM1_BRK_UP_TRG_COM_interrupt';
procedure TIM1_CC_interrupt; external name 'TIM1_CC_interrupt';
procedure TIM2_interrupt; external name 'TIM2_interrupt';
procedure TIM3_interrupt; external name 'TIM3_interrupt';
procedure TIM6_DAC_LPTIM1_interrupt; external name 'TIM6_DAC_LPTIM1_interrupt';
procedure TIM7_LPTIM2_interrupt; external name 'TIM7_LPTIM2_interrupt';
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
procedure USART3_4_LPUART1_interrupt; external name 'USART3_4_LPUART1_interrupt';
procedure CEC_interrupt; external name 'CEC_interrupt';


{$i cortexm0_start.inc}

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
  .long RTC_TAMP_interrupt
  .long FLASH_interrupt
  .long RCC_interrupt
  .long EXTI0_1_interrupt
  .long EXTI2_3_interrupt
  .long EXTI4_15_interrupt
  .long UCPD1_2_interrupt
  .long DMA1_Channel1_interrupt
  .long DMA1_Channel2_3_interrupt
  .long DMA1_Ch4_7_DMAMUX1_OVR_interrupt
  .long ADC1_COMP_interrupt
  .long TIM1_BRK_UP_TRG_COM_interrupt
  .long TIM1_CC_interrupt
  .long TIM2_interrupt
  .long TIM3_interrupt
  .long TIM6_DAC_LPTIM1_interrupt
  .long TIM7_LPTIM2_interrupt
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
  .long USART3_4_LPUART1_interrupt
  .long CEC_interrupt

  .weak NonMaskableInt_interrupt
  .weak HardFault_interrupt
  .weak SVC_interrupt
  .weak PendSV_interrupt
  .weak SysTick_interrupt
  .weak WWDG_interrupt
  .weak PVD_interrupt
  .weak RTC_TAMP_interrupt
  .weak FLASH_interrupt
  .weak RCC_interrupt
  .weak EXTI0_1_interrupt
  .weak EXTI2_3_interrupt
  .weak EXTI4_15_interrupt
  .weak UCPD1_2_interrupt
  .weak DMA1_Channel1_interrupt
  .weak DMA1_Channel2_3_interrupt
  .weak DMA1_Ch4_7_DMAMUX1_OVR_interrupt
  .weak ADC1_COMP_interrupt
  .weak TIM1_BRK_UP_TRG_COM_interrupt
  .weak TIM1_CC_interrupt
  .weak TIM2_interrupt
  .weak TIM3_interrupt
  .weak TIM6_DAC_LPTIM1_interrupt
  .weak TIM7_LPTIM2_interrupt
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
  .weak USART3_4_LPUART1_interrupt
  .weak CEC_interrupt

  .set NonMaskableInt_interrupt, Haltproc
  .set HardFault_interrupt, Haltproc
  .set SVC_interrupt, Haltproc
  .set PendSV_interrupt, Haltproc
  .set SysTick_interrupt, Haltproc
  .set WWDG_interrupt, Haltproc
  .set PVD_interrupt, Haltproc
  .set RTC_TAMP_interrupt, Haltproc
  .set FLASH_interrupt, Haltproc
  .set RCC_interrupt, Haltproc
  .set EXTI0_1_interrupt, Haltproc
  .set EXTI2_3_interrupt, Haltproc
  .set EXTI4_15_interrupt, Haltproc
  .set UCPD1_2_interrupt, Haltproc
  .set DMA1_Channel1_interrupt, Haltproc
  .set DMA1_Channel2_3_interrupt, Haltproc
  .set DMA1_Ch4_7_DMAMUX1_OVR_interrupt, Haltproc
  .set ADC1_COMP_interrupt, Haltproc
  .set TIM1_BRK_UP_TRG_COM_interrupt, Haltproc
  .set TIM1_CC_interrupt, Haltproc
  .set TIM2_interrupt, Haltproc
  .set TIM3_interrupt, Haltproc
  .set TIM6_DAC_LPTIM1_interrupt, Haltproc
  .set TIM7_LPTIM2_interrupt, Haltproc
  .set TIM14_interrupt, Haltproc
  .set TIM15_interrupt, Haltproc
  .set TIM16_interrupt, Haltproc
  .set TIM17_interrupt, Haltproc
  .set I2C1_interrupt, Haltproc
  .set I2C2_interrupt, Haltproc
  .set SPI1_interrupt, Haltproc
  .set SPI2_interrupt, Haltproc
  .set USART1_interrupt, Haltproc
  .set USART2_interrupt, Haltproc
  .set USART3_4_LPUART1_interrupt, Haltproc
  .set CEC_interrupt, Haltproc

  .text
  end;
end.

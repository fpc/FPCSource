{
Register definitions and utility code for STM32F746

Created by Jeppe Johansen 2015 - jeppe@j-software.dk
}
unit stm32f746;

{$goto on}

interface

{$PACKRECORDS C}

(**
 * @brief Configuration of the Cortex-M7 Processor and Core Peripherals
  *)

const
  __CM7_REV        = $0000;       (*!< Cortex-M7 revision r0p1                        *)
  __MPU_PRESENT    = true;        (*!< CM7 provides an MPU                            *)
  __NVIC_PRIO_BITS = 4;           (*!< CM7 uses 4 Bits for the Priority Levels        *)
  __Vendor_SysTickConfig = 0;     (*!< Set to 1 if different SysTick Config is used   *)
  __FPU_PRESENT    = true;        (*!< FPU present                                    *)
  CACHE_PRESENT    = true;        (*!< CM7 instruction cache present                  *)
  __DCACHE_PRESENT = true;        (*!< CM7 data cache present                         *)


  (** @addtogroup Peripheral_registers_structures
  * @{
   *)

  (**
  * @brief Analog to Digital Converter
   *)

type
  ADC_TypeDef = record
    SR: longword;     (*!< ADC status register,                         Address offset: 0x00  *)
    CR1: longword;    (*!< ADC control register 1,                      Address offset: 0x04  *)
    CR2: longword;    (*!< ADC control register 2,                      Address offset: 0x08  *)
    SMPR1: longword;  (*!< ADC sample time register 1,                  Address offset: 0x0C  *)
    SMPR2: longword;  (*!< ADC sample time register 2,                  Address offset: 0x10  *)
    JOFR1: longword;  (*!< ADC injected channel data offset register 1, Address offset: 0x14  *)
    JOFR2: longword;  (*!< ADC injected channel data offset register 2, Address offset: 0x18  *)
    JOFR3: longword;  (*!< ADC injected channel data offset register 3, Address offset: 0x1C  *)
    JOFR4: longword;  (*!< ADC injected channel data offset register 4, Address offset: 0x20  *)
    HTR: longword;    (*!< ADC watchdog higher threshold register,      Address offset: 0x24  *)
    LTR: longword;    (*!< ADC watchdog lower threshold register,       Address offset: 0x28  *)
    SQR1: longword;   (*!< ADC regular sequence register 1,             Address offset: 0x2C  *)
    SQR2: longword;   (*!< ADC regular sequence register 2,             Address offset: 0x30  *)
    SQR3: longword;   (*!< ADC regular sequence register 3,             Address offset: 0x34  *)
    JSQR: longword;   (*!< ADC injected sequence register,              Address offset: 0x38 *)
    JDR1: longword;   (*!< ADC injected data register 1,                Address offset: 0x3C  *)
    JDR2: longword;   (*!< ADC injected data register 2,                Address offset: 0x40  *)
    JDR3: longword;   (*!< ADC injected data register 3,                Address offset: 0x44  *)
    JDR4: longword;   (*!< ADC injected data register 4,                Address offset: 0x48  *)
    DR: longword;     (*!< ADC regular data register,                   Address offset: 0x4C  *)
  end;

  ADC_Common_TypeDef = record
    CSR: longword;  (*!< ADC Common status register,                  Address offset: ADC1 base address + 0x300  *)
    CCR: longword;  (*!< ADC common control register,                 Address offset: ADC1 base address + 0x304  *)
    CDR: longword;  (*!< ADC common regular data register for dual
                             AND triple modes,                            Address offset: ADC1 base address + 0x308  *)
  end;


  (**
  * @brief Controller Area Network TxMailBox
   *)

  CAN_TxMailBox_TypeDef = record
    TIR: longword;   (*!< CAN TX mailbox identifier register  *)
    TDTR: longword;  (*!< CAN mailbox data length control and time stamp register  *)
    TDLR: longword;  (*!< CAN mailbox data low register  *)
    TDHR: longword;  (*!< CAN mailbox data high register  *)
  end;

  (**
  * @brief Controller Area Network FIFOMailBox
   *)

  CAN_FIFOMailBox_TypeDef = record
    RIR: longword;   (*!< CAN receive FIFO mailbox identifier register  *)
    RDTR: longword;  (*!< CAN receive FIFO mailbox data length control and time stamp register  *)
    RDLR: longword;  (*!< CAN receive FIFO mailbox data low register  *)
    RDHR: longword;  (*!< CAN receive FIFO mailbox data high register  *)
  end;

  (**
  * @brief Controller Area Network FilterRegister
   *)

  CAN_FilterRegister_TypeDef = record
    FR1: longword;  (*!< CAN Filter bank register 1  *)
    FR2: longword;  (*!< CAN Filter bank register 1  *)
  end;

  (**
  * @brief Controller Area Network
   *)

  CAN_TypeDef = record
    MCR: longword;        (*!< CAN master control register,         Address offset: 0x00           *)
    MSR: longword;        (*!< CAN master status register,          Address offset: 0x04           *)
    TSR: longword;        (*!< CAN transmit status register,        Address offset: 0x08           *)
    RF0R: longword;       (*!< CAN receive FIFO 0 register,         Address offset: 0x0C           *)
    RF1R: longword;       (*!< CAN receive FIFO 1 register,         Address offset: 0x10           *)
    IER: longword;        (*!< CAN interrupt enable register,       Address offset: 0x14           *)
    ESR: longword;        (*!< CAN error status register,           Address offset: 0x18           *)
    BTR: longword;        (*!< CAN bit timing register,             Address offset: 0x1C           *)
    RESERVED0: array [0..87] of longword;  (*!< Reserved, 0x020 - 0x17F                                             *)
    sTxMailBox: array [0..2] of CAN_TxMailBox_TypeDef;  (*!< CAN Tx MailBox,                      Address offset: 0x180 - 0x1AC  *)
    sFIFOMailBox: array [0..1] of CAN_FIFOMailBox_TypeDef;  (*!< CAN FIFO MailBox,                    Address offset: 0x1B0 - 0x1CC  *)
    RESERVED1: array [0..11] of longword;  (*!< Reserved, 0x1D0 - 0x1FF                                             *)
    FMR: longword;        (*!< CAN filter master register,          Address offset: 0x200          *)
    FM1R: longword;       (*!< CAN filter mode register,            Address offset: 0x204          *)
    RESERVED2: longword;  (*!< Reserved, 0x208                                                     *)
    FS1R: longword;       (*!< CAN filter scale register,           Address offset: 0x20C          *)
    RESERVED3: longword;  (*!< Reserved, 0x210                                                     *)
    FFA1R: longword;      (*!< CAN filter FIFO assignment register, Address offset: 0x214          *)
    RESERVED4: longword;  (*!< Reserved, 0x218                                                     *)
    FA1R: longword;       (*!< CAN filter activation register,      Address offset: 0x21C          *)
    RESERVED5: array [0..7] of longword;  (*!< Reserved, 0x220-0x23F                                               *)
    sFilterRegister: array [0..27] of CAN_FilterRegister_TypeDef;  (*!< CAN Filter Register,                 Address offset: 0x240-0x31C    *)
  end;

  (**
  * @brief HDMI-CEC
   *)

  CEC_TypeDef = record
    CR: longword;    (*!< CEC control register,                                       Address offset:0x00  *)
    CFGR: longword;  (*!< CEC configuration register,                                 Address offset:0x04  *)
    TXDR: longword;  (*!< CEC Tx data register ,                                      Address offset:0x08  *)
    RXDR: longword;  (*!< CEC Rx Data Register,                                       Address offset:0x0C  *)
    ISR: longword;   (*!< CEC Interrupt and Status Register,                          Address offset:0x10  *)
    IER: longword;   (*!< CEC interrupt enable register,                              Address offset:0x14  *)
  end;


  (**
  * @brief CRC calculation unit
   *)

  CRC_TypeDef = record
    DR: longword;     (*!< CRC Data register,                           Address offset: 0x00  *)
    IDR: byte;        (*!< CRC Independent data register,               Address offset: 0x04  *)
    RESERVED0: byte;  (*!< Reserved, 0x05                                                     *)
    RESERVED1: word;  (*!< Reserved, 0x06                                                     *)
    CR: longword;     (*!< CRC Control register,                        Address offset: 0x08  *)
    RESERVED2: longword;  (*!< Reserved,                                                    0x0C  *)
    INIT: longword;   (*!< Initial CRC value register,                  Address offset: 0x10  *)
    POL: longword;    (*!< CRC polynomial register,                     Address offset: 0x14  *)
  end;

  (**
  * @brief Digital to Analog Converter
   *)

  DAC_TypeDef = record
    CR: longword;       (*!< DAC control register,                                    Address offset: 0x00  *)
    SWTRIGR: longword;  (*!< DAC software trigger register,                           Address offset: 0x04  *)
    DHR12R1: longword;  (*!< DAC channel1 12-bit right-aligned data holding register, Address offset: 0x08  *)
    DHR12L1: longword;  (*!< DAC channel1 12-bit left aligned data holding register,  Address offset: 0x0C  *)
    DHR8R1: longword;   (*!< DAC channel1 8-bit right aligned data holding register,  Address offset: 0x10  *)
    DHR12R2: longword;  (*!< DAC channel2 12-bit right aligned data holding register, Address offset: 0x14  *)
    DHR12L2: longword;  (*!< DAC channel2 12-bit left aligned data holding register,  Address offset: 0x18  *)
    DHR8R2: longword;   (*!< DAC channel2 8-bit right-aligned data holding register,  Address offset: 0x1C  *)
    DHR12RD: longword;  (*!< Dual DAC 12-bit right-aligned data holding register,     Address offset: 0x20  *)
    DHR12LD: longword;  (*!< DUAL DAC 12-bit left aligned data holding register,      Address offset: 0x24  *)
    DHR8RD: longword;   (*!< DUAL DAC 8-bit right aligned data holding register,      Address offset: 0x28  *)
    DOR1: longword;     (*!< DAC channel1 data output register,                       Address offset: 0x2C  *)
    DOR2: longword;     (*!< DAC channel2 data output register,                       Address offset: 0x30  *)
    SR: longword;       (*!< DAC status register,                                     Address offset: 0x34  *)
  end;

  (**
  * @brief Debug MCU
   *)

  DBGMCU_TypeDef = record
    IDCODE: longword;  (*!< MCU device ID code,               Address offset: 0x00  *)
    CR: longword;      (*!< Debug MCU configuration register, Address offset: 0x04  *)
    APB1FZ: longword;  (*!< Debug MCU APB1 freeze register,   Address offset: 0x08  *)
    APB2FZ: longword;  (*!< Debug MCU APB2 freeze register,   Address offset: 0x0C  *)
  end;

  (**
  * @brief DCMI
   *)

  DCMI_TypeDef = record
    CR: longword;       (*!< DCMI control register 1,                       Address offset: 0x00  *)
    SR: longword;       (*!< DCMI status register,                          Address offset: 0x04  *)
    RISR: longword;     (*!< DCMI raw interrupt status register,            Address offset: 0x08  *)
    IER: longword;      (*!< DCMI interrupt enable register,                Address offset: 0x0C  *)
    MISR: longword;     (*!< DCMI masked interrupt status register,         Address offset: 0x10  *)
    ICR: longword;      (*!< DCMI interrupt clear register,                 Address offset: 0x14  *)
    ESCR: longword;     (*!< DCMI embedded synchronization code register,   Address offset: 0x18  *)
    ESUR: longword;     (*!< DCMI embedded synchronization unmask register, Address offset: 0x1C  *)
    CWSTRTR: longword;  (*!< DCMI crop window start,                        Address offset: 0x20  *)
    CWSIZER: longword;  (*!< DCMI crop window size,                         Address offset: 0x24  *)
    DR: longword;       (*!< DCMI data register,                            Address offset: 0x28  *)
  end;

  (**
  * @brief DMA Controller
   *)

  DMA_Stream_TypeDef = record
    CR: longword;    (*!< DMA stream x configuration register       *)
    NDTR: longword;  (*!< DMA stream x number of data register      *)
    PAR: longword;   (*!< DMA stream x peripheral address register  *)
    M0AR: longword;  (*!< DMA stream x memory 0 address register    *)
    M1AR: longword;  (*!< DMA stream x memory 1 address register    *)
    FCR: longword;   (*!< DMA stream x FIFO control register        *)
  end;

  DMA_TypeDef = record
    LISR: longword;   (*!< DMA low interrupt status register,      Address offset: 0x00  *)
    HISR: longword;   (*!< DMA high interrupt status register,     Address offset: 0x04  *)
    LIFCR: longword;  (*!< DMA low interrupt flag clear register,  Address offset: 0x08  *)
    HIFCR: longword;  (*!< DMA high interrupt flag clear register, Address offset: 0x0C  *)
  end;


  (**
  * @brief DMA2D Controller
   *)

  DMA2D_TypeDef = record
    CR: longword;       (*!< DMA2D Control Register,                         Address offset: 0x00  *)
    ISR: longword;      (*!< DMA2D Interrupt Status Register,                Address offset: 0x04  *)
    IFCR: longword;     (*!< DMA2D Interrupt Flag Clear Register,            Address offset: 0x08  *)
    FGMAR: longword;    (*!< DMA2D Foreground Memory Address Register,       Address offset: 0x0C  *)
    FGOR: longword;     (*!< DMA2D Foreground Offset Register,               Address offset: 0x10  *)
    BGMAR: longword;    (*!< DMA2D Background Memory Address Register,       Address offset: 0x14  *)
    BGOR: longword;     (*!< DMA2D Background Offset Register,               Address offset: 0x18  *)
    FGPFCCR: longword;  (*!< DMA2D Foreground PFC Control Register,          Address offset: 0x1C  *)
    FGCOLR: longword;   (*!< DMA2D Foreground Color Register,                Address offset: 0x20  *)
    BGPFCCR: longword;  (*!< DMA2D Background PFC Control Register,          Address offset: 0x24  *)
    BGCOLR: longword;   (*!< DMA2D Background Color Register,                Address offset: 0x28  *)
    FGCMAR: longword;   (*!< DMA2D Foreground CLUT Memory Address Register,  Address offset: 0x2C  *)
    BGCMAR: longword;   (*!< DMA2D Background CLUT Memory Address Register,  Address offset: 0x30  *)
    OPFCCR: longword;   (*!< DMA2D Output PFC Control Register,              Address offset: 0x34  *)
    OCOLR: longword;    (*!< DMA2D Output Color Register,                    Address offset: 0x38  *)
    OMAR: longword;     (*!< DMA2D Output Memory Address Register,           Address offset: 0x3C  *)
    OOR: longword;      (*!< DMA2D Output Offset Register,                   Address offset: 0x40  *)
    NLR: longword;      (*!< DMA2D Number of Line Register,                  Address offset: 0x44  *)
    LWR: longword;      (*!< DMA2D Line Watermark Register,                  Address offset: 0x48  *)
    AMTCR: longword;    (*!< DMA2D AHB Master Timer Configuration Register,  Address offset: 0x4C  *)
    RESERVED: array [0..235] of longword;  (*!< Reserved, 0x50-0x3FF  *)
    FGCLUT: array [0..255] of longword;  (*!< DMA2D Foreground CLUT,                          Address offset:400-7FF  *)
    BGCLUT: array [0..255] of longword;  (*!< DMA2D Background CLUT,                          Address offset:800-BFF  *)
  end;


  (**
  * @brief Ethernet MAC
   *)

  ETH_TypeDef = record
    MACCR: longword;
    MACFFR: longword;
    MACHTHR: longword;
    MACHTLR: longword;
    MACMIIAR: longword;
    MACMIIDR: longword;
    MACFCR: longword;
    MACVLANTR: longword;  (*    8  *)
    RESERVED0: array [0..1] of longword;
    MACRWUFFR: longword;  (*   11  *)
    MACPMTCSR: longword;
    RESERVED1: array [0..1] of longword;
    MACSR: longword;  (*   15  *)
    MACIMR: longword;
    MACA0HR: longword;
    MACA0LR: longword;
    MACA1HR: longword;
    MACA1LR: longword;
    MACA2HR: longword;
    MACA2LR: longword;
    MACA3HR: longword;
    MACA3LR: longword;  (*   24  *)
    RESERVED2: array [0..39] of longword;
    MMCCR: longword;  (*   65  *)
    MMCRIR: longword;
    MMCTIR: longword;
    MMCRIMR: longword;
    MMCTIMR: longword;  (*   69  *)
    RESERVED3: array [0..13] of longword;
    MMCTGFSCCR: longword;  (*   84  *)
    MMCTGFMSCCR: longword;
    RESERVED4: array [0..4] of longword;
    MMCTGFCR: longword;
    RESERVED5: array [0..9] of longword;
    MMCRFCECR: longword;
    MMCRFAECR: longword;
    RESERVED6: array [0..9] of longword;
    MMCRGUFCR: longword;
    RESERVED7: array [0..333] of longword;
    PTPTSCR: longword;
    PTPSSIR: longword;
    PTPTSHR: longword;
    PTPTSLR: longword;
    PTPTSHUR: longword;
    PTPTSLUR: longword;
    PTPTSAR: longword;
    PTPTTHR: longword;
    PTPTTLR: longword;
    RESERVED8: longword;
    PTPTSSR: longword;
    RESERVED9: array [0..564] of longword;
    DMABMR: longword;
    DMATPDR: longword;
    DMARPDR: longword;
    DMARDLAR: longword;
    DMATDLAR: longword;
    DMASR: longword;
    DMAOMR: longword;
    DMAIER: longword;
    DMAMFBOCR: longword;
    DMARSWTR: longword;
    RESERVED10: array [0..7] of longword;
    DMACHTDR: longword;
    DMACHRDR: longword;
    DMACHTBAR: longword;
    DMACHRBAR: longword;
  end;

  (**
  * @brief External Interrupt/Event Controller
   *)

  EXTI_TypeDef = record
    IMR: longword;    (*!< EXTI Interrupt mask register,            Address offset: 0x00  *)
    EMR: longword;    (*!< EXTI Event mask register,                Address offset: 0x04  *)
    RTSR: longword;   (*!< EXTI Rising trigger selection register,  Address offset: 0x08  *)
    FTSR: longword;   (*!< EXTI Falling trigger selection register, Address offset: 0x0C  *)
    SWIER: longword;  (*!< EXTI Software interrupt event register,  Address offset: 0x10  *)
    PR: longword;     (*!< EXTI Pending register,                   Address offset: 0x14  *)
  end;

  (**
  * @brief FLASH Registers
   *)

  FLASH_TypeDef = record
    ACR: longword;      (*!< FLASH access control register,     Address offset: 0x00  *)
    KEYR: longword;     (*!< FLASH key register,                Address offset: 0x04  *)
    OPTKEYR: longword;  (*!< FLASH option key register,         Address offset: 0x08  *)
    SR: longword;       (*!< FLASH status register,             Address offset: 0x0C  *)
    CR: longword;       (*!< FLASH control register,            Address offset: 0x10  *)
    OPTCR: longword;    (*!< FLASH option control register ,    Address offset: 0x14  *)
    OPTCR1: longword;   (*!< FLASH option control register 1 ,  Address offset: 0x18  *)
  end;


  (**
  * @brief Flexible Memory Controller
   *)

  FMC_Bank1_TypeDef = record
    BTCR: array [0..7] of longword;  (*!< NOR/PSRAM chip-select control register(BCR) and chip-select timing register(BTR), Address offset: 0x00-1C  *)
  end;

  (**
  * @brief Flexible Memory Controller Bank1E
   *)

  FMC_Bank1E_TypeDef = record
    BWTR: array [0..6] of longword;  (*!< NOR/PSRAM write timing registers, Address offset: 0x104-0x11C  *)
  end;

  (**
  * @brief Flexible Memory Controller Bank3
   *)

  FMC_Bank3_TypeDef = record
    PCR: longword;        (*!< NAND Flash control register,                       Address offset: 0x80  *)
    SR: longword;         (*!< NAND Flash FIFO status and interrupt register,     Address offset: 0x84  *)
    PMEM: longword;       (*!< NAND Flash Common memory space timing register,    Address offset: 0x88  *)
    PATT: longword;       (*!< NAND Flash Attribute memory space timing register, Address offset: 0x8C  *)
    RESERVED0: longword;  (*!< Reserved, 0x90                                                           *)
    ECCR: longword;       (*!< NAND Flash ECC result registers,                   Address offset: 0x94  *)
  end;

  (**
  * @brief Flexible Memory Controller Bank5_6
   *)

  FMC_Bank5_6_TypeDef = record
    SDCR: array [0..1] of longword;  (*!< SDRAM Control registers ,      Address offset: 0x140-0x144   *)
    SDTR: array [0..1] of longword;  (*!< SDRAM Timing registers ,       Address offset: 0x148-0x14C   *)
    SDCMR: longword;  (*!< SDRAM Command Mode register,    Address offset: 0x150   *)
    SDRTR: longword;  (*!< SDRAM Refresh Timer register,   Address offset: 0x154   *)
    SDSR: longword;   (*!< SDRAM Status register,          Address offset: 0x158   *)
  end;


  (**
  * @brief General Purpose I/O
   *)

  GPIO_TypeDef = record
    MODER: longword;    (*!< GPIO port mode register,               Address offset: 0x00       *)
    OTYPER: longword;   (*!< GPIO port output type register,        Address offset: 0x04       *)
    OSPEEDR: longword;  (*!< GPIO port output speed register,       Address offset: 0x08       *)
    PUPDR: longword;    (*!< GPIO port pull-up/pull-down register,  Address offset: 0x0C       *)
    IDR: longword;      (*!< GPIO port input data register,         Address offset: 0x10       *)
    ODR: longword;      (*!< GPIO port output data register,        Address offset: 0x14       *)
    BSRR: longword;     (*!< GPIO port bit set/reset register,      Address offset: 0x18       *)
    LCKR: longword;     (*!< GPIO port configuration lock register, Address offset: 0x1C       *)
    AFR: array [0..1] of longword;  (*!< GPIO alternate function registers,     Address offset: 0x20-0x24  *)
  end;

  (**
  * @brief System configuration controller
   *)

  SYSCFG_TypeDef = record
    MEMRMP: longword;  (*!< SYSCFG memory remap register,                      Address offset: 0x00       *)
    PMC: longword;     (*!< SYSCFG peripheral mode configuration register,     Address offset: 0x04       *)
    EXTICR: array [0..3] of longword;  (*!< SYSCFG external interrupt configuration registers, Address offset: 0x08-0x14  *)
    RESERVED: array [0..1] of longword;  (*!< Reserved, 0x18-0x1C                                                           *)
    CMPCR: longword;   (*!< SYSCFG Compensation cell control register,         Address offset: 0x20       *)
  end;

  (**
  * @brief Inter-integrated Circuit Interface
   *)

  I2C_TypeDef = record
    CR1: longword;       (*!< I2C Control register 1,            Address offset: 0x00  *)
    CR2: longword;       (*!< I2C Control register 2,            Address offset: 0x04  *)
    OAR1: longword;      (*!< I2C Own address 1 register,        Address offset: 0x08  *)
    OAR2: longword;      (*!< I2C Own address 2 register,        Address offset: 0x0C  *)
    TIMINGR: longword;   (*!< I2C Timing register,               Address offset: 0x10  *)
    TIMEOUTR: longword;  (*!< I2C Timeout register,              Address offset: 0x14  *)
    ISR: longword;       (*!< I2C Interrupt and status register, Address offset: 0x18  *)
    ICR: longword;       (*!< I2C Interrupt clear register,      Address offset: 0x1C  *)
    PECR: longword;      (*!< I2C PEC register,                  Address offset: 0x20  *)
    RXDR: longword;      (*!< I2C Receive data register,         Address offset: 0x24  *)
    TXDR: longword;      (*!< I2C Transmit data register,        Address offset: 0x28  *)
  end;

  (**
  * @brief Independent WATCHDOG
   *)

  IWDG_TypeDef = record
    KR: longword;    (*!< IWDG Key register,       Address offset: 0x00  *)
    PR: longword;    (*!< IWDG Prescaler register, Address offset: 0x04  *)
    RLR: longword;   (*!< IWDG Reload register,    Address offset: 0x08  *)
    SR: longword;    (*!< IWDG Status register,    Address offset: 0x0C  *)
    WINR: longword;  (*!< IWDG Window register,    Address offset: 0x10  *)
  end;


  (**
  * @brief LCD-TFT Display Controller
   *)

  LTDC_TypeDef = record
    RESERVED0: array [0..1] of longword;  (*!< Reserved, 0x00-0x04  *)
    SSCR: longword;   (*!< LTDC Synchronization Size Configuration Register,    Address offset: 0x08  *)
    BPCR: longword;   (*!< LTDC Back Porch Configuration Register,              Address offset: 0x0C  *)
    AWCR: longword;   (*!< LTDC Active Width Configuration Register,            Address offset: 0x10  *)
    TWCR: longword;   (*!< LTDC Total Width Configuration Register,             Address offset: 0x14  *)
    GCR: longword;    (*!< LTDC Global Control Register,                        Address offset: 0x18  *)
    RESERVED1: array [0..1] of longword;  (*!< Reserved, 0x1C-0x20  *)
    SRCR: longword;   (*!< LTDC Shadow Reload Configuration Register,           Address offset: 0x24  *)
    RESERVED2: array [0..0] of longword;  (*!< Reserved, 0x28  *)
    BCCR: longword;   (*!< LTDC Background Color Configuration Register,        Address offset: 0x2C  *)
    RESERVED3: array [0..0] of longword;  (*!< Reserved, 0x30  *)
    IER: longword;    (*!< LTDC Interrupt Enable Register,                      Address offset: 0x34  *)
    ISR: longword;    (*!< LTDC Interrupt Status Register,                      Address offset: 0x38  *)
    ICR: longword;    (*!< LTDC Interrupt Clear Register,                       Address offset: 0x3C  *)
    LIPCR: longword;  (*!< LTDC Line Interrupt Position Configuration Register, Address offset: 0x40  *)
    CPSR: longword;   (*!< LTDC Current Position Status Register,               Address offset: 0x44  *)
    CDSR: longword;   (*!< LTDC Current Display Status Register,                 Address offset: 0x48  *)
  end;

  (**
  * @brief LCD-TFT Display layer x Controller
   *)

  LTDC_Layer_TypeDef = record
    CR: longword;      (*!< LTDC Layerx Control Register                                  Address offset: 0x84  *)
    WHPCR: longword;   (*!< LTDC Layerx Window Horizontal Position Configuration Register Address offset: 0x88  *)
    WVPCR: longword;   (*!< LTDC Layerx Window Vertical Position Configuration Register   Address offset: 0x8C  *)
    CKCR: longword;    (*!< LTDC Layerx Color Keying Configuration Register               Address offset: 0x90  *)
    PFCR: longword;    (*!< LTDC Layerx Pixel Format Configuration Register               Address offset: 0x94  *)
    CACR: longword;    (*!< LTDC Layerx Constant Alpha Configuration Register             Address offset: 0x98  *)
    DCCR: longword;    (*!< LTDC Layerx Default Color Configuration Register              Address offset: 0x9C  *)
    BFCR: longword;    (*!< LTDC Layerx Blending Factors Configuration Register           Address offset: 0xA0  *)
    RESERVED0: array [0..1] of longword;  (*!< Reserved  *)
    CFBAR: longword;   (*!< LTDC Layerx Color Frame Buffer Address Register               Address offset: 0xAC  *)
    CFBLR: longword;   (*!< LTDC Layerx Color Frame Buffer Length Register                Address offset: 0xB0  *)
    CFBLNR: longword;  (*!< LTDC Layerx ColorFrame Buffer Line Number Register            Address offset: 0xB4  *)
    RESERVED1: array [0..2] of longword;  (*!< Reserved  *)
    CLUTWR: longword;  (*!< LTDC Layerx CLUT Write Register                              Address offset: 0x144  *)
  end;


  (**
  * @brief Power Control
   *)

  PWR_TypeDef = record
    CR1: longword;   (*!< PWR power control register 1,        Address offset: 0x00  *)
    CSR1: longword;  (*!< PWR power control/status register 2, Address offset: 0x04  *)
    CR2: longword;   (*!< PWR power control register 2,        Address offset: 0x08  *)
    CSR2: longword;  (*!< PWR power control/status register 2, Address offset: 0x0C  *)
  end;


  (**
  * @brief Reset and Clock Control
   *)

  RCC_TypeDef = record
    CR: longword;         (*!< RCC clock control register,                                  Address offset: 0x00  *)
    PLLCFGR: longword;    (*!< RCC PLL configuration register,                              Address offset: 0x04  *)
    CFGR: longword;       (*!< RCC clock configuration register,                            Address offset: 0x08  *)
    CIR: longword;        (*!< RCC clock interrupt register,                                Address offset: 0x0C  *)
    AHB1RSTR: longword;   (*!< RCC AHB1 peripheral reset register,                          Address offset: 0x10  *)
    AHB2RSTR: longword;   (*!< RCC AHB2 peripheral reset register,                          Address offset: 0x14  *)
    AHB3RSTR: longword;   (*!< RCC AHB3 peripheral reset register,                          Address offset: 0x18  *)
    RESERVED0: longword;  (*!< Reserved, 0x1C                                                                     *)
    APB1RSTR: longword;   (*!< RCC APB1 peripheral reset register,                          Address offset: 0x20  *)
    APB2RSTR: longword;   (*!< RCC APB2 peripheral reset register,                          Address offset: 0x24  *)
    RESERVED1: array [0..1] of longword;  (*!< Reserved, 0x28-0x2C                                                                *)
    AHB1ENR: longword;    (*!< RCC AHB1 peripheral clock register,                          Address offset: 0x30  *)
    AHB2ENR: longword;    (*!< RCC AHB2 peripheral clock register,                          Address offset: 0x34  *)
    AHB3ENR: longword;    (*!< RCC AHB3 peripheral clock register,                          Address offset: 0x38  *)
    RESERVED2: longword;  (*!< Reserved, 0x3C                                                                     *)
    APB1ENR: longword;    (*!< RCC APB1 peripheral clock enable register,                   Address offset: 0x40  *)
    APB2ENR: longword;    (*!< RCC APB2 peripheral clock enable register,                   Address offset: 0x44  *)
    RESERVED3: array [0..1] of longword;  (*!< Reserved, 0x48-0x4C                                                                *)
    AHB1LPENR: longword;  (*!< RCC AHB1 peripheral clock enable in low power mode register, Address offset: 0x50  *)
    AHB2LPENR: longword;  (*!< RCC AHB2 peripheral clock enable in low power mode register, Address offset: 0x54  *)
    AHB3LPENR: longword;  (*!< RCC AHB3 peripheral clock enable in low power mode register, Address offset: 0x58  *)
    RESERVED4: longword;  (*!< Reserved, 0x5C                                                                     *)
    APB1LPENR: longword;  (*!< RCC APB1 peripheral clock enable in low power mode register, Address offset: 0x60  *)
    APB2LPENR: longword;  (*!< RCC APB2 peripheral clock enable in low power mode register, Address offset: 0x64  *)
    RESERVED5: array [0..1] of longword;  (*!< Reserved, 0x68-0x6C                                                                *)
    BDCR: longword;       (*!< RCC Backup domain control register,                          Address offset: 0x70  *)
    CSR: longword;        (*!< RCC clock control & status register,                         Address offset: 0x74  *)
    RESERVED6: array [0..1] of longword;  (*!< Reserved, 0x78-0x7C                                                                *)
    SSCGR: longword;      (*!< RCC spread spectrum clock generation register,               Address offset: 0x80  *)
    PLLI2SCFGR: longword;  (*!< RCC PLLI2S configuration register,                           Address offset: 0x84  *)
    PLLSAICFGR: longword;  (*!< RCC PLLSAI configuration register,                           Address offset: 0x88  *)
    DCKCFGR1: longword;   (*!< RCC Dedicated Clocks configuration register1,                 Address offset: 0x8C  *)
    DCKCFGR2: longword;   (*!< RCC Dedicated Clocks configuration register 2,               Address offset: 0x90  *)
  end;

  (**
  * @brief Real-Time Clock
   *)

  RTC_TypeDef = record
    TR: longword;        (*!< RTC time register,                                         Address offset: 0x00  *)
    DR: longword;        (*!< RTC date register,                                         Address offset: 0x04  *)
    CR: longword;        (*!< RTC control register,                                      Address offset: 0x08  *)
    ISR: longword;       (*!< RTC initialization and status register,                    Address offset: 0x0C  *)
    PRER: longword;      (*!< RTC prescaler register,                                    Address offset: 0x10  *)
    WUTR: longword;      (*!< RTC wakeup timer register,                                 Address offset: 0x14  *)
    reserved: longword;  (*!< Reserved   *)
    ALRMAR: longword;    (*!< RTC alarm A register,                                      Address offset: 0x1C  *)
    ALRMBR: longword;    (*!< RTC alarm B register,                                      Address offset: 0x20  *)
    WPR: longword;       (*!< RTC write protection register,                             Address offset: 0x24  *)
    SSR: longword;       (*!< RTC sub second register,                                   Address offset: 0x28  *)
    SHIFTR: longword;    (*!< RTC shift control register,                                Address offset: 0x2C  *)
    TSTR: longword;      (*!< RTC time stamp time register,                              Address offset: 0x30  *)
    TSDR: longword;      (*!< RTC time stamp date register,                              Address offset: 0x34  *)
    TSSSR: longword;     (*!< RTC time-stamp sub second register,                        Address offset: 0x38  *)
    CALR: longword;      (*!< RTC calibration register,                                  Address offset: 0x3C  *)
    TAMPCR: longword;    (*!< RTC tamper configuration register,                         Address offset: 0x40  *)
    ALRMASSR: longword;  (*!< RTC alarm A sub second register,                           Address offset: 0x44  *)
    ALRMBSSR: longword;  (*!< RTC alarm B sub second register,                           Address offset: 0x48  *)
    OR_: longword;       (*!< RTC option register,                                       Address offset: 0x4C  *)
    BKP0R: longword;     (*!< RTC backup register 0,                                     Address offset: 0x50  *)
    BKP1R: longword;     (*!< RTC backup register 1,                                     Address offset: 0x54  *)
    BKP2R: longword;     (*!< RTC backup register 2,                                     Address offset: 0x58  *)
    BKP3R: longword;     (*!< RTC backup register 3,                                     Address offset: 0x5C  *)
    BKP4R: longword;     (*!< RTC backup register 4,                                     Address offset: 0x60  *)
    BKP5R: longword;     (*!< RTC backup register 5,                                     Address offset: 0x64  *)
    BKP6R: longword;     (*!< RTC backup register 6,                                     Address offset: 0x68  *)
    BKP7R: longword;     (*!< RTC backup register 7,                                     Address offset: 0x6C  *)
    BKP8R: longword;     (*!< RTC backup register 8,                                     Address offset: 0x70  *)
    BKP9R: longword;     (*!< RTC backup register 9,                                     Address offset: 0x74  *)
    BKP10R: longword;    (*!< RTC backup register 10,                                    Address offset: 0x78  *)
    BKP11R: longword;    (*!< RTC backup register 11,                                    Address offset: 0x7C  *)
    BKP12R: longword;    (*!< RTC backup register 12,                                    Address offset: 0x80  *)
    BKP13R: longword;    (*!< RTC backup register 13,                                    Address offset: 0x84  *)
    BKP14R: longword;    (*!< RTC backup register 14,                                    Address offset: 0x88  *)
    BKP15R: longword;    (*!< RTC backup register 15,                                    Address offset: 0x8C  *)
    BKP16R: longword;    (*!< RTC backup register 16,                                    Address offset: 0x90  *)
    BKP17R: longword;    (*!< RTC backup register 17,                                    Address offset: 0x94  *)
    BKP18R: longword;    (*!< RTC backup register 18,                                    Address offset: 0x98  *)
    BKP19R: longword;    (*!< RTC backup register 19,                                    Address offset: 0x9C  *)
    BKP20R: longword;    (*!< RTC backup register 20,                                    Address offset: 0xA0  *)
    BKP21R: longword;    (*!< RTC backup register 21,                                    Address offset: 0xA4  *)
    BKP22R: longword;    (*!< RTC backup register 22,                                    Address offset: 0xA8  *)
    BKP23R: longword;    (*!< RTC backup register 23,                                    Address offset: 0xAC  *)
    BKP24R: longword;    (*!< RTC backup register 24,                                    Address offset: 0xB0  *)
    BKP25R: longword;    (*!< RTC backup register 25,                                    Address offset: 0xB4  *)
    BKP26R: longword;    (*!< RTC backup register 26,                                    Address offset: 0xB8  *)
    BKP27R: longword;    (*!< RTC backup register 27,                                    Address offset: 0xBC  *)
    BKP28R: longword;    (*!< RTC backup register 28,                                    Address offset: 0xC0  *)
    BKP29R: longword;    (*!< RTC backup register 29,                                    Address offset: 0xC4  *)
    BKP30R: longword;    (*!< RTC backup register 30,                                    Address offset: 0xC8  *)
    BKP31R: longword;    (*!< RTC backup register 31,                                    Address offset: 0xCC  *)
  end;


  (**
  * @brief Serial Audio Interface
   *)

  SAI_TypeDef = record
    GCR: longword;  (*!< SAI global configuration register,        Address offset: 0x00  *)
  end;

  SAI_Block_TypeDef = record
    CR1: longword;    (*!< SAI block x configuration register 1,     Address offset: 0x04  *)
    CR2: longword;    (*!< SAI block x configuration register 2,     Address offset: 0x08  *)
    FRCR: longword;   (*!< SAI block x frame configuration register, Address offset: 0x0C  *)
    SLOTR: longword;  (*!< SAI block x slot register,                Address offset: 0x10  *)
    IMR: longword;    (*!< SAI block x interrupt mask register,      Address offset: 0x14  *)
    SR: longword;     (*!< SAI block x status register,              Address offset: 0x18  *)
    CLRFR: longword;  (*!< SAI block x clear flag register,          Address offset: 0x1C  *)
    DR: longword;     (*!< SAI block x data register,                Address offset: 0x20  *)
  end;

  (**
  * @brief SPDIF-RX Interface
   *)

  SPDIFRX_TypeDef = record
    CR: longword;    (*!< Control register,                   Address offset: 0x00  *)
    IMR: longword;   (*!< Interrupt mask register,            Address offset: 0x04  *)
    SR: longword;    (*!< Status register,                    Address offset: 0x08  *)
    IFCR: longword;  (*!< Interrupt Flag Clear register,      Address offset: 0x0C  *)
    DR: longword;    (*!< Data input register,                Address offset: 0x10  *)
    CSR: longword;   (*!< Channel Status register,            Address offset: 0x14  *)
    DIR: longword;   (*!< Debug Information register,         Address offset: 0x18  *)
  end;


  (**
  * @brief SD host Interface
   *)

  SDMMC_TypeDef = record
    POWER: longword;    (*!< SDMMC power control register,    Address offset: 0x00  *)
    CLKCR: longword;    (*!< SDMMClock control register,     Address offset: 0x04  *)
    ARG: longword;      (*!< SDMMC argument register,         Address offset: 0x08  *)
    CMD: longword;      (*!< SDMMC command register,          Address offset: 0x0C  *)
    RESPCMD: longword;  (*!< SDMMC command response register, Address offset: 0x10  *)
    RESP1: longword;    (*!< SDMMC response 1 register,       Address offset: 0x14  *)
    RESP2: longword;    (*!< SDMMC response 2 register,       Address offset: 0x18  *)
    RESP3: longword;    (*!< SDMMC response 3 register,       Address offset: 0x1C  *)
    RESP4: longword;    (*!< SDMMC response 4 register,       Address offset: 0x20  *)
    DTIMER: longword;   (*!< SDMMC data timer register,       Address offset: 0x24  *)
    DLEN: longword;     (*!< SDMMC data length register,      Address offset: 0x28  *)
    DCTRL: longword;    (*!< SDMMC data control register,     Address offset: 0x2C  *)
    DCOUNT: longword;   (*!< SDMMC data counter register,     Address offset: 0x30  *)
    STA: longword;      (*!< SDMMC status register,           Address offset: 0x34  *)
    ICR: longword;      (*!< SDMMC interrupt clear register,  Address offset: 0x38  *)
    MASK: longword;     (*!< SDMMC mask register,             Address offset: 0x3C  *)
    RESERVED0: array [0..1] of longword;  (*!< Reserved, 0x40-0x44                                   *)
    FIFOCNT: longword;  (*!< SDMMC FIFO counter register,     Address offset: 0x48  *)
    RESERVED1: array [0..12] of longword;  (*!< Reserved, 0x4C-0x7C                                   *)
    FIFO: longword;     (*!< SDMMC data FIFO register,        Address offset: 0x80  *)
  end;

  (**
  * @brief Serial Peripheral Interface
   *)

  SPI_TypeDef = record
    CR1: longword;      (*!< SPI control register 1 (not used in I2S mode),      Address offset: 0x00  *)
    CR2: longword;      (*!< SPI control register 2,                             Address offset: 0x04  *)
    SR: longword;       (*!< SPI status register,                                Address offset: 0x08  *)
    DR: longword;       (*!< SPI data register,                                  Address offset: 0x0C  *)
    CRCPR: longword;    (*!< SPI CRC polynomial register (not used in I2S mode), Address offset: 0x10  *)
    RXCRCR: longword;   (*!< SPI RX CRC register (not used in I2S mode),         Address offset: 0x14  *)
    TXCRCR: longword;   (*!< SPI TX CRC register (not used in I2S mode),         Address offset: 0x18  *)
    I2SCFGR: longword;  (*!< SPI_I2S configuration register,                     Address offset: 0x1C  *)
    I2SPR: longword;    (*!< SPI_I2S prescaler register,                         Address offset: 0x20  *)
  end;

  (**
  * @brief QUAD Serial Peripheral Interface
   *)

  QUADSPI_TypeDef = record
    CR: longword;     (*!< QUADSPI Control register,                           Address offset: 0x00  *)
    DCR: longword;    (*!< QUADSPI Device Configuration register,              Address offset: 0x04  *)
    SR: longword;     (*!< QUADSPI Status register,                            Address offset: 0x08  *)
    FCR: longword;    (*!< QUADSPI Flag Clear register,                        Address offset: 0x0C  *)
    DLR: longword;    (*!< QUADSPI Data Length register,                       Address offset: 0x10  *)
    CCR: longword;    (*!< QUADSPI Communication Configuration register,       Address offset: 0x14  *)
    AR: longword;     (*!< QUADSPI Address register,                           Address offset: 0x18  *)
    ABR: longword;    (*!< QUADSPI Alternate Bytes register,                   Address offset: 0x1C  *)
    DR: longword;     (*!< QUADSPI Data register,                              Address offset: 0x20  *)
    PSMKR: longword;  (*!< QUADSPI Polling Status Mask register,               Address offset: 0x24  *)
    PSMAR: longword;  (*!< QUADSPI Polling Status Match register,              Address offset: 0x28  *)
    PIR: longword;    (*!< QUADSPI Polling Interval register,                  Address offset: 0x2C  *)
    LPTR: longword;   (*!< QUADSPI Low Power Timeout register,                 Address offset: 0x30  *)
  end;

  (**
  * @brief TIM
   *)

  TIM_TypeDef = record
    CR1: longword;    (*!< TIM control register 1,              Address offset: 0x00  *)
    CR2: longword;    (*!< TIM control register 2,              Address offset: 0x04  *)
    SMCR: longword;   (*!< TIM slave mode control register,     Address offset: 0x08  *)
    DIER: longword;   (*!< TIM DMA/interrupt enable register,   Address offset: 0x0C  *)
    SR: longword;     (*!< TIM status register,                 Address offset: 0x10  *)
    EGR: longword;    (*!< TIM event generation register,       Address offset: 0x14  *)
    CCMR1: longword;  (*!< TIM capture/compare mode register 1, Address offset: 0x18  *)
    CCMR2: longword;  (*!< TIM capture/compare mode register 2, Address offset: 0x1C  *)
    CCER: longword;   (*!< TIM capture/compare enable register, Address offset: 0x20  *)
    CNT: longword;    (*!< TIM counter register,                Address offset: 0x24  *)
    PSC: longword;    (*!< TIM prescaler,                       Address offset: 0x28  *)
    ARR: longword;    (*!< TIM auto-reload register,            Address offset: 0x2C  *)
    RCR: longword;    (*!< TIM repetition counter register,     Address offset: 0x30  *)
    CCR1: longword;   (*!< TIM capture/compare register 1,      Address offset: 0x34  *)
    CCR2: longword;   (*!< TIM capture/compare register 2,      Address offset: 0x38  *)
    CCR3: longword;   (*!< TIM capture/compare register 3,      Address offset: 0x3C  *)
    CCR4: longword;   (*!< TIM capture/compare register 4,      Address offset: 0x40  *)
    BDTR: longword;   (*!< TIM break and dead-time register,    Address offset: 0x44  *)
    DCR: longword;    (*!< TIM DMA control register,            Address offset: 0x48  *)
    DMAR: longword;   (*!< TIM DMA address for full transfer,   Address offset: 0x4C  *)
    OR_: longword;    (*!< TIM option register,                 Address offset: 0x50  *)
    CCMR3: longword;  (*!< TIM capture/compare mode register 3,      Address offset: 0x54  *)
    CCR5: longword;   (*!< TIM capture/compare mode register5,       Address offset: 0x58  *)
    CCR6: longword;   (*!< TIM capture/compare mode register6,       Address offset: 0x5C  *)
  end;

  (**
  * @brief LPTIMIMER
   *)

  LPTIM_TypeDef = record
    ISR: longword;   (*!< LPTIM Interrupt and Status register,                Address offset: 0x00  *)
    ICR: longword;   (*!< LPTIM Interrupt Clear register,                     Address offset: 0x04  *)
    IER: longword;   (*!< LPTIM Interrupt Enable register,                    Address offset: 0x08  *)
    CFGR: longword;  (*!< LPTIM Configuration register,                       Address offset: 0x0C  *)
    CR: longword;    (*!< LPTIM Control register,                             Address offset: 0x10  *)
    CMP: longword;   (*!< LPTIM Compare register,                             Address offset: 0x14  *)
    ARR: longword;   (*!< LPTIM Autoreload register,                          Address offset: 0x18  *)
    CNT: longword;   (*!< LPTIM Counter register,                             Address offset: 0x1C  *)
    OR_: longword;   (*!< LPTIM Option register,                              Address offset: 0x20  *)
  end;


  (**
  * @brief Universal Synchronous Asynchronous Receiver Transmitter
   *)

  USART_TypeDef = record
    CR1: longword;   (*!< USART Control register 1,                 Address offset: 0x00  *)
    CR2: longword;   (*!< USART Control register 2,                 Address offset: 0x04  *)
    CR3: longword;   (*!< USART Control register 3,                 Address offset: 0x08  *)
    BRR: longword;   (*!< USART Baud rate register,                 Address offset: 0x0C  *)
    GTPR: longword;  (*!< USART Guard time and prescaler register,  Address offset: 0x10  *)
    RTOR: longword;  (*!< USART Receiver Time Out register,         Address offset: 0x14  *)
    RQR: longword;   (*!< USART Request register,                   Address offset: 0x18  *)
    ISR: longword;   (*!< USART Interrupt and status register,      Address offset: 0x1C  *)
    ICR: longword;   (*!< USART Interrupt flag Clear register,      Address offset: 0x20  *)
    RDR: longword;   (*!< USART Receive Data register,              Address offset: 0x24  *)
    TDR: longword;   (*!< USART Transmit Data register,             Address offset: 0x28  *)
  end;


  (**
  * @brief Window WATCHDOG
   *)

  WWDG_TypeDef = record
    CR: longword;   (*!< WWDG Control register,       Address offset: 0x00  *)
    CFR: longword;  (*!< WWDG Configuration register, Address offset: 0x04  *)
    SR: longword;   (*!< WWDG Status register,        Address offset: 0x08  *)
  end;

  (**
  * @brief RNG
   *)

  RNG_TypeDef = record
    CR: longword;  (*!< RNG control register, Address offset: 0x00  *)
    SR: longword;  (*!< RNG status register,  Address offset: 0x04  *)
    DR: longword;  (*!< RNG data register,    Address offset: 0x08  *)
  end;

  (**
  * @}
   *)

  (**
  * @brief USB_OTG_Core_Registers
   *)

  USB_OTG_GlobalTypeDef = record
    GOTGCTL: longword;    (*!< USB_OTG Control and Status Register          000h  *)
    GOTGINT: longword;    (*!< USB_OTG Interrupt Register                   004h  *)
    GAHBCFG: longword;    (*!< Core AHB Configuration Register              008h  *)
    GUSBCFG: longword;    (*!< Core USB Configuration Register              00Ch  *)
    GRSTCTL: longword;    (*!< Core Reset Register                          010h  *)
    GINTSTS: longword;    (*!< Core Interrupt Register                      014h  *)
    GINTMSK: longword;    (*!< Core Interrupt Mask Register                 018h  *)
    GRXSTSR: longword;    (*!< Receive Sts Q Read Register                  01Ch  *)
    GRXSTSP: longword;    (*!< Receive Sts Q Read & POP Register            020h  *)
    GRXFSIZ: longword;    (*!< Receive FIFO Size Register                   024h  *)
    DIEPTXF0_HNPTXFSIZ: longword;  (*!< EP0 / Non Periodic Tx FIFO Size Register     028h  *)
    HNPTXSTS: longword;   (*!< Non Periodic Tx FIFO/Queue Sts reg           02Ch  *)
    Reserved30: array [0..1] of longword;  (*!< Reserved                                     030h  *)
    GCCFG: longword;      (*!< General Purpose IO Register                  038h  *)
    CID: longword;        (*!< User ID Register                             03Ch  *)
    Reserved5: array [0..2] of longword;  (*!< Reserved                                040h-048h  *)
    GHWCFG3: longword;    (*!< User HW config3                              04Ch  *)
    Reserved6: longword;  (*!< Reserved                                     050h  *)
    GLPMCFG: longword;    (*!< LPM Register                                 054h  *)
    GPWRDN: longword;     (*!< Power Down Register                          058h  *)
    GDFIFOCFG: longword;  (*!< DFIFO Software Config Register               05Ch  *)
    GADPCTL: longword;    (*!< ADP Timer, Control and Status Register       60Ch  *)
    Reserved43: array [0..38] of longword;  (*!< Reserved                                058h-0FFh  *)
    HPTXFSIZ: longword;   (*!< Host Periodic Tx FIFO Size Reg               100h  *)
    DIEPTXF: array [0..14] of longword;  (*!< dev Periodic Transmit FIFO  *)
  end;


  (**
  * @brief USB_OTG_device_Registers
   *)

  USB_OTG_DeviceTypeDef = record
    DCFG: longword;        (*!< dev Configuration Register   800h  *)
    DCTL: longword;        (*!< dev Control Register         804h  *)
    DSTS: longword;        (*!< dev Status Register (RO)     808h  *)
    Reserved0C: longword;  (*!< Reserved                     80Ch  *)
    DIEPMSK: longword;     (*!< dev IN Endpoint Mask         810h  *)
    DOEPMSK: longword;     (*!< dev OUT Endpoint Mask        814h  *)
    DAINT: longword;       (*!< dev All Endpoints Itr Reg    818h  *)
    DAINTMSK: longword;    (*!< dev All Endpoints Itr Mask   81Ch  *)
    Reserved20: longword;  (*!< Reserved                     820h  *)
    Reserved9: longword;   (*!< Reserved                     824h  *)
    DVBUSDIS: longword;    (*!< dev VBUS discharge Register  828h  *)
    DVBUSPULSE: longword;  (*!< dev VBUS Pulse Register      82Ch  *)
    DTHRCTL: longword;     (*!< dev threshold                830h  *)
    DIEPEMPMSK: longword;  (*!< dev empty msk                834h  *)
    DEACHINT: longword;    (*!< dedicated EP interrupt       838h  *)
    DEACHMSK: longword;    (*!< dedicated EP msk             83Ch  *)
    Reserved40: longword;  (*!< dedicated EP mask            840h  *)
    DINEP1MSK: longword;   (*!< dedicated EP mask            844h  *)
    Reserved44: array [0..14] of longword;  (*!< Reserved                 844-87Ch  *)
    DOUTEP1MSK: longword;  (*!< dedicated EP msk             884h  *)
  end;


  (**
  * @brief USB_OTG_IN_Endpoint-Specific_Register
   *)

  USB_OTG_INEndpointTypeDef = record
    DIEPCTL: longword;     (*!< dev IN Endpoint Control Reg    900h + (ep_num * 20h) + 00h  *)
    Reserved04: longword;  (*!< Reserved                       900h + (ep_num * 20h) + 04h  *)
    DIEPINT: longword;     (*!< dev IN Endpoint Itr Reg        900h + (ep_num * 20h) + 08h  *)
    Reserved0C: longword;  (*!< Reserved                       900h + (ep_num * 20h) + 0Ch  *)
    DIEPTSIZ: longword;    (*!< IN Endpoint Txfer Size         900h + (ep_num * 20h) + 10h  *)
    DIEPDMA: longword;     (*!< IN Endpoint DMA Address Reg    900h + (ep_num * 20h) + 14h  *)
    DTXFSTS: longword;     (*!< IN Endpoint Tx FIFO Status Reg 900h + (ep_num * 20h) + 18h  *)
    Reserved18: longword;  (*!< Reserved  900h+(ep_num*20h)+1Ch-900h+ (ep_num * 20h) + 1Ch  *)
  end;


  (**
  * @brief USB_OTG_OUT_Endpoint-Specific_Registers
   *)

  USB_OTG_OUTEndpointTypeDef = record
    DOEPCTL: longword;     (*!< dev OUT Endpoint Control Reg           B00h + (ep_num * 20h) + 00h  *)
    Reserved04: longword;  (*!< Reserved                               B00h + (ep_num * 20h) + 04h  *)
    DOEPINT: longword;     (*!< dev OUT Endpoint Itr Reg               B00h + (ep_num * 20h) + 08h  *)
    Reserved0C: longword;  (*!< Reserved                               B00h + (ep_num * 20h) + 0Ch  *)
    DOEPTSIZ: longword;    (*!< dev OUT Endpoint Txfer Size            B00h + (ep_num * 20h) + 10h  *)
    DOEPDMA: longword;     (*!< dev OUT Endpoint DMA Address           B00h + (ep_num * 20h) + 14h  *)
    Reserved18: array [0..1] of longword;  (*!< Reserved B00h + (ep_num * 20h) + 18h - B00h + (ep_num * 20h) + 1Ch  *)
  end;


  (**
  * @brief USB_OTG_Host_Mode_Register_Structures
   *)

  USB_OTG_HostTypeDef = record
    HCFG: longword;         (*!< Host Configuration Register          400h  *)
    HFIR: longword;         (*!< Host Frame Interval Register         404h  *)
    HFNUM: longword;        (*!< Host Frame Nbr/Frame Remaining       408h  *)
    Reserved40C: longword;  (*!< Reserved                             40Ch  *)
    HPTXSTS: longword;      (*!< Host Periodic Tx FIFO/ Queue Status  410h  *)
    HAINT: longword;        (*!< Host All Channels Interrupt Register 414h  *)
    HAINTMSK: longword;     (*!< Host All Channels Interrupt Mask     418h  *)
  end;

  (**
  * @brief USB_OTG_Host_Channel_Specific_Registers
   *)

  USB_OTG_HostChannelTypeDef = record
    HCCHAR: longword;    (*!< Host Channel Characteristics Register    500h  *)
    HCSPLT: longword;    (*!< Host Channel Split Control Register      504h  *)
    HCINT: longword;     (*!< Host Channel Interrupt Register          508h  *)
    HCINTMSK: longword;  (*!< Host Channel Interrupt Mask Register     50Ch  *)
    HCTSIZ: longword;    (*!< Host Channel Transfer Size Register      510h  *)
    HCDMA: longword;     (*!< Host Channel DMA Address Register        514h  *)
    Reserved: array [0..1] of longword;  (*!< Reserved                                       *)
  end;

  (**
  * @}
   *)


  (** @addtogroup Peripheral_memory_map
  * @{
   *)

const
  RAMITCM_BASE   = $00000000;  (*!< Base address of :16KB RAM reserved for CPU execution/instruction accessible over ITCM    *)
  FLASHITCM_BASE = $00200000;  (*!< Base address of :(up to 1 MB) embedded FLASH memory  accessible over ITCM                *)
  FLASHAXI_BASE  = $08000000;  (*!< Base address of : (up to 1 MB) embedded FLASH memory accessible over AXI                 *)
  RAMDTCM_BASE   = $20000000;  (*!< Base address of : 64KB system data RAM accessible over DTCM                              *)
  SRAM1_BASE     = $20010000;  (*!< Base address of : 240KB RAM1 accessible over AXI/AHB                                     *)
  SRAM2_BASE     = $2004C000;  (*!< Base address of : 16KB RAM2 accessible over AXI/AHB                                      *)
  PERIPH_BASE    = $40000000;  (*!< Base address of : AHB/ABP Peripherals                                                    *)
  BKPSRAM_BASE   = $40024000;  (*!< Base address of : Backup SRAM(4 KB)                                                      *)
  QSPI_BASE      = $90000000;  (*!< Base address of : QSPI memories  accessible over AXI                                     *)
  FMC_R_BASE     = $A0000000;  (*!< Base address of : FMC Control registers                                                  *)
  QSPI_R_BASE    = $A0001000;  (*!< Base address of : QSPI Control  registers                                                *)
  FLASH_END      = $080FFFFF;  (*!< FLASH end address  *)
  (* Legacy define  *)

  FLASH_BASE = FLASHAXI_BASE;
  (*!< Peripheral memory map  *)

  APB1PERIPH_BASE = PERIPH_BASE;
  APB2PERIPH_BASE = (PERIPH_BASE + $00010000);
  AHB1PERIPH_BASE = (PERIPH_BASE + $00020000);
  AHB2PERIPH_BASE = (PERIPH_BASE + $10000000);
  (*!< APB1 peripherals  *)

  TIM2_BASE    = (APB1PERIPH_BASE + $0000);
  TIM3_BASE    = (APB1PERIPH_BASE + $0400);
  TIM4_BASE    = (APB1PERIPH_BASE + $0800);
  TIM5_BASE    = (APB1PERIPH_BASE + $0C00);
  TIM6_BASE    = (APB1PERIPH_BASE + $1000);
  TIM7_BASE    = (APB1PERIPH_BASE + $1400);
  TIM12_BASE   = (APB1PERIPH_BASE + $1800);
  TIM13_BASE   = (APB1PERIPH_BASE + $1C00);
  TIM14_BASE   = (APB1PERIPH_BASE + $2000);
  LPTIM1_BASE  = (APB1PERIPH_BASE + $2400);
  RTC_BASE     = (APB1PERIPH_BASE + $2800);
  WWDG_BASE    = (APB1PERIPH_BASE + $2C00);
  IWDG_BASE    = (APB1PERIPH_BASE + $3000);
  SPI2_BASE    = (APB1PERIPH_BASE + $3800);
  SPI3_BASE    = (APB1PERIPH_BASE + $3C00);
  SPDIFRX_BASE = (APB1PERIPH_BASE + $4000);
  USART2_BASE  = (APB1PERIPH_BASE + $4400);
  USART3_BASE  = (APB1PERIPH_BASE + $4800);
  UART4_BASE   = (APB1PERIPH_BASE + $4C00);
  UART5_BASE   = (APB1PERIPH_BASE + $5000);
  I2C1_BASE    = (APB1PERIPH_BASE + $5400);
  I2C2_BASE    = (APB1PERIPH_BASE + $5800);
  I2C3_BASE    = (APB1PERIPH_BASE + $5C00);
  I2C4_BASE    = (APB1PERIPH_BASE + $6000);
  CAN1_BASE    = (APB1PERIPH_BASE + $6400);
  CAN2_BASE    = (APB1PERIPH_BASE + $6800);
  CEC_BASE     = (APB1PERIPH_BASE + $6C00);
  PWR_BASE     = (APB1PERIPH_BASE + $7000);
  DAC_BASE     = (APB1PERIPH_BASE + $7400);
  UART7_BASE   = (APB1PERIPH_BASE + $7800);
  UART8_BASE   = (APB1PERIPH_BASE + $7C00);
  (*!< APB2 peripherals  *)

  TIM1_BASE   = (APB2PERIPH_BASE + $0000);
  TIM8_BASE   = (APB2PERIPH_BASE + $0400);
  USART1_BASE = (APB2PERIPH_BASE + $1000);
  USART6_BASE = (APB2PERIPH_BASE + $1400);
  ADC1_BASE   = (APB2PERIPH_BASE + $2000);
  ADC2_BASE   = (APB2PERIPH_BASE + $2100);
  ADC3_BASE   = (APB2PERIPH_BASE + $2200);
  ADC_BASE    = (APB2PERIPH_BASE + $2300);
  SDMMC1_BASE = (APB2PERIPH_BASE + $2C00);
  SPI1_BASE   = (APB2PERIPH_BASE + $3000);
  SPI4_BASE   = (APB2PERIPH_BASE + $3400);
  SYSCFG_BASE = (APB2PERIPH_BASE + $3800);
  EXTI_BASE   = (APB2PERIPH_BASE + $3C00);
  TIM9_BASE   = (APB2PERIPH_BASE + $4000);
  TIM10_BASE  = (APB2PERIPH_BASE + $4400);
  TIM11_BASE  = (APB2PERIPH_BASE + $4800);
  SPI5_BASE   = (APB2PERIPH_BASE + $5000);
  SPI6_BASE   = (APB2PERIPH_BASE + $5400);
  SAI1_BASE   = (APB2PERIPH_BASE + $5800);
  SAI2_BASE   = (APB2PERIPH_BASE + $5C00);
  SAI1_Block_A_BASE = (SAI1_BASE + $004);
  SAI1_Block_B_BASE = (SAI1_BASE + $024);
  SAI2_Block_A_BASE = (SAI2_BASE + $004);
  SAI2_Block_B_BASE = (SAI2_BASE + $024);
  LTDC_BASE   = (APB2PERIPH_BASE + $6800);
  LTDC_Layer1_BASE = (LTDC_BASE + $84);
  LTDC_Layer2_BASE = (LTDC_BASE + $104);
  (*!< AHB1 peripherals  *)

  GPIOA_BASE   = (AHB1PERIPH_BASE + $0000);
  GPIOB_BASE   = (AHB1PERIPH_BASE + $0400);
  GPIOC_BASE   = (AHB1PERIPH_BASE + $0800);
  GPIOD_BASE   = (AHB1PERIPH_BASE + $0C00);
  GPIOE_BASE   = (AHB1PERIPH_BASE + $1000);
  GPIOF_BASE   = (AHB1PERIPH_BASE + $1400);
  GPIOG_BASE   = (AHB1PERIPH_BASE + $1800);
  GPIOH_BASE   = (AHB1PERIPH_BASE + $1C00);
  GPIOI_BASE   = (AHB1PERIPH_BASE + $2000);
  GPIOJ_BASE   = (AHB1PERIPH_BASE + $2400);
  GPIOK_BASE   = (AHB1PERIPH_BASE + $2800);
  CRC_BASE     = (AHB1PERIPH_BASE + $3000);
  RCC_BASE     = (AHB1PERIPH_BASE + $3800);
  FLASH_R_BASE = (AHB1PERIPH_BASE + $3C00);
  DMA1_BASE    = (AHB1PERIPH_BASE + $6000);
  DMA1_Stream0_BASE = (DMA1_BASE + $010);
  DMA1_Stream1_BASE = (DMA1_BASE + $028);
  DMA1_Stream2_BASE = (DMA1_BASE + $040);
  DMA1_Stream3_BASE = (DMA1_BASE + $058);
  DMA1_Stream4_BASE = (DMA1_BASE + $070);
  DMA1_Stream5_BASE = (DMA1_BASE + $088);
  DMA1_Stream6_BASE = (DMA1_BASE + $0A0);
  DMA1_Stream7_BASE = (DMA1_BASE + $0B8);
  DMA2_BASE    = (AHB1PERIPH_BASE + $6400);
  DMA2_Stream0_BASE = (DMA2_BASE + $010);
  DMA2_Stream1_BASE = (DMA2_BASE + $028);
  DMA2_Stream2_BASE = (DMA2_BASE + $040);
  DMA2_Stream3_BASE = (DMA2_BASE + $058);
  DMA2_Stream4_BASE = (DMA2_BASE + $070);
  DMA2_Stream5_BASE = (DMA2_BASE + $088);
  DMA2_Stream6_BASE = (DMA2_BASE + $0A0);
  DMA2_Stream7_BASE = (DMA2_BASE + $0B8);
  ETH_BASE     = (AHB1PERIPH_BASE + $8000);
  ETH_MAC_BASE = (ETH_BASE);
  ETH_MMC_BASE = (ETH_BASE + $0100);
  ETH_PTP_BASE = (ETH_BASE + $0700);
  ETH_DMA_BASE = (ETH_BASE + $1000);
  DMA2D_BASE   = (AHB1PERIPH_BASE + $B000);
  (*!< AHB2 peripherals  *)

  DCMI_BASE = (AHB2PERIPH_BASE + $50000);
  RNG_BASE  = (AHB2PERIPH_BASE + $60800);
  (*!< FMC Bankx registers base address  *)

  FMC_Bank1_R_BASE   = (FMC_R_BASE + $0000);
  FMC_Bank1E_R_BASE  = (FMC_R_BASE + $0104);
  FMC_Bank3_R_BASE   = (FMC_R_BASE + $0080);
  FMC_Bank5_6_R_BASE = (FMC_R_BASE + $0140);
  (* Debug MCU registers base address  *)

  DBGMCU_BASE = $E0042000;
  (*!< USB registers base address  *)

  USB_OTG_HS_PERIPH_BASE = $40040000;
  USB_OTG_FS_PERIPH_BASE = $50000000;
  USB_OTG_GLOBAL_BASE    = $000;
  USB_OTG_DEVICE_BASE    = $800;
  USB_OTG_IN_ENDPOINT_BASE = $900;
  USB_OTG_OUT_ENDPOINT_BASE = $B00;
  USB_OTG_EP_REG_SIZE    = $20;
  USB_OTG_HOST_BASE      = $400;
  USB_OTG_HOST_PORT_BASE = $440;
  USB_OTG_HOST_CHANNEL_BASE = $500;
  USB_OTG_HOST_CHANNEL_SIZE = $20;
  USB_OTG_PCGCCTL_BASE   = $E00;
  USB_OTG_FIFO_BASE      = $1000;
  USB_OTG_FIFO_SIZE      = $1000;

var
  TIM2:       TIM_TypeDef absolute TIM2_BASE;
  TIM3:       TIM_TypeDef absolute TIM3_BASE;
  TIM4:       TIM_TypeDef absolute TIM4_BASE;
  TIM5:       TIM_TypeDef absolute TIM5_BASE;
  TIM6:       TIM_TypeDef absolute TIM6_BASE;
  TIM7:       TIM_TypeDef absolute TIM7_BASE;
  TIM12:      TIM_TypeDef absolute TIM12_BASE;
  TIM13:      TIM_TypeDef absolute TIM13_BASE;
  TIM14:      TIM_TypeDef absolute TIM14_BASE;
  LPTIM1:     LPTIM_TypeDef absolute LPTIM1_BASE;
  RTC:        RTC_TypeDef absolute RTC_BASE;
  WWDG:       WWDG_TypeDef absolute WWDG_BASE;
  IWDG:       IWDG_TypeDef absolute IWDG_BASE;
  SPI2:       SPI_TypeDef absolute SPI2_BASE;
  SPI3:       SPI_TypeDef absolute SPI3_BASE;
  SPDIFRX:    SPDIFRX_TypeDef absolute SPDIFRX_BASE;
  USART2:     USART_TypeDef absolute USART2_BASE;
  USART3:     USART_TypeDef absolute USART3_BASE;
  UART4:      USART_TypeDef absolute UART4_BASE;
  UART5:      USART_TypeDef absolute UART5_BASE;
  I2C1:       I2C_TypeDef absolute I2C1_BASE;
  I2C2:       I2C_TypeDef absolute I2C2_BASE;
  I2C3:       I2C_TypeDef absolute I2C3_BASE;
  I2C4:       I2C_TypeDef absolute I2C4_BASE;
  CAN1:       CAN_TypeDef absolute CAN1_BASE;
  CAN2:       CAN_TypeDef absolute CAN2_BASE;
  CEC:        CEC_TypeDef absolute CEC_BASE;
  PWR:        PWR_TypeDef absolute PWR_BASE;
  DAC:        DAC_TypeDef absolute DAC_BASE;
  UART7:      USART_TypeDef absolute UART7_BASE;
  UART8:      USART_TypeDef absolute UART8_BASE;
  TIM1:       TIM_TypeDef absolute TIM1_BASE;
  TIM8:       TIM_TypeDef absolute TIM8_BASE;
  USART1:     USART_TypeDef absolute USART1_BASE;
  USART6:     USART_TypeDef absolute USART6_BASE;
  ADC:        ADC_Common_TypeDef absolute ADC_BASE;
  ADC1:       ADC_TypeDef absolute ADC1_BASE;
  ADC2:       ADC_TypeDef absolute ADC2_BASE;
  ADC3:       ADC_TypeDef absolute ADC3_BASE;
  SDMMC1:     SDMMC_TypeDef absolute SDMMC1_BASE;
  SPI1:       SPI_TypeDef absolute SPI1_BASE;
  SPI4:       SPI_TypeDef absolute SPI4_BASE;
  SYSCFG:     SYSCFG_TypeDef absolute SYSCFG_BASE;
  EXTI:       EXTI_TypeDef absolute EXTI_BASE;
  TIM9:       TIM_TypeDef absolute TIM9_BASE;
  TIM10:      TIM_TypeDef absolute TIM10_BASE;
  TIM11:      TIM_TypeDef absolute TIM11_BASE;
  SPI5:       SPI_TypeDef absolute SPI5_BASE;
  SPI6:       SPI_TypeDef absolute SPI6_BASE;
  SAI1:       SAI_TypeDef absolute SAI1_BASE;
  SAI2:       SAI_TypeDef absolute SAI2_BASE;
  SAI1_Block_A: SAI_Block_TypeDef absolute SAI1_Block_A_BASE;
  SAI1_Block_B: SAI_Block_TypeDef absolute SAI1_Block_B_BASE;
  SAI2_Block_A: SAI_Block_TypeDef absolute SAI2_Block_A_BASE;
  SAI2_Block_B: SAI_Block_TypeDef absolute SAI2_Block_B_BASE;
  LTDC:       LTDC_TypeDef absolute LTDC_BASE;
  LTDC_Layer1: LTDC_Layer_TypeDef absolute LTDC_Layer1_BASE;
  LTDC_Layer2: LTDC_Layer_TypeDef absolute LTDC_Layer2_BASE;
  GPIOA:      GPIO_TypeDef absolute GPIOA_BASE;
  GPIOB:      GPIO_TypeDef absolute GPIOB_BASE;
  GPIOC:      GPIO_TypeDef absolute GPIOC_BASE;
  GPIOD:      GPIO_TypeDef absolute GPIOD_BASE;
  GPIOE:      GPIO_TypeDef absolute GPIOE_BASE;
  GPIOF:      GPIO_TypeDef absolute GPIOF_BASE;
  GPIOG:      GPIO_TypeDef absolute GPIOG_BASE;
  GPIOH:      GPIO_TypeDef absolute GPIOH_BASE;
  GPIOI:      GPIO_TypeDef absolute GPIOI_BASE;
  GPIOJ:      GPIO_TypeDef absolute GPIOJ_BASE;
  GPIOK:      GPIO_TypeDef absolute GPIOK_BASE;
  CRC:        CRC_TypeDef absolute CRC_BASE;
  RCC:        RCC_TypeDef absolute RCC_BASE;
  FLASH:      FLASH_TypeDef absolute FLASH_R_BASE;
  DMA1:       DMA_TypeDef absolute DMA1_BASE;
  DMA1_Stream0: DMA_Stream_TypeDef absolute DMA1_Stream0_BASE;
  DMA1_Stream1: DMA_Stream_TypeDef absolute DMA1_Stream1_BASE;
  DMA1_Stream2: DMA_Stream_TypeDef absolute DMA1_Stream2_BASE;
  DMA1_Stream3: DMA_Stream_TypeDef absolute DMA1_Stream3_BASE;
  DMA1_Stream4: DMA_Stream_TypeDef absolute DMA1_Stream4_BASE;
  DMA1_Stream5: DMA_Stream_TypeDef absolute DMA1_Stream5_BASE;
  DMA1_Stream6: DMA_Stream_TypeDef absolute DMA1_Stream6_BASE;
  DMA1_Stream7: DMA_Stream_TypeDef absolute DMA1_Stream7_BASE;
  DMA2:       DMA_TypeDef absolute DMA2_BASE;
  DMA2_Stream0: DMA_Stream_TypeDef absolute DMA2_Stream0_BASE;
  DMA2_Stream1: DMA_Stream_TypeDef absolute DMA2_Stream1_BASE;
  DMA2_Stream2: DMA_Stream_TypeDef absolute DMA2_Stream2_BASE;
  DMA2_Stream3: DMA_Stream_TypeDef absolute DMA2_Stream3_BASE;
  DMA2_Stream4: DMA_Stream_TypeDef absolute DMA2_Stream4_BASE;
  DMA2_Stream5: DMA_Stream_TypeDef absolute DMA2_Stream5_BASE;
  DMA2_Stream6: DMA_Stream_TypeDef absolute DMA2_Stream6_BASE;
  DMA2_Stream7: DMA_Stream_TypeDef absolute DMA2_Stream7_BASE;
  ETH:        ETH_TypeDef absolute ETH_BASE;
  DMA2D:      DMA2D_TypeDef absolute DMA2D_BASE;
  DCMI:       DCMI_TypeDef absolute DCMI_BASE;
  RNG:        RNG_TypeDef absolute RNG_BASE;
  FMC_Bank1:  FMC_Bank1_TypeDef absolute FMC_Bank1_R_BASE;
  FMC_Bank1E: FMC_Bank1E_TypeDef absolute FMC_Bank1E_R_BASE;
  FMC_Bank3:  FMC_Bank3_TypeDef absolute FMC_Bank3_R_BASE;
  FMC_Bank5_6: FMC_Bank5_6_TypeDef absolute FMC_Bank5_6_R_BASE;
  QUADSPI:    QUADSPI_TypeDef absolute QSPI_R_BASE;
  DBGMCU:     DBGMCU_TypeDef absolute DBGMCU_BASE;
  USB_OTG_FS: USB_OTG_GlobalTypeDef absolute USB_OTG_FS_PERIPH_BASE;
  USB_OTG_HS: USB_OTG_GlobalTypeDef absolute USB_OTG_HS_PERIPH_BASE;

implementation

procedure NMI_Interrupt; external name 'NMI_Interrupt';
procedure HardFault_Interrupt; external name 'HardFault_Interrupt';
procedure MemManage_Interrupt; external name 'MemManage_Interrupt';
procedure BusFault_Interrupt; external name 'BusFault_Interrupt';
procedure UsageFault_Interrupt; external name 'UsageFault_Interrupt';
procedure SVC_Interrupt; external name 'SVC_Interrupt';
procedure DebugMon_Interrupt; external name 'DebugMon_Interrupt';
procedure PendSV_Interrupt; external name 'PendSV_Interrupt';
procedure SysTick_Interrupt; external name 'SysTick_Interrupt';
procedure WWDG_Interrupt; external name 'WWDG_Interrupt';
procedure PVD_Interrupt; external name 'PVD_Interrupt';
procedure TAMP_STAMP_Interrupt; external name 'TAMP_STAMP_Interrupt';
procedure RTC_WKUP_Interrupt; external name 'RTC_WKUP_Interrupt';
procedure FLASH_Interrupt; external name 'FLASH_Interrupt';
procedure RCC_Interrupt; external name 'RCC_Interrupt';
procedure EXTI0_Interrupt; external name 'EXTI0_Interrupt';
procedure EXTI1_Interrupt; external name 'EXTI1_Interrupt';
procedure EXTI2_Interrupt; external name 'EXTI2_Interrupt';
procedure EXTI3_Interrupt; external name 'EXTI3_Interrupt';
procedure EXTI4_Interrupt; external name 'EXTI4_Interrupt';
procedure DMA1_Stream0_Interrupt; external name 'DMA1_Stream0_Interrupt';
procedure DMA1_Stream1_Interrupt; external name 'DMA1_Stream1_Interrupt';
procedure DMA1_Stream2_Interrupt; external name 'DMA1_Stream2_Interrupt';
procedure DMA1_Stream3_Interrupt; external name 'DMA1_Stream3_Interrupt';
procedure DMA1_Stream4_Interrupt; external name 'DMA1_Stream4_Interrupt';
procedure DMA1_Stream5_Interrupt; external name 'DMA1_Stream5_Interrupt';
procedure DMA1_Stream6_Interrupt; external name 'DMA1_Stream6_Interrupt';
procedure ADC_Interrupt; external name 'ADC_Interrupt';
procedure CAN1_TX_Interrupt; external name 'CAN1_TX_Interrupt';
procedure CAN1_RX0_Interrupt; external name 'CAN1_RX0_Interrupt';
procedure CAN1_RX1_Interrupt; external name 'CAN1_RX1_Interrupt';
procedure CAN1_SCE_Interrupt; external name 'CAN1_SCE_Interrupt';
procedure EXTI9_5_Interrupt; external name 'EXTI9_5_Interrupt';
procedure TIM1_BRK_TIM9_Interrupt; external name 'TIM1_BRK_TIM9_Interrupt';
procedure TIM1_UP_TIM10_Interrupt; external name 'TIM1_UP_TIM10_Interrupt';
procedure TIM1_TRG_COM_TIM11_Interrupt; external name 'TIM1_TRG_COM_TIM11_Interrupt';
procedure TIM1_CC_Interrupt; external name 'TIM1_CC_Interrupt';
procedure TIM2_Interrupt; external name 'TIM2_Interrupt';
procedure TIM3_Interrupt; external name 'TIM3_Interrupt';
procedure TIM4_Interrupt; external name 'TIM4_Interrupt';
procedure I2C1_EV_Interrupt; external name 'I2C1_EV_Interrupt';
procedure I2C1_ER_Interrupt; external name 'I2C1_ER_Interrupt';
procedure I2C2_EV_Interrupt; external name 'I2C2_EV_Interrupt';
procedure I2C2_ER_Interrupt; external name 'I2C2_ER_Interrupt';
procedure SPI1_Interrupt; external name 'SPI1_Interrupt';
procedure SPI2_Interrupt; external name 'SPI2_Interrupt';
procedure USART1_Interrupt; external name 'USART1_Interrupt';
procedure USART2_Interrupt; external name 'USART2_Interrupt';
procedure USART3_Interrupt; external name 'USART3_Interrupt';
procedure EXTI15_10_Interrupt; external name 'EXTI15_10_Interrupt';
procedure RTC_Alarm_Interrupt; external name 'RTC_Alarm_Interrupt';
procedure OTG_FS_WKUP_Interrupt; external name 'OTG_FS_WKUP_Interrupt';
procedure TIM8_BRK_TIM12_Interrupt; external name 'TIM8_BRK_TIM12_Interrupt';
procedure TIM8_UP_TIM13_Interrupt; external name 'TIM8_UP_TIM13_Interrupt';
procedure TIM8_TRG_COM_TIM14_Interrupt; external name 'TIM8_TRG_COM_TIM14_Interrupt';
procedure TIM8_CC_Interrupt; external name 'TIM8_CC_Interrupt';
procedure DMA1_Stream7_Interrupt; external name 'DMA1_Stream7_Interrupt';
procedure FMC_Interrupt; external name 'FMC_Interrupt';
procedure SDMMC1_Interrupt; external name 'SDMMC1_Interrupt';
procedure TIM5_Interrupt; external name 'TIM5_Interrupt';
procedure SPI3_Interrupt; external name 'SPI3_Interrupt';
procedure UART4_Interrupt; external name 'UART4_Interrupt';
procedure UART5_Interrupt; external name 'UART5_Interrupt';
procedure TIM6_DAC_Interrupt; external name 'TIM6_DAC_Interrupt';
procedure TIM7_Interrupt; external name 'TIM7_Interrupt';
procedure DMA2_Stream0_Interrupt; external name 'DMA2_Stream0_Interrupt';
procedure DMA2_Stream1_Interrupt; external name 'DMA2_Stream1_Interrupt';
procedure DMA2_Stream2_Interrupt; external name 'DMA2_Stream2_Interrupt';
procedure DMA2_Stream3_Interrupt; external name 'DMA2_Stream3_Interrupt';
procedure DMA2_Stream4_Interrupt; external name 'DMA2_Stream4_Interrupt';
procedure ETH_Interrupt; external name 'ETH_Interrupt';
procedure ETH_WKUP_Interrupt; external name 'ETH_WKUP_Interrupt';
procedure CAN2_TX_Interrupt; external name 'CAN2_TX_Interrupt';
procedure CAN2_RX0_Interrupt; external name 'CAN2_RX0_Interrupt';
procedure CAN2_RX1_Interrupt; external name 'CAN2_RX1_Interrupt';
procedure CAN2_SCE_Interrupt; external name 'CAN2_SCE_Interrupt';
procedure OTG_FS_Interrupt; external name 'OTG_FS_Interrupt';
procedure DMA2_Stream5_Interrupt; external name 'DMA2_Stream5_Interrupt';
procedure DMA2_Stream6_Interrupt; external name 'DMA2_Stream6_Interrupt';
procedure DMA2_Stream7_Interrupt; external name 'DMA2_Stream7_Interrupt';
procedure USART6_Interrupt; external name 'USART6_Interrupt';
procedure I2C3_EV_Interrupt; external name 'I2C3_EV_Interrupt';
procedure I2C3_ER_Interrupt; external name 'I2C3_ER_Interrupt';
procedure OTG_HS_EP1_OUT_Interrupt; external name 'OTG_HS_EP1_OUT_Interrupt';
procedure OTG_HS_EP1_IN_Interrupt; external name 'OTG_HS_EP1_IN_Interrupt';
procedure OTG_HS_WKUP_Interrupt; external name 'OTG_HS_WKUP_Interrupt';
procedure OTG_HS_Interrupt; external name 'OTG_HS_Interrupt';
procedure DCMI_Interrupt; external name 'DCMI_Interrupt';
procedure RNG_Interrupt; external name 'RNG_Interrupt';
procedure FPU_Interrupt; external name 'FPU_Interrupt';
procedure UART7_Interrupt; external name 'UART7_Interrupt';
procedure UART8_Interrupt; external name 'UART8_Interrupt';
procedure SPI4_Interrupt; external name 'SPI4_Interrupt';
procedure SPI5_Interrupt; external name 'SPI5_Interrupt';
procedure SPI6_Interrupt; external name 'SPI6_Interrupt';
procedure SAI1_Interrupt; external name 'SAI1_Interrupt';
procedure LTDC_Interrupt; external name 'LTDC_Interrupt';
procedure LTDC_ER_Interrupt; external name 'LTDC_ER_Interrupt';
procedure DMA2D_Interrupt; external name 'DMA2D_Interrupt';
procedure SAI2_Interrupt; external name 'SAI2_Interrupt';
procedure QUADSPI_Interrupt; external name 'QUADSPI_Interrupt';
procedure LPTIM1_Interrupt; external name 'LPTIM1_Interrupt';
procedure CEC_Interrupt; external name 'CEC_Interrupt';
procedure I2C4_EV_Interrupt; external name 'I2C4_EV_Interrupt';
procedure I2C4_ER_Interrupt; external name 'I2C4_ER_Interrupt';
procedure SPDIF_RX_Interrupt; external name 'SPDIF_RX_Interrupt';

{$i cortexm4f_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
interrupt_vectors:
  .long _stack_top
  .long  Startup

  .long  NMI_Interrupt
  .long  HardFault_Interrupt
  .long  MemManage_Interrupt
  .long  BusFault_Interrupt
  .long  UsageFault_Interrupt
  .long  0
  .long  0
  .long  0
  .long  0
  .long  SVC_Interrupt
  .long  DebugMon_Interrupt
  .long  0
  .long  PendSV_Interrupt
  .long  SysTick_Interrupt

  (* External Interrupts *)
  .long     WWDG_Interrupt
  .long     PVD_Interrupt
  .long     TAMP_STAMP_Interrupt
  .long     RTC_WKUP_Interrupt
  .long     FLASH_Interrupt
  .long     RCC_Interrupt
  .long     EXTI0_Interrupt
  .long     EXTI1_Interrupt
  .long     EXTI2_Interrupt
  .long     EXTI3_Interrupt
  .long     EXTI4_Interrupt
  .long     DMA1_Stream0_Interrupt
  .long     DMA1_Stream1_Interrupt
  .long     DMA1_Stream2_Interrupt
  .long     DMA1_Stream3_Interrupt
  .long     DMA1_Stream4_Interrupt
  .long     DMA1_Stream5_Interrupt
  .long     DMA1_Stream6_Interrupt
  .long     ADC_Interrupt
  .long     CAN1_TX_Interrupt
  .long     CAN1_RX0_Interrupt
  .long     CAN1_RX1_Interrupt
  .long     CAN1_SCE_Interrupt
  .long     EXTI9_5_Interrupt
  .long     TIM1_BRK_TIM9_Interrupt
  .long     TIM1_UP_TIM10_Interrupt
  .long     TIM1_TRG_COM_TIM11_Interrupt
  .long     TIM1_CC_Interrupt
  .long     TIM2_Interrupt
  .long     TIM3_Interrupt
  .long     TIM4_Interrupt
  .long     I2C1_EV_Interrupt
  .long     I2C1_ER_Interrupt
  .long     I2C2_EV_Interrupt
  .long     I2C2_ER_Interrupt
  .long     SPI1_Interrupt
  .long     SPI2_Interrupt
  .long     USART1_Interrupt
  .long     USART2_Interrupt
  .long     USART3_Interrupt
  .long     EXTI15_10_Interrupt
  .long     RTC_Alarm_Interrupt
  .long     OTG_FS_WKUP_Interrupt
  .long     TIM8_BRK_TIM12_Interrupt
  .long     TIM8_UP_TIM13_Interrupt
  .long     TIM8_TRG_COM_TIM14_Interrupt
  .long     TIM8_CC_Interrupt
  .long     DMA1_Stream7_Interrupt
  .long     FMC_Interrupt
  .long     SDMMC1_Interrupt
  .long     TIM5_Interrupt
  .long     SPI3_Interrupt
  .long     UART4_Interrupt
  .long     UART5_Interrupt
  .long     TIM6_DAC_Interrupt
  .long     TIM7_Interrupt
  .long     DMA2_Stream0_Interrupt
  .long     DMA2_Stream1_Interrupt
  .long     DMA2_Stream2_Interrupt
  .long     DMA2_Stream3_Interrupt
  .long     DMA2_Stream4_Interrupt
  .long     ETH_Interrupt
  .long     ETH_WKUP_Interrupt
  .long     CAN2_TX_Interrupt
  .long     CAN2_RX0_Interrupt
  .long     CAN2_RX1_Interrupt
  .long     CAN2_SCE_Interrupt
  .long     OTG_FS_Interrupt
  .long     DMA2_Stream5_Interrupt
  .long     DMA2_Stream6_Interrupt
  .long     DMA2_Stream7_Interrupt
  .long     USART6_Interrupt
  .long     I2C3_EV_Interrupt
  .long     I2C3_ER_Interrupt
  .long     OTG_HS_EP1_OUT_Interrupt
  .long     OTG_HS_EP1_IN_Interrupt
  .long     OTG_HS_WKUP_Interrupt
  .long     OTG_HS_Interrupt
  .long     DCMI_Interrupt
  .long     0
  .long     RNG_Interrupt
  .long     FPU_Interrupt
  .long     UART7_Interrupt
  .long     UART8_Interrupt
  .long     SPI4_Interrupt
  .long     SPI5_Interrupt
  .long     SPI6_Interrupt
  .long     SAI1_Interrupt
  .long     LTDC_Interrupt
  .long     LTDC_ER_Interrupt
  .long     DMA2D_Interrupt
  .long     SAI2_Interrupt
  .long     QUADSPI_Interrupt
  .long     LPTIM1_Interrupt
  .long     CEC_Interrupt
  .long     I2C4_EV_Interrupt
  .long     I2C4_ER_Interrupt
  .long     SPDIF_RX_Interrupt

  .weak NMI_Interrupt
  .weak HardFault_Interrupt
  .weak MemManage_Interrupt
  .weak BusFault_Interrupt
  .weak UsageFault_Interrupt
  .weak SVC_Interrupt
  .weak DebugMon_Interrupt
  .weak PendSV_Interrupt
  .weak SysTick_Interrupt
  .weak WWDG_Interrupt
  .weak PVD_Interrupt
  .weak TAMP_STAMP_Interrupt
  .weak RTC_WKUP_Interrupt
  .weak FLASH_Interrupt
  .weak RCC_Interrupt
  .weak EXTI0_Interrupt
  .weak EXTI1_Interrupt
  .weak EXTI2_Interrupt
  .weak EXTI3_Interrupt
  .weak EXTI4_Interrupt
  .weak DMA1_Stream0_Interrupt
  .weak DMA1_Stream1_Interrupt
  .weak DMA1_Stream2_Interrupt
  .weak DMA1_Stream3_Interrupt
  .weak DMA1_Stream4_Interrupt
  .weak DMA1_Stream5_Interrupt
  .weak DMA1_Stream6_Interrupt
  .weak ADC_Interrupt
  .weak CAN1_TX_Interrupt
  .weak CAN1_RX0_Interrupt
  .weak CAN1_RX1_Interrupt
  .weak CAN1_SCE_Interrupt
  .weak EXTI9_5_Interrupt
  .weak TIM1_BRK_TIM9_Interrupt
  .weak TIM1_UP_TIM10_Interrupt
  .weak TIM1_TRG_COM_TIM11_Interrupt
  .weak TIM1_CC_Interrupt
  .weak TIM2_Interrupt
  .weak TIM3_Interrupt
  .weak TIM4_Interrupt
  .weak I2C1_EV_Interrupt
  .weak I2C1_ER_Interrupt
  .weak I2C2_EV_Interrupt
  .weak I2C2_ER_Interrupt
  .weak SPI1_Interrupt
  .weak SPI2_Interrupt
  .weak USART1_Interrupt
  .weak USART2_Interrupt
  .weak USART3_Interrupt
  .weak EXTI15_10_Interrupt
  .weak RTC_Alarm_Interrupt
  .weak OTG_FS_WKUP_Interrupt
  .weak TIM8_BRK_TIM12_Interrupt
  .weak TIM8_UP_TIM13_Interrupt
  .weak TIM8_TRG_COM_TIM14_Interrupt
  .weak TIM8_CC_Interrupt
  .weak DMA1_Stream7_Interrupt
  .weak FMC_Interrupt
  .weak SDMMC1_Interrupt
  .weak TIM5_Interrupt
  .weak SPI3_Interrupt
  .weak UART4_Interrupt
  .weak UART5_Interrupt
  .weak TIM6_DAC_Interrupt
  .weak TIM7_Interrupt
  .weak DMA2_Stream0_Interrupt
  .weak DMA2_Stream1_Interrupt
  .weak DMA2_Stream2_Interrupt
  .weak DMA2_Stream3_Interrupt
  .weak DMA2_Stream4_Interrupt
  .weak ETH_Interrupt
  .weak ETH_WKUP_Interrupt
  .weak CAN2_TX_Interrupt
  .weak CAN2_RX0_Interrupt
  .weak CAN2_RX1_Interrupt
  .weak CAN2_SCE_Interrupt
  .weak OTG_FS_Interrupt
  .weak DMA2_Stream5_Interrupt
  .weak DMA2_Stream6_Interrupt
  .weak DMA2_Stream7_Interrupt
  .weak USART6_Interrupt
  .weak I2C3_EV_Interrupt
  .weak I2C3_ER_Interrupt
  .weak OTG_HS_EP1_OUT_Interrupt
  .weak OTG_HS_EP1_IN_Interrupt
  .weak OTG_HS_WKUP_Interrupt
  .weak OTG_HS_Interrupt
  .weak DCMI_Interrupt
  .weak RNG_Interrupt
  .weak FPU_Interrupt
  .weak UART7_Interrupt
  .weak UART8_Interrupt
  .weak SPI4_Interrupt
  .weak SPI5_Interrupt
  .weak SPI6_Interrupt
  .weak SAI1_Interrupt
  .weak LTDC_Interrupt
  .weak LTDC_ER_Interrupt
  .weak DMA2D_Interrupt
  .weak SAI2_Interrupt
  .weak QUADSPI_Interrupt
  .weak LPTIM1_Interrupt
  .weak CEC_Interrupt
  .weak I2C4_EV_Interrupt
  .weak I2C4_ER_Interrupt
  .weak SPDIF_RX_Interrupt

  .set NMI_Interrupt, HaltProc
  .set HardFault_Interrupt, HaltProc
  .set MemManage_Interrupt, HaltProc
  .set BusFault_Interrupt, HaltProc
  .set UsageFault_Interrupt, HaltProc
  .set SVC_Interrupt, HaltProc
  .set DebugMon_Interrupt, HaltProc
  .set PendSV_Interrupt, HaltProc
  .set SysTick_Interrupt, HaltProc
  .set WWDG_Interrupt, HaltProc
  .set PVD_Interrupt, HaltProc
  .set TAMP_STAMP_Interrupt, HaltProc
  .set RTC_WKUP_Interrupt, HaltProc
  .set FLASH_Interrupt, HaltProc
  .set RCC_Interrupt, HaltProc
  .set EXTI0_Interrupt, HaltProc
  .set EXTI1_Interrupt, HaltProc
  .set EXTI2_Interrupt, HaltProc
  .set EXTI3_Interrupt, HaltProc
  .set EXTI4_Interrupt, HaltProc
  .set DMA1_Stream0_Interrupt, HaltProc
  .set DMA1_Stream1_Interrupt, HaltProc
  .set DMA1_Stream2_Interrupt, HaltProc
  .set DMA1_Stream3_Interrupt, HaltProc
  .set DMA1_Stream4_Interrupt, HaltProc
  .set DMA1_Stream5_Interrupt, HaltProc
  .set DMA1_Stream6_Interrupt, HaltProc
  .set ADC_Interrupt, HaltProc
  .set CAN1_TX_Interrupt, HaltProc
  .set CAN1_RX0_Interrupt, HaltProc
  .set CAN1_RX1_Interrupt, HaltProc
  .set CAN1_SCE_Interrupt, HaltProc
  .set EXTI9_5_Interrupt, HaltProc
  .set TIM1_BRK_TIM9_Interrupt, HaltProc
  .set TIM1_UP_TIM10_Interrupt, HaltProc
  .set TIM1_TRG_COM_TIM11_Interrupt, HaltProc
  .set TIM1_CC_Interrupt, HaltProc
  .set TIM2_Interrupt, HaltProc
  .set TIM3_Interrupt, HaltProc
  .set TIM4_Interrupt, HaltProc
  .set I2C1_EV_Interrupt, HaltProc
  .set I2C1_ER_Interrupt, HaltProc
  .set I2C2_EV_Interrupt, HaltProc
  .set I2C2_ER_Interrupt, HaltProc
  .set SPI1_Interrupt, HaltProc
  .set SPI2_Interrupt, HaltProc
  .set USART1_Interrupt, HaltProc
  .set USART2_Interrupt, HaltProc
  .set USART3_Interrupt, HaltProc
  .set EXTI15_10_Interrupt, HaltProc
  .set RTC_Alarm_Interrupt, HaltProc
  .set OTG_FS_WKUP_Interrupt, HaltProc
  .set TIM8_BRK_TIM12_Interrupt, HaltProc
  .set TIM8_UP_TIM13_Interrupt, HaltProc
  .set TIM8_TRG_COM_TIM14_Interrupt, HaltProc
  .set TIM8_CC_Interrupt, HaltProc
  .set DMA1_Stream7_Interrupt, HaltProc
  .set FMC_Interrupt, HaltProc
  .set SDMMC1_Interrupt, HaltProc
  .set TIM5_Interrupt, HaltProc
  .set SPI3_Interrupt, HaltProc
  .set UART4_Interrupt, HaltProc
  .set UART5_Interrupt, HaltProc
  .set TIM6_DAC_Interrupt, HaltProc
  .set TIM7_Interrupt, HaltProc
  .set DMA2_Stream0_Interrupt, HaltProc
  .set DMA2_Stream1_Interrupt, HaltProc
  .set DMA2_Stream2_Interrupt, HaltProc
  .set DMA2_Stream3_Interrupt, HaltProc
  .set DMA2_Stream4_Interrupt, HaltProc
  .set ETH_Interrupt, HaltProc
  .set ETH_WKUP_Interrupt, HaltProc
  .set CAN2_TX_Interrupt, HaltProc
  .set CAN2_RX0_Interrupt, HaltProc
  .set CAN2_RX1_Interrupt, HaltProc
  .set CAN2_SCE_Interrupt, HaltProc
  .set OTG_FS_Interrupt, HaltProc
  .set DMA2_Stream5_Interrupt, HaltProc
  .set DMA2_Stream6_Interrupt, HaltProc
  .set DMA2_Stream7_Interrupt, HaltProc
  .set USART6_Interrupt, HaltProc
  .set I2C3_EV_Interrupt, HaltProc
  .set I2C3_ER_Interrupt, HaltProc
  .set OTG_HS_EP1_OUT_Interrupt, HaltProc
  .set OTG_HS_EP1_IN_Interrupt, HaltProc
  .set OTG_HS_WKUP_Interrupt, HaltProc
  .set OTG_HS_Interrupt, HaltProc
  .set DCMI_Interrupt, HaltProc
  .set RNG_Interrupt, HaltProc
  .set FPU_Interrupt, HaltProc
  .set UART7_Interrupt, HaltProc
  .set UART8_Interrupt, HaltProc
  .set SPI4_Interrupt, HaltProc
  .set SPI5_Interrupt, HaltProc
  .set SPI6_Interrupt, HaltProc
  .set SAI1_Interrupt, HaltProc
  .set LTDC_Interrupt, HaltProc
  .set LTDC_ER_Interrupt, HaltProc
  .set DMA2D_Interrupt, HaltProc
  .set SAI2_Interrupt, HaltProc
  .set QUADSPI_Interrupt, HaltProc
  .set LPTIM1_Interrupt, HaltProc
  .set CEC_Interrupt, HaltProc
  .set I2C4_EV_Interrupt, HaltProc
  .set I2C4_ER_Interrupt, HaltProc
  .set SPDIF_RX_Interrupt, HaltProc

  .text
end;

end.

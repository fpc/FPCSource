unit nrf52832;

{$goto on}

interface

{************************************************************************************************** }  {*
 * @file     nrf52.h
 *
 * @brief    CMSIS Cortex-M4 Peripheral Access Layer Header File for
 *           nrf52 from Nordic Semiconductor.
 *
 * @version  V1
 * @date     23. February 2016
 *
 * @note     Generated with SVDConv V2.81d
 *           from CMSIS SVD File 'nrf52.xml' Version 1,
 *
 *           The Contents of this file are made available subject to the terms of
 *           the BSD license.
 *
 * @par      Copyright (c) 2015, Nordic Semiconductor ASA
 *           All rights reserved.
 *
 *           Redistribution and use in source and binary forms, with or without
 *           modification, are permitted provided that the following conditions are met:
 *
 *           * Redistributions of source code must retain the above copyright notice, this
 *           list of conditions and the following disclaimer.
 *
 *           * Redistributions in binary form must reproduce the above copyright notice,
 *           this list of conditions and the following disclaimer in the documentation
 *           and/or other materials provided with the distribution.
 *
 *           * Neither the name of Nordic Semiconductor ASA nor the names of its
 *           contributors may be used to endorse or promote products derived from
 *           this software without specific prior written permission.
 *
 *           THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *           AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *           IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *           DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *           FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *           DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *           SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *           CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *           OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *           OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *
 ****************************************************************************************************** }


{ -------------------------  Interrupt Number Definition  ------------------------ }

type
TIRQn_Enum = (
{ -------------------  Cortex-M4 Processor Exceptions Numbers  ------------------- }
  Reset_IRQn                    = -15,              {   1  Reset Vector, invoked on Power up and warm reset                 }
  NonMaskableInt_IRQn           = -14,              {   2  Non maskable Interrupt, cannot be stopped or preempted           }
  HardFault_IRQn                = -13,              {   3  Hard Fault, all classes of Fault                                 }
  MemoryManagement_IRQn         = -12,              {   4  Memory Management, MPU mismatch, including Access Violation
                                                       and No Match                                                          }
  BusFault_IRQn                 = -11,              {   5  Bus Fault, Pre-Fetch-, Memory Access Fault, other address/memory
                                                       related Fault                                                         }
  UsageFault_IRQn               = -10,              {   6  Usage Fault, i.e. Undef Instruction, Illegal State Transition    }
  SVCall_IRQn                   =  -5,              {  11  System Service Call via SVC instruction                          }
  DebugMonitor_IRQn             =  -4,              {  12  Debug Monitor                                                    }
  PendSV_IRQn                   =  -2,              {  14  Pendable request for system service                              }
  SysTick_IRQn                  =  -1,              {  15  System Tick Timer                                                }
{ ----------------------  nrf52 Specific Interrupt Numbers  ---------------------- }
  POWER_CLOCK_IRQn              =   0,              {   0  POWER_CLOCK                                                      }
  RADIO_IRQn                    =   1,              {   1  RADIO                                                            }
  UARTE0_UART0_IRQn             =   2,              {   2  UARTE0_UART0                                                     }
  SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_IRQn=   3,      {   3  SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0                                }
  SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_IRQn=   4,      {   4  SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1                                }
  NFCT_IRQn                     =   5,              {   5  NFCT                                                             }
  GPIOTE_IRQn                   =   6,              {   6  GPIOTE                                                           }
  SAADC_IRQn                    =   7,              {   7  SAADC                                                            }
  TIMER0_IRQn                   =   8,              {   8  TIMER0                                                           }
  TIMER1_IRQn                   =   9,              {   9  TIMER1                                                           }
  TIMER2_IRQn                   =  10,              {  10  TIMER2                                                           }
  RTC0_IRQn                     =  11,              {  11  RTC0                                                             }
  TEMP_IRQn                     =  12,              {  12  TEMP                                                             }
  RNG_IRQn                      =  13,              {  13  RNG                                                              }
  ECB_IRQn                      =  14,              {  14  ECB                                                              }
  CCM_AAR_IRQn                  =  15,              {  15  CCM_AAR                                                          }
  WDT_IRQn                      =  16,              {  16  WDT                                                              }
  RTC1_IRQn                     =  17,              {  17  RTC1                                                             }
  QDEC_IRQn                     =  18,              {  18  QDEC                                                             }
  COMP_LPCOMP_IRQn              =  19,              {  19  COMP_LPCOMP                                                      }
  SWI0_EGU0_IRQn                =  20,              {  20  SWI0_EGU0                                                        }
  SWI1_EGU1_IRQn                =  21,              {  21  SWI1_EGU1                                                        }
  SWI2_EGU2_IRQn                =  22,              {  22  SWI2_EGU2                                                        }
  SWI3_EGU3_IRQn                =  23,              {  23  SWI3_EGU3                                                        }
  SWI4_EGU4_IRQn                =  24,              {  24  SWI4_EGU4                                                        }
  SWI5_EGU5_IRQn                =  25,              {  25  SWI5_EGU5                                                        }
  TIMER3_IRQn                   =  26,              {  26  TIMER3                                                           }
  TIMER4_IRQn                   =  27,              {  27  TIMER4                                                           }
  PWM0_IRQn                     =  28,              {  28  PWM0                                                             }
  PDM_IRQn                      =  29,              {  29  PDM                                                              }
  MWU_IRQn                      =  32,              {  32  MWU                                                              }
  PWM1_IRQn                     =  33,              {  33  PWM1                                                             }
  PWM2_IRQn                     =  34,              {  34  PWM2                                                             }
  SPIM2_SPIS2_SPI2_IRQn         =  35,              {  35  SPIM2_SPIS2_SPI2                                                 }
  RTC2_IRQn                     =  36,              {  36  RTC2                                                             }
  I2S_IRQn                      =  37,              {  37  I2S                                                              }
  FPU_IRQn                      =  38               {  38  FPU                                                              }
);


{ ================================================================================  }
{ ================      Processor and Core Peripheral Section     ================  }
{ ================================================================================  }

{ ----------------Configuration of the Cortex-M4 Processor and Core Peripherals----------------  }

const
  __CM4_REV              = $0001;                    { Cortex-M4 Core Revision                                               }
  __MPU_PRESENT          = 1;                        { MPU present or not                                                    }
  __NVIC_PRIO_BITS       = 3;                        { Number of Bits used for Priority Levels                               }
  __Vendor_SysTickConfig = 0;                        { Set to 1 if different SysTick Config is used                          }
  __FPU_PRESENT          = 1;                        { FPU present or not                                                    }


{ ================================================================================  }
{ ================       Device Specific Peripheral Section       ================  }
{ ================================================================================  }

type
FICR_INFO_Type = record
  PART,                                              { Part code                                                             }
  VARIANT_,                                          { Part Variant, Hardware version and Production configuration           }
  PACKAGE,                                           { Package option                                                        }
  RAM,                                               { RAM variant                                                           }
  FLASH: longword;                                   { Flash variant                                                         }
  UNUSED0: array [0..2] of longword;                 { Description collection[0]: Unspecified                                }
end;

FICR_TEMP_Type = record
  A0,                                                { Slope definition A0.                                                  }
  A1,                                                { Slope definition A1.                                                  }
  A2,                                                { Slope definition A2.                                                  }
  A3,                                                { Slope definition A3.                                                  }
  A4,                                                { Slope definition A4.                                                  }
  A5,                                                { Slope definition A5.                                                  }
  B0,                                                { y-intercept B0.                                                       }
  B1,                                                { y-intercept B1.                                                       }
  B2,                                                { y-intercept B2.                                                       }
  B3,                                                { y-intercept B3.                                                       }
  B4,                                                { y-intercept B4.                                                       }
  B5,                                                { y-intercept B5.                                                       }
  T0,                                                { Segment end T0.                                                       }
  T1,                                                { Segment end T1.                                                       }
  T2,                                                { Segment end T2.                                                       }
  T3,                                                { Segment end T3.                                                       }
  T4: longword;                                      { Segment end T4.                                                       }
end;

FICR_NFC_Type = record
  TAGHEADER0,                                        { Default header for NFC Tag. Software can read these values to
                                                       populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.            }
  TAGHEADER1,                                        { Default header for NFC Tag. Software can read these values to
                                                       populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.            }
  TAGHEADER2,                                        { Default header for NFC Tag. Software can read these values to
                                                       populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.            }
  TAGHEADER3: longword;                              { Default header for NFC Tag. Software can read these values to
                                                       populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.            }
end;

POWER_RAM_Type = record
  POWER,                                             { Description cluster[0]: RAM0 power control register                   }
  POWERSET,                                          { Description cluster[0]: RAM0 power control set register               }
  POWERCLR,                                          { Description cluster[0]: RAM0 power control clear register             }
  RESERVED0: longword;
end;

AMLI_RAMPRI_Type = record
  CPU0,                                              { AHB bus master priority register for CPU0                             }
  SPIS1,                                             { AHB bus master priority register for SPIM1, SPIS1, TWIM1 and
                                                       TWIS1                                                                 }
  RADIO,                                             { AHB bus master priority register for RADIO                            }
  ECB,                                               { AHB bus master priority register for ECB                              }
  CCM,                                               { AHB bus master priority register for CCM                              }
  AAR,                                               { AHB bus master priority register for AAR                              }
  SAADC,                                             { AHB bus master priority register for SAADC                            }
  UARTE,                                             { AHB bus master priority register for UARTE                            }
  SERIAL0,                                           { AHB bus master priority register for SPIM0, SPIS0, TWIM0 and
                                                       TWIS0                                                                 }
  SERIAL2,                                           { AHB bus master priority register for SPIM2 and SPIS2                  }
  NFCT,                                              { AHB bus master priority register for NFCT                             }
  I2S,                                               { AHB bus master priority register for I2S                              }
  PDM,                                               { AHB bus master priority register for PDM                              }
  PWM: longword;                                     { AHB bus master priority register for PWM0, PWM1 and PWM2              }
end;

UARTE_PSEL_Type = record
  RTS,                                               { Pin select for RTS signal                                             }
  TXD,                                               { Pin select for TXD signal                                             }
  CTS,                                               { Pin select for CTS signal                                             }
  RXD: longword;                                     { Pin select for RXD signal                                             }
end;

UARTE_RXD_Type = record
  PTR,                                               { Data pointer                                                          }
  MAXCNT,                                            { Maximum number of bytes in receive buffer                             }
  AMOUNT: longword;                                  { Number of bytes transferred in the last transaction                   }
end;

UARTE_TXD_Type = record
  PTR,                                               { Data pointer                                                          }
  MAXCNT,                                            { Maximum number of bytes in transmit buffer                            }
  AMOUNT: longword;                                  { Number of bytes transferred in the last transaction                   }
end;

SPIM_PSEL_Type = record
  SCK,                                               { Pin select for SCK                                                    }
  MOSI,                                              { Pin select for MOSI signal                                            }
  MISO: longword;                                    { Pin select for MISO signal                                            }
end;

SPIM_RXD_Type = record
  PTR,                                               { Data pointer                                                          }
  MAXCNT,                                            { Maximum number of bytes in receive buffer                             }
  AMOUNT,                                            { Number of bytes transferred in the last transaction                   }
  LIST: longword;                                    { EasyDMA list type                                                     }
end;

SPIM_TXD_Type = record
  PTR,                                               { Data pointer                                                          }
  MAXCNT,                                            { Maximum number of bytes in transmit buffer                            }
  AMOUNT,                                            { Number of bytes transferred in the last transaction                   }
  LIST: longword;                                    { EasyDMA list type                                                     }
end;

SPIS_PSEL_Type = record
  SCK,                                               { Pin select for SCK                                                    }
  MISO,                                              { Pin select for MISO signal                                            }
  MOSI,                                              { Pin select for MOSI signal                                            }
  CSN: longword;                                     { Pin select for CSN signal                                             }
end;

SPIS_RXD_Type = record
  PTR,                                               { RXD data pointer                                                      }
  MAXCNT,                                            { Maximum number of bytes in receive buffer                             }
  AMOUNT: longword;                                  { Number of bytes received in last granted transaction                  }
end;

SPIS_TXD_Type = record
  PTR,                                               { TXD data pointer                                                      }
  MAXCNT,                                            { Maximum number of bytes in transmit buffer                            }
  AMOUNT: longword;                                  { Number of bytes transmitted in last granted transaction               }
end;

TWIM_PSEL_Type = record
  SCL,                                               { Pin select for SCL signal                                             }
  SDA: longword;                                     { Pin select for SDA signal                                             }
end;

TWIM_RXD_Type = record
  PTR,                                               { Data pointer                                                          }
  MAXCNT,                                            { Maximum number of bytes in receive buffer                             }
  AMOUNT,                                            { Number of bytes transferred in the last transaction                   }
  LIST: longword;                                    { EasyDMA list type                                                     }
end;

TWIM_TXD_Type = record
  PTR,                                               { Data pointer                                                          }
  MAXCNT,                                            { Maximum number of bytes in transmit buffer                            }
  AMOUNT,                                            { Number of bytes transferred in the last transaction                   }
  LIST: longword;                                    { EasyDMA list type                                                     }
end;

TWIS_PSEL_Type = record
  SCL,                                               { Pin select for SCL signal                                             }
  SDA: longword;                                     { Pin select for SDA signal                                             }
end;

TWIS_RXD_Type = record
  PTR,                                               { RXD Data pointer                                                      }
  MAXCNT,                                            { Maximum number of bytes in RXD buffer                                 }
  AMOUNT: longword;                                  { Number of bytes transferred in the last RXD transaction               }
end;

TWIS_TXD_Type = record
  PTR,                                               { TXD Data pointer                                                      }
  MAXCNT,                                            { Maximum number of bytes in TXD buffer                                 }
  AMOUNT: longword;                                  { Number of bytes transferred in the last TXD transaction               }
end;

SPI_PSEL_Type = record
  SCK,                                               { Pin select for SCK                                                    }
  MOSI,                                              { Pin select for MOSI                                                   }
  MISO: longword;                                    { Pin select for MISO                                                   }
end;

NFCT_FRAMESTATUS_Type = record
  RX: longword;                                      { Result of last incoming frames                                        }
end;

NFCT_TXD_Type = record
  FRAMECONFIG,                                       { Configuration of outgoing frames                                      }
  AMOUNT: longword;                                  { Size of outgoing frame                                                }
end;

NFCT_RXD_Type = record
  FRAMECONFIG,                                       { Configuration of incoming frames                                      }
  AMOUNT: longword;                                  { Size of last incoming frame                                           }
end;

SAADC_EVENTS_CH_Type = record
  LIMITH,                                            { Description cluster[0]: Last results is equal or above CH[0].LIMIT.HIGH }
  LIMITL: longword;                                  { Description cluster[0]: Last results is equal or below CH[0].LIMIT.LOW }
end;

SAADC_CH_Type = record
  PSELP,                                             { Description cluster[0]: Input positive pin selection for CH[0]        }
  PSELN,                                             { Description cluster[0]: Input negative pin selection for CH[0]        }
  CONFIG,                                            { Description cluster[0]: Input configuration for CH[0]                 }
  LIMIT: longword;                                   { Description cluster[0]: High/low limits for event monitoring
                                                       a channel                                                             }
end;

SAADC_RESULT_Type = record
  PTR,                                               { Data pointer                                                          }
  MAXCNT,                                            { Maximum number of buffer words to transfer                            }
  AMOUNT: longword;                                  { Number of buffer words transferred since last START                   }
end;

QDEC_PSEL_Type = record
  LED,                                               { Pin select for LED signal                                             }
  A,                                                 { Pin select for A signal                                               }
  B: longword;                                       { Pin select for B signal                                               }
end;

PWM_SEQ_Type = record
  PTR,                                               { Description cluster[0]: Beginning address in Data RAM of sequence
                                                       A                                                                     }
  CNT,                                               { Description cluster[0]: Amount of values (duty cycles) in sequence
                                                       A                                                                     }
  REFRESH,                                           { Description cluster[0]: Amount of additional PWM periods between
                                                       samples loaded to compare register (load every CNT+1 PWM periods)     }
  ENDDELAY: longword;                                { Description cluster[0]: Time added after the sequence                 }
  RESERVED1: array [0..3] of longword;
end;

PWM_PSEL_Type = record
  OUT: array [0..3] of longword;                     { Description collection[0]: Output pin select for PWM channel
                                                       0                                                                     }
end;

PDM_PSEL_Type = record
  CLK,                                               { Pin number configuration for PDM CLK signal                           }
  DIN: longword;                                     { Pin number configuration for PDM DIN signal                           }
end;

PDM_SAMPLE_Type = record
  PTR,                                               { RAM address pointer to write samples to with EasyDMA                  }
  MAXCNT: longword;                                  { Number of samples to allocate memory for in EasyDMA mode              }
end;

PPI_TASKS_CHG_Type = record
  EN,                                                { Description cluster[0]: Enable channel group 0                        }
  DIS: longword;                                     { Description cluster[0]: Disable channel group 0                       }
end;

PPI_CH_Type = record
  EEP,                                               { Description cluster[0]: Channel 0 event end-point                     }
  TEP: longword;                                     { Description cluster[0]: Channel 0 task end-point                      }
end;

PPI_FORK_Type = record
  TEP: longword;                                     { Description cluster[0]: Channel 0 task end-point                      }
end;

MWU_EVENTS_REGION_Type = record
  WA,                                                { Description cluster[0]: Write access to region 0 detected             }
  RA: longword;                                      { Description cluster[0]: Read access to region 0 detected              }
end;

MWU_EVENTS_PREGION_Type = record
  WA,                                                { Description cluster[0]: Write access to peripheral region 0
                                                       detected                                                              }
  RA: longword;                                      { Description cluster[0]: Read access to peripheral region 0 detected   }
end;

MWU_PERREGION_Type = record
  SUBSTATWA,                                         { Description cluster[0]: Source of event/interrupt in region 0,
                                                       write access detected while corresponding subregion was enabled
                                                       for watching                                                          }
  SUBSTATRA: longword;                               { Description cluster[0]: Source of event/interrupt in region 0,
                                                       read access detected while corresponding subregion was enabled
                                                       for watching                                                          }
end;

MWU_REGION_Type = record
  START,                                             { Description cluster[0]: Start address for region 0                    }
  END_,                                              { Description cluster[0]: End address of region 0                       }
  RESERVED2: array [0..1] of longword;
end;

MWU_PREGION_Type = record
  START,                                             { Description cluster[0]: Reserved for future use                       }
  END_,                                              { Description cluster[0]: Reserved for future use                       }
  SUBS,                                              { Description cluster[0]: Subregions of region 0                        }
  RESERVED3: longword;
end;

I2S_CONFIG_Type = record
  MODE,                                              { I<sup>2</sup>S mode.                                                  }
  RXEN,                                              { Reception (RX) enable.                                                }
  TXEN,                                              { Transmission (TX) enable.                                             }
  MCKEN,                                             { Master clock generator enable.                                        }
  MCKFREQ,                                           { Master clock generator frequency.                                     }
  RATIO,                                             { MCK / LRCK ratio.                                                     }
  SWIDTH,                                            { Sample width.                                                         }
  ALIGN,                                             { Alignment of sample within a frame.                                   }
  FORMAT,                                            { Frame format.                                                         }
  CHANNELS: longword;                                { Enable channels.                                                      }
end;

I2S_RXD_Type = record
  PTR: longword;                                     { Receive buffer RAM start address.                                     }
end;

I2S_TXD_Type = record
  PTR: longword;                                     { Transmit buffer RAM start address.                                    }
end;

I2S_RXTXD_Type = record
  MAXCNT: longword;                                  { Size of RXD and TXD buffers.                                          }
end;

I2S_PSEL_Type = record
  MCK,                                               { Pin select for MCK signal.                                            }
  SCK,                                               { Pin select for SCK signal.                                            }
  LRCK,                                              { Pin select for LRCK signal.                                           }
  SDIN,                                              { Pin select for SDIN signal.                                           }
  SDOUT: longword;                                   { Pin select for SDOUT signal.                                          }
end;


{ ================================================================================ }
{ ================                      FICR                      ================ }
{ ================================================================================ }

{
  Factory Information Configuration Registers (FICR)
}

NRF_FICR_Type = record                               { FICR Structure                                                        }
  RESERVED0: array [0..3] of longword;
  CODEPAGESIZE,                                      { Code memory page size                                                 }
  CODESIZE: longword;                                { Code memory size                                                      }
  RESERVED1: array [0..17] of longword;
  DEVICEID: array [0..1] of longword;                { Description collection[0]: Device identifier                          }
  RESERVED2: array [0..5] of longword;
  ER: array [0..3] of longword;                      { Description collection[0]: Encryption Root, word 0                    }
  IR: array [0..3] of longword;                      { Description collection[0]: Identity Root, word 0                      }
  DEVICEADDRTYPE: longword;                          { Device address type                                                   }
  DEVICEADDR: array [0..1] of longword;              { Description collection[0]: Device address 0                           }
  RESERVED3: array [0..20] of longword;
  INFO: FICR_INFO_Type;                              { Device info                                                           }
  RESERVED4: array [0..184] of longword;
  TEMP: FICR_TEMP_Type;                              { Registers storing factory TEMP module linearization coefficients      }
  RESERVED5: array [0..1] of longword;
  NFC: FICR_NFC_Type;                                { Unspecified                                                           }
end;


{ ================================================================================ }
{ ================                      UICR                      ================ }
{ ================================================================================ }

{
  User Information Configuration Registers (UICR)
}

NRF_UICR_Type = record                               { UICR Structure                                                        }
  UNUSED0,                                           { Unspecified                                                           }
  UNUSED1,                                           { Unspecified                                                           }
  UNUSED2,                                           { Unspecified                                                           }
  RESERVED0,
  UNUSED3: longword;                                 { Unspecified                                                           }
  NRFFW: array [0..14] of longword;                  { Description collection[0]: Reserved for Nordic firmware design        }
  NRFHW: array [0..11] of longword;                  { Description collection[0]: Reserved for Nordic hardware design        }
  CUSTOMER: array [0..31] of longword;               { Description collection[0]: Reserved for customer                      }
  RESERVED1: array [0..63] of longword;
  PSELRESET: array [0..1] of longword;               { Description collection[0]: Mapping of the nRESET function (see
                                                       POWER chapter for details)                                            }
  APPROTECT,                                         { Access Port protection                                                }
  NFCPINS: longword;                                 { Setting of pins dedicated to NFC functionality: NFC antenna
                                                       or GPIO                                                               }
end;


{ ================================================================================ }
{ ================                      BPROT                     ================ }
{ ================================================================================ }

{
  Block Protect (BPROT)
}

NRF_BPROT_Type = record                              { BPROT Structure                                                       }
  RESERVED0: array [0..383] of longword;
  CONFIG0,                                           { Block protect configuration register 0                                }
  CONFIG1,                                           { Block protect configuration register 1                                }
  DISABLEINDEBUG,                                    { Disable protection mechanism in debug interface mode                  }
  UNUSED0,                                           { Unspecified                                                           }
  CONFIG2,                                           { Block protect configuration register 2                                }
  CONFIG3: longword;                                 { Block protect configuration register 3                                }
end;


{ ================================================================================ }
{ ================                      POWER                     ================ }
{ ================================================================================ }

{
  Power control (POWER)
}

NRF_POWER_Type = record                              { POWER Structure                                                       }
  RESERVED0: array [0..29] of longword;
  TASKS_CONSTLAT,                                    { Enable constant latency mode                                          }
  TASKS_LOWPWR: longword;                            { Enable low power mode (variable latency)                              }
  RESERVED1: array [0..33] of longword;
  EVENTS_POFWARN: longword;                          { Power failure warning                                                 }
  RESERVED2: array [0..1] of longword;
  EVENTS_SLEEPENTER,                                 { CPU entered WFI/WFE sleep                                             }
  EVENTS_SLEEPEXIT: longword;                        { CPU exited WFI/WFE sleep                                              }
  RESERVED3: array [0..121] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED4: array [0..60] of longword;
  RESETREAS: longword;                               { Reset reason                                                          }
  RESERVED5: array [0..8] of longword;
  RAMSTATUS: longword;                               { Deprecated register - RAM status register                             }
  RESERVED6: array [0..52] of longword;
  SYSTEMOFF: longword;                               { System OFF register                                                   }
  RESERVED7: array [0..2] of longword;
  POFCON: longword;                                  { Power failure comparator configuration                                }
  RESERVED8: array [0..1] of longword;
  GPREGRET,                                          { General purpose retention register                                    }
  GPREGRET2,                                         { General purpose retention register                                    }
  RAMON: longword;                                   { Deprecated register - RAM on/off register (this register is
                                                       retained)                                                             }
  RESERVED9: array [0..10] of longword;
  RAMONB: longword;                                  { Deprecated register - RAM on/off register (this register is
                                                       retained)                                                             }
  RESERVED10: array [0..7] of longword;
  DCDCEN: longword;                                  { DC/DC enable register                                                 }
  RESERVED11: array [0..224] of longword;
  RAM: array [0..7] of POWER_RAM_Type;               { Unspecified                                                           }
end;


{ ================================================================================ }
{ ================                      CLOCK                     ================ }
{ ================================================================================ }

{
  Clock control (CLOCK)
}

NRF_CLOCK_Type = record                              { CLOCK Structure                                                       }
  TASKS_HFCLKSTART,                                  { Start HFCLK crystal oscillator                                        }
  TASKS_HFCLKSTOP,                                   { Stop HFCLK crystal oscillator                                         }
  TASKS_LFCLKSTART,                                  { Start LFCLK source                                                    }
  TASKS_LFCLKSTOP,                                   { Stop LFCLK source                                                     }
  TASKS_CAL,                                         { Start calibration of LFRC oscillator                                  }
  TASKS_CTSTART,                                     { Start calibration timer                                               }
  TASKS_CTSTOP: longword;                            { Stop calibration timer                                                }
  RESERVED0: array [0..56] of longword;
  EVENTS_HFCLKSTARTED,                               { HFCLK oscillator started                                              }
  EVENTS_LFCLKSTARTED,                               { LFCLK started                                                         }
  RESERVED1,
  EVENTS_DONE,                                       { Calibration of LFCLK RC oscillator complete event                     }
  EVENTS_CTTO: longword;                             { Calibration timer timeout                                             }
  RESERVED2: array [0..123] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..62] of longword;
  HFCLKRUN,                                          { Status indicating that HFCLKSTART task has been triggered             }
  HFCLKSTAT,                                         { HFCLK status                                                          }
  RESERVED4,
  LFCLKRUN,                                          { Status indicating that LFCLKSTART task has been triggered             }
  LFCLKSTAT,                                         { LFCLK status                                                          }
  LFCLKSRCCOPY: longword;                            { Copy of LFCLKSRC register, set when LFCLKSTART task was triggered     }
  RESERVED5: array [0..61] of longword;
  LFCLKSRC: longword;                                { Clock source for the LFCLK                                            }
  RESERVED6: array [0..6] of longword;
  CTIV: longword;                                    { Calibration timer interval (retained register, same reset behaviour
                                                       as RESETREAS)                                                         }
  RESERVED7: array [0..7] of longword;
  TRACECONFIG: longword;                             { Clocking options for the Trace Port debug interface                   }
end;


{ ================================================================================ }
{ ================                      AMLI                      ================ }
{ ================================================================================ }

{
  AHB Multi-Layer Interface (AMLI)
}

NRF_AMLI_Type = record                               { AMLI Structure                                                        }
  RESERVED0: array [0..895] of longword;
  RAMPRI: AMLI_RAMPRI_Type;                          { RAM configurable priority configuration structure                     }
end;


{ ================================================================================ }
{ ================                      RADIO                     ================ }
{ ================================================================================ }

{
  2.4 GHz Radio (RADIO)
}

NRF_RADIO_Type = record                              { RADIO Structure                                                       }
  TASKS_TXEN,                                        { Enable RADIO in TX mode                                               }
  TASKS_RXEN,                                        { Enable RADIO in RX mode                                               }
  TASKS_START,                                       { Start RADIO                                                           }
  TASKS_STOP,                                        { Stop RADIO                                                            }
  TASKS_DISABLE,                                     { Disable RADIO                                                         }
  TASKS_RSSISTART,                                   { Start the RSSI and take one single sample of the receive signal
                                                       strength.                                                             }
  TASKS_RSSISTOP,                                    { Stop the RSSI measurement                                             }
  TASKS_BCSTART,                                     { Start the bit counter                                                 }
  TASKS_BCSTOP: longword;                            { Stop the bit counter                                                  }
  RESERVED0: array [0..54] of longword;
  EVENTS_READY,                                      { RADIO has ramped up and is ready to be started                        }
  EVENTS_ADDRESS,                                    { Address sent or received                                              }
  EVENTS_PAYLOAD,                                    { Packet payload sent or received                                       }
  EVENTS_END,                                        { Packet sent or received                                               }
  EVENTS_DISABLED,                                   { RADIO has been disabled                                               }
  EVENTS_DEVMATCH,                                   { A device address match occurred on the last received packet           }
  EVENTS_DEVMISS,                                    { No device address match occurred on the last received packet          }
  EVENTS_RSSIEND: longword;                          { Sampling of receive signal strength complete.                         }
  RESERVED1: array [0..1] of longword;
  EVENTS_BCMATCH,                                    { Bit counter reached bit count value.                                  }
  RESERVED2,
  EVENTS_CRCOK,                                      { Packet received with CRC ok                                           }
  EVENTS_CRCERROR: longword;                         { Packet received with CRC error                                        }
  RESERVED3: array [0..49] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED4: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED5: array [0..60] of longword;
  CRCSTATUS,                                         { CRC status                                                            }
  RESERVED6,
  RXMATCH,                                           { Received address                                                      }
  RXCRC,                                             { CRC field of previously received packet                               }
  DAI: longword;                                     { Device address match index                                            }
  RESERVED7: array [0..59] of longword;
  PACKETPTR,                                         { Packet pointer                                                        }
  FREQUENCY,                                         { Frequency                                                             }
  TXPOWER,                                           { Output power                                                          }
  MODE,                                              { Data rate and modulation                                              }
  PCNF0,                                             { Packet configuration register 0                                       }
  PCNF1,                                             { Packet configuration register 1                                       }
  BASE0,                                             { Base address 0                                                        }
  BASE1,                                             { Base address 1                                                        }
  PREFIX0,                                           { Prefixes bytes for logical addresses 0-3                              }
  PREFIX1,                                           { Prefixes bytes for logical addresses 4-7                              }
  TXADDRESS,                                         { Transmit address select                                               }
  RXADDRESSES,                                       { Receive address select                                                }
  CRCCNF,                                            { CRC configuration                                                     }
  CRCPOLY,                                           { CRC polynomial                                                        }
  CRCINIT,                                           { CRC initial value                                                     }
  UNUSED0,                                           { Unspecified                                                           }
  TIFS,                                              { Inter Frame Spacing in us                                             }
  RSSISAMPLE,                                        { RSSI sample                                                           }
  RESERVED8,
  STATE,                                             { Current radio state                                                   }
  DATAWHITEIV: longword;                             { Data whitening initial value                                          }
  RESERVED9: array [0..1] of longword;
  BCC: longword;                                     { Bit counter compare                                                   }
  RESERVED10: array [0..38] of longword;
  DAB: array [0..7] of longword;                     { Description collection[0]: Device address base segment 0              }
  DAP: array [0..7] of longword;                     { Description collection[0]: Device address prefix 0                    }
  DACNF: longword;                                   { Device address match configuration                                    }
  RESERVED11: array [0..2] of longword;
  MODECNF0: longword;                                { Radio mode configuration register 0                                   }
  RESERVED12: array [0..617] of longword;
  POWER: longword;                                   { Peripheral power control                                              }
end;


{ ================================================================================ }
{ ================                      UARTE                     ================ }
{ ================================================================================ }

{
  UART with EasyDMA (UARTE)
}

NRF_UARTE_Type = record                              { UARTE Structure                                                       }
  TASKS_STARTRX,                                     { Start UART receiver                                                   }
  TASKS_STOPRX,                                      { Stop UART receiver                                                    }
  TASKS_STARTTX,                                     { Start UART transmitter                                                }
  TASKS_STOPTX: longword;                            { Stop UART transmitter                                                 }
  RESERVED0: array [0..6] of longword;
  TASKS_FLUSHRX: longword;                           { Flush RX FIFO into RX buffer                                          }
  RESERVED1: array [0..51] of longword;
  EVENTS_CTS,                                        { CTS is activated (set low). Clear To Send.                            }
  EVENTS_NCTS: longword;                             { CTS is deactivated (set high). Not Clear To Send.                     }
  RESERVED2: array [0..1] of longword;
  EVENTS_ENDRX: longword;                            { Receive buffer is filled up                                           }
  RESERVED3: array [0..2] of longword;
  EVENTS_ENDTX,                                      { Last TX byte transmitted                                              }
  EVENTS_ERROR: longword;                            { Error detected                                                        }
  RESERVED4: array [0..6] of longword;
  EVENTS_RXTO,                                       { Receiver timeout                                                      }
  RESERVED5,
  EVENTS_RXSTARTED,                                  { UART receiver has started                                             }
  EVENTS_TXSTARTED,                                  { UART transmitter has started                                          }
  RESERVED6,
  EVENTS_TXSTOPPED: longword;                        { Transmitter stopped                                                   }
  RESERVED7: array [0..40] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED8: array [0..62] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED9: array [0..92] of longword;
  ERRORSRC: longword;                                { Error source                                                          }
  RESERVED10: array [0..30] of longword;
  ENABLE,                                            { Enable UART                                                           }
  RESERVED11,
  PSEL: UARTE_PSEL_Type;                             { Unspecified                                                           }
  RESERVED12: array [0..2] of longword;
  BAUDRATE: longword;                                { Baud rate                                                             }
  RESERVED13: array [0..2] of longword;
  RXD: UARTE_RXD_Type;                               { RXD EasyDMA channel                                                   }
  RESERVED14,
  TXD: UARTE_TXD_Type;                               { TXD EasyDMA channel                                                   }
  RESERVED15: array [0..6] of longword;
  CONFIG: longword;                                  { Configuration of parity and hardware flow control                     }
end;


{ ================================================================================ }
{ ================                      UART                      ================ }
{ ================================================================================ }

{
  Universal Asynchronous Receiver/Transmitter (UART)
}

NRF_UART_Type = record                               { UART Structure                                                        }
  TASKS_STARTRX,                                     { Start UART receiver                                                   }
  TASKS_STOPRX,                                      { Stop UART receiver                                                    }
  TASKS_STARTTX,                                     { Start UART transmitter                                                }
  TASKS_STOPTX: longword;                            { Stop UART transmitter                                                 }
  RESERVED0: array [0..2] of longword;
  TASKS_SUSPEND: longword;                           { Suspend UART                                                          }
  RESERVED1: array [0..55] of longword;
  EVENTS_CTS,                                        { CTS is activated (set low). Clear To Send.                            }
  EVENTS_NCTS,                                       { CTS is deactivated (set high). Not Clear To Send.                     }
  EVENTS_RXDRDY: longword;                           { Data received in RXD                                                  }
  RESERVED2: array [0..3] of longword;
  EVENTS_TXDRDY,                                     { Data sent from TXD                                                    }
  RESERVED3,
  EVENTS_ERROR: longword;                            { Error detected                                                        }
  RESERVED4: array [0..6] of longword;
  EVENTS_RXTO: longword;                             { Receiver timeout                                                      }
  RESERVED5: array [0..45] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED6: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED7: array [0..92] of longword;
  ERRORSRC: longword;                                { Error source                                                          }
  RESERVED8: array [0..30] of longword;
  ENABLE,                                            { Enable UART                                                           }
  RESERVED9,
  PSELRTS,                                           { Pin select for RTS                                                    }
  PSELTXD,                                           { Pin select for TXD                                                    }
  PSELCTS,                                           { Pin select for CTS                                                    }
  PSELRXD,                                           { Pin select for RXD                                                    }
  RXD,                                               { RXD register                                                          }
  TXD,                                               { TXD register                                                          }
  RESERVED10,
  BAUDRATE: longword;                                { Baud rate                                                             }
  RESERVED11: array [0..16] of longword;
  CONFIG: longword;                                  { Configuration of parity and hardware flow control                     }
end;


{ ================================================================================ }
{ ================                      SPIM                      ================ }
{ ================================================================================ }

{
  Serial Peripheral Interface Master with EasyDMA 0 (SPIM)
}

NRF_SPIM_Type = record                               { SPIM Structure                                                        }
  RESERVED0: array [0..3] of longword;
  TASKS_START,                                       { Start SPI transaction                                                 }
  TASKS_STOP,                                        { Stop SPI transaction                                                  }
  RESERVED1,
  TASKS_SUSPEND,                                     { Suspend SPI transaction                                               }
  TASKS_RESUME: longword;                            { Resume SPI transaction                                                }
  RESERVED2: array [0..55] of longword;
  EVENTS_STOPPED: longword;                          { SPI transaction has stopped                                           }
  RESERVED3: array [0..1] of longword;
  EVENTS_ENDRX,                                      { End of RXD buffer reached                                             }
  RESERVED4,
  EVENTS_END,                                        { End of RXD buffer and TXD buffer reached                              }
  RESERVED5,
  EVENTS_ENDTX: longword;                            { End of TXD buffer reached                                             }
  RESERVED6: array [0..9] of longword;
  EVENTS_STARTED: longword;                          { Transaction started                                                   }
  RESERVED7: array [0..43] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED8: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED9: array [0..124] of longword;
  ENABLE,                                            { Enable SPIM                                                           }
  RESERVED10,
  PSEL: SPIM_PSEL_Type;                              { Unspecified                                                           }
  RESERVED11: array [0..3] of longword;
  FREQUENCY: longword;                               { SPI frequency                                                         }
  RESERVED12: array [0..2] of longword;
  RXD: SPIM_RXD_Type;                                { RXD EasyDMA channel                                                   }
  TXD: SPIM_TXD_Type;                                { TXD EasyDMA channel                                                   }
  CONFIG: longword;                                  { Configuration register                                                }
  RESERVED13: array [0..25] of longword;
  ORC: longword;                                     { Over-read character. Character clocked out in case and over-read
                                                       of the TXD buffer.                                                    }
end;


{ ================================================================================ }
{ ================                      SPIS                      ================ }
{ ================================================================================ }

{
  SPI Slave 0 (SPIS)
}

NRF_SPIS_Type = record                               { SPIS Structure                                                        }
  RESERVED0: array [0..8] of longword;
  TASKS_ACQUIRE,                                     { Acquire SPI semaphore                                                 }
  TASKS_RELEASE: longword;                           { Release SPI semaphore, enabling the SPI slave to acquire it           }
  RESERVED1: array [0..53] of longword;
  EVENTS_END: longword;                              { Granted transaction completed                                         }
  RESERVED2: array [0..1] of longword;
  EVENTS_ENDRX: longword;                            { End of RXD buffer reached                                             }
  RESERVED3: array [0..4] of longword;
  EVENTS_ACQUIRED: longword;                         { Semaphore acquired                                                    }
  RESERVED4: array [0..52] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED5: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED6: array [0..60] of longword;
  SEMSTAT: longword;                                 { Semaphore status register                                             }
  RESERVED7: array [0..14] of longword;
  STATUS: longword;                                  { Status from last transaction                                          }
  RESERVED8: array [0..46] of longword;
  ENABLE,                                            { Enable SPI slave                                                      }
  RESERVED9: longword;
  PSEL: SPIS_PSEL_Type;                              { Unspecified                                                           }
  RESERVED10: array [0..6] of longword;
  RXD: SPIS_RXD_Type;                                { Unspecified                                                           }
  RESERVED11,
  TXD: SPIS_TXD_Type;                                { Unspecified                                                           }
  RESERVED12,
  CONFIG,                                            { Configuration register                                                }
  RESERVED13,
  DEF: longword;                                     { Default character. Character clocked out in case of an ignored
                                                       transaction.                                                          }
  RESERVED14: array [0..23] of longword;
  ORC: longword;                                     { Over-read character                                                   }
end;


{ ================================================================================ }
{ ================                      TWIM                      ================ }
{ ================================================================================ }

{
  I2C compatible Two-Wire Master Interface with EasyDMA 0 (TWIM)
}

NRF_TWIM_Type = record                               { TWIM Structure                                                        }
  TASKS_STARTRX,                                     { Start TWI receive sequence                                            }
  RESERVED0,
  TASKS_STARTTX: longword;                           { Start TWI transmit sequence                                           }
  RESERVED1: array [0..1] of longword;
  TASKS_STOP,                                        { Stop TWI transaction. Must be issued while the TWI master is
                                                       not suspended.                                                        }
  RESERVED2,
  TASKS_SUSPEND,                                     { Suspend TWI transaction                                               }
  TASKS_RESUME: longword;                            { Resume TWI transaction                                                }
  RESERVED3: array [0..55] of longword;
  EVENTS_STOPPED: longword;                          { TWI stopped                                                           }
  RESERVED4: array [0..6] of longword;
  EVENTS_ERROR: longword;                            { TWI error                                                             }
  RESERVED5: array [0..7] of longword;
  EVENTS_SUSPENDED,                                  { Last byte has been sent out after the SUSPEND task has been
                                                       issued, TWI traffic is now suspended.                                 }
  EVENTS_RXSTARTED,                                  { Receive sequence started                                              }
  EVENTS_TXSTARTED: longword;                        { Transmit sequence started                                             }
  RESERVED6: array [0..1] of longword;
  EVENTS_LASTRX,                                     { Byte boundary, starting to receive the last byte                      }
  EVENTS_LASTTX: longword;                           { Byte boundary, starting to transmit the last byte                     }
  RESERVED7: array [0..38] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED8: array [0..62] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED9: array [0..109] of longword;
  ERRORSRC: longword;                                { Error source                                                          }
  RESERVED10: array [0..13] of longword;
  ENABLE,                                            { Enable TWIM                                                           }
  RESERVED11: longword;
  PSEL: TWIM_PSEL_Type;                              { Unspecified                                                           }
  RESERVED12: array [0..4] of longword;
  FREQUENCY: longword;                               { TWI frequency                                                         }
  RESERVED13: array [0..2] of longword;
  RXD: TWIM_RXD_Type;                                { RXD EasyDMA channel                                                   }
  TXD: TWIM_TXD_Type;                                { TXD EasyDMA channel                                                   }
  RESERVED14: array [0..12] of longword;
  ADDRESS: longword;                                 { Address used in the TWI transfer                                      }
end;


{ ================================================================================ }
{ ================                      TWIS                      ================ }
{ ================================================================================ }

{
  I2C compatible Two-Wire Slave Interface with EasyDMA 0 (TWIS)
}

NRF_TWIS_Type = record                               { TWIS Structure                                                        }
  RESERVED0: array [0..4] of longword;
  TASKS_STOP,                                        { Stop TWI transaction                                                  }
  RESERVED1,
  TASKS_SUSPEND,                                     { Suspend TWI transaction                                               }
  TASKS_RESUME: longword;                            { Resume TWI transaction                                                }
  RESERVED2: array [0..2] of longword;
  TASKS_PREPARERX,                                   { Prepare the TWI slave to respond to a write command                   }
  TASKS_PREPARETX: longword;                         { Prepare the TWI slave to respond to a read command                    }
  RESERVED3: array [0..50] of longword;
  EVENTS_STOPPED: longword;                          { TWI stopped                                                           }
  RESERVED4: array [0..6] of longword;
  EVENTS_ERROR: longword;                            { TWI error                                                             }
  RESERVED5: array [0..8] of longword;
  EVENTS_RXSTARTED,                                  { Receive sequence started                                              }
  EVENTS_TXSTARTED: longword;                        { Transmit sequence started                                             }
  RESERVED6: array [0..3] of longword;
  EVENTS_WRITE,                                      { Write command received                                                }
  EVENTS_READ: longword;                             { Read command received                                                 }
  RESERVED7: array [0..36] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED8: array [0..62] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED9: array [0..112] of longword;
  ERRORSRC,                                          { Error source                                                          }
  MATCH: longword;                                   { Status register indicating which address had a match                  }
  RESERVED10: array [0..9] of longword;
  ENABLE,                                            { Enable TWIS                                                           }
  RESERVED11: longword;
  PSEL: TWIS_PSEL_Type;                              { Unspecified                                                           }
  RESERVED12: array [0..8] of longword;
  RXD: TWIS_RXD_Type;                                { RXD EasyDMA channel                                                   }
  RESERVED13,
  TXD: TWIS_TXD_Type;                                { TXD EasyDMA channel                                                   }
  RESERVED14: array [0..13] of longword;
  ADDRESS: array [0..1] of longword;                 { Description collection[0]: TWI slave address 0                        }
  RESERVED15,
  CONFIG: longword;                                  { Configuration register for the address match mechanism                }
  RESERVED16: array [0..9] of longword;
  ORC: longword;                                     { Over-read character. Character sent out in case of an over-read
                                                       of the transmit buffer.                                               }
end;


{ ================================================================================ }
{ ================                       SPI                      ================ }
{ ================================================================================ }

{
  Serial Peripheral Interface 0 (SPI)
}

NRF_SPI_Type = record                                { SPI Structure                                                         }
  RESERVED0: array [0..65] of longword;
  EVENTS_READY: longword;                            { TXD byte sent and RXD byte received                                   }
  RESERVED1: array [0..125] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED2: array [0..124] of longword;
  ENABLE,                                            { Enable SPI                                                            }
  RESERVED3: longword;
  PSEL: SPI_PSEL_Type;                               { Unspecified                                                           }
  RESERVED4,
  RXD,                                               { RXD register                                                          }
  TXD,                                               { TXD register                                                          }
  RESERVED5,
  FREQUENCY: longword;                               { SPI frequency                                                         }
  RESERVED6: array [0..10] of longword;
  CONFIG: longword;                                  { Configuration register                                                }
end;


{ ================================================================================ }
{ ================                       TWI                      ================ }
{ ================================================================================ }

{
  I2C compatible Two-Wire Interface 0 (TWI)
}

NRF_TWI_Type = record                                { TWI Structure                                                         }
  TASKS_STARTRX,                                     { Start TWI receive sequence                                            }
  RESERVED0,
  TASKS_STARTTX: longword;                           { Start TWI transmit sequence                                           }
  RESERVED1: array [0..1] of longword;
  TASKS_STOP,                                        { Stop TWI transaction                                                  }
  RESERVED2,
  TASKS_SUSPEND,                                     { Suspend TWI transaction                                               }
  TASKS_RESUME: longword;                            { Resume TWI transaction                                                }
  RESERVED3: array [0..55] of longword;
  EVENTS_STOPPED,                                    { TWI stopped                                                           }
  EVENTS_RXDREADY: longword;                         { TWI RXD byte received                                                 }
  RESERVED4: array [0..3] of longword;
  EVENTS_TXDSENT,                                    { TWI TXD byte sent                                                     }
  RESERVED5,
  EVENTS_ERROR: longword;                            { TWI error                                                             }
  RESERVED6: array [0..3] of longword;
  EVENTS_BB: longword;                               { TWI byte boundary, generated before each byte that is sent or
                                                       received                                                              }
  RESERVED7: array [0..2] of longword;
  EVENTS_SUSPENDED: longword;                        { TWI entered the suspended state                                       }
  RESERVED8: array [0..44] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED9: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED10: array [0..109] of longword;
  ERRORSRC: longword;                                { Error source                                                          }
  RESERVED11: array [0..13] of longword;
  ENABLE,                                            { Enable TWI                                                            }
  RESERVED12,
  PSELSCL,                                           { Pin select for SCL                                                    }
  PSELSDA: longword;                                 { Pin select for SDA                                                    }
  RESERVED13: array [0..1] of longword;
  RXD,                                               { RXD register                                                          }
  TXD,                                               { TXD register                                                          }
  RESERVED14,
  FREQUENCY: longword;                               { TWI frequency                                                         }
  RESERVED15: array [0..23] of longword;
  ADDRESS: longword;                                 { Address used in the TWI transfer                                      }
end;


{ ================================================================================ }
{ ================                      NFCT                      ================ }
{ ================================================================================ }

{
  NFC-A compatible radio (NFCT)
}

NRF_NFCT_Type = record                               { NFCT Structure                                                        }
  TASKS_ACTIVATE,                                    { Activate NFC peripheral for incoming and outgoing frames, change
                                                       state to activated                                                    }
  TASKS_DISABLE,                                     { Disable NFC peripheral                                                }
  TASKS_SENSE,                                       { Enable NFC sense field mode, change state to sense mode               }
  TASKS_STARTTX: longword;                           { Start transmission of a outgoing frame, change state to transmit      }
  RESERVED0: array [0..2] of longword;
  TASKS_ENABLERXDATA,                                { Initializes the EasyDMA for receive.                                  }
  RESERVED1,
  TASKS_GOIDLE,                                      { Force state machine to IDLE state                                     }
  TASKS_GOSLEEP: longword;                           { Force state machine to SLEEP_A state                                  }
  RESERVED2: array [0..52] of longword;
  EVENTS_READY,                                      { The NFC peripheral is ready to receive and send frames                }
  EVENTS_FIELDDETECTED,                              { Remote NFC field detected                                             }
  EVENTS_FIELDLOST,                                  { Remote NFC field lost                                                 }
  EVENTS_TXFRAMESTART,                               { Marks the start of the first symbol of a transmitted frame            }
  EVENTS_TXFRAMEEND,                                 { Marks the end of the last transmitted on-air symbol of a frame        }
  EVENTS_RXFRAMESTART,                               { Marks the end of the first symbol of a received frame                 }
  EVENTS_RXFRAMEEND,                                 { Received data have been checked (CRC, parity) and transferred
                                                       to RAM, and EasyDMA has ended accessing the RX buffer                 }
  EVENTS_ERROR: longword;                            { NFC error reported. The ERRORSTATUS register contains details
                                                       on the source of the error.                                           }
  RESERVED3: array [0..1] of longword;
  EVENTS_RXERROR,                                    { NFC RX frame error reported. The FRAMESTATUS.RX register contains
                                                       details on the source of the error.                                   }
  EVENTS_ENDRX,                                      { RX buffer (as defined by PACKETPTR and MAXLEN) in Data RAM full.      }
  EVENTS_ENDTX,                                      { Transmission of data in RAM has ended, and EasyDMA has ended
                                                       accessing the TX buffer                                               }
  RESERVED4,
  EVENTS_AUTOCOLRESSTARTED: longword;                { Auto collision resolution process has started                         }
  RESERVED5: array [0..2] of longword;
  EVENTS_COLLISION,                                  { NFC Auto collision resolution error reported.                         }
  EVENTS_SELECTED,                                   { NFC Auto collision resolution successfully completed                  }
  EVENTS_STARTED: longword;                          { EasyDMA is ready to receive or send frames.                           }
  RESERVED6: array [0..42] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED7: array [0..62] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED8: array [0..61] of longword;
  ERRORSTATUS,                                       { NFC Error Status register                                             }
  RESERVED9: longword;
  FRAMESTATUS: NFCT_FRAMESTATUS_Type;                { Unspecified                                                           }
  RESERVED10: array [0..7] of longword;
  CURRENTLOADCTRL: longword;                         { Current value driven to the NFC Load Control                          }
  RESERVED11: array [0..1] of longword;
  FIELDPRESENT: longword;                            { Indicates the presence or not of a valid field                        }
  RESERVED12: array [0..48] of longword;
  FRAMEDELAYMIN,                                     { Minimum frame delay                                                   }
  FRAMEDELAYMAX,                                     { Maximum frame delay                                                   }
  FRAMEDELAYMODE,                                    { Configuration register for the Frame Delay Timer                      }
  PACKETPTR,                                         { Packet pointer for TXD and RXD data storage in Data RAM               }
  MAXLEN: longword;                                  { Size of allocated for TXD and RXD data storage buffer in Data
                                                       RAM                                                                   }
  TXD: NFCT_TXD_Type;                                { Unspecified                                                           }
  RXD: NFCT_RXD_Type;                                { Unspecified                                                           }
  RESERVED13: array [0..25] of longword;
  NFCID1_LAST,                                       { Last NFCID1 part (4, 7 or 10 bytes ID)                                }
  NFCID1_2ND_LAST,                                   { Second last NFCID1 part (7 or 10 bytes ID)                            }
  NFCID1_3RD_LAST,                                   { Third last NFCID1 part (10 bytes ID)                                  }
  RESERVED14,
  SENSRES,                                           { NFC-A SENS_RES auto-response settings                                 }
  SELRES: longword;                                  { NFC-A SEL_RES auto-response settings                                  }
end;


{ ================================================================================ }
{ ================                     GPIOTE                     ================ }
{ ================================================================================ }

{
  GPIO Tasks and Events (GPIOTE)
}

NRF_GPIOTE_Type = record                             { GPIOTE Structure                                                      }
  TASKS_OUT: array [0..7] of longword;               { Description collection[0]: Task for writing to pin specified
                                                       in CONFIG[0].PSEL. Action on pin is configured in CONFIG[0].POLARITY. }
  RESERVED0: array [0..3] of longword;
  TASKS_SET: array [0..7] of longword;               { Description collection[0]: Task for writing to pin specified
                                                       in CONFIG[0].PSEL. Action on pin is to set it high.                   }
  RESERVED1: array [0..3] of longword;
  TASKS_CLR: array [0..7] of longword;               { Description collection[0]: Task for writing to pin specified
                                                       in CONFIG[0].PSEL. Action on pin is to set it low.                    }
  RESERVED2: array [0..31] of longword;
  EVENTS_IN: array [0..7] of longword;               { Description collection[0]: Event generated from pin specified
                                                       in CONFIG[0].PSEL                                                     }
  RESERVED3: array [0..22] of longword;
  EVENTS_PORT: longword;                             { Event generated from multiple input GPIO pins with SENSE mechanism
                                                       enabled                                                               }
  RESERVED4: array [0..96] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED5: array [0..128] of longword;
  CONFIG: array [0..7] of longword;                  { Description collection[0]: Configuration for OUT[n], SET[n]
                                                       and CLR[n] tasks and IN[n] event                                      }
end;


{ ================================================================================ }
{ ================                      SAADC                     ================ }
{ ================================================================================ }

{
  Analog to Digital Converter (SAADC)
}

NRF_SAADC_Type = record                              { SAADC Structure                                                       }
  TASKS_START,                                       { Start the ADC and prepare the result buffer in RAM                    }
  TASKS_SAMPLE,                                      { Take one ADC sample, if scan is enabled all channels are sampled      }
  TASKS_STOP,                                        { Stop the ADC and terminate any on-going conversion                    }
  TASKS_CALIBRATEOFFSET: longword;                   { Starts offset auto-calibration                                        }
  RESERVED0: array [0..59] of longword;
  EVENTS_STARTED,                                    { The ADC has started                                                   }
  EVENTS_END,                                        { The ADC has filled up the Result buffer                               }
  EVENTS_DONE,                                       { A conversion task has been completed. Depending on the mode,
                                                       multiple conversions might be needed for a result to be transferred
                                                       to RAM.                                                               }
  EVENTS_RESULTDONE,                                 { A result is ready to get transferred to RAM.                          }
  EVENTS_CALIBRATEDONE,                              { Calibration is complete                                               }
  EVENTS_STOPPED: longword;                          { The ADC has stopped                                                   }
  EVENTS_CH: array [0..7] of SAADC_EVENTS_CH_Type;   { Unspecified                                                           }
  RESERVED1: array [0..105] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED2: array [0..60] of longword;
  STATUS: longword;                                  { Status                                                                }
  RESERVED3: array [0..62] of longword;
  ENABLE: longword;                                  { Enable or disable ADC                                                 }
  RESERVED4: array [0..2] of longword;
  CH: array [0..7] of SAADC_CH_Type;                 { Unspecified                                                           }
  RESERVED5: array [0..23] of longword;
  RESOLUTION,                                        { Resolution configuration                                              }
  OVERSAMPLE,                                        { Oversampling configuration. OVERSAMPLE should not be combined
                                                       with SCAN. The RESOLUTION is applied before averaging, thus
                                                       for high OVERSAMPLE a higher RESOLUTION should be used.               }
  SAMPLERATE: longword;                              { Controls normal or continuous sample rate                             }
  RESERVED6: array [0..11] of longword;
  RESULT: SAADC_RESULT_Type;                         { RESULT EasyDMA channel                                                }
end;


{ ================================================================================ }
{ ================                      TIMER                     ================ }
{ ================================================================================ }

{
  Timer/Counter 0 (TIMER)
}

NRF_TIMER_Type = record                              { TIMER Structure                                                       }
  TASKS_START,                                       { Start Timer                                                           }
  TASKS_STOP,                                        { Stop Timer                                                            }
  TASKS_COUNT,                                       { Increment Timer (Counter mode only)                                   }
  TASKS_CLEAR,                                       { Clear time                                                            }
  TASKS_SHUTDOWN: longword;                          { Deprecated register - Shut down timer                                 }
  RESERVED0: array [0..10] of longword;
  TASKS_CAPTURE: array [0..5] of longword;           { Description collection[0]: Capture Timer value to CC[0] register      }
  RESERVED1: array [0..57] of longword;
  EVENTS_COMPARE: array [0..5] of longword;          { Description collection[0]: Compare event on CC[0] match               }
  RESERVED2: array [0..41] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED3: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED4: array [0..125] of longword;
  MODE,                                              { Timer mode selection                                                  }
  BITMODE,                                           { Configure the number of bits used by the TIMER                        }
  RESERVED5,
  PRESCALER: longword;                               { Timer prescaler register                                              }
  RESERVED6: array [0..10] of longword;
  CC: array [0..5] of longword;                      { Description collection[0]: Capture/Compare register 0                 }
end;


{ ================================================================================ }
{ ================                       RTC                      ================ }
{ ================================================================================ }

{
  Real time counter 0 (RTC)
}

NRF_RTC_Type = record                                { RTC Structure                                                         }
  TASKS_START,                                       { Start RTC COUNTER                                                     }
  TASKS_STOP,                                        { Stop RTC COUNTER                                                      }
  TASKS_CLEAR,                                       { Clear RTC COUNTER                                                     }
  TASKS_TRIGOVRFLW: longword;                        { Set COUNTER to 0xFFFFF0                                               }
  RESERVED0: array [0..59] of longword;
  EVENTS_TICK,                                       { Event on COUNTER increment                                            }
  EVENTS_OVRFLW: longword;                           { Event on COUNTER overflow                                             }
  RESERVED1: array [0..13] of longword;
  EVENTS_COMPARE: array [0..3] of longword;          { Description collection[0]: Compare event on CC[0] match               }
  RESERVED2: array [0..108] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..12] of longword;
  EVTEN,                                             { Enable or disable event routing                                       }
  EVTENSET,                                          { Enable event routing                                                  }
  EVTENCLR: longword;                                { Disable event routing                                                 }
  RESERVED4: array [0..109] of longword;
  COUNTER,                                           { Current COUNTER value                                                 }
  PRESCALER: longword;                               { 12 bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).Must
                                                       be written when RTC is stopped                                        }
  RESERVED5: array [0..12] of longword;
  CC: array [0..3] of longword;                      { Description collection[0]: Compare register 0                         }
end;


{ ================================================================================ }
{ ================                      TEMP                      ================ }
{ ================================================================================ }

{
  Temperature Sensor (TEMP)
}

NRF_TEMP_Type = record                               { TEMP Structure                                                        }
  TASKS_START,                                       { Start temperature measurement                                         }
  TASKS_STOP: longword;                              { Stop temperature measurement                                          }
  RESERVED0: array [0..61] of longword;
  EVENTS_DATARDY: longword;                          { Temperature measurement complete, data ready                          }
  RESERVED1: array [0..127] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED2: array [0..126] of longword;
  TEMP: longint;                                     { Temperature in degC (0.25deg steps)                                   }
  RESERVED3: array [0..4] of longword;
  A0,                                                { Slope of 1st piece wise linear function                               }
  A1,                                                { Slope of 2nd piece wise linear function                               }
  A2,                                                { Slope of 3rd piece wise linear function                               }
  A3,                                                { Slope of 4th piece wise linear function                               }
  A4,                                                { Slope of 5th piece wise linear function                               }
  A5: longword;                                      { Slope of 6th piece wise linear function                               }
  RESERVED4: array [0..1] of longword;
  B0,                                                { y-intercept of 1st piece wise linear function                         }
  B1,                                                { y-intercept of 2nd piece wise linear function                         }
  B2,                                                { y-intercept of 3rd piece wise linear function                         }
  B3,                                                { y-intercept of 4th piece wise linear function                         }
  B4,                                                { y-intercept of 5th piece wise linear function                         }
  B5: longword;                                      { y-intercept of 6th piece wise linear function                         }
  RESERVED5: array [0..1] of longword;
  T0,                                                { End point of 1st piece wise linear function                           }
  T1,                                                { End point of 2nd piece wise linear function                           }
  T2,                                                { End point of 3rd piece wise linear function                           }
  T3,                                                { End point of 4th piece wise linear function                           }
  T4: longword;                                      { End point of 5th piece wise linear function                           }
end;


{ ================================================================================ }
{ ================                       RNG                      ================ }
{ ================================================================================ }

{
  Random Number Generator (RNG)
}

NRF_RNG_Type = record                                { RNG Structure                                                         }
  TASKS_START,                                       { Task starting the random number generator                             }
  TASKS_STOP: longword;                              { Task stopping the random number generator                             }
  RESERVED0: array [0..61] of longword;
  EVENTS_VALRDY: longword;                           { Event being generated for every new random number written to
                                                       the VALUE register                                                    }
  RESERVED1: array [0..62] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED2: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..125] of longword;
  CONFIG,                                            { Configuration register                                                }
  VALUE: longword;                                   { Output random number                                                  }
end;


{ ================================================================================ }
{ ================                       ECB                      ================ }
{ ================================================================================ }

{
  AES ECB Mode Encryption (ECB)
}

NRF_ECB_Type = record                                { ECB Structure                                                         }
  TASKS_STARTECB,                                    { Start ECB block encrypt                                               }
  TASKS_STOPECB: longword;                           { Abort a possible executing ECB operation                              }
  RESERVED0: array [0..61] of longword;
  EVENTS_ENDECB,                                     { ECB block encrypt complete                                            }
  EVENTS_ERRORECB: longword;                         { ECB block encrypt aborted because of a STOPECB task or due to
                                                       an error                                                              }
  RESERVED1: array [0..126] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED2: array [0..125] of longword;
  ECBDATAPTR: longword;                              { ECB block encrypt memory pointers                                     }
end;


{ ================================================================================ }
{ ================                       CCM                      ================ }
{ ================================================================================ }

{
  AES CCM Mode Encryption (CCM)
}

NRF_CCM_Type = record                                { CCM Structure                                                         }
  TASKS_KSGEN,                                       { Start generation of key-stream. This operation will stop by
                                                       itself when completed.                                                }
  TASKS_CRYPT,                                       { Start encryption/decryption. This operation will stop by itself
                                                       when completed.                                                       }
  TASKS_STOP: longword;                              { Stop encryption/decryption                                            }
  RESERVED0: array [0..60] of longword;
  EVENTS_ENDKSGEN,                                   { Key-stream generation complete                                        }
  EVENTS_ENDCRYPT,                                   { Encrypt/decrypt complete                                              }
  EVENTS_ERROR: longword;                            { CCM error event                                                       }
  RESERVED1: array [0..60] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED2: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..60] of longword;
  MICSTATUS: longword;                               { MIC check result                                                      }
  RESERVED4: array [0..62] of longword;
  ENABLE,                                            { Enable                                                                }
  MODE,                                              { Operation mode                                                        }
  CNFPTR,                                            { Pointer to data structure holding AES key and NONCE vector            }
  INPTR,                                             { Input pointer                                                         }
  OUTPTR,                                            { Output pointer                                                        }
  SCRATCHPTR: longword;                              { Pointer to data area used for temporary storage                       }
end;


{ ================================================================================ }
{ ================                       AAR                      ================ }
{ ================================================================================ }

{
  Accelerated Address Resolver (AAR)
}

NRF_AAR_Type = record                                { AAR Structure                                                         }
  TASKS_START,                                       { Start resolving addresses based on IRKs specified in the IRK
                                                       data structure                                                        }
  RESERVED0,
  TASKS_STOP: longword;                              { Stop resolving addresses                                              }
  RESERVED1: array [0..60] of longword;
  EVENTS_END,                                        { Address resolution procedure complete                                 }
  EVENTS_RESOLVED,                                   { Address resolved                                                      }
  EVENTS_NOTRESOLVED: longword;                      { Address not resolved                                                  }
  RESERVED2: array [0..125] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..60] of longword;
  STATUS: longword;                                  { Resolution status                                                     }
  RESERVED4: array [0..62] of longword;
  ENABLE,                                            { Enable AAR                                                            }
  NIRK,                                              { Number of IRKs                                                        }
  IRKPTR,                                            { Pointer to IRK data structure                                         }
  RESERVED5,
  ADDRPTR,                                           { Pointer to the resolvable address                                     }
  SCRATCHPTR: longword;                              { Pointer to data area used for temporary storage                       }
end;


{ ================================================================================ }
{ ================                       WDT                      ================ }
{ ================================================================================ }

{
  Watchdog Timer (WDT)
}

NRF_WDT_Type = record                                { WDT Structure                                                         }
  TASKS_START: longword;                             { Start the watchdog                                                    }
  RESERVED0: array [0..62] of longword;
  EVENTS_TIMEOUT: longword;                          { Watchdog timeout                                                      }
  RESERVED1: array [0..127] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED2: array [0..60] of longword;
  RUNSTATUS,                                         { Run status                                                            }
  REQSTATUS: longword;                               { Request status                                                        }
  RESERVED3: array [0..62] of longword;
  CRV,                                               { Counter reload value                                                  }
  RREN,                                              { Enable register for reload request registers                          }
  CONFIG: longword;                                  { Configuration register                                                }
  RESERVED4: array [0..59] of longword;
  RR: array [0..7] of longword;                      { Description collection[0]: Reload request 0                           }
end;


{ ================================================================================ }
{ ================                      QDEC                      ================ }
{ ================================================================================ }

{
  Quadrature Decoder (QDEC)
}

NRF_QDEC_Type = record                               { QDEC Structure                                                        }
  TASKS_START,                                       { Task starting the quadrature decoder                                  }
  TASKS_STOP,                                        { Task stopping the quadrature decoder                                  }
  TASKS_READCLRACC,                                  { Read and clear ACC and ACCDBL                                         }
  TASKS_RDCLRACC,                                    { Read and clear ACC                                                    }
  TASKS_RDCLRDBL: longword;                          { Read and clear ACCDBL                                                 }
  RESERVED0: array [0..58] of longword;
  EVENTS_SAMPLERDY,                                  { Event being generated for every new sample value written to
                                                       the SAMPLE register                                                   }
  EVENTS_REPORTRDY,                                  { Non-null report ready                                                 }
  EVENTS_ACCOF,                                      { ACC or ACCDBL register overflow                                       }
  EVENTS_DBLRDY,                                     { Double displacement(s) detected                                       }
  EVENTS_STOPPED: longword;                          { QDEC has been stopped                                                 }
  RESERVED1: array [0..58] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED2: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..124] of longword;
  ENABLE,                                            { Enable the quadrature decoder                                         }
  LEDPOL,                                            { LED output pin polarity                                               }
  SAMPLEPER: longword;                               { Sample period                                                         }
  SAMPLE: longint;                                   { Motion sample value                                                   }
  REPORTPER: longword;                               { Number of samples to be taken before REPORTRDY and DBLRDY events
                                                       can be generated                                                      }
  ACC,                                               { Register accumulating the valid transitions                           }
  ACCREAD: longint;                                  { Snapshot of the ACC register, updated by the READCLRACC or RDCLRACC
                                                       task                                                                  }
  PSEL: QDEC_PSEL_Type;                              { Unspecified                                                           }
  DBFEN: longword;                                   { Enable input debounce filters                                         }
  RESERVED4: array [0..4] of longword;
  LEDPRE,                                            { Time period the LED is switched ON prior to sampling                  }
  ACCDBL,                                            { Register accumulating the number of detected double transitions       }
  ACCDBLREAD: longword;                              { Snapshot of the ACCDBL, updated by the READCLRACC or RDCLRDBL
                                                       task                                                                  }
end;


{ ================================================================================ }
{ ================                      COMP                      ================ }
{ ================================================================================ }

{
  Comparator (COMP)
}

NRF_COMP_Type = record                               { COMP Structure                                                        }
  TASKS_START,                                       { Start comparator                                                      }
  TASKS_STOP,                                        { Stop comparator                                                       }
  TASKS_SAMPLE: longword;                            { Sample comparator value                                               }
  RESERVED0: array [0..60] of longword;
  EVENTS_READY,                                      { COMP is ready and output is valid                                     }
  EVENTS_DOWN,                                       { Downward crossing                                                     }
  EVENTS_UP,                                         { Upward crossing                                                       }
  EVENTS_CROSS: longword;                            { Downward or upward crossing                                           }
  RESERVED1: array [0..59] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED2: array [0..62] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..60] of longword;
  RESULT: longword;                                  { Compare result                                                        }
  RESERVED4: array [0..62] of longword;
  ENABLE,                                            { COMP enable                                                           }
  PSEL,                                              { Pin select                                                            }
  REFSEL,                                            { Reference source select                                               }
  EXTREFSEL: longword;                               { External reference select                                             }
  RESERVED5: array [0..7] of longword;
  TH,                                                { Threshold configuration for hysteresis unit                           }
  MODE,                                              { Mode configuration                                                    }
  HYST,                                              { Comparator hysteresis enable                                          }
  ISOURCE: longword;                                 { Current source select on analog input                                 }
end;


{ ================================================================================ }
{ ================                     LPCOMP                     ================ }
{ ================================================================================ }

{
  Low Power Comparator (LPCOMP)
}

NRF_LPCOMP_Type = record                             { LPCOMP Structure                                                      }
  TASKS_START,                                       { Start comparator                                                      }
  TASKS_STOP,                                        { Stop comparator                                                       }
  TASKS_SAMPLE: longword;                            { Sample comparator value                                               }
  RESERVED0: array [0..60] of longword;
  EVENTS_READY,                                      { LPCOMP is ready and output is valid                                   }
  EVENTS_DOWN,                                       { Downward crossing                                                     }
  EVENTS_UP,                                         { Upward crossing                                                       }
  EVENTS_CROSS: longword;                            { Downward or upward crossing                                           }
  RESERVED1: array [0..59] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED2: array [0..63] of longword;
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..60] of longword;
  RESULT: longword;                                  { Compare result                                                        }
  RESERVED4: array [0..62] of longword;
  ENABLE,                                            { Enable LPCOMP                                                         }
  PSEL,                                              { Input pin select                                                      }
  REFSEL,                                            { Reference select                                                      }
  EXTREFSEL: longword;                               { External reference select                                             }
  RESERVED5: array [0..3] of longword;
  ANADETECT: longword;                               { Analog detect configuration                                           }
  RESERVED6: array [0..4] of longword;
  HYST: longword;                                    { Comparator hysteresis enable                                          }
end;


{ ================================================================================ }
{ ================                       SWI                      ================ }
{ ================================================================================ }

{
  Software interrupt 0 (SWI)
}

NRF_SWI_Type = record                                { SWI Structure                                                         }
  UNUSED: longword;                                  { Unused.                                                               }
end;


{ ================================================================================ }
{ ================                       EGU                      ================ }
{ ================================================================================ }

{
  Event Generator Unit 0 (EGU)
}

NRF_EGU_Type = record                                { EGU Structure                                                         }
  TASKS_TRIGGER: array [0..15] of longword;          { Description collection[0]: Trigger 0 for triggering the corresponding
                                                       TRIGGERED[0] event                                                    }
  RESERVED0: array [0..47] of longword;
  EVENTS_TRIGGERED: array [0..15] of longword;       { Description collection[0]: Event number 0 generated by triggering
                                                       the corresponding TRIGGER[0] task                                     }
  RESERVED1: array [0..111] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
end;


{ ================================================================================ }
{ ================                       PWM                      ================ }
{ ================================================================================ }

{
  Pulse Width Modulation Unit 0 (PWM)
}

NRF_PWM_Type = record                                { PWM Structure                                                         }
  RESERVED0,
  TASKS_STOP: longword;                              { Stops PWM pulse generation on all channels at the end of current
                                                       PWM period, and stops sequence playback                               }
  TASKS_SEQSTART: array [0..1] of longword;          { Description collection[0]: Loads the first PWM value on all
                                                       enabled channels from sequence 0, and starts playing that sequence
                                                       at the rate defined in SEQ[0]REFRESH and/or DECODER.MODE. Causes
                                                       PWM generation to start it was not running.                           }
  TASKS_NEXTSTEP: longword;                          { Steps by one value in the current sequence on all enabled channels
                                                       if DECODER.MODE=NextStep. Does not cause PWM generation to start
                                                       it was not running.                                                   }
  RESERVED1: array [0..59] of longword;
  EVENTS_STOPPED: longword;                          { Response to STOP task, emitted when PWM pulses are no longer
                                                       generated                                                             }
  EVENTS_SEQSTARTED: array [0..1] of longword;       { Description collection[0]: First PWM period started on sequence
                                                       0                                                                     }
  EVENTS_SEQEND: array [0..1] of longword;           { Description collection[0]: Emitted at end of every sequence
                                                       0, when last value from RAM has been applied to wave counter          }
  EVENTS_PWMPERIODEND,                               { Emitted at the end of each PWM period                                 }
  EVENTS_LOOPSDONE: longword;                        { Concatenated sequences have been played the amount of times
                                                       defined in LOOP.CNT                                                   }
  RESERVED2: array [0..55] of longword;
  SHORTS: longword;                                  { Shortcut register                                                     }
  RESERVED3: array [0..62] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED4: array [0..124] of longword;
  ENABLE,                                            { PWM module enable register                                            }
  MODE,                                              { Selects operating mode of the wave counter                            }
  COUNTERTOP,                                        { Value up to which the pulse generator counter counts                  }
  PRESCALER,                                         { Configuration for PWM_CLK                                             }
  DECODER,                                           { Configuration of the decoder                                          }
  LOOP: longword;                                    { Amount of playback of a loop                                          }
  RESERVED5: array [0..1] of longword;
  SEQ: array [0..1] of PWM_SEQ_Type;                 { Unspecified                                                           }
  PSEL: PWM_PSEL_Type;                               { Unspecified                                                           }
end;


{ ================================================================================ }
{ ================                       PDM                      ================ }
{ ================================================================================ }

{
  Pulse Density Modulation (Digital Microphone) Interface (PDM)
}

NRF_PDM_Type = record                                { PDM Structure                                                         }
  TASKS_START,                                       { Starts continuous PDM transfer                                        }
  TASKS_STOP: longword;                              { Stops PDM transfer                                                    }
  RESERVED0: array [0..61] of longword;
  EVENTS_STARTED,                                    { PDM transfer has started                                              }
  EVENTS_STOPPED,                                    { PDM transfer has finished                                             }
  EVENTS_END: longword;                              { The PDM has written the last sample specified by SAMPLE.MAXCNT
                                                       (or the last sample after a STOP task has been received) to
                                                       Data RAM                                                              }
  RESERVED1: array [0..124] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED2: array [0..124] of longword;
  ENABLE,                                            { PDM module enable register                                            }
  PDMCLKCTRL,                                        { PDM clock generator control                                           }
  MODE: longword;                                    { Defines the routing of the connected PDM microphones' signals         }
  RESERVED3: array [0..2] of longword;
  GAINL,                                             { Left output gain adjustment                                           }
  GAINR: longword;                                   { Right output gain adjustment                                          }
  RESERVED4: array [0..7] of longword;
  PSEL: PDM_PSEL_Type;                               { Unspecified                                                           }
  RESERVED5: array [0..5] of longword;
  SAMPLE: PDM_SAMPLE_Type;                           { Unspecified                                                           }
end;


{ ================================================================================ }
{ ================                      NVMC                      ================ }
{ ================================================================================ }

{
  Non Volatile Memory Controller (NVMC)
}

NRF_NVMC_Type = record                               { NVMC Structure                                                        }
  RESERVED0: array [0..255] of longword;
  READY: longword;                                   { Ready flag                                                            }
  RESERVED1: array [0..63] of longword;
  CONFIG: longword;                                  { Configuration register                                                }


  ERASEPAGE: longword;                               { Register for erasing a page in Code area                              }
  ERASEALL,                                          { Register for erasing all non-volatile user memory                     }
  ERASEPCR0,                                         { Deprecated register - Register for erasing a page in Code area.
                                                       Equivalent to ERASEPAGE.                                              }
  ERASEUICR: longword;                               { Register for erasing User Information Configuration Registers         }
  RESERVED2: array [0..9] of longword;
  ICACHECNF,                                         { I-Code cache configuration register.                                  }
  RESERVED3,
  IHIT,                                              { I-Code cache hit counter.                                             }
  IMISS: longword;                                   { I-Code cache miss counter.                                            }
end;


{ ================================================================================ }
{ ================                       PPI                      ================ }
{ ================================================================================ }

{
  Programmable Peripheral Interconnect (PPI)
}

NRF_PPI_Type = record                                { PPI Structure                                                         }
  TASKS_CHG: array [0..5] of PPI_TASKS_CHG_Type;     { Channel group tasks                                                   }
  RESERVED0: array [0..307] of longword;
  CHEN,                                              { Channel enable register                                               }
  CHENSET,                                           { Channel enable set register                                           }
  CHENCLR,                                           { Channel enable clear register                                         }
  RESERVED1: longint;
  CH: array [0..19] of PPI_CH_Type;                  { PPI Channel                                                           }
  RESERVED2: array [0..147] of longword;
  CHG: array [0..5] of longword;                     { Description collection[0]: Channel group 0                            }
  RESERVED3: array [0..61] of longword;
  FORK: array [0..31] of PPI_FORK_Type;              { Fork                                                                  }
end;


{ ================================================================================ }
{ ================                       MWU                      ================ }
{ ================================================================================ }

{
  Memory Watch Unit (MWU)
}

NRF_MWU_Type = record                                { MWU Structure                                                         }
  RESERVED0: array [0..63] of longword;
  EVENTS_REGION: array [0..3] of MWU_EVENTS_REGION_Type;   { Unspecified                                                     }
  RESERVED1: array [0..15] of longword;
  EVENTS_PREGION: array [0..1] of MWU_EVENTS_PREGION_Type; { Unspecified                                                     }
  RESERVED2: array [0..99] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..4] of longword;
  NMIEN,                                             { Enable or disable non-maskable interrupt                              }
  NMIENSET,                                          { Enable non-maskable interrupt                                         }
  NMIENCLR: longword;                                { Disable non-maskable interrupt                                        }
  RESERVED4: array [0..52] of longword;
  PERREGION: array [0..1] of MWU_PERREGION_Type;     { Unspecified                                                           }
  RESERVED5: array [0..63] of longword;
  REGIONEN,                                          { Enable/disable regions watch                                          }
  REGIONENSET,                                       { Enable regions watch                                                  }
  REGIONENCLR: longword;                             { Disable regions watch                                                 }
  RESERVED6: array [0..56] of longword;
  REGION: array [0..3] of MWU_REGION_Type;           { Unspecified                                                           }
  RESERVED7: array [0..31] of longword;
  PREGION: array [0..1] of MWU_PREGION_Type;         { Unspecified                                                           }
end;


{ ================================================================================ }
{ ================                       I2S                      ================ }
{ ================================================================================ }

{
  Inter-IC Sound (I2S)
}

NRF_I2S_Type = record                                { I2S Structure                                                         }
  TASKS_START,                                       { Starts continuous I<sup>2</sup>S transfer. Also starts MCK generator
                                                       when this is enabled.                                                 }
  TASKS_STOP: longword;                              { Stops I<sup>2</sup>S transfer. Also stops MCK generator. Triggering
                                                       this task will cause the STOPPED event to be generated.               }
  RESERVED0: array [0..62] of longword;
  EVENTS_RXPTRUPD,                                   { The RXD.PTR register has been copied to internal double-buffers.
                                                       When the I2S module is started and RX is enabled, this event
                                                       will be generated for every RXTXD.MAXCNT words that are received
                                                       on the SDIN pin.                                                      }
  EVENTS_STOPPED: longword;                          { I<sup>2</sup>S transfer stopped.                                      }
  RESERVED1: array [0..1] of longword;
  EVENTS_TXPTRUPD: longword;                         { The TDX.PTR register has been copied to internal double-buffers.
                                                       When the I2S module is started and TX is enabled, this event
                                                       will be generated for every RXTXD.MAXCNT words that are sent
                                                       on the SDOUT pin.                                                     }
  RESERVED2: array [0..121] of longword;
  INTEN,                                             { Enable or disable interrupt                                           }
  INTENSET,                                          { Enable interrupt                                                      }
  INTENCLR: longword;                                { Disable interrupt                                                     }
  RESERVED3: array [0..124] of longword;
  ENABLE: longword;                                  { Enable I<sup>2</sup>S module.                                         }
  CONFIG: I2S_CONFIG_Type;                           { Unspecified                                                           }
  RESERVED4: array [0..2] of longword;
  RXD: I2S_RXD_Type;                                 { Unspecified                                                           }
  RESERVED5,
  TXD: I2S_TXD_Type;                                 { Unspecified                                                           }
  RESERVED6: array [0..2] of longword;
  RXTXD: I2S_RXTXD_Type;                             { Unspecified                                                           }
  RESERVED7: array [0..2] of longword;
  PSEL: I2S_PSEL_Type;                               { Unspecified                                                           }
end;


{ ================================================================================ }
{ ================                       FPU                      ================ }
{ ================================================================================ }

{
  FPU (FPU)
}

NRF_FPU_Type = record                               { FPU Structure                                                          }
  UNUSED: longword;                                 { Unused.                                                                }
end;


{ ================================================================================ }
{ ================                      GPIO                      ================ }
{ ================================================================================ }

{
  GPIO Port 1 (GPIO)
}

NRF_GPIO_Type = record                               { GPIO Structure                                                        }
  RESERVED0: array [0..320] of longword;
  OUT,                                               { Write GPIO port                                                       }
  OUTSET,                                            { Set individual bits in GPIO port                                      }
  OUTCLR,                                            { Clear individual bits in GPIO port                                    }
  IN_,                                               { Read GPIO port                                                        }
  DIR,                                               { Direction of GPIO pins                                                }
  DIRSET,                                            { DIR set register                                                      }
  DIRCLR,                                            { DIR clear register                                                    }
  LATCH,                                             { Latch register indicating what GPIO pins that have met the criteria
                                                       set in the PIN_CNF[n].SENSE registers                                 }
  DETECTMODE: longword;                              { Select between default DETECT signal behaviour and LDETECT mode       }
  RESERVED1: array [0..117] of longword;
  PIN_CNF: array [0..31] of longword;                { Description collection[0]: Configuration of GPIO pins                 }
end;


{ ================================================================================ }
{ ================              Peripheral memory map             ================ }
{ ================================================================================ }

const
  NRF_FICR_BASE                 = $10000000;
  NRF_UICR_BASE                 = $10001000;
  NRF_BPROT_BASE                = $40000000;
  NRF_POWER_BASE                = $40000000;
  NRF_CLOCK_BASE                = $40000000;
  NRF_AMLI_BASE                 = $40000000;
  NRF_RADIO_BASE                = $40001000;
  NRF_UARTE0_BASE               = $40002000;
  NRF_UART0_BASE                = $40002000;
  NRF_SPIM0_BASE                = $40003000;
  NRF_SPIS0_BASE                = $40003000;
  NRF_TWIM0_BASE                = $40003000;
  NRF_TWIS0_BASE                = $40003000;
  NRF_SPI0_BASE                 = $40003000;
  NRF_TWI0_BASE                 = $40003000;
  NRF_SPIM1_BASE                = $40004000;
  NRF_SPIS1_BASE                = $40004000;
  NRF_TWIM1_BASE                = $40004000;
  NRF_TWIS1_BASE                = $40004000;
  NRF_SPI1_BASE                 = $40004000;
  NRF_TWI1_BASE                 = $40004000;
  NRF_NFCT_BASE                 = $40005000;
  NRF_GPIOTE_BASE               = $40006000;
  NRF_SAADC_BASE                = $40007000;
  NRF_TIMER0_BASE               = $40008000;
  NRF_TIMER1_BASE               = $40009000;
  NRF_TIMER2_BASE               = $4000A000;
  NRF_RTC0_BASE                 = $4000B000;
  NRF_TEMP_BASE                 = $4000C000;
  NRF_RNG_BASE                  = $4000D000;
  NRF_ECB_BASE                  = $4000E000;
  NRF_CCM_BASE                  = $4000F000;
  NRF_AAR_BASE                  = $4000F000;
  NRF_WDT_BASE                  = $40010000;
  NRF_RTC1_BASE                 = $40011000;
  NRF_QDEC_BASE                 = $40012000;
  NRF_COMP_BASE                 = $40013000;
  NRF_LPCOMP_BASE               = $40013000;
  NRF_SWI0_BASE                 = $40014000;
  NRF_EGU0_BASE                 = $40014000;
  NRF_SWI1_BASE                 = $40015000;
  NRF_EGU1_BASE                 = $40015000;
  NRF_SWI2_BASE                 = $40016000;
  NRF_EGU2_BASE                 = $40016000;
  NRF_SWI3_BASE                 = $40017000;
  NRF_EGU3_BASE                 = $40017000;
  NRF_SWI4_BASE                 = $40018000;
  NRF_EGU4_BASE                 = $40018000;
  NRF_SWI5_BASE                 = $40019000;
  NRF_EGU5_BASE                 = $40019000;
  NRF_TIMER3_BASE               = $4001A000;
  NRF_TIMER4_BASE               = $4001B000;
  NRF_PWM0_BASE                 = $4001C000;
  NRF_PDM_BASE                  = $4001D000;
  NRF_NVMC_BASE                 = $4001E000;
  NRF_PPI_BASE                  = $4001F000;
  NRF_MWU_BASE                  = $40020000;
  NRF_PWM1_BASE                 = $40021000;
  NRF_PWM2_BASE                 = $40022000;
  NRF_SPIM2_BASE                = $40023000;
  NRF_SPIS2_BASE                = $40023000;
  NRF_SPI2_BASE                 = $40023000;
  NRF_RTC2_BASE                 = $40024000;
  NRF_I2S_BASE                  = $40025000;
  NRF_FPU_BASE                  = $40026000;
  NRF_P0_BASE                   = $50000000;


{ ================================================================================ }
{ ================             Peripheral declaration             ================ }
{ ================================================================================ }

var
  NRF_FICR  : NRF_FICR_Type   absolute NRF_FICR_BASE;
  NRF_UICR  : NRF_UICR_Type   absolute NRF_UICR_BASE;
  NRF_BPROT : NRF_BPROT_Type  absolute NRF_BPROT_BASE;
  NRF_POWER : NRF_POWER_Type  absolute NRF_POWER_BASE;
  NRF_CLOCK : NRF_CLOCK_Type  absolute NRF_CLOCK_BASE;
  NRF_AMLI  : NRF_AMLI_Type   absolute NRF_AMLI_BASE;
  NRF_RADIO : NRF_RADIO_Type  absolute NRF_RADIO_BASE;
  NRF_UARTE0: NRF_UARTE_Type  absolute NRF_UARTE0_BASE;
  NRF_UART0 : NRF_UART_Type   absolute NRF_UART0_BASE;
  NRF_SPIM0 : NRF_SPIM_Type   absolute NRF_SPIM0_BASE;
  NRF_SPIS0 : NRF_SPIS_Type   absolute NRF_SPIS0_BASE;
  NRF_TWIM0 : NRF_TWIM_Type   absolute NRF_TWIM0_BASE;
  NRF_TWIS0 : NRF_TWIS_Type   absolute NRF_TWIS0_BASE;
  NRF_SPI0  : NRF_SPI_Type    absolute NRF_SPI0_BASE;
  NRF_TWI0  : NRF_TWI_Type    absolute NRF_TWI0_BASE;
  NRF_SPIM1 : NRF_SPIM_Type   absolute NRF_SPIM1_BASE;
  NRF_SPIS1 : NRF_SPIS_Type   absolute NRF_SPIS1_BASE;
  NRF_TWIM1 : NRF_TWIM_Type   absolute NRF_TWIM1_BASE;
  NRF_TWIS1 : NRF_TWIS_Type   absolute NRF_TWIS1_BASE;
  NRF_SPI1  : NRF_SPI_Type    absolute NRF_SPI1_BASE;
  NRF_TWI1  : NRF_TWI_Type    absolute NRF_TWI1_BASE;
  NRF_NFCT  : NRF_NFCT_Type   absolute NRF_NFCT_BASE;
  NRF_GPIOTE: NRF_GPIOTE_Type absolute NRF_GPIOTE_BASE;
  NRF_SAADC : NRF_SAADC_Type  absolute NRF_SAADC_BASE;
  NRF_TIMER0: NRF_TIMER_Type  absolute NRF_TIMER0_BASE;
  NRF_TIMER1: NRF_TIMER_Type  absolute NRF_TIMER1_BASE;
  NRF_TIMER2: NRF_TIMER_Type  absolute NRF_TIMER2_BASE;
  NRF_RTC0  : NRF_RTC_Type    absolute NRF_RTC0_BASE;
  NRF_TEMP  : NRF_TEMP_Type   absolute NRF_TEMP_BASE;
  NRF_RNG   : NRF_RNG_Type    absolute NRF_RNG_BASE;
  NRF_ECB   : NRF_ECB_Type    absolute NRF_ECB_BASE;
  NRF_CCM   : NRF_CCM_Type    absolute NRF_CCM_BASE;
  NRF_AAR   : NRF_AAR_Type    absolute NRF_AAR_BASE;
  NRF_WDT   : NRF_WDT_Type    absolute NRF_WDT_BASE;
  NRF_RTC1  : NRF_RTC_Type    absolute NRF_RTC1_BASE;
  NRF_QDEC  : NRF_QDEC_Type   absolute NRF_QDEC_BASE;
  NRF_COMP  : NRF_COMP_Type   absolute NRF_COMP_BASE;
  NRF_LPCOMP: NRF_LPCOMP_Type absolute NRF_LPCOMP_BASE;
  NRF_SWI0  : NRF_SWI_Type    absolute NRF_SWI0_BASE;
  NRF_EGU0  : NRF_EGU_Type    absolute NRF_EGU0_BASE;
  NRF_SWI1  : NRF_SWI_Type    absolute NRF_SWI1_BASE;
  NRF_EGU1  : NRF_EGU_Type    absolute NRF_EGU1_BASE;
  NRF_SWI2  : NRF_SWI_Type    absolute NRF_SWI2_BASE;
  NRF_EGU2  : NRF_EGU_Type    absolute NRF_EGU2_BASE;
  NRF_SWI3  : NRF_SWI_Type    absolute NRF_SWI3_BASE;
  NRF_EGU3  : NRF_EGU_Type    absolute NRF_EGU3_BASE;
  NRF_SWI4  : NRF_SWI_Type    absolute NRF_SWI4_BASE;
  NRF_EGU4  : NRF_EGU_Type    absolute NRF_EGU4_BASE;
  NRF_SWI5  : NRF_SWI_Type    absolute NRF_SWI5_BASE;
  NRF_EGU5  : NRF_EGU_Type    absolute NRF_EGU5_BASE;
  NRF_TIMER3: NRF_TIMER_Type  absolute NRF_TIMER3_BASE;
  NRF_TIMER4: NRF_TIMER_Type  absolute NRF_TIMER4_BASE;
  NRF_PWM0  : NRF_PWM_Type    absolute NRF_PWM0_BASE;
  NRF_PDM   : NRF_PDM_Type    absolute NRF_PDM_BASE;
  NRF_NVMC  : NRF_NVMC_Type   absolute NRF_NVMC_BASE;
  NRF_PPI   : NRF_PPI_Type    absolute NRF_PPI_BASE;
  NRF_MWU   : NRF_MWU_Type    absolute NRF_MWU_BASE;
  NRF_PWM1  : NRF_PWM_Type    absolute NRF_PWM1_BASE;
  NRF_PWM2  : NRF_PWM_Type    absolute NRF_PWM2_BASE;
  NRF_SPIM2 : NRF_SPIM_Type   absolute NRF_SPIM2_BASE;
  NRF_SPIS2 : NRF_SPIS_Type   absolute NRF_SPIS2_BASE;
  NRF_SPI2  : NRF_SPI_Type    absolute NRF_SPI2_BASE;
  NRF_RTC2  : NRF_RTC_Type    absolute NRF_RTC2_BASE;
  NRF_I2S   : NRF_I2S_Type    absolute NRF_I2S_BASE;
  NRF_FPU   : NRF_FPU_Type    absolute NRF_FPU_BASE;
  NRF_P0    : NRF_GPIO_Type   absolute NRF_P0_BASE;


implementation

procedure NMI_Handler; external name 'NMI_Handler';
procedure HardFault_Handler; external name 'HardFault_Handler';
procedure MemoryManagement_Handler; external name 'MemoryManagement_Handler';
procedure BusFault_Handler; external name 'BusFault_Handler';
procedure UsageFault_Handler; external name 'UsageFault_Handler';
procedure SVC_Handler; external name 'SVC_Handler';
procedure DebugMonitor_Handler; external name 'DebugMonitor_Handler';
procedure PendSV_Handler; external name 'PendSV_Handler';
procedure SysTick_Handler; external name 'SysTick_Handler';

procedure POWER_CLOCK_IRQHandler; external name 'POWER_CLOCK_IRQHandler';
procedure RADIO_IRQHandler; external name 'RADIO_IRQHandler';
procedure UARTE0_UART0_IRQHandler; external name 'UARTE0_UART0_IRQHandler';
procedure SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_IRQHandler; external name 'SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_IRQHandler';
procedure SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_IRQHandler; external name 'SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_IRQHandler';
procedure NFCT_IRQHandler; external name 'NFCT_IRQHandler';
procedure GPIOTE_IRQHandler; external name 'GPIOTE_IRQHandler';
procedure SAADC_IRQHandler; external name 'SAADC_IRQHandler';
procedure TIMER0_IRQHandler; external name 'TIMER0_IRQHandler';
procedure TIMER1_IRQHandler; external name 'TIMER1_IRQHandler';
procedure TIMER2_IRQHandler; external name 'TIMER2_IRQHandler';
procedure RTC0_IRQHandler; external name 'RTC0_IRQHandler';
procedure TEMP_IRQHandler; external name 'TEMP_IRQHandler';
procedure RNG_IRQHandler; external name 'RNG_IRQHandler';
procedure ECB_IRQHandler; external name 'ECB_IRQHandler';
procedure CCM_AAR_IRQHandler; external name 'CCM_AAR_IRQHandler';
procedure WDT_IRQHandler; external name 'WDT_IRQHandler';
procedure RTC1_IRQHandler; external name 'RTC1_IRQHandler';
procedure QDEC_IRQHandler; external name 'QDEC_IRQHandler';
procedure COMP_LPCOMP_IRQHandler; external name 'COMP_LPCOMP_IRQHandler';
procedure SWI0_EGU0_IRQHandler; external name 'SWI0_EGU0_IRQHandler';
procedure SWI1_EGU1_IRQHandler; external name 'SWI1_EGU1_IRQHandler';
procedure SWI2_EGU2_IRQHandler; external name 'SWI2_EGU2_IRQHandler';
procedure SWI3_EGU3_IRQHandler; external name 'SWI3_EGU3_IRQHandler';
procedure SWI4_EGU4_IRQHandler; external name 'SWI4_EGU4_IRQHandler';
procedure SWI5_EGU5_IRQHandler; external name 'SWI5_EGU5_IRQHandler';
procedure TIMER3_IRQHandler; external name 'TIMER3_IRQHandler';
procedure TIMER4_IRQHandler; external name 'TIMER4_IRQHandler';
procedure PWM0_IRQHandler; external name 'PWM0_IRQHandler';
procedure PDM_IRQHandler; external name 'PDM_IRQHandler';
procedure MWU_IRQHandler; external name 'MWU_IRQHandler';
procedure PWM1_IRQHandler; external name 'PWM1_IRQHandler';
procedure PWM2_IRQHandler; external name 'PWM2_IRQHandler';
procedure SPIM2_SPIS2_SPI2_IRQHandler; external name 'SPIM2_SPIS2_SPI2_IRQHandler';
procedure RTC2_IRQHandler; external name 'RTC2_IRQHandler';
procedure I2S_IRQHandler; external name 'I2S_IRQHandler';


{$i cortexm4f_start.inc}


procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long   _stack_top
  .long   Startup
  .long   NMI_Handler
  .long   HardFault_Handler
  .long   MemoryManagement_Handler
  .long   BusFault_Handler
  .long   UsageFault_Handler
  .long   0
  .long   0
  .long   0
  .long   0
  .long   SVC_Handler
  .long   DebugMonitor_Handler
  .long   0
  .long   PendSV_Handler
  .long   SysTick_Handler

  .long   POWER_CLOCK_IRQHandler
  .long   RADIO_IRQHandler
  .long   UARTE0_UART0_IRQHandler
  .long   SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_IRQHandler
  .long   SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_IRQHandler
  .long   NFCT_IRQHandler
  .long   GPIOTE_IRQHandler
  .long   SAADC_IRQHandler
  .long   TIMER0_IRQHandler
  .long   TIMER1_IRQHandler
  .long   TIMER2_IRQHandler
  .long   RTC0_IRQHandler
  .long   TEMP_IRQHandler
  .long   RNG_IRQHandler
  .long   ECB_IRQHandler
  .long   CCM_AAR_IRQHandler
  .long   WDT_IRQHandler
  .long   RTC1_IRQHandler
  .long   QDEC_IRQHandler
  .long   COMP_LPCOMP_IRQHandler
  .long   SWI0_EGU0_IRQHandler
  .long   SWI1_EGU1_IRQHandler
  .long   SWI2_EGU2_IRQHandler
  .long   SWI3_EGU3_IRQHandler
  .long   SWI4_EGU4_IRQHandler
  .long   SWI5_EGU5_IRQHandler
  .long   TIMER3_IRQHandler
  .long   TIMER4_IRQHandler
  .long   PWM0_IRQHandler
  .long   PDM_IRQHandler
  .long   0
  .long   0
  .long   MWU_IRQHandler
  .long   PWM1_IRQHandler
  .long   PWM2_IRQHandler
  .long   SPIM2_SPIS2_SPI2_IRQHandler
  .long   RTC2_IRQHandler
  .long   I2S_IRQHandler
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0
  .long   0

  .weak   NMI_Handler
  .weak   HardFault_Handler
  .weak   MemoryManagement_Handler
  .weak   BusFault_Handler
  .weak   UsageFault_Handler
  .weak   SVC_Handler
  .weak   DebugMonitor_Handler
  .weak   PendSV_Handler
  .weak   SysTick_Handler

  .weak   POWER_CLOCK_IRQHandler
  .weak   RADIO_IRQHandler
  .weak   UARTE0_UART0_IRQHandler
  .weak   SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_IRQHandler
  .weak   SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_IRQHandler
  .weak   NFCT_IRQHandler
  .weak   GPIOTE_IRQHandler
  .weak   SAADC_IRQHandler
  .weak   TIMER0_IRQHandler
  .weak   TIMER1_IRQHandler
  .weak   TIMER2_IRQHandler
  .weak   RTC0_IRQHandler
  .weak   TEMP_IRQHandler
  .weak   RNG_IRQHandler
  .weak   ECB_IRQHandler
  .weak   CCM_AAR_IRQHandler
  .weak   WDT_IRQHandler
  .weak   RTC1_IRQHandler
  .weak   QDEC_IRQHandler
  .weak   COMP_LPCOMP_IRQHandler
  .weak   SWI0_EGU0_IRQHandler
  .weak   SWI1_EGU1_IRQHandler
  .weak   SWI2_EGU2_IRQHandler
  .weak   SWI3_EGU3_IRQHandler
  .weak   SWI4_EGU4_IRQHandler
  .weak   SWI5_EGU5_IRQHandler
  .weak   TIMER3_IRQHandler
  .weak   TIMER4_IRQHandler
  .weak   PWM0_IRQHandler
  .weak   PDM_IRQHandler
  .weak   MWU_IRQHandler
  .weak   PWM1_IRQHandler
  .weak   PWM2_IRQHandler
  .weak   SPIM2_SPIS2_SPI2_IRQHandler
  .weak   RTC2_IRQHandler
  .weak   I2S_IRQHandler

  .set   NMI_Handler, HaltProc
  .set   HardFault_Handler, HaltProc
  .set   MemoryManagement_Handler, HaltProc
  .set   BusFault_Handler, HaltProc
  .set   UsageFault_Handler, HaltProc
  .set   SVC_Handler, HaltProc
  .set   DebugMonitor_Handler, HaltProc
  .set   PendSV_Handler, HaltProc
  .set   SysTick_Handler, HaltProc

  .set   POWER_CLOCK_IRQHandler, HaltProc
  .set   RADIO_IRQHandler, HaltProc
  .set   UARTE0_UART0_IRQHandler, HaltProc
  .set   SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_IRQHandler, HaltProc
  .set   SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_IRQHandler, HaltProc
  .set   NFCT_IRQHandler, HaltProc
  .set   GPIOTE_IRQHandler, HaltProc
  .set   SAADC_IRQHandler, HaltProc
  .set   TIMER0_IRQHandler, HaltProc
  .set   TIMER1_IRQHandler, HaltProc
  .set   TIMER2_IRQHandler, HaltProc
  .set   RTC0_IRQHandler, HaltProc
  .set   TEMP_IRQHandler, HaltProc
  .set   RNG_IRQHandler, HaltProc
  .set   ECB_IRQHandler, HaltProc
  .set   CCM_AAR_IRQHandler, HaltProc
  .set   WDT_IRQHandler, HaltProc
  .set   RTC1_IRQHandler, HaltProc
  .set   QDEC_IRQHandler, HaltProc
  .set   COMP_LPCOMP_IRQHandler, HaltProc
  .set   SWI0_EGU0_IRQHandler, HaltProc
  .set   SWI1_EGU1_IRQHandler, HaltProc
  .set   SWI2_EGU2_IRQHandler, HaltProc
  .set   SWI3_EGU3_IRQHandler, HaltProc
  .set   SWI4_EGU4_IRQHandler, HaltProc
  .set   SWI5_EGU5_IRQHandler, HaltProc
  .set   TIMER3_IRQHandler, HaltProc
  .set   TIMER4_IRQHandler, HaltProc
  .set   PWM0_IRQHandler, HaltProc
  .set   PDM_IRQHandler, HaltProc
  .set   MWU_IRQHandler, HaltProc
  .set   PWM1_IRQHandler, HaltProc
  .set   PWM2_IRQHandler, HaltProc
  .set   SPIM2_SPIS2_SPI2_IRQHandler, HaltProc
  .set   RTC2_IRQHandler, HaltProc
  .set   I2S_IRQHandler, HaltProc
  .text
end;
end.

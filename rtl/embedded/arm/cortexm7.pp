{
 System register definitions and utility code for Cortex-M7

 Created by Jeppe Johansen 2015 - jeppe@j-software.dk
}
unit cortexm7;

interface

{$PACKRECORDS C}
type
  NVIC_Type = record
    ISER: array [0..7] of longword;  (*!< Offset: 0x000 (R/W)  Interrupt Set Enable Register            *)
    RESERVED0: array [0..23] of longword;
    ICER: array [0..7] of longword;  (*!< Offset: 0x080 (R/W)  Interrupt Clear Enable Register          *)
    RSERVED1: array [0..23] of longword;
    ISPR: array [0..7] of longword;  (*!< Offset: 0x100 (R/W)  Interrupt Set Pending Register           *)
    RESERVED2: array [0..23] of longword;
    ICPR: array [0..7] of longword;  (*!< Offset: 0x180 (R/W)  Interrupt Clear Pending Register         *)
    RESERVED3: array [0..23] of longword;
    IABR: array [0..7] of longword;  (*!< Offset: 0x200 (R/W)  Interrupt Active bit Register            *)
    RESERVED4: array [0..55] of longword;
    IP: array [0..239] of byte;  (*!< Offset: 0x300 (R/W)  Interrupt Priority Register (8Bit wide)  *)
    RESERVED5: array [0..643] of longword;
    STIR: longword;  (*!< Offset: 0xE00 ( /W)  Software Trigger Interrupt Register      *)
  end;

  SCB_Type = record
    CPUID: longword;   (*!< Offset: 0x000 (R/ )  CPUID Base Register                                    *)
    ICSR: longword;    (*!< Offset: 0x004 (R/W)  Interrupt Control and State Register                   *)
    VTOR: longword;    (*!< Offset: 0x008 (R/W)  Vector Table Offset Register                           *)
    AIRCR: longword;   (*!< Offset: 0x00C (R/W)  Application Interrupt and Reset Control Register       *)
    SCR: longword;     (*!< Offset: 0x010 (R/W)  System Control Register                                *)
    CCR: longword;     (*!< Offset: 0x014 (R/W)  Configuration Control Register                         *)
    SHPR: array [0..11] of byte;  (*!< Offset: 0x018 (R/W)  System Handlers Priority Registers (4-7, 8-11, 12-15)  *)
    SHCSR: longword;   (*!< Offset: 0x024 (R/W)  System Handler Control and State Register              *)
    CFSR: longword;    (*!< Offset: 0x028 (R/W)  Configurable Fault Status Register                     *)
    HFSR: longword;    (*!< Offset: 0x02C (R/W)  HardFault Status Register                              *)
    DFSR: longword;    (*!< Offset: 0x030 (R/W)  Debug Fault Status Register                            *)
    MMFAR: longword;   (*!< Offset: 0x034 (R/W)  MemManage Fault Address Register                       *)
    BFAR: longword;    (*!< Offset: 0x038 (R/W)  BusFault Address Register                              *)
    AFSR: longword;    (*!< Offset: 0x03C (R/W)  Auxiliary Fault Status Register                        *)
    ID_PFR: array [0..1] of longword;  (*!< Offset: 0x040 (R/ )  Processor Feature Register                             *)
    ID_DFR: longword;  (*!< Offset: 0x048 (R/ )  Debug Feature Register                                 *)
    ID_AFR: longword;  (*!< Offset: 0x04C (R/ )  Auxiliary Feature Register                             *)
    ID_MFR: array [0..3] of longword;  (*!< Offset: 0x050 (R/ )  Memory Model Feature Register                          *)
    ID_ISAR: array [0..4] of longword;  (*!< Offset: 0x060 (R/ )  Instruction Set Attributes Register                    *)
    RESERVED0: array [0..0] of longword;
    CLIDR: longword;   (*!< Offset: 0x078 (R/ )  Cache Level ID register                                *)
    CTR: longword;     (*!< Offset: 0x07C (R/ )  Cache Type register                                    *)
    CCSIDR: longword;  (*!< Offset: 0x080 (R/ )  Cache Size ID Register                                 *)
    CSSELR: longword;  (*!< Offset: 0x084 (R/W)  Cache Size Selection Register                          *)
    CPACR: longword;   (*!< Offset: 0x088 (R/W)  Coprocessor Access Control Register                    *)
    RESERVED3: array [0..92] of longword;
    STIR: longword;  (*!< Offset: 0x200 ( /W)  Software Triggered Interrupt Register                  *)
    RESERVED4: array [0..14] of longword;
    MVFR0: longword;  (*!< Offset: 0x240 (R/ )  Media and VFP Feature Register 0                       *)
    MVFR1: longword;  (*!< Offset: 0x244 (R/ )  Media and VFP Feature Register 1                       *)
    MVFR2: longword;  (*!< Offset: 0x248 (R/ )  Media and VFP Feature Register 1                       *)
    RESERVED5: array [0..0] of longword;
    ICIALLU: longword;  (*!< Offset: 0x250 ( /W)  I-Cache Invalidate All to PoU                          *)
    RESERVED6: array [0..0] of longword;
    ICIMVAU: longword;   (*!< Offset: 0x258 ( /W)  I-Cache Invalidate by MVA to PoU                       *)
    DCIMVAC: longword;   (*!< Offset: 0x25C ( /W)  D-Cache Invalidate by MVA to PoC                       *)
    DCISW: longword;     (*!< Offset: 0x260 ( /W)  D-Cache Invalidate by Set-way                          *)
    DCCMVAU: longword;   (*!< Offset: 0x264 ( /W)  D-Cache Clean by MVA to PoU                            *)
    DCCMVAC: longword;   (*!< Offset: 0x268 ( /W)  D-Cache Clean by MVA to PoC                            *)
    DCCSW: longword;     (*!< Offset: 0x26C ( /W)  D-Cache Clean by Set-way                               *)
    DCCIMVAC: longword;  (*!< Offset: 0x270 ( /W)  D-Cache Clean and Invalidate by MVA to PoC             *)
    DCCISW: longword;    (*!< Offset: 0x274 ( /W)  D-Cache Clean and Invalidate by Set-way                *)
    RESERVED7: array [0..5] of longword;
    ITCMCR: longword;  (*!< Offset: 0x290 (R/W)  Instruction Tightly-Coupled Memory Control Register    *)
    DTCMCR: longword;  (*!< Offset: 0x294 (R/W)  Data Tightly-Coupled Memory Control Registers          *)
    AHBPCR: longword;  (*!< Offset: 0x298 (R/W)  AHBP Control Register                                  *)
    CACR: longword;    (*!< Offset: 0x29C (R/W)  L1 Cache Control Register                              *)
    AHBSCR: longword;  (*!< Offset: 0x2A0 (R/W)  AHB Slave Control Register                             *)
    RESERVED8: array [0..0] of longword;
    ABFSR: longword;  (*!< Offset: 0x2A8 (R/W)  Auxiliary Bus Fault Status Register                    *)
  end;

  SCnSCB_Type = record
    RESERVED0: array [0..0] of longword;
    ICTR: longword;   (*!< Offset: 0x004 (R/ )  Interrupt Controller Type Register       *)
    ACTLR: longword;  (*!< Offset: 0x008 (R/W)  Auxiliary Control Register               *)
  end;

  SysTick_Type = record
    CTRL: longword;   (*!< Offset: 0x000 (R/W)  SysTick Control and Status Register  *)
    LOAD: longword;   (*!< Offset: 0x004 (R/W)  SysTick Reload Value Register        *)
    VAL: longword;    (*!< Offset: 0x008 (R/W)  SysTick Current Value Register       *)
    CALIB: longword;  (*!< Offset: 0x00C (R/ )  SysTick Calibration Register         *)
  end;

  ITM_Type = record
    PORT: array [0..31] of record
      case integer of
        0: (u8: byte;);       (*!< Offset: 0x000 ( /W)  ITM Stimulus Port 8-bit                    *)
        1: (u16: word;);      (*!< Offset: 0x000 ( /W)  ITM Stimulus Port 16-bit                   *)
        2: (u32: longword;);  (*!< Offset: 0x000 ( /W)  ITM Stimulus Port 32-bit                   *)
    end;
    (*!< Offset: 0x000 ( /W)  ITM Stimulus Port Registers                *)
    RESERVED0: array [0..863] of longword;
    TER: longword;  (*!< Offset: 0xE00 (R/W)  ITM Trace Enable Register                  *)
    RESERVED1: array [0..14] of longword;
    TPR: longword;  (*!< Offset: 0xE40 (R/W)  ITM Trace Privilege Register               *)
    RESERVED2: array [0..14] of longword;
    TCR: longword;  (*!< Offset: 0xE80 (R/W)  ITM Trace Control Register                 *)
    RESERVED3: array [0..28] of longword;
    IWR: longword;   (*!< Offset: 0xEF8 ( /W)  ITM Integration Write Register             *)
    IRR: longword;   (*!< Offset: 0xEFC (R/ )  ITM Integration Read Register              *)
    IMCR: longword;  (*!< Offset: 0xF00 (R/W)  ITM Integration Mode Control Register      *)
    RESERVED4: array [0..42] of longword;
    LAR: longword;  (*!< Offset: 0xFB0 ( /W)  ITM Lock Access Register                   *)
    LSR: longword;  (*!< Offset: 0xFB4 (R/ )  ITM Lock Status Register                   *)
    RESERVED5: array [0..5] of longword;
    PID4: longword;  (*!< Offset: 0xFD0 (R/ )  ITM Peripheral Identification Register #4  *)
    PID5: longword;  (*!< Offset: 0xFD4 (R/ )  ITM Peripheral Identification Register #5  *)
    PID6: longword;  (*!< Offset: 0xFD8 (R/ )  ITM Peripheral Identification Register #6  *)
    PID7: longword;  (*!< Offset: 0xFDC (R/ )  ITM Peripheral Identification Register #7  *)
    PID0: longword;  (*!< Offset: 0xFE0 (R/ )  ITM Peripheral Identification Register #0  *)
    PID1: longword;  (*!< Offset: 0xFE4 (R/ )  ITM Peripheral Identification Register #1  *)
    PID2: longword;  (*!< Offset: 0xFE8 (R/ )  ITM Peripheral Identification Register #2  *)
    PID3: longword;  (*!< Offset: 0xFEC (R/ )  ITM Peripheral Identification Register #3  *)
    CID0: longword;  (*!< Offset: 0xFF0 (R/ )  ITM Component  Identification Register #0  *)
    CID1: longword;  (*!< Offset: 0xFF4 (R/ )  ITM Component  Identification Register #1  *)
    CID2: longword;  (*!< Offset: 0xFF8 (R/ )  ITM Component  Identification Register #2  *)
    CID3: longword;  (*!< Offset: 0xFFC (R/ )  ITM Component  Identification Register #3  *)
  end;

  DWT_Type = record
    CTRL: longword;       (*!< Offset: 0x000 (R/W)  Control Register                           *)
    CYCCNT: longword;     (*!< Offset: 0x004 (R/W)  Cycle Count Register                       *)
    CPICNT: longword;     (*!< Offset: 0x008 (R/W)  CPI Count Register                         *)
    EXCCNT: longword;     (*!< Offset: 0x00C (R/W)  Exception Overhead Count Register          *)
    SLEEPCNT: longword;   (*!< Offset: 0x010 (R/W)  Sleep Count Register                       *)
    LSUCNT: longword;     (*!< Offset: 0x014 (R/W)  LSU Count Register                         *)
    FOLDCNT: longword;    (*!< Offset: 0x018 (R/W)  Folded-instruction Count Register          *)
    PCSR: longword;       (*!< Offset: 0x01C (R/ )  Program Counter Sample Register            *)
    COMP0: longword;      (*!< Offset: 0x020 (R/W)  Comparator Register 0                      *)
    MASK0: longword;      (*!< Offset: 0x024 (R/W)  Mask Register 0                            *)
    FUNCTION0: longword;  (*!< Offset: 0x028 (R/W)  Function Register 0                        *)
    RESERVED0: array [0..0] of longword;
    COMP1: longword;      (*!< Offset: 0x030 (R/W)  Comparator Register 1                      *)
    MASK1: longword;      (*!< Offset: 0x034 (R/W)  Mask Register 1                            *)
    FUNCTION1: longword;  (*!< Offset: 0x038 (R/W)  Function Register 1                        *)
    RESERVED1: array [0..0] of longword;
    COMP2: longword;      (*!< Offset: 0x040 (R/W)  Comparator Register 2                      *)
    MASK2: longword;      (*!< Offset: 0x044 (R/W)  Mask Register 2                            *)
    FUNCTION2: longword;  (*!< Offset: 0x048 (R/W)  Function Register 2                        *)
    RESERVED2: array [0..0] of longword;
    COMP3: longword;      (*!< Offset: 0x050 (R/W)  Comparator Register 3                      *)
    MASK3: longword;      (*!< Offset: 0x054 (R/W)  Mask Register 3                            *)
    FUNCTION3: longword;  (*!< Offset: 0x058 (R/W)  Function Register 3                        *)
    RESERVED3: array [0..980] of longword;
    LAR: longword;  (*!< Offset: 0xFB0 (  W)  Lock Access Register                       *)
    LSR: longword;  (*!< Offset: 0xFB4 (R  )  Lock Status Register                       *)
  end;

  TPI_Type = record
    SSPSR: longword;  (*!< Offset: 0x000 (R/ )  Supported Parallel Port Size Register      *)
    CSPSR: longword;  (*!< Offset: 0x004 (R/W)  Current Parallel Port Size Register  *)
    RESERVED0: array [0..1] of longword;
    ACPR: longword;  (*!< Offset: 0x010 (R/W)  Asynchronous Clock Prescaler Register  *)
    RESERVED1: array [0..54] of longword;
    SPPR: longword;  (*!< Offset: 0x0F0 (R/W)  Selected Pin Protocol Register  *)
    RESERVED2: array [0..130] of longword;
    FFSR: longword;  (*!< Offset: 0x300 (R/ )  Formatter and Flush Status Register  *)
    FFCR: longword;  (*!< Offset: 0x304 (R/W)  Formatter and Flush Control Register  *)
    FSCR: longword;  (*!< Offset: 0x308 (R/ )  Formatter Synchronization Counter Register  *)
    RESERVED3: array [0..758] of longword;
    TRIGGER: longword;    (*!< Offset: 0xEE8 (R/ )  TRIGGER  *)
    FIFO0: longword;      (*!< Offset: 0xEEC (R/ )  Integration ETM Data  *)
    ITATBCTR2: longword;  (*!< Offset: 0xEF0 (R/ )  ITATBCTR2  *)
    RESERVED4: array [0..0] of longword;
    ITATBCTR0: longword;  (*!< Offset: 0xEF8 (R/ )  ITATBCTR0  *)
    FIFO1: longword;      (*!< Offset: 0xEFC (R/ )  Integration ITM Data  *)
    ITCTRL: longword;     (*!< Offset: 0xF00 (R/W)  Integration Mode Control  *)
    RESERVED5: array [0..38] of longword;
    CLAIMSET: longword;  (*!< Offset: 0xFA0 (R/W)  Claim tag set  *)
    CLAIMCLR: longword;  (*!< Offset: 0xFA4 (R/W)  Claim tag clear  *)
    RESERVED7: array [0..7] of longword;
    DEVID: longword;    (*!< Offset: 0xFC8 (R/ )  TPIU_DEVID  *)
    DEVTYPE: longword;  (*!< Offset: 0xFCC (R/ )  TPIU_DEVTYPE  *)
  end;

  MPU_Type = record
    TYPE_: longword;    (*!< Offset: 0x000 (R/ )  MPU Type Register                               *)
    CTRL: longword;     (*!< Offset: 0x004 (R/W)  MPU Control Register                            *)
    RNR: longword;      (*!< Offset: 0x008 (R/W)  MPU Region RNRber Register                      *)
    RBAR: longword;     (*!< Offset: 0x00C (R/W)  MPU Region Base Address Register                *)
    RASR: longword;     (*!< Offset: 0x010 (R/W)  MPU Region Attribute and Size Register          *)
    RBAR_A1: longword;  (*!< Offset: 0x014 (R/W)  MPU Alias 1 Region Base Address Register        *)
    RASR_A1: longword;  (*!< Offset: 0x018 (R/W)  MPU Alias 1 Region Attribute and Size Register  *)
    RBAR_A2: longword;  (*!< Offset: 0x01C (R/W)  MPU Alias 2 Region Base Address Register        *)
    RASR_A2: longword;  (*!< Offset: 0x020 (R/W)  MPU Alias 2 Region Attribute and Size Register  *)
    RBAR_A3: longword;  (*!< Offset: 0x024 (R/W)  MPU Alias 3 Region Base Address Register        *)
    RASR_A3: longword;  (*!< Offset: 0x028 (R/W)  MPU Alias 3 Region Attribute and Size Register  *)
  end;

  FPU_Type = record
    RESERVED0: array [0..0] of longword;
    FPCCR: longword;   (*!< Offset: 0x004 (R/W)  Floating-Point Context Control Register                *)
    FPCAR: longword;   (*!< Offset: 0x008 (R/W)  Floating-Point Context Address Register                *)
    FPDSCR: longword;  (*!< Offset: 0x00C (R/W)  Floating-Point Default Status Control Register         *)
    MVFR0: longword;   (*!< Offset: 0x010 (R/ )  Media and FP Feature Register 0                        *)
    MVFR1: longword;   (*!< Offset: 0x014 (R/ )  Media and FP Feature Register 1                        *)
    MVFR2: longword;   (*!< Offset: 0x018 (R/ )  Media and FP Feature Register 2                        *)
  end;

  CoreDebug_Type = record
    DHCSR: longword;  (*!< Offset: 0x000 (R/W)  Debug Halting Control and Status Register     *)
    DCRSR: longword;  (*!< Offset: 0x004 ( /W)  Debug Core Register Selector Register         *)
    DCRDR: longword;  (*!< Offset: 0x008 (R/W)  Debug Core Register Data Register             *)
    DEMCR: longword;  (*!< Offset: 0x00C (R/W)  Debug Exception and Monitor Control Register  *)
  end;

(* Memory mapping of Cortex-M4 Hardware  *)

const
  SCS_BASE       = $E000E000;         (*!< System Control Space Base Address   *)
  ITM_BASE       = $E0000000;         (*!< ITM Base Address                    *)
  DWT_BASE       = $E0001000;         (*!< DWT Base Address                    *)
  TPI_BASE       = $E0040000;         (*!< TPI Base Address                    *)
  CoreDebug_BASE = $E000EDF0;         (*!< Core Debug Base Address             *)
  SysTick_BASE   = SCS_BASE + $001;   (*!< SysTick Base Address                *)
  NVIC_BASE      = SCS_BASE + $010;   (*!< NVIC Base Address                   *)
  SCB_BASE       = SCS_BASE + $0D0;   (*!< System Control Block Base Address   *)

var
  SCnSCB:    SCnSCB_Type absolute SCS_BASE;       (*!< System control Register not in SCB  *)
  SCB:       SCB_Type absolute SCB_BASE;          (*!< SCB configuration struct            *)
  SysTick:   SysTick_Type absolute SysTick_BASE;  (*!< SysTick configuration struct        *)
  NVIC:      NVIC_Type absolute NVIC_BASE;        (*!< NVIC configuration struct           *)
  ITM:       ITM_Type absolute ITM_BASE;          (*!< ITM configuration struct            *)
  DWT:       DWT_Type absolute DWT_BASE;          (*!< DWT configuration struct            *)
  TPI:       TPI_Type absolute TPI_BASE;          (*!< TPI configuration struct            *)
  CoreDebug: CoreDebug_Type absolute CoreDebug_BASE;  (*!< Core Debug configuration struct     *)

type
  TITM_Port = 0..31;


procedure ITM_SendData(Port: TITM_Port; Data: char); inline;

implementation

procedure ITM_SendData(Port: TITM_Port; Data: char);
begin
  if (((ITM.TCR and 1) <> 0) and ((ITM.TER and 1) <> 0)) then
  begin
    while (ITM.PORT[integer(Port)].u32 = 0) do ;
    ITM.PORT[integer(Port)].u8 := byte(Data);
  end;
end;

end.

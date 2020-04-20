{
 System register definitions and utility code for Cortex-M0
 Created by Jeppe Johansen 2012 - jeppe@j-software.dk
 Modified for M0 by Michael Ring 2013 - mail@michael-ring.org
}
unit cortexm0;

interface

{$PACKRECORDS 2}
const
 SCS_BASE   = $E000E000;
 SysTick_BASE = SCS_BASE+$0010;
 NVIC_BASE = SCS_BASE+$0100;
 SCB_BASE = SCS_BASE+$0D00;

 DWT_BASE   = $E0001000;
 FP_BASE    = $E0002000;
 ITM_BASE   = $E0000000;
 TPIU_BASE  = $E0040000;
 ETM_BASE   = $E0041000;

type

 TNVICRegisters = record
   ISER : dword;
   RESERVED0 : array[0..30] of dword;
   ICER : dword;
   RSERVED1 : array[0..30] of dword;
   ISPR : dword;
   RESERVED2 : array[0..30] of dword;
   ICPR : dword;
   RESERVED3 : array[0..30] of dword;
   RESERVED4 : array[0..63] of dword;
   IPR : array[0..7] of dword;
 end;

 TSCBRegisters = record
  CPUID,                            {!< CPU ID Base Register                                     }
  ICSR,                             {!< Interrupt Control State Register                         }
  RESERVED0,
  AIRCR,                            {!< Application Interrupt / Reset Control Register           }
  SCR,                              {!< System Control Register                                  }
  CCR: dword;                       {!< Configuration Control Register                           }
  RESERVED1 : dword;
  SHP: array[0..1] of dword;        {!< System Handlers Priority Registers (4-7, 8-11, 12-15)    }
 end;

 TSysTickRegisters = record
  Ctrl,
  Load,
  Val,
  Calib: dword;
 end;

 TCoreDebugRegisters = record
  DHCSR,
  DCRSR,
  DCRDR,
  DEMCR: longword;
 end;


var
 // System Control
 InterruptControlType: longword     absolute (SCS_BASE+$0004);
 SCB: TSCBRegisters                 absolute (SCS_BASE+$0D00);
 SysTick: TSysTickRegisters         absolute (SCS_BASE+$0010);
 NVIC: TNVICRegisters               absolute (SCS_BASE+$0100);
 SoftwareTriggerInterrupt: longword absolute (SCS_BASE+$0000);

 // Core Debug
 CoreDebug: TCoreDebugRegisters     absolute (SCS_BASE+$0DF0);


implementation

end.

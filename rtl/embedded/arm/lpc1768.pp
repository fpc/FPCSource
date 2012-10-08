
unit lpc1768;

{$goto on}
{$define lpc1768}

interface

var
    STCTRL   : DWord absolute $E000E010;
    STRELOAD : DWord absolute $E000E014;
    STCURR   : DWord absolute $E000E018;

    FIO1DIR2 : Byte  absolute $2009C022;
    FIO1SET2 : Byte  absolute $2009C03A;
    FIO1CLR2 : Byte  absolute $2009C03E;

    SCS      : DWord absolute $400FC1A0;
    CLKSRCSEL: DWord absolute $400FC10C;
    PLL0FEED : DWord absolute $400FC08C;
    PLL0CON  : DWord absolute $400FC080;
    PLL0CFG  : DWord absolute $400FC084;
    PLL0STAT : DWord absolute $400FC088;
    CCLKCFG  : DWord absolute $400FC104;

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
procedure Watchdog_Interrupt; external name 'Watchdog_Interrupt';
procedure Timer0_Interrupt; external name 'Timer0_Interrupt';
procedure Timer1_Interrupt; external name 'Timer1_Interrupt';
procedure Timer2_Interrupt; external name 'Timer2_Interrupt';
procedure Timer3_Interrupt; external name 'Timer3_Interrupt';
procedure UART0_Interrupt; external name 'UART0_Interrupt';
procedure UART1_Interrupt; external name 'UART1_Interrupt';
procedure UART2_Interrupt; external name 'UART2_Interrupt';
procedure UART3_Interrupt; external name 'UART3_Interrupt';
procedure PWM1_Interrupt; external name 'PWM1_Interrupt';
procedure I2C0_Interrupt; external name 'I2C0_Interrupt';
procedure I2C1_Interrupt; external name 'I2C1_Interrupt';
procedure I2C2_Interrupt; external name 'I2C2_Interrupt';
procedure SPI_Interrupt; external name 'SPI_Interrupt';
procedure SSP0_Interrupt; external name 'SSP0_Interrupt';
procedure SSP1_Interrupt; external name 'SSP1_Interrupt';
procedure PLL0_Interrupt; external name 'PLL0_Interrupt';
procedure RTC_Interrupt; external name 'RTC_Interrupt';
procedure EINT0_Interrupt; external name 'EINT0_Interrupt';
procedure EINT1_Interrupt; external name 'EINT1_Interrupt';
procedure EINT2_Interrupt; external name 'EINT2_Interrupt';
procedure EINT3_Interrupt; external name 'EINT3_Interrupt';
procedure ADC_Interrupt; external name 'ADC_Interrupt';
procedure BOD_Interrupt; external name 'BOD_Interrupt';
procedure USB_Interrupt; external name 'USB_Interrupt';
procedure CAN_Interrupt; external name 'CAN_Interrupt';
procedure HPDMA_Interrupt; external name 'HPDMA_Interrupt';
procedure I2C_Interrupt; external name 'I2C_Interrupt';
procedure Ethernet_Interrupt; external name 'Ethernet_Interrupt';
procedure RITINT_Interrupt; external name 'RITINT_Interrupt';
procedure MotorControlPWM_Interrupt; external name 'MotorControlPWM_Interrupt';
procedure QuadratureEncoder_Interrupt; external name 'QuadratureEncoder_Interrupt';
procedure PLL1_Interrupt; external name 'PLL1_Interrupt';
procedure USBActivity_Interrupt; external name 'USBActivity_Interrupt';
procedure CanActivity_Interrupt; external name 'CanActivity_Interrupt';

{$i cortexm3_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
interrupt_vectors:
  .long _stack_top            // stack top address
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
  
  .long Watchdog_Interrupt
  .long Timer0_Interrupt
  .long Timer1_Interrupt
  .long Timer2_Interrupt
  .long Timer3_Interrupt
  .long UART0_Interrupt
  .long UART1_Interrupt
  .long UART2_Interrupt
  .long UART3_Interrupt
  .long PWM1_Interrupt
  .long I2C0_Interrupt
  .long I2C1_Interrupt
  .long I2C2_Interrupt
  .long SPI_Interrupt
  .long SSP0_Interrupt
  .long SSP1_Interrupt
  .long PLL0_Interrupt
  .long RTC_Interrupt
  .long EINT0_Interrupt
  .long EINT1_Interrupt
  .long EINT2_Interrupt
  .long EINT3_Interrupt
  .long ADC_Interrupt
  .long BOD_Interrupt
  .long USB_Interrupt
  .long CAN_Interrupt
  .long HPDMA_Interrupt
  .long I2C_Interrupt
  .long Ethernet_Interrupt
  .long RITINT_Interrupt
  .long MotorControlPWM_Interrupt
  .long QuadratureEncoder_Interrupt
  .long PLL1_Interrupt
  .long USBActivity_Interrupt
  .long CanActivity_Interrupt
  
  .weak NMI_interrupt
  .weak Hardfault_interrupt
  .weak MemManage_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SWI_interrupt
  .weak DebugMonitor_interrupt
  .weak PendingSV_interrupt
  .weak SysTick_interrupt
  .weak Watchdog_Interrupt
  .weak Timer0_Interrupt
  .weak Timer1_Interrupt
  .weak Timer2_Interrupt
  .weak Timer3_Interrupt
  .weak UART0_Interrupt
  .weak UART1_Interrupt
  .weak UART2_Interrupt
  .weak UART3_Interrupt
  .weak PWM1_Interrupt
  .weak I2C0_Interrupt
  .weak I2C1_Interrupt
  .weak I2C2_Interrupt
  .weak SPI_Interrupt
  .weak SSP0_Interrupt
  .weak SSP1_Interrupt
  .weak PLL0_Interrupt
  .weak RTC_Interrupt
  .weak EINT0_Interrupt
  .weak EINT1_Interrupt
  .weak EINT2_Interrupt
  .weak EINT3_Interrupt
  .weak ADC_Interrupt
  .weak BOD_Interrupt
  .weak USB_Interrupt
  .weak CAN_Interrupt
  .weak HPDMA_Interrupt
  .weak I2C_Interrupt
  .weak Ethernet_Interrupt
  .weak RITINT_Interrupt
  .weak MotorControlPWM_Interrupt
  .weak QuadratureEncoder_Interrupt
  .weak PLL1_Interrupt
  .weak USBActivity_Interrupt
  .weak CanActivity_Interrupt
  
    .set NMI_interrupt, Startup
  .set Hardfault_interrupt, Startup
  .set MemManage_interrupt, Startup
  .set BusFault_interrupt, Startup
  .set UsageFault_interrupt, Startup
  .set SWI_interrupt, Startup
  .set DebugMonitor_interrupt, Startup
  .set PendingSV_interrupt, Startup
  .set SysTick_interrupt, Startup
  .set Watchdog_Interrupt, Startup
  .set Timer0_Interrupt, Startup
  .set Timer1_Interrupt, Startup
  .set Timer2_Interrupt, Startup
  .set Timer3_Interrupt, Startup
  .set UART0_Interrupt, Startup
  .set UART1_Interrupt, Startup
  .set UART2_Interrupt, Startup
  .set UART3_Interrupt, Startup
  .set PWM1_Interrupt, Startup
  .set I2C0_Interrupt, Startup
  .set I2C1_Interrupt, Startup
  .set I2C2_Interrupt, Startup
  .set SPI_Interrupt, Startup
  .set SSP0_Interrupt, Startup
  .set SSP1_Interrupt, Startup
  .set PLL0_Interrupt, Startup
  .set RTC_Interrupt, Startup
  .set EINT0_Interrupt, Startup
  .set EINT1_Interrupt, Startup
  .set EINT2_Interrupt, Startup
  .set EINT3_Interrupt, Startup
  .set ADC_Interrupt, Startup
  .set BOD_Interrupt, Startup
  .set USB_Interrupt, Startup
  .set CAN_Interrupt, Startup
  .set HPDMA_Interrupt, Startup
  .set I2C_Interrupt, Startup
  .set Ethernet_Interrupt, Startup
  .set RITINT_Interrupt, Startup
  .set MotorControlPWM_Interrupt, Startup
  .set QuadratureEncoder_Interrupt, Startup
  .set PLL1_Interrupt, Startup
  .set USBActivity_Interrupt, Startup
  .set CanActivity_Interrupt, Startup
  
  .text
end;

end.



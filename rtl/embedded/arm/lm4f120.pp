{$goto on}
unit lm4f120;

interface

const
 Flash_Base             = $00000000;
 ROM_Base               = $01000000;
 SRAM_Base              = $20000000;
 Bitband_Base           = $22000000;
 
 // FiRM Peripherals
 Watchdog0_Base         = $40000000;
 Watchdog1_Base         = $40001000;
 GPIOA_Base             = $40004000;
 GPIOB_Base             = $40005000;
 GPIOC_Base             = $40006000;
 GPIOD_Base             = $40007000;
 SSI0_Base              = $40008000;
 SSI1_Base              = $40009000;
 SSI2_Base              = $4000A000;
 SSI3_Base              = $4000B000;
 UART0_Base             = $4000C000;
 UART1_Base             = $4000D000;
 UART2_Base             = $4000E000;
 UART3_Base             = $4000F000;
 UART4_Base             = $40010000;
 UART5_Base             = $40011000;
 UART6_Base             = $40012000;
 UART7_Base             = $40013000;
 
 // Peripherals
 I2C0_Base              = $40020000;
 I2C1_Base              = $40021000;
 I2C2_Base              = $40022000;
 I2C3_Base              = $40023000;
 GPIOE_Base             = $40024000;
 GPIOF_Base             = $40025000;
 Timer0_1632_Base       = $40030000;
 Timer1_1632_Base       = $40031000;
 Timer2_1632_Base       = $40032000;
 Timer3_1632_Base       = $40033000;
 Timer4_1632_Base       = $40034000;
 Timer5_1632_Base       = $40035000;
 Timer0_3264_Base       = $40036000;
 Timer1_3264_Base       = $40037000;
 ADC0_Base              = $40038000;
 ADC1_Base              = $40039000;
 AnalogComp_Base        = $4003C000;
 CAN0_Base              = $40040000;
 Timer2_3264_Base       = $4004C000;
 Timer3_3264_Base       = $4004D000;
 Timer4_3264_Base       = $4004E000;
 Timer5_3264_Base       = $4004F000;
 USB_Base               = $40050000;
 GPIOA_AHB_Base         = $40058000;
 GPIOB_AHB_Base         = $40059000;
 GPIOC_AHB_Base         = $4005A000;
 GPIOD_AHB_Base         = $4005B000;
 GPIOE_AHB_Base         = $4005C000;
 GPIOF_AHB_Base         = $4005D000;
 EEPROMKeyLocker_Base   = $400AF000;
 SystemException_Base   = $400F9000;
 Hibernation_Base       = $400FC000;
 FlashControl_Base      = $400FD000;
 SystemControl_Base     = $400FE000;
 uDMA_Base              = $400FF000;
 PeriphBitband_Base     = $42000000;
// Private Peripheral Bus
 ITM_Base               = $E0000000;
 DWT_Base               = $E0001000;
 FPB_Base               = $E0002000;
 CortexM4F_Base         = $E000E000;
 TPIU_Base              = $E0040000;
 ETM_Base               = $E0041000;

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

procedure GPIO_Port_A_interrupt; external name 'GPIO_Port_A_interrupt';
procedure GPIO_Port_B_interrupt; external name 'GPIO_Port_B_interrupt';
procedure GPIO_Port_C_interrupt; external name 'GPIO_Port_C_interrupt';
procedure GPIO_Port_D_interrupt; external name 'GPIO_Port_D_interrupt';
procedure GPIO_Port_E_interrupt; external name 'GPIO_Port_E_interrupt';
procedure UART0_interrupt; external name 'UART0_interrupt';
procedure UART1_interrupt; external name 'UART1_interrupt';
procedure SSI0_interrupt; external name 'SSI0_interrupt';
procedure I2C0_interrupt; external name 'I2C0_interrupt';
procedure ADC0_Seq_0_interrupt; external name 'ADC0_Seq_0_interrupt';
procedure ADC0_Seq_1_interrupt; external name 'ADC0_Seq_1_interrupt';
procedure ADC0_Seq_2_interrupt; external name 'ADC0_Seq_2_interrupt';
procedure ADC0_Seq_3_interrupt; external name 'ADC0_Seq_3_interrupt';
procedure Watchdog_0_and_1_interrupt; external name 'Watchdog_0_and_1_interrupt';
procedure Timer_1632_0A_interrupt; external name 'Timer_1632_0A_interrupt';
procedure Timer_1632_0B_interrupt; external name 'Timer_1632_0B_interrupt';
procedure Timer_1632_1A_interrupt; external name 'Timer_1632_1A_interrupt';
procedure Timer_1632_1B_interrupt; external name 'Timer_1632_1B_interrupt';
procedure Timer_1632_2A_interrupt; external name 'Timer_1632_2A_interrupt';
procedure Timer_1632_2B_interrupt; external name 'Timer_1632_2B_interrupt';
procedure Analog_Comp_0_interrupt; external name 'Analog_Comp_0_interrupt';
procedure Analog_Comp_1_interrupt; external name 'Analog_Comp_1_interrupt';
procedure System_Control_interrupt; external name 'System_Control_interrupt';
procedure Flash_and_EEPROM_interrupt; external name 'Flash_and_EEPROM_interrupt';
procedure GPIO_Port_F_interrupt; external name 'GPIO_Port_F_interrupt';
procedure UART2_interrupt; external name 'UART2_interrupt';
procedure SSI1_interrupt; external name 'SSI1_interrupt';
procedure Timer_1632_3A_interrupt; external name 'Timer_1632_3A_interrupt';
procedure Timer_1632_3B_interrupt; external name 'Timer_1632_3B_interrupt';
procedure I2C1_interrupt; external name 'I2C1_interrupt';
procedure CAN0_interrupt; external name 'CAN0_interrupt';
procedure Hibernation_interrupt; external name 'Hibernation_interrupt';
procedure USB_interrupt; external name 'USB_interrupt';
procedure uDMA_Software_interrupt; external name 'uDMA_Software_interrupt';
procedure uDMA_Error_interrupt; external name 'uDMA_Error_interrupt';
procedure ADC1_Seq_0_interrupt; external name 'ADC1_Seq_0_interrupt';
procedure ADC1_Seq_1_interrupt; external name 'ADC1_Seq_1_interrupt';
procedure ADC1_Seq_2_interrupt; external name 'ADC1_Seq_2_interrupt';
procedure ADC1_Seq_3_interrupt; external name 'ADC1_Seq_3_interrupt';
procedure SSI2_interrupt; external name 'SSI2_interrupt';
procedure SSI3_interrupt; external name 'SSI3_interrupt';
procedure UART3_interrupt; external name 'UART3_interrupt';
procedure UART4_interrupt; external name 'UART4_interrupt';
procedure UART5_interrupt; external name 'UART5_interrupt';
procedure UART6_interrupt; external name 'UART6_interrupt';
procedure UART7_interrupt; external name 'UART7_interrupt';
procedure I2C2_interrupt; external name 'I2C2_interrupt';
procedure I2C3_interrupt; external name 'I2C3_interrupt';
procedure Timer_1632_4A_interrupt; external name 'Timer_1632_4A_interrupt';
procedure Timer_1632_4B_interrupt; external name 'Timer_1632_4B_interrupt';
procedure Timer_1632_5A_interrupt; external name 'Timer_1632_5A_interrupt';
procedure Timer_1632_5B_interrupt; external name 'Timer_1632_5B_interrupt';
procedure Timer_3264_0A_interrupt; external name 'Timer_3264_0A_interrupt';
procedure Timer_3264_0B_interrupt; external name 'Timer_3264_0B_interrupt';
procedure Timer_3264_1A_interrupt; external name 'Timer_3264_1A_interrupt';
procedure Timer_3264_1B_interrupt; external name 'Timer_3264_1B_interrupt';
procedure Timer_3264_2A_interrupt; external name 'Timer_3264_2A_interrupt';
procedure Timer_3264_2B_interrupt; external name 'Timer_3264_2B_interrupt';
procedure Timer_3264_3A_interrupt; external name 'Timer_3264_3A_interrupt';
procedure Timer_3264_3B_interrupt; external name 'Timer_3264_3B_interrupt';
procedure Timer_3264_4A_interrupt; external name 'Timer_3264_4A_interrupt';
procedure Timer_3264_4B_interrupt; external name 'Timer_3264_4B_interrupt';
procedure Timer_3264_5A_interrupt; external name 'Timer_3264_5A_interrupt';
procedure Timer_3264_5B_interrupt; external name 'Timer_3264_5B_interrupt';
procedure System_Exception_imprecise_interrupt; external name 'System_Exception_imprecise_interrupt';

{$i cortexm4f_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
interrupt_vectors:
  .long _stack_top
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

  .long GPIO_Port_A_interrupt
  .long GPIO_Port_B_interrupt
  .long GPIO_Port_C_interrupt
  .long GPIO_Port_D_interrupt
  .long GPIO_Port_E_interrupt
  .long UART0_interrupt
  .long UART1_interrupt
  .long SSI0_interrupt
  .long I2C0_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long ADC0_Seq_0_interrupt
  .long ADC0_Seq_1_interrupt
  .long ADC0_Seq_2_interrupt
  .long ADC0_Seq_3_interrupt
  .long Watchdog_0_and_1_interrupt
  .long Timer_1632_0A_interrupt
  .long Timer_1632_0B_interrupt
  .long Timer_1632_1A_interrupt
  .long Timer_1632_1B_interrupt
  .long Timer_1632_2A_interrupt
  .long Timer_1632_2B_interrupt
  .long Analog_Comp_0_interrupt
  .long 0
  .long Analog_Comp_1_interrupt
  .long System_Control_interrupt
  .long Flash_and_EEPROM_interrupt
  .long GPIO_Port_F_interrupt
  .long 0
  .long 0
  .long UART2_interrupt
  .long SSI1_interrupt
  .long Timer_1632_3A_interrupt
  .long Timer_1632_3B_interrupt
  .long I2C1_interrupt
  .long 0
  .long CAN0_interrupt
  .long 0
  .long 0
  .long 0
  .long Hibernation_interrupt
  .long USB_interrupt
  .long 0
  .long uDMA_Software_interrupt
  .long uDMA_Error_interrupt
  .long ADC1_Seq_0_interrupt
  .long ADC1_Seq_1_interrupt
  .long ADC1_Seq_2_interrupt
  .long ADC1_Seq_3_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long SSI2_interrupt
  .long SSI3_interrupt
  .long UART3_interrupt
  .long UART4_interrupt
  .long UART5_interrupt
  .long UART6_interrupt
  .long UART7_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long I2C2_interrupt
  .long I2C3_interrupt
  .long Timer_1632_4A_interrupt
  .long Timer_1632_4B_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long Timer_1632_5A_interrupt
  .long Timer_1632_5B_interrupt
  .long Timer_3264_0A_interrupt
  .long Timer_3264_0B_interrupt
  .long Timer_3264_1A_interrupt
  .long Timer_3264_1B_interrupt
  .long Timer_3264_2A_interrupt
  .long Timer_3264_2B_interrupt
  .long Timer_3264_3A_interrupt
  .long Timer_3264_3B_interrupt
  .long Timer_3264_4A_interrupt
  .long Timer_3264_4B_interrupt
  .long Timer_3264_5A_interrupt
  .long Timer_3264_5B_interrupt
  .long System_Exception_imprecise_interrupt

  .weak NMI_interrupt
  .weak Hardfault_interrupt
  .weak MemManage_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SWI_interrupt
  .weak DebugMonitor_interrupt
  .weak PendingSV_interrupt
  .weak SysTick_interrupt

  .weak GPIO_Port_A_interrupt
  .weak GPIO_Port_B_interrupt
  .weak GPIO_Port_C_interrupt
  .weak GPIO_Port_D_interrupt
  .weak GPIO_Port_E_interrupt
  .weak UART0_interrupt
  .weak UART1_interrupt
  .weak SSI0_interrupt
  .weak I2C0_interrupt
  .weak ADC0_Seq_0_interrupt
  .weak ADC0_Seq_1_interrupt
  .weak ADC0_Seq_2_interrupt
  .weak ADC0_Seq_3_interrupt
  .weak Watchdog_0_and_1_interrupt
  .weak Timer_1632_0A_interrupt
  .weak Timer_1632_0B_interrupt
  .weak Timer_1632_1A_interrupt
  .weak Timer_1632_1B_interrupt
  .weak Timer_1632_2A_interrupt
  .weak Timer_1632_2B_interrupt
  .weak Analog_Comp_0_interrupt
  .weak Analog_Comp_1_interrupt
  .weak System_Control_interrupt
  .weak Flash_and_EEPROM_interrupt
  .weak GPIO_Port_F_interrupt
  .weak UART2_interrupt
  .weak SSI1_interrupt
  .weak Timer_1632_3A_interrupt
  .weak Timer_1632_3B_interrupt
  .weak I2C1_interrupt
  .weak CAN0_interrupt
  .weak Hibernation_interrupt
  .weak USB_interrupt
  .weak uDMA_Software_interrupt
  .weak uDMA_Error_interrupt
  .weak ADC1_Seq_0_interrupt
  .weak ADC1_Seq_1_interrupt
  .weak ADC1_Seq_2_interrupt
  .weak ADC1_Seq_3_interrupt
  .weak SSI2_interrupt
  .weak SSI3_interrupt
  .weak UART3_interrupt
  .weak UART4_interrupt
  .weak UART5_interrupt
  .weak UART6_interrupt
  .weak UART7_interrupt
  .weak I2C2_interrupt
  .weak I2C3_interrupt
  .weak Timer_1632_4A_interrupt
  .weak Timer_1632_4B_interrupt
  .weak Timer_1632_5A_interrupt
  .weak Timer_1632_5B_interrupt
  .weak Timer_3264_0A_interrupt
  .weak Timer_3264_0B_interrupt
  .weak Timer_3264_1A_interrupt
  .weak Timer_3264_1B_interrupt
  .weak Timer_3264_2A_interrupt
  .weak Timer_3264_2B_interrupt
  .weak Timer_3264_3A_interrupt
  .weak Timer_3264_3B_interrupt
  .weak Timer_3264_4A_interrupt
  .weak Timer_3264_4B_interrupt
  .weak Timer_3264_5A_interrupt
  .weak Timer_3264_5B_interrupt
  .weak System_Exception_imprecise_interrupt

  .set NMI_interrupt, HaltProc
  .set Hardfault_interrupt, HaltProc
  .set MemManage_interrupt, HaltProc
  .set BusFault_interrupt, HaltProc
  .set UsageFault_interrupt, HaltProc
  .set SWI_interrupt, HaltProc
  .set DebugMonitor_interrupt, HaltProc
  .set PendingSV_interrupt, HaltProc
  .set SysTick_interrupt, HaltProc

  .set GPIO_Port_A_interrupt, HaltProc
  .set GPIO_Port_B_interrupt, HaltProc
  .set GPIO_Port_C_interrupt, HaltProc
  .set GPIO_Port_D_interrupt, HaltProc
  .set GPIO_Port_E_interrupt, HaltProc
  .set UART0_interrupt, HaltProc
  .set UART1_interrupt, HaltProc
  .set SSI0_interrupt, HaltProc
  .set I2C0_interrupt, HaltProc
  .set ADC0_Seq_0_interrupt, HaltProc
  .set ADC0_Seq_1_interrupt, HaltProc
  .set ADC0_Seq_2_interrupt, HaltProc
  .set ADC0_Seq_3_interrupt, HaltProc
  .set Watchdog_0_and_1_interrupt, HaltProc
  .set Timer_1632_0A_interrupt, HaltProc
  .set Timer_1632_0B_interrupt, HaltProc
  .set Timer_1632_1A_interrupt, HaltProc
  .set Timer_1632_1B_interrupt, HaltProc
  .set Timer_1632_2A_interrupt, HaltProc
  .set Timer_1632_2B_interrupt, HaltProc
  .set Analog_Comp_0_interrupt, HaltProc
  .set Analog_Comp_1_interrupt, HaltProc
  .set System_Control_interrupt, HaltProc
  .set Flash_and_EEPROM_interrupt, HaltProc
  .set GPIO_Port_F_interrupt, HaltProc
  .set UART2_interrupt, HaltProc
  .set SSI1_interrupt, HaltProc
  .set Timer_1632_3A_interrupt, HaltProc
  .set Timer_1632_3B_interrupt, HaltProc
  .set I2C1_interrupt, HaltProc
  .set CAN0_interrupt, HaltProc
  .set Hibernation_interrupt, HaltProc
  .set USB_interrupt, HaltProc
  .set uDMA_Software_interrupt, HaltProc
  .set uDMA_Error_interrupt, HaltProc
  .set ADC1_Seq_0_interrupt, HaltProc
  .set ADC1_Seq_1_interrupt, HaltProc
  .set ADC1_Seq_2_interrupt, HaltProc
  .set ADC1_Seq_3_interrupt, HaltProc
  .set SSI2_interrupt, HaltProc
  .set SSI3_interrupt, HaltProc
  .set UART3_interrupt, HaltProc
  .set UART4_interrupt, HaltProc
  .set UART5_interrupt, HaltProc
  .set UART6_interrupt, HaltProc
  .set UART7_interrupt, HaltProc
  .set I2C2_interrupt, HaltProc
  .set I2C3_interrupt, HaltProc
  .set Timer_1632_4A_interrupt, HaltProc
  .set Timer_1632_4B_interrupt, HaltProc
  .set Timer_1632_5A_interrupt, HaltProc
  .set Timer_1632_5B_interrupt, HaltProc
  .set Timer_3264_0A_interrupt, HaltProc
  .set Timer_3264_0B_interrupt, HaltProc
  .set Timer_3264_1A_interrupt, HaltProc
  .set Timer_3264_1B_interrupt, HaltProc
  .set Timer_3264_2A_interrupt, HaltProc
  .set Timer_3264_2B_interrupt, HaltProc
  .set Timer_3264_3A_interrupt, HaltProc
  .set Timer_3264_3B_interrupt, HaltProc
  .set Timer_3264_4A_interrupt, HaltProc
  .set Timer_3264_4B_interrupt, HaltProc
  .set Timer_3264_5A_interrupt, HaltProc
  .set Timer_3264_5B_interrupt, HaltProc
  .set System_Exception_imprecise_interrupt, HaltProc

  .text
end;

end.


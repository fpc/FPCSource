{
Register definitions and utility code for stellaris
Preliminary startup code 
Geoffrey Barton 2010 08 01  gjb@periphon.net
based on stm32f103 created by Jeppe Johansen 2009 - jepjoh2@kom.aau.dk
}
{$goto on}
unit lm3fury;

  interface

    type
      TBitvector32 = bitpacked array[0..31] of 0..1;

    {$PACKRECORDS 4}
    const
      PeripheralBase 	= $40000000;
      PPBbase			= $E0000fff;
      APBbase 		= PeripheralBase;
      AHBbase 		= PeripheralBase+$54000;
      portAoffset=APBbase+$4000;
      portBoffset=APBbase+$5000;
      portCoffset=APBbase+$6000;
      portDoffset=APBbase+$7000;
      portEoffset=APBbase+$24000;
      portFoffset=APBbase+$25000;
      portGoffset=APBbase+$26000;
      portHoffset=APBbase+$27000;
      portJoffset=APBbase+$3d000;
      sysconoffset=APBbase+$fe000;


    type
      TgpioPort=record
        data:array[0..255] of dword;dir,_is,ibe,iev,im,ris,mis,icr,
        afsel:dword;dummy1:array[0..54] of dword;dr2r,dr4r,dr8r,odr,pur,pdr,slr,den,lock,cr,amsel,pctl:dword;
      end;

      Tsyscon=record
        did0,did1,dc0,res0c,dc1,dc2,dc3,dc4,dc5,dc6,dc7,dc8,borc,res34,res38,res3c,
        src0,src1,src2,res4c,ris,imc,misc,resc,rcc,pllcfg,res68,gpiohbctl,rcc2,res74,res78,moscctl:dword;res80:array[0..31] of dword;
        rcgc0,rcgc1,rcgc2,res10,scgc0,scgc1,scgc2,
        res11,dcgc0,dcgc1,dcgc2,res12c,res130,res134,res138,res13c,res140,dsplpclk,res13,res14,res15,piosccal,
        i2smclk,res174,res178,res17c,res180,res184,res188,res18c,dc9,res194,res198,res19c,nvmstat:dword;
      end;

    {$ALIGN 4}
    var
      PortA			:Tgpioport 	absolute portAoffset;
      PortB			:Tgpioport	absolute portBoffset;
      PortC			:Tgpioport 	absolute portCoffset;
      PortD			:Tgpioport	absolute portDoffset;
      PortE			:Tgpioport 	absolute portEoffset;
      PortF			:Tgpioport	absolute portFoffset;
      PortG			:Tgpioport 	absolute portGoffset;
      PortH			:Tgpioport	absolute portHoffset;
      PortJ			:Tgpioport	absolute portJoffset;

      syscon		:Tsyscon	absolute sysconoffset;
      rcgc0			:dword absolute (sysconoffset+$100);
      rcgc1			:dword absolute (sysconoffset+$104);
      rcgc2			:dword absolute (sysconoffset+$108);

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
procedure GPIO_Port_A_Interrupt; external name 'GPIO_Port_A_Interrupt';
procedure GPIO_Port_B_Interrupt; external name 'GPIO_Port_B_Interrupt';
procedure GPIO_Port_C_Interrupt; external name 'GPIO_Port_C_Interrupt';
procedure GPIO_Port_D_Interrupt; external name 'GPIO_Port_D_Interrupt';
procedure GPIO_Port_E_Interrupt; external name 'GPIO_Port_E_Interrupt';
procedure UART0_Interrupt; external name 'UART0_Interrupt';
procedure UART1_Interrupt; external name 'UART1_Interrupt';
procedure SSI0_Interrupt; external name 'SSI0_Interrupt';
procedure I2C0_Interrupt; external name 'I2C0_Interrupt';
procedure ADC0_Sequence_0_Interrupt; external name 'ADC0_Sequence_0_Interrupt';
procedure ADC0_Sequence_1_Interrupt; external name 'ADC0_Sequence_1_Interrupt';
procedure ADC0_Sequence_2_Interrupt; external name 'ADC0_Sequence_2_Interrupt';
procedure ADC0_Sequence_3_Interrupt; external name 'ADC0_Sequence_3_Interrupt';
procedure Watchdog_Timer_0_Interrupt; external name 'Watchdog_Timer_0_Interrupt';
procedure Timer_0A_Interrupt; external name 'Timer_0A_Interrupt';
procedure Timer_0B_Interrupt; external name 'Timer_0B_Interrupt';
procedure Timer_1A_Interrupt; external name 'Timer_1A_Interrupt';
procedure Timer_1B_Interrupt; external name 'Timer_1B_Interrupt';
procedure Timer_2A_Interrupt; external name 'Timer_2A_Interrupt';
procedure Timer_2B_Interrupt; external name 'Timer_2B_Interrupt';
procedure Analog_Comparator_0_Interrupt; external name 'Analog_Comparator_0_Interrupt';
procedure Analog_Comparator_1_Interrupt; external name 'Analog_Comparator_1_Interrupt';
procedure Analog_Comparator_2_Interrupt; external name 'Analog_Comparator_2_Interrupt';
procedure System_Control_Interrupt; external name 'System_Control_Interrupt';
procedure Flash_Memory_Control_Interrupt; external name 'Flash_Memory_Control_Interrupt';
procedure GPIO_Port_F_Interrupt; external name 'GPIO_Port_F_Interrupt';
procedure GPIO_Port_G_Interrupt; external name 'GPIO_Port_G_Interrupt';
procedure GPIO_Port_H_Interrupt; external name 'GPIO_Port_H_Interrupt';
procedure UART2_Interrupt; external name 'UART2_Interrupt';
procedure SSI1_Interrupt; external name 'SSI1_Interrupt';
procedure Timer_3A_Interrupt; external name 'Timer_3A_Interrupt';
procedure Timer_3B_Interrupt; external name 'Timer_3B_Interrupt';
procedure I2C1_Interrupt; external name 'I2C1_Interrupt';
procedure Hibernation_Module_Interrupt; external name 'Hibernation_Module_Interrupt';

{$i cortexm3_start.inc}

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
  
  .long GPIO_Port_A_Interrupt
  .long GPIO_Port_B_Interrupt
  .long GPIO_Port_C_Interrupt
  .long GPIO_Port_D_Interrupt
  .long GPIO_Port_E_Interrupt
  .long UART0_Interrupt
  .long UART1_Interrupt
  .long SSI0_Interrupt
  .long I2C0_Interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long ADC0_Sequence_0_Interrupt
  .long ADC0_Sequence_1_Interrupt
  .long ADC0_Sequence_2_Interrupt
  .long ADC0_Sequence_3_Interrupt
  .long Watchdog_Timer_0_Interrupt
  .long Timer_0A_Interrupt
  .long Timer_0B_Interrupt
  .long Timer_1A_Interrupt
  .long Timer_1B_Interrupt
  .long Timer_2A_Interrupt
  .long Timer_2B_Interrupt
  .long Analog_Comparator_0_Interrupt
  .long Analog_Comparator_1_Interrupt
  .long Analog_Comparator_2_Interrupt
  .long System_Control_Interrupt
  .long Flash_Memory_Control_Interrupt
  .long GPIO_Port_F_Interrupt
  .long GPIO_Port_G_Interrupt
  .long GPIO_Port_H_Interrupt
  .long UART2_Interrupt
  .long SSI1_Interrupt
  .long Timer_3A_Interrupt
  .long Timer_3B_Interrupt
  .long I2C1_Interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long Hibernation_Module_Interrupt
  
  .weak NMI_interrupt
  .weak Hardfault_interrupt
  .weak MemManage_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SWI_interrupt
  .weak DebugMonitor_interrupt
  .weak PendingSV_interrupt
  .weak SysTick_interrupt
  .weak GPIO_Port_A_Interrupt
  .weak GPIO_Port_B_Interrupt
  .weak GPIO_Port_C_Interrupt
  .weak GPIO_Port_D_Interrupt
  .weak GPIO_Port_E_Interrupt
  .weak UART0_Interrupt
  .weak UART1_Interrupt
  .weak SSI0_Interrupt
  .weak I2C0_Interrupt
  .weak ADC0_Sequence_0_Interrupt
  .weak ADC0_Sequence_1_Interrupt
  .weak ADC0_Sequence_2_Interrupt
  .weak ADC0_Sequence_3_Interrupt
  .weak Watchdog_Timer_0_Interrupt
  .weak Timer_0A_Interrupt
  .weak Timer_0B_Interrupt
  .weak Timer_1A_Interrupt
  .weak Timer_1B_Interrupt
  .weak Timer_2A_Interrupt
  .weak Timer_2B_Interrupt
  .weak Analog_Comparator_0_Interrupt
  .weak Analog_Comparator_1_Interrupt
  .weak Analog_Comparator_2_Interrupt
  .weak System_Control_Interrupt
  .weak Flash_Memory_Control_Interrupt
  .weak GPIO_Port_F_Interrupt
  .weak GPIO_Port_G_Interrupt
  .weak GPIO_Port_H_Interrupt
  .weak UART2_Interrupt
  .weak SSI1_Interrupt
  .weak Timer_3A_Interrupt
  .weak Timer_3B_Interrupt
  .weak I2C1_Interrupt
  .weak Hibernation_Module_Interrupt
  
  .set NMI_interrupt, Startup
  .set Hardfault_interrupt, Startup
  .set MemManage_interrupt, Startup
  .set BusFault_interrupt, Startup
  .set UsageFault_interrupt, Startup
  .set SWI_interrupt, Startup
  .set DebugMonitor_interrupt, Startup
  .set PendingSV_interrupt, Startup
  .set SysTick_interrupt, Startup
  .set GPIO_Port_A_Interrupt, Startup
  .set GPIO_Port_B_Interrupt, Startup
  .set GPIO_Port_C_Interrupt, Startup
  .set GPIO_Port_D_Interrupt, Startup
  .set GPIO_Port_E_Interrupt, Startup
  .set UART0_Interrupt, Startup
  .set UART1_Interrupt, Startup
  .set SSI0_Interrupt, Startup
  .set I2C0_Interrupt, Startup
  .set ADC0_Sequence_0_Interrupt, Startup
  .set ADC0_Sequence_1_Interrupt, Startup
  .set ADC0_Sequence_2_Interrupt, Startup
  .set ADC0_Sequence_3_Interrupt, Startup
  .set Watchdog_Timer_0_Interrupt, Startup
  .set Timer_0A_Interrupt, Startup
  .set Timer_0B_Interrupt, Startup
  .set Timer_1A_Interrupt, Startup
  .set Timer_1B_Interrupt, Startup
  .set Timer_2A_Interrupt, Startup
  .set Timer_2B_Interrupt, Startup
  .set Analog_Comparator_0_Interrupt, Startup
  .set Analog_Comparator_1_Interrupt, Startup
  .set Analog_Comparator_2_Interrupt, Startup
  .set System_Control_Interrupt, Startup
  .set Flash_Memory_Control_Interrupt, Startup
  .set GPIO_Port_F_Interrupt, Startup
  .set GPIO_Port_G_Interrupt, Startup
  .set GPIO_Port_H_Interrupt, Startup
  .set UART2_Interrupt, Startup
  .set SSI1_Interrupt, Startup
  .set Timer_3A_Interrupt, Startup
  .set Timer_3B_Interrupt, Startup
  .set I2C1_Interrupt, Startup
  .set Hibernation_Module_Interrupt, Startup
  
  .text
end;

end.


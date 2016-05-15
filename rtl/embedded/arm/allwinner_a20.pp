unit allwinner_a20;

{$goto on}

  interface

    const
      SRAM_A1_Base = $00000000;
      SRAM_A2_Base = $00004000;
      SRAM_A3_Base = $00008000;
      SRAM_A4_Base = $0000B400;
      SRAM_D_Base = $00010000;
      SRAM_B_Secure_Base = $00020000;
      SRAM_Controller_Base = $01C00000;
      DRAM_Controller_Base = $01C01000;
      DMA_Base = $01C02000;
      NAND_Flash_Base = $01C03000;
      Transport_Stream_Base = $01C04000;
      SPI_0_Base = $01C05000;
      SPI_1_Base = $01C06000;
      Memory_Stick_Base = $01C07000;
      TVD_Base = $01C08000;
      CSI_0_Base = $01C09000;
      TVE_0_Base = $01C0A000;
      EMAC_Base = $01C0B000;
      LCD_0_Base = $01C0C000;
      LCD_1_Base = $01C0D000;
      Video_Engine_Base = $01C0E000;
      SD_MMC_0_Base = $01C0F000;
      SD_MMC_1_Base = $01C10000;
      SD_MMC_2_Base = $01C11000;
      SD_MMC_3_Base = $01C12000;
      USB_0_Base = $01C13000;
      USB_1_Base = $01C14000;
      Security_System_Base = $01C15000;
      HDMI_Base = $01C16000;
      SPI_2_Base = $01C17000;
      SATA_Base = $01C18000;
      PATA_Base = $01C19000;
      ACE_Base = $01C1A000;
      TVE_1_Base = $01C1B000;
      USB_2_Base = $01C1C000;
      CSI_1_Base = $01C1D000;
      SPI3_Base = $01C1F000;
      CCU_Base = $01C20000;
      Interrupt_Base = $01C20400;
      PIO_Base = $01C20800;
      Timer_Base = $01C20C00;
      SPDIF_Base = $01C21000;
      AC97_Base = $01C21400;
      IR0_Base = $01C21800;
      IR_1_Base = $01C21C00;
      IIS_1_Base = $01C22000;
      IIS_0_Base = $01C22400;
      LRADC_0_1_Base = $01C22800;
      AD_DA_Base = $01C22C00;
      Keypad_Base = $01C23000;
      SID_Base = $01C23800;
      SJTAG_Base = $01C23C00;
      IIS_2_Base = $01C24400;
      TP_Base = $01C25000;
      PMU_Base = $01C25400;
      CPU_Configuration_Base = $01C25C00;
      UART_0_Base = $01C28000;
      UART_1_Base = $01C28400;
      UART_2_Base = $01C28800;
      UART_3_Base = $01C28C00;
      UART_4_Base = $01C29000;
      UART_5_Base = $01C29400;
      UART_6_Base = $01C29800;
      UART_7_Base = $01C29C00;
      PS2_0_Base = $01C2A000;
      PS2_1_Base = $01C2A400;
      TWI_0_Base = $01C2AC00;
      TWI_1_Base = $01C2B000;
      TWI_2_Base = $01C2B400;
      TWI_3_Base = $01C2B800;
      CAN_Base = $01C2BC00;
      TWI_4_Base = $01C2C000;
      Smart_Card_Reader_Base = $01C2C400;
      GPS_Base = $01C30000;
      Mali400_Base = $01C40000;
      GMAC_Base = $01C50000;
      HSTIMER_Base = $01C60000;
      GIC_Registers_Base = $01C80000;
      HDMI1_Base = $01CE0000;
      CPUBIST_Base = $3F501000;
      SRAM_C_Base = $01D00000;
      DE_FE0_Base = $01E00000;
      DE_FE1_Base = $01E20000;
      DE_BE0_Base = $01E60000;
      DE_BE1_Base = $01E40000;
      MP_Base = $01E80000;
      AVG_Base = $01EA0000;
      CoreSight_Debug_Module_Base = $3F500000;
      DDR_Base = $40000000;
      BROM_Base = $FFFF0000;

  implementation

    procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';
      asm
      .Lhalt:
        b .Lhalt
      end;

{$ifndef CUSTOM_ENTRY}
    procedure PASCALMAIN; external name 'PASCALMAIN';

    var
      _stack_top: record end; external name '_stack_top';
      
    procedure _FPC_start; assembler; nostackframe;
      label
        _start;
      asm
        .init
        .align 16
        .globl _start
      _start:
        ldr r0,.L_stack_top
        bl PASCALMAIN
        bl _FPC_haltproc
      .L_stack_top:
        .long _stack_top
        .text
      end;
{$endif CUSTOM_ENTRY}

end.

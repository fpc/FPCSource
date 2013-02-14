{******************************************************************************
lpc13XX.h - Register defs for Philips LPC13XX series MPU


THE SOFTWARE IS DELIVERED "AS IS" WITHOUT WARRANTY OR CONDITION OF ANY KIND,
EITHER EXPRESS, IMPLIED OR STATUTORY. THIS INCLUDES WITHOUT LIMITATION ANY
WARRANTY OR CONDITION WITH RESPECT TO MERCHANTABILITY OR FITNESS FOR ANY
PARTICULAR PURPOSE, OR AGAINST THE INFRINGEMENTS OF INTELLECTUAL PROPERTY RIGHTS
OF OTHERS.

This file may be freely used for commercial and non-commercial applications,
including being redistributed with any tools.

If you find a problem with the file, please report it so that it can be fixed.

You can contact me directly @ - delphian (at) smythconsulting (dot) net
}
unit lpc1343;

{$goto on}
{$define lpc1343}

interface

{$PACKRECORDS 2}

{*##############################################################################
## System Control Block
##############################################################################*}

var
  SCB_BASE_ADDRESS : dword absolute $40048000;// System control block base address
  SCB_MEMREMAP : dword absolute $40048000;// System memory remap
  SCB_PRESETCTRL : dword absolute $40048004;// Peripheral reset control
  SCB_PLLCTRL : dword absolute $40048008;// System PLL control
  SCB_PLLSTAT : dword absolute $4004800C;// System PLL status
  SCB_USBPLLCTRL : dword absolute $40048010;// USB PLL control
  SCB_USBPLLSTAT : dword absolute $40048014;// USB PLL status
  SCB_SYSOSCCTRL : dword absolute $40048020;// System oscillator control
  SCB_WDTOSCCTRL : dword absolute $40048024;// Watchdog oscillator control
  SCB_IRCCTRL : dword absolute $40048028;// IRC control
  SCB_RESETSTAT : dword absolute $40048030;// System reset status register
  SCB_PLLCLKSEL : dword absolute $40048040;// System PLL clock source select
  SCB_PLLCLKUEN : dword absolute $40048044;// System PLL clock source update enable
  SCB_USBPLLCLKSEL : dword absolute $40048048;// USB PLL clock source select
  SCB_USBPLLCLKUEN : dword absolute $4004804C;// USB PLL clock source update enable
  SCB_MAINCLKSEL : dword absolute $40048070;// Main clock source select
  SCB_MAINCLKUEN : dword absolute $40048074;// Main clock source update enable
  SCB_SYSAHBCLKDIV : dword absolute $40048078;// System AHB clock divider
  SCB_SYSAHBCLKCTRL : dword absolute $40048080;// System AHB clock control
  SCB_SSP0CLKDIV : dword absolute $40048094;// SSP0 clock divider
  SCB_UARTCLKDIV : dword absolute $40048098;// UART clock divider
  SCB_SYSTICKCLKDIV : dword absolute $400480B0;// System tick clock divider
  SCB_USBCLKSEL : dword absolute $400480C0;// USB clock source select
  SCB_USBCLKUEN : dword absolute $400480C4;// USB clock source update enable
  SCB_USBCLKDIV : dword absolute $400480C8;// USB clock divider
  SCB_WDTCLKSEL : dword absolute $400480D0;// Watchdog clock source select
  SCB_WDTCLKUEN : dword absolute $400480D4;// Watchdog clock source update enable
  SCB_WDTCLKDIV : dword absolute $400480D8;// Watchdog clock divider
  SCB_CLKOUTCLKSEL : dword absolute $400480E0;// CLKOUT clock source select
  SCB_CLKOUTCLKUEN : dword absolute $400480E4;// CLKOUT clock source update enable
  SCB_CLKOUTCLKDIV : dword absolute $400480E8;// CLKOUT clock divider
  SCB_PIOPORCAP0 : dword absolute $40048100;// POR captured PIO status 0
  SCB_PIOPORCAP1 : dword absolute $40048104;// POR captured PIO status 1
  SCB_BODCTRL : dword absolute $40048150;// Brown-out detector control
  SCB_SYSTICKCCAL : dword absolute $40048158;// System tick counter calibration
  SCB_STARTAPRP0 : dword absolute $40048200;// Start logic edge control register 0; bottom 32 interrupts
  SCB_STARTERP0 : dword absolute $40048204;// Start logic signal enable register 0; bottom 32 interrupts
  SCB_STARTRSRP0CLR : dword absolute $40048208;// Start logic reset register 0; bottom 32 interrupts
  SCB_STARTSRP0 : dword absolute $4004820C;// Start logic status register 0; bottom 32 interrupts
  SCB_STARTAPRP1 : dword absolute $40048210;// Start logic edge control register 1; top 8 interrupts
  SCB_STARTERP1 : dword absolute $40048214;// Start logic signal enable register 1; top 8 interrupts
  SCB_STARTRSRP1CLR : dword absolute $40048218;// Start logic reset register 1; top 8 interrupts
  SCB_STARTSRP1 : dword absolute $4004821C;// Start logic status register 1; top 8 interrupts
  SCB_PDSLEEPCFG : dword absolute $40048230;// Power-down states in Deep-sleep mode
  SCB_PDAWAKECFG : dword absolute $40048234;// Power-down states after wake-up from Deep-sleep mode
  SCB_PDRUNCFG : dword absolute $40048238;// Power-down configuration register
  SCB_DEVICEID : dword absolute $400483F4;// Device ID
  SCB_MMFAR : dword absolute $E000ED34;// Memory Manage Address Register (MMAR)
  SCB_BFAR : dword absolute $E000ED38;// Bus Fault Manage Address Register (BFAR)
  SCB_DEMCR : dword absolute $E000EDFC;

(* CPU ID Base Register *)
  SCB_CPUID : dword absolute $E000ED00;
  SCB_CPUID_REVISION_MASK : dword absolute $0000000F;// Revision Code
  SCB_CPUID_PARTNO_MASK : dword absolute $0000FFF0;// Part Number
  SCB_CPUID_CONSTANT_MASK : dword absolute $000F0000;// Constant
  SCB_CPUID_VARIANT_MASK : dword absolute $00F00000;// Variant
  SCB_CPUID_IMPLEMENTER_MASK : dword absolute $FF000000;// Implementer

(*System Control Register *)

  SCB_SCR : dword absolute $E000ED10;
  SCB_SCR_SLEEPONEXIT_MASK : dword absolute $00000002;// Enable sleep on exit
  SCB_SCR_SLEEPONEXIT : dword absolute $00000002;
  SCB_SCR_SLEEPDEEP_MASK : dword absolute $00000004;
  SCB_SCR_SLEEPDEEP : dword absolute $00000004;// Enable deep sleep
  SCB_SCR_SEVONPEND_MASK : dword absolute $00000010;// Wake up from WFE is new int is pended regardless of priority
  SCB_SCR_SEVONPEND : dword absolute $00000010;

(*Application Interrupt and Reset Control Register *)

  SCB_AIRCR : dword absolute $E000ED0C;
  SCB_AIRCR_VECTKEY_VALUE : dword absolute $05FA0000;// Vect key needs to be set to 05FA for reset to work
  SCB_AIRCR_VECTKEY_MASK : dword absolute $FFFF0000;
  SCB_AIRCR_ENDIANESS : dword absolute $00008000;// Read Endianness (1=Big, 0=Little)
  SCB_AIRCR_ENDIANESS_MASK : dword absolute $00008000;
  SCB_AIRCR_PRIGROUP : dword absolute $00000700;
  SCB_AIRCR_PRIGROUP_MASK : dword absolute $00000700;
  SCB_AIRCR_SYSRESETREQ : dword absolute $00000004;// Request system reset
  SCB_AIRCR_SYSRESETREQ_MASK : dword absolute $00000004;
  SCB_AIRCR_VECTCLRACTIVE : dword absolute $00000002;// Used to prevent accidental reset
  SCB_AIRCR_VECTCLRACTIVE_MASK : dword absolute $00000002;
  SCB_AIRCR_VECTRESET : dword absolute $00000001;
  SCB_AIRCR_VECTRESET_MASK : dword absolute $00000001;

(*Memory Management Fault Status Register *)

  SCB_MMFSR : dword absolute $E000ED28;
  SCB_MMFSR_IACCVIOL_MASK : dword absolute $00000001;// Instruction access violation
  SCB_MMFSR_IACCVIOL : dword absolute $00000001;
  SCB_MMFSR_DACCVIOL_MASK : dword absolute $00000002;// Data access violation
  SCB_MMFSR_DACCVIOL : dword absolute $00000002;
  SCB_MMFSR_MUNSTKERR_MASK : dword absolute $00000008;// Unstacking error
  SCB_MMFSR_MUNSTKERR : dword absolute $00000008;
  SCB_MMFSR_MSTKERR_MASK : dword absolute $00000010;// Stacking error
  SCB_MMFSR_MSTKERR : dword absolute $00000010;
  SCB_MMFSR_MMARVALID_MASK : dword absolute $00000080;// Indicates MMAR is valid
  SCB_MMFSR_MMARVALID : dword absolute $00000080;

(*Bus Fault Status Register *)

  SCB_BFSR : dword absolute $E000ED29;
  SCB_BFSR_IBUSERR_MASK : dword absolute $00000001;// Instruction access violation
  SCB_BFSR_IBUSERR : dword absolute $00000001;
  SCB_BFSR_PRECISERR_MASK : dword absolute $00000002;// Precise data access violation
  SCB_BFSR_PRECISERR : dword absolute $00000002;
  SCB_BFSR_IMPRECISERR_MASK : dword absolute $00000004;// Imprecise data access violation
  SCB_BFSR_IMPRECISERR : dword absolute $00000004;
  SCB_BFSR_UNSTKERR_MASK : dword absolute $00000008;// Unstacking error
  SCB_BFSR_UNSTKERR : dword absolute $00000008;
  SCB_BFSR_STKERR_MASK : dword absolute $00000010;// Stacking error
  SCB_BFSR_STKERR : dword absolute $00000010;
  SCB_BFSR_BFARVALID_MASK : dword absolute $00000080;// Indicates BFAR is valid
  SCB_BFSR_BFARVALID : dword absolute $00000080;

(*Usage Fault Status Register *)

  SCB_UFSR : dword absolute $E000ED2A;
  SCB_UFSR_UNDEFINSTR_MASK : dword absolute $00000001;// Attempt to execute an undefined instruction
  SCB_UFSR_UNDEFINSTR : dword absolute $00000001;
  SCB_UFSR_INVSTATE_MASK : dword absolute $00000002;// Attempt to switch to invalid state (i.e. ARM)
  SCB_UFSR_INVSTATE : dword absolute $00000002;
  SCB_UFSR_INVPC_MASK : dword absolute $00000004;// Attempt to do exception with bad value in EXC_RETURN number
  SCB_UFSR_INVPC : dword absolute $00000004;
  SCB_UFSR_NOCP_MASK : dword absolute $00000008;// Attempt to execute a coprocessor instruction
  SCB_UFSR_NOCP : dword absolute $00000008;
  SCB_UFSR_UNALIGNED_MASK : dword absolute $00000100;// Unaligned access
  SCB_UFSR_UNALIGNED : dword absolute $00000100;
  SCB_UFSR_DIVBYZERO_MASK : dword absolute $00000200;// Divide by zero
  SCB_UFSR_DIVBYZERO : dword absolute $00000200;

(*Hard Fault Status Register *)

  SCB_HFSR : dword absolute $E000ED2C;
  SCB_HFSR_VECTTBL_MASK : dword absolute $00000002;// Hard fault caused by failed vector fetch
  SCB_HFSR_VECTTBL : dword absolute $00000002;
  SCB_HFSR_FORCED_MASK : dword absolute $40000000;// Hard fault taken because of bus/mem man/usage fault
  SCB_HFSR_FORCED : dword absolute $40000000;
  SCB_HFSR_DEBUGEVT_MASK : dword absolute $80000000;// Hard fault triggered by debug event
  SCB_HFSR_DEBUGEVT : dword absolute $80000000;

(*Debug Fault Status Register *)

  SCB_DFSR : dword absolute $E000ED30;
  SCB_DFSR_HALTED_MASK : dword absolute $00000001;// Halt requested in NVIC
  SCB_DFSR_HALTED : dword absolute $00000001;
  SCB_DFSR_BKPT_MASK : dword absolute $00000002;// BKPT instruction executed
  SCB_DFSR_BKPT : dword absolute $00000002;
  SCB_DFSR_DWTTRAP_MASK : dword absolute $00000004;// DWT match occurred
  SCB_DFSR_DWTTRAP : dword absolute $00000004;
  SCB_DFSR_VCATCH_MASK : dword absolute $00000008;// Vector fetch occurred
  SCB_DFSR_VCATCH : dword absolute $00000008;
  SCB_DFSR_EXTERNAL_MASK : dword absolute $00000010;// EDBGRQ signal asserted
  SCB_DFSR_EXTERNAL : dword absolute $00000010;

(*SCB_MEMREMAP (System memory remap register)
//The system memory remap register selects whether the ARM interrupt vectors are read
//from the boot ROM, the flash, or the SRAM. *)

  SCB_MEMREMAP_MODE_BOOTLOADER : dword absolute $00000000;// Interrupt vectors are remapped to Boot ROM
  SCB_MEMREMAP_MODE_RAM : dword absolute $00000001;// Interrupt vectors are remapped to Static ROM
  SCB_MEMREMAP_MODE_FLASH : dword absolute $00000002;// Interrupt vectors are not remapped and reside in Flash
  SCB_MEMREMAP_MASK : dword absolute $00000003;

(*PRESETCTRL (Peripheral reset control register) *)

  SCB_PRESETCTRL_SSP0_RESETENABLED : dword absolute $00000000;
  SCB_PRESETCTRL_SSP0_RESETDISABLED : dword absolute $00000001;
  SCB_PRESETCTRL_SSP0_MASK : dword absolute $00000001;
  SCB_PRESETCTRL_I2C_RESETENABLED : dword absolute $00000000;
  SCB_PRESETCTRL_I2C_RESETDISABLED : dword absolute $00000002;
  SCB_PRESETCTRL_I2C_MASK : dword absolute $00000002;

(*SYSPLLCTRL (System PLL control register)
//This register connects and enables the system PLL and configures the PLL multiplier and
//divider values. The PLL accepts an input frequency from 10 MHz to 25 MHz from various
//clock sources. The input frequency is multiplied up to a high frequency, then divided down
//to provide the actual clock used by the CPU, peripherals, and optionally the USB
//subsystem. Note that the USB subsystem has its own dedicated PLL. The PLL can
//produce a clock up to the maximum allowed for the CPU, which is 72 MHz. *)

  SCB_PLLCTRL_MSEL_1 : dword absolute $00000000;
  SCB_PLLCTRL_MSEL_2 : dword absolute $00000001;
  SCB_PLLCTRL_MSEL_3 : dword absolute $00000002;
  SCB_PLLCTRL_MSEL_4 : dword absolute $00000003;
  SCB_PLLCTRL_MSEL_5 : dword absolute $00000004;
  SCB_PLLCTRL_MSEL_6 : dword absolute $00000005;
  SCB_PLLCTRL_MSEL_7 : dword absolute $00000006;
  SCB_PLLCTRL_MSEL_8 : dword absolute $00000007;
  SCB_PLLCTRL_MSEL_9 : dword absolute $00000008;
  SCB_PLLCTRL_MSEL_10 : dword absolute $00000009;
  SCB_PLLCTRL_MSEL_11 : dword absolute $0000000A;
  SCB_PLLCTRL_MSEL_12 : dword absolute $0000000B;
  SCB_PLLCTRL_MSEL_13 : dword absolute $0000000C;
  SCB_PLLCTRL_MSEL_14 : dword absolute $0000000D;
  SCB_PLLCTRL_MSEL_15 : dword absolute $0000000E;
  SCB_PLLCTRL_MSEL_16 : dword absolute $0000000F;
  SCB_PLLCTRL_MSEL_17 : dword absolute $00000010;
  SCB_PLLCTRL_MSEL_18 : dword absolute $00000011;
  SCB_PLLCTRL_MSEL_19 : dword absolute $00000012;
  SCB_PLLCTRL_MSEL_20 : dword absolute $00000013;
  SCB_PLLCTRL_MSEL_21 : dword absolute $00000014;
  SCB_PLLCTRL_MSEL_22 : dword absolute $00000015;
  SCB_PLLCTRL_MSEL_23 : dword absolute $00000016;
  SCB_PLLCTRL_MSEL_24 : dword absolute $00000017;
  SCB_PLLCTRL_MSEL_25 : dword absolute $00000018;
  SCB_PLLCTRL_MSEL_26 : dword absolute $00000019;
  SCB_PLLCTRL_MSEL_27 : dword absolute $0000001A;
  SCB_PLLCTRL_MSEL_28 : dword absolute $0000001B;
  SCB_PLLCTRL_MSEL_29 : dword absolute $0000001C;
  SCB_PLLCTRL_MSEL_30 : dword absolute $0000001D;
  SCB_PLLCTRL_MSEL_31 : dword absolute $0000001E;
  SCB_PLLCTRL_MSEL_32 : dword absolute $0000001F;
  SCB_PLLCTRL_MSEL_MASK : dword absolute $0000001F;
  SCB_PLLCTRL_PSEL_2 : dword absolute $00000000;
  SCB_PLLCTRL_PSEL_4 : dword absolute $00000020;
  SCB_PLLCTRL_PSEL_8 : dword absolute $00000040;
  SCB_PLLCTRL_PSEL_16 : dword absolute $00000060;
  const
    SCB_PLLCTRL_PSEL_BIT = 5;
  var
  SCB_PLLCTRL_PSEL_MASK : dword absolute $00000060;
  SCB_PLLCTRL_DIRECT_MASK : dword absolute $00000080;// Direct CCO clock output control
  SCB_PLLCTRL_BYPASS_MASK : dword absolute $00000100;// Input clock bypass control
  SCB_PLLCTRL_MASK : dword absolute $000001FF;

(*SYSPLLSTAT (System PLL status register)
//This register is a Read-only register and supplies the PLL lock status *)

  SCB_PLLSTAT_LOCK : dword absolute $00000001;// 0 = PLL not locked, 1 = PLL locked
  SCB_PLLSTAT_LOCK_MASK : dword absolute $00000001;

(*USBPLLCTRL (USB PLL control register)
//The USB PLL is identical to the system PLL and is used to provide a dedicated clock to
//the USB block if available. The USB PLL should be always connected to the system
//oscillator to produce a stable USB clock. *)

  SCB_USBPLLCTRL_MULT_1 : dword absolute $00000000;
  SCB_USBPLLCTRL_MULT_2 : dword absolute $00000001;
  SCB_USBPLLCTRL_MULT_3 : dword absolute $00000002;
  SCB_USBPLLCTRL_MULT_4 : dword absolute $00000003;
  SCB_USBPLLCTRL_MULT_5 : dword absolute $00000004;
  SCB_USBPLLCTRL_MULT_6 : dword absolute $00000005;
  SCB_USBPLLCTRL_MULT_7 : dword absolute $00000006;
  SCB_USBPLLCTRL_MULT_8 : dword absolute $00000007;
  SCB_USBPLLCTRL_MULT_9 : dword absolute $00000008;
  SCB_USBPLLCTRL_MULT_10 : dword absolute $00000009;
  SCB_USBPLLCTRL_MULT_11 : dword absolute $0000000A;
  SCB_USBPLLCTRL_MULT_12 : dword absolute $0000000B;
  SCB_USBPLLCTRL_MULT_13 : dword absolute $0000000C;
  SCB_USBPLLCTRL_MULT_14 : dword absolute $0000000D;
  SCB_USBPLLCTRL_MULT_15 : dword absolute $0000000E;
  SCB_USBPLLCTRL_MULT_16 : dword absolute $0000000F;
  SCB_USBPLLCTRL_MULT_17 : dword absolute $00000010;
  SCB_USBPLLCTRL_MULT_18 : dword absolute $00000011;
  SCB_USBPLLCTRL_MULT_19 : dword absolute $00000012;
  SCB_USBPLLCTRL_MULT_20 : dword absolute $00000013;
  SCB_USBPLLCTRL_MULT_21 : dword absolute $00000014;
  SCB_USBPLLCTRL_MULT_22 : dword absolute $00000015;
  SCB_USBPLLCTRL_MULT_23 : dword absolute $00000016;
  SCB_USBPLLCTRL_MULT_24 : dword absolute $00000017;
  SCB_USBPLLCTRL_MULT_25 : dword absolute $00000018;
  SCB_USBPLLCTRL_MULT_26 : dword absolute $00000019;
  SCB_USBPLLCTRL_MULT_27 : dword absolute $0000001A;
  SCB_USBPLLCTRL_MULT_28 : dword absolute $0000001B;
  SCB_USBPLLCTRL_MULT_29 : dword absolute $0000001C;
  SCB_USBPLLCTRL_MULT_30 : dword absolute $0000001D;
  SCB_USBPLLCTRL_MULT_31 : dword absolute $0000001E;
  SCB_USBPLLCTRL_MULT_32 : dword absolute $0000001F;
  SCB_USBPLLCTRL_MULT_MASK : dword absolute $0000001F;
  SCB_USBPLLCTRL_DIV_2 : dword absolute $00000000;
  SCB_USBPLLCTRL_DIV_4 : dword absolute $00000020;
  SCB_USBPLLCTRL_DIV_8 : dword absolute $00000040;
  SCB_USBPLLCTRL_DIV_16 : dword absolute $00000060;
const
  SCB_USBPLLCTRL_DIV_BIT = 5;
var
  SCB_USBPLLCTRL_DIV_MASK : dword absolute $00000060;
  SCB_USBPLLCTRL_DIRECT_MASK : dword absolute $00000080;// Direct CCO clock output control
  SCB_USBPLLCTRL_BYPASS_MASK : dword absolute $00000100;// Input clock bypass control
  SCB_USBPLLCTRL_MASK : dword absolute $000001FF;

(*USBPLLSTAT (System PLL status register)
//This register is a Read-only register and supplies the PLL lock status. *)

  SCB_USBPLLSTAT_LOCK : dword absolute $00000001;// 0 = PLL not locked, 1 = PLL locked
  SCB_USBPLLSTAT_LOCK_MASK : dword absolute $00000001;

(*SYSOSCCTRL (System oscillator control register)
//This register configures the frequency range for the system oscillator. *)

  SCB_SYSOSCCTRL_BYPASS_DISABLED : dword absolute $00000000;// Oscillator is not bypassed.
  SCB_SYSOSCCTRL_BYPASS_ENABLED : dword absolute $00000001;// Bypass enabled
  SCB_SYSOSCCTRL_BYPASS_MASK : dword absolute $00000001;
  SCB_SYSOSCCTRL_FREQRANGE_1TO20MHZ : dword absolute $00000000;// 1-20 MHz frequency range
  SCB_SYSOSCCTRL_FREQRANGE_15TO25MHZ : dword absolute $00000002;// 15-25 MHz frequency range
  SCB_SYSOSCCTRL_FREQRANGE_MASK : dword absolute $00000002;

(*WDTOSCTRL (Watchdog oscillator control register)
//This register configures the watchdog oscillator. The oscillator consists of an analog and a
//digital part. The analog part contains the oscillator function and generates an analog clock
//(Fclkana). With the digital part, the analog output clock (Fclkana) can be divided to the
//required output clock frequency wdt_osc_clk. The analog output frequency (Fclkana) can
//be adjusted with the FREQSEL bits between 500 kHz and 3.7 MHz. With the digital part
//Fclkana will be divided (divider ratios = 2, 4,...,64) to wdt_osc_clk using the DIVSEL bits.*)

  SCB_WDTOSCCTRL_DIVSEL_DIV2 : dword absolute $00000000;// Reset value
  SCB_WDTOSCCTRL_DIVSEL_DIV4 : dword absolute $00000001;
  SCB_WDTOSCCTRL_DIVSEL_DIV6 : dword absolute $00000002;
  SCB_WDTOSCCTRL_DIVSEL_DIV8 : dword absolute $00000003;
  SCB_WDTOSCCTRL_DIVSEL_DIV10 : dword absolute $00000004;
  SCB_WDTOSCCTRL_DIVSEL_DIV12 : dword absolute $00000005;
  SCB_WDTOSCCTRL_DIVSEL_DIV14 : dword absolute $00000006;
  SCB_WDTOSCCTRL_DIVSEL_DIV16 : dword absolute $00000007;
  SCB_WDTOSCCTRL_DIVSEL_DIV18 : dword absolute $00000008;
  SCB_WDTOSCCTRL_DIVSEL_DIV20 : dword absolute $00000009;
  SCB_WDTOSCCTRL_DIVSEL_DIV22 : dword absolute $0000000A;
  SCB_WDTOSCCTRL_DIVSEL_DIV24 : dword absolute $0000000B;
  SCB_WDTOSCCTRL_DIVSEL_DIV26 : dword absolute $0000000C;
  SCB_WDTOSCCTRL_DIVSEL_DIV28 : dword absolute $0000000D;
  SCB_WDTOSCCTRL_DIVSEL_DIV30 : dword absolute $0000000E;
  SCB_WDTOSCCTRL_DIVSEL_DIV32 : dword absolute $0000000F;
  SCB_WDTOSCCTRL_DIVSEL_DIV34 : dword absolute $00000010;
  SCB_WDTOSCCTRL_DIVSEL_DIV36 : dword absolute $00000011;
  SCB_WDTOSCCTRL_DIVSEL_DIV38 : dword absolute $00000012;
  SCB_WDTOSCCTRL_DIVSEL_DIV40 : dword absolute $00000013;
  SCB_WDTOSCCTRL_DIVSEL_DIV42 : dword absolute $00000014;
  SCB_WDTOSCCTRL_DIVSEL_DIV44 : dword absolute $00000015;
  SCB_WDTOSCCTRL_DIVSEL_DIV46 : dword absolute $00000016;
  SCB_WDTOSCCTRL_DIVSEL_DIV48 : dword absolute $00000017;
  SCB_WDTOSCCTRL_DIVSEL_DIV50 : dword absolute $00000018;
  SCB_WDTOSCCTRL_DIVSEL_DIV52 : dword absolute $00000019;
  SCB_WDTOSCCTRL_DIVSEL_DIV54 : dword absolute $0000001A;
  SCB_WDTOSCCTRL_DIVSEL_DIV56 : dword absolute $0000001B;
  SCB_WDTOSCCTRL_DIVSEL_DIV58 : dword absolute $0000001C;
  SCB_WDTOSCCTRL_DIVSEL_DIV60 : dword absolute $0000001D;
  SCB_WDTOSCCTRL_DIVSEL_DIV62 : dword absolute $0000001E;
  SCB_WDTOSCCTRL_DIVSEL_DIV64 : dword absolute $0000001F;
  SCB_WDTOSCCTRL_DIVSEL_MASK : dword absolute $0000001F;
  SCB_WDTOSCCTRL_FREQSEL_0_5MHZ : dword absolute $00000020;
  SCB_WDTOSCCTRL_FREQSEL_0_8MHZ : dword absolute $00000040;
  SCB_WDTOSCCTRL_FREQSEL_1_1MHZ : dword absolute $00000060;
  SCB_WDTOSCCTRL_FREQSEL_1_4MHZ : dword absolute $00000080;
  SCB_WDTOSCCTRL_FREQSEL_1_6MHZ : dword absolute $000000A0;// Reset value
  SCB_WDTOSCCTRL_FREQSEL_1_8MHZ : dword absolute $000000C0;
  SCB_WDTOSCCTRL_FREQSEL_2_0MHZ : dword absolute $000000E0;
  SCB_WDTOSCCTRL_FREQSEL_2_2MHZ : dword absolute $00000100;
  SCB_WDTOSCCTRL_FREQSEL_2_4MHZ : dword absolute $00000120;
  SCB_WDTOSCCTRL_FREQSEL_2_6MHZ : dword absolute $00000140;
  SCB_WDTOSCCTRL_FREQSEL_2_7MHZ : dword absolute $00000160;
  SCB_WDTOSCCTRL_FREQSEL_2_9MHZ : dword absolute $00000180;
  SCB_WDTOSCCTRL_FREQSEL_3_1MHZ : dword absolute $000001A0;
  SCB_WDTOSCCTRL_FREQSEL_3_2MHZ : dword absolute $000001C0;
  SCB_WDTOSCCTRL_FREQSEL_3_4MHZ : dword absolute $000001E0;
  SCB_WDTOSCCTRL_FREQSEL_MASK : dword absolute $000001E0;

(*IRCCTRL (Internal resonant crystal control register)
//This register is used to trim the on-chip 12 MHz oscillator. The trim value is factory-preset
//and written by the boot code on start-up. *)

  SCB_IRCCTRL_MASK : dword absolute $000000FF;

(*SYSRSTSTAT (System reset status register)
//The SYSRSTSTAT register shows the source of the latest reset event. The bits are
//cleared by writing a one to any of the bits. The POR event clears all other bits in this
//register, but if another reset signal (e.g., EXTRST) remains asserted after the POR signal
//is negated, then its bit is set to detected. *)

  SCB_RESETSTAT_POR_MASK : dword absolute $00000001;// POR reset status
  SCB_RESETSTAT_EXTRST_MASK : dword absolute $00000002;// Status of the external reset pin
  SCB_RESETSTAT_WDT_MASK : dword absolute $00000004;// Status of the watchdog reset
  SCB_RESETSTAT_BOD_MASK : dword absolute $00000008;// Status of the brown-out detect reset
  SCB_RESETSTAT_SYSRST_MASK : dword absolute $00000010;// Status of the software system reset
  SCB_RESETSTAT_MASK : dword absolute $00000010;

(*SYSPLLCLKSEL (System PLL clock source select register)
//This register selects the clock source for the system PLL. The SYSPLLCLKUEN register
//must be toggled from LOW to HIGH for the update to take effect.
//Remark: The system oscillator must be selected if the system PLL is used to generate a
//48 MHz clock to the USB block. *)

  SCB_CLKSEL_SOURCE_INTERNALOSC : dword absolute $00000000;
  SCB_CLKSEL_SOURCE_MAINOSC : dword absolute $00000001;
  SCB_CLKSEL_SOURCE_RTCOSC : dword absolute $00000002;
  SCB_CLKSEL_SOURCE_MASK : dword absolute $00000002;

(*SYSPLLUEN (System PLL clock source update enable register)
//This register updates the clock source of the system PLL with the new input clock after the
//SYSPLLCLKSEL register has been written to. In order for the update to take effect, first
//write a zero to the SYSPLLUEN register and then write a one to SYSPLLUEN. *)

  SCB_PLLCLKUEN_DISABLE : dword absolute $00000000;
  SCB_PLLCLKUEN_UPDATE : dword absolute $00000001;
  SCB_PLLCLKUEN_MASK : dword absolute $00000001;

(*USBPLLCLKSEL (USB PLL clock source select register)
//This register selects the clock source for the dedicated USB PLL. The SYSPLLCLKUEN
//register must be toggled from LOW to HIGH for the update to take effect.
//Remark: Always select the system oscillator to produce a stable 48 MHz clock for
//the USB block. *)

  SCB_USBPLLCLKSEL_SOURCE_INTERNALOSC : dword absolute $00000000;// Do NOT use (even though this is the default value)
  SCB_USBPLLCLKSEL_SOURCE_MAINOSC : dword absolute $00000001;// Main oscillator should always be used for USB clock
  SCB_USBPLLCLKSEL_SOURCE_MASK : dword absolute $00000002;

(*USBPLLUEN (USB PLL clock source update enable register)
//This register updates the clock source of the USB PLL with the new input clock after the
//USBPLLCLKSEL register has been written to. In order for the update to take effect at the
//USB PLL input, first write a zero to the USBPLLUEN register and then write a one to
//USBPLLUEN. *)

  SCB_USBPLLCLKUEN_DISABLE : dword absolute $00000000;
  SCB_USBPLLCLKUEN_UPDATE : dword absolute $00000001;
  SCB_USBPLLCLKUEN_MASK : dword absolute $00000001;

(*MAINCLKSEL (Main clock source select register)
//This register selects the main system clock which can be either the output from the
//system PLL or the IRC, system, or Watchdog oscillators directly. The main system clock
//clocks the core, the peripherals, and optionally the USB block.
//The MAINCLKUEN register must be toggled from LOW to HIGH for the update to take effect.*)

  SCB_MAINCLKSEL_SOURCE_INTERNALOSC : dword absolute $00000000;// Use IRC oscillator for main clock source
  SCB_MAINCLKSEL_SOURCE_INPUTCLOCK : dword absolute $00000001;// Use Input clock to system PLL for main clock source
  SCB_MAINCLKSEL_SOURCE_WDTOSC : dword absolute $00000002;// Use watchdog oscillator for main clock source
  SCB_MAINCLKSEL_SOURCE_SYSPLLCLKOUT : dword absolute $00000003;// Use system PLL clock out for main clock source
  SCB_MAINCLKSEL_MASK : dword absolute $00000003;

(*MAINCLKUEN (Main clock source update enable register)
//This register updates the clock source of the main clock with the new input clock after the
//MAINCLKSEL register has been written to. In order for the update to take effect, first write
//a zero to the MAINUEN register and then write a one to MAINCLKUEN. *)

  SCB_MAINCLKUEN_DISABLE : dword absolute $00000000;
  SCB_MAINCLKUEN_UPDATE : dword absolute $00000001;
  SCB_MAINCLKUEN_MASK : dword absolute $00000001;

(*SYSAHBCLKDIV (System AHB clock divider register)
//This register divides the main clock to provide the system clock to the core, memories,
//and the peripherals. The system clock can be shut down completely by setting the DIV
//bits to 0x0. *)

  SCB_SYSAHBCLKDIV_DISABLE : dword absolute $00000000;// 0 will shut the system clock down completely
  SCB_SYSAHBCLKDIV_DIV1 : dword absolute $00000001;// 1, 2 or 4 are the most common values
  SCB_SYSAHBCLKDIV_DIV2 : dword absolute $00000002;
  SCB_SYSAHBCLKDIV_DIV4 : dword absolute $00000004;
  SCB_SYSAHBCLKDIV_MASK : dword absolute $000000FF;// AHB clock divider can be from 0 to 255

(*AHBCLKCTRL (System AHB clock control register)
//The AHBCLKCTRL register enables the clocks to individual system and peripheral blocks.
//The system clock (sys_ahb_clk[0], bit 0 in the AHBCLKCTRL register) provides the clock
//for the AHB to APB bridge, the AHB matrix, the ARM Cortex-M3, the Syscon block, and
//the PMU. This clock cannot be disabled. *)

  SCB_SYSAHBCLKCTRL_SYS : dword absolute $00000001;// Enables clock for AHB and APB bridges, FCLK, HCLK, SysCon and PMU
  SCB_SYSAHBCLKCTRL_SYS_MASK : dword absolute $00000001;
  SCB_SYSAHBCLKCTRL_ROM : dword absolute $00000002;// Enables clock for ROM
  SCB_SYSAHBCLKCTRL_ROM_MASK : dword absolute $00000002;
  SCB_SYSAHBCLKCTRL_RAM : dword absolute $00000004;// Enables clock for SRAM
  SCB_SYSAHBCLKCTRL_RAM_MASK : dword absolute $00000004;
  SCB_SYSAHBCLKCTRL_FLASH1 : dword absolute $00000008;// Enables clock for flash1
  SCB_SYSAHBCLKCTRL_FLASH1_MASK : dword absolute $00000008;
  SCB_SYSAHBCLKCTRL_FLASH2 : dword absolute $00000010;// Enables clock for flash2
  SCB_SYSAHBCLKCTRL_FLASH2_MASK : dword absolute $00000010;
  SCB_SYSAHBCLKCTRL_I2C : dword absolute $00000020;// Enables clock for I2C
  SCB_SYSAHBCLKCTRL_I2C_MASK : dword absolute $00000020;
  SCB_SYSAHBCLKCTRL_GPIO : dword absolute $00000040;// Enables clock for GPIO
  SCB_SYSAHBCLKCTRL_GPIO_MASK : dword absolute $00000040;
  SCB_SYSAHBCLKCTRL_CT16B0 : dword absolute $00000080;// Enables clock for 16-bit counter/timer 0
  SCB_SYSAHBCLKCTRL_CT16B0_MASK : dword absolute $00000080;
  SCB_SYSAHBCLKCTRL_CT16B1 : dword absolute $00000100;// Enables clock for 16-bit counter/timer 1
  SCB_SYSAHBCLKCTRL_CT16B1_MASK : dword absolute $00000100;
  SCB_SYSAHBCLKCTRL_CT32B0 : dword absolute $00000200;// Enables clock for 32-bit counter/timer 0
  SCB_SYSAHBCLKCTRL_CT32B0_MASK : dword absolute $00000200;
  SCB_SYSAHBCLKCTRL_CT32B1 : dword absolute $00000400;// Enables clock for 32-bit counter/timer 1
  SCB_SYSAHBCLKCTRL_CT32B1_MASK : dword absolute $00000400;
  SCB_SYSAHBCLKCTRL_SSP0 : dword absolute $00000800;// Enables clock for SSP0
  SCB_SYSAHBCLKCTRL_SSP0_MASK : dword absolute $00000800;
  SCB_SYSAHBCLKCTRL_UART : dword absolute $00001000;// Enables clock for UART. UART pins must be configured
  SCB_SYSAHBCLKCTRL_UART_MASK : dword absolute $00001000;// in the IOCON block before the UART clock can be enabled.
  SCB_SYSAHBCLKCTRL_ADC : dword absolute $00002000;// Enables clock for ADC
  SCB_SYSAHBCLKCTRL_ADC_MASK : dword absolute $00002000;
  SCB_SYSAHBCLKCTRL_USB_REG : dword absolute $00004000;// Enables clock for USB_REG
  SCB_SYSAHBCLKCTRL_USB_REG_MASK : dword absolute $00004000;
  SCB_SYSAHBCLKCTRL_WDT : dword absolute $00008000;// Enables clock for watchdog timer
  SCB_SYSAHBCLKCTRL_WDT_MASK : dword absolute $00008000;
  SCB_SYSAHBCLKCTRL_IOCON : dword absolute $00010000;// Enables clock for IO configuration block
  SCB_SYSAHBCLKCTRL_IOCON_MASK : dword absolute $00010000;
  SCB_SYSAHBCLKCTRL_ALL_MASK : dword absolute $0001FFFF;

(*SSP0CLKDIV (SSP0 clock divider register)
//This register configures the SSP0 peripheral clock SSP_PCLK. The SSP_PCLK can be
//shut down by setting the DIV bits to 0x0. It can be set from 1..255. *)

  SCB_SSP0CLKDIV_DISABLE : dword absolute $00000000;
  SCB_SSP0CLKDIV_DIV1 : dword absolute $00000001;// Divide SSP0 clock by 1 (can be set from 1..255)
  SCB_SSP0CLKDIV_DIV2 : dword absolute $00000002;
  SCB_SSP0CLKDIV_DIV3 : dword absolute $00000003;
  SCB_SSP0CLKDIV_DIV4 : dword absolute $00000004;
  SCB_SSP0CLKDIV_DIV6 : dword absolute $00000006;
  SCB_SSP0CLKDIV_DIV10 : dword absolute $0000000A;
  SCB_SSP0CLKDIV_DIV12 : dword absolute $0000000C;
  SCB_SSP0CLKDIV_DIV20 : dword absolute $00000014;
  SCB_SSP0CLKDIV_DIV40 : dword absolute $00000028;
  SCB_SSP0CLKDIV_MASK : dword absolute $000000FF;

(*UARTCLKDIV (UART clock divider register)
//This register configures the UART peripheral. The UART_PCLK can be shut down by
//setting the DIV bits to 0x0.
//Remark: Note that the UART pins must be configured in the IOCON block before the
//UART clock can be enabled. *)

  SCB_UARTCLKDIV_DISABLE : dword absolute $00000000;
  SCB_UARTCLKDIV_DIV1 : dword absolute $00000001;// Divide UART clock by 1 (can be set from 1..255)
  SCB_UARTCLKDIV_DIV2 : dword absolute $00000002;
  SCB_UARTCLKDIV_DIV4 : dword absolute $00000004;
  SCB_UARTCLKDIV_MASK : dword absolute $000000FF;

(*SYSTICKCLKDIV (SYSTICK clock divider register)
//This register configures the SYSTICK peripheral clock. The SYSTICK timer clock can be
//shut down by setting the DIV bits to 0x0. *)

  SCB_SYSTICKCLKDIV_DISABLE : dword absolute $00000000;
  SCB_SYSTICKCLKDIV_DIV1 : dword absolute $00000001;// Divide SYSTICK clock by 1 (can be set from 1..255)
  SCB_SYSTICKCLKDIV_DIV2 : dword absolute $00000002;// Divide SYSTICK clock by 2
  SCB_SYSTICKCLKDIV_DIV4 : dword absolute $00000004;// Divide SYSTICK clock by 4
  SCB_SYSTICKCLKDIV_DIV8 : dword absolute $00000008;// Divide SYSTICK clock by 8
  SCB_SYSTICKCLKDIV_MASK : dword absolute $000000FF;

(*USBCLKSEL (USB clock source select register)
//This register selects the clock source for the USB usb_clk. The clock source can be either
//the USB PLL output or the main clock, and the clock can be further divided by the
//USBCLKDIV register to obtain a 48 MHz clock. The USBCLKUEN register must be toggled from
//LOW to HIGH for the update to take effect. *)

  SCB_USBCLKSEL_SOURCE_USBPLLOUT : dword absolute $00000000;// USB PLL output
  SCB_USBCLKSEL_SOURCE_INPUTCLOCK : dword absolute $00000001;// Use the main clock
  SCB_USBCLKSEL_MASK : dword absolute $00000003;

(*USBCLKUEN (USB clock source update enable register)
//This register updates the clock source of the USB with the new input clock after the
//USBCLKSEL register has been written to. In order for the update to take effect, first write
//a zero to the USBCLKUEN register and then write a one to USBCLKUEN. *)

  SCB_USBCLKUEN_DISABLE : dword absolute $00000000;
  SCB_USBCLKUEN_UPDATE : dword absolute $00000001;
  SCB_USBCLKUEN_MASK : dword absolute $00000001;

(*USBCLKDIV (USB clock divider register)
//This register allows the USB clock usb_clk to be divided to 48 MHz. The usb_clk can be
//shut down by setting the DIV bits to 0x0. *)

  SCB_USBCLKDIV_DISABLE : dword absolute $00000000;
  SCB_USBCLKDIV_DIV1 : dword absolute $00000001;// Divide USB clock by 1 (can be set from 1..255)
  SCB_USBCLKDIV_MASK : dword absolute $000000FF;

(*WDTCLKSEL (WDT clock source select register)
//This register selects the clock source for the watchdog timer. The WDTCLKUEN register
//must be toggled from LOW to HIGH for the update to take effect. *)

  SCB_WDTCLKSEL_SOURCE_INTERNALOSC : dword absolute $00000000;// Use the internal oscillator
  SCB_WDTCLKSEL_SOURCE_INPUTCLOCK : dword absolute $00000001;// Use the main clock
  SCB_WDTCLKSEL_SOURCE_WATCHDOGOSC : dword absolute $00000002;// Use the watchdog oscillator
  SCB_WDTCLKSEL_MASK : dword absolute $00000003;

(*WDTCLKUEN (WDT clock source update enable register)
//This register updates the clock source of the watchdog timer with the new input clock after
//the WDTCLKSEL register has been written to. In order for the update to take effect at the
//input of the watchdog timer, first write a zero to the WDTCLKUEN register and then write
//a one to WDTCLKUEN. *)

  SCB_WDTCLKUEN_DISABLE : dword absolute $00000000;
  SCB_WDTCLKUEN_UPDATE : dword absolute $00000001;
  SCB_WDTCLKUEN_MASK : dword absolute $00000001;

(*WDTCLKDIV (WDT clock divider register)
//This register determines the divider values for the watchdog clock wdt_clk. *)

  SCB_WDTCLKDIV_DISABLE : dword absolute $00000000;
  SCB_WDTCLKDIV_DIV1 : dword absolute $00000001;// Divide clock by 1 (can be set from 1..255)
  SCB_WDTCLKDIV_MASK : dword absolute $000000FF;

(*CLKOUTCLKSEL (CLKOUT clock source select register)
//This register configures the clkout_clk signal to be output on the CLKOUT pin. All three
//oscillators and the main clock can be selected for the clkout_clk clock.
//The CLKOUTCLKUEN register must be toggled from LOW to HIGH for the update to take effect. *)

  SCB_CLKOUTCLKSEL_SOURCE_USBPLLOUT : dword absolute $00000000;// USB PLL output
  SCB_CLKOUTCLKSEL_SOURCE_INPUTCLOCK : dword absolute $00000001;// Use the main clock
  SCB_CLKOUTCLKSEL_MASK : dword absolute $00000003;

(*CLKOUTUEN (CLKOUT clock source update enable register)
//This register updates the clock source of the CLKOUT pin with the new clock after the
//CLKOUTCLKSEL register has been written to. In order for the update to take effect at the
//input of the CLKOUT pin, first write a zero to the CLKCLKUEN register and then write a
//one to CLKCLKUEN. *)

  SCB_CLKOUTCLKUEN_DISABLE : dword absolute $00000000;
  SCB_CLKOUTCLKUEN_UPDATE : dword absolute $00000001;
  SCB_CLKOUTCLKUEN_MASK : dword absolute $00000001;

(*CLKOUTCLKDIV (CLKOUT clock divider register)
//This register determines the divider value for the clkout_clk signal on the CLKOUT pin. *)

  SCB_CLKOUTCLKDIV_DISABLE : dword absolute $00000000;
  SCB_CLKOUTCLKDIV_DIV1 : dword absolute $00000001;// Divide clock by 1 (can be set from 1..255)
  SCB_CLKOUTCLKDIV_MASK : dword absolute $000000FF;


(*PIOPORCAP0 (POR captured PIO status register 0)
//The PIOPORCAP0 register captures the state (HIGH or LOW) of the PIO pins of ports 0,1,
//and 2 (pins PIO2_0 to PIO2_7) at power-on-reset. Each bit represents the reset state of
//one GPIO pin. This register is a read-only status register. *)

  SCB_PIOPORCAP0_PIO0_0 : dword absolute $00000001;
  SCB_PIOPORCAP0_PIO0_0_MASK : dword absolute $00000001;
  SCB_PIOPORCAP0_PIO0_1 : dword absolute $00000002;
  SCB_PIOPORCAP0_PIO0_1_MASK : dword absolute $00000002;
  SCB_PIOPORCAP0_PIO0_2 : dword absolute $00000004;
  SCB_PIOPORCAP0_PIO0_2_MASK : dword absolute $00000004;
  SCB_PIOPORCAP0_PIO0_3 : dword absolute $00000008;
  SCB_PIOPORCAP0_PIO0_3_MASK : dword absolute $00000008;
  SCB_PIOPORCAP0_PIO0_4 : dword absolute $00000010;
  SCB_PIOPORCAP0_PIO0_4_MASK : dword absolute $00000010;
  SCB_PIOPORCAP0_PIO0_5 : dword absolute $00000020;
  SCB_PIOPORCAP0_PIO0_5_MASK : dword absolute $00000020;
  SCB_PIOPORCAP0_PIO0_6 : dword absolute $00000040;
  SCB_PIOPORCAP0_PIO0_6_MASK : dword absolute $00000040;
  SCB_PIOPORCAP0_PIO0_7 : dword absolute $00000080;
  SCB_PIOPORCAP0_PIO0_7_MASK : dword absolute $00000080;
  SCB_PIOPORCAP0_PIO0_8 : dword absolute $00000100;
  SCB_PIOPORCAP0_PIO0_8_MASK : dword absolute $00000100;
  SCB_PIOPORCAP0_PIO0_9 : dword absolute $00000200;
  SCB_PIOPORCAP0_PIO0_9_MASK : dword absolute $00000200;
  SCB_PIOPORCAP0_PIO0_10 : dword absolute $00000400;
  SCB_PIOPORCAP0_PIO0_10_MASK : dword absolute $00000400;
  SCB_PIOPORCAP0_PIO0_11 : dword absolute $00000800;
  SCB_PIOPORCAP0_PIO0_11_MASK : dword absolute $00000800;
  SCB_PIOPORCAP0_PIO1_0 : dword absolute $00001000;
  SCB_PIOPORCAP0_PIO1_0_MASK : dword absolute $00001000;
  SCB_PIOPORCAP0_PIO1_1 : dword absolute $00002000;
  SCB_PIOPORCAP0_PIO1_1_MASK : dword absolute $00002000;
  SCB_PIOPORCAP0_PIO1_2 : dword absolute $00004000;
  SCB_PIOPORCAP0_PIO1_2_MASK : dword absolute $00004000;
  SCB_PIOPORCAP0_PIO1_3 : dword absolute $00008000;
  SCB_PIOPORCAP0_PIO1_3_MASK : dword absolute $00008000;
  SCB_PIOPORCAP0_PIO1_4 : dword absolute $00010000;
  SCB_PIOPORCAP0_PIO1_4_MASK : dword absolute $00010000;
  SCB_PIOPORCAP0_PIO1_5 : dword absolute $00020000;
  SCB_PIOPORCAP0_PIO1_5_MASK : dword absolute $00020000;
  SCB_PIOPORCAP0_PIO1_6 : dword absolute $00040000;
  SCB_PIOPORCAP0_PIO1_6_MASK : dword absolute $00040000;
  SCB_PIOPORCAP0_PIO1_7 : dword absolute $00080000;
  SCB_PIOPORCAP0_PIO1_7_MASK : dword absolute $00080000;
  SCB_PIOPORCAP0_PIO1_8 : dword absolute $00100000;
  SCB_PIOPORCAP0_PIO1_8_MASK : dword absolute $00100000;
  SCB_PIOPORCAP0_PIO1_9 : dword absolute $00200000;
  SCB_PIOPORCAP0_PIO1_9_MASK : dword absolute $00200000;
  SCB_PIOPORCAP0_PIO1_10 : dword absolute $00400000;
  SCB_PIOPORCAP0_PIO1_10_MASK : dword absolute $00400000;
  SCB_PIOPORCAP0_PIO1_11 : dword absolute $00800000;
  SCB_PIOPORCAP0_PIO1_11_MASK : dword absolute $00800000;
  SCB_PIOPORCAP0_PIO2_0 : dword absolute $01000000;
  SCB_PIOPORCAP0_PIO2_0_MASK : dword absolute $01000000;
  SCB_PIOPORCAP0_PIO2_1 : dword absolute $02000000;
  SCB_PIOPORCAP0_PIO2_1_MASK : dword absolute $02000000;
  SCB_PIOPORCAP0_PIO2_2 : dword absolute $04000000;
  SCB_PIOPORCAP0_PIO2_2_MASK : dword absolute $04000000;
  SCB_PIOPORCAP0_PIO2_3 : dword absolute $08000000;
  SCB_PIOPORCAP0_PIO2_3_MASK : dword absolute $08000000;
  SCB_PIOPORCAP0_PIO2_4 : dword absolute $10000000;
  SCB_PIOPORCAP0_PIO2_4_MASK : dword absolute $10000000;
  SCB_PIOPORCAP0_PIO2_5 : dword absolute $20000000;
  SCB_PIOPORCAP0_PIO2_5_MASK : dword absolute $20000000;
  SCB_PIOPORCAP0_PIO2_6 : dword absolute $40000000;
  SCB_PIOPORCAP0_PIO2_6_MASK : dword absolute $40000000;
  SCB_PIOPORCAP0_PIO2_7 : dword absolute $80000000;
  SCB_PIOPORCAP0_PIO2_7_MASK : dword absolute $80000000;

(*PIOPORCAP1 (POR captured PIO status register 1)
//The PIOPORCAP1 register captures the state (HIGH or LOW) of the PIO pins of port 2
//(PIO2_8 to PIO2_11) and port 3 at power-on-reset. Each bit represents the reset state of
//one PIO pin. This register is a read-only status register. *)

  SCB_PIOPORCAP1_PIO2_8 : dword absolute $00000001;
  SCB_PIOPORCAP1_PIO2_8_MASK : dword absolute $00000001;
  SCB_PIOPORCAP1_PIO2_9 : dword absolute $00000002;
  SCB_PIOPORCAP1_PIO2_9_MASK : dword absolute $00000002;
  SCB_PIOPORCAP1_PIO2_10 : dword absolute $00000004;
  SCB_PIOPORCAP1_PIO2_10_MASK : dword absolute $00000004;
  SCB_PIOPORCAP1_PIO2_11 : dword absolute $00000008;
  SCB_PIOPORCAP1_PIO2_11_MASK : dword absolute $00000008;
  SCB_PIOPORCAP1_PIO3_0 : dword absolute $00000010;
  SCB_PIOPORCAP1_PIO3_0_MASK : dword absolute $00000010;
  SCB_PIOPORCAP1_PIO3_1 : dword absolute $00000020;
  SCB_PIOPORCAP1_PIO3_1_MASK : dword absolute $00000020;
  SCB_PIOPORCAP1_PIO3_2 : dword absolute $00000040;
  SCB_PIOPORCAP1_PIO3_2_MASK : dword absolute $00000040;
  SCB_PIOPORCAP1_PIO3_3 : dword absolute $00000080;
  SCB_PIOPORCAP1_PIO3_3_MASK : dword absolute $00000080;
  SCB_PIOPORCAP1_PIO3_4 : dword absolute $00000100;
  SCB_PIOPORCAP1_PIO3_4_MASK : dword absolute $00000100;
  SCB_PIOPORCAP1_PIO3_5 : dword absolute $00000200;
  SCB_PIOPORCAP1_PIO3_5_MASK : dword absolute $00000200;

(*BODCTRL (Brown-out detection control register)
//The BOD control register selects four separate threshold values for sending a BOD
//interrupt to the NVIC. Only one level is allowed for forced reset. *)

  SCB_BODCTRL_RSTLEVEL_MASK : dword absolute $00000003;
  SCB_BODCTRL_INTLEVEL_1_69V_1_84V : dword absolute $00000000;
  SCB_BODCTRL_INTLEVEL_2_29V_2_44V : dword absolute $00000004;
  SCB_BODCTRL_INTLEVEL_2_59V_2_74V : dword absolute $00000008;
  SCB_BODCTRL_INTLEVEL_2_87V_2_98V : dword absolute $0000000C;
  SCB_BODCTRL_INTLEVEL_MASK : dword absolute $0000000C;
  SCB_BODCTRL_RSTENABLE_DISABLE : dword absolute $00000000;
  SCB_BODCTRL_RSTENABLE_ENABLE : dword absolute $00000010;
  SCB_BODCTRL_RSTENABLE_MASK : dword absolute $00000010;

(*SYSTCKCAL (System tick counter calibration register) *)

  SCB_SYSTICKCCAL_MASK : dword absolute $03FFFFFF;// Undefined as of v0.07 of the LPC1343 User Manual

(*STARTAPRP0 (Start logic edge control register 0)
//The STARTAPRP0 register controls the start logic inputs of ports 0 (PIO0_0 to PIO0_11)
//and 1 (PIO1_0 to PIO1_11) and the lower 8 inputs of port 2 (PIO2_0 to PIO2_7). This
//register selects a falling or rising edge on the corresponding PIO input to produce a falling
//or rising clock edge, respectively, for the start logic (see Section 3Â?9.3).
//Every bit in the STARTAPRP0 register controls one port input and is connected to one
//wake-up interrupt in the NVIC. Bit 0 in the STARTAPRP0 register corresponds to interrupt
//0, bit 1 to interrupt 1, etc.. The bottom 32 interrupts are contained this register,
//the top 8 interrupts are contained in the STARTAPRP1 register for total of 40 wake-up
//interrupts.
//Remark: Each interrupt connected to a start logic input must be enabled in the NVIC if the
//corresponding PIO pin is used to wake up the chip from Deep-sleep mode. *)

  SCB_STARTAPRP0_APRPIO0_0 : dword absolute $00000001;
  SCB_STARTAPRP0_APRPIO0_0_MASK : dword absolute $00000001;
  SCB_STARTAPRP0_APRPIO0_1 : dword absolute $00000002;
  SCB_STARTAPRP0_APRPIO0_1_MASK : dword absolute $00000002;
  SCB_STARTAPRP0_APRPIO0_2 : dword absolute $00000004;
  SCB_STARTAPRP0_APRPIO0_2_MASK : dword absolute $00000004;
  SCB_STARTAPRP0_APRPIO0_3 : dword absolute $00000008;
  SCB_STARTAPRP0_APRPIO0_3_MASK : dword absolute $00000008;
  SCB_STARTAPRP0_APRPIO0_4 : dword absolute $00000010;
  SCB_STARTAPRP0_APRPIO0_4_MASK : dword absolute $00000010;
  SCB_STARTAPRP0_APRPIO0_5 : dword absolute $00000020;
  SCB_STARTAPRP0_APRPIO0_5_MASK : dword absolute $00000020;
  SCB_STARTAPRP0_APRPIO0_6 : dword absolute $00000040;
  SCB_STARTAPRP0_APRPIO0_6_MASK : dword absolute $00000040;
  SCB_STARTAPRP0_APRPIO0_7 : dword absolute $00000080;
  SCB_STARTAPRP0_APRPIO0_7_MASK : dword absolute $00000080;
  SCB_STARTAPRP0_APRPIO0_8 : dword absolute $00000100;
  SCB_STARTAPRP0_APRPIO0_8_MASK : dword absolute $00000100;
  SCB_STARTAPRP0_APRPIO0_9 : dword absolute $00000200;
  SCB_STARTAPRP0_APRPIO0_9_MASK : dword absolute $00000200;
  SCB_STARTAPRP0_APRPIO0_10 : dword absolute $00000400;
  SCB_STARTAPRP0_APRPIO0_10_MASK : dword absolute $00000400;
  SCB_STARTAPRP0_APRPIO0_11 : dword absolute $00000800;
  SCB_STARTAPRP0_APRPIO0_11_MASK : dword absolute $00000800;
  SCB_STARTAPRP0_APRPIO1_0 : dword absolute $00001000;
  SCB_STARTAPRP0_APRPIO1_0_MASK : dword absolute $00001000;
  SCB_STARTAPRP0_APRPIO1_1 : dword absolute $00002000;
  SCB_STARTAPRP0_APRPIO1_1_MASK : dword absolute $00002000;
  SCB_STARTAPRP0_APRPIO1_2 : dword absolute $00004000;
  SCB_STARTAPRP0_APRPIO1_2_MASK : dword absolute $00004000;
  SCB_STARTAPRP0_APRPIO1_3 : dword absolute $00008000;
  SCB_STARTAPRP0_APRPIO1_3_MASK : dword absolute $00008000;
  SCB_STARTAPRP0_APRPIO1_4 : dword absolute $00010000;
  SCB_STARTAPRP0_APRPIO1_4_MASK : dword absolute $00010000;
  SCB_STARTAPRP0_APRPIO1_5 : dword absolute $00020000;
  SCB_STARTAPRP0_APRPIO1_5_MASK : dword absolute $00020000;
  SCB_STARTAPRP0_APRPIO1_6 : dword absolute $00040000;
  SCB_STARTAPRP0_APRPIO1_6_MASK : dword absolute $00040000;
  SCB_STARTAPRP0_APRPIO1_7 : dword absolute $00080000;
  SCB_STARTAPRP0_APRPIO1_7_MASK : dword absolute $00080000;
  SCB_STARTAPRP0_APRPIO1_8 : dword absolute $00100000;
  SCB_STARTAPRP0_APRPIO1_8_MASK : dword absolute $00100000;
  SCB_STARTAPRP0_APRPIO1_9 : dword absolute $00200000;
  SCB_STARTAPRP0_APRPIO1_9_MASK : dword absolute $00200000;
  SCB_STARTAPRP0_APRPIO1_10 : dword absolute $00400000;
  SCB_STARTAPRP0_APRPIO1_10_MASK : dword absolute $00400000;
  SCB_STARTAPRP0_APRPIO1_11 : dword absolute $00800000;
  SCB_STARTAPRP0_APRPIO1_11_MASK : dword absolute $00800000;
  SCB_STARTAPRP0_APRPIO2_0 : dword absolute $01000000;
  SCB_STARTAPRP0_APRPIO2_0_MASK : dword absolute $01000000;
  SCB_STARTAPRP0_APRPIO2_1 : dword absolute $02000000;
  SCB_STARTAPRP0_APRPIO2_1_MASK : dword absolute $02000000;
  SCB_STARTAPRP0_APRPIO2_2 : dword absolute $04000000;
  SCB_STARTAPRP0_APRPIO2_2_MASK : dword absolute $04000000;
  SCB_STARTAPRP0_APRPIO2_3 : dword absolute $08000000;
  SCB_STARTAPRP0_APRPIO2_3_MASK : dword absolute $08000000;
  SCB_STARTAPRP0_APRPIO2_4 : dword absolute $10000000;
  SCB_STARTAPRP0_APRPIO2_4_MASK : dword absolute $10000000;
  SCB_STARTAPRP0_APRPIO2_5 : dword absolute $20000000;
  SCB_STARTAPRP0_APRPIO2_5_MASK : dword absolute $20000000;
  SCB_STARTAPRP0_APRPIO2_6 : dword absolute $40000000;
  SCB_STARTAPRP0_APRPIO2_6_MASK : dword absolute $40000000;
  SCB_STARTAPRP0_APRPIO2_7 : dword absolute $80000000;
  SCB_STARTAPRP0_APRPIO2_7_MASK : dword absolute $80000000;
  SCB_STARTAPRP0_MASK : dword absolute $FFFFFFFF;

(*STARTERP0 (Start logic signal enable register 0)
//This STARTERP0 register enables or disables the start signal bits in the start logic. *)

  SCB_STARTERP0_ERPIO0_0 : dword absolute $00000001;
  SCB_STARTERP0_ERPIO0_0_MASK : dword absolute $00000001;
  SCB_STARTERP0_ERPIO0_1 : dword absolute $00000002;
  SCB_STARTERP0_ERPIO0_1_MASK : dword absolute $00000002;
  SCB_STARTERP0_ERPIO0_2 : dword absolute $00000004;
  SCB_STARTERP0_ERPIO0_2_MASK : dword absolute $00000004;
  SCB_STARTERP0_ERPIO0_3 : dword absolute $00000008;
  SCB_STARTERP0_ERPIO0_3_MASK : dword absolute $00000008;
  SCB_STARTERP0_ERPIO0_4 : dword absolute $00000010;
  SCB_STARTERP0_ERPIO0_4_MASK : dword absolute $00000010;
  SCB_STARTERP0_ERPIO0_5 : dword absolute $00000020;
  SCB_STARTERP0_ERPIO0_5_MASK : dword absolute $00000020;
  SCB_STARTERP0_ERPIO0_6 : dword absolute $00000040;
  SCB_STARTERP0_ERPIO0_6_MASK : dword absolute $00000040;
  SCB_STARTERP0_ERPIO0_7 : dword absolute $00000080;
  SCB_STARTERP0_ERPIO0_7_MASK : dword absolute $00000080;
  SCB_STARTERP0_ERPIO0_8 : dword absolute $00000100;
  SCB_STARTERP0_ERPIO0_8_MASK : dword absolute $00000100;
  SCB_STARTERP0_ERPIO0_9 : dword absolute $00000200;
  SCB_STARTERP0_ERPIO0_9_MASK : dword absolute $00000200;
  SCB_STARTERP0_ERPIO0_10 : dword absolute $00000400;
  SCB_STARTERP0_ERPIO0_10_MASK : dword absolute $00000400;
  SCB_STARTERP0_ERPIO0_11 : dword absolute $00000800;
  SCB_STARTERP0_ERPIO0_11_MASK : dword absolute $00000800;
  SCB_STARTERP0_ERPIO1_0 : dword absolute $00001000;
  SCB_STARTERP0_ERPIO1_0_MASK : dword absolute $00001000;
  SCB_STARTERP0_ERPIO1_1 : dword absolute $00002000;
  SCB_STARTERP0_ERPIO1_1_MASK : dword absolute $00002000;
  SCB_STARTERP0_ERPIO1_2 : dword absolute $00004000;
  SCB_STARTERP0_ERPIO1_2_MASK : dword absolute $00004000;
  SCB_STARTERP0_ERPIO1_3 : dword absolute $00008000;
  SCB_STARTERP0_ERPIO1_3_MASK : dword absolute $00008000;
  SCB_STARTERP0_ERPIO1_4 : dword absolute $00010000;
  SCB_STARTERP0_ERPIO1_4_MASK : dword absolute $00010000;
  SCB_STARTERP0_ERPIO1_5 : dword absolute $00020000;
  SCB_STARTERP0_ERPIO1_5_MASK : dword absolute $00020000;
  SCB_STARTERP0_ERPIO1_6 : dword absolute $00040000;
  SCB_STARTERP0_ERPIO1_6_MASK : dword absolute $00040000;
  SCB_STARTERP0_ERPIO1_7 : dword absolute $00080000;
  SCB_STARTERP0_ERPIO1_7_MASK : dword absolute $00080000;
  SCB_STARTERP0_ERPIO1_8 : dword absolute $00100000;
  SCB_STARTERP0_ERPIO1_8_MASK : dword absolute $00100000;
  SCB_STARTERP0_ERPIO1_9 : dword absolute $00200000;
  SCB_STARTERP0_ERPIO1_9_MASK : dword absolute $00200000;
  SCB_STARTERP0_ERPIO1_10 : dword absolute $00400000;
  SCB_STARTERP0_ERPIO1_10_MASK : dword absolute $00400000;
  SCB_STARTERP0_ERPIO1_11 : dword absolute $00800000;
  SCB_STARTERP0_ERPIO1_11_MASK : dword absolute $00800000;
  SCB_STARTERP0_ERPIO2_0 : dword absolute $01000000;
  SCB_STARTERP0_ERPIO2_0_MASK : dword absolute $01000000;
  SCB_STARTERP0_ERPIO2_1 : dword absolute $02000000;
  SCB_STARTERP0_ERPIO2_1_MASK : dword absolute $02000000;
  SCB_STARTERP0_ERPIO2_2 : dword absolute $04000000;
  SCB_STARTERP0_ERPIO2_2_MASK : dword absolute $04000000;
  SCB_STARTERP0_ERPIO2_3 : dword absolute $08000000;
  SCB_STARTERP0_ERPIO2_3_MASK : dword absolute $08000000;
  SCB_STARTERP0_ERPIO2_4 : dword absolute $10000000;
  SCB_STARTERP0_ERPIO2_4_MASK : dword absolute $10000000;
  SCB_STARTERP0_ERPIO2_5 : dword absolute $20000000;
  SCB_STARTERP0_ERPIO2_5_MASK : dword absolute $20000000;
  SCB_STARTERP0_ERPIO2_6 : dword absolute $40000000;
  SCB_STARTERP0_ERPIO2_6_MASK : dword absolute $40000000;
  SCB_STARTERP0_ERPIO2_7 : dword absolute $80000000;
  SCB_STARTERP0_ERPIO2_7_MASK : dword absolute $80000000;
  SCB_STARTERP0_MASK : dword absolute $FFFFFFFF;

(*STARTRSRP0CLR (Start logic reset register 0)
//Writing a one to a bit in the STARTRSRP0CLR register resets the start logic state. The
//start-up logic uses the input signals to generate a clock edge for registering a start
//signal. This clock edge (falling or rising) sets the interrupt for waking up from
//Deep-sleep mode. Therefore, the start-up logic states must be cleared before being used. *)

  SCB_STARTRSRP0CLR_RSRPIO0_0 : dword absolute $00000001;
  SCB_STARTRSRP0CLR_RSRPIO0_0_MASK : dword absolute $00000001;
  SCB_STARTRSRP0CLR_RSRPIO0_1 : dword absolute $00000002;
  SCB_STARTRSRP0CLR_RSRPIO0_1_MASK : dword absolute $00000002;
  SCB_STARTRSRP0CLR_RSRPIO0_2 : dword absolute $00000004;
  SCB_STARTRSRP0CLR_RSRPIO0_2_MASK : dword absolute $00000004;
  SCB_STARTRSRP0CLR_RSRPIO0_3 : dword absolute $00000008;
  SCB_STARTRSRP0CLR_RSRPIO0_3_MASK : dword absolute $00000008;
  SCB_STARTRSRP0CLR_RSRPIO0_4 : dword absolute $00000010;
  SCB_STARTRSRP0CLR_RSRPIO0_4_MASK : dword absolute $00000010;
  SCB_STARTRSRP0CLR_RSRPIO0_5 : dword absolute $00000020;
  SCB_STARTRSRP0CLR_RSRPIO0_5_MASK : dword absolute $00000020;
  SCB_STARTRSRP0CLR_RSRPIO0_6 : dword absolute $00000040;
  SCB_STARTRSRP0CLR_RSRPIO0_6_MASK : dword absolute $00000040;
  SCB_STARTRSRP0CLR_RSRPIO0_7 : dword absolute $00000080;
  SCB_STARTRSRP0CLR_RSRPIO0_7_MASK : dword absolute $00000080;
  SCB_STARTRSRP0CLR_RSRPIO0_8 : dword absolute $00000100;
  SCB_STARTRSRP0CLR_RSRPIO0_8_MASK : dword absolute $00000100;
  SCB_STARTRSRP0CLR_RSRPIO0_9 : dword absolute $00000200;
  SCB_STARTRSRP0CLR_RSRPIO0_9_MASK : dword absolute $00000200;
  SCB_STARTRSRP0CLR_RSRPIO0_10 : dword absolute $00000400;
  SCB_STARTRSRP0CLR_RSRPIO0_10_MASK : dword absolute $00000400;
  SCB_STARTRSRP0CLR_RSRPIO0_11 : dword absolute $00000800;
  SCB_STARTRSRP0CLR_RSRPIO0_11_MASK : dword absolute $00000800;
  SCB_STARTRSRP0CLR_RSRPIO1_0 : dword absolute $00001000;
  SCB_STARTRSRP0CLR_RSRPIO1_0_MASK : dword absolute $00001000;
  SCB_STARTRSRP0CLR_RSRPIO1_1 : dword absolute $00002000;
  SCB_STARTRSRP0CLR_RSRPIO1_1_MASK : dword absolute $00002000;
  SCB_STARTRSRP0CLR_RSRPIO1_2 : dword absolute $00004000;
  SCB_STARTRSRP0CLR_RSRPIO1_2_MASK : dword absolute $00004000;
  SCB_STARTRSRP0CLR_RSRPIO1_3 : dword absolute $00008000;
  SCB_STARTRSRP0CLR_RSRPIO1_3_MASK : dword absolute $00008000;
  SCB_STARTRSRP0CLR_RSRPIO1_4 : dword absolute $00010000;
  SCB_STARTRSRP0CLR_RSRPIO1_4_MASK : dword absolute $00010000;
  SCB_STARTRSRP0CLR_RSRPIO1_5 : dword absolute $00020000;
  SCB_STARTRSRP0CLR_RSRPIO1_5_MASK : dword absolute $00020000;
  SCB_STARTRSRP0CLR_RSRPIO1_6 : dword absolute $00040000;
  SCB_STARTRSRP0CLR_RSRPIO1_6_MASK : dword absolute $00040000;
  SCB_STARTRSRP0CLR_RSRPIO1_7 : dword absolute $00080000;
  SCB_STARTRSRP0CLR_RSRPIO1_7_MASK : dword absolute $00080000;
  SCB_STARTRSRP0CLR_RSRPIO1_8 : dword absolute $00100000;
  SCB_STARTRSRP0CLR_RSRPIO1_8_MASK : dword absolute $00100000;
  SCB_STARTRSRP0CLR_RSRPIO1_9 : dword absolute $00200000;
  SCB_STARTRSRP0CLR_RSRPIO1_9_MASK : dword absolute $00200000;
  SCB_STARTRSRP0CLR_RSRPIO1_10 : dword absolute $00400000;
  SCB_STARTRSRP0CLR_RSRPIO1_10_MASK : dword absolute $00400000;
  SCB_STARTRSRP0CLR_RSRPIO1_11 : dword absolute $00800000;
  SCB_STARTRSRP0CLR_RSRPIO1_11_MASK : dword absolute $00800000;
  SCB_STARTRSRP0CLR_RSRPIO2_0 : dword absolute $01000000;
  SCB_STARTRSRP0CLR_RSRPIO2_0_MASK : dword absolute $01000000;
  SCB_STARTRSRP0CLR_RSRPIO2_1 : dword absolute $02000000;
  SCB_STARTRSRP0CLR_RSRPIO2_1_MASK : dword absolute $02000000;
  SCB_STARTRSRP0CLR_RSRPIO2_2 : dword absolute $04000000;
  SCB_STARTRSRP0CLR_RSRPIO2_2_MASK : dword absolute $04000000;
  SCB_STARTRSRP0CLR_RSRPIO2_3 : dword absolute $08000000;
  SCB_STARTRSRP0CLR_RSRPIO2_3_MASK : dword absolute $08000000;
  SCB_STARTRSRP0CLR_RSRPIO2_4 : dword absolute $10000000;
  SCB_STARTRSRP0CLR_RSRPIO2_4_MASK : dword absolute $10000000;
  SCB_STARTRSRP0CLR_RSRPIO2_5 : dword absolute $20000000;
  SCB_STARTRSRP0CLR_RSRPIO2_5_MASK : dword absolute $20000000;
  SCB_STARTRSRP0CLR_RSRPIO2_6 : dword absolute $40000000;
  SCB_STARTRSRP0CLR_RSRPIO2_6_MASK : dword absolute $40000000;
  SCB_STARTRSRP0CLR_RSRPIO2_7 : dword absolute $80000000;
  SCB_STARTRSRP0CLR_RSRPIO2_7_MASK : dword absolute $80000000;
  SCB_STARTRSRP0CLR_MASK : dword absolute $FFFFFFFF;

(*(Start logic status register 0)
This register reflects the status of the enabled start signal bits. Each bit
(if enabled) reflects the state of the start logic, i.e. whether or not a
wake-up signal has been received for a given pin. *)

  SCB_STARTSRP0_SRPIO0_0 : dword absolute $00000001;
  SCB_STARTSRP0_SRPIO0_0_MASK : dword absolute $00000001;
  SCB_STARTSRP0_SRPIO0_1 : dword absolute $00000002;
  SCB_STARTSRP0_SRPIO0_1_MASK : dword absolute $00000002;
  SCB_STARTSRP0_SRPIO0_2 : dword absolute $00000004;
  SCB_STARTSRP0_SRPIO0_2_MASK : dword absolute $00000004;
  SCB_STARTSRP0_SRPIO0_3 : dword absolute $00000008;
  SCB_STARTSRP0_SRPIO0_3_MASK : dword absolute $00000008;
  SCB_STARTSRP0_SRPIO0_4 : dword absolute $00000010;
  SCB_STARTSRP0_SRPIO0_4_MASK : dword absolute $00000010;
  SCB_STARTSRP0_SRPIO0_5 : dword absolute $00000020;
  SCB_STARTSRP0_SRPIO0_5_MASK : dword absolute $00000020;
  SCB_STARTSRP0_SRPIO0_6 : dword absolute $00000040;
  SCB_STARTSRP0_SRPIO0_6_MASK : dword absolute $00000040;
  SCB_STARTSRP0_SRPIO0_7 : dword absolute $00000080;
  SCB_STARTSRP0_SRPIO0_7_MASK : dword absolute $00000080;
  SCB_STARTSRP0_SRPIO0_8 : dword absolute $00000100;
  SCB_STARTSRP0_SRPIO0_8_MASK : dword absolute $00000100;
  SCB_STARTSRP0_SRPIO0_9 : dword absolute $00000200;
  SCB_STARTSRP0_SRPIO0_9_MASK : dword absolute $00000200;
  SCB_STARTSRP0_SRPIO0_10 : dword absolute $00000400;
  SCB_STARTSRP0_SRPIO0_10_MASK : dword absolute $00000400;
  SCB_STARTSRP0_SRPIO0_11 : dword absolute $00000800;
  SCB_STARTSRP0_SRPIO0_11_MASK : dword absolute $00000800;
  SCB_STARTSRP0_SRPIO1_0 : dword absolute $00001000;
  SCB_STARTSRP0_SRPIO1_0_MASK : dword absolute $00001000;
  SCB_STARTSRP0_SRPIO1_1 : dword absolute $00002000;
  SCB_STARTSRP0_SRPIO1_1_MASK : dword absolute $00002000;
  SCB_STARTSRP0_SRPIO1_2 : dword absolute $00004000;
  SCB_STARTSRP0_SRPIO1_2_MASK : dword absolute $00004000;
  SCB_STARTSRP0_SRPIO1_3 : dword absolute $00008000;
  SCB_STARTSRP0_SRPIO1_3_MASK : dword absolute $00008000;
  SCB_STARTSRP0_SRPIO1_4 : dword absolute $00010000;
  SCB_STARTSRP0_SRPIO1_4_MASK : dword absolute $00010000;
  SCB_STARTSRP0_SRPIO1_5 : dword absolute $00020000;
  SCB_STARTSRP0_SRPIO1_5_MASK : dword absolute $00020000;
  SCB_STARTSRP0_SRPIO1_6 : dword absolute $00040000;
  SCB_STARTSRP0_SRPIO1_6_MASK : dword absolute $00040000;
  SCB_STARTSRP0_SRPIO1_7 : dword absolute $00080000;
  SCB_STARTSRP0_SRPIO1_7_MASK : dword absolute $00080000;
  SCB_STARTSRP0_SRPIO1_8 : dword absolute $00100000;
  SCB_STARTSRP0_SRPIO1_8_MASK : dword absolute $00100000;
  SCB_STARTSRP0_SRPIO1_9 : dword absolute $00200000;
  SCB_STARTSRP0_SRPIO1_9_MASK : dword absolute $00200000;
  SCB_STARTSRP0_SRPIO1_10 : dword absolute $00400000;
  SCB_STARTSRP0_SRPIO1_10_MASK : dword absolute $00400000;
  SCB_STARTSRP0_SRPIO1_11 : dword absolute $00800000;
  SCB_STARTSRP0_SRPIO1_11_MASK : dword absolute $00800000;
  SCB_STARTSRP0_SRPIO2_0 : dword absolute $01000000;
  SCB_STARTSRP0_SRPIO2_0_MASK : dword absolute $01000000;
  SCB_STARTSRP0_SRPIO2_1 : dword absolute $02000000;
  SCB_STARTSRP0_SRPIO2_1_MASK : dword absolute $02000000;
  SCB_STARTSRP0_SRPIO2_2 : dword absolute $04000000;
  SCB_STARTSRP0_SRPIO2_2_MASK : dword absolute $04000000;
  SCB_STARTSRP0_SRPIO2_3 : dword absolute $08000000;
  SCB_STARTSRP0_SRPIO2_3_MASK : dword absolute $08000000;
  SCB_STARTSRP0_SRPIO2_4 : dword absolute $10000000;
  SCB_STARTSRP0_SRPIO2_4_MASK : dword absolute $10000000;
  SCB_STARTSRP0_SRPIO2_5 : dword absolute $20000000;
  SCB_STARTSRP0_SRPIO2_5_MASK : dword absolute $20000000;
  SCB_STARTSRP0_SRPIO2_6 : dword absolute $40000000;
  SCB_STARTSRP0_SRPIO2_6_MASK : dword absolute $40000000;
  SCB_STARTSRP0_SRPIO2_7 : dword absolute $80000000;
  SCB_STARTSRP0_SRPIO2_7_MASK : dword absolute $80000000;
  SCB_STARTSRP0_MASK : dword absolute $FFFFFFFF;


(*STARTAPRP1 (Start logic edge control register 1)
//The STARTAPRP1 register controls the start logic inputs of ports 2 (PIO2_8 to PIO2_11)
//and 3 (PIO3_0 to PIO3_3). This register selects a falling or rising edge on the
//corresponding PIO input to produce a falling or rising clock edge, respectively, for the
//start-up logic.
//Every bit in the STARTAPRP1 register controls one port input and is connected to one
//wake-up interrupt in the NVIC. Bit 0 in the STARTAPRP1 register corresponds to interrupt
//32, bit 1 to interrupt 33, up to bit 7 corresponding to interrupt 39.
//Remark: Each interrupt connected to a start logic input must be enabled in the NVIC if the
//corresponding PIO pin is used to wake up the chip from Deep-sleep mode.*)

  SCB_STARTAPRP1_APRPIO2_8 : dword absolute $00000001;
  SCB_STARTAPRP1_APRPIO2_8_MASK : dword absolute $00000001;
  SCB_STARTAPRP1_APRPIO2_9 : dword absolute $00000002;
  SCB_STARTAPRP1_APRPIO2_9_MASK : dword absolute $00000002;
  SCB_STARTAPRP1_APRPIO2_10 : dword absolute $00000004;
  SCB_STARTAPRP1_APRPIO2_10_MASK : dword absolute $00000004;
  SCB_STARTAPRP1_APRPIO2_11 : dword absolute $00000008;
  SCB_STARTAPRP1_APRPIO2_11_MASK : dword absolute $00000008;
  SCB_STARTAPRP1_APRPIO3_0 : dword absolute $00000010;
  SCB_STARTAPRP1_APRPIO3_0_MASK : dword absolute $00000010;
  SCB_STARTAPRP1_APRPIO3_1 : dword absolute $00000020;
  SCB_STARTAPRP1_APRPIO3_1_MASK : dword absolute $00000020;
  SCB_STARTAPRP1_APRPIO3_2 : dword absolute $00000040;
  SCB_STARTAPRP1_APRPIO3_2_MASK : dword absolute $00000040;
  SCB_STARTAPRP1_APRPIO3_3 : dword absolute $00000080;
  SCB_STARTAPRP1_APRPIO3_3_MASK : dword absolute $00000080;
  SCB_STARTAPRP1_MASK : dword absolute $000000FF;

(*STARTERP1 (Start logic signal enable register 1)
//This STARTERP1 register enables or disables the start signal bits in the start logic. *)

  SCB_STARTERP1_ERPIO2_8 : dword absolute $00000001;
  SCB_STARTERP1_ERPIO2_8_MASK : dword absolute $00000001;
  SCB_STARTERP1_ERPIO2_9 : dword absolute $00000002;
  SCB_STARTERP1_ERPIO2_9_MASK : dword absolute $00000002;
  SCB_STARTERP1_ERPIO2_10 : dword absolute $00000004;
  SCB_STARTERP1_ERPIO2_10_MASK : dword absolute $00000004;
  SCB_STARTERP1_ERPIO2_11 : dword absolute $00000008;
  SCB_STARTERP1_ERPIO2_11_MASK : dword absolute $00000008;
  SCB_STARTERP1_ERPIO3_0 : dword absolute $00000010;
  SCB_STARTERP1_ERPIO3_0_MASK : dword absolute $00000010;
  SCB_STARTERP1_ERPIO3_1 : dword absolute $00000020;
  SCB_STARTERP1_ERPIO3_1_MASK : dword absolute $00000020;
  SCB_STARTERP1_ERPIO3_2 : dword absolute $00000040;
  SCB_STARTERP1_ERPIO3_2_MASK : dword absolute $00000040;
  SCB_STARTERP1_ERPIO3_3 : dword absolute $00000080;
  SCB_STARTERP1_ERPIO3_3_MASK : dword absolute $00000080;
  SCB_STARTERP1_MASK : dword absolute $000000FF;

(*(Start logic reset register 1)
//Writing a one to a bit in the STARTRSRP1CLR register resets the start logic state. The
//start-up logic uses the input signals to generate a clock edge for registering a start
//signal. This clock edge (falling or rising) sets the interrupt for waking up from
//Deep-sleep mode. Therefore, the start-up logic states must be cleared before being used. *)

  SCB_STARTRSRP1CLR_RSRPIO2_8 : dword absolute $00000001;
  SCB_STARTRSRP1CLR_RSRPIO2_8_MASK : dword absolute $00000001;
  SCB_STARTRSRP1CLR_RSRPIO2_9 : dword absolute $00000002;
  SCB_STARTRSRP1CLR_RSRPIO2_9_MASK : dword absolute $00000002;
  SCB_STARTRSRP1CLR_RSRPIO2_10 : dword absolute $00000004;
  SCB_STARTRSRP1CLR_RSRPIO2_10_MASK : dword absolute $00000004;
  SCB_STARTRSRP1CLR_RSRPIO2_11 : dword absolute $00000008;
  SCB_STARTRSRP1CLR_RSRPIO2_11_MASK : dword absolute $00000008;
  SCB_STARTRSRP1CLR_RSRPIO3_0 : dword absolute $00000010;
  SCB_STARTRSRP1CLR_RSRPIO3_0_MASK : dword absolute $00000010;
  SCB_STARTRSRP1CLR_RSRPIO3_1 : dword absolute $00000020;
  SCB_STARTRSRP1CLR_RSRPIO3_1_MASK : dword absolute $00000020;
  SCB_STARTRSRP1CLR_RSRPIO3_2 : dword absolute $00000040;
  SCB_STARTRSRP1CLR_RSRPIO3_2_MASK : dword absolute $00000040;
  SCB_STARTRSRP1CLR_RSRPIO3_3 : dword absolute $00000080;
  SCB_STARTRSRP1CLR_RSRPIO3_3_MASK : dword absolute $00000080;
  SCB_STARTRSRP1CLR_MASK : dword absolute $000000FF;

(*STARTSRP1 (Start logic status register 1)
//This register reflects the status of the enabled start signals. *)

  SCB_STARTSRP1_SRPIO2_8 : dword absolute $00000001;
  SCB_STARTSRP1_SRPIO2_8_MASK : dword absolute $00000001;
  SCB_STARTSRP1_SRPIO2_9 : dword absolute $00000002;
  SCB_STARTSRP1_SRPIO2_9_MASK : dword absolute $00000002;
  SCB_STARTSRP1_SRPIO2_10 : dword absolute $00000004;
  SCB_STARTSRP1_SRPIO2_10_MASK : dword absolute $00000004;
  SCB_STARTSRP1_SRPIO2_11 : dword absolute $00000008;
  SCB_STARTSRP1_SRPIO2_11_MASK : dword absolute $00000008;
  SCB_STARTSRP1_SRPIO3_0 : dword absolute $00000010;
  SCB_STARTSRP1_SRPIO3_0_MASK : dword absolute $00000010;
  SCB_STARTSRP1_SRPIO3_1 : dword absolute $00000020;
  SCB_STARTSRP1_SRPIO3_1_MASK : dword absolute $00000020;
  SCB_STARTSRP1_SRPIO3_2 : dword absolute $00000040;
  SCB_STARTSRP1_SRPIO3_2_MASK : dword absolute $00000040;
  SCB_STARTSRP1_SRPIO3_3 : dword absolute $00000080;
  SCB_STARTSRP1_SRPIO3_3_MASK : dword absolute $00000080;
  SCB_STARTSRP1_MASK : dword absolute $000000FF;

(*PDSLEEPCFG (Deep-sleep mode configuration register)
//The bits in this register can be programmed to indicate the state the chip must enter when
//the Deep-sleep mode is asserted by the ARM. The value of the PDSLEEPCFG register
//will be automatically loaded into the PDRUNCFG register when the Sleep mode is
//entered. *)

  SCB_PDSLEEPCFG_IRCOUT_PD : dword absolute $00000001;
  SCB_PDSLEEPCFG_IRCOUT_PD_MASK : dword absolute $00000001;
  SCB_PDSLEEPCFG_IRC_PD : dword absolute $00000002;
  SCB_PDSLEEPCFG_IRC_PD_MASK : dword absolute $00000002;
  SCB_PDSLEEPCFG_FLASH_PD : dword absolute $00000004;
  SCB_PDSLEEPCFG_FLASH_PD_MASK : dword absolute $00000004;
  SCB_PDSLEEPCFG_BOD_PD : dword absolute $00000008;
  SCB_PDSLEEPCFG_BOD_PD_MASK : dword absolute $00000008;
  SCB_PDSLEEPCFG_ADC_PD : dword absolute $00000010;
  SCB_PDSLEEPCFG_ADC_PD_MASK : dword absolute $00000010;
  SCB_PDSLEEPCFG_SYSOSC_PD : dword absolute $00000020;
  SCB_PDSLEEPCFG_SYSOSC_PD_MASK : dword absolute $00000020;
  SCB_PDSLEEPCFG_WDTOSC_PD : dword absolute $00000040;
  SCB_PDSLEEPCFG_WDTOSC_PD_MASK : dword absolute $00000040;
  SCB_PDSLEEPCFG_SYSPLL_PD : dword absolute $00000080;
  SCB_PDSLEEPCFG_SYSPLL_PD_MASK : dword absolute $00000080;
  SCB_PDSLEEPCFG_USBPLL_PD : dword absolute $00000100;
  SCB_PDSLEEPCFG_USBPLL_PD_MASK : dword absolute $00000100;
  SCB_PDSLEEPCFG_USBPAD_PD : dword absolute $00000400;
  SCB_PDSLEEPCFG_USBPAD_PD_MASK : dword absolute $00000400;

(*PDAWAKECFG (Wake-up configuration register)
The bits in this register can be programmed to indicate the state the chip must enter when
it is waking up from Deep-sleep mode. *)

  SCB_PDAWAKECFG_IRCOUT_PD : dword absolute $00000001;
  SCB_PDAWAKECFG_IRCOUT_PD_MASK : dword absolute $00000001;
  SCB_PDAWAKECFG_IRC_PD : dword absolute $00000002;
  SCB_PDAWAKECFG_IRC_PD_MASK : dword absolute $00000002;
  SCB_PDAWAKECFG_FLASH_PD : dword absolute $00000004;
  SCB_PDAWAKECFG_FLASH_PD_MASK : dword absolute $00000004;
  SCB_PDAWAKECFG_BOD_PD : dword absolute $00000008;
  SCB_PDAWAKECFG_BOD_PD_MASK : dword absolute $00000008;
  SCB_PDAWAKECFG_ADC_PD : dword absolute $00000010;
  SCB_PDAWAKECFG_ADC_PD_MASK : dword absolute $00000010;
  SCB_PDAWAKECFG_SYSOSC_PD : dword absolute $00000020;
  SCB_PDAWAKECFG_SYSOSC_PD_MASK : dword absolute $00000020;
  SCB_PDAWAKECFG_WDTOSC_PD : dword absolute $00000040;
  SCB_PDAWAKECFG_WDTOSC_PD_MASK : dword absolute $00000040;
  SCB_PDAWAKECFG_SYSPLL_PD : dword absolute $00000080;
  SCB_PDAWAKECFG_SYSPLL_PD_MASK : dword absolute $00000080;
  SCB_PDAWAKECFG_USBPLL_PD : dword absolute $00000100;
  SCB_PDAWAKECFG_USBPLL_PD_MASK : dword absolute $00000100;
  SCB_PDAWAKECFG_USBPAD_PD : dword absolute $00000400;
  SCB_PDAWAKECFG_USBPAD_PD_MASK : dword absolute $00000400;

(*PDRUNCFG (Power-down configuration register)
The bits in the PDRUNCFG register control the power to the various analog blocks. This
register can be written to at any time while the chip is running, and a write will take effect
immediately with the exception of the power-down signal to the IRC. Setting a 1 powers-down
a peripheral and 0 enables it. *)

  SCB_PDRUNCFG_IRCOUT : dword absolute $00000001;// IRC oscillator output power-down
  SCB_PDRUNCFG_IRCOUT_MASK : dword absolute $00000001;
  SCB_PDRUNCFG_IRC : dword absolute $00000002;// IRC oscillator power-down
  SCB_PDRUNCFG_IRC_MASK : dword absolute $00000002;
  SCB_PDRUNCFG_FLASH : dword absolute $00000004;// Flash power-down
  SCB_PDRUNCFG_FLASH_MASK : dword absolute $00000004;
  SCB_PDRUNCFG_BOD : dword absolute $00000008;// Brown-out detector power-down
  SCB_PDRUNCFG_BOD_MASK : dword absolute $00000008;
  SCB_PDRUNCFG_ADC : dword absolute $00000010;// ADC power-down
  SCB_PDRUNCFG_ADC_MASK : dword absolute $00000010;
  SCB_PDRUNCFG_SYSOSC : dword absolute $00000020;// System oscillator power-down
  SCB_PDRUNCFG_SYSOSC_MASK : dword absolute $00000020;
  SCB_PDRUNCFG_WDTOSC : dword absolute $00000040;// Watchdog oscillator power-down
  SCB_PDRUNCFG_WDTOSC_MASK : dword absolute $00000040;
  SCB_PDRUNCFG_SYSPLL : dword absolute $00000080;// System PLL power-down
  SCB_PDRUNCFG_SYSPLL_MASK : dword absolute $00000080;
  SCB_PDRUNCFG_USBPLL : dword absolute $00000100;// USB PLL power-down
  SCB_PDRUNCFG_USBPLL_MASK : dword absolute $00000100;
  SCB_PDRUNCFG_USBPAD : dword absolute $00000400;// USB PHY power-down
  SCB_PDRUNCFG_USBPAD_MASK : dword absolute $00000400;

(*DEVICE_ID (Device ID Register)
This device ID register is a read-only register and contains the device ID for each
LPC13xx part. This register is also read by the ISP/IAP commands. *)

  SCB_DEVICEID_LPC1311FHN33 : dword absolute $2C42502B;
  SCB_DEVICEID_LPC1313FHN33 : dword absolute $2C40102B;
  SCB_DEVICEID_LPC1313FBD48 : dword absolute $2C40102B;
  SCB_DEVICEID_LPC1342FHN33 : dword absolute $3D01402B;
  SCB_DEVICEID_LPC1343FHN33 : dword absolute $3D00002B;
  SCB_DEVICEID_LPC1343FBD48 : dword absolute $3D00002B;

(*##############################################################################
## Data Watchpoint and Trace Unit (DWT)
##############################################################################*)
// For more information, see Cortex-M3 Technical Reference Manual 8.3
// This block is optional and not all comparators or functionality may
// be present on all chips, though basic DWT functionality is present
// on the LPC1343 since CYCNT works

  DWT_CTRL : dword absolute $E0001000;// Control register
  DWT_CYCCNT : dword absolute $E0001004;// Cycle counter (useful for rough performance testing)
  DWT_CPICNT : dword absolute $E0001008;// CPI Count Register
  DWT_EXCCNT : dword absolute $E000100C;// Exception overhead count register
  DWT_SLEEPCNT : dword absolute $E0001010;// Sleep count register
  DWT_LSUCNT : dword absolute $E0001014;// LSU count register
  DWT_FOLDCNT : dword absolute $E0001018;// Folder-instruction count register
  DWT_PCSR : dword absolute $E000101C;// Program counter sample register
  DWT_COMP0 : dword absolute $E0001020;// Comparator register 0
  DWT_MASK0 : dword absolute $E0001024;// Mask register 0
  DWT_FUNCTION0 : dword absolute $E0001028;// Function register 0
  DWT_COMP1 : dword absolute $E0001030;// Comparator register 1
  DWT_MASK1 : dword absolute $E0001034;// Mask register 1
  DWT_FUNCTION1 : dword absolute $E0001038;// Function register 1
  DWT_COMP2 : dword absolute $E0001040;// Comparator register 2
  DWT_MASK2 : dword absolute $E0001044;// Mask register 2
  DWT_FUNCTION2 : dword absolute $E0001048;// Function register 2
  DWT_COMP3 : dword absolute $E0001050;// Comparator register 3
  DWT_MASK3 : dword absolute $E0001054;// Mask register 3
  DWT_FUNCTION3 : dword absolute $E0001058;// Function register 3

(*##############################################################################
## Power Management Unit (PMU)
##############################################################################*)

  PMU_BASE_ADDRESS : dword absolute $40038000;

  PMU_PMUCTRL : dword absolute $40038000;// Power control register
  PMU_GPREG0 : dword absolute $40038004;// General purpose register 0
  PMU_GPREG1 : dword absolute $40038008;// General purpose register 1
  PMU_GPREG2 : dword absolute $4003800C;// General purpose register 2
  PMU_GPREG3 : dword absolute $40038010;// General purpose register 3
  PMU_GPREG4 : dword absolute $40038014;// General purpose register 4

  PMU_PMUCTRL_DPDEN_MASK : dword absolute $00000002;// Deep power-down enable
  PMU_PMUCTRL_DPDEN_DEEPPOWERDOWN : dword absolute $00000002;// WFI will enter deep power-down mode
  PMU_PMUCTRL_DPDEN_SLEEP : dword absolute $00000000;// WFI will enter sleep mode
  PMU_PMUCTRL_DPDFLAG_MASK : dword absolute $00000800;// Deep power-down flag
  PMU_PMUCTRL_DPDFLAG : dword absolute $00000800;

(*GPREG0..3 (General purpose registers 0 to 3)
The general purpose registers retain data through the Deep power-down mode when
power is still applied to the VDD(3V3) pin but the chip has entered Deep power-down mode.
Only a Â?coldÂ? boot when all power has been completely removed from the chip will reset
the general purpose registers. *)

  PMU_GPREG0_GPDATA_MASK : dword absolute $FFFFFFFF;
  PMU_GPREG1_GPDATA_MASK : dword absolute $FFFFFFFF;
  PMU_GPREG2_GPDATA_MASK : dword absolute $FFFFFFFF;
  PMU_GPREG3_GPDATA_MASK : dword absolute $FFFFFFFF;

(*GPREG4 (General purpose register 4)
The general purpose register 4 retains data through the Deep power-down mode when
power is still applied to the VDD(3V3) pin but the chip has entered Deep power-down mode.
Only a Â?coldÂ? boot, when all power has been completely removed from the chip, will reset
the general purpose registers.
Remark: If the external voltage applied on pin VDD(3V3) drops below <tbd> V, the
hysteresis of the WAKEUP input pin has to be disabled in order for the chip to wake up
from Deep power-down mode. *)

  PMU_GPREG4_GPDATA_MASK : dword absolute $FFFFF800;
  PMU_GPREG4_WAKEUPHYS_MASK : dword absolute $00000400;
  PMU_GPREG4_WAKEUPHYS_HYSTERESISENABLED : dword absolute $00000400;
  PMU_GPREG4_WAKEUPHYS_HYSTERESISDISABLED : dword absolute $00000000;
 // PMU_GPREG4_GPDATA_MASK : dword absolute $FFFFF800;

(*##############################################################################
## I/O Control (IOCON)
##############################################################################*)

  IOCON_BASE_ADDRESS :dword absolute $40044000;

(*Values that should be common to all pins, though they are also defined
on the individual pin level in case they change with a pin on any future
device *)

  IOCON_COMMON_FUNC_MASK : dword absolute $00000007;
  IOCON_COMMON_MODE_MASK : dword absolute $00000018;
  IOCON_COMMON_MODE_INACTIVE : dword absolute $00000000;
  IOCON_COMMON_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_COMMON_MODE_PULLUP : dword absolute $00000010;
  IOCON_COMMON_MODE_REPEATER : dword absolute $00000018;
  IOCON_COMMON_HYS_MASK : dword absolute $00000020;
  IOCON_COMMON_HYS_DISABLE : dword absolute $00000000;
  IOCON_COMMON_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_6 : dword absolute $40044000;
  IOCON_PIO2_6_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_6_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_6_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_6_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_6_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_6_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_6_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_6_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_6_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_6_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_0 : dword absolute $40044008;
  IOCON_PIO2_0_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_0_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_0_FUNC_DTR : dword absolute $00000001;
  IOCON_PIO2_0_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_0_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_0_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_0_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_0_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_0_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_0_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_0_HYS_ENABLE : dword absolute $00000020;

  IOCON_nRESET_PIO0_0 : dword absolute $4004400C;
  IOCON_nRESET_PIO0_0_FUNC_MASK : dword absolute $00000007;
  IOCON_nRESET_PIO0_0_FUNC_RESET : dword absolute $00000000;
  IOCON_nRESET_PIO0_0_FUNC_GPIO : dword absolute $00000001;
  IOCON_nRESET_PIO0_0_MODE_MASK : dword absolute $00000018;
  IOCON_nRESET_PIO0_0_MODE_INACTIVE : dword absolute $00000000;
  IOCON_nRESET_PIO0_0_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_nRESET_PIO0_0_MODE_PULLUP : dword absolute $00000010;
  IOCON_nRESET_PIO0_0_MODE_REPEATER : dword absolute $00000018;
  IOCON_nRESET_PIO0_0_HYS_MASK : dword absolute $00000020;
  IOCON_nRESET_PIO0_0_HYS_DISABLE : dword absolute $00000000;
  IOCON_nRESET_PIO0_0_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO0_1 : dword absolute $40044010;
  IOCON_PIO0_1_FUNC_MASK : dword absolute$00000007;
  IOCON_PIO0_1_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_1_FUNC_CLKOUT : dword absolute $00000001;
  IOCON_PIO0_1_FUNC_CT32B0_MAT2 : dword absolute $00000002;
  IOCON_PIO0_1_FUNC_USB_FTOGGLE : dword absolute $00000003;
  IOCON_PIO0_1_MODE_MASK : dword absolute $00000018;
  IOCON_PIO0_1_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO0_1_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO0_1_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO0_1_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO0_1_HYS_MASK : dword absolute $00000020;
  IOCON_PIO0_1_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO0_1_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO1_8 : dword absolute $40044014;
  IOCON_PIO1_8_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_8_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_8_FUNC_CT16B1_CAP0 : dword absolute $00000001;
  IOCON_PIO1_8_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_8_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_8_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_8_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_8_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_8_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_8_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_8_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO0_2 : dword absolute $4004401C;
  IOCON_PIO0_2_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_2_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_2_FUNC_SSEL : dword absolute $00000001;
  IOCON_PIO0_2_FUNC_CT16B0_CAP0 : dword absolute $00000002;
  IOCON_PIO0_2_MODE_MASK : dword absolute $00000018;
  IOCON_PIO0_2_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO0_2_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO0_2_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO0_2_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO0_2_HYS_MASK : dword absolute $00000020;
  IOCON_PIO0_2_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO0_2_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_7 : dword absolute $40044020;
  IOCON_PIO2_7_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_7_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_7_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_7_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_7_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_7_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_7_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_7_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_7_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_7_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_8 : dword absolute $40044024;
  IOCON_PIO2_8_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_8_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_8_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_8_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_8_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_8_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_8_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_8_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_8_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_8_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_1 : dword absolute $40044028;
  IOCON_PIO2_1_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_1_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_1_FUNC_DSR : dword absolute $00000001;
  IOCON_PIO2_1_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_1_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_1_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_1_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_1_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_1_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_1_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_1_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO0_3 : dword absolute $4004402C;
  IOCON_PIO0_3_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_3_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_3_FUNC_USB_VBUS : dword absolute $00000001;
  IOCON_PIO0_3_MODE_MASK : dword absolute $00000018;
  IOCON_PIO0_3_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO0_3_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO0_3_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO0_3_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO0_3_HYS_MASK : dword absolute $00000020;
  IOCON_PIO0_3_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO0_3_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO0_4 : dword absolute $40044030;
  IOCON_PIO0_4_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_4_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_4_FUNC_I2CSCL : dword absolute $00000001;
  IOCON_PIO0_4_I2CMODE_MASK : dword absolute $00000300;
  IOCON_PIO0_4_I2CMODE_STANDARDI2C : dword absolute $00000000;
  IOCON_PIO0_4_I2CMODE_STANDARDIO : dword absolute $00000100;
  IOCON_PIO0_4_I2CMODE_FASTPLUSI2C : dword absolute $00000200;

  IOCON_PIO0_5 : dword absolute $40044034;
  IOCON_PIO0_5_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_5_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_5_FUNC_I2CSDA : dword absolute $00000001;
  IOCON_PIO0_5_I2CMODE_MASK : dword absolute $00000300;
  IOCON_PIO0_5_I2CMODE_STANDARDI2C : dword absolute $00000000;
  IOCON_PIO0_5_I2CMODE_STANDARDIO : dword absolute $00000100;
  IOCON_PIO0_5_I2CMODE_FASTPLUSI2C : dword absolute $00000200;

  IOCON_PIO1_9 : dword absolute $40044038;
  IOCON_PIO1_9_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_9_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_9_FUNC_CT16B1_MAT0 : dword absolute $00000001;
  IOCON_PIO1_9_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_9_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_9_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_9_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_9_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_9_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_9_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_9_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO3_4 : dword absolute $4004403C;
  IOCON_PIO3_4_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO3_4_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO3_4_MODE_MASK : dword absolute $00000018;
  IOCON_PIO3_4_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO3_4_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO3_4_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO3_4_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO3_4_HYS_MASK : dword absolute $00000020;
  IOCON_PIO3_4_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO3_4_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_4 : dword absolute $40044040;
  IOCON_PIO2_4_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_4_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_4_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_4_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_4_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_4_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_4_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_4_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_4_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_4_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_5 : dword absolute $40044044;
  IOCON_PIO2_5_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_5_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_5_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_5_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_5_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_5_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_5_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_5_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_5_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_5_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO3_5 : dword absolute $40044048;
  IOCON_PIO3_5_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO3_5_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO3_5_MODE_MASK : dword absolute $00000018;
  IOCON_PIO3_5_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO3_5_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO3_5_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO3_5_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO3_5_HYS_MASK : dword absolute $00000020;
  IOCON_PIO3_5_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO3_5_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO0_6 : dword absolute $4004404C;
  IOCON_PIO0_6_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_6_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_6_FUNC_USB_CONNECT : dword absolute $00000001;
  IOCON_PIO0_6_FUNC_SCK : dword absolute $00000002;
  IOCON_PIO0_6_MODE_MASK : dword absolute $00000018;
  IOCON_PIO0_6_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO0_6_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO0_6_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO0_6_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO0_6_HYS_MASK : dword absolute $00000020;
  IOCON_PIO0_6_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO0_6_HYS_ENABLE : dword absolute $00000020;


  IOCON_PIO0_7 : dword absolute $40044050;
  IOCON_PIO0_7_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_7_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_7_FUNC_CTS : dword absolute $00000001;
  IOCON_PIO0_7_MODE_MASK : dword absolute $00000018;
  IOCON_PIO0_7_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO0_7_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO0_7_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO0_7_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO0_7_HYS_MASK : dword absolute $00000020;
  IOCON_PIO0_7_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO0_7_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_9 : dword absolute $40044054;
  IOCON_PIO2_9_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_9_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_9_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_9_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_9_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_9_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_9_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_9_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_9_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_9_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_10 : dword absolute $40044058;
  IOCON_PIO2_10_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_10_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_10_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_10_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_10_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_10_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_10_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_10_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_10_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_10_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_2 : dword absolute $4004405C;
  IOCON_PIO2_2_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_2_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_2_FUNC_DCD : dword absolute $00000001;
  IOCON_PIO2_2_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_2_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_2_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_2_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_2_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_2_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_2_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_2_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO0_8 : dword absolute $40044060;
  IOCON_PIO0_8_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_8_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_8_FUNC_MISO0 : dword absolute $00000001;
  IOCON_PIO0_8_FUNC_CT16B0_MAT0 : dword absolute $00000002;
  IOCON_PIO0_8_MODE_MASK : dword absolute $00000018;
  IOCON_PIO0_8_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO0_8_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO0_8_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO0_8_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO0_8_HYS_MASK : dword absolute $00000020;
  IOCON_PIO0_8_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO0_8_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO0_9 : dword absolute $40044064;
  IOCON_PIO0_9_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO0_9_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO0_9_FUNC_MOSI0 : dword absolute $00000001;
  IOCON_PIO0_9_FUNC_CT16B0_MAT1 : dword absolute $00000002;
  IOCON_PIO0_9_FUNC_SWO : dword absolute $00000003;
  IOCON_PIO0_9_MODE_MASK : dword absolute $00000018;
  IOCON_PIO0_9_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO0_9_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO0_9_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO0_9_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO0_9_HYS_MASK : dword absolute $00000020;
  IOCON_PIO0_9_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO0_9_HYS_ENABLE : dword absolute $00000020;

  IOCON_JTAG_TCK_PIO0_10 : dword absolute $40044068;
  IOCON_JTAG_TCK_PIO0_10_FUNC_MASK : dword absolute $00000007;
  IOCON_JTAG_TCK_PIO0_10_FUNC_SWCLK : dword absolute $00000000;
  IOCON_JTAG_TCK_PIO0_10_FUNC_GPIO : dword absolute $00000001;
  IOCON_JTAG_TCK_PIO0_10_FUNC_SCK : dword absolute $00000002;
  IOCON_JTAG_TCK_PIO0_10_FUNC_CT16B0_MAT2 : dword absolute $00000003;
  IOCON_JTAG_TCK_PIO0_10_MODE_MASK : dword absolute $00000018;
  IOCON_JTAG_TCK_PIO0_10_MODE_INACTIVE : dword absolute $00000000;
  IOCON_JTAG_TCK_PIO0_10_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_JTAG_TCK_PIO0_10_MODE_PULLUP : dword absolute $00000010;
  IOCON_JTAG_TCK_PIO0_10_MODE_REPEATER : dword absolute $00000018;
  IOCON_JTAG_TCK_PIO0_10_HYS_MASK : dword absolute $00000020;
  IOCON_JTAG_TCK_PIO0_10_HYS_DISABLE : dword absolute $00000000;
  IOCON_JTAG_TCK_PIO0_10_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO1_10 : dword absolute $4004406C;
  IOCON_PIO1_10_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_10_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_10_FUNC_AD6 : dword absolute $00000001;
  IOCON_PIO1_10_FUNC_CT16B1_MAT1 : dword absolute $00000002;
  IOCON_PIO1_10_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_10_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_10_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_10_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_10_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_10_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_10_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_10_HYS_ENABLE : dword absolute $00000020;
  IOCON_PIO1_10_ADMODE_MASK : dword absolute $00000080;
  IOCON_PIO1_10_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_PIO1_10_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_PIO2_11 : dword absolute $40044070;
  IOCON_PIO2_11_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO2_11_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO2_11_FUNC_SCK0 : dword absolute $00000001;
  IOCON_PIO2_11_MODE_MASK : dword absolute $00000018;
  IOCON_PIO2_11_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO2_11_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO2_11_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO2_11_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO2_11_HYS_MASK : dword absolute $00000020;
  IOCON_PIO2_11_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO2_11_HYS_ENABLE : dword absolute $00000020;

  IOCON_JTAG_TDI_PIO0_11 : dword absolute $40044074;
  IOCON_JTAG_TDI_PIO0_11_FUNC_MASK : dword absolute $00000007;
  IOCON_JTAG_TDI_PIO0_11_FUNC_TDI : dword absolute $00000000;
  IOCON_JTAG_TDI_PIO0_11_FUNC_GPIO : dword absolute $00000001;
  IOCON_JTAG_TDI_PIO0_11_FUNC_AD0 : dword absolute $00000002;
  IOCON_JTAG_TDI_PIO0_11_FUNC_CT32B0_MAT3 : dword absolute $00000003;
  IOCON_JTAG_TDI_PIO0_11_MODE_MASK : dword absolute $00000018;
  IOCON_JTAG_TDI_PIO0_11_MODE_INACTIVE : dword absolute $00000000;
  IOCON_JTAG_TDI_PIO0_11_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_JTAG_TDI_PIO0_11_MODE_PULLUP : dword absolute $00000010;
  IOCON_JTAG_TDI_PIO0_11_MODE_REPEATER : dword absolute $00000018;
  IOCON_JTAG_TDI_PIO0_11_HYS_MASK : dword absolute $00000020;
  IOCON_JTAG_TDI_PIO0_11_HYS_DISABLE : dword absolute $00000000;
  IOCON_JTAG_TDI_PIO0_11_HYS_ENABLE : dword absolute $00000020;
  IOCON_JTAG_TDI_PIO0_11_ADMODE_MASK : dword absolute $00000080;
  IOCON_JTAG_TDI_PIO0_11_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_JTAG_TDI_PIO0_11_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_JTAG_TMS_PIO1_0 : dword absolute $40044078;
  IOCON_JTAG_TMS_PIO1_0_FUNC_MASK : dword absolute $00000007;
  IOCON_JTAG_TMS_PIO1_0_FUNC_TMS : dword absolute $00000000;
  IOCON_JTAG_TMS_PIO1_0_FUNC_GPIO : dword absolute $00000001;
  IOCON_JTAG_TMS_PIO1_0_FUNC_AD1 : dword absolute $00000002;
  IOCON_JTAG_TMS_PIO1_0_FUNC_CT32B1_CAP0 : dword absolute $00000003;
  IOCON_JTAG_TMS_PIO1_0_MODE_MASK : dword absolute $00000018;
  IOCON_JTAG_TMS_PIO1_0_MODE_INACTIVE : dword absolute $00000000;
  IOCON_JTAG_TMS_PIO1_0_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_JTAG_TMS_PIO1_0_MODE_PULLUP : dword absolute $00000010;
  IOCON_JTAG_TMS_PIO1_0_MODE_REPEATER : dword absolute $00000018;
  IOCON_JTAG_TMS_PIO1_0_HYS_MASK : dword absolute $00000020;
  IOCON_JTAG_TMS_PIO1_0_HYS_DISABLE : dword absolute $00000000;
  IOCON_JTAG_TMS_PIO1_0_HYS_ENABLE : dword absolute $00000020;
  IOCON_JTAG_TMS_PIO1_0_ADMODE_MASK : dword absolute $00000080;
  IOCON_JTAG_TMS_PIO1_0_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_JTAG_TMS_PIO1_0_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_JTAG_TDO_PIO1_1 : dword absolute $4004407C;
  IOCON_JTAG_TDO_PIO1_1_FUNC_MASK : dword absolute $00000007;
  IOCON_JTAG_TDO_PIO1_1_FUNC_TDO : dword absolute $00000000;
  IOCON_JTAG_TDO_PIO1_1_FUNC_GPIO : dword absolute $00000001;
  IOCON_JTAG_TDO_PIO1_1_FUNC_AD2 : dword absolute $00000002;
  IOCON_JTAG_TDO_PIO1_1_FUNC_CT32B1_MAT0 : dword absolute $00000003;
  IOCON_JTAG_TDO_PIO1_1_MODE_MASK : dword absolute $00000018;
  IOCON_JTAG_TDO_PIO1_1_MODE_INACTIVE : dword absolute $00000000;
  IOCON_JTAG_TDO_PIO1_1_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_JTAG_TDO_PIO1_1_MODE_PULLUP : dword absolute $00000010;
  IOCON_JTAG_TDO_PIO1_1_MODE_REPEATER : dword absolute $00000018;
  IOCON_JTAG_TDO_PIO1_1_HYS_MASK : dword absolute $00000020;
  IOCON_JTAG_TDO_PIO1_1_HYS_DISABLE : dword absolute $00000000;
  IOCON_JTAG_TDO_PIO1_1_HYS_ENABLE : dword absolute $00000020;
  IOCON_JTAG_TDO_PIO1_1_ADMODE_MASK : dword absolute $00000080;
  IOCON_JTAG_TDO_PIO1_1_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_JTAG_TDO_PIO1_1_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_JTAG_nTRST_PIO1_2 : dword absolute $40044080;
  IOCON_JTAG_nTRST_PIO1_2_FUNC_MASK : dword absolute $00000007;
  IOCON_JTAG_nTRST_PIO1_2_FUNC_TRST : dword absolute $00000000;
  IOCON_JTAG_nTRST_PIO1_2_FUNC_GPIO : dword absolute $00000001;
  IOCON_JTAG_nTRST_PIO1_2_FUNC_AD3 : dword absolute $00000002;
  IOCON_JTAG_nTRST_PIO1_2_FUNC_CT32B1_MAT1 : dword absolute $00000003;
  IOCON_JTAG_nTRST_PIO1_2_MODE_MASK : dword absolute $00000018;
  IOCON_JTAG_nTRST_PIO1_2_MODE_INACTIVE : dword absolute $00000000;
  IOCON_JTAG_nTRST_PIO1_2_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_JTAG_nTRST_PIO1_2_MODE_PULLUP : dword absolute $00000010;
  IOCON_JTAG_nTRST_PIO1_2_MODE_REPEATER : dword absolute $00000018;
  IOCON_JTAG_nTRST_PIO1_2_HYS_MASK : dword absolute $00000020;
  IOCON_JTAG_nTRST_PIO1_2_HYS_DISABLE : dword absolute $00000000;
  IOCON_JTAG_nTRST_PIO1_2_HYS_ENABLE : dword absolute $00000020;
  IOCON_JTAG_nTRST_PIO1_2_ADMODE_MASK : dword absolute $00000080;
  IOCON_JTAG_nTRST_PIO1_2_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_JTAG_nTRST_PIO1_2_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_PIO3_0 : dword absolute $40044084;
  IOCON_PIO3_0_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO3_0_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO3_0_MODE_MASK : dword absolute $00000018;
  IOCON_PIO3_0_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO3_0_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO3_0_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO3_0_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO3_0_HYS_MASK : dword absolute $00000020;
  IOCON_PIO3_0_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO3_0_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO3_1 : dword absolute $40044088;
  IOCON_PIO3_1_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO3_1_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO3_1_MODE_MASK : dword absolute $00000018;
  IOCON_PIO3_1_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO3_1_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO3_1_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO3_1_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO3_1_HYS_MASK : dword absolute $00000020;
  IOCON_PIO3_1_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO3_1_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO2_3 : dword absolute $4004408C;
const
  IOCON_PIO2_3_FUNC_MASK = $7;
  IOCON_PIO2_3_MODE_MASK = $18;
  IOCON_PIO2_3_HYS_MASK = $20;
  IOCON_PIO2_3_HYS = $20;
var
  IOCON_SWDIO_PIO1_3 : dword absolute $40044090;
  IOCON_SWDIO_PIO1_3_FUNC_MASK : dword absolute $00000007;
  IOCON_SWDIO_PIO1_3_FUNC_SWDIO : dword absolute $00000000;
  IOCON_SWDIO_PIO1_3_FUNC_GPIO : dword absolute $00000001;
  IOCON_SWDIO_PIO1_3_FUNC_AD4 : dword absolute $00000002;
  IOCON_SWDIO_PIO1_3_FUNC_CT32B1_MAT2 : dword absolute $00000003;
  IOCON_SWDIO_PIO1_3_HYS_MASK : dword absolute $00000020;
  IOCON_SWDIO_PIO1_3_HYS_DISABLE : dword absolute $00000000;
  IOCON_SWDIO_PIO1_3_HYS_ENABLE : dword absolute $00000020;
  IOCON_SWDIO_PIO1_3_ADMODE_MASK : dword absolute $00000080;
  IOCON_SWDIO_PIO1_3_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_SWDIO_PIO1_3_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_PIO1_4 : dword absolute $40044094;
  IOCON_PIO1_4_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_4_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_4_FUNC_AD5 : dword absolute $00000001;
  IOCON_PIO1_4_FUNC_CT32B1_MAT3 : dword absolute $00000002;
  IOCON_PIO1_4_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_4_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_4_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_4_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_4_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_4_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_4_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_4_HYS_ENABLE : dword absolute $00000020;
  IOCON_PIO1_4_ADMODE_MASK : dword absolute $00000080;
  IOCON_PIO1_4_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_PIO1_4_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_PIO1_11 : dword absolute $40044098;
  IOCON_PIO1_11_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_11_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_11_FUNC_AD7 : dword absolute $00000001;
  IOCON_PIO1_11_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_11_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_11_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_11_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_11_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_11_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_11_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_11_HYS_ENABLE : dword absolute $00000020;
  IOCON_PIO1_11_ADMODE_MASK : dword absolute $00000080;
  IOCON_PIO1_11_ADMODE_ANALOG : dword absolute $00000000;
  IOCON_PIO1_11_ADMODE_DIGITAL : dword absolute $00000080;

  IOCON_PIO3_2 : dword absolute $4004409C;
  IOCON_PIO3_2_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO3_2_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO3_2_MODE_MASK : dword absolute $00000018;
  IOCON_PIO3_2_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO3_2_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO3_2_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO3_2_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO3_2_HYS_MASK : dword absolute $00000020;
  IOCON_PIO3_2_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO3_2_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO1_5 : dword absolute $400440A0;
  IOCON_PIO1_5_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_5_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_5_FUNC_RTS : dword absolute $00000001;
  IOCON_PIO1_5_FUNC_CT32B0_CAP0 : dword absolute $00000002;
  IOCON_PIO1_5_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_5_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_5_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_5_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_5_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_5_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_5_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_5_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO1_6 : dword absolute $400440A4;
  IOCON_PIO1_6_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_6_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_6_FUNC_UART_RXD : dword absolute $00000001;
  IOCON_PIO1_6_FUNC_CT32B0_MAT0 : dword absolute $00000002;
  IOCON_PIO1_6_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_6_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_6_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_6_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_6_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_6_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_6_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_6_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO1_7 : dword absolute $400440A8;
  IOCON_PIO1_7_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO1_7_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO1_7_FUNC_UART_TXD : dword absolute $00000001;
  IOCON_PIO1_7_FUNC_CT32B0_MAT1 : dword absolute $00000002;
  IOCON_PIO1_7_MODE_MASK : dword absolute $00000018;
  IOCON_PIO1_7_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO1_7_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO1_7_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO1_7_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO1_7_HYS_MASK : dword absolute $00000020;
  IOCON_PIO1_7_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO1_7_HYS_ENABLE : dword absolute $00000020;

  IOCON_PIO3_3 : dword absolute $400440AC;
  IOCON_PIO3_3_FUNC_MASK : dword absolute $00000007;
  IOCON_PIO3_3_FUNC_GPIO : dword absolute $00000000;
  IOCON_PIO3_3_MODE_MASK : dword absolute $00000018;
  IOCON_PIO3_3_MODE_INACTIVE : dword absolute $00000000;
  IOCON_PIO3_3_MODE_PULLDOWN : dword absolute $00000008;
  IOCON_PIO3_3_MODE_PULLUP : dword absolute $00000010;
  IOCON_PIO3_3_MODE_REPEATER : dword absolute $00000018;
  IOCON_PIO3_3_HYS_MASK : dword absolute $00000020;
  IOCON_PIO3_3_HYS_DISABLE : dword absolute $00000000;
  IOCON_PIO3_3_HYS_ENABLE : dword absolute $00000020;

  IOCON_SCKLOC : dword absolute $400440B0;
  IOCON_SCKLOC_SCKPIN_MASK : dword absolute $00000003;
  IOCON_SCKLOC_SCKPIN_PIO0_10 : dword absolute $00000000;// Set SCK function to pin 0.10
  IOCON_SCKLOC_SCKPIN_PIO2_11 : dword absolute $00000001;// Set SCK function to pin 2.11
  IOCON_SCKLOC_SCKPIN_PIO0_6 : dword absolute $00000003;// Set SCK function to pin 0.6

(*##############################################################################
## Nested Vectored Interrupt Controller
##############################################################################*)

//  NVIC_BASE_ADDRESS $E000E100)

//typedef struct
{
  volatile uint32_t ISER[8]; (*!< Offset: 0x000 Interrupt Set Enable Register *)
uint32_t RESERVED0[24];
  volatile uint32_t ICER[8]; (*!< Offset: 0x080 Interrupt Clear Enable Register *)
uint32_t RSERVED1[24];
  volatile uint32_t ISPR[8]; (*!< Offset: 0x100 Interrupt Set Pending Register *)
uint32_t RESERVED2[24];
  volatile uint32_t ICPR[8]; (*!< Offset: 0x180 Interrupt Clear Pending Register *)
uint32_t RESERVED3[24];
  volatile uint32_t IABR[8]; (*!< Offset: 0x200 Interrupt Active bit Register *)
uint32_t RESERVED4[56];
  volatile uint8_t IP[240]; (*!< Offset: 0x300 Interrupt Priority Register (8Bit wide) *)
uint32_t RESERVED5[644];
  volatile uint32_t STIR; (*!< Offset: 0xE00 Software Trigger Interrupt Register *)
//}// NVIC_Type;

//  NVIC ((NVIC_Type *) NVIC_BASE_ADDRESS)

type
  NVIC_Type =  record
    ISER: Array [1..8] of longword;
    ICER: Array [1..8] of longword;
    ISPR: Array [1..8] of longword;
    IABR: Array [1..8] of longword;
    IP: Array [1..240] of shortint;
    Reserver5: Array[1..644] of longword;
    STIR : longword;
  end;
  NVIC_BASE_ADDRESS = ^NVIC_Type;

var
  NVIC:NVIC_BASE_ADDRESS;

Const
  NonMaskableInt_IRQn = -14; (*!< 2 Non Maskable Interrupt *)
  MemoryManagement_IRQn = -12;(*!< 4 Cortex-M3 Memory Management Interrupt *)
  BusFault_IRQn = -11; (*!< 5 Cortex-M3 Bus Fault Interrupt *)
  UsageFault_IRQn = -10;(*!< 6 Cortex-M3 Usage Fault Interrupt *)
  SVCall_IRQn = -5; (*!< 11 Cortex-M3 SV Call Interrupt *)
  DebugMonitor_IRQn = -4; (*!< 12 Cortex-M3 Debug Monitor Interrupt *)
  PendSV_IRQn = -2;(*!< 14 Cortex-M3 Pend SV Interrupt *)
  SysTick_IRQn = -1; (*!< 15 Cortex-M3 System Tick Interrupt *)

(****** LPC13xx Specific Interrupt Numbers *******************************************************)
  WAKEUP0_IRQn = 0; (*!< All I/O pins can be used as wakeup source. *)
  WAKEUP1_IRQn = 1; (*!< There are 40 pins in total for LPC17xx *)
  WAKEUP2_IRQn = 2;
  WAKEUP3_IRQn = 3;
  WAKEUP4_IRQn = 4;
  WAKEUP5_IRQn = 5;
  WAKEUP6_IRQn = 6;
  WAKEUP7_IRQn = 7;
  WAKEUP8_IRQn = 8;
  WAKEUP9_IRQn = 9;
  WAKEUP10_IRQn = 10;
  WAKEUP11_IRQn = 11;
  WAKEUP12_IRQn = 12;
  WAKEUP13_IRQn = 13;
  WAKEUP14_IRQn = 14;
  WAKEUP15_IRQn = 15;
  WAKEUP16_IRQn = 16;
  WAKEUP17_IRQn = 17;
  WAKEUP18_IRQn = 18;
  WAKEUP19_IRQn = 19;
  WAKEUP20_IRQn = 20;
  WAKEUP21_IRQn = 21;
  WAKEUP22_IRQn = 22;
  WAKEUP23_IRQn = 23;
  WAKEUP24_IRQn = 24;
  WAKEUP25_IRQn = 25;
  WAKEUP26_IRQn = 26;
  WAKEUP27_IRQn = 27;
  WAKEUP28_IRQn = 28;
  WAKEUP29_IRQn = 29;
  WAKEUP30_IRQn = 30;
  WAKEUP31_IRQn = 31;
  WAKEUP32_IRQn = 32;
  WAKEUP33_IRQn = 33;
  WAKEUP34_IRQn = 34;
  WAKEUP35_IRQn = 35;
  WAKEUP36_IRQn = 36;
  WAKEUP37_IRQn = 37;
  WAKEUP38_IRQn = 38;
  WAKEUP39_IRQn = 39;
  I2C_IRQn = 40;//(*!< I2C Interrupt *)
  TIMER_16_0_IRQn = 41;//(*!< 16-bit Timer0 Interrupt *)
  TIMER_16_1_IRQn = 42;//(*!< 16-bit Timer1 Interrupt *)
  TIMER_32_0_IRQn = 43;//(*!< 32-bit Timer0 Interrupt *)
  TIMER_32_1_IRQn = 44;//(*!< 32-bit Timer1 Interrupt *)
  SSP_IRQn = 45;//(*!< SSP Interrupt *)
  UART_IRQn = 46;//(*!< UART Interrupt *)
  USB_IRQn = 47;//(*!< USB Regular Interrupt *)
  USB_FIQn = 48;//(*!< USB Fast Interrupt *)
  ADC_IRQn = 49;//(*!< A/D Converter Interrupt *)
  WDT_IRQn = 50;//(*!< Watchdog timer Interrupt *)
  BOD_IRQn = 51;//(*!< Brown Out Detect(BOD) Interrupt *)
  EINT3_IRQn = 53;//(*!< External Interrupt 3 Interrupt *)
  EINT2_IRQn = 54; (*!< External Interrupt 2 Interrupt *)
  EINT1_IRQn = 55; (*!< External Interrupt 1 Interrupt *)
  EINT0_IRQn = 56; (*!< External Interrupt 0 Interrupt *)


(*##############################################################################
## GPIO - General Purpose I/O
##############################################################################*)

const
  GPIO_GPIO0_BASE = $50000000;
  GPIO_GPIO1_BASE = $50010000;
  GPIO_GPIO2_BASE = $50020000;
  GPIO_GPIO3_BASE = $50030000;

var
  GPIO_GPIO0DATA : dword absolute $50003FFC;// Port data register
  GPIO_GPIO0DIR : dword absolute $50008000;// Data direction register
  GPIO_GPIO0IS : dword absolute $50008004;// Interrupt sense register
  GPIO_GPIO0IBE : dword absolute $50008008;// Interrupt both edges register
  GPIO_GPIO0IEV : dword absolute $5000800C;// Interrupt event register
  GPIO_GPIO0IE : dword absolute $50008010;// Interrupt mask register
  GPIO_GPIO0RIS : dword absolute $50008014;// Raw interrupt status register
  GPIO_GPIO0MIS : dword absolute $50008018;// Masked interrupt status register
  GPIO_GPIO0IC : dword absolute $5000801C;// Interrupt clear register

  GPIO_GPIO1DATA : dword absolute $50013FFC;// Port data register
  GPIO_GPIO1DIR : dword absolute $50018000;// Data direction register
  GPIO_GPIO1IS : dword absolute $50018004;// Interrupt sense register
  GPIO_GPIO1IBE : dword absolute $50018008;// Interrupt both edges register
  GPIO_GPIO1IEV : dword absolute $5001800C;// Interrupt event register
  GPIO_GPIO1IE : dword absolute $50018010;// Interrupt mask register
  GPIO_GPIO1RIS : dword absolute $50018014;// Raw interrupt status register
  GPIO_GPIO1MIS : dword absolute $50018018;// Masked interrupt status register
  GPIO_GPIO1IC : dword absolute $5001801C;// Interrupt clear register

  GPIO_GPIO2DATA : dword absolute $50023FFC;// Port data register
  GPIO_GPIO2DIR : dword absolute $50028000;// Data direction register
  GPIO_GPIO2IS : dword absolute $50028004;// Interrupt sense register
  GPIO_GPIO2IBE : dword absolute $50028008;// Interrupt both edges register
  GPIO_GPIO2IEV : dword absolute $5002800C;// Interrupt event register
  GPIO_GPIO2IE : dword absolute $50028010;// Interrupt mask register
  GPIO_GPIO2RIS : dword absolute $50028014;// Raw interrupt status register
  GPIO_GPIO2MIS : dword absolute $50028018;// Masked interrupt status register
  GPIO_GPIO2IC : dword absolute $5002801C;// Interrupt clear register

  GPIO_GPIO3DATA : dword absolute $50033FFC;// Port data register
  GPIO_GPIO3DIR : dword absolute $50038000;// Data direction register
  GPIO_GPIO3IS : dword absolute $50038004;// Interrupt sense register
  GPIO_GPIO3IBE : dword absolute $50038008;// Interrupt both edges register
  GPIO_GPIO3IEV : dword absolute $5003800C;// Interrupt event register
  GPIO_GPIO3IE : dword absolute $50038010;// Interrupt mask register
  GPIO_GPIO3RIS : dword absolute $50038014;// Raw interrupt status register
  GPIO_GPIO3MIS : dword absolute $50038018;// Masked interrupt status register
  GPIO_GPIO3IC : dword absolute $5003801C;// Interrupt clear register

  GPIO_IO_P0 : dword absolute $00000001;
  GPIO_IO_P1 : dword absolute $00000002;
  GPIO_IO_P2 : dword absolute $00000004;
  GPIO_IO_P3 : dword absolute $00000008;
  GPIO_IO_P4 : dword absolute $00000010;
  GPIO_IO_P5 : dword absolute $00000020;
  GPIO_IO_P6 : dword absolute $00000040;
  GPIO_IO_P7 : dword absolute $00000080;
  GPIO_IO_P8 : dword absolute $00000100;
  GPIO_IO_P9 : dword absolute $00000200;
  GPIO_IO_P10 : dword absolute $00000400;
  GPIO_IO_P11 : dword absolute $00000800;
  GPIO_IO_ALL : dword absolute $00000FFF;

(*##############################################################################
## USB
##############################################################################*)

(*USB registers are defined in USB code *)
  const
  USB_BASE_ADDRESS = $40020000;

  var
(*USB Device Interrupt Status Register *)
  USB_DEVINTST : dword absolute $40020000;
  USB_DEVINTST_FRAME_MASK : dword absolute $00000001;
  USB_DEVINTST_FRAME : dword absolute $00000001;// Frame interrupt
  USB_DEVINTST_EP0_MASK : dword absolute $00000002;
  USB_DEVINTST_EP0 : dword absolute $00000002;// USB core interrupt for EP0
  USB_DEVINTST_EP1_MASK : dword absolute $00000004;
  USB_DEVINTST_EP1 : dword absolute $00000004;// USB core interrupt for EP1
  USB_DEVINTST_EP2_MASK : dword absolute $00000008;
  USB_DEVINTST_EP2 : dword absolute $00000008;// USB core interrupt for EP2
  USB_DEVINTST_EP3_MASK : dword absolute $00000010;
  USB_DEVINTST_EP3 : dword absolute $00000010;// USB core interrupt for EP3
  USB_DEVINTST_EP4_MASK : dword absolute $00000020;
  USB_DEVINTST_EP4 : dword absolute $00000020;// USB core interrupt for EP4
  USB_DEVINTST_EP5_MASK : dword absolute $00000040;
  USB_DEVINTST_EP5 : dword absolute $00000040;// USB core interrupt for EP5
  USB_DEVINTST_EP6_MASK : dword absolute $00000080;
  USB_DEVINTST_EP6 : dword absolute $00000080;// USB core interrupt for EP6
  USB_DEVINTST_EP7_MASK : dword absolute $00000100;
  USB_DEVINTST_EP7 : dword absolute $00000100;// USB core interrupt for EP7
  USB_DEVINTST_DEV_STAT_MASK : dword absolute $00000200;
  USB_DEVINTST_DEV_STAT : dword absolute $00000200;
  USB_DEVINTST_CC_EMPTY_MASK : dword absolute $00000400;
  USB_DEVINTST_CC_EMPTY : dword absolute $00000400;
  USB_DEVINTST_CD_FULL_MASK : dword absolute $00000800;
  USB_DEVINTST_CD_FULL : dword absolute $00000800;
  USB_DEVINTST_RxENDPKT_MASK : dword absolute $00001000;
  USB_DEVINTST_RxENDPKT : dword absolute $00001000;
  USB_DEVINTST_TxENDPKT_MASK : dword absolute $00002000;
  USB_DEVINTST_TxENDPKT : dword absolute $00002000;

(*USB Device Interrupt Enable Register *)
  USB_DEVINTEN : dword absolute $40020004;
  USB_DEVINTEN_FRAME_MASK : dword absolute $00000001;
  USB_DEVINTEN_FRAME : dword absolute $00000001;
  USB_DEVINTEN_EP0_MASK : dword absolute $00000002;
  USB_DEVINTEN_EP0 : dword absolute $00000002;
  USB_DEVINTEN_EP1_MASK : dword absolute $00000004;
  USB_DEVINTEN_EP1 : dword absolute $00000004;
  USB_DEVINTEN_EP2_MASK : dword absolute $00000008;
  USB_DEVINTEN_EP2 : dword absolute $00000008;
  USB_DEVINTEN_EP3_MASK : dword absolute $00000010;
  USB_DEVINTEN_EP3 : dword absolute $00000010;
  USB_DEVINTEN_EP4_MASK : dword absolute $00000020;
  USB_DEVINTEN_EP4 : dword absolute $00000020;
  USB_DEVINTEN_EP5_MASK : dword absolute $00000040;
  USB_DEVINTEN_EP5 : dword absolute $00000040;
  USB_DEVINTEN_EP6_MASK : dword absolute $00000080;
  USB_DEVINTEN_EP6 : dword absolute $00000080;
  USB_DEVINTEN_EP7_MASK : dword absolute $00000100;
  USB_DEVINTEN_EP7 : dword absolute $00000100;
  USB_DEVINTEN_DEV_STAT_MASK : dword absolute $00000200;
  USB_DEVINTEN_DEV_STAT : dword absolute $00000200;
  USB_DEVINTEN_CC_EMPTY_MASK : dword absolute $00000400;
  USB_DEVINTEN_CC_EMPTY : dword absolute $00000400;
  USB_DEVINTEN_CD_FULL_MASK : dword absolute $00000800;
  USB_DEVINTEN_CD_FULL : dword absolute $00000800;
  USB_DEVINTEN_RxENDPKT_MASK : dword absolute $00001000;
  USB_DEVINTEN_RxENDPKT : dword absolute $00001000;
  USB_DEVINTEN_TxENDPKT_MASK : dword absolute $00002000;
  USB_DEVINTEN_TxENDPKT : dword absolute $00002000;

(*USB Device Interrupt Clear Register *)
  USB_DEVINTCLR : dword absolute $40020008;
  USB_DEVINTCLR_FRAME_MASK : dword absolute $00000001;
  USB_DEVINTCLR_FRAME : dword absolute $00000001;
  USB_DEVINTCLR_EP0_MASK : dword absolute $00000002;
  USB_DEVINTCLR_EP0 : dword absolute $00000002;
  USB_DEVINTCLR_EP1_MASK : dword absolute $00000004;
  USB_DEVINTCLR_EP1 : dword absolute $00000004;
  USB_DEVINTCLR_EP2_MASK : dword absolute $00000008;
  USB_DEVINTCLR_EP2 : dword absolute $00000008;
  USB_DEVINTCLR_EP3_MASK : dword absolute $00000010;
  USB_DEVINTCLR_EP3 : dword absolute $00000010;
  USB_DEVINTCLR_EP4_MASK : dword absolute $00000020;
  USB_DEVINTCLR_EP4 : dword absolute $00000020;
  USB_DEVINTCLR_EP5_MASK : dword absolute $00000040;
  USB_DEVINTCLR_EP5 : dword absolute $00000040;
  USB_DEVINTCLR_EP6_MASK : dword absolute $00000080;
  USB_DEVINTCLR_EP6 : dword absolute $00000080;
  USB_DEVINTCLR_EP7_MASK : dword absolute $00000100;
  USB_DEVINTCLR_EP7 : dword absolute $00000100;
  USB_DEVINTCLR_DEV_STAT_MASK : dword absolute $00000200;
  USB_DEVINTCLR_DEV_STAT : dword absolute $00000200;
  USB_DEVINTCLR_CC_EMPTY_MASK : dword absolute $00000400;
  USB_DEVINTCLR_CC_EMPTY : dword absolute $00000400;
  USB_DEVINTCLR_CD_FULL_MASK : dword absolute $00000800;
  USB_DEVINTCLR_CD_FULL : dword absolute $00000800;
  USB_DEVINTCLR_RxENDPKT_MASK : dword absolute $00001000;
  USB_DEVINTCLR_RxENDPKT : dword absolute $00001000;
  USB_DEVINTCLR_TxENDPKT_MASK : dword absolute $00002000;
  USB_DEVINTCLR_TxENDPKT : dword absolute $00002000;

(*USB Device Interrupt Set Register *)
  USB_DEVINTSET : dword absolute $4002000C;
  USB_DEVINTSET_FRAME_MASK : dword absolute $00000001;
  USB_DEVINTSET_FRAME : dword absolute $00000001;
  USB_DEVINTSET_EP0_MASK : dword absolute $00000002;
  USB_DEVINTSET_EP0 : dword absolute $00000002;
  USB_DEVINTSET_EP1_MASK : dword absolute $00000004;
  USB_DEVINTSET_EP1 : dword absolute $00000004;
  USB_DEVINTSET_EP2_MASK : dword absolute $00000008;
  USB_DEVINTSET_EP2 : dword absolute $00000008;
  USB_DEVINTSET_EP3_MASK : dword absolute $00000010;
  USB_DEVINTSET_EP3 : dword absolute $00000010;
  USB_DEVINTSET_EP4_MASK : dword absolute $00000020;
  USB_DEVINTSET_EP4 : dword absolute $00000020;
  USB_DEVINTSET_EP5_MASK : dword absolute $00000040;
  USB_DEVINTSET_EP5 : dword absolute $00000040;
  USB_DEVINTSET_EP6_MASK : dword absolute $00000080;
  USB_DEVINTSET_EP6 : dword absolute $00000080;
  USB_DEVINTSET_EP7_MASK : dword absolute $00000100;
  USB_DEVINTSET_EP7 : dword absolute $00000100;
  USB_DEVINTSET_DEV_STAT_MASK : dword absolute $00000200;
  USB_DEVINTSET_DEV_STAT : dword absolute $00000200;
  USB_DEVINTSET_CC_EMPTY_MASK : dword absolute $00000400;
  USB_DEVINTSET_CC_EMPTY : dword absolute $00000400;
  USB_DEVINTSET_CD_FULL_MASK : dword absolute $00000800;
  USB_DEVINTSET_CD_FULL : dword absolute $00000800;
  USB_DEVINTSET_RxENDPKT_MASK : dword absolute $00001000;
  USB_DEVINTSET_RxENDPKT : dword absolute $00001000;
  USB_DEVINTSET_TxENDPKT_MASK : dword absolute $00002000;
  USB_DEVINTSET_TxENDPKT : dword absolute $00002000;

(*USB Command Code Register *)
  USB_CMDCODE : dword absolute $40020010;
  USB_CMDCODE_CMD_PHASE_WRITE : dword absolute $00000100;
  USB_CMDCODE_CMD_PHASE_READ : dword absolute $00000200;
  USB_CMDCODE_CMD_PHASE_COMMAND : dword absolute $00000500;
  USB_CMDCODE_CMD_PHASE_MASK : dword absolute $0000FF00;
  USB_CMDCODE_CMD_CODE_MASK : dword absolute $00FF0000;
  USB_CMDCODE_CMD_WDATA_MASK : dword absolute $00FF0000;

(*USB Command Data Register *)
  USB_CMDDATA : dword absolute $40020014;
  USB_CMDDATA_CMD_RDATA_MASK : dword absolute $000000FF;

(*USB Receive Data Register *)
  USB_RXDATA : dword absolute $40020018;

(*USB Transmit Data Register *)
  USB_TXDATA : dword absolute $4002001C;

(*USB Receive Packet Length Register *)
  USB_RXPLEN : dword absolute $40020020;
  USB_RXPLEN_PKT_LNGTH_MASK : dword absolute $000003FF;
  USB_RXPLEN_DV_MASK : dword absolute $00000400;
  USB_RXPLEN_DV : dword absolute $00000400;

(*USB Transmit Packet Length Register *)
  USB_TXPLEN : dword absolute $40020024;
  const
    USB_TXPLEN_PKT_LNGTH_MASK  = $3FF;
  var
(*USB Control Register *)
  USB_CTRL : dword absolute $40020028;
  USB_CTRL_RD_EN_MASK : dword absolute $00000001;
  USB_CTRL_RD_EN : dword absolute $00000001;
  USB_CTRL_WR_EN_MASK : dword absolute $00000002;
  USB_CTRL_WR_EN : dword absolute $00000002;
  USB_CTRL_LOG_ENDPOINT_MASK : dword absolute $0000003C;

(*USB Device FIQ Select Register *)
  USB_DEVFIQSEL : dword absolute $4002002C;
  USB_DEVFIQSEL_FRAME_MASK : dword absolute $00000001;
  USB_DEVFIQSEL_FRAME : dword absolute $00000001;
  USB_DEVFIQSEL_BULKOUT_MASK : dword absolute $00000002;
  USB_DEVFIQSEL_BULKOUT : dword absolute $00000002;
  USB_DEVFIQSEL_BULKIN_MASK : dword absolute $00000004;
  USB_DEVFIQSEL_BULKIN : dword absolute $00000004;

(*##############################################################################
## UART
##############################################################################*)

const
  UART_BASE_ADDRESS = $40008000;
var
  UART_U0RBR : dword absolute $40008000;// Receive buffer
  UART_U0THR : dword absolute $40008000;// Transmitter holding register
  UART_U0DLL : dword absolute $40008000;// Divisor latch LSB
  UART_U0DLM : dword absolute $40008004;// Divisor latch MSB
  UART_U0IER : dword absolute $40008004;// Interrupt enable
  UART_U0IIR : dword absolute $40008008;// Interrupt identification
  UART_U0FCR : dword absolute $40008008;// FIFO control
  UART_U0MCR : dword absolute $40008010;// Modem control
  UART_U0LCR : dword absolute $4000800C;// Line control
  UART_U0LSR : dword absolute $40008014;// Line status
  UART_U0MSR : dword absolute $40008018;// Modem status
  UART_U0SCR : dword absolute $4000801C;// Scratch pad
  UART_U0ACR : dword absolute $40008020;// Auto-baud control
  UART_U0FDR : dword absolute $40008028;// Fractional divider
  UART_U0TER : dword absolute $40008030;// Transmit enable
  UART_U0RS485CTRL : dword absolute $4000804C;// RS485 control register
  UART_U0RS485ADRMATCH : dword absolute $40008050;// RS485 address match
  UART_U0RS485DLY : dword absolute $40008054;// RS485 Delay value
  UART_U0FIFOLVL : dword absolute $40008058;// UART FIFO level

  UART_U0RBR_MASK : dword absolute $000000FF;

  UART_U0IER_RBR_Interrupt_MASK : dword absolute $00000001;// Enables the received data available interrupt
  UART_U0IER_RBR_Interrupt_Enabled : dword absolute $00000001;
  UART_U0IER_RBR_Interrupt_Disabled : dword absolute $00000000;
  UART_U0IER_THRE_Interrupt_MASK : dword absolute $00000002;// Enables the THRE interrupt
  UART_U0IER_THRE_Interrupt_Enabled : dword absolute $00000002;
  UART_U0IER_THRE_Interrupt_Disabled : dword absolute $00000000;
  UART_U0IER_RLS_Interrupt_MASK : dword absolute $00000004;// Enables the Rx line status interrupt
  UART_U0IER_RLS_Interrupt_Enabled : dword absolute $00000004;
  UART_U0IER_RLS_Interrupt_Disabled : dword absolute $00000000;
  UART_U0IER_ABEOIntEn_MASK : dword absolute $00000100;// End of auto-baud interrupt
  UART_U0IER_ABEOIntEn_Enabled : dword absolute $00000100;
  UART_U0IER_ABEOIntEn_Disabled : dword absolute $00000000;
  UART_U0IER_ABTOIntEn_MASK : dword absolute $00000200;// Auto-baud timeout interrupt
  UART_U0IER_ABTOIntEn_Enabled : dword absolute $00000200;
  UART_U0IER_ABTOIntEn_Disabled : dword absolute $00000000;

  UART_U0IIR_IntStatus_MASK : dword absolute $00000001;// Interrupt status
  UART_U0IIR_IntStatus_InterruptPending : dword absolute $00000001;
  UART_U0IIR_IntStatus_NoInterruptPending : dword absolute $00000000;
  UART_U0IIR_IntId_MASK : dword absolute $0000000E;// Interrupt identification
  UART_U0IIR_IntId_RLS : dword absolute $00000006;// Receive line status
  UART_U0IIR_IntId_RDA : dword absolute $00000004;// Receive data available
  UART_U0IIR_IntId_CTI : dword absolute $0000000C;// Character time-out indicator
  UART_U0IIR_IntId_THRE : dword absolute $00000002;// THRE interrupt
  UART_U0IIR_IntId_MODEM : dword absolute $00000000;// Modem interrupt
  UART_U0IIR_FIFO_Enable_MASK : dword absolute $000000C0;
  UART_U0IIR_ABEOInt_MASK : dword absolute $00000100;// End of auto-baud interrupt
  UART_U0IIR_ABEOInt : dword absolute $00000100;
  UART_U0IIR_ABTOInt_MASK : dword absolute $00000200;// Auto-baud time-out interrupt
  UART_U0IIR_ABTOInt : dword absolute $00000200;

  UART_U0FCR_FIFO_Enable_MASK : dword absolute $00000001;// UART FIFOs enabled/disabled
  UART_U0FCR_FIFO_Enabled : dword absolute $00000001;
  UART_U0FCR_FIFO_Disabled : dword absolute $00000000;
  UART_U0FCR_Rx_FIFO_Reset_MASK : dword absolute $00000002;
  UART_U0FCR_Rx_FIFO_Reset : dword absolute $00000002;// Clear Rx FIFO
  UART_U0FCR_Tx_FIFO_Reset_MASK : dword absolute $00000004;
  UART_U0FCR_Tx_FIFO_Reset : dword absolute $00000004;// Clear Tx FIFO
  UART_U0FCR_Rx_Trigger_Level_Select_MASK : dword absolute $000000C0;// Chars written before before interrupt
  UART_U0FCR_Rx_Trigger_Level_Select_1Char : dword absolute $00000000;
  UART_U0FCR_Rx_Trigger_Level_Select_4Char : dword absolute $00000040;
  UART_U0FCR_Rx_Trigger_Level_Select_8Char : dword absolute $00000080;
  UART_U0FCR_Rx_Trigger_Level_Select_12Char : dword absolute $000000C0;

  UART_U0MCR_DTR_Control_MASK : dword absolute $00000001;// Source for modem output pin DTR
  UART_U0MCR_DTR_Control : dword absolute $00000001;
  UART_U0MCR_RTS_Control_MASK : dword absolute $00000002;// Source for modem output pin RTS
  UART_U0MCR_RTS_Control : dword absolute $00000002;
  UART_U0MCR_Loopback_Mode_Select_MASK : dword absolute $00000010;// Diagnostic loopback mode
  UART_U0MCR_Loopback_Mode_Select_Enabled : dword absolute $00000010;
  UART_U0MCR_Loopback_Mode_Select_Disabled : dword absolute $00000000;
  UART_U0MCR_RTSen_MASK : dword absolute $00000040;// Disable auto-rts flow control
  UART_U0MCR_RTSen_Enabled : dword absolute $00000040;
  UART_U0MCR_RTSen_Disabled : dword absolute $00000000;
  UART_U0MCR_CTSen_MASK : dword absolute $00000080;// Disable auto-cts flow control
  UART_U0MCR_CTSen_Enabled : dword absolute $00000080;
  UART_U0MCR_CTSen_Disabled : dword absolute $00000000;

  UART_U0LCR_Word_Length_Select_MASK : dword absolute $00000003;// Word Length Selector
  UART_U0LCR_Word_Length_Select_5Chars : dword absolute $00000000;
  UART_U0LCR_Word_Length_Select_6Chars : dword absolute $00000001;
  UART_U0LCR_Word_Length_Select_7Chars : dword absolute $00000002;
  UART_U0LCR_Word_Length_Select_8Chars : dword absolute $00000003;
  UART_U0LCR_Stop_Bit_Select_MASK : dword absolute $00000004;// Stop bit select
  UART_U0LCR_Stop_Bit_Select_1Bits : dword absolute $00000000;
  UART_U0LCR_Stop_Bit_Select_2Bits : dword absolute $00000004;
  UART_U0LCR_Parity_Enable_MASK : dword absolute $00000008;// Parity enable
  UART_U0LCR_Parity_Enabled : dword absolute $00000008;
  UART_U0LCR_Parity_Disabled : dword absolute $00000000;
  UART_U0LCR_Parity_Select_MASK : dword absolute $00000030;// Parity select
  UART_U0LCR_Parity_Select_OddParity : dword absolute $00000000;
  UART_U0LCR_Parity_Select_EvenParity : dword absolute $00000010;
  UART_U0LCR_Parity_Select_Forced1 : dword absolute $00000020;
  UART_U0LCR_Parity_Select_Forced0 : dword absolute $00000030;
  UART_U0LCR_Break_Control_MASK : dword absolute $00000040;// Break transmission control
  UART_U0LCR_Break_Control_Enabled : dword absolute $00000040;
  UART_U0LCR_Break_Control_Disabled : dword absolute $00000000;
  UART_U0LCR_Divisor_Latch_Access_MASK : dword absolute $00000080;// Divisor latch access
  UART_U0LCR_Divisor_Latch_Access_Enabled : dword absolute $00000080;
  UART_U0LCR_Divisor_Latch_Access_Disabled : dword absolute $00000000;

  UART_U0LSR_RDR_MASK : dword absolute $00000001;// Receiver data ready
  UART_U0LSR_RDR_EMPTY : dword absolute $00000000;// U0RBR is empty
  UART_U0LSR_RDR_DATA : dword absolute $00000001;// U0RBR contains valid data
  UART_U0LSR_OE_MASK : dword absolute $00000002;// Overrun error
  UART_U0LSR_OE : dword absolute $00000002;
  UART_U0LSR_PE_MASK : dword absolute $00000004;// Parity error
  UART_U0LSR_PE : dword absolute $00000004;
  UART_U0LSR_FE_MASK : dword absolute $00000008;// Framing error
  UART_U0LSR_FE : dword absolute $00000008;
  UART_U0LSR_BI_MASK : dword absolute $00000010;// Break interrupt
  UART_U0LSR_BI : dword absolute $00000010;
  UART_U0LSR_THRE_MASK : dword absolute $00000020;// Transmitter holding register empty
  UART_U0LSR_THRE : dword absolute $00000020;
  UART_U0LSR_TEMT_MASK : dword absolute $00000040;// Transmitter empty
  UART_U0LSR_TEMT : dword absolute $00000040;
  UART_U0LSR_RXFE_MASK : dword absolute $00000080;// Error in Rx FIFO
  UART_U0LSR_RXFE : dword absolute $00000080;

  UART_U0MSR_Delta_CTS_MASK : dword absolute $00000001;// State change of input CTS
  UART_U0MSR_Delta_CTS : dword absolute $00000001;
  UART_U0MSR_Delta_DSR_MASK : dword absolute $00000002;// State change of input DSR
  UART_U0MSR_Delta_DSR : dword absolute $00000002;
  UART_U0MSR_Trailing_Edge_RI_MASK : dword absolute $00000004;// Low to high transition of input RI
  UART_U0MSR_Trailing_Edge_RI : dword absolute $00000004;
  UART_U0MSR_Delta_DCD_MASK : dword absolute $00000008;// State change of input DCD
  UART_U0MSR_Delta_DCD : dword absolute $00000008;
  UART_U0MSR_CTS_MASK : dword absolute $00000010;// Clear to send state
  UART_U0MSR_CTS : dword absolute $00000010;
  UART_U0MSR_DSR_MASK : dword absolute $00000020;// Data set ready state
  UART_U0MSR_DSR : dword absolute $00000020;
  UART_U0MSR_RI_MASK : dword absolute $00000040;// Ring indicator state
  UART_U0MSR_RI : dword absolute $00000040;
  UART_U0MSR_DCD_MASK : dword absolute $00000080;// Data carrier detect state
  UART_U0MSR_DCD : dword absolute $00000080;

  UART_U0ACR_Start_MASK : dword absolute $00000001;// Auto-baud start/stop
  UART_U0ACR_Start : dword absolute $00000001;
  UART_U0ACR_Stop : dword absolute $00000000;
  UART_U0ACR_Mode_MASK : dword absolute $00000002;// Auto-baud mode select
  UART_U0ACR_Mode_Mode1 : dword absolute $00000000;
  UART_U0ACR_Mode_Mode2 : dword absolute $00000002;
  UART_U0ACR_AutoRestart_MASK : dword absolute $00000004;
  UART_U0ACR_AutoRestart_NoRestart : dword absolute $00000000;
  UART_U0ACR_AutoRestart_Restart : dword absolute $00000004;// Restart in case of time-out
  UART_U0ACR_ABEOIntClr_MASK : dword absolute $00000100;// End of auto-baud interrupt clear bit
  UART_U0ACR_ABEOIntClr : dword absolute $00000100;
  UART_U0ACR_ABTOIntClr_MASK : dword absolute $00000200;// Auto-baud timeout interrupt clear bit
  UART_U0ACR_ABTOIntClr : dword absolute $00000200;

  UART_U0FDR_DIVADDVAL_MASK : dword absolute $0000000F;// Fractional divider: prescaler register
  UART_U0FDR_MULVAL_MASK : dword absolute $000000F0;// Fractional divider: prescaler multiplier

  UART_U0TER_TXEN_MASK : dword absolute $00000080;// UART transmit enable
  UART_U0TER_TXEN_Enabled : dword absolute $00000080;
  UART_U0TER_TXEN_Disabled : dword absolute $00000000;

  UART_U0RS485CTRL_NMMEN_MASK : dword absolute $00000001;// Normal multi-drop mode
  UART_U0RS485CTRL_NMMEN : dword absolute $00000001;
  UART_U0RS485CTRL_RXDIS_MASK : dword absolute $00000002;// Receiver
  UART_U0RS485CTRL_RXDIS : dword absolute $00000002;
  UART_U0RS485CTRL_AADEN_MASK : dword absolute $00000004;// Auto-address detect
  UART_U0RS485CTRL_AADEN : dword absolute $00000004;
  UART_U0RS485CTRL_SEL_MASK : dword absolute $00000008;
  UART_U0RS485CTRL_SEL_RTS : dword absolute $00000000;// Use RTS for direction control
  UART_U0RS485CTRL_SEL_DTS : dword absolute $00000008;// Use DTS for direction control
  UART_U0RS485CTRL_DCTRL_MASK : dword absolute $00000010;// Enable/Disable auto-direction control
  UART_U0RS485CTRL_DCTRL_Disabled : dword absolute $00000000;
  UART_U0RS485CTRL_DCTRL_Enabled : dword absolute $00000010;
  UART_U0RS485CTRL_OINV_MASK : dword absolute $00000020;// Reverse polarity of direction control signal on RTS/DTR pin
  UART_U0RS485CTRL_OINV_Normal : dword absolute $00000000;
  UART_U0RS485CTRL_OINV_Inverted : dword absolute $00000020;

  UART_U0FIFOLVL_RXFIFOLVL_MASK : dword absolute $0000000F;
  UART_U0FIFOLVL_RXFIFOLVL_Empty : dword absolute $00000000;
  UART_U0FIFOLVL_RXFIFOLVL_Full : dword absolute $0000000F;
  UART_U0FIFOLVL_TXFIFOLVL_MASK : dword absolute $00000F00;
  UART_U0FIFOLVL_TXFIFOLVL_Empty : dword absolute $00000000;
  UART_U0FIFOLVL_TXFIFOLVL_Full : dword absolute $00000F00;

(*##############################################################################
## SSP - Synchronous Serial Port
##############################################################################*)

const
  SSP_SSP0_BASE_ADDRESS = $40040000;
var
  SSP_SSP0CR0 : dword absolute $40040000;// Control register 0
  SSP_SSP0CR1 : dword absolute $40040004;// Control register 1
  SSP_SSP0DR : dword absolute $40040008;// Data register
  SSP_SSP0SR : dword absolute $4004000C;// Status register
  SSP_SSP0CPSR : dword absolute $40040010;// Clock prescale register
  SSP_SSP0IMSC : dword absolute $40040014;// Interrupt mask set/clear register
  SSP_SSP0RIS : dword absolute $40040018;// Raw interrupt status register
  SSP_SSP0MIS : dword absolute $4004001C;// Masked interrupt status register
  SSP_SSP0ICR : dword absolute $40040020;// SSPICR interrupt clear register

(*SSP0CR0 (SSP0 Control Register 0)
This register controls the basic operation of the SSP controller. *)

  SSP_SSP0CR0_DSS_MASK : dword absolute $0000000F;// Data size select
  SSP_SSP0CR0_DSS_4BIT : dword absolute $00000003;
  SSP_SSP0CR0_DSS_5BIT : dword absolute $00000004;
  SSP_SSP0CR0_DSS_6BIT : dword absolute $00000005;
  SSP_SSP0CR0_DSS_7BIT : dword absolute $00000006;
  SSP_SSP0CR0_DSS_8BIT : dword absolute $00000007;
  SSP_SSP0CR0_DSS_9BIT : dword absolute $00000008;
  SSP_SSP0CR0_DSS_10BIT : dword absolute $00000009;
  SSP_SSP0CR0_DSS_11BIT : dword absolute $0000000A;
  SSP_SSP0CR0_DSS_12BIT : dword absolute $0000000B;
  SSP_SSP0CR0_DSS_13BIT : dword absolute $0000000C;
  SSP_SSP0CR0_DSS_14BIT : dword absolute $0000000D;
  SSP_SSP0CR0_DSS_15BIT : dword absolute $0000000E;
  SSP_SSP0CR0_DSS_16BIT : dword absolute $0000000F;
  SSP_SSP0CR0_FRF_MASK : dword absolute $00000030;// Frame format
  SSP_SSP0CR0_FRF_SPI : dword absolute $00000000;
  SSP_SSP0CR0_FRF_TI : dword absolute $00000010;
  SSP_SSP0CR0_FRF_MWIRE : dword absolute $00000020;
  SSP_SSP0CR0_CPOL_MASK : dword absolute $00000040;// Clock out polarity
  SSP_SSP0CR0_CPOL_LOW : dword absolute $00000000;
  SSP_SSP0CR0_CPOL_HIGH : dword absolute $00000040;
  SSP_SSP0CR0_CPHA_MASK : dword absolute $00000080;// Clock out phase
  SSP_SSP0CR0_CPHA_FIRST : dword absolute $00000000;
  SSP_SSP0CR0_CPHA_SECOND : dword absolute $00000080;

(*Serial Clock Rate. The number of prescaler-output clocks per
bit on the bus, minus one. Given that CPSDVSR is the
prescale divider, and the APB clock PCLK clocks the
prescaler, the bit frequency is PCLK / (CPSDVSR ? [SCR+1]). *)

  SSP_SSP0CR0_SCR_MASK : dword absolute $0000FF00;// Serial clock rate
  SSP_SSP0CR0_SCR_1 : dword absolute $00000100;
  SSP_SSP0CR0_SCR_2 : dword absolute $00000200;
  SSP_SSP0CR0_SCR_3 : dword absolute $00000300;
  SSP_SSP0CR0_SCR_4 : dword absolute $00000400;
  SSP_SSP0CR0_SCR_5 : dword absolute $00000500;
  SSP_SSP0CR0_SCR_6 : dword absolute $00000600;
  SSP_SSP0CR0_SCR_7 : dword absolute $00000700;
  SSP_SSP0CR0_SCR_8 : dword absolute $00000800;
  SSP_SSP0CR0_SCR_9 : dword absolute $00000900;
  SSP_SSP0CR0_SCR_10 : dword absolute $00000A00;
  SSP_SSP0CR0_SCR_11 : dword absolute $00000B00;
  SSP_SSP0CR0_SCR_12 : dword absolute $00000C00;
  SSP_SSP0CR0_SCR_13 : dword absolute $00000D00;
  SSP_SSP0CR0_SCR_14 : dword absolute $00000E00;
  SSP_SSP0CR0_SCR_15 : dword absolute $00000F00;
  SSP_SSP0CR0_SCR_16 : dword absolute $00001000;

(*SSP0CR1 (SSP0 Control Register 1)
This register controls certain aspects of the operation of the SSP controller. *)

  SSP_SSP0CR1_LBM_MASK : dword absolute $00000001;// Loop back mode
  SSP_SSP0CR1_LBM_NORMAL : dword absolute $00000000;
  SSP_SSP0CR1_LBM_INVERTED : dword absolute $00000001;// MISO/MOSI are reversed
  SSP_SSP0CR1_SSE_MASK : dword absolute $00000002;// SSP enable
  SSP_SSP0CR1_SSE_DISABLED : dword absolute $00000000;
  SSP_SSP0CR1_SSE_ENABLED : dword absolute $00000002;
  SSP_SSP0CR1_MS_MASK : dword absolute $00000004;// Master/Slave Mode
  SSP_SSP0CR1_MS_MASTER : dword absolute $00000000;
  SSP_SSP0CR1_MS_SLAVE : dword absolute $00000004;
  SSP_SSP0CR1_SOD_MASK : dword absolute $00000008;// Slave output disable

(*SSP0DR (SSP0 Data Register)
Software can write data to be transmitted to this register, and read data that has been
received. *)

  SSP_SSP0DR_MASK : dword absolute $0000FFFF;// Data

(*SSP0SR (SSP0 Status Register)
This read-only register reflects the current status of the SSP controller. *)

  SSP_SSP0SR_TFE_MASK : dword absolute $00000001;// Transmit FIFO empty
  SSP_SSP0SR_TFE_EMPTY : dword absolute $00000001;
  SSP_SSP0SR_TFE_NOTEMPTY : dword absolute $00000000;
  SSP_SSP0SR_TNF_MASK : dword absolute $00000002;// Transmit FIFO not full
  SSP_SSP0SR_TNF_NOTFULL : dword absolute $00000002;
  SSP_SSP0SR_TNF_FULL : dword absolute $00000000;
  SSP_SSP0SR_RNE_MASK : dword absolute $00000004;// Receive FIFO not empty
  SSP_SSP0SR_RNE_NOTEMPTY : dword absolute $00000004;
  SSP_SSP0SR_RNE_EMPTY : dword absolute $00000000;
  SSP_SSP0SR_RFF_MASK : dword absolute $00000008;// Receive FIFO full
  SSP_SSP0SR_RFF_FULL : dword absolute $00000008;
  SSP_SSP0SR_RFF_NOTFULL : dword absolute $00000000;
  SSP_SSP0SR_BSY_MASK : dword absolute $00000010;// Busy Flag
  SSP_SSP0SR_BSY_IDLE : dword absolute $00000000;
  SSP_SSP0SR_BSY_BUSY : dword absolute $00000010;

(*SSP0CPSR (SSP0 Clock Prescale Register)
This register controls the factor by which the Prescaler divides the SSP peripheral clock
SSP_PCLK to yield the prescaler clock that is, in turn, divided by the SCR factor in
SSP0CR0, to determine the bit clock. *)

  SSP_SSP0CPSR_CPSDVSR_MASK : dword absolute $000000FF;
  SSP_SSP0CPSR_CPSDVSR_DIV2 : dword absolute $00000002;
  SSP_SSP0CPSR_CPSDVSR_DIV4 : dword absolute $00000004;
  SSP_SSP0CPSR_CPSDVSR_DIV10 : dword absolute $0000000A;
  SSP_SSP0CPSR_CPSDVSR_DIV12 : dword absolute $0000000C;
  SSP_SSP0CPSR_CPSDVSR_DIV16 : dword absolute $00000010;
  SSP_SSP0CPSR_CPSDVSR_DIV20 : dword absolute $00000014;

(*SSP0IMSC (SSP0 Interrupt Mask Set/Clear Register)
This register controls whether each of the four possible interrupt conditions in the SSP
controller are enabled. Note that ARM uses the word Â?maskedÂ? in the opposite sense from
classic computer terminology, in which Â?maskedÂ? meant Â?disabledÂ?. ARM uses the word
Â?maskedÂ? to mean Â?enabledÂ?. To avoid confusion we will not use the word Â?maskedÂ?. *)

  SSP_SSP0IMSC_RORIM_MASK : dword absolute $00000001;// Receive overrun interrupt
  SSP_SSP0IMSC_RORIM_ENBL : dword absolute $00000001;
  SSP_SSP0IMSC_RORIM_DSBL : dword absolute $00000000;
  SSP_SSP0IMSC_RTIM_MASK : dword absolute $00000002;// Receive timeout interrupt
  SSP_SSP0IMSC_RTIM_ENBL : dword absolute $00000002;
  SSP_SSP0IMSC_RTIM_DSBL : dword absolute $00000000;
  SSP_SSP0IMSC_RXIM_MASK : dword absolute $00000004;// Rx FIFO >= 1/2 full interrupt
  SSP_SSP0IMSC_RXIM_ENBL : dword absolute $00000004;
  SSP_SSP0IMSC_RXIM_DSBL : dword absolute $00000000;
  SSP_SSP0IMSC_TXIM_MASK : dword absolute $00000008;// Tx FIFO >= 1/2 empty interrupt
  SSP_SSP0IMSC_TXIM_ENBL : dword absolute $00000008;
  SSP_SSP0IMSC_TXIM_DSBL : dword absolute $00000000;

(*SSP0RIS (SSP0 Raw Interrupt Status Register)
This read-only register contains a 1 for each interrupt condition that is asserted,
regardless of whether or not the interrupt is enabled in the SSP0IMSC. *)

  SSP_SSP0RIS_RORRIS_MASK : dword absolute $00000001;// Frame received while Rx FIFO full
  SSP_SSP0RIS_RORRIS_RCVD : dword absolute $00000001;
  SSP_SSP0RIS_RTRIS_MASK : dword absolute $00000002;// Rx FIFO not empty no read within timeout
  SSP_SSP0RIS_RTRIS_NOTEMPTY : dword absolute $00000002;
  SSP_SSP0RIS_RXRIS_MASK : dword absolute $00000004;// Rx FIFO >= half full
  SSP_SSP0RIS_RXRIS_HALFFULL : dword absolute $00000004;
  SSP_SSP0RIS_TXRIS_MASK : dword absolute $00000008;// Tx FIF0 >= half-empty
  SSP_SSP0RIS_TXRIS_HALFEMPTY : dword absolute $00000008;

(*SSP0MIS (SSP0 Masked Interrupt Status Register)
This read-only register contains a 1 for each interrupt condition that is asserted and
enabled in the SSP0IMSC. When an SSP interrupt occurs, the interrupt service routine
should read this register to determine the cause(s) of the interrupt. *)

  SSP_SSP0MIS_RORMIS_MASK : dword absolute $00000001;// Frame received while Rx FIFO full
  SSP_SSP0MIS_RORMIS_FRMRCVD : dword absolute $00000001;
  SSP_SSP0MIS_RTMIS_MASK : dword absolute $00000002;// Rx FIFO not empty no read withing timeout
  SSP_SSP0MIS_RTMIS_NOTEMPTY : dword absolute $00000002;
  SSP_SSP0MIS_RXMIS_MASK : dword absolute $00000004;// Rx FIFO >= half full
  SSP_SSP0MIS_RXMIS_HALFFULL : dword absolute $00000004;
  SSP_SSP0MIS_TXMIS_MASK : dword absolute $00000008;// Tx FIFO >= half-empty
  SSP_SSP0MIS_TXMIS_HALFEMPTY : dword absolute $00000008;

(*SSP0ICR (SSP0 Interrupt Clear Register)
Software can write one or more one(s) to this write-only register, to clear the
corresponding interrupt condition(s) in the SSP controller. Note that the other two interrupt
conditions can be cleared by writing or reading the appropriate FIFO, or disabled by
clearing the corresponding bit in SSP0IMSC. *)

  SSP_SSP0ICR_RORIC_MASK : dword absolute $00000001;// Clears RORIC interrupt flag
  SSP_SSP0ICR_RORIC_CLEAR : dword absolute $00000001;
  SSP_SSP0ICR_RTIC_MASK : dword absolute $00000002;// Clear Rx FIFO not empty/no read flag
  SSP_SSP0ICR_RTIC_CLEAR : dword absolute $00000002;

(*##############################################################################
## I2C
##############################################################################*)
const
  I2C_BASE_ADDRESS = $40000000;

var
  I2C_I2CCONSET : dword absolute $40000000;// I2C control set register
  I2C_I2CSTAT : dword absolute $40000004;// I2C status register
  I2C_I2CDAT : dword absolute $40000008;// I2C data register
  I2C_I2CADR0 : dword absolute $4000000C;// I2C slave address register
  I2C_I2CSCLH : dword absolute $40000010;// I2C SCL HIGH/LOW duty cycle register
  I2C_I2CSCLL : dword absolute $40000014;
  I2C_I2CCONCLR : dword absolute $40000018;// I2C control clear register
  I2C_I2CMMCTRL : dword absolute $4000001C;// I2C monitor control register
  I2C_I2CADR1 : dword absolute $40000020;// I2C slave address register 1
  I2C_I2CADR2 : dword absolute $40000024;// I2C slave address register 2
  I2C_I2CADR3 : dword absolute $40000028;// I2C slave address register 3
  I2C_I2CDATA_BUFFER : dword absolute $4000002C;// I2C data buffer register
  I2C_I2CMASK0 : dword absolute $40000030;// I2C mask register 0
  I2C_I2CMASK1 : dword absolute $40000034;// I2C mask register 1
  I2C_I2CMASK2 : dword absolute $40000038;// I2C mask register 2
  I2C_I2CMASK3 : dword absolute $4000003C;// I2C mask register 3

(*I2CCONSET (I2C Control Set register)
The I2CONSET registers control setting of bits in the I2CON register that controls
operation of the I2C interface. Writing a one to a bit of this register causes the
corresponding bit in the I2C control register to be set. Writing a zero has no effect. *)

  I2C_I2CCONSET_AA_MASK : dword absolute $00000004;
  I2C_I2CCONSET_AA : dword absolute $00000004;// Asset acknowlegde flag
  I2C_I2CCONSET_SI_MASK : dword absolute $00000008;
  I2C_I2CCONSET_SI : dword absolute $00000008;// I2C interrupt flag
  I2C_I2CCONSET_STO_MASK : dword absolute $00000010;
  I2C_I2CCONSET_STO : dword absolute $00000010;// Stop flag
  I2C_I2CCONSET_STA_MASK : dword absolute $00000020;
  I2C_I2CCONSET_STA : dword absolute $00000020;// Start flag
  I2C_I2CCONSET_I2EN_MASK : dword absolute $00000040;
  I2C_I2CCONSET_I2EN : dword absolute $00000040;// I2C interface enable

(*I2CSTAT (I2C Status register)
Each I2C Status register reflects the condition of the corresponding I2C interface. The I2C
Status register is Read-Only. *)

  I2C_I2CSTAT_Status_MASK : dword absolute $000000F8;// Status information

(*I2CADR0 (I2C Slave Address register)
These registers are readable and writable and are only used when an I2C interface is set
to slave mode. *)

  I2C_I2CADR0_GC_MASK : dword absolute $00000001;
  I2C_I2CADR0_GC : dword absolute $00000001;// General call enable bit
  I2C_I2CADR0_Address_MASK : dword absolute $000000FE;// I2C device address for slave mode

(*I2CCONCLR (I2C Control Clear register)
The I2CONCLR registers control clearing of bits in the I2CON register that controls
operation of the I2C interface. Writing a one to a bit of this register causes the
corresponding bit in the I2C control register to be cleared. Writing a zero has no effect. *)

  I2C_I2CCONCLR_AAC_MASK : dword absolute $00000004;// Assert acknowledge clear bit
  I2C_I2CCONCLR_AAC : dword absolute $00000004;
  I2C_I2CCONCLR_SIC_MASK : dword absolute $00000008;// I2C interrupt clear bit
  I2C_I2CCONCLR_SIC : dword absolute $00000008;
  I2C_I2CCONCLR_STAC_MASK : dword absolute $00000020;// Start flag clear bit
  I2C_I2CCONCLR_STAC : dword absolute $00000020;
  I2C_I2CCONCLR_I2ENC_MASK : dword absolute $00000040;// I2C interface disable bit
  I2C_I2CCONCLR_I2ENC : dword absolute $00000040;

(*I2CMMCTRL (I2C Monitor mode control register)
This register controls the Monitor mode which allows the I2C module to monitor traffic on
the I2C bus without actually participating in traffic or interfering with the I2C bus. *)

  I2C_I2CMMCTRL_MM_ENA_MASK : dword absolute $00000001;// Monitor mode enable
  I2C_I2CMMCTRL_MM_ENA_ENABLED : dword absolute $00000001;
  I2C_I2CMMCTRL_MM_ENA_DISABLED : dword absolute $00000000;
  I2C_I2CMMCTRL_ENA_SCL_MASK : dword absolute $00000002;// SCL output enable
  I2C_I2CMMCTRL_ENA_SCL_HOLDLOW : dword absolute $00000002;
  I2C_I2CMMCTRL_ENA_SCL_FORCEHIGH : dword absolute $00000000;
  I2C_I2CMMCTRL_MATCH_ALL_MASK : dword absolute $00000008;// Select interrupt register match
  I2C_I2CMMCTRL_MATCH_ALL_NORMAL : dword absolute $00000000;
  I2C_I2CMMCTRL_MATCH_ALL_ANYADDRESS : dword absolute $00000008;

(*I2CADR1..3 (I2C Slave Address registers)
These registers are readable and writable and are only used when an I2C interface is set
to slave mode. In master mode, this register has no effect. The LSB of I2ADR is the
General Call bit. When this bit is set, the General Call address $00) is recognized. *)

  I2C_I2CADR1_GC_MASK : dword absolute $00000001;// General call enable bit
  I2C_I2CADR1_GC : dword absolute $00000001;
  I2C_I2CADR1_Address_MASK : dword absolute $000000FE;

  I2C_I2CADR2_GC_MASK : dword absolute $00000001;// General call enable bit
  I2C_I2CADR2_GC : dword absolute $00000001;
  I2C_I2CADR2_Address_MASK : dword absolute $000000FE;

  I2C_I2CADR3_GC_MASK : dword absolute $00000001;// General call enable bit
  I2C_I2CADR3_GC : dword absolute $00000001;
  I2C_I2CADR3_Address_MASK : dword absolute $000000FE;

(*I2CMASK0..3 (I2C Mask registers) *)

  I2C_I2CMASK0_MASK_MASK : dword absolute $000000FE;
  I2C_I2CMASK1_MASK_MASK : dword absolute $000000FE;
  I2C_I2CMASK2_MASK_MASK : dword absolute $000000FE;
  I2C_I2CMASK3_MASK_MASK : dword absolute $000000FE;

(*##############################################################################
## 16-Bit Timers (CT16B0/1)
##############################################################################*)
const
  TMR_CT16B0_BASE_ADDRESS = $4000C000;
var
  TMR_TMR16B0IR : dword absolute $4000C000;// Interrupt register
  TMR_TMR16B0TCR : dword absolute $4000C004;// Timer control register
  TMR_TMR16B0TC : dword absolute $4000C008;// Timer counter
  TMR_TMR16B0PR : dword absolute $4000C00C;// Prescale register
  TMR_TMR16B0PC : dword absolute $4000C010;// Prescale counter register
  TMR_TMR16B0MCR : dword absolute $4000C014;// Match control register
  TMR_TMR16B0MR0 : dword absolute $4000C018;// Match register 0
  TMR_TMR16B0MR1 : dword absolute $4000C01C;// Match register 1
  TMR_TMR16B0MR2 : dword absolute $4000C020;// Match register 2
  TMR_TMR16B0MR3 : dword absolute $4000C024;// Match register 3
  TMR_TMR16B0CCR : dword absolute $4000C028;// Capture control register
  TMR_TMR16B0CR0 : dword absolute $4000C02C;// Capture register
  TMR_TMR16B0EMR : dword absolute $4000C03C;// External match register
  TMR_TMR16B0CTCR : dword absolute $4000C070;// Count control register
  TMR_TMR16B0PWMC : dword absolute $4000C074;// PWM control register

  TMR_TMR16B0IR_MR0_MASK : dword absolute $00000001;// Interrupt flag for match channel 0
  TMR_TMR16B0IR_MR0 : dword absolute $00000001;
  TMR_TMR16B0IR_MR1_MASK : dword absolute $00000002;// Interrupt flag for match channel 1
  TMR_TMR16B0IR_MR1 : dword absolute $00000002;
  TMR_TMR16B0IR_MR2_MASK : dword absolute $00000004;// Interrupt flag for match channel 2
  TMR_TMR16B0IR_MR2 : dword absolute $00000004;
  TMR_TMR16B0IR_MR3_MASK : dword absolute $00000008;// Interrupt flag for match channel 3
  TMR_TMR16B0IR_MR3 : dword absolute $00000008;
  TMR_TMR16B0IR_CR0_MASK : dword absolute $00000010;// Interrupt flag for capture channel 0 event
  TMR_TMR16B0IR_CR0 : dword absolute $00000010;
  TMR_TMR16B0IR_MASK_ALL : dword absolute $0000001F;

  TMR_TMR16B0TCR_COUNTERENABLE_MASK : dword absolute $00000001;// Counter enable
  TMR_TMR16B0TCR_COUNTERENABLE_ENABLED : dword absolute $00000001;
  TMR_TMR16B0TCR_COUNTERENABLE_DISABLED : dword absolute $00000000;
  TMR_TMR16B0TCR_COUNTERRESET_MASK : dword absolute $00000002;
  TMR_TMR16B0TCR_COUNTERRESET_ENABLED : dword absolute $00000002;
  TMR_TMR16B0TCR_COUNTERRESET_DISABLED : dword absolute $00000000;

  TMR_TMR16B0MCR_MR0_INT_MASK : dword absolute $00000001;// Interrupt on MRO
  TMR_TMR16B0MCR_MR0_INT_ENABLED : dword absolute $00000001;
  TMR_TMR16B0MCR_MR0_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR0_RESET_MASK : dword absolute $00000002;// Reset on MR0
  TMR_TMR16B0MCR_MR0_RESET_ENABLED : dword absolute $00000002;
  TMR_TMR16B0MCR_MR0_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR0_STOP_MASK : dword absolute $00000004;// Stop on MR0
  TMR_TMR16B0MCR_MR0_STOP_ENABLED : dword absolute $00000004;
  TMR_TMR16B0MCR_MR0_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR1_INT_MASK : dword absolute $00000008;// Interrupt on MR1
  TMR_TMR16B0MCR_MR1_INT_ENABLED : dword absolute $00000008;
  TMR_TMR16B0MCR_MR1_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR1_RESET_MASK : dword absolute $00000010;// Reset on MR1
  TMR_TMR16B0MCR_MR1_RESET_ENABLED : dword absolute $00000010;
  TMR_TMR16B0MCR_MR1_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR1_STOP_MASK : dword absolute $00000020;// Stop on MR1
  TMR_TMR16B0MCR_MR1_STOP_ENABLED : dword absolute $00000020;
  TMR_TMR16B0MCR_MR1_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR2_INT_MASK : dword absolute $00000040;// Interrupt on MR2
  TMR_TMR16B0MCR_MR2_INT_ENABLED : dword absolute $00000040;
  TMR_TMR16B0MCR_MR2_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR2_RESET_MASK : dword absolute $00000080;// Reset on MR2
  TMR_TMR16B0MCR_MR2_RESET_ENABLED : dword absolute $00000080;
  TMR_TMR16B0MCR_MR2_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR2_STOP_MASK : dword absolute $00000100;// Stop on MR2
  TMR_TMR16B0MCR_MR2_STOP_ENABLED : dword absolute $00000100;
  TMR_TMR16B0MCR_MR2_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR3_INT_MASK : dword absolute $00000200;// Interrupt on MR3
  TMR_TMR16B0MCR_MR3_INT_ENABLED : dword absolute $00000200;
  TMR_TMR16B0MCR_MR3_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR3_RESET_MASK : dword absolute $00000400;// Reset on MR3
  TMR_TMR16B0MCR_MR3_RESET_ENABLED : dword absolute $00000400;
  TMR_TMR16B0MCR_MR3_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B0MCR_MR3_STOP_MASK : dword absolute $00000800;// Stop on MR3
  TMR_TMR16B0MCR_MR3_STOP_ENABLED : dword absolute $00000800;
  TMR_TMR16B0MCR_MR3_STOP_DISABLED : dword absolute $00000000;

  TMR_TMR16B0CCR_CAP0RE_MASK : dword absolute $00000001;// Capture on rising edge
  TMR_TMR16B0CCR_CAP0RE_ENABLED : dword absolute $00000001;
  TMR_TMR16B0CCR_CAP0RE_DISABLED : dword absolute $00000000;
  TMR_TMR16B0CCR_CAP0FE_MASK : dword absolute $00000002;// Capture on falling edge
  TMR_TMR16B0CCR_CAP0FE_ENABLED : dword absolute $00000002;
  TMR_TMR16B0CCR_CAP0FE_DISABLED : dword absolute $00000000;
  TMR_TMR16B0CCR_CAP0I_MASK : dword absolute $00000004;// Interrupt on CAP0 event
  TMR_TMR16B0CCR_CAP0I_ENABLED : dword absolute $00000004;
  TMR_TMR16B0CCR_CAP0I_DISABLED : dword absolute $00000000;

  TMR_TMR16B0EMR_EM0_MASK : dword absolute $00000001;// External match 0
  TMR_TMR16B0EMR_EM0 : dword absolute $00000001;
  TMR_TMR16B0EMR_EMC0_MASK : dword absolute $00000030;
  TMR_TMR16B0EMR_EMC0_DONOTHING : dword absolute $00000000;
  TMR_TMR16B0EMR_EMC0_LOW : dword absolute $00000010;
  TMR_TMR16B0EMR_EMC0_HIGH : dword absolute $00000020;
  TMR_TMR16B0EMR_EMC0_TOGGLE : dword absolute $00000030;
  TMR_TMR16B0EMR_EM1_MASK : dword absolute $00000002;// External match 1
  TMR_TMR16B0EMR_EM1 : dword absolute $00000002;
  TMR_TMR16B0EMR_EMC1_MASK : dword absolute $000000C0;
  TMR_TMR16B0EMR_EMC1_DONOTHING : dword absolute $00000000;
  TMR_TMR16B0EMR_EMC1_LOW : dword absolute $00000040;
  TMR_TMR16B0EMR_EMC1_HIGH : dword absolute $00000080;
  TMR_TMR16B0EMR_EMC1_TOGGLE : dword absolute $000000C0;
  TMR_TMR16B0EMR_EM2_MASK : dword absolute $00000004;// External match 2
  TMR_TMR16B0EMR_EM2 : dword absolute $00000004;
  TMR_TMR16B0EMR_EMC2_MASK : dword absolute $00000300;
  TMR_TMR16B0EMR_EMC2_DONOTHING : dword absolute $00000000;
  TMR_TMR16B0EMR_EMC2_LOW : dword absolute $00000100;
  TMR_TMR16B0EMR_EMC2_HIGH : dword absolute $00000200;
  TMR_TMR16B0EMR_EMC2_TOGGLE : dword absolute $00000300;
  TMR_TMR16B0EMR_EM3_MASK : dword absolute $00000008;// External match 3
  TMR_TMR16B0EMR_EM3 : dword absolute $00000008;
  TMR_TMR16B0EMR_EMC3_MASK : dword absolute $00000C00;
  TMR_TMR16B0EMR_EMC3_DONOTHING : dword absolute $00000000;
  TMR_TMR16B0EMR_EMC3_LOW : dword absolute $00000400;
  TMR_TMR16B0EMR_EMC3_HIGH : dword absolute $00000800;
  TMR_TMR16B0EMR_EMC3_TOGGLE : dword absolute $00000C00;

  TMR_TMR16B0CTCR_CTMODE_MASK : dword absolute $00000003;// Counter/Timer mode
  TMR_TMR16B0CTCR_CTMODE_TIMER : dword absolute $00000000;// Timer Mode: Every rising PCLK edge
  TMR_TMR16B0CTCR_CTMODE_COUNTERRISING : dword absolute $00000001;// Counter: TC increments on rising edge of input
  TMR_TMR16B0CTCR_CTMODE_COUNTERFALLING : dword absolute $00000002;// Counter: TC increments on falling edge of input
  TMR_TMR16B0CTCR_CTMODE_COUNTERBOTH : dword absolute $00000003;// Counter: TC increments on both edges of input
  TMR_TMR16B0CTCR_CINPUTSELECT_MASK : dword absolute $0000000C;
  TMR_TMR16B0CTCR_CINPUTSELECT : dword absolute $00000000;// CINPUTSELECT must be set to 00

  TMR_TMR16B0PWMC_PWM0_MASK : dword absolute $00000001;
  TMR_TMR16B0PWMC_PWM0_ENABLED : dword absolute $00000001;// PWM mode is enabled for CT16Bn_MAT0
  TMR_TMR16B0PWMC_PWM0_DISABLED : dword absolute $00000000;
  TMR_TMR16B0PWMC_PWM1_MASK : dword absolute $00000002;
  TMR_TMR16B0PWMC_PWM1_ENABLED : dword absolute $00000002;// PWM mode is enabled for CT16Bn_MAT1
  TMR_TMR16B0PWMC_PWM1_DISABLED : dword absolute $00000000;
  TMR_TMR16B0PWMC_PWM2_MASK : dword absolute $00000004;
  TMR_TMR16B0PWMC_PWM2_ENABLED : dword absolute $00000004;// PWM mode is enabled for CT16Bn_MAT2
  TMR_TMR16B0PWMC_PWM2_DISABLED : dword absolute $00000000;
  TMR_TMR16B0PWMC_PWM3_MASK : dword absolute $00000008;
  TMR_TMR16B0PWMC_PWM3_ENABLED : dword absolute $00000008;
  TMR_TMR16B0PWMC_PWM3_DISABLED : dword absolute $00000000;

const

  TMR_CT16B1_BASE_ADDRESS = $40010000;

var
  TMR_TMR16B1IR : dword absolute $40010000;// Interrupt register
  TMR_TMR16B1TCR : dword absolute $40010004;// Timer control register
  TMR_TMR16B1TC : dword absolute $40010008;// Timer counter
  TMR_TMR16B1PR : dword absolute $4001000C;// Prescale register
  TMR_TMR16B1PC : dword absolute $40010010;// Prescale counter register
  TMR_TMR16B1MCR : dword absolute $40010014;// Match control register
  TMR_TMR16B1MR0 : dword absolute $40010018;// Match register 0
  TMR_TMR16B1MR1 : dword absolute $4001001C;// Match register 1
  TMR_TMR16B1MR2 : dword absolute $40010020;// Match register 2
  TMR_TMR16B1MR3 : dword absolute $40010024;// Match register 3
  TMR_TMR16B1CCR : dword absolute $40010028;// Capture control register
  TMR_TMR16B1CR0 : dword absolute $4001002C;// Capture register
  TMR_TMR16B1EMR : dword absolute $4001003C;// External match register
  TMR_TMR16B1CTCR : dword absolute $40010070;// Count control register
  TMR_TMR16B1PWMC : dword absolute $40010074;// PWM control register

  TMR_TMR16B1IR_MR0_MASK : dword absolute $00000001;// Interrupt flag for match channel 0
  TMR_TMR16B1IR_MR0 : dword absolute $00000001;
  TMR_TMR16B1IR_MR1_MASK : dword absolute $00000002;// Interrupt flag for match channel 1
  TMR_TMR16B1IR_MR1 : dword absolute $00000002;
  TMR_TMR16B1IR_MR2_MASK : dword absolute $00000004;// Interrupt flag for match channel 2
  TMR_TMR16B1IR_MR2 : dword absolute $00000004;
  TMR_TMR16B1IR_MR3_MASK : dword absolute $00000008;// Interrupt flag for match channel 3
  TMR_TMR16B1IR_MR3 : dword absolute $00000008;
  TMR_TMR16B1IR_CR0_MASK : dword absolute $00000010;// Interrupt flag for capture channel 0 event
  TMR_TMR16B1IR_CR0 : dword absolute $00000010;
  TMR_TMR16B1IR_MASK_ALL : dword absolute $0000001F;

  TMR_TMR16B1TCR_COUNTERENABLE_MASK : dword absolute $00000001;// Counter enable
  TMR_TMR16B1TCR_COUNTERENABLE_ENABLED : dword absolute $00000001;
  TMR_TMR16B1TCR_COUNTERENABLE_DISABLED : dword absolute $00000000;
  TMR_TMR16B1TCR_COUNTERRESET_MASK : dword absolute $00000002;
  TMR_TMR16B1TCR_COUNTERRESET_ENABLED : dword absolute $00000002;
  TMR_TMR16B1TCR_COUNTERRESET_DISABLED : dword absolute $00000000;

  TMR_TMR16B1MCR_MR0_INT_MASK : dword absolute $00000001;// Interrupt on MRO
  TMR_TMR16B1MCR_MR0_INT_ENABLED : dword absolute $00000001;
  TMR_TMR16B1MCR_MR0_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR0_RESET_MASK : dword absolute $00000002;// Reset on MR0
  TMR_TMR16B1MCR_MR0_RESET_ENABLED : dword absolute $00000002;
  TMR_TMR16B1MCR_MR0_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR0_STOP_MASK : dword absolute $00000004;// Stop on MR0
  TMR_TMR16B1MCR_MR0_STOP_ENABLED : dword absolute $00000004;
  TMR_TMR16B1MCR_MR0_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR1_INT_MASK : dword absolute $00000008;// Interrupt on MR1
  TMR_TMR16B1MCR_MR1_INT_ENABLED : dword absolute $00000008;
  TMR_TMR16B1MCR_MR1_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR1_RESET_MASK : dword absolute $00000010;// Reset on MR1
  TMR_TMR16B1MCR_MR1_RESET_ENABLED : dword absolute $00000010;
  TMR_TMR16B1MCR_MR1_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR1_STOP_MASK : dword absolute $00000020;// Stop on MR1
  TMR_TMR16B1MCR_MR1_STOP_ENABLED : dword absolute $00000020;
  TMR_TMR16B1MCR_MR1_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR2_INT_MASK : dword absolute $00000040;// Interrupt on MR2
  TMR_TMR16B1MCR_MR2_INT_ENABLED : dword absolute $00000040;
  TMR_TMR16B1MCR_MR2_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR2_RESET_MASK : dword absolute $00000080;// Reset on MR2
  TMR_TMR16B1MCR_MR2_RESET_ENABLED : dword absolute $00000080;
  TMR_TMR16B1MCR_MR2_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR2_STOP_MASK : dword absolute $00000100;// Stop on MR2
  TMR_TMR16B1MCR_MR2_STOP_ENABLED : dword absolute $00000100;
  TMR_TMR16B1MCR_MR2_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR3_INT_MASK : dword absolute $00000200;// Interrupt on MR3
  TMR_TMR16B1MCR_MR3_INT_ENABLED : dword absolute $00000200;
  TMR_TMR16B1MCR_MR3_INT_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR3_RESET_MASK : dword absolute $00000400;// Reset on MR3
  TMR_TMR16B1MCR_MR3_RESET_ENABLED : dword absolute $00000400;
  TMR_TMR16B1MCR_MR3_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR16B1MCR_MR3_STOP_MASK : dword absolute $00000800;// Stop on MR3
  TMR_TMR16B1MCR_MR3_STOP_ENABLED : dword absolute $00000800;
  TMR_TMR16B1MCR_MR3_STOP_DISABLED : dword absolute $00000000;

  TMR_TMR16B1CCR_CAP0RE_MASK : dword absolute $00000001;// Capture on rising edge
  TMR_TMR16B1CCR_CAP0RE_ENABLED : dword absolute $00000001;
  TMR_TMR16B1CCR_CAP0RE_DISABLED : dword absolute $00000000;
  TMR_TMR16B1CCR_CAP0FE_MASK : dword absolute $00000002;// Capture on falling edge
  TMR_TMR16B1CCR_CAP0FE_ENABLED : dword absolute $00000002;
  TMR_TMR16B1CCR_CAP0FE_DISABLED : dword absolute $00000000;
  TMR_TMR16B1CCR_CAP0I_MASK : dword absolute $00000004;// Interrupt on CAP0 event
  TMR_TMR16B1CCR_CAP0I_ENABLED : dword absolute $00000004;
  TMR_TMR16B1CCR_CAP0I_DISABLED : dword absolute $00000000;

  TMR_TMR16B1EMR_EM0_MASK : dword absolute $00000001;// External match 0
  TMR_TMR16B1EMR_EM0 : dword absolute $00000001;
  TMR_TMR16B1EMR_EMC0_MASK : dword absolute $00000030;
  TMR_TMR16B1EMR_EMC0_DONOTHING : dword absolute $00000000;
  TMR_TMR16B1EMR_EMC0_LOW : dword absolute $00000010;
  TMR_TMR16B1EMR_EMC0_HIGH : dword absolute $00000020;
  TMR_TMR16B1EMR_EMC0_TOGGLE : dword absolute $00000030;
  TMR_TMR16B1EMR_EM1_MASK : dword absolute $00000002;// External match 1
  TMR_TMR16B1EMR_EM1 : dword absolute $00000002;
  TMR_TMR16B1EMR_EMC1_MASK : dword absolute $000000C0;
  TMR_TMR16B1EMR_EMC1_DONOTHING : dword absolute $00000000;
  TMR_TMR16B1EMR_EMC1_LOW : dword absolute $00000040;
  TMR_TMR16B1EMR_EMC1_HIGH : dword absolute $00000080;
  TMR_TMR16B1EMR_EMC1_TOGGLE : dword absolute $000000C0;
  TMR_TMR16B1EMR_EM2_MASK : dword absolute $00000004;// External match 2
  TMR_TMR16B1EMR_EM2 : dword absolute $00000004;
  TMR_TMR16B1EMR_EMC2_MASK : dword absolute $00000300;
  TMR_TMR16B1EMR_EMC2_DONOTHING : dword absolute $00000000;
  TMR_TMR16B1EMR_EMC2_LOW : dword absolute $00000100;
  TMR_TMR16B1EMR_EMC2_HIGH : dword absolute $00000200;
  TMR_TMR16B1EMR_EMC2_TOGGLE : dword absolute $00000300;
  TMR_TMR16B1EMR_EM3_MASK : dword absolute $00000008;// External match 3
  TMR_TMR16B1EMR_EM3 : dword absolute $00000008;
  TMR_TMR16B1EMR_EMC3_MASK : dword absolute $00000C00;
  TMR_TMR16B1EMR_EMC3_DONOTHING : dword absolute $00000000;
  TMR_TMR16B1EMR_EMC3_LOW : dword absolute $00000400;
  TMR_TMR16B1EMR_EMC3_HIGH : dword absolute $00000800;
  TMR_TMR16B1EMR_EMC3_TOGGLE : dword absolute $00000C00;

  TMR_TMR16B1CTCR_CTMODE_MASK : dword absolute $00000003;// Counter/Timer mode
  TMR_TMR16B1CTCR_CTMODE_TIMER : dword absolute $00000000;// Timer Mode: Every rising PCLK edge
  TMR_TMR16B1CTCR_CTMODE_COUNTERRISING : dword absolute $00000001;// Counter: TC increments on rising edge of input
  TMR_TMR16B1CTCR_CTMODE_COUNTERFALLING : dword absolute $00000002;// Counter: TC increments on falling edge of input
  TMR_TMR16B1CTCR_CTMODE_COUNTERBOTH : dword absolute $00000003;// Counter: TC increments on both edges of input
  TMR_TMR16B1CTCR_CINPUTSELECT_MASK : dword absolute $0000000C;
  TMR_TMR16B1CTCR_CINPUTSELECT : dword absolute $00000000;// CINPUTSELECT must be set to 00

  TMR_TMR16B1PWMC_PWM0_MASK : dword absolute $00000001;
  TMR_TMR16B1PWMC_PWM0_ENABLED : dword absolute $00000001;// PWM mode is enabled for CT16Bn_MAT0
  TMR_TMR16B1PWMC_PWM0_DISABLED : dword absolute $00000000;
  TMR_TMR16B1PWMC_PWM1_MASK : dword absolute $00000002;
  TMR_TMR16B1PWMC_PWM1_ENABLED : dword absolute $00000002;// PWM mode is enabled for CT16Bn_MAT1
  TMR_TMR16B1PWMC_PWM1_DISABLED : dword absolute $00000000;
  TMR_TMR16B1PWMC_PWM2_MASK : dword absolute $00000004;
  TMR_TMR16B1PWMC_PWM2_ENABLED : dword absolute $00000004;// PWM mode is enabled for CT16Bn_MAT2
  TMR_TMR16B1PWMC_PWM2_DISABLED : dword absolute $00000000;
  TMR_TMR16B1PWMC_PWM3_MASK : dword absolute $00000008;
  TMR_TMR16B1PWMC_PWM3_ENABLED : dword absolute $00000008;
  TMR_TMR16B1PWMC_PWM3_DISABLED : dword absolute $00000000;

(*##############################################################################
## 32-Bit Timers (CT32B0/1)
##############################################################################*)
const
  TMR_CT32B0_BASE_ADDRESS = $40014000;
var
  TMR_TMR32B0IR : dword absolute $40014000;// Interrupt register
  TMR_TMR32B0TCR : dword absolute $40014004;// Timer control register
  TMR_TMR32B0TC : dword absolute $40014008;// Timer counter
  TMR_TMR32B0PR : dword absolute $4001400C;// Prescale register
  TMR_TMR32B0PC : dword absolute $40014010;// Prescale counter register
  TMR_TMR32B0MCR : dword absolute $40014014;// Match control register
  TMR_TMR32B0MR0 : dword absolute $40014018;// Match register 0
  TMR_TMR32B0MR1 : dword absolute $4001401C;// Match register 1
  TMR_TMR32B0MR2 : dword absolute $40014020;// Match register 2
  TMR_TMR32B0MR3 : dword absolute $40014024;// Match register 3
  TMR_TMR32B0CCR : dword absolute $40014028;// Capture control register
  TMR_TMR32B0CR0 : dword absolute $4001402C;// Capture register
  TMR_TMR32B0EMR : dword absolute $4001403C;// External match register
  TMR_TMR32B0CTCR : dword absolute $40014070;// Count control register
  TMR_TMR32B0PWMC : dword absolute $40014074;// PWM control register

  TMR_TMR32B0IR_MR0_MASK : dword absolute $00000001;// Interrupt flag for match channel 0
  TMR_TMR32B0IR_MR0 : dword absolute $00000001;
  TMR_TMR32B0IR_MR1_MASK : dword absolute $00000002;// Interrupt flag for match channel 1
  TMR_TMR32B0IR_MR1 : dword absolute $00000002;
  TMR_TMR32B0IR_MR2_MASK : dword absolute $00000004;// Interrupt flag for match channel 2
  TMR_TMR32B0IR_MR2 : dword absolute $00000004;
  TMR_TMR32B0IR_MR3_MASK : dword absolute $00000008;// Interrupt flag for match channel 3
  TMR_TMR32B0IR_MR3 : dword absolute $00000008;
  TMR_TMR32B0IR_CR0_MASK : dword absolute $00000010;// Interrupt flag for capture channel 0 event
  TMR_TMR32B0IR_CR0 : dword absolute $00000010;
  TMR_TMR32B0IR_MASK_ALL : dword absolute $0000001F;

  TMR_TMR32B0TCR_COUNTERENABLE_MASK : dword absolute $00000001;// Counter enable
  TMR_TMR32B0TCR_COUNTERENABLE_ENABLED : dword absolute $00000001;
  TMR_TMR32B0TCR_COUNTERENABLE_DISABLED : dword absolute $00000000;
  TMR_TMR32B0TCR_COUNTERRESET_MASK : dword absolute $00000002;
  TMR_TMR32B0TCR_COUNTERRESET_ENABLED : dword absolute $00000002;
  TMR_TMR32B0TCR_COUNTERRESET_DISABLED : dword absolute $00000000;

  TMR_TMR32B0MCR_MR0_INT_MASK : dword absolute $00000001;// Interrupt on MRO
  TMR_TMR32B0MCR_MR0_INT_ENABLED : dword absolute $00000001;
  TMR_TMR32B0MCR_MR0_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR0_RESET_MASK : dword absolute $00000002;// Reset on MR0
  TMR_TMR32B0MCR_MR0_RESET_ENABLED : dword absolute $00000002;
  TMR_TMR32B0MCR_MR0_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR0_STOP_MASK : dword absolute $00000004;// Stop on MR0
  TMR_TMR32B0MCR_MR0_STOP_ENABLED : dword absolute $00000004;
  TMR_TMR32B0MCR_MR0_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR1_INT_MASK : dword absolute $00000008;// Interrupt on MR1
  TMR_TMR32B0MCR_MR1_INT_ENABLED : dword absolute $00000008;
  TMR_TMR32B0MCR_MR1_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR1_RESET_MASK : dword absolute $00000010;// Reset on MR1
  TMR_TMR32B0MCR_MR1_RESET_ENABLED : dword absolute $00000010;
  TMR_TMR32B0MCR_MR1_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR1_STOP_MASK : dword absolute $00000020;// Stop on MR1
  TMR_TMR32B0MCR_MR1_STOP_ENABLED : dword absolute $00000020;
  TMR_TMR32B0MCR_MR1_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR2_INT_MASK : dword absolute $00000040;// Interrupt on MR2
  TMR_TMR32B0MCR_MR2_INT_ENABLED : dword absolute $00000040;
  TMR_TMR32B0MCR_MR2_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR2_RESET_MASK : dword absolute $00000080;// Reset on MR2
  TMR_TMR32B0MCR_MR2_RESET_ENABLED : dword absolute $00000080;
  TMR_TMR32B0MCR_MR2_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR2_STOP_MASK : dword absolute $00000100;// Stop on MR2
  TMR_TMR32B0MCR_MR2_STOP_ENABLED : dword absolute $00000100;
  TMR_TMR32B0MCR_MR2_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR3_INT_MASK : dword absolute $00000200;// Interrupt on MR3
  TMR_TMR32B0MCR_MR3_INT_ENABLED : dword absolute $00000200;
  TMR_TMR32B0MCR_MR3_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR3_RESET_MASK : dword absolute $00000400;// Reset on MR3
  TMR_TMR32B0MCR_MR3_RESET_ENABLED : dword absolute $00000400;
  TMR_TMR32B0MCR_MR3_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B0MCR_MR3_STOP_MASK : dword absolute $00000800;// Stop on MR3
  TMR_TMR32B0MCR_MR3_STOP_ENABLED : dword absolute $00000800;
  TMR_TMR32B0MCR_MR3_STOP_DISABLED : dword absolute $00000000;

  TMR_TMR32B0CCR_CAP0RE_MASK : dword absolute $00000001;// Capture on rising edge
  TMR_TMR32B0CCR_CAP0RE_ENABLED : dword absolute $00000001;
  TMR_TMR32B0CCR_CAP0RE_DISABLED : dword absolute $00000000;
  TMR_TMR32B0CCR_CAP0FE_MASK : dword absolute $00000002;// Capture on falling edge
  TMR_TMR32B0CCR_CAP0FE_ENABLED : dword absolute $00000002;
  TMR_TMR32B0CCR_CAP0FE_DISABLED : dword absolute $00000000;
  TMR_TMR32B0CCR_CAP0I_MASK : dword absolute $00000004;// Interrupt on CAP0 event
  TMR_TMR32B0CCR_CAP0I_ENABLED : dword absolute $00000004;
  TMR_TMR32B0CCR_CAP0I_DISABLED : dword absolute $00000000;

  TMR_TMR32B0EMR_EM0_MASK : dword absolute $00000001;// External match 0
  TMR_TMR32B0EMR_EM0 : dword absolute $00000001;
  TMR_TMR32B0EMR_EMC0_MASK : dword absolute $00000030;
  TMR_TMR32B0EMR_EMC0_DONOTHING : dword absolute $00000000;
  TMR_TMR32B0EMR_EMC0_LOW : dword absolute $00000010;
  TMR_TMR32B0EMR_EMC0_HIGH : dword absolute $00000020;
  TMR_TMR32B0EMR_EMC0_TOGGLE : dword absolute $00000030;
  TMR_TMR32B0EMR_EM1_MASK : dword absolute $00000002;// External match 1
  TMR_TMR32B0EMR_EM1 : dword absolute $00000002;
  TMR_TMR32B0EMR_EMC1_MASK : dword absolute $000000C0;
  TMR_TMR32B0EMR_EMC1_DONOTHING : dword absolute $00000000;
  TMR_TMR32B0EMR_EMC1_LOW : dword absolute $00000040;
  TMR_TMR32B0EMR_EMC1_HIGH : dword absolute $00000080;
  TMR_TMR32B0EMR_EMC1_TOGGLE : dword absolute $000000C0;
  TMR_TMR32B0EMR_EM2_MASK : dword absolute $00000004;// External match 2
  TMR_TMR32B0EMR_EM2 : dword absolute $00000004;
  TMR_TMR32B0EMR_EMC2_MASK : dword absolute $00000300;
  TMR_TMR32B0EMR_EMC2_DONOTHING : dword absolute $00000000;
  TMR_TMR32B0EMR_EMC2_LOW : dword absolute $00000100;
  TMR_TMR32B0EMR_EMC2_HIGH : dword absolute $00000200;
  TMR_TMR32B0EMR_EMC2_TOGGLE : dword absolute $00000300;
  TMR_TMR32B0EMR_EM3_MASK : dword absolute $00000008;// External match 3
  TMR_TMR32B0EMR_EM3 : dword absolute $00000008;
  TMR_TMR32B0EMR_EMC3_MASK : dword absolute $00000C00;
  TMR_TMR32B0EMR_EMC3_DONOTHING : dword absolute $00000000;
  TMR_TMR32B0EMR_EMC3_LOW : dword absolute $00000400;
  TMR_TMR32B0EMR_EMC3_HIGH : dword absolute $00000800;
  TMR_TMR32B0EMR_EMC3_TOGGLE : dword absolute $00000C00;

  TMR_TMR32B0CTCR_CTMODE_MASK : dword absolute $00000003;// Counter/Timer mode
  TMR_TMR32B0CTCR_CTMODE_TIMER : dword absolute $00000000;// Timer Mode: Every rising PCLK edge
  TMR_TMR32B0CTCR_CTMODE_COUNTERRISING : dword absolute $00000001;// Counter: TC increments on rising edge of input
  TMR_TMR32B0CTCR_CTMODE_COUNTERFALLING : dword absolute $00000002;// Counter: TC increments on falling edge of input
  TMR_TMR32B0CTCR_CTMODE_COUNTERBOTH : dword absolute $00000003;// Counter: TC increments on both edges of input
  TMR_TMR32B0CTCR_CINPUTSELECT_MASK : dword absolute $0000000C;
  TMR_TMR32B0CTCR_CINPUTSELECT : dword absolute $00000000;// CINPUTSELECT must be set to 00

  TMR_TMR32B0PWMC_PWM0_MASK : dword absolute $00000001;
  TMR_TMR32B0PWMC_PWM0_ENABLED : dword absolute $00000001;// PWM mode is enabled for CT32Bn_MAT0
  TMR_TMR32B0PWMC_PWM0_DISABLED : dword absolute $00000000;
  TMR_TMR32B0PWMC_PWM1_MASK : dword absolute $00000002;
  TMR_TMR32B0PWMC_PWM1_ENABLED : dword absolute $00000002;// PWM mode is enabled for CT32Bn_MAT1
  TMR_TMR32B0PWMC_PWM1_DISABLED : dword absolute $00000000;
  TMR_TMR32B0PWMC_PWM2_MASK : dword absolute $00000004;
  TMR_TMR32B0PWMC_PWM2_ENABLED : dword absolute $00000004;// PWM mode is enabled for CT32Bn_MAT2
  TMR_TMR32B0PWMC_PWM2_DISABLED : dword absolute $00000000;
  TMR_TMR32B0PWMC_PWM3_MASK : dword absolute $00000008;
  TMR_TMR32B0PWMC_PWM3_ENABLED : dword absolute $00000008;// PWM mode is enabled for CT32Bn_MAT3
  TMR_TMR32B0PWMC_PWM3_DISABLED : dword absolute $00000000;
const
  TMR_CT32B1_BASE_ADDRESS = $40018000;
var
  TMR_TMR32B1IR : dword absolute $40018000;// Interrupt register
  TMR_TMR32B1TCR : dword absolute $40018004;// Timer control register
  TMR_TMR32B1TC : dword absolute $40018008;// Timer counter
  TMR_TMR32B1PR : dword absolute $4001800C;// Prescale register
  TMR_TMR32B1PC : dword absolute $40018010;// Prescale counter register
  TMR_TMR32B1MCR : dword absolute $40018014;// Match control register
  TMR_TMR32B1MR0 : dword absolute $40018018;// Match register 0
  TMR_TMR32B1MR1 : dword absolute $4001801C;// Match register 1
  TMR_TMR32B1MR2 : dword absolute $40018020;// Match register 2
  TMR_TMR32B1MR3 : dword absolute $40018024;// Match register 3
  TMR_TMR32B1CCR : dword absolute $40018028;// Capture control register
  TMR_TMR32B1CR0 : dword absolute $4001802C;// Capture register
  TMR_TMR32B1EMR : dword absolute $4001803C;// External match register
  TMR_TMR32B1CTCR : dword absolute $40018070;// Count control register
  TMR_TMR32B1PWMC : dword absolute $40018074;// PWM control register

  TMR_TMR32B1IR_MR0_MASK : dword absolute $00000001;// Interrupt flag for match channel 0
  TMR_TMR32B1IR_MR0 : dword absolute $00000001;
  TMR_TMR32B1IR_MR1_MASK : dword absolute $00000002;// Interrupt flag for match channel 1
  TMR_TMR32B1IR_MR1 : dword absolute $00000002;
  TMR_TMR32B1IR_MR2_MASK : dword absolute $00000004;// Interrupt flag for match channel 2
  TMR_TMR32B1IR_MR2 : dword absolute $00000004;
  TMR_TMR32B1IR_MR3_MASK : dword absolute $00000008;// Interrupt flag for match channel 3
  TMR_TMR32B1IR_MR3 : dword absolute $00000008;
  TMR_TMR32B1IR_CR0_MASK : dword absolute $00000010;// Interrupt flag for capture channel 0 event
  TMR_TMR32B1IR_CR0 : dword absolute $00000010;
  TMR_TMR32B1IR_MASK_ALL : dword absolute $0000001F;

  TMR_TMR32B1TCR_COUNTERENABLE_MASK : dword absolute $00000001;// Counter enable
  TMR_TMR32B1TCR_COUNTERENABLE_ENABLED : dword absolute $00000001;
  TMR_TMR32B1TCR_COUNTERENABLE_DISABLED : dword absolute $00000000;
  TMR_TMR32B1TCR_COUNTERRESET_MASK : dword absolute $00000002;
  TMR_TMR32B1TCR_COUNTERRESET_ENABLED : dword absolute $00000002;
  TMR_TMR32B1TCR_COUNTERRESET_DISABLED : dword absolute $00000000;

  TMR_TMR32B1MCR_MR0_INT_MASK : dword absolute $00000001;// Interrupt on MRO
  TMR_TMR32B1MCR_MR0_INT_ENABLED : dword absolute $00000001;
  TMR_TMR32B1MCR_MR0_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR0_RESET_MASK : dword absolute $00000002;// Reset on MR0
  TMR_TMR32B1MCR_MR0_RESET_ENABLED : dword absolute $00000002;
  TMR_TMR32B1MCR_MR0_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR0_STOP_MASK : dword absolute $00000004;// Stop on MR0
  TMR_TMR32B1MCR_MR0_STOP_ENABLED : dword absolute $00000004;
  TMR_TMR32B1MCR_MR0_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR1_INT_MASK : dword absolute $00000008;// Interrupt on MR1
  TMR_TMR32B1MCR_MR1_INT_ENABLED : dword absolute $00000008;
  TMR_TMR32B1MCR_MR1_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR1_RESET_MASK : dword absolute $00000010;// Reset on MR1
  TMR_TMR32B1MCR_MR1_RESET_ENABLED : dword absolute $00000010;
  TMR_TMR32B1MCR_MR1_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR1_STOP_MASK : dword absolute $00000020;// Stop on MR1
  TMR_TMR32B1MCR_MR1_STOP_ENABLED : dword absolute $00000020;
  TMR_TMR32B1MCR_MR1_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR2_INT_MASK : dword absolute $00000040;// Interrupt on MR2
  TMR_TMR32B1MCR_MR2_INT_ENABLED : dword absolute $00000040;
  TMR_TMR32B1MCR_MR2_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR2_RESET_MASK : dword absolute $00000080;// Reset on MR2
  TMR_TMR32B1MCR_MR2_RESET_ENABLED : dword absolute $00000080;
  TMR_TMR32B1MCR_MR2_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR2_STOP_MASK : dword absolute $00000100;// Stop on MR2
  TMR_TMR32B1MCR_MR2_STOP_ENABLED : dword absolute $00000100;
  TMR_TMR32B1MCR_MR2_STOP_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR3_INT_MASK : dword absolute $00000200;// Interrupt on MR3
  TMR_TMR32B1MCR_MR3_INT_ENABLED : dword absolute $00000200;
  TMR_TMR32B1MCR_MR3_INT_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR3_RESET_MASK : dword absolute $00000400;// Reset on MR3
  TMR_TMR32B1MCR_MR3_RESET_ENABLED : dword absolute $00000400;
  TMR_TMR32B1MCR_MR3_RESET_DISABLED : dword absolute $00000000;
  TMR_TMR32B1MCR_MR3_STOP_MASK : dword absolute $00000800;// Stop on MR3
  TMR_TMR32B1MCR_MR3_STOP_ENABLED : dword absolute $00000800;
  TMR_TMR32B1MCR_MR3_STOP_DISABLED : dword absolute $00000000;

  TMR_TMR32B1CCR_CAP0RE_MASK : dword absolute $00000001;// Capture on rising edge
  TMR_TMR32B1CCR_CAP0RE_ENABLED : dword absolute $00000001;
  TMR_TMR32B1CCR_CAP0RE_DISABLED : dword absolute $00000000;
  TMR_TMR32B1CCR_CAP0FE_MASK : dword absolute $00000002;// Capture on falling edge
  TMR_TMR32B1CCR_CAP0FE_ENABLED : dword absolute $00000002;
  TMR_TMR32B1CCR_CAP0FE_DISABLED : dword absolute $00000000;
  TMR_TMR32B1CCR_CAP0I_MASK : dword absolute $00000004;// Interrupt on CAP0 event
  TMR_TMR32B1CCR_CAP0I_ENABLED : dword absolute $00000004;
  TMR_TMR32B1CCR_CAP0I_DISABLED : dword absolute $00000000;

  TMR_TMR32B1EMR_EM0_MASK : dword absolute $00000001;// External match 0
  TMR_TMR32B1EMR_EM0 : dword absolute $00000001;
  TMR_TMR32B1EMR_EMC0_MASK : dword absolute $00000030;
  TMR_TMR32B1EMR_EMC0_DONOTHING : dword absolute $00000000;
  TMR_TMR32B1EMR_EMC0_LOW : dword absolute $00000010;
  TMR_TMR32B1EMR_EMC0_HIGH : dword absolute $00000020;
  TMR_TMR32B1EMR_EMC0_TOGGLE : dword absolute $00000030;
  TMR_TMR32B1EMR_EM1_MASK : dword absolute $00000002;// External match 1
  TMR_TMR32B1EMR_EM1 : dword absolute $00000002;
  TMR_TMR32B1EMR_EMC1_MASK : dword absolute $000000C0;
  TMR_TMR32B1EMR_EMC1_DONOTHING : dword absolute $00000000;
  TMR_TMR32B1EMR_EMC1_LOW : dword absolute $00000040;
  TMR_TMR32B1EMR_EMC1_HIGH : dword absolute $00000080;
  TMR_TMR32B1EMR_EMC1_TOGGLE : dword absolute $000000C0;
  TMR_TMR32B1EMR_EM2_MASK : dword absolute $00000004;// External match 2
  TMR_TMR32B1EMR_EM2 : dword absolute $00000004;
  TMR_TMR32B1EMR_EMC2_MASK : dword absolute $00000300;
  TMR_TMR32B1EMR_EMC2_DONOTHING : dword absolute $00000000;
  TMR_TMR32B1EMR_EMC2_LOW : dword absolute $00000100;
  TMR_TMR32B1EMR_EMC2_HIGH : dword absolute $00000200;
  TMR_TMR32B1EMR_EMC2_TOGGLE : dword absolute $00000300;
  TMR_TMR32B1EMR_EM3_MASK : dword absolute $00000008;// External match 3
  TMR_TMR32B1EMR_EM3 : dword absolute $00000008;
  TMR_TMR32B1EMR_EMC3_MASK : dword absolute $00000C00;
  TMR_TMR32B1EMR_EMC3_DONOTHING : dword absolute $00000000;
  TMR_TMR32B1EMR_EMC3_LOW : dword absolute $00000400;
  TMR_TMR32B1EMR_EMC3_HIGH : dword absolute $00000800;
  TMR_TMR32B1EMR_EMC3_TOGGLE : dword absolute $00000C00;

  TMR_TMR32B1CTCR_CTMODE_MASK : dword absolute $00000003;// Counter/Timer mode
  TMR_TMR32B1CTCR_CTMODE_TIMER : dword absolute $00000000;// Timer Mode: Every rising PCLK edge
  TMR_TMR32B1CTCR_CTMODE_COUNTERRISING : dword absolute $00000001;// Counter: TC increments on rising edge of input
  TMR_TMR32B1CTCR_CTMODE_COUNTERFALLING : dword absolute $00000002;// Counter: TC increments on falling edge of input
  TMR_TMR32B1CTCR_CTMODE_COUNTERBOTH : dword absolute $00000003;// Counter: TC increments on both edges of input
  TMR_TMR32B1CTCR_CINPUTSELECT_MASK : dword absolute $0000000C;
  TMR_TMR32B1CTCR_CINPUTSELECT : dword absolute $00000000;// CINPUTSELECT must be set to 00

  TMR_TMR32B1PWMC_PWM0_MASK : dword absolute $00000001;
  TMR_TMR32B1PWMC_PWM0_ENABLED : dword absolute $00000001;// PWM mode is enabled for CT32Bn_MAT0
  TMR_TMR32B1PWMC_PWM0_DISABLED : dword absolute $00000000;
  TMR_TMR32B1PWMC_PWM1_MASK : dword absolute $00000002;
  TMR_TMR32B1PWMC_PWM1_ENABLED : dword absolute $00000002;// PWM mode is enabled for CT32Bn_MAT1
  TMR_TMR32B1PWMC_PWM1_DISABLED : dword absolute $00000000;
  TMR_TMR32B1PWMC_PWM2_MASK : dword absolute $00000004;
  TMR_TMR32B1PWMC_PWM2_ENABLED : dword absolute $00000004;// PWM mode is enabled for CT32Bn_MAT2
  TMR_TMR32B1PWMC_PWM2_DISABLED : dword absolute $00000000;
  TMR_TMR32B1PWMC_PWM3_MASK : dword absolute $00000008;
  TMR_TMR32B1PWMC_PWM3_ENABLED : dword absolute $00000008;// PWM mode is enabled for CT32Bn_MAT3
  TMR_TMR32B1PWMC_PWM3_DISABLED : dword absolute $00000000;

(*##############################################################################
//## System Tick Timer
//##############################################################################*)

const
  SYSTICK_BASE_ADDRESS = $E000E000;
var
  SYSTICK_STCTRL : dword absolute $E000E010;// System tick control
  SYSTICK_STRELOAD : dword absolute $E000E014;// System timer reload
  SYSTICK_STCURR : dword absolute $E000E018;// System timer current
  SYSTICK_STCALIB : dword absolute $E000E01C;// System timer calibration

(*STCTRL (System Timer Control and status register)
//The STCTRL register contains control information for the System Tick Timer, and provides
//a status flag. *)
const
  SYSTICK_STCTRL_ENABLE  = $00000001;// System tick counter enable
  SYSTICK_STCTRL_TICKINT  = $00000002;// System tick interrupt enable
  SYSTICK_STCTRL_CLKSOURCE  = $00000004;// NOTE: This isn't documented but is based on NXP examples
  SYSTICK_STCTRL_COUNTFLAG  = $00010000;// System tick counter flag

(*STRELOAD (System Timer Reload value register)
//The STRELOAD register is set to the value that will be loaded into the System Tick Timer
//whenever it counts down to zero. This register is loaded by software as part of timer
//initialization. The STCALIB register may be read and used as the value for STRELOAD if
//the CPU or external clock is running at the frequency intended for use with the STCALIB
//value. *)

  SYSTICK_STRELOAD_MASK = $00FFFFFF;

(*STCURR (System Timer Current value register)
//The STCURR register returns the current count from the System Tick counter when it is
//read by software. *)

  SYSTICK_STCURR_MASK  = $00FFFFFF;

(*STCALIB (System Timer Calibration value register) *)

  SYSTICK_STCALIB_TENMS_MASK  = $00FFFFFF;
  SYSTICK_STCALIB_SKEW_MASK  = $40000000;
  SYSTICK_STCALIB_NOREF_MASK  = $80000000;

(*##############################################################################
//## ADC
//##############################################################################*)
  ADC_AD0_BASE_ADDRESS = $4001C000;

var

  ADC_AD0CR : dword absolute $4001C000;// ADC Control Register
  ADC_AD0GDR : dword absolute $4001C004;// ADC Global Data Register
  ADC_AD0INTEN : dword absolute $4001C00C;// ADC Interrupt Enable Register
  ADC_AD0DR0 : dword absolute $4001C010;// ADC Data Register 0
  ADC_AD0DR1 : dword absolute $4001C014;// ADC Data Register 1
  ADC_AD0DR2 : dword absolute $4001C018;// ADC Data Register 2
  ADC_AD0DR3 : dword absolute $4001C01C;// ADC Data Register 3
  ADC_AD0DR4 : dword absolute $4001C020;// ADC Data Register 4
  ADC_AD0DR5 : dword absolute $4001C024;// ADC Data Register 5
  ADC_AD0DR6 : dword absolute $4001C028;// ADC Data Register 6
  ADC_AD0DR7 : dword absolute $4001C02C;// ADC Data Register 7
  ADC_AD0STAT : dword absolute $4001C030;// ADC Status Register

const
  ADC_AD0CR_SEL_MASK  = $000000FF;
  ADC_AD0CR_SEL_AD0  = $00000001;
  ADC_AD0CR_SEL_AD1  = $00000002;
  ADC_AD0CR_SEL_AD2  = $00000004;
  ADC_AD0CR_SEL_AD3  = $00000008;
  ADC_AD0CR_SEL_AD4  = $00000010;
  ADC_AD0CR_SEL_AD5  = $00000020;
  ADC_AD0CR_SEL_AD6  = $00000040;
  ADC_AD0CR_SEL_AD7  = $00000080;
  ADC_AD0CR_CLKDIV_MASK  = $0000FF00;
  ADC_AD0CR_BURST_MASK  = $00010000;
  ADC_AD0CR_BURST_SWMODE  = $00000000;
  ADC_AD0CR_BURST_HWSCANMODE  = $00010000;
  ADC_AD0CR_CLKS_MASK  = $000E0000;
  ADC_AD0CR_CLKS_10BITS  = $00000000;
  ADC_AD0CR_CLKS_9BITS  = $00020000;
  ADC_AD0CR_CLKS_8BITS  = $00040000;
  ADC_AD0CR_CLKS_7BITS  = $00060000;
  ADC_AD0CR_CLKS_6BITS  = $00080000;
  ADC_AD0CR_CLKS_5BITS  = $000A0000;
  ADC_AD0CR_CLKS_4BITS  = $000C0000;
  ADC_AD0CR_CLKS_3BITS  = $000E0000;
  ADC_AD0CR_START_MASK  = $07000000;
  ADC_AD0CR_START_NOSTART  = $00000000;
  ADC_AD0CR_START_STARTNOW  = $01000000;
  ADC_AD0CR_EDGE_MASK  = $08000000;
  ADC_AD0CR_EDGE_FALLING  = $08000000;
  ADC_AD0CR_EDGE_RISING  = $00000000;

(*AD9GDR (A/D Global Data Register)
//The A/D Global Data Register contains the result of the most recent A/D conversion. This
//includes the data, DONE, and Overrun flags, and the number of the A/D channel to which
//the data relates. *)

  ADC_AD0GDR_RESULT_MASK  = $0000FFC0;
  ADC_AD0GDR_CHN_MASK  = $07000000;// Channel from which the results were converted
  ADC_AD0GDR_OVERUN_MASK  = $40000000;
  ADC_AD0GDR_OVERUN  = $40000000;
  ADC_AD0GDR_DONE_MASK  = $80000000;
  ADC_AD0GDR_DONE  = $80000000;

(*AD0STAT (A/D Status Register)
//The A/D Status register allows checking the status of all A/D channels simultaneously.
//The DONE and OVERRUN flags appearing in the ADDRn register for each A/D channel
//are mirrored in ADSTAT. The interrupt flag (the logical OR of all DONE flags) is also found
//in ADSTAT. *)

  ADC_AD0STAT_DONE0_MASK  = $00000001;
  ADC_AD0STAT_DONE0  = $00000001;
  ADC_AD0STAT_DONE1_MASK  = $00000002;
  ADC_AD0STAT_DONE1  = $00000002;
  ADC_AD0STAT_DONE2_MASK  = $00000004;
  ADC_AD0STAT_DONE2  = $00000004;
  ADC_AD0STAT_DONE3_MASK  = $00000008;
  ADC_AD0STAT_DONE3  = $00000008;
  ADC_AD0STAT_DONE4_MASK  = $00000010;
  ADC_AD0STAT_DONE4  = $00000010;
  ADC_AD0STAT_DONE5_MASK  = $00000020;
  ADC_AD0STAT_DONE5  = $00000020;
  ADC_AD0STAT_DONE6_MASK  = $00000040;
  ADC_AD0STAT_DONE6  = $00000040;
  ADC_AD0STAT_DONE7_MASK  = $00000080;
  ADC_AD0STAT_DONE7  = $00000080;
  ADC_AD0STAT_OVERRUN0_MASK  = $00000100;
  ADC_AD0STAT_OVERRUN0  = $00000100;
  ADC_AD0STAT_OVERRUN1_MASK  = $00000200;
  ADC_AD0STAT_OVERRUN1  = $00000200;
  ADC_AD0STAT_OVERRUN2_MASK  = $00000400;
  ADC_AD0STAT_OVERRUN2  = $00000400;
  ADC_AD0STAT_OVERRUN3_MASK  = $00000800;
  ADC_AD0STAT_OVERRUN3  = $00000800;
  ADC_AD0STAT_OVERRUN4_MASK  = $00001000;
  ADC_AD0STAT_OVERRUN4  = $00001000;
  ADC_AD0STAT_OVERRUN5_MASK  = $00002000;
  ADC_AD0STAT_OVERRUN5  = $00002000;
  ADC_AD0STAT_OVERRUN6_MASK  = $00004000;
  ADC_AD0STAT_OVERRUN6  = $00004000;
  ADC_AD0STAT_OVERRUN7_MASK  = $00008000;
  ADC_AD0STAT_OVERRUN7  = $00008000;
  ADC_AD0STAT_ADINT_MASK  = $00010000;
  ADC_AD0STAT_ADINT  = $00010000;

//(*ADINTEN0 (A/D Interrupt Enable Register)
//This register allows control over which A/D channels generate an interrupt when a
//conversion is complete. For example, it may be desirable to use some A/D channels to
//monitor sensors by continuously performing conversions on them. The most recent
//results are read by the application program whenever they are needed. In this case, an
//interrupt is not desirable at the end of each conversion for some A/D channels. *)
  const
  ADC_AD0INTEN_ADINTEN0_MASK  = $00000001;
  ADC_AD0INTEN_ADINTEN0  = $00000001;
  ADC_AD0INTEN_ADINTEN1_MASK  = $00000002;
  ADC_AD0INTEN_ADINTEN1  = $00000002;
  ADC_AD0INTEN_ADINTEN2_MASK  = $00000004;
  ADC_AD0INTEN_ADINTEN2  = $00000004;
  ADC_AD0INTEN_ADINTEN3_MASK  = $00000008;
  ADC_AD0INTEN_ADINTEN3  = $00000008;
  ADC_AD0INTEN_ADINTEN4_MASK  = $00000010;
  ADC_AD0INTEN_ADINTEN4  = $00000010;
  ADC_AD0INTEN_ADINTEN5_MASK  = $00000020;
  ADC_AD0INTEN_ADINTEN5  = $00000020;
  ADC_AD0INTEN_ADINTEN6_MASK  = $00000040;
  ADC_AD0INTEN_ADINTEN6  = $00000040;
  ADC_AD0INTEN_ADINTEN7_MASK  = $00000080;
  ADC_AD0INTEN_ADINTEN7  = $00000080;
  ADC_AD0INTEN_ADGINTEN_MASK  = $00000100;
  ADC_AD0INTEN_ADGINTEN_ENABLE  = $00000100;
  ADC_AD0INTEN_ADGINTEN_DISABLE  = $00000000;

//(*AD0DR0..7 (A/D Data Registers)
//The A/D Data Register hold the result when an A/D conversion is complete, and also
//include the flags that indicate when a conversion has been completed and when a
//conversion overrun has occurred. *)
const
  ADC_DR_V_MASK = $0000FFC0;
  ADC_DR_OVERRUN_MASK  = $40000000;
  ADC_DR_OVERRUN = $40000000;
  ADC_DR_DONE_MASK = $80000000;
  ADC_DR_DONE = $80000000;

(*##############################################################################
//## WDT - Watchdog Timer
//##############################################################################*)

const
  WDT_BASE_ADDRESS  = $40004000;

var
  WDT_WDMOD : dword absolute $40004000;// Watchdog mode register
  WDT_WDTC : dword absolute $40004004;// Watchdog timer constant register
  WDT_WDFEED : dword absolute $40004008;// Watchdog feed sequence register
  WDT_WDTV : dword absolute $4000400C;// Watchdog timer value register

(*WDMOD (Watchdog Mode register)
//The WDMOD register controls the operation of the Watchdog through the combination of
//WDEN and RESET bits. Note that a watchdog feed must be performed before any
//changes to the WDMOD register take effect. *)
const
  WDT_WDMOD_WDEN_DISABLED = $00000000;// Watchdog enable bit
  WDT_WDMOD_WDEN_ENABLED = $00000001;
  WDT_WDMOD_WDEN_MASK = $00000001;
  WDT_WDMOD_WDRESET_DISABLED = $00000000;// Watchdog reset enable bit
  WDT_WDMOD_WDRESET_ENABLED = $00000002;
  WDT_WDMOD_WDRESET_MASK  =$00000002;
  WDT_WDMOD_WDTOF =  $00000004;// Watchdog time-out interrupt flag
  WDT_WDMOD_WDTOF_MASK  = $00000004;// Set when the watchdog times out
  WDT_WDMOD_WDINT = $00000008;// Watchdog timer interrupt flag
  WDT_WDMOD_WDINT_MASK  =$00000008;

(*WDFEED (Watchdog Feed register)
//Writing 0xAA followed by 0x55 to this register will reload the Watchdog timer with the
//WDTC value. This operation will also start the Watchdog if it is enabled via the WDMOD
//register. Setting the WDEN bit in the WDMOD register is not sufficient to enable the
//Watchdog. A valid feed sequence must be completed after setting WDEN before the
//Watchdog is capable of generating a reset. Until then, the Watchdog will ignore feed
//errors. After writing 0xAA to WDFEED, access to any Watchdog register other than writing
//0x55 to WDFEED causes an immediate reset/interrupt when the Watchdog is enabled.
//The reset will be generated during the second PCLK following an incorrect access to a
//Watchdog register during a feed sequence.
//Interrupts should be disabled during the feed sequence. An abort condition will occur if an
//interrupt happens during the feed sequence. *)

  WDT_WDFEED_FEED1 = $000000AA;
  WDT_WDFEED_FEED2 = $00000055;

(*##############################################################################
//## Misc. Inline Functions
//##############################################################################*)
(*
//@brief Reverses the bit order of a 32-bit value
//Allows single-cycle reversing of 32-bit values (ASM RBIT)
//@/param[in] value
//The 32-bit value to reverse
//@returns The reversed value
//static inline uint32_t RBIT(uint32_t value) { uint32_t result=0; __asm volatile ("rbit %0, %1" : "=r" (result) : "r" (value) ); return(result); }
//@brief Causes a system reset and enters the USB Bootloader
//Resets the system using the AIRCR register, and waits in a loop until
//reset occurs. The resistor/divider on the LPC1343 Reference Design
//Base Board [1] causes the AIRCR reset to enter the bootloader rather
//than executing the existing firmware. If you wish to reset and execute
//the existing firmware, you need to use the watchdog timer to reset
//(see "wdt/wdt.c").

//[1] http://www.microbuilder.eu/Projects/LPC1343ReferenceDesign.aspx

//static inline void __resetBootloader() { __disable_irq(); SCB_AIRCR = SCB_AIRCR_VECTKEY_VALUE | SCB_AIRCR_SYSRESETREQ; while(1); }

//#endif
*)
procedure NVIC_EnableIRQ(IRQN:Byte);
procedure NVIC_DisableIRQ(IRQN:Byte);
procedure Enable_IRQ;
procedure Disable_IRQ;


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

procedure NVIC_EnableIRQ(IRQN:Byte);
begin
  NVIC^.ISER[(IRQn shr  5)] := (1 shl (IRQn) and $1F);
end;

procedure NVIC_DisableIRQ(IRQN:Byte);
begin
  NVIC^.ICER[(IRQn shr  5)] := (1 shl (IRQn) and $1F);
end;

procedure Enable_IRQ;
begin
  asm
    cpsie i;
  end;
end;

procedure Disable_IRQ;
begin
  asm
    cpsid i;
  end;
end;


procedure  WAKEUP_IRQHandler_PIO0_0; external name 'WAKEUP_IRQHandler_PIO0_0';         //*16+ 0: Wakeup PIO0.0
procedure  WAKEUP_IRQHandler_PIO0_1; external name 'WAKEUP_IRQHandler_PIO0_1';         //*16+ 1: Wakeup PIO0.1
procedure  WAKEUP_IRQHandler_PIO0_2; external name 'WAKEUP_IRQHandler_PIO0_2';         //*16+ 2: Wakeup PIO0.2
procedure  WAKEUP_IRQHandler_PIO0_3; external name 'WAKEUP_IRQHandler_PIO0_3';         //*16+ 3: Wakeup PIO0.3
procedure  WAKEUP_IRQHandler_PIO0_4; external name 'WAKEUP_IRQHandler_PIO0_4';         //*16+ 4: Wakeup PIO0.4
procedure  WAKEUP_IRQHandler_PIO0_5; external name 'WAKEUP_IRQHandler_PIO0_5';         //*16+ 5: Wakeup PIO0.5
procedure  WAKEUP_IRQHandler_PIO0_6; external name 'WAKEUP_IRQHandler_PIO0_6';         //*16+ 6: Wakeup PIO0.6
procedure  WAKEUP_IRQHandler_PIO0_7; external name 'WAKEUP_IRQHandler_PIO0_7';         //*16+ 7: Wakeup PIO0.7
procedure  WAKEUP_IRQHandler_PIO0_8; external name 'WAKEUP_IRQHandler_PIO0_8';         //*16+ 8: Wakeup PIO0.8
procedure  WAKEUP_IRQHandler_PIO0_9; external name 'WAKEUP_IRQHandler_PIO0_9';         //*16+ 9: Wakeup PIO0.9
procedure  WAKEUP_IRQHandler_PIO0_10; external name 'WAKEUP_IRQHandler_PIO0_10';         //*16+10: Wakeup PIO0.10
procedure  WAKEUP_IRQHandler_PIO0_11; external name 'WAKEUP_IRQHandler_PIO0_11';         //*16+11: Wakeup PIO0.11
procedure  WAKEUP_IRQHandler_PIO1_0; external name 'WAKEUP_IRQHandler_PIO1_0';      //*16+12: Wakeup PIO1.0
procedure  WAKEUP_IRQHandler_PIO1_1; external name 'WAKEUP_IRQHandler_PIO1_1';         //*16+13: Wakeup PIO1.1
procedure  WAKEUP_IRQHandler_PIO1_2; external name 'WAKEUP_IRQHandler_PIO1_2';         //*16+14: Wakeup PIO1.2
procedure  WAKEUP_IRQHandler_PIO1_3; external name 'WAKEUP_IRQHandler_PIO1_3';         //*16+15: Wakeup PIO1.3
procedure  WAKEUP_IRQHandler_PIO1_4; external name 'WAKEUP_IRQHandler_PIO1_4';         //*16+16: Wakeup PIO1.4
procedure  WAKEUP_IRQHandler_PIO1_5; external name 'WAKEUP_IRQHandler_PIO1_5';         //*16+17: Wakeup PIO1.5
procedure  WAKEUP_IRQHandler_PIO1_6; external name 'WAKEUP_IRQHandler_PIO1_6';         //*16+18: Wakeup PIO1.6
procedure  WAKEUP_IRQHandler_PIO1_7; external name 'WAKEUP_IRQHandler_PIO1_7';         //*16+19: Wakeup PIO1.7
procedure  WAKEUP_IRQHandler_PIO1_8; external name 'WAKEUP_IRQHandler_PIO1_8';         //*16+20: Wakeup PIO1.8
procedure  WAKEUP_IRQHandler_PIO1_9; external name 'WAKEUP_IRQHandler_PIO1_9';         //*16+21: Wakeup PIO1.9
procedure  WAKEUP_IRQHandler_PIO1_10; external name 'WAKEUP_IRQHandler_PIO1_10';         //*16+22: Wakeup PIO1.10
procedure  WAKEUP_IRQHandler_PIO1_11; external name 'WAKEUP_IRQHandler_PIO1_11';         //*16+23: Wakeup PIO1.11
procedure  WAKEUP_IRQHandler_PIO2_0; external name 'WAKEUP_IRQHandler_PIO2_0';         //*16+24: Wakeup PIO2.0
procedure  WAKEUP_IRQHandler_PIO2_1; external name 'WAKEUP_IRQHandler_PIO2_1';         //*16+25: Wakeup PIO2.1
procedure  WAKEUP_IRQHandler_PIO2_2; external name 'WAKEUP_IRQHandler_PIO2_2';         //*16+26: Wakeup PIO2.2
procedure  WAKEUP_IRQHandler_PIO2_3; external name 'WAKEUP_IRQHandler_PIO2_3';         //*16+27: Wakeup PIO2.3
procedure  WAKEUP_IRQHandler_PIO2_4; external name 'WAKEUP_IRQHandler_PIO2_4';         //*16+28: Wakeup PIO2.4
procedure  WAKEUP_IRQHandler_PIO2_5; external name 'WAKEUP_IRQHandler_PIO2_5';          //*16+29: Wakeup PIO2.5
procedure  WAKEUP_IRQHandler_PIO2_6; external name 'WAKEUP_IRQHandler_PIO2_6';         //*16+30: Wakeup PIO2.6
procedure  WAKEUP_IRQHandler_PIO2_7; external name 'WAKEUP_IRQHandler_PIO2_7';         //*16+31: Wakeup PIO2.7
procedure  WAKEUP_IRQHandler_PIO2_8; external name 'WAKEUP_IRQHandler_PIO2_8';         //*16+32: Wakeup PIO2.8
procedure  WAKEUP_IRQHandler_PIO2_9; external name 'WAKEUP_IRQHandler_PIO2_9';         //*16+33: Wakeup PIO2.9
procedure  WAKEUP_IRQHandler_PIO2_10; external name 'WAKEUP_IRQHandler_PIO2_10';         //*16+34: Wakeup PIO2.10
procedure  WAKEUP_IRQHandler_PIO2_11; external name 'WAKEUP_IRQHandler_PIO2_11';         //*16+35: Wakeup PIO2.11
procedure  WAKEUP_IRQHandler_PIO3_0; external name 'WAKEUP_IRQHandler_PIO3_0';         //*16+36: Wakeup PIO3.0
procedure  WAKEUP_IRQHandler_PIO3_1; external name 'WAKEUP_IRQHandler_PIO3_1';         //*16+37: Wakeup PIO3.1
procedure  WAKEUP_IRQHandler_PIO3_2; external name 'WAKEUP_IRQHandler_PIO3_2';         //*16+38: Wakeup PIO3.2
procedure  WAKEUP_IRQHandler_PIO3_3; external name 'WAKEUP_IRQHandler_PIO3_3';         //*16+39: Wakeup PIO3.3
procedure  I2C_IRQHandler; external name 'I2C_IRQHandler';            //*16+40: I2C
procedure  TIMER16_0_IRQHandler; external name 'TIMER16_0_IRQHandler';      //*16+41: 16-bit Counter-Timer 0
procedure  TIMER16_1_IRQHandler; external name 'TIMER16_1_IRQHandler';     //*16+42: 16-bit Counter-Timer 1
procedure  TIMER32_0_IRQHandler; external name 'TIMER32_0_IRQHandler';      //*16+43: 32-bit Counter-Timer 0
procedure  TIMER32_1_IRQHandler; external name 'TIMER32_1_IRQHandler';      //*16+44: 32-bit Counter-Timer 1
procedure  SSP0_IRQHandler; external name 'SSP0_IRQHandler';            //*16+45: SSP0
procedure  UART_IRQHandler; external name 'UART_IRQHandler';           //*16+46: UART
procedure  USB_IRQHandler; external name 'USB_IRQHandler';            //*16+47: USB IRQ
procedure  USB_FIQHandler; external name 'USB_FIQHandler';            //*16+48: USB FIQ
procedure  ADC_IRQHandler; external name 'ADC_IRQHandler';            //*16+49: A/D Converter
procedure  WDT_IRQHandler; external name 'WDT_IRQHandler';            //*16+50: Watchdog Timer
procedure  BOD_IRQHandler; external name 'BOD_IRQHandler';            //*16+51: Brown Out Detect
procedure  FMC_IRQHandler; external name 'FMC_IRQHandler';            //*16+52: IP2111 Flash Memory Controller
procedure  PIOINT3_IRQHandler; external name 'PIOINT3_IRQHandler';        //*16+53: PIO INT3
procedure  PIOINT2_IRQHandler; external name 'PIOINT2_IRQHandler';        //*16+54: PIO INT2
procedure  PIOINT1_IRQHandler; external name 'PIOINT1_IRQHandler';       //*16+55: PIO INT1
procedure  PIOINT0_IRQHandler; external name 'PIOINT0_IRQHandler';        //*16+56: PIO INT0
procedure  SSP1_IRQHandler; external name  'SSP1_IRQHandler';           //*16+57: SSP1

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

  .long WAKEUP_IRQHandler_PIO0_0         //*16+ 0: Wakeup PIO0.0
  .long WAKEUP_IRQHandler_PIO0_1         //*16+ 1: Wakeup PIO0.1
  .long WAKEUP_IRQHandler_PIO0_2         //*16+ 2: Wakeup PIO0.2
  .long WAKEUP_IRQHandler_PIO0_3         //*16+ 3: Wakeup PIO0.3
  .long WAKEUP_IRQHandler_PIO0_4         //*16+ 4: Wakeup PIO0.4
  .long WAKEUP_IRQHandler_PIO0_5         //*16+ 5: Wakeup PIO0.5
  .long WAKEUP_IRQHandler_PIO0_6         //*16+ 6: Wakeup PIO0.6
  .long WAKEUP_IRQHandler_PIO0_7         //*16+ 7: Wakeup PIO0.7
  .long WAKEUP_IRQHandler_PIO0_8         //*16+ 8: Wakeup PIO0.8
  .long WAKEUP_IRQHandler_PIO0_9         //*16+ 9: Wakeup PIO0.9
  .long WAKEUP_IRQHandler_PIO0_10         //*16+10: Wakeup PIO0.10
  .long WAKEUP_IRQHandler_PIO0_11         //*16+11: Wakeup PIO0.11
  .long WAKEUP_IRQHandler_PIO1_0        //*16+12: Wakeup PIO1.0
  .long WAKEUP_IRQHandler_PIO1_1         //*16+13: Wakeup PIO1.1
  .long WAKEUP_IRQHandler_PIO1_2         //*16+14: Wakeup PIO1.2
  .long WAKEUP_IRQHandler_PIO1_3         //*16+15: Wakeup PIO1.3
  .long WAKEUP_IRQHandler_PIO1_4         //*16+16: Wakeup PIO1.4
  .long WAKEUP_IRQHandler_PIO1_5         //*16+17: Wakeup PIO1.5
  .long WAKEUP_IRQHandler_PIO1_6         //*16+18: Wakeup PIO1.6
  .long WAKEUP_IRQHandler_PIO1_7         //*16+19: Wakeup PIO1.7
  .long WAKEUP_IRQHandler_PIO1_8         //*16+20: Wakeup PIO1.8
  .long WAKEUP_IRQHandler_PIO1_9         //*16+21: Wakeup PIO1.9
  .long WAKEUP_IRQHandler_PIO1_10         //*16+22: Wakeup PIO1.10
  .long WAKEUP_IRQHandler_PIO1_11         //*16+23: Wakeup PIO1.11
  .long WAKEUP_IRQHandler_PIO2_0         //*16+24: Wakeup PIO2.0
  .long WAKEUP_IRQHandler_PIO2_1         //*16+25: Wakeup PIO2.1
  .long WAKEUP_IRQHandler_PIO2_2         //*16+26: Wakeup PIO2.2
  .long WAKEUP_IRQHandler_PIO2_3         //*16+27: Wakeup PIO2.3
  .long WAKEUP_IRQHandler_PIO2_4         //*16+28: Wakeup PIO2.4
  .long WAKEUP_IRQHandler_PIO2_5         //*16+29: Wakeup PIO2.5
  .long WAKEUP_IRQHandler_PIO2_6         //*16+30: Wakeup PIO2.6
  .long WAKEUP_IRQHandler_PIO2_7         //*16+31: Wakeup PIO2.7
  .long WAKEUP_IRQHandler_PIO2_8         //*16+32: Wakeup PIO2.8
  .long WAKEUP_IRQHandler_PIO2_9         //*16+33: Wakeup PIO2.9
  .long WAKEUP_IRQHandler_PIO2_10         //*16+34: Wakeup PIO2.10
  .long WAKEUP_IRQHandler_PIO2_11         //*16+35: Wakeup PIO2.11
  .long WAKEUP_IRQHandler_PIO3_0         //*16+36: Wakeup PIO3.0
  .long WAKEUP_IRQHandler_PIO3_1         //*16+37: Wakeup PIO3.1
  .long WAKEUP_IRQHandler_PIO3_2         //*16+38: Wakeup PIO3.2
  .long WAKEUP_IRQHandler_PIO3_3         //*16+39: Wakeup PIO3.3
  .long I2C_IRQHandler            //*16+40: I2C
  .long TIMER16_0_IRQHandler      //*16+41: 16-bit Counter-Timer 0
  .long TIMER16_1_IRQHandler      //*16+42: 16-bit Counter-Timer 1
  .long TIMER32_0_IRQHandler      //*16+43: 32-bit Counter-Timer 0
  .long TIMER32_1_IRQHandler      //*16+44: 32-bit Counter-Timer 1
  .long SSP0_IRQHandler           //*16+45: SSP0
  .long UART_IRQHandler           //*16+46: UART
  .long USB_IRQHandler            //*16+47: USB IRQ
  .long USB_FIQHandler            //*16+48: USB FIQ
  .long ADC_IRQHandler            //*16+49: A/D Converter
  .long WDT_IRQHandler            //*16+50: Watchdog Timer
  .long BOD_IRQHandler            //*16+51: Brown Out Detect
  .long FMC_IRQHandler            //*16+52: IP2111 Flash Memory Controller
  .long PIOINT3_IRQHandler        //*16+53: PIO INT3
  .long PIOINT2_IRQHandler        //*16+54: PIO INT2
  .long PIOINT1_IRQHandler        //*16+55: PIO INT1
  .long PIOINT0_IRQHandler        //*16+56: PIO INT0
  .long SSP1_IRQHandler           //*16+57: SSP1
  .weak NMI_interrupt
  .weak Hardfault_interrupt
  .weak MemManage_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SWI_interrupt
  .weak DebugMonitor_interrupt
  .weak PendingSV_interrupt
  .weak SysTick_interrupt
  .weak WAKEUP_IRQHandler_PIO0_0         //*16+ 0: Wakeup PIO0.0
  .weak WAKEUP_IRQHandler_PIO0_1         //*16+ 1: Wakeup PIO0.1
  .weak WAKEUP_IRQHandler_PIO0_2         //*16+ 2: Wakeup PIO0.2
  .weak WAKEUP_IRQHandler_PIO0_3         //*16+ 3: Wakeup PIO0.3
  .weak WAKEUP_IRQHandler_PIO0_4         //*16+ 4: Wakeup PIO0.4
  .weak WAKEUP_IRQHandler_PIO0_5         //*16+ 5: Wakeup PIO0.5
  .weak WAKEUP_IRQHandler_PIO0_6         //*16+ 6: Wakeup PIO0.6
  .weak WAKEUP_IRQHandler_PIO0_7         //*16+ 7: Wakeup PIO0.7
  .weak WAKEUP_IRQHandler_PIO0_8         //*16+ 8: Wakeup PIO0.8
  .weak WAKEUP_IRQHandler_PIO0_9         //*16+ 9: Wakeup PIO0.9
  .weak WAKEUP_IRQHandler_PIO0_10         //*16+10: Wakeup PIO0.10
  .weak WAKEUP_IRQHandler_PIO0_11         //*16+11: Wakeup PIO0.11
  .weak WAKEUP_IRQHandler_PIO1_0        //*16+12: Wakeup PIO1.0
  .weak WAKEUP_IRQHandler_PIO1_1         //*16+13: Wakeup PIO1.1
  .weak WAKEUP_IRQHandler_PIO1_2         //*16+14: Wakeup PIO1.2
  .weak WAKEUP_IRQHandler_PIO1_3         //*16+15: Wakeup PIO1.3
  .weak WAKEUP_IRQHandler_PIO1_4         //*16+16: Wakeup PIO1.4
  .weak WAKEUP_IRQHandler_PIO1_5         //*16+17: Wakeup PIO1.5
  .weak WAKEUP_IRQHandler_PIO1_6         //*16+18: Wakeup PIO1.6
  .weak WAKEUP_IRQHandler_PIO1_7         //*16+19: Wakeup PIO1.7
  .weak WAKEUP_IRQHandler_PIO1_8         //*16+20: Wakeup PIO1.8
  .weak WAKEUP_IRQHandler_PIO1_9         //*16+21: Wakeup PIO1.9
  .weak WAKEUP_IRQHandler_PIO1_10         //*16+22: Wakeup PIO1.10
  .weak WAKEUP_IRQHandler_PIO1_11         //*16+23: Wakeup PIO1.11
  .weak WAKEUP_IRQHandler_PIO2_0         //*16+24: Wakeup PIO2.0
  .weak WAKEUP_IRQHandler_PIO2_1         //*16+25: Wakeup PIO2.1
  .weak WAKEUP_IRQHandler_PIO2_2         //*16+26: Wakeup PIO2.2
  .weak WAKEUP_IRQHandler_PIO2_3         //*16+27: Wakeup PIO2.3
  .weak WAKEUP_IRQHandler_PIO2_4         //*16+28: Wakeup PIO2.4
  .weak WAKEUP_IRQHandler_PIO2_5         //*16+29: Wakeup PIO2.5
  .weak WAKEUP_IRQHandler_PIO2_6         //*16+30: Wakeup PIO2.6
  .weak WAKEUP_IRQHandler_PIO2_7         //*16+31: Wakeup PIO2.7
  .weak WAKEUP_IRQHandler_PIO2_8         //*16+32: Wakeup PIO2.8
  .weak WAKEUP_IRQHandler_PIO2_9         //*16+33: Wakeup PIO2.9
  .weak WAKEUP_IRQHandler_PIO2_10         //*16+34: Wakeup PIO2.10
  .weak WAKEUP_IRQHandler_PIO2_11         //*16+35: Wakeup PIO2.11
  .weak WAKEUP_IRQHandler_PIO3_0         //*16+36: Wakeup PIO3.0
  .weak WAKEUP_IRQHandler_PIO3_1         //*16+37: Wakeup PIO3.1
  .weak WAKEUP_IRQHandler_PIO3_2         //*16+38: Wakeup PIO3.2
  .weak WAKEUP_IRQHandler_PIO3_3         //*16+39: Wakeup PIO3.3
  .weak I2C_IRQHandler            //*16+40: I2C
  .weak TIMER16_0_IRQHandler      //*16+41: 16-bit Counter-Timer 0
  .weak TIMER16_1_IRQHandler      //*16+42: 16-bit Counter-Timer 1
  .weak TIMER32_0_IRQHandler      //*16+43: 32-bit Counter-Timer 0
  .weak TIMER32_1_IRQHandler      //*16+44: 32-bit Counter-Timer 1
  .weak SSP0_IRQHandler           //*16+45: SSP0
  .weak UART_IRQHandler           //*16+46: UART
  .weak USB_IRQHandler            //*16+47: USB IRQ
  .weak USB_FIQHandler            //*16+48: USB FIQ
  .weak ADC_IRQHandler            //*16+49: A/D Converter
  .weak WDT_IRQHandler            //*16+50: Watchdog Timer
  .weak BOD_IRQHandler            //*16+51: Brown Out Detect
  .weak FMC_IRQHandler            //*16+52: IP2111 Flash Memory Controller
  .weak PIOINT3_IRQHandler        //*16+53: PIO INT3
  .weak PIOINT2_IRQHandler        //*16+54: PIO INT2
  .weak PIOINT1_IRQHandler        //*16+55: PIO INT1
  .weak PIOINT0_IRQHandler        //*16+56: PIO INT0
  .weak SSP1_IRQHandler           //*16+57: SSP1
  .set NMI_interrupt, Startup
  .set Hardfault_interrupt, Startup
  .set MemManage_interrupt, Startup
  .set BusFault_interrupt, Startup
  .set UsageFault_interrupt, Startup
  .set SWI_interrupt, Startup
  .set DebugMonitor_interrupt, Startup
  .set PendingSV_interrupt, Startup
  .set SysTick_interrupt, Startup
  .set WAKEUP_IRQHandler_PIO0_0, Startup         //*16+ 0: Wakeup PIO0.0
  .set WAKEUP_IRQHandler_PIO0_1, Startup         //*16+ 1: Wakeup PIO0.1
  .set WAKEUP_IRQHandler_PIO0_2, Startup         //*16+ 2: Wakeup PIO0.2
  .set WAKEUP_IRQHandler_PIO0_3, Startup         //*16+ 3: Wakeup PIO0.3
  .set WAKEUP_IRQHandler_PIO0_4, Startup         //*16+ 4: Wakeup PIO0.4
  .set WAKEUP_IRQHandler_PIO0_5, Startup         //*16+ 5: Wakeup PIO0.5
  .set WAKEUP_IRQHandler_PIO0_6, Startup         //*16+ 6: Wakeup PIO0.6
  .set WAKEUP_IRQHandler_PIO0_7, Startup         //*16+ 7: Wakeup PIO0.7
  .set WAKEUP_IRQHandler_PIO0_8, Startup         //*16+ 8: Wakeup PIO0.8
  .set WAKEUP_IRQHandler_PIO0_9, Startup         //*16+ 9: Wakeup PIO0.9
  .set WAKEUP_IRQHandler_PIO0_10, Startup         //*16+10: Wakeup PIO0.10
  .set WAKEUP_IRQHandler_PIO0_11, Startup         //*16+11: Wakeup PIO0.11
  .set WAKEUP_IRQHandler_PIO1_0, Startup        //*16+12: Wakeup PIO1.0
  .set WAKEUP_IRQHandler_PIO1_1, Startup         //*16+13: Wakeup PIO1.1
  .set WAKEUP_IRQHandler_PIO1_2, Startup         //*16+14: Wakeup PIO1.2
  .set WAKEUP_IRQHandler_PIO1_3, Startup         //*16+15: Wakeup PIO1.3
  .set WAKEUP_IRQHandler_PIO1_4, Startup         //*16+16: Wakeup PIO1.4
  .set WAKEUP_IRQHandler_PIO1_5, Startup         //*16+17: Wakeup PIO1.5
  .set WAKEUP_IRQHandler_PIO1_6, Startup         //*16+18: Wakeup PIO1.6
  .set WAKEUP_IRQHandler_PIO1_7, Startup         //*16+19: Wakeup PIO1.7
  .set WAKEUP_IRQHandler_PIO1_8, Startup        //*16+20: Wakeup PIO1.8
  .set WAKEUP_IRQHandler_PIO1_9, Startup         //*16+21: Wakeup PIO1.9
  .set WAKEUP_IRQHandler_PIO1_10, Startup         //*16+22: Wakeup PIO1.10
  .set WAKEUP_IRQHandler_PIO1_11, Startup         //*16+23: Wakeup PIO1.11
  .set WAKEUP_IRQHandler_PIO2_0, Startup         //*16+24: Wakeup PIO2.0
  .set WAKEUP_IRQHandler_PIO2_1, Startup         //*16+25: Wakeup PIO2.1
  .set WAKEUP_IRQHandler_PIO2_2, Startup         //*16+26: Wakeup PIO2.2
  .set WAKEUP_IRQHandler_PIO2_3, Startup         //*16+27: Wakeup PIO2.3
  .set WAKEUP_IRQHandler_PIO2_4, Startup         //*16+28: Wakeup PIO2.4
  .set WAKEUP_IRQHandler_PIO2_5, Startup         //*16+29: Wakeup PIO2.5
  .set WAKEUP_IRQHandler_PIO2_6, Startup         //*16+30: Wakeup PIO2.6
  .set WAKEUP_IRQHandler_PIO2_7, Startup         //*16+31: Wakeup PIO2.7
  .set WAKEUP_IRQHandler_PIO2_8, Startup         //*16+32: Wakeup PIO2.8
  .set WAKEUP_IRQHandler_PIO2_9, Startup         //*16+33: Wakeup PIO2.9
  .set WAKEUP_IRQHandler_PIO2_10, Startup         //*16+34: Wakeup PIO2.10
  .set WAKEUP_IRQHandler_PIO2_11, Startup         //*16+35: Wakeup PIO2.11
  .set WAKEUP_IRQHandler_PIO3_0, Startup         //*16+36: Wakeup PIO3.0
  .set WAKEUP_IRQHandler_PIO3_1, Startup         //*16+37: Wakeup PIO3.1
  .set WAKEUP_IRQHandler_PIO3_2, Startup         //*16+38: Wakeup PIO3.2
  .set WAKEUP_IRQHandler_PIO3_3, Startup         //*16+39: Wakeup PIO3.3
  .set I2C_IRQHandler, Startup                  //*16+40: I2C
  .set TIMER16_0_IRQHandler, Startup      //*16+41: 16-bit Counter-Timer 0
  .set TIMER16_1_IRQHandler, Startup      //*16+42: 16-bit Counter-Timer 1
  .set TIMER32_0_IRQHandler, Startup      //*16+43: 32-bit Counter-Timer 0
  .set TIMER32_1_IRQHandler, Startup      //*16+44: 32-bit Counter-Timer 1
  .set SSP0_IRQHandler, Startup           //*16+45: SSP0
  .set UART_IRQHandler, Startup           //*16+46: UART
  .set USB_IRQHandler, Startup            //*16+47: USB IRQ
  .set USB_FIQHandler, Startup            //*16+48: USB FIQ
  .set ADC_IRQHandler, Startup            //*16+49: A/D Converter
  .set WDT_IRQHandler, Startup            //*16+50: Watchdog Timer
  .set BOD_IRQHandler, Startup            //*16+51: Brown Out Detect
  .set FMC_IRQHandler, Startup            //*16+52: IP2111 Flash Memory Controller
  .set PIOINT3_IRQHandler, Startup        //*16+53: PIO INT3
  .set PIOINT2_IRQHandler, Startup        //*16+54: PIO INT2
  .set PIOINT1_IRQHandler, Startup        //*16+55: PIO INT1
  .set PIOINT0_IRQHandler, Startup        //*16+56: PIO INT0
  .set SSP1_IRQHandler, Startup           //*16+57: SSP1

  .text
end;


end.

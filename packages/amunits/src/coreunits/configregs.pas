{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit configregs;

INTERFACE

uses exec;

{
** AutoConfig (tm) boards each contain a 32 byte "ExpansionRom" area that is
** read by the system software at configuration time.  Configuration of each
** board starts when the ConfigIn* signal is passed from the previous board
** (or from the system for the first board).  Each board will present it's
** ExpansionRom structure at location $00E80000 to be read by the system.
** This file defines the appearance of the ExpansionRom area.
**
** Expansion boards are actually organized such that only one nybble per
** 16 bit word contains valid information.  The low nybbles of each
** word are combined to fill the structure below. (This table is structured
** as LOGICAL information.  This means that it never corresponds exactly
** with a physical implementation.)
**
** The ExpansionRom space is further split into two regions:  The first 16
** bytes are read-only.  Except for the er_type field, this area is inverted
** by the system software when read in.  The second 16 bytes contain the
** control portion, where all read/write registers are located.
**
** The system builds one "ConfigDev" structure for each board found.  The
** list of boards can be examined using the expansion.library/FindConfigDev
** function.
**
** A special "hacker" Manufacturer ID number is reserved for test use:
** 2011 ($7DB).  When inverted this will look like $F824.
}

Type

    pExpansionRom = ^tExpansionRom;
    tExpansionRom = record
        er_Type         : Byte;
        er_Product      : Byte;
        er_Flags        : Byte;
        er_Reserved03   : Byte;
        er_Manufacturer : Word;
        er_SerialNumber : ULONG;
        er_InitDiagVec  : Word;
        er_Reserved0c   : Byte;
        er_Reserved0d   : Byte;
        er_Reserved0e   : Byte;
        er_Reserved0f   : Byte;
    end;

{
** Note that use of the ec_BaseAddress register is tricky.  The system
** will actually write twice.  First the low order nybble is written
** to the ec_BaseAddress register+2 (D15-D12).  Then the entire byte is
** written to ec_BaseAddress (D15-D8).  This allows writing of a byte-wide
** address to nybble size registers.
}

    pExpansionControl = ^tExpansionControl;
    tExpansionControl = record
        ec_Interrupt    : Byte;         { interrupt control register }
        ec_Reserved11   : Byte;
        ec_BaseAddress  : Byte;         { set new config address }
        ec_Shutup       : Byte;         { don't respond, pass config out }
        ec_Reserved14   : Byte;
        ec_Reserved15   : Byte;
        ec_Reserved16   : Byte;
        ec_Reserved17   : Byte;
        ec_Reserved18   : Byte;
        ec_Reserved19   : Byte;
        ec_Reserved1a   : Byte;
        ec_Reserved1b   : Byte;
        ec_Reserved1c   : Byte;
        ec_Reserved1d   : Byte;
        ec_Reserved1e   : Byte;
        ec_Reserved1f   : Byte;
    end;

{
** many of the constants below consist of a triplet of equivalent
** definitions: xxMASK is a bit mask of those bits that matter.
** xxBIT is the starting bit number of the field.  xxSIZE is the
** number of bits that make up the definition.  This method is
** used when the field is larger than one bit.
**
** If the field is only one bit wide then the xxB_xx and xxF_xx convention
** is used (xxB_xx is the bit number, and xxF_xx is mask of the bit).
}

Const

{ manifest constants }

           E_SLOTSIZE          = $10000;
           E_SLOTMASK          = -1;
           E_SLOTSHIFT         = 16;

{ these define the two free regions of Zorro memory space.
** THESE MAY WELL CHANGE FOR FUTURE PRODUCTS!
}

           E_EXPANSIONBASE     = $e80000;
           EZ3_EXPANSIONBASE   = $ff000000;      {           Zorro III config address }
           E_EXPANSIONSIZE     = $080000;
           E_EXPANSIONSLOTS    = 8;

           E_MEMORYBASE        = $200000;
           E_MEMORYSIZE        = $800000;
           E_MEMORYSLOTS       = 128;

           EZ3_CONFIGAREA      =    $40000000;      {           Zorro III space }
           EZ3_CONFIGAREAEND   =    $7FFFFFFF;      {           Zorro III space }
           EZ3_SIZEGRANULARITY =    $00080000;      {           512K increments }



{          *** er_Type definitions (ttldcmmm) **************************************}

{           er_Type board type bits -- the OS ignores "old style" boards }
           ERT_TYPEMASK          =  $c0;    {Bits 7-6 }
           ERT_TYPEBIT           =  6  ;
           ERT_TYPESIZE          =  2  ;
           ERT_NEWBOARD          =  $c0;
           ERT_ZORROII           =  ERT_NEWBOARD;
           ERT_ZORROIII          =  $80;

{           other bits defined in er_Type }
           ERTB_MEMLIST          =  5;   {           Link RAM into free memory list }
           ERTB_DIAGVALID        =  4;   {           ROM vector is valid }
           ERTB_CHAINEDCONFIG    =  3;   {           Next config is part of the same card }

           ERTF_MEMLIST          =  32;
           ERTF_DIAGVALID        =  16;
           ERTF_CHAINEDCONFIG    =  8;

{           er_Type field memory size bits }
           ERT_MEMMASK           =  $07;    {Bits 2-0 }
           ERT_MEMBIT            =  0  ;
           ERT_MEMSIZE           =  3  ;



{          *** er_Flags byte -- for those things that didn't fit into the type byte ***}
{          *** the hardware stores this byte in inverted form                       ***}
           ERFF_MEMSPACE         =  128;     {           Wants to be in 8 meg space. }
           ERFB_MEMSPACE         =  7;       {           (NOT IMPLEMENTED) }

           ERFF_NOSHUTUP         =  64;      {           Board can't be shut up }
           ERFB_NOSHUTUP         =  6;

           ERFF_EXTENDED         =  32;      {           Zorro III: Use extended size table }
           ERFB_EXTENDED         =  5;       {                      for bits 0-2 of er_Type }
                                             {           Zorro II : Must be 0 }

           ERFF_ZORRO_III        =  16;      {           Zorro III: must be 1 }
           ERFB_ZORRO_III        =  4;       {           Zorro II : must be 0 }

           ERT_Z3_SSMASK         =  $0F;     {           Bits 3-0.  Zorro III Sub-Size.  How }
           ERT_Z3_SSBIT          =  0;       {           much space the card actually uses   }
           ERT_Z3_SSSIZE         =  4;       {           (regardless of config granularity)  }
                                             {           Zorro II : must be 0        }


{           ec_Interrupt register (unused) *******************************************}
           ECIB_INTENA           =  1;
           ECIB_RESET            =  3;
           ECIB_INT2PEND         =  4;
           ECIB_INT6PEND         =  5;
           ECIB_INT7PEND         =  6;
           ECIB_INTERRUPTING     =  7;

           ECIF_INTENA           =  2;
           ECIF_RESET            =  8;
           ECIF_INT2PEND         =  16;
           ECIF_INT6PEND         =  32;
           ECIF_INT7PEND         =  64;
           ECIF_INTERRUPTING     =  128;


{**************************************************************************
**
** these are the specifications for the diagnostic area.  If the Diagnostic
** Address Valid bit is set in the Board Type byte (the first byte in
** expansion space) then the Diag Init vector contains a valid offset.
**
** The Diag Init vector is actually a word offset from the base of the
** board.  The resulting address points to the base of the DiagArea
** structure.  The structure may be physically implemented either four,
** eight, or sixteen bits wide.  The code will be copied out into
** ram first before being called.
**
** The da_Size field, and both code offsets (da_DiagPoint and da_BootPoint)
** are offsets from the diag area AFTER it has been copied into ram, and
** "de-nibbleized" (if needed).  Inotherwords, the size is the size of
** the actual information, not how much address space is required to
** store it.
**
** All bits are encoded with uninverted logic (e.g. 5 volts on the bus
** is a logic one).
**
** If your board is to make use of the boot facility then it must leave
** its config area available even after it has been configured.  Your
** boot vector will be called AFTER your board's final address has been
** set.
**
***************************************************************************}

Type

    pDiagArea = ^tDiagArea;
    tDiagArea = record
        da_Config       : Byte;         { see below for definitions }
        da_Flags        : Byte;         { see below for definitions }
        da_Size         : Word;         { the size (in bytes) of the total diag area }
        da_DiagPoint    : Word;         { where to start for diagnostics, or zero }
        da_BootPoint    : Word;         { where to start for booting }
        da_Name         : Word;         { offset in diag area where a string }
                                        {   identifier can be found (or zero if no }
                                        {   identifier is present). }

        da_Reserved01   : Word;         { two words of reserved data.   must be zero. }
        da_Reserved02   : Word;
    end;

Const

{ da_Config definitions }

    DAC_BUSWIDTH        = $C0;  { two bits for bus width }
    DAC_NIBBLEWIDE      = $00;
    DAC_BYTEWIDE        = $40;
    DAC_WORDWIDE        = $80;

    DAC_BOOTTIME        = $30;  { two bits for when to boot }
    DAC_NEVER           = $00;  { obvious }
    DAC_CONFIGTIME      = $10;  { call da_BootPoint when first configing the }
                                {   the device }
    DAC_BINDTIME        = $20;  { run when binding drivers to boards }

{
** These are the calling conventions for Diag or Boot area
**
** A7 -- points to at least 2K of stack
** A6 -- ExecBase
** A5 -- ExpansionBase
** A3 -- your board's ConfigDev structure
** A2 -- Base of diag/init area that was copied
** A0 -- Base of your board
**
** Your board should return a value in D0.  If this value is NULL, then
** the diag/init area that was copied in will be returned to the free
** memory pool.
}

IMPLEMENTATION

end.

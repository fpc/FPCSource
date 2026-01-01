{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by Free Pascal development team

    card.resource interface for m68k-amiga

    Pascal translation of:
    card.h 1.11 (14.12.1992) 
    (C) Copyright 1991-1999 Amiga, Inc.

    With misc. extensions from various other sources.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit cardres;
{$ENDIF FPC_DOTTEDUNITS}

{$PACKRECORDS 2}

interface

uses
  exec;

const
  CARDRESNAME = 'card.resource';

{* Structures used by the card.resource                                *}

type
  PCardHandle = ^TCardHandle;
  TCardHandle = record
    cah_CardNode: TNode;
    cah_CardRemoved: PInterrupt;
    cah_CardInserted: PInterrupt;
    cah_CardStatus: PInterrupt;
    cah_CardFlags: byte;
  end;

type
  PDeviceTData = ^TDeviceTData;
  TDeviceTData = record
    dtd_DTsize: DWord;  {* Size in bytes                *}
    dtd_DTspeed: DWord; {* Speed in nanoseconds         *}
    dtd_DTtype: Byte;   {* Type of card                 *}
    dtd_DTflags: Byte;  {* Other flags                  *}
  end;

type
  PCardMemoryMap = ^TCardMemoryMap;
  TCardMemoryMap = record
    cmm_CommonMemory: PByte;
    cmm_AttributeMemory: PByte;
    cmm_IOMemory: PByte;

{* Extended for V39 - These are the size of the memory spaces above *}

    cmm_CommonMemSize: DWord;
    cmm_AttributeMemSize: DWord;
    cmm_IOMemSize: DWord;
  end;

{* CardHandle.cah_CardFlags for OwnCard() function *}

const
  CARDB_RESETREMOVE = 0;
  CARDF_RESETREMOVE = (1 shl CARDB_RESETREMOVE);

  CARDB_IFAVAILABLE = 1;
  CARDF_IFAVAILABLE = (1 shl CARDB_IFAVAILABLE);

  CARDB_DELAYOWNERSHIP = 2;
  CARDF_DELAYOWNERSHIP = (1 shl CARDB_DELAYOWNERSHIP);

  CARDB_POSTSTATUS = 3;
  CARDF_POSTSTATUS = (1 shl CARDB_POSTSTATUS);

{* ReleaseCreditCard() function flags *}
const
  CARDB_REMOVEHANDLE = 0;
  CARDF_REMOVEHANDLE = (1 shl CARDB_REMOVEHANDLE);

{* ReadStatus() return flags *}
const
  CARD_STATUSB_CCDET = 6;
  CARD_STATUSF_CCDET = (1 shl CARD_STATUSB_CCDET);

  CARD_STATUSB_BVD1 = 5;
  CARD_STATUSF_BVD1 = (1 shl CARD_STATUSB_BVD1);

  CARD_STATUSB_SC = 5;
  CARD_STATUSF_SC = (1 shl CARD_STATUSB_SC);

  CARD_STATUSB_BVD2 = 4;
  CARD_STATUSF_BVD2 = (1 shl CARD_STATUSB_BVD2);

  CARD_STATUSB_DA = 4;
  CARD_STATUSF_DA = (1 shl CARD_STATUSB_DA);

  CARD_STATUSB_WR = 3;
  CARD_STATUSF_WR = (1 shl CARD_STATUSB_WR);

  CARD_STATUSB_BSY = 2;
  CARD_STATUSF_BSY = (1 shl CARD_STATUSB_BSY);

  CARD_STATUSB_IRQ = 2;
  CARD_STATUSF_IRQ = (1 shl CARD_STATUSB_IRQ);

{* CardProgramVoltage() defines *}
const
  CARD_VOLTAGE_0V = 0; {* Set to default; may be the same as 5V *}
  CARD_VOLTAGE_5V = 1;
  CARD_VOLTAGE_12V = 2;

{* CardMiscControl() defines *}

  CARD_ENABLEB_DIGAUDIO = 1;
  CARD_ENABLEF_DIGAUDIO = (1 shl CARD_ENABLEB_DIGAUDIO);

  CARD_DISABLEB_WP = 3;
  CARD_DISABLEF_WP = (1 shl CARD_DISABLEB_WP);

{*
 * New CardMiscControl() bits for V39 card.resource.  Use these bits to set,
 * or clear status change interrupts for BVD1/SC, BVD2/DA, and BSY/IRQ.
 * Write-enable/protect change interrupts are always enabled.  The defaults
 * are unchanged (BVD1/SC is enabled, BVD2/DA is disabled, and BSY/IRQ is enabled).
 *
 * IMPORTANT -- Only set these bits for V39 card.resource or greater (check
 * resource base VERSION)
 *
 *}
const
  CARD_INTB_SETCLR = 7;
  CARD_INTF_SETCLR = (1 shl CARD_INTB_SETCLR);

  CARD_INTB_BVD1 = 5;
  CARD_INTF_BVD1 = (1 shl CARD_INTB_BVD1);

  CARD_INTB_SC = 5;
  CARD_INTF_SC = (1 shl CARD_INTB_SC);

  CARD_INTB_BVD2 = 4;
  CARD_INTF_BVD2 = (1 shl CARD_INTB_BVD2);

  CARD_INTB_DA = 4;
  CARD_INTF_DA = (1 shl CARD_INTB_DA);

  CARD_INTB_BSY = 2;
  CARD_INTF_BSY = (1 shl CARD_INTB_BSY);

  CARD_INTB_IRQ = 2;
  CARD_INTF_IRQ = (1 shl CARD_INTB_IRQ);


{* CardInterface() defines *}
const
  CARD_INTERFACE_AMIGA_0 = 0;

{*
 * Tuple for Amiga execute-in-place software (e.g., games, or other
 * such software which wants to use execute-in-place software stored
 * on a credit-card, such as a ROM card).
 *
 * See documentation for IfAmigaXIP().
 *}
const
  CISTPL_AMIGAXIP = $91;

type
  TP_AmigaXIP = record
    TPL_CODE: byte;
    TPL_LINK: byte;
    TP_XIPLOC: array[0..3] of byte;
    TP_XIPFLAGS: byte;
    TP_XIPRESRV: byte;
  end;
{*

    ; The XIPFLAGB_AUTORUN bit means that you want the machine
    ; to perform a reset if the execute-in-place card is inserted
    ; after DOS has been started.  The machine will then reset,
    ; and execute your execute-in-place code the next time around.
    ;
    ; NOTE -- this flag may be ignored on some machines, in which
    ; case the user will have to manually reset the machine in the
    ; usual way.

*}
const
  XIPFLAGSB_AUTORUN = 0;
  XIPFLAGSF_AUTORUN = (1 shl XIPFLAGSB_AUTORUN);

var
  CardResBase: Pointer;

function OwnCard(handle: PCardHandle location 'a1'): PCardHandle; syscall CardResBase 6;
procedure ReleaseCard(handle: PCardHandle location 'a1'; flags: Dword location 'd0'); syscall CardResBase 12;
function GetCardMap: PCardMemoryMap; syscall CardResBase 18;
function BeginCardAccess(handle: PCardHandle location 'a1'): longbool; syscall CardResBase 24;
function EndCardAccess(handle: PCardHandle location 'a1'): longbool; syscall CardResBase 30;
function ReadCardStatus: byte; syscall CardResBase 36; 
function CardResetRemove(handle: PCardHandle location 'a1'; flag: dword location 'd0'): longbool; syscall CardResBase 42;
function CardMiscControl(handle: PCardHandle location 'a1'; control_bits: dword location 'd1'): byte; syscall CardResBase 48;
function CardAccessSpeed(handle: PCardHandle location 'a1'; nanoseconds: dword location 'd0'): dword; syscall CardResBase 54;
function CardProgramVoltage(handle: PCardHandle location 'a1'; voltage: dword location 'd0'): longint; syscall CardResBase 60;
function CardResetCard(handle: PCardHandle location 'a1'): longbool; syscall CardResBase 66;
function CopyTuple(handle: PCardHandle location 'a1'; buffer: PByte location 'a0'; tuplecode: dword location 'd1'; size: dword location 'd0'): longbool; syscall CardResBase 72;
function DeviceTuple(tuple_data: PByte location 'a0'; storage: PDeviceTData location 'a1'): dword; syscall CardResBase 78;
function IfAmigaXIP(handle: PCardHandle location 'a2'): PResident; syscall CardResBase 84;
function CardForceChange: longbool; syscall CardResBase 90;
function CardChangeCount: DWord; syscall CardResBase 96;
function CardInterface: DWord; syscall CardResBase 102;

implementation

begin
  CardResBase:=OpenResource(CARDRESNAME);
end.

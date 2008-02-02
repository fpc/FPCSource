{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ExpansionMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header file for Expansion Manager.
 *
 * History:
 *    02/25/00 jed   Created by Jesse Donaldson.
 *
 *****************************************************************************)
unit expansionmgr;
interface
uses palmos, coretraps, errorbase;
const
  sysTrapExpansionMgr = sysTrapExpansionDispatch;
  expFtrIDVersion = 0; // ID of feature containing version of ExpansionMgr.
                       // Check existence of this feature to see if ExpMgr is installed.
  expMgrVersionNum = UInt16(200); // version of the ExpansionMgr, obtained from the feature
  expInvalidSlotRefNum = 0;
type
  ExpPollingProcPtr = function(slotLibRefNum: UInt16; slotPollRefConP: Pointer): Err;
(************************************************************
 * Capabilities of the hardware device for ExpCardInfoType.capabilityFlags
 *************************************************************)
const
  expCapabilityHasStorage = $00000001; // card supports reading (& maybe writing) sectors
  expCapabilityReadOnly   = $00000002; // card is read only
  expCapabilitySerial     = $00000004; // card supports dumb serial interface
  expCardInfoStringMaxLen = 31;
type
  ExpCardInfoTag = record
    capabilityFlags: UInt32;                                       // bits for different stuff the card supports
    manufacturerStr: array [0..expCardInfoStringMaxLen] of Char;   // Manufacturer, e.g., "Palm", "Motorola", etc...
    productStr: array [0..expCardInfoStringMaxLen] of Char;        // Name of product, e.g., "SafeBackup 32MB"
    deviceClassStr: array [0..expCardInfoStringMaxLen] of Char;    // Type of product, e.g., "Backup", "Ethernet", etc.
    deviceUniqueIDStr: array [0..expCardInfoStringMaxLen] of Char; // Unique identifier for product, e.g., a serial number.  Set to "" if no such identifier exists.
  end;
  ExpCardInfoType = ExpCardInfoTag;
  ExpCardInfoPtr = ^ExpCardInfoType;
(************************************************************
 * Iterator start and stop constants.
 * Used by ExpSlotEnumerate
 *************************************************************)
const
  expIteratorStart = 0;
  expIteratorStop  = $ffffffff;
(************************************************************
 * Bits in the 'handled' field used in Card Inserted and Removed notifications
 *************************************************************)
  expHandledVolume = $01; // any volumes associated with the card have been dealt with... the ExpansionMgr will not mount or unmount as appropriate.
  expHandledSound  = $02; // Any pleasing sounds have already been played... the ExpansionMgr will not play a pleasing sound on this insertion/removal.
(************************************************************
 * Error codes
 *************************************************************)
  expErrUnsupportedOperation  = expErrorClass or 1;  // unsupported or undefined opcode and/or creator
  expErrNotEnoughPower        = expErrorClass or 2;  // the required power is not available
  expErrCardNotPresent        = expErrorClass or 3;  // no card is present
  expErrInvalidSlotRefNum     = expErrorClass or 4;  // slot reference number is bad
  expErrSlotDeallocated       = expErrorClass or 5;  // slot reference number is within valid range, but has been deallocated.
  expErrCardNoSectorReadWrite = expErrorClass or 6;  // the card does not support the
                                                     // SlotDriver block read/write API
  expErrCardReadOnly          = expErrorClass or 7;  // the card does support R/W API
                                                     // but the card is read only
  expErrCardBadSector         = expErrorClass or 8;  // the card does support R/W API
                                                     // but the sector is bad
  expErrCardProtectedSector   = expErrorClass or 9;  // The card does support R/W API
                                                     // but the sector is protected
  expErrNotOpen               = expErrorClass or 10; // slot driver library has not been opened
  expErrStillOpen             = expErrorClass or 11; // slot driver library is still open - maybe it was opened > once
  expErrUnimplemented         = expErrorClass or 12; // Call is unimplemented
  expErrEnumerationEmpty      = expErrorClass or 13; // No values remaining to enumerate
  expErrIncompatibleAPIVer    = expErrorClass or 14; // The API version of this slot driver is not supported by this version of ExpansionMgr.
(************************************************************
 * Common media types.  Used by SlotCardMediaType and SlotMediaType.
 *************************************************************)
  expMediaType_Any            = Rsc('wild'); // matches all media types when looking up a default directory
  expMediaType_MemoryStick    = Rsc('mstk');
  expMediaType_CompactFlash   = Rsc('cfsh');
  expMediaType_SecureDigital  = Rsc('sdig');
  expMediaType_MultiMediaCard = Rsc('mmcd');
  expMediaType_SmartMedia     = Rsc('smed');
  expMediaType_RAMDisk        = Rsc('ramd'); // a RAM disk based media
  expMediaType_PoserHost      = Rsc('pose'); // Host filesystem emulated by Poser
  expMediaType_MacSim         = Rsc('PSim'); // Host filesystem emulated by Poser
(************************************************************
 * Selectors for routines found in the Expansion manager. The order
 * of these selectors MUST match the jump table in ExpansionMgr.c.
 *************************************************************)
  expInit_              = 0;
  expSlotDriverInstall_ = 1;
  expSlotDriverRemove_  = 2;
  expSlotLibFind_       = 3;
  expSlotRegister_      = 4;
  expSlotUnregister_    = 5;
  expCardInserted_      = 6;
  expCardRemoved_       = 7;
  expCardPresent_       = 8;
  expCardInfo_          = 9;
  expSlotEnumerate_     = 10;
  expCardGetSerialPort_ = 11;
  expMaxSelector        = expCardGetSerialPort_;
function ExpInit: Err;
function ExpSlotDriverInstall(dbCreator: UInt32; var slotLibRefNumP: UInt16): Err;
function ExpSlotDriverRemove(slotLibRefNum: UInt16): Err;
function ExpSlotLibFind(slotRefNum: UInt16; var slotLibRefNum: UInt16): Err;
function ExpSlotRegister(slotLibRefNum: UInt16; var slotRefNum: UInt16): Err;
function ExpSlotUnregister(slotRefNum: UInt16): Err;
function ExpCardInserted(slotRefNum: UInt16): Err;
function ExpCardRemoved(slotRefNum: UInt16): Err;
function ExpCardPresent(slotRefNum: UInt16): Err;
function ExpCardInfo(slotRefNum: UInt16; var infoP: ExpCardInfoType): Err;
function ExpSlotEnumerate(var slotRefNumP: UInt16; var slotIteratorP: UInt32): Err;
function ExpCardGetSerialPort(slotRefNum: UInt16; var portP: UInt32): Err;

implementation
(**)
function __ExpInit: Err; syscall sysTrapExpansionMgr;
function __ExpSlotDriverInstall(dbCreator: UInt32; var slotLibRefNumP: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpSlotDriverRemove(slotLibRefNum: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpSlotLibFind(slotRefNum: UInt16; var slotLibRefNum: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpSlotRegister(slotLibRefNum: UInt16; var slotRefNum: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpSlotUnregister(slotRefNum: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpCardInserted(slotRefNum: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpCardRemoved(slotRefNum: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpCardPresent(slotRefNum: UInt16): Err; syscall sysTrapExpansionMgr;
function __ExpCardInfo(slotRefNum: UInt16; var infoP: ExpCardInfoType): Err; syscall sysTrapExpansionMgr;
function __ExpSlotEnumerate(var slotRefNumP: UInt16; var slotIteratorP: UInt32): Err; syscall sysTrapExpansionMgr;
function __ExpCardGetSerialPort(slotRefNum: UInt16; var portP: UInt32): Err; syscall sysTrapExpansionMgr;
(**)

function ExpInit: Err;
begin
 asm
  move.l #expInit_,D2;
 end;
 ExpInit := __ExpInit;
end;

function ExpSlotDriverInstall(dbCreator: UInt32; var slotLibRefNumP: UInt16): Err;
begin
 asm
  move.l #expSlotDriverInstall_,D2;
 end;
 ExpSlotDriverInstall := __ExpSlotDriverInstall(dbCreator, slotLibRefNumP);
end;

function ExpSlotDriverRemove(slotLibRefNum: UInt16): Err;
begin
 asm
  move.l #expSlotDriverRemove_,D2;
 end;
 ExpSlotDriverRemove := __ExpSlotDriverRemove(slotLibRefNum);
end;

function ExpSlotLibFind(slotRefNum: UInt16; var slotLibRefNum: UInt16): Err;
begin
 asm
  move.l #expSlotLibFind_,D2;
 end;
 ExpSlotLibFind := __ExpSlotLibFind(slotRefNum, slotLibRefNum);
end;

function ExpSlotRegister(slotLibRefNum: UInt16; var slotRefNum: UInt16): Err;
begin
 asm
  move.l #expSlotRegister_,D2;
 end;
 ExpSlotRegister := __ExpSlotRegister(slotLibRefNum, slotRefNum);
end;

function ExpSlotUnregister(slotRefNum: UInt16): Err;
begin
 asm
  move.l #expSlotUnregister_,D2;
 end;
 ExpSlotUnregister := __ExpSlotUnregister(slotRefNum);
end;

function ExpCardInserted(slotRefNum: UInt16): Err;
begin
 asm
  move.l #expCardInserted_,D2;
 end;
 ExpCardInserted := __ExpCardInserted(slotRefNum);
end;

function ExpCardRemoved(slotRefNum: UInt16): Err;
begin
 asm
  move.l #expCardRemoved_,D2;
 end;
 ExpCardRemoved := __ExpCardRemoved(slotRefNum);
end;

function ExpCardPresent(slotRefNum: UInt16): Err;
begin
 asm
  move.l #expCardPresent_,D2;
 end;
 ExpCardPresent := __ExpCardPresent(slotRefNum);
end;

function ExpCardInfo(slotRefNum: UInt16; var infoP: ExpCardInfoType): Err;
begin
 asm
  move.l #expCardInfo_,D2;
 end;
 ExpCardInfo := __ExpCardInfo(slotRefNum, infoP);
end;

function ExpSlotEnumerate(var slotRefNumP: UInt16; var slotIteratorP: UInt32): Err;
begin
 asm
  move.l #expSlotEnumerate_,D2;
 end;
 ExpSlotEnumerate := __ExpSlotEnumerate(slotRefNumP, slotIteratorP);
end;

function ExpCardGetSerialPort(slotRefNum: UInt16; var portP: UInt32): Err;
begin
 asm
  move.l #expCardGetSerialPort_,D2;
 end;
 ExpCardGetSerialPort := __ExpCardGetSerialPort(slotRefNum, portP);
end;


end.

{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1998-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SlotDrvrLib.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *                Sample Slot Driver library implementation.
 *
 * History:
 *    02/25/00 Created by Steve Minns
 *    09/15/00 lrt      Updated CardMetricsType stucture to include more
 *                   useful fields, inc'd API Version
 *
 *****************************************************************************)

unit slotdrvrlib;

interface

uses palmos, libtraps, expansionmgr;

const
  slotDrvrAPIVersion = $00000002;

// The number of bytes per sector is fixed
  slotSectorSize = 512;

(********************************************************************
 * Card Metrics
 * These structures contains all of the information about the physical
 * structure of the card that may be needed by a filesystem in order
 * to format volumes on the card.
 ********************************************************************)

  slotDrvrPartitionTypeFAT12          = $01;
  slotDrvrPartitionTypeFAT16Under32MB = $04;
  slotDrvrPartitionTypeFAT16Over32MB  = $06;
  slotDrvrBootablePartition           = $80;
  slotDrvrNonBootablePartition        = $00;

type
  CardMetricsType = record
    totalSectors: UInt32;     // The total number of sectors accessable via SlotCardSector[Read/Write]
                              //    (some media may contain extra sectors in case one goes bad,
                              //     or for storing configuration information, but they are handled
                              //     internally to the slot driver, and not accessable)
    bytesPerSector: UInt16;   // The number of bytes in one sector.
                              //    currently for Palm, this must be the standard 512
    sectorsPerHead: UInt16;   // The number of Sectors per Head
                              //    as given by guidelines in the specification for this media type
                              //    even though all of our disks accesses are LBA,
                              //    this is for compatibility when filling out MBRs and PBRs
                              //       if the media guidelines don't care, this value is set to 0
    headsPerCylinder: UInt16; // The number of Heads per Cylinder
                              //    as given by guidelines in the specification for this media type
                              //    even though all of our disks accesses are LBA,
                              //    this is for compatibility when filling out MBRs and PBRs
                              //       if the media guidelines don't care, this value is set to 0
    reserved1: UInt16;        // Reserved
    sectorsPerBlock: UInt8;   // A suggested number of Sectors per Block (Cluster)
                              //    as given by guidelines in the specification for this media type
                              //    if the media guidelines don't care, this value will be set to 0

    partitionType: UInt8;     // The suggested partition type (System ID) of the first partition
                              //    as given by guidelines in the specification for this media type
                              //    if the media guidelines don't care, this value will be set to 0
    bootIndicator: UInt8;     // The suggested bootability of the first partition
                              //    as given by guidelines in the specification for this media type
                              //    (generally, 0x80=bootable, default boot partition 0x00=not-bootable)
                              //    if the media guidelines don't care, this value will be set to 0xFF
    reserved2: UInt8;         // Reserved
    partitionStart: UInt32;   // The suggested starting sector of the first partition
                              //    as given by guidelines in the specification for this media type
                              //    if this value is set to zero, and the partitionSize value is non-zero
                              //     the media guidelines suggest to not use an MBR, and only use a PBR at sector 0
                              //    if the media guidelines don't care, the partitionSize value will be set to 0

    partitionSize: UInt32;    // The suggested size of the first partition
                              //    as given by guidelines in the specification for this media type
                              //       if the media guidelines don't care, this value will be set to 0, and
                              //     the partitionStart parameter is also ignored
   end;
   CardMetricsTag = CardMetricsType;
   CardMetricsPtr = ^CardMetricsType;

(********************************************************************
 * SlotDrvr library function trap ID's. Each library call gets a trap number:
 *   SlotDrvrLibTrapXXXX which serves as an index into the library's dispatch table.
 *   The constant sysLibTrapCustom is the first available trap number after
 *   the system predefined library traps Open,Close,Sleep & Wake.
 *
 * WARNING!!! The order of these traps MUST match the order of the dispatch
 *  table in SlotDrvrLibDispatch.c!!!
 ********************************************************************)

const
  SlotTrapLibAPIVersion             = sysLibTrapCustom;
  SlotTrapCustomControl             = sysLibTrapCustom + 1;
  SlotTrapCardPresent               = sysLibTrapCustom + 2;
  SlotTrapCardInfo                  = sysLibTrapCustom + 3;
  SlotTrapCardMediaType             = sysLibTrapCustom + 4;
  SlotTrapCardIsFilesystemSupported = sysLibTrapCustom + 5;
  SlotTrapCardMetrics               = sysLibTrapCustom + 6;
  SlotTrapCardLowLevelFormat        = sysLibTrapCustom + 7;
  SlotTrapCardSectorRead            = sysLibTrapCustom + 8;
  SlotTrapCardSectorWrite           = sysLibTrapCustom + 9;
  SlotTrapPowerCheck                = sysLibTrapCustom + 10;
  SlotTrapMediaType                 = sysLibTrapCustom + 11;
  SlotTrapCardReserve               = sysLibTrapCustom + 12;
  SlotTrapCardRelease               = sysLibTrapCustom + 13;
  SlotTrapCardGetSerialPort         = sysLibTrapCustom + 14;

(********************************************************************
 * API Prototypes
 ********************************************************************)

(********************************************************************
 * Standard library open, close, sleep and wake functions
 ********************************************************************)

function SlotOpen(slotLibRefNum: UInt16): Err; syscall sysLibTrapOpen;

function SlotClose(slotLibRefNum: UInt16): Err; syscall sysLibTrapClose;

function SlotSleep(slotLibRefNum: UInt16): Err; syscall sysLibTrapSleep;

function SlotWake(slotLibRefNum: UInt16): Err; syscall sysLibTrapWake;

(********************************************************************
 * Custom library API functions
 ********************************************************************)

function SlotLibAPIVersion(slotLibRefNum: UInt16):UInt32; syscall SlotTrapLibAPIVersion;

function SlotCustomControl(slotLibRefNum: UInt16; apiCreator: UInt32; apiSelector: UInt16; valueP: Pointer; var valueLenP: UInt16): Err; syscall SlotTrapCustomControl;

function SlotCardPresent(slotLibRefNum: UInt16; slotRefNum: UInt16): Err; syscall SlotTrapCardPresent;

function SlotCardInfo(slotLibRefNum, slotRefNum: UInt16; var infoP: ExpCardInfoType): Err; syscall SlotTrapCardInfo;

function SlotCardMediaType(slotLibRefNum, slotRefNum: UInt16; var mediaTypeP: UInt32): Err; syscall SlotTrapCardMediaType;

function SlotCardReserve(slotLibRefNum, slotRefNum: UInt16): Err; syscall SlotTrapCardReserve;

function SlotCardRelease(slotLibRefNum, slotRefNum: UInt16): Err; syscall SlotTrapCardRelease;

function SlotCardGetSerialPort(slotLibRefNum, slotRefNum: UInt16; var portP: UInt32): Err; syscall SlotTrapCardGetSerialPort;

(********************************************************************
 * SlotDriver Formatting APIs:
 ********************************************************************)

function SlotCardIsFilesystemSupported(slotLibRefNum, slotRefNum: UInt16; filesystemType: UInt32): Boolean; syscall SlotTrapCardIsFilesystemSupported;

function SlotCardMetrics(slotLibRefNum, slotRefNum: UInt16; cardMetricsP: CardMetricsPtr): Err; syscall SlotTrapCardMetrics;

function SlotCardLowLevelFormat(slotLibRefNum, slotRefNum: UInt16): Err; syscall SlotTrapCardLowLevelFormat;

(********************************************************************
 * SlotDriver Logical Block Read/Write APIs:
 ********************************************************************)

function SlotCardSectorRead(slotLibRefNum, slotRefNum: UInt16; sectorNumber: UInt32; var bufferP: UInt8; var numSectorsP: UInt32): Err; syscall SlotTrapCardSectorRead;

function SlotCardSectorWrite(slotLibRefNum, slotRefNum: UInt16; sectorNumber: UInt32; var bufferP: UInt8; var numSectorsP: UInt32): Err; syscall SlotTrapCardSectorWrite;

(********************************************************************
 * Power Mgmt APIs:
 ********************************************************************)

const
  slotLibPowerFlag_WakeUp      = $0001; // Add the power required to bring the slot hardware out of low-power mode
  slotLibPowerFlag_FormatMedia = $0002; // Add the power required to perform a low-level format of the card media

function SlotPowerCheck(slotLibRefNum, slotRefNum, operationFlags, readBlocks, writeBlocks: UInt16): Err; syscall SlotTrapPowerCheck;

implementation

end.

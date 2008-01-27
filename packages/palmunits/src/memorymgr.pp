{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: MemoryMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Memory Manager
 *
 * History:
 *    10/25/94 RM    Created by Ron Marianetti
 *    10/28/99 kwk   Added memErrROMOnlyCard.
 *
 *****************************************************************************)

unit memorymgr;

interface

uses palmos, coretraps, errorbase;

(************************************************************
 * Memory Manager Types
 *************************************************************)

type
  LocalIDKind = Enum;

const
  memIDPtr = 0;
  memIDHandle = Succ(memIDPtr);

(************************************************************
 * Flags accepted as parameter for MemNewChunk.
 *************************************************************)

  memNewChunkFlagPreLock    = $0100;
  memNewChunkFlagNonMovable = $0200;
  memNewChunkFlagAtStart    = $0400; // force allocation at front of heap
  memNewChunkFlagAtEnd      = $0800; // force allocation at end of heap

(************************************************************
 * Memory Manager Debug settings for the MemSetDebugMode function
 *************************************************************)

  memDebugModeCheckOnChange        = $0001;
  memDebugModeCheckOnAll           = $0002;
  memDebugModeScrambleOnChange     = $0004;
  memDebugModeScrambleOnAll        = $0008;
  memDebugModeFillFree             = $0010;
  memDebugModeAllHeaps             = $0020;
  memDebugModeRecordMinDynHeapFree = $0040;

(************************************************************
 * Memory Manager result codes
 *************************************************************)

  memErrChunkLocked        = memErrorClass or 1;
  memErrNotEnoughSpace     = memErrorClass or 2;
  memErrInvalidParam       = memErrorClass or 3; // invalid param or requested size is too big
  memErrChunkNotLocked     = memErrorClass or 4;
  memErrCardNotPresent     = memErrorClass or 5;
  memErrNoCardHeader       = memErrorClass or 6;
  memErrInvalidStoreHeader = memErrorClass or 7;
  memErrRAMOnlyCard        = memErrorClass or 8;
  memErrWriteProtect       = memErrorClass or 9;
  memErrNoRAMOnCard        = memErrorClass or 10;
  memErrNoStore            = memErrorClass or 11;
  memErrROMOnlyCard        = memErrorClass or 12;

(********************************************************************
 * Memory Manager Routines
 * These are define as syscall calls only under emulation mode or
 *  under native mode from the module that actually installs the trap
 *  vectors
 ********************************************************************)

//-------------------------------------------------------------------
// Initialization
//-------------------------------------------------------------------

function MemInit: Err; syscall sysTrapMemInit;

function MemKernelInit: Err; syscall sysTrapMemKernelInit;

function MemInitHeapTable(cardNo: UInt16): Err; syscall sysTrapMemInitHeapTable;

//-------------------------------------------------------------------
// Card formatting and Info
//-------------------------------------------------------------------

function MemNumCards: UInt16; syscall sysTrapMemNumCards;

function MemCardFormat(cardNo: UInt16; const cardNameP, manufNameP, ramStoreNameP: PChar): Err; syscall sysTrapMemCardFormat;

function MemCardInfo(cardNo: UInt16; cardNameP, manufNameP: PChar; var versionP: UInt16;
                     var crDateP, romSizeP, ramSizeP, freeBytesP: UInt32): Err; syscall sysTrapMemCardInfo;

//-------------------------------------------------------------------
// Store Info
//-------------------------------------------------------------------

function MemStoreInfo(cardNo, storeNumber: UInt16; var versionP, flagsP: UInt16;
                      nameP: PChar; var crDateP, bckUpDateP, heapListOffsetP, initCodeOffset1P,
                      initCodeOffset2P: UInt32; var databaseDirIDP: LocalID): Err; syscall sysTrapMemStoreInfo;

function MemStoreSetInfo(cardNo, storeNumber: UInt16; var versionP, flagsP: UInt16;
                      nameP: PChar; var crDateP, bckUpDateP, heapListOffsetP, initCodeOffset1P,
                      initCodeOffset2P: UInt32; var databaseDirIDP: LocalID): Err; syscall sysTrapMemStoreSetInfo;

//-------------------------------------------------------------------
// Heap Info & Utilities
//-------------------------------------------------------------------

function MemNumHeaps(cardNo: UInt16): UInt16; syscall sysTrapMemNumHeaps;

function MemNumRAMHeaps(cardNo: UInt16): UInt16; syscall sysTrapMemNumRAMHeaps;

function MemHeapID(cardNo, heapIndex: UInt16): UInt16; syscall sysTrapMemHeapID;

function MemHeapDynamic(heapID: UInt16): Boolean; syscall sysTrapMemHeapDynamic;

function MemHeapFreeBytes(heapID: UInt16; var freeP, maxP: UInt32): Err; syscall sysTrapMemHeapFreeBytes;

function MemHeapSize(heapID: UInt16): UInt32; syscall sysTrapMemHeapSize;

function MemHeapFlags(heapID: UInt16): UInt16; syscall sysTrapMemHeapFlags;

// Heap utilities
function MemHeapCompact(heapID: UInt16): Err; syscall sysTrapMemHeapCompact;

function MemHeapInit(heapID: UInt16; numHandles: Int16; initContents: Boolean): Err; syscall sysTrapMemHeapInit;

function MemHeapFreeByOwnerID(heapID, ownerID: UInt16): Err; syscall sysTrapMemHeapFreeByOwnerID;

//-------------------------------------------------------------------
// Low Level Allocation
//-------------------------------------------------------------------

function MemChunkNew(heapID: UInt16; size: UInt32; attr: UInt16): MemPtr; syscall sysTrapMemChunkNew;

function MemChunkFree(chunkDataP: MemPtr): Err; syscall sysTrapMemChunkFree;

//-------------------------------------------------------------------
// Pointer (Non-Movable) based Chunk Routines
//-------------------------------------------------------------------

function MemPtrNew(size: UInt32): MemPtr; syscall sysTrapMemPtrNew;

function MemPtrFree(chunkDataP: MemPtr): Err; syscall sysTrapMemChunkFree;

// Getting Attributes
function MemPtrRecoverHandle(p: MemPtr): MemHandle; syscall sysTrapMemPtrRecoverHandle;

function MemPtrFlags(p: MemPtr): UInt16; syscall sysTrapMemPtrFlags;

function MemPtrSize(p: MemPtr): UInt32; syscall sysTrapMemPtrSize;

function MemPtrOwner(p: MemPtr): UInt16; syscall sysTrapMemPtrOwner;

function MemPtrHeapID(p: MemPtr): UInt16; syscall sysTrapMemPtrHeapID;

function MemPtrDataStorage(p: MemPtr): Boolean; syscall sysTrapMemPtrDataStorage;

function MemPtrCardNo(p: MemPtr): UInt16; syscall sysTrapMemPtrCardNo;

function MemPtrToLocalID(p: MemPtr): LocalID; syscall sysTrapMemPtrToLocalID;

// Setting Attributes
function MemPtrSetOwner(p: MemPtr; owner: UInt16): Err; syscall sysTrapMemPtrSetOwner;

function MemPtrResize(p: MemPtr; newSize: UInt32): Err; syscall sysTrapMemPtrResize;

function MemPtrResetLock(p: MemPtr): Err; syscall sysTrapMemPtrResetLock;

function MemPtrUnlock(p: MemPtr): Err; syscall sysTrapMemPtrUnlock;

//-------------------------------------------------------------------
// MemHandle (Movable) based Chunk Routines
//-------------------------------------------------------------------

function MemHandleNew(size: UInt32): MemHandle; syscall sysTrapMemHandleNew;

function MemHandleFree(h: MemHandle): Err; syscall sysTrapMemHandleFree;

// Getting Attributes
function MemHandleFlags(h: MemHandle): UInt16; syscall sysTrapMemHandleFlags;

function MemHandleSize(h: MemHandle): UInt32; syscall sysTrapMemHandleSize;

function MemHandleOwner(h: MemHandle): UInt16; syscall sysTrapMemHandleOwner;

function MemHandleLockCount(h: MemHandle): UInt16; syscall sysTrapMemHandleLockCount;

function MemHandleHeapID(h: MemHandle): UInt16; syscall sysTrapMemHandleHeapID;

function MemHandleDataStorage(h: MemHandle): Boolean; syscall sysTrapMemHandleDataStorage;

function MemHandleCardNo(h: MemHandle): UInt16; syscall sysTrapMemHandleCardNo;

function MemHandleToLocalID(h: MemHandle): LocalID; syscall sysTrapMemHandleToLocalID;

// Setting Attributes
function MemHandleSetOwner(h: MemHandle; owner: UInt16): Err; syscall sysTrapMemHandleSetOwner;

function MemHandleResize(h: MemHandle; newSize: UInt32): Err; syscall sysTrapMemHandleResize;

function MemHandleLock(h: MemHandle): MemPtr; syscall sysTrapMemHandleLock;

function MemHandleUnlock(h: MemHandle): Err; syscall sysTrapMemHandleUnlock;

function MemHandleResetLock(h: MemHandle): Err; syscall sysTrapMemHandleResetLock;

//-------------------------------------------------------------------
// Local ID based routines
//-------------------------------------------------------------------

function MemLocalIDToGlobal(local: LocalID; cardNo: UInt16): MemPtr; syscall sysTrapMemLocalIDToGlobal;

function MemLocalIDKind(local: LocalID): LocalIDKind; syscall sysTrapMemLocalIDKind;

function MemLocalIDToPtr(local: LocalID; cardNo: UInt16): MemPtr; syscall sysTrapMemLocalIDToPtr;

function MemLocalIDToLockedPtr(local: LocalID; cardNo: UInt16): MemPtr; syscall sysTrapMemLocalIDToLockedPtr;

//-------------------------------------------------------------------
// Utilities
//-------------------------------------------------------------------

function MemMove(dstP: Pointer; const sP: Pointer; numBytes: Int32): Err; syscall sysTrapMemMove;

function MemSet(dstP: Pointer; numBytes: Int32; value: UInt8): Err; syscall sysTrapMemSet;

function MemCmp(const s1: Pointer; const s2: Pointer; numBytes: Int32): Int16; syscall sysTrapMemCmp;

function MemSemaphoreReserve(writeAccess: Boolean): Err; syscall sysTrapMemSemaphoreReserve;

function MemSemaphoreRelease(writeAccess: Boolean): Err; syscall sysTrapMemSemaphoreRelease;

//-------------------------------------------------------------------
// Debugging Support
//-------------------------------------------------------------------

function MemDebugMode: UInt16; syscall sysTrapMemDebugMode;

function MemSetDebugMode(flags: UInt16): Err; syscall sysTrapMemSetDebugMode;

function MemHeapScramble(heapID: UInt16): Err; syscall sysTrapMemHeapScramble;

function MemHeapCheck(heapID: UInt16): Err; syscall sysTrapMemHeapCheck;

implementation

end.

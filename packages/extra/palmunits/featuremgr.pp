(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: FeatureMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header for the Feature Manager
 *
 * History:
 *    11/14/94  RM - Created by Ron Marianetti
 *
 *****************************************************************************)

unit featuremgr;

interface

uses palmos, coretraps, errorbase;

(************************************************************
 * Feature manager error codes
 * the constant ftrErrorClass is defined in ErrorBase.h
 *************************************************************)

const
  ftrErrInvalidParam  = ftrErrorClass or 1;
  ftrErrNoSuchFeature = ftrErrorClass or 2;
  ftrErrAlreadyExists = ftrErrorClass or 3;
  ftrErrROMBased      = ftrErrorClass or 4;
  ftrErrInternalErr   = ftrErrorClass or 5;

(************************************************************
 * Feature Manager procedures
 *************************************************************)

// Init the feature Manager
function FtrInit: Err; syscall sysTrapFtrInit;

// Get a feature
function FtrGet(creator: UInt32; featureNum: UInt16; var valueP: UInt32): Err; syscall sysTrapFtrGet;

// Set/Create a feature.
function FtrSet(creator: UInt32; featureNum: UInt16; newValue: UInt32): Err; syscall sysTrapFtrSet;

// Unregister a feature
function FtrUnregister(creator: UInt32; featureNum: UInt16): Err; syscall sysTrapFtrUnregister;

// Get a feature by index
function FtrGetByIndex(index: UInt16; romTable: Boolean; var creatorP: UInt32; var numP: UInt16; var valueP: UInt32): Err; syscall sysTrapFtrGetByIndex;

// Get temporary space from storage heap
function FtrPtrNew(creator: UInt32; featureNum: UInt16; size: UInt32; var newPtrP: Pointer): Err; syscall sysTrapFtrPtrNew;

// Release temporary space to storage heap
function FtrPtrFree(creator: UInt32; featureNum: UInt16): Err; syscall sysTrapFtrPtrFree;

// Resize block of temporary storage
function FtrPtrResize(creator: UInt32; featureNum: UInt16; newSize: UInt32; var newPtrP: Pointer): Err; syscall sysTrapFtrPtrResize;

implementation

end.

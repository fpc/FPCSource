{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SysUtils.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   These are miscellaneous routines.
 *
 * History:
 *    April 27, 1995 Created by Roger Flores
 *
 *****************************************************************************)

unit sysutil;

interface

uses palmos, coretraps;

//typedef Int16 _comparF (const void *, const void *, Int16 other);
type
  _comparF = function(p1, p2: Pointer; other: Int32): Int16;
  CmpFuncPtr = _comparF;


  _searchF = function(const searchData, arrayData: Pointer; other: Int32): Int16;
  SearchFuncPtr = _searchF;

// For backwards compatibility
//const
//  GremlinIsOn = hostSelectorGremlinIsRunning;

(************************************************************
 * Constants
 *************************************************************)

const
  sysRandomMax = $7FFF; // Max value returned from SysRandom()

(************************************************************
 * Macros
 *************************************************************)

// Abs(a) (((a) >= 0) ? (a) : -(a))

(************************************************************
 * procedures
 *************************************************************)

function SysBinarySearch(const baseP: Pointer; numOfElements, width: Int16;
                         searchF: SearchFuncPtr; const searchData: Pointer;
                         other: Int32; var position: Int32; findFirst: Boolean): Boolean; syscall sysTrapSysBinarySearch;

procedure SysInsertionSort(baseP: Pointer; numOfElements, width: Int16;
                           comparF: CmpFuncPtr; other: Int32); syscall sysTrapSysInsertionSort;

procedure SysQSort(baseP: Pointer; numOfElements, width: Int16;
                   comparF: CmpFuncPtr; other: Int32); syscall sysTrapSysQSort;

procedure SysCopyStringResource(string_: PChar; theID: Int16); syscall sysTrapSysCopyStringResource;

function SysFormPointerArrayToStrings(c: PChar; stringCount: Int16): MemHandle; syscall sysTrapSysFormPointerArrayToStrings;


// Return a random number ranging from 0 to sysRandomMax.
// Normally, 0 is passed unless you want to start with a new seed.
function SysRandom(newSeed: Int32): Int16; syscall sysTrapSysRandom;


function SysStringByIndex(resID, index: UInt16; strP: PChar; maxLen: UInt16): PChar; syscall sysTrapSysStringByIndex;

function SysErrString(err: Err; strP: PChar; maxLen: UInt16): PChar; syscall sysTrapSysErrString;

// This function is not to be called directly.  Instead, use the various Emu* calls
// in EmuTraps.h because they work for Poser, the device, and the simulator, and
// they are safer because of the type checking.
//!!!function HostControl(selector: HostControlTrapNumber, ...): UInt32; syscall sysTrapHostControl;

// For backwards compatibility
//const
//  SysGremlins = HostControl;

implementation

end.

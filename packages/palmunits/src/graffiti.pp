{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Graffiti.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header for the Graffiti interface
 *
 * History:
 *    6/30  RM - Created by Ron Marianetti
 *
 *****************************************************************************)

unit graffiti;

interface

uses palmos, coretraps, rect, errorbase;

(*------------------------------------------------------------------------------
 * Match info structure. Returned by GrfMatch and GrfMatchGlyph
 *-----------------------------------------------------------------------------*)

type
  GrfMatchType = record
    glyphID: UInt8;     // glyph ID of this match
    unCertainty: UInt8; // unCertainty of this match (0 most certain)
  end;

const
  grfMaxMatches = 4;

type
  GrfMatchInfoType = record
    numMatches: UInt16; // number of matches returned in this structure
    match: array [0..grfMaxMatches-1] of GrfMatchType;
  end;

  GrfMatchInfoPtr = ^GrfMatchInfoType;

//----------------------------------------------------------------------------
// Escape codes preceding special sequences in the dictionary or macros
//----------------------------------------------------------------------------
// In dictionary or macros preceding virtual key event sequences. These are always
// 13 byte sequences that have ASCII encoded values for the ascii code, keyCode,
//   and modifiers:
//   grfVirtualSequence, ascii,   keyCode,  modifiers.
//         1 byte        4 bytes   4 bytes   4 bytes

const
  grfVirtualSequence     = $01;

// In dictionary to tell us about temp shift state changes.
  grfShiftSequence       = $02;

// In dictionary/macros to hide special features
  grfSpecialSequence     = $03;


// Determine if a string has a sequence
{
  HasVirtualSequence(s)       (s[0] == grfVirtualSequence)
  HasSpecialSequence(s)       (s[0] == grfSpecialSequence)
}

(*------------------------------------------------------------------------------
 * Temp shift states, returned by GrfGetState
 *-----------------------------------------------------------------------------*)

  grfTempShiftPunctuation = 1;
  grfTempShiftExtended    = 2;
  grfTempShiftUpper       = 3;
  grfTempShiftLower       = 4;

(*------------------------------------------------------------------------------
 * Macro (aka Shortcut) related constants/macros
 * Use the definitions in ShortcutLib.h instead!
 *-----------------------------------------------------------------------------*)

  // Char indicating a seqeunce of characters to expand.
  grfExpansionSequence   = '@';

  // Chars indicating what to expand into
  expandDateChar         = 'D';
  expandTimeChar         = 'T';
  expandStampChar        = 'S'; //  This follows 'D' or 'T' for the sake
                                //  of the mnemonic name.
  // max shortcut name length
  grfNameLength = 8; // eight letters possible (don't forget CR)

  // index which is not a shortcut
  grfNoShortCut = $ffff;

(************************************************************
 * Graffiti result codes
 *************************************************************)

  grfErrBadParam          = grfErrorClass or 1;
  grfErrPointBufferFull   = grfErrorClass or 2;
  grfErrNoGlyphTable      = grfErrorClass or 3;
  grfErrNoDictionary      = grfErrorClass or 4;
  grfErrNoMapping         = grfErrorClass or 5;
  grfErrMacroNotFound     = grfErrorClass or 6;
  grfErrDepthTooDeep      = grfErrorClass or 7;
  grfErrMacroPtrTooSmall  = grfErrorClass or 8;
  grfErrNoMacros          = grfErrorClass or 9;

  grfErrMacroIncomplete   = grfErrorClass or 129; // (grfWarningOffset+1)
  grfErrBranchNotFound    = grfErrorClass or 130; // (grfWarningOffset+2)
  grfErrGenericHWRErrBase = grfErrorClass or 16;
  grfErrNoHWRInstalled    = grfErrGenericHWRErrBase;

(************************************************************
 * Graffiti interface procedures
 *************************************************************)

//-----------------------------------------------------------------
// High Level Calls
//------------------------------------------------------------------

function GrfInit: Err; syscall sysTrapGrfInit;

function GrfFree: Err; syscall sysTrapGrfFree;

function GrfProcessStroke(var startPtP, endPtP: PointType; upShift: Boolean): Err; syscall sysTrapGrfProcessStroke;

function GrfFieldChange(resetState: Boolean; var characterToDelete: UInt16): Err; syscall sysTrapGrfFieldChange;

function GrfGetState(var capsLockP, numLockP: Boolean; var tempShiftP: UInt16; var autoShiftedP: Boolean): Err; syscall sysTrapGrfGetState;

function GrfSetState(capsLock, numLock, upperShift: Boolean): Err; syscall sysTrapGrfSetState;

//-----------------------------------------------------------------
// Mid Level Calls
//------------------------------------------------------------------

function GrfFlushPoints: Err; syscall sysTrapGrfFlushPoints;

function GrfAddPoint(var pt: PointType): Err; syscall sysTrapGrfAddPoint;

function GrfInitState: Err; syscall sysTrapGrfInitState;

function GrfCleanState: Err; syscall sysTrapGrfCleanState;

function GrfMatch(var flagsP: UInt16; dataPtrP: Pointer; var dataLenP, uncertainLenP: UInt16;
                  matchInfoP: GrfMatchInfoPtr): Err; syscall sysTrapGrfMatch;

function GrfGetMacro(nameP: PChar; var macroDataP: UInt8; var dataLenP: UInt16): Err; syscall sysTrapGrfGetMacro;

function GrfGetAndExpandMacro(nameP: PChar; var macroDataP: UInt8; var dataLenP: UInt16): Err; syscall sysTrapGrfGetAndExpandMacro;

//-----------------------------------------------------------------
// Low Level Calls
//------------------------------------------------------------------

function GrfFilterPoints: Err; syscall sysTrapGrfFilterPoints;

function GrfGetNumPoints(var numPtsP: UInt16): Err; syscall sysTrapGrfGetNumPoints;

function GrfGetPoint(index: UInt16; var pointP: PointType): Err; syscall sysTrapGrfGetPoint;

function GrfFindBranch(flags: UInt16): Err; syscall sysTrapGrfFindBranch;

function GrfMatchGlyph(matchInfoP: GrfMatchInfoPtr; maxUnCertainty: Int16; maxMatches: UInt16): Err; syscall sysTrapGrfMatchGlyph;

function GrfGetGlyphMapping(glyphID: UInt16; var flagsP: UInt16; dataPtrP: Pointer;
                            var dataLenP, uncertainLenP: UInt16): Err; syscall sysTrapGrfGetGlyphMapping;

function GrfGetMacroName(index: UInt16; nameP: PChar): Err; syscall sysTrapGrfGetMacroName;

function GrfDeleteMacro(index: UInt16): Err; syscall sysTrapGrfDeleteMacro;

function GrfAddMacro(const nameP: PChar; var macroDataP: UInt8; dataLen: UInt16): Err; syscall sysTrapGrfAddMacro;

implementation

end.

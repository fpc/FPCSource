{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1999-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: OverlayMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Public header for routines that support overlays.
 *
 * History:
 *    06/24/99 kwk   Created by Ken Krugler.
 *    07/06/99 CS    Added omSpecAttrForBase
 *                   (and renumbered omSpecAttrStripped).
 *    07/29/99 CS    Added omOverlayKindBase for the entries in the base
 *                   DBs 'ovly' resource (they had been set to
 *                   omOverlayKindReplace before).
 *    07/29/99 CS    Bumped version to 3, since now we're supposed to
 *                   support omOverlayKindAdd.
 *    09/29/99 kwk   Bumped version to 4, since we added the baseChecksum
 *                   field to OmOverlaySpecType, as a way of speeding up
 *                   overlay validation.
 *    09/29/99 CS    Actually bumped version to 4, which Ken forgot.
 *    10/08/99 kwk   Added OmGetRoutineAddress selector/declaration.
 *                   Moved OmDispatch, OmInit, and OmOpenOverlayDatabase
 *                   into OverlayPrv.h
 *                   ===== Palm OS 3.5 Released =====
 *    03/12/00 kwk   Fixed comment for omOverlayKindHide.
 *    05/18/00 kwk   Added omFtrDefaultLocale feature selector.
 *                   Added OmGetNextSystemLocale routine. Changed OmLocaleType
 *                   to LmLocaleType. Deleted omOverlayDBType (use sysFileTOverlay).
 *                   Moved more private stuff into OverlayPrv.h.
 *    07/29/00 kwk   Added note about using OmGetNextSystemLocale vs OmGetIndexedLocale.
 *    09/14/00 kwk   Added didNoOverlaySystem & foundSystem to OmSearchStateType,
 *                   plus 6 more reserved bytes for future expansion.
 *    09/18/00 kwk   Added omErrNoNextSystemLocale.
 *
 *****************************************************************************)

unit overlaymgr;

interface

uses palmos, coretraps, errorbase, datamgr, localemgr;

(***********************************************************************
 * Overlay Manager constants
 **********************************************************************)

const
  omOverlayRscType_        = Rsc('ovly'); // Overlay desc resource type
  omOverlayRscID           = 1000;  // Overlay desc resource ID

  omFtrCreator             = Rsc('ovly'); // For get/set of Overlay features.
  omFtrShowErrorsFlag      = 0;     // Boolean - True => display overlay errors.
  omFtrDefaultLocale       = 1;     // LmLocaleType record => default locale to
                                    // try with stripped bases & no valid overlay.

// OmFindOverlayDatabase called with stripped base, and no appropriate overlay was found.
  omErrBaseRequiresOverlay = omErrorClass or 1;

// OmOverlayDBNameToLocale or OmLocaleToOverlayDBName were passed an unknown locale.
  mErrUnknownLocale        = omErrorClass or 2;

// OmOverlayDBNameToLocale was passed a poorly formed string.
  mErrBadOverlayDBName     = omErrorClass or 3;

// OmGetIndexedLocale was passed an invalid index.
  mErrInvalidLocaleIndex   = omErrorClass or 4;

// OmSetSystemLocale was passed an invalid locale (doesn't correspond to available
// system overlay).

  mErrInvalidLocale        = omErrorClass or 5;

// OmSetSystemLocale was passed a locale that referenced an invalid system overlay
// (missing one or more required resources)

  mErrInvalidSystemOverlay = omErrorClass or 6;

// OmGetNextSystemLocale was called, but there were no more valid system
// locales to return.
  omErrNoNextSystemLocale  = omErrorClass or 7;

(***********************************************************************
 * Selectors & macros used for calling Overlay Manager routines
 **********************************************************************)

// Selectors used for getting to the right Overlay Manager routine via
// the OmDispatch trap.

type
  OmSelector = WordEnum;

const
  omInit = 0;
  omOpenOverlayDatabase = Succ(omInit);
  omLocaleToOverlayDBName_ = Succ(omOpenOverlayDatabase);
  omOverlayDBNameToLocale_ = Succ(omLocaleToOverlayDBName_);
  omGetCurrentLocale_ = Succ(omOverlayDBNameToLocale_);
  omGetIndexedLocale_ = Succ(omGetCurrentLocale_);
  omGetSystemLocale_ = Succ(omGetIndexedLocale_);
  omSetSystemLocale_ = Succ(omGetSystemLocale_);
  omGetRoutineAddress_ = Succ(omSetSystemLocale_);
  omGetNextSystemLocale_ = Succ(omGetRoutineAddress_);

  omMaxSelector = omGetNextSystemLocale_;

(***********************************************************************
 * Overlay Manager types
 **********************************************************************)

// DOLATER kwk - decide how to deprecate this.
type
  OmLocaleType = LmLocaleType;

// Structure passed to OmGetNextSystemLocale.
type
  OmSearchStateType = record
    searchState: DmSearchStateType;
    systemDBRef: DmOpenRef;
    systemDBCard: UInt16;
    systemDBName: array [0..dmDBNameLength-1] of Char;
    systemDBNameLen: Int16;
    curLocale: LmLocaleType;
    didNoOverlaySystem: Boolean;
    foundSystem: Boolean;
    reserved: array [0..5] of UInt8;
  end;

(***********************************************************************
 * Overlay Manager routines
 **********************************************************************)

// Return in <overlayDBName> an overlay database name that's appropriate
// for the base name <baseDBName> and the locale <targetLocale>. If the
// <targetLocale> param in NULL, use the current locale. The <overlayDBName>
// buffer must be at least dmDBNameLength bytes.

function OmLocaleToOverlayDBName(const baseDBName: PChar; {const} var targetLocale: LmLocaleType;
                                 overlayDBName: PChar): Err;

// Given the name of an overlay database in <overlayDBName>, return back
// the overlay in overlayLocale. If the name isn't an overlay name,
// return omErrBadOverlayDBName.

function OmOverlayDBNameToLocale(const overlayDBName: PChar; var overlayLocale: LmLocaleType): Err;

// Return the current locale in <currentLocale>. This may not be the same as
// the system locale, which will take effect after the next reset.

procedure OmGetCurrentLocale(var currentLocale: LmLocaleType);

// Return the nth valid system locale in <theLocale>. Indexes are zero-based,
// and omErrInvalidLocaleIndex will be returned if <localeIndex> is out of
// bounds. Note that OmGetNextSystemLocale should be used on Palm OS 4.0 or
// later, since OmGetIndexedLocale can be slow on ROMs with more than few
// valid system locales.

function OmGetIndexedLocale(localeIndex: UInt16; var theLocale: LmLocaleType): Err;

// Return the system locale in <systemLocale>. This may not be the same as
// the current locale. WARNING!!! This routine should only be used in very
// special situations; typically OmGetCurrentLocale should be used to determine
// the "active" locale.

procedure OmGetSystemLocale(var systemLocale: LmLocaleType);

//  Set the post-reset system locale to be <systemLocale>. Return omErrInvalidLocale if
// the passed locale doesnմ correspond to a valid System.prc overlay.

function OmSetSystemLocale({const} var systemLocale: LmLocaleType): Err;

// Return back the address of the routine indicated by <inSelector>. If
// <inSelector> isn't a valid routine selector, return back NULL.

function OmGetRoutineAddress(inSelector: OmSelector): Pointer;

// NEW in 4.0. Return back the next valid system locale in <oLocaleP>. The first
// time the routine is called, <iNewSearch> must be true. When there are no more
// valid system locales, omErrInvalidLocaleIndex will be returned. This routine
// should be used in place of OmGetIndexedLocale on Palm OS 4.0 or later, since
// it's much faster.

function OmGetNextSystemLocale(iNewSearch: Boolean; var ioStateInfoP: OmSearchStateType; var oLocaleP: LmLocaleType): Err;

implementation

function __OmLocaleToOverlayDBName(const baseDBName: PChar; {const} var targetLocale: LmLocaleType;
                                 overlayDBName: PChar): Err; syscall sysTrapOmDispatch;
function __OmOverlayDBNameToLocale(const overlayDBName: PChar; var overlayLocale: LmLocaleType): Err; syscall sysTrapOmDispatch;
procedure __OmGetCurrentLocale(var currentLocale: LmLocaleType); syscall sysTrapOmDispatch;
function __OmGetIndexedLocale(localeIndex: UInt16; var theLocale: LmLocaleType): Err; syscall sysTrapOmDispatch;
procedure __OmGetSystemLocale(var systemLocale: LmLocaleType); syscall sysTrapOmDispatch;
function __OmSetSystemLocale({const} var systemLocale: LmLocaleType): Err; syscall sysTrapOmDispatch;
function __OmGetRoutineAddress(inSelector: OmSelector): Pointer; syscall sysTrapOmDispatch;
function __OmGetNextSystemLocale(iNewSearch: Boolean; var ioStateInfoP: OmSearchStateType; var oLocaleP: LmLocaleType): Err; syscall sysTrapOmDispatch;

function OmLocaleToOverlayDBName(const baseDBName: PChar; var targetLocale: LmLocaleType;
                                 overlayDBName: PChar): Err;
begin
 asm
  move.l #$omLocaleToOverlayDBName_,D2;
 end;
 OmLocaleToOverlayDBName := __OmLocaleToOverlayDBName(baseDBName, targetLocale, overlayDBName);
end;

function OmOverlayDBNameToLocale(const overlayDBName: PChar; var overlayLocale: LmLocaleType): Err;
begin
 asm
  move.l #$omOverlayDBNameToLocale_,D2;
 end;
 OmOverlayDBNameToLocale := __OmOverlayDBNameToLocale(overlayDBName, overlayLocale);
end;

procedure OmGetCurrentLocale(var currentLocale: LmLocaleType);
begin
 asm
  move.l #$omGetCurrentLocale_,D2;
 end;
 __OmGetCurrentLocale(currentLocale);
end;

function OmGetIndexedLocale(localeIndex: UInt16; var theLocale: LmLocaleType): Err;
begin
 asm
  move.l #$omGetIndexedLocale_,D2;
 end;
 OmGetIndexedLocale := __OmGetIndexedLocale(localeIndex, theLocale);
end;

procedure OmGetSystemLocale(var systemLocale: LmLocaleType);
begin
 asm
  move.l #$omGetSystemLocale_,D2;
 end;
 __OmGetSystemLocale(systemLocale);
end;

function OmSetSystemLocale(var systemLocale: LmLocaleType): Err;
begin
 asm
  move.l #$omSetSystemLocale_,D2;
 end;
 OmSetSystemLocale := __OmSetSystemLocale(systemLocale);
end;

function OmGetRoutineAddress(inSelector: OmSelector): Pointer;
begin
 asm
  move.l #$omGetRoutineAddress_,D2;
 end;
 OmGetRoutineAddress := __OmGetRoutineAddress(inSelector);
end;

function OmGetNextSystemLocale(iNewSearch: Boolean; var ioStateInfoP: OmSearchStateType; var oLocaleP: LmLocaleType): Err;
begin
 asm
  move.l #$omGetNextSystemLocale_,D2;
 end;
 OmGetNextSystemLocale := __OmGetNextSystemLocale(iNewSearch, ioStateInfoP, oLocaleP);
end;


end.

(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: LocaleMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Public header for routines that support locales (information specific
 *       to locales and regions).
 *
 * History:
 * 04/28/00 CS    Created by Chris Schneider.
 * 05/16/00 CS    LmCountryType/LmLanguageType are now back to
 *                CountryType/LanguageType.
 * 05/31/00 CS    Moved country and language codes to new Locale.h and removed
 *                kLanguageFirst, etc.
 * 06/06/00 kwk   Made _LmLocaleType's language & country fields be UInt16's,
 *                so that it's binary compatible with OmLocaleType.
 * 07/28/00 CS    Replaced lmChoiceMinutesWestOfGMT & lmChoiceDaylightSavings
 *                selectors with lmChoiceTimeZone.
 * 08/08/00 CS    Renamed LmGetLocaleSetting's <iMaxSize> parameter <iValueSize>
 *                to reflect the fact that the routine now checks to make sure
 *                that <oValue> is the correct size for all fixed-size settings.
 * 09/29/00 CS    Made iLocale parm of LmLocaleToIndex const.
 * 11/17/00 CS    Removed support for lmChoiceLanguage & lmChoiceCountry,
 *                since these guys were returning UInt8's, which probably
 *                won't cut it at some point in the future.  Callers can use
 *                lmChoiceLocale, which returns an LmLocaleType struct that
 *                places the country and language into UInt16 fields.
 *          CS    Defined lmAnyCountry & lmAnyLanguage, which can now be passed
 *                in LmLocaleToIndex's iLocale parameter (as wildcards).
 *
 *****************************************************************************)
unit localemgr;

interface

uses palmos, coretraps, errorbase;

// Supporting lmChoiceLanguageName would add over 3K to the 'locs' resource.
// DOLATER CS - either do it or punt.

const
  SUPPORT_LANGUAGE_NAME = 0;

(***********************************************************************
 * Locale Manager errors
 **********************************************************************)

// Locale not found in 'locs' resource.

const
  lmErrUnknownLocale = lmErrorClass or 1;

// Locale index >= LmGetNumLocales().

  lmErrBadLocaleIndex = lmErrorClass or 2;

// LmLocaleSettingChoice out of bounds.

  lmErrBadLocaleSettingChoice = lmErrorClass or 3;

// Data for locale setting too big for destination.

  lmErrSettingDataOverflow = lmErrorClass or 4;

(***********************************************************************
 * Locale Manager settings (pass to LmGetLocaleSetting)
 **********************************************************************)

type
  LmLocaleSettingChoice = UInt16;

// LmLocaleType

const
  lmChoiceLocale = LmLocaleSettingChoice(1);

// Char[kMaxLanguageNameLen+1] - Name of the language spoken there (localized)

const
  lmChoiceLanguageName = LmLocaleSettingChoice(4);

// Char[kMaxCountryNameLen+1] - Name of the country (localized)

  lmChoiceCountryName = LmLocaleSettingChoice(5);

// DateFormatType

  lmChoiceDateFormat = LmLocaleSettingChoice(6);

// DateFormatType

  lmChoiceLongDateFormat = LmLocaleSettingChoice(7);

// TimeFormatType

  lmChoiceTimeFormat = LmLocaleSettingChoice(8);

// UInt16 - Weekday for calendar column 1 (sunday=0, monday=1, etc.)

  lmChoiceWeekStartDay = LmLocaleSettingChoice(9);

// Int16 - Default GMT offset minutes, + for east of GMT, - for west

  lmChoiceTimeZone = LmLocaleSettingChoice(10);

// NumberFormatType - Specifies decimal and thousands separator characters

  lmChoiceNumberFormat = LmLocaleSettingChoice(11);

// Char[kMaxCurrencyNameLen+1] - Name of local currency (e.g., "US Dollar")

  lmChoiceCurrencyName = LmLocaleSettingChoice(12);

// Char[kMaxCurrencySymbolLen+1] - Currency symbol (e.g., "$")

  lmChoiceCurrencySymbol = LmLocaleSettingChoice(13);

// Char[kMaxCurrencySymbolLen+1] - Unique currency symbol (e.g., "US$")

  lmChoiceUniqueCurrencySymbol = LmLocaleSettingChoice(14);

// UInt16 - Number of decimals for currency (e.g., 2 for $10.12)

  lmChoiceCurrencyDecimalPlaces = LmLocaleSettingChoice(15);

// MeasurementSystemType - Metric, English, etc.

  lmChoiceMeasurementSystem = LmLocaleSettingChoice(16);

(***********************************************************************
 * Locale Manager constants
 **********************************************************************)

const
  lmAnyCountry  = Word(65535); // Pass LmLocaleToIndex's iLocale
  lmAnyLanguage = Word(65535); // Pass LmLocaleToIndex's iLocale

const
  kMaxCountryNameLen = 19;
  kMaxLanguageNameLen = 19;
  kMaxCurrencyNameLen = 19;
  kMaxCurrencySymbolLen = 5;

(***********************************************************************
 * Selectors & macros used for calling Locale Manager routines
 **********************************************************************)

// Selectors used for getting to the right Locale Manager routine via
// the LmDispatch trap.

// DOLATER:jwm: remove me after fixing LocaleMgr.c:PrvSelectorError
type
  LmRoutineSelector = UInt16;

const
  lmInit              = 0;
  lmGetNumLocales_    = 1;
  lmLocaleToIndex_    = 2;
  lmGetLocaleSetting_ = 3;

const
  lmMaxRoutineSelector = lmGetLocaleSetting_;

(***********************************************************************
 * Locale Manager types
 **********************************************************************)

type
  LanguageType = UInt8;
  CountryType = UInt8;

  _LmLocaleType = record
    language: UInt16; // Language spoken in locale (LanguageType)
    country: UInt16;  // Specifies "dialect" of language (CountryType)
  end;
  LmLocaleType = _LmLocaleType;
  LmLocalePtr = ^LmLocaleType;

(***********************************************************************
 * Locale Manager routines
 **********************************************************************)

// Return the number of known locales (maximum locale index + 1).

function LmGetNumLocales: UInt16; // syscall sysTrapLmDispatch, lmGetNumLocales_;

// Convert <iLocale> to <oLocaleIndex> by locating it within the set of known
// locales.

function LmLocaleToIndex({const} var iLocale: LmLocaleType; var oLocaleIndex: UInt16): Err; // syscall sysTrapLmDispatch, lmLocaleToIndex_;

// Return in <oValue> the setting identified by <iChoice> which is appropriate for
// the locale identified by <iLocaleIndex>.  Return lmErrSettingDataOverflow if the
// data for <iChoice> occupies more than <iValueSize> bytes.  Display a non-fatal
// error if <iValueSize> is larger than the data for a fixed-size setting.

function LmGetLocaleSetting(iLocaleIndex: UInt16; iChoice: LmLocaleSettingChoice;
                            oValue: Pointer; iValueSize: UInt16): Err; // syscall sysTrapLmDispatch, lmGetLocaleSetting_;

implementation

function __LmGetNumLocales: UInt16; syscall sysTrapLmDispatch;
function __LmLocaleToIndex(var iLocale: LmLocaleType; var oLocaleIndex: UInt16): Err; syscall SysTrapLmDispatch;
function __LmGetLocaleSetting(iLocaleIndex: UInt16; iChoice: LmLocaleSettingChoice;
                              oValue: Pointer; iValueSize: UInt16): Err;  syscall sysTrapLmDispatch;

function LmGetNumLocales: UInt16; // syscall sysTrapLmDispatch, lmGetNumLocales_;
begin
 asm
  move.l #$lmGetNumLocales_, d2;
 end;
 LmGetNumLocales := __LmGetNumLocales;
end;

function LmLocaleToIndex({const} var iLocale: LmLocaleType; var oLocaleIndex: UInt16): Err; // syscall sysTrapLmDispatch, lmLocaleToIndex_;
begin
 asm
  MOVE.L #$lmLocaleToIndex_, d2;
 end;
 LmLocaleToIndex := __LmLocaleToIndex(iLocale, oLocaleIndex);
end;

function LmGetLocaleSetting(iLocaleIndex: UInt16; iChoice: LmLocaleSettingChoice;
                            oValue: Pointer; iValueSize: UInt16): Err; // syscall sysTrapLmDispatch, lmGetLocaleSetting_;
begin
 asm
  MOVE.L #$lmGetLocaleSetting_, d2;
 end;
 LmGetLocaleSetting := __LmGetLocaleSetting(iLocaleIndex, iChoice, oValue, iValueSize);
end;

end.

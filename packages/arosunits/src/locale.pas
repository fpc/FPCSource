{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    locale.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$PACKRECORDS 2}

unit locale;

interface

uses
  exec, amigados, utility;

type
  PLocaleBase = ^TLocaleBase;
  TLocaleBase = record
    lb_LibNode: TLibrary;
    lb_SysPatches: WordBool;
  end;

{ This structure must only be allocated by locale.library and is READ-ONLY! }
  PLocale = ^TLocale;
  TLocale = record
    // Basics
    loc_LocaleName: STRPTR;                   // Name of locale
    loc_LanguageName: STRPTR;                 // Language of locale
    loc_PrefLanguages: array[0..9] of STRPTR; // Preferred languages as array
    loc_Flags: LongWord;                      // Locale Flags (LOCF_*)
    loc_CodeSet: LongWord;
    // Country
    loc_CountryCode: LongWord;         // country code
    loc_TelephoneCode: LongWord;       // telephone code of country
    loc_GMTOffset: LongInt;            // distance to GMT in minutes
    loc_MeasuringSystem: Byte;         // Used measuring system (MS_*)
    loc_CalendarType: Byte;            // Define start of the Week (CT_*)
    loc_Reserved0: array[0..1] of Byte;
    // DateTime
    loc_DateTimeFormat,                     // Long date, time format
    loc_DateFormat,                         // Long date format
    loc_TimeFormat,                         // Long time format
    loc_ShortDateTimeFormat,                // Short date, time format
    loc_ShortDateFormat,                    // Short date format
    loc_ShortTimeFormat,                    // Short time format
    // Numerics
    loc_DecimalPoint: STRPTR;               // Decimalseparator
    loc_GroupSeparator: STRPTR;             // Thousandseparator
    loc_FracGroupSeparator: STRPTR;         // Additional separator
    loc_Grouping: PByte;                    // Size of loc_FracGroupSeparator group
    loc_FracGrouping: PByte;                // Size of loc_FracGroupSeparator group
    // Currency Format
    loc_MonDecimalPoint: STRPTR;
    loc_MonGroupSeparator: STRPTR;
    loc_MonFracGroupSeparator: STRPTR;
    loc_MonGrouping: PByte;
    loc_MonFracGrouping: PByte;
    loc_MonFracDigits: Byte;       // Numbers of places after loc_MonDecimalPoint
    loc_MonIntFracDigits: Byte;
    loc_Reserved1: array[0..1] of Byte;
    // Currency symbol
    loc_MonCS: STRPTR;             // Currency symbol (e.g. $)
    loc_MonSmallCS: STRPTR;        // Symbol for small amounts (eg. cent)
    loc_MonIntCS: STRPTR;          // Internationl (ISO 4217) code (e.g. EUR/USD/GBP)

    loc_MonPositiveSign: STRPTR;   // for positive money value
    loc_MonPositiveSpaceSep: Byte; // Separated by space (SS_*)
    loc_MonPositiveSignPos: Byte;  // Position of positive sign (SP_*)
    loc_MonPositiveCSPos: Byte;    // Position of currency symbol (CSP_*)
    loc_Reserved2: Byte;

    loc_MonNegativeSign: STRPTR;   // for negative money values
    loc_MonNegativeSpaceSep: Byte; // Separated by space (SS_*)
    loc_MonNegativeSignPos: Byte;  // Position of negative sign (SP_*)
    loc_MonNegativeCSPos: Byte;    // Position of currency symbol (CSP_*)
    loc_Reserved3: Byte;
 end;


const
  // Flags for loc_Flags
  LOCF_GMT_CLOCK = 1 shl 16; // Hardware clock stores GMT, AROS specific

  // Values for loc_MeasuringSystem
  MS_ISO      = 0;
  MS_AMERICAN = 1;
  MS_IMPERIAL = 2;
  MS_BRITISH  = 3;

  // Values for loc_CalendarType
  CT_7SUN = 0; // First day is Sunday
  CT_7MON = 1; // First day is Monday
  CT_7TUE = 2; // First day is Tuesday
  CT_7WED = 3; // First day is Wednesday
  CT_7THU = 4; // First day is Thursday
  CT_7FRI = 5; // First day is Friday
  CT_7SAT = 6; // First day is Saturday

  // Values for loc_MonPositiveSpaceSep and loc_MonNegativeSpaceSep
  SS_NOSPACE = 0; // No space between currency symbol and value
  SS_SPACE   = 1; // Set a space between currency symbol and value

  // Values for loc_MonPositiveSignPos and loc_MonNegativeSignPos
  SP_PARENS    = 0; // () surround the quantity and currency_symbol
  SP_PREC_ALL  = 1; // sign before value and symbol
  SP_SUCC_ALL  = 2; // sign after value and symbol
  SP_PREC_CURR = 3; // sign right before currency symbol
  SP_SUCC_CURR = 4; // sign right after currency symbol

  // Values for loc_MonPositiveCSPos and loc_MonNegativeCSPos
  CSP_PRECEDES = 0; // currency symbol before value
  CSP_SUCCEEDS = 1; // currency symbol after value

  // Tags for OpenCatalog()
  OC_TagBase         = TAG_USER + $90000;
  OC_BuiltInLanguage = OC_TagBase + 1; // language of built-in strings
  OC_BuiltInCodeSet  = OC_TagBase + 2; // code set of built-in strings
  OC_Version         = OC_TagBase + 3; // catalog version number required
  OC_Language        = OC_TagBase + 4; // preferred language of catalog

  // Comparison types for StrnCmp()
  SC_ASCII    = 0;
  SC_COLLATE1 = 1;
  SC_COLLATE2 = 2;

// Internal String-Numbers GetLocaleStr()
  // Days of Week
  DAY_1   = 1; // Sunday
  DAY_2   = 2; // Monday
  DAY_3   = 3; // Tuesday
  DAY_4   = 4; // Wednesday
  DAY_5   = 5; // Thursday
  DAY_6   = 6; // Friday
  DAY_7   = 7; // Saturday
  // Abbreviated Days of Week
  ABDAY_1 = 8;  // Sun
  ABDAY_2 = 9;  // Mon
  ABDAY_3 = 10; // Tue
  ABDAY_4 = 11; // Wed
  ABDAY_5 = 12; // Thu
  ABDAY_6 = 13; // Fri
  ABDAY_7 = 14; // Sat
  // Months
  MON_1    = 15; // January
  MON_2    = 16; // February
  MON_3    = 17; // March
  MON_4    = 18; // April
  MON_5    = 19; // May
  MON_6    = 20; // June
  MON_7    = 21; // July
  MON_8    = 22; // August
  MON_9    = 23; // September
  MON_10   = 24; // October
  MON_11   = 25; // November
  MON_12   = 26; // December
  // Abbreviated Months
  ABMON_1  = 27; // Jan
  ABMON_2  = 28; // Feb
  ABMON_3  = 29; // Mar
  ABMON_4  = 30; // Apr
  ABMON_5  = 31; // May
  ABMON_6  = 32; // Jun
  ABMON_7  = 33; // Jul
  ABMON_8  = 34; // Aug
  ABMON_9  = 35; // Sep
  ABMON_10 = 36; // Oct
  ABMON_11 = 37; // Nov
  ABMON_12 = 38; // Dec
  // Question
  YESSTR = 39; // affirmative response for yes/no queries
  NOSTR  = 40; // negative response for yes/no queries
  // 12 hour clock
  AM_STR = 41;
  PM_STR = 42;
  // Hyphen
  SOFTHYPHEN = 43;
  HARDHYPHEN = 44;
  // Quotes
  OPENQUOTE  = 45; // Open quote
  CLOSEQUOTE = 46; // Close quote
  // relative day names
  YESTERDAYSTR = 47; // Yesterday
  TODAYSTR     = 48; // Today
  TOMORROWSTR  = 49; // Tomorrow
  FUTURESTR    = 50; // Future

  LANG_NAME = 51; // V50
  MAXSTRMSG = 52; // current number of defined strings

type
// This structure must only be allocated by locale.library and is READ-ONLY!
  PCatalog = ^TCatalog;
  TCatalog = record
    cat_Link: TNode;       // for internal linkage
    cat_Pad: Word;         // to longword align
    cat_Language: STRPTR;  // language of the catalog
    cat_CodeSet: LongWord; // currently always 0
    cat_Version: Word;     // version of the catalog
    cat_Revision: Word;    // revision of the catalog
 end;

var
  LocaleBase: PLocaleBase = nil;

const
  LOCALENAME: PChar = 'locale.library';

procedure CloseCatalog(Catalog: PCatalog); syscall LocaleBase 6;
procedure CloseLocale(Locale: PLocale); syscall LocaleBase 7;
function ConvToLower(Locale: PLocale; Character: LongWord): LongWord; syscall LocaleBase 8;
function ConvToUpper(Locale: PLocale; Character: LongWord): LongWord; syscall LocaleBase 9;
procedure FormatDate(Locale: PLocale; FormatString: STRPTR; Date: PDateStamp; Hook: PHook); syscall LocaleBase 10;
function FormatString(Locale: PLocale; FmtTemplate: STRPTR; DataStream: APTR; PutCharFunc: PHook): APTR; syscall LocaleBase 11;
function GetCatalogStr(Catalog: PCatalog; StringNum: LongWord; DefaultString: STRPTR): STRPTR; syscall LocaleBase 12;
function GetLocaleStr(Locale: PLocale; StringNum: LongWord): STRPTR; syscall LocaleBase 13;
function IsAlNum(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 14;
function IsAlpha(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 15;
function IsCntrl(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 16;
function IsDigit(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 17;
function IsGraph(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 18;
function IsLower(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 19;
function IsPrint(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 20;
function IsPunct(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 21;
function IsSpace(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 22;
function IsUpper(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 23;
function IsXDigit(Locale: PLocale; Character: LongWord): LongBool; syscall LocaleBase 24;
function OpenCatalogA(Locale: PLocale; Name: STRPTR; Tags: PTagItem): PCatalog; syscall LocaleBase 25;
function OpenLocale(Name: STRPTR): PLocale; syscall LocaleBase 26;
function ParseDate(Locale: PLocale; Date: PDateStamp; FmtTemplate: STRPTR; GetCharFunc: PHook): LongBool; syscall LocaleBase 27;
function LocalePrefsUpdate(Locale: PLocale): PLocale; syscall LocaleBase 28;
function StrConvert(Locale: PLocale; String1: STRPTR; Buffer: APTR; BufferSize: LongWord; Typ: LongWord): LongWord; syscall LocaleBase 29;
function StrnCmp(Locale: PLocale; String1: STRPTR; String2: STRPTR; Length: LongInt; Typ: LongWord): LongInt; syscall LocaleBase 30;

function OpenCatalog(Locale: PLocale; Name: STRPTR; const Tags: array of PtrUInt): PCatalog;

implementation

function OpenCatalog(Locale: PLocale; Name: STRPTR; const Tags: array of PtrUInt): PCatalog;
begin
  OpenCatalog := OpenCatalogA(Locale, Name, @Tags);
end;

const
  LIBVERSION = 0;

initialization
  LocaleBase := PLocaleBase(OpenLibrary(LOCALENAME, LIBVERSION));
finalization
  if Assigned(LocaleBase) then
    CloseLibrary(PLibrary(LocaleBase));
end.




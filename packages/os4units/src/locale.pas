{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by Free Pascal development team

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

const
// constants for GetLocaleStr()
  LANG_CODE = 0; // ISO-639-2/T 3-letter lowercase language code (e.g. "eng"). Added in V50

  DAY_1 = 1; // Sunday
  DAY_2 = 2; // Monday
  DAY_3 = 3; // Tuesday
  DAY_4 = 4; // Wednesday
  DAY_5 = 5; // Thursday
  DAY_6 = 6; // Friday
  DAY_7 = 7; // Saturday

  ABDAY_1 = 8;  // Sun
  ABDAY_2 = 9;  // Mon
  ABDAY_3 = 10; // Tue
  ABDAY_4 = 11; // Wed
  ABDAY_5 = 12; // Thu
  ABDAY_6 = 13; // Fri
  ABDAY_7 = 14; // Sat

  MON_1  = 15; // January
  MON_2  = 16; // February
  MON_3  = 17; // March
  MON_4  = 18; // April
  MON_5  = 19; // May
  MON_6  = 20; // June
  MON_7  = 21; // July
  MON_8  = 22; // August
  MON_9  = 23; // September
  MON_10 = 24; // October
  MON_11 = 25; // November
  MON_12 = 26; // December

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

  YESSTR = 39; // affirmative response for yes/no queries
  NOSTR  = 40; // negative response for yes/no queries

  AM_STR = 41; // AM
  PM_STR = 42; // PM

  SOFTHYPHEN = 43; // soft hyphenation
  HARDHYPHEN = 44; // hard hyphenation

  OPENQUOTE  = 45; // start of quoted block
  CLOSEQUOTE = 46; // end of quoted block

  YESTERDAYSTR = 47; // Yesterday
  TODAYSTR     = 48; // Today
  TOMORROWSTR  = 49; // Tomorrow
  FUTURESTR    = 50; // Future

  LANG_NAME = 51; // native name of this language in this language and charset. Added in V50.

// Added in V50: Alternate forms of weekday names to be used
// in date strings, some languages need those. Fallback are
// the normal weekday names for standalone usage.
  ALTDAY_1 = 52; // Sunday
  ALTDAY_2 = 53; // Monday
  ALTDAY_3 = 54; // Tuesday
  ALTDAY_4 = 55; // Wednesday
  ALTDAY_5 = 56; // Thursday
  ALTDAY_6 = 57; // Friday
  ALTDAY_7 = 58; // Saturday

// Added in V50: Alternate forms of month names to be used
// in date strings, some languages need those. Fallback are
// the normal month names for standalone usage.
  ALTMON_1  = 59; // January
  ALTMON_2  = 60; // February
  ALTMON_3  = 61; // March
  ALTMON_4  = 62; // April
  ALTMON_5  = 63; // May
  ALTMON_6  = 64; // June
  ALTMON_7  = 65; // July
  ALTMON_8  = 66; // August
  ALTMON_9  = 67; // September
  ALTMON_10 = 68; // October
  ALTMON_11 = 69; // November
  ALTMON_12 = 70; // December

  MAXSTRMSG = 71;      { current number of defined strings }


//***************************************************************************

type
  // OpenLibrary("locale.library",0) returns a pointer to this structure
  PLocaleBase = ^TLocaleBase;
  TLocaleBase = record
    lb_LibNode: TLibrary;
    lb_SysPatches: Boolean;   // True if locale installed its patches
  end;

{***************************************************************************}

type
// This structure must only be allocated by locale.library and is READ-ONLY!
  PLocale = ^TLocale;
  TLocale = record
    loc_LocaleName: STRPTR;                   // locale's name
    loc_LanguageName: STRPTR;                 // language of this locale
    loc_PrefLanguages: array[0..9] of STRPTR; // preferred languages
    loc_Flags: LongWord;                      // always 0 for now

    loc_CodeSet: LongWord;       // IANA charset number. V50+ default charset
    loc_CountryCode: LongWord;   // user's country code
    loc_TelephoneCode: LongWord; // country's telephone code
    loc_GMTOffset: LongWord;     // minutes from GMT
    loc_MeasuringSystem: Byte;   // what measuring system?
    loc_CalendarType: Byte;      // what calendar type?
    loc_Reserved0: array[0..1] of Byte;

    loc_DateTimeFormat: STRPTR; // regular date & time format
    loc_DateFormat: STRPTR;     // date format by itself
    loc_TimeFormat: STRPTR;     // time format by itself

    loc_ShortDateTimeFormat: STRPTR; // short date & time format
    loc_ShortDateFormat: STRPTR;     // short date format by itself
    loc_ShortTimeFormat: STRPTR;     // short time format by itself

    // for numeric values
    loc_DecimalPoint: STRPTR;       // character before the decimals
    loc_GroupSeparator: STRPTR;     // separates groups of digits
    loc_FracGroupSeparator: STRPTR; // separates groups of digits
    loc_Grouping: PByte;            // size of each group
    loc_FracGrouping: PByte;        // size of each group

    // for monetary values
    loc_MonDecimalPoint: STRPTR;
    loc_MonGroupSeparator: STRPTR;
    loc_MonFracGroupSeparator: STRPTR;
    loc_MonGrouping: PByte;
    loc_MonFracGrouping: PByte;
    loc_MonFracDigits: Byte;           // digits after the decimal point
    loc_MonIntFracDigits : Byte;       // for international representation
    loc_Reserved1: array[0..1] of Byte;

    // for currency symbols
    loc_MonCS: STRPTR;      // currency symbol
    loc_MonSmallCS: STRPTR; // symbol for small amounts
    loc_MonIntCS: STRPTR;   // internationl (ISO 4217) code

    { for positive monetary values }
    loc_MonPositiveSign: STRPTR;   // indicate positive money value
    loc_MonPositiveSpaceSep: Byte; // determine if separated by space
    loc_MonPositiveSignPos: Byte;  // position of positive sign
    loc_MonPositiveCSPos: Byte;    // position of currency symbol
    loc_Reserved2: Byte;

    //{ for negative monetary values }
    loc_MonNegativeSign: STRPTR;     // indicate negative money value
    loc_MonNegativeSpaceSep: STRPTR; // determine if separated by space
    loc_MonNegativeSignPos: STRPTR;  // position of negative sign
    loc_MonNegativeCSPos: STRPTR;    // position of currency symbol
    loc_Reserved3: Byte;
 end;

const
// constants for Locale.loc_MeasuringSystem
  MS_ISO      = 0; // international metric system
  MS_AMERICAN = 1; // american system
  MS_IMPERIAL = 2; // imperial system
  MS_BRITISH  = 3; // british system

// constants for Locale.loc_CalendarType
  CT_7SUN = 0; // 7 days a week, Sunday is the first day
  CT_7MON = 1; // 7 days a week, Monday is the first day
  CT_7TUE = 2; // 7 days a week, Tuesday is the first day
  CT_7WED = 3; // 7 days a week, Wednesday is the first day
  CT_7THU = 4; // 7 days a week, Thursday is the first day
  CT_7FRI = 5; // 7 days a week, Friday is the first day
  CT_7SAT = 6; // 7 days a week, Saturday is the first day

// constants for Locale.loc_MonPositiveSpaceSep and Locale.loc_MonNegativeSpaceSep
  SS_NOSPACE = 0; // cur. symbol is NOT separated from value with a space
  SS_SPACE   = 1; // cur. symbol IS separated from value with a space

// constants for Locale.loc_MonPositiveSignPos and Locale.loc_MonNegativeSignPos }
  SP_PARENS    = 0; // () surround the quantity and currency_symbol
  SP_PREC_ALL  = 1; // sign string comes before amount and symbol
  SP_SUCC_ALL  = 2; // sign string comes after amount and symbol
  SP_PREC_CURR = 3; // sign string comes right before currency symbol
  SP_SUCC_CURR = 4; // sign string comes right after currency symbol

// constants for Locale.loc_MonPositiveCSPos and Locale.loc_MonNegativeCSPos
  CSP_PRECEDES = 0; // currency symbol comes before value
  CSP_SUCCEEDS = 1; // currency symbol comes after value

{ elements of the byte arrays pointed to by:
    Locale.loc_Grouping
    Locale.loc_FracGrouping
    Locale.loc_MonGrouping
    Locale.loc_MonFracGrouping
  are interpreted as follows:

     255     indicates that no further grouping is to be performed
     0       indicates that the previous element is to be repeatedly used
             for the remainder of the digits
     <other> the number of digits that comprises the current group}


//***************************************************************************


// Tags for OpenCatalog()
  OC_TagBase         = TAG_USER + $90000;
  OC_BuiltInLanguage = OC_TagBase + 1; // language of built-in strings
  OC_BuiltInCodeSet  = OC_TagBase + 2; // code set of built-in strings
  OC_Version         = OC_TagBase + 3; // catalog version number required
  OC_Language        = OC_TagBase + 4; // preferred language of catalog
  OC_WantedCodeSet   = OC_TagBase + 5; // code set of font to be used (V50)
  OC_BuiltInVersion  = OC_TagBase + 6; // version of builtin strings (V52.3)
  OC_PreferExternal  = OC_TagBase + 7; // replace builtin language? (V53.4)


//***************************************************************************


// Comparison types for StrnCmp()
  SC_ASCII    = 0;
  SC_COLLATE1 = 1;
  SC_COLLATE2 = 2;


//***************************************************************************

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
  ILocale: PInterface = nil;

const
  LOCALENAME: PChar = 'locale.library';

function LocaleObtain(): LongWord; syscall ILocale 60;
function LocaleRelease(): LongWord; syscall ILocale 64;
procedure LocaleExpunge(); syscall ILocale 68;
function LocaleClone(): PInterface; syscall ILocale 72;
// 76 Private 1
procedure CloseCatalog(Catalog: PCatalog); syscall ILocale 80;
procedure CloseLocale(Locale: PLocale); syscall ILocale 84;
function ConvToLower(Locale: PLocale; Character: LongWord): LongWord; syscall ILocale 88;
function ConvToUpper(Locale: PLocale; Character: LongWord): LongWord; syscall ILocale 92;
procedure FormatDate(Locale: PLocale; FmtTemplate: STRPTR; Date: PDateStamp; PutCharFunc: PHook); syscall ILocale 96;
function FormatString(Locale: PLocale; FmtTemplate: STRPTR; DataStream: APTR; PutCharFunc: PHook): POINTER; syscall ILocale 100;
function GetCatalogStr(Catalog: PCatalog; StringNum: LongInt; DefaultString: STRPTR): PChar; syscall ILocale 104;
function GetLocaleStr(Locale: PLocale; StringNum: LongWord): PChar; syscall ILocale 108;
function IsAlNum(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 112;
function IsAlpha(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 116;
function IsCntrl(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 120;
function IsDigit(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 124;
function IsGraph(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 128;
function IsLower(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 132;
function IsPrint(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 136;
function IsPunct(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 140;
function IsSpace(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 144;
function IsUpper(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 148;
function IsXDigit(Locale: PLocale; Character: LongWord): LongBool; syscall ILocale 152;
function OpenCatalogA(Locale: PLocale; Name: PChar; Tags: PTagItem): PCatalog; syscall ILocale 156;
// 160 OpenCatalog
function OpenLocale(Name: STRPTR): PLocale; syscall ILocale 164;
function ParseDate(Locale: PLocale; Date: PDateStamp; FmtTemplate: STRPTR; GetCharFunc: PHook): LongBool; syscall ILocale 168;
// 172 Private2
function StrConvert(Locale: PLocale; String1: STRPTR; Buffer: APTR; BufferSize: LongWord; Typ: LongWord): LongWord; syscall ILocale 176;
function StrnCmp(Locale: PLocale; String1: STRPTR; String2: STRPTR; Length: LongInt; Typ: LongWord): LongInt; syscall ILocale 180;
function Locale_DateToStr(DateTime: PDateTime): LongInt; syscall ILocale 184;
function Locale_StrToDate(DateTime: PDateTime): LongInt; syscall ILocale 188;
function IsBlank(Locale: PLocale; Character: LongWord): LongInt; syscall ILocale 192;
function FormatString32(Locale: Plocale; FmtTemplate: STRPTR; DataStream: APTR; PutCharFunc: PHook): APTR; syscall ILocale 196;

function OpenCatalog(Locale: PLocale; Name: STRPTR; const Args: array of PtrUInt): PCatalog; inline;

implementation

function OpenCatalog(Locale: PLocale; Name: STRPTR; const Args: array of PtrUInt): PCatalog;
begin
  OpenCatalog := OpenCatalogA(Locale, Name, @Args);
end;

const
  LIBVERSION: LongWord = 0;

initialization
  LocaleBase := PLocaleBase(OpenLibrary(LOCALENAME, LIBVERSION));
  if Assigned(LocaleBase) then
    ILocale := GetInterface(PLibrary(LocaleBase), 'main', 1, nil);
finalization
  if Assigned(ILocale) then
    DropInterface(ILocale);
  if Assigned(LocaleBase) then
    CloseLibrary(PLibrary(LocaleBase));
end.




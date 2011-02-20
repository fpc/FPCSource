{ Parsed from Foundation.framework NSLocale.h }

{$ifdef TYPES}
{$ifndef NSLOCALE_PAS_T}
{$define NSLOCALE_PAS_T}

{ Constants }

const
  NSLocaleLanguageDirectionUnknown = kCFLocaleLanguageDirectionUnknown;
  NSLocaleLanguageDirectionLeftToRight = kCFLocaleLanguageDirectionLeftToRight;
  NSLocaleLanguageDirectionRightToLeft = kCFLocaleLanguageDirectionRightToLeft;
  NSLocaleLanguageDirectionTopToBottom = kCFLocaleLanguageDirectionTopToBottom;
  NSLocaleLanguageDirectionBottomToTop = kCFLocaleLanguageDirectionBottomToTop;

{ Types }
type
  NSLocaleLanguageDirection = NSUInteger;
  NSLocaleLanguageDirectionPtr = ^NSLocaleLanguageDirection;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSLOCALE_PAS_R}
{$define NSLOCALE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSLOCALE_PAS_F}
{$define NSLOCALE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSLOCALE_PAS_S}
{$define NSLOCALE_PAS_S}

{ External string constants }
var
  NSCurrentLocaleDidChangeNotification: NSString; cvar; external;
  NSLocaleIdentifier: NSString; cvar; external;
  NSLocaleLanguageCode: NSString; cvar; external;
  NSLocaleCountryCode: NSString; cvar; external;
  NSLocaleScriptCode: NSString; cvar; external;
  NSLocaleVariantCode: NSString; cvar; external;
  NSLocaleExemplarCharacterSet: NSString; cvar; external;
  NSLocaleCalendar: NSString; cvar; external;
  NSLocaleCollationIdentifier: NSString; cvar; external;
  NSLocaleUsesMetricSystem: NSString; cvar; external;
  NSLocaleMeasurementSystem: NSString; cvar; external;
  NSLocaleDecimalSeparator: NSString; cvar; external;
  NSLocaleGroupingSeparator: NSString; cvar; external;
  NSLocaleCurrencySymbol: NSString; cvar; external;
  NSLocaleCurrencyCode: NSString; cvar; external;
  NSLocaleCollatorIdentifier: NSString; cvar; external;
  NSLocaleQuotationBeginDelimiterKey: NSString; cvar; external;
  NSLocaleQuotationEndDelimiterKey: NSString; cvar; external;
  NSLocaleAlternateQuotationBeginDelimiterKey: NSString; cvar; external;
  NSLocaleAlternateQuotationEndDelimiterKey: NSString; cvar; external;
  NSGregorianCalendar: NSString; cvar; external;
  NSBuddhistCalendar: NSString; cvar; external;
  NSChineseCalendar: NSString; cvar; external;
  NSHebrewCalendar: NSString; cvar; external;
  NSIslamicCalendar: NSString; cvar; external;
  NSIslamicCivilCalendar: NSString; cvar; external;
  NSJapaneseCalendar: NSString; cvar; external;
  NSRepublicOfChinaCalendar: NSString; cvar; external;
  NSPersianCalendar: NSString; cvar; external;
  NSIndianCalendar: NSString; cvar; external;
  NSISO8601Calendar: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLocale = objcclass;
  NSLocalePointer = ^NSLocale;
  NSLocalePtr = NSLocalePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSLOCALE_PAS_C}
{$define NSLOCALE_PAS_C}

{ NSLocale }
  NSLocale = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    function objectForKey(key: id): id; message 'objectForKey:';
    function displayNameForKey_value(key: id; value: id): NSString; message 'displayNameForKey:value:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSExtendedLocaleCategory }
  NSExtendedLocaleCategory = objccategory external (NSLocale)
    function localeIdentifier: NSString; message 'localeIdentifier';
  end;

{ NSLocaleCreationCategory }
  NSLocaleCreationCategory = objccategory external (NSLocale)
    class function systemLocale: id; message 'systemLocale';
    class function currentLocale: id; message 'currentLocale';
    class function autoupdatingCurrentLocale: id; message 'autoupdatingCurrentLocale';
    function initWithLocaleIdentifier(string_: NSString): id; message 'initWithLocaleIdentifier:';
  end;

{ NSLocaleGeneralInfoCategory }
  NSLocaleGeneralInfoCategory = objccategory external (NSLocale)
    class function availableLocaleIdentifiers: NSArray; message 'availableLocaleIdentifiers';
    class function ISOLanguageCodes: NSArray; message 'ISOLanguageCodes';
    class function ISOCountryCodes: NSArray; message 'ISOCountryCodes';
    class function ISOCurrencyCodes: NSArray; message 'ISOCurrencyCodes';
    class function commonISOCurrencyCodes: NSArray; message 'commonISOCurrencyCodes';
    class function preferredLanguages: NSArray; message 'preferredLanguages';
    class function componentsFromLocaleIdentifier(string_: NSString): NSDictionary; message 'componentsFromLocaleIdentifier:';
    class function localeIdentifierFromComponents(dict: NSDictionary): NSString; message 'localeIdentifierFromComponents:';
    class function canonicalLocaleIdentifierFromString(string_: NSString): NSString; message 'canonicalLocaleIdentifierFromString:';
    class function canonicalLanguageIdentifierFromString(string_: NSString): NSString; message 'canonicalLanguageIdentifierFromString:';
    class function localeIdentifierFromWindowsLocaleCode(lcid: cuint32): NSString; message 'localeIdentifierFromWindowsLocaleCode:';
    class function windowsLocaleCodeFromLocaleIdentifier(localeIdentifier_: NSString): cuint32; message 'windowsLocaleCodeFromLocaleIdentifier:';
    class function characterDirectionForLanguage(isoLangCode: NSString): NSLocaleLanguageDirection; message 'characterDirectionForLanguage:';
    class function lineDirectionForLanguage(isoLangCode: NSString): NSLocaleLanguageDirection; message 'lineDirectionForLanguage:';
  end;

{$endif}
{$endif}

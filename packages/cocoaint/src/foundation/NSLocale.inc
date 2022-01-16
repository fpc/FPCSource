{ Parsed from Foundation.framework NSLocale.h }


{ Types from NSLocaleGeneralInfo }
{$ifdef TYPES}

type
  NSLocaleLanguageDirection = NSUInteger;
type
  NSLocaleLanguageDirectionPtr = ^NSLocaleLanguageDirection;

const
  NSLocaleLanguageDirectionUnknown = kCFLocaleLanguageDirectionUnknown;
  NSLocaleLanguageDirectionLeftToRight = kCFLocaleLanguageDirectionLeftToRight;
  NSLocaleLanguageDirectionRightToLeft = kCFLocaleLanguageDirectionRightToLeft;
  NSLocaleLanguageDirectionTopToBottom = kCFLocaleLanguageDirectionTopToBottom;
  NSLocaleLanguageDirectionBottomToTop = kCFLocaleLanguageDirectionBottomToTop;
{$endif}


{$ifdef TYPES}
type
  NSLocalePtr = ^NSLocale;
{$endif}

{$ifdef CLASSES}

type
  NSLocale = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    function objectForKey (key: id): id; message 'objectForKey:';
    function displayNameForKey_value (key: id; value: id): NSString; message 'displayNameForKey:value:';
    function initWithLocaleIdentifier (string_: NSString): instancetype; message 'initWithLocaleIdentifier:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSExtendedLocale = objccategory external (NSLocale)
    function localeIdentifier: NSString; message 'localeIdentifier';
  end;

type
  NSLocaleCreation = objccategory external (NSLocale)
    class function autoupdatingCurrentLocale: NSLocale; message 'autoupdatingCurrentLocale'; { available in 10_5, 2_0 }
    class function currentLocale: NSLocale; message 'currentLocale';
    class function systemLocale: NSLocale; message 'systemLocale';
    class function localeWithLocaleIdentifier (ident: NSString): instancetype; message 'localeWithLocaleIdentifier:'; { available in 10_6, 4_0 }
    function init: instancetype; message 'init';
  end;

type
  NSLocaleGeneralInfo = objccategory external (NSLocale)
    class function availableLocaleIdentifiers: NSArray; message 'availableLocaleIdentifiers';
    class function ISOLanguageCodes: NSArray; message 'ISOLanguageCodes';
    class function ISOCountryCodes: NSArray; message 'ISOCountryCodes';
    class function ISOCurrencyCodes: NSArray; message 'ISOCurrencyCodes';
    class function commonISOCurrencyCodes: NSArray; message 'commonISOCurrencyCodes'; { available in 10_5, 2_0 }
    class function preferredLanguages: NSArray; message 'preferredLanguages'; { available in 10_5, 2_0 }
    class function componentsFromLocaleIdentifier (string_: NSString): NSDictionary; message 'componentsFromLocaleIdentifier:';
    class function localeIdentifierFromComponents (dict: NSDictionary): NSString; message 'localeIdentifierFromComponents:';
    class function canonicalLocaleIdentifierFromString (string_: NSString): NSString; message 'canonicalLocaleIdentifierFromString:';
    class function canonicalLanguageIdentifierFromString (string_: NSString): NSString; message 'canonicalLanguageIdentifierFromString:';
    class function localeIdentifierFromWindowsLocaleCode (lcid: cuint32): NSString; message 'localeIdentifierFromWindowsLocaleCode:'; { available in 10_6, 4_0 }
    class function windowsLocaleCodeFromLocaleIdentifier (localeIdentifier: NSString): cuint32; message 'windowsLocaleCodeFromLocaleIdentifier:'; { available in 10_6, 4_0 }
    class function characterDirectionForLanguage (isoLangCode: NSString): NSLocaleLanguageDirection; message 'characterDirectionForLanguage:'; { available in 10_6, 4_0 }
    class function lineDirectionForLanguage (isoLangCode: NSString): NSLocaleLanguageDirection; message 'lineDirectionForLanguage:'; { available in 10_6, 4_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSCurrentLocaleDidChangeNotification: NSString { available in 10_5, 2_0 }; cvar; external;
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
  NSLocaleCollatorIdentifier: NSString { available in 10_6, 4_0 }; cvar; external;
  NSLocaleQuotationBeginDelimiterKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSLocaleQuotationEndDelimiterKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSLocaleAlternateQuotationBeginDelimiterKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSLocaleAlternateQuotationEndDelimiterKey: NSString { available in 10_6, 4_0 }; cvar; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSGregorianCalendar: NSString deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarIdentifierGregorian instead"'; cvar; external;
  NSBuddhistCalendar: NSString deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarIdentifierBuddhist instead"'; cvar; external;
  NSChineseCalendar: NSString deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarIdentifierChinese instead"'; cvar; external;
  NSHebrewCalendar: NSString deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarIdentifierHebrew instead"'; cvar; external;
  NSIslamicCalendar: NSString deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarIdentifierIslamic instead"'; cvar; external;
  NSIslamicCivilCalendar: NSString deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarIdentifierIslamicCivil instead"'; cvar; external;
  NSJapaneseCalendar: NSString deprecated 'in 10_4, 10_10, 2_0, 8_0, "Use NSCalendarIdentifierJapanese instead"'; cvar; external;
  NSRepublicOfChinaCalendar: NSString deprecated 'in 10_6, 10_10, 4_0, 8_0, "Use NSCalendarIdentifierRepublicOfChina instead"'; cvar; external;
  NSPersianCalendar: NSString deprecated 'in 10_6, 10_10, 4_0, 8_0, "Use NSCalendarIdentifierPersian instead"'; cvar; external;
  NSIndianCalendar: NSString deprecated 'in 10_6, 10_10, 4_0, 8_0, "Use NSCalendarIdentifierIndian instead"'; cvar; external;
  NSISO8601Calendar: NSString deprecated 'in 10_6, 10_10, 4_0, 8_0, "Use NSCalendarIdentifierISO8601 instead"'; cvar; external;
{$endif}


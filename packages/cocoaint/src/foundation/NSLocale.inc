{ Parsed from Foundation.framework NSLocale.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSLOCALE_PAS_H}
{$define NSLOCALE_PAS_H}
type
  NSLocalePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSLOCALE_PAS_T}
{$define NSLOCALE_PAS_T}

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
  NSCurrentLocaleDidChangeNotification: NSString; external name '_NSCurrentLocaleDidChangeNotification';
  NSLocaleIdentifier: NSString; external name '_NSLocaleIdentifier';
  NSLocaleLanguageCode: NSString; external name '_NSLocaleLanguageCode';
  NSLocaleCountryCode: NSString; external name '_NSLocaleCountryCode';
  NSLocaleScriptCode: NSString; external name '_NSLocaleScriptCode';
  NSLocaleVariantCode: NSString; external name '_NSLocaleVariantCode';
  NSLocaleExemplarCharacterSet: NSString; external name '_NSLocaleExemplarCharacterSet';
  NSLocaleCalendar: NSString; external name '_NSLocaleCalendar';
  NSLocaleCollationIdentifier: NSString; external name '_NSLocaleCollationIdentifier';
  NSLocaleUsesMetricSystem: NSString; external name '_NSLocaleUsesMetricSystem';
  NSLocaleMeasurementSystem: NSString; external name '_NSLocaleMeasurementSystem';
  NSLocaleDecimalSeparator: NSString; external name '_NSLocaleDecimalSeparator';
  NSLocaleGroupingSeparator: NSString; external name '_NSLocaleGroupingSeparator';
  NSLocaleCurrencySymbol: NSString; external name '_NSLocaleCurrencySymbol';
  NSLocaleCurrencyCode: NSString; external name '_NSLocaleCurrencyCode';
  NSGregorianCalendar: NSString; external name '_NSGregorianCalendar';
  NSBuddhistCalendar: NSString; external name '_NSBuddhistCalendar';
  NSChineseCalendar: NSString; external name '_NSChineseCalendar';
  NSHebrewCalendar: NSString; external name '_NSHebrewCalendar';
  NSIslamicCalendar: NSString; external name '_NSIslamicCalendar';
  NSIslamicCivilCalendar: NSString; external name '_NSIslamicCivilCalendar';
  NSJapaneseCalendar: NSString; external name '_NSJapaneseCalendar';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLocale = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSLOCALE_PAS_C}
{$define NSLOCALE_PAS_C}

{ NSLocale }
  NSLocale = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSLocale; message 'alloc';

    function objectForKey(key: id): id; message 'objectForKey:';
    function displayNameForKey_value(key: id; value: id): NSString; message 'displayNameForKey:value:';

    { Category: NSExtendedLocale }
    function localeIdentifier: NSString; message 'localeIdentifier';

    { Category: NSLocaleCreation }
    class function systemLocale: id; message 'systemLocale';
    class function currentLocale: id; message 'currentLocale';
    function initWithLocaleIdentifier(string_: NSString): id; message 'initWithLocaleIdentifier:';

    { Category: NSLocaleGeneralInfo }
    class function availableLocaleIdentifiers: NSArray; message 'availableLocaleIdentifiers';
    class function ISOLanguageCodes: NSArray; message 'ISOLanguageCodes';
    class function ISOCountryCodes: NSArray; message 'ISOCountryCodes';
    class function ISOCurrencyCodes: NSArray; message 'ISOCurrencyCodes';
    class function componentsFromLocaleIdentifier(string_: NSString): NSDictionary; message 'componentsFromLocaleIdentifier:';
    class function localeIdentifierFromComponents(dict: NSDictionary): NSString; message 'localeIdentifierFromComponents:';
    class function canonicalLocaleIdentifierFromString(string_: NSString): NSString; message 'canonicalLocaleIdentifierFromString:';
  end; external;

{$endif}
{$endif}

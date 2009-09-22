{ Parsed from Foundation.framework NSLocale.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

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
{$ifndef NSLOCALE_PAS_T}
{$define NSLOCALE_PAS_T}

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

{ Parsed from Foundation.framework NSOrthography.h }

{$ifdef TYPES}
{$ifndef NSORTHOGRAPHY_PAS_T}
{$define NSORTHOGRAPHY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSORTHOGRAPHY_PAS_R}
{$define NSORTHOGRAPHY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSORTHOGRAPHY_PAS_F}
{$define NSORTHOGRAPHY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSORTHOGRAPHY_PAS_S}
{$define NSORTHOGRAPHY_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOrthography = objcclass;
  NSOrthographyPointer = ^NSOrthography;
  NSOrthographyPtr = NSOrthographyPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSORTHOGRAPHY_PAS_C}
{$define NSORTHOGRAPHY_PAS_C}

{ NSOrthography }
  NSOrthography = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    function dominantScript: NSString; message 'dominantScript';
    function languageMap: NSDictionary; message 'languageMap';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSOrthographyExtendedCategory }
  NSOrthographyExtendedCategory = objccategory external (NSOrthography)
    function languagesForScript(script: NSString): NSArray; message 'languagesForScript:';
    function dominantLanguageForScript(script: NSString): NSString; message 'dominantLanguageForScript:';
    function dominantLanguage: NSString; message 'dominantLanguage';
    function allScripts: NSArray; message 'allScripts';
    function allLanguages: NSArray; message 'allLanguages';
  end;

{ NSOrthographyCreationCategory }
  NSOrthographyCreationCategory = objccategory external (NSOrthography)
    class function orthographyWithDominantScript_languageMap(script: NSString; map: NSDictionary): id; message 'orthographyWithDominantScript:languageMap:';
    function initWithDominantScript_languageMap(script: NSString; map: NSDictionary): id; message 'initWithDominantScript:languageMap:';
  end;

{$endif}
{$endif}

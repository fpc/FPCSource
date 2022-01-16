{ Parsed from Foundation.framework NSOrthography.h }


{$ifdef TYPES}
type
  NSOrthographyPtr = ^NSOrthography;
{$endif}

{$ifdef CLASSES}

type
  NSOrthography = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    function dominantScript: NSString; message 'dominantScript';
    function languageMap: NSDictionary; message 'languageMap';
    function initWithDominantScript_languageMap (script: NSString; map: NSDictionary): instancetype; message 'initWithDominantScript:languageMap:'; { available in 10_6, 4_0 }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;


type
  NSOrthographyExtended = objccategory external (NSOrthography)
    function languagesForScript (script: NSString): NSArray; message 'languagesForScript:'; { available in 10_6, 4_0 }
    function dominantLanguageForScript (script: NSString): NSString; message 'dominantLanguageForScript:'; { available in 10_6, 4_0 }
    function dominantLanguage: NSString; message 'dominantLanguage';
    function allScripts: NSArray; message 'allScripts';
    function allLanguages: NSArray; message 'allLanguages';
  end;

type
  NSOrthographyCreation = objccategory external (NSOrthography)
    class function orthographyWithDominantScript_languageMap (script: NSString; map: NSDictionary): instancetype; message 'orthographyWithDominantScript:languageMap:'; { available in 10_6, 4_0 }
  end;
{$endif}


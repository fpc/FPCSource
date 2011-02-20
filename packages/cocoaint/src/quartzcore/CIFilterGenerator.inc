{ Parsed from Quartzcore.framework CIFilterGenerator.h }

{$ifdef TYPES}
{$ifndef CIFILTERGENERATOR_PAS_T}
{$define CIFILTERGENERATOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIFILTERGENERATOR_PAS_R}
{$define CIFILTERGENERATOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIFILTERGENERATOR_PAS_F}
{$define CIFILTERGENERATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIFILTERGENERATOR_PAS_S}
{$define CIFILTERGENERATOR_PAS_S}

{ External string constants }
var
  kCIFilterGeneratorExportedKey: NSString; cvar; external;
  kCIFilterGeneratorExportedKeyTargetObject: NSString; cvar; external;
  kCIFilterGeneratorExportedKeyName: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CIFilterGenerator = objcclass;
  CIFilterGeneratorPointer = ^CIFilterGenerator;
  CIFilterGeneratorPtr = CIFilterGeneratorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CIFILTERGENERATOR_PAS_C}
{$define CIFILTERGENERATOR_PAS_C}

{ CIFilterGenerator }
  CIFilterGenerator = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    ___strong: Pointer;
    
  public
    class function filterGenerator: CIFilterGenerator; message 'filterGenerator';
    class function filterGeneratorWithContentsOfURL(aURL: NSURL): CIFilterGenerator; message 'filterGeneratorWithContentsOfURL:';
    function initWithContentsOfURL(aURL: NSURL): id; message 'initWithContentsOfURL:';
    procedure connectObject_withKey_toObject_withKey(sourceObject: id; sourceKey: NSString; targetObject: id; targetKey: NSString); message 'connectObject:withKey:toObject:withKey:';
    procedure disconnectObject_withKey_toObject_withKey(sourceObject: id; key: NSString; targetObject: id; targetKey: NSString); message 'disconnectObject:withKey:toObject:withKey:';
    procedure exportKey_fromObject_withName(key: NSString; targetObject: id; exportedKeyName: NSString); message 'exportKey:fromObject:withName:';
    procedure removeExportedKey(exportedKeyName: NSString); message 'removeExportedKey:';
    function exportedKeys: NSDictionary; message 'exportedKeys';
    procedure setAttributes_forExportedKey(attributes: NSDictionary; key: NSString); message 'setAttributes:forExportedKey:';
    function classAttributes: NSDictionary; message 'classAttributes';
    procedure setClassAttributes(attributes: NSDictionary); message 'setClassAttributes:';
    function filter: CIFilter; message 'filter';
    procedure registerFilterName(name: NSString); message 'registerFilterName:';
    function writeToURL_atomically(aURL: NSURL; flag: Boolean): Boolean; message 'writeToURL:atomically:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}

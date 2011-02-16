{ Parsed from Appkit.framework NSGlyphInfo.h }

{$ifdef TYPES}
{$ifndef NSGLYPHINFO_PAS_T}
{$define NSGLYPHINFO_PAS_T}

{ Constants }

const
  NSIdentityMappingCharacterCollection = 0;
  NSAdobeCNS1CharacterCollection = 1;
  NSAdobeGB1CharacterCollection = 2;
  NSAdobeJapan1CharacterCollection = 3;
  NSAdobeJapan2CharacterCollection = 4;
  NSAdobeKorea1CharacterCollection = 5;

{ Types }
type
  NSCharacterCollection = NSUInteger;
  NSCharacterCollectionPtr = ^NSCharacterCollection;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGLYPHINFO_PAS_R}
{$define NSGLYPHINFO_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGLYPHINFO_PAS_F}
{$define NSGLYPHINFO_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGLYPHINFO_PAS_S}
{$define NSGLYPHINFO_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGlyphInfo = objcclass;
  NSGlyphInfoPointer = ^NSGlyphInfo;
  NSGlyphInfoPtr = NSGlyphInfoPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGLYPHINFO_PAS_C}
{$define NSGLYPHINFO_PAS_C}

{ NSGlyphInfo }
  NSGlyphInfo = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _baseString: NSString;
    
  public
    class function glyphInfoWithGlyphName_forFont_baseString(glyphName_: NSString; font: NSFont; theString: NSString): NSGlyphInfo; message 'glyphInfoWithGlyphName:forFont:baseString:';
    class function glyphInfoWithGlyph_forFont_baseString(glyph: NSGlyph; font: NSFont; theString: NSString): NSGlyphInfo; message 'glyphInfoWithGlyph:forFont:baseString:';
    class function glyphInfoWithCharacterIdentifier_collection_baseString(cid: NSUInteger; characterCollection_: NSCharacterCollection; theString: NSString): NSGlyphInfo; message 'glyphInfoWithCharacterIdentifier:collection:baseString:';
    function glyphName: NSString; message 'glyphName';
    function characterIdentifier: NSUInteger; message 'characterIdentifier';
    function characterCollection: NSCharacterCollection; message 'characterCollection';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}

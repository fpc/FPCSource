{ Parsed from AppKit.framework NSGlyphInfo.h }


{$ifdef TYPES}
type
  NSGlyphInfoPtr = ^NSGlyphInfo;
{$endif}

{$ifdef TYPES}
type
  NSCharacterCollection = NSUInteger;
  NSCharacterCollectionPtr = ^NSCharacterCollection;

const
  NSIdentityMappingCharacterCollection = 0;
  NSAdobeCNS1CharacterCollection = 1;
  NSAdobeGB1CharacterCollection = 2;
  NSAdobeJapan1CharacterCollection = 3;
  NSAdobeJapan2CharacterCollection = 4;
  NSAdobeKorea1CharacterCollection = 5;
{$endif}

{$ifdef CLASSES}

type
  NSGlyphInfo = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _baseString: NSString;
  public
    class function glyphInfoWithGlyphName_forFont_baseString (glyphName: NSString; font: NSFont; theString: NSString): NSGlyphInfo; message 'glyphInfoWithGlyphName:forFont:baseString:';
    class function glyphInfoWithGlyph_forFont_baseString (glyph: NSGlyph; font: NSFont; theString: NSString): NSGlyphInfo; message 'glyphInfoWithGlyph:forFont:baseString:';
    class function glyphInfoWithCharacterIdentifier_collection_baseString (cid: NSUInteger; characterCollection: NSCharacterCollection; theString: NSString): NSGlyphInfo; message 'glyphInfoWithCharacterIdentifier:collection:baseString:';
    function glyphName: NSString; message 'glyphName';
    function characterIdentifier: NSUInteger; message 'characterIdentifier';
    function characterCollection: NSCharacterCollection; message 'characterCollection';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}


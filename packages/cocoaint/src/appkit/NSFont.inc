{ Parsed from AppKit.framework NSFont.h }


{$ifdef TYPES}
type
  NSFontPtr = ^NSFont;
{$endif}

{$ifdef TYPES}
type
  NSGlyph = cuint;
  NSGlyphPtr = ^NSGlyph;

const
  NSControlGlyph = $00FFFFFF;
  NSNullGlyph = $0;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFontIdentityMatrix: CGFloatPtr; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSMultibyteGlyphPacking = NSUInteger;
  NSMultibyteGlyphPackingPtr = ^NSMultibyteGlyphPacking;

const
  NSNativeShortGlyphPacking = 5;

type
  NSFontRenderingMode = NSUInteger;
  NSFontRenderingModePtr = ^NSFontRenderingMode;

const
  NSFontDefaultRenderingMode = 0;
  NSFontAntialiasedRenderingMode = 1;
  NSFontIntegerAdvancementsRenderingMode = 2;
  NSFontAntialiasedIntegerAdvancementsRenderingMode = 3;
{$endif}

{$ifdef CLASSES}

type
  NSFont = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _name: NSString;
    _size: CGFloat;
    _retainCount: NSInteger;
    __fFlags: bitpacked record
      case byte of
        0: (_anonBitField___fFlags0: cuint);
        1: (
          _isScreenFont: 0..1;
          _systemFontType: 0..((1 shl 8)-1);
          _reserved1: 0..((1 shl 4)-1);
          _matrixIsIdentity: 0..1;
          _renderingMode: 0..((1 shl 3)-1);
          _inInstanceCache: 0..1;
          _reserved2: 0..((1 shl 14)-1);
        );
      end;
    _private: id;
  public
    class function fontWithName_size (fontName: NSString; fontSize: CGFloat): NSFont; message 'fontWithName:size:';
    class function fontWithName_matrix (fontName: NSString; fontMatrix: CGFloatPtr): NSFont; message 'fontWithName:matrix:';
    class function fontWithDescriptor_size (fontDescriptor: NSFontDescriptor; fontSize: CGFloat): NSFont; message 'fontWithDescriptor:size:';
    class function fontWithDescriptor_textTransform (fontDescriptor: NSFontDescriptor; textTransform: NSAffineTransform): NSFont; message 'fontWithDescriptor:textTransform:';
    class function userFontOfSize (fontSize: CGFloat): NSFont; message 'userFontOfSize:';
    class function userFixedPitchFontOfSize (fontSize: CGFloat): NSFont; message 'userFixedPitchFontOfSize:';
    class procedure setUserFont (aFont: NSFont); message 'setUserFont:';
    class procedure setUserFixedPitchFont (aFont: NSFont); message 'setUserFixedPitchFont:';
    class function systemFontOfSize (fontSize: CGFloat): NSFont; message 'systemFontOfSize:';
    class function boldSystemFontOfSize (fontSize: CGFloat): NSFont; message 'boldSystemFontOfSize:';
    class function labelFontOfSize (fontSize: CGFloat): NSFont; message 'labelFontOfSize:';
    class function titleBarFontOfSize (fontSize: CGFloat): NSFont; message 'titleBarFontOfSize:';
    class function menuFontOfSize (fontSize: CGFloat): NSFont; message 'menuFontOfSize:';
    class function menuBarFontOfSize (fontSize: CGFloat): NSFont; message 'menuBarFontOfSize:';
    class function messageFontOfSize (fontSize: CGFloat): NSFont; message 'messageFontOfSize:';
    class function paletteFontOfSize (fontSize: CGFloat): NSFont; message 'paletteFontOfSize:';
    class function toolTipsFontOfSize (fontSize: CGFloat): NSFont; message 'toolTipsFontOfSize:';
    class function controlContentFontOfSize (fontSize: CGFloat): NSFont; message 'controlContentFontOfSize:';
    class function systemFontSize: CGFloat; message 'systemFontSize';
    class function smallSystemFontSize: CGFloat; message 'smallSystemFontSize';
    class function labelFontSize: CGFloat; message 'labelFontSize';
    class function systemFontSizeForControlSize (controlSize: NSControlSize): CGFloat; message 'systemFontSizeForControlSize:';
    function fontName: NSString; message 'fontName';
    function pointSize: CGFloat; message 'pointSize';
    function matrix: CGFloatPtr; message 'matrix';
    function familyName: NSString; message 'familyName';
    function displayName: NSString; message 'displayName';
    function fontDescriptor: NSFontDescriptor; message 'fontDescriptor';
    function textTransform: NSAffineTransform; message 'textTransform';
    function numberOfGlyphs: NSUInteger; message 'numberOfGlyphs';
    function mostCompatibleStringEncoding: NSStringEncoding; message 'mostCompatibleStringEncoding';
    function glyphWithName (aName: NSString): NSGlyph; message 'glyphWithName:';
    function coveredCharacterSet: NSCharacterSet; message 'coveredCharacterSet';
    function boundingRectForFont: NSRect; message 'boundingRectForFont';
    function maximumAdvancement: NSSize; message 'maximumAdvancement';
    function ascender: CGFloat; message 'ascender';
    function descender: CGFloat; message 'descender';
    function leading: CGFloat; message 'leading';
    function underlinePosition: CGFloat; message 'underlinePosition';
    function underlineThickness: CGFloat; message 'underlineThickness';
    function italicAngle: CGFloat; message 'italicAngle';
    function capHeight: CGFloat; message 'capHeight';
    function xHeight: CGFloat; message 'xHeight';
    function isFixedPitch: ObjCBOOL; message 'isFixedPitch';
    function boundingRectForGlyph (aGlyph: NSGlyph): NSRect; message 'boundingRectForGlyph:';
    function advancementForGlyph (ag: NSGlyph): NSSize; message 'advancementForGlyph:';
    procedure getBoundingRects_forGlyphs_count (bounds: NSRectArray; glyphs: NSGlyphPtr; glyphCount: NSUInteger); message 'getBoundingRects:forGlyphs:count:';
    procedure getAdvancements_forGlyphs_count (advancements: NSSizeArray; glyphs: NSGlyphPtr; glyphCount: NSUInteger); message 'getAdvancements:forGlyphs:count:';
    procedure getAdvancements_forPackedGlyphs_length (advancements: NSSizeArray; packedGlyphs: pointer; length: NSUInteger); message 'getAdvancements:forPackedGlyphs:length:';
    procedure set_; message 'set';
    procedure setInContext (graphicsContext: NSGraphicsContext); message 'setInContext:';
    function printerFont: NSFont; message 'printerFont';
    function screenFont: NSFont; message 'screenFont';
    function screenFontWithRenderingMode (renderingMode: NSFontRenderingMode): NSFont; message 'screenFontWithRenderingMode:';
    function renderingMode: NSFontRenderingMode; message 'renderingMode';
    function verticalFont: NSFont; message 'verticalFont';
    function isVertical: ObjCBOOL; message 'isVertical';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef FUNCTIONS}
function NSConvertGlyphsToPackedGlyphs(glBuf: NSGlyphPtr; count: NSInteger; packing: NSMultibyteGlyphPacking; packedGlyphs: PChar): NSInteger; cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAntialiasThresholdChangedNotification: NSString; cvar; external;
  NSFontSetChangedNotification: NSString; cvar; external;
{$endif}

{$if  not defined(__LP64__)}

{$if (MAC_OS_X_VERSION_MIN_REQUIRED <= MAC_OS_X_VERSION_10_4)}
{$ifdef TYPES}
const
  NSOneByteGlyphPacking = 0;
  NSJapaneseEUCGlyphPacking = 1;
  NSAsciiWithDoubleByteEUCGlyphPacking = 2;
  NSTwoByteGlyphPacking = 3;
  NSFourByteGlyphPacking = 4;
const
  NSGlyphBelow = 1;
  NSGlyphAbove = 2;
{$endif}

{$endif}
{$ifdef TYPES}
type
  NSGlyphRelation = NSUInteger deprecated 'in 10_0, 10_4';
  NSGlyphRelationPtr = ^NSGlyphRelation deprecated 'in 10_0, 10_4';
{$endif}

{$ifdef CLASSES}

type
  NSFontDeprecated = objccategory external (NSFont)
    class procedure useFont (fontName_: NSString); message 'useFont:'; deprecated 'in 10_0, 10_4';
    function widthOfString (string_: NSString): CGFloat; message 'widthOfString:'; deprecated 'in 10_0, 10_4';
    function isBaseFont: ObjCBOOL; message 'isBaseFont'; deprecated 'in 10_0, 10_4';
    function afmDictionary: NSDictionary; message 'afmDictionary'; deprecated 'in 10_0, 10_4';
    function glyphIsEncoded (aGlyph: NSGlyph): ObjCBOOL; message 'glyphIsEncoded:'; deprecated 'in 10_0, 10_4';
    function defaultLineHeightForFont: CGFloat; message 'defaultLineHeightForFont'; deprecated 'in 10_0, 10_4';
    class function preferredFontNames: NSArray; message 'preferredFontNames'; deprecated 'in 10_0, 10_4';
    class procedure setPreferredFontNames (fontNameArray: NSArray); message 'setPreferredFontNames:'; deprecated 'in 10_0, 10_4';
    function encodingScheme: NSString; message 'encodingScheme'; deprecated 'in 10_0, 10_4';
    function glyphPacking: NSMultibyteGlyphPacking; message 'glyphPacking'; deprecated 'in 10_0, 10_4';
    function positionOfGlyph_precededByGlyph_isNominal (curGlyph: NSGlyph; prevGlyph: NSGlyph; nominal: pObjCBOOL): NSPoint; message 'positionOfGlyph:precededByGlyph:isNominal:'; deprecated 'in 10_0, 10_4';
    function positionsForCompositeSequence_numberOfGlyphs_pointArray (someGlyphs: NSGlyphPtr; numGlyphs: NSInteger; points: NSPointArray): NSInteger; message 'positionsForCompositeSequence:numberOfGlyphs:pointArray:'; deprecated 'in 10_0, 10_4';
    function positionOfGlyph_struckOverGlyph_metricsExist (curGlyph: NSGlyph; prevGlyph: NSGlyph; exist: pObjCBOOL): NSPoint; message 'positionOfGlyph:struckOverGlyph:metricsExist:'; deprecated 'in 10_0, 10_4';
    function positionOfGlyph_struckOverRect_metricsExist (aGlyph: NSGlyph; aRect: NSRect; exist: pObjCBOOL): NSPoint; message 'positionOfGlyph:struckOverRect:metricsExist:'; deprecated 'in 10_0, 10_4';
    function positionOfGlyph_forCharacter_struckOverRect (aGlyph: NSGlyph; aChar: unichar; aRect: NSRect): NSPoint; message 'positionOfGlyph:forCharacter:struckOverRect:'; deprecated 'in 10_0, 10_4';
    function positionOfGlyph_withRelation_toBaseGlyph_totalAdvancement_metricsExist (thisGlyph: NSGlyph; rel: NSGlyphRelation; baseGlyph: NSGlyph; adv: NSSizePointer; exist: pObjCBOOL): NSPoint; message 'positionOfGlyph:withRelation:toBaseGlyph:totalAdvancement:metricsExist:'; deprecated 'in 10_0, 10_4';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAFMFamilyName: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMFontName: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMFormatVersion: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMFullName: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMNotice: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMVersion: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMWeight: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMEncodingScheme: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMCharacterSet: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMCapHeight: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMXHeight: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMAscender: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMDescender: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMUnderlinePosition: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMUnderlineThickness: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMItalicAngle: NSString deprecated 'in 10_0, 10_4'; cvar; external;
  NSAFMMappingScheme: NSString deprecated 'in 10_0, 10_4'; cvar; external;
{$endif}

{$endif}

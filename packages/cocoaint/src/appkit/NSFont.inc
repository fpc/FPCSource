{ Parsed from Appkit.framework NSFont.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFONT_PAS_H}
{$define NSFONT_PAS_H}
type
  NSFontPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSFONT_PAS_T}
{$define NSFONT_PAS_T}

{ Types }
type
  NSGlyph = cuint;
  NSMultibyteGlyphPacking = culong;
  NSFontRenderingMode = culong;
  NSGlyphRelation = culong;

{ Constants }

const
  NSControlGlyph = $00FFFFFF;
  NSNullGlyph = $0;

const
  NSNativeShortGlyphPacking = 5;

const
  NSOneByteGlyphPacking = 0;
  NSJapaneseEUCGlyphPacking = 1;
  NSAsciiWithDoubleByteEUCGlyphPacking = 2;
  NSTwoByteGlyphPacking = 3;
  NSFourByteGlyphPacking = 4;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFONT_PAS_R}
{$define NSFONT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFONT_PAS_F}
{$define NSFONT_PAS_F}

{ Functions }
function NSConvertGlyphsToPackedGlyphs(var glBuf: NSGlyph; count: clong; packing: NSMultibyteGlyphPacking; var packedGlyphs: char): clong; cdecl; external name 'NSConvertGlyphsToPackedGlyphs';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFONT_PAS_T}
{$define NSFONT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFont = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFONT_PAS_C}
{$define NSFONT_PAS_C}

{ NSFont }
  NSFont = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __name: NSString;
    __size: CGFloat;
    __reservedFont1: Pointer;
    __fFlags: bitpacked record
        _isScreenFont: 0..1;
        _systemFontType: 0..((1 shl 8)-1);
        _reserved1: 0..((1 shl 4)-1);
        _matrixIsIdentity: 0..1;
        _renderingMode: 0..((1 shl 3)-1);
        _reserved2: 0..((1 shl 15)-1);
      end;
    __private: id;
    
  public
    class function alloc: NSFont; message 'alloc';

    class function fontWithName_size(fontName_: NSString; fontSize: CGFloat): NSFont; message 'fontWithName:size:';
    class function fontWithName_matrix(fontName_: NSString; var fontMatrix: CGFloat): NSFont; message 'fontWithName:matrix:';
    class function fontWithDescriptor_size(fontDescriptor_: NSFontDescriptor; fontSize: CGFloat): NSFont; message 'fontWithDescriptor:size:';
    class function fontWithDescriptor_textTransform(fontDescriptor_: NSFontDescriptor; textTransform_: NSAffineTransform): NSFont; message 'fontWithDescriptor:textTransform:';
    class function userFontOfSize(fontSize: CGFloat): NSFont; message 'userFontOfSize:';
    class function userFixedPitchFontOfSize(fontSize: CGFloat): NSFont; message 'userFixedPitchFontOfSize:';
    class procedure setUserFont(aFont: NSFont); message 'setUserFont:';
    class procedure setUserFixedPitchFont(aFont: NSFont); message 'setUserFixedPitchFont:';
    class function systemFontOfSize(fontSize: CGFloat): NSFont; message 'systemFontOfSize:';
    class function boldSystemFontOfSize(fontSize: CGFloat): NSFont; message 'boldSystemFontOfSize:';
    class function labelFontOfSize(fontSize: CGFloat): NSFont; message 'labelFontOfSize:';
    class function titleBarFontOfSize(fontSize: CGFloat): NSFont; message 'titleBarFontOfSize:';
    class function menuFontOfSize(fontSize: CGFloat): NSFont; message 'menuFontOfSize:';
    class function menuBarFontOfSize(fontSize: CGFloat): NSFont; message 'menuBarFontOfSize:';
    class function messageFontOfSize(fontSize: CGFloat): NSFont; message 'messageFontOfSize:';
    class function paletteFontOfSize(fontSize: CGFloat): NSFont; message 'paletteFontOfSize:';
    class function toolTipsFontOfSize(fontSize: CGFloat): NSFont; message 'toolTipsFontOfSize:';
    class function controlContentFontOfSize(fontSize: CGFloat): NSFont; message 'controlContentFontOfSize:';
    class function systemFontSize: CGFloat; message 'systemFontSize';
    class function smallSystemFontSize: CGFloat; message 'smallSystemFontSize';
    class function labelFontSize: CGFloat; message 'labelFontSize';
    class function systemFontSizeForControlSize(controlSize: NSControlSize): CGFloat; message 'systemFontSizeForControlSize:';
    function fontName: NSString; message 'fontName';
    function pointSize: CGFloat; message 'pointSize';
    function matrix: CGFloat; message 'matrix';
    function familyName: NSString; message 'familyName';
    function displayName: NSString; message 'displayName';
    function fontDescriptor: NSFontDescriptor; message 'fontDescriptor';
    function textTransform: NSAffineTransform; message 'textTransform';
    function numberOfGlyphs: culong; message 'numberOfGlyphs';
    function mostCompatibleStringEncoding: NSStringEncoding; message 'mostCompatibleStringEncoding';
    function glyphWithName(aName: NSString): NSGlyph; message 'glyphWithName:';
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
    function isFixedPitch: Boolean; message 'isFixedPitch';
    function boundingRectForGlyph(aGlyph: NSGlyph): NSRect; message 'boundingRectForGlyph:';
    function advancementForGlyph(ag: NSGlyph): NSSize; message 'advancementForGlyph:';
    procedure getBoundingRects_forGlyphs_count(bounds: NSRectArray; var glyphs: NSGlyph; glyphCount: culong); message 'getBoundingRects:forGlyphs:count:';
    procedure getAdvancements_forGlyphs_count(advancements: NSSizeArray; var glyphs: NSGlyph; glyphCount: culong); message 'getAdvancements:forGlyphs:count:';
    procedure getAdvancements_forPackedGlyphs_length(advancements: NSSizeArray; packedGlyphs: Pointer; length: culong); message 'getAdvancements:forPackedGlyphs:length:';
    procedure set_; message 'set';
    procedure setInContext(graphicsContext: NSGraphicsContext); message 'setInContext:';
    function printerFont: NSFont; message 'printerFont';
    function screenFont: NSFont; message 'screenFont';
    function screenFontWithRenderingMode(renderingMode_: NSFontRenderingMode): NSFont; message 'screenFontWithRenderingMode:';
    function renderingMode: NSFontRenderingMode; message 'renderingMode';

    { Category: NSFontDeprecated }
    class procedure useFont(fontName_: NSString); message 'useFont:';
    function widthOfString(string_: NSString): CGFloat; message 'widthOfString:';
    function glyphIsEncoded(aGlyph: NSGlyph): Boolean; message 'glyphIsEncoded:';
    class procedure setPreferredFontNames(fontNameArray: NSArray); message 'setPreferredFontNames:';
    function positionOfGlyph_precededByGlyph_isNominal(curGlyph: NSGlyph; prevGlyph: NSGlyph; var nominal: Boolean): NSPoint; message 'positionOfGlyph:precededByGlyph:isNominal:';
    function positionsForCompositeSequence_numberOfGlyphs_pointArray(var someGlyphs: NSGlyph; numGlyphs: clong; points: NSPointArray): clong; message 'positionsForCompositeSequence:numberOfGlyphs:pointArray:';
    function positionOfGlyph_struckOverGlyph_metricsExist(curGlyph: NSGlyph; prevGlyph: NSGlyph; var exist: Boolean): NSPoint; message 'positionOfGlyph:struckOverGlyph:metricsExist:';
    function positionOfGlyph_struckOverRect_metricsExist(aGlyph: NSGlyph; aRect: NSRect; var exist: Boolean): NSPoint; message 'positionOfGlyph:struckOverRect:metricsExist:';
    function positionOfGlyph_forCharacter_struckOverRect(aGlyph: NSGlyph; aChar: unichar; aRect: NSRect): NSPoint; message 'positionOfGlyph:forCharacter:struckOverRect:';
    function positionOfGlyph_withRelation_toBaseGlyph_totalAdvancement_metricsExist(thisGlyph: NSGlyph; rel: NSGlyphRelation; baseGlyph: NSGlyph; adv: NSSizePointer; var exist: Boolean): NSPoint; message 'positionOfGlyph:withRelation:toBaseGlyph:totalAdvancement:metricsExist:';
  end; external;

{$endif}
{$endif}

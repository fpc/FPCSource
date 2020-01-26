{ Parsed from AppKit.framework NSSimpleHorizontalTypesetter.h }


{$ifdef TYPES}
type
  NSSimpleHorizontalTypesetterPtr = ^NSSimpleHorizontalTypesetter;
{$endif}

{$if defined(__LP64__) or (MAC_OS_X_VERSION_MIN_REQUIRED > MAC_OS_X_VERSION_10_4)}

{$else}
{$ifdef TYPES}
const
  NSLayoutNotDone = 0;
  NSLayoutDone = 1;
  NSLayoutCantFit = 2;
  NSLayoutOutOfGlyphs = 3;
type
  NSLayoutStatus = clong;
const
  NSGlyphLayoutAtAPoint = 0;
  NSGlyphLayoutAgainstAPoint = 1;
  NSGlyphLayoutWithPrevious = 2;
type
  NSGlyphLayoutMode = clong;
const
  NSLayoutLeftToRight = 0;
  NSLayoutRightToLeft = 1;
type
  NSLayoutDirection = clong;

const
  NSBaselineNotSet = -1.0;
  NumGlyphsToGetEachTime = 20;

type
  _NSPositionOfGlyphMethod = function (obj: NSFont; sel: SEL; cur: NSGlyph; prev: NSGlyph; isNominal: pObjCBOOL): NSPoint; cdecl;

type
  _NSTypesetterGlyphInfo = bitpacked record
    case byte of
      0: (_anonBitField__NSTypesetterGlyphInfo0: cuint);
      1: (
        curLocation: NSPoint;
        extent: single;
        belowBaseline: single;
        aboveBaseline: single;
        glyphCharacterIndex: cuint;
        font: NSFont;
        attachmentSize: NSSize;
        _giflags: bitpacked record
          case byte of
            0: (_anonBitField__giflags0: cuint);
            1: (
              defaultPositioning: 0..1;
              dontShow: 0..1;
              isAttachment: 0..1;
            );
          end;
      );
    end;
type
  NSTypesetterGlyphInfo = _NSTypesetterGlyphInfo;
type
  NSTypesetterGlyphInfoPtr = ^_NSTypesetterGlyphInfo;
type
  _NSTypesetterGlyphInfoPtr = ^_NSTypesetterGlyphInfo;
{$endif}

{$ifdef CLASSES}

type
  NSSimpleHorizontalTypesetter = objcclass external (NSTypesetter)
  private
    layoutManager: NSLayoutManager;
    textStorage: NSTextStorage;
    firstGlyphIndex: cuint;
    curGlyphIndex: cuint;
    firstInvalidGlyphIndex: cuint;
    glyphs: NSTypesetterGlyphInfoPtr;
    glyphCache: NSGlyphPtr;
    glyphInscriptionCache: NSGlyphInscriptionPtr;
    glyphCharacterIndexCache: pcuint;
    glyphElasticCache: pObjCBOOL;
    glyphLocationOffset: NSSize;
    curMaxGlyphLocation: single;
    lastFixedGlyphIndex: cuint;
    sizeOfGlyphInfo: cuint;
    curGlyph: NSGlyph;
    curGlyphInscription: NSGlyphInscription;
    curCharacterIndex: cuint;
    previousGlyph: cuint;
    previousBaseGlyphIndex: cuint;
    previousBaseGlyph: cuint;
    previousFont: NSFont;
    curGlyphOffset: single;
    curGlyphOffsetOutOfDate: ObjCBOOL;
    curGlyphIsAControlGlyph: ObjCBOOL;
    containerBreakAfterCurGlyph: ObjCBOOL;
    wrapAfterCurGlyph: ObjCBOOL;
    curSpaceAfter: single;
    previousSpaceAfter: single;
    glyphLayoutMode: NSGlyphLayoutMode;
    curGlyphExtentAboveLocation: single;
    curGlyphExtentBelowLocation: single;
    curLayoutDirection: NSLayoutDirection;
    curTextAlignment: NSTextAlignment;
    curFont: NSFont;
    curFontBoundingBox: NSRect;
    curFontIsFixedPitch: ObjCBOOL;
    curFontAdvancement: NSPoint;
    curFontPositionOfGlyphMethod: _NSPositionOfGlyphMethod;
    attrs: NSDictionary;
    attrsRange: NSRange;
    curBaselineOffset: single;
    curMinBaselineDistance: single;
    curMaxBaselineDistance: single;
    curSuperscript: cint;
    curParaStyle: NSParagraphStyle;
    curContainer: NSTextContainer;
    curContainerIndex: cuint;
    curContainerLineFragmentPadding: single;
    curContainerIsSimpleRectangular: ObjCBOOL;
    curContainerSize: NSSize;
    curMinLineHeight: single;
    curMaxLineHeight: single;
    textString: NSString;
    capacityOfGlyphs: cuint;
    busy: ObjCBOOL;
    _tsFlags: bitpacked record
      case byte of
        0: (_anonBitField__tsFlags0: cuint);
        1: (
          _glyphPostLay: 0..1;
          _fragPostLay: 0..1;
          _useItal: 0..1;
          _curFontIsDefaultFace: 0..1;
          _tabState: 0..((1 shl 2)-1);
          _tabType: 0..((1 shl 2)-1);
          _tabEOL: 0..1;
          reserved: 0..((1 shl 23)-1);
        );
      end;
    glyphBidiLevelCache: PChar;
    curBidiLevel: char;
    previousBidiLevel: char;
    _reservedChars:array[0..1] of char;
    _reserved2:array[0..5] of cuint;
  public
    class function sharedInstance: id; message 'sharedInstance';
    function currentLayoutManager: NSLayoutManager; message 'currentLayoutManager';
    function currentContainer: NSTextContainer; message 'currentContainer';
    function currentParagraphStyle_: NSParagraphStyle; message 'currentParagraphStyle';
    function currentTextStorage: NSTextStorage; message 'currentTextStorage';
    function baseOfTypesetterGlyphInfo: NSTypesetterGlyphInfoPtr; message 'baseOfTypesetterGlyphInfo';
    function sizeOfTypesetterGlyphInfo: NSUInteger; message 'sizeOfTypesetterGlyphInfo';
    function capacityOfTypesetterGlyphInfo: NSUInteger; message 'capacityOfTypesetterGlyphInfo';
    function firstGlyphIndexOfCurrentLineFragment: NSUInteger; message 'firstGlyphIndexOfCurrentLineFragment';
    procedure layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndex (layoutManager_: NSLayoutManager; startGlyphIndex: NSUInteger; maxNumLines: NSUInteger; nextGlyph: NSUIntegerPtr); message 'layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:';
    function layoutGlyphsInHorizontalLineFragment_baseline (lineFragmentRect: NSRectPtr; baseline: psingle): NSLayoutStatus; message 'layoutGlyphsInHorizontalLineFragment:baseline:';
    procedure clearGlyphCache; message 'clearGlyphCache';
    procedure fillAttributesCache; message 'fillAttributesCache';
    procedure clearAttributesCache; message 'clearAttributesCache';
    function growGlyphCaches_fillGlyphInfo (desiredCapacity: NSUInteger; fillGlyphInfo: ObjCBOOL): NSUInteger; message 'growGlyphCaches:fillGlyphInfo:';
    procedure updateCurGlyphOffset; message 'updateCurGlyphOffset';
    procedure insertGlyph_atGlyphIndex_characterIndex (glyph: NSGlyph; glyphIndex: NSUInteger; charIndex: NSUInteger); message 'insertGlyph:atGlyphIndex:characterIndex:';
    function layoutControlGlyphForLineFragment (lineFrag: NSRect): NSLayoutStatus; message 'layoutControlGlyphForLineFragment:';
    procedure layoutTab; message 'layoutTab';
    procedure breakLineAtIndex (location: NSUInteger); message 'breakLineAtIndex:';
    function glyphIndexToBreakLineByHyphenatingWordAtIndex (charIndex: NSUInteger): NSUInteger; message 'glyphIndexToBreakLineByHyphenatingWordAtIndex:';
    function glyphIndexToBreakLineByWordWrappingAtIndex (charIndex: NSUInteger): NSUInteger; message 'glyphIndexToBreakLineByWordWrappingAtIndex:';
    procedure layoutTab_; message 'layoutTab';
    function layoutControlGlyphForLineFragment_ (lineFrag: NSRect): NSLayoutStatus; message 'layoutControlGlyphForLineFragment:';
    procedure fullJustifyLineAtGlyphIndex (glyphIndexForLineBreak: NSUInteger); message 'fullJustifyLineAtGlyphIndex:';
  end;


type
  NSTypesetterSubclassExtensions = objccategory external (NSSimpleHorizontalTypesetter)
    procedure typesetterLaidOneGlyph (gl: NSTypesetterGlyphInfoPtr); message 'typesetterLaidOneGlyph:';
    procedure willSetLineFragmentRect_forGlyphRange_usedRect (aRect: NSRectPtr; aRange: NSRange; bRect: NSRectPtr); message 'willSetLineFragmentRect:forGlyphRange:usedRect:';
  end;
{$endif}

{$endif}

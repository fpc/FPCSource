{ Parsed from AppKit.framework NSLayoutManager.h }


{$ifdef TYPES}
type
  NSLayoutManagerPtr = ^NSLayoutManager;
  NSTextLayoutOrientationProviderProtocolPtr = ^NSTextLayoutOrientationProviderProtocol;
  NSLayoutManagerDelegateProtocolPtr = ^NSLayoutManagerDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  NSGlyphAttributeSoft = 0;
  NSGlyphAttributeElastic = 1;
  NSGlyphAttributeBidiLevel = 2;
  NSGlyphAttributeInscribe = 5;

type
  NSGlyphInscription = NSUInteger;
  NSGlyphInscriptionPtr = ^NSGlyphInscription;

const
  NSGlyphInscribeBase = 0;
  NSGlyphInscribeBelow = 1;
  NSGlyphInscribeAbove = 2;
  NSGlyphInscribeOverstrike = 3;
  NSGlyphInscribeOverBelow = 4;

type
  NSTypesetterBehavior = NSInteger;
  NSTypesetterBehaviorPtr = ^NSTypesetterBehavior;

const
  NSTypesetterLatestBehavior = -1;
  NSTypesetterOriginalBehavior = 0;
  NSTypesetterBehavior_10_2_WithCompatibility = 1;
  NSTypesetterBehavior_10_2 = 2;
  NSTypesetterBehavior_10_3 = 3;
  NSTypesetterBehavior_10_4 = 4;

type
  NSTextLayoutOrientation = NSInteger;
  NSTextLayoutOrientationPtr = ^NSTextLayoutOrientation;

const
  NSTextLayoutOrientationHorizontal = 0;
  NSTextLayoutOrientationVertical = 1;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTextLayoutOrientationProviderProtocol = objcprotocol external name 'NSTextLayoutOrientationProvider'
    function layoutOrientation: NSTextLayoutOrientation; message 'layoutOrientation'; { available in 10_7 }
  end;
{$endif}

{$ifdef CLASSES}

type
  NSLayoutManager = objcclass external (NSObject, NSCodingProtocol, NSGlyphStorageProtocol)
  private
    _textStorage: NSTextStorage;
    _glyphGenerator: NSGlyphGenerator;
    _typesetter: NSTypesetter;
    _textContainers: NSMutableArray;
    _containerUsedRects: NSStorage;
    _glyphs: NSStorage;
    _containerRuns: NSRunStorage;
    _fragmentRuns: NSRunStorage;
    _glyphLocations: NSRunStorage;
    _glyphRotationRuns: NSRunStorage;
    _extraLineFragmentRect: NSRect;
    _extraLineFragmentUsedRect: NSRect;
    _extraLineFragmentContainer: NSTextContainer;
    _glyphHoles: NSSortedArray;
    _layoutHoles: NSSortedArray;
    _lmFlags: bitpacked record
      case byte of
        0: (_anonBitField__lmFlags0: cuint);
        1: (
          containersAreFull: 0..1;
          glyphsMightDrawOutsideLines: 0..1;
          backgroundLayoutEnabled: 0..1;
          resizingInProgress: 0..1;
          allowScreenFonts: 0..1;
          cachedRectArrayInUse: 0..1;
          displayInvalidationInProgress: 0..1;
          insertionPointNeedsUpdate: 0..1;
          layoutManagerInDirtyList: 0..1;
          originalFontOverride: 0..1;
          showInvisibleCharacters: 0..1;
          showControlCharacters: 0..1;
          delegateRespondsToDidInvalidate: 0..1;
          delegateRespondsToDidComplete: 0..1;
          glyphFormat: 0..((1 shl 2)-1);
          textStorageRespondsToIsEditing: 0..1;
          notifyEditedInProgress: 0..1;
          containersChanged: 0..1;
          isGeneratingGlyphs: 0..1;
          hasNonGeneratedGlyphData: 0..1;
          inBackgroundLayout: 0..1;
          syncAlignmentToDirection: 0..1;
          defaultAttachmentScaling: 0..((1 shl 2)-1);
          usesFontLeading: 0..1;
          seenRightToLeft: 0..1;
          ignoresViewTransformations: 0..1;
          needToFlushGlyph: 0..1;
          flipsIfNeeded: 0..1;
          allowNonContig: 0..1;
          useNonContig: 0..1;
        );
      end;
    _delegate: id;
    _textViewResizeDisableStack: cushort;
    _displayInvalidationDisableStack: cushort;
    _deferredDisplayCharRange: NSRange;
    _firstTextView: NSTextView;
    _cachedRectArray: NSRectPtr;
    _cachedRectArrayCapacity: NSUInteger;
    _glyphBuffer: PChar;
    _glyphBufferSize: NSUInteger;
    _cachedLocationNominalGlyphRange: NSRange;
    _cachedLocationGlyphIndex: NSUInteger;
    _cachedLocation: NSPoint;
    _cachedFontCharRange: NSRange;
    _cachedFont: NSFont;
    _firstUnlaidGlyphIndex: NSUInteger;
    _firstUnlaidCharIndex: NSUInteger;
    _rulerAccView: NSBox;
    _rulerAccViewAlignmentButtons: id;
    _rulerAccViewSpacing: id;
    _rulerAccViewLeftTabWell: NSTabWell;
    _rulerAccViewRightTabWell: NSTabWell;
    _rulerAccViewCenterTabWell: NSTabWell;
    _rulerAccViewDecimalTabWell: NSTabWell;
    _rulerAccViewStyles: id;
    _rulerAccViewLists: id;
    _newlyFilledGlyphRange: NSRange;
    _extraData: id;
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    procedure setTextStorage(newValue: NSTextStorage); message 'setTextStorage:';
    function textStorage: NSTextStorage; message 'textStorage';
    function attributedString: NSAttributedString; message 'attributedString';
    procedure replaceTextStorage (newTextStorage: NSTextStorage); message 'replaceTextStorage:';
    procedure setGlyphGenerator(newValue: NSGlyphGenerator); message 'setGlyphGenerator:';
    function glyphGenerator: NSGlyphGenerator; message 'glyphGenerator';
    procedure setTypesetter(newValue: NSTypesetter); message 'setTypesetter:';
    function typesetter: NSTypesetter; message 'typesetter';
    procedure setDelegate(newValue: NSLayoutManagerDelegateProtocol); message 'setDelegate:';
    function delegate: NSLayoutManagerDelegateProtocol; message 'delegate';
    function textContainers: NSArray; message 'textContainers';
    procedure addTextContainer (container: NSTextContainer); message 'addTextContainer:';
    procedure insertTextContainer_atIndex (container: NSTextContainer; index: NSUInteger); message 'insertTextContainer:atIndex:';
    procedure removeTextContainerAtIndex (index: NSUInteger); message 'removeTextContainerAtIndex:';
    procedure textContainerChangedGeometry (container: NSTextContainer); message 'textContainerChangedGeometry:';
    procedure textContainerChangedTextView (container: NSTextContainer); message 'textContainerChangedTextView:';
    procedure setBackgroundLayoutEnabled(newValue: ObjCBOOL); message 'setBackgroundLayoutEnabled:';
    function backgroundLayoutEnabled: ObjCBOOL; message 'backgroundLayoutEnabled';
    procedure setUsesScreenFonts(newValue: ObjCBOOL); message 'setUsesScreenFonts:';
    function usesScreenFonts: ObjCBOOL; message 'usesScreenFonts';
    procedure setShowsInvisibleCharacters(newValue: ObjCBOOL); message 'setShowsInvisibleCharacters:';
    function showsInvisibleCharacters: ObjCBOOL; message 'showsInvisibleCharacters';
    procedure setShowsControlCharacters(newValue: ObjCBOOL); message 'setShowsControlCharacters:';
    function showsControlCharacters: ObjCBOOL; message 'showsControlCharacters';
    procedure setHyphenationFactor(newValue: single); message 'setHyphenationFactor:';
    function hyphenationFactor: single; message 'hyphenationFactor';
    procedure setDefaultAttachmentScaling(newValue: NSImageScaling); message 'setDefaultAttachmentScaling:';
    function defaultAttachmentScaling: NSImageScaling; message 'defaultAttachmentScaling';
    procedure setTypesetterBehavior(newValue: NSTypesetterBehavior); message 'setTypesetterBehavior:';
    function typesetterBehavior: NSTypesetterBehavior; message 'typesetterBehavior';
    function layoutOptions: NSUInteger; message 'layoutOptions';
    procedure setAllowsNonContiguousLayout(newValue: ObjCBOOL); message 'setAllowsNonContiguousLayout:';
    function allowsNonContiguousLayout: ObjCBOOL; message 'allowsNonContiguousLayout';
    function hasNonContiguousLayout: ObjCBOOL; message 'hasNonContiguousLayout';
    procedure invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRange (charRange: NSRange; delta: NSInteger; actualCharRange: NSRangePointer); message 'invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:';
    procedure invalidateLayoutForCharacterRange_actualCharacterRange (charRange: NSRange; actualCharRange: NSRangePointer); message 'invalidateLayoutForCharacterRange:actualCharacterRange:'; { available in 10_5 }
    procedure invalidateLayoutForCharacterRange_isSoft_actualCharacterRange (charRange: NSRange; flag: ObjCBOOL; actualCharRange: NSRangePointer); message 'invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:';
    procedure invalidateDisplayForCharacterRange (charRange: NSRange); message 'invalidateDisplayForCharacterRange:';
    procedure invalidateDisplayForGlyphRange (glyphRange: NSRange); message 'invalidateDisplayForGlyphRange:';
    procedure textStorage_edited_range_changeInLength_invalidatedRange (str: NSTextStorage; editedMask: NSTextStorageEditedOptions; newCharRange: NSRange; delta: NSInteger; invalidatedCharRange: NSRange); message 'textStorage:edited:range:changeInLength:invalidatedRange:';
    procedure ensureGlyphsForCharacterRange (charRange: NSRange); message 'ensureGlyphsForCharacterRange:'; { available in 10_5 }
    procedure ensureGlyphsForGlyphRange (glyphRange: NSRange); message 'ensureGlyphsForGlyphRange:'; { available in 10_5 }
    procedure ensureLayoutForCharacterRange (charRange: NSRange); message 'ensureLayoutForCharacterRange:'; { available in 10_5 }
    procedure ensureLayoutForGlyphRange (glyphRange: NSRange); message 'ensureLayoutForGlyphRange:'; { available in 10_5 }
    procedure ensureLayoutForTextContainer (container: NSTextContainer); message 'ensureLayoutForTextContainer:'; { available in 10_5 }
    procedure ensureLayoutForBoundingRect_inTextContainer (bounds: NSRect; container: NSTextContainer); message 'ensureLayoutForBoundingRect:inTextContainer:'; { available in 10_5 }
    procedure insertGlyphs_length_forStartingGlyphAtIndex_characterIndex (glyphs: NSGlyphPtr; length: NSUInteger; glyphIndex: NSUInteger; charIndex: NSUInteger); message 'insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:';
    procedure insertGlyph_atGlyphIndex_characterIndex (glyph: NSGlyph; glyphIndex: NSUInteger; charIndex: NSUInteger); message 'insertGlyph:atGlyphIndex:characterIndex:';
    procedure replaceGlyphAtIndex_withGlyph (glyphIndex: NSUInteger; newGlyph: NSGlyph); message 'replaceGlyphAtIndex:withGlyph:';
    procedure deleteGlyphsInRange (glyphRange: NSRange); message 'deleteGlyphsInRange:';
    procedure setCharacterIndex_forGlyphAtIndex (charIndex: NSUInteger; glyphIndex: NSUInteger); message 'setCharacterIndex:forGlyphAtIndex:';
    procedure setIntAttribute_value_forGlyphAtIndex (attributeTag: NSInteger; val: NSInteger; glyphIndex: NSUInteger); message 'setIntAttribute:value:forGlyphAtIndex:';
    procedure invalidateGlyphsOnLayoutInvalidationForGlyphRange (glyphRange: NSRange); message 'invalidateGlyphsOnLayoutInvalidationForGlyphRange:'; { available in 10_5 }
    function numberOfGlyphs: NSUInteger; message 'numberOfGlyphs';
    function glyphAtIndex_isValidIndex (glyphIndex: NSUInteger; isValidIndex: pObjCBOOL): NSGlyph; message 'glyphAtIndex:isValidIndex:';
    function glyphAtIndex (glyphIndex: NSUInteger): NSGlyph; message 'glyphAtIndex:';
    function isValidGlyphIndex (glyphIndex: NSUInteger): ObjCBOOL; message 'isValidGlyphIndex:';
    function characterIndexForGlyphAtIndex (glyphIndex: NSUInteger): NSUInteger; message 'characterIndexForGlyphAtIndex:';
    function glyphIndexForCharacterAtIndex (charIndex: NSUInteger): NSUInteger; message 'glyphIndexForCharacterAtIndex:'; { available in 10_5 }
    function intAttribute_forGlyphAtIndex (attributeTag: NSInteger; glyphIndex: NSUInteger): NSInteger; message 'intAttribute:forGlyphAtIndex:';
    function getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits (glyphRange: NSRange; glyphBuffer: NSGlyphPtr; charIndexBuffer: NSUIntegerPtr; inscribeBuffer: NSGlyphInscriptionPtr; elasticBuffer: pObjCBOOL): NSUInteger; message 'getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:';
    function getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels (glyphRange: NSRange; glyphBuffer: NSGlyphPtr; charIndexBuffer: NSUIntegerPtr; inscribeBuffer: NSGlyphInscriptionPtr; elasticBuffer: pObjCBOOL; bidiLevelBuffer: PChar): NSUInteger; message 'getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:';
    function getGlyphs_range (glyphArray: NSGlyphPtr; glyphRange: NSRange): NSUInteger; message 'getGlyphs:range:';
    procedure setTextContainer_forGlyphRange (container: NSTextContainer; glyphRange: NSRange); message 'setTextContainer:forGlyphRange:';
    procedure setLineFragmentRect_forGlyphRange_usedRect (fragmentRect: NSRect; glyphRange: NSRange; usedRect: NSRect); message 'setLineFragmentRect:forGlyphRange:usedRect:';
    procedure setExtraLineFragmentRect_usedRect_textContainer (fragmentRect: NSRect; usedRect: NSRect; container: NSTextContainer); message 'setExtraLineFragmentRect:usedRect:textContainer:';
    procedure setLocation_forStartOfGlyphRange (location: NSPoint; glyphRange: NSRange); message 'setLocation:forStartOfGlyphRange:';
    procedure setLocations_startingGlyphIndexes_count_forGlyphRange (locations: NSPointArray; glyphIndexes: NSUIntegerPtr; count: NSUInteger; glyphRange: NSRange); message 'setLocations:startingGlyphIndexes:count:forGlyphRange:'; { available in 10_5 }
    procedure setNotShownAttribute_forGlyphAtIndex (flag: ObjCBOOL; glyphIndex: NSUInteger); message 'setNotShownAttribute:forGlyphAtIndex:';
    procedure setDrawsOutsideLineFragment_forGlyphAtIndex (flag: ObjCBOOL; glyphIndex: NSUInteger); message 'setDrawsOutsideLineFragment:forGlyphAtIndex:';
    procedure setAttachmentSize_forGlyphRange (attachmentSize: NSSize; glyphRange: NSRange); message 'setAttachmentSize:forGlyphRange:';
    procedure getFirstUnlaidCharacterIndex_glyphIndex (charIndex: NSUIntegerPtr; glyphIndex: NSUIntegerPtr); message 'getFirstUnlaidCharacterIndex:glyphIndex:';
    function firstUnlaidCharacterIndex: NSUInteger; message 'firstUnlaidCharacterIndex';
    function firstUnlaidGlyphIndex: NSUInteger; message 'firstUnlaidGlyphIndex';
    function textContainerForGlyphAtIndex_effectiveRange (glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSTextContainer; message 'textContainerForGlyphAtIndex:effectiveRange:';
    function usedRectForTextContainer (container: NSTextContainer): NSRect; message 'usedRectForTextContainer:';
    function lineFragmentRectForGlyphAtIndex_effectiveRange (glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'lineFragmentRectForGlyphAtIndex:effectiveRange:';
    function lineFragmentUsedRectForGlyphAtIndex_effectiveRange (glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'lineFragmentUsedRectForGlyphAtIndex:effectiveRange:';
    function lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout (glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer; flag: ObjCBOOL): NSRect; message 'lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:';
    function lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout (glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer; flag: ObjCBOOL): NSRect; message 'lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:';
    function textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayout (glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer; flag: ObjCBOOL): NSTextContainer; message 'textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:';
    function extraLineFragmentRect: NSRect; message 'extraLineFragmentRect';
    function extraLineFragmentUsedRect: NSRect; message 'extraLineFragmentUsedRect';
    function extraLineFragmentTextContainer: NSTextContainer; message 'extraLineFragmentTextContainer';
    function locationForGlyphAtIndex (glyphIndex: NSUInteger): NSPoint; message 'locationForGlyphAtIndex:';
    function notShownAttributeForGlyphAtIndex (glyphIndex: NSUInteger): ObjCBOOL; message 'notShownAttributeForGlyphAtIndex:';
    function drawsOutsideLineFragmentForGlyphAtIndex (glyphIndex: NSUInteger): ObjCBOOL; message 'drawsOutsideLineFragmentForGlyphAtIndex:';
    function attachmentSizeForGlyphAtIndex (glyphIndex: NSUInteger): NSSize; message 'attachmentSizeForGlyphAtIndex:';
    procedure setLayoutRect_forTextBlock_glyphRange (rect: NSRect; block: NSTextBlock; glyphRange: NSRange); message 'setLayoutRect:forTextBlock:glyphRange:';
    procedure setBoundsRect_forTextBlock_glyphRange (rect: NSRect; block: NSTextBlock; glyphRange: NSRange); message 'setBoundsRect:forTextBlock:glyphRange:';
    function layoutRectForTextBlock_glyphRange (block: NSTextBlock; glyphRange: NSRange): NSRect; message 'layoutRectForTextBlock:glyphRange:';
    function boundsRectForTextBlock_glyphRange (block: NSTextBlock; glyphRange: NSRange): NSRect; message 'boundsRectForTextBlock:glyphRange:';
    function layoutRectForTextBlock_atIndex_effectiveRange (block: NSTextBlock; glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'layoutRectForTextBlock:atIndex:effectiveRange:';
    function boundsRectForTextBlock_atIndex_effectiveRange (block: NSTextBlock; glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'boundsRectForTextBlock:atIndex:effectiveRange:';
    function glyphRangeForCharacterRange_actualCharacterRange (charRange: NSRange; actualCharRange: NSRangePointer): NSRange; message 'glyphRangeForCharacterRange:actualCharacterRange:';
    function characterRangeForGlyphRange_actualGlyphRange (glyphRange: NSRange; actualGlyphRange: NSRangePointer): NSRange; message 'characterRangeForGlyphRange:actualGlyphRange:';
    function glyphRangeForTextContainer (container: NSTextContainer): NSRange; message 'glyphRangeForTextContainer:';
    function rangeOfNominallySpacedGlyphsContainingIndex (glyphIndex: NSUInteger): NSRange; message 'rangeOfNominallySpacedGlyphsContainingIndex:';
    function rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount (charRange: NSRange; selCharRange: NSRange; container: NSTextContainer; rectCount: NSUIntegerPtr): NSRectArray; message 'rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:';
    function rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount (glyphRange: NSRange; selGlyphRange: NSRange; container: NSTextContainer; rectCount: NSUIntegerPtr): NSRectArray; message 'rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:';
    function boundingRectForGlyphRange_inTextContainer (glyphRange: NSRange; container: NSTextContainer): NSRect; message 'boundingRectForGlyphRange:inTextContainer:';
    function glyphRangeForBoundingRect_inTextContainer (bounds: NSRect; container: NSTextContainer): NSRange; message 'glyphRangeForBoundingRect:inTextContainer:';
    function glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainer (bounds: NSRect; container: NSTextContainer): NSRange; message 'glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:';
    function glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph (point: NSPoint; container: NSTextContainer; partialFraction: CGFloatPtr): NSUInteger; message 'glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:';
    function glyphIndexForPoint_inTextContainer (point: NSPoint; container: NSTextContainer): NSUInteger; message 'glyphIndexForPoint:inTextContainer:';
    function fractionOfDistanceThroughGlyphForPoint_inTextContainer (point: NSPoint; container: NSTextContainer): CGFloat; message 'fractionOfDistanceThroughGlyphForPoint:inTextContainer:';
    function characterIndexForPoint_inTextContainer_fractionOfDistanceBetweenInsertionPoints (point: NSPoint; container: NSTextContainer; partialFraction: CGFloatPtr): NSUInteger; message 'characterIndexForPoint:inTextContainer:fractionOfDistanceBetweenInsertionPoints:'; { available in 10_6 }
    function getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexes (charIndex: NSUInteger; aFlag: ObjCBOOL; dFlag: ObjCBOOL; positions: CGFloatPtr; charIndexes: NSUIntegerPtr): NSUInteger; message 'getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:'; { available in 10_5 }
    function temporaryAttributesAtCharacterIndex_effectiveRange (charIndex: NSUInteger; effectiveCharRange: NSRangePointer): NSDictionary; message 'temporaryAttributesAtCharacterIndex:effectiveRange:';
    procedure setTemporaryAttributes_forCharacterRange (attrs: NSDictionary; charRange: NSRange); message 'setTemporaryAttributes:forCharacterRange:';
    procedure addTemporaryAttributes_forCharacterRange (attrs: NSDictionary; charRange: NSRange); message 'addTemporaryAttributes:forCharacterRange:';
    procedure removeTemporaryAttribute_forCharacterRange (attrName: NSString; charRange: NSRange); message 'removeTemporaryAttribute:forCharacterRange:';
    function temporaryAttribute_atCharacterIndex_effectiveRange (attrName: NSString; location: NSUInteger; range: NSRangePointer): id; message 'temporaryAttribute:atCharacterIndex:effectiveRange:'; { available in 10_5 }
    function temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRange (attrName: NSString; location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): id; message 'temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:'; { available in 10_5 }
    function temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRange (location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): NSDictionary; message 'temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:'; { available in 10_5 }
    procedure addTemporaryAttribute_value_forCharacterRange (attrName: NSString; value: id; charRange: NSRange); message 'addTemporaryAttribute:value:forCharacterRange:'; { available in 10_5 }
    function substituteFontForFont (originalFont: NSFont): NSFont; message 'substituteFontForFont:';
    function defaultLineHeightForFont (theFont: NSFont): CGFloat; message 'defaultLineHeightForFont:';
    function defaultBaselineOffsetForFont (theFont: NSFont): CGFloat; message 'defaultBaselineOffsetForFont:';
    procedure setUsesFontLeading(newValue: ObjCBOOL); message 'setUsesFontLeading:';
    function usesFontLeading: ObjCBOOL; message 'usesFontLeading';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSTextViewSupport = objccategory external (NSLayoutManager)
    function rulerMarkersForTextView_paragraphStyle_ruler (view: NSTextView; style: NSParagraphStyle; ruler: NSRulerView): NSArray; message 'rulerMarkersForTextView:paragraphStyle:ruler:';
    function rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled (view: NSTextView; style: NSParagraphStyle; ruler: NSRulerView; isEnabled: ObjCBOOL): NSView; message 'rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:';
    function layoutManagerOwnsFirstResponderInWindow (window: NSWindow): ObjCBOOL; message 'layoutManagerOwnsFirstResponderInWindow:';
    function firstTextView: NSTextView; message 'firstTextView';
    function textViewForBeginningOfSelection: NSTextView; message 'textViewForBeginningOfSelection';
    procedure drawBackgroundForGlyphRange_atPoint (glyphsToShow: NSRange; origin: NSPoint); message 'drawBackgroundForGlyphRange:atPoint:';
    procedure drawGlyphsForGlyphRange_atPoint (glyphsToShow: NSRange; origin: NSPoint); message 'drawGlyphsForGlyphRange:atPoint:';
    procedure showCGGlyphs_positions_count_font_matrix_attributes_inContext (glyphs: CGGlyphPtr; positions: NSPointPtr; glyphCount: NSUInteger; font: NSFont; textMatrix: NSAffineTransform; attributes: NSDictionary; graphicsContext: NSGraphicsContext); message 'showCGGlyphs:positions:count:font:matrix:attributes:inContext:'; { available in 10_7 }
    procedure showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment (glyphs: PChar; glyphLen: NSUInteger; glyphRange: NSRange; point: NSPoint; font: NSFont; color: NSColor; printingAdjustment: NSSize); message 'showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:'; deprecated 'in 10_0, 10_7';
    procedure showAttachmentCell_inRect_characterIndex (cell: NSCell; rect: NSRect; attachmentIndex: NSUInteger); message 'showAttachmentCell:inRect:characterIndex:';
    procedure fillBackgroundRectArray_count_forCharacterRange_color (rectArray: NSRectArray; rectCount: NSUInteger; charRange: NSRange; color: NSColor); message 'fillBackgroundRectArray:count:forCharacterRange:color:'; { available in 10_6 }
    procedure drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin (glyphRange: NSRange; underlineVal: NSInteger; baselineOffset: CGFloat; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
    procedure underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin (glyphRange: NSRange; underlineVal: NSInteger; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
    procedure drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin (glyphRange: NSRange; strikethroughVal: NSInteger; baselineOffset: CGFloat; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
    procedure strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin (glyphRange: NSRange; strikethroughVal: NSInteger; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSLayoutManagerDelegateProtocol = objcprotocol external name 'NSLayoutManagerDelegate' (NSObjectProtocol)
  optional
    procedure layoutManagerDidInvalidateLayout (sender: NSLayoutManager); message 'layoutManagerDidInvalidateLayout:';
    procedure layoutManager_didCompleteLayoutForTextContainer_atEnd (layoutManager: NSLayoutManager; textContainer: NSTextContainer; layoutFinishedFlag: ObjCBOOL); message 'layoutManager:didCompleteLayoutForTextContainer:atEnd:';
    function layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange (layoutManager: NSLayoutManager; attrs: NSDictionary; toScreen: ObjCBOOL; charIndex: NSUInteger; effectiveCharRange: NSRangePointer): NSDictionary; message 'layoutManager:shouldUseTemporaryAttributes:forDrawingToScreen:atCharacterIndex:effectiveRange:'; { available in 10_5 }
  end;
{$endif}


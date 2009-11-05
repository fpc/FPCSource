{ Parsed from Appkit.framework NSLayoutManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSLAYOUTMANAGER_PAS_T}
{$define NSLAYOUTMANAGER_PAS_T}

{ Constants }

const
  NSGlyphAttributeSoft = 0;
  NSGlyphAttributeElastic = 1;
  NSGlyphAttributeBidiLevel = 2;
  NSGlyphAttributeInscribe = 5;

const
  NSGlyphInscribeBase = 0;
  NSGlyphInscribeBelow = 1;
  NSGlyphInscribeAbove = 2;
  NSGlyphInscribeOverstrike = 3;
  NSGlyphInscribeOverBelow = 4;

const
  NSTypesetterLatestBehavior = -1;
  NSTypesetterOriginalBehavior = 0;
  NSTypesetterBehavior_10_2_WithCompatibility = 1;
  NSTypesetterBehavior_10_2 = 2;
  NSTypesetterBehavior_10_3 = 3;
  NSTypesetterBehavior_10_4 = 4;

{ Types }
type
  NSGlyphInscription = NSUInteger;
  NSTypesetterBehavior = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSLAYOUTMANAGER_PAS_R}
{$define NSLAYOUTMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSLAYOUTMANAGER_PAS_F}
{$define NSLAYOUTMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSLAYOUTMANAGER_PAS_S}
{$define NSLAYOUTMANAGER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLayoutManager = objcclass;
  NSLayoutManagerPointer = ^NSLayoutManager;

{$endif}

{$ifdef CLASSES}
{$ifndef NSLAYOUTMANAGER_PAS_C}
{$define NSLAYOUTMANAGER_PAS_C}

{ NSLayoutManager }
  NSLayoutManager = objcclass(NSObject, NSCodingProtocol)
  private
    __textStorage: NSTextStorage;
    __glyphGenerator: NSGlyphGenerator;
    __typesetter: NSTypesetter;
    __textContainers: NSMutableArray;
    __containerUsedRects: NSStorage;
    __glyphs: NSStorage;
    __containerRuns: NSRunStorage;
    __fragmentRuns: NSRunStorage;
    __glyphLocations: NSRunStorage;
    __glyphRotationRuns: NSRunStorage;
    __extraLineFragmentRect: NSRect;
    __extraLineFragmentUsedRect: NSRect;
    __extraLineFragmentContainer: NSTextContainer;
    __glyphHoles: NSSortedArray;
    __layoutHoles: NSSortedArray;
    __lmFlags: bitpacked record
        containersAreFull: 0..1;
        glyphsMightDrawOutsideLines: 0..1;
        backgroundLayoutEnabled: 0..1;
        resizingInProgress: 0..1;
        allowScreenFonts: 0..1;
        cachedRectArrayInUse: 0..1;
        displayInvalidationInProgress: 0..1;
        insertionPointNeedsUpdate: 0..1;
        layoutManagerInDirtyList: 0..1;
        usingGlyphCache: 0..1;
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
        loggedBGLayoutException: 0..1;
        isLayoutRequestedFromSubthread: 0..1;
        defaultAttachmentScaling: 0..((1 shl 2)-1);
        isInUILayoutMode: 0..1;
        seenRightToLeft: 0..1;
        ignoresViewTransformations: 0..1;
        needToFlushGlyph: 0..1;
        flipsIfNeeded: 0..1;
        allowNonContig: 0..1;
        useNonContig: 0..1;
      end;
    __delegate: id;
    __textViewResizeDisableStack: cushort;
    __displayInvalidationDisableStack: cushort;
    __deferredDisplayCharRange: NSRange;
    __firstTextView: NSTextView;
    __cachedRectArray: NSRect; {garbage collector: __strong }
    __cachedRectArrayCapacity: NSUInteger;
    __glyphBuffer: char; {garbage collector: __strong }
    __glyphBufferSize: NSUInteger;
    __cachedLocationNominalGlyphRange: NSRange;
    __cachedLocationGlyphIndex: NSUInteger;
    __cachedLocation: NSPoint;
    __cachedFontCharRange: NSRange;
    __cachedFont: NSFont;
    __firstUnlaidGlyphIndex: NSUInteger;
    __firstUnlaidCharIndex: NSUInteger;
    __rulerAccView: NSBox;
    __rulerAccViewAlignmentButtons: id;
    __rulerAccViewSpacing: id;
    __rulerAccViewLeftTabWell: NSTabWell;
    __rulerAccViewRightTabWell: NSTabWell;
    __rulerAccViewCenterTabWell: NSTabWell;
    __rulerAccViewDecimalTabWell: NSTabWell;
    __rulerAccViewStyles: id;
    __rulerAccViewLists: id;
    __newlyFilledGlyphRange: NSRange;
    __extraData: id;
    
  public
    class function alloc: NSLayoutManager; message 'alloc';

    function init: id; message 'init';
    function textStorage: NSTextStorage; message 'textStorage';
    procedure setTextStorage(textStorage_: NSTextStorage); message 'setTextStorage:';
    function attributedString: NSAttributedString; message 'attributedString';
    procedure replaceTextStorage(newTextStorage: NSTextStorage); message 'replaceTextStorage:';
    function glyphGenerator: NSGlyphGenerator; message 'glyphGenerator';
    procedure setGlyphGenerator(glyphGenerator_: NSGlyphGenerator); message 'setGlyphGenerator:';
    function typesetter: NSTypesetter; message 'typesetter';
    procedure setTypesetter(typesetter_: NSTypesetter); message 'setTypesetter:';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function textContainers: NSArray; message 'textContainers';
    procedure addTextContainer(container: NSTextContainer); message 'addTextContainer:';
    procedure insertTextContainer_atIndex(container: NSTextContainer; index: NSUInteger); message 'insertTextContainer:atIndex:';
    procedure removeTextContainerAtIndex(index: NSUInteger); message 'removeTextContainerAtIndex:';
    procedure textContainerChangedGeometry(container: NSTextContainer); message 'textContainerChangedGeometry:';
    procedure textContainerChangedTextView(container: NSTextContainer); message 'textContainerChangedTextView:';
    procedure setBackgroundLayoutEnabled(flag: Boolean); message 'setBackgroundLayoutEnabled:';
    function backgroundLayoutEnabled: Boolean; message 'backgroundLayoutEnabled';
    procedure setUsesScreenFonts(flag: Boolean); message 'setUsesScreenFonts:';
    function usesScreenFonts: Boolean; message 'usesScreenFonts';
    procedure setShowsInvisibleCharacters(flag: Boolean); message 'setShowsInvisibleCharacters:';
    function showsInvisibleCharacters: Boolean; message 'showsInvisibleCharacters';
    procedure setShowsControlCharacters(flag: Boolean); message 'setShowsControlCharacters:';
    function showsControlCharacters: Boolean; message 'showsControlCharacters';
    procedure setHyphenationFactor(factor: single); message 'setHyphenationFactor:';
    function hyphenationFactor: single; message 'hyphenationFactor';
    procedure setDefaultAttachmentScaling(scaling: NSImageScaling); message 'setDefaultAttachmentScaling:';
    function defaultAttachmentScaling: NSImageScaling; message 'defaultAttachmentScaling';
    procedure setTypesetterBehavior(theBehavior: NSTypesetterBehavior); message 'setTypesetterBehavior:';
    function typesetterBehavior: NSTypesetterBehavior; message 'typesetterBehavior';
    function layoutOptions: NSUInteger; message 'layoutOptions';
    procedure setAllowsNonContiguousLayout(flag: Boolean); message 'setAllowsNonContiguousLayout:';
    function allowsNonContiguousLayout: Boolean; message 'allowsNonContiguousLayout';
    function hasNonContiguousLayout: Boolean; message 'hasNonContiguousLayout';
    procedure invalidateGlyphsForCharacterRange_changeInLength_actualCharacterRange(charRange: NSRange; delta: NSInteger; actualCharRange: NSRangePointer); message 'invalidateGlyphsForCharacterRange:changeInLength:actualCharacterRange:';
    procedure invalidateLayoutForCharacterRange_actualCharacterRange(charRange: NSRange; actualCharRange: NSRangePointer); message 'invalidateLayoutForCharacterRange:actualCharacterRange:';
    procedure invalidateLayoutForCharacterRange_isSoft_actualCharacterRange(charRange: NSRange; flag: Boolean; actualCharRange: NSRangePointer); message 'invalidateLayoutForCharacterRange:isSoft:actualCharacterRange:';
    procedure invalidateDisplayForCharacterRange(charRange: NSRange); message 'invalidateDisplayForCharacterRange:';
    procedure invalidateDisplayForGlyphRange(glyphRange: NSRange); message 'invalidateDisplayForGlyphRange:';
    procedure textStorage_edited_range_changeInLength_invalidatedRange(str: NSTextStorage; editedMask: NSUInteger; newCharRange: NSRange; delta: NSInteger; invalidatedCharRange: NSRange); message 'textStorage:edited:range:changeInLength:invalidatedRange:';
    procedure ensureGlyphsForCharacterRange(charRange: NSRange); message 'ensureGlyphsForCharacterRange:';
    procedure ensureGlyphsForGlyphRange(glyphRange: NSRange); message 'ensureGlyphsForGlyphRange:';
    procedure ensureLayoutForCharacterRange(charRange: NSRange); message 'ensureLayoutForCharacterRange:';
    procedure ensureLayoutForGlyphRange(glyphRange: NSRange); message 'ensureLayoutForGlyphRange:';
    procedure ensureLayoutForTextContainer(container: NSTextContainer); message 'ensureLayoutForTextContainer:';
    procedure ensureLayoutForBoundingRect_inTextContainer(bounds: NSRect; container: NSTextContainer); message 'ensureLayoutForBoundingRect:inTextContainer:';
    procedure insertGlyphs_length_forStartingGlyphAtIndex_characterIndex(glyphs: NSGlyphPointer; length: NSUInteger; glyphIndex: NSUInteger; charIndex: NSUInteger); message 'insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:';
    procedure insertGlyph_atGlyphIndex_characterIndex(glyph: NSGlyph; glyphIndex: NSUInteger; charIndex: NSUInteger); message 'insertGlyph:atGlyphIndex:characterIndex:';
    procedure replaceGlyphAtIndex_withGlyph(glyphIndex: NSUInteger; newGlyph: NSGlyph); message 'replaceGlyphAtIndex:withGlyph:';
    procedure deleteGlyphsInRange(glyphRange: NSRange); message 'deleteGlyphsInRange:';
    procedure setCharacterIndex_forGlyphAtIndex(charIndex: NSUInteger; glyphIndex: NSUInteger); message 'setCharacterIndex:forGlyphAtIndex:';
    procedure setIntAttribute_value_forGlyphAtIndex(attributeTag: NSInteger; val: NSInteger; glyphIndex: NSUInteger); message 'setIntAttribute:value:forGlyphAtIndex:';
    procedure invalidateGlyphsOnLayoutInvalidationForGlyphRange(glyphRange: NSRange); message 'invalidateGlyphsOnLayoutInvalidationForGlyphRange:';
    function numberOfGlyphs: NSUInteger; message 'numberOfGlyphs';
    function glyphAtIndex_isValidIndex(glyphIndex: NSUInteger; isValidIndex: pboolean): NSGlyph; message 'glyphAtIndex:isValidIndex:';
    function glyphAtIndex(glyphIndex: NSUInteger): NSGlyph; message 'glyphAtIndex:';
    function isValidGlyphIndex(glyphIndex: NSUInteger): Boolean; message 'isValidGlyphIndex:';
    function characterIndexForGlyphAtIndex(glyphIndex: NSUInteger): NSUInteger; message 'characterIndexForGlyphAtIndex:';
    function glyphIndexForCharacterAtIndex(charIndex: NSUInteger): NSUInteger; message 'glyphIndexForCharacterAtIndex:';
    function intAttribute_forGlyphAtIndex(attributeTag: NSInteger; glyphIndex: NSUInteger): NSInteger; message 'intAttribute:forGlyphAtIndex:';
    function getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits(glyphRange: NSRange; glyphBuffer: NSGlyphPointer; charIndexBuffer: NSUIntegerPointer; inscribeBuffer: NSGlyphInscriptionPointer; elasticBuffer: pboolean): NSUInteger; message 'getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:';
    function getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels(glyphRange: NSRange; glyphBuffer: NSGlyphPointer; charIndexBuffer: NSUIntegerPointer; inscribeBuffer: NSGlyphInscriptionPointer; elasticBuffer: pboolean; bidiLevelBuffer: Pointer): NSUInteger; message 'getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:';
    function getGlyphs_range(glyphArray: NSGlyphPointer; glyphRange: NSRange): NSUInteger; message 'getGlyphs:range:';
    procedure setTextContainer_forGlyphRange(container: NSTextContainer; glyphRange: NSRange); message 'setTextContainer:forGlyphRange:';
    procedure setLineFragmentRect_forGlyphRange_usedRect(fragmentRect: NSRect; glyphRange: NSRange; usedRect: NSRect); message 'setLineFragmentRect:forGlyphRange:usedRect:';
    procedure setExtraLineFragmentRect_usedRect_textContainer(fragmentRect: NSRect; usedRect: NSRect; container: NSTextContainer); message 'setExtraLineFragmentRect:usedRect:textContainer:';
    procedure setLocation_forStartOfGlyphRange(location: NSPoint; glyphRange: NSRange); message 'setLocation:forStartOfGlyphRange:';
    procedure setLocations_startingGlyphIndexes_count_forGlyphRange(locations: NSPointArray; glyphIndexes: NSUIntegerPointer; count: NSUInteger; glyphRange: NSRange); message 'setLocations:startingGlyphIndexes:count:forGlyphRange:';
    procedure setNotShownAttribute_forGlyphAtIndex(flag: Boolean; glyphIndex: NSUInteger); message 'setNotShownAttribute:forGlyphAtIndex:';
    procedure setDrawsOutsideLineFragment_forGlyphAtIndex(flag: Boolean; glyphIndex: NSUInteger); message 'setDrawsOutsideLineFragment:forGlyphAtIndex:';
    procedure setAttachmentSize_forGlyphRange(attachmentSize: NSSize; glyphRange: NSRange); message 'setAttachmentSize:forGlyphRange:';
    procedure getFirstUnlaidCharacterIndex_glyphIndex(charIndex: NSUIntegerPointer; glyphIndex: NSUIntegerPointer); message 'getFirstUnlaidCharacterIndex:glyphIndex:';
    function firstUnlaidCharacterIndex: NSUInteger; message 'firstUnlaidCharacterIndex';
    function firstUnlaidGlyphIndex: NSUInteger; message 'firstUnlaidGlyphIndex';
    function textContainerForGlyphAtIndex_effectiveRange(glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSTextContainer; message 'textContainerForGlyphAtIndex:effectiveRange:';
    function usedRectForTextContainer(container: NSTextContainer): NSRect; message 'usedRectForTextContainer:';
    function lineFragmentRectForGlyphAtIndex_effectiveRange(glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'lineFragmentRectForGlyphAtIndex:effectiveRange:';
    function lineFragmentUsedRectForGlyphAtIndex_effectiveRange(glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'lineFragmentUsedRectForGlyphAtIndex:effectiveRange:';
    function lineFragmentRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout(glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer; flag: Boolean): NSRect; message 'lineFragmentRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:';
    function lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout(glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer; flag: Boolean): NSRect; message 'lineFragmentUsedRectForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:';
    function textContainerForGlyphAtIndex_effectiveRange_withoutAdditionalLayout(glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer; flag: Boolean): NSTextContainer; message 'textContainerForGlyphAtIndex:effectiveRange:withoutAdditionalLayout:';
    function extraLineFragmentRect: NSRect; message 'extraLineFragmentRect';
    function extraLineFragmentUsedRect: NSRect; message 'extraLineFragmentUsedRect';
    function extraLineFragmentTextContainer: NSTextContainer; message 'extraLineFragmentTextContainer';
    function locationForGlyphAtIndex(glyphIndex: NSUInteger): NSPoint; message 'locationForGlyphAtIndex:';
    function notShownAttributeForGlyphAtIndex(glyphIndex: NSUInteger): Boolean; message 'notShownAttributeForGlyphAtIndex:';
    function drawsOutsideLineFragmentForGlyphAtIndex(glyphIndex: NSUInteger): Boolean; message 'drawsOutsideLineFragmentForGlyphAtIndex:';
    function attachmentSizeForGlyphAtIndex(glyphIndex: NSUInteger): NSSize; message 'attachmentSizeForGlyphAtIndex:';
    procedure setLayoutRect_forTextBlock_glyphRange(rect: NSRect; block: NSTextBlock; glyphRange: NSRange); message 'setLayoutRect:forTextBlock:glyphRange:';
    procedure setBoundsRect_forTextBlock_glyphRange(rect: NSRect; block: NSTextBlock; glyphRange: NSRange); message 'setBoundsRect:forTextBlock:glyphRange:';
    function layoutRectForTextBlock_glyphRange(block: NSTextBlock; glyphRange: NSRange): NSRect; message 'layoutRectForTextBlock:glyphRange:';
    function boundsRectForTextBlock_glyphRange(block: NSTextBlock; glyphRange: NSRange): NSRect; message 'boundsRectForTextBlock:glyphRange:';
    function layoutRectForTextBlock_atIndex_effectiveRange(block: NSTextBlock; glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'layoutRectForTextBlock:atIndex:effectiveRange:';
    function boundsRectForTextBlock_atIndex_effectiveRange(block: NSTextBlock; glyphIndex: NSUInteger; effectiveGlyphRange: NSRangePointer): NSRect; message 'boundsRectForTextBlock:atIndex:effectiveRange:';
    function glyphRangeForCharacterRange_actualCharacterRange(charRange: NSRange; actualCharRange: NSRangePointer): NSRange; message 'glyphRangeForCharacterRange:actualCharacterRange:';
    function characterRangeForGlyphRange_actualGlyphRange(glyphRange: NSRange; actualGlyphRange: NSRangePointer): NSRange; message 'characterRangeForGlyphRange:actualGlyphRange:';
    function glyphRangeForTextContainer(container: NSTextContainer): NSRange; message 'glyphRangeForTextContainer:';
    function rangeOfNominallySpacedGlyphsContainingIndex(glyphIndex: NSUInteger): NSRange; message 'rangeOfNominallySpacedGlyphsContainingIndex:';
    function rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount(charRange: NSRange; selCharRange: NSRange; container: NSTextContainer; rectCount: NSUIntegerPointer): NSRectArray; message 'rectArrayForCharacterRange:withinSelectedCharacterRange:inTextContainer:rectCount:';
    function rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount(glyphRange: NSRange; selGlyphRange: NSRange; container: NSTextContainer; rectCount: NSUIntegerPointer): NSRectArray; message 'rectArrayForGlyphRange:withinSelectedGlyphRange:inTextContainer:rectCount:';
    function boundingRectForGlyphRange_inTextContainer(glyphRange: NSRange; container: NSTextContainer): NSRect; message 'boundingRectForGlyphRange:inTextContainer:';
    function glyphRangeForBoundingRect_inTextContainer(bounds: NSRect; container: NSTextContainer): NSRange; message 'glyphRangeForBoundingRect:inTextContainer:';
    function glyphRangeForBoundingRectWithoutAdditionalLayout_inTextContainer(bounds: NSRect; container: NSTextContainer): NSRange; message 'glyphRangeForBoundingRectWithoutAdditionalLayout:inTextContainer:';
    function glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph(point: NSPoint; container: NSTextContainer; partialFraction: psingle): NSUInteger; message 'glyphIndexForPoint:inTextContainer:fractionOfDistanceThroughGlyph:';
    function glyphIndexForPoint_inTextContainer(point: NSPoint; container: NSTextContainer): NSUInteger; message 'glyphIndexForPoint:inTextContainer:';
    function fractionOfDistanceThroughGlyphForPoint_inTextContainer(point: NSPoint; container: NSTextContainer): CGFloat; message 'fractionOfDistanceThroughGlyphForPoint:inTextContainer:';
    function getLineFragmentInsertionPointsForCharacterAtIndex_alternatePositions_inDisplayOrder_positions_characterIndexes(charIndex: NSUInteger; aFlag: Boolean; dFlag: Boolean; positions: psingle; charIndexes: NSUIntegerPointer): NSUInteger; message 'getLineFragmentInsertionPointsForCharacterAtIndex:alternatePositions:inDisplayOrder:positions:characterIndexes:';
    function temporaryAttributesAtCharacterIndex_effectiveRange(charIndex: NSUInteger; effectiveCharRange: NSRangePointer): NSDictionary; message 'temporaryAttributesAtCharacterIndex:effectiveRange:';
    procedure setTemporaryAttributes_forCharacterRange(attrs: NSDictionary; charRange: NSRange); message 'setTemporaryAttributes:forCharacterRange:';
    procedure addTemporaryAttributes_forCharacterRange(attrs: NSDictionary; charRange: NSRange); message 'addTemporaryAttributes:forCharacterRange:';
    procedure removeTemporaryAttribute_forCharacterRange(attrName: NSString; charRange: NSRange); message 'removeTemporaryAttribute:forCharacterRange:';
    function temporaryAttribute_atCharacterIndex_effectiveRange(attrName: NSString; location: NSUInteger; range: NSRangePointer): id; message 'temporaryAttribute:atCharacterIndex:effectiveRange:';
    function temporaryAttribute_atCharacterIndex_longestEffectiveRange_inRange(attrName: NSString; location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): id; message 'temporaryAttribute:atCharacterIndex:longestEffectiveRange:inRange:';
    function temporaryAttributesAtCharacterIndex_longestEffectiveRange_inRange(location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): NSDictionary; message 'temporaryAttributesAtCharacterIndex:longestEffectiveRange:inRange:';
    procedure addTemporaryAttribute_value_forCharacterRange(attrName: NSString; value: id; charRange: NSRange); message 'addTemporaryAttribute:value:forCharacterRange:';
    function substituteFontForFont(originalFont: NSFont): NSFont; message 'substituteFontForFont:';
    function defaultLineHeightForFont(theFont: NSFont): CGFloat; message 'defaultLineHeightForFont:';
    function defaultBaselineOffsetForFont(theFont: NSFont): CGFloat; message 'defaultBaselineOffsetForFont:';
    function usesFontLeading: Boolean; message 'usesFontLeading';
    procedure setUsesFontLeading(flag: Boolean); message 'setUsesFontLeading:';

    { Category: NSTextViewSupport }
    function rulerMarkersForTextView_paragraphStyle_ruler(view: NSTextView; style: NSParagraphStyle; ruler: NSRulerView): NSArray; message 'rulerMarkersForTextView:paragraphStyle:ruler:';
    function rulerAccessoryViewForTextView_paragraphStyle_ruler_enabled(view: NSTextView; style: NSParagraphStyle; ruler: NSRulerView; isEnabled: Boolean): NSView; message 'rulerAccessoryViewForTextView:paragraphStyle:ruler:enabled:';
    function layoutManagerOwnsFirstResponderInWindow(window: NSWindow): Boolean; message 'layoutManagerOwnsFirstResponderInWindow:';
    function firstTextView: NSTextView; message 'firstTextView';
    function textViewForBeginningOfSelection: NSTextView; message 'textViewForBeginningOfSelection';
    procedure drawBackgroundForGlyphRange_atPoint(glyphsToShow: NSRange; origin: NSPoint); message 'drawBackgroundForGlyphRange:atPoint:';
    procedure drawGlyphsForGlyphRange_atPoint(glyphsToShow: NSRange; origin: NSPoint); message 'drawGlyphsForGlyphRange:atPoint:';
    procedure showPackedGlyphs_length_glyphRange_atPoint_font_color_printingAdjustment(glyphs: Pointer; glyphLen: NSUInteger; glyphRange: NSRange; point: NSPoint; font: NSFont; color: NSColor; printingAdjustment: NSSize); message 'showPackedGlyphs:length:glyphRange:atPoint:font:color:printingAdjustment:';
    procedure showAttachmentCell_inRect_characterIndex(cell: NSCell; rect: NSRect; attachmentIndex: NSUInteger); message 'showAttachmentCell:inRect:characterIndex:';
    procedure drawUnderlineForGlyphRange_underlineType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin(glyphRange: NSRange; underlineVal: NSInteger; baselineOffset: CGFloat; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'drawUnderlineForGlyphRange:underlineType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
    procedure underlineGlyphRange_underlineType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin(glyphRange: NSRange; underlineVal: NSInteger; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'underlineGlyphRange:underlineType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
    procedure drawStrikethroughForGlyphRange_strikethroughType_baselineOffset_lineFragmentRect_lineFragmentGlyphRange_containerOrigin(glyphRange: NSRange; strikethroughVal: NSInteger; baselineOffset: CGFloat; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'drawStrikethroughForGlyphRange:strikethroughType:baselineOffset:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
    procedure strikethroughGlyphRange_strikethroughType_lineFragmentRect_lineFragmentGlyphRange_containerOrigin(glyphRange: NSRange; strikethroughVal: NSInteger; lineRect: NSRect; lineGlyphRange: NSRange; containerOrigin: NSPoint); message 'strikethroughGlyphRange:strikethroughType:lineFragmentRect:lineFragmentGlyphRange:containerOrigin:';
  end; external;

{$endif}
{$endif}

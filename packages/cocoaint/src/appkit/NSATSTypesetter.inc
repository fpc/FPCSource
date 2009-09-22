{ Parsed from Appkit.framework NSATSTypesetter.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSATSTYPESETTER_PAS_H}
{$define NSATSTYPESETTER_PAS_H}
type
  NSATSTypesetterPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSATSTYPESETTER_PAS_T}
{$define NSATSTYPESETTER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSATSTYPESETTER_PAS_R}
{$define NSATSTYPESETTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSATSTYPESETTER_PAS_F}
{$define NSATSTYPESETTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSATSTYPESETTER_PAS_T}
{$define NSATSTYPESETTER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSATSTypesetter = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSATSTYPESETTER_PAS_C}
{$define NSATSTYPESETTER_PAS_C}

{ NSATSTypesetter }
  NSATSTypesetter = objcclass(NSTypesetter)
  private
    _attributedString: NSAttributedString;
    _paragraphGlyphRange: NSRange;
    _paragraphSeparatorGlyphRange: NSRange;
    _lineFragmentPadding: CGFloat;
    _layoutManager: NSLayoutManager;
    _textContainers: NSArray;
    _currentTextContainer: NSTextContainer;
    _currentTextContainerIndex: NSUInteger;
    _currentTextContainerSize: NSSize;
    _currentParagraphStyle: NSParagraphStyle;
        _atsReserved: array[0..7] of Pointer;
    __private: id;
    
  public
    class function alloc: NSATSTypesetter; message 'alloc';

    class function sharedTypesetter: id; message 'sharedTypesetter';

    { Category: NSPantherCompatibility }
    function lineFragmentRectForProposedRect_remainingRect(proposedRect: NSRect; remainingRect: NSRectPointer): NSRect; message 'lineFragmentRectForProposedRect:remainingRect:';

    { Category: NSPrimitiveInterface }
    function usesFontLeading: Boolean; message 'usesFontLeading';
    procedure setUsesFontLeading(flag: Boolean); message 'setUsesFontLeading:';
    function typesetterBehavior: NSTypesetterBehavior; message 'typesetterBehavior';
    procedure setTypesetterBehavior(behavior: NSTypesetterBehavior); message 'setTypesetterBehavior:';
    function hyphenationFactor: single; message 'hyphenationFactor';
    procedure setHyphenationFactor(factor: single); message 'setHyphenationFactor:';
    function lineFragmentPadding: CGFloat; message 'lineFragmentPadding';
    procedure setLineFragmentPadding(padding: CGFloat); message 'setLineFragmentPadding:';
    function substituteFontForFont(originalFont: NSFont): NSFont; message 'substituteFontForFont:';
    function textTabForGlyphLocation_writingDirection_maxLocation(glyphLocation: CGFloat; direction: NSWritingDirection; maxLocation: CGFloat): NSTextTab; message 'textTabForGlyphLocation:writingDirection:maxLocation:';
    function bidiProcessingEnabled: Boolean; message 'bidiProcessingEnabled';
    procedure setBidiProcessingEnabled(flag: Boolean); message 'setBidiProcessingEnabled:';
    procedure setAttributedString(attrString: NSAttributedString); message 'setAttributedString:';
    function attributedString: NSAttributedString; message 'attributedString';
    procedure setParagraphGlyphRange_separatorGlyphRange(paragraphRange: NSRange; paragraphSeparatorRange: NSRange); message 'setParagraphGlyphRange:separatorGlyphRange:';
    function paragraphGlyphRange: NSRange; message 'paragraphGlyphRange';
    function paragraphSeparatorGlyphRange: NSRange; message 'paragraphSeparatorGlyphRange';
    function layoutParagraphAtPoint(lineFragmentOrigin: NSPointPointer): NSUInteger; message 'layoutParagraphAtPoint:';
    function lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect(glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:';
    function paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect(glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:';
    function paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect(glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:';
    function layoutManager: NSLayoutManager; message 'layoutManager';
    function currentTextContainer: NSTextContainer; message 'currentTextContainer';
    procedure setHardInvalidation_forGlyphRange(flag: Boolean; glyphRange: NSRange); message 'setHardInvalidation:forGlyphRange:';
    procedure getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin(lineFragmentRect: NSRectPointer; lineFragmentUsedRect: NSRectPointer; paragraphSeparatorGlyphRange_: NSRange; lineOrigin: NSPoint); message 'getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:';

    { Category: NSLayoutPhaseInterface }
    procedure willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset(lineRect: NSRectPointer; glyphRange: NSRange; usedRect: NSRectPointer; baselineOffset: psingle); message 'willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:';
    function shouldBreakLineByWordBeforeCharacterAtIndex(charIndex: NSUInteger): Boolean; message 'shouldBreakLineByWordBeforeCharacterAtIndex:';
    function shouldBreakLineByHyphenatingBeforeCharacterAtIndex(charIndex: NSUInteger): Boolean; message 'shouldBreakLineByHyphenatingBeforeCharacterAtIndex:';
    function hyphenationFactorForGlyphAtIndex(glyphIndex: NSUInteger): single; message 'hyphenationFactorForGlyphAtIndex:';
    function hyphenCharacterForGlyphAtIndex(glyphIndex: NSUInteger): UTF32Char; message 'hyphenCharacterForGlyphAtIndex:';
    function boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex(glyphIndex: NSUInteger; textContainer: NSTextContainer; proposedRect: NSRect; glyphPosition: NSPoint; charIndex: NSUInteger): NSRect; message 'boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:';

    { Category: NSGlyphStorageInterface }
    function characterRangeForGlyphRange_actualGlyphRange(glyphRange: NSRange; actualGlyphRange: NSRangePointer): NSRange; message 'characterRangeForGlyphRange:actualGlyphRange:';
    function glyphRangeForCharacterRange_actualCharacterRange(charRange: NSRange; actualCharRange: NSRangePointer): NSRange; message 'glyphRangeForCharacterRange:actualCharacterRange:';
    function getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits(glyphsRange: NSRange; glyphBuffer: NSGlyphPointer; charIndexBuffer: NSUIntegerPointer; inscribeBuffer: NSGlyphInscriptionPointer; elasticBuffer: pboolean): NSUInteger; message 'getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:';
    procedure setLineFragmentRect_forGlyphRange_usedRect_baselineOffset(fragmentRect: NSRect; glyphRange: NSRange; usedRect: NSRect; baselineOffset: CGFloat); message 'setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:';
    procedure substituteGlyphsInRange_withGlyphs(glyphRange: NSRange; glyphs: NSGlyphPointer); message 'substituteGlyphsInRange:withGlyphs:';
    procedure insertGlyph_atGlyphIndex_characterIndex(glyph: NSGlyph; glyphIndex: NSUInteger; characterIndex: NSUInteger); message 'insertGlyph:atGlyphIndex:characterIndex:';
    procedure deleteGlyphsInRange(glyphRange: NSRange); message 'deleteGlyphsInRange:';
    procedure setNotShownAttribute_forGlyphRange(flag: Boolean; glyphRange: NSRange); message 'setNotShownAttribute:forGlyphRange:';
    procedure setDrawsOutsideLineFragment_forGlyphRange(flag: Boolean; glyphRange: NSRange); message 'setDrawsOutsideLineFragment:forGlyphRange:';
    procedure setLocation_withAdvancements_forStartOfGlyphRange(location: NSPoint; advancements: psingle; glyphRange: NSRange); message 'setLocation:withAdvancements:forStartOfGlyphRange:';
    procedure setAttachmentSize_forGlyphRange(attachmentSize: NSSize; glyphRange: NSRange); message 'setAttachmentSize:forGlyphRange:';
    procedure setBidiLevels_forGlyphRange(levels: pbyte; glyphRange: NSRange); message 'setBidiLevels:forGlyphRange:';
  end; external;

{$endif}
{$endif}

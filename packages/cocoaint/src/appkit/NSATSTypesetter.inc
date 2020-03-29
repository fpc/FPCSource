{ Parsed from AppKit.framework NSATSTypesetter.h }


{$ifdef TYPES}
type
  NSATSTypesetterPtr = ^NSATSTypesetter;
{$endif}

{$ifdef CLASSES}

type
  NSATSTypesetter = objcclass external (NSTypesetter)
  private
    attributedString_: NSAttributedString;
    paragraphGlyphRange_: NSRange;
    paragraphSeparatorGlyphRange_: NSRange;
    lineFragmentPadding_: CGFloat;
    layoutManager_: NSLayoutManager;
    textContainers_: NSArray;
    currentTextContainer_: NSTextContainer;
    currentTextContainerIndex: NSUInteger;
    currentTextContainerSize: NSSize;
    currentParagraphStyle_: NSParagraphStyle;
    _atsReserved:array[0..7] of pointer;
    _private: id;
  public
    class function sharedTypesetter: NSATSTypesetter; message 'sharedTypesetter';
  end;


type
  NSPantherCompatibility = objccategory external (NSATSTypesetter)
    function lineFragmentRectForProposedRect_remainingRect (proposedRect: NSRect; remainingRect: NSRectPointer): NSRect; message 'lineFragmentRectForProposedRect:remainingRect:'; deprecated 'in 10_3, 10_4';
  end;

type
  NSPrimitiveInterface = objccategory external (NSATSTypesetter)
    procedure setUsesFontLeading(newValue: ObjCBOOL); message 'setUsesFontLeading:';
    function usesFontLeading: ObjCBOOL; message 'usesFontLeading';
    procedure setTypesetterBehavior(newValue: NSTypesetterBehavior); message 'setTypesetterBehavior:';
    function typesetterBehavior: NSTypesetterBehavior; message 'typesetterBehavior';
    procedure setHyphenationFactor(newValue: single); message 'setHyphenationFactor:';
    function hyphenationFactor: single; message 'hyphenationFactor';
    procedure setLineFragmentPadding(newValue: CGFloat); message 'setLineFragmentPadding:';
    function lineFragmentPadding: CGFloat; message 'lineFragmentPadding';
    function substituteFontForFont (originalFont: NSFont): NSFont; message 'substituteFontForFont:';
    function textTabForGlyphLocation_writingDirection_maxLocation (glyphLocation: CGFloat; direction: NSWritingDirection; maxLocation: CGFloat): NSTextTab; message 'textTabForGlyphLocation:writingDirection:maxLocation:';
    procedure setBidiProcessingEnabled(newValue: ObjCBOOL); message 'setBidiProcessingEnabled:';
    function bidiProcessingEnabled: ObjCBOOL; message 'bidiProcessingEnabled';
    procedure setAttributedString(newValue: NSAttributedString); message 'setAttributedString:';
    function attributedString: NSAttributedString; message 'attributedString';
    procedure setParagraphGlyphRange_separatorGlyphRange (paragraphRange: NSRange; paragraphSeparatorRange: NSRange); message 'setParagraphGlyphRange:separatorGlyphRange:';
    function paragraphGlyphRange: NSRange; message 'paragraphGlyphRange';
    function paragraphSeparatorGlyphRange: NSRange; message 'paragraphSeparatorGlyphRange';
    function layoutParagraphAtPoint (lineFragmentOrigin: NSPointPtr): NSUInteger; message 'layoutParagraphAtPoint:';
    function lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect (glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:';
    function paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect (glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:';
    function paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect (glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:';
    function layoutManager: NSLayoutManager; message 'layoutManager';
    function currentTextContainer: NSTextContainer; message 'currentTextContainer';
    procedure setHardInvalidation_forGlyphRange (flag: ObjCBOOL; glyphRange: NSRange); message 'setHardInvalidation:forGlyphRange:';
    procedure getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin (lineFragmentRect: NSRectPtr; lineFragmentUsedRect: NSRectPtr; paragraphSeparatorGlyphRange__: NSRange; lineOrigin: NSPoint); message 'getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:';
  end;

type
  NSLayoutPhaseInterface = objccategory external (NSATSTypesetter)
    procedure willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset (lineRect: NSRectPtr; glyphRange: NSRange; usedRect: NSRectPtr; baselineOffset: CGFloatPtr); message 'willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:';
    function shouldBreakLineByWordBeforeCharacterAtIndex (charIndex: NSUInteger): ObjCBOOL; message 'shouldBreakLineByWordBeforeCharacterAtIndex:';
    function shouldBreakLineByHyphenatingBeforeCharacterAtIndex (charIndex: NSUInteger): ObjCBOOL; message 'shouldBreakLineByHyphenatingBeforeCharacterAtIndex:';
    function hyphenationFactorForGlyphAtIndex (glyphIndex: NSUInteger): single; message 'hyphenationFactorForGlyphAtIndex:';
    function hyphenCharacterForGlyphAtIndex (glyphIndex: NSUInteger): UTF32Char; message 'hyphenCharacterForGlyphAtIndex:';
    function boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex (glyphIndex: NSUInteger; textContainer: NSTextContainer; proposedRect: NSRect; glyphPosition: NSPoint; charIndex: NSUInteger): NSRect; message 'boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:';
  end;

type
  NSGlyphStorageInterface = objccategory external (NSATSTypesetter)
    function characterRangeForGlyphRange_actualGlyphRange (glyphRange: NSRange; actualGlyphRange: NSRangePointer): NSRange; message 'characterRangeForGlyphRange:actualGlyphRange:';
    function glyphRangeForCharacterRange_actualCharacterRange (charRange: NSRange; actualCharRange: NSRangePointer): NSRange; message 'glyphRangeForCharacterRange:actualCharacterRange:';
    function getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits (glyphsRange: NSRange; glyphBuffer: NSGlyphPtr; charIndexBuffer: NSUIntegerPtr; inscribeBuffer: NSGlyphInscriptionPtr; elasticBuffer: pObjCBOOL): NSUInteger; message 'getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:';
    procedure setLineFragmentRect_forGlyphRange_usedRect_baselineOffset (fragmentRect: NSRect; glyphRange: NSRange; usedRect: NSRect; baselineOffset: CGFloat); message 'setLineFragmentRect:forGlyphRange:usedRect:baselineOffset:';
    procedure substituteGlyphsInRange_withGlyphs (glyphRange: NSRange; glyphs: NSGlyphPtr); message 'substituteGlyphsInRange:withGlyphs:';
    procedure insertGlyph_atGlyphIndex_characterIndex (glyph: NSGlyph; glyphIndex: NSUInteger; characterIndex: NSUInteger); message 'insertGlyph:atGlyphIndex:characterIndex:';
    procedure deleteGlyphsInRange (glyphRange: NSRange); message 'deleteGlyphsInRange:';
    procedure setNotShownAttribute_forGlyphRange (flag: ObjCBOOL; glyphRange: NSRange); message 'setNotShownAttribute:forGlyphRange:';
    procedure setDrawsOutsideLineFragment_forGlyphRange (flag: ObjCBOOL; glyphRange: NSRange); message 'setDrawsOutsideLineFragment:forGlyphRange:';
    procedure setLocation_withAdvancements_forStartOfGlyphRange (location: NSPoint; advancements: CGFloatPtr; glyphRange: NSRange); message 'setLocation:withAdvancements:forStartOfGlyphRange:';
    procedure setAttachmentSize_forGlyphRange (attachmentSize: NSSize; glyphRange: NSRange); message 'setAttachmentSize:forGlyphRange:';
    procedure setBidiLevels_forGlyphRange (levels: pbyte; glyphRange: NSRange); message 'setBidiLevels:forGlyphRange:';
  end;
{$endif}


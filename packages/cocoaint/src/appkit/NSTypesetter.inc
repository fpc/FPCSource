{ Parsed from AppKit.framework NSTypesetter.h }


{$ifdef TYPES}
type
  NSTypesetterPtr = ^NSTypesetter;
{$endif}

{$ifdef TYPES}
type
  NSTypesetterControlCharacterAction = NSUInteger;
  NSTypesetterControlCharacterActionPtr = ^NSTypesetterControlCharacterAction;

const
  NSTypesetterZeroAdvancementAction = 1 shl 0;
  NSTypesetterWhitespaceAction = 1 shl 1;
  NSTypesetterHorizontalTabAction = 1 shl 2;
  NSTypesetterLineBreakAction = 1 shl 3;
  NSTypesetterParagraphBreakAction = 1 shl 4;
  NSTypesetterContainerBreakAction = 1 shl 5;
{$endif}

{$ifdef CLASSES}

type
  NSTypesetter = objcclass external (NSObject)
  private
    {$if defined(__LP64__)}
    _reserved: pointer;
    {$else}
    _reserved_:array[0..1] of cuint;
    {$endif}
  public
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
    function paragraphCharacterRange: NSRange; message 'paragraphCharacterRange';
    function paragraphSeparatorCharacterRange: NSRange; message 'paragraphSeparatorCharacterRange';
    function layoutParagraphAtPoint (lineFragmentOrigin: NSPointPointer): NSUInteger; message 'layoutParagraphAtPoint:';
    procedure beginParagraph; message 'beginParagraph';
    procedure endParagraph; message 'endParagraph';
    procedure beginLineWithGlyphAtIndex (glyphIndex: NSUInteger); message 'beginLineWithGlyphAtIndex:';
    procedure endLineWithGlyphRange (lineGlyphRange: NSRange); message 'endLineWithGlyphRange:';
    function lineSpacingAfterGlyphAtIndex_withProposedLineFragmentRect (glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'lineSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:';
    function paragraphSpacingBeforeGlyphAtIndex_withProposedLineFragmentRect (glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'paragraphSpacingBeforeGlyphAtIndex:withProposedLineFragmentRect:';
    function paragraphSpacingAfterGlyphAtIndex_withProposedLineFragmentRect (glyphIndex: NSUInteger; rect: NSRect): CGFloat; message 'paragraphSpacingAfterGlyphAtIndex:withProposedLineFragmentRect:';
    procedure getLineFragmentRect_usedRect_forParagraphSeparatorGlyphRange_atProposedOrigin (lineFragmentRect: NSRectPointer; lineFragmentUsedRect: NSRectPointer; paragraphSeparatorGlyphRange_: NSRange; lineOrigin: NSPoint); message 'getLineFragmentRect:usedRect:forParagraphSeparatorGlyphRange:atProposedOrigin:';
    function attributesForExtraLineFragment: NSDictionary; message 'attributesForExtraLineFragment';
    function actionForControlCharacterAtIndex (charIndex: NSUInteger): NSTypesetterControlCharacterAction; message 'actionForControlCharacterAtIndex:';
    function layoutManager: NSLayoutManager; message 'layoutManager';
    function textContainers: NSArray; message 'textContainers';
    function currentTextContainer: NSTextContainer; message 'currentTextContainer';
    function currentParagraphStyle: NSParagraphStyle; message 'currentParagraphStyle';
    procedure setHardInvalidation_forGlyphRange (flag: ObjCBOOL; glyphRange: NSRange); message 'setHardInvalidation:forGlyphRange:';
    procedure layoutGlyphsInLayoutManager_startingAtGlyphIndex_maxNumberOfLineFragments_nextGlyphIndex (layoutManager_: NSLayoutManager; startGlyphIndex: NSUInteger; maxNumLines: NSUInteger; nextGlyph: NSUIntegerPtr); message 'layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex:';
    function layoutCharactersInRange_forLayoutManager_maximumNumberOfLineFragments (characterRange: NSRange; layoutManager_: NSLayoutManager; maxNumLines: NSUInteger): NSRange; message 'layoutCharactersInRange:forLayoutManager:maximumNumberOfLineFragments:'; { available in 10_5 }
    class function printingAdjustmentInLayoutManager_forNominallySpacedGlyphRange_packedGlyphs_count (layoutMgr: NSLayoutManager; nominallySpacedGlyphsRange: NSRange; packedGlyphs: PChar; packedGlyphsCount: NSUInteger): NSSize; message 'printingAdjustmentInLayoutManager:forNominallySpacedGlyphRange:packedGlyphs:count:';
    function baselineOffsetInLayoutManager_glyphIndex (layoutMgr: NSLayoutManager; glyphIndex: NSUInteger): CGFloat; message 'baselineOffsetInLayoutManager:glyphIndex:';
    class function sharedSystemTypesetter: id; message 'sharedSystemTypesetter';
    class function sharedSystemTypesetterForBehavior (theBehavior: NSTypesetterBehavior): id; message 'sharedSystemTypesetterForBehavior:';
    class function defaultTypesetterBehavior: NSTypesetterBehavior; message 'defaultTypesetterBehavior';
  end;


type
  NSTypesetter_NSLayoutPhaseInterface = objccategory external name 'NSLayoutPhaseInterface' (NSTypesetter)
    procedure willSetLineFragmentRect_forGlyphRange_usedRect_baselineOffset (lineRect: NSRectPointer; glyphRange: NSRange; usedRect: NSRectPointer; baselineOffset: CGFloatPtr); message 'willSetLineFragmentRect:forGlyphRange:usedRect:baselineOffset:';
    function shouldBreakLineByWordBeforeCharacterAtIndex (charIndex: NSUInteger): ObjCBOOL; message 'shouldBreakLineByWordBeforeCharacterAtIndex:';
    function shouldBreakLineByHyphenatingBeforeCharacterAtIndex (charIndex: NSUInteger): ObjCBOOL; message 'shouldBreakLineByHyphenatingBeforeCharacterAtIndex:';
    function hyphenationFactorForGlyphAtIndex (glyphIndex: NSUInteger): single; message 'hyphenationFactorForGlyphAtIndex:';
    function hyphenCharacterForGlyphAtIndex (glyphIndex: NSUInteger): UTF32Char; message 'hyphenCharacterForGlyphAtIndex:';
    function boundingBoxForControlGlyphAtIndex_forTextContainer_proposedLineFragment_glyphPosition_characterIndex (glyphIndex: NSUInteger; textContainer: NSTextContainer; proposedRect: NSRect; glyphPosition: NSPoint; charIndex: NSUInteger): NSRect; message 'boundingBoxForControlGlyphAtIndex:forTextContainer:proposedLineFragment:glyphPosition:characterIndex:';
  end;

type
  NSTypesetter_NSGlyphStorageInterface = objccategory external name 'NSGlyphStorageInterface' (NSTypesetter)
    function characterRangeForGlyphRange_actualGlyphRange (glyphRange: NSRange; actualGlyphRange: NSRangePointer): NSRange; message 'characterRangeForGlyphRange:actualGlyphRange:';
    function glyphRangeForCharacterRange_actualCharacterRange (charRange: NSRange; actualCharRange: NSRangePointer): NSRange; message 'glyphRangeForCharacterRange:actualCharacterRange:';
    function getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels (glyphsRange: NSRange; glyphBuffer: NSGlyphPtr; charIndexBuffer: NSUIntegerPtr; inscribeBuffer: NSGlyphInscriptionPtr; elasticBuffer: pObjCBOOL; bidiLevelBuffer: PChar): NSUInteger; message 'getGlyphsInRange:glyphs:characterIndexes:glyphInscriptions:elasticBits:bidiLevels:';
    procedure getLineFragmentRect_usedRect_remainingRect_forStartingGlyphAtIndex_proposedRect_lineSpacing_paragraphSpacingBefore_paragraphSpacingAfter (lineFragmentRect: NSRectPointer; lineFragmentUsedRect: NSRectPointer; remainingRect: NSRectPointer; startingGlyphIndex: NSUInteger; proposedRect: NSRect; lineSpacing: CGFloat; paragraphSpacingBefore: CGFloat; paragraphSpacingAfter: CGFloat); message 'getLineFragmentRect:usedRect:remainingRect:forStartingGlyphAtIndex:proposedRect:lineSpacing:paragraphSpacingBefore:paragraphSpacingAfter:';
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


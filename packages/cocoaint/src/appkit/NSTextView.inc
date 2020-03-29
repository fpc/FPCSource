{ Parsed from AppKit.framework NSTextView.h }


{$ifdef TYPES}
type
  NSTextViewPtr = ^NSTextView;
  NSTextViewDelegateProtocolPtr = ^NSTextViewDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSSelectionGranularity = NSUInteger;
  NSSelectionGranularityPtr = ^NSSelectionGranularity;

const
  NSSelectByCharacter = 0;
  NSSelectByWord = 1;
  NSSelectByParagraph = 2;

type
  NSSelectionAffinity = NSUInteger;
  NSSelectionAffinityPtr = ^NSSelectionAffinity;

const
  NSSelectionAffinityUpstream = 0;
  NSSelectionAffinityDownstream = 1;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAllRomanInputSourcesLocaleIdentifier: NSString { available in 10_5 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSTextView = objcclass external (NSText, NSUserInterfaceValidationsProtocol, NSTextInputClientProtocol, NSTextLayoutOrientationProviderProtocol, NSDraggingSourceProtocol, NSTextInputProtocol, NSAccessibilityNavigableStaticTextProtocol)
  public
    function initWithFrame_textContainer (frameRect: NSRect; container: NSTextContainer): instancetype; message 'initWithFrame:textContainer:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function initWithFrame (frameRect: NSRect): instancetype; message 'initWithFrame:';
    procedure setTextContainer(newValue: NSTextContainer); message 'setTextContainer:';
    function textContainer: NSTextContainer; message 'textContainer';
    procedure replaceTextContainer (newContainer: NSTextContainer); message 'replaceTextContainer:';
    procedure setTextContainerInset(newValue: NSSize); message 'setTextContainerInset:';
    function textContainerInset: NSSize; message 'textContainerInset';
    function textContainerOrigin: NSPoint; message 'textContainerOrigin';
    procedure invalidateTextContainerOrigin; message 'invalidateTextContainerOrigin';
    function layoutManager: NSLayoutManager; message 'layoutManager';
    function textStorage: NSTextStorage; message 'textStorage';
    procedure insertText (insertString: id); message 'insertText:';
    procedure setConstrainedFrameSize (desiredSize: NSSize); message 'setConstrainedFrameSize:';
    procedure setAlignment_range (alignment_: NSTextAlignment; range: NSRange); message 'setAlignment:range:';
    procedure setBaseWritingDirection_range (writingDirection: NSWritingDirection; range: NSRange); message 'setBaseWritingDirection:range:';
    procedure turnOffKerning (sender: id); message 'turnOffKerning:';
    procedure tightenKerning (sender: id); message 'tightenKerning:';
    procedure loosenKerning (sender: id); message 'loosenKerning:';
    procedure useStandardKerning (sender: id); message 'useStandardKerning:';
    procedure turnOffLigatures (sender: id); message 'turnOffLigatures:';
    procedure useStandardLigatures (sender: id); message 'useStandardLigatures:';
    procedure useAllLigatures (sender: id); message 'useAllLigatures:';
    procedure raiseBaseline (sender: id); message 'raiseBaseline:';
    procedure lowerBaseline (sender: id); message 'lowerBaseline:';
    procedure toggleTraditionalCharacterShape (sender: id); message 'toggleTraditionalCharacterShape:';
    procedure outline (sender: id); message 'outline:';
    procedure performFindPanelAction (sender: id); message 'performFindPanelAction:';
    procedure alignJustified (sender: id); message 'alignJustified:';
    procedure changeColor (sender: id); message 'changeColor:';
    procedure changeAttributes (sender: id); message 'changeAttributes:';
    procedure changeDocumentBackgroundColor (sender: id); message 'changeDocumentBackgroundColor:';
    procedure orderFrontSpacingPanel (sender: id); message 'orderFrontSpacingPanel:';
    procedure orderFrontLinkPanel (sender: id); message 'orderFrontLinkPanel:';
    procedure orderFrontListPanel (sender: id); message 'orderFrontListPanel:';
    procedure orderFrontTablePanel (sender: id); message 'orderFrontTablePanel:';
    procedure rulerView_didMoveMarker (ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didMoveMarker:';
    procedure rulerView_didRemoveMarker (ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didRemoveMarker:';
    procedure rulerView_didAddMarker (ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didAddMarker:';
    function rulerView_shouldMoveMarker (ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldMoveMarker:';
    function rulerView_shouldAddMarker (ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldAddMarker:';
    function rulerView_willMoveMarker_toLocation (ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willMoveMarker:toLocation:';
    function rulerView_shouldRemoveMarker (ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldRemoveMarker:';
    function rulerView_willAddMarker_atLocation (ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willAddMarker:atLocation:';
    procedure rulerView_handleMouseDown (ruler: NSRulerView; event: NSEvent); message 'rulerView:handleMouseDown:';
    procedure setNeedsDisplayInRect_avoidAdditionalLayout (rect: NSRect; flag: ObjCBOOL); message 'setNeedsDisplayInRect:avoidAdditionalLayout:';
    function shouldDrawInsertionPoint: ObjCBOOL; message 'shouldDrawInsertionPoint';
    procedure drawInsertionPointInRect_color_turnedOn (rect: NSRect; color: NSColor; flag: ObjCBOOL); message 'drawInsertionPointInRect:color:turnedOn:';
    procedure drawViewBackgroundInRect (rect: NSRect); message 'drawViewBackgroundInRect:';
    procedure updateRuler; message 'updateRuler';
    procedure updateFontPanel; message 'updateFontPanel';
    procedure updateDragTypeRegistration; message 'updateDragTypeRegistration';
    function selectionRangeForProposedRange_granularity (proposedCharRange: NSRange; granularity: NSSelectionGranularity): NSRange; message 'selectionRangeForProposedRange:granularity:';
    procedure clickedOnLink_atIndex (link: id; charIndex: NSUInteger); message 'clickedOnLink:atIndex:';
    procedure startSpeaking (sender: id); message 'startSpeaking:';
    procedure stopSpeaking (sender: id); message 'stopSpeaking:';
    procedure setLayoutOrientation (theOrientation: NSTextLayoutOrientation); message 'setLayoutOrientation:'; { available in 10_7 }
    procedure changeLayoutOrientation (sender: id); message 'changeLayoutOrientation:'; { available in 10_7 }
    function characterIndexForInsertionAtPoint (point: NSPoint): NSUInteger; message 'characterIndexForInsertionAtPoint:'; { available in 10_5 }

    { Adopted protocols }
    function attributedString: NSAttributedString; message 'attributedString';
    function attributedSubstringForProposedRange_actualRange (aRange: NSRange; actualRange: NSRangePointer): NSAttributedString; message 'attributedSubstringForProposedRange:actualRange:';
    function attributedSubstringFromRange (theRange: NSRange): NSAttributedString; message 'attributedSubstringFromRange:'; deprecated 'in 10_0, 10_6';
    function baselineDeltaForCharacterAtIndex (anIndex: NSUInteger): CGFloat; message 'baselineDeltaForCharacterAtIndex:';
    function characterIndexForPoint (aPoint: NSPoint): NSUInteger; message 'characterIndexForPoint:';
    function conversationIdentifier: NSInteger; message 'conversationIdentifier'; deprecated 'in 10_0, 10_6';
    procedure doCommandBySelector (aSelector: SEL); message 'doCommandBySelector:';
    procedure draggingSession_endedAtPoint_operation (session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'draggingSession:endedAtPoint:operation:';
    procedure draggingSession_movedToPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:movedToPoint:';
    function draggingSession_sourceOperationMaskForDraggingContext (session: NSDraggingSession; context: NSDraggingContext): NSDragOperation; message 'draggingSession:sourceOperationMaskForDraggingContext:';
    procedure draggingSession_willBeginAtPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:willBeginAtPoint:';
    function drawsVerticallyForCharacterAtIndex (charIndex: NSUInteger): ObjCBOOL; message 'drawsVerticallyForCharacterAtIndex:'; { available in 10_6 }
    function firstRectForCharacterRange (theRange: NSRange): NSRect; message 'firstRectForCharacterRange:'; deprecated 'in 10_0, 10_6';
    function firstRectForCharacterRange_actualRange (aRange: NSRange; actualRange: NSRangePointer): NSRect; message 'firstRectForCharacterRange:actualRange:';
    function fractionOfDistanceThroughGlyphForPoint (aPoint: NSPoint): CGFloat; message 'fractionOfDistanceThroughGlyphForPoint:';
    function hasMarkedText: ObjCBOOL; message 'hasMarkedText';
    function ignoreModifierKeysForDraggingSession (session: NSDraggingSession): ObjCBOOL; message 'ignoreModifierKeysForDraggingSession:';
    procedure insertText_replacementRange (aString: id; replacementRange: NSRange); message 'insertText:replacementRange:';
    function layoutOrientation: NSTextLayoutOrientation; message 'layoutOrientation'; { available in 10_7 }
    function markedRange: NSRange; message 'markedRange';
    procedure setMarkedText_selectedRange (aString: id; selRange: NSRange); message 'setMarkedText:selectedRange:'; deprecated 'in 10_0, 10_6';
    procedure setMarkedText_selectedRange_replacementRange (aString: id; selectedRange: NSRange; replacementRange: NSRange); message 'setMarkedText:selectedRange:replacementRange:';
    procedure unmarkText; message 'unmarkText';
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
    function validAttributesForMarkedText: NSArray; message 'validAttributesForMarkedText';
    function windowLevel: NSInteger; message 'windowLevel';
    function accessibilityStringForRange (range: NSRange): NSString; message 'accessibilityStringForRange:';
    function accessibilityLineForIndex (index: NSInteger): NSInteger; message 'accessibilityLineForIndex:';
    function accessibilityRangeForLine (lineNumber: NSInteger): NSRange; message 'accessibilityRangeForLine:';
    function accessibilityFrameForRange (range: NSRange): NSRect; message 'accessibilityFrameForRange:';
    function accessibilityValue: NSString; message 'accessibilityValue';
  end;


type
  NSCompletion = objccategory external (NSTextView)
    procedure complete (sender: id); message 'complete:';
    function rangeForUserCompletion: NSRange; message 'rangeForUserCompletion';
    function completionsForPartialWordRange_indexOfSelectedItem (charRange: NSRange; index: NSIntegerPtr): NSArray; message 'completionsForPartialWordRange:indexOfSelectedItem:';
    procedure insertCompletion_forPartialWordRange_movement_isFinal (word: NSString; charRange: NSRange; movement: NSInteger; flag: ObjCBOOL); message 'insertCompletion:forPartialWordRange:movement:isFinal:';
  end;

type
  NSTextView_NSPasteboard = objccategory external name 'NSPasteboard' (NSTextView)
    function writablePasteboardTypes: NSArray; message 'writablePasteboardTypes';
    function writeSelectionToPasteboard_type (pboard: NSPasteboard; type_: NSString): ObjCBOOL; message 'writeSelectionToPasteboard:type:';
    function writeSelectionToPasteboard_types (pboard: NSPasteboard; types: NSArray): ObjCBOOL; message 'writeSelectionToPasteboard:types:';
    function readablePasteboardTypes: NSArray; message 'readablePasteboardTypes';
    function preferredPasteboardTypeFromArray_restrictedToTypesFromArray (availableTypes: NSArray; allowedTypes: NSArray): NSString; message 'preferredPasteboardTypeFromArray:restrictedToTypesFromArray:';
    function readSelectionFromPasteboard_type (pboard: NSPasteboard; type_: NSString): ObjCBOOL; message 'readSelectionFromPasteboard:type:';
    function readSelectionFromPasteboard (pboard: NSPasteboard): ObjCBOOL; message 'readSelectionFromPasteboard:';
    class procedure registerForServices; message 'registerForServices';
    function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure pasteAsPlainText (sender: id); message 'pasteAsPlainText:';
    procedure pasteAsRichText (sender: id); message 'pasteAsRichText:';
  end;

type
  NSDragging = objccategory external (NSTextView)
    function dragSelectionWithEvent_offset_slideBack (event: NSEvent; mouseOffset: NSSize; slideBack: ObjCBOOL): ObjCBOOL; message 'dragSelectionWithEvent:offset:slideBack:';
    function dragImageForSelectionWithEvent_origin (event: NSEvent; origin: NSPointPointer): NSImage; message 'dragImageForSelectionWithEvent:origin:';
    function acceptableDragTypes: NSArray; message 'acceptableDragTypes';
    function dragOperationForDraggingInfo_type (dragInfo: NSDraggingInfoProtocol; type_: NSString): NSDragOperation; message 'dragOperationForDraggingInfo:type:';
    procedure cleanUpAfterDragOperation; message 'cleanUpAfterDragOperation';
  end;

type
  NSSharing = objccategory external (NSTextView)
    procedure setSelectedRanges(newValue: NSArray); message 'setSelectedRanges:';
    function selectedRanges: NSArray; message 'selectedRanges';
    procedure setSelectedRanges_affinity_stillSelecting (ranges: NSArray; affinity: NSSelectionAffinity; stillSelectingFlag: ObjCBOOL); message 'setSelectedRanges:affinity:stillSelecting:';
    procedure setSelectedRange_affinity_stillSelecting (charRange: NSRange; affinity: NSSelectionAffinity; stillSelectingFlag: ObjCBOOL); message 'setSelectedRange:affinity:stillSelecting:';
    function selectionAffinity: NSSelectionAffinity; message 'selectionAffinity';
    procedure setSelectionGranularity(newValue: NSSelectionGranularity); message 'setSelectionGranularity:';
    function selectionGranularity: NSSelectionGranularity; message 'selectionGranularity';
    procedure setSelectedTextAttributes(newValue: NSDictionary); message 'setSelectedTextAttributes:';
    function selectedTextAttributes: NSDictionary; message 'selectedTextAttributes';
    procedure setInsertionPointColor(newValue: NSColor); message 'setInsertionPointColor:';
    function insertionPointColor: NSColor; message 'insertionPointColor';
    procedure updateInsertionPointStateAndRestartTimer (restartFlag: ObjCBOOL); message 'updateInsertionPointStateAndRestartTimer:';
    procedure setMarkedTextAttributes(newValue: NSDictionary); message 'setMarkedTextAttributes:';
    function markedTextAttributes: NSDictionary; message 'markedTextAttributes';
    procedure setLinkTextAttributes(newValue: NSDictionary); message 'setLinkTextAttributes:';
    function linkTextAttributes: NSDictionary; message 'linkTextAttributes';
    procedure setDisplaysLinkToolTips(newValue: ObjCBOOL); message 'setDisplaysLinkToolTips:';
    function displaysLinkToolTips: ObjCBOOL; message 'displaysLinkToolTips';
    procedure setAcceptsGlyphInfo(newValue: ObjCBOOL); message 'setAcceptsGlyphInfo:';
    function acceptsGlyphInfo: ObjCBOOL; message 'acceptsGlyphInfo';
    procedure setUsesRuler(newValue: ObjCBOOL); message 'setUsesRuler:';
    function usesRuler: ObjCBOOL; message 'usesRuler';
    procedure setUsesInspectorBar(newValue: ObjCBOOL); message 'setUsesInspectorBar:';
    function usesInspectorBar: ObjCBOOL; message 'usesInspectorBar';
    procedure setContinuousSpellCheckingEnabled(newValue: ObjCBOOL); message 'setContinuousSpellCheckingEnabled:';
    function isContinuousSpellCheckingEnabled: ObjCBOOL; message 'isContinuousSpellCheckingEnabled';
    procedure toggleContinuousSpellChecking (sender: id); message 'toggleContinuousSpellChecking:';
    function spellCheckerDocumentTag: NSInteger; message 'spellCheckerDocumentTag';
    procedure setGrammarCheckingEnabled(newValue: ObjCBOOL); message 'setGrammarCheckingEnabled:';
    function isGrammarCheckingEnabled: ObjCBOOL; message 'isGrammarCheckingEnabled';
    procedure toggleGrammarChecking (sender: id); message 'toggleGrammarChecking:'; { available in 10_5 }
    procedure setSpellingState_range (value: NSInteger; charRange: NSRange); message 'setSpellingState:range:'; { available in 10_5 }
    procedure setTypingAttributes(newValue: NSDictionary); message 'setTypingAttributes:';
    function typingAttributes: NSDictionary; message 'typingAttributes';
    function shouldChangeTextInRanges_replacementStrings (affectedRanges: NSArray; replacementStrings: NSArray): ObjCBOOL; message 'shouldChangeTextInRanges:replacementStrings:';
    function rangesForUserTextChange: NSArray; message 'rangesForUserTextChange';
    function rangesForUserCharacterAttributeChange: NSArray; message 'rangesForUserCharacterAttributeChange';
    function rangesForUserParagraphAttributeChange: NSArray; message 'rangesForUserParagraphAttributeChange';
    function shouldChangeTextInRange_replacementString (affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL; message 'shouldChangeTextInRange:replacementString:';
    procedure didChangeText; message 'didChangeText';
    function rangeForUserTextChange: NSRange; message 'rangeForUserTextChange';
    function rangeForUserCharacterAttributeChange: NSRange; message 'rangeForUserCharacterAttributeChange';
    function rangeForUserParagraphAttributeChange: NSRange; message 'rangeForUserParagraphAttributeChange';
    procedure setAllowsDocumentBackgroundColorChange(newValue: ObjCBOOL); message 'setAllowsDocumentBackgroundColorChange:';
    function allowsDocumentBackgroundColorChange: ObjCBOOL; message 'allowsDocumentBackgroundColorChange';
    procedure setDefaultParagraphStyle(newValue: NSParagraphStyle); message 'setDefaultParagraphStyle:';
    function defaultParagraphStyle: NSParagraphStyle; message 'defaultParagraphStyle';
    procedure setAllowsUndo(newValue: ObjCBOOL); message 'setAllowsUndo:';
    function allowsUndo: ObjCBOOL; message 'allowsUndo';
    procedure breakUndoCoalescing; message 'breakUndoCoalescing';
    function isCoalescingUndo: ObjCBOOL; message 'isCoalescingUndo';
    procedure setAllowsImageEditing(newValue: ObjCBOOL); message 'setAllowsImageEditing:';
    function allowsImageEditing: ObjCBOOL; message 'allowsImageEditing';
    procedure showFindIndicatorForRange (charRange: NSRange); message 'showFindIndicatorForRange:'; { available in 10_5 }
    procedure setUsesRolloverButtonForSelection(newValue: ObjCBOOL); message 'setUsesRolloverButtonForSelection:';
    function usesRolloverButtonForSelection: ObjCBOOL; message 'usesRolloverButtonForSelection';
    procedure setDelegate(newValue: NSTextViewDelegateProtocol); message 'setDelegate:';
    function delegate: NSTextViewDelegateProtocol; message 'delegate';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setSelectable(newValue: ObjCBOOL); message 'setSelectable:';
    function isSelectable: ObjCBOOL; message 'isSelectable';
    procedure setRichText(newValue: ObjCBOOL); message 'setRichText:';
    function isRichText: ObjCBOOL; message 'isRichText';
    procedure setImportsGraphics(newValue: ObjCBOOL); message 'setImportsGraphics:';
    function importsGraphics: ObjCBOOL; message 'importsGraphics';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setFieldEditor(newValue: ObjCBOOL); message 'setFieldEditor:';
    function isFieldEditor: ObjCBOOL; message 'isFieldEditor';
    procedure setUsesFontPanel(newValue: ObjCBOOL); message 'setUsesFontPanel:';
    function usesFontPanel: ObjCBOOL; message 'usesFontPanel';
    procedure setRulerVisible(newValue: ObjCBOOL); message 'setRulerVisible:';
    function isRulerVisible: ObjCBOOL; message 'isRulerVisible';
    procedure setSelectedRange (charRange: NSRange); message 'setSelectedRange:';
    procedure setAllowedInputSourceLocales(newValue: NSArray); message 'setAllowedInputSourceLocales:';
    function allowedInputSourceLocales: NSArray; message 'allowedInputSourceLocales';
  end;

type
  NSTextChecking = objccategory external (NSTextView)
    procedure setSmartInsertDeleteEnabled(newValue: ObjCBOOL); message 'setSmartInsertDeleteEnabled:';
    function smartInsertDeleteEnabled: ObjCBOOL; message 'smartInsertDeleteEnabled';
    function smartDeleteRangeForProposedRange (proposedCharRange: NSRange): NSRange; message 'smartDeleteRangeForProposedRange:';
    procedure toggleSmartInsertDelete (sender: id); message 'toggleSmartInsertDelete:';
    procedure smartInsertForString_replacingRange_beforeString_afterString (pasteString: NSString; charRangeToReplace: NSRange; beforeString: NSStringPtr; afterString: NSStringPtr); message 'smartInsertForString:replacingRange:beforeString:afterString:';
    function smartInsertBeforeStringForString_replacingRange (pasteString: NSString; charRangeToReplace: NSRange): NSString; message 'smartInsertBeforeStringForString:replacingRange:';
    function smartInsertAfterStringForString_replacingRange (pasteString: NSString; charRangeToReplace: NSRange): NSString; message 'smartInsertAfterStringForString:replacingRange:';
    procedure setAutomaticQuoteSubstitutionEnabled(newValue: ObjCBOOL); message 'setAutomaticQuoteSubstitutionEnabled:';
    function isAutomaticQuoteSubstitutionEnabled: ObjCBOOL; message 'isAutomaticQuoteSubstitutionEnabled';
    procedure toggleAutomaticQuoteSubstitution (sender: id); message 'toggleAutomaticQuoteSubstitution:'; { available in 10_5 }
    procedure setAutomaticLinkDetectionEnabled(newValue: ObjCBOOL); message 'setAutomaticLinkDetectionEnabled:';
    function isAutomaticLinkDetectionEnabled: ObjCBOOL; message 'isAutomaticLinkDetectionEnabled';
    procedure toggleAutomaticLinkDetection (sender: id); message 'toggleAutomaticLinkDetection:'; { available in 10_5 }
    procedure setAutomaticDataDetectionEnabled(newValue: ObjCBOOL); message 'setAutomaticDataDetectionEnabled:';
    function isAutomaticDataDetectionEnabled: ObjCBOOL; message 'isAutomaticDataDetectionEnabled';
    procedure toggleAutomaticDataDetection (sender: id); message 'toggleAutomaticDataDetection:'; { available in 10_6 }
    procedure setAutomaticDashSubstitutionEnabled(newValue: ObjCBOOL); message 'setAutomaticDashSubstitutionEnabled:';
    function isAutomaticDashSubstitutionEnabled: ObjCBOOL; message 'isAutomaticDashSubstitutionEnabled';
    procedure toggleAutomaticDashSubstitution (sender: id); message 'toggleAutomaticDashSubstitution:'; { available in 10_6 }
    procedure setAutomaticTextReplacementEnabled(newValue: ObjCBOOL); message 'setAutomaticTextReplacementEnabled:';
    function isAutomaticTextReplacementEnabled: ObjCBOOL; message 'isAutomaticTextReplacementEnabled';
    procedure toggleAutomaticTextReplacement (sender: id); message 'toggleAutomaticTextReplacement:'; { available in 10_6 }
    procedure setAutomaticSpellingCorrectionEnabled(newValue: ObjCBOOL); message 'setAutomaticSpellingCorrectionEnabled:';
    function isAutomaticSpellingCorrectionEnabled: ObjCBOOL; message 'isAutomaticSpellingCorrectionEnabled';
    procedure toggleAutomaticSpellingCorrection (sender: id); message 'toggleAutomaticSpellingCorrection:'; { available in 10_6 }
    procedure setEnabledTextCheckingTypes(newValue: NSTextCheckingTypes); message 'setEnabledTextCheckingTypes:';
    function enabledTextCheckingTypes: NSTextCheckingTypes; message 'enabledTextCheckingTypes';
    procedure checkTextInRange_types_options (range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary); message 'checkTextInRange:types:options:'; { available in 10_6 }
    procedure handleTextCheckingResults_forRange_types_options_orthography_wordCount (results: NSArray; range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary; orthography: NSOrthography; wordCount: NSInteger); message 'handleTextCheckingResults:forRange:types:options:orthography:wordCount:'; { available in 10_6 }
    procedure orderFrontSubstitutionsPanel (sender: id); message 'orderFrontSubstitutionsPanel:'; { available in 10_6 }
    procedure checkTextInSelection (sender: id); message 'checkTextInSelection:'; { available in 10_6 }
    procedure checkTextInDocument (sender: id); message 'checkTextInDocument:'; { available in 10_6 }
    procedure setUsesFindPanel(newValue: ObjCBOOL); message 'setUsesFindPanel:';
    function usesFindPanel: ObjCBOOL; message 'usesFindPanel';
    procedure setUsesFindBar(newValue: ObjCBOOL); message 'setUsesFindBar:';
    function usesFindBar: ObjCBOOL; message 'usesFindBar';
    procedure setIncrementalSearchingEnabled(newValue: ObjCBOOL); message 'setIncrementalSearchingEnabled:';
    function isIncrementalSearchingEnabled: ObjCBOOL; message 'isIncrementalSearchingEnabled';
  end;

type
  NSQuickLookPreview = objccategory external (NSTextView)
    procedure toggleQuickLookPreviewPanel (sender: id); message 'toggleQuickLookPreviewPanel:'; { available in 10_7 }
    function quickLookPreviewableItemsInRanges (ranges: NSArray): NSArray; message 'quickLookPreviewableItemsInRanges:'; { available in 10_7 }
    procedure updateQuickLookPreviewPanel; message 'updateQuickLookPreviewPanel'; { available in 10_7 }
  end;

type
  NSTextView_SharingService = objccategory external (NSTextView)
    procedure orderFrontSharingServicePicker (sender: id); message 'orderFrontSharingServicePicker:'; { available in 10_8 }
  end;

type
  NSTextView_NSDeprecated = objccategory external name 'NSDeprecated' (NSTextView)
    procedure toggleBaseWritingDirection (sender: id); message 'toggleBaseWritingDirection:'; deprecated 'in 10_3, 10_6, "Use NSResponders makeBaseWritingDirectionNatural:, makeBaseWritingDirectionLeftToRight:, and makeBaseWritingDirectionRightToLeft: instead"';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTextViewDelegateProtocol = objcprotocol external name 'NSTextViewDelegate' (NSTextDelegateProtocol)
  optional
    function textView_clickedOnLink_atIndex (textView: NSTextView; link: id; charIndex: NSUInteger): ObjCBOOL; message 'textView:clickedOnLink:atIndex:';
    procedure textView_clickedOnCell_inRect_atIndex (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect; charIndex: NSUInteger); message 'textView:clickedOnCell:inRect:atIndex:';
    procedure textView_doubleClickedOnCell_inRect_atIndex (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect; charIndex: NSUInteger); message 'textView:doubleClickedOnCell:inRect:atIndex:';
    procedure textView_draggedCell_inRect_event_atIndex (view: NSTextView; cell: NSTextAttachmentCellProtocol; rect: NSRect; event: NSEvent; charIndex: NSUInteger); message 'textView:draggedCell:inRect:event:atIndex:';
    function textView_writablePasteboardTypesForCell_atIndex (view: NSTextView; cell: NSTextAttachmentCellProtocol; charIndex: NSUInteger): NSArray; message 'textView:writablePasteboardTypesForCell:atIndex:';
    function textView_writeCell_atIndex_toPasteboard_type (view: NSTextView; cell: NSTextAttachmentCellProtocol; charIndex: NSUInteger; pboard: NSPasteboard; type_: NSString): ObjCBOOL; message 'textView:writeCell:atIndex:toPasteboard:type:';
    function textView_willChangeSelectionFromCharacterRange_toCharacterRange (textView: NSTextView; oldSelectedCharRange: NSRange; newSelectedCharRange: NSRange): NSRange; message 'textView:willChangeSelectionFromCharacterRange:toCharacterRange:';
    function textView_willChangeSelectionFromCharacterRanges_toCharacterRanges (textView: NSTextView; oldSelectedCharRanges: NSArray; newSelectedCharRanges: NSArray): NSArray; message 'textView:willChangeSelectionFromCharacterRanges:toCharacterRanges:';
    function textView_shouldChangeTextInRanges_replacementStrings (textView: NSTextView; affectedRanges: NSArray; replacementStrings: NSArray): ObjCBOOL; message 'textView:shouldChangeTextInRanges:replacementStrings:';
    function textView_shouldChangeTypingAttributes_toAttributes (textView: NSTextView; oldTypingAttributes: NSDictionary; newTypingAttributes: NSDictionary): NSDictionary; message 'textView:shouldChangeTypingAttributes:toAttributes:';
    procedure textViewDidChangeSelection (notification: NSNotification); message 'textViewDidChangeSelection:';
    procedure textViewDidChangeTypingAttributes (notification: NSNotification); message 'textViewDidChangeTypingAttributes:';
    function textView_willDisplayToolTip_forCharacterAtIndex (textView: NSTextView; tooltip: NSString; characterIndex: NSUInteger): NSString; message 'textView:willDisplayToolTip:forCharacterAtIndex:';
    function textView_completions_forPartialWordRange_indexOfSelectedItem (textView: NSTextView; words: NSArray; charRange: NSRange; index: NSIntegerPtr): NSArray; message 'textView:completions:forPartialWordRange:indexOfSelectedItem:';
    function textView_shouldChangeTextInRange_replacementString (textView: NSTextView; affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL; message 'textView:shouldChangeTextInRange:replacementString:';
    function textView_doCommandBySelector (textView: NSTextView; commandSelector: SEL): ObjCBOOL; message 'textView:doCommandBySelector:';
    function textView_shouldSetSpellingState_range (textView: NSTextView; value: NSInteger; affectedCharRange: NSRange): NSInteger; message 'textView:shouldSetSpellingState:range:'; { available in 10_5 }
    function textView_menu_forEvent_atIndex (view: NSTextView; menu: NSMenu; event: NSEvent; charIndex: NSUInteger): NSMenu; message 'textView:menu:forEvent:atIndex:'; { available in 10_5 }
    function textView_willCheckTextInRange_options_types (view: NSTextView; range: NSRange; options: NSDictionary; checkingTypes: NSTextCheckingTypesPtr): NSDictionary; message 'textView:willCheckTextInRange:options:types:'; { available in 10_6 }
    function textView_didCheckTextInRange_types_options_results_orthography_wordCount (view: NSTextView; range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary; results: NSArray; orthography: NSOrthography; wordCount: NSInteger): NSArray; message 'textView:didCheckTextInRange:types:options:results:orthography:wordCount:'; { available in 10_6 }
    function textView_URLForContentsOfTextAttachment_atIndex (textView: NSTextView; textAttachment: NSTextAttachment; charIndex: NSUInteger): NSURL; message 'textView:URLForContentsOfTextAttachment:atIndex:'; { available in 10_7 }
    function textView_willShowSharingServicePicker_forItems (textView: NSTextView; servicePicker: NSSharingServicePicker; items: NSArray): NSSharingServicePicker; message 'textView:willShowSharingServicePicker:forItems:'; { available in 10_8 }
    function undoManagerForTextView (view: NSTextView): NSUndoManager; message 'undoManagerForTextView:';
    function textView_clickedOnLink (textView: NSTextView; link: id): ObjCBOOL; message 'textView:clickedOnLink:'; deprecated 'in 10_0, 10_6, "Use -textView:clickedOnLink:atIndex: instead"';
    procedure textView_clickedOnCell_inRect (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect); message 'textView:clickedOnCell:inRect:'; deprecated 'in 10_0, 10_6, "Use -textView:clickedOnCell:inRect:atIndex: instead"';
    procedure textView_doubleClickedOnCell_inRect (textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect); message 'textView:doubleClickedOnCell:inRect:'; deprecated 'in 10_0, 10_6, "Use -textView:doubleClickedOnCell:inRect:atIndex: instead"';
    procedure textView_draggedCell_inRect_event (view: NSTextView; cell: NSTextAttachmentCellProtocol; rect: NSRect; event: NSEvent); message 'textView:draggedCell:inRect:event:'; deprecated 'in 10_0, 10_6, "Use -textView:draggedCell:inRect:event:atIndex: instead"';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTextViewWillChangeNotifyingTextViewNotification: NSString; cvar; external;
  NSTextViewDidChangeSelectionNotification: NSString; cvar; external;
  NSTextViewDidChangeTypingAttributesNotification: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSFindPanelAction = NSUInteger;
  NSFindPanelActionPtr = ^NSFindPanelAction;

const
  NSFindPanelActionShowFindPanel = 1;
  NSFindPanelActionNext = 2;
  NSFindPanelActionPrevious = 3;
  NSFindPanelActionReplaceAll = 4;
  NSFindPanelActionReplace = 5;
  NSFindPanelActionReplaceAndFind = 6;
  NSFindPanelActionSetFindString = 7;
  NSFindPanelActionReplaceAllInSelection = 8;
  NSFindPanelActionSelectAll = 9;
  NSFindPanelActionSelectAllInSelection = 10;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFindPanelSearchOptionsPboardType: NSString { available in 10_5 }; cvar; external;
  NSFindPanelCaseInsensitiveSearch: NSString { available in 10_5 }; cvar; external;
  NSFindPanelSubstringMatch: NSString { available in 10_5 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSFindPanelSubstringMatchType = NSUInteger;
  NSFindPanelSubstringMatchTypePtr = ^NSFindPanelSubstringMatchType;

const
  NSFindPanelSubstringMatchTypeContains = 0;
  NSFindPanelSubstringMatchTypeStartsWith = 1;
  NSFindPanelSubstringMatchTypeFullWord = 2;
  NSFindPanelSubstringMatchTypeEndsWith = 3;
{$endif}


{ Parsed from Appkit.framework NSTextView.h }

{$ifdef TYPES}
{$ifndef NSTEXTVIEW_PAS_T}
{$define NSTEXTVIEW_PAS_T}

{ Constants }

const
  NSSelectByCharacter = 0;
  NSSelectByWord = 1;
  NSSelectByParagraph = 2;

const
  NSSelectionAffinityUpstream = 0;
  NSSelectionAffinityDownstream = 1;

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

const
  NSFindPanelSubstringMatchTypeContains = 0;
  NSFindPanelSubstringMatchTypeStartsWith = 1;
  NSFindPanelSubstringMatchTypeFullWord = 2;
  NSFindPanelSubstringMatchTypeEndsWith = 3;

{ Types }
type
  NSSelectionGranularity = NSUInteger;
  NSSelectionGranularityPtr = ^NSSelectionGranularity;
  NSSelectionAffinity = NSUInteger;
  NSSelectionAffinityPtr = ^NSSelectionAffinity;
  NSFindPanelAction = NSUInteger;
  NSFindPanelActionPtr = ^NSFindPanelAction;
  NSFindPanelSubstringMatchType = NSUInteger;
  NSFindPanelSubstringMatchTypePtr = ^NSFindPanelSubstringMatchType;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTVIEW_PAS_R}
{$define NSTEXTVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTVIEW_PAS_F}
{$define NSTEXTVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTVIEW_PAS_S}
{$define NSTEXTVIEW_PAS_S}

{ External string constants }
var
  NSFindPanelSearchOptionsPboardType: NSString; cvar; external;
  NSFindPanelCaseInsensitiveSearch: NSString; cvar; external;
  NSFindPanelSubstringMatch: NSString; cvar; external;
  NSAllRomanInputSourcesLocaleIdentifier: NSString; cvar; external;
  NSTextViewWillChangeNotifyingTextViewNotification: NSString; cvar; external;
  NSTextViewDidChangeSelectionNotification: NSString; cvar; external;
  NSTextViewDidChangeTypingAttributesNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextViewDelegateProtocol = objcprotocol;
  NSTextView = objcclass;
  NSTextViewPointer = ^NSTextView;
  NSTextViewPtr = NSTextViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTVIEW_PAS_C}
{$define NSTEXTVIEW_PAS_C}

{ NSTextView }
  NSTextView = objcclass external (NSText)
    
  public
    function initWithFrame_textContainer(frameRect: NSRect; container: NSTextContainer): id; message 'initWithFrame:textContainer:';
    function initWithFrame(frameRect: NSRect): id; message 'initWithFrame:';
    function textContainer: NSTextContainer; message 'textContainer';
    procedure setTextContainer(container: NSTextContainer); message 'setTextContainer:';
    procedure replaceTextContainer(newContainer: NSTextContainer); message 'replaceTextContainer:';
    procedure setTextContainerInset(inset: NSSize); message 'setTextContainerInset:';
    function textContainerInset: NSSize; message 'textContainerInset';
    function textContainerOrigin: NSPoint; message 'textContainerOrigin';
    procedure invalidateTextContainerOrigin; message 'invalidateTextContainerOrigin';
    function layoutManager: NSLayoutManager; message 'layoutManager';
    function textStorage: NSTextStorage; message 'textStorage';
    procedure insertText(insertString: id); message 'insertText:';
    procedure setConstrainedFrameSize(desiredSize: NSSize); message 'setConstrainedFrameSize:';
    procedure setAlignment_range(alignment_: NSTextAlignment; range: NSRange); message 'setAlignment:range:';
    procedure setBaseWritingDirection_range(writingDirection: NSWritingDirection; range: NSRange); message 'setBaseWritingDirection:range:';
    procedure turnOffKerning(sender: id); message 'turnOffKerning:';
    procedure tightenKerning(sender: id); message 'tightenKerning:';
    procedure loosenKerning(sender: id); message 'loosenKerning:';
    procedure useStandardKerning(sender: id); message 'useStandardKerning:';
    procedure turnOffLigatures(sender: id); message 'turnOffLigatures:';
    procedure useStandardLigatures(sender: id); message 'useStandardLigatures:';
    procedure useAllLigatures(sender: id); message 'useAllLigatures:';
    procedure raiseBaseline(sender: id); message 'raiseBaseline:';
    procedure lowerBaseline(sender: id); message 'lowerBaseline:';
    procedure toggleTraditionalCharacterShape(sender: id); message 'toggleTraditionalCharacterShape:';
    procedure outline(sender: id); message 'outline:';
    procedure performFindPanelAction(sender: id); message 'performFindPanelAction:';
    procedure alignJustified(sender: id); message 'alignJustified:';
    procedure changeColor(sender: id); message 'changeColor:';
    procedure changeAttributes(sender: id); message 'changeAttributes:';
    procedure changeDocumentBackgroundColor(sender: id); message 'changeDocumentBackgroundColor:';
    procedure orderFrontSpacingPanel(sender: id); message 'orderFrontSpacingPanel:';
    procedure orderFrontLinkPanel(sender: id); message 'orderFrontLinkPanel:';
    procedure orderFrontListPanel(sender: id); message 'orderFrontListPanel:';
    procedure orderFrontTablePanel(sender: id); message 'orderFrontTablePanel:';
    procedure rulerView_didMoveMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didMoveMarker:';
    procedure rulerView_didRemoveMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didRemoveMarker:';
    procedure rulerView_didAddMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didAddMarker:';
    function rulerView_shouldMoveMarker(ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldMoveMarker:';
    function rulerView_shouldAddMarker(ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldAddMarker:';
    function rulerView_willMoveMarker_toLocation(ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willMoveMarker:toLocation:';
    function rulerView_shouldRemoveMarker(ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldRemoveMarker:';
    function rulerView_willAddMarker_atLocation(ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willAddMarker:atLocation:';
    procedure rulerView_handleMouseDown(ruler: NSRulerView; event: NSEvent); message 'rulerView:handleMouseDown:';
    procedure setNeedsDisplayInRect_avoidAdditionalLayout(rect: NSRect; flag: ObjCBOOL); message 'setNeedsDisplayInRect:avoidAdditionalLayout:';
    function shouldDrawInsertionPoint: ObjCBOOL; message 'shouldDrawInsertionPoint';
    procedure drawInsertionPointInRect_color_turnedOn(rect: NSRect; color: NSColor; flag: ObjCBOOL); message 'drawInsertionPointInRect:color:turnedOn:';
    procedure drawViewBackgroundInRect(rect: NSRect); message 'drawViewBackgroundInRect:';
    procedure updateRuler; message 'updateRuler';
    procedure updateFontPanel; message 'updateFontPanel';
    procedure updateDragTypeRegistration; message 'updateDragTypeRegistration';
    function selectionRangeForProposedRange_granularity(proposedCharRange: NSRange; granularity: NSSelectionGranularity): NSRange; message 'selectionRangeForProposedRange:granularity:';
    procedure clickedOnLink_atIndex(link: id; charIndex: NSUInteger); message 'clickedOnLink:atIndex:';
    procedure startSpeaking(sender: id); message 'startSpeaking:';
    procedure stopSpeaking(sender: id); message 'stopSpeaking:';
    function characterIndexForInsertionAtPoint(point: NSPoint): NSUInteger; message 'characterIndexForInsertionAtPoint:';
  end;

{ NSCompletionCategory }
  NSCompletionCategory = objccategory external (NSTextView)
    procedure complete(sender: id); message 'complete:';
    function rangeForUserCompletion: NSRange; message 'rangeForUserCompletion';
    function completionsForPartialWordRange_indexOfSelectedItem(charRange: NSRange; index: NSIntegerPtr): NSArray; message 'completionsForPartialWordRange:indexOfSelectedItem:';
    procedure insertCompletion_forPartialWordRange_movement_isFinal(word: NSString; charRange: NSRange; movement: NSInteger; flag: ObjCBOOL); message 'insertCompletion:forPartialWordRange:movement:isFinal:';
  end;

{ NSPasteboard_NSTextViewCategory }
  NSPasteboard_NSTextViewCategory = objccategory external name 'NSPasteboard' (NSTextView)
    function writablePasteboardTypes: NSArray; message 'writablePasteboardTypes';
    function writeSelectionToPasteboard_type(pboard: NSPasteboard; type_: NSString): ObjCBOOL; message 'writeSelectionToPasteboard:type:';
    function writeSelectionToPasteboard_types(pboard: NSPasteboard; types: NSArray): ObjCBOOL; message 'writeSelectionToPasteboard:types:';
    function readablePasteboardTypes: NSArray; message 'readablePasteboardTypes';
    function preferredPasteboardTypeFromArray_restrictedToTypesFromArray(availableTypes: NSArray; allowedTypes: NSArray): NSString; message 'preferredPasteboardTypeFromArray:restrictedToTypesFromArray:';
    function readSelectionFromPasteboard_type(pboard: NSPasteboard; type_: NSString): ObjCBOOL; message 'readSelectionFromPasteboard:type:';
    function readSelectionFromPasteboard(pboard: NSPasteboard): ObjCBOOL; message 'readSelectionFromPasteboard:';
    class procedure registerForServices; message 'registerForServices';
    function validRequestorForSendType_returnType(sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure pasteAsPlainText(sender: id); message 'pasteAsPlainText:';
    procedure pasteAsRichText(sender: id); message 'pasteAsRichText:';
  end;

{ NSDraggingCategory }
  NSDraggingCategory = objccategory external (NSTextView)
    function dragSelectionWithEvent_offset_slideBack(event: NSEvent; mouseOffset: NSSize; slideBack: ObjCBOOL): ObjCBOOL; message 'dragSelectionWithEvent:offset:slideBack:';
    function dragImageForSelectionWithEvent_origin(event: NSEvent; origin: NSPointPointer): NSImage; message 'dragImageForSelectionWithEvent:origin:';
    function acceptableDragTypes: NSArray; message 'acceptableDragTypes';
    function dragOperationForDraggingInfo_type(dragInfo: NSDraggingInfoProtocol; type_: NSString): NSDragOperation; message 'dragOperationForDraggingInfo:type:';
    procedure cleanUpAfterDragOperation; message 'cleanUpAfterDragOperation';
  end;

{ NSSharingCategory }
  NSSharingCategory = objccategory external (NSTextView)
    function selectedRanges: NSArray; message 'selectedRanges';
    procedure setSelectedRanges_affinity_stillSelecting(ranges: NSArray; affinity: NSSelectionAffinity; stillSelectingFlag: ObjCBOOL); message 'setSelectedRanges:affinity:stillSelecting:';
    procedure setSelectedRanges(ranges: NSArray); message 'setSelectedRanges:';
    procedure setSelectedRange_affinity_stillSelecting(charRange: NSRange; affinity: NSSelectionAffinity; stillSelectingFlag: ObjCBOOL); message 'setSelectedRange:affinity:stillSelecting:';
    function selectionAffinity: NSSelectionAffinity; message 'selectionAffinity';
    function selectionGranularity: NSSelectionGranularity; message 'selectionGranularity';
    procedure setSelectionGranularity(granularity: NSSelectionGranularity); message 'setSelectionGranularity:';
    procedure setSelectedTextAttributes(attributeDictionary: NSDictionary); message 'setSelectedTextAttributes:';
    function selectedTextAttributes: NSDictionary; message 'selectedTextAttributes';
    procedure setInsertionPointColor(color: NSColor); message 'setInsertionPointColor:';
    function insertionPointColor: NSColor; message 'insertionPointColor';
    procedure updateInsertionPointStateAndRestartTimer(restartFlag: ObjCBOOL); message 'updateInsertionPointStateAndRestartTimer:';
    procedure setMarkedTextAttributes(attributeDictionary: NSDictionary); message 'setMarkedTextAttributes:';
    function markedTextAttributes: NSDictionary; message 'markedTextAttributes';
    procedure setLinkTextAttributes(attributeDictionary: NSDictionary); message 'setLinkTextAttributes:';
    function linkTextAttributes: NSDictionary; message 'linkTextAttributes';
    function displaysLinkToolTips: ObjCBOOL; message 'displaysLinkToolTips';
    procedure setDisplaysLinkToolTips(flag: ObjCBOOL); message 'setDisplaysLinkToolTips:';
    function acceptsGlyphInfo: ObjCBOOL; message 'acceptsGlyphInfo';
    procedure setAcceptsGlyphInfo(flag: ObjCBOOL); message 'setAcceptsGlyphInfo:';
    procedure setRulerVisible(flag: ObjCBOOL); message 'setRulerVisible:';
    function usesRuler: ObjCBOOL; message 'usesRuler';
    procedure setUsesRuler(flag: ObjCBOOL); message 'setUsesRuler:';
    procedure setContinuousSpellCheckingEnabled(flag: ObjCBOOL); message 'setContinuousSpellCheckingEnabled:';
    function isContinuousSpellCheckingEnabled: ObjCBOOL; message 'isContinuousSpellCheckingEnabled';
    procedure toggleContinuousSpellChecking(sender: id); message 'toggleContinuousSpellChecking:';
    function spellCheckerDocumentTag: NSInteger; message 'spellCheckerDocumentTag';
    procedure setGrammarCheckingEnabled(flag: ObjCBOOL); message 'setGrammarCheckingEnabled:';
    function isGrammarCheckingEnabled: ObjCBOOL; message 'isGrammarCheckingEnabled';
    procedure toggleGrammarChecking(sender: id); message 'toggleGrammarChecking:';
    procedure setSpellingState_range(value: NSInteger; charRange: NSRange); message 'setSpellingState:range:';
    function typingAttributes: NSDictionary; message 'typingAttributes';
    procedure setTypingAttributes(attrs: NSDictionary); message 'setTypingAttributes:';
    function shouldChangeTextInRanges_replacementStrings(affectedRanges: NSArray; replacementStrings: NSArray): ObjCBOOL; message 'shouldChangeTextInRanges:replacementStrings:';
    function rangesForUserTextChange: NSArray; message 'rangesForUserTextChange';
    function rangesForUserCharacterAttributeChange: NSArray; message 'rangesForUserCharacterAttributeChange';
    function rangesForUserParagraphAttributeChange: NSArray; message 'rangesForUserParagraphAttributeChange';
    function shouldChangeTextInRange_replacementString(affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL; message 'shouldChangeTextInRange:replacementString:';
    procedure didChangeText; message 'didChangeText';
    function rangeForUserTextChange: NSRange; message 'rangeForUserTextChange';
    function rangeForUserCharacterAttributeChange: NSRange; message 'rangeForUserCharacterAttributeChange';
    function rangeForUserParagraphAttributeChange: NSRange; message 'rangeForUserParagraphAttributeChange';
    procedure setUsesFindPanel(flag: ObjCBOOL); message 'setUsesFindPanel:';
    function usesFindPanel: ObjCBOOL; message 'usesFindPanel';
    procedure setAllowsDocumentBackgroundColorChange(flag: ObjCBOOL); message 'setAllowsDocumentBackgroundColorChange:';
    function allowsDocumentBackgroundColorChange: ObjCBOOL; message 'allowsDocumentBackgroundColorChange';
    procedure setDefaultParagraphStyle(paragraphStyle: NSParagraphStyle); message 'setDefaultParagraphStyle:';
    function defaultParagraphStyle: NSParagraphStyle; message 'defaultParagraphStyle';
    procedure setAllowsUndo(flag: ObjCBOOL); message 'setAllowsUndo:';
    function allowsUndo: ObjCBOOL; message 'allowsUndo';
    procedure breakUndoCoalescing; message 'breakUndoCoalescing';
    function isCoalescingUndo: ObjCBOOL; message 'isCoalescingUndo';
    function allowsImageEditing: ObjCBOOL; message 'allowsImageEditing';
    procedure setAllowsImageEditing(flag: ObjCBOOL); message 'setAllowsImageEditing:';
    procedure showFindIndicatorForRange(charRange: NSRange); message 'showFindIndicatorForRange:';
    function delegate: NSTextViewDelegateProtocol; message 'delegate';
    procedure setDelegate(anObject: NSTextViewDelegateProtocol); message 'setDelegate:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setEditable(flag: ObjCBOOL); message 'setEditable:';
    function isSelectable: ObjCBOOL; message 'isSelectable';
    procedure setSelectable(flag: ObjCBOOL); message 'setSelectable:';
    function isRichText: ObjCBOOL; message 'isRichText';
    procedure setRichText(flag: ObjCBOOL); message 'setRichText:';
    function importsGraphics: ObjCBOOL; message 'importsGraphics';
    procedure setImportsGraphics(flag: ObjCBOOL); message 'setImportsGraphics:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setDrawsBackground(flag: ObjCBOOL); message 'setDrawsBackground:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function isFieldEditor: ObjCBOOL; message 'isFieldEditor';
    procedure setFieldEditor(flag: ObjCBOOL); message 'setFieldEditor:';
    function usesFontPanel: ObjCBOOL; message 'usesFontPanel';
    procedure setUsesFontPanel(flag: ObjCBOOL); message 'setUsesFontPanel:';
    function isRulerVisible: ObjCBOOL; message 'isRulerVisible';
    procedure setSelectedRange(charRange: NSRange); message 'setSelectedRange:';
    function allowedInputSourceLocales: NSArray; message 'allowedInputSourceLocales';
    procedure setAllowedInputSourceLocales(localeIdentifiers: NSArray); message 'setAllowedInputSourceLocales:';
  end;

{ NSTextCheckingCategory }
  NSTextCheckingCategory = objccategory external (NSTextView)
    function smartInsertDeleteEnabled: ObjCBOOL; message 'smartInsertDeleteEnabled';
    procedure setSmartInsertDeleteEnabled(flag: ObjCBOOL); message 'setSmartInsertDeleteEnabled:';
    function smartDeleteRangeForProposedRange(proposedCharRange: NSRange): NSRange; message 'smartDeleteRangeForProposedRange:';
    procedure toggleSmartInsertDelete(sender: id); message 'toggleSmartInsertDelete:';
    procedure smartInsertForString_replacingRange_beforeString_afterString(pasteString: NSString; charRangeToReplace: NSRange; beforeString: NSStringPointer; afterString: NSStringPointer); message 'smartInsertForString:replacingRange:beforeString:afterString:';
    function smartInsertBeforeStringForString_replacingRange(pasteString: NSString; charRangeToReplace: NSRange): NSString; message 'smartInsertBeforeStringForString:replacingRange:';
    function smartInsertAfterStringForString_replacingRange(pasteString: NSString; charRangeToReplace: NSRange): NSString; message 'smartInsertAfterStringForString:replacingRange:';
    procedure setAutomaticQuoteSubstitutionEnabled(flag: ObjCBOOL); message 'setAutomaticQuoteSubstitutionEnabled:';
    function isAutomaticQuoteSubstitutionEnabled: ObjCBOOL; message 'isAutomaticQuoteSubstitutionEnabled';
    procedure toggleAutomaticQuoteSubstitution(sender: id); message 'toggleAutomaticQuoteSubstitution:';
    procedure setAutomaticLinkDetectionEnabled(flag: ObjCBOOL); message 'setAutomaticLinkDetectionEnabled:';
    function isAutomaticLinkDetectionEnabled: ObjCBOOL; message 'isAutomaticLinkDetectionEnabled';
    procedure toggleAutomaticLinkDetection(sender: id); message 'toggleAutomaticLinkDetection:';
    procedure setAutomaticDataDetectionEnabled(flag: ObjCBOOL); message 'setAutomaticDataDetectionEnabled:';
    function isAutomaticDataDetectionEnabled: ObjCBOOL; message 'isAutomaticDataDetectionEnabled';
    procedure toggleAutomaticDataDetection(sender: id); message 'toggleAutomaticDataDetection:';
    procedure setAutomaticDashSubstitutionEnabled(flag: ObjCBOOL); message 'setAutomaticDashSubstitutionEnabled:';
    function isAutomaticDashSubstitutionEnabled: ObjCBOOL; message 'isAutomaticDashSubstitutionEnabled';
    procedure toggleAutomaticDashSubstitution(sender: id); message 'toggleAutomaticDashSubstitution:';
    procedure setAutomaticTextReplacementEnabled(flag: ObjCBOOL); message 'setAutomaticTextReplacementEnabled:';
    function isAutomaticTextReplacementEnabled: ObjCBOOL; message 'isAutomaticTextReplacementEnabled';
    procedure toggleAutomaticTextReplacement(sender: id); message 'toggleAutomaticTextReplacement:';
    procedure setAutomaticSpellingCorrectionEnabled(flag: ObjCBOOL); message 'setAutomaticSpellingCorrectionEnabled:';
    function isAutomaticSpellingCorrectionEnabled: ObjCBOOL; message 'isAutomaticSpellingCorrectionEnabled';
    procedure toggleAutomaticSpellingCorrection(sender: id); message 'toggleAutomaticSpellingCorrection:';
    function enabledTextCheckingTypes: NSTextCheckingTypes; message 'enabledTextCheckingTypes';
    procedure setEnabledTextCheckingTypes(checkingTypes: NSTextCheckingTypes); message 'setEnabledTextCheckingTypes:';
    procedure checkTextInRange_types_options(range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary); message 'checkTextInRange:types:options:';
    procedure handleTextCheckingResults_forRange_types_options_orthography_wordCount(results: NSArray; range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary; orthography: NSOrthography; wordCount: NSInteger); message 'handleTextCheckingResults:forRange:types:options:orthography:wordCount:';
    procedure orderFrontSubstitutionsPanel(sender: id); message 'orderFrontSubstitutionsPanel:';
    procedure checkTextInSelection(sender: id); message 'checkTextInSelection:';
    procedure checkTextInDocument(sender: id); message 'checkTextInDocument:';
  end;

{ NSDeprecated_NSTextViewCategory }
  NSDeprecated_NSTextViewCategory = objccategory external name 'NSDeprecated' (NSTextView)
    procedure toggleBaseWritingDirection(sender: id); message 'toggleBaseWritingDirection:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSTEXTVIEW_PAS_P}
{$define NSTEXTVIEW_PAS_P}
  
{ NSTextViewDelegate Protocol }
  NSTextViewDelegateProtocol = objcprotocol external name 'NSTextViewDelegate'
  optional
    function textView_clickedOnLink_atIndex(textView: NSTextView; link: id; charIndex: NSUInteger): ObjCBOOL; message 'textView:clickedOnLink:atIndex:';
    procedure textView_clickedOnCell_inRect_atIndex(textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect; charIndex: NSUInteger); message 'textView:clickedOnCell:inRect:atIndex:';
    procedure textView_doubleClickedOnCell_inRect_atIndex(textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect; charIndex: NSUInteger); message 'textView:doubleClickedOnCell:inRect:atIndex:';
    procedure textView_draggedCell_inRect_event_atIndex(view: NSTextView; cell: NSTextAttachmentCellProtocol; rect: NSRect; event: NSEvent; charIndex: NSUInteger); message 'textView:draggedCell:inRect:event:atIndex:';
    function textView_writablePasteboardTypesForCell_atIndex(view: NSTextView; cell: NSTextAttachmentCellProtocol; charIndex: NSUInteger): NSArray; message 'textView:writablePasteboardTypesForCell:atIndex:';
    function textView_writeCell_atIndex_toPasteboard_type(view: NSTextView; cell: NSTextAttachmentCellProtocol; charIndex: NSUInteger; pboard: NSPasteboard; type_: NSString): ObjCBOOL; message 'textView:writeCell:atIndex:toPasteboard:type:';
    function textView_willChangeSelectionFromCharacterRange_toCharacterRange(textView: NSTextView; oldSelectedCharRange: NSRange; newSelectedCharRange: NSRange): NSRange; message 'textView:willChangeSelectionFromCharacterRange:toCharacterRange:';
    function textView_willChangeSelectionFromCharacterRanges_toCharacterRanges(textView: NSTextView; oldSelectedCharRanges: NSArray; newSelectedCharRanges: NSArray): NSArray; message 'textView:willChangeSelectionFromCharacterRanges:toCharacterRanges:';
    function textView_shouldChangeTextInRanges_replacementStrings(textView: NSTextView; affectedRanges: NSArray; replacementStrings: NSArray): ObjCBOOL; message 'textView:shouldChangeTextInRanges:replacementStrings:';
    function textView_shouldChangeTypingAttributes_toAttributes(textView: NSTextView; oldTypingAttributes: NSDictionary; newTypingAttributes: NSDictionary): NSDictionary; message 'textView:shouldChangeTypingAttributes:toAttributes:';
    procedure textViewDidChangeSelection(notification: NSNotification); message 'textViewDidChangeSelection:';
    procedure textViewDidChangeTypingAttributes(notification: NSNotification); message 'textViewDidChangeTypingAttributes:';
    function textView_willDisplayToolTip_forCharacterAtIndex(textView: NSTextView; tooltip: NSString; characterIndex: NSUInteger): NSString; message 'textView:willDisplayToolTip:forCharacterAtIndex:';
    function textView_completions_forPartialWordRange_indexOfSelectedItem(textView: NSTextView; words: NSArray; charRange: NSRange; index: NSIntegerPtr): NSArray; message 'textView:completions:forPartialWordRange:indexOfSelectedItem:';
    function textView_shouldChangeTextInRange_replacementString(textView: NSTextView; affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL; message 'textView:shouldChangeTextInRange:replacementString:';
    function textView_doCommandBySelector(textView: NSTextView; commandSelector: SEL): ObjCBOOL; message 'textView:doCommandBySelector:';
    function textView_shouldSetSpellingState_range(textView: NSTextView; value: NSInteger; affectedCharRange: NSRange): NSInteger; message 'textView:shouldSetSpellingState:range:';
    function textView_menu_forEvent_atIndex(view: NSTextView; menu: NSMenu; event: NSEvent; charIndex: NSUInteger): NSMenu; message 'textView:menu:forEvent:atIndex:';
    function textView_willCheckTextInRange_options_types(view: NSTextView; range: NSRange; options: NSDictionary; checkingTypes: NSTextCheckingTypesPtr): NSDictionary; message 'textView:willCheckTextInRange:options:types:';
    function textView_didCheckTextInRange_types_options_results_orthography_wordCount(view: NSTextView; range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary; results: NSArray; orthography: NSOrthography; wordCount: NSInteger): NSArray; message 'textView:didCheckTextInRange:types:options:results:orthography:wordCount:';
    function textView_clickedOnLink(textView: NSTextView; link: id): ObjCBOOL; message 'textView:clickedOnLink:';
    procedure textView_clickedOnCell_inRect(textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect); message 'textView:clickedOnCell:inRect:';
    procedure textView_doubleClickedOnCell_inRect(textView: NSTextView; cell: NSTextAttachmentCellProtocol; cellFrame: NSRect); message 'textView:doubleClickedOnCell:inRect:';
    procedure textView_draggedCell_inRect_event(view: NSTextView; cell: NSTextAttachmentCellProtocol; rect: NSRect; event: NSEvent); message 'textView:draggedCell:inRect:event:';
    function undoManagerForTextView(view: NSTextView): NSUndoManager; message 'undoManagerForTextView:';
  end;
{$endif}
{$endif}

{ Parsed from Appkit.framework NSTextView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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
  NSSelectionAffinity = NSUInteger;
  NSFindPanelAction = NSUInteger;
  NSFindPanelSubstringMatchType = NSUInteger;

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
  NSFindPanelSearchOptionsPboardType: NSString; external name '_NSFindPanelSearchOptionsPboardType';
  NSFindPanelCaseInsensitiveSearch: NSString; external name '_NSFindPanelCaseInsensitiveSearch';
  NSFindPanelSubstringMatch: NSString; external name '_NSFindPanelSubstringMatch';
  NSAllRomanInputSourcesLocaleIdentifier: NSString; external name '_NSAllRomanInputSourcesLocaleIdentifier';
  NSTextViewWillChangeNotifyingTextViewNotification: NSString; external name '_NSTextViewWillChangeNotifyingTextViewNotification';
  NSTextViewDidChangeSelectionNotification: NSString; external name '_NSTextViewDidChangeSelectionNotification';
  NSTextViewDidChangeTypingAttributesNotification: NSString; external name '_NSTextViewDidChangeTypingAttributesNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextView = objcclass;
  NSTextViewPointer = ^NSTextView;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTVIEW_PAS_C}
{$define NSTEXTVIEW_PAS_C}

{ NSTextView }
  NSTextView = objcclass(NSText)
    
  public
    class function alloc: NSTextView; message 'alloc';

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
    procedure toggleBaseWritingDirection(sender: id); message 'toggleBaseWritingDirection:';
    procedure orderFrontSpacingPanel(sender: id); message 'orderFrontSpacingPanel:';
    procedure orderFrontLinkPanel(sender: id); message 'orderFrontLinkPanel:';
    procedure orderFrontListPanel(sender: id); message 'orderFrontListPanel:';
    procedure orderFrontTablePanel(sender: id); message 'orderFrontTablePanel:';
    procedure rulerView_didMoveMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didMoveMarker:';
    procedure rulerView_didRemoveMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didRemoveMarker:';
    procedure rulerView_didAddMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didAddMarker:';
    function rulerView_shouldMoveMarker(ruler: NSRulerView; marker: NSRulerMarker): Boolean; message 'rulerView:shouldMoveMarker:';
    function rulerView_shouldAddMarker(ruler: NSRulerView; marker: NSRulerMarker): Boolean; message 'rulerView:shouldAddMarker:';
    function rulerView_willMoveMarker_toLocation(ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willMoveMarker:toLocation:';
    function rulerView_shouldRemoveMarker(ruler: NSRulerView; marker: NSRulerMarker): Boolean; message 'rulerView:shouldRemoveMarker:';
    function rulerView_willAddMarker_atLocation(ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willAddMarker:atLocation:';
    procedure rulerView_handleMouseDown(ruler: NSRulerView; event: NSEvent); message 'rulerView:handleMouseDown:';
    procedure setNeedsDisplayInRect_avoidAdditionalLayout(rect: NSRect; flag: Boolean); message 'setNeedsDisplayInRect:avoidAdditionalLayout:';
    function shouldDrawInsertionPoint: Boolean; message 'shouldDrawInsertionPoint';
    procedure drawInsertionPointInRect_color_turnedOn(rect: NSRect; color: NSColor; flag: Boolean); message 'drawInsertionPointInRect:color:turnedOn:';
    procedure drawViewBackgroundInRect(rect: NSRect); message 'drawViewBackgroundInRect:';
    procedure updateRuler; message 'updateRuler';
    procedure updateFontPanel; message 'updateFontPanel';
    procedure updateDragTypeRegistration; message 'updateDragTypeRegistration';
    function selectionRangeForProposedRange_granularity(proposedCharRange: NSRange; granularity: NSSelectionGranularity): NSRange; message 'selectionRangeForProposedRange:granularity:';
    procedure clickedOnLink_atIndex(link: id; charIndex: NSUInteger); message 'clickedOnLink:atIndex:';
    procedure startSpeaking(sender: id); message 'startSpeaking:';
    procedure stopSpeaking(sender: id); message 'stopSpeaking:';
    function characterIndexForInsertionAtPoint(point: NSPoint): NSUInteger; message 'characterIndexForInsertionAtPoint:';

    { Category: NSCompletion }
    procedure complete(sender: id); message 'complete:';
    function rangeForUserCompletion: NSRange; message 'rangeForUserCompletion';
    function completionsForPartialWordRange_indexOfSelectedItem(charRange: NSRange; index: NSIntegerPointer): NSArray; message 'completionsForPartialWordRange:indexOfSelectedItem:';
    procedure insertCompletion_forPartialWordRange_movement_isFinal(word: NSString; charRange: NSRange; movement: NSInteger; flag: Boolean); message 'insertCompletion:forPartialWordRange:movement:isFinal:';

    { Category: NSPasteboard }
    function writablePasteboardTypes: NSArray; message 'writablePasteboardTypes';
    function writeSelectionToPasteboard_type(pboard: NSPasteboard; type_: NSString): Boolean; message 'writeSelectionToPasteboard:type:';
    function writeSelectionToPasteboard_types(pboard: NSPasteboard; types: NSArray): Boolean; message 'writeSelectionToPasteboard:types:';
    function readablePasteboardTypes: NSArray; message 'readablePasteboardTypes';
    function preferredPasteboardTypeFromArray_restrictedToTypesFromArray(availableTypes: NSArray; allowedTypes: NSArray): NSString; message 'preferredPasteboardTypeFromArray:restrictedToTypesFromArray:';
    function readSelectionFromPasteboard_type(pboard: NSPasteboard; type_: NSString): Boolean; message 'readSelectionFromPasteboard:type:';
    function readSelectionFromPasteboard(pboard: NSPasteboard): Boolean; message 'readSelectionFromPasteboard:';
    class procedure registerForServices; message 'registerForServices';
    function validRequestorForSendType_returnType(sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure pasteAsPlainText(sender: id); message 'pasteAsPlainText:';
    procedure pasteAsRichText(sender: id); message 'pasteAsRichText:';

    { Category: NSDragging }
    function dragSelectionWithEvent_offset_slideBack(event: NSEvent; mouseOffset: NSSize; slideBack: Boolean): Boolean; message 'dragSelectionWithEvent:offset:slideBack:';
    function dragImageForSelectionWithEvent_origin(event: NSEvent; origin: NSPointPointer): NSImage; message 'dragImageForSelectionWithEvent:origin:';
    function acceptableDragTypes: NSArray; message 'acceptableDragTypes';
    function dragOperationForDraggingInfo_type(dragInfo: id; type_: NSString): NSDragOperation; message 'dragOperationForDraggingInfo:type:';
    procedure cleanUpAfterDragOperation; message 'cleanUpAfterDragOperation';

    { Category: NSSharing }
    function selectedRanges: NSArray; message 'selectedRanges';
    procedure setSelectedRanges_affinity_stillSelecting(ranges: NSArray; affinity: NSSelectionAffinity; stillSelectingFlag: Boolean); message 'setSelectedRanges:affinity:stillSelecting:';
    procedure setSelectedRanges(ranges: NSArray); message 'setSelectedRanges:';
    procedure setSelectedRange_affinity_stillSelecting(charRange: NSRange; affinity: NSSelectionAffinity; stillSelectingFlag: Boolean); message 'setSelectedRange:affinity:stillSelecting:';
    function selectionAffinity: NSSelectionAffinity; message 'selectionAffinity';
    function selectionGranularity: NSSelectionGranularity; message 'selectionGranularity';
    procedure setSelectionGranularity(granularity: NSSelectionGranularity); message 'setSelectionGranularity:';
    procedure setSelectedTextAttributes(attributeDictionary: NSDictionary); message 'setSelectedTextAttributes:';
    function selectedTextAttributes: NSDictionary; message 'selectedTextAttributes';
    procedure setInsertionPointColor(color: NSColor); message 'setInsertionPointColor:';
    function insertionPointColor: NSColor; message 'insertionPointColor';
    procedure updateInsertionPointStateAndRestartTimer(restartFlag: Boolean); message 'updateInsertionPointStateAndRestartTimer:';
    procedure setMarkedTextAttributes(attributeDictionary: NSDictionary); message 'setMarkedTextAttributes:';
    function markedTextAttributes: NSDictionary; message 'markedTextAttributes';
    procedure setLinkTextAttributes(attributeDictionary: NSDictionary); message 'setLinkTextAttributes:';
    function linkTextAttributes: NSDictionary; message 'linkTextAttributes';
    function displaysLinkToolTips: Boolean; message 'displaysLinkToolTips';
    procedure setDisplaysLinkToolTips(flag: Boolean); message 'setDisplaysLinkToolTips:';
    function acceptsGlyphInfo: Boolean; message 'acceptsGlyphInfo';
    procedure setAcceptsGlyphInfo(flag: Boolean); message 'setAcceptsGlyphInfo:';
    procedure setRulerVisible(flag: Boolean); message 'setRulerVisible:';
    function usesRuler: Boolean; message 'usesRuler';
    procedure setUsesRuler(flag: Boolean); message 'setUsesRuler:';
    procedure setContinuousSpellCheckingEnabled(flag: Boolean); message 'setContinuousSpellCheckingEnabled:';
    function isContinuousSpellCheckingEnabled: Boolean; message 'isContinuousSpellCheckingEnabled';
    procedure toggleContinuousSpellChecking(sender: id); message 'toggleContinuousSpellChecking:';
    function spellCheckerDocumentTag: NSInteger; message 'spellCheckerDocumentTag';
    procedure setGrammarCheckingEnabled(flag: Boolean); message 'setGrammarCheckingEnabled:';
    function isGrammarCheckingEnabled: Boolean; message 'isGrammarCheckingEnabled';
    procedure toggleGrammarChecking(sender: id); message 'toggleGrammarChecking:';
    procedure setSpellingState_range(value: NSInteger; charRange: NSRange); message 'setSpellingState:range:';
    function typingAttributes: NSDictionary; message 'typingAttributes';
    procedure setTypingAttributes(attrs: NSDictionary); message 'setTypingAttributes:';
    function shouldChangeTextInRanges_replacementStrings(affectedRanges: NSArray; replacementStrings: NSArray): Boolean; message 'shouldChangeTextInRanges:replacementStrings:';
    function rangesForUserTextChange: NSArray; message 'rangesForUserTextChange';
    function rangesForUserCharacterAttributeChange: NSArray; message 'rangesForUserCharacterAttributeChange';
    function rangesForUserParagraphAttributeChange: NSArray; message 'rangesForUserParagraphAttributeChange';
    function shouldChangeTextInRange_replacementString(affectedCharRange: NSRange; replacementString: NSString): Boolean; message 'shouldChangeTextInRange:replacementString:';
    procedure didChangeText; message 'didChangeText';
    function rangeForUserTextChange: NSRange; message 'rangeForUserTextChange';
    function rangeForUserCharacterAttributeChange: NSRange; message 'rangeForUserCharacterAttributeChange';
    function rangeForUserParagraphAttributeChange: NSRange; message 'rangeForUserParagraphAttributeChange';
    procedure setUsesFindPanel(flag: Boolean); message 'setUsesFindPanel:';
    function usesFindPanel: Boolean; message 'usesFindPanel';
    procedure setAllowsDocumentBackgroundColorChange(flag: Boolean); message 'setAllowsDocumentBackgroundColorChange:';
    function allowsDocumentBackgroundColorChange: Boolean; message 'allowsDocumentBackgroundColorChange';
    procedure setDefaultParagraphStyle(paragraphStyle: NSParagraphStyle); message 'setDefaultParagraphStyle:';
    function defaultParagraphStyle: NSParagraphStyle; message 'defaultParagraphStyle';
    procedure setAllowsUndo(flag: Boolean); message 'setAllowsUndo:';
    function allowsUndo: Boolean; message 'allowsUndo';
    procedure breakUndoCoalescing; message 'breakUndoCoalescing';
    function allowsImageEditing: Boolean; message 'allowsImageEditing';
    procedure setAllowsImageEditing(flag: Boolean); message 'setAllowsImageEditing:';
    procedure showFindIndicatorForRange(charRange: NSRange); message 'showFindIndicatorForRange:';
    function delegate: id; message 'delegate';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function isEditable: Boolean; message 'isEditable';
    procedure setEditable(flag: Boolean); message 'setEditable:';
    function isSelectable: Boolean; message 'isSelectable';
    procedure setSelectable(flag: Boolean); message 'setSelectable:';
    function isRichText: Boolean; message 'isRichText';
    procedure setRichText(flag: Boolean); message 'setRichText:';
    function importsGraphics: Boolean; message 'importsGraphics';
    procedure setImportsGraphics(flag: Boolean); message 'setImportsGraphics:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function isFieldEditor: Boolean; message 'isFieldEditor';
    procedure setFieldEditor(flag: Boolean); message 'setFieldEditor:';
    function usesFontPanel: Boolean; message 'usesFontPanel';
    procedure setUsesFontPanel(flag: Boolean); message 'setUsesFontPanel:';
    function isRulerVisible: Boolean; message 'isRulerVisible';
    procedure setSelectedRange(charRange: NSRange); message 'setSelectedRange:';
    function smartInsertDeleteEnabled: Boolean; message 'smartInsertDeleteEnabled';
    procedure setSmartInsertDeleteEnabled(flag: Boolean); message 'setSmartInsertDeleteEnabled:';
    function smartDeleteRangeForProposedRange(proposedCharRange: NSRange): NSRange; message 'smartDeleteRangeForProposedRange:';
    procedure toggleSmartInsertDelete(sender: id); message 'toggleSmartInsertDelete:';
    procedure smartInsertForString_replacingRange_beforeString_afterString(pasteString: NSString; charRangeToReplace: NSRange; beforeString: NSStringPointer; afterString: NSStringPointer); message 'smartInsertForString:replacingRange:beforeString:afterString:';
    function smartInsertBeforeStringForString_replacingRange(pasteString: NSString; charRangeToReplace: NSRange): NSString; message 'smartInsertBeforeStringForString:replacingRange:';
    function smartInsertAfterStringForString_replacingRange(pasteString: NSString; charRangeToReplace: NSRange): NSString; message 'smartInsertAfterStringForString:replacingRange:';
    procedure setAutomaticQuoteSubstitutionEnabled(flag: Boolean); message 'setAutomaticQuoteSubstitutionEnabled:';
    function isAutomaticQuoteSubstitutionEnabled: Boolean; message 'isAutomaticQuoteSubstitutionEnabled';
    procedure toggleAutomaticQuoteSubstitution(sender: id); message 'toggleAutomaticQuoteSubstitution:';
    procedure setAutomaticLinkDetectionEnabled(flag: Boolean); message 'setAutomaticLinkDetectionEnabled:';
    function isAutomaticLinkDetectionEnabled: Boolean; message 'isAutomaticLinkDetectionEnabled';
    procedure toggleAutomaticLinkDetection(sender: id); message 'toggleAutomaticLinkDetection:';
    function allowedInputSourceLocales: NSArray; message 'allowedInputSourceLocales';
    procedure setAllowedInputSourceLocales(localeIdentifiers: NSArray); message 'setAllowedInputSourceLocales:';
  end; external;

{$endif}
{$endif}

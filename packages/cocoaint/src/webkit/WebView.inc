{ Parsed from Webkit.framework WebView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef WEBVIEW_PAS_T}
{$define WEBVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBVIEW_PAS_R}
{$define WEBVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBVIEW_PAS_F}
{$define WEBVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBVIEW_PAS_S}
{$define WEBVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebView = objcclass;
  WebViewPointer = ^WebView;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBVIEW_PAS_C}
{$define WEBVIEW_PAS_C}

{ WebView }
  WebView = objcclass(NSView)
  private
    __private: WebViewPrivate;
    
  public
    class function alloc: WebView; message 'alloc';

    class function canShowMIMEType(MIMEType: NSStringPointer): Boolean; message 'canShowMIMEType:';
    class function canShowMIMETypeAsHTML(MIMEType: NSStringPointer): Boolean; message 'canShowMIMETypeAsHTML:';
    class function MIMETypesShownAsHTML: NSArray; message 'MIMETypesShownAsHTML';
    class procedure setMIMETypesShownAsHTML(MIMETypes: NSArrayPointer); message 'setMIMETypesShownAsHTML:';
    class function URLFromPasteboard(pasteboard: NSPasteboardPointer): NSURL; message 'URLFromPasteboard:';
    class function URLTitleFromPasteboard(pasteboard: NSPasteboardPointer): NSString; message 'URLTitleFromPasteboard:';
    class procedure registerURLSchemeAsLocal(scheme: NSStringPointer); message 'registerURLSchemeAsLocal:';
    function initWithFrame_frameName_groupName(frame_: NSRect; frameName: NSStringPointer; groupName_: NSStringPointer): id; message 'initWithFrame:frameName:groupName:';
    procedure close; message 'close';
    procedure setShouldCloseWithWindow(close_: Boolean); message 'setShouldCloseWithWindow:';
    function shouldCloseWithWindow: Boolean; message 'shouldCloseWithWindow';
    procedure setUIDelegate(delegate: id); message 'setUIDelegate:';
    function UIDelegate: id; message 'UIDelegate';
    procedure setResourceLoadDelegate(delegate: id); message 'setResourceLoadDelegate:';
    function resourceLoadDelegate: id; message 'resourceLoadDelegate';
    procedure setDownloadDelegate(delegate: id); message 'setDownloadDelegate:';
    function downloadDelegate: id; message 'downloadDelegate';
    procedure setFrameLoadDelegate(delegate: id); message 'setFrameLoadDelegate:';
    function frameLoadDelegate: id; message 'frameLoadDelegate';
    procedure setPolicyDelegate(delegate: id); message 'setPolicyDelegate:';
    function policyDelegate: id; message 'policyDelegate';
    function mainFrame: WebFrame; message 'mainFrame';
    function selectedFrame: WebFrame; message 'selectedFrame';
    function backForwardList: WebBackForwardList; message 'backForwardList';
    procedure setMaintainsBackForwardList(flag: Boolean); message 'setMaintainsBackForwardList:';
    function goBack: Boolean; message 'goBack';
    function goForward: Boolean; message 'goForward';
    function goToBackForwardItem(item: WebHistoryItem): Boolean; message 'goToBackForwardItem:';
    procedure setTextSizeMultiplier(multiplier: single); message 'setTextSizeMultiplier:';
    function textSizeMultiplier: single; message 'textSizeMultiplier';
    procedure setApplicationNameForUserAgent(applicationName: NSStringPointer); message 'setApplicationNameForUserAgent:';
    function applicationNameForUserAgent: NSString; message 'applicationNameForUserAgent';
    procedure setCustomUserAgent(userAgentString: NSStringPointer); message 'setCustomUserAgent:';
    function customUserAgent: NSString; message 'customUserAgent';
    function userAgentForURL(URL: NSURLPointer): NSString; message 'userAgentForURL:';
    function supportsTextEncoding: Boolean; message 'supportsTextEncoding';
    procedure setCustomTextEncodingName(encodingName: NSStringPointer); message 'setCustomTextEncodingName:';
    function customTextEncodingName: NSString; message 'customTextEncodingName';
    procedure setMediaStyle(mediaStyle_: NSStringPointer); message 'setMediaStyle:';
    function mediaStyle: NSString; message 'mediaStyle';
    function stringByEvaluatingJavaScriptFromString(script: NSStringPointer): NSString; message 'stringByEvaluatingJavaScriptFromString:';
    function windowScriptObject: WebScriptObject; message 'windowScriptObject';
    procedure setPreferences(prefs: WebPreferences); message 'setPreferences:';
    function preferences: WebPreferences; message 'preferences';
    procedure setPreferencesIdentifier(anIdentifier: NSStringPointer); message 'setPreferencesIdentifier:';
    function preferencesIdentifier: NSString; message 'preferencesIdentifier';
    procedure setHostWindow(hostWindow_: NSWindowPointer); message 'setHostWindow:';
    function hostWindow: NSWindow; message 'hostWindow';
    function searchFor_direction_caseSensitive_wrap(string_: NSStringPointer; forward: Boolean; caseFlag: Boolean; wrapFlag: Boolean): Boolean; message 'searchFor:direction:caseSensitive:wrap:';
    class procedure registerViewClass_representationClass_forMIMEType(viewClass: Pobjc_class; representationClass: Pobjc_class; MIMEType: NSStringPointer); message 'registerViewClass:representationClass:forMIMEType:';
    procedure setGroupName(groupName_: NSStringPointer); message 'setGroupName:';
    function groupName: NSString; message 'groupName';
    function estimatedProgress: double; message 'estimatedProgress';
    function isLoading: Boolean; message 'isLoading';
    function elementAtPoint(point: NSPoint): NSDictionary; message 'elementAtPoint:';
    function pasteboardTypesForSelection: NSArray; message 'pasteboardTypesForSelection';
    procedure writeSelectionWithPasteboardTypes_toPasteboard(types: NSArrayPointer; pasteboard: NSPasteboardPointer); message 'writeSelectionWithPasteboardTypes:toPasteboard:';
    function pasteboardTypesForElement(element: NSDictionaryPointer): NSArray; message 'pasteboardTypesForElement:';
    procedure writeElement_withPasteboardTypes_toPasteboard(element: NSDictionaryPointer; types: NSArrayPointer; pasteboard: NSPasteboardPointer); message 'writeElement:withPasteboardTypes:toPasteboard:';
    procedure moveDragCaretToPoint(point: NSPoint); message 'moveDragCaretToPoint:';
    procedure removeDragCaret; message 'removeDragCaret';
    procedure setDrawsBackground(drawsBackround: Boolean); message 'setDrawsBackground:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setMainFrameURL(URLString: NSStringPointer); message 'setMainFrameURL:';
    function mainFrameURL: NSString; message 'mainFrameURL';
    function mainFrameDocument: DOMDocument; message 'mainFrameDocument';
    function mainFrameTitle: NSString; message 'mainFrameTitle';
    function mainFrameIcon: NSImage; message 'mainFrameIcon';

    { Category: WebIBActions }
    procedure takeStringURLFrom(sender: id); message 'takeStringURLFrom:';
    procedure stopLoading(sender: id); message 'stopLoading:';
    procedure reload(sender: id); message 'reload:';
    function canGoBack: Boolean; message 'canGoBack';
    function canGoForward: Boolean; message 'canGoForward';
    function canMakeTextLarger: Boolean; message 'canMakeTextLarger';
    procedure makeTextLarger(sender: id); message 'makeTextLarger:';
    function canMakeTextSmaller: Boolean; message 'canMakeTextSmaller';
    procedure makeTextSmaller(sender: id); message 'makeTextSmaller:';
    function canMakeTextStandardSize: Boolean; message 'canMakeTextStandardSize';
    procedure makeTextStandardSize(sender: id); message 'makeTextStandardSize:';
    procedure toggleContinuousSpellChecking(sender: id); message 'toggleContinuousSpellChecking:';
    procedure toggleSmartInsertDelete(sender: id); message 'toggleSmartInsertDelete:';

    { Category: WebViewCSS }
    function computedStyleForElement_pseudoElement(element: DOMElement; pseudoElement: NSStringPointer): DOMCSSStyleDeclaration; message 'computedStyleForElement:pseudoElement:';

    { Category: WebViewEditing }
    function editableDOMRangeForPoint(point: NSPoint): DOMRange; message 'editableDOMRangeForPoint:';
    procedure setSelectedDOMRange_affinity(range: DOMRange; selectionAffinity_: NSSelectionAffinity); message 'setSelectedDOMRange:affinity:';
    function selectedDOMRange: DOMRange; message 'selectedDOMRange';
    function selectionAffinity: NSSelectionAffinity; message 'selectionAffinity';
    function maintainsInactiveSelection: Boolean; message 'maintainsInactiveSelection';
    procedure setEditable(flag: Boolean); message 'setEditable:';
    function isEditable: Boolean; message 'isEditable';
    procedure setTypingStyle(style: DOMCSSStyleDeclaration); message 'setTypingStyle:';
    function typingStyle: DOMCSSStyleDeclaration; message 'typingStyle';
    procedure setSmartInsertDeleteEnabled(flag: Boolean); message 'setSmartInsertDeleteEnabled:';
    function smartInsertDeleteEnabled: Boolean; message 'smartInsertDeleteEnabled';
    procedure setContinuousSpellCheckingEnabled(flag: Boolean); message 'setContinuousSpellCheckingEnabled:';
    function isContinuousSpellCheckingEnabled: Boolean; message 'isContinuousSpellCheckingEnabled';
    function spellCheckerDocumentTag: WebNSInteger; message 'spellCheckerDocumentTag';
    function undoManager: NSUndoManager; message 'undoManager';
    procedure setEditingDelegate(delegate: id); message 'setEditingDelegate:';
    function editingDelegate: id; message 'editingDelegate';
    function styleDeclarationWithText(text: NSStringPointer): DOMCSSStyleDeclaration; message 'styleDeclarationWithText:';

    { Category: WebViewUndoableEditing }
    procedure replaceSelectionWithNode(node: DOMNode); message 'replaceSelectionWithNode:';
    procedure replaceSelectionWithText(text: NSStringPointer); message 'replaceSelectionWithText:';
    procedure replaceSelectionWithMarkupString(markupString: NSStringPointer); message 'replaceSelectionWithMarkupString:';
    procedure replaceSelectionWithArchive(archive: WebArchive); message 'replaceSelectionWithArchive:';
    procedure deleteSelection; message 'deleteSelection';
    procedure applyStyle(style: DOMCSSStyleDeclaration); message 'applyStyle:';

    { Category: WebViewEditingActions }
    procedure copy_(sender: id); message 'copy:';
    procedure cut(sender: id); message 'cut:';
    procedure paste(sender: id); message 'paste:';
    procedure copyFont(sender: id); message 'copyFont:';
    procedure pasteFont(sender: id); message 'pasteFont:';
    procedure delete(sender: id); message 'delete:';
    procedure pasteAsPlainText(sender: id); message 'pasteAsPlainText:';
    procedure pasteAsRichText(sender: id); message 'pasteAsRichText:';
    procedure changeFont(sender: id); message 'changeFont:';
    procedure changeAttributes(sender: id); message 'changeAttributes:';
    procedure changeDocumentBackgroundColor(sender: id); message 'changeDocumentBackgroundColor:';
    procedure changeColor(sender: id); message 'changeColor:';
    procedure alignCenter(sender: id); message 'alignCenter:';
    procedure alignJustified(sender: id); message 'alignJustified:';
    procedure alignLeft(sender: id); message 'alignLeft:';
    procedure alignRight(sender: id); message 'alignRight:';
    procedure checkSpelling(sender: id); message 'checkSpelling:';
    procedure showGuessPanel(sender: id); message 'showGuessPanel:';
    procedure performFindPanelAction(sender: id); message 'performFindPanelAction:';
    procedure startSpeaking(sender: id); message 'startSpeaking:';
    procedure stopSpeaking(sender: id); message 'stopSpeaking:';
    procedure moveToBeginningOfSentence(sender: id); message 'moveToBeginningOfSentence:';
    procedure moveToBeginningOfSentenceAndModifySelection(sender: id); message 'moveToBeginningOfSentenceAndModifySelection:';
    procedure moveToEndOfSentence(sender: id); message 'moveToEndOfSentence:';
    procedure moveToEndOfSentenceAndModifySelection(sender: id); message 'moveToEndOfSentenceAndModifySelection:';
    procedure selectSentence(sender: id); message 'selectSentence:';
    procedure capitalizeWord(sender: id); message 'capitalizeWord:';
    procedure centerSelectionInVisibleArea(sender: id); message 'centerSelectionInVisibleArea:';
    procedure changeCaseOfLetter(sender: id); message 'changeCaseOfLetter:';
    procedure complete(sender: id); message 'complete:';
    procedure deleteBackward(sender: id); message 'deleteBackward:';
    procedure deleteBackwardByDecomposingPreviousCharacter(sender: id); message 'deleteBackwardByDecomposingPreviousCharacter:';
    procedure deleteForward(sender: id); message 'deleteForward:';
    procedure deleteToBeginningOfLine(sender: id); message 'deleteToBeginningOfLine:';
    procedure deleteToBeginningOfParagraph(sender: id); message 'deleteToBeginningOfParagraph:';
    procedure deleteToEndOfLine(sender: id); message 'deleteToEndOfLine:';
    procedure deleteToEndOfParagraph(sender: id); message 'deleteToEndOfParagraph:';
    procedure deleteWordBackward(sender: id); message 'deleteWordBackward:';
    procedure deleteWordForward(sender: id); message 'deleteWordForward:';
    procedure indent(sender: id); message 'indent:';
    procedure insertBacktab(sender: id); message 'insertBacktab:';
    procedure insertNewline(sender: id); message 'insertNewline:';
    procedure insertParagraphSeparator(sender: id); message 'insertParagraphSeparator:';
    procedure insertTab(sender: id); message 'insertTab:';
    procedure lowercaseWord(sender: id); message 'lowercaseWord:';
    procedure moveBackward(sender: id); message 'moveBackward:';
    procedure moveBackwardAndModifySelection(sender: id); message 'moveBackwardAndModifySelection:';
    procedure moveDown(sender: id); message 'moveDown:';
    procedure moveDownAndModifySelection(sender: id); message 'moveDownAndModifySelection:';
    procedure moveForward(sender: id); message 'moveForward:';
    procedure moveForwardAndModifySelection(sender: id); message 'moveForwardAndModifySelection:';
    procedure moveLeft(sender: id); message 'moveLeft:';
    procedure moveLeftAndModifySelection(sender: id); message 'moveLeftAndModifySelection:';
    procedure moveRight(sender: id); message 'moveRight:';
    procedure moveRightAndModifySelection(sender: id); message 'moveRightAndModifySelection:';
    procedure moveToBeginningOfDocument(sender: id); message 'moveToBeginningOfDocument:';
    procedure moveToBeginningOfDocumentAndModifySelection(sender: id); message 'moveToBeginningOfDocumentAndModifySelection:';
    procedure moveToBeginningOfLine(sender: id); message 'moveToBeginningOfLine:';
    procedure moveToBeginningOfLineAndModifySelection(sender: id); message 'moveToBeginningOfLineAndModifySelection:';
    procedure moveToBeginningOfParagraph(sender: id); message 'moveToBeginningOfParagraph:';
    procedure moveToBeginningOfParagraphAndModifySelection(sender: id); message 'moveToBeginningOfParagraphAndModifySelection:';
    procedure moveToEndOfDocument(sender: id); message 'moveToEndOfDocument:';
    procedure moveToEndOfDocumentAndModifySelection(sender: id); message 'moveToEndOfDocumentAndModifySelection:';
    procedure moveToEndOfLine(sender: id); message 'moveToEndOfLine:';
    procedure moveToEndOfLineAndModifySelection(sender: id); message 'moveToEndOfLineAndModifySelection:';
    procedure moveToEndOfParagraph(sender: id); message 'moveToEndOfParagraph:';
    procedure moveToEndOfParagraphAndModifySelection(sender: id); message 'moveToEndOfParagraphAndModifySelection:';
    procedure moveUp(sender: id); message 'moveUp:';
    procedure moveUpAndModifySelection(sender: id); message 'moveUpAndModifySelection:';
    procedure moveWordBackward(sender: id); message 'moveWordBackward:';
    procedure moveWordBackwardAndModifySelection(sender: id); message 'moveWordBackwardAndModifySelection:';
    procedure moveWordForward(sender: id); message 'moveWordForward:';
    procedure moveWordForwardAndModifySelection(sender: id); message 'moveWordForwardAndModifySelection:';
    procedure moveWordLeft(sender: id); message 'moveWordLeft:';
    procedure moveWordLeftAndModifySelection(sender: id); message 'moveWordLeftAndModifySelection:';
    procedure moveWordRight(sender: id); message 'moveWordRight:';
    procedure moveWordRightAndModifySelection(sender: id); message 'moveWordRightAndModifySelection:';
    procedure pageDown(sender: id); message 'pageDown:';
    procedure pageUp(sender: id); message 'pageUp:';
    procedure scrollLineDown(sender: id); message 'scrollLineDown:';
    procedure scrollLineUp(sender: id); message 'scrollLineUp:';
    procedure scrollPageDown(sender: id); message 'scrollPageDown:';
    procedure scrollPageUp(sender: id); message 'scrollPageUp:';
    procedure selectAll(sender: id); message 'selectAll:';
    procedure selectLine(sender: id); message 'selectLine:';
    procedure selectParagraph(sender: id); message 'selectParagraph:';
    procedure selectWord(sender: id); message 'selectWord:';
    procedure uppercaseWord(sender: id); message 'uppercaseWord:';
  end; external;

{$endif}
{$endif}

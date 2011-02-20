{ Parsed from Webkit.framework WebView.h }

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

{ External string constants }
var
  WebElementDOMNodeKey: NSString; cvar; external;
  WebElementFrameKey: NSString; cvar; external;
  WebElementImageAltStringKey: NSString; cvar; external;
  WebElementImageKey: NSString; cvar; external;
  WebElementImageRectKey: NSString; cvar; external;
  WebElementImageURLKey: NSString; cvar; external;
  WebElementIsSelectedKey: NSString; cvar; external;
  WebElementLinkURLKey: NSString; cvar; external;
  WebElementLinkTargetFrameKey: NSString; cvar; external;
  WebElementLinkTitleKey: NSString; cvar; external;
  WebElementLinkLabelKey: NSString; cvar; external;
  WebViewProgressStartedNotification: NSString; cvar; external;
  WebViewProgressEstimateChangedNotification: NSString; cvar; external;
  WebViewProgressFinishedNotification: NSString; cvar; external;
  WebViewDidBeginEditingNotification: NSString; cvar; external;
  WebViewDidChangeNotification: NSString; cvar; external;
  WebViewDidEndEditingNotification: NSString; cvar; external;
  WebViewDidChangeTypingStyleNotification: NSString; cvar; external;
  WebViewDidChangeSelectionNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  WebView = objcclass;
  WebViewPointer = ^WebView;
  WebViewPtr = WebViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBVIEW_PAS_C}
{$define WEBVIEW_PAS_C}

{ WebView }
  WebView = objcclass external (NSView)
  private
    _private: WebViewPrivate;
    
  public
    class function canShowMIMEType(MIMEType: NSString): Boolean; message 'canShowMIMEType:';
    class function canShowMIMETypeAsHTML(MIMEType: NSString): Boolean; message 'canShowMIMETypeAsHTML:';
    class function MIMETypesShownAsHTML: NSArray; message 'MIMETypesShownAsHTML';
    class procedure setMIMETypesShownAsHTML(MIMETypes: NSArray); message 'setMIMETypesShownAsHTML:';
    class function URLFromPasteboard(pasteboard: NSPasteboardPtr): NSURL; message 'URLFromPasteboard:';
    class function URLTitleFromPasteboard(pasteboard: NSPasteboardPtr): NSString; message 'URLTitleFromPasteboard:';
    class procedure registerURLSchemeAsLocal(scheme: NSString); message 'registerURLSchemeAsLocal:';
    function initWithFrame_frameName_groupName(frame_: NSRect; frameName: NSString; groupName_: NSString): id; message 'initWithFrame:frameName:groupName:';
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
    procedure setApplicationNameForUserAgent(applicationName: NSString); message 'setApplicationNameForUserAgent:';
    function applicationNameForUserAgent: NSString; message 'applicationNameForUserAgent';
    procedure setCustomUserAgent(userAgentString: NSString); message 'setCustomUserAgent:';
    function customUserAgent: NSString; message 'customUserAgent';
    function userAgentForURL(URL: NSURL): NSString; message 'userAgentForURL:';
    function supportsTextEncoding: Boolean; message 'supportsTextEncoding';
    procedure setCustomTextEncodingName(encodingName: NSString); message 'setCustomTextEncodingName:';
    function customTextEncodingName: NSString; message 'customTextEncodingName';
    procedure setMediaStyle(mediaStyle_: NSString); message 'setMediaStyle:';
    function mediaStyle: NSString; message 'mediaStyle';
    function stringByEvaluatingJavaScriptFromString(script: NSString): NSString; message 'stringByEvaluatingJavaScriptFromString:';
    function windowScriptObject: WebScriptObject; message 'windowScriptObject';
    procedure setPreferences(prefs: WebPreferences); message 'setPreferences:';
    function preferences: WebPreferences; message 'preferences';
    procedure setPreferencesIdentifier(anIdentifier: NSString); message 'setPreferencesIdentifier:';
    function preferencesIdentifier: NSString; message 'preferencesIdentifier';
    procedure setHostWindow(hostWindow_: NSWindowPtr); message 'setHostWindow:';
    function hostWindow: NSWindowPtr; message 'hostWindow';
    function searchFor_direction_caseSensitive_wrap(string_: NSString; forward: Boolean; caseFlag: Boolean; wrapFlag: Boolean): Boolean; message 'searchFor:direction:caseSensitive:wrap:';
    class procedure registerViewClass_representationClass_forMIMEType(viewClass: Pobjc_class; representationClass: Pobjc_class; MIMEType: NSString); message 'registerViewClass:representationClass:forMIMEType:';
    procedure setGroupName(groupName_: NSString); message 'setGroupName:';
    function groupName: NSString; message 'groupName';
    function estimatedProgress: double; message 'estimatedProgress';
    function isLoading: Boolean; message 'isLoading';
    function elementAtPoint(point: NSPoint): NSDictionary; message 'elementAtPoint:';
    function pasteboardTypesForSelection: NSArray; message 'pasteboardTypesForSelection';
    procedure writeSelectionWithPasteboardTypes_toPasteboard(types: NSArray; pasteboard: NSPasteboardPtr); message 'writeSelectionWithPasteboardTypes:toPasteboard:';
    function pasteboardTypesForElement(element: NSDictionary): NSArray; message 'pasteboardTypesForElement:';
    procedure writeElement_withPasteboardTypes_toPasteboard(element: NSDictionary; types: NSArray; pasteboard: NSPasteboardPtr); message 'writeElement:withPasteboardTypes:toPasteboard:';
    procedure moveDragCaretToPoint(point: NSPoint); message 'moveDragCaretToPoint:';
    procedure removeDragCaret; message 'removeDragCaret';
    procedure setDrawsBackground(drawsBackround: Boolean); message 'setDrawsBackground:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setShouldUpdateWhileOffscreen(updateWhileOffscreen: Boolean); message 'setShouldUpdateWhileOffscreen:';
    function shouldUpdateWhileOffscreen: Boolean; message 'shouldUpdateWhileOffscreen';
    procedure setMainFrameURL(URLString: NSString); message 'setMainFrameURL:';
    function mainFrameURL: NSString; message 'mainFrameURL';
    function mainFrameDocument: DOMDocument; message 'mainFrameDocument';
    function mainFrameTitle: NSString; message 'mainFrameTitle';
    function mainFrameIcon: NSImage; message 'mainFrameIcon';
  end;

{ WebIBActionsCategory }
  WebIBActionsCategory = objccategory external (WebView)
    procedure takeStringURLFrom(sender: id); message 'takeStringURLFrom:';
    procedure stopLoading(sender: id); message 'stopLoading:';
    procedure reload(sender: id); message 'reload:';
    procedure reloadFromOrigin(sender: id); message 'reloadFromOrigin:';
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
  end;

{ WebViewCSSCategory }
  WebViewCSSCategory = objccategory external (WebView)
    function computedStyleForElement_pseudoElement(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'computedStyleForElement:pseudoElement:';
  end;

{ WebViewEditingCategory }
  WebViewEditingCategory = objccategory external (WebView)
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
    function styleDeclarationWithText(text: NSString): DOMCSSStyleDeclaration; message 'styleDeclarationWithText:';
  end;

{ WebViewUndoableEditingCategory }
  WebViewUndoableEditingCategory = objccategory external (WebView)
    procedure replaceSelectionWithNode(node: DOMNode); message 'replaceSelectionWithNode:';
    procedure replaceSelectionWithText(text: NSString); message 'replaceSelectionWithText:';
    procedure replaceSelectionWithMarkupString(markupString: NSString); message 'replaceSelectionWithMarkupString:';
    procedure replaceSelectionWithArchive(archive: WebArchive); message 'replaceSelectionWithArchive:';
    procedure deleteSelection; message 'deleteSelection';
    procedure applyStyle(style: DOMCSSStyleDeclaration); message 'applyStyle:';
  end;

{ WebViewEditingActionsCategory }
  WebViewEditingActionsCategory = objccategory external (WebView)
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
  end;

{$endif}
{$endif}

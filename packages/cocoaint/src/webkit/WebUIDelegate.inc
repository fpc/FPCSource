{ Parsed from Webkit.framework WebUIDelegate.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:41 CET 2011 }


{$ifdef TYPES}
{$ifndef WEBUIDELEGATE_PAS_T}
{$define WEBUIDELEGATE_PAS_T}

{ Constants }

const
  WebMenuItemTagOpenLinkInNewWindow = 1;
  WebMenuItemTagDownloadLinkToDisk = 0;
  WebMenuItemTagCopyLinkToClipboard = 1;
  WebMenuItemTagOpenImageInNewWindow = 2;
  WebMenuItemTagDownloadImageToDisk = 3;
  WebMenuItemTagCopyImageToClipboard = 4;
  WebMenuItemTagOpenFrameInNewWindow = 5;
  WebMenuItemTagCopy = 6;
  WebMenuItemTagGoBack = 7;
  WebMenuItemTagGoForward = 8;
  WebMenuItemTagStop = 9;
  WebMenuItemTagReload = 10;
  WebMenuItemTagCut = 11;
  WebMenuItemTagPaste = 12;
  WebMenuItemTagSpellingGuess = 13;
  WebMenuItemTagNoGuessesFound = 14;
  WebMenuItemTagIgnoreSpelling = 15;
  WebMenuItemTagLearnSpelling = 16;
  WebMenuItemTagOther = 17;
  WebMenuItemTagSearchInSpotlight = 18;
  WebMenuItemTagSearchWeb = 19;
  WebMenuItemTagLookUpInDictionary = 20;
  WebMenuItemTagOpenWithDefaultApplication = 21;
  WebMenuItemPDFActualSize = 22;
  WebMenuItemPDFZoomIn = 23;
  WebMenuItemPDFZoomOut = 24;
  WebMenuItemPDFAutoSize = 25;
  WebMenuItemPDFSinglePage = 26;
  WebMenuItemPDFFacingPages = 27;
  WebMenuItemPDFContinuous = 28;
  WebMenuItemPDFNextPage = 29;
  WebMenuItemPDFPreviousPage = 30;

const
  WebDragDestinationActionNone = 0;
  WebDragDestinationActionDHTML = 1;
  WebDragDestinationActionEdit = 2;
  WebDragDestinationActionLoad = 4;
  WebDragDestinationActionAny = UINT_MAX;

const
  WebDragSourceActionNone = 0;
  WebDragSourceActionDHTML = 1;
  WebDragSourceActionImage = 2;
  WebDragSourceActionLink = 4;
  WebDragSourceActionSelection = 8;
  WebDragSourceActionAny = UINT_MAX;

{ Types }
type
  
  WebDragDestinationAction = culong;
  WebDragSourceAction = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBUIDELEGATE_PAS_R}
{$define WEBUIDELEGATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBUIDELEGATE_PAS_F}
{$define WEBUIDELEGATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBUIDELEGATE_PAS_S}
{$define WEBUIDELEGATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebOpenPanelResultListenerProtocol = objcprotocol;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBUIDELEGATE_PAS_C}
{$define WEBUIDELEGATE_PAS_C}

{ WebUIDelegateCategory }
  WebUIDelegateCategory = objccategory external (NSObject)
    function webView_createWebViewWithRequest(sender: WebView; request: NSURLRequest): WebView; message 'webView:createWebViewWithRequest:';
    procedure webViewShow(sender: WebView); message 'webViewShow:';
    function webView_createWebViewModalDialogWithRequest(sender: WebView; request: NSURLRequest): WebView; message 'webView:createWebViewModalDialogWithRequest:';
    procedure webViewRunModal(sender: WebView); message 'webViewRunModal:';
    procedure webViewClose(sender: WebView); message 'webViewClose:';
    procedure webViewFocus(sender: WebView); message 'webViewFocus:';
    procedure webViewUnfocus(sender: WebView); message 'webViewUnfocus:';
    function webViewFirstResponder(sender: WebView): NSResponderPtr; message 'webViewFirstResponder:';
    procedure webView_makeFirstResponder(sender: WebView; responder: NSResponderPtr); message 'webView:makeFirstResponder:';
    procedure webView_setStatusText(sender: WebView; text: NSString); message 'webView:setStatusText:';
    function webViewStatusText(sender: WebView): NSString; message 'webViewStatusText:';
    function webViewAreToolbarsVisible(sender: WebView): Boolean; message 'webViewAreToolbarsVisible:';
    procedure webView_setToolbarsVisible(sender: WebView; visible: Boolean); message 'webView:setToolbarsVisible:';
    function webViewIsStatusBarVisible(sender: WebView): Boolean; message 'webViewIsStatusBarVisible:';
    procedure webView_setStatusBarVisible(sender: WebView; visible: Boolean); message 'webView:setStatusBarVisible:';
    function webViewIsResizable(sender: WebView): Boolean; message 'webViewIsResizable:';
    procedure webView_setResizable(sender: WebView; resizable: Boolean); message 'webView:setResizable:';
    procedure webView_setFrame(sender: WebView; frame: NSRect); message 'webView:setFrame:';
    function webViewFrame(sender: WebView): NSRect; message 'webViewFrame:';
    procedure webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame(sender: WebView; message: NSString; frame: WebFrame); message 'webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:';
    function webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame(sender: WebView; message: NSString; frame: WebFrame): Boolean; message 'webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:';
    function webView_runJavaScriptTextInputPanelWithPrompt_defaultText_initiatedByFrame(sender: WebView; prompt: NSString; defaultText: NSString; frame: WebFrame): NSString; message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:';
    function webView_runBeforeUnloadConfirmPanelWithMessage_initiatedByFrame(sender: WebView; message: NSString; frame: WebFrame): Boolean; message 'webView:runBeforeUnloadConfirmPanelWithMessage:initiatedByFrame:';
    procedure webView_runOpenPanelForFileButtonWithResultListener(sender: WebView; resultListener: id); message 'webView:runOpenPanelForFileButtonWithResultListener:';
    procedure webView_runOpenPanelForFileButtonWithResultListener_allowMultipleFiles(sender: WebView; resultListener: id; allowMultipleFiles: Boolean); message 'webView:runOpenPanelForFileButtonWithResultListener:allowMultipleFiles:';
    procedure webView_mouseDidMoveOverElement_modifierFlags(sender: WebView; elementInformation: NSDictionary; modifierFlags: WebNSUInteger); message 'webView:mouseDidMoveOverElement:modifierFlags:';
    function webView_contextMenuItemsForElement_defaultMenuItems(sender: WebView; element: NSDictionary; defaultMenuItems: NSArray): NSArray; message 'webView:contextMenuItemsForElement:defaultMenuItems:';
    function webView_validateUserInterfaceItem_defaultValidation(webView_: WebView; item: id; defaultValidation: Boolean): Boolean; message 'webView:validateUserInterfaceItem:defaultValidation:';
    function webView_shouldPerformAction_fromSender(webView_: WebView; action: SEL; sender: id): Boolean; message 'webView:shouldPerformAction:fromSender:';
    function webView_dragDestinationActionMaskForDraggingInfo(webView_: WebView; draggingInfo: id): WebNSUInteger; message 'webView:dragDestinationActionMaskForDraggingInfo:';
    procedure webView_willPerformDragDestinationAction_forDraggingInfo(webView_: WebView; action: WebDragDestinationAction; draggingInfo: id); message 'webView:willPerformDragDestinationAction:forDraggingInfo:';
    function webView_dragSourceActionMaskForPoint(webView_: WebView; point: NSPoint): WebNSUInteger; message 'webView:dragSourceActionMaskForPoint:';
    procedure webView_willPerformDragSourceAction_fromPoint_withPasteboard(webView_: WebView; action: WebDragSourceAction; point: NSPoint; pasteboard: NSPasteboardPtr); message 'webView:willPerformDragSourceAction:fromPoint:withPasteboard:';
    procedure webView_printFrameView(sender: WebView; frameView: WebFrameView); message 'webView:printFrameView:';
    function webViewHeaderHeight(sender: WebView): single; message 'webViewHeaderHeight:';
    function webViewFooterHeight(sender: WebView): single; message 'webViewFooterHeight:';
    procedure webView_drawHeaderInRect(sender: WebView; rect: NSRect); message 'webView:drawHeaderInRect:';
    procedure webView_drawFooterInRect(sender: WebView; rect: NSRect); message 'webView:drawFooterInRect:';
    procedure webView_runJavaScriptAlertPanelWithMessage(sender: WebView; message: NSString); message 'webView:runJavaScriptAlertPanelWithMessage:';
    function webView_runJavaScriptConfirmPanelWithMessage(sender: WebView; message: NSString): Boolean; message 'webView:runJavaScriptConfirmPanelWithMessage:';
    function webView_runJavaScriptTextInputPanelWithPrompt_defaultText(sender: WebView; prompt: NSString; defaultText: NSString): NSString; message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:';
    procedure webView_setContentRect(sender: WebView; frame: NSRect); message 'webView:setContentRect:';
    function webViewContentRect(sender: WebView): NSRect; message 'webViewContentRect:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef WEBUIDELEGATE_PAS_P}
{$define WEBUIDELEGATE_PAS_P}
  
{ WebOpenPanelResultListener Protocol }
  WebOpenPanelResultListenerProtocol = objcprotocol external name 'WebOpenPanelResultListener'
    procedure chooseFilename(fileName: NSString); message 'chooseFilename:';
    procedure chooseFilenames(fileNames: NSArray); message 'chooseFilenames:';
    procedure cancel; message 'cancel';
  end;
{$endif}
{$endif}

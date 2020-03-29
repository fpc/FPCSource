{ Parsed from WebKit.framework WebUIDelegate.h }


{$ifdef TYPES}
type
  WebOpenPanelResultListenerProtocolPtr = ^WebOpenPanelResultListenerProtocol;
{$endif}

{$ifdef TYPES}
const
  WebMenuItemTagOpenLinkInNewWindow = 1;
  WebMenuItemTagDownloadLinkToDisk = 2;
  WebMenuItemTagCopyLinkToClipboard = 3;
  WebMenuItemTagOpenImageInNewWindow = 4;
  WebMenuItemTagDownloadImageToDisk = 5;
  WebMenuItemTagCopyImageToClipboard = 6;
  WebMenuItemTagOpenFrameInNewWindow = 7;
  WebMenuItemTagCopy = 8;
  WebMenuItemTagGoBack = 9;
  WebMenuItemTagGoForward = 10;
  WebMenuItemTagStop = 11;
  WebMenuItemTagReload = 12;
  WebMenuItemTagCut = 13;
  WebMenuItemTagPaste = 14;
  WebMenuItemTagSpellingGuess = 15;
  WebMenuItemTagNoGuessesFound = 16;
  WebMenuItemTagIgnoreSpelling = 17;
  WebMenuItemTagLearnSpelling = 18;
  WebMenuItemTagOther = 19;
  WebMenuItemTagSearchInSpotlight = 20;
  WebMenuItemTagSearchWeb = 21;
  WebMenuItemTagLookUpInDictionary = 22;
  WebMenuItemTagOpenWithDefaultApplication = 23;
  WebMenuItemPDFActualSize = 24;
  WebMenuItemPDFZoomIn = 25;
  WebMenuItemPDFZoomOut = 26;
  WebMenuItemPDFAutoSize = 27;
  WebMenuItemPDFSinglePage = 28;
  WebMenuItemPDFFacingPages = 29;
  WebMenuItemPDFContinuous = 30;
  WebMenuItemPDFNextPage = 31;
  WebMenuItemPDFPreviousPage = 32;

type
  WebDragDestinationAction = NSUInteger;
  WebDragDestinationActionPtr = ^WebDragDestinationAction;

const
  WebDragDestinationActionNone = 0;
  WebDragDestinationActionDHTML = 1;
  WebDragDestinationActionEdit = 2;
  WebDragDestinationActionLoad = 4;
  WebDragDestinationActionAny = UINT_MAX;

type
  WebDragSourceAction = NSUInteger;
  WebDragSourceActionPtr = ^WebDragSourceAction;

const
  WebDragSourceActionNone = 0;
  WebDragSourceActionDHTML = 1;
  WebDragSourceActionImage = 2;
  WebDragSourceActionLink = 4;
  WebDragSourceActionSelection = 8;
  WebDragSourceActionAny = UINT_MAX;
{$endif}

{$ifdef PROTOCOLS}

type
  WebOpenPanelResultListenerProtocol = objcprotocol external name 'WebOpenPanelResultListener' (NSObjectProtocol)
    procedure chooseFilename (fileName: NSString); message 'chooseFilename:';
    procedure chooseFilenames (fileNames: NSArray); message 'chooseFilenames:'; { available in 10_6 }
    procedure cancel; message 'cancel';
  end;
{$endif}

{$ifdef CLASSES}

type
  WebUIDelegate = objccategory external (NSObject)
    function webView_createWebViewWithRequest (sender: WebView; request: NSURLRequest): WebView; message 'webView:createWebViewWithRequest:';
    procedure webViewShow (sender: WebView); message 'webViewShow:';
    function webView_createWebViewModalDialogWithRequest (sender: WebView; request: NSURLRequest): WebView; message 'webView:createWebViewModalDialogWithRequest:';
    procedure webViewRunModal (sender: WebView); message 'webViewRunModal:';
    procedure webViewClose (sender: WebView); message 'webViewClose:';
    procedure webViewFocus (sender: WebView); message 'webViewFocus:';
    procedure webViewUnfocus (sender: WebView); message 'webViewUnfocus:';
    function webViewFirstResponder (sender: WebView): NSResponder; message 'webViewFirstResponder:';
    procedure webView_makeFirstResponder (sender: WebView; responder: NSResponder); message 'webView:makeFirstResponder:';
    procedure webView_setStatusText (sender: WebView; text: NSString); message 'webView:setStatusText:';
    function webViewStatusText (sender: WebView): NSString; message 'webViewStatusText:';
    function webViewAreToolbarsVisible (sender: WebView): ObjCBOOL; message 'webViewAreToolbarsVisible:';
    procedure webView_setToolbarsVisible (sender: WebView; visible: ObjCBOOL); message 'webView:setToolbarsVisible:';
    function webViewIsStatusBarVisible (sender: WebView): ObjCBOOL; message 'webViewIsStatusBarVisible:';
    procedure webView_setStatusBarVisible (sender: WebView; visible: ObjCBOOL); message 'webView:setStatusBarVisible:';
    function webViewIsResizable (sender: WebView): ObjCBOOL; message 'webViewIsResizable:';
    procedure webView_setResizable (sender: WebView; resizable: ObjCBOOL); message 'webView:setResizable:';
    procedure webView_setFrame (sender: WebView; frame: NSRect); message 'webView:setFrame:';
    function webViewFrame (sender: WebView): NSRect; message 'webViewFrame:';
    procedure webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame (sender: WebView; message_: NSString; frame: WebFrame); message 'webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:';
    function webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame (sender: WebView; message_: NSString; frame: WebFrame): ObjCBOOL; message 'webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:';
    function webView_runJavaScriptTextInputPanelWithPrompt_defaultText_initiatedByFrame (sender: WebView; prompt: NSString; defaultText: NSString; frame: WebFrame): NSString; message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:';
    function webView_runBeforeUnloadConfirmPanelWithMessage_initiatedByFrame (sender: WebView; message_: NSString; frame: WebFrame): ObjCBOOL; message 'webView:runBeforeUnloadConfirmPanelWithMessage:initiatedByFrame:';
    procedure webView_runOpenPanelForFileButtonWithResultListener (sender: WebView; resultListener: WebOpenPanelResultListenerProtocol); message 'webView:runOpenPanelForFileButtonWithResultListener:';
    procedure webView_runOpenPanelForFileButtonWithResultListener_allowMultipleFiles (sender: WebView; resultListener: WebOpenPanelResultListenerProtocol; allowMultipleFiles: ObjCBOOL); message 'webView:runOpenPanelForFileButtonWithResultListener:allowMultipleFiles:'; { available in 10_6 }
    procedure webView_mouseDidMoveOverElement_modifierFlags (sender: WebView; elementInformation: NSDictionary; modifierFlags: NSUInteger); message 'webView:mouseDidMoveOverElement:modifierFlags:';
    function webView_contextMenuItemsForElement_defaultMenuItems (sender: WebView; element: NSDictionary; defaultMenuItems: NSArray): NSArray; message 'webView:contextMenuItemsForElement:defaultMenuItems:';
    function webView_validateUserInterfaceItem_defaultValidation (webView_: WebView; item: NSValidatedUserInterfaceItemProtocol; defaultValidation: ObjCBOOL): ObjCBOOL; message 'webView:validateUserInterfaceItem:defaultValidation:';
    function webView_shouldPerformAction_fromSender (webView_: WebView; action: SEL; sender: id): ObjCBOOL; message 'webView:shouldPerformAction:fromSender:';
    function webView_dragDestinationActionMaskForDraggingInfo (webView_: WebView; draggingInfo: NSDraggingInfoProtocol): NSUInteger; message 'webView:dragDestinationActionMaskForDraggingInfo:';
    procedure webView_willPerformDragDestinationAction_forDraggingInfo (webView_: WebView; action: WebDragDestinationAction; draggingInfo: NSDraggingInfoProtocol); message 'webView:willPerformDragDestinationAction:forDraggingInfo:';
    function webView_dragSourceActionMaskForPoint (webView_: WebView; point: NSPoint): NSUInteger; message 'webView:dragSourceActionMaskForPoint:';
    procedure webView_willPerformDragSourceAction_fromPoint_withPasteboard (webView_: WebView; action: WebDragSourceAction; point: NSPoint; pasteboard: NSPasteboard); message 'webView:willPerformDragSourceAction:fromPoint:withPasteboard:';
    procedure webView_printFrameView (sender: WebView; frameView: WebFrameView); message 'webView:printFrameView:';
    function webViewHeaderHeight (sender: WebView): single; message 'webViewHeaderHeight:';
    function webViewFooterHeight (sender: WebView): single; message 'webViewFooterHeight:';
    procedure webView_drawHeaderInRect (sender: WebView; rect: NSRect); message 'webView:drawHeaderInRect:';
    procedure webView_drawFooterInRect (sender: WebView; rect: NSRect); message 'webView:drawFooterInRect:';
    procedure webView_runJavaScriptAlertPanelWithMessage (sender: WebView; message_: NSString); message 'webView:runJavaScriptAlertPanelWithMessage:'; deprecated 'in 10_3, 10_5';
    function webView_runJavaScriptConfirmPanelWithMessage (sender: WebView; message_: NSString): ObjCBOOL; message 'webView:runJavaScriptConfirmPanelWithMessage:'; deprecated 'in 10_3, 10_5';
    function webView_runJavaScriptTextInputPanelWithPrompt_defaultText (sender: WebView; prompt: NSString; defaultText: NSString): NSString; message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:'; deprecated 'in 10_3, 10_5';
    procedure webView_setContentRect (sender: WebView; frame: NSRect); message 'webView:setContentRect:'; deprecated 'in 10_3, 10_5';
    function webViewContentRect (sender: WebView): NSRect; message 'webViewContentRect:'; deprecated 'in 10_3, 10_5';
  end;
{$endif}


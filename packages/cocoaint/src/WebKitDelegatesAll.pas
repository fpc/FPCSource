{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:45:10 ICT 2009 }

unit WebKitDelegatesAll;
interface

{ Copy and paste these delegate methods into your real classes. }

type
  WebDownloadDelegate = objccategory (NSObject)
    function downloadWindowForAuthenticationSheet(download: WebDownload): NSWindow; message 'downloadWindowForAuthenticationSheet:';
  end;

type
  WebFrameLoadDelegate = objccategory (NSObject)
    procedure webView_didCancelClientRedirectForFrame(sender: webView_Pointer; frame: WebFrame); message 'webView:didCancelClientRedirectForFrame:';
    procedure webView_didChangeLocationWithinPageForFrame(sender: webView_Pointer; frame: WebFrame); message 'webView:didChangeLocationWithinPageForFrame:';
    procedure webView_didClearWindowObject_forFrame(webView_: webView_Pointer; windowObject_: WebScriptObject; frame: WebFrame); message 'webView:didClearWindowObject:forFrame:';
    procedure webView_didCommitLoadForFrame(sender: webView_Pointer; frame: WebFrame); message 'webView:didCommitLoadForFrame:';
    procedure webView_didFailLoadWithError_forFrame(sender: webView_Pointer; error: NSErrorPointer; frame: WebFrame); message 'webView:didFailLoadWithError:forFrame:';
    procedure webView_didFailProvisionalLoadWithError_forFrame(sender: webView_Pointer; error: NSErrorPointer; frame: WebFrame); message 'webView:didFailProvisionalLoadWithError:forFrame:';
    procedure webView_didFinishLoadForFrame(sender: webView_Pointer; frame: WebFrame); message 'webView:didFinishLoadForFrame:';
    procedure webView_didReceiveIcon_forFrame(sender: webView_Pointer; image: NSImagePointer; frame: WebFrame); message 'webView:didReceiveIcon:forFrame:';
    procedure webView_didReceiveServerRedirectForProvisionalLoadForFrame(sender: webView_Pointer; frame: WebFrame); message 'webView:didReceiveServerRedirectForProvisionalLoadForFrame:';
    procedure webView_didReceiveTitle_forFrame(sender: webView_Pointer; title: NSStringPointer; frame: WebFrame); message 'webView:didReceiveTitle:forFrame:';
    procedure webView_didStartProvisionalLoadForFrame(sender: webView_Pointer; frame: WebFrame); message 'webView:didStartProvisionalLoadForFrame:';
    procedure webView_willCloseFrame(sender: webView_Pointer; frame: WebFrame); message 'webView:willCloseFrame:';
    procedure webView_willPerformClientRedirectToURL_delay_fireDate_forFrame(sender: webView_Pointer; URL: NSURLPointer; seconds: NSTimeInterval; date: NSDatePointer; frame: WebFrame); message 'webView:willPerformClientRedirectToURL:delay:fireDate:forFrame:';
    procedure webView_windowScriptObjectAvailable(webView_: webView_Pointer; windowScriptObject: WebScriptObject); message 'webView:windowScriptObjectAvailable:';
  end;

type
  WebPolicyDelegate = objccategory (NSObject)
    procedure webView_decidePolicyForMIMEType_request_frame_decisionListener; message 'webView:decidePolicyForMIMEType:request:frame:decisionListener:';
    procedure webView_decidePolicyForNavigationAction_request_frame_decisionListener; message 'webView:decidePolicyForNavigationAction:request:frame:decisionListener:';
    procedure webView_decidePolicyForNewWindowAction_request_newFrameName_decisionListener; message 'webView:decidePolicyForNewWindowAction:request:newFrameName:decisionListener:';
    procedure webView_unableToImplementPolicyWithError_frame(webView: WebView; error: NSErrorPointer; frame: WebFrame); message 'webView:unableToImplementPolicyWithError:frame:';
  end;

type
  WebResourceLoadDelegate = objccategory (NSObject)
    function webView_identifierForInitialRequest_fromDataSource(sender: WebView; request: NSURLRequestPointer; dataSource: WebDataSource): id; message 'webView:identifierForInitialRequest:fromDataSource:';
    procedure webView_plugInFailedWithError_dataSource(sender: WebView; error: NSErrorPointer; dataSource: WebDataSource); message 'webView:plugInFailedWithError:dataSource:';
    procedure webView_resource_didCancelAuthenticationChallenge_fromDataSource(sender: WebView; identifier: id; challenge: NSURLAuthenticationChallengePointer; dataSource: WebDataSource); message 'webView:resource:didCancelAuthenticationChallenge:fromDataSource:';
    procedure webView_resource_didFailLoadingWithError_fromDataSource(sender: WebView; identifier: id; error: NSErrorPointer; dataSource: WebDataSource); message 'webView:resource:didFailLoadingWithError:fromDataSource:';
    procedure webView_resource_didFinishLoadingFromDataSource(sender: WebView; identifier: id; dataSource: WebDataSource); message 'webView:resource:didFinishLoadingFromDataSource:';
    procedure webView_resource_didReceiveAuthenticationChallenge_fromDataSource(sender: WebView; identifier: id; challenge: NSURLAuthenticationChallengePointer; dataSource: WebDataSource); message 'webView:resource:didReceiveAuthenticationChallenge:fromDataSource:';
    procedure webView_resource_didReceiveContentLength_fromDataSource(sender: WebView; identifier: id; length: WebNSInteger; dataSource: WebDataSource); message 'webView:resource:didReceiveContentLength:fromDataSource:';
    procedure webView_resource_didReceiveResponse_fromDataSource(sender: WebView; identifier: id; response: NSURLResponsePointer; dataSource: WebDataSource); message 'webView:resource:didReceiveResponse:fromDataSource:';
    function webView_resource_willSendRequest_redirectResponse_fromDataSource(sender: WebView; identifier: id; request: NSURLRequestPointer; redirectResponse: NSURLResponsePointer; dataSource: WebDataSource): NSURLRequest; message 'webView:resource:willSendRequest:redirectResponse:fromDataSource:';
  end;

type
  WebUIDelegate = objccategory (NSObject)
    function webView_contextMenuItemsForElement_defaultMenuItems(sender: WebView; element: NSDictionaryPointer; defaultMenuItems: NSArrayPointer): NSArray; message 'webView:contextMenuItemsForElement:defaultMenuItems:';
    function webView_createWebViewModalDialogWithRequest(sender: WebView; request: NSURLRequestPointer): WebView; message 'webView:createWebViewModalDialogWithRequest:';
    function webView_createWebViewWithRequest(sender: WebView; request: NSURLRequestPointer): WebView; message 'webView:createWebViewWithRequest:';
    function webView_dragDestinationActionMaskForDraggingInfo(webView: WebView; draggingInfo: id): WebNSUInteger; message 'webView:dragDestinationActionMaskForDraggingInfo:';
    function webView_dragSourceActionMaskForPoint(webView: WebView; point: NSPoint): WebNSUInteger; message 'webView:dragSourceActionMaskForPoint:';
    procedure webView_drawFooterInRect(sender: WebView; rect: NSRect); message 'webView:drawFooterInRect:';
    procedure webView_drawHeaderInRect(sender: WebView; rect: NSRect); message 'webView:drawHeaderInRect:';
    procedure webView_makeFirstResponder(sender: WebView; responder: NSResponderPointer); message 'webView:makeFirstResponder:';
    procedure webView_mouseDidMoveOverElement_modifierFlags(sender: WebView; elementInformation: NSDictionaryPointer; modifierFlags: WebNSUInteger); message 'webView:mouseDidMoveOverElement:modifierFlags:';
    procedure webView_printFrameView(sender: WebView; frameView: WebFrameView); message 'webView:printFrameView:';
    function webView_runBeforeUnloadConfirmPanelWithMessage_initiatedByFrame(sender: WebView; message: NSStringPointer; frame: WebFrame): Boolean; message 'webView:runBeforeUnloadConfirmPanelWithMessage:initiatedByFrame:';
    procedure webView_runJavaScriptAlertPanelWithMessage(sender: WebView; message: NSStringPointer); message 'webView:runJavaScriptAlertPanelWithMessage:';
    procedure webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame(sender: WebView; message: NSStringPointer; frame: WebFrame); message 'webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:';
    function webView_runJavaScriptConfirmPanelWithMessage(sender: WebView; message: NSStringPointer): Boolean; message 'webView:runJavaScriptConfirmPanelWithMessage:';
    function webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame(sender: WebView; message: NSStringPointer; frame: WebFrame): Boolean; message 'webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:';
    function webView_runJavaScriptTextInputPanelWithPrompt_defaultText(sender: WebView; prompt: NSStringPointer; defaultText: NSStringPointer): NSString; message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:';
    function webView_runJavaScriptTextInputPanelWithPrompt_defaultText_initiatedByFrame(sender: WebView; prompt: NSStringPointer; defaultText: NSStringPointer; frame: WebFrame): NSString; message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:';
    procedure webView_runOpenPanelForFileButtonWithResultListener(sender: WebView; resultListener: id); message 'webView:runOpenPanelForFileButtonWithResultListener:';
    procedure webView_setContentRect(sender: WebView; frame: NSRect); message 'webView:setContentRect:';
    procedure webView_setFrame(sender: WebView; frame: NSRect); message 'webView:setFrame:';
    procedure webView_setResizable(sender: WebView; resizable: Boolean); message 'webView:setResizable:';
    procedure webView_setStatusBarVisible(sender: WebView; visible: Boolean); message 'webView:setStatusBarVisible:';
    procedure webView_setStatusText(sender: WebView; text: NSStringPointer); message 'webView:setStatusText:';
    procedure webView_setToolbarsVisible(sender: WebView; visible: Boolean); message 'webView:setToolbarsVisible:';
    function webView_shouldPerformAction_fromSender(webView: WebView; action: SEL; sender: id): Boolean; message 'webView:shouldPerformAction:fromSender:';
    function webView_validateUserInterfaceItem_defaultValidation(webView: WebView; item: id; defaultValidation: Boolean): Boolean; message 'webView:validateUserInterfaceItem:defaultValidation:';
    procedure webView_willPerformDragDestinationAction_forDraggingInfo(webView: WebView; action: WebDragDestinationAction; draggingInfo: id); message 'webView:willPerformDragDestinationAction:forDraggingInfo:';
    procedure webView_willPerformDragSourceAction_fromPoint_withPasteboard(webView: WebView; action: WebDragSourceAction; point: NSPoint; pasteboard: NSPasteboardPointer); message 'webView:willPerformDragSourceAction:fromPoint:withPasteboard:';
    function webViewAreToolbarsVisible(sender: WebView): Boolean; message 'webViewAreToolbarsVisible:';
    procedure webViewClose(sender: WebView); message 'webViewClose:';
    function webViewContentRect(sender: WebView): NSRect; message 'webViewContentRect:';
    function webViewFirstResponder(sender: WebView): NSResponder; message 'webViewFirstResponder:';
    procedure webViewFocus(sender: WebView); message 'webViewFocus:';
    function webViewFooterHeight(sender: WebView): single; message 'webViewFooterHeight:';
    function webViewFrame(sender: WebView): NSRect; message 'webViewFrame:';
    function webViewHeaderHeight(sender: WebView): single; message 'webViewHeaderHeight:';
    function webViewIsResizable(sender: WebView): Boolean; message 'webViewIsResizable:';
    function webViewIsStatusBarVisible(sender: WebView): Boolean; message 'webViewIsStatusBarVisible:';
    procedure webViewRunModal(sender: WebView); message 'webViewRunModal:';
    procedure webViewShow(sender: WebView); message 'webViewShow:';
    function webViewStatusText(sender: WebView): NSString; message 'webViewStatusText:';
    procedure webViewUnfocus(sender: WebView); message 'webViewUnfocus:';
  end;

type
  WebViewEditingDelegate = objccategory (NSObject)
    function undoManagerForWebView(webView: WebView): NSUndoManager; message 'undoManagerForWebView:';
    function webView_doCommandBySelector(webView: WebView; selector: SEL): Boolean; message 'webView:doCommandBySelector:';
    function webView_shouldApplyStyle_toElementsInDOMRange(webView: WebView; style: DOMCSSStyleDeclaration; range: DOMRange): Boolean; message 'webView:shouldApplyStyle:toElementsInDOMRange:';
    function webView_shouldBeginEditingInDOMRange(webView: WebView; range: DOMRange): Boolean; message 'webView:shouldBeginEditingInDOMRange:';
    function webView_shouldChangeSelectedDOMRange_toDOMRange_affinity_stillSelecting(webView: WebView; currentRange: DOMRange; proposedRange: DOMRange; selectionAffinity: NSSelectionAffinity; flag: Boolean): Boolean; message 'webView:shouldChangeSelectedDOMRange:toDOMRange:affinity:stillSelecting:';
    function webView_shouldChangeTypingStyle_toStyle(webView: WebView; currentStyle: DOMCSSStyleDeclaration; proposedStyle: DOMCSSStyleDeclaration): Boolean; message 'webView:shouldChangeTypingStyle:toStyle:';
    function webView_shouldDeleteDOMRange(webView: WebView; range: DOMRange): Boolean; message 'webView:shouldDeleteDOMRange:';
    function webView_shouldEndEditingInDOMRange(webView: WebView; range: DOMRange): Boolean; message 'webView:shouldEndEditingInDOMRange:';
    function webView_shouldInsertNode_replacingDOMRange_givenAction(webView: WebView; node: DOMNode; range: DOMRange; action: WebViewInsertAction): Boolean; message 'webView:shouldInsertNode:replacingDOMRange:givenAction:';
    function webView_shouldInsertText_replacingDOMRange_givenAction(webView: WebView; text: NSStringPointer; range: DOMRange; action: WebViewInsertAction): Boolean; message 'webView:shouldInsertText:replacingDOMRange:givenAction:';
    procedure webViewDidBeginEditing(notification: NSNotificationPointer); message 'webViewDidBeginEditing:';
    procedure webViewDidChange(notification: NSNotificationPointer); message 'webViewDidChange:';
    procedure webViewDidChangeSelection(notification: NSNotificationPointer); message 'webViewDidChangeSelection:';
    procedure webViewDidChangeTypingStyle(notification: NSNotificationPointer); message 'webViewDidChangeTypingStyle:';
    procedure webViewDidEndEditing(notification: NSNotificationPointer); message 'webViewDidEndEditing:';
  end;

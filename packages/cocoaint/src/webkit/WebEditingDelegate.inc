{ Parsed from WebKit.framework WebEditingDelegate.h }

{$ifdef TYPES}
type
  WebViewInsertAction = NSInteger;
  WebViewInsertActionPtr = ^WebViewInsertAction;

const
  WebViewInsertActionTyped = 0;
  WebViewInsertActionPasted = 1;
  WebViewInsertActionDropped = 2;
{$endif}

{$ifdef CLASSES}

type
  WebViewEditingDelegate = objccategory external (NSObject)
    function webView_shouldBeginEditingInDOMRange (webView_: WebView; range: DOMRange): ObjCBOOL; message 'webView:shouldBeginEditingInDOMRange:';
    function webView_shouldEndEditingInDOMRange (webView_: WebView; range: DOMRange): ObjCBOOL; message 'webView:shouldEndEditingInDOMRange:';
    function webView_shouldInsertNode_replacingDOMRange_givenAction (webView_: WebView; node: DOMNode; range: DOMRange; action: WebViewInsertAction): ObjCBOOL; message 'webView:shouldInsertNode:replacingDOMRange:givenAction:';
    function webView_shouldInsertText_replacingDOMRange_givenAction (webView_: WebView; text: NSString; range: DOMRange; action: WebViewInsertAction): ObjCBOOL; message 'webView:shouldInsertText:replacingDOMRange:givenAction:';
    function webView_shouldDeleteDOMRange (webView_: WebView; range: DOMRange): ObjCBOOL; message 'webView:shouldDeleteDOMRange:';
    function webView_shouldChangeSelectedDOMRange_toDOMRange_affinity_stillSelecting (webView_: WebView; currentRange: DOMRange; proposedRange: DOMRange; selectionAffinity: NSSelectionAffinity; flag: ObjCBOOL): ObjCBOOL; message 'webView:shouldChangeSelectedDOMRange:toDOMRange:affinity:stillSelecting:';
    function webView_shouldApplyStyle_toElementsInDOMRange (webView_: WebView; style: DOMCSSStyleDeclaration; range: DOMRange): ObjCBOOL; message 'webView:shouldApplyStyle:toElementsInDOMRange:';
    function webView_shouldChangeTypingStyle_toStyle (webView_: WebView; currentStyle: DOMCSSStyleDeclaration; proposedStyle: DOMCSSStyleDeclaration): ObjCBOOL; message 'webView:shouldChangeTypingStyle:toStyle:';
    function webView_doCommandBySelector (webView_: WebView; selector: SEL): ObjCBOOL; message 'webView:doCommandBySelector:';
    procedure webViewDidBeginEditing (notification: NSNotification); message 'webViewDidBeginEditing:';
    procedure webViewDidChange (notification: NSNotification); message 'webViewDidChange:';
    procedure webViewDidEndEditing (notification: NSNotification); message 'webViewDidEndEditing:';
    procedure webViewDidChangeTypingStyle (notification: NSNotification); message 'webViewDidChangeTypingStyle:';
    procedure webViewDidChangeSelection (notification: NSNotification); message 'webViewDidChangeSelection:';
    function undoManagerForWebView (webView_: WebView): NSUndoManager; message 'undoManagerForWebView:';
  end;
{$endif}


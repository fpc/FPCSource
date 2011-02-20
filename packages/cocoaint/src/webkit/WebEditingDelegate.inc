{ Parsed from Webkit.framework WebEditingDelegate.h }

{$ifdef TYPES}
{$ifndef WEBEDITINGDELEGATE_PAS_T}
{$define WEBEDITINGDELEGATE_PAS_T}

{ Types }
type
  
  WebViewInsertAction = culong;

{ Constants }

const
  WebViewInsertActionTyped = 0;
  WebViewInsertActionPasted = 1;
  WebViewInsertActionDropped = 2;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBEDITINGDELEGATE_PAS_R}
{$define WEBEDITINGDELEGATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBEDITINGDELEGATE_PAS_F}
{$define WEBEDITINGDELEGATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBEDITINGDELEGATE_PAS_S}
{$define WEBEDITINGDELEGATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef WEBEDITINGDELEGATE_PAS_C}
{$define WEBEDITINGDELEGATE_PAS_C}

{ WebViewEditingDelegateCategory }
  WebViewEditingDelegateCategory = objccategory external (NSObject)
    function webView_shouldBeginEditingInDOMRange(webView_: WebView; range: DOMRange): Boolean; message 'webView:shouldBeginEditingInDOMRange:';
    function webView_shouldEndEditingInDOMRange(webView_: WebView; range: DOMRange): Boolean; message 'webView:shouldEndEditingInDOMRange:';
    function webView_shouldInsertNode_replacingDOMRange_givenAction(webView_: WebView; node: DOMNode; range: DOMRange; action: WebViewInsertAction): Boolean; message 'webView:shouldInsertNode:replacingDOMRange:givenAction:';
    function webView_shouldInsertText_replacingDOMRange_givenAction(webView_: WebView; text: NSString; range: DOMRange; action: WebViewInsertAction): Boolean; message 'webView:shouldInsertText:replacingDOMRange:givenAction:';
    function webView_shouldDeleteDOMRange(webView_: WebView; range: DOMRange): Boolean; message 'webView:shouldDeleteDOMRange:';
    function webView_shouldChangeSelectedDOMRange_toDOMRange_affinity_stillSelecting(webView_: WebView; currentRange: DOMRange; proposedRange: DOMRange; selectionAffinity: NSSelectionAffinity; flag: Boolean): Boolean; message 'webView:shouldChangeSelectedDOMRange:toDOMRange:affinity:stillSelecting:';
    function webView_shouldApplyStyle_toElementsInDOMRange(webView_: WebView; style: DOMCSSStyleDeclaration; range: DOMRange): Boolean; message 'webView:shouldApplyStyle:toElementsInDOMRange:';
    function webView_shouldChangeTypingStyle_toStyle(webView_: WebView; currentStyle: DOMCSSStyleDeclaration; proposedStyle: DOMCSSStyleDeclaration): Boolean; message 'webView:shouldChangeTypingStyle:toStyle:';
    function webView_doCommandBySelector(webView_: WebView; selector: SEL): Boolean; message 'webView:doCommandBySelector:';
    procedure webViewDidBeginEditing(notification: NSNotification); message 'webViewDidBeginEditing:';
    procedure webViewDidChange(notification: NSNotification); message 'webViewDidChange:';
    procedure webViewDidEndEditing(notification: NSNotification); message 'webViewDidEndEditing:';
    procedure webViewDidChangeTypingStyle(notification: NSNotification); message 'webViewDidChangeTypingStyle:';
    procedure webViewDidChangeSelection(notification: NSNotification); message 'webViewDidChangeSelection:';
    function undoManagerForWebView(webView_: WebView): NSUndoManager; message 'undoManagerForWebView:';
  end;

{$endif}
{$endif}

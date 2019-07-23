{ Parsed from Webkit.framework WebFrameView.h }

{$ifdef TYPES}
{$ifndef WEBFRAMEVIEW_PAS_T}
{$define WEBFRAMEVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBFRAMEVIEW_PAS_R}
{$define WEBFRAMEVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBFRAMEVIEW_PAS_F}
{$define WEBFRAMEVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBFRAMEVIEW_PAS_S}
{$define WEBFRAMEVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebFrameView = objcclass;
  WebFrameViewPointer = ^WebFrameView;
  WebFrameViewPtr = WebFrameViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBFRAMEVIEW_PAS_C}
{$define WEBFRAMEVIEW_PAS_C}

{ WebFrameView }
  WebFrameView = objcclass external (NSView)
  private
    _private: WebFrameViewPrivate;
    
  public
    function webFrame: WebFrame; message 'webFrame';
    function documentView: NSViewPtr; message 'documentView';
    procedure setAllowsScrolling(flag: ObjCBOOL); message 'setAllowsScrolling:';
    function allowsScrolling: ObjCBOOL; message 'allowsScrolling';
    function canPrintHeadersAndFooters: ObjCBOOL; message 'canPrintHeadersAndFooters';
    function printOperationWithPrintInfo(printInfo: NSPrintInfoPtr): NSPrintOperationPtr; message 'printOperationWithPrintInfo:';
    function documentViewShouldHandlePrint: ObjCBOOL; message 'documentViewShouldHandlePrint';
    procedure printDocumentView; message 'printDocumentView';
  end;

{$endif}
{$endif}

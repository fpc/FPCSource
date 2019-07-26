{ Parsed from PDFKit.framework PDFView.h }


{ Types from PDFView }
{$ifdef TYPES}










{$endif}


{ Types from PDFViewDelegate }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  PDFViewPtr = ^PDFView;
{$endif}

{$ifdef TYPES}
type
  PDFDisplayMode = NSInteger;
  PDFDisplayModePtr = ^PDFDisplayMode;

const
  kPDFDisplaySinglePage = 0;
  kPDFDisplaySinglePageContinuous = 1;
  kPDFDisplayTwoUp = 2;
  kPDFDisplayTwoUpContinuous = 3;

type
  PDFAreaOfInterest = NSInteger;
  PDFAreaOfInterestPtr = ^PDFAreaOfInterest;

const
  kPDFNoArea = 0;
  kPDFPageArea = 1;
  kPDFTextArea = 2;
  kPDFAnnotationArea = 4;
  kPDFLinkArea = 8;
  kPDFControlArea = 16;
  kPDFTextFieldArea = 32;
  kPDFIconArea = 64;
  kPDFPopupArea = 128;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7)}
{$ifdef TYPES}
type
  PDFInterpolationQuality = NSInteger;
  PDFInterpolationQualityPtr = ^PDFInterpolationQuality;

const
  kPDFInterpolationQualityNone = 0;
  kPDFInterpolationQualityLow = 1;
  kPDFInterpolationQualityHigh = 2;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  PDFViewDocumentChangedNotification: NSString; cvar; external;
  PDFViewChangedHistoryNotification: NSString; cvar; external;
  PDFViewPageChangedNotification: NSString; cvar; external;
  PDFViewScaleChangedNotification: NSString; cvar; external;
  PDFViewAnnotationHitNotification: NSString; cvar; external;
  PDFViewCopyPermissionNotification: NSString; cvar; external;
  PDFViewPrintPermissionNotification: NSString; cvar; external;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef EXTERNAL_SYMBOLS}
var
  PDFViewAnnotationWillHitNotification: NSString; cvar; external;
  PDFViewSelectionChangedNotification: NSString; cvar; external;
  PDFViewDisplayModeChangedNotification: NSString; cvar; external;
  PDFViewDisplayBoxChangedNotification: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  PDFViewVisiblePagesChangedNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  PDFView = objcclass external (NSView, NSAnimationDelegateProtocol)
  private
    _pdfPriv: PDFViewPrivateVars;
  public
    function document: PDFDocument; message 'document';
    procedure setDocument (document_: PDFDocument); message 'setDocument:';
    function canGoToFirstPage: ObjCBOOL; message 'canGoToFirstPage';
    procedure goToFirstPage (sender: id); message 'goToFirstPage:';
    function canGoToLastPage: ObjCBOOL; message 'canGoToLastPage';
    procedure goToLastPage (sender: id); message 'goToLastPage:';
    function canGoToNextPage: ObjCBOOL; message 'canGoToNextPage';
    procedure goToNextPage (sender: id); message 'goToNextPage:';
    function canGoToPreviousPage: ObjCBOOL; message 'canGoToPreviousPage';
    procedure goToPreviousPage (sender: id); message 'goToPreviousPage:';
    function canGoBack: ObjCBOOL; message 'canGoBack';
    procedure goBack (sender: id); message 'goBack:';
    function canGoForward: ObjCBOOL; message 'canGoForward';
    procedure goForward (sender: id); message 'goForward:';
    function currentPage: PDFPage; message 'currentPage';
    procedure goToPage (page: PDFPage); message 'goToPage:';
    function currentDestination: PDFDestination; message 'currentDestination';
    procedure goToDestination (destination: PDFDestination); message 'goToDestination:';
    procedure goToSelection (selection: PDFSelection); message 'goToSelection:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure goToRect_onPage (rect: NSRect; page: PDFPage); message 'goToRect:onPage:';
    {$endif}
    procedure setDisplayMode (mode: PDFDisplayMode); message 'setDisplayMode:';
    function displayMode: PDFDisplayMode; message 'displayMode';
    procedure setDisplaysPageBreaks (breaks: ObjCBOOL); message 'setDisplaysPageBreaks:';
    function displaysPageBreaks: ObjCBOOL; message 'displaysPageBreaks';
    procedure setDisplayBox (box: PDFDisplayBox); message 'setDisplayBox:';
    function displayBox: PDFDisplayBox; message 'displayBox';
    procedure setDisplaysAsBook (asBook: ObjCBOOL); message 'setDisplaysAsBook:';
    function displaysAsBook: ObjCBOOL; message 'displaysAsBook';
    procedure setShouldAntiAlias (aliasing: ObjCBOOL); message 'setShouldAntiAlias:';
    function shouldAntiAlias: ObjCBOOL; message 'shouldAntiAlias';
    procedure setGreekingThreshold (threshold: CGFloat); message 'setGreekingThreshold:';
    function greekingThreshold: CGFloat; message 'greekingThreshold';
    procedure takeBackgroundColorFrom (sender: id); message 'takeBackgroundColorFrom:';
    procedure setBackgroundColor (newColor: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7)}
    procedure setInterpolationQuality (quality: PDFInterpolationQuality); message 'setInterpolationQuality:';
    function interpolationQuality: PDFInterpolationQuality; message 'interpolationQuality';
    {$endif}
    procedure setDelegate (anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setScaleFactor (scale: CGFloat); message 'setScaleFactor:';
    function scaleFactor: CGFloat; message 'scaleFactor';
    procedure zoomIn (sender: id); message 'zoomIn:';
    function canZoomIn: ObjCBOOL; message 'canZoomIn';
    procedure zoomOut (sender: id); message 'zoomOut:';
    function canZoomOut: ObjCBOOL; message 'canZoomOut';
    procedure setAutoScales (newAuto: ObjCBOOL); message 'setAutoScales:';
    function autoScales: ObjCBOOL; message 'autoScales';
    function areaOfInterestForMouse (event: NSEvent): PDFAreaOfInterest; message 'areaOfInterestForMouse:';
    procedure setCursorForAreaOfInterest (area: PDFAreaOfInterest); message 'setCursorForAreaOfInterest:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure performAction (action: PDFAction); message 'performAction:';
    {$endif}
    function currentSelection: PDFSelection; message 'currentSelection';
    procedure setCurrentSelection (selection: PDFSelection); message 'setCurrentSelection:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure setCurrentSelection_animate (selection: PDFSelection; animate: ObjCBOOL); message 'setCurrentSelection:animate:';
    {$endif}
    procedure clearSelection; message 'clearSelection';
    procedure selectAll (sender: id); message 'selectAll:';
    procedure scrollSelectionToVisible (sender: id); message 'scrollSelectionToVisible:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure setHighlightedSelections (selections: NSArray); message 'setHighlightedSelections:';
    function highlightedSelections: NSArray; message 'highlightedSelections';
    {$endif}
    procedure takePasswordFrom (sender: id); message 'takePasswordFrom:';
    procedure drawPage (page: PDFPage); message 'drawPage:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure drawPagePost (page: PDFPage); message 'drawPagePost:';
    {$endif}
    procedure copy_ (sender: id); message 'copy:';
    procedure printWithInfo_autoRotate (printInfo: NSPrintInfo; doRotate: ObjCBOOL); message 'printWithInfo:autoRotate:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure printWithInfo_autoRotate_pageScaling (printInfo: NSPrintInfo; doRotate: ObjCBOOL; scale: PDFPrintScalingMode); message 'printWithInfo:autoRotate:pageScaling:';
    {$endif}
    function pageForPoint_nearest (point: NSPoint; nearest: ObjCBOOL): PDFPage; message 'pageForPoint:nearest:';
    function convertPoint_toPage (point: NSPoint; page: PDFPage): NSPoint; message 'convertPoint:toPage:';
    function convertRect_toPage (rect: NSRect; page: PDFPage): NSRect; message 'convertRect:toPage:';
    function convertPoint_fromPage (point: NSPoint; page: PDFPage): NSPoint; message 'convertPoint:fromPage:';
    function convertRect_fromPage (rect: NSRect; page: PDFPage): NSRect; message 'convertRect:fromPage:';
    function documentView: NSView; message 'documentView';
    procedure layoutDocumentView; message 'layoutDocumentView';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure annotationsChangedOnPage (page: PDFPage); message 'annotationsChangedOnPage:';
    {$endif}
    function rowSizeForPage (page: PDFPage): NSSize; message 'rowSizeForPage:';
    procedure setAllowsDragging (allow: ObjCBOOL); message 'setAllowsDragging:';
    function allowsDragging: ObjCBOOL; message 'allowsDragging';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function visiblePages: NSArray; message 'visiblePages';
    {$endif}
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    procedure setEnableDataDetectors (enable: ObjCBOOL); message 'setEnableDataDetectors:';
    function enableDataDetectors: ObjCBOOL; message 'enableDataDetectors';
    {$endif}

    { Adopted protocols }
    procedure animation_didReachProgressMark (animation: NSAnimation; progress: NSAnimationProgress); message 'animation:didReachProgressMark:';
    function animation_valueForProgress (animation: NSAnimation; progress: NSAnimationProgress): single; message 'animation:valueForProgress:';
    procedure animationDidEnd (animation: NSAnimation); message 'animationDidEnd:';
    procedure animationDidStop (animation: NSAnimation); message 'animationDidStop:';
    function animationShouldStart (animation: NSAnimation): ObjCBOOL; message 'animationShouldStart:';
  end;


type
  PDFViewDelegate = objccategory external (NSObject)
    function PDFViewWillChangeScaleFactor_toScale (sender: PDFView; scale: CGFloat): CGFloat; message 'PDFViewWillChangeScaleFactor:toScale:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure PDFViewWillClickOnLink_withURL (sender: PDFView; url: NSURL); message 'PDFViewWillClickOnLink:withURL:';
    function PDFViewPrintJobTitle (sender: PDFView): NSString; message 'PDFViewPrintJobTitle:';
    procedure PDFViewPerformFind (sender: PDFView); message 'PDFViewPerformFind:';
    procedure PDFViewPerformGoToPage (sender: PDFView); message 'PDFViewPerformGoToPage:';
    procedure PDFViewPerformPrint (sender: PDFView); message 'PDFViewPerformPrint:';
    procedure PDFViewOpenPDF_forRemoteGoToAction (sender: PDFView; action: PDFActionRemoteGoTo); message 'PDFViewOpenPDF:forRemoteGoToAction:';
    {$endif}
  end;
{$endif}


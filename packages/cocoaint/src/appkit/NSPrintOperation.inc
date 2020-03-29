{ Parsed from AppKit.framework NSPrintOperation.h }


{$ifdef TYPES}
type
  NSPrintOperationPtr = ^NSPrintOperation;
{$endif}

{$ifdef TYPES}
type
  NSPrintingPageOrder = NSInteger;
  NSPrintingPageOrderPtr = ^NSPrintingPageOrder;

const
  NSDescendingPageOrder = -1;
  NSSpecialPageOrder = 0;
  NSAscendingPageOrder = 1;
  NSUnknownPageOrder = 2;

type
  NSPrintRenderingQuality = NSInteger;
  NSPrintRenderingQualityPtr = ^NSPrintRenderingQuality;

const
  NSPrintRenderingQualityBest = 0;
  NSPrintRenderingQualityResponsive = 1;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPrintOperationExistsException: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSPrintOperation = objcclass external (NSObject)
  private
  public
    class function printOperationWithView_printInfo (view: NSView; printInfo: NSPrintInfo): NSPrintOperation; message 'printOperationWithView:printInfo:';
    class function PDFOperationWithView_insideRect_toData_printInfo (view: NSView; rect: NSRect; data: NSMutableData; printInfo: NSPrintInfo): NSPrintOperation; message 'PDFOperationWithView:insideRect:toData:printInfo:';
    class function PDFOperationWithView_insideRect_toPath_printInfo (view: NSView; rect: NSRect; path: NSString; printInfo: NSPrintInfo): NSPrintOperation; message 'PDFOperationWithView:insideRect:toPath:printInfo:';
    class function EPSOperationWithView_insideRect_toData_printInfo (view: NSView; rect: NSRect; data: NSMutableData; printInfo: NSPrintInfo): NSPrintOperation; message 'EPSOperationWithView:insideRect:toData:printInfo:';
    class function EPSOperationWithView_insideRect_toPath_printInfo (view: NSView; rect: NSRect; path: NSString; printInfo: NSPrintInfo): NSPrintOperation; message 'EPSOperationWithView:insideRect:toPath:printInfo:';
    class function printOperationWithView (view: NSView): NSPrintOperation; message 'printOperationWithView:';
    class function PDFOperationWithView_insideRect_toData (view: NSView; rect: NSRect; data: NSMutableData): NSPrintOperation; message 'PDFOperationWithView:insideRect:toData:';
    class function EPSOperationWithView_insideRect_toData (view: NSView; rect: NSRect; data: NSMutableData): NSPrintOperation; message 'EPSOperationWithView:insideRect:toData:';
    class function currentOperation: NSPrintOperation; message 'currentOperation';
    class procedure setCurrentOperation (operation: NSPrintOperation); message 'setCurrentOperation:';
    function isCopyingOperation: ObjCBOOL; message 'isCopyingOperation';
    function preferredRenderingQuality: NSPrintRenderingQuality; message 'preferredRenderingQuality';
    procedure setJobTitle(newValue: NSString); message 'setJobTitle:';
    function jobTitle: NSString; message 'jobTitle';
    procedure setShowsPrintPanel(newValue: ObjCBOOL); message 'setShowsPrintPanel:';
    function showsPrintPanel: ObjCBOOL; message 'showsPrintPanel';
    procedure setShowsProgressPanel(newValue: ObjCBOOL); message 'setShowsProgressPanel:';
    function showsProgressPanel: ObjCBOOL; message 'showsProgressPanel';
    procedure setPrintPanel(newValue: NSPrintPanel); message 'setPrintPanel:';
    function printPanel: NSPrintPanel; message 'printPanel';
    procedure setPDFPanel(newValue: NSPDFPanel); message 'setPDFPanel:';
    function PDFPanel: NSPDFPanel; message 'PDFPanel';
    procedure setCanSpawnSeparateThread(newValue: ObjCBOOL); message 'setCanSpawnSeparateThread:';
    function canSpawnSeparateThread: ObjCBOOL; message 'canSpawnSeparateThread';
    procedure setPageOrder(newValue: NSPrintingPageOrder); message 'setPageOrder:';
    function pageOrder: NSPrintingPageOrder; message 'pageOrder';
    procedure runOperationModalForWindow_delegate_didRunSelector_contextInfo (docWindow: NSWindow; delegate: id; didRunSelector: SEL; contextInfo: pointer); message 'runOperationModalForWindow:delegate:didRunSelector:contextInfo:';
    function runOperation: ObjCBOOL; message 'runOperation';
    function view: NSView; message 'view';
    procedure setPrintInfo(newValue: NSPrintInfo); message 'setPrintInfo:';
    function printInfo: NSPrintInfo; message 'printInfo';
    function context: NSGraphicsContext; message 'context';
    function pageRange: NSRange; message 'pageRange';
    function currentPage: NSInteger; message 'currentPage';
    function createContext: NSGraphicsContext; message 'createContext';
    procedure destroyContext; message 'destroyContext';
    function deliverResult: ObjCBOOL; message 'deliverResult';
    procedure cleanUpOperation; message 'cleanUpOperation';
  end;


type
  NSPrintOperation_NSDeprecated = objccategory external name 'NSDeprecated' (NSPrintOperation)
    procedure setAccessoryView (view_: NSView); message 'setAccessoryView:'; deprecated 'in 10_0, 10_5';
    function accessoryView: NSView; message 'accessoryView'; deprecated 'in 10_0, 10_5';
    procedure setJobStyleHint (hint: NSString); message 'setJobStyleHint:'; deprecated 'in 10_2, 10_5';
    function jobStyleHint: NSString; message 'jobStyleHint'; deprecated 'in 10_2, 10_5';
    procedure setShowPanels (flag: ObjCBOOL); message 'setShowPanels:'; deprecated 'in 10_0, 10_4';
    function showPanels: ObjCBOOL; message 'showPanels'; deprecated 'in 10_0, 10_4';
  end;
{$endif}


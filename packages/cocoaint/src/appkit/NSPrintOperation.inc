{ Parsed from Appkit.framework NSPrintOperation.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPRINTOPERATION_PAS_T}
{$define NSPRINTOPERATION_PAS_T}

{ Constants }

const
  NSDescendingPageOrder = -1;
  NSSpecialPageOrder = 0;
  NSAscendingPageOrder = 1;
  NSUnknownPageOrder = 2;

{ Types }
type
  NSPrintingPageOrder = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPRINTOPERATION_PAS_R}
{$define NSPRINTOPERATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPRINTOPERATION_PAS_F}
{$define NSPRINTOPERATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPRINTOPERATION_PAS_S}
{$define NSPRINTOPERATION_PAS_S}

{ External string constants }
var
  NSPrintOperationExistsException: NSString; external name '_NSPrintOperationExistsException';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPrintOperation = objcclass;
  NSPrintOperationPointer = ^NSPrintOperation;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPRINTOPERATION_PAS_C}
{$define NSPRINTOPERATION_PAS_C}

{ NSPrintOperation }
  NSPrintOperation = objcclass(NSObject)
    
  public
    class function alloc: NSPrintOperation; message 'alloc';

    class function printOperationWithView_printInfo(view_: NSView; printInfo_: NSPrintInfo): NSPrintOperation; message 'printOperationWithView:printInfo:';
    class function PDFOperationWithView_insideRect_toData_printInfo(view_: NSView; rect: NSRect; data: NSMutableData; printInfo_: NSPrintInfo): NSPrintOperation; message 'PDFOperationWithView:insideRect:toData:printInfo:';
    class function PDFOperationWithView_insideRect_toPath_printInfo(view_: NSView; rect: NSRect; path: NSString; printInfo_: NSPrintInfo): NSPrintOperation; message 'PDFOperationWithView:insideRect:toPath:printInfo:';
    class function EPSOperationWithView_insideRect_toData_printInfo(view_: NSView; rect: NSRect; data: NSMutableData; printInfo_: NSPrintInfo): NSPrintOperation; message 'EPSOperationWithView:insideRect:toData:printInfo:';
    class function EPSOperationWithView_insideRect_toPath_printInfo(view_: NSView; rect: NSRect; path: NSString; printInfo_: NSPrintInfo): NSPrintOperation; message 'EPSOperationWithView:insideRect:toPath:printInfo:';
    class function printOperationWithView(view_: NSView): NSPrintOperation; message 'printOperationWithView:';
    class function PDFOperationWithView_insideRect_toData(view_: NSView; rect: NSRect; data: NSMutableData): NSPrintOperation; message 'PDFOperationWithView:insideRect:toData:';
    class function EPSOperationWithView_insideRect_toData(view_: NSView; rect: NSRect; data: NSMutableData): NSPrintOperation; message 'EPSOperationWithView:insideRect:toData:';
    class function currentOperation: NSPrintOperation; message 'currentOperation';
    class procedure setCurrentOperation(operation: NSPrintOperation); message 'setCurrentOperation:';
    function isCopyingOperation: Boolean; message 'isCopyingOperation';
    procedure setJobTitle(jobTitle_: NSString); message 'setJobTitle:';
    function jobTitle: NSString; message 'jobTitle';
    procedure setShowsPrintPanel(flag: Boolean); message 'setShowsPrintPanel:';
    function showsPrintPanel: Boolean; message 'showsPrintPanel';
    procedure setShowsProgressPanel(flag: Boolean); message 'setShowsProgressPanel:';
    function showsProgressPanel: Boolean; message 'showsProgressPanel';
    procedure setPrintPanel(panel: NSPrintPanel); message 'setPrintPanel:';
    function printPanel: NSPrintPanel; message 'printPanel';
    procedure setCanSpawnSeparateThread(canSpawnSeparateThread_: Boolean); message 'setCanSpawnSeparateThread:';
    function canSpawnSeparateThread: Boolean; message 'canSpawnSeparateThread';
    procedure setPageOrder(pageOrder_: NSPrintingPageOrder); message 'setPageOrder:';
    function pageOrder: NSPrintingPageOrder; message 'pageOrder';
    procedure runOperationModalForWindow_delegate_didRunSelector_contextInfo(docWindow: NSWindow; delegate: id; didRunSelector: SEL; contextInfo: Pointer); message 'runOperationModalForWindow:delegate:didRunSelector:contextInfo:';
    function runOperation: Boolean; message 'runOperation';
    function view: NSView; message 'view';
    function printInfo: NSPrintInfo; message 'printInfo';
    procedure setPrintInfo(printInfo_: NSPrintInfo); message 'setPrintInfo:';
    function context: NSGraphicsContext; message 'context';
    function pageRange: NSRange; message 'pageRange';
    function currentPage: NSInteger; message 'currentPage';
    function createContext: NSGraphicsContext; message 'createContext';
    procedure destroyContext; message 'destroyContext';
    function deliverResult: Boolean; message 'deliverResult';
    procedure cleanUpOperation; message 'cleanUpOperation';

    { Category: NSDeprecated }
    procedure setAccessoryView(view_: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure setJobStyleHint(hint: NSString); message 'setJobStyleHint:';
    function jobStyleHint: NSString; message 'jobStyleHint';
    procedure setShowPanels(flag: Boolean); message 'setShowPanels:';
    function showPanels: Boolean; message 'showPanels';
  end; external;

{$endif}
{$endif}

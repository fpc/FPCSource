{ Parsed from Appkit.framework NSPageLayout.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPAGELAYOUT_PAS_T}
{$define NSPAGELAYOUT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPAGELAYOUT_PAS_R}
{$define NSPAGELAYOUT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPAGELAYOUT_PAS_F}
{$define NSPAGELAYOUT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPAGELAYOUT_PAS_S}
{$define NSPAGELAYOUT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPageLayout = objcclass;
  NSPageLayoutPointer = ^NSPageLayout;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPAGELAYOUT_PAS_C}
{$define NSPAGELAYOUT_PAS_C}

{ NSPageLayout }
  NSPageLayout = objcclass(NSObject)
  private
    __accessoryControllers: NSMutableArray;
    __originalPrintInfo: NSPrintInfo;
    __delegate: id;
    __didEndSelector: SEL;
    __contextInfo: Pointer;
    __presentedPrintInfo: NSPrintInfo;
    __windowController: NSWindowController;
    {$ifdef cpu64}
        _reserved: array[0..3] of id;
    {$else}
        _compatibilityPadding: array[0..155] of char;
    {$endif}
    
  public
    class function alloc: NSPageLayout; message 'alloc';

    class function pageLayout: NSPageLayout; message 'pageLayout';
    procedure addAccessoryController(accessoryController: NSViewController); message 'addAccessoryController:';
    procedure removeAccessoryController(accessoryController: NSViewController); message 'removeAccessoryController:';
    function accessoryControllers: NSArray; message 'accessoryControllers';
    procedure beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo(printInfo_: NSPrintInfo; docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:';
    function runModalWithPrintInfo(printInfo_: NSPrintInfo): NSInteger; message 'runModalWithPrintInfo:';
    function runModal: NSInteger; message 'runModal';
    function printInfo: NSPrintInfo; message 'printInfo';

    { Category: NSDeprecated }
    procedure setAccessoryView(accessoryView_: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure readPrintInfo; message 'readPrintInfo';
    procedure writePrintInfo; message 'writePrintInfo';
  end; external;

{$endif}
{$endif}

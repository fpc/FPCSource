{ Parsed from AppKit.framework NSPageLayout.h }


{$ifdef TYPES}
type
  NSPageLayoutPtr = ^NSPageLayout;
{$endif}

{$ifdef CLASSES}

type
  NSPageLayout = objcclass external (NSObject)
  private
    _accessoryControllers: NSMutableArray;
    _originalPrintInfo: NSPrintInfo;
    _delegate: id;
    _didEndSelector: SEL;
    _contextInfo: pointer;
    _presentedPrintInfo: NSPrintInfo;
    _windowController: NSWindowController;
    {$if defined(__LP64__)}
    _reserved:array[0..3] of id;
    {$else}
    _compatibilityPadding:array[0..155] of char;
    {$endif}
  public
    class function pageLayout: NSPageLayout; message 'pageLayout';
    procedure addAccessoryController (accessoryController: NSViewController); message 'addAccessoryController:'; { available in 10_5 }
    procedure removeAccessoryController (accessoryController: NSViewController); message 'removeAccessoryController:'; { available in 10_5 }
    function accessoryControllers: NSArray; message 'accessoryControllers';
    procedure beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo (printInfo: NSPrintInfo; docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: pointer); message 'beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:';
    function runModalWithPrintInfo (printInfo: NSPrintInfo): NSInteger; message 'runModalWithPrintInfo:';
    function runModal: NSInteger; message 'runModal';
    function printInfo: NSPrintInfo; message 'printInfo';
  end;


type
  NSPageLayout_NSDeprecated = objccategory external name 'NSDeprecated' (NSPageLayout)
    procedure setAccessoryView (accessoryView: NSView); message 'setAccessoryView:'; deprecated 'in 10_0, 10_5';
    function accessoryView: NSView; message 'accessoryView'; deprecated 'in 10_0, 10_5';
    procedure readPrintInfo; message 'readPrintInfo'; deprecated 'in 10_0, 10_5';
    procedure writePrintInfo; message 'writePrintInfo'; deprecated 'in 10_0, 10_5';
  end;

type
  NSPageLayoutPanel = objccategory external (NSApplication)
    procedure runPageLayout (sender: id); message 'runPageLayout:';
  end;
{$endif}


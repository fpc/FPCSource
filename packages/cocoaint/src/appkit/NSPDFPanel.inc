{ Parsed from AppKit.framework NSPDFPanel.h }


{$ifdef TYPES}
type
  NSPDFPanelPtr = ^NSPDFPanel;
{$endif}

{$ifdef TYPES}
type
  NSPDFPanelOptions = NSInteger;
  NSPDFPanelOptionsPtr = ^NSPDFPanelOptions;

const
  NSPDFPanelShowsPaperSize = 1 shl 2;
  NSPDFPanelShowsOrientation = 1 shl 3;
  NSPDFPanelRequestsParentDirectory = 1 shl 24;
{$endif}

{$ifdef CLASSES}

type
  NSPDFPanel = objcclass external (NSObject)
  private
    _accessoryController: NSViewController;
    _options: NSPDFPanelOptions;
    _defaultFileName: NSString;
    _panel: id;
    _private: id;
  public
    class function panel: NSPDFPanel; message 'panel';
    procedure setAccessoryController(newValue: NSViewController); message 'setAccessoryController:';
    function accessoryController: NSViewController; message 'accessoryController';
    procedure setOptions(newValue: NSPDFPanelOptions); message 'setOptions:';
    function options: NSPDFPanelOptions; message 'options';
    procedure setDefaultFileName(newValue: NSString); message 'setDefaultFileName:';
    function defaultFileName: NSString; message 'defaultFileName';
    procedure beginSheetWithPDFInfo_modalForWindow_completionHandler (pdfInfo: NSPDFInfo; docWindow: NSWindow; completionHandler: OpaqueCBlock); message 'beginSheetWithPDFInfo:modalForWindow:completionHandler:';
  end;
{$endif}


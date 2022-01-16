{ Parsed from AppKit.framework NSPrintPanel.h }


{$ifdef TYPES}
type
  NSPrintPanelPtr = ^NSPrintPanel;
  NSPrintPanelAccessorizingProtocolPtr = ^NSPrintPanelAccessorizingProtocol;
{$endif}

{$ifdef TYPES}
type
  NSPrintPanelOptions = NSUInteger;
  NSPrintPanelOptionsPtr = ^NSPrintPanelOptions;

const
  NSPrintPanelShowsCopies = 1 shl 0;
  NSPrintPanelShowsPageRange = 1 shl 1;
  NSPrintPanelShowsPaperSize = 1 shl 2;
  NSPrintPanelShowsOrientation = 1 shl 3;
  NSPrintPanelShowsScaling = 1 shl 4;
  NSPrintPanelShowsPrintSelection = 1 shl 5 { available in 10_6 };
  NSPrintPanelShowsPageSetupAccessory = 1 shl 8;
  NSPrintPanelShowsPreview = 1 shl 17;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPrintPhotoJobStyleHint: NSString; cvar; external;
  NSPrintAllPresetsJobStyleHint: NSString { available in 10_6 }; cvar; external;
  NSPrintNoPresetsJobStyleHint: NSString { available in 10_6 }; cvar; external;
  NSPrintPanelAccessorySummaryItemNameKey: NSString { available in 10_5 }; cvar; external;
  NSPrintPanelAccessorySummaryItemDescriptionKey: NSString { available in 10_5 }; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPrintPanelAccessorizingProtocol = objcprotocol external name 'NSPrintPanelAccessorizing'
    function localizedSummaryItems: NSArray; message 'localizedSummaryItems';
  optional
    function keyPathsForValuesAffectingPreview: NSSet; message 'keyPathsForValuesAffectingPreview';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSPrintPanel = objcclass external (NSObject)
  private
    _accessoryControllers: NSMutableArray;
    _previewController: id;
    _thumbnailView: NSView;
    _options: NSInteger;
    _defaultButtonTitle: NSString;
    _helpAnchor: NSString;
    _jobStyleHint: NSString;
    _originalPrintInfo: NSPrintInfo;
    _delegate: id;
    _didEndSelector: SEL;
    _contextInfo: pointer;
    _presentedPrintInfo: NSPrintInfo;
    _windowController: NSWindowController;
    {$if defined(__LP64__)}
    _reserved:array[0..1] of id;
    {$else}
    _compatibilityPadding:array[0..191] of char;
    {$endif}
  public
    class function printPanel: NSPrintPanel; message 'printPanel';
    procedure addAccessoryController (accessoryController: NSPrintPanelAccessorizingProtocol); message 'addAccessoryController:'; { available in 10_5 }
    procedure removeAccessoryController (accessoryController: NSPrintPanelAccessorizingProtocol); message 'removeAccessoryController:'; { available in 10_5 }
    function accessoryControllers: NSArray; message 'accessoryControllers';
    procedure setOptions(newValue: NSPrintPanelOptions); message 'setOptions:';
    function options: NSPrintPanelOptions; message 'options';
    procedure setDefaultButtonTitle (defaultButtonTitle: NSString); message 'setDefaultButtonTitle:'; { available in 10_5 }
    function defaultButtonTitle: NSString; message 'defaultButtonTitle'; { available in 10_5 }
    procedure setHelpAnchor(newValue: NSString); message 'setHelpAnchor:';
    function helpAnchor: NSString; message 'helpAnchor';
    procedure setJobStyleHint(newValue: NSString); message 'setJobStyleHint:';
    function jobStyleHint: NSString; message 'jobStyleHint';
    procedure beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo (printInfo: NSPrintInfo; docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: pointer); message 'beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:';
    function runModalWithPrintInfo (printInfo: NSPrintInfo): NSInteger; message 'runModalWithPrintInfo:'; { available in 10_5 }
    function runModal: NSInteger; message 'runModal';
    function printInfo: NSPrintInfo; message 'printInfo';
  end;


type
  NSPrintPanel_NSDeprecated = objccategory external name 'NSDeprecated' (NSPrintPanel)
    procedure setAccessoryView (accessoryView: NSView); message 'setAccessoryView:'; deprecated 'in 10_0, 10_5';
    function accessoryView: NSView; message 'accessoryView'; deprecated 'in 10_0, 10_5';
    procedure updateFromPrintInfo; message 'updateFromPrintInfo'; deprecated 'in 10_0, 10_5';
    procedure finalWritePrintInfo; message 'finalWritePrintInfo'; deprecated 'in 10_0, 10_5';
  end;
{$endif}


{ Parsed from Appkit.framework NSPrintPanel.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPRINTPANEL_PAS_T}
{$define NSPRINTPANEL_PAS_T}

{ Constants }

const
  NSPrintPanelShowsCopies = $01;
  NSPrintPanelShowsPageRange = $02;
  NSPrintPanelShowsPaperSize = $04;
  NSPrintPanelShowsOrientation = $08;
  NSPrintPanelShowsScaling = $10;
  NSPrintPanelShowsPageSetupAccessory = $100;
  NSPrintPanelShowsPreview = $20000;

{ Types }
type
  NSPrintPanelOptions = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPRINTPANEL_PAS_R}
{$define NSPRINTPANEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPRINTPANEL_PAS_F}
{$define NSPRINTPANEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPRINTPANEL_PAS_S}
{$define NSPRINTPANEL_PAS_S}

{ External string constants }
var
  NSPrintPhotoJobStyleHint: NSString; external name '_NSPrintPhotoJobStyleHint';
  NSPrintPanelAccessorySummaryItemNameKey: NSString; external name '_NSPrintPanelAccessorySummaryItemNameKey';
  NSPrintPanelAccessorySummaryItemDescriptionKey: NSString; external name '_NSPrintPanelAccessorySummaryItemDescriptionKey';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPrintPanelAccessorizingProtocol = objcprotocol;
  NSPrintPanel = objcclass;
  NSPrintPanelPointer = ^NSPrintPanel;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPRINTPANEL_PAS_C}
{$define NSPRINTPANEL_PAS_C}

{ NSPrintPanel }
  NSPrintPanel = objcclass(NSObject)
  private
    __accessoryControllers: NSMutableArray;
    __previewController: id;
    __thumbnailView: NSView;
    __options: NSInteger;
    __defaultButtonTitle: NSString;
    __helpAnchor: NSString;
    __jobStyleHint: NSString;
    __originalPrintInfo: NSPrintInfo;
    __delegate: id;
    __didEndSelector: SEL;
    __contextInfo: Pointer;
    __presentedPrintInfo: NSPrintInfo;
    __windowController: NSWindowController;
    {$ifdef cpu64}
        _reserved: array[0..1] of id;
    {$else}
        _compatibilityPadding: array[0..191] of char;
    {$endif}
    
  public
    class function alloc: NSPrintPanel; message 'alloc';

    class function printPanel: NSPrintPanel; message 'printPanel';
    procedure addAccessoryController(accessoryController: NSViewController); message 'addAccessoryController:';
    procedure removeAccessoryController(accessoryController: NSViewController); message 'removeAccessoryController:';
    function accessoryControllers: NSArray; message 'accessoryControllers';
    procedure setOptions(options_: NSPrintPanelOptions); message 'setOptions:';
    function options: NSPrintPanelOptions; message 'options';
    procedure setDefaultButtonTitle(defaultButtonTitle_: NSString); message 'setDefaultButtonTitle:';
    function defaultButtonTitle: NSString; message 'defaultButtonTitle';
    procedure setHelpAnchor(helpAnchor_: NSString); message 'setHelpAnchor:';
    function helpAnchor: NSString; message 'helpAnchor';
    procedure setJobStyleHint(hint: NSString); message 'setJobStyleHint:';
    function jobStyleHint: NSString; message 'jobStyleHint';
    procedure beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo(printInfo_: NSPrintInfo; docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:';
    function runModalWithPrintInfo(printInfo_: NSPrintInfo): NSInteger; message 'runModalWithPrintInfo:';
    function runModal: NSInteger; message 'runModal';
    function printInfo: NSPrintInfo; message 'printInfo';

    { Category: NSDeprecated }
    procedure setAccessoryView(accessoryView_: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure updateFromPrintInfo; message 'updateFromPrintInfo';
    procedure finalWritePrintInfo; message 'finalWritePrintInfo';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSPRINTPANEL_PAS_P}
{$define NSPRINTPANEL_PAS_P}
  
{ NSPrintPanelAccessorizing Protocol }
  NSPrintPanelAccessorizingProtocol = objcprotocol
    function localizedSummaryItems: NSArray; message 'localizedSummaryItems';
    function keyPathsForValuesAffectingPreview: NSSet; message 'keyPathsForValuesAffectingPreview';
  end; external name 'NSPrintPanelAccessorizing';
{$endif}
{$endif}

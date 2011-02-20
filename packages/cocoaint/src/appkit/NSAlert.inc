{ Parsed from Appkit.framework NSAlert.h }

{$ifdef TYPES}
{$ifndef NSALERT_PAS_T}
{$define NSALERT_PAS_T}

{ Constants }

const
  NSWarningAlertStyle = 0;
  NSInformationalAlertStyle = 1;
  NSCriticalAlertStyle = 2;

const
  NSAlertFirstButtonReturn = 1000;
  NSAlertSecondButtonReturn = 1001;
  NSAlertThirdButtonReturn = 1002;

{ Types }
type
  NSAlertStyle = NSUInteger;
  NSAlertStylePtr = ^NSAlertStyle;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSALERT_PAS_R}
{$define NSALERT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSALERT_PAS_F}
{$define NSALERT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSALERT_PAS_S}
{$define NSALERT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAlertDelegateProtocol = objcprotocol;
  NSAlert = objcclass;
  NSAlertPointer = ^NSAlert;
  NSAlertPtr = NSAlertPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSALERT_PAS_C}
{$define NSALERT_PAS_C}

{ NSAlert }
  NSAlert = objcclass external (NSObject)
  private
    _informationField: NSTextField;
    _first: id;
    _second: id;
    _third: id;
    _buttons: NSArray;
    _panel: NSPanel;
    _messageField: id;
    _imageView: id;
    _minButtonSize: NSSize;
    _buttonSpacing: CGFloat;
    _buttonPadding: CGFloat;
    _messagePadding: CGFloat;
    _buttonSpacingMaxX: CGFloat;
    _buttonSpacingY: CGFloat;
    _modalDelegate: id;
    _docWindow: NSWindow;
    _didEndSelector: SEL;
    _didDismissSelector: SEL;
    _unbadgedImage: NSImage;
    _defaultPanelSize: NSSize;
    _helpButton: id;
    _delegate: id;
    _alertStyle: NSAlertStyle;
    _helpAnchor: id;
    _layoutDone: Boolean;
    _showsHelp: Boolean;
    _showsSuppressionButton: Boolean;
    reserved: Boolean;
    _suppressionButton: id;
    _accessoryView: id;
    
  public
    class function alertWithError(error: NSError): NSAlert; message 'alertWithError:';
    class function alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormat(message: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; firstKey: id): NSAlert; varargs; message 'alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:';
    procedure setMessageText(messageText_: NSString); message 'setMessageText:';
    procedure setInformativeText(informativeText_: NSString); message 'setInformativeText:';
    function messageText: NSString; message 'messageText';
    function informativeText: NSString; message 'informativeText';
    procedure setIcon(icon_: NSImage); message 'setIcon:';
    function icon: NSImage; message 'icon';
    function addButtonWithTitle(title: NSString): NSButton; message 'addButtonWithTitle:';
    function buttons: NSArray; message 'buttons';
    procedure setShowsHelp(showsHelp_: Boolean); message 'setShowsHelp:';
    function showsHelp: Boolean; message 'showsHelp';
    procedure setHelpAnchor(anchor: NSString); message 'setHelpAnchor:';
    function helpAnchor: NSString; message 'helpAnchor';
    procedure setAlertStyle(style: NSAlertStyle); message 'setAlertStyle:';
    function alertStyle: NSAlertStyle; message 'alertStyle';
    procedure setDelegate(delegate_: NSAlertDelegateProtocol); message 'setDelegate:';
    function delegate: NSAlertDelegateProtocol; message 'delegate';
    procedure setShowsSuppressionButton(flag: Boolean); message 'setShowsSuppressionButton:';
    function showsSuppressionButton: Boolean; message 'showsSuppressionButton';
    function suppressionButton: NSButton; message 'suppressionButton';
    procedure setAccessoryView(view: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure layout; message 'layout';
    function runModal: NSInteger; message 'runModal';
    procedure alertDidEnd_returnCode_contextInfo(alert: NSAlert; returnCode: NSInteger; contextInfo: Pointer); message 'alertDidEnd:returnCode:contextInfo:';
    procedure beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo(window_: NSWindow; delegate_: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:';
    function window: id; message 'window';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSALERT_PAS_P}
{$define NSALERT_PAS_P}
  
{ NSAlertDelegate Protocol }
  NSAlertDelegateProtocol = objcprotocol external name 'NSAlertDelegate'
  optional
    function alertShowHelp(alert: NSAlert): Boolean; message 'alertShowHelp:';
  end;
{$endif}
{$endif}

{ Parsed from AppKit.framework NSAlert.h }


{ Types from NSAlert }
{$ifdef TYPES}

const
  NSAlertFirstButtonReturn = 1000;
  NSAlertSecondButtonReturn = 1001;
  NSAlertThirdButtonReturn = 1002;
{$endif}


{$ifdef TYPES}
type
  NSAlertPtr = ^NSAlert;
  NSAlertDelegateProtocolPtr = ^NSAlertDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSAlertStyle = NSUInteger;
  NSAlertStylePtr = ^NSAlertStyle;

const
  NSWarningAlertStyle = 0;
  NSInformationalAlertStyle = 1;
  NSCriticalAlertStyle = 2;
{$endif}

{$ifdef CLASSES}

type
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
    _layoutDone: ObjCBOOL;
    _showsHelp: ObjCBOOL;
    _showsSuppressionButton: ObjCBOOL;
    reserved: ObjCBOOL;
    _suppressionButton: id;
    _accessoryView: id;
  public
    class function alertWithError (error: NSError): NSAlert; message 'alertWithError:';
    class function alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormat (message_: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; format: NSString): NSAlert; varargs; message 'alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:'; deprecated 'in 10_3, 10_10, "Use -init instead"';
    procedure setMessageText(newValue: NSString); message 'setMessageText:';
    function messageText: NSString; message 'messageText';
    procedure setInformativeText(newValue: NSString); message 'setInformativeText:';
    function informativeText: NSString; message 'informativeText';
    procedure setIcon(newValue: NSImage); message 'setIcon:';
    function icon: NSImage; message 'icon';
    function addButtonWithTitle (title: NSString): NSButton; message 'addButtonWithTitle:';
    function buttons: NSArray; message 'buttons';
    procedure setShowsHelp(newValue: ObjCBOOL); message 'setShowsHelp:';
    function showsHelp: ObjCBOOL; message 'showsHelp';
    procedure setHelpAnchor(newValue: NSString); message 'setHelpAnchor:';
    function helpAnchor: NSString; message 'helpAnchor';
    procedure setAlertStyle(newValue: NSAlertStyle); message 'setAlertStyle:';
    function alertStyle: NSAlertStyle; message 'alertStyle';
    procedure setDelegate(newValue: NSAlertDelegateProtocol); message 'setDelegate:';
    function delegate: NSAlertDelegateProtocol; message 'delegate';
    procedure setShowsSuppressionButton(newValue: ObjCBOOL); message 'setShowsSuppressionButton:';
    function showsSuppressionButton: ObjCBOOL; message 'showsSuppressionButton';
    function suppressionButton: NSButton; message 'suppressionButton';
    procedure setAccessoryView(newValue: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure layout; message 'layout'; { available in 10_5 }
    function runModal: NSModalResponse; message 'runModal';
    procedure beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo (window: NSWindow; delegate_: id; didEndSelector: SEL; contextInfo: pointer); message 'beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:'; deprecated 'in 10_3, 10_10, "Use -beginSheetModalForWindow:completionHandler: instead"';
    procedure beginSheetModalForWindow_completionHandler (sheetWindow: NSWindow; handler: OpaqueCBlock); message 'beginSheetModalForWindow:completionHandler:'; { available in 10_9 }
    function window: id; message 'window';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSAlertDelegateProtocol = objcprotocol external name 'NSAlertDelegate' (NSObjectProtocol)
  optional
    function alertShowHelp (alert: NSAlert): ObjCBOOL; message 'alertShowHelp:';
  end;
{$endif}


{ Parsed from Appkit.framework NSAlert.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSALERT_PAS_H}
{$define NSALERT_PAS_H}
type
  NSAlertPointer = Pointer;

{$endif}
{$endif}

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
  NSAlert = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSALERT_PAS_C}
{$define NSALERT_PAS_C}

{ NSAlert }
  NSAlert = objcclass(NSObject)
  private
    __informationField: NSTextField;
    __first: id;
    __second: id;
    __third: id;
    __buttons: NSArray;
    __panel: NSPanel;
    __messageField: id;
    __imageView: id;
    __minButtonSize: NSSize;
    __buttonSpacing: CGFloat;
    __buttonPadding: CGFloat;
    __messagePadding: CGFloat;
    __buttonSpacingMaxX: CGFloat;
    __buttonSpacingY: CGFloat;
    __modalDelegate: id;
    __docWindow: NSWindow;
    __didEndSelector: SEL;
    __didDismissSelector: SEL;
    __unbadgedImage: NSImage;
    __defaultPanelSize: NSSize;
    __helpButton: id;
    __delegate: id;
    __alertStyle: NSAlertStyle;
    __helpAnchor: id;
    __layoutDone: Boolean;
    __showsHelp: Boolean;
    __showsSuppressionButton: Boolean;
    _reserved: Boolean;
    __suppressionButton: id;
    __accessoryView: id;
    
  public
    class function alloc: NSAlert; message 'alloc';

    class function alertWithError(error: NSError): NSAlert; message 'alertWithError:';
    class function alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormat(message: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; format: NSString): NSAlert; varargs; message 'alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:';
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
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setShowsSuppressionButton(flag: Boolean); message 'setShowsSuppressionButton:';
    function showsSuppressionButton: Boolean; message 'showsSuppressionButton';
    function suppressionButton: NSButton; message 'suppressionButton';
    procedure setAccessoryView(view: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure layout; message 'layout';
    function runModal: NSInteger; message 'runModal';
    procedure beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo(window_: NSWindow; delegate_: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:';
    function window: id; message 'window';
  end; external;

{$endif}
{$endif}

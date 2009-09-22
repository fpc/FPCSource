{ Parsed from Appkit.framework NSPanel.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPANEL_PAS_H}
{$define NSPANEL_PAS_H}
type
  NSPanelPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPANEL_PAS_T}
{$define NSPANEL_PAS_T}

{ Constants }

const
  NSAlertDefaultReturn = 1;
  NSAlertAlternateReturn = 0;
  NSAlertOtherReturn = -1;
  NSAlertErrorReturn = -2;

const
  NSOKButton = 1;
  NSCancelButton = 0;

const
  NSUtilityWindowMask = 1  shl  4;
  NSDocModalWindowMask = 1  shl  6;

const
  NSNonactivatingPanelMask = 1  shl  7;

const
  NSHUDWindowMask = 1  shl  13;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPANEL_PAS_R}
{$define NSPANEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPANEL_PAS_F}
{$define NSPANEL_PAS_F}

{ Functions }
function NSRunAlertPanel(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; multipleParams: array of Pointer): NSInteger; cdecl; external name 'NSRunAlertPanel';
function NSRunInformationalAlertPanel(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; multipleParams: array of Pointer): NSInteger; cdecl; external name 'NSRunInformationalAlertPanel';
function NSRunCriticalAlertPanel(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; multipleParams: array of Pointer): NSInteger; cdecl; external name 'NSRunCriticalAlertPanel';
function NSRunAlertPanelRelativeToWindow(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; docWindowPointer: NSWindow; multipleParams: array of Pointer): NSInteger; cdecl; external name 'NSRunAlertPanelRelativeToWindow';
function NSRunInformationalAlertPanelRelativeToWindow(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; docWindowPointer: NSWindow; multipleParams: array of Pointer): NSInteger; cdecl; external name 'NSRunInformationalAlertPanelRelativeToWindow';
function NSRunCriticalAlertPanelRelativeToWindow(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; docWindowPointer: NSWindow; multipleParams: array of Pointer): NSInteger; cdecl; external name 'NSRunCriticalAlertPanelRelativeToWindow';
procedure NSBeginAlertSheet(titlePointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; docWindowPointer: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfoPointer: Pointer; msgFormatPointer: NSString; multipleParams: array of Pointer); cdecl; external name 'NSBeginAlertSheet';
procedure NSBeginInformationalAlertSheet(titlePointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; docWindowPointer: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfoPointer: Pointer; msgFormatPointer: NSString; multipleParams: array of Pointer); cdecl; external name 'NSBeginInformationalAlertSheet';
procedure NSBeginCriticalAlertSheet(titlePointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; docWindowPointer: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfoPointer: Pointer; msgFormatPointer: NSString; multipleParams: array of Pointer); cdecl; external name 'NSBeginCriticalAlertSheet';
function NSGetAlertPanel(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; multipleParams: array of Pointer): id; cdecl; external name 'NSGetAlertPanel';
function NSGetInformationalAlertPanel(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; multipleParams: array of Pointer): id; cdecl; external name 'NSGetInformationalAlertPanel';
function NSGetCriticalAlertPanel(titlePointer: NSString; msgFormatPointer: NSString; defaultButtonPointer: NSString; alternateButtonPointer: NSString; otherButtonPointer: NSString; multipleParams: array of Pointer): id; cdecl; external name 'NSGetCriticalAlertPanel';
procedure NSReleaseAlertPanel(panel: id); cdecl; external name 'NSReleaseAlertPanel';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPANEL_PAS_T}
{$define NSPANEL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPanel = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPANEL_PAS_C}
{$define NSPANEL_PAS_C}

{ NSPanel }
  NSPanel = objcclass(NSWindow)
    
  public
    class function alloc: NSPanel; message 'alloc';

    function isFloatingPanel: Boolean; message 'isFloatingPanel';
    procedure setFloatingPanel(flag: Boolean); message 'setFloatingPanel:';
    function becomesKeyOnlyIfNeeded: Boolean; message 'becomesKeyOnlyIfNeeded';
    procedure setBecomesKeyOnlyIfNeeded(flag: Boolean); message 'setBecomesKeyOnlyIfNeeded:';
    function worksWhenModal: Boolean; message 'worksWhenModal';
    procedure setWorksWhenModal(flag: Boolean); message 'setWorksWhenModal:';
  end; external;

{$endif}
{$endif}

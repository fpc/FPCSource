{ Parsed from Appkit.framework NSPanel.h }

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
  NSUtilityWindowMask = 1 shl 4;
  NSDocModalWindowMask = 1 shl 6;

const
  NSNonactivatingPanelMask = 1 shl 7;

const
  NSHUDWindowMask = 1 shl 13;

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
function NSRunAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; varargs: array of const): NSInteger; cdecl; external;
function NSRunInformationalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; varargs: array of const): NSInteger; cdecl; external;
function NSRunCriticalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; varargs: array of const): NSInteger; cdecl; external;
function NSRunAlertPanelRelativeToWindow(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; varargs: array of const): NSInteger; cdecl; external; deprecated 'in Mac OS X 10 and later';
function NSRunInformationalAlertPanelRelativeToWindow(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; varargs: array of const): NSInteger; cdecl; external; deprecated 'in Mac OS X 10 and later';
function NSRunCriticalAlertPanelRelativeToWindow(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; varargs: array of const): NSInteger; cdecl; external; deprecated 'in Mac OS X 10 and later';
procedure NSBeginAlertSheet(title: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfo: Pointer; msgFormat: NSString; varargs: array of const); cdecl; external;
procedure NSBeginInformationalAlertSheet(title: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfo: Pointer; msgFormat: NSString; varargs: array of const); cdecl; external;
procedure NSBeginCriticalAlertSheet(title: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfo: Pointer; msgFormat: NSString; varargs: array of const); cdecl; external;
function NSGetAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; varargs: array of const): id; cdecl; external;
function NSGetInformationalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; varargs: array of const): id; cdecl; external;
function NSGetCriticalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; varargs: array of const): id; cdecl; external;
procedure NSReleaseAlertPanel(panel: id); cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPANEL_PAS_S}
{$define NSPANEL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPanel = objcclass;
  NSPanelPointer = ^NSPanel;
  NSPanelPtr = NSPanelPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPANEL_PAS_C}
{$define NSPANEL_PAS_C}

{ NSPanel }
  NSPanel = objcclass external (NSWindow)
    
  public
    function isFloatingPanel: Boolean; message 'isFloatingPanel';
    procedure setFloatingPanel(flag: Boolean); message 'setFloatingPanel:';
    function becomesKeyOnlyIfNeeded: Boolean; message 'becomesKeyOnlyIfNeeded';
    procedure setBecomesKeyOnlyIfNeeded(flag: Boolean); message 'setBecomesKeyOnlyIfNeeded:';
    function worksWhenModal: Boolean; message 'worksWhenModal';
    procedure setWorksWhenModal(flag: Boolean); message 'setWorksWhenModal:';
  end;

{$endif}
{$endif}

{ Parsed from AppKit.framework NSPanel.h }


{$ifdef TYPES}
type
  NSPanelPtr = ^NSPanel;
{$endif}

{$ifdef TYPES}
const
  NSUtilityWindowMask = 1 shl 4;
  NSDocModalWindowMask = 1 shl 6;
  NSNonactivatingPanelMask = 1 shl 7;
  NSHUDWindowMask = 1 shl 13 { available in 10_6 };
{$endif}

{$ifdef CLASSES}

type
  NSPanel = objcclass external (NSWindow)
  private
  public
    procedure setFloatingPanel(newValue: ObjCBOOL); message 'setFloatingPanel:';
    function isFloatingPanel: ObjCBOOL; message 'isFloatingPanel';
    procedure setBecomesKeyOnlyIfNeeded(newValue: ObjCBOOL); message 'setBecomesKeyOnlyIfNeeded:';
    function becomesKeyOnlyIfNeeded: ObjCBOOL; message 'becomesKeyOnlyIfNeeded';
    procedure setWorksWhenModal(newValue: ObjCBOOL); message 'setWorksWhenModal:';
    function worksWhenModal: ObjCBOOL; message 'worksWhenModal';
  end;
{$endif}

{$ifdef FUNCTIONS}
function NSRunAlertPanelRelativeToWindow(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow): NSInteger; varargs; cdecl; external; deprecated 'in 10_0, 10_0';
function NSRunInformationalAlertPanelRelativeToWindow(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow): NSInteger; varargs; cdecl; external; deprecated 'in 10_0, 10_0';
function NSRunCriticalAlertPanelRelativeToWindow(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow): NSInteger; varargs; cdecl; external; deprecated 'in 10_0, 10_0';
function NSRunAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString): NSInteger; varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
function NSRunInformationalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString): NSInteger; varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
function NSRunCriticalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString): NSInteger; varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
procedure NSBeginAlertSheet(title: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfo: pointer; msgFormat: NSString); varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
procedure NSBeginInformationalAlertSheet(title: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfo: pointer; msgFormat: NSString); varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
procedure NSBeginCriticalAlertSheet(title: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; didDismissSelector: SEL; contextInfo: pointer; msgFormat: NSString); varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
function NSGetAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString): id; varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
function NSGetInformationalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString): id; varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
function NSGetCriticalAlertPanel(title: NSString; msgFormat: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString): id; varargs; cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
procedure NSReleaseAlertPanel(panel: id); cdecl; external; deprecated 'in 10_0, 10_10, "Use NSAlert instead"';
{$endif}

{$ifdef TYPES}
const
  NSAlertDefaultReturn = 1 deprecated 'in 10_0, 10_10, "Use NSAlertFirstButtonReturn, etc instead"';
  NSAlertAlternateReturn = 0 deprecated 'in 10_0, 10_10, "Use NSAlertFirstButtonReturn, etc instead"';
  NSAlertOtherReturn = -1 deprecated 'in 10_0, 10_10, "Use NSAlertFirstButtonReturn, etc instead"';
  NSAlertErrorReturn = -2 deprecated 'in 10_0, 10_10, "Use NSAlertFirstButtonReturn, etc instead"';
const
  NSOKButton = 1 deprecated 'in 10_0, 10_10, "Use NSModalResponseOK instead"';
  NSCancelButton = 0 deprecated 'in 10_0, 10_10, "Use NSModalResponseCancel instead"';
{$endif}


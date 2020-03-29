{ Parsed from AppKit.framework NSHelpManager.h }


{$ifdef TYPES}
type
  NSHelpManagerPtr = ^NSHelpManager;
{$endif}

{$ifdef CLASSES}

type
  NSHelpManager = objcclass external (NSObject)
  private
    _helpMapTable: NSMapTable;
    _keyMapTable: NSMapTable;
    _bundleMapTable: NSMapTable;
    _helpWindow: NSWindow;
    _shadowWindow: NSWindow;
    _evtWindow: NSWindow;
    _helpBundle: NSBundle;
  public
    class function sharedHelpManager: NSHelpManager; message 'sharedHelpManager';
    class procedure setContextHelpModeActive (active: ObjCBOOL); message 'setContextHelpModeActive:';
    class function isContextHelpModeActive: ObjCBOOL; message 'isContextHelpModeActive';
    procedure setContextHelp_forObject (attrString: NSAttributedString; object_: id); message 'setContextHelp:forObject:';
    procedure removeContextHelpForObject (object_: id); message 'removeContextHelpForObject:';
    function contextHelpForObject (object_: id): NSAttributedString; message 'contextHelpForObject:';
    function showContextHelpForObject_locationHint (object_: id; pt: NSPoint): ObjCBOOL; message 'showContextHelpForObject:locationHint:';
    procedure openHelpAnchor_inBook (anchor: NSString; book: NSString); message 'openHelpAnchor:inBook:';
    procedure findString_inBook (query: NSString; book: NSString); message 'findString:inBook:';
    function registerBooksInBundle (bundle: NSBundle): ObjCBOOL; message 'registerBooksInBundle:'; { available in 10_6 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSContextHelpModeDidActivateNotification: NSString; cvar; external;
  NSContextHelpModeDidDeactivateNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSBundleHelpExtension = objccategory external (NSBundle)
    function contextHelpForKey (key: NSString): NSAttributedString; message 'contextHelpForKey:';
  end;

type
  NSApplicationHelpExtension = objccategory external (NSApplication)
    procedure activateContextHelpMode (sender: id); message 'activateContextHelpMode:';
    procedure showHelp (sender: id); message 'showHelp:';
  end;
{$endif}


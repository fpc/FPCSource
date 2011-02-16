{ Parsed from Appkit.framework NSHelpManager.h }

{$ifdef TYPES}
{$ifndef NSHELPMANAGER_PAS_T}
{$define NSHELPMANAGER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHELPMANAGER_PAS_R}
{$define NSHELPMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHELPMANAGER_PAS_F}
{$define NSHELPMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHELPMANAGER_PAS_S}
{$define NSHELPMANAGER_PAS_S}

{ External string constants }
var
  NSContextHelpModeDidActivateNotification: NSString; cvar; external;
  NSContextHelpModeDidDeactivateNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHelpManager = objcclass;
  NSHelpManagerPointer = ^NSHelpManager;
  NSHelpManagerPtr = NSHelpManagerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHELPMANAGER_PAS_C}
{$define NSHELPMANAGER_PAS_C}

{ NSHelpManager }
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
    class procedure setContextHelpModeActive(active: Boolean); message 'setContextHelpModeActive:';
    class function isContextHelpModeActive: Boolean; message 'isContextHelpModeActive';
    procedure setContextHelp_forObject(attrString: NSAttributedString; object_: id); message 'setContextHelp:forObject:';
    procedure removeContextHelpForObject(object_: id); message 'removeContextHelpForObject:';
    function contextHelpForObject(object_: id): NSAttributedString; message 'contextHelpForObject:';
    function showContextHelpForObject_locationHint(object_: id; pt: NSPoint): Boolean; message 'showContextHelpForObject:locationHint:';
    procedure openHelpAnchor_inBook(anchor: NSString; book: NSString); message 'openHelpAnchor:inBook:';
    procedure findString_inBook(query: NSString; book: NSString); message 'findString:inBook:';
    function registerBooksInBundle(bundle: NSBundle): Boolean; message 'registerBooksInBundle:';
  end;

{ NSBundleHelpExtensionCategory }
  NSBundleHelpExtensionCategory = objccategory external (NSBundle)
    function contextHelpForKey(key: NSString): NSAttributedString; message 'contextHelpForKey:';
  end;

{ NSApplicationHelpExtensionCategory }
  NSApplicationHelpExtensionCategory = objccategory external (NSApplication)
    procedure activateContextHelpMode(sender: id); message 'activateContextHelpMode:';
    procedure showHelp(sender: id); message 'showHelp:';
  end;

{$endif}
{$endif}

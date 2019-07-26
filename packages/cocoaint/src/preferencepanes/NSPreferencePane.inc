{ Parsed from PreferencePanes.framework NSPreferencePane.h }


{$ifdef TYPES}
type
  NSPreferencePanePtr = ^NSPreferencePane;
{$endif}

{$ifdef TYPES}
const
  NSUnselectCancel = 0;
  NSUnselectNow = 1;
  NSUnselectLater = 2;
type
  NSPreferencePaneUnselectReply = clong;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPreferencePaneDoUnselectNotification: NSString; cvar; external;
  NSPreferencePaneCancelUnselectNotification: NSString; cvar; external;
  NSPrefPaneHelpMenuInfoPListKey: NSString; cvar; external;
  NSPrefPaneHelpMenuTitleKey: NSString; cvar; external;
  NSPrefPaneHelpMenuAnchorKey: NSString; cvar; external;
{$endif}

{$if (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_7)}

{$else}
{$ifdef TYPES}
const
  kNSPrefPaneHelpMenuInfoPListKey = 'NSPrefPaneHelpAnchors';
  kNSPrefPaneHelpMenuTitleKey = 'title';
  kNSPrefPaneHelpMenuAnchorKey = 'anchor';
{$endif}

{$endif}
{$ifdef CLASSES}

type
  NSPreferencePane = objcclass external (NSObject)
  private
    _window: NSWindow;
    _initialKeyView: NSView;
    _firstKeyView: NSView;
    _lastKeyView: NSView;
    _mainView: NSView;
    _bundle: NSBundle;
    _reserved1: id;
    _reserved2: id;
    _reserved3: id;
  public
    function initWithBundle (bundle: NSBundle): instancetype; message 'initWithBundle:';
    function bundle: NSBundle; message 'bundle';
    function loadMainView: NSView; message 'loadMainView';
    procedure mainViewDidLoad; message 'mainViewDidLoad';
    function mainNibName: NSString; message 'mainNibName';
    procedure assignMainView; message 'assignMainView';
    procedure willSelect; message 'willSelect';
    procedure didSelect; message 'didSelect';
    function shouldUnselect: NSPreferencePaneUnselectReply; message 'shouldUnselect';
    procedure replyToShouldUnselect (shouldUnselect_: ObjCBOOL); message 'replyToShouldUnselect:';
    procedure willUnselect; message 'willUnselect';
    procedure didUnselect; message 'didUnselect';
    procedure setMainView(newValue: NSView); message 'setMainView:';
    function mainView: NSView; message 'mainView';
    procedure setInitialKeyView(newValue: NSView); message 'setInitialKeyView:';
    function initialKeyView: NSView; message 'initialKeyView';
    procedure setFirstKeyView(newValue: NSView); message 'setFirstKeyView:';
    function firstKeyView: NSView; message 'firstKeyView';
    procedure setLastKeyView(newValue: NSView); message 'setLastKeyView:';
    function lastKeyView: NSView; message 'lastKeyView';
    function autoSaveTextFields: ObjCBOOL; message 'autoSaveTextFields';
    function isSelected: ObjCBOOL; message 'isSelected';
    procedure updateHelpMenuWithArray (inArrayOfMenuItems: NSArray); message 'updateHelpMenuWithArray:';
  end;
{$endif}


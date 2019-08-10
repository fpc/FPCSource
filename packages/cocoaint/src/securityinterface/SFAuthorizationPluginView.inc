{ Parsed from SecurityInterface.framework SFAuthorizationPluginView.h }


{$ifdef TYPES}
type
  SFAuthorizationPluginViewPtr = ^SFAuthorizationPluginView;
{$endif}

{$ifndef _SFAUTHORIZATIONPLUGINVIEW_H_}
{$ifdef TYPES}
const
  SFButtonTypeCancel = NSCancelButton;
  SFButtonTypeOK = NSOKButton;
  SFButtonTypeBack = SFButtonTypeCancel;
  SFButtonTypeLogin = SFButtonTypeOK;
type
  SFButtonType = clong;
const
  SFViewTypeIdentityAndCredentials = 0;
  SFViewTypeCredentials = 1;
type
  SFViewType = clong;
{$endif}

{$ifdef CLASSES}

type
  SFAuthorizationPluginView = objcclass external (NSObject)
  private
    _callbacks: AuthorizationCallbacksPtr;
    _engineRef: AuthorizationEngineRef;
    _private: pointer;
  public
    function initWithCallbacks_andEngineRef (callbacks: AuthorizationCallbacksPtr; engineRef: AuthorizationEngineRef): id; message 'initWithCallbacks:andEngineRef:';
    function engineRef: AuthorizationEngineRef; message 'engineRef';
    function callbacks: AuthorizationCallbacksPtr; message 'callbacks';
    procedure buttonPressed (inButtonType: SFButtonType); message 'buttonPressed:';
    function lastError: NSError; message 'lastError';
    procedure didActivate; message 'didActivate';
    procedure willActivateWithUser (inUserInformation: NSDictionary); message 'willActivateWithUser:';
    procedure didDeactivate; message 'didDeactivate';
    function firstKeyView: NSView; message 'firstKeyView';
    function firstResponder: NSResponder; message 'firstResponder';
    function lastKeyView: NSView; message 'lastKeyView';
    procedure setEnabled (inEnabled: ObjCBOOL); message 'setEnabled:';
    function viewForType (inType: SFViewType): NSView; message 'viewForType:';
  end;


type
  SFHostControl = objccategory external (SFAuthorizationPluginView)
    procedure displayView; message 'displayView';
    procedure setButton_enabled (inButtonType: SFButtonType; inEnabled: ObjCBOOL); message 'setButton:enabled:';
    procedure updateView; message 'updateView';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SFAuthorizationPluginViewUserNameKey: NSString; cvar; external;
  SFAuthorizationPluginViewUserShortNameKey: NSString; cvar; external;
  SFDisplayViewException: NSString; cvar; external;
{$endif}

{$endif}

{ Parsed from InputMethodKit.framework IMKServer.h }


{$ifdef TYPES}
type
  IMKServerPtr = ^IMKServer;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  IMKModeDictionary: NSString; cvar; external;
  IMKControllerClass: NSString; cvar; external;
  IMKDelegateClass: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  IMKServer = objcclass external (NSObject, IMKServerProxyProtocol)
  private
    _private: IMKServerPrivate;
  public
    function initWithName_bundleIdentifier (name: NSString; bundleIdentifier: NSString): id; message 'initWithName:bundleIdentifier:';
    function initWithName_controllerClass_delegateClass (name: NSString; controllerClassID: pobjc_class; delegateClassID: pobjc_class): id; message 'initWithName:controllerClass:delegateClass:';
    function bundle: NSBundle; message 'bundle';
    function paletteWillTerminate: ObjCBOOL; message 'paletteWillTerminate';
    function lastKeyEventWasDeadKey: ObjCBOOL; message 'lastKeyEventWasDeadKey';
  end;
{$endif}


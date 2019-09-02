{ Parsed from WebKit.framework WebScriptObject.h }


{ Types from WebScriptObject }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  WebScriptObjectPtr = ^WebScriptObject;
  WebUndefinedPtr = ^WebUndefined;
{$endif}

{$ifndef WebScriptObject_h}
{$ifdef CLASSES}

type
  WebScripting = objccategory external (NSObject)
    class function webScriptNameForSelector (selector: SEL): NSString; message 'webScriptNameForSelector:'; { available in 10_4 }
    class function isSelectorExcludedFromWebScript (selector: SEL): ObjCBOOL; message 'isSelectorExcludedFromWebScript:'; { available in 10_4 }
    class function webScriptNameForKey (name: PChar): NSString; message 'webScriptNameForKey:'; { available in 10_4 }
    class function isKeyExcludedFromWebScript (name: PChar): ObjCBOOL; message 'isKeyExcludedFromWebScript:'; { available in 10_4 }
    function invokeUndefinedMethodFromWebScript_withArguments (name: NSString; arguments: NSArray): id; message 'invokeUndefinedMethodFromWebScript:withArguments:'; { available in 10_4 }
    function invokeDefaultMethodWithArguments (arguments: NSArray): id; message 'invokeDefaultMethodWithArguments:'; { available in 10_4 }
    procedure finalizeForWebScript; message 'finalizeForWebScript'; { available in 10_4 }
  end;


type
  WebScriptObject = objcclass external (NSObject)
  private
    _private: WebScriptObjectPrivate;
  public
    class function throwException (exceptionMessage: NSString): ObjCBOOL; message 'throwException:';
    function JSObject: JSObjectRef; message 'JSObject'; { available in 10_5 }
    function callWebScriptMethod_withArguments (name: NSString; arguments: NSArray): id; message 'callWebScriptMethod:withArguments:';
    function evaluateWebScript (script: NSString): id; message 'evaluateWebScript:';
    procedure removeWebScriptKey (name: NSString); message 'removeWebScriptKey:';
    function stringRepresentation: NSString; message 'stringRepresentation';
    function webScriptValueAtIndex (index: cuint): id; message 'webScriptValueAtIndex:';
    procedure setWebScriptValueAtIndex_value (index: cuint; value: id); message 'setWebScriptValueAtIndex:value:';
    procedure setException (description: NSString); message 'setException:';
    {$if defined(JSC_OBJC_API_ENABLED)}
    function JSValue_: JSValue; message 'JSValue';
    {$endif}
  end;

type
  WebUndefined = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  public
    class function undefined: WebUndefined; message 'undefined';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$endif}

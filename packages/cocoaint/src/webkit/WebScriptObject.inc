{ Parsed from Webkit.framework WebScriptObject.h }

{$ifdef TYPES}
{$ifndef WEBSCRIPTOBJECT_PAS_T}
{$define WEBSCRIPTOBJECT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBSCRIPTOBJECT_PAS_R}
{$define WEBSCRIPTOBJECT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBSCRIPTOBJECT_PAS_F}
{$define WEBSCRIPTOBJECT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBSCRIPTOBJECT_PAS_S}
{$define WEBSCRIPTOBJECT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebScriptObject = objcclass;
  WebScriptObjectPointer = ^WebScriptObject;
  WebScriptObjectPtr = WebScriptObjectPointer;
  WebUndefined = objcclass;
  WebUndefinedPointer = ^WebUndefined;
  WebUndefinedPtr = WebUndefinedPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBSCRIPTOBJECT_PAS_C}
{$define WEBSCRIPTOBJECT_PAS_C}

{ WebScriptObject }
  WebScriptObject = objcclass external (NSObject)
  private
    _private: WebScriptObjectPrivate;
    
  public
    class function throwException(exceptionMessage: NSString): Boolean; message 'throwException:';
    function JSObject: JSObjectRef; message 'JSObject';
    function callWebScriptMethod_withArguments(name: NSString; arguments: NSArray): id; message 'callWebScriptMethod:withArguments:';
    function evaluateWebScript(script: NSString): id; message 'evaluateWebScript:';
    procedure removeWebScriptKey(name: NSString); message 'removeWebScriptKey:';
    function stringRepresentation: NSString; message 'stringRepresentation';
    function webScriptValueAtIndex(index: cuint): id; message 'webScriptValueAtIndex:';
    procedure setWebScriptValueAtIndex_value(index: cuint; value: id); message 'setWebScriptValueAtIndex:value:';
    procedure setException(description_: NSString); message 'setException:';
  end;

{ WebUndefined }
  WebUndefined = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
    
  public
    class function undefined: WebUndefined; message 'undefined';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ WebScriptingCategory }
  WebScriptingCategory = objccategory external (NSObject)
    class function webScriptNameForSelector(selector: SEL): NSString; message 'webScriptNameForSelector:';
    class function isSelectorExcludedFromWebScript(selector: SEL): Boolean; message 'isSelectorExcludedFromWebScript:';
    class function webScriptNameForKey(name: PChar): NSString; message 'webScriptNameForKey:';
    class function isKeyExcludedFromWebScript(name: PChar): Boolean; message 'isKeyExcludedFromWebScript:';
    function invokeUndefinedMethodFromWebScript_withArguments(name: NSString; arguments: NSArray): id; message 'invokeUndefinedMethodFromWebScript:withArguments:';
    function invokeDefaultMethodWithArguments(arguments: NSArray): id; message 'invokeDefaultMethodWithArguments:';
    procedure finalizeForWebScript; message 'finalizeForWebScript';
  end;

{$endif}
{$endif}

{ Parsed from Webkit.framework WebScriptObject.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
  WebUndefined = objcclass;
  WebUndefinedPointer = ^WebUndefined;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBSCRIPTOBJECT_PAS_C}
{$define WEBSCRIPTOBJECT_PAS_C}

{ WebScriptObject }
  WebScriptObject = objcclass(NSObject)
  private
    __private: WebScriptObjectPrivate;
    
  public
    class function alloc: WebScriptObject; message 'alloc';

    class function throwException(exceptionMessage: NSStringPointer): Boolean; message 'throwException:';
    function JSObject: JSObjectRef; message 'JSObject';
    function callWebScriptMethod_withArguments(name: NSStringPointer; args: NSArrayPointer): id; message 'callWebScriptMethod:withArguments:';
    function evaluateWebScript(script: NSStringPointer): id; message 'evaluateWebScript:';
    procedure removeWebScriptKey(name: NSStringPointer); message 'removeWebScriptKey:';
    function stringRepresentation: NSString; message 'stringRepresentation';
    function webScriptValueAtIndex(index: cuint): id; message 'webScriptValueAtIndex:';
    procedure setWebScriptValueAtIndex_value(index: cuint; value: id); message 'setWebScriptValueAtIndex:value:';
    procedure setException(description_: NSStringPointer); message 'setException:';
  end; external;

{ WebUndefined }
  WebUndefined = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
    
  public
    class function alloc: WebUndefined; message 'alloc';

    class function undefined: WebUndefined; message 'undefined';
  end; external;

{$endif}
{$endif}

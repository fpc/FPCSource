{ Parsed from Foundation.framework NSAppleScript.h }


{$ifdef TYPES}
type
  NSAppleScriptPtr = ^NSAppleScript;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAppleScriptErrorMessage: NSString { available in 10_2, NA }; cvar; external;
  NSAppleScriptErrorNumber: NSString { available in 10_2, NA }; cvar; external;
  NSAppleScriptErrorAppName: NSString { available in 10_2, NA }; cvar; external;
  NSAppleScriptErrorBriefMessage: NSString { available in 10_2, NA }; cvar; external;
  NSAppleScriptErrorRange: NSString { available in 10_2, NA }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSAppleScript = objcclass external (NSObject, NSCopyingProtocol)
  private
    _source: NSString;
    _compiledScriptID: cuint;
    _reserved1: pointer;
    _reserved2: pointer;
  public
    function initWithContentsOfURL_error (url: NSURL; errorInfo: NSDictionaryPtr): instancetype; message 'initWithContentsOfURL:error:'; { NS_DESIGNATED_INITIALIZER }
    function initWithSource (source: NSString): instancetype; message 'initWithSource:'; { NS_DESIGNATED_INITIALIZER }
    function source: NSString; message 'source';
    function isCompiled: ObjCBOOL; message 'isCompiled';
    function compileAndReturnError (errorInfo: NSDictionaryPtr): ObjCBOOL; message 'compileAndReturnError:';
    function executeAndReturnError (errorInfo: NSDictionaryPtr): NSAppleEventDescriptor; message 'executeAndReturnError:';
    function executeAppleEvent_error (event: NSAppleEventDescriptor; errorInfo: NSDictionaryPtr): NSAppleEventDescriptor; message 'executeAppleEvent:error:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}


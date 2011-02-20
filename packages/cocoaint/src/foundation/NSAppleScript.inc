{ Parsed from Foundation.framework NSAppleScript.h }

{$ifdef TYPES}
{$ifndef NSAPPLESCRIPT_PAS_T}
{$define NSAPPLESCRIPT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSAPPLESCRIPT_PAS_R}
{$define NSAPPLESCRIPT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAPPLESCRIPT_PAS_F}
{$define NSAPPLESCRIPT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAPPLESCRIPT_PAS_S}
{$define NSAPPLESCRIPT_PAS_S}

{ External string constants }
var
  NSAppleScriptErrorMessage: NSString; cvar; external;
  NSAppleScriptErrorNumber: NSString; cvar; external;
  NSAppleScriptErrorAppName: NSString; cvar; external;
  NSAppleScriptErrorBriefMessage: NSString; cvar; external;
  NSAppleScriptErrorRange: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAppleScript = objcclass;
  NSAppleScriptPointer = ^NSAppleScript;
  NSAppleScriptPtr = NSAppleScriptPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAPPLESCRIPT_PAS_C}
{$define NSAPPLESCRIPT_PAS_C}

{ NSAppleScript }
  NSAppleScript = objcclass external (NSObject, NSCopyingProtocol)
  private
    _source: NSString;
    _compiledScriptID: cuint;
    _reserved1: Pointer;
    _reserved2: Pointer;
    
  public
    function initWithContentsOfURL_error(url: NSURL; errorInfo: NSDictionaryPointer): id; message 'initWithContentsOfURL:error:';
    function initWithSource(source_: NSString): id; message 'initWithSource:';
    function source: NSString; message 'source';
    function isCompiled: Boolean; message 'isCompiled';
    function compileAndReturnError(errorInfo: NSDictionaryPointer): Boolean; message 'compileAndReturnError:';
    function executeAndReturnError(errorInfo: NSDictionaryPointer): NSAppleEventDescriptor; message 'executeAndReturnError:';
    function executeAppleEvent_error(event: NSAppleEventDescriptor; errorInfo: NSDictionaryPointer): NSAppleEventDescriptor; message 'executeAppleEvent:error:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}

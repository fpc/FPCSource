{ Parsed from Foundation.framework NSAppleScript.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSAPPLESCRIPT_PAS_H}
{$define NSAPPLESCRIPT_PAS_H}
type
  NSAppleScriptPointer = Pointer;

{$endif}
{$endif}

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
  NSAppleScriptErrorMessage: NSString; external name '_NSAppleScriptErrorMessage';
  NSAppleScriptErrorNumber: NSString; external name '_NSAppleScriptErrorNumber';
  NSAppleScriptErrorAppName: NSString; external name '_NSAppleScriptErrorAppName';
  NSAppleScriptErrorBriefMessage: NSString; external name '_NSAppleScriptErrorBriefMessage';
  NSAppleScriptErrorRange: NSString; external name '_NSAppleScriptErrorRange';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAppleScript = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAPPLESCRIPT_PAS_C}
{$define NSAPPLESCRIPT_PAS_C}

{ NSAppleScript }
  NSAppleScript = objcclass(NSObject, NSCopyingProtocol)
  private
    __source: NSString;
    __compiledScriptID: cuint;
    __reserved1: Pointer;
    __reserved2: Pointer;
    
  public
    class function alloc: NSAppleScript; message 'alloc';

    function initWithContentsOfURL_error(url: NSURL; errorInfo: NSDictionaryPointer): id; message 'initWithContentsOfURL:error:';
    function initWithSource(source_: NSString): id; message 'initWithSource:';
    function source: NSString; message 'source';
    function isCompiled: Boolean; message 'isCompiled';
    function compileAndReturnError(errorInfo: NSDictionaryPointer): Boolean; message 'compileAndReturnError:';
    function executeAndReturnError(errorInfo: NSDictionaryPointer): NSAppleEventDescriptor; message 'executeAndReturnError:';
    function executeAppleEvent_error(event: NSAppleEventDescriptor; errorInfo: NSDictionaryPointer): NSAppleEventDescriptor; message 'executeAppleEvent:error:';

    { Category: NSExtensions }
    function richTextSource: NSAttributedString; message 'richTextSource';
  end; external;

{$endif}
{$endif}

{ Parsed from Appkit.framework NSApplicationScripting.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSAPPLICATIONSCRIPTING_PAS_T}
{$define NSAPPLICATIONSCRIPTING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSAPPLICATIONSCRIPTING_PAS_R}
{$define NSAPPLICATIONSCRIPTING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAPPLICATIONSCRIPTING_PAS_F}
{$define NSAPPLICATIONSCRIPTING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAPPLICATIONSCRIPTING_PAS_S}
{$define NSAPPLICATIONSCRIPTING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSAPPLICATIONSCRIPTING_PAS_C}
{$define NSAPPLICATIONSCRIPTING_PAS_C}

{ NSScripting_NSApplicationCategory }
  NSScripting_NSApplicationCategory = objccategory(NSApplication)
    function orderedDocuments: NSArray; message 'orderedDocuments';
    function orderedWindows: NSArray; message 'orderedWindows';
  end; external name 'NSScripting';

{ NSApplicationScriptingDelegationCategory }
  NSApplicationScriptingDelegationCategory = objccategory(NSObject)
    function application_delegateHandlesKey(sender: NSApplication; key: NSString): Boolean; message 'application:delegateHandlesKey:';
  end; external;

{$endif}
{$endif}

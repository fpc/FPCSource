{ Parsed from AppKit.framework NSApplicationScripting.h }

{$ifdef CLASSES}

type
  NSApplication_NSScripting = objccategory external name 'NSScripting' (NSApplication)
    function orderedDocuments: NSArray; message 'orderedDocuments';
    function orderedWindows: NSArray; message 'orderedWindows';
  end;

type
  NSApplicationScriptingDelegation = objccategory external (NSObject)
    function application_delegateHandlesKey (sender: NSApplication; key: NSString): ObjCBOOL; message 'application:delegateHandlesKey:';
  end;
{$endif}


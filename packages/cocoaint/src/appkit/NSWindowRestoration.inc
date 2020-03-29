{ Parsed from AppKit.framework NSWindowRestoration.h }


{$ifdef TYPES}
type
  NSWindowRestorationProtocolPtr = ^NSWindowRestorationProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSWindowRestorationProtocol = objcprotocol external name 'NSWindowRestoration' (NSObjectProtocol)
    class procedure restoreWindowWithIdentifier_state_completionHandler (identifier: NSString; state: NSCoder; completionHandler: OpaqueCBlock); message 'restoreWindowWithIdentifier:state:completionHandler:'; { available in 10_7 }
  end;
{$endif}

{$ifdef CLASSES}

type
  NSDocumentController_NSWindowRestoration = objccategory external name 'NSWindowRestoration' (NSDocumentController)
  end;

type
  NSWindowRestoration = objccategory external (NSApplication)
    function restoreWindowWithIdentifier_state_completionHandler (identifier: NSString; state: NSCoder; completionHandler: OpaqueCBlock): ObjCBOOL; message 'restoreWindowWithIdentifier:state:completionHandler:'; { available in 10_7 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSApplicationDidFinishRestoringWindowsNotification: NSString { available in 10_7 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSUserInterfaceRestoration = objccategory external (NSWindow)
    procedure setRestorable(newValue: ObjCBOOL); message 'setRestorable:';
    function isRestorable: ObjCBOOL; message 'isRestorable';
    procedure setRestorationClass(newValue: NSWindowRestorationProtocol); message 'setRestorationClass:';
    function restorationClass: NSWindowRestorationProtocol; message 'restorationClass';
    procedure disableSnapshotRestoration; message 'disableSnapshotRestoration';
    procedure enableSnapshotRestoration; message 'enableSnapshotRestoration';
  end;

type
  NSResponder_NSRestorableState = objccategory external name 'NSRestorableState' (NSResponder)
    procedure encodeRestorableStateWithCoder (coder: NSCoder); message 'encodeRestorableStateWithCoder:'; { available in 10_7 }
    procedure restoreStateWithCoder (coder: NSCoder); message 'restoreStateWithCoder:'; { available in 10_7 }
    procedure invalidateRestorableState; message 'invalidateRestorableState'; { available in 10_7 }
    class function restorableStateKeyPaths: NSArray; message 'restorableStateKeyPaths'; { available in 10_7 }
  end;

type
  NSRestorableStateExtension = objccategory external (NSApplication)
    procedure extendStateRestoration; message 'extendStateRestoration'; { available in 10_7 }
    procedure completeStateRestoration; message 'completeStateRestoration'; { available in 10_7 }
  end;

type
  NSRestorableState = objccategory external (NSDocument)
    procedure restoreDocumentWindowWithIdentifier_state_completionHandler (identifier: NSString; state: NSCoder; completionHandler: OpaqueCBlock); message 'restoreDocumentWindowWithIdentifier:state:completionHandler:'; { available in 10_7 }
    procedure encodeRestorableStateWithCoder (coder: NSCoder); message 'encodeRestorableStateWithCoder:'; { available in 10_7 }
    procedure restoreStateWithCoder (coder: NSCoder); message 'restoreStateWithCoder:'; { available in 10_7 }
    procedure invalidateRestorableState; message 'invalidateRestorableState'; { available in 10_7 }
    class function restorableStateKeyPaths: NSArray; message 'restorableStateKeyPaths'; { available in 10_7 }
  end;
{$endif}


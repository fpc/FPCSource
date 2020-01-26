{ Parsed from CloudKit.framework CKOperation.h }


{$ifdef TYPES}
type
  CKOperationPtr = ^CKOperation;
{$endif}

{$ifdef CLASSES}

type
  CKOperation = objcclass external (NSOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    procedure setContainer(newValue: CKContainer); message 'setContainer:';
    function container: CKContainer; message 'container';
    procedure setUsesBackgroundSession(newValue: ObjCBOOL); message 'setUsesBackgroundSession:';
    function usesBackgroundSession: ObjCBOOL; message 'usesBackgroundSession';
    procedure setAllowsCellularAccess(newValue: ObjCBOOL); message 'setAllowsCellularAccess:';
    function allowsCellularAccess: ObjCBOOL; message 'allowsCellularAccess';
  end;
{$endif}


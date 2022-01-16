{ Parsed from CloudKit.framework CKDatabaseOperation.h }


{$ifdef TYPES}
type
  CKDatabaseOperationPtr = ^CKDatabaseOperation;
{$endif}

{$ifdef CLASSES}

type
  CKDatabaseOperation = objcclass external (CKOperation)
  public
    procedure setDatabase(newValue: CKDatabase); message 'setDatabase:';
    function database: CKDatabase; message 'database';
  end;
{$endif}


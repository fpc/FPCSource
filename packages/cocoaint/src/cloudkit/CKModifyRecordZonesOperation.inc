{ Parsed from CloudKit.framework CKModifyRecordZonesOperation.h }


{$ifdef TYPES}
type
  CKModifyRecordZonesOperationPtr = ^CKModifyRecordZonesOperation;
{$endif}

{$ifdef CLASSES}

type
  CKModifyRecordZonesOperation = objcclass external (CKDatabaseOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithRecordZonesToSave_recordZoneIDsToDelete (recordZonesToSave: NSArray; recordZoneIDsToDelete: NSArray): instancetype; message 'initWithRecordZonesToSave:recordZoneIDsToDelete:';
    procedure setRecordZonesToSave(newValue: NSArray); message 'setRecordZonesToSave:';
    function recordZonesToSave: NSArray; message 'recordZonesToSave';
    procedure setRecordZoneIDsToDelete(newValue: NSArray); message 'setRecordZoneIDsToDelete:';
    function recordZoneIDsToDelete: NSArray; message 'recordZoneIDsToDelete';
    procedure setModifyRecordZonesCompletionBlock(newValue: OpaqueCBlock); message 'setModifyRecordZonesCompletionBlock:';
    function modifyRecordZonesCompletionBlock: OpaqueCBlock; message 'modifyRecordZonesCompletionBlock';
  end;
{$endif}


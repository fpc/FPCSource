{ Parsed from CloudKit.framework CKModifyRecordsOperation.h }


{$ifdef TYPES}
type
  CKModifyRecordsOperationPtr = ^CKModifyRecordsOperation;
{$endif}

{$ifdef TYPES}
type
  CKRecordSavePolicy = NSInteger;
  CKRecordSavePolicyPtr = ^CKRecordSavePolicy;

const
  CKRecordSaveIfServerRecordUnchanged = 0;
  CKRecordSaveChangedKeys = 1;
  CKRecordSaveAllKeys = 2;
{$endif}

{$ifdef CLASSES}

type
  CKModifyRecordsOperation = objcclass external (CKDatabaseOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithRecordsToSave_recordIDsToDelete (records: NSArray; recordIDs: NSArray): instancetype; message 'initWithRecordsToSave:recordIDsToDelete:';
    procedure setRecordsToSave(newValue: NSArray); message 'setRecordsToSave:';
    function recordsToSave: NSArray; message 'recordsToSave';
    procedure setRecordIDsToDelete(newValue: NSArray); message 'setRecordIDsToDelete:';
    function recordIDsToDelete: NSArray; message 'recordIDsToDelete';
    procedure setSavePolicy(newValue: CKRecordSavePolicy); message 'setSavePolicy:';
    function savePolicy: CKRecordSavePolicy; message 'savePolicy';
    procedure setClientChangeTokenData(newValue: NSData); message 'setClientChangeTokenData:';
    function clientChangeTokenData: NSData; message 'clientChangeTokenData';
    procedure setAtomic(newValue: ObjCBOOL); message 'setAtomic:';
    function atomic: ObjCBOOL; message 'atomic';
    procedure setPerRecordProgressBlock(newValue: OpaqueCBlock); message 'setPerRecordProgressBlock:';
    function perRecordProgressBlock: OpaqueCBlock; message 'perRecordProgressBlock';
    procedure setPerRecordCompletionBlock(newValue: OpaqueCBlock); message 'setPerRecordCompletionBlock:';
    function perRecordCompletionBlock: OpaqueCBlock; message 'perRecordCompletionBlock';
    procedure setModifyRecordsCompletionBlock(newValue: OpaqueCBlock); message 'setModifyRecordsCompletionBlock:';
    function modifyRecordsCompletionBlock: OpaqueCBlock; message 'modifyRecordsCompletionBlock';
  end;
{$endif}


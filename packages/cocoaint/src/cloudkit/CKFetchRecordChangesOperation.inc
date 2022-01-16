{ Parsed from CloudKit.framework CKFetchRecordChangesOperation.h }


{$ifdef TYPES}
type
  CKServerChangeTokenPtr = ^CKServerChangeToken;
  CKFetchRecordChangesOperationPtr = ^CKFetchRecordChangesOperation;
{$endif}

{$ifdef CLASSES}

type
  CKServerChangeToken = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    function init: instancetype; message 'init';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  CKFetchRecordChangesOperation = objcclass external (CKDatabaseOperation)
  public
    function initWithRecordZoneID_previousServerChangeToken (recordZoneID: CKRecordZoneID; previousServerChangeToken: CKServerChangeToken): instancetype; message 'initWithRecordZoneID:previousServerChangeToken:';
    procedure setRecordZoneID(newValue: CKRecordZoneID); message 'setRecordZoneID:';
    function recordZoneID: CKRecordZoneID; message 'recordZoneID';
    procedure setPreviousServerChangeToken(newValue: CKServerChangeToken); message 'setPreviousServerChangeToken:';
    function previousServerChangeToken: CKServerChangeToken; message 'previousServerChangeToken';
    procedure setResultsLimit(newValue: NSUInteger); message 'setResultsLimit:';
    function resultsLimit: NSUInteger; message 'resultsLimit';
    procedure setDesiredKeys(newValue: NSArray); message 'setDesiredKeys:';
    function desiredKeys: NSArray; message 'desiredKeys';
    procedure setRecordChangedBlock(newValue: OpaqueCBlock); message 'setRecordChangedBlock:';
    function recordChangedBlock: OpaqueCBlock; message 'recordChangedBlock';
    procedure setRecordWithIDWasDeletedBlock(newValue: OpaqueCBlock); message 'setRecordWithIDWasDeletedBlock:';
    function recordWithIDWasDeletedBlock: OpaqueCBlock; message 'recordWithIDWasDeletedBlock';
    function moreComing: ObjCBOOL; message 'moreComing';
    procedure setFetchRecordChangesCompletionBlock(newValue: OpaqueCBlock); message 'setFetchRecordChangesCompletionBlock:';
    function fetchRecordChangesCompletionBlock: OpaqueCBlock; message 'fetchRecordChangesCompletionBlock';
  end;
{$endif}


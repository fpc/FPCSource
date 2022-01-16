{ Parsed from CloudKit.framework CKQueryOperation.h }


{$ifdef TYPES}
type
  CKQueryCursorPtr = ^CKQueryCursor;
  CKQueryOperationPtr = ^CKQueryOperation;
{$endif}

{$ifdef CLASSES}

type
  CKQueryCursor = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    function init: instancetype; message 'init';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CKQueryOperationMaximumResults: NSUInteger { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CKQueryOperation = objcclass external (CKDatabaseOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithQuery (query: CKQuery): instancetype; message 'initWithQuery:';
    function initWithCursor (cursor: CKQueryCursor): instancetype; message 'initWithCursor:';
    procedure setQuery(newValue: CKQuery); message 'setQuery:';
    function query: CKQuery; message 'query';
    procedure setCursor(newValue: CKQueryCursor); message 'setCursor:';
    function cursor: CKQueryCursor; message 'cursor';
    procedure setZoneID(newValue: CKRecordZoneID); message 'setZoneID:';
    function zoneID: CKRecordZoneID; message 'zoneID';
    procedure setResultsLimit(newValue: NSUInteger); message 'setResultsLimit:';
    function resultsLimit: NSUInteger; message 'resultsLimit';
    procedure setDesiredKeys(newValue: NSArray); message 'setDesiredKeys:';
    function desiredKeys: NSArray; message 'desiredKeys';
    procedure setRecordFetchedBlock(newValue: OpaqueCBlock); message 'setRecordFetchedBlock:';
    function recordFetchedBlock: OpaqueCBlock; message 'recordFetchedBlock';
    procedure setQueryCompletionBlock(newValue: OpaqueCBlock); message 'setQueryCompletionBlock:';
    function queryCompletionBlock: OpaqueCBlock; message 'queryCompletionBlock';
  end;
{$endif}


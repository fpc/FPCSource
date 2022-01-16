{ Parsed from CloudKit.framework CKRecord.h }


{$ifdef TYPES}
type
  CKRecordPtr = ^CKRecord;
  CKRecordValueProtocolPtr = ^CKRecordValueProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CKRecordTypeUserRecord: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  CKRecordValueProtocol = objcprotocol external name 'CKRecordValue' (NSObjectProtocol)
  end;
{$endif}

{$ifdef CLASSES}

type
  CKRecord = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  public
    function init: instancetype; message 'init';
    function initWithRecordType (recordType: NSString): instancetype; message 'initWithRecordType:';
    function initWithRecordType_recordID (recordType: NSString; recordID: CKRecordID): instancetype; message 'initWithRecordType:recordID:';
    function initWithRecordType_zoneID (recordType: NSString; zoneID: CKRecordZoneID): instancetype; message 'initWithRecordType:zoneID:';
    function recordType: NSString; message 'recordType';
    function recordID: CKRecordID; message 'recordID';
    function recordChangeTag: NSString; message 'recordChangeTag';
    function creatorUserRecordID: CKRecordID; message 'creatorUserRecordID';
    function creationDate: NSDate; message 'creationDate';
    function lastModifiedUserRecordID: CKRecordID; message 'lastModifiedUserRecordID';
    function modificationDate: NSDate; message 'modificationDate';
    function objectForKey (key: NSString): id; message 'objectForKey:';
    procedure setObject_forKey (object_: CKRecordValueProtocol; key: NSString); message 'setObject:forKey:';
    function allKeys: NSArray; message 'allKeys';
    function allTokens: NSArray; message 'allTokens';
    function objectForKeyedSubscript (key: NSString): id; message 'objectForKeyedSubscript:';
    procedure setObject_forKeyedSubscript (object_: CKRecordValueProtocol; key: NSString); message 'setObject:forKeyedSubscript:';
    function changedKeys: NSArray; message 'changedKeys';
    procedure encodeSystemFieldsWithCoder (coder: NSCoder); message 'encodeSystemFieldsWithCoder:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSString_CKRecordValue = objccategory external name 'CKRecordValue' (NSString)
  end;

type
  NSNumber_CKRecordValue = objccategory external name 'CKRecordValue' (NSNumber)
  end;

type
  NSArray_CKRecordValue = objccategory external name 'CKRecordValue' (NSArray)
  end;

type
  NSDate_CKRecordValue = objccategory external name 'CKRecordValue' (NSDate)
  end;

type
  NSData_CKRecordValue = objccategory external name 'CKRecordValue' (NSData)
  end;

type
  CKReference_CKRecordValue = objccategory external name 'CKRecordValue' (CKReference)
  end;

type
  CKAsset_CKRecordValue = objccategory external name 'CKRecordValue' (CKAsset)
  end;

type
  CKRecordValue = objccategory external (CLLocation)
  end;
{$endif}


{ Parsed from CloudKit.framework CKQuery.h }


{$ifdef TYPES}
type
  CKQueryPtr = ^CKQuery;
{$endif}

{$ifdef CLASSES}

type
  CKQuery = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  public
    function init: instancetype; message 'init';
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function initWithRecordType_predicate (recordType: NSString; predicate: NSPredicate): instancetype; message 'initWithRecordType:predicate:'; { NS_DESIGNATED_INITIALIZER }
    function recordType: NSString; message 'recordType';
    function predicate: NSPredicate; message 'predicate';
    procedure setSortDescriptors(newValue: NSArray); message 'setSortDescriptors:';
    function sortDescriptors: NSArray; message 'sortDescriptors';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}


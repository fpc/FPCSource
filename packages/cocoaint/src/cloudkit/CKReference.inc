{ Parsed from CloudKit.framework CKReference.h }


{$ifdef TYPES}
type
  CKReferencePtr = ^CKReference;
{$endif}

{$ifdef TYPES}
type
  CKReferenceAction = NSUInteger;
  CKReferenceActionPtr = ^CKReferenceAction;

const
  CKReferenceActionNone = 0;
  CKReferenceActionDeleteSelf = 1;
{$endif}

{$ifdef CLASSES}

type
  CKReference = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  public
    function init: instancetype; message 'init';
    function initWithRecordID_action (recordID: CKRecordID; action: CKReferenceAction): instancetype; message 'initWithRecordID:action:'; { NS_DESIGNATED_INITIALIZER }
    function initWithRecord_action (record_: CKRecord; action: CKReferenceAction): instancetype; message 'initWithRecord:action:';
    function referenceAction: CKReferenceAction; message 'referenceAction';
    function recordID: CKRecordID; message 'recordID';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}


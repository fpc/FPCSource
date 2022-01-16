{ Parsed from Foundation.framework NSAppleEventDescriptor.h }


{$ifdef TYPES}
type
  NSAppleEventDescriptorPtr = ^NSAppleEventDescriptor;
{$endif}

{$ifdef CLASSES}

type
  NSAppleEventDescriptor = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _desc: AEDesc;
    _hasValidDesc: ObjCBOOL;
    _padding:array[0..2] of char;
  public
    class function nullDescriptor: NSAppleEventDescriptor; message 'nullDescriptor';
    class function descriptorWithDescriptorType_bytes_length (descriptorType: DescType; bytes: pointer; byteCount: NSUInteger): NSAppleEventDescriptor; message 'descriptorWithDescriptorType:bytes:length:';
    class function descriptorWithDescriptorType_data (descriptorType: DescType; data: NSData): NSAppleEventDescriptor; message 'descriptorWithDescriptorType:data:';
    class function descriptorWithBoolean (ObjCBOOL_: Boolean): NSAppleEventDescriptor; message 'descriptorWithBoolean:';
    class function descriptorWithEnumCode (enumerator: OSType): NSAppleEventDescriptor; message 'descriptorWithEnumCode:';
    class function descriptorWithInt32 (signedInt: SInt32): NSAppleEventDescriptor; message 'descriptorWithInt32:';
    class function descriptorWithTypeCode (typeCode: OSType): NSAppleEventDescriptor; message 'descriptorWithTypeCode:';
    class function descriptorWithString (string_: NSString): NSAppleEventDescriptor; message 'descriptorWithString:';
    class function appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionID (eventClass: AEEventClass; eventID: AEEventID; targetDescriptor: NSAppleEventDescriptor; returnID: AEReturnID; transactionID: AETransactionID): NSAppleEventDescriptor; message 'appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:';
    class function listDescriptor: NSAppleEventDescriptor; message 'listDescriptor';
    class function recordDescriptor: NSAppleEventDescriptor; message 'recordDescriptor';
    function initWithAEDescNoCopy (aeDesc: AEDescPtr): instancetype; message 'initWithAEDescNoCopy:'; { NS_DESIGNATED_INITIALIZER }
    function initWithDescriptorType_bytes_length (descriptorType: DescType; bytes: pointer; byteCount: NSUInteger): instancetype; message 'initWithDescriptorType:bytes:length:';
    function initWithDescriptorType_data (descriptorType: DescType; data: NSData): instancetype; message 'initWithDescriptorType:data:';
    function initWithEventClass_eventID_targetDescriptor_returnID_transactionID (eventClass: AEEventClass; eventID: AEEventID; targetDescriptor: NSAppleEventDescriptor; returnID: AEReturnID; transactionID: AETransactionID): instancetype; message 'initWithEventClass:eventID:targetDescriptor:returnID:transactionID:';
    function initListDescriptor: instancetype; message 'initListDescriptor';
    function initRecordDescriptor: instancetype; message 'initRecordDescriptor';
    function aeDesc: AEDescPtr; message 'aeDesc';
    function descriptorType: DescType; message 'descriptorType';
    function data: NSData; message 'data';
    function ObjCBOOLValue: Boolean; message 'ObjCBOOLValue';
    function enumCodeValue: OSType; message 'enumCodeValue';
    function int32Value: SInt32; message 'int32Value';
    function typeCodeValue: OSType; message 'typeCodeValue';
    function stringValue: NSString; message 'stringValue';
    function eventClass: AEEventClass; message 'eventClass';
    function eventID: AEEventID; message 'eventID';
    function returnID: AEReturnID; message 'returnID';
    function transactionID: AETransactionID; message 'transactionID';
    procedure setParamDescriptor_forKeyword (descriptor: NSAppleEventDescriptor; keyword: AEKeyword); message 'setParamDescriptor:forKeyword:';
    function paramDescriptorForKeyword (keyword: AEKeyword): NSAppleEventDescriptor; message 'paramDescriptorForKeyword:';
    procedure removeParamDescriptorWithKeyword (keyword: AEKeyword); message 'removeParamDescriptorWithKeyword:';
    procedure setAttributeDescriptor_forKeyword (descriptor: NSAppleEventDescriptor; keyword: AEKeyword); message 'setAttributeDescriptor:forKeyword:';
    function attributeDescriptorForKeyword (keyword: AEKeyword): NSAppleEventDescriptor; message 'attributeDescriptorForKeyword:';
    function numberOfItems: NSInteger; message 'numberOfItems';
    procedure insertDescriptor_atIndex (descriptor: NSAppleEventDescriptor; index: NSInteger); message 'insertDescriptor:atIndex:';
    function descriptorAtIndex (index: NSInteger): NSAppleEventDescriptor; message 'descriptorAtIndex:';
    procedure removeDescriptorAtIndex (index: NSInteger); message 'removeDescriptorAtIndex:';
    procedure setDescriptor_forKeyword (descriptor: NSAppleEventDescriptor; keyword: AEKeyword); message 'setDescriptor:forKeyword:';
    function descriptorForKeyword (keyword: AEKeyword): NSAppleEventDescriptor; message 'descriptorForKeyword:';
    procedure removeDescriptorWithKeyword (keyword: AEKeyword); message 'removeDescriptorWithKeyword:';
    function keywordForDescriptorAtIndex (index: NSInteger): AEKeyword; message 'keywordForDescriptorAtIndex:';
    function coerceToDescriptorType (descriptorType_: DescType): NSAppleEventDescriptor; message 'coerceToDescriptorType:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}


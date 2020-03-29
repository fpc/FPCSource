{ Parsed from Foundation.framework NSPropertyList.h }


{$ifdef TYPES}
type
  NSPropertyListSerializationPtr = ^NSPropertyListSerialization;
{$endif}

{$ifdef TYPES}
type
  NSPropertyListMutabilityOptions = NSUInteger;
  NSPropertyListMutabilityOptionsPtr = ^NSPropertyListMutabilityOptions;

const
  NSPropertyListImmutable = kCFPropertyListImmutable;
  NSPropertyListMutableContainers = kCFPropertyListMutableContainers;
  NSPropertyListMutableContainersAndLeaves = kCFPropertyListMutableContainersAndLeaves;

type
  NSPropertyListFormat = NSUInteger;
  NSPropertyListFormatPtr = ^NSPropertyListFormat;

const
  NSPropertyListOpenStepFormat = kCFPropertyListOpenStepFormat;
  NSPropertyListXMLFormat_v1_0 = kCFPropertyListXMLFormat_v1_0;
  NSPropertyListBinaryFormat_v1_0 = kCFPropertyListBinaryFormat_v1_0;

type
  NSPropertyListReadOptions = NSUInteger;
  NSPropertyListReadOptionsPtr = ^NSPropertyListReadOptions;
  NSPropertyListWriteOptions = NSUInteger;
  NSPropertyListWriteOptionsPtr = ^NSPropertyListWriteOptions;
{$endif}

{$ifdef CLASSES}

type
  NSPropertyListSerialization = objcclass external (NSObject)
  private
    reserved:array[0..5] of pointer;
  public
    class function propertyList_isValidForFormat (plist: id; format: NSPropertyListFormat): ObjCBOOL; message 'propertyList:isValidForFormat:';
    class function dataWithPropertyList_format_options_error (plist: id; format: NSPropertyListFormat; opt: NSPropertyListWriteOptions; error: NSErrorPtr): NSData; message 'dataWithPropertyList:format:options:error:'; { available in 10_6, 4_0 }
    class function writePropertyList_toStream_format_options_error (plist: id; stream: NSOutputStream; format: NSPropertyListFormat; opt: NSPropertyListWriteOptions; error: NSErrorPtr): NSInteger; message 'writePropertyList:toStream:format:options:error:'; { available in 10_6, 4_0 }
    class function propertyListWithData_options_format_error (data: NSData; opt: NSPropertyListReadOptions; format: NSPropertyListFormatPtr; error: NSErrorPtr): id; message 'propertyListWithData:options:format:error:'; { available in 10_6, 4_0 }
    class function propertyListWithStream_options_format_error (stream: NSInputStream; opt: NSPropertyListReadOptions; format: NSPropertyListFormatPtr; error: NSErrorPtr): id; message 'propertyListWithStream:options:format:error:'; { available in 10_6, 4_0 }
    class function dataFromPropertyList_format_errorDescription (plist: id; format: NSPropertyListFormat; errorString: NSStringPtr): NSData; message 'dataFromPropertyList:format:errorDescription:'; deprecated 'in 10_0, 10_10, 2_0, 8_0, "Use dataWithPropertyList:format:options:error: instead."';
    class function propertyListFromData_mutabilityOption_format_errorDescription (data: NSData; opt: NSPropertyListMutabilityOptions; format: NSPropertyListFormatPtr; errorString: NSStringPtr): id; message 'propertyListFromData:mutabilityOption:format:errorDescription:'; deprecated 'in 10_0, 10_10, 2_0, 8_0, "Use propertyListWithData:options:format:error: instead."';
  end;
{$endif}


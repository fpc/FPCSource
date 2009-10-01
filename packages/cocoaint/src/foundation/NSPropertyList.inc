{ Parsed from Foundation.framework NSPropertyList.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPROPERTYLIST_PAS_T}
{$define NSPROPERTYLIST_PAS_T}

{ Constants }

const
  NSPropertyListImmutable = kCFPropertyListImmutable;
  NSPropertyListMutableContainers = kCFPropertyListMutableContainers;
  NSPropertyListMutableContainersAndLeaves = kCFPropertyListMutableContainersAndLeaves;

const
  NSPropertyListOpenStepFormat = kCFPropertyListOpenStepFormat;

{ Types }
type
  NSPropertyListMutabilityOptions = NSUInteger;
  NSPropertyListFormat = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPROPERTYLIST_PAS_R}
{$define NSPROPERTYLIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPROPERTYLIST_PAS_F}
{$define NSPROPERTYLIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPROPERTYLIST_PAS_S}
{$define NSPROPERTYLIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPropertyListSerialization = objcclass;
  NSPropertyListSerializationPointer = ^NSPropertyListSerialization;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROPERTYLIST_PAS_C}
{$define NSPROPERTYLIST_PAS_C}

{ NSPropertyListSerialization }
  NSPropertyListSerialization = objcclass(NSObject)
  private
        reserved: array[0..5] of Pointer;
    
  public
    class function alloc: NSPropertyListSerialization; message 'alloc';

    class function propertyList_isValidForFormat(plist: id; format: NSPropertyListFormat): Boolean; message 'propertyList:isValidForFormat:';
    class function dataFromPropertyList_format_errorDescription(plist: id; format: NSPropertyListFormat; errorString: NSStringPointer): NSData; message 'dataFromPropertyList:format:errorDescription:';
    class function propertyListFromData_mutabilityOption_format_errorDescription(data: NSData; opt: NSPropertyListMutabilityOptions; format: NSPropertyListFormatPointer; errorString: NSStringPointer): id; message 'propertyListFromData:mutabilityOption:format:errorDescription:';
  end; external;

{$endif}
{$endif}

{ Parsed from Foundation.framework NSPropertyList.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPROPERTYLIST_PAS_H}
{$define NSPROPERTYLIST_PAS_H}
type
  NSPropertyListSerializationPointer = Pointer;

{$endif}
{$endif}

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
  NSPropertyListMutabilityOptions = culong;
  NSPropertyListFormat = culong;

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
{$ifndef NSPROPERTYLIST_PAS_T}
{$define NSPROPERTYLIST_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPropertyListSerialization = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROPERTYLIST_PAS_C}
{$define NSPROPERTYLIST_PAS_C}

{ NSPropertyListSerialization }
  NSPropertyListSerialization = objcclass(NSObject)
  private
    _reserved: Pointer;
    
  public
    class function alloc: NSPropertyListSerialization; message 'alloc';

    class function propertyList_isValidForFormat(plist: id; format: NSPropertyListFormat): Boolean; message 'propertyList:isValidForFormat:';
    class function dataFromPropertyList_format_errorDescription(plist: id; format: NSPropertyListFormat; var errorString: NSString): NSData; message 'dataFromPropertyList:format:errorDescription:';
    class function propertyListFromData_mutabilityOption_format_errorDescription(data: NSData; opt: NSPropertyListMutabilityOptions; var format: NSPropertyListFormat; var errorString: NSString): id; message 'propertyListFromData:mutabilityOption:format:errorDescription:';
  end; external;

{$endif}
{$endif}

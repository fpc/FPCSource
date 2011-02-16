{ Parsed from Appkit.framework NSPasteboardItem.h }

{$ifdef TYPES}
{$ifndef NSPASTEBOARDITEM_PAS_T}
{$define NSPASTEBOARDITEM_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPASTEBOARDITEM_PAS_R}
{$define NSPASTEBOARDITEM_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPASTEBOARDITEM_PAS_F}
{$define NSPASTEBOARDITEM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPASTEBOARDITEM_PAS_S}
{$define NSPASTEBOARDITEM_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPasteboardItemDataProviderProtocol = objcprotocol;
  NSPasteboardItem = objcclass;
  NSPasteboardItemPointer = ^NSPasteboardItem;
  NSPasteboardItemPtr = NSPasteboardItemPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPASTEBOARDITEM_PAS_C}
{$define NSPASTEBOARDITEM_PAS_C}

{ NSPasteboardItem }
  NSPasteboardItem = objcclass external (NSObject, NSPasteboardWritingProtocol, NSPasteboardReadingProtocol)
  private
    _pasteboard: NSPasteboard;
    _index: NSUInteger;
    _gen: NSInteger;
    _auxObject: id;
    _reserved: Pointer;
    
  public
    function types: NSArray; message 'types';
    function availableTypeFromArray(types_: NSArray): NSString; message 'availableTypeFromArray:';
    function setDataProvider_forTypes(dataProvider: NSPasteboardItemDataProviderProtocol; types_: NSArray): Boolean; message 'setDataProvider:forTypes:';
    function setData_forType(data: NSData; type_: NSString): Boolean; message 'setData:forType:';
    function setString_forType(string_: NSString; type_: NSString): Boolean; message 'setString:forType:';
    function setPropertyList_forType(propertyList: id; type_: NSString): Boolean; message 'setPropertyList:forType:';
    function dataForType(type_: NSString): NSData; message 'dataForType:';
    function stringForType(type_: NSString): NSString; message 'stringForType:';
    function propertyListForType(type_: NSString): id; message 'propertyListForType:';

    { Adopted Protocols }
    function writableTypesForPasteboard(pasteboard: NSPasteboard): NSArray;
    function writingOptionsForType_pasteboard(type_: NSString; pasteboard: NSPasteboard): NSPasteboardWritingOptions;
    function pasteboardPropertyListForType(type_: NSString): id;
    class function readableTypesForPasteboard(pasteboard: NSPasteboard): NSArray;
    class function readingOptionsForType_pasteboard(type_: NSString; pasteboard: NSPasteboard): NSPasteboardReadingOptions;
    function initWithPasteboardPropertyList_ofType(propertyList: id; type_: NSString): id;
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSPASTEBOARDITEM_PAS_P}
{$define NSPASTEBOARDITEM_PAS_P}
  
{ NSPasteboardItemDataProvider Protocol }
  NSPasteboardItemDataProviderProtocol = objcprotocol external name 'NSPasteboardItemDataProvider'
  required
    procedure pasteboard_item_provideDataForType(pasteboard: NSPasteboard; item: NSPasteboardItem; type_: NSString); message 'pasteboard:item:provideDataForType:';
  optional
    procedure pasteboardFinishedWithDataProvider(pasteboard: NSPasteboard); message 'pasteboardFinishedWithDataProvider:';
  end;
{$endif}
{$endif}

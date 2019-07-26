{ Parsed from AppKit.framework NSPasteboardItem.h }


{$ifdef TYPES}
type
  NSPasteboardItemPtr = ^NSPasteboardItem;
  NSPasteboardItemDataProviderProtocolPtr = ^NSPasteboardItemDataProviderProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSPasteboardItem = objcclass external (NSObject, NSPasteboardWritingProtocol, NSPasteboardReadingProtocol)
  private
    _pasteboard: NSPasteboard;
    _index: NSUInteger;
    _gen: NSInteger;
    _auxObject: id;
    _reserved: pointer;
  public
    function types: NSArray; message 'types';
    function availableTypeFromArray (types_: NSArray): NSString; message 'availableTypeFromArray:';
    function setDataProvider_forTypes (dataProvider: NSPasteboardItemDataProviderProtocol; types_: NSArray): ObjCBOOL; message 'setDataProvider:forTypes:';
    function setData_forType (data: NSData; type_: NSString): ObjCBOOL; message 'setData:forType:';
    function setString_forType (string_: NSString; type_: NSString): ObjCBOOL; message 'setString:forType:';
    function setPropertyList_forType (propertyList: id; type_: NSString): ObjCBOOL; message 'setPropertyList:forType:';
    function dataForType (type_: NSString): NSData; message 'dataForType:';
    function stringForType (type_: NSString): NSString; message 'stringForType:';
    function propertyListForType (type_: NSString): id; message 'propertyListForType:';

    { Adopted protocols }
    function initWithPasteboardPropertyList_ofType (propertyList: id; type_: NSString): id; message 'initWithPasteboardPropertyList:ofType:';
    function pasteboardPropertyListForType (type_: NSString): id; message 'pasteboardPropertyListForType:';
    class function readableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'readableTypesForPasteboard:';
    class function readingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardReadingOptions; message 'readingOptionsForType:pasteboard:';
    function writableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'writableTypesForPasteboard:';
    function writingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardWritingOptions; message 'writingOptionsForType:pasteboard:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPasteboardItemDataProviderProtocol = objcprotocol external name 'NSPasteboardItemDataProvider' (NSObjectProtocol)
  required
    procedure pasteboard_item_provideDataForType (pasteboard: NSPasteboard; item: NSPasteboardItem; type_: NSString); message 'pasteboard:item:provideDataForType:';
  optional
    procedure pasteboardFinishedWithDataProvider (pasteboard: NSPasteboard); message 'pasteboardFinishedWithDataProvider:';
  end;
{$endif}


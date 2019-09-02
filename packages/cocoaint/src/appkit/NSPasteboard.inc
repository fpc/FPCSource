{ Parsed from AppKit.framework NSPasteboard.h }


{$ifdef TYPES}
type
  NSPasteboardPtr = ^NSPasteboard;
  NSPasteboardWritingProtocolPtr = ^NSPasteboardWritingProtocol;
  NSPasteboardReadingProtocolPtr = ^NSPasteboardReadingProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPasteboardTypeString: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypePDF: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeTIFF: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypePNG: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeRTF: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeRTFD: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeHTML: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeTabularText: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeFont: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeRuler: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeColor: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeSound: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeMultipleTextSelection: NSString { available in 10_6 }; cvar; external;
  NSPasteboardTypeTextFinderOptions: NSString { available in 10_7 }; cvar; external;
  NSGeneralPboard: NSString; cvar; external;
  NSFontPboard: NSString; cvar; external;
  NSRulerPboard: NSString; cvar; external;
  NSFindPboard: NSString; cvar; external;
  NSDragPboard: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSPasteboard = objcclass external (NSObject)
  private
    _pboard: id;
    _gen: cint;
    _owners: id;
    _cachedTypeNameChangeCount: CFIndex;
    _cachedTypeNames: id;
    _promiseTypeNamesByIdentifier: NSMutableDictionary;
    _support: id;
    _pasteboardItems: id;
    _reserved:array[0..2] of pointer;
  public
    class function generalPasteboard: NSPasteboard; message 'generalPasteboard';
    class function pasteboardWithName (name: NSString): NSPasteboard; message 'pasteboardWithName:';
    class function pasteboardWithUniqueName: NSPasteboard; message 'pasteboardWithUniqueName';
    function name: NSString; message 'name';
    function changeCount: NSInteger; message 'changeCount';
    procedure releaseGlobally; message 'releaseGlobally';
    function clearContents: NSInteger; message 'clearContents'; { available in 10_6 }
    function writeObjects (objects: NSArray): ObjCBOOL; message 'writeObjects:'; { available in 10_6 }
    function readObjectsForClasses_options (classArray: NSArray; options: NSDictionary): NSArray; message 'readObjectsForClasses:options:'; { available in 10_6 }
    function pasteboardItems: NSArray; message 'pasteboardItems';
    function indexOfPasteboardItem (pasteboardItem: NSPasteboardItem): NSUInteger; message 'indexOfPasteboardItem:'; { available in 10_6 }
    function canReadItemWithDataConformingToTypes (types: NSArray): ObjCBOOL; message 'canReadItemWithDataConformingToTypes:'; { available in 10_6 }
    function canReadObjectForClasses_options (classArray: NSArray; options: NSDictionary): ObjCBOOL; message 'canReadObjectForClasses:options:'; { available in 10_6 }
    function declareTypes_owner (newTypes: NSArray; newOwner: id): NSInteger; message 'declareTypes:owner:';
    function addTypes_owner (newTypes: NSArray; newOwner: id): NSInteger; message 'addTypes:owner:';
    function types: NSArray; message 'types';
    function availableTypeFromArray (types_: NSArray): NSString; message 'availableTypeFromArray:';
    function setData_forType (data: NSData; dataType: NSString): ObjCBOOL; message 'setData:forType:';
    function setPropertyList_forType (plist: id; dataType: NSString): ObjCBOOL; message 'setPropertyList:forType:';
    function setString_forType (string_: NSString; dataType: NSString): ObjCBOOL; message 'setString:forType:';
    function dataForType (dataType: NSString): NSData; message 'dataForType:';
    function propertyListForType (dataType: NSString): id; message 'propertyListForType:';
    function stringForType (dataType: NSString): NSString; message 'stringForType:';
  end;


type
  FilterServices = objccategory external (NSPasteboard)
    class function typesFilterableTo (type_: NSString): NSArray; message 'typesFilterableTo:';
    class function pasteboardByFilteringFile (filename: NSString): NSPasteboard; message 'pasteboardByFilteringFile:';
    class function pasteboardByFilteringData_ofType (data: NSData; type_: NSString): NSPasteboard; message 'pasteboardByFilteringData:ofType:';
    class function pasteboardByFilteringTypesInPasteboard (pboard: NSPasteboard): NSPasteboard; message 'pasteboardByFilteringTypesInPasteboard:';
  end;

type
  NSPasteboardOwner = objccategory external (NSObject)
    procedure pasteboard_provideDataForType (sender: NSPasteboard; type_: NSString); message 'pasteboard:provideDataForType:';
    procedure pasteboardChangedOwner (sender: NSPasteboard); message 'pasteboardChangedOwner:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPasteboardURLReadingFileURLsOnlyKey: NSString { available in 10_6 }; cvar; external;
  NSPasteboardURLReadingContentsConformToTypesKey: NSString { available in 10_6 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSPasteboardWritingOptions = NSUInteger;
  NSPasteboardWritingOptionsPtr = ^NSPasteboardWritingOptions;

const
  NSPasteboardWritingPromised = 1 shl 9;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPasteboardWritingProtocol = objcprotocol external name 'NSPasteboardWriting' (NSObjectProtocol)
  required
    function writableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'writableTypesForPasteboard:';
  optional
    function writingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardWritingOptions; message 'writingOptionsForType:pasteboard:';
  required
    function pasteboardPropertyListForType (type_: NSString): id; message 'pasteboardPropertyListForType:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSPasteboardReadingOptions = NSUInteger;
  NSPasteboardReadingOptionsPtr = ^NSPasteboardReadingOptions;

const
  NSPasteboardReadingAsData = 0;
  NSPasteboardReadingAsString = 1 shl 0;
  NSPasteboardReadingAsPropertyList = 1 shl 1;
  NSPasteboardReadingAsKeyedArchive = 1 shl 2;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPasteboardReadingProtocol = objcprotocol external name 'NSPasteboardReading' (NSObjectProtocol)
  required
    class function readableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'readableTypesForPasteboard:';
  optional
    class function readingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardReadingOptions; message 'readingOptionsForType:pasteboard:';
  optional
    function initWithPasteboardPropertyList_ofType (propertyList: id; type_: NSString): id; message 'initWithPasteboardPropertyList:ofType:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSURL_NSPasteboardSupport = objccategory external name 'NSPasteboardSupport' (NSURL)
    class function URLFromPasteboard (pasteBoard: NSPasteboard): NSURL; message 'URLFromPasteboard:';
    procedure writeToPasteboard (pasteBoard: NSPasteboard); message 'writeToPasteboard:';
  end;

type
  NSPasteboardSupport = objccategory external (NSString)
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSStringPboardType: NSString; cvar; external;
  NSFilenamesPboardType: NSString; cvar; external;
  NSTIFFPboardType: NSString; cvar; external;
  NSRTFPboardType: NSString; cvar; external;
  NSTabularTextPboardType: NSString; cvar; external;
  NSFontPboardType: NSString; cvar; external;
  NSRulerPboardType: NSString; cvar; external;
  NSColorPboardType: NSString; cvar; external;
  NSRTFDPboardType: NSString; cvar; external;
  NSHTMLPboardType: NSString; cvar; external;
  NSURLPboardType: NSString; cvar; external;
  NSPDFPboardType: NSString; cvar; external;
  NSMultipleTextSelectionPboardType: NSString { available in 10_5 }; cvar; external;
  NSPostScriptPboardType: NSString; cvar; external;
  NSVCardPboardType: NSString; cvar; external;
  NSInkTextPboardType: NSString; cvar; external;
  NSFilesPromisePboardType: NSString; cvar; external;
  NSPasteboardTypeFindPanelSearchOptions: NSString { available in 10_6 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSFileContents = objccategory external (NSPasteboard)
    function writeFileContents (filename: NSString): ObjCBOOL; message 'writeFileContents:';
    function readFileContentsType_toFile (type_: NSString; filename: NSString): NSString; message 'readFileContentsType:toFile:';
    function writeFileWrapper (wrapper: NSFileWrapper): ObjCBOOL; message 'writeFileWrapper:';
    function readFileWrapper: NSFileWrapper; message 'readFileWrapper';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFileContentsPboardType: NSString; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function NSCreateFilenamePboardType(fileType: NSString): NSString; cdecl; external;
function NSCreateFileContentsPboardType(fileType: NSString): NSString; cdecl; external;
function NSGetFileType(pboardType: NSString): NSString; cdecl; external;
function NSGetFileTypes(pboardTypes: NSArray): NSArray; cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPICTPboardType: NSString deprecated 'in 10_0, 10_6'; cvar; external;
{$endif}


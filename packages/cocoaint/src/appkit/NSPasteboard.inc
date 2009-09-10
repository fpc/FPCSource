{ Parsed from Appkit.framework NSPasteboard.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPASTEBOARD_PAS_H}
{$define NSPASTEBOARD_PAS_H}
type
  NSPasteboardPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPASTEBOARD_PAS_T}
{$define NSPASTEBOARD_PAS_T}

{ CFString constants }
var
  NSStringPboardType: CFStringRef; external name '_NSStringPboardType';
  NSFilenamesPboardType: CFStringRef; external name '_NSFilenamesPboardType';
  NSPostScriptPboardType: CFStringRef; external name '_NSPostScriptPboardType';
  NSTIFFPboardType: CFStringRef; external name '_NSTIFFPboardType';
  NSRTFPboardType: CFStringRef; external name '_NSRTFPboardType';
  NSTabularTextPboardType: CFStringRef; external name '_NSTabularTextPboardType';
  NSFontPboardType: CFStringRef; external name '_NSFontPboardType';
  NSRulerPboardType: CFStringRef; external name '_NSRulerPboardType';
  NSFileContentsPboardType: CFStringRef; external name '_NSFileContentsPboardType';
  NSColorPboardType: CFStringRef; external name '_NSColorPboardType';
  NSRTFDPboardType: CFStringRef; external name '_NSRTFDPboardType';
  NSHTMLPboardType: CFStringRef; external name '_NSHTMLPboardType';
  NSPICTPboardType: CFStringRef; external name '_NSPICTPboardType';
  NSURLPboardType: CFStringRef; external name '_NSURLPboardType';
  NSPDFPboardType: CFStringRef; external name '_NSPDFPboardType';
  NSGeneralPboard: CFStringRef; external name '_NSGeneralPboard';
  NSFontPboard: CFStringRef; external name '_NSFontPboard';
  NSRulerPboard: CFStringRef; external name '_NSRulerPboard';
  NSFindPboard: CFStringRef; external name '_NSFindPboard';
  NSDragPboard: CFStringRef; external name '_NSDragPboard';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPASTEBOARD_PAS_R}
{$define NSPASTEBOARD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPASTEBOARD_PAS_F}
{$define NSPASTEBOARD_PAS_F}

{ Functions }
function NSCreateFilenamePboardType(var fileType: NSString): NSString; cdecl; external name 'NSCreateFilenamePboardType';
function NSCreateFileContentsPboardType(var fileType: NSString): NSString; cdecl; external name 'NSCreateFileContentsPboardType';
function NSGetFileType(var pboardType: NSString): NSString; cdecl; external name 'NSGetFileType';
function NSGetFileTypes(var pboardTypes: NSArray): NSArray; cdecl; external name 'NSGetFileTypes';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPASTEBOARD_PAS_T}
{$define NSPASTEBOARD_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPasteboard = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPASTEBOARD_PAS_C}
{$define NSPASTEBOARD_PAS_C}

{ NSPasteboard }
  NSPasteboard = objcclass(NSObject)
  private
    __pboard: id;
    __gen: cint;
    __owners: id;
    __cachedTypeNameChangeCount: CFIndex;
    __cachedTypeNames: NSArray;
    __promiseTypeNamesByIdentifier: NSMutableDictionary;
    __support: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSPasteboard; message 'alloc';

    class function generalPasteboard: NSPasteboard; message 'generalPasteboard';
    class function pasteboardWithName(name_: NSString): NSPasteboard; message 'pasteboardWithName:';
    class function pasteboardWithUniqueName: NSPasteboard; message 'pasteboardWithUniqueName';
    class function typesFilterableTo(type_: NSString): NSArray; message 'typesFilterableTo:';
    class function pasteboardByFilteringFile(filename: NSString): NSPasteboard; message 'pasteboardByFilteringFile:';
    class function pasteboardByFilteringData_ofType(data: NSData; type_: NSString): NSPasteboard; message 'pasteboardByFilteringData:ofType:';
    class function pasteboardByFilteringTypesInPasteboard(pboard: NSPasteboard): NSPasteboard; message 'pasteboardByFilteringTypesInPasteboard:';
    function name: NSString; message 'name';
    procedure releaseGlobally; message 'releaseGlobally';
    function declareTypes_owner(newTypes: NSArray; newOwner: id): clong; message 'declareTypes:owner:';
    function addTypes_owner(newTypes: NSArray; newOwner: id): clong; message 'addTypes:owner:';
    function changeCount: clong; message 'changeCount';
    function types: NSArray; message 'types';
    function availableTypeFromArray(types_: NSArray): NSString; message 'availableTypeFromArray:';
    function setData_forType(data: NSData; dataType: NSString): Boolean; message 'setData:forType:';
    function dataForType(dataType: NSString): NSData; message 'dataForType:';
    function setPropertyList_forType(plist: id; dataType: NSString): Boolean; message 'setPropertyList:forType:';
    function propertyListForType(dataType: NSString): id; message 'propertyListForType:';
    function setString_forType(string_: NSString; dataType: NSString): Boolean; message 'setString:forType:';
    function stringForType(dataType: NSString): NSString; message 'stringForType:';

    { Category: NSFileContents }
    function writeFileContents(filename: NSString): Boolean; message 'writeFileContents:';
    function readFileContentsType_toFile(type_: NSString; filename: NSString): NSString; message 'readFileContentsType:toFile:';
    function writeFileWrapper(wrapper: NSFileWrapper): Boolean; message 'writeFileWrapper:';
    function readFileWrapper: NSFileWrapper; message 'readFileWrapper';
  end; external;

{$endif}
{$endif}

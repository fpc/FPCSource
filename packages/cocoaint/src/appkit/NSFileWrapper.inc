{ Parsed from Appkit.framework NSFileWrapper.h }

{$ifdef TYPES}
{$ifndef NSFILEWRAPPER_PAS_T}
{$define NSFILEWRAPPER_PAS_T}

{ Constants }

const
  NSFileWrapperReadingImmediate = 1 shl 0;
  NSFileWrapperReadingWithoutMapping = 1 shl 1;

const
  NSFileWrapperWritingAtomic = 1 shl 0;
  NSFileWrapperWritingWithNameUpdating = 1 shl 1;

{ Types }
type
  NSFileWrapperReadingOptions = NSUInteger;
  NSFileWrapperReadingOptionsPtr = ^NSFileWrapperReadingOptions;
  NSFileWrapperWritingOptions = NSUInteger;
  NSFileWrapperWritingOptionsPtr = ^NSFileWrapperWritingOptions;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFILEWRAPPER_PAS_R}
{$define NSFILEWRAPPER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFILEWRAPPER_PAS_F}
{$define NSFILEWRAPPER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFILEWRAPPER_PAS_S}
{$define NSFILEWRAPPER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFileWrapper = objcclass;
  NSFileWrapperPointer = ^NSFileWrapper;
  NSFileWrapperPtr = NSFileWrapperPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFILEWRAPPER_PAS_C}
{$define NSFILEWRAPPER_PAS_C}

{ NSFileWrapper }
  NSFileWrapper = objcclass external (NSObject, NSCodingProtocol)
  private
    _fileAttributes: NSDictionary;
    _preferredFileName: NSString;
    _fileName: NSString;
    _contents: id;
    _icon: NSImage;
    _moreVars: id;
    
  public
    function initWithURL_options_error(url: NSURL; options: NSFileWrapperReadingOptions; outError: NSErrorPointer): id; message 'initWithURL:options:error:';
    function initDirectoryWithFileWrappers(childrenByPreferredName: NSDictionary): id; message 'initDirectoryWithFileWrappers:';
    function initRegularFileWithContents(contents: NSData): id; message 'initRegularFileWithContents:';
    function initSymbolicLinkWithDestinationURL(url: NSURL): id; message 'initSymbolicLinkWithDestinationURL:';
    function initWithSerializedRepresentation(serializeRepresentation: NSData): id; message 'initWithSerializedRepresentation:';
    function isDirectory: Boolean; message 'isDirectory';
    function isRegularFile: Boolean; message 'isRegularFile';
    function isSymbolicLink: Boolean; message 'isSymbolicLink';
    procedure setPreferredFilename(fileName_: NSString); message 'setPreferredFilename:';
    function preferredFilename: NSString; message 'preferredFilename';
    procedure setFilename(fileName_: NSString); message 'setFilename:';
    function filename: NSString; message 'filename';
    procedure setFileAttributes(fileAttributes_: NSDictionary); message 'setFileAttributes:';
    function fileAttributes: NSDictionary; message 'fileAttributes';
    procedure setIcon(icon_: NSImage); message 'setIcon:';
    function icon: NSImage; message 'icon';
    function matchesContentsOfURL(url: NSURL): Boolean; message 'matchesContentsOfURL:';
    function readFromURL_options_error(url: NSURL; options: NSFileWrapperReadingOptions; outError: NSErrorPointer): Boolean; message 'readFromURL:options:error:';
    function writeToURL_options_originalContentsURL_error(url: NSURL; options: NSFileWrapperWritingOptions; originalContentsURL: NSURL; outError: NSErrorPointer): Boolean; message 'writeToURL:options:originalContentsURL:error:';
    function serializedRepresentation: NSData; message 'serializedRepresentation';
    function addFileWrapper(child: NSFileWrapper): NSString; message 'addFileWrapper:';
    function addRegularFileWithContents_preferredFilename(data: NSData; fileName_: NSString): NSString; message 'addRegularFileWithContents:preferredFilename:';
    procedure removeFileWrapper(child: NSFileWrapper); message 'removeFileWrapper:';
    function fileWrappers: NSDictionary; message 'fileWrappers';
    function keyForFileWrapper(child: NSFileWrapper): NSString; message 'keyForFileWrapper:';
    function regularFileContents: NSData; message 'regularFileContents';
    function symbolicLinkDestinationURL: NSURL; message 'symbolicLinkDestinationURL';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSDeprecated_NSFileWrapperCategory }
  NSDeprecated_NSFileWrapperCategory = objccategory external name 'NSDeprecated' (NSFileWrapper)
    function initWithPath(path: NSString): id; message 'initWithPath:';
    function initSymbolicLinkWithDestination(path: NSString): id; message 'initSymbolicLinkWithDestination:';
    function needsToBeUpdatedFromPath(path: NSString): Boolean; message 'needsToBeUpdatedFromPath:';
    function updateFromPath(path: NSString): Boolean; message 'updateFromPath:';
    function writeToFile_atomically_updateFilenames(path: NSString; atomicFlag: Boolean; updateFilenamesFlag: Boolean): Boolean; message 'writeToFile:atomically:updateFilenames:';
    function addFileWithPath(path: NSString): NSString; message 'addFileWithPath:';
    function addSymbolicLinkWithDestination_preferredFilename(path: NSString; filename_: NSString): NSString; message 'addSymbolicLinkWithDestination:preferredFilename:';
    function symbolicLinkDestination: NSString; message 'symbolicLinkDestination';
  end;

{$endif}
{$endif}

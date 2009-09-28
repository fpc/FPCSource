{ Parsed from Appkit.framework NSFileWrapper.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFILEWRAPPER_PAS_H}
{$define NSFILEWRAPPER_PAS_H}
type
  NSFileWrapperPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSFILEWRAPPER_PAS_T}
{$define NSFILEWRAPPER_PAS_T}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSFILEWRAPPER_PAS_C}
{$define NSFILEWRAPPER_PAS_C}

{ NSFileWrapper }
  NSFileWrapper = objcclass(NSObject, NSCodingProtocol)
  private
    __impl: id;
    __fileName: NSString;
    __preferredFileName: NSString;
    __fileAttributes: NSDictionary;
    __image: NSImage;
    __moreVars: id;
    
  public
    class function alloc: NSFileWrapper; message 'alloc';

    function initDirectoryWithFileWrappers(docs: NSDictionary): id; message 'initDirectoryWithFileWrappers:';
    function initRegularFileWithContents(data: NSData): id; message 'initRegularFileWithContents:';
    function initSymbolicLinkWithDestination(path: NSString): id; message 'initSymbolicLinkWithDestination:';
    function initWithPath(path: NSString): id; message 'initWithPath:';
    function initWithSerializedRepresentation(data: NSData): id; message 'initWithSerializedRepresentation:';
    function writeToFile_atomically_updateFilenames(path: NSString; atomicFlag: Boolean; updateFilenamesFlag: Boolean): Boolean; message 'writeToFile:atomically:updateFilenames:';
    function serializedRepresentation: NSData; message 'serializedRepresentation';
    procedure setFilename(filename_: NSString); message 'setFilename:';
    function filename: NSString; message 'filename';
    procedure setPreferredFilename(filename_: NSString); message 'setPreferredFilename:';
    function preferredFilename: NSString; message 'preferredFilename';
    procedure setFileAttributes(attributes: NSDictionary); message 'setFileAttributes:';
    function fileAttributes: NSDictionary; message 'fileAttributes';
    function isRegularFile: Boolean; message 'isRegularFile';
    function isDirectory: Boolean; message 'isDirectory';
    function isSymbolicLink: Boolean; message 'isSymbolicLink';
    procedure setIcon(icon_: NSImage); message 'setIcon:';
    function icon: NSImage; message 'icon';
    function needsToBeUpdatedFromPath(path: NSString): Boolean; message 'needsToBeUpdatedFromPath:';
    function updateFromPath(path: NSString): Boolean; message 'updateFromPath:';
    function addFileWrapper(doc: NSFileWrapper): NSString; message 'addFileWrapper:';
    procedure removeFileWrapper(doc: NSFileWrapper); message 'removeFileWrapper:';
    function fileWrappers: NSDictionary; message 'fileWrappers';
    function keyForFileWrapper(doc: NSFileWrapper): NSString; message 'keyForFileWrapper:';
    function addFileWithPath(path: NSString): NSString; message 'addFileWithPath:';
    function addRegularFileWithContents_preferredFilename(data: NSData; filename_: NSString): NSString; message 'addRegularFileWithContents:preferredFilename:';
    function addSymbolicLinkWithDestination_preferredFilename(path: NSString; filename_: NSString): NSString; message 'addSymbolicLinkWithDestination:preferredFilename:';
    function regularFileContents: NSData; message 'regularFileContents';
    function symbolicLinkDestination: NSString; message 'symbolicLinkDestination';
  end; external;

{$endif}
{$endif}

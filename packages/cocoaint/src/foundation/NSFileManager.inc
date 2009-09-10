{ Parsed from Foundation.framework NSFileManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFILEMANAGER_PAS_H}
{$define NSFILEMANAGER_PAS_H}
type
  NSFileManagerPointer = Pointer;
  NSDirectoryEnumeratorPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSFILEMANAGER_PAS_T}
{$define NSFILEMANAGER_PAS_T}

{ Defines }
const
  NSFoundationVersionWithFileManagerResourceForkSupport = 412;

{ CFString constants }
var
  NSFileType: CFStringRef; external name '_NSFileType';
  NSFileSize: CFStringRef; external name '_NSFileSize';
  NSFileModificationDate: CFStringRef; external name '_NSFileModificationDate';
  NSFileReferenceCount: CFStringRef; external name '_NSFileReferenceCount';
  NSFileDeviceIdentifier: CFStringRef; external name '_NSFileDeviceIdentifier';
  NSFileOwnerAccountName: CFStringRef; external name '_NSFileOwnerAccountName';
  NSFileGroupOwnerAccountName: CFStringRef; external name '_NSFileGroupOwnerAccountName';
  NSFilePosixPermissions: CFStringRef; external name '_NSFilePosixPermissions';
  NSFileSystemNumber: CFStringRef; external name '_NSFileSystemNumber';
  NSFileSystemFileNumber: CFStringRef; external name '_NSFileSystemFileNumber';
  NSFileExtensionHidden: CFStringRef; external name '_NSFileExtensionHidden';
  NSFileHFSCreatorCode: CFStringRef; external name '_NSFileHFSCreatorCode';
  NSFileHFSTypeCode: CFStringRef; external name '_NSFileHFSTypeCode';
  NSFileImmutable: CFStringRef; external name '_NSFileImmutable';
  NSFileAppendOnly: CFStringRef; external name '_NSFileAppendOnly';
  NSFileCreationDate: CFStringRef; external name '_NSFileCreationDate';
  NSFileOwnerAccountID: CFStringRef; external name '_NSFileOwnerAccountID';
  NSFileGroupOwnerAccountID: CFStringRef; external name '_NSFileGroupOwnerAccountID';
  NSFileBusy: CFStringRef; external name '_NSFileBusy';
  NSFileSystemSize: CFStringRef; external name '_NSFileSystemSize';
  NSFileSystemFreeSize: CFStringRef; external name '_NSFileSystemFreeSize';
  NSFileSystemNodes: CFStringRef; external name '_NSFileSystemNodes';
  NSFileSystemFreeNodes: CFStringRef; external name '_NSFileSystemFreeNodes';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFILEMANAGER_PAS_R}
{$define NSFILEMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFILEMANAGER_PAS_F}
{$define NSFILEMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFILEMANAGER_PAS_T}
{$define NSFILEMANAGER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFileManager = objcclass;
  NSDirectoryEnumerator = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFILEMANAGER_PAS_C}
{$define NSFILEMANAGER_PAS_C}

{ NSFileManager }
  NSFileManager = objcclass(NSObject)
    
  public
    class function alloc: NSFileManager; message 'alloc';

    class function defaultManager: NSFileManager; message 'defaultManager';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function setAttributes_ofItemAtPath_error(attributes: NSDictionary; path: NSString; var error: NSError): Boolean; message 'setAttributes:ofItemAtPath:error:';
    function createDirectoryAtPath_withIntermediateDirectories_attributes_error(path: NSString; createIntermediates: Boolean; attributes: NSDictionary; var error: NSError): Boolean; message 'createDirectoryAtPath:withIntermediateDirectories:attributes:error:';
    function contentsOfDirectoryAtPath_error(path: NSString; var error: NSError): NSArray; message 'contentsOfDirectoryAtPath:error:';
    function subpathsOfDirectoryAtPath_error(path: NSString; var error: NSError): NSArray; message 'subpathsOfDirectoryAtPath:error:';
    function attributesOfItemAtPath_error(path: NSString; var error: NSError): NSDictionary; message 'attributesOfItemAtPath:error:';
    function attributesOfFileSystemForPath_error(path: NSString; var error: NSError): NSDictionary; message 'attributesOfFileSystemForPath:error:';
    function createSymbolicLinkAtPath_withDestinationPath_error(path: NSString; destPath: NSString; var error: NSError): Boolean; message 'createSymbolicLinkAtPath:withDestinationPath:error:';
    function destinationOfSymbolicLinkAtPath_error(path: NSString; var error: NSError): NSString; message 'destinationOfSymbolicLinkAtPath:error:';
    function copyItemAtPath_toPath_error(srcPath: NSString; dstPath: NSString; var error: NSError): Boolean; message 'copyItemAtPath:toPath:error:';
    function moveItemAtPath_toPath_error(srcPath: NSString; dstPath: NSString; var error: NSError): Boolean; message 'moveItemAtPath:toPath:error:';
    function linkItemAtPath_toPath_error(srcPath: NSString; dstPath: NSString; var error: NSError): Boolean; message 'linkItemAtPath:toPath:error:';
    function removeItemAtPath_error(path: NSString; var error: NSError): Boolean; message 'removeItemAtPath:error:';
    function fileAttributesAtPath_traverseLink(path: NSString; yorn: Boolean): NSDictionary; message 'fileAttributesAtPath:traverseLink:';
    function changeFileAttributes_atPath(attributes: NSDictionary; path: NSString): Boolean; message 'changeFileAttributes:atPath:';
    function directoryContentsAtPath(path: NSString): NSArray; message 'directoryContentsAtPath:';
    function fileSystemAttributesAtPath(path: NSString): NSDictionary; message 'fileSystemAttributesAtPath:';
    function pathContentOfSymbolicLinkAtPath(path: NSString): NSString; message 'pathContentOfSymbolicLinkAtPath:';
    function createSymbolicLinkAtPath_pathContent(path: NSString; otherpath: NSString): Boolean; message 'createSymbolicLinkAtPath:pathContent:';
    function createDirectoryAtPath_attributes(path: NSString; attributes: NSDictionary): Boolean; message 'createDirectoryAtPath:attributes:';
    function linkPath_toPath_handler(src: NSString; dest: NSString; handler: id): Boolean; message 'linkPath:toPath:handler:';
    function copyPath_toPath_handler(src: NSString; dest: NSString; handler: id): Boolean; message 'copyPath:toPath:handler:';
    function movePath_toPath_handler(src: NSString; dest: NSString; handler: id): Boolean; message 'movePath:toPath:handler:';
    function removeFileAtPath_handler(path: NSString; handler: id): Boolean; message 'removeFileAtPath:handler:';
    function currentDirectoryPath: NSString; message 'currentDirectoryPath';
    function changeCurrentDirectoryPath(path: NSString): Boolean; message 'changeCurrentDirectoryPath:';
    function fileExistsAtPath(path: NSString): Boolean; message 'fileExistsAtPath:';
    function fileExistsAtPath_isDirectory(path: NSString; var isDirectory: Boolean): Boolean; message 'fileExistsAtPath:isDirectory:';
    function isReadableFileAtPath(path: NSString): Boolean; message 'isReadableFileAtPath:';
    function isWritableFileAtPath(path: NSString): Boolean; message 'isWritableFileAtPath:';
    function isExecutableFileAtPath(path: NSString): Boolean; message 'isExecutableFileAtPath:';
    function isDeletableFileAtPath(path: NSString): Boolean; message 'isDeletableFileAtPath:';
    function contentsEqualAtPath_andPath(path: NSString; path1: NSString): Boolean; message 'contentsEqualAtPath:andPath:';
    function displayNameAtPath(path: NSString): NSString; message 'displayNameAtPath:';
    function componentsToDisplayForPath(path: NSString): NSArray; message 'componentsToDisplayForPath:';
    function enumeratorAtPath(path: NSString): NSDirectoryEnumerator; message 'enumeratorAtPath:';
    function subpathsAtPath(path: NSString): NSArray; message 'subpathsAtPath:';
    function contentsAtPath(path: NSString): NSData; message 'contentsAtPath:';
    function createFileAtPath_contents_attributes(path: NSString; data: NSData; attr: NSDictionary): Boolean; message 'createFileAtPath:contents:attributes:';
    function fileSystemRepresentationWithPath(path: NSString): char; message 'fileSystemRepresentationWithPath:';
    function stringWithFileSystemRepresentation_length(str: PChar; len: culong): NSString; message 'stringWithFileSystemRepresentation:length:';
  end; external;

{ NSDirectoryEnumerator }
  NSDirectoryEnumerator = objcclass(NSEnumerator)
    
  public
    class function alloc: NSDirectoryEnumerator; message 'alloc';

    function fileAttributes: NSDictionary; message 'fileAttributes';
    function directoryAttributes: NSDictionary; message 'directoryAttributes';
    procedure skipDescendents; message 'skipDescendents';
  end; external;

{$endif}
{$endif}

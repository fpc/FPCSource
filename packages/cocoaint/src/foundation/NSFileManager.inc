{ Parsed from Foundation.framework NSFileManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSFILEMANAGER_PAS_T}
{$define NSFILEMANAGER_PAS_T}

{ Defines }
const
  NSFoundationVersionWithFileManagerResourceForkSupport = 412;

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
{$ifndef NSFILEMANAGER_PAS_S}
{$define NSFILEMANAGER_PAS_S}

{ External string constants }
var
  NSFileType: NSString; external name '_NSFileType';
  NSFileTypeDirectory: NSString; external name '_NSFileTypeDirectory';
  NSFileTypeRegular: NSString; external name '_NSFileTypeRegular';
  NSFileTypeSymbolicLink: NSString; external name '_NSFileTypeSymbolicLink';
  NSFileTypeSocket: NSString; external name '_NSFileTypeSocket';
  NSFileTypeCharacterSpecial: NSString; external name '_NSFileTypeCharacterSpecial';
  NSFileTypeBlockSpecial: NSString; external name '_NSFileTypeBlockSpecial';
  NSFileTypeUnknown: NSString; external name '_NSFileTypeUnknown';
  NSFileSize: NSString; external name '_NSFileSize';
  NSFileModificationDate: NSString; external name '_NSFileModificationDate';
  NSFileReferenceCount: NSString; external name '_NSFileReferenceCount';
  NSFileDeviceIdentifier: NSString; external name '_NSFileDeviceIdentifier';
  NSFileOwnerAccountName: NSString; external name '_NSFileOwnerAccountName';
  NSFileGroupOwnerAccountName: NSString; external name '_NSFileGroupOwnerAccountName';
  NSFilePosixPermissions: NSString; external name '_NSFilePosixPermissions';
  NSFileSystemNumber: NSString; external name '_NSFileSystemNumber';
  NSFileSystemFileNumber: NSString; external name '_NSFileSystemFileNumber';
  NSFileExtensionHidden: NSString; external name '_NSFileExtensionHidden';
  NSFileHFSCreatorCode: NSString; external name '_NSFileHFSCreatorCode';
  NSFileHFSTypeCode: NSString; external name '_NSFileHFSTypeCode';
  NSFileImmutable: NSString; external name '_NSFileImmutable';
  NSFileAppendOnly: NSString; external name '_NSFileAppendOnly';
  NSFileCreationDate: NSString; external name '_NSFileCreationDate';
  NSFileOwnerAccountID: NSString; external name '_NSFileOwnerAccountID';
  NSFileGroupOwnerAccountID: NSString; external name '_NSFileGroupOwnerAccountID';
  NSFileBusy: NSString; external name '_NSFileBusy';
  NSFileSystemSize: NSString; external name '_NSFileSystemSize';
  NSFileSystemFreeSize: NSString; external name '_NSFileSystemFreeSize';
  NSFileSystemNodes: NSString; external name '_NSFileSystemNodes';
  NSFileSystemFreeNodes: NSString; external name '_NSFileSystemFreeNodes';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFileManager = objcclass;
  NSFileManagerPointer = ^NSFileManager;
  NSDirectoryEnumerator = objcclass;
  NSDirectoryEnumeratorPointer = ^NSDirectoryEnumerator;

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
    function setAttributes_ofItemAtPath_error(attributes: NSDictionary; path: NSString; error: NSErrorPointer): Boolean; message 'setAttributes:ofItemAtPath:error:';
    function createDirectoryAtPath_withIntermediateDirectories_attributes_error(path: NSString; createIntermediates: Boolean; attributes: NSDictionary; error: NSErrorPointer): Boolean; message 'createDirectoryAtPath:withIntermediateDirectories:attributes:error:';
    function contentsOfDirectoryAtPath_error(path: NSString; error: NSErrorPointer): NSArray; message 'contentsOfDirectoryAtPath:error:';
    function subpathsOfDirectoryAtPath_error(path: NSString; error: NSErrorPointer): NSArray; message 'subpathsOfDirectoryAtPath:error:';
    function attributesOfItemAtPath_error(path: NSString; error: NSErrorPointer): NSDictionary; message 'attributesOfItemAtPath:error:';
    function attributesOfFileSystemForPath_error(path: NSString; error: NSErrorPointer): NSDictionary; message 'attributesOfFileSystemForPath:error:';
    function createSymbolicLinkAtPath_withDestinationPath_error(path: NSString; destPath: NSString; error: NSErrorPointer): Boolean; message 'createSymbolicLinkAtPath:withDestinationPath:error:';
    function destinationOfSymbolicLinkAtPath_error(path: NSString; error: NSErrorPointer): NSString; message 'destinationOfSymbolicLinkAtPath:error:';
    function copyItemAtPath_toPath_error(srcPath: NSString; dstPath: NSString; error: NSErrorPointer): Boolean; message 'copyItemAtPath:toPath:error:';
    function moveItemAtPath_toPath_error(srcPath: NSString; dstPath: NSString; error: NSErrorPointer): Boolean; message 'moveItemAtPath:toPath:error:';
    function linkItemAtPath_toPath_error(srcPath: NSString; dstPath: NSString; error: NSErrorPointer): Boolean; message 'linkItemAtPath:toPath:error:';
    function removeItemAtPath_error(path: NSString; error: NSErrorPointer): Boolean; message 'removeItemAtPath:error:';
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
    function fileExistsAtPath_isDirectory(path: NSString; isDirectory: pboolean): Boolean; message 'fileExistsAtPath:isDirectory:';
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
    function fileSystemRepresentationWithPath(path: NSString): PChar; message 'fileSystemRepresentationWithPath:';
    function stringWithFileSystemRepresentation_length(str: PChar; len: NSUInteger): NSString; message 'stringWithFileSystemRepresentation:length:';
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

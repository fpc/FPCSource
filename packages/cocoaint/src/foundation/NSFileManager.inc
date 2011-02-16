{ Parsed from Foundation.framework NSFileManager.h }

{$ifdef TYPES}
{$ifndef NSFILEMANAGER_PAS_T}
{$define NSFILEMANAGER_PAS_T}

{ Defines }
const
  NSFoundationVersionWithFileManagerResourceForkSupport = 412;

{ Constants }

const
  NSVolumeEnumerationSkipHiddenVolumes = 1 shl 1;
  NSVolumeEnumerationProduceFileReferenceURLs = 1 shl 2;

const
  NSDirectoryEnumerationSkipsSubdirectoryDescendants = 1 shl 0;
  NSDirectoryEnumerationSkipsPackageDescendants = 1 shl 1;
  NSDirectoryEnumerationSkipsHiddenFiles = 1 shl 2;

const
  NSFileManagerItemReplacementUsingNewMetadataOnly = 1 shl 0;
  NSFileManagerItemReplacementWithoutDeletingBackupItem = 1 shl 1;

{ Types }
type
  NSVolumeEnumerationOptions = NSUInteger;
  NSVolumeEnumerationOptionsPtr = ^NSVolumeEnumerationOptions;
  NSDirectoryEnumerationOptions = NSUInteger;
  NSDirectoryEnumerationOptionsPtr = ^NSDirectoryEnumerationOptions;
  NSFileManagerItemReplacementOptions = NSUInteger;
  NSFileManagerItemReplacementOptionsPtr = ^NSFileManagerItemReplacementOptions;

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
  NSFileType: NSString; cvar; external;
  NSFileTypeDirectory: NSString; cvar; external;
  NSFileTypeRegular: NSString; cvar; external;
  NSFileTypeSymbolicLink: NSString; cvar; external;
  NSFileTypeSocket: NSString; cvar; external;
  NSFileTypeCharacterSpecial: NSString; cvar; external;
  NSFileTypeBlockSpecial: NSString; cvar; external;
  NSFileTypeUnknown: NSString; cvar; external;
  NSFileSize: NSString; cvar; external;
  NSFileModificationDate: NSString; cvar; external;
  NSFileReferenceCount: NSString; cvar; external;
  NSFileDeviceIdentifier: NSString; cvar; external;
  NSFileOwnerAccountName: NSString; cvar; external;
  NSFileGroupOwnerAccountName: NSString; cvar; external;
  NSFilePosixPermissions: NSString; cvar; external;
  NSFileSystemNumber: NSString; cvar; external;
  NSFileSystemFileNumber: NSString; cvar; external;
  NSFileExtensionHidden: NSString; cvar; external;
  NSFileHFSCreatorCode: NSString; cvar; external;
  NSFileHFSTypeCode: NSString; cvar; external;
  NSFileImmutable: NSString; cvar; external;
  NSFileAppendOnly: NSString; cvar; external;
  NSFileCreationDate: NSString; cvar; external;
  NSFileOwnerAccountID: NSString; cvar; external;
  NSFileGroupOwnerAccountID: NSString; cvar; external;
  NSFileBusy: NSString; cvar; external;
  NSFileSystemSize: NSString; cvar; external;
  NSFileSystemFreeSize: NSString; cvar; external;
  NSFileSystemNodes: NSString; cvar; external;
  NSFileSystemFreeNodes: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFileManager = objcclass;
  NSFileManagerPointer = ^NSFileManager;
  NSFileManagerPtr = NSFileManagerPointer;
  NSDirectoryEnumerator = objcclass;
  NSDirectoryEnumeratorPointer = ^NSDirectoryEnumerator;
  NSDirectoryEnumeratorPtr = NSDirectoryEnumeratorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFILEMANAGER_PAS_C}
{$define NSFILEMANAGER_PAS_C}

{ NSFileManager }
  NSFileManager = objcclass external (NSObject)
    
  public
    class function defaultManager: NSFileManager; message 'defaultManager';
    function mountedVolumeURLsIncludingResourceValuesForKeys_options(propertyKeys: NSArray; options: NSVolumeEnumerationOptions): NSArray; message 'mountedVolumeURLsIncludingResourceValuesForKeys:options:';
    function contentsOfDirectoryAtURL_includingPropertiesForKeys_options_error(url: NSURL; keys: NSArray; mask: NSDirectoryEnumerationOptions; error: NSErrorPointer): NSArray; message 'contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:';
    function URLsForDirectory_inDomains(directory: NSSearchPathDirectory; domainMask: NSSearchPathDomainMask): NSArray; message 'URLsForDirectory:inDomains:';
    function URLForDirectory_inDomain_appropriateForURL_create_error(directory: NSSearchPathDirectory; domain: NSSearchPathDomainMask; url: NSURL; shouldCreate: Boolean; error: NSErrorPointer): NSURL; message 'URLForDirectory:inDomain:appropriateForURL:create:error:';
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
    function copyItemAtURL_toURL_error(srcURL: NSURL; dstURL: NSURL; error: NSErrorPointer): Boolean; message 'copyItemAtURL:toURL:error:';
    function moveItemAtURL_toURL_error(srcURL: NSURL; dstURL: NSURL; error: NSErrorPointer): Boolean; message 'moveItemAtURL:toURL:error:';
    function linkItemAtURL_toURL_error(srcURL: NSURL; dstURL: NSURL; error: NSErrorPointer): Boolean; message 'linkItemAtURL:toURL:error:';
    function removeItemAtURL_error(URL: NSURL; error: NSErrorPointer): Boolean; message 'removeItemAtURL:error:';
    function fileAttributesAtPath_traverseLink(path: NSString; yorn: Boolean): NSDictionary; message 'fileAttributesAtPath:traverseLink:'; deprecated 'in Mac OS X 10.5 and later';
    function changeFileAttributes_atPath(attributes: NSDictionary; path: NSString): Boolean; message 'changeFileAttributes:atPath:'; deprecated 'in Mac OS X 10.5 and later';
    function directoryContentsAtPath(path: NSString): NSArray; message 'directoryContentsAtPath:'; deprecated 'in Mac OS X 10.5 and later';
    function fileSystemAttributesAtPath(path: NSString): NSDictionary; message 'fileSystemAttributesAtPath:'; deprecated 'in Mac OS X 10.5 and later';
    function pathContentOfSymbolicLinkAtPath(path: NSString): NSString; message 'pathContentOfSymbolicLinkAtPath:'; deprecated 'in Mac OS X 10.5 and later';
    function createSymbolicLinkAtPath_pathContent(path: NSString; otherpath: NSString): Boolean; message 'createSymbolicLinkAtPath:pathContent:'; deprecated 'in Mac OS X 10.5 and later';
    function createDirectoryAtPath_attributes(path: NSString; attributes: NSDictionary): Boolean; message 'createDirectoryAtPath:attributes:'; deprecated 'in Mac OS X 10.5 and later';
    function linkPath_toPath_handler(src: NSString; dest: NSString; handler: id): Boolean; message 'linkPath:toPath:handler:'; deprecated 'in Mac OS X 10.5 and later';
    function copyPath_toPath_handler(src: NSString; dest: NSString; handler: id): Boolean; message 'copyPath:toPath:handler:'; deprecated 'in Mac OS X 10.5 and later';
    function movePath_toPath_handler(src: NSString; dest: NSString; handler: id): Boolean; message 'movePath:toPath:handler:'; deprecated 'in Mac OS X 10.5 and later';
    function removeFileAtPath_handler(path: NSString; handler: id): Boolean; message 'removeFileAtPath:handler:'; deprecated 'in Mac OS X 10.5 and later';
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
    function replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_error(originalItemURL: NSURL; newItemURL: NSURL; backupItemName: NSString; options: NSFileManagerItemReplacementOptions; resultingURL: NSURLPointer; error: NSErrorPointer): Boolean; message 'replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:';
  end;

{ NSDirectoryEnumerator }
  NSDirectoryEnumerator = objcclass external (NSEnumerator)
    
  public
    function fileAttributes: NSDictionary; message 'fileAttributes';
    function directoryAttributes: NSDictionary; message 'directoryAttributes';
    procedure skipDescendents; message 'skipDescendents';
    function level: NSUInteger; message 'level';
    procedure skipDescendants; message 'skipDescendants';
  end;

{ NSCopyLinkMoveHandlerCategory }
  NSCopyLinkMoveHandlerCategory = objccategory external (NSObject)
    function fileManager_shouldProceedAfterError(fm: NSFileManager; errorInfo: NSDictionary): Boolean; message 'fileManager:shouldProceedAfterError:'; deprecated 'in Mac OS X 10.5 and later';
    procedure fileManager_willProcessPath(fm: NSFileManager; path: NSString); message 'fileManager:willProcessPath:'; deprecated 'in Mac OS X 10.5 and later';
  end;

{ NSFileManagerFileOperationAdditionsCategory }
  NSFileManagerFileOperationAdditionsCategory = objccategory external (NSObject)
    function fileManager_shouldCopyItemAtPath_toPath(fileManager: NSFileManager; srcPath: NSString; dstPath: NSString): Boolean; message 'fileManager:shouldCopyItemAtPath:toPath:';
    function fileManager_shouldCopyItemAtURL_toURL(fileManager: NSFileManager; srcURL: NSURL; dstURL: NSURL): Boolean; message 'fileManager:shouldCopyItemAtURL:toURL:';
    function fileManager_shouldProceedAfterError_copyingItemAtPath_toPath(fileManager: NSFileManager; error: NSError; srcPath: NSString; dstPath: NSString): Boolean; message 'fileManager:shouldProceedAfterError:copyingItemAtPath:toPath:';
    function fileManager_shouldProceedAfterError_copyingItemAtURL_toURL(fileManager: NSFileManager; error: NSError; srcURL: NSURL; dstURL: NSURL): Boolean; message 'fileManager:shouldProceedAfterError:copyingItemAtURL:toURL:';
    function fileManager_shouldMoveItemAtPath_toPath(fileManager: NSFileManager; srcPath: NSString; dstPath: NSString): Boolean; message 'fileManager:shouldMoveItemAtPath:toPath:';
    function fileManager_shouldMoveItemAtURL_toURL(fileManager: NSFileManager; srcURL: NSURL; dstURL: NSURL): Boolean; message 'fileManager:shouldMoveItemAtURL:toURL:';
    function fileManager_shouldProceedAfterError_movingItemAtPath_toPath(fileManager: NSFileManager; error: NSError; srcPath: NSString; dstPath: NSString): Boolean; message 'fileManager:shouldProceedAfterError:movingItemAtPath:toPath:';
    function fileManager_shouldProceedAfterError_movingItemAtURL_toURL(fileManager: NSFileManager; error: NSError; srcURL: NSURL; dstURL: NSURL): Boolean; message 'fileManager:shouldProceedAfterError:movingItemAtURL:toURL:';
    function fileManager_shouldLinkItemAtPath_toPath(fileManager: NSFileManager; srcPath: NSString; dstPath: NSString): Boolean; message 'fileManager:shouldLinkItemAtPath:toPath:';
    function fileManager_shouldLinkItemAtURL_toURL(fileManager: NSFileManager; srcURL: NSURL; dstURL: NSURL): Boolean; message 'fileManager:shouldLinkItemAtURL:toURL:';
    function fileManager_shouldProceedAfterError_linkingItemAtPath_toPath(fileManager: NSFileManager; error: NSError; srcPath: NSString; dstPath: NSString): Boolean; message 'fileManager:shouldProceedAfterError:linkingItemAtPath:toPath:';
    function fileManager_shouldProceedAfterError_linkingItemAtURL_toURL(fileManager: NSFileManager; error: NSError; srcURL: NSURL; dstURL: NSURL): Boolean; message 'fileManager:shouldProceedAfterError:linkingItemAtURL:toURL:';
    function fileManager_shouldRemoveItemAtPath(fileManager: NSFileManager; path: NSString): Boolean; message 'fileManager:shouldRemoveItemAtPath:';
    function fileManager_shouldRemoveItemAtURL(fileManager: NSFileManager; URL: NSURL): Boolean; message 'fileManager:shouldRemoveItemAtURL:';
    function fileManager_shouldProceedAfterError_removingItemAtPath(fileManager: NSFileManager; error: NSError; path: NSString): Boolean; message 'fileManager:shouldProceedAfterError:removingItemAtPath:';
    function fileManager_shouldProceedAfterError_removingItemAtURL(fileManager: NSFileManager; error: NSError; URL: NSURL): Boolean; message 'fileManager:shouldProceedAfterError:removingItemAtURL:';
  end;

{ NSFileAttributesCategory }
  NSFileAttributesCategory = objccategory external (NSDictionary)
    function fileSize: culonglong; message 'fileSize';
    function fileModificationDate: NSDate; message 'fileModificationDate';
    function fileType: NSString; message 'fileType';
    function filePosixPermissions: NSUInteger; message 'filePosixPermissions';
    function fileOwnerAccountName: NSString; message 'fileOwnerAccountName';
    function fileGroupOwnerAccountName: NSString; message 'fileGroupOwnerAccountName';
    function fileSystemNumber: NSInteger; message 'fileSystemNumber';
    function fileSystemFileNumber: NSUInteger; message 'fileSystemFileNumber';
    function fileExtensionHidden: Boolean; message 'fileExtensionHidden';
    function fileHFSCreatorCode: OSType; message 'fileHFSCreatorCode';
    function fileHFSTypeCode: OSType; message 'fileHFSTypeCode';
    function fileIsImmutable: Boolean; message 'fileIsImmutable';
    function fileIsAppendOnly: Boolean; message 'fileIsAppendOnly';
    function fileCreationDate: NSDate; message 'fileCreationDate';
    function fileOwnerAccountID: NSNumber; message 'fileOwnerAccountID';
    function fileGroupOwnerAccountID: NSNumber; message 'fileGroupOwnerAccountID';
  end;

{$endif}
{$endif}

{ Parsed from Foundation.framework NSFileManager.h }


{ Types from NSFileManager }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSFileManagerPtr = ^NSFileManager;
  NSDirectoryEnumeratorPtr = ^NSDirectoryEnumerator;
  NSFileManagerDelegateProtocolPtr = ^NSFileManagerDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  NSFoundationVersionWithFileManagerResourceForkSupport = 412;

type
  NSVolumeEnumerationOptions = NSUInteger;
  NSVolumeEnumerationOptionsPtr = ^NSVolumeEnumerationOptions;

const
  NSVolumeEnumerationSkipHiddenVolumes = 1 shl 1;
  NSVolumeEnumerationProduceFileReferenceURLs = 1 shl 2;

type
  NSDirectoryEnumerationOptions = NSUInteger;
  NSDirectoryEnumerationOptionsPtr = ^NSDirectoryEnumerationOptions;

const
  NSDirectoryEnumerationSkipsSubdirectoryDescendants = 1 shl 0;
  NSDirectoryEnumerationSkipsPackageDescendants = 1 shl 1;
  NSDirectoryEnumerationSkipsHiddenFiles = 1 shl 2;

type
  NSFileManagerItemReplacementOptions = NSUInteger;
  NSFileManagerItemReplacementOptionsPtr = ^NSFileManagerItemReplacementOptions;

const
  NSFileManagerItemReplacementUsingNewMetadataOnly = 1 shl 0;
  NSFileManagerItemReplacementWithoutDeletingBackupItem = 1 shl 1;

type
  NSURLRelationship = NSInteger;
  NSURLRelationshipPtr = ^NSURLRelationship;

const
  NSURLRelationshipContains = 0;
  NSURLRelationshipSame = 1;
  NSURLRelationshipOther = 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUbiquityIdentityDidChangeNotification: NSString { available in 10_8, 6_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSFileManager = objcclass external (NSObject)
  public
    class function defaultManager: NSFileManager; message 'defaultManager';
    function mountedVolumeURLsIncludingResourceValuesForKeys_options (propertyKeys: NSArray; options: NSVolumeEnumerationOptions): NSArray; message 'mountedVolumeURLsIncludingResourceValuesForKeys:options:'; { available in 10_6, 4_0 }
    function contentsOfDirectoryAtURL_includingPropertiesForKeys_options_error (url: NSURL; keys: NSArray; mask: NSDirectoryEnumerationOptions; error: NSErrorPtr): NSArray; message 'contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:'; { available in 10_6, 4_0 }
    function URLsForDirectory_inDomains (directory: NSSearchPathDirectory; domainMask: NSSearchPathDomainMask): NSArray; message 'URLsForDirectory:inDomains:'; { available in 10_6, 4_0 }
    function URLForDirectory_inDomain_appropriateForURL_create_error (directory: NSSearchPathDirectory; domain: NSSearchPathDomainMask; url: NSURL; shouldCreate: ObjCBOOL; error: NSErrorPtr): NSURL; message 'URLForDirectory:inDomain:appropriateForURL:create:error:'; { available in 10_6, 4_0 }
    function getRelationship_ofDirectoryAtURL_toItemAtURL_error (outRelationship: NSURLRelationshipPtr; directoryURL: NSURL; otherURL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'getRelationship:ofDirectoryAtURL:toItemAtURL:error:'; { available in 10_10, 8_0 }
    function getRelationship_ofDirectory_inDomain_toItemAtURL_error (outRelationship: NSURLRelationshipPtr; directory: NSSearchPathDirectory; domainMask: NSSearchPathDomainMask; url: NSURL; error: NSErrorPtr): ObjCBOOL; message 'getRelationship:ofDirectory:inDomain:toItemAtURL:error:'; { available in 10_10, 8_0 }
    function createDirectoryAtURL_withIntermediateDirectories_attributes_error (url: NSURL; createIntermediates: ObjCBOOL; attributes: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'createDirectoryAtURL:withIntermediateDirectories:attributes:error:'; { available in 10_7, 5_0 }
    function createSymbolicLinkAtURL_withDestinationURL_error (url: NSURL; destURL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'createSymbolicLinkAtURL:withDestinationURL:error:'; { available in 10_7, 5_0 }
    procedure setDelegate(newValue: NSFileManagerDelegateProtocol); message 'setDelegate:';
    function delegate: NSFileManagerDelegateProtocol; message 'delegate';
    function setAttributes_ofItemAtPath_error (attributes: NSDictionary; path: NSString; error: NSErrorPtr): ObjCBOOL; message 'setAttributes:ofItemAtPath:error:'; { available in 10_5, 2_0 }
    function createDirectoryAtPath_withIntermediateDirectories_attributes_error (path: NSString; createIntermediates: ObjCBOOL; attributes: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'createDirectoryAtPath:withIntermediateDirectories:attributes:error:'; { available in 10_5, 2_0 }
    function contentsOfDirectoryAtPath_error (path: NSString; error: NSErrorPtr): NSArray; message 'contentsOfDirectoryAtPath:error:'; { available in 10_5, 2_0 }
    function subpathsOfDirectoryAtPath_error (path: NSString; error: NSErrorPtr): NSArray; message 'subpathsOfDirectoryAtPath:error:'; { available in 10_5, 2_0 }
    function attributesOfItemAtPath_error (path: NSString; error: NSErrorPtr): NSDictionary; message 'attributesOfItemAtPath:error:'; { available in 10_5, 2_0 }
    function attributesOfFileSystemForPath_error (path: NSString; error: NSErrorPtr): NSDictionary; message 'attributesOfFileSystemForPath:error:'; { available in 10_5, 2_0 }
    function createSymbolicLinkAtPath_withDestinationPath_error (path: NSString; destPath: NSString; error: NSErrorPtr): ObjCBOOL; message 'createSymbolicLinkAtPath:withDestinationPath:error:'; { available in 10_5, 2_0 }
    function destinationOfSymbolicLinkAtPath_error (path: NSString; error: NSErrorPtr): NSString; message 'destinationOfSymbolicLinkAtPath:error:'; { available in 10_5, 2_0 }
    function copyItemAtPath_toPath_error (srcPath: NSString; dstPath: NSString; error: NSErrorPtr): ObjCBOOL; message 'copyItemAtPath:toPath:error:'; { available in 10_5, 2_0 }
    function moveItemAtPath_toPath_error (srcPath: NSString; dstPath: NSString; error: NSErrorPtr): ObjCBOOL; message 'moveItemAtPath:toPath:error:'; { available in 10_5, 2_0 }
    function linkItemAtPath_toPath_error (srcPath: NSString; dstPath: NSString; error: NSErrorPtr): ObjCBOOL; message 'linkItemAtPath:toPath:error:'; { available in 10_5, 2_0 }
    function removeItemAtPath_error (path: NSString; error: NSErrorPtr): ObjCBOOL; message 'removeItemAtPath:error:'; { available in 10_5, 2_0 }
    function copyItemAtURL_toURL_error (srcURL: NSURL; dstURL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'copyItemAtURL:toURL:error:'; { available in 10_6, 4_0 }
    function moveItemAtURL_toURL_error (srcURL: NSURL; dstURL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'moveItemAtURL:toURL:error:'; { available in 10_6, 4_0 }
    function linkItemAtURL_toURL_error (srcURL: NSURL; dstURL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'linkItemAtURL:toURL:error:'; { available in 10_6, 4_0 }
    function removeItemAtURL_error (URL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'removeItemAtURL:error:'; { available in 10_6, 4_0 }
    function trashItemAtURL_resultingItemURL_error (url: NSURL; outResultingURL: NSURLPtr; error: NSErrorPtr): ObjCBOOL; message 'trashItemAtURL:resultingItemURL:error:'; { available in 10_8 }
    function fileAttributesAtPath_traverseLink (path: NSString; yorn: ObjCBOOL): NSDictionary; message 'fileAttributesAtPath:traverseLink:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function changeFileAttributes_atPath (attributes: NSDictionary; path: NSString): ObjCBOOL; message 'changeFileAttributes:atPath:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function directoryContentsAtPath (path: NSString): NSArray; message 'directoryContentsAtPath:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function fileSystemAttributesAtPath (path: NSString): NSDictionary; message 'fileSystemAttributesAtPath:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function pathContentOfSymbolicLinkAtPath (path: NSString): NSString; message 'pathContentOfSymbolicLinkAtPath:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function createSymbolicLinkAtPath_pathContent (path: NSString; otherpath: NSString): ObjCBOOL; message 'createSymbolicLinkAtPath:pathContent:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function createDirectoryAtPath_attributes (path: NSString; attributes: NSDictionary): ObjCBOOL; message 'createDirectoryAtPath:attributes:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
    function linkPath_toPath_handler (src: NSString; dest: NSString; handler: id): ObjCBOOL; message 'linkPath:toPath:handler:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function copyPath_toPath_handler (src: NSString; dest: NSString; handler: id): ObjCBOOL; message 'copyPath:toPath:handler:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function movePath_toPath_handler (src: NSString; dest: NSString; handler: id): ObjCBOOL; message 'movePath:toPath:handler:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    function removeFileAtPath_handler (path: NSString; handler: id): ObjCBOOL; message 'removeFileAtPath:handler:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    {$endif}
    function currentDirectoryPath: NSString; message 'currentDirectoryPath';
    function changeCurrentDirectoryPath (path: NSString): ObjCBOOL; message 'changeCurrentDirectoryPath:';
    function fileExistsAtPath (path: NSString): ObjCBOOL; message 'fileExistsAtPath:';
    function fileExistsAtPath_isDirectory (path: NSString; isDirectory: pObjCBOOL): ObjCBOOL; message 'fileExistsAtPath:isDirectory:';
    function isReadableFileAtPath (path: NSString): ObjCBOOL; message 'isReadableFileAtPath:';
    function isWritableFileAtPath (path: NSString): ObjCBOOL; message 'isWritableFileAtPath:';
    function isExecutableFileAtPath (path: NSString): ObjCBOOL; message 'isExecutableFileAtPath:';
    function isDeletableFileAtPath (path: NSString): ObjCBOOL; message 'isDeletableFileAtPath:';
    function contentsEqualAtPath_andPath (path1: NSString; path2: NSString): ObjCBOOL; message 'contentsEqualAtPath:andPath:';
    function displayNameAtPath (path: NSString): NSString; message 'displayNameAtPath:';
    function componentsToDisplayForPath (path: NSString): NSArray; message 'componentsToDisplayForPath:';
    function enumeratorAtPath (path: NSString): NSDirectoryEnumerator; message 'enumeratorAtPath:';
    function enumeratorAtURL_includingPropertiesForKeys_options_errorHandler (url: NSURL; keys: NSArray; mask: NSDirectoryEnumerationOptions; handler: OpaqueCBlock): NSDirectoryEnumerator; message 'enumeratorAtURL:includingPropertiesForKeys:options:errorHandler:'; { available in 10_6, 4_0 }
    function subpathsAtPath (path: NSString): NSArray; message 'subpathsAtPath:';
    function contentsAtPath (path: NSString): NSData; message 'contentsAtPath:';
    function createFileAtPath_contents_attributes (path: NSString; data: NSData; attr: NSDictionary): ObjCBOOL; message 'createFileAtPath:contents:attributes:';
    function fileSystemRepresentationWithPath (path: NSString): PChar; message 'fileSystemRepresentationWithPath:';
    function stringWithFileSystemRepresentation_length (str: PChar; len: NSUInteger): NSString; message 'stringWithFileSystemRepresentation:length:';
    function replaceItemAtURL_withItemAtURL_backupItemName_options_resultingItemURL_error (originalItemURL: NSURL; newItemURL: NSURL; backupItemName: NSString; options: NSFileManagerItemReplacementOptions; resultingURL: NSURLPtr; error: NSErrorPtr): ObjCBOOL; message 'replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error:'; { available in 10_6, 4_0 }
    function setUbiquitous_itemAtURL_destinationURL_error (flag: ObjCBOOL; url: NSURL; destinationURL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'setUbiquitous:itemAtURL:destinationURL:error:'; { available in 10_7, 5_0 }
    function isUbiquitousItemAtURL (url: NSURL): ObjCBOOL; message 'isUbiquitousItemAtURL:'; { available in 10_7, 5_0 }
    function startDownloadingUbiquitousItemAtURL_error (url: NSURL; error: NSErrorPtr): ObjCBOOL; message 'startDownloadingUbiquitousItemAtURL:error:'; { available in 10_7, 5_0 }
    function evictUbiquitousItemAtURL_error (url: NSURL; error: NSErrorPtr): ObjCBOOL; message 'evictUbiquitousItemAtURL:error:'; { available in 10_7, 5_0 }
    function URLForUbiquityContainerIdentifier (containerIdentifier: NSString): NSURL; message 'URLForUbiquityContainerIdentifier:'; { available in 10_7, 5_0 }
    function URLForPublishingUbiquitousItemAtURL_expirationDate_error (url: NSURL; outDate: NSDatePtr; error: NSErrorPtr): NSURL; message 'URLForPublishingUbiquitousItemAtURL:expirationDate:error:'; { available in 10_7, 5_0 }
    function ubiquityIdentityToken: id; message 'ubiquityIdentityToken';
    function containerURLForSecurityApplicationGroupIdentifier (groupIdentifier: NSString): NSURL; message 'containerURLForSecurityApplicationGroupIdentifier:'; { available in 10_8, 7_0 }
  end;


type
  NSCopyLinkMoveHandler = objccategory external (NSObject)
    function fileManager_shouldProceedAfterError (fm: NSFileManager; errorInfo: NSDictionary): ObjCBOOL; message 'fileManager:shouldProceedAfterError:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
    procedure fileManager_willProcessPath (fm: NSFileManager; path: NSString); message 'fileManager:willProcessPath:'; deprecated 'in 10_0, 10_5, 2_0, 2_0';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSFileManagerDelegateProtocol = objcprotocol external name 'NSFileManagerDelegate' (NSObjectProtocol)
  optional
    function fileManager_shouldCopyItemAtPath_toPath (fileManager: NSFileManager; srcPath: NSString; dstPath: NSString): ObjCBOOL; message 'fileManager:shouldCopyItemAtPath:toPath:';
    function fileManager_shouldCopyItemAtURL_toURL (fileManager: NSFileManager; srcURL: NSURL; dstURL: NSURL): ObjCBOOL; message 'fileManager:shouldCopyItemAtURL:toURL:'; { available in 10_6, 4_0 }
    function fileManager_shouldProceedAfterError_copyingItemAtPath_toPath (fileManager: NSFileManager; error: NSError; srcPath: NSString; dstPath: NSString): ObjCBOOL; message 'fileManager:shouldProceedAfterError:copyingItemAtPath:toPath:';
    function fileManager_shouldProceedAfterError_copyingItemAtURL_toURL (fileManager: NSFileManager; error: NSError; srcURL: NSURL; dstURL: NSURL): ObjCBOOL; message 'fileManager:shouldProceedAfterError:copyingItemAtURL:toURL:'; { available in 10_6, 4_0 }
    function fileManager_shouldMoveItemAtPath_toPath (fileManager: NSFileManager; srcPath: NSString; dstPath: NSString): ObjCBOOL; message 'fileManager:shouldMoveItemAtPath:toPath:';
    function fileManager_shouldMoveItemAtURL_toURL (fileManager: NSFileManager; srcURL: NSURL; dstURL: NSURL): ObjCBOOL; message 'fileManager:shouldMoveItemAtURL:toURL:'; { available in 10_6, 4_0 }
    function fileManager_shouldProceedAfterError_movingItemAtPath_toPath (fileManager: NSFileManager; error: NSError; srcPath: NSString; dstPath: NSString): ObjCBOOL; message 'fileManager:shouldProceedAfterError:movingItemAtPath:toPath:';
    function fileManager_shouldProceedAfterError_movingItemAtURL_toURL (fileManager: NSFileManager; error: NSError; srcURL: NSURL; dstURL: NSURL): ObjCBOOL; message 'fileManager:shouldProceedAfterError:movingItemAtURL:toURL:'; { available in 10_6, 4_0 }
    function fileManager_shouldLinkItemAtPath_toPath (fileManager: NSFileManager; srcPath: NSString; dstPath: NSString): ObjCBOOL; message 'fileManager:shouldLinkItemAtPath:toPath:';
    function fileManager_shouldLinkItemAtURL_toURL (fileManager: NSFileManager; srcURL: NSURL; dstURL: NSURL): ObjCBOOL; message 'fileManager:shouldLinkItemAtURL:toURL:'; { available in 10_6, 4_0 }
    function fileManager_shouldProceedAfterError_linkingItemAtPath_toPath (fileManager: NSFileManager; error: NSError; srcPath: NSString; dstPath: NSString): ObjCBOOL; message 'fileManager:shouldProceedAfterError:linkingItemAtPath:toPath:';
    function fileManager_shouldProceedAfterError_linkingItemAtURL_toURL (fileManager: NSFileManager; error: NSError; srcURL: NSURL; dstURL: NSURL): ObjCBOOL; message 'fileManager:shouldProceedAfterError:linkingItemAtURL:toURL:'; { available in 10_6, 4_0 }
    function fileManager_shouldRemoveItemAtPath (fileManager: NSFileManager; path: NSString): ObjCBOOL; message 'fileManager:shouldRemoveItemAtPath:';
    function fileManager_shouldRemoveItemAtURL (fileManager: NSFileManager; URL: NSURL): ObjCBOOL; message 'fileManager:shouldRemoveItemAtURL:'; { available in 10_6, 4_0 }
    function fileManager_shouldProceedAfterError_removingItemAtPath (fileManager: NSFileManager; error: NSError; path: NSString): ObjCBOOL; message 'fileManager:shouldProceedAfterError:removingItemAtPath:';
    function fileManager_shouldProceedAfterError_removingItemAtURL (fileManager: NSFileManager; error: NSError; URL: NSURL): ObjCBOOL; message 'fileManager:shouldProceedAfterError:removingItemAtURL:'; { available in 10_6, 4_0 }
  end;
{$endif}

{$ifdef CLASSES}

type
  NSDirectoryEnumerator = objcclass external (NSEnumerator)
  public
    function fileAttributes: NSDictionary; message 'fileAttributes';
    function directoryAttributes: NSDictionary; message 'directoryAttributes';
    procedure skipDescendents; message 'skipDescendents';
    function level: NSUInteger; message 'level';
    procedure skipDescendants; message 'skipDescendants'; { available in 10_6, 4_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
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
  NSFileProtectionKey: NSString { available in 4_0 }; cvar; external;
  NSFileProtectionNone: NSString { available in 4_0 }; cvar; external;
  NSFileProtectionComplete: NSString { available in 4_0 }; cvar; external;
  NSFileProtectionCompleteUnlessOpen: NSString { available in 5_0 }; cvar; external;
  NSFileProtectionCompleteUntilFirstUserAuthentication: NSString { available in 5_0 }; cvar; external;
  NSFileSystemSize: NSString; cvar; external;
  NSFileSystemFreeSize: NSString; cvar; external;
  NSFileSystemNodes: NSString; cvar; external;
  NSFileSystemFreeNodes: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSFileAttributes = objccategory external (NSDictionary)
    function fileSize: culonglong; message 'fileSize';
    function fileModificationDate: NSDate; message 'fileModificationDate';
    function fileType: NSString; message 'fileType';
    function filePosixPermissions: NSUInteger; message 'filePosixPermissions';
    function fileOwnerAccountName: NSString; message 'fileOwnerAccountName';
    function fileGroupOwnerAccountName: NSString; message 'fileGroupOwnerAccountName';
    function fileSystemNumber: NSInteger; message 'fileSystemNumber';
    function fileSystemFileNumber: NSUInteger; message 'fileSystemFileNumber';
    function fileExtensionHidden: ObjCBOOL; message 'fileExtensionHidden';
    function fileHFSCreatorCode: OSType; message 'fileHFSCreatorCode';
    function fileHFSTypeCode: OSType; message 'fileHFSTypeCode';
    function fileIsImmutable: ObjCBOOL; message 'fileIsImmutable';
    function fileIsAppendOnly: ObjCBOOL; message 'fileIsAppendOnly';
    function fileCreationDate: NSDate; message 'fileCreationDate';
    function fileOwnerAccountID: NSNumber; message 'fileOwnerAccountID';
    function fileGroupOwnerAccountID: NSNumber; message 'fileGroupOwnerAccountID';
  end;
{$endif}


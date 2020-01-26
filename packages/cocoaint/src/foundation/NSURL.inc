{ Parsed from Foundation.framework NSURL.h }


{ Types from NSURL }
{$ifdef TYPES}


type
  NSURLBookmarkCreationOptions = NSUInteger;
type
  NSURLBookmarkCreationOptionsPtr = ^NSURLBookmarkCreationOptions;

const
  NSURLBookmarkCreationPreferFileIDResolution = 1 shl 8 deprecated 'in 10_6, 10_9, 4_0, 7_0';
  NSURLBookmarkCreationMinimalBookmark = 1 shl 9;
  NSURLBookmarkCreationSuitableForBookmarkFile = 1 shl 10;
  NSURLBookmarkCreationWithSecurityScope = 1 shl 11 { available in 10_7, NA };
  NSURLBookmarkCreationSecurityScopeAllowOnlyReadAccess = 1 shl 12 { available in 10_7, NA };

type
  NSURLBookmarkResolutionOptions = NSUInteger;
type
  NSURLBookmarkResolutionOptionsPtr = ^NSURLBookmarkResolutionOptions;

const
  NSURLBookmarkResolutionWithoutUI = 1 shl 8;
  NSURLBookmarkResolutionWithoutMounting = 1 shl 9;
  NSURLBookmarkResolutionWithSecurityScope = 1 shl 10 { available in 10_7, NA };

type
  NSURLBookmarkFileCreationOptions = NSUInteger;
type
  NSURLBookmarkFileCreationOptionsPtr = ^NSURLBookmarkFileCreationOptions;
{$endif}


{$ifdef TYPES}
type
  NSURLPtr = ^NSURL;
  NSURLQueryItemPtr = ^NSURLQueryItem;
  NSURLComponentsPtr = ^NSURLComponents;
{$endif}

{$ifdef CLASSES}

type
  NSURL = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _urlString: NSString;
    _baseURL: NSURL;
    _clients: pointer;
    _reserved: pointer;
  public
    function initWithScheme_host_path (scheme: NSString; host: NSString; path: NSString): instancetype; message 'initWithScheme:host:path:';
    function initFileURLWithPath_isDirectory (path: NSString; isDir: ObjCBOOL): instancetype; message 'initFileURLWithPath:isDirectory:'; { available in 10_5, 2_0 }
    function initFileURLWithPath (path: NSString): instancetype; message 'initFileURLWithPath:'; { NS_DESIGNATED_INITIALIZER }
    class function fileURLWithPath_isDirectory (path: NSString; isDir: ObjCBOOL): NSURL; message 'fileURLWithPath:isDirectory:'; { available in 10_5, 2_0 }
    class function fileURLWithPath (path: NSString): NSURL; message 'fileURLWithPath:';
    function initFileURLWithFileSystemRepresentation_isDirectory_relativeToURL (path: PChar; isDir: ObjCBOOL; baseURL: NSURL): instancetype; message 'initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:'; { available in 10_9, 7_0 }
    class function fileURLWithFileSystemRepresentation_isDirectory_relativeToURL (path: PChar; isDir: ObjCBOOL; baseURL: NSURL): NSURL; message 'fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:'; { available in 10_9, 7_0 }
    function initWithString (URLString: NSString): instancetype; message 'initWithString:';
    function initWithString_relativeToURL (URLString: NSString; baseURL: NSURL): instancetype; message 'initWithString:relativeToURL:'; { NS_DESIGNATED_INITIALIZER }
    class function URLWithString (URLString: NSString): instancetype; message 'URLWithString:';
    class function URLWithString_relativeToURL (URLString: NSString; baseURL: NSURL): instancetype; message 'URLWithString:relativeToURL:';
    function absoluteString: NSString; message 'absoluteString';
    function relativeString: NSString; message 'relativeString';
    function baseURL: NSURL; message 'baseURL';
    function absoluteURL: NSURL; message 'absoluteURL';
    function scheme: NSString; message 'scheme';
    function resourceSpecifier: NSString; message 'resourceSpecifier';
    function host: NSString; message 'host';
    function port: NSNumber; message 'port';
    function user: NSString; message 'user';
    function password: NSString; message 'password';
    function path: NSString; message 'path';
    function fragment: NSString; message 'fragment';
    function parameterString: NSString; message 'parameterString';
    function query: NSString; message 'query';
    function relativePath: NSString; message 'relativePath';
    function getFileSystemRepresentation_maxLength (buffer: PChar; maxBufferLength: NSUInteger): ObjCBOOL; message 'getFileSystemRepresentation:maxLength:'; { available in 10_9, 7_0 }
    function fileSystemRepresentation: PChar; message 'fileSystemRepresentation';
    function isFileURL: ObjCBOOL; message 'isFileURL';
    function standardizedURL: NSURL; message 'standardizedURL';
    function checkResourceIsReachableAndReturnError (error: NSErrorPtr): ObjCBOOL; message 'checkResourceIsReachableAndReturnError:'; { available in 10_6, 4_0 }
    function isFileReferenceURL: ObjCBOOL; message 'isFileReferenceURL'; { available in 10_6, 4_0 }
    function fileReferenceURL: NSURL; message 'fileReferenceURL'; { available in 10_6, 4_0 }
    function filePathURL: NSURL; message 'filePathURL';
    function getResourceValue_forKey_error (value: idPtr; key: NSString; error: NSErrorPtr): ObjCBOOL; message 'getResourceValue:forKey:error:'; { available in 10_6, 4_0 }
    function resourceValuesForKeys_error (keys: NSArray; error: NSErrorPtr): NSDictionary; message 'resourceValuesForKeys:error:'; { available in 10_6, 4_0 }
    function setResourceValue_forKey_error (value: id; key: NSString; error: NSErrorPtr): ObjCBOOL; message 'setResourceValue:forKey:error:'; { available in 10_6, 4_0 }
    function setResourceValues_error (keyedValues: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'setResourceValues:error:'; { available in 10_6, 4_0 }
    procedure removeCachedResourceValueForKey (key: NSString); message 'removeCachedResourceValueForKey:'; { available in 10_9, 7_0 }
    procedure removeAllCachedResourceValues; message 'removeAllCachedResourceValues'; { available in 10_9, 7_0 }
    procedure setTemporaryResourceValue_forKey (value: id; key: NSString); message 'setTemporaryResourceValue:forKey:'; { available in 10_9, 7_0 }
    function bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_error (options: NSURLBookmarkCreationOptions; keys: NSArray; relativeURL: NSURL; error: NSErrorPtr): NSData; message 'bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:'; { available in 10_6, 4_0 }
    function initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error (bookmarkData: NSData; options: NSURLBookmarkResolutionOptions; relativeURL: NSURL; isStale: pObjCBOOL; error: NSErrorPtr): instancetype; message 'initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:'; { available in 10_6, 4_0 }
    class function URLByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error (bookmarkData: NSData; options: NSURLBookmarkResolutionOptions; relativeURL: NSURL; isStale: pObjCBOOL; error: NSErrorPtr): instancetype; message 'URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:'; { available in 10_6, 4_0 }
    class function resourceValuesForKeys_fromBookmarkData (keys: NSArray; bookmarkData: NSData): NSDictionary; message 'resourceValuesForKeys:fromBookmarkData:'; { available in 10_6, 4_0 }
    class function writeBookmarkData_toURL_options_error (bookmarkData: NSData; bookmarkFileURL: NSURL; options: NSURLBookmarkFileCreationOptions; error: NSErrorPtr): ObjCBOOL; message 'writeBookmarkData:toURL:options:error:'; { available in 10_6, 4_0 }
    class function bookmarkDataWithContentsOfURL_error (bookmarkFileURL: NSURL; error: NSErrorPtr): NSData; message 'bookmarkDataWithContentsOfURL:error:'; { available in 10_6, 4_0 }
    class function URLByResolvingAliasFileAtURL_options_error (url: NSURL; options: NSURLBookmarkResolutionOptions; error: NSErrorPtr): instancetype; message 'URLByResolvingAliasFileAtURL:options:error:'; { available in 10_10, 8_0 }
    function startAccessingSecurityScopedResource: ObjCBOOL; message 'startAccessingSecurityScopedResource'; { available in 10_7, 8_0 }
    procedure stopAccessingSecurityScopedResource; message 'stopAccessingSecurityScopedResource'; { available in 10_7, 8_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSPromisedItems = objccategory external (NSURL)
    function getPromisedItemResourceValue_forKey_error (value: idPtr; key: NSString; error: NSErrorPtr): ObjCBOOL; message 'getPromisedItemResourceValue:forKey:error:'; { available in 10_10, 8_0 }
    function promisedItemResourceValuesForKeys_error (keys: NSArray; error: NSErrorPtr): NSDictionary; message 'promisedItemResourceValuesForKeys:error:'; { available in 10_10, 8_0 }
    function checkPromisedItemIsReachableAndReturnError (error: NSErrorPtr): ObjCBOOL; message 'checkPromisedItemIsReachableAndReturnError:'; { available in 10_10, 8_0 }
  end;


type
  NSURLQueryItem = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _name: NSString;
    _value: NSString;
  public
    function initWithName_value (name: NSString; value: NSString): instancetype; message 'initWithName:value:'; { NS_DESIGNATED_INITIALIZER }
    class function queryItemWithName_value (name: NSString; value: NSString): instancetype; message 'queryItemWithName:value:';
    function name: NSString; message 'name';
    function value: NSString; message 'value';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  NSURLComponents = objcclass external (NSObject, NSCopyingProtocol)
  public
    function init: instancetype; message 'init';
    function initWithURL_resolvingAgainstBaseURL (url: NSURL; resolve: ObjCBOOL): instancetype; message 'initWithURL:resolvingAgainstBaseURL:';
    class function componentsWithURL_resolvingAgainstBaseURL (url: NSURL; resolve: ObjCBOOL): instancetype; message 'componentsWithURL:resolvingAgainstBaseURL:';
    function initWithString (URLString: NSString): instancetype; message 'initWithString:';
    class function componentsWithString (URLString: NSString): instancetype; message 'componentsWithString:';
    function URL: NSURL; message 'URL';
    function URLRelativeToURL (baseURL: NSURL): NSURL; message 'URLRelativeToURL:';
    function string_: NSString; message 'string';
    procedure setScheme(newValue: NSString); message 'setScheme:';
    function scheme: NSString; message 'scheme';
    procedure setUser(newValue: NSString); message 'setUser:';
    function user: NSString; message 'user';
    procedure setPassword(newValue: NSString); message 'setPassword:';
    function password: NSString; message 'password';
    procedure setHost(newValue: NSString); message 'setHost:';
    function host: NSString; message 'host';
    procedure setPort(newValue: NSNumber); message 'setPort:';
    function port: NSNumber; message 'port';
    procedure setPath(newValue: NSString); message 'setPath:';
    function path: NSString; message 'path';
    procedure setQuery(newValue: NSString); message 'setQuery:';
    function query: NSString; message 'query';
    procedure setFragment(newValue: NSString); message 'setFragment:';
    function fragment: NSString; message 'fragment';
    procedure setPercentEncodedUser(newValue: NSString); message 'setPercentEncodedUser:';
    function percentEncodedUser: NSString; message 'percentEncodedUser';
    procedure setPercentEncodedPassword(newValue: NSString); message 'setPercentEncodedPassword:';
    function percentEncodedPassword: NSString; message 'percentEncodedPassword';
    procedure setPercentEncodedHost(newValue: NSString); message 'setPercentEncodedHost:';
    function percentEncodedHost: NSString; message 'percentEncodedHost';
    procedure setPercentEncodedPath(newValue: NSString); message 'setPercentEncodedPath:';
    function percentEncodedPath: NSString; message 'percentEncodedPath';
    procedure setPercentEncodedQuery(newValue: NSString); message 'setPercentEncodedQuery:';
    function percentEncodedQuery: NSString; message 'percentEncodedQuery';
    procedure setPercentEncodedFragment(newValue: NSString); message 'setPercentEncodedFragment:';
    function percentEncodedFragment: NSString; message 'percentEncodedFragment';
    procedure setQueryItems(newValue: NSArray); message 'setQueryItems:';
    function queryItems: NSArray; message 'queryItems';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;


type
  NSCharacterSet_NSURLUtilities = objccategory external name 'NSURLUtilities' (NSCharacterSet)
    class function URLUserAllowedCharacterSet: NSCharacterSet; message 'URLUserAllowedCharacterSet'; { available in 10_9, 7_0 }
    class function URLPasswordAllowedCharacterSet: NSCharacterSet; message 'URLPasswordAllowedCharacterSet'; { available in 10_9, 7_0 }
    class function URLHostAllowedCharacterSet: NSCharacterSet; message 'URLHostAllowedCharacterSet'; { available in 10_9, 7_0 }
    class function URLPathAllowedCharacterSet: NSCharacterSet; message 'URLPathAllowedCharacterSet'; { available in 10_9, 7_0 }
    class function URLQueryAllowedCharacterSet: NSCharacterSet; message 'URLQueryAllowedCharacterSet'; { available in 10_9, 7_0 }
    class function URLFragmentAllowedCharacterSet: NSCharacterSet; message 'URLFragmentAllowedCharacterSet'; { available in 10_9, 7_0 }
  end;

type
  NSURLUtilities = objccategory external (NSString)
    function stringByAddingPercentEncodingWithAllowedCharacters (allowedCharacters: NSCharacterSet): NSString; message 'stringByAddingPercentEncodingWithAllowedCharacters:'; { available in 10_9, 7_0 }
    function stringByRemovingPercentEncoding: NSString; message 'stringByRemovingPercentEncoding';
    function stringByAddingPercentEscapesUsingEncoding (enc: NSStringEncoding): NSString; message 'stringByAddingPercentEscapesUsingEncoding:';
    function stringByReplacingPercentEscapesUsingEncoding (enc: NSStringEncoding): NSString; message 'stringByReplacingPercentEscapesUsingEncoding:';
  end;

type
  NSURLPathUtilities = objccategory external (NSURL)
    class function fileURLWithPathComponents (components: NSArray): NSURL; message 'fileURLWithPathComponents:'; { available in 10_6, 4_0 }
    function pathComponents: NSArray; message 'pathComponents';
    function lastPathComponent: NSString; message 'lastPathComponent';
    function pathExtension: NSString; message 'pathExtension';
    function URLByAppendingPathComponent (pathComponent: NSString): NSURL; message 'URLByAppendingPathComponent:'; { available in 10_6, 4_0 }
    function URLByAppendingPathComponent_isDirectory (pathComponent: NSString; isDirectory: ObjCBOOL): NSURL; message 'URLByAppendingPathComponent:isDirectory:'; { available in 10_7, 5_0 }
    function URLByDeletingLastPathComponent: NSURL; message 'URLByDeletingLastPathComponent';
    function URLByAppendingPathExtension (pathExtension_: NSString): NSURL; message 'URLByAppendingPathExtension:'; { available in 10_6, 4_0 }
    function URLByDeletingPathExtension: NSURL; message 'URLByDeletingPathExtension';
    function URLByStandardizingPath: NSURL; message 'URLByStandardizingPath';
    function URLByResolvingSymlinksInPath: NSURL; message 'URLByResolvingSymlinksInPath';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  NSURLClient = objccategory external (NSObject)
    procedure URL_resourceDataDidBecomeAvailable (sender: NSURL; newBytes: NSData); message 'URL:resourceDataDidBecomeAvailable:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    procedure URLResourceDidFinishLoading (sender: NSURL); message 'URLResourceDidFinishLoading:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    procedure URLResourceDidCancelLoading (sender: NSURL); message 'URLResourceDidCancelLoading:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    procedure URL_resourceDidFailLoadingWithReason (sender: NSURL; reason: NSString); message 'URL:resourceDidFailLoadingWithReason:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
  end;

type
  NSURLLoading = objccategory external (NSURL)
    function resourceDataUsingCache (shouldUseCache: ObjCBOOL): NSData; message 'resourceDataUsingCache:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    procedure loadResourceDataNotifyingClient_usingCache (client: id; shouldUseCache: ObjCBOOL); message 'loadResourceDataNotifyingClient:usingCache:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    function propertyForKey (propertyKey: NSString): id; message 'propertyForKey:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    function setResourceData (data: NSData): ObjCBOOL; message 'setResourceData:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    function setProperty_forKey (property_: id; propertyKey: NSString): ObjCBOOL; message 'setProperty:forKey:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    function URLHandleUsingCache (shouldUseCache: ObjCBOOL): NSURLHandle; message 'URLHandleUsingCache:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
  end;
{$endif}

{$endif}

{$ifdef EXTERNAL_SYMBOLS}
NSURLFileScheme: NSString; cvar; external;
NSURLKeysOfUnsetValuesKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLNameKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLLocalizedNameKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsRegularFileKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsDirectoryKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsSymbolicLinkKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsVolumeKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsPackageKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsSystemImmutableKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsUserImmutableKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLIsHiddenKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLHasHiddenExtensionKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLCreationDateKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLContentAccessDateKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLContentModificationDateKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLAttributeModificationDateKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLLinkCountKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLParentDirectoryURLKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeURLKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLTypeIdentifierKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLLocalizedTypeDescriptionKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLLabelNumberKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLLabelColorKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLLocalizedLabelKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLEffectiveIconKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLCustomIconKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLFileResourceIdentifierKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIdentifierKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLPreferredIOBlockSizeKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLIsReadableKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLIsWritableKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLIsExecutableKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileSecurityKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLIsExcludedFromBackupKey: NSString { available in 10_8, 5_1 }; cvar; external;
NSURLTagNamesKey: NSString { available in 10_9, NA }; cvar; external;
NSURLPathKey: NSString { available in 10_8, 6_0 }; cvar; external;
NSURLIsMountTriggerKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLGenerationIdentifierKey: NSString { available in 10_10, 8_0 }; cvar; external;
NSURLDocumentIdentifierKey: NSString { available in 10_10, 8_0 }; cvar; external;
NSURLAddedToDirectoryDateKey: NSString { available in 10_10, 8_0 }; cvar; external;
NSURLQuarantinePropertiesKey: NSString { available in 10_10, NA }; cvar; external;
NSURLFileResourceTypeKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeNamedPipe: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeCharacterSpecial: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeDirectory: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeBlockSpecial: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeRegular: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeSymbolicLink: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeSocket: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLFileResourceTypeUnknown: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLThumbnailDictionaryKey: NSString { available in 10_10, 8_0 }; cvar; external;
NSURLThumbnailKey: NSString { available in 10_10 }; cvar; external;
NSThumbnail1024x1024SizeKey: NSString { available in 10_10, 8_0 }; cvar; external;
NSURLFileSizeKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLFileAllocatedSizeKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLTotalFileSizeKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLTotalFileAllocatedSizeKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLIsAliasFileKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeLocalizedFormatDescriptionKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeTotalCapacityKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeAvailableCapacityKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeResourceCountKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsPersistentIDsKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsSymbolicLinksKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsHardLinksKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsJournalingKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeIsJournalingKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsSparseFilesKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsZeroRunsKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsCaseSensitiveNamesKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsCasePreservedNamesKey: NSString { available in 10_6, 4_0 }; cvar; external;
NSURLVolumeSupportsRootDirectoryDatesKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeSupportsVolumeSizesKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeSupportsRenamingKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeSupportsAdvisoryFileLockingKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeSupportsExtendedSecurityKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIsBrowsableKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeMaximumFileSizeKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIsEjectableKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIsRemovableKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIsInternalKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIsAutomountedKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIsLocalKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeIsReadOnlyKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeCreationDateKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeURLForRemountingKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeUUIDStringKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeNameKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLVolumeLocalizedNameKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLIsUbiquitousItemKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLUbiquitousItemHasUnresolvedConflictsKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLUbiquitousItemIsDownloadedKey: NSString deprecated 'in 10_7, 10_9, 5_0, 7_0, "Use NSURLUbiquitousItemDownloadingStatusKey instead"'; cvar; external;
NSURLUbiquitousItemIsDownloadingKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLUbiquitousItemIsUploadedKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLUbiquitousItemIsUploadingKey: NSString { available in 10_7, 5_0 }; cvar; external;
NSURLUbiquitousItemPercentDownloadedKey: NSString deprecated 'in 10_7, 10_8, 5_0, 6_0'; cvar; external;
NSURLUbiquitousItemPercentUploadedKey: NSString deprecated 'in 10_7, 10_8, 5_0, 6_0'; cvar; external;
NSURLUbiquitousItemDownloadingStatusKey: NSString { available in 10_9, 7_0 }; cvar; external;
NSURLUbiquitousItemDownloadingErrorKey: NSString { available in 10_9, 7_0 }; cvar; external;
NSURLUbiquitousItemUploadingErrorKey: NSString { available in 10_9, 7_0 }; cvar; external;
NSURLUbiquitousItemDownloadRequestedKey: NSString { available in 10_10, 8_0 }; cvar; external;
NSURLUbiquitousItemContainerDisplayNameKey: NSString { available in 10_10, 8_0 }; cvar; external;
NSURLUbiquitousItemDownloadingStatusNotDownloaded: NSString { available in 10_9, 7_0 }; cvar; external;
NSURLUbiquitousItemDownloadingStatusDownloaded: NSString { available in 10_9, 7_0 }; cvar; external;
NSURLUbiquitousItemDownloadingStatusCurrent: NSString { available in 10_9, 7_0 }; cvar; external;
{$endif}

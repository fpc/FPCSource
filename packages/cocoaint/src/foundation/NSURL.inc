{ Parsed from Foundation.framework NSURL.h }

{$ifdef TYPES}
{$ifndef NSURL_PAS_T}
{$define NSURL_PAS_T}

{ Constants }

{ Types }
type
  NSURLBookmarkCreationOptions = NSUInteger;
  NSURLBookmarkCreationOptionsPtr = ^NSURLBookmarkCreationOptions;
  NSURLBookmarkResolutionOptions = NSUInteger;
  NSURLBookmarkResolutionOptionsPtr = ^NSURLBookmarkResolutionOptions;
  NSURLBookmarkFileCreationOptions = NSUInteger;
  NSURLBookmarkFileCreationOptionsPtr = ^NSURLBookmarkFileCreationOptions;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURL_PAS_R}
{$define NSURL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURL_PAS_F}
{$define NSURL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURL_PAS_S}
{$define NSURL_PAS_S}

{ External string constants }
var
  NSURLFileScheme: NSString; cvar; external;
  NSURLNameKey: NSString; cvar; external;
  NSURLLocalizedNameKey: NSString; cvar; external;
  NSURLIsRegularFileKey: NSString; cvar; external;
  NSURLIsDirectoryKey: NSString; cvar; external;
  NSURLIsSymbolicLinkKey: NSString; cvar; external;
  NSURLIsVolumeKey: NSString; cvar; external;
  NSURLIsPackageKey: NSString; cvar; external;
  NSURLIsSystemImmutableKey: NSString; cvar; external;
  NSURLIsUserImmutableKey: NSString; cvar; external;
  NSURLIsHiddenKey: NSString; cvar; external;
  NSURLHasHiddenExtensionKey: NSString; cvar; external;
  NSURLCreationDateKey: NSString; cvar; external;
  NSURLContentAccessDateKey: NSString; cvar; external;
  NSURLContentModificationDateKey: NSString; cvar; external;
  NSURLAttributeModificationDateKey: NSString; cvar; external;
  NSURLLinkCountKey: NSString; cvar; external;
  NSURLParentDirectoryURLKey: NSString; cvar; external;
  NSURLVolumeURLKey: NSString; cvar; external;
  NSURLTypeIdentifierKey: NSString; cvar; external;
  NSURLLocalizedTypeDescriptionKey: NSString; cvar; external;
  NSURLLabelNumberKey: NSString; cvar; external;
  NSURLLabelColorKey: NSString; cvar; external;
  NSURLLocalizedLabelKey: NSString; cvar; external;
  NSURLEffectiveIconKey: NSString; cvar; external;
  NSURLCustomIconKey: NSString; cvar; external;
  NSURLFileSizeKey: NSString; cvar; external;
  NSURLFileAllocatedSizeKey: NSString; cvar; external;
  NSURLIsAliasFileKey: NSString; cvar; external;
  NSURLVolumeLocalizedFormatDescriptionKey: NSString; cvar; external;
  NSURLVolumeTotalCapacityKey: NSString; cvar; external;
  NSURLVolumeAvailableCapacityKey: NSString; cvar; external;
  NSURLVolumeResourceCountKey: NSString; cvar; external;
  NSURLVolumeSupportsPersistentIDsKey: NSString; cvar; external;
  NSURLVolumeSupportsSymbolicLinksKey: NSString; cvar; external;
  NSURLVolumeSupportsHardLinksKey: NSString; cvar; external;
  NSURLVolumeSupportsJournalingKey: NSString; cvar; external;
  NSURLVolumeIsJournalingKey: NSString; cvar; external;
  NSURLVolumeSupportsSparseFilesKey: NSString; cvar; external;
  NSURLVolumeSupportsZeroRunsKey: NSString; cvar; external;
  NSURLVolumeSupportsCaseSensitiveNamesKey: NSString; cvar; external;
  NSURLVolumeSupportsCasePreservedNamesKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURL = objcclass;
  NSURLPointer = ^NSURL;
  NSURLPtr = NSURLPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURL_PAS_C}
{$define NSURL_PAS_C}

{ NSURL }
  NSURL = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol, NSURLHandleClientProtocol)
  private
    _urlString: NSString;
    _baseURL: NSURL;
    _clients: Pointer;
    _reserved: Pointer; {garbage collector: __strong }
    
  public
    function initWithScheme_host_path(scheme_: NSString; host_: NSString; path_: NSString): id; message 'initWithScheme:host:path:';
    function initFileURLWithPath_isDirectory(path_: NSString; isDir: Boolean): id; message 'initFileURLWithPath:isDirectory:';
    function initFileURLWithPath(path_: NSString): id; message 'initFileURLWithPath:';
    class function fileURLWithPath_isDirectory(path_: NSString; isDir: Boolean): id; message 'fileURLWithPath:isDirectory:';
    class function fileURLWithPath(path_: NSString): id; message 'fileURLWithPath:';
    class function URLWithString(URLString: NSString): id; message 'URLWithString:';
    class function URLWithString_relativeToURL(URLString: NSString; baseURL_: NSURL): id; message 'URLWithString:relativeToURL:';
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
    function isFileURL: Boolean; message 'isFileURL';
    function standardizedURL: NSURL; message 'standardizedURL';
    function getResourceValue_forKey_error(value: idPtr; key: NSString; error: NSErrorPointer): Boolean; message 'getResourceValue:forKey:error:';
    function resourceValuesForKeys_error(keys: NSArray; error: NSErrorPointer): NSDictionary; message 'resourceValuesForKeys:error:';
    function setResourceValue_forKey_error(value: id; key: NSString; error: NSErrorPointer): Boolean; message 'setResourceValue:forKey:error:';
    function setResourceValues_error(keyedValues: NSDictionary; error: NSErrorPointer): Boolean; message 'setResourceValues:error:';
    function checkResourceIsReachableAndReturnError(error: NSErrorPointer): Boolean; message 'checkResourceIsReachableAndReturnError:';
    function isFileReferenceURL: Boolean; message 'isFileReferenceURL';
    function fileReferenceURL: NSURL; message 'fileReferenceURL';
    function filePathURL: NSURL; message 'filePathURL';
    function bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_error(options: NSURLBookmarkCreationOptions; keys: NSArray; relativeURL: NSURL; error: NSErrorPointer): NSData; message 'bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:';
    function initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error(bookmarkData: NSData; options: NSURLBookmarkResolutionOptions; relativeURL: NSURL; isStale: pboolean; error: NSErrorPointer): id; message 'initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:';
    class function URLByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error(bookmarkData: NSData; options: NSURLBookmarkResolutionOptions; relativeURL: NSURL; isStale: pboolean; error: NSErrorPointer): id; message 'URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:';
    class function resourceValuesForKeys_fromBookmarkData(keys: NSArray; bookmarkData: NSData): NSDictionary; message 'resourceValuesForKeys:fromBookmarkData:';
    class function writeBookmarkData_toURL_options_error(bookmarkData: NSData; bookmarkFileURL: NSURL; options: NSURLBookmarkFileCreationOptions; error: NSErrorPointer): Boolean; message 'writeBookmarkData:toURL:options:error:';
    class function bookmarkDataWithContentsOfURL_error(bookmarkFileURL: NSURL; error: NSErrorPointer): NSData; message 'bookmarkDataWithContentsOfURL:error:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
    procedure URLHandle_resourceDataDidBecomeAvailable(sender: NSURLHandle; newBytes: NSData);
    procedure URLHandleResourceDidBeginLoading(sender: NSURLHandle);
    procedure URLHandleResourceDidFinishLoading(sender: NSURLHandle);
    procedure URLHandleResourceDidCancelLoading(sender: NSURLHandle);
    procedure URLHandle_resourceDidFailLoadingWithReason(sender: NSURLHandle; reason: NSString);
  end;

{ NSURLUtilitiesCategory }
  NSURLUtilitiesCategory = objccategory external (NSString)
    function stringByAddingPercentEscapesUsingEncoding(enc: NSStringEncoding): NSString; message 'stringByAddingPercentEscapesUsingEncoding:';
    function stringByReplacingPercentEscapesUsingEncoding(enc: NSStringEncoding): NSString; message 'stringByReplacingPercentEscapesUsingEncoding:';
  end;

{ NSURLClientCategory }
  NSURLClientCategory = objccategory external (NSObject)
    procedure URL_resourceDataDidBecomeAvailable(sender: NSURL; newBytes: NSData); message 'URL:resourceDataDidBecomeAvailable:'; deprecated 'in Mac OS X 10.4 and later';
    procedure URLResourceDidFinishLoading(sender: NSURL); message 'URLResourceDidFinishLoading:'; deprecated 'in Mac OS X 10.4 and later';
    procedure URLResourceDidCancelLoading(sender: NSURL); message 'URLResourceDidCancelLoading:'; deprecated 'in Mac OS X 10.4 and later';
    procedure URL_resourceDidFailLoadingWithReason(sender: NSURL; reason: NSString); message 'URL:resourceDidFailLoadingWithReason:'; deprecated 'in Mac OS X 10.4 and later';
  end;

{ NSURLPathUtilitiesCategory }
  NSURLPathUtilitiesCategory = objccategory external (NSURL)
    class function fileURLWithPathComponents(components: NSArray): NSURL; message 'fileURLWithPathComponents:';
    function pathComponents: NSArray; message 'pathComponents';
    function lastPathComponent: NSString; message 'lastPathComponent';
    function pathExtension: NSString; message 'pathExtension';
    function URLByAppendingPathComponent(pathComponent: NSString): NSURL; message 'URLByAppendingPathComponent:';
    function URLByDeletingLastPathComponent: NSURL; message 'URLByDeletingLastPathComponent';
    function URLByAppendingPathExtension(pathExtension_: NSString): NSURL; message 'URLByAppendingPathExtension:';
    function URLByDeletingPathExtension: NSURL; message 'URLByDeletingPathExtension';
    function URLByStandardizingPath: NSURL; message 'URLByStandardizingPath';
    function URLByResolvingSymlinksInPath: NSURL; message 'URLByResolvingSymlinksInPath';
  end;

{$endif}
{$endif}

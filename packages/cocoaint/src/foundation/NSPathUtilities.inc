{ Parsed from Foundation.framework NSPathUtilities.h }

{$ifdef TYPES}
{$ifndef NSPATHUTILITIES_PAS_T}
{$define NSPATHUTILITIES_PAS_T}

{ Constants }

const
  NSApplicationDirectory = 1;
  NSDemoApplicationDirectory = 2;
  NSDeveloperApplicationDirectory = 3;
  NSAdminApplicationDirectory = 4;
  NSLibraryDirectory = 5;
  NSDeveloperDirectory = 6;
  NSUserDirectory = 7;
  NSDocumentationDirectory = 8;
  NSDocumentDirectory = 9;
  NSCoreServiceDirectory = 10;
  NSAutosavedInformationDirectory = 11;
  NSDesktopDirectory = 12;
  NSCachesDirectory = 13;
  NSApplicationSupportDirectory = 14;
  NSDownloadsDirectory = 15;
  NSInputMethodsDirectory = 16;
  NSMoviesDirectory = 17;
  NSMusicDirectory = 18;
  NSPicturesDirectory = 19;
  NSPrinterDescriptionDirectory = 20;
  NSSharedPublicDirectory = 21;
  NSPreferencePanesDirectory = 22;
  NSItemReplacementDirectory = 99;
  NSAllApplicationsDirectory = 100;
  NSAllLibrariesDirectory = 101;

const
  NSUserDomainMask = 1;
  NSLocalDomainMask = 2;
  NSNetworkDomainMask = 4;
  NSSystemDomainMask = 8;
  NSAllDomainsMask = $0ffff;

{ Types }
type
  NSSearchPathDirectory = NSUInteger;
  NSSearchPathDirectoryPtr = ^NSSearchPathDirectory;
  NSSearchPathDomainMask = NSUInteger;
  NSSearchPathDomainMaskPtr = ^NSSearchPathDomainMask;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPATHUTILITIES_PAS_R}
{$define NSPATHUTILITIES_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPATHUTILITIES_PAS_F}
{$define NSPATHUTILITIES_PAS_F}

{ Functions }
function NSUserName: NSString; cdecl; external;
function NSFullUserName: NSString; cdecl; external;
function NSHomeDirectory: NSString; cdecl; external;
function NSHomeDirectoryForUser(userName: NSString): NSString; cdecl; external;
function NSTemporaryDirectory: NSString; cdecl; external;
function NSOpenStepRootDirectory: NSString; cdecl; external;
function NSSearchPathForDirectoriesInDomains(directory: NSSearchPathDirectory; domainMask: NSSearchPathDomainMask; expandTilde: Boolean): NSArray; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPATHUTILITIES_PAS_S}
{$define NSPATHUTILITIES_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSPATHUTILITIES_PAS_C}
{$define NSPATHUTILITIES_PAS_C}

{ NSStringPathExtensionsCategory }
  NSStringPathExtensionsCategory = objccategory external (NSString)
    class function pathWithComponents(components: NSArray): NSString; message 'pathWithComponents:';
    function pathComponents: NSArray; message 'pathComponents';
    function isAbsolutePath: Boolean; message 'isAbsolutePath';
    function lastPathComponent: NSString; message 'lastPathComponent';
    function stringByDeletingLastPathComponent: NSString; message 'stringByDeletingLastPathComponent';
    function stringByAppendingPathComponent(str: NSString): NSString; message 'stringByAppendingPathComponent:';
    function pathExtension: NSString; message 'pathExtension';
    function stringByDeletingPathExtension: NSString; message 'stringByDeletingPathExtension';
    function stringByAppendingPathExtension(str: NSString): NSString; message 'stringByAppendingPathExtension:';
    function stringByAbbreviatingWithTildeInPath: NSString; message 'stringByAbbreviatingWithTildeInPath';
    function stringByExpandingTildeInPath: NSString; message 'stringByExpandingTildeInPath';
    function stringByStandardizingPath: NSString; message 'stringByStandardizingPath';
    function stringByResolvingSymlinksInPath: NSString; message 'stringByResolvingSymlinksInPath';
    function stringsByAppendingPaths(paths: NSArray): NSArray; message 'stringsByAppendingPaths:';
    function completePathIntoString_caseSensitive_matchesIntoArray_filterTypes(outputName: NSStringPointer; flag: Boolean; outputArray: NSArrayPointer; filterTypes: NSArray): NSUInteger; message 'completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:';
    function fileSystemRepresentation: PChar; message 'fileSystemRepresentation';
    function getFileSystemRepresentation_maxLength(cname: PChar; max: NSUInteger): Boolean; message 'getFileSystemRepresentation:maxLength:';
  end;

{ NSArrayPathExtensionsCategory }
  NSArrayPathExtensionsCategory = objccategory external (NSArray)
    function pathsMatchingExtensions(filterTypes: NSArray): NSArray; message 'pathsMatchingExtensions:';
  end;

{$endif}
{$endif}

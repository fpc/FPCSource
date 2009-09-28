{ Parsed from Foundation.framework NSPathUtilities.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPATHUTILITIES_PAS_T}
{$define NSPATHUTILITIES_PAS_T}

{ Constants }

const
  NSApplicationDirectory = 1;
  NSDemoApplicationDirectory = 0;
  NSDeveloperApplicationDirectory = 1;
  NSAdminApplicationDirectory = 2;
  NSLibraryDirectory = 3;
  NSDeveloperDirectory = 4;
  NSUserDirectory = 5;
  NSDocumentationDirectory = 6;
  NSDocumentDirectory = 7;
  NSCoreServiceDirectory = 8;
  NSDesktopDirectory = 12;
  NSCachesDirectory = 13;
  NSApplicationSupportDirectory = 14;
  NSDownloadsDirectory = 15;
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
  NSSearchPathDomainMask = NSUInteger;

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
function NSUserName: NSString; cdecl; external name 'NSUserName';
function NSFullUserName: NSString; cdecl; external name 'NSFullUserName';
function NSHomeDirectory: NSString; cdecl; external name 'NSHomeDirectory';
function NSHomeDirectoryForUser(userNamePointer: NSString): NSString; cdecl; external name 'NSHomeDirectoryForUser';
function NSTemporaryDirectory: NSString; cdecl; external name 'NSTemporaryDirectory';
function NSOpenStepRootDirectory: NSString; cdecl; external name 'NSOpenStepRootDirectory';
function NSSearchPathForDirectoriesInDomains(directory: NSSearchPathDirectory; domainMask: NSSearchPathDomainMask; expandTilde: Boolean): NSArray; cdecl; external name 'NSSearchPathForDirectoriesInDomains';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPATHUTILITIES_PAS_S}
{$define NSPATHUTILITIES_PAS_S}

{$endif}
{$endif}

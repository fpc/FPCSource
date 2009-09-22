{ Parsed from Foundation.framework NSPathUtilities.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPATHUTILITIES_PAS_T}
{$define NSPATHUTILITIES_PAS_T}

{ Types }
type
  NSSearchPathDirectory = NSUInteger;
  NSSearchPathDomainMask = NSUInteger;

{ Constants }

const
  NSAllDomainsMask = $0ffff;

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
{$ifndef NSPATHUTILITIES_PAS_T}
{$define NSPATHUTILITIES_PAS_T}

{$endif}
{$endif}

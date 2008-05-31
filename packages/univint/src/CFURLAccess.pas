{	CFURLAccess.h
	Copyright (c) 1998-2005, Apple, Inc. All rights reserved.
}
{	  Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CFURLAccess;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,CFBase,CFArray,CFData,CFDictionary,CFString,CFURL;
{$ALIGN POWER}


{ Attempts to read the data and properties for the given URL.  If
only interested in one of the resourceData and properties, pass NULL
for the other.  If properties is non-NULL and desiredProperties is
NULL, then all properties are fetched.  Returns success or failure;
note that as much work as possible is done even if false is returned.
So for instance if one property is not available, the others are
fetched anyway. errorCode is set to 0 on success, and some other
value on failure.  If non-NULL, it is the caller 's responsibility
to release resourceData and properties.

    Apple reserves for its use all negative error code values; these
values represent errors common to any scheme.  Scheme-specific error
codes should be positive, non-zero, and should be used only if one of
the predefined Apple error codes does not apply.  Error codes should
be publicized and documented with the scheme-specific properties.

NOTE: When asking for the resource data, this call will allocate the entire
resource in memory. This can be very expensive, depending on the size of the
resource (file). Please use CFStream or other techniques if you are downloading
large files.

}
function CFURLCreateDataAndPropertiesFromResource( alloc: CFAllocatorRef; url: CFURLRef; resourceData: CFDataRefPtr; properties: CFDictionaryRefPtr; desiredProperties: CFArrayRef; var errorCode: SInt32 ): Boolean; external name '_CFURLCreateDataAndPropertiesFromResource';

{ Attempts to write the given data and properties to the given URL.
If dataToWrite is NULL, only properties are written out (use
CFURLDestroyResource() to delete a resource).  Properties not present
in propertiesToWrite are left unchanged, hence if propertiesToWrite
is NULL or empty, the URL's properties are not changed at all.
Returns success or failure; errorCode is set as for
CFURLCreateDataAndPropertiesFromResource(), above.
}
function CFURLWriteDataAndPropertiesToResource( url: CFURLRef; dataToWrite: CFDataRef; propertiesToWrite: CFDictionaryRef; var errorCode: SInt32 ): Boolean; external name '_CFURLWriteDataAndPropertiesToResource';

{ Destroys the resource indicated by url. }
{ Returns success or failure; errorCode set as above. }
function CFURLDestroyResource( url: CFURLRef; var errorCode: SInt32 ): Boolean; external name '_CFURLDestroyResource';

{ Convenience method which calls through to CFURLCreateDataAndPropertiesFromResource(). }
{ Returns NULL on error and sets errorCode accordingly. }
function CFURLCreatePropertyFromResource( alloc: CFAllocatorRef; url: CFURLRef; proprty: CFStringRef; var errorCode: SInt32 ): CFTypeRef; external name '_CFURLCreatePropertyFromResource';

{ Common error codes; this list is expected to grow }
type
	CFURLError = SInt32;
const
	kCFURLUnknownError = -10;
	kCFURLUnknownSchemeError = -11;
	kCFURLResourceNotFoundError = -12;
	kCFURLResourceAccessViolationError = -13;
	kCFURLRemoteHostUnavailableError = -14;
	kCFURLImproperArgumentsError = -15;
	kCFURLUnknownPropertyKeyError = -16;
	kCFURLPropertyKeyUnavailableError = -17;
	kCFURLTimeoutError = -18;

{ Property keys }

var kCFURLFileExists: CFStringRef; external name '_kCFURLFileExists'; (* attribute const *)
var kCFURLFileDirectoryContents: CFStringRef; external name '_kCFURLFileDirectoryContents'; (* attribute const *)
var kCFURLFileLength: CFStringRef; external name '_kCFURLFileLength'; (* attribute const *)
var kCFURLFileLastModificationTime: CFStringRef; external name '_kCFURLFileLastModificationTime'; (* attribute const *)
var kCFURLFilePOSIXMode: CFStringRef; external name '_kCFURLFilePOSIXMode'; (* attribute const *)
var kCFURLFileOwnerID: CFStringRef; external name '_kCFURLFileOwnerID'; (* attribute const *)
var kCFURLHTTPStatusCode: CFStringRef; external name '_kCFURLHTTPStatusCode'; (* attribute const *)
var kCFURLHTTPStatusLine: CFStringRef; external name '_kCFURLHTTPStatusLine'; (* attribute const *)

{ The value of kCFURLFileExists is a CFBoolean }
{ The value of kCFURLFileDirectoryContents is a CFArray containing CFURLs.  An empty array means the directory exists, but is empty }
{ The value of kCFURLFileLength is a CFNumber giving the file's length in bytes }
{ The value of kCFURLFileLastModificationTime is a CFDate }
{ The value of kCFURLFilePOSIXMode is a CFNumber as given in stat.h }
{ The value of kCFURLFileOwnerID is a CFNumber representing the owner's uid }

{ Properties for the http: scheme.  Except for the common error codes, above, errorCode will be set to the HTTP response status code upon failure.  Any HTTP header name can also be used as a property }
{ The value of kCFURLHTTPStatusCode is a CFNumber }
{ The value of kCFURLHTTPStatusLine is a CFString }


end.

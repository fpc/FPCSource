{
    CFURLEnumerator.h
    Copyright (c) 2008-2013, Apple Inc. All rights reserved.
 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CFURLEnumerator;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,CFBase,CFArray,CFError,CFURL;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{ #if (TARGET_OS_MAC || TARGET_OS_EMBEDDED || TARGET_OS_IPHONE) || CF_BUILDING_CF || NSBUILDINGFOUNDATION }


type
	CFURLEnumeratorRef = ^__CFURLEnumerator; { an opaque type }
	__CFURLEnumerator = record end;

{ CFURLEnumeratorGetTypeID - Returns the CFURLEnumerator CFTypeID. }
function CFURLEnumeratorGetTypeID: CFTypeID; external name '_CFURLEnumeratorGetTypeID';
(* CF_AVAILABLE_STARTING(10_6, 4_0) *)


{ CFURLEnumeratorOptions - Options for controlling enumerator behavior. }
type
  CFURLEnumeratorOptions = CFOptionFlags;
const
    kCFURLEnumeratorDefaultBehavior                 = 0;        { Use the default behavior for the enumerator. }
    kCFURLEnumeratorDescendRecursively              = UNSIGNEDLONG(1) shl 0; { The directory enumerator will recurse ("depth-first") into each subdirectory enumerated }
    kCFURLEnumeratorSkipInvisibles                  = UNSIGNEDLONG(1) shl 1; { The directory or volume enumerator skips "hidden" or "invisible" objects }
    kCFURLEnumeratorGenerateFileReferenceURLs       = UNSIGNEDLONG(1) shl 2; { The volume enumerator generates file reference URLs }
    kCFURLEnumeratorSkipPackageContents             = UNSIGNEDLONG(1) shl 3; { The directory enumerator skips package directory contents }
    kCFURLEnumeratorIncludeDirectoriesPreOrder      = UNSIGNEDLONG(1) shl 4; { With this option set; a recursive directory enumerator will return directory URLs when CFURLEnumeratorGetNextURL() returns kCFURLEnumeratorSuccess before any of the directory's descendants are visited (pre-order). }
    kCFURLEnumeratorIncludeDirectoriesPostOrder     = UNSIGNEDLONG(1) shl 5; { With this option set; a recursive directory enumerator will return directory URLs when CFURLEnumeratorGetNextURL() returns kCFURLEnumeratorDirectoryPostOrderSuccess after all of directory's descendants have been visited (post-order). }
    { Note: if both kCFURLEnumeratorIncludeDirectoriesPreOrder and kCFURLEnumeratorIncludeDirectoriesPostOrder are used; directories will be seen twice (even empty directories and directories whose descendants are skipped) -- once when kCFURLEnumeratorSuccess is returned and once when kCFURLEnumeratorDirectoryPostOrderSuccess is returned. }


{ CFURLEnumeratorCreateForDirectoryURL - Creates a directory enumerator, flat or recursive. Client specifies the directory URL to enumerate, a bit array of options, and an optional array of property keys to pre-fetch for the found URLs. Specifying pre-fetch properties allows the implementation to optimize device access by using bulk operations when available. Pre-fetching more properties than are actually needed may degrade performance.

A directory enumerator generates URLs with the same type as the directory URL being enumerated. If the directoryURL input parameter is a file reference URL, then generated URLs will be file reference URLs. If the directoryURL input parameter is a file path URL, then generated URLs will be file path URLs.

The kCFURLEnumeratorGenerateFileReferenceURLs option is ignored by CFURLEnumeratorCreateForDirectoryURL.
}
function CFURLEnumeratorCreateForDirectoryURL( alloc: CFAllocatorRef; directoryURL: CFURLRef; option: CFURLEnumeratorOptions; propertyKeys: CFArrayRef ): CFURLEnumeratorRef; external name '_CFURLEnumeratorCreateForDirectoryURL';
(* CF_AVAILABLE_STARTING(10_6, 4_0) *)


{ CFURLEnumeratorCreateForMountedVolumes - Creates an enumerator for mounted filesystem volumes. Client specifies an allocator, a bit array of options, and an optional array of property keys to pre-fetch for the volume URLs. Specifying pre-fetch properties allows the implementation to optimize device access by using bulk operations when available. Pre-fetching more properties than are actually needed may degrade performance.

A volume enumerator generates file path URLs. If you want a volume enumerator to generate file reference URLs, pass the kCFURLEnumeratorGenerateFileReferenceURLs option.

The kCFURLEnumeratorDescendRecursively and kCFURLEnumeratorSkipPackageContents options are ignored by CFURLEnumeratorCreateForMountedVolumes.
}
function CFURLEnumeratorCreateForMountedVolumes( alloc: CFAllocatorRef; option: CFURLEnumeratorOptions; propertyKeys: CFArrayRef ): CFURLEnumeratorRef; external name '_CFURLEnumeratorCreateForMountedVolumes';
(* CF_AVAILABLE_STARTING(10_6, 4_0) *)


{ CFURLEnumeratorResult - Results for CFURLEnumeratorGetNextURL }
type
  CFURLEnumeratorResult = CFIndex;
const
  kCFURLEnumeratorSuccess = 1;                    { The enumeration was successful. The url output parameter is valid. }
  kCFURLEnumeratorEnd = 2;                        { The enumeration is complete. }
  kCFURLEnumeratorError = 3;			    { An error occured during enumeration. The retained error output parameter describes the error. }
  kCFURLEnumeratorDirectoryPostOrderSuccess = 4;  { The enumeration was successful. The url output parameter is for a directory after all of directory's descendants have been visited (post-order). This result will only be returned for directories when both the kCFURLEnumeratorDescendRecursively and kCFURLEnumeratorIncludeDirectoriesPostOrder options are passed to CFURLEnumeratorCreateForDirectoryURL. }

{ CFURLEnumeratorGetNextURL - Advances the enumerator. If kCFURLEnumeratorSuccess is returned, the url output parameter returns the next URL found. If kCFURLEnumeratorError is returned, an error has occured and the error output parameter describes the error. If kCFURLEnumeratorEnd, the enumeration is finished.

The url output parameter, if returned, is not retained. The error output parameter, if returned, is retained and must be released.
}
function CFURLEnumeratorGetNextURL( enumerator: CFURLEnumeratorRef; var url: CFURLRef; var error: CFErrorRef ): CFURLEnumeratorResult; external name '_CFURLEnumeratorGetNextURL';
(* CF_AVAILABLE_STARTING(10_6, 4_0) *)


{ CFURLEnumeratorSkipDescendents - Tells a recursive CFURLEnumerator not to descend into the directory of the last CFURLRef returned by CFURLEnumeratorGetNextURL.

Calls to CFURLEnumeratorSkipDescendents are ignored if:
    * CFURLEnumeratorGetNextURL has never been called with the CFURLEnumerator.
    * The last CFURL returned by CFURLEnumeratorGetNextURL is not a directory.
    * The CFURLEnumerator was not created with CFURLEnumeratorCreateForDirectoryURL using the kCFURLEnumeratorDescendRecursively option.
}
procedure CFURLEnumeratorSkipDescendents( enumerator: CFURLEnumeratorRef ); external name '_CFURLEnumeratorSkipDescendents';
(* CF_AVAILABLE_STARTING(10_6, 4_0) *)


{ CFURLEnumeratorGetDescendentLevel - Returns the number of levels a directory enumerator has descended down into the directory hierarchy from the starting directory. The children of the starting directory are at level 1. Each time a recursive enumerator descends into a subdirectory, it adds one to the descendent level. It then subtracts one from the level when it finishes a subdirectory and continues enumerating the parent directory.
}
function CFURLEnumeratorGetDescendentLevel( enumerator: CFURLEnumeratorRef ): CFIndex; external name '_CFURLEnumeratorGetDescendentLevel';
(* CF_AVAILABLE_STARTING(10_6, 4_0) *)


{ CFURLEnumeratorGetSourceDidChange is deprecated. If your program is interested in directory hierarchy changes during enumeration (and most programs are not interested), you should use the File System Events API.
 
 CFURLEnumeratorGetSourceDidChange does nothing and always returns false.
 }
function CFURLEnumeratorGetSourceDidChange( enumerator: CFURLEnumeratorRef ): Boolean; external name '_CFURLEnumeratorGetSourceDidChange';
(* CF_DEPRECATED(10_6, 10_7, 4_0, 5_0) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

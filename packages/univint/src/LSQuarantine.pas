{
     File:       LSQuarantine.h
 
     Contains:   File quarantine property keys
 
     Copyright:  Copyright 2003-2009 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
}
{	 Pascal Translation: Gorazd Krosl <gorazd_1957@yahoo.ca>, October 2009 }
{	 Updated Pascal Translation: Jonas Maebe <jonas@freepascal.org>, September 2012 }

{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit LSQuarantine;
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
{$ifc defined(iphonesim)}
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes,CFBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}


{$ALIGN POWER}


{
 *    The following keys may appear in a quarantine properties dictionary. To access a
 *    file's quarantine properties dictionary, fetch the kLSItemQuarantineProperties
 *    attribute using LSCopyItemAttribute() or LSCopyItemAttributes(). 
 *    The quarantine property dictionary can be set using LSSetItemAttribute().
 *
 *
 * kLSQuarantineAgentNameKey - value type CFStringRef
 *
 *    The name of the quarantining agent (application or program). When setting quarantine
 *    properties, this value is set automatically to the current process name if this key is not 
 *    present in the caller's dictionary.
 *
 *
 * kLSQuarantineAgentBundleIdentifierKey - value type CFStringRef
 *
 *    The bundle identifier of the quarantining agent, if available. When setting quarantine 
 *    properties, this value is set automatically if the key is not present in the caller's 
 *    dictionary. The automatic value is the main bundle identifier of the current process.
 *
 *
 * kLSQuarantineTimeStampKey - value type CFDateRef
 *
 *    The date and time the item was quarantined. When setting quarantine properties,
 *    this property is set automatically to the current date and time if this key is not present 
 *    in the caller's dictionary.
 *
 *
 * kLSQuarantineTypeKey - value type CFStringRef
 *
 *    A symbolic string identifying the why the item is quarantined, if available. 
 *    The value is one of the following:
 *
 *    kLSQuarantineTypeWebDownload
 *    kLSQuarantineTypeOtherDownload
 *    kLSQuarantineTypeEmailAttachment
 *    kLSQuarantineTypeInstantMessageAttachment
 *    kLSQuarantineTypeCalendarEventAttachment
 *    kLSQuarantineTypeOtherAttachment
 *
 *
 * kLSQuarantineDataURLKey - value type CFURLRef
 *
 *    The URL from which the data for the quarantined item data was actaully streamed
 *    or downloaded, if available.
 *
 *
 * kLSQuarantineOriginURLKey - value type CFURLRef
 *
 *    The URL of the resource originally hosting the quarantined item, from the user's point of
 *    view. For web downloads, this property is the URL of the web page on which the user initiated
 *    the download. For attachments, this property is the URL of the resource to which the quarantined
 *    item was attached (e.g. the email message, calendar event, etc.). The origin URL may be a file URL
 *    for local resources, or a custom URL to which the quarantining application will respond when asked 
 *    to open it. The quarantining application should respond by displaying the resource to the user. 
 *    Note: The origin URL should not be set to the data URL, or the quarantining application may start 
 *    downloading the file again if the user choses to view the origin URL while resolving a quarantine 
 *    warning.
 *
 }
{
 *  kLSQuarantineAgentNameKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineAgentNameKey: CFStringRef; external name '_kLSQuarantineAgentNameKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineAgentBundleIdentifierKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineAgentBundleIdentifierKey: CFStringRef; external name '_kLSQuarantineAgentBundleIdentifierKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTimeStampKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTimeStampKey: CFStringRef; external name '_kLSQuarantineTimeStampKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTypeKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTypeKey: CFStringRef; external name '_kLSQuarantineTypeKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTypeWebDownload
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTypeWebDownload: CFStringRef; external name '_kLSQuarantineTypeWebDownload'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTypeOtherDownload
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTypeOtherDownload: CFStringRef; external name '_kLSQuarantineTypeOtherDownload'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTypeEmailAttachment
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTypeEmailAttachment: CFStringRef; external name '_kLSQuarantineTypeEmailAttachment'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTypeInstantMessageAttachment
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTypeInstantMessageAttachment: CFStringRef; external name '_kLSQuarantineTypeInstantMessageAttachment'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTypeCalendarEventAttachment
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTypeCalendarEventAttachment: CFStringRef; external name '_kLSQuarantineTypeCalendarEventAttachment'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineTypeOtherAttachment
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineTypeOtherAttachment: CFStringRef; external name '_kLSQuarantineTypeOtherAttachment'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineOriginURLKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineOriginURLKey: CFStringRef; external name '_kLSQuarantineOriginURLKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSQuarantineDataURLKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSQuarantineDataURLKey: CFStringRef; external name '_kLSQuarantineDataURLKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

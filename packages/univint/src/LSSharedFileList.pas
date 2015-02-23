{
     File:       LSSharedFileList.h
 
     Contains:   Services to load and share file lists.
 
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

unit LSSharedFileList;
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
uses MacTypes,CFBase,CFArray,CFDictionary,CFURL,CFRunLoop,Files,IconsCore,Authorization;
{$endc} {not MACOSALLINCLUDE}



{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{ The shared file list API is for sharing and storing list of references to file system objects.
   The shared file list is a persistent list of objects, where each item has assigned display name, icon, and url
   as well as other optional properties.

   Each list can also have various properties attached.
}
type
	LSSharedFileListRef = ^OpaqueLSSharedFileListRef; { an opaque type }
	OpaqueLSSharedFileListRef = record end;
	LSSharedFileListItemRef = ^OpaqueLSSharedFileListItemRef; { an opaque type }
	OpaqueLSSharedFileListItemRef = record end;

{ list types }

{
 *  kLSSharedFileListFavoriteVolumes
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListFavoriteVolumes: CFStringRef; external name '_kLSSharedFileListFavoriteVolumes'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  kLSSharedFileListFavoriteItems
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListFavoriteItems: CFStringRef; external name '_kLSSharedFileListFavoriteItems'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  kLSSharedFileListRecentApplicationItems
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListRecentApplicationItems: CFStringRef; external name '_kLSSharedFileListRecentApplicationItems'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  kLSSharedFileListRecentDocumentItems
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListRecentDocumentItems: CFStringRef; external name '_kLSSharedFileListRecentDocumentItems'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  kLSSharedFileListRecentServerItems
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListRecentServerItems: CFStringRef; external name '_kLSSharedFileListRecentServerItems'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  kLSSharedFileListSessionLoginItems
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListSessionLoginItems: CFStringRef; external name '_kLSSharedFileListSessionLoginItems'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  kLSSharedFileListGlobalLoginItems
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListGlobalLoginItems: CFStringRef; external name '_kLSSharedFileListGlobalLoginItems'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{ LSSharedFileList property keys }


{
 *  kLSSharedFileListRecentItemsMaxAmount
 *  
 *  Discussion:
 *    maximum amount of items in the list.  Associated property is
 *    CFNumber
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListRecentItemsMaxAmount: CFStringRef; external name '_kLSSharedFileListRecentItemsMaxAmount'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{
 *  kLSSharedFileListVolumesComputerVisible
 *  
 *  Discussion:
 *    is Computer item visible in favorite volumes list. Associated
 *    property is CFBoolean.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListVolumesComputerVisible: CFStringRef; external name '_kLSSharedFileListVolumesComputerVisible'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{
 *  kLSSharedFileListVolumesIDiskVisible
 *  
 *  Discussion:
 *    is iDisk item visible in favorite volumes list. Associated
 *    property is CFBoolean.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListVolumesIDiskVisible: CFStringRef; external name '_kLSSharedFileListVolumesIDiskVisible'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  kLSSharedFileListVolumesNetworkVisible
 *  
 *  Discussion:
 *    is Network item visible in favorite volumes list. Associated
 *    property is CFBoolean.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListVolumesNetworkVisible: CFStringRef; external name '_kLSSharedFileListVolumesNetworkVisible'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{ item default positions }


{
 *  kLSSharedFileListItemBeforeFirst
 *  
 *  Discussion:
 *    A virtual item reference for inserting new item at beginning of
 *    the list.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListItemBeforeFirst: LSSharedFileListItemRef; external name '_kLSSharedFileListItemBeforeFirst'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{
 *  kLSSharedFileListItemLast
 *  
 *  Discussion:
 *    A virtual item reference for inserting new item at end of the
 *    list.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListItemLast: LSSharedFileListItemRef; external name '_kLSSharedFileListItemLast'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{ LSSharedFileListItem property keys }


{
 *  kLSSharedFileListItemHidden
 *  
 *  Discussion:
 *    Is item hidden in UI? Associated property is CFBoolean.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListItemHidden: CFStringRef; external name '_kLSSharedFileListItemHidden'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{
 *  kLSSharedFileListLoginItemHidden
 *  
 *  Discussion:
 *    Should UI hide login item's window? Associated property is
 *    CFBoolean.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.6 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kLSSharedFileListLoginItemHidden: CFStringRef; external name '_kLSSharedFileListLoginItemHidden'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)

{ LSSharedFileListItemResolve flags }
const
	kLSSharedFileListNoUserInteraction = 1 shl 0; { no user interaction during resolution }
	kLSSharedFileListDoNotMountVolumes = 1 shl 1; { do not mount volumes during resolution }


{
 *  LSSharedFileListChangedProcPtr
 *  
 *  Discussion:
 *    callback to use with LSSharedFileListAddObserver and
 *    LSSharedFileListRemoveObserver.
 }
type
	LSSharedFileListChangedProcPtr = procedure( inList: LSSharedFileListRef; context: UnivPtr );

{
 *  LSSharedFileListGetTypeID()
 *  
 *  Discussion:
 *    This routine will return the CFTypeID for the LSSharedFileListRef
 *    type.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Result:
 *    CFTypeID for the LSSharedFileListRef type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListGetTypeID: CFTypeID; external name '_LSSharedFileListGetTypeID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemGetTypeID()
 *  
 *  Discussion:
 *    This routine will return the CFTypeID for the
 *    LSSharedFileListItemRef type.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Result:
 *    CFTypeID for the LSSharedFileListItemRef type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemGetTypeID: CFTypeID; external name '_LSSharedFileListItemGetTypeID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListCreate()
 *  
 *  Summary:
 *    Create shared file list reference.
 *  
 *  Discussion:
 *    Creates shared file list reference to be used for changing list
 *    and reading its various properties.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inAllocator:
 *      CFAllocatorRef used to allocate the LSSharedFileListRef object.
 *      As usual, NULL means default allocator.
 *    
 *    inListType:
 *      A constant indicating list type to create (for example
 *      kLSSharedFileListSessionLoginItems).
 *    
 *    listOptions:
 *      Additional parameters for the list type (as applicable). NULL
 *      means no options.
 *  
 *  Result:
 *    A reference to created shared file list object or NULL if
 *    creation failed. Has to be released with CFRelease when list
 *    object is not needed anymore.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListCreate( inAllocator: CFAllocatorRef; inListType: CFStringRef; listOptions: CFTypeRef ): LSSharedFileListRef; external name '_LSSharedFileListCreate';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListSetAuthorization()
 *  
 *  Summary:
 *    Set authorization reference for the shared list.
 *  
 *  Discussion:
 *    Sets authorization reference for the shared list. Before
 *    attempting to perform a privileged operation on the shared list
 *    caller must authorize appropriate rights. For example, modifying
 *    kLSSharedFileListGlobalLoginItems list requires
 *    "system.global-login-items." right authorized.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    inAuthorization:
 *      Authorization reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListSetAuthorization( inList: LSSharedFileListRef; inAuthorization: AuthorizationRef ): OSStatus; external name '_LSSharedFileListSetAuthorization';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListAddObserver()
 *  
 *  Summary:
 *    Add observer of shared list changes.
 *  
 *  Discussion:
 *    Adds observer of shared list changes. The provided function will
 *    be called when the list has changed (or any item property has
 *    changed).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    inRunloop:
 *      Runloop to run on.
 *    
 *    inRunloopMode:
 *      Mode for runloop.
 *    
 *    callback:
 *      Function to call when list has changed.
 *    
 *    context:
 *      Context pointer defined by client.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure LSSharedFileListAddObserver( inList: LSSharedFileListRef; inRunloop: CFRunLoopRef; inRunloopMode: CFStringRef; callback: LSSharedFileListChangedProcPtr; context: UnivPtr ); external name '_LSSharedFileListAddObserver';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListRemoveObserver()
 *  
 *  Summary:
 *    Remove observer of shared list changes.
 *  
 *  Discussion:
 *    Removes observer of shared list changes.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    inRunloop:
 *      Runloop to run on.
 *    
 *    inRunloopMode:
 *      Mode for runloop.
 *    
 *    callback:
 *      Function to call when list has changed.
 *    
 *    context:
 *      Context pointer defined by client.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure LSSharedFileListRemoveObserver( inList: LSSharedFileListRef; inRunloop: CFRunLoopRef; inRunloopMode: CFStringRef; callback: LSSharedFileListChangedProcPtr; context: UnivPtr ); external name '_LSSharedFileListRemoveObserver';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListGetSeedValue()
 *  
 *  Summary:
 *    Return current seed value.
 *  
 *  Discussion:
 *    Returns seed value of the shared list.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListGetSeedValue( inList: LSSharedFileListRef ): UInt32; external name '_LSSharedFileListGetSeedValue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListCopyProperty()
 *  
 *  Summary:
 *    Return property by its name.
 *  
 *  Discussion:
 *    Returns lists named property as CFTypeRef object.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    inPropertyName:
 *      Name of the property to return.
 *  
 *  Result:
 *    CFTypeRef containing requested property. NULL means list has no
 *    property with this name. Has to be released with CFRelease when
 *    property object is not needed anymore.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListCopyProperty( inList: LSSharedFileListRef; inPropertyName: CFStringRef ): CFTypeRef; external name '_LSSharedFileListCopyProperty';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListSetProperty()
 *  
 *  Summary:
 *    Set property by its name.
 *  
 *  Discussion:
 *    Set lists named property as CFTypeRef object.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    inPropertyName:
 *      Name of the property to return.
 *    
 *    inPropertyData:
 *      Property data to set. Pass NULL to remove existing property.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListSetProperty( inList: LSSharedFileListRef; inPropertyName: CFStringRef; inPropertyData: CFTypeRef ): OSStatus; external name '_LSSharedFileListSetProperty';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListCopySnapshot()
 *  
 *  Summary:
 *    Create snapshot array.
 *  
 *  Discussion:
 *    Creates snapshot array, which is list of all items at the moment
 *    LSSharedFileListCopySnapshot was called.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    outSnapshotSeed:
 *      Returned seed value at which snapshot was taken.
 *  
 *  Result:
 *    Immutable CFArray of LSSharedFileListItemRef values. Has to be
 *    released with CFRelease when snapshot array is not needed anymore.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListCopySnapshot( inList: LSSharedFileListRef; var outSnapshotSeed: UInt32 ): CFArrayRef; external name '_LSSharedFileListCopySnapshot';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListInsertItemURL()
 *  
 *  Summary:
 *    Insert item into shared list.
 *  
 *  Discussion:
 *    Inserts item into shared list at specified location. If the item
 *    already exists in the list it will be moved and its icon, display
 *    name and properties will be updated.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    insertAfterThisItem:
 *      Item after which new item has to be inserted. To insert at the
 *      beginning of the list use kLSSharedFileListItemBeforeFirst or
 *      to insert at the end of the list use kLSSharedFileListItemLast.
 *    
 *    inDisplayName:
 *      Display name of the new item. Can be NULL.
 *    
 *    inIconRef:
 *      Icon of the new item. Can be NULL.
 *    
 *    inURL:
 *      URL of the new item.
 *    
 *    inPropertiesToSet:
 *      CFDictionary of properties to set. Keys are property names and
 *      values are property values. The property values are retained
 *      and copied by the API. Can be NULL.
 *    
 *    inPropertiesToClear:
 *      CFArray of property names to clear if item already exists. Can
 *      be NULL.
 *  
 *  Result:
 *    Reference to new item. Has to be released with CFRelease when the
 *    item is not needed anymore.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListInsertItemURL( inList: LSSharedFileListRef; insertAfterThisItem: LSSharedFileListItemRef; inDisplayName: CFStringRef; inIconRef: IconRef; inURL: CFURLRef; inPropertiesToSet: CFDictionaryRef; inPropertiesToClear: CFArrayRef ): LSSharedFileListItemRef; external name '_LSSharedFileListInsertItemURL';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListInsertItemFSRef()
 *  
 *  Summary:
 *    Insert item into shared list.
 *  
 *  Discussion:
 *    Inserts item into shared list at specified location. If the item
 *    already exists in the list it will be moved and its icon, display
 *    name and properties will be updated.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    insertAfterThisItem:
 *      Item after which new item has to be inserted. To insert at the
 *      beginning of the list use kLSSharedFileListItemBeforeFirst or
 *      to insert at the end of the list use kLSSharedFileListItemLast.
 *    
 *    inDisplayName:
 *      Display name of the new item. Can be NULL.
 *    
 *    inIconRef:
 *      Icon of the new item. Can be NULL.
 *    
 *    inFSRef:
 *      FSRef of the new item.
 *    
 *    inPropertiesToSet:
 *      CFDictionary of properties to set. Keys are property names and
 *      values are property values. The property values are retained
 *      and copied by the API. Can be NULL.
 *    
 *    inPropertiesToClear:
 *      CFArray of property names to clear if item already exists. Can
 *      be NULL.
 *  
 *  Result:
 *    Reference to new item. Has to be released with CFRelease when the
 *    item is not needed anymore.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListInsertItemFSRef( inList: LSSharedFileListRef; insertAfterThisItem: LSSharedFileListItemRef; inDisplayName: CFStringRef; inIconRef: IconRef; const (*var*) inFSRef: FSRef; inPropertiesToSet: CFDictionaryRef; inPropertiesToClear: CFArrayRef ): LSSharedFileListItemRef; external name '_LSSharedFileListInsertItemFSRef';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemMove()
 *  
 *  Summary:
 *    Move item.
 *  
 *  Discussion:
 *    Moves item at specified location.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    inItem:
 *      Item to move.
 *    
 *    inMoveAfterItem:
 *      New icon of the new item. Use kLSSharedFileListItemBeforeFirst
 *      and kLSSharedFileListItemLast to move at the beginning or the
 *      end of the shared list.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemMove( inList: LSSharedFileListRef; inItem: LSSharedFileListItemRef; inMoveAfterItem: LSSharedFileListItemRef ): OSStatus; external name '_LSSharedFileListItemMove';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemRemove()
 *  
 *  Summary:
 *    Remove item from shared list.
 *  
 *  Discussion:
 *    Removes item from shared list.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list reference.
 *    
 *    inItem:
 *      Item to remove.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemRemove( inList: LSSharedFileListRef; inItem: LSSharedFileListItemRef ): OSStatus; external name '_LSSharedFileListItemRemove';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListRemoveAllItems()
 *  
 *  Summary:
 *    Remove all items from shared list.
 *  
 *  Discussion:
 *    Removes all items from shared list.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inList:
 *      Shared list to clear.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListRemoveAllItems( inList: LSSharedFileListRef ): OSStatus; external name '_LSSharedFileListRemoveAllItems';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemGetID()
 *  
 *  Summary:
 *    Obtain unique item id.
 *  
 *  Discussion:
 *    Returns unique item id.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemGetID( inItem: LSSharedFileListItemRef ): UInt32; external name '_LSSharedFileListItemGetID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemCopyIconRef()
 *  
 *  Summary:
 *    Obtain item's icon.
 *  
 *  Discussion:
 *    Returns icon ref for the item.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Result:
 *    IconRef to the item. Caller is responsible to releasing it by
 *    calling ReleaseIconRef().
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemCopyIconRef( inItem: LSSharedFileListItemRef ): IconRef; external name '_LSSharedFileListItemCopyIconRef';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemCopyDisplayName()
 *  
 *  Summary:
 *    Obtain item's display name.
 *  
 *  Discussion:
 *    Returns display name of the item. Caller is responsible to
 *    releasing it by calling CFRelease().
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemCopyDisplayName( inItem: LSSharedFileListItemRef ): CFStringRef; external name '_LSSharedFileListItemCopyDisplayName';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemResolve()
 *  
 *  Summary:
 *    Resolve LSSharedFileListItemRef's item and return its FSRef.
 *  
 *  Discussion:
 *    Resolves LSSharedFileListItemRef's item and returns its FSRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    inItem:
 *      Item to resolve.
 *    
 *    inFlags:
 *      Resolution flags. Pass zero for default resolution flags.
 *    
 *    outURL:
 *      CFURL of original item. Can be NULL. Returned valus has to be
 *      released with CFRelease().
 *    
 *    outRef:
 *      FSRef of original item. Can be NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemResolve( inItem: LSSharedFileListItemRef; inFlags: UInt32; var outURL: CFURLRef; var outRef: FSRef ): OSStatus; external name '_LSSharedFileListItemResolve';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemCopyProperty()
 *  
 *  Summary:
 *    Obtain item's property by its name.
 *  
 *  Discussion:
 *    Returns persistent item's property as CF object. Caller is
 *    responsible to releasing it by calling CFRelease().
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemCopyProperty( inItem: LSSharedFileListItemRef; inPropertyName: CFStringRef ): CFTypeRef; external name '_LSSharedFileListItemCopyProperty';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{
 *  LSSharedFileListItemSetProperty()
 *  
 *  Summary:
 *    Set item property by its name.
 *  
 *  Discussion:
 *    Sets persistent item property by its name.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function LSSharedFileListItemSetProperty( inItem: LSSharedFileListItemRef; inPropertyName: CFStringRef; inPropertyData: CFTypeRef ): OSStatus; external name '_LSSharedFileListItemSetProperty';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

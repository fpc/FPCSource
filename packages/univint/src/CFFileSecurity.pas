{
 *  CFFileSecurity.h
 *  NSFileSecurity and CFFileSecurity are toll-free bridged.
 *
 *  Copyright (c) 2010-2013, Apple Inc. All rights reserved.
 }
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

unit CFFileSecurity;
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
uses MacTypes,MacOSXPosix,CFBase,acl,CFUUID;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{ #if (TARGET_OS_MAC || TARGET_OS_EMBEDDED || TARGET_OS_IPHONE) || CF_BUILDING_CF || NSBUILDINGFOUNDATION }


{
 *	A CFFileSecurity object encapulates a file system object's security information
 *	in a CF object.
 }
type
	CFFileSecurityRef = ^__CFFileSecurity; { an opaque type }
	__CFFileSecurity = record end;

{
 *	Returns the type identifier for the CFFileSecurity opaque type.
 *
 *	Return Value
 *		The type identifier for the CFFileSecurity opaque type.
 }
function CFFileSecurityGetTypeID: CFTypeID; external name '_CFFileSecurityGetTypeID';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	Creates an CFFileSecurity object.
 *
 *	Parameters
 *		allocator
 *			The allocator to use to allocate memory for the new object. Pass
 *			NULL or kCFAllocatorDefault to use the current default allocator.
 *	Return Value
 *		A new CFFileSecurity object, or NULL if there was a problem creating the
 *		object. Ownership follows the Create Rule.
 }
function CFFileSecurityCreate( allocator: CFAllocatorRef ): CFFileSecurityRef; external name '_CFFileSecurityCreate';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *  Creates a copy of a CFFileSecurity object.
 *
 *  Parameters
 *		allocator
 *			The allocator to use to allocate memory for the new object. Pass
 *			NULL or kCFAllocatorDefault to use the current default allocator.
 *		fileSec
 *			The CFFileSecurity object to copy.
 *	Return Value
 *		A copy of fileSec, or NULL if there was a problem creating the object.
 *		Ownership follows the Create Rule.
 }
function CFFileSecurityCreateCopy( allocator: CFAllocatorRef; fileSec: CFFileSecurityRef ): CFFileSecurityRef; external name '_CFFileSecurityCreateCopy';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine copies the owner UUID associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		ownerUUID
 *			A pointer to storage for the owner UUID.
 *	Return Value
 *		true if ownerUUID is successfully returned; false if there is no owner
 *		UUID property associated with an CFFileSecurity object.
 }
function CFFileSecurityCopyOwnerUUID( fileSec: CFFileSecurityRef; var ownerUUID: CFUUIDRef ): Boolean; external name '_CFFileSecurityCopyOwnerUUID';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)

{
 *	This routine sets the owner UUID associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		ownerUUID
 *			The owner UUID.
 *	Return Value
 *		true if the owner UUID was successfully set; otherwise, false.
 }
function CFFileSecuritySetOwnerUUID( fileSec: CFFileSecurityRef; ownerUUID: CFUUIDRef ): Boolean; external name '_CFFileSecuritySetOwnerUUID';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)

{
 *	This routine copies the group UUID associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		groupUUID
 *			A pointer to storage for the group UUID.
 *	Return Value
 *		true if groupUUID is successfully returned; false if there is no group
 *		UUID property associated with an CFFileSecurity object.
 }
function CFFileSecurityCopyGroupUUID( fileSec: CFFileSecurityRef; var groupUUID: CFUUIDRef ): Boolean; external name '_CFFileSecurityCopyGroupUUID';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine sets the group UUID associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		groupUUID
 *			The group UUID.
 *	Return Value
 *		true if the group UUID was successfully set; otherwise, false.
 }
function CFFileSecuritySetGroupUUID( fileSec: CFFileSecurityRef; groupUUID: CFUUIDRef ): Boolean; external name '_CFFileSecuritySetGroupUUID';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine copies the access control list (acl_t) associated with an
 *	CFFileSecurity object. The acl_t returned by this routine is a copy and must
 *	be released using acl_free(3). The acl_t is meant to be manipulated using
 *	the acl calls defined in <sys/acl.h>.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		accessControlList
 *			A pointer to storage for an acl_t. The acl_t be released using
 *			acl_free(3)
 *	Return Value
 *		true if the access control list is successfully copied; false if there is
 *		no access control list property associated with the CFFileSecurity object.
 }
function CFFileSecurityCopyAccessControlList( fileSec: CFFileSecurityRef; var accessControlList: acl_t ): Boolean; external name '_CFFileSecurityCopyAccessControlList';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)

const kCFFileSecurityRemoveACL = acl_t(1);

{
 *	This routine will set the access control list (acl_t) associated with an
 *	CFFileSecurityRef. To request removal of an access control list from a
 *	filesystem object, pass in kCFFileSecurityRemoveACL as the acl_t and set
 *	the fileSec on the target object using CFURLSetResourcePropertyForKey and
 *	the kCFURLFileSecurityKey. Setting the accessControlList to NULL will result
 *	in the property being unset.
 *
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		accessControlList
 *			The acl_t to set, or kCFFileSecurityRemoveACL to remove the access
 *			control list, or NULL to unset the accessControlList.
 *	Return Value
 *		true if the access control list is successfully set; otherwise, false.
 }
function CFFileSecuritySetAccessControlList( fileSec: CFFileSecurityRef; accessControlList: acl_t ): Boolean; external name '_CFFileSecuritySetAccessControlList';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine gets the owner uid_t associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		owner
 *			A pointer to where the owner uid_t will be put.
 *	Return Value
 *		true if owner uid_t is successfully obtained; false if there is no owner
 *		property associated with an CFFileSecurity object.
 }
function CFFileSecurityGetOwner( fileSec: CFFileSecurityRef; var owner: uid_t ): Boolean; external name '_CFFileSecurityGetOwner';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine sets the owner uid_t associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		owner
 *			The owner uid_t.
 *	Return Value
 *		true if the owner uid_t was successfully set; otherwise, false.
 }
function CFFileSecuritySetOwner( fileSec: CFFileSecurityRef; owner: uid_t ): Boolean; external name '_CFFileSecuritySetOwner';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine gets the group gid_t associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		owner
 *			A pointer to where the group gid_t will be put.
 *	Return Value
 *		true if group gid_t is successfully obtained; false if there is no group
 *		property associated with an CFFileSecurity object.
 }
function CFFileSecurityGetGroup( fileSec: CFFileSecurityRef; var group: gid_t ): Boolean; external name '_CFFileSecurityGetGroup';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine sets the group gid_t associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		owner
 *			The group gid_t.
 *	Return Value
 *		true if the group gid_t was successfully set; otherwise, false.
 }
function CFFileSecuritySetGroup( fileSec: CFFileSecurityRef; group: gid_t ): Boolean; external name '_CFFileSecuritySetGroup';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine gets the mode_t associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		owner
 *			A pointer to where the mode_t will be put.
 *	Return Value
 *		true if mode_t is successfully obtained; false if there is no mode
 *		property associated with an CFFileSecurity object.
 }
function CFFileSecurityGetMode( fileSec: CFFileSecurityRef; var mode: mode_t ): Boolean; external name '_CFFileSecurityGetMode';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{
 *	This routine sets the mode_t associated with an CFFileSecurity object.
 *  
 *	Parameters
 *		fileSec
 *			The CFFileSecurity object.
 *		owner
 *			The mode_t.
 *	Return Value
 *		true if the mode_t was successfully set; otherwise, false.
 }
function CFFileSecuritySetMode( fileSec: CFFileSecurityRef; mode: mode_t ): Boolean; external name '_CFFileSecuritySetMode';
(* CF_AVAILABLE_STARTING(10_7, 5_0) *)


{ values to pass in the clearPropertyMask to CFFileSecurityClearProperties }
type
  CFFileSecurityClearOptions = CFOptionFlags;
const
    kCFFileSecurityClearOwner               = UNSIGNEDLONG(1) shl 0;
    kCFFileSecurityClearGroup               = UNSIGNEDLONG(1) shl 1;
    kCFFileSecurityClearMode                = UNSIGNEDLONG(1) shl 2;
    kCFFileSecurityClearOwnerUUID           = UNSIGNEDLONG(1) shl 3;
    kCFFileSecurityClearGroupUUID           = UNSIGNEDLONG(1) shl 4;
    kCFFileSecurityClearAccessControlList   = UNSIGNEDLONG(1) shl 5;

{
 *	This routine clears file security properties in the CFFileSecurity object.
 *  
 *	Parameters
 *		clearPropertyMask
 *			The file security properties to clear.
 *	Return Value
 *		true if the file security properties were successfully cleared; otherwise, false.
 }
function CFFileSecurityClearProperties( fileSec: CFFileSecurityRef; clearPropertyMask: CFFileSecurityClearOptions ): Boolean; external name '_CFFileSecurityClearProperties';
(* CF_AVAILABLE_STARTING(10_8, 6_0) *)


{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

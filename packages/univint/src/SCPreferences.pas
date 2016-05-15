{
 * Copyright (c) 2000, 2001, 2004, 2005, 2007-2010 Apple Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 }
{  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
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

unit SCPreferences;
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
uses MacTypes,MacOSXPosix,CFBase,SCDynamicStore,CFDate,CFPropertyList,CFArray,CFData,CFRunLoop,Authorization;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}

{$ifc not TARGET_OS_IPHONE}
{$elsec} {not TARGET_OS_IPHONE}
type
	AuthorizationOpaqueRef = record end;
	AuthorizationRef = ^AuthorizationOpaqueRef;
{$endc} {not TARGET_OS_IPHONE}

{!
	@header SCPreferences
	@discussion The SCPreferences API allows an application to load and
		store XML configuration data in a controlled manner and provide
		the necessary notifications to other applications that need to
		be aware of configuration changes.

		To access configuration preferences, you must first establish a
		preferences session using the SCPreferencesCreate function.
		To identify a specific set of preferences to access, you pass a
		value in the prefsID parameter.
		A NULL value indicates that the default system preferences are
		to be accessed.
		A string that starts with a leading "/" character specifies
		the absolute path to the file containing the preferences to
		be accessed.
		A string that does not start with a leading "/" character
		specifies a file relative to the default system preferences
		directory.

		When you are finished with the preferences session, use
		CFRelease to close it.
 }


{!
	@typedef SCPreferencesRef
	@discussion This is the handle to an open preferences session for
		accessing system configuration preferences.
 }
type
	SCPreferencesRef = ^__SCPreferences; { an opaque type }
	__SCPreferences = record end;

{!
	@enum SCPreferencesNotification
	@discussion Used with the SCPreferencesCallBack callback
		to describe the type of notification.
	@constant kSCPreferencesNotificationCommit Indicates when new
		preferences have been saved.
	@constant kSCPreferencesNotificationApply Key Indicates when a
		request has been made to apply the currently saved
		preferences to the active system configuration.
 }
const
	kSCPreferencesNotificationCommit = 1 shl 0;	// __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_NA)
	kSCPreferencesNotificationApply = 1 shl 1;	// __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_NA)

type
	SCPreferencesNotification = UInt32;

{!
	@typedef SCPreferencesContext
	Structure containing user-specified data and callbacks for SCPreferences.
	@field version The version number of the structure type being passed
		in as a parameter to the SCPreferencesSetCallback function.
		This structure is version 0.
	@field info A C pointer to a user-specified block of data.
	@field retain The callback used to add a retain for the info field.
		If this parameter is not a pointer to a function of the correct
		prototype, the behavior is undefined.
		The value may be NULL.
	@field release The calllback used to remove a retain previously added
		for the info field.
		If this parameter is not a pointer to a function of the
		correct prototype, the behavior is undefined.
		The value may be NULL.
	@field copyDescription The callback used to provide a description of
		the info field.
 }
type
	SCPreferencesContext = record
		version: CFIndex;
		info: UnivPtr;
		retain: function( info: UnivPtr ): UnivPtr;
		release: procedure( info: UnivPtr );
		copyDescription: function( info: UnivPtr ): CFStringRef;
	end;

{!
	@typedef SCPreferencesCallBack
	@discussion Type of the callback function used when the
		preferences have been updated and/or applied.
	@param prefs The preferences session.
	@param notificationType The type of notification, such as changes
		committed, changes applied, etc.
	@param info A C pointer to a user-specified block of data.
 }
type
	SCPreferencesCallBack = procedure( prefs: SCPreferencesRef; notificationType: SCPreferencesNotification; info: UnivPtr );

{ until __IPHONE_NA is automatically translated }
{$ifc TARGET_OS_MAC}

{!
	@function SCPreferencesGetTypeID
	@discussion Returns the type identifier of all SCPreferences instances.
 }
function SCPreferencesGetTypeID: CFTypeID; external name '_SCPreferencesGetTypeID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesCreate
	@discussion Initiates access to the per-system set of configuration
		preferences.
	@param allocator The CFAllocator that should be used to allocate
		memory for this preferences session.
		This parameter may be NULL in which case the current
		default CFAllocator is used.
		If this reference is not a valid CFAllocator, the behavior
		is undefined.
	@param name A string that describes the name of the calling
		process.
	@param prefsID A string that identifies the name of the
		group of preferences to be accessed or updated.
	@result Returns a reference to the new SCPreferences.
		You must release the returned value.
 }
function SCPreferencesCreate( allocator: CFAllocatorRef; name: CFStringRef; prefsID: CFStringRef ): SCPreferencesRef; external name '_SCPreferencesCreate';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)


{!
	@function SCPreferencesCreateWithAuthorization
	@discussion Initiates access to the per-system set of configuration
		preferences.
	@param allocator The CFAllocator that should be used to allocate
		memory for this preferences session.
		This parameter may be NULL in which case the current
		default CFAllocator is used.
		If this reference is not a valid CFAllocator, the behavior
		is undefined.
	@param name A string that describes the name of the calling
		process.
	@param prefsID A string that identifies the name of the
		group of preferences to be accessed or updated.
	@param authorization An authorization reference that is used to
		authorize any access to the enhanced privileges needed
		to manage the preferences session.
	@result Returns a reference to the new SCPreferences.
		You must release the returned value.
 }
function SCPreferencesCreateWithAuthorization( allocator: CFAllocatorRef; name: CFStringRef; prefsID: CFStringRef; authorization: AuthorizationRef ): SCPreferencesRef; external name '_SCPreferencesCreateWithAuthorization';
(* __OSX_AVAILABLE_STARTING(__MAC_10_5,__IPHONE_NA) *)

{!
	@function SCPreferencesLock
	@discussion Locks access to the configuration preferences.

		This function obtains exclusive access to the configuration
		preferences.  Clients attempting to obtain exclusive access
		to the preferences will either receive a kSCStatusPrefsBusy
		error or block waiting for the lock to be released.
	@param prefs The preferences session.
	@param wait A boolean flag indicating whether the calling process
		should block waiting for another process to complete its update
		operation and release its lock.
	@result Returns TRUE if the lock was obtained;
		FALSE if an error occurred.
 }
function SCPreferencesLock( prefs: SCPreferencesRef; wait: Boolean ): Boolean; external name '_SCPreferencesLock';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesCommitChanges
	@discussion Commits changes made to the configuration preferences to
		persistent storage.

		This function commits any changes to permanent storage.
		Implicit calls to the SCPreferencesLock and SCPreferencesUnlock
		functions will be made if exclusive access has not already been
		established.

		Note: This routine commits changes to persistent storage.
		Call the SCPreferencesApplyChanges function to apply the
		changes to the running system.
	@param prefs The preferences session.
	@result Returns TRUE if the lock was obtained;
		FALSE if an error occurred.
 }
function SCPreferencesCommitChanges( prefs: SCPreferencesRef ): Boolean; external name '_SCPreferencesCommitChanges';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesApplyChanges
	@discussion Requests that the currently stored configuration
		preferences be applied to the active configuration.
	@param prefs The preferences session.
	@result Returns TRUE if the lock was obtained;
		FALSE if an error occurred.
 }
function SCPreferencesApplyChanges( prefs: SCPreferencesRef ): Boolean; external name '_SCPreferencesApplyChanges';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesUnlock
	@discussion Releases exclusive access to the configuration preferences.

		This function releases the exclusive access lock to the
		preferences.  Other clients will be now be able to establish
		exclusive access to the preferences.
	@param prefs The preferences session.
	@result Returns TRUE if the lock was obtained;
		FALSE if an error occurred.
 }
function SCPreferencesUnlock( prefs: SCPreferencesRef ): Boolean; external name '_SCPreferencesUnlock';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesGetSignature
	@discussion Returns a sequence of bytes that can be used to determine
		if the saved configuration preferences have changed.
	@param prefs The preferences session.
	@result Returns a CFDataRef that reflects the signature of the configuration
		preferences at the time of the call to the SCPreferencesCreate function.
 }
function SCPreferencesGetSignature( prefs: SCPreferencesRef ): CFDataRef; external name '_SCPreferencesGetSignature';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesCopyKeyList
	@discussion Returns an array of currently defined preference keys.
	@param prefs The preferences session.
	@result Returns the list of keys.
		You must release the returned value.
 }
function SCPreferencesCopyKeyList( prefs: SCPreferencesRef ): CFArrayRef; external name '_SCPreferencesCopyKeyList';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesGetValue
	@discussion Returns the data associated with a preference key.

		This function retrieves data associated with the specified
		key.

		Note: To avoid inadvertantly reading stale data, first call
		the SCPreferencesLock function.
	@param prefs The preferences session.
	@param key The preference key to be returned.
	@result Returns the value associated with the specified preference key;
		NULL if no value was located.
 }
function SCPreferencesGetValue( prefs: SCPreferencesRef; key: CFStringRef ): CFPropertyListRef; external name '_SCPreferencesGetValue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesAddValue
	@discussion Adds data for a preference key.

		This function associates new data with the specified key.
		To commit these changes to permanent storage, a call must
		be made to the SCPreferencesCommitChanges function.
	@param prefs The preferences session.
	@param key The preference key to be updated.
	@param value The CFPropertyListRef object containing the
		value to be associated with the specified preference key.
	@result Returns TRUE if the value was added;
		FALSE if the key already exists or
		if an error occurred.
 }
function SCPreferencesAddValue( prefs: SCPreferencesRef; key: CFStringRef; value: CFPropertyListRef ): Boolean; external name '_SCPreferencesAddValue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesSetValue
	@discussion Updates the data associated with a preference key.

		This function adds or replaces the value associated with the
		specified key.  To commit these changes to permanent storage
		a call must be made to the SCPreferencesCommitChanges function.
	@param prefs The preferences session.
	@param key The preference key to be updated.
	@param value The CFPropertyListRef object containing the
		data to be associated with the specified preference key.
	@result Returns TRUE if the value was set;
		FALSE if an error occurred.
 }
function SCPreferencesSetValue( prefs: SCPreferencesRef; key: CFStringRef; value: CFPropertyListRef ): Boolean; external name '_SCPreferencesSetValue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesRemoveValue
	@discussion Removes the data associated with a preference key.

		This function removes the data associated with the specified
		key.  To commit these changes to permanent storage a call must
		be made to the SCPreferencesCommitChanges function.
	@param prefs The preferences session.
	@param key The preference key to be removed.
	@result Returns TRUE if the value was removed;
		FALSE if the key did not exist or if an error occurred.
 }
function SCPreferencesRemoveValue( prefs: SCPreferencesRef; key: CFStringRef ): Boolean; external name '_SCPreferencesRemoveValue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_NA) *)

{!
	@function SCPreferencesSetCallback
	@discussion Assigns a callback to a preferences session.  The function
		is called when the changes to the preferences have been
		committed or applied.
	@param prefs The preferences session.
	@param callout The function to be called when the preferences have
		been changed or applied.
		If NULL, the current callback is removed.
	@param context The SCPreferencesContext associated with
		the callout.
	@result Returns TRUE if the notification client was successfully set.
 }
function SCPreferencesSetCallback( prefs: SCPreferencesRef; callout: SCPreferencesCallBack; var context: SCPreferencesContext ): Boolean; external name '_SCPreferencesSetCallback';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_NA) *)

{!
	@function SCPreferencesScheduleWithRunLoop
	@discussion Schedule commit and apply notifications for the specified
		preferences session using the specified run loop and mode.
	@param prefs The preferences session.
	@param runLoop A reference to a run loop on which the notification
		should be scheduled.
		Must be non-NULL.
	@param runLoopMode The mode on which to schedule the notification.
		Must be non-NULL.
	@result Returns TRUE if the notifications are successfully scheduled;
		FALSE otherwise.
 }
function SCPreferencesScheduleWithRunLoop( prefs: SCPreferencesRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): Boolean; external name '_SCPreferencesScheduleWithRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_NA) *)

{!
	@function SCPreferencesUnscheduleFromRunLoop
	@discussion Unschedule commit and apply notifications for the specified
		preferences session from the specified run loop and mode.
	@param prefs The preferences session.
	@param runLoop A reference to a run loop from which the notification
		should be unscheduled.
		Must be non-NULL.
	@param runLoopMode The mode on which to unschedule the notification.
		Must be non-NULL.
	@result Returns TRUE if the notifications are successfully unscheduled;
		FALSE otherwise.
 }
function SCPreferencesUnscheduleFromRunLoop( prefs: SCPreferencesRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): Boolean; external name '_SCPreferencesUnscheduleFromRunLoop';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_NA) *)

{!
	@function SCPreferencesSetDispatchQueue
	@discussion Schedule commit and apply notifications for the specified
		preferences session.
	@param prefs The preferences session.
	@param queue The dispatch queue to run the callback function on.
	@result Returns TRUE if the notifications are successfully scheduled;
		FALSE otherwise.
 }
function SCPreferencesSetDispatchQueue( prefs: SCPreferencesRef; queue: dispatch_queue_t ): Boolean; external name '_SCPreferencesSetDispatchQueue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_NA) *)

{!
	@function SCPreferencesSynchronize
	@discussion Synchronizes accessed preferences with committed changes.

		Any references to preference values returned by calls to the
		SCPreferencesGetValue function are no longer valid unless they
		were explicitly retained or copied.  Any preference values
		that were updated (add, set, remove) but not committed will
		be discarded.
	@param prefs The preferences session.
 }
procedure SCPreferencesSynchronize( prefs: SCPreferencesRef ); external name '_SCPreferencesSynchronize';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_NA) *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

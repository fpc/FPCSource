{
 * Copyright (c) 2000-2003 Apple Computer, Inc. All rights reserved.
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
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY of ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES of MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 }
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }


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

unit SCPreferences;
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
uses MacTypes,CFBase,SCDynamicStore,CFDate,CFPropertyList,CFArray,CFData;
{$ALIGN MAC68K}

{!
	@header SCPreferences
	The SCPreferencesXXX() APIs allow an application to load and
	store XML configuration data in a controlled manner and provide
	the necessary notifications to other applications that need to
	be aware of configuration changes.

	The stored XML configuration data is accessed using a prefsID. A
	NULL value indicates that the default system preferences are to
	be accessed.
	A string which starts with a leading "/" character specifies the
	path to the file containing te preferences to be accessed.
	A string which does not start with a leading "/" character
	specifies a file relative to the default system preferences
	directory.
 }


{!
	@typedef SCPreferencesRef
	@discussion This is the handle to an open "session" for
		accessing system configuration preferences.
 }
type
	SCPreferencesRef    = ^SInt32; { an opaque 32-bit type }

{!
	@function SCPreferencesGetTypeID
	Returns the type identifier of all SCPreferences instances.
 }
function SCPreferencesGetTypeID: CFTypeID; external name '_SCPreferencesGetTypeID';


{!
	@function SCPreferencesCreate
	@discussion Initiates access to the per-system set of configuration
		preferences.
	@param allocator ...
	@param name A string that describes the name of the calling
		process.
	@param prefsID A string that identifies the name of the
		group of preferences to be accessed/updated.
	@result prefs A pointer to memory that will be filled with an
		SCPreferencesRef handle to be used for all subsequent requests.
		If a session cannot be established, the contents of
		memory pointed to by this parameter are undefined.
 }
function SCPreferencesCreate( allocator: CFAllocatorRef; name: CFStringRef; prefsID: CFStringRef ): SCPreferencesRef; external name '_SCPreferencesCreate';

{!
	@function SCPreferencesLock
	@discussion Locks access to the configuration preferences.

	This function obtains exclusive access to the configuration
	preferences associated with this prefsID. Clients attempting
	to obtain exclusive access to the preferences will either receive
	an kSCStatusPrefsBusy error or block waiting for the lock to be
	released.
	@param session An SCPreferencesRef handle that should be used for
		all API calls.
	@param wait A boolean flag indicating whether the calling process
		should block waiting for another process to complete its update
		operation and release its lock.
	@result TRUE if the lock was obtained; FALSE if an error occurred.
 }
function SCPreferencesLock( session: SCPreferencesRef; wait: Boolean ): Boolean; external name '_SCPreferencesLock';

{!
	@function SCPreferencesCommitChanges
	@discussion Commits changes made to the configuration preferences to
		persitent storage.

		This function commits any changes to permanent storage. An
		implicit call to SCPreferencesLock/SCPreferencesUnlock will
		be made if exclusive access has not already been established.

		Note:  This routine commits changes to persistent storage.
		Call SCPreferencesApplyChanges() to apply the changes
		to the running system.
	@param session An SCPreferencesRef handle that should be used for
		all API calls.
	@result TRUE if the lock was obtained; FALSE if an error occurred.
 }
function SCPreferencesCommitChanges( session: SCPreferencesRef ): Boolean; external name '_SCPreferencesCommitChanges';

{!
	@function SCPreferencesApplyChanges
	@discussion Requests that the currently stored configuration
		preferences be applied to the active configuration.
	@param session An SCPreferencesRef handle that should be used for
		all API calls.
	@result TRUE if the lock was obtained; FALSE if an error occurred.
 }
function SCPreferencesApplyChanges( session: SCPreferencesRef ): Boolean; external name '_SCPreferencesApplyChanges';

{!
	@function SCPreferencesUnlock
	@discussion Releases exclusive access to the configuration preferences.

		This function releases the exclusive access "lock" for this prefsID.
		Other clients will be now be able to establish exclusive access to
		the preferences.
	@param session An SCPreferencesRef handle that should be used for
		all API calls.
	@result TRUE if the lock was obtained; FALSE if an error occurred.
 }
function SCPreferencesUnlock( session: SCPreferencesRef ): Boolean; external name '_SCPreferencesUnlock';

{!
	@function SCPreferencesGetSignature
	@discussion Returns a sequence of bytes that can be used to determine
		if the saved configuration preferences have changed.
	@param session An SCPreferencesRef handle that should be used for
		all API calls.
	@param signature A pointer to a CFDataRef that will reflect
		the signature of the configuration preferences at the time
		of the call to SCPreferencesCreate().
	@result A CFDataRef that reflects the signature of the configuration
		preferences at the time of the call to SCPreferencesCreate().
 }
function SCPreferencesGetSignature( session: SCPreferencesRef ): CFDataRef; external name '_SCPreferencesGetSignature';

{!
	@function SCPreferencesCopyKeyList
	@discussion Returns an array of currently defined preference keys.
	@param session An SCPreferencesRef handle that should be used for
		all API calls.
	@result The list of keys.  You must release the returned value.
 }
function SCPreferencesCopyKeyList( session: SCPreferencesRef ): CFArrayRef; external name '_SCPreferencesCopyKeyList';

{!
	@function SCPreferencesGetValue
	@discussion Returns the data associated with a preference key.

		This function retrieves data associated with a key for the prefsID.

		Note:  You could read stale data and not know it, unless you
		first call SCPreferencesLock().
	@param session An SCPreferencesRef handle that should be used for
		all API calls.
	@param key The preference key to be returned.
	@result The value associated with the specified preference key; If no
		value was located, NULL is returned.
 }
function SCPreferencesGetValue( session: SCPreferencesRef; key: CFStringRef ): CFPropertyListRef; external name '_SCPreferencesGetValue';

{!
	@function SCPreferencesAddValue
	@discussion Adds data for a preference key.

	This function associates new data with the specified key. In order
	to commit these changes to permanent storage a call must be made to
	SCPreferencesCommitChanges().
	@param session The SCPreferencesRef handle that should be used to
		communicate with the APIs.
	@param key The preference key to be updated.
	@param value The CFPropertyListRef object containing the
		value to be associated with the specified preference key.
	@result TRUE if the value was added; FALSE if the key already exists or
		if an error occurred.
 }
function SCPreferencesAddValue( session: SCPreferencesRef; key: CFStringRef; value: CFPropertyListRef ): Boolean; external name '_SCPreferencesAddValue';

{!
	@function SCPreferencesSetValue
	@discussion Updates the data associated with a preference key.

	This function adds or replaces the value associated with the
	specified key. In order to commit these changes to permanent
	storage a call must be made to SCPreferencesCommitChanges().
	@param session The SCPreferencesRef handle that should be used to
		communicate with the APIs.
	@param key The preference key to be updated.
	@param value The CFPropertyListRef object containing the
		data to be associated with the specified preference key.
	@result TRUE if the value was set; FALSE if an error occurred.
 }
function SCPreferencesSetValue( session: SCPreferencesRef; key: CFStringRef; value: CFPropertyListRef ): Boolean; external name '_SCPreferencesSetValue';

{!
	@function SCPreferencesRemoveValue
	@discussion Removes the data associated with a preference key.

	This function removes the data associated with the specified
	key. In order to commit these changes to permanent storage a
	call must be made to SCPreferencesCommitChanges().
	@param session The SCPreferencesRef handle that should be used to
		communicate with the APIs.
	@param key The preference key to be removed.
	@result TRUE if the value was removed; FALSE if the key did not exist or
		if an error occurred.
 }
function SCPreferencesRemoveValue( session: SCPreferencesRef; key: CFStringRef ): Boolean; external name '_SCPreferencesRemoveValue';

end.

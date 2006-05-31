{
 * Copyright (c) 2000 Apple Computer, Inc. All rights reserved.
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
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit SCDynamicStore;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,CFBase,CFArray,CFRunLoop,CFPropertyList,CFDictionary;
{$ALIGN MAC68K}

{!
	@header SCDynamicStore
	The SystemConfiguration framework provides access to the
	data used to configure a running system.  The APIs provided
	by this framework communicate with the "configd" daemon.

	The "configd" daemon manages a "dynamic store" reflecting the
	desired configuration settings as well as the current state
	of the system.  The daemon provides a notification mechanism
	for user-level processes that need to be aware of changes
	made to the data.  Lastly, the daemon loads a number of
	bundles (or plug-ins) that monitor low-level kernel events
	and, via a set of policy modules, keep the state data up
	to date.
 }


{!
	@typedef SCDynamicStoreRef
	@discussion This is the handle to an open "dynamic store" session
		with the system configuration daemon.
 }
type    
 	SCDynamicStoreRef            = ^SInt32; { an opaque 32-bit type }

{!
	@typedef SCDynamicStoreContext
 }
type
	SCDynamicStoreContext = record
		version: CFIndex;
		info: Ptr;
		retain: function( info: Ptr ): Ptr;
		release: procedure( info: Ptr );
		copyDescription: function( info: Ptr ): CFStringRef;
	end;
	SCDynamicStoreContextPtr = ^SCDynamicStoreContext;

{!
	@typedef SCDynamicStoreCallBack
	@discussion Type of the callback function used when a
		dynamic store change is delivered.
	@param store The "dynamic store" session.
	@param changedKeys The list of changed keys.
	@param info ....
 }
type SCDynamicStoreCallBack = procedure( store: SCDynamicStoreRef; changedKeys: CFArrayRef; info: Ptr );

{!
	@function SCDynamicStoreGetTypeID
	Returns the type identifier of all SCDynamicStore instances.
 }
function SCDynamicStoreGetTypeID: CFTypeID; external name '_SCDynamicStoreGetTypeID';


{!
	@function SCDynamicStoreCreate
	@discussion Creates a new session used to interact with the dynamic
		store maintained by the SystemConfiguration server.
	@param allocator The CFAllocator which should be used to allocate
		memory for the local "dynamic store" and its storage for
		values.
		This parameter may be NULL in which case the current
		default CFAllocator is used. If this reference is not
		a valid CFAllocator, the behavior is undefined.
	@param name A string that describes the name of the calling
		process or plug-in of the caller.
	@param callout The function to be called when a watched value
		in the "dynamic store" is changed.
		A NULL value can be specified if no callouts are
		desired.
	@param context The SCDynamicStoreContext associated with the callout.
	@result A reference to the new SCDynamicStore.
 }
function SCDynamicStoreCreate( allocator: CFAllocatorRef; name: CFStringRef; callout: SCDynamicStoreCallBack; var context: SCDynamicStoreContext ): SCDynamicStoreRef; external name '_SCDynamicStoreCreate';

{!
	@function SCDynamicStoreCreateRunLoopSource
	@discussion Creates a new session used to interact with the dynamic
		store maintained by the SystemConfiguration server.
	@param allocator The CFAllocator which should be used to allocate
		memory for the local "dynamic store" and its storage for
		values.
		This parameter may be NULL in which case the current
		default CFAllocator is used. If this reference is not
		a valid CFAllocator, the behavior is undefined.
	@param store The "dynamic store" session.
	@param order On platforms which support it, this parameter
		determines the order in which the sources which are
		ready to be processed are handled.  A lower order
		number causes processing before higher order number
		sources. It is inadvisable to depend on the order
		number for any architectural or design aspect of
		code. In the absence of any reason to do otherwise,
		zero should be used.
	@result A reference to the new CFRunLoopSource.
		You must release the returned value.

 }
function SCDynamicStoreCreateRunLoopSource( allocator: CFAllocatorRef; store: SCDynamicStoreRef; order: CFIndex ): CFRunLoopSourceRef; external name '_SCDynamicStoreCreateRunLoopSource';

{!
	@function SCDynamicStoreCopyKeyList
	@discussion Returns an array of CFString keys representing the
		configuration "dynamic store" entries that match a
		specified pattern.
	@param store The "dynamic store" session.
	@param pattern A regex(3) regular expression pattern that
		will be used to match the "dynamic store" keys.
	@result The list of matching keys.
		You must release the returned value.
		A NULL value will be returned if the list could not be obtained.
 }
function SCDynamicStoreCopyKeyList( store: SCDynamicStoreRef; pattern: CFStringRef ): CFArrayRef; external name '_SCDynamicStoreCopyKeyList';

{!
	@function SCDynamicStoreAddValue
	@discussion Adds the key-value pair to the "dynamic store" if no
		such key already exists.
	@param store The "dynamic store" session.
	@param key The key of the value to add to the "dynamic store".
	@param value The value to add to the "dynamic store".
	@result TRUE if the key was added; FALSE if the key was already
		present in the "dynamic store" or if an error was encountered.
 }
function SCDynamicStoreAddValue( store: SCDynamicStoreRef; key: CFStringRef; value: CFPropertyListRef ): Boolean; external name '_SCDynamicStoreAddValue';

{!
	@function SCDynamicStoreAddTemporaryValue
	@discussion Adds the key-value pair on a temporary basis to the
		"dynamic store" if no such key already exists.  This entry
		will, unless updated by another session, automatically be
		removed when the session is closed.
	@param store The "dynamic store" session.
	@param key The key of the value to add to the "dynamic store".
	@param value The value to add to the "dynamic store".
	@result TRUE if the key was added; FALSE if the key was already
		present in the "dynamic store" or if an error was encountered.
 }
function SCDynamicStoreAddTemporaryValue( store: SCDynamicStoreRef; key: CFStringRef; value: CFPropertyListRef ): Boolean; external name '_SCDynamicStoreAddTemporaryValue';

{!
	@function SCDynamicStoreCopyValue
	@discussion Obtains a value from the "dynamic store" for the
		specified key.
	@param store The "dynamic store" session.
	@param key The key you wish to obtain.
	@result The value from the store that is associated with the
		given key.  The value is returned as a Core Foundation
		Property List data type.
		You must release the returned value.
		If no value was located, NULL is returned.
 }
function SCDynamicStoreCopyValue( store: SCDynamicStoreRef; key: CFStringRef ): CFPropertyListRef; external name '_SCDynamicStoreCopyValue';

{!
	@function SCDynamicStoreCopyMultiple
	@discussion Fetches multiple values in the "dynamic store".
	@param store The "dynamic store" session.
	@param keys The keys to be fetched; NULL if no specific keys
		are requested.
	@param patterns The regex(3) pattern strings to be fetched; NULL
		if no key patterns are requested.
	@result A dictionary containing the specific keys which were found
		in the "dynamic store" and any keys which matched the specified
		patterns; NULL is returned if an error was encountered.
		You must release the returned value.
 }
function SCDynamicStoreCopyMultiple( store: SCDynamicStoreRef; keys: CFArrayRef; patterns: CFArrayRef ): CFDictionaryRef; external name '_SCDynamicStoreCopyMultiple';

{!
	@function SCDynamicStoreSetValue
	@discussion Adds or replaces a value in the "dynamic store" for
		the specified key.
	@param store The "dynamic store" session.
	@param key The key you wish to set.
	@param value The value to add to or replace in the "dynamic store".
	@result TRUE if the key was updated; FALSE if an error was encountered.
 }
function SCDynamicStoreSetValue( store: SCDynamicStoreRef; key: CFStringRef; value: CFPropertyListRef ): Boolean; external name '_SCDynamicStoreSetValue';

{!
	@function SCDynamicStoreSetMultiple
	@discussion Updates multiple values in the "dynamic store".
	@param store The "dynamic store" session.
	@param keysToSet Key/value pairs you wish to set into the "dynamic store".
	@param keysToRemove A list of keys you wish to remove from the "dynamic store".
	@param keysToNotify A list of keys to flag as changed (without actually changing the data).
	@result TRUE if the dynamic store updates were successful; FALSE if an error was encountered.
 }
function SCDynamicStoreSetMultiple( store: SCDynamicStoreRef; keysToSet: CFDictionaryRef; keysToRemove: CFArrayRef; keysToNotify: CFArrayRef ): Boolean; external name '_SCDynamicStoreSetMultiple';

{!
	@function SCDynamicStoreRemoveValue
	@discussion Removes the value of the specified key from the
		"dynamic store".
	@param store The "dynamic store" session.
	@param key The key of the value you wish to remove.
	@result TRUE if the key was removed; FALSE if no value was
		located or an error was encountered.
 }
function SCDynamicStoreRemoveValue( store: SCDynamicStoreRef; key: CFStringRef ): Boolean; external name '_SCDynamicStoreRemoveValue';

{!
	@function SCDynamicStoreNotifyValue
	@discussion Triggers a notification to be delivered for the
		specified key in the dynamic store.
	@param store The "dynamic store" session.
	@param key The key which should be flagged as changed (without actually changing the data).
	@result TRUE if the value was updated; FALSE if an error was encountered.
 }
function SCDynamicStoreNotifyValue( store: SCDynamicStoreRef; key: CFStringRef ): Boolean; external name '_SCDynamicStoreNotifyValue';

{!
	@function SCDynamicStoreSetNotificationKeys
	@discussion Specifies a set of specific keys and key patterns
		which should be monitored for changes.
	@param store The "dynamic store" session being watched.
	@param keys The keys to be monitored; NULL if no specific keys
		are to be monitored.
	@param patterns The regex(3) pattern strings to be monitored; NULL
		if no key patterns are to be monitored.
	@result TRUE if the monitored keys were set; FALSE if an error
		was encountered.
 }
function SCDynamicStoreSetNotificationKeys( store: SCDynamicStoreRef; keys: CFArrayRef; patterns: CFArrayRef ): Boolean; external name '_SCDynamicStoreSetNotificationKeys';

{!
	@function SCDynamicStoreCopyNotifiedKeys
	@discussion Returns an array of CFString keys representing the
		"dynamic store" entries that have changed since this
		function was last called.
	@param store The "dynamic store" session.
	@result The list of changed keys.
		You must release the returned value.
		A NULL value will be returned if the list could not be obtained.
 }
function SCDynamicStoreCopyNotifiedKeys( store: SCDynamicStoreRef ): CFArrayRef; external name '_SCDynamicStoreCopyNotifiedKeys';

end.

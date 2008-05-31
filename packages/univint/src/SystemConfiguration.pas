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

unit SystemConfiguration;
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
uses MacTypes,CFBase;
{$ALIGN MAC68K}

{!
	@header SystemConfiguration.h
	The SystemConfiguration framework provides access to the
	data used to configure a running system.  The APIs provided
	by this framework communicate with the "configd" daemon.

	The "configd" daemon manages a "dynamic store" reflecting the
	desired configuration settings as well as the current state
	of the system.  The daemon provides a notification mechanism
	for user-level processes which need to be aware of changes
	made to the data.  Lastly, the daemon loads a number of
	bundles (or plug-ins) that monitor low-level kernel events
	and, via a set of policy modules, keep the state data up
	to date.
 }

{!
	@enum
	@discussion Returned status codes.

	@constant kSCStatusOK			Success
	@constant kSCStatusFailed		Non-specific Failure
	@constant kSCStatusInvalidArgument	Invalid argument
	@constant kSCStatusAccessError		Permission denied
	@constant kSCStatusNoKey		No such key
	@constant kSCStatusKeyExists		Data associated with key already defined
	@constant kSCStatusLocked		Lock already held
	@constant kSCStatusNeedLock		Lock required for this operation

	@constant kSCStatusNoStoreSession	Configuration daemon session not active
	@constant kSCStatusNoStoreServer	Configuration daemon not (no longer) available
	@constant kSCStatusNotifierActive	Notifier is currently active

	@constant kSCStatusNoPrefsSession	Preference session not active
	@constant kSCStatusPrefsBusy		Preferences update currently in progress
	@constant kSCStatusNoConfigFile		Configuration file not found
	@constant kSCStatusNoLink		No such link
	@constant kSCStatusStale		Write attempted on stale version of object
	@constant kSCStatusMaxLink		Maximum link count exceeded

	@constant kSCStatusReachabilityUnknown
		A determination could not be made regarding the reachability
		of the specified nodename/address.
}
const
	{
	 * Generic status codes
	 }
	kSCStatusOK				= 0;	{ Success }
	kSCStatusFailed				= 1001;	{ Non-specific failure }
	kSCStatusInvalidArgument		= 1002;	{ Invalid argument }
	kSCStatusAccessError			= 1003;	{ Permission denied
							   - must be root to obtain lock
							   - could not create access/create preferences
							 }
	kSCStatusNoKey				= 1004;	{ No such key }
	kSCStatusKeyExists			= 1005;	{ Key already defined }
	kSCStatusLocked				= 1006;	{ Lock already held }
	kSCStatusNeedLock			= 1007;	{ Lock required for this operation }
	{
	 * SCDynamicStore status codes
	 }
	kSCStatusNoStoreSession			= 2001;	{ Configuration daemon session not active }
	kSCStatusNoStoreServer			= 2002;	{ Configuration daemon not (no longer) available }
	kSCStatusNotifierActive			= 2003;	{ Notifier is currently active }
	{
	 * SCPreferences status codes
	 }
	kSCStatusNoPrefsSession			= 3001;	{ Preference session not active }
	kSCStatusPrefsBusy			= 3002;	{ Preferences update currently in progress }
	kSCStatusNoConfigFile			= 3003;	{ Configuration file not found }
	kSCStatusNoLink				= 3004;	{ No such link }
	kSCStatusStale				= 3005;	{ Write attempted on stale version of object }
	kSCStatusMaxLink			= 3006;	{ Maximum link count exceeded }
	{
	 * SCNetwork status codes
	 }
	kSCStatusReachabilityUnknown		= 4001;	{ Network reachability cannot be determined }


{!
	@function SCError
	@discussion Returns a last SystemConfiguration.framework API error code.
	@result The last error encountered.
 }
function SCError: SInt32; external name '_SCError';

{!
	@function SCErrorString
	@discussion Returns a pointer to the error message string associated
		with the specified status.
	@param status The SCDynamicStoreStatus to be returned.
	@result The error message string.
 }
function SCErrorString( status: SInt32 ): ConstCStringPtr; external name '_SCErrorString';

end.

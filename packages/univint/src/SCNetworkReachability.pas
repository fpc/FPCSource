{
 * Copyright (c) 2003 Apple Computer, Inc. All rights reserved.
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

unit SCNetworkReachability;
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
uses MacTypes,CFBase,SCNetwork,MacOSXPosix,CFRunLoop;
{$ALIGN MAC68K}

{!
	@header SCNetworkReachability
	The SCNetworkReachabilityXXX() APIs allow an application to determine the status
	of a system's current network configuration and the reachability
	of a target host.  In addition, the reachability can be monitored
	with a notification being provided when/if the status has changed.

	The term "reachable" reflects whether a data packet, sent by
	an application into the network stack, can be sent to the
	the target host/address.  Please note that there is no
	guarantee that the data packet will actually be received by
	the host.
 }

{!
	@typedef SCNetworkReachabilityRef
	@discussion This is the handle to a network address/name.
 }
type
	SCNetworkReachabilityRef    = ^SInt32; { an opaque 32-bit type }


{!
	@typedef SCNetworkReachabilityContext
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
type
	SCNetworkReachabilityContext = record
    version: CFIndex;
    info: Ptr;
		retain: function( info: Ptr ): Ptr;
		release: procedure( info: Ptr );
		copyDescription: function( info: Ptr ): CFStringRef;
	end;
	SCNetworkReachabilityContextPtr = ^SCNetworkReachabilityContext;

{!
	@typedef SCNetworkReachabilityCallBack
	@discussion Type of the callback function used when the
		reachability of a network address/name changes.
	@param target The SCNetworkReachability reference being monitored for changes.
	@param flags The new SCNetworkConnectionFlags representing the
		reachability status of the network address/name.
	@param info ....
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
type SCNetworkReachabilityCallBack = procedure( target: SCNetworkReachabilityRef; flags: SCNetworkConnectionFlags; info: UnivPtr );

{!
	@function SCNetworkReachabilityCreateWithAddress
	@discussion Creates a reference to the specified network
		address.  This reference can later be used to monitor
		the reachability of the target host.
	@param address The address of the desired host.
	@result A reference to the new immutable SCNetworkReachabilityRef.

		 You must release the returned value.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilityCreateWithAddress( allocator: CFAllocatorRef; address: sockaddr_ptr ): SCNetworkReachabilityRef; external name '_SCNetworkReachabilityCreateWithAddress';

{!
	@function SCNetworkReachabilityCreateWithAddressPair
	@discussion Creates a reference to the specified network
		address.  This reference can later be used to monitor
		the reachability of the target host.
	@param localAddress The local address associated with a network
		connection.  If NULL, only the remote address is of interest.
	@param remoteAddress The remote address associated with a network
		connection.  If NULL, only the local address is of interest.
	@result A reference to the new immutable SCNetworkReachabilityRef.

		 You must release the returned value.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilityCreateWithAddressPair( allocator: CFAllocatorRef; localAddress: sockaddr_ptr; remoteAddress: sockaddr_ptr ): SCNetworkReachabilityRef; external name '_SCNetworkReachabilityCreateWithAddressPair';

{!
	@function SCNetworkReachabilityCreateWithName
	@discussion Creates a reference to the specified network host/node
		name.  This reference can later be used to monitor the
		reachability of the target host.
	@param nodename The node name of the desired host. This name would
		be the same as that passed to gethostbyname() or getaddrinfo().
	@result A reference to the new immutable SCNetworkReachabilityRef.

		You must release the returned value.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilityCreateWithName( allocator: CFAllocatorRef; nodename: CStringPtr ): SCNetworkReachabilityRef; external name '_SCNetworkReachabilityCreateWithName';

{!
	@function SCNetworkReachabilityGetTypeID
	Returns the type identifier of all SCNetworkReachability instances.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilityGetTypeID: CFTypeID; external name '_SCNetworkReachabilityGetTypeID';


{!
	@function SCNetworkReachabilityGetFlags
	@discussion Determines if the given target is reachable using the
		current network configuration.
	@param target The network reference associated with the address/name
		to be checked for reachability.
	@param flags A pointer to memory that will be filled with the
		SCNetworkConnectionFlags detailing the reachability
		of the specified target.
	@result TRUE if the network connection flags are valid; FALSE if the
		status could not be determined.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilityGetFlags( target: SCNetworkReachabilityRef; var flags: SCNetworkConnectionFlags ): Boolean; external name '_SCNetworkReachabilityGetFlags';

{!
	@function SCNetworkReachabilitySetCallback
	@discussion Assigns a client to a target, which receives callbacks
		when the reachability of the target changes.
	@param target The network reference associated with the address/name
		to be checked for reachability.
	@param callout The function to be called when the reachability of
		target changes.  If NULL, the current client for the target
		is removed.
	@param context The SCNetworkReachabilityContext associated with
		the callout.
	@result TRUE if the notification client was successfully set.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilitySetCallback( target: SCNetworkReachabilityRef; callout: SCNetworkReachabilityCallBack; var context: SCNetworkReachabilityContext ): Boolean; external name '_SCNetworkReachabilitySetCallback';

{!
	@function SCNetworkReachabilityScheduleWithRunLoop
	@discussion Schedule the given target from the given run loop and mode.
	@param target The address/name which is set up for asynchronous mode.  Must be non-NULL.
	@param runLoop A reference to a runloop on which the target should be scheduled.  Must be non-NULL.
	@param runLoopMode The mode on which to schedule the target.  Must be non-NULL.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilityScheduleWithRunLoop( target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): Boolean; external name '_SCNetworkReachabilityScheduleWithRunLoop';

{!
	@function SCNetworkReachabilityUnscheduleFromRunLoop
	@discussion Unschedule the given target from the given run loop and mode.
	@param target The address/name which is set up for asynchronous mode.  Must be non-NULL.
	@param runLoop A reference to a runloop on which the target should be scheduled.  Must be non-NULL.
	@param runLoopMode The mode on which to schedule the target.  Must be non-NULL.
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function SCNetworkReachabilityUnscheduleFromRunLoop( target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): Boolean; external name '_SCNetworkReachabilityUnscheduleFromRunLoop';

end.

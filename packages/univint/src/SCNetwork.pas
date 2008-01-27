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
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit SCNetwork;
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
uses MacTypes,CFBase,MacOSXPosix;
{$ALIGN MAC68K}

{!
	@header SCNetwork

	SCNetworkCheckReachabilityXXX()

	The SCNetworkCheckReachabilityXXX() APIs allow an application to
	determine the status of the system's current network configuration
	and the accessibility of a target host/address.

	The term "reachable" reflects whether a data packet, sent by
	an application into the network stack, can be sent to the
	the target host/address.  Please note that their is no
	guarantee that the data packet will actually be received by
	the host.


	SCNetworkInterfaceRefreshConfiguration()

	This API sends a notification to interested network configuration
	agents to retry	their configuraton immediately. For example, calling
	this API will cause the DHCP client to contact the DHCP server
	immediately rather than waiting until its timeout has expired.
	The utility of this API is to allow the caller to give a hint to
	the system that the network infrastructure/configuration has changed.
 }

{!
	@enum SCNetworkConnectionFlags
	@discussion Flags that indicate whether the specified network
		nodename/address is reachable, requires a connection,
		requires some user intervention in establishing the
		connection, and whether the calling application must
		initiate the connection using the (TBD???) API.

	@constant kSCNetworkFlagsTransientConnection
		This flag indicates that the specified nodename/address can
		be reached via a transient (e.g. PPP) connection.

	@constant kSCNetworkFlagsReachable
		This flag indicates that the specified nodename/address can
		be reached using the current network configuration.

	@constant kSCNetworkFlagsConnectionRequired
		This flag indicates that the specified nodename/address can
		be reached using the current network configuration but a
		connection must first be established.

		As an example, this status would be returned for a dialup
		connection that was not currently active but could handle
		network traffic for the target system.

	@constant kSCNetworkFlagsConnectionAutomatic
		This flag indicates that the specified nodename/address can
		be reached using the current network configuration but a
		connection must first be established.  Any traffic directed
		to the specified name/address will initiate the connection.

	@constant kSCNetworkFlagsInterventionRequired
		This flag indicates that the specified nodename/address can
		be reached using the current network configuration but a
		connection must first be established.  In addition, some
		form of user intervention will be required to establish
		this connection (e.g. providing a password, authentication
		token, etc.).

	@constant kSCNetworkFlagsIsLocalAddress
		This flag indicates that the specified nodename/address
		is one associated with a network interface on the current
		system.

	@constant kSCNetworkFlagsIsDirect
		This flag indicates that network traffic to the specified
		nodename/address will not go through a gateway but is routed
		directly to one of the interfaces in the system.
 }
type
	SCNetworkConnectionFlags = UInt32;
const
	kSCNetworkFlagsTransientConnection	= 1 shl 0;
	kSCNetworkFlagsReachable		= 1 shl 1;
	kSCNetworkFlagsConnectionRequired	= 1 shl 2;
	kSCNetworkFlagsConnectionAutomatic	= 1 shl 3;
	kSCNetworkFlagsInterventionRequired	= 1 shl 4;
	kSCNetworkFlagsIsLocalAddress		= 1 shl 16;
	kSCNetworkFlagsIsDirect			= 1 shl 17;

{!
	@function SCNetworkCheckReachabilityByAddress
	@discussion Determines if the given network address is
		reachable using the current network configuration.
	@param address The network address of the desired host.
	@param addrlen The length, in bytes, of the address.
	@param flags A pointer to memory that will be filled with a
		set of SCNetworkConnectionFlags detailing the reachability
		of the specified address.
	@result TRUE if the network connection flags are valid; FALSE if the
		status could not be determined.
 }
function SCNetworkCheckReachabilityByAddress( address: sockaddr_ptr; addrlen: SInt32; var flags: SCNetworkConnectionFlags ): Boolean; external name '_SCNetworkCheckReachabilityByAddress';

{!
	@function SCNetworkCheckReachabilityByName
	@discussion Determines if the given network host/node name is
		reachable using the current network configuration.
	@param nodename The node name of the desired host. This name would
		be the same as that passed to gethostbyname() or getaddrinfo().
	@param flags A pointer to memory that will be filled with a
		set of SCNetworkConnectionFlags detailing the reachability
		of the specified node name.
	@result TRUE if the network connection flags are valid; FALSE if the
		status could not be determined.
 }
function SCNetworkCheckReachabilityByName( nodename: CStringPtr; var flags: SCNetworkConnectionFlags ): Boolean; external name '_SCNetworkCheckReachabilityByName';
{!
	@function SCNetworkInterfaceRefreshConfiguration
	@discussion Sends a notification to interested configuration agents
		to have them immediately retry their configuration over a
		particular network interface.
		Note: This API must be invoked by root (uid == 0).

	@param ifName The BSD name of the network interface e.g. CFSTR("en0").
	@result TRUE if the notification was sent; FALSE otherwise.
 }
function SCNetworkInterfaceRefreshConfiguration( ifName: CFStringRef ): Boolean; external name '_SCNetworkInterfaceRefreshConfiguration';

end.

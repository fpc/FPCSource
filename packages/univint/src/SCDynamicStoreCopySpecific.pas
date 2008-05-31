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

unit SCDynamicStoreCopySpecific;
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
uses MacTypes,CFBase,SCDynamicStore,MacOSXPosix,CFString,CFDictionary;
{$ALIGN MAC68K}

{!
	@header SCDynamicStoreCopySpecific
	The following APIs allow an application to determine specific
	configuration information about the current system (e.g. the
	computer/sharing name, the currently logged in user, etc).
 }

{!
	@function SCDynamicStoreCopyComputerName
	@discussion Gets the current computer/host name.
	@param store An SCDynamicStoreRef that should be used for communication
		with the server.
		If NULL, a temporary session will be used.
	@param nameEncoding A pointer to memory that, if non-NULL, will be
		filled with the encoding associated with the computer/host name.
	@result The current computer/host name;
		NULL if the name has not been set or if an error was encountered.
		You must release the returned value.
 }
function SCDynamicStoreCopyComputerName( store: SCDynamicStoreRef; nameEncoding: CFStringEncodingPtr ): CFStringRef; external name '_SCDynamicStoreCopyComputerName';

{!
	@function SCDynamicStoreCopyConsoleUser
	@discussion Gets the name, user ID, and group ID of the currently
		logged in user.
	@param store An SCDynamicStoreRef that should be used for communication
		with the server.
		If NULL, a temporary session will be used.
	@param uid A pointer to memory that will be filled with the user ID
		of the current "Console" user. If NULL, this value will not
		be returned.
	@param gid A pointer to memory that will be filled with the group ID
		of the current "Console" user. If NULL, this value will not be
		returned.
	@result The current user logged into the system;
		NULL if no user is logged in or if an error was encountered.
		You must release the returned value.
 }
function SCDynamicStoreCopyConsoleUser( store: SCDynamicStoreRef; uid: uid_t_ptr; gid: gid_t_ptr ): CFStringRef; external name '_SCDynamicStoreCopyConsoleUser';

{!
	@function SCDynamicStoreCopyLocalHostName
	@discussion Gets the current local host name.

		See SCDynamicStoreKeyCreateHostNames() for notification
		key information.
	@param store An SCDynamicStoreRef that should be used for communication
		with the server.
		If NULL, a temporary session will be used.
	@result The current local host name;
		NULL if the name has not been set or if an error was encountered.
		You must release the returned value.
 }
function SCDynamicStoreCopyLocalHostName( store: SCDynamicStoreRef ): CFStringRef; external name '_SCDynamicStoreCopyLocalHostName';

{!
	@function SCDynamicStoreCopyLocation
	@discussion Gets the current "location" identifier.
	@param store An SCDynamicStoreRef that should be used for communication
		with the server.
		If NULL, a temporary session will be used.
	@result A string representing the current "location" identifier;
		NULL if no "location" identifier has been defined or if an error
		was encountered.
		You must release the returned value.
 }
function SCDynamicStoreCopyLocation( store: SCDynamicStoreRef ): CFStringRef; external name '_SCDynamicStoreCopyLocation';

{!
	@function SCDynamicStoreCopyProxies
	@discussion Gets the current internet proxy settings.
	@param store An SCDynamicStoreRef that should be used for communication
		with the server.
		If NULL, a temporary session will be used.
	@result A dictionary with key/value pairs representing the current
		internet proxy settings (HTTP, FTP, etc);
		NULL if no proxy settings have been defined or if an error was encountered.
		You must release the returned value.
 }
function SCDynamicStoreCopyProxies( store: SCDynamicStoreRef ): CFDictionaryRef; external name '_SCDynamicStoreCopyProxies';

end.

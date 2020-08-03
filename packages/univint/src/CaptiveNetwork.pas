{
 * Copyright (c) 2009-2011 Apple Inc. All rights reserved.
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
{  Initial Pascal Translation:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CaptiveNetwork;
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
{$ifc defined iphonesim}
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
{$ifc defined iphonesim}
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
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,CFBase,CFArray,CFDictionary;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{!
	@header CaptiveNetwork
	@discussion The CaptiveNetwork API allows applications to interact
		with Captive Network Support. Captive Network Support is a
		system component responsible for detecting and help users
		navigate networks that require interaction before providing
		internet access. The most common Captive Networks are WiFi
		Hotspots in places like airports, restaurants, and hotels.
		Captive Network Support will attempt to authenticate if
		possible or drop a user in to a web sheet if authentication
		is not possible. In the web sheet the user has an opportunity
		to authenticate or disassociate from the network.

		The following APIs are designed for third party applications
		that may handle authentication on these networks on behalf of
		the user.

		These APIs are treated as advisory only.
		There is no guarantee or contract that the operating system
		will take the intended action.
 }


{!
	@function CNSetSupportedSSIDs
	@discussion Provides Captive Network Support with an updated list of
		SSIDs that this application will perform authentication on.

		Captive Network Support suppresses showing the Web Sheet
		for a captive Wi-Fi network if that network's SSID is in the
		specified list.

		On iOS, the registrations persist until the application is
		removed from the device.

		On MacOSX, the registrations persist as long as the application
		is running.

	@param ssidArray A CFArray of CFStrings of the SSIDs.
	@result Returns TRUE if the operation succeeded, FALSE otherwise.
 }
function CNSetSupportedSSIDs( ssidArray: CFArrayRef ): Boolean; external name '_CNSetSupportedSSIDs';
(* __OSX_AVAILABLE_STARTING(__MAC_10_8,__IPHONE_4_0) *)

{!
	@function CNMarkPortalOnline
	@discussion Tells Captive Network Support that your application has
		authenticated the device to the network. Captive Network Support
		will notify the rest of the system that WiFi is now a viable
		interface.
	@param interfaceName Name of the interface that is now online.
	@result Returns TRUE if the operation succeeded, FALSE otherwise.
 }
function CNMarkPortalOnline( interfaceName: CFStringRef ): Boolean; external name '_CNMarkPortalOnline';
(* __OSX_AVAILABLE_STARTING(__MAC_10_8,__IPHONE_4_0) *)

{!
	@function CNMarkPortalOffline
	@discussion Tells Captive Network Support that the device is not
		authenticated on the given network interface.
	@param interfaceName Name of the interface that is still captive.
	@result Returns TRUE if the operation succeeded, FALSE otherwise.
 }
function CNMarkPortalOffline( interfaceName: CFStringRef ): Boolean; external name '_CNMarkPortalOffline';
(* __OSX_AVAILABLE_STARTING(__MAC_10_8,__IPHONE_4_0) *)


{!
 @function CNCopySupportedInterfaces
 @discussion copies a list of all interfaces CaptiveNetworkSupport is monitoring.
 @result An array of CFStringRef- BSD interface names.
	 Returns NULL if an error was encountered.
	 You MUST release the returned value.
 }
function CNCopySupportedInterfaces: CFArrayRef; external name '_CNCopySupportedInterfaces';
(* __OSX_AVAILABLE_STARTING(__MAC_10_8,__IPHONE_4_1) *)

{$ifc	TARGET_OS_EMBEDDED}

{!
 @constant kCNNetworkInfoKeySSIDData
 @discussion NetworkInfo Dictionary key for SSID in CFData format
 }
var kCNNetworkInfoKeySSIDData: CFStringRef; external name '_kCNNetworkInfoKeySSIDData'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_1) *)

{!
 @constant kCNNetworkInfoKeySSID
 @discussion NetworkInfo Dictionary key for SSID in CFString format
 }
var kCNNetworkInfoKeySSID: CFStringRef; external name '_kCNNetworkInfoKeySSID'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_1) *)

{!
 @constant kCNNetworkInfoKeyBSSID
 @discussion NetworkInfo Dictionary key for BSSID in CFString format
 }
var kCNNetworkInfoKeyBSSID: CFStringRef; external name '_kCNNetworkInfoKeyBSSID'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_1) *)

{!
 @function CNCopyCurrentNetworkInfo
 @discussion Returns the Network Info for the specified interface.
	For example, Network Info dictionary will contain the following
	keys, and values:
	<pre>
	@textblock
	Keys                      : Values
	=======================================
	kCNNetworkInfoKeySSIDData : CFDataRef
	kCNNetworkInfoKeySSID     : CFStringRef
	kCNNetworkInfoKeyBSSID    : CFStringRef
	@/textblock
	</pre>
 @param interfaceName Name of the interface you are interested in
 @result Network Info dictionary associated with the interface.
	 Returns NULL if an error was encountered.
	 You MUST release the returned value.
 }
function CNCopyCurrentNetworkInfo( interfaceName: CFStringRef ): CFDictionaryRef; external name '_CNCopyCurrentNetworkInfo';
(* __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_1) *)

{$endc} { TARGET_OS_EMBEDDED }
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

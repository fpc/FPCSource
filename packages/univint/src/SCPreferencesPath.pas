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

unit SCPreferencesPath;
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
uses MacTypes,CFBase,SCPreferences,CFDictionary;
{$ALIGN MAC68K}

{!
	@header SCPreferencesPath
	The SCPreferencesPathXXX() APIs allow an application to
	load and store XML configuration data in a controlled
	manner and provide the necessary notifications to other
	applications that need to be aware of configuration
	changes.

	The SCPreferencesPathXXX() APIs make certain assumptions
	about the layout of the preferences data.  These APIs view
	the data as a collection of dictionaries of key/value pairs
	and an associated path name.  The root path ("/") identifies
	the top-level dictionary.  Additional path components
	specify the keys for sub-dictionaries.

	For example, the following dictionary can be accessed via
	two paths.  The root ("/") path would return a dictionary
	with all keys and values.  The path "/path1" would only
	return the dictionary with the "key3" and "key4" properties.

	<PRE>
	<BR>    &lt;dict&gt;
	<BR>        &lt;key&gt;key1&lt;/key&gt;
	<BR>        &lt;string&gt;val1&lt;/string&gt;
	<BR>        &lt;key&gt;key2&lt;/key&gt;
	<BR>        &lt;string&gt;val2&lt;/string&gt;
	<BR>        &lt;key&gt;path1&lt;/key&gt;
	<BR>        &lt;dict&gt;
	<BR>            &lt;key&gt;key3&lt;/key&gt;
	<BR>            &lt;string&gt;val3&lt;/string&gt;
	<BR>        &lt;key&gt;key4&lt;/key&gt;
	<BR>        &lt;string&gt;val4&lt;/string&gt;
	<BR>        &lt;/dict&gt;
	<BR>    &lt;/dict&gt;
	</PRE>

	Each dictionary can also include the kSCResvLink key.  The
	value associated with this key is interpreted as a "link" to
	another path.  If this key is present, a call to the
	SCPreferencesPathGetValue() API will return the dictionary
	specified by the link.
 }

{!
	@function SCPreferencesPathCreateUniqueChild
	@discussion Creates a new path component within the dictionary
		hierarchy.
	@param session The SCPreferencesRef handle that should be used to
	 communicate with the APIs.
	@param prefix A string that represents the parent path.
	@result A string representing the new (unique) child path; NULL
		if the specified path does not exist.
 }
function SCPreferencesPathCreateUniqueChild( session: SCPreferencesRef; prefix: CFStringRef ): CFStringRef; external name '_SCPreferencesPathCreateUniqueChild';

{!
	@function SCPreferencesPathGetValue
	@discussion Returns the dictionary associated with the specified
		path.
	@param session The SCPreferencesRef handle that should be used to
		communicate with the APIs.
	@param path A string that represents the path to be returned.
	@result	The dictionary associated with the specified path; NULL
		if the path does not exist.
 }
function SCPreferencesPathGetValue( session: SCPreferencesRef; path: CFStringRef ): CFDictionaryRef; external name '_SCPreferencesPathGetValue';

{!
	@function SCPreferencesPathGetLink
	@discussion Returns the link (if one exists) associated with the
		specified path.
	@param session The SCPreferencesRef handle that should be used to
	 communicate with the APIs.
	@param path A string that represents the path to be returned.
	@result The dictionary associated with the specified path; NULL
		if the path is not a link or does not exist.
 }
function SCPreferencesPathGetLink( session: SCPreferencesRef; path: CFStringRef ): CFStringRef; external name '_SCPreferencesPathGetLink';

{!
	@function SCPreferencesPathSetValue
	@discussion Associates a dictionary with the specified path.
	@param session The SCPreferencesRef handle that should be used to
	 communicate with the APIs.
	@param path A string that represents the path to be updated.
	@param value A dictionary that represents the data to be
		stored at the specified path.
	@result A boolean indicating the success (or failure) of the call.
 }
function SCPreferencesPathSetValue( session: SCPreferencesRef; path: CFStringRef; value: CFDictionaryRef ): Boolean; external name '_SCPreferencesPathSetValue';

{!
	@function SCPreferencesPathSetLink
	@discussion Associates a link to a second dictionary at the
		specified path.
	@param session The SCPreferencesRef handle that should be used to
		communicate with the APIs.
	@param path A string that represents the path to be updated.
	@param link A string that represents the link to be stored
		at the specified path.
	@result A boolean indicating the success (or failure) of the call.
 }
function SCPreferencesPathSetLink( session: SCPreferencesRef; path: CFStringRef; link: CFStringRef ): Boolean; external name '_SCPreferencesPathSetLink';

{!
	@function SCPreferencesPathRemoveValue
	@discussion Removes the data associated with the specified path.
	@param session The SCPreferencesRef handle that should be used to
	 communicate with the APIs.
	@param path A string that represents the path to be returned.
	@result A boolean indicating the success (or failure) of the call.
 }
function SCPreferencesPathRemoveValue( session: SCPreferencesRef; path: CFStringRef ): Boolean; external name '_SCPreferencesPathRemoveValue';

end.

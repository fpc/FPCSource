{
	 File:	   CFNetwork/CFFTPStream.h
 
	 Contains:   CoreFoundation FTP stream header
  
	 Copyright:  Copyright (c) 2001-2008, Apple Inc. All rights reserved.
 
	 Bugs?:	  For bug reports, consult the following page on
				 the World Wide Web:
 
					 http://www.freepascal.org/bugs.html
 
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{   Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CFFTPStream;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := TFALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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
uses MacTypes,CFBase,CFStream,CFURL,CFDictionary;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{
 *  kCFStreamErrorDomainFTP
 *  
 *  Discussion:
 *	Result code returned by FTP server
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamErrorDomainFTP: SInt32; external name '_kCFStreamErrorDomainFTP'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
FTP Stream Property keys.  These keys can be passed to the stream
property "set/get" functions, such as CFReadStreamSetProperty/
CFReadStreamCopyProperty, or to a CFDictionary creator or an item
accessor/mutator.  The comment before each key declaration (treated
as definition) indicates the value type of the property.
}


{
 *  kCFStreamPropertyFTPUserName
 *  
 *  Discussion:
 *	Stream property key, for both set and copy operations.  CFString
 *	type to hold login user name.  Don't set this property if you
 *	want anonymous FTP.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPUserName: CFStringRef; external name '_kCFStreamPropertyFTPUserName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPPassword
 *  
 *  Discussion:
 *	Stream property key, for both set and copy operations. CFString
 *	type to hold login password.  Don't set this property if you want
 *	anonymous FTP.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPPassword: CFStringRef; external name '_kCFStreamPropertyFTPPassword'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPUsePassiveMode
 *  
 *  Discussion:
 *	Stream property key, for both set and copy operations. CFBoolean
 *	type. kCFBooleanTrue means use passive mode, kCFBooleanFalse
 *	otherwise
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPUsePassiveMode: CFStringRef; external name '_kCFStreamPropertyFTPUsePassiveMode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPResourceSize
 *  
 *  Discussion:
 *	Stream property key, for read stream copy operations.  CFNumber
 *	of kCFNumberLongLongType to hold resource size in bytes.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPResourceSize: CFStringRef; external name '_kCFStreamPropertyFTPResourceSize'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPFetchResourceInfo
 *  
 *  Discussion:
 *	Stream property key, for both set and copy operations.  CFBoolean
 *	type.  TRUE means that resource info, such as size, must be
 *	provided before download starts at higher cost.  Don't set if
 *	resource size/other info is unnecessary.  Initially, only
 *	resource size is implemented.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPFetchResourceInfo: CFStringRef; external name '_kCFStreamPropertyFTPFetchResourceInfo'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPFileTransferOffset
 *  
 *  Discussion:
 *	Stream property key, for both set and copy operations.  CFNumber
 *	of kCFNumberLongLongType for the file offset to start transfer at.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPFileTransferOffset: CFStringRef; external name '_kCFStreamPropertyFTPFileTransferOffset'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPAttemptPersistentConnection
 *  
 *  Discussion:
 *	Stream property key, for both set and copy operations.  CFBoolean
 *	type.  TRUE by default, set to FALSE to avoid reusing existing
 *	server connections.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPAttemptPersistentConnection: CFStringRef; external name '_kCFStreamPropertyFTPAttemptPersistentConnection'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPProxy
 *  
 *  Discussion:
 *	Stream property key, for both set and copy operations. 
 *	CFDictionary type that holds key-value pairs of proxy dictionary.
 *	 The dictionary returned by SystemConfiguration can also be
 *	passed directly as the value.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPProxy: CFStringRef; external name '_kCFStreamPropertyFTPProxy'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPProxyHost
 *  
 *  Discussion:
 *	Stream property key or FTP Proxy dictionary key, for both set and
 *	copy operations.  It matches kSCPropNetProxiesFTPProxy defined in
 *	SCSchemaDefinitions.h.  CFString for proxy server host name. 
 *	This property can be set and copied individually or via a
 *	CFDictionary.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPProxyHost: CFStringRef; external name '_kCFStreamPropertyFTPProxyHost'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFStreamPropertyFTPProxyPort
 *  
 *  Discussion:
 *	Stream property key or FTP Proxy dictionary key, for both set and
 *	copy operations.  It matches kSCPropNetProxiesFTPPort defined in
 *	SCSchemaDefinitions.h.  CFNumber of kCFNumberIntType for proxy
 *	server port number.  This property can be set and copied
 *	individually or via a CFDictionary.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPProxyPort: CFStringRef; external name '_kCFStreamPropertyFTPProxyPort'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)

{
 *  kCFStreamPropertyFTPProxyUser
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPProxyUser: CFStringRef; external name '_kCFStreamPropertyFTPProxyUser'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)
{
 *  kCFStreamPropertyFTPProxyPassword
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFStreamPropertyFTPProxyPassword: CFStringRef; external name '_kCFStreamPropertyFTPProxyPassword'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
CFDictionary keys for resource information.  The information is
extracted from a line of the directory list by function
CFFTPCreateParsedResourceListing.
}


{
 *  kCFFTPResourceMode
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFNumber to hold the
 *	resource access permission defined in sys/types.h.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceMode: CFStringRef; external name '_kCFFTPResourceMode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFFTPResourceName
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFString that holds
 *	the resource name.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceName: CFStringRef; external name '_kCFFTPResourceName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFFTPResourceOwner
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFString that holds
 *	the resource owner's name.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceOwner: CFStringRef; external name '_kCFFTPResourceOwner'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFFTPResourceGroup
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFString to hold the
 *	name of the group that shares the resource.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceGroup: CFStringRef; external name '_kCFFTPResourceGroup'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFFTPResourceLink
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFString to hold
 *	symbolic link information.  If the item is a symbolic link the
 *	string will contain the path to the item the link references.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceLink: CFStringRef; external name '_kCFFTPResourceLink'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFFTPResourceSize
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFNumber of
 *	kCFNumberLongLongType to hold the resource length in bytes.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceSize: CFStringRef; external name '_kCFFTPResourceSize'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFFTPResourceType
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFNumber to hold the
 *	resource type as defined in sys/dirent.h.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceType: CFStringRef; external name '_kCFFTPResourceType'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  kCFFTPResourceModDate
 *  
 *  Discussion:
 *	CFDictionary key, for get value operation.  CFDate to hold the
 *	last modification date and time information.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFFTPResourceModDate: CFStringRef; external name '_kCFFTPResourceModDate'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  CFReadStreamCreateWithFTPURL()
 *  
 *  Discussion:
 *	Create an FTP read stream for downloading operation from an FTP
 *	URL. If the URL refers to a directory, the stream is a filtered
 *	line-at-a-time read stream corresponding to the listing results
 *	provided by the server. If it's a file, then the stream is a
 *	regular read stream providing the data for that file.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  A pointer to the CFAllocator which should be used to allocate
 *	  memory for the CF read stream and its storage for values. If
 *	  this reference is not a valid CFAllocator, the behavior is
 *	  undefined.
 *	
 *	ftpURL:
 *	  A pointer to a CFURL structure created by CFURLCreateWithString
 *	  function.  If this parameter is not a pointer to a valid CFURL
 *	  structure, the behavior is undefined.
 *  
 *  Result:
 *	A pointer to the CF read stream created, or NULL if failed. It is
 *	caller's responsibilty to release the memory allocated for the
 *	read stream.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFReadStreamCreateWithFTPURL( alloc: CFAllocatorRef; ftpURL: CFURLRef ): CFReadStreamRef; external name '_CFReadStreamCreateWithFTPURL';
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  CFFTPCreateParsedResourceListing()
 *  
 *  Discussion:
 *	Parse a line of file or folder listing of Unix format, and store
 *	the extracted result in a CFDictionary.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  A pointer to the CFAllocator which should be used to allocate
 *	  memory for the CFDictionary to hold resource info.  If this
 *	  reference is not a valid CFAllocator, the behavior is undefined.
 *	
 *	buffer:
 *	  A pointer to a buffer that may hold lines of resource listing,
 *	  but only the first line starting from buffer[0] will be parsed
 *	  each call.
 *	
 *	bufferLength:
 *	  The maximum buffer size in bytes started from the location
 *	  pointed by "buffer."
 *	
 *	parsed:
 *	  A pointer to a CFDictionary pointer.  The dictionary holds the
 *	  extracted resource information.  When parsing fails, a NULL
 *	  pointer will be returned.  It is caller's responsibilty to
 *	  release the memory allocated for the dictionary.
 *  
 *  Result:
 *	The number of bytes consumed from buffer, 0 if there are not
 *	enough bytes, or -1 if a parse failure occurs.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFFTPCreateParsedResourceListing( alloc: CFAllocatorRef; buffer: UnivPtr; bufferLength: CFIndex; var parsed: CFDictionaryRef ): CFIndex; external name '_CFFTPCreateParsedResourceListing';
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)


{
 *  CFWriteStreamCreateWithFTPURL()
 *  
 *  Discussion:
 *	Create an FTP write stream for uploading operation to a FTP URL.
 *	If the URL specifies a directory, the open will be followed by a
 *	close event/state and the directory will have been created. 
 *	Intermediary directory structure is not created.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  A pointer to the CFAllocator which should be used to allocate
 *	  memory for the CF read stream and its storage for values. If
 *	  this reference is not a valid CFAllocator, the behavior is
 *	  undefined.
 *	
 *	ftpURL:
 *	  A pointer to a CFURL structure created by CFURLCreateWithString
 *	  function.  If this parameter is not a pointer to a valid CFURL
 *	  structure, the behavior is undefined.
 *  
 *  Result:
 *	A pointer to the CF write stream created, or NULL if failed. It
 *	is caller's responsibilty to release the memory allocated for the
 *	write stream.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.3 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFWriteStreamCreateWithFTPURL( alloc: CFAllocatorRef; ftpURL: CFURLRef ): CFWriteStreamRef; external name '_CFWriteStreamCreateWithFTPURL';
(* __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_2_0) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

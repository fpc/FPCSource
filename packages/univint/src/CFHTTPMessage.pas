{
	 File:	   CFNetwork/CFHTTPMessage.h
 
	 Contains:   CoreFoundation Network socket streams header
 
	 Copyright:  Copyright (c) 2001-2008, Apple Inc. All rights reserved.
 
	 Bugs?:	  For bug reports, consult the following page on
				 the World Wide Web:
 
					 http://www.freepascal.org/bugs.html
 
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{   Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2008 }
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

unit CFHTTPMessage;
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
uses MacTypes,CFString,CFURL,CFBase,CFData,CFDictionary;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}

{
 *  kCFHTTPVersion1_0
 *  
 *  Discussion:
 *	Version string for HTTP 1.0.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFHTTPVersion1_0: CFStringRef; external name '_kCFHTTPVersion1_0'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  kCFHTTPVersion1_1
 *  
 *  Discussion:
 *	Version string for HTTP 1.1.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFHTTPVersion1_1: CFStringRef; external name '_kCFHTTPVersion1_1'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  kCFHTTPAuthenticationSchemeBasic
 *  
 *  Discussion:
 *	HTTP Basic authentication scheme.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFHTTPAuthenticationSchemeBasic: CFStringRef; external name '_kCFHTTPAuthenticationSchemeBasic'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)


{
 *  kCFHTTPAuthenticationSchemeDigest
 *  
 *  Discussion:
 *	HTTP Digest Access authentication scheme.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.2 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFHTTPAuthenticationSchemeDigest: CFStringRef; external name '_kCFHTTPAuthenticationSchemeDigest'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0) *)

{
 *  kCFHTTPAuthenticationSchemeNTLM
 *  
 *  Discussion:
 *	HTTP NTLM authentication scheme.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.5 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFHTTPAuthenticationSchemeNTLM: CFStringRef; external name '_kCFHTTPAuthenticationSchemeNTLM'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5,__IPHONE_2_0) *)

{
 *  kCFHTTPAuthenticationSchemeNegotiate
 *  
 *  Discussion:
 *	HTTP Negotiate authentication scheme.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.5 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
var kCFHTTPAuthenticationSchemeNegotiate: CFStringRef; external name '_kCFHTTPAuthenticationSchemeNegotiate'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5,__IPHONE_2_0) *)


{
 *  CFHTTPMessageRef
 *  
 *  Discussion:
 *	This is the type of a reference to an HTTP message. An HTTP
 *	message can be a request or a response.
 }
type
	CFHTTPMessageRef = ^SInt32; { an opaque type }

{
 *  CFHTTPMessageGetTypeID()
 *  
 *  Discussion:
 *	Return the unique type for this class.
 *  
 *  Mac OS X threading:
 *	Thread safe
 *  
 *  Result:
 *	A unique CFType for CFHTTPMessage.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageGetTypeID: CFTypeID; external name '_CFHTTPMessageGetTypeID';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCreateRequest()
 *  
 *  Discussion:
 *	Create an HTTPMessage from an HTTP method, url and version.
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
 *	requestMethod:
 *	  A pointer to a CFString indicating the method of request. For a
 *	  "GET" request, for example, the value would be CFSTR("GET").
 *	
 *	url:
 *	  A pointer to a CFURL structure created any of the several
 *	  CFURLCreate... functions.  If this parameter is not a pointer
 *	  to a valid CFURL structure, the behavior is undefined.
 *	
 *	httpVersion:
 *	  A pointer to a CFString indicating the version of request.
 *  
 *  Result:
 *	A pointer to the CFHTTPMessage created, or NULL if failed. It is
 *	caller's responsibilty to release the memory allocated for the
 *	message.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCreateRequest( alloc: CFAllocatorRef; requestMethod: CFStringRef; url: CFURLRef; httpVersion: CFStringRef ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateRequest';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCreateResponse()
 *  
 *  Discussion:
 *	Create an HTTPMessage from an HTTP status code, description and
 *	version.
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
 *	statusCode:
 *	  An integer status code for the response.
 *	
 *	statusDescription:
 *	  A pointer to a CFString for the status. Pass NULL to use the
 *	  standard description for the given status code, as found in RFC
 *	  2616.
 *	
 *	httpVersion:
 *	  A pointer to a CFString for the HTTP version.
 *  
 *  Result:
 *	A pointer to the CFHTTPMessage created, or NULL if failed. It is
 *	caller's responsibilty to release the memory allocated for the
 *	message.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCreateResponse( alloc: CFAllocatorRef; statusCode: CFIndex; statusDescription: CFStringRef; httpVersion: CFStringRef ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateResponse';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCreateEmpty()
 *  
 *  Discussion:
 *	Creates an empty request or response, which you can then append
 *	bytes to via CFHTTPMessageAppendBytes().
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
 *	isRequest:
 *	  A boolean. Pass kCFBooleanTrue if the message should be a
 *	  request.
 *  
 *  Result:
 *	A pointer to the CFHTTPMessage created, or NULL if failed. It is
 *	caller's responsibilty to release the memory allocated for the
 *	message.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCreateEmpty( alloc: CFAllocatorRef; isRequest: Boolean ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateEmpty';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCreateCopy()
 *  
 *  Discussion:
 *	Creates a copy of a CFHTTPMessage.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	alloc:
 *	  A pointer to the CFAllocator which should be used to allocate
 *	  memory for the CF read stream and its storage for values. If
 *	  this reference is not a valid CFAllocator, the behavior is
 *	  undefined.
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A pointer to the CFHTTPMessage created, or NULL if failed. It is
 *	caller's responsibilty to release the memory allocated for the
 *	message.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCreateCopy( alloc: CFAllocatorRef; message: CFHTTPMessageRef ): CFHTTPMessageRef; external name '_CFHTTPMessageCreateCopy';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageIsRequest()
 *  
 *  Discussion:
 *	Returns whether the CFHTTPMessage is a request or a response.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A Boolean. A value of kCFBooleanTrue indicates the message is a
 *	request. A value of kCFBooleanFalse indicates the message is a
 *	response.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageIsRequest( message: CFHTTPMessageRef ): Boolean; external name '_CFHTTPMessageIsRequest';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCopyVersion()
 *  
 *  Discussion:
 *	Returns the HTTP version.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A pointer to a CFString, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the string.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopyVersion( message: CFHTTPMessageRef ): CFStringRef; external name '_CFHTTPMessageCopyVersion';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCopyBody()
 *  
 *  Discussion:
 *	Returns the body of the message.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A pointer to a CFData, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the data.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopyBody( message: CFHTTPMessageRef ): CFDataRef; external name '_CFHTTPMessageCopyBody';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageSetBody()
 *  
 *  Discussion:
 *	Sets the body of the message from a CFData.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *	
 *	bodyData:
 *	  A pointer to a CFData containing the body to be set. If the
 *	  bodyData is NULL, the behavior is undefined.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFHTTPMessageSetBody( message: CFHTTPMessageRef; bodyData: CFDataRef ); external name '_CFHTTPMessageSetBody';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCopyHeaderFieldValue()
 *  
 *  Discussion:
 *	Returns the specified header field.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *	
 *	headerField:
 *	  A pointer to the CFString. If the headerField is NULL, the
 *	  behavior is undefined.
 *  
 *  Result:
 *	A pointer to a CFString, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the string.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopyHeaderFieldValue( message: CFHTTPMessageRef; headerField: CFStringRef ): CFStringRef; external name '_CFHTTPMessageCopyHeaderFieldValue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCopyAllHeaderFields()
 *  
 *  Discussion:
 *	Returns a CFDictionary containing all of the header fields.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A pointer to a CFDictionary, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the dictionary.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopyAllHeaderFields( message: CFHTTPMessageRef ): CFDictionaryRef; external name '_CFHTTPMessageCopyAllHeaderFields';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageSetHeaderFieldValue()
 *  
 *  Discussion:
 *	Sets the value of the specified header field.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *	
 *	headerField:
 *	  A pointer to the CFString. If headerField is NULL, the behavior
 *	  is undefined.
 *	
 *	value:
 *	  A pointer to the CFString containing the value to set. Set the
 *	  value to NULL to remove the header field.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
procedure CFHTTPMessageSetHeaderFieldValue( message: CFHTTPMessageRef; headerField: CFStringRef; value: CFStringRef ); external name '_CFHTTPMessageSetHeaderFieldValue';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageAppendBytes()
 *  
 *  Discussion:
 *	Appends the given bytes to the message given (parsing out any
 *	control information if appropriate).  Returns kCFBooleanFalse if
 *	a parsing error occurs while processing the new data.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *	
 *	newBytes:
 *	  A pointer to the bytes. If newBytes is NULL, the behavior is
 *	  undefined.
 *	
 *	numBytes:
 *	  A CFIndex of the number of bytes to append.
 *  
 *  Result:
 *	A Boolean indicating success or failure.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageAppendBytes( message: CFHTTPMessageRef; newBytes: UnivPtr; numBytes: CFIndex ): Boolean; external name '_CFHTTPMessageAppendBytes';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageIsHeaderComplete()
 *  
 *  Discussion:
 *	Returns whether further header data is expected by the message.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	message:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A Boolean. A value of kCFBooleanTrue indicates the header is
 *	complete and no further data is expected.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageIsHeaderComplete( message: CFHTTPMessageRef ): Boolean; external name '_CFHTTPMessageIsHeaderComplete';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCopySerializedMessage()
 *  
 *  Discussion:
 *	Creates a self-contained copy of a CFHTTPMessage. This would be
 *	suitable for persistant storage or for transmitting over the
 *	network independently.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	request:
 *	  A pointer to the CFHTTPMessage to be seralized.
 *  
 *  Result:
 *	A pointer to a CFData, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the data.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopySerializedMessage( request: CFHTTPMessageRef ): CFDataRef; external name '_CFHTTPMessageCopySerializedMessage';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{*******************}
{ Request functions }
{*******************}

{
 *  CFHTTPMessageCopyRequestURL()
 *  
 *  Discussion:
 *	Creates a copy of the request URL.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	request:
 *	  A pointer to the CFHTTPMessage.
 *  
 *  Result:
 *	A pointer to a CFURL, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the url.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopyRequestURL( request: CFHTTPMessageRef ): CFURLRef; external name '_CFHTTPMessageCopyRequestURL';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCopyRequestMethod()
 *  
 *  Discussion:
 *	Creates a copy of the request method.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	request:
 *	  A pointer to the CFHTTPMessage.
 *  
 *  Result:
 *	A pointer to a CFString, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the string.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopyRequestMethod( request: CFHTTPMessageRef ): CFStringRef; external name '_CFHTTPMessageCopyRequestMethod';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageAddAuthentication()
 *  
 *  Discussion:
 *	Adds authentication to the request. Tries to modify request to
 *	contain the authentication information requested by the failed
 *	response (which presumably is a 401 or 407 response).
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	request:
 *	  A pointer to a CFHTTPMessage request.
 *	
 *	authenticationFailureResponse:
 *	  A pointer to a CFHTTPMessage of the failed response.
 *	
 *	username:
 *	  A pointer to a CFString containing the user name to
 *	  authenticate.
 *	
 *	password:
 *	  A pointer to a CFString containing the password of the user.
 *	
 *	authenticationScheme:
 *	  A pointer to a CFString containing the authentication scheme to
 *	  use to authenticate. If authenticationScheme is NULL, strongest
 *	  supported scheme listed authenticationFailureResponse will be
 *	  used.
 *	
 *	forProxy:
 *	  A boolean indicating whether the authentication applies to a
 *	  proxy or not.
 *  
 *  Result:
 *	A pointer to a CFString, or NULL if failed. It is caller's
 *	responsibilty to release the memory allocated for the string.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageAddAuthentication( request: CFHTTPMessageRef; authenticationFailureResponse: CFHTTPMessageRef; username: CFStringRef; password: CFStringRef; authenticationScheme: CFStringRef; forProxy: Boolean ): Boolean; external name '_CFHTTPMessageAddAuthentication';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{********************}
{ Response functions }
{********************}

{
 *  CFHTTPMessageGetResponseStatusCode()
 *  
 *  Discussion:
 *	Returns the status code for the response.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	response:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A UInt32 indicating the status code.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageGetResponseStatusCode( response: CFHTTPMessageRef ): CFIndex; external name '_CFHTTPMessageGetResponseStatusCode';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)


{
 *  CFHTTPMessageCopyResponseStatusLine()
 *  
 *  Discussion:
 *	Returns the status line for the response.
 *  
 *  Mac OS X threading:
 *	Not thread safe
 *  
 *  Parameters:
 *	
 *	response:
 *	  A pointer to the CFHTTPMessage to be copied. If the message is
 *	  NULL, the behavior is undefined.
 *  
 *  Result:
 *	A CFString indicating the status code, or NULL if failed. It is
 *	caller's responsibilty to release the memory allocated for the
 *	string.
 *  
 *  Availability:
 *	Mac OS X:		 in version 10.1 and later in CoreServices.framework
 *	CarbonLib:		not available
 *	Non-Carbon CFM:   not available
 }
function CFHTTPMessageCopyResponseStatusLine( response: CFHTTPMessageRef ): CFStringRef; external name '_CFHTTPMessageCopyResponseStatusLine';
(* __OSX_AVAILABLE_STARTING(__MAC_10_1,__IPHONE_2_0) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

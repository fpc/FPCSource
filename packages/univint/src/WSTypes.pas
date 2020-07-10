{
     File:       OSServices/WSTypes.h
 
     Contains:   *** DEPRECATED *** WebServicesCore Method Invocation API
 
     Copyright:  (c) 2002-2011 Apple Inc. All rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
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

unit WSTypes;
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
uses MacTypes,CFBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}



{
    WSTypes
 }
{
    WebServicesCore error codes
 }


{$ALIGN MAC68K}

const
	errWSInternalError = -65793; { An internal framework error }
	errWSTransportError = -65794; { A network error occured }
	errWSParseError = -65795; { The server response wasn't valid XML }
	errWSTimeoutError = -65796; { The invocation timed out }


{
 *  WSTypeID
 *  
 *  Discussion:
 *    Internally, WebServicesCore uses the following enumeration when
 *    serializing between CoreFoundation and XML types. Because CFTypes
 *    are defined at runtime, it isn't always possible to produce a
 *    static mapping to a particular CFTypeRef.  This enum and
 *    associated API allows for static determination of the expected
 *    serialization.
 }
type
	WSTypeID = SInt32;
const
{
   * No mapping is known for this type
   }
	eWSUnknownType = 0;

  {
   * CFNullRef
   }
	eWSNullType = 1;

  {
   * CFBooleanRef
   }
	eWSBooleanType = 2;

  {
   * CFNumberRef for 8, 16, 32 bit integers
   }
	eWSIntegerType = 3;

  {
   * CFNumberRef for long double real numbers
   }
	eWSDoubleType = 4;

  {
   * CFStringRef
   }
	eWSStringType = 5;

  {
   * CFDateRef
   }
	eWSDateType = 6;

  {
   * CFDataRef
   }
	eWSDataType = 7;

  {
   * CFArrayRef
   }
	eWSArrayType = 8;

  {
   * CFDictionaryRef
   }
	eWSDictionaryType = 9;

type
	WSClientContextRetainCallBackProcPtr = function( info: UnivPtr ): UnivPtr;
	WSClientContextReleaseCallBackProcPtr = procedure( info: UnivPtr );
	WSClientContextCopyDescriptionCallBackProcPtr = function( info: UnivPtr ): CFStringRef;

{
 *  WSClientContext
 *  
 *  Discussion:
 *    Several calls in WebServicesCore take a callback with an optional
 *    context pointer.  The context is copied and the info pointer
 *    retained.  When the callback is made, the info pointer is passed
 *    to the callback.
 }
type
	WSClientContext = record
{
   * set to zero (0)
   }
		version: CFIndex;

  {
   * info pointer to be passed to the callback
   }
		info: UnivPtr;

  {
   * callback made on the info pointer. This field may be NULL.
   }
		retain: WSClientContextRetainCallBackProcPtr;

  {
   * callback made on the info pointer. This field may be NULL.
   }
		release: WSClientContextReleaseCallBackProcPtr;

  {
   * callback made on the info pointer. This field may be NULL.
   }
		copyDescription: WSClientContextCopyDescriptionCallBackProcPtr;
	end;
{
    Web Service protocol types.  These constant strings specify the type
    of web service method invocation created.  These are passed to
    WSMethodInvocationCreate.

    For information on these service types, see:

    XML-RPC:    <http://www.xml-rpc.com/spec/>
    SOAP 1.1:   <http://www.w3.org/TR/SOAP/>
    SOAP 1.2:   <http://www.w3.org/2002/ws/>
}
var kWSXMLRPCProtocol: CFStringRef; external name '_kWSXMLRPCProtocol'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)
var kWSSOAP1999Protocol: CFStringRef; external name '_kWSSOAP1999Protocol'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)
var kWSSOAP2001Protocol: CFStringRef; external name '_kWSSOAP2001Protocol'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSGetWSTypeIDFromCFType()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Returns the WSTypeID associated with CFTypeRef.  There is not a
 *    one to one mapping between CFTypeID and WSTypesID therefore an
 *    actual instance of a CFType must be passed.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      a CFTypeRef object
 *  
 *  Result:
 *    the WSTypeID used in serializing the object.  If no WSTypeID
 *    matches, eWSUnknownType is returned.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSGetWSTypeIDFromCFType( ref: CFTypeRef ): WSTypeID; external name '_WSGetWSTypeIDFromCFType';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSGetCFTypeIDFromWSTypeID()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Returns the CFTypeID that is associated with a given WSTypeID. 
 *    CFTypeIDs are only valid during a particular instance of a
 *    process and should not be used as static values.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    typeID:
 *      a WSTypeID constant
 *  
 *  Result:
 *    a CFTypeID, or 0 if not found
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSGetCFTypeIDFromWSTypeID( typeID: WSTypeID ): CFTypeID; external name '_WSGetCFTypeIDFromWSTypeID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)



{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

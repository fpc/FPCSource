{
     File:       OSServices/WSProtocolHandler.h
 
     Contains:   *** DEPRECATED *** WebServicesCore Method implementation API
 
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

unit WSProtocolHandler;
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
uses MacTypes,CFBase,CFArray,CFData,CFDictionary,CFRunLoop,CFXMLNode,WSTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{
    WSProtocolHandler
}


{
 *  WSProtocolHandlerRef
 *  
 *  Discussion:
 *    a WSProtocolHandlerRef represents an object that translates
 *    dictionaries into web services requests.  Typically, it is used
 *    to implement the server side of a web service by converting XML
 *    into CoreFoundation types, but it can also be used to produced
 *    serialzed web services requests without going through a
 *    WSMethodInvocation. It is created with a string specifying the
 *    web services protocol (XML-RPC or SOAP) and can also be modified
 *    through a set of external properties.
 }
type
	WSProtocolHandlerRef = ^OpaqueWSProtocolHandlerRef; { an opaque type }
	OpaqueWSProtocolHandlerRef = record end;


{
 *  WSProtocolHandlerGetTypeID()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerGetTypeID: CFTypeID; external name '_WSProtocolHandlerGetTypeID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerCreate()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Creates a WSProtocolHandlerRef for use in translating an XML
 *    document.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    allocator:
 *      a CFAllocatorRef used to allocate the protocol handler.
 *    
 *    protocol:
 *      a constant string, defiend in WSMethodInvocation.h, that
 *      determines the type of implementation to create (XML-RPC vs.
 *      SOAP).
 *  
 *  Result:
 *    a WSProtocolHandlerRef, NULL if a parse error occured.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerCreate( allocator: CFAllocatorRef; protocol: CFStringRef ): WSProtocolHandlerRef; external name '_WSProtocolHandlerCreate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerCopyRequestDictionary()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Parse an incoming XML document for the method name and
 *    parameters.  The results are in a dictionory as kWSMethodName (a
 *    CFString), kWSMethodParameters (a CFDict) and
 *    kWSMethodParameterOrder (a CFArray). If there was a parse error,
 *    NULL is returned. Protocol specific additions (eg,
 *    kWSSOAPMessageHeaders) may also be present in the dictionary. 
 *    The dictionary returned also represents the context with which
 *    XML reply documents are created (see
 *    WSProtocolHandlerCreateReply).  The caller must release the
 *    resulting dictionary.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      the protocol handler to use
 *    
 *    data:
 *      the XML document to parse
 *  
 *  Result:
 *    a CFDictionary
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerCopyRequestDictionary( ref: WSProtocolHandlerRef; data: CFDataRef ): CFDictionaryRef; external name '_WSProtocolHandlerCopyRequestDictionary';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerCopyReplyDictionary()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Parse an incoming XML document as if it were the reply of a
 *    method.  The results are the same as the WSMethodInvocationInvoke
 *    response; that is, the reply could be a fault.  If there was a
 *    parse error, NULL is returned.  Protocol specific additions (eg,
 *    kWSSOAPMessageHeaders) may also be present in the dictionary. 
 *    The caller must release the resulting dictionary.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      the protocol handler to use
 *    
 *    methodName:
 *      the method name corrosponding to this result
 *    
 *    data:
 *      the XML document to parse
 *  
 *  Result:
 *    a CFDictionary
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerCopyReplyDictionary( ref: WSProtocolHandlerRef; methodName: CFStringRef; data: CFDataRef ): CFDictionaryRef; external name '_WSProtocolHandlerCopyReplyDictionary';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerCopyReplyDocument()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Creates a Reply XML document for a given WSProtocolHandler and
 *    context dictionary.  Protocol specific addtions (eg,
 *    kWSSOAPMessageHeaders) may also be present in the dictionary.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      the WSProtocolHandler to respond
 *    
 *    methodContext:
 *      the CFDictionary containing the context for this method call,
 *      as returned by WSProtocolHandlerParseRequest
 *    
 *    resultValue:
 *      a CFTypeRef representing the reply data to be serialized
 *  
 *  Result:
 *    a CFDataRef containing the XML response
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerCopyReplyDocument( ref: WSProtocolHandlerRef; methodContext: CFDictionaryRef; resultValue: CFTypeRef ): CFDataRef; external name '_WSProtocolHandlerCopyReplyDocument';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerCopyFaultDocument()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Creates a Fault XML response for a given WSProtocolHandler and
 *    fault details dictionary
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      the WSProtocolHandler
 *    
 *    methodContext:
 *      the CFDictionary containing the context for this method call,
 *      as returned by WSProtocolHandlerParseRequest
 *    
 *    faultDict:
 *      a CFDictionary containing the fault information.  See
 *      WSMethodInvocation.h for valid keys.
 *  
 *  Result:
 *    a CFDataRef containing the XML fault
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerCopyFaultDocument( ref: WSProtocolHandlerRef; methodContext: CFDictionaryRef; faultDict: CFDictionaryRef ): CFDataRef; external name '_WSProtocolHandlerCopyFaultDocument';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerCopyRequestDocument()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Creates an XML request for a given WSProtocolHandler and
 *    parameter list.  This is the request sent to a server.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      the WSProtocolHandler
 *    
 *    methodName:
 *      a CFString of the method name to call
 *    
 *    methodParams:
 *      a CFDictionary containing the parameters to send
 *    
 *    methodParamOrder:
 *      a CFArray, which, if not NULL, specifies the order of the
 *      parameters in the CFDictionary
 *    
 *    methodExtras:
 *      a CFDictionary, which, if not NULL, contains additional
 *      information for the protocol (eg, kWSSoapMessageHeaders)
 *  
 *  Result:
 *    a CFDataRef
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerCopyRequestDocument( ref: WSProtocolHandlerRef; methodName: CFStringRef; methodParams: CFDictionaryRef; methodParamOrder: CFArrayRef; methodExtras: CFDictionaryRef ): CFDataRef; external name '_WSProtocolHandlerCopyRequestDocument';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerCopyProperty()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    returns a property from a protocol handler.  If the result is
 *    NULL, the property doesn't exist.  Being a "Copy" call, you must
 *    release the result.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      the implementation
 *    
 *    propertyName:
 *      the name of the property to retreive
 *  
 *  Result:
 *    the CFTypeRef value of the property, or NULL if the property was
 *    not specified.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function WSProtocolHandlerCopyProperty( ref: WSProtocolHandlerRef; propertyName: CFStringRef ): CFTypeRef; external name '_WSProtocolHandlerCopyProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerSetProperty()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Sets a property to a method implementation.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    ref:
 *      the implementation
 *    
 *    propertyName:
 *      the name of the property to retreive
 *    
 *    propertyValue:
 *      the value to set in the method implementation
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure WSProtocolHandlerSetProperty( ref: WSProtocolHandlerRef; propertyName: CFStringRef; propertyValue: CFTypeRef ); external name '_WSProtocolHandlerSetProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
    Serialization / Deserialization override support.
    
    You can add serialization and deserialization callbacks for custom
    types, or types not otherwise handled by the framework. 
 }


{
 *  WSProtocolHandlerSerializationProcPtr
 *  
 *  Discussion:
 *    Prototypes the callback function for a custom serialization proc.
 *     This callback is called whenever a type has the given CFTypeID. 
 *    The callback should return an XML snippet that will be understood
 *    by the server as a correct serialization for a given type.  If
 *    the callback returns NULL, the default serializer will be used.
 *    Note for for SOAP serializations, the parameter key (element
 *    name) is not part of the callback; it will be substituded for all
 *    occurances of "%@" in the returned string.
 *  
 *  Parameters:
 *    
 *    protocol:
 *      the protocol currently being serialized
 *    
 *    obj:
 *      the CFTypeRef to be serialized
 *    
 *    info:
 *      private callback data
 *  
 *  Result:
 *    a CFStringRef containing valid XML.  The caller of this callback
 *    will release the string.
 }
type
	WSProtocolHandlerSerializationProcPtr = function( protocol: WSProtocolHandlerRef; obj: CFTypeRef; info: UnivPtr ): CFStringRef;

	
	
{
 *  WSProtocolHandlerSetSerializationOverride()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Specifies a callback which will be called to produce the XML that
 *    represents the serialization of a given type ref.  See
 *    WSDescription.h for a list of CFTypes for which there currently
 *    exist serializers.  If your callback returns NULL, the default
 *    serializer will be used.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    protocol:
 *      the protocol
 *    
 *    objType:
 *      the CFTypeID of the object
 *    
 *    serializationProc:
 *      the callback called
 *    
 *    context:
 *      a pointer to a WSClientContext.  The structure will be copied.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure WSProtocolHandlerSetSerializationOverride( protocol: WSProtocolHandlerRef; objType: CFTypeID; serializationProc: WSProtocolHandlerSerializationProcPtr; var context: WSClientContext ); external name '_WSProtocolHandlerSetSerializationOverride';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 *  WSProtocolHandlerDeserializationProcPtr
 *  
 *  Discussion:
 *    Prototypes the callback function for a custom deserializer.  This
 *    callback is passed a reference to the protocol element currently
 *    being executed, the root of the response parse tree, the current
 *    node being deserialized, and a pointer to private data. The
 *    return result should be a valid CFTypeRef object (which will be
 *    released by the caller) or NULL to allow the default deserializer
 *    to act.
 *  
 *  Parameters:
 *    
 *    protocol:
 *      the protocol executing
 *    
 *    msgRoot:
 *      the root tree element
 *    
 *    deserializeRoot:
 *      the tree element that needs to be deserialied
 *    
 *    info:
 *      private callback data
 *  
 *  Result:
 *    a CFTypeRef representing the deserialized data, or NULL to allow
 *    the default deserializers to act.
 }
type
	WSProtocolHandlerDeserializationProcPtr = function( protocol: WSProtocolHandlerRef; msgRoot: CFXMLTreeRef; deserializeRoot: CFXMLTreeRef; info: UnivPtr ): CFTypeRef;

	
	
{
 *  WSProtocolHandlerSetDeserializationOverride()   *** DEPRECATED ***
 *  
 *  Discussion:
 *    Specifies a callback to be made when parsing an XML method
 *    response.  The callback should return a CFTypeRef containing the
 *    deserialized object value.  If the callback returns NULL, the
 *    default deserializer will be used.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    protocol:
 *      the protocol
 *    
 *    typeNamespace:
 *      the fully resolved namespace for a specific type.  If NULL, the
 *      default namespace will be used.  For example, this field could
 *      be: CFSTR("http://www.w3.org/2001/XMLSchema-instance").
 *    
 *    typeName:
 *      the non-qualified type name.  This parameter must not be NULL.
 *    
 *    deserializationProc:
 *      a ProcPtr to be called to perform the deserialization
 *    
 *    context:
 *      a pointer to a WSClientContext.  The structure will be copied.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later but deprecated in 10.8
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure WSProtocolHandlerSetDeserializationOverride( protocol: WSProtocolHandlerRef; typeNamespace: CFStringRef; typeName: CFStringRef; deserializationProc: WSProtocolHandlerDeserializationProcPtr; var context: WSClientContext ); external name '_WSProtocolHandlerSetDeserializationOverride';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3,__MAC_10_8,__IPHONE_NA,__IPHONE_NA) *)


{
 * Properties in the Request Dictionary
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kWSMethodName CFSTRP('/WSMethodName')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kWSMethodParameters CFSTRP('/WSMethodParameters')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kWSMethodParameterOrder CFSTRP('/WSMethodParameterOrder')}
{$endc}

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

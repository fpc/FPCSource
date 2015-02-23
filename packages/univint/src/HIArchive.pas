{
     File:       HIToolbox/HIArchive.h
 
     Contains:   HIArchive Interfaces.
 
     Version:    HIToolbox-624~3
 
     Copyright:  © 2004-2008 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
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

unit HIArchive;
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
{$ifc defined(iphonesim)}
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
{$ifc defined(iphonesim)}
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes,CFBase,CFData,CFNumber,HIObject;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{
 *  HIArchive
 *  
 *  Discussion:
 *    HIArchive provides a standard, extensible mechanism to flatten
 *    objects for storage in memory or on disk for later retrieval or
 *    transfer to another application. The archive is encoded using the
 *    binary property list format. The binary plist can be converted to
 *    text XML with /usr/bin/plutil for development purposes. Details
 *    on how to create an object that supports the HIArchive protocol
 *    are provided in HIToolbox/HIObject.h. 
 *    
 *    When writing data out to an archive, the client must first use
 *    HIArchiveCreateForEncoding to generate the archive into which the
 *    data will be encoded. From there, data may be added to the
 *    archive by calling HIArchiveEncodeBoolean, HIArchiveEncodeNumber,
 *    and HIArchiveEncodeCFType. If HIArchiveEncodeCFType is being
 *    called on one of your custom HIObjects, HIToolbox will send it
 *    the kEventHIObjectEncode event (see HIObject.h). In order to
 *    receive this event your HIObject must first have set its
 *    archiving-ignored value to false via HIObjectSetArchivingIgnored.
 *    This lets HIToolbox know your object supports archiving. The
 *    kEventParamHIArchive parameter contains the HIArchiveRef into
 *    which it should encode all of its relevant state information. All
 *    information added to the archive is written with a key. This key
 *    is used later during the unarchiving process to pull the encoded
 *    data from the archive. System supplied HIObjects namespace their
 *    keys with an HI prefix. Subclasses of system supplied HIObjects
 *    should only use this namespace if explicitly overriding a value
 *    written to the archive by the superclass. Take care to mantain
 *    the same data format when overriding the default to avoid
 *    incompatibilities. When your archiving process is complete,
 *    HIArchiveCopyEncodedData will compress the data into the archive
 *    and return it in a CFDataRef. This CFDataRef can be sent to
 *    another application or written out to disk for later retrieval.
 *    Once the encoded data is compressed, no more data may be added to
 *    the archive. At this point, the HIArchiveRef must be released via
 *    CFRelease. 
 *    
 *    When retrieving data from an archive, the client must first use
 *    HIArchiveCreateForDecoding to create an archive reference capable
 *    of decoding the data from the provided CFDataRef. Given the
 *    HIArchiveRef, data may be pulled from the archive via
 *    HIArchiveDecodeBoolean, HIArchiveDecodeNumber, and
 *    HIArchiveCopyDecodedCFType. If HIArchiveCopyDecodedCFType is
 *    called on one of your custom HIObjects, HIToolbox will send it
 *    the kEventHIObjectInitialize event (see HIOject.h). The
 *    kEventParamHIArchive parameter contains the HIArchiveRef from
 *    which it should decode all of its relevant state information.
 *    Because these data values were written by key, they can be read
 *    in any order regardless of how they were written. This also means
 *    new keyed values can be added without breaking existing decoding
 *    routines. Once all data has been read from the archive, it may
 *    simply be released via CFRelease. 
 *    
 *    For those clients who wish to provide HIArchive editing features
 *    there are a few tricks necessary to achieve the desired behavior.
 *    A generic HIArchive editor will likely be used by clients to edit
 *    objects for which it has no direct knowledge (or which have not
 *    yet been designed). For instance, it may provide users with the
 *    ability to edit custom HIViews, including generic functionality
 *    to set the view's class identifier, title, frame, etc. In this
 *    case, it is necessary to instantiate the superclass
 *    ("com.apple.hiview") of the custom view object because the custom
 *    view class itself hasn't been registered within the editor.
 *    
 *    
 *    After the user has completed editing the object and desires to
 *    write out the archive, the editor must set the custom archive
 *    data to the object with HIObjectSetCustomArchiveData as a
 *    CFDictionary. Standard keys for initialization parameter types,
 *    names and values, class and superclass identifiers and CDEF
 *    procID are provided in HIToolbox/HIObject.h. Of particular
 *    importance are the object's class and superclass identifier.
 *    HIArchive uses these values to instantiate the proper object when
 *    loading the archive within the client's application. The
 *    parameter types, names and values are then automatically passed
 *    to the client object through its initialization event. 
 *    
 *    At this point, the object can simply be written into the archive
 *    with HIArchiveCreateForEncoding and HIArchiveEncodeCFType.
 *    HIArchive will handle writing the appropriate classID based on
 *    the custom data that was assigned earlier. 
 *    
 *    Generic HIArchive editing applications will also need to handle
 *    loading client archives. In this case, the archive is decoded in
 *    editing mode by passing the
 *    kHIArchiveDecodeSuperclassForUnregisteredObjects proxy bit in
 *    HIArchiveCreateForDecoding. When objects not registered in the
 *    current application are decoded, HIArchive will notice this and
 *    look into the custom data for the object's superclass identifier,
 *    instantiate an object of that type instead and attach the custom
 *    data to the newly created object. The editor can then look at the
 *    attached custom data with HIObjectCopyCustomArchiveData and
 *    provide it in the UI for the user to edit.
 }
type
	HIArchiveRef = ^OpaqueHIArchiveRef; { an opaque type }
	OpaqueHIArchiveRef = record end;

{
 *  Discussion:
 *    HIArchive errors
 }
const
{
   * The archive was created specifically for encoding or decoding but
   * passed into a non-corresponding routine.
   }
	hiArchiveTypeMismatchErr = -6780;

  {
   * The keyed value requested does not exist in the archive.
   }
	hiArchiveKeyNotAvailableErr = -6781;

  {
   * HIArchiveCopyEncodedData has been called and no more data may be
   * encoded.
   }
	hiArchiveEncodingCompleteErr = -6782;

  {
   * The HIObject does not support the archiving protocol.
   }
	hiArchiveHIObjectIgnoresArchivingErr = -6783;


{
 *  Discussion:
 *    HIArchiveCreateForDecoding options
 }
const
{
   * kDecodeSuperclassForUnregisteredObjects is passed to
   * HIArchiveCreateForDecoding indicating that if an HIObject's class
   * has not been registered before it is pulled from the archive,
   * HIArchiveCopyDecodedCFType will automatically instantiate the
   * unarchived object as its superclass if it exists. For instance, a
   * custom HIView of class "com.myco.customview" being unarchived will
   * be instantiated as class "com.apple.hiview" if your app has not
   * yet registered "com.myco.customview". This is useful for archive
   * editors that do not implement all objects contained in a client
   * archive. Note that only data written to the archive by the
   * superclass will be decoded. All data unique to the unregistered
   * subclass will be ignored. This option also signals the HIObject to
   * load its custom archive data so it can be accessed via
   * HIObjectCopyCustomArchiveData. HIArchive is unable to instantiate
   * unregistered objects whose superclasses are also unregistered.
   }
	kHIArchiveDecodeSuperclassForUnregisteredObjects = 1 shl 0;

  {
   * Indicates that an archive is being decoded by an archive editor.
   * This information is passed to the object being decoded via the
   * kEventParamDecodingForEditor parameter in the
   * kEventHIObjectInitialize and kEventHIObjectCreatedFromArchive
   * event. This option may be used in Mac OS X 10.5 and later.
   }
	kHIArchiveDecodingForEditor = 1 shl 1;

{$ifc not TARGET_CPU_64}
{
 *  HIArchiveGetTypeID()
 *  
 *  Summary:
 *    Returns the CFType identifier for an HIArchive object.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Result:
 *    A CFTypeID unique to HIArchive instances.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveGetTypeID: CFTypeID; external name '_HIArchiveGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveCreateForEncoding()
 *  
 *  Summary:
 *    Creates an HIArchive for use in encoding object information.
 *  
 *  Discussion:
 *    The created HIArchiveRef is a CFType and must be released via
 *    CFRelease.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    outEncoder:
 *      An HIArchive reference which receives the created HIArchive on
 *      return.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveCreateForEncoding( var outEncoder: HIArchiveRef ): OSStatus; external name '_HIArchiveCreateForEncoding';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveEncodeBoolean()
 *  
 *  Summary:
 *    Adds a keyed boolean value to the provided archive.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inEncoder:
 *      An HIArchiveRef to which the boolean value is added.
 *    
 *    inKey:
 *      The key associated with the boolean value used for later
 *      decoding.
 *    
 *    inBoolean:
 *      The boolean value to be encoded.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveEncodeBoolean( inEncoder: HIArchiveRef; inKey: CFStringRef; inBoolean: Boolean ): OSStatus; external name '_HIArchiveEncodeBoolean';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveEncodeNumber()
 *  
 *  Summary:
 *    Adds a keyed number value to the provided archive.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inEncoder:
 *      An HIArchiveRef to which the number value is added.
 *    
 *    inKey:
 *      The key associated with the number value used for later
 *      decoding.
 *    
 *    inNumberType:
 *      A CFNumberType describing the type of number value being
 *      encoded.
 *    
 *    inNumberValue:
 *      The number value to be encoded.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveEncodeNumber( inEncoder: HIArchiveRef; inKey: CFStringRef; inNumberType: CFNumberType; inNumberValue: {const} UnivPtr ): OSStatus; external name '_HIArchiveEncodeNumber';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveEncodeCFType()
 *  
 *  Summary:
 *    Adds a keyed CFType to the provided archive.
 *  
 *  Discussion:
 *    Encodes basic and property list based CFTypes including HIObjects
 *    supporting the archiving protocol. If the HIObject supports the
 *    archiving protocol, it will receive the kEventHIObjectEncode
 *    event during which it should encode all relevant state
 *    information. The kEventParamHIArchive parameter contains the
 *    archive into which data should be added.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inEncoder:
 *      An HIArchiveRef to which the CFType is added.
 *    
 *    inKey:
 *      The key associated with the CFType used for later decoding.
 *    
 *    inCFType:
 *      The CFType to be encoded.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveEncodeCFType( inEncoder: HIArchiveRef; inKey: CFStringRef; inCFType: CFTypeRef ): OSStatus; external name '_HIArchiveEncodeCFType';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveCopyEncodedData()
 *  
 *  Summary:
 *    Returns the encoded archive as a CFDataRef.
 *  
 *  Discussion:
 *    Compresses the archived data for storage and returns it as a
 *    CFDataRef. After the archived data is compressed, no further
 *    information may be encoded. Do not call this routine until the
 *    encoding process is complete.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inEncoder:
 *      An HIArchiveRef into which the archived data was compiled.
 *    
 *    outData:
 *      A CFData reference which receives the compressed archive data
 *      on return.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveCopyEncodedData( inEncoder: HIArchiveRef; var outData: CFDataRef ): OSStatus; external name '_HIArchiveCopyEncodedData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveCreateForDecoding()
 *  
 *  Summary:
 *    Creates an HIArchive for use in decoding the object information
 *    contained in the provided CFData reference.
 *  
 *  Discussion:
 *    The created HIArchiveRef is a CFType and must be released via
 *    CFRelease.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inData:
 *      A CFDataRef containing a previously encoded archive.
 *    
 *    inOptions:
 *      The only option supported by this routine at present is
 *      kHIArchiveDecodeSuperclassForUnregisteredObjects. You may also
 *      pass zero for this parameter to get the default behavior.
 *    
 *    outDecoder:
 *      An HIArchive reference which receives the created HIArchive on
 *      return.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveCreateForDecoding( inData: CFDataRef; inOptions: OptionBits; var outDecoder: HIArchiveRef ): OSStatus; external name '_HIArchiveCreateForDecoding';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveDecodeBoolean()
 *  
 *  Summary:
 *    Pulls a keyed boolean value from the provided archive.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inDecoder:
 *      An HIArchiveRef from which the boolean value is pulled.
 *    
 *    inKey:
 *      The key associated with the boolean value used while encoding.
 *    
 *    outBoolean:
 *      The boolean value to be decoded.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveDecodeBoolean( inDecoder: HIArchiveRef; inKey: CFStringRef; var outBoolean: Boolean ): OSStatus; external name '_HIArchiveDecodeBoolean';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveDecodeNumber()
 *  
 *  Summary:
 *    Pulls a keyed number value from the provided archive.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inDecoder:
 *      An HIArchiveRef from which the number value is pulled.
 *    
 *    inKey:
 *      The key associated with the number value used while encoding.
 *    
 *    inNumberType:
 *      A CFNumberType describing the type of number value being
 *      encoded.
 *    
 *    outNumberValue:
 *      The number value to be decoded.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveDecodeNumber( inDecoder: HIArchiveRef; inKey: CFStringRef; inNumberType: CFNumberType; outNumberValue: UnivPtr ): OSStatus; external name '_HIArchiveDecodeNumber';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIArchiveCopyDecodedCFType()
 *  
 *  Summary:
 *    Pulls a keyed CFType from the provided archive.
 *  
 *  Discussion:
 *    Decodes basic and property list based CFTypes and HIObjects. If
 *    the CFType is an HIObject, it will be constructed and receive the
 *    kEventHIObjectInitialize event. The kEventParamHIArchive
 *    parameter contains the archive from which data should be
 *    retrieved.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inDecoder:
 *      An HIArchiveRef from which the CFType value is pulled.
 *    
 *    inKey:
 *      The key associated with the CFType used while encoding.
 *    
 *    outCFType:
 *      The CFType to be decoded.
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIArchiveCopyDecodedCFType( inDecoder: HIArchiveRef; inKey: CFStringRef; var outCFType: CFTypeRef ): OSStatus; external name '_HIArchiveCopyDecodedCFType';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

{
     File:       CarbonCore/TextEncodingConverter.h
 
     Contains:   Text Encoding Conversion Interfaces.
 
     Copyright:  © 1994-2011 Apple Inc. All rights reserved.
 
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

unit TextEncodingConverter;
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
uses MacTypes,TextCommon,CFBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}


{$ALIGN MAC68K}

type
	TECPluginSignature = OSType;
	TECPluginVersion = UInt32;
{ plugin signatures }
const
	kTECSignature = FourCharCode('encv');
	kTECUnicodePluginSignature = FourCharCode('puni');
	kTECJapanesePluginSignature = FourCharCode('pjpn');
	kTECChinesePluginSignature = FourCharCode('pzho');
	kTECKoreanPluginSignature = FourCharCode('pkor');


{ converter object reference }
type
	TECObjectRef = ^SInt32; { an opaque type }
	TECObjectRefPtr = ^TECObjectRef;  { when a var xx:TECObjectRef parameter can be nil, it is changed to xx: TECObjectRefPtr }
	TECSnifferObjectRef = ^SInt32; { an opaque type }
	TECSnifferObjectRefPtr = ^TECSnifferObjectRef;  { when a var xx:TECSnifferObjectRef parameter can be nil, it is changed to xx: TECSnifferObjectRefPtr }
	TECPluginSig = OSType;
	TECConversionInfoPtr = ^TECConversionInfo;
	TECConversionInfo = record
		sourceEncoding: TextEncoding;
		destinationEncoding: TextEncoding;
		reserved1: UInt16;
		reserved2: UInt16;
	end;

{
 *  TECInternetNameUsageMask
 *  
 *  Discussion:
 *    Mask values that control the mapping between TextEncoding and
 *    IANA charset name or MIB enum.
 }
type
	TECInternetNameUsageMask = UInt32;
const
{ Use one of the following}

  {
   * Use the default type of mapping given other usage information
   * (none currently defined).
   }
	kTECInternetNameDefaultUsageMask = 0;

  {
   * Use the closest possible match between TextEncoding value and IANA
   * charset name or MIB enum
   }
	kTECInternetNameStrictUsageMask = 1;

  {
   * When mapping from IANA charset name or MIB enum to TextEncoding,
   * map to the largest superset of the encoding specified by the
   * charset name or MIB enum (i.e. be tolerant). When mapping from
   * TextEncoding to IANA charset name or MIB enum, typically map to
   * the most generic or widely recognized charset name or MIB enum.
   }
	kTECInternetNameTolerantUsageMask = 2;

{ Special values for MIB enums }
const
	kTEC_MIBEnumDontCare = -1;

{ Additional control flags for TECSetBasicOptions }
const
	kTECDisableFallbacksBit = 16;
	kTECDisableLooseMappingsBit = 17;

const
	kTECDisableFallbacksMask = 1 shl kTECDisableFallbacksBit;
	kTECDisableLooseMappingsMask = 1 shl kTECDisableLooseMappingsBit;


{ return number of encodings types supported by user's configuraton of the encoding converter }
{
 *  TECCountAvailableTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 }
function TECCountAvailableTextEncodings( var numberEncodings: ItemCount ): OSStatus; external name '_TECCountAvailableTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ fill in an array of type TextEncoding passed in by the user with types of encodings the current configuration of the encoder can handle. }
{
 *  TECGetAvailableTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECGetAvailableTextEncodings( availableEncodings: {variable-size-array} TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount ): OSStatus; external name '_TECGetAvailableTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ return number of from-to encoding conversion pairs supported  }
{
 *  TECCountDirectTextEncodingConversions()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCountDirectTextEncodingConversions( var numberOfEncodings: ItemCount ): OSStatus; external name '_TECCountDirectTextEncodingConversions';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ fill in an array of type TextEncodingPair passed in by the user with types of encoding pairs the current configuration of the encoder can handle. }
{
 *  TECGetDirectTextEncodingConversions()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECGetDirectTextEncodingConversions( availableConversions: {variable-size-array} TECConversionInfoPtr; maxAvailableConversions: ItemCount; var actualAvailableConversions: ItemCount ): OSStatus; external name '_TECGetDirectTextEncodingConversions';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ return number of encodings a given encoding can be converter into }
{
 *  TECCountDestinationTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCountDestinationTextEncodings( inputEncoding: TextEncoding; var numberOfEncodings: ItemCount ): OSStatus; external name '_TECCountDestinationTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ fill in an array of type TextEncodingPair passed in by the user with types of encodings pairs the current configuration of the encoder can handle. }
{
 *  TECGetDestinationTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECGetDestinationTextEncodings( inputEncoding: TextEncoding; destinationEncodings: {variable-size-array} TextEncodingPtr; maxDestinationEncodings: ItemCount; var actualDestinationEncodings: ItemCount ): OSStatus; external name '_TECGetDestinationTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ get info about a text encoding }
{
 *  TECGetTextEncodingInternetName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 }
function TECGetTextEncodingInternetName( textEncoding_: TextEncoding; var encodingName: Str255 ): OSStatus; external name '_TECGetTextEncodingInternetName';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECGetTextEncodingFromInternetName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 }
function TECGetTextEncodingFromInternetName( var textEncoding_: TextEncoding; const (*var*) encodingName: Str255 ): OSStatus; external name '_TECGetTextEncodingFromInternetName';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ create/dispose converters }
{
 *  TECCreateConverter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 }
function TECCreateConverter( var newEncodingConverter: TECObjectRef; inputEncoding: TextEncoding; outputEncoding: TextEncoding ): OSStatus; external name '_TECCreateConverter';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECCreateConverterFromPath()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCreateConverterFromPath( var newEncodingConverter: TECObjectRef; {const} inPath: {variable-size-array} TextEncodingPtr; inEncodings: ItemCount ): OSStatus; external name '_TECCreateConverterFromPath';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECDisposeConverter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 }
function TECDisposeConverter( newEncodingConverter: TECObjectRef ): OSStatus; external name '_TECDisposeConverter';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ convert text encodings }
{
 *  TECClearConverterContextInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECClearConverterContextInfo( encodingConverter: TECObjectRef ): OSStatus; external name '_TECClearConverterContextInfo';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECConvertText()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECConvertText( encodingConverter: TECObjectRef; inputBuffer: ConstTextPtr; inputBufferLength: ByteCount; var actualInputLength: ByteCount; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount ): OSStatus; external name '_TECConvertText';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECFlushText()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECFlushText( encodingConverter: TECObjectRef; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount ): OSStatus; external name '_TECFlushText';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ one-to-many routines }
{
 *  TECCountSubTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCountSubTextEncodings( inputEncoding: TextEncoding; var numberOfEncodings: ItemCount ): OSStatus; external name '_TECCountSubTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECGetSubTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECGetSubTextEncodings( inputEncoding: TextEncoding; subEncodings: {variable-size-array} TextEncodingPtr; maxSubEncodings: ItemCount; var actualSubEncodings: ItemCount ): OSStatus; external name '_TECGetSubTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECGetEncodingList()
 *  
 *  Parameters:
 *    
 *    encodingConverter:
 *      The encodingConverter to return the encoding list for
 *    
 *    numEncodings:
 *      On exit, the number of encodings in encodingList
 *    
 *    encodingList:
 *      On exit, a handle containing numEncodings values of type
 *      TextEncoding, for each known encoding.  Do not dispose of this
 *      handle.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 }
function TECGetEncodingList( encodingConverter: TECObjectRef; var numEncodings: ItemCount; var encodingList: Handle ): OSStatus; external name '_TECGetEncodingList';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECCreateOneToManyConverter()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCreateOneToManyConverter( var newEncodingConverter: TECObjectRef; inputEncoding: TextEncoding; numOutputEncodings: ItemCount; {const} outputEncodings: {variable-size-array} TextEncodingPtr ): OSStatus; external name '_TECCreateOneToManyConverter';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECConvertTextToMultipleEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECConvertTextToMultipleEncodings( encodingConverter: TECObjectRef; inputBuffer: ConstTextPtr; inputBufferLength: ByteCount; var actualInputLength: ByteCount; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount; outEncodingsBuffer: {variable-size-array} TextEncodingRunPtr; maxOutEncodingRuns: ItemCount; var actualOutEncodingRuns: ItemCount ): OSStatus; external name '_TECConvertTextToMultipleEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECFlushMultipleEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECFlushMultipleEncodings( encodingConverter: TECObjectRef; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount; outEncodingsBuffer: {variable-size-array} TextEncodingRunPtr; maxOutEncodingRuns: ItemCount; var actualOutEncodingRuns: ItemCount ): OSStatus; external name '_TECFlushMultipleEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ international internet info }
{
 *  TECCountWebTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCountWebTextEncodings( locale: RegionCode; var numberEncodings: ItemCount ): OSStatus; external name '_TECCountWebTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECGetWebTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECGetWebTextEncodings( locale: RegionCode; availableEncodings: {variable-size-array} TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount ): OSStatus; external name '_TECGetWebTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECCountMailTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCountMailTextEncodings( locale: RegionCode; var numberEncodings: ItemCount ): OSStatus; external name '_TECCountMailTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECGetMailTextEncodings()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECGetMailTextEncodings( locale: RegionCode; availableEncodings: {variable-size-array} TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount ): OSStatus; external name '_TECGetMailTextEncodings';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ examine text encodings }
{
 *  TECCountAvailableSniffers()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCountAvailableSniffers( var numberOfEncodings: ItemCount ): OSStatus; external name '_TECCountAvailableSniffers';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECGetAvailableSniffers()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECGetAvailableSniffers( availableSniffers: {variable-size-array} TextEncodingPtr; maxAvailableSniffers: ItemCount; var actualAvailableSniffers: ItemCount ): OSStatus; external name '_TECGetAvailableSniffers';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECCreateSniffer()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECCreateSniffer( var encodingSniffer: TECSnifferObjectRef; {const} testEncodings: {variable-size-array} TextEncodingPtr; numTextEncodings: ItemCount ): OSStatus; external name '_TECCreateSniffer';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECSniffTextEncoding()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECSniffTextEncoding( encodingSniffer: TECSnifferObjectRef; inputBuffer: ConstTextPtr; inputBufferLength: ByteCount; testEncodings: {variable-size-array} TextEncodingPtr; numTextEncodings: ItemCount; numErrsArray: {variable-size-array} ItemCountPtr; maxErrs: ItemCount; numFeaturesArray: {variable-size-array} ItemCountPtr; maxFeatures: ItemCount ): OSStatus; external name '_TECSniffTextEncoding';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECDisposeSniffer()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECDisposeSniffer( encodingSniffer: TECSnifferObjectRef ): OSStatus; external name '_TECDisposeSniffer';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECClearSnifferContextInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 }
function TECClearSnifferContextInfo( encodingSniffer: TECSnifferObjectRef ): OSStatus; external name '_TECClearSnifferContextInfo';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  TECSetBasicOptions()
 *  
 *  Summary:
 *    Sets encodingConverter options affecting
 *    TECConvertText[ToMultipleEncodings].
 *  
 *  Parameters:
 *    
 *    encodingConverter:
 *      The high-level encoding converter object created by
 *      TECCreateConverter or TECCreateOneToManyConverter whose
 *      behavior is to be modified by the options specified in
 *      controlFlags.
 *    
 *    controlFlags:
 *      A bit mask specifying the desired options. The following mask
 *      constants are valid for this parameter; multiple mask constants
 *      may be ORed together to set multiple options; passing 0 for
 *      this parameter clears all options: 
 *      
 *      kUnicodeForceASCIIRangeMask, kUnicodeNoHalfwidthCharsMask
 *      (defined in UnicodeConverter.h) 
 *      
 *      kTECDisableFallbacksMask, kTECDisableLooseMappingsMask (defined
 *      above) - loose and fallback mappings are both enabled by
 *      default for the TextEncodingConverter.h conversion APIs
 *      (TECConvertText, TECConvertTextToMultipleEncodings), unlike the
 *      behavior of the conversion APIs in UnicodeConverter.h. These
 *      options may be used to disable loose and/or fallback mappings
 *      for the TextEncodingConverter.h conversion APIs.
 *  
 *  Result:
 *    The function returns paramErr for invalid masks,
 *    kTECCorruptConverterErr for an invalid encodingConverter, noErr
 *    otherwise.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   in TextEncodingConverter 1.5 and later
 }
function TECSetBasicOptions( encodingConverter: TECObjectRef; controlFlags: OptionBits ): OSStatus; external name '_TECSetBasicOptions';
(* __OSX_AVAILABLE_STARTING(__MAC_10_3, __IPHONE_NA) *)


{ Map TextEncoding values to/from IANA charset names and/or MIB enums, with usage control }
{
 *  TECCopyTextEncodingInternetNameAndMIB()
 *  
 *  Summary:
 *    Converts a TextEncoding value to an IANA charset name and/or a
 *    MIB enum value
 *  
 *  Discussion:
 *    Given a TextEncoding value, this function maps it to an IANA
 *    charset name (if encodingNamePtr is non-NULL) and/or a MIB enum
 *    value (if mibEnumPtr is non-NULL), as specified by the usage
 *    parameter.
 *  
 *  Parameters:
 *    
 *    textEncoding:
 *      A TextEncoding value to map to a charset name and/or MIB enum.
 *    
 *    usage:
 *      Specifies the type of mapping desired (see
 *      TECInternetNameUsageMask above).
 *    
 *    encodingNamePtr:
 *      If non-NULL, is a pointer to a CStringRef for an immutable
 *      CFString created by this function; when the caller is finished
 *      with it, the caller must dispose of it by calling CFRelease.
 *    
 *    mibEnumPtr:
 *      If non-NULL, is a pointer to an SInt32 that will be set to the
 *      appropriate MIB enum value, or to 0 (or kTEC_MIBEnumDontCare)
 *      if there is no appropriate MIB enum value (valid MIB enums
 *      begin at 3).
 *  
 *  Result:
 *    The function returns paramErr if encodingNamePtr and mibEnumPtr
 *    are both NULL. It returns kTextUnsupportedEncodingErr if it has
 *    no data for the supplied textEncoding. It returns noErr if it
 *    found useful data.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function TECCopyTextEncodingInternetNameAndMIB( textEncoding_: TextEncoding; usage: TECInternetNameUsageMask; encodingNamePtr: CFStringRefPtr { can be NULL }; mibEnumPtr: SInt32Ptr { can be NULL } ): OSStatus; external name '_TECCopyTextEncodingInternetNameAndMIB';
(* __OSX_AVAILABLE_STARTING(__MAC_10_3, __IPHONE_NA) *)


{
 *  TECGetTextEncodingFromInternetNameOrMIB()
 *  
 *  Summary:
 *    Converts an IANA charset name or a MIB enum value to a
 *    TextEncoding value
 *  
 *  Discussion:
 *    If encodingName is non-NULL, this function treats it as an IANA
 *    charset name and maps it to a TextEncoding value; in this case
 *    mibEnum is ignored, and may be set to kTEC_MIBEnumDontCare.
 *    Otherwise, this function maps the mibEnum to a TextEncoding
 *    value. In either case, the mapping is controlled by the usage
 *    parameter. The textEncodingPtr parameter must be non-NULL.
 *  
 *  Result:
 *    The function returns paramErr if textEncodingPtr is NULL. It
 *    returns kTextUnsupportedEncodingErr if it has no data for the
 *    supplied encodingName or mibEnum. It returns noErr if it found
 *    useful data.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function TECGetTextEncodingFromInternetNameOrMIB( var textEncodingPtr: TextEncoding; usage: TECInternetNameUsageMask; encodingName: CFStringRef; mibEnum: SInt32 ): OSStatus; external name '_TECGetTextEncodingFromInternetNameOrMIB';
(* __OSX_AVAILABLE_STARTING(__MAC_10_3, __IPHONE_NA) *)


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

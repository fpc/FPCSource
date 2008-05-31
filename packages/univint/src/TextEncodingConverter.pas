{
     File:       TextEncodingConverter.p
 
     Contains:   Text Encoding Conversion Interfaces.
 
     Version:    Technology: Mac OS 9.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1994-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit TextEncodingConverter;
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
uses MacTypes,TextCommon;


{$ALIGN MAC68K}


type
	TECPluginSignature					= OSType;
	TECPluginVersion					= UInt32;
	{	 plugin signatures 	}

const
	kTECSignature				= FourCharCode('encv');
	kTECUnicodePluginSignature	= FourCharCode('puni');
	kTECJapanesePluginSignature	= FourCharCode('pjpn');
	kTECChinesePluginSignature	= FourCharCode('pzho');
	kTECKoreanPluginSignature	= FourCharCode('pkor');


	{	 converter object reference 	}

type
	TECObjectRef    = ^SInt32; { an opaque 32-bit type }
	TECObjectRefPtr = ^TECObjectRef;  { when a var xx:TECObjectRef parameter can be nil, it is changed to xx: TECObjectRefPtr }
	TECSnifferObjectRef    = ^SInt32; { an opaque 32-bit type }
	TECSnifferObjectRefPtr = ^TECSnifferObjectRef;  { when a var xx:TECSnifferObjectRef parameter can be nil, it is changed to xx: TECSnifferObjectRefPtr }
	TECPluginSig						= OSType;
	TECConversionInfoPtr = ^TECConversionInfo;
	TECConversionInfo = record
		sourceEncoding:			TextEncoding;
		destinationEncoding:	TextEncoding;
		reserved1:				UInt16;
		reserved2:				UInt16;
	end;

	{	 return number of encodings types supported by user's configuraton of the encoding converter 	}
	{
	 *  TECCountAvailableTextEncodings()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function TECCountAvailableTextEncodings(var numberEncodings: ItemCount): OSStatus; external name '_TECCountAvailableTextEncodings';

{ fill in an array of type TextEncoding passed in by the user with types of encodings the current configuration of the encoder can handle. }
{
 *  TECGetAvailableTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetAvailableTextEncodings(availableEncodings: TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus; external name '_TECGetAvailableTextEncodings';

{ return number of from-to encoding conversion pairs supported  }
{
 *  TECCountDirectTextEncodingConversions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCountDirectTextEncodingConversions(var numberOfEncodings: ItemCount): OSStatus; external name '_TECCountDirectTextEncodingConversions';

{ fill in an array of type TextEncodingPair passed in by the user with types of encoding pairs the current configuration of the encoder can handle. }
{
 *  TECGetDirectTextEncodingConversions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetDirectTextEncodingConversions(availableConversions: TECConversionInfoPtr; maxAvailableConversions: ItemCount; var actualAvailableConversions: ItemCount): OSStatus; external name '_TECGetDirectTextEncodingConversions';

{ return number of encodings a given encoding can be converter into }
{
 *  TECCountDestinationTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCountDestinationTextEncodings(inputEncoding: TextEncoding; var numberOfEncodings: ItemCount): OSStatus; external name '_TECCountDestinationTextEncodings';

{ fill in an array of type TextEncodingPair passed in by the user with types of encodings pairs the current configuration of the encoder can handle. }
{
 *  TECGetDestinationTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetDestinationTextEncodings(inputEncoding: TextEncoding; destinationEncodings: TextEncodingPtr; maxDestinationEncodings: ItemCount; var actualDestinationEncodings: ItemCount): OSStatus; external name '_TECGetDestinationTextEncodings';

{ get info about a text encoding }
{
 *  TECGetTextEncodingInternetName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetTextEncodingInternetName(textEncoding_: TextEncoding; var encodingName: Str255): OSStatus; external name '_TECGetTextEncodingInternetName';

{
 *  TECGetTextEncodingFromInternetName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetTextEncodingFromInternetName(var textEncoding_: TextEncoding; const (*var*) encodingName: Str255): OSStatus; external name '_TECGetTextEncodingFromInternetName';

{ create/dispose converters }
{
 *  TECCreateConverter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCreateConverter(var newEncodingConverter: TECObjectRef; inputEncoding: TextEncoding; outputEncoding: TextEncoding): OSStatus; external name '_TECCreateConverter';

{
 *  TECCreateConverterFromPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCreateConverterFromPath(var newEncodingConverter: TECObjectRef; inPath: TextEncodingPtr; inEncodings: ItemCount): OSStatus; external name '_TECCreateConverterFromPath';

{
 *  TECDisposeConverter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECDisposeConverter(newEncodingConverter: TECObjectRef): OSStatus; external name '_TECDisposeConverter';

{ convert text encodings }
{
 *  TECClearConverterContextInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECClearConverterContextInfo(encodingConverter: TECObjectRef): OSStatus; external name '_TECClearConverterContextInfo';

{
 *  TECConvertText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECConvertText(encodingConverter: TECObjectRef; inputBuffer: ConstTextPtr; inputBufferLength: ByteCount; var actualInputLength: ByteCount; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount): OSStatus; external name '_TECConvertText';

{
 *  TECFlushText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECFlushText(encodingConverter: TECObjectRef; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount): OSStatus; external name '_TECFlushText';

{ one-to-many routines }
{
 *  TECCountSubTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCountSubTextEncodings(inputEncoding: TextEncoding; var numberOfEncodings: ItemCount): OSStatus; external name '_TECCountSubTextEncodings';

{
 *  TECGetSubTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetSubTextEncodings(inputEncoding: TextEncoding; subEncodings: TextEncodingPtr; maxSubEncodings: ItemCount; var actualSubEncodings: ItemCount): OSStatus; external name '_TECGetSubTextEncodings';

{
 *  TECGetEncodingList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetEncodingList(encodingConverter: TECObjectRef; var numEncodings: ItemCount; var encodingList: Handle): OSStatus; external name '_TECGetEncodingList';

{
 *  TECCreateOneToManyConverter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCreateOneToManyConverter(var newEncodingConverter: TECObjectRef; inputEncoding: TextEncoding; numOutputEncodings: ItemCount; outputEncodings: TextEncodingPtr): OSStatus; external name '_TECCreateOneToManyConverter';

{
 *  TECConvertTextToMultipleEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECConvertTextToMultipleEncodings(encodingConverter: TECObjectRef; inputBuffer: ConstTextPtr; inputBufferLength: ByteCount; var actualInputLength: ByteCount; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount; var outEncodingsBuffer: TextEncodingRun; maxOutEncodingRuns: ItemCount; var actualOutEncodingRuns: ItemCount): OSStatus; external name '_TECConvertTextToMultipleEncodings';

{
 *  TECFlushMultipleEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECFlushMultipleEncodings(encodingConverter: TECObjectRef; outputBuffer: TextPtr; outputBufferLength: ByteCount; var actualOutputLength: ByteCount; var outEncodingsBuffer: TextEncodingRun; maxOutEncodingRuns: ItemCount; var actualOutEncodingRuns: ItemCount): OSStatus; external name '_TECFlushMultipleEncodings';

{ international internet info }
{
 *  TECCountWebTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCountWebTextEncodings(locale: RegionCode; var numberEncodings: ItemCount): OSStatus; external name '_TECCountWebTextEncodings';

{
 *  TECGetWebTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetWebTextEncodings(locale: RegionCode; availableEncodings: TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus; external name '_TECGetWebTextEncodings';

{
 *  TECCountMailTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCountMailTextEncodings(locale: RegionCode; var numberEncodings: ItemCount): OSStatus; external name '_TECCountMailTextEncodings';

{
 *  TECGetMailTextEncodings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetMailTextEncodings(locale: RegionCode; availableEncodings: TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus; external name '_TECGetMailTextEncodings';

{ examine text encodings }
{
 *  TECCountAvailableSniffers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCountAvailableSniffers(var numberOfEncodings: ItemCount): OSStatus; external name '_TECCountAvailableSniffers';

{
 *  TECGetAvailableSniffers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECGetAvailableSniffers(availableSniffers: TextEncodingPtr; maxAvailableSniffers: ItemCount; var actualAvailableSniffers: ItemCount): OSStatus; external name '_TECGetAvailableSniffers';

{
 *  TECCreateSniffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECCreateSniffer(var encodingSniffer: TECSnifferObjectRef; testEncodings: TextEncodingPtr; numTextEncodings: ItemCount): OSStatus; external name '_TECCreateSniffer';

{
 *  TECSniffTextEncoding()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECSniffTextEncoding(encodingSniffer: TECSnifferObjectRef; inputBuffer: TextPtr; inputBufferLength: ByteCount; testEncodings: TextEncodingPtr; numTextEncodings: ItemCount; var numErrsArray: ItemCount; maxErrs: ItemCount; var numFeaturesArray: ItemCount; maxFeatures: ItemCount): OSStatus; external name '_TECSniffTextEncoding';

{
 *  TECDisposeSniffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECDisposeSniffer(encodingSniffer: TECSnifferObjectRef): OSStatus; external name '_TECDisposeSniffer';

{
 *  TECClearSnifferContextInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TECClearSnifferContextInfo(encodingSniffer: TECSnifferObjectRef): OSStatus; external name '_TECClearSnifferContextInfo';

{$ifc CALL_NOT_IN_CARBON}
{
 *  TECSetBasicOptions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in TextEncodingConverter 1.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function TECSetBasicOptions(encodingConverter: TECObjectRef; controlFlags: OptionBits): OSStatus; external name '_TECSetBasicOptions';

{$endc}  {CALL_NOT_IN_CARBON}


{$ALIGN MAC68K}


end.

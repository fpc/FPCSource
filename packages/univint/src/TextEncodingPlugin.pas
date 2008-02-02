{
     File:       TextEncodingPlugin.p
 
     Contains:   Required interface for Text Encoding Converter-Plugins
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1996-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit TextEncodingPlugin;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,TextCommon,TextEncodingConverter;


{$ALIGN MAC68K}

{
  ####################################################################################
        Constants
  ####################################################################################
}
{
  ####################################################################################
        Structs
  ####################################################################################
}

type
	TECBufferContextRecPtr = ^TECBufferContextRec;
	TECBufferContextRec = record
		textInputBuffer:		TextPtr;
		textInputBufferEnd:		TextPtr;
		textOutputBuffer:		TextPtr;
		textOutputBufferEnd:	TextPtr;
		encodingInputBuffer:	TextEncodingRunPtr;
		encodingInputBufferEnd:	TextEncodingRunPtr;
		encodingOutputBuffer:	TextEncodingRunPtr;
		encodingOutputBufferEnd: TextEncodingRunPtr;
	end;

	TECPluginStateRecPtr = ^TECPluginStateRec;
	TECPluginStateRec = record
		state1:					SInt8;
		state2:					SInt8;
		state3:					SInt8;
		state4:					SInt8;
		longState1:				UInt32;
		longState2:				UInt32;
		longState3:				UInt32;
		longState4:				UInt32;
	end;

	TECConverterContextRecPtr = ^TECConverterContextRec;
	TECConverterContextRec = record
																		{  public - manipulated externally and by plugin }
		pluginRec:				Ptr;
		sourceEncoding:			TextEncoding;
		destEncoding:			TextEncoding;
		reserved1:				UInt32;
		reserved2:				UInt32;
		bufferContext:			TECBufferContextRec;
																		{  private - manipulated only within Plugin }
		contextRefCon:			UInt32;
		conversionProc:			ProcPtr;
		flushProc:				ProcPtr;
		clearContextInfoProc:	ProcPtr;
		options1:				UInt32;
		options2:				UInt32;
		pluginState:			TECPluginStateRec;
	end;

	TECSnifferContextRecPtr = ^TECSnifferContextRec;
	TECSnifferContextRec = record
																		{  public - manipulated externally }
		pluginRec:				Ptr;
		encoding:				TextEncoding;
		maxErrors:				ItemCount;
		maxFeatures:			ItemCount;
		textInputBuffer:		TextPtr;
		textInputBufferEnd:		TextPtr;
		numFeatures:			ItemCount;
		numErrors:				ItemCount;
																		{  private - manipulated only within Plugin }
		contextRefCon:			UInt32;
		sniffProc:				ProcPtr;
		clearContextInfoProc:	ProcPtr;
		pluginState:			TECPluginStateRec;
	end;

	{
	  ####################################################################################
	        Functional Messages
	  ####################################################################################
	}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginNewEncodingConverterPtr = function(var newEncodingConverter: TECObjectRef; var plugContext: TECConverterContextRec; inputEncoding: TextEncoding; outputEncoding: TextEncoding): OSStatus;
{$elsec}
	TECPluginNewEncodingConverterPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginClearContextInfoPtr = function(encodingConverter: TECObjectRef; var plugContext: TECConverterContextRec): OSStatus;
{$elsec}
	TECPluginClearContextInfoPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginConvertTextEncodingPtr = function(encodingConverter: TECObjectRef; var plugContext: TECConverterContextRec): OSStatus;
{$elsec}
	TECPluginConvertTextEncodingPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginFlushConversionPtr = function(encodingConverter: TECObjectRef; var plugContext: TECConverterContextRec): OSStatus;
{$elsec}
	TECPluginFlushConversionPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginDisposeEncodingConverterPtr = function(newEncodingConverter: TECObjectRef; var plugContext: TECConverterContextRec): OSStatus;
{$elsec}
	TECPluginDisposeEncodingConverterPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginNewEncodingSnifferPtr = function(var encodingSniffer: TECSnifferObjectRef; var snifContext: TECSnifferContextRec; inputEncoding: TextEncoding): OSStatus;
{$elsec}
	TECPluginNewEncodingSnifferPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginClearSnifferContextInfoPtr = function(encodingSniffer: TECSnifferObjectRef; var snifContext: TECSnifferContextRec): OSStatus;
{$elsec}
	TECPluginClearSnifferContextInfoPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginSniffTextEncodingPtr = function(encodingSniffer: TECSnifferObjectRef; var snifContext: TECSnifferContextRec): OSStatus;
{$elsec}
	TECPluginSniffTextEncodingPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginDisposeEncodingSnifferPtr = function(encodingSniffer: TECSnifferObjectRef; var snifContext: TECSnifferContextRec): OSStatus;
{$elsec}
	TECPluginDisposeEncodingSnifferPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetCountAvailableTextEncodingsPtr = function(availableEncodings: TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus;
{$elsec}
	TECPluginGetCountAvailableTextEncodingsPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetCountAvailableTextEncodingPairsPtr = function(availableEncodings: TECConversionInfoPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus;
{$elsec}
	TECPluginGetCountAvailableTextEncodingPairsPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetCountDestinationTextEncodingsPtr = function(inputEncoding: TextEncoding; destinationEncodings: TextEncodingPtr; maxDestinationEncodings: ItemCount; var actualDestinationEncodings: ItemCount): OSStatus;
{$elsec}
	TECPluginGetCountDestinationTextEncodingsPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetCountSubTextEncodingsPtr = function(inputEncoding: TextEncoding; subEncodings: TextEncodingPtr; maxSubEncodings: ItemCount; var actualSubEncodings: ItemCount): OSStatus;
{$elsec}
	TECPluginGetCountSubTextEncodingsPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetCountAvailableSniffersPtr = function(availableEncodings: TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus;
{$elsec}
	TECPluginGetCountAvailableSniffersPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetTextEncodingInternetNamePtr = function(textEncoding_: TextEncoding; var encodingName: Str255): OSStatus;
{$elsec}
	TECPluginGetTextEncodingInternetNamePtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetTextEncodingFromInternetNamePtr = function(var textEncoding_: TextEncoding; encodingName: Str255): OSStatus;
{$elsec}
	TECPluginGetTextEncodingFromInternetNamePtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetCountWebEncodingsPtr = function(availableEncodings: TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus;
{$elsec}
	TECPluginGetCountWebEncodingsPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	TECPluginGetCountMailEncodingsPtr = function(availableEncodings: TextEncodingPtr; maxAvailableEncodings: ItemCount; var actualAvailableEncodings: ItemCount): OSStatus;
{$elsec}
	TECPluginGetCountMailEncodingsPtr = ProcPtr;
{$endc}

	{
	  ####################################################################################
	        Dispatch Table Definition
	  ####################################################################################
	}


const
	kTECPluginDispatchTableVersion1 = $00010000;				{  1.0 through 1.0.3 releases }
	kTECPluginDispatchTableVersion1_1 = $00010001;				{  1.1 releases }
	kTECPluginDispatchTableVersion1_2 = $00010002;				{  1.2 releases }
	kTECPluginDispatchTableCurrentVersion = $00010002;


type
	TECPluginDispatchTablePtr = ^TECPluginDispatchTable;
	TECPluginDispatchTable = record
		version:				TECPluginVersion;
		compatibleVersion:		TECPluginVersion;
		PluginID:				TECPluginSignature;
		PluginNewEncodingConverter: TECPluginNewEncodingConverterPtr;
		PluginClearContextInfo:	TECPluginClearContextInfoPtr;
		PluginConvertTextEncoding: TECPluginConvertTextEncodingPtr;
		PluginFlushConversion:	TECPluginFlushConversionPtr;
		PluginDisposeEncodingConverter: TECPluginDisposeEncodingConverterPtr;
		PluginNewEncodingSniffer: TECPluginNewEncodingSnifferPtr;
		PluginClearSnifferContextInfo: TECPluginClearSnifferContextInfoPtr;
		PluginSniffTextEncoding: TECPluginSniffTextEncodingPtr;
		PluginDisposeEncodingSniffer: TECPluginDisposeEncodingSnifferPtr;
		PluginGetCountAvailableTextEncodings: TECPluginGetCountAvailableTextEncodingsPtr;
		PluginGetCountAvailableTextEncodingPairs: TECPluginGetCountAvailableTextEncodingPairsPtr;
		PluginGetCountDestinationTextEncodings: TECPluginGetCountDestinationTextEncodingsPtr;
		PluginGetCountSubTextEncodings: TECPluginGetCountSubTextEncodingsPtr;
		PluginGetCountAvailableSniffers: TECPluginGetCountAvailableSniffersPtr;
		PluginGetCountWebTextEncodings: TECPluginGetCountWebEncodingsPtr;
		PluginGetCountMailTextEncodings: TECPluginGetCountMailEncodingsPtr;
		PluginGetTextEncodingInternetName: TECPluginGetTextEncodingInternetNamePtr;
		PluginGetTextEncodingFromInternetName: TECPluginGetTextEncodingFromInternetNamePtr;
	end;


{$ALIGN MAC68K}


end.

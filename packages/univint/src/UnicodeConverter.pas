{
     File:       UnicodeConverter.p
 
     Contains:   Types, constants, and prototypes for Unicode Converter
 
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

unit UnicodeConverter;
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
uses MacTypes,TextCommon,MixedMode;


{$ALIGN MAC68K}

{ Unicode conversion contexts: }

type
	TextToUnicodeInfo    = ^SInt32; { an opaque 32-bit type }
	TextToUnicodeInfoPtr = ^TextToUnicodeInfo;  { when a var xx:TextToUnicodeInfo parameter can be nil, it is changed to xx: TextToUnicodeInfoPtr }
	UnicodeToTextInfo    = ^SInt32; { an opaque 32-bit type }
	UnicodeToTextInfoPtr = ^UnicodeToTextInfo;  { when a var xx:UnicodeToTextInfo parameter can be nil, it is changed to xx: UnicodeToTextInfoPtr }
	UnicodeToTextRunInfo    = ^SInt32; { an opaque 32-bit type }
	UnicodeToTextRunInfoPtr = ^UnicodeToTextRunInfo;  { when a var xx:UnicodeToTextRunInfo parameter can be nil, it is changed to xx: UnicodeToTextRunInfoPtr }
	ConstTextToUnicodeInfo				= TextToUnicodeInfo;
	ConstUnicodeToTextInfo				= UnicodeToTextInfo;
	{	 UnicodeMapVersion type & values 	}
	UnicodeMapVersion					= SInt32;

const
	kUnicodeUseLatestMapping	= -1;
	kUnicodeUseHFSPlusMapping	= 4;

	{	 Types used in conversion 	}

type
	UnicodeMappingPtr = ^UnicodeMapping;
	UnicodeMapping = record
		unicodeEncoding:		TextEncoding;
		otherEncoding:			TextEncoding;
		mappingVersion:			UnicodeMapVersion;
	end;

	ConstUnicodeMappingPtr				= ^UnicodeMapping;
	{	 Control flags for ConvertFromUnicodeToText and ConvertFromTextToUnicode 	}

const
	kUnicodeUseFallbacksBit		= 0;
	kUnicodeKeepInfoBit			= 1;
	kUnicodeDirectionalityBits	= 2;
	kUnicodeVerticalFormBit		= 4;
	kUnicodeLooseMappingsBit	= 5;
	kUnicodeStringUnterminatedBit = 6;
	kUnicodeTextRunBit			= 7;
	kUnicodeKeepSameEncodingBit	= 8;
	kUnicodeForceASCIIRangeBit	= 9;
	kUnicodeNoHalfwidthCharsBit	= 10;
	kUnicodeTextRunHeuristicsBit = 11;

	kUnicodeUseFallbacksMask	= $00000001;
	kUnicodeKeepInfoMask		= $00000002;
	kUnicodeDirectionalityMask	= $0000000C;
	kUnicodeVerticalFormMask	= $00000010;
	kUnicodeLooseMappingsMask	= $00000020;
	kUnicodeStringUnterminatedMask = $00000040;
	kUnicodeTextRunMask			= $00000080;
	kUnicodeKeepSameEncodingMask = $00000100;
	kUnicodeForceASCIIRangeMask	= $00000200;
	kUnicodeNoHalfwidthCharsMask = $00000400;
	kUnicodeTextRunHeuristicsMask = $00000800;

	{	 Values for kUnicodeDirectionality field 	}
	kUnicodeDefaultDirection	= 0;
	kUnicodeLeftToRight			= 1;
	kUnicodeRightToLeft			= 2;

	{	 Directionality masks for control flags 	}
	kUnicodeDefaultDirectionMask = $00;
	kUnicodeLeftToRightMask		= $04;
	kUnicodeRightToLeftMask		= $08;


	{	 Control flags for TruncateForUnicodeToText: 	}
	{
	   Now TruncateForUnicodeToText uses control flags from the same set as used by
	   ConvertFromTextToUnicode, ConvertFromUnicodeToText, etc., but only
	   kUnicodeStringUnterminatedMask is meaningful for TruncateForUnicodeToText.
	   
	   Previously two special control flags were defined for TruncateForUnicodeToText:
	        kUnicodeTextElementSafeBit = 0
	        kUnicodeRestartSafeBit = 1
	   However, neither of these was implemented.
	   Instead of implementing kUnicodeTextElementSafeBit, we now use
	   kUnicodeStringUnterminatedMask since it accomplishes the same thing and avoids
	   having special flags just for TruncateForUnicodeToText
	   Also, kUnicodeRestartSafeBit is unnecessary, since restart-safeness is handled by
	   setting kUnicodeKeepInfoBit with ConvertFromUnicodeToText.
	   If TruncateForUnicodeToText is called with one or both of the old special control
	   flags set (bits 0 or 1), it will not generate a paramErr, but the old bits have no
	   effect on its operation.
	}

	{	 Filter bits for filter field in QueryUnicodeMappings and CountUnicodeMappings: 	}
	kUnicodeMatchUnicodeBaseBit	= 0;
	kUnicodeMatchUnicodeVariantBit = 1;
	kUnicodeMatchUnicodeFormatBit = 2;
	kUnicodeMatchOtherBaseBit	= 3;
	kUnicodeMatchOtherVariantBit = 4;
	kUnicodeMatchOtherFormatBit	= 5;

	kUnicodeMatchUnicodeBaseMask = $00000001;
	kUnicodeMatchUnicodeVariantMask = $00000002;
	kUnicodeMatchUnicodeFormatMask = $00000004;
	kUnicodeMatchOtherBaseMask	= $00000008;
	kUnicodeMatchOtherVariantMask = $00000010;
	kUnicodeMatchOtherFormatMask = $00000020;

	{	 Control flags for SetFallbackUnicodeToText 	}
	kUnicodeFallbackSequencingBits = 0;

	kUnicodeFallbackSequencingMask = $00000003;
	kUnicodeFallbackInterruptSafeMask = $00000004;				{  To indicate that caller fallback routine doesn’t move memory }

	{	 values for kUnicodeFallbackSequencing field 	}
	kUnicodeFallbackDefaultOnly	= 0;
	kUnicodeFallbackCustomOnly	= 1;
	kUnicodeFallbackDefaultFirst = 2;
	kUnicodeFallbackCustomFirst	= 3;


	{	 Caller-supplied entry point to a fallback handler 	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	UnicodeToTextFallbackProcPtr = function(iSrcUniStr: UniCharPtr; iSrcUniStrLen: ByteCount; var oSrcConvLen: ByteCount; oDestStr: TextPtr; iDestStrLen: ByteCount; var oDestConvLen: ByteCount; iInfoPtr: LogicalAddress; iUnicodeMappingPtr: ConstUnicodeMappingPtr): OSStatus;
{$elsec}
	UnicodeToTextFallbackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	UnicodeToTextFallbackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	UnicodeToTextFallbackUPP = UniversalProcPtr;
{$endc}	

const
	uppUnicodeToTextFallbackProcInfo = $003FFFF0;
	{
	 *  NewUnicodeToTextFallbackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewUnicodeToTextFallbackUPP(userRoutine: UnicodeToTextFallbackProcPtr): UnicodeToTextFallbackUPP; external name '_NewUnicodeToTextFallbackUPP'; { old name was NewUnicodeToTextFallbackProc }
{
 *  DisposeUnicodeToTextFallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeUnicodeToTextFallbackUPP(userUPP: UnicodeToTextFallbackUPP); external name '_DisposeUnicodeToTextFallbackUPP';
{
 *  InvokeUnicodeToTextFallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeUnicodeToTextFallbackUPP(iSrcUniStr: UniCharPtr; iSrcUniStrLen: ByteCount; var oSrcConvLen: ByteCount; oDestStr: TextPtr; iDestStrLen: ByteCount; var oDestConvLen: ByteCount; iInfoPtr: LogicalAddress; iUnicodeMappingPtr: ConstUnicodeMappingPtr; userRoutine: UnicodeToTextFallbackUPP): OSStatus; external name '_InvokeUnicodeToTextFallbackUPP'; { old name was CallUnicodeToTextFallbackProc }
{ Function prototypes }
{$ifc TARGET_CPU_68K AND NOT TARGET_RT_MAC_CFM}
{
    Routine to Initialize the Unicode Converter and cleanup once done with it. 
    These routines must be called from Static Library clients.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  InitializeUnicodeConverter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InitializeUnicodeConverter(TECFileName: StringPtr): OSStatus; external name '_InitializeUnicodeConverter';

{
 *  TerminateUnicodeConverter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure TerminateUnicodeConverter; external name '_TerminateUnicodeConverter';

{  Note: the old names (InitializeUnicode, TerminateUnicode) for the above are still exported. }
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}

{
 *  CreateTextToUnicodeInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CreateTextToUnicodeInfo(iUnicodeMapping: ConstUnicodeMappingPtr; var oTextToUnicodeInfo: TextToUnicodeInfo): OSStatus; external name '_CreateTextToUnicodeInfo';

{
 *  CreateTextToUnicodeInfoByEncoding()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CreateTextToUnicodeInfoByEncoding(iEncoding: TextEncoding; var oTextToUnicodeInfo: TextToUnicodeInfo): OSStatus; external name '_CreateTextToUnicodeInfoByEncoding';

{
 *  CreateUnicodeToTextInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CreateUnicodeToTextInfo(iUnicodeMapping: ConstUnicodeMappingPtr; var oUnicodeToTextInfo: UnicodeToTextInfo): OSStatus; external name '_CreateUnicodeToTextInfo';

{
 *  CreateUnicodeToTextInfoByEncoding()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CreateUnicodeToTextInfoByEncoding(iEncoding: TextEncoding; var oUnicodeToTextInfo: UnicodeToTextInfo): OSStatus; external name '_CreateUnicodeToTextInfoByEncoding';

{
 *  CreateUnicodeToTextRunInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CreateUnicodeToTextRunInfo(iNumberOfMappings: ItemCount; const (*var*) iUnicodeMappings: UnicodeMapping; var oUnicodeToTextInfo: UnicodeToTextRunInfo): OSStatus; external name '_CreateUnicodeToTextRunInfo';

{
 *  CreateUnicodeToTextRunInfoByEncoding()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CreateUnicodeToTextRunInfoByEncoding(iNumberOfEncodings: ItemCount; iEncodings: TextEncodingPtr; var oUnicodeToTextInfo: UnicodeToTextRunInfo): OSStatus; external name '_CreateUnicodeToTextRunInfoByEncoding';

{
 *  CreateUnicodeToTextRunInfoByScriptCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CreateUnicodeToTextRunInfoByScriptCode(iNumberOfScriptCodes: ItemCount; iScripts: ScriptCodePtr; var oUnicodeToTextInfo: UnicodeToTextRunInfo): OSStatus; external name '_CreateUnicodeToTextRunInfoByScriptCode';

{ Change the TextToUnicodeInfo to another mapping. }
{
 *  ChangeTextToUnicodeInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ChangeTextToUnicodeInfo(ioTextToUnicodeInfo: TextToUnicodeInfo; iUnicodeMapping: ConstUnicodeMappingPtr): OSStatus; external name '_ChangeTextToUnicodeInfo';

{ Change the UnicodeToTextInfo to another mapping. }
{
 *  ChangeUnicodeToTextInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ChangeUnicodeToTextInfo(ioUnicodeToTextInfo: UnicodeToTextInfo; iUnicodeMapping: ConstUnicodeMappingPtr): OSStatus; external name '_ChangeUnicodeToTextInfo';


{
 *  DisposeTextToUnicodeInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DisposeTextToUnicodeInfo(var ioTextToUnicodeInfo: TextToUnicodeInfo): OSStatus; external name '_DisposeTextToUnicodeInfo';

{
 *  DisposeUnicodeToTextInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DisposeUnicodeToTextInfo(var ioUnicodeToTextInfo: UnicodeToTextInfo): OSStatus; external name '_DisposeUnicodeToTextInfo';

{
 *  DisposeUnicodeToTextRunInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DisposeUnicodeToTextRunInfo(var ioUnicodeToTextRunInfo: UnicodeToTextRunInfo): OSStatus; external name '_DisposeUnicodeToTextRunInfo';

{
 *  ConvertFromTextToUnicode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertFromTextToUnicode(iTextToUnicodeInfo: TextToUnicodeInfo; iSourceLen: ByteCount; iSourceStr: ConstLogicalAddress; iControlFlags: OptionBits; iOffsetCount: ItemCount; iOffsetArray: ByteOffsetPtr; oOffsetCount: ItemCountPtr; oOffsetArray: ByteOffsetPtr; iOutputBufLen: ByteCount; var oSourceRead: ByteCount; var oUnicodeLen: ByteCount; oUnicodeStr: UniCharPtr): OSStatus; external name '_ConvertFromTextToUnicode';

{
 *  ConvertFromUnicodeToText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertFromUnicodeToText(iUnicodeToTextInfo: UnicodeToTextInfo; iUnicodeLen: ByteCount; iUnicodeStr: UniCharPtr; iControlFlags: OptionBits; iOffsetCount: ItemCount; iOffsetArray: ByteOffsetPtr; oOffsetCount: ItemCountPtr; oOffsetArray: ByteOffsetPtr; iOutputBufLen: ByteCount; var oInputRead: ByteCount; var oOutputLen: ByteCount; oOutputStr: LogicalAddress): OSStatus; external name '_ConvertFromUnicodeToText';

{
 *  ConvertFromUnicodeToTextRun()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertFromUnicodeToTextRun(iUnicodeToTextInfo: UnicodeToTextRunInfo; iUnicodeLen: ByteCount; iUnicodeStr: UniCharPtr; iControlFlags: OptionBits; iOffsetCount: ItemCount; iOffsetArray: ByteOffsetPtr; oOffsetCount: ItemCountPtr; oOffsetArray: ByteOffsetPtr; iOutputBufLen: ByteCount; var oInputRead: ByteCount; var oOutputLen: ByteCount; oOutputStr: LogicalAddress; iEncodingRunBufLen: ItemCount; var oEncodingRunOutLen: ItemCount; var oEncodingRuns: TextEncodingRun): OSStatus; external name '_ConvertFromUnicodeToTextRun';

{
 *  ConvertFromUnicodeToScriptCodeRun()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertFromUnicodeToScriptCodeRun(iUnicodeToTextInfo: UnicodeToTextRunInfo; iUnicodeLen: ByteCount; iUnicodeStr: UniCharPtr; iControlFlags: OptionBits; iOffsetCount: ItemCount; iOffsetArray: ByteOffsetPtr; oOffsetCount: ItemCountPtr; oOffsetArray: ByteOffsetPtr; iOutputBufLen: ByteCount; var oInputRead: ByteCount; var oOutputLen: ByteCount; oOutputStr: LogicalAddress; iScriptRunBufLen: ItemCount; var oScriptRunOutLen: ItemCount; var oScriptCodeRuns: ScriptCodeRun): OSStatus; external name '_ConvertFromUnicodeToScriptCodeRun';

{ Truncate a multibyte string at a safe place. }
{
 *  TruncateForTextToUnicode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TruncateForTextToUnicode(iTextToUnicodeInfo: ConstTextToUnicodeInfo; iSourceLen: ByteCount; iSourceStr: ConstLogicalAddress; iMaxLen: ByteCount; var oTruncatedLen: ByteCount): OSStatus; external name '_TruncateForTextToUnicode';

{ Truncate a Unicode string at a safe place. }
{
 *  TruncateForUnicodeToText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TruncateForUnicodeToText(iUnicodeToTextInfo: ConstUnicodeToTextInfo; iSourceLen: ByteCount; iSourceStr: ConstUniCharPtr; iControlFlags: OptionBits; iMaxLen: ByteCount; var oTruncatedLen: ByteCount): OSStatus; external name '_TruncateForUnicodeToText';

{ Convert a Pascal string to Unicode string. }
{
 *  ConvertFromPStringToUnicode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertFromPStringToUnicode(iTextToUnicodeInfo: TextToUnicodeInfo; const (*var*) iPascalStr: Str255; iOutputBufLen: ByteCount; var oUnicodeLen: ByteCount; oUnicodeStr: UniCharPtr): OSStatus; external name '_ConvertFromPStringToUnicode';

{ Convert a Unicode string to Pascal string. }
{
 *  ConvertFromUnicodeToPString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ConvertFromUnicodeToPString(iUnicodeToTextInfo: UnicodeToTextInfo; iUnicodeLen: ByteCount; iUnicodeStr: UniCharPtr; var oPascalStr: Str255): OSStatus; external name '_ConvertFromUnicodeToPString';

{ Count the available conversion mappings. }
{
 *  CountUnicodeMappings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountUnicodeMappings(iFilter: OptionBits; iFindMapping: ConstUnicodeMappingPtr; var oActualCount: ItemCount): OSStatus; external name '_CountUnicodeMappings';

{ Get a list of the available conversion mappings. }
{
 *  QueryUnicodeMappings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QueryUnicodeMappings(iFilter: OptionBits; iFindMapping: ConstUnicodeMappingPtr; iMaxCount: ItemCount; var oActualCount: ItemCount; var oReturnedMappings: UnicodeMapping): OSStatus; external name '_QueryUnicodeMappings';

{ Setup the fallback handler for converting Unicode To Text. }
{
 *  SetFallbackUnicodeToText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetFallbackUnicodeToText(iUnicodeToTextInfo: UnicodeToTextInfo; iFallback: UnicodeToTextFallbackUPP; iControlFlags: OptionBits; iInfoPtr: LogicalAddress): OSStatus; external name '_SetFallbackUnicodeToText';

{ Setup the fallback handler for converting Unicode To TextRuns. }
{
 *  SetFallbackUnicodeToTextRun()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetFallbackUnicodeToTextRun(iUnicodeToTextRunInfo: UnicodeToTextRunInfo; iFallback: UnicodeToTextFallbackUPP; iControlFlags: OptionBits; iInfoPtr: LogicalAddress): OSStatus; external name '_SetFallbackUnicodeToTextRun';

{ Re-initialize all state information kept by the context objects. }
{
 *  ResetTextToUnicodeInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.3 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResetTextToUnicodeInfo(ioTextToUnicodeInfo: TextToUnicodeInfo): OSStatus; external name '_ResetTextToUnicodeInfo';

{ Re-initialize all state information kept by the context objects. }
{
 *  ResetUnicodeToTextInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResetUnicodeToTextInfo(ioUnicodeToTextInfo: UnicodeToTextInfo): OSStatus; external name '_ResetUnicodeToTextInfo';

{ Re-initialize all state information kept by the context objects in TextRun conversions. }
{
 *  ResetUnicodeToTextRunInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeConverter 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResetUnicodeToTextRunInfo(ioUnicodeToTextRunInfo: UnicodeToTextRunInfo): OSStatus; external name '_ResetUnicodeToTextRunInfo';


{$ALIGN MAC68K}


end.

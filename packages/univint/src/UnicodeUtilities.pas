{
     File:       UnicodeUtilities.p
 
     Contains:   Types, constants, prototypes for Unicode Utilities (Unicode input and text utils)
 
     Version:    Technology: Mac OS 9.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1997-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit UnicodeUtilities;
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
uses MacTypes,MacLocales,TextCommon;


{$ALIGN MAC68K}

{
   -------------------------------------------------------------------------------------------------
   CONSTANTS & DATA STRUCTURES for UCKeyTranslate & UCKeyboardLayout ('uchr' resource)
   -------------------------------------------------------------------------------------------------
}

{
   -------------------------------------------------------------------------------------------------
   UCKeyOutput & related stuff
   The interpretation of UCKeyOutput depends on bits 15-14.
   If they are 01, then bits 0-13 are an index in UCKeyStateRecordsIndex (resource-wide list).
   If they are 10, then bits 0-13 are an index in UCKeySequenceDataIndex (resource-wide list),
     or if UCKeySequenceDataIndex is not present or the index is beyond the end of the list,
     then bits 0-15 are a single Unicode character.
   Otherwise, bits 0-15 are a single Unicode character; a value of 0xFFFE-0xFFFF means no character
     output.
   UCKeyCharSeq is similar, but does not support indices in UCKeyStateRecordsIndex. For bits 15-14:
   If they are 10, then bits 0-13 are an index in UCKeySequenceDataIndex (resource-wide list),
     or if UCKeySequenceDataIndex is not present or the index is beyond the end of the list,
     then bits 0-15 are a single Unicode character.
   Otherwise, bits 0-15 are a single Unicode character; a value of 0xFFFE-0xFFFF means no character
     output.
   -------------------------------------------------------------------------------------------------
}


type
	UCKeyOutput							= UInt16;
	UCKeyCharSeq						= UInt16;

const
	kUCKeyOutputStateIndexMask	= $4000;
	kUCKeyOutputSequenceIndexMask = $8000;
	kUCKeyOutputTestForIndexMask = $C000;						{  test bits 14-15 }
	kUCKeyOutputGetIndexMask	= $3FFF;						{  get bits 0-13 }

	{
	   -------------------------------------------------------------------------------------------------
	   UCKeyStateRecord & related stuff
	   The UCKeyStateRecord information is used as follows. If the current state is zero,
	   output stateZeroCharData and set the state to stateZeroNextState. If the current state
	   is non-zero and there is an entry for it in stateEntryData, then output the corresponding
	   charData and set the state to nextState. Otherwise, output the state terminator from
	   UCKeyStateTerminators for the current state (or nothing if there is no UCKeyStateTerminators
	   table or it has no entry for the current state), then output stateZeroCharData and set the
	   state to stateZeroNextState.
	   -------------------------------------------------------------------------------------------------
	}


type
	UCKeyStateRecordPtr = ^UCKeyStateRecord;
	UCKeyStateRecord = record
		stateZeroCharData:		UCKeyCharSeq;
		stateZeroNextState:		UInt16;
		stateEntryCount:		UInt16;
		stateEntryFormat:		UInt16;
																		{  This is followed by an array of stateEntryCount elements }
																		{  in the specified format. Here we just show a dummy array. }
		stateEntryData:			array [0..0] of UInt32;
	end;

	{
	   Here are the codes for entry formats currently defined.
	   Each entry maps from curState to charData and nextState.
	}

const
	kUCKeyStateEntryTerminalFormat = $0001;
	kUCKeyStateEntryRangeFormat	= $0002;

	{
	   For UCKeyStateEntryTerminal -
	   nextState is always 0, so we don't have a field for it
	}


type
	UCKeyStateEntryTerminalPtr = ^UCKeyStateEntryTerminal;
	UCKeyStateEntryTerminal = record
		curState:				UInt16;
		charData:				UCKeyCharSeq;
	end;

	{
	   For UCKeyStateEntryRange -
	   If curState >= curStateStart and curState <= curStateStart+curStateRange,
	   then it matches the entry, and we transform charData and nextState as follows:
	   If charData < 0xFFFE, then charData += (curState-curStateStart)*deltaMultiplier
	   If nextState != 0, then nextState += (curState-curStateStart)*deltaMultiplier
	}
	UCKeyStateEntryRangePtr = ^UCKeyStateEntryRange;
	UCKeyStateEntryRange = record
		curStateStart:			UInt16;
		curStateRange:			SInt8;
		deltaMultiplier:		SInt8;
		charData:				UCKeyCharSeq;
		nextState:				UInt16;
	end;

	{
	   -------------------------------------------------------------------------------------------------
	   UCKeyboardLayout & related stuff
	   The UCKeyboardLayout struct given here is only for the resource header. It specifies
	   offsets to the various subtables which each have their own structs, given below.
	   The keyboardTypeHeadList array selects table offsets that depend on keyboardType. The
	   first entry in keyboardTypeHeadList is the default entry, which will be used if the
	   keyboardType passed to UCKeyTranslate does not match any other entry - i.e. does not fall
	   within the range keyboardTypeFirst..keyboardTypeLast for some entry. The first entry
	   should have keyboardTypeFirst = keyboardTypeLast = 0.
	   -------------------------------------------------------------------------------------------------
	}
	UCKeyboardTypeHeaderPtr = ^UCKeyboardTypeHeader;
	UCKeyboardTypeHeader = record
		keyboardTypeFirst:		UInt32;									{  first keyboardType in this entry }
		keyboardTypeLast:		UInt32;									{  last keyboardType in this entry }
		keyModifiersToTableNumOffset: ByteOffset;						{  required }
		keyToCharTableIndexOffset: ByteOffset;							{  required }
		keyStateRecordsIndexOffset: ByteOffset;							{  0 => no table }
		keyStateTerminatorsOffset: ByteOffset;							{  0 => no table }
		keySequenceDataIndexOffset: ByteOffset;							{  0 => no table }
	end;

	UCKeyboardLayoutPtr = ^UCKeyboardLayout;
	UCKeyboardLayout = record
																		{  header only; other tables accessed via offsets }
		keyLayoutHeaderFormat:	UInt16;									{  =kUCKeyLayoutHeaderFormat }
		keyLayoutDataVersion:	UInt16;									{  0x0100 = 1.0, 0x0110 = 1.1, etc. }
		keyLayoutFeatureInfoOffset: ByteOffset;							{  may be 0                        }
		keyboardTypeCount:		ItemCount;								{  Dimension for keyboardTypeHeadList[]      }
		keyboardTypeList:		array [0..0] of UCKeyboardTypeHeader;
	end;

	{  ------------------------------------------------------------------------------------------------- }
	UCKeyLayoutFeatureInfoPtr = ^UCKeyLayoutFeatureInfo;
	UCKeyLayoutFeatureInfo = record
		keyLayoutFeatureInfoFormat: UInt16;								{  =kUCKeyLayoutFeatureInfoFormat }
		reserved:				UInt16;
		maxOutputStringLength:	UniCharCount;							{  longest possible output string }
	end;

	{  ------------------------------------------------------------------------------------------------- }
	UCKeyModifiersToTableNumPtr = ^UCKeyModifiersToTableNum;
	UCKeyModifiersToTableNum = record
		keyModifiersToTableNumFormat: UInt16;							{  =kUCKeyModifiersToTableNumFormat }
		defaultTableNum:		UInt16;									{  For modifier combos not in tableNum[] }
		modifiersCount:			ItemCount;								{  Dimension for tableNum[] }
		tableNum:				SInt8;
																		{  Then there is padding to a 4-byte boundary with bytes containing 0, if necessary. }
	end;

	{  ------------------------------------------------------------------------------------------------- }
	UCKeyToCharTableIndexPtr = ^UCKeyToCharTableIndex;
	UCKeyToCharTableIndex = record
		keyToCharTableIndexFormat: UInt16;								{  =kUCKeyToCharTableIndexFormat }
		keyToCharTableSize:		UInt16;									{  Max keyCode (128 for ADB keyboards) }
		keyToCharTableCount:	ItemCount;								{  Dimension for keyToCharTableOffsets[] (usually 6 to 12 tables) }
		keyToCharTableOffsets:	array [0..0] of ByteOffset;
																		{  Each offset in keyToCharTableOffsets is from the beginning of the resource to a }
																		{  table as follows: }
																		{     UCKeyOutput       keyToCharData[keyToCharTableSize]; }
																		{  These tables follow the UCKeyToCharTableIndex. }
																		{  Then there is padding to a 4-byte boundary with bytes containing 0, if necessary. }
	end;

	{  ------------------------------------------------------------------------------------------------- }
	UCKeyStateRecordsIndexPtr = ^UCKeyStateRecordsIndex;
	UCKeyStateRecordsIndex = record
		keyStateRecordsIndexFormat: UInt16;								{  =kUCKeyStateRecordsIndexFormat }
		keyStateRecordCount:	UInt16;									{  Dimension for keyStateRecordOffsets[] }
		keyStateRecordOffsets:	array [0..0] of ByteOffset;
																		{  Each offset in keyStateRecordOffsets is from the beginning of the resource to a }
																		{  UCKeyStateRecord. These UCKeyStateRecords follow the keyStateRecordOffsets[] array. }
																		{  Then there is padding to a 4-byte boundary with bytes containing 0, if necessary. }
	end;

	{  ------------------------------------------------------------------------------------------------- }
	UCKeyStateTerminatorsPtr = ^UCKeyStateTerminators;
	UCKeyStateTerminators = record
		keyStateTerminatorsFormat: UInt16;								{  =kUCKeyStateTerminatorsFormat }
		keyStateTerminatorCount: UInt16;								{  Dimension for keyStateTerminators[] (# of nonzero states) }
		keyStateTerminators:	array [0..0] of UCKeyCharSeq;
																		{  Note: keyStateTerminators[0] is terminator for state 1, etc. }
																		{  Then there is padding to a 4-byte boundary with bytes containing 0, if necessary. }
	end;

	{  ------------------------------------------------------------------------------------------------- }
	UCKeySequenceDataIndexPtr = ^UCKeySequenceDataIndex;
	UCKeySequenceDataIndex = record
		keySequenceDataIndexFormat: UInt16;								{  =kUCKeySequenceDataIndexFormat }
		charSequenceCount:		UInt16;									{  Dimension of charSequenceOffsets[] is charSequenceCount+1 }
		charSequenceOffsets:	array [0..0] of UInt16;
																		{  Each offset in charSequenceOffsets is in bytes, from the beginning of }
																		{  UCKeySequenceDataIndex to a sequence of UniChars; the next offset indicates the }
																		{  end of the sequence. The UniChar sequences follow the UCKeySequenceDataIndex. }
																		{  Then there is padding to a 4-byte boundary with bytes containing 0, if necessary. }
	end;

	{  ------------------------------------------------------------------------------------------------- }
	{  Current format codes for the various tables (bits 12-15 indicate which table) }


const
	kUCKeyLayoutHeaderFormat	= $1002;
	kUCKeyLayoutFeatureInfoFormat = $2001;
	kUCKeyModifiersToTableNumFormat = $3001;
	kUCKeyToCharTableIndexFormat = $4001;
	kUCKeyStateRecordsIndexFormat = $5001;
	kUCKeyStateTerminatorsFormat = $6001;
	kUCKeySequenceDataIndexFormat = $7001;


	{
	   -------------------------------------------------------------------------------------------------
	   Constants for keyAction parameter in UCKeyTranslate() 
	   -------------------------------------------------------------------------------------------------
	}

	kUCKeyActionDown			= 0;							{  key is going down }
	kUCKeyActionUp				= 1;							{  key is going up }
	kUCKeyActionAutoKey			= 2;							{  auto-key down }
	kUCKeyActionDisplay			= 3;							{  get information for key display (as in Key Caps)       }

	{
	   -------------------------------------------------------------------------------------------------
	   Bit assignments & masks for keyTranslateOptions parameter in UCKeyTranslate() 
	   -------------------------------------------------------------------------------------------------
	}

	kUCKeyTranslateNoDeadKeysBit = 0;							{  Prevents setting any new dead-key states }

	kUCKeyTranslateNoDeadKeysMask = $00000001;

	{
	   -------------------------------------------------------------------------------------------------
	   CONSTANTS & DATA STRUCTURES for Unicode Collation
	   -------------------------------------------------------------------------------------------------
	}
	{  constant for LocaleOperationClass }
	kUnicodeCollationClass		= $75636F6C (* 'ucol' *);


type
	CollatorRef    = ^SInt32; { an opaque 32-bit type }
	CollatorRefPtr = ^CollatorRef;  { when a var xx:CollatorRef parameter can be nil, it is changed to xx: CollatorRefPtr }
	UCCollateOptions 			= UInt32;
const
																{  Sensitivity options }
	kUCCollateComposeInsensitiveMask = $00000002;
	kUCCollateWidthInsensitiveMask = $00000004;
	kUCCollateCaseInsensitiveMask = $00000008;
	kUCCollateDiacritInsensitiveMask = $00000010;				{  Other general options  }
	kUCCollatePunctuationSignificantMask = $00008000;			{  Number-handling options  }
	kUCCollateDigitsOverrideMask = $00010000;
	kUCCollateDigitsAsNumberMask = $00020000;

	kUCCollateStandardOptions	= $00000006;

	{
	   Special values to specify various invariant orders for UCCompareTextNoLocale.
	   These values use the high 8 bits of UCCollateOptions.
	}
	kUCCollateTypeHFSExtended	= 1;

	{  These constants are used for masking and shifting the invariant order type. }
	kUCCollateTypeSourceMask	= $000000FF;
	kUCCollateTypeShiftBits		= 24;

	kUCCollateTypeMask			= $FF000000;


type
	UCCollationValue					= UInt32;
	UCCollationValuePtr				= ^UCCollationValue;
	{
	   -------------------------------------------------------------------------------------------------
	   CONSTANTS & DATA STRUCTURES for Unicode TextBreak
	   -------------------------------------------------------------------------------------------------
	}
	{  constant for LocaleOperationClass }

const
	kUnicodeTextBreakClass		= $7562726B (* 'ubrk' *);


type
	TextBreakLocatorRef    = ^SInt32; { an opaque 32-bit type }
	TextBreakLocatorRefPtr = ^TextBreakLocatorRef;  { when a var xx:TextBreakLocatorRef parameter can be nil, it is changed to xx: TextBreakLocatorRefPtr }
	UCTextBreakType 			= UInt32;
const
	kUCTextBreakCharMask		= $00000001;
	kUCTextBreakClusterMask		= $00000004;
	kUCTextBreakWordMask		= $00000010;
	kUCTextBreakLineMask		= $00000040;


type
	UCTextBreakOptions 			= UInt32;
const
	kUCTextBreakLeadingEdgeMask	= $00000001;
	kUCTextBreakGoBackwardsMask	= $00000002;
	kUCTextBreakIterateMask		= $00000004;

	{
	   -------------------------------------------------------------------------------------------------
	   function PROTOTYPES
	   -------------------------------------------------------------------------------------------------
	}

	{
	 *  UCKeyTranslate()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in UnicodeUtilitiesCoreLib 8.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function UCKeyTranslate(const (*var*) keyLayoutPtr: UCKeyboardLayout; virtualKeyCode: UInt16; keyAction: UInt16; modifierKeyState: UInt32; keyboardType: UInt32; keyTranslateOptions: OptionBits; var deadKeyState: UInt32; maxStringLength: UniCharCount; var actualStringLength: UniCharCount; unicodeString: UniCharPtr): OSStatus; external name '_UCKeyTranslate';

{  Standard collation functions }
{
 *  UCCreateCollator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCCreateCollator(locale: LocaleRef; opVariant: LocaleOperationVariant; options: UCCollateOptions; var collatorRef_: CollatorRef): OSStatus; external name '_UCCreateCollator';

{
 *  UCGetCollationKey()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCGetCollationKey(collatorRef_: CollatorRef; textPtr: ConstUniCharPtr; textLength: UniCharCount; maxKeySize: ItemCount; var actualKeySize: ItemCount; collationKey: UCCollationValuePtr): OSStatus; external name '_UCGetCollationKey';

{
 *  UCCompareCollationKeys()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesCoreLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCCompareCollationKeys(key1Ptr: UCCollationValuePtr; key1Length: ItemCount; key2Ptr: UCCollationValuePtr; key2Length: ItemCount; var equivalent: boolean; var order: SInt32): OSStatus; external name '_UCCompareCollationKeys';

{
 *  UCCompareText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCCompareText(collatorRef_: CollatorRef; text1Ptr: ConstUniCharPtr; text1Length: UniCharCount; text2Ptr: ConstUniCharPtr; text2Length: UniCharCount; var equivalent: boolean; var order: SInt32): OSStatus; external name '_UCCompareText';

{
 *  UCDisposeCollator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCDisposeCollator(var collatorRef_: CollatorRef): OSStatus; external name '_UCDisposeCollator';

{  Simple collation using default locale }

{
 *  UCCompareTextDefault()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCCompareTextDefault(options: UCCollateOptions; text1Ptr: ConstUniCharPtr; text1Length: UniCharCount; text2Ptr: ConstUniCharPtr; text2Length: UniCharCount; var equivalent: boolean; var order: SInt32): OSStatus; external name '_UCCompareTextDefault';


{  Simple locale-independent collation }

{
 *  UCCompareTextNoLocale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesCoreLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCCompareTextNoLocale(options: UCCollateOptions; text1Ptr: ConstUniCharPtr; text1Length: UniCharCount; text2Ptr: ConstUniCharPtr; text2Length: UniCharCount; var equivalent: boolean; var order: SInt32): OSStatus; external name '_UCCompareTextNoLocale';

{  Standard text break (text boundary) functions }
{
 *  UCCreateTextBreakLocator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCCreateTextBreakLocator(locale: LocaleRef; opVariant: LocaleOperationVariant; breakTypes: UCTextBreakType; var breakRef: TextBreakLocatorRef): OSStatus; external name '_UCCreateTextBreakLocator';

{
 *  UCFindTextBreak()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCFindTextBreak(breakRef: TextBreakLocatorRef; breakType: UCTextBreakType; options: UCTextBreakOptions; textPtr: ConstUniCharPtr; textLength: UniCharCount; startOffset: UniCharArrayOffset; var breakOffset: UniCharArrayOffset): OSStatus; external name '_UCFindTextBreak';

{
 *  UCDisposeTextBreakLocator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UCDisposeTextBreakLocator(var breakRef: TextBreakLocatorRef): OSStatus; external name '_UCDisposeTextBreakLocator';

{$ALIGN MAC68K}


end.

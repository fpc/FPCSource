{
     File:       CarbonCore/UnicodeUtilities.h
 
     Contains:   Types, constants, prototypes for Unicode Utilities (Unicode input and text utils)
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1997-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{    Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit UnicodeUtilities;
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
	{$setc TARGET_CPU_PPC := FALSE}
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
uses MacTypes,MacLocales,TextCommon,CFBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

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
	UCKeyOutput = UInt16;
	UCKeyCharSeq = UInt16;
const
	kUCKeyOutputStateIndexMask = $4000;
	kUCKeyOutputSequenceIndexMask = $8000;
	kUCKeyOutputTestForIndexMask = $C000; { test bits 14-15}
	kUCKeyOutputGetIndexMask = $3FFF; { get bits 0-13}

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
	UCKeyStateRecord = record
		stateZeroCharData: UCKeyCharSeq;
		stateZeroNextState: UInt16;
		stateEntryCount: UInt16;
		stateEntryFormat: UInt16;
                                              { This is followed by an array of stateEntryCount elements}
                                              { in the specified format. Here we just show a dummy array.}
		stateEntryData: array [0..0] of UInt32;
	end;
{
   Here are the codes for entry formats currently defined.
   Each entry maps from curState to charData and nextState.
}
const
	kUCKeyStateEntryTerminalFormat = $0001;
	kUCKeyStateEntryRangeFormat = $0002;

{
   For UCKeyStateEntryTerminal -
   nextState is always 0, so we don't have a field for it
}

type
	UCKeyStateEntryTerminalPtr = ^UCKeyStateEntryTerminal;
	UCKeyStateEntryTerminal = record
		curState: UInt16;
		charData: UCKeyCharSeq;
	end;
{
   For UCKeyStateEntryRange -
   If curState >= curStateStart and curState <= curStateStart+curStateRange,
   then it matches the entry, and we transform charData and nextState as follows:
   If charData < 0xFFFE, then charData += (curState-curStateStart)*deltaMultiplier
   If nextState != 0, then nextState += (curState-curStateStart)*deltaMultiplier
}
type
	UCKeyStateEntryRangePtr = ^UCKeyStateEntryRange;
	UCKeyStateEntryRange = record
		curStateStart: UInt16;
		curStateRange: UInt8;
		deltaMultiplier: UInt8;
		charData: UCKeyCharSeq;
		nextState: UInt16;
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
type
	UCKeyboardTypeHeaderPtr = ^UCKeyboardTypeHeader;
	UCKeyboardTypeHeader = record
		keyboardTypeFirst: UInt32;      { first keyboardType in this entry}
		keyboardTypeLast: UInt32;       { last keyboardType in this entry}
		keyModifiersToTableNumOffset: UInt32; { required}
		keyToCharTableIndexOffset: UInt32; { required}
		keyStateRecordsIndexOffset: UInt32; { 0 => no table}
		keyStateTerminatorsOffset: UInt32; { 0 => no table}
		keySequenceDataIndexOffset: UInt32; { 0 => no table}
	end;
type
	UCKeyboardLayoutPtr = ^UCKeyboardLayout;
	UCKeyboardLayout = record
{ header only; other tables accessed via offsets}
		keyLayoutHeaderFormat: UInt16;  { =kUCKeyLayoutHeaderFormat}
		keyLayoutDataVersion: UInt16;   { 0x0100 = 1.0, 0x0110 = 1.1, etc.}
		keyLayoutFeatureInfoOffset: UInt32; { may be 0                       }
		keyboardTypeCount: UInt32;      { Dimension for keyboardTypeHeadList[]     }
		keyboardTypeList:		array [0..0] of UCKeyboardTypeHeader;
	end;
{ -------------------------------------------------------------------------------------------------}
type
	UCKeyLayoutFeatureInfoPtr = ^UCKeyLayoutFeatureInfo;
	UCKeyLayoutFeatureInfo = record
		keyLayoutFeatureInfoFormat: UInt16; { =kUCKeyLayoutFeatureInfoFormat}
		reserved: UInt16;
		maxOutputStringLength: UInt32;  { longest possible output string}
	end;
{ -------------------------------------------------------------------------------------------------}
type
	UCKeyModifiersToTableNumPtr = ^UCKeyModifiersToTableNum;
	UCKeyModifiersToTableNum = record
		keyModifiersToTableNumFormat: UInt16; { =kUCKeyModifiersToTableNumFormat}
		defaultTableNum: UInt16;        { For modifier combos not in tableNum[]}
		modifiersCount: UInt32;         { Dimension for tableNum[]}
		tableNum: array [0..0] of UInt8;

                                              { Then there is padding to a 4-byte boundary with bytes containing 0, if necessary.}
	end;
{ -------------------------------------------------------------------------------------------------}
type
	UCKeyToCharTableIndexPtr = ^UCKeyToCharTableIndex;
	UCKeyToCharTableIndex = record
		keyToCharTableIndexFormat: UInt16; { =kUCKeyToCharTableIndexFormat}
		keyToCharTableSize: UInt16;     { Max keyCode (128 for ADB keyboards)}
		keyToCharTableCount: UInt32;    { Dimension for keyToCharTableOffsets[] (usually 6 to 12 tables)}
		keyToCharTableOffsets:	array [0..0] of ByteOffset;

                                              { Each offset in keyToCharTableOffsets is from the beginning of the resource to a}
                                              { table as follows:}
                                              {    UCKeyOutput       keyToCharData[keyToCharTableSize];}
                                              { These tables follow the UCKeyToCharTableIndex.}
                                              { Then there is padding to a 4-byte boundary with bytes containing 0, if necessary.}
	end;
{ -------------------------------------------------------------------------------------------------}
type
	UCKeyStateRecordsIndexPtr = ^UCKeyStateRecordsIndex;
	UCKeyStateRecordsIndex = record
		keyStateRecordsIndexFormat: UInt16; { =kUCKeyStateRecordsIndexFormat}
		keyStateRecordCount: UInt16;    { Dimension for keyStateRecordOffsets[]}
		keyStateRecordOffsets:	array [0..0] of ByteOffset;

                                              { Each offset in keyStateRecordOffsets is from the beginning of the resource to a}
                                              { UCKeyStateRecord. These UCKeyStateRecords follow the keyStateRecordOffsets[] array.}
                                              { Then there is padding to a 4-byte boundary with bytes containing 0, if necessary.}
	end;
{ -------------------------------------------------------------------------------------------------}
type
	UCKeyStateTerminatorsPtr = ^UCKeyStateTerminators;
	UCKeyStateTerminators = record
		keyStateTerminatorsFormat: UInt16; { =kUCKeyStateTerminatorsFormat}
		keyStateTerminatorCount: UInt16; { Dimension for keyStateTerminators[] (# of nonzero states)}
		keyStateTerminators:	array [0..0] of UCKeyCharSeq;

                                              { Note: keyStateTerminators[0] is terminator for state 1, etc.}
                                              { Then there is padding to a 4-byte boundary with bytes containing 0, if necessary.}
	end;
{ -------------------------------------------------------------------------------------------------}
type
	UCKeySequenceDataIndexPtr = ^UCKeySequenceDataIndex;
	UCKeySequenceDataIndex = record
		keySequenceDataIndexFormat: UInt16; { =kUCKeySequenceDataIndexFormat}
		charSequenceCount: UInt16;      { Dimension of charSequenceOffsets[] is charSequenceCount+1}
		charSequenceOffsets:	array [0..0] of UInt16;

                                              { Each offset in charSequenceOffsets is in bytes, from the beginning of}
                                              { UCKeySequenceDataIndex to a sequence of UniChars; the next offset indicates the}
                                              { end of the sequence. The UniChar sequences follow the UCKeySequenceDataIndex.}
                                              { Then there is padding to a 4-byte boundary with bytes containing 0, if necessary.}
	end;
{ -------------------------------------------------------------------------------------------------}
{ Current format codes for the various tables (bits 12-15 indicate which table)}

const
	kUCKeyLayoutHeaderFormat = $1002;
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

const
	kUCKeyActionDown = 0;    { key is going down}
	kUCKeyActionUp = 1;    { key is going up}
	kUCKeyActionAutoKey = 2;    { auto-key down}
	kUCKeyActionDisplay = 3;     { get information for key display (as in Key Caps)      }

{
   -------------------------------------------------------------------------------------------------
   Bit assignments & masks for keyTranslateOptions parameter in UCKeyTranslate() 
   -------------------------------------------------------------------------------------------------
}

const
	kUCKeyTranslateNoDeadKeysBit = 0;     { Prevents setting any new dead-key states}

const
	kUCKeyTranslateNoDeadKeysMask = 1 shl kUCKeyTranslateNoDeadKeysBit;

{
   -------------------------------------------------------------------------------------------------
   CONSTANTS & DATA STRUCTURES for Unicode Collation
   -------------------------------------------------------------------------------------------------
}

{ constant for LocaleOperationClass}
const
	kUnicodeCollationClass = FourCharCode('ucol');

type
	CollatorRef = ^SInt32; { an opaque type }
	CollatorRefPtr = ^CollatorRef;  { when a var xx:CollatorRef parameter can be nil, it is changed to xx: CollatorRefPtr }
	UCCollateOptions = UInt32;
const
{ Sensitivity options}
	kUCCollateComposeInsensitiveMask = 1 shl 1;
	kUCCollateWidthInsensitiveMask = 1 shl 2;
	kUCCollateCaseInsensitiveMask = 1 shl 3;
	kUCCollateDiacritInsensitiveMask = 1 shl 4; { Other general options }
	kUCCollatePunctuationSignificantMask = 1 shl 15; { Number-handling options }
	kUCCollateDigitsOverrideMask = 1 shl 16;
	kUCCollateDigitsAsNumberMask = 1 shl 17;

const
	kUCCollateStandardOptions = kUCCollateComposeInsensitiveMask or kUCCollateWidthInsensitiveMask;

{
   Special values to specify various invariant orders for UCCompareTextNoLocale.
   These values use the high 8 bits of UCCollateOptions.
}
const
	kUCCollateTypeHFSExtended = 1;

{ These constants are used for masking and shifting the invariant order type.}
const
	kUCCollateTypeSourceMask = $000000FF;
	kUCCollateTypeShiftBits = 24;

const
	kUCCollateTypeMask = kUCCollateTypeSourceMask shl kUCCollateTypeShiftBits;


type
	UCCollationValue = UInt32;
	UCCollationValuePtr = ^UCCollationValue;
{
   -------------------------------------------------------------------------------------------------
   CONSTANTS & DATA STRUCTURES for Unicode TypeSelect
   -------------------------------------------------------------------------------------------------
}

{
   UCTypeSelectRef
   This is the single opaque object needed to implement the Unicode TypeSelect
   utilities. It is created and initialized via a call to UCTypeSelectCreateSelector
}
type
	UCTypeSelectRef = ^SInt32; { an opaque type }
{
   UCTypeSelectCompareResult
   Used as the return value for UCTypeSelectCompare()
}
type
	UCTypeSelectCompareResult = SInt32;
{
   UCTSWalkDirection
   Used for UCTypeSelectWalkList to determine the direction of the walk
}
type
	UCTSWalkDirection = UInt16;
	UCTSWalkDirectionPtr = ^UCTSWalkDirection;
const
	kUCTSDirectionNext = 0;
	kUCTSDirectionPrevious = 1;

{
   UCTypeSelectOptions
   These constants may be returned from an IndexToUCString callback function
   in the location pointed to by the tsOptions parameter. *tsOptions is pre-
   initialized to zero before the callback function is called. A callback
   function does not need to set *tsOptions unless it wants behavior different
   from the default.
   kUCTSOptionsReleaseStringMask: indicates that UCTypeSelectFindItem should
   release the CFStringRef returned by the IndexToUCString callback function
   once it is done with the string. If this bit is not set, the string will
   not be released.
   kUCTSOptionsDataIsOrderedMask: indicates that the data being returned by the
   IndexToUCString callback is already in the correct alphabetical order. If so,
   UCTypeSelectFindItem can optimize its search through the data to find the closest
   matching item.
}
type
	UCTypeSelectOptions = UInt16;
	UCTypeSelectOptionsPtr = ^UCTypeSelectOptions;
const
	kUCTSOptionsNoneMask = 0;
	kUCTSOptionsReleaseStringMask = 1;
	kUCTSOptionsDataIsOrderedMask = 2;


{
   IndexToUCStringProcPtr
   This is the type used to define the user's IndexToUCString callback
}
type
	IndexToUCStringProcPtr = function( index: UInt32; listDataPtr: UnivPtr; refcon: UnivPtr; var outString: CFStringRef; var tsOptions: UCTypeSelectOptions ): Boolean;

type
	IndexToUCStringUPP = IndexToUCStringProcPtr;
{
 *  NewIndexToUCStringUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewIndexToUCStringUPP( userRoutine: IndexToUCStringProcPtr ): IndexToUCStringUPP; external name '_NewIndexToUCStringUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 *  DisposeIndexToUCStringUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeIndexToUCStringUPP( userUPP: IndexToUCStringUPP ); external name '_DisposeIndexToUCStringUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 *  InvokeIndexToUCStringUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeIndexToUCStringUPP( index: UInt32; listDataPtr: UnivPtr; refcon: UnivPtr; var outString: CFStringRef; var tsOptions: UCTypeSelectOptions; userUPP: IndexToUCStringUPP ): Boolean; external name '_InvokeIndexToUCStringUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
   kUCTypeSelectMaxListSize can be used for any listSize arguement
   when the length of the list is unknown.
}
const
	kUCTypeSelectMaxListSize = $FFFFFFFF;


{
   -------------------------------------------------------------------------------------------------
   CONSTANTS & DATA STRUCTURES for Unicode TextBreak
   -------------------------------------------------------------------------------------------------
}

{ constant for LocaleOperationClass}
const
	kUnicodeTextBreakClass = FourCharCode('ubrk');

type
	TextBreakLocatorRef = ^SInt32; { an opaque type }
	TextBreakLocatorRefPtr = ^TextBreakLocatorRef;  { when a var xx:TextBreakLocatorRef parameter can be nil, it is changed to xx: TextBreakLocatorRefPtr }

{
 *  UCTextBreakType
 *  
 *  Discussion:
 *    Specifies kinds of text boundaries.
 }
type
	UCTextBreakType = UInt32;
const
{
   * If the bit specified by this mask is set, boundaries of characters
   * may be located (with surrogate pairs treated as a single
   * character).
   }
	kUCTextBreakCharMask = 1 shl 0;

  {
   * If the bit specified by this mask is set, boundaries of character
   * clusters may be located. A cluster is a group of characters that
   * should be treated as single text element for editing operations
   * such as cursor movement. Typically this includes groups such as a
   * base character followed by a sequence of combining characters, for
   * example, a Hangul syllable represented as a sequence of conjoining
   * jamo characters or an Indic consonant cluster.
   }
	kUCTextBreakClusterMask = 1 shl 2;

  {
   * If the bit specified by this mask is set, boundaries of words may
   * be located. This can be used to determine what to highlight as the
   * result of a double-click.
   }
	kUCTextBreakWordMask = 1 shl 4;
	kUCTextBreakLineMask = 1 shl 6;

  {
   * If the bit specified by this mask is set, boundaries of paragraphs
   * may be located. This just finds the next hard-line break as
   * defined by the Unicode standard.
   }
	kUCTextBreakParagraphMask = 1 shl 8;

type
	UCTextBreakOptions = UInt32;
const
	kUCTextBreakLeadingEdgeMask = 1 shl 0;
	kUCTextBreakGoBackwardsMask = 1 shl 1;
	kUCTextBreakIterateMask = 1 shl 2;


{
   -------------------------------------------------------------------------------------------------
   FUNCTION PROTOTYPES
   -------------------------------------------------------------------------------------------------
}

{
 *  UCKeyTranslate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesCoreLib 8.5 and later
 }
function UCKeyTranslate( const (*var*) keyLayoutPtr: UCKeyboardLayout; virtualKeyCode: UInt16; keyAction: UInt16; modifierKeyState: UInt32; keyboardType: UInt32; keyTranslateOptions: OptionBits; var deadKeyState: UInt32; maxStringLength: UniCharCount; var actualStringLength: UniCharCount; unicodeString: {variable-size-array} UniCharPtr ): OSStatus; external name '_UCKeyTranslate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Standard collation functions}

{
 *  UCCreateCollator()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 }
function UCCreateCollator( locale: LocaleRef; opVariant: LocaleOperationVariant; options: UCCollateOptions; var collatorRef_: CollatorRef ): OSStatus; external name '_UCCreateCollator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UCGetCollationKey()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 }
function UCGetCollationKey( collatorRef_: CollatorRef; textPtr: ConstUniCharPtr; textLength: UniCharCount; maxKeySize: ItemCount; var actualKeySize: ItemCount; collationKey: {variable-size-array} UCCollationValuePtr ): OSStatus; external name '_UCGetCollationKey';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UCCompareCollationKeys()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesCoreLib 8.6 and later
 }
function UCCompareCollationKeys( key1Ptr: UCCollationValuePtr; key1Length: ItemCount; key2Ptr: UCCollationValuePtr; key2Length: ItemCount; var equivalent: Boolean; var order: SInt32 ): OSStatus; external name '_UCCompareCollationKeys';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UCCompareText()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 }
function UCCompareText( collatorRef_: CollatorRef; text1Ptr: ConstUniCharPtr; text1Length: UniCharCount; text2Ptr: ConstUniCharPtr; text2Length: UniCharCount; var equivalent: Boolean; var order: SInt32 ): OSStatus; external name '_UCCompareText';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UCDisposeCollator()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 }
function UCDisposeCollator( var collatorRef_: CollatorRef ): OSStatus; external name '_UCDisposeCollator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Simple collation using default locale}

{
 *  UCCompareTextDefault()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 8.6 and later
 }
function UCCompareTextDefault( options: UCCollateOptions; text1Ptr: ConstUniCharPtr; text1Length: UniCharCount; text2Ptr: ConstUniCharPtr; text2Length: UniCharCount; var equivalent: Boolean; var order: SInt32 ): OSStatus; external name '_UCCompareTextDefault';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Simple locale-independent collation}

{
 *  UCCompareTextNoLocale()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesCoreLib 8.6 and later
 }
function UCCompareTextNoLocale( options: UCCollateOptions; text1Ptr: ConstUniCharPtr; text1Length: UniCharCount; text2Ptr: ConstUniCharPtr; text2Length: UniCharCount; var equivalent: Boolean; var order: SInt32 ): OSStatus; external name '_UCCompareTextNoLocale';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
*===============================================================================
*   Text break (text boundary) functions
*
*   These are deprecated. Replacements are as follows:
*
*   1. To determine locale-sensitive text breaks for word, line, sentence and
*   paragraph boundaries, use the CFStringTokenizer functions:
*   CFStringTokenizerCreate (balanced by CFRelease), CFStringTokenizerAdvanceToNextToken
*   or CFStringTokenizerGoToTokenAtIndex, then CFStringTokenizerGetCurrentTokenRange...
*
*   2. To determine cluster breaks, use CFStringGetRangeOfComposedCharactersAtIndex.
*
*   3. For handling character boundaries / surrogate pairs in UTF16 text, the
*   following inline functions are available in CFString.h:
*   CFStringIsSurrogateHighCharacter, CFStringIsSurrogateLowCharacter,
*   CFStringGetLongCharacterForSurrogatePair, and CFStringGetSurrogatePairForLongCharacter.
*   However, CFString clients do not usually need to worry about handling surrogate pairs
*   directly.
*
*===============================================================================
}
{
 *  UCCreateTextBreakLocator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use CFStringTokenizer functions or
 *    CFStringGetRangeOfComposedCharactersAtIndex, see discussion above
 *    for details.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.6
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 9.0 and later
 }
function UCCreateTextBreakLocator( locale: LocaleRef; opVariant: LocaleOperationVariant; breakTypes: UCTextBreakType; var breakRef: TextBreakLocatorRef ): OSStatus; external name '_UCCreateTextBreakLocator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_6 *)


{
 *  UCFindTextBreak()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use CFStringTokenizer functions or
 *    CFStringGetRangeOfComposedCharactersAtIndex, see discussion above
 *    for details.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.6
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 9.0 and later
 }
function UCFindTextBreak( breakRef: TextBreakLocatorRef; breakType: UCTextBreakType; options: UCTextBreakOptions; textPtr: ConstUniCharPtr; textLength: UniCharCount; startOffset: UniCharArrayOffset; var breakOffset: UniCharArrayOffset ): OSStatus; external name '_UCFindTextBreak';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_6 *)


{
 *  UCDisposeTextBreakLocator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use CFStringTokenizer functions or
 *    CFStringGetRangeOfComposedCharactersAtIndex, see discussion above
 *    for details.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.6
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in UnicodeUtilitiesLib 9.0 and later
 }
function UCDisposeTextBreakLocator( var breakRef: TextBreakLocatorRef ): OSStatus; external name '_UCDisposeTextBreakLocator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_6 *)


{
   -------------------------------------------------------------------------------------------------
   UNICODE TYPESELECT - FUNCTION APIs
   -------------------------------------------------------------------------------------------------
}

{
 *  UCTypeSelectCreateSelector()
 *  
 *  Summary:
 *    Responsible for creating the opaque UCTypeSelectRef object.
 *  
 *  Parameters:
 *    
 *    locale:
 *      LocaleRef obtained by client from a call such as
 *      LocaleRefFromLangOrRegionCode. This can be set to NULL if the
 *      default system locale is desired.
 *    
 *    opVariant:
 *      Variant of the locale. Specify 0 if no variant is needed.
 *    
 *    options:
 *      Any collation options the client wishes to specify. These will
 *      have an impact on the order in which selection will occur.
 *      Specify kUCCollateStandardOptions for the default options.
 *    
 *    newSelector:
 *      The newly created UCTypeSelectRef object.
 *  
 *  Result:
 *    Will return paramErr if newSelector is NULL, or any other error
 *    that may be returned by an internal function call.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectCreateSelector( locale: LocaleRef { can be NULL }; opVariant: LocaleOperationVariant; options: UCCollateOptions; var newSelector: UCTypeSelectRef ): OSStatus; external name '_UCTypeSelectCreateSelector';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UCTypeSelectFlushSelectorData()
 *  
 *  Summary:
 *    Flushes the key list and resets the timeout timer for the
 *    UCTypeSelectRef.
 *  
 *  Parameters:
 *    
 *    ref:
 *      UCTypeSelectRef to be flushed.
 *  
 *  Result:
 *    Returns paramErr if ref is invalid.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectFlushSelectorData( ref: UCTypeSelectRef ): OSStatus; external name '_UCTypeSelectFlushSelectorData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UCTypeSelectReleaseSelector()
 *  
 *  Summary:
 *    Cleans up and disposes of any temporary memory acquired by the
 *    UCTypeSelectRef object.
 *  
 *  Parameters:
 *    
 *    ref:
 *      A pointer to the UCTypeSelectRef to be disposed of. On exit,
 *      the UCTypeSelectRef to which this parameter points will be set
 *      to NULL.
 *  
 *  Result:
 *    Returns paramErr if ref is invalid.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectReleaseSelector( var ref: UCTypeSelectRef ): OSStatus; external name '_UCTypeSelectReleaseSelector';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UCTypeSelectWouldResetBuffer()
 *  
 *  Summary:
 *    Indicates whether, if the specified text were added to the
 *    buffer, the current text in the buffer would be discarded.
 *  
 *  Parameters:
 *    
 *    inRef:
 *      The type-selection object.
 *    
 *    inText:
 *      The text that would be added to the buffer. Some text (such as
 *      Backspace, Enter, and Clear keys) always causes the buffer to
 *      be reset. May be NULL; in that case, the implementation only
 *      considers the event time.
 *    
 *    inEventTime:
 *      The time in seconds since boot (as returned by
 *      GetCurrentEventTime) that the text event occurred. If the event
 *      occurred at a time greater than the type-select timeout, then
 *      the current buffered text would be discarded.
 *  
 *  Result:
 *    Whether the current text in the buffer would be discarded while
 *    processing the specified text.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectWouldResetBuffer( inRef: UCTypeSelectRef; inText: CFStringRef { can be NULL }; inEventTime: Float64 ): Boolean; external name '_UCTypeSelectWouldResetBuffer';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UCTypeSelectAddKeyToSelector()
 *  
 *  Summary:
 *    Appends the given Unicode values to the selector's internal
 *    buffer of keystrokes. It also handles timeouts and delete/clear
 *    keys. If the key sent is a delete/clear/esc code, the key buffer
 *    will be flushed and false will be returned.
 *  
 *  Parameters:
 *    
 *    inRef:
 *      The type-selection object.
 *    
 *    inText:
 *      A CFString that contains the keystroke to be added.
 *    
 *    inEventTime:
 *      The time in seconds since boot (as returned by
 *      GetCurrentEventTime) that the text event occurred.  If zero is
 *      passed, then the current time is used automatically.
 *    
 *    updateFlag:
 *      On exit, notifies the client if it needs to update its current
 *      selection, as follows: 
 *      
 *      TRUE - indicates that the client needs to update its selection
 *      based on the keystroke passed in. A call to UCTypeSelectCompare
 *      or UCTypeSelectFindItem should be made to find the new item to
 *      select based on the new keys added. 
 *      
 *      FALSE - indicates that the client does not need to update its
 *      selection. This would occur if a delete/clear/esc key was
 *      passed in.
 *  
 *  Result:
 *    returns paramErr if ref or textPtr are invalid. Can also return
 *    other errors from intermediate function calls.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectAddKeyToSelector( inRef: UCTypeSelectRef; inText: CFStringRef; inEventTime: Float64; var updateFlag: Boolean ): OSStatus; external name '_UCTypeSelectAddKeyToSelector';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UCTypeSelectCompare()
 *  
 *  Summary:
 *    Compares the Unicode text buffer passed in inText with the
 *    keystroke buffer stored in the UCTypeSelectRef. This function
 *    works the same as the non-Unicode TypeSelectCompare() call.
 *  
 *  Parameters:
 *    
 *    ref:
 *      UCTypeSelectRef to which the Unicode text sent in inText will
 *      be compared.
 *    
 *    inText:
 *      A reference to the text to be compared
 *    
 *    result:
 *      Just as in TypeSelectCompare(), the following values are 
 *      returned: -1 if characters in UCTypeSelectRef’s keystroke
 *      buffer sort before those in inText, 0 if characters in
 *      UCTypeSelectRef’s keystroke buffer are the same as those in
 *      inText, and 1 if the characters in UCTypeSelectRef’s keystroke
 *      buffer sort after those in inText.
 *  
 *  Result:
 *    This function can return three different types of values. First,
 *    it will return paramErr if ref, inText, or result are invalid.
 *    Second, if there have been no keys added to the UCTypeSelectRef
 *    via calls to UCTypeSelectAddKeyToSelectorData(),
 *    kUCTSNoKeysAddedToObjectErr will be returned. Finally, it can
 *    also return other errors should any be encountered by
 *    intermediate function calls.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectCompare( ref: UCTypeSelectRef; inText: CFStringRef; var result: UCTypeSelectCompareResult ): OSStatus; external name '_UCTypeSelectCompare';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UCTypeSelectFindItem()
 *  
 *  Summary:
 *    In a given list, pointed to by listDataPtr, find the closest
 *    match to the keystrokes stored in the UCTypeSelectRef. The
 *    closest match’s index will be returned in closestItem. The list
 *    to be searched may be ordered or unordered. In order for this
 *    call to work, the client needs to provide a an IndexToUCString
 *    callback UPP. This callback is necessary in order to provide the
 *    client with data structure independence through client-side
 *    indexing.
 *  
 *  Parameters:
 *    
 *    ref:
 *      UCTypeSelectRef holding the state and keystrokes to be compared.
 *    
 *    listSize:
 *      Size of the list to be searched through. If the size of the
 *      list is unknown, pass in kUCTypeSelectMaxListSize (0xFFFFFFFF)
 *      and have the IndexToUCString function return false after it has
 *      reached the last item in the list.
 *    
 *    listDataPtr:
 *      Pointer to the head or first node of the client’s data
 *      structure. This will be passed into to the client’s
 *      IndexToUCString function. Can be NULL, depending on the
 *      client’s IndexToUCString implementation.
 *    
 *    refcon:
 *      Any parameter the calling function wishes to pass as a
 *      reference into its IndexToUCString callback function. This
 *      parameter can be set to NULL if not needed.
 *    
 *    userUPP:
 *      The UPP pointing to the client’s IndexToUCString callback
 *      function.
 *    
 *    closestItem:
 *      Upon return, this will contain the index of the item that
 *      matches the text in the keystroke buffer of UCTypeSelectRef.
 *  
 *  Result:
 *    This function has four possibilities for return values. First,
 *    paramErr will be returned if ref or closestItem are invalid.
 *    Second, if the search list is empty or if the first item cannot
 *    be read, kUCTSSearchListErr will be returned. Third, if there
 *    have been no keys added to the UCTypeSelectRef via calls to
 *    UCTypeSelectAddKeyToSelectorData(), kUCTSNoKeysAddedToObjectErr
 *    will be returned. Finally, this function can return other OS
 *    errors should any be encountered by an internal function call.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectFindItem( ref: UCTypeSelectRef; listSize: UInt32; listDataPtr: UnivPtr { can be NULL }; refcon: UnivPtr { can be NULL }; userUPP: IndexToUCStringUPP; var closestItem: UInt32 ): OSStatus; external name '_UCTypeSelectFindItem';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UCTypeSelectWalkList()
 *  
 *  Summary:
 *    UCTypeSelectWalkList can perform an in-order alphabetical walk of
 *    an unsorted list. To do this, the client passes a pointer to the
 *    current selection’s text in the currSelect parameter.
 *    UCTypeSelectWalkList will then search the list pointed to by
 *    listDataPtr for the closest item that is lexicographically either
 *    right before or right after the selected item. The client can
 *    choose which one to be returned by setting the direction
 *    parameter to kUCTSDirectionPrevious or kUCTSDirectionNext to get
 *    the item either lexicographically right before or after
 *    respectively. This call will not walk off the end of the list or
 *    do any wraparound searching. That is, if the item selected is the
 *    last item in the list and kUCDirectionNext is specified for the
 *    direction, that same item will be returned. Likewise for the case
 *    where the first item is selected nd kUCTSDirectionPrevious is
 *    specified. In order for this call to work, the client needs to
 *    provide an IndexToUCString callback UPP. This callback is
 *    necessary in order to provide the client with data structure
 *    independence through client-side indexing.
 *  
 *  Parameters:
 *    
 *    ref:
 *      UCTypeSelectRef holding state information as well as the
 *      function pointer needed to call the client’s IndexToUCString
 *      function
 *    
 *    currSelect:
 *      CFString reference to the current selection’s text.
 *    
 *    direction:
 *      The direction of the walk. The valid values for this parameter
 *      are: 
 *      
 *      kUCTSDirectionNext  - find the next item in the list
 *       kUCTSDirectionPrevious - find the previous item in the list
 *      
 *      
 *      If kUCTSDirectionNext is specified and the selected item is the
 *      last item in the list or if kUCTSDirectionPrevious is specified
 *      and the selected item is the first item in the list, the index
 *      of the selected item will be returned in closestItem.
 *    
 *    listSize:
 *      Size of the list to be searched through. If the size of the
 *      list is unknown, pass in kUCTypeSelectMaxListSize (0xFFFFFFFF)
 *      and have the IndexToUCString function return false after it has
 *      reached the last item in the list.
 *    
 *    listDataPtr:
 *      Pointer to the head or first node of the client’s data
 *      structure. This will be passed into to the client’s
 *      IndexToUCString function. Can be NULL, depending on the
 *      client’s IndexToUCString implementation.
 *    
 *    refcon:
 *      Any parameter the calling function wishes to pass as a
 *      reference into its IndexToUCString callback function. This
 *      parameter can be set to NULL if not needed.
 *    
 *    userUPP:
 *      The UPP pointing to the client’s IndexToUCString callback
 *      function.
 *    
 *    closestItem:
 *      Upon return, this will contain the index of the item that
 *      matches the text in the keystroke buffer of UCTypeSelectRef.
 *  
 *  Result:
 *    This function has three possibilities for return values. First,
 *    paramErr will be returned if ref, currSelect, or closestItem are
 *    invalid. Second, if the search list is empty or if the first item
 *    cannot be read, kUCTSSearchListErr  will be returned. Finally,
 *    this function can return other OS errors should any be
 *    encountered by an internal function call.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function UCTypeSelectWalkList( ref: UCTypeSelectRef; currSelect: CFStringRef; direction: UCTSWalkDirection; listSize: UInt32; listDataPtr: UnivPtr { can be NULL }; refcon: UnivPtr { can be NULL }; userUPP: IndexToUCStringUPP; var closestItem: UInt32 ): OSStatus; external name '_UCTypeSelectWalkList';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

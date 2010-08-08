{
     File:       LangAnalysis/Dictionary.h
 
     Contains:   Dictionary Manager Interfaces
 
     Version:    LanguageAnalysis-214~9
 
     Copyright:  © 1992-2008 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit Dictionary;
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
uses MacTypes,AEDataModel,Files,AERegistry,CodeFragments,MacErrors;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}

{
=============================================================================================
 Modern Dictionary Manager
=============================================================================================
}
{
    Dictionary information
}
const
	kDictionaryFileType = FourCharCode('dict');
	kDCMDictionaryHeaderSignature = FourCharCode('dict');
	kDCMDictionaryHeaderVersion = 2;

const
	kDCMAnyFieldTag = typeWildCard;
	kDCMAnyFieldType = typeWildCard;

{
    Contents of a Field Info Record (an AERecord)
}
const
	keyDCMFieldTag = FourCharCode('ftag'); { typeEnumeration }
	keyDCMFieldType = FourCharCode('ftyp'); { typeEnumeration }
	keyDCMMaxRecordSize = FourCharCode('mrsz'); { typeMagnitude }
	keyDCMFieldAttributes = FourCharCode('fatr');
	keyDCMFieldDefaultData = FourCharCode('fdef');
	keyDCMFieldName = FourCharCode('fnam'); { typeChar }
	keyDCMFieldFindMethods = FourCharCode('ffnd'); { typeAEList of typeDCMFindMethod }

{
    Special types for fields of a Field Info Record
}
const
	typeDCMFieldAttributes = FourCharCode('fatr');
	typeDCMFindMethod = FourCharCode('fmth');


{
    Field attributes
}
const
	kDCMIndexedFieldMask = $00000001;
	kDCMRequiredFieldMask = $00000002;
	kDCMIdentifyFieldMask = $00000004;
	kDCMFixedSizeFieldMask = $00000008;
	kDCMHiddenFieldMask = $80000000;

type
	DCMFieldAttributes = OptionBits;
{
    Standard dictionary properties
}
const
	pDCMAccessMethod = FourCharCode('amtd'); { data type: typeChar ReadOnly }
	pDCMPermission = FourCharCode('perm'); { data type: typeUInt16 }
	pDCMListing = FourCharCode('list'); { data type: typeUInt16 }
	pDCMMaintenance = FourCharCode('mtnc'); { data type: typeUInt16 }
	pDCMLocale = FourCharCode('locl'); { data type: typeUInt32.  Optional; default = kLocaleIdentifierWildCard }
	pDCMClass = pClass; { data type: typeUInt16 }
	pDCMCopyright = FourCharCode('info'); { data type: typeChar }

{
    pDCMPermission property constants
}
const
	kDCMReadOnlyDictionary = 0;
	kDCMReadWriteDictionary = 1;

{
    pDCMListing property constants
}
const
	kDCMAllowListing = 0;
	kDCMProhibitListing = 1;

{
    pDCMClass property constants
}
const
	kDCMUserDictionaryClass = 0;
	kDCMSpecificDictionaryClass = 1;
	kDCMBasicDictionaryClass = 2;

{
    Standard search method
}
const
	kDCMFindMethodExactMatch = kAEEquals;
	kDCMFindMethodBeginningMatch = kAEBeginsWith;
	kDCMFindMethodContainsMatch = kAEContains;
	kDCMFindMethodEndingMatch = kAEEndsWith;
	kDCMFindMethodForwardTrie = FourCharCode('ftri'); { used for morphological analysis}
	kDCMFindMethodBackwardTrie = FourCharCode('btri'); { used for morphological analysis}

type
	DCMFindMethod = OSType;
	DCMFindMethodPtr = ^DCMFindMethod;
{
    AccessMethod features
}
const
	kDCMCanUseFileDictionaryMask = $00000001;
	kDCMCanUseMemoryDictionaryMask = $00000002;
	kDCMCanStreamDictionaryMask = $00000004;
	kDCMCanHaveMultipleIndexMask = $00000008;
	kDCMCanModifyDictionaryMask = $00000010;
	kDCMCanCreateDictionaryMask = $00000020;
	kDCMCanAddDictionaryFieldMask = $00000040;
	kDCMCanUseTransactionMask = $00000080;

type
	DCMAccessMethodFeature = OptionBits;
	DCMUniqueID = UInt32;
	DCMObjectID = ^OpaqueDCMObjectID; { an opaque type }
	OpaqueDCMObjectID = record end;
	DCMObjectIDPtr = ^DCMObjectID;  { when a var xx:DCMObjectID parameter can be nil, it is changed to xx: DCMObjectIDPtr }
	DCMAccessMethodID = DCMObjectID;
	DCMDictionaryID = DCMObjectID;
	DCMObjectRef = ^OpaqueDCMObjectRef; { an opaque type }
	OpaqueDCMObjectRef = record end;
	DCMObjectRefPtr = ^DCMObjectRef;  { when a var xx:DCMObjectRef parameter can be nil, it is changed to xx: DCMObjectRefPtr }
	DCMDictionaryRef = DCMObjectRef;
	DCMDictionaryStreamRef = DCMObjectRef;
	DCMObjectIterator = ^OpaqueDCMObjectIterator; { an opaque type }
	OpaqueDCMObjectIterator = record end;
	DCMObjectIteratorPtr = ^DCMObjectIterator;  { when a var xx:DCMObjectIterator parameter can be nil, it is changed to xx: DCMObjectIteratorPtr }
	DCMAccessMethodIterator = DCMObjectIterator;
	DCMDictionaryIterator = DCMObjectIterator;
	DCMFoundRecordIterator = ^OpaqueDCMFoundRecordIterator; { an opaque type }
	OpaqueDCMFoundRecordIterator = record end;
	DCMFoundRecordIteratorPtr = ^DCMFoundRecordIterator;  { when a var xx:DCMFoundRecordIterator parameter can be nil, it is changed to xx: DCMFoundRecordIteratorPtr }
{
    Field specification declarations
}
type
	DCMFieldTag = DescType;
	DCMFieldTagPtr = ^DCMFieldTag;
	DCMFieldType = DescType;
{
    Dictionary header information
}
type
	DCMDictionaryHeader = record
		headerSignature: FourCharCode;
		headerVersion: UInt32;
		headerSize: UInt32;
		accessMethod: Str63;
	end;
{
    Callback routines
}
type
	DCMProgressFilterProcPtr = function( determinateProcess: Boolean; percentageComplete: UInt16; callbackUD: UInt32 ): Boolean;
	DCMProgressFilterUPP = DCMProgressFilterProcPtr;
{
 *  NewDCMProgressFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  DisposeDCMProgressFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  InvokeDCMProgressFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{$ifc not TARGET_CPU_64}
{
    Library version
}
{$ifc not TARGET_CPU_64}
{
 *  DCMLibraryVersion()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMLibraryVersion: UInt32; external name '_DCMLibraryVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Create/delete dictionary
}
{
 *  DCMNewDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMNewDictionary( accessMethodID: DCMAccessMethodID; const (*var*) newDictionaryFile: FSSpec; scriptTag: ScriptCode; const (*var*) listOfFieldInfoRecords: AEDesc; invisible: Boolean; recordCapacity: ItemCount; var newDictionary: DCMDictionaryID ): OSStatus; external name '_DCMNewDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMDeriveNewDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMDeriveNewDictionary( srcDictionary: DCMDictionaryID; const (*var*) newDictionaryFile: FSSpec; scriptTag: ScriptCode; invisible: Boolean; recordCapacity: ItemCount; var newDictionary: DCMDictionaryID ): OSStatus; external name '_DCMDeriveNewDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMDeleteDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMDeleteDictionary( dictionaryID: DCMDictionaryID ): OSStatus; external name '_DCMDeleteDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Register dictionary
}
{
 *  DCMRegisterDictionaryFile()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMRegisterDictionaryFile( const (*var*) dictionaryFile: FSSpec; var dictionaryID: DCMDictionaryID ): OSStatus; external name '_DCMRegisterDictionaryFile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMUnregisterDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMUnregisterDictionary( dictionaryID: DCMDictionaryID ): OSStatus; external name '_DCMUnregisterDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Open dictionary
}
{
 *  DCMOpenDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMOpenDictionary( dictionaryID: DCMDictionaryID; protectKeySize: ByteCount; protectKey: ConstLogicalAddress; var dictionaryRef: DCMDictionaryRef ): OSStatus; external name '_DCMOpenDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMCloseDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCloseDictionary( dictionaryRef: DCMDictionaryRef ): OSStatus; external name '_DCMCloseDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Change access privilege
}
{
 *  DCMGetDictionaryWriteAccess()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetDictionaryWriteAccess( dictionaryRef: DCMDictionaryRef; timeOutDuration: Duration ): OSStatus; external name '_DCMGetDictionaryWriteAccess';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMReleaseDictionaryWriteAccess()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMReleaseDictionaryWriteAccess( dictionaryRef: DCMDictionaryRef; commitTransaction: Boolean ): OSStatus; external name '_DCMReleaseDictionaryWriteAccess';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Find records
}
{
 *  DCMFindRecords()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMFindRecords( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; findMethod: DCMFindMethod; preFetchedDataNum: ItemCount; preFetchedData: {variable-size-array} DCMFieldTagPtr; skipCount: ItemCount; maxRecordCount: ItemCount; var recordIterator: DCMFoundRecordIterator ): OSStatus; external name '_DCMFindRecords';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMCountRecordIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCountRecordIterator( recordIterator: DCMFoundRecordIterator ): ItemCount; external name '_DCMCountRecordIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMIterateFoundRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMIterateFoundRecord( recordIterator: DCMFoundRecordIterator; maxKeySize: ByteCount; var actualKeySize: ByteCount; keyData: LogicalAddress; var uniqueID: DCMUniqueID; var dataList: AEDesc ): OSStatus; external name '_DCMIterateFoundRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMDisposeRecordIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMDisposeRecordIterator( recordIterator: DCMFoundRecordIterator ): OSStatus; external name '_DCMDisposeRecordIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Dump dictionary
}
{
 *  DCMCountRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCountRecord( dictionaryID: DCMDictionaryID; var count: ItemCount ): OSStatus; external name '_DCMCountRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetRecordSequenceNumber()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetRecordSequenceNumber( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; var sequenceNum: ItemCount ): OSStatus; external name '_DCMGetRecordSequenceNumber';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetNthRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetNthRecord( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; serialNum: ItemCount; maxKeySize: ByteCount; var keySize: ByteCount; keyData: LogicalAddress; var uniqueID: DCMUniqueID ): OSStatus; external name '_DCMGetNthRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetNextRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetNextRecord( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; maxKeySize: ByteCount; var nextKeySize: ByteCount; nextKeyData: LogicalAddress; var nextUniqueID: DCMUniqueID ): OSStatus; external name '_DCMGetNextRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetPrevRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetPrevRecord( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; maxKeySize: ByteCount; var prevKeySize: ByteCount; prevKeyData: LogicalAddress; var prevUniqueID: DCMUniqueID ): OSStatus; external name '_DCMGetPrevRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Get field data
}
{
 *  DCMGetFieldData()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetFieldData( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; numOfData: ItemCount; {const} dataTag: {variable-size-array} DCMFieldTagPtr; var dataList: AEDesc ): OSStatus; external name '_DCMGetFieldData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMSetFieldData()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMSetFieldData( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; const (*var*) dataList: AEDesc ): OSStatus; external name '_DCMSetFieldData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Add record
}
{
 *  DCMAddRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMAddRecord( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; checkOnly: Boolean; const (*var*) dataList: AEDesc; var newUniqueID: DCMUniqueID ): OSStatus; external name '_DCMAddRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMDeleteRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMDeleteRecord( dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID ): OSStatus; external name '_DCMDeleteRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Reorganize/compact dictionary
}
{
 *  DCMReorganizeDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMReorganizeDictionary( dictionaryID: DCMDictionaryID; extraCapacity: ItemCount; progressProc: DCMProgressFilterUPP; userData: UInt32 ): OSStatus; external name '_DCMReorganizeDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMCompactDictionary()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCompactDictionary( dictionaryID: DCMDictionaryID; progressProc: DCMProgressFilterUPP; userData: UInt32 ): OSStatus; external name '_DCMCompactDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    DictionaryID utilities
}
{
 *  DCMGetFileFromDictionaryID()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetFileFromDictionaryID( dictionaryID: DCMDictionaryID; var fileRef: FSSpec ): OSStatus; external name '_DCMGetFileFromDictionaryID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetDictionaryIDFromFile()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetDictionaryIDFromFile( const (*var*) fileRef: FSSpec; var dictionaryID: DCMDictionaryID ): OSStatus; external name '_DCMGetDictionaryIDFromFile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetDictionaryIDFromRef()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetDictionaryIDFromRef( dictionaryRef: DCMDictionaryRef ): DCMDictionaryID; external name '_DCMGetDictionaryIDFromRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Field information and manipulation
}
{
 *  DCMGetDictionaryFieldInfo()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetDictionaryFieldInfo( dictionaryID: DCMDictionaryID; fieldTag: DCMFieldTag; var fieldInfoRecord: AEDesc ): OSStatus; external name '_DCMGetDictionaryFieldInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Dictionary property
}
{
 *  DCMGetDictionaryProperty()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetDictionaryProperty( dictionaryID: DCMDictionaryID; propertyTag: DCMFieldTag; maxPropertySize: ByteCount; var actualSize: ByteCount; propertyValue: LogicalAddress ): OSStatus; external name '_DCMGetDictionaryProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMSetDictionaryProperty()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMSetDictionaryProperty( dictionaryID: DCMDictionaryID; propertyTag: DCMFieldTag; propertySize: ByteCount; propertyValue: ConstLogicalAddress ): OSStatus; external name '_DCMSetDictionaryProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetDictionaryPropertyList()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetDictionaryPropertyList( dictionaryID: DCMDictionaryID; maxPropertyNum: ItemCount; var numProperties: ItemCount; propertyTag: {variable-size-array} DCMFieldTagPtr ): OSStatus; external name '_DCMGetDictionaryPropertyList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Seaarch dictionary
}
{
 *  DCMCreateDictionaryIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCreateDictionaryIterator( var dictionaryIterator: DCMDictionaryIterator ): OSStatus; external name '_DCMCreateDictionaryIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Search AccessMethod
}
{
 *  DCMCreateAccessMethodIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCreateAccessMethodIterator( var accessMethodIterator: DCMAccessMethodIterator ): OSStatus; external name '_DCMCreateAccessMethodIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Iterator Operation
}
{
 *  DCMCountObjectIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCountObjectIterator( iterator: DCMObjectIterator ): ItemCount; external name '_DCMCountObjectIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMIterateObject()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMIterateObject( iterator: DCMObjectIterator; var objectID: DCMObjectID ): OSStatus; external name '_DCMIterateObject';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMResetObjectIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMResetObjectIterator( iterator: DCMObjectIterator ): OSStatus; external name '_DCMResetObjectIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMDisposeObjectIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMDisposeObjectIterator( iterator: DCMObjectIterator ): OSStatus; external name '_DCMDisposeObjectIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Get AccessMethod information
}
{
 *  DCMGetAccessMethodIDFromName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetAccessMethodIDFromName( const (*var*) accessMethodName: Str63; var accessMethodID: DCMAccessMethodID ): OSStatus; external name '_DCMGetAccessMethodIDFromName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
    Field Info Record routines
}
{
 *  DCMCreateFieldInfoRecord()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMCreateFieldInfoRecord( fieldTag: DescType; fieldType: DescType; maxRecordSize: ByteCount; fieldAttributes: DCMFieldAttributes; var fieldDefaultData: AEDesc; numberOfFindMethods: ItemCount; findMethods: {variable-size-array} DCMFindMethodPtr; var fieldInfoRecord: AEDesc ): OSStatus; external name '_DCMCreateFieldInfoRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetFieldTagAndType()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetFieldTagAndType( const (*var*) fieldInfoRecord: AEDesc; var fieldTag: DCMFieldTag; var fieldType: DCMFieldType ): OSStatus; external name '_DCMGetFieldTagAndType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetFieldMaxRecordSize()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetFieldMaxRecordSize( const (*var*) fieldInfoRecord: AEDesc; var maxRecordSize: ByteCount ): OSStatus; external name '_DCMGetFieldMaxRecordSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetFieldAttributes()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetFieldAttributes( const (*var*) fieldInfoRecord: AEDesc; var attributes: DCMFieldAttributes ): OSStatus; external name '_DCMGetFieldAttributes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetFieldDefaultData()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetFieldDefaultData( const (*var*) fieldInfoRecord: AEDesc; desiredType: DescType; var fieldDefaultData: AEDesc ): OSStatus; external name '_DCMGetFieldDefaultData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DCMGetFieldFindMethods()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Dictionary Manager is deprecated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 }
function DCMGetFieldFindMethods( const (*var*) fieldInfoRecord: AEDesc; findMethodsArrayMaxSize: ItemCount; findMethods: {variable-size-array} DCMFindMethodPtr; var actualNumberOfFindMethods: ItemCount ): OSStatus; external name '_DCMGetFieldFindMethods';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{$endc} {not TARGET_CPU_64}

{$endc} {not TARGET_CPU_64}

{
    Check Dictionary Manager availability
}
{$ifc TARGET_RT_MAC_CFM}
{
        DCMDictionaryManagerAvailable() is a macro available only in C/C++.  
        To get the same functionality from pascal or assembly, you need
        to test if Dictionary Manager functions are not NULL.
        For instance:
        
            IF @DCMLibraryVersion <> kUnresolvedCFragSymbolAddress THEN
                gDictionaryManagerAvailable = TRUE;
            ELSE
                gDictionaryManagerAvailable = FALSE;
            end
    
}
{$elsec}
  {$ifc TARGET_RT_MAC_MACHO}
{ Dictionary Manager is always available on OS X }
  {$endc}
{$endc}

{
=============================================================================================
    Definitions for Japanese Analysis Module
=============================================================================================
}
{
    Default dictionary access method for Japanese analysis
}
const
	kAppleJapaneseDefaultAccessMethodName = 'DAM:Apple Backward Trie Access Method';
{
    Data length limitations of Apple Japanese dictionaries
}

const
	kMaxYomiLengthInAppleJapaneseDictionary = 40;
	kMaxKanjiLengthInAppleJapaneseDictionary = 64;

{
    Defined field tags of Apple Japanese dictionary
}
const
	kDCMJapaneseYomiTag = FourCharCode('yomi');
	kDCMJapaneseHyokiTag = FourCharCode('hyok');
	kDCMJapaneseHinshiTag = FourCharCode('hins');
	kDCMJapaneseWeightTag = FourCharCode('hind');
	kDCMJapanesePhoneticTag = FourCharCode('hton');
	kDCMJapaneseAccentTag = FourCharCode('acnt');
	kDCMJapaneseOnKunReadingTag = FourCharCode('OnKn');
	kDCMJapaneseFukugouInfoTag = FourCharCode('fuku');

const
	kDCMJapaneseYomiType = typeUnicodeText;
	kDCMJapaneseHyokiType = typeUnicodeText;
	kDCMJapaneseHinshiType = FourCharCode('hins');
	kDCMJapaneseWeightType = typeSInt16;
	kDCMJapanesePhoneticType = typeUnicodeText;
	kDCMJapaneseAccentType = FourCharCode('byte');
	kDCMJapaneseOnKunReadingType = typeUnicodeText;
	kDCMJapaneseFukugouInfoType = FourCharCode('fuku');


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

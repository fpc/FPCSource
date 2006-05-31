{
     File:       Dictionary.p
 
     Contains:   Dictionary Manager Interfaces
 
     Version:    Technology: System 7
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1992-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit Dictionary;
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
uses MacTypes,AEDataModel,Files,AERegistry,CodeFragments,MacErrors;


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
	kDictionaryFileType			= $64696374 (* 'dict' *);
	kDCMDictionaryHeaderSignature = $64696374 (* 'dict' *);
	kDCMDictionaryHeaderVersion	= 2;

	kDCMAnyFieldTag				= $2A2A2A2A (* '****' *);
	kDCMAnyFieldType			= $2A2A2A2A (* '****' *);

	{	
	    Contents of a Field Info Record (an AERecord)
		}
	keyDCMFieldTag				= $66746167 (* 'ftag' *);						{  typeEnumeration  }
	keyDCMFieldType				= $66747970 (* 'ftyp' *);						{  typeEnumeration  }
	keyDCMMaxRecordSize			= $6D72737A (* 'mrsz' *);						{  typeMagnitude  }
	keyDCMFieldAttributes		= $66617472 (* 'fatr' *);
	keyDCMFieldDefaultData		= $66646566 (* 'fdef' *);
	keyDCMFieldName				= $666E616D (* 'fnam' *);						{  typeChar  }
	keyDCMFieldFindMethods		= $66666E64 (* 'ffnd' *);						{  typeAEList of typeDCMFindMethod  }

	{	
	    Special types for fields of a Field Info Record
		}
	typeDCMFieldAttributes		= $66617472 (* 'fatr' *);
	typeDCMFindMethod			= $666D7468 (* 'fmth' *);


	{	
	    Field attributes
		}
	kDCMIndexedFieldMask		= $00000001;
	kDCMRequiredFieldMask		= $00000002;
	kDCMIdentifyFieldMask		= $00000004;
	kDCMFixedSizeFieldMask		= $00000008;
	kDCMHiddenFieldMask			= $80000000;


type
	DCMFieldAttributes					= OptionBits;
	{	
	    Standard dictionary properties
		}

const
	pDCMAccessMethod			= $616D7464 (* 'amtd' *);						{  data type: typeChar ReadOnly  }
	pDCMPermission				= $7065726D (* 'perm' *);						{  data type: typeUInt16  }
	pDCMListing					= $6C697374 (* 'list' *);						{  data type: typeUInt16  }
	pDCMMaintenance				= $6D746E63 (* 'mtnc' *);						{  data type: typeUInt16  }
	pDCMLocale					= $6C6F636C (* 'locl' *);						{  data type: typeUInt32.  Optional; default = kLocaleIdentifierWildCard  }
	pDCMClass					= $70636C73 (* 'pcls' *);						{  data type: typeUInt16  }
	pDCMCopyright				= $696E666F (* 'info' *);						{  data type: typeChar  }

	{	
	    pDCMPermission property constants
		}
	kDCMReadOnlyDictionary		= 0;
	kDCMReadWriteDictionary		= 1;

	{	
	    pDCMListing property constants
		}
	kDCMAllowListing			= 0;
	kDCMProhibitListing			= 1;

	{	
	    pDCMClass property constants
		}
	kDCMUserDictionaryClass		= 0;
	kDCMSpecificDictionaryClass	= 1;
	kDCMBasicDictionaryClass	= 2;

	{	
	    Standard search method
		}
	kDCMFindMethodExactMatch	= $3D202020 (* '=   ' *);
	kDCMFindMethodBeginningMatch = $62677774 (* 'bgwt' *);
	kDCMFindMethodContainsMatch	= $636F6E74 (* 'cont' *);
	kDCMFindMethodEndingMatch	= $656E6473 (* 'ends' *);
	kDCMFindMethodForwardTrie	= $66747269 (* 'ftri' *);						{  used for morphological analysis }
	kDCMFindMethodBackwardTrie	= $62747269 (* 'btri' *);						{  used for morphological analysis }


type
	DCMFindMethod						= OSType;
	{	
	    AccessMethod features
		}

const
	kDCMCanUseFileDictionaryMask = $00000001;
	kDCMCanUseMemoryDictionaryMask = $00000002;
	kDCMCanStreamDictionaryMask	= $00000004;
	kDCMCanHaveMultipleIndexMask = $00000008;
	kDCMCanModifyDictionaryMask	= $00000010;
	kDCMCanCreateDictionaryMask	= $00000020;
	kDCMCanAddDictionaryFieldMask = $00000040;
	kDCMCanUseTransactionMask	= $00000080;


type
	DCMAccessMethodFeature				= OptionBits;
	DCMUniqueID							= UInt32;
	DCMObjectID    = ^SInt32; { an opaque 32-bit type }
	DCMObjectIDPtr = ^DCMObjectID;  { when a var xx:DCMObjectID parameter can be nil, it is changed to xx: DCMObjectIDPtr }
	DCMAccessMethodID					= DCMObjectID;
	DCMDictionaryID						= DCMObjectID;
	DCMObjectRef    = ^SInt32; { an opaque 32-bit type }
	DCMObjectRefPtr = ^DCMObjectRef;  { when a var xx:DCMObjectRef parameter can be nil, it is changed to xx: DCMObjectRefPtr }
	DCMDictionaryRef					= DCMObjectRef;
	DCMDictionaryStreamRef				= DCMObjectRef;
	DCMObjectIterator    = ^SInt32; { an opaque 32-bit type }
	DCMObjectIteratorPtr = ^DCMObjectIterator;  { when a var xx:DCMObjectIterator parameter can be nil, it is changed to xx: DCMObjectIteratorPtr }
	DCMAccessMethodIterator				= DCMObjectIterator;
	DCMDictionaryIterator				= DCMObjectIterator;
	DCMFoundRecordIterator    = ^SInt32; { an opaque 32-bit type }
	DCMFoundRecordIteratorPtr = ^DCMFoundRecordIterator;  { when a var xx:DCMFoundRecordIterator parameter can be nil, it is changed to xx: DCMFoundRecordIteratorPtr }
	{	
	    Field specification declarations
		}
	DCMFieldTag							= DescType;
	DCMFieldTagPtr							= ^DCMFieldTag;
	DCMFieldType						= DescType;
	{	
	    Dictionary header information
		}
	DCMDictionaryHeaderPtr = ^DCMDictionaryHeader;
	DCMDictionaryHeader = record
		headerSignature:		FourCharCode;
		headerVersion:			UInt32;
		headerSize:				ByteCount;
		accessMethod:			Str63;
	end;

	{	
	    Callback routines
		}
{$ifc TYPED_FUNCTION_POINTERS}
	DCMProgressFilterProcPtr = function(determinateProcess: boolean; percentageComplete: UInt16; callbackUD: UInt32): boolean;
{$elsec}
	DCMProgressFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	DCMProgressFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DCMProgressFilterUPP = UniversalProcPtr;
{$endc}	

const
	uppDCMProgressFilterProcInfo = $00000E50;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewDCMProgressFilterUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewDCMProgressFilterUPP(userRoutine: DCMProgressFilterProcPtr): DCMProgressFilterUPP; external name '_NewDCMProgressFilterUPP'; { old name was NewDCMProgressFilterProc }
{
 *  DisposeDCMProgressFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposeDCMProgressFilterUPP(userUPP: DCMProgressFilterUPP); external name '_DisposeDCMProgressFilterUPP';
{
 *  InvokeDCMProgressFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InvokeDCMProgressFilterUPP(determinateProcess: boolean; percentageComplete: UInt16; callbackUD: UInt32; userRoutine: DCMProgressFilterUPP): boolean; external name '_InvokeDCMProgressFilterUPP'; { old name was CallDCMProgressFilterProc }
{$endc}  {CALL_NOT_IN_CARBON}

{
    Library version
}
{
 *  DCMLibraryVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMLibraryVersion: UInt32; external name '_DCMLibraryVersion';

{
    Create/delete dictionary
}
{
 *  DCMNewDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMNewDictionary(accessMethodID: DCMAccessMethodID; const (*var*) newDictionaryFile: FSSpec; scriptTag: ScriptCode; const (*var*) listOfFieldInfoRecords: AEDesc; invisible: boolean; recordCapacity: ItemCount; var newDictionary: DCMDictionaryID): OSStatus; external name '_DCMNewDictionary';

{
 *  DCMDeriveNewDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMDeriveNewDictionary(srcDictionary: DCMDictionaryID; const (*var*) newDictionaryFile: FSSpec; scriptTag: ScriptCode; invisible: boolean; recordCapacity: ItemCount; var newDictionary: DCMDictionaryID): OSStatus; external name '_DCMDeriveNewDictionary';

{
 *  DCMDeleteDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMDeleteDictionary(dictionaryID: DCMDictionaryID): OSStatus; external name '_DCMDeleteDictionary';

{
    Register dictionary
}
{
 *  DCMRegisterDictionaryFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMRegisterDictionaryFile(const (*var*) dictionaryFile: FSSpec; var dictionaryID: DCMDictionaryID): OSStatus; external name '_DCMRegisterDictionaryFile';

{
 *  DCMUnregisterDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMUnregisterDictionary(dictionaryID: DCMDictionaryID): OSStatus; external name '_DCMUnregisterDictionary';

{
    Open dictionary
}
{
 *  DCMOpenDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMOpenDictionary(dictionaryID: DCMDictionaryID; protectKeySize: ByteCount; protectKey: ConstLogicalAddress; var dictionaryRef: DCMDictionaryRef): OSStatus; external name '_DCMOpenDictionary';

{
 *  DCMCloseDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCloseDictionary(dictionaryRef: DCMDictionaryRef): OSStatus; external name '_DCMCloseDictionary';

{
    Change access privilege
}
{
 *  DCMGetDictionaryWriteAccess()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetDictionaryWriteAccess(dictionaryRef: DCMDictionaryRef; timeOutDuration: Duration): OSStatus; external name '_DCMGetDictionaryWriteAccess';

{
 *  DCMReleaseDictionaryWriteAccess()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMReleaseDictionaryWriteAccess(dictionaryRef: DCMDictionaryRef; commitTransaction: boolean): OSStatus; external name '_DCMReleaseDictionaryWriteAccess';

{
    Find records
}
{
 *  DCMFindRecords()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMFindRecords(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; findMethod: DCMFindMethod; preFetchedDataNum: ItemCount; preFetchedData: DCMFieldTagPtr; skipCount: ItemCount; maxRecordCount: ItemCount; var recordIterator: DCMFoundRecordIterator): OSStatus; external name '_DCMFindRecords';

{
 *  DCMCountRecordIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCountRecordIterator(recordIterator: DCMFoundRecordIterator): ItemCount; external name '_DCMCountRecordIterator';

{
 *  DCMIterateFoundRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMIterateFoundRecord(recordIterator: DCMFoundRecordIterator; maxKeySize: ByteCount; var actualKeySize: ByteCount; keyData: LogicalAddress; var uniqueID: DCMUniqueID; var dataList: AEDesc): OSStatus; external name '_DCMIterateFoundRecord';

{
 *  DCMDisposeRecordIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMDisposeRecordIterator(recordIterator: DCMFoundRecordIterator): OSStatus; external name '_DCMDisposeRecordIterator';

{
    Dump dictionary
}
{
 *  DCMCountRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCountRecord(dictionaryID: DCMDictionaryID; var count: ItemCount): OSStatus; external name '_DCMCountRecord';

{
 *  DCMGetRecordSequenceNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetRecordSequenceNumber(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; var sequenceNum: ItemCount): OSStatus; external name '_DCMGetRecordSequenceNumber';

{
 *  DCMGetNthRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetNthRecord(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; serialNum: ItemCount; maxKeySize: ByteCount; var keySize: ByteCount; keyData: LogicalAddress; var uniqueID: DCMUniqueID): OSStatus; external name '_DCMGetNthRecord';

{
 *  DCMGetNextRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetNextRecord(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; maxKeySize: ByteCount; var nextKeySize: ByteCount; nextKeyData: LogicalAddress; var nextUniqueID: DCMUniqueID): OSStatus; external name '_DCMGetNextRecord';

{
 *  DCMGetPrevRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetPrevRecord(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; maxKeySize: ByteCount; var prevKeySize: ByteCount; prevKeyData: LogicalAddress; var prevUniqueID: DCMUniqueID): OSStatus; external name '_DCMGetPrevRecord';

{
    Get field data
}
{
 *  DCMGetFieldData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetFieldData(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; numOfData: ItemCount; dataTag: DCMFieldTagPtr; var dataList: AEDesc): OSStatus; external name '_DCMGetFieldData';

{
 *  DCMSetFieldData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMSetFieldData(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID; const (*var*) dataList: AEDesc): OSStatus; external name '_DCMSetFieldData';

{
    Add record
}
{
 *  DCMAddRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMAddRecord(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; checkOnly: boolean; const (*var*) dataList: AEDesc; var newUniqueID: DCMUniqueID): OSStatus; external name '_DCMAddRecord';

{
 *  DCMDeleteRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMDeleteRecord(dictionaryRef: DCMDictionaryRef; keyFieldTag: DCMFieldTag; keySize: ByteCount; keyData: ConstLogicalAddress; uniqueID: DCMUniqueID): OSStatus; external name '_DCMDeleteRecord';

{
    Reorganize/compact dictionary
}
{
 *  DCMReorganizeDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMReorganizeDictionary(dictionaryID: DCMDictionaryID; extraCapacity: ItemCount; progressProc: DCMProgressFilterUPP; userData: UInt32): OSStatus; external name '_DCMReorganizeDictionary';

{
 *  DCMCompactDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCompactDictionary(dictionaryID: DCMDictionaryID; progressProc: DCMProgressFilterUPP; userData: UInt32): OSStatus; external name '_DCMCompactDictionary';

{
    DictionaryID utilities
}
{
 *  DCMGetFileFromDictionaryID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetFileFromDictionaryID(dictionaryID: DCMDictionaryID; var fileRef: FSSpec): OSStatus; external name '_DCMGetFileFromDictionaryID';

{
 *  DCMGetDictionaryIDFromFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetDictionaryIDFromFile(const (*var*) fileRef: FSSpec; var dictionaryID: DCMDictionaryID): OSStatus; external name '_DCMGetDictionaryIDFromFile';

{
 *  DCMGetDictionaryIDFromRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetDictionaryIDFromRef(dictionaryRef: DCMDictionaryRef): DCMDictionaryID; external name '_DCMGetDictionaryIDFromRef';

{
    Field information and manipulation
}
{
 *  DCMGetDictionaryFieldInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetDictionaryFieldInfo(dictionaryID: DCMDictionaryID; fieldTag: DCMFieldTag; var fieldInfoRecord: AEDesc): OSStatus; external name '_DCMGetDictionaryFieldInfo';

{
    Dictionary property
}
{
 *  DCMGetDictionaryProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetDictionaryProperty(dictionaryID: DCMDictionaryID; propertyTag: DCMFieldTag; maxPropertySize: ByteCount; var actualSize: ByteCount; propertyValue: LogicalAddress): OSStatus; external name '_DCMGetDictionaryProperty';

{
 *  DCMSetDictionaryProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMSetDictionaryProperty(dictionaryID: DCMDictionaryID; propertyTag: DCMFieldTag; propertySize: ByteCount; propertyValue: ConstLogicalAddress): OSStatus; external name '_DCMSetDictionaryProperty';

{
 *  DCMGetDictionaryPropertyList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetDictionaryPropertyList(dictionaryID: DCMDictionaryID; maxPropertyNum: ItemCount; var numProperties: ItemCount; propertyTag: DCMFieldTagPtr): OSStatus; external name '_DCMGetDictionaryPropertyList';

{
    Seaarch dictionary
}
{
 *  DCMCreateDictionaryIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCreateDictionaryIterator(var dictionaryIterator: DCMDictionaryIterator): OSStatus; external name '_DCMCreateDictionaryIterator';

{
    Search AccessMethod
}
{
 *  DCMCreateAccessMethodIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCreateAccessMethodIterator(var accessMethodIterator: DCMAccessMethodIterator): OSStatus; external name '_DCMCreateAccessMethodIterator';

{
    Iterator Operation
}
{
 *  DCMCountObjectIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCountObjectIterator(iterator: DCMObjectIterator): ItemCount; external name '_DCMCountObjectIterator';

{
 *  DCMIterateObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMIterateObject(iterator: DCMObjectIterator; var objectID: DCMObjectID): OSStatus; external name '_DCMIterateObject';

{
 *  DCMResetObjectIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMResetObjectIterator(iterator: DCMObjectIterator): OSStatus; external name '_DCMResetObjectIterator';

{
 *  DCMDisposeObjectIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMDisposeObjectIterator(iterator: DCMObjectIterator): OSStatus; external name '_DCMDisposeObjectIterator';

{
    Get AccessMethod information
}
{
 *  DCMGetAccessMethodIDFromName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetAccessMethodIDFromName(const (*var*) accessMethodName: Str63; var accessMethodID: DCMAccessMethodID): OSStatus; external name '_DCMGetAccessMethodIDFromName';

{
    Field Info Record routines
}
{
 *  DCMCreateFieldInfoRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMCreateFieldInfoRecord(fieldTag: DescType; fieldType: DescType; maxRecordSize: ByteCount; fieldAttributes: DCMFieldAttributes; var fieldDefaultData: AEDesc; numberOfFindMethods: ItemCount; var findMethods: DCMFindMethod; var fieldInfoRecord: AEDesc): OSStatus; external name '_DCMCreateFieldInfoRecord';

{
 *  DCMGetFieldTagAndType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetFieldTagAndType(const (*var*) fieldInfoRecord: AEDesc; var fieldTag: DCMFieldTag; var fieldType: DCMFieldType): OSStatus; external name '_DCMGetFieldTagAndType';

{
 *  DCMGetFieldMaxRecordSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetFieldMaxRecordSize(const (*var*) fieldInfoRecord: AEDesc; var maxRecordSize: ByteCount): OSStatus; external name '_DCMGetFieldMaxRecordSize';

{
 *  DCMGetFieldAttributes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetFieldAttributes(const (*var*) fieldInfoRecord: AEDesc; var attributes: DCMFieldAttributes): OSStatus; external name '_DCMGetFieldAttributes';

{
 *  DCMGetFieldDefaultData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetFieldDefaultData(const (*var*) fieldInfoRecord: AEDesc; desiredType: DescType; var fieldDefaultData: AEDesc): OSStatus; external name '_DCMGetFieldDefaultData';

{
 *  DCMGetFieldFindMethods()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DictionaryMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DCMGetFieldFindMethods(const (*var*) fieldInfoRecord: AEDesc; findMethodsArrayMaxSize: ItemCount; var findMethods: DCMFindMethod; var actualNumberOfFindMethods: ItemCount): OSStatus; external name '_DCMGetFieldFindMethods';

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
{
    Data length limitations of Apple Japanese dictionaries
}

const
	kMaxYomiLengthInAppleJapaneseDictionary = 40;
	kMaxKanjiLengthInAppleJapaneseDictionary = 64;

	{	
	    Defined field tags of Apple Japanese dictionary
		}
	kDCMJapaneseYomiTag			= $796F6D69 (* 'yomi' *);
	kDCMJapaneseHyokiTag		= $68796F6B (* 'hyok' *);
	kDCMJapaneseHinshiTag		= $68696E73 (* 'hins' *);
	kDCMJapaneseWeightTag		= $68696E64 (* 'hind' *);
	kDCMJapanesePhoneticTag		= $68746F6E (* 'hton' *);
	kDCMJapaneseAccentTag		= $61636E74 (* 'acnt' *);
	kDCMJapaneseOnKunReadingTag	= $4F6E4B6E (* 'OnKn' *);
	kDCMJapaneseFukugouInfoTag	= $66756B75 (* 'fuku' *);

	kDCMJapaneseYomiType		= $75747874 (* 'utxt' *);
	kDCMJapaneseHyokiType		= $75747874 (* 'utxt' *);
	kDCMJapaneseHinshiType		= $68696E73 (* 'hins' *);
	kDCMJapaneseWeightType		= $73686F72 (* 'shor' *);
	kDCMJapanesePhoneticType	= $75747874 (* 'utxt' *);
	kDCMJapaneseAccentType		= $62797465 (* 'byte' *);
	kDCMJapaneseOnKunReadingType = $75747874 (* 'utxt' *);
	kDCMJapaneseFukugouInfoType	= $66756B75 (* 'fuku' *);


	{	
	=============================================================================================
	 System 7 Dictionary Manager
	=============================================================================================
		}
{$ALIGN MAC68K}
																{  Dictionary data insertion modes  }
	kInsert						= 0;							{  Only insert the input entry if there is nothing in the dictionary that matches the key.  }
	kReplace					= 1;							{  Only replace the entries which match the key with the input entry.  }
	kInsertOrReplace			= 2;							{  Insert the entry if there is nothing in the dictionary which matches the key, otherwise replaces the existing matched entries with the input entry.  }

	{	 This Was InsertMode 	}

type
	DictionaryDataInsertMode			= SInt16;

const
																{  Key attribute constants  }
	kIsCaseSensitive			= $10;							{  case sensitive = 16        }
	kIsNotDiacriticalSensitive	= $20;							{  diac not sensitive = 32     }

																{  Registered attribute type constants.    }
	kNoun						= -1;
	kVerb						= -2;
	kAdjective					= -3;
	kAdverb						= -4;

	{	 This Was AttributeType 	}

type
	DictionaryEntryAttribute			= SInt8;
	{	 Dictionary information record 	}
	DictionaryInformationPtr = ^DictionaryInformation;
	DictionaryInformation = record
		dictionaryFSSpec:		FSSpec;
		numberOfRecords:		SInt32;
		currentGarbageSize:		SInt32;
		script:					ScriptCode;
		maximumKeyLength:		SInt16;
		keyAttributes:			SInt8;
	end;

	DictionaryAttributeTablePtr = ^DictionaryAttributeTable;
	DictionaryAttributeTable = packed record
		datSize:				UInt8;
		datTable:				array [0..0] of DictionaryEntryAttribute;
	end;

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  InitializeDictionary()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function InitializeDictionary(const (*var*) theFsspecPtr: FSSpec; maximumKeyLength: SInt16; keyAttributes: SInt8; script: ScriptCode): OSErr; external name '_InitializeDictionary';
{
 *  OpenDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenDictionary(const (*var*) theFsspecPtr: FSSpec; accessPermission: SInt8; var dictionaryReference: SInt32): OSErr; external name '_OpenDictionary';
{
 *  CloseDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CloseDictionary(dictionaryReference: SInt32): OSErr; external name '_CloseDictionary';
{
 *  InsertRecordToDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InsertRecordToDictionary(dictionaryReference: SInt32; const (*var*) key: Str255; recordDataHandle: Handle; whichMode: DictionaryDataInsertMode): OSErr; external name '_InsertRecordToDictionary';
{
 *  DeleteRecordFromDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DeleteRecordFromDictionary(dictionaryReference: SInt32; const (*var*) key: Str255): OSErr; external name '_DeleteRecordFromDictionary';
{
 *  FindRecordInDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FindRecordInDictionary(dictionaryReference: SInt32; const (*var*) key: Str255; requestedAttributeTablePointer: DictionaryAttributeTablePtr; recordDataHandle: Handle): OSErr; external name '_FindRecordInDictionary';
{
 *  FindRecordByIndexInDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FindRecordByIndexInDictionary(dictionaryReference: SInt32; recordIndex: SInt32; requestedAttributeTablePointer: DictionaryAttributeTablePtr; var recordKey: Str255; recordDataHandle: Handle): OSErr; external name '_FindRecordByIndexInDictionary';
{
 *  GetDictionaryInformation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDictionaryInformation(dictionaryReference: SInt32; var theDictionaryInformation: DictionaryInformation): OSErr; external name '_GetDictionaryInformation';
{
 *  CompactDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CompactDictionary(dictionaryReference: SInt32): OSErr; external name '_CompactDictionary';
{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.

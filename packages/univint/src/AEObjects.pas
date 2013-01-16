{
     File:       AE/AEObjects.h
 
     Contains:   Object Support Library Interfaces.
 
    
 
     Copyright:  © 1991-2008 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, September 2011 }
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

unit AEObjects;
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
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
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
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes,AEDataModel,OSUtils,AppleEvents,MacErrors;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

const
{*** LOGICAL OPERATOR CONSTANTS  ***}
	kAEAND = FourCharCode('AND '); {  0x414e4420  }
	kAEOR = FourCharCode('OR  '); {  0x4f522020  }
	kAENOT = FourCharCode('NOT '); {  0x4e4f5420  }
                                        {*** ABSOLUTE ORDINAL CONSTANTS  ***}
	kAEFirst = FourCharCode('firs'); {  0x66697273  }
	kAELast = FourCharCode('last'); {  0x6c617374  }
	kAEMiddle = FourCharCode('midd'); {  0x6d696464  }
	kAEAny = FourCharCode('any '); {  0x616e7920  }
	kAEAll = FourCharCode('all '); {  0x616c6c20  }
                                        {*** RELATIVE ORDINAL CONSTANTS  ***}
	kAENext = FourCharCode('next'); {  0x6e657874  }
	kAEPrevious = FourCharCode('prev'); {  0x70726576  }
                                        {*** KEYWORD CONSTANT    ***}
	keyAECompOperator = FourCharCode('relo'); {  0x72656c6f  }
	keyAELogicalTerms = FourCharCode('term'); {  0x7465726d  }
	keyAELogicalOperator = FourCharCode('logc'); {  0x6c6f6763  }
	keyAEObject1 = FourCharCode('obj1'); {  0x6f626a31  }
	keyAEObject2 = FourCharCode('obj2'); {  0x6f626a32  }
                                        {    ... for Keywords for getting fields out of object specifier records. }
	keyAEDesiredClass = FourCharCode('want'); {  0x77616e74  }
	keyAEContainer = FourCharCode('from'); {  0x66726f6d  }
	keyAEKeyForm = FourCharCode('form'); {  0x666f726d  }
	keyAEKeyData = FourCharCode('seld'); {  0x73656c64  }

const
{    ... for Keywords for getting fields out of Range specifier records. }
	keyAERangeStart = FourCharCode('star'); {  0x73746172  }
	keyAERangeStop = FourCharCode('stop'); {  0x73746f70  }
                                        {    ... special handler selectors for OSL Callbacks. }
	keyDisposeTokenProc = FourCharCode('xtok'); {  0x78746f6b  }
	keyAECompareProc = FourCharCode('cmpr'); {  0x636d7072  }
	keyAECountProc = FourCharCode('cont'); {  0x636f6e74  }
	keyAEMarkTokenProc = FourCharCode('mkid'); {  0x6d6b6964  }
	keyAEMarkProc = FourCharCode('mark'); {  0x6d61726b  }
	keyAEAdjustMarksProc = FourCharCode('adjm'); {  0x61646a6d  }
	keyAEGetErrDescProc = FourCharCode('indc'); {  0x696e6463  }

{***   VALUE and TYPE CONSTANTS    ***}
const
{    ... possible values for the keyAEKeyForm field of an object specifier. }
	formAbsolutePosition = FourCharCode('indx'); {  0x696e6478  }
	formRelativePosition = FourCharCode('rele'); {  0x72656c65  }
	formTest = FourCharCode('test'); {  0x74657374  }
	formRange = FourCharCode('rang'); {  0x72616e67  }
	formPropertyID = FourCharCode('prop'); {  0x70726f70  }
	formName = FourCharCode('name'); {  0x6e616d65  }
	formUniqueID = FourCharCode('ID  '); {  0x49442020  }
                                        {    ... relevant types (some of these are often pared with forms above). }
	typeObjectSpecifier = FourCharCode('obj '); {  0x6f626a20  }
	typeObjectBeingExamined = FourCharCode('exmn'); {  0x65786d6e  }
	typeCurrentContainer = FourCharCode('ccnt'); {  0x63636e74  }
	typeToken = FourCharCode('toke'); {  0x746f6b65  }
	typeRelativeDescriptor = FourCharCode('rel '); {  0x72656c20  }
	typeAbsoluteOrdinal = FourCharCode('abso'); {  0x6162736f  }
	typeIndexDescriptor = FourCharCode('inde'); {  0x696e6465  }
	typeRangeDescriptor = FourCharCode('rang'); {  0x72616e67  }
	typeLogicalDescriptor = FourCharCode('logi'); {  0x6c6f6769  }
	typeCompDescriptor = FourCharCode('cmpd'); {  0x636d7064  }
	typeOSLTokenList = FourCharCode('ostl'); {  0x6F73746C  }

{ Possible values for flags parameter to AEResolve.  They're additive }
const
	kAEIDoMinimum = $0000;
	kAEIDoWhose = $0001;
	kAEIDoMarking = $0004;
	kAEPassSubDescs = $0008;
	kAEResolveNestedLists = $0010;
	kAEHandleSimpleRanges = $0020;
	kAEUseRelativeIterators = $0040;

{*** SPECIAL CONSTANTS FOR CUSTOM WHOSE-CLAUSE RESOLUTION }
const
	typeWhoseDescriptor = FourCharCode('whos'); {  0x77686f73  }
	formWhose = FourCharCode('whos'); {  0x77686f73  }
	typeWhoseRange = FourCharCode('wrng'); {  0x77726e67  }
	keyAEWhoseRangeStart = FourCharCode('wstr'); {  0x77737472  }
	keyAEWhoseRangeStop = FourCharCode('wstp'); {  0x77737470  }
	keyAEIndex = FourCharCode('kidx'); {  0x6b696478  }
	keyAETest = FourCharCode('ktst'); {  0x6b747374  }

{
    used for rewriting tokens in place of 'ccnt' descriptors
    This record is only of interest to those who, when they...
    ...get ranges as key data in their accessor procs, choose
    ...to resolve them manually rather than call AEResolve again.
}
type
	ccntTokenRecordPtr = ^ccntTokenRecord;
	ccntTokenRecord = record
		tokenClass: DescType;
		token: AEDesc;
	end;
type
	ccntTokenRecPtr = ccntTokenRecordPtr;
	ccntTokenRecHandle = ^ccntTokenRecPtr;
{$ifc OLDROUTINENAMES}
type
	DescPtr = AEDescPtr;
	DescHandle = DescPtrPtr;
{$endc} {OLDROUTINENAMES}

{ typedefs providing type checking for procedure pointers }
type
	OSLAccessorProcPtr = function( desiredClass: DescType; const (*var*) container: AEDesc; containerClass: DescType; form: DescType; const (*var*) selectionData: AEDesc; var value: AEDesc; accessorRefcon: SRefCon ): OSErr;
	OSLCompareProcPtr = function( oper: DescType; const (*var*) obj1: AEDesc; const (*var*) obj2: AEDesc; var result: Boolean ): OSErr;
	OSLCountProcPtr = function( desiredType: DescType; containerClass: DescType; const (*var*) container: AEDesc; var result: SIGNEDLONG ): OSErr;
	OSLDisposeTokenProcPtr = function( var unneededToken: AEDesc ): OSErr;
	OSLGetMarkTokenProcPtr = function( const (*var*) dContainerToken: AEDesc; containerClass: DescType; var result: AEDesc ): OSErr;
	OSLGetErrDescProcPtr = function( var appDescPtr: AEDescPtr ): OSErr;
	OSLMarkProcPtr = function( const (*var*) dToken: AEDesc; const (*var*) markToken: AEDesc; index: SIGNEDLONG ): OSErr;
	OSLAdjustMarksProcPtr = function( newStart: SIGNEDLONG; newStop: SIGNEDLONG; const (*var*) markToken: AEDesc ): OSErr;
	OSLAccessorUPP = OSLAccessorProcPtr;
	OSLCompareUPP = OSLCompareProcPtr;
	OSLCountUPP = OSLCountProcPtr;
	OSLDisposeTokenUPP = OSLDisposeTokenProcPtr;
	OSLGetMarkTokenUPP = OSLGetMarkTokenProcPtr;
	OSLGetErrDescUPP = OSLGetErrDescProcPtr;
	OSLMarkUPP = OSLMarkProcPtr;
	OSLAdjustMarksUPP = OSLAdjustMarksProcPtr;
{
 *  NewOSLAccessorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLAccessorUPP( userRoutine: OSLAccessorProcPtr ): OSLAccessorUPP; external name '_NewOSLAccessorUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  NewOSLCompareUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLCompareUPP( userRoutine: OSLCompareProcPtr ): OSLCompareUPP; external name '_NewOSLCompareUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  NewOSLCountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLCountUPP( userRoutine: OSLCountProcPtr ): OSLCountUPP; external name '_NewOSLCountUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  NewOSLDisposeTokenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLDisposeTokenUPP( userRoutine: OSLDisposeTokenProcPtr ): OSLDisposeTokenUPP; external name '_NewOSLDisposeTokenUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  NewOSLGetMarkTokenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLGetMarkTokenUPP( userRoutine: OSLGetMarkTokenProcPtr ): OSLGetMarkTokenUPP; external name '_NewOSLGetMarkTokenUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  NewOSLGetErrDescUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLGetErrDescUPP( userRoutine: OSLGetErrDescProcPtr ): OSLGetErrDescUPP; external name '_NewOSLGetErrDescUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  NewOSLMarkUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLMarkUPP( userRoutine: OSLMarkProcPtr ): OSLMarkUPP; external name '_NewOSLMarkUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  NewOSLAdjustMarksUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewOSLAdjustMarksUPP( userRoutine: OSLAdjustMarksProcPtr ): OSLAdjustMarksUPP; external name '_NewOSLAdjustMarksUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLAccessorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLAccessorUPP( userUPP: OSLAccessorUPP ); external name '_DisposeOSLAccessorUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLCompareUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLCompareUPP( userUPP: OSLCompareUPP ); external name '_DisposeOSLCompareUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLCountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLCountUPP( userUPP: OSLCountUPP ); external name '_DisposeOSLCountUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLDisposeTokenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLDisposeTokenUPP( userUPP: OSLDisposeTokenUPP ); external name '_DisposeOSLDisposeTokenUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLGetMarkTokenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLGetMarkTokenUPP( userUPP: OSLGetMarkTokenUPP ); external name '_DisposeOSLGetMarkTokenUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLGetErrDescUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLGetErrDescUPP( userUPP: OSLGetErrDescUPP ); external name '_DisposeOSLGetErrDescUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLMarkUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLMarkUPP( userUPP: OSLMarkUPP ); external name '_DisposeOSLMarkUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  DisposeOSLAdjustMarksUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeOSLAdjustMarksUPP( userUPP: OSLAdjustMarksUPP ); external name '_DisposeOSLAdjustMarksUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLAccessorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLAccessorUPP( desiredClass: DescType; const (*var*) container: AEDesc; containerClass: DescType; form: DescType; const (*var*) selectionData: AEDesc; var value: AEDesc; accessorRefcon: SRefCon; userUPP: OSLAccessorUPP ): OSErr; external name '_InvokeOSLAccessorUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLCompareUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLCompareUPP( oper: DescType; const (*var*) obj1: AEDesc; const (*var*) obj2: AEDesc; var result: Boolean; userUPP: OSLCompareUPP ): OSErr; external name '_InvokeOSLCompareUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLCountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLCountUPP( desiredType: DescType; containerClass: DescType; const (*var*) container: AEDesc; var result: SIGNEDLONG; userUPP: OSLCountUPP ): OSErr; external name '_InvokeOSLCountUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLDisposeTokenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLDisposeTokenUPP( var unneededToken: AEDesc; userUPP: OSLDisposeTokenUPP ): OSErr; external name '_InvokeOSLDisposeTokenUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLGetMarkTokenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLGetMarkTokenUPP( const (*var*) dContainerToken: AEDesc; containerClass: DescType; var result: AEDesc; userUPP: OSLGetMarkTokenUPP ): OSErr; external name '_InvokeOSLGetMarkTokenUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLGetErrDescUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLGetErrDescUPP( var appDescPtr: AEDescPtr; userUPP: OSLGetErrDescUPP ): OSErr; external name '_InvokeOSLGetErrDescUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLMarkUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLMarkUPP( const (*var*) dToken: AEDesc; const (*var*) markToken: AEDesc; index: SIGNEDLONG; userUPP: OSLMarkUPP ): OSErr; external name '_InvokeOSLMarkUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  InvokeOSLAdjustMarksUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeOSLAdjustMarksUPP( newStart: SIGNEDLONG; newStop: SIGNEDLONG; const (*var*) markToken: AEDesc; userUPP: OSLAdjustMarksUPP ): OSErr; external name '_InvokeOSLAdjustMarksUPP';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{
 *  AEObjectInit()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AEObjectInit: OSErr; external name '_AEObjectInit';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{ Not done by inline, but by direct linking into code.  It sets up the pack
  such that further calls can be via inline }
{
 *  AESetObjectCallbacks()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AESetObjectCallbacks( myCompareProc: OSLCompareUPP; myCountProc: OSLCountUPP; myDisposeTokenProc: OSLDisposeTokenUPP; myGetMarkTokenProc: OSLGetMarkTokenUPP; myMarkProc: OSLMarkUPP; myAdjustMarksProc: OSLAdjustMarksUPP; myGetErrDescProcPtr: OSLGetErrDescUPP ): OSErr; external name '_AESetObjectCallbacks';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AEResolve()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AEResolve( const (*var*) objectSpecifier: AEDesc; callbackFlags: SInt16; var theToken: AEDesc ): OSErr; external name '_AEResolve';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AEInstallObjectAccessor()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AEInstallObjectAccessor( desiredClass: DescType; containerType: DescType; theAccessor: OSLAccessorUPP; accessorRefcon: SRefCon; isSysHandler: Boolean ): OSErr; external name '_AEInstallObjectAccessor';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AERemoveObjectAccessor()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AERemoveObjectAccessor( desiredClass: DescType; containerType: DescType; theAccessor: OSLAccessorUPP; isSysHandler: Boolean ): OSErr; external name '_AERemoveObjectAccessor';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AEGetObjectAccessor()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AEGetObjectAccessor( desiredClass: DescType; containerType: DescType; var accessor: OSLAccessorUPP; var accessorRefcon: SRefCon; isSysHandler: Boolean ): OSErr; external name '_AEGetObjectAccessor';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AEDisposeToken()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AEDisposeToken( var theToken: AEDesc ): OSErr; external name '_AEDisposeToken';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AECallObjectAccessor()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 }
function AECallObjectAccessor( desiredClass: DescType; const (*var*) containerToken: AEDesc; containerClass: DescType; keyForm: DescType; const (*var*) keyData: AEDesc; var token: AEDesc ): OSErr; external name '_AECallObjectAccessor';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

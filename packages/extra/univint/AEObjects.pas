{
     File:       AEObjects.p
 
     Contains:   Object Support Library Interfaces.
 
     Version:    Technology: System 8.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1991-2002 by Apple Computer, Inc., all rights reserved
 
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

unit AEObjects;
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
uses MacTypes,AEDataModel,OSUtils,AppleEvents,MacErrors;


{$ALIGN MAC68K}


const
																{ *** LOGICAL OPERATOR CONSTANTS  *** }
	kAEAND						= $414E4420 (* 'AND ' *);						{   0x414e4420   }
	kAEOR						= $4F522020 (* 'OR  ' *);						{   0x4f522020   }
	kAENOT						= $4E4F5420 (* 'NOT ' *);						{   0x4e4f5420   }
																{ *** ABSOLUTE ORDINAL CONSTANTS  *** }
	kAEFirst					= $66697273 (* 'firs' *);						{   0x66697273   }
	kAELast						= $6C617374 (* 'last' *);						{   0x6c617374   }
	kAEMiddle					= $6D696464 (* 'midd' *);						{   0x6d696464   }
	kAEAny						= $616E7920 (* 'any ' *);						{   0x616e7920   }
	kAEAll						= $616C6C20 (* 'all ' *);						{   0x616c6c20   }
																{ *** RELATIVE ORDINAL CONSTANTS  *** }
	kAENext						= $6E657874 (* 'next' *);						{   0x6e657874   }
	kAEPrevious					= $70726576 (* 'prev' *);						{   0x70726576   }
																{ *** KEYWORD CONSTANT    *** }
	keyAECompOperator			= $72656C6F (* 'relo' *);						{   0x72656c6f   }
	keyAELogicalTerms			= $7465726D (* 'term' *);						{   0x7465726d   }
	keyAELogicalOperator		= $6C6F6763 (* 'logc' *);						{   0x6c6f6763   }
	keyAEObject1				= $6F626A31 (* 'obj1' *);						{   0x6f626a31   }
	keyAEObject2				= $6F626A32 (* 'obj2' *);						{   0x6f626a32   }
																{     ... for Keywords for getting fields out of object specifier records.  }
	keyAEDesiredClass			= $77616E74 (* 'want' *);						{   0x77616e74   }
	keyAEContainer				= $66726F6D (* 'from' *);						{   0x66726f6d   }
	keyAEKeyForm				= $666F726D (* 'form' *);						{   0x666f726d   }
	keyAEKeyData				= $73656C64 (* 'seld' *);						{   0x73656c64   }

																{     ... for Keywords for getting fields out of Range specifier records.  }
	keyAERangeStart				= $73746172 (* 'star' *);						{   0x73746172   }
	keyAERangeStop				= $73746F70 (* 'stop' *);						{   0x73746f70   }
																{     ... special handler selectors for OSL Callbacks.  }
	keyDisposeTokenProc			= $78746F6B (* 'xtok' *);						{   0x78746f6b   }
	keyAECompareProc			= $636D7072 (* 'cmpr' *);						{   0x636d7072   }
	keyAECountProc				= $636F6E74 (* 'cont' *);						{   0x636f6e74   }
	keyAEMarkTokenProc			= $6D6B6964 (* 'mkid' *);						{   0x6d6b6964   }
	keyAEMarkProc				= $6D61726B (* 'mark' *);						{   0x6d61726b   }
	keyAEAdjustMarksProc		= $61646A6D (* 'adjm' *);						{   0x61646a6d   }
	keyAEGetErrDescProc			= $696E6463 (* 'indc' *);						{   0x696e6463   }

	{	***   VALUE and type CONSTANTS    ***	}
																{     ... possible values for the keyAEKeyForm field of an object specifier.  }
	formAbsolutePosition		= $696E6478 (* 'indx' *);						{   0x696e6478   }
	formRelativePosition		= $72656C65 (* 'rele' *);						{   0x72656c65   }
	formTest					= $74657374 (* 'test' *);						{   0x74657374   }
	formRange					= $72616E67 (* 'rang' *);						{   0x72616e67   }
	formPropertyID				= $70726F70 (* 'prop' *);						{   0x70726f70   }
	formName					= $6E616D65 (* 'name' *);						{   0x6e616d65   }
																{     ... relevant types (some of these are often pared with forms above).  }
	typeObjectSpecifier			= $6F626A20 (* 'obj ' *);						{   0x6f626a20   }
	typeObjectBeingExamined		= $65786D6E (* 'exmn' *);						{   0x65786d6e   }
	typeCurrentContainer		= $63636E74 (* 'ccnt' *);						{   0x63636e74   }
	typeToken					= $746F6B65 (* 'toke' *);						{   0x746f6b65   }
	typeRelativeDescriptor		= $72656C20 (* 'rel ' *);						{   0x72656c20   }
	typeAbsoluteOrdinal			= $6162736F (* 'abso' *);						{   0x6162736f   }
	typeIndexDescriptor			= $696E6465 (* 'inde' *);						{   0x696e6465   }
	typeRangeDescriptor			= $72616E67 (* 'rang' *);						{   0x72616e67   }
	typeLogicalDescriptor		= $6C6F6769 (* 'logi' *);						{   0x6c6f6769   }
	typeCompDescriptor			= $636D7064 (* 'cmpd' *);						{   0x636d7064   }
	typeOSLTokenList			= $6F73746C (* 'ostl' *);						{   0x6F73746C   }

	{	 Possible values for flags parameter to AEResolve.  They're additive 	}
	kAEIDoMinimum				= $0000;
	kAEIDoWhose					= $0001;
	kAEIDoMarking				= $0004;
	kAEPassSubDescs				= $0008;
	kAEResolveNestedLists		= $0010;
	kAEHandleSimpleRanges		= $0020;
	kAEUseRelativeIterators		= $0040;

	{	*** SPECIAL CONSTANTS FOR CUSTOM WHOSE-CLAUSE RESOLUTION 	}
	typeWhoseDescriptor			= $77686F73 (* 'whos' *);						{   0x77686f73   }
	formWhose					= $77686F73 (* 'whos' *);						{   0x77686f73   }
	typeWhoseRange				= $77726E67 (* 'wrng' *);						{   0x77726e67   }
	keyAEWhoseRangeStart		= $77737472 (* 'wstr' *);						{   0x77737472   }
	keyAEWhoseRangeStop			= $77737470 (* 'wstp' *);						{   0x77737470   }
	keyAEIndex					= $6B696478 (* 'kidx' *);						{   0x6b696478   }
	keyAETest					= $6B747374 (* 'ktst' *);						{   0x6b747374   }

	{	
	    used for rewriting tokens in place of 'ccnt' descriptors
	    This record is only of interest to those who, when they...
	    ...get ranges as key data in their accessor procs, choose
	    ...to resolve them manually rather than call AEResolve again.
		}

type
	ccntTokenRecordPtr = ^ccntTokenRecord;
	ccntTokenRecord = record
		tokenClass:				DescType;
		token:					AEDesc;
	end;

	ccntTokenRecPtr						= ^ccntTokenRecord;
	ccntTokenRecHandle					= ^ccntTokenRecPtr;
{$ifc OLDROUTINENAMES}
	DescPtr								= ^AEDesc;
	DescHandle							= ^DescPtr;
{$endc}  {OLDROUTINENAMES}

	{	 typedefs providing type checking for procedure pointers 	}
{$ifc TYPED_FUNCTION_POINTERS}
	OSLAccessorProcPtr = function(desiredClass: DescType; const (*var*) container: AEDesc; containerClass: DescType; form: DescType; const (*var*) selectionData: AEDesc; var value: AEDesc; accessorRefcon: SInt32): OSErr;
{$elsec}
	OSLAccessorProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSLCompareProcPtr = function(oper: DescType; const (*var*) obj1: AEDesc; const (*var*) obj2: AEDesc; var result: boolean): OSErr;
{$elsec}
	OSLCompareProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSLCountProcPtr = function(desiredType: DescType; containerClass: DescType; const (*var*) container: AEDesc; var result: SInt32): OSErr;
{$elsec}
	OSLCountProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSLDisposeTokenProcPtr = function(var unneededToken: AEDesc): OSErr;
{$elsec}
	OSLDisposeTokenProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSLGetMarkTokenProcPtr = function(const (*var*) dContainerToken: AEDesc; containerClass: DescType; var result: AEDesc): OSErr;
{$elsec}
	OSLGetMarkTokenProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSLGetErrDescProcPtr = function(var appDescPtr: AEDescPtr): OSErr;
{$elsec}
	OSLGetErrDescProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSLMarkProcPtr = function(const (*var*) dToken: AEDesc; const (*var*) markToken: AEDesc; index: SInt32): OSErr;
{$elsec}
	OSLMarkProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSLAdjustMarksProcPtr = function(newStart: SInt32; newStop: SInt32; const (*var*) markToken: AEDesc): OSErr;
{$elsec}
	OSLAdjustMarksProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	OSLAccessorUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLAccessorUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSLCompareUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLCompareUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSLCountUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLCountUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSLDisposeTokenUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLDisposeTokenUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSLGetMarkTokenUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLGetMarkTokenUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSLGetErrDescUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLGetErrDescUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSLMarkUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLMarkUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSLAdjustMarksUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSLAdjustMarksUPP = UniversalProcPtr;
{$endc}	

const
	uppOSLAccessorProcInfo = $000FFFE0;
	uppOSLCompareProcInfo = $00003FE0;
	uppOSLCountProcInfo = $00003FE0;
	uppOSLDisposeTokenProcInfo = $000000E0;
	uppOSLGetMarkTokenProcInfo = $00000FE0;
	uppOSLGetErrDescProcInfo = $000000E0;
	uppOSLMarkProcInfo = $00000FE0;
	uppOSLAdjustMarksProcInfo = $00000FE0;
	{
	 *  NewOSLAccessorUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewOSLAccessorUPP(userRoutine: OSLAccessorProcPtr): OSLAccessorUPP; external name '_NewOSLAccessorUPP'; { old name was NewOSLAccessorProc }
{
 *  NewOSLCompareUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSLCompareUPP(userRoutine: OSLCompareProcPtr): OSLCompareUPP; external name '_NewOSLCompareUPP'; { old name was NewOSLCompareProc }
{
 *  NewOSLCountUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSLCountUPP(userRoutine: OSLCountProcPtr): OSLCountUPP; external name '_NewOSLCountUPP'; { old name was NewOSLCountProc }
{
 *  NewOSLDisposeTokenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSLDisposeTokenUPP(userRoutine: OSLDisposeTokenProcPtr): OSLDisposeTokenUPP; external name '_NewOSLDisposeTokenUPP'; { old name was NewOSLDisposeTokenProc }
{
 *  NewOSLGetMarkTokenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSLGetMarkTokenUPP(userRoutine: OSLGetMarkTokenProcPtr): OSLGetMarkTokenUPP; external name '_NewOSLGetMarkTokenUPP'; { old name was NewOSLGetMarkTokenProc }
{
 *  NewOSLGetErrDescUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSLGetErrDescUPP(userRoutine: OSLGetErrDescProcPtr): OSLGetErrDescUPP; external name '_NewOSLGetErrDescUPP'; { old name was NewOSLGetErrDescProc }
{
 *  NewOSLMarkUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSLMarkUPP(userRoutine: OSLMarkProcPtr): OSLMarkUPP; external name '_NewOSLMarkUPP'; { old name was NewOSLMarkProc }
{
 *  NewOSLAdjustMarksUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSLAdjustMarksUPP(userRoutine: OSLAdjustMarksProcPtr): OSLAdjustMarksUPP; external name '_NewOSLAdjustMarksUPP'; { old name was NewOSLAdjustMarksProc }
{
 *  DisposeOSLAccessorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLAccessorUPP(userUPP: OSLAccessorUPP); external name '_DisposeOSLAccessorUPP';
{
 *  DisposeOSLCompareUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLCompareUPP(userUPP: OSLCompareUPP); external name '_DisposeOSLCompareUPP';
{
 *  DisposeOSLCountUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLCountUPP(userUPP: OSLCountUPP); external name '_DisposeOSLCountUPP';
{
 *  DisposeOSLDisposeTokenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLDisposeTokenUPP(userUPP: OSLDisposeTokenUPP); external name '_DisposeOSLDisposeTokenUPP';
{
 *  DisposeOSLGetMarkTokenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLGetMarkTokenUPP(userUPP: OSLGetMarkTokenUPP); external name '_DisposeOSLGetMarkTokenUPP';
{
 *  DisposeOSLGetErrDescUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLGetErrDescUPP(userUPP: OSLGetErrDescUPP); external name '_DisposeOSLGetErrDescUPP';
{
 *  DisposeOSLMarkUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLMarkUPP(userUPP: OSLMarkUPP); external name '_DisposeOSLMarkUPP';
{
 *  DisposeOSLAdjustMarksUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSLAdjustMarksUPP(userUPP: OSLAdjustMarksUPP); external name '_DisposeOSLAdjustMarksUPP';
{
 *  InvokeOSLAccessorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLAccessorUPP(desiredClass: DescType; const (*var*) container: AEDesc; containerClass: DescType; form: DescType; const (*var*) selectionData: AEDesc; var value: AEDesc; accessorRefcon: SInt32; userRoutine: OSLAccessorUPP): OSErr; external name '_InvokeOSLAccessorUPP'; { old name was CallOSLAccessorProc }
{
 *  InvokeOSLCompareUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLCompareUPP(oper: DescType; const (*var*) obj1: AEDesc; const (*var*) obj2: AEDesc; var result: boolean; userRoutine: OSLCompareUPP): OSErr; external name '_InvokeOSLCompareUPP'; { old name was CallOSLCompareProc }
{
 *  InvokeOSLCountUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLCountUPP(desiredType: DescType; containerClass: DescType; const (*var*) container: AEDesc; var result: SInt32; userRoutine: OSLCountUPP): OSErr; external name '_InvokeOSLCountUPP'; { old name was CallOSLCountProc }
{
 *  InvokeOSLDisposeTokenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLDisposeTokenUPP(var unneededToken: AEDesc; userRoutine: OSLDisposeTokenUPP): OSErr; external name '_InvokeOSLDisposeTokenUPP'; { old name was CallOSLDisposeTokenProc }
{
 *  InvokeOSLGetMarkTokenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLGetMarkTokenUPP(const (*var*) dContainerToken: AEDesc; containerClass: DescType; var result: AEDesc; userRoutine: OSLGetMarkTokenUPP): OSErr; external name '_InvokeOSLGetMarkTokenUPP'; { old name was CallOSLGetMarkTokenProc }
{
 *  InvokeOSLGetErrDescUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLGetErrDescUPP(var appDescPtr: AEDescPtr; userRoutine: OSLGetErrDescUPP): OSErr; external name '_InvokeOSLGetErrDescUPP'; { old name was CallOSLGetErrDescProc }
{
 *  InvokeOSLMarkUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLMarkUPP(const (*var*) dToken: AEDesc; const (*var*) markToken: AEDesc; index: SInt32; userRoutine: OSLMarkUPP): OSErr; external name '_InvokeOSLMarkUPP'; { old name was CallOSLMarkProc }
{
 *  InvokeOSLAdjustMarksUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSLAdjustMarksUPP(newStart: SInt32; newStop: SInt32; const (*var*) markToken: AEDesc; userRoutine: OSLAdjustMarksUPP): OSErr; external name '_InvokeOSLAdjustMarksUPP'; { old name was CallOSLAdjustMarksProc }
{
 *  AEObjectInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEObjectInit: OSErr; external name '_AEObjectInit';

{ Not done by inline, but by direct linking into code.  It sets up the pack
  such that further calls can be via inline }
{
 *  AESetObjectCallbacks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AESetObjectCallbacks(myCompareProc: OSLCompareUPP; myCountProc: OSLCountUPP; myDisposeTokenProc: OSLDisposeTokenUPP; myGetMarkTokenProc: OSLGetMarkTokenUPP; myMarkProc: OSLMarkUPP; myAdjustMarksProc: OSLAdjustMarksUPP; myGetErrDescProcPtr: OSLGetErrDescUPP): OSErr; external name '_AESetObjectCallbacks';
{
 *  AEResolve()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEResolve(const (*var*) objectSpecifier: AEDesc; callbackFlags: SInt16; var theToken: AEDesc): OSErr; external name '_AEResolve';
{
 *  AEInstallObjectAccessor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEInstallObjectAccessor(desiredClass: DescType; containerType: DescType; theAccessor: OSLAccessorUPP; accessorRefcon: SInt32; isSysHandler: boolean): OSErr; external name '_AEInstallObjectAccessor';
{
 *  AERemoveObjectAccessor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AERemoveObjectAccessor(desiredClass: DescType; containerType: DescType; theAccessor: OSLAccessorUPP; isSysHandler: boolean): OSErr; external name '_AERemoveObjectAccessor';
{
 *  AEGetObjectAccessor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetObjectAccessor(desiredClass: DescType; containerType: DescType; var accessor: OSLAccessorUPP; var accessorRefcon: SInt32; isSysHandler: boolean): OSErr; external name '_AEGetObjectAccessor';
{
 *  AEDisposeToken()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEDisposeToken(var theToken: AEDesc): OSErr; external name '_AEDisposeToken';
{
 *  AECallObjectAccessor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ObjectSupportLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECallObjectAccessor(desiredClass: DescType; const (*var*) containerToken: AEDesc; containerClass: DescType; keyForm: DescType; const (*var*) keyData: AEDesc; var token: AEDesc): OSErr; external name '_AECallObjectAccessor';
{$ALIGN MAC68K}


end.

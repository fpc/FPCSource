{
     File:       CMTypes.p
 
     Contains:   xxx put contents here xxx
 
     Version:    Technology: ColorSync 3
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://developer.apple.com/bugreporter/
 
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

unit CMTypes;
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
uses MacTypes,MixedMode;


{ Standard type for ColorSync and other system error codes }

{$ALIGN MAC68K}


type
	CMError								= SInt32;
	{	 Abstract data type for memory-based Profile 	}
	CMProfileRef    = ^SInt32; { an opaque 32-bit type }
	CMProfileRefPtr = ^CMProfileRef;  { when a var xx:CMProfileRef parameter can be nil, it is changed to xx: CMProfileRefPtr }
	{	 Abstract data type for Profile search result 	}
	CMProfileSearchRef    = ^SInt32; { an opaque 32-bit type }
	CMProfileSearchRefPtr = ^CMProfileSearchRef;  { when a var xx:CMProfileSearchRef parameter can be nil, it is changed to xx: CMProfileSearchRefPtr }
	{	 Abstract data type for BeginMatching(É) reference 	}
	CMMatchRef    = ^SInt32; { an opaque 32-bit type }
	CMMatchRefPtr = ^CMMatchRef;  { when a var xx:CMMatchRef parameter can be nil, it is changed to xx: CMMatchRefPtr }
	{	 Abstract data type for ColorWorld reference 	}
	CMWorldRef    = ^SInt32; { an opaque 32-bit type }
	CMWorldRefPtr = ^CMWorldRef;  { when a var xx:CMWorldRef parameter can be nil, it is changed to xx: CMWorldRefPtr }
	{	 Data type for ColorSync DisplayID reference 	}
	{	 On 8 & 9 this is a AVIDType 	}
	{	 On X this is a CGSDisplayID 	}
	CMDisplayIDType						= UInt32;

	{	 Caller-supplied flatten function 	}
{$ifc TYPED_FUNCTION_POINTERS}
	CMFlattenProcPtr = function(command: SInt32; var size: SInt32; data: UnivPtr; refCon: UnivPtr): OSErr;
{$elsec}
	CMFlattenProcPtr = ProcPtr;
{$endc}

	{	 Caller-supplied progress function for Bitmap & PixMap matching routines 	}
{$ifc TYPED_FUNCTION_POINTERS}
	CMBitmapCallBackProcPtr = function(progress: SInt32; refCon: UnivPtr): boolean;
{$elsec}
	CMBitmapCallBackProcPtr = ProcPtr;
{$endc}

	{	 Caller-supplied progress function for NCMMConcatInit & NCMMNewLinkProfile routines 	}
{$ifc TYPED_FUNCTION_POINTERS}
	CMConcatCallBackProcPtr = function(progress: SInt32; refCon: UnivPtr): boolean;
{$elsec}
	CMConcatCallBackProcPtr = ProcPtr;
{$endc}

	{	 Caller-supplied filter function for Profile search 	}
{$ifc TYPED_FUNCTION_POINTERS}
	CMProfileFilterProcPtr = function(prof: CMProfileRef; refCon: UnivPtr): boolean;
{$elsec}
	CMProfileFilterProcPtr = ProcPtr;
{$endc}

	{	 Caller-supplied function for profile access 	}
{$ifc TYPED_FUNCTION_POINTERS}
	CMProfileAccessProcPtr = function(command: SInt32; offset: SInt32; var size: SInt32; data: UnivPtr; refCon: UnivPtr): OSErr;
{$elsec}
	CMProfileAccessProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	CMFlattenUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CMFlattenUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CMBitmapCallBackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CMBitmapCallBackUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CMConcatCallBackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CMConcatCallBackUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CMProfileFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CMProfileFilterUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CMProfileAccessUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CMProfileAccessUPP = UniversalProcPtr;
{$endc}	

const
	uppCMFlattenProcInfo = $00003FE0;
	uppCMBitmapCallBackProcInfo = $000003D0;
	uppCMConcatCallBackProcInfo = $000003D0;
	uppCMProfileFilterProcInfo = $000003D0;
	uppCMProfileAccessProcInfo = $0000FFE0;
	{
	 *  NewCMFlattenUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in 3.0 and later
	 	}
function NewCMFlattenUPP(userRoutine: CMFlattenProcPtr): CMFlattenUPP; external name '_NewCMFlattenUPP'; { old name was NewCMFlattenProc }
{
 *  NewCMBitmapCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function NewCMBitmapCallBackUPP(userRoutine: CMBitmapCallBackProcPtr): CMBitmapCallBackUPP; external name '_NewCMBitmapCallBackUPP'; { old name was NewCMBitmapCallBackProc }
{
 *  NewCMConcatCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function NewCMConcatCallBackUPP(userRoutine: CMConcatCallBackProcPtr): CMConcatCallBackUPP; external name '_NewCMConcatCallBackUPP'; { old name was NewCMConcatCallBackProc }
{
 *  NewCMProfileFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function NewCMProfileFilterUPP(userRoutine: CMProfileFilterProcPtr): CMProfileFilterUPP; external name '_NewCMProfileFilterUPP'; { old name was NewCMProfileFilterProc }
{
 *  NewCMProfileAccessUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function NewCMProfileAccessUPP(userRoutine: CMProfileAccessProcPtr): CMProfileAccessUPP; external name '_NewCMProfileAccessUPP'; { old name was NewCMProfileAccessProc }
{
 *  DisposeCMFlattenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
procedure DisposeCMFlattenUPP(userUPP: CMFlattenUPP); external name '_DisposeCMFlattenUPP';
{
 *  DisposeCMBitmapCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
procedure DisposeCMBitmapCallBackUPP(userUPP: CMBitmapCallBackUPP); external name '_DisposeCMBitmapCallBackUPP';
{
 *  DisposeCMConcatCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
procedure DisposeCMConcatCallBackUPP(userUPP: CMConcatCallBackUPP); external name '_DisposeCMConcatCallBackUPP';
{
 *  DisposeCMProfileFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
procedure DisposeCMProfileFilterUPP(userUPP: CMProfileFilterUPP); external name '_DisposeCMProfileFilterUPP';
{
 *  DisposeCMProfileAccessUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
procedure DisposeCMProfileAccessUPP(userUPP: CMProfileAccessUPP); external name '_DisposeCMProfileAccessUPP';
{
 *  InvokeCMFlattenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function InvokeCMFlattenUPP(command: SInt32; var size: SInt32; data: UnivPtr; refCon: UnivPtr; userRoutine: CMFlattenUPP): OSErr; external name '_InvokeCMFlattenUPP'; { old name was CallCMFlattenProc }
{
 *  InvokeCMBitmapCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function InvokeCMBitmapCallBackUPP(progress: SInt32; refCon: UnivPtr; userRoutine: CMBitmapCallBackUPP): boolean; external name '_InvokeCMBitmapCallBackUPP'; { old name was CallCMBitmapCallBackProc }
{
 *  InvokeCMConcatCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function InvokeCMConcatCallBackUPP(progress: SInt32; refCon: UnivPtr; userRoutine: CMConcatCallBackUPP): boolean; external name '_InvokeCMConcatCallBackUPP'; { old name was CallCMConcatCallBackProc }
{
 *  InvokeCMProfileFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function InvokeCMProfileFilterUPP(prof: CMProfileRef; refCon: UnivPtr; userRoutine: CMProfileFilterUPP): boolean; external name '_InvokeCMProfileFilterUPP'; { old name was CallCMProfileFilterProc }
{
 *  InvokeCMProfileAccessUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function InvokeCMProfileAccessUPP(command: SInt32; offset: SInt32; var size: SInt32; data: UnivPtr; refCon: UnivPtr; userRoutine: CMProfileAccessUPP): OSErr; external name '_InvokeCMProfileAccessUPP'; { old name was CallCMProfileAccessProc }
{$ALIGN MAC68K}


end.

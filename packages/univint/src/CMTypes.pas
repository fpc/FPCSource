{
     File:       ColorSync/CMTypes.h
 
     Contains:   ColorSync types
 
     Version:    ColorSync-174.1~229
 
     Copyright:  © 2000-2006 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2007 }

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

unit CMTypes;
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
	CMChromaticAdaptation = UInt32;

const
	cmUseDefaultChromaticAdaptation = 0;
	cmLinearChromaticAdaptation     = 1;
	cmVonKriesChromaticAdaptation   = 2;
	cmBradfordChromaticAdaptation   = 3;

type
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
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCMFlattenUPP(userRoutine: CMFlattenProcPtr): CMFlattenUPP; external name '_NewCMFlattenUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewCMBitmapCallBackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCMBitmapCallBackUPP(userRoutine: CMBitmapCallBackProcPtr): CMBitmapCallBackUPP; external name '_NewCMBitmapCallBackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewCMConcatCallBackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCMConcatCallBackUPP(userRoutine: CMConcatCallBackProcPtr): CMConcatCallBackUPP; external name '_NewCMConcatCallBackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewCMProfileFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCMProfileFilterUPP(userRoutine: CMProfileFilterProcPtr): CMProfileFilterUPP; external name '_NewCMProfileFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewCMProfileAccessUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCMProfileAccessUPP(userRoutine: CMProfileAccessProcPtr): CMProfileAccessUPP; external name '_NewCMProfileAccessUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeCMFlattenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCMFlattenUPP(userUPP: CMFlattenUPP); external name '_DisposeCMFlattenUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeCMBitmapCallBackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCMBitmapCallBackUPP(userUPP: CMBitmapCallBackUPP); external name '_DisposeCMBitmapCallBackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeCMConcatCallBackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCMConcatCallBackUPP(userUPP: CMConcatCallBackUPP); external name '_DisposeCMConcatCallBackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeCMProfileFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCMProfileFilterUPP(userUPP: CMProfileFilterUPP); external name '_DisposeCMProfileFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeCMProfileAccessUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCMProfileAccessUPP(userUPP: CMProfileAccessUPP); external name '_DisposeCMProfileAccessUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeCMFlattenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCMFlattenUPP(command: SInt32; var size: SInt32; data: UnivPtr; refCon: UnivPtr; userRoutine: CMFlattenUPP): OSErr; external name '_InvokeCMFlattenUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeCMBitmapCallBackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCMBitmapCallBackUPP(progress: SInt32; refCon: UnivPtr; userRoutine: CMBitmapCallBackUPP): boolean; external name '_InvokeCMBitmapCallBackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeCMConcatCallBackUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCMConcatCallBackUPP(progress: SInt32; refCon: UnivPtr; userRoutine: CMConcatCallBackUPP): boolean; external name '_InvokeCMConcatCallBackUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeCMProfileFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCMProfileFilterUPP(prof: CMProfileRef; refCon: UnivPtr; userRoutine: CMProfileFilterUPP): boolean; external name '_InvokeCMProfileFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeCMProfileAccessUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCMProfileAccessUPP(command: SInt32; offset: SInt32; var size: SInt32; data: UnivPtr; refCon: UnivPtr; userRoutine: CMProfileAccessUPP): OSErr; external name '_InvokeCMProfileAccessUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$ALIGN MAC68K}


end.

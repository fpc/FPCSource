{
     File:       CMMComponent.p
 
     Contains:   ColorSync CMM Component API
 
     Version:    Technology: ColorSync 2.6
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1994-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit CMMComponent;
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
uses MacTypes,Files,CMTypes,CMICCProfile,CMApplication,Quickdraw,Components;

{$ALIGN MAC68K}

{ Component-based CMM interface version }

const
	CMMInterfaceVersion			= 1;


	{	 Component-based CMM function selectors 	}
																{  Required  }
	kCMMOpen					= -1;							{  kComponentOpenSelect, }
	kCMMClose					= -2;							{  kComponentCloseSelect, }
	kCMMGetInfo					= -4;							{  kComponentVersionSelect }
	kNCMMInit					= 6;
	kCMMMatchColors				= 1;
	kCMMCheckColors				= 2;							{  }
																{  }
																{  Optional  }
	kCMMValidateProfile			= 8;
	kCMMMatchBitmap				= 9;
	kCMMCheckBitmap				= 10;
	kCMMConcatenateProfiles		= 5;
	kCMMConcatInit				= 7;
	kCMMNewLinkProfile			= 16;
	kNCMMConcatInit				= 18;
	kNCMMNewLinkProfile			= 19;
	kCMMGetPS2ColorSpace		= 11;
	kCMMGetPS2ColorRenderingIntent = 12;
	kCMMGetPS2ColorRendering	= 13;
	kCMMGetPS2ColorRenderingVMSize = 17;						{  }
																{  }
																{  obsolete with ColorSync 2.5  }
	kCMMFlattenProfile			= 14;
	kCMMUnflattenProfile		= 15;							{  }
																{  }
																{  obsolete with ColorSync 2.6  }
	kCMMInit					= 0;
	kCMMGetNamedColorInfo		= 70;
	kCMMGetNamedColorValue		= 71;
	kCMMGetIndNamedColorValue	= 72;
	kCMMGetNamedColorIndex		= 73;
	kCMMGetNamedColorName		= 74;							{  }
																{  }
																{  obsolete with ColorSync 3.0  }
	kCMMMatchPixMap				= 3;
	kCMMCheckPixMap				= 4;


{$ifc TARGET_API_MAC_OS8}

type
	CMMComponentInst					= ComponentInstance;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NCMMInit()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NCMMInit(cmm: CMMComponentInst; srcProfile: CMProfileRef; dstProfile: CMProfileRef): CMError; external name '_NCMMInit';
{
 *  CMMInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMInit(cmm: CMMComponentInst; srcProfile: CMProfileHandle; dstProfile: CMProfileHandle): CMError; external name '_CMMInit';
{
 *  CMMMatchColors()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMMatchColors(cmm: CMMComponentInst; var colors: CMColor; count: UInt32): CMError; external name '_CMMMatchColors';
{
 *  CMMCheckColors()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMCheckColors(cmm: CMMComponentInst; var colors: CMColor; count: UInt32; var result: UInt32): CMError; external name '_CMMCheckColors';
{
 *  CMMValidateProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMValidateProfile(cmm: CMMComponentInst; prof: CMProfileRef; var valid: boolean): CMError; external name '_CMMValidateProfile';
{
 *  CMMFlattenProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMFlattenProfile(cmm: CMMComponentInst; prof: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr): CMError; external name '_CMMFlattenProfile';
{
 *  CMMUnflattenProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMUnflattenProfile(cmm: CMMComponentInst; var resultFileSpec: FSSpec; proc: CMFlattenUPP; refCon: UnivPtr): CMError; external name '_CMMUnflattenProfile';
{
 *  CMMMatchBitmap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMMatchBitmap(cmm: CMMComponentInst; var bitmap: CMBitmap; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr; var matchedBitmap: CMBitmap): CMError; external name '_CMMMatchBitmap';
{
 *  CMMCheckBitmap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMCheckBitmap(cmm: CMMComponentInst; const (*var*) bitmap: CMBitmap; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr; var resultBitmap: CMBitmap): CMError; external name '_CMMCheckBitmap';
{
 *  CMMMatchPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMMatchPixMap(cmm: CMMComponentInst; var pixMap_: PixMap; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr): CMError; external name '_CMMMatchPixMap';
{
 *  CMMCheckPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMCheckPixMap(cmm: CMMComponentInst; const (*var*) pixMap_: PixMap; progressProc: CMBitmapCallBackUPP; var bitMap_: BitMap; refCon: UnivPtr): CMError; external name '_CMMCheckPixMap';
{
 *  CMMConcatInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMConcatInit(cmm: CMMComponentInst; var profileSet: CMConcatProfileSet): CMError; external name '_CMMConcatInit';
{
 *  NCMMConcatInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NCMMConcatInit(cmm: CMMComponentInst; var profileSet: NCMConcatProfileSet; proc: CMConcatCallBackUPP; refCon: UnivPtr): CMError; external name '_NCMMConcatInit';
{
 *  CMMNewLinkProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMNewLinkProfile(cmm: CMMComponentInst; var prof: CMProfileRef; const (*var*) targetLocation: CMProfileLocation; var profileSet: CMConcatProfileSet): CMError; external name '_CMMNewLinkProfile';
{
 *  NCMMNewLinkProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NCMMNewLinkProfile(cmm: CMMComponentInst; prof: CMProfileRef; var profileSet: NCMConcatProfileSet; proc: CMConcatCallBackUPP; refCon: UnivPtr): CMError; external name '_NCMMNewLinkProfile';
{
 *  CMMGetPS2ColorSpace()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetPS2ColorSpace(cmm: CMMComponentInst; srcProf: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr): CMError; external name '_CMMGetPS2ColorSpace';
{
 *  CMMGetPS2ColorRenderingIntent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetPS2ColorRenderingIntent(cmm: CMMComponentInst; srcProf: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr): CMError; external name '_CMMGetPS2ColorRenderingIntent';
{
 *  CMMGetPS2ColorRendering()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetPS2ColorRendering(cmm: CMMComponentInst; srcProf: CMProfileRef; dstProf: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr): CMError; external name '_CMMGetPS2ColorRendering';
{
 *  CMMGetPS2ColorRenderingVMSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetPS2ColorRenderingVMSize(cmm: CMMComponentInst; srcProf: CMProfileRef; dstProf: CMProfileRef; var vmSize: UInt32): CMError; external name '_CMMGetPS2ColorRenderingVMSize';
{
 *  CMMConcatenateProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMConcatenateProfiles(cmm: CMMComponentInst; thru: CMProfileHandle; dst: CMProfileHandle; var newDst: CMProfileHandle): CMError; external name '_CMMConcatenateProfiles';
{
 *  CMMGetNamedColorInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetNamedColorInfo(cmm: CMMComponentInst; srcProf: CMProfileRef; var deviceChannels: UInt32; var deviceColorSpace: OSType; var PCSColorSpace: OSType; var count: UInt32; prefix: StringPtr; suffix: StringPtr): CMError; external name '_CMMGetNamedColorInfo';
{
 *  CMMGetNamedColorValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetNamedColorValue(cmm: CMMComponentInst; prof: CMProfileRef; name: StringPtr; var deviceColor: CMColor; var PCSColor: CMColor): CMError; external name '_CMMGetNamedColorValue';
{
 *  CMMGetIndNamedColorValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetIndNamedColorValue(cmm: CMMComponentInst; prof: CMProfileRef; index: UInt32; var deviceColor: CMColor; var PCSColor: CMColor): CMError; external name '_CMMGetIndNamedColorValue';
{
 *  CMMGetNamedColorIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetNamedColorIndex(cmm: CMMComponentInst; prof: CMProfileRef; name: StringPtr; var index: UInt32): CMError; external name '_CMMGetNamedColorIndex';
{
 *  CMMGetNamedColorName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMMGetNamedColorName(cmm: CMMComponentInst; prof: CMProfileRef; index: UInt32; name: StringPtr): CMError; external name '_CMMGetNamedColorName';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_API_MAC_OS8}

{$ALIGN MAC68K}


end.

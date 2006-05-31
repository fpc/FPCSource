{
     File:       ColorPicker.p
 
     Contains:   Color Picker package Interfaces.
 
     Version:    Technology: System 7.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1987-2002 by Apple Computer, Inc., all rights reserved
 
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

unit ColorPicker;
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
uses MacTypes,CMTypes,CMICCProfile,MixedMode,Quickdraw,CMApplication,Events;


{$ALIGN MAC68K}


const
																{ Maximum small fract value, as long }
	kMaximumSmallFract			= $0000FFFF;

	kDefaultColorPickerWidth	= 383;
	kDefaultColorPickerHeight	= 238;


type
	DialogPlacementSpec 		= SInt16;
const
	kAtSpecifiedOrigin			= 0;
	kDeepestColorScreen			= 1;
	kCenterOnMainScreen			= 2;


	{	 These are for the flags field in the structs below (for example ColorPickerInfo). 	}
	kColorPickerDialogIsMoveable = 1;
	kColorPickerDialogIsModal	= 2;
	kColorPickerCanModifyPalette = 4;
	kColorPickerCanAnimatePalette = 8;
	kColorPickerAppIsColorSyncAware = 16;
	kColorPickerInSystemDialog	= 32;
	kColorPickerInApplicationDialog = 64;
	kColorPickerInPickerDialog	= 128;
	kColorPickerDetachedFromChoices = 256;
	kColorPickerCallColorProcLive = 512;


{$ifc OLDROUTINENAMES}
																{ Maximum small fract value, as long }
	MaxSmallFract				= $0000FFFF;

	kDefaultWidth				= 383;
	kDefaultHeight				= 238;

	{	 These are for the flags field in the structs below (for example ColorPickerInfo). 	}
	DialogIsMoveable			= 1;
	DialogIsModal				= 2;
	CanModifyPalette			= 4;
	CanAnimatePalette			= 8;
	AppIsColorSyncAware			= 16;
	InSystemDialog				= 32;
	InApplicationDialog			= 64;
	InPickerDialog				= 128;
	DetachedFromChoices			= 256;
	CallColorProcLive			= 512;

{$endc}  {OLDROUTINENAMES}

	{	 A SmallFract value is just the fractional part of a Fixed number,
	which is the low order word.  SmallFracts are used to save room,
	and to be compatible with Quickdraw's RGBColor.  They can be
	assigned directly to and from INTEGERs. 	}
	{	 Unsigned fraction between 0 and 1 	}

type
	SmallFract							= UInt16;
	{	 For developmental simplicity in switching between the HLS and HSV
	models, HLS is reordered into HSL. Thus both models start with
	hue and saturation values; value/lightness/brightness is last. 	}

	HSVColorPtr = ^HSVColor;
	HSVColor = record
		hue:					SmallFract;								{ Fraction of circle, red at 0 }
		saturation:				SmallFract;								{ 0-1, 0 for gray, 1 for pure color }
		value:					SmallFract;								{ 0-1, 0 for black, 1 for max intensity }
	end;

	HSLColorPtr = ^HSLColor;
	HSLColor = record
		hue:					SmallFract;								{ Fraction of circle, red at 0 }
		saturation:				SmallFract;								{ 0-1, 0 for gray, 1 for pure color }
		lightness:				SmallFract;								{ 0-1, 0 for black, 1 for white }
	end;

	CMYColorPtr = ^CMYColor;
	CMYColor = record
		cyan:					SmallFract;
		magenta:				SmallFract;
		yellow:					SmallFract;
	end;

	PMColorPtr = ^PMColor;
	PMColor = record
		profile:				CMProfileHandle;
		color:					CMColor;
	end;

	NPMColorPtr = ^NPMColor;
	NPMColor = record
		profile:				CMProfileRef;
		color:					CMColor;
	end;

	Picker    = ^SInt32; { an opaque 32-bit type }
	PickerPtr = ^Picker;  { when a var xx:Picker parameter can be nil, it is changed to xx: PickerPtr }
	PickerMenuItemInfoPtr = ^PickerMenuItemInfo;
	PickerMenuItemInfo = record
		editMenuID:				SInt16;
		cutItem:				SInt16;
		copyItem:				SInt16;
		pasteItem:				SInt16;
		clearItem:				SInt16;
		undoItem:				SInt16;
	end;

	{	 Structs related to deprecated API's have been pulled from this file. 	}
	{	 Those structs necessary for developers writing their own color pickers... 	}
	{	 have been moved to ColorPickerComponents.h. 	}

{$ifc TYPED_FUNCTION_POINTERS}
	ColorChangedProcPtr = procedure(userData: SInt32; var newColor: PMColor);
{$elsec}
	ColorChangedProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	NColorChangedProcPtr = procedure(userData: SInt32; var newColor: NPMColor);
{$elsec}
	NColorChangedProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	UserEventProcPtr = function(var event: EventRecord): boolean;
{$elsec}
	UserEventProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	ColorChangedUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ColorChangedUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	NColorChangedUPP = ^SInt32; { an opaque UPP }
{$elsec}
	NColorChangedUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	UserEventUPP = ^SInt32; { an opaque UPP }
{$elsec}
	UserEventUPP = UniversalProcPtr;
{$endc}	
	ColorPickerInfoPtr = ^ColorPickerInfo;
	ColorPickerInfo = record
		theColor:				PMColor;
		dstProfile:				CMProfileHandle;
		flags:					UInt32;
		placeWhere:				DialogPlacementSpec;
		dialogOrigin:			Point;
		pickerType:				OSType;
		eventProc:				UserEventUPP;
		colorProc:				ColorChangedUPP;
		colorProcData:			UInt32;
		prompt:					Str255;
		mInfo:					PickerMenuItemInfo;
		newColorChosen:			boolean;
		filler:					SInt8;
	end;

	NColorPickerInfoPtr = ^NColorPickerInfo;
	NColorPickerInfo = record
		theColor:				NPMColor;
		dstProfile:				CMProfileRef;
		flags:					UInt32;
		placeWhere:				DialogPlacementSpec;
		dialogOrigin:			Point;
		pickerType:				OSType;
		eventProc:				UserEventUPP;
		colorProc:				NColorChangedUPP;
		colorProcData:			UInt32;
		prompt:					Str255;
		mInfo:					PickerMenuItemInfo;
		newColorChosen:			boolean;
		reserved:				SInt8;									{ Must be 0 }
	end;


	{   Below are the color conversion routines. }
	{
	 *  Fix2SmallFract()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         not available
	 	}
function Fix2SmallFract(f: Fixed): SmallFract; external name '_Fix2SmallFract';
{
 *  SmallFract2Fix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function SmallFract2Fix(s: SmallFract): Fixed; external name '_SmallFract2Fix';
{
 *  CMY2RGB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure CMY2RGB(const (*var*) cColor: CMYColor; var rColor: RGBColor); external name '_CMY2RGB';
{
 *  RGB2CMY()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure RGB2CMY(const (*var*) rColor: RGBColor; var cColor: CMYColor); external name '_RGB2CMY';
{
 *  HSL2RGB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure HSL2RGB(const (*var*) hColor: HSLColor; var rColor: RGBColor); external name '_HSL2RGB';
{
 *  RGB2HSL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure RGB2HSL(const (*var*) rColor: RGBColor; var hColor: HSLColor); external name '_RGB2HSL';
{
 *  HSV2RGB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure HSV2RGB(const (*var*) hColor: HSVColor; var rColor: RGBColor); external name '_HSV2RGB';
{
 *  RGB2HSV()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure RGB2HSV(const (*var*) rColor: RGBColor; var hColor: HSVColor); external name '_RGB2HSV';
{   GetColor() works with or without the Color Picker extension. }
{
 *  GetColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function GetColor(where: Point; const (*var*) prompt: Str255; const (*var*) inColor: RGBColor; var outColor: RGBColor): boolean; external name '_GetColor';
{   PickColor() requires the Color Picker extension (version 2.0 or greater). }
{
 *  PickColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorPickerLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function PickColor(var theColorInfo: ColorPickerInfo): OSErr; external name '_PickColor';
{   NPickColor() requires the Color Picker extension (version 2.1 or greater). }
{
 *  NPickColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorPickerLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NPickColor(var theColorInfo: NColorPickerInfo): OSErr; external name '_NPickColor';
{ A suite of mid-level API calls have been deprecated.  Likely you never...  }
{ used them anyway.  They were removed from this file and should not be... }
{ used in the future as they are not gauranteed to be supported. }

const
	uppColorChangedProcInfo = $000003C0;
	uppNColorChangedProcInfo = $000003C0;
	uppUserEventProcInfo = $000000D0;
	{
	 *  NewColorChangedUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         not available
	 	}
function NewColorChangedUPP(userRoutine: ColorChangedProcPtr): ColorChangedUPP; external name '_NewColorChangedUPP'; { old name was NewColorChangedProc }
{
 *  NewNColorChangedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NewNColorChangedUPP(userRoutine: NColorChangedProcPtr): NColorChangedUPP; external name '_NewNColorChangedUPP'; { old name was NewNColorChangedProc }
{
 *  NewUserEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NewUserEventUPP(userRoutine: UserEventProcPtr): UserEventUPP; external name '_NewUserEventUPP'; { old name was NewUserEventProc }
{
 *  DisposeColorChangedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure DisposeColorChangedUPP(userUPP: ColorChangedUPP); external name '_DisposeColorChangedUPP';
{
 *  DisposeNColorChangedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure DisposeNColorChangedUPP(userUPP: NColorChangedUPP); external name '_DisposeNColorChangedUPP';
{
 *  DisposeUserEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure DisposeUserEventUPP(userUPP: UserEventUPP); external name '_DisposeUserEventUPP';
{
 *  InvokeColorChangedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure InvokeColorChangedUPP(userData: SInt32; var newColor: PMColor; userRoutine: ColorChangedUPP); external name '_InvokeColorChangedUPP'; { old name was CallColorChangedProc }
{
 *  InvokeNColorChangedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure InvokeNColorChangedUPP(userData: SInt32; var newColor: NPMColor; userRoutine: NColorChangedUPP); external name '_InvokeNColorChangedUPP'; { old name was CallNColorChangedProc }
{
 *  InvokeUserEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function InvokeUserEventUPP(var event: EventRecord; userRoutine: UserEventUPP): boolean; external name '_InvokeUserEventUPP'; { old name was CallUserEventProc }
{$ALIGN MAC68K}


end.

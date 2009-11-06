{
     File:       CommonPanels/ColorPicker.h
 
     Contains:   Color Picker package Interfaces.
 
     Version:    CommonPanels-91~177
 
     Copyright:  © 1987-2008 by Apple Computer, Inc., all rights reserved
 
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

unit ColorPicker;
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
	{$setc TARGET_CPU_PPC := TFALSE}
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
uses MacTypes,ColorSyncDeprecated,MixedMode,QuickdrawTypes,Events;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

{$ifc not TARGET_CPU_64}
const
{Maximum small fract value, as long}
	kMaximumSmallFract = $0000FFFF;

{$endc} {not TARGET_CPU_64}

{ These are legacy constants. The Color Picker on OS X uses the Cocoa NSColorPanel. }
const
	kDefaultColorPickerWidth = 383;
	kDefaultColorPickerHeight = 238;

type
	DialogPlacementSpec = SInt16;
const
	kAtSpecifiedOrigin = 0;
	kDeepestColorScreen = 1;
	kCenterOnMainScreen = 2;

{ Since OS X uses the Cocoa NSColorPanel, the flags below are no longer used. }
const
	kColorPickerDialogIsMoveable = 1;
	kColorPickerDialogIsModal = 2;
	kColorPickerCanModifyPalette = 4;
	kColorPickerCanAnimatePalette = 8;
	kColorPickerAppIsColorSyncAware = 16;
	kColorPickerInSystemDialog = 32;
	kColorPickerInApplicationDialog = 64;
	kColorPickerInPickerDialog = 128;
	kColorPickerDetachedFromChoices = 256;
	kColorPickerCallColorProcLive = 512;

{$ifc OLDROUTINENAMES}
{$ifc not TARGET_CPU_64}
const
{Maximum small fract value, as long}
	MaxSmallFract = $0000FFFF;

{$endc} {not TARGET_CPU_64}

const
	kDefaultWidth = 383;
	kDefaultHeight = 238;

{ Since OS X uses the Cocoa NSColorPanel, the flags below are no longer used. }
const
	DialogIsMoveable = 1;
	DialogIsModal = 2;
	CanModifyPalette = 4;
	CanAnimatePalette = 8;
	AppIsColorSyncAware = 16;
	InSystemDialog = 32;
	InApplicationDialog = 64;
	InPickerDialog = 128;
	DetachedFromChoices = 256;
	CallColorProcLive = 512;

{$endc} {OLDROUTINENAMES}

{$ifc not TARGET_CPU_64}
{ A SmallFract value is just the fractional part of a Fixed number,
which is the low order word.  They can be
assigned directly to and from INTEGERs. }
{ Unsigned fraction between 0 and 1 }
type
	SmallFract = UInt16;
	HSVColorPtr = ^HSVColor;
	HSVColor = record
		hue: SmallFract;                    { Fraction of circle, red at 0 }
		saturation: SmallFract;             { 0-1, 0 for gray, 1 for pure color }
		value: SmallFract;                  { 0-1, 0 for black, 1 for max intensity }
	end;
type
	HSLColorPtr = ^HSLColor;
	HSLColor = record
		hue: SmallFract;                    { Fraction of circle, red at 0 }
		saturation: SmallFract;             { 0-1, 0 for gray, 1 for pure color }
		lightness: SmallFract;              { 0-1, 0 for black, 1 for white }
	end;
type
	CMYColorPtr = ^CMYColor;
	CMYColor = record
		cyan: SmallFract;
		magenta: SmallFract;
		yellow: SmallFract;
	end;
type
	PMColor = record
		profile: CMProfileHandle;
		color: CMColor;
	end;
	PMColorPtr = ^PMColor;
{$endc} {not TARGET_CPU_64}

type
	NPMColor = record
		profile: CMProfileRef;
		color: CMColor;
	end;
	NPMColorPtr = ^NPMColor;
type
	Picker = ^SInt32; { an opaque type }
	PickerPtr = ^Picker;  { when a var xx:Picker parameter can be nil, it is changed to xx: PickerPtr }
{ Since OS X uses the Cocoa NSColorPanel, the struct below is no longer used. }
type
	PickerMenuItemInfoPtr = ^PickerMenuItemInfo;
	PickerMenuItemInfo = record
		editMenuID: SInt16;
		cutItem: SInt16;
		copyItem: SInt16;
		pasteItem: SInt16;
		clearItem: SInt16;
		undoItem: SInt16;
	end;
{ The following proc ptr is the only supported way to communicate with the Cocoa NSColorPanel on OS X. }
type
	NColorChangedProcPtr = procedure( userData: SRefCon; var newColor: NPMColor );
	NColorChangedUPP = NColorChangedProcPtr;
{
 *  NewNColorChangedUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewNColorChangedUPP( userRoutine: NColorChangedProcPtr ): NColorChangedUPP; external name '_NewNColorChangedUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeNColorChangedUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeNColorChangedUPP( userUPP: NColorChangedUPP ); external name '_DisposeNColorChangedUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeNColorChangedUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeNColorChangedUPP( userData: SRefCon; var newColor: NPMColor; userUPP: NColorChangedUPP ); external name '_InvokeNColorChangedUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$ifc TARGET_CPU_64}

type
	ColorChangedUPP = UnivPtr;
	UserEventUPP = UnivPtr;
{$elsec}
type
	ColorChangedProcPtr = procedure( userData: SInt32; var newColor: PMColor );
	UserEventProcPtr = function( var event: EventRecord ): Boolean;
	ColorChangedUPP = ColorChangedProcPtr;
	UserEventUPP = UserEventProcPtr;
{
 *  NewColorChangedUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewColorChangedUPP( userRoutine: ColorChangedProcPtr ): ColorChangedUPP; external name '_NewColorChangedUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewUserEventUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewUserEventUPP( userRoutine: UserEventProcPtr ): UserEventUPP; external name '_NewUserEventUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeColorChangedUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeColorChangedUPP( userUPP: ColorChangedUPP ); external name '_DisposeColorChangedUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeUserEventUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeUserEventUPP( userUPP: UserEventUPP ); external name '_DisposeUserEventUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeColorChangedUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeColorChangedUPP( userData: SInt32; var newColor: PMColor; userUPP: ColorChangedUPP ); external name '_InvokeColorChangedUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeUserEventUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeUserEventUPP( var event: EventRecord; userUPP: UserEventUPP ): Boolean; external name '_InvokeUserEventUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$endc} {TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
type
	ColorPickerInfo = record
		theColor: PMColor;
		dstProfile: CMProfileHandle;
		flags: UInt32;
		placeWhere: DialogPlacementSpec;
		dialogOrigin: Point;
		pickerType: OSType;
		eventProc: UserEventUPP;
		colorProc: ColorChangedUPP;
		colorProcData: UInt32;
		prompt: Str255;
		mInfo: PickerMenuItemInfo;
		newColorChosen: Boolean;
		filler: SInt8;
	end;
{$endc} {not TARGET_CPU_64}

type
	NColorPickerInfo = record
		theColor: NPMColor;
		dstProfile: CMProfileRef;             { Currently ignored }
		flags: UInt32;                  { Currently ignored }
		placeWhere: DialogPlacementSpec;            { Currently ignored }
		dialogOrigin: Point;           { Currently ignored }
		pickerType: OSType;             { Currently ignored }
{$ifc not TARGET_CPU_64}

		eventProc: UserEventUPP;              { Ignored }
{$endc} {not TARGET_CPU_64}

		colorProc: NColorChangedUPP;
		colorProcData: URefCon;
		prompt: Str255;                 { Currently ignored }
		mInfo: PickerMenuItemInfo;                  { Ignored }
		newColorChosen: Boolean;
		reserved: UInt8;               { Must be 0 }
	end;

{$ifc not TARGET_CPU_64}
{
 *  Fix2SmallFract()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function Fix2SmallFract( f: Fixed ): SmallFract; external name '_Fix2SmallFract';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SmallFract2Fix()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SmallFract2Fix( s: SmallFract ): Fixed; external name '_SmallFract2Fix';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CMY2RGB()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure CMY2RGB( const (*var*) cColor: CMYColor; var rColor: RGBColor ); external name '_CMY2RGB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RGB2CMY()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure RGB2CMY( const (*var*) rColor: RGBColor; var cColor: CMYColor ); external name '_RGB2CMY';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HSL2RGB()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HSL2RGB( const (*var*) hColor: HSLColor; var rColor: RGBColor ); external name '_HSL2RGB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RGB2HSL()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure RGB2HSL( const (*var*) rColor: RGBColor; var hColor: HSLColor ); external name '_RGB2HSL';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HSV2RGB()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure HSV2RGB( const (*var*) hColor: HSVColor; var rColor: RGBColor ); external name '_HSV2RGB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  RGB2HSV()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure RGB2HSV( const (*var*) rColor: RGBColor; var hColor: HSVColor ); external name '_RGB2HSV';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$endc} {not TARGET_CPU_64}

{
 *  GetColor()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetColor( where: Point; const (*var*) prompt: Str255; const (*var*) inColor: RGBColor; var outColor: RGBColor ): Boolean; external name '_GetColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$ifc not TARGET_CPU_64}
{
 *  PickColor()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorPickerLib 2.0 and later
 }
function PickColor( var theColorInfo: ColorPickerInfo ): OSErr; external name '_PickColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$endc} {not TARGET_CPU_64}

{
 *  NPickColor()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorPickerLib 2.1 and later
 }
function NPickColor( var theColorInfo: NColorPickerInfo ): OSErr; external name '_NPickColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

{
     File:       Icons.p
 
     Contains:   Icon Utilities and Icon Services Interfaces.
 
     Version:    Technology: Mac OS 9.x
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1990-2002 by Apple Computer, Inc. All rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}

{    Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit Icons;
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
uses MacTypes,CFBase,CGGeometry,Quickdraw,Files,CodeFragments,CGContext;


{$ALIGN MAC68K}

{ The following are icons for which there are both icon suites and SICNs. }
{ Avoid using icon resources if possible. Use IconServices instead. }

const
	kGenericDocumentIconResource = -4000;
	kGenericStationeryIconResource = -3985;
	kGenericEditionFileIconResource = -3989;
	kGenericApplicationIconResource = -3996;
	kGenericDeskAccessoryIconResource = -3991;
	kGenericFolderIconResource	= -3999;
	kPrivateFolderIconResource	= -3994;
	kFloppyIconResource			= -3998;
	kTrashIconResource			= -3993;
	kGenericRAMDiskIconResource	= -3988;
	kGenericCDROMIconResource	= -3987;

	{	 The following are icons for which there are SICNs only. 	}
	{	 Avoid using icon resources if possible. Use IconServices instead. 	}
	kDesktopIconResource		= -3992;
	kOpenFolderIconResource		= -3997;
	kGenericHardDiskIconResource = -3995;
	kGenericFileServerIconResource = -3972;
	kGenericSuitcaseIconResource = -3970;
	kGenericMoverObjectIconResource = -3969;

	{	 The following are icons for which there are icon suites only. 	}
	{	 Avoid using icon resources if possible. Use IconServices instead. 	}
	kGenericPreferencesIconResource = -3971;
	kGenericQueryDocumentIconResource = -16506;
	kGenericExtensionIconResource = -16415;
	kSystemFolderIconResource	= -3983;
	kHelpIconResource			= -20271;
	kAppleMenuFolderIconResource = -3982;

	{	 Obsolete. Use named constants defined above. 	}
	genericDocumentIconResource	= -4000;
	genericStationeryIconResource = -3985;
	genericEditionFileIconResource = -3989;
	genericApplicationIconResource = -3996;
	genericDeskAccessoryIconResource = -3991;
	genericFolderIconResource	= -3999;
	privateFolderIconResource	= -3994;
	floppyIconResource			= -3998;
	trashIconResource			= -3993;
	genericRAMDiskIconResource	= -3988;
	genericCDROMIconResource	= -3987;
	desktopIconResource			= -3992;
	openFolderIconResource		= -3997;
	genericHardDiskIconResource	= -3995;
	genericFileServerIconResource = -3972;
	genericSuitcaseIconResource	= -3970;
	genericMoverObjectIconResource = -3969;
	genericPreferencesIconResource = -3971;
	genericQueryDocumentIconResource = -16506;
	genericExtensionIconResource = -16415;
	systemFolderIconResource	= -3983;
	appleMenuFolderIconResource	= -3982;

	{	 Avoid using icon resources if possible. Use IconServices instead. 	}
	kStartupFolderIconResource	= -3981;
	kOwnedFolderIconResource	= -3980;
	kDropFolderIconResource		= -3979;
	kSharedFolderIconResource	= -3978;
	kMountedFolderIconResource	= -3977;
	kControlPanelFolderIconResource = -3976;
	kPrintMonitorFolderIconResource = -3975;
	kPreferencesFolderIconResource = -3974;
	kExtensionsFolderIconResource = -3973;
	kFontsFolderIconResource	= -3968;
	kFullTrashIconResource		= -3984;

	{	 Obsolete. Use named constants defined above. 	}
	startupFolderIconResource	= -3981;
	ownedFolderIconResource		= -3980;
	dropFolderIconResource		= -3979;
	sharedFolderIconResource	= -3978;
	mountedFolderIconResource	= -3977;
	controlPanelFolderIconResource = -3976;
	printMonitorFolderIconResource = -3975;
	preferencesFolderIconResource = -3974;
	extensionsFolderIconResource = -3973;
	fontsFolderIconResource		= -3968;
	fullTrashIconResource		= -3984;

	{	 The following icon types can only be used as an icon element 	}
	{	 inside a 'icns' icon family 	}
    kIconServices256PixelDataARGB = $69633038 (* 'ic08' *);
	kThumbnail32BitData			= $69743332 (* 'it32' *);
	kThumbnail8BitMask			= $74386D6B (* 't8mk' *);

	kHuge1BitMask				= $69636823 (* 'ich#' *);
	kHuge4BitData				= $69636834 (* 'ich4' *);
	kHuge8BitData				= $69636838 (* 'ich8' *);
	kHuge32BitData				= $69683332 (* 'ih32' *);
	kHuge8BitMask				= $68386D6B (* 'h8mk' *);

	{	 The following icon types can be used as a resource type 	}
	{	 or as an icon element type inside a 'icns' icon family 	}
	kLarge1BitMask				= $49434E23 (* 'ICN#' *);
	kLarge4BitData				= $69636C34 (* 'icl4' *);
	kLarge8BitData				= $69636C38 (* 'icl8' *);
	kLarge32BitData				= $696C3332 (* 'il32' *);
	kLarge8BitMask				= $6C386D6B (* 'l8mk' *);
	kSmall1BitMask				= $69637323 (* 'ics#' *);
	kSmall4BitData				= $69637334 (* 'ics4' *);
	kSmall8BitData				= $69637338 (* 'ics8' *);
	kSmall32BitData				= $69733332 (* 'is32' *);
	kSmall8BitMask				= $73386D6B (* 's8mk' *);
	kMini1BitMask				= $69636D23 (* 'icm#' *);
	kMini4BitData				= $69636D34 (* 'icm4' *);
	kMini8BitData				= $69636D38 (* 'icm8' *);

	{	  Icon Variants 	}
	{	 These can be used as an element of an 'icns' icon family 	}
	{	 or as a parameter to GetIconRefVariant 	}
	kTileIconVariant			= $74696C65 (* 'tile' *);
	kRolloverIconVariant		= $6F766572 (* 'over' *);
	kDropIconVariant			= $64726F70 (* 'drop' *);
	kOpenIconVariant			= $6F70656E (* 'open' *);
	kOpenDropIconVariant		= $6F647270 (* 'odrp' *);

	{	 Obsolete. Use names defined above. 	}
	large1BitMask				= $49434E23 (* 'ICN#' *);
	large4BitData				= $69636C34 (* 'icl4' *);
	large8BitData				= $69636C38 (* 'icl8' *);
	small1BitMask				= $69637323 (* 'ics#' *);
	small4BitData				= $69637334 (* 'ics4' *);
	small8BitData				= $69637338 (* 'ics8' *);
	mini1BitMask				= $69636D23 (* 'icm#' *);
	mini4BitData				= $69636D34 (* 'icm4' *);
	mini8BitData				= $69636D38 (* 'icm8' *);

	{	 Alignment type values. 	}
	kAlignNone					= $00;
	kAlignVerticalCenter		= $01;
	kAlignTop					= $02;
	kAlignBottom				= $03;
	kAlignHorizontalCenter		= $04;
	kAlignAbsoluteCenter		= $05;
	kAlignCenterTop				= $06;
	kAlignCenterBottom			= $07;
	kAlignLeft					= $08;
	kAlignCenterLeft			= $09;
	kAlignTopLeft				= $0A;
	kAlignBottomLeft			= $0B;
	kAlignRight					= $0C;
	kAlignCenterRight			= $0D;
	kAlignTopRight				= $0E;
	kAlignBottomRight			= $0F;

	{	 Obsolete. Use names defined above. 	}
	atNone						= $00;
	atVerticalCenter			= $01;
	atTop						= $02;
	atBottom					= $03;
	atHorizontalCenter			= $04;
	atAbsoluteCenter			= $05;
	atCenterTop					= $06;
	atCenterBottom				= $07;
	atLeft						= $08;
	atCenterLeft				= $09;
	atTopLeft					= $0A;
	atBottomLeft				= $0B;
	atRight						= $0C;
	atCenterRight				= $0D;
	atTopRight					= $0E;
	atBottomRight				= $0F;


type
	IconAlignmentType					= SInt16;
	{	 Transform type values. 	}

const
	kTransformNone				= $00;
	kTransformDisabled			= $01;
	kTransformOffline			= $02;
	kTransformOpen				= $03;
	kTransformLabel1			= $0100;
	kTransformLabel2			= $0200;
	kTransformLabel3			= $0300;
	kTransformLabel4			= $0400;
	kTransformLabel5			= $0500;
	kTransformLabel6			= $0600;
	kTransformLabel7			= $0700;
	kTransformSelected			= $4000;
	kTransformSelectedDisabled	= $4001;
	kTransformSelectedOffline	= $4002;
	kTransformSelectedOpen		= $4003;

	{	 Obsolete. Use names defined above. 	}
	ttNone						= $00;
	ttDisabled					= $01;
	ttOffline					= $02;
	ttOpen						= $03;
	ttLabel1					= $0100;
	ttLabel2					= $0200;
	ttLabel3					= $0300;
	ttLabel4					= $0400;
	ttLabel5					= $0500;
	ttLabel6					= $0600;
	ttLabel7					= $0700;
	ttSelected					= $4000;
	ttSelectedDisabled			= $4001;
	ttSelectedOffline			= $4002;
	ttSelectedOpen				= $4003;


type
	IconTransformType					= SInt16;
	{	 Selector mask values. 	}

const
	kSelectorLarge1Bit			= $00000001;
	kSelectorLarge4Bit			= $00000002;
	kSelectorLarge8Bit			= $00000004;
	kSelectorLarge32Bit			= $00000008;
	kSelectorLarge8BitMask		= $00000010;
	kSelectorSmall1Bit			= $00000100;
	kSelectorSmall4Bit			= $00000200;
	kSelectorSmall8Bit			= $00000400;
	kSelectorSmall32Bit			= $00000800;
	kSelectorSmall8BitMask		= $00001000;
	kSelectorMini1Bit			= $00010000;
	kSelectorMini4Bit			= $00020000;
	kSelectorMini8Bit			= $00040000;
	kSelectorHuge1Bit			= $01000000;
	kSelectorHuge4Bit			= $02000000;
	kSelectorHuge8Bit			= $04000000;
	kSelectorHuge32Bit			= $08000000;
	kSelectorHuge8BitMask		= $10000000;
	kSelectorAllLargeData		= $000000FF;
	kSelectorAllSmallData		= $0000FF00;
	kSelectorAllMiniData		= $00FF0000;
	kSelectorAllHugeData		= $FF000000;
	kSelectorAll1BitData		= $01010101;
	kSelectorAll4BitData		= $02020202;
	kSelectorAll8BitData		= $04040404;
	kSelectorAll32BitData		= $08000808;
	kSelectorAllAvailableData	= $FFFFFFFF;


	{	 Obsolete. Use names defined above. 	}
	svLarge1Bit					= $00000001;
	svLarge4Bit					= $00000002;
	svLarge8Bit					= $00000004;
	svSmall1Bit					= $00000100;
	svSmall4Bit					= $00000200;
	svSmall8Bit					= $00000400;
	svMini1Bit					= $00010000;
	svMini4Bit					= $00020000;
	svMini8Bit					= $00040000;
	svAllLargeData				= $000000FF;
	svAllSmallData				= $0000FF00;
	svAllMiniData				= $00FF0000;
	svAll1BitData				= $01010101;
	svAll4BitData				= $02020202;
	svAll8BitData				= $04040404;
	svAllAvailableData			= $FFFFFFFF;


type
	IconSelectorValue					= UInt32;
{$ifc TYPED_FUNCTION_POINTERS}
	IconActionProcPtr = function(theType: ResType; var theIcon: Handle; yourDataPtr: UnivPtr): OSErr;
{$elsec}
	IconActionProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	IconGetterProcPtr = function(theType: ResType; yourDataPtr: UnivPtr): Handle;
{$elsec}
	IconGetterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	IconActionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	IconActionUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	IconGetterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	IconGetterUPP = UniversalProcPtr;
{$endc}	

const
	uppIconActionProcInfo = $00000FE0;
	uppIconGetterProcInfo = $000003F0;
	{
	 *  NewIconActionUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewIconActionUPP(userRoutine: IconActionProcPtr): IconActionUPP; external name '_NewIconActionUPP'; { old name was NewIconActionProc }
{
 *  NewIconGetterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewIconGetterUPP(userRoutine: IconGetterProcPtr): IconGetterUPP; external name '_NewIconGetterUPP'; { old name was NewIconGetterProc }
{
 *  DisposeIconActionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeIconActionUPP(userUPP: IconActionUPP); external name '_DisposeIconActionUPP';
{
 *  DisposeIconGetterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeIconGetterUPP(userUPP: IconGetterUPP); external name '_DisposeIconGetterUPP';
{
 *  InvokeIconActionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeIconActionUPP(theType: ResType; var theIcon: Handle; yourDataPtr: UnivPtr; userRoutine: IconActionUPP): OSErr; external name '_InvokeIconActionUPP'; { old name was CallIconActionProc }
{
 *  InvokeIconGetterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeIconGetterUPP(theType: ResType; yourDataPtr: UnivPtr; userRoutine: IconGetterUPP): Handle; external name '_InvokeIconGetterUPP'; { old name was CallIconGetterProc }
type
	IconGetter							= IconGetterProcPtr;
	IconAction							= IconActionProcPtr;
	{  CIconHandle, GetCIcon(), PlotCIcon(), and DisposeCIcon() moved here from Quickdraw.h }
	CIconPtr = ^CIcon;
	CIcon = record
		iconPMap:				PixMap;									{ the icon's pixMap }
		iconMask:				BitMap;									{ the icon's mask }
		iconBMap:				BitMap;									{ the icon's bitMap }
		iconData:				Handle;									{ the icon's data }
		iconMaskData:			array [0..0] of SInt16;					{ icon's mask and BitMap data }
	end;

	CIconHandle							= ^CIconPtr;
	CIconHandle_fix                     = CIconHandle; { used as field type when a record declaration contains a CIconHandle field identifier }
	{
	 *  GetCIcon()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function GetCIcon(iconID: SInt16): CIconHandle; external name '_GetCIcon';
{
 *  PlotCIcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PlotCIcon(const (*var*) theRect: Rect; theIcon: CIconHandle); external name '_PlotCIcon';
{
 *  DisposeCIcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeCIcon(theIcon: CIconHandle); external name '_DisposeCIcon';
{  GetIcon and PlotIcon moved here from ToolUtils }
{
 *  GetIcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIcon(iconID: SInt16): Handle; external name '_GetIcon';
{
 *  PlotIcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PlotIcon(const (*var*) theRect: Rect; theIcon: Handle); external name '_PlotIcon';
{
    Note:   IconSuiteRef and IconCacheRef should be an abstract types, 
            but too much source code already relies on them being of type Handle.
}

type
	IconSuiteRef						= Handle;
	IconCacheRef						= Handle;
	{  IconRefs are 32-bit values identifying cached icon data. IconRef 0 is invalid. }
	IconRef     = ^SInt32;  { an opaque 32-bit type }
	IconRef_fix = IconRef;  { used as field type when a record declaration contains a IconRef field identifier }
	IconRefPtr  = ^IconRef; { when a var xx:IconRef parameter can be nil, it is changed to xx: IconRefPtr }
	{
	 *  PlotIconID()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function PlotIconID(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theResID: SInt16): OSErr; external name '_PlotIconID';
{
 *  NewIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewIconSuite(var theIconSuite: IconSuiteRef): OSErr; external name '_NewIconSuite';
{
 *  AddIconToSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AddIconToSuite(theIconData: Handle; theSuite: IconSuiteRef; theType: ResType): OSErr; external name '_AddIconToSuite';
{
 *  GetIconFromSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconFromSuite(var theIconData: Handle; theSuite: IconSuiteRef; theType: ResType): OSErr; external name '_GetIconFromSuite';
{
 *  ForEachIconDo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ForEachIconDo(theSuite: IconSuiteRef; selector: IconSelectorValue; action: IconActionUPP; yourDataPtr: UnivPtr): OSErr; external name '_ForEachIconDo';
{
 *  GetIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconSuite(var theIconSuite: IconSuiteRef; theResID: SInt16; selector: IconSelectorValue): OSErr; external name '_GetIconSuite';
{
 *  DisposeIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DisposeIconSuite(theIconSuite: IconSuiteRef; disposeData: boolean): OSErr; external name '_DisposeIconSuite';
{
 *  PlotIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PlotIconSuite(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIconSuite: IconSuiteRef): OSErr; external name '_PlotIconSuite';
{
 *  MakeIconCache()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MakeIconCache(var theCache: IconCacheRef; makeIcon: IconGetterUPP; yourDataPtr: UnivPtr): OSErr; external name '_MakeIconCache';
{
 *  LoadIconCache()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LoadIconCache(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIconCache: IconCacheRef): OSErr; external name '_LoadIconCache';
{
 *  PlotIconMethod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PlotIconMethod(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr): OSErr; external name '_PlotIconMethod';
{
 *  GetLabel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetLabel(labelNumber: SInt16; var labelColor: RGBColor; var labelString: Str255): OSErr; external name '_GetLabel';
{
 *  PtInIconID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PtInIconID(testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; iconID: SInt16): boolean; external name '_PtInIconID';
{
 *  PtInIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PtInIconSuite(testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconSuite: IconSuiteRef): boolean; external name '_PtInIconSuite';
{
 *  PtInIconMethod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PtInIconMethod(testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr): boolean; external name '_PtInIconMethod';
{
 *  RectInIconID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RectInIconID(const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; iconID: SInt16): boolean; external name '_RectInIconID';
{
 *  RectInIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RectInIconSuite(const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconSuite: IconSuiteRef): boolean; external name '_RectInIconSuite';
{
 *  RectInIconMethod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RectInIconMethod(const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr): boolean; external name '_RectInIconMethod';
{
 *  IconIDToRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IconIDToRgn(theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; iconID: SInt16): OSErr; external name '_IconIDToRgn';
{
 *  IconSuiteToRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IconSuiteToRgn(theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconSuite: IconSuiteRef): OSErr; external name '_IconSuiteToRgn';
{
 *  IconMethodToRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IconMethodToRgn(theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr): OSErr; external name '_IconMethodToRgn';
{
 *  SetSuiteLabel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSuiteLabel(theSuite: IconSuiteRef; theLabel: SInt16): OSErr; external name '_SetSuiteLabel';
{
 *  GetSuiteLabel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSuiteLabel(theSuite: IconSuiteRef): SInt16; external name '_GetSuiteLabel';
{
 *  GetIconCacheData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconCacheData(theCache: IconCacheRef; var theData: UnivPtr): OSErr; external name '_GetIconCacheData';
{
 *  SetIconCacheData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetIconCacheData(theCache: IconCacheRef; theData: UnivPtr): OSErr; external name '_SetIconCacheData';
{
 *  GetIconCacheProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconCacheProc(theCache: IconCacheRef; var theProc: IconGetterUPP): OSErr; external name '_GetIconCacheProc';
{
 *  SetIconCacheProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetIconCacheProc(theCache: IconCacheRef; theProc: IconGetterUPP): OSErr; external name '_SetIconCacheProc';
{
 *  PlotIconHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PlotIconHandle(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIcon: Handle): OSErr; external name '_PlotIconHandle';
{
 *  PlotSICNHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PlotSICNHandle(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theSICN: Handle): OSErr; external name '_PlotSICNHandle';
{
 *  PlotCIconHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PlotCIconHandle(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theCIcon: CIconHandle): OSErr; external name '_PlotCIconHandle';
{
   IconServices is an efficient mechanism to share icon data amongst multiple 
   clients. It avoids duplication of data; it provides efficient caching, 
   releasing memory when the icon data is no longer needed; it can provide
   the appropriate icon for any filesystem object; it can provide commonly 
   used icons (caution, note, help...); it is Appearance-savvy: the icons
   are switched when appropriate.
   IconServices refer to cached icon data using IconRef, a 32-bit opaque
   value. IconRefs are reference counted. When there are no more "owners" 
   of an IconRef, the memory used by the icon bitmap is disposed of.
   Two files of same type and creator with no custom icon will have the same IconRef.
   Files with custom icons will have their own IconRef.
}

{
   Use the special creator kSystemIconsCreator to get "standard" icons 
   that are not associated with a file, such as the help icon.
   Note that all lowercase creators are reserved by Apple.
}

const
	kSystemIconsCreator			= $6D616373 (* 'macs' *);


	{
	   Type of the predefined/generic icons. For example, the call:
	      err = GetIconRef(kOnSystemDisk, kSystemIconsCreator, kHelpIcon, &iconRef);
	   will retun in iconRef the IconRef for the standard help icon.
	}

	{	 Generic Finder icons 	}
	kClipboardIcon				= $434C4950 (* 'CLIP' *);
	kClippingUnknownTypeIcon	= $636C7075 (* 'clpu' *);
	kClippingPictureTypeIcon	= $636C7070 (* 'clpp' *);
	kClippingTextTypeIcon		= $636C7074 (* 'clpt' *);
	kClippingSoundTypeIcon		= $636C7073 (* 'clps' *);
	kDesktopIcon				= $6465736B (* 'desk' *);
	kFinderIcon					= $464E4452 (* 'FNDR' *);
	kComputerIcon					= $726F6F74 (* 'root' *);
	kFontSuitcaseIcon			= $4646494C (* 'FFIL' *);
	kFullTrashIcon				= $66747268 (* 'ftrh' *);
	kGenericApplicationIcon		= $4150504C (* 'APPL' *);
	kGenericCDROMIcon			= $63646472 (* 'cddr' *);
	kGenericControlPanelIcon	= $41505043 (* 'APPC' *);
	kGenericControlStripModuleIcon = $73646576 (* 'sdev' *);
	kGenericComponentIcon		= $74686E67 (* 'thng' *);
	kGenericDeskAccessoryIcon	= $41505044 (* 'APPD' *);
	kGenericDocumentIcon		= $646F6375 (* 'docu' *);
	kGenericEditionFileIcon		= $65647466 (* 'edtf' *);
	kGenericExtensionIcon		= $494E4954 (* 'INIT' *);
	kGenericFileServerIcon		= $73727672 (* 'srvr' *);
	kGenericFontIcon			= $6666696C (* 'ffil' *);
	kGenericFontScalerIcon		= $73636C72 (* 'sclr' *);
	kGenericFloppyIcon			= $666C7079 (* 'flpy' *);
	kGenericHardDiskIcon		= $6864736B (* 'hdsk' *);
	kGenericIDiskIcon			= $6964736B (* 'idsk' *);
	kGenericRemovableMediaIcon	= $726D6F76 (* 'rmov' *);
	kGenericMoverObjectIcon		= $6D6F7672 (* 'movr' *);
	kGenericPCCardIcon			= $70636D63 (* 'pcmc' *);
	kGenericPreferencesIcon		= $70726566 (* 'pref' *);
	kGenericQueryDocumentIcon	= $71657279 (* 'qery' *);
	kGenericRAMDiskIcon			= $72616D64 (* 'ramd' *);
	kGenericSharedLibaryIcon	= $73686C62 (* 'shlb' *);
	kGenericStationeryIcon		= $73646F63 (* 'sdoc' *);
	kGenericSuitcaseIcon		= $73756974 (* 'suit' *);
	kGenericURLIcon				= $6775726C (* 'gurl' *);
	kGenericWORMIcon			= $776F726D (* 'worm' *);
	kInternationalResourcesIcon	= $6966696C (* 'ifil' *);
	kKeyboardLayoutIcon			= $6B66696C (* 'kfil' *);
	kSoundFileIcon				= $7366696C (* 'sfil' *);
	kSystemSuitcaseIcon			= $7A737973 (* 'zsys' *);
	kTrashIcon					= $74727368 (* 'trsh' *);
	kTrueTypeFontIcon			= $7466696C (* 'tfil' *);
	kTrueTypeFlatFontIcon		= $73666E74 (* 'sfnt' *);
	kTrueTypeMultiFlatFontIcon	= $74746366 (* 'ttcf' *);
	kUserIDiskIcon				= $7564736B (* 'udsk' *);
	kUnknownFSObjectIcon				= $756E6673 (* 'unfs' *);
	kInternationResourcesIcon	= $6966696C (* 'ifil' *);						{  old misspelling }

	{	 Internet locations 	}
	kInternetLocationHTTPIcon	= $696C6874 (* 'ilht' *);
	kInternetLocationFTPIcon	= $696C6674 (* 'ilft' *);
	kInternetLocationAppleShareIcon = $696C6166 (* 'ilaf' *);
	kInternetLocationAppleTalkZoneIcon = $696C6174 (* 'ilat' *);
	kInternetLocationFileIcon	= $696C6669 (* 'ilfi' *);
	kInternetLocationMailIcon	= $696C6D61 (* 'ilma' *);
	kInternetLocationNewsIcon	= $696C6E77 (* 'ilnw' *);
	kInternetLocationNSLNeighborhoodIcon = $696C6E73 (* 'ilns' *);
	kInternetLocationGenericIcon = $696C6765 (* 'ilge' *);

	{	 Folders 	}
	kGenericFolderIcon			= $666C6472 (* 'fldr' *);
	kDropFolderIcon				= $64626F78 (* 'dbox' *);
	kMountedFolderIcon			= $6D6E7464 (* 'mntd' *);
	kOpenFolderIcon				= $6F666C64 (* 'ofld' *);
	kOwnedFolderIcon			= $6F776E64 (* 'ownd' *);
	kPrivateFolderIcon			= $70727666 (* 'prvf' *);
	kSharedFolderIcon			= $7368666C (* 'shfl' *);

	{	 Sharing Privileges icons 	}
	kSharingPrivsNotApplicableIcon = $73686E61 (* 'shna' *);
	kSharingPrivsReadOnlyIcon	= $7368726F (* 'shro' *);
	kSharingPrivsReadWriteIcon	= $73687277 (* 'shrw' *);
	kSharingPrivsUnknownIcon	= $7368756B (* 'shuk' *);
	kSharingPrivsWritableIcon	= $77726974 (* 'writ' *);


	{	 Users and Groups icons 	}
	kUserFolderIcon				= $75666C64 (* 'ufld' *);
	kWorkgroupFolderIcon		= $77666C64 (* 'wfld' *);
	kGuestUserIcon				= $67757372 (* 'gusr' *);
	kUserIcon					= $75736572 (* 'user' *);
	kOwnerIcon					= $73757372 (* 'susr' *);
	kGroupIcon					= $67727570 (* 'grup' *);

	{	 Special folders 	}
	kAppearanceFolderIcon		= $61707072 (* 'appr' *);
	kAppleExtrasFolderIcon		= $616578C6 (* 'aexÄ' *);
	kAppleMenuFolderIcon		= $616D6E75 (* 'amnu' *);
	kApplicationsFolderIcon		= $61707073 (* 'apps' *);
	kApplicationSupportFolderIcon = $61737570 (* 'asup' *);
	kAssistantsFolderIcon		= $617374C6 (* 'astÄ' *);
	kColorSyncFolderIcon		= $70726F66 (* 'prof' *);
	kContextualMenuItemsFolderIcon = $636D6E75 (* 'cmnu' *);
	kControlPanelDisabledFolderIcon = $63747244 (* 'ctrD' *);
	kControlPanelFolderIcon		= $6374726C (* 'ctrl' *);
	kControlStripModulesFolderIcon = $736476C6 (* 'sdvÄ' *);
	kDocumentsFolderIcon		= $646F6373 (* 'docs' *);
	kExtensionsDisabledFolderIcon = $65787444 (* 'extD' *);
	kExtensionsFolderIcon		= $6578746E (* 'extn' *);
	kFavoritesFolderIcon		= $66617673 (* 'favs' *);
	kFontsFolderIcon			= $666F6E74 (* 'font' *);
	kHelpFolderIcon				= $C692686C (* 'Ählp' *);
	kInternetFolderIcon			= $696E74C6 (* 'intÄ' *);
	kInternetPlugInFolderIcon	= $C6926E65 (* 'Änet' *);
	kInternetSearchSitesFolderIcon = $69737366 (* 'issf' *);
	kLocalesFolderIcon			= $C6926C6F (* 'Äloc' *);
	kMacOSReadMeFolderIcon		= $6D6F72C6 (* 'morÄ' *);
	kPublicFolderIcon			= $70756266 (* 'pubf' *);
	kPreferencesFolderIcon		= $707266C6 (* 'prfÄ' *);
	kPrinterDescriptionFolderIcon = $70706466 (* 'ppdf' *);
	kPrinterDriverFolderIcon	= $C6927072 (* 'Äprd' *);
	kPrintMonitorFolderIcon		= $70726E74 (* 'prnt' *);
	kRecentApplicationsFolderIcon = $72617070 (* 'rapp' *);
	kRecentDocumentsFolderIcon	= $72646F63 (* 'rdoc' *);
	kRecentServersFolderIcon	= $72737276 (* 'rsrv' *);
	kScriptingAdditionsFolderIcon = $C6927363 (* 'Äscr' *);
	kSharedLibrariesFolderIcon	= $C6926C69 (* 'Älib' *);
	kScriptsFolderIcon			= $736372C6 (* 'scrÄ' *);
	kShutdownItemsDisabledFolderIcon = $73686444 (* 'shdD' *);
	kShutdownItemsFolderIcon	= $73686466 (* 'shdf' *);
	kSpeakableItemsFolder		= $73706B69 (* 'spki' *);
	kStartupItemsDisabledFolderIcon = $73747244 (* 'strD' *);
	kStartupItemsFolderIcon		= $73747274 (* 'strt' *);
	kSystemExtensionDisabledFolderIcon = $6D616344 (* 'macD' *);
	kSystemFolderIcon			= $6D616373 (* 'macs' *);
	kTextEncodingsFolderIcon	= $C6927465 (* 'Ätex' *);
	kUsersFolderIcon			= $757372C6 (* 'usrÄ' *);
	kUtilitiesFolderIcon		= $757469C6 (* 'utiÄ' *);
	kVoicesFolderIcon			= $66766F63 (* 'fvoc' *);
	kSystemFolderXIcon			= $6D616378 (* 'macx' *);

	{	 Badges 	}
	kAppleScriptBadgeIcon		= $73637270 (* 'scrp' *);
	kLockedBadgeIcon			= $6C626467 (* 'lbdg' *);
	kMountedBadgeIcon			= $6D626467 (* 'mbdg' *);
	kSharedBadgeIcon			= $73626467 (* 'sbdg' *);
	kAliasBadgeIcon				= $61626467 (* 'abdg' *);
	kAlertCautionBadgeIcon		= $63626467 (* 'cbdg' *);

	{	 Alert icons 	}
	kAlertNoteIcon				= $6E6F7465 (* 'note' *);
	kAlertCautionIcon			= $63617574 (* 'caut' *);
	kAlertStopIcon				= $73746F70 (* 'stop' *);

	{	 Networking icons 	}
	kAppleTalkIcon				= $61746C6B (* 'atlk' *);
	kAppleTalkZoneIcon			= $61747A6E (* 'atzn' *);
	kAFPServerIcon				= $61667073 (* 'afps' *);
	kFTPServerIcon				= $66747073 (* 'ftps' *);
	kHTTPServerIcon				= $68747073 (* 'htps' *);
	kGenericNetworkIcon			= $676E6574 (* 'gnet' *);
	kIPFileServerIcon			= $69737276 (* 'isrv' *);

	{	 Toolbar icons 	}
	kToolbarCustomizeIcon		= $74637573 (* 'tcus' *);
	kToolbarDeleteIcon			= $7464656C (* 'tdel' *);
	kToolbarFavoritesIcon		= $74666176 (* 'tfav' *);
	kToolbarHomeIcon			= $74686F6D (* 'thom' *);

	{	 Other icons 	}
	kAppleLogoIcon				= $6361706C (* 'capl' *);
	kAppleMenuIcon				= $7361706C (* 'sapl' *);
	kBackwardArrowIcon			= $6261726F (* 'baro' *);
	kFavoriteItemsIcon			= $66617672 (* 'favr' *);
	kForwardArrowIcon			= $6661726F (* 'faro' *);
	kGridIcon					= $67726964 (* 'grid' *);
	kHelpIcon					= $68656C70 (* 'help' *);
	kKeepArrangedIcon			= $61726E67 (* 'arng' *);
	kLockedIcon					= $6C6F636B (* 'lock' *);
	kNoFilesIcon				= $6E66696C (* 'nfil' *);
	kNoFolderIcon				= $6E666C64 (* 'nfld' *);
	kNoWriteIcon				= $6E777274 (* 'nwrt' *);
	kProtectedApplicationFolderIcon = $70617070 (* 'papp' *);
	kProtectedSystemFolderIcon	= $70737973 (* 'psys' *);
	kRecentItemsIcon			= $72636E74 (* 'rcnt' *);
	kShortcutIcon				= $73687274 (* 'shrt' *);
	kSortAscendingIcon			= $61736E64 (* 'asnd' *);
	kSortDescendingIcon			= $64736E64 (* 'dsnd' *);
	kUnlockedIcon				= $756C636B (* 'ulck' *);
	kConnectToIcon				= $636E6374 (* 'cnct' *);
	kGenericWindowIcon			= $6777696E (* 'gwin' *);
	kQuestionMarkIcon			= $71756573 (* 'ques' *);
	kDeleteAliasIcon			= $64616C69 (* 'dali' *);
	kEjectMediaIcon				= $656A6563 (* 'ejec' *);
	kBurningIcon				= $6275726E (* 'burn' *);
	kRightContainerArrowIcon	= $72636172 (* 'rcar' *);


	{	  IconServicesUsageFlags 	}

type
	IconServicesUsageFlags				= UInt32;

const
	kIconServicesNormalUsageFlag    = $00000000;
	kIconServicesNoBadgeFlag        = $00000001;     // available on Panther and later
	kIconServicesUpdateIfNeededFlag = $00000002;     // available on Panther and later


	{	
	  kIconServicesCatalogInfoMask - Minimal bitmask for use with
	    GetIconRefFromFileInfo(). Use this mask with FSGetCatalogInfo
	    before calling GetIconRefFromFileInfo().
		}
	kIconServicesCatalogInfoMask = $0008181E;


	{
	 *  PlotIconRefFlags
	 *  
	 *  Discussion:
	 *    Flags that can be passed to the PlotIconRefInContext routine.
	 	}

type
	PlotIconRefFlags 			= UInt32;
const
	kPlotIconRefNormalFlags		= 0;
	kPlotIconRefNoImage			= $02;
	kPlotIconRefNoMask			= $04;


	{
	    IconFamily 'icns' resources contain an entire IconFamily (all sizes and depths).  
	   For custom icons, icns IconFamily resources of the custom icon resource ID are fetched first before
	   the classic custom icons (individual 'ics#, ICN#, etc) are fetched.  If the fetch of the icns resource
	   succeeds then the icns is looked at exclusively for the icon data.
	   For custom icons, new icon features such as 32-bit deep icons are only fetched from the icns resource.
	   This is to avoid incompatibilities with cut & paste of new style icons with an older version of the
	   MacOS Finder.
	   DriverGestalt is called with code kdgMediaIconSuite by IconServices after calling FSM to determine a
	   driver icon for a particular device.  The result of the kdgMediaIconSuite call to DriverGestalt should
	   be a pointer an an IconFamily.  In this manner driver vendors can provide rich, detailed drive icons
	   instead of the 1-bit variety previously supported.
	}
	kIconFamilyType				= $69636E73 (* 'icns' *);


type
	IconFamilyElementPtr = ^IconFamilyElement;
	IconFamilyElement = record
		elementType:			OSType;									{  'ICN#', 'icl8', etc... }
		elementSize:			Size;									{  Size of this element }
		elementData:			SInt8;
	end;

	IconFamilyResourcePtr = ^IconFamilyResource;
	IconFamilyResource = record
		resourceType:			OSType;									{  Always 'icns' }
		resourceSize:			Size;									{  Total size of this resource }
		elements:				array [0..0] of IconFamilyElement;
	end;

	IconFamilyPtr						= ^IconFamilyResource;
	IconFamilyHandle					= ^IconFamilyPtr;
	{
	  ==============================================================================
	   Initialization and Termination
	  ==============================================================================
	}

	{
	   IconServicesInit
	   
	   Call this routine once per classic 68K application initialization.
	   This routine does not need to be called at boot time.
	}

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  IconServicesInit()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function IconServicesInit(initBlockPtr: CFragInitBlockPtr): OSErr; external name '_IconServicesInit';
{
   IconServicesTerminate:
   
   Call this routine once from the termination of a classic 68K application.
   This routine does not need to be called at boot time.
}

{
 *  IconServicesTerminate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IconServicesTerminate; external name '_IconServicesTerminate';
{$endc}  {CALL_NOT_IN_CARBON}

{
  ==============================================================================
   Converting data structures
  ==============================================================================
}


{
   IconRefToIconFamily
   This routines returns a new IconFamily that contains the data corresponding
   to the specified IconRef.
}

{
 *  IconRefToIconFamily()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IconRefToIconFamily(theIconRef: IconRef; whichIcons: IconSelectorValue; var iconFamily: IconFamilyHandle): OSErr; external name '_IconRefToIconFamily';
{
   IconFamilyToIconSuite
   This routine transfers the data from an icon family handle into an icon suite.
}

{
 *  IconFamilyToIconSuite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IconFamilyToIconSuite(iconFamily: IconFamilyHandle; whichIcons: IconSelectorValue; var iconSuite: IconSuiteRef): OSErr; external name '_IconFamilyToIconSuite';
{
   IconSuiteToIconFamily
   This routine transfers the data in an icon suite into an icon family.
}

{
 *  IconSuiteToIconFamily()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IconSuiteToIconFamily(iconSuite: IconSuiteRef; whichIcons: IconSelectorValue; var iconFamily: IconFamilyHandle): OSErr; external name '_IconSuiteToIconFamily';
{
   SetIconFamilyData
   Change the data of an icon family. The data is copied.
   The type can be one of the icon type, or 'PICT'.
   The data will be compressed if needed.
}

{
 *  SetIconFamilyData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetIconFamilyData(iconFamily: IconFamilyHandle; iconType: OSType; h: Handle): OSErr; external name '_SetIconFamilyData';
{
   GetIconFamilyData
   Return a copy of the data in the icon family.
   The type can be one of the icon type, or 'PICT'
   The data will be returned uncompressed.
   The handle (h) will be resized as appropriate. If no data of the 
   requested type is present, the handle size will be set to 0.
}

{
 *  GetIconFamilyData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconFamilyData(iconFamily: IconFamilyHandle; iconType: OSType; h: Handle): OSErr; external name '_GetIconFamilyData';
{
  ==============================================================================
   Reference counting
  ==============================================================================
}


{
   GetIconRefOwners
   
   This routine returns the reference count for the IconRef, or number of owners.
   
   A valid IconRef always has at least one owner.
}

{
 *  GetIconRefOwners()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconRefOwners(theIconRef: IconRef; var owners: UInt16): OSErr; external name '_GetIconRefOwners';
{
   AcquireIconRef
   This routine increments the reference count for the IconRef
}

{
 *  AcquireIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AcquireIconRef(theIconRef: IconRef): OSErr; external name '_AcquireIconRef';
{
   ReleaseIconRef
   
   This routine decrements the reference count for the IconRef.
   
   When the reference count reaches 0, all memory allocated for the icon
   is disposed. Any subsequent use of the IconRef is invalid.
}

{
 *  ReleaseIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReleaseIconRef(theIconRef: IconRef): OSErr; external name '_ReleaseIconRef';
{
  ==============================================================================
   Getting an IconRef
  ==============================================================================
}


{
   GetIconRefFromFile
   
   This routine returns an icon ref for the specified file, folder or volume.
   The label information is provided separately, since two files with the same icon 
   but a different label would share the same iconRef. The label can be used in 
   PlotIconRef() for example.
   
   Use this routine if you have no information about the file system object. If 
   you have already done a GetCatInfo on the file and want to save some I/O, 
   call GetIconRefFromFolder() if you know it's a folder with no custom icon or 
   call GetIconRef() if it's a file with no custom icon.
   This routine increments the reference count of the returned IconRef. Call 
   ReleaseIconRef() when you're done with it.
}

{
 *  GetIconRefFromFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconRefFromFile(const (*var*) theFile: FSSpec; var theIconRef: IconRef; var theLabel: SInt16): OSErr; external name '_GetIconRefFromFile';
{
   GetIconRef
   
   This routine returns an icon ref for an icon in the desktop database or
   for a registered icon.
   The system registers a set of icon such as the help icon with the creator 
   code kSystemIconsCreator. See above for a list of the registered system types.
   The vRefNum is used as a hint on where to look for the icon first. Use 
   kOnSystemDisk if you don't know what to pass.
   This routine increments the reference count of the returned IconRef. Call 
   ReleaseIconRef() when you're done with it.
}

{
 *  GetIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconRef(vRefNum: SInt16; creator: OSType; iconType: OSType; var theIconRef: IconRef): OSErr; external name '_GetIconRef';
{
   GetIconRefFromFolder
   
   This routine returns an icon ref for a folder with no custom icon.
   Use the more generic, but slightly slower, GetIconRefFromFile() if
   you don't already have the necessary info about the file.
   Attributes should be CInfoPBRec.dirInfo.ioFlAttrib for this folder.
   Access privileges should be CInfoPBRec.dirInfo.ioACUser for this folder.
   This routine increments the reference count of the IconRef. Call ReleaseIconRef() 
   when you're done with it.
}

{
 *  GetIconRefFromFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconRefFromFolder(vRefNum: SInt16; parentFolderID: SInt32; folderID: SInt32; attributes: SInt8; accessPrivileges: SInt8; var theIconRef: IconRef): OSErr; external name '_GetIconRefFromFolder';
{  GetIconRefFromFileInfo }
{
 *  GetIconRefFromFileInfo()
 *  
 *  Summary:
 *    This routine returns an IconRef for a file with minimal file I/O.
 *  
 *  Discussion:
 *    To minimize file operations, FSGetCatalogInfo should be called
 *    prior to calling this routine. The FSCatalogInfo should
 *    correspond to kIconServicesCatalogInfoMask The name should be
 *    fetched and passed in. If either the name or the correct catalog
 *    info is not passed in, this routine will do file operations for
 *    this information instead.
 *  
 *  Parameters:
 *    
 *    inRef:
 *      An FSRef for the target file
 *    
 *    inFileNameLength:
 *      The length of the name of the target file
 *    
 *    inFileName:
 *      The name of the target file
 *    
 *    inWhichInfo:
 *      The mask of file info already acquired.
 *    
 *    inCatalogInfo:
 *      The catalog info already acquired.
 *    
 *    inUsageFlags:
 *      The usage flags for this call (use
 *      kIconServicesNormalUsageFlag).
 *    
 *    outIconRef:
 *      The output IconRef for the routine.
 *    
 *    outLabel:
 *      The output label for the icon/file.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function GetIconRefFromFileInfo(const (*var*) inRef: FSRef; inFileNameLength: UniCharCount; inFileName: UniCharPtr; inWhichInfo: FSCatalogInfoBitmap; inCatalogInfo: {Const}FSCatalogInfoPtr; inUsageFlags: IconServicesUsageFlags; var outIconRef: IconRef; var outLabel: SInt16): OSStatus; external name '_GetIconRefFromFileInfo';

{ GetIconRefFromTypeInfo}
{
 *  GetIconRefFromTypeInfo()
 *  
 *  Summary:
 *    Create an IconRef for a type information.
 *  
 *  Discussion:
 *    Creates IconRef based on provided type info. Any of the input
 *    parameters can be zero (meaning it is unknown). Returns generic
 *    document icon in case if all parameters are zeroes. Calling the
 *    routine with non zero inCreator and inType and zero inExtension
 *    and inMIMEType is equivalent to GetIconRef(kOnSystemDisk,
 *    inCreator, inType).
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inCreator:
 *      The creator.
 *    
 *    inType:
 *      The type.
 *    
 *    inExtension:
 *      The extension.
 *    
 *    inMIMEType:
 *      The MIME type.
 *    
 *    inUsageFlags:
 *      The usage flags for this call (use
 *      kIconServicesNormalUsageFlag).
 *    
 *    outIconRef:
 *      The output IconRef for the routine.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function GetIconRefFromTypeInfo( inCreator: OSType; inType: OSType; inExtension: CFStringRef; inMIMEType: CFStringRef; inUsageFlags: IconServicesUsageFlags; var outIconRef: IconRef ): OSErr; external name '_GetIconRefFromTypeInfo';

{ GetIconRefFromIconFamilyPtr}
{
 *  GetIconRefFromIconFamilyPtr()
 *  
 *  Summary:
 *    Create an IconRef for the IconFamilyPtr.
 *  
 *  Discussion:
 *    This routine creates IconRef for the IconFamilyPtr.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inIconFamilyPtr:
 *      The icon data
 *    
 *    inSize:
 *      The icon data size
 *    
 *    outIconRef:
 *      The output IconRef for the routine.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function GetIconRefFromIconFamilyPtr( const (*var*) inIconFamilyPtr: IconFamilyResource; inSize: Size; var outIconRef: IconRef ): OSStatus; external name '_GetIconRefFromIconFamilyPtr';

{
  ==============================================================================
   Adding and modifying IconRef
  ==============================================================================
}


{
   RegisterIconRefFromIconFamily
   This routine adds a new entry to the IconRef registry. Other clients will be 
   able to access it using the (creator, iconType) pair specified here.
   Lower-case creators are reserved for the system.
   Consider using RegisterIconRefFromResource() if possible, since the data 
   registered using RegisterIconRefFromFamily() cannot be purged.
   The iconFamily data is copied and the caller is reponsible for disposing of it.
   This routine increments the reference count of the IconRef. Call ReleaseIconRef() 
   when you're done with it.
}

{
 *  RegisterIconRefFromIconFamily()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RegisterIconRefFromIconFamily(creator: OSType; iconType: OSType; iconFamily: IconFamilyHandle; var theIconRef: IconRef): OSErr; external name '_RegisterIconRefFromIconFamily';
{
   RegisterIconRefFromResource
   
   Registers an IconRef from a resouce file.  
   Lower-case creators are reserved for the system.
   The icon data to be fetched is either classic icon data or an icon family.  
   The 'icns' icon family is searched for before the classic icon data.
   This routine increments the reference count of the IconRef. Call ReleaseIconRef() 
   when you're done with it.
}

{
 *  RegisterIconRefFromResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RegisterIconRefFromResource(creator: OSType; iconType: OSType; const (*var*) resourceFile: FSSpec; resourceID: SInt16; var theIconRef: IconRef): OSErr; external name '_RegisterIconRefFromResource';
{  RegisterIconRefFromFSRef }
{
 *  RegisterIconRefFromFSRef()
 *  
 *  Discussion:
 *    This routine registers an IconRef from a ".icns" file and
 *    associates it with a creator/type pair.
 *  
 *  Parameters:
 *    
 *    creator:
 *      The creator code for the icns file.
 *    
 *    iconType:
 *      The type code for the icns file
 *    
 *    iconFile:
 *      The FSRef of the icns file.
 *    
 *    theIconRef:
 *      The output IconRef for the routine.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function RegisterIconRefFromFSRef(creator: OSType; iconType: OSType; const (*var*) iconFile: FSRef; var theIconRef: IconRef): OSStatus; external name '_RegisterIconRefFromFSRef';

{
   UnregisterIconRef
   
   Removes the specified icon from the icon cache (if there are no users of it).  
   If some clients are using this iconRef, then the IconRef will be removed when the 
   last user calls ReleaseIconRef.
}

{
 *  UnregisterIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UnregisterIconRef(creator: OSType; iconType: OSType): OSErr; external name '_UnregisterIconRef';
{
   UpdateIconRef
   
   Call this routine to force an update of the data for iconRef.
   
   For example after changing an icon in the desktop database or changing the custom 
   icon of a file. Note that after _adding_ a custom icon to file or folder, you 
   need to call GetIconRefFromFile() to get a new IconRef specific to this file. 
   
   This routine does nothing if the IconRef is a registered icon.
}

{
 *  UpdateIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UpdateIconRef(theIconRef: IconRef): OSErr; external name '_UpdateIconRef';
{
   OverrideIconRefFromResource
   
   This routines replaces the bitmaps of the specified IconRef with the ones
   in the specified resource file.
}

{
 *  OverrideIconRefFromResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OverrideIconRefFromResource(theIconRef: IconRef; const (*var*) resourceFile: FSSpec; resourceID: SInt16): OSErr; external name '_OverrideIconRefFromResource';
{
   OverrideIconRef
   
   This routines replaces the bitmaps of the specified IconRef with the ones
   from the new IconRef.
}

{
 *  OverrideIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OverrideIconRef(oldIconRef: IconRef; newIconRef: IconRef): OSErr; external name '_OverrideIconRef';
{
   RemoveIconRefOverride
   This routine remove an override if one was applied to the icon and 
   reverts back to the original bitmap data.
}

{
 *  RemoveIconRefOverride()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RemoveIconRefOverride(theIconRef: IconRef): OSErr; external name '_RemoveIconRefOverride';
{
  ==============================================================================
   Creating composite IconRef
  ==============================================================================
}


{
   CompositeIconRef
   
   Superimposes an IconRef on top of another one
}

{
 *  CompositeIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CompositeIconRef(backgroundIconRef: IconRef; foregroundIconRef: IconRef; var compositeIconRef: IconRef): OSErr; external name '_CompositeIconRef';
{
   IsIconRefComposite
   Indicates if a given icon ref is a composite of two other icon refs (and which ones)
   If it isn't a composite, backgroundIconRef and foreGroundIconRef will be 0.
}

{
 *  IsIconRefComposite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsIconRefComposite(compositeIconRef: IconRef; var backgroundIconRef: IconRef; var foregroundIconRef: IconRef): OSErr; external name '_IsIconRefComposite';
{
  ==============================================================================
   Using IconRef
  ==============================================================================
}

{
   IsValidIconRef
   Return true if the iconRef passed in is a valid icon ref
}

{
 *  IsValidIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsValidIconRef(theIconRef: IconRef): boolean; external name '_IsValidIconRef';
{
   PlotIconRef
   
   This routine plots the IconRef.  It mostly takes the same parameters as 
   PlotIconSuite. Pass kIconServicesNormalUsageFlag as a default value for 
   IconServicesUsageFlags.
}

{
 *  PlotIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PlotIconRef(const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef): OSErr; external name '_PlotIconRef';
{  PlotIconRefInContext }
{
 *  PlotIconRefInContext()
 *  
 *  Discussion:
 *    This routines plots an IconRef using Quartz/CoreGraphics.
 *  
 *  Parameters:
 *    
 *    inContext:
 *      The graphics context to use.
 *    
 *    inRect:
 *      The rect to plot the icon in.
 *    
 *    inAlign:
 *      The icon alignment.
 *    
 *    inTransform:
 *      The icon transform.
 *    
 *    inLabelColor:
 *      The icon label color.
 *    
 *    inFlags:
 *      The drawing flags to use (usually kPlotIconRefNormalFlags).
 *    
 *    inIconRef:
 *      The IconRef to plot.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function PlotIconRefInContext(inContext: CGContextRef; const (*var*) inRect: CGRect; inAlign: IconAlignmentType; inTransform: IconTransformType; inLabelColor: RGBColorPtr; inFlags: PlotIconRefFlags; inIconRef: IconRef): OSStatus; external name '_PlotIconRefInContext';


{
   PtInIconRef
   
   This routine indicates if testPt would hit the icon designated by iconRef.
   It mostly takes the same parameters as PtInIconSuite.
   Pass kIconServicesNormalUsageFlag as a default value for IconServicesUsageFlags.
}


{
 *  PtInIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PtInIconRef(const (*var*) testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef): boolean; external name '_PtInIconRef';
{
   RectInIconRef
   
   This routine indicates if testRect would intersect the icon designated by iconRef.
   It mostly takes the same parameters as RectInIconSuite.
   Pass kIconServicesNormalUsageFlag as a default value for IconServicesUsageFlags.
}


{
 *  RectInIconRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RectInIconRef(const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; iconServicesUsageFlags_: IconServicesUsageFlags; theIconRef: IconRef): boolean; external name '_RectInIconRef';
{
   IconRefToRgn
   
   This routine returns a region for the icon.
   It mostly takes the same parameters as IconSuiteToRgn.
   Pass kIconServicesNormalUsageFlag as a default value for IconServicesUsageFlags.
}

{
 *  IconRefToRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IconRefToRgn(theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; iconServicesUsageFlags_: IconServicesUsageFlags; theIconRef: IconRef): OSErr; external name '_IconRefToRgn';
{
   GetIconSizesFromIconRef
   
   This routine returns an IconSelectorValue indicating the depths and sizes of 
   icon data which are actually available.  It takes an IconSelectorValue 
   indicating which sizes/depths the caller is interested and returns an 
   IconSelectorValue indicating which sizes/depths exist.
   
   Caution:
   
   This is potentially an extremely expensive call as IconServices may be forced 
   to attempt fetching the data for the IconRef's sizes/depths which may result 
   in hitting the local disk or even the network to obtain the data to determine 
   which sizes/depths actually exist.
   Pass kIconServicesNormalUsageFlag as a default value for IconServicesUsageFlags.
   
   This call is deprecated. Please use IsDataAvailableInIconRef() instead.
}

{
 *  GetIconSizesFromIconRef()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconSizesFromIconRef(iconSelectorInput: IconSelectorValue; var iconSelectorOutputPtr: IconSelectorValue; iconServicesUsageFlags_: IconServicesUsageFlags; theIconRef: IconRef): OSErr; external name '_GetIconSizesFromIconRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)

{ IsDataAvailableInIconRef}
{
 *  IsDataAvailableInIconRef()
 *  
 *  Summary:
 *    Check if IconRef has specific data.
 *  
 *  Discussion:
 *    This routine returns true if inIconKind icon data is availabe or
 *    can be created.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inIconKind:
 *      The icon data kind
 *    
 *    inIconRef:
 *      The IconRef to test.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   not available
 }
function IsDataAvailableInIconRef( inIconKind: OSType; inIconRef: IconRef ): Boolean; external name '_IsDataAvailableInIconRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{
  ==============================================================================
   Flushing IconRef data
  ==============================================================================
}


{
   FlushIconRefs
   
   Making this call will dispose of all the data for the specified icons if 
   the data can be reacquired, for example if the data is provided from a resource.
   '****' is a wildcard for all types or all creators.
}

{
 *  FlushIconRefs()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function FlushIconRefs(creator: OSType; iconType: OSType): OSErr; external name '_FlushIconRefs';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)

{
   FlushIconRefsByVolume
   
   This routine disposes of the data for the icons related to the indicated volume
   if this data can be reacquired, for example if the data is provided from a 
   resource.
}

{
 *  FlushIconRefsByVolume()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function FlushIconRefsByVolume(vRefNum: SInt16): OSErr; external name '_FlushIconRefsByVolume';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)

{
  ==============================================================================
   Controling custom icons
  ==============================================================================
}


{
   SetCustomIconsEnabled
   
   Enable or disable custom icons on the specified volume.
}

{
 *  SetCustomIconsEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetCustomIconsEnabled(vRefNum: SInt16; enableCustomIcons: boolean): OSErr; external name '_SetCustomIconsEnabled';
{
   GetCustomIconsEnabled
   
   Return true if custom icons are enabled on the specified volume, false otherwise.
}

{
 *  GetCustomIconsEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCustomIconsEnabled(vRefNum: SInt16; var customIconsEnabled: boolean): OSErr; external name '_GetCustomIconsEnabled';
{
   IsIconRefMaskEmpty
   Returns true if the mask for this icon is blank
}

{
 *  IsIconRefMaskEmpty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsIconRefMaskEmpty(iconRef_: IconRef): boolean; external name '_IsIconRefMaskEmpty';
{
   GetIconRefVariant
   Icon variants allows different images to be used with different icon state.
   For example, the 'open' variant for a folder could be represented with
   an open folder.
   Given an icon ref and a desired variant, this routine returns an icon
   ref (which may be the same as the input icon ref) and a transformation 
   which should be used with PlotIconRef() to render the icon appropriately.
   The returned icon ref should be used to do hit-testing.
}

{
 *  GetIconRefVariant()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIconRefVariant(inIconRef: IconRef; inVariant: OSType; var outTransform: IconTransformType): IconRef; external name '_GetIconRefVariant';
{
  ==============================================================================
   Icon files (.icns files)
  ==============================================================================
}


{
   RegisterIconRefFromIconFile
   This routine adds a new entry to the IconRef registry. Other clients will be 
   able to access it using the (creator, iconType) pair specified here.
   Lower-case creators are reserved for the system.
   If the creator is kSystemIconsCreator and the iconType is 0, a new IconRef
   is always returned. Otherwise, if the creator and type have already been
   registered, the previously registered IconRef is returned.
   This routine increments the reference count of the IconRef. Call ReleaseIconRef() 
   when you're done with it.
}

{
 *  RegisterIconRefFromIconFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RegisterIconRefFromIconFile(creator: OSType; iconType: OSType; const (*var*) iconFile: FSSpec; var theIconRef: IconRef): OSErr; external name '_RegisterIconRefFromIconFile';
{
   ReadIconFile
   Read the specified icon file into the icon family handle.
   The caller is responsible for disposing the iconFamily
}

{
 *  ReadIconFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReadIconFile(const (*var*) iconFile: FSSpec; var iconFamily: IconFamilyHandle): OSErr; external name '_ReadIconFile';
{  ReadIconFromFSRef }
{
 *  ReadIconFromFSRef()
 *  
 *  Discussion:
 *    This routine reads an icon (icns) file into memory.
 *  
 *  Parameters:
 *    
 *    ref:
 *      The FSRef for the icon file.
 *    
 *    iconFamily:
 *      The handle for the icon family.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function ReadIconFromFSRef(const (*var*) ref: FSRef; var iconFamily: IconFamilyHandle): OSStatus; external name '_ReadIconFromFSRef';


{
   WriteIconFile
   Write the iconFamily handle to the specified file
}

{
 *  WriteIconFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in IconServicesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function WriteIconFile(iconFamily: IconFamilyHandle; const (*var*) iconFile: FSSpec): OSErr; external name '_WriteIconFile';
{$ALIGN MAC68K}


end.

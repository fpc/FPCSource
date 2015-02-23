{
     File:       HIServices/Icons.h
 
     Contains:   Icon Utilities and Icon Services Interfaces.
 
     Version:    HIServices-416~44
 
     Copyright:  © 1990-2008 by Apple Computer, Inc. All rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}

{  Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }

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

unit Icons;
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
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
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
uses MacTypes,CFBase,CGGeometry,QuickdrawTypes,IconStorage,IconsCore,Files,CGContext,HIShape;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

{ Alignment type values. }
const
	kAlignNone = $00;
	kAlignVerticalCenter = $01;
	kAlignTop = $02;
	kAlignBottom = $03;
	kAlignHorizontalCenter = $04;
	kAlignAbsoluteCenter = kAlignVerticalCenter or kAlignHorizontalCenter;
	kAlignCenterTop = kAlignTop or kAlignHorizontalCenter;
	kAlignCenterBottom = kAlignBottom or kAlignHorizontalCenter;
	kAlignLeft = $08;
	kAlignCenterLeft = kAlignVerticalCenter or kAlignLeft;
	kAlignTopLeft = kAlignTop or kAlignLeft;
	kAlignBottomLeft = kAlignBottom or kAlignLeft;
	kAlignRight = $0C;
	kAlignCenterRight = kAlignVerticalCenter or kAlignRight;
	kAlignTopRight = kAlignTop or kAlignRight;
	kAlignBottomRight = kAlignBottom or kAlignRight;

{ Obsolete. Use names defined above. }
const
	atNone = kAlignNone;
	atVerticalCenter = kAlignVerticalCenter;
	atTop = kAlignTop;
	atBottom = kAlignBottom;
	atHorizontalCenter = kAlignHorizontalCenter;
	atAbsoluteCenter = kAlignAbsoluteCenter;
	atCenterTop = kAlignCenterTop;
	atCenterBottom = kAlignCenterBottom;
	atLeft = kAlignLeft;
	atCenterLeft = kAlignCenterLeft;
	atTopLeft = kAlignTopLeft;
	atBottomLeft = kAlignBottomLeft;
	atRight = kAlignRight;
	atCenterRight = kAlignCenterRight;
	atTopRight = kAlignTopRight;
	atBottomRight = kAlignBottomRight;

type
	IconAlignmentType = SInt16;
{ Transform type values. }
const
	kTransformNone = $00;
	kTransformDisabled = $01;
	kTransformOffline = $02;
	kTransformOpen = $03;
	kTransformLabel1 = $0100;
	kTransformLabel2 = $0200;
	kTransformLabel3 = $0300;
	kTransformLabel4 = $0400;
	kTransformLabel5 = $0500;
	kTransformLabel6 = $0600;
	kTransformLabel7 = $0700;
	kTransformSelected = $4000;
	kTransformSelectedDisabled = kTransformSelected or kTransformDisabled;
	kTransformSelectedOffline = kTransformSelected or kTransformOffline;
	kTransformSelectedOpen = kTransformSelected or kTransformOpen;

{ Obsolete. Use names defined above. }
const
	ttNone = kTransformNone;
	ttDisabled = kTransformDisabled;
	ttOffline = kTransformOffline;
	ttOpen = kTransformOpen;
	ttLabel1 = kTransformLabel1;
	ttLabel2 = kTransformLabel2;
	ttLabel3 = kTransformLabel3;
	ttLabel4 = kTransformLabel4;
	ttLabel5 = kTransformLabel5;
	ttLabel6 = kTransformLabel6;
	ttLabel7 = kTransformLabel7;
	ttSelected = kTransformSelected;
	ttSelectedDisabled = kTransformSelectedDisabled;
	ttSelectedOffline = kTransformSelectedOffline;
	ttSelectedOpen = kTransformSelectedOpen;

type
	IconTransformType = SInt16;
{ Selector mask values. }
const
	kSelectorLarge1Bit = $00000001;
	kSelectorLarge4Bit = $00000002;
	kSelectorLarge8Bit = $00000004;
	kSelectorLarge32Bit = $00000008;
	kSelectorLarge8BitMask = $00000010;
	kSelectorSmall1Bit = $00000100;
	kSelectorSmall4Bit = $00000200;
	kSelectorSmall8Bit = $00000400;
	kSelectorSmall32Bit = $00000800;
	kSelectorSmall8BitMask = $00001000;
	kSelectorMini1Bit = $00010000;
	kSelectorMini4Bit = $00020000;
	kSelectorMini8Bit = $00040000;
	kSelectorHuge1Bit = $01000000;
	kSelectorHuge4Bit = $02000000;
	kSelectorHuge8Bit = $04000000;
	kSelectorHuge32Bit = $08000000;
	kSelectorHuge8BitMask = $10000000;
	kSelectorAllLargeData = $000000FF;
	kSelectorAllSmallData = $0000FF00;
	kSelectorAllMiniData = $00FF0000;
	kSelectorAllHugeData = $FF000000;
	kSelectorAll1BitData = kSelectorLarge1Bit or kSelectorSmall1Bit or kSelectorMini1Bit or kSelectorHuge1Bit;
	kSelectorAll4BitData = kSelectorLarge4Bit or kSelectorSmall4Bit or kSelectorMini4Bit or kSelectorHuge4Bit;
	kSelectorAll8BitData = kSelectorLarge8Bit or kSelectorSmall8Bit or kSelectorMini8Bit or kSelectorHuge8Bit;
	kSelectorAll32BitData = kSelectorLarge32Bit or kSelectorSmall32Bit or kSelectorHuge32Bit;
	kSelectorAllAvailableData = $FFFFFFFF;

type
	IconSelectorValue = UInt32;
{ Obsolete. Use names defined above. }
const
	svLarge1Bit = kSelectorLarge1Bit;
	svLarge4Bit = kSelectorLarge4Bit;
	svLarge8Bit = kSelectorLarge8Bit;
	svSmall1Bit = kSelectorSmall1Bit;
	svSmall4Bit = kSelectorSmall4Bit;
	svSmall8Bit = kSelectorSmall8Bit;
	svMini1Bit = kSelectorMini1Bit;
	svMini4Bit = kSelectorMini4Bit;
	svMini8Bit = kSelectorMini8Bit;
	svAllLargeData = kSelectorAllLargeData;
	svAllSmallData = kSelectorAllSmallData;
	svAllMiniData = kSelectorAllMiniData;
	svAll1BitData = kSelectorAll1BitData;
	svAll4BitData = kSelectorAll4BitData;
	svAll8BitData = kSelectorAll8BitData;
	svAllAvailableData = kSelectorAllAvailableData;

type
	IconActionProcPtr = function( theType: ResType; var theIcon: Handle; yourDataPtr: UnivPtr ): OSErr;
	IconGetterProcPtr = function( theType: ResType; yourDataPtr: UnivPtr ): Handle;
	IconActionUPP = IconActionProcPtr;
	IconGetterUPP = IconGetterProcPtr;
{
 *  NewIconActionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewIconActionUPP( userRoutine: IconActionProcPtr ): IconActionUPP; external name '_NewIconActionUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewIconGetterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
 { old name was NewIconGetterProc }
 function NewIconGetterUPP( userRoutine: IconGetterProcPtr ): IconGetterUPP; external name '_NewIconGetterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeIconActionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeIconActionUPP( userUPP: IconActionUPP ); external name '_DisposeIconActionUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeIconGetterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeIconGetterUPP( userUPP: IconGetterUPP ); external name '_DisposeIconGetterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeIconActionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
 { old name was CallIconActionProc }
 function InvokeIconActionUPP( theType: ResType; var theIcon: Handle; yourDataPtr: UnivPtr; userUPP: IconActionUPP ): OSErr; external name '_InvokeIconActionUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeIconGetterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeIconGetterUPP( theType: ResType; yourDataPtr: UnivPtr; userUPP: IconGetterUPP ): Handle; external name '_InvokeIconGetterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$ifc not TARGET_CPU_64}
type
	IconGetter = IconGetterProcPtr;
	IconAction = IconActionProcPtr;
	{  CIconHandle, GetCIcon(), PlotCIcon(), and DisposeCIcon() moved here from Quickdraw.h }
	 CIcon = record
		iconPMap: PixMap;               {the icon's pixMap}
		iconMask: BitMap;               {the icon's mask}
		iconBMap: BitMap;               {the icon's bitMap}
		iconData: Handle;               {the icon's data}
		iconMaskData: array [0..0] of SInt16;        {icon's mask and BitMap data}
	end;
	CIconPtr = ^CIcon;
type
	CIconHandle = ^CIconPtr;
	CIconHandle_fix = ^CIconPtr; { used as field type when a record declaration contains a CIconHandle field identifier }
{
    Note:   IconSuiteRef and IconCacheRef should be an abstract types, 
            but too much source code already relies on them being of type Handle.
}
type
	IconSuiteRef = Handle;
	IconCacheRef = Handle;

{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}

{
 *  GetCIcon()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetCIcon( iconID: SInt16 ): CIconHandle; external name '_GetCIcon';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotCIcon()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure PlotCIcon( const (*var*) theRect: Rect; theIcon: CIconHandle ); external name '_PlotCIcon';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DisposeCIcon()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure DisposeCIcon( theIcon: CIconHandle ); external name '_DisposeCIcon';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  GetIcon()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetIcon( iconID: SInt16 ): Handle; external name '_GetIcon';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotIcon()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure PlotIcon( const (*var*) theRect: Rect; theIcon: Handle ); external name '_PlotIcon';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotIconID()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PlotIconID( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theResID: SInt16 ): OSErr; external name '_PlotIconID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  NewIconSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function NewIconSuite( var theIconSuite: IconSuiteRef ): OSErr; external name '_NewIconSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  AddIconToSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AddIconToSuite( theIconData: Handle; theSuite: IconSuiteRef; theType: ResType ): OSErr; external name '_AddIconToSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  GetIconFromSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetIconFromSuite( var theIconData: Handle; theSuite: IconSuiteRef; theType: ResType ): OSErr; external name '_GetIconFromSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  ForEachIconDo()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function ForEachIconDo( theSuite: IconSuiteRef; selector: IconSelectorValue; action: IconActionUPP; yourDataPtr: UnivPtr ): OSErr; external name '_ForEachIconDo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  GetIconSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetIconSuite( var theIconSuite: IconSuiteRef; theResID: SInt16; selector: IconSelectorValue ): OSErr; external name '_GetIconSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  DisposeIconSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DisposeIconSuite( theIconSuite: IconSuiteRef; disposeData: Boolean ): OSErr; external name '_DisposeIconSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotIconSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PlotIconSuite( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIconSuite: IconSuiteRef ): OSErr; external name '_PlotIconSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  MakeIconCache()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function MakeIconCache( var theCache: IconCacheRef; makeIcon: IconGetterUPP; yourDataPtr: UnivPtr ): OSErr; external name '_MakeIconCache';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  LoadIconCache()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function LoadIconCache( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIconCache: IconCacheRef ): OSErr; external name '_LoadIconCache';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotIconMethod()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PlotIconMethod( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr ): OSErr; external name '_PlotIconMethod';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  GetLabel()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetLabel( labelNumber: SInt16; var labelColor: RGBColor; var labelString: Str255 ): OSErr; external name '_GetLabel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PtInIconID()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PtInIconID( testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; iconID: SInt16 ): Boolean; external name '_PtInIconID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PtInIconSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PtInIconSuite( testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconSuite: IconSuiteRef ): Boolean; external name '_PtInIconSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PtInIconMethod()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PtInIconMethod( testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr ): Boolean; external name '_PtInIconMethod';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  RectInIconID()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function RectInIconID( const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; iconID: SInt16 ): Boolean; external name '_RectInIconID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  RectInIconSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function RectInIconSuite( const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconSuite: IconSuiteRef ): Boolean; external name '_RectInIconSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  RectInIconMethod()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function RectInIconMethod( const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr ): Boolean; external name '_RectInIconMethod';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  IconIDToRgn()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IconIDToRgn( theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; iconID: SInt16 ): OSErr; external name '_IconIDToRgn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  IconSuiteToRgn()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IconSuiteToRgn( theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconSuite: IconSuiteRef ): OSErr; external name '_IconSuiteToRgn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  IconMethodToRgn()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IconMethodToRgn( theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; theMethod: IconGetterUPP; yourDataPtr: UnivPtr ): OSErr; external name '_IconMethodToRgn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  SetSuiteLabel()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetSuiteLabel( theSuite: IconSuiteRef; theLabel: SInt16 ): OSErr; external name '_SetSuiteLabel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  GetSuiteLabel()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetSuiteLabel( theSuite: IconSuiteRef ): SInt16; external name '_GetSuiteLabel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  GetIconCacheData()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetIconCacheData( theCache: IconCacheRef; var theData: UnivPtr ): OSErr; external name '_GetIconCacheData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  SetIconCacheData()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetIconCacheData( theCache: IconCacheRef; theData: UnivPtr ): OSErr; external name '_SetIconCacheData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  GetIconCacheProc()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetIconCacheProc( theCache: IconCacheRef; var theProc: IconGetterUPP ): OSErr; external name '_GetIconCacheProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  SetIconCacheProc()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetIconCacheProc( theCache: IconCacheRef; theProc: IconGetterUPP ): OSErr; external name '_SetIconCacheProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotIconHandle()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PlotIconHandle( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIcon: Handle ): OSErr; external name '_PlotIconHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotSICNHandle()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PlotSICNHandle( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theSICN: Handle ): OSErr; external name '_PlotSICNHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PlotCIconHandle()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PlotCIconHandle( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theCIcon: CIconHandle ): OSErr; external name '_PlotCIconHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{$endc} {not TARGET_CPU_64}


{
 *  PlotIconRefFlags
 *  
 *  Discussion:
 *    Flags that can be passed to the PlotIconRefInContext routine.
 }
type
	PlotIconRefFlags = UInt32;
const
{
   * Draw the icon image and alpha channel.
   }
	kPlotIconRefNormalFlags = 0;

  {
   * Only draw the alpha channel.
   }
	kPlotIconRefNoImage = 1 shl 1;

  {
   * Only draw the image.
   }
	kPlotIconRefNoMask = 1 shl 2;


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
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function IconRefToIconFamily( theIconRef: IconRef; whichIcons: IconSelectorValue; var iconFamily: IconFamilyHandle ): OSErr; external name '_IconRefToIconFamily';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   IconFamilyToIconSuite
   This routine transfers the data from an icon family handle into an icon suite.
}

{$ifc not TARGET_CPU_64}
{
 *  IconFamilyToIconSuite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function IconFamilyToIconSuite( iconFamily: IconFamilyHandle; whichIcons: IconSelectorValue; var iconSuite: IconSuiteRef ): OSErr; external name '_IconFamilyToIconSuite';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   IconSuiteToIconFamily
   This routine transfers the data in an icon suite into an icon family.
}

{
 *  IconSuiteToIconFamily()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function IconSuiteToIconFamily( iconSuite: IconSuiteRef; whichIcons: IconSelectorValue; var iconFamily: IconFamilyHandle ): OSErr; external name '_IconSuiteToIconFamily';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   SetIconFamilyData
   Change the data of an icon family. The data is copied.
   The type can be one of the icon type, or 'PICT'.
   The data will be compressed if needed.
}

{$endc} {not TARGET_CPU_64}

{
 *  SetIconFamilyData()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function SetIconFamilyData( iconFamily: IconFamilyHandle; iconType: OSType; h: Handle ): OSErr; external name '_SetIconFamilyData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


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
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function GetIconFamilyData( iconFamily: IconFamilyHandle; iconType: OSType; h: Handle ): OSErr; external name '_GetIconFamilyData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   PlotIconRef
   
   This routine plots the IconRef.  It mostly takes the same parameters as 
   PlotIconSuite. Pass kIconServicesNormalUsageFlag as a default value for 
   IconServicesUsageFlags.
}

{$ifc not TARGET_CPU_64}
{
 *  PlotIconRef()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function PlotIconRef( const (*var*) theRect: Rect; align: IconAlignmentType; transform: IconTransformType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef ): OSErr; external name '_PlotIconRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ PlotIconRefInContext}
{$endc} {not TARGET_CPU_64}

{
 *  PlotIconRefInContext()
 *  
 *  Discussion:
 *    This routines plots an IconRef using Quartz/CoreGraphics.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
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
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function PlotIconRefInContext( inContext: CGContextRef; const (*var*) inRect: CGRect; inAlign: IconAlignmentType; inTransform: IconTransformType; inLabelColor: RGBColorPtr; inFlags: PlotIconRefFlags; inIconRef: IconRef ): OSStatus; external name '_PlotIconRefInContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
   PtInIconRef
   
   This routine indicates if testPt would hit the icon designated by iconRef.
   It mostly takes the same parameters as PtInIconSuite.
   Pass kIconServicesNormalUsageFlag as a default value for IconServicesUsageFlags.
   This call is deprecated. Please use IconRefContainsCGPoint() instead.
}


{$ifc not TARGET_CPU_64}
{
 *  PtInIconRef()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function PtInIconRef( const (*var*) testPt: Point; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef ): Boolean; external name '_PtInIconRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   RectInIconRef
   
   This routine indicates if testRect would intersect the icon designated by iconRef.
   It mostly takes the same parameters as RectInIconSuite.
   Pass kIconServicesNormalUsageFlag as a default value for IconServicesUsageFlags.
   This call is deprecated. Please use IconRefIntersectsCGRect() instead.
}


{
 *  RectInIconRef()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function RectInIconRef( const (*var*) testRect: Rect; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef ): Boolean; external name '_RectInIconRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   IconRefToRgn
   
   This routine returns a region for the icon.
   It mostly takes the same parameters as IconSuiteToRgn.
   Pass kIconServicesNormalUsageFlag as a default value for IconServicesUsageFlags.
   This call is deprecated. Please use IconRefToHIShape() instead.
}

{
 *  IconRefToRgn()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function IconRefToRgn( theRgn: RgnHandle; const (*var*) iconRect: Rect; align: IconAlignmentType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef ): OSErr; external name '_IconRefToRgn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{$endc} {not TARGET_CPU_64}

{
 *  IconRefContainsCGPoint()
 *  
 *  Discussion:
 *    Tests whether a specified point falls within an icon’s mask.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    testPt:
 *      A pointer to the location that IconServices tests to see
 *      whether is falls within the mask of indicated icon.
 *    
 *    iconRect:
 *      A pointer to the rectangle defining the area that Icon Services
 *      uses to determine which icon is hit-tested. Use the same CGRect
 *      value as when the icon was last drawn.
 *    
 *    align:
 *      A value that specifies how the indicated icon is aligned within
 *      the rectangle specified in the iconRect parameter. Use the same
 *      IconAlignmentType value as when the icon was last drawn.
 *    
 *    iconServicesUsageFlags:
 *      Reserved for future use. Pass the kIconServicesNormalUsageFlag
 *      constant in this parameter.
 *    
 *    theIconRef:
 *      The icon to be tested.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function IconRefContainsCGPoint( const (*var*) testPt: CGPoint; const (*var*) iconRect: CGRect; align: IconAlignmentType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef ): Boolean; external name '_IconRefContainsCGPoint';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{
 *  IconRefIntersectsCGRect()
 *  
 *  Discussion:
 *    Tests whether a specified rectangle falls within an icon’s mask.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    testRect:
 *      A pointer to the rectangle that IconServices tests to see
 *      whether is falls within the mask of indicated icon.
 *    
 *    iconRect:
 *      A pointer to the rectangle defining the area that Icon Services
 *      uses to determine which icon is hit-tested. Use the same CGRect
 *      value as when the icon was last drawn.
 *    
 *    align:
 *      A value that specifies how the indicated icon is aligned within
 *      the rectangle specified in the iconRect parameter. Use the same
 *      IconAlignmentType value as when the icon was last drawn.
 *    
 *    iconServicesUsageFlags:
 *      Reserved for future use. Pass the kIconServicesNormalUsageFlag
 *      constant in this parameter.
 *    
 *    theIconRef:
 *      The icon to be tested.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function IconRefIntersectsCGRect( const (*var*) testRect: CGRect; const (*var*) iconRect: CGRect; align: IconAlignmentType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef ): Boolean; external name '_IconRefIntersectsCGRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{
 *  IconRefToHIShape()
 *  
 *  Discussion:
 *    Converts an IconRef-derived icon into an HIShape.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    iconRect:
 *      A pointer to the rectangle defining the area that Icon Services
 *      uses as the bounding box of the region.
 *    
 *    align:
 *      The value which determines how Icon Services aligns the region
 *      within the rectangle.
 *    
 *    iconServicesUsageFlags:
 *      Reserved for future use. Pass the kIconServicesNormalUsageFlag
 *      constant in this parameter.
 *    
 *    theIconRef:
 *      The icon to be tested.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function IconRefToHIShape( const (*var*) iconRect: CGRect; align: IconAlignmentType; theIconServicesUsageFlags: IconServicesUsageFlags; theIconRef: IconRef ): HIShapeRef; external name '_IconRefToHIShape';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


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

{$ifc not TARGET_CPU_64}
{
 *  GetIconSizesFromIconRef()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.3
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function GetIconSizesFromIconRef( iconSelectorInput: IconSelectorValue; var iconSelectorOutputPtr: IconSelectorValue; iconServicesUsageFlags_: IconServicesUsageFlags; theIconRef: IconRef ): OSErr; external name '_GetIconSizesFromIconRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3 *)


{
   IsIconRefMaskEmpty
   Returns true if the mask for this icon is blank
}

{$endc} {TARGET_CPU_64}

{
 *  IsIconRefMaskEmpty()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 8.5 and later
 }
function IsIconRefMaskEmpty( iconRef_: IconRef ): Boolean; external name '_IsIconRefMaskEmpty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


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
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in IconServicesLib 9.0 and later
 }
function GetIconRefVariant( inIconRef: IconRef; inVariant: OSType; var outTransform: IconTransformType ): IconRef; external name '_GetIconRefVariant';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

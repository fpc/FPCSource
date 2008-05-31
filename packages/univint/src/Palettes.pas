{
     File:       Palettes.p
 
     Contains:   Palette Manager Interfaces.
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1987-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit Palettes;
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
uses MacTypes,Quickdraw;


{$ALIGN MAC68K}


const
	pmCourteous					= 0;							{ Record use of color on each device touched. }
	pmDithered					= $0001;
	pmTolerant					= $0002;						{ render ciRGB if ciTolerance is exceeded by best match. }
	pmAnimated					= $0004;						{ reserve an index on each device touched and render ciRGB. }
	pmExplicit					= $0008;						{ no reserve, no render, no record; stuff index into port. }
	pmWhite						= $0010;
	pmBlack						= $0020;
	pmInhibitG2					= $0100;
	pmInhibitC2					= $0200;
	pmInhibitG4					= $0400;
	pmInhibitC4					= $0800;
	pmInhibitG8					= $1000;
	pmInhibitC8					= $2000;						{  NSetPalette Update Constants  }
	pmNoUpdates					= $8000;						{ no updates }
	pmBkUpdates					= $A000;						{ background updates only }
	pmFgUpdates					= $C000;						{ foreground updates only }
	pmAllUpdates				= $E000;						{ all updates }


type
	ColorInfoPtr = ^ColorInfo;
	ColorInfo = record
		ciRGB:					RGBColor;								{ true RGB values }
		ciUsage:				SInt16;								{ color usage }
		ciTolerance:			SInt16;								{ tolerance value }
		ciDataFields:			array [0..2] of SInt16;				{ private fields }
	end;

	ColorInfoHandle						= ^ColorInfoPtr;
	PalettePtr = ^Palette;
	Palette = record
		pmEntries:				SInt16;								{ entries in pmTable }
		pmDataFields:			array [0..6] of SInt16;				{ private fields }
		pmInfo:					array [0..0] of ColorInfo;
	end;

	PaletteHandle						= ^PalettePtr;
	{
	 *  InitPalettes()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure InitPalettes; external name '_InitPalettes';
{
 *  NewPalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPalette(entries: SInt16; srcColors: CTabHandle; srcUsage: SInt16; srcTolerance: SInt16): PaletteHandle; external name '_NewPalette';
{
 *  GetNewPalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetNewPalette(PaletteID: SInt16): PaletteHandle; external name '_GetNewPalette';
{
 *  DisposePalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePalette(srcPalette: PaletteHandle); external name '_DisposePalette';
{
 *  ActivatePalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure ActivatePalette(srcWindow: WindowRef); external name '_ActivatePalette';
{
 *  SetPalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetPalette(dstWindow: WindowRef; srcPalette: PaletteHandle; cUpdates: boolean); external name '_SetPalette';
{
 *  NSetPalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure NSetPalette(dstWindow: WindowRef; srcPalette: PaletteHandle; nCUpdates: SInt16); external name '_NSetPalette';
{
 *  GetPalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetPalette(srcWindow: WindowRef): PaletteHandle; external name '_GetPalette';
{
 *  CopyPalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure CopyPalette(srcPalette: PaletteHandle; dstPalette: PaletteHandle; srcEntry: SInt16; dstEntry: SInt16; dstLength: SInt16); external name '_CopyPalette';
{
 *  PmForeColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PmForeColor(dstEntry: SInt16); external name '_PmForeColor';
{
 *  PmBackColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PmBackColor(dstEntry: SInt16); external name '_PmBackColor';
{
 *  AnimateEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AnimateEntry(dstWindow: WindowRef; dstEntry: SInt16; const (*var*) srcRGB: RGBColor); external name '_AnimateEntry';
{
 *  [Mac]AnimatePalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AnimatePalette(dstWindow: WindowRef; srcCTab: CTabHandle; srcIndex: SInt16; dstEntry: SInt16; dstLength: SInt16); external name '_AnimatePalette';
{
 *  GetEntryColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetEntryColor(srcPalette: PaletteHandle; srcEntry: SInt16; var dstRGB: RGBColor); external name '_GetEntryColor';
{
 *  SetEntryColor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetEntryColor(dstPalette: PaletteHandle; dstEntry: SInt16; const (*var*) srcRGB: RGBColor); external name '_SetEntryColor';
{
 *  GetEntryUsage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetEntryUsage(srcPalette: PaletteHandle; srcEntry: SInt16; var dstUsage: SInt16; var dstTolerance: SInt16); external name '_GetEntryUsage';
{
 *  SetEntryUsage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetEntryUsage(dstPalette: PaletteHandle; dstEntry: SInt16; srcUsage: SInt16; srcTolerance: SInt16); external name '_SetEntryUsage';
{
 *  CTab2Palette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure CTab2Palette(srcCTab: CTabHandle; dstPalette: PaletteHandle; srcUsage: SInt16; srcTolerance: SInt16); external name '_CTab2Palette';
{
 *  Palette2CTab()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure Palette2CTab(srcPalette: PaletteHandle; dstCTab: CTabHandle); external name '_Palette2CTab';
{
 *  Entry2Index()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Entry2Index(entry: SInt16): SInt32; external name '_Entry2Index';
{
 *  RestoreDeviceClut()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure RestoreDeviceClut(gd: GDHandle); external name '_RestoreDeviceClut';
{
 *  [Mac]ResizePalette()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure ResizePalette(p: PaletteHandle; size: SInt16); external name '_ResizePalette';
{
 *  SaveFore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SaveFore(var c: ColorSpec); external name '_SaveFore';
{
 *  SaveBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SaveBack(var c: ColorSpec); external name '_SaveBack';
{
 *  RestoreFore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure RestoreFore(const (*var*) c: ColorSpec); external name '_RestoreFore';
{
 *  RestoreBack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure RestoreBack(const (*var*) c: ColorSpec); external name '_RestoreBack';
{
 *  SetDepth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetDepth(gd: GDHandle; depth: SInt16; whichFlags: SInt16; flags: SInt16): OSErr; external name '_SetDepth';
{
 *  HasDepth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HasDepth(gd: GDHandle; depth: SInt16; whichFlags: SInt16; flags: SInt16): SInt16; external name '_HasDepth';
{
 *  PMgrVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMgrVersion: SInt16; external name '_PMgrVersion';
{
 *  SetPaletteUpdates()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetPaletteUpdates(p: PaletteHandle; updates: SInt16); external name '_SetPaletteUpdates';
{
 *  GetPaletteUpdates()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetPaletteUpdates(p: PaletteHandle): SInt16; external name '_GetPaletteUpdates';
{
 *  GetGray()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetGray(device: GDHandle; const (*var*) backGround: RGBColor; var foreGround: RGBColor): boolean; external name '_GetGray';
{$ALIGN MAC68K}


end.

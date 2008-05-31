{
     File:       QDOffscreen.p
 
     Contains:   Quickdraw Offscreen GWorld Interfaces.
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved
 
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

unit QDOffscreen;
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
uses MacTypes,MacErrors,Quickdraw;


{$ALIGN MAC68K}

{
    NOTE:   GWorldFlags is no longer defined as a set.  Instead it is a SInt32
            and the set elements are bit masks.  You will need to use Bit-OR's 
            to build a set and Bit-AND's to test sets.
}

const
	pixPurgeBit					= 0;
	noNewDeviceBit				= 1;
	useTempMemBit				= 2;
	keepLocalBit				= 3;
	useDistantHdwrMemBit		= 4;
	useLocalHdwrMemBit			= 5;
	pixelsPurgeableBit			= 6;
	pixelsLockedBit				= 7;
	mapPixBit					= 16;
	newDepthBit					= 17;
	alignPixBit					= 18;
	newRowBytesBit				= 19;
	reallocPixBit				= 20;
	clipPixBit					= 28;
	stretchPixBit				= 29;
	ditherPixBit				= 30;
	gwFlagErrBit				= 31;

	pixPurge					= $00000001;
	noNewDevice					= $00000002;
	useTempMem					= $00000004;
	keepLocal					= $00000008;
	useDistantHdwrMem			= $00000010;
	useLocalHdwrMem				= $00000020;
	pixelsPurgeable				= $00000040;
	pixelsLocked				= $00000080;
	kAllocDirectDrawSurface		= $00004000;
	mapPix						= $00010000;
	newDepth					= $00020000;
	alignPix					= $00040000;
	newRowBytes					= $00080000;
	reallocPix					= $00100000;
	clipPix						= $10000000;
	stretchPix					= $20000000;
	ditherPix					= $40000000;
	gwFlagErr					= $80000000;


type
	GWorldFlags							= UInt32;
	{	 Type definition of a GWorldPtr 	}
	GWorldPtr							= CGrafPtr;
	{
	 *  NewGWorld()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewGWorld(var offscreenGWorld: GWorldPtr; PixelDepth: SInt16; const (*var*) boundsRect: Rect; cTable: CTabHandle; aGDevice: GDHandle; flags: GWorldFlags): QDErr; external name '_NewGWorld';
{  GDevice attribute bits for Carbon and QuickTime 3.0 }

const
	deviceIsIndirect			= $00000001;
	deviceNeedsLock				= $00000002;
	deviceIsStatic				= $00000004;
	deviceIsExternalBuffer		= $00000008;
	deviceIsDDSurface			= $00000010;
	deviceIsDCISurface			= $00000020;
	deviceIsGDISurface			= $00000040;
	deviceIsAScreen				= $00000080;
	deviceIsOverlaySurface		= $00000100;

{$ifc TARGET_OS_WIN32}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  GetGDeviceSurface()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function GetGDeviceSurface(gdh: GDHandle): Ptr; external name '_GetGDeviceSurface';

{
 *  GetGDeviceAttributes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetGDeviceAttributes(gdh: GDHandle): UInt32; external name '_GetGDeviceAttributes';

{ to allocate non-mac-rgb GWorlds use QTNewGWorld (ImageCompression.h) }
{
 *  NewGWorldFromHBITMAP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewGWorldFromHBITMAP(var offscreenGWorld: GWorldPtr; cTable: CTabHandle; aGDevice: GDHandle; flags: GWorldFlags; newHBITMAP: UnivPtr; newHDC: UnivPtr): QDErr; external name '_NewGWorldFromHBITMAP';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_OS_WIN32}

{
 *  NewGWorldFromPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewGWorldFromPtr(var offscreenGWorld: GWorldPtr; PixelFormat: UInt32; const (*var*) boundsRect: Rect; cTable: CTabHandle; aGDevice: GDHandle; flags: GWorldFlags; newBuffer: Ptr; rowBytes: SInt32): QDErr; external name '_NewGWorldFromPtr';

{
 *  LockPixels()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LockPixels(pm: PixMapHandle): boolean; external name '_LockPixels';
{
 *  UnlockPixels()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure UnlockPixels(pm: PixMapHandle); external name '_UnlockPixels';
{
 *  UpdateGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UpdateGWorld(var offscreenGWorld: GWorldPtr; pixelDepth: SInt16; const (*var*) boundsRect: Rect; cTable: CTabHandle; aGDevice: GDHandle; flags: GWorldFlags): GWorldFlags; external name '_UpdateGWorld';
{
 *  DisposeGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeGWorld(offscreenGWorld: GWorldPtr); external name '_DisposeGWorld';
{
 *  GetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetGWorld(var port: CGrafPtr; var gdh: GDHandle); external name '_GetGWorld';
{
 *  SetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetGWorld(port: CGrafPtr; gdh: GDHandle); external name '_SetGWorld';
{
 *  CTabChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure CTabChanged(ctab: CTabHandle); external name '_CTabChanged';
{
 *  PixPatChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PixPatChanged(ppat: PixPatHandle); external name '_PixPatChanged';
{
 *  PortChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PortChanged(port: GrafPtr); external name '_PortChanged';
{
 *  GDeviceChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GDeviceChanged(gdh: GDHandle); external name '_GDeviceChanged';
{
 *  AllowPurgePixels()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AllowPurgePixels(pm: PixMapHandle); external name '_AllowPurgePixels';
{
 *  NoPurgePixels()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure NoPurgePixels(pm: PixMapHandle); external name '_NoPurgePixels';
{
 *  GetPixelsState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetPixelsState(pm: PixMapHandle): GWorldFlags; external name '_GetPixelsState';
{
 *  SetPixelsState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetPixelsState(pm: PixMapHandle; state: GWorldFlags); external name '_SetPixelsState';
{
 *  GetPixBaseAddr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetPixBaseAddr(pm: PixMapHandle): Ptr; external name '_GetPixBaseAddr';
{
 *  GetPixRowBytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetPixRowBytes(pm: PixMapHandle): SInt32; external name '_GetPixRowBytes';
{
 *  NewScreenBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewScreenBuffer(const (*var*) globalRect: Rect; purgeable: boolean; var gdh: GDHandle; var offscreenPixMap: PixMapHandle): QDErr; external name '_NewScreenBuffer';
{
 *  DisposeScreenBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeScreenBuffer(offscreenPixMap: PixMapHandle); external name '_DisposeScreenBuffer';
{
 *  GetGWorldDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetGWorldDevice(offscreenGWorld: GWorldPtr): GDHandle; external name '_GetGWorldDevice';
{
 *  QDDone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function QDDone(port: GrafPtr): boolean; external name '_QDDone';
{
 *  OffscreenVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OffscreenVersion: SInt32; external name '_OffscreenVersion';
{
 *  NewTempScreenBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewTempScreenBuffer(const (*var*) globalRect: Rect; purgeable: boolean; var gdh: GDHandle; var offscreenPixMap: PixMapHandle): QDErr; external name '_NewTempScreenBuffer';
{
 *  PixMap32Bit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PixMap32Bit(pmHandle: PixMapHandle): boolean; external name '_PixMap32Bit';
{
 *  GetGWorldPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetGWorldPixMap(offscreenGWorld: GWorldPtr): PixMapHandle; external name '_GetGWorldPixMap';
{$ALIGN MAC68K}


end.

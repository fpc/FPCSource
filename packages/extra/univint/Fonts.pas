{
     File:       Fonts.p
 
     Contains:   Public interface to the Font Manager.
 
     Version:    Technology: Mac OS 9 / Carbon
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved
 
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

unit Fonts;
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
uses MacTypes,QuickdrawText,ATSTypes,Files,MacErrors,Quickdraw,TextCommon;


{$ALIGN MAC68K}


const
	systemFont					= 0;
	applFont					= 1;

	kFMDefaultOptions			= 0;

	{	 Activation contexts 	}
	kFMDefaultActivationContext	= 0;
	kFMGlobalActivationContext	= $00000001;
	kFMLocalActivationContext	= 0;

	{	 Iteration scopes 	}
	kFMDefaultIterationScope	= 0;
	kFMGlobalIterationScope		= $00000001;
	kFMLocalIterationScope		= 0;


	{	 kPlatformDefaultGuiFontID is used in QuickTime 3.0. 	}
{$ifc TARGET_OS_MAC}
	kPlatformDefaultGuiFontID	= 1;

{$elsec}
	kPlatformDefaultGuiFontID	= -1;

{$endc}  {TARGET_OS_MAC}

	commandMark					= 17;
	checkMark					= 18;
	diamondMark					= 19;
	appleMark					= 20;

	propFont					= 36864;
	prpFntH						= 36865;
	prpFntW						= 36866;
	prpFntHW					= 36867;
	fixedFont					= 45056;
	fxdFntH						= 45057;
	fxdFntW						= 45058;
	fxdFntHW					= 45059;
	fontWid						= 44208;


type
	FMInputPtr = ^FMInput;
	FMInput = packed record
		family:					SInt16;
		size:					SInt16;
		face:					Style;
		needBits:				boolean;
		device:					SInt16;
		numer:					Point;
		denom:					Point;
	end;

	FMOutputPtr = ^FMOutput;
	FMOutput = packed record
		errNum:					SInt16;
		fontHandle:				Handle;
		boldPixels:				UInt8;
		italicPixels:			UInt8;
		ulOffset:				UInt8;
		ulShadow:				UInt8;
		ulThick:				UInt8;
		shadowPixels:			UInt8;
		extra:					SInt8;
		ascent:					UInt8;
		descent:				UInt8;
		widMax:					UInt8;
		leading:				SInt8;
		curStyle:				SInt8;
		numer:					Point;
		denom:					Point;
	end;

	FMOutPtr							= FMOutputPtr;
	FMetricRecPtr = ^FMetricRec;
	FMetricRec = record
		ascent:					Fixed;									{ base line to top }
		descent:				Fixed;									{ base line to bottom }
		leading:				Fixed;									{ leading between lines }
		widMax:					Fixed;									{ maximum character width }
		wTabHandle:				Handle;									{ handle to font width table }
	end;

	FMetricRecHandle					= ^FMetricRecPtr;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  InitFonts()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
procedure InitFonts; external name '_InitFonts';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  GetFontName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetFontName(familyID: SInt16; var name: Str255); external name '_GetFontName';
{
 *  GetFNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetFNum(const (*var*) name: Str255; var familyID: SInt16); external name '_GetFNum';
{
 *  RealFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RealFont(fontNum: SInt16; size: SInt16): boolean; external name '_RealFont';
{$ifc CALL_NOT_IN_CARBON}
{
 *  SetFontLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure SetFontLock(lockFlag: boolean); external name '_SetFontLock';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  FMSwapFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMSwapFont(const (*var*) inRec: FMInput): FMOutPtr; external name '_FMSwapFont';
{
 *  SetFScaleDisable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetFScaleDisable(fscaleDisable: boolean); external name '_SetFScaleDisable';
{
 *  FontMetrics()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure FontMetrics(theMetrics: FMetricRecPtr); external name '_FontMetrics';
{
 *  SetFractEnable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetFractEnable(fractEnable: boolean); external name '_SetFractEnable';
{
 *  GetDefFontSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetDefFontSize: SInt16; external name '_GetDefFontSize';
{
 *  IsOutline()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsOutline(numer: Point; denom: Point): boolean; external name '_IsOutline';
{
 *  SetOutlinePreferred()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetOutlinePreferred(outlinePreferred: boolean); external name '_SetOutlinePreferred';
{
 *  GetOutlinePreferred()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetOutlinePreferred: boolean; external name '_GetOutlinePreferred';
{
 *  OutlineMetrics()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OutlineMetrics(byteCount: SInt16; textPtr: UnivPtr; numer: Point; denom: Point; var yMax: SInt16; var yMin: SInt16; awArray: FixedPtr; lsbArray: FixedPtr; boundsArray: RectPtr): OSErr; external name '_OutlineMetrics';
{
 *  SetPreserveGlyph()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetPreserveGlyph(preserveGlyph: boolean); external name '_SetPreserveGlyph';
{
 *  GetPreserveGlyph()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetPreserveGlyph: boolean; external name '_GetPreserveGlyph';
{$ifc CALL_NOT_IN_CARBON}
{
 *  FlushFonts()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FlushFonts: OSErr; external name '_FlushFonts';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  GetSysFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSysFont: SInt16; external name '_GetSysFont';
{
 *  GetAppFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetAppFont: SInt16; external name '_GetAppFont';
{--------------------------------------------------------------------------------------}
{  Extended font data functions (available only with Mac OS 8.5 or later)              }
{--------------------------------------------------------------------------------------}
{
 *  SetAntiAliasedTextEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetAntiAliasedTextEnabled(iEnable: boolean; iMinFontSize: SInt16): OSStatus; external name '_SetAntiAliasedTextEnabled';
{
 *  IsAntiAliasedTextEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsAntiAliasedTextEnabled(var oMinFontSize: SInt16): boolean; external name '_IsAntiAliasedTextEnabled';
{
 *  QDTextBounds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure QDTextBounds(byteCount: SInt16; textAddr: UnivPtr; var bounds: Rect); external name '_QDTextBounds';
{
 *  FetchFontInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FetchFontInfo(fontID: SInt16; fontSize: SInt16; fontStyle: SInt16; var info: FontInfo): OSErr; external name '_FetchFontInfo';
{--------------------------------------------------------------------------------------}
{  Font access and data management functions (available only with Mac OS 9.0 or later) }
{--------------------------------------------------------------------------------------}
{ Enumeration }
{
 *  FMCreateFontFamilyIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMCreateFontFamilyIterator(iFilter: {Const}FMFilterPtr; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontFamilyIterator): OSStatus; external name '_FMCreateFontFamilyIterator';

{
 *  FMDisposeFontFamilyIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMDisposeFontFamilyIterator(var ioIterator: FMFontFamilyIterator): OSStatus; external name '_FMDisposeFontFamilyIterator';

{
 *  FMResetFontFamilyIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMResetFontFamilyIterator(iFilter: {Const}FMFilterPtr; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontFamilyIterator): OSStatus; external name '_FMResetFontFamilyIterator';

{
 *  FMGetNextFontFamily()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetNextFontFamily(var ioIterator: FMFontFamilyIterator; var oFontFamily: FMFontFamily): OSStatus; external name '_FMGetNextFontFamily';

{
 *  FMCreateFontIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMCreateFontIterator(iFilter: {Const}FMFilterPtr; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontIterator): OSStatus; external name '_FMCreateFontIterator';

{
 *  FMDisposeFontIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMDisposeFontIterator(var ioIterator: FMFontIterator): OSStatus; external name '_FMDisposeFontIterator';

{
 *  FMResetFontIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMResetFontIterator(iFilter: {Const}FMFilterPtr; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontIterator): OSStatus; external name '_FMResetFontIterator';

{
 *  FMGetNextFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetNextFont(var ioIterator: FMFontIterator; var oFont: FMFont): OSStatus; external name '_FMGetNextFont';

{ Font families }
{
 *  FMCreateFontFamilyInstanceIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMCreateFontFamilyInstanceIterator(iFontFamily: FMFontFamily; var ioIterator: FMFontFamilyInstanceIterator): OSStatus; external name '_FMCreateFontFamilyInstanceIterator';

{
 *  FMDisposeFontFamilyInstanceIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMDisposeFontFamilyInstanceIterator(var ioIterator: FMFontFamilyInstanceIterator): OSStatus; external name '_FMDisposeFontFamilyInstanceIterator';

{
 *  FMResetFontFamilyInstanceIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMResetFontFamilyInstanceIterator(iFontFamily: FMFontFamily; var ioIterator: FMFontFamilyInstanceIterator): OSStatus; external name '_FMResetFontFamilyInstanceIterator';

{
 *  FMGetNextFontFamilyInstance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetNextFontFamilyInstance(var ioIterator: FMFontFamilyInstanceIterator; var oFont: FMFont; oStyle: FMFontStylePtr; oSize: FMFontSizePtr): OSStatus; external name '_FMGetNextFontFamilyInstance';

{
 *  FMGetFontFamilyFromName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontFamilyFromName(const (*var*) iName: Str255): FMFontFamily; external name '_FMGetFontFamilyFromName';

{
 *  FMGetFontFamilyName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontFamilyName(iFontFamily: FMFontFamily; var oName: Str255): OSStatus; external name '_FMGetFontFamilyName';

{
 *  FMGetFontFamilyTextEncoding()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontFamilyTextEncoding(iFontFamily: FMFontFamily; var oTextEncoding: TextEncoding): OSStatus; external name '_FMGetFontFamilyTextEncoding';

{
 *  FMGetFontFamilyGeneration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontFamilyGeneration(iFontFamily: FMFontFamily; var oGeneration: FMGeneration): OSStatus; external name '_FMGetFontFamilyGeneration';

{ Fonts }
{
 *  FMGetFontFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontFormat(iFont: FMFont; var oFormat: FourCharCode): OSStatus; external name '_FMGetFontFormat';

{
 *  FMGetFontTableDirectory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontTableDirectory(iFont: FMFont; iLength: ByteCount; iBuffer: UnivPtr; oActualLength: ByteCountPtr): OSStatus; external name '_FMGetFontTableDirectory';

{
 *  FMGetFontTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontTable(iFont: FMFont; iTag: FourCharCode; iOffset: ByteOffset; iLength: ByteCount; iBuffer: UnivPtr; oActualLength: ByteCountPtr): OSStatus; external name '_FMGetFontTable';

{
 *  FMGetFontGeneration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontGeneration(iFont: FMFont; var oGeneration: FMGeneration): OSStatus; external name '_FMGetFontGeneration';

{
 *  FMGetFontContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontContainer(iFont: FMFont; var oFontContainer: FSSpec): OSStatus; external name '_FMGetFontContainer';

{ Conversion }
{
 *  FMGetFontFromFontFamilyInstance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontFromFontFamilyInstance(iFontFamily: FMFontFamily; iStyle: FMFontStyle; var oFont: FMFont; oIntrinsicStyle: FMFontStylePtr): OSStatus; external name '_FMGetFontFromFontFamilyInstance';

{
 *  FMGetFontFamilyInstanceFromFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetFontFamilyInstanceFromFont(iFont: FMFont; var oFontFamily: FMFontFamily; var oStyle: FMFontStyle): OSStatus; external name '_FMGetFontFamilyInstanceFromFont';

{
 *  FMGetATSFontRefFromFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FMGetATSFontRefFromFont(iFont: FMFont): ATSFontRef; external name '_FMGetATSFontRefFromFont';

{
 *  FMGetATSFontFamilyRefFromFontFamily()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FMGetATSFontFamilyRefFromFontFamily(iFamily: FMFontFamily): ATSFontFamilyRef; external name '_FMGetATSFontFamilyRefFromFontFamily';

{
 *  FMGetFontFromATSFontRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FMGetFontFromATSFontRef(iFont: ATSFontRef): FMFont; external name '_FMGetFontFromATSFontRef';

{
 *  FMGetFontFamilyFromATSFontFamilyRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FMGetFontFamilyFromATSFontFamilyRef(iFamily: ATSFontFamilyRef): FMFontFamily; external name '_FMGetFontFamilyFromATSFontFamilyRef';

{ Activation }
{
 *  FMActivateFonts()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMActivateFonts(const (*var*) iFontContainer: FSSpec; iFilter: {Const}FMFilterPtr; iRefCon: UnivPtr; iOptions: OptionBits): OSStatus; external name '_FMActivateFonts';

{
 *  FMDeactivateFonts()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMDeactivateFonts(const (*var*) iFontContainer: FSSpec; iFilter: {Const}FMFilterPtr; iRefCon: UnivPtr; iOptions: OptionBits): OSStatus; external name '_FMDeactivateFonts';

{
 *  FMGetGeneration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FMGetGeneration: FMGeneration; external name '_FMGetGeneration';

{ Container Access }
{
 *  FMGetFontContainerFromFontFamilyInstance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FMGetFontContainerFromFontFamilyInstance(iFontFamily: FMFontFamily; iStyle: FMFontStyle; iFontSize: FMFontSize; var oFontContainer: FSSpec): OSStatus; external name '_FMGetFontContainerFromFontFamilyInstance';

{
 *  FMGetFontFamilyResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FMGetFontFamilyResource(iFontFamily: FMFontFamily; iFontStyle: FMFontStyle; iFontSize: FMFontSize; iBufferSize: ByteCount; ioBuffer: UnivPtr; oSize: ByteCountPtr): OSStatus; external name '_FMGetFontFamilyResource';


type
	FontFamilyID						= FMFontFamily;
	FontPointSize						= FMFontSize;
	{	--------------------------------------------------------------------------------------	}
	{	 Deprecated constant and type definitions                                             	}
	{	--------------------------------------------------------------------------------------	}
	{	 The font identifier constants are deprecated; use GetFNum or FMGetFontFamilyFromName
	   to find a font family from a standard QuickDraw name.
		}

const
	kFMUseGlobalScopeOption		= $00000001;

	kFontIDNewYork				= 2;
	kFontIDGeneva				= 3;
	kFontIDMonaco				= 4;
	kFontIDVenice				= 5;
	kFontIDLondon				= 6;
	kFontIDAthens				= 7;
	kFontIDSanFrancisco			= 8;
	kFontIDToronto				= 9;
	kFontIDCairo				= 11;
	kFontIDLosAngeles			= 12;
	kFontIDTimes				= 20;
	kFontIDHelvetica			= 21;
	kFontIDCourier				= 22;
	kFontIDSymbol				= 23;
	kFontIDMobile				= 24;

	{	 The following data structures referenced by the low memory global variables of the
	   Font Manager are deprecated on Mac OS X and CarbonLib 1.1. The low memory global
	   variables are not shared between processes and may result in inconsistencies
	   compared to previous releases of the system software. Changes made to the
	   information contained in the low memory global variables, including any
	   indirectly referenced width tables, font family records, and font records, are
	   not reflected in the global state of the Font Manager and may only be accessed
	   through the font access and data management functions of the Font Manager or ATS.
		}

type
	WidEntryPtr = ^WidEntry;
	WidEntry = record
		widStyle:				SInt16;								{ style entry applies to }
	end;

	WidTablePtr = ^WidTable;
	WidTable = record
		numWidths:				SInt16;								{ number of entries - 1 }
	end;

	AsscEntryPtr = ^AsscEntry;
	AsscEntry = record
		fontSize:				SInt16;
		fontStyle:				SInt16;
		fontID:					SInt16;								{ font resource ID }
	end;

	FontAssocPtr = ^FontAssoc;
	FontAssoc = record
		numAssoc:				SInt16;								{ number of entries - 1 }
	end;

	StyleTablePtr = ^StyleTable;
	StyleTable = record
		fontClass:				SInt16;
		offset:					SInt32;
		reserved:				SInt32;
		indexes:				packed array [0..47] of char;
	end;

	NameTablePtr = ^NameTable;
	NameTable = record
		stringCount:			SInt16;
		baseFontName:			Str255;
	end;

	KernPairPtr = ^KernPair;
	KernPair = record
		kernFirst:				SInt8;									{ 1st character of kerned pair }
		kernSecond:				SInt8;									{ 2nd character of kerned pair }
		kernWidth:				SInt16;								{ kerning in 1pt fixed format }
	end;

	KernEntryPtr = ^KernEntry;
	KernEntry = record
		kernStyle:				SInt16;								{ style the entry applies to }
		kernLength:				SInt16;								{ length of this entry }
	end;

	KernTablePtr = ^KernTable;
	KernTable = record
		numKerns:				SInt16;								{ number of kerning entries }
	end;

	WidthTablePtr = ^WidthTable;
	WidthTable = packed record
		tabData:				array [0..255] of Fixed;				{ character widths }
		tabFont:				Handle;									{ font record used to build table }
		sExtra:					SInt32;								{ space extra used for table }
		style:					SInt32;								{ extra due to style }
		fID:					SInt16;								{ font family ID }
		fSize:					SInt16;								{ font size request }
		face:					SInt16;								{ style (face) request }
		device:					SInt16;								{ device requested }
		inNumer:				Point;									{ scale factors requested }
		inDenom:				Point;									{ scale factors requested }
		aFID:					SInt16;								{ actual font family ID for table }
		fHand:					Handle;									{ family record used to build up table }
		usedFam:				boolean;								{ used fixed point family widths }
		aFace:					UInt8;									{ actual face produced }
		vOutput:				SInt16;								{ vertical scale output value }
		hOutput:				SInt16;								{ horizontal scale output value }
		vFactor:				SInt16;								{ vertical scale output value }
		hFactor:				SInt16;								{ horizontal scale output value }
		aSize:					SInt16;								{ actual size of actual font used }
		tabSize:				SInt16;								{ total size of table }
	end;

	WidthTableHdl						= ^WidthTablePtr;
	FamRecPtr = ^FamRec;
	FamRec = record
		ffFlags:				SInt16;								{ flags for family }
		ffFamID:				SInt16;								{ family ID number }
		ffFirstChar:			SInt16;								{ ASCII code of 1st character }
		ffLastChar:				SInt16;								{ ASCII code of last character }
		ffAscent:				SInt16;								{ maximum ascent for 1pt font }
		ffDescent:				SInt16;								{ maximum descent for 1pt font }
		ffLeading:				SInt16;								{ maximum leading for 1pt font }
		ffWidMax:				SInt16;								{ maximum widMax for 1pt font }
		ffWTabOff:				SInt32;								{ offset to width table }
		ffKernOff:				SInt32;								{ offset to kerning table }
		ffStylOff:				SInt32;								{ offset to style mapping table }
		ffProperty:				array [0..8] of SInt16;				{ style property info }
		ffIntl:					array [0..1] of SInt16;				{ for international use }
		ffVersion:				SInt16;								{ version number }
	end;

	FontRecPtr = ^FontRec;
	FontRec = record
		fontType:				SInt16;								{ font type }
		firstChar:				SInt16;								{ ASCII code of first character }
		lastChar:				SInt16;								{ ASCII code of last character }
		widMax:					SInt16;								{ maximum character width }
		kernMax:				SInt16;								{ negative of maximum character kern }
		nDescent:				SInt16;								{ negative of descent }
		fRectWidth:				SInt16;								{ width of font rectangle }
		fRectHeight:			SInt16;								{ height of font rectangle }
		owTLoc:					UInt16;									{ offset to offset/width table }
		ascent:					SInt16;								{ ascent }
		descent:				SInt16;								{ descent }
		leading:				SInt16;								{ leading }
		rowWords:				SInt16;								{ row width of bit image / 2  }
	end;

	FontRecHdl							= ^FontRecPtr;
	{	--------------------------------------------------------------------------------------	}
{$ifc OLDROUTINENAMES}

const
	newYork						= 2;
	geneva						= 3;
	monaco						= 4;
	venice						= 5;
	london						= 6;
	athens						= 7;
	sanFran						= 8;
	toronto						= 9;
	cairo						= 11;
	losAngeles					= 12;
	times						= 20;
	helvetica					= 21;
	courier						= 22;
	symbol						= 23;
	mobile						= 24;

{$endc}  {OLDROUTINENAMES}

{--------------------------------------------------------------------------------------}
{$ALIGN MAC68K}


end.
